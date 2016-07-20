/* ----------------------------------------------- cec/box/pr42mis2.p 9/93 cd */
DEFINE INPUT PARAMETER ip-last-ef AS LOG NO-UNDO.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def shared var qty as INT NO-UNDO .
def var i as int no-undo.
def var tmpstore as cha no-undo.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def var v as int no-undo.
def var v-mat-cost as dec no-undo.
def var v-lab-cost as dec no-undo.
def var v-yld as dec no-undo.
DEF VAR ld-fac AS DEC NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.
DEF VAR v-line AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.

DEF BUFFER b-eb FOR eb.
DEF BUFFER b-ef FOR ef.

{cec/print4.i shared shared}
{cec/print42.i shared}
{sys/inc/ceprep.i}

{sys/inc/cerun.i C}
{sys/inc/ceprepprice.i}

DEF VAR v-orig-prep-mat LIKE prep-mat NO-UNDO.
DEF VAR v-orig-prep-lab LIKE prep-lab NO-UNDO.
DEF VAR v-sep-prep-tot AS DEC NO-UNDO.
DEF VAR v-sep-misc-tot AS DEC NO-UNDO.

find first xeb where xeb.company = xest.company 
                    and xeb.est-no eq xest.est-no
                    and xeb.form-no ne 0 no-error.

   output to value(outfile2).

   find first est-prep where est-prep.company = xest.company 
                         and est-prep.est-no = xest.est-no
                         and (est-prep.s-num eq v-form-no or (not vmclean2))
        and index("SON",est-prep.simon) > 0 no-lock no-error.

   if (xeb.chg-method ne "P" and fr-tot <> 0) or avail est-prep then
      PUT SKIP(1)
         SPACE(24) "B I L L A B L E    C H A R G E S" SKIP
         "Prep Description      Mat'l   Labor  Addt'l   Amtz   Charge    Cost/M Total Cost" SKIP.

   assign save-qty = qty
         tprep-mat = 0
         tprep-lab = 0
         tprep-tot = 0.

   for each est-prep where est-prep.company = xest.company 
                       and est-prep.est-no = xest.est-no
                       and (est-prep.s-num eq v-form-no or (not vmclean2))
         and index("SON",est-prep.simon) > 0 no-lock
       with frame ag no-box no-labels:

       qty = 0.
       FOR EACH xeb FIELDS(yld-qty)
           WHERE xeb.company     EQ xest.company 
             AND xeb.est-no      EQ est-prep.est-no
             AND xeb.form-no     EQ est-prep.s-num
             AND (xeb.blank-no   EQ est-prep.b-num OR
                  est-prep.b-num EQ 0)
           NO-LOCK:
           ASSIGN
              v-yld = if xeb.yld-qty lt 0 then -1 / xeb.yld-qty else xeb.yld-qty
              qty = qty + (tt-blk * v-yld).
       END.
      
       if est-prep.code ne "" then do:
          prep-add = est-prep.mkup / 100.
          if est-prep.amtz ne 0 then prep-atz = est-prep.amtz / 100.
          else prep-atz = 1.
          if est-prep.ml = true THEN
             ASSIGN
                prep-lab = 0
                prep-mat = est-prep.cost * est-prep.qty.
          else
             ASSIGN
                prep-mat = 0
                prep-lab = est-prep.cost * est-prep.qty.
      
          ASSIGN
             v-orig-prep-mat = prep-mat
             v-orig-prep-lab = prep-lab
             prep-tot = prep-mat + prep-lab.

          IF ceprepprice-chr EQ "Profit" THEN
             prep-tot  = prep-tot / (1 - prep-add) * prep-atz.
          ELSE
             prep-tot  = prep-tot * (1 + prep-add) * prep-atz.
             
          IF ceprep-cha EQ "Dollar" AND est-prep.simon EQ "S" AND
             prep-tot ne 0 THEN DO:
            ld-fac = prep-tot.
            {sys/inc/roundup.i prep-tot}
            ASSIGN
               ld-fac = prep-tot / ld-fac
               prep-mat = prep-mat * ld-fac
               prep-lab = prep-lab * ld-fac.
          END.
          ELSE IF ceprep-cha EQ "FiveDollar" AND
               prep-tot ne 0 THEN DO:
            ld-fac = prep-tot.
            {sys/inc/roundupfive.i prep-tot}
            ASSIGN
               ld-fac = prep-tot / ld-fac
               prep-mat = prep-mat * ld-fac
               prep-lab = prep-lab * ld-fac.
          END.
      
          IF ceprepprice-chr EQ "Profit" THEN
             ASSIGN
                tprep-mat = tprep-mat + (prep-mat / (1 - prep-add) * prep-atz)
                tprep-lab = tprep-lab + (prep-lab / (1 - prep-add) * prep-atz).
          ELSE
             ASSIGN
                tprep-mat = tprep-mat + (prep-mat * (1 + prep-add) * prep-atz)
                tprep-lab = tprep-lab + (prep-lab * (1 + prep-add) * prep-atz).

          tprep-tot = tprep-tot + prep-tot.
      
          create xprep.
          assign xprep.frm      = est-prep.s-num
                 xprep.blank-no = est-prep.b-num
                 xprep.qty      = est-prep.qty
                 xprep.std-cost = est-prep.cost
                 xprep.ml       = est-prep.ml
                 xprep.cost-m   = prep-tot / (qty / 1000)
                 xprep.simon    = est-prep.simon
                 xprep.code     = est-prep.CODE.
      
          display est-prep.dscr format "x(19)"
                  v-orig-prep-mat format "->>9.99"
                  v-orig-prep-lab format "->>9.99"
                  est-prep.mkup format ">>9.99" to 42 space(0) "%"
                  est-prep.amtz to 50 format ">>9.99" space(0) "%"
                  est-prep.simon format "X" to 58
                  prep-tot / (qty / 1000) to 69
                  prep-tot to 80 format ">>>,>>9.99"
                    when index("SO",est-prep.simon) > 0
                         "      N/C " when est-prep.simon = "N" @ prep-tot
                  SKIP WITH STREAM-IO.

          IF cerunc EQ "Protagon" AND
             est-prep.simon EQ "S" THEN
             DO:
                FIND FIRST b-eb WHERE
                     b-eb.company EQ est-prep.company AND
                     b-eb.est-no  EQ est-prep.est-no AND
                     b-eb.form-no EQ est-prep.s-num AND
                     b-eb.blank-no EQ est-prep.b-num
                     USE-INDEX est-qty
                     NO-LOCK NO-ERROR.

                IF NOT AVAIL b-eb THEN
                   FIND FIRST b-eb WHERE
                        b-eb.company EQ est-prep.company AND
                        b-eb.est-no  EQ est-prep.est-no AND
                        b-eb.form-no EQ est-prep.s-num
                        USE-INDEX est-qty
                        NO-LOCK NO-ERROR.
            
                CREATE tt-prep-sep.
                ASSIGN
                   tt-prep-sep.CODE = est-prep.CODE
                   tt-prep-sep.form-no = est-prep.s-num
                   tt-prep-sep.blank-no = est-prep.b-num
                   tt-prep-sep.item-name = IF AVAIL b-eb THEN b-eb.part-dscr1 ELSE ""
                   tt-prep-sep.sep-cost = prep-tot
                   tt-prep-sep.misc = NO.
            
                RELEASE tt-prep-sep.
             END.
       end.
   end.
       
   find first xeb where xeb.company = xest.company 
                    and xeb.est-no eq xest.est-no
                    and xeb.form-no ne 0 no-error.

   if xeb.chg-method ne "P" and fr-tot ne 0 then do:
      put "Freight"   at 1
           fr-tot      FORMAT ">>>9.99" to 27
           (fr-tot / (tt-blk / 1000)) TO 69
           fr-tot                  TO 80 SKIP.
      assign
       tprep-mat = tprep-mat + fr-tot
       tprep-tot = tprep-tot + fr-tot.
   end.

   tmpstore = "new".
   for each xef where xef.company = xest.company 
                  and xef.est-no    eq xest.est-no
                  and (xef.form-no eq v-form-no or (not vmclean)):

      if tmpstore = "new" then do i = 1 to 6:

         if index("SON",xef.mis-simon[i]) = 0 then next.
         tmpstore = "done".
         put skip(1).
         IF cerunc = "Protagon" THEN
            PUT "Miscellaneous Cost    Mat/F   Lab/F    Mat/M  Lab/M  Charge      OH% Total Cost" SKIP.
         ELSE    
            PUT "Miscellaneous Cost    Mat/F   Lab/F    Mat/M  Lab/M  Charge   Mrkup% Total Cost" SKIP.
         leave.
      end.

      if tmpstore = "new" then next.
      do i = 1 to 6 with frame ah down no-labels no-box:

         if index("SON",xef.mis-simon[i]) = 0 then next.

         if mis-cost[i] ne "" then do:
            qty = 0.
            FOR EACH xeb FIELDS(yld-qty)
                WHERE xeb.company      EQ xef.company
                  AND xeb.est-no       EQ xef.est-no
                  AND xeb.form-no      EQ xef.form-no
                  AND (xeb.blank-no    EQ xef.mis-bnum[i] OR
                       xef.mis-bnum[i] EQ 0)
                NO-LOCK:
              ASSIGN
               v-yld = IF xeb.yld-qty LT 0 THEN -1 / xeb.yld-qty ELSE xeb.yld-qty
               qty   = qty + tt-blk * v-yld.
            END.

            {cec/refest5aW.i MAT-QTY i "no-lock no-error"}

            if avail reftable then do v = 1 to EXTENT(reftable.val):
               if qty le reftable.val[v] then leave.
               if v = EXTENT(reftable.val) then do:
                  v = 0.
                  release reftable.
                  leave.
               end.
            end.

            if avail reftable then
               {cec/refest5aW.i MAT-CST i "no-lock no-error"}

            ASSIGN
               v-mat-cost = if avail reftable then reftable.val[v] else 0.

            IF ceprepprice-chr EQ "Profit" THEN
               mis-tot[5] = (xef.mis-matf[i] + (v-mat-cost * (qty / 1000))) /
                                               (1 - (xef.mis-mkup[i] / 100)).
            ELSE
               mis-tot[5] = (xef.mis-matf[i] + (v-mat-cost * (qty / 1000))) *
                                               (1 + (xef.mis-mkup[i] / 100)).

            /*IF ceprep-cha EQ "Dollar" THEN DO:
              {sys/inc/roundup.i mis-tot[5]}
            END.*/

            {cec/refest5aW.i LAB-QTY i "no-lock no-error"}

            if avail reftable then do v = 1 to EXTENT(reftable.val):
               if qty le reftable.val[v] then leave.
               if v = EXTENT(reftable.val) then do:
                  v = 0.
                  release reftable.
                  leave.
               end.
            end.

            if avail reftable then
               {cec/refest5aW.i LAB-CST i "no-lock no-error"}

            v-lab-cost = if avail reftable then reftable.val[v] else 0.

            IF ceprepprice-chr EQ "Profit" THEN
               mis-tot[6] = (xef.mis-labf[i] + (v-lab-cost * (qty / 1000))) /
                                               (1 - (xef.mis-mkup[i] / 100)).
            ELSE
               mis-tot[6] = (xef.mis-labf[i] + (v-lab-cost * (qty / 1000))) *
                                               (1 + (xef.mis-mkup[i] / 100)).

            prep-tot = mis-tot[5] + mis-tot[6].

            IF ceprep-cha EQ "Dollar" AND xef.mis-simon[i] EQ "S" AND
               prep-tot ne 0 THEN DO:
              ld-fac = prep-tot.
              {sys/inc/roundup.i prep-tot}
              ASSIGN
                 ld-fac = prep-tot / ld-fac
                 mis-tot[5] = mis-tot[5] * ld-fac
                 mis-tot[6] = mis-tot[6] * ld-fac.
            END.
            ELSE IF ceprep-cha EQ "FiveDollar" AND
                 prep-tot ne 0 THEN DO:
              ld-fac = prep-tot.
              {sys/inc/roundupfive.i prep-tot}
              ASSIGN
                 ld-fac = prep-tot / ld-fac
                 mis-tot[5] = mis-tot[5] * ld-fac
                 mis-tot[6] = mis-tot[6] * ld-fac.
            END.

	        if mis-tot[5] ne 0 then do:
	          create xprep.
	          assign xprep.frm      = xef.mis-snum[i]
		         xprep.blank-no = xef.mis-bnum[i]
		         xprep.qty      = 1
		         xprep.std-cost = mis-tot[5]
		         xprep.ml       = yes
		         xprep.cost-m   = mis-tot[5] / (qty / 1000)
		         xprep.simon    = xef.mis-simon[i]
		         xprep.code     = "MISM" + string(i,"9").
	        end.

	        if mis-tot[6] ne 0 then do:
	          create xprep.
	          assign xprep.frm      = xef.mis-snum[i]
		         xprep.blank-no = xef.mis-bnum[i]
		         xprep.qty      = 1
		         xprep.std-cost = mis-tot[6]
		         xprep.ml       = no
		         xprep.cost-m   = mis-tot[6] / (qty / 1000)
		         xprep.simon    = xef.mis-simon[i]
		         xprep.code     = "MISL" + string(i,"9").
	        end.

            IF cerunc EQ "Protagon" AND
            xef.mis-simon[i] EQ "S" THEN
            DO:
               FIND FIRST b-eb WHERE
                    b-eb.company EQ xef.company AND
                    b-eb.est-no  EQ xef.est-no AND
                    b-eb.form-no EQ xef.mis-snum[i] AND
                    b-eb.blank-no EQ xef.mis-bnum[i]
                    USE-INDEX est-qty
                    NO-LOCK NO-ERROR.

               IF NOT AVAIL b-eb THEN
                  FIND FIRST b-eb WHERE
                       b-eb.company EQ xef.company AND
                       b-eb.est-no  EQ xef.est-no AND
                       b-eb.form-no EQ xef.mis-snum[i]
                       USE-INDEX est-qty
                       NO-LOCK NO-ERROR.

               CREATE tt-prep-sep.
               ASSIGN
                  tt-prep-sep.CODE = xef.mis-cost[i]
                  tt-prep-sep.form-no = xef.mis-snum[i]
                  tt-prep-sep.blank-no = xef.mis-bnum[i]
                  tt-prep-sep.item-name = IF AVAIL b-eb THEN b-eb.part-dscr1 ELSE ""
                  tt-prep-sep.sep-cost = prep-tot
                  tt-prep-sep.misc = YES.

               RELEASE tt-prep-sep.
            END.

            display
             xef.mis-cost[i]  format "x(19)"
             xef.mis-matf[i]  format "->>9.99"
             xef.mis-labf[i]  format "->>9.99"
             v-mat-cost       format "->>>9.99" to 44
             v-lab-cost       format "->>>9.99" to 53
             xef.mis-simon[i] format "X" to 58
             xef.mis-mkup[i]  to 69
             prep-tot to 80   format ">>>,>>9.99" SKIP  WITH STREAM-IO.
         end.
      end.
   end.

   put skip(2).

   output close.

   FIND FIRST tt-prep-sep NO-LOCK NO-ERROR.
   IF cerunc EQ "Protagon" AND AVAIL tt-prep-sep AND ip-last-ef  THEN
   DO:
      output to value(outfile4) APPEND.

/*       IF vmclean2 THEN                         */
/*          FIND FIRST b-ef WHERE                 */
/*               b-ef.company EQ xest.company AND */
/*               b-ef.est-no EQ xest.est-no       */
/*               NO-LOCK NO-ERROR.                */

      
/*       IF not vmclean2 OR (AVAIL b-ef AND b-ef.form-no EQ v-form-no) THEN DO: */
       FIND FIRST tt-prep-sep WHERE tt-prep-sep.misc = YES NO-LOCK NO-ERROR.
       IF AVAIL tt-prep-sep THEN DO:

           PUT "BILLABLE MISC" "ITEM" AT 21 "COST" AT 59 SKIP.
          /*PUT "Misc Charge Code     Item Name                            Cost" SKIP.*/
         /*       END. */

           FOR EACH tt-prep-sep WHERE tt-prep-sep.misc = YES:
             PUT
                 tt-prep-sep.CODE FORMAT "X(20)"
                 tt-prep-sep.item-name FORMAT "X(30)"
                 tt-prep-sep.sep-cost FORMAT ">>>,>>9.99" at 53
                 /*WITH STREAM-IO NO-BOX NO-LABELS*/ SKIP .
              v-sep-misc-tot = v-sep-misc-tot + ROUND(tt-prep-sep.sep-cost, 2).
           END.
          PUT "TOTAL:" v-sep-misc-tot FORMAT ">>>,>>9.99" AT 53 SKIP(1).
       END.


       FIND FIRST tt-prep-sep WHERE tt-prep-sep.misc = NO NO-LOCK NO-ERROR.
       IF AVAIL tt-prep-sep THEN DO:
/*       IF not vmclean2 /* OR (AVAIL b-ef AND b-ef.form-no EQ v-form-no) */ THEN DO: */
        
         PUT "BILLABLE PREP" SKIP.
        /*PUT "Sep Charge Prep Code Item Name                            Cost" SKIP.*/
/*       END. */

         FOR EACH tt-prep-sep WHERE tt-prep-sep.misc = NO:
            PUT
                tt-prep-sep.CODE FORMAT "X(20)"
                tt-prep-sep.item-name FORMAT "X(30)"
                tt-prep-sep.sep-cost FORMAT ">>>,>>9.99" at 53
                /*WITH STREAM-IO NO-BOX NO-LABELS*/ SKIP .
             v-sep-prep-tot = v-sep-prep-tot + ROUND(tt-prep-sep.sep-cost, 2).
         END.
         PUT "TOTAL:" v-sep-prep-tot format ">>>,>>9.99" AT 53 SKIP(1).
       END.
/*       IF vmclean2 THEN                                                   */
/*          FIND LAST b-ef WHERE                                            */
/*               b-ef.company EQ xest.company AND                           */
/*               b-ef.est-no EQ xest.est-no                                 */
/*               NO-LOCK NO-ERROR.                                          */
/*                                                                          */
/*       IF not vmclean2 OR (AVAIL b-ef AND b-ef.form-no EQ v-form-no) THEN */
/*       DO:                                                                */
/*          IF vmclean2 THEN                                                */
/*          DO:                                                             */
         
/*             OUTPUT CLOSE.                                                    */
/*             INPUT FROM VALUE(outfile4).                                      */
/*             repeat:                                                          */
/*                ASSIGN                                                        */
/*                   v-line = ""                                                */
/*                   v-count = v-count + 1.                                     */
/*                                                                              */
/*                IMPORT UNFORMATTED v-line.                                    */
/*                IF v-line EQ "" OR v-count EQ 1 THEN NEXT.                    */
/*                v-sep-prep-tot = v-sep-prep-tot + DECIMAL(SUBSTR(v-line,53)). */
/*             end.                                                             */
/*                                                                              */
/*             INPUT CLOSE.                                                     */
/*          END. */

/*          IF vmclean2 THEN                                                                  */
/*             OUTPUT TO VALUE(outfile4) APPEND.                                              */
/*                                                                                            */
/*          DISPLAY                                                                           */
/*          SKIP (1) "Separate Charges Total Cost: " v-sep-prep-tot format ">>>,>>9.99" AT 53 */
/*          SKIP(1)                                                                           */
/*          SKIP WITH STREAM-IO NO-BOX NO-LABELS.                                             */
     /* END. */

      OUTPUT CLOSE.
      EMPTY TEMP-TABLE tt-prep-sep.

   END.

qty = save-qty.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
