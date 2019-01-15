/* ----------------------------------------------- cec/box/pr42mis2.p 9/93 cd */
DEFINE INPUT PARAMETER ip-last-ef AS LOG NO-UNDO.

DEF SHARED VAR cocode AS cha NO-UNDO.
DEF SHARED VAR locode AS cha NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO .
DEF VAR i AS INT NO-UNDO.
DEF VAR tmpstore AS cha NO-UNDO.

DEF SHARED BUFFER xest FOR est.
DEF SHARED BUFFER xef  FOR ef.
DEF SHARED BUFFER xeb  FOR eb.

DEF VAR v AS INT NO-UNDO.
DEF VAR v-mat-cost AS DEC NO-UNDO.
DEF VAR v-lab-cost AS DEC NO-UNDO.
DEF VAR v-yld AS DEC NO-UNDO.
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

FIND FIRST xeb WHERE xeb.company = xest.company 
                    AND xeb.est-no EQ xest.est-no
                    AND xeb.form-no NE 0 NO-ERROR.

   OUTPUT to value(outfile2).

   FIND FIRST est-prep WHERE est-prep.company = xest.company 
                         AND est-prep.est-no = xest.est-no
                         AND (est-prep.s-num EQ v-form-no OR (NOT vmclean2))
        AND index("SON",est-prep.simon) > 0 NO-LOCK NO-ERROR.

   IF (xeb.chg-method NE "P" AND fr-tot <> 0) OR AVAIL est-prep THEN
      PUT SKIP(1)
         SPACE(24) "B I L L A B L E    C H A R G E S" SKIP
         "Prep Description      Mat'l   Labor  Addt'l   Amtz   Charge    Cost/M Total Cost" SKIP.

   ASSIGN save-qty = qty
         tprep-mat = 0
         tprep-lab = 0
         tprep-tot = 0.

   FOR EACH est-prep WHERE est-prep.company = xest.company 
                       AND est-prep.est-no = xest.est-no
                       AND (est-prep.s-num EQ v-form-no OR (NOT vmclean2))
         AND index("SON",est-prep.simon) > 0 NO-LOCK
       WITH FRAME ag NO-BOX NO-LABELS:

       qty = 0.
       FOR EACH xeb FIELDS(quantityPerSet)
           WHERE xeb.company     EQ xest.company 
             AND xeb.est-no      EQ est-prep.est-no
             AND xeb.form-no     EQ est-prep.s-num
             AND (xeb.blank-no   EQ est-prep.b-num OR
                  est-prep.b-num EQ 0)
           NO-LOCK:
           ASSIGN
              v-yld = IF xeb.quantityPerSet LT 0 THEN -1 / xeb.quantityPerSet ELSE xeb.quantityPerSet
              qty = qty + (tt-blk * v-yld).
       END.
      
       IF est-prep.code NE "" THEN DO:
          prep-add = est-prep.mkup / 100.
          IF est-prep.amtz NE 0 THEN prep-atz = est-prep.amtz / 100.
          ELSE prep-atz = 1.
          IF est-prep.ml = TRUE THEN
             ASSIGN
                prep-lab = 0
                prep-mat = est-prep.cost * est-prep.qty.
          ELSE
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
             prep-tot NE 0 THEN DO:
            ld-fac = prep-tot.
            {sys/inc/roundup.i prep-tot}
            ASSIGN
               ld-fac = prep-tot / ld-fac
               prep-mat = prep-mat * ld-fac
               prep-lab = prep-lab * ld-fac.
          END.
          ELSE IF ceprep-cha EQ "FiveDollar" AND
               prep-tot NE 0 THEN DO:
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
      
          CREATE xprep.
          ASSIGN xprep.frm      = est-prep.s-num
                 xprep.blank-no = est-prep.b-num
                 xprep.qty      = est-prep.qty
                 xprep.std-cost = est-prep.cost
                 xprep.ml       = est-prep.ml
                 xprep.cost-m   = prep-tot / (qty / 1000)
                 xprep.simon    = est-prep.simon
                 xprep.code     = est-prep.CODE.
      
          DISPLAY est-prep.dscr FORMAT "x(17)"
                  v-orig-prep-mat FORMAT "->>>>9.99"
                  v-orig-prep-lab FORMAT "->>9.99"
                  est-prep.mkup FORMAT "->>9.99" TO 43 SPACE(0) "%"
                  est-prep.amtz TO 50 FORMAT ">>9.99" SPACE(0) "%"
                  est-prep.simon FORMAT "X" TO 58
                  prep-tot / (qty / 1000) TO 70
                  prep-tot TO 82 FORMAT ">>>,>>9.99"
                    WHEN INDEX("SO",est-prep.simon) > 0
                         "      N/C " WHEN est-prep.simon = "N" @ prep-tot
                  SKIP WITH STREAM-IO.

          IF LOOKUP(cerunc,"Protagon,CERunC 3") NE 0 /*cerunc EQ "Protagon"*/ AND
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
       END.
   END.
       
   FIND FIRST xeb WHERE xeb.company = xest.company 
                    AND xeb.est-no EQ xest.est-no
                    AND xeb.form-no NE 0 NO-ERROR.

   IF xeb.chg-method NE "P" AND fr-tot NE 0 THEN DO:
      PUT "Freight"   AT 1
           fr-tot      FORMAT ">>>9.99" TO 27
           (fr-tot / (tt-blk / 1000)) TO 69
           fr-tot                  TO 80 SKIP.
      ASSIGN
       tprep-mat = tprep-mat + fr-tot
       tprep-tot = tprep-tot + fr-tot.
   END.

   tmpstore = "new".
   FOR EACH xef WHERE xef.company = xest.company 
                  AND xef.est-no    EQ xest.est-no
                  AND (xef.form-no EQ v-form-no OR (NOT vmclean)):

      IF tmpstore = "new" THEN DO i = 1 TO 6:

         IF INDEX("SON",xef.mis-simon[i]) = 0 THEN NEXT.
         tmpstore = "done".
         PUT SKIP(1).
         IF LOOKUP(cerunc,"Protagon,CERunC 3") NE 0 /*cerunc = "Protagon"*/ THEN
            PUT "Miscellaneous Cost    Mat/F   Lab/F    Mat/M  Lab/M  Charge      OH% Total Cost" SKIP.
         ELSE    
            PUT "Miscellaneous Cost    Mat/F   Lab/F    Mat/M  Lab/M  Charge   Mrkup% Total Cost" SKIP.
         LEAVE.
      END.

      IF tmpstore = "new" THEN NEXT.
      DO i = 1 TO 6 WITH FRAME ah DOWN NO-LABELS NO-BOX:

         IF INDEX("SON",xef.mis-simon[i]) = 0 THEN NEXT.

         IF mis-cost[i] NE "" THEN DO:
            qty = 0.
            FOR EACH xeb FIELDS(quantityPerSet)
                WHERE xeb.company      EQ xef.company
                  AND xeb.est-no       EQ xef.est-no
                  AND xeb.form-no      EQ xef.form-no
                  AND (xeb.blank-no    EQ xef.mis-bnum[i] OR
                       xef.mis-bnum[i] EQ 0)
                NO-LOCK:
              ASSIGN
               v-yld = IF xeb.quantityPerSet LT 0 THEN -1 / xeb.quantityPerSet ELSE xeb.quantityPerSet
               qty   = qty + tt-blk * v-yld.
            END.

            {cec/refest5a.i MAT-QTY i "no-lock no-error"}

            IF AVAIL reftable THEN DO v = 1 TO EXTENT(reftable.val):
               IF qty LE reftable.val[v] THEN LEAVE.
               IF v = EXTENT(reftable.val) THEN DO:
                  v = 0.
                  RELEASE reftable.
                  LEAVE.
               END.
            END.

            IF AVAIL reftable THEN
               {cec/refest5a.i MAT-CST i "no-lock no-error"}

            ASSIGN
               v-mat-cost = IF AVAIL reftable THEN reftable.val[v] ELSE 0.

            IF ceprepprice-chr EQ "Profit" THEN
               mis-tot[5] = (xef.mis-matf[i] + (v-mat-cost * (qty / 1000))) /
                                               (1 - (xef.mis-mkup[i] / 100)).
            ELSE
               mis-tot[5] = (xef.mis-matf[i] + (v-mat-cost * (qty / 1000))) *
                                               (1 + (xef.mis-mkup[i] / 100)).

            /*IF ceprep-cha EQ "Dollar" THEN DO:
              {sys/inc/roundup.i mis-tot[5]}
            END.*/

            {cec/refest5a.i LAB-QTY i "no-lock no-error"}

            IF AVAIL reftable THEN DO v = 1 TO EXTENT(reftable.val):
               IF qty LE reftable.val[v] THEN LEAVE.
               IF v = EXTENT(reftable.val) THEN DO:
                  v = 0.
                  RELEASE reftable.
                  LEAVE.
               END.
            END.

            IF AVAIL reftable THEN
               {cec/refest5a.i LAB-CST i "no-lock no-error"}

            v-lab-cost = IF AVAIL reftable THEN reftable.val[v] ELSE 0.

            IF ceprepprice-chr EQ "Profit" THEN
               mis-tot[6] = (xef.mis-labf[i] + (v-lab-cost * (qty / 1000))) /
                                               (1 - (xef.mis-mkup[i] / 100)).
            ELSE
               mis-tot[6] = (xef.mis-labf[i] + (v-lab-cost * (qty / 1000))) *
                                               (1 + (xef.mis-mkup[i] / 100)).

            prep-tot = mis-tot[5] + mis-tot[6].

            IF ceprep-cha EQ "Dollar" AND xef.mis-simon[i] EQ "S" AND
               prep-tot NE 0 THEN DO:
              ld-fac = prep-tot.
              {sys/inc/roundup.i prep-tot}
              ASSIGN
                 ld-fac = prep-tot / ld-fac
                 mis-tot[5] = mis-tot[5] * ld-fac
                 mis-tot[6] = mis-tot[6] * ld-fac.
            END.
            ELSE IF ceprep-cha EQ "FiveDollar" AND
                 prep-tot NE 0 THEN DO:
              ld-fac = prep-tot.
              {sys/inc/roundupfive.i prep-tot}
              ASSIGN
                 ld-fac = prep-tot / ld-fac
                 mis-tot[5] = mis-tot[5] * ld-fac
                 mis-tot[6] = mis-tot[6] * ld-fac.
            END.

	        IF mis-tot[5] NE 0 THEN DO:
	          CREATE xprep.
	          ASSIGN xprep.frm      = xef.mis-snum[i]
		         xprep.blank-no = xef.mis-bnum[i]
		         xprep.qty      = 1
		         xprep.std-cost = mis-tot[5]
		         xprep.ml       = YES
		         xprep.cost-m   = mis-tot[5] / (qty / 1000)
		         xprep.simon    = xef.mis-simon[i]
		         xprep.code     = "MISM" + string(i,"9").
	        END.

	        IF mis-tot[6] NE 0 THEN DO:
	          CREATE xprep.
	          ASSIGN xprep.frm      = xef.mis-snum[i]
		         xprep.blank-no = xef.mis-bnum[i]
		         xprep.qty      = 1
		         xprep.std-cost = mis-tot[6]
		         xprep.ml       = NO
		         xprep.cost-m   = mis-tot[6] / (qty / 1000)
		         xprep.simon    = xef.mis-simon[i]
		         xprep.code     = "MISL" + string(i,"9").
	        END.

            IF LOOKUP(cerunc,"Protagon,CERunC 3") NE 0 /*cerunc EQ "Protagon"*/ AND
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

            DISPLAY
             xef.mis-cost[i]  FORMAT "x(19)"
             xef.mis-matf[i]  FORMAT "->>9.99"
             xef.mis-labf[i]  FORMAT "->>9.99"
             v-mat-cost       FORMAT "->>>9.99" TO 44
             v-lab-cost       FORMAT "->>>9.99" TO 53
             xef.mis-simon[i] FORMAT "X" TO 58
             xef.mis-mkup[i]  TO 69
             prep-tot TO 80   FORMAT ">>>,>>9.99" SKIP  WITH STREAM-IO.
         END.
      END.
   END.

   PUT SKIP(2).

   OUTPUT close.

   FIND FIRST tt-prep-sep NO-LOCK NO-ERROR.
   IF LOOKUP(cerunc,"Protagon,CERunC 3") NE 0 /*cerunc EQ "Protagon"*/ AND AVAIL tt-prep-sep AND ip-last-ef  THEN
   DO:
      OUTPUT to value(outfile4) .

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
                 tt-prep-sep.sep-cost FORMAT ">>>,>>9.99" AT 53
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
                tt-prep-sep.sep-cost FORMAT ">>>,>>9.99" AT 53
                /*WITH STREAM-IO NO-BOX NO-LABELS*/ SKIP .
             v-sep-prep-tot = v-sep-prep-tot + ROUND(tt-prep-sep.sep-cost, 2).
         END.
         PUT "TOTAL:" v-sep-prep-tot FORMAT ">>>,>>9.99" AT 53 SKIP(1).
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
