/* ------------------------------------------------- cec/pr4-mis2.p 01/97 JLF */

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def shared var qty as int NO-UNDO.
def var i as int no-undo.

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{cec/print4.i shared shared}

{cec/msfcalc.i}

{cec/rollfac.i}

def shared var v-form-no like ef.form-no no-undo.

def var v as int NO-UNDO.
def var v-mat-cost as dec NO-UNDO.
def var v-lab-cost as dec NO-UNDO.
DEF VAR ld-fac AS DEC NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.
DEF VAR v-orig-prep-mat LIKE prep-mat NO-UNDO.
DEF VAR v-orig-prep-lab LIKE prep-lab NO-UNDO.
DEF VAR v-sep-prep-tot AS DEC NO-UNDO.



{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}
{sys/inc/cerun.i C}

   output to value(outfile2).

   assign
    v-fac-hdr = "Cost/M" + (if v-rollfac then "SF" else "")
    v-fac-hdr = fill(" ",8 - length(trim(v-fac-hdr))) + trim(v-fac-hdr).

   find first est-prep where est-prep.company = xest.company 
                         and est-prep.est-no = xest.est-no 
                         and index("SON",est-prep.simon) > 0
      no-lock no-error.
   if (xeb.chg-method ne "P" and fr-tot ne 0) or avail est-prep then
      put skip(1)
          space(24) "B I L L A B L E    C H A R G E S" skip
          "Prep Description      Mat'l   Labor  Addt'l   Amtz   Charge"
          space(2) v-fac-hdr format "x(8)" space(1) "Total Cost" skip.

   ASSIGN
      tprep-mat = 0
      tprep-lab = 0
      tprep-tot = 0.

   for each est-prep where est-prep.company = xest.company 
                         and est-prep.est-no = xest.est-no 
                         and index("SON",est-prep.simon) > 0
   with frame ag no-box no-labels:
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
                xprep.code     = est-prep.code.

         display est-prep.dscr format "x(19)"
                 v-orig-prep-mat format "->>9.99"
                 v-orig-prep-lab format "->>9.99"
                 est-prep.mkup format ">>9.99" to 42 space(0) "%"
                 est-prep.amtz to 50 format ">>9.99" space(0) "%"
                 est-prep.simon format "X" to 58
                 prep-tot / (qty / 1000) / v-sqft-fac to 69
                 prep-tot to 80 format ">>>,>>9.99" 
                    when index("SO",est-prep.simon) > 0
                    "      N/C " when est-prep.simon = "N" @ prep-tot  skip with stream-io.

         IF cerunc EQ "Protagon" AND
            est-prep.simon EQ "S" THEN
            DO:
               CREATE tt-prep-sep.
               ASSIGN
                  tt-prep-sep.CODE = est-prep.CODE
                  tt-prep-sep.form-no = est-prep.s-num
                  tt-prep-sep.blank-no = est-prep.b-num
                  tt-prep-sep.item-name = xeb.part-dscr1
                  tt-prep-sep.sep-cost = prep-tot
                  tt-prep-sep.misc = NO.

               RELEASE tt-prep-sep.
            END.
      end.
   end.

   if xeb.chg-method ne "P" and fr-tot ne 0 then do:
      put "Freight"   at 1
           fr-tot      FORMAT ">>>9.99" to 27
           (fr-tot / (qty / 1000)) TO 69
           fr-tot                  TO 80 format ">>>,>>9.99" SKIP.
      assign
       tprep-mat = tprep-mat + fr-tot
       tprep-tot = tprep-tot + fr-tot.
   end.

   do i = 1 to 6:
      if index("SON",xef.mis-simon[i]) = 0 then next.
      put skip(1).
      IF cerunc = "Protagon" THEN
          PUT "Miscellaneous Cost    Mat/F   Lab/F    Mat/M    Lab/M  Charge     OH% Total Cost"           skip .
      ELSE
          PUT "Miscellaneous Cost    Mat/F   Lab/F    Mat/M    Lab/M  Charge  Mrkup% Total Cost"           skip .
      leave.
   end.
   do i = 1 to 6 with frame ah down no-labels no-box:
      if index("SON",xef.mis-simon[i]) = 0 then next.

      if mis-cost[i] ne "" then do:
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

         ASSIGN
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
               CREATE tt-prep-sep.
               ASSIGN
                  tt-prep-sep.CODE = xef.mis-cost[i]
                  tt-prep-sep.form-no = xef.mis-snum[i]
                  tt-prep-sep.blank-no = xef.mis-bnum[i]
                  tt-prep-sep.item-name = xeb.part-dscr1
                  tt-prep-sep.sep-cost = prep-tot
                  tt-prep-sep.misc = YES.

               RELEASE tt-prep-sep.
            END.

         display xef.mis-cost[i]  format "x(19)"
                 xef.mis-matf[i]  format "->>9.99"
                 xef.mis-labf[i]  format "->>9.99"
                 v-mat-cost       format "->>>9.99" to 44
                 v-lab-cost       format "->>>9.99" to 53
                 xef.mis-simon[i] format "X" to 58
                 xef.mis-mkup[i]  to 69
                 prep-tot format ">>>,>>9.99" to 80 skip with stream-io.
      end.
   end.

   put skip(2).
   output close.
   FIND FIRST tt-prep-sep NO-LOCK NO-ERROR.
   IF cerunc EQ "Protagon" AND AVAIL tt-prep-sep THEN
   DO:

      output to value(outfile4).
      
      PUT "BILLABLE MISC" "ITEM" AT 21 "COST" AT 59 SKIP.
      /*PUT "Sep Charge Prep Code Item Name                            Cost".*/

      FOR EACH tt-prep-sep WHERE tt-prep-sep.misc = YES:

         v-sep-prep-tot = v-sep-prep-tot + tt-prep-sep.sep-cost.
   
         DISPLAY
            tt-prep-sep.CODE FORMAT "X(20)"
            tt-prep-sep.item-name FORMAT "X(30)"
            tt-prep-sep.sep-cost FORMAT ">>>,>>9.99"
            WITH STREAM-IO NO-BOX NO-LABELS.
      END.
      DISPLAY "TOTAL:"
          v-sep-prep-tot format ">>>,>>9.99" AT 53
          SKIP(1)
          SKIP WITH STREAM-IO NO-BOX NO-LABELS.
      v-sep-prep-tot = 0.
      PUT "BILLABLE PREP" SKIP.
      /*PUT "Sep Charge Prep Code Item Name                            Cost".*/

      FOR EACH tt-prep-sep WHERE tt-prep-sep.misc = NO:

         v-sep-prep-tot = v-sep-prep-tot + tt-prep-sep.sep-cost.
   
         DISPLAY
            tt-prep-sep.CODE FORMAT "X(20)"
            tt-prep-sep.item-name FORMAT "X(30)"
            tt-prep-sep.sep-cost FORMAT ">>>,>>9.99"
            WITH STREAM-IO NO-BOX NO-LABELS.
      END.
       DISPLAY "TOTAL:"
          v-sep-prep-tot format ">>>,>>9.99" AT 53
         SKIP(1)
         SKIP WITH STREAM-IO NO-BOX NO-LABELS.
/*       DISPLAY                                                                     */
/*          SKIP                                                                     */
/*          "Separate Charges Total Cost: " v-sep-prep-tot format ">>>,>>9.99" AT 53 */
/*          SKIP(1)                                                                  */
/*          SKIP WITH STREAM-IO NO-BOX NO-LABELS.                                    */

      OUTPUT CLOSE.
   END.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
