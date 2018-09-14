/* ------------------------------------------------- cec/pr4-mis2.p 01/97 JLF */

DEF SHARED VAR cocode AS cha NO-UNDO.
DEF SHARED VAR locode AS cha NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO.
DEF VAR i AS INT NO-UNDO.

DEF SHARED BUFFER xest FOR est.
DEF SHARED BUFFER xef FOR ef.
DEF SHARED BUFFER xeb FOR eb.

{cec/print4.i shared shared}

{cec/msfcalc.i}

{cec/rollfac.i}

DEF SHARED VAR v-form-no LIKE ef.form-no NO-UNDO.

DEF VAR v AS INT NO-UNDO.
DEF VAR v-mat-cost AS DEC NO-UNDO.
DEF VAR v-lab-cost AS DEC NO-UNDO.
DEF VAR ld-fac AS DEC NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.
DEF VAR v-orig-prep-mat LIKE prep-mat NO-UNDO.
DEF VAR v-orig-prep-lab LIKE prep-lab NO-UNDO.
DEF VAR v-sep-prep-tot AS DEC NO-UNDO.



{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}
{sys/inc/cerun.i C}

   OUTPUT to value(outfile2).

   ASSIGN
    v-fac-hdr = "Cost/M" + (IF v-rollfac THEN "SF" ELSE "")
    v-fac-hdr = FILL(" ",8 - length(TRIM(v-fac-hdr))) + trim(v-fac-hdr).

   FIND FIRST est-prep WHERE est-prep.company = xest.company 
                         AND est-prep.est-no = xest.est-no 
                         AND index("SON",est-prep.simon) > 0
      NO-LOCK NO-ERROR.
   IF (xeb.chg-method NE "P" AND fr-tot NE 0) OR AVAIL est-prep THEN
      PUT SKIP(1)
          SPACE(24) "B I L L A B L E    C H A R G E S" SKIP
          "Prep Description      Mat'l   Labor  Addt'l   Amtz   Charge"
          SPACE(2) v-fac-hdr FORMAT "x(8)" SPACE(1) "Total Cost" SKIP.

   ASSIGN
      tprep-mat = 0
      tprep-lab = 0
      tprep-tot = 0.

   FOR EACH est-prep WHERE est-prep.company = xest.company 
                         AND est-prep.est-no = xest.est-no 
                         AND index("SON",est-prep.simon) > 0
   WITH FRAME ag NO-BOX NO-LABELS:
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
                xprep.code     = est-prep.code.

         DISPLAY est-prep.dscr FORMAT "x(17)"
                 v-orig-prep-mat FORMAT "->>>>9.99"
                 v-orig-prep-lab FORMAT "->>9.99"
                 est-prep.mkup FORMAT "->>9.99" TO 43 SPACE(0) "%"
                 est-prep.amtz TO 50 FORMAT ">>9.99" SPACE(0) "%"
                 est-prep.simon FORMAT "X" TO 58
                 prep-tot / (qty / 1000) / v-sqft-fac TO 70
                 prep-tot TO 82 FORMAT ">>>,>>9.99" 
                    WHEN INDEX("SO",est-prep.simon) > 0
                    "      N/C " WHEN est-prep.simon = "N" @ prep-tot  SKIP WITH STREAM-IO.

         IF LOOKUP(cerunc,"Protagon,CERunC 3") NE 0 /*cerunc EQ "Protagon"*/ AND
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
      END.
   END.

   IF xeb.chg-method NE "P" AND fr-tot NE 0 THEN DO:
      PUT "Freight"   AT 1
           fr-tot      FORMAT ">>>9.99" TO 27
           (fr-tot / (qty / 1000)) TO 69
           fr-tot                  TO 80 FORMAT ">>>,>>9.99" SKIP.
      ASSIGN
       tprep-mat = tprep-mat + fr-tot
       tprep-tot = tprep-tot + fr-tot.
   END.

   DO i = 1 TO 6:
      IF INDEX("SON",xef.mis-simon[i]) = 0 THEN NEXT.
      PUT SKIP(1).
      IF LOOKUP(cerunc,"Protagon,CERunC 3") NE 0 /*cerunc = "Protagon"*/ THEN
          PUT "Miscellaneous Cost    Mat/F   Lab/F    Mat/M    Lab/M  Charge     OH% Total Cost"           SKIP .
      ELSE
          PUT "Miscellaneous Cost    Mat/F   Lab/F    Mat/M    Lab/M  Charge  Mrkup% Total Cost"           SKIP .
      LEAVE.
   END.
   DO i = 1 TO 6 WITH FRAME ah DOWN NO-LABELS NO-BOX:
      IF INDEX("SON",xef.mis-simon[i]) = 0 THEN NEXT.

      IF mis-cost[i] NE "" THEN DO:
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

         ASSIGN
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

         DISPLAY xef.mis-cost[i]  FORMAT "x(19)"
                 xef.mis-matf[i]  FORMAT "->>9.99"
                 xef.mis-labf[i]  FORMAT "->>9.99"
                 v-mat-cost       FORMAT "->>>9.99" TO 44
                 v-lab-cost       FORMAT "->>>9.99" TO 53
                 xef.mis-simon[i] FORMAT "X" TO 58
                 xef.mis-mkup[i]  TO 69
                 prep-tot FORMAT ">>>,>>9.99" TO 80 SKIP WITH STREAM-IO.
      END.
   END.

   PUT SKIP(2).
   OUTPUT close.
   FIND FIRST tt-prep-sep NO-LOCK NO-ERROR.
   IF LOOKUP(cerunc,"Protagon,CERunC 3") NE 0 /*cerunc EQ "Protagon"*/ AND AVAIL tt-prep-sep THEN
   DO:

      OUTPUT to value(outfile4).
      
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
          v-sep-prep-tot FORMAT ">>>,>>9.99" AT 53
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
          v-sep-prep-tot FORMAT ">>>,>>9.99" AT 53
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
