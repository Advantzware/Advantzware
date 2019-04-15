/* --------------------------------------------------- cec/pr4-prp.p 9/93 cd  */
/* Prep File calc & Display for estimates                                     */
/* -------------------------------------------------------------------------- */

def shared var cocode as cha no-undo.
def shared var qty as int NO-UNDO.
def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{cec/print4.i shared shared}

DEF VAR ld-fac AS DEC NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.
DEF VAR v-prep-mat-orig LIKE prep-mat NO-UNDO.
DEF VAR v-prep-lab-orig LIKE prep-lab NO-UNDO.

{cec/msfcalc.i}
{cec/rollfac.i}
{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}

   assign
    v-fac-hdr = "Cost/M" + (if v-rollfac then "SF" else "")
    v-fac-hdr = fill(" ",8 - length(trim(v-fac-hdr))) + trim(v-fac-hdr).

   for each est-prep where est-prep.company = xest.company and
                           est-prep.est-no = xest.est-no  :
      if index("IM",est-prep.simon) > 0 and est-prep.code ne "" then do:
         put "Prep Description      Mat'l   Labor  Addt'l   Amtz   Charge"
             space(2) v-fac-hdr format "x(8)" space(1) "Total Cost" skip.
         leave.
      end.
   end.

   /* prep */
   ASSIGN
      tprep-mat = 0
      tprep-lab = 0
      tprep-tot = 0
    .

   for each est-prep where est-prep.company = xest.company and
                           est-prep.est-no = xest.est-no 
                           with frame ad down no-labels no-box:
      /* only (i)ntegrate and (m)arkup lines are done here */
      if index("SON",est-prep.simon) > 0 then next.
      if est-prep.code ne "" then do:
         ASSIGN
            prep-add = est-prep.mkup / 100
            prep-atz = est-prep.amtz / 100.

         if est-prep.ml = true THEN
            ASSIGN
               prep-lab = 0
               prep-mat = est-prep.cost * est-prep.qty.
         else
            ASSIGN
               prep-mat = 0
               prep-lab = est-prep.cost * est-prep.qty.

         ASSIGN
            v-prep-mat-orig = prep-mat
            v-prep-lab-orig = prep-lab
            prep-tot = prep-mat + prep-lab.

        IF est-prep.simon = 'M' THEN DO:
            dMCostToExcludePrep = dMCostToExcludePrep + prep-tot.
            IF ceprepprice-chr EQ 'Profit' THEN 
                dMPriceToAddPrep = dMPriceToAddPrep + prep-tot / (1 - prep-add) * prep-atz.
            ELSE 
                dMPriceToAddPrep = dMPriceToAddPrep + prep-tot * (1 + prep-add) * prep-atz.
        END.
         ELSE IF ceprepprice-chr EQ "Profit" THEN
            prep-tot  = prep-tot / (1 - prep-add) * prep-atz.
         ELSE
            prep-tot  = prep-tot * (1 + prep-add) * prep-atz.

         IF ceprep-cha EQ "FiveDollar" AND
            prep-tot NE 0 THEN DO:
            ld-fac = prep-tot.
            {sys/inc/roundupfive.i prep-tot}
            ASSIGN
               ld-fac = prep-tot / ld-fac
               prep-mat = prep-mat * ld-fac
               prep-lab = prep-lab * ld-fac.
         END.

          IF est-prep.simon = 'M' THEN
           ASSIGN 
                tprep-mat = tprep-mat + prep-mat * prep-atz
                tprep-lab = tprep-lab + prep-lab * prep-atz
                .
         ELSE IF ceprepprice-chr EQ "Profit" THEN
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

         display est-prep.dscr format "x(17)"
                 v-prep-mat-orig format "->>>>9.99"
                 v-prep-lab-orig format "->>9.99"
                 est-prep.mkup format "->>9.99" to 43 space(0) "%"
                 est-prep.amtz to 51 format ">>9.99" space(0) "%"
                 est-prep.simon format "X" to 58
                 prep-tot / (qty / 1000) / v-sqft-fac to 69
                 prep-tot to 82 format ">>>,>>9.99" skip 
                 with stream-io width 82.

      end.
   end.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
