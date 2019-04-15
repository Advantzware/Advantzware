/* ---------------------------------------------- cec/box/pr42-prp.p 9/93 cd  */
/* Prep File calc & Display for estimates                                     */
/* -------------------------------------------------------------------------- */

def shared var cocode as cha no-undo.
def shared var qty as int NO-UNDO.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{cec/print4.i shared shared}
{cec/print42.i shared}

def var v-yld as DEC NO-UNDO.
DEF VAR ld-fac AS DEC NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.
DEF VAR v-orig-prep-mat LIKE prep-mat NO-UNDO.
DEF VAR v-orig-prep-lab LIKE prep-lab NO-UNDO.

{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}

find first xeb where xeb.company = xest.company 
                 and xeb.est-no   eq xest.est-no
                 and xeb.form-no eq v-form-no
    no-lock.
    
for each blk where blk.snum eq v-form-no or (not vmclean2):
   /* {sys/inc/debug.i "Processing BLK workfiles...."} */
   find first xjob
       where xjob.i-no eq blk.id
         and xjob.qty  eq blk.qreq
       no-error.
   if not avail xjob then do:
      create xjob.
      assign
       xjob.i-no     = blk.id
       xjob.qty      = blk.qreq
       xjob.form-no  = xeb.form-no
       xjob.blank-no = xeb.blank-no.
   end.
   assign
    xjob.mat      = blk.cost - blk.lab
    xjob.lab      = blk.lab
    xjob.qty      = blk.qreq
    xjob.cust-no  = xeb.cust-no
    xjob.pct      = 1.00
    xjob.stock-no = xeb.stock-no.
end.

v-yld = if xeb.quantityPerSet lt 0 then -1 / xeb.quantityPerSet else xeb.quantityPerSet.

display "TOTAL  DIRECT  MATERIALS "
        dm-tot[5] /
         (tt-blk * (if xest.form-qty eq 1 or vmclean2 then v-yld else 1) / 1000)
                  format ">>>,>>9.99"   to 70
        dm-tot[5] format ">>>,>>9.99" to 80
        skip(1)
        with frame ac5 no-labels no-box stream-io.

for each est-prep where est-prep.company = xest.company 
                    and est-prep.est-no = xest.est-no
                    and (est-prep.s-num eq v-form-no or (not vmclean2)):
   if index("IM",est-prep.simon) > 0 and est-prep.code ne "" then do: put
   "Prep Description         Mat'l   Labor  Addt'l Amtz  Charge    Cost/M Total Cost" skip. leave.
   end.
end.

/* prep */
ASSIGN
   tprep-mat = 0
   tprep-lab = 0
   tprep-tot = 0
.

for each est-prep where est-prep.company = xest.company 
                    and est-prep.est-no = xest.est-no
                    and (est-prep.s-num eq v-form-no or (not vmclean2))
    with frame ad down no-labels no-box stream-io:

      qty = 0.
      FOR EACH xeb FIELDS(quantityPerSet)
          WHERE xeb.company     EQ est-prep.company 
            AND xeb.est-no      EQ est-prep.est-no
            AND xeb.form-no     EQ est-prep.s-num
            AND (xeb.blank-no   EQ est-prep.b-num OR
                 est-prep.b-num EQ 0)
          NO-LOCK:
        ASSIGN
           v-yld = if xeb.quantityPerSet lt 0 then -1 / xeb.quantityPerSet else xeb.quantityPerSet
           qty = qty + (tt-blk * v-yld).
      END.

   /* only (i)ntegrate and (m)aintenance lines are done here */
   if index("SON",est-prep.simon) > 0 then next.
   if est-prep.code ne "" then do:
      ASSIGN
         prep-add = est-prep.mkup / 100
         prep-atz = est-prep.amtz / 100.

      if est-prep.ml = true then
         assign
            prep-lab = 0
            prep-mat = est-prep.cost * est-prep.qty.
      else
         assign prep-mat = 0
                prep-lab = est-prep.cost * est-prep.qty.

      ASSIGN
         v-orig-prep-lab = prep-lab
         v-orig-prep-mat = prep-mat
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
         prep-tot ne 0 THEN DO:
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

      if est-prep.b-num = 0 then
      for each BLK where BLK.snum = est-prep.s-num:
         find first xjob
             where xjob.i-no eq blk.id
               and xjob.qty  eq blk.qreq.
/*
               and xjob.qty  eq qtty[vmcl].
*/
         if est-prep.ML = NO
         then assign BLK.lab = BLK.lab + (PREP-tot * BLK.pct)
                     xjob.lab = xjob.lab + (PREP-tot * BLK.pct).
         else assign xjob.mat = xjob.mat + (PREP-tot * BLK.pct).
         BLK.cosT = BLK.cosT + (PREP-tot * BLK.pct).
      end.
      else do:
         find first BLK where BLK.snum = est-prep.s-num and
                              BLK.bnum = est-prep.b-num no-error.
         find first xjob
             where xjob.i-no eq blk.id
               and xjob.qty  eq blk.qreq.
/*
               and xjob.qty  eq qtty[vmcl].
*/
         if est-prep.ML = NO
         then assign BLK.lab  = BLK.lab  + PREP-tot
                     xjob.lab = xjob.lab + PREP-tot.
         else assign xjob.mat = xjob.mat + PREP-tot.
         BLK.cosT = BLK.cosT + PREP-tot.
      end.

      create xprep.
      assign xprep.frm      = est-prep.s-num
             xprep.blank-no = est-prep.b-num
             xprep.qty      = est-prep.qty
             xprep.std-cost = est-prep.cost
             xprep.ml       = est-prep.ml
             xprep.cost-m   = prep-tot / (qty / 1000)
             xprep.simon    = est-prep.simon
             xprep.code     = est-prep.code.

      display string(est-prep.s-num,"99") + "-" +
              string(est-prep.b-num,"99") format "x(5)"
              est-prep.dscr format "x(15)"
              v-orig-prep-mat      format "->>>>9.99"
              v-orig-prep-lab      format "->>9.99"
              est-prep.mkup format "->>9" to 46 space(0) "%"
              est-prep.amtz to 50 format ">>9" space(0) "%"
              est-prep.simon format "X" to 58
              prep-tot / (qty / 1000) to 70
              prep-tot to 82 format ">>>>>9.99" skip 
              with stream-io width 82.
   end.
end.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
