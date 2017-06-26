/* ------------------------------------------------ cec/com/pr4-prp.p         */
/* Prep File calc & Display for estimates                                     */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

DEF BUFFER b-eb FOR eb.

{cec/print4.i shared shared}

DEF VAR qty AS INT NO-UNDO.

DEF VAR ld-fac AS DEC NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.

{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}

DEF VAR v-orig-prep-mat LIKE prep-mat NO-UNDO.
DEF VAR v-orig-prep-lab LIKE prep-lab NO-UNDO.

/* prep */
assign
 tprep-mat = 0
 tprep-lab = 0
 tprep-tot = 0
 dMCostToExcludePrep = 0
 dMPriceToAddPrep = 0.

for each est-prep                     
    where est-prep.company = xest.company
      AND est-prep.est-no = xest.est-no
      AND index("IM",est-prep.simon) gt 0
      and est-prep.code ne ""
    no-lock
    break by est-prep.est-no
          by est-prep.s-num
          by est-prep.b-num
          by est-prep.dscr
    with frame ad down no-labels no-box:
    
  qty = 0.
  FOR EACH b-eb FIELDS(yrprice yld-qty bl-qty)
      WHERE b-eb.company   EQ est-prep.company
        AND b-eb.est-no    EQ est-prep.est-no
        AND b-eb.form-no   EQ est-prep.s-num
        AND (b-eb.blank-no EQ est-prep.b-num OR
             est-prep.b-num EQ 0)
      NO-LOCK:
    qty = qty + (IF b-eb.yrprice THEN b-eb.yld-qty ELSE b-eb.bl-qty).  
  END.
      
  if first-of(est-prep.est-no) then
    put "Prep Description"
        "Mat'l"             to 30
        "Labor"             to 40
        "Addt'l"            to 50
        "Amtz"              to 60
        "Cost/M"            to 69
        "Total Cost"        to 80
        skip.

  ASSIGN  
     prep-add = est-prep.mkup / 100
     prep-atz = est-prep.amtz / 100.

  if est-prep.ml then
    assign
     prep-lab = 0
     prep-mat = est-prep.cost * est-prep.qty.
     
  else
    assign
     prep-mat = 0
     prep-lab = est-prep.cost * est-prep.qty.

  ASSIGN
     v-orig-prep-mat = prep-mat
     v-orig-prep-lab = prep-lab
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

  if est-prep.b-num eq 0 then
  for each blk where blk.snum eq est-prep.s-num:
     find first xjob
         where xjob.i-no     eq blk.id
           and xjob.form-no  eq blk.snum
           and xjob.blank-no eq blk.bnum
         no-error.
     
     if not est-prep.ML then
       assign
        blk.lab = blk.lab + (prep-tot * blk.pct)
        xjob.lab = xjob.lab + (prep-tot * blk.pct).
     else
       xjob.mat = xjob.mat + (prep-tot * blk.pct).
       
     blk.cost = blk.cost + (prep-tot * blk.pct).
   end.
   
   else do:
     find first blk
         where blk.snum eq est-prep.s-num
           and blk.bnum eq est-prep.b-num
         no-error.
     find first xjob
         where xjob.i-no     eq blk.id
           and xjob.form-no  eq blk.snum
           and xjob.blank-no eq blk.bnum
         no-error.
     
     if not est-prep.ml then
       assign
        blk.lab  = blk.lab  + prep-tot
        xjob.lab = xjob.lab + prep-tot.
        
     else
       xjob.mat = xjob.mat + prep-tot.
       
     blk.cost = blk.cost + prep-tot.
   end.

   create xprep.
   assign
    xprep.frm      = est-prep.s-num
    xprep.blank-no = est-prep.b-num
    xprep.qty      = est-prep.qty
    xprep.std-cost = est-prep.cost
    xprep.ml       = est-prep.ml
    xprep.cost-m   = prep-tot / (qty / 1000)
    xprep.simon    = est-prep.simon
    xprep.code     = est-prep.code.

  display string(est-prep.s-num,">9")  + "-" +
          string(est-prep.b-num,"99")   format "x(5)"
          est-prep.dscr                 format "x(14)"
          v-orig-prep-mat               format ">>>>>9.99"   to 30
          v-orig-prep-lab               format ">>>>>9.99"   to 40
          est-prep.mkup                 format "  >>9.99%"   to 50
          est-prep.amtz                 format "  >>9.99%"   to 60
          prep-tot / (qty / 1000)       format ">>>>9.99"    to 69
          prep-tot                      format ">>>>,>>9.99" to 80
          SKIP WITH STREAM-IO.
end.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
