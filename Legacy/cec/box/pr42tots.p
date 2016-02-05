/* --------------------------------------------- cec/box/pr42tots.p 02/96 JLF */
/* copy of com for 2 sheet boxes                                              */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def shared buffer xop for est-op.

{cec/print4.i shared shared}
{cec/print42.i shared}

def shared var qty as INT NO-UNDO.
def shared var v-do-gsa like do-gsa no-undo.
DEF SHARED VAR v-update-qty-gsa AS LOG NO-UNDO.
DEF SHARED VAR ld-gsa-brd AS DEC NO-UNDO.
DEF SHARED VAR ld-gsa-mat AS DEC NO-UNDO.
DEF SHARED VAR ld-gsa-lab AS DEC NO-UNDO.

def var xxx as dec no-undo.
def var j as int no-undo.
def var i as int no-undo.
def var z as int no-undo.
def var yyy as dec no-undo.
def var fg-wt$ as de no-undo.
def var fg-wt% as de no-undo.
def var qm as de no-undo .
def var v-yld as dec no-undo.
def var v-sqft as dec no-undo.
def var v-msf as dec no-undo.
def var v-pct as dec no-undo.
def var v-markup as dec extent 2 no-undo.
DEF VAR v-cewhspct AS LOG NO-UNDO.
DEF VAR v-qty AS INT NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
DEF VAR ld-fg-rate AS DEC NO-UNDO.
DEF VAR ll-gsa-pct AS LOG NO-UNDO.
DEF VAR v-cust-no AS CHAR NO-UNDO.

DEF BUFFER reftable-fm FOR reftable.
DEF BUFFER reftable-broker-pct FOR reftable.
DEF BUFFER reftable-pr FOR reftable.

DEF VAR vcarqty AS DEC NO-UNDO.
DEF VAR vcarmsf AS DEC NO-UNDO.
DEF VAR qmMclean2 AS DEC NO-UNDO.
DEF VAR mclean2yld AS dec NO-UNDO.
DEF VAR Mclean2Qty AS DEC NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF VAR isUnitized AS LOG NO-UNDO.

{cec/msfcalc.i}

DO TRANSACTION:
  {sys/inc/cewhschg.i}
  v-cewhspct = NOT cewhschg-cha BEGINS "$".
END.

ll-gsa-pct = CAN-FIND(FIRST sys-ctrl
                      WHERE sys-ctrl.company EQ cocode
                        AND sys-ctrl.name    EQ "CEGSA"
                        AND sys-ctrl.int-fld EQ 0).

find first ce-ctrl where ce-ctrl.company = cocode and
                         ce-ctrl.loc     = locode no-lock no-error.

output to value(outfile1) append .

fg-wt = 0.


find first bf-eb where bf-eb.company = xest.company and
                   bf-eb.est-no    = xest.est-no and
                   bf-eb.form-no = 0 NO-LOCK NO-ERROR.
isUnitized = AVAIL bf-eb AND bf-eb.pur-man.

FOR EACH car WHERE car.snum NE 0:
  find first blk where blk.id = car.id no-error.
  find first xjob
      where xjob.i-no eq blk.id
        and xjob.qty  eq blk.qreq.

  ASSIGN
   ld-fg-rate = IF blk.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate
   blk.lab  = blk.lab  + (car.qty / 100 * ld-fg-rate)
   blk.cost = blk.cost + (car.qty / 100 * ld-fg-rate)
   xjob.lab = xjob.lab + (car.qty / 100 * ld-fg-rate)
   fg-wt$   = fg-wt$ + (car.qty / 100 * ld-fg-rate)
   fg-wt = fg-wt + car.qty.

   for each eb FIELDS(form-no yld-qty) where
          eb.company = xest.company and
          eb.est-no = xest.est-no AND
          eb.form-no > 0 AND eb.form-no <= v-form-no        NO-LOCK:
          ASSIGN
             mclean2yld = if eb.yld-qty lt 0 then -1 / eb.yld-qty else eb.yld-qty
             mclean2Qty = mclean2Qty + (qtty[vmcl] * mclean2yld).
   end.
  IF CAN-FIND(FIRST xeb
              WHERE xeb.company EQ xest.company
                AND xeb.est-no  EQ xest.est-no
                AND xeb.form-no EQ 0
                AND xeb.cas-no  NE "") THEN do:
      IF vmclean2 THEN DO:
         FIND FIRST reftable-pr WHERE reftable-pr.reftable EQ "print42"
                AND reftable-pr.company  EQ xest.company
                AND reftable-pr.loc      EQ xest.est-no
                AND reftable-pr.code     EQ STRING(car.snum)
              NO-ERROR.
          IF NOT AVAIL reftable-pr THEN DO:
             CREATE reftable-pr.
             ASSIGN reftable-pr.reftable = "print42"
                    reftable-pr.company = xest.company
                    reftable-pr.loc = xest.est-no
                    reftable-pr.CODE = STRING(car.snum).
          END.
          ASSIGN reftable-pr.val[1] = car.qty
                 reftable-pr.val[2] = car.msf.

      END.
       /* DELETE car. */      
  END.
END.

for each car
    WHERE car.snum EQ v-form-no 
       OR NOT vmclean2
/*        OR (car.snum EQ 0 /*AND v-form-no EQ xest.form-qty*/) */
    BREAK BY car.id:

      z = 0.
      for each eb FIELDS(form-no yld-qty) where
          eb.company = xest.company and
          eb.est-no = xest.est-no AND
          eb.part-no = car.id
          NO-LOCK:
          ASSIGN
             v-yld = IF eb.form-no EQ 0 THEN 1 ELSE
                     if eb.yld-qty lt 0 then -1 / eb.yld-qty else eb.yld-qty
             z = z + (qtty[vmcl] * v-yld).
      end.
      find first xeb where xeb.company = xest.company and
                           xeb.est-no    = xest.est-no and
                           xeb.form-no  = car.snum   and
                           xeb.blank-no = car.bnum no-lock no-error.
      find first carrier where carrier.company = cocode and carrier.loc = locode
      and carrier.carrier = car.carrier no-lock no-error.
      if avail carrier then
           find first carr-mtx 
        where carr-mtx.company  eq cocode
          and carr-mtx.loc      eq locode
          and carr-mtx.carrier  eq carrier.carrier
          and carr-mtx.del-zone eq car.dscr
          AND carr-mtx.del-zip EQ xeb.ship-zip
        no-lock no-error.
    IF NOT AVAIL carr-mtx THEN 
      find first carr-mtx
          where carr-mtx.company  eq cocode
            and carr-mtx.loc      eq locode
            and carr-mtx.carrier  eq carrier.carrier
            and carr-mtx.del-zone eq car.dscr
          no-lock no-error.    

      IF v-cust-no EQ "" THEN
         v-cust-no = xeb.cust-no.

      ASSIGN yyy = 0  /* use for rate per cwt */
             vcarqty = car.qty
             vcarmsf = car.msf.

     IF vmclean2 THEN DO:
        /* FOR each reftable-pr NO-LOCK WHERE reftable-pr.reftable EQ "print42"
                AND reftable-pr.company  EQ xest.company
                AND reftable-pr.loc      EQ xest.est-no
                AND int(reftable-pr.CODE) <> v-form-no
               .
          IF int(reftable-pr.CODE) <> car.snum THEN
            ASSIGN vcarqty = vcarqty + reftable-pr.val[1]
                   vcarmsf = vcarmsf + reftable-pr.val[2] .       
         END.
         */
        IF car.snum = 0 AND AVAIL carrier AND carrier.chg-method eq "W" THEN NEXT.
      END.
     
      if xeb.fr-out-c ne 0 then
        yyy = yyy + (xeb.fr-out-c * (vcarqty / 100)).
      else
      if xeb.fr-out-m ne 0 then
        yyy = yyy + (xeb.fr-out-m * (z / 1000)).
      else
      if avail carr-mtx then do:
        if carrier.chg-method eq "W" then
        do i = 1 to 10:
          yyy = carr-mtx.rate[i] * vcarqty / 100.          
          if carr-mtx.weight[i] ge vcarqty then leave.
        end.
        
        else
        if carrier.chg-method eq "P" then do:
          p-qty = 0.
        /*##PN: cas temp-table only created with final form in set*/
        /*##PN: cas is not usable since we need to get full freight cost*/  
        /*##PN: so that we can pro-rate freight across all forms*/
          IF isUnitized AND AVAIL bf-eb /*set header*/ THEN DO:
                IF bf-eb.cas-pal ne 0 then 
                    p-qty = qty / (bf-eb.cas-pal * bf-eb.cas-cnt).
          END.
          ELSE FOR EACH cas
              WHERE cas.typ EQ 3
                AND cas.id  EQ xeb.part-no:
            p-qty = p-qty + cas.qty.            
          END.
          do i = 1 to 10:
            yyy = carr-mtx.rate[i] * p-qty.            
            if carr-mtx.weight[i] ge p-qty then leave.
          end.
        end.
        
        else
        do i = 1 to 10:
          fr-tot = carr-mtx.rate[i] * vcarmsf.          
          if carr-mtx.weight[i] ge vcarmsf then leave.
        end.
        
/*         find first bf-eb where bf-eb.company = xest.company and */
/*                            bf-eb.est-no    = xest.est-no and    */
/*                            bf-eb.form-no = 0 NO-LOCK NO-ERROR.  */
/*         isUnitized = AVAIL bf-eb AND bf-eb.pur-man.             */
        

        /*##BL: Compare total rate vs. rate min rate * all shipments*/
            if yyy lt carr-mtx.min-rate * rels[vmcl] then yyy = carr-mtx.min-rate  * rels[vmcl].
            IF isUnitized THEN yyy = yyy / xest.form-qty.

    end.
      /* wfk - 05251304 - 2 pc box, only include freight for form 0 */
/*       find first bf-eb where bf-eb.company = xest.company and */
/*                          bf-eb.est-no    = xest.est-no and    */
/*                          bf-eb.form-no = 0 NO-LOCK NO-ERROR.  */
/*       isUnitized = AVAIL bf-eb AND bf-eb.pur-man.             */
      
      
      IF NOT (xest.est-type EQ 6 
              AND avail(bf-eb) 
              AND bf-eb.pur-man = YES 
              AND bf-eb.set-is-assembled 
              AND bf-eb.stock-no EQ xeb.stock-no
              AND xeb.FORM-no NE 0) THEN      
      ASSIGN
         car.cost = car.cost + yyy
         fr-tot = fr-tot + yyy.           
      
      find first blk
           where (blk.id = car.id and blk.snum = car.snum and blk.bnum = car.bnum)
              or (blk.snum = xest.form-qty and car.snum = 0)
           no-error.
      blk.sell = blk.sell + yyy . /* use sell for freight costs for now */
end.

if fg-wt$ > 0 then put "Finished Goods Handling" fg-wt$ to 80 skip.

ASSIGN
   op-tot[5] = op-tot[5] + fg-wt$
   v-qty = 0
   .

IF xest.form-qty EQ 1 OR vmclean2 THEN
FOR EACH eb FIELDS(yld-qty)
    WHERE eb.company EQ xest.company
      AND eb.est-no  EQ xest.est-no
      AND eb.form-no EQ v-form-no
    NO-LOCK:
  ASSIGN
     v-yld = IF eb.yld-qty LT 0 THEN -1 / eb.yld-qty ELSE eb.yld-qty
     v-qty = v-qty + (qtty[vmcl] * v-yld)     
     .

END.

ELSE v-qty = qtty[vmcl].

if vmclean then op-tot[4] = op-tot[4] / (v-qty / 1000).

put "TOTAL  OPERATIONS        " op-tot[3] format ">>>>9.99" to 59
    op-tot[4] format ">>>>9.99" to 68 op-tot[5] to 80 skip(1).

if vmclean then op-tot[4] = op-tot[4] * (v-qty / 1000).

{est/calcpcts.i xest}  

if index("SB",ce-ctrl.sell-by) eq 0 then do:
  /* mat */
  do i = 1 to 6:
    ctrl[9] = ce-ctrl.mat-pct[i] / 100.
    if ce-ctrl.mat-cost[i] > dm-tot[5]  then leave.
  end.
  /* lab */
  do i = 1 to 6:
    ctrl[10] = ce-ctrl.lab-pct[i] / 100.
    if ce-ctrl.lab-cost[i] > op-tot[5]  then leave.
  end.
  calcpcts.val[1] = ctrl[9] * 100.
end.

assign
 gsa-mat = ctrl[9]  * 100
 gsa-lab = ctrl[10] * 100
 gsa-com = ce-ctrl.comm-mrkup
 gsa-war = ctrl[1] * 100.

FIND FIRST reftable-fm NO-LOCK
     WHERE reftable-fm.reftable EQ "gsa-fm"
       AND reftable-fm.company  EQ xest.company
       AND reftable-fm.loc      EQ ""
       AND reftable-fm.code     EQ xest.est-no
     NO-ERROR.

FIND FIRST cust WHERE
     cust.company EQ xest.company AND
     cust.cust-no EQ v-cust-no
     NO-LOCK NO-ERROR.

IF AVAIL reftable-fm THEN
   gsa-fm = reftable-fm.val[1].
ELSE
   IF AVAIL cust AND cust.scomm NE 0 THEN
      gsa-fm = cust.scomm.
ELSE
   DO:
      FIND FIRST reftable-broker-pct
           WHERE reftable-broker-pct.reftable EQ "ce-ctrl.broker-pct"
             AND reftable-broker-pct.company  EQ ce-ctrl.company
             AND reftable-broker-pct.loc      EQ ce-ctrl.loc
           NO-LOCK NO-ERROR.

      IF AVAIL reftable-broker-pct THEN
         gsa-fm = reftable-broker-pct.val[1].
   END.

output close.

run cec/gsa.p (ip-rowid, qtty[vmcl], rels[vmcl], INPUT YES,
               INPUT-OUTPUT v-update-qty-gsa,
               INPUT-OUTPUT ld-gsa-brd, INPUT-OUTPUT ld-gsa-mat, INPUT-OUTPUT ld-gsa-lab).
output to value(outfile1) append .

ASSIGN
ctrl[9]  = gsa-mat / 100
ctrl[10] = gsa-lab / 100
ctrl[1] = gsa-war / 100
ctrl[19] = gsa-fm / 100.

find first xeb where xeb.company = xest.company 
                 and xeb.est-no eq xest.est-no
                 and xeb.form-no ne 0 no-error.

assign
 v-yld = if xeb.yld-qty lt 0 then -1 / xeb.yld-qty else xeb.yld-qty
 qm    = qtty[vmcl] /* * (if vmclean2 then v-yld else 1) */ / 1000
 fac-tot = dm-tot[5] + op-tot[5] +
           tprep-mat + tprep-lab + mis-tot[1] + mis-tot[3].

qmMclean2 = mclean2Qty.

if xeb.chg-method eq "P" and ctrl[6] ne 0 then fac-tot = fac-tot + fr-tot.

if ce-ctrl.sell-by eq "B" then do:
  for each xef where xef.company = xest.company and
                     xef.est-no    eq xest.est-no
                 and (xef.form-no eq v-form-no or (not vmclean2))  no-lock,          
      first eb where eb.company = xest.company and
                     eb.est-no   eq xef.est-no
                     and eb.form-no eq xef.form-no
                  no-lock:

    RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).
          
    find first style  where style.company eq cocode
                       and style.style   eq eb.style
        no-lock no-error.
    if avail style then v-markup[1] = v-markup[1] + style.royalty.

    find first est-op where est-op.company = xest.company 
                        and est-op.est-no eq xest.est-no
                        and est-op.qty eq v-op-qty
                        and est-op.s-num eq xef.form-no
          and est-op.line  ge 500
        no-lock no-error.

    v-sqft = if avail est-op then est-op.num-sh
             else (qtty[vmcl] * v-yld /
                   (eb.num-up * v-n-out)).
             
    {sys/inc/roundup.i v-sqft}
     
    v-msf = v-msf +
            if v-corr then
              round(((xef.gsh-len * xef.gsh-wid) * .007) * v-sqft,0)
            else
              round(((xef.gsh-len * xef.gsh-wid) / 144) * v-sqft,0).
  end.
     
  v-msf = v-msf / 1000.
   
  {cec/sqftmrkp.i v-msf v-pct}
   
  v-markup[2] = (fac-tot - op-tot[7]) * v-pct / 100.
end.

FIND CURRENT calcpcts.

IF calcpcts.val[1] EQ 0 THEN calcpcts.val[2] = 0.

ASSIGN
   xxx = dm-tot[5] - calcpcts.val[2] + tprep-mat + mis-tot[1] 
   ctrl2[9] = xxx * ctrl[9]
   xxx = op-tot[5] + tprep-lab + mis-tot[3]
   ctrl2[10] = xxx * ctrl[10]
   calcpcts.val[2] = calcpcts.val[2] * calcpcts.val[1] / 100.

FIND CURRENT calcpcts NO-LOCK NO-ERROR.

IF v-cewhspct THEN
  ctrl2[1] = (fac-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10]) * ctrl[1].

ASSIGN
   ctrl2[13] = (fac-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10]) * ctrl[19]
   tt-tot = dm-tot[5] + op-tot[5] + ctrl2[1] + ctrl2[13] +
            tprep-mat + tprep-lab + mis-tot[1] + mis-tot[3] +
            calcpcts.val[2] + ctrl2[9] + ctrl2[10]
   ctrl2[4] = 0
   ctrl2[5] = 0.

   if ctrl[4] > 0 then do:             /* set spec#1 */
      if ctrl[4] <= 1
      then ctrl2[4] = (fac-tot + ctrl2[9] + ctrl2[10]) * ctrl[4].
      else ctrl2[4] = ctrl[4].
   end.
   tt-tot = tt-tot + ctrl2[4].

   if ctrl[11] > 0 then do:             /* set spec#2 */
      if ctrl[11] <= 1
      then ctrl2[11] = fac-tot * ctrl[11].
      else ctrl2[11] = ctrl[11].
   end.
   if ctrl[12] > 0 then do:             /* set spec#3 */
      if ctrl[12] <= 1
      then ctrl2[12] = fac-tot * ctrl[12].
      else ctrl2[12] = ctrl[12].
   end.
   
   if ctrl[6] ne 0 then
   for each blk:
      blk.cost = blk.cost + blk.sell. blk.sell = 0.
   end.

   for each blk:
      find first xeb no-lock
          where xeb.company  = xest.company
            and xeb.est-no   = xest.est-no
            and xeb.form-no  = blk.snum
            and xeb.blank-no = blk.bnum
          no-error.

      ASSIGN
         blk.fact = blk.cost
         xxx = blk.sell. /* xxx = 0 if freight already included! */

      if (not vmclean) or ctrl[16] ne 0 then
        blk.fact = blk.fact + ((blk.cost - blk.lab) * ctrl[9]) +
                              (blk.lab * ctrl[10]).

      ASSIGN
      /* add material gsa amount to blk.lab */
      blk.sell = ((blk.cost - blk.lab) * ctrl[9])
      /* set blk.lab to labor gsa amount */
      blk.lab  = blk.lab * ctrl[10]
      /* add gsa's to blk.cost */
      blk.cost = blk.cost + blk.lab + blk.sell
      yyy = blk.cost. /* yyy = total cost of blk */

      if ctrl[1] ne 0 then  /* warehousing % */
         blk.cost = blk.cost + (yyy * ctrl[1]).

      blk.cost = blk.cost + xxx. /* add freight if not already done */

      if ctrl[4] ne 0 then  /* special markup % */
         blk.cost = blk.cost + (yyy * ctrl[4]).
      
   end.
   

   find first xeb where xeb.company = xest.company 
                    and xeb.est-no eq xest.est-no
                    and xeb.form-no ne 0 no-error.
                    
   if not vmclean then do:

      display skip(1)
   "   T  O  T  A  L  S                         Cost/M     MR $    Run $  Total Cost" skip
      "Direct Material"  dm-tot[5] / qm to 50
                         dm-tot[3] format ">>>>9.99" to 59
                         dm-tot[5] to 80 skip with stream-io no-labels no-box.
      put "Direct Labor" op-tot[5] / qm to 50
                         op-tot[3] format ">>>>9.99" to 59
                         op-tot[4] format ">>>>>9.99" to 69
                         op-tot[5] to 80 format ">>>,>>9.99" skip.
                         
      if tprep-mat ne 0 then do: put
      "Prep.  Material" tprep-mat / qm to 50 tprep-mat to 80  skip.
      lin-count = lin-count + 1. end.
      if tprep-lab ne 0 then do: put
      "Prep.  Labor   " tprep-lab / qm to 50 tprep-lab to 80  skip.
      lin-count = lin-count + 1. end.
      if mis-tot[1] ne 0 then do: put
      "Misc.  Material" mis-tot[1] / qm to 50 mis-tot[1] to 80 skip.
      lin-count = lin-count + 1. end.
      if mis-tot[3] ne 0 then do: put
      "Misc.  Labor   " mis-tot[3] / qm to 50 mis-tot[3] to 80 skip.
      lin-count = lin-count + 6. end.

      if xeb.chg-method eq "P" and ctrl[6] ne 0 and fr-tot ne 0 THEN 
         put "Freight"       fr-tot / qm to 50 fr-tot to 80 skip.         
      
      put "DIRECT FACTORY COST" fac-tot / qm to 50 fac-tot to 80 skip.
      
      if ctrl[14] = 1 and ctrl[11] ne 0 then do:
         if ctrl[11] > 0 then put
         ce-ctrl.spec-l[2] space(1).
         if ctrl[11] <= 1 then
         put string(ce-ctrl.spec-%[2] * 100,"->>9.99") + "%" to 30.
         put ctrl2[11] / qm to 50 ctrl2[11] to 80 skip.
      end.
      if ctrl[15] = 1 and ctrl[12] ne 0 then do:
         if ctrl[12] > 0  then put ce-ctrl.spec-l[3] space(1).
         if ctrl[12] <= 1 then
         put string(ce-ctrl.spec-%[3] * 100,"->>9.99") + "%" to 30.
         put ctrl2[12] / qm to 50 ctrl2[12] to 80 skip.
      end.

      IF ctrl[16] NE 0 THEN DO:

        IF calcpcts.val[2] NE 0 THEN DO:

          PUT "GS&A Board".
          IF ll-gsa-pct THEN
            PUT STRING(calcpcts.val[1]) + "%" TO 30.
          PUT calcpcts.val[2] / qm TO 50
              calcpcts.val[2]      TO 80 SKIP.
        END.

        IF ctrl2[9] NE 0 THEN DO:
          PUT "GS&A Material".
          IF ll-gsa-pct THEN
            PUT STRING(ctrl[9] * 100) + "%" TO 30.
          PUT ctrl2[9] / qm TO 50
              ctrl2[9]      TO 80 SKIP.
        END.

        IF ctrl2[10] ne 0 THEN DO:
          PUT "GS&A Labor".
          IF ll-gsa-pct THEN
            PUT STRING(ctrl[10] * 100) + "%" TO 30.
          PUT ctrl2[10] / qm TO 50
              ctrl2[10]      TO 80 SKIP.
        END.

        fac-tot = fac-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10].
      END.

      put "TOTAL FACTORY COST"
          fac-tot / qm to 50
          fac-tot      to 80 skip.

      assign
       ord-cost = fac-tot + ctrl2[9] + ctrl2[10]
       tt-tot   = tt-tot + ctrl2[5].

      if ctrl2[1] ne 0 then do:

        put "Warehousing" +
            if v-cewhspct then (string(ctrl[1] * 100)  + "%") else "" to 30
            ctrl2[1] / qm to 50
            ctrl2[1]      to 80 skip.
      end.

      if ctrl2[13] ne 0 then do:

        put "Broker Comm %" +
            string(ctrl[19] * 100)  + "%" to 30
            ctrl2[13] / qm to 50
            ctrl2[13]      to 80 skip.
      end.

      if xeb.chg-method eq "P" and ctrl[6] eq 0 and fr-tot ne 0 then do:
         put "Freight"       fr-tot / qm to 50 fr-tot to 80 skip.
         tt-tot = tt-tot + fr-tot.         
      end.

      if ctrl2[4] > 0 then do:
        put ce-ctrl.spec-l[1].
        if ctrl[4] <= 1 then
          put space(1) string(ce-ctrl.spec-%[1]) + "%" to 30.
        put ctrl2[4] / qm to 50
            ctrl2[4] to 80 skip.
      end.

      IF ctrl[16] NE 0 THEN DO:
        IF calcpcts.val[2] NE 0 THEN DO:

          PUT "GS&A Board".
          IF ll-gsa-pct THEN
            PUT STRING(calcpcts.val[1]) + "%" TO 30.
          PUT calcpcts.val[2] / qm TO 50
              calcpcts.val[2]      TO 80 SKIP.
        END.

        IF ctrl2[9] NE 0 THEN DO:
          PUT "GS&A Material".
          IF ll-gsa-pct THEN
            PUT STRING(ctrl[9] * 100) + "%" TO 30.
          PUT ctrl2[9] / qm TO 50
             ctrl2[9]       TO 80 SKIP.
        END.

        IF ctrl2[10] ne 0 THEN DO:
          PUT "GS&A Labor".
          IF ll-gsa-pct THEN
            PUT STRING(ctrl[10] * 100) + "%" TO 30.
          PUT ctrl2[10] / qm TO 50
              ctrl2[10]      TO 80 SKIP.
        END.

        tt-tot = tt-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10].
      END.
      
      if v-markup[1] ne 0 then do:
        put "Style Markup"
            v-markup[1] / qm to 48
            v-markup[1]      to 80 skip.
        tt-tot = tt-tot + v-markup[1].
      end.
      
      if v-markup[2] ne 0 then do:
         if not vmclean2 then v-pct = v-markup[2] / fac-tot * 100.
         
         put "Board Markup" string(v-pct,"->>9.99") + "%" to 30
             v-markup[2] / qm to 48
             v-markup[2]      to 80 skip.
         tt-tot = tt-tot + v-markup[2].
      end.
   end.

   else do:
     assign
      vmcl-desc = "Prep.  Material"
      vmcl-cost = tprep-mat / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 0}

     assign
      vmcl-desc = "Prep.  Labor"
      vmcl-cost = tprep-lab / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 1}

     assign
      vmcl-desc = "Misc.  Material"
      vmcl-cost = mis-tot[1] / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 2}

     assign
      vmcl-desc = "Misc.  Labor"
      vmcl-cost = mis-tot[3] / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 3}

     ASSIGN
      op-tot[5] = op-tot[5] - op-tot[6] - op-tot[7]
      fac-tot   = fac-tot   - op-tot[7]
      vmcl-desc = "Direct Material"
      vmcl-cost = dm-tot[5] / qm.

     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 4}

     assign
      vmcl-desc = "Direct Labor"
      vmcl-cost = op-tot[5] / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 5}

     assign
      vmcl-desc = "Variable Overhead"
      vmcl-cost = op-tot[6] / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 6}

     if xeb.chg-method eq "P" and ctrl[6] ne 0 and fr-tot ne 0 then do:
       assign
        vmcl-desc = "Freight"
        vmcl-cost = fr-tot / qm /*(IF vmclean2 THEN qmMclean2 ELSE qm)*/.  
       
       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 7}
     end.

     assign
      vmcl-desc = "DIRECT FACTORY COST"
      vmcl-cost = fac-tot / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 8}

     fac-tot2 = fac-tot.

     assign
      vmcl-desc = "Fixed Overhead"
      vmcl-cost = op-tot[7] / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 9}

     fac-tot2 = fac-tot2 + op-tot[7].
     if ctrl[13] ne 0 then fac-tot2 = fac-tot2 + ctrl2[4].
     if ctrl[14] ne 0 then fac-tot2 = fac-tot2 + ctrl2[11].
     if ctrl[15] ne 0 then fac-tot2 = fac-tot2 + ctrl2[12].
     if ctrl[18] ne 0 and ce-ctrl.sell-by ne "B" then
                           fac-tot2 = fac-tot2 + ctrl2[18].

     if ctrl[13] = 1 then do:
       assign
        vmcl-desc = ce-ctrl.spec-l[1]
        vmcl-cost = ctrl2[4] / qm.

       if ctrl[4] <= 1 then
         vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                     string(ce-ctrl.spec-%[1] * 100,"->>9.99%").
       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 10}
     end.

     if ctrl[14] = 1 then do:
       assign
        vmcl-desc = ce-ctrl.spec-l[2]
        vmcl-cost = ctrl2[11] / qm.

       if ctrl[11] <= 1 then
         vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                     string(ce-ctrl.spec-%[2] * 100,"->>9.99%").

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 11}
     end.

     if ctrl[15] = 1 then do:
       assign
        vmcl-desc = ce-ctrl.spec-l[3]
        vmcl-cost = ctrl2[12] / qm.

       if ctrl[12] <= 1 then
         vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                     string(ce-ctrl.spec-%[3] * 100,"->>9.99%").

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 12}
     end.

     IF ctrl[16] NE 0 THEN DO:

       ASSIGN
        vmcl-desc = "GS&A Board"
        vmcl-cost = calcpcts.val[2] / qm
        fac-tot2  = fac-tot2 + calcpcts.val[2].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 13}

       IF ll-gsa-pct AND calcpcts.val[2] NE 0 THEN DO:
         mclean.rec-type = "gsabrd".

         ASSIGN
          vmcl-desc = "    GS&A Board %"
          vmcl-cost = calcpcts.val[1].

         {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 14}
         mclean.rec-type = "gsabrd".
       END.

       ASSIGN
        vmcl-desc = "GS&A Material"
        vmcl-cost = ctrl2[9] / qm
        fac-tot2  = fac-tot2 + ctrl2[9].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 15}

       IF ll-gsa-pct AND ctrl2[9] NE 0 THEN DO:
         mclean.rec-type = "gsamat".

         ASSIGN
          vmcl-desc = "    GS&A Material %"
          vmcl-cost = ctrl[9] * 100.

         {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 16}
         mclean.rec-type = "gsamat".
       END.

       ASSIGN
        vmcl-desc = "GS&A Labor"
        vmcl-cost = ctrl2[10] / qm
        fac-tot2  = fac-tot2 + ctrl2[10].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 17}

       IF ll-gsa-pct AND ctrl2[10] NE 0 THEN DO:
         mclean.rec-type = "gsalab".

         ASSIGN
          vmcl-desc = "    GS&A Labor %"
          vmcl-cost = ctrl[10] * 100.

         {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 18}
         mclean.rec-type = "gsalab".
       END.
     END.  
       
     if ctrl[18] > 0 and ce-ctrl.sell-by ne "B" then do:   /* Royalty */
       vmcl-desc = "Royalty".

       if ctrl[18] <= 1 then
         assign
          vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                      string(ctrl2[18] * 100,"->>9.99%")
          vmcl-cost = (ctrl2[18] * fac-tot).

       else vmcl-cost = ctrl2[18].

       assign
        fac-tot2  = fac-tot2 + vmcl-cost
        vmcl-cost = vmcl-cost / qm.

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 19}
     end.

     assign
      vmcl-desc = "TOTAL FACTORY COST"
      vmcl-cost = fac-tot2 / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 20}

     assign
      ord-cost = fac-tot2
      tt-tot   = fac-tot2.

     if ctrl2[1] ne 0 or not v-cewhspct then do:
       assign
        vmcl-desc = "Warehousing"
        vmcl-cost = ctrl2[1] / qm 
        tt-tot    = tt-tot + ctrl2[1].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 21}
       
       if v-cewhspct then do:
         assign
          mclean.rec-type = "warehousing"
          vmcl-desc       = "    Warehousing %"
          vmcl-cost       = ctrl[1] * 100.

         {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 22}
         mclean.rec-type = "warehousing".
       end.
     end.

     if ctrl2[13] ne 0 then do:
       assign
        vmcl-desc = "Broker Comm"
        vmcl-cost = ctrl2[13] / qm
        tt-tot    = tt-tot + ctrl2[13].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 23}
       
       assign
        mclean.rec-type = "broker"
        vmcl-desc       = "    Broker Comm %"
        vmcl-cost       = ctrl[19] * 100.

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 24}
       mclean.rec-type = "broker".
       
     end.

     if xeb.chg-method eq "P" and ctrl[6] eq 0 and fr-tot ne 0 then do:
       assign
        vmcl-desc = "Freight"
        vmcl-cost = fr-tot / qm
        tt-tot    = tt-tot + fr-tot.
       
       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 25}
     end.

     if ctrl[13] = 0 then do:
       vmcl-desc = ce-ctrl.spec-l[1].

       if ctrl[4] <= 1 then
         assign
          vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                      string(ce-ctrl.spec-%[1] * 100,"->>9.99%")
          ctrl2[4]  = fac-tot2 * ctrl[4].
       else ctrl2[4] = ctrl[4].

       assign
        vmcl-cost = ctrl2[4] / qm
        tt-tot    = tt-tot + ctrl2[4].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 26}
     end.

     if ctrl[14] = 0 then do:             /* set spec#2 */
       vmcl-desc = ce-ctrl.spec-l[2].

       if ctrl[11] <= 1 then
         assign
          vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                      string(ce-ctrl.spec-%[2] * 100,"->>9.99%")
          ctrl2[11]  = fac-tot2 * ctrl[11].
       else ctrl2[11] = ctrl[11].

       assign
        vmcl-cost = ctrl2[11] / qm
        tt-tot    = tt-tot + ctrl2[11].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 27}
     end.

     if ctrl[15] = 0 then do:             /* set spec#3 */
       vmcl-desc = ce-ctrl.spec-l[3].

       if ctrl[12] <= 1 then
         assign
          vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                      string(ce-ctrl.spec-%[3] * 100,"->>9.99%")
          ctrl2[12]  = fac-tot2 * ctrl[12].
       else ctrl2[12] = ctrl[12].

       assign
        vmcl-cost = ctrl2[12] / qm
        tt-tot    = tt-tot + ctrl2[12].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 28}
     end.

     IF ctrl[16] EQ 0 THEN DO:

       ASSIGN
        vmcl-desc = "GS&A Board"
        vmcl-cost = calcpcts.val[2] / qm
        tt-tot    = tt-tot + calcpcts.val[2].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 29}

       IF ll-gsa-pct AND calcpcts.val[2] NE 0 THEN DO:
          ASSIGN
          mclean.rec-type = "gsabrd"
          vmcl-desc = "    GS&A Board %"
          vmcl-cost = calcpcts.val[1].

         {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 30}
         mclean.rec-type = "gsabrd".
       END.

       ASSIGN
        vmcl-desc = "GS&A Material"
        vmcl-cost = ctrl2[9] / qm
        tt-tot    = tt-tot + ctrl2[9].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 31}

       IF ll-gsa-pct AND ctrl2[9] NE 0 THEN DO:
         mclean.rec-type = "gsamat".

         ASSIGN
          vmcl-desc = "    GS&A Material %"
          vmcl-cost = ctrl[9] * 100.

         {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 32}
         mclean.rec-type = "gsamat".
       END.

       ASSIGN
        vmcl-desc = "GS&A Labor"
        vmcl-cost = ctrl2[10] / qm
        tt-tot    = tt-tot + ctrl2[10].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 33}

       IF ll-gsa-pct AND ctrl2[10] NE 0 THEN DO:
         mclean.rec-type = "gsalab".

         ASSIGN
          vmcl-desc = "    GS&A Labor %"
          vmcl-cost = ctrl[10] * 100.

         {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 34}
         mclean.rec-type = "gsalab".
       END.
     END.

     if ctrl[18] eq 0 and ce-ctrl.sell-by ne "B" then do:   /* Royalty */
       vmcl-desc = "Royalty".

       if ctrl[18] <= 1 then
         assign
          vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                      string(ctrl2[18] * 100,"->>9.99%")
          vmcl-cost = (ctrl2[18] * fac-tot).

       else vmcl-cost = ctrl2[18].

       assign
        tt-tot    = tt-tot + vmcl-cost
        vmcl-cost = vmcl-cost / qm.

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 35}
     end.
     
     if v-markup[1] ne 0 then do:
       assign
        vmcl-desc = "Style Markup"
        tt-tot    = tt-tot + v-markup[1]
        vmcl-cost = v-markup[1] / qm.
                
       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 36}
     end.
     
     if v-markup[2] ne 0 then do:
       if not vmclean2 then v-pct = v-markup[2] / fac-tot * 100.
         
       assign
        vmcl-desc = "Board Markup"
        tt-tot    = tt-tot + v-markup[2]
        vmcl-cost = v-markup[2] / qm.
        
       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 37}
       mclean.rec-type = "boardm".
       
       assign
        vmcl-desc = "    Board Markup %"
        vmcl-cost = v-pct.
        
       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 38}
       mclean.rec-type = "boardm".
     end.
   end.

   output close.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
