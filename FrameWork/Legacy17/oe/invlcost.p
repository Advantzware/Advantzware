/* -------------------------------------------------- oe/invlpost.p 08/00 JLF */
/* set o/e invoice line cost                                                  */
/* -------------------------------------------------------------------------- */

def input  parameter v-rowid        as   ROWID NO-UNDO.
def output parameter v-cost1        as   DEC DECIMALS 4 NO-UNDO.
def output parameter v-cost2        as   DEC DECIMALS 4 NO-UNDO.
def output parameter v-cost3        as   DEC DECIMALS 4 NO-UNDO.
def output parameter v-cost4        as   DEC DECIMALS 4 NO-UNDO.
def output parameter v-u-cost       as   DEC DECIMALS 4 NO-UNDO.
def output parameter v-t-cost       as   DEC DECIMALS 4 NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

DEF VAR lv-b-no    LIKE inv-line.b-no NO-UNDO.
DEF VAR lv-ord-no  LIKE inv-line.ord-no NO-UNDO.
DEF VAR lv-i-no    LIKE inv-line.i-no NO-UNDO.
DEF VAR lv-po-no   LIKE inv-line.po-no NO-UNDO.
DEF VAR lv-job-no  LIKE inv-line.job-no NO-UNDO.
DEF VAR lv-job-no2 LIKE inv-line.job-no2 NO-UNDO.
DEF VAR lv-inv-qty LIKE inv-line.inv-qty NO-UNDO.

def var v-uom like itemfg.prod-uom NO-UNDO.
def var v-qty like oe-boll.qty NO-UNDO.
def var v-cost AS DEC DECIMALS 4 extent 4 NO-UNDO.
def var v-cost-m AS DEC DECIMALS 4 extent 4 NO-UNDO.

def var fgb-avg-cost      like fg-bin.avg-cost     NO-UNDO.
def var fgb-last-cost     like fg-bin.last-cost    NO-UNDO. 
def var fgb-std-fix-cost  like fg-bin.std-fix-cost NO-UNDO.    
def var fgb-std-lab-cost  like fg-bin.std-lab-cost NO-UNDO.    
def var fgb-std-mat-cost  like fg-bin.std-mat-cost NO-UNDO.   
def var fgb-std-tot-cost  like fg-bin.std-tot-cost NO-UNDO.    
def var fgb-std-var-cost  LIKE fg-bin.std-var-cost NO-UNDO.
DEF VAR fgb-pur-uom       LIKE fg-bin.pur-uom      NO-UNDO.

DEF TEMP-TABLE tt-itemcost
  FIELD i-no LIKE itemfg.i-no
  FIELD fgb-avg-cost LIKE fgb-avg-cost
  FIELD fgb-last-cost LIKE fgb-last-cost
  FIELD fgb-std-fix-cost LIKE fgb-std-fix-cost
  FIELD fgb-std-mat-cost LIKE fgb-std-mat-cost
  FIELD fgb-std-tot-cost LIKE fgb-std-tot-cost
  FIELD fgb-std-var-cost LIKE fgb-std-var-cost
  FIELD fgb-std-lab-cost LIKE fgb-std-lab-cost
  INDEX i1 i-no.


DEF BUFFER b-fg-rcpth FOR fg-rcpth.
DEF BUFFER b-fg-rdtlh FOR fg-rdtlh.

find inv-line where ROWID(inv-line) eq v-rowid no-lock no-error.

IF NOT AVAIL inv-line THEN
find ar-invl where ROWID(ar-invl) eq v-rowid no-lock no-error.

IF AVAIL inv-line OR AVAIL ar-invl THEN DO:
  IF AVAIL inv-line THEN
    ASSIGN
     lv-b-no    = inv-line.b-no
     lv-ord-no  = inv-line.ord-no
     lv-i-no    = inv-line.i-no
     lv-po-no   = inv-line.po-no
     lv-job-no  = inv-line.job-no
     lv-job-no2 = inv-line.job-no2
     lv-inv-qty = inv-line.inv-qty.
  ELSE
    ASSIGN
     lv-b-no    = ar-invl.b-no
     lv-ord-no  = ar-invl.ord-no
     lv-i-no    = ar-invl.i-no
     lv-po-no   = ar-invl.po-no
     lv-job-no  = ar-invl.job-no
     lv-job-no2 = ar-invl.job-no2
     lv-inv-qty = ar-invl.inv-qty.

  RELEASE inv-line.
  RELEASE ar-invl.

  find first job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job-no  eq lv-job-no
        and job-hdr.job-no2 eq lv-job-no2
        and job-hdr.i-no    eq lv-i-no
      use-index job-no no-lock no-error.

  find first itemfg
      {sys/look/itemfgrlW.i}
        and itemfg.i-no eq lv-i-no
      NO-LOCK no-error.

  FOR EACH oe-boll
      WHERE oe-boll.company eq cocode
        AND oe-boll.b-no    eq lv-b-no
        AND oe-boll.ord-no  eq lv-ord-no
        AND oe-boll.i-no    eq lv-i-no
        AND oe-boll.po-no   eq lv-po-no
        AND oe-boll.qty     NE 0
      USE-INDEX b-no NO-LOCK:

      ASSIGN fgb-avg-cost      = 0  
             fgb-last-cost     = 0   
             fgb-std-fix-cost  = 0     
             fgb-std-lab-cost  = 0    
             fgb-std-mat-cost  = 0   
             fgb-std-tot-cost  = 0     
             fgb-std-var-cost  = 0
             fgb-pur-uom       = "".

    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq oe-boll.i-no
          and fg-bin.tag     eq oe-boll.tag
          and fg-bin.loc     eq oe-boll.loc
          and fg-bin.loc-bin eq oe-boll.loc-bin
          and fg-bin.job-no  eq oe-boll.job-no
          and fg-bin.job-no2 eq oe-boll.job-no2
        no-lock no-error.

    IF AVAIL fg-bin THEN
      ASSIGN fgb-avg-cost      = fg-bin.avg-cost     
             fgb-last-cost     = fg-bin.last-cost     
             fgb-std-fix-cost  = fg-bin.std-fix-cost     
             fgb-std-lab-cost  = fg-bin.std-lab-cost     
             fgb-std-mat-cost  = fg-bin.std-mat-cost    
             fgb-std-tot-cost  = fg-bin.std-tot-cost     
             fgb-std-var-cost  = fg-bin.std-var-cost 
             fgb-pur-uom       = fg-bin.pur-uom.

    if not avail fg-bin and avail(itemfg) then do:
        FIND FIRST tt-itemcost WHERE tt-itemcost.i-no = lv-i-no NO-ERROR. 
        IF AVAIL tt-itemcost THEN DO:
            ASSIGN 
                fgb-avg-cost     = tt-itemcost.fgb-avg-cost
                fgb-last-cost    = tt-itemcost.fgb-last-cost
                fgb-std-fix-cost = tt-itemcost.fgb-std-fix-cost
                fgb-std-mat-cost = tt-itemcost.fgb-std-mat-cost
                fgb-std-tot-cost = tt-itemcost.fgb-std-tot-cost
                fgb-std-lab-cost = tt-itemcost.fgb-std-lab-cost
                fgb-std-var-cost = tt-itemcost.fgb-std-var-cost.
        END.
        ELSE DO:
             each-fg:
             FOR EACH b-fg-rcpth WHERE b-fg-rcpth.company   EQ itemfg.company
                                  AND b-fg-rcpth.i-no      EQ itemfg.i-no
                                  AND b-fg-rcpth.rita-code EQ "S"
                                USE-INDEX tran NO-LOCK  ,
            
                FIRST b-fg-rdtlh WHERE b-fg-rdtlh.r-no    EQ b-fg-rcpth.r-no 
                                   AND b-fg-rdtlh.rita-code EQ b-fg-rcpth.rita-code
                                 NO-LOCK
                 by b-fg-rcpth.trans-date descending:
            
                FIND FIRST reftable NO-LOCK
                      WHERE reftable.reftable EQ "fg-bin.cost"
                        AND reftable.company  EQ b-fg-rdtlh.company
                        AND reftable.rec_key  = b-fg-rdtlh.rec_key
                        USE-INDEX rec_key
                      NO-ERROR.
                
                IF AVAIL reftable THEN    DO:

                      ASSIGN
                        fgb-avg-cost      = reftable.val[1]    
                        fgb-last-cost     = reftable.val[2]    
                        fgb-std-fix-cost  = reftable.val[3]    
                        fgb-std-lab-cost  = reftable.val[4]    
                        fgb-std-mat-cost  = reftable.val[5]    
                        fgb-std-tot-cost  = reftable.val[6]    
                        fgb-std-var-cost  = reftable.val[7]    .
                END. /* avail reftabl */
                FIND FIRST tt-itemcost WHERE tt-itemcost.i-no = lv-i-no NO-ERROR.
                IF NOT AVAIL tt-itemcost THEN
                    CREATE tt-itemcost.                    
                
                ASSIGN 
                  tt-itemcost.i-no             = lv-i-no
                  tt-itemcost.fgb-avg-cost     = fgb-avg-cost
                  tt-itemcost.fgb-last-cost    = fgb-last-cost
                  tt-itemcost.fgb-std-fix-cost = fgb-std-fix-cost
                  tt-itemcost.fgb-std-mat-cost = fgb-std-mat-cost
                  tt-itemcost.fgb-std-tot-cost = fgb-std-tot-cost
                  tt-itemcost.fgb-std-lab-cost = fgb-std-lab-cost
                  tt-itemcost.fgb-std-var-cost = fgb-std-var-cost.            

                 LEAVE each-fg. 
             END. /* each fg-rcp */
        END. /* else do */
    END.  /* not avail bin */

               
    assign
     v-qty = v-qty + oe-boll.qty
     v-uom = "".

    if fgb-std-tot-cost ne 0 then
      assign
       v-cost-m[1] = fgb-std-lab-cost
       v-cost-m[2] = fgb-std-fix-cost
       v-cost-m[3] = fgb-std-var-cost
       v-cost-m[4] = fgb-std-mat-cost
       v-uom       = fgb-pur-uom.
       
    else
    if avail job-hdr and job-hdr.std-tot-cost ne 0 then
      assign
       v-cost-m[1] = job-hdr.std-lab-cost
       v-cost-m[2] = job-hdr.std-fix-cost
       v-cost-m[3] = job-hdr.std-var-cost
       v-cost-m[4] = job-hdr.std-mat-cost
       v-uom       = "M".
       
    else   
      assign
       v-cost-m[1] = itemfg.std-lab-cost
       v-cost-m[2] = itemfg.std-fix-cost
       v-cost-m[3] = itemfg.std-var-cost
       v-cost-m[4] = itemfg.std-mat-cost.

    if v-uom eq "" then v-uom = itemfg.prod-uom.

    do i = 1 to 4:

       if v-uom ne "M" then
          run sys/ref/convcuom.p(v-uom, "M", 0, 0, 0, 0,
                                 v-cost-m[i], output v-cost-m[i]).       
       v-cost[i] = v-cost[i] + (v-cost-m[i] * oe-boll.qty / 1000 ).
    end.

  end. /*end each oe-boll*/

  do i = 1 to 4:
     v-cost[i] = v-cost[i] / (v-qty / 1000) .
    
     if v-cost[i] eq ? then v-cost[i] = 0.
  end.
  
  assign
   v-cost1 = v-cost[1]
   v-cost2 = v-cost[2]
   v-cost3 = v-cost[3]
   v-cost4 = v-cost[4]
   v-u-cost = v-cost[1] + v-cost[2] + v-cost[3] + v-cost[4].
          
  if v-u-cost eq ? then v-u-cost = 0.
          
  v-t-cost = v-u-cost * lv-inv-qty / 1000.

  if v-t-cost eq ? then v-t-cost = 0.

end.  
          
 
