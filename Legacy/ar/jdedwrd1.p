/* -------------------------------------------------- jc/jdedwrd1.p 12/02 JLF */
/*                                                                            */
/* Export AR to JD Edwards                                                    */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input parameter v-recid as recid no-undo.

{sys/inc/var.i shared}

def var v-uom like itemfg.prod-uom.
def var v-qty like oe-boll.qty.
def var v-cost like inv-line.cost extent 4.
def var v-cost-m like inv-line.cost extent 4.
def var v-est-type like est.est-type.
def var v-pct as dec.
def var v-mat-dscr like mat.dscr.
def var v-mat-cost as dec.

def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.

{ce/print4.i "new shared" "new shared"}
{ce/print42.i "new shared"}

def new shared var qty as int NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.

def shared temp-table tt-cost field tt-dscr as   char
                              field tt-cost as   dec
                              field tt-type like item.mat-type
                              index tt-cost tt-type tt-dscr
                              index tt-dscr tt-dscr.

def workfile work-eb
   field e-num    like job.e-num
   field form-no  like job-hdr.frm
   field cust-seq like xeb.cust-seq
   field blank-no like job-hdr.blank-no
   field bl-qty   like xeb.bl-qty
   field yld-qty  like xeb.yld-qty.
   
def workfile work-ord
   field cust-no like job-hdr.cust-no
   field ord-no  like job-hdr.ord-no.

def var chcs as char extent 6 init ["ce/print4.p",
                                    "ce/box/print42.p",
                                    "ce/tan/print4.p",
                                    "ce/com/print4.p",
                                    "cec/print4.p",
                                    "cec/box/print42.p"] no-undo.


for each tt-cost:
  delete tt-cost.
end.

for first ar-invl where recid(ar-invl) eq v-recid no-lock,
    first ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock:
    
  find first itemfg
      {sys/look/itemfgrlW.i}
        and itemfg.i-no eq ar-invl.i-no
      no-lock no-error.
  
  if ar-invl.b-no ne 0 then
  for each oe-boll
      where oe-boll.company eq cocode
        and oe-boll.b-no    eq ar-invl.b-no
        and oe-boll.i-no    eq ar-invl.i-no
        and oe-boll.po-no   eq ar-invl.po-no
        and oe-boll.qty     ne 0
      no-lock:
      
    find first job-hdr
        where job-hdr.company eq cocode
          and job-hdr.job-no  eq ar-invl.job-no
          and job-hdr.job-no2 eq ar-invl.job-no2
          and job-hdr.i-no    eq ar-invl.i-no
        use-index job-no no-lock no-error.
    if avail job-hdr then
    find first job
        where job.company eq cocode
          and job.job     eq job-hdr.job
          and job.job-no  eq job-hdr.job-no
          and job.job-no2 eq job-hdr.job-no2
        no-lock no-error.
        
    if avail job then do:
      run jc/calc-est.p (recid(job)).
    
      for each mclean:
        find first tt-cost where tt-dscr eq mclean.descr no-error.
        if not avail tt-cost then create tt-cost.
        assign
         tt-dscr = mclean.descr
         tt-cost = tt-cost + (mclean.cost[1] * (oe-boll.qty / 1000)).
       
        delete mclean.
      end.
      
      find first est where est.e-num eq job.e-num no-lock no-error.
      v-est-type = if avail est then
                     if est.est-type gt 4 then (est.est-type - 4)
                     else est.est-type
                   else 1.
      
      for each job-hdr
          where job-hdr.company eq cocode
            and job-hdr.job     eq job.job
            and job-hdr.job-no  eq job.job-no
            and job-hdr.job-no2 eq job.job-no2
            and job-hdr.i-no    eq ar-invl.i-no
          no-lock,
          each brd where brd.form-no eq job-hdr.frm or v-est-type eq 2,
          first item
          where item.company eq cocode
            and item.i-no    eq brd.i-no
          no-lock,
          first mat where mat.mat eq item.mat-type no-lock:
          
        v-pct = job-hdr.sq-in / 100.
        if v-pct eq ? or v-pct eq 0 then v-pct = 100.
          
        v-mat-dscr = if index("1234",mat.mat) gt 0 then "Foam" else mat.dscr.
        
        find first tt-cost where tt-dscr eq v-mat-dscr no-error.
        if not avail tt-cost then create tt-cost.
        assign
         tt-dscr = v-mat-dscr
         tt-type = item.mat-type
         tt-cost = tt-cost + (brd.cost-m * (oe-boll.qty / 1000) *
                              (if brd.blank-no gt 0 then 1 else v-pct)).
      end.
    end.
    
    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq oe-boll.i-no
          and fg-bin.tag     eq oe-boll.tag
          and fg-bin.loc     eq oe-boll.loc
          and fg-bin.loc-bin eq oe-boll.loc-bin
          and fg-bin.job-no  eq oe-boll.job-no
          and fg-bin.job-no2 eq oe-boll.job-no2
        no-lock no-error.
                
    assign
     v-qty = v-qty + oe-boll.qty
     v-uom = "".
  
    if avail fg-bin and fg-bin.std-tot-cost ne 0 then
      assign
       v-cost-m[1] = fg-bin.std-lab-cost
       v-cost-m[2] = fg-bin.std-fix-cost
       v-cost-m[3] = fg-bin.std-var-cost
       v-cost-m[4] = fg-bin.std-mat-cost
       v-uom       = fg-bin.pur-uom.
       
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
                                       
      v-cost[i] = v-cost[i] + (v-cost-m[i] * oe-boll.qty / 1000).
    end.                           
  end.
  
  find first tt-cost where tt-type gt "" no-error.
  if not avail tt-cost then do:
    create tt-cost.
    assign
     tt-dscr = "Board"
     tt-type = "B"
     tt-cost = v-cost[4].
  end.
  
  v-mat-cost = 0.
  for each tt-cost where tt-type gt "":
    v-mat-cost = v-mat-cost + tt-cost.
    if tt-cost eq 0 then delete tt-cost.
  end.
  
  if v-mat-cost ne 0 and v-mat-cost ne v-cost[4] then
  for each tt-cost where tt-type gt "":
    tt-cost = tt-cost * (v-cost[4] / v-mat-cost).
  end.
  
  for each tt-cost:
    tt-cost = tt-cost / (v-qty / 1000).
  end.
  
  create tt-cost.
  assign
   tt-dscr = "DL"
   tt-cost = v-cost[1] / (v-qty / 1000).
   
  create tt-cost.
  assign
   tt-dscr = "FO"
   tt-cost = v-cost[2] / (v-qty / 1000).
   
  create tt-cost.
  assign
   tt-dscr = "VO"
   tt-cost = v-cost[3] / (v-qty / 1000).
end.

