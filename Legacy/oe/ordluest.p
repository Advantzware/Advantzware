/* -------------------------------------------------- oe/ordluest.p 12/98 JLF */
/* Order Entry - update for adding from estimating                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def new shared var save_id as recid.  /* RECORD ID for ORDER LINE */

def shared buffer xoe-ord for oe-ord.
def shared buffer xest    for est.
def shared buffer xef     for ef.
def shared buffer xeb     for eb.
def shared workfile work-ordl like oe-ordl.

def shared var v-fr-tax like oe-ctrl.f-tax.
def shared var v-qty-mod as log no-undo.

def TEMP-TABLE w-oe-ordl NO-UNDO like oe-ordl.

def var v-tax-rate as dec format ">,>>9.99<<<".
def var v-frt-tax-rate like v-tax-rate.  /* 9508 CAH */
def var v-quo-price like sys-ctrl.log-fld no-undo.
def var v-est-fg like sys-ctrl.log-fld no-undo.
def var v-est-fg1 like sys-ctrl.char-fld no-undo.
def var v-run-list as char.
def var v-qty like oe-ordl.qty.
DEF VAR lv-q-no LIKE quotehd.q-no NO-UNDO.
DEF VAR fil_id AS RECID NO-UNDO.
DEF VAR nufile AS LOG NO-UNDO.
DEF NEW SHARED VAR gEstSummaryOnly AS LOG NO-UNDO.

{ce/print4a.i shared}

{sys/ref/oecount.i}

{sys/ref/fgoecost.i}


v-run-list = "ce/print4.p,ce/box/print42.p,ce/tan/print4.p,ce/com/print4.p," +
             "cec/print4.p,cec/box/print42.p,cec/tan/print4.p,cec/com/print4.p".

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "QUOPRICE"
    no-lock no-error.
if not avail sys-ctrl then
do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "QUOPRICE"
   sys-ctrl.descrip = "Default Price for Order from Last Quote"
   sys-ctrl.log-fld = no.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
v-quo-price = sys-ctrl.log-fld.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGITEM#"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "FGITEM#"
   sys-ctrl.descrip = "Order Entry default FG Item Number from Estimate?"
   sys-ctrl.log-fld = yes.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
assign
 v-est-fg  = sys-ctrl.log-fld
 v-est-fg1 = sys-ctrl.char-fld.

save_id = recid(xoe-ord).

find xoe-ord where recid(xoe-ord) eq save_id no-error.
find first xest
    where xest.company eq xoe-ord.company
      and xest.est-no  eq xoe-ord.est-no.

if xest.est-type eq 3 or
   xest.est-type eq 4 or
   xest.est-type eq 8 then
  run value(entry(xest.est-type,v-run-list)).
  
for each oe-ordl
    where oe-ordl.company  eq cocode
      and oe-ordl.ord-no   eq xoe-ord.ord-no
      and oe-ordl.est-no   eq xoe-ord.est-no:
      
  create w-oe-ordl.
  buffer-copy oe-ordl to w-oe-ordl.
  delete oe-ordl.
end.

assign
 j      = 1
 fil_id = save_id.        /* reset fil_id, scrambled in calc...*/
 
for each eb
    where eb.company eq xest.company
      and eb.est-no  eq xest.est-no
      and eb.cust-no eq xoe-ord.cust-no
      and eb.form-no ne 0
    no-lock

    break by eb.part-no:
          
  v-qty = v-qty + if xest.est-type eq 3 or
                     xest.est-type eq 4 or
                     xest.est-type eq 8 then eb.bl-qty else xest.est-qty[1].
          
  if last-of(eb.part-no) then do:
    release w-oe-ordl.
  
    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq xoe-ord.ord-no
          and oe-ordl.est-no  eq xoe-ord.est-no
          and oe-ordl.part-no eq eb.part-no
        no-error.
        
    if not avail oe-ordl then
    find first w-oe-ordl where w-oe-ordl.i-no eq eb.stock-no no-error.
    
    if avail w-oe-ordl then do:
      create oe-ordl.
      buffer-copy w-oe-ordl to oe-ordl.
    end.
    
    if avail oe-ordl then do:
      if oe-ordl.qty ne v-qty then v-qty-mod = yes.
       
      {oe/ordluest.i oe-ordl}   
    end.  

    else do:
      create work-ordl.
        
      assign
       work-ordl.company    = cocode
       work-ordl.ord-no     = xoe-ord.ord-no
       work-ordl.po-no      = xoe-ord.po-no
       work-ordl.job-no     = xoe-ord.job-no
       work-ordl.req-code   = xoe-ord.due-code
       work-ordl.prom-code  = xoe-ord.due-code
       work-ordl.job-no2    = xoe-ord.job-no2
       work-ordl.cust-no    = xoe-ord.cust-no
       nufile               = yes.

      if xoe-ord.est-no ne "" then
        assign
         work-ordl.est-no = xoe-ord.est-no
         work-ordl.e-num  = xoe-ord.e-num
         work-ordl.pr-uom = "M".
           
      {oe/ordluest.i work-ordl}

      if work-ordl.i-no eq "" then
        if v-est-fg then
          work-ordl.i-no = if (xest.est-type eq 2 or xest.est-type eq 6) and
                              avail xeb then xeb.part-no else eb.part-no.
        else 
        if v-est-fg1 ne "Manual" then do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.part-no eq (if (xest.est-type eq 2 or
                                           xest.est-type eq 6) and
                                          avail xeb then xeb.part-no
                                                    else eb.part-no)
                and itemfg.cust-no eq eb.cust-no
              no-lock no-error.
          if avail itemfg then
            assign
             work-ordl.i-no       = itemfg.i-no
             work-ordl.part-dscr2 = itemfg.part-dscr2.
        end.
    end.
    
    v-qty = 0.
  end.
end.

find xoe-ord where recid(xoe-ord) eq fil_id no-lock no-error.
for each work-ordl where work-ordl.cust-no eq xoe-ord.cust-no:
  leave.
end.
if avail work-ordl then v-qty-mod = yes.

return.

run ce/print4.p.
run ce/box/print42.p.
run ce/tan/print4.p.
run ce/com/print4.p.
run cec/print4.p.
run cec/box/print42.p.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
