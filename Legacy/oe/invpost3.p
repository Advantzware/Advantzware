/* -------------------------------------------------- oe/invpost3.p 04/99 JLF */
/* Invoicing  - Edit Register & Post Invoicing Transactions                   */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAMETER udate   LIKE period.pst  NO-UNDO.
DEF INPUT PARAMETER uperiod LIKE period.pnum NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}   /* 1st screen line - date,title, time */

def new shared buffer xoe-ord for oe-ord.

def buffer tmp-oe-boll for oe-boll.
def buffer b-itemfg for itemfg.
DEF BUFFER b-oe-ordl FOR oe-ordl.

def shared buffer xoe-relh for oe-relh.
def shared buffer yoe-relh for oe-relh.
def shared buffer xoe-rell for oe-rell.

def new shared var v-tax-rate as dec format ">,>>9.99<<<".
def new shared var v-frt-tax-rate like v-tax-rate.
def new shared var v-dcr-val like oe-ordl.cost init 0.
def new shared var v-uom-rate as int init 0.

{oe/oe-bolpi.i NEW}

DEF NEW SHARED TEMP-TABLE tt-bolh NO-UNDO LIKE oe-bolh.
DEF NEW SHARED TEMP-TABLE tt-boll NO-UNDO LIKE oe-boll.

def shared var v-back like itemfg.q-back.
def shared var v-invline as recid.
def shared var v-invhead as recid.

def var v-tmp-tax-rate as dec format ">,>>9.99<<<".
def var v-create as log init no no-undo.
def var v-rcpth-no as int.
def var v-frst as log init no.
def var v-bo-ch as log init no no-undo.
def var v-close-qty like oe-ordl.qty.
def var v-inv-date like inv-head.inv-date no-undo.
def var save_id AS RECID.

{fg/fullset.i NEW}


v-inv-date = today.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

find inv-line where recid(inv-line) eq v-invline no-lock.
find inv-head where recid(inv-head) eq v-invhead no-lock.

find first itemfg
    {sys/look/itemfgrlW.i}
      and itemfg.i-no eq inv-line.i-no.

find first cust
    {sys/ref/custW.i}
      and cust.cust-no eq inv-head.cust-no.
    
/** Update Inventory if oe-ctrl is set to update at Invoicing **/
if not oe-ctrl.u-inv or inv-line.ord-no eq 0 then
  run oe/invpost4.p (recid(inv-line), 1).

/* PTD & YTD INVOICED & SHIPPED */
assign
 itemfg.q-inv       = itemfg.q-inv       + inv-line.inv-qty
 itemfg.q-inv-ytd   = itemfg.q-inv-ytd   + inv-line.inv-qty

 itemfg.q-ship      = itemfg.q-ship      + inv-line.ship-qty
 itemfg.q-ship-ytd  = itemfg.q-ship-ytd  + inv-line.ship-qty
 
 itemfg.q-alloc-ytd = itemfg.q-alloc-ytd + inv-line.ship-qty
    
 itemfg.ytd-msf     = itemfg.ytd-msf +
                      (inv-line.inv-qty * itemfg.t-sqft / 1000)
 
 cust.ytd-msf = (cust.ytd-msf + ((inv-line.inv-qty / 1000) * itemfg.t-sqft)).
 
find first period
    where period.company eq cocode
      and period.pst     le udate
      and period.pend    ge udate
      and period.pnum    eq uperiod
    no-lock no-error.
if avail period                     and
   inv-head.inv-date ge period.pst  and
   inv-head.inv-date le period.pend then
  assign
   itemfg.q-inv-ptd        = itemfg.q-inv-ptd   + inv-line.inv-qty
   itemfg.q-ship-ptd       = itemfg.q-ship-ptd  + inv-line.ship-qty
   itemfg.q-alloc-ptd      = itemfg.q-alloc-ptd + inv-line.ship-qty
   itemfg.ptd-msf[uperiod] = itemfg.ptd-msf[uperiod] +
                             (inv-line.inv-qty * itemfg.t-sqft / 1000)
   cust.ptd-msf[uperiod]   = (cust.ptd-msf[uperiod] +
                              ((inv-line.inv-qty / 1000) * itemfg.t-sqft)).
                              
find first oe-ordl
    where oe-ordl.company eq cocode
      and oe-ordl.ord-no  eq inv-line.ord-no
      and oe-ordl.line    eq inv-line.line
      and oe-ordl.i-no    eq inv-line.i-no
    use-index ord-no no-error.

for each oe-boll
    where oe-boll.company eq cocode
      and oe-boll.b-no    eq inv-line.b-no
      and oe-boll.ord-no  eq inv-line.ord-no
      and oe-boll.i-no    eq inv-line.i-no
      and oe-boll.line    eq inv-line.line
      and oe-boll.po-no   eq inv-line.po-no,
    
    first oe-bolh
    where oe-bolh.company  eq cocode
      and oe-bolh.b-no     eq oe-boll.b-no
      and (oe-bolh.deleted eq no or
           inv-line.ord-no eq 0):

  {oe/invpost3.i "bolh1"}
end.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
