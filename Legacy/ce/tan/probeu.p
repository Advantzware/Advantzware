/* ------------------------------------------------ ce/tan/probeu.p 10/96 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input parameter v-rowid as rowid no-undo.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def shared var tmp-dir as cha no-undo.
def buffer b-pro for probe.

DEF NEW SHARED VAR lv-cebrowse-dir AS CHAR NO-UNDO.

find first sys-ctrl where
    sys-ctrl.company eq cocode AND
    sys-ctrl.name    eq "CEBROWSE"
    no-lock no-error.

if not avail sys-ctrl then DO TRANSACTION:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CEBROWSE"
   sys-ctrl.descrip = "# of Records to be displayed in browser"
   sys-ctrl.log-fld = YES
   sys-ctrl.char-fld = "CE"
   sys-ctrl.int-fld = 30.
end.

IF sys-ctrl.char-fld NE "" THEN
   tmp-dir = sys-ctrl.char-fld.
ELSE
   tmp-dir = "users\".

IF LOOKUP(SUBSTRING(tmp-dir,LENGTH(tmp-dir)),"\,/") EQ 0 THEN
   tmp-dir = tmp-dir + "\".

ASSIGN
  tmp-dir = REPLACE(tmp-dir,"/","\").
  lv-cebrowse-dir = tmp-dir.

{ce/print4.i shared}

def var v-qty like probe.est-qty no-undo.


find probe where rowid(probe) eq v-rowid exclusive.
find first est
    where est.company eq probe.company
      and est.est-no  eq probe.est-no
    no-lock.

v-qty = probe.est-qty.

for each b-pro
    where b-pro.company eq probe.company
      and b-pro.est-no  eq probe.est-no
      and b-pro.line    eq probe.line
      and recid(b-pro)  ne recid(probe)
    no-lock:
    
  probe.est-qty = probe.est-qty + b-pro.est-qty.
end.
 
find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

{ce/com/probe.u}

probe.est-qty = v-qty.

for each b-pro
    where b-pro.company eq probe.company
      and b-pro.est-no  eq probe.est-no
      and b-pro.line    eq probe.line
      and recid(b-pro) ne recid(probe):
      
  assign
   b-pro.sell-price   = probe.sell-price
   b-pro.net-profit   = probe.net-profit
   b-pro.gross-profit = probe.gross-profit.
end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
