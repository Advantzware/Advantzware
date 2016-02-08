/* --------------------------------------------- sys/inc/po-invqa.p 04/01 JLF */
/* Calculate Qty & Amt Invoiced for a PO Line Item                            */
/* -------------------------------------------------------------------------- */

def input  param v-recid        as   recid.
def output param v-qty          as   dec.
def output param v-amt          as   dec.

{sys/inc/var.i shared}
{sys/form/s-top.f}


find po-ordl where recid(po-ordl) eq v-recid no-lock no-error.

if avail po-ordl then
find first po-ord
    where po-ord.company eq cocode
      and po-ord.po-no   eq po-ordl.po-no
    no-lock no-error.

if avail po-ord then
for each reftable
    {ap/ap-reftbW.i po-ordl.po-no}
    no-lock,
    
    each ap-inv
    where ap-inv.company eq cocode
      and ap-inv.i-no    eq int(reftable.code2)
      and ap-inv.vend-no eq po-ord.vend-no
      and (ap-inv.po-no  eq po-ordl.po-no or ap-inv.po-no eq 0)
      and ap-inv.posted  eq yes
    use-index i-no no-lock,
        
    each ap-invl
    where ap-invl.i-no       eq ap-inv.i-no
      and (ap-invl.po-no     eq po-ordl.po-no or ap-inv.po-no ne 0)
      and {ap/invlline.i -1} eq po-ordl.line
    use-index i-no no-lock:
    
  assign
   v-qty = v-qty + ap-invl.qty
   v-amt = v-amt + ap-invl.amt.
end.
      
