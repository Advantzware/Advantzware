/* sys/inc/OEPrepTaxCode.i*/

def var PrepTax-log like sys-ctrl.log-fld no-undo.
def var PrepTax-cha like sys-ctrl.char-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OEPrepTaxCode?"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "OEPrepTaxCode?"
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = ""
   sys-ctrl.descrip  = "OEPrepTaxCode?".
  
end.
assign
 PrepTax-log = sys-ctrl.log-fld
 PrepTax-cha = sys-ctrl.char-fld.

