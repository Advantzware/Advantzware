

def var v-min-mchg  like sys-ctrl.log-fld no-undo.
def var v-markup    like sys-ctrl.dec-fld no-undo.
def var ceprice-chr like sys-ctrl.char-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CEPRICE"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CEPRICE"
   sys-ctrl.descrip = "Utilize Market Rate as the Minimum Charge per machine?"
   sys-ctrl.log-fld = no.
 
end.

assign
 v-min-mchg  = sys-ctrl.log-fld
 v-markup    = sys-ctrl.dec-fld
 ceprice-chr = sys-ctrl.char-fld.
