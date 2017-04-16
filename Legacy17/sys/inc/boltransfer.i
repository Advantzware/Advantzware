/* sys/inc/BOLTransfer.i*/

def var BOLTransfer-log like sys-ctrl.log-fld no-undo.
def var BOLTransfer-cha like sys-ctrl.char-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "BOLTransfer"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "BOLTransfer"
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = ""
   sys-ctrl.descrip  = "BOLTransfer".
 
end.
assign
 BOLTransfer-log = sys-ctrl.log-fld
 BOLTransfer-cha = sys-ctrl.char-fld.

