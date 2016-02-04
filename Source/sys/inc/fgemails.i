/*sys/inc/fgemails.i  */
def var fgemails like sys-ctrl.char-fld no-undo.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "FGEMAILS"
                      no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "FGEMAILS"
   sys-ctrl.log-fld  = yes
   sys-ctrl.char-fld = "NONE"
   sys-ctrl.descrip  = "FG post to automatically Email Customer Service for Hot Customers.".
end.

fgemails = sys-ctrl.char-fld.
