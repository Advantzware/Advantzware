
def var oereleas-log like sys-ctrl.log-fld no-undo.
def var oereleas-cha like sys-ctrl.char-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OERELEAS"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "OERELEAS"
   sys-ctrl.log-fld  = yes
   sys-ctrl.char-fld = "Due Date"
   sys-ctrl.descrip  = "Create scheduled release when adding an order line?".

end.
assign
 oereleas-log = sys-ctrl.log-fld
 oereleas-cha = sys-ctrl.char-fld.

