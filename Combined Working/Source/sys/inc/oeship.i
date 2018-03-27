
def var oeship-log like sys-ctrl.log-fld no-undo.
def var oeship-cha like sys-ctrl.char-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OESHIP"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "OESHIP"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Ship components of an unassembled set?".
   
 
end.
ASSIGN oeship-log = sys-ctrl.log-fld
       oeship-cha = sys-ctrl.char-fld.
