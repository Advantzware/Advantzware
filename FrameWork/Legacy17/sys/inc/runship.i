
def var runship-log like sys-ctrl.log-fld no-undo.
DEF VAR runship-char LIKE sys-ctrl.char-fld NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "RUNSHIP"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "RUNSHIP"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Show Run and Ship Box in O/E?".
end.
ASSIGN runship-log  = sys-ctrl.log-fld
       runship-char = sys-ctrl.char-fld.
