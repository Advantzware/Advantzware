/*sys/inc/fgreorder.i */
def var fgreorder-log like sys-ctrl.log-fld no-undo.
DEF VAR fgreorder-char LIKE sys-ctrl.char-fld NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGReorder"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "FGReOrder"
   sys-ctrl.descrip = "FGReOrder?"
   sys-ctrl.char-fld  = "ReOrder Point"
   sys-ctrl.log-fld = no   .
end.
ASSIGN fgreorder-log = sys-ctrl.log-fld
       fgreorder-char = sys-ctrl.char-fld.
