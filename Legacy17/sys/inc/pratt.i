/* sys/inc/pratt.i */

DEF VAR pratt-dir LIKE sys-ctrl.descrip NO-UNDO.
DEF VAR pratt-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR pratt-cha LIKE sys-ctrl.char-fld NO-UNDO.


{sys/inc/poexport.i}

RELEASE sys-ctrl.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "Pratt"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "Pratt".
end.
assign
 pratt-dir = sys-ctrl.descrip
 pratt-log = sys-ctrl.log-fld
 pratt-cha = sys-ctrl.char-fld.
