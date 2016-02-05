
DEF VAR kiwi-dir LIKE sys-ctrl.descrip NO-UNDO.
DEF VAR kiwi-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR kiwi-char LIKE sys-ctrl.char-fld NO-UNDO.

{sys/inc/poexport.i}

RELEASE sys-ctrl.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "Kiwi"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "Kiwi".
end.
assign
 kiwi-dir = sys-ctrl.descrip
 kiwi-log = sys-ctrl.log-fld
 kiwi-char = sys-ctrl.char-fld.

