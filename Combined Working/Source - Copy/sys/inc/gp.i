
DEF VAR gp-dir LIKE sys-ctrl.descrip NO-UNDO.
DEF VAR gp-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR gp-cha LIKE sys-ctrl.char-fld NO-UNDO.


{sys/inc/poexport.i}

RELEASE sys-ctrl.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "GP"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "GP".
end.
assign
 gp-dir = sys-ctrl.descrip
 gp-log = sys-ctrl.log-fld
 gp-cha = sys-ctrl.char-fld.
