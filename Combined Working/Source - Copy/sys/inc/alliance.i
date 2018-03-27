
DEF VAR alliance-dir LIKE sys-ctrl.descrip NO-UNDO.
DEF VAR alliance-log LIKE sys-ctrl.log-fld NO-UNDO.


{sys/inc/poexport.i}

RELEASE sys-ctrl.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "ALLIANCE"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "ALLIANCE".
end.
assign
 alliance-dir = sys-ctrl.descrip
 alliance-log = sys-ctrl.log-fld.
