
DEF VAR hrms-dir LIKE sys-ctrl.descrip NO-UNDO.
DEF VAR hrms-log LIKE sys-ctrl.log-fld NO-UNDO.


{sys/inc/poexport.i}

RELEASE sys-ctrl.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "HRMS"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "HRMS".
end.
assign
 hrms-dir = sys-ctrl.descrip
 hrms-log = sys-ctrl.log-fld.

