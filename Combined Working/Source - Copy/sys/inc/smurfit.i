
DEF VAR smurfit-dir LIKE sys-ctrl.descrip NO-UNDO.
DEF VAR smurfit-log LIKE sys-ctrl.log-fld NO-UNDO.

{sys/inc/poexport.i}

RELEASE sys-ctrl.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "Smurfit"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "Smurfit".
end.
assign
 smurfit-dir = sys-ctrl.descrip
 smurfit-log = sys-ctrl.log-fld.
