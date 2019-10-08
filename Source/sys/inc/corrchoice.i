
DEF VAR corrchoice-dir LIKE sys-ctrl.descrip NO-UNDO.
DEF VAR corrchoice-log LIKE sys-ctrl.log-fld NO-UNDO.

{sys/inc/poexport.i}

RELEASE sys-ctrl.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CORRCHOICE"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CORRCHOICE".
end.
assign
 corrchoice-dir = sys-ctrl.descrip
 corrchoice-log = sys-ctrl.log-fld.
 
