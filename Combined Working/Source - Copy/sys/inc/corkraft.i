
DEF VAR corkraft-dir LIKE sys-ctrl.descrip NO-UNDO.
DEF VAR corkraft-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR corkraft-int LIKE sys-ctrl.int-fld NO-UNDO.


{sys/inc/poexport.i}

RELEASE sys-ctrl.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CorKraft"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CorKraft".
end.
assign
 corkraft-dir = sys-ctrl.descrip
 corkraft-log = sys-ctrl.log-fld
 corkraft-int = sys-ctrl.int-fld.

