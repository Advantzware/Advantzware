/* sys/inc/CEPDies.i */

DEF VAR cepdies-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR cepdies-cha LIKE sys-ctrl.char-fld NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CEPDIES"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CEPDies"
   sys-ctrl.DESCRIP = "Check for Partition Dies on Maching Routing for Manufactured Partitons".
end.
ASSIGN
 cepdies-log = sys-ctrl.log-fld
 cepdies-cha = sys-ctrl.char-fld.
