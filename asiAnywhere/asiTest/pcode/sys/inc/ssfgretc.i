/* sys/inc/ssfgretc.i */

def var ssfgretc-log AS LOG NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "SSFGRETC"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "SSFGRETC"
   sys-ctrl.log-fld  = no
   sys-ctrl.descrip  = "SS FG Returns Create Physical Count?".
end.
ssfgretc-log = sys-ctrl.log-fld.

