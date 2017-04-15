/* sys/inc/stclair.i */

DEF VAR stclair-dir LIKE sys-ctrl.descrip NO-UNDO.
DEF VAR stclair-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR stclair-cha LIKE sys-ctrl.char-fld NO-UNDO.


{sys/inc/poexport.i}

RELEASE sys-ctrl.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "ST.Clair"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "ST.Clair".
end.
assign
 stclair-dir = sys-ctrl.descrip
 stclair-log = sys-ctrl.log-fld
 stclair-cha = sys-ctrl.char-fld.
