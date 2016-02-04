
DEF VAR corsuply-dir LIKE sys-ctrl.descrip NO-UNDO.
DEF VAR corsuply-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR corsuply-cha LIKE sys-ctrl.char-fld NO-UNDO.


{sys/inc/poexport.i}

RELEASE sys-ctrl.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CorSuply"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CorSuply".
end.
assign
 corsuply-dir = sys-ctrl.descrip
 corsuply-log = sys-ctrl.log-fld
 corsuply-cha = sys-ctrl.char-fld.

