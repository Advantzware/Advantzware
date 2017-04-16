/* sys/inc/corrtrim.i */

DEF VAR corrtrim-dir LIKE sys-ctrl.descrip NO-UNDO.
DEF VAR corrtrim-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR corrtrim-char LIKE sys-ctrl.char-fld NO-UNDO.


{sys/inc/poexport.i}

RELEASE sys-ctrl.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CORRTRIM"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CORRTRIM".
end.
assign
 corrtrim-dir = sys-ctrl.descrip
 corrtrim-log = sys-ctrl.log-fld
 corrtrim-char = sys-ctrl.char-fld  .
 
