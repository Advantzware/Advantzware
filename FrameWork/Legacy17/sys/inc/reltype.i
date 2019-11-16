/* sys/inc/RelType.i*/

def var RelType-log like sys-ctrl.log-fld no-undo.
def var RelType-cha like sys-ctrl.char-fld no-undo.
def var RelType-int like sys-ctrl.int-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "RELTYPE"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "RelType"
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = ""
   sys-ctrl.descrip  = "Default scheduled release type (Ship Only,Invoice Only,Both,Transfer)".
 
end.
assign
 RelType-log = sys-ctrl.log-fld
 RelType-cha = sys-ctrl.char-fld
 RelType-int = sys-ctrl.int-fld .

