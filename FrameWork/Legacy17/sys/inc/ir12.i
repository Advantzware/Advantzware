/*sys/inc/IR12.i*/

def var IR12-log like sys-ctrl.log-fld no-undo.
def var IR12-cha like sys-ctrl.char-fld no-undo.
DEF VAR IR12-int LIKE sys-ctrl.int-fld NO-UNDO.


find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "IR12" no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "IR12"
   sys-ctrl.log-fld  = no
   sys-ctrl.int-fld  = 0
   sys-ctrl.descrip  = "Prompt for standard for custom IR12 report? ".
   
  
end.

assign
 IR12-cha = sys-ctrl.char-fld
 IR12-log = sys-ctrl.log-fld
 IR12-int = sys-ctrl.int-fld.

