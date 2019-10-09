/*sys/inc/fgpost.i*/

def var fgpost-log like sys-ctrl.log-fld no-undo.
def var fgpost-cha like sys-ctrl.char-fld no-undo.
DEF VAR fgpost-int LIKE sys-ctrl.int-fld NO-UNDO.


find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "FGPOST" no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "FGPOST"
   sys-ctrl.log-fld  = yes
   sys-ctrl.int-fld  = 0
   sys-ctrl.descrip  = "Print posting report before posting?  No for after.".
   
  
end.

assign
 fgpost-cha = sys-ctrl.char-fld
 fgpost-log = sys-ctrl.log-fld
 fgpost-int = sys-ctrl.int-fld.

IF fgpost-cha EQ "" THEN fgpost-cha = STRING(fgpost-log,"Before/After").
