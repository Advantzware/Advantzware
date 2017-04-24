/* sys/inc/ackmst.i*/

def VAR AckMst-log like sys-ctrl.log-fld no-undo.
def var AckMst-cha like sys-ctrl.char-fld no-undo.
def var AckMst-int like sys-ctrl.int-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "AckMaster"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "AckMaster"
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = "3CPack"
   sys-ctrl.descrip  = "Default Acknowledgement Order Master".
 
end.
assign
 AckMst-log = sys-ctrl.log-fld
 AckMst-cha = sys-ctrl.char-fld
 AckMst-int = sys-ctrl.int-fld .

