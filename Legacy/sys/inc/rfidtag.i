/* sys/inc/RFIDTag.i*/

def var RFIDTag-log like sys-ctrl.log-fld no-undo.
def var RFIDTag-cha like sys-ctrl.char-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "RFIDTag"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "RFIDTAG"
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = ""
   sys-ctrl.descrip  = "RFIDTAG".
 
end.
assign
 RFIDTag-log = sys-ctrl.log-fld
 RFIDTag-cha = sys-ctrl.char-fld.

