/* sys/inc/acksps.i*/

def VAR AckSPS-log like sys-ctrl.log-fld no-undo.
def var AckSPS-cha like sys-ctrl.char-fld no-undo.
def var AckSPS-int like sys-ctrl.int-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "AckSPS"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "AckSPS"
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = "c:\temp\"
   sys-ctrl.descrip  = "Generate SPS Acknowledgement?".
 
end.
assign
 AckSPS-log = sys-ctrl.log-fld
 AckSPS-cha = sys-ctrl.char-fld
 AckSPS-int = sys-ctrl.int-fld .

