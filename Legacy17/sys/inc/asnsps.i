/* sys/inc/asnsps.i*/

def VAR ASNSPS-log like sys-ctrl.log-fld no-undo.
def var ASNSPS-cha like sys-ctrl.char-fld no-undo.
def var ASNSPS-int like sys-ctrl.int-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "ASNSPS"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "ASNSPS"
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = "c:\temp\"
   sys-ctrl.descrip  = "Generate SPS ASN?".
 
end.
assign
 ASNSPS-log = sys-ctrl.log-fld
 ASNSPS-cha = sys-ctrl.char-fld
 ASNSPS-int = sys-ctrl.int-fld .

