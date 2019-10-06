/* sys/inc/edimonimp.i*/

def VAR EDIMONIMP-log like sys-ctrl.log-fld no-undo.
def var EDIMONIMP-cha like sys-ctrl.char-fld no-undo.
def var EDIMONIMP-int like sys-ctrl.int-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "EDIMON"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "EDIMONImp"
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = "c:\temp\"
   sys-ctrl.descrip  = "Run EDI Monitor to import edi orders?".
 
end.
assign
 EDIMONIMP-log = sys-ctrl.log-fld
 EDIMONIMP-cha = sys-ctrl.char-fld
 EDIMONIMP-int = sys-ctrl.int-fld .

