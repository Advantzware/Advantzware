
def var cewhschg-log as log no-undo.
def var cewhschg-int as int no-undo.
def var cewhschg-cha as cha no-undo.
def var cewhschg-dec as dec no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CEWHSCHG"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CEWHSCHG"
   sys-ctrl.char-fld = "Percent"
   sys-ctrl.descrip  = "Estimating warehouse charge method".
end.
assign
 cewhschg-log = sys-ctrl.log-fld
 cewhschg-int = sys-ctrl.int-fld   
 cewhschg-cha = sys-ctrl.char-fld   
 cewhschg-dec = sys-ctrl.dec-fld.
