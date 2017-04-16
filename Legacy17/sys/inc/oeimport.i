/* sys/inc/oeimport.ii*/

def var oeimport-log like sys-ctrl.log-fld no-undo.
def var oeimport-cha like sys-ctrl.char-fld no-undo.
def var oeimport-int like sys-ctrl.int-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "oeimport"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "oeimport"
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = ""
   sys-ctrl.descrip  = "Import Customer Purchase Orders  Electronically?".
 
end.
assign
 oeimport-log = sys-ctrl.log-fld
 oeimport-cha = sys-ctrl.char-fld
 oeimport-int = sys-ctrl.int-fld   .

