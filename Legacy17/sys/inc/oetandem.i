
def var oetandem-log like sys-ctrl.log-fld no-undo.
def var oetandem-cha like sys-ctrl.char-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OETANDEM"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "OETANDEM"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Create tandem estimates from OE?".
  
end.
assign
 oetandem-log = sys-ctrl.log-fld
 oetandem-cha = sys-ctrl.char-fld.

