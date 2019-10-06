
def var fgwhsbin-cha like sys-ctrl.char-fld no-undo.
def var fgwhsbin-log like sys-ctrl.log-fld no-undo.
def var fgwhsbin-int like sys-ctrl.int-fld no-undo.
def var fgwhsbin-dec like sys-ctrl.dec-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGWHSBIN"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "FGWHSBIN"
   sys-ctrl.descrip  = "Default Location for FG Warehouse / Bin?"
   sys-ctrl.char-fld = "AUTOPOST".
end.
assign
 fgwhsbin-cha = sys-ctrl.char-fld
 fgwhsbin-log = sys-ctrl.log-fld
 fgwhsbin-int = sys-ctrl.int-fld
 fgwhsbin-dec = sys-ctrl.dec-fld.
