
def var fgsetrec like sys-ctrl.char-fld no-undo.
def var fgsetrec-log like sys-ctrl.log-fld no-undo.
def var fgsetrec-int like sys-ctrl.int-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGSETREC"
    no-lock no-error.
    
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "FGSETREC"
   sys-ctrl.descrip  = "Whse/Bin Location for components of Unassembled Sets" +
                       " (Set/Item)"
   sys-ctrl.char-fld = "Set".
 /* message sys-ctrl.descrip update sys-ctrl.char-fld. */
  if sys-ctrl.char-fld ne "Set"  and
     sys-ctrl.char-fld ne "Item" then undo, retry.
end.
assign
 fgsetrec     = sys-ctrl.char-fld
 fgsetrec-log = sys-ctrl.log-fld
 fgsetrec-int = sys-ctrl.int-fld.

