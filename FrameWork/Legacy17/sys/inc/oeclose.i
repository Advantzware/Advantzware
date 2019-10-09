
def var oeclose-chr like sys-ctrl.char-fld init "Complete,OnHand=0" no-undo.
DEF VAR oeclose-log AS LOG NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OECLOSE"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "OECLOSE"
   sys-ctrl.char-fld = entry(1,oeclose-chr)
   sys-ctrl.descrip  = "Close Customer Orders when...?".
   
  do while true:
    message trim(sys-ctrl.descrip) + "   (" + trim(oeclose-chr) + ")" 
        update sys-ctrl.char-fld.
    if lookup(sys-ctrl.char-fld,oeclose-chr) gt 0 then leave.
  end.
end.

ASSIGN
   oeclose-chr = sys-ctrl.char-fld
   oeclose-log = sys-ctrl.log-fld.
