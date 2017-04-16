
def var fgpostgl like sys-ctrl.char-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGPOSTGL"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "FGPOSTGL"
   sys-ctrl.log-fld  = yes
   sys-ctrl.descrip  = "Post GL for purchased FG Receipts".
   
 
end.
fgpostgl = sys-ctrl.char-fld.

IF fgpostgl EQ "" THEN DO:
  fgpostgl = STRING(sys-ctrl.log-fld,"POOnly,None").
  FIND CURRENT sys-ctrl EXCLUSIVE NO-WAIT NO-ERROR.
  IF AVAIL sys-ctrl THEN DO:
    sys-ctrl.char-fld = fgpostgl.
    FIND CURRENT sys-ctrl NO-LOCK.
  END.
END.


