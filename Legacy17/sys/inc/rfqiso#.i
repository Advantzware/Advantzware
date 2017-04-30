/*sys/inc/rfqiso#.i */
def var rfqiso#-char like sys-ctrl.char-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "RFQISO#"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "RFQISO#"
   sys-ctrl.descrip = "ISO Code".
end.
ASSIGN  rfqiso#-char = sys-ctrl.char-fld.
