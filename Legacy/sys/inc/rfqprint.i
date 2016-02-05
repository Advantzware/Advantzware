
def var rfqprint-char like sys-ctrl.char-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "RFQPRINT"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "RFQPRINT"
   sys-ctrl.descrip = "RFQ Print Mode"
   sys-ctrl.char-fld = "Customer".
end.
rfqprint-char = sys-ctrl.char-fld.
