
def var autopost like sys-ctrl.char-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "AUTOPOST"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "AUTOPOST"
   sys-ctrl.descrip = "Autopost to Finished Goods Receipts".
  message "System control record NOT found.  Would you like to autopost to FG?"
          update sys-ctrl.log-fld.
end.
autopost = sys-ctrl.char-fld.
