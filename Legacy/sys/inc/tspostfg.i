/*sys/inc/tspostfg.i */
def var tspostfg-log like sys-ctrl.log-fld no-undo.
def var tspostfg-char like sys-ctrl.char-fld no-undo.
def var tspostfg-int like sys-ctrl.int-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "TSPOSTFG"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "TSPOSTFG"
   sys-ctrl.descrip = "Touch-Screen does Autopost to Finished Goods Receipts?".

  
end.
ASSIGN tspostfg-log = sys-ctrl.log-fld
       tspostfg-char = sys-ctrl.char-fld
       tspostfg-int = sys-ctrl.int-fld
       .
