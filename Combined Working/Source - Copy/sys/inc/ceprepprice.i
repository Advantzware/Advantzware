def var ceprepprice-chr AS CHAR no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CEPREPPRICE"
    no-lock no-error.
if not avail sys-ctrl then DO TRANSACTION:
   create sys-ctrl.
   assign
      sys-ctrl.company  = cocode
      sys-ctrl.name     = "CEPREPPRICE"
      sys-ctrl.char-fld = "Profit"
      sys-ctrl.descrip  = "Markup Formula for Prep and Misc. Charges".
end.

ceprepprice-chr = sys-ctrl.char-fld.
