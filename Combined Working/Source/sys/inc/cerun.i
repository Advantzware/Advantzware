
def var cerun{1} as char.
def var v-hop{1} as int.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CERUN" + "{1}"
    no-lock no-error.

if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.char-fld = "ASI".
   sys-ctrl.name     = "CERUN" + "{1}".
  
  if "{1}" eq "" then
    sys-ctrl.descrip = "Calc Qty Price Break? Print Method? " +
                       "Default Prompt for Whatif?".
  else
    sys-ctrl.descrip = "Default for CE machine run speed (" +
                       (if "{1}" eq "F" then "Fold" else "Corr") + "ware)".

end.

cerun{1} = sys-ctrl.char-fld.

