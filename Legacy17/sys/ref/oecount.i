def var v-oecount like sys-ctrl.log-fld no-undo.
def var v-oecount-int like sys-ctrl.int-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OECOUNT"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "OECOUNT"
   sys-ctrl.descrip = "Default Order Entry Count to Case/Bundle Count?"
   sys-ctrl.log-fld = yes.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
assign
 v-oecount     = sys-ctrl.log-fld
 v-oecount-int = sys-ctrl.int-fld.
