
def var round-log like sys-ctrl.log-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "ROUND"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "ROUND"
   sys-ctrl.descrip = "Round Up Scoring Allowances?".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.

round-log = sys-ctrl.log-fld.

