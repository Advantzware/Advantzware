
def var cegoto-log like sys-ctrl.log-fld.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CEGOTO"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CEGOTO"
   sys-ctrl.log-fld = YES
   sys-ctrl.descrip = "Display Request/Yield Qtys after adding each Combo Part#?".

  MESSAGE TRIM(sys-ctrl.descrip)
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.

cegoto-log = sys-ctrl.log-fld.
