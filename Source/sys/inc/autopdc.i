
def var autopdc like sys-ctrl.descrip.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "AUTOPDC"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.log-fld = no
   sys-ctrl.name    = "AUTOPDC".
   
  MESSAGE "Automatic Plant Data Collection?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
  
  if sys-ctrl.log-fld then
    message "Enter Machines ('*' for all):" update sys-ctrl.descrip.
end.

autopdc = if sys-ctrl.log-fld then sys-ctrl.descrip else "".

