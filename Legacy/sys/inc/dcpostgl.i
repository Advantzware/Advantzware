
def var dcpostgl-log like sys-ctrl.log-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "DCPOSTGL"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "DCPOSTGL"
   sys-ctrl.log-fld  = no
   sys-ctrl.descrip  = "Post GL for Data Collection?".
   
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.

dcpostgl-log = sys-ctrl.log-fld.
