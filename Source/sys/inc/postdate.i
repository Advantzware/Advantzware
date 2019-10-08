
def var postdate-log like sys-ctrl.log-fld no-undo.
def var postdate-dat like sys-ctrl.date-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "POSTDATE"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "POSTDATE"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Default Posting Date to today's date?".
   
  
end.
assign
 postdate-log = sys-ctrl.log-fld
 postdate-dat = sys-ctrl.date-fld.
