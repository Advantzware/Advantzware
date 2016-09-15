
def var fgpofrt-log like sys-ctrl.log-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGPOFRT"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "FGPOFRT"
   sys-ctrl.log-fld  = no
   sys-ctrl.descrip  = "Include the cost of freight into the cost of purchased goods?".
   
 
END.

fgpofrt-log = sys-ctrl.log-fld.
