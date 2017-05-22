/*sys/inc/tsclock.i */
def var tsclock-log like sys-ctrl.log-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq g_company
      and sys-ctrl.name    eq "TSCLOCK"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = g_company
   sys-ctrl.name    = "TSCLOCK"
   sys-ctrl.descrip = "Touch-Screen requires CLOCK IN/OUT?".

  
end.
ASSIGN tsclock-log = sys-ctrl.log-fld
       .
