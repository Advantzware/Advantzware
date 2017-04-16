/*sys/inc/tstimeb.i */
def var tstimeb-log like sys-ctrl.log-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq g_company
      and sys-ctrl.name    eq "TSTIMEB"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = g_company
   sys-ctrl.name    = "TSTIMEB"
   sys-ctrl.descrip = "Always enable TS Time Buttons?".
end.
ASSIGN tstimeb-log = sys-ctrl.log-fld.
       
