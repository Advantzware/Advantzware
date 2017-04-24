/*sys/inc/tsendwash.i */
def var tsendwash-log like sys-ctrl.log-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq g_company
      and sys-ctrl.name    eq "TSENDWASH"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = g_company
   sys-ctrl.name    = "TSENDWASH"
   sys-ctrl.descrip = "Complete Always No for TS End Wash Operation?".
end.
tsendwash-log = sys-ctrl.log-fld.
       
