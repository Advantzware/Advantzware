
def var v-out       as   char format "x(40)"        no-undo.
def var v-slash     as   char init "/"              no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "JDEDWDIR"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "JDEDWDIR"
   sys-ctrl.descrip = "z:\JD Edwards".
  message "JD Edwards Output Directory:" update sys-ctrl.descrip.
end.

assign
 v-out   = sys-ctrl.descrip
 v-slash = if opsys eq "unix" then "~/" else "~\".
