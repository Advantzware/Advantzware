DEF VAR v-ceroute#out-log AS LOG NO-UNDO.
           
find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CEROUTE#OUT"
    no-lock no-error.

if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CEROUTE#OUT"
   sys-ctrl.descrip  = "Allow updating the PREP/ROUTE folder Number Out for DC/GL for Folding".
end.

v-ceroute#out-log = sys-ctrl.log-fld.

