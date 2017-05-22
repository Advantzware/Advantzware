DEF VAR v-estopmch-log AS LOG NO-UNDO.
           
find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "ESTOPMCH"
    no-lock no-error.

if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "ESTOPMCH"
   sys-ctrl.descrip  = "Allow overriding machine in Estimate Operations?"
   sys-ctrl.log-fld  = YES.
end.

v-estopmch-log = sys-ctrl.log-fld.

