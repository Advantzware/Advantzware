
def var v-prt-inst  like sys-ctrl.log-fld.
def var v-exc-depts like sys-ctrl.char-fld.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "NOTES"
    no-lock no-error.

if not avail sys-ctrl then do &if '{1}' eq '' &then transaction &endif:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "NOTES"
   sys-ctrl.descrip  = "Print Manufacturing Instuctions"
   sys-ctrl.log-fld  = yes.
end.

assign
 v-prt-inst  = sys-ctrl.log-fld
 v-exc-depts = sys-ctrl.char-fld.

