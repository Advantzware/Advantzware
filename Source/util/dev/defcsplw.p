
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.


find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

for each item
    where item.company eq cocode
      and index("DC",item.mat-type) gt 0:
  
  item.basis-w = if item.mat-type eq "C" then ce-ctrl.def-cas-w
                                         else ce-ctrl.def-pal-w.
end.
