
{custom/globdefs.i}

{sys/inc/var.i new shared}


SESSION:SET-WAIT-STATE("general").

assign
 cocode = g_company
 locode = g_loc.

for each gltrans where gltrans.company eq cocode,
    first period
    where period.company eq gltrans.company
      and period.pst     le gltrans.tr-date
      and period.pend    ge gltrans.tr-date
      and period.pnum    ne gltrans.period
    no-lock:
    
  gltrans.period = period.pnum.
end.

SESSION:SET-WAIT-STATE("").
