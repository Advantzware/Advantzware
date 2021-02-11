
{custom/globdefs.i}

{sys/inc/var.i new shared}


SESSION:SET-WAIT-STATE("general").

assign
 cocode = g_company
 locode = g_loc.

for each glhist where glhist.company eq cocode
                AND glhist.posted EQ NO,
    first period
    where period.company eq glhist.company
      and period.pst     le glhist.tr-date
      and period.pend    ge glhist.tr-date
      and period.pnum    ne glhist.period
    no-lock:
    
  glhist.period = period.pnum.
end.

SESSION:SET-WAIT-STATE("").
