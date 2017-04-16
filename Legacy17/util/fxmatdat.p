
{custom/globdefs.i}

{sys/inc/var.i new shared}


SESSION:SET-WAIT-STATE("general").

assign
 cocode = g_company
 locode = g_loc.

for each mat-act
    where mat-act.company  eq cocode
      and mat-act.mat-date eq ?:

  for each rm-rcpth
      where rm-rcpth.company eq mat-act.company
        and rm-rcpth.i-no    eq mat-act.rm-i-no
        and rm-rcpth.job-no  eq mat-act.job-no
        and rm-rcpth.job-no2 eq mat-act.job-no2
        and rita-code        eq "I"                    
        and rm-rcpth.rec_key gt mat-act.rec_key
      no-lock,
      each rm-rdtlh
      where rm-rdtlh.r-no      eq rm-rcpth.r-no
        and rm-rdtlh.rita-code eq rm-rcpth.rita-code
      break by rm-rcpth.trans-date desc:
                  
    if (rm-rdtlh.s-num eq mat-act.s-num and
        (rm-rdtlh.b-num eq mat-act.b-num or mat-act.b-num eq 0)) or
       last(rm-rcpth.trans-date)                                 then do:
      mat-act.mat-date = rm-rcpth.post-date.
      leave.
    end.
  end.
end.

SESSION:SET-WAIT-STATE("").
