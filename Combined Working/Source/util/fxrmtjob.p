
SESSION:SET-WAIT-STATE ("general").

for each rm-rcpth where job-no ne "" no-lock,
    each rm-rdtlh
    where rm-rdtlh.company   eq rm-rcpth.company
      and rm-rdtlh.r-no      eq rm-rcpth.r-no
      and rm-rdtlh.rita-code eq rm-rdtlh.rita-code
      and rm-rdtlh.job-no    eq "":
    
  assign
   rm-rdtlh.job-no  = rm-rcpth.job-no
   rm-rdtlh.job-no2 = rm-rcpth.job-no2.
end.

SESSION:SET-WAIT-STATE ("").
