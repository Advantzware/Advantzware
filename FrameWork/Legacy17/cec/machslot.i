
if avail mach                                       and
   style.type eq "B"                                and
   (mach.dept[1] eq "PR" or mach.dept[1] eq "DC" or
    mach.dept[2] eq "PR" or mach.dept[2] eq "DC" or
    mach.dept[3] eq "PR" or mach.dept[3] eq "DC" or
    mach.dept[4] eq "PR" or mach.dept[4] eq "DC")   then do:

  if xeb.k-wid-array2[1] lt mach.min-dep then tt-mach-exc.reason = "Minimum Slot Size".
  else
  if xeb.k-wid-array2[1] gt mach.max-dep then tt-mach-exc.reason = "Maximum Slot Size".

  if tt-mach-exc.reason ne "" then release mach.
end.

