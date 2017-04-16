
if not avail ce-ctrl then
find first ce-ctrl where ce-ctrl.company eq cocode no-lock no-error.

if avail ce-ctrl then msf-blok:
do j = 1 to 2:
  do i = 1 to 6.
    if j eq 1 then do:
      {2} = ce-ctrl.mat-pct[i].
      if ce-ctrl.mat-cost[i] ge {1} then leave msf-blok.
    end.
    
    else do:
      {2} = ce-ctrl.lab-pct[i].
      if ce-ctrl.lab-cost[i] ge {1} then leave msf-blok.
    end.
  end.
end.
