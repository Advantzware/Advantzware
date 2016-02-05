
if index("RATCE",fg-rcpth.rita-code) ne 0 then
  fg-bin.qty = fg-rdtlh.qty +
               if fg-rcpth.rita-code eq "C" then 0 else fg-bin.qty.

else
  fg-bin.qty = fg-bin.qty - fg-rdtlh.qty.
