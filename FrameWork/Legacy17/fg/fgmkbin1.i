
if index("RATCE",fg-rcpth.rita-code) ne 0 then
  {1} = fg-rdtlh.qty + if fg-rcpth.rita-code eq "C" then 0 else {1}.

else
  {1} = {1} - fg-rdtlh.qty.