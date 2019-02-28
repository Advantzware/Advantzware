output to firstOrderReport.txt.

for each company no-lock,
  last oe-ord no-lock where oe-ord.company eq company.company  use-index ordate.

  if avail oe-ord then 
    put unformatted "LastOrder=" + "Company " oe-ord.company " " company.name " Order Date " oe-ord.ord-date skip.
  else 
    put unformatted "LastOrder=No order Found " company.company skip.

end.
output close.

QUIT.
