
if lastship-cha eq "Fibre" then
  assign
   {1}lead-days = {1}lead-days * {2}
   {1}last-date = {1}ord-date + {1}lead-days
   {1}due-date  = {1}ord-date + (lastship-int * {2}).
