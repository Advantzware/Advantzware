/* --------------------------------------------------- fg/fg-rdtl.i 11/93 rd */
/* Finished Goods - Warehouse Transactions Posting                            */
/*   Create Transaction History Records.                                     */
/* -------------------------------------------------------------------------- */
/* UserId not copied from fg-rctd since it uses different field names */
BUFFER-COPY {2} EXCEPT rec_key TO {1}
ASSIGN
 {1}.qty          = {2}.t-qty
 {1}.cost         = {2}.std-cost
 {1}.stacks-unit  = {2}.cases-unit
 {1}.user-id      = USERID('asi')
 .

IF {1}.cost EQ 0 THEN
  {1}.cost = {2}.ext-cost / ({2}.t-qty / IF {2}.cost-uom EQ "M" THEN 1000 ELSE 1).

IF {1}.cost EQ ? THEN {1}.cost = {2}.cost.
IF {1}.cost EQ ? THEN {1}.cost = 0.

IF {2}.rita-code EQ "T" THEN DO:
  ASSIGN
   {1}.qty     = {2}.t-qty * -1
   {1}.cases   = {2}.cases * -1.
  
  CREATE {1}.
  BUFFER-COPY {2} EXCEPT rec_key TO {1}
  ASSIGN
   {1}.loc          = {2}.loc2
   {1}.loc-bin      = {2}.loc-bin2
   {1}.tag          = {2}.tag2
   {1}.qty          = {2}.t-qty
   {1}.cost         = {2}.std-cost
   {1}.stacks-unit  = {2}.cases-unit
   {1}.user-id      = USERID('asi')
   .
   
  IF {1}.cost EQ 0 THEN
    {1}.cost = {2}.ext-cost / ({2}.t-qty / IF {2}.cost-uom EQ "M" THEN 1000 ELSE 1).

  IF {1}.cost EQ ? THEN {1}.cost = {2}.cost.
  IF {1}.cost EQ ? THEN {1}.cost = 0.
END.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

