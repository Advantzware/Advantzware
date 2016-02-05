/* --------------------------------------------------- fg/fg-rdtl.i 11/93 rd */
/* Finished Goods - Warehouse Transactions Posting                            */
/*   Create Transaction History Records.                                     */
/* -------------------------------------------------------------------------- */

assign
 {1}.r-no      = {2}.r-no
 {1}.company   = {2}.company
 {1}.loc       = {2}.loc
 {1}.loc-bin   = {2}.loc-bin
 {1}.tag       = {2}.tag
 {1}.qty       = {2}.t-qty
 {1}.cost      = {2}.ext-cost / ({2}.t-qty / 1000)
 {1}.loc2      = {2}.loc2
 {1}.loc-bin2  = {2}.loc-bin2
 {1}.tag2      = {2}.tag2
 {1}.rita-code = {2}.rita-code
 {1}.cases     = {2}.cases
 {1}.qty-case  = {2}.qty-case.

if {2}.rita-code eq "T" then do:
  {1}.qty = {2}.t-qty * -1.
  
  create {1}.
  assign
   {1}.r-no      = {2}.r-no
   {1}.company   = {2}.company
   {1}.loc       = {2}.loc2
   {1}.loc-bin   = {2}.loc-bin2
   {1}.tag       = {2}.tag2
   {1}.qty       = {2}.t-qty
   {1}.cost      = {2}.ext-cost / ({2}.t-qty / 1000)
   {1}.rita-code = {2}.rita-code
   {1}.cases     = {2}.cases
   {1}.qty-case  = {2}.qty-case.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

