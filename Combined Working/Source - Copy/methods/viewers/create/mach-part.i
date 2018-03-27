/* mach-part.i */

{methods/run_link.i "RECORD-SOURCE" "Get-Values"
  "(OUTPUT op-company,OUTPUT op-m-code)"}
ASSIGN
  mach-part.company = op-company
  mach-part.m-code = op-m-code.
