/* truck.i */

{methods/run_link.i "RECORD-SOURCE" "Get-Values"
  "(OUTPUT op-company,OUTPUT op-carrier,OUTPUT op-loc)"}
ASSIGN
  truck.company = op-company
  truck.carrier = op-carrier
  truck.loc = op-loc.
