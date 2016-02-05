/* carr-mtx.i */

{methods/run_link.i "RECORD-SOURCE" "Get-Values"
  "(OUTPUT op-company,OUTPUT op-carrier,OUTPUT op-loc)"}
ASSIGN
  carr-mtx.company = op-company
  carr-mtx.carrier = op-carrier
  carr-mtx.loc = op-loc.
