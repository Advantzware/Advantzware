/* fg-bin.i */

{methods/run_link.i "RECORD-SOURCE" "Get-Values"
    "(OUTPUT op-company,OUTPUT op-loc)"}
ASSIGN
  fg-bin.company = op-company
  fg-bin.loc = op-loc.
