/* machchrg.i */

DEFINE VARIABLE op-company AS CHARACTER NO-UNDO.
DEFINE VARIABLE op-machine AS CHARACTER NO-UNDO.

{methods/run_link.i "RECORD-SOURCE" "Get-Values"
    "(OUTPUT op-company,OUTPUT op-machine)"}
ASSIGN
  {&FIRST-EXTERNAL-TABLE}.company = op-company
  {&FIRST-EXTERNAL-TABLE}.machine = op-machine.
