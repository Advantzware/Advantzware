/* empmach.i */

DEFINE VARIABLE op-company AS CHARACTER NO-UNDO.
DEFINE VARIABLE op-employee AS CHARACTER NO-UNDO.

{methods/run_link.i "RECORD-SOURCE" "Get-Values"
    "(OUTPUT op-company,OUTPUT op-employee)"}
ASSIGN
  {&FIRST-EXTERNAL-TABLE}.company = op-company
  {&FIRST-EXTERNAL-TABLE}.employee = op-employee.
