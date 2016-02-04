/* uservend.i */

{methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT op-user_id)"}
ASSIGN 
  uservend.user_id = op-user_id
  uservend.company = g_company.
