/* usercust.i */

{methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT op-user_id)"}
ASSIGN 
  usercust.user_id = op-user_id
  usercust.company = g_company.
