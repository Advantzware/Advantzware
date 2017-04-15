/* usercust.i */

{methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT op-user_id)"}
ASSIGN 
  usersman.user_id = op-user_id
  usersman.company = g_company.
