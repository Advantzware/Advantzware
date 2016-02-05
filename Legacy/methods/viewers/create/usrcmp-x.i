/* usrcmp-l.i */

{methods/run_link.i "RECORD-SOURCE" "Get-Values"
    "(OUTPUT op-user_id,OUTPUT op-company)"}

ASSIGN
  usrx.uid = op-user_id
  usrx.company = g_company.
