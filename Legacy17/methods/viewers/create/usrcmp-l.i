/* usrcmp-l.i */

{methods/run_link.i "RECORD-SOURCE" "Get-Values"
    "(OUTPUT op-user_id,OUTPUT op-company)"}
ASSIGN
  usercomp.user_id = op-user_id
  usercomp.company = op-company.
