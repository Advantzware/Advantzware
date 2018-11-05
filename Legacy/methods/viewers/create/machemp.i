/* machemp.i */

DEFINE VARIABLE op-rec_key AS CHARACTER NO-UNDO.

{methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT op-rec_key)"}
machemp.table_rec_key = op-rec_key.

{methods/viewers/create/timeflds.i}
