/* update.i */

DEFINE VARIABLE allow-update AS LOGICAL NO-UNDO.

{methods/run_link.i "CONTAINER-SOURCE" "Allow-Update" "(OUTPUT allow-update)"}
IF NOT allow-update THEN
RETURN "ADM-ERROR":U.
