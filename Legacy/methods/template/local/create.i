/* create.i */

DEFINE VARIABLE allow-create AS LOGICAL NO-UNDO.

{methods/run_link.i "CONTAINER-SOURCE" "Allow-Create" "(OUTPUT allow-create)"}
IF NOT allow-create THEN
RETURN "ADM-ERROR":U.

&IF "{&create-more}" <> "" &THEN
    {{&create-more}.i}
&ENDIF
