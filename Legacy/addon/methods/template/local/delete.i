/* delete.i */

DEFINE VARIABLE allow-delete AS LOGICAL NO-UNDO.

{methods/run_link.i "CONTAINER-SOURCE" "Allow-Delete" "(OUTPUT allow-delete)"}
IF NOT allow-delete THEN
RETURN "ADM-ERROR":U.

&IF INDEX("{custom/delete.i}","{&FIRST-EXTERNAL-TABLE}") NE 0 &THEN
{methods/viewers/delete/{&FIRST-EXTERNAL-TABLE}.i}
&ENDIF

MESSAGE "Delete Currently Selected Record?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE response AS LOGICAL.
IF NOT response THEN
RETURN "ADM-ERROR":U.
