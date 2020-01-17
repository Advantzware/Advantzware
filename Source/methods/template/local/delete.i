/* delete.i */

DEFINE VARIABLE allow-delete AS LOGICAL NO-UNDO.

IF NOT adm-new-record THEN DO:
  {methods/run_link.i "CONTAINER-SOURCE" "Allow-Delete" "(OUTPUT allow-delete)"}

  IF NOT allow-delete THEN RETURN "ADM-ERROR":U.

  &IF LOOKUP("{&FIRST-EXTERNAL-TABLE}","{custom/delete.i}"," ") NE 0 &THEN
  {methods/viewers/delete/{&FIRST-EXTERNAL-TABLE}.i}
  &ENDIF
  
  MESSAGE "Delete Currently Selected Record?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE response AS LOGICAL.

  &IF LOOKUP("{&FIRST-EXTERNAL-TABLE}","{custom/deleteSaveValue.i}"," ") NE 0 &THEN
  {methods/viewers/delete/{&FIRST-EXTERNAL-TABLE}SaveValue.i}
  &ENDIF

  IF NOT response THEN RETURN "ADM-ERROR":U.
END.
