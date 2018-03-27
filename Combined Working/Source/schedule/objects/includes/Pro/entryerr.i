/* entryerr.i */

&IF "{&can-find}" NE "" &THEN
correct-error = NOT CAN-FIND({&can-find}).
&ENDIF

IF correct-error THEN DO:
  MESSAGE '{&error-message}' SKIP(1) 'Correct Entry?'
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'ERROR - Invalid Entry'
      UPDATE correct-error.
  IF correct-error THEN DO:
    APPLY 'ENTRY' TO SELF.
    RETURN NO-APPLY.
  END.
  ELSE
&IF '{&FIRST-EXTERNAL-TABLE}' NE '' &THEN
  IF AVAILABLE {&FIRST-EXTERNAL-TABLE} THEN
&ENDIF
  SELF:SCREEN-VALUE = STRING({&SELF-NAME}).
END.
