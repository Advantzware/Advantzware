/* scenario.i - used in value-changed trigger for scenario in board.w */

  IF schdChanged THEN
  DO:
    MESSAGE 'Scenario "' + scenario + '" has been Changed.' SKIP
      'Save Scenario Changes?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE saveScenario AS LOGICAL.
    IF saveScenario THEN
    APPLY 'CHOOSE' TO btnSave.
  END.
  
  ASSIGN {&SELF-NAME}
    schdChanged = NO.
  
  IF {&SELF-NAME} EQ '<New>' THEN
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE btnRemove btnReset.
    RETURN NO-APPLY.
  END.
  ELSE
  ENABLE btnRemove btnReset WITH FRAME {&FRAME-NAME}.
  
  IF showStatus OR openBoard THEN
  VIEW FRAME msgFrame.
  ELSE
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
  EMPTY TEMP-TABLE ttblJob-do.
  /* DISABLE btnRedo btnUndo WITH FRAME {&FRAME-NAME}. */
  SESSION:SET-WAIT-STATE('GENERAL').
  RUN changeDowntimeScenario.
  RUN getConfiguration (NO).
  RUN getScenario.
  RUN getResource.
  RUN buildResource.
  APPLY 'VALUE-CHANGED' TO intervals.
  APPLY 'ENTRY' TO intervals.
  RETURN NO-APPLY.
