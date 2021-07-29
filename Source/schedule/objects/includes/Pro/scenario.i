/* scenario.i - used in value-changed trigger for scenario in board.w */

  DEFINE VARIABLE initialDir AS CHARACTER NO-UNDO.

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
  
  DO WITH FRAME {&FRAME-NAME}:
    CASE {&SELF-NAME}:
      WHEN '<New>' THEN DO:
        DISABLE btnRemove btnReset.
        RETURN NO-APPLY.
      END. /* new  */
      WHEN '<Open>' THEN DO:
        initialDir = REPLACE('{&scenarios}/' + ID,"/","~\").
        SYSTEM-DIALOG GET-FILE scenario
          TITLE 'Choose Scenario File'
          INITIAL-DIR initialDir
          DEFAULT-EXTENSION '.dat'
          RETURN-TO-START-DIR
          FILTERS 'Scenario Files (*.dat)' '*.dat'
          UPDATE ldummy.
        IF scenario EQ ? OR scenario EQ "<Open>" THEN
        scenario = "Actual".
        ASSIGN
          scenario = SUBSTRING(scenario,R-INDEX(scenario,'\') + 1)
          scenario = REPLACE(scenario,'.dat','')
          scenario:SCREEN-VALUE = scenario.
          .
      END. /* open */
      OTHERWISE
      ENABLE btnRemove btnReset.
    END.
  END.
  
  IF showStatus OR openBoard THEN
  VIEW FRAME msgFrame.
  ELSE
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
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
