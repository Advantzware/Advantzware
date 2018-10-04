/* btnSave.i - used in trigger for btnSave in board.w */

  DEFINE VARIABLE initialDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE downtimeFile AS CHARACTER NO-UNDO.

  /* IF NOT proOpts[2] THEN DO:
    MESSAGE proOptsMsg(2) VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
  END. */
  
  initialDir = '{&scenarios}/' + ID.
  IF scenario EQ '<New>' THEN DO:
    scenario = ?.
    /* RUN {&prompts}/saveschd.w (INPUT-OUTPUT scenario). */
    SYSTEM-DIALOG GET-FILE scenario
      TITLE 'Choose Scenario File'
      INITIAL-DIR initialDir
      DEFAULT-EXTENSION '.dat'
      SAVE-AS
      ASK-OVERWRITE
      RETURN-TO-START-DIR
      FILTERS 'Scenario Files (*.dat)' '*.dat'
      UPDATE ldummy.
    IF scenario EQ ? THEN DO:
      ASSIGN scenario.
      RETURN NO-APPLY.
    END.
    ENABLE btnRemove btnReset WITH FRAME {&FRAME-NAME}.
    ASSIGN
      scenario = SUBSTRING(scenario,R-INDEX(scenario,'\') + 1)
      scenario = REPLACE(scenario,'.dat','').
    IF NOT CAN-DO(scenario:LIST-ITEMS,scenario) THEN
    scenario:ADD-LAST(scenario).
    scenario:SCREEN-VALUE = scenario.
  END.
  
  RUN setScreenStatus.
  RUN saveScenario (initialDir).
  ASSIGN
    accumTimeInterval = ETIME(YES)
    saveTimeInterval = 0
    schdChanged = NO.
  IF scenario EQ 'Actual' THEN DO:
    RUN msgFrame ('Upload Scheduler {&Board} Jobs').
    RUN VALUE(findProgram('{&loads}/',ID,'/upload.p')).
  END.
  ELSE DO:
    downtimeFile = '{&data}/' + ID + '/downtimes.' + scenario + '.dat'.
    IF SEARCH(downtimeFile) EQ ? THEN
    OS-COPY VALUE(SEARCH('{&data}/' + ID + '/downtimes.Actual.dat')) VALUE(downtimeFile).
    RUN changeDowntimeScenario.
  END.
  /* RUN buildBoard (YES). */
  RUN msgFrame (?).
  HIDE FRAME msgFrame NO-PAUSE.
  RUN LockWindowUpdate (0,OUTPUT i).
