/* btnRemove.i - used in trigger for btnRemove in board.w */

  IF scenario EQ 'Actual' THEN
  DO:
    MESSAGE 'Scenario "Actual" can not be Removed!' VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
  END.
  MESSAGE 'Remove Scenario: "' + scenario + '"?' VIEW-AS ALERT-BOX QUESTION
    BUTTONS YES-NO UPDATE removeScenario AS LOGICAL.
  IF NOT removeScenario THEN
  RETURN NO-APPLY.
  OS-DELETE VALUE('{&scenarios}/' + ID + '/' + scenario + '.dat').
  OS-DELETE VALUE('{&data}/' + ID + '/downtimes.' + scenario + '.dat').
  ASSIGN
    ldummy = scenario:DELETE(scenario)
    scenario:SCREEN-VALUE = 'Actual'
    schdChanged = NO.
  APPLY 'VALUE-CHANGED' TO scenario.
