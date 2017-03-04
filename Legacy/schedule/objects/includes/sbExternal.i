/* sbExternal.i - used in sbReport.p */

{{&includes}/defBoard.i}
{{&includes}/sharedVars.i NEW}
{{&includes}/filterVars.i NEW}
{{&includes}/ttblJob.i NEW}

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

DEFINE VARIABLE containerHandle AS HANDLE    NO-UNDO.
DEFINE VARIABLE hSBHandle       AS HANDLE    NO-UNDO.
DEFINE VARIABLE loadProgram     AS CHARACTER NO-UNDO.
DEFINE VARIABLE reload          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE scenario        AS CHARACTER NO-UNDO INITIAL 'Actual'.

DEFINE SHARED VARIABLE g_company AS CHARACTER NO-UNDO.

SESSION:SET-WAIT-STATE('').

/* *** function definitions ******************************************** */
FUNCTION jobBGColor RETURNS INTEGER ():
  RETURN 0.
END FUNCTION.

FUNCTION jobFGColor RETURNS INTEGER ():
  RETURN 0.
END FUNCTION.

FUNCTION numericDateTime RETURNS DECIMAL (ipDate AS DATE,ipTime AS INTEGER):
  {{&includes}/numericDateTime.i}
END FUNCTION.

FUNCTION timeSpan RETURNS INTEGER PRIVATE
  (ipStartDate AS DATE,ipStartTime AS INTEGER,ipEndDate AS DATE,ipEndTime AS INTEGER) :
  {{&includes}/timeSpan.i}
END FUNCTION.

/* *** main block ****************************************************** */

RUN {&prompts}/ID.w (OUTPUT ID).
IF ID EQ '' THEN RETURN.
{{&includes}/startUp.i}
containerHandle = THIS-PROCEDURE:HANDLE.

PROCESS EVENTS.

RUN getConfiguration.
reload = IF '{&sbExternal}' EQ 'sbReport' THEN reloadReport
    ELSE IF '{&sbExternal}' EQ 'sbStatus' THEN reloadStatus
    ELSE ?.

IF reload EQ ? THEN DO:
  IF version NE '{&version}' THEN DO:
    MESSAGE 'Schedule Board ({&version}) {&Board} has No Current Jobs' SKIP
            'Unable to Proceed, Contact SB Administrator'
      VIEW-AS ALERT-BOX ERROR TITLE 'Invalid Version'.
    RETURN.
  END. /* invalid version */
  reload = NO.
  MESSAGE 'Re-Load ALL Current Jobs' SKIP
          '(NO Uses Last Saved Schedule Board Jobs)'
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE 'Reload Jobs {&sbExternal}'
    UPDATE reload.
  IF reload EQ ? THEN RETURN.
END.

IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ FALSE THEN
DISPLAY SKIP(1) 'Getting Schedule Board Jobs' SKIP(1)
  WITH FRAME fMsg1 OVERLAY CENTERED ROW 10 BGCOLOR 14 FONT 6
  TITLE ' Schedule Board ({&sbExternal})'.

IF reload EQ NO THEN
RUN VALUE(findProgram('{&loads}/',ID,'/loadView.p')) (containerHandle).
ELSE DO:
  {{&includes}/loadProgram.i} /* runs load{&Board}.p program */
END. /* else do */

IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ FALSE THEN DO: 
    HIDE FRAME fMsg1 NO-PAUSE.
    DISPLAY SKIP(1) 'Loading Schedule Board Jobs' SKIP(1)
      WITH FRAME fMsg2 OVERLAY CENTERED ROW 10 BGCOLOR 14 FONT 6
      TITLE ' Schedule Board ({&sbExternal})'.
END.      

PROCESS EVENTS.

RUN getScenario.
HIDE FRAME fMsg2 NO-PAUSE.

&IF '{&sbExternal}' EQ 'sbHTML' &THEN
RUN {&objects}/sbHTML.p.
&ELSEIF '{&sbExternal}' EQ 'sbReport' &THEN
IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO: 
    RUN {&prompts}/fieldFilter.w PERSISTENT SET hSBHandle ('{&Board}','','',NO,NO,?,'print').
/*    RUN dispatch IN hSBHandle ('Initialize').*/
END.
ELSE 
RUN {&prompts}/fieldFilter.w ('{&Board}','','',NO,NO,?,'print').
&ELSEIF '{&sbExternal}' EQ 'sbStatus' &THEN
IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO: 
    RUN {&objects}/sbStatus.w PERSISTENT SET hSBHandle.
    RUN dispatch IN hSBHandle ('Initialize').
END.
ELSE 
RUN {&objects}/sbStatus.w.
&ENDIF

/* *** internal procedures ********************************************* */
PROCEDURE asiCommaList:
  DEFINE INPUT PARAMETER ipValue AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opValue AS CHARACTER NO-UNDO.

  opValue = g_company.
END PROCEDURE.

PROCEDURE getScenario:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  {{&includes}/getPending.i}
  {{&includes}/getScenario.i}
END PROCEDURE.

PROCEDURE getConfiguration:
  {{&includes}/{&Board}/getConfiguration.i}
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  IMPORT version.
  INPUT CLOSE.
  RUN VALUE('get' + version).
END PROCEDURE.
