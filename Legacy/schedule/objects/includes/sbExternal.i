/* sbExternal.i - used in sbDMI.p, sbReport.p, sbStatus.p and sbNotes.p */

{{&includes}/defBoard.i}
{{&includes}/sharedVars.i NEW}
{{&includes}/filterVars.i NEW}
{{&includes}/ttblJob.i NEW}
&SCOPED-DEFINE useTable ttblJob
{{&includes}/jobStatusFunc.i}

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

DEFINE VARIABLE containerHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE loadProgram AS CHARACTER NO-UNDO.
DEFINE VARIABLE reload AS LOGICAL NO-UNDO.
DEFINE VARIABLE scenario AS CHARACTER NO-UNDO INITIAL 'Actual'.
DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.
DEFINE VARIABLE cProdAceDat AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx AS INTEGER NO-UNDO.

DEFINE SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.

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
reload = IF '{&sbExternal}' EQ 'sbDMI'    THEN YES
    ELSE IF '{&sbExternal}' EQ 'sbJScan'  THEN reloadStatus
    ELSE IF '{&sbExternal}' EQ 'sbNotes'  THEN reloadStatus
    ELSE IF '{&sbExternal}' EQ 'sbReport' THEN reloadReport
    ELSE IF '{&sbExternal}' EQ 'sbStatus' THEN reloadStatus
    ELSE ?
    .
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

DISPLAY SKIP(1) 'Getting Schedule Board Jobs' SKIP(1)
  WITH FRAME fMsg1 OVERLAY COL 10 ROW 10 BGCOLOR 14 FONT 6
  TITLE ' Schedule Board ({&sbExternal})'.

PROCESS EVENTS.

IF reload EQ NO THEN
RUN VALUE(findProgram('{&loads}/',ID,'/loadView.p')) (containerHandle).
ELSE DO:
  {{&includes}/loadProgram.i} /* runs load{&Board}.p program */
END. /* else do */

HIDE FRAME fMsg1 NO-PAUSE.
DISPLAY SKIP(1) 'Loading Schedule Board Jobs' SKIP(1)
  WITH FRAME fMsg2 OVERLAY COL 10 ROW 10 BGCOLOR 14 FONT 6
  TITLE ' Schedule Board ({&sbExternal})'.

PROCESS EVENTS.

RUN getScenario.
HIDE FRAME fMsg2 NO-PAUSE.

&IF '{&sbExternal}' EQ 'sbDMI' &THEN
FOR EACH mach NO-LOCK 
    WHERE mach.company EQ ENTRY(1,commaList)
      AND mach.spare-int-2 NE 0
    :
    CREATE ttblResource.
    ASSIGN 
        ttblResource.resource = mach.m-code
        ttblResource.resourceDescription = mach.m-dscr
        ttblResource.dmiID = mach.spare-int-2
        .
END. /* each mach */

FOR EACH ttblJob
    BREAK BY ttblJob.resource
          BY ttblJob.dueDate
          BY ttblJob.jobSort
    :
    IF FIRST-OF(ttblJob.resource) THEN
    idx = 0.
    ASSIGN 
        idx = idx + 1
        ttblJob.jobSequence = idx
        .
END. /* each ttbljob */

cProdAceDat = findProgram('{&data}/',ID,'/ProdAce.dat').
RUN VALUE(findProgram('{&loads}/',ID,'/prodAce.w')) (cProdAceDat, THIS-PROCEDURE, NO, OUTPUT lContinue).
&ELSEIF '{&sbExternal}' EQ 'sbJScan' &THEN
RUN {&prompts}/jobSeqScan.w (THIS-PROCEDURE, ENTRY(1,commaList)).
&ELSEIF '{&sbExternal}' EQ 'sbNotes' &THEN
RUN {&objects}/sbNotes.w.
&ELSEIF '{&sbExternal}' EQ 'sbReport' &THEN
RUN {&prompts}/fieldFilter.w ('{&Board}','','',NO,NO,?,'print').
&ELSEIF '{&sbExternal}' EQ 'sbStatus' &THEN
RUN {&objects}/sbStatus.w.
&ENDIF

/* *** internal procedures ********************************************* */
PROCEDURE asiCommaList:
  DEFINE INPUT PARAMETER ipValue AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opValue AS CHARACTER NO-UNDO.
  
  CASE ipValue:
      WHEN "Company" THEN 
      opValue = g_company.
      WHEN "Location" THEN 
      opValue = g_loc.
  END CASE.
END PROCEDURE.

PROCEDURE buildBoard:
  DEFINE INPUT PARAMETER iplLogical AS LOGICAL NO-UNDO.
  
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
