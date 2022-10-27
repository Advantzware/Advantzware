/*------------------------------------------------------------------------
  File:         sbJobs.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 10.25.2022
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttblJob
{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i NEW}
{{&includes}/filterVars.i NEW}
{{&includes}/ttblJob.i NEW}
&SCOPED-DEFINE useTable ttblJob
{{&includes}/jobStatusFunc.i}

DEFINE VARIABLE scenario AS CHARACTER NO-UNDO INITIAL 'Actual'.

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 216
{AOA/includes/subjectID{&subjectID}Defs.i}

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

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE containerHandle AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iHTMLPage       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE loadProgram     AS CHARACTER NO-UNDO.

    IF cSBID EQ ? THEN RETURN.
    IF cEndJobNo EQ CHR(254) THEN
    cEndJobNo = "zzzzzz".
    ASSIGN
        cStartJobNo     = FILL(" ",6 - LENGTH(cStartJobNo)) + cStartJobNo + "-" + STRING(iStartJobNo2)
        cEndJobNo       = FILL(" ",6 - LENGTH(cEndJobNo))   + cEndJobNo   + "-" + STRING(iEndJobNo2)
        containerHandle = THIS-PROCEDURE:HANDLE
        ID              = cSBID
        .
    RUN getConfiguration.
    IF lReload EQ NO THEN
    RUN VALUE(findProgram('{&loads}/',ID,'/loadView.p')) (containerHandle).
    ELSE DO:
      {{&includes}/loadProgram.i} /* runs load{&Board}.p program */
    END. /* else do */
    RUN getScenario.
    FOR EACH ttblJob
        :
        IF ttblJob.resource  LT cStartMachine  OR
           ttblJob.resource  GT cEndMachine    OR
           ttblJob.jobSort   LT cStartJobNo    OR
           ttblJob.jobSort   GT cEndJobNo      OR
           ttblJob.startDate LT dtStartDate    OR
           ttblJob.startDate GT dtEndDate      OR
           ttblJob.dueDate   LT dtStartDueDate OR
           ttblJob.dueDate   GT dtEndDueDate THEN DO:
            DELETE ttblJob.
            NEXT.
        END.
        IF cDepartments NE ? THEN DO:
            FIND FIRST job-mch NO-LOCK
                  WHERE ROWID(job-mch) EQ TO-ROWID(ENTRY(2,ttblJob.rowIDs))
                  NO-ERROR.
            IF NOT AVAILABLE job-mch THEN DO:
                DELETE ttblJob.
                NEXT.
            END.
            FIND FIRST mach NO-LOCK
                 WHERE mach.company EQ job-mch.company
                   AND mach.m-code  EQ job-mch.m-code
                   AND CAN-DO(cDepartments,mach.dept[1])
                 NO-ERROR.
            IF NOT AVAILABLE mach THEN DO:
                DELETE ttblJob.
                NEXT.
            END.
        END. // if cdepartments ne ?
    END. // each ttbljob

END PROCEDURE.

PROCEDURE buildBoard:
  DEFINE INPUT PARAMETER iplLogical AS LOGICAL NO-UNDO.
  
END PROCEDURE.

PROCEDURE getConfiguration:
  {{&includes}/{&Board}/getConfiguration.i}
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  IMPORT version.
  INPUT CLOSE.
  RUN VALUE('get' + version).
END PROCEDURE.

PROCEDURE getScenario:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  {{&includes}/getScenario.i}
END PROCEDURE.
