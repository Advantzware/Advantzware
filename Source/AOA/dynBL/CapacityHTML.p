/*------------------------------------------------------------------------
  File:         CapacityHTML.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 8.15.2020
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

&Scoped-define subjectID 138
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

    ASSIGN
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
    iHTMLPage = INTEGER(cHTMLPage).
    RUN {&objects}/sbHTML.p (
        cCompany,
        iHTMLPage,
        lLaunchHTMLPage,
        lProgressBar,
        cProgressBar
        ).

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
