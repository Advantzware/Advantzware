/*------------------------------------------------------------------------
  File:         r-mcheff.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 9.20.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable tt-srt
{pcrep/tt-srt.i}

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 145
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE locode AS CHARACTER NO-UNDO.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE v-date        AS DATE      NO-UNDO EXTENT 2.
    DEFINE VARIABLE v-time        AS INTEGER   NO-UNDO EXTENT 2.
    DEFINE VARIABLE v-dept        AS CHARACTER NO-UNDO EXTENT 2.
    DEFINE VARIABLE v-mach        AS CHARACTER NO-UNDO EXTENT 2.
    DEFINE VARIABLE v-shift       AS INTEGER   NO-UNDO EXTENT 2.
    DEFINE VARIABLE v-show        AS LOGICAL   NO-UNDO INITIAL YES.
    DEFINE VARIABLE mr-eff        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE run-eff       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE tot-eff       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dt-eff        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE tot-std-hrs   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE tot-act-hrs   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE hDynCalcField AS HANDLE    NO-UNDO.

    DEFINE BUFFER b-mch-act FOR mch-act.

    RUN AOA/spDynCalcField.p PERSISTENT SET hDynCalcField.
    SESSION:ADD-SUPER-PROCEDURE (hDynCalcField).

    ASSIGN
        cocode     = cCompany
        locode     = cLocation
        v-dept[1]  = cStartDeptNo
        v-dept[2]  = cEndDeptNo
        v-mach[1]  = cStartMachine
        v-mach[2]  = cEndMachine
        v-shift[1] = INTEGER(cStartShift)
        v-shift[2] = IF cEndShift EQ CHR(254) THEN 99 ELSE INTEGER(cEndShift)
        v-date[1]  = dtStartTransDate
        v-date[2]  = dtEndTransDate
        v-time[1]  = DYNAMIC-FUNCTION("fCalcTime", cStartTime)
        v-time[2]  = DYNAMIC-FUNCTION("fCalcTime", cEndTime)
        .
    {pcrep/r-mcheff.i}
    DELETE PROCEDURE hDynCalcField.

END PROCEDURE.

PROCEDURE pro-rate-mr :
    DEFINE BUFFER b-mch-act FOR mch-act.
    DEFINE BUFFER b-job-cod FOR job-code.

    FOR EACH b-mch-act NO-LOCK
        WHERE b-mch-act.company  EQ mch-act.company
          AND b-mch-act.job      EQ mch-act.job
          AND b-mch-act.job-no   EQ mch-act.job-no
          AND b-mch-act.job-no2  EQ mch-act.job-no2
          AND b-mch-act.m-code   EQ mch-act.m-code
          AND b-mch-act.dept     EQ mch-act.dept
          AND b-mch-act.pass     EQ mch-act.pass
          AND b-mch-act.frm      EQ mch-act.frm
          AND b-mch-act.blank-no EQ mch-act.blank-no,
        FIRST b-job-cod NO-LOCK
        WHERE b-job-cod.code EQ b-mch-act.code
        :
        IF b-job-cod.cat EQ "RUN" THEN
        tt-srt.tot-run-hours = tt-srt.tot-run-hours + b-mch-act.hours.
        ELSE
        IF b-job-cod.cat EQ "MR" THEN
        tt-srt.tot-mr-hours  = tt-srt.tot-mr-hours  + b-mch-act.hours.
    END. /* each b-mch-act */

END PROCEDURE.
