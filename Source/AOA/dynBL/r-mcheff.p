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

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cocode        AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE locode        AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE v-date        AS DATE          NO-UNDO EXTENT 2.
    DEFINE VARIABLE v-time        AS INTEGER       NO-UNDO EXTENT 2.
    DEFINE VARIABLE v-dept        AS CHARACTER     NO-UNDO EXTENT 2.
    DEFINE VARIABLE v-mach        AS CHARACTER     NO-UNDO EXTENT 2.
    DEFINE VARIABLE v-shift     LIKE mch-act.shift NO-UNDO EXTENT 2.
    DEFINE VARIABLE v-show        AS LOGICAL       NO-UNDO INITIAL YES.
    DEFINE VARIABLE mr-eff        AS DECIMAL       NO-UNDO.
    DEFINE VARIABLE run-eff       AS DECIMAL       NO-UNDO.
    DEFINE VARIABLE tot-eff       AS DECIMAL       NO-UNDO.
    DEFINE VARIABLE dt-eff        AS DECIMAL       NO-UNDO.
    DEFINE VARIABLE tot-std-hrs   AS DECIMAL       NO-UNDO.
    DEFINE VARIABLE tot-act-hrs   AS DECIMAL       NO-UNDO.
    DEFINE VARIABLE hDynCalcField AS HANDLE        NO-UNDO.
    
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
        v-shift[2] = INTEGER(cEndShift)
        v-date[1]  = dtStartTransDate
        v-date[2]  = dtEndTransDate
        v-time[1]  = DYNAMIC-FUNCTION("fCalcTime", cStartTime)
        v-time[2]  = DYNAMIC-FUNCTION("fCalcTime", cEndTime)
        .
    {pcrep/r-mcheff.i}
    DELETE PROCEDURE hDynCalcField.

END PROCEDURE.
