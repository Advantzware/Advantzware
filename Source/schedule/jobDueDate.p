&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : schedule/jobDueDate.p
    Purpose     : persistent procedures used to calc job-mch due date

    Syntax      : RUN jc/jobDueDate.p PERSISTENT SET scheduleHndl.

    Description : 

    Author(s)   : Ron Stark
    Created     : 8.15.2020
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipcType    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iprRowID   AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.

DEFINE OUTPUT PARAMETER opdtDueDate AS DATE    NO-UNDO.
DEFINE OUTPUT PARAMETER opiDueTime  AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE baseOnText AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrorMsg  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lContinue  AS LOGICAL   NO-UNDO.

{schedule/objects/includes/scheduleTempTables.i}

SESSION:SET-WAIT-STATE ("").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */
/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 

/* ***************************  Main Block  *************************** */

SESSION:SET-WAIT-STATE ("General").
RUN pLoadDowntime.
RUN pBuildTTMachine.
RUN pBuildTTJob (ipcType, ipcCompany, iprRowID).
RUN pRemoveUnusedDowntime.
RUN pScheduleJob (iprRowID).
FOR EACH ttblJob
    WHERE ttblJob.newJob EQ YES
       BY ttblJob.endDateTime DESCENDING
    :
    ASSIGN
        opdtDueDate = ttblJob.endDate
        opiDueTime  = ttblJob.endTime
        .
    MESSAGE
        ttblJob.m-code SKIP 
        "Due Date:" opdtDueDate STRING(opiDueTime,"hh:mm:ss am")
    VIEW-AS ALERT-BOX.
    LEAVE.
END. /* each ttbljob */
SESSION:SET-WAIT-STATE ("").

{schedule/objects/includes/scheduleProcs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* **********************  Internal Procedures  *********************** */

/* ************************  Function Implementations ***************** */
