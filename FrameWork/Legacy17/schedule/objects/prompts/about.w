&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: about.w

  Description: About (Support Contact Information)

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 6.12.2004
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipBoard           AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipID              AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipContainerHandle AS HANDLE    NO-UNDO.
&ELSE
DEFINE VARIABLE ipBoard           AS CHARACTER NO-UNDO INITIAL 'Pro'.
DEFINE VARIABLE ipID              AS CHARACTER NO-UNDO INITIAL 'ASI/Corrugated'.
DEFINE VARIABLE ipContainerHandle AS HANDLE    NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

{schedule/scopDir.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 aboutBox btnClearCheckoffs ~
deleteStatusCheckoffs btnClearNotes deleteJobNotes btnReturnToPending ~
btnFromPending btnFromPendingByDueDate btnSave btnRestore btnExit 
&Scoped-Define DISPLAYED-OBJECTS aboutBox deleteStatusCheckoffs ~
deleteJobNotes version 

/* Custom List Definitions                                              */
/* adminFunction,List-2,List-3,List-4,List-5,List-6                     */
&Scoped-define adminFunction btnClearCheckoffs btnClearNotes ~
btnReturnToPending btnFromPending btnFromPendingByDueDate btnSave ~
btnRestore btnExit 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDeleteRecord Dialog-Frame 
FUNCTION fDeleteRecord RETURNS LOGICAL
  (ipcCompany AS CHARACTER, ipcCode AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClearCheckoffs 
     IMAGE-UP FILE "schedule/images/save.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Clear Status Checkoffs".

DEFINE BUTTON btnClearNotes 
     IMAGE-UP FILE "schedule/images/notetack.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Clear Notes".

DEFINE BUTTON btnExit AUTO-GO 
     IMAGE-UP FILE "schedule/images/exit1.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Exit".

DEFINE BUTTON btnFromPending 
     IMAGE-UP FILE "schedule/images/pendingjobs.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Schedule From Pending".

DEFINE BUTTON btnFromPendingByDueDate 
     IMAGE-UP FILE "schedule/images/date.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Schedule From Pending By Due Date".

DEFINE BUTTON btnRestore 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Restore Configuration Data Files".

DEFINE BUTTON btnReturnToPending 
     IMAGE-UP FILE "schedule/images/pending.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Return To Pending".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Save Configuration Data Files".

DEFINE VARIABLE aboutBox AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 87 BY 15.24
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE version AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 42 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE deleteJobNotes AS CHARACTER INITIAL "Completed" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Delete ALL", "ALL",
"Delete Run Completed Only", "COMPLETED"
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE deleteStatusCheckoffs AS CHARACTER INITIAL "Completed" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Delete ALL", "ALL",
"Delete Run Completed Only", "COMPLETED"
     SIZE 48 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 87 BY 8.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     aboutBox AT ROW 1.95 COL 2 NO-LABEL
     btnClearCheckoffs AT ROW 17.67 COL 3 HELP
          "Clear Status Checkoffs" WIDGET-ID 2
     deleteStatusCheckoffs AT ROW 17.67 COL 27 NO-LABEL WIDGET-ID 12
     btnClearNotes AT ROW 18.86 COL 3 HELP
          "Clear Notes" WIDGET-ID 4
     deleteJobNotes AT ROW 18.86 COL 27 NO-LABEL WIDGET-ID 16
     btnReturnToPending AT ROW 20.05 COL 3 HELP
          "Return To Pending" WIDGET-ID 6
     btnFromPending AT ROW 21.24 COL 3 HELP
          "Schedule From Pending" WIDGET-ID 8
     btnFromPendingByDueDate AT ROW 22.43 COL 3 HELP
          "Schedule From Pending By Due Date" WIDGET-ID 10
     btnSave AT ROW 23.62 COL 3 HELP
          "Save Configuration Data Files"
     btnRestore AT ROW 24.81 COL 3 HELP
          "Restore Configuration Data Files"
     btnExit AT ROW 24.81 COL 83 HELP
          "Exit About" WIDGET-ID 34
     version AT ROW 1.24 COL 28 NO-LABEL
     "Job Notes:" VIEW-AS TEXT
          SIZE 11 BY 1 AT ROW 18.86 COL 16 WIDGET-ID 32
     "Status Checkoffs:" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 17.67 COL 9 WIDGET-ID 30
     "Restore Custom Configuration .DAT Files" VIEW-AS TEXT
          SIZE 50 BY 1 AT ROW 24.81 COL 9 WIDGET-ID 28
     "Create Backup's of Custom Configuration .DAT Files" VIEW-AS TEXT
          SIZE 50 BY 1 AT ROW 23.62 COL 9 WIDGET-ID 26
     "Move ALL Pending Jobs to the Board by Due Date" VIEW-AS TEXT
          SIZE 50 BY 1 AT ROW 22.43 COL 9 WIDGET-ID 24
     "Move ALL Pending Jobs to the Board" VIEW-AS TEXT
          SIZE 37 BY 1 AT ROW 21.24 COL 9 WIDGET-ID 22
     "Return ALL Jobs to Pending" VIEW-AS TEXT
          SIZE 29 BY 1 AT ROW 20.05 COL 9 WIDGET-ID 20
     RECT-4 AT ROW 17.43 COL 2
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "About (Support Contact Information)".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       aboutBox:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR BUTTON btnClearCheckoffs IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR BUTTON btnClearNotes IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR BUTTON btnExit IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR BUTTON btnFromPending IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR BUTTON btnFromPendingByDueDate IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR BUTTON btnRestore IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR BUTTON btnReturnToPending IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR BUTTON btnSave IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN version IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* About (Support Contact Information) */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearCheckoffs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearCheckoffs Dialog-Frame
ON CHOOSE OF btnClearCheckoffs IN FRAME Dialog-Frame
DO:
  RUN pClearCheckOffs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearNotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearNotes Dialog-Frame
ON CHOOSE OF btnClearNotes IN FRAME Dialog-Frame
DO:
  RUN pClearNotes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFromPending
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFromPending Dialog-Frame
ON CHOOSE OF btnFromPending IN FRAME Dialog-Frame
DO:
  RUN pFromPending.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFromPendingByDueDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFromPendingByDueDate Dialog-Frame
ON CHOOSE OF btnFromPendingByDueDate IN FRAME Dialog-Frame
DO:
  RUN pFromPendingByDueDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRestore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestore Dialog-Frame
ON CHOOSE OF btnRestore IN FRAME Dialog-Frame
DO:
  RUN pRestoreDatFiles.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReturnToPending
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReturnToPending Dialog-Frame
ON CHOOSE OF btnReturnToPending IN FRAME Dialog-Frame
DO:
  RUN pReturnToPending.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame
DO:
  RUN pSaveDatFiles.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME deleteJobNotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL deleteJobNotes Dialog-Frame
ON VALUE-CHANGED OF deleteJobNotes IN FRAME Dialog-Frame
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME deleteStatusCheckoffs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL deleteStatusCheckoffs Dialog-Frame
ON VALUE-CHANGED OF deleteStatusCheckoffs IN FRAME Dialog-Frame
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  version:SCREEN-VALUE = 'Scheduler Release ({&version} ' + ipBoard + ')'.
  aboutBox:READ-FILE('{&startDir}/about.txt').
  IF ipBoard NE 'Pro' THEN
  DISABLE {&adminFunction} WITH FRAME {&FRAME-NAME}.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY aboutBox deleteStatusCheckoffs deleteJobNotes version 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-4 aboutBox btnClearCheckoffs deleteStatusCheckoffs btnClearNotes 
         deleteJobNotes btnReturnToPending btnFromPending 
         btnFromPendingByDueDate btnSave btnRestore btnExit 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClearCheckoffs Dialog-Frame 
PROCEDURE pClearCheckoffs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    MESSAGE
        "WARNING: This process will DELETE" SKIP
        deleteStatusCheckoffs "Schedule Board Status Checkoffs." SKIP
        "Continue?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue AS LOGICAL.
    IF NOT lContinue THEN RETURN.
    SESSION:SET-WAIT-STATE("General").
    DISABLE TRIGGERS FOR LOAD OF reftable.
    DISABLE TRIGGERS FOR LOAD OF sbStatus.
    FOR EACH reftable EXCLUSIVE-LOCK
        WHERE reftable.reftable BEGINS 'SB: Status'
        :
        IF deleteStatusCheckoffs EQ "ALL" OR
           fDeleteRecord(reftable.company,reftable.code) THEN
        DELETE reftable.
    END. /* each reftable */
    FOR EACH sbStatus EXCLUSIVE-LOCK
        :
        IF deleteStatusCheckoffs EQ "ALL" OR
           fDeleteRecord(sbStatus.company,
                         sbStatus.m-code + ",,"
                       + sbStatus.job-no + ","
                       + STRING(sbStatus.job-no2) + ","
                       + STRING(sbStatus.frm)) THEN
        DELETE sbStatus.
    END. /* each sbStatus */
    RUN pReload IN ipContainerHandle.
    SESSION:SET-WAIT-STATE("").
    MESSAGE "Status Checkoffs Cleared" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClearNotes Dialog-Frame 
PROCEDURE pClearNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    MESSAGE
        "WARNING: This process will DELETE" SKIP
        deleteJobNotes "Schedule Board Job Notes." SKIP
        "Continue?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue AS LOGICAL.
    IF NOT lContinue THEN RETURN.
    SESSION:SET-WAIT-STATE("General").
    DISABLE TRIGGERS FOR LOAD OF reftable.
    DISABLE TRIGGERS FOR LOAD OF sbNote.
    FOR EACH reftable EXCLUSIVE-LOCK
        WHERE reftable.reftable BEGINS 'SB: Note'
        :
        IF deleteJobNotes EQ "ALL" OR
           fDeleteRecord(reftable.company,reftable.code) THEN
        DELETE reftable.
    END. /* each reftable */
    FOR EACH sbNote EXCLUSIVE-LOCK
        :
        IF deleteJobNotes EQ "ALL" OR
           fDeleteRecord(sbNote.company,
                         sbNote.m-code + ",,"
                       + sbNote.job-no + ","
                       + STRING(sbNote.job-no2) + ","
                       + STRING(sbNote.frm)) THEN
        DELETE sbNote.
    END. /* each sbNote */
    RUN pReload IN ipContainerHandle.
    SESSION:SET-WAIT-STATE("").
    MESSAGE "Job Notes Cleared" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFromPending Dialog-Frame 
PROCEDURE pFromPending :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    MESSAGE
        "WARNING: This process will move all" SKIP
        "currently Pending Jobs to the Board." SKIP
        "Continue?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue AS LOGICAL.
    IF NOT lContinue THEN RETURN.
    SESSION:SET-WAIT-STATE("General").
    IF VALID-HANDLE(ipContainerHandle) THEN
    RUN pFromPending IN ipContainerHandle.
    SESSION:SET-WAIT-STATE("").
    MESSAGE "All Jobs Scheduled from Pending" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFromPendingByDueDate Dialog-Frame 
PROCEDURE pFromPendingByDueDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    MESSAGE
        "WARNING: This process will move all" SKIP
        "currently Pending Jobs to the Board" SKIP
        "by Due Date.  Continue?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue AS LOGICAL.
    IF NOT lContinue THEN RETURN.
    SESSION:SET-WAIT-STATE("General").
    IF VALID-HANDLE(ipContainerHandle) THEN
    RUN pFromPendingByDueDate IN ipContainerHandle.
    SESSION:SET-WAIT-STATE("").
    MESSAGE "All Jobs Scheduled from Pending" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRestoreDatFiles Dialog-Frame 
PROCEDURE pRestoreDatFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE backupDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE sbDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE datFile AS CHARACTER FORMAT 'X(30)' NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER FORMAT 'X(4)' NO-UNDO.

  MESSAGE 'Restore Schedule Board ".dat" Files from' SKIP
    'directory "{&backup}"?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO UPDATE restore AS LOGICAL.
  IF NOT restore THEN RETURN.
  ASSIGN
    backupDir = '{&backup}\data'
    sbDir = clientDat + '{&data}'.
  OS-COPY VALUE(backupDir + '\gridColor.dat') VALUE(sbDir).
  INPUT FROM OS-DIR(backupDir) NO-ECHO.
  REPEAT:
    IMPORT datFile ^ attrList.
    IF attrList NE 'f' OR NOT datFile BEGINS 'rpt' THEN NEXT.
    OS-COPY VALUE(backupDir + '\' + datFile) VALUE(sbDir).
  END.
  INPUT CLOSE.
  DO i = 1 TO NUM-ENTRIES(ipID,'/'):
    ASSIGN
      backupDir = backupDir + '\' + ENTRY(i,ipID,'/')
      sbDir = sbDir + '\' + ENTRY(i,ipID,'/').
    INPUT FROM OS-DIR(backupDir) NO-ECHO.
    REPEAT:
      IMPORT datFile ^ attrList.
      IF attrList NE 'f' THEN NEXT.
      OS-COPY VALUE(backupDir + '\' + datFile) VALUE(sbDir).
    END.
    INPUT CLOSE.
  END. /* do i */
  MESSAGE 'Schedule Board ".dat" Files Successfully' SKIP
    'Restored from directory "{&backup}"' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReturnToPending Dialog-Frame 
PROCEDURE pReturnToPending :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE asiCompany   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE beginEstType AS INTEGER   NO-UNDO.
    DEFINE VARIABLE endEstType   AS INTEGER   NO-UNDO.

    MESSAGE
        "WARNING: This process will return all" SKIP
        "currently scheduled Jobs to Pending." SKIP
        "Continue?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue AS LOGICAL.
    IF NOT lContinue THEN RETURN.
    SESSION:SET-WAIT-STATE("General").
    DISABLE TRIGGERS FOR LOAD OF job-mch.
    IF VALID-HANDLE(ipContainerHandle) THEN
    RUN asiCommaList IN ipContainerHandle ('Company',OUTPUT asiCompany).
    ASSIGN
        beginEstType = IF CAN-DO('ASI/ALL*,ASI/Folding*,{&HOP}',ipID) THEN 0 ELSE 5
        endEstType = IF CAN-DO('ASI/ALL*,ASI/Corrugated*,{&Fleetwood}',ipID) THEN 99 ELSE 4
        .
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ asiCompany
          AND job-hdr.opened EQ YES
       ,EACH job OF job-hdr NO-LOCK
       ,FIRST est OF job NO-LOCK
        WHERE est.est-type GE beginEstType
          AND est.est-type LE endEstType
        BREAK BY est.est-type
              BY job-hdr.job-no
              BY job-hdr.job-no2
              BY job-hdr.frm
              BY job-hdr.blank-no
        :
        FOR EACH job-mch EXCLUSIVE-LOCK
            WHERE job-mch.company EQ job.company
              AND job-mch.job EQ job.job
              AND job-mch.job-no EQ job.job-no
              AND job-mch.job-no2 EQ job.job-no2
              AND job-mch.run-complete EQ NO
            BREAK BY job-mch.job
                  BY job-mch.frm
                  BY job-mch.blank-no
                  BY job-mch.line
            :
            ASSIGN
                job-mch.end-date      = ?
                job-mch.end-date-su   = ?
                job-mch.end-time      = 0
                job-mch.end-time-su   = 0
                job-mch.start-date    = ?
                job-mch.start-date-su = ?
                job-mch.start-time    = 0
                job-mch.start-time-su = 0
                job-mch.anchored      = NO
                .
        END. /* each job-mch */
    END. /* each job-hdr */
    RELEASE job-mch.
    RUN pReload IN ipContainerHandle.
    SESSION:SET-WAIT-STATE("").
    MESSAGE "All Jobs Returned to Pending" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveDatFiles Dialog-Frame 
PROCEDURE pSaveDatFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE backupDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE sbDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE datFile AS CHARACTER FORMAT 'X(30)' NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER FORMAT 'X(4)' NO-UNDO.

  MESSAGE 'Save Schedule Board ".dat" Files to' SKIP
    'directory "{&backup}"?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO UPDATE backup AS LOGICAL.
  IF NOT backup THEN RETURN.
  OS-CREATE-DIR {&backup}.
  ASSIGN
    backupDir = '{&backup}\data'
    sbDir = clientDat + '{&data}'.
  OS-CREATE-DIR VALUE(backupDir).
  OS-COPY VALUE(sbDir + '\gridColor.dat') VALUE(backupDir).
  INPUT FROM OS-DIR(sbDir) NO-ECHO.
  REPEAT:
    IMPORT datFile ^ attrList.
    IF attrList NE 'f' OR NOT datFile BEGINS 'rpt' THEN NEXT.
    OS-COPY VALUE(sbDir + '\' + datFile) VALUE(backupDir).
  END.
  INPUT CLOSE.
  DO i = 1 TO NUM-ENTRIES(ipID,'/'):
    ASSIGN
      backupDir = backupDir + '\' + ENTRY(i,ipID,'/')
      sbDir = sbDir + '\' + ENTRY(i,ipID,'/').
    OS-CREATE-DIR VALUE(backupDir).
    INPUT FROM OS-DIR(sbDir) NO-ECHO.
    REPEAT:
      IMPORT datFile ^ attrList.
      IF attrList NE 'f' THEN NEXT.
      OS-COPY VALUE(sbDir + '\' + datFile) VALUE(backupDir).
    END.
    INPUT CLOSE.
  END. /* do i */
  MESSAGE 'Schedule Board ".dat" Files Successfully' SKIP
    'Saved in directory "{&backup}"' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDeleteRecord Dialog-Frame 
FUNCTION fDeleteRecord RETURNS LOGICAL
  (ipcCompany AS CHARACTER, ipcCode AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lDelete AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMCode  AS CHARACTER NO-UNDO.
/*    DEFINE VARIABLE iJob    AS INTEGER   NO-UNDO.*/
    DEFINE VARIABLE cJobNo  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFrm    AS INTEGER   NO-UNDO.

    ASSIGN
        cMCode  = ENTRY(1,ipcCode)
/*        iJob    = INTEGER(ENTRY(2,ipcCode))*/
        cJobNo  = ENTRY(3,ipcCode)
        iJobNo2 = INTEGER(ENTRY(4,ipcCode))
        iFrm    = INTEGER(ENTRY(5,ipcCode))
        lDelete = CAN-FIND(FIRST job-mch
                           WHERE job-mch.company EQ ipcCompany
                             AND job-mch.m-code  EQ cMCode
/*                             AND job-mch.job     EQ iJob*/
                             AND job-mch.job-no  EQ cJobNo
                             AND job-mch.job-no2 EQ iJobNo2
                             AND job-mch.frm     EQ iFrm
                             AND job-mch.run-complete EQ YES)
        .
    RETURN lDelete.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

