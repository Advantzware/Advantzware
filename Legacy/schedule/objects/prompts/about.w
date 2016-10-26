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
btnClearNotes btnReturnToPending btnSave btnRestore 
&Scoped-Define DISPLAYED-OBJECTS aboutBox version 

/* Custom List Definitions                                              */
/* adminFunction,List-2,List-3,List-4,List-5,List-6                     */
&Scoped-define adminFunction btnClearCheckoffs btnClearNotes ~
btnReturnToPending btnSave btnRestore 

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     aboutBox AT ROW 1.24 COL 2 NO-LABEL
     btnClearCheckoffs AT ROW 16.76 COL 2 HELP
          "Clear Status Checkoffs" WIDGET-ID 2
     btnClearNotes AT ROW 16.76 COL 7 HELP
          "Clear Notes" WIDGET-ID 4
     btnReturnToPending AT ROW 16.76 COL 12 HELP
          "Return To Pending" WIDGET-ID 6
     btnSave AT ROW 16.76 COL 79 HELP
          "Save Configuration Data Files"
     btnRestore AT ROW 16.76 COL 84 HELP
          "Restore Configuration Data Files"
     version AT ROW 16.95 COL 27 NO-LABEL
     RECT-4 AT ROW 16.71 COL 1
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  version:SCREEN-VALUE = 'Scheduler ({&version} ' + ipBoard + ')'.
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
  DISPLAY aboutBox version 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-4 aboutBox btnClearCheckoffs btnClearNotes btnReturnToPending 
         btnSave btnRestore 
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
        "ALL Schedule Board Status Checkoffs." SKIP
        "Continue?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue AS LOGICAL.
    IF NOT lContinue THEN RETURN.
    SESSION:SET-WAIT-STATE("General").
    DISABLE TRIGGERS FOR LOAD OF reftable.
    FOR EACH reftable EXCLUSIVE-LOCK
        WHERE reftable.reftable BEGINS 'SB: Status'
        :
        DELETE reftable.
    END. /* each reftable */
    SESSION:SET-WAIT-STATE("").
    MESSAGE
        "Status Checkoffs Cleared" SKIP
        "Perform a RELOAD without SAVING"
        VIEW-AS ALERT-BOX.

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
        "ALL Schedule Board Job Notes." SKIP
        "Continue?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue AS LOGICAL.
    IF NOT lContinue THEN RETURN.
    SESSION:SET-WAIT-STATE("General").
    DISABLE TRIGGERS FOR LOAD OF reftable.
    FOR EACH reftable EXCLUSIVE-LOCK
        WHERE reftable.reftable BEGINS 'SB: Note'
        :
        DELETE reftable.
    END. /* each reftable */
    SESSION:SET-WAIT-STATE("").
    MESSAGE
        "Job Notes Cleared" SKIP
        "Perform a RELOAD without SAVING"
         VIEW-AS ALERT-BOX.

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
    SESSION:SET-WAIT-STATE("").
    MESSAGE
        "All Jobs Returned to Pending" SKIP
        "Perform a RELOAD without SAVING"
         VIEW-AS ALERT-BOX.

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

