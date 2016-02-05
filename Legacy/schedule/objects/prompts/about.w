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
DEFINE INPUT PARAMETER ipBoard AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipID AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipBoard AS CHARACTER NO-UNDO INITIAL 'Pro'.
DEFINE VARIABLE ipID AS CHARACTER NO-UNDO INITIAL 'ASI/Corrugated'.
&ENDIF

/* Local Variable Definitions ---                                       */

{schedule/scopDir.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 aboutBox btnSave btnRestore 
&Scoped-Define DISPLAYED-OBJECTS aboutBox version 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnRestore 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Restore Configuration Data Files".

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
     SIZE 87 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     aboutBox AT ROW 1.24 COL 2 NO-LABEL
     btnSave AT ROW 16.76 COL 79 HELP
          "Save Configuration Data Files"
     btnRestore AT ROW 16.76 COL 84 HELP
          "Restore Configuration Data Files"
     version AT ROW 16.95 COL 32 NO-LABEL
     RECT-4 AT ROW 16.71 COL 1
     SPACE(1.00) SKIP(0.00)
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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       aboutBox:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

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


&Scoped-define SELF-NAME btnRestore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestore Dialog-Frame
ON CHOOSE OF btnRestore IN FRAME Dialog-Frame
DO:
  RUN restoreDatFiles.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame
DO:
  RUN saveDatFiles.
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
  ENABLE RECT-4 aboutBox btnSave btnRestore 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restoreDatFiles Dialog-Frame 
PROCEDURE restoreDatFiles :
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
    sbDir = installDir + '{&data}'.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveDatFiles Dialog-Frame 
PROCEDURE saveDatFiles :
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
    sbDir = installDir + '{&data}'.
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

