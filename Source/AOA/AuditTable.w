&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: AuditTable.w

  Description: Audit Table Selection

  Input Parameters: Query Handle

  Output Parameters: <none>

  Author: Ron Stark

  Created: 7.9.2019

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT  PARAMETER iphQuery     AS HANDLE    NO-UNDO.
DEFINE INPUT  PARAMETER iphExternal  AS HANDLE    NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableList AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcTable     AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ophTable     AS HANDLE    NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cMsgResponse AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTable       AS CHARACTER NO-UNDO EXTENT 20.
DEFINE VARIABLE hTable       AS HANDLE    NO-UNDO EXTENT 20.
DEFINE VARIABLE iTableCount  AS INTEGER   NO-UNDO.
DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.
DEFINE VARIABLE jdx          AS INTEGER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnExit iAuditTables btnOK 
&Scoped-Define DISPLAYED-OBJECTS iAuditTables 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnExit DEFAULT 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Exit"
     BGCOLOR 8 FONT 4.

DEFINE BUTTON btnOK AUTO-GO DEFAULT 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Save"
     BGCOLOR 8 FONT 4.

DEFINE VARIABLE iAuditTables AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "None", 0
     SIZE 77 BY 7.62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 19 BY 2.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnExit AT ROW 9.57 COL 71 HELP
          "Use this function to CANCEL field selecition" WIDGET-ID 4
     iAuditTables AT ROW 1.48 COL 3 NO-LABEL WIDGET-ID 2
     btnOK AT ROW 9.57 COL 62 HELP
          "Use this function to ACCEPT selected field" WIDGET-ID 6
     RECT-1 AT ROW 9.33 COL 61 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 10.95 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Audit Table Selection"
         HEIGHT             = 10.95
         WIDTH              = 80
         MAX-HEIGHT         = 10.95
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 10.95
         VIRTUAL-WIDTH      = 80
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Audit Table Selection */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Audit Table Selection */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit C-Win
ON CHOOSE OF btnExit IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        opcTable = ""
        ophTable = ?
        .
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        iAuditTables
        opcTable = cTable[iAuditTables]
        ophTable = hTable[iAuditTables]
        .
    IF NOT CAN-FIND(FIRST AuditTbl
                    WHERE AuditTbl.AuditTable   EQ opcTable
                      AND (AuditTbl.AuditCreate EQ YES
                       OR  AuditTbl.AuditDelete EQ YES
                       OR  AuditTbl.AuditUpdate EQ YES)) THEN DO:
        RUN displayMessageQuestion ("17", OUTPUT cMsgResponse).
        IF cMsgResponse EQ "no" THEN DO:
            APPLY "CHOOSE":U TO btnExit.
            RETURN NO-APPLY.
        END. /* if no */
    END. /* if audittble */
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF ipcTableList EQ ? THEN DO:
      IF VALID-HANDLE(iphExternal) THEN
      DO idx = 1 TO iphExternal:NUM-BUFFERS:
        ASSIGN
            jdx         = jdx + 1
            cTable[jdx] = iphExternal:GET-BUFFER-HANDLE(idx):NAME
            hTable[jdx] = iphExternal:GET-BUFFER-HANDLE(idx):HANDLE
            .
      END. /* do idx */
      IF VALID-HANDLE(iphQuery) THEN
      DO idx = 1 TO iphQuery:NUM-BUFFERS:
        ASSIGN
            jdx         = jdx + 1
            cTable[jdx] = iphQuery:GET-BUFFER-HANDLE(idx):NAME
            hTable[jdx] = iphQuery:GET-BUFFER-HANDLE(idx):HANDLE
            .
      END. /* do idx */
  END. /* if ipctablelist */
  ELSE
  DO idx = 1 TO NUM-ENTRIES(ipcTableList):
    ASSIGN
        jdx         = jdx + 1
        cTable[jdx] = ENTRY(idx,ipcTableList)
        .
  END. /* do idx */
  DO idx = 1 TO jdx:
    FIND FIRST ASI._file NO-LOCK
         WHERE ASI._file._file-name EQ cTable[idx]
         NO-ERROR.
    IF AVAILABLE ASI._file THEN DO:
        iAuditTables:ADD-LAST(STRING(idx) + ")  " + cTable[idx] + " - " + ASI._file._Desc,idx).
        iTableCount = iTableCount + 1.
    END. /* if avail */
  END. /* do idx */
  iAuditTables:DELETE("None").
  RUN enable_UI.
  IF iTableCount EQ 1 THEN DO:
      APPLY "CHOOSE":U TO btnOK IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
  END. /* if itablecount */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY iAuditTables 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnExit iAuditTables btnOK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

