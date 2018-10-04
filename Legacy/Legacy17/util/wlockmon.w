&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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

/* Local Variable Definitions ---                                       */
DEF VAR lStart AS LOG NO-UNDO.
DEF VAR hqRecKey AS HANDLE NO-UNDO.
DEF VAR hbRecKey AS HANDLE NO-UNDO.
DEF VAR hbhRecKey1 AS HANDLE NO-UNDO.
DEF VAR hbhRecKey2 AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS slLockList bStart fiInterval bQuit fiTable 
&Scoped-Define DISPLAYED-OBJECTS slLockList fiInterval fiTable 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bQuit 
     LABEL "Leave" 
     SIZE 15 BY 1.1.

DEFINE BUTTON bStart 
     LABEL "Start Monitor" 
     SIZE 15 BY 1.1.

DEFINE BUTTON bStop 
     LABEL "Stop Monitor" 
     SIZE 15 BY 1.1.

DEFINE VARIABLE fiInterval AS INTEGER FORMAT ">>9":U INITIAL 3 
     LABEL "Refresh Interval (secs)" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE fiTable AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
     LABEL "Table (*) for all" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE slLockList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 95 BY 12.38
     FONT 2 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     slLockList AT ROW 2.33 COL 3 NO-LABEL
     bStart AT ROW 15.19 COL 3
     bStop AT ROW 15.19 COL 19
     fiInterval AT ROW 15.19 COL 57 COLON-ALIGNED
     bQuit AT ROW 15.19 COL 67
     fiTable AT ROW 16.81 COL 29 COLON-ALIGNED
     "Table" VIEW-AS TEXT
          SIZE 11 BY .67 AT ROW 1.24 COL 39
     "Rec Key" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 1.24 COL 56
     "Type" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 1.24 COL 87
     "User" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 1.24 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99.2 BY 18.11.


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
         TITLE              = "Record Lock Viewer"
         HEIGHT             = 18.1
         WIDTH              = 99.2
         MAX-HEIGHT         = 18.1
         MAX-WIDTH          = 99.2
         VIRTUAL-HEIGHT     = 18.1
         VIRTUAL-WIDTH      = 99.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bStop IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Record Lock Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Record Lock Viewer */
DO:
  /* This event will close the window and terminate the procedure.  */
    APPLY 'choose' TO bStop IN FRAME default-frame.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
  /* RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bQuit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bQuit C-Win
ON CHOOSE OF bQuit IN FRAME DEFAULT-FRAME /* Leave */
DO:
    APPLY 'choose' TO bStop.
    APPLY 'window-close' TO c-win.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bStart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bStart C-Win
ON CHOOSE OF bStart IN FRAME DEFAULT-FRAME /* Start Monitor */
DO:
    STATUS INPUT ("Monitor is running").
    STATUS DEFAULT  ("Monitor is running").

    ASSIGN 
        lStart = TRUE
        bStop:SENSITIVE = TRUE
        bQuit:SENSITIVE = FALSE
        SELF:SENSITIVE = FALSE
        fiInterval:SENSITIVE = FALSE
        fiTable:SENSITIVE = FALSE. 

    RUN ipMonitor IN THIS-PROCEDURE.

    RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bStop C-Win
ON CHOOSE OF bStop IN FRAME DEFAULT-FRAME /* Stop Monitor */
DO:
    ASSIGN
        lStart = FALSE
        SELF:SENSITIVE = FALSE
        bQuit:SENSITIVE = TRUE
        bStart:SENSITIVE = TRUE
        fiInterval:SENSITIVE = TRUE
        fiTable:SENSITIVE = TRUE. 
        
    STATUS INPUT ("Monitor is not running").
    STATUS DEFAULT ("Monitor is not running").
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
  RUN enable_UI.
  STATUS INPUT ("Monitor is NOT running").
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
  DISPLAY slLockList fiInterval fiTable 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE slLockList bStart fiInterval bQuit fiTable 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipMonitor C-Win 
PROCEDURE ipMonitor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cDisp AS CHAR NO-UNDO.
    DEF VAR a AS INT NO-UNDO.
    runmon:
    REPEAT WHILE lStart:
        PROCESS EVENTS.
        IF NOT lStart THEN LEAVE runmon.
        ASSIGN
            slLockList:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
        ASSIGN
            slLockList:VISIBLE = FALSE
            slLockList:VISIBLE = TRUE.
        
        a = ETIME(YES).
        DO WHILE ETIME < 20:
        END.
        
        ASSIGN 
             fitable = fitable:SCREEN-VALUE 
             fiInterval = int(fiInterval:SCREEN-VALUE) .

        FOR EACH _lock where
            _lock._lock-id < 50 AND 
            _lock._lock-table <> ? NO-LOCK:
            FIND FIRST _file WHERE
                _file._file-number = _lock._lock-table
                USE-INDEX _file-number
                NO-LOCK NO-ERROR.
            FIND FIRST _index OF _file WHERE 
                RECID(_index) = _file._prime-index
                NO-LOCK NO-ERROR.
            CREATE BUFFER hbRecKey FOR TABLE _file._file-name.
            CREATE QUERY hqRecKey.
            hqRecKey:SET-BUFFERS(hbRecKey).
            hqRecKey:QUERY-PREPARE("for each " + _file._file-name + " where recid(" + _file._file-name + 
                                   ") = " + string(_lock._lock-recid)).
            hqRecKey:QUERY-OPEN.
            hqRecKey:GET-FIRST.
            FIND FIRST _index-field OF _index NO-LOCK NO-ERROR.
            IF AVAIL _index-field THEN DO:
                FIND FIRST _field OF _index-field NO-LOCK NO-ERROR.
                IF AVAIL _field THEN ASSIGN
                   hbhRecKey1 = hbRecKey:BUFFER-FIELD(_field._field-physpos).
            END.
            FIND NEXT _index-field OF _index NO-LOCK NO-ERROR.
            IF AVAIL _index-field THEN DO:
                FIND FIRST _field OF _index-field NO-LOCK NO-ERROR.
                IF AVAIL _field THEN ASSIGN
                    hbhRecKey2 = hbRecKey:BUFFER-FIELD(_field._field-physpos).
            END.
            IF AVAIL (_file) THEN ASSIGN
                cDisp = FILL(" ",80)
                SUBSTRING(cDisp,1,24) = _lock._lock-name
                SUBSTRING(cDisp,26,LENGTH(_file._file-name)) = _file._file-name
                SUBSTRING(cDisp,36,22) = (IF VALID-HANDLE(hbhRecKey1) THEN TRIM(hbhRecKey1:STRING-VALUE) ELSE "") + "|" + 
                                         (IF VALID-HANDLE(hbhRecKey2) THEN TRIM(hbhRecKey2:STRING-VALUE) ELSE "")
                SUBSTRING(cDisp,60,4) = IF INDEX(_lock._lock-flags,"X") > 0 THEN "EXCL" ELSE
                                        IF INDEX(_lock._lock-flags,"S") > 0 THEN "SHRD" ELSE
                                        IF INDEX(_lock._lock-flags,"U") > 0 THEN "UPGR" ELSE
                                        IF INDEX(_lock._lock-flags,"L") > 0 THEN "LIMB" ELSE
                                        IF INDEX(_lock._lock-flags,"Q") > 0 THEN "QUED" ELSE
                                        "".
            slLockList:ADD-LAST(cDisp).
            IF VALID-HANDLE(hbhRecKey1) THEN DELETE OBJECT hbhRecKey1.
            IF VALID-HANDLE(hbhRecKey2) THEN DELETE OBJECT hbhRecKey2.
            DELETE OBJECT hqRecKey.
            DELETE OBJECT hbRecKey.
        END.
        WAIT-FOR 'CHOOSE' OF bStop PAUSE INTEGER(fiInterval:SCREEN-VALUE).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

