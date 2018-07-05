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
DEF VAR cName AS CHAR NO-UNDO.

DEF TEMP-TABLE ttLocks
    FIELD ttfLock-Id AS INT64
    FIELD ttfLock-Usr AS INT
    FIELD ttfLock-Name AS CHAR
    FIELD ttfLock-Type AS CHAR
    FIELD ttfLock-Table AS INT
    FIELD ttfLock-RecId AS RECID
    FIELD ttfLock-Tty AS CHAR
    FIELD ttfUserID AS CHAR
    FIELD ttfRecKey AS CHAR
    FIELD ttfTable AS CHAR
    FIELD ttfLockTime AS DATETIME-TZ
    FIELD ttfDuration AS INT
    FIELD ttfDispTime AS CHAR.
    
DEF TEMP-TABLE ttLocks2
    FIELD ttfLock-Id AS INT64
    FIELD ttfLock-Usr AS INT
    FIELD ttfLock-Name AS CHAR
    FIELD ttfLock-Type AS CHAR
    FIELD ttfLock-Table AS INT
    FIELD ttfLock-RecId AS RECID
    FIELD ttfLock-Tty AS CHAR
    FIELD ttfUserID AS CHAR
    FIELD ttfRecKey AS CHAR
    FIELD ttfTable AS CHAR
    FIELD ttfLockTime AS DATETIME-TZ
    FIELD ttfDuration AS INT
    FIELD ttfDispTime AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS slLockList bStart fiInterval bQuit 
&Scoped-Define DISPLAYED-OBJECTS slLockList fiInterval RADIO-SET-1 

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

DEFINE VARIABLE fiInterval AS INTEGER FORMAT ">>9":U INITIAL 30 
     LABEL "Refresh Interval (secs)" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "All Locks", 1,
"Lingering Only", 2
     SIZE 34 BY .86 NO-UNDO.

DEFINE VARIABLE slLockList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 162 BY 14.62
     FONT 2 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     slLockList AT ROW 2.33 COL 3 NO-LABEL
     bStart AT ROW 17.67 COL 4
     bStop AT ROW 17.67 COL 20
     fiInterval AT ROW 17.67 COL 58 COLON-ALIGNED
     bQuit AT ROW 17.67 COL 151
     RADIO-SET-1 AT ROW 17.76 COL 70 NO-LABEL WIDGET-ID 6
     "Phone" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.24 COL 43 WIDGET-ID 12
     "Terminal" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.24 COL 62 WIDGET-ID 10
     "Duration" VIEW-AS TEXT
          SIZE 11 BY .67 AT ROW 1.24 COL 149 WIDGET-ID 4
     "Table" VIEW-AS TEXT
          SIZE 11 BY .67 AT ROW 1.24 COL 76
     "Rec Key" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 1.24 COL 108
     "Lock Type" VIEW-AS TEXT
          SIZE 11 BY .67 AT ROW 1.24 COL 136
     "User ID/Name" VIEW-AS TEXT
          SIZE 16 BY .67 AT ROW 1.24 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 166.8 BY 18.43.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Record Lock Viewer"
         HEIGHT             = 18.62
         WIDTH              = 166.8
         MAX-HEIGHT         = 18.86
         MAX-WIDTH          = 166.8
         VIRTUAL-HEIGHT     = 18.86
         VIRTUAL-WIDTH      = 166.8
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
/* SETTINGS FOR WINDOW c-win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bStop IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RADIO-SET-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(c-win)
THEN c-win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME c-win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-win c-win
ON END-ERROR OF c-win /* Record Lock Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-win c-win
ON WINDOW-CLOSE OF c-win /* Record Lock Viewer */
DO:
  /* This event will close the window and terminate the procedure.  */
    APPLY 'choose' TO bStop IN FRAME default-frame.
    APPLY 'choose' to bQuit IN FRAME default-frame.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
  /* RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bQuit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bQuit c-win
ON CHOOSE OF bQuit IN FRAME DEFAULT-FRAME /* Leave */
DO:
    APPLY 'choose' TO bStop.
    APPLY 'window-close' TO c-win.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bStart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bStart c-win
ON CHOOSE OF bStart IN FRAME DEFAULT-FRAME /* Start Monitor */
DO:
    STATUS INPUT ("Monitor is running").
    STATUS DEFAULT  ("Monitor is running").

    ASSIGN 
        lStart = TRUE
        bStop:SENSITIVE = TRUE
        bQuit:SENSITIVE = FALSE
        SELF:SENSITIVE = FALSE
        fiInterval:SENSITIVE = FALSE. 

    RUN ipMonitor IN THIS-PROCEDURE.

    RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bStop c-win
ON CHOOSE OF bStop IN FRAME DEFAULT-FRAME /* Stop Monitor */
DO:
    ASSIGN
        lStart = FALSE
        SELF:SENSITIVE = FALSE
        bQuit:SENSITIVE = TRUE
        bStart:SENSITIVE = TRUE
        fiInterval:SENSITIVE = TRUE. 
        
    STATUS INPUT ("Monitor is not running").
    STATUS DEFAULT ("Monitor is not running").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiInterval
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiInterval c-win
ON LEAVE OF fiInterval IN FRAME DEFAULT-FRAME /* Refresh Interval (secs) */
DO:
    IF INTEGER(SELF:SCREEN-VALUE) GT 600 THEN DO:
        MESSAGE
            "Maximum interval is 600 seconds (10 minutes). Resetting."
            VIEW-AS ALERT-BOX INFO.
        ASSIGN
            SELF:SCREEN-VALUE = "600".
    END.
    ELSE IF INTEGER(SELF:SCREEN-VALUE) LT 3 THEN DO:
        MESSAGE
            "Minimum interval is 3 seconds. Resetting."
            VIEW-AS ALERT-BOX INFO.
        ASSIGN
            SELF:SCREEN-VALUE = "3".
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK c-win 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI c-win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(c-win)
  THEN DELETE WIDGET c-win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI c-win  _DEFAULT-ENABLE
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
  DISPLAY slLockList fiInterval RADIO-SET-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW c-win.
  ENABLE slLockList bStart fiInterval bQuit 
      WITH FRAME DEFAULT-FRAME IN WINDOW c-win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW c-win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipMonitor c-win 
PROCEDURE ipMonitor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cDisp AS CHAR NO-UNDO.
    DEF VAR a AS INT NO-UNDO.
    DEF VAR deDuration AS DEC NO-UNDO.
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
             fiInterval = int(fiInterval:SCREEN-VALUE) .
        EMPTY TEMP-TABLE ttLocks.
    
        FOR EACH _lock WHERE
            _lock._lock-usr <> ? AND 
            _lock._lock-recid <> ? NO-LOCK:
            FIND FIRST _file WHERE
                _file._file-number = _lock._lock-table
                USE-INDEX _file-number
                NO-LOCK NO-ERROR.
            CREATE ttLocks.
            ASSIGN
                ttLocks.ttfLock-Id = _lock._Lock-ID
                ttLocks.ttfLock-Usr = _lock._Lock-Usr
                ttLocks.ttfLock-Name = _lock._Lock-Name
                ttLocks.ttfLock-Type = _lock._Lock-Type
                ttLocks.ttfLock-Table = _lock._Lock-Table
                ttLocks.ttfLock-RecID = _lock._Lock-RecID
                ttLocks.ttfLock-Tty = _lock._Lock-Tty
                ttLocks.ttfTable = _file._file-name
                ttLocks.ttfLockTime = NOW
                ttLocks.ttfDuration = 0
                ttLocks.ttfDispTime = "00:00:00".
            FIND FIRST ttLocks2 WHERE
                ttLocks2.ttfLock-ID EQ ttLocks.ttfLock-ID
                NO-ERROR.
            IF NOT AVAIL ttLocks2 THEN DO:
                CREATE ttLocks2.
                BUFFER-COPY ttLocks TO ttLocks2.
            END.
            ELSE DO:
                ASSIGN
                    ttLocks2.ttfDuration = (NOW - ttLocks2.ttfLockTime) / 1000
                    ttLocks2.ttfDispTime = STRING(ttLocks2.ttfDuration,"HH:MM:SS").
            END.
        END.
        FOR EACH ttLocks2 WHERE NOT CAN-FIND (FIRST ttLocks WHERE
                                ttLocks.ttfLock-ID = ttLocks2.ttfLock-ID):
            DELETE ttLocks2.
        END.
                
        FOR EACH ttLocks2:
            CREATE BUFFER hbRecKey FOR TABLE ttLocks2.ttfTable.
            CREATE QUERY hqRecKey.
            hqRecKey:SET-BUFFERS(hbRecKey).
            hqRecKey:QUERY-PREPARE("for each " + ttLocks2.ttfTable + " where recid(" + ttLocks2.ttfTable + 
                                   ") = " + string(ttLocks2.ttfLock-RECID)).
            hqRecKey:QUERY-OPEN.
            hqRecKey:GET-FIRST.
            FIND _lock WHERE
                _lock._lock-ID EQ ttLocks2.ttfLock-ID
                NO-LOCK NO-ERROR.
            IF AVAIL _lock THEN DO:
                
                FIND users NO-LOCK WHERE
                    users.user_id EQ _lock._lock-name
                    NO-ERROR.
                IF AVAIL users THEN ASSIGN
                    cName = users.user_id + " - " + users.user_name.
                ELSE ASSIGN
                    cName = _lock._lock-name.
                
                FIND FIRST _file WHERE
                    _file._file-number = _lock._lock-table
                    USE-INDEX _file-number
                    NO-LOCK NO-ERROR.
                IF AVAIL _file THEN FIND FIRST _index OF _file WHERE 
                    RECID(_index) = _file._prime-index
                    NO-LOCK NO-ERROR.
                
                IF AVAIL _index THEN DO:
                    FIND FIRST _index-field OF _index NO-LOCK NO-ERROR.
                    IF AVAIL _index-field THEN DO:
                        FIND FIRST _field OF _index-field NO-LOCK NO-ERROR.
                        IF AVAIL _field THEN ASSIGN
                           hbhRecKey1 = hbRecKey:BUFFER-FIELD(_field._field-physpos) NO-ERROR.
                        FIND NEXT _index-field OF _index NO-LOCK NO-ERROR.
                        IF AVAIL _index-field THEN DO:
                            FIND FIRST _field OF _index-field NO-LOCK NO-ERROR.
                            IF AVAIL _field THEN ASSIGN
                                hbhRecKey2 = hbRecKey:BUFFER-FIELD(_field._field-physpos) NO-ERROR.
                        END.
                    END.
                END.                
                IF AVAIL (_file) THEN ASSIGN
                    cDisp = FILL(" ",124)
                    SUBSTRING(cDisp,1,26) = SUBSTRING(cName,1,26)
                    SUBSTRING(cDisp,28,12) = IF AVAIL users THEN STRING(users.phone,"999-999-9999") ELSE ""
                    SUBSTRING(cDisp,42,8) = SUBSTRING(_lock._lock-Tty,1,8)
                    SUBSTRING(cDisp,52,20) = SUBSTRING(_file._file-name,1,20)
                    SUBSTRING(cDisp,74,22) = (IF VALID-HANDLE(hbhRecKey1) AND hbhRecKey1:STRING-VALUE NE ? THEN TRIM(hbhRecKey1:STRING-VALUE) ELSE "") + "|" + 
                                             (IF VALID-HANDLE(hbhRecKey2) AND hbhRecKey2:STRING-VALUE NE ? THEN TRIM(hbhRecKey2:STRING-VALUE) ELSE "")
                    SUBSTRING(cDisp,98,4) = IF INDEX(_lock._lock-flags,"X") > 0 THEN "EXCL" ELSE
                                            IF INDEX(_lock._lock-flags,"S") > 0 THEN "SHRD" ELSE
                                            IF INDEX(_lock._lock-flags,"U") > 0 THEN "UPGR" ELSE
                                            IF INDEX(_lock._lock-flags,"L") > 0 THEN "LIMB" ELSE
                                            IF INDEX(_lock._lock-flags,"Q") > 0 THEN "QUED" ELSE
                                            ""
                    SUBSTRING(cDisp,104,12) = ttLocks2.ttfDispTime.
                slLockList:ADD-LAST(cDisp).
                IF VALID-HANDLE(hbhRecKey1) THEN DELETE OBJECT hbhRecKey1.
                IF VALID-HANDLE(hbhRecKey2) THEN DELETE OBJECT hbhRecKey2.
                DELETE OBJECT hqRecKey.
                DELETE OBJECT hbRecKey.            
            END.
        END.
        WAIT-FOR 'CHOOSE' OF bStop PAUSE INTEGER(fiInterval:SCREEN-VALUE).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

