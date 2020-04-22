&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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
DEF VAR cUserList AS CHAR NO-UNDO.
DEF VAR ictr AS INT NO-UNDO.
DEF VAR jCtr AS INT NO-UNDO.
DEF VAR lMonitor AS LOG NO-UNDO.
DEF VAR hqRecKey AS HANDLE NO-UNDO.
DEF VAR hbRecKey AS HANDLE NO-UNDO.
DEF VAR hbhRecKey1 AS HANDLE NO-UNDO.
DEF VAR hbhRecKey2 AS HANDLE NO-UNDO.
DEF VAR cName AS CHAR NO-UNDO.
DEF VAR cKeyString AS CHAR NO-UNDO.
DEF VAR lMonitorRunning AS LOG NO-UNDO.
DEF VAR iLockCountSel AS INT NO-UNDO.
DEF VAR iLockCountAll AS INT NO-UNDO.
DEF VAR iTransCountSel AS INT NO-UNDO.
DEF VAR iTransCountAll AS INT NO-UNDO.
DEF VAR hQuery AS HANDLE NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hBufField AS HANDLE NO-UNDO EXTENT 17.
DEF VAR hBrowse AS HANDLE NO-UNDO.
DEF VAR cExportFile AS CHAR NO-UNDO.
DEF VAR cRecKey AS CHAR NO-UNDO.

{src/adm2/widgetprto.i}

DEF TEMP-TABLE ttLocks
    FIELD ttfFlags AS CHAR LABEL "Type"
    FIELD ttfLock-userid AS CHAR LABEL "User ID" FORMAT "x(12)"
    FIELD ttfLock-name AS CHAR LABEL "Name" FORMAT "x(24)"
    FIELD ttfLock-Tty AS CHAR LABEL "Terminal" FORMAT "x(12)"
    FIELD ttfTable AS CHAR LABEL "Table" FORMAT "x(12)"
    FIELD ttfDispTime AS CHAR LABEL "Duration" FORMAT "x(10)"
    FIELD ttfFields AS CHAR  LABEL "Fields" FORMAT "x(32)"
    FIELD ttfValues AS CHAR LABEL "Values" FORMAT "x(32)"

    FIELD ttfUserID AS CHAR LABEL "User ID"
    FIELD ttfLock-Id AS INT64 LABEL "Lock ID"
    FIELD ttfLock-Usr AS INT LABEL "Lock User#"
    FIELD ttfLock-Table AS INT LABEL "Table#"
    FIELD ttfLock-RecId AS RECID LABEL "Recid"
    FIELD ttfLockTime AS DATETIME-TZ LABEL "Lock Time"
    FIELD ttfDuration AS INT LABEL "Duration"
    FIELD ttfLock-Type AS CHAR LABEL "Lk Type"
    .
DEF VAR cLabels AS CHAR INITIAL 
    "Type,User ID,Name,Terminal,Table,Duration,Fields,Values,User ID,Lock ID,Lock User#,Table#,Recid,Lock Time,Duration,Lk Type".
    
DEF TEMP-TABLE ttLocks2
    LIKE ttLocks.
    
DEF TEMP-TABLE ttLockUsers
    FIELD ttfUserID AS CHAR 
    FIELD ttfLockCount AS INT
    FIELD ttfExclusive AS INT 
    FIELD ttfShared AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiInstructions eInstructions bRefresh bExit ~
fiExportFile bGetFile bExport 
&Scoped-Define DISPLAYED-OBJECTS fiInstructions eInstructions fiExportFile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "Exit" 
     SIZE 7 BY 1.67 TOOLTIP "Exit"
     FONT 6.

DEFINE BUTTON bExport 
     LABEL "Export" 
     SIZE 27 BY 1.43 TOOLTIP "Export to a CSV file"
     FONT 6.

DEFINE BUTTON bGetFile 
     IMAGE-UP FILE "graphics/16x16/folder.png":U
     LABEL "" 
     SIZE 6 BY 1.19 TOOLTIP "Select a file for export".

DEFINE BUTTON bRefresh 
     LABEL "Refresh" 
     SIZE 27 BY 1.43 TOOLTIP "Refresh the list"
     FONT 6.

DEFINE VARIABLE eInstructions AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 143 BY 7.14 NO-UNDO.

DEFINE VARIABLE fiExportFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Export File Location" 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE fiInstructions AS CHARACTER FORMAT "X(256)":U INITIAL "Instructions:" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiInstructions AT ROW 1.24 COL 4 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     eInstructions AT ROW 2.43 COL 11 NO-LABEL NO-TAB-STOP 
     bRefresh AT ROW 9.81 COL 11
     bExit AT ROW 27.43 COL 149
     fiExportFile AT ROW 27.67 COL 29 COLON-ALIGNED
     bGetFile AT ROW 27.67 COL 108
     bExport AT ROW 27.67 COL 116
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.8 BY 28.52
         DEFAULT-BUTTON bRefresh.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Lock Monitor"
         HEIGHT             = 28.52
         WIDTH              = 159.8
         MAX-HEIGHT         = 30.48
         MAX-WIDTH          = 159.8
         VIRTUAL-HEIGHT     = 30.48
         VIRTUAL-WIDTH      = 159.8
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
ASSIGN 
       eInstructions:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fiInstructions:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Lock Monitor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Lock Monitor */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bExport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bExport wWin
ON CHOOSE OF bExport IN FRAME fMain /* Export */
DO:
    DEF VARIABLE cLineOut AS CHAR.
    OUTPUT TO VALUE(cExportFile).
    PUT UNFORMATTED cLabels + CHR(10).
    FOR EACH ttLocks2:
        EXPORT DELIMITER "," ttLocks2.
    END.
    OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME bGetFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bGetFile wWin
ON CHOOSE OF bGetFile IN FRAME fMain
DO:
    SYSTEM-DIALOG GET-FILE cExportFile
        TITLE "Lock Monitor Export File"
        FILTERS "CSV Files (*.csv)" "*.csv"
        CREATE-TEST-FILE
        USE-FILENAME
        DEFAULT-EXTENSION ".csv"
        INITIAL-DIR "C:\tmp".  
    ASSIGN 
        fiExportFile:SCREEN-VALUE = cExportFile.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bRefresh wWin
ON CHOOSE OF bRefresh IN FRAME fMain /* Refresh */
DO:
    RUN pRefresh.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY fiInstructions eInstructions fiExportFile 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE fiInstructions eInstructions bRefresh bExit fiExportFile bGetFile 
         bExport 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

    ASSIGN 
        fiExportFile:SCREEN-VALUE IN FRAME {&frame-name} = 
            "c:\tmp\lockmonitor-" + 
            STRING(YEAR(TODAY),"9999") + 
            STRING(MONTH(TODAY),"99") + 
            STRING(DAY(TODAY),"99") + "-" +
            SUBSTRING(STRING(TIME,"HH:MM:SS"),1,2) + 
            SUBSTRING(STRING(TIME,"HH:MM:SS"),4,2) + 
            SUBSTRING(STRING(TIME,"HH:MM:SS"),7,2) + ".csv"
        cExportFile = fiExportFile:SCREEN-VALUE
        eInstructions:SCREEN-VALUE = 
            "This function displays a list of all record locks currently held by your system's users.  Each line represents " +
            "a separate record being locked.  The visible columns include:" + CHR(10) +
            "'Type' - the level of the lock.  These vary in importance:" + CHR(10) +
            "    'EXCL' - other users may not access this record at all until the lock is released" + CHR(10) +
            "    'SHRD' - other users may view this record but not update it until the lock is released" + CHR(10) +
            "    Locks can normally be released by a SAVE action, a CANCEL action, or closing a function." + CHR(10) +
            "'UserID/Name/Terminal' - the user holding the lock and the workstation where the lock was originated" + CHR(10) +
            "'Table/Duration' - the table containing the locked record and the amount of time the lock has been in place" + CHR(10) +
            "    (Note: the duration will be the lesser of the total lock time or the time since you started this program)"  + CHR(10) +
            "'Fields/Values' - information describing the specific record held by this lock" + CHR(10) +
            "You can EXPORT the contents of this list to a CSV file by entering a file name in the box below and pressing the Export button.". 
    APPLY 'entry' TO bRefresh.            
            
    RUN pRefresh.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipGetLockData wWin 
PROCEDURE ipGetLockData :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR hIdxFld AS HANDLE EXTENT 50 NO-UNDO.
    DEF VAR cFieldString AS CHAR NO-UNDO.
    DEF VAR iIdxCt AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    DEF VAR kCtr AS INT NO-UNDO.
    DEF VAR hTestName AS HANDLE NO-UNDO.
    DEF VAR cTestName AS CHAR NO-UNDO.
    DEF VAR cDisp AS CHAR NO-UNDO.
    
    ASSIGN 
        iLockCountAll = 0
        iLockCountSel = 0
        jCtr = 0.

    EMPTY TEMP-TABLE ttLocks.
    
    FOR EACH _lock WHERE
        _lock._lock-usr <> ? AND 
        _lock._lock-recid <> ? NO-LOCK:
        ASSIGN 
            iIdxCt = 0
            cFieldString = ""
            cKeyString = "".     
        FIND FIRST _file WHERE
            _file._file-number = _lock._lock-table
            USE-INDEX _file-number
            NO-LOCK NO-ERROR.
        IF _file._file-name BEGINS "_" 
        OR _file._file-name EQ "users" THEN NEXT.
        FIND _user NO-LOCK WHERE
            _user._userid EQ _lock._lock-name
            NO-ERROR.
        IF AVAIL _file THEN FIND FIRST _index OF _file WHERE 
            _index._unique EQ TRUE  
            NO-LOCK NO-ERROR.
        IF NOT AVAIL _index THEN FIND FIRST _index OF _file WHERE 
                RECID(_index) = _file._prime-index
                NO-LOCK NO-ERROR.
        IF AVAIL _index THEN DO:
            CREATE BUFFER hbRecKey FOR TABLE _file._file-name.
            CREATE QUERY hqRecKey.
            hqRecKey:SET-BUFFERS(hbRecKey).
            hqRecKey:QUERY-PREPARE("preselect each " + _file._file-name + " no-lock where recid(" + _file._file-name + 
                ") = " + STRING(_lock._Lock-RecID)).
            hqRecKey:QUERY-OPEN.
            IF hqRecKey:NUM-RESULTS NE 0 THEN DO:
                hqRecKey:GET-FIRST().
                FOR EACH _index-field OF _index NO-LOCK:
                    FIND FIRST _field OF _index-field NO-LOCK NO-ERROR.
                    IF AVAIL _field 
                    AND _field._extent LT 2 THEN DO:
                        ASSIGN
                            iIdxCt = iIdxCt + 1.
                        checkname:
                        DO jCtr = 1 TO hbRecKey:NUM-FIELDS:
                            ASSIGN 
                                hTestName = hbRecKey:BUFFER-FIELD(jCtr)
                                cTestName = hTestName:NAME.
                            IF cTestName EQ "rec_key" THEN ASSIGN 
                                cRecKey = hTestName:BUFFER-VALUE().
                            IF hTestName:EXTENT GT 1 THEN NEXT.
                            IF cTestName EQ _field._field-name THEN DO:
                                ASSIGN 
                                    hIdxFld[iIdxCt] = hbRecKey:BUFFER-FIELD(jCtr) 
                                    cFieldString = cFieldString + hIdxFld[iIdxCt]:NAME  + "|" 
                                    cKeyString = cKeyString + 
                                                    (IF hIdxFld[iIdxCt]:BUFFER-VALUE EQ "" THEN "N/A" ELSE STRING(hIdxFld[iIdxCt]:BUFFER-VALUE)) + "|" 
                                    NO-ERROR.
                                LEAVE checkname.
                            END.
                        END.
                    END.
                END.    
                ASSIGN 
                    cFieldString = TRIM(cFieldString,"|")
                    cKeyString = TRIM(cKeyString,"|").     
            END.                

            CREATE ttLocks.
            ASSIGN
                ttLocks.ttfLock-Id = _lock._Lock-ID
                ttLocks.ttfLock-Usr = _lock._Lock-Usr
                ttLocks.ttfLock-userid = IF AVAIL _user THEN _user._userid ELSE "" 
                ttLocks.ttfLock-Name = _lock._Lock-Name
                ttLocks.ttfLock-Type = _lock._Lock-Type
                ttLocks.ttfLock-Table = _lock._Lock-Table
                ttLocks.ttfLock-RecID = _lock._Lock-RecID
                ttLocks.ttfLock-Tty = _lock._Lock-Tty
                ttLocks.ttfFlags = IF INDEX(_lock._lock-flags,"X") > 0 THEN "EXCL" ELSE
                                   IF INDEX(_lock._lock-flags,"S") > 0 THEN "SHRD" ELSE
                                   IF INDEX(_lock._lock-flags,"U") > 0 THEN "UPGR" ELSE
                                   IF INDEX(_lock._lock-flags,"L") > 0 THEN "LIMB" ELSE
                                   IF INDEX(_lock._lock-flags,"Q") > 0 THEN "QUED" ELSE
                                   ""
                ttLocks.ttfTable = _file._file-name
                ttLocks.ttfLockTime = NOW
                ttLocks.ttfDuration = 0
                ttLocks.ttfDispTime = "00:00:00"
                ttLocks.ttfUserID = _lock._lock-name
                ttLocks.ttfFields = cFieldString
                ttLocks.ttfValues = cKeyString
                .
            DELETE OBJECT hqRecKey.
            DELETE OBJECT hbRecKey.            
        END.
    END.
    
    FOR EACH ttLocks:
        FIND FIRST ttLocks2 WHERE
            ttLocks2.ttfLock-ID EQ ttLocks.ttfLock-ID
            NO-ERROR.
        IF NOT AVAIL ttLocks2 THEN 
        DO:
            CREATE ttLocks2.
            BUFFER-COPY ttLocks TO ttLocks2.
        END.
        ELSE 
        DO:
            ASSIGN
                ttLocks2.ttfDuration = (NOW - ttLocks2.ttfLockTime) / 1000
                ttLocks2.ttfDispTime = STRING(ttLocks2.ttfDuration,"HH:MM:SS").
        END.
    END.
    FOR EACH ttLocks2 WHERE NOT CAN-FIND (FIRST ttLocks WHERE
        ttLocks.ttfLock-ID = ttLocks2.ttfLock-ID):
        DELETE ttLocks2.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipGetTransData wWin 
PROCEDURE ipGetTransData :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    ASSIGN 
        iCtr = 0
        jCtr = 0
        iTransCountAll = 0
        iTransCountSel = 0.
    
    FOR EACH _trans NO-LOCK WHERE 
        _trans._trans-usrnum NE ?:
        FIND FIRST _user NO-LOCK WHERE 
            _user._user_number = _trans._Trans-Usrnum
            NO-ERROR.
        IF NOT AVAIL _user THEN NEXT.
        ASSIGN 
            iTransCountAll = iTransCountAll + 1
            iTransCountSel = IF CAN-DO(cUserList,_user._userid) THEN iTransCountSel + 1 ELSE iTransCountSel.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRefresh wWin 
PROCEDURE pRefresh :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN pRunMonitor.
    IF VALID-HANDLE(hBuffer) THEN DELETE OBJECT hBuffer.
    IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hBuffer.
    IF VALID-HANDLE(hBrowse) THEN DELETE OBJECT hBuffer.
    DO iCtr = 1 TO 17:
        IF VALID-HANDLE(hBufField[iCtr]) THEN DELETE OBJECT hBufField[iCtr].
    END.    
    hBuffer = TEMP-TABLE ttLocks2:HANDLE.
    CREATE QUERY hQuery.
    hQuery:ADD-BUFFER (BUFFER ttLocks2:HANDLE).
    hQuery:QUERY-PREPARE ("FOR EACH ttLocks2").
    hQuery:QUERY-OPEN.
    CREATE BROWSE hBrowse
        ASSIGN 
            FRAME = FRAME {&frame-name}:HANDLE 
            QUERY = hQuery:handle
            ROW-MARKERS = FALSE
            ROW = 11.48
            COLUMN = 11.00
            WIDTH = 143.00
            HEIGHT = 15.71
            VISIBLE = TRUE 
            READ-ONLY = TRUE 
            SENSITIVE = TRUE
            SEPARATORS = TRUE
            . 
    hBrowse:ADD-COLUMNS-FROM (BUFFER ttLocks2:HANDLE).
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunMonitor wWin 
PROCEDURE pRunMonitor :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipGetTransData.
    RUN ipGetLockData.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

