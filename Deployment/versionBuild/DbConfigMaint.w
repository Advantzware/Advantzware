&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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
DEFINE TEMP-TABLE ttConfig
    FIELD cDbName AS CHAR 
    FIELD cConfigName AS CHAR 
    FIELD cVarName AS CHAR 
    FIELD cVarValue AS CHAR.
    
DEFINE TEMP-TABLE ttDatabase
    FIELD cDbName AS CHAR 
    FIELD cVarName AS CHAR 
    FIELD cVarValue AS CHAR.
    
DEFINE TEMP-TABLE ttServer
    FIELD cDbName AS CHAR 
    FIELD cServerName AS CHAR 
    FIELD cVarName AS CHAR 
    FIELD cVarValue AS CHAR.
    
DEFINE TEMP-TABLE ttPorts
    FIELD iPort AS INT.    
    
DEFINE VARIABLE cFileHeader AS CHAR NO-UNDO.
DEFINE VARIABLE cFileFooter AS CHAR NO-UNDO.
DEFINE VARIABLE cConfigList AS CHAR NO-UNDO.
DEFINE VARIABLE cDatabaseList AS CHAR NO-UNDO.
DEFINE VARIABLE cServerGroupList AS CHAR NO-UNDO.
DEFINE VARIABLE cHostName AS CHAR FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE cDLCDir AS CHAR NO-UNDO.
DEFINE VARIABLE cAdminServerPort AS CHAR NO-UNDO.
DEFINE VARIABLE lAdminServerRunning AS LOG NO-UNDO.
DEFINE VARIABLE lUpdateMode AS LOG NO-UNDO.
DEFINE VARIABLE lInputError AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 slDbList tbDbRunning cbDrive ~
tbAutoStart tbWatchdog tbSQL tbCreateDB bAdd bUpdate bDelete bBackup ~
bViewDbLog bRestore tbAdminService bExit bViewAdmin 
&Scoped-Define DISPLAYED-OBJECTS cExistingDBs lParameters slDbList cDbName ~
tbDbRunning iConnections cbDrive cFilePath iPort tbAutoStart tbWatchdog ~
iBufferBlocks iLockTable iMaxUsers iBufferSize cOtherParms tbSQL iSqlPort ~
tbCreateDB rsCreateFrom cBackupFile lAdminService iAdminPort tbAdminService 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAdminServerStatus C-Win 
FUNCTION fAdminServerStatus RETURNS LOGICAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetAdminServerPort C-Win 
FUNCTION fGetAdminServerPort RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetDateTimeForExtension C-Win 
FUNCTION fGetDateTimeForExtension RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetDbLongName C-Win 
FUNCTION fGetDbLongName RETURNS CHARACTER
  ( INPUT ipcDbName AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetLongDate C-Win 
FUNCTION fGetLongDate RETURNS CHARACTER
  ( INPUT ipdDate AS DATE ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetNextPort C-Win 
FUNCTION fGetNextPort RETURNS INTEGER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU MENU-BAR-C-Win MENUBAR
       MENU-ITEM m_File         LABEL "File"          
       MENU-ITEM m_Tools        LABEL "Tools"         .


/* Definitions of the field level widgets                               */
DEFINE BUTTON bAdd 
     LABEL "Add DB" 
     SIZE 17 BY 1.14.

DEFINE BUTTON bBackup 
     LABEL "Backup DB" 
     SIZE 17 BY 1.14.

DEFINE BUTTON bDelete 
     LABEL "Delete DB" 
     SIZE 17 BY 1.14.

DEFINE BUTTON bExit AUTO-END-KEY 
     LABEL "Exit" 
     SIZE 12 BY 2.19.

DEFINE BUTTON bRestore 
     LABEL "Restore DB" 
     SIZE 17 BY 1.14.

DEFINE BUTTON bStartAdmin 
     LABEL "Start Service" 
     SIZE 18 BY 1.14.

DEFINE BUTTON bStartDB 
     LABEL "Start DB" 
     SIZE 17 BY 1.14.

DEFINE BUTTON bStopAdmin 
     LABEL "Stop Service" 
     SIZE 18 BY 1.14.

DEFINE BUTTON bStopDB 
     LABEL "Stop DB" 
     SIZE 17 BY 1.14.

DEFINE BUTTON bUpdate 
     LABEL "Update DB" 
     SIZE 17 BY 1.14.

DEFINE BUTTON bViewAdmin 
     LABEL "View Svc Log" 
     SIZE 18 BY 1.14.

DEFINE BUTTON bViewDbLog 
     LABEL "View DB Log" 
     SIZE 17 BY 1.14.

DEFINE VARIABLE cbDrive AS CHARACTER FORMAT "X(2)":U INITIAL "C:" 
     LABEL "Drive~\Path" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEMS "C:","D:","E:","F:","G:","H:","I:","J:" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE cBackupFile AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\asigui~\backups~\databases~\" 
     LABEL "Backup File" 
     VIEW-AS FILL-IN 
     SIZE 68 BY 1.24 NO-UNDO.

DEFINE VARIABLE cDbName AS CHARACTER FORMAT "X(11)":U 
     LABEL "Database Name" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.24
     FONT 35 NO-UNDO.

DEFINE VARIABLE cExistingDBs AS CHARACTER FORMAT "X(256)":U INITIAL "Existing Databases:" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE cFilePath AS CHARACTER FORMAT "X(256)":U INITIAL "~\asigui~\databases~\test" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1.24 NO-UNDO.

DEFINE VARIABLE cOtherParms AS CHARACTER FORMAT "X(256)":U 
     LABEL "Other Parameters" 
     VIEW-AS FILL-IN 
     SIZE 68 BY 1.24 NO-UNDO.

DEFINE VARIABLE iAdminPort AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Port" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.24 NO-UNDO.

DEFINE VARIABLE iBufferBlocks AS CHARACTER FORMAT "X(256)":U 
     LABEL "Buffer Blocks" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.24 NO-UNDO.

DEFINE VARIABLE iBufferSize AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Buffer Size" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.24 NO-UNDO.

DEFINE VARIABLE iConnections AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Active Users" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.24 NO-UNDO.

DEFINE VARIABLE iLockTable AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Lock Table" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.24 NO-UNDO.

DEFINE VARIABLE iMaxUsers AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "Max Users" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.24 NO-UNDO.

DEFINE VARIABLE iPort AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Port" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.24 NO-UNDO.

DEFINE VARIABLE iSqlPort AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "SQL Port" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.24 NO-UNDO.

DEFINE VARIABLE lAdminService AS CHARACTER FORMAT "X(256)":U INITIAL " Admin Service" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE lParameters AS CHARACTER FORMAT "X(256)":U INITIAL " Parameters/Stats:" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE rsCreateFrom AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "From Backup", "Backup",
"From Empty", "Empty"
     SIZE 37 BY 1.24 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY 17.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 3.81.

DEFINE VARIABLE slDbList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 33 BY 20.71 NO-UNDO.

DEFINE VARIABLE tbAdminService AS LOGICAL INITIAL no 
     LABEL "Admin Service Running?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE tbAutoStart AS LOGICAL INITIAL yes 
     LABEL "AutoStart?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1.24 NO-UNDO.

DEFINE VARIABLE tbCreateDB AS LOGICAL INITIAL no 
     LABEL "Create New DB Files?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1.24 NO-UNDO.

DEFINE VARIABLE tbDbRunning AS LOGICAL INITIAL no 
     LABEL "Running?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1.24 NO-UNDO.

DEFINE VARIABLE tbSQL AS LOGICAL INITIAL no 
     LABEL "SQL Connections?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1.24 NO-UNDO.

DEFINE VARIABLE tbWatchdog AS LOGICAL INITIAL yes 
     LABEL "Watchdog?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1.24 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmMain
     cExistingDBs AT ROW 1.24 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 12 NO-TAB-STOP 
     lParameters AT ROW 1.24 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 42 NO-TAB-STOP 
     slDbList AT ROW 2.67 COL 5 NO-LABEL WIDGET-ID 10
     cDbName AT ROW 2.67 COL 64 COLON-ALIGNED WIDGET-ID 2
     tbDbRunning AT ROW 2.67 COL 91 WIDGET-ID 64 NO-TAB-STOP 
     iConnections AT ROW 2.67 COL 123 COLON-ALIGNED WIDGET-ID 70 NO-TAB-STOP 
     cbDrive AT ROW 4.1 COL 64 COLON-ALIGNED WIDGET-ID 8
     cFilePath AT ROW 4.1 COL 73 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     iPort AT ROW 5.52 COL 64 COLON-ALIGNED WIDGET-ID 14
     tbAutoStart AT ROW 5.52 COL 82 WIDGET-ID 24
     tbWatchdog AT ROW 5.52 COL 104 WIDGET-ID 26
     iBufferBlocks AT ROW 7.19 COL 64 COLON-ALIGNED WIDGET-ID 94
     iLockTable AT ROW 7.19 COL 98 COLON-ALIGNED WIDGET-ID 30
     iMaxUsers AT ROW 8.62 COL 64 COLON-ALIGNED WIDGET-ID 32
     iBufferSize AT ROW 8.62 COL 98 COLON-ALIGNED WIDGET-ID 34
     cOtherParms AT ROW 10.05 COL 64 COLON-ALIGNED WIDGET-ID 22
     tbSQL AT ROW 11.48 COL 66 WIDGET-ID 36
     iSqlPort AT ROW 11.48 COL 107 COLON-ALIGNED WIDGET-ID 40
     tbCreateDB AT ROW 12.91 COL 66 WIDGET-ID 92
     rsCreateFrom AT ROW 12.91 COL 97 NO-LABEL WIDGET-ID 58
     cBackupFile AT ROW 14.33 COL 64 COLON-ALIGNED WIDGET-ID 62
     bAdd AT ROW 16.24 COL 66 WIDGET-ID 44
     bUpdate AT ROW 16.24 COL 84 WIDGET-ID 46
     bDelete AT ROW 16.24 COL 102 WIDGET-ID 48
     bBackup AT ROW 16.24 COL 120 WIDGET-ID 74
     bStopDB AT ROW 17.67 COL 66 WIDGET-ID 80
     bStartDB AT ROW 17.67 COL 84 WIDGET-ID 82
     bViewDbLog AT ROW 17.67 COL 102 WIDGET-ID 78
     bRestore AT ROW 17.67 COL 120 WIDGET-ID 84
     lAdminService AT ROW 19.81 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 90 NO-TAB-STOP 
     iAdminPort AT ROW 20.76 COL 110 COLON-ALIGNED WIDGET-ID 96
     tbAdminService AT ROW 21 COL 66 WIDGET-ID 76 NO-TAB-STOP 
     bExit AT ROW 21.95 COL 128 WIDGET-ID 72
     bStopAdmin AT ROW 22.19 COL 66 WIDGET-ID 52
     bStartAdmin AT ROW 22.19 COL 85 WIDGET-ID 50
     bViewAdmin AT ROW 22.19 COL 104 WIDGET-ID 54
     RECT-6 AT ROW 1.71 COL 42 WIDGET-ID 86
     RECT-7 AT ROW 20.29 COL 42 WIDGET-ID 88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 142.6 BY 23.62
         FONT 5
         CANCEL-BUTTON bExit WIDGET-ID 100.


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
         TITLE              = "Database Configuration Maintenance"
         HEIGHT             = 23.62
         WIDTH              = 142.6
         MAX-HEIGHT         = 26.24
         MAX-WIDTH          = 184.4
         VIRTUAL-HEIGHT     = 26.24
         VIRTUAL-WIDTH      = 184.4
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frmMain
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bStartAdmin IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bStartDB IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bStopAdmin IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bStopDB IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cBackupFile IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cDbName IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cExistingDBs IN FRAME frmMain
   NO-ENABLE                                                            */
ASSIGN 
       cExistingDBs:READ-ONLY IN FRAME frmMain        = TRUE.

/* SETTINGS FOR FILL-IN cFilePath IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cOtherParms IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iAdminPort IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iBufferBlocks IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iBufferSize IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iConnections IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iLockTable IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iMaxUsers IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iPort IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iSqlPort IN FRAME frmMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lAdminService IN FRAME frmMain
   NO-ENABLE                                                            */
ASSIGN 
       lAdminService:READ-ONLY IN FRAME frmMain        = TRUE.

/* SETTINGS FOR FILL-IN lParameters IN FRAME frmMain
   NO-ENABLE                                                            */
ASSIGN 
       lParameters:READ-ONLY IN FRAME frmMain        = TRUE.

/* SETTINGS FOR RADIO-SET rsCreateFrom IN FRAME frmMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Database Configuration Maintenance */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
  QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Database Configuration Maintenance */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bStartDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bStartDB C-Win
ON CHOOSE OF bStartDB IN FRAME frmMain /* Start DB */
OR CHOOSE OF bStopDB 
OR CHOOSE OF bAdd
OR CHOOSE OF bBackup
OR CHOOSE OF bDelete
OR CHOOSE OF bRestore
OR CHOOSE OF bUpdate
OR CHOOSE OF bViewDbLog
OR CHOOSE OF bViewAdmin
OR CHOOSE OF bViewDbLog
DO:
    CASE SELF:NAME:
        WHEN "bStartDB" THEN RUN pStartDatabase (fGetDbLongName(cDbName:SCREEN-VALUE)).
        WHEN "bStopDB" THEN RUN pStopDatabase (fGetDbLongName(cDbName:SCREEN-VALUE)). 
        WHEN "bAdd" THEN DO:
            ASSIGN 
                lUpdateMode = SELF:LABEL EQ "Add DB" 
                SELF:LABEL = IF lUpdateMode THEN "Save" ELSE "Add DB"
                bUpdate:SENSITIVE = NOT lUpdateMode
                bDelete:SENSITIVE = NOT lUpdateMode
                bStopDB:SENSITIVE = NOT lUpdateMode
                bStartDB:SENSITIVE = NOT lUpdateMode
                bViewDBLog:SENSITIVE = NOT lUpdateMode
                bBackup:SENSITIVE = NOT lUpdateMode
                bRestore :SENSITIVE = NOT lUpdateMode
                slDbList:SENSITIVE = NOT lUpdateMode
                .
            IF lUpdateMode THEN DO:
                RUN pOpenForEdit ("Add").
                RUN pAddDB.
            END.
            ELSE DO:
                IF tbCreateDb:CHECKED 
                AND rsCreateFrom:SCREEN-VALUE EQ "Backup"
                AND INDEX(cBackupFile:SCREEN-VALUE,".bak") EQ 0 THEN DO:
                    MESSAGE 
                        "You must supply a valid backup filename to complete this action." SKIP 
                        "Press F1 to locate a file, or enter one manually."
                        VIEW-AS ALERT-BOX.
                    ASSIGN 
                        SELF:LABEL = "Save".
                    APPLY 'entry' TO cBackupFile.
                    RETURN NO-APPLY.
                END.
                RUN pOpenForEdit ("Save").
                RUN pSaveNewDB.
            END.
        END.
        WHEN "bUpdate" THEN DO:
            ASSIGN 
                lUpdateMode = SELF:LABEL EQ "Update DB" OR lInputError
                SELF:LABEL = IF lUpdateMode THEN "Save" ELSE "Update DB"
                bAdd:SENSITIVE = NOT lUpdateMode
                bDelete:SENSITIVE = NOT lUpdateMode
                bStopDB:SENSITIVE = NOT lUpdateMode
                bStartDB:SENSITIVE = NOT lUpdateMode
                bViewDBLog:SENSITIVE = NOT lUpdateMode
                bBackup:SENSITIVE = NOT lUpdateMode
                bRestore :SENSITIVE = NOT lUpdateMode
                slDbList:SENSITIVE = NOT lUpdateMode
                .
            IF lUpdateMode THEN DO:
                RUN pOpenForEdit ("Update").
                RUN pUpdateDB.
            END.
            ELSE DO:
                RUN pOpenForEdit ("Save").
                RUN pSaveExistingDB.
            END.
        END.
        WHEN "bViewAdmin" THEN DO:
            OS-COMMAND SILENT VALUE("notepad '" + cDLCDir + "WRK\admserv.log'").
        END.
        WHEN "bViewDbLog" THEN DO:
            OS-COMMAND SILENT VALUE("notepad '" + cbDrive:SCREEN-VALUE + cFilePath:SCREEN-VALUE + 
                                    "\" + cDbName:SCREEN-VALUE + ".lg'").
        END.
        
        
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bStopAdmin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bStopAdmin C-Win
ON CHOOSE OF bStopAdmin IN FRAME frmMain /* Stop Service */
OR CHOOSE OF bStartAdmin
DO:
    DEF VAR cCmdString AS CHAR NO-UNDO.
    CASE SELF:NAME:
        WHEN "bStopAdmin" THEN DO:
            ASSIGN 
                cCmdString = cDLCDir + "\bin\proadsv -stop -keepservers -port " + iAdminPort:SCREEN-VALUE + " && EXIT".
            OS-COMMAND VALUE(cCmdString).
        END.
        WHEN "bStartAdmin" THEN DO:
            ASSIGN 
                cCmdString = cDLCDir + "\bin\proadsv -start -port " + 
                             IF iAdminPort:SCREEN-VALUE NE "0" THEN 
                                iAdminPort:SCREEN-VALUE ELSE "20931" 
                             + " && EXIT".
            OS-COMMAND VALUE(cCmdString).
        END.
    END CASE.
    tbAdminService:CHECKED = fAdminServerStatus().    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME cBackupFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cBackupFile C-Win
ON HELP OF cBackupFile IN FRAME frmMain /* Backup File */
DO:
    DEFINE VARIABLE cFileName AS CHAR NO-UNDO.
    DEFINE VARIABLE lOkPressed AS LOG NO-UNDO INITIAL TRUE.
    SYSTEM-DIALOG GET-FILE cFileName
        TITLE "Choose backup file..."
        FILTERS "Backup files (*.bak)" "*.bak"
        INITIAL-DIR cbDrive:SCREEN-VALUE + "\asigui\backups\databases"
        MUST-EXIST 
        USE-FILENAME   
        UPDATE lOkPressed.
    IF lOkPressed THEN ASSIGN 
        cBackupFile:SCREEN-VALUE = cFileName.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cDbName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cDbName C-Win
ON LEAVE OF cDbName IN FRAME frmMain /* Database Name */
DO:
    CASE SELF:NAME:
        WHEN "cDbName" THEN DO:
            IF SELF:SCREEN-VALUE EQ "" THEN RETURN.
            IF SUBSTRING(SELF:SCREEN-VALUE,1,3) EQ "asi"
            OR SUBSTRING(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) EQ "D" THEN ASSIGN 
                iBufferBlocks:SCREEN-VALUE = "250000"
                tbSQL:CHECKED = TRUE.
            ELSE IF SUBSTRING(SELF:SCREEN-VALUE,1,3) EQ "aud"
            OR SUBSTRING(SELF:screen-value,LENGTH(SELF:SCREEN-VALUE),1) EQ "A" THEN ASSIGN 
                iBufferBlocks:SCREEN-VALUE = "100000"
                tbSQL:CHECKED = FALSE.
            APPLY 'value-changed' TO tbSQL.
            
            IF INDEX(SELF:SCREEN-VALUE,"test") NE 0 THEN ASSIGN 
                cFilePath:SCREEN-VALUE = "asigui\databases\test".
            ELSE IF INDEX(SELF:SCREEN-VALUE,"prod") NE 0  THEN ASSIGN 
                cFilePath:SCREEN-VALUE = "asigui\databases\prod".
            ELSE IF INDEX(SELF:SCREEN-VALUE,"ship") NE 0  THEN ASSIGN 
                cFilePath:SCREEN-VALUE = "asigui\databases\ship".
            ELSE IF SUBSTRING(SELF:SCREEN-VALUE,1,3) EQ "asi" 
            OR SUBSTRING(SELF:SCREEN-VALUE,1,3) EQ "aud" THEN 
                cFilePath:SCREEN-VALUE = "asigui\databases\cust".
                
            ASSIGN 
                cBackupFile:SCREEN-VALUE = "c:\asigui\backups\databases\" + SELF:SCREEN-VALUE + "bak".                
                
        END.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slDbList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slDbList C-Win
ON VALUE-CHANGED OF slDbList IN FRAME frmMain
OR VALUE-CHANGED OF tbDbRunning
OR VALUE-CHANGED OF tbAdminService
OR VALUE-CHANGED OF tbCreateDB
OR VALUE-CHANGED OF tbAutostart
OR VALUE-CHANGED OF tbSQL
OR VALUE-CHANGED OF tbWatchdog
DO:
    CASE SELF:NAME:
        WHEN "slDbList" THEN DO:
            RUN pClearDisplay.
            ASSIGN 
                cDbName:SCREEN-VALUE = SELF:SCREEN-VALUE.
            RUN pGetScreenValuesFromTempTables (SELF:SCREEN-VALUE).
            RUN pGetScreenValuesFromFileSystem (cbDrive:SCREEN-VALUE,
                                                cFilePath:screen-value,
                                                SELF:SCREEN-VALUE).
            RUN pGetOtherValues (SELF:SCREEN-VALUE).
            IF tbDbRunning:CHECKED THEN ASSIGN 
                bStopDB:SENSITIVE = TRUE 
                bStartDB:SENSITIVE = FALSE.                                 
            ELSE ASSIGN 
                bStopDB:SENSITIVE = FALSE  
                bStartDB:SENSITIVE = TRUE.
        END.   
        WHEN "tbDbRunning" OR 
        WHEN "tbAdminService" OR 
        WHEN "tbCreateDB" OR 
        WHEN "tbAutoStart" OR 
        WHEN "tbSQL" OR 
        WHEN "tbWatchdog" THEN DO:
            IF NOT lUpdateMode THEN DO:
                ASSIGN SELF:CHECKED = NOT SELF:CHECKED.
                RETURN NO-APPLY.
            END.
            ELSE IF SELF:NAME EQ "tbSQL" THEN ASSIGN 
                iSqlPort:SENSITIVE = SELF:CHECKED.
            ELSE IF SELF:NAME EQ "tbCreateDb" THEN ASSIGN 
                rsCreateFrom:SENSITIVE = SELF:CHECKED.
        END.
    END CASE.
    
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
  
  RUN pInitialize.
  
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
  DISPLAY cExistingDBs lParameters slDbList cDbName tbDbRunning iConnections 
          cbDrive cFilePath iPort tbAutoStart tbWatchdog iBufferBlocks 
          iLockTable iMaxUsers iBufferSize cOtherParms tbSQL iSqlPort tbCreateDB 
          rsCreateFrom cBackupFile lAdminService iAdminPort tbAdminService 
      WITH FRAME frmMain IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 slDbList tbDbRunning cbDrive tbAutoStart tbWatchdog 
         tbSQL tbCreateDB bAdd bUpdate bDelete bBackup bViewDbLog bRestore 
         tbAdminService bExit bViewAdmin 
      WITH FRAME frmMain IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmMain}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddDB C-Win 
PROCEDURE pAddDB :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR iNewPort AS INT.
    ASSIGN 
        iNewPort = fGetNextPort()
        iPort:SCREEN-VALUE IN FRAME frmMain = STRING(iNewPort)
        iSQLPort:SCREEN-VALUE = STRING(iNewPort + 1).
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildConfig C-Win 
PROCEDURE pBuildConfig :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    ASSIGN 
        cFileHeader = 
            "#" + CHR(10) +
            "# Connection Manager Properties File" + CHR(10) +
            "#" + CHR(10) +
            "%% Juniper Properties File" + CHR(10) +
            "%% version 1.1" + CHR(10) +
            "%% " + fGetLongDate(TODAY) + CHR(10)
        cFileFooter = 
            "#    host=localhost                  # -H" + CHR(10) +
            "#    initialservers=0                # n/a" + CHR(10) +
            "#    maxclientsperserver=0           # -Ma (calculated value)" + CHR(10) +
            "#    maxdynamicport=5000             # -maxport (5000 for NT; 2000 for UNIX)" + CHR(10) +
            "#    messagebuffersize=350           # -Mm (4gl only)" + CHR(10) +
            "#    minclientsperserver=1           # -Mi" + CHR(10) +
            "#    mindynamicport=3000             # -minport (3000 for NT; 1025 for UNIX)" + CHR(10) +
            "#    networkclientsupport=true       # false for self-service" + CHR(10) +
            "#    numberofservers=0               # -Mpb" + CHR(10) +
            "#    port=0                          # -S ; Must be non-zero" + CHR(10) +
            "#                                    # when networkclientsupport=true" + CHR(10) +
            "#    prosqltrc=nnnnnnnnnnn           # turn on various levels of SQL tracing" + CHR(10) +
            "#    reportinginterval=1             # -rpint (4gl only)" + CHR(10) +
            "#    serverexe=<4gl server location> # _mprosrv (4gl only)" + CHR(10) +
            "#    type=both                       # n/a"
            .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClearDisplay C-Win 
PROCEDURE pClearDisplay :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    ASSIGN 
        cDbName:SCREEN-VALUE IN FRAME frmMain = ""
        cFilePath:SCREEN-VALUE = ""
        cOtherParms:SCREEN-VALUE = ""
        cBackupFile:SCREEN-VALUE = ""
        iConnections:SCREEN-VALUE = ""
        iPort:SCREEN-VALUE = ""
        iBufferBlocks:SCREEN-VALUE = ""
        iLockTable:SCREEN-VALUE = ""
        iMaxUsers:SCREEN-VALUE = ""
        iBufferSize:SCREEN-VALUE = ""
        iSQLPort:SCREEN-VALUE = ""
        tbDbRunning:CHECKED = FALSE 
        tbAutoStart:CHECKED = FALSE
        tbWatchdog:CHECKED = FALSE
        tbSQL:CHECKED = FALSE
        tbCreateDb:CHECKED = FALSE
        cbDrive:SCREEN-VALUE = "C:"
        rsCreateFrom:SCREEN-VALUE = "Backup"
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateDatabase C-Win 
PROCEDURE pCreateDatabase :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDbFileName AS CHAR NO-UNDO.
    DEFINE VARIABLE cCmdString AS CHAR NO-UNDO.
    
    ASSIGN 
        cDbFileName = cbDrive:SCREEN-VALUE IN FRAME frmMain + 
                      cFilePath:SCREEN-VALUE + "\" +
                      cDbName:SCREEN-VALUE + ".db"
        FILE-INFO:FILE-NAME = cDbFileName.
    IF FILE-INFO:FULL-PATHNAME NE ? THEN DO:
        MESSAGE 
            "The database name you're trying to create already exists."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    RUN pVerifyStructureFiles.
    
    IF SUBSTRING(cDbName:SCREEN-VALUE,1,3) EQ "aud" 
    OR SUBSTRING(cDbName:SCREEN-VALUE, LENGTH(cDbName:SCREEN-VALUE),1) EQ "a" THEN DO: 
        ASSIGN 
            cCmdString = cbDrive:SCREEN-VALUE + " && " +
                         "CD " + cFilePath:SCREEN-VALUE + " && " + 
                         cDLCDir + "\bin\prostrct create " + cDbName:SCREEN-VALUE + " " +
                         "audit.st" + " && " +
                         cDLCDir + "\bin\proutil " + cDbName:SCREEN-VALUE + " -C EnableLargeFiles" +
                         " && EXIT"
                         . 
        OS-COMMAND VALUE(cCmdString).  
    END.
    ELSE DO:
        ASSIGN 
            cCmdString = cbDrive:SCREEN-VALUE + " && " +
                         "CD " + cFilePath:SCREEN-VALUE + " && " + 
                         cDLCDir + "\bin\prostrct create " + cDbName:SCREEN-VALUE + " " +
                         "asi.st" + " && " +
                         cDLCDir + "\bin\proutil " + cDbName:SCREEN-VALUE + " -C EnableLargeFiles" + 
                         " && EXIT"
                         . 
        OS-COMMAND VALUE(cCmdString).  
    END.    
    
    IF rsCreateFrom:SCREEN-VALUE EQ "Empty" THEN DO:
        ASSIGN 
            cCmdString = cbDrive:SCREEN-VALUE + " && " +
                         "CD " + cFilePath:SCREEN-VALUE + " && " + 
                         cDLCDir + "\bin\procopy " + cDLCDir + "\empty4 " + cDbName:SCREEN-VALUE + 
                         " && EXIT"
                         . 
        OS-COMMAND VALUE(cCmdString).  
    END.
    ELSE DO:
        
        ASSIGN 
            cCmdString = cbDrive:SCREEN-VALUE + " && " +
                         "CD " + cFilePath:SCREEN-VALUE + " && " + 
                         cDLCDir + "\bin\prorest " + cDbName:SCREEN-VALUE + " " + cBackupFile:SCREEN-VALUE + 
                         " && EXIT"
                         . 
        OS-COMMAND VALUE(cCmdString).  
    END.
                
         

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateMissingTTentries C-Win 
PROCEDURE pCreateMissingTTentries :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCtr AS INT NO-UNDO.
    DO iCtr = 1 TO NUM-ENTRIES(slDbList:LIST-ITEMS IN FRAME frmMain):
        FIND FIRST ttConfig WHERE 
            ttConfig.cDbName EQ ENTRY(iCtr,slDbList:LIST-ITEMS) AND 
            ttConfig.cVarName EQ "maxusers"
            NO-ERROR.
        IF NOT AVAIL ttConfig THEN DO:
            CREATE ttConfig.
            ASSIGN 
                ttConfig.cDbName = ENTRY(iCtr,slDbList:LIST-ITEMS) 
                ttConfig.cVarName = "maxusers"
                ttConfig.cVarValue = "500".
        END.
        FIND FIRST ttConfig WHERE 
            ttConfig.cDbName EQ ENTRY(iCtr,slDbList:LIST-ITEMS) AND 
            ttConfig.cVarName EQ "maxservers"
            NO-ERROR.
        IF NOT AVAIL ttConfig THEN DO:
            CREATE ttConfig.
            ASSIGN 
                ttConfig.cDbName = ENTRY(iCtr,slDbList:LIST-ITEMS) 
                ttConfig.cVarName = "maxservers"
                ttConfig.cVarValue = "50".
        END.
        FIND FIRST ttServer WHERE 
            ttServer.cDbName EQ ENTRY(iCtr,slDbList:LIST-ITEMS) AND
            ttServer.cServerName EQ "defaultservergroup" AND   
            ttServer.cVarName EQ "maxclientsperserver"
            NO-ERROR.
        IF NOT AVAIL ttServer THEN DO:
            CREATE ttServer.
            ASSIGN 
                ttServer.cDbName = ENTRY(iCtr,slDbList:LIST-ITEMS)
                ttServer.cServerName = "defaultservergroup" 
                ttServer.cVarName = "maxclientsperserver"
                ttServer.cVarValue = "10".
        END.
        FIND FIRST ttServer WHERE 
            ttServer.cDbName EQ ENTRY(iCtr,slDbList:LIST-ITEMS) AND
            ttServer.cServerName EQ "defaultservergroup" AND   
            ttServer.cVarName EQ "minclientsperserver"
            NO-ERROR.
        IF NOT AVAIL ttServer THEN DO:
            CREATE ttServer.
            ASSIGN 
                ttServer.cDbName = ENTRY(iCtr,slDbList:LIST-ITEMS)
                ttServer.cServerName = "defaultservergroup" 
                ttServer.cVarName = "minclientsperserver"
                ttServer.cVarValue = "3".
        END.

        FIND FIRST ttServer WHERE 
            ttServer.cDbName EQ ENTRY(iCtr,slDbList:LIST-ITEMS) AND
            ttServer.cServerName EQ ENTRY(iCtr,slDbList:LIST-ITEMS) + "sql"
            NO-ERROR.
        IF AVAIL ttServer THEN DO:
            FIND FIRST ttServer WHERE 
                ttServer.cDbName EQ ENTRY(iCtr,slDbList:LIST-ITEMS) AND
                ttServer.cServerName EQ ENTRY(iCtr,slDbList:LIST-ITEMS) + "sql" AND   
                ttServer.cVarName EQ "maxclientsperserver"
                NO-ERROR.
            IF NOT AVAIL ttServer THEN DO:
                CREATE ttServer.
                ASSIGN 
                    ttServer.cDbName = ENTRY(iCtr,slDbList:LIST-ITEMS)
                    ttServer.cServerName = ENTRY(iCtr,slDbList:LIST-ITEMS) + "sql"   
                    ttServer.cVarName = "maxclientsperserver"
                    ttServer.cVarValue = "10".
            END.
            FIND FIRST ttServer WHERE 
                ttServer.cDbName EQ ENTRY(iCtr,slDbList:LIST-ITEMS) AND
                ttServer.cServerName EQ ENTRY(iCtr,slDbList:LIST-ITEMS) + "sql" AND   
                ttServer.cVarName EQ "numberofservers"
                NO-ERROR.
            IF NOT AVAIL ttServer THEN DO:
                CREATE ttServer.
                ASSIGN 
                    ttServer.cDbName = ENTRY(iCtr,slDbList:LIST-ITEMS)
                    ttServer.cServerName = ENTRY(iCtr,slDbList:LIST-ITEMS) + "sql"   
                    ttServer.cVarName = "numberofservers"
                    ttServer.cVarValue = "1".
            END.
        END.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayDbList C-Win 
PROCEDURE pDisplayDbList :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lcDbList AS CHAR.
    
    FOR EACH ttDatabase WHERE 
        ttDatabase.cVarName EQ "displayname"
        BREAK BY ttDatabase.cDbName:
        IF FIRST-OF(ttDatabase.cDbName) THEN ASSIGN 
            lcDbList = lcDbList + ttDatabase.cVarValue + ",".
    END.
    ASSIGN 
        lcDbList = trim(lcDbList,",")
        slDbList:LIST-ITEMS IN FRAME frmMain = lcDbList.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetOtherValues C-Win 
PROCEDURE pGetOtherValues :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcDbName AS CHAR NO-UNDO.
    DEFINE VARIABLE cRaw AS CHAR NO-UNDO.
    DEFINE VARIABLE iLoginCt AS INT NO-UNDO.
    DEFINE VARIABLE iLogoutCt AS INT NO-UNDO.
    DEFINE VAR iLine AS INT NO-UNDO.
    DEFINE VAR iTestLine AS INT NO-UNDO.
    
    INPUT FROM VALUE(cbDrive:SCREEN-VALUE IN FRAME frmMain + cFilePath:SCREEN-VALUE + 
                     "\" + cDbName:SCREEN-VALUE + ".lg").
    REPEAT:
        IMPORT UNFORMATTED cRaw.
        ASSIGN 
            iLine = iLine + 1.
        IF INDEX(cRaw,"Multi-user session begin") NE 0 THEN ASSIGN 
            iTestLine = iLine.
    END.
    ASSIGN iLine = 0.
    INPUT CLOSE.

    INPUT FROM VALUE(cbDrive:SCREEN-VALUE IN FRAME frmMain + cFilePath:SCREEN-VALUE + 
                     "\" + cDbName:SCREEN-VALUE + ".lg").
    REPEAT:
        IMPORT UNFORMATTED cRaw.
        ASSIGN 
            iLine = iLine + 1.
        IF iLine LT iTestLine THEN NEXT.
        IF INDEX(cRaw,"Login usernum") NE 0 THEN ASSIGN 
            iLoginCt = iLoginCt + 1.
        ELSE IF INDEX(cRaw,"Logout usernum") NE 0 
        OR (INDEX(cRaw,"Logout by user") NE 0 AND INDEX(cRaw,"SQLSRV2") NE 0) THEN ASSIGN 
       
            iLogoutCt = iLogoutCt + 1.
    END.
    INPUT CLOSE.
    ASSIGN iConnections:SCREEN-VALUE IN FRAME frmMain = STRING(iLoginCt - iLogoutCt).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetScreenValuesFromFileSystem C-Win 
PROCEDURE pGetScreenValuesFromFileSystem :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcDrive AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER ipcFilePath AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER ipcDbName AS CHAR NO-UNDO.
    
    DEFINE VARIABLE cLockFile AS CHAR NO-UNDO.
    
    FIND FIRST ttDatabase WHERE 
        ttDatabase.cDbName EQ ipcDbName AND 
        ttDatabase.cVarName EQ "databaseName"
        NO-ERROR.
    IF NOT AVAIL ttDatabase THEN DO:
        MESSAGE 
            "Unable to locate the required Database entry in config file"
            VIEW-AS ALERT-BOX.
        RETURN.
    END.
    ELSE DO:
        /* Phase 1 - is dbMan handling it? */
        /* Phase 2 - if not dbman, check for .lk file (started with prosrv) */
        ASSIGN 
            cLockFile = ttDatabase.cVarValue + ".lk"
            FILE-INFO:FILE-NAME = cLockFile
            tbDbRunning:CHECKED IN FRAME frmMain = FILE-INFO:FILE-TYPE BEGINS "F".
        FIND FIRST ttServer WHERE 
            ttServer.cDbName EQ ipcDbName AND 
            ttServer.cServerName = "defaultservergroup" AND 
            ttServer.cVarName = "port"
            NO-ERROR.
        IF NOT AVAIL ttServer THEN DO:
            MESSAGE 
                "Unable to locate the required ServerGroup in the config file."
                VIEW-AS ALERT-BOX.
            RETURN.
        END.
        ELSE DO:  
/*            RUN pConnectDB (ipcDbName,                  */
/*                            INTEGER(ttServer.cVarValue).*/
/*            RUN pGetConnectionCount (OUTPUT iUsers).    */
/*            RUN pDisconnectDb (ipcDbName).              */
        END.
            
//            'SELECT * FROM PUB."_Connect" WHERE "_Connect-Type" IS NOT NULL AND "_Connect-ClientType" = 'ABL' OR "_Connect-ClientType" = 'SQLC';' + CHR(10).
                    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetScreenValuesFromTempTables C-Win 
PROCEDURE pGetScreenValuesFromTempTables :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcDbName AS CHAR.
    
    DEFINE VARIABLE cTestString AS CHAR NO-UNDO.

    ASSIGN     
        tbSql:CHECKED IN FRAME frmMain = FALSE.

    FOR EACH ttDatabase WHERE 
        ttDatabase.cDbName = ipcDbName:
        CASE ttDatabase.cVarName:
            WHEN "autostart" THEN ASSIGN 
                tbAutoStart:CHECKED = LOGICAL(ttDatabase.cVarValue).
            WHEN "databasename" THEN ASSIGN 
                cbDrive:SCREEN-VALUE = SUBSTRING(ttDatabase.cVarValue,1,2)
                cTestString = SUBSTRING(ttDatabase.cVarValue,3)
                cTestString = REPLACE(cTestString,"\" + ipcDbName,"")
                cFilePath:SCREEN-VALUE = cTestString.
            WHEN "displayName" THEN ASSIGN 
                cDbName:SCREEN-VALUE = ttDatabase.cVarValue.
        END CASE.
    END.
    FOR EACH ttConfig WHERE 
        ttConfig.cDbName = ipcDbName:
        CASE ttConfig.cVarName:
            WHEN "blocksindatabasebuffers" THEN ASSIGN 
                iBufferBlocks:SCREEN-VALUE = ttConfig.cVarValue.
            WHEN "locktableentries" THEN ASSIGN 
                iLockTable:SCREEN-VALUE = ttConfig.cVarValue.
            WHEN "otherargs" THEN ASSIGN 
                cOtherParms = ttConfig.cVarValue.
            WHEN "watchdogprocess" THEN ASSIGN 
                tbWatchdog:CHECKED = LOGICAL(ttConfig.cVarValue).
            WHEN "maxusers" THEN ASSIGN 
                iMaxUsers:SCREEN-VALUE = ttConfig.cVarValue.
        END CASE.
    END.
    FOR EACH ttServer WHERE 
        ttServer.cDbName = ipcDbName:
        IF ttServer.cServerName EQ "defaultservergroup" THEN DO:
            CASE ttServer.cVarName:
                WHEN "messagebuffersize" THEN ASSIGN 
                    iBufferSize:SCREEN-VALUE = ttServer.cVarValue.
                WHEN "port" THEN ASSIGN 
                    iPort:SCREEN-VALUE = ttServer.cVarValue.
                WHEN "type" THEN ASSIGN
                    ttServer.cVarValue = "4gl". 
            END CASE.
        END.
        ELSE IF INDEX(ttServer.cServerName,"sql") NE 0 THEN DO:
            tbSql:CHECKED = TRUE.
            CASE ttServer.cVarName:
                WHEN "port" THEN ASSIGN 
                    iSQLPort:SCREEN-VALUE = ttServer.cVarValue.
                WHEN "type" THEN ASSIGN
                    ttServer.cVarValue = "sql". 
            END CASE.
        END.
    END.
    
    IF iMaxUsers:SCREEN-VALUE EQ "" THEN ASSIGN 
        iMaxUsers:SCREEN-VALUE = "20".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInitialize C-Win 
PROCEDURE pInitialize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    ASSIGN 
        cHostName = OS-GETENV("COMPUTERNAME")
        c-win:TITLE = "Database Configuration Maintenance" + " - Hostname:" + cHostName
        lAdminServerRunning = fAdminServerStatus().
        
    GET-KEY-VALUE SECTION 'Startup' KEY 'DLC' VALUE cDLCDir.

    EMPTY TEMP-TABLE ttConfig.
    EMPTY TEMP-TABLE ttDatabase.
    EMPTY TEMP-TABLE ttPorts.
    EMPTY TEMP-TABLE ttServer.
    
    
    RUN pReadExistingPropsFile.
    RUN pDisplayDbList.
    RUN pCreateMissingTTentries.
    
    ASSIGN 
        slDbList:SCREEN-VALUE IN FRAME frmMain = ENTRY(1,slDbList:LIST-ITEMS)
        cAdminServerPort = fGetAdminServerPort()
        iAdminPort:SCREEN-VALUE = cAdminServerPort
        tbAdminService:CHECKED = lAdminServerRunning.

    IF tbAdminService:CHECKED THEN ASSIGN 
        bStopAdmin:SENSITIVE = TRUE 
        bStartAdmin:SENSITIVE = FALSE.                                 
    ELSE ASSIGN 
        bStopAdmin:SENSITIVE = FALSE  
        bStartAdmin:SENSITIVE = TRUE.                                 

    RUN pClearDisplay.
    APPLY 'value-changed' TO slDbList.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOpenForEdit C-Win 
PROCEDURE pOpenForEdit :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMode AS CHAR NO-UNDO.
    
    IF ipcMode EQ "Add" THEN ASSIGN 
        cDbName:SENSITIVE IN FRAME frmMain = TRUE 
        cFilePath:SENSITIVE = TRUE
        cOtherParms:SENSITIVE = TRUE
        cBackupFile:SENSITIVE = TRUE
        iPort:SENSITIVE = TRUE
        iBufferBlocks:SENSITIVE = TRUE
        iLockTable:SENSITIVE = TRUE
        iMaxUsers:SENSITIVE = TRUE
        iBufferSize:SENSITIVE = TRUE
        iSQLPort:SENSITIVE = TRUE
        tbAutoStart:SENSITIVE = TRUE
        tbWatchdog:SENSITIVE = TRUE
        tbSQL:SENSITIVE = TRUE
        tbCreateDB:SENSITIVE = TRUE
        cbDrive:SENSITIVE = TRUE
        rsCreateFrom:SENSITIVE = TRUE
        cDbName:SCREEN-VALUE = "" 
        cFilePath:SCREEN-VALUE = "\asigui\Databases\"
        cOtherParms:SCREEN-VALUE = ""
        cBackupFile:SCREEN-VALUE = "C:\Backups\Databases\"
        iPort:SCREEN-VALUE = ""
        iBufferBlocks:SCREEN-VALUE = ""
        iLockTable:SCREEN-VALUE = "96000"
        iMaxUsers:SCREEN-VALUE = "500"
        iBufferSize:SCREEN-VALUE = "8192"
        iSQLPort:SCREEN-VALUE = ""
        tbAutoStart:CHECKED = TRUE 
        tbWatchdog:CHECKED = TRUE 
        tbSQL:CHECKED = TRUE 
        tbCreateDB:CHECKED = TRUE 
        cbDrive = "C:"
        rsCreateFrom = "Backup"
        tbDbRunning:CHECKED = FALSE 
        tbDbRunning:SENSITIVE = FALSE  
        iConnections:SCREEN-VALUE = ""
        .
    ELSE IF ipcMode EQ "Update" THEN ASSIGN  
        cFilePath:SENSITIVE = TRUE
        cOtherParms:SENSITIVE = TRUE
        iPort:SENSITIVE = TRUE
        iBufferBlocks:SENSITIVE = TRUE
        iLockTable:SENSITIVE = TRUE
        iMaxUsers:SENSITIVE = TRUE
        iBufferSize:SENSITIVE = TRUE
        iSQLPort:SENSITIVE = TRUE
        tbAutoStart:SENSITIVE = TRUE
        tbWatchdog:SENSITIVE = TRUE
        tbSQL:SENSITIVE = TRUE
        cbDrive:SENSITIVE = TRUE
        tbCreateDB:SENSITIVE = FALSE
        .
    ELSE IF ipcMode EQ "Save" THEN ASSIGN 
        cDbName:SENSITIVE = FALSE 
        cFilePath:SENSITIVE = FALSE
        cOtherParms:SENSITIVE = FALSE
        cBackupFile:SENSITIVE = FALSE
        iPort:SENSITIVE = FALSE
        iBufferBlocks:SENSITIVE = FALSE
        iLockTable:SENSITIVE = FALSE
        iMaxUsers:SENSITIVE = FALSE
        iBufferSize:SENSITIVE = FALSE
        iSQLPort:SENSITIVE = FALSE
        tbAutoStart:SENSITIVE = FALSE
        tbWatchdog:SENSITIVE = FALSE
        tbSQL:SENSITIVE = FALSE
        tbCreateDB:SENSITIVE = FALSE
        cbDrive:SENSITIVE = FALSE
        rsCreateFrom:SENSITIVE = FALSE
        lUpdateMode = FALSE 
        .

    IF ipcMode EQ "Add" THEN 
        APPLY 'entry' TO cDbName.
    ELSE IF ipcMode EQ "Update" THEN 
        APPLY 'entry' TO cbDrive.
    ELSE IF ipcMode EQ "Save" THEN 
        APPLY 'entry' TO bAdd.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReadExistingPropsFile C-Win 
PROCEDURE pReadExistingPropsFile :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFileLoc AS CHAR NO-UNDO.
    DEFINE VARIABLE iStartPos AS INT.
    DEFINE VARIABLE iEntryCtr AS INT.
    DEFINE VARIABLE cRawLine AS CHAR.
    DEFINE VARIABLE lInConfig AS LOG.
    DEFINE VARIABLE lInDatabase AS LOG.
    DEFINE VARIABLE lInServer AS LOG.
    DEFINE VARIABLE clConfigName AS CHAR.
    DEFINE VARIABLE clDbName AS CHAR.
    DEFINE VARIABLE clServerName AS CHAR.
    
    ASSIGN 
        cFileLoc = cDLCDir + "\properties\conmgr.properties".

    INPUT FROM VALUE(cFileLoc).
    REPEAT:
        IMPORT UNFORMATTED cRawLine.
        IF SUBSTRING(cRawLine,1,1) EQ "[" THEN DO:
            IF SUBSTRING(cRawLine,1,2) EQ "[c" THEN DO:
                ASSIGN 
                    lInConfig = TRUE
                    lInDatabase = FALSE 
                    lInServer = FALSE
                    clDbName = ENTRY(2,cRawLine,".")
                    cConfigList = cConfigList + clDbName + ",". 
            END.
            IF SUBSTRING(cRawLine,1,2) EQ "[d" THEN DO:
                ASSIGN 
                    lInConfig = FALSE 
                    lInDatabase = TRUE  
                    lInServer = FALSE
                    clDbName = TRIM(ENTRY(2,cRawLine,"."),"]")
                    cDatabaseList = cDatabaseList + clDbName + ",".
                   
            END.
            IF SUBSTRING(cRawLine,1,2) EQ "[s" THEN DO:
                ASSIGN 
                    lInConfig = FALSE 
                    lInDatabase = FALSE 
                    lInServer = TRUE
                    clDbName = ENTRY(2,cRawLine,".")
                    clServerName = TRIM(ENTRY(4,cRawLine,"."),"]")
                    cServerGroupList = clDbName + "|" + clServerName + "," .
            END.
        END.
        ELSE IF cRawLine EQ "" THEN DO:
                ASSIGN 
                    lInConfig = FALSE 
                    lInDatabase = FALSE 
                    lInServer = FALSE.
        END.
        ELSE IF SUBSTRING(cRawLine,1,4) EQ "    " THEN DO:
            IF lInConfig THEN DO:
                CREATE ttConfig.
                ASSIGN 
                    ttConfig.cDbName = clDbName 
                    ttConfig.cVarName = ENTRY(1,TRIM(cRawLine),"=")
                    ttConfig.cVarValue = ENTRY(2,TRIM(cRawLine),"=")
                    .
            END.
            ELSE IF lInDatabase THEN DO:
                CREATE ttDatabase.
                ASSIGN 
                    ttDatabase.cDbName = clDbName 
                    ttDatabase.cVarName = ENTRY(1,TRIM(cRawLine),"=")
                    ttDatabase.cVarValue = ENTRY(2,TRIM(cRawLine),"=")
                    .
            END.
            ELSE IF lInServer THEN DO:
                CREATE ttServer.
                ASSIGN 
                    ttServer.cDbName = clDbName
                    ttServer.cServerName = clServerName 
                    ttServer.cVarName = ENTRY(1,TRIM(cRawLine),"=")
                    ttServer.cVarValue = ENTRY(2,TRIM(cRawLine),"=")
                    .
                IF ttServer.cVarName EQ "Port" THEN DO:
                    CREATE ttPorts.
                    ASSIGN 
                        ttPorts.iPort = INTEGER(ttServer.cVarValue).
                END.
            END.
        END.
    END.
    INPUT CLOSE.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveConfigFile C-Win 
PROCEDURE pSaveConfigFile :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    OS-COPY VALUE(cDLCDir + "\properties\conmgr.properties") VALUE(cDLCDir + "\properties\conmgr.properties.bak" + fGetDateTimeForExtension()). 
    RUN pBuildConfig.
    
    OUTPUT TO VALUE(cDLCDir + "\properties\conmgr.properties").
    
    PUT UNFORMATTED cFileHeader + CHR(10).
    
    FOR EACH ttConfig
        BREAK BY ttConfig.cDbName BY ttConfig.cVarName:
        IF FIRST-OF(ttConfig.cDbName) THEN 
            PUT UNFORMATTED "[configuration." + LC(ttConfig.cDbName) + ".defaultconfiguration]" + CHR(10).
        PUT UNFORMATTED "    " + ttConfig.cVarName + "=" + ttConfig.cVarValue + CHR(10).
        IF LAST-OF(ttConfig.cDbName) THEN
            PUT UNFORMATTED CHR(10).
    END.
    
    FOR EACH ttDatabase
        BREAK BY ttDatabase.cDbName BY ttDatabase.cVarName:
        IF FIRST-OF(ttDatabase.cDbName) THEN 
            PUT UNFORMATTED "[database." + LC(ttDatabase.cDbName) + "]" + CHR(10).
        PUT UNFORMATTED "    " + ttDatabase.cVarName + "=" + ttDatabase.cVarValue + CHR(10).
        IF LAST-OF(ttDatabase.cDbName) THEN
            PUT UNFORMATTED CHR(10).
    END.
            
    PUT UNFORMATTED "[environment]" + CHR(10) + CHR(10).
    
    FOR EACH ttServer
        BREAK BY ttServer.cDbName BY ttServer.cServerName BY ttServer.cVarName:
        IF FIRST-OF(ttServer.cServerName) THEN 
            PUT UNFORMATTED "[servergroup." + LC(ttServer.cDbName) + ".defaultconfiguration." + LC(ttServer.cServerName) + "]" + CHR(10).
        PUT UNFORMATTED "    " + ttServer.cVarName + "=" + ttServer.cVarValue + CHR(10).
        IF LAST-OF(ttServer.cServerName) THEN
            PUT UNFORMATTED CHR(10).
    END.
        
    PUT UNFORMATTED cFileFooter + CHR(10).
    
    OUTPUT CLOSE.
    
    RUN pInitialize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveExistingDB C-Win 
PROCEDURE pSaveExistingDB :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR cConfigVarList AS CHAR INITIAL "watchdogprocess,blocksindatabasebuffers,locktableentries,otherargs" NO-UNDO.
    DEF VAR cDbVarList AS CHAR INITIAL "databasename,autostart" NO-UNDO.
    DEF VAR cGroupVarList AS CHAR INITIAL "port,type,messagebuffersize" NO-UNDO.    
    DEF VAR iCtr AS INT NO-UNDO.
    
    DO iCtr = 1 TO NUM-ENTRIES(cConfigVarList):
        FIND FIRST ttConfig WHERE 
            ttConfig.cDbName EQ cDbName:SCREEN-VALUE IN FRAME frmMain AND 
            ttConfig.cVarName EQ ENTRY(iCtr,cConfigVarList)
            NO-ERROR.
        IF NOT AVAIL ttConfig THEN DO:
            CREATE ttConfig.
            ASSIGN 
                ttConfig.cDbName = cDbName:SCREEN-VALUE 
                ttConfig.cVarName = ENTRY(iCtr,cConfigVarList).
        END.
        CASE ENTRY(iCtr,cConfigVarList):
            WHEN "watchdogprocess" THEN ASSIGN ttConfig.cVarValue = STRING(tbWatchdog:CHECKED,"true/false").
            WHEN "blocksindatabasebuffers" THEN ASSIGN ttConfig.cVarValue = iBufferBlocks:SCREEN-VALUE.
            WHEN "locktableentries" THEN ASSIGN ttConfig.cVarValue = iLockTable:SCREEN-VALUE.
            WHEN "otherargs" THEN ASSIGN ttConfig.cVarValue = cOtherParms:SCREEN-VALUE.
        END CASE.
    END.
        
    DO iCtr = 1 TO NUM-ENTRIES(cDbVarList):
        FIND FIRST ttDatabase WHERE 
            ttDatabase.cDbName EQ cDbName:SCREEN-VALUE IN FRAME frmMain AND 
            ttDatabase.cVarName EQ ENTRY(iCtr,cDbVarList)
            NO-ERROR.
        IF NOT AVAIL ttDatabase THEN DO:
            CREATE ttDatabase.
            ASSIGN 
                ttDatabase.cDbName = cDbName:SCREEN-VALUE 
                ttDatabase.cVarName = ENTRY(iCtr,cDbVarList).
        END.
        CASE ENTRY(iCtr,cDbVarList):
            WHEN "databasename" THEN ASSIGN ttDatabase.cVarValue = LC(cbDrive:SCREEN-VALUE) + 
                                                                 cFilePath:SCREEN-VALUE + "\" +
                                                                 cDbName:SCREEN-VALUE.
            WHEN "autostart" THEN ASSIGN ttDatabase.cVarValue = STRING(tbAutoStart:CHECKED,"true/false").
        END CASE.
    END.
    
    DO iCtr = 1 TO NUM-ENTRIES(cGroupVarList):
        FIND FIRST ttServer WHERE 
            ttServer.cDbName EQ cDbName:SCREEN-VALUE IN FRAME frmMain AND 
            ttServer.cServerName EQ "defaultservergroup" AND 
            ttServer.cVarName EQ ENTRY(iCtr,cGroupVarList) 
            NO-ERROR.
        IF NOT AVAIL ttServer THEN DO:
            CREATE ttServer.
            ASSIGN 
                ttServer.cDbName = cDbName:SCREEN-VALUE 
                ttServer.cServerName = "defaultservergroup"
                ttServer.cVarName = ENTRY(iCtr,cGroupVarList).
        END.
        CASE ENTRY(iCtr,cGroupVarList):
            WHEN "port" THEN ASSIGN ttServer.cVarValue = iPort:SCREEN-VALUE.
            WHEN "messagebuffersize" THEN ASSIGN ttServer.cVarValue = iBufferSize:SCREEN-VALUE.
            WHEN "type" THEN ASSIGN ttServer.cVarValue = "4gl".
        END CASE.
    END.
    
    IF tbSQL:CHECKED THEN DO:
        FIND FIRST ttServer WHERE 
            ttServer.cDbName EQ cDbName:SCREEN-VALUE IN FRAME frmMain AND 
            INDEX(ttServer.cServerName,"sql") NE 0
            NO-ERROR.
        IF NOT AVAIL ttServer THEN DO:
            CREATE ttServer.
            ASSIGN 
                ttServer.cDbName = cDbName:SCREEN-VALUE
                ttServer.cServerName = LC(cDbName:SCREEN-VALUE) + "sql"
                ttServer.cVarName = "configuration"
                ttServer.cVarValue = LC(cDbName:SCREEN-VALUE) + ".defaultconfiguration".
            CREATE ttServer.
            ASSIGN 
                ttServer.cDbName = cDbName:SCREEN-VALUE
                ttServer.cServerName = LC(cDbName:SCREEN-VALUE) + "sql"
                ttServer.cVarName = "displayname"
                ttServer.cVarValue = cDbName:SCREEN-VALUE + "SQL".
            CREATE ttServer.
            ASSIGN 
                ttServer.cDbName = cDbName:SCREEN-VALUE
                ttServer.cServerName = LC(cDbName:SCREEN-VALUE) + "sql"
                ttServer.cVarName = "messagebuffersize"
                ttServer.cVarValue = iBufferSize:SCREEN-VALUE.
            CREATE ttServer.
            ASSIGN 
                ttServer.cDbName = cDbName:SCREEN-VALUE
                ttServer.cServerName = LC(cDbName:SCREEN-VALUE) + "sql"
                ttServer.cVarName = "numberofservers"
                ttServer.cVarValue = "1".
            CREATE ttServer.
            ASSIGN 
                ttServer.cDbName = cDbName:SCREEN-VALUE
                ttServer.cServerName = LC(cDbName:SCREEN-VALUE) + "sql"
                ttServer.cVarName = "port"
                ttServer.cVarValue = iSQLPort:SCREEN-VALUE.
            CREATE ttServer.
            ASSIGN 
                ttServer.cDbName = cDbName:SCREEN-VALUE
                ttServer.cServerName = LC(cDbName:SCREEN-VALUE) + "sql"
                ttServer.cVarName = "type"
                ttServer.cVarValue = "sql".
            CREATE ttServer.
            ASSIGN 
                ttServer.cDbName = cDbName:SCREEN-VALUE
                ttServer.cServerName = LC(cDbName:SCREEN-VALUE) + "sql"
                ttServer.cVarName = "maxclientsperserver"
                ttServer.cVarValue = "10".
            CREATE ttServer.
            ASSIGN 
                ttServer.cDbName = cDbName:SCREEN-VALUE
                ttServer.cServerName = LC(cDbName:SCREEN-VALUE) + "sql"
                ttServer.cVarName = "minclientsperserver"
                ttServer.cVarValue = "1".
        END.
        
        DO iCtr = 1 TO NUM-ENTRIES(cGroupVarList):
            FIND FIRST ttServer WHERE 
                ttServer.cDbName EQ cDbName:SCREEN-VALUE IN FRAME frmMain AND 
                ttServer.cVarName EQ ENTRY(iCtr,cGroupVarList) AND 
                INDEX(ttServer.cServerName,"sql") NE 0
                NO-ERROR.
            IF NOT AVAIL ttServer THEN DO:
                CREATE ttServer.
                ASSIGN 
                    ttServer.cDbName = cDbName:SCREEN-VALUE 
                    ttServer.cVarName = ENTRY(iCtr,cGroupVarList).
            END.
            CASE ENTRY(iCtr,cGroupVarList):
                WHEN "port" THEN ASSIGN ttServer.cVarValue = iSQLPort:SCREEN-VALUE. 
                WHEN "messagebuffersize" THEN ASSIGN ttServer.cVarValue = iBufferSize:SCREEN-VALUE.
            END CASE.
        END.
    END.
    ELSE FOR EACH ttServer WHERE  
        ttServer.cDbName EQ cDbName:SCREEN-VALUE IN FRAME frmMain AND 
        INDEX(ttServer.cServerName,"sql") NE 0:
        DELETE ttServer.
    END.
    
    RUN pSaveConfigFile.
                        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveNewDB C-Win 
PROCEDURE pSaveNewDB :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR cConfigVarList AS CHAR INITIAL "afterimageprocess,asynchronouspagewriters,beforeimageprocess,blocksindatabasebuffers,database,displayname,locktableentries,maxservers,maxusers,monitored,otherargs,servergroups,watchdogprocess" NO-UNDO.
    DEF VAR cDbVarList AS CHAR INITIAL "autostart,configurations,databasename,defaultconfiguration,displayname,monitorlicensed" NO-UNDO.
    DEF VAR cGroupVarList AS CHAR INITIAL "configuration,displayname,maxclientsperserver,messagebuffersize,minclientsperserver,numberofservers,port,type" NO-UNDO.    
    DEF VAR iCtr AS INT NO-UNDO.

    IF tbCreateDB:CHECKED IN FRAME frmMain THEN 
        RUN pCreateDatabase.
    
    DO iCtr = 1 TO NUM-ENTRIES(cConfigVarList):
        CREATE ttConfig.
        ASSIGN 
            ttConfig.cDbName = cDbName:SCREEN-VALUE IN FRAME frmMain
            ttConfig.cVarName = ENTRY(iCtr,cConfigVarList).
        CASE ENTRY(iCtr,cConfigVarList):
            WHEN "afterimageprocess" THEN ASSIGN ttConfig.cVarValue = "true".
            WHEN "asynchronouspagewriters" THEN ASSIGN ttConfig.cVarValue = "1".
            WHEN "beforeimageprocess" THEN ASSIGN ttConfig.cVarValue = "true".
            WHEN "blocksindatabasebuffers" THEN ASSIGN ttConfig.cVarValue = iBufferBlocks:SCREEN-VALUE IN FRAME frmMain.
            WHEN "database" THEN ASSIGN ttConfig.cVarValue = LC(cDbName:SCREEN-VALUE).
            WHEN "displayname" THEN ASSIGN ttConfig.cVarValue = "defaultconfiguration".
            WHEN "locktableentries" THEN ASSIGN ttConfig.cVarValue = iLockTable:SCREEN-VALUE.
            WHEN "maxservers" THEN ASSIGN ttConfig.cVarValue = "50".
            WHEN "maxusers" THEN ASSIGN ttConfig.cVarValue = "500".
            WHEN "monitored" THEN ASSIGN ttConfig.cVarValue = "true".
            WHEN "otherargs" THEN ASSIGN ttConfig.cVarValue = cOtherParms:SCREEN-VALUE.
            WHEN "servergroups" THEN ASSIGN ttConfig.cVarValue = LC(cDbName:SCREEN-VALUE) + "." +
                                                                 "defaultconfiguration" + "." +
                                                                 "defaultservergroup" + 
                                                                 IF tbSQL:CHECKED THEN 
                                                                    "," + LC(cDbName:SCREEN-VALUE) + "." +
                                                                    "defaultconfiguration" + "." +
                                                                    LC(cDbName:SCREEN-VALUE) + "sql"
                                                                 ELSE "".
            WHEN "watchdogprocess" THEN ASSIGN ttConfig.cVarValue = STRING(tbWatchdog:CHECKED,"true/false").
        END CASE.
    END.
        
    DO iCtr = 1 TO NUM-ENTRIES(cDbVarList):
        CREATE ttDatabase.
        ASSIGN 
            ttDatabase.cDbName = cDbName:SCREEN-VALUE 
            ttDatabase.cVarName = ENTRY(iCtr,cDbVarList).
        CASE ENTRY(iCtr,cDbVarList):
            WHEN "autostart" THEN ASSIGN ttDatabase.cVarValue = STRING(tbAutoStart:CHECKED,"true/false").
            WHEN "configurations" THEN ASSIGN ttDatabase.cVarValue = LC(cDbName:SCREEN-VALUE) + ".defaultconfiguration".
            WHEN "databasename" THEN ASSIGN ttDatabase.cVarValue = LC(cbDrive:SCREEN-VALUE) + 
                                                                   cFilePath:SCREEN-VALUE + "\" +
                                                                   cDbName:SCREEN-VALUE.
            WHEN "defaultconfiguration" THEN ASSIGN ttDatabase.cVarValue = LC(cDbName:SCREEN-VALUE) + ".defaultconfiguration".
            WHEN "displayname" THEN ASSIGN ttDatabase.cVarValue = cDbName:SCREEN-VALUE. 
            WHEN "monitorlicensed" THEN ASSIGN ttDatabase.cVarValue = "true".
        END CASE.
    END.
    
    DO iCtr = 1 TO NUM-ENTRIES(cGroupVarList):
        IF ENTRY(iCtr,cGroupVarList) EQ "numberofservers" THEN  
            NEXT.
        CREATE ttServer.
        ASSIGN 
            ttServer.cDbName = cDbName:SCREEN-VALUE 
            ttServer.cServerName = "defaultservergroup"
            ttServer.cVarName = ENTRY(iCtr,cGroupVarList).
        CASE ENTRY(iCtr,cGroupVarList):
            WHEN "configuration" THEN ASSIGN ttServer.cVarValue = LC(cDbName:SCREEN-VALUE) + ".defaultconfiguration".
            WHEN "displayname" THEN ASSIGN ttServer.cVarValue = "defaultservergroup". 
            WHEN "maxclientsperserver" THEN ASSIGN ttServer.cVarValue = "10".
            WHEN "messagebuffersize" THEN ASSIGN ttServer.cVarValue = iBufferSize:SCREEN-VALUE.
            WHEN "minclientsperserver" THEN ASSIGN ttServer.cVarValue = "3".
            WHEN "port" THEN ASSIGN ttServer.cVarValue = iPort:SCREEN-VALUE.
            WHEN "type" THEN ASSIGN ttServer.cVarValue = "4gl".
        END CASE.
    END.
    
    IF tbSQL:CHECKED THEN DO iCtr = 1 TO NUM-ENTRIES(cGroupVarList):
        CREATE ttServer.
        ASSIGN 
            ttServer.cDbName = cDbName:SCREEN-VALUE
            ttServer.cServerName = LC(cDbName:SCREEN-VALUE) + "sql"
            ttServer.cVarName = ENTRY(iCtr,cGroupVarList).
        CASE ENTRY(iCtr,cGroupVarList):
            WHEN "configuration" THEN ASSIGN ttServer.cVarValue = LC(cDbName:SCREEN-VALUE) + ".defaultconfiguration".
            WHEN "displayname" THEN ASSIGN ttServer.cVarValue = cDbName:SCREEN-VALUE + "SQL".
            WHEN "maxclientsperserver" THEN ASSIGN ttServer.cVarValue = "10".
            WHEN "messagebuffersize" THEN ASSIGN ttServer.cVarValue = iBufferSize:SCREEN-VALUE.
            WHEN "minclientsperserver" THEN ASSIGN ttServer.cVarValue = "1".
            WHEN "numberofservers" THEN ASSIGN ttServer.cVarValue = "1".
            WHEN "port" THEN ASSIGN ttServer.cVarValue = iSQLPort:SCREEN-VALUE. 
            WHEN "type" THEN ASSIGN ttServer.cVarValue = "sql".
        END CASE.
    END.
    ELSE FOR EACH ttServer WHERE  
        ttServer.cDbName EQ cDbName:SCREEN-VALUE IN FRAME frmMain AND 
        INDEX(ttServer.cServerName,"sql") NE 0:
        DELETE ttServer.
    END.
    
    RUN pSaveConfigFile.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pStartDatabase C-Win 
PROCEDURE pStartDatabase :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcDbLongName AS CHAR NO-UNDO.
    
    DEFINE VARIABLE cLockFile AS CHAR NO-UNDO.
    DEFINE VARIABLE lOnline AS LOG NO-UNDO.
    DEFINE VARIABLE cStartString AS CHAR NO-UNDO.
    DEFINE VARIABLE iWaitCount AS INT NO-UNDO.
    
    ASSIGN
        cLockFile = ipcDbLongName + ".lk" 
        lOnline = SEARCH(cLockFile) NE ?.
    ASSIGN 
        cStartString = cDlcDir + "\bin\dbman" + 
            " -host " + cHostName + 
            " -port " + cAdminServerPort +  
            " -database " + cDbName:SCREEN-VALUE IN FRAME frmMain + 
            " -start"
            .
    IF NOT lOnline THEN DO:
        STATUS DEFAULT "Starting database server for " + cDbName:SCREEN-VALUE.
        
        OS-COMMAND SILENT VALUE(cStartString).
       
        /* May have to wait for DB to start up */
        WaitBlock:
        DO WHILE SEARCH(cLockFile) EQ ?:
            ASSIGN 
                iWaitCount = iWaitCount + 1.
            PAUSE 2 NO-MESSAGE.
            STATUS DEFAULT "Waiting for creation of lock file (" + STRING(iWaitCount) + ")".
            IF iWaitCount EQ 15 THEN 
            DO:
                LEAVE waitblock.
            END.
        END.
        IF SEARCH(cLockFile) EQ ? THEN 
        DO:
            MESSAGE 
                "Unable to start server for " + cDbName:SCREEN-VALUE + ".".
            RETURN.
        END.
        ELSE ASSIGN 
            tbDbRunning:CHECKED = TRUE
            bStopDb:SENSITIVE = TRUE
            bStartDb:SENSITIVE = FALSE 
            .
        STATUS DEFAULT "".
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pStopDatabase C-Win 
PROCEDURE pStopDatabase :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcDbLongName AS CHAR NO-UNDO.
    
    DEFINE VARIABLE cLockFile AS CHAR NO-UNDO.
    DEFINE VARIABLE lOnline AS LOG NO-UNDO.
    DEFINE VARIABLE cStopString AS CHAR NO-UNDO.
    DEFINE VARIABLE iWaitCount AS INT NO-UNDO.
    
    ASSIGN
        cLockFile = ipcDbLongName + ".lk" 
        lOnline = SEARCH(cLockFile) NE ?.

    ASSIGN 
        cStopString = cDlcDir + "\bin\dbman" + 
            " -host " + cHostName + 
            " -port " + cAdminServerPort +  
            " -database " + cDbName:SCREEN-VALUE IN FRAME frmMain + 
            " -stop"
            .
    IF lOnline THEN DO:
        STATUS DEFAULT "Stopping database server for " + cDbName:SCREEN-VALUE.
        
        OS-COMMAND SILENT VALUE(cStopString).
       
        /* May have to wait for DB to shut down */
        WaitBlock:
        DO WHILE SEARCH(cLockFile) NE ?:
            ASSIGN 
                iWaitCount = iWaitCount + 1.
            PAUSE 2 NO-MESSAGE.
            STATUS DEFAULT "Waiting for removal of lock file (" + STRING(iWaitCount) + ")".
            IF iWaitCount EQ 5 THEN 
            DO:
                LEAVE waitblock.
            END.
        END.
        IF SEARCH(cLockFile) NE ? THEN 
        DO:
            STATUS DEFAULT "Can't stop with dbMan process.  Attempting proshut.".
            ASSIGN 
                cStopString = cbDrive:SCREEN-VALUE + 
                                "&& CD " + cFilePath:SCREEN-VALUE + 
                                " && " + cDLCdir + "\bin\proshut -by " + cDbName:SCREEN-VALUE.
            OS-COMMAND VALUE(cStopString).
           
            /* May have to wait for DB to shut down */
            WaitBlock:
            DO WHILE SEARCH(cLockFile) NE ?:
                ASSIGN 
                    iWaitCount = iWaitCount + 1.
                PAUSE 2 NO-MESSAGE.
                STATUS DEFAULT "Waiting for removal of lock file (" + STRING(iWaitCount) + ")".
                IF iWaitCount EQ 5 THEN 
                DO:
                    LEAVE waitblock.
                END.
            END.
        END.
        IF SEARCH(cLockFile) NE ? THEN 
        DO:
            MESSAGE 
                "Unable to shut down server for " + cDbName:SCREEN-VALUE + "." 
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
        ELSE ASSIGN 
            tbDbRunning:CHECKED = FALSE
            bStopDb:SENSITIVE = FALSE 
            bStartDb:SENSITIVE = TRUE 
            .
        STATUS DEFAULT "".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateDB C-Win 
PROCEDURE pUpdateDB :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pVerifyStructureFiles C-Win 
PROCEDURE pVerifyStructureFiles :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStFileName AS CHAR NO-UNDO.
    
    ASSIGN 
        cStFileName = cbDrive:SCREEN-VALUE IN FRAME frmMain + 
                      cFilePath:SCREEN-VALUE + "\asi.st"
        FILE-INFO:FILE-NAME = cStFileName.
    IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
        OUTPUT TO VALUE(cStFileName).
        PUT UNFORMATTED '#' + CHR(10).
        PUT UNFORMATTED 'b .' + CHR(10).
        PUT UNFORMATTED 'd "Schema Area":6,32;1 .' + CHR(10).
        PUT UNFORMATTED 'd "Data":7,64;64 .' + CHR(10).
        PUT UNFORMATTED 'd "Index":8,32;8 .' + CHR(10).
        PUT UNFORMATTED 'a .' + CHR(10).
        PUT UNFORMATTED '#' + CHR(10).
        OUTPUT CLOSE.
    END.
                      
    ASSIGN 
        cStFileName = cbDrive:SCREEN-VALUE IN FRAME frmMain + 
                      cFilePath:SCREEN-VALUE + "\audit.st"
        FILE-INFO:FILE-NAME = cStFileName.
    IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
        OUTPUT TO VALUE(cStFileName).
        PUT UNFORMATTED '#' + CHR(10).
        PUT UNFORMATTED 'b .' + CHR(10).
        PUT UNFORMATTED 'd "Schema Area":6,32;1 .' + CHR(10).
        PUT UNFORMATTED 'd "Audit":9,64;64 .' + CHR(10).
        PUT UNFORMATTED 'a .' + CHR(10).
        PUT UNFORMATTED '#' + CHR(10).
        OUTPUT CLOSE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAdminServerStatus C-Win 
FUNCTION fAdminServerStatus RETURNS LOGICAL
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cRaw AS CHAR NO-UNDO.
    
    OS-COMMAND SILENT VALUE('tasklist /fi "imagename eq admsrvc.exe" > c:\tmp\tasklist.txt').
    INPUT FROM c:\tmp\tasklist.txt.
    REPEAT:
        IMPORT cRaw.
        IF cRaw BEGINS "admsrvc" THEN lResult = TRUE.
    END.
    OS-DELETE c:\tmp\tasklist.txt.
    RETURN lResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetAdminServerPort C-Win 
FUNCTION fGetAdminServerPort RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileLoc AS CHAR NO-UNDO.
    DEFINE VARIABLE cRawLine AS CHAR NO-UNDO.
    DEFINE VARIABLE lInSection AS LOG NO-UNDO.
        
    ASSIGN 
        cFileLoc = cDLCDir + "\properties\AdminServerPlugins.properties".

    INPUT FROM VALUE(cFileLoc).
    REPEAT:
        IMPORT UNFORMATTED cRawLine.
        IF cRawLine EQ "[PluginPolicy.Progress.AdminServer]" THEN ASSIGN 
            lInSection = TRUE.
        IF lInSection 
        AND TRIM(cRawLine) BEGINS "port" THEN DO: 
            ASSIGN 
                cResult = TRIM(ENTRY(2,TRIM(cRawLine),"="))
                lInSection = FALSE.
            LEAVE.
        END.
    END.
    INPUT CLOSE.

    RETURN cResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetDateTimeForExtension C-Win 
FUNCTION fGetDateTimeForExtension RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDtTm AS CHARACTER NO-UNDO.
    
    ASSIGN 
        cDtTm = STRING(YEAR(today),"9999") + 
                STRING(MONTH(TODAY),"99") + 
                STRING(DAY(TODAY),"99") + "-" +
                STRING(TIME,"99999"). 
                
    RETURN cDtTm.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetDbLongName C-Win 
FUNCTION fGetDbLongName RETURNS CHARACTER
  ( INPUT ipcDbName AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
    FIND FIRST ttDatabase WHERE 
           ttDatabase.cDbName EQ ipcDbName AND 
        ttDatabase.cVarName EQ "databasename"
        NO-ERROR.
    IF NOT AVAIL ttDatabase THEN DO:
        MESSAGE
            "Unable to locate ttDatabase in temp table."
            VIEW-AS ALERT-BOX.
        RETURN "".
    END. 

    RETURN ttDatabase.cVarValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetLongDate C-Win 
FUNCTION fGetLongDate RETURNS CHARACTER
  ( INPUT ipdDate AS DATE ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLongDate AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMonthList AS CHAR NO-UNDO.
    
    ASSIGN 
        cMonthList = "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec"
        cLongDate = ENTRY(MONTH(ipdDate),cMonthList) + " " +
                    STRING(DAY(ipdDate)) + ", " + 
                    STRING(YEAR(ipdDate),"9999") + " " +
                    STRING(TIME,"HH:MM:SS AM").

    RETURN cLongDate.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetNextPort C-Win 
FUNCTION fGetNextPort RETURNS INTEGER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iPort AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCt AS INT NO-UNDO.
    
    DO iCt = 2801 TO 2999:
        IF NOT CAN-FIND(FIRST ttPorts WHERE 
                        ttPorts.iPort EQ iCt) THEN DO:
            ASSIGN 
                iPort = iCt.
            LEAVE.
        END.
    END.

    RETURN iPort.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

