&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:             asiLogin.w
  Description:      General login program for ASI application

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
DEF NEW GLOBAL SHARED    VAR      fwd-embedded-mode AS LOG           NO-UNDO INIT FALSE.

DEFINE NEW GLOBAL SHARED VARIABLE g_lookup-var      AS CHARACTER     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_track_usage     AS LOGICAL       NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_header_line     AS CHARACTER     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_groups          AS CHARACTER     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE init_menu         AS LOGICAL       NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_developer       AS CHARACTER     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_version         AS CHARACTER     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_rec_key         AS CHARACTER     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_pageno          AS INTEGER       NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_mainmenu        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g-sharpshooter    AS LOG           NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cIniLoc           AS CHAR          NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cUsrLoc           AS CHAR          NO-UNDO.
DEFINE NEW SHARED        VARIABLE g_company         AS CHARACTER     NO-UNDO.
DEFINE NEW SHARED        VARIABLE g_loc             AS CHARACTER     NO-UNDO.
DEFINE NEW SHARED        VARIABLE g_sysdate         AS DATE          NO-UNDO.
DEFINE NEW SHARED        VARIABLE g_period          AS INTEGER       NO-UNDO.
DEFINE NEW SHARED        VARIABLE g_init            AS LOGICAL       NO-UNDO.
DEFINE NEW SHARED        VARIABLE g_batch           AS LOGICAL       NO-UNDO.
DEFINE NEW SHARED        VARIABLE g_batch-rowid     AS ROWID         NO-UNDO.
DEFINE NEW SHARED        VARIABLE miscflds_reckey   AS CHARACTER.
DEFINE NEW SHARED        VARIABLE table_reckey      AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE persistent-handle AS HANDLE.
DEFINE NEW SHARED        VARIABLE ListLogic-Handle  AS HANDLE.
DEFINE NEW SHARED        VARIABLE igsSessionID      AS INTEGER.
DEFINE NEW SHARED        VARIABLE quit_login        AS LOGICAL       NO-UNDO.

DEF                      VAR      hPreRun           AS HANDLE.
DEF VAR cModeList         AS CHAR INITIAL "Advantzware,Addon,CaseLabel,Schedule Monitor,Editor,Esko Monitor,FG XML Monitor,Loadtags,Monitor Users,Rel XML Monitor,RFID Monitor,RM Loadtag,Sharpshooter,Touchscreen" NO-UNDO.
DEF VAR cEnvList          AS CHAR INITIAL "Prod" NO-UNDO.
DEF VAR cDbList           AS CHAR INITIAL "asiProd" NO-UNDO.
/* #ASI Login Items Support */
DEF VAR cPgmList          AS CHAR INITIAL "system/mainmenu.w,system/addmain.w,oerep/r-casetg.w,custom/asiSchW.w,_edit.p,jobxml\monitor.w,fgXml\monitor.w,oerep/r-loadtg.w,proshut.bat,relxml\monitor.w,rfid\monitor.w,rmrep/rmloadtg.w,sshoot/sshoot.w,touch/touchscr.w" NO-UNDO.
DEF VAR cDbDirList        AS CHAR INITIAL "Prod" NO-UNDO.
DEF VAR cDbPortList       AS CHAR INITIAL "2826" NO-UNDO.
DEF VAR cAudDirList       AS CHAR INITIAL "Audit" NO-UNDO.
DEF VAR cAudDBList        AS CHAR INITIAL "audProd" NO-UNDO.
DEF VAR cAudPortList      AS CHAR INITIAL "2836" NO-UNDO.
DEF VAR cEnvVerList       AS CHAR INITIAL "16.7.0" NO-UNDO.
DEF VAR cDbVerList        AS CHAR INITIAL "16.7.0" NO-UNDO.
DEF VAR hAsiLoginProc     AS HANDLE NO-UNDO.
DEF                      VAR      cSessionParam     AS CHAR          NO-UNDO.
DEF                      VAR      cEnvironmentList  AS CHAR          NO-UNDO.
DEF                      VAR      cDatabaseList     AS CHAR          NO-UNDO.
DEF                      VAR      cModeScrList      AS CHAR          NO-UNDO.
DEF                      VAR      cRunPgm           AS CHAR          NO-UNDO.
DEF                      VAR      cValidDBs         AS CHAR          NO-UNDO.
DEF                      VAR      cValidEnvs        AS CHAR          NO-UNDO.
DEF                      VAR      cValidModes       AS CHAR          NO-UNDO.
DEF                      VAR      iTruncLevel       AS INT           NO-UNDO.
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
&SCOPED-DEFINE IN IN FRAME DEFAULT-FRAME
&SCOPED-DEFINE SV SCREEN-VALUE {&IN}

PROCEDURE fwdRunProgram.
   def input param pname as char.
   def input param runPersistent as log.
   def output param phandle as handle.
   
   // these need to be executed in the context of asiLogin.w, in embedded mode
   if runPersistent
      then run value(pname) persistent set phandle.
      else run value(pname).
END.


/* Pre-visualization tasks */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-2 fiUserID fiPassword cbMode ~
cbEnvironment cbDatabase Btn_Cancel Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS fiUserID fiPassword cbMode cbEnvironment ~
cbDatabase 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD intVer C-Win 
FUNCTION intVer RETURNS INTEGER
    ( INPUT cVerString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Login" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cbDatabase AS CHARACTER FORMAT "X(256)":U 
     LABEL "Database" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cbEnvironment AS CHARACTER FORMAT "X(256)":U 
     LABEL "Environment" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "Live","Test" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cbMode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mode" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "Standard","Touchscreen","Sharpshooter","Monitor" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE fiPassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE fiUserID AS CHARACTER FORMAT "X(256)":U 
     LABEL "User ID" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-2
     SIZE 40 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiUserID AT ROW 8.38 COL 15 COLON-ALIGNED WIDGET-ID 4
     fiPassword AT ROW 9.81 COL 15 COLON-ALIGNED WIDGET-ID 6 PASSWORD-FIELD 
     cbMode AT ROW 11.24 COL 15 COLON-ALIGNED WIDGET-ID 10
     cbEnvironment AT ROW 12.67 COL 15 COLON-ALIGNED WIDGET-ID 8
     cbDatabase AT ROW 14.1 COL 15 COLON-ALIGNED WIDGET-ID 18
     Btn_Cancel AT ROW 15.76 COL 6 WIDGET-ID 22
     Btn_OK AT ROW 15.76 COL 27 WIDGET-ID 26
     IMAGE-2 AT ROW 1.48 COL 4 WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 46.8 BY 17.19
         CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
         TITLE              = "Login"
         HEIGHT             = 16.86
         WIDTH              = 45.6
         MAX-HEIGHT         = 21.48
         MAX-WIDTH          = 83.2
         VIRTUAL-HEIGHT     = 21.48
         VIRTUAL-WIDTH      = 83.2
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = yes
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
ASSIGN 
       cbDatabase:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Login */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Login */
DO:
  /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    QUIT.
    /* RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    IF VALID-HANDLE(hPreRun) THEN 
        RUN epDisconnectDB IN hPreRun.
    APPLY 'window-close' TO C-Win.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* Login */
OR RETURN OF fiPassword
OR RETURN OF cbEnvironment
OR RETURN OF cbDatabase
OR RETURN OF cbMode
DO:
    RUN ipAssignSV.
    RUN ipClickOK (OUTPUT cRunPgm).
    
    /* Before directory change */
    IF SEARCH("preRun" + STRING(iTruncLevel,"9999") + ".r") NE ? THEN
            RUN VALUE("preRun" + STRING(iTruncLevel,"9999") + ".p") PERSISTENT SET hPreRun.
        ELSE RUN VALUE("prerun.p") PERSISTENT SET hPreRun.
    
    RUN ipChangeDir.
    
    /* Persistent procedures after directory change */
    RUN ipRunMenu (INPUT cRunPgm).
    
    RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbDatabase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbDatabase C-Win
ON VALUE-CHANGED OF cbDatabase IN FRAME DEFAULT-FRAME /* Database */
OR VALUE-CHANGED OF cbEnvironment
OR VALUE-CHANGED OF cbMode
DO:
    RUN ipAssignSV.
    CASE SELF:NAME:
        WHEN "cbDatabase" THEN DO:
            RUN ipChangeDatabase.
        END.
        WHEN "cbEnvironment" THEN DO:
            RUN ipChangeEnvironment.
            RUN ipGetSV (OUTPUT cbDatabase).
            cbDatabase:SCREEN-VALUE = cbDatabase.
        END.
        WHEN "cbMode" THEN DO:
            RUN ipChangeMode.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUserID C-Win
ON LEAVE OF fiUserID IN FRAME DEFAULT-FRAME /* User ID */
DO:
    RUN ipAssignSV.
    RUN ipFindUser IN THIS-PROCEDURE.
    /* wfk
    IF NOT AVAIL ttUsers THEN DO:
        IF fwd-embedded-mode THEN 
            RETURN NO-APPLY "Unable to locate this user in the advantzware.usr file." +
                            "Please contact your system administrator for assistance.".
        ELSE DO:
            MESSAGE
                "Unable to locate this user in the advantzware.usr file." SKIP
                "Please contact your system administrator for assistance."
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
    END.
    ELSE DO:
        ASSIGN
            cbEnvironment:LIST-ITEMS = IF cValidEnvs <> "" THEN cValidEnvs ELSE IF ttUsers.ttfEnvList <> "" THEN ttUsers.ttfEnvList ELSE cEnvList
            cbDatabase:LIST-ITEMS = IF cValidDbs <> "" THEN cValidDbs ELSE IF ttUsers.ttfDbList <> "" THEN ttUsers.ttfDbList ELSE cDbList
            cbEnvironment:SCREEN-VALUE = ENTRY(1,cbEnvironment:LIST-ITEMS)
            cbDatabase:SCREEN-VALUE = ENTRY(1,cbDatabase:LIST-ITEMS).
        APPLY 'value-changed' TO cbEnvironment.
        APPLY 'value-changed' to cbDatabase.

        RUN ipFindUser IN THIS-PROCEDURE.

        
    END.
        */
    /* Super procedure has values for lists */
    RUN ipGetLists (OUTPUT cEnvList, OUTPUT cModeList, OUTPUT cDbList,
                    OUTPUT cModeScrList, OUTPUT cEnvironmentList,
                    OUTPUT cEnvVerList, OUTPUT cDatabaseList,
                    OUTPUT cDbVerList, OUTPUT cValidDBs,
                    OUTPUT cValidEnvs, OUTPUT cValidModes).
    ASSIGN
        cbMode:LIST-ITEMS = IF cValidModes <> "" THEN cValidModes ELSE cModeList
        cbMode:SCREEN-VALUE = ENTRY(1, cbMode:LIST-ITEMS).
        APPLY 'value-changed' TO cbMode.
    ASSIGN
        cbEnvironment:LIST-ITEMS = IF cValidEnvs <> "" THEN cValidEnvs ELSE  /*IF ttUsers.ttfEnvList <> "" THEN ttUsers.ttfEnvList ELSE */ cEnvList
        cbDatabase:LIST-ITEMS = IF cValidDbs <> "" THEN cValidDbs ELSE /* IF ttUsers.ttfDbList <> "" THEN ttUsers.ttfDbList ELSE */ cDbList
        cbEnvironment:SCREEN-VALUE = ENTRY(1,cbEnvironment:LIST-ITEMS)
        cbDatabase:SCREEN-VALUE = ENTRY(1,cbDatabase:LIST-ITEMS).
    APPLY 'value-changed' TO cbEnvironment.
    APPLY 'value-changed' to cbDatabase.            
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
       
        RUN asiLoginProc.p PERSISTENT SET hAsiLoginProc.
        SESSION:ADD-SUPER-PROCEDURE (hAsiLoginProc).       
    /* Read advantzware.ini, etc */
    RUN ipInit.
    
    /* Super procedure has list values */
    RUN ipGetLists (OUTPUT cEnvList, OUTPUT cModeList, OUTPUT cDbList,
                    OUTPUT cModeScrList, OUTPUT cEnvironmentList,
                    OUTPUT cEnvVerList, OUTPUT cDatabaseList,
                    OUTPUT cDbVerList, OUTPUT cValidDBs,
                    OUTPUT cValidEnvs, OUTPUT cValidModes).
    IMAGE-2:LOAD-IMAGE("asilogosm.jpg").
    ASSIGN
        cbEnvironment:LIST-ITEMS = TRIM(cEnvList,",")
        cbMode:LIST-ITEMS = TRIM(cModeList,",")
        cbDatabase:LIST-ITEMS = TRIM(cdbList,",").
    IF cSessionParam EQ "" THEN DO:
        RUN enable_UI.
    /*    RUN no-top-bann (C-Win:HWND, YES, 0,0). */
        
        ASSIGN
            cbDatabase:SCREEN-VALUE = ENTRY(1,cbDatabase:LIST-ITEMS)
            cbEnvironment:SCREEN-VALUE = ENTRY(1,cbEnvironment:LIST-ITEMS)
            cbMode:SCREEN-VALUE = ENTRY(1,cModeScrList)
            fiUserID:SCREEN-VALUE = OS-GETENV("USERNAME").

        APPLY 'entry' TO fiUserID.
    
        IF NOT THIS-PROCEDURE:PERSISTENT THEN
            WAIT-FOR CLOSE OF THIS-PROCEDURE.
    END. /* If there is a UI */
    ELSE 
      RUN ipAutoLogin.
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
  DISPLAY fiUserID fiPassword cbMode cbEnvironment cbDatabase 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE IMAGE-2 fiUserID fiPassword cbMode cbEnvironment cbDatabase Btn_Cancel 
         Btn_OK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipAssignSV C-Win 
PROCEDURE ipAssignSV :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      fiUserID 
      fiPassword 
      cbEnvironment = cbEnvironment:{&SV}
      cbMode = cbMode:{&SV}      
      cbDatabase = cbDatabase:{&SV}  
      cEnvironmentList = cbEnvironment:LIST-ITEMS
      cDatabaseList = cbDatabase:LIST-ITEMS
      cModeScrList = cbMode:LIST-ITEMS
      .
    RUN ipPassSV (INPUT fiUserID, INPUT fiPassword,
                  INPUT cbEnvironment, INPUT cbMode,
                  INPUT cbDatabase, INPUT cEnvironmentList,
                  INPUT cDatabaseList, INPUT cModeScrList).
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipPreRun C-Win 
PROCEDURE ipPreRun :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>        RUN system\session.p PERSISTENT SET hSession.
        SESSION:ADD-SUPER-PROCEDURE (hSession).
      Notes:       
    ------------------------------------------------------------------------------*/
DEFINE VARIABLE lOK           AS LOGICAL   INITIAL TRUE NO-UNDO.
DEFINE VARIABLE lExit         AS LOGICAL   INITIAL TRUE NO-UNDO.
DEFINE VARIABLE hSession      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hTags         AS HANDLE    NO-UNDO.
DEFINE VARIABLE iEnvVer       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPos          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iEnvLevel     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iDbLevel      AS INTEGER   NO-UNDO.

DEFINE VARIABLE tslogin-log   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cUsrList      AS CHARACTER NO-UNDO.

    ASSIGN
        iPos        = LOOKUP(cbEnvironment,cEnvironmentList)
        iEnvLevel   = intVer(ENTRY(iPos,cEnvVerList))
        iPos        = LOOKUP(cbDatabase,cDatabaseList)
        iDbLevel    = intVer(ENTRY(iPos,cDbVerList))
        iTruncLevel = iDbLevel / 10000
        .

    /* Here the format for both is 16070400 */

    IF iDbLevel GT 16050000
        AND USERID(LDBNAME(1)) NE "asi" THEN 
    DO:
        RUN epCheckPwdExpire IN hPreRun (INPUT-OUTPUT lOK).
        IF NOT lOK THEN QUIT.
        RUN epCheckUserLocked IN hPreRun (INPUT-OUTPUT lOK).
        IF NOT lOK THEN QUIT.
    END.

    IF NOT VALID-HANDLE(hSession)
        AND iEnvLevel GE 16071600 THEN 
    DO:
        RUN system\session.p PERSISTENT SET hSession.
        SESSION:ADD-SUPER-PROCEDURE (hSession).
    END.
    
    IF NOT VALID-HANDLE(hTags) 
        AND iEnvLevel GE 16080000 THEN 
    DO:
        RUN system\TagProcs.p PERSISTENT SET hTags.
        SESSION:ADD-SUPER-PROCEDURE (hTags).
    END.
    
    IF NOT VALID-HANDLE(persistent-handle) THEN
        RUN nosweat/persist.p PERSISTENT SET persistent-handle.
    IF NOT VALID-HANDLE(listlogic-handle) THEN
        RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

    IF iDbLevel GT 16050000
        AND cbMode NE "Monitor Users" 
        AND cbMode NE "Editor" THEN 
    DO:
        RUN epUserLogin IN hPreRun (OUTPUT lExit).
        IF lExit THEN QUIT.
    END.

    IF cbMode = "Touchscreen" THEN 
        RUN epTouchLogin IN hPreRun (OUTPUT tslogin-log).

    RUN epUserRecordCheck IN hPreRun (OUTPUT lOK, OUTPUT g_track_usage).
    IF NOT lOK THEN QUIT.
    /* wfk
    RUN epUpdateUsrFile IN hPreRun (OUTPUT cUsrList).

    RUN ipUpdUsrFile IN THIS-PROCEDURE (cUsrList).
    */
    RUN epGetUserGroups IN hPreRun (OUTPUT g_groups).

    IF iDbLevel GT 16061200 THEN 
        RUN epSetUpEDI IN hPreRun.

    RUN epCheckExpiration IN hPreRun (OUTPUT lOK).
    IF NOT lOK THEN QUIT.

    RUN epGetDeveloperList IN hPreRun (OUTPUT g_developer).

    RUN epGetUsercomp IN hPreRun (OUTPUT g_company, OUTPUT g_loc).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRunMenu C-Win 
PROCEDURE ipRunMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcMenuName AS CHARACTER.

SESSION:REMOVE-SUPER-PROCEDURE (hAsiLoginProc).
ASSIGN
        c-Win:VISIBLE = FALSE. 
IF NOT cbMode = "Monitor Users" THEN DO:
  RUN ipPreRun.
  RUN VALUE(ipcMenuName). 
END.
ELSE
  OS-COMMAND VALUE(ipcMenuName).  
  
QUIT.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION intVer C-Win 
FUNCTION intVer RETURNS INTEGER
    ( INPUT cVerString AS CHAR ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEF VAR cStrVal AS CHAR EXTENT 4 NO-UNDO.
    DEF VAR iIntVal AS INT  EXTENT 4 NO-UNDO.
    DEF VAR iIntVer AS INT  NO-UNDO.
    ASSIGN
        cStrVal[1] = IF NUM-ENTRIES(cVerString,".") GE 1 THEN ENTRY(1,cVerString,".") ELSE "00"
        cStrVal[2] = IF NUM-ENTRIES(cVerString,".") GE 2 THEN ENTRY(2,cVerString,".") ELSE "00"
        cStrVal[3] = IF NUM-ENTRIES(cVerString,".") GE 3 THEN ENTRY(3,cVerString,".") ELSE "00"
        cStrVal[4] = IF NUM-ENTRIES(cVerString,".") GE 4 THEN ENTRY(4,cVerString,".") ELSE "00"
        iIntVal[1] = INT(cStrVal[1])
        iIntVal[2] = INT(cStrVal[2])
        iIntVal[3] = INT(cStrVal[3])
        iIntVal[4] = INT(cStrVal[4])
        iIntVer    = (iIntVal[1] * 1000000) + (iIntVal[2] * 10000) + (iIntVal[3] * 100) + iIntVal[4]
        NO-ERROR.
    RETURN iIntVer.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

