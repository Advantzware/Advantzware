
/*------------------------------------------------------------------------
    File        : asiLoginBat.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Dec 27 10:28:16 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

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
DEF                      VAR      cModeList         AS CHAR          INITIAL "Advantzware,Addon,CaseLabel,Schedule Monitor,Editor,Esko Monitor,FG XML Monitor,Loadtags,Monitor Users,Rel XML Monitor,RFID Monitor,RM Loadtag,Sharpshooter,Touchscreen" NO-UNDO.
DEF                      VAR      cEnvList          AS CHAR          INITIAL "Prod" NO-UNDO.
DEF                      VAR      cDbList           AS CHAR          INITIAL "asiProd" NO-UNDO.
/* #ASI Login Items Support */
DEF                      VAR      cPgmList          AS CHAR          INITIAL "system/mainmenu.w,system/addmain.w,oerep/r-casetg.w,custom/asiSchW.w,_edit.p,jobxml\monitor.w,fgXml\monitor.w,oerep/r-loadtg.w,proshut.bat,relxml\monitor.w,rfid\monitor.w,rmrep/rmloadtg.w,sshoot/sshoot.w,touch/touchscr.w" NO-UNDO.
DEF                      VAR      cDbDirList        AS CHAR          INITIAL "Prod" NO-UNDO.
DEF                      VAR      cDbPortList       AS CHAR          INITIAL "2826" NO-UNDO.
DEF                      VAR      cAudDirList       AS CHAR          INITIAL "Audit" NO-UNDO.
DEF                      VAR      cAudDBList        AS CHAR          INITIAL "audProd" NO-UNDO.
DEF                      VAR      cAudPortList      AS CHAR          INITIAL "2836" NO-UNDO.
DEF                      VAR      cEnvVerList       AS CHAR          INITIAL "16.7.0" NO-UNDO.
DEF                      VAR      cDbVerList        AS CHAR          INITIAL "16.7.0" NO-UNDO.
DEF                      VAR      hAsiLoginProc     AS HANDLE        NO-UNDO.
DEF                      VAR      cSessionParam     AS CHAR          NO-UNDO.
DEF                      VAR      cEnvironmentList  AS CHAR          NO-UNDO.
DEF                      VAR      cDatabaseList     AS CHAR          NO-UNDO.
DEF                      VAR      cModeScrList      AS CHAR          NO-UNDO.
DEF                      VAR      cRunPgm           AS CHAR          NO-UNDO.
DEF                      VAR      cValidDBs         AS CHAR          NO-UNDO.
DEF                      VAR      cValidEnvs        AS CHAR          NO-UNDO.
DEF                      VAR      cValidModes       AS CHAR          NO-UNDO.
DEF                      VAR      iTruncLevel       AS INT           NO-UNDO.
/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
    LABEL "Cancel" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
    LABEL "Login" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE VARIABLE cbDatabase    AS CHARACTER FORMAT "X(256)":U 
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

DEFINE VARIABLE cbMode        AS CHARACTER FORMAT "X(256)":U 
    LABEL "Mode" 
    VIEW-AS COMBO-BOX INNER-LINES 8
    LIST-ITEMS "Standard","Touchscreen","Sharpshooter","Monitor" 
    DROP-DOWN-LIST
    SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE fiPassword    AS CHARACTER FORMAT "X(256)":U 
    LABEL "Password" 
    VIEW-AS FILL-IN 
    SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE fiUserID      AS CHARACTER FORMAT "X(256)":U 
    LABEL "User ID" 
    VIEW-AS FILL-IN 
    SIZE 25 BY 1 NO-UNDO.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION intVer RETURNS INTEGER 
    (INPUT cVerString AS CHAR  ) FORWARD.

&SCOPED-DEFINE SV SCREEN-VALUE {&IN}

/* ***************************  Main Block  *************************** */
RUN asiLoginProc.p PERSISTENT SET hAsiLoginProc.
SESSION:ADD-SUPER-PROCEDURE (hAsiLoginProc). 
      
/* Read session parameters, advantzware.ini, etc */
RUN ipInit.
    
/* Super procedure has list values */
RUN ipGetLists (OUTPUT cEnvList, OUTPUT cModeList, OUTPUT cDbList,
    OUTPUT cModeScrList, OUTPUT cEnvironmentList,
    OUTPUT cEnvVerList, OUTPUT cDatabaseList,
    OUTPUT cDbVerList, OUTPUT cValidDBs,
    OUTPUT cValidEnvs, OUTPUT cValidModes).
    
RUN ipAutoLogin (OUTPUT cRunPgm).


/* **********************  Internal Procedures  *********************** */

PROCEDURE chooseOK:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipAssignSV.
    RUN ipClickOK (OUTPUT cRunPgm).
    
    /* Before directory change */
    IF SEARCH("preRun" + STRING(iTruncLevel,"9999") + ".r") NE ? THEN
        RUN VALUE("preRun" + STRING(iTruncLevel,"9999") + ".p") PERSISTENT SET hPreRun.
    ELSE RUN VALUE("prerun.p") PERSISTENT SET hPreRun.
    
    RUN ipChangeDir.
    
    /* Persistent procedures after directory change */
    RUN ipPreRun.
    
    RUN ipRunMenu (INPUT cRunPgm).
    
    RETURN.

END PROCEDURE.

PROCEDURE ipAssignSV:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipPassSV (INPUT fiUserID, INPUT fiPassword,
        INPUT cbEnvironment, INPUT cbMode,
        INPUT cbDatabase, INPUT cEnvironmentList,
        INPUT cDatabaseList, INPUT cModeScrList).

END PROCEDURE.

PROCEDURE ipPreRun:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lOK         AS LOGICAL   INITIAL TRUE NO-UNDO.
    DEFINE VARIABLE lExit       AS LOGICAL   INITIAL TRUE NO-UNDO.
    DEFINE VARIABLE hSession    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTags       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iEnvVer     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPos        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iEnvLevel   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iDbLevel    AS INTEGER   NO-UNDO.

    DEFINE VARIABLE tslogin-log AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cUsrList    AS CHARACTER NO-UNDO.

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

PROCEDURE ipRunMenu:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMenuName AS CHARACTER.
    SESSION:REMOVE-SUPER-PROCEDURE (hAsiLoginProc).
    IF NOT cbMode = "Monitor Users" THEN 
    DO:
        RUN ipPreRun.
        RUN VALUE(ipcMenuName). 
    END.
    ELSE
        OS-COMMAND VALUE(ipcMenuName).  
  
    QUIT.

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION intVer RETURNS INTEGER 
    (INPUT cVerString AS CHAR  ):
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

