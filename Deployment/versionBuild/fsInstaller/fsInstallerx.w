&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: fsInstaller.w

  Description: Foresight's Progress Application Installation Routine

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
    original logic located in MXP util/xcomp.p (MYT)
    rewritten as fsiNstall.p (with supplementary programs April 2004 (MYT)
    this version originally written June 2005 - MYT
          
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
DEF STREAM fileStream.   
DEF STREAM inStream.
DEF STREAM logStream.
DEF STREAM outStream.

DEF VAR cAttrList               AS CHAR NO-UNDO.
DEF VAR cBaseCodeLocn           AS CHAR NO-UNDO.
DEF VAR cPatchCodeLocn          AS CHAR NO-UNDO.
DEF VAR cBaseFromLocn           AS CHAR NO-UNDO FORMAT "x(255)"  LABEL "Source Location for Uncompiled Base".
DEF VAR cBaseList               AS CHAR NO-UNDO.
DEF VAR cBaseSrcLocn            AS CHAR NO-UNDO FORMAT "x(255)"  LABEL "Source Location for Base Directories".
DEF VAR cBaseTgtLocn            AS CHAR NO-UNDO FORMAT "x(255)"  LABEL "Target Location for Base Directories".
DEF VAR cBaseToLocn             AS CHAR NO-UNDO FORMAT "x(255)"  LABEL "Target Location for Compiled Base".
DEF VAR cCleanBaseFrom          AS CHAR NO-UNDO.
DEF VAR cCleanBaseSrc           AS CHAR NO-UNDO.
DEF VAR cCleanCompileTo         AS CHAR NO-UNDO.
DEF VAR cCleanCopyTo            AS CHAR NO-UNDO.
DEF VAR cCleanCustSrc           AS CHAR NO-UNDO.
DEF VAR cCleanFullName          AS CHAR NO-UNDO.
DEF VAR cCleanObjSrc            AS CHAR NO-UNDO.
DEF VAR cClearLog               AS CHAR NO-UNDO.
DEF VAR cCompileBase            AS CHAR NO-UNDO.
DEF VAR cCompilePatch           AS CHAR NO-UNDO.
DEF VAR cCompileCustom          AS CHAR NO-UNDO.
DEF VAR cCompileSelected        AS CHAR NO-UNDO.
DEF VAR cConnectMulti           AS CHAR NO-UNDO.
DEF VAR cCopyBaseFiles          AS CHAR NO-UNDO.
DEF VAR cCopyBaseTo             AS CHAR NO-UNDO.
DEF VAR cCopyCustFiles          AS CHAR NO-UNDO.
DEF VAR cCopyCustTo             AS CHAR NO-UNDO.
DEF VAR cCopyDFFiles            AS CHAR NO-UNDO.
DEF VAR cCreateBase             AS CHAR NO-UNDO.
DEF VAR cCreateCustom           AS CHAR NO-UNDO.
DEF VAR cCreateCustObj          AS CHAR NO-UNDO.
DEF VAR cCreateDB               AS CHAR NO-UNDO.
DEF VAR cCreateObject           AS CHAR NO-UNDO.
DEF VAR cCreateStartup          AS CHAR NO-UNDO.
DEF VAR cCustCodeLocn           AS CHAR NO-UNDO.
DEF VAR cCustFromLocn           AS CHAR NO-UNDO FORMAT "x(55)"  LABEL "Source Location for Uncompiled Custom".
DEF VAR cCustList               AS CHAR NO-UNDO.
DEF VAR cCustObjLocn            AS CHAR NO-UNDO.
DEF VAR cCustSrcLocn            AS CHAR NO-UNDO FORMAT "x(55)"  LABEL "Source Location for Custom Directories".
DEF VAR cCustTgtLocn            AS CHAR NO-UNDO FORMAT "x(55)"  LABEL "Target Location for Custom Directories".
DEF VAR cCustToLocn             AS CHAR NO-UNDO FORMAT "x(55)"  LABEL "Target Location for Compiled Custom".
DEF VAR cDataFilesLocn          AS CHAR NO-UNDO.
DEF VAR cDBDir2                 AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB Directory (2)".
DEF VAR cDBDir3                 AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB Directory (3)" .
DEF VAR cDBDir4                 AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB Directory (4)" .
DEF VAR cDBDir5                 AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB Directory (5)" .
DEF VAR cDBDir6                 AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB Directory (6)" .
DEF VAR cDBFullName             AS CHAR NO-UNDO FORMAT "x(55)"  LABEL "Full Pathed DB Name".
DEF VAR cDBLocn                 AS CHAR NO-UNDO.
DEF VAR cDBName1                AS CHAR NO-UNDO.
DEF VAR cDBName2                AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB Name (2)".
DEF VAR cDBName3                AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB Name (3)" .
DEF VAR cDBName4                AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB Name (4)" .
DEF VAR cDBName5                AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB Name (5)" .
DEF VAR cDBName6                AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB Name (6)" .
DEF VAR cDBParms1               AS CHAR NO-UNDO.
DEF VAR cDBParms2               AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB Parms (2)" .
DEF VAR cDBParms3               AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB  Parms (3)" .
DEF VAR cDBParms4               AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB  Parms (4)" .
DEF VAR cDBParms5               AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB  Parms (5)" .
DEF VAR cDBParms6               AS CHAR NO-UNDO FORMAT "x(120)" LABEL "DB  Parms (6)" .
DEF VAR cDBStartDir             AS CHAR NO-UNDO FORMAT "x(55)"  LABEL "DB Startup File Directory".
DEF VAR cDeltaLocn              AS CHAR NO-UNDO.
DEF VAR cDFLocn                 AS CHAR NO-UNDO.
DEF VAR cDummy                  AS CHAR NO-UNDO.
DEF VAR cEndValue               AS CHAR NO-UNDO initial "ZZZZZZZZ".
DEF VAR cEnvTargetLocn          AS CHAR NO-UNDO.
DEF VAR cEnvTempLocn            AS CHAR NO-UNDO.
DEF VAR cFileName               AS CHAR NO-UNDO.
DEF VAR cFixDir                 AS CHAR NO-UNDO.
DEF VAR cFixEntry               AS CHAR NO-UNDO.
DEF VAR cFixList                AS CHAR NO-UNDO.
DEF VAR cFullName               AS CHAR NO-UNDO FORMAT "x(255)".
DEF VAR cLoadData               AS CHAR NO-UNDO.
DEF VAR cLogDir                 AS CHAR NO-UNDO.
DEF VAR cLogErrors              AS CHAR NO-UNDO.
DEF VAR cLogLocn                AS CHAR NO-UNDO.
DEF VAR cMyVar                  AS CHAR NO-UNDO FORMAT "x(160)".
DEF VAR cName                   AS CHAR NO-UNDO.
DEF VAR cObjCodeLocn            AS CHAR NO-UNDO.
DEF VAR cObjCustLocn            AS CHAR NO-UNDO FORMAT "x(55)"  LABEL "Optional Location for Custom Object (.r) Directories".
DEF VAR cObjSrcLocn             AS CHAR NO-UNDO FORMAT "x(55)"  LABEL "Source Location for Object Directory Structure".
DEF VAR cObjTgtLocn             AS CHAR NO-UNDO FORMAT "x(55)"  LABEL "Location for Object (.r) Directories".
DEF VAR cOldPropath             AS CHAR NO-UNDO.
DEF VAR cOutputDir1             AS CHAR NO-UNDO.
DEF VAR cOutputDir2             AS CHAR NO-UNDO.
DEF VAR cParmList               AS CHAR NO-UNDO.
DEF VAR cPattern                AS CHAR NO-UNDO initial "*".
DEF VAR cProgressLocn           AS CHAR NO-UNDO.
DEF VAR cPropath                AS CHAR NO-UNDO.
DEF VAR cRunFix                 AS CHAR NO-UNDO.
DEF VAR cSaveConfig             AS CHAR NO-UNDO.
DEF VAR cStartValue             AS CHAR NO-UNDO initial "".
DEF VAR cStopOnError            AS CHAR NO-UNDO.
DEF VAR cTableList              AS CHAR NO-UNDO.
DEF VAR cThisConfig             AS CHAR NO-UNDO FORMAT "x(120)".
DEF VAR cTxt1                   AS CHAR NO-UNDO.
DEF VAR cTxt2                   AS CHAR NO-UNDO.
DEF VAR cUpdateDB               AS CHAR NO-UNDO.
DEF VAR cUpdateDbDir            AS CHAR NO-UNDO FORMAT "x(55)"  LABEL "Target Database Directory".
DEF VAR cUpdateDbLocn           AS CHAR NO-UNDO FORMAT "x(55)"  LABEL "Location of Target Database".
DEF VAR xcomp                   AS CHAR EXTENT 6 NO-UNDO.

DEF VAR iCtr                    AS INT  NO-UNDO.
DEF VAR iCtr1                   AS INT  NO-UNDO.
DEF VAR iCtr2                   AS INT  NO-UNDO.
DEF VAR iCtr3                   AS INT  NO-UNDO.
DEF VAR iEndTime                AS INT  NO-UNDO.
DEF VAR iStartTime              AS INT  NO-UNDO.

DEF VAR lDBConnected            AS LOG  NO-UNDO.
DEF VAR lClearLogFirst          AS LOG  NO-UNDO INITIAL TRUE    LABEL "Clear Log Before Write?".
DEF VAR lErrorsOnlyLog          AS LOG  NO-UNDO INITIAL FALSE   LABEL "Log Errors Only?".
DEF VAR lFound                  AS LOG  NO-UNDO.
DEF VAR lFoundError             AS LOG  NO-UNDO.
DEF VAR lLog                    AS LOG  NO-UNDO.
DEF VAR lPPModified             AS LOG  NO-UNDO.
DEF VAR lSaveConfig             AS LOG  NO-UNDO INITIAL TRUE    LABEL "Save Configuration When Done?".
DEF VAR lStopOnError            AS LOG  NO-UNDO INITIAL FALSE   LABEL "Stop on Any Error?".

PROCEDURE ShellExecuteA EXTERNAL "shell32" :
     define input parameter hwnd as long.
     define input parameter lpOperation as char.
     define input parameter lpFile as char.
     define input parameter lpParameters as char.
     define input parameter lpDirectory as char.
     define input parameter nShowCmd as long.
     define return parameter hInstance as long.
END.

FUNCTION AreControlsRegistered
         RETURNS LOGICAL (INPUT ChrObjectList AS CHARACTER):

    DEFINE VARIABLE logRC      AS LOGICAL    NO-UNDO INITIAL True.
    DEFINE VARIABLE intCnt     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE hdlCheckIt AS COM-HANDLE NO-UNDO.

/*    DO intCnt = 1 TO NUM-ENTRIES(chrObjectList):
        CREATE VALUE(ENTRY(intCnt,chrObjectList)) hdlCheckIt NO-ERROR.
        IF ERROR-STATUS:ERROR = FALSE
        OR ERROR-STATUS:GET-NUMBER(1) = 5894 THEN
            RELEASE OBJECT hdlCheckIt NO-ERROR.
        ELSE DO:
            ASSIGN logRC = False.
  
            CREATE "CALOBJX.CalendarADOCtrl.2" hdlCheckIt NO-ERROR.
            IF ERROR-STATUS:ERROR = FALSE
            OR ERROR-STATUS:GET-NUMBER(1) = 5894 THEN
                RELEASE OBJECT hdlCheckIt NO-ERROR.
            ELSE
                ASSIGN logRC = False.
        END.
    END.
*/
    IF SEARCH("fpCal30.ocx") <> ? THEN DO:
        OS-COMMAND SILENT "regsvr32.exe /s" + value(search("fpCal30.ocx")).
    END.
    ASSIGN
        logRC = TRUE.
    RETURN logRC.

END FUNCTION.

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
&Scoped-Define ENABLED-OBJECTS tCreateDB tUpdateDB tLoadData tCreateBase ~
tCreateObject tCreateCustom tCreateCustObj tCompileBase tCompilePatch ~
tCompileCustom tCompileSelected tRunFix tCreateStartup tCopyDFFiles ~
tCopyBaseFiles tCopyCustFiles tConnectMulti tClearLog tStopOnError ~
tLogErrors bRunDBAdmin bClear bModPropath bSaveConfig cbSuggest fLogDir ~
tHelp bProcess bQuit RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS fiAsOfDate tConfigName tCreateDB tUpdateDB ~
tLoadData tCreateBase tCreateObject tCreateCustom tCreateCustObj ~
tCompileBase tCompilePatch tCompileCustom tCompileSelected tRunFix ~
tCreateStartup tCopyDFFiles tCopyBaseFiles tCopyCustFiles tConnectMulti ~
tClearLog tStopOnError tLogErrors cbSuggest fLogDir fDBLocn fDBName ~
fDBConnParms fDFLocn fDeltaLocn fDataFilesLocn fBaseCodeLocn fPatchCodeLocn ~
fCopyBaseTo fCustCodeLocn fCopyCustTo fObjCodeLocn fCustObjLocn tHelp ~
fEnvTempLocn fEnvTargetLocn fProgressLocn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bClear AUTO-GO 
     LABEL "Clear Entries" 
     SIZE 15 BY .91.

DEFINE BUTTON bModPropath 
     LABEL "Mod Propath" 
     SIZE 15 BY .91.

DEFINE BUTTON bProcess AUTO-GO 
     LABEL "Process" 
     SIZE 15 BY 1.19.

DEFINE BUTTON bQuit 
     LABEL "Cancel/Quit" 
     SIZE 15 BY 1.19.

DEFINE BUTTON bRunDBAdmin 
     LABEL "DB Admin" 
     SIZE 15 BY .91.

DEFINE BUTTON bSaveConfig AUTO-GO 
     LABEL "Save Config" 
     SIZE 15 BY .91.

DEFINE VARIABLE cbSuggest AS CHARACTER FORMAT "X(256)":U INITIAL "Patch Application" 
     LABEL "Suggest Tasks for" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Custom","New Install","Patch Application","Upgrade" 
     DROP-DOWN-LIST
     SIZE 28 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE tHelp AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 83 BY 1.38
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fBaseCodeLocn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Base Code Directory" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fCopyBaseTo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Copy Base To (dir)" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fCopyCustTo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Copy Custom To (dir)" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fCustCodeLocn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cust. Code Directory" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fCustObjLocn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Custom Obj. Directory" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fDataFilesLocn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Data Files Directory" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fDBConnParms AS CHARACTER FORMAT "X(256)":U 
     LABEL "DB Connect Parms" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fDBLocn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Database Directory" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fDBName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Database Name" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fDeltaLocn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Delta DF Locn" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fDFLocn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Structure File Locn" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fEnvTargetLocn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Env. Target Directory" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fEnvTempLocn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Env. Template Dir." 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fiAsOfDate AS CHARACTER FORMAT "X(256)":U INITIAL "Installer Version: 91201" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE fLogDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Config File Directory" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fObjCodeLocn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Object Code Directory" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fPatchCodeLocn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Patch Code Directory" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fProgressLocn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Progress Directory" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE tConfigName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 2.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 10.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 2.62.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 3.57.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 17.38.

DEFINE VARIABLE tClearLog AS LOGICAL INITIAL yes 
     LABEL "Clear Log Before Write?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tCompileBase AS LOGICAL INITIAL no 
     LABEL "Compile Base Code?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tCompileCustom AS LOGICAL INITIAL no 
     LABEL "Compile Custom Code?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tCompilePatch AS LOGICAL INITIAL no 
     LABEL "Compile Patch Code?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tCompileSelected AS LOGICAL INITIAL no 
     LABEL "Compile Selected Code?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tConnectMulti AS LOGICAL INITIAL no 
     LABEL "Connect Multiple DBs" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tCopyBaseFiles AS LOGICAL INITIAL no 
     LABEL "Copy Base Files" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tCopyCustFiles AS LOGICAL INITIAL no 
     LABEL "Copy Custom Files" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tCopyDFFiles AS LOGICAL INITIAL no 
     LABEL "Copy DF Files" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tCreateBase AS LOGICAL INITIAL no 
     LABEL "Create Base Structure?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tCreateCustObj AS LOGICAL INITIAL no 
     LABEL "Create CustObj Structure?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tCreateCustom AS LOGICAL INITIAL no 
     LABEL "Create Custom Structure?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tCreateDB AS LOGICAL INITIAL no 
     LABEL "Create Database?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tCreateObject AS LOGICAL INITIAL no 
     LABEL "Create Object Structure?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tCreateStartup AS LOGICAL INITIAL no 
     LABEL "Create Startup Programs?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tLoadData AS LOGICAL INITIAL no 
     LABEL "Load Existing Data?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tLogErrors AS LOGICAL INITIAL no 
     LABEL "Log Errors Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tRunFix AS LOGICAL INITIAL no 
     LABEL "Run Data Fix Programs?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tStopOnError AS LOGICAL INITIAL no 
     LABEL "Stop on First Error?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.

DEFINE VARIABLE tUpdateDB AS LOGICAL INITIAL no 
     LABEL "Update Database?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .67 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiAsOfDate AT ROW 20.52 COL 2 NO-LABEL NO-TAB-STOP 
     tConfigName AT ROW 1.52 COL 85 COLON-ALIGNED NO-LABEL
     tCreateDB AT ROW 1.52 COL 3
     tUpdateDB AT ROW 2.19 COL 3
     tLoadData AT ROW 2.91 COL 3
     tCreateBase AT ROW 3.62 COL 3
     tCreateObject AT ROW 4.33 COL 3
     tCreateCustom AT ROW 5.05 COL 3
     tCreateCustObj AT ROW 5.76 COL 3
     tCompileBase AT ROW 6.52 COL 3
     tCompilePatch AT ROW 7.19 COL 3
     tCompileCustom AT ROW 7.91 COL 3
     tCompileSelected AT ROW 8.62 COL 3
     tRunFix AT ROW 9.33 COL 3
     tCreateStartup AT ROW 10.33 COL 3
     tCopyDFFiles AT ROW 11.67 COL 3
     tCopyBaseFiles AT ROW 12.43 COL 3
     tCopyCustFiles AT ROW 13.14 COL 3
     tConnectMulti AT ROW 14.14 COL 3
     tClearLog AT ROW 15.57 COL 3
     tStopOnError AT ROW 16.33 COL 3
     tLogErrors AT ROW 17.05 COL 3
     bRunDBAdmin AT ROW 18.14 COL 2
     bClear AT ROW 18.14 COL 18
     bModPropath AT ROW 19.33 COL 2
     bSaveConfig AT ROW 19.33 COL 18
     cbSuggest AT ROW 1.52 COL 38.6
     fLogDir AT ROW 2.57 COL 55 COLON-ALIGNED
     fDBLocn AT ROW 4.1 COL 55 COLON-ALIGNED
     fDBName AT ROW 5.14 COL 55 COLON-ALIGNED
     fDBConnParms AT ROW 6.19 COL 55 COLON-ALIGNED
     fDFLocn AT ROW 7.24 COL 55 COLON-ALIGNED
     fDeltaLocn AT ROW 8.33 COL 55 COLON-ALIGNED
     fDataFilesLocn AT ROW 9.33 COL 55 COLON-ALIGNED
     fBaseCodeLocn AT ROW 10.38 COL 55 COLON-ALIGNED
     fPatchCodeLocn AT ROW 11.52 COL 55 COLON-ALIGNED
     fCopyBaseTo AT ROW 12.67 COL 55 COLON-ALIGNED
     fCustCodeLocn AT ROW 13.67 COL 55 COLON-ALIGNED
     fCopyCustTo AT ROW 14.76 COL 55 COLON-ALIGNED
     fObjCodeLocn AT ROW 15.81 COL 55 COLON-ALIGNED
     fCustObjLocn AT ROW 16.86 COL 55 COLON-ALIGNED
     tHelp AT ROW 21.52 COL 2 NO-LABEL NO-TAB-STOP 
     fEnvTempLocn AT ROW 17.95 COL 55 COLON-ALIGNED
     fEnvTargetLocn AT ROW 19 COL 55 COLON-ALIGNED
     fProgressLocn AT ROW 20.05 COL 55 COLON-ALIGNED
     bProcess AT ROW 21.52 COL 86
     bQuit AT ROW 21.52 COL 104
     RECT-1 AT ROW 15.33 COL 2
     RECT-2 AT ROW 1.24 COL 2
     RECT-3 AT ROW 1.24 COL 34
     RECT-4 AT ROW 11.52 COL 2
     RECT-5 AT ROW 3.86 COL 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120 BY 23.16
         CANCEL-BUTTON bQuit.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "fsInstaller - Create/Update your DECADE environment"
         HEIGHT             = 22
         WIDTH              = 120
         MAX-HEIGHT         = 41.1
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 41.1
         VIRTUAL-WIDTH      = 256
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR COMBO-BOX cbSuggest IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fBaseCodeLocn IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fCopyBaseTo IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fCopyCustTo IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fCustCodeLocn IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fCustObjLocn IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fDataFilesLocn IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fDBConnParms IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fDBLocn IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fDBName IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fDeltaLocn IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fDFLocn IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fEnvTargetLocn IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fEnvTempLocn IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiAsOfDate IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiAsOfDate:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fObjCodeLocn IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fPatchCodeLocn IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fProgressLocn IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fProgressLocn:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tConfigName IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       tHelp:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* fsInstaller - Create/Update your DECADE environment */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
apply 'choose' to bQuit in frame fmain.
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* fsInstaller - Create/Update your DECADE environment */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
apply 'choose' to bQuit in frame fmain.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* this is just a "dummy" trigger to set the help section values based on
   anything the user can possibly do (i hope) */
ON ENTRY ANYWHERE
DO:
    CASE SELF:NAME:
        WHEN "tCreateDB" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Use this option to Create a new DECADE database.  You will need to know the location of the data structure (.df) file needed to build the database, as well as the location (directory) and name of the database you wish to create. (NOTE: YOU ARE LIMITED TO CREATING A NEW DATABASE ON YOUR LOCAL MACHINE.  IF YOU WISH TO CREATE A DATABASE THAT WILL BE 'SERVED', YOU MUST RUN THE INSTALLER ON THE DATABASE SERVER WHERE THE DATABASE WILL RESIDE.)".
        WHEN "tUpdateDB" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Use this option to Update a DECADE database.  You will need to know the location of the DELTA data structure (delta.df) file needed to build the database, as well as the location (directory) and name of the database you wish to update.  If the existing database is on a remote machine, you will need to know the parameters needed to connect to it (typically -H, -S and -N parameters).".
        WHEN "tLoadData" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "NOTE: DUMP YOUR DATABASE CONTENTS TO .d FILES BEFORE USING THIS OPTION!  Use this option to load data into an existing DECADE database.  You will need to know the location of the data files (xxxxx.d) needed to populate the database, as well as the location (directory) and name of the database you wish to populate.  If the existing database is on a remote machine, you will need to know the parameters needed to connect to it (typically -H, -S and -N parameters).".

        WHEN "tCreateBase" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Use this option to create a directory structure ready to hold the DECADE source files.  You will need to know the location of the directory structure you wish to copy, as well as the target directory where you wish to create the new structure.  You also have the option to copy the source files into the new structure (this is the default behavior).  To turn off the file copy option, UNcheck the 'Copy Base Files' option.".
        WHEN "tCreateCustom" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Use this option to create a directory structure ready to hold your custom source files.  You will need to know the location of the directory structure you wish to copy, as well as the target directory where you wish to create the new structure.  You also have the option to copy the custom files into the new structure (this is the default behavior).  To turn off the file copy option, UNcheck the 'Copy Custom Files' option.".
        WHEN "tCreateObject" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Use this option to create a directory structure ready to hold the DECADE object (or compiled) files.  You will need to know the location of the directory structure you wish to copy, as well as the target directory where you wish to create the new structure.  Typically, you would choose the structure containing the base SOURCE code as a template from which to copy.".
        WHEN "tCreateCustObj" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Use this option to create a directory structure ready to hold your CUSTOM object (or compiled) files.  You will need to know the location of the directory structure you wish to copy, as well as the target directory where you wish to create the new structure.  Typically, you would choose the structure containing the CUSTOM SOURCE code as a template from which to copy.".

        WHEN "tCompileBase" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Use this option to compile ALL of the DECADE base code.  You will need to know the location of the source files, the object structure where you wish the compiled code to be placed, and the connection parameters necessary to connect to the DECADE database.".
        WHEN "tCompilePatch" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Use this option to compile any of the DECADE patch code.  You will need to know the location of the source patch files, the object structure where you wish the compiled code to be placed, and the connection parameters necessary to connect to the DECADE database.".
        WHEN "tCompileCustom" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Use this option to compile your custom code against the DECADE database.  You will need to know the location of the custom source files, the object structure where you wish the compiled code to be placed, and the connection parameters necessary to connect to the DECADE database.  If you need to connect to an additional/custom database, CHECK the 'Connect Multiple DBs' box and you will be prompted for the additional connection information required.".
        WHEN "tCompileSelected" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Use this option to compile selected code against the DECADE database.  You will be prompted to select the directories, sub-directories and/or individual programs you wish to compile.  You will need to know the location of the object structure where you wish the compiled code to be placed, and the connection parameters necessary to connect to the DECADE database.  If you need to connect to an additional/custom database, CHECK the 'Connect Multiple DBs' box and you will be prompted for the additional connection information required.".

        WHEN "tRunFix" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Use this option to run FIX programs to update an existing DECADE database.  You will need be prompted to locate and choose the FIX programs you wish to run, and you will need to know the location (directory) and name of the database you wish to populate.  If the existing database is on a remote machine, you will need to know the parameters needed to connect to it (typically -H, -S and -N parameters).".
        WHEN "tCreateStartup" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Use this option to have the Installer create a set up startup programs for a DECADE client.  Typically, this will include a tailored 'progress.ini' and 'mxp.ini' plus specialized shortcuts, icons, and the 'Foresight' font.  You will need to know the location of the startup program templates, and the target directory where you want the customized files to be placed.".

        WHEN "tClearLog" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "This option allows you to determine whether any existing fsInstaller log files will be erased prior to generating a new log for this run.  If this option is UNchecked, the entries from this run will be appended to any previous log entries.".
        WHEN "tStopOnError" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "This option allows you to control the error handling of the fsInstaller.  If this option is checked, and the installer encounters ANY Progress error, the installer will halt.  If UNchecked, the installer will post the error to the install log and continue processing.".
        WHEN "tLogErrors" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "This option allows you to control the amount of information posted to the fsInstaller log.  If checked, ONLY errors found during the process will be written to the log.  If UNchecked, the installer will post both errors and (many) success messages.".

        WHEN "tCopyDFFiles" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "This option allows you to have the fsInstaller program copy the currently selected data structure file into the same directory as the new/updated database.  This can be helpful for reference purposes later.".
        WHEN "tCopyBaseFiles" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "This option allows you to have the fsInstaller program copy the actual source files from alternate media, such as a download location or CD, into the base structure.".
        WHEN "tCopyCustFiles" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "This option allows you to have the fsInstaller program copy the actual custom files from alternate media, such as an existing location or CD, into the custom structure.".
        WHEN "tConnectMulti" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "This option allows you to connect the fsInstaller to multiple databases, prior to running any compilation processes.  If you choose this option, you will be prompted for additional connection information when you press the 'Process' button.".

        WHEN "bModPropath" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Pressing this button will calculate a set of 'propath' values that will be appropriate for this run of the installer, based on the field values you've entered.  You will have the opportunity to modify this propath if you choose prior to processing *this* run of the installer.".

        WHEN "cbSuggest" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "The fsInstaller can suggest typical tasks for several common installation scenarios:" + CHR(10) + "--New Install: creates a database, creates all structures and compiles all code" + CHR(10) + 
            "--Patch Application: updates a database, runs FIX programs, copies new source files and compiles all code" + CHR(10) + "--Upgrade: creates a new database, loads existing data, runs FIX programs, copies new source files and compiles all code" + CHR(10) +
            "The default choice is 'Custom' which allows you to make your own choices individually.".

        WHEN "fDBLocn" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the directory where the fsInstaller can find an existing (local) database, or where it should store a new database you are creating.  Pressing F6 while in this field will open a popup application to help you locate this directory.".
        WHEN "fDBName" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the name of the database to which the fsInstaller can connect, or which it should create.  Note: if you are connecting to a remote database, enter the database name here, and the connection parameters in the 'DB Connect Parms' field below.".
        WHEN "fDBConnParms" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the connection parameters required to connect to the specified database.  If you are connecting to a local database, you should enter '-1' here; if to a remote database, the typical parameters would be '-H <hostname> -S <servicename> -N TCP'.".

        WHEN "fBaseCodeLocn" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the location (directory) FROM which the fsInstaller should copy source code directories (and optionally files, if you have chosen the 'Copy Base Files' option).  This is also the default location the installer uses to create the object code directory structure. (F6 to browse)".
        WHEN "fPatchCodeLocn" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the location (directory) where the fsInstaller can find patch files to compile. (F6 to browse)".
        WHEN "fCopyBaseTo" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the location (directory) where you want to store a local copy of the source code files.".
        WHEN "fCustCodeLocn" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the location (directory) FROM which the fsInstaller should copy your custom code directories (and optionally files, if you have chosen the 'Copy Custom Files' option)  This is also the default location the installer uses to create the custom object directory structure. (F6 to browse)".
        WHEN "fCopyCustTo" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the location (directory) where you want to store a local copy of the custom source code files. (F6 to browse)".
        WHEN "fObjCodeLocn" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the location (directory) INTO which you wish the fsInstaller to compile the base code. (F6 to browse)".
        WHEN "fCustObjLocn" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the location (directory) INTO which you wish the fsInstaller to compile the custom code. (F6 to browse)".

        WHEN "fEnvTempLocn" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the location (directory) where the fsInstaller can find the startup file templates.  Typically, this is in the delivered media in the '/winenv' directory. (F6 to browse)".
        WHEN "fEnvTargetLocn" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the location (directory) where you wish the fsInstaller to place your customized startup files. (F6 to browse)".
        WHEN "fProgressLocn" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the location (directory) of your Progress installation.  This information will be necessary to create the proper startup entries.".

        WHEN "fDFLocn" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the location of the data structure file to be used when building the new database.  Pressing F6 while in this field will open a popup application to help you locate this file.".
        WHEN "fDeltaLocn" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Enter the location of the DELTA data structure file to be used when updating a database.  Pressing F6 while in this field will open a popup application to help you locate this file.".

        WHEN "bClear" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Pressing this button will clear all entries on the screen.".
        WHEN "bProcess" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Pressing this button will process all the tasks you've selected, using the values currently shown on the screen.".
        WHEN "bQuit" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Pressing this button will quit this run of the fsInstaller.".
        WHEN "bSaveConfig" THEN ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain =
            "Pressing this button will cause the fsInstaller to save the current screen values into a configuration file so that they will be available during the next installer run.".

        OTHERWISE ASSIGN tHelp:SCREEN-VALUE IN FRAME fMain = "".
    END CASE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bClear wWin
ON CHOOSE OF bClear IN FRAME fMain /* Clear Entries */
DO:
  /* runs procedure to blank all entries */
  RUN ipClearEntries.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bModPropath
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bModPropath wWin
ON CHOOSE OF bModPropath IN FRAME fMain /* Mod Propath */
DO:
  DEF VAR cOldPP AS CHAR NO-UNDO.
  ASSIGN
    cOldPP = PROPATH.
    
  /* runs a routine to calculate propath additions based on current screen values */
  IF NOT lPPModified THEN
      RUN ipModifyPropath.
  
  /* runs internal propath editor */
  RUN protools/_propath.w.
  
  IF PROPATH <> cOldPP THEN ASSIGN
    lPPModified = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bProcess wWin
ON CHOOSE OF bProcess IN FRAME fMain /* Process */
DO:
  /* only screen element that does any work */
  
  /* set some logical variables used in procedures based on screen values */
  ASSIGN 
      lClearLogFirst = tClearLog:CHECKED IN FRAME fMain
      lStopOnError = tStopOnError:CHECKED IN FRAME fMain     
      lErrorsOnlyLog = tLogErrors:CHECKED IN FRAME fMain.
  
  /* let the user know that a VB script can be viewed as spyware or virus activity */
  IF tCreateStartup:CHECKED IN FRAME fMain THEN MESSAGE
    "Creating startup programs runs an unsigned VBScript program." SKIP
    "If your antivirus or antispyware programs object to this," SKIP
    "please instruct them to allow this process."
    VIEW-AS ALERT-BOX WARNING.
    
  IF NOT lPPModified THEN RUN ipModifyPropath.
  
  /* initalize the log file (will clear if told to do so) */
  RUN ipInitializeLog.
  
  /* Always run the ocx registration */
  AreControlsRegistered("CALOBJX.CalendarADOCtrl.2").

  /* these routines require additional user input.  if they are selected, go ahead
     and ask for what's needed so the user can walk away for a while */
  IF tCompileSelected:CHECKED IN FRAME fMain THEN RUN ipSelectCode.
  IF tRunFix:CHECKED IN FRAME fMain AND SEARCH("fsiFIX.p") = ? THEN RUN ipSelectFix.
  IF tConnectMulti:CHECKED IN FRAME fMain THEN RUN ipConnectMulti.
  
  /* sometimes multiple values can conflict; let the user pick the one they want*/
  IF  fBaseCodeLocn:SENSITIVE IN FRAME fMain = TRUE
  AND fCopyBaseTo:SENSITIVE IN FRAME fMain = TRUE
  AND cBaseCodeLocn <> ""
  AND cCopyBaseTo <> "" 
  AND (tCompileBase:CHECKED IN FRAME fMain 
    OR tCompileSelected:CHECKED IN FRAME fMain) THEN DO:
      RUN ipSelectBase 
        (INPUT "Base Code",
         INPUT cBaseCodeLocn,
         INPUT cCopyBaseTo,
         OUTPUT cOutputDir1).
  END.
  ELSE IF cBaseCodeLocn <> "" THEN ASSIGN cOutputDir1 = cBaseCodeLocn.
  ELSE IF cCopyBaseTo <> "" THEN ASSIGN cOutputDir1 = cCopyBaseTo.
  
  IF  fCustCodeLocn:SENSITIVE IN FRAME fMain = TRUE
  AND fCopyCustTo:SENSITIVE IN FRAME fMain = TRUE
  AND cCustCodeLocn <> ""
  AND cCopyCustTo <> "" 
  AND (tCompileCustom:CHECKED IN FRAME fMain 
    OR tCompileSelected:CHECKED IN FRAME fMain) THEN DO:
      RUN ipSelectBase 
        (INPUT "Custom Code",
         INPUT cCustCodeLocn,
         INPUT cCopyCustTo,
         OUTPUT cOutputDir2).
  END.
  ELSE IF cCustCodeLocn <> "" THEN ASSIGN cOutputDir2 = cCustCodeLocn.
  ELSE IF cCopyCustTo <> "" THEN ASSIGN cOutputDir2 = cCopyCustTo.
  
  
  /* now start the requested processes */
SESSION:SET-WAIT-STATE("general").
  IF tCreateBase:CHECKED IN FRAME fMain THEN RUN ipCreateBase.
SESSION:SET-WAIT-STATE("").
    IF (lStopOnError AND lFoundError) OR LASTKEY = KEYCODE("ESC") THEN RETURN.
  
SESSION:SET-WAIT-STATE("general").
  IF tCreateCustom:CHECKED IN FRAME fMain THEN RUN ipCreateCustom.
SESSION:SET-WAIT-STATE("").
    IF (lStopOnError AND lFoundError) OR LASTKEY = KEYCODE("ESC") THEN RETURN.
  
SESSION:SET-WAIT-STATE("general").
  IF tCreateObject:CHECKED IN FRAME fMain THEN RUN ipCreateObject.
SESSION:SET-WAIT-STATE("").
    IF (lStopOnError AND lFoundError) OR LASTKEY = KEYCODE("ESC") THEN RETURN.
  
SESSION:SET-WAIT-STATE("general").
  IF tCreateDB:CHECKED IN FRAME fMain THEN RUN ipCreateDB.
SESSION:SET-WAIT-STATE("").
    IF (lStopOnError AND lFoundError) OR LASTKEY = KEYCODE("ESC") THEN RETURN.
  
  /* here we're going to make sure all dbs are connected */
SESSION:SET-WAIT-STATE("general").
  IF (tUpdateDB:CHECKED IN FRAME fMain
  OR tLoadData:CHECKED IN FRAME fMain
  OR tCompileBase:CHECKED IN FRAME fMain
  OR tCompilePatch:CHECKED IN FRAME fMain
  OR tCompileCustom:CHECKED IN FRAME fMain
  OR tCompileSelected:CHECKED IN FRAME fMain
  OR tRunFix:CHECKED IN FRAME fMain) 
  AND NOT lDBConnected THEN 
    RUN ipConnectDBs.
SESSION:SET-WAIT-STATE("").
  IF (lStopOnError AND lFoundError) OR LASTKEY = KEYCODE("ESC") THEN RETURN.
  
SESSION:SET-WAIT-STATE("general").
  IF tUpdateDB:CHECKED IN FRAME fMain THEN RUN ipUpdateDB.
SESSION:SET-WAIT-STATE("").
    IF (lStopOnError AND lFoundError) OR LASTKEY = KEYCODE("ESC") THEN RETURN.
  
  IF tLoadData:CHECKED IN FRAME fMain THEN RUN ipLoadData.
    IF (lStopOnError AND lFoundError) OR LASTKEY = KEYCODE("ESC") THEN RETURN.
SESSION:SET-WAIT-STATE("general").
  IF tRunFix:CHECKED IN FRAME fMain THEN DO:
    IF SEARCH("fsiFIX.p") = ? THEN RUN ipRunFix.
    ELSE RUN ipRunFixMandatory.
  END.
SESSION:SET-WAIT-STATE("").
    IF (lStopOnError AND lFoundError) OR LASTKEY = KEYCODE("ESC") THEN RETURN.
  
  IF tCompileBase:CHECKED IN FRAME fMain THEN RUN ipCompileBase.
    IF (lStopOnError AND lFoundError) OR LASTKEY = KEYCODE("ESC") THEN RETURN.
  IF tCompilePatch:CHECKED IN FRAME fMain THEN RUN ipCompilePatch.
    IF (lStopOnError AND lFoundError) OR LASTKEY = KEYCODE("ESC") THEN RETURN.
  IF tCompileCustom:CHECKED IN FRAME fMain THEN RUN ipCompileCustom.
    IF (lStopOnError AND lFoundError) OR LASTKEY = KEYCODE("ESC") THEN RETURN.
  IF tCompileSelected:CHECKED IN FRAME fMain THEN do:
    ASSIGN 
        iCtr2 = 0.
    RUN ipCompileSelected.
    IF (lStopOnError AND lFoundError) OR LASTKEY = KEYCODE("ESC") THEN RETURN.
  END.
  
  /* update z_version if required */
  IF lDBConnected THEN RUN ipUpdateVersion.
  
  /* now that all routines that need connections are done, let's disconnect 
     (this really isn't mandatory, but cleaner than letting the QUIT handle it */
  RUN ipDisconnectDBs.
  
  /* if there are pgms that need to compile without db connection, do them here */
  IF xcomp[1] <> "" THEN
    COMPILE VALUE(xcomp[1]) SAVE INTO VALUE(xcomp[2]) NO-ERROR. 
  IF xcomp[3] <> "" THEN
    COMPILE VALUE(xcomp[3]) SAVE INTO VALUE(xcomp[4]) NO-ERROR. 
  IF xcomp[5] <> "" THEN
    COMPILE VALUE(xcomp[5]) SAVE INTO VALUE(xcomp[6]) NO-ERROR. 

  
SESSION:SET-WAIT-STATE("general").
  IF tCreateStartup:CHECKED IN FRAME fMain THEN RUN ipCreateStartup.
SESSION:SET-WAIT-STATE("").
    IF (lStopOnError AND lFoundError) OR LASTKEY = KEYCODE("ESC") THEN RETURN.

  /* close out the log, and we're done working, so return to ready state */
  RUN ipFinalizeLog.

  MESSAGE
    "fsInstaller completed with a total of " + STRING(iCtr2) + " errors/warnings." SKIP
    "Would you like to view the log file now?"
    VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lChoice AS LOGICAL.
    
  IF lChoice THEN DO:
      DEF VAR hMyInstance as integer no-undo.
      DEF VAR cMyFileName AS CHAR NO-UNDO.
      ASSIGN
        cMyFileName = (cLogDir + REPLACE(cThisConfig,".cfg",".log")).
      
      RUN ShellExecuteA IN THIS-PROCEDURE
        (0,
        "open",
        cMyFileName,
        "",
        "",
        1,
        OUTPUT hMyInstance). 
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bQuit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bQuit wWin
ON CHOOSE OF bQuit IN FRAME fMain /* Cancel/Quit */
DO:
    /* prompt to save this configuration.  i know it gets annoying, but better
       than working for 20 minutes and forgetting to save */
    MESSAGE "Save Configuration?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
        UPDATE lSave AS LOGICAL.
    IF lSave THEN DO:
        RUN ipSaveConfig IN THIS-PROCEDURE.
        APPLY 'window-close' TO THIS-PROCEDURE.
        QUIT.
    END.
    ELSE IF lSave = ? THEN RETURN NO-APPLY.
    ELSE DO:
        APPLY 'window-close' TO THIS-PROCEDURE.
        QUIT.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bRunDBAdmin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bRunDBAdmin wWin
ON CHOOSE OF bRunDBAdmin IN FRAME fMain /* DB Admin */
DO:
  /* just runs the internal db administration tool, so user can dump data files
     from existing dbs, or create delta.dfs */
  RUN _admin.r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bSaveConfig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bSaveConfig wWin
ON CHOOSE OF bSaveConfig IN FRAME fMain /* Save Config */
DO:
  /* fairly self-explanatory */
  RUN ipSaveConfig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbSuggest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbSuggest wWin
ON VALUE-CHANGED OF cbSuggest IN FRAME fMain /* Suggest Tasks for */
DO:
    /* this allows the user to scroll through some common task groups and have
       the installer "automatically" set itself up for doing the right tasks */
    CASE SELF:SCREEN-VALUE:
        WHEN "" THEN RUN ipUseExisting.
        WHEN "New Install" THEN ASSIGN
            tCreateDB:CHECKED IN FRAME fMain = TRUE
            tUpdateDB:CHECKED IN FRAME fMain = FALSE
            tLoadData:CHECKED IN FRAME fMain = FALSE
            tRunFix:CHECKED IN FRAME fMain = FALSE
            tCreateBase:CHECKED IN FRAME fMain = TRUE
            tCreateCustom:CHECKED IN FRAME fMain = FALSE
            tCreateObject:CHECKED IN FRAME fMain = TRUE
            tCreateCustObj:CHECKED IN FRAME fMain = FALSE
            tCompileBase:CHECKED IN FRAME fMain = TRUE
            tCompilePatch:CHECKED IN FRAME fMain = FALSE
            tCompileCustom:CHECKED IN FRAME fMain = FALSE
            tCompileSelected:CHECKED IN FRAME fMain = FALSE
            tRunFix:CHECKED IN FRAME fMain = FALSE
            tCreateStartup:CHECKED IN FRAME fMain = TRUE
            tCopyDFFiles:CHECKED IN FRAME fMain = TRUE
            tCopyBaseFiles:CHECKED IN FRAME fMain = TRUE
            tCopyCustFiles:CHECKED IN FRAME fMain = FALSE
            .
        WHEN "Patch Application" THEN ASSIGN
            tCreateDB:CHECKED IN FRAME fMain = FALSE
            tUpdateDB:CHECKED IN FRAME fMain = TRUE
            tLoadData:CHECKED IN FRAME fMain = FALSE
            tRunFix:CHECKED IN FRAME fMain = TRUE
            tCreateBase:CHECKED IN FRAME fMain = TRUE
            tCreateCustom:CHECKED IN FRAME fMain = FALSE
            tCreateObject:CHECKED IN FRAME fMain = FALSE
            tCreateCustObj:CHECKED IN FRAME fMain = FALSE
            tCompileBase:CHECKED IN FRAME fMain = FALSE
            tCompilePatch:CHECKED IN FRAME fMain = TRUE
            tCompileCustom:CHECKED IN FRAME fMain = TRUE
            tCompileSelected:CHECKED IN FRAME fMain = FALSE
            tRunFix:CHECKED IN FRAME fMain = TRUE
            tCreateStartup:CHECKED IN FRAME fMain = FALSE
            tCopyDFFiles:CHECKED IN FRAME fMain = TRUE
            tCopyBaseFiles:CHECKED IN FRAME fMain = TRUE
            tCopyCustFiles:CHECKED IN FRAME fMain = FALSE
            .
        WHEN "Upgrade" THEN ASSIGN
            tCreateDB:CHECKED IN FRAME fMain = TRUE
            tUpdateDB:CHECKED IN FRAME fMain = FALSE
            tLoadData:CHECKED IN FRAME fMain = TRUE
            tRunFix:CHECKED IN FRAME fMain = TRUE
            tCreateBase:CHECKED IN FRAME fMain = TRUE
            tCreateCustom:CHECKED IN FRAME fMain = FALSE
            tCreateObject:CHECKED IN FRAME fMain = FALSE
            tCreateCustObj:CHECKED IN FRAME fMain = FALSE
            tCompileBase:CHECKED IN FRAME fMain = TRUE
            tCompilePatch:CHECKED IN FRAME fMain = FALSE
            tCompileCustom:CHECKED IN FRAME fMain = TRUE
            tCompileSelected:CHECKED IN FRAME fMain = FALSE
            tRunFix:CHECKED IN FRAME fMain = TRUE
            tCreateStartup:CHECKED IN FRAME fMain = FALSE
            tCopyDFFiles:CHECKED IN FRAME fMain = TRUE
            tCopyBaseFiles:CHECKED IN FRAME fMain = TRUE
            tCopyCustFiles:CHECKED IN FRAME fMain = FALSE
            .
        OTHERWISE ASSIGN
            tCreateDB:CHECKED IN FRAME fMain = FALSE
            tUpdateDB:CHECKED IN FRAME fMain = FALSE
            tLoadData:CHECKED IN FRAME fMain = FALSE
            tRunFix:CHECKED IN FRAME fMain = FALSE
            tCreateBase:CHECKED IN FRAME fMain = FALSE
            tCreateCustom:CHECKED IN FRAME fMain = FALSE
            tCreateObject:CHECKED IN FRAME fMain = FALSE
            tCreateCustObj:CHECKED IN FRAME fMain = FALSE
            tCompileBase:CHECKED IN FRAME fMain = FALSE
            tCompilePatch:CHECKED IN FRAME fMain = FALSE
            tCompileCustom:CHECKED IN FRAME fMain = FALSE
            tCompileSelected:CHECKED IN FRAME fMain = FALSE
            tRunFix:CHECKED IN FRAME fMain = FALSE
            tCreateStartup:CHECKED IN FRAME fMain = FALSE
            tCopyDFFiles:CHECKED IN FRAME fMain = FALSE
            tCopyBaseFiles:CHECKED IN FRAME fMain = FALSE
            tCopyCustFiles:CHECKED IN FRAME fMain = FALSE
            .
    END CASE.
    
    /* make sure all the internal variables are reset to reflect the new state */
    APPLY 'value-changed' TO tCreateDB IN FRAME fMain.
    
    /* and bring focus back here */
    APPLY 'entry' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fLogDir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fLogDir wWin
ON ENTRY OF fLogDir IN FRAME fMain /* Config File Directory */
OR 'LEAVE' OF fLogDir IN FRAME fMain
OR 'LEAVE' OF fDBLocn IN FRAME fMain
OR 'LEAVE' OF fDBName IN FRAME fMain
OR 'LEAVE' OF fDBConnParms IN FRAME fMain
OR 'LEAVE' OF fDFLocn IN FRAME fMain
OR 'LEAVE' OF fDeltaLocn IN FRAME fMain
OR 'LEAVE' OF fDataFilesLocn IN FRAME fMain
OR 'LEAVE' OF fBaseCodeLocn IN FRAME fMain
OR 'LEAVE' OF fPatchCodeLocn IN FRAME fMain
OR 'LEAVE' OF fCopyBaseTo IN FRAME fMain
OR 'LEAVE' OF fCustCodeLocn IN FRAME fMain
OR 'LEAVE' OF fCopyCustTo IN FRAME fMain
OR 'LEAVE' OF fObjCodeLocn IN FRAME fMain
OR 'LEAVE' OF fCustObjLocn IN FRAME fMain
OR 'LEAVE' OF fEnvTempLocn IN FRAME fMain
OR 'LEAVE' OF fEnvTargetLocn IN FRAME fMain
OR 'LEAVE' OF fProgressLocn IN FRAME fMain
DO:
    /* this gets a little complicated, because we're trying to idiot-proof as
       much as possible.  basically, we want to make sure that directories are
       specified as directories (with "\" characters) so we don't have to 
       constantly test for this in routines.  we also want to be careful of the
       60 character Progress limit on filenames, so we do the 8-character
       abbreviation where possible.  finally, every time we leave a fill-in, 
       we want to populate the internal (cXXX) variable with the fill-in's
       screen value, so we don't have to type ':SCREEN-VALUE IN FRAME fMain'
       all the time */
    DEF VAR cFileName AS CHAR NO-UNDO.
    
    SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE,"/","\").
    SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE,"\\","//").
                                                           
    IF SUBSTRING(SELF:SCREEN-VALUE IN FRAME fMain,1,2) = "//" THEN DO:
        IF SUBSTRING(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) <> "/" THEN ASSIGN
            SELF:SCREEN-VALUE = SELF:SCREEN-VALUE + "/".
        RETURN.
    END.

    CASE SELF:NAME:
        WHEN 'fLogDir' THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE + "\","\\","\")
                cLogDir = SELF:SCREEN-VALUE
                tHelp:SCREEN-VALUE IN FRAME fMain =
                    "In this field, you should enter the directory where the fsInstaller can find any existing configuration files, and where it will place the resulting log files from this run.  Pressing F6 while in this field will open a popup application to help you locate this directory.".
            DO iCtr = 1 TO NUM-ENTRIES(cLogDir,"\"):
                IF INDEX(ENTRY(iCtr,cLogDir,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cLogDir,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cLogDir,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cLogDir,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cLogDir,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cLogDir.
                END.
            END.
            IF SEARCH(cLogDir + "fsInstall.cfg") <> ? THEN RUN ipUseExisting.
        END.
        WHEN 'fDBName' THEN DO:
            ASSIGN
                cDBName1 = SELF:SCREEN-VALUE.
        END.
        WHEN 'fDBConnParms' THEN DO:
            ASSIGN
                cDBParms1 = SELF:SCREEN-VALUE.
        END.
        WHEN 'fDBLocn' THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE + "\","\\","\").
            IF SELF:SCREEN-VALUE = "\" THEN ASSIGN SELF:SCREEN-VALUE = "".
            ASSIGN
                cDBLocn = SELF:SCREEN-VALUE.
            /* Don't do this here; the .st file complains */
            /*
            DO iCtr = 1 TO NUM-ENTRIES(cDBLocn,"\"):
                IF INDEX(ENTRY(iCtr,cDBLocn,"\")," ") <> 0 
                OR LENGTH(ENTRY(iCtr,cDBLocn,"\")) > 8 THEN DO:
                    REPLACE(ENTRY(iCtr,cDBLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cDBLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cDBLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cDBLocn.
                END.
            END.
            */
        END.
        WHEN 'fDFLocn' THEN DO:
            ASSIGN cFileName = fDFLocn:SCREEN-VALUE. 
            ASSIGN 
                cDFLocn = cFileName
                SELF:SCREEN-VALUE = cDFLocn.
            DO iCtr = 1 TO NUM-ENTRIES(cDFLocn,"\") - 1:
                IF INDEX(ENTRY(iCtr,cDFLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cDFLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cDFLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cDFLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cDFLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cDFLocn.
                END.
            END.
        END.
        WHEN 'fDeltaLocn' THEN DO:
            ASSIGN cFileName = fDeltaLocn:SCREEN-VALUE. 
            ASSIGN 
                cDeltaLocn = cFileName
                SELF:SCREEN-VALUE = cDeltaLocn.
            DO iCtr = 1 TO NUM-ENTRIES(cDeltaLocn,"\") - 1:
                IF INDEX(ENTRY(iCtr,cDeltaLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cDeltaLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cDeltaLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cDeltaLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cDeltaLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cDeltaLocn.
                END.
            END.
        END.
        WHEN 'fDataFilesLocn' THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE + "\","\\","\")
                cDataFilesLocn = SELF:SCREEN-VALUE.
            DO iCtr = 1 TO NUM-ENTRIES(cDataFilesLocn,"\"):
                IF INDEX(ENTRY(iCtr,cDataFilesLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cDataFilesLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cDataFilesLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cDataFilesLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cDataFilesLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cDataFilesLocn.
                END.
            END.
        END.
        WHEN 'fBaseCodeLocn' THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE + "\","\\","\")
                cBaseCodeLocn = SELF:SCREEN-VALUE.
            DO iCtr = 1 TO NUM-ENTRIES(cBaseCodeLocn,"\"):
                IF INDEX(ENTRY(iCtr,cBaseCodeLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cBaseCodeLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cBaseCodeLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cBaseCodeLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cBaseCodeLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cBaseCodeLocn.
                END.
            END.
        END.
        WHEN 'fPatchCodeLocn' THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE + "\","\\","\")
                cPatchCodeLocn = SELF:SCREEN-VALUE.
            DO iCtr = 1 TO NUM-ENTRIES(cPatchCodeLocn,"\"):
                IF INDEX(ENTRY(iCtr,cPatchCodeLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cBaseCodeLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cPatchCodeLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cPatchCodeLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cPatchCodeLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cPatchCodeLocn.
                END.
            END.
        END.
        WHEN 'fCopyBaseTo' THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE + "\","\\","\")
                cCopyBaseTo = SELF:SCREEN-VALUE.
            DO iCtr = 1 TO NUM-ENTRIES(cCopyBaseTo,"\"):
                IF INDEX(ENTRY(iCtr,cCopyBaseTo,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cCopyBaseTo,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cCopyBaseTo,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cCopyBaseTo,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cCopyBaseTo,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cCopyBaseTo.
                END.
            END.
        END.
        WHEN 'fCustCodeLocn' THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE + "\","\\","\")
                cCustCodeLocn = SELF:SCREEN-VALUE.
            DO iCtr = 1 TO NUM-ENTRIES(cCustCodeLocn,"\"):
                IF INDEX(ENTRY(iCtr,cCustCodeLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cCustCodeLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cCustCodeLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cCustCodeLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cCustCodeLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cCustCodeLocn.
                END.
            END.
        END.
        WHEN 'fCopyCustTo' THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE + "\","\\","\")
                cCopyCustTo = SELF:SCREEN-VALUE.
            DO iCtr = 1 TO NUM-ENTRIES(cCopyCustTo,"\"):
                IF INDEX(ENTRY(iCtr,cCopyCustTo,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cCopyCustTo,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cCopyCustTo,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cCopyCustTo,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cCopyCustTo,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cCopyCustTo.
                END.
            END.
        END.
        WHEN 'fObjCodeLocn' THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE + "\","\\","\")
                cObjCodeLocn = SELF:SCREEN-VALUE.
            DO iCtr = 1 TO NUM-ENTRIES(cObjCodeLocn,"\"):
                IF INDEX(ENTRY(iCtr,cObjCodeLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cObjCodeLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cObjCodeLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cObjCodeLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cObjCodeLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cObjCodeLocn.
                END.
            END.
        END.
        WHEN 'fCustObjLocn' THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE + "\","\\","\")
                cCustObjLocn = SELF:SCREEN-VALUE.
            DO iCtr = 1 TO NUM-ENTRIES(cCustObjLocn,"\"):
                IF INDEX(ENTRY(iCtr,cCustObjLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cCustObjLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cCustObjLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cCustObjLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cCustObjLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cCustObjLocn.
                END.
            END.
        END.
        WHEN 'fEnvTempLocn' THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE + "\","\\","\")
                cEnvTempLocn = SELF:SCREEN-VALUE.
            DO iCtr = 1 TO NUM-ENTRIES(cEnvTempLocn,"\"):
                IF INDEX(ENTRY(iCtr,cEnvTempLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cEnvTempLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cEnvTempLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cEnvTempLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cEnvTempLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cEnvTempLocn.
                END.
            END.
        END.
        WHEN 'fEnvTargetLocn' THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE + "\","\\","\")
                cEnvTargetLocn = SELF:SCREEN-VALUE.
            DO iCtr = 1 TO NUM-ENTRIES(cEnvTargetLocn,"\"):
                IF INDEX(ENTRY(iCtr,cEnvTargetLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cEnvTargetLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cEnvTargetLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cEnvTargetLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cEnvTargetLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cEnvTargetLocn.
                END.
            END.
        END.
        WHEN 'fProgressLocn' THEN DO:
            ASSIGN 
                SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE + "\","\\","\")
                cProgressLocn = SELF:SCREEN-VALUE.
            DO iCtr = 1 TO NUM-ENTRIES(cProgressLocn,"\"):
                IF INDEX(ENTRY(iCtr,cProgressLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cProgressLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cProgressLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cProgressLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cProgressLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cProgressLocn.
                END.
            END.
            /* If Progress location hasn't been specified, get from current configuration */
            IF cProgressLocn = "\" AND SEARCH("prowin32.exe") <> ? THEN DO:
                ASSIGN
                    cProgressLocn = SUBSTRING(SEARCH("prowin32.exe"),1,INDEX(SEARCH("prowin32.exe"),"bin") - 1).
                DO iCtr = 1 TO NUM-ENTRIES(cProgressLocn,"\"):
                    IF INDEX(ENTRY(iCtr,cProgressLocn,"\")," ") <> 0 
                    /* OR LENGTH(ENTRY(iCtr,cProgressLocn,"\")) > 8 */ THEN DO:
                        REPLACE(ENTRY(iCtr,cProgressLocn,"\")," ","").
                        ASSIGN 
                            ENTRY(iCtr,cProgressLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cProgressLocn,"\"),1,6)," ") + "~~1"
                            fProgressLocn:SCREEN-VALUE IN FRAME fMain = cProgressLocn.
                   END.
                END.
            END.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fLogDir wWin
ON F6 OF fLogDir IN FRAME fMain /* Config File Directory */
OR 'F6' OF fDBLocn IN FRAME fMain
OR 'F6' OF fDFLocn IN FRAME fMain
OR 'F6' OF fDeltaLocn IN FRAME fMain
OR 'F6' OF fDataFilesLocn IN FRAME fMain
OR 'F6' OF fBaseCodeLocn IN FRAME fMain
OR 'F6' OF fPatchCodeLocn IN FRAME fMain
OR 'F6' OF fCopyBaseTo IN FRAME fMain
OR 'F6' OF fCustCodeLocn IN FRAME fMain
OR 'F6' OF fCopyCustTo IN FRAME fMain
OR 'F6' OF fObjCodeLocn IN FRAME fMain
OR 'F6' OF fCustObjLocn IN FRAME fMain
OR 'F6' OF fEnvTempLocn IN FRAME fMain
OR 'F6' OF fEnvTargetLocn IN FRAME fMain
OR 'F6' OF fProgressLocn IN FRAME fMain
DO:
    /* this is simpler than it looks.  just running the windows get-file
    dialog whenever the user presses F6.  only real issues are:
    (1) are we looking for a file or directory, and
    (2) are we reading or writing
    */
    
    DEF VAR cFileName AS CHAR NO-UNDO.
    
    CASE SELF:NAME:
        WHEN 'fLogDir' THEN DO:
            ASSIGN cFileName = "fsInstall.cfg".
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Directory for Config/Log Files"
            FILTERS "Config Files (*.cfg)"  "*.cfg",
                    "Log Files (*.log)"  "*.log"
            CREATE-TEST-FILE
            USE-FILENAME
            DEFAULT-EXTENSION ".cfg"
            INITIAL-DIR ".".
            ASSIGN 
                cLogDir = substring(cFileName, 1,R-INDEX(cFileName,"\"))
                cThisConfig = substring(cFileName,R-INDEX(cFileName,"\") + 1).
                SELF:SCREEN-VALUE = cLogDir.
            DO iCtr = 1 TO NUM-ENTRIES(cLogDir,"\"):
                IF INDEX(ENTRY(iCtr,cLogDir,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cLogDir,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cLogDir,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cLogDir,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cLogDir,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cLogDir.
                END.
            END.
            IF SEARCH(cFileName) <> ? THEN RUN ipUseExisting.
        END.
        WHEN 'fDBLocn' THEN DO:
            ASSIGN cFileName = fDBName:SCREEN-VALUE. 
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Database Directory"
            FILTERS "Database Files (*.db)"  "*.db"
            CREATE-TEST-FILE
            USE-FILENAME
            DEFAULT-EXTENSION ".db"
            INITIAL-DIR fDBLocn:SCREEN-VALUE.
            ASSIGN 
                cDBLocn = substring(cFileName, 1,R-INDEX(cFileName,"\"))
                SELF:SCREEN-VALUE = cDBLocn.
            /* Don't do this here; the .st file complains */
            /*
            DO iCtr = 1 TO NUM-ENTRIES(cDBLocn,"\"):
                IF INDEX(ENTRY(iCtr,cDBLocn,"\")," ") <> 0 
                OR LENGTH(ENTRY(iCtr,cDBLocn,"\")) > 8 THEN DO:
                    REPLACE(ENTRY(iCtr,cDBLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cDBLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cDBLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cDBLocn.
                END.
            END.
            */
            IF SEARCH(cFileName) <> ? THEN ASSIGN fDBName:SCREEN-VALUE = substring(cFileName,R-INDEX(cFileName,"\") + 1).
        END.
        WHEN 'fDFLocn' THEN DO:
            ASSIGN cFileName = fDFLocn:SCREEN-VALUE. 
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Data Structure File"
            FILTERS "Structure Files (*.df)"  "*.df"
            CREATE-TEST-FILE
            USE-FILENAME
            DEFAULT-EXTENSION ".df"
            INITIAL-DIR fDFLocn:SCREEN-VALUE.
            ASSIGN 
                cDFLocn = cFileName
                SELF:SCREEN-VALUE = cDFLocn.
            DO iCtr = 1 TO NUM-ENTRIES(cDFLocn,"\") - 1:
                IF INDEX(ENTRY(iCtr,cDFLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cDFLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cDFLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cDFLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cDFLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cDFLocn.
                END.
            END.
        END.
        WHEN 'fDeltaLocn' THEN DO:
            ASSIGN cFileName = fDeltaLocn:SCREEN-VALUE. 
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Data Structure File"
            FILTERS "Structure Files (*.df)"  "*.df"
            CREATE-TEST-FILE
            USE-FILENAME
            DEFAULT-EXTENSION ".df"
            INITIAL-DIR fDeltaLocn:SCREEN-VALUE.
            ASSIGN 
                cDeltaLocn = cFileName
                SELF:SCREEN-VALUE = cDeltaLocn.
            DO iCtr = 1 TO NUM-ENTRIES(cDeltaLocn,"\") - 1:
                IF INDEX(ENTRY(iCtr,cDeltaLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cDeltaLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cDeltaLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cDeltaLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cDeltaLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cDeltaLocn.
                END.
            END.
        END.
        WHEN 'fDataFilesLocn' THEN DO:
            ASSIGN cFileName = "_tag.d".
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Data Files Directory"
            FILTERS "Data Files (*.d)"  "*.d"
            CREATE-TEST-FILE
            USE-FILENAME
            DEFAULT-EXTENSION ".d"
            INITIAL-DIR fDataFilesLocn:SCREEN-VALUE.
            ASSIGN 
                cDataFilesLocn = substring(cFileName, 1,R-INDEX(cFileName,"\"))
                SELF:SCREEN-VALUE = cDataFilesLocn.
            DO iCtr = 1 TO NUM-ENTRIES(cDataFilesLocn,"\"):
                IF INDEX(ENTRY(iCtr,cDataFilesLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cDataFilesLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cDataFilesLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cDataFilesLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cDataFilesLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cDataFilesLocn.
                END.
            END.
        END.
        WHEN 'fBaseCodeLocn' THEN DO:
            ASSIGN cFileName = "_tag.w".
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Base Code Source Directory"
            FILTERS "Code Files (*.w)"  "*.w"
            CREATE-TEST-FILE
            USE-FILENAME
            DEFAULT-EXTENSION ".w"
            INITIAL-DIR fBaseCodeLocn:SCREEN-VALUE.
            ASSIGN 
                cBaseCodeLocn = substring(cFileName, 1,R-INDEX(cFileName,"\"))
                SELF:SCREEN-VALUE = cBaseCodeLocn.
            DO iCtr = 1 TO NUM-ENTRIES(cBaseCodeLocn,"\"):
                IF INDEX(ENTRY(iCtr,cBaseCodeLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cBaseCodeLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cBaseCodeLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cBaseCodeLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cBaseCodeLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cBaseCodeLocn.
                END.
            END.
        END.
        WHEN 'fPatchCodeLocn' THEN DO:
            ASSIGN cFileName = "_tag.w".
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Base Code Source Directory"
            FILTERS "Code Files (*.w)"  "*.w"
            CREATE-TEST-FILE
            USE-FILENAME
            DEFAULT-EXTENSION ".w"
            INITIAL-DIR fPatchCodeLocn:SCREEN-VALUE.
            ASSIGN 
                cPatchCodeLocn = substring(cFileName, 1,R-INDEX(cFileName,"\"))
                SELF:SCREEN-VALUE = cPatchCodeLocn.
            DO iCtr = 1 TO NUM-ENTRIES(cPatchCodeLocn,"\"):
                IF INDEX(ENTRY(iCtr,cPatchCodeLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cPatchCodeLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cPatchCodeLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cPatchCodeLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cPatchCodeLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cPatchCodeLocn.
                END.
            END.
        END.
        WHEN 'fCopyBaseTo' THEN DO:
            ASSIGN cFileName = "_tag.w".
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Base Code Target Directory"
            FILTERS "Code Files (*.w)"  "*.w"
            CREATE-TEST-FILE
            USE-FILENAME
            DEFAULT-EXTENSION ".w"
            INITIAL-DIR fCopyBaseTo:SCREEN-VALUE.
            ASSIGN 
                cCopyBaseTo = substring(cFileName, 1,R-INDEX(cFileName,"\"))
                SELF:SCREEN-VALUE = cCopyBaseTo.
            DO iCtr = 1 TO NUM-ENTRIES(cCopyBaseTo,"\"):
                IF INDEX(ENTRY(iCtr,cCopyBaseTo,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cCopyBaseTo,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cCopyBaseTo,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cCopyBaseTo,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cCopyBaseTo,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cCopyBaseTo.
                END.
            END.
        END.
        WHEN 'fCustCodeLocn' THEN DO:
            ASSIGN cFileName = "_tag.w".
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Custom Code Source Directory"
            FILTERS "Code Files (*.w)"  "*.w"
            CREATE-TEST-FILE
            USE-FILENAME
            DEFAULT-EXTENSION ".w"
            INITIAL-DIR fCustCodeLocn:SCREEN-VALUE.
            ASSIGN 
                cCustCodeLocn = substring(cFileName, 1,R-INDEX(cFileName,"\"))
                SELF:SCREEN-VALUE = cCustCodeLocn.
            DO iCtr = 1 TO NUM-ENTRIES(cCustCodeLocn,"\"):
                IF INDEX(ENTRY(iCtr,cCustCodeLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cCustCodeLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cCustCodeLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cCustCodeLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cCustCodeLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cCustCodeLocn.
                END.
            END.
        END.
        WHEN 'fCopyCustTo' THEN DO:
            ASSIGN cFileName = "_tag.w".
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Custom Code Target Directory"
            FILTERS "Code Files (*.w)"  "*.w"
            CREATE-TEST-FILE
            USE-FILENAME
            DEFAULT-EXTENSION ".w"
            INITIAL-DIR fCopyCustTo:SCREEN-VALUE.
            ASSIGN 
                cCopyCustTo = substring(cFileName, 1,R-INDEX(cFileName,"\"))
                SELF:SCREEN-VALUE = cCopyCustTo.
            DO iCtr = 1 TO NUM-ENTRIES(cCopyCustTo,"\"):
                IF INDEX(ENTRY(iCtr,cCopyCustTo,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cCopyCustTo,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cCopyCustTo,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cCopyCustTo,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cCopyCustTo,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cCopyCustTo.
                END.
            END.
        END.
        WHEN 'fObjCodeLocn' THEN DO:
            ASSIGN cFileName = "_tag.w".
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Object Code Target Directory"
            FILTERS "Code Files (*.w)"  "*.w"
            CREATE-TEST-FILE
            USE-FILENAME
            DEFAULT-EXTENSION ".w"
            INITIAL-DIR fObjCodeLocn:SCREEN-VALUE.
            ASSIGN 
                cObjCodeLocn = substring(cFileName, 1,R-INDEX(cFileName,"\"))
                SELF:SCREEN-VALUE = cObjCodeLocn.
            DO iCtr = 1 TO NUM-ENTRIES(cObjCodeLocn,"\"):
                IF INDEX(ENTRY(iCtr,cObjCodeLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cObjCodeLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cObjCodeLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cObjCodeLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cObjCodeLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cObjCodeLocn.
                END.
            END.
        END.
        WHEN 'fCustObjLocn' THEN DO:
            ASSIGN cFileName = "_tag.w".
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Custom Object Target Directory"
            FILTERS "Code Files (*.w)"  "*.w"
            CREATE-TEST-FILE
            USE-FILENAME
            DEFAULT-EXTENSION ".w"
            INITIAL-DIR fCustObjLocn:SCREEN-VALUE.
            ASSIGN 
                cCustObjLocn = substring(cFileName, 1,R-INDEX(cFileName,"\"))
                SELF:SCREEN-VALUE = cCustObjLocn.
            DO iCtr = 1 TO NUM-ENTRIES(cCustObjLocn,"\"):
                IF INDEX(ENTRY(iCtr,cCustObjLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cCustObjLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cCustObjLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cCustObjLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cCustObjLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cCustObjLocn.
                END.
            END.
        END.
        WHEN 'fEnvTempLocn' THEN DO:
            ASSIGN cFileName = "_tag.tpl".
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Template Files Source Directory"
            FILTERS "Template Files (*.tpl)"  "*.tpl"
            USE-FILENAME
            DEFAULT-EXTENSION ".tpl"
            INITIAL-DIR fEnvTempLocn:SCREEN-VALUE.
            ASSIGN 
                cEnvTempLocn = substring(cFileName, 1,R-INDEX(cFileName,"\"))
                SELF:SCREEN-VALUE = cEnvTempLocn.
            DO iCtr = 1 TO NUM-ENTRIES(cEnvTempLocn,"\"):
                IF INDEX(ENTRY(iCtr,cEnvTempLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cEnvTempLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cEnvTempLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cEnvTempLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cEnvTempLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cEnvTempLocn.
                END.
            END.
        END.
        WHEN 'fEnvTargetLocn' THEN DO:
            ASSIGN cFileName = "_tag.ini".
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Environment Files Target Directory"
            FILTERS "Startup Files (*.ini)"  "*.ini"
            CREATE-TEST-FILE
            USE-FILENAME
            DEFAULT-EXTENSION ".ini"
            INITIAL-DIR fEnvTargetLocn:SCREEN-VALUE.
            ASSIGN 
                cEnvTargetLocn = substring(cFileName, 1,R-INDEX(cFileName,"\"))
                SELF:SCREEN-VALUE = cEnvTargetLocn.
            DO iCtr = 1 TO NUM-ENTRIES(cEnvTargetLocn,"\"):
                IF INDEX(ENTRY(iCtr,cEnvTargetLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cEnvTargetLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cEnvTargetLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cEnvTargetLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cEnvTargetLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cEnvTargetLocn.
                END.
            END.
        END.
        WHEN 'fProgressLocn' THEN DO:
            ASSIGN cFileName = "progress.cfg".
            SYSTEM-DIALOG GET-FILE cFileName
            TITLE "Choose Progress Directory"
            FILTERS "Config Files (*.cfg)"  "*.cfg"
            USE-FILENAME
            MUST-EXIST
            DEFAULT-EXTENSION ".cfg"
            INITIAL-DIR fProgressLocn:SCREEN-VALUE.
            ASSIGN 
                cProgressLocn = substring(cFileName, 1,R-INDEX(cFileName,"\"))
                SELF:SCREEN-VALUE = cProgressLocn.
            DO iCtr = 1 TO NUM-ENTRIES(cProgressLocn,"\"):
                IF INDEX(ENTRY(iCtr,cProgressLocn,"\")," ") <> 0 
                /* OR LENGTH(ENTRY(iCtr,cProgressLocn,"\")) > 8 */ THEN DO:
                    REPLACE(ENTRY(iCtr,cProgressLocn,"\")," ","").
                    ASSIGN 
                        ENTRY(iCtr,cProgressLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cProgressLocn,"\"),1,6)," ") + "~~1"
                        SELF:SCREEN-VALUE = cProgressLocn.
                END.
            END.
        END.
    END CASE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tCreateDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tCreateDB wWin
ON VALUE-CHANGED OF tCreateDB IN FRAME fMain /* Create Database? */
OR VALUE-CHANGED OF tUpdateDB
OR VALUE-CHANGED OF tLoadData
OR VALUE-CHANGED OF tCreateBase
OR VALUE-CHANGED OF tCreateCustom
OR VALUE-CHANGED OF tCreateCustObj
OR VALUE-CHANGED OF tCreateObject
OR VALUE-CHANGED OF tCompileBase
OR VALUE-CHANGED OF tCompilePatch
OR VALUE-CHANGED OF tCompileCustom
OR VALUE-CHANGED OF tCompileSelected
OR VALUE-CHANGED OF tRunFIX
OR VALUE-CHANGED OF tCreateStartup
OR ENTRY OF tCreateDB
DO:
    /* enables and/or disables fill-ins based on tasks selected */
    ASSIGN
        fDBLocn:SENSITIVE = FALSE
        fDBName:SENSITIVE = FALSE
        fDBConnParms:SENSITIVE = FALSE
        fDFLocn:SENSITIVE = FALSE
        fDeltaLocn:SENSITIVE = FALSE
        fDataFilesLocn:SENSITIVE = FALSE
        fBaseCodeLocn:SENSITIVE = FALSE
        fPatchCodeLocn:SENSITIVE = FALSE
        fCopyBaseTo:SENSITIVE = FALSE
        fCustCodeLocn:SENSITIVE = FALSE
        fCopyCustTo:SENSITIVE = FALSE
        fObjCodeLocn:SENSITIVE = FALSE
        fCustObjLocn:SENSITIVE = FALSE
        fEnvTempLocn:SENSITIVE = FALSE
        fEnvTargetLocn:SENSITIVE = FALSE
        fProgressLocn:SENSITIVE = FALSE.

    IF tCreateDB:CHECKED = TRUE THEN ASSIGN
        fDBLocn:SENSITIVE = TRUE
        fDBName:SENSITIVE = TRUE
        fDFLocn:SENSITIVE = TRUE
        tUpdateDB:SENSITIVE = FALSE
        tCopyDFFiles:CHECKED = TRUE.
    ELSE ASSIGN
        tUpdateDB:SENSITIVE = TRUE.

    IF tUpdateDB:CHECKED = TRUE THEN ASSIGN
        fDBLocn:SENSITIVE = TRUE
        fDBName:SENSITIVE = TRUE
        fDBConnParms:SENSITIVE = TRUE
        fDeltaLocn:SENSITIVE = TRUE
        tCreateDB:SENSITIVE = FALSE
        tCopyDFFiles:CHECKED = TRUE.
    ELSE ASSIGN
        tCreateDB:SENSITIVE = TRUE.

    IF tLoadData:CHECKED = TRUE THEN ASSIGN
        fDBLocn:SENSITIVE = TRUE
        fDBName:SENSITIVE = TRUE
        fDBConnParms:SENSITIVE = TRUE
        fDataFilesLocn:SENSITIVE = TRUE.

    IF tCreateBase:CHECKED = TRUE THEN ASSIGN
        fBaseCodeLocn:SENSITIVE = TRUE
        fCopyBaseTo:SENSITIVE = TRUE
        tCopyBaseFiles:CHECKED = TRUE.
  
    IF tCreateCustom:CHECKED = TRUE THEN ASSIGN
        fCustCodeLocn:SENSITIVE = TRUE
        fCopyCustTo:SENSITIVE = TRUE
        tCopyCustFiles:CHECKED = TRUE.
  
    IF tCreateObject:CHECKED = TRUE THEN ASSIGN
        fBaseCodeLocn:SENSITIVE = TRUE
        fObjCodeLocn:SENSITIVE = TRUE.
  
    IF tCreateCustObj:CHECKED = TRUE THEN ASSIGN
        fCustCodeLocn:SENSITIVE = TRUE
        fCustObjLocn:SENSITIVE = TRUE.
  
    IF tCompileBase:CHECKED = TRUE THEN ASSIGN
        fDBLocn:SENSITIVE = TRUE
        fDBName:SENSITIVE = TRUE
        fDBConnParms:SENSITIVE = TRUE
        fBaseCodeLocn:SENSITIVE = TRUE
        fObjCodeLocn:SENSITIVE = TRUE.

    IF tCompilePatch:CHECKED = TRUE THEN ASSIGN
        fDBLocn:SENSITIVE = TRUE
        fDBName:SENSITIVE = TRUE
        fDBConnParms:SENSITIVE = TRUE
        fPatchCodeLocn:SENSITIVE = TRUE
        fObjCodeLocn:SENSITIVE = TRUE.

    IF tCompileCustom:CHECKED = TRUE THEN ASSIGN
        fDBLocn:SENSITIVE = TRUE
        fDBName:SENSITIVE = TRUE
        fDBConnParms:SENSITIVE = TRUE
        fBaseCodeLocn:SENSITIVE = TRUE
        fCustCodeLocn:SENSITIVE = TRUE
        fCustObjLocn:SENSITIVE = TRUE.

    IF tCompileSelected:CHECKED = TRUE THEN ASSIGN
        fDBLocn:SENSITIVE = TRUE
        fDBName:SENSITIVE = TRUE
        fDBConnParms:SENSITIVE = TRUE
        fBaseCodeLocn:SENSITIVE = TRUE
        fCustCodeLocn:SENSITIVE = TRUE
        fObjCodeLocn:SENSITIVE = TRUE
        fCustObjLocn:SENSITIVE = TRUE.

    IF tRunFix:CHECKED = TRUE THEN ASSIGN
        fDBLocn:SENSITIVE = TRUE
        fDBName:SENSITIVE = TRUE
        fDBConnParms:SENSITIVE = TRUE.

    IF tCreateStartup:CHECKED = TRUE THEN ASSIGN
        fDBLocn:SENSITIVE = TRUE
        fDBName:SENSITIVE = TRUE
        fDBConnParms:SENSITIVE = TRUE
        fBaseCodeLocn:SENSITIVE = TRUE
        fCopyBaseTo:SENSITIVE = TRUE
        fCustCodeLocn:SENSITIVE = TRUE
        fCopyCustTo:SENSITIVE = TRUE
        fObjCodeLocn:SENSITIVE = TRUE
        fCustObjLocn:SENSITIVE = TRUE
        fEnvTempLocn:SENSITIVE = TRUE
        fEnvTargetLocn:SENSITIVE = TRUE.
        
    ASSIGN
        lClearLogFirst = tClearLog:CHECKED IN FRAME fMain
        lErrorsOnlyLog = tLogErrors:CHECKED IN FRAME fMain
        lStopOnError = tStopOnError:CHECKED IN FRAME fMain.

    IF NOT lPPModified THEN RUN ipModifyPropath.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* write the copyright message on the status line */
STATUS DEFAULT "(c) 2005 Foresight Software, Inc. -- All Rights Reserved".
STATUS INPUT "(c) 2005 Foresight Software, Inc. -- All Rights Reserved".

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
  DISPLAY fiAsOfDate tConfigName tCreateDB tUpdateDB tLoadData tCreateBase 
          tCreateObject tCreateCustom tCreateCustObj tCompileBase tCompilePatch 
          tCompileCustom tCompileSelected tRunFix tCreateStartup tCopyDFFiles 
          tCopyBaseFiles tCopyCustFiles tConnectMulti tClearLog tStopOnError 
          tLogErrors cbSuggest fLogDir fDBLocn fDBName fDBConnParms fDFLocn 
          fDeltaLocn fDataFilesLocn fBaseCodeLocn fPatchCodeLocn fCopyBaseTo 
          fCustCodeLocn fCopyCustTo fObjCodeLocn fCustObjLocn tHelp fEnvTempLocn 
          fEnvTargetLocn fProgressLocn 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE tCreateDB tUpdateDB tLoadData tCreateBase tCreateObject tCreateCustom 
         tCreateCustObj tCompileBase tCompilePatch tCompileCustom 
         tCompileSelected tRunFix tCreateStartup tCopyDFFiles tCopyBaseFiles 
         tCopyCustFiles tConnectMulti tClearLog tStopOnError tLogErrors 
         bRunDBAdmin bClear bModPropath bSaveConfig cbSuggest fLogDir tHelp 
         bProcess bQuit RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
    
    /* save it so we can restore it after modification */
    ASSIGN cOldPropath = PROPATH.

    /*
    /* if we can find a config file, use it */
    IF SEARCH("winkit.cfg") <> ? THEN DO:
        ASSIGN 
            cThisConfig = "fsInstall.cfg"
            fLogDir:SCREEN-VALUE IN FRAME fMain = REPLACE(SEARCH("fsInstall.cfg"),"fsInstall.cfg","").
        RUN ipUseExisting.
    END.
    */
    IF SEARCH("fsiFIX.p") <> ? THEN ASSIGN
        tRunFix:SENSITIVE IN FRAME fMain = TRUE.
    ELSE ASSIGN 
        tRunFix:SENSITIVE IN FRAME fMain = TRUE.
    
    IF SEARCH("delta.df") <> ? THEN ASSIGN
        tCompileBase:CHECKED IN FRAME fMain = TRUE
        tCompileBase:SENSITIVE IN FRAME fMain = TRUE
        tCompilePatch:CHECKED IN FRAME fMain = TRUE
        tCompilePatch:SENSITIVE IN FRAME fMain = TRUE
        tCompileCustom:CHECKED IN FRAME fMain = TRUE
        tCompileCustom:SENSITIVE IN FRAME fMain = TRUE
        tCompileSelected:SENSITIVE IN FRAME fMain = TRUE.
        
    IF CONNECTED("ptdb1") THEN ASSIGN
        tCreateDB:CHECKED IN FRAME fMain = FALSE
        tCreateDB:SENSITIVE IN FRAME fMain = FALSE.
    
    IF SEARCH("prowin32.exe") <> ? THEN ASSIGN
        cProgressLocn = REPLACE (SUBSTRING(SEARCH("prowin32.exe"),1,R-INDEX(SEARCH("prowin32.exe"),"\")),"bin\","")
        fProgressLocn:SCREEN-VALUE IN FRAME fMain = cProgressLocn.

    /* place the cursor in the top left field */
    APPLY "entry" TO tCreateDB IN FRAME fMain.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipClearEntries wWin 
PROCEDURE ipClearEntries :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* clear all fields, all check boxes */
ASSIGN
    fDBLocn:SCREEN-VALUE IN FRAME fMain = ""
    fDBName:SCREEN-VALUE IN FRAME fMain = ""
    fDBConnParms:SCREEN-VALUE IN FRAME fMain = ""
    fDFLocn:SCREEN-VALUE IN FRAME fMain = ""
    fDeltaLocn:SCREEN-VALUE IN FRAME fMain = ""
    fDataFilesLocn:SCREEN-VALUE IN FRAME fMain = ""
    fBaseCodeLocn:SCREEN-VALUE IN FRAME fMain = ""
    fPatchCodeLocn:SCREEN-VALUE IN FRAME fMain = ""
    fCopyBaseTo:SCREEN-VALUE IN FRAME fMain = ""
    fCustCodeLocn:SCREEN-VALUE IN FRAME fMain = ""
    fCopyCustTo:SCREEN-VALUE IN FRAME fMain = ""
    fObjCodeLocn:SCREEN-VALUE IN FRAME fMain = ""
    fCustObjLocn:SCREEN-VALUE IN FRAME fMain = ""
    fEnvTempLocn:SCREEN-VALUE IN FRAME fMain = ""
    fEnvTargetLocn:SCREEN-VALUE IN FRAME fMain = ""
    fProgressLocn:SCREEN-VALUE IN FRAME fMain = ""
    tCreateDB:CHECKED IN FRAME fMain = FALSE
    tUpdateDB:CHECKED IN FRAME fMain = FALSE
    tLoadData:CHECKED IN FRAME fMain = FALSE
    tCreateBase:CHECKED IN FRAME fMain = FALSE
    tCreateCustom:CHECKED IN FRAME fMain = FALSE
    tCreateObject:CHECKED IN FRAME fMain = FALSE
    tCompileBase:CHECKED IN FRAME fMain = FALSE
    tCompilePatch:CHECKED IN FRAME fMain = FALSE
    tCompileCustom:CHECKED IN FRAME fMain = FALSE
    tCompileSelected:CHECKED IN FRAME fMain = FALSE
    tRunFIX:CHECKED IN FRAME fMain = FALSE
    tCreateStartup:CHECKED IN FRAME fMain = FALSE
    tClearLog:CHECKED IN FRAME fMain = FALSE
    tStopOnError:CHECKED IN FRAME fMain = FALSE
    tLogErrors:CHECKED IN FRAME fMain = FALSE
    tCopyDFFiles:CHECKED IN FRAME fMain = FALSE
    tCopyBaseFiles:CHECKED IN FRAME fMain = FALSE
    tCopyCustFiles:CHECKED IN FRAME fMain = FALSE
    tConnectMulti:CHECKED IN FRAME fMain = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCompileBase wWin 
PROCEDURE ipCompileBase :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipWriteMessage (INPUT "Initiating ipCompileBase in fsInstaller.w").
    
    ASSIGN 
        cBaseFromLocn = cOutputDir1
        cBaseToLocn = cObjCodeLocn
        iCtr1 = 0
        iCtr2 = 0.
    
    /* Check for missing data */
    IF cBaseFromLocn = "" THEN DO:
        RUN ipWriteMessage (INPUT "ERROR: No base source location given.  Cancelling execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cBaseToLocn = "" THEN DO:
        RUN ipWriteMessage (INPUT "ERROR: No base target location given.  Cancelling execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    
    /* Clean up the filename(s) so we know what to expect later */
    ASSIGN
        cBaseFromLocn = RIGHT-TRIM(cBaseFromLocn,"/\") + "\"
        cBaseFromLocn = REPLACE(cBaseFromLocn,"/","\")
        cBaseToLocn = RIGHT-TRIM(cBaseToLocn,"/\") + "\"
        cBaseToLocn = REPLACE(cBaseToLocn,"/","\").
        
    /* Walk up the source path 3 levels to make sure the propath gets everything */
    /* Ex: "C:/1/2/3/" becomes "C:/1/2/3/,C:/1/2/,C:/1/,"                        */
    ASSIGN cTxt1 = cBaseFromLocn.
    DO iCtr1 = 1 TO 3:
        IF LOOKUP(cTxt1,cTxt2) = 0 THEN ASSIGN PROPATH = PROPATH + "," + cTxt1.
        cTxt1 = SUBSTRING(cTxt1,1,LENGTH(cTxt1) - 1).
        cTxt1 = SUBSTRING(cTxt1,1,R-INDEX(cTxt1,"\")).
    END.
        
    /* Next use of iCtr1 is NOT a do loop; reset it here */
    ASSIGN iCtr1 = 0.

    RUN ipCompileBase2 (INPUT cBaseFromLocn, INPUT cBaseToLocn).
    IF LASTKEY = KEYCODE("ESC") THEN RETURN.
    
    RUN ipWriteMessage (INPUT "Base Code compile (" + STRING(iCtr1 + iCtr2) + " entries) completed with " + STRING(iCtr2) + " errors").

    RUN ipWriteMessage (INPUT "Completing ipCompileBase in fsInstaller.w").

    IF lStopOnError AND lFoundError THEN RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCompileBase2 wWin 
PROCEDURE ipCompileBase2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER cCompileFrom   AS CHAR NO-UNDO.
    DEF INPUT PARAMETER cCompileTo     AS CHAR FORMAT "x(60)".

    DEF VAR lv-filename     AS CHAR INIT "c:\autoexec.bat" NO-UNDO.
    DEF VAR lv-attribs      AS INTEGER NO-UNDO.
    DEF VAR lv-attribs-list AS CHAR NO-UNDO.
    def var cXtra as char no-undo.
    def var cdispName as char no-undo.

    /* Clean up the filename(s) so we know what to expect later */
    ASSIGN 
        cCleanCompileTo = REPLACE(cCompileTo,"/","\")
        cCleanBaseFrom = REPLACE(cBaseFromLocn,"/","\").
    
    /* This will show execution status to user without requiring interaction */
    FORM 
        "Press ESC to cancel..." AT 2
        SKIP(1)
        "Compiling: " AT 2 cDispName FORMAT "x(60)"    AT 13 NO-LABEL
        "To: "        AT 2 
        "Number of Errors: " AT 2 SPACE(1) iCtr2 NO-LABEL
        SKIP(1)
        "(c) 2005, Foresight Software, Inc." AT 2
        "All rights reserved." TO 68
        WITH FRAME compdisp TITLE "fsInstaller - Compiling Files"
        THREE-D SIDE-LABELS VIEW-AS DIALOG-BOX WIDTH 77.
    
    /* Start reading from source dir */
    INPUT FROM OS-DIR (cCompileFrom).
    REPEAT:
        IMPORT cName cFullName cAttrList.
        ASSIGN cCleanFullName = REPLACE(cFullName,"/","\").
        /* If this is a directory... */
        IF SUBSTRING(cFullName,LENGTH(cFullName),1) <> "." 
        AND INDEX(cAttrList,"D") <> 0 THEN DO:
            /* Build directory if required (shouldn't be), and write a single message to the log to indicate 
            directory compiling; individual msgs get lost */
            
            OS-CREATE-DIR VALUE(cCleanCompileTo + replace(cCleanFullName,cCleanBaseFrom,"")). 
            /* RUN ipWriteMessage (INPUT "Creating dir " + STRING(cCleanCompileTo + replace(cCleanFullName,cCleanBaseFrom,""),"x(155)")). */
            
            RUN ipCompileBase2 (INPUT cFullName, INPUT cCompileTo).
        END.
        /* If this is a procedure file... */
        ELSE IF INDEX(cAttrList,"F") <> 0 
        and length(cName) > 2
        and (substring(cName,length(cName) - 1,2) = ".p" or
             substring(cName,length(cName) - 1,2) = ".w" or
             (length(cName) GT 3 and substring(cName,length(cName) - 3,4) = ".cls"))
        THEN DO:
            /* Display the message on screen */
            ASSIGN 
                cDispName = replace(cFullName,cBaseFromLocn,"")
                cMyVar = STRING(cCleanCompileTo) + REPLACE(REPLACE(cCleanFullName,cCleanBaseFrom,""),cName,"").
            DISPLAY cDispName  iCtr2 WITH FRAME compdisp.
            /* and do the actual compile */
            /*
            assign cXtra = replace(cFullName,cBaseFromLocn,"")
            cXtra = replace(cxtra,cName,"").
            */
            IF cDispName = "oe\oe850imp.p" 
            AND INDEX(tConfigName:SCREEN-VALUE IN FRAME fMain,"v10.cfg") <> 0 THEN.
            ELSE
            COMPILE value(replace(cFullName,cBaseFromLocn,"")) XREF value(cBaseToLocn) NO-ERROR. 
            
            
            /* If compile failed, write message to the log and otherwise handle it */
            IF ERROR-STATUS:GET-MESSAGE(1) <> "" THEN DO:
                IF INDEX(ERROR-STATUS:GET-MESSAGE(1),"(9430)") <> 0 THEN DO:
                    RUN ipWriteMessage (INPUT "WARNING: Progam " + cName + " contains multiple action code segments.   ").
                END.
                ELSE IF INDEX(ERROR-STATUS:GET-MESSAGE(1),"(214)") <> 0 THEN DO:  /* Transaction in transaction */
                END.
                ELSE DO:
                    ASSIGN
                        iCtr2 = iCtr2 + 1
                        lFoundError = TRUE.
                    RUN ipWriteMessage (INPUT "  ERROR: Compile for " + cFullName + " FAILED").
                    RUN ipWriteMessage (INPUT "  ERROR:  " + ERROR-STATUS:GET-MESSAGE(1)).
                    RUN ipWriteMessage (INPUT "  ERROR:  " + ERROR-STATUS:GET-MESSAGE(2)).
                    IF lStopOnError THEN RETURN.
                END.
            END.
            ELSE DO:
                ASSIGN iCtr1 = iCtr1 + 1.
                IF NOT lErrorsOnlyLog THEN RUN ipWriteMessage (INPUT "  Compile for " + cFullName + " SUCCEEDED").
            END.
        END.
        /* 3 pgms have to compile while the DB is NOT connected. handle those here */
        ELSE IF INDEX(cAttrList,"F") <> 0 
        AND (INDEX(cName,"probase.p") <> 0 
        OR INDEX(cName,"userpwd") <> 0
        ) THEN DO:
            IF INDEX(cName,"probase.p") <> 0 THEN ASSIGN
                xcomp[1] = cFullName
                xcomp[2] = cCleanCompileTo + REPLACE(REPLACE(cCleanFullName,cCleanBaseFrom,""),cName,"").
            IF INDEX(cName,"userpwd.w") <> 0 THEN ASSIGN
                xcomp[3] = cFullName
                xcomp[4] = cCleanCompileTo + REPLACE(REPLACE(cCleanFullName,cCleanBaseFrom,""),cName,"").
            IF INDEX(cName,"userpwd2.w") <> 0 THEN ASSIGN
                xcomp[5] = cFullName
                xcomp[6] = cCleanCompileTo + REPLACE(REPLACE(cCleanFullName,cCleanBaseFrom,""),cName,"").
            
        END.
        
        /* If this is a file that should be copied to object code without compile file... */
        ELSE IF INDEX(cAttrList,"F") <> 0  /* it's a file */
        AND (SUBSTRING(cName,length(cName) - 1,2) <> ".i"   /* but not an include */
            OR INDEX(cName,".r") <> 0)                      /* and not r-code */
        THEN DO:
            /* Display the message on screen */
            ASSIGN cMyVar = STRING(cCleanCompileTo) + REPLACE(REPLACE(cCleanFullName,cCleanBaseFrom,""),cName,"").
            DISPLAY cdispName  iCtr2 WITH FRAME compdisp.
            
            /* check the target's RO bit, and reset if necessary */
            ASSIGN 
                iCtr1 = iCtr1 + 1
                lv-attribs-list = "".

            RUN GetFileAttributesA ( (cCleanCompileTo + REPLACE(cCleanFullName,cCleanBaseFrom,"")), OUTPUT lv-attribs ).
            
            RUN CheckOneAttribute( lv-attribs, 
                       1,  
                       "READONLY",
                       INPUT-OUTPUT lv-attribs-list).
                       
            IF INDEX(lv-attribs-list,"READONLY") <> 0 THEN
                RUN SetFileAttributesA ( (cCleanCompileTo + REPLACE(cCleanFullName,cCleanBaseFrom,"")), lv-attribs - 1).
            
            /* check the source's RO bit, and reset if necessary */
            ASSIGN 
                lv-attribs-list = "".

            RUN GetFileAttributesA ( cFullName, OUTPUT lv-attribs ).
            
            RUN CheckOneAttribute( lv-attribs, 
                       1,  
                       "READONLY",
                       INPUT-OUTPUT lv-attribs-list).
                       
            IF INDEX(lv-attribs-list,"READONLY") <> 0 THEN
                RUN SetFileAttributesA ( cFullName, lv-attribs - 1).
            
            /* now copy source to target */
            OS-COPY VALUE(cFullName) VALUE(cCleanCompileTo + replace(cCleanFullName,cCleanBaseFrom,"")). 
            
            IF NOT lErrorsOnlyLog THEN RUN ipWriteMessage (INPUT "  Copy for " + cFullName + " SUCCEEDED").
        END.
        
        /* One of these lastkeys allows trapping during Process Events; don't know which one */
        IF LASTKEY = KEYCODE("ESC") THEN RETURN.
        PROCESS EVENTS.
        IF LASTKEY = KEYCODE("ESC") THEN RETURN.
    END.

END PROCEDURE.

PROCEDURE SetFileAttributesA EXTERNAL "kernel32" :
   DEFINE INPUT PARAMETER lpFilename AS CHAR.
   DEFINE INPUT PARAMETER dwFileAttributes AS LONG.
END.

PROCEDURE GetFileAttributesA EXTERNAL "kernel32" :
   DEFINE INPUT PARAMETER lpFilename AS CHAR.
   DEFINE RETURN PARAMETER dwFileAttributes AS LONG.
END.

PROCEDURE CheckOneAttribute :
  DEFINE INPUT PARAMETER lp-attribs  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER lp-attrib-num  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER lp-attrib-name  AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER lp-attrib-list  AS CHARACTER NO-UNDO.
 
  IF lp-attribs MOD ( lp-attrib-num * 2 ) GE lp-attrib-num THEN
     lp-attrib-list = lp-attrib-list + MIN(lp-attrib-list,", ") + lp-attrib-name.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCompileCustom wWin 
PROCEDURE ipCompileCustom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipWriteMessage (INPUT "Initiating ipCompileCustom in fsInstaller.w").
    
    ASSIGN 
        cBaseFromLocn = cOutputDir2
        cBaseToLocn = cCustObjLocn
        iCtr1 = 0
        iCtr2 = 0.
        
    /* Check for missing data */
    IF cBaseFromLocn = "" THEN DO:
        RUN ipWriteMessage (INPUT "ERROR: No custom source location given.  Cancelling execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cBaseToLocn = "" THEN DO:
        RUN ipWriteMessage (INPUT "ERROR: No custom target location given.  Cancelling execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    
    /* Clean up the filename(s) so we know what to expect later */
    ASSIGN
        cBaseFromLocn = RIGHT-TRIM(cBaseFromLocn,"/\") + "\"
        cBaseFromLocn = REPLACE(cBaseFromLocn,"/","\")
        cBaseToLocn = RIGHT-TRIM(cBaseToLocn,"/\") + "\"
        cBaseToLocn = REPLACE(cBaseToLocn,"/","\").
        
    /* Walk up the source path 3 levels to make sure the propath gets everything */
    /* Ex: "C:/1/2/3/" becomes "C:/1/2/3/,C:/1/2/,C:/1/,"                        */
    ASSIGN cTxt1 = cBaseFromLocn.
    DO iCtr1 = 1 TO 3:
        IF LOOKUP(cTxt1,cTxt2) = 0 THEN ASSIGN PROPATH = PROPATH + "," + cTxt1.
        cTxt1 = SUBSTRING(cTxt1,1,LENGTH(cTxt1) - 1).
        cTxt1 = SUBSTRING(cTxt1,1,R-INDEX(cTxt1,"\")).
    END.
        
    /* Next use of iCtr1 is NOT a do loop; reset it here */
    ASSIGN iCtr1 = 0.

    RUN ipCompileBase2 (INPUT cBaseFromLocn, INPUT cBaseToLocn).
    IF LASTKEY = KEYCODE("ESC") THEN RETURN.

    RUN ipWriteMessage (INPUT "Custom Code compile (" + STRING(iCtr1 + iCtr2) + " entries) completed with " + STRING(iCtr2) + " errors").

    RUN ipWriteMessage (INPUT "Completing ipCompileCustom in fsInstaller.w").

    IF lStopOnError AND lFoundError THEN RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCompilePatch wWin 
PROCEDURE ipCompilePatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipWriteMessage (INPUT "Initiating ipCompilePatch in fsInstaller.w").
    
    ASSIGN 
        cBaseFromLocn = cPatchCodeLocn
        cBaseToLocn = cObjCodeLocn
        iCtr1 = 0
        iCtr2 = 0.
    
    /* Check for missing data */
    IF cBaseFromLocn = "" THEN DO:
        RUN ipWriteMessage (INPUT "ERROR: No patch source location given.  Cancelling execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cBaseToLocn = "" THEN DO:
        RUN ipWriteMessage (INPUT "ERROR: No base target location given.  Cancelling execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    
    /* Clean up the filename(s) so we know what to expect later */
    ASSIGN
        cBaseFromLocn = RIGHT-TRIM(cBaseFromLocn,"/\") + "\"
        cBaseFromLocn = REPLACE(cBaseFromLocn,"/","\")
        cBaseToLocn = RIGHT-TRIM(cBaseToLocn,"/\") + "\"
        cBaseToLocn = REPLACE(cBaseToLocn,"/","\").
        
    /* Walk up the source path 3 levels to make sure the propath gets everything */
    /* Ex: "C:/1/2/3/" becomes "C:/1/2/3/,C:/1/2/,C:/1/,"                        */
    ASSIGN cTxt1 = cBaseFromLocn.
    DO iCtr1 = 1 TO 3:
        IF LOOKUP(cTxt1,cTxt2) = 0 THEN ASSIGN PROPATH = PROPATH + "," + cTxt1.
        cTxt1 = SUBSTRING(cTxt1,1,LENGTH(cTxt1) - 1).
        cTxt1 = SUBSTRING(cTxt1,1,R-INDEX(cTxt1,"\")).
    END.
        
    /* Next use of iCtr1 is NOT a do loop; reset it here */
    ASSIGN iCtr1 = 0.

    RUN ipCompileBase2 (INPUT cBaseFromLocn, INPUT cBaseToLocn).
    IF LASTKEY = KEYCODE("ESC") THEN RETURN.
    
    RUN ipWriteMessage (INPUT "Patch Code compile (" + STRING(iCtr1 + iCtr2) + " entries) completed with " + STRING(iCtr2) + " errors").

    RUN ipWriteMessage (INPUT "Completing ipCompilePatch in fsInstaller.w").

    IF lStopOnError AND lFoundError THEN RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCompileSelected wWin 
PROCEDURE ipCompileSelected :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN ipWriteMessage (INPUT "Initiating ipCompileSelected in fsInstaller.w").
  
  /* Build the target directory if it doesn't already exist */
  DEF VAR vTest1 AS CHAR NO-UNDO.
  DEF VAR vTest2 AS CHAR NO-UNDO.
  def var iCtr4 as int no-undo.
  
  ASSIGN 
    vTest1 = REPLACE(cObjCodeLocn,"/","\")
    vTest2 = "".
  DO iCtr = 1 TO NUM-ENTRIES(vTest1,"\"):
      ASSIGN vTest2 = vTest2 + ENTRY(iCtr,vTest1,"\") + "\".
      OS-CREATE-DIR VALUE(vTest1).
  END.
  OS-CREATE-DIR VALUE(vTest2).

  ASSIGN 
    vTest1 = REPLACE(cCustObjLocn,"/","\")
    vTest2 = "".
  DO iCtr = 1 TO NUM-ENTRIES(vTest1,"\"):
      ASSIGN vTest2 = vTest2 + ENTRY(iCtr,vTest1,"\") + "\".
      OS-CREATE-DIR VALUE(vTest1).
  END.
  OS-CREATE-DIR VALUE(vTest2).

  ASSIGN 
      iCtr1 = 0
      iCtr4 = 0.
  basecomp:
  DO iCtr3 = 1 TO NUM-ENTRIES(cBaseList):
    
    ASSIGN 
        cBaseFromLocn = cOutputDir1 + ENTRY(iCtr3,cBaseList)
        cBaseToLocn = cObjCodeLocn + ENTRY(iCtr3,cBaseList).
        
    /* Check for missing data */
    IF cBaseFromLocn = "" THEN DO:
        RUN ipWriteMessage (INPUT "ERROR: No base source location given.  Cancelling execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cBaseToLocn = "" THEN DO:
        RUN ipWriteMessage (INPUT "ERROR: No base target location given.  Cancelling execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.

    IF INDEX(cBaseFromLocn,".") = 0
    OR R-INDEX(cBaseFromLocn,"\") >= R-INDEX(cBaseFromLocn,".") 
    OR R-INDEX(cBaseFromLocn,"/") >= R-INDEX(cBaseFromLocn,".") 
    THEN DO:
        /* Clean up the filename(s) so we know what to expect later */
        ASSIGN
            cBaseFromLocn = RIGHT-TRIM(cBaseFromLocn,"/\") + "\"
            cBaseFromLocn = REPLACE(cBaseFromLocn,"/","\")
            cBaseToLocn = RIGHT-TRIM(cBaseToLocn,"/\") + "\"
            cBaseToLocn = REPLACE(cBaseToLocn,"/","\").

        OS-CREATE-DIR VALUE(cBaseToLocn).
    END.
    ELSE IF INDEX(cBaseFromLocn,".") <> 0 THEN DO: 
        ASSIGN
            cBaseFromLocn = RIGHT-TRIM(cBaseFromLocn,"/\") + "\"
            cBaseFromLocn = REPLACE(cBaseFromLocn,"/","\")
            cBaseToLocn = RIGHT-TRIM(cBaseToLocn,"/\") + "\"
            cBaseToLocn = REPLACE(cBaseToLocn,"/","\")
            cBaseFromLocn = RIGHT-TRIM(cBaseFromLocn,"\")
            cBaseToLocn = SUBSTRING(cBaseToLocn,1,R-INDEX(cBaseToLocn,"\") - 1)
            cBaseToLocn = SUBSTRING(cBaseToLocn,1,R-INDEX(cBaseToLocn,"\") - 1).

        COMPILE VALUE(cBaseFromLocn) XREF value(cBaseToLocn) NO-ERROR.

            
        /* If compile failed, write message to the log and otherwise handle it */
        IF ERROR-STATUS:GET-MESSAGE(1) <> "" THEN DO:
            IF INDEX(ERROR-STATUS:GET-MESSAGE(1),"(9430)") <> 0 THEN DO:
                RUN ipWriteMessage (INPUT "WARNING: Progam " + cBaseFromLocn + " contains multiple action code segments.   ").
            END.
            ELSE IF INDEX(ERROR-STATUS:GET-MESSAGE(1),"(4027)") <> 0 THEN DO:
                RUN ipWriteMessage (INPUT "WARNING: Progam " + cBaseFromLocn + " contains incorrect formatting.   ").
            END.
            ELSE DO:
                ASSIGN
                    iCtr2 = iCtr2 + 1
                    lFoundError = TRUE.
                RUN ipWriteMessage (INPUT "  ERROR: Compile for " + cBaseFromLocn + " FAILED").
                RUN ipWriteMessage (INPUT "  ERROR:  " + ERROR-STATUS:GET-MESSAGE(1)).
                RUN ipWriteMessage (INPUT "  ERROR:  " + ERROR-STATUS:GET-MESSAGE(2)).
                IF lStopOnError THEN RETURN.
            END.
        END.
        ELSE DO:
        ASSIGN 
            iCtr1 = iCtr1 + 1
            iCtr4 = iCtr4 + 1.
            IF NOT lErrorsOnlyLog THEN RUN ipWriteMessage (INPUT "  Compile for " + cBaseFromLocn + " SUCCEEDED").
        END.
        
        NEXT basecomp.
    END.

    /* Walk up the source path 3 levels to make sure the propath gets everything */
    /* Ex: "C:/1/2/3/" becomes "C:/1/2/3/,C:/1/2/,C:/1/,"                        */
    ASSIGN cTxt1 = cBaseFromLocn.
    DO iCtr1 = 1 TO 3:
        cTxt2 = cTxt2 + cTxt1 + ",".
        cTxt1 = SUBSTRING(cTxt1,1,LENGTH(cTxt1) - 1).
        cTxt1 = SUBSTRING(cTxt1,1,R-INDEX(cTxt1,"\")).
    END.
    IF INDEX(PROPATH,cTxt2) = 0 THEN ASSIGN PROPATH = cTxt2 + PROPATH.
        
    /* Next use of iCtr1 is NOT a do loop; reset it here */
    ASSIGN iCtr1 = 0.

    RUN ipCompileBase2 (INPUT cBaseFromLocn, INPUT cBaseToLocn).
    IF LASTKEY = KEYCODE("ESC") THEN RETURN.

    assign iCtr4 = iCtr4 + iCtr1.

    RUN ipWriteMessage (INPUT "Code compile (" + STRING(iCtr1 + iCtr2) + " entries) completed with " + STRING(iCtr2) + " errors").

    IF lStopOnError AND lFoundError THEN RETURN.

  END.

  custcomp:
  DO iCtr3 = 1 TO NUM-ENTRIES(cCustList):
    
    ASSIGN 
        cBaseFromLocn = cOutputDir1 + ENTRY(iCtr3,cCustList)
        cBaseToLocn = cCustObjLocn + ENTRY(iCtr3,cCustList).
        
    /* Check for missing data */
    IF cBaseFromLocn = "" THEN DO:
        RUN ipWriteMessage (INPUT "ERROR: No base source location given.  Cancelling execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cBaseToLocn = "" THEN DO:
        RUN ipWriteMessage (INPUT "ERROR: No base target location given.  Cancelling execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    
    IF INDEX(cBaseFromLocn,".") = 0
    OR R-INDEX(cBaseFromLocn,"\") >= R-INDEX(cBaseFromLocn,".") THEN DO:
        /* Clean up the filename(s) so we know what to expect later */
        ASSIGN
            cBaseFromLocn = RIGHT-TRIM(cBaseFromLocn,"/\") + "\"
            cBaseFromLocn = REPLACE(cBaseFromLocn,"/","\")
            cBaseToLocn = RIGHT-TRIM(cBaseToLocn,"/\") + "\"
            cBaseToLocn = REPLACE(cBaseToLocn,"/","\").

        OS-CREATE-DIR VALUE(cBaseToLocn).
    END.
    ELSE IF INDEX(cBaseFromLocn,".") <> 0 THEN DO: 
        ASSIGN
            cBaseFromLocn = RIGHT-TRIM(cBaseFromLocn,"/\") + "\"
            cBaseFromLocn = REPLACE(cBaseFromLocn,"/","\")
            cBaseToLocn = RIGHT-TRIM(cBaseToLocn,"/\") + "\"
            cBaseToLocn = REPLACE(cBaseToLocn,"/","\")
            cBaseFromLocn = RIGHT-TRIM(cBaseFromLocn,"\")
            cBaseToLocn = SUBSTRING(cBaseToLocn,1,R-INDEX(cBaseToLocn,"\") - 1)
            cBaseToLocn = SUBSTRING(cBaseToLocn,1,R-INDEX(cBaseToLocn,"\") - 1).

        COMPILE VALUE(cBaseFromLocn) XREF value(cBaseToLocn) NO-ERROR.

            
        /* If compile failed, write message to the log and otherwise handle it */
        IF ERROR-STATUS:GET-MESSAGE(1) <> "" THEN DO:
            IF INDEX(ERROR-STATUS:GET-MESSAGE(1),"(9430)") <> 0 THEN DO:
                RUN ipWriteMessage (INPUT "WARNING: Progam " + SUBSTRING(cBaseFromLocn,1,6) + "..." + SUBSTRING(cBaseFromLocn,LENGTH(cBaseFromLocn) - 20) + " contains multiple action code segments.   ").
            END.
            ELSE IF INDEX(ERROR-STATUS:GET-MESSAGE(1),"(4027)") <> 0 THEN DO:
                RUN ipWriteMessage (INPUT "WARNING: Progam " + SUBSTRING(cBaseFromLocn,1,6) + "..." + SUBSTRING(cBaseFromLocn,LENGTH(cBaseFromLocn) - 20) + " contains incorrect formatting.   ").
            END.
            ELSE DO:
                ASSIGN
                    iCtr2 = iCtr2 + 1
                    lFoundError = TRUE.
                RUN ipWriteMessage (INPUT "  ERROR: Compile for " + cBaseFromLocn + " FAILED").
                RUN ipWriteMessage (INPUT "  ERROR:  " + ERROR-STATUS:GET-MESSAGE(1)).
                RUN ipWriteMessage (INPUT "  ERROR:  " + ERROR-STATUS:GET-MESSAGE(2)).
                IF lStopOnError THEN RETURN.
            END.
        END.
        ELSE DO:
        ASSIGN 
            iCtr1 = iCtr1 + 1
            iCtr4 = iCtr4 + 1.
            IF NOT lErrorsOnlyLog THEN RUN ipWriteMessage (INPUT "  Compile for " + cBaseFromLocn + " SUCCEEDED").
        END.
        
        NEXT custcomp.
    END.

    /* Walk up the source path 3 levels to make sure the propath gets everything */
    /* Ex: "C:/1/2/3/" becomes "C:/1/2/3/,C:/1/2/,C:/1/,"                        */
    ASSIGN cTxt1 = cBaseFromLocn.
    DO iCtr1 = 1 TO 3:
        cTxt2 = cTxt2 + cTxt1 + ",".
        cTxt1 = SUBSTRING(cTxt1,1,LENGTH(cTxt1) - 1).
        cTxt1 = SUBSTRING(cTxt1,1,R-INDEX(cTxt1,"\")).
    END.
    IF INDEX(PROPATH,cTxt2) = 0 THEN ASSIGN PROPATH = cTxt2 + PROPATH.
        
    /* Next use of iCtr1 is NOT a do loop; reset it here */
    ASSIGN iCtr1 = 0.

    RUN ipCompileBase2 (INPUT cBaseFromLocn, INPUT cBaseToLocn).
    IF LASTKEY = KEYCODE("ESC") THEN RETURN.

    assign iCtr4 = iCtr4 + iCtr1.

    RUN ipWriteMessage (INPUT "Code compile (" + STRING(iCtr1 + iCtr2) + " entries) completed with " + STRING(iCtr2) + " errors").

    IF lStopOnError AND lFoundError THEN RETURN.

  END.

  RUN ipWriteMessage (INPUT "Code compile (" + STRING(iCtr4) + " entries) completed with " + STRING(iCtr2) + " errors").

  RUN ipWriteMessage (INPUT "Completing ipCompileSelected in fsInstaller.w").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConnectDBs wWin 
PROCEDURE ipConnectDBs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipWriteMessage (INPUT "Initiating ipConnectDBs in fsInstaller.w").
    
    ASSIGN
        cDbLocn = RIGHT-TRIM(cDbLocn,"/\") + "\"
        cDbLocn = REPLACE(cDbLocn,"/","\")
        cDBFullName = RIGHT-TRIM(RIGHT-TRIM(cDBLocn,"/\") + "\" + REPLACE(cDBName1,".db","") + ".db","\").
    
    /* Now connect to target DB; write status msg */
    IF NOT CONNECTED("asi") THEN DO:
        IF INDEX(cDBParms1," -1 ") <> 0 THEN
            CONNECT VALUE(cDBFullName) VALUE(cDBParms1).
        ELSE
            CONNECT VALUE(REPLACE(cDBFullName,".db","")) VALUE(cDBParms1).
    END.
    ELSE DO:
        RUN ipWriteMessage (INPUT "Database " + LDBNAME(1) + " was previously connected").
        ASSIGN lDBConnected = TRUE.
    END.

    IF CONNECTED(LDBNAME(1)) THEN DO:
        RUN ipWriteMessage (INPUT "Database " + LDBNAME(1) + " connected successfully").
        ASSIGN lDBConnected = TRUE.
    END.
    ELSE DO:
        RUN ipWriteMessage (INPUT "ERROR: Database " + LDBNAME(1) + " failed to connect").
        ASSIGN 
            lDBConnected = FALSE
            lFoundError = TRUE.
        RETURN.
    END.
    
    /* Connect additional DBs if requested */
    IF tConnectMulti:CHECKED IN FRAME fMain THEN DO:
        IF cDBName2 <> "" AND NOT CONNECTED(cDBName2) THEN DO:
            IF INDEX(cDBParms2,"-1 ") = 1
            OR INDEX(cDBParms2," -1 ") <> 0
            OR INDEX(cDBParms2," -1") = LENGTH(cDBParms2) - 2 THEN
                CONNECT VALUE(cDBDir2 + cDBName2) VALUE(cDBParms2).
            ELSE 
                CONNECT VALUE(cDBName2) VALUE(cDBParms2).
        
            IF CONNECTED(LDBNAME(2)) THEN
                RUN ipWriteMessage (INPUT "Database " + cDBName2 + " connected as " + LDBNAME(2) + ".").
            ELSE DO:
                RUN ipWriteMessage (INPUT "ERROR: Database " + cDBName2 + " failed to connect").
                ASSIGN 
                    lFoundError = TRUE.
                RETURN.
            END.
        END.
        ELSE IF cDBName2 <> "" AND CONNECTED(cDBName2) THEN DO:
            RUN ipWriteMessage (INPUT "Database " + cDBName2 + " was previously connected").
        END.
        IF cDBName3 <> "" AND NOT CONNECTED(cDBName3) THEN DO:
            IF INDEX(cDBParms3,"-1 ") = 1
            OR INDEX(cDBParms3," -1 ") <> 0
            OR INDEX(cDBParms3," -1") = LENGTH(cDBParms3) - 2 THEN
                CONNECT VALUE(cDBDir3 + cDBName3) VALUE(cDBParms3).
            ELSE 
                CONNECT VALUE(cDBName3) VALUE(cDBParms3).
        
            IF CONNECTED(LDBNAME(3)) THEN
                RUN ipWriteMessage (INPUT "Database " + cDBName3 + " connected as " + LDBNAME(3) + ".").
            ELSE DO:
                RUN ipWriteMessage (INPUT "ERROR: Database " + cDBName3 + " failed to connect").
                ASSIGN 
                    lFoundError = TRUE.
                RETURN.
            END.
        END.
        ELSE IF cDBName3 <> "" AND CONNECTED(cDBName3) THEN DO:
            RUN ipWriteMessage (INPUT "Database " + cDBName3 + " was previously connected").
        END.
        IF cDBName4 <> "" AND NOT CONNECTED(cDBName4) THEN DO:
            IF INDEX(cDBParms4,"-1 ") = 1
            OR INDEX(cDBParms4," -1 ") <> 0
            OR INDEX(cDBParms4," -1") = LENGTH(cDBParms4) - 2 THEN
                CONNECT VALUE(cDBDir4 + cDBName4) VALUE(cDBParms4).
            ELSE 
                CONNECT VALUE(cDBName4) VALUE(cDBParms4).
        
            IF CONNECTED(LDBNAME(4)) THEN
                RUN ipWriteMessage (INPUT "Database " + cDBName4 + " connected as " + LDBNAME(4) + ".").
            ELSE DO:
                RUN ipWriteMessage (INPUT "ERROR: Database " + cDBName4 + " failed to connect").
                ASSIGN 
                    lFoundError = TRUE.
                RETURN.
            END.
        END.
        ELSE IF cDBName4 <> "" AND CONNECTED(cDBName4) THEN DO:
            RUN ipWriteMessage (INPUT "Database " + cDBName4 + " was previously connected").
        END.
        IF cDBName5 <> "" AND NOT CONNECTED(cDBName5) THEN DO:
            IF INDEX(cDBParms5,"-1 ") = 1
            OR INDEX(cDBParms5," -1 ") <> 0
            OR INDEX(cDBParms5," -1") = LENGTH(cDBParms5) - 2 THEN
                CONNECT VALUE(cDBDir5 + cDBName5) VALUE(cDBParms5).
            ELSE 
                CONNECT VALUE(cDBName5) VALUE(cDBParms5).
        
            IF CONNECTED(LDBNAME(5)) THEN
                RUN ipWriteMessage (INPUT "Database " + cDBName5 + " connected as " + LDBNAME(5) + ".").
            ELSE DO:
                RUN ipWriteMessage (INPUT "ERROR: Database " + cDBName5 + " failed to connect").
                ASSIGN 
                    lFoundError = TRUE.
                RETURN.
            END.
        END.
        ELSE IF cDBName5 <> "" AND CONNECTED(cDBName5) THEN DO:
            RUN ipWriteMessage (INPUT "Database " + cDBName5 + " was previously connected").
        END.
        IF cDBName6 <> "" AND NOT CONNECTED(cDBName6) THEN DO:
            IF INDEX(cDBParms6,"-1 ") = 1
            OR INDEX(cDBParms6," -1 ") <> 0
            OR INDEX(cDBParms6," -1") = LENGTH(cDBParms6) - 2 THEN
                CONNECT VALUE(cDBDir6 + cDBName6) VALUE(cDBParms6).
            ELSE 
                CONNECT VALUE(cDBName6) VALUE(cDBParms6).
        
            IF CONNECTED(LDBNAME(6)) THEN
                RUN ipWriteMessage (INPUT "Database " + cDBName6 + " connected as " + LDBNAME(6) + ".").
            ELSE DO:
                RUN ipWriteMessage (INPUT "ERROR: Database " + cDBName6 + " failed to connect").
                ASSIGN 
                    lFoundError = TRUE.
                RETURN.
            END.
        END.
        ELSE IF cDBName6 <> "" AND CONNECTED(cDBName6) THEN DO:
            RUN ipWriteMessage (INPUT "Database " + cDBName6 + " was previously connected").
        END.
    END.

    /* Note this creates all known aliases for "master" DB; customize as appropriate */
    if ldbname(1) = "ASI"
    /* and (ldbname(3) = "" or ldbname(3) = ?) */ then do:
        CREATE ALIAS "nosweat"  FOR DATABASE value(ldbname(1)).
        CREATE ALIAS "asinos"  FOR DATABASE value(ldbname(1)).
        CREATE ALIAS "emptrack"  FOR DATABASE value(ldbname(1)).
        CREATE ALIAS "rfq"  FOR DATABASE value(ldbname(1)).
        CREATE ALIAS "jobs"  FOR DATABASE value(ldbname(1)).
        CREATE ALIAS "asihlp"  FOR DATABASE value(ldbname(1)).
        CREATE ALIAS "asihelp"  FOR DATABASE value(ldbname(1)).
    end.
    else do:
        if connected("nosweat") then create alias "asinos" for database "nosweat".
        if connected("asihelp") then create alias "asihlp" for database "asihelp".
    end.
    RUN ipWriteMessage (INPUT "Completing ipConnectDBs in fsInstaller.w").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConnectMulti wWin 
PROCEDURE ipConnectMulti :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF BUTTON bOK LABEL "OK" SIZE 12 BY 1.
    DEF BUTTON bCancel LABEL "Cancel" SIZE 12 BY 1.

    DISPLAY
        cDBDir2 VIEW-AS FILL-IN SIZE 61 BY 1
        cDBName2 VIEW-AS FILL-IN SIZE 61 BY 1
        cDBParms2 VIEW-AS FILL-IN SIZE 61 BY 1
        SKIP(1)
        cDBDir3 VIEW-AS FILL-IN SIZE 61 BY 1
        cDBName3 VIEW-AS FILL-IN SIZE 61 BY 1
        cDBParms3 VIEW-AS FILL-IN SIZE 61 BY 1
        SKIP(1)
        cDBDir4 VIEW-AS FILL-IN SIZE 61 BY 1
        cDBName4 VIEW-AS FILL-IN SIZE 61 BY 1
        cDBParms4 VIEW-AS FILL-IN SIZE 61 BY 1
        SKIP(1)
        cDBDir5 VIEW-AS FILL-IN SIZE 61 BY 1
        cDBName5 VIEW-AS FILL-IN SIZE 61 BY 1
        cDBParms5 VIEW-AS FILL-IN SIZE 61 BY 1
        SKIP(1)
        cDBDir6 VIEW-AS FILL-IN SIZE 61 BY 1
        cDBName6 VIEW-AS FILL-IN SIZE 61 BY 1
        cDBParms6 VIEW-AS FILL-IN SIZE 61 BY 1
        WITH FRAME a TITLE "Enter Additional DB Connection Information" 1 COLUMN SIDE-LABELS THREE-D OVERLAY WIDTH 82 VIEW-AS DIALOG-BOX.
    ENABLE 
        cDBDir2 
        cDBName2
        cDBParms2
        cDBDir3
        cDBName3
        cDBParms3
        cDBDir4
        cDBName4
        cDBParms4
        cDBDir5
        cDBName5
        cDBParms5
        cDBDir6
        cDBName6
        cDBParms6
        bOK    AT ROW 20.8 COLUMN 56
        bCancel AT ROW 20.8 COLUMN 69
        WITH FRAME a.
    ON 'choose':U OF bOK IN FRAME a
    DO:
        ASSIGN
            cDBDir2 = cDBDir2:SCREEN-VALUE IN FRAME a
            cDBName2 = cDBName2:SCREEN-VALUE IN FRAME a
            cDBParms2 = cDBParms2:SCREEN-VALUE IN FRAME a
            cDBDir3 = cDBDir3:SCREEN-VALUE IN FRAME a
            cDBName3 = cDBName3:SCREEN-VALUE IN FRAME a
            cDBParms3 = cDBParms3:SCREEN-VALUE IN FRAME a
            cDBDir4 = cDBDir4:SCREEN-VALUE IN FRAME a
            cDBName4 = cDBName4:SCREEN-VALUE IN FRAME a
            cDBParms4 = cDBParms4:SCREEN-VALUE IN FRAME a
            cDBDir5 = cDBDir5:SCREEN-VALUE IN FRAME a
            cDBName5 = cDBName5:SCREEN-VALUE IN FRAME a
            cDBParms5 = cDBParms5:SCREEN-VALUE IN FRAME a
            cDBDir6 = cDBDir6:SCREEN-VALUE IN FRAME a
            cDBName6 = cDBName6:SCREEN-VALUE IN FRAME a
            cDBParms6 = cDBParms6:SCREEN-VALUE IN FRAME a.
        RETURN.
    END.
    ON 'choose':U OF bCancel IN FRAME a
    DO:
        ASSIGN tConnectMulti:CHECKED IN FRAME fMain = FALSE.
        RETURN.
    END.
    WAIT-FOR 'choose' OF bOK IN FRAME a
        OR 'choose' OF bCancel IN FRAME a.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateBase wWin 
PROCEDURE ipCreateBase :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipWriteMessage (INPUT "Initiating ipCreateBase in fsInstaller.w").
    
    /* Check for missing data */
    IF cBaseCodeLocn = "" 
    OR cBaseCodeLocn = "x-" THEN DO:
        MESSAGE "No base source location given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No base source location given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cCopyBaseTo = "" 
    OR cCopyBaseTo = "x-" THEN DO:
        MESSAGE "No base target location given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No base target location given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    
    /* Clean up the filename(s) so we know what to expect later */
    ASSIGN
        cBaseCodeLocn = RIGHT-TRIM(cBaseCodeLocn,"/\") + "\"
        cBaseCodeLocn = REPLACE(cBaseCodeLocn,"/","\")
        cCopyBaseTo = RIGHT-TRIM(cCopyBaseTo,"/\") + "\"
        cCopyBaseTo = REPLACE(cCopyBaseTo,"/","\").
        
    /* Build the target directory if it doesn't already exist */
    DEF VAR vTest AS CHAR NO-UNDO.
    DO iCtr = 1 TO NUM-ENTRIES(cCopyBaseTo,"\"):
        ASSIGN vTest = vTest + ENTRY(iCtr,cCopyBaseTo,"\") + "\".
        OS-CREATE-DIR VALUE(vTest).
    END.
    OS-CREATE-DIR VALUE(cCopyBaseTo).
    
    RUN ipCreateBase2 (INPUT cBaseCodeLocn, INPUT cCopyBaseTo).

    RUN ipWriteMessage (INPUT "Completing ipCreateBase in fsInstaller.w").

    IF lStopOnError AND lFoundError THEN RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateBase2 wWin 
PROCEDURE ipCreateBase2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER cCopyFrom   AS CHAR NO-UNDO.
    DEF INPUT PARAMETER cCopyTo     AS CHAR FORMAT "x(60)".

    /* Clean up the filename(s) so we know what to expect later */
    ASSIGN 
        cCleanCopyTo = REPLACE(cCopyTo,"\","\")
        cCleanBaseSrc = REPLACE(cBaseCodeLocn,"/","\").
    
    /* This will show execution status to user without requiring interaction */
    FORM
        "Press ESC to cancel..." AT 2
        SKIP(1)
        "Copying: " AT 2 cCleanFullName FORMAT "x(60)"   AT 12 NO-LABEL
        "To: "      AT 2 
        SKIP(1)
        "(c) 2005, Foresight Software, Inc." AT 2
        "All rights reserved." TO 68
        WITH FRAME copydisp TITLE "fsInstaller - Copying Base Files"
        THREE-D SIDE-LABELS VIEW-AS DIALOG-BOX WIDTH 75.
                                               
    /* Start reading from source dir */
    INPUT FROM OS-DIR (cCopyFrom).
    REPEAT:
        IMPORT cName cFullName cAttrList.
        ASSIGN cCleanFullName = REPLACE(cFullName,"/","\").
        IF INDEX(cAttrList,"D") <> 0 AND cName <> "." AND cName <> ".." THEN DO:
            /* If necessary, create a target subdir */
            OS-CREATE-DIR VALUE(cCleanCopyTo + replace(cCleanFullName,cCleanBaseSrc,"")). 
            /* Note type of build in log, as user specified */
            IF tCopyBaseFiles:CHECKED IN FRAME fMain THEN                            
                RUN ipWriteMessage (INPUT "Building: " + STRING(cCleanCopyTo + replace(cCleanFullName,cCleanBaseSrc,""),"x(55)")).
            ELSE
                RUN ipWriteMessage (INPUT "Creating: " + STRING(cCleanCopyTo + replace(cCleanFullName,cCleanBaseSrc,""),"x(55)")).
            RUN ipCreateBase2 (INPUT cFullName, INPUT cCopyTo).
        END.
        ELSE IF tCopyBaseFiles:CHECKED IN FRAME fMain AND INDEX(cAttrList,"F") <> 0 THEN DO:
            /* Write a pretty display message and copy the file */
            ASSIGN cMyVar = STRING(cCleanCopyTo) + REPLACE(REPLACE(cCleanFullName,cCleanBaseSrc,""),cName,"").
            DISPLAY cCleanFullName  WITH FRAME copydisp.
            OS-COPY VALUE(cFullName) VALUE(cCleanCopyTo + REPLACE(REPLACE(cCleanFullName,cCleanBaseSrc,""),cName,"")). 
        END.
        /* One of these lastkeys allows trapping during Process Events; don't know which one */
        IF LASTKEY = KEYCODE("ESC") THEN RETURN.
        PROCESS EVENTS.
        IF LASTKEY = KEYCODE("ESC") THEN RETURN.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateCustom wWin 
PROCEDURE ipCreateCustom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipWriteMessage (INPUT "Initiating ipCreateCustom in fsInstaller.w").
    
    /* Check for missing data */
    IF cCustCodeLocn = "" 
    OR cCustCodeLocn = "-" THEN DO:
        MESSAGE "No custom source location given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No custom source location given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cCopyCustTo = "" 
    OR cCopyCustTo = "x-" THEN DO:
        MESSAGE "No custom target location given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No custom target location given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    
    /* Clean up the filename(s) so we know what to expect later */
    ASSIGN
        cCustCodeLocn = RIGHT-TRIM(cCustCodeLocn,"/\") + "\"
        cCustCodeLocn = REPLACE(cCustCodeLocn,"/","\")
        cCopyCustTo = RIGHT-TRIM(cCopyCustTo,"/\") + "\"
        cCopyCustTo = REPLACE(cCopyCustTo,"/","\").
        
    /* Build the target directory if it doesn't already exist */
    DEF VAR vTest AS CHAR NO-UNDO.
    DO iCtr = 1 TO NUM-ENTRIES(cCopyCustTo,"\"):
        ASSIGN vTest = vTest + ENTRY(iCtr,cCopyCustTo,"\") + "\".
        OS-CREATE-DIR VALUE(vTest).
    END.
    OS-CREATE-DIR VALUE(cCopyCustTo).
    
    RUN ipCreateCustom2 (INPUT cCustCodeLocn, INPUT cCopyCustTo).
        
    RUN ipWriteMessage (INPUT "Completing ipCreateCustom in fsInstaller.w").

    IF lStopOnError AND lFoundError THEN RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateCustom2 wWin 
PROCEDURE ipCreateCustom2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER cCopyFrom   AS CHAR NO-UNDO.
    DEF INPUT PARAMETER cCopyTo     AS CHAR FORMAT "x(60)".

    /* Clean up the filename(s) so we know what to expect later */
    ASSIGN 
        cCleanCopyTo = REPLACE(cCopyTo,"/","\")
        cCleanCustSrc = REPLACE(cCustCodeLocn,"/","\").
    
    /* This will show execution status to user without requiring interaction */
    FORM
        "Press ESC to cancel..." AT 2
        SKIP(1)
        "Copying: " AT 2 cCleanFullName FORMAT "x(60)"   AT 12 NO-LABEL
        "To: "      AT 2 
        SKIP(1)
        "(c) 2005, Foresight Software, Inc." AT 2
        "All rights reserved." TO 68
        WITH FRAME copydisp TITLE "fsInstaller - Copying Custom Files"
        THREE-D SIDE-LABELS VIEW-AS DIALOG-BOX WIDTH 75.
                                               
    /* Start reading from source dir */
    INPUT FROM OS-DIR (cCopyFrom).
    REPEAT:
        IMPORT cName cFullName cAttrList.
        ASSIGN cCleanFullName = REPLACE(cFullName,"/","\").
        IF INDEX(cAttrList,"D") <> 0 AND cName <> "." AND cName <> ".." THEN DO:
            /* If necessary, create a target subdir */
            OS-CREATE-DIR VALUE(cCleanCopyTo + replace(cCleanFullName,cCleanCustSrc,"")). 
            /* Note type of build in log, as user specified */
            IF tCopyCustFiles:CHECKED IN FRAME fMain THEN                            
                RUN ipWriteMessage (INPUT "Building: " + STRING(cCleanCopyTo + replace(cCleanFullName,cCleanCustSrc,""),"x(55)")).
            ELSE
                RUN ipWriteMessage (INPUT "Creating: " + STRING(cCleanCopyTo + replace(cCleanFullName,cCleanCustSrc,""),"x(55)")).
            RUN ipCreateCustom2 (INPUT cFullName, INPUT cCopyTo).
        END.
        ELSE IF tCopyCustFiles:CHECKED IN FRAME fMain AND INDEX(cAttrList,"F") <> 0 THEN DO:
            /* Write a pretty display message and copy the file */
            ASSIGN cMyVar = STRING(cCleanCopyTo) + REPLACE(REPLACE(cCleanFullName,cCleanCustSrc,""),cName,"").
            DISPLAY cCleanFullName  WITH FRAME copydisp.
            OS-COPY VALUE(cFullName) VALUE(cCleanCopyTo + REPLACE(REPLACE(cCleanFullName,cCleanCustSrc,""),cName,"")). 
        END.
        /* One of these lastkeys allows trapping during Process Events; don't know which one */
        IF LASTKEY = KEYCODE("ESC") THEN RETURN.
        PROCESS EVENTS.
        IF LASTKEY = KEYCODE("ESC") THEN RETURN.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateDB wWin 
PROCEDURE ipCreateDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipWriteMessage (INPUT "Initiating ipCreateDB in fsInstaller.w").

    DEF VAR iCtr1                   AS INT  NO-UNDO.
    DEF VAR iCtr2                   AS INT  NO-UNDO.
    DEF VAR cTxt1                   AS CHAR NO-UNDO.
    DEF VAR cTxt2                   AS CHAR NO-UNDO.
    DEF VAR lLog                    AS LOG  NO-UNDO.
    DEF VAR vTest                   AS CHAR NO-UNDO.

    /* Check for missing data */
    IF cDFLocn = "" THEN DO:
        MESSAGE "No source .DF file name given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No source .DF file name given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cDBLocn = "" 
    OR cDBLocn = "\" THEN DO:
        MESSAGE "No target database location given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No target database location given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cDBName1 = "" THEN DO:
        MESSAGE "No target database name given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No target database name given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    
    /* Clean up the filename(s) so we know what to expect later */
    ASSIGN
        cDFLocn = REPLACE(cDFLocn,"/","\")
        cDBLocn = RIGHT-TRIM(cDBLocn,"/\") + "\"
        cDBLocn = REPLACE(cDBLocn,"/","\")
        cDBFullName = RIGHT-TRIM(cDBLocn,"/\") + "\" + REPLACE(cDBName1,".db","") + ".db".
    
    /* Make sure there is a directory; if not create one */
    OS-CREATE-DIR VALUE(cDBLocn).

    /* If user wanted copy of .df, build a target and copy it in */
    IF tCopyDFFiles:CHECKED IN FRAME fMain AND SEARCH(cDFLocn) <> ? THEN DO:
        OS-CREATE-DIR VALUE(cDBLocn + "defs/").
        OS-COPY VALUE(cDFLocn) VALUE(cDBLocn + "defs/").
    END.
    
    /* This is easy; just create it, test for it, and write a status message */
    OS-COMMAND NO-CONSOLE VALUE('"' + SEARCH('prodb.exe') + '"' + ' ' + cDBFullName + ' ' + REPLACE(SEARCH('empty1.db'),"Program Files","Progra~~1")).
    IF SEARCH(cDBFullName) <> ? THEN 
        RUN ipWriteMessage (INPUT "Database " + cDBFullName + " created successfully.").
    ELSE DO:
        RUN ipWriteMessage (INPUT "ERROR: Database " + cDBFullName + " creation failed.").
        ASSIGN lFoundError = TRUE.
    END.
        
    IF lStopOnError AND lFoundError THEN RETURN.
    
    /* Now connect to target DB (note single user for speed); write status msg */
    IF NOT lDBConnected THEN DO:
        RUN ipConnectDBs.
        ASSIGN lDBConnected = TRUE.
    END.
    
    /* This will show execution status to user without requiring interaction */
    DEF VAR vLoadFileName AS CHAR NO-UNDO.
    FORM
        "Loading: " AT 2 vLoadFileName FORMAT "x(60)"   AT 12 NO-LABEL
        SKIP(1)
        "(c) 2005, Foresight Software, Inc." AT 2
        "All rights reserved." TO 68
        WITH FRAME loaddf TITLE "fsInstaller - Loading Definitions File"
        THREE-D SIDE-LABELS VIEW-AS DIALOG-BOX WIDTH 75.
    
    /* Load the .df */
    ASSIGN vLoadFileName = SUBSTRING(cDFLocn,R-INDEX(cDFLocn,"\") + 1).
    IF SEARCH("prodict/load_df.r") <> ? THEN DO: 
        DISPLAY vLoadFileName WITH FRAME loaddf.
        /* Stupidly, the load_df program writes a message; this prevents the ugly grey screen */
        OUTPUT TO nul.
        RUN prodict/load_df.r (INPUT cDFLocn + ",yes,NEW OBJECTS").
        OUTPUT CLOSE.
    END.
    ELSE DO:
        RUN ipWriteMessage (INPUT "ERROR: load_df program was not found").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    
    /* Scan the .df file to create a list of dump file names we'll need to 
    load data */
    ASSIGN cTableList = "_user,".
    INPUT STREAM inStream FROM VALUE(cDFLocn) NO-ECHO.
    REPEAT:
        IMPORT STREAM inStream UNFORMATTED vTest.
        IF INDEX(vTest,"ADD TABLE") = 1 THEN ASSIGN
            cTableList = cTableList + 
                TRIM(SUBSTRING(vTest,12),'"') + ",".
    END.
    INPUT STREAM inStream CLOSE.
    ASSIGN cTableList = TRIM(cTableList,",").

 
    /* If errors were generated (in .e file), append the .e text to the log, then delete the .e */
    IF SEARCH(REPLACE(cDBFullName,".db",".e")) <> ? THEN DO:
        RUN ipWriteMessage (INPUT "ERROR: Errors found during database update: (shown below)").
        OS-APPEND VALUE(SEARCH(REPLACE(cDBName1,".db",".e"))) VALUE(SEARCH(cLogLocn + "fsInstall.log")).
        OS-DELETE VALUE(SEARCH(REPLACE(cDBName1,".db",".e"))).
        ASSIGN lFoundError = TRUE.
    END.
    
    /* Note the status of the load in the log */
    IF NOT lFoundError THEN
        RUN ipWriteMessage (INPUT "Database " + LDBNAME(1) + " load from " + cDFLocn + " completed successfully.").
    ELSE
        RUN ipWriteMessage (INPUT "Database " + LDBNAME(1) + " load from " + cDFLocn + " completed with errors.").

    RUN ipWriteMessage (INPUT "Completing ipCreateDB in fsInstaller.w").
    
    IF lStopOnError AND lFoundError THEN RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateObject wWin 
PROCEDURE ipCreateObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipWriteMessage (INPUT "Initiating ipCreateObject in fsInstaller.w").
    
    /* Check for missing data */
    IF cBaseCodeLocn = "" 
    OR cBaseCodeLocn = "x-" THEN DO:
        MESSAGE "No base source location given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No base source location given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cObjCodeLocn = "" 
    OR cObjCodeLocn = "x-" THEN DO:
        MESSAGE "No object target location given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No object target location given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cCustObjLocn <> "" AND cCustCodeLocn = "" THEN DO:
        MESSAGE "No custom source location given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No custom source location given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cCustObjLocn = "" AND cCustCodeLocn <> "" THEN DO:
        MESSAGE "No custom target location given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No custom target location given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    
    /* Clean up the filename(s) so we know what to expect later */
    ASSIGN
        cBaseCodeLocn = RIGHT-TRIM(cBaseCodeLocn,"/\") + "\"
        cBaseCodeLocn = REPLACE(cBaseCodeLocn,"/","\")
        cCustCodeLocn = RIGHT-TRIM(cCustCodeLocn,"/\") + "\"
        cCustCodeLocn = REPLACE(cCustCodeLocn,"/","\")
        cObjCodeLocn = RIGHT-TRIM(cObjCodeLocn,"/\") + "\"
        cObjCodeLocn = REPLACE(cObjCodeLocn,"/","")
        cCustObjLocn = RIGHT-TRIM(cCustObjLocn,"/\") + "\"
        cCustObjLocn = REPLACE(cCustObjLocn,"/","\").
        
    /* Build the target directory if it doesn't already exist */
    DEF VAR vTest AS CHAR NO-UNDO.
    DO iCtr = 1 TO NUM-ENTRIES(cObjCodeLocn,"/"):
        ASSIGN vTest = vTest + ENTRY(iCtr,cObjCodeLocn,"\") + "\".
        OS-CREATE-DIR VALUE(vTest).
    END.
    OS-CREATE-DIR VALUE(cObjCodeLocn).

    RUN ipCreateObject2 (INPUT cBaseCodeLocn, INPUT cObjCodeLocn).
        IF lStopOnError AND lFoundError THEN RETURN.

    IF cCustObjLocn <> "" AND cCustCodeLocn <> "" 
    AND cCustObjLocn <> "\" AND cCustCodeLocn <> "\" THEN DO:
        ASSIGN vTest = "".
        DO iCtr = 1 TO NUM-ENTRIES(cCustObjLocn,"\"):
            ASSIGN vTest = vTest + ENTRY(iCtr,cCustObjLocn,"\") + "\".
            OS-CREATE-DIR VALUE(vTest).
        END.
        OS-CREATE-DIR VALUE(cCustObjLocn).
    
        RUN ipCreateObject2 (INPUT cCustCodeLocn, INPUT cCustObjLocn).
            IF lStopOnError AND lFoundError THEN RETURN.
        
    END.
    
    RUN ipWriteMessage (INPUT "Completing ipCreateObject in fsInstaller.w").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateObject2 wWin 
PROCEDURE ipCreateObject2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAMETER cCopyFrom   AS CHAR NO-UNDO.
    DEF INPUT PARAMETER cCopyTo     AS CHAR FORMAT "x(60)".

    /* Clean up the filename(s) so we know what to expect later */
    ASSIGN 
        cCleanCopyTo = REPLACE(cCopyTo,"/","\")
        cCleanBaseSrc = REPLACE(cBaseCodeLocn,"/","\").
    
    /* This will show execution status to user without requiring interaction */
    FORM
        "Press ESC to cancel..." AT 2
        SKIP(1)
        "Copying: " AT 2 cCleanFullName FORMAT "x(60)"   AT 12 NO-LABEL
        "To: "      AT 2 
        SKIP(1)
        "(c) 2005, Foresight Software, Inc." AT 2
        "All rights reserved." TO 68
        WITH FRAME copydisp TITLE "fsInstaller - Copying Base Files"
        THREE-D SIDE-LABELS VIEW-AS DIALOG-BOX WIDTH 75.
                                               
    /* Start reading from source dir */
    INPUT FROM OS-DIR (cCopyFrom).
    REPEAT:
        IMPORT cName cFullName cAttrList.
        ASSIGN cCleanFullName = REPLACE(cFullName,"/","\").
        IF INDEX(cAttrList,"D") <> 0 AND cName <> "." AND cName <> ".." THEN DO:
            /* If necessary, create a target subdir */
            OS-CREATE-DIR VALUE(cCleanCopyTo + replace(cCleanFullName,cCleanBaseSrc,"")). 
            /* Note type of build in log, as user specified */
            RUN ipWriteMessage (INPUT "Creating: " + STRING(cCleanCopyTo + replace(cCleanFullName,cCleanBaseSrc,""),"x(55)")).
            RUN ipCreateObject2 (INPUT cFullName, INPUT cCopyTo).
        END.
        /* One of these lastkeys allows trapping during Process Events; don't know which one */
        IF LASTKEY = KEYCODE("ESC") THEN RETURN.
        PROCESS EVENTS.
        IF LASTKEY = KEYCODE("ESC") THEN RETURN.
    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateStartup wWin 
PROCEDURE ipCreateStartup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipWriteMessage (INPUT "Initiating ipCreateStartup in fsInstaller.w").
    
    /* Check for missing data */
    IF cEnvTempLocn = "" THEN DO:
        MESSAGE "No base template location given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No base template location given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cEnvTargetLocn = "" THEN DO:
        MESSAGE "No environment target location given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No environment target location given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    
    /* Create the target directory structure */
    DEF VAR vTest AS CHAR NO-UNDO.
    DO iCtr = 1 TO NUM-ENTRIES(cEnvTargetLocn,"\"):
        ASSIGN vTest = vTest + ENTRY(iCtr,cEnvTargetLocn,"\") + "\".
        OS-CREATE-DIR VALUE(vTest).
    END.
    OS-CREATE-DIR VALUE(cEnvTargetLocn).

    /* Copy the files into the target directory */
    ASSIGN vTest = SEARCH(cEnvTempLocn + "foresight.fon").
    OS-COPY VALUE(vTest) VALUE(cEnvTargetLocn + "foresight.fon"). 
    RUN ipWriteMessage (INPUT "Writing environment file " + cEnvTargetLocn + "foresight.fon").

    ASSIGN vTest = SEARCH(cEnvTempLocn + "foresight.ico").
    OS-COPY VALUE(vTest) VALUE(cEnvTargetLocn + "foresight.ico"). 
    RUN ipWriteMessage (INPUT "Writing environment file " + cEnvTargetLocn + "foresight.ico").

    ASSIGN vTest = SEARCH(cEnvTempLocn + "proextra.dll").
    OS-COPY VALUE(vTest) VALUE(cEnvTargetLocn + "proextra.dll"). 
    RUN ipWriteMessage (INPUT "Writing environment file " + cEnvTargetLocn + "proextra.dll").

    ASSIGN vTest = SEARCH(cEnvTempLocn + "client.pf").
    OS-COPY VALUE(vTest) VALUE(cEnvTargetLocn + "client.pf"). 
    RUN ipWriteMessage (INPUT "Writing environment file " + cEnvTargetLocn + "client.pf").

    ASSIGN vTest = SEARCH(cEnvTempLocn + "progress.ini.tpl").
    OS-COPY VALUE(vTest) VALUE(cEnvTargetLocn + "progress.ini.tpl"). 
    RUN ipWriteMessage (INPUT "Writing environment file " + cEnvTargetLocn + "progress.ini").

    ASSIGN vTest = SEARCH(cEnvTempLocn + "mxp.ini.tpl").
    OS-COPY VALUE(vTest) VALUE(cEnvTargetLocn + "mxp.ini.tpl"). 
    RUN ipWriteMessage (INPUT "Writing environment file " + cEnvTargetLocn + "mxp.ini").

    ASSIGN vTest = SEARCH(cEnvTempLocn + "mxp.pc.tpl").
    OS-COPY VALUE(vTest) VALUE(cEnvTargetLocn + "mxp.pc.tpl"). 
    RUN ipWriteMessage (INPUT "Writing environment file " + cEnvTargetLocn + "mxp.pc").

    ASSIGN vTest = SEARCH(cEnvTempLocn + "makeshortcut.vbs.tpl").
    OS-COPY VALUE(vTest) VALUE(cEnvTargetLocn + "makeshortcut.vbs.tpl"). 
    RUN ipWriteMessage (INPUT "Writing environment file " + cEnvTargetLocn + "makeshortcut.vbs").

    DEF VAR vIniPropath AS CHAR NO-UNDO.
    
    ASSIGN vIniPropath = ".," + cObjCodeLocn + "," + cBaseTgtLocn.

    /* Edit progress.ini */
    INPUT STREAM inStream FROM VALUE(cEnvTargetLocn + "progress.ini.tpl") NO-ECHO.
    OUTPUT STREAM outStream TO VALUE (cEnvTargetLocn + "progress.ini").
    REPEAT:
        IMPORT STREAM inStream UNFORMATTED vTest.
        ASSIGN 
            vTest = trim(trim(REPLACE(vTest,"<<dlc>>",cProgressLocn),"\"),"/")
            vTest = REPLACE(vTest,"<<propath>>",vIniPropath)
            vTest = REPLACE(vTest,"<<dbname>>",cDBName1)
            vTest = REPLACE(vTest,"<<dbdir>>",cDBLocn)
            vTest = REPLACE(vTest,"<<dbparms>>",cDBParms1)
            vTest = REPLACE(vTest,"<<envlocn>>",cEnvTargetLocn).
        PUT STREAM outStream UNFORMATTED vTest SKIP.
    END.
    INPUT STREAM inStream CLOSE.
    OUTPUT STREAM outStream CLOSE.
    /* OS-DELETE VALUE(cEnvTargetLocn + "progress.ini.tpl"). */

    /* Edit mxp.ini */
    INPUT STREAM inStream FROM VALUE(cEnvTargetLocn + "mxp.ini.tpl") NO-ECHO.
    OUTPUT STREAM outStream TO VALUE (cEnvTargetLocn + "mxp.ini").
    REPEAT:
        IMPORT STREAM inStream UNFORMATTED vTest.
        ASSIGN 
            vTest = trim(REPLACE(vTest,"<<dlc>>",cProgressLocn),"/")
            vTest = REPLACE(vTest,"<<propath>>",vINIPropath)
            vTest = REPLACE(vTest,"<<dbname>>",cDBName1)
            vTest = REPLACE(vTest,"<<dbdir>>",cDBLocn)
            vTest = REPLACE(vTest,"<<dbparms>>",cDBParms1)
            vTest = REPLACE(vTest,"<<envlocn>>",cEnvTargetLocn).
        PUT STREAM outStream UNFORMATTED vTest SKIP.
    END.
    INPUT STREAM inStream CLOSE.
    OUTPUT STREAM outStream CLOSE.
    /* OS-DELETE VALUE(cEnvTargetLocn + "mxp.ini.tpl"). */

    /* Edit mxp.pc */
    INPUT STREAM inStream FROM VALUE(cEnvTargetLocn + "mxp.pc.tpl") NO-ECHO.
    OUTPUT STREAM outStream TO VALUE (cEnvTargetLocn + "mxp.pc").
    REPEAT:
        IMPORT STREAM inStream UNFORMATTED vTest.
        ASSIGN 
            vTest = trim(REPLACE(vTest,"<<dlc>>",cProgressLocn),"/")
            vTest = REPLACE(vTest,"<<propath>>",PROPATH)
            vTest = REPLACE(vTest,"<<dbname>>",cDBName1)
            vTest = REPLACE(vTest,"<<dbdir>>",cDBLocn)
            vTest = REPLACE(vTest,"<<dbparms>>",cDBParms1)
            vTest = REPLACE(vTest,"<<envlocn>>",cEnvTargetLocn).
        PUT STREAM outStream UNFORMATTED vTest SKIP.
    END.
    INPUT STREAM inStream CLOSE.
    OUTPUT STREAM outStream CLOSE.
    OS-DELETE VALUE(cEnvTargetLocn + "mxp.pc.tpl").
    /* OS-COPY VALUE(cEnvTargetLocn + "mxp.pc") VALUE(cDBLocn + "mxp.pc"). */

    /* Edit makeshortcut.vbs */
    INPUT STREAM inStream FROM VALUE(cEnvTargetLocn + "makeshortcut.vbs.tpl") NO-ECHO.
    OUTPUT STREAM outStream TO VALUE (cEnvTargetLocn + "makeshortcut.vbs").
    REPEAT:
        IMPORT STREAM inStream UNFORMATTED vTest.
        ASSIGN 
            vTest = trim(REPLACE(vTest,"<<dlc>>",cProgressLocn),"/")
            vTest = REPLACE(vTest,"<<propath>>",PROPATH)
            vTest = REPLACE(vTest,"<<dbname>>",cDBName1)
            vTest = REPLACE(vTest,"<<dbdir>>",cDBLocn)
            vTest = REPLACE(vTest,"<<dbparms>>",cDBParms1)
            vTest = REPLACE(vTest,"<<envlocn>>",cEnvTargetLocn).
        PUT STREAM outStream UNFORMATTED vTest SKIP.
    END.
    INPUT STREAM inStream CLOSE.
    OUTPUT STREAM outStream CLOSE.
    /* OS-DELETE VALUE(cEnvTargetLocn + "makeshortcut.vbs.tpl"). */

    /* Run script to create shortcut, then delete it */
    OS-COMMAND SILENT VALUE(SEARCH(cEnvTargetLocn + "makeshortcut.vbs")).
    OS-DELETE VALUE(cEnvTargetLocn + "makeshortcut.vbs").

    RUN ipWriteMessage (INPUT "Completing ipCreateStartup in fsInstaller.w").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDisconnectDBs wWin 
PROCEDURE ipDisconnectDBs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO  WHILE NUM-DBS > 0:
    DISCONNECT value(ldbname(1)).
END.
DO  WHILE NUM-ALIASES > 0:
    DELETE ALIAS value(ALIAS(1)).
END.

ASSIGN
    lDBConnected = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFinalizeLog wWin 
PROCEDURE ipFinalizeLog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN iEndTime = TIME.

    OUTPUT STREAM logStream TO VALUE(cLogDir + REPLACE(cThisConfig,".cfg",".log")) APPEND.

    PUT STREAM logStream UNFORMATTED 
        "-------------------------------------------------------------------------------------------------------------" SKIP
        "Ending run of fsInstaller system on " TODAY " at " STRING(iEndTime,"HH:MM:SS") 
                                                      FILL(" ",36) "Elapsed: " STRING(iEndTime - iStartTime,"HH:MM:SS") SKIP
        "-------------------------------------------------------------------------------------------------------------" SKIP
        " " SKIP
        " " SKIP
        " " SKIP.
        
    OUTPUT STREAM logStream CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipInitializeLog wWin 
PROCEDURE ipInitializeLog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Build the target directory if it doesn't already exist */
    DEF VAR vTest1 AS CHAR NO-UNDO.
    DEF VAR vTest2 AS CHAR NO-UNDO.
  
    ASSIGN 
        vTest1 = REPLACE(cLogDir,"/","\")
        vTest2 = "".
    DO iCtr = 1 TO NUM-ENTRIES(vTest1,"\"):
        ASSIGN vTest2 = vTest2 + ENTRY(iCtr,vTest1,"\") + "\".
        OS-CREATE-DIR VALUE(vTest1).
    END.
    OS-CREATE-DIR VALUE(vTest2).

    /* if user wants to clear old log entries, do that first */
    IF lClearLogFirst THEN DO:
        OUTPUT STREAM logStream TO VALUE(cLogDir + REPLACE(cThisConfig,".cfg",".log")).
        OUTPUT STREAM logStream CLOSE.
        RUN ipWriteMessage (INPUT "Previous log entries cleared.").
    END.
    
    /* set the start time */
    ASSIGN iStartTime = TIME.

    OUTPUT STREAM logStream TO VALUE(cLogDir + REPLACE(cThisConfig,".cfg",".log")) APPEND.
    
    PUT STREAM logStream UNFORMATTED
        "-------------------------------------------------------------------------------------------------------------" SKIP
        "Beginning run of fsInstaller system on " TODAY " at " STRING(TIME,"HH:MM:SS") FILL(" ",42) "--TIME--"            SKIP
        "-------------------------------------------------------------------------------------------------------------" SKIP.
    
    OUTPUT STREAM logStream CLOSE.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadData wWin 
PROCEDURE ipLoadData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipWriteMessage (INPUT "Initiating ipLoadData in fsInstaller.w").
    
    /* Check for missing data */
    IF cDataFilesLocn = "" THEN DO:
        RUN ipWriteMessage (INPUT "ERROR: No data directory name given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cDBLocn = "" 
    OR cDBLocn = "\" THEN DO:
        MESSAGE "No target database location given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No target database location given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cDBName1 = "" THEN DO:
        MESSAGE "No target database name given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No target database name given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.

    /* Clean up the filename(s) so we know what to expect later */
    ASSIGN
        cDataFilesLocn = RIGHT-TRIM(cDataFilesLocn,"/\") + "\"
        cDataFilesLocn = REPLACE(cDataFilesLocn,"/","\")
        cDFLocn = REPLACE(cDFLocn,"/","\")
        cDBLocn = RIGHT-TRIM(cDBLocn,"/\") + "\"
        cDBLocn = REPLACE(cDBLocn,"/","\")
        cDBFullName = RIGHT-TRIM(cDBLocn,"/\") + "\" + REPLACE(cDBName1,".db","") + ".db".
    
    /* Now connect to target DB (note single user for speed); write status msg */
    IF NOT lDBConnected THEN DO:
        RUN ipConnectDBs.
        ASSIGN lDBConnected = TRUE.
    END.
    
    /* Note: we built the list of files to import when building the DB */
    
    /* This will show execution status to user without requiring interaction */
    FORM
        "Loading: " AT 2 cDataFilesLocn FORMAT "x(60)"   AT 12 NO-LABEL
        SKIP(1)
        "(c) 2005, Foresight Software, Inc." AT 2
        "All rights reserved." TO 68
        WITH FRAME loadd TITLE "fsINstaller - Loading Data Files"
        THREE-D SIDE-LABELS VIEW-AS DIALOG-BOX WIDTH 75.
    
    /* Run the import routine */
    IF SEARCH("prodict/load_d.r") <> ? THEN DO: 
        DISPLAY cDataFilesLocn WITH FRAME loadd.
        OUTPUT TO nul.
        RUN prodict/load_d.r (INPUT cTableList, INPUT cDataFilesLocn).
        OUTPUT CLOSE.
    END.
    ELSE DO:
        RUN ipWriteMessage (INPUT "ERROR: load_d program was not found").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    
    /* If errors were generated (in .e files), append the .e text to the log, then delete the .e */
    INPUT FROM OS-DIR(cDataFilesLocn).
    REPEAT:
        IMPORT cName cFullName cAttrList.
        IF INDEX(cAttrList,"F") <> 0 AND INDEX(cName,".e") <> 0 THEN DO:
            RUN ipWriteMessage (INPUT "ERROR: Errors found during database update: (shown below)").
            OS-APPEND VALUE(cFullName) VALUE(cLogDir + REPLACE(cThisConfig,".cfg",".log")).
            OS-DELETE VALUE(cFullName).
            ASSIGN lFoundError = TRUE.
        END.
    END.
    
    /* Note the status of the load in the log */
    IF NOT lFoundError THEN DO:
        RUN ipWriteMessage (INPUT "Database " + cDBFullName + " update from ").
        RUN ipWriteMessage (INPUT "          " + cDataFilesLocn + " completed successfully.").
    END.
    ELSE DO:
        RUN ipWriteMessage (INPUT "Database " + cDBFullName + " update from ").
        RUN ipWriteMessage (INPUT "          " + cDataFilesLocn + " completed with ERRORS.").
    END.

    RUN ipWriteMessage (INPUT "Completing ipLoadData in fsInstaller.w").
    
    IF lStopOnError AND lFoundError THEN RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipModifyPropath wWin 
PROCEDURE ipModifyPropath :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT lPPModified THEN DO WITH FRAME fMain:
    ASSIGN 
        PROPATH = REPLACE(PROPATH,"/","\").
    
    /* build these in reverse order */        
    /*       
           IF fDBLocn:SCREEN-VALUE <> "" AND
       LOOKUP(fDBLocn:SCREEN-VALUE,PROPATH) = 0 THEN ASSIGN
    PROPATH = fDBLocn:SCREEN-VALUE + "," + PROPATH.
       
           IF fObjCodeLocn:SCREEN-VALUE <> "" AND
       LOOKUP(fObjCodeLocn:SCREEN-VALUE,PROPATH) = 0 THEN ASSIGN
    PROPATH = fObjCodeLocn:SCREEN-VALUE + "," + PROPATH.
       
           IF fCustObjLocn:SCREEN-VALUE <> "" AND
       LOOKUP(fCustObjLocn:SCREEN-VALUE,PROPATH) = 0 THEN ASSIGN
    PROPATH = fCustObjLocn:SCREEN-VALUE + "," + PROPATH.
    */   
           IF fCopyBaseTo:SCREEN-VALUE <> "" AND
       LOOKUP(fCopyBaseTo:SCREEN-VALUE,PROPATH) = 0 THEN ASSIGN
    PROPATH = fCopyBaseTo:SCREEN-VALUE + "," + PROPATH.
       
           IF fBaseCodeLocn:SCREEN-VALUE <> "" AND
       LOOKUP(fBaseCodeLocn:SCREEN-VALUE,PROPATH) = 0 THEN ASSIGN
    PROPATH = fBaseCodeLocn:SCREEN-VALUE + "," + PROPATH.
       
           IF fPatchCodeLocn:SCREEN-VALUE <> "" AND
       LOOKUP(fPatchCodeLocn:SCREEN-VALUE,PROPATH) = 0 THEN ASSIGN
    PROPATH = fPatchCodeLocn:SCREEN-VALUE + "," + PROPATH.
       
           IF fCopyCustTo:SCREEN-VALUE <> "" AND
       LOOKUP(fCopyCustTo:SCREEN-VALUE,PROPATH) = 0 THEN ASSIGN
    PROPATH = fCopyCustTo:SCREEN-VALUE + "," + PROPATH.
       
           IF fCustCodeLocn:SCREEN-VALUE <> "" AND
       LOOKUP(fCustCodeLocn:SCREEN-VALUE,PROPATH) = 0 THEN ASSIGN
    PROPATH = fCustCodeLocn:SCREEN-VALUE + "," + PROPATH.
       
           IF fLogDir:SCREEN-VALUE <> "" AND
       LOOKUP(fLogDir:SCREEN-VALUE,PROPATH) = 0 THEN ASSIGN
    PROPATH = fLogDir:SCREEN-VALUE + "," + PROPATH.
       
    PROPATH = ".," + PROPATH.
    PROPATH = REPLACE(PROPATH,"/","\").
    
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRunFix wWin 
PROCEDURE ipRunFix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipWriteMessage (INPUT "Initiating ipRunFix in fsInstaller.w").
    
    ASSIGN
        lFoundError = FALSE
        cDFLocn = REPLACE(cDFLocn,"/","\")
        cDBLocn = RIGHT-TRIM(cDBLocn,"/\") + "\"
        cDBLocn = REPLACE(cDBLocn,"/","\")
        cDBFullName = RIGHT-TRIM(cDBLocn,"/\") + "\" + REPLACE(cDBName1,".db","") + ".db".

    /* This will show execution status to user without requiring interaction */
    FORM
        "Press ESC to cancel..." AT 2
        SKIP(1)
        "Running: " AT 2 cCleanFullName FORMAT "x(60)"   AT 12 NO-LABEL
        SKIP(1)
        "(c) 2005, Foresight Software, Inc." AT 2
        "All rights reserved." TO 68
        WITH FRAME rundisp TITLE "fsInstaller - Running fix programs"
        THREE-D SIDE-LABELS VIEW-AS DIALOG-BOX WIDTH 75.

    /* Now run everything in the selected list */
    DO iCtr = 1 TO NUM-ENTRIES(cFixList):
        IF SEARCH(cFixDir + ENTRY(iCtr,cFixList)) <> ? THEN DO:
            ASSIGN cCleanFullName = cFixDir + ENTRY(iCtr,cFixList).
            DISPLAY cCleanFullName  WITH FRAME copydisp.
            RUN ipWriteMessage (INPUT "Running: " + STRING(cFixDir + ENTRY(iCtr,cFixList),"x(55)")).
            RUN VALUE(SEARCH(cFixDir + ENTRY(iCtr,cFixList))).
        END.
        /* One of these lastkeys allows trapping during Process Events; don't know which one */
        IF LASTKEY = KEYCODE("ESC") THEN RETURN.
        PROCESS EVENTS.
        IF LASTKEY = KEYCODE("ESC") THEN RETURN.
    END.
    
    /* Note the status of the load in the log */
    IF NOT lFoundError THEN
        RUN ipWriteMessage (INPUT "Fix programs completed successfully.").
    ELSE
        RUN ipWriteMessage (INPUT "Fix programs completed with errors.").
    
    RUN ipWriteMessage (INPUT "Completing ipRunFix in fsInstaller.w").
    
    IF lStopOnError AND lFoundError THEN RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRunFixMandatory wWin 
PROCEDURE ipRunFixMandatory :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipWriteMessage (INPUT "Initiating ipRunFixMandatory in fsInstaller.w").
    
    ASSIGN
        lFoundError = FALSE
        cDFLocn = REPLACE(cDFLocn,"/","\")
        cDBLocn = RIGHT-TRIM(cDBLocn,"/\") + "\"
        cDBLocn = REPLACE(cDBLocn,"/","\")
        cDBFullName = RIGHT-TRIM(cDBLocn,"/\") + "\" + REPLACE(cDBName1,".db","") + ".db".

    RUN VALUE(SEARCH("fsiFIX.p")).
        
    /* Note the status of the load in the log */
    IF NOT lFoundError THEN
        RUN ipWriteMessage (INPUT "Fix programs completed successfully.").
    ELSE
        RUN ipWriteMessage (INPUT "Fix programs completed with errors.").
    
    RUN ipWriteMessage (INPUT "Completing ipRunFixMandatory in fsInstaller.w").
    
    IF lStopOnError AND lFoundError THEN RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSaveConfig wWin 
PROCEDURE ipSaveConfig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUTTON bOK LABEL "OK" SIZE 12 BY 1.
    DEF BUTTON bCancel LABEL "Cancel" SIZE 12 BY 1.

    DISPLAY
        cThisConfig LABEL "Config File Name" VIEW-AS FILL-IN SIZE 21 BY 1
        WITH FRAME a TITLE "Save This File As..." 1 COLUMN SIDE-LABELS THREE-D OVERLAY WIDTH 42 VIEW-AS DIALOG-BOX.
    ENABLE 
        cThisConfig 
        bOK    AT ROW 2.8 COLUMN 16
        bCancel AT ROW 2.8 COLUMN 29
        WITH FRAME a.
    ON 'choose':U OF bOK IN FRAME a
    DO:
        ASSIGN cThisConfig = cThisConfig:SCREEN-VALUE IN FRAME a.
    END.
    ON 'choose':U OF bCancel IN FRAME a
    DO:
        RETURN.
    END.
    WAIT-FOR 'choose' OF bOK IN FRAME a
        OR 'choose' OF bCancel IN FRAME a.

ASSIGN
    cCreateDB           = STRING(tCreateDB:CHECKED IN FRAME fMain)
    cUpdateDB           = STRING(tUpdateDB:CHECKED IN FRAME fMain)
    cLoadData           = STRING(tLoadData:CHECKED IN FRAME fMain)
    cCreateBase         = STRING(tCreateBase:CHECKED IN FRAME fMain)
    cCreateCustom       = STRING(tCreateCustom:CHECKED IN FRAME fMain)
    cCreateObject       = STRING(tCreateObject:CHECKED IN FRAME fMain)
    cCompileBase        = STRING(tCompileBase:CHECKED IN FRAME fMain)
    cCompilePatch       = STRING(tCompilePatch:CHECKED IN FRAME fMain)
    cCreateCustObj      = STRING(tCreateCustObj:CHECKED IN FRAME fMain)
    cCompileCustom      = STRING(tCompileCustom:CHECKED IN FRAME fMain)
    cCompileSelected    = STRING(tCompileSelected:CHECKED IN FRAME fMain)
    cRunFix             = STRING(tRunFIX:CHECKED IN FRAME fMain)
    cCreateStartup      = STRING(tCreateStartup:CHECKED IN FRAME fMain)
    
    cCopyDFFiles        = STRING(tCopyDFFiles:CHECKED IN FRAME fMain)
    cCopyBaseFiles      = STRING(tCopyBaseFiles:CHECKED IN FRAME fMain)
    cCopyCustFiles      = STRING(tCopyCustFiles:CHECKED IN FRAME fMain)
    cConnectMulti       = STRING(tConnectMulti:CHECKED IN FRAME fMain)

    cClearLog           = STRING(tClearLog:CHECKED IN FRAME fMain)
    cStopOnError        = STRING(tStopOnError:CHECKED IN FRAME fMain)
    cLogErrors          = STRING(tLogErrors:CHECKED IN FRAME fMain)
    
    cLogDir             = fLogDir:SCREEN-VALUE IN FRAME fMain
    
    cDBLocn             = fDBLocn:SCREEN-VALUE IN FRAME fMain
    cDBName1            = fDBName:SCREEN-VALUE IN FRAME fMain
    cDBParms1           = fDBConnParms:SCREEN-VALUE IN FRAME fMain
    cDFLocn             = fDFLocn:SCREEN-VALUE IN FRAME fMain
    cDeltaLocn          = fDeltaLocn:SCREEN-VALUE IN FRAME fMain
    cDataFilesLocn      = fDataFilesLocn:SCREEN-VALUE IN FRAME fMain
    cBaseCodeLocn       = fBaseCodeLocn:SCREEN-VALUE IN FRAME fMain
    cPatchCodeLocn      = fPatchCodeLocn:SCREEN-VALUE IN FRAME fMain
    cCopyBaseTo         = fCopyBaseTo:SCREEN-VALUE IN FRAME fMain
    cCustCodeLocn       = fCustCodeLocn:SCREEN-VALUE IN FRAME fMain
    cCopyCustTo         = fCopyCustTo:SCREEN-VALUE IN FRAME fMain
    cObjCodeLocn        = fObjCodeLocn:SCREEN-VALUE IN FRAME fMain
    cCustObjLocn        = fCustObjLocn:SCREEN-VALUE IN FRAME fMain
    cEnvTempLocn        = fEnvTempLocn:SCREEN-VALUE IN FRAME fMain
    cEnvTargetLocn      = fEnvTargetLocn:SCREEN-VALUE IN FRAME fMain
    cProgressLocn       = fProgressLocn:SCREEN-VALUE IN FRAME fMain
    cPropath            = propath.
    
/* Make sure there is a directory; if not create one */
OS-CREATE-DIR VALUE(cLogDir).

OUTPUT TO VALUE(cLogDir + cThisConfig).
    PUT UNFORMATTED cCreateDB + CHR(10).
    PUT UNFORMATTED cUpdateDB + CHR(10).
    PUT UNFORMATTED cLoadData + CHR(10).        
    PUT UNFORMATTED cCreateBase + CHR(10).         
    PUT UNFORMATTED cCreateCustom + CHR(10).       
    PUT UNFORMATTED cCreateObject + CHR(10).       
    PUT UNFORMATTED cCreateCustObj + CHR(10).       
    PUT UNFORMATTED cCompileBase + CHR(10).        
    PUT UNFORMATTED cCompilePatch + CHR(10).        
    PUT UNFORMATTED cCompileCustom + CHR(10).      
    PUT UNFORMATTED cCompileSelected + CHR(10).    
    PUT UNFORMATTED cRunFix + CHR(10).             
    PUT UNFORMATTED cCreateStartup + CHR(10).      
    
    PUT UNFORMATTED cCopyDFFiles + CHR(10).        
    PUT UNFORMATTED cCopyBaseFiles + CHR(10).      
    PUT UNFORMATTED cCopyCustFiles + CHR(10).      
    PUT UNFORMATTED cConnectMulti + CHR(10).       

    PUT UNFORMATTED cClearLog + CHR(10).           
    PUT UNFORMATTED cStopOnError + CHR(10).        
    PUT UNFORMATTED cLogErrors + CHR(10).          

    PUT UNFORMATTED cLogDir + CHR(10).             
    
    PUT UNFORMATTED cDBLocn + CHR(10).             
    PUT UNFORMATTED cDBName1 + CHR(10).             
    PUT UNFORMATTED cDBParms1 + CHR(10).        
    PUT UNFORMATTED cDFLocn + CHR(10).             
    PUT UNFORMATTED cDeltaLocn + CHR(10).          
    PUT UNFORMATTED cDataFilesLocn + CHR(10).      
    PUT UNFORMATTED cBaseCodeLocn + CHR(10).       
    PUT UNFORMATTED cPatchCodeLocn + CHR(10).       
    PUT UNFORMATTED cCopyBaseTo + CHR(10).         
    PUT UNFORMATTED cCustCodeLocn + CHR(10).       
    PUT UNFORMATTED cCopyCustTo + CHR(10).         
    PUT UNFORMATTED cObjCodeLocn + CHR(10).        
    PUT UNFORMATTED cCustObjLocn + CHR(10).        
    PUT UNFORMATTED cEnvTempLocn + CHR(10).        
    PUT UNFORMATTED cEnvTargetLocn + CHR(10).      
    PUT UNFORMATTED cProgressLocn + CHR(10). 

    PUT UNFORMATTED "x-" + cFixList + CHR(10).
    PUT UNFORMATTED "x-" + cFixEntry + CHR(10).
    PUT UNFORMATTED "x-" + cFileName + CHR(10).
    PUT UNFORMATTED "x-" + cFixDir + CHR(10).
    PUT UNFORMATTED "x-" + cBaseList + CHR(10).
    PUT UNFORMATTED "x-" + cCustList + CHR(10).
    PUT UNFORMATTED "x-" + cParmList + CHR(10).
    PUT UNFORMATTED "x-" + cDBDir2 + CHR(10).
    PUT UNFORMATTED "x-" + cDBName2 + CHR(10).
    PUT UNFORMATTED "x-" + cDBParms2 + CHR(10).
    PUT UNFORMATTED "x-" + cDBDir3 + CHR(10).
    PUT UNFORMATTED "x-" + cDBName3 + CHR(10).
    PUT UNFORMATTED "x-" + cDBParms3 + CHR(10).
    PUT UNFORMATTED "x-" + cDBDir4 + CHR(10).
    PUT UNFORMATTED "x-" + cDBName4 + CHR(10).
    PUT UNFORMATTED "x-" + cDBParms4 + CHR(10).
    PUT UNFORMATTED "x-" + cDBDir5 + CHR(10).
    PUT UNFORMATTED "x-" + cDBName5 + CHR(10).
    PUT UNFORMATTED "x-" + cDBParms5 + CHR(10).
    PUT UNFORMATTED "x-" + cDBDir6 + CHR(10).
    PUT UNFORMATTED "x-" + cDBName6 + CHR(10).
    PUT UNFORMATTED "x-" + cDBParms6 + CHR(10).
    put unformatted cPropath + chr(10).

    PUT UNFORMATTED "." + CHR(10).

OUTPUT CLOSE.
ASSIGN tConfigName:SCREEN-VALUE IN FRAME fMain = "Using " + cThisConfig + "...".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSelectBase wWin 
PROCEDURE ipSelectBase :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER vSelType AS CHAR NO-UNDO.
    DEF INPUT PARAMETER vInput1 AS CHAR NO-UNDO.
    DEF INPUT PARAMETER vInput2 AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER vOutput AS CHAR NO-UNDO.
    
    DEF VAR cTxt1 AS CHAR NO-UNDO.
    DEF VAR cTxt2 AS CHAR NO-UNDO.
    DEF BUTTON bOK LABEL "OK" SIZE 12 BY 1.
    DEF BUTTON bCancel LABEL "Cancel" SIZE 12 BY 1.
    DEF VAR slist1 AS CHAR VIEW-AS SELECTION-LIST SCROLLBAR-VERTICAL INNER-LINES 10 INNER-CHARS 50.
    
    DISPLAY
        "Found more than one possible location for source code for compiling" AT ROW 1 COLUMN 2
        string(vSelType + ".  Choose the one you prefer to use:") FORMAT "X(50)" AT ROW 1.8 COLUMN 2 
        sList1 AT ROW 3 COLUMN 1
        bOK    AT ROW 3 COLUMN 57
        bCancel AT ROW 4.2 COLUMN 57
        SKIP(1)
        "(c) 2005, Foresight Software, Inc." AT ROW 9.5 COLUMN 2
        "All rights reserved." TO 68
        WITH FRAME a TITLE "fsInstaller - Select Compile FROM Directory"
        THREE-D NO-LABELS VIEW-AS DIALOG-BOX WIDTH 75.
    
    sList1:LIST-ITEMS IN FRAME a = vInput1 + "," + vInput2.
    ENABLE sList1 bOK bCancel WITH FRAME a.
    
    ON 'choose':U OF bOK IN FRAME a
    OR 'MOUSE-SELECT-DBLCLICK' OF sList1 IN FRAME a
    DO:
        ASSIGN vOutput = sList1:SCREEN-VALUE IN FRAME a.
    END.
    ON 'choose':U OF bCancel IN FRAME a
    DO:
        ASSIGN vOutput = "".
    END.

    WAIT-FOR 'choose' OF bOK IN FRAME a OR
             'mouse-select-dblclick' OF sList1 IN FRAME a OR
             'choose' OF bCancel IN FRAME a.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSelectCode wWin 
PROCEDURE ipSelectCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUTTON bOK LABEL "OK" SIZE 12 BY 1.
    DEF BUTTON bCancel LABEL "Cancel" SIZE 12 BY 1.
    DEF VAR tlist1 AS CHAR VIEW-AS EDITOR SCROLLBAR-VERTICAL INNER-LINES 10 INNER-CHARS 62.
    DEF VAR tlist2 AS CHAR VIEW-AS EDITOR SCROLLBAR-VERTICAL INNER-LINES 10 INNER-CHARS 62.

    ASSIGN
        tList1 = cBaseList
        tList2 = cCustList.

    DISPLAY
        "In this box, enter the names of subdirectories (like 'ar/'), or files" VIEW-AS TEXT AT ROW 1 COLUMN 1.5
        "(like 'ar/bslsmn.w') that you wish to compile.  Items in this box must" VIEW-AS TEXT AT ROW 1.7 COLUMN 1.5
        "exist under the BASE code directory you specified in the main window." VIEW-AS TEXT AT ROW 2.4 COLUMN 1.5
        tList1
        "In this box, enter the names of subdirectories (like 'ar/'), or files" VIEW-AS TEXT AT ROW 10 COLUMN 1.5
        "(like 'ar/bslsmn.w') that you wish to compile.  Items in this box must" VIEW-AS TEXT AT ROW 10.7 COLUMN 1.5
        "exist under the CUSTOM code directory in the main window." VIEW-AS TEXT AT ROW 11.4 COLUMN 1.5
        tList2
        "NOTE: SEPARATE ALL VALUES WITH COMMAS (ar/,ap/, etc.)" VIEW-AS TEXT AT ROW 19 COLUMN 1.5
        WITH FRAME a.
    ENABLE 
        tList1 AT ROW 3.2 COLUMN 1.5
        tList2 AT ROW 12.2 COLUMN 1.5
        bOK    AT ROW 19.8 COLUMN 44
        bCancel AT ROW 19.8 COLUMN 57
        WITH FRAME a NO-LABELS TITLE "Enter List of Items to Compile" THREE-D OVERLAY WIDTH 70 VIEW-AS DIALOG-BOX.
    ON 'choose':U OF bOK IN FRAME a
    DO:
        ASSIGN 
            cBaseList = trim(tList1:SCREEN-VALUE IN FRAME a,",")
            cCustList = trim(tList2:SCREEN-VALUE IN FRAME a,",").
        RETURN.
    END.
    ON 'return':U OF tlist1 IN FRAME a
    OR 'return':U OF tlist2 IN FRAME a
    DO:
        APPLY 'choose' TO bOK IN FRAME a.
    END.
    ON 'choose':U OF bCancel IN FRAME a
    DO:
        ASSIGN 
            cBaseList = ""
            cCustList = "".
        RETURN.
    END.
    WAIT-FOR 'choose' OF bOK IN FRAME a
        OR 'choose' OF bCancel IN FRAME a.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSelectFix wWin 
PROCEDURE ipSelectFix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cTxt1 AS CHAR NO-UNDO.
    DEF VAR cTxt2 AS CHAR NO-UNDO.
    DEF BUTTON bOK LABEL "OK" SIZE 12 BY 1.
    DEF BUTTON bCancel LABEL "Cancel" SIZE 12 BY 1.
    DEF VAR slist1 AS CHAR VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL INNER-LINES 10 INNER-CHARS 50.
    ASSIGN cFileName = "_tag.fix".
    
    SYSTEM-DIALOG GET-FILE cFileName
        TITLE "Choose Directory Containing Fix Programs"
        FILTERS "Programs (*.p)"  "*.p"
        CREATE-TEST-FILE
        USE-FILENAME
        DEFAULT-EXTENSION ".p"
        INITIAL-DIR fBaseCodeLocn:SCREEN-VALUE IN FRAME fMain.

    ASSIGN cFixDir = SUBSTRING(cFileName, 1,R-INDEX(cFileName,"\")).
    INPUT FROM OS-DIR (SUBSTRING(cFileName, 1,R-INDEX(cFileName,"\"))).
    IMPORT cFixList cTxt1 cTxt2.
    IF cFixList = "."
    OR cFixList = ".." 
    OR SUBSTRING(cTxt2,1,1) <> "F" THEN ASSIGN cFixList = "".
    REPEAT:
        IMPORT cFixEntry cTxt1 cTxt2.
        IF cFixEntry = "."
        OR cFixEntry = ".." 
        OR cTxt2 <> "F" THEN NEXT.
        ASSIGN cFixList = cFixList + "," + cFixEntry.
    END.
    INPUT CLOSE.
   
    DISPLAY 
        sList1 AT ROW 1 COLUMN 1
        bOK    AT ROW 1 COLUMN 57
        bCancel AT ROW 2.2 COLUMN 57
        SKIP(1)
        "(c) 2005, Foresight Software, Inc." AT ROW 7.5 COLUMN 2
        "All rights reserved." TO 68
        WITH FRAME a TITLE "fsInstaller - Select Fix Programs to Run"
        THREE-D NO-LABELS VIEW-AS DIALOG-BOX WIDTH 75.
    
    sList1:LIST-ITEMS IN FRAME a = TRIM(cFixList,",").
    ENABLE sList1 bOK bCancel WITH FRAME a.
    
    ON 'choose':U OF bOK IN FRAME a
    DO:
        ASSIGN cFixList = sList1:SCREEN-VALUE IN FRAME a.
    END.
    ON 'choose':U OF bCancel IN FRAME a
    DO:
        ASSIGN tRunFix:CHECKED IN FRAME fMain = FALSE.
        ASSIGN cFixList = "".
    END.

    WAIT-FOR 'choose' OF bOK IN FRAME a OR
             'choose' OF bCancel IN FRAME a.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpdateDB wWin 
PROCEDURE ipUpdateDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipWriteMessage (INPUT "Initiating ipUpdateDB in fsInstaller.w").

    DEF VAR iCtr1                   AS INT  NO-UNDO.
    DEF VAR iCtr2                   AS INT  NO-UNDO.
    DEF VAR cTxt1                   AS CHAR NO-UNDO.
    DEF VAR cTxt2                   AS CHAR NO-UNDO.
    DEF VAR lLog                    AS LOG  NO-UNDO.

    /* Check for missing data */
    IF cDeltaLocn = "" THEN DO:
        MESSAGE "No Delta .DF file name given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No Delta .DF file name given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.
    IF cDBName1 = "" THEN DO:
        MESSAGE "No target database name given.  Cancelling task execution." VIEW-AS ALERT-BOX ERROR.
        RUN ipWriteMessage (INPUT "ERROR: No target database name given.  Cancelling task execution.").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.

    /* Clean up the filename(s) so we know what to expect later */
    ASSIGN
        cDeltaLocn = REPLACE(cDeltaLocn,"/","\")
        cDbLocn = RIGHT-TRIM(cDbLocn,"/\") + "\"
        cDbLocn = REPLACE(cDbLocn,"/","\")
        cUpdateDbDir = cDbLocn
        cUpdateDBLocn = cDBLocn + "\" + cDBName1
        cDBFullName = RIGHT-TRIM(cDBLocn,"/\") + "\" + REPLACE(cDBName1,".db","") + ".db".
    
    /* Rip the .df filename, leaving the directory */    
    DO WHILE SUBSTRING(cUpdateDbDir,LENGTH(cUpdateDbDir),1) <> "\":
        ASSIGN cUpdateDbDir = SUBSTRING(cUpdateDbDir,1,(LENGTH(cUpdateDbDir) - 1)).
    END.

    /* If user wanted copy of .df, build a target and copy it in */
    IF tCopyDFFiles:CHECKED IN FRAME fMain AND SEARCH(cDeltaLocn) <> ? THEN DO:
        OS-CREATE-DIR VALUE(cDBLocn + "defs/").
        OS-COPY VALUE(cDeltaLocn) VALUE(cDBLocn + "defs/").
    END.
    
    /* This will show execution status to user without requiring interaction */
    FORM
        "Loading: " AT 2 cDeltaLocn FORMAT "x(60)"   AT 12 NO-LABEL
        SKIP(1)
        "(c) 2005, Foresight Software, Inc." AT 2
        "All rights reserved." TO 68
        WITH FRAME loaddf2 TITLE "fsInstaller - Updating Database Definitions"
        THREE-D SIDE-LABELS VIEW-AS DIALOG-BOX WIDTH 75.
    
    /* Run the delta load */
    IF SEARCH("prodict/load_df.r") <> ? THEN DO: 
        DISPLAY cDeltaLocn WITH FRAME loaddf2.
        /* Stupidly, the load_df program writes a message; this prevents the ugly grey screen */
        /* OUTPUT TO nul. */
        RUN prodict/load_df.r (INPUT cDeltaLocn + ",yes,NEW OBJECTS").
        /* OUTPUT CLOSE. */
    END.
    ELSE DO:
        RUN ipWriteMessage (INPUT "ERROR: load_df program was not found").
        ASSIGN lFoundError = TRUE.
        RETURN.
    END.

    /* If errors were generated (in .e file), append the .e text to the log, then delete the .e */
    IF SEARCH(REPLACE(REPLACE(cUpdateDbLocn,cUpdateDbDir,""),".db",".e")) <> ? THEN DO:
        RUN ipWriteMessage (INPUT "ERROR: Errors found during database update: (shown below)").
        OS-APPEND VALUE(SEARCH(REPLACE(REPLACE(cUpdateDbLocn,cUpdateDbDir,""),".db",".e"))) VALUE(SEARCH(cLogLocn + "fsInstall.log")).
        OS-DELETE VALUE(SEARCH(REPLACE(REPLACE(cUpdateDbLocn,cUpdateDbDir,""),".db",".e"))).
        ASSIGN lFoundError = TRUE.
    END.
    ELSE DO:
        OS-COPY VALUE(cDeltaLocn) VALUE(cDataFilesLocn).
    END.
    
    /* Note the status of the load in the log */
    IF NOT lFoundError THEN
        RUN ipWriteMessage (INPUT "Database " + LDBNAME(1) + " update from " + cDeltaLocn + " completed successfully.").
    ELSE
        RUN ipWriteMessage (INPUT "Database " + LDBNAME(1) + " update from " + cDeltaLocn + " completed with errors.").
    
    RUN ipWriteMessage (INPUT "Completing ipUpdateDB in fsInstaller.w").
    
    IF lStopOnError AND lFoundError THEN RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpdateVersion wWin 
PROCEDURE ipUpdateVersion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF SEARCH("updVersion.p") <> ? THEN RUN updVersion.p (INPUT cObjCodeLocn).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUseExisting wWin 
PROCEDURE ipUseExisting :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF cThisConfig = "" THEN ASSIGN cThisConfig = "fsInstall.cfg".

INPUT STREAM fileStream FROM VALUE(fLogDir:SCREEN-VALUE IN FRAME fMain + cThisConfig) NO-ECHO.

IMPORT STREAM fileStream UNFORMATTED cCreateDB.
IMPORT STREAM fileStream UNFORMATTED cUpdateDB.
IMPORT STREAM fileStream UNFORMATTED cLoadData.        
IMPORT STREAM fileStream UNFORMATTED cCreateBase.         
IMPORT STREAM fileStream UNFORMATTED cCreateCustom.       
IMPORT STREAM fileStream UNFORMATTED cCreateObject.       
IMPORT STREAM fileStream UNFORMATTED cCreateCustObj.       
IMPORT STREAM fileStream UNFORMATTED cCompileBase. 
IMPORT STREAM fileStream UNFORMATTED cCompilePatch.       
IMPORT STREAM fileStream UNFORMATTED cCompileCustom.      
IMPORT STREAM fileStream UNFORMATTED cCompileSelected.    
IMPORT STREAM fileStream UNFORMATTED cRunFix.             
IMPORT STREAM fileStream UNFORMATTED cCreateStartup.      
    
IMPORT STREAM fileStream UNFORMATTED cCopyDFFiles.        
IMPORT STREAM fileStream UNFORMATTED cCopyBaseFiles.      
IMPORT STREAM fileStream UNFORMATTED cCopyCustFiles.      
IMPORT STREAM fileStream UNFORMATTED cConnectMulti.       

IMPORT STREAM fileStream UNFORMATTED cClearLog.           
IMPORT STREAM fileStream UNFORMATTED cStopOnError.        
IMPORT STREAM fileStream UNFORMATTED cLogErrors.          

IMPORT STREAM fileStream UNFORMATTED cLogDir.             
    
IMPORT STREAM fileStream UNFORMATTED cDBLocn.             
IMPORT STREAM fileStream UNFORMATTED cDBName1.             
IMPORT STREAM fileStream UNFORMATTED cDBParms1.        
IMPORT STREAM fileStream UNFORMATTED cDFLocn.             
IMPORT STREAM fileStream UNFORMATTED cDeltaLocn.          
IMPORT STREAM fileStream UNFORMATTED cDataFilesLocn.      
IMPORT STREAM fileStream UNFORMATTED cBaseCodeLocn.       
IMPORT STREAM fileStream UNFORMATTED cPatchCodeLocn.       
IMPORT STREAM fileStream UNFORMATTED cCopyBaseTo.         
IMPORT STREAM fileStream UNFORMATTED cCustCodeLocn.       
IMPORT STREAM fileStream UNFORMATTED cCopyCustTo.         
IMPORT STREAM fileStream UNFORMATTED cObjCodeLocn.        
IMPORT STREAM fileStream UNFORMATTED cCustObjLocn.        
IMPORT STREAM fileStream UNFORMATTED cEnvTempLocn.        
IMPORT STREAM fileStream UNFORMATTED cEnvTargetLocn.      
IMPORT STREAM fileStream UNFORMATTED cProgressLocn.  

IMPORT STREAM fileStream UNFORMATTED cFixList.
IMPORT STREAM fileStream UNFORMATTED cFixEntry.
IMPORT STREAM fileStream UNFORMATTED cFileName.
IMPORT STREAM fileStream UNFORMATTED cFixDir.
IMPORT STREAM fileStream UNFORMATTED cBaseList.
IMPORT STREAM fileStream UNFORMATTED cCustList.
IMPORT STREAM fileStream UNFORMATTED cParmList.
IMPORT STREAM fileStream UNFORMATTED cDBDir2.
IMPORT STREAM fileStream UNFORMATTED cDBName2.
IMPORT STREAM fileStream UNFORMATTED cDBParms2.
IMPORT STREAM fileStream UNFORMATTED cDBDir3.
IMPORT STREAM fileStream UNFORMATTED cDBName3.
IMPORT STREAM fileStream UNFORMATTED cDBParms3.
IMPORT STREAM fileStream UNFORMATTED cDBDir4.
IMPORT STREAM fileStream UNFORMATTED cDBName4.
IMPORT STREAM fileStream UNFORMATTED cDBParms4.
IMPORT STREAM fileStream UNFORMATTED cDBDir5.
IMPORT STREAM fileStream UNFORMATTED cDBName5.
IMPORT STREAM fileStream UNFORMATTED cDBParms5.
IMPORT STREAM fileStream UNFORMATTED cDBDir6.
IMPORT STREAM fileStream UNFORMATTED cDBName6.
IMPORT STREAM fileStream UNFORMATTED cDBParms6.
import stream fileStream unformatted cPropath.

INPUT STREAM fileStream CLOSE.

ASSIGN
    tCreateDB:CHECKED IN FRAME fMain            = (IF cCreateDB = "yes" THEN TRUE ELSE FALSE)
    tUpdateDB:CHECKED IN FRAME fMain            = (IF cUpdateDB = "yes" THEN TRUE ELSE FALSE)
    tLoadData:CHECKED IN FRAME fMain            = (IF cLoadData = "yes" THEN TRUE ELSE FALSE)
    tCreateBase:CHECKED IN FRAME fMain          = (IF cCreateBase = "yes" THEN TRUE ELSE FALSE)
    tCreateCustom:CHECKED IN FRAME fMain        = (IF cCreateCustom = "yes" THEN TRUE ELSE FALSE)
    tCreateObject:CHECKED IN FRAME fMain        = (IF cCreateObject = "yes" THEN TRUE ELSE FALSE)
    tCreateCustObj:CHECKED IN FRAME fMain       = (IF cCreateCustObj = "yes" THEN TRUE ELSE FALSE)
    tCompileBase:CHECKED IN FRAME fMain         = (IF cCompileBase = "yes" THEN TRUE ELSE FALSE)
    tCompilePatch:CHECKED IN FRAME fMain        = (IF cCompilePatch = "yes" THEN TRUE ELSE FALSE)
    tCompileCustom:CHECKED IN FRAME fMain       = (IF cCompileCustom = "yes" THEN TRUE ELSE FALSE)
    tCompileSelected:CHECKED IN FRAME fMain     = (IF cCompileSelected = "yes" THEN TRUE ELSE FALSE)
    tRunFIX:CHECKED IN FRAME fMain              = (IF cRunFix = "yes" THEN TRUE ELSE FALSE)
    tCreateStartup:CHECKED IN FRAME fMain       = (IF cCreateStartup = "yes" THEN TRUE ELSE FALSE)
    
    tClearLog:CHECKED IN FRAME fMain            = (IF cClearLog = "yes" THEN TRUE ELSE FALSE)
    tStopOnError:CHECKED IN FRAME fMain         = (IF cStopOnError = "yes" THEN TRUE ELSE FALSE)
    tLogErrors:CHECKED IN FRAME fMain           = (IF cLogErrors = "yes" THEN TRUE ELSE FALSE)
    
    tCopyDFFiles:CHECKED IN FRAME fMain         = (IF cCopyDFFiles = "yes" THEN TRUE ELSE FALSE)
    tCopyBaseFiles:CHECKED IN FRAME fMain       = (IF cCopyBaseFiles = "yes" THEN TRUE ELSE FALSE)
    tCopyCustFiles:CHECKED IN FRAME fMain       = (IF cCopyCustFiles = "yes" THEN TRUE ELSE FALSE)
    tConnectMulti:CHECKED IN FRAME fMain        = (IF cConnectMulti = "yes" THEN TRUE ELSE FALSE)

    /* fLogDir:SCREEN-VALUE IN FRAME fMain         = cLogDir */
    
    fDBLocn:SCREEN-VALUE IN FRAME fMain         = cDBLocn
    fDBName:SCREEN-VALUE IN FRAME fMain         = cDBName1
    fDBConnParms:SCREEN-VALUE IN FRAME fMain    = cDBParms1
    fDFLocn:SCREEN-VALUE IN FRAME fMain         = cDFLocn
    fDeltaLocn:SCREEN-VALUE IN FRAME fMain      = cDeltaLocn
    fDataFilesLocn:SCREEN-VALUE IN FRAME fMain  = cDataFilesLocn
    fBaseCodeLocn:SCREEN-VALUE IN FRAME fMain   = cBaseCodeLocn
    fPatchCodeLocn:SCREEN-VALUE IN FRAME fMain  = cPatchCodeLocn
    fCopyBaseTo:SCREEN-VALUE IN FRAME fMain     = cCopyBaseTo
    fCustCodeLocn:SCREEN-VALUE IN FRAME fMain   = cCustCodeLocn
    fCopyCustTo:SCREEN-VALUE IN FRAME fMain     = cCopyCustTo
    fObjCodeLocn:SCREEN-VALUE IN FRAME fMain    = cObjCodeLocn
    fCustObjLocn:SCREEN-VALUE IN FRAME fMain    = cCustObjLocn
    fEnvTempLocn:SCREEN-VALUE IN FRAME fMain    = cEnvTempLocn
    fEnvTargetLocn:SCREEN-VALUE IN FRAME fMain  = cEnvTargetLocn
    fProgressLocn:SCREEN-VALUE IN FRAME fMain   = cProgressLocn
    
    cLogDir = fLogDir:SCREEN-VALUE IN FRAME fMain

    cFixList = SUBSTRING(cFixList,3)
    cFixEntry = SUBSTRING(cFixEntry,3)
    cFileName = SUBSTRING(cFileName,3)
    cFixDir = SUBSTRING(cFixDir,3)
    cBaseList = SUBSTRING(cBaseList,3)
    cCustList = SUBSTRING(cCustList,3)
    cParmList = SUBSTRING(cParmList,3)
    cDBDir2 = SUBSTRING(cDBDir2,3)
    cDBName2 = SUBSTRING(cDBName2,3)
    cDBParms2 = SUBSTRING(cDBParms2,3)
    cDBDir3 = SUBSTRING(cDBDir3,3)
    cDBName3 = SUBSTRING(cDBName3,3)
    cDBParms3 = SUBSTRING(cDBParms3,3)
    cDBDir4 = SUBSTRING(cDBDir4,3)
    cDBName4 = SUBSTRING(cDBName4,3)
    cDBParms4 = SUBSTRING(cDBParms4,3)
    cDBDir5 = SUBSTRING(cDBDir5,3)
    cDBName5 = SUBSTRING(cDBName5,3)
    cDBParms5 = SUBSTRING(cDBParms5,3)
    cDBDir6 = SUBSTRING(cDBDir6,3)
    cDBName6 = SUBSTRING(cDBName6,3)
    cDBParms6 = SUBSTRING(cDBParms6,3).

    IF cPropath <> "" THEN ASSIGN 
        propath = cPropath
        lPPModified = TRUE.
                

APPLY 'value-changed' TO tCreateDB IN FRAME fMain.
ASSIGN tConfigName:SCREEN-VALUE IN FRAME fMain = "Using " + cThisConfig + "...".

/* If Progress location hasn't been specified, get from current configuration */
IF cProgressLocn = "" AND SEARCH("prowin32.exe") <> ? THEN DO:
    ASSIGN
        cProgressLocn = SUBSTRING(SEARCH("prowin32.exe"),1,INDEX(SEARCH("prowin32.exe"),"bin") - 1).
    DO iCtr = 1 TO NUM-ENTRIES(cProgressLocn,"\"):
        IF INDEX(ENTRY(iCtr,cProgressLocn,"\")," ") <> 0 
        OR LENGTH(ENTRY(iCtr,cProgressLocn,"\")) > 8 THEN DO:
            REPLACE(ENTRY(iCtr,cProgressLocn,"\")," ","").
            ASSIGN 
                ENTRY(iCtr,cProgressLocn,"\") = TRIM(SUBSTRING(ENTRY(iCtr,cProgressLocn,"\"),1,6)," ") + "~~1"
                fProgressLocn:SCREEN-VALUE IN FRAME fMain = cProgressLocn.
       END.
    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipWriteMessage wWin 
PROCEDURE ipWriteMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
    /* I worked hard on this spacing; don't mess with it */
    DEF INPUT PARAMETER cMessage AS CHAR FORMAT "x(110)".
    IF NOT lErrorsOnlyLog 
    OR INDEX(cMessage,"ERROR") <> 0 
    OR INDEX(cMessage,"WARNING") <> 0 
    OR INDEX(cMessage,"Initiating") <> 0 
    OR INDEX(cMessage,"Completing") <> 0 
    THEN DO:
        OUTPUT STREAM logStream TO VALUE(cLogDir + REPLACE(cThisConfig,".cfg",".log")) APPEND.
        IF INDEX(cMessage,"ABORT") <> 0 THEN DO:
            PUT STREAM logStream UNFORMATTED " " + CHR(10).
            PUT STREAM logStream UNFORMATTED "******************************************" + CHR(10).
        END.
        IF INDEX(cMessage,"Initiating") <> 1 
        AND INDEX(cMessage,"Completing") <> 1
        AND INDEX(cMessage,"Previous") <> 1 
        AND INDEX(cMessage,"not selected") = 0 
        AND index(cMessage,"ABORT") = 0 THEN
            PUT STREAM logStream UNFORMATTED "  " + cMessage + FILL(" ",INT(99 - LENGTH(cMessage))) + STRING(TIME,"HH:MM:SS") + CHR(10).
        ELSE
            PUT STREAM logStream UNFORMATTED cMessage + FILL(" ",INT(101 - LENGTH(cMessage))) + STRING(TIME,"HH:MM:SS") + CHR(10).
        IF INDEX(cMessage,"Initiating") <> 0 
        AND INDEX(cMessage,"compile") <> 0 THEN DO:
            PUT STREAM logStream UNFORMATTED "**************** PROPATH *****************" + CHR(10).
            PUT STREAM logStream UNFORMATTED "   (PROPATH display suppressed. See config file for PROPATH details." + CHR(10).
            /*
            DO iCtr = 1 TO NUM-ENTRIES(PROPATH):
                PUT STREAM logStream UNFORMATTED ENTRY(iCtr,PROPATH) + "," + CHR(10).
            END.
            */
            PUT STREAM logStream UNFORMATTED "******************************************" + CHR(10).
        END.
        IF INDEX(cMessage,"Completing") = 1 
        OR INDEX(cMessage,"not selected") <> 0 
        OR INDEX(cMessage,"Previous") = 1 THEN PUT STREAM logStream UNFORMATTED " " + CHR(10).
        IF INDEX(cMessage,"ABORT") <> 0 THEN DO:
            PUT STREAM logStream UNFORMATTED "******************************************" + CHR(10).
            PUT STREAM logStream UNFORMATTED " " + CHR(10).
        END.
        OUTPUT STREAM logStream CLOSE.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

