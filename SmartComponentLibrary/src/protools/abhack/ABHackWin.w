&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases
*/
USING Consultingwerk.WindowIntegrationKit.*   FROM PROPATH.

&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win
/*------------------------------------------------------------------------

  File: ABHackWin.w by Sebastien Lacroix  November 2006

  Description: Nowadays, an Application Development Environment without
  auto-completion is a kind of torture.   So hack it or die...

  See protools/abhack/ABHackWin.readme.txt for a more detailed list of features and
  how to use, and about the supported versions of Progress

  This Hack Tool should be run with the PERSISTENT option in a developpement session.
    1) IT DOES NOT REQUIRE ANY RECOMPILATION OF THE ADE
    2) It is made in pure 4GL (very few simple win32 API's, and a few PSTimers)
    3) Source code is given

  This tool can spy the activity in source-code procedure editor to achieve auto
  completion for local or global variables, attributes/methods, buffers, temp-table,
  table and fields names.
  It includes many other features like a query analyser for a selected text, an
  improved navigation by words, ++ insertion, quick comment out

  Many thanks to my current project leader Jean Christophe Cardot for his strong
  interest and support in the achievement of this tool, and to my developper
  pears of PSA for their feedback when using it.

/* 11-DEC-2006 sla: Swith LARGE option off for the monitoring editor => less problems on Linux with Wine */

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
&SCOPED-DEFINE VSOPTION_DRAW_BOX_AROUND_CURRENT_LINE 15
/* 03-JUL-2007 sla: Many thanks to Johan Samyn for implementing this feature */
&SCOPED-DEFINE VSOPTION_RIGHT_MARGIN_INDICATOR_COL 3
&SCOPED-DEFINE VSOPTIONZ_SPECIAL_CHAR_XLAT_TAB 1001

/* 21-MAY-2007 sla: global Resource temp-tables are now defined in a seprate include file
 so we can reuse them in other procedures that will play with a few datasets...  */
{protools/abhack/ABHackResourcesTT.i  &SHARED="NEW SHARED"  &excludeGuiVars="YES"}

/* 01-SEP-2007 sla: necessary for shared variables */
&SCOPED-DEFINE NEW NEW

DEFINE TEMP-TABLE ttModified NO-UNDO LABEL "ttModified (to keep track editor in modified state)"
 FIELD hEditor AS HANDLE
 INDEX heditor heditor.

DEFINE BUFFER bttEdt FOR ttEdt. /* used for the browse */

DEFINE TEMP-TABLE ttDbTableCache NO-UNDO LABEL "ttDbTableCache (to cache schema of all connected databases)"
 FIELD cDataBase AS CHARACTER
 FIELD cTable    AS CHARACTER
 FIELD cFLA      AS CHARACTER   LABEL "FLA = _file._dump-File"
 INDEX dbTable IS PRIMARY UNIQUE cDataBase cTable
 INDEX cTable cTable /* important when no qualified database, not unique because a table name might be used in multiple databases */
 INDEX dbFla cDataBase cFla
 INDEX fla cFla. /* important when no qualified database, not unique because a table name might be used in multiple databases */


DEFINE TEMP-TABLE ttAlignAssign NO-UNDO LABEL "ttAlignAssign (used to align Assignement lines on CTRL-ALT-L)"
 FIELD iSeq        AS INTEGER
 FIELD cLeft       AS CHARACTER
 FIELD cRightPart  AS CHARACTER
 FIELD iLeftLength AS INTEGER
 INDEX iLeftLength iLeftLength
 INDEX iSeq iSeq.

DEFINE TEMP-TABLE ttAlignDef NO-UNDO LABEL "ttAlignDef (used to align Variable Definitions on CTRL-ALT-L)"
 FIELD cDataType   AS CHARACTER
 FIELD cDefType    AS CHARACTER /* DEF VAR or FIELD */
 FIELD cName       AS CHARACTER
 FIELD cRightPart  AS CHARACTER
 FIELD iNameLength AS INTEGER
 FIELD iSeq        AS INTEGER
 FIELD iTypeLength AS INTEGER
 FIELD lLikeAs     AS LOGICAL /* YES => LIKE  NO => AS */
 INDEX cName cName iSeq
 INDEX iTypeLength iTypeLength
 INDEX iNameLength iNameLength.

DEFINE TEMP-TABLE ttAlignParam NO-UNDO LABEL "ttAlignParam (used to align Param Definitions on CTRL-ALT-L)"
 FIELD cInOut        AS CHARACTER
 FIELD iInOutLength  AS INTEGER
 FIELD cDefType      AS CHARACTER   /* scalar,BUFFER,TABLE,TABLE-HANDLE,DATASET etc... */
 FIELD cName         AS CHARACTER
 FIELD iNameLength   AS INTEGER
 FIELD cDataType     AS CHARACTER
 FIELD iTypeLength   AS INTEGER
 FIELD cRightPart    AS CHARACTER
 FIELD iSeq          AS INTEGER
 FIELD lLikeAs       AS LOGICAL     /* YES => LIKE NO => AS */
 FIELD cBuffer       AS CHARACTER
 FIELD iBufferLength AS INTEGER
 INDEX iInOutLength iInOutLength
 INDEX iTypeLength iTypeLength
 INDEX iSeq iSeq
 INDEX iNameLength iNameLength
 INDEX iBufferLength iBufferLength.


DEFINE TEMP-TABLE ttCustAlias NO-UNDO LABEL "ttCustAlias (To define custom aliases on TAB Key)"
 FIELD cAlias     AS CHARACTER
 FIELD cExp       AS CHARACTER
 FIELD cLargeText AS CHARACTER
 INDEX cAlias cAlias.

DEFINE TEMP-TABLE ttAbortCompl NO-UNDO LABEL "ttAbortCompl (To manage words defined with the @abortCompletionProcess@ directive in the config file customTabCompletion.ofMine.txt)"
 FIELD cForThatWord AS CHARACTER
 INDEX cForThatWord cForThatWord.

DEFINE TEMP-TABLE testTT LIKE ttAlignDef LABEL "The point OF this TT is to test the catch of TT's defined LIKE another TT"
 FIELD additionalField AS CHARACTER.


DEFINE TEMP-TABLE ttVar NO-UNDO LABEL "ttVar (To keep track of variable definitions in a procedure editor)"
 FIELD cVar       AS CHARACTER
 FIELD cDataType  AS CHARACTER
 FIELD cViewAs    AS CHARACTER
 FIELD cClassFile AS CHARACTER HELP "Class file for non basic data types (resolved with USING's when needed)"
 INDEX cVar cVar. /* up to me to avoid duplicates when loading these guys */


DEFINE TEMP-TABLE ttbuffer NO-UNDO LABEL "ttbuffer (to keep track of local buffers)"
 FIELD cName AS CHARACTER
 FIELD cfor  AS CHARACTER
 INDEX cName cName.

DEFINE TEMP-TABLE ttAPI NO-UNDO LABEL "ttAPI (To define custom API tooltips to show on '(' )"
 FIELD cAPI  AS CHARACTER
 FIELD chelp AS CHARACTER
 INDEX cAPI cAPI.

DEFINE TEMP-TABLE ttAttr NO-UNDO LABEL "ttAttr (for completion of attributes and methods on TAB Key)"
 FIELD cAttr AS CHARACTER
 FIELD cObjTypes AS CHARACTER  /* list of Object/widget types for a given attribute/method */
 FIELD cSortOption AS CHARACTER
 INDEX cAttr cAttr
 INDEX cSortOptionAttr IS PRIMARY cSortOption cAttr
 INDEX cObjTypes IS WORD-INDEX cObjTypes.

DEFINE TEMP-TABLE ttQueryInfoBuffer NO-UNDO
  FIELD hBuffer AS HANDLE
  FIELD cDatabase AS CHARACTER
  FIELD cTableName AS CHARACTER
  INDEX iPrimary cDatabase cTableName.

DEFINE TEMP-TABLE ttObjType NO-UNDO LABEL "ttObjType (contains object types defined in attrTabCompletion<X>.txt)"
 FIELD cObjType AS CHARACTER
 INDEX cObjType cObjType.

DEFINE TEMP-TABLE ttGuessObjType NO-UNDO LABEL "ttGuessObjType (contains defintions to guess object types in attrGuessObjType<X>.txt)"
 FIELD cObjType AS CHARACTER
 FIELD cGuess   AS CHARACTER
 FIELD iGuessLength AS INTEGER
 INDEX cObjType cObjType
 INDEX guess iGuessLength DESCENDING cGuess.

DEFINE TEMP-TABLE ttUsedBuffers NO-UNDO LABEL "ttUsedBuffers (contains all the buffers used before the currrent line in FIND, EACH, FIRST, NEXT, LAST, PARAMETER BUFFER...)"
 FIELD cName AS CHARACTER
 FIELD iLine AS INTEGER
 INDEX cName cName
 INDEX iLine iLine DESCENDING.

DEFINE TEMP-TABLE ttSuggestBuffer
 NO-UNDO LABEL "ttSuggestBuffer (Used buffers to suggest in a popup)"
 FIELD cName AS CHARACTER
 FIELD iLine AS INTEGER
 INDEX cName IS UNIQUE cName
 INDEX iLine iLine DESCENDING.

DEFINE TEMP-TABLE ttBlockLabels NO-UNDO LABEL "ttBlockLabels (block labels to suggest after UNDO NEXT LEAVE or RETRY)"
 FIELD cName AS CHARACTER
 FIELD iLine AS INTEGER
 INDEX cName cName
 INDEX iLine iLine DESCENDING.

DEFINE TEMP-TABLE ttLibHandle NO-UNDO LABEL "ttLibHandle (to track RUN <someLib> PER SISTENT SET <someLocalVar>)"
 FIELD iLine            AS INTEGER
 FIELD cLibFile         AS CHARACTER
 FIELD cVar             AS CHARACTER
 FIELD lSuperOfThisProc AS LOGICAL
 INDEX cVar cVar
 INDEX iLine iLine.


/* global shared vars are not worse than using SUBSCRIBE ANYWHERE... */
DEFINE NEW GLOBAL SHARED VARIABLE gshABHackWin          AS HANDLE     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gshprocEditorTriggers AS HANDLE     NO-UNDO.


DEFINE STREAM edtImport.

&SCOPED-DEFINE giABHackedTextColorsExt 8
DEFINE VARIABLE gcABHackedPicture              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcABHackedPictures             AS CHARACTER   EXTENT {&giABHackedTextColorsExt} INITIAL ["1", "1", "1", "2", "3", "4", "5", "6"] NO-UNDO.
DEFINE VARIABLE gcAbhackFindMru                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcAbhackFindScope              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcDelayTriggerWinEvent         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcKeyWordsToConvertToCase      AS CHARACTER   INITIAL "NO-UNDO,INITIAL,LABEL,HELP,COLUMN-LABEL,EXTENT" NO-UNDO.
DEFINE VARIABLE gcLastSelection                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcABHackedPicRootPath          AS CHARACTER   NO-UNDO. /* 15-AUG-2007 sla: some people have protools/abhack relative to '.' and get problems when the system-dialog open-file is open */.
DEFINE VARIABLE gcProtoolsButtonTooltip        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcSuggestBufferForNewGuysStamp AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gdABHackedTextColorLastEtime   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE gdLastAltCEtime                AS DECIMAL     NO-UNDO.
DEFINE VARIABLE gdLoadLocalLastEtime           AS DECIMAL     NO-UNDO.
DEFINE VARIABLE ghABHackedText                 AS HANDLE      NO-UNDO.
DEFINE VARIABLE ghABHackSleepUntilPrintable    AS HANDLE      NO-UNDO.
DEFINE VARIABLE ghCurrentEditor                AS HANDLE      NO-UNDO.
DEFINE VARIABLE ghDelayTriggerHandle           AS HANDLE      NO-UNDO.
DEFINE VARIABLE ghEditorValueChanged           AS HANDLE      NO-UNDO. /* handle to pass to valueChanged form timer tick */
DEFINE VARIABLE ghLastActiveWinNoEditor        AS HANDLE      NO-UNDO.
DEFINE VARIABLE ghParser                       AS HANDLE      NO-UNDO.
DEFINE VARIABLE ghMainAppBuilderWindow         AS HANDLE      NO-UNDO.
DEFINE VARIABLE ghProtoolsButton               AS HANDLE      NO-UNDO.
DEFINE VARIABLE ghSectionLayoutWin             AS HANDLE      NO-UNDO.
DEFINE VARIABLE giABHackedTextColors           AS INTEGER     EXTENT {&giABHackedTextColorsExt} INITIAL [0,0,0,0,0,7,7,8] NO-UNDO.
DEFINE VARIABLE giABHackedTextColorSeq         AS INTEGER     NO-UNDO.
DEFINE VARIABLE giABHackedTextFgColor          AS INTEGER     NO-UNDO.
DEFINE VARIABLE giCtrlKeyStateLowerBit         AS INTEGER     INITIAL 2 NO-UNDO.
DEFINE VARIABLE giCursorLine                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE giEscKeyStateLowerBit          AS INTEGER     INITIAL 2 NO-UNDO.
DEFINE VARIABLE giMKeyStateLowerBit            AS INTEGER     INITIAL 2 NO-UNDO.
DEFINE VARIABLE giNumLines                     AS INTEGER     NO-UNDO.
DEFINE VARIABLE giPreferencesWinXOffset        AS INTEGER     NO-UNDO.
DEFINE VARIABLE giSpaceKeyStateLowerBit        AS INTEGER     INITIAL 2 NO-UNDO.
DEFINE VARIABLE giTabKeyStateLowerBit          AS INTEGER     INITIAL 2 NO-UNDO.
DEFINE VARIABLE giUpdateMarksAboveLine         AS INTEGER     NO-UNDO.
DEFINE VARIABLE giWinHeightPixelsMax           AS INTEGER     NO-UNDO.
DEFINE VARIABLE giWinWidthPixelsMax            AS INTEGER     NO-UNDO.
DEFINE VARIABLE giWinWidth                     AS INTEGER     NO-UNDO. /* to make the width invariant on a window-resized event */
DEFINE VARIABLE giWinHeight                    AS INTEGER     NO-UNDO. /* to keep track the preferences  */
DEFINE VARIABLE glHasSheSeenFrenchRomance      AS LOGICAL     NO-UNDO.

DEFINE VARIABLE giInfoWinBgcolor AS INTEGER   NO-UNDO.
DEFINE VARIABLE giInfoWinFgcolor AS INTEGER  INITIAL 10 NO-UNDO.  /* 14-SEP-2007 sla:  added initial 10 as default for very first run */
DEFINE VARIABLE giInfoWinHeight  AS INTEGER   NO-UNDO.
DEFINE VARIABLE giInfoWinWidth   AS INTEGER   NO-UNDO.
DEFINE VARIABLE giInfoWinX       AS INTEGER   NO-UNDO.
DEFINE VARIABLE giInfoWinY       AS INTEGER   NO-UNDO.
DEFINE VARIABLE glInfoWinTopOnly AS LOGICAL   NO-UNDO.

DEFINE VARIABLE glSectionLayoutWinTopOnly AS LOGICAL     NO-UNDO.
DEFINE VARIABLE giSectionLayoutWinHeight  AS INTEGER     NO-UNDO.
DEFINE VARIABLE giSectionLayoutWinWidth   AS INTEGER     NO-UNDO.
DEFINE VARIABLE giSectionLayoutWinX       AS INTEGER     NO-UNDO.
DEFINE VARIABLE giSectionLayoutWinY       AS INTEGER     NO-UNDO.


/* 05-DEC-2006 sla: resources to hack the data dictionnary */
DEFINE VARIABLE hDictFrame     AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_Browse_Stat AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_btn_Create  AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_btn_Dbs     AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_btn_Delete  AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_btn_Flds    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_btn_Idxs    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_btn_Props   AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_btn_Seqs    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_btn_Tbls    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_fil_Dbs     AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_fil_Flds    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_fil_Idxs    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_fil_Seqs    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_fil_Tbls    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_lst_Dbs     AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_lst_Flds    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_lst_Idxs    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_lst_Seqs    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_lst_Tbls    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_txt_DBs     AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_txt_Flds    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_txt_Idxs    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_txt_Seqs    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hs_txt_Tbls    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hwDict         AS HANDLE  NO-UNDO.




PROCEDURE GetKeyboardState EXTERNAL "user32.dll":
   DEFINE INPUT  PARAMETER KBState AS LONG. /* memptr */
   DEFINE RETURN PARAMETER RetVal  AS LONG. /* bool   */
END PROCEDURE.


PROCEDURE LockWindowUpdate EXTERNAL "user32.dll":
   DEFINE INPUT  PARAMETER piWindowHwnd AS LONG NO-UNDO.
   DEFINE RETURN PARAMETER piResult     AS LONG NO-UNDO.
END PROCEDURE.

/* 29-NOV-2006 sla: better than using _connect._Connect-Pid, because it does not require any connection to a Database */
PROCEDURE GetCurrentProcessId EXTERNAL "kernel32.dll":
    DEFINE RETURN PARAMETER IntProcessHandle AS LONG.
END PROCEDURE.


PROCEDURE ShellExecuteA EXTERNAL "shell32.dll" :
  DEFINE INPUT  PARAMETER hwnd          AS LONG.
  DEFINE INPUT  PARAMETER lpOperation   AS CHARACTER .
  DEFINE INPUT  PARAMETER lpFile        AS CHARACTER .
  DEFINE INPUT  PARAMETER lpParameters  AS CHARACTER .
  DEFINE INPUT  PARAMETER lpDirectory   AS CHARACTER .
  DEFINE INPUT  PARAMETER nShowCmd      AS LONG.
  DEFINE RETURN PARAMETER hInstance     AS LONG.
END PROCEDURE.

PROCEDURE ShowScrollBar EXTERNAL "user32.dll":
  DEFINE INPUT  PARAMETER hwnd     AS LONG NO-UNDO.
  DEFINE INPUT  PARAMETER fnBar    AS LONG NO-UNDO.
  DEFINE INPUT  PARAMETER fShow    AS LONG NO-UNDO.
  DEFINE RETURN PARAMETER rtnValue AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE GetCursorPos EXTERNAL "user32.dll" :
    DEFINE INPUT-OUTPUT PARAMETER lRect AS MEMPTR.
END PROCEDURE.

PROCEDURE ScreenToClient EXTERNAL "user32.dll" :
   DEFINE INPUT  PARAMETER hWnd     AS LONG.
   DEFINE INPUT  PARAMETER lpPoint  AS MEMPTR.
END PROCEDURE.

PROCEDURE Sleep EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT PARAMETER intMilliseconds AS LONG.
END PROCEDURE.

/* 06-SEP-2007 pjv: fix a problem with old progress versions (usage of filterocxevents)*/
PROCEDURE GetCommandLineA EXTERNAL "KERNEL32.DLL":
    DEFINE RETURN PARAMETER ptrToString AS MEMPTR.
END PROCEDURE.

PROCEDURE vsSetDefaultOption EXTERNAL "vsapi.dll":
   DEFINE INPUT  PARAMETER iOption  AS LONG    NO-UNDO.
   DEFINE INPUT  PARAMETER iValue   AS LONG    NO-UNDO.
END PROCEDURE.

PROCEDURE vsSetDefaultOptionZ EXTERNAL "vsapi.dll":
   DEFINE INPUT  PARAMETER iOption  AS LONG    NO-UNDO.
   DEFINE INPUT  PARAMETER iValue   AS CHARACTER    NO-UNDO.
END PROCEDURE.

/* 18-SEP-2007 sla:  */
PROCEDURE isAnyRunningABHackWinInfo:
    DEFINE OUTPUT PARAMETER ophYesItWasRunning AS HANDLE      NO-UNDO.
    ophYesItWasRunning = THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME bedtBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES bttEdt

/* Definitions for BROWSE bedtBrowse                                    */
&Scoped-define FIELDS-IN-QUERY-bedtBrowse bttEdt.cFileName bttEdt.cLastSaveOpenTime bttEdt.iNumLines
&Scoped-define ENABLED-FIELDS-IN-QUERY-bedtBrowse
&Scoped-define SELF-NAME bedtBrowse
&Scoped-define QUERY-STRING-bedtBrowse FOR EACH bttEdt BY bttEdt.dLastSaveOpenTime DESCENDING
&Scoped-define OPEN-QUERY-bedtBrowse OPEN QUERY {&SELF-NAME} FOR EACH bttEdt BY bttEdt.dLastSaveOpenTime DESCENDING.
&Scoped-define TABLES-IN-QUERY-bedtBrowse bttEdt
&Scoped-define FIRST-TABLE-IN-QUERY-bedtBrowse bttEdt


/* Definitions for FRAME fMain                                          */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cmon gcSectionOutlineMode ~
btnOpenReleaseNotes btnHide btnOpenPeg btnOpenPSDN btnOpenReadmePdf ~
glSectionLayoutIgnoreStructProcs BtnCompletion BtnKeys btnMisc glAbhackFind ~
bedtBrowse btnLaunchInfoFloatingWin btnExpand btnloadGlobalResources ~
btnStopStart
&Scoped-Define DISPLAYED-OBJECTS gcSectionOutlineMode ~
glSectionLayoutIgnoreStructProcs glAbhackFind

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD abReturnValue C-Win
FUNCTION abReturnValue RETURNS CHARACTER
  ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD carretInComment C-Win
FUNCTION carretInComment RETURNS LOGICAL
  (pcLine AS CHARACTER
  ,piPos  AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD carretInQuotedString C-Win
FUNCTION carretInQuotedString RETURNS LOGICAL
  (pcLine AS CHARACTER
  ,piPos  AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cLinzDummy C-Win
FUNCTION cLinzDummy RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createTTorDBBuffer C-Win
FUNCTION createTTorDBBuffer RETURNS HANDLE
  (phEditor     AS HANDLE
  ,pcBufferName AS CHARACTER
  ,pcOptn       AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD disabledApiTooltip C-Win
FUNCTION disabledApiTooltip RETURNS LOGICAL
  (pcApiTooltip AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getgiAutoCompMinSize C-Win
FUNCTION getgiAutoCompMinSize RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getglCompleteTableOnFLA C-Win
FUNCTION getglCompleteTableOnFLA RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getglHypeStrikeMode C-Win
FUNCTION getglHypeStrikeMode RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getglLowerCaseCompletion C-Win
FUNCTION getglLowerCaseCompletion RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPSTimerEnabled C-Win
FUNCTION getPSTimerEnabled RETURNS LOGICAL
 ( ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD innerTrim C-Win
FUNCTION innerTrim RETURNS CHARACTER
  (cString AS CHARACTER
  ,cTrimChar AS CHARACTER  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isValidBLockLabel C-Win
FUNCTION isValidBLockLabel RETURNS LOGICAL
  (pcBlockLabel AS CHARACTER
  ,pcLine       AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD queryIndexDetails C-Win
FUNCTION queryIndexDetails RETURNS CHARACTER
  (phBuffer AS HANDLE
  ,pcIndex AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD queryReplaceWord C-Win
FUNCTION queryReplaceWord RETURNS CHARACTER
 (pcPhrase AS CHARACTER
 ,pcWord   AS CHARACTER
 ,pcBy     AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD refinedAlias C-Win
FUNCTION refinedAlias RETURNS CHARACTER
  (pcAliasExp AS CHARACTER  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeLineComments C-Win
FUNCTION removeLineComments RETURNS CHARACTER
 (pcLine   AS CHARACTER
 ,piCIndex AS INTEGER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD wordBuffer C-Win
FUNCTION wordBuffer RETURNS CHARACTER
  (pcWord AS CHARACTER
  ,pcOptn AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD wordField C-Win
FUNCTION wordField RETURNS CHARACTER
  (pcWord AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlDelay AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlDelay AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlEdtValueChanged AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlEdtValueChanged AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlSpy AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlSpy AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE glAlignAssign AS LOGICAL INITIAL yes
     LABEL "Assign Statement (align equal signs)"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .71 NO-UNDO.

DEFINE VARIABLE glAlignEnabled AS LOGICAL INITIAL yes
     LABEL "A&lign on Ctrl-Alt-&L activated"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 TOOLTIP "To align selected text beautifully for VAR definition or ASSIGN" NO-UNDO.

DEFINE VARIABLE glAlignParams AS LOGICAL INITIAL yes
     LABEL "Parameter &definitions"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 TOOLTIP "Cannot be unchecked (fake check box to remind the feature)" NO-UNDO.

DEFINE VARIABLE glAlignTTFields AS LOGICAL INITIAL yes
     LABEL "Temp-table f&ields definition"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE glAlignTTFieldsSort AS LOGICAL INITIAL no
     LABEL "&Sort Temp-table Fields"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 TOOLTIP "BEWARE, do that on new temp-tables only, this changes the signature of TT's" NO-UNDO.

DEFINE VARIABLE glAlignVars AS LOGICAL INITIAL yes
     LABEL "Variable &definitions"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 TOOLTIP "Align selected definition of variable.  Beginning 1st line is reference" NO-UNDO.

DEFINE VARIABLE glCompleteBlockLabels AS LOGICAL INITIAL yes
     LABEL "Completion of block label enabled"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .71 TOOLTIP "Completion of block labels after NEXT,LEAVE, UNDO and RETRY" NO-UNDO.

DEFINE VARIABLE glClassNewCastAfterEqual AS LOGICAL INITIAL yes
     LABEL "NEW or CAST after '<classVar> ='"
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.

DEFINE VARIABLE glEnhancedCloseParenthese AS LOGICAL INITIAL yes
     LABEL "Enhanced behavior to insert )"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 TOOLTIP "Improved way to guess where to put a closing ')'" NO-UNDO.

DEFINE VARIABLE glInsertClosingGuys AS LOGICAL INITIAL yes
     LABEL "Enabled on ), ] single/double quotes"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 TOOLTIP "Insert closing ~" or ' or ) or [ as appropriate." NO-UNDO.

DEFINE BUTTON btnOpenAttrCompletionFile
     LABEL "Open attrTabCompletion<X>.txt"
     CONTEXT-HELP-ID 0
     SIZE 34 BY .91 TOOLTIP "Open the attrTabCompletion<ProgressVersion>.txt file".

DEFINE BUTTON btnOpenAttrGuessObjTypeFile
     LABEL "Open attrGuessObjType<X>.txt"
     CONTEXT-HELP-ID 0
     SIZE 31 BY .91 TOOLTIP "Open defintion file to guess object/widget types from variable names".

DEFINE VARIABLE glAddEndAfterDo AS LOGICAL INITIAL yes
     LABEL "Add END after DO: or REPEAT:"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 TOOLTIP "Insert a blank line and 'END.' automatically after DO: or REPEAT:" NO-UNDO.

DEFINE VARIABLE glCompleteAttrOnTab AS LOGICAL INITIAL yes
     LABEL "Attribute/Method popup on : key"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 TOOLTIP "See protools/abhack/attrTabCompletion<x>.txt file to add A4GBL attributes/methods" NO-UNDO.

DEFINE VARIABLE glCompleteOOSuperMethod AS LOGICAL INITIAL yes
     LABEL "OO  SUPER:method()"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 TOOLTIP "In a OO class, suggest current Method and parameters when typing ':' after SUPER" NO-UNDO.

DEFINE VARIABLE glFilterGuessObjectType AS LOGICAL INITIAL yes
     LABEL "Guess Object type from var name"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 TOOLTIP "To guess object/type base on var names, reloads loadttGuessObjType<X>.txt" NO-UNDO.

DEFINE VARIABLE glFilterOnViewAsWidgetType AS LOGICAL INITIAL yes
     LABEL "Filter on widget type (use view-as ...)"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 TOOLTIP "To filter attr list based on the view-as option used in variable definitions" NO-UNDO.

DEFINE BUTTON btnOpenCustomCompletionFile
     LABEL "Open customTabCompletion.txt"
     CONTEXT-HELP-ID 0
     SIZE 39 BY .91 TOOLTIP "Open the STANDARD protools/abhack/customTabCompletion.txt config file".

DEFINE BUTTON btnOpenCustomCompletionFileOfMin
     LABEL "Open customTabCompletion.ofMine.txt"
     CONTEXT-HELP-ID 0
     SIZE 39 BY .91 TOOLTIP "Open the protools/abhack/customTabCompletion.ofMine.txt config file".

DEFINE VARIABLE gcComplDateFormat AS CHARACTER FORMAT "X(20)":U INITIAL "DD-MMM-YYYY"
     LABEL "Compl. date format"
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "DD-MMM-YYYY","YYYY-MM-DD","MMM-DD-YYYY","DD FFFF YYYY","DD-MM-YYYY"
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE glCustomCompletionOnTab AS LOGICAL INITIAL yes
     LABEL "&Custom completion (refresh defs)"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 TOOLTIP "See protools/abhack/customTabCompletion.txt   A easier way to define smarter Aliases" NO-UNDO.

DEFINE VARIABLE gcFieldSortedBy AS CHARACTER INITIAL "order"
     CONTEXT-HELP-ID 0
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS
          "Sorted by &order", "order",
"na&me", "name"
     SIZE 31 BY .71 TOOLTIP "If you have a clean schema with relevant orders, then you might want to use them" NO-UNDO.

DEFINE VARIABLE glCompleteField AS LOGICAL INITIAL yes
     LABEL "Complete DB && TT Fields on dot"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 TOOLTIP "Suggest Field list when typing dot after a valid table name" NO-UNDO.

DEFINE VARIABLE giAutoCompMinSize AS INTEGER FORMAT "9":U INITIAL 3
     LABEL " => Min size of words for completion"
     VIEW-AS FILL-IN
     SIZE 3 BY .86 TOOLTIP "Minimum Length of word to fire a completion popup list" NO-UNDO.

DEFINE VARIABLE giTabNumSpaces AS INTEGER FORMAT "9":U INITIAL 4
     LABEL " => Num of spaces for a normal TAB"
     VIEW-AS FILL-IN
     SIZE 3 BY .86 TOOLTIP "Number of spaces to insert for a normal TAB (when no completion is available)" NO-UNDO.

DEFINE VARIABLE glAutoComplete AS LOGICAL INITIAL yes
     LABEL "Auto complete/popup while typing"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .71 TOOLTIP "The killer guy... switch it on, and suggestions will come without pressing TAB"
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE glCompleteOnTab AS LOGICAL INITIAL yes
     LABEL "Complete/popup on TAB"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 TOOLTIP "Fire completion/popup on TAB key when applicable (otherwise insert 4 spaces)" NO-UNDO.

DEFINE VARIABLE glCompleteWithSimilar AS LOGICAL INITIAL yes
     LABEL "Similar completion on Ctrl-alt-left/right"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .71 TOOLTIP "To be changed in setTooltips" NO-UNDO.

DEFINE VARIABLE glHypeStrikeMode AS LOGICAL INITIAL yes
     LABEL "Hype Strike Mode (push on space,.:()[])"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .71 TOOLTIP "This feature is really a killer guy for hypes!!!!"
     BGCOLOR 12  NO-UNDO.

DEFINE VARIABLE glLowerCaseCompletion AS LOGICAL INITIAL no
     LABEL "Completion with lower case"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .71 TOOLTIP "Force usage of lower case for some completions (attributes, custom, 'no-lock')" NO-UNDO.

DEFINE VARIABLE glNoCompletionInComment AS LOGICAL INITIAL yes
     LABEL "No completion in comment (single line)"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .71 TOOLTIP "Disable completion popups and tooltips in a  comment (limited to single line)" NO-UNDO.

DEFINE VARIABLE glNoCompletionInStrings AS LOGICAL INITIAL yes
     LABEL "No completion in quoted strings"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .71 TOOLTIP "Disable completion popups and tooltips when the carret is in a quoted string" NO-UNDO.

DEFINE VARIABLE gcExtProcIgnoreDir AS CHARACTER FORMAT "X(256)":U INITIAL "%DLC%,%DLC%/src,%DLC%/bin"
     LABEL "Ignore dirs"
     VIEW-AS FILL-IN
     SIZE 25 BY 1 TOOLTIP "List of Directory patterns to ignore" NO-UNDO.

DEFINE VARIABLE giMaxEtimeToLoadFiles AS INTEGER FORMAT ">>9":U INITIAL 200
     LABEL "Max time to spend at loading"
     VIEW-AS FILL-IN
     SIZE 6 BY 1 TOOLTIP "Maximum number of milliseconds to spend in the load file process" NO-UNDO.

DEFINE VARIABLE giMaxFilesToLoad AS INTEGER FORMAT ">>9":U INITIAL 100
     LABEL "Max Number of Items"
     VIEW-AS FILL-IN
     SIZE 6 BY 1 TOOLTIP "Maximum number of dir,*.p,*.w files to load in the list" NO-UNDO.

DEFINE VARIABLE giSplitParamInCalls AS INTEGER FORMAT ">>9":U INITIAL 2
     LABEL "Split line when more param than"
     VIEW-AS FILL-IN
     SIZE 5 BY 1 TOOLTIP "Maximum number of parameter to keep in one line in call" NO-UNDO.

DEFINE VARIABLE gcSplitParamCommaStyle AS CHARACTER INITIAL "leadingComma"
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS
          "leading comma", "leadingComma",
"trailing comma", "trailingComma"
     SIZE 37 BY .71 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 40 BY 5.1.

DEFINE VARIABLE glExtProcCompletion AS LOGICAL INITIAL yes
     LABEL "OS File list after RUN *.p,*.w or *.i"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81 TOOLTIP "To popup of OS File list after RUN *.p or RUN *.w or" NO-UNDO.

DEFINE VARIABLE glFindOSFilesWithAbhackDB AS LOGICAL INITIAL yes
     LABEL "Use ABHack.db to find OS files"
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 TOOLTIP "To use the ABHack Catalog DB rather scanning the OS file system => MUCH Faste !"
     BGCOLOR 13  NO-UNDO.

DEFINE VARIABLE glIntProcCompletion AS LOGICAL INITIAL yes
     LABEL "Internal Procedure List after RUN"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81 TOOLTIP "To popup a suggestion list of Internal Procedures after RUN" NO-UNDO.

DEFINE VARIABLE glIntProcInputInSig AS LOGICAL INITIAL no
     LABEL "Keep 'INPUT' in param signature"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 TOOLTIP "Keep the 'INPUT' keyword when inserting simple INPUT parameters" NO-UNDO.

DEFINE VARIABLE glRunInLibComp AS LOGICAL INITIAL yes
     LABEL "LibHandles after RUN IN  => then IP"
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 TOOLTIP "Requires access to abhack global resource defintion desc files (OE 10.0+)" NO-UNDO.

DEFINE BUTTON btnSuggesBufferSelectAll  NO-FOCUS
     LABEL "<= Select All"
     SIZE 20 BY 1.91.

DEFINE BUTTON btnSuggesBufferSelectAll-2  NO-FOCUS
     LABEL "Help"
     SIZE 20 BY 1.91 TOOLTIP "Indeed, press this button once for quick help".

DEFINE VARIABLE gcSuggestBufferFor AS CHARACTER
     VIEW-AS SELECTION-LIST MULTIPLE SORT SCROLLBAR-VERTICAL
     LIST-ITEMS "AVAILABLE","WHERE","OF","AND","OR","DELETE","NOT","IF","BUFFER-COPY","TO","VALIDATE","RELEASE","AVAIL","CREATE","LIKE","BUFFER-COMPARE","BY","DISP","DISPLAY","ASSIGN","=","ROWID","RECID","AMBIGUOUS","LOCKED","EACH","FIRST","LAST","PREV","NEXT"
     SIZE 21 BY 4.05 TOOLTIP "Multi-select the words after which ABHack shall suggests used buffers" NO-UNDO.

DEFINE VARIABLE glAddNoLock AS LOGICAL INITIAL yes
     LABEL "Propose NO-LOCK/EXCLUSIVE"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81 TOOLTIP "Propose NO-LOCK and EXCLUSIVE when appropriate (after EACH, FIRST... no TT)" NO-UNDO.

DEFINE VARIABLE glAddWhereAfterBuffer AS LOGICAL INITIAL yes
     LABEL "WHERE after FIRST/EACH... <buffer>"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 TOOLTIP "To automatically porpose WHERE after FIRST/EACH/LAST <buffer>" NO-UNDO.

DEFINE VARIABLE glCompleteTableOnFLA AS LOGICAL INITIAL no
     LABEL "First try with &FLA (dump-file)"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 TOOLTIP "Try to identify a table with short dump file-name (Dynamics 5 Letters Acronym)" NO-UNDO.

DEFINE VARIABLE glConvertDbTablesToLC AS LOGICAL INITIAL no
     LABEL "Convert 100% CAPS to lower case"
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .81 TOOLTIP "To convert 100% UPPER Case Table and Field names to lower case" NO-UNDO.

DEFINE VARIABLE glIntFuncCompletion AS LOGICAL INITIAL yes
     LABEL "User Defined Function / Method Compl."
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 TOOLTIP "To offer completion of User Defined Function / Method together with variables" NO-UNDO.

DEFINE VARIABLE glVariableCompletion AS LOGICAL INITIAL yes
     LABEL "&Variable name/TT completion"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 TOOLTIP "To popup a suggestion list of local/global variable & Temp-tables" NO-UNDO.

DEFINE IMAGE imgPage
     FILENAME "adeicon/blank":U
     STRETCH-TO-FIT
     SIZE 28.6 BY 6.33 TOOLTIP "A few other amazing pics are available in protools\abhack\pics".

DEFINE VARIABLE gcActionOnCtrlS AS CHARACTER INITIAL "saveAndGlobalLoad"
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS
          "Just save", "saveOnly",
"Save + Global Load", "saveAndGlobalLoad"
     SIZE 37 BY .95 TOOLTIP "Save and Global Load =>  Ctrl-S = F6 + Alt-G (good for structured procs)" NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 42 BY 1.67.

DEFINE VARIABLE glAdaptWindowHeightOnCtrlW AS LOGICAL INITIAL yes
     LABEL "Adapt windows height to editor lines on Ctrl-W"
     VIEW-AS TOGGLE-BOX
     SIZE 49 BY .81 TOOLTIP "Extend window's height up to the taskBar to show more code" NO-UNDO.

DEFINE VARIABLE glcommentOnDblSlash AS LOGICAL INITIAL yes
     LABEL "Comment or uncomment line on '//'  (or add comment at end of line)"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 68 BY .81 TOOLTIP "Just press slash twice anywhere in a line to comment/uncomment it" NO-UNDO.

DEFINE VARIABLE glcommentSelectionOnSlash AS LOGICAL INITIAL yes
     LABEL "Comment/uncomment selection on '/'"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 TOOLTIP "Just press slash to comment or uncomment a selected text" NO-UNDO.

DEFINE VARIABLE glCompile AS LOGICAL INITIAL yes
     LABEL "Compile on Ctrl-Alt-M"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 TOOLTIP "Compile the active file on CTRL-ALT-M (Monitor reports if success)" NO-UNDO.

DEFINE VARIABLE glCompileMinsize AS LOGICAL INITIAL no
     LABEL "Use MIN-SIZE option"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 TOOLTIP "Add MIN-SIZE to the compile statement" NO-UNDO.

DEFINE VARIABLE glCopyUIBelowMouse AS LOGICAL INITIAL yes
     LABEL "Alt-Ctrl-C=>copy UI-value of widget below the mouse pointer"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 62 BY .81 TOOLTIP "Copy active-window SCREEN-VALUE/LABEL below the mouse pointer on Alt-Ctrl-C" NO-UNDO.

DEFINE VARIABLE glCtrlBackspacelDelPrevWord AS LOGICAL INITIAL yes
     LABEL "Del Prev Word on Ctrl-Backspace"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81 TOOLTIP "To delete Previous Word on Ctrl-Backspace instead of del current line (default)" NO-UNDO.

DEFINE VARIABLE glCtrlDelDelsWord AS LOGICAL INITIAL yes
     LABEL "Del Current Word on Ctrl-Del"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 TOOLTIP "To delete current word on Ctrl-Delete" NO-UNDO.

DEFINE VARIABLE glCtrlF3NewProcWin AS LOGICAL INITIAL yes
     LABEL "New procedure Window on Ctrl-F3 (from section editor)"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 57 BY .81 TOOLTIP "Handy to open a new procedure editor from a section editor window" NO-UNDO.

DEFINE VARIABLE glDebugListingOnCtrlAltD AS LOGICAL INITIAL yes
     LABEL "Debug listing on Ctrl-Alt-D"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 TOOLTIP "Compile-Listing of the active file on CTRL-ALT-T" NO-UNDO.

DEFINE VARIABLE glDuplicateLineOnAltDown AS LOGICAL INITIAL yes
     LABEL "Duplicate line on Alt-Ctrl-Down/Up"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .71 TOOLTIP "To duplicate the current line like Eclipse does on Ctrl-Alt-Down" NO-UNDO.

DEFINE VARIABLE glFindNextSelected AS LOGICAL INITIAL yes
     LABEL "Find Next/Prev Selected Text on Alt-Right/Left"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 49 BY .71 TOOLTIP "Find next/prev Occurence of Selected Text or Current Word on Alt-Right/Left" NO-UNDO.

DEFINE VARIABLE glHideAllComments AS LOGICAL INITIAL yes
     LABEL "Hide all comments / Show All on Alt-C"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 TOOLTIP "To be changed by program" NO-UNDO.

DEFINE VARIABLE glHideSelection AS LOGICAL INITIAL yes
     LABEL "Hide/Show Selection on Alt-H/Alt-S"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 TOOLTIP "To be changed in setTooltips" NO-UNDO.

DEFINE VARIABLE glImprovedCtrlKeftRight AS LOGICAL INITIAL yes
     LABEL "Improved Navigation by Word  Ctrl-Left/Right"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 TOOLTIP "Improved behavior on Ctrl-cursor-left/right to navigate by words" NO-UNDO.

DEFINE VARIABLE glListingOnCtrlAltT AS LOGICAL INITIAL yes
     LABEL "Listing on Ctrl-Alt-T"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 TOOLTIP "Compile-Listing of the active file on CTRL-ALT-T" NO-UNDO.

DEFINE VARIABLE glMaximizeRestore AS LOGICAL INITIAL yes
     LABEL "Maximize/Restore Window on Ctrl-M"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 TOOLTIP "To be set in setTooltips" NO-UNDO.

DEFINE VARIABLE glMulitpleClipboardDummyToggle AS LOGICAL INITIAL yes
     LABEL "Multiple clipboard on Ctrl-Alt-V"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 TOOLTIP "To be set in setTooltips" NO-UNDO.

DEFINE VARIABLE glOpenPW AS LOGICAL INITIAL yes
     LABEL "Generated procedure window on Ctrl-Alt-E   (for global search)"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 63 BY .81 TOOLTIP "Open the whole generated source code for the current section editor" NO-UNDO.

DEFINE VARIABLE glShowLineNumbers AS LOGICAL INITIAL yes
     LABEL "Show/Hide Line Numbers on Ctrl-Alt-I"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 TOOLTIP "Native VSlick feature to show/hide line numbers" NO-UNDO.

DEFINE VARIABLE glShowSpecialChars AS LOGICAL INITIAL yes
     LABEL "Show/Hide sPecial chars (space, CR) on Ctrl-Alt-P"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 53 BY .81 TOOLTIP "Native VSLick feature to show hidden characters like Space and CR" NO-UNDO.

DEFINE VARIABLE glStopStartTimerOnCtrlAltO AS LOGICAL INITIAL yes
     LABEL "Stop/Start Spying Timer on Ctrl-Alt-O"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 TOOLTIP "It can be interesting to stop the timer before tracing the A4GBL" NO-UNDO.

DEFINE VARIABLE glXrefAnal AS LOGICAL INITIAL yes
     LABEL "Analyze XRef utility on Ctrl-Alt-X  (from section editor)"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .81 TOOLTIP "Launch an XREF analyzer tool" NO-UNDO.

DEFINE BUTTON BtnCompletion
     LABEL "&Completion"
     SIZE 23 BY 1.67.

DEFINE BUTTON btnExpand  NO-FOCUS FLAT-BUTTON
     LABEL ">>"
     CONTEXT-HELP-ID 0
     SIZE 5 BY 1.19 TOOLTIP "To access source file browse & preferences.  Right-click to go *backward*".

DEFINE BUTTON btnHide  NO-FOCUS FLAT-BUTTON
     LABEL "&Hide"
     CONTEXT-HELP-ID 0
     SIZE 6 BY 1.19 TOOLTIP "Relaunching it from Protools will restore this window".

DEFINE BUTTON BtnKeys
     LABEL "&Keys"
     SIZE 18 BY 1.67.

DEFINE BUTTON btnLaunchInfoFloatingWin  NO-FOCUS FLAT-BUTTON
     LABEL "Info Win"
     SIZE 10 BY 1.19 TOOLTIP "Launch a ABHach Info Floating Window now".

DEFINE BUTTON btnloadGlobalResources  NO-FOCUS FLAT-BUTTON
     LABEL "Load &Global"
     CONTEXT-HELP-ID 0
     SIZE 13 BY 1.19 TOOLTIP "To extract global vars & TT from a compile listing, can take a few secs".

DEFINE BUTTON btnMisc
     LABEL "&Misc"
     SIZE 18 BY 1.67.

DEFINE BUTTON btnOpenPeg
     LABEL "ABHack on the PEG"
     CONTEXT-HELP-ID 0
     SIZE 29 BY 1.19 TOOLTIP "Open http://www.peg.com/lists/abhack/web/"
     FGCOLOR 9 FONT 6.

DEFINE BUTTON btnOpenPSDN
     LABEL "ABHack on the OpenEdge Hive"
     CONTEXT-HELP-ID 0
     SIZE 41 BY 1.19 TOOLTIP "http://www.oehive.org/abhack".

DEFINE BUTTON btnOpenReadmePdf
     LABEL "Readme.pdf"
     CONTEXT-HELP-ID 0
     SIZE 17 BY 1.19 TOOLTIP "Open Readme.pdf".

DEFINE BUTTON btnOpenReleaseNotes
     LABEL "Release Notes"
     CONTEXT-HELP-ID 0
     SIZE 29 BY 1.19 TOOLTIP "Open the release notes".

DEFINE BUTTON btnStopStart  NO-FOCUS FLAT-BUTTON
     LABEL "&Stop"
     CONTEXT-HELP-ID 0
     SIZE 6 BY 1.19 TOOLTIP "To Stop/Start the Spying Timer manually (think of4gl trace logging)".

DEFINE VARIABLE cmon AS CHARACTER
     CONTEXT-HELP-ID 0
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 40 BY 2.86 TOOLTIP "Monitoring info  (green if global resource already loaded for current editor)"
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE gcSectionOutlineMode AS CHARACTER INITIAL "free"
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS
          "No Section Outline", "disabled",
"Free Section Outline Window", "free",
"Section Outline stuck to editor", "stick"
     SIZE 36 BY 2.86 NO-UNDO.

DEFINE VARIABLE glAbhackFind AS LOGICAL INITIAL yes
     LABEL "Enable ABHack Find Window"
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE glSectionLayoutIgnoreStructProcs AS LOGICAL INITIAL no
     LABEL "Ignore structured procedure"
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE BUTTON BtnBulkDumpAbhackGlobDesc
     LABEL "Bulk Dump Utility for ABHack Resource Descriptions"
     SIZE 55 BY 1.14 TOOLTIP "Bulk Dump Utility of ABHack GlobResource  to XML files or to abhack.db".

DEFINE BUTTON btnOpenAbhackDbReadme
     LABEL "Open AbhackDbReadme.txt"
     CONTEXT-HELP-ID 0
     SIZE 35 BY 1.19 TOOLTIP "Open AbhackDbReadme  PLEASE READ".

DEFINE BUTTON btnOpenAPITooltipFile
     LABEL "Open customAPITooltip.txt"
     CONTEXT-HELP-ID 0
     SIZE 35 BY 1.43 TOOLTIP "Open Readme.txt".

DEFINE BUTTON btnOpenHowToShowMatchingParent
     LABEL "Instructions to make the editor show you the matching '(' when typing ')'"
     CONTEXT-HELP-ID 0
     SIZE 91 BY 1.14 TOOLTIP "Open the help file with steps to enable native VSlick feature : show matching ("
     FONT 6.

DEFINE BUTTON BtnReenableAllAPITooltips
     LABEL " => Re-enable all API Tooltips"
     SIZE 35 BY 1.43 TOOLTIP "To re-enable API Tooltip that were disabled individually".

DEFINE BUTTON btnTestAbhackDb
     LABEL "Hit Return in the fill-in above of Press this to validate it"
     SIZE 53 BY 1.19.

DEFINE VARIABLE gcFancyInABWin AS CHARACTER FORMAT "X(256)":U INITIAL "viveLaFrance"
     LABEL "Fancy in AppBuilder Win"
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "viveLaFrance","viveLaFrance",
                     "text","text",
                     "smiley","smileyGun",
                     "nothing","nothing"
     DROP-DOWN-LIST
     SIZE 18 BY 1 TOOLTIP "To display a fancy picture or text in the AppBuilder Window" NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE gcGlobalResCat AS CHARACTER FORMAT "X(256)":U INITIAL ?
     LABEL " Global Resource Catalog"
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "disabled","disabled",
                     "XML Files","XML",
                     "ABHack Database","DB"
     DROP-DOWN-LIST
     SIZE 24 BY 1 TOOLTIP "THE REAL POWER OF ABHACK... YOU SHOULD DEFINITELY USE IT"
     BGCOLOR 13  NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE gcCatDbConnection AS CHARACTER FORMAT "X(256)":U INITIAL ?
     LABEL "DB Connection phrase"
     VIEW-AS FILL-IN
     SIZE 35 BY 1 TOOLTIP "Connection phrase to abhack.db.  Do not forget '-db' if you use -S"
     BGCOLOR 13  NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE gcDumpedResourceFileRoot AS CHARACTER FORMAT "X(256)":U INITIAL ?
     LABEL "XML Files Root Dir"
     VIEW-AS FILL-IN
     SIZE 35 BY 1 TOOLTIP "Root directory to dump/load xml abhack global resource files"
     BGCOLOR 13  NO-UNDO.

DEFINE VARIABLE giMaxEmptyLinesBeforeShrinking AS INTEGER FORMAT ">>9":U INITIAL 7
     LABEL "Max empty lines before shrinking an editor window down"
     VIEW-AS FILL-IN
     SIZE 7 BY 1 TOOLTIP "0 to disable.  Minimum value is 5 lines" NO-UNDO.

DEFINE VARIABLE giQueryTooltipTimeFailTxt AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0
      VIEW-AS TEXT
     SIZE 6 BY .62 TOOLTIP "Time in ms set with associated slider" NO-UNDO.

DEFINE VARIABLE giQueryTooltipTimeSuccessTxt AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0
      VIEW-AS TEXT
     SIZE 7 BY .48 TOOLTIP "Time in ms set with associated slider" NO-UNDO.

DEFINE VARIABLE giRightMarginIndicatorCol AS INTEGER FORMAT ">>>9":U INITIAL 100
     LABEL "Right margin indicator at column"
     VIEW-AS FILL-IN
     SIZE 8 BY 1 TOOLTIP "Type the position for the right margin indicator line. (0 = disable)" NO-UNDO.

DEFINE VARIABLE giNameComboFont AS INTEGER INITIAL 1
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS
          "use ?", -1,
"use 1", 1,
"original 2", 2
     SIZE 32 BY .95 TOOLTIP "A non fixed FONT (like 1) will help to make long procedure names fit in combo" NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 53 BY 3.81.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 61 BY 4.29.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 60 BY 3.1.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 53 BY 4.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 53 BY 3.67.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 61 BY 8.33.

DEFINE VARIABLE giQueryTooltipTimeFail AS INTEGER INITIAL 800
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 5000 HORIZONTAL
     TIC-MARKS NONE
     SIZE 42 BY 1.19 TOOLTIP "Time in ms to keep a Query Analyser Tooltip when analises failed" NO-UNDO.

DEFINE VARIABLE giQueryTooltipTimeSuccess AS INTEGER INITIAL 2500
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 5000 HORIZONTAL
     TIC-MARKS NONE
     SIZE 42 BY 1.19 TOOLTIP "Time in ms to keep a Query Analyser Tooltip when analises are success" NO-UNDO.

DEFINE VARIABLE glAnalyseQuery AS LOGICAL INITIAL yes
     LABEL "Analyse Text Selected Static Queries"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 TOOLTIP "Analyse static query of selected text if it begins with FOR or FIND" NO-UNDO.

DEFINE VARIABLE glAutoStopDialog AS LOGICAL INITIAL no
     LABEL "Stop spying when the focus is in dialog"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY .81 TOOLTIP "To stop spying when focus is in a dialog-box.  Stability for 9.1E00 or earlier" NO-UNDO.

DEFINE VARIABLE glAutoStopWhenNonEditorWin AS LOGICAL INITIAL no
     LABEL "Stop spying when not in editor window (leverage tracing)"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 58 BY .81 TOOLTIP "Auto stop when active-window is not an editor window => leverage A4GBL tracing" NO-UNDO.

DEFINE VARIABLE glDisplaytextSelectedInfo AS LOGICAL INITIAL yes
     LABEL "Display Info about selected Text (in Info Win)"
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY .81 NO-UNDO.

DEFINE VARIABLE glDrawBoxAroundCurrentLine AS LOGICAL INITIAL yes
     LABEL "Draw box aroud current line"
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE glForceWaitFor AS LOGICAL INITIAL no
     LABEL "Force a WAIT-FOR in the ABHack procedure"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 TOOLTIP "To avoid some instabilities in 9.IE FCS (no SP), or some other 10.0Bxx" NO-UNDO.

DEFINE VARIABLE glJumpMruSection AS LOGICAL INITIAL yes
     LABEL "Navigate MRU Sections on Alt-Page-Up/Down"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 50 BY .81 TOOLTIP "Jump to previous/next Most Recently Used section on Alt-Page-Up/Down" NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE glKeepTrackListingBuffer AS LOGICAL INITIAL no
     LABEL "Always keep track compile-listing buffer usage && scope"
     VIEW-AS TOGGLE-BOX
     SIZE 57 BY .81 TOOLTIP "A MUST HAVE TO MASTER RECORD BUFFER SCOPE" NO-UNDO.

DEFINE VARIABLE glKeepTrackOfCursor AS LOGICAL INITIAL yes
     LABEL "Keep track of the Cursor Position in each Section"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 51 BY .81 TOOLTIP "Keep track of cursor row+col of section to restore it when we come back to them" NO-UNDO.

DEFINE VARIABLE glMoveNewWindowsAway AS LOGICAL INITIAL yes
     LABEL "Move new detected editor windows away from ABHack"
     VIEW-AS TOGGLE-BOX
     SIZE 56 BY .81 TOOLTIP "To avoid procedure windows open with drag and drop that overlay ABHack" NO-UNDO.

DEFINE VARIABLE glPlusplus AS LOGICAL INITIAL yes
     LABEL "myVar&++   ==>   myVar =  myVar +   (for any word)"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 51 BY .81 TOOLTIP "tootlip set in main block by program" NO-UNDO.

DEFINE VARIABLE glProposeOpenSelectedFile AS LOGICAL INITIAL yes
     LABEL "Propose to open selected source file  (path + .p/.w/.i)"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 55 BY .81 TOOLTIP "Propose to open the text-selected source file-name in the AppBuilder" NO-UNDO.

DEFINE VARIABLE glQADispInABHWin AS LOGICAL INITIAL no
     LABEL "Display report in floating Info Window"
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.

DEFINE VARIABLE glResizableDictionary AS LOGICAL INITIAL yes
     LABEL "Catch Data Dictionary and make it resizable"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY .81 TOOLTIP "See protools/abhack/openParentheseAPI.txt   A file to redefine tooltip help" NO-UNDO.

DEFINE VARIABLE glSaveSession AS LOGICAL INITIAL no
     LABEL "Save && restore session"
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .95 TOOLTIP "Save opened files && restore them at startup" NO-UNDO.

DEFINE VARIABLE gltooltipOnOpenParenthese AS LOGICAL INITIAL yes
     LABEL "API Tooltips on open parenthese or on selection"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 49 BY .81 TOOLTIP "See protools/abhack/openParentheseAPI.txt   A file to redefine tooltip help" NO-UNDO.

DEFINE VARIABLE glTopOnly AS LOGICAL INITIAL no
     LABEL "ABHack Window top-only"
     CONTEXT-HELP-ID 0
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 TOOLTIP "Toggle top-only attribute of this window" NO-UNDO.

DEFINE VARIABLE glUseSchemaCache AS LOGICAL INITIAL yes
     LABEL "Use DB Table Schema Cache"
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 TOOLTIP "Improves performances for remote databases - Uncheck and Recheck to refresh" NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE glPreprocessors AS LOGICAL INITIAL no
     LABEL "Completion for preprocessor names"
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 TOOLTIP "Note it makes the parsing of compiled listing slightly slower" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY bedtBrowse FOR
      bttEdt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE bedtBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS bedtBrowse C-Win _FREEFORM
  QUERY bedtBrowse DISPLAY
      bttEdt.cFileName bttEdt.cLastSaveOpenTime bttEdt.iNumLines
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 40 BY 22.86
         FONT 4 FIT-LAST-COLUMN TOOLTIP "Bold => source modified  Red => Same Source open earlier in other editor".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     cmon AT ROW 1 COL 1 NO-LABEL
     gcSectionOutlineMode AT ROW 1 COL 42 NO-LABEL
     btnOpenReleaseNotes AT ROW 1 COL 82
     btnHide AT ROW 3.86 COL 1
     btnOpenPeg AT ROW 1 COL 112
     btnOpenPSDN AT ROW 2.19 COL 82
     btnOpenReadmePdf AT ROW 2.19 COL 124
     glSectionLayoutIgnoreStructProcs AT ROW 3.86 COL 45
     BtnCompletion AT ROW 3.86 COL 82
     BtnKeys AT ROW 3.86 COL 105
     btnMisc AT ROW 3.86 COL 123
     glAbhackFind AT ROW 4.81 COL 45
     bedtBrowse AT ROW 5.29 COL 1
     btnLaunchInfoFloatingWin AT ROW 3.86 COL 26
     btnExpand AT ROW 3.86 COL 36
     btnloadGlobalResources AT ROW 3.86 COL 13
     btnStopStart AT ROW 3.86 COL 7
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 171.2 BY 27.19
         FONT 4.

DEFINE FRAME fImagePage
     imgPage AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 142.6 ROW 1
         SIZE 29 BY 6.43
         FONT 4.

DEFINE FRAME fCompletion
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 41.6 ROW 6.05
         SIZE 130 BY 22.1
         FONT 4.

DEFINE FRAME fComplTable
     glCompleteTableOnFLA AT ROW 1 COL 2
     glAddWhereAfterBuffer AT ROW 1.71 COL 2
     glAddNoLock AT ROW 2.43 COL 2
     glConvertDbTablesToLC AT ROW 3.14 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 88 ROW 2.43
         SIZE 43 BY 4.29
         FONT 4
         TITLE "7) DB Table and TT buffers".

DEFINE FRAME fComplEnable
     glCompleteOnTab AT ROW 1 COL 2
     giTabNumSpaces AT ROW 1.71 COL 37 COLON-ALIGNED
     glAutoComplete AT ROW 2.91 COL 2
     giAutoCompMinSize AT ROW 3.62 COL 36 COLON-ALIGNED
     glHypeStrikeMode AT ROW 4.76 COL 2
     glCompleteWithSimilar AT ROW 5.76 COL 2
     glNoCompletionInStrings AT ROW 6.48 COL 2
     glNoCompletionInComment AT ROW 7.19 COL 2
     glLowerCaseCompletion AT ROW 8.14 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 42 BY 9.05
         FONT 4
         TITLE "0) Enable/Disable completion".

DEFINE FRAME fComplRun
     glRunInLibComp AT ROW 1 COL 2
     glIntProcCompletion AT ROW 2.19 COL 2
     glIntProcInputInSig AT ROW 2.91 COL 5
     giSplitParamInCalls AT ROW 3.62 COL 5.8
     gcSplitParamCommaStyle AT ROW 4.81 COL 4 NO-LABEL
     glExtProcCompletion AT ROW 5.76 COL 3
     glFindOSFilesWithAbhackDB AT ROW 6.71 COL 6
     giMaxEtimeToLoadFiles AT ROW 7.67 COL 32 COLON-ALIGNED
     giMaxFilesToLoad AT ROW 8.86 COL 32 COLON-ALIGNED
     gcExtProcIgnoreDir AT ROW 10.05 COL 13 COLON-ALIGNED
     RECT-5 AT ROW 6.14 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 10.29
         SIZE 42 BY 11.67
         FONT 4
         TITLE "1) Completion after RUN".

DEFINE FRAME fComplVarUDF
     glVariableCompletion AT ROW 1.24 COL 2
     glIntFuncCompletion AT ROW 2.19 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 44 ROW 19.57
         SIZE 43 BY 3.33
         FONT 4
         TITLE "6) Variables UDF's and Methods".

DEFINE FRAME fComplSuggestUsedBuffers
     btnSuggesBufferSelectAll-2 AT ROW 3.14 COL 22
     gcSuggestBufferFor AT ROW 1 COL 1 NO-LABEL
     btnSuggesBufferSelectAll AT ROW 1 COL 22
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 44 ROW 6.24
         SIZE 43 BY 5.24
         FONT 4
         TITLE "3) Suggest used buffers after".

DEFINE FRAME fComplBlockLabel
     glCompleteBlockLabels AT ROW 1.24 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 44 ROW 11.71
         SIZE 43 BY 2.14
         FONT 4
         TITLE "4) Block label after next leave...".

DEFINE FRAME fComplColon
     glCompleteOOSuperMethod AT ROW 1 COL 2
     glCompleteAttrOnTab AT ROW 1.71 COL 2
     btnOpenAttrCompletionFile AT ROW 2.43 COL 5
     glFilterOnViewAsWidgetType AT ROW 3.38 COL 5
     glFilterGuessObjectType AT ROW 4.1 COL 5
     btnOpenAttrGuessObjTypeFile AT ROW 4.81 COL 8
     glAddEndAfterDo AT ROW 5.76 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 88 ROW 6.95
         SIZE 43 BY 7.14
         FONT 4
         TITLE "Completion after ':'  (colon)".

DEFINE FRAME fComplDot
     glCompleteField AT ROW 1 COL 2
     gcFieldSortedBy AT ROW 1.95 COL 5 NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 88 ROW 14.33
         SIZE 43 BY 3.33
         FONT 4
         TITLE "Completion on '.' (dot)".

DEFINE FRAME fComplClosingGuy
     glInsertClosingGuys AT ROW 1 COL 3
     glEnhancedCloseParenthese AT ROW 1.95 COL 7
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 88 ROW 17.91
         SIZE 43 BY 3.33
         FONT 4
         TITLE "Insert closing guy".

DEFINE FRAME fComplClasses
     glClassNewCastAfterEqual AT ROW 1.24 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 44 ROW 1
         SIZE 43 BY 2.38
         FONT 4
         TITLE "2) Completion for Classes".

DEFINE FRAME FRAME-A
     glPreprocessors AT ROW 1.24 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 44 ROW 3.62
         SIZE 43 BY 2.38
         FONT 4
         TITLE "2.5) Completion for Preprocessors".

DEFINE FRAME fComplCustom
     glCustomCompletionOnTab AT ROW 1 COL 4
     btnOpenCustomCompletionFile AT ROW 1.95 COL 2
     btnOpenCustomCompletionFileOfMin AT ROW 2.91 COL 2
     gcComplDateFormat AT ROW 3.86 COL 2.2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 44 ROW 14.1
         SIZE 43 BY 5.24
         FONT 4
         TITLE "5) Custom Completion".

DEFINE FRAME fKeys
     glFindNextSelected AT ROW 2.19 COL 73
     glDuplicateLineOnAltDown AT ROW 3.62 COL 73
     glCtrlBackspacelDelPrevWord AT ROW 4.33 COL 73
     glCtrlDelDelsWord AT ROW 5.05 COL 73
     glImprovedCtrlKeftRight AT ROW 5.76 COL 73
     glcommentOnDblSlash AT ROW 8.38 COL 3
     gcActionOnCtrlS AT ROW 8.38 COL 75 NO-LABEL
     glcommentSelectionOnSlash AT ROW 9.1 COL 3
     glMulitpleClipboardDummyToggle AT ROW 9.81 COL 3
     glCompile AT ROW 10.05 COL 73
     glCompileMinsize AT ROW 10.76 COL 76
     glAdaptWindowHeightOnCtrlW AT ROW 11.48 COL 3
     glListingOnCtrlAltT AT ROW 11.67 COL 73
     glMaximizeRestore AT ROW 12.19 COL 3
     glDebugListingOnCtrlAltD AT ROW 12.43 COL 73
     glCtrlF3NewProcWin AT ROW 12.91 COL 3
     glStopStartTimerOnCtrlAltO AT ROW 13.14 COL 73
     glOpenPW AT ROW 13.62 COL 3
     glHideAllComments AT ROW 14.1 COL 73
     glXrefAnal AT ROW 14.33 COL 3
     glHideSelection AT ROW 14.81 COL 73
     glCopyUIBelowMouse AT ROW 15.05 COL 3
     glShowLineNumbers AT ROW 15.52 COL 73
     glShowSpecialChars AT ROW 16.24 COL 73
     " Action On Ctrl-S:" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 7.67 COL 77
     RECT-8 AT ROW 7.91 COL 72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 43 ROW 8.43
         SIZE 125 BY 16.43
         FONT 4.

DEFINE FRAME fAlign
     glAlignEnabled AT ROW 1 COL 2
     glAlignVars AT ROW 1.67 COL 6
     glAlignParams AT ROW 2.43 COL 6
     glAlignTTFields AT ROW 3.38 COL 6
     glAlignTTFieldsSort AT ROW 4.1 COL 9
     glAlignAssign AT ROW 5.05 COL 6
     "} do not mix the selection" VIEW-AS TEXT
          SIZE 25 BY 1.19 AT ROW 1.91 COL 30
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 3 ROW 1.95
         SIZE 55 BY 6.19
         FONT 4
         TITLE "Alignement on Ctrl-Alt-L".

DEFINE FRAME fMisc
     btnOpenHowToShowMatchingParent AT ROW 1.24 COL 3
     glProposeOpenSelectedFile AT ROW 2.43 COL 4
     gcFancyInABWin AT ROW 2.76 COL 99 COLON-ALIGNED
     glDisplaytextSelectedInfo AT ROW 3.14 COL 4
     glUseSchemaCache AT ROW 3.62 COL 62
     glPlusplus AT ROW 3.86 COL 4
     glKeepTrackListingBuffer AT ROW 4.33 COL 62
     glDrawBoxAroundCurrentLine AT ROW 4.57 COL 4
     glMoveNewWindowsAway AT ROW 5.05 COL 62
     giRightMarginIndicatorCol AT ROW 5.29 COL 35 COLON-ALIGNED
     giMaxEmptyLinesBeforeShrinking AT ROW 5.76 COL 111 COLON-ALIGNED
     glResizableDictionary AT ROW 6.48 COL 4
     glTopOnly AT ROW 7.19 COL 4
     glAutoStopWhenNonEditorWin AT ROW 7.19 COL 62
     gcGlobalResCat AT ROW 8.38 COL 87 COLON-ALIGNED
     glKeepTrackOfCursor AT ROW 8.95 COL 4
     glJumpMruSection AT ROW 9.67 COL 4
     gcDumpedResourceFileRoot AT ROW 9.81 COL 81 COLON-ALIGNED
     giNameComboFont AT ROW 11.1 COL 20 NO-LABEL
     btnOpenAbhackDbReadme AT ROW 11.48 COL 83
     gltooltipOnOpenParenthese AT ROW 12.43 COL 5
     gcCatDbConnection AT ROW 12.91 COL 81 COLON-ALIGNED
     BtnReenableAllAPITooltips AT ROW 13.14 COL 12
     btnTestAbhackDb AT ROW 14.1 COL 65
     btnOpenAPITooltipFile AT ROW 14.81 COL 12
     BtnBulkDumpAbhackGlobDesc AT ROW 15.76 COL 62
     glAnalyseQuery AT ROW 16.71 COL 5
     giQueryTooltipTimeSuccess AT ROW 17.57 COL 13 NO-LABEL
     giQueryTooltipTimeFail AT ROW 18.76 COL 13 NO-LABEL
     glAutoStopDialog AT ROW 19.14 COL 66
     glQADispInABHWin AT ROW 19.95 COL 7
     glForceWaitFor AT ROW 20.05 COL 66
     glSaveSession AT ROW 21.48 COL 4 WIDGET-ID 2
     giQueryTooltipTimeSuccessTxt AT ROW 17.81 COL 5 COLON-ALIGNED NO-LABEL
     giQueryTooltipTimeFailTxt AT ROW 19.05 COL 5 COLON-ALIGNED NO-LABEL
     "Combo fonts:" VIEW-AS TEXT
          SIZE 13 BY .86 AT ROW 11.1 COL 7
     " Old Progress versions (9.1D 9.1E<02) stability tweaks :" VIEW-AS TEXT
          SIZE 53 BY .62 TOOLTIP "For old versions like 9.1Dxx, 9.1E and 9.1E01 (not needed since 9.1E02)" AT ROW 17.67 COL 62
     " Section Editor Options:" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 8.14 COL 12
     "=> Use -filterocxevents instead of these options" VIEW-AS TEXT
          SIZE 47 BY .71 AT ROW 18.38 COL 64
          FGCOLOR 12
     RECT-3 AT ROW 17.91 COL 59
     RECT-6 AT ROW 17.1 COL 3
     RECT-7 AT ROW 12.81 COL 3
     RECT-9 AT ROW 8.86 COL 59
     RECT-10 AT ROW 8.48 COL 3
     RECT-11 AT ROW 11.24 COL 59
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 48.2 ROW 5.71
         SIZE 120 BY 22.19
         FONT 4.


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
         TITLE              = "ABHack is your friend"
         COLUMN             = 51.4
         ROW                = 5.71
         HEIGHT             = 27.19
         WIDTH              = 171.2
         MAX-HEIGHT         = 40.19
         MAX-WIDTH          = 288
         VIRTUAL-HEIGHT     = 40.19
         VIRTUAL-WIDTH      = 288
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = no
         CONTROL-BOX        = yes
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         DROP-TARGET        = yes
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("protools/abhack/tux.ico":U) THEN
    MESSAGE "Unable to load icon: protools/abhack/tux.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fAlign:FRAME = FRAME fKeys:HANDLE
       FRAME fComplBlockLabel:FRAME = FRAME fCompletion:HANDLE
       FRAME fComplClasses:FRAME = FRAME fCompletion:HANDLE
       FRAME fComplClosingGuy:FRAME = FRAME fCompletion:HANDLE
       FRAME fComplColon:FRAME = FRAME fCompletion:HANDLE
       FRAME fComplCustom:FRAME = FRAME fCompletion:HANDLE
       FRAME fComplDot:FRAME = FRAME fCompletion:HANDLE
       FRAME fComplEnable:FRAME = FRAME fCompletion:HANDLE
       FRAME fCompletion:FRAME = FRAME fMain:HANDLE
       FRAME fComplRun:FRAME = FRAME fCompletion:HANDLE
       FRAME fComplSuggestUsedBuffers:FRAME = FRAME fCompletion:HANDLE
       FRAME fComplTable:FRAME = FRAME fCompletion:HANDLE
       FRAME fComplVarUDF:FRAME = FRAME fCompletion:HANDLE
       FRAME fImagePage:FRAME = FRAME fMain:HANDLE
       FRAME fKeys:FRAME = FRAME fMain:HANDLE
       FRAME fMisc:FRAME = FRAME fMain:HANDLE
       FRAME FRAME-A:FRAME = FRAME fCompletion:HANDLE.

/* SETTINGS FOR FRAME fAlign
                                                                        */
/* SETTINGS FOR TOGGLE-BOX glAlignParams IN FRAME fAlign
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fComplBlockLabel
                                                                        */
/* SETTINGS FOR FRAME fComplClasses
                                                                        */
/* SETTINGS FOR FRAME fComplClosingGuy
                                                                        */
/* SETTINGS FOR FRAME fComplColon
                                                                        */
/* SETTINGS FOR FRAME fComplCustom
                                                                        */
/* SETTINGS FOR COMBO-BOX gcComplDateFormat IN FRAME fComplCustom
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME fComplDot
                                                                        */
/* SETTINGS FOR FRAME fComplEnable
                                                                        */
/* SETTINGS FOR FRAME fCompletion
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME fComplClosingGuy:MOVE-BEFORE-TAB-ITEM (FRAME fComplVarUDF:HANDLE)
       XXTABVALXX = FRAME fComplDot:MOVE-BEFORE-TAB-ITEM (FRAME fComplClosingGuy:HANDLE)
       XXTABVALXX = FRAME fComplCustom:MOVE-BEFORE-TAB-ITEM (FRAME fComplDot:HANDLE)
       XXTABVALXX = FRAME fComplBlockLabel:MOVE-BEFORE-TAB-ITEM (FRAME fComplCustom:HANDLE)
       XXTABVALXX = FRAME fComplRun:MOVE-BEFORE-TAB-ITEM (FRAME fComplBlockLabel:HANDLE)
       XXTABVALXX = FRAME fComplColon:MOVE-BEFORE-TAB-ITEM (FRAME fComplRun:HANDLE)
       XXTABVALXX = FRAME fComplSuggestUsedBuffers:MOVE-BEFORE-TAB-ITEM (FRAME fComplColon:HANDLE)
       XXTABVALXX = FRAME FRAME-A:MOVE-BEFORE-TAB-ITEM (FRAME fComplSuggestUsedBuffers:HANDLE)
       XXTABVALXX = FRAME fComplTable:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-A:HANDLE)
       XXTABVALXX = FRAME fComplClasses:MOVE-BEFORE-TAB-ITEM (FRAME fComplTable:HANDLE)
       XXTABVALXX = FRAME fComplEnable:MOVE-BEFORE-TAB-ITEM (FRAME fComplClasses:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME fComplRun
                                                                        */
/* SETTINGS FOR FILL-IN giSplitParamInCalls IN FRAME fComplRun
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME fComplSuggestUsedBuffers
                                                                        */
/* SETTINGS FOR FRAME fComplTable
                                                                        */
/* SETTINGS FOR FRAME fComplVarUDF
                                                                        */
/* SETTINGS FOR FRAME fImagePage
                                                                        */
/* SETTINGS FOR FRAME fKeys
                                                                        */
/* SETTINGS FOR TOGGLE-BOX glMaximizeRestore IN FRAME fKeys
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX glMulitpleClipboardDummyToggle IN FRAME fKeys
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* BROWSE-TAB bedtBrowse glAbhackFind fMain */
ASSIGN
       bedtBrowse:COLUMN-RESIZABLE IN FRAME fMain       = TRUE.

/* SETTINGS FOR EDITOR cmon IN FRAME fMain
   NO-DISPLAY                                                           */
ASSIGN
       cmon:RETURN-INSERTED IN FRAME fMain  = TRUE.

/* SETTINGS FOR FRAME fMisc
                                                                        */
/* SETTINGS FOR FILL-IN gcCatDbConnection IN FRAME fMisc
   SHARED                                                               */
/* SETTINGS FOR FILL-IN gcDumpedResourceFileRoot IN FRAME fMisc
   SHARED                                                               */
/* SETTINGS FOR COMBO-BOX gcGlobalResCat IN FRAME fMisc
   SHARED                                                               */
/* SETTINGS FOR FILL-IN giQueryTooltipTimeFailTxt IN FRAME fMisc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN giQueryTooltipTimeSuccessTxt IN FRAME fMisc
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX glKeepTrackListingBuffer IN FRAME fMisc
   SHARED                                                               */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* SETTINGS FOR TOGGLE-BOX glPreprocessors IN FRAME FRAME-A
   SHARED                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE bedtBrowse
/* Query rebuild information for BROWSE bedtBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH bttEdt BY bttEdt.dLastSaveOpenTime DESCENDING.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE bedtBrowse */
&ANALYZE-RESUME




/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlDelay ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1
       COLUMN          = 78
       HEIGHT          = 1.43
       WIDTH           = 6
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlSpy ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 2.43
       COLUMN          = 78
       HEIGHT          = 1.43
       WIDTH           = 6
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlEdtValueChanged ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 3.86
       COLUMN          = 78
       HEIGHT          = 1.43
       WIDTH           = 6
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlDelay OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimerApplyEvent */
/* CtrlSpy OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimerSpy */
/* CtrlEdtValueChanged OCXINFO:CREATE-CONTROL from: type: type: PSTimerValueChanged */
      CtrlDelay:MOVE-AFTER(gcSectionOutlineMode:HANDLE IN FRAME fMain).
      CtrlSpy:MOVE-AFTER(btnOpenReadmePdf:HANDLE IN FRAME fMain).
      CtrlEdtValueChanged:MOVE-AFTER(glSectionLayoutIgnoreStructProcs:HANDLE IN FRAME fMain).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON DROP-FILE-NOTIFY OF C-Win /* ABHack is your friend */
DO:
    /* 14-MAY-2007 sla: suggestion of Jan Keirse */

    DEFINE VARIABLE iCount AS INTEGER     NO-UNDO.
    REPEAT iCount = 1 TO SELF:NUM-DROPPED-FILES:
        FILE-INFO:FILE-NAME = SELF:GET-DROPPED-FILE(iCount).
        IF FILE-INFO:FULL-PATHNAME <> ? THEN RUN openFile (FILE-INFO:FULL-PATHNAME).
    END.
    SELF:END-FILE-DROP().

    /* 26-OCT-2007 sla: new behavior, when you drop files on ABHack window, then register them right
      now, plus move them away if needed */
    RUN registerAllEditorWindows.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ABHack is your friend */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ABHack is your friend */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN rememberOpenedFiles. /* 23-OCT-2013 jcc: save closing state */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* ABHack is your friend */
DO:
/* cancel resize action */
IF CAN-DO(">>,<<<", btnExpand:LABEL IN FRAME {&FRAME-NAME})THEN DO:
    APPLY 'CHOOSE' TO btnExpand.  /* restore the ... */
    APPLY 'RIGHT-MOUSE-CLICK' TO btnExpand.  /* ... previous state */
    RETURN.
END.

SELF:HEIGHT-PIXELS = MAXIMUM(SELF:HEIGHT-PIXELS, bedtBrowse:Y + 7 * bedtBrowse:ROW-HEIGHT-PIXELS). /* don't allow less than the min size' */
SELF:HEIGHT-PIXELS = MINIMUM(SELF:HEIGHT-PIXELS, giWinHeightPixelsMax - 12). /* don't ask me why this constant */

bedtBrowse:HEIGHT-PIXELS IN FRAME {&FRAME-NAME} = SELF:HEIGHT-PIXELS - bedtBrowse:Y NO-ERROR.

/* make sure we do not change the width of the window */
SELF:WIDTH-PIXELS = giWinWidth. /* set in expandWindow */
giWinHeight = SELF:HEIGHT-PIXELS. /* keep track of it */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME bedtBrowse
&Scoped-define SELF-NAME bedtBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bedtBrowse C-Win
ON ROW-DISPLAY OF bedtBrowse IN FRAME fMain
DO:
  DEFINE BUFFER ttModified FOR ttModified.
  DEFINE BUFFER otherttEdt FOR ttEdt.

  IF NOT VALID-HANDLE(bttEdt.hEditor) THEN RETURN.

  IF bttEdt.hEditor:MODIFIED THEN DO:
      FIND FIRST ttModified WHERE ttModified.hEditor = bttEdt.hEditor NO-ERROR.
      IF NOT AVAILABLE ttModified THEN DO:
          CREATE ttModified.
          ttModified.hEditor = bttEdt.hEditor.
      END.
      bttEdt.cFileName:FONT IN BROWSE {&BROWSE-NAME} = 6.
  END.
  ELSE DO:
      FIND FIRST ttModified WHERE ttModified.hEditor = bttEdt.hEditor NO-ERROR.
      IF AVAILABLE ttModified THEN DELETE ttModified.
      bttEdt.cFileName:FONT IN BROWSE {&BROWSE-NAME} = ?.
  END.

  IF   NOT bttEdt.cFileName BEGINS "untitled"
   AND CAN-FIND(FIRST otherttEdt WHERE
                    otherttEdt.cFullPathName = bttEdt.cFullPathName
                AND otherttEdt.dLastSaveOpenTime < bttEdt.dLastSaveOpenTime)
   THEN
    bttEdt.cFileName:FGCOLOR = 12.
  ELSE bttEdt.cFileName:FGCOLOR = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bedtBrowse C-Win
ON VALUE-CHANGED OF bedtBrowse IN FRAME fMain
DO:
/* 20-AUG-2007 sla: move the code of DEFAULT-ACTION to here because this
 event is much more convenient than a double click */

DEFINE VARIABLE hEditorWin AS HANDLE      NO-UNDO.

IF  NOT AVAILABLE bttEdt THEN RETURN. /* how could this happen */

IF NOT VALID-HANDLE(bttEdt.hEditor)
 OR NOT VALID-HANDLE(bttEdt.hWin)
 OR bttEdt.hEditor:TYPE <> "EDITOR"
 OR bttEdt.hWin:TYPE <> "WINDOW"
 OR NOT bttEdt.hWin:VISIBLE /* 24-OCT-2013 jcc: "closed" section editor */
 THEN DO:
    DELETE bttEdt.
    RUN bedtBrowseReopen.
    RETURN.
 END.

/* 10-SEP-2007 sla: this can cause weird issues with an EMPTY section editor window */
/* IF bttEdt.hWin:VISIBLE = NO THEN bttEdt.hWin:VISIBLE = YES. */

IF bttEdt.hEditor:WINDOW:WINDOW-STATE = WINDOW-MINIMIZED
 THEN bttEdt.hEditor:WINDOW:WINDOW-STATE = WINDOW-NORMAL.
hEditorWin = bttEdt.hEditor.
APPLY "ENTRY" TO hEditorWin.
APPLY "ENTRY" TO bttEdt.hEditor.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME BtnBulkDumpAbhackGlobDesc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnBulkDumpAbhackGlobDesc C-Win
ON CHOOSE OF BtnBulkDumpAbhackGlobDesc IN FRAME fMisc /* Bulk Dump Utility for ABHack Resource Descriptions */
DO:
    RUN protools/abhack/abhackBulkDumpWin.w PERSISTENT
      (THIS-PROCEDURE
      ,gcGlobalResCat
      ,gcDumpedResourceFileRoot
      ,gcImportDirName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BtnCompletion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCompletion C-Win
ON CHOOSE OF BtnCompletion IN FRAME fMain /* Completion */
DO:
  BtnCompletion:FONT = 6.
  BtnKeys:FONT = ?.
  BtnMisc:FONT = ?.

  FRAME fCompletion:VISIBLE = YES.
  FRAME fKeys:VISIBLE = NO.
  FRAME fMisc:VISIBLE = NO.
  imgPage:LOAD-IMAGE("protools\abhack\pics\abhackRules.jpg") IN FRAME fImagePage NO-ERROR.
  FRAME fImagePage:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExpand
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExpand C-Win
ON CHOOSE OF btnExpand IN FRAME fMain /* >> */
DO:
RUN expandWindow ("forward").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExpand C-Win
ON RIGHT-MOUSE-CLICK OF btnExpand IN FRAME fMain /* >> */
DO:
  RUN expandWindow ("backward").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHide
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHide C-Win
ON CHOOSE OF btnHide IN FRAME fMain /* Hide */
DO:
    RUN hideWindow IN TARGET-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnKeys
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnKeys C-Win
ON CHOOSE OF BtnKeys IN FRAME fMain /* Keys */
DO:
  BtnCompletion:FONT = ?.
  BtnKeys:FONT = 6.
  BtnMisc:FONT = ?.

  FRAME fCompletion:VISIBLE = NO.
  FRAME fKeys:VISIBLE = YES.
  FRAME fMisc:VISIBLE = NO.
  imgPage:LOAD-IMAGE("protools\abhack\pics\abHack.jpg") IN FRAME fImagePage NO-ERROR.
  FRAME fImagePage:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLaunchInfoFloatingWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLaunchInfoFloatingWin C-Win
ON CHOOSE OF btnLaunchInfoFloatingWin IN FRAME fMain /* Info Win */
DO:
    RUN protools/abhack/abhackinfowin.w PERSISTENT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnloadGlobalResources
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnloadGlobalResources C-Win
ON CHOOSE OF btnloadGlobalResources IN FRAME fMain /* Load Global */
DO:
    RUN loadGlobalResources IN TARGET-PROCEDURE (ghCurrentEditor).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMisc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMisc C-Win
ON CHOOSE OF btnMisc IN FRAME fMain /* Misc */
DO:
  BtnCompletion:FONT = ?.
  BtnKeys:FONT = ?.
  BtnMisc:FONT = 6.

  FRAME fCompletion:VISIBLE = NO.
  FRAME fKeys:VISIBLE = NO.
  FRAME fMisc:VISIBLE = YES.
  imgPage:LOAD-IMAGE("protools\abhack\pics\abhackIsSelfDocumented.jpg") IN FRAME fImagePage NO-ERROR.
  FRAME fImagePage:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME btnOpenAbhackDbReadme
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpenAbhackDbReadme C-Win
ON CHOOSE OF btnOpenAbhackDbReadme IN FRAME fMisc /* Open AbhackDbReadme.txt */
DO:
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

   cFileName = SEARCH("protools/abhack/AbhackDbReadme.txt").

   RUN adecomm/_pwmain.p ( "", cFileName , "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOpenAPITooltipFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpenAPITooltipFile C-Win
ON CHOOSE OF btnOpenAPITooltipFile IN FRAME fMisc /* Open customAPITooltip.txt */
DO:
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

   cFileName = SEARCH("protools/abhack/customAPITooltip.txt").

   RUN adecomm/_pwmain.p ( "", cFileName , "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplColon
&Scoped-define SELF-NAME btnOpenAttrCompletionFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpenAttrCompletionFile C-Win
ON CHOOSE OF btnOpenAttrCompletionFile IN FRAME fComplColon /* Open attrTabCompletion<X>.txt */
DO:
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cVersion  AS CHARACTER   NO-UNDO.

   cVersion = ENTRY(1, PROVERSION, ".").

   cFileName = SEARCH("protools/abhack/attrTabCompletion" + cVersion + ".txt").

   RUN adecomm/_pwmain.p ( "", cFileName , "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOpenAttrGuessObjTypeFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpenAttrGuessObjTypeFile C-Win
ON CHOOSE OF btnOpenAttrGuessObjTypeFile IN FRAME fComplColon /* Open attrGuessObjType<X>.txt */
DO:
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cVersion  AS CHARACTER   NO-UNDO.

   cVersion = ENTRY(1, PROVERSION, ".").

   cFileName = SEARCH("protools/abhack/attrGuessObjType" + cVersion + ".txt").

   RUN adecomm/_pwmain.p ( "", cFileName , "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplCustom
&Scoped-define SELF-NAME btnOpenCustomCompletionFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpenCustomCompletionFile C-Win
ON CHOOSE OF btnOpenCustomCompletionFile IN FRAME fComplCustom /* Open customTabCompletion.txt */
DO:
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

   cFileName = SEARCH("protools/abhack/customTabCompletion.txt").

   RUN adecomm/_pwmain.p ( "", cFileName , "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOpenCustomCompletionFileOfMin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpenCustomCompletionFileOfMin C-Win
ON CHOOSE OF btnOpenCustomCompletionFileOfMin IN FRAME fComplCustom /* Open customTabCompletion.ofMine.txt */
DO:
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

   cFileName = SEARCH("protools/abhack/customTabCompletion.ofMine.txt").

   RUN adecomm/_pwmain.p ( "", cFileName , "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME btnOpenHowToShowMatchingParent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpenHowToShowMatchingParent C-Win
ON CHOOSE OF btnOpenHowToShowMatchingParent IN FRAME fMisc /* Instructions to make the editor show you the matching '(' when typing ')' */
DO:
   DEFINE VARIABLE iDontCare AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

   cFileName = SEARCH("protools/abhack/HowToShowMatchingOpenParenthese.txt").

   RUN ShellExecuteA (0, "open", cFileName, "", "", 1 /* 0 hidden, 1 normal, minimized 2, maximized 3*/ , OUTPUT IDontCare).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME btnOpenPeg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpenPeg C-Win
ON CHOOSE OF btnOpenPeg IN FRAME fMain /* ABHack on the PEG */
DO:
   DEFINE VARIABLE iDontCare AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

   cFileName = "http://www.peg.com/lists/abhack/web/".

   RUN ShellExecuteA (0, "open", cFileName, "", "", 1 /* 0 hidden, 1 normal, minimized 2, maximized 3*/ , OUTPUT IDontCare).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOpenPSDN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpenPSDN C-Win
ON CHOOSE OF btnOpenPSDN IN FRAME fMain /* ABHack on the OpenEdge Hive */
DO:
   DEFINE VARIABLE iDontCare AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

   /* cFileName = "http://communities.progress.com/pcom/docs/DOC-18868". */
   cFileName = "http://www.oehive.org/abhack".

   RUN ShellExecuteA (0, "open", cFileName, "", "", 1 /* 0 hidden, 1 normal, minimized 2, maximized 3*/ , OUTPUT IDontCare).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOpenReadmePdf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpenReadmePdf C-Win
ON CHOOSE OF btnOpenReadmePdf IN FRAME fMain /* Readme.pdf */
DO:
   DEFINE VARIABLE iDontCare AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

   cFileName = SEARCH("protools/abhack/ABHackWin.readme.pdf").

   RUN ShellExecuteA (0, "open", cFileName, "", "", 1 /* 0 hidden, 1 normal, minimized 2, maximized 3*/ , OUTPUT IDontCare).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOpenReleaseNotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpenReleaseNotes C-Win
ON CHOOSE OF btnOpenReleaseNotes IN FRAME fMain /* Release Notes */
DO:
    DEFINE VARIABLE iDontCare AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

    cFileName = SEARCH("protools/abhack/ABHackWin.releaseNotes.txt").

    RUN ShellExecuteA (0, "open", cFileName, "", "", 1 /* 0 hidden, 1 normal, minimized 2, maximized 3*/ , OUTPUT IDontCare).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME BtnReenableAllAPITooltips
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnReenableAllAPITooltips C-Win
ON CHOOSE OF BtnReenableAllAPITooltips IN FRAME fMisc /*  => Re-enable all API Tooltips */
DO:

DEFINE VARIABLE cSection  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ckv       AS CHARACTER  NO-UNDO.

DEFINE BUFFER ttAPI FOR ttAPI.

LOAD 'Software\PSC\PROGRESS\slacroixTools' BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
/* create it if it does not exist  ... well, would be surprising...*/
IF ERROR-STATUS:ERROR THEN LOAD 'Software\PSC\PROGRESS\slacroixTools' NEW BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN USE 'Software\PSC\PROGRESS\slacroixTools'.

cSection = 'ABHacker'.

FOR EACH ttAPI:
    PUT-KEY-VALUE SECTION cSection KEY 'disabledApiTooltip' + ttAPI.cAPI VALUE ?.
END.

/* unload the environment to go back to default env */
UNLOAD 'Software\PSC\PROGRESS\slacroixTools' NO-ERROR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME btnStopStart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStopStart C-Win
ON CHOOSE OF btnStopStart IN FRAME fMain /* Stop */
DO:
    IF REPLACE(SELF:LABEL, "&", "") = "STOP" THEN DO:
        RUN stopABHackSpyingTimer IN TARGET-PROCEDURE.
        PUBLISH "killEdtPrintableTrigPersitProc". /* so it won't wake up  */
        PUBLISH "stopABHackTimers" FROM THIS-PROCEDURE. /* 05-OCT-2007 sla: we should use this event now */
    END.

    /* Start => publish restartABHackSpyingTimer to notify EdtPrintableTrigPersit Procedures */
    ELSE DO:
        PUBLISH "restartABHackSpyingTimer" FROM THIS-PROCEDURE.
        PUBLISH "restartABHackTimers" FROM THIS-PROCEDURE. /* 05-OCT-2007 sla: we should use this event now */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplSuggestUsedBuffers
&Scoped-define SELF-NAME btnSuggesBufferSelectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSuggesBufferSelectAll C-Win
ON CHOOSE OF btnSuggesBufferSelectAll IN FRAME fComplSuggestUsedBuffers /* <= Select All */
DO:
  gcSuggestBufferFor:SCREEN-VALUE = gcSuggestBufferFor:LIST-ITEMS.
  APPLY "VALUE-CHANGED" TO gcSuggestBufferFor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSuggesBufferSelectAll-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSuggesBufferSelectAll-2 C-Win
ON CHOOSE OF btnSuggesBufferSelectAll-2 IN FRAME fComplSuggestUsedBuffers /* Help */
DO:
  MESSAGE "Use the Control key to add or remove items to this selection list" SKIP
   "You will probably appreciate this feature"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME btnTestAbhackDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTestAbhackDb C-Win
ON CHOOSE OF btnTestAbhackDb IN FRAME fMisc /* Hit Return in the fill-in above of Press this to validate it */
DO:
    ASSIGN FRAME fMisc gcCatDbConnection.

    RUN connectToAbhackDb NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Successfully connected to " PDBNAME("abhack") SKIP
         " => You can select ABHackDatase in the"
         QUOTER(gcGlobalResCat:LABEL)
         "combo, and enjoy the full power of ABHack"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME CtrlDelay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlDelay C-Win OCX.Tick
PROCEDURE CtrlDelay.PSTimerApplyEvent.Tick .
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  None required for OCX.
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cLine              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iNumSpacesToInsert AS INTEGER    NO-UNDO.

/* one shot only */
chCtrlDelay:PSTimerApplyEvent:ENABLED = NO.

IF NOT VALID-HANDLE(ghDelayTriggerHandle) THEN RETURN.

CASE gcDelayTriggerWinEvent:
    WHEN "WINDOW-RESIZED" THEN APPLY "WINDOW-RESIZED" TO ghDelayTriggerHandle.
    WHEN "WINDOW-CLOSE"   THEN APPLY "WINDOW-CLOSE"   TO ghDelayTriggerHandle.
    WHEN "SOURCE-COMMAND('undo', '')" THEN ghDelayTriggerHandle:SOURCE-COMMAND('undo', '').
    WHEN "delete4SpacesAndApplyDot" THEN DO:
        ghDelayTriggerHandle:SOURCE-COMMAND('p4gl-maybe-case-backspace', '').
        ghDelayTriggerHandle:SOURCE-COMMAND('p4gl-maybe-case-backspace', '').
        ghDelayTriggerHandle:SOURCE-COMMAND('p4gl-maybe-case-backspace', '').
        ghDelayTriggerHandle:SOURCE-COMMAND('p4gl-maybe-case-backspace', '').
        /*RUN dotPressed IN THIS-PROCEDURE*/
        APPLY "." TO ghDelayTriggerHandle.
    END.
    WHEN "dot" THEN APPLY "." TO ghDelayTriggerHandle.
    WHEN "colon" THEN APPLY ":" TO ghDelayTriggerHandle.
    WHEN "dotPressedOnly" THEN RUN dotPressed (ghDelayTriggerHandle).
    WHEN "valueChanged" THEN RUN valueChanged (ghDelayTriggerHandle).
    WHEN "colonPressedOnly" THEN RUN colonPressed (ghDelayTriggerHandle).
    WHEN "SOURCE-COMMAND('DELETE-CHARACTER', '')" THEN ghDelayTriggerHandle:SOURCE-COMMAND('DELETE-CHARACTER', '').
    WHEN "MENU-DROP" THEN APPLY "MENU-DROP" TO ghDelayTriggerHandle.
    WHEN "deleteOneChar" THEN ghDelayTriggerHandle:SOURCE-COMMAND('p4gl-maybe-case-backspace', '').
    WHEN "CarriageReturn" THEN IF ghDelayTriggerHandle:TYPE = "EDITOR" THEN DO:
        RUN getEditorLine IN TARGET-PROCEDURE (ghDelayTriggerHandle, OUTPUT cLine).
        cMon:SCREEN-VALUE IN FRAME fMain = "Insert CR".
        iNumSpacesToInsert = LENGTH(cLine) - LENGTH(LEFT-TRIM(cLine)).
        ghDelayTriggerHandle:INSERT-STRING("~n" + FILL(' ', iNumSpacesToInsert)).
        ghDelayTriggerHandle:CURSOR-CHAR = iNumSpacesToInsert + 1.
    END.
END CASE.

/* 20-MAR-2007 sla: I do not like the idea to keep this guys with something too long */
ASSIGN
 ghDelayTriggerHandle   = ?
 gcDelayTriggerWinEvent = "" WHEN gcDelayTriggerWinEvent <> "colonPressedOnly". /* 16-NOV-2007 sla: special trick to let a colonPress fire when we pressed ':' ealier on a popup */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlEdtValueChanged
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlEdtValueChanged C-Win OCX.Tick
PROCEDURE CtrlEdtValueChanged.PSTimerValueChanged.Tick .
/*------------------------------------------------------------------------------
  Purpose:     fire a value changed event for a source-editor widget
          The timer is enabled by the fireValueChanged procedure event
          The idea is to first let the editor do is job with key stokes and do
          something afterwards
------------------------------------------------------------------------------*/

DEFINE VARIABLE lOneShotOnly AS LOGICAL     NO-UNDO. /* weird case, it seems that this event can fire even when the enabled property is unset!!! */

/* One single shot only */
lOneShotOnly = chCtrlEdtValueChanged:PSTimerValueChanged:enabled.
chCtrlEdtValueChanged:PSTimerValueChanged:enabled = NO.

IF lOneShotOnly
 AND VALID-HANDLE(ghEditorValueChanged) /* just in case... */
 THEN RUN valueChanged IN TARGET-PROCEDURE (ghEditorValueChanged).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlSpy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlSpy C-Win OCX.Tick
PROCEDURE CtrlSpy.PSTimerSpy.Tick .
/*Purpose: This code will first find out if the active window holds and editor
source code editor widget.  If yes, then it will keep track of it in a special
temp-table.  We have to spy the keyboard for a few special keys that cannot be
handled with simple 4GL triggers in a source code editor (like TAB and ESC)*/

DEFINE VARIABLE cActiveWindowTitle    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cClassFile            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cClassPath            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCurrentEvent         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCurrentSection       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCurrentWidget        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hAreWeInADialog       AS HANDLE      NO-UNDO.
DEFINE VARIABLE hEditor               AS HANDLE      NO-UNDO.
DEFINE VARIABLE hFrame                AS HANDLE      NO-UNDO.
DEFINE VARIABLE hListPopUpFrame       AS HANDLE      NO-UNDO.
DEFINE VARIABLE iCtrlKeyStateLowerBit AS INTEGER     NO-UNDO.
DEFINE VARIABLE iEscKeyStateLowerBit  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iGetKeyboardRtn       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMKeyStateLowerBit    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTabKeyStateLowerBit  AS INTEGER     NO-UNDO.
DEFINE VARIABLE lAltPressed           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lCtrlPressed          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lEscPressed           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lMPressed             AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lShiftPressed         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lTabPressed           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lZPressed             AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mKBState              AS MEMPTR      NO-UNDO.

DEFINE VARIABLE hACTIVE-WINDOW        AS HANDLE      NO-UNDO .

ASSIGN hACTIVE-WINDOW = ActiveWindow:GetHandle() .

/*-----------------------------------------------------------------------------------------
 14-AUG-2007 sla: NASTY ISSUE THAT HAS ALREADY BEEN REPORTED IN THE PAST ON pegabhack
 but I understand the real root cause only now
 If this trigger PROCEDURE fires while a message is displayed at the screen, the ERROR-STATUS:ERROR flag
 might be changed by this procedure, which can change the flow of the application being tested
 from the AppBuilder.

 Code to illustrate the problem:
        RUN myProc NO-ERROR.

        MESSAGE 1 ERROR-STATUS:ERROR SKIP RETURN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        /* => the CtrlSpy.PSTimerSpy.Tick might fire during this message */

        /* ERROR-STATUS:ERROR or/and RETURN-VALUE have changed!!! */
        MESSAGE 2 ERROR-STATUS:ERROR SKIP RETURN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        PRO CEDURE myProc:
            RETURN ERROR "some value".
        END PROCEDURE.

  Solution: store ERROR-STATUS:ERROR and restore it as it is before returning from this procedure
     => this works fine for ERROR-STATUS:ERROR

  Tried to apply the same for the RETURN-VALUE function (with a new setReturnValue procedure)
  without success...  Indeed, it seems that the RETURN-VALUE is always reset when we leave a trigger procedure  :((
   Note I tried many tricks:
      a) use a LEAVE CtrlSpyTick (main block labek) instead of RETURN
      b) RETURN cReturnValueBeforeTick
      c) RUN setReturnValue (cReturnValueBeforeTick).  then LEAVE CtrlSpyTick
     I could track the value of RETURN-VALUE by displaying it in the abhack window:TITLE at the end of this
     trigger procedure  => it was reset correctly, but the 2nd message after this trigger proc shows that
     RETURN-VALUE is always reset when we leave a trigger procedure...   It looks like a limitation
        => so no way to work it around
        => I will keep the version that is most likely version to work in the future with
        the RUN setReturnValue  and leave CtrlSpyTick.

    Note also this shows the limitation of using the RETURN-VALUE function for serious error handling
-----------------------------------------------------------------------------------------*/

DEFINE VARIABLE cReturnValueBeforeTick     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lErrorStatusFlagBeforeTick AS LOGICAL     NO-UNDO.
ASSIGN
 lErrorStatusFlagBeforeTick = ERROR-STATUS:ERROR
 cReturnValueBeforeTick     = RETURN-VALUE.

DEFINE BUFFER ttEdt     FOR ttEdt. /*private buffer please*/
DEFINE BUFFER ttAPI     FOR ttAPI.
DEFINE BUFFER ttsection FOR ttsection.
DEFINE BUFFER ttUsing   FOR ttUsing.

/*10-JAN-2007 sla: fancy flashing text or image in AppBuilder window*/
IF VALID-HANDLE(ghABHackedText)
 AND DECIMAL(ETIME) > gdABHackedTextColorLastEtime + 500 THEN DO:
  ASSIGN
   gdABHackedTextColorLastEtime = DECIMAL(ETIME)
   giABHackedTextColorSeq = (giABHackedTextColorSeq + 1) MODULO {&giABHackedTextColorsExt}.
  IF giABHackedTextColorSeq = 0 THEN giABHackedTextColorSeq = {&giABHackedTextColorsExt}.
  IF ghABHackedText:TYPE = "TEXT" THEN DO:
    /*little optimization for Linux wine (touch the UI attributes as less as possible, especially in such a process)*/
    IF giABHackedTextFgColor  <> giABHackedTextColors[ giABHackedTextColorSeq ]
     THEN ASSIGN
      giABHackedTextFgColor  = giABHackedTextColors[ giABHackedTextColorSeq ]
      ghABHackedText:FGCOLOR = giABHackedTextFgColor.
  END.
  IF ghABHackedText:TYPE = "IMAGE" AND gcABHackedPicture <> gcABHackedPictures [ giABHackedTextColorSeq ]
   THEN DO:
    IF gcFancyInABWin = "smileyGun" THEN
       gcABHackedPicture = gcABHackedPicRootPath + "protools/abhack/pics/smileyGun" + gcABHackedPictures[ giABHackedTextColorSeq ] + ".bmp".
      ELSE gcABHackedPicture = gcABHackedPicRootPath + "protools/abhack/pics/french" + STRING(giABHackedTextColorSeq) + ".bmp".
      ghABHackedText:LOAD-IMAGE(gcABHackedPicture).
  END.
END.

/*little optimization when the active window has not changed*/
/*IF ghLastActiveWinNoEditor = hACTIVE-WINDOW THEN RETURN.*/

cActiveWindowTitle = hACTIVE-WINDOW:TITLE NO-ERROR.  /* 24-DEC-2008 sla: added to solve possible error 3140 with .NET forms */

/*Make sure this tick trigger is running only once at a given time (something called this could piece of code could well do a PROCESS EVENT or something alike )*/

chCtrlSpy:PSTimerSpy:ENABLED = NO.
/* 14-AUG-2007 sla: I now reset the ERROR-STATUS:ERROR and RETURN-VALUE before returning
    => by the way, use a LEAVE statement instead of RETURN in order to keep the restored RETURN-VALUE */
&SCOPED-DEFINE RETURN DO:~
    chCtrlSpy:PSTimerSpy:ENABLED = YES. ~
    hEditor = ? NO-ERROR. ~
    ERROR-STATUS:ERROR = lErrorStatusFlagBeforeTick. ~
    IF cReturnValueBeforeTick <> "" THEN RUN setReturnValue (cReturnValueBeforeTick). ~
    LEAVE CtrlSpyTick. /* RETURN cReturnValueBeforeTick. */ ~
END. /* preprocessed do block for RETURN */



/* 15-AUG-2007 sla: give a name to this pseudo main block => trick to use LEAVE instead of RETURN */
CtrlSpyTick:
DO WITH FRAME fMain
/*23-NOV-2006 sla:  This it ERROR and STOP handling options seem to catch the STOP condition that occurs
 at the end of RUN action of the AppBuilder
 Lets trace it and renable the timer*/
 ON ERROR UNDO, RETRY
 ON STOP  UNDO, RETRY:
    IF RETRY THEN DO:
        cMon:SCREEN-VALUE = 'STOP caught while being in PSTimer.OCX.Tick, reanabling PSTimerSpy'.
        {&RETURN}
    END.

/*code optimized to detect if we are in an source editor with lowest CPU usage
  assuming a source-code editor is loacated in first level frames (not in another child frame)*/
FIND ttEdt WHERE ttEdt.hWin = hACTIVE-WINDOW NO-ERROR.
/*Sanity cleanup*/
IF AVAILABLE ttEdt THEN DO:
    hEditor = ttEdt.hEditor.
    IF VALID-HANDLE(ttEdt.hEditor) = NO
     OR ttEdt.hEditor:TYPE <> "EDITOR"
     OR ttEdt.hEditor:SOURCE-EDITOR = NO
     OR ttEdt.cWinTitle <> ttEdt.hWin:TITLE /* 11-AUG-2007 sla: find out another code has been loaded in a window */
     THEN DO:
        RUN clearGlobalResources IN ghParser (ttEdt.hEditor).
        FOR EACH ttsection WHERE ttsection.hEditor = ttEdt.hEditor:
            DELETE ttsection.
        END.
        DELETE ttEdt.
        ghCurrentEditor = ?.
        {&RETURN}
    END.
END.

IF NOT AVAILABLE ttEdt THEN DO:
    IF glResizableDictionary THEN DO:
        IF VALID-HANDLE(hwDict) AND hwDict:TITLE BEGINS "Data Dictionary " THEN DO:
            /*05-DEC-2006 sla: get rid off scrollbar that may come when choosing index/field or buttons*/
            DEFINE VARIABLE iDontCare AS INTEGER    NO-UNDO.
            IF VALID-HANDLE(hDictFrame) THEN RUN ShowScrollBar (hDictFrame:HWND, 3, 0, OUTPUT iDontCare).
            {&RETURN} /*nothing else to do, we know this dictionary guy*/
        END.
        IF cActiveWindowTitle = "Data Dictionary" THEN DO:
            /*06-DEC-2006 sla: Error if not connected to any database => wait until we leave the dialog-box*/
            DEFINE VARIABLE hDictionaryDialog AS HANDLE NO-UNDO.
            hDictionaryDialog = FOCUS:FRAME NO-ERROR.
            IF VALID-HANDLE(hDictionaryDialog)
             AND hDictionaryDialog:TYPE = "DIALOG-BOX"
             THEN {&RETURN}
            hwDict = ACTIVE-WINDOW.
            cmon:SCREEN-VALUE = STRING(TIME, "hh:mm:ss") + " Data Dictionary Window found, make it resizable :)".
            RUN refineDictWidget.
            {&RETURN}
        END.
        IF hwDict <> ? THEN ASSIGN
         cmon:SCREEN-VALUE = STRING(TIME, "hh:mm:ss") + " We left the Data Dictionary"
         hwDict = ?.
    END.

    hFrame = hACTIVE-WINDOW:FIRST-CHILD NO-ERROR.
    hEditor = hFrame:FIRST-CHILD:LAST-CHILD NO-ERROR.
    DO WHILE TRUE:
        IF VALID-HANDLE(hEditor) AND hEditor:TYPE = "EDITOR" AND hEditor:SOURCE-EDITOR THEN LEAVE.
        hEditor = hEditor:PREV-SIBLING NO-ERROR. /*14-DEC-2006 sla: Added NO-ERROR for case of window without frame*/
        IF hEditor = ? THEN DO:
            hFrame = hFrame:NEXT-SIBLING NO-ERROR.
            IF hFrame = ? THEN LEAVE.
            hEditor = hFrame:FIRST-CHILD:LAST-CHILD NO-ERROR. /*14-DEC-2006 sla: case of window without frame*/
            IF hEditor = ? THEN LEAVE.
        END.
    END.
    /*not in a source-code editor window => leave*/
    IF VALID-HANDLE(hEditor) = NO
     OR hEditor:TYPE <> "EDITOR"
     OR hEditor:SOURCE-EDITOR = NO THEN DO:
        IF glAutoStopWhenNonEditorWin
         AND hACTIVE-WINDOW <> {&WINDOW-NAME} /*THIS-PROCEDURE*/
         AND NOT cActiveWindowTitle MATCHES "* - *~~~.p"  /*AppBuilder design window*/
         AND NOT cActiveWindowTitle MATCHES "* - *~~~.w"
         AND NOT cActiveWindowTitle MATCHES "* - *~~~.i"
         AND NOT CAN-DO("PRO*Tools,Palette,AppBuilder",cActiveWindowTitle)
         THEN DO:
            RUN waitForEditorsKeyStroke.
            btnStopStart:LABEL IN FRAME fMain = "&Start". /*so we can restart it manually*/
            cMon:SCREEN-VALUE IN FRAME fMain = "active window is not editor, stop spying until a printable key event occurs"
             + "~nINFO: A-W:PRIVATE-DATA = " + QUOTER(hACTIVE-WINDOW:PRIVATE-DATA).
            RETURN. /*leave the timer disabled*/
        END.

        /*we never save this var if glAutoStopWhenNonEditorWin*/
        ghLastActiveWinNoEditor = hACTIVE-WINDOW.
        {&RETURN}
    END.
    RUN keepTrackEdt (hEditor, BUFFER ttEdt).
END.

/*At this point, we know the focus is in a procedure editor window, it's time to spy the keyboard for a few keys*/

/*launch trigger persistent procedure for this editor*/
IF ghCurrentEditor <> hEditor THEN RUN trackEditor (BUFFER ttEdt).

/*Problem with an alert-box message that comes from find/replace dialog-box called from the editor window:
 an unstable situation can occur in 9.1E00 with errors 2910 or 4123 (in client logs)
 solution is to detect we are in such a dialog, then stop the timer until a printable
 key event occurs thanks to a little temporary persistent trigger in protools/abhack/ABHackSleepUntilPrintable.p*/
IF glAutoStopDialog THEN DO:
    hAreWeInADialog = FOCUS:FRAME NO-ERROR.
    IF   VALID-HANDLE(hAreWeInADialog)
     AND hAreWeInADialog:TYPE = "DIALOG-BOX"
     AND hAreWeInADialog:VISIBLE /*03-FEB-2007 sla: Added this condition, as it seems get here even when a Find dialog is not visible*/
    /*no needed AND hEditor:FRAME:TYPE <> "DIALOG-BOX" /*happens on compile preview dialog-box*/*/
    THEN DO:
        RUN waitForEditorsKeyStroke.
        cMon:SCREEN-VALUE IN FRAME fMain = "Focus in a DIALOG-BOX, stop spying until a printable key event occurs".
        RETURN. /*leave the timer disabled*/
    END.
END.

/*04-DEC-2006 sla: Well, if we are in a dialog, then return anyay*/
ELSE DO:
    hAreWeInADialog = FOCUS:FRAME NO-ERROR.
    IF VALID-HANDLE(hAreWeInADialog) AND hAreWeInADialog:TYPE = "DIALOG-BOX" AND hAreWeInADialog:VISIBLE
     THEN DO:
        cMon:SCREEN-VALUE IN FRAME fMain = STRING(TIME, "hh:mm:ss") + " Focus in a DIALOG-BOX (find/replace), return from spying until we leave it".
        /*14-DEC-2006 sla: tweak for old Progress version*/
        IF CAN-DO("Find,Replace", hAreWeInADialog:TITLE)
         AND SESSION:GET-WAIT-STATE() > "" THEN DO:
            cMon:SCREEN-VALUE = cMon:SCREEN-VALUE + " Find/Replace Dialog and GET-WAIT-STATE <> ''  => forcing WAIT-STATE to empty".
            SESSION:SET-WAIT-STATE("").
        END.
        {&RETURN}
    END.
END.

/*25-FEB-2007 sla: New feature to keep track of the cursor row and col in sections and possibly
 restore them when going back to a section we have visited before*/
IF   glKeepTrackOfCursor
 AND ttEdt.lManageableSections THEN DO:
    ASSIGN
     cCurrentSection = ttEdt.hSectionCombo:SCREEN-VALUE
     cCurrentEvent   = ttEdt.hEventCombo:SCREEN-VALUE
     cCurrentWidget  = ttEdt.hWidgetName:SCREEN-VALUE.

    IF  ttEdt.currentSection <> cCurrentSection
     OR     CAN-DO("Triggers,Procedures,Functions", cCurrentSection)
      AND ttEdt.currentEvent   <> cCurrentEvent
     OR     cCurrentSection = "Triggers"
      AND ttEdt.currentWidget  <> cCurrentWidget
     THEN DO:
        RUN restoreOrRecordSectionPostion (BUFFER ttEdt, cCurrentSection, cCurrentEvent, cCurrentWidget).
        {&RETURN}
    END.

    FIND ttsection WHERE
         ttsection.hEditor  = ttEdt.hEditor
     AND ttsection.cSection = cCurrentSection
     AND (   CAN-DO("Definitions,Main Block", cCurrentSection)
          OR ttsection.cEvent = cCurrentEvent)
     AND (   cCurrentSection <> "Triggers"
          OR ttsection.cWidget = cCurrentWidget). /*no error possible*/

    ASSIGN
     ttsection.cursorLine = ttEdt.hEditor:CURSOR-LINE
     ttsection.cursorChar = ttEdt.hEditor:CURSOR-CHAR.
END.


SET-SIZE(mKBState) = 256.
RUN GetKeyboardState(GET-POINTER-VALUE(mKBState), OUTPUT iGetKeyboardRtn).
lEscPressed   = GET-BYTE(mKBState, 28) > 127. /*27+1=28=Esc*/
lTabPressed   = GET-BYTE(mKBState, 10) > 127. /*9+ 1=10=TAB*/
lShiftPressed = GET-BYTE(mKBState, 17) > 127. /*16+1=17=Shift*/
lCtrlPressed  = GET-BYTE(mKBState, 18) > 127. /*17+1=18=Ctrl*/
lMPressed     = GET-BYTE(mKBState, 78) > 127. /*77+1=78=M*/
lZPressed     = GET-BYTE(mKBState, 91) > 127. /*90+1=91=Z*/
lAltPressed   = GET-BYTE(mKBState, 19) > 127. /*18+ 1=19=ALT*/
iEscKeyStateLowerBit  = GET-BYTE(mKBState, 28) MODULO 2.
iTabKeyStateLowerBit  = GET-BYTE(mKBState, 10) MODULO 2.
iCtrlKeyStateLowerBit = GET-BYTE(mKBState, 18) MODULO 2.
iMKeyStateLowerBit    = GET-BYTE(mKBState, 78) MODULO 2.
SET-SIZE(mKBState) = 0.

/*wait for the key to be released*/
IF lEscPressed AND giEscKeyStateLowerBit = iEscKeyStateLowerBit THEN {&RETURN}
IF lEscPressed THEN giEscKeyStateLowerBit = iEscKeyStateLowerBit. /*OK, it's just been released, flip flop it*/

IF lTabPressed AND giTabKeyStateLowerBit = iTabKeyStateLowerBit THEN {&RETURN}
IF lTabPressed THEN giTabKeyStateLowerBit = iTabKeyStateLowerBit.

IF lCtrlPressed AND lZPressed THEN RUN disableNextValueChanged (hEditor, "ctrlZ").
IF   lCtrlPressed
 AND giCtrlKeyStateLowerBit = iCtrlKeyStateLowerBit
 AND (   lMPressed AND giMKeyStateLowerBit = iMKeyStateLowerBit
      OR lZPressed)
 THEN {&RETURN}
IF lMPressed AND lCtrlPressed AND NOT lAltPressed THEN DO:
    giCtrlKeyStateLowerBit = iCtrlKeyStateLowerBit.
    giMKeyStateLowerBit    = iMKeyStateLowerBit.
    RUN ctrlM (hEditor).
END.

/*Is there a hListPopUpFrame running ?  Note that by design, there can be only a single one in a given source editor at a given time*/
hListPopUpFrame = hACTIVE-WINDOW:FIRST-CHILD.
DO WHILE hListPopUpFrame <> ?:
    IF hListPopUpFrame:NAME = "fEditorPopupList" THEN LEAVE.
    hListPopUpFrame = hListPopUpFrame:NEXT-SIBLING.
END.

/*special handling if a hListPopUpFrame is running*/
IF VALID-HANDLE(hListPopUpFrame) THEN DO:
    IF lEscPressed THEN DO:
      cmon:SCREEN-VALUE IN FRAME fMain = STRING(TIME, "hh:mm:ss") + " ESC Pressed, kill popup list".
      PUBLISH "KillEditorList" FROM THIS-PROCEDURE (hEditor).
      {&RETURN}
    END.

    IF lTabPressed THEN DO:
        cmon:SCREEN-VALUE IN FRAME fMain = STRING(TIME, "hh:mm:ss") + " TAB Pressed, if focus in list, then put focus back to editor".
        APPLY "ENTRY" TO hEditor.
        {&RETURN}
    END.
END. /*VALID-HANDLE(hListPopUpFrame)*/

IF lEscPressed THEN PUBLISH "KillEditorTooltip" FROM THIS-PROCEDURE (hEditor).

IF hEditor:TEXT-SELECTED THEN DO: /*if code selected in the editor then do nothing, or even kill the running popup list*/
    DEFINE VARIABLE cCurrentSelection AS CHARACTER  NO-UNDO.
    cCurrentSelection = hEditor:SELECTION-TEXT NO-ERROR. /*Note that hEditor:SELECTION-START and hEditor:SELECTION-END are not reliable with a source-code editor*/
    /* 22-AUG-2007 sla: make it easier to select an include file on triple click */
    cCurrentSelection = TRIM(cCurrentSelection).
    IF cCurrentSelection BEGINS "~{" AND INDEX(cCurrentSelection, ".i") > 0 THEN DO:
        cCurrentSelection = LEFT-TRIM(cCurrentSelection, "~{").
        cCurrentSelection = ENTRY(1, cCurrentSelection, " "). /* remove arguments */
        cCurrentSelection = RIGHT-TRIM(cCurrentSelection, "~}").
    END.

    IF VALID-HANDLE(hListPopUpFrame) THEN DO:
        IF   glProposeOpenSelectedFile
         AND (   gcLastSelection = cCurrentSelection
              OR gcLastSelection MATCHES "*~~~.cls")
         THEN {&RETURN}
        PUBLISH "KillEditorList" (hEditor).
    END.

    /*02-JAN-2007 sla: avoid error 42 when a large text is selected*/
    IF LENGTH(cCurrentSelection) < 1980 /* 05-AUG-2008 sla: went down from 3k to 1980 as suggested on the peg by Fred de Weerd in order to avoid a bad crash */      /*31-MAR-2007 sla: 3k might be enough */
     AND gcLastSelection <> cCurrentSelection THEN DO:
        cMon:SCREEN-VALUE = "Text selected cCurrentSelection = " + QUOTER(cCurrentSelection) NO-ERROR.
        gcLastSelection = cCurrentSelection.

        /* 24-AUG-2007 sla: now always do that here, as it does not cost anything */
        IF glDisplaytextSelectedInfo THEN RUN displaytextSelectedInfo (hEditor, gcLastSelection).

        IF glAnalyseQuery
         AND (   TRIM(cCurrentSelection) BEGINS "FOR "
              OR TRIM(cCurrentSelection) BEGINS "FIND "
              OR TRIM(cCurrentSelection) BEGINS "CAN-FIND(" AND cCurrentSelection MATCHES "*)"
              /*19-DEC-2006 sla: some people like to put one (or few) spaces after CAN-FIND*/
              OR TRIM(cCurrentSelection) BEGINS "CAN-FIND (" AND cCurrentSelection MATCHES "*)"
              OR TRIM(cCurrentSelection) BEGINS "CAN-FIND  (" AND cCurrentSelection MATCHES "*)")
         THEN DO:
            cCurrentSelection = REPLACE(cCurrentSelection, "CAN-FIND (", "CAN-FIND(").
            cCurrentSelection = REPLACE(cCurrentSelection, "CAN-FIND  (", "CAN-FIND(").
            RUN queryInfo (hEditor, TRIM(cCurrentSelection)).
        END.
        ELSE DO:
            FIND FIRST ttAPI WHERE ttAPI.cAPI = gcLastSelection NO-ERROR.
            IF AVAILABLE ttAPI
             AND NOT disabledApiTooltip(gcLastSelection) THEN DO:
              RUN showTooltip (hEditor, ttAPI.chelp, "canDisableApiTooltip=" + gcLastSelection /*"ScrollHoriz"*/ ).
              cMon:SCREEN-VALUE IN FRAME fMain = "API Tooltip for selected:" + gcLastSelection.
            END.
            /*08-JAN-2007 sla: new glProposeOpenSelectedFile feature*/
            ELSE IF glProposeOpenSelectedFile
             AND INDEX(gcLastSelection, "<") = 0
             AND INDEX(gcLastSelection, ">") = 0
             AND INDEX(gcLastSelection, " ") = 0
             AND INDEX(gcLastSelection, ",") = 0
             AND (   gcLastSelection MATCHES "*~~~.i"
                  OR gcLastSelection MATCHES "*~~~.p"
                  OR gcLastSelection MATCHES "*~~~.w")
             AND SEARCH(gcLastSelection) <> ?
             THEN DO:
                RUN proposeOpenSelected (hEditor, gcLastSelection).
                {&RETURN}
            END.
            /* attempt to find .cls file with ttUsing entries */
            ELSE IF glProposeOpenSelectedFile
             AND INDEX(gcLastSelection, " ") = 0
             AND INDEX(gcLastSelection, ">") = 0
             AND INDEX(gcLastSelection, "<") = 0
             AND INDEX(gcLastSelection, ",") = 0
             AND INDEX(gcLastSelection, "'") = 0
             AND INDEX(gcLastSelection, '"') = 0
             AND INDEX(gcLastSelection, "~~") = 0
             THEN DO:
                cClassFile = REPLACE(gcLastSelection, ".", "/") + ".cls".
                IF SEARCH(cClassFile) <> ? THEN DO:
                    gcLastSelection = REPLACE(gcLastSelection, ".", "/") + ".cls".
                    RUN proposeOpenSelected (hEditor, gcLastSelection).
                    {&RETURN}
                END.
                FOR EACH ttUsing WHERE ttUsing.hEditor = ghCurrentEditor:
                    cClassPath = RIGHT-TRIM(REPLACE(ttUsing.cUsing, ".", "/"), "*").
                    IF SEARCH(cClassPath + cClassFile) = ? THEN NEXT.
                    gcLastSelection = cClassPath + cClassFile.
                    RUN proposeOpenSelected (hEditor, gcLastSelection).
                    {&RETURN}
                END.  /* attempt to find .cls file with ttUsing entries */
            END.  /* attempt to find .cls file with ttUsing entries */
        END. /* ELSE   (IF glAnalyseQuery) */
        {&RETURN}
    END. /*IF   LENGTH(cCurrentSelection) < 10000*/
END. /*IF hEditor:TEXT-SELECTED*/
ELSE gcLastSelection = "".


/*Now the point is to know if we want to fire a completion on TAB*/
IF glCompleteOnTab AND lTabPressed THEN DO:
    IF lShiftPressed THEN {&RETURN}

    /*avoid ugly flashing*/
    DEFINE VARIABLE iReturnCode AS INTEGER    NO-UNDO.
    RUN lockWindowUpdate  (INPUT hEditor:FRAME:HWND, OUTPUT iReturnCode).
    hEditor:SOURCE-COMMAND('undo', '').
    RUN tabPressed IN THIS-PROCEDURE (hEditor).
    IF RETURN-VALUE = "" THEN DO:
        /*06-DEC-2006 sla: give abitliy to the developper to choose how many spaces to insert*/
        /*07-DEC-2006 sla: Fixed Tab Position like vslick does originaly*/
        DEFINE VARIABLE cSpacesToInsert AS CHAR NO-UNDO.
        cSpacesToInsert =  FILL(' ', giTabNumSpaces - (hEditor:CURSOR-CHAR - 1) MODULO giTabNumSpaces).
        hEditor:INSERT-STRING(cSpacesToInsert).
        /*hEditor:INSERT-STRING('    ').   /* Does not work because of what we've done in tabPressed... hEditor:SOURCE-COMMAND('undo', '').*/*/
        cmon:SCREEN-VALUE IN FRAME fMain = STRING(TIME, "hh:mm:ss") + " normal TAB".
    END.

    RUN lockWindowUpdate (INPUT 0, OUTPUT iReturnCode).

    /*cmon:INSERT-STRING(STRING(TIME, "hh:mm:ss") + " completion TAB").*/
    {&RETURN}
END.

/*if glAutoComplete is set, then simulate a value-changed by tracking the
editor length and the cursor positions*/
IF   glAutoComplete
 AND gdLoadLocalLastEtime = 0 /* don't fire value changed if pending loadLocal */
 AND ttEdt.iLength <> DECIMAL(hEditor:LENGTH) THEN DO:
    /*the point of this trick is to let the editor finish to type multiple
     characters (like when using native aliases, or paste or large deletions*/
    chCtrlEdtValueChanged:PSTimerValueChanged:enabled = NO. /*reset only the last change observed will be taken into account*/
    chCtrlEdtValueChanged:PSTimerValueChanged:enabled = YES.
    ttEdt.iLength = DECIMAL(hEditor:LENGTH) NO-ERROR. /*04-JAN-2007 sla: convert to INTEGER to avoid error in 10.0B when located in an empty editor*/
    ghEditorValueChanged = hEditor.
    {&RETURN}
END.

/*21-JAN-2007 sla: found better than relying on the fact the number of lines as changed
 Since I now detect the usage of buffer up to the current line, I need to reload
 each time the carret moves to a another line.  The good performances of the loader can allow this
 But I will use a trick to wait to be in the same line for 100ms before loading*/
IF gdLoadLocalLastEtime = 0
 AND (   ttEdt.iCursorLine <> hEditor:CURSOR-LINE
      OR ttEdt.iNumLines   <> hEditor:NUM-LINES)
 THEN gdLoadLocalLastEtime = DECIMAL(ETIME) + 100.

IF giUpdateMarksAboveLine = 0 AND ttEdt.iNumLines <> hEditor:NUM-LINES
 THEN giUpdateMarksAboveLine = hEditor:CURSOR-LINE.

/*keep track of current line*/
IF gdLoadLocalLastEtime > 0
 AND (   giCursorLine <> hEditor:CURSOR-LINE
      OR     giNumLines <> hEditor:NUM-LINES
         AND NOT hEditor:TEXT-SELECTED) /* don't fire localLoad while text is selected */
 THEN ASSIGN
  giCursorLine = hEditor:CURSOR-LINE
  giNumLines   = hEditor:NUM-LINES
  gdLoadLocalLastEtime = DECIMAL(ETIME) + 100.

/*fire the load when in same line for more than 100 ms*/
IF   gdLoadLocalLastEtime > 0
 AND gdLoadLocalLastEtime < DECIMAL(ETIME)
 AND giCursorLine = hEditor:CURSOR-LINE
 AND giNumLines   = hEditor:NUM-LINES
 THEN DO:
    RUN loadLocalResources (hEditor).
    ASSIGN
     gdLoadLocalLastEtime = 0
     ttEdt.iCursorLine = hEditor:CURSOR-LINE
     ttEdt.iNumLines   = hEditor:NUM-LINES.
END.

{&RETURN}

END. /* CtrlSpyTick:  DO ON ERROR UNDO, RETRY:*/
&UNDEFINE RETURN


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fKeys
&Scoped-define SELF-NAME gcActionOnCtrlS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcActionOnCtrlS C-Win
ON VALUE-CHANGED OF gcActionOnCtrlS IN FRAME fKeys
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME gcCatDbConnection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcCatDbConnection C-Win
ON RETURN OF gcCatDbConnection IN FRAME fMisc /* DB Connection phrase */
DO:
    APPLY "CHOOSE" TO btnTestAbhackDb.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcCatDbConnection C-Win
ON VALUE-CHANGED OF gcCatDbConnection IN FRAME fMisc /* DB Connection phrase */
DO:
    btnTestAbhackDb:VISIBLE = NO.
    PROCESS EVENTS.
    RUN Sleep (100).
    btnTestAbhackDb:VISIBLE = YES.
    PROCESS EVENTS.
    RUN Sleep (100).
    btnTestAbhackDb:VISIBLE = NO.
    PROCESS EVENTS.
    RUN Sleep (100).
    btnTestAbhackDb:VISIBLE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplCustom
&Scoped-define SELF-NAME gcComplDateFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcComplDateFormat C-Win
ON VALUE-CHANGED OF gcComplDateFormat IN FRAME fComplCustom /* Compl. date format */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME gcDumpedResourceFileRoot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcDumpedResourceFileRoot C-Win
ON VALUE-CHANGED OF gcDumpedResourceFileRoot IN FRAME fMisc /* XML Files Root Dir */
DO:
    RUN gcDumpedResourceFileRootValueChanged NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplRun
&Scoped-define SELF-NAME gcExtProcIgnoreDir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcExtProcIgnoreDir C-Win
ON VALUE-CHANGED OF gcExtProcIgnoreDir IN FRAME fComplRun /* Ignore dirs */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME gcFancyInABWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcFancyInABWin C-Win
ON VALUE-CHANGED OF gcFancyInABWin IN FRAME fMisc /* Fancy in AppBuilder Win */
DO:
  ASSIGN {&SELF-NAME}.

  IF NOT VALID-HANDLE(ghMainAppBuilderWindow) THEN RUN findAppBuilder.
  IF NOT VALID-HANDLE(ghMainAppBuilderWindow) THEN RETURN.

  IF VALID-HANDLE(ghABHackedText) THEN DELETE OBJECT ghABHackedText.

  CASE {&SELF-NAME}:
      WHEN "TEXT" THEN RUN createABHackText     (ghMainAppBuilderWindow).
      WHEN "smileyGun" OR WHEN "viveLaFrance" THEN RUN createABHackPic (ghMainAppBuilderWindow).
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplDot
&Scoped-define SELF-NAME gcFieldSortedBy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcFieldSortedBy C-Win
ON VALUE-CHANGED OF gcFieldSortedBy IN FRAME fComplDot
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME gcGlobalResCat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcGlobalResCat C-Win
ON VALUE-CHANGED OF gcGlobalResCat IN FRAME fMisc /*  Global Resource Catalog */
DO:
  ASSIGN {&SELF-NAME}.

  IF {&SELF-NAME} = "XML" THEN DO:
    RUN gcDumpedResourceFileRootValueChanged NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  ELSE IF {&SELF-NAME} = "DB" THEN RUN connectToAbhackDb NO-ERROR.

  IF {&SELF-NAME} <> "DB" AND CONNECTED("abhack") THEN DO:
      PUBLISH "exitAbhackDbLib".
      DISCONNECT abhack.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME gcSectionOutlineMode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcSectionOutlineMode C-Win
ON VALUE-CHANGED OF gcSectionOutlineMode IN FRAME fMain
DO:
ASSIGN {&SELF-NAME}.

/* restart it later */
PUBLISH "abhackSectionOutlineWindowExit".

IF {&SELF-NAME} = "DISABLED" THEN RETURN.


/* 20-AUG-2007 sla: added a special version of puire4gltv to make abhack independent */
/* IF SEARCH("pure4GLTv.w") = ? /* 21-JUN-2007 sla: changed "pure4GLTv.r" to "pure4GLTv.w" for Sneda that uses the same tools direcdtory for both V9 and OE10 */                               */
/*  OR SEARCH("tvpics\rightArrow.bmp") = ?                                                                                                                                                     */
/*  THEN DO:                                                                                                                                                                                   */
/*     MESSAGE 'The "Section Outline Window" feature requires the pureabltv/pure4gltv component to be installed (version equal or greater to "pureabltv-2007-JUN-09.zip")' SKIP                */
/*      "Please download it from PSDN and unzip/compile it in a directory of your propath." SKIP(2)                                                                                            */
/*      " Note: people that already have an older version of pureabltv will just need to add/update the new files present in the zip file (plus recompile pure4gltv.w for Progress Version 9)" */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                                                                                  */
/*     {&SELF-NAME} = "DISABLED".                                                                                                                                                              */
/*     {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}.                                                                                                                                               */
/*     RETURN NO-APPLY.                                                                                                                                                                        */
/* END.                                                                                                                                                                                        */

RUN protools/abhack/abhackSections.w PERSIST SET ghSectionLayoutWin.
RUN initializeObject IN ghSectionLayoutWin.
IF VALID-HANDLE(ghCurrentEditor)
 AND ghCurrentEditor:TYPE = "EDITOR"
 AND ghCurrentEditor:SOURCE-EDITOR
 THEN PUBLISH "ListSections" (ghCurrentEditor).
PUBLISH "AttachMode" ({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplRun
&Scoped-define SELF-NAME gcSplitParamCommaStyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcSplitParamCommaStyle C-Win
ON VALUE-CHANGED OF gcSplitParamCommaStyle IN FRAME fComplRun
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplSuggestUsedBuffers
&Scoped-define SELF-NAME gcSuggestBufferFor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcSuggestBufferFor C-Win
ON VALUE-CHANGED OF gcSuggestBufferFor IN FRAME fComplSuggestUsedBuffers
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplEnable
&Scoped-define SELF-NAME giAutoCompMinSize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL giAutoCompMinSize C-Win
ON VALUE-CHANGED OF giAutoCompMinSize IN FRAME fComplEnable /*  => Min size of words for completion */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME giMaxEmptyLinesBeforeShrinking
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL giMaxEmptyLinesBeforeShrinking C-Win
ON VALUE-CHANGED OF giMaxEmptyLinesBeforeShrinking IN FRAME fMisc /* Max empty lines before shrinking an editor window down */
DO:
    ASSIGN {&SELF-NAME}.

    IF {&SELF-NAME} > 0 AND {&SELF-NAME} < 5 THEN DO:
        MESSAGE "Do not set a value below 5" SKIP
         "Exception is zero to disable the feature"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplRun
&Scoped-define SELF-NAME giMaxEtimeToLoadFiles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL giMaxEtimeToLoadFiles C-Win
ON VALUE-CHANGED OF giMaxEtimeToLoadFiles IN FRAME fComplRun /* Max time to spend at loading */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME giMaxFilesToLoad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL giMaxFilesToLoad C-Win
ON VALUE-CHANGED OF giMaxFilesToLoad IN FRAME fComplRun /* Max Number of Items */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME giNameComboFont
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL giNameComboFont C-Win
ON VALUE-CHANGED OF giNameComboFont IN FRAME fMisc
DO:
  ASSIGN {&SELF-NAME}.

  DEFINE BUFFER ttEdt FOR ttEdt.

  FOR EACH ttEdt:
      IF NOT VALID-HANDLE(ttEdt.hEventCombo) THEN NEXT.

      ttEdt.hEventCombo:FONT = IF giNameComboFont = -1
                                THEN ?
                                ELSE giNameComboFont.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME giQueryTooltipTimeFail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL giQueryTooltipTimeFail C-Win
ON VALUE-CHANGED OF giQueryTooltipTimeFail IN FRAME fMisc
DO:
    ASSIGN {&SELF-NAME}.
    {&SELF-NAME}txt:SCREEN-VALUE = STRING({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME giQueryTooltipTimeSuccess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL giQueryTooltipTimeSuccess C-Win
ON VALUE-CHANGED OF giQueryTooltipTimeSuccess IN FRAME fMisc
DO:
  ASSIGN {&SELF-NAME}.
  {&SELF-NAME}txt:SCREEN-VALUE = STRING({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME giRightMarginIndicatorCol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL giRightMarginIndicatorCol C-Win
ON VALUE-CHANGED OF giRightMarginIndicatorCol IN FRAME fMisc /* Right margin indicator at column */
DO:
  ASSIGN {&SELF-NAME}.
  RUN vsSetDefaultOption ({&VSOPTION_RIGHT_MARGIN_INDICATOR_COL}, {&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplRun
&Scoped-define SELF-NAME giSplitParamInCalls
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL giSplitParamInCalls C-Win
ON VALUE-CHANGED OF giSplitParamInCalls IN FRAME fComplRun /* Split line when more param than */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplEnable
&Scoped-define SELF-NAME giTabNumSpaces
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL giTabNumSpaces C-Win
ON VALUE-CHANGED OF giTabNumSpaces IN FRAME fComplEnable /*  => Num of spaces for a normal TAB */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME glAbhackFind
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glAbhackFind C-Win
ON VALUE-CHANGED OF glAbhackFind IN FRAME fMain /* Enable ABHack Find Window */
DO:
ASSIGN glAbhackFind.

IF VALID-HANDLE(ghCurrentEditor) THEN RUN setCtrlFAction (ghCurrentEditor).

PUBLISH "abhackFindExit". /* kill running instance(s) */
IF NOT glAbhackFind THEN RETURN.

RUN protools/abhack/abhackFind.w PERSIST (gcImportDirName).
PUBLISH "abhackFindTrackThis" (ghCurrentEditor).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplColon
&Scoped-define SELF-NAME glAddEndAfterDo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glAddEndAfterDo C-Win
ON VALUE-CHANGED OF glAddEndAfterDo IN FRAME fComplColon /* Add END after DO: or REPEAT: */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplTable
&Scoped-define SELF-NAME glAddNoLock
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glAddNoLock C-Win
ON VALUE-CHANGED OF glAddNoLock IN FRAME fComplTable /* Propose NO-LOCK/EXCLUSIVE */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glAddWhereAfterBuffer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glAddWhereAfterBuffer C-Win
ON VALUE-CHANGED OF glAddWhereAfterBuffer IN FRAME fComplTable /* WHERE after FIRST/EACH... <buffer> */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fAlign
&Scoped-define SELF-NAME glAlignAssign
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glAlignAssign C-Win
ON VALUE-CHANGED OF glAlignAssign IN FRAME fAlign /* Assign Statement (align equal signs) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glAlignEnabled
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glAlignEnabled C-Win
ON VALUE-CHANGED OF glAlignEnabled IN FRAME fAlign /* Align on Ctrl-Alt-L activated */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glAlignParams
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glAlignParams C-Win
ON VALUE-CHANGED OF glAlignParams IN FRAME fAlign /* Parameter definitions */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glAlignTTFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glAlignTTFields C-Win
ON VALUE-CHANGED OF glAlignTTFields IN FRAME fAlign /* Temp-table fields definition */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glAlignTTFieldsSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glAlignTTFieldsSort C-Win
ON VALUE-CHANGED OF glAlignTTFieldsSort IN FRAME fAlign /* Sort Temp-table Fields */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glAlignVars
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glAlignVars C-Win
ON VALUE-CHANGED OF glAlignVars IN FRAME fAlign /* Variable definitions */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME glAnalyseQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glAnalyseQuery C-Win
ON VALUE-CHANGED OF glAnalyseQuery IN FRAME fMisc /* Analyse Text Selected Static Queries */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplEnable
&Scoped-define SELF-NAME glAutoComplete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glAutoComplete C-Win
ON VALUE-CHANGED OF glAutoComplete IN FRAME fComplEnable /* Auto complete/popup while typing */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME glAutoStopDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glAutoStopDialog C-Win
ON VALUE-CHANGED OF glAutoStopDialog IN FRAME fMisc /* Stop spying when the focus is in dialog */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glAutoStopWhenNonEditorWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glAutoStopWhenNonEditorWin C-Win
ON VALUE-CHANGED OF glAutoStopWhenNonEditorWin IN FRAME fMisc /* Stop spying when not in editor window (leverage tracing) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplClasses
&Scoped-define SELF-NAME glClassNewCastAfterEqual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glClassNewCastAfterEqual C-Win
ON VALUE-CHANGED OF glClassNewCastAfterEqual IN FRAME fComplClasses /* NEW or CAST after '<classVar> =' */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fKeys
&Scoped-define SELF-NAME glcommentOnDblSlash
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glcommentOnDblSlash C-Win
ON VALUE-CHANGED OF glcommentOnDblSlash IN FRAME fKeys /* Comment or uncomment line on '//'  (or add comment at end of line) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glcommentSelectionOnSlash
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glcommentSelectionOnSlash C-Win
ON VALUE-CHANGED OF glcommentSelectionOnSlash IN FRAME fKeys /* Comment/uncomment selection on '/' */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glCompile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCompile C-Win
ON VALUE-CHANGED OF glCompile IN FRAME fKeys /* Compile on Ctrl-Alt-M */
DO:
  ASSIGN {&SELF-NAME}.
  glCompileMinsize:SENSITIVE = glCompile.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glCompileMinsize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCompileMinsize C-Win
ON VALUE-CHANGED OF glCompileMinsize IN FRAME fKeys /* Use MIN-SIZE option */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplColon
&Scoped-define SELF-NAME glCompleteAttrOnTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCompleteAttrOnTab C-Win
ON VALUE-CHANGED OF glCompleteAttrOnTab IN FRAME fComplColon /* Attribute/Method popup on : key */
DO:
  ASSIGN glCompleteAttrOnTab.
  IF glCompleteAttrOnTab THEN RUN loadttAttr IN TARGET-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplBlockLabel
&Scoped-define SELF-NAME glCompleteBlockLabels
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCompleteBlockLabels C-Win
ON VALUE-CHANGED OF glCompleteBlockLabels IN FRAME fComplBlockLabel /* Completion of block label enabled */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplDot
&Scoped-define SELF-NAME glCompleteField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCompleteField C-Win
ON VALUE-CHANGED OF glCompleteField IN FRAME fComplDot /* Complete DB  TT Fields on dot */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplEnable
&Scoped-define SELF-NAME glCompleteOnTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCompleteOnTab C-Win
ON VALUE-CHANGED OF glCompleteOnTab IN FRAME fComplEnable /* Complete/popup on TAB */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplColon
&Scoped-define SELF-NAME glCompleteOOSuperMethod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCompleteOOSuperMethod C-Win
ON VALUE-CHANGED OF glCompleteOOSuperMethod IN FRAME fComplColon /* OO  SUPER:method() */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplTable
&Scoped-define SELF-NAME glCompleteTableOnFLA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCompleteTableOnFLA C-Win
ON VALUE-CHANGED OF glCompleteTableOnFLA IN FRAME fComplTable /* First try with FLA (dump-file) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplEnable
&Scoped-define SELF-NAME glCompleteWithSimilar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCompleteWithSimilar C-Win
ON VALUE-CHANGED OF glCompleteWithSimilar IN FRAME fComplEnable /* Similar completion on Ctrl-alt-left/right */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplTable
&Scoped-define SELF-NAME glConvertDbTablesToLC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glConvertDbTablesToLC C-Win
ON VALUE-CHANGED OF glConvertDbTablesToLC IN FRAME fComplTable /* Convert 100% CAPS to lower case */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fKeys
&Scoped-define SELF-NAME glCopyUIBelowMouse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCopyUIBelowMouse C-Win
ON VALUE-CHANGED OF glCopyUIBelowMouse IN FRAME fKeys /* Alt-Ctrl-C=>copy UI-value of widget below the mouse pointer */
DO:
  ASSIGN {&SELF-NAME}.

  IF {&SELF-NAME} THEN RUN protools/abhack/abhackCtrlAltCAnywhereTrig.p PERSISTENT.
  ELSE PUBLISH "KillabhackCtrlAltCAnywhereTrig".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glCtrlBackspacelDelPrevWord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCtrlBackspacelDelPrevWord C-Win
ON VALUE-CHANGED OF glCtrlBackspacelDelPrevWord IN FRAME fKeys /* Del Prev Word on Ctrl-Backspace */
DO:
  ASSIGN {&SELF-NAME}.
  IF {&SELF-NAME} THEN
   MESSAGE "/* 20-DEC-2006 sla:  */" SKIP
    'Note this feature is not very reliable with vslick...' SKIP
    'Sometimes, it will end up in deleting the whole line, espcially if you just did Ctrl-Z before' SKIP(2)
    'Perhaps I will find a trick later'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glCtrlDelDelsWord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCtrlDelDelsWord C-Win
ON VALUE-CHANGED OF glCtrlDelDelsWord IN FRAME fKeys /* Del Current Word on Ctrl-Del */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glCtrlF3NewProcWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCtrlF3NewProcWin C-Win
ON VALUE-CHANGED OF glCtrlF3NewProcWin IN FRAME fKeys /* New procedure Window on Ctrl-F3 (from section editor) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplCustom
&Scoped-define SELF-NAME glCustomCompletionOnTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCustomCompletionOnTab C-Win
ON VALUE-CHANGED OF glCustomCompletionOnTab IN FRAME fComplCustom /* Custom completion (refresh defs) */
DO:
  ASSIGN glCustomCompletionOnTab.
  IF glCustomCompletionOnTab THEN RUN loadttCustAlias IN TARGET-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fKeys
&Scoped-define SELF-NAME glDebugListingOnCtrlAltD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glDebugListingOnCtrlAltD C-Win
ON VALUE-CHANGED OF glDebugListingOnCtrlAltD IN FRAME fKeys /* Debug listing on Ctrl-Alt-D */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME glDisplaytextSelectedInfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glDisplaytextSelectedInfo C-Win
ON VALUE-CHANGED OF glDisplaytextSelectedInfo IN FRAME fMisc /* Display Info about selected Text (in Info Win) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glDrawBoxAroundCurrentLine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glDrawBoxAroundCurrentLine C-Win
ON VALUE-CHANGED OF glDrawBoxAroundCurrentLine IN FRAME fMisc /* Draw box aroud current line */
DO:
  ASSIGN {&SELF-NAME}.

  RUN vsSetDefaultOption ({&VSOPTION_DRAW_BOX_AROUND_CURRENT_LINE}, IF {&SELF-NAME} THEN 1 ELSE 0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fKeys
&Scoped-define SELF-NAME glDuplicateLineOnAltDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glDuplicateLineOnAltDown C-Win
ON VALUE-CHANGED OF glDuplicateLineOnAltDown IN FRAME fKeys /* Duplicate line on Alt-Ctrl-Down/Up */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplClosingGuy
&Scoped-define SELF-NAME glEnhancedCloseParenthese
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glEnhancedCloseParenthese C-Win
ON VALUE-CHANGED OF glEnhancedCloseParenthese IN FRAME fComplClosingGuy /* Enhanced behavior to insert ) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplRun
&Scoped-define SELF-NAME glExtProcCompletion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glExtProcCompletion C-Win
ON VALUE-CHANGED OF glExtProcCompletion IN FRAME fComplRun /* OS File list after RUN *.p,*.w or *.i */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplColon
&Scoped-define SELF-NAME glFilterGuessObjectType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glFilterGuessObjectType C-Win
ON VALUE-CHANGED OF glFilterGuessObjectType IN FRAME fComplColon /* Guess Object type from var name */
DO:
  ASSIGN {&SELF-NAME}.
  IF glFilterGuessObjectType THEN RUN loadttGuessObjType IN TARGET-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glFilterOnViewAsWidgetType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glFilterOnViewAsWidgetType C-Win
ON VALUE-CHANGED OF glFilterOnViewAsWidgetType IN FRAME fComplColon /* Filter on widget type (use view-as ...) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fKeys
&Scoped-define SELF-NAME glFindNextSelected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glFindNextSelected C-Win
ON VALUE-CHANGED OF glFindNextSelected IN FRAME fKeys /* Find Next/Prev Selected Text on Alt-Right/Left */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplRun
&Scoped-define SELF-NAME glFindOSFilesWithAbhackDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glFindOSFilesWithAbhackDB C-Win
ON VALUE-CHANGED OF glFindOSFilesWithAbhackDB IN FRAME fComplRun /* Use ABHack.db to find OS files */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME glForceWaitFor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glForceWaitFor C-Win
ON VALUE-CHANGED OF glForceWaitFor IN FRAME fMisc /* Force a WAIT-FOR in the ABHack procedure */
DO:
  ASSIGN {&SELF-NAME}.
  MESSAGE "You need to restard ABHack to take this property into account"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fKeys
&Scoped-define SELF-NAME glHideAllComments
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glHideAllComments C-Win
ON VALUE-CHANGED OF glHideAllComments IN FRAME fKeys /* Hide all comments / Show All on Alt-C */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glHideSelection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glHideSelection C-Win
ON VALUE-CHANGED OF glHideSelection IN FRAME fKeys /* Hide/Show Selection on Alt-H/Alt-S */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplEnable
&Scoped-define SELF-NAME glHypeStrikeMode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glHypeStrikeMode C-Win
ON VALUE-CHANGED OF glHypeStrikeMode IN FRAME fComplEnable /* Hype Strike Mode (push on space,.:()[]) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fKeys
&Scoped-define SELF-NAME glImprovedCtrlKeftRight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glImprovedCtrlKeftRight C-Win
ON VALUE-CHANGED OF glImprovedCtrlKeftRight IN FRAME fKeys /* Improved Navigation by Word  Ctrl-Left/Right */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplClosingGuy
&Scoped-define SELF-NAME glInsertClosingGuys
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glInsertClosingGuys C-Win
ON VALUE-CHANGED OF glInsertClosingGuys IN FRAME fComplClosingGuy /* Enabled on ), ] single/double quotes */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplVarUDF
&Scoped-define SELF-NAME glIntFuncCompletion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glIntFuncCompletion C-Win
ON VALUE-CHANGED OF glIntFuncCompletion IN FRAME fComplVarUDF /* User Defined Function / Method Compl. */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplRun
&Scoped-define SELF-NAME glIntProcCompletion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glIntProcCompletion C-Win
ON VALUE-CHANGED OF glIntProcCompletion IN FRAME fComplRun /* Internal Procedure List after RUN */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glIntProcInputInSig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glIntProcInputInSig C-Win
ON VALUE-CHANGED OF glIntProcInputInSig IN FRAME fComplRun /* Keep 'INPUT' in param signature */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME glJumpMruSection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glJumpMruSection C-Win
ON VALUE-CHANGED OF glJumpMruSection IN FRAME fMisc /* Navigate MRU Sections on Alt-Page-Up/Down */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glKeepTrackListingBuffer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glKeepTrackListingBuffer C-Win
ON VALUE-CHANGED OF glKeepTrackListingBuffer IN FRAME fMisc /* Always keep track compile-listing buffer usage  scope */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glKeepTrackOfCursor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glKeepTrackOfCursor C-Win
ON VALUE-CHANGED OF glKeepTrackOfCursor IN FRAME fMisc /* Keep track of the Cursor Position in each Section */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fKeys
&Scoped-define SELF-NAME glListingOnCtrlAltT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glListingOnCtrlAltT C-Win
ON VALUE-CHANGED OF glListingOnCtrlAltT IN FRAME fKeys /* Listing on Ctrl-Alt-T */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplEnable
&Scoped-define SELF-NAME glLowerCaseCompletion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glLowerCaseCompletion C-Win
ON VALUE-CHANGED OF glLowerCaseCompletion IN FRAME fComplEnable /* Completion with lower case */
DO:
  DEFINE BUFFER ttAttr FOR ttAttr.

  ASSIGN {&SELF-NAME}.

  FOR EACH ttAttr:
      ttAttr.cAttr = IF glLowerCaseCompletion THEN LC(ttAttr.cAttr) ELSE CAPS(ttAttr.cAttr).
  END.

  MESSAGE "VSLick has the ability to auto-caps Progress 4GL keywords.  It is up to you to disable/re-enable this functionality manually in:" SKIP
   "AppBuilder -> Options -> Editing Options -> Option ...  (button)" SKIP
   "   => 'Auto case keywords' toggle-box at the bottom" SKIP(2)
   "Then you might have to press 'Update' in the privious dialog."
      VIEW-AS ALERT-BOX WARNING BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fKeys
&Scoped-define SELF-NAME glMaximizeRestore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glMaximizeRestore C-Win
ON VALUE-CHANGED OF glMaximizeRestore IN FRAME fKeys /* Maximize/Restore Window on Ctrl-M */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME glMoveNewWindowsAway
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glMoveNewWindowsAway C-Win
ON VALUE-CHANGED OF glMoveNewWindowsAway IN FRAME fMisc /* Move new detected editor windows away from ABHack */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fKeys
&Scoped-define SELF-NAME glMulitpleClipboardDummyToggle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glMulitpleClipboardDummyToggle C-Win
ON VALUE-CHANGED OF glMulitpleClipboardDummyToggle IN FRAME fKeys /* Multiple clipboard on Ctrl-Alt-V */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplEnable
&Scoped-define SELF-NAME glNoCompletionInComment
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glNoCompletionInComment C-Win
ON VALUE-CHANGED OF glNoCompletionInComment IN FRAME fComplEnable /* No completion in comment (single line) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glNoCompletionInStrings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glNoCompletionInStrings C-Win
ON VALUE-CHANGED OF glNoCompletionInStrings IN FRAME fComplEnable /* No completion in quoted strings */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fKeys
&Scoped-define SELF-NAME glOpenPW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glOpenPW C-Win
ON VALUE-CHANGED OF glOpenPW IN FRAME fKeys /* Generated procedure window on Ctrl-Alt-E   (for global search) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME glPlusplus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glPlusplus C-Win
ON VALUE-CHANGED OF glPlusplus IN FRAME fMisc /* myVar++   ==>   myVar =  myVar +   (for any word) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME glPreprocessors
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glPreprocessors C-Win
ON VALUE-CHANGED OF glPreprocessors IN FRAME FRAME-A /* Completion for preprocessor names */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME glProposeOpenSelectedFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glProposeOpenSelectedFile C-Win
ON VALUE-CHANGED OF glProposeOpenSelectedFile IN FRAME fMisc /* Propose to open selected source file  (path + .p/.w/.i) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glQADispInABHWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glQADispInABHWin C-Win
ON VALUE-CHANGED OF glQADispInABHWin IN FRAME fMisc /* Display report in floating Info Window */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glResizableDictionary
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glResizableDictionary C-Win
ON VALUE-CHANGED OF glResizableDictionary IN FRAME fMisc /* Catch Data Dictionary and make it resizable */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplRun
&Scoped-define SELF-NAME glRunInLibComp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glRunInLibComp C-Win
ON VALUE-CHANGED OF glRunInLibComp IN FRAME fComplRun /* LibHandles after RUN IN  => then IP */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME glSaveSession
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glSaveSession C-Win
ON VALUE-CHANGED OF glSaveSession IN FRAME fMisc /* Save  restore session */
/* 25-OCT-2013 jcc: new configuration option */
DO:
  ASSIGN {&SELF-NAME}.

  /* 25-OCT-2013 jcc: when disabled, cleanup registry keys */
  IF NOT {&SELF-NAME} THEN DO:
      DEFINE VARIABLE cSection AS CHARACTER   NO-UNDO.
      LOAD 'Software\PSC\PROGRESS\slacroixTools' BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
      /* create it if it does not exist  ... well, would be surprising...*/
      IF ERROR-STATUS:ERROR THEN LOAD 'Software\PSC\PROGRESS\slacroixTools' NEW BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN USE 'Software\PSC\PROGRESS\slacroixTools'.
      cSection = 'ABHacker'.
      PUT-KEY-VALUE SECTION cSection KEY 'openedFiles'         VALUE ?.
      PUT-KEY-VALUE SECTION cSection KEY 'openedFilesGeometry' VALUE ?.
      PUT-KEY-VALUE SECTION cSection KEY 'openedFilesPosition' VALUE ?.
      PUT-KEY-VALUE SECTION cSection KEY 'openedFilesSection'  VALUE ?.
      /* unload the environment to go back to default env */
      UNLOAD 'Software\PSC\PROGRESS\slacroixTools' NO-ERROR.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME glSectionLayoutIgnoreStructProcs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glSectionLayoutIgnoreStructProcs C-Win
ON VALUE-CHANGED OF glSectionLayoutIgnoreStructProcs IN FRAME fMain /* Ignore structured procedure */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fKeys
&Scoped-define SELF-NAME glShowLineNumbers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glShowLineNumbers C-Win
ON VALUE-CHANGED OF glShowLineNumbers IN FRAME fKeys /* Show/Hide Line Numbers on Ctrl-Alt-I */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glShowSpecialChars
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glShowSpecialChars C-Win
ON VALUE-CHANGED OF glShowSpecialChars IN FRAME fKeys /* Show/Hide sPecial chars (space, CR) on Ctrl-Alt-P */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glStopStartTimerOnCtrlAltO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glStopStartTimerOnCtrlAltO C-Win
ON VALUE-CHANGED OF glStopStartTimerOnCtrlAltO IN FRAME fKeys /* Stop/Start Spying Timer on Ctrl-Alt-O */
DO:
  ASSIGN {&SELF-NAME}.

  IF {&SELF-NAME} THEN RUN protools/abhack/abhackCtrlOAnywhereTrig.p PERSISTENT.
  ELSE PUBLISH "KillabhackCtrlOAnywhereTrig".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMisc
&Scoped-define SELF-NAME gltooltipOnOpenParenthese
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gltooltipOnOpenParenthese C-Win
ON VALUE-CHANGED OF gltooltipOnOpenParenthese IN FRAME fMisc /* API Tooltips on open parenthese or on selection */
DO:
  ASSIGN gltooltipOnOpenParenthese.
  IF gltooltipOnOpenParenthese THEN RUN loadttAPITooltip IN TARGET-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glTopOnly
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glTopOnly C-Win
ON VALUE-CHANGED OF glTopOnly IN FRAME fMisc /* ABHack Window top-only */
DO:
  ASSIGN {&SELF-NAME}.
  {&WINDOW-NAME}:TOP-ONLY = {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glUseSchemaCache
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glUseSchemaCache C-Win
ON VALUE-CHANGED OF glUseSchemaCache IN FRAME fMisc /* Use DB Table Schema Cache */
DO:
    ASSIGN {&SELF-NAME}.

    DEFINE BUFFER ttDbTableCache FOR ttDbTableCache.

    IF {&SELF-NAME} THEN RUN loadDbTableCache.
    ELSE EMPTY TEMP-TABLE ttDbTableCache.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fComplVarUDF
&Scoped-define SELF-NAME glVariableCompletion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glVariableCompletion C-Win
ON VALUE-CHANGED OF glVariableCompletion IN FRAME fComplVarUDF /* Variable name/TT completion */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fKeys
&Scoped-define SELF-NAME glXrefAnal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glXrefAnal C-Win
ON VALUE-CHANGED OF glXrefAnal IN FRAME fKeys /* Analyze XRef utility on Ctrl-Alt-X  (from section editor) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win


/* ***************************  Main Block  *************************** */


/* 07-DEC-2006 sla: security */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    MESSAGE "Please run ABHack with the PERSISTENT option !  It does not make sense otherwise" SKIP(2)
        "About to abort the launch now"
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    RETURN.
END.


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME}
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

RUN recompileAbhack.
IF RETURN-VALUE = "exitABHack" THEN DO:
    /* 26-AUG-2008 sla: improvement suggested by Dries, restart it automatically */
    DEFINE VARIABLE cRestartAbHackProc AS CHARACTER  NO-UNDO.
    cRestartAbHackProc = THIS-PROCEDURE:FILE-NAME.
    DELETE PROCEDURE THIS-PROCEDURE.
    RUN VALUE(cRestartAbHackProc) PERSISTENT.
    RETURN.
END.

RUN initVlx. /* 14-MAR-2014 jcc */

SUBSCRIBE TO "stopABHackSpyingTimer"    ANYWHERE.
SUBSCRIBE TO "restartABHackSpyingTimer" ANYWHERE.



IF VALID-HANDLE(gshABHackWin) THEN DO:
    DEFINE VARIABLE hRunningInstanceWin AS HANDLE     NO-UNDO.
    hRunningInstanceWin = gshABHackWin:CURRENT-WINDOW.

    /* 14-MAR-2007 sla: this can happen when a previous instance of ABHack exited in a badly way */
    IF NOT VALID-HANDLE(hRunningInstanceWin) THEN DO:
        APPLY 'CLOSE' TO gshABHackWin.
        RETURN.
    END.

    IF hRunningInstanceWin:VISIBLE THEN RUN hideWindow IN gshABHackWin.
    ELSE DO:
        RUN hideWindow IN gshABHackWin.
        APPLY "ENTRY" TO hRunningInstanceWin.
    END.
    DELETE PROCEDURE THIS-PROCEDURE.
    RETURN.
END.
ELSE gshABHackWin = THIS-PROCEDURE.


/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE RUN CloseProcedure.

/* 26-AUG-2008 sla: the following subscription introduces a serious regression since this
 event is fired when a GUI window or a Structured Procedure is closed
/* 20-AUG-2008 sla: this will help to catch a AppBuilder Exit event */
SUBSCRIBE TO "SE_EXIT" ANYWHERE RUN-PROCEDURE "CloseProcedure".
*/


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN initializeABHack NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN.

    /* 07-DEC-2006 sla: Trick to avoid instabilities in 9.1E FCS
     took opportunity to remove the standard if THIS-PROCEDURE:PERSISTENT THEN WAIT-FOR...
     19-DEC-2006 sla: improvement to avoid badly nested WAIT-FOR's */
    IF glForceWaitFor AND VALID-HANDLE(ghMainAppBuilderWindow) THEN DO:
        ON "WINDOW-CLOSE" OF ghMainAppBuilderWindow APPLY "CLOSE" TO THIS-PROCEDURE.
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ABFancyGuyChoose C-Win
PROCEDURE ABFancyGuyChoose :
/*------------------------------------------------------------------------------
  Purpose:   06-JUL-2007 new hook to do the following:

Now a click on the fancy pic on the AppBuilder (flag/smiley/or text) takes the opportunity to implement a resurect
Session Windows feature.  This is very handy when one uses a Virtual Desktop Solution like "Enable Virtual
Desktop" (which I recommand : http://enablevirtualdesktop.com/virtualdesktop/index.htm)
Indeed, Virtual Desktop are not native on Windoze (I've always seen them on *NIX for 14 yers...) and those nice
magic tools can sometimes have problems with the collaborative implementation of the Window Manager of M$ Windoze,
which can lead to hidden small windows when switching to another Virtual Desktop.
  => The new ABHack resurect windows feature will help to repair de situation.  Just click on the fancy picture
and the Object Palette, Protools will be made visible again.  On the other hand, ABHack windows will be made hidden then visible
so click twice to resurect abhack

  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE hWin AS HANDLE     NO-UNDO.
DEFINE VARIABLE hChildWin AS HANDLE      NO-UNDO. /* special case for AppBuilder Window */

hWin = SESSION:FIRST-CHILD.
/* the point of this routine is to flip flop the visible attribute to make a window hidden by a Virtual Desktop Change visible again */
DO WHILE hwin <> ?:
    IF  (   hWin:TITLE MATCHES "*PRO~~~*Tools*"
         OR hWin:TITLE MATCHES "*Section Layout*"
         OR hWin:TITLE MATCHES "*ABHack*")
       AND hWin:VISIBLE THEN ASSIGN
      hWin:VISIBLE = NO
      hWin:VISIBLE = YES.

    /* manage the palette window */
    IF hWin:TITLE = "AppBuilder" THEN DO:
        hChildWin = hWin:FIRST-CHILD.
        DO WHILE hChildWin <> ?:
            IF hChildWin:TYPE = "WINDOW"
             AND hChildWin:TITLE MATCHES "*palette*"
             AND hChildWin:VISIBLE THEN ASSIGN
              hChildWin:VISIBLE = NO
              hChildWin:VISIBLE = YES.

            hChildWin = hChildWin:NEXT-SIBLING.
        END.
    END.

    hWin = hWin:NEXT-SIBLING.
END.

/* and now the usual hide/show Main abhackwin */
RUN hideWindow IN THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adaptWindow C-Win
PROCEDURE adaptWindow :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

DEFINE BUFFER ttEdt FOR ttEdt.

IF glAdaptWindowHeightOnCtrlW = NO THEN RETURN.

FIND ttEdt WHERE ttEdt.hEditor = phEditor. /* cannot fail */

DEFINE VARIABLE iEditorLineHeightPixels AS INTEGER     NO-UNDO.

iEditorLineHeightPixels = FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(ttEdt.hEditor:FONT).

/* shrink */
/* 06-NOV-2007 sla: changed the + 2  into + 3 because of the automatic scrolling done by vslick when reachjing the bottom of the viewport */
IF MAXIMUM (giMaxEmptyLinesBeforeShrinking ,ttEdt.hEditor:NUM-LINES + 3) < (SESSION:WORK-AREA-HEIGHT-PIXELS - ttEdt.hWin:Y - 72 ) / iEditorLineHeightPixels
 THEN DO:
    ttEdt.hWin:HEIGHT-PIXELS = MAXIMUM(giMaxEmptyLinesBeforeShrinking, ttEdt.hEditor:NUM-LINES + 3) * iEditorLineHeightPixels
                               + 25 + IF ttEdt.lManageableSections THEN 55 ELSE 0.
    .
    APPLY "WINDOW-RESIZED"  TO ttEdt.hWin.
END.
/* enlarge */
ELSE IF ttEdt.hWin:HEIGHT-PIXELS < MINIMUM(SESSION:WORK-AREA-HEIGHT-PIXELS - ttEdt.hWin:Y - 72  /* 72 for wintitle and  abhack find */
                                             - IF ttEdt.lManageableSections THEN 55 ELSE 0
                                          ,(ttEdt.hEditor:NUM-LINES + 2) * iEditorLineHeightPixels)
 THEN DO:
    ttEdt.hWin:HEIGHT-PIXELS = MINIMUM(SESSION:WORK-AREA-HEIGHT-PIXELS - ttEdt.hWin:Y - 72 - IF ttEdt.lManageableSections THEN 55 ELSE 0
                                      ,(ttEdt.hEditor:NUM-LINES + 2) * iEditorLineHeightPixels).
    APPLY "WINDOW-RESIZED"  TO ttEdt.hWin.
END.

END PROCEDURE.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE alignAssign C-Win
PROCEDURE alignAssign :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER pcTxt AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cLine          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTabIdent      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iLeftLengthMax AS INTEGER    NO-UNDO.
DEFINE VARIABLE iLine          AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNameLengthMax AS INTEGER    NO-UNDO.
DEFINE VARIABLE iseq           AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTypeLengthMax AS INTEGER    NO-UNDO.
DEFINE VARIABLE iWord          AS INTEGER    NO-UNDO.
DEFINE VARIABLE lFirstLineAssign AS LOGICAL     NO-UNDO.

DEFINE BUFFER ttAlignAssign FOR ttAlignAssign.

EMPTY TEMP-TABLE ttAlignAssign.

REPEAT iLine = 1 TO NUM-ENTRIES(pcTxt, "~n")
 ON ERROR UNDO, RETURN:
    cLine = ENTRY(iLine, pcTxt, "~n").
    IF iLine = 1 THEN cTabIdent = FILL(" ", LENGTH(cLine) - LENGTH(LEFT-TRIM(cLine))).
    cLine = TRIM(cLine).

    /* keep single spaces */
    DO WHILE INDEX(cLine, "  ") > 0:
        cLine = REPLACE(cLine, "  ", " ").
    END.

    /* Line ready to be imported, now store the words */
    CREATE ttAlignAssign.
    ASSIGN
     iSeq = iSeq + 1
     ttAlignAssign.iSeq = iSeq.
    /* weird line, keep it somewhere... */
    IF INDEX(cLine, " = ") = 0 THEN DO:
        ttAlignAssign.cRightPart = cLine.
        NEXT.
    END.

    ttAlignAssign.cLeft = SUBSTRING(cLine, 1, INDEX(cLine, " =") - 1).
    IF iLine = 1 AND cLine BEGINS "ASSIGN " THEN ASSIGN
     lFirstLineAssign = YES
     ttAlignAssign.cLeft = SUBSTRING(ttAlignAssign.cLeft, 8). /* remove the'ASSIGN ', we will read it later */

    ASSIGN
     ttAlignAssign.cRightPart = SUBSTRING(cLine, INDEX(cLine, "= ") + 2)
     ttAlignAssign.iLeftLength = LENGTH(ttAlignAssign.cLeft).
END.

FIND LAST ttAlignAssign USE-INDEX iLeftLength.
iLeftLengthMax = ttAlignAssign.iLeftLength.

pcTxt = "".
FOR EACH ttAlignAssign BY ttAlignAssign.iSeq:
    IF ttAlignAssign.cLeft + ttAlignAssign.cRightPart = "" THEN DO:
        pcTxt = pcTxt + "~n".
        NEXT.
    END.
    pcTxt = pcTxt + "~n" + cTabIdent
           + (IF lFirstLineAssign
               THEN IF ttAlignAssign.iSeq = 1
                     THEN (IF glLowerCaseCompletion THEN "assign " ELSE "ASSIGN ")
                     ELSE "       "
               ELSE ""
             )
           + (IF ttAlignAssign.cLeft = ""
               THEN ""
               ELSE ttAlignAssign.cLeft
                    + FILL(" ", iLeftLengthMax - ttAlignAssign.iLeftLength)
                    + " = "
             )

           + ttAlignAssign.cRightPart.
END.
pcTxt = SUBSTR(pcTxt,2).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE alignBuffers C-Win
PROCEDURE alignBuffers :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER pcTxt AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cKeyWordCase     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTabIdent        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWord            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iBufferLengthMax AS INTEGER     NO-UNDO.
DEFINE VARIABLE iInOutLengthMax  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iKeyWordCase     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLeftLengthMax   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLine            AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNameLengthMax   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iseq             AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWord            AS INTEGER     NO-UNDO.
DEFINE VARIABLE lCRAtTheEnd      AS LOGICAL     NO-UNDO.

DEFINE BUFFER ttAlignParam FOR ttAlignParam.

DEFINE QUERY qttAlignParam FOR ttAlignParam.

EMPTY TEMP-TABLE ttAlignParam.

lCRAtTheEnd = pcTxt MATCHES "*~n".

lineLoop:
REPEAT iLine = 1 TO NUM-ENTRIES(pcTxt, "~n")
 ON ERROR UNDO, RETURN:
    cLine = ENTRY(iLine, pcTxt, "~n").
    IF iLine = 1 THEN cTabIdent = FILL(" ", LENGTH(cLine) - LENGTH(LEFT-TRIM(cLine))).
    cLine = TRIM(cLine).
    IF cLine = "" THEN NEXT.

    /* keep single spaces */
    DO WHILE INDEX(cLine, "  ") > 0:
        cLine = REPLACE(cLine, "  ", " ").
    END.

    /* 19-MAR-2007 sla: protection against having a selection with variable field index definitions */
    IF  cLine BEGINS "DEFINE VARIABLE "
     OR cLine BEGINS "DEF VARIABLE "
     OR cLine BEGINS "DEF PARAMETER "
     OR cLine BEGINS "DEFINE PARAMETER "
     OR cLine BEGINS "INDEX "
     OR cLine BEGINS "FIELD "
     THEN DO:
        MESSAGE "Please do not select a mix of parameters and variable/fields/indices/buffer definitions" SKIP
         "Alignment action canceled"
          VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN ERROR.
    END.

    /* Line ready to be imported, now store the words */
    CREATE ttAlignParam.
    ASSIGN
     iseq = iseq + 1
     ttAlignParam.iseq = iseq
     iWord = 2.

    /* weird line, keep it somewhere... */
    IF     NOT (    NUM-ENTRIES(cLine, " ") >= 5
                AND cLine MATCHES "DEF* BUFFER*")
      THEN DO:
        ttAlignParam.cRightPart = cLine.
        NEXT.
    END.

    cWord = ENTRY(iWord, cLine, " ").
    ASSIGN
     ttalignparam.cDefType      = "BUFFER"
     iWord                      = 3
     ttalignparam.cBuffer       = ENTRY(iWord, cLine, " ")
     ttalignparam.iBufferLength = LENGTH(ttalignparam.cBuffer)
     iWord                      = iWord + 1.

    DO iWord = iWord TO NUM-ENTRIES(cLine, " "):
        cWord = ENTRY(iWord, cLine, " ").
        IF cWord = "FOR" THEN cWord = IF glLowerCaseCompletion THEN "for" ELSE "FOR".
        ttalignparam.cRightPart = ttalignparam.cRightPart + " " + cWord.
    END.
    ttalignparam.cRightPart = SUBSTRING(ttalignparam.cRightPart, 2). /* remove first space */
END.

FIND LAST ttAlignParam USE-INDEX iInOutLength.
iInOutLengthMax = ttAlignParam.iInOutLength.

FIND LAST ttAlignParam USE-INDEX iNameLength.
iNameLengthMax = ttAlignParam.iNameLength.

FIND LAST ttAlignParam USE-INDEX iBufferLength.
iBufferLengthMax = ttAlignParam.iBufferLength.

pcTxt = "".
OPEN QUERY qttAlignParam FOR EACH ttAlignParam BY ttAlignParam.iSeq.

REPEAT:
    GET NEXT qttAlignParam.
    IF QUERY qttAlignParam:QUERY-OFF-END THEN LEAVE.

    /* 12-MAY-2007 sla: convert 'NO-UNDO' to appropriate case
      => this is the place to handle other similar guys later
      well let's do it now */
    DO iKeyWordCase = NUM-ENTRIES(gcKeyWordsToConvertToCase) TO 1 BY -1:
        cKeyWordCase = ENTRY(iKeyWordCase, gcKeyWordsToConvertToCase).
        ttAlignParam.cRightPart = REPLACE(ttAlignParam.cRightPart
                                         ," " + cKeyWordCase
                                         ," " + IF glLowerCaseCompletion
                                                 THEN LC(cKeyWordCase)
                                                 ELSE CAPS(cKeyWordCase)).
    END.


    cLine = (IF glLowerCaseCompletion THEN "define buffer " ELSE "DEFINE BUFFER ")
            + ttalignparam.cBuffer
            + FILL(" ", iBufferLengthMax - ttAlignParam.iBufferLength) + " "
            + ttalignparam.cRightPart.
    pcTxt = pcTxt + "~n" + cTabIdent + cLine.
END.
pcTxt = SUBSTR(pcTxt,2).

IF lCRAtTheEnd THEN pcTxt = pcTxt + "~n".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE alignCode C-Win
PROCEDURE alignCode :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

DEFINE VARIABLE cFirstLine      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cText           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lDefinitionMode AS LOGICAL     NO-UNDO.

IF NOT glAlignEnabled THEN RETURN.

IF VALID-HANDLE(phEditor) AND phEditor:TEXT-SELECTED
 THEN cText = phEditor:SELECTION-TEXT.

IF cText = "" THEN RETURN.

cFirstLine = TRIM(ENTRY(1, cText, "~n")).
IF cFirstLine MATCHES "DEF* PARAMETER *" THEN DO:
    lDefinitionMode = YES.
    RUN alignParameters (INPUT-OUTPUT cText) NO-ERROR.
END.
ELSE IF cFirstLine MATCHES "DEF* BUFFER *" THEN DO:
    lDefinitionMode = YES.
    RUN alignBuffers (INPUT-OUTPUT cText) NO-ERROR.
END.
ELSE IF  glAlignVars     AND LEFT-TRIM(cText) BEGINS "DEF"
      OR glAlignTTFields AND LEFT-TRIM(cText) BEGINS "FIELD "
 THEN DO:
    lDefinitionMode = YES.
    RUN alignDefitions IN TARGET-PROCEDURE (INPUT-OUTPUT cText) NO-ERROR.
END.
ELSE IF glAlignAssign AND INDEX( ENTRY(1, cText, "~n"), " = ") > 0
      THEN RUN alignAssign IN TARGET-PROCEDURE (INPUT-OUTPUT cText)  NO-ERROR.

IF NOT ERROR-STATUS:ERROR THEN DO:
    IF   VALID-HANDLE(phEditor)
     AND phEditor:TEXT-SELECTED
      THEN DO:
        phEditor:EDIT-CLEAR().
        phEditor:INSERT-STRING(cText).
    END.
END.

IF lDefinitionMode THEN RUN loadLocalResources IN TARGET-PROCEDURE (phEditor). /* 18-DEC-2006 sla: perhaps a variable name was created or changed since the number of lines changed  */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE alignDefitions C-Win
PROCEDURE alignDefitions :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER pcTxt AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cKeyWordCase   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTabIdent      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iKeyWordCase   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLeftLengthMax AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLine          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNameLengthMax AS INTEGER     NO-UNDO.
DEFINE VARIABLE iseq           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTypeLengthMax AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWord          AS INTEGER     NO-UNDO.
DEFINE VARIABLE lCRAtTheEnd    AS LOGICAL     NO-UNDO.


DEFINE BUFFER ttAlignDef FOR ttAlignDef.

DEFINE QUERY qttAlignDef FOR ttAlignDef.
EMPTY TEMP-TABLE ttAlignDef.

lCRAtTheEnd = pcTxt MATCHES "*~n".

REPEAT iLine = 1 TO NUM-ENTRIES(pcTxt, "~n")
 ON ERROR UNDO, RETURN:
    cLine = ENTRY(iLine, pcTxt, "~n").
    IF iLine = 1 THEN cTabIdent = FILL(" ", LENGTH(cLine) - LENGTH(LEFT-TRIM(cLine))).
    cLine = TRIM(cLine).
    IF cLine = "" THEN NEXT.

    /* keep single spaces */
    DO WHILE INDEX(cLine, "  ") > 0:
        cLine = REPLACE(cLine, "  ", " ").
    END.

    /* 18-JAN-2007 sla: protection against having DEFINE TEMP-TABLE in a line */
    IF cLine MATCHES "DEF* TEMP-TABLE *" THEN DO:
        MESSAGE "Please do not select the line that defines a temp-table, but only the lines that define its fields" SKIP
         "Alignment action canceled"
          VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN ERROR.
    END.

    /* 18-JAN-2007 sla: protection against having INDEX  in a line */
    IF cLine MATCHES "INDEX *" THEN DO:
        MESSAGE "Please do not select the line that defines a temp-table INDEX, but only the lines that define the fields" SKIP
         "Alignment action canceled"
          VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN ERROR.
    END.

    /* Line ready to be imported, now store the words */
    CREATE ttAlignDef.
    iseq = iseq + 1.
    ttAlignDef.iseq = iseq.

    /* weird line, keep it somewhere... */
    IF     NOT (    NUM-ENTRIES(cLine, " ") >= 5
                AND cLine MATCHES "*DEF* VAR*")
       AND NOT (    NUM-ENTRIES(cLine, " ") >= 4
                AND cLine BEGINS "FIELD ")
      THEN DO:
        ttAlignDef.cRightPart = cLine.
        NEXT.
    END.

    /* 12-DEC-2006 sla: better handling of GLOBAL NEW and SHARED keyWord
     They will not be correclty aligned with non shared vars, but at least the keyword will not be lost
     Note that we should keep a different distinct block for each sharing type to result in correclty aligned blocks
       => in other words, align new global shared animal together, then non shared vars as a separate block
       Side note : NEW GLOBAL SHARED vars are fine (do not abuse, but fine)
        ,but simple SHARED VAR and simple NEW SHARED vars are bad  (I mena without the GLOBAL keepword) */

    IF cLine BEGINS "FIELD " THEN ASSIGN
     ttAlignDef.cDefType = "FIELD"
     iWord = 2.

    ELSE DO:
        IF cLine MATCHES "DEF* NEW GLOBAL SHARED VAR*" THEN ASSIGN
         ttAlignDef.cDefType = "NEW GLOBAL SHARED VARIABLE"
         cLine = REPLACE(cLine, " NEW GLOBAL SHARED ", " ").
        ELSE IF cLine MATCHES "DEF* NEW SHARED VAR*" THEN ASSIGN
         ttAlignDef.cDefType = "NEW SHARED VARIABLE"
         cLine = REPLACE(cLine, " NEW SHARED ", " ").
        ELSE IF cLine MATCHES "DEF* SHARED VAR*" THEN ASSIGN
         ttAlignDef.cDefType = "SHARED VARIABLE"
         cLine = REPLACE(cLine, " SHARED ", " ").
        ELSE IF cLine MATCHES "DEF* PUBLIC VAR*" THEN ASSIGN
         ttAlignDef.cDefType = "PUBLIC    VARIABLE"
         cLine = REPLACE(cLine, " PUBLIC ", " ").
        ELSE IF cLine MATCHES "DEF* PROTECTED VAR*" THEN ASSIGN
         ttAlignDef.cDefType = "PROTECTED VARIABLE"
         cLine = REPLACE(cLine, " PROTECTED ", " ").
        ELSE IF cLine MATCHES "DEF* PRIVATE VAR*" THEN ASSIGN
         ttAlignDef.cDefType = "PRIVATE   VARIABLE"
         cLine = REPLACE(cLine, " PRIVATE ", " ").
        ELSE IF cLine MATCHES "DEF* VAR*" THEN ASSIGN
         ttAlignDef.cDefType = "VARIABLE".

        iWord = 3.
    END.


    ASSIGN
     ttAlignDef.cName       = ENTRY(iWord, cLine, " ")
     iWord                  = iWord + 1 /* AS or LIKE */
     ttAlignDef.iNameLength = LENGTH(ttAlignDef.cName).

    IF ENTRY(iWord , cLine, " ") = "LIKE" THEN ASSIGN
     ttAlignDef.lLikeAs = YES.

    ASSIGN
     iWord = iWord + 1
     ttAlignDef.cDataType = ENTRY(iWord, cLine, " "). /* data type, or like-field */

    IF ttAlignDef.lLikeAs THEN ttAlignDef.iTypeLength = MINIMUM(LENGTH("LIKE " + ttAlignDef.cDataType) ,18). /* do not take too high for alignments */
    ELSE DO:
        /* Refine Data Type */
        IF ttAlignDef.cDataType BEGINS "CHAR"  THEN ttAlignDef.cDataType = "CHARACTER".
        /* 25-JAN-2007 sla: support for INT64 datatype */
        IF ttAlignDef.cDataType = "INT64" THEN ttAlignDef.cDataType = "INT64".
        ELSE IF ttAlignDef.cDataType BEGINS "INT"   THEN ttAlignDef.cDataType = "INTEGER".

        IF ttAlignDef.cDataType BEGINS "DEC"    THEN ttAlignDef.cDataType = "DECIMAL".
        IF ttAlignDef.cDataType = "DATE"        THEN ttAlignDef.cDataType = "DATE".
        IF ttAlignDef.cDataType = "DATETIME"    THEN ttAlignDef.cDataType = "DATETIME".
        IF ttAlignDef.cDataType = "DATETIME-TZ" THEN ttAlignDef.cDataType = "DATETIME-TZ".
        IF ttAlignDef.cDataType BEGINS "LOG"    THEN ttAlignDef.cDataType = "LOGICAL".
        ttAlignDef.iTypeLength = LENGTH("AS " + ttAlignDef.cDataType).
    END.

    /* 12-MAY-2007 sla: smarter way to handle the case (keep mix of caps and lc in like resources) */
    IF NOT ttAlignDef.lLikeAs
     AND glLowerCaseCompletion
     THEN ttAlignDef.cDataType = LC(ttAlignDef.cDataType).

    DO WHILE iWord < NUM-ENTRIES(cLine, " "):
        iWord = iWord + 1.
        ttAlignDef.cRightPart = ttAlignDef.cRightPart + " " + ENTRY(iWord, cLine, " ").
    END.
END.

FIND LAST ttAlignDef USE-INDEX iNameLength.
iNameLengthMax = ttAlignDef.iNameLength.

FIND LAST ttAlignDef USE-INDEX iTypeLength.
iTypeLengthMax = ttAlignDef.iTypeLength.

pcTxt = "".
IF NOT glAlignTTFieldsSort
 AND CAN-FIND(FIRST ttAlignDef WHERE ttAlignDef.cDefType = "FIELD")
 THEN OPEN QUERY qttAlignDef FOR EACH ttAlignDef BY ttAlignDef.iSeq.
ELSE OPEN QUERY qttAlignDef FOR EACH ttAlignDef BY ttAlignDef.cName.

REPEAT:
    GET NEXT qttAlignDef.
    IF QUERY qttAlignDef:QUERY-OFF-END THEN LEAVE.

    /* 12-MAY-2007 sla: convert 'NO-UNDO' to appropriate case
      => this is the place to handle other similar guys later
      well let's do it now */
    DO iKeyWordCase = NUM-ENTRIES(gcKeyWordsToConvertToCase) TO 1 BY -1:
        cKeyWordCase = ENTRY(iKeyWordCase, gcKeyWordsToConvertToCase).
        ttAlignDef.cRightPart = REPLACE(ttAlignDef.cRightPart
                                       ," " + cKeyWordCase
                                       ," " + IF glLowerCaseCompletion
                                               THEN LC(cKeyWordCase)
                                               ELSE CAPS(cKeyWordCase)).
    END.

    IF ttAlignDef.cName = ""  THEN cLine = "".
    ELSE DO:
        cLine = (IF ttAlignDef.cDefType = "FIELD" THEN "" ELSE "DEFINE ")
                /* 12-DEC-2006 sla: support of NEW GLOBAL SHARED options */
                + ttAlignDef.cDefType.
        IF glLowerCaseCompletion THEN cLine = LC(cLine).

        cLine = cLine + " " + ttAlignDef.cName
               + FILL(" ", iNameLengthMax - ttAlignDef.iNameLength)

               /* 12-MAY-2007 sla: smarter way to handle the case (keep mix of caps and lc in like resources) */
               + (IF glLowerCaseCompletion
                   THEN IF ttAlignDef.lLikeAs THEN " like " ELSE " as "
                   ELSE IF ttAlignDef.lLikeAs THEN " LIKE " ELSE " AS ")
               + ttAlignDef.cDataType + "  "
               + FILL(" ", iTypeLengthMax - ttAlignDef.iTypeLength)
               + ttAlignDef.cRightPart.

        /* 06-AUG-2007 sla: the process can remove the period at the end of some definition lines without 'NO-UNDO' (when the data type was originaly truncated) */
        IF ttAlignDef.cDefType <> "FIELD"
         AND NOT RIGHT-TRIM(cLine) MATCHES "*~~~."
         THEN cLine = RIGHT-TRIM(cLine) + ".".
    END.

    pcTxt = pcTxt + "~n" + cTabIdent + cLine.
END.
pcTxt = SUBSTR(pcTxt,2).

IF lCRAtTheEnd THEN pcTxt = pcTxt + "~n".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE alignParameters C-Win
PROCEDURE alignParameters :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER pcTxt AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cKeyWordCase     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTabIdent        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWord            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iBufferLengthMax AS INTEGER     NO-UNDO.
DEFINE VARIABLE iInOutLengthMax  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iKeyWordCase     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLeftLengthMax   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLine            AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNameLengthMax   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iseq             AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTypeLengthMax   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWord            AS INTEGER     NO-UNDO.
DEFINE VARIABLE lCRAtTheEnd      AS LOGICAL     NO-UNDO.

DEFINE BUFFER ttAlignParam FOR ttAlignParam.

DEFINE QUERY qttAlignParam FOR ttAlignParam.

EMPTY TEMP-TABLE ttAlignParam.

lCRAtTheEnd = pcTxt MATCHES "*~n".

lineLoop:
REPEAT iLine = 1 TO NUM-ENTRIES(pcTxt, "~n")
 ON ERROR UNDO, RETURN:
    cLine = ENTRY(iLine, pcTxt, "~n").
    IF iLine = 1 THEN cTabIdent = FILL(" ", LENGTH(cLine) - LENGTH(LEFT-TRIM(cLine))).
    cLine = TRIM(cLine).
    IF cLine = "" THEN NEXT.

    /* keep single spaces */
    DO WHILE INDEX(cLine, "  ") > 0:
        cLine = REPLACE(cLine, "  ", " ").
    END.

    /* 19-MAR-2007 sla: protection against having a selection with variable field index definitions */
    IF  cLine BEGINS "DEFINE VARIABLE "
     OR cLine BEGINS "DEF VARIABLE "
     OR cLine BEGINS "DEF BUFFER "
     OR cLine BEGINS "DEFINE BUFFER "
     OR cLine BEGINS "INDEX "
     OR cLine BEGINS "FIELD "
     THEN DO:
        MESSAGE "Please do not select a mix of parameters and variable/fields/indices/buffer definitions" SKIP
         "Alignment action canceled"
          VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN ERROR.
    END.

    /* Line ready to be imported, now store the words */
    CREATE ttAlignParam.
    ASSIGN
     iseq = iseq + 1
     ttAlignParam.iseq = iseq
     iWord = 2.

    /* weird line, keep it somewhere... */
    IF     NOT (    NUM-ENTRIES(cLine, " ") >= 5
                AND cLine MATCHES "DEF* PARAMETER*")
      THEN DO:
        ttAlignParam.cRightPart = cLine.
        NEXT.
    END.

    cWord = ENTRY(iWord, cLine, " ").
    IF CAN-DO("INPUT,OUTPUT,INPUT-OUTPUT,RETURN", cWord) THEN DO:
        ASSIGN
         ttAlignParam.cInOut       = cWord
         ttalignparam.iInOutLength = LENGTH(ttalignparam.cInOut)
         iWord                     = iWord + 2.

        /* non scalar type, for now, I will not try to align them */
        cWord = ENTRY(iWord, cLine, " ").
        IF CAN-DO("TABLE,TABLE-HANDLE,DATASET", cWord) THEN DO:
            ttalignparam.cDefType = cWord.
            DO iWord = 5 TO NUM-ENTRIES(cLine, " "):
                cWord = ENTRY(iWord, cLine, " ").
                ttalignparam.cRightPart = ttalignparam.cRightPart + " " + cWord.
            END.
            ttalignparam.cRightPart = SUBSTRING(ttalignparam.cRightPart, 2). /* remove first space */
            NEXT lineLoop.
        END.
        ELSE ttalignparam.cDefType = "scalar" .

        ASSIGN
         cWord                    = ENTRY(iWord, cLine, " ")
         ttAlignParam.cName       = cWord
         iWord                    = iWord + 1 /* AS or LIKE */
         ttAlignParam.iNameLength = LENGTH(ttAlignParam.cName).

        cWord = ENTRY(iWord, cLine, " ").
        IF cWord = "LIKE" THEN ASSIGN
         ttAlignParam.lLikeAs = YES.

        ASSIGN
         iWord                  = iWord + 1
         cWord                  = ENTRY(iWord, cLine, " ").
         ttAlignParam.cDataType = cWord. /* data type, or like-field */

        IF ttAlignParam.lLikeAs THEN ttAlignParam.iTypeLength = MINIMUM(LENGTH("LIKE " + ttAlignParam.cDataType) ,18). /* do not take too high for alignments */
        ELSE DO:
            /* Refine Data Type */
            IF ttAlignParam.cDataType BEGINS "CHAR"  THEN ttAlignParam.cDataType = "CHARACTER".
            /* 25-JAN-2007 sla: support for INT64 datatype */
            IF ttAlignParam.cDataType = "INT64" THEN ttAlignParam.cDataType = "INT64".
            ELSE IF ttAlignParam.cDataType BEGINS "INT"   THEN ttAlignParam.cDataType = "INTEGER".

            IF ttAlignParam.cDataType BEGINS "DEC"    THEN ttAlignParam.cDataType = "DECIMAL".
            IF ttAlignParam.cDataType = "DATE"        THEN ttAlignParam.cDataType = "DATE".
            IF ttAlignParam.cDataType = "DATETIME"    THEN ttAlignParam.cDataType = "DATETIME".
            IF ttAlignParam.cDataType = "DATETIME-TZ" THEN ttAlignParam.cDataType = "DATETIME-TZ".
            IF ttAlignParam.cDataType BEGINS "LOG"    THEN ttAlignParam.cDataType = "LOGICAL".
            ttAlignParam.iTypeLength = LENGTH("AS " + ttAlignParam.cDataType).
        END.

        /* 12-MAY-2007 sla: smarter way to handle the case (keep mix of caps and lc in like resources) */
        IF NOT ttAlignParam.lLikeAs
         AND glLowerCaseCompletion
         THEN ttAlignParam.cDataType = LC(ttAlignParam.cDataType).

        DO WHILE iWord < NUM-ENTRIES(cLine, " "):
            iWord = iWord + 1.
            ttAlignParam.cRightPart = ttAlignParam.cRightPart + " " + ENTRY(iWord, cLine, " ").
        END.
    END. /*  IF CAN-DO("INPUT,OUTPUT,INPUT-OUTPUT,RETURN", cWord) THEN DO: */
    ELSE DO: /* probably a buffer PARAMETER  */
        IF cWord = "PARAMETER" AND ENTRY(3, cLine, " ") = "BUFFER " THEN ASSIGN
         ttalignparam.cDefType      = "BUFFER"
         iWord                      = 4
         ttalignparam.cBuffer       = ENTRY(iWord, cLine, " ")
         ttalignparam.iBufferLength = LENGTH(ttalignparam.cBuffer)
         iWord                      = iWord + 1.

        DO iWord = iWord TO NUM-ENTRIES(cLine, " "):
            cWord = ENTRY(iWord, cLine, " ").
            ttalignparam.cRightPart = ttalignparam.cRightPart + " " + cWord.
        END.
        ttalignparam.cRightPart = SUBSTRING(ttalignparam.cRightPart, 2). /* remove first space */
        NEXT lineLoop.
    END.
END.

FIND LAST ttAlignParam USE-INDEX iInOutLength.
iInOutLengthMax = ttAlignParam.iInOutLength.

FIND LAST ttAlignParam USE-INDEX iTypeLength.
iTypeLengthMax = ttAlignParam.iTypeLength.

FIND LAST ttAlignParam USE-INDEX iNameLength.
iNameLengthMax = ttAlignParam.iNameLength.

FIND LAST ttAlignParam USE-INDEX iBufferLength.
iBufferLengthMax = ttAlignParam.iBufferLength.

pcTxt = "".
OPEN QUERY qttAlignParam FOR EACH ttAlignParam BY ttAlignParam.iSeq.

REPEAT:
    GET NEXT qttAlignParam.
    IF QUERY qttAlignParam:QUERY-OFF-END THEN LEAVE.

    /* 12-MAY-2007 sla: convert 'NO-UNDO' to appropriate case
      => this is the place to handle other similar guys later
      well let's do it now */
    DO iKeyWordCase = NUM-ENTRIES(gcKeyWordsToConvertToCase) TO 1 BY -1:
        cKeyWordCase = ENTRY(iKeyWordCase, gcKeyWordsToConvertToCase).
        ttAlignParam.cRightPart = REPLACE(ttAlignParam.cRightPart
                                         ," " + cKeyWordCase
                                         ," " + IF glLowerCaseCompletion
                                                 THEN LC(cKeyWordCase)
                                                 ELSE CAPS(cKeyWordCase)).
    END.


    cLine = "DEFINE ".
    IF ttAlignParam.cInOut = "" THEN DO:
        IF ttAlignParam.cDefType = "BUFFER" THEN ASSIGN
         cLine = cLine + "PARAMETER BUFFER "
                + ttalignparam.cBuffer + FILL(" ", iBufferLengthMax - ttAlignParam.iBufferLength) + " ".

        IF glLowerCaseCompletion THEN cLine = LC(cLine).
        cLine = cLine + ttalignparam.cRightPart.
    END.
    ELSE DO:
        cLine = cLine + ttalignparam.cInOut.
        cLine = cLine + FILL(" ", iInOutLengthMax - ttAlignParam.iInOutLength)
                + " PARAMETER ".

        IF glLowerCaseCompletion THEN cLine = LC(cLine).

        IF ttAlignParam.cName = "" THEN cLine = cLine + ttalignparam.cDefType + " " + ttalignparam.cRightPart.
        ELSE cLine = cLine + ttAlignParam.cName
                    + FILL(" ", iNameLengthMax - ttAlignParam.iNameLength)
                    /* 12-MAY-2007 sla: smarter way to handle the case (keep mix of caps and lc in like resources) */
                    + (IF glLowerCaseCompletion
                        THEN IF ttAlignParam.lLikeAs THEN " like " ELSE " as "
                        ELSE IF ttAlignParam.lLikeAs THEN " LIKE " ELSE " AS ")
                    + ttAlignParam.cDataType + "  "
                    + FILL(" ", iTypeLengthMax - ttAlignParam.iTypeLength)
                    + ttAlignParam.cRightPart.
    END.
    pcTxt = pcTxt + "~n" + cTabIdent + cLine.
END.
pcTxt = SUBSTR(pcTxt,2).

IF lCRAtTheEnd THEN pcTxt = pcTxt + "~n".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE altCPressed C-Win
PROCEDURE altCPressed :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

IF NOT glHideAllComments THEN RETURN.

IF DECIMAL(ETIME) < gdLastAltCEtime + 500 THEN
 phEditor:SOURCE-COMMAND('show-all', '').
ELSE phEditor:SOURCE-COMMAND('hide-all-comments', '').

gdLastAltCEtime = DECIMAL(ETIME).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE altCtrlCursorDown C-Win
PROCEDURE altCtrlCursorDown :
/* 10-MAR-2007 sla: new feature to duplicate a line like Eclipse does */
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

DEFINE VARIABLE cLine       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCursorChar AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCursorLine AS INTEGER     NO-UNDO.

IF NOT glDuplicateLineOnAltDown THEN RETURN.

iCursorLine = phEditor:CURSOR-LINE.
iCursorChar = phEditor:CURSOR-CHAR.
RUN getEditorLine (phEditor, OUTPUT cLine).

phEditor:CURSOR-CHAR = LENGTH(cLine) + 1.
phEditor:INSERT-STRING("~n" + cLine). /* insert CR plus copy of current line */
phEditor:CURSOR-LINE = iCursorLine + 1. /* go to the new line */
phEditor:CURSOR-CHAR = iCursorChar. /* restore cursor to same col */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE altCursorUpDown C-Win
PROCEDURE altCursorUpDown :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcMode   AS CHARACTER   NO-UNDO.


DEFINE VARIABLE cTextToMove AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lMoveUpAtEnd     AS LOGICAL     NO-UNDO.
IF NOT phEditor:TEXT-SELECTED THEN DO:
    phEditor:SOURCE-COMMAND('select_line', '').
    lMoveUpAtEnd = YES.  /* seems to be necessary */
END.

cTextToMove = ?.
cTextToMove = phEditor:SELECTION-TEXT NO-ERROR. /* if text > 32k then error */
IF cTextToMove = ? THEN DO:
    MESSAGE 'The selected text is too large for this operation' SKIP
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    RETURN.
END.
phEditor:EDIT-CLEAR().

CASE pcMode:
    WHEN "up"   THEN phEditor:CURSOR-LINE = phEditor:CURSOR-LINE - 1.
    WHEN "down" THEN phEditor:CURSOR-LINE = phEditor:CURSOR-LINE + 1.
END CASE.

phEditor:CURSOR-CHAR = 1.
phEditor:INSERT-STRING(cTextToMove).
IF lMoveUpAtEnd THEN phEditor:CURSOR-LINE = phEditor:CURSOR-LINE - 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE altHPressed C-Win
PROCEDURE altHPressed :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

IF NOT glHideSelection THEN RETURN.

phEditor:SOURCE-COMMAND('hide-selection', '').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE altSPressed C-Win
PROCEDURE altSPressed :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

IF NOT glHideSelection THEN RETURN.

phEditor:SOURCE-COMMAND('show-selection', '').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE analyzeXref C-Win
PROCEDURE analyzeXref :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

DEFINE VARIABLE cFileName  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hxrefanal  AS HANDLE      NO-UNDO.
DEFINE VARIABLE iXrefEtime AS DECIMAL     NO-UNDO.

IF NOT glXrefAnal THEN RETURN.

RUN getEditorFileName(phEditor, OUTPUT cFileName) NO-ERROR.
IF ERROR-STATUS:ERROR OR cFileName = ? THEN DO:
    cMon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cMon:SCREEN-VALUE + "~n can find this file on disk, aborting!".
    RETURN "invalid file".
END.

iXrefEtime = DECIMAL(ETIME).
RUN protools/abhack/xrefanalcodeviewer.w PERSISTENT SET hxrefanal.
RUN setEditorFont IN hxrefanal (phEditor:FONT).
RUN analyze IN hxrefanal(cFileName).
iXrefEtime = DECIMAL(ETIME) - iXrefEtime.

cMon:SCREEN-VALUE = cMon:SCREEN-VALUE + "...   Xref analysis took "  + STRING(iXrefEtime).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bedtBrowseReopen C-Win
PROCEDURE bedtBrowseReopen :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
{&OPEN-QUERY-bedtBrowse}

/* 23-OCT-2013 jcc: save list of opened file and geometry of editors */
RUN rememberOpenedFiles.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bedtBrowseSynch C-Win
PROCEDURE bedtBrowseSynch :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

DEFINE VARIABLE iFocusedRow AS INTEGER     NO-UNDO.

DEFINE BUFFER ttEdt FOR ttEdt.

FIND ttEdt WHERE ttEdt.hEditor = phEditor NO-ERROR.
IF NOT AVAILABLE ttEdt THEN RETURN.

iFocusedRow = bedtBrowse:FOCUSED-ROW IN FRAME {&FRAME-NAME}.

RUN bedtBrowseReopen.

bedtBrowse:SET-REPOSITIONED-ROW(iFocusedRow, "CONDITIONAL").
REPOSITION bedtBrowse TO ROWID ROWID(ttEdt).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildSuggestBufferList C-Win
PROCEDURE buildSuggestBufferList :
/*------------------------------------------------------------------------------
  Purpose:     Build a list of buffer to suggest.
  Typicaly, this is fired by valueChanged when pressing space after a word like
  WHERE AVAILABLE OF AND OR RELEASE VALIDATE....
  A list of buffers is build from ttUsedBuffers up to the current line number
  The last used one are added at first, and buffers appear only once in the list.
  Then, if the previous word is OF:
    => We test each buffers to be used with the OF operator with a parent buffer to find out
     in the pcLine param.  If not, then they are removed
  At last, if the previous word is where, then move the target buffer of a FIND or FOR to first
  position, as it is likely to be the needed one
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcPreviousWord AS CHARACTER   NO-UNDO. /* I may use this one later to make it smarter depending on the keyword */
DEFINE INPUT  PARAMETER phEditor       AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER piCursorLine   AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pcLine         AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcBufferList  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cBuffer          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cChildBuffer     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEqualBuffer     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNewBufferList   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cQueryWhere      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWhereBuffer     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBuffer          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hChildBuffer     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hQuery           AS HANDLE      NO-UNDO.
DEFINE VARIABLE iEqualBufferFrom AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWhereBufferFrom AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWord            AS INTEGER     NO-UNDO.
DEFINE VARIABLE lQueryOK         AS LOGICAL     NO-UNDO.

/* If I do not reset ERROR-STATUS:ERROR (set by a create buffer/query that fails) then it can distrub the caller procedure */
&SCOPED-DEFINE RETURN DO: DELETE WIDGET-POOL.  ERROR-STATUS:ERROR = NO. RETURN.  END.

CREATE WIDGET-POOL.

DEFINE BUFFER ttSuggestBuffer FOR ttSuggestBuffer.
DEFINE BUFFER ttUsedBuffers   FOR ttUsedBuffers.

EMPTY TEMP-TABLE ttSuggestBuffer.
FOR EACH ttUsedBuffers WHERE ttUsedBuffers.iLine < piCursorLine
 BY ttUsedBuffers.iLine DESCENDING:
    FIND FIRST ttSuggestBuffer WHERE ttSuggestBuffer.cName = ttUsedBuffers.cName NO-ERROR.
    IF AVAILABLE ttSuggestBuffer THEN NEXT.
    CREATE ttSuggestBuffer.
    BUFFER-COPY ttUsedBuffers TO ttSuggestBuffer.
    VALIDATE ttSuggestBuffer. /* 02-SEP-2007 sla: flush indices */
END.

FOR EACH ttSuggestBuffer BY ttSuggestBuffer.iLine DESCENDING:
    opcBufferList = opcBufferList + "," + ttSuggestBuffer.cName.
END.
opcBufferList = SUBSTRING(opcBufferList, 2).

/*------------------------------------------------------------------------------
This block removes valid buffers (real DB or TT buffers, not with a typo) from opcBufferList
when they are not OK with OF operator for the parent buffer used in pcLine

In other words, invalid buffers are kept in the list because it is not up to
this API to correct typos ;)
------------------------------------------------------------------------------*/
IF pcPreviousWord = "OF" AND LOOKUP("OF", pcLine, " ") > 0 THEN DO:
    iWord = LOOKUP("OF", pcLine, " ").

    DO iWord = iWord - 1 TO 1 BY -1:
        cChildBuffer = ENTRY(iWord, pcLine, " ").
        IF cChildBuffer = "" THEN NEXT.
        IF CAN-DO("NO-LOCK,EXCLUSIVE-LOCK,SHARE-LOCK,EXCLUSIVE", cChildBuffer) THEN NEXT.
        LEAVE.
    END.

    IF cChildBuffer > "" THEN hChildBuffer = createTTorDBBuffer(phEditor, cChildBuffer, "").
    IF VALID-HANDLE(hChildBuffer) THEN DO:
        DO iWord = 1 TO NUM-ENTRIES(opcBufferList):
            cBuffer = ENTRY(iWord, opcBufferList).

            IF cBuffer = cChildBuffer THEN NEXT.  /* does "FIND FIRST customer OF customer" make sense ? */

            hBuffer = createTTorDBBuffer(phEditor, cBuffer, "").
            IF NOT VALID-HANDLE(hBuffer) THEN DO: /* unvalid buffer name, but keep it */
                cNewBufferList = cNewBufferList + "," + cBuffer.
                NEXT.
            END.

            CREATE QUERY hQuery.
            hQuery:SET-BUFFERS(hBuffer, hChildBuffer).
            cQueryWhere = SUBSTITUTE("FOR EACH &1 NO-LOCK, EACH &2 NO-LOCK OF &1"
                                     ,hBuffer:NAME
                                     ,hChildBuffer:NAME).
            lQueryOK = hQuery:QUERY-PREPARE(cQueryWhere) NO-ERROR.
            IF lQueryOK THEN cNewBufferList = cNewBufferList + "," + cBuffer.
            DELETE OBJECT hQuery. /* do not keep a query on hChildBuffer */

            /* 31-AUG-2007 sla: fix error when deleting temp-table buffer */
            IF hBuffer:DBNAME = "PROGRESST" THEN DELETE OBJECT hBuffer:TABLE-HANDLE.
            ELSE DELETE OBJECT hBuffer. /* /* 27-AUG-2007 sla: heal bad memory leak */ */
        END.  /*DO iWord = 1 TO NUM-ENTRIES(opcBufferList):*/

        opcBufferList = SUBSTRING(cNewBufferList, 2).

        /* 31-AUG-2007 sla: fix error when deleting temp-table buffer */
        IF hChildBuffer:DBNAME = "PROGRESST" THEN DELETE OBJECT hChildBuffer:TABLE-HANDLE.
        ELSE DELETE OBJECT hChildBuffer. /* /* 27-AUG-2007 sla: heal bad memory leak */ */
    END.   /*IF VALID-HANDLE:(hChildBuffer) */

END.  /* IF cPreviousWord = "OF" THEN DO: */


/* Move most appropriate buffer to first position if popup after 'WHERE' */
IF pcPreviousWord = "WHERE" THEN DO:
    iWhereBufferFrom = 0.
    IF pcLine MATCHES "* OF * WHERE *" THEN iWhereBufferFrom = LOOKUP("OF", pcLine, " "). /* special case for WHERE when used after OF */
    IF iWhereBufferFrom = 0 THEN iWhereBufferFrom = LOOKUP("WHERE", pcLine, " ").
    DO iWord = iWhereBufferFrom - 1 TO 1 BY -1:
        cWhereBuffer = ENTRY(iWord, pcLine, " ").
        IF cWhereBuffer = "" THEN NEXT.

        IF cWhereBuffer BEGINS "CAN-FIND(" THEN cWhereBuffer = SUBSTRING(cWhereBuffer, 10).

        IF CAN-DO("NO-LOCK,EXCLUSIVE-LOCK,EXCLUSIVE,SHARED-LOCK", cWhereBuffer)
         THEN NEXT.
        /* if already in the list, then remove so we add it in first position */
        IF LOOKUP(cWhereBuffer, opcBufferList) > 0 THEN DO:
            ENTRY(LOOKUP(cWhereBuffer, opcBufferList), opcBufferList) = "######".
            opcBufferList = REPLACE(opcBufferList, "######,", ""). /* remove the item */
            opcBufferList = REPLACE(opcBufferList, ",######", ""). /* remove the item */
            /* 26-JAN-2007 sla: for this case with possible double item */
            opcBufferList = REPLACE(opcBufferList, "######", "").  /* remove the item */
        END.
        IF opcBufferList = "" THEN opcBufferList = cWhereBuffer.
        ELSE opcBufferList = cWhereBuffer + "," + opcBufferList.
        LEAVE.
    END. /* DO iWord = iWhereBufferFrom - 1 TO 1 BY -1 */
END. /* IF cPreviousWord = "WHERE"  */

/* 25-FEB-2007 sla: something more cleaver after '=' => remove the buffer that is on the left of the equal sign */
IF pcPreviousWord = "=" THEN DO:
    iEqualBufferFrom = LOOKUP("=", pcLine, " ").
    DO iWord = iEqualBufferFrom - 1 TO 1 BY -1:
        cEqualBuffer = ENTRY(iWord, pcLine, " ").
        IF cEqualBuffer = "" THEN NEXT.

        IF      NUM-ENTRIES(cEqualBuffer, ".") = 3 THEN cEqualBuffer = ENTRY(2, cEqualBuffer, "."). /* dbName.table.field */
        ELSE IF NUM-ENTRIES(cEqualBuffer, ".") = 2 THEN cEqualBuffer = ENTRY(1, cEqualBuffer, "."). /* table.field */
        ELSE LEAVE.

        /* if already in the list, then remove so we add it in first position */
        IF LOOKUP(cEqualBuffer, opcBufferList) > 0 THEN DO:
            ENTRY(LOOKUP(cEqualBuffer, opcBufferList), opcBufferList) = "######".
            opcBufferList = REPLACE(opcBufferList, "######,", ""). /* remove the item */
            opcBufferList = REPLACE(opcBufferList, ",######", ""). /* remove the item */
            opcBufferList = REPLACE(opcBufferList, "######", "").  /* remove the item */
        END.

        /* 26-FEB-2007 sla: well, when the '=' sign is used in a where clause, we are usually
        not interested by the buffer on the left side so we might want to remove it,
        however when it is used for a simple assignment like
         myTable.fieldA = myTable.fieldB + <whatever> then we will use the left side buffer
          => a good solution is to suggest this left side buffer at the end of the list
          so we still have it, but not at the first place */
         opcBufferList = (IF opcBufferList = "" THEN "" ELSE opcBufferList + ",") + cEqualBuffer.

        LEAVE.
    END. /* DO iWord = iWhereBufferFrom - 1 TO 1 BY -1 */
END. /* IF cPreviousWord = "WHERE"  */


{&RETURN}
&UNDEFINE RETURN

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cleanupABHackTempFiles C-Win
PROCEDURE cleanupABHackTempFiles :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cFileFlag     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFileName     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFullPathName AS CHARACTER   NO-UNDO.

IF gcPID = "" THEN DO:
    MESSAGE "Serious ABHack error: attempt to clean up abhackTmp without proving the PID temp-directory" SKIP
     "cleanup canceled"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

/* this this a rather dangerous operation, so I prefer to use 'SESSION:TEMP-DIRECTORY + "abhackTmp" + gcPID' instead of gcImportDirName */
OS-DELETE VALUE(SESSION:TEMP-DIRECTORY + "abhackTmp\" + gcPID) RECURSIVE.

INPUT FROM OS-DIR (SESSION:TEMP-DIRECTORY + "abhackTmp").
/* delete old directories left there when Progress did a forced exit */
REPEAT:
    IMPORT cFileName cFullPathName cFileFlag.
    IF CAN-DO(".,..", cFileName) THEN NEXT.
    IF INDEX(cFileFlag, "D") = 0 THEN NEXT. /* take care of directories only */

    FILE-INFO:FILE-NAME = cFullPathName.

    IF FILE-INFO:FILE-MOD-DATE >= TODAY - 7 THEN NEXT.
    /* this way, we will delete a directory that has remained untouched for one week in a row
      => not that the modification date attribute should normally be updated by the OS if one
      file has been updated in the directory */
    OS-DELETE VALUE(cFullPathName) RECURSIVE.
END.
INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseProcedure C-Win
PROCEDURE CloseProcedure :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hThereWasARunningInfoWin AS HANDLE      NO-UNDO.

    PUBLISH "isAnyRunningABHackWinInfo" (OUTPUT hThereWasARunningInfoWin).
    IF VALID-HANDLE(hThereWasARunningInfoWin) THEN SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "isAnyRunningABHackWinInfo" IN THIS-PROCEDURE.

    PUBLISH "abhackExit".

    RUN cleanupABHackTempFiles.

    PUBLISH "killEdtPrintableTrigPersitProc".
    PUBLISH "abhackSectionOutlineWindowExit".
    PUBLISH "abhackFindExit".

    /* 14-MAY-2007 sla: see code in protools\abhack\openFileInAB.p (WIDGET-POOL cleanup) */
    PUBLISH "ABHackOpenFileInABCleanUp".  /* ask running instances to cleanup */

    /* 14-MAR-2007 sla: testing VALID-HANDLE(c-win) can be important when a previous instance of ABHack exited in a badly way */
    IF VALID-HANDLE(c-win) THEN RUN saveSetting IN TARGET-PROCEDURE.

    IF VALID-HANDLE(ghProtoolsButton) THEN DO:
        ghProtoolsButton:LOAD-IMAGE(REPLACE(ghProtoolsButton:IMAGE, "On.bmp", ".bmp")) NO-ERROR.
        ghProtoolsButton:TOOLTIP = gcProtoolsButtonTooltip.
    END.

    /* 02-NOV-2009 sla: better handling of cascade kill */
    IF VALID-HANDLE(gshprocEditorTriggers) THEN RUN killProcedure IN gshprocEditorTriggers.
    RUN disable_UI.

    IF CONNECTED("abhack") THEN DISCONNECT abhack. /* 21-MAR-2008 sla: it just makes more sense */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE colonPressed C-Win
PROCEDURE colonPressed :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

DEFINE VARIABLE cExtendedType      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cObjectType        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrevWord          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrimitiveMethod   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dTestCast          AS DECIMAL     INITIAL ? NO-UNDO.
DEFINE VARIABLE hEditorListProc    AS HANDLE      NO-UNDO.
DEFINE VARIABLE iCursorChar        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNumSpacesToInsert AS INTEGER     NO-UNDO.

DEFINE BUFFER ttEdt          FOR ttEdt.
DEFINE BUFFER ttVar          FOR ttVar.
DEFINE BUFFER ttgVar         FOR ttgVar.
DEFINE BUFFER ttAttr         FOR ttAttr.
DEFINE BUFFER ttFreeList     FOR ttFreeList.
DEFINE BUFFER ttGuessObjType FOR ttGuessObjType.
DEFINE BUFFER ttMethod       FOR ttMethod.
DEFINE BUFFER ttUsing        FOR ttUsing.

/* 15-APR-2007 sla: disable the NEXT value change for consistency sake (like action on dot) */
RUN disableNextValueChanged (phEditor, "colonPressed").


iCursorChar = phEditor:CURSOR-CHAR.

IF iCursorChar <= 1 THEN RETURN. /* nothing we can do if ':' is typed at the very beginning of a line */

RUN getEditorLine IN TARGET-PROCEDURE (phEditor, OUTPUT cLine).


/* 27-JAN-2007 sla: new feature to disable popup when the carret is in a quoted string */
IF glNoCompletionInStrings AND carretInQuotedString(cLine, iCursorChar) THEN RETURN.

IF chCtrlSpy:PSTimerSpy:ENABLED = NO THEN RETURN. /* 01-FEB-2008 sla: improvment so people can disable A4GBL completion in javascripts */

/* 08-SEP-2008 sla: attempt to support OO ABL multiple object chaining like objA:getSomeObjB:<here> */
/* 31-OCT-2008 sla: added doNotPassCursor and corrected usage of two options *QuotePartOfWords */
/* 01-NOV-2008 sla: problem is that the cursor-char is not incremented yet when the ':' user trigger fires (from  USER-INTERFACE-TRIGGER protools/abhack/procEditorTriggers.p (c:\v10tools\protools\abhack\procEditorTriggers)
 but IS incremented if this action is called by   CtrlDelay.PSTimerApplyEvent.Tick  */
IF NOT PROGRAM-NAME(2) BEGINS "USER-INTERFACE-TRIGGER" THEN iCursorChar = iCursorChar - 1.
RUN extractWordN IN TARGET-PROCEDURE ( 0, cLine, iCursorChar, "doNotPassCursor,doubleQuotePartOfWords,SingleQuotePartOfWords,:PartOfWords,protectOKMatchingPar", OUTPUT cPrevWord) NO-ERROR.
cPrevWord = TRIM(cPrevWord). /* 16-SEP-2008 sla: remove potential leading space */


FIND ttEdt WHERE ttEdt.hEditor = phEditor.

/* 30-OCT-2007 sla: new feature */
IF   ttEdt.isClass
 AND cPrevWord = "SUPER"
 AND glCompleteOOSuperMethod
 THEN DO:
    RUN prepareFuncMethList (phEditor
                            ,ttEdt.currentEvent + "&SuperCurrentMethod"
                            ,INPUT-OUTPUT TABLE ttFreeList).

    RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).
    RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty,trailingParenthesesMeansFunction").
    cmon:INSERT-STRING("~nSUPER:<method> for " + QUOTER(ttEdt.currentEvent)) IN FRAME {&FRAME-NAME}.
    RETURN.
END.


/* 09-SEP-2008 sla: added "THIS-OBJECT" case
Also take care of case when the previous word is a method that returns an extended type, or or attribute of extend type */
IF   ttEdt.isClass
 AND glCompleteOOSuperMethod
 AND cPrevWord = "THIS-OBJECT" THEN DO:
    RUN prepareFuncMethList (phEditor
                            ,""
                            ,INPUT-OUTPUT TABLE ttFreeList).

    RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).
    RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty,trailingParenthesesMeansFunction").
    cmon:INSERT-STRING("~nSUPER:<method> for " + QUOTER(ttEdt.currentEvent)) IN FRAME {&FRAME-NAME}.
    RETURN.
END.

/* 06-DEC-2006 sla: some regret the automatic adding of 'END.' after a DO: */
IF glAddEndAfterDo AND CAN-DO("DO,REPEAT", cPrevWord) THEN DO:
    cmon:INSERT-STRING("~nInsert END. after " + QUOTER(cPrevWord)).
    iNumSpacesToInsert = LENGTH(cLine) - LENGTH(LEFT-TRIM(cLine)).
    phEditor:INSERT-STRING(":~n"
                           + FILL(' ', iNumSpacesToInsert) + FILL(' ', giTabNumSpaces) + "~n"
                           + FILL(' ', iNumSpacesToInsert) + IF glLowerCaseCompletion THEN "end." ELSE "END.").
    phEditor:CURSOR-LINE = phEditor:CURSOR-LINE - 1.
    phEditor:CURSOR-CHAR = iNumSpacesToInsert + giTabNumSpaces + 1.
    RUN delayWindowEvent(phEditor, "deleteOneChar").
    /* 06-DEC-2006 sla: this does not work...  RETURN NO-APPLY. */
    RETURN.
END.

/* 21-APR-2007 sla: The rest of this procedure is attribute/method completion */
IF NOT glCompleteAttrOnTab THEN RETURN.

dTestCast = DECIMAL(cPrevWord) NO-ERROR.
IF LOOKUP(cPrevWord, "DO,REPEAT,TRUE,NO,FALSE,YES,TRUE") > 0 THEN RETURN.
IF INDEX(", '~"", SUBSTRING(cLine, iCursorChar - 1, 1)) > 0
 OR    SUBSTRING(cLine, iCursorChar - 1, 1) = ":"  /* 16-NOV-2007 sla: move ':' here */
   AND gcDelayTriggerWinEvent <> "colonPressedOnly"
 OR dTestCast <> ? /* numeric constant before ':' => we are probable at the end of a FOR EACH block (not attribute) */
 /* 20-AUG-2007 sla: added a few conditions to avoid attribute/method completion after ':' of a FOR block */
 THEN DO:
    cmon:INSERT-STRING("~nSpecial char or constant before ':' Popup aborted").
    RETURN.
END.

/* 16-NOV-2007 sla: moved this block here to make a special monitoring message */
IF  LEFT-TRIM(cLine) BEGINS "FOR "
 OR LEFT-TRIM(cLine) BEGINS "EACH "
 OR LEFT-TRIM(cLine) BEGINS "FIRST "
 OR LEFT-TRIM(cLine) BEGINS "LAST "
 OR REPLACE(cLine, " ", "") BEGINS ",EACH"   /* 16-NOV-2007 sla: improvement */
 OR REPLACE(cLine, " ", "") BEGINS ",FIRST"  /* 16-NOV-2007 sla: improvement */
 OR REPLACE(cLine, " ", "") BEGINS ",LAST"   /* 16-NOV-2007 sla: improvement */
 THEN DO:
    cmon:INSERT-STRING("~n':' Popup aborted because the line starts a block with FOR, EACH, FIRST etc...").
    RETURN.
END.


IF gcDelayTriggerWinEvent = "colonPressedOnly" THEN gcDelayTriggerWinEvent = "".

cmon:INSERT-STRING("~ncolon pressed after " + cPrevWord).

/* 15-AUG-2007 sla: first try to get or guess a widget or basic object type */
IF glFilterOnViewAsWidgetType THEN FIND FIRST ttgVar WHERE
     ttgVar.hEditor = phEditor
 AND ttgVar.cVar = ENTRY(1, cPrevWord, ":") NO-ERROR.
IF AVAILABLE ttgVar THEN cObjectType = ttgVar.cViewAs.
ELSE IF glFilterGuessObjectType THEN DO:
    FOR EACH ttGuessObjType WHERE cPrevWord MATCHES ttGuessObjType.cGuess
     BY ttGuessObjType.iGuessLength DESCENDING:
        cObjectType = ttGuessObjType.cObjType.
        LEAVE.
    END.
END.

/* 17-SEP-2008 sla: improvement to better support completion after buffer-field("FieldName"):<here> */
cPrimitiveMethod = ENTRY(NUM-ENTRIES(cPrevWord, ":"), cPrevWord, ":").
IF cPrimitiveMethod MATCHES "*(*)" THEN cObjectType = ENTRY(1, cPrimitiveMethod, "(").


/* 15-AUG-2007 sla: support of extended types */
IF AVAILABLE ttgVar THEN cExtendedType = ttgVar.cDataType.
/* last word for local resources */
FIND FIRST ttVar WHERE ttVar.cVar = ENTRY(1, cPrevWord, ":") NO-ERROR.
IF AVAILABLE ttVar THEN cExtendedType = ttVar.cDataType.
/* 09-SEP-2008 sla: object chaining with first item being a method that returns an extended type */
/* note for methods that are called with parameters: I need to improved the extractWordN so a pair of matching parentheses can be taken as part of words */
ELSE IF ENTRY(1, cPrevWord, ":") MATCHES "*(*)*" THEN DO:
    FIND FIRST ttMethod WHERE ttMethod.cName = ENTRY(1, cPrevWord, "(") NO-ERROR. /* ignore parameters */
    IF AVAILABLE ttMethod THEN cExtendedType = ttMethod.cReturnType.
END.
IF CAN-DO("ABHackDidNotCatch,{&BasicProgressTypes}", cExtendedType) THEN cExtendedType = "".


/* 14-OCT-2009 sla: support of static class ref */
DEFINE VARIABLE lTryStaticExtendedType AS LOGICAL     NO-UNDO.
IF cExtendedType = "" THEN ASSIGN
 lTryStaticExtendedType = YES
 cExtendedType          = ENTRY(1, cPrevWord, ":").

/* 08-SEP-2008 sla: support of OO ABL chaining */
IF INDEX(cPrevWord, ":") > 0
 /* AND cExtendedType > ""  14-OCT-2009 sla: try to catch a static reference to extended types */
 THEN DO:
    DEFINE VARIABLE cClassFileName     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cFullClassFileName AS CHARACTER   NO-UNDO.

    IF cExtendedType BEGINS "Progress.Lang." THEN ASSIGN
     cClassFileName     = cExtendedType
     cFullClassFileName = cExtendedType.
    ELSE DO:
        cClassFileName = REPLACE(cExtendedType, ".", "/") + ".cls".
        FILE-INFO:FILE-NAME = cClassFileName.
        IF FILE-INFO:FULL-PATHNAME = ?
         THEN FOR EACH ttUsing WHERE ttUsing.hEditor = phEditor:
            FILE-INFO:FILE-NAME = REPLACE(ttUsing.cUsing, ".*", "") + "/" + cClassFileName.
            IF FILE-INFO:FULL-PATHNAME = ? THEN NEXT.

            cFullClassFileName = FILE-INFO:FULL-PATHNAME.
            LEAVE.
        END.
        ELSE cFullClassFileName = FILE-INFO:FULL-PATHNAME.
    END.

    IF cFullClassFileName > "" THEN DO:
        cFullClassFileName = REPLACE(cFullClassFileName, "\", "/").
        ENTRY(1, cPrevWord, ":") = cFullClassFileName.
        RUN resolveSubtypeChain IN ghParser (cPrevWord , OUTPUT cExtendedType) NO-ERROR.
        IF ERROR-STATUS:ERROR OR RETURN-VALUE > "" THEN DO:
            cmon:INSERT-STRING("~nresolveSubtypeChain returned for " +  QUOTER(cPrevWord) + " : " + RETURN-VALUE).
            cExtendedType = "".
        END.
        ELSE IF cExtendedType > "" THEN cmon:INSERT-STRING("~nresolveSubtypeChain worked out extendedtype " + QUOTER(cExtendedType) + " for " +  QUOTER(cPrevWord)).
    END.
END.


IF cExtendedType > "" THEN /* 14-OCT-2009 sla: note we get there for OO classes refered statically now */
 extendedType:
 DO:
    DEFINE VARIABLE iPrepareClassResourceStart AS INTEGER     NO-UNDO.
    iPrepareClassResourceStart = ETIME.
    RUN prepareClassResource (phEditor, cExtendedType, lTryStaticExtendedType, OUTPUT TABLE ttFreeList).
    cmon:CURSOR-OFFSET = cmon:LENGTH.
    IF NOT CAN-FIND(FIRST ttFreeList) THEN DO:
        cmon:INSERT-STRING("~nNo Class Resource found for " +  QUOTER(cExtendedType)).
        LEAVE extendedType. /* don´t break simple attr completion */
        /* RETURN. */
    END.

    RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).
    RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty,trailingParenthesesMeansFunction").
    cmon:INSERT-STRING("~nResource prepared in " + STRING(ETIME - iPrepareClassResourceStart) + " ms"
                       + " for class " + QUOTER(cExtendedType)).
    RETURN.
END.


RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).

IF VALID-HANDLE(hEditorListProc)  /* invalid if it decided to kill itself */
 THEN RUN loadListFromTT IN hEditorListProc
  (TABLE ttAttr
  ,"addFirstEmptyItem,ABLAttributesAndMethods" + IF cObjectType = ""
                           THEN ""
                           ELSE ",objectType=" + cObjectType
  ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE compileFile C-Win
PROCEDURE compileFile :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

DEFINE VARIABLE cFileName     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCompileEtime AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cRfile        AS CHARACTER  NO-UNDO. /* 23-APR-2008 sla: a guy to delete */

IF NOT glCompile THEN RETURN.

cMon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

RUN getEditorFileName(phEditor, OUTPUT cFileName) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    cMon:SCREEN-VALUE = cMon:SCREEN-VALUE + "~n can find this file on disk, aborting!".
    RETURN "invalid file".
END.

iCompileEtime = DECIMAL(ETIME).
cRfile = cFileName.
ENTRY(NUM-ENTRIES(cRfile, "."), cRfile, ".") = "r".
IF SEARCH(cRfile) <> ? THEN OS-DELETE VALUE(cRfile).  /* 23-APR-2008 sla: a guy to delete, so we do not keep an old .r if the compilation fails */

cMon:SCREEN-VALUE = "Compile & save in process... ".

IF glCompileMinsize THEN COMPILE VALUE(cFilename) SAVE MIN-SIZE NO-ERROR.
ELSE COMPILE VALUE(cFilename) SAVE NO-ERROR.
iCompileEtime = DECIMAL(ETIME) - iCompileEtime.

cMon:SCREEN-VALUE = cMon:SCREEN-VALUE + "~nDone in " + STRING(iCompileEtime) + "ms~n"
 + IF COMPILER:ERROR-ROW = ?
    THEN "OK"
    ELSE "failed with this error" + ERROR-STATUS:GET-MESSAGE(1) + " (do shift-F2 for more details)".

/* 23-APR-2008 sla: make it more visible when a compilation fails */
IF COMPILER:ERROR-ROW <> ? THEN DO:
    DEFINE VARIABLE iFlash      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iMonBgColor AS INTEGER   NO-UNDO.
    iMonBgColor = cmon:BGCOLOR.

    DO iFlash = 1 TO 4:
        IF iFlash > 1 THEN RUN Sleep (100).
        cmon:BGCOLOR = 0.
        PROCESS EVENTS.
        RUN Sleep (100).
        cmon:BGCOLOR = iMonBgColor.
        PROCESS EVENTS.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE completeWithSimilar C-Win
PROCEDURE completeWithSimilar :
/* 15-APR-2007 sla: restructured this PROCEDURE to handle the two cases.
It will now be fired on alt-ctrl-left/right so we can use alt-left/right for another
need (find selected) */

/* 16-JAN-2007 sla: Native VSlick completion with a similar word before or after the carret */

DEFINE INPUT  PARAMETER phEditor    AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcDirection AS CHARACTER   NO-UNDO.

IF NOT glCompleteWithSimilar THEN RETURN.

CASE pcDirection:
    WHEN 'left'  THEN phEditor:SOURCE-COMMAND('complete-prev', '').
    WHEN 'right' THEN phEditor:SOURCE-COMMAND('complete-next', '').
END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE connectToAbhackDb C-Win
PROCEDURE connectToAbhackDb :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

DEFINE VARIABLE hbahdbversion          AS HANDLE    NO-UNDO.
DEFINE VARIABLE hbfabhackVersion       AS HANDLE    NO-UNDO.
DEFINE VARIABLE lErrorMessageDisplayed AS LOGICAL   NO-UNDO.

DEFINE BUFFER ttEdt FOR ttEdt.


IF CONNECTED("abhack") THEN DO:
    PUBLISH "exitAbhackDbLib".
    DISCONNECT abhack.
END.

CONNECT VALUE(gcCatDbConnection) NO-ERROR.
IF CONNECTED("abhack") THEN DO:

    CREATE WIDGET-POOL. /* 26-NOV-2008 sla: solve memory leak problem on hbahdbversion */
    CREATE BUFFER hbahdbversion FOR TABLE "ahdbversion" NO-ERROR.
    IF VALID-HANDLE(hbahdbversion) THEN DO:
        hbfabhackVersion = hbahdbversion:BUFFER-FIELD("abhackVersion").
        IF hbfabhackVersion:INITIAL <> BUFFER ttEdt:BUFFER-FIELD("cABHackDumpVersion"):INITIAL THEN DO:
            MESSAGE "Your abhack database has an obsolete version of" QUOTER(hbfabhackVersion:INITIAL)
             "(expecting " QUOTER(BUFFER ttEdt:BUFFER-FIELD("cABHackDumpVersion"):INITIAL) ")" SKIP
             "You may have to recreate a new database with latest sampleCatDbs/abhack.df file that came with the package, and refeed it with the Bulk Dump utility."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            DISCONNECT abhack.
            lErrorMessageDisplayed = YES.
        END.
    END.
    ELSE DO:
        MESSAGE "Your abhack database is probably empty.  Please load the abhack.df file"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        DISCONNECT abhack.
        lErrorMessageDisplayed = YES.
    END.

    /* sanity check OK */
    IF lErrorMessageDisplayed = NO THEN DO:
        PUBLISH "launchAbhackDbLib".
        gcCatDbConnection:BGCOLOR IN FRAME fMisc = 10.
        RETURN.
    END.

END.

gcCatDbConnection:BGCOLOR = 12.

IF gcGlobalResCat = "DB" THEN DO:
    gcGlobalResCat = "disabled".
    DISPLAY gcGlobalResCat WITH FRAME fMisc.
END.

IF lErrorMessageDisplayed = NO THEN
 MESSAGE "Failed to connect to abhack.db with" QUOTER(gcCatDbConnection) SKIP
  "If you just started ABHack, then it probably means you want to use a proserved database that is not up and running." SKIP(2)
  "Progress Connection Errors:" SKIP
  ERROR-STATUS:GET-MESSAGE(1) SKIP /* this one should be enough */
  ERROR-STATUS:GET-MESSAGE(2) SKIP /* not necessary, but does not fail if ERROR-STATUS:NUM-MESSAGES = 1 */
  ERROR-STATUS:GET-MESSAGE(3)      /* same as above */
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

RETURN ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the
               OCXs in the interface.
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "ABHackWin.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlDelay = CtrlDelay:COM-HANDLE
    UIB_S = chCtrlDelay:LoadControls( OCXFile, "CtrlDelay":U)
    CtrlDelay:NAME = "CtrlDelay":U
    chCtrlEdtValueChanged = CtrlEdtValueChanged:COM-HANDLE
    UIB_S = chCtrlEdtValueChanged:LoadControls( OCXFile, "CtrlEdtValueChanged":U)
    CtrlEdtValueChanged:NAME = "CtrlEdtValueChanged":U
    chCtrlSpy = CtrlSpy:COM-HANDLE
    UIB_S = chCtrlSpy:LoadControls( OCXFile, "CtrlSpy":U)
    CtrlSpy:NAME = "CtrlSpy":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "ABHackWin.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyUISVBelowMouse C-Win
PROCEDURE copyUISVBelowMouse :
/*------------------------------------------------------------------------------
  Purpose:     Copy in CLIPBOARD-VALUE the screen-value of a widget below the mouse
               pointer.  Very handy
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hActiveWindowToRestore AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hWidget                AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iColor1                AS INTEGER   NO-UNDO INITIAL 9. /* bleue */
    DEFINE VARIABLE iColor2                AS INTEGER   NO-UNDO INITIAL 15. /* white */
    DEFINE VARIABLE iColor3                AS INTEGER   NO-UNDO INITIAL 12. /* red */
    DEFINE VARIABLE iOrigColor             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iToForceRefreshPlease  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lSpyingTimerEnabled    AS LOGICAL     NO-UNDO.

    /* the following widget types have a move interesting label than screen-value
     they may even not support color changes so I will make them blink */
    &SCOPED-DEFINE labelWidgets TOGGLE-BOX,BUTTON

    hActiveWindowToRestore = ActiveWindow:GetHandle().

    cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME, "hh:mm:ss") + " copyUISVBelowMouse initiated".

    lSpyingTimerEnabled = chCtrlSpy:PSTimerSpy:ENABLED.
    chCtrlSpy:PSTimerSpy:ENABLED = NO. /* do not let the spying timer do any job during our flashing  */
    RUN GetInputWidgetInXY (ActiveWindow:GetHandle():HANDLE, OUTPUT hWidget).

    IF NOT VALID-HANDLE(hWidget) THEN DO:
        cmon:SCREEN-VALUE = STRING(TIME, "hh:mm:ss") + " copyUISVBelowMouse couldn't find any valid widget below the mouse pointer"
         + " in window " + QUOTER(ActiveWindow:GetHandle():TITLE).

        chCtrlSpy:PSTimerSpy:ENABLED = lSpyingTimerEnabled. /* restor spying timer enabled state */
        RETURN .
    END.

    cmon:SCREEN-VALUE = STRING(TIME, "hh:mm:ss") + " copyUISVBelowMouse found widget "
     + QUOTER(hWidget) + " (type:" + hWidget:TYPE
     + (IF CAN-QUERY(hWidget, "name") THEN " name:" + QUOTER(hWidget:NAME) ELSE "")
     + ")".

    IF NO THEN MESSAGE hWidget:NAME hWidget:TYPE hWidget:SCREEN-VALUE
     VIEW-AS ALERT-BOX INFO BUTTONS OK.

    iOrigColor = hWidget:BGCOLOR.
    IF CAN-DO("{&labelWidgets}", hWidget:TYPE) THEN hWidget:VISIBLE = NOT hWidget:VISIBLE.
    ELSE hWidget:BGCOLOR = iColor1.

    /* trick to force a screen repaint on Windows TSE */
    SESSION:SET-WAIT-STATE("general").
    DO iToForceRefreshPlease = 1 TO 20:
        WAIT-FOR 'U9' OF THIS-PROCEDURE PAUSE 0.
    END.

    RUN Sleep (40).
    IF CAN-DO("{&labelWidgets}", hWidget:TYPE) THEN hWidget:VISIBLE = NOT hWidget:VISIBLE.
    ELSE hWidget:BGCOLOR = iColor2.

    SESSION:SET-WAIT-STATE("").
    DO iToForceRefreshPlease = 1 TO 20:
        WAIT-FOR 'U9' OF THIS-PROCEDURE PAUSE 0.
    END.

    RUN Sleep (40).
    IF CAN-DO("{&labelWidgets}", hWidget:TYPE) THEN hWidget:VISIBLE = NOT hWidget:VISIBLE.
    ELSE hWidget:BGCOLOR = iColor3.

    SESSION:SET-WAIT-STATE("general").
    DO iToForceRefreshPlease = 1 TO 20:
        WAIT-FOR 'U9' OF THIS-PROCEDURE PAUSE 0.
    END.

    RUN Sleep (40).
    IF CAN-DO("{&labelWidgets}", hWidget:TYPE) THEN hWidget:VISIBLE = NOT hWidget:VISIBLE.
    ELSE hWidget:BGCOLOR = iOrigColor.

    SESSION:SET-WAIT-STATE("").
    DO iToForceRefreshPlease = 1 TO 10:
        WAIT-FOR 'U9' OF THIS-PROCEDURE PAUSE 0.
    END.

    /* do it smarter for list-item-pairs widgets */
    IF CAN-QUERY(hWidget, "LIST-ITEM-PAIRS") THEN DO:
        DEFINE VARIABLE clip   AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE ilip   AS INTEGER    NO-UNDO.
        DEFINE VARIABLE cLabel AS CHARACTER  NO-UNDO.
        clip = hWidget:LIST-ITEM-PAIRS.
        DO ilip = hWidget:NUM-ITEMS TO 1 BY -1:
            /* beware about leading spaces => use TRIM for each item */
            IF TRIM(hWidget:ENTRY(ilip)) <> hWidget:SCREEN-VALUE THEN NEXT.
            CLIPBOARD:VALUE = ENTRY( ilip * 2 - 1, cLip, hWidget:DELIMITER).
            LEAVE.
        END.
    END.
    ELSE IF CAN-DO("{&labelWidgets}", hWidget:TYPE)
          THEN CLIPBOARD:VALUE = hWidget:LABEL.
    ELSE CLIPBOARD:VALUE = hWidget:SCREEN-VALUE.

    /* 03-APR-2007 sla: seems to be required otherwise the active-window points to the ABHack Window! */
    APPLY 'ENTRY' TO hActiveWindowToRestore.
    chCtrlSpy:PSTimerSpy:ENABLED = lSpyingTimerEnabled. /* restor spying timer enabled state */

    &UNDEFINE labelWidgets
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createABHackPic C-Win
PROCEDURE createABHackPic :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER hABWin AS HANDLE      NO-UNDO.

DEFINE VARIABLE hActionFrame AS HANDLE      NO-UNDO.
DEFINE VARIABLE iTextWidth AS INTEGER     NO-UNDO.

hActionFrame = hABWin:FIRST-CHILD.
DO WHILE hActionFrame <> ?:
    IF hActionFrame:NAME = "action_icons" THEN LEAVE.
    hActionFrame = hActionFrame:NEXT-SIBLING.
END.

IF NOT VALID-HANDLE(hActionFrame) THEN RETURN.

CREATE IMAGE ghABHackedText ASSIGN
 FRAME         = hActionFrame
 WIDTH-PIXELS  = 36
 HEIGHT-PIXELS = 18
 Y             = 5
 X             = hActionFrame:WIDTH-PIXELS - 37
 SENSITIVE     = YES
 VISIBLE       = YES
 TRANSPARENT   = YES
 TOOLTIP       = "Click => Hide/Show the ABHack Ctrl window.  Animated when spying is active"
 TRIGGERS:
    ON "LEFT-MOUSE-CLICK" PERSISTENT RUN ABFancyGuyChoose IN THIS-PROCEDURE.
 END TRIGGERS.
ghABHackedText:LOAD-IMAGE("protools/abhack/pics/babyTux.bmp").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createABHackText C-Win
PROCEDURE createABHackText :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER hABWin AS HANDLE      NO-UNDO.

DEFINE VARIABLE hActionFrame AS HANDLE      NO-UNDO.
DEFINE VARIABLE iTextWidth AS INTEGER     NO-UNDO.

hActionFrame = hABWin:FIRST-CHILD.
DO WHILE hActionFrame <> ?:
    IF hActionFrame:NAME = "action_icons" THEN LEAVE.
    hActionFrame = hActionFrame:NEXT-SIBLING.
END.

IF NOT VALID-HANDLE(hActionFrame) THEN RETURN.

iTextWidth = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(" ABHacked") + 2.

CREATE TEXT ghABHackedText ASSIGN
 FRAME         = hActionFrame
 WIDTH-PIXELS  = iTextWidth
 HEIGHT-PIXELS = 19
 Y             = 5
 X             = hActionFrame:WIDTH-PIXELS - iTextWidth - 1
 FORMAT        = "XXXXXXXXXXXX"
 SCREEN-VALUE  = " ABHacked"
 SENSITIVE     = YES
 VISIBLE       = YES
 TOOLTIP       = "Click => Hide/Show the ABHack Ctrl window.  Slow blinking means spying is active~n"
   + "The Dark Green color when ABHack knows global resources for the current procedure editor"
 TRIGGERS:
    ON "LEFT-MOUSE-CLICK" PERSISTENT RUN ABFancyGuyChoose IN THIS-PROCEDURE.
 END TRIGGERS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createCustomTabCompOfMineFile C-Win
PROCEDURE createCustomTabCompOfMineFile :
/*------------------------------------------------------------------------------
  Purpose:
<extractOf 30-MAR-2007 release notes>
Removed the protools/abhack/customTabCompletion.ofMine.txt file from the package to make sure it does

not overrides yours.  ABHack will create this file from scratch if it finds out it does not exist.  I

will not rely on copying a template file because yet another template file could bring extra

confusion.
</extractOf 30-MAR-2007 release notes>
------------------------------------------------------------------------------*/
FILE-INFO:FILE-NAME = "protools/abhack".
IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN. /* can't help' */

OUTPUT TO VALUE(FILE-INFO:FULL-PATHNAME + "/customTabCompletion.ofMine.txt").
IF ERROR-STATUS:ERROR THEN RETURN.

PUT UNFORMATTED "# protools/abhack/customTabCompletion.ofMine.txt is a list of custom aliases to extend the ~n" +
"# standard aliases defined in protools/abhack/customTabCompletion.txt ~n" +
"# ~n" +
"# This new file introduced on 24-MAR-2007 will help to keep your own aliases when ~n" +
"# applying a new release of ABHack with possible changes in the standard ~n" +
"# protools/abhack/customTabCompletion.txt ~n" +
"# ~n" +
"# ~n" +
"# Lines that begins with '#' are ignored ~n" +
"# This file is reloaded each time you recheck the '&Custom completion' toggle-box ~n" +
"# One can define aliases for the matches operator ~n" +
"# ~~n => insert carriage return ~n" +
"# %\d  => date in your *own* format (European, or American) => better than with VSlick ~n" +
"# %\c put the cursor there  /* 18-DEC-2006 sla: Improvement: %\c now work for aliases that insert multiple lines */ ~n" +
"# %\( will make ABHack insert a '(' in such a way it can guess where to put the matching ')' ~n" +
"# ttEdt => to be replaced by the next chosen buffer in a popup list  /* 17-FEB-2007 sla: new buffer directive */ ~n" +
"# ~n" +
"# Example to disable a standard alias ~n" +
"lirtn @dislabledAlias@ ~n" +
"# Example to abort the completion process for a given word ~n" +
"blockCompletion @abortCompletionProcess@ ~n" +
"# ~n" +
"# To implement your own, have a look at the standard protools/abhack/customTabCompletion.txt ~n" +
"# ~n" +
" ~n" +
" ~n" +
" ~n" +
" ~n" +
"# Do not forget to put a carriage return at the end of the file".


OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTTBuffer C-Win
PROCEDURE createTTBuffer :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER tttt FOR tttt.
DEFINE INPUT  PARAMETER pcTableName AS CHARACTER   NO-UNDO.  /* might differ from tttt.cttname when using an additional Buffers against a TT  */
DEFINE INPUT  PARAMETER pcOptn      AS CHARACTER   NO-UNDO.  /* 03-MAR-2007 sla:  ability to ignore indices, such as for field list pop up */
DEFINE OUTPUT PARAMETER ophBuffer   AS HANDLE      NO-UNDO.

DEFINE VARIABLE cFieldType    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cIdxAscDesFld AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cIdxFld       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTTInfo       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hTempTable    AS HANDLE      NO-UNDO.
DEFINE VARIABLE iIdx          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxIdx       AS INTEGER     NO-UNDO.
DEFINE VARIABLE lMthRtn       AS LOGICAL     INITIAL YES NO-UNDO.

DEFINE BUFFER ttfld FOR ttfld.
DEFINE BUFFER ttIdx FOR ttIdx.

cTTInfo = "Temp-Table:" + tttt.cttname.

CREATE TEMP-TABLE hTempTable.
FOR EACH ttfld WHERE ttfld.ittid = tttt.ittid:
    cTTInfo = cTTInfo + "~n  Field: " + ttfld.cfldname + " AS " + ttfld.cDataType.
    cFieldType = ttfld.cDataType.
    IF cFieldType = "CLASS" THEN cFieldType = "progress.lang.object".
    hTempTable:ADD-NEW-FIELD(ttfld.cfldname, cFieldType). /* 05-FEB-2007 sla: now use the data-type we kept track of */
END.

/* 03-FEB-2007 sla: Support of TT indices.... let's be crazy */
IF NOT CAN-DO(pcOptn, "ignoreIndices") THEN /* 03-MAR-2007 sla: help to handle the rowObject temp-table in smart Data viewers */
ttIdxLoop:
FOR EACH ttIdx WHERE ttIdx.ittid = tttt.ittid:
    cTTInfo = cTTInfo + "~n  Index: " + ttIdx.cidxname + " Optn: " + ttIdx.coptn.
    lMthRtn = hTempTable:ADD-NEW-INDEX(ttIdx.cidxname
                                      ,CAN-DO(ttIdx.coptn, "UNIQUE")
                                      ,CAN-DO(ttIdx.coptn, "PRIMARY")
                                      ,CAN-DO(ttIdx.coptn, "WORD-INDEX")) NO-ERROR.

    IF NOT lMthRtn THEN LEAVE ttIdxLoop.

    cTTInfo = cTTInfo + "~n   IndexFields:".
    iMaxIdx = NUM-ENTRIES(ttIdx.cFieldsInfo).
    DO iIdx = 1 TO iMaxIdx BY 2:
        cTTInfo =  cTTInfo + "~n    " + ENTRY(iIdx    , ttIdx.cFieldsInfo)
                  + "    " + ENTRY(iIdx + 1, ttIdx.cFieldsInfo).
        lMthRtn = hTempTable:ADD-INDEX-FIELD(ttIdx.cidxname
                                            ,ENTRY(iIdx    , ttIdx.cFieldsInfo)
                                            ,ENTRY(iIdx + 1, ttIdx.cFieldsInfo) ) NO-ERROR.
        IF NOT lMthRtn THEN LEAVE ttIdxLoop.
    END.
    IF iMaxIdx = 0 THEN cTTInfo =  cTTInfo + "~n    NO FIELD CAUGHT FOR THIS INDEX!!".
END.

/* will be flase if we exited the loop with an error */
IF lMthRtn THEN lMthRtn = hTempTable:TEMP-TABLE-PREPARE(pcTableName) NO-ERROR.
IF NOT lMthRtn THEN DO:
    cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cmon:SCREEN-VALUE
     + STRING(TIME, "hh:mm:ss") + "  PROBLEM TO CREATE A DYN TT"
     + "  Check how ABHack could catch its definition: " + cTTInfo
     + "~nThe TEMP-TABLE-PREPARE() method failed with this errors:~n"
     + abReturnValue().
    DELETE OBJECT hTempTable.
    RETURN.
END.
ophBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ctrlAltI C-Win
PROCEDURE ctrlAltI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

IF NOT glShowLineNumbers THEN RETURN.

phEditor:SOURCE-COMMAND('view-line-numbers-toggle', '').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ctrlAltP C-Win
PROCEDURE ctrlAltP :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:       18-JAN-2008 sla: removed the Show-procs feature that was rather funny but useless
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

IF NOT glShowSpecialChars THEN RETURN.


/* 18-JAN-2008 sla: Many thanks to Jan Keirse for sharing this trick with us */
RUN vsSetDefaultOptionZ (INPUT {&VSOPTIONZ_SPECIAL_CHAR_XLAT_TAB},
                         INPUT CHR(182) + CHR(32) + CHR(149) + CHR(149) + CHR(149) + CHR(149) /* EOLCH1, EOLCH2, TAB(9), SPACE(32), VIRTUAL TAB SPACE, and EOF(26) */
                        ).

phEditor:SOURCE-COMMAND('view-spaces-toggle', '').
phEditor:SOURCE-COMMAND('view-nlchars-toggle', '').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtrlBackspacePressed C-Win
PROCEDURE CtrlBackspacePressed :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME, "hh:mm:ss") + " Ctrl-Backspace pressed".

/* we will do the job in the trigger .p */
RETURN STRING(glCtrlBackspacelDelPrevWord).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ctrlCursorLeft C-Win
PROCEDURE ctrlCursorLeft :
/*------------------------------------------------------------------------------
  Purpose:     Improved version to navigate by words:
                1) it does not ignore words made of digits
                2) goes to beginning of line before going to prev word at prev line
                3) it considers '_' as part of words
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

DEFINE VARIABLE cLine           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCursorChar     AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNewPos         AS INTEGER    NO-UNDO.

IF NOT glImprovedCtrlKeftRight THEN DO:
    phEditor:SOURCE-COMMAND('prev-word', '').
    RETURN.
END.

cMon:SCREEN-VALUE IN FRAME fMain = STRING(TIME, "hh:mm:ss") + " improved prev-word".

iCursorChar = phEditor:CURSOR-CHAR - 1.
IF iCursorChar = 0 THEN DO:
    DO WHILE cLine = "" AND phEditor:CURSOR-LINE > 1:
        phEditor:CURSOR-LINE = phEditor:CURSOR-LINE - 1.
        RUN getEditorLine IN TARGET-PROCEDURE (phEditor, OUTPUT cLine).
        cLine = RIGHT-TRIM(cLine).
    END.
    phEditor:SOURCE-COMMAND('end-line', '').
    RETURN.
END.

RUN getEditorLine IN TARGET-PROCEDURE (phEditor, OUTPUT cLine).
cLine = RIGHT-TRIM(cLine).

/* little expection when the cursor is in a large blank */
IF   iCursorChar >= 1
 AND SUBSTRING(cLine, iCursorChar, 1) = " " THEN DO WHILE TRUE:
    iCursorChar = iCursorChar - 1.
    IF  iCursorChar = 0
     OR SUBSTRING(cLine, iCursorChar, 1) <> " " THEN LEAVE.
END.
IF iCursorChar <= 0 THEN DO:
    phEditor:CURSOR-CHAR = 1.
    RETURN.
END.

iCursorChar = iCursorChar - 1.
IF iCursorChar = 0 THEN iNewPos = 1.  /* 18-JAN-2007 sla: Thanks to Jan Keirse to report this little bug*/
ELSE DO:
    cMon:CURSOR-OFFSET = cMon:LENGTH + 1.
    cMon:INSERT-STRING(STRING(iCursorChar)).

    iNewPos = MAXIMUM( R-INDEX(cLine, " ", iCursorChar)
                      ,R-INDEX(cLine, ",", iCursorChar)
                      ,R-INDEX(cLine, ".", iCursorChar)
                      ,R-INDEX(cLine, "(", iCursorChar)
                      ,R-INDEX(cLine, "~{", iCursorChar)
                      ,R-INDEX(cLine, ":", iCursorChar)
                      ,R-INDEX(cLine, "'", iCursorChar)
                      ,R-INDEX(cLine, '"', iCursorChar)).

    cMon:INSERT-STRING(" " + STRING(iNewPos)).

    /* once I have found a the right break, then put the cursor right after it */
    iNewPos = iNewPos + 1.
END.

phEditor:CURSOR-CHAR = iNewPos.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ctrlCursorRight C-Win
PROCEDURE ctrlCursorRight :
/*------------------------------------------------------------------------------
  Purpose:     Improved version to navigate by words:
                1) it does not ignore words made of digits
                2) goes to end of line before going to next word at next line
                3) it considers '_' as part of words
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

DEFINE VARIABLE cLine       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCursorChar AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNewPos     AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTestNewPos AS INTEGER    NO-UNDO.

IF NOT glImprovedCtrlKeftRight THEN DO:
    phEditor:SOURCE-COMMAND('next-word', '').
    RETURN.
END.

cMon:SCREEN-VALUE IN FRAME fMain = STRING(TIME, "hh:mm:ss") + " improved next-word".

iCursorChar = phEditor:CURSOR-CHAR.
RUN getEditorLine IN TARGET-PROCEDURE (phEditor, OUTPUT cLine).
cLine = RIGHT-TRIM(cLine).
IF iCursorChar > LENGTH(cLine) THEN DO:
    DO WHILE phEditor:CURSOR-LINE < phEditor:NUM-LINES:
        phEditor:CURSOR-LINE = phEditor:CURSOR-LINE + 1.
        RUN getEditorLine IN TARGET-PROCEDURE (phEditor, OUTPUT cLine).
        cLine = RIGHT-TRIM(cLine).
        IF cLine <> "" THEN LEAVE.
    END.
    phEditor:CURSOR-CHAR = 1.
    RETURN.
END.

iNewPos = 31000.

/* Start searching from next char */
iCursorChar = iCursorChar +  1.

iTestNewPos = INDEX(cLine, " ", iCursorChar).
IF iTestNewPos <> 0 THEN iNewPos = MINIMUM(iNewPos, iTestNewPos).

iTestNewPos = INDEX(cLine, ",", iCursorChar).
IF iTestNewPos <> 0 THEN iNewPos = MINIMUM(iNewPos, iTestNewPos).

iTestNewPos = INDEX(cLine, ".", iCursorChar).
IF iTestNewPos <> 0 THEN iNewPos = MINIMUM(iNewPos, iTestNewPos).

iTestNewPos = INDEX(cLine, "(", iCursorChar).
IF iTestNewPos <> 0 THEN iNewPos = MINIMUM(iNewPos, iTestNewPos).

iTestNewPos = INDEX(cLine, ")", iCursorChar).
IF iTestNewPos <> 0 THEN iNewPos = MINIMUM(iNewPos, iTestNewPos).

iTestNewPos = INDEX(cLine, "~{", iCursorChar).
IF iTestNewPos <> 0 THEN iNewPos = MINIMUM(iNewPos, iTestNewPos).

iTestNewPos = INDEX(cLine, ":", iCursorChar).
IF iTestNewPos <> 0 THEN iNewPos = MINIMUM(iNewPos, iTestNewPos).

iTestNewPos = INDEX(cLine, "'", iCursorChar).
IF iTestNewPos <> 0 THEN iNewPos = MINIMUM(iNewPos, iTestNewPos).

iTestNewPos = INDEX(cLine, '"', iCursorChar).
IF iTestNewPos <> 0 THEN iNewPos = MINIMUM(iNewPos, iTestNewPos).

IF iNewPos = 31000 THEN DO:
    phEditor:SOURCE-COMMAND('end-line', '').
    RETURN.
END.

/* now put the cursor at the beginning of the next word */
iNewPos = iNewPos +  1.

/* little expection when multiple spaces... */
IF   iNewPos < LENGTH(cLine)
 AND SUBSTRING(cLine, iNewPos, 1) = " " THEN DO WHILE TRUE:
    iNewPos = iNewPos + 1.
    IF iNewPos > LENGTH(cLine) THEN LEAVE. /* should never happen with earlier test, but I do not like infinite loops */
    IF SUBSTRING(cLine, iNewPos, 1) <> " " THEN LEAVE.
END.

phEditor:CURSOR-CHAR = iNewPos.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtrlDeletePressed C-Win
PROCEDURE CtrlDeletePressed :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME, "hh:mm:ss") + " Ctrl-Delete pressed".

/* we will do the job in the trigger .p */
RETURN STRING(glCtrlDelDelsWord).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtrlF3Pressed C-Win
PROCEDURE CtrlF3Pressed :
/*------------------------------------------------------------------------------
  Purpose:     Open a new procedure as well as in a Section Editor Window
------------------------------------------------------------------------------*/

IF NOT glCtrlF3NewProcWin THEN RETURN.

RUN adecomm/_pwmain.p ("", "", "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtrlFPressed C-Win
PROCEDURE CtrlFPressed :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

DEFINE VARIABLE cText        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hWinMenuFind AS HANDLE      NO-UNDO.

IF glAbhackFind THEN DO:
    IF phEditor:TEXT-SELECTED THEN DO:
        cText = phEditor:SELECTION-TEXT NO-ERROR.  /* perhaps too large */
        IF LENGTH(cText) > 100 THEN cText = "".
        IF INDEX(cText, CHR(10)) > 0 THEN cText = "".
    END.
    PUBLISH "abhackFindEntry" (cText).
    IF FOCUS <> phEditor THEN RETURN. /* was success */
END.

/* something broken (hte abhack find window is no longer there => repair it */
APPLY 'VALUE-CHANGED' TO glAbhackFind IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ctrlM C-Win
PROCEDURE ctrlM :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

DEFINE VARIABLE hWin AS HANDLE      NO-UNDO.
hWin = phEditor:WINDOW.

IF hWin:WINDOW-STATE = WINDOW-MAXIMIZED THEN DO:
    hwin:WINDOW-STATE = WINDOW-NORMAL.
    APPLY 'WINDOW-RESIZED' TO hWin.
END.
ELSE IF hWin:WINDOW-STATE = WINDOW-NORMAL THEN DO:
    IF glAbhackFind THEN hWin:MAX-HEIGHT-PIXELS = SESSION:WORK-AREA-HEIGHT-PIXELS - 22 - 50 /* let some space for the abhack finder */.
    IF gcSectionOutlineMode <> "disabled"
     AND VALID-HANDLE(ghSectionLayoutWin) /* 14-NOV-2007 sla: adding this to avoid ERROR 3137 when the section layout window has been closed */
     THEN hWin:MAX-WIDTH-PIXELS = IF gcSectionOutlineMode = "free"
                                   THEN (IF ghSectionLayoutWin:CURRENT-WINDOW:X < SESSION:WORK-AREA-WIDTH-PIXELS / 2  /* 06-JUL-2007 sla: handle situations where the sectionWindow is on the right and the editor on its left */
                                          THEN SESSION:WORK-AREA-WIDTH-PIXELS - giSectionLayoutWinWidth - 7
                                              /* 20-OCT-2008 sla: fix problem with window:X that does not work very well when the task bar is located on the side (with the ELSE 0) */
                                              -  (IF SESSION:WORK-AREA-WIDTH-PIXELS = SESSION:WIDTH-PIXELS THEN ghSectionLayoutWin:CURRENT-WINDOW:X ELSE 0)

                                          ELSE ghSectionLayoutWin:CURRENT-WINDOW:X - 7)
                                   ELSE SESSION:WORK-AREA-WIDTH-PIXELS - giSectionLayoutWinWidth - 7.
    hwin:WINDOW-STATE = WINDOW-MAXIMIZED.
    IF glAbhackFind THEN hWin:Y = 1.
    IF gcSectionOutlineMode <> "disabled"
     AND VALID-HANDLE(ghSectionLayoutWin) /* 14-NOV-2007 sla: adding this to avoid ERROR 3137 when the section layout window has been closed */
     THEN hWin:X = IF ghSectionLayoutWin:CURRENT-WINDOW:X < SESSION:WORK-AREA-WIDTH-PIXELS / 2 /* 06-JUL-2007 sla: handle situations where the sectionWindow is on the right and the editor on its left */
                    THEN ghSectionLayoutWin:CURRENT-WINDOW:X + giSectionLayoutWinWidth + 7
                    ELSE 1.
    APPLY 'WINDOW-RESIZED' TO hWin.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ctrlZ C-Win
PROCEDURE ctrlZ :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

RUN disableNextValueChanged (phEditor, "ctrlZ").
phEditor:SOURCE-COMMAND('undo','').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelayWindowEvent C-Win
PROCEDURE DelayWindowEvent :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER phTargetWidget AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER pcEvent        AS CHARACTER  NO-UNDO.

ASSIGN
 ghDelayTriggerHandle                  = phTargetWidget
 gcDelayTriggerWinEvent                = pcEvent
 chCtrlDelay:PSTimerApplyEvent:ENABLED = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DictResized C-Win
PROCEDURE DictResized :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE iDiffHeight AS INTEGER  NO-UNDO.
DEFINE VARIABLE iDiffWidth  AS INTEGER  NO-UNDO.

iDiffWidth  = ( hwDict:WIDTH-PIXELS - hDictFrame:WIDTH-PIXELS ) / 3.
iDiffHeight = hwDict:HEIGHT-PIXELS - hDictFrame:HEIGHT-PIXELS.
hDictFrame:SCROLLABLE = YES.

IF iDiffHeight > 0 THEN ASSIGN
 hDictFrame:HEIGHT-PIXELS         = hwDict:HEIGHT-PIXELS
 hDictFrame:VIRTUAL-HEIGHT-PIXELS = hwDict:HEIGHT-PIXELS.

IF iDiffHeight <> 0 THEN ASSIGN
 hs_lst_Dbs:HEIGHT-PIXELS  = hs_lst_Dbs:HEIGHT-PIXELS + iDiffHeight
 hs_lst_Tbls:HEIGHT-PIXELS = hs_lst_Tbls:HEIGHT-PIXELS + iDiffHeight
 hs_lst_Seqs:HEIGHT-PIXELS = hs_lst_Seqs:HEIGHT-PIXELS + iDiffHeight
 hs_lst_Flds:HEIGHT-PIXELS = hs_lst_Flds:HEIGHT-PIXELS + iDiffHeight
 hs_lst_Idxs:HEIGHT-PIXELS = hs_lst_Idxs:HEIGHT-PIXELS + iDiffHeight

 hs_btn_Create:Y           = hs_btn_Create:Y + iDiffHeight
 hs_btn_Props:Y            = hs_btn_Props:Y + iDiffHeight
 hs_btn_Delete:Y           = hs_btn_Delete:Y + iDiffHeight
 hs_Browse_Stat:Y          = hs_Browse_Stat:Y + iDiffHeight.

IF iDiffHeight < 0 THEN ASSIGN
 hDictFrame:VIRTUAL-HEIGHT-PIXELS = hwDict:HEIGHT-PIXELS
 hDictFrame:HEIGHT-PIXELS         = hwDict:HEIGHT-PIXELS.

IF iDiffWidth > 0 THEN ASSIGN
 hDictFrame:WIDTH-PIXELS         = hwDict:WIDTH-PIXELS
 hDictFrame:VIRTUAL-WIDTH-PIXELS = hwDict:WIDTH-PIXELS.

IF iDiffWidth <> 0 THEN ASSIGN
 hs_lst_Dbs:WIDTH-PIXELS  = hs_lst_Dbs:WIDTH-PIXELS + iDiffWidth
 hs_txt_Dbs:WIDTH-PIXELS  = hs_lst_Dbs:WIDTH-PIXELS
 hs_fil_Dbs:WIDTH-PIXELS  = hs_lst_Dbs:WIDTH-PIXELS

 hs_lst_Tbls:X            = hs_lst_Tbls:X + iDiffWidth
 hs_lst_Tbls:WIDTH-PIXELS = hs_lst_Tbls:WIDTH-PIXELS + iDiffWidth
 hs_txt_Tbls:X            = hs_lst_Tbls:X
 hs_fil_Tbls:X            = hs_lst_Tbls:X
 hs_txt_Tbls:WIDTH-PIXELS = hs_lst_Tbls:WIDTH-PIXELS
 hs_fil_Tbls:WIDTH-PIXELS = hs_lst_Tbls:WIDTH-PIXELS

 hs_lst_Seqs:X            = hs_lst_Seqs:X + iDiffWidth
 hs_lst_Seqs:WIDTH-PIXELS = hs_lst_Seqs:WIDTH-PIXELS + iDiffWidth
 hs_lst_Seqs:X            = hs_lst_Tbls:X
 hs_fil_Seqs:X            = hs_lst_Tbls:X
 hs_lst_Seqs:WIDTH-PIXELS = hs_lst_Tbls:WIDTH-PIXELS
 hs_fil_Seqs:WIDTH-PIXELS = hs_lst_Tbls:WIDTH-PIXELS

 hs_lst_Flds:X            = hs_lst_Flds:X + 2 * iDiffWidth
 hs_lst_Flds:WIDTH-PIXELS = hs_lst_Tbls:WIDTH-PIXELS
 hs_txt_Flds:X            = hs_lst_Flds:X
 hs_fil_Flds:X            = hs_lst_Flds:X
 hs_txt_Flds:WIDTH-PIXELS = hs_lst_Flds:WIDTH-PIXELS
 hs_fil_Flds:WIDTH-PIXELS = hs_lst_Flds:WIDTH-PIXELS

 hs_lst_Idxs:X            = hs_lst_Flds:X
 hs_lst_Idxs:WIDTH-PIXEL  = hs_lst_Tbls:WIDTH-PIXELS
 hs_fil_Idxs:X            = hs_lst_Flds:X
 hs_fil_Idxs:WIDTH-PIXELS = hs_lst_Tbls:WIDTH-PIXELS

 hs_btn_Tbls:X            = hs_lst_Tbls:X
 hs_btn_Seqs:X            = hs_lst_Tbls:X + hs_btn_Tbls:WIDTH-PIXELS + 10
 hs_btn_Flds:X            = hs_lst_Flds:X
 hs_btn_Idxs:X            = hs_lst_Flds:X + hs_btn_Flds:WIDTH-PIXELS + 10

 hs_btn_Create:X          = hs_btn_Create:X + ( iDiffWidth * 3 ) / 2
 hs_btn_Props:X           = hs_btn_Create:X + hs_btn_Create:WIDTH-PIXELS + 5
 hs_btn_Delete:X          = hs_btn_Props:X + hs_btn_Props:WIDTH-PIXELS + 5
 hs_Browse_Stat:X         = hs_btn_Delete:X + hs_btn_Delete:WIDTH-PIXELS + 5.

IF iDiffWidth < 0 THEN ASSIGN
 hDictFrame:VIRTUAL-WIDTH-PIXELS = hwDict:WIDTH-PIXELS
 hDictFrame:WIDTH-PIXELS         = hwDict:WIDTH-PIXELS.


/* no scrollbar when sizing down plzzz */
hDictFrame:SCROLLABLE            = NO.
hDictFrame:VIRTUAL-HEIGHT-PIXELS = hDictFrame:HEIGHT-PIXELS.
hDictFrame:VIRTUAL-WIDTH-PIXELS  = hDictFrame:WIDTH-PIXELS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableNextValueChanged C-Win
PROCEDURE disableNextValueChanged :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER phEditor    AS HANDLE      NO-UNDO.
    DEFINE INPUT  PARAMETER pcReasonWhy AS CHARACTER   NO-UNDO.

    DEFINE BUFFER ttEdt FOR ttEdt.

    FIND ttEdt WHERE ttEdt.hEditor = phEditor.
    ttEdt.cIgnoreValueChanged = pcReasonWhy. /* will be reset by valueChanged */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displaytextSelectedInfo C-Win
PROCEDURE displaytextSelectedInfo :
/*------------------------------------------------------------------------------
  Purpose:
  Notes:        26-NOV-2008 sla:  no longer use a WIDGET-POOL to manage the deletion
                of hBuffer created with createTTorDBBuffer()
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcItem   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cBufferInfo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDbName     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cField      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cIdx        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cInfo       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTable      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBuffer     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hField      AS HANDLE      NO-UNDO.
DEFINE VARIABLE iDotEntries AS INTEGER     NO-UNDO.
DEFINE VARIABLE iField      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iIdx        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxField   AS INTEGER     NO-UNDO.

iDotEntries = NUM-ENTRIES(pcItem, ".").

DEFINE BUFFER ttgbuffer FOR ttgbuffer.
DEFINE BUFFER ttbuffer  FOR ttbuffer.
DEFINE BUFFER ttgVar    FOR ttgVar.
DEFINE BUFFER ttVar     FOR ttVar.

cInfo = pcItem.

IF pcItem = ""
 OR iDotEntries > 3 THEN RETURN.  /* what to do with this ? */

IF iDotEntries = 3 THEN ASSIGN
 cTable    = ENTRY(1, pcItem, ".") + "." + ENTRY(2, pcItem, ".")
 cField    = ENTRY(3, pcItem, ".").

ELSE IF iDotEntries = 2 THEN DO:
     cDbName = ENTRY(1, pcItem, ".").
     IF INDEX(cDbName, "/") > 0 OR INDEX(cDbName, " ") > 0 OR INDEX(cDbName, "\") > 0 THEN RETURN. /* 19-MAR-2007 sla: fix, avoid error 1016  */
     IF CONNECTED(cDbName) THEN cTable = pcItem.
     ELSE ASSIGN
     cTable    = ENTRY(1, pcItem, ".")
     cField    = ENTRY(2, pcItem, ".").
END.

ELSE cTable = pcItem.

/* first try to find variable info */
FIND FIRST ttVar WHERE ttVar.cVar = pcItem NO-ERROR.
IF AVAILABLE ttVar THEN DO:
    cInfo = cInfo + " is a local variable~nType: " + ttVar.cDataType.
    PUBLISH "abhackDisplayInfo" (cInfo).
    RETURN.
END.

FIND FIRST ttgVar WHERE ttgVar.hEditor = phEditor AND ttgVar.cVar = pcItem NO-ERROR.
IF AVAILABLE ttgVar THEN DO:
    cInfo = cInfo + " is a GLOBAL variable~nType: " + ttgVar.cDataType
     + "~n  Access Mode: " + ttgVar.cAccessMode
     + "~n  Defined in: " + ttgVar.cFileName.
    PUBLISH "abhackDisplayInfo" (cInfo).
    RETURN.
END.

/* try to find out database info */
IF cTable > "" THEN DO:
    FIND FIRST ttbuffer WHERE ttbuffer.cName = cTable NO-ERROR.
    IF AVAILABLE ttbuffer THEN cBufferInfo = "~nLocal Buffer for " + ttbuffer.cfor + " ".
    ELSE DO:
        FIND FIRST ttgbuffer WHERE ttgbuffer.hEditor = phEditor AND ttgbuffer.cName = pcItem NO-ERROR.
        IF AVAILABLE ttgbuffer THEN cBufferInfo = "~nGLOBAL Buffer for " + ttgbuffer.cfor
         + "~n  Access Mode: " + ttgbuffer.cAccessMode
         + "~n  Defined in: " + ttgbuffer.cFileName + "~n".
    END.


    hBuffer = createTTorDBBuffer(phEditor, cTable, "").
    IF NOT VALID-HANDLE(hBuffer) THEN RETURN.

    cInfo = cInfo + cBufferInfo
      + " (" + (IF hBuffer:DBNAME = "PROGRESST" THEN "Temp-table" ELSE hBuffer:DBNAME) + ")".

    /* no field info => say more about the table */
    IF cField = "" THEN DO:
        iMaxField =  hBuffer:NUM-FIELDS.
        DO iField = 1 TO iMaxField:
            hField = hBuffer:BUFFER-FIELD(iField).
            cInfo = cInfo + "~nField " + hField:NAME + " " + CAPS(hField:DATA-TYPE)
             + (IF hField:EXTENT = 0 THEN "" ELSE "~n    Extent: " + STRING(hField:EXTENT))
             + "~n    Format: " + hField:FORMAT
             + "~n    Initial value: " + hField:INITIAL
             + "~n    Label: " + hField:LABEL + "~n".
        END.
        DO WHILE TRUE:
            iIdx = iIdx + 1.
            cIdx = hBuffer:INDEX-INFORMATION(iIdx).
            IF cIdx = ? THEN LEAVE.
            cIdx = ENTRY(1, cIdx).
            cInfo = cInfo + "~nIndex " + cIdx + "~n" + queryIndexDetails(hBuffer, cIdx) + "~n".
        END.
    END.

    /* give info about the field */
    ELSE DO:
        hField = hBuffer:BUFFER-FIELD(cField) NO-ERROR.
        IF NOT VALID-HANDLE(hField) THEN cInfo = cInfo + "~nUnknown Field " + QUOTER(cField).
        ELSE cInfo = cInfo + "~n  " + CAPS(hField:DATA-TYPE) + " Field"
         + (IF hField:EXTENT = 0 THEN "" ELSE "~n    Extent: " + STRING(hField:EXTENT))
         + "~n    Format: " + hField:FORMAT
         + "~n    Initial value: " + hField:INITIAL
         + "~n    Label: " + hField:LABEL.
    END.

    /* 04-DEC-2008 sla: solve error Cannot delete the default buffer for a TEMP-TABLE object--delete the TEMP-TABLE object instead.(328)
             => I may have to review my buffer memory leak fix done recently */
    IF VALID-HANDLE(hBuffer) THEN  DO:
        IF hBuffer:TABLE-HANDLE <> ? THEN
         hBuffer = hBuffer:TABLE-HANDLE.
        DELETE OBJECT hBuffer NO-ERROR.
    END.
END.

PUBLISH "abhackDisplayInfo" (cInfo).
RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dotPressed C-Win
PROCEDURE dotPressed :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

DEFINE VARIABLE cEqualField           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPopupListRunningMode AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrevPrevWord         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrevWord             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBuffer               AS HANDLE      NO-UNDO.
DEFINE VARIABLE hEditorListProc       AS HANDLE      NO-UNDO.
DEFINE VARIABLE iCursorChar           AS INTEGER     NO-UNDO.

DEFINE BUFFER ttEdt FOR ttEdt.

IF chCtrlSpy:PSTimerSpy:ENABLED = NO THEN RETURN. /* 01-FEB-2008 sla: improvment so people can disable A4GBL completion in javascripts */
IF NOT glCompleteField THEN RETURN.

/* 30-MAR-2007 sla: do this first to avoid another popup when choosing a table/buffer by pressing dot */
RUN disableNextValueChanged (phEditor, "DotPressed").

/* 14-MAR-2007 sla: new feature to first fire completion of current selected table in table list */
PUBLISH "getEditorListRunningMode" (phEditor, INPUT-OUTPUT cPopupListRunningMode).
IF cPopupListRunningMode = "tableList" THEN DO:
    PUBLISH "completionTableAndApplyDot" (phEditor).
    RETURN.
END.

iCursorChar = phEditor:CURSOR-CHAR.

RUN getEditorLine IN TARGET-PROCEDURE (phEditor, OUTPUT cLine).

/* 21-MAR-2007 sla: ignore things located after the cursor */
cLine = SUBSTRING(cLine, 1, iCursorChar - 1).

/* 27-JAN-2007 sla: new feature to disable popup when the carret is in a quoted string */
IF glNoCompletionInStrings AND carretInQuotedString(cLine, iCursorChar) THEN RETURN.

/* 16-MAR-2007 sla: added "periodPartOfWords" option to improve the support of qualified database names */
RUN extractWordN IN TARGET-PROCEDURE (0, cLine, iCursorChar - 1, "periodPartOfWords,SingleQuotePartOfWords,DoubleQuotePartOfWords", OUTPUT cPrevWord) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

IF gcDelayTriggerWinEvent = "dotPressedOnly" THEN cPrevWord = SUBSTRING(cPrevWord, 1, LENGTH(cPrevWord) - 1).  /* 20-MAR-2007 sla: ignore trailing dot */

RUN extractWordN IN TARGET-PROCEDURE (-1, cLine, iCursorChar - 1, "periodPartOfWords,SingleQuotePartOfWords,DoubleQuotePartOfWords", OUTPUT cPrevPrevWord) NO-ERROR.

cMon:SCREEN-VALUE IN FRAME fMain = "Dot Pressed.   cPrevPrevWord: >" + cPrevPrevWord + "< cPrevWord: >" + cPrevWord + " <".

IF  cPrevWord = ""
 OR INDEX(cPrevWord, "'")  > 0  /* this would lead to vst when pressing ''. */
 OR INDEX(cPrevWord, '"')  > 0  /* this would lead to vst when pressing "". */
 OR INDEX(cPrevWord, "&")  > 0  /* these 3 ... */
 OR INDEX(cPrevWord, "~{") > 0  /* condition ...*/
 OR INDEX(cPrevWord, "~}") > 0  /* can lead to a crash when I find in _file with BEGINS operator*/
 THEN RETURN.

/* 07-FEB-2007 sla: avoid unwanted field popup when typing '.' after ')' or some other characters */
IF  SUBSTRING(cLine, 1, iCursorChar - 1)  MATCHES "*)"
 OR SUBSTRING(cLine, 1, iCursorChar - 1)  MATCHES "*("
 OR SUBSTRING(cLine, 1, iCursorChar - 1)  MATCHES "* "
 OR SUBSTRING(cLine, 1, iCursorChar - 1)  MATCHES "*,"
 /* 14-MAR-2007 sla: after all, why keepig this one
 OR SUBSTRING(cLine, 1, iCursorChar - 1)  MATCHES "*~~~."*/
 OR SUBSTRING(cLine, 1, iCursorChar - 1)  MATCHES "*:"
 OR SUBSTRING(cLine, 1, iCursorChar - 1)  MATCHES "*/"
 THEN RETURN.

/* for => definition of buffers     TO => buffer-copy   FIND => find unique */
IF CAN-DO("FIRST,PREV*,NEXT,LAST,FOR,DELETE,CREATE,VALIDATE,RELEASE,AVAIL*,TO,FIND,OF"
         ,cPrevPrevWord) THEN RETURN.

IF cPrevPrevWord = "=" THEN DO:
    RUN extractWordN IN TARGET-PROCEDURE (-2, cLine, iCursorChar - 1, "SingleQuotePartOfWords,DoubleQuotePartOfWords", OUTPUT cEqualField) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR AND cEqualField > "" AND cEqualField <> ?
     THEN cEqualField = ENTRY(NUM-ENTRIES(cEqualField, "."), cEqualField, "."). /* ends up as empty in case of <table.> without field */
    IF cEqualField > "" THEN cEqualField = ",scrollToFieldLike=" + cEqualField. /* this way  */
END.

/* 03-FEB-2007 sla: use new API */
hBuffer = createTTorDBBuffer(phEditor, cPrevWord, "IgnoreIndices").

IF NOT VALID-HANDLE(hBuffer) THEN RETURN.

RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).

IF NOT VALID-HANDLE(hEditorListProc) THEN DO:
    DELETE OBJECT hBuffer. /* 27-AUG-2007 sla: solve possible memory leak */
    RETURN. /* invalid if it decided to kill itself */
END.

cMon:SCREEN-VALUE = cMon:SCREEN-VALUE + "~nSuggest field list for buffer "  + cPrevWord.
RUN loadFieldList IN hEditorListProc
 (hBuffer
 ,  "sortedBy=" + gcFieldSortedBy
  + ",multipleSelection"
  + ",glConvertDbTablesToLC=" + STRING(glConvertDbTablesToLC)
  + cEqualField).

/* note: procEditorList.w will delete the BUFFER object on CLOSE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dumpGlobalResources C-Win
PROCEDURE dumpGlobalResources :
/*------------------------------------------------------------------------------
  Purpose:     Load global definitions of vars, tt's and their fields
------------------------------------------------------------------------------*/

DEFINE BUFFER ttEdt           FOR ttEdt.
DEFINE BUFFER tttt            FOR tttt.
DEFINE BUFFER ttfld           FOR ttfld.
DEFINE BUFFER ttIdx           FOR ttIdx.
DEFINE BUFFER ttgVar          FOR ttgVar.
DEFINE BUFFER ttgbuffer       FOR ttgbuffer.
DEFINE BUFFER ttProc          FOR ttProc.
DEFINE BUFFER ttFunc          FOR ttFunc.
DEFINE BUFFER ttMethod        FOR ttMethod.
DEFINE BUFFER ttMark          FOR ttMark.
DEFINE BUFFER ttSection       FOR ttSection.
DEFINE BUFFER ttUsing         FOR ttUsing.
DEFINE BUFFER ttReferedBuffer FOR ttReferedBuffer.
DEFINE BUFFER ttgLibHandle    FOR ttgLibHandle.
DEFINE BUFFER ttPreproc       FOR ttPreproc.

FIND ttEdt WHERE ttEdt.hEditor = ghCurrentEditor NO-ERROR.

OUTPUT TO "CLIPBOARD".
PUT UNFORMATTED "giLastttid:" giLastttid SKIP.

IF VALID-HANDLE(ghCurrentEditor) THEN
 PUT UNFORM "Global resources of editor" QUOTER(ghCurrentEditor) " Window:"
  ghCurrentEditor:WINDOW:TITLE SKIP.

IF AVAILABLE ttEdt THEN PUT UNFORMATTED "Source: " ttEdt.cFileName.
IF AVAILABLE ttEdt AND ttEdt.isClass = YES THEN PUT " inherits: " ttEdt.cInherits.

PUT SKIP(2) "Track of preprocessor definitions: " SKIP.
FOR EACH ttPreproc WHERE ttPreproc.hEditor = ghCurrentEditor
  BY ttPreproc.iLineNum:
    PUT UNFORMATTED " "
     STRING(ttPreproc.iLineNum, ">>>>>9")
     STRING(ttPreproc.iIncludeLevel, ">>9") " "
     STRING(ttPreproc.cType, "X(8)") " "
     ttPreproc.cName "  "
     ttPreproc.cValue
     SKIP.
END.


PUT SKIP(2) "Track of RUN someLib.p PERSIST SET someGlobalVar: " SKIP.
FOR EACH ttgLibHandle WHERE ttgLibHandle.hEditor = ghCurrentEditor:
    PUT UNFORMATTED " " ttgLibHandle.iLine "  RUN " ttgLibHandle.cLibFile " PERSIST SET " ttgLibHandle.cVar SKIP.
END.

PUT SKIP(2).

FOR EACH tttt WHERE tttt.hEditor = ghCurrentEditor BY tttt.ittid:
    PUT UNFORMATTED "Temp-table: " tttt.cttname "  before-table: " tttt.cBeforeTable   "  Acces mode: " tttt.cAccessMode "  defined in: " tttt.cFileName
     IF tttt.isStatic THEN " STATIC " ELSE "" SKIP.
    FOR EACH ttfld WHERE ttfld.ittid = tttt.ittid BY ttfld.ifldseq:
        PUT UNFORMATTED "   field: " STRING(ttfld.cfldname, "X(20)") " AS " ttfld.cDataType SKIP.
    END.
    FOR EACH ttIdx WHERE ttIdx.ittid = tttt.ittid BY ttIdx.idxSeq:
        PUT UNFORMATTED "   index: " ttIdx.idxSeq " " ttIdx.cidxname " " ttIdx.coptn " " ttIdx.cFieldsInfo SKIP.
    END.
END.

PUT SKIP(2) "Variables: " SKIP.
FOR EACH ttgVar WHERE ttgVar.hEditor = ghCurrentEditor:
    PUT UNFORMATTED " " ttgvar.cvar "  " ttgVar.cViewAs "  Acces mode: " ttgVar.cAccessMode "  defined in: " ttgVar.cFileName
     (IF ttgVar.isStatic THEN " STATIC " ELSE "")  SKIP.
END.

PUT UNFORMATTED SKIP(2) "Global Buffers:" SKIP.
FOR EACH ttgbuffer WHERE ttgbuffer.hEditor = ghCurrentEditor:
    PUT UNFORMATTED ttgbuffer.cName " FOR " ttgbuffer.cfor "  Acces mode: " ttgbuffer.cAccessMode "  defined in: " ttgbuffer.cFileName
     (IF ttgbuffer.isStatic THEN " STATIC " ELSE "")  SKIP.
END.

PUT UNFORMATTED SKIP(2) "procedures:" SKIP.
FOR EACH ttProc WHERE ttProc.hEditor = ghCurrentEditor:
    PUT UNFORMATTED ttProc.cName "  " ttProc.cParameters "  defined in: " ttProc.cFileName SKIP.
END.

PUT UNFORMATTED SKIP(2) "functions:" SKIP.
FOR EACH ttFunc WHERE ttFunc.hEditor = ghCurrentEditor:
    PUT UNFORMATTED ttFunc.cName "  " ttFunc.cParameters " defined in: " ttFunc.cFileName SKIP.
END.

PUT UNFORMATTED SKIP(2) "methods:" SKIP.
FOR EACH ttMethod WHERE ttMethod.hEditor = ghCurrentEditor:
    PUT UNFORMATTED ttMethod.cName "  " ttMethod.cAccessMode "  "  ttMethod.cReturnType "  defined in: " ttMethod.cFileName "   " ttMethod.cParameters
     (IF ttMethod.isStatic THEN " STATIC " ELSE "")  SKIP.
END.

PUT SKIP(2) "Marks: " SKIP.
FOR EACH ttMark WHERE ttMark.hEditor = ghCurrentEditor BY ttMark.iLine:
    PUT " " ttMark.iLine " " ttMark.iOffset " " ttMark.cBlockType " ".
    PUT UNFORMATTED ttMark.cBlockName SKIP.
    FOR EACH ttReferedBuffer
     WHERE ttReferedBuffer.hEditor     = ttMark.hEditor
       AND ttReferedBuffer.cBlockType  = ttMark.cBlockType
       AND ttReferedBuffer.cBlockName  = ttMark.cBlockName
       BY ttReferedBuffer.cDataBase BY ttReferedBuffer.cBufferName:
        PUT UNFORMATTED "        Buffer: "
         STRING(ttReferedBuffer.cBufferName, "X(30)")
         (IF ttReferedBuffer.cDataBase = "" THEN "" ELSE "(" + ttReferedBuffer.cDataBase + ")")
         SKIP.
    END.
END.

PUT SKIP(2) "Using: " SKIP.
FOR EACH ttUsing WHERE ttUsing.hEditor = ghCurrentEditor:
    PUT UNFORMATTED ttUsing.cUsing SKIP.
END.

OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dumpLocalResources C-Win
PROCEDURE dumpLocalResources :
/*------------------------------------------------------------------------------
  Purpose:     used for debugging purposes
------------------------------------------------------------------------------*/
DEFINE BUFFER ttVar         FOR ttVar.
DEFINE BUFFER ttbuffer      FOR ttbuffer.
DEFINE BUFFER ttUsedBuffers FOR ttUsedBuffers.
DEFINE BUFFER ttBlocklabels FOR ttBlocklabels.
DEFINE BUFFER ttsection     FOR ttsection.
DEFINE BUFFER ttLibHandle   FOR ttLibHandle.
DEFINE BUFFER ttPreproc     FOR ttPreproc.
DEFINE BUFFER ttEdt         FOR ttEdt.

DEFINE VARIABLE iEndLine   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iStartLine AS INTEGER   NO-UNDO.

FIND FIRST ttEdt WHERE ttEdt.hEditor = ghCurrentEditor.

RUN getCurrentBlockBoundaries (ttEdt.hEditor
                              ,OUTPUT ttEdt.currentSection
                              ,OUTPUT iStartLine
                              ,OUTPUT iEndLine).
IF iEndLine = 0 THEN iEndLine = 99999999.

OUTPUT TO "clipboard".


PUT SKIP(2) "Track of preprocessor definitions (not filtered (yet) on current section...): " SKIP.
FOR EACH ttPreproc WHERE ttPreproc.hEditor = ghCurrentEditor
                     AND ttPreproc.iLineNum < iEndLine
  BY ttPreproc.iLineNum:
    PUT UNFORMATTED " "
     STRING(ttPreproc.iLineNum, ">>>>>9")
     STRING(ttPreproc.iIncludeLevel, ">>9") " "
     STRING(ttPreproc.cType, "X(8)") " "
     ttPreproc.cName "  "
     ttPreproc.cValue
     SKIP.
END.


PUT "Track of RUN someLib.p PERSIST SET someLocalVar: " SKIP.

FOR EACH ttLibHandle :
    PUT UNFORMATTED ttLibHandle.iLine "   RUN " ttLibHandle.cLibFile " PERSIST SET " ttLibHandle.cVar SKIP.
END.

PUT UNFORMATTED "Variables/params:" SKIP.
FOR EACH ttVar :
    PUT UNFORMATTED ttVar.cVar SKIP.
END.

PUT UNFORMATTED SKIP(2) "Buffers:" SKIP.
FOR EACH ttbuffer:
    PUT UNFORMATTED ttbuffer.cName " FOR " ttbuffer.cfor SKIP.
END.

PUT UNFORMATTED SKIP(2) "Used Buffers:" SKIP.
FOR EACH ttUsedBuffers BY ttUsedBuffers.iLine:
    PUT ttUsedBuffers.iLine.
    PUT UNFORMATTED "  " ttUsedBuffers.cName SKIP.
END.

PUT UNFORMATTED SKIP(2) "Block labels:" SKIP.
FOR EACH ttBlocklabels BY ttBlocklabels.iLine:
    PUT ttBlocklabels.iLine.
    PUT UNFORMATTED "  " ttBlocklabels.cName SKIP.
END.


PUT UNFORMATTED SKIP(2) "Used sections" SKIP.
FOR EACH ttsection WHERE ttsection.hEditor = ghCurrentEditor BY ttsection.iMruSequence:
    DISPLAY ttsection.iMruSequence ttsection.cSection ttsection.cEvent
     ttsection.cWidget ttsection.cursorLine ttsection.cursorChar SKIP WITH WIDTH 200 .
END.


OUTPUT CLOSE.

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
  RUN control_load.
  DISPLAY gcSectionOutlineMode glSectionLayoutIgnoreStructProcs glAbhackFind
      WITH FRAME fMain IN WINDOW C-Win.
  ENABLE cmon gcSectionOutlineMode btnOpenReleaseNotes btnHide btnOpenPeg
         btnOpenPSDN btnOpenReadmePdf glSectionLayoutIgnoreStructProcs
         BtnCompletion BtnKeys btnMisc glAbhackFind bedtBrowse
         btnLaunchInfoFloatingWin btnExpand btnloadGlobalResources btnStopStart
      WITH FRAME fMain IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY glCompleteOnTab giTabNumSpaces glAutoComplete giAutoCompMinSize
          glHypeStrikeMode glCompleteWithSimilar glNoCompletionInStrings
          glNoCompletionInComment glLowerCaseCompletion
      WITH FRAME fComplEnable IN WINDOW C-Win.
  ENABLE glCompleteOnTab giTabNumSpaces glAutoComplete giAutoCompMinSize
         glHypeStrikeMode glCompleteWithSimilar glNoCompletionInStrings
         glNoCompletionInComment glLowerCaseCompletion
      WITH FRAME fComplEnable IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fComplEnable}
  DISPLAY glClassNewCastAfterEqual
      WITH FRAME fComplClasses IN WINDOW C-Win.
  ENABLE glClassNewCastAfterEqual
      WITH FRAME fComplClasses IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fComplClasses}
  ENABLE imgPage
      WITH FRAME fImagePage IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fImagePage}
  DISPLAY glAlignEnabled glAlignVars glAlignParams glAlignTTFields
          glAlignTTFieldsSort glAlignAssign
      WITH FRAME fAlign IN WINDOW C-Win.
  ENABLE glAlignEnabled glAlignVars glAlignTTFields glAlignTTFieldsSort
         glAlignAssign
      WITH FRAME fAlign IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fAlign}
  DISPLAY glCompleteTableOnFLA glAddWhereAfterBuffer glAddNoLock
          glConvertDbTablesToLC
      WITH FRAME fComplTable IN WINDOW C-Win.
  ENABLE glCompleteTableOnFLA glAddWhereAfterBuffer glAddNoLock
         glConvertDbTablesToLC
      WITH FRAME fComplTable IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fComplTable}
  DISPLAY glPreprocessors
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE glPreprocessors
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  DISPLAY glProposeOpenSelectedFile gcFancyInABWin glDisplaytextSelectedInfo
          glUseSchemaCache glPlusplus glKeepTrackListingBuffer
          glDrawBoxAroundCurrentLine glMoveNewWindowsAway
          giRightMarginIndicatorCol giMaxEmptyLinesBeforeShrinking
          glResizableDictionary glTopOnly glAutoStopWhenNonEditorWin
          gcGlobalResCat glKeepTrackOfCursor glJumpMruSection
          gcDumpedResourceFileRoot giNameComboFont gltooltipOnOpenParenthese
          gcCatDbConnection glAnalyseQuery giQueryTooltipTimeSuccess
          giQueryTooltipTimeFail glAutoStopDialog glQADispInABHWin
          glForceWaitFor glSaveSession giQueryTooltipTimeSuccessTxt
          giQueryTooltipTimeFailTxt
      WITH FRAME fMisc IN WINDOW C-Win.
  ENABLE RECT-3 RECT-6 RECT-7 RECT-9 RECT-10 RECT-11
         btnOpenHowToShowMatchingParent glProposeOpenSelectedFile
         gcFancyInABWin glDisplaytextSelectedInfo glUseSchemaCache glPlusplus
         glKeepTrackListingBuffer glDrawBoxAroundCurrentLine
         glMoveNewWindowsAway giRightMarginIndicatorCol
         giMaxEmptyLinesBeforeShrinking glResizableDictionary glTopOnly
         glAutoStopWhenNonEditorWin gcGlobalResCat glKeepTrackOfCursor
         glJumpMruSection gcDumpedResourceFileRoot giNameComboFont
         btnOpenAbhackDbReadme gltooltipOnOpenParenthese gcCatDbConnection
         BtnReenableAllAPITooltips btnTestAbhackDb btnOpenAPITooltipFile
         BtnBulkDumpAbhackGlobDesc glAnalyseQuery giQueryTooltipTimeSuccess
         giQueryTooltipTimeFail glAutoStopDialog glQADispInABHWin
         glForceWaitFor glSaveSession
      WITH FRAME fMisc IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fMisc}
  VIEW FRAME fCompletion IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fCompletion}
  DISPLAY gcSuggestBufferFor
      WITH FRAME fComplSuggestUsedBuffers IN WINDOW C-Win.
  ENABLE btnSuggesBufferSelectAll-2 gcSuggestBufferFor btnSuggesBufferSelectAll
      WITH FRAME fComplSuggestUsedBuffers IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fComplSuggestUsedBuffers}
  DISPLAY glCompleteOOSuperMethod glCompleteAttrOnTab glFilterOnViewAsWidgetType
          glFilterGuessObjectType glAddEndAfterDo
      WITH FRAME fComplColon IN WINDOW C-Win.
  ENABLE glCompleteOOSuperMethod glCompleteAttrOnTab btnOpenAttrCompletionFile
         glFilterOnViewAsWidgetType glFilterGuessObjectType
         btnOpenAttrGuessObjTypeFile glAddEndAfterDo
      WITH FRAME fComplColon IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fComplColon}
  DISPLAY glFindNextSelected glDuplicateLineOnAltDown
          glCtrlBackspacelDelPrevWord glCtrlDelDelsWord glImprovedCtrlKeftRight
          glcommentOnDblSlash gcActionOnCtrlS glcommentSelectionOnSlash
          glMulitpleClipboardDummyToggle glCompile glCompileMinsize
          glAdaptWindowHeightOnCtrlW glListingOnCtrlAltT glMaximizeRestore
          glDebugListingOnCtrlAltD glCtrlF3NewProcWin glStopStartTimerOnCtrlAltO
          glOpenPW glHideAllComments glXrefAnal glHideSelection
          glCopyUIBelowMouse glShowLineNumbers glShowSpecialChars
      WITH FRAME fKeys IN WINDOW C-Win.
  ENABLE RECT-8 glFindNextSelected glDuplicateLineOnAltDown
         glCtrlBackspacelDelPrevWord glCtrlDelDelsWord glImprovedCtrlKeftRight
         glcommentOnDblSlash gcActionOnCtrlS glcommentSelectionOnSlash
         glCompile glCompileMinsize glAdaptWindowHeightOnCtrlW
         glListingOnCtrlAltT glDebugListingOnCtrlAltD glCtrlF3NewProcWin
         glStopStartTimerOnCtrlAltO glOpenPW glHideAllComments glXrefAnal
         glHideSelection glCopyUIBelowMouse glShowLineNumbers
         glShowSpecialChars
      WITH FRAME fKeys IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fKeys}
  DISPLAY glRunInLibComp glIntProcCompletion glIntProcInputInSig
          giSplitParamInCalls gcSplitParamCommaStyle glExtProcCompletion
          glFindOSFilesWithAbhackDB giMaxEtimeToLoadFiles giMaxFilesToLoad
          gcExtProcIgnoreDir
      WITH FRAME fComplRun IN WINDOW C-Win.
  ENABLE RECT-5 glRunInLibComp glIntProcCompletion glIntProcInputInSig
         giSplitParamInCalls gcSplitParamCommaStyle glExtProcCompletion
         glFindOSFilesWithAbhackDB giMaxEtimeToLoadFiles giMaxFilesToLoad
         gcExtProcIgnoreDir
      WITH FRAME fComplRun IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fComplRun}
  DISPLAY glCompleteBlockLabels
      WITH FRAME fComplBlockLabel IN WINDOW C-Win.
  ENABLE glCompleteBlockLabels
      WITH FRAME fComplBlockLabel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fComplBlockLabel}
  DISPLAY glCustomCompletionOnTab gcComplDateFormat
      WITH FRAME fComplCustom IN WINDOW C-Win.
  ENABLE glCustomCompletionOnTab btnOpenCustomCompletionFile
         btnOpenCustomCompletionFileOfMin gcComplDateFormat
      WITH FRAME fComplCustom IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fComplCustom}
  DISPLAY glCompleteField gcFieldSortedBy
      WITH FRAME fComplDot IN WINDOW C-Win.
  ENABLE glCompleteField gcFieldSortedBy
      WITH FRAME fComplDot IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fComplDot}
  DISPLAY glInsertClosingGuys glEnhancedCloseParenthese
      WITH FRAME fComplClosingGuy IN WINDOW C-Win.
  ENABLE glInsertClosingGuys glEnhancedCloseParenthese
      WITH FRAME fComplClosingGuy IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fComplClosingGuy}
  DISPLAY glVariableCompletion glIntFuncCompletion
      WITH FRAME fComplVarUDF IN WINDOW C-Win.
  ENABLE glVariableCompletion glIntFuncCompletion
      WITH FRAME fComplVarUDF IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fComplVarUDF}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE expandWindow C-Win
PROCEDURE expandWindow :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcMode AS CHARACTER   NO-UNDO.

IF  SELF:LABEL = ">>"  AND pcMode = "forward"
 OR SELF:LABEL = "<<<" AND pcMode = "backward"
 THEN DO:
    ASSIGN
     {&WINDOW-NAME}:WIDTH-PIXELS  = SELF:X + SELF:WIDTH-PIXELS
     {&WINDOW-NAME}:HEIGHT-PIXELS = giWinHeight
     SELF:LABEL = ">>>".
END.

ELSE
 IF  SELF:LABEL = ">>>" AND pcMode = "forward"
  OR SELF:LABEL = "<<"  AND pcMode = "backward"
 THEN DO:
    ASSIGN
     {&WINDOW-NAME}:WIDTH-PIXELS  = giWinWidthPixelsMax
     {&WINDOW-NAME}:HEIGHT-PIXELS = giWinHeightPixelsMax
     SELF:LABEL = "<<<".
    IF {&WINDOW-NAME}:X + {&WINDOW-NAME}:WIDTH-PIXELS > SESSION:WORK-AREA-WIDTH-PIXELS THEN
     ASSIGN
      giPreferencesWinXOffset = {&WINDOW-NAME}:X + {&WINDOW-NAME}:WIDTH-PIXELS - SESSION:WORK-AREA-WIDTH-PIXELS + 7
      {&WINDOW-NAME}:X = {&WINDOW-NAME}:X - giPreferencesWinXOffset.
END.

ELSE
 IF  SELF:LABEL = "<<<" AND pcMode = "forward"
  OR SELF:LABEL = ">>"   AND pcMode = "backward"
 THEN DO:
    ASSIGN
     {&WINDOW-NAME}:WIDTH-PIXELS  = SELF:X + SELF:WIDTH-PIXELS
     {&WINDOW-NAME}:HEIGHT-PIXELS = giWinHeight
     SELF:LABEL = "<<".
    IF giPreferencesWinXOffset > 0 THEN ASSIGN
     {&WINDOW-NAME}:X = {&WINDOW-NAME}:X + giPreferencesWinXOffset
     giPreferencesWinXOffset = 0.
END.

ELSE
 IF  SELF:LABEL = "<<" AND pcMode = "forward"
  OR SELF:LABEL = ">>>"  AND pcMode = "backward"
 THEN DO:
    ASSIGN
     {&WINDOW-NAME}:WIDTH-PIXELS  = SELF:X + SELF:WIDTH-PIXELS
     {&WINDOW-NAME}:HEIGHT-PIXELS = SELF:Y + SELF:HEIGHT-PIXELS
     SELF:LABEL = ">>".
END.

giWinWidth = {&WINDOW-NAME}:WIDTH-PIXELS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE extractWordN C-Win
PROCEDURE extractWordN :
/*------------------------------------------------------------------------------
  Purpose:     Returns the n'th word of a line starting from the position of the cursor
     0  => current word
    -1  => previous word
    -2  => the word before the previous word
     1  => next word  /* 26-AUG-2007 sla: implementing today */

    Returns an error if the wanted word does not exist.
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER piWord       AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER pcLine       AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER piCursorChar AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER pcOptn       AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER opcWord      AS CHARACTER  NO-UNDO.

DEFINE VARIABLE iPrevBreak  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNextBreak  AS INTEGER    NO-UNDO.
DEFINE VARIABLE cWord       AS CHARACTER  NO-UNDO.

/* 08-SEP-2008 sla: added ´()partOfWords´  option to support OO ABL multi level completion */
/* 27-AUG-2007 sla: perf improvement: set these flags once and for all */
DEFINE VARIABLE lcloseParPartOfWords    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcolonPartOfWords       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcommaPartOfWords       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ldoubleQuotePartOfWords AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lopenParPartOfWords     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lperiodPartOfWords      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSingleQuotePartOfWords AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSlashPartOfWords       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lprotectOKMatchingPar   AS LOGICAL   NO-UNDO.


ASSIGN
 lcloseParPartOfWords    = LOOKUP(")PartOfWords", pcOptn) > 0
 lColonPartOfWords       = LOOKUP(":PartOfWords", pcOptn) > 0
 lcommaPartOfWords       = LOOKUP("commaPartOfWords", pcOptn) > 0
 lDoubleQuotePartOfWords = LOOKUP("DoubleQuotePartOfWords", pcOptn) > 0
 lopenParPartOfWords     = LOOKUP("(PartOfWords", pcOptn) > 0
 lperiodPartOfWords      = LOOKUP("periodPartOfWords", pcOptn) > 0
 lSingleQuotePartOfWords = LOOKUP("SingleQuotePartOfWords", pcOptn) > 0
 lSlashPartOfWords       = LOOKUP("/PartOfWords", pcOptn) > 0
 lprotectOKMatchingPar   = LOOKUP("protectOKMatchingPar", pcOptn) > 0.  /* 09-SEP-2008 sla: support of new option to keep parameters in chained methods */


IF piCursorChar < 1 THEN RETURN. /* unvalid, may happen when we query prev prev word */

/* 31-OCT-2008 sla: note the  - 1   in the length option is important for alias completion like "str" to insert STRING(  )  in front of an item
   If you want to really let up to the cursor, then use new option "upToCursor" */
IF CAN-DO(pcOptn, "doNotPassCursor") THEN pcLine = SUBSTRING(pcLine, 1, piCursorChar - 1) NO-ERROR.
IF CAN-DO(pcOptn, "upToCursor")      THEN pcLine = SUBSTRING(pcLine, 1, piCursorChar) NO-ERROR.

ASSIGN
 pcLine     = pcLine + " ". /* so if a line ends with no space, I will be able to consider the last word */
 iPrevBreak = 32000.

IF piWord <= 0 THEN DO:
    IF lprotectOKMatchingPar THEN pcLine = DYNAMIC-FUNCTION('protectWrappedContent' IN ghParser, pcLine, "parentheses", piWord).

    IF INDEX(pcLine, " ", piCursorChar) > 0 THEN iPrevBreak = MINIMUM(iPrevBreak, INDEX(pcLine, " ", piCursorChar)).
    IF lcommaPartOfWords = NO AND INDEX(pcLine, ",", piCursorChar) > 0 THEN iPrevBreak = MINIMUM(iPrevBreak, INDEX(pcLine, ",", piCursorChar)).

    IF lperiodPartOfWords = NO AND INDEX(pcLine, ".", piCursorChar) > 0 THEN iPrevBreak = MINIMUM(iPrevBreak, INDEX(pcLine, ".", piCursorChar)).

    IF lColonPartOfWords = NO AND INDEX(pcLine, ":", piCursorChar) > 0 THEN iPrevBreak = MINIMUM(iPrevBreak, INDEX(pcLine, ":", piCursorChar)).

    IF lcloseParPartOfWords = NO AND INDEX(pcLine, ")", piCursorChar) > 0 THEN iPrevBreak = MINIMUM(iPrevBreak, INDEX(pcLine, ")", piCursorChar)).

    IF lopenParPartOfWords = NO AND INDEX(pcLine, "(", piCursorChar) > 0 THEN iPrevBreak = MINIMUM(iPrevBreak, INDEX(pcLine, "(", piCursorChar)).

    IF lSlashPartOfWords = NO AND INDEX(pcLine, "/", piCursorChar) > 0 THEN iPrevBreak = MINIMUM(iPrevBreak, INDEX(pcLine, "/", piCursorChar)).

    IF lSingleQuotePartOfWords = NO AND INDEX(pcLine, "'", piCursorChar) > 0 THEN iPrevBreak = MINIMUM(iPrevBreak, INDEX(pcLine, "'", piCursorChar)).

    IF lDoubleQuotePartOfWords = NO AND INDEX(pcLine, '"', piCursorChar) > 0 THEN iPrevBreak = MINIMUM(iPrevBreak, INDEX(pcLine, '"', piCursorChar)).

    /*iPrevBreak = MAXIMUM(piCursorChar, iPrevBreak) + 1.*/
    /*iPrevBreak = iPrevBreak . /* we will start with its value minus one */*/

    IF LOOKUP("trace", pcOptn) > 0 THEN DO:
        cmon:SCREEN-VALUE IN FRAME fMain = "".
        cmon:INSERT-STRING("extractWordN: " + STRING(piWord) + " " + STRING(piCursorChar)
         + " " + STRING(iPrevBreak)).
    END.

    DO WHILE TRUE:
        IF piWord > 0 THEN LEAVE.
        pcLine  = SUBSTRING(pcLine, 1, iPrevBreak - 1).

        IF pcLine = "" THEN RETURN ERROR. /* begining of line earlier than wanted word */

        iPrevBreak = MAXIMUM(R-INDEX(pcLine, " ")
                            ,IF lperiodPartOfWords      = NO THEN R-INDEX(pcLine, ".") ELSE 0
                            ,IF lSingleQuotePartOfWords = NO THEN R-INDEX(pcLine, "'") ELSE 0
                            ,IF lDoubleQuotePartOfWords = NO THEN R-INDEX(pcLine, '"') ELSE 0
                            ,IF lcommaPartOfWords       = NO THEN R-INDEX(pcLine, ",") ELSE 0
                            ,IF lColonPartOfWords       = NO THEN R-INDEX(pcLine, ":") ELSE 0
                            ,IF lcloseParPartOfWords    = NO THEN R-INDEX(pcLine, ")") ELSE 0
                            ,IF lopenParPartOfWords     = NO THEN R-INDEX(pcLine, "(") ELSE 0
                            ,IF lSlashPartOfWords       = NO THEN R-INDEX(pcLine, "/") ELSE 0
                            ).

        cWord = SUBSTRING(pcLine, iPrevBreak + 1).
        IF TRIM(cWord) > "" /* do not count spaces as words */
         OR LOOKUP("allowEmptyWord", pcOptn) > 0
         THEN piWord = piWord + 1.
    END.

    opcWord = SUBSTRING(pcLine, iPrevBreak + 1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
END.

ELSE DO:  /*  piWord > 0   */
    ASSIGN
     pcLine     = SUBSTRING(pcLine, MAXIMUM(1, piCursorChar))
     iNextBreak = 32000.

    IF lprotectOKMatchingPar THEN pcLine = DYNAMIC-FUNCTION('protectWrappedContent' IN ghParser, pcLine, "parentheses", piWord).

    IF INDEX(pcLine, " ") > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(pcLine, " ")).


    IF lcommaPartOfWords = NO AND INDEX(pcLine, ",") > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(pcLine, ",")).

    IF lperiodPartOfWords = NO AND INDEX(pcLine, ".") > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(pcLine, ".")).

    IF lColonPartOfWords = NO AND INDEX(pcLine, ":") > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(pcLine, ":")).

    IF lcloseParPartOfWords = NO AND INDEX(pcLine, ")") > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(pcLine, ")")).

    IF lopenParPartOfWords = NO AND INDEX(pcLine, "(") > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(pcLine, "(")).

    IF lSlashPartOfWords = NO AND INDEX(pcLine, "/") > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(pcLine, "/")).

    IF lSingleQuotePartOfWords = NO AND INDEX(pcLine, "'") > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(pcLine, "'")).

    IF lDoubleQuotePartOfWords = NO AND INDEX(pcLine, '"') > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(pcLine, '"')).

    /*iNextBreak = MAXIMUM(piCursorChar, iNextBreak) + 1.*/
    /*iNextBreak = iNextBreak . /* we will start with its value minus one */*/

    IF LOOKUP("trace", pcOptn) > 0 THEN DO:
        cmon:SCREEN-VALUE IN FRAME fMain = "".
        cmon:INSERT-STRING("extractWordN: " + STRING(piWord) + " " + STRING(piCursorChar)
         + " " + STRING(iNextBreak)).
    END.
    DO WHILE TRUE:
        IF piWord < 1 THEN LEAVE.
        pcLine  = SUBSTRING(pcLine, iNextBreak + 1).

        IF pcLine = "" THEN RETURN ERROR. /* begining of line earlier than wanted word */

        iNextBreak = MINIMUM(INDEX(pcLine, " ")
                            ,IF lperiodPartOfWords      = NO THEN MAXIMUM(32000,INDEX(pcLine, ".")) ELSE 32000
                            ,IF lSingleQuotePartOfWords = NO THEN MAXIMUM(32000,INDEX(pcLine, "'")) ELSE 32000
                            ,IF lDoubleQuotePartOfWords = NO THEN MAXIMUM(32000,INDEX(pcLine, '"')) ELSE 32000
                            ,IF lcommaPartOfWords       = NO THEN MAXIMUM(32000,INDEX(pcLine, ",")) ELSE 32000
                            ,IF lColonPartOfWords       = NO THEN MAXIMUM(32000,INDEX(pcLine, ":")) ELSE 32000
                            ,IF lcloseParPartOfWords    = NO THEN MAXIMUM(32000,INDEX(pcLine, ")")) ELSE 32000
                            ,IF lopenParPartOfWords     = NO THEN MAXIMUM(32000,INDEX(pcLine, "(")) ELSE 32000
                            ,IF lSlashPartOfWords       = NO THEN MAXIMUM(32000,INDEX(pcLine, "/")) ELSE 32000
                            ).

        cWord = SUBSTRING(pcLine, 1, iNextBreak - 1).
        IF TRIM(cWord) > "" /* do not count spaces as words */
         OR LOOKUP("allowEmptyWord", pcOptn) > 0
         THEN piWord = piWord - 1.
    END.

    opcWord = SUBSTRING(pcLine, 1, iNextBreak - 1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
END.

/* put the parentheses back */
IF lprotectOKMatchingPar THEN ASSIGN
 opcWord = REPLACE(opcWord, CHR(1), "(")
 opcWord = REPLACE(opcWord, CHR(2), ")").
 opcWord = REPLACE(opcWord, CHR(3), " ").
 opcWord = REPLACE(opcWord, CHR(4), ",").
 opcWord = REPLACE(opcWord, CHR(5), ":").
 opcWord = REPLACE(opcWord, CHR(6), "'").
 opcWord = REPLACE(opcWord, CHR(7), '"').

IF LOOKUP("trace", pcOptn) > 0 THEN cMon:INSERT-STRING("found word:" + opcWord).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findAppBuilder C-Win
PROCEDURE findAppBuilder :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:   14-SEP-2007 sla: more reliable proof to catch the AppBuilder Window
------------------------------------------------------------------------------*/

/* 05-JAN-2007 sla: We will probably need this guy soon to add a penguin button on it */
DEFINE VARIABLE hCheckFirstMenuItem    AS HANDLE     NO-UNDO.
ghMainAppBuilderWindow = SESSION:FIRST-CHILD.

DO WHILE ghMainAppBuilderWindow <> ?:
    /* 14-SEP-2007 sla: as asked by Jan Keirse, some people change the title of the AppBuilder, so let's be more flexible' */
    /* IF ghMainAppBuilderWindow:TITLE = "AppBuilder" THEN DO: */
    IF ghMainAppBuilderWindow:TITLE MATCHES "*AppBuilder" THEN DO:

        /* 14-SEP-2007 sla: safer test afterward */
        /* hCheckFirstMenuItem = ghMainAppBuilderWindow:MENU-BAR NO-ERROR. */
        /*IF VALID-HANDLE(hCheckFirstMenuItem) THEN hCheckFirstMenuItem = hCheckFirstMenuItem:FIRST-CHILD NO-ERROR. */
        /*IF   VALID-HANDLE(hCheckFirstMenuItem)                                                                    */
        /* AND hCheckFirstMenuItem:LABEL = "&File"                                                                  */
        /* THEN LEAVE.                                                                                              */

        hCheckFirstMenuItem = ghMainAppBuilderWindow:MENU-BAR NO-ERROR.
        IF VALID-HANDLE(hCheckFirstMenuItem) THEN hCheckFirstMenuItem = hCheckFirstMenuItem:LAST-CHILD NO-ERROR.

        /* 06-DEC-2007 bot: Make sure we have the "&Help" menu, since this is not always the last one with modified appbuilders */
        IF   VALID-HANDLE(hCheckFirstMenuItem)
         AND hCheckFirstMenuItem:LABEL <> "&Help":U THEN DO WHILE hCheckFirstMenuItem <> ?:
           ASSIGN hCheckFirstMenuItem = hCheckFirstMenuItem:PREV-SIBLING NO-ERROR. /*Walk back from the last menuitem*/
           IF   VALID-HANDLE(hCheckFirstMenuItem)
            AND hCheckFirstMenuItem:LABEL = "&Help":U
              THEN LEAVE. /* We found the correct menuitem*/
         END.
        /* 06-DEC-2007 bot: now return to the "About AppBuilder" check*/

        IF   VALID-HANDLE(hCheckFirstMenuItem)
         AND hCheckFirstMenuItem:LABEL = "&Help" THEN DO:
            hCheckFirstMenuItem = hCheckFirstMenuItem:LAST-CHILD NO-ERROR.

            IF VALID-HANDLE(hCheckFirstMenuItem)
             AND hCheckFirstMenuItem:TYPE = "MENU-ITEM"
             AND hCheckFirstMenuItem:LABEL = "About AppBuilder"
             THEN LEAVE.  /* OK , found a reliable way */
         END.

    END.
    ghMainAppBuilderWindow = ghMainAppBuilderWindow:NEXT-SIBLING.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findSelected C-Win
PROCEDURE findSelected :
/* 15-APR-2007 sla: find next/prev occurence of selected text */
DEFINE INPUT  PARAMETER phEditor    AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcDirection AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cCurrentWord     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDummyBlockName  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFindScope       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCurrentCol      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCurrentLine     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCursorChar      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxLine         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMinLine         AS INTEGER     NO-UNDO.
DEFINE VARIABLE lSearchIsSuccess AS LOGICAL     NO-UNDO.


DEFINE BUFFER ttEdt FOR ttEdt.

IF NOT glFindNextSelected THEN RETURN.

/* 18-AUG-2007 sla: new feature to limit the scope to local if selected so in the abhackfinder window */
ASSIGN
 iCurrentCol  = phEditor:CURSOR-CHAR
 iCurrentLine = phEditor:CURSOR-LINE.

FIND ttEdt WHERE ttEdt.hEditor = phEditor.
IF ttEdt.lManageableSections = NO THEN DO:
    PUBLISH "abhackFindGetFindScope" (OUTPUT cFindScope).
    IF cFindScope = "local" THEN RUN getCurrentBlockBoundaries (phEditor
                                                              ,OUTPUT cDummyBlockName
                                                              ,OUTPUT iMinLine
                                                              ,OUTPUT iMaxLine).
    IF iMaxLine = 0 OR iMinLine = 0 THEN cFindScope = "DontKnowBoundaries".
END.

IF phEditor:TEXT-SELECTED THEN cCurrentWord = phEditor:SELECTION-TEXT.
ELSE DO:
    RUN getEditorLine IN TARGET-PROCEDURE (phEditor, OUTPUT cLine).
    iCursorChar = phEditor:CURSOR-CHAR.

    RUN extractWordN (0  /* word's position */
                     ,cLine
                     ,iCursorChar /* to cursor pos */
                     ,"/PartOfWords"
                     ,OUTPUT cCurrentWord)
                    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN.
END.

CASE pcDirection:
    WHEN 'left'  THEN DO:
        IF phEditor:CURSOR-CHAR  > 1 THEN phEditor:CURSOR-CHAR  = phEditor:CURSOR-CHAR  - 1.
        ELSE IF phEditor:CURSOR-LINE <> 1 THEN ASSIGN
         phEditor:CURSOR-LINE = phEditor:CURSOR-LINE - 1
         phEditor:CURSOR-CHAR  = 5000.
        ELSE ASSIGN
         phEditor:CURSOR-LINE = phEditor:NUM-LINES
         phEditor:CURSOR-CHAR  = 5000.

        /* 22-AUG-2007 sla: removed the + FIND-WRAP-AROUND from the search option */
        lSearchIsSuccess = phEditor:SEARCH(cCurrentWord, FIND-PREV-OCCURRENCE + FIND-SELECT) NO-ERROR.
        /* The point of this search is to switch back to forward search
          so a next alt-right/left without any selected text does not behave in a mad way */
        IF lSearchIsSuccess THEN DO:
            IF cFindScope = "local" AND phEditor:CURSOR-LINE < iMinLine THEN DO:
                ASSIGN
                 phEditor:CURSOR-LINE = iCurrentLine
                 phEditor:CURSOR-CHAR = iCurrentCol.
                PUBLISH "abhackFindBlinkRed".
            END.

            ELSE DO:
                /* 22-AUG-2007 sla: removed the + FIND-WRAP-AROUND from the search option */
                phEditor:SEARCH(cCurrentWord, FIND-NEXT-OCCURRENCE + FIND-SELECT) NO-ERROR.
                phEditor:CURSOR-CHAR = phEditor:CURSOR-CHAR - LENGTH(phEditor:SELECTION-TEXT).
            END.
        END.


    END.
    WHEN 'right' THEN DO:
        /* 22-AUG-2007 sla: removed the + FIND-WRAP-AROUND from the search option */
        phEditor:SEARCH(cCurrentWord, FIND-NEXT-OCCURRENCE + FIND-SELECT) NO-ERROR.
        IF cFindScope = "local" AND phEditor:CURSOR-LINE > iMaxLine THEN DO:
            ASSIGN
             phEditor:CURSOR-LINE = iCurrentLine
             phEditor:CURSOR-CHAR = iCurrentCol.
            PUBLISH "abhackFindBlinkRed".
        END.
    END.
END CASE.


/* 17-APR-2007 sla: the following code is just equivalent to F9 or shift-F9
 as it was a bit hard to achieve I keep it there just in case */
/* if no text selected, then search the same expression as last find done with ctrl-F */
/* ELSE DO:                                                                          */
/*     CASE pcDirection:                                                             */
/*         WHEN 'left'  THEN DO:                                                     */
/*             /* weird, but it seems we have to do it twice to do what we want ! */ */
/*             /* phEditor:SOURCE-COMMAND("find_prev", ''). */                       */
/*             phEditor:SOURCE-COMMAND("find_prev", '').                             */
/*         END.                                                                      */
/*         WHEN 'right' THEN phEditor:SOURCE-COMMAND("find_next", '').               */
/*     END CASE.                                                                     */
/* END.                                                                              */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gcDumpedResourceFileRootValueChanged C-Win
PROCEDURE gcDumpedResourceFileRootValueChanged :
/*------------------------------------------------------------------------------
  Purpose: This proc is required because a trigger can call itself recursively
     with the APPLY statement...
  Author slacroix   10-AUG-2007
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cScreenValue  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iCursorOffset AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iFlash        AS INTEGER     NO-UNDO.

    gcDumpedResourceFileRoot = gcDumpedResourceFileRoot:SCREEN-VALUE IN FRAME fMisc.

    IF INDEX(gcDumpedResourceFileRoot, "\") > 0 THEN ASSIGN
     iCursorOffset                          = gcDumpedResourceFileRoot:CURSOR-OFFSET
     gcDumpedResourceFileRoot               = REPLACE(gcDumpedResourceFileRoot, "\", "/")
     gcDumpedResourceFileRoot:SCREEN-VALUE  = gcDumpedResourceFileRoot
     gcDumpedResourceFileRoot:CURSOR-OFFSET = iCursorOffset.

    FILE-INFO:FILE-NAME = gcDumpedResourceFileRoot.

    IF FILE-INFO:FULL-PATHNAME = ?
     OR INDEX(FILE-INFO:FILE-TYPE, "D") = 0
     THEN DO:
        ASSIGN
         gcDumpedResourceFileRoot:TOOLTIP = ENTRY(1, gcDumpedResourceFileRoot:TOOLTIP, "~n") + "~nINVALID DIRECTORY !!"
         cScreenValue                     = gcDumpedResourceFileRoot. /* to find out the value has changed during the PROCESS EVENTS */

        IF gcGlobalResCat = "XML" THEN ASSIGN
         gcGlobalResCat              = 'disabled'
         gcGlobalResCat:SCREEN-VALUE = gcGlobalResCat.


        DO iFlash = 1 TO 4:
            IF iFlash > 1 THEN RUN Sleep (100).
            gcDumpedResourceFileRoot:BGCOLOR = 10.
            PROCESS EVENTS. /* flush the UI */
            IF cScreenValue <> gcDumpedResourceFileRoot:SCREEN-VALUE THEN DO:
                RUN gcDumpedResourceFileRootValueChanged.
                RETURN.  /* the screen value has been changed during the PROCESS EVENTS... */
            END.
            RUN Sleep (100).
            gcDumpedResourceFileRoot:BGCOLOR = 12.
            PROCESS EVENTS. /* slush the UI */
            IF cScreenValue <> gcDumpedResourceFileRoot:SCREEN-VALUE THEN DO:
                RUN gcDumpedResourceFileRootValueChanged.
                RETURN.  /* the screen value has been changed during the PROCESS EVENTS... */
            END.
        END.
        RETURN ERROR.
    END.
    ELSE ASSIGN
     gcDumpedResourceFileRoot:BGCOLOR = 10
     gcDumpedResourceFileRoot:TOOLTIP = ENTRY(1, gcDumpedResourceFileRoot:TOOLTIP, "~n").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generateListing C-Win
PROCEDURE generateListing :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor   AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER icWhatList AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cFileName     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListing      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCompileEtime AS DECIMAL     NO-UNDO.

IF icWhatList = "list" AND NOT glListingOnCtrlAltT THEN RETURN.
IF icWhatList = "debug" AND NOT glDebugListingOnCtrlAltD THEN RETURN.

cMon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

RUN getEditorFileName(phEditor, OUTPUT cFileName) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    cMon:SCREEN-VALUE = cMon:SCREEN-VALUE + "~n can find this file on disk, aborting!".
    RETURN "invalid file".
END.

iCompileEtime = DECIMAL(ETIME).
cListing = cFileName.
ENTRY(NUM-ENTRIES(cListing, "."), cListing, ".") = (IF icWhatList = "debug" THEN  "dbg" ELSE "") + "lis.p". /* .p extension to let AppBuilder open it as a simple procedure with appropriate syntax color */

cMon:SCREEN-VALUE = "Compile & Listing in process... ".
IF icWhatList = "list" THEN
  COMPILE VALUE(cFilename) LISTING VALUE(cListing)  NO-ERROR.
ELSE IF icWhatList = "debug" THEN
  COMPILE VALUE(cFilename) DEBUG-LIST VALUE(cListing)  NO-ERROR.

iCompileEtime = DECIMAL(ETIME) - iCompileEtime.

cMon:SCREEN-VALUE = cMon:SCREEN-VALUE + "~nDone in " + STRING(iCompileEtime) + "ms~n"
 + IF COMPILER:ERROR-ROW = ? OR ERROR-STATUS:GET-NUMBER(1) = 6430
      /* 6430 =  SAVE not specified, and r-code file C:\v10Tools\protools\abhack\procEditorTriggers.r exists. List/xref/preprocess/debug-list listing will proceed, but no r-code will be generated. (6430) */
    THEN "OK"
    ELSE "failed with this error" + ERROR-STATUS:GET-MESSAGE(1) + " (do shift-F2 for more details)".

IF   COMPILER:ERROR-ROW <> ?
 AND ERROR-STATUS:GET-NUMBER(1) <> 6430
 AND ERROR-STATUS:NUM-LOCKED-COLUMNS <> 1 THEN DO:
    DEFINE VARIABLE iFlash      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iMonBgColor AS INTEGER   NO-UNDO.
    iMonBgColor = cmon:BGCOLOR.

    DO iFlash = 1 TO 4:
        IF iFlash > 1 THEN RUN Sleep (100).
        cmon:BGCOLOR = 0.
        PROCESS EVENTS.
        RUN Sleep (100).
        cmon:BGCOLOR = iMonBgColor.
        PROCESS EVENTS.
    END.
END.

RUN openFile (cListing).
cListing = SEARCH(cListing).
OS-DELETE VALUE(cListing). /* do not keep this temp-file */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getAbhackFindProfile C-Win
PROCEDURE getAbhackFindProfile :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opcAbhackFindScope AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcAbhackFindMru   AS CHARACTER   NO-UNDO.

ASSIGN
 opcAbhackFindScope = gcAbhackFindScope
 opcAbhackFindMru   = gcAbhackFindMru.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getAliasLargeText C-Win
PROCEDURE getAliasLargeText :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcExp        AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcLargeText AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttCustAlias FOR ttCustAlias.

FIND FIRST ttCustAlias WHERE ttCustAlias.cExp = pcExp NO-ERROR.
opcLargeText = IF AVAILABLE ttCustAlias
                THEN refinedAlias(ttCustAlias.cLargeText)
                ELSE "Corrupted alias table ttCustAlias, no record for " + QUOTER(pcExp).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getBuffer C-Win
PROCEDURE getBuffer :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcBuffBegins  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcBuffer     AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cDbName     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iDbNum      AS INTEGER    NO-UNDO.
DEFINE VARIABLE hFileBuffer AS HANDLE     NO-UNDO.
DEFINE VARIABLE cDbNameList AS CHARACTER  NO-UNDO.

DEFINE BUFFER ttDbTableCache FOR ttDbTableCache.

/* 18-JAN-2007 sla: if a database name is passed as input, then limit the database
 list to it plus prepend the opcBuffer with it as the end of the process */
DEFINE VARIABLE cInputDbName AS CHARACTER   NO-UNDO.

IF NUM-ENTRIES(pcBuffBegins, ".") = 2 THEN ASSIGN
 cInputDbName = ENTRY(1, pcBuffBegins, ".")
 pcBuffBegins = ENTRY(2, pcBuffBegins, ".").

/* 18-MAY-2007 sla: use new local schema cache to improve performances */
IF glUseSchemaCache THEN DO:
    IF cInputDbName > "" THEN DO:
        FIND FIRST ttDbTableCache WHERE
             ttDbTableCache.cDataBase = cInputDbName
         AND ttDbTableCache.cTable BEGINS pcBuffBegins
         NO-ERROR.
        IF AVAILABLE ttDbTableCache THEN opcBuffer = cInputDbName + "." + ttDbTableCache.cTable.
        RETURN.
    END. /* IF cInputDbName > ""  */

    FIND FIRST ttDbTableCache WHERE ttDbTableCache.cTable BEGINS pcBuffBegins NO-ERROR.
    IF AVAILABLE ttDbTableCache THEN opcBuffer = ttDbTableCache.cTable.
    RETURN.
END.


/* go dynamically through the schema */
DO iDbNum = 1 TO NUM-DBS:
    /* 18-JAN-2007 sla: limit database list to the one explicitely passed in input if any */
    IF cInputDbName > "" AND LDBNAME(iDbNum) <> cInputDbName THEN NEXT.

    IF CAN-DO("PROGRESS,OpenEdge", DBTYPE(iDbNum))
     THEN cDbNameList = cDbNameList + "," + LDBNAME(iDbNum).
END.

/* I want lddb to appear in last position in the list, so I will
first find buffers that begins something in the DB I wish
  => Feel free to refine it to your needs*/
IF   LOOKUP("lddb", cDbNameList) > 0
 AND LOOKUP("lddb", cDbNameList) < NUM-ENTRIES(cDbNameList) THEN DO:
    cDbNameList = REPLACE(cDbNameList, ",lddb", "").
    cDbNameList = cDbNameList + ",lddb".
END.

cDbNameList = SUBSTRING(cDbNameList, 2) NO-ERROR. /* error if empty => ignore */

DO iDbNum = 1 TO NUM-ENTRIES(cDbNameList):
    cDbName = ENTRY(iDbNum, cDbNameList).

    CREATE BUFFER hFileBuffer FOR TABLE cDbName + "._file" BUFFER-NAME "yo".
    hFileBuffer:FIND-FIRST("WHERE yo._file-name BEGINS " + QUOTER(pcBuffBegins), NO-LOCK) NO-ERROR.
    IF hFileBuffer:AVAILABLE THEN DO:
        cMon:SCREEN-VALUE IN FRAME fMain = cMon:SCREEN-VALUE + "~n" + cDbName.
        opcBuffer = hFileBuffer:BUFFER-FIELD("_file-name"):BUFFER-VALUE.
        DELETE OBJECT hFileBuffer.

        IF cInputDbName > "" THEN opcBuffer = cInputDbName + "." + opcBuffer.
        RETURN.
    END.
    DELETE OBJECT hFileBuffer.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getBufferByFLA C-Win
PROCEDURE getBufferByFLA :
/*------------------------------------------------------------------------------
  Purpose:     FLA = Five Letter Acronym
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcFLA     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pcDbName  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcBuffer AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cCaseSensitive  AS CHARACTER   CASE-SENSITIVE NO-UNDO.
DEFINE VARIABLE cDbName         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDbNameList     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDumpFile       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hbFile          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hqFile          AS HANDLE      NO-UNDO.
DEFINE VARIABLE iBufferListSize AS INTEGER     NO-UNDO.
DEFINE VARIABLE iDbNum          AS INTEGER     NO-UNDO.
DEFINE VARIABLE lMthRtn         AS LOGICAL     NO-UNDO.

DEFINE BUFFER ttDbTableCache FOR ttDbTableCache.

/* avoid sytem error  */
IF  INDEX( pcFLA, "~{") > 0
 OR INDEX( pcFLA, "~}") > 0
 OR INDEX( pcFLA, "&") > 0
 THEN RETURN.

IF INDEX(pcDbName, "/") > 0 OR INDEX(pcDbName, " ") > 0 OR INDEX(pcDbName, "\") > 0 THEN RETURN. /* 19-MAR-2007 sla: fix, avoid error 1016  */

IF pcDbName > "" AND NOT CONNECTED(pcDbName) THEN RETURN. /* can especially occur when typing a field name after a table that was passed as a database name here  */


/* 18-MAY-2007 sla: use new local schema cache to improve performances */
IF glUseSchemaCache THEN DO:
    IF pcDbName > "" THEN DO:
        FIND FIRST ttDbTableCache WHERE
             ttDbTableCache.cDataBase = pcDbName
         AND ttDbTableCache.cFLA      = pcFLA
         NO-ERROR.
        IF AVAILABLE ttDbTableCache THEN opcBuffer = pcDbName + "." + ttDbTableCache.cFLA.
        RETURN.
    END. /* IF cInputDbName > ""  */

    FIND FIRST ttDbTableCache WHERE ttDbTableCache.cFLA = pcFLA NO-ERROR.
    IF AVAILABLE ttDbTableCache THEN opcBuffer = ttDbTableCache.cTable.

    /* 15-NOV-2007 sla: this part was missing to convert a 100% upper case table to LC */
    IF glConvertDbTablesToLC THEN DO:
        cCaseSensitive = opcBuffer.
        IF CAPS(opcBuffer) = cCaseSensitive THEN opcBuffer = LC(opcBuffer).
    END.
    RETURN.
END.


/* MAR-15-2007 sla: filter on input dbName */
IF pcDbName > "" THEN cDbNameList = pcDbName.
ELSE DO:  /* dbname not passed as param => build the list */
    DO iDbNum = 1 TO NUM-DBS:
        IF CAN-DO("PROGRESS,OpenEdge", DBTYPE(iDbNum))
         THEN cDbNameList = cDbNameList + "," + LDBNAME(iDbNum).
    END.
    /* I want lddb to appear in last position in the list, so I will
    first find buffers that begins something in the DB I wish
      => Feel free to refine it to your needs*/
    IF   LOOKUP("lddb", cDbNameList) > 0
     AND LOOKUP("lddb", cDbNameList) < NUM-ENTRIES(cDbNameList) THEN DO:
        cDbNameList = REPLACE(cDbNameList, ",lddb", "").
        cDbNameList = cDbNameList + ",lddb".
    END.

    cDbNameList = SUBSTRING(cDbNameList, 2). /* does not raise any error if empty*/
END.

DO iDbNum = 1 TO NUM-ENTRIES(cDbNameList):
    cDbName = ENTRY(iDbNum, cDbNameList).

    CREATE BUFFER hbFile FOR TABLE cDbName + "._file" BUFFER-NAME "yo".
    CREATE QUERY hqFile.
    hqFile:SET-BUFFERS(hbFile).

    lMthRtn = hqFile:QUERY-PREPARE("FOR EACH yo NO-LOCK WHERE yo._dump-name = " + QUOTER(pcFLA)) NO-ERROR.
    IF lMthRtn THEN lMthRtn = hqFile:QUERY-OPEN() NO-ERROR.
    IF lMthRtn THEN DO WHILE TRUE:
        hqFile:GET-NEXT().
        IF hqFile:QUERY-OFF-END THEN LEAVE.

        cDumpFile = LC(hbFile:BUFFER-FIELD("_file-name"):BUFFER-VALUE).
        iBufferListSize = iBufferListSize + LENGTH(cDumpFile).
        /* protection against 32k limit, which may happen in environement with many tables */
        IF iBufferListSize > 31900 THEN LEAVE.
        opcBuffer = opcBuffer + "," + cDumpFile.
    END.
    DELETE OBJECT hqFile.
    DELETE OBJECT hbFile.
END.
opcBuffer = SUBSTR(opcBuffer, 2).

/* 15-NOV-2007 sla: this part was missing to convert a 100% upper case table to LC */
IF glConvertDbTablesToLC THEN DO:
    cCaseSensitive = opcBuffer.
    IF CAPS(opcBuffer) = cCaseSensitive THEN opcBuffer = LC(opcBuffer).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getBufferList C-Win
PROCEDURE getBufferList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor      AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER pcBuffBegins  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pcDbName      AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcBufferList AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cBufferName     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCaseSensitive  AS CHARACTER   NO-UNDO CASE-SENSITIVE.
DEFINE VARIABLE cDbName         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDbNameList     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hbFile          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hqFile          AS HANDLE      NO-UNDO.
DEFINE VARIABLE iBufferListSize AS INTEGER     NO-UNDO.
DEFINE VARIABLE iDbNum          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iEntry          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxEntry       AS INTEGER     NO-UNDO.
DEFINE VARIABLE lMthRtn         AS LOGICAL     NO-UNDO.

DEFINE BUFFER tttt           FOR tttt.
DEFINE BUFFER ttDbTableCache FOR ttDbTableCache.
DEFINE BUFFER ttbuffer       FOR ttbuffer.
DEFINE BUFFER ttgbuffer      FOR ttgbuffer.

IF INDEX(pcDbName, "/") > 0 OR INDEX(pcDbName, " ") > 0 OR INDEX(pcDbName, "\") > 0 THEN RETURN. /* 19-MAR-2007 sla: fix, avoid error 1016  */

IF pcDbName > "" AND NOT CONNECTED(pcDbName) THEN RETURN. /* can especially occur when typing a field name after a table that was passed as a database name here  */


/* 28-NOV-2006 sla: support of local and global buffers */
FOR EACH ttbuffer NO-LOCK WHERE ttbuffer.cName BEGINS pcBuffBegins:
    IF LOOKUP(ttbuffer.cName, opcBufferList) = 0
     THEN opcBufferList = opcBufferList + "," + ttbuffer.cName.
END.

FOR EACH ttgbuffer NO-LOCK WHERE
     ttgbuffer.hEditor = phEditor
 AND ttgbuffer.cName BEGINS pcBuffBegins:
    IF LOOKUP(ttgbuffer.cName, opcBufferList) = 0
     THEN opcBufferList = opcBufferList + "," + ttgbuffer.cName.
END.

/* 27-NOV-2006 sla: support of temp-tables */
FOR EACH tttt WHERE
     tttt.hEditor = phEditor
 AND tttt.cttname BEGINS pcBuffBegins:
    IF LOOKUP(tttt.cttName, opcBufferList) = 0
     THEN opcBufferList = opcBufferList + "," + tttt.cttName.
END.


/* avoid sytem error  */
IF  INDEX( pcBuffBegins, "~{") > 0
 OR INDEX( pcBuffBegins, "~}") > 0
 OR INDEX( pcBuffBegins, "&") > 0
 THEN DO: /* but remove leading ',' */
    opcBufferList = SUBSTR(opcBufferList, 2).
    RETURN.
 END.


/* 18-MAY-2007 sla: use new local schema cache to improve performances */
IF glUseSchemaCache THEN DO:
    IF pcDbName > "" THEN
     FOR EACH ttDbTableCache WHERE
          ttDbTableCache.cDataBase = pcDbName
      AND ttDbTableCache.cTable    BEGINS pcBuffBegins:
        opcBufferList = opcBufferList + "," + ttDbTableCache.cTable.
    END. /* IF cInputDbName > ""  */
    ELSE  FOR EACH ttDbTableCache WHERE ttDbTableCache.cTable BEGINS pcBuffBegins:
        /* 06-SEP-2007 sla: this code could lead to having a buffer twice in the list */
        IF LOOKUP(ttDbTableCache.cTable, opcBufferList) = 0
         THEN opcBufferList = opcBufferList + "," + ttDbTableCache.cTable.
    END. /* IF cInputDbName > ""  */
END.
ELSE DO: /* 18-MAY-2007 sla: or go through schema via _file  */
    /* MAR-15-2007 sla: filter on input dbName */
    IF pcDbName > "" THEN cDbNameList = pcDbName.
    ELSE DO:  /* dbname not passed as param => build the list */
        DO iDbNum = 1 TO NUM-DBS:
            IF CAN-DO("PROGRESS,OpenEdge", DBTYPE(iDbNum))
             THEN cDbNameList = cDbNameList + "," + LDBNAME(iDbNum).
        END.
        /* I want lddb to appear in last position in the list, so I will
        first find buffers that begins something in the DB I wish
          => Feel free to refine it to your needs*/
        IF   LOOKUP("lddb", cDbNameList) > 0
         AND LOOKUP("lddb", cDbNameList) < NUM-ENTRIES(cDbNameList) THEN DO:
            cDbNameList = REPLACE(cDbNameList, ",lddb", "").
            cDbNameList = cDbNameList + ",lddb".
        END.

        cDbNameList = SUBSTRING(cDbNameList, 2). /* does not raise any error if empty*/
    END.

    DO iDbNum = 1 TO NUM-ENTRIES(cDbNameList):
        cDbName = ENTRY(iDbNum, cDbNameList).

        CREATE BUFFER hbFile FOR TABLE cDbName + "._file" BUFFER-NAME "yo".
        CREATE QUERY hqFile.
        hqFile:SET-BUFFERS(hbFile).
        lMthRtn = hqFile:QUERY-PREPARE("FOR EACH yo NO-LOCK WHERE yo._file-name BEGINS " + QUOTER(pcBuffBegins)) NO-ERROR.
        IF lMthRtn THEN hqFile:QUERY-OPEN() NO-ERROR.
        IF lMthRtn THEN DO WHILE TRUE:
            hqFile:GET-NEXT().
            IF hqFile:QUERY-OFF-END THEN LEAVE.

            cBufferName = hbFile:BUFFER-FIELD("_file-name"):BUFFER-VALUE.
            iBufferListSize = iBufferListSize + LENGTH(cBufferName).
            /* protection against 32k limit, which may happen in environement with many tables */
            IF iBufferListSize > 31900 THEN LEAVE.
            IF LOOKUP(cBufferName, opcBufferList) = 0
             THEN opcBufferList = opcBufferList + "," + cBufferName.
        END.
        DELETE OBJECT hqFile.
        DELETE OBJECT hbFile.
    END.
END. /* ELSE DO (IF glUseSchemaCache ) */

opcBufferList = SUBSTR(opcBufferList, 2).

/* 13-APR-2007 sla: convert to Lower Case only the guys that are in full caps */
IF glConvertDbTablesToLC THEN DO:
    iMaxEntry = NUM-ENTRIES(opcBufferList).
    DO iEntry = 1 TO iMaxEntry:
        cCaseSensitive = ENTRY(iEntry, opcBufferList).
        IF CAPS(cCaseSensitive) = cCaseSensitive THEN ENTRY(iEntry, opcBufferList) = LC(cCaseSensitive).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCurrentBlockBoundaries C-Win
PROCEDURE getCurrentBlockBoundaries :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor     AS HANDLE      NO-UNDO.
DEFINE OUTPUT PARAMETER opcBlockName AS CHARACTER   NO-UNDO. /* not really need, but will help for debugging */
DEFINE OUTPUT PARAMETER opiStartLine AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiEndLine   AS INTEGER     NO-UNDO.

DEFINE BUFFER ttMark FOR ttMark.

IF NOT VALID-HANDLE(phEditor)
 OR phEditor:TYPE <> "EDITOR"
 THEN RETURN.

opiEndLine = phEditor:NUM-LINES. /* default value */

FIND LAST ttMark WHERE
     ttMark.hEditor = phEditor
 AND ttMark.iLine <= phEditor:CURSOR-LINE
 USE-INDEX editorLine NO-ERROR.
IF NOT AVAILABLE ttMark THEN RETURN.

ASSIGN
 opiStartLine = ttMark.iLine
 opcBlockName = ttMark.cBlockName.

FIND FIRST ttMark WHERE
     ttMark.hEditor = phEditor
 AND ttMark.iLine >= phEditor:CURSOR-LINE
 USE-INDEX editorLine NO-ERROR.
IF NOT AVAILABLE ttMark THEN RETURN.

opiEndLine = ttMark.iLine.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getEditorFileName C-Win
PROCEDURE getEditorFileName :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor    AS HANDLE      NO-UNDO.
DEFINE OUTPUT PARAMETER opcFileName AS CHARACTER   NO-UNDO.

opcFileName = REPLACE(phEditor:WINDOW:TITLE, "\", "/").
opcFileName = SUBSTRING(opcFileName, R-INDEX(opcFileName, ' - ') + 3) NO-ERROR.
IF  ERROR-STATUS:ERROR
 OR opcFileName = ?
 OR opcFileName = ""
 THEN DO:
    opcFileName = ?.
    RETURN ERROR.
END.
IF SEARCH(opcFileName) = ? THEN DO WHILE TRUE:
    /* 27-NOV-2006 sla: I made a nice tool to use shorter path in windows title (because default is to display aboslute path even if long), sadly, they become not reliable, solution is to remove the directory names step by step to rely on propath */
    IF INDEX(opcFileName , "/" ) > 0 THEN DO:
        opcFileName = SUBSTRING(opcFileName, INDEX(opcFileName, "/") + 1) NO-ERROR.
        IF SEARCH(opcFileName) = ? THEN NEXT.
        LEAVE. /* OK we found it */
    END.
    IF SEARCH(opcFileName) = ? THEN DO:
        opcFileName = ?.
        RETURN ERROR.
    END.
END.

opcFileName = REPLACE( SEARCH(opcFileName), '\', '/').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getEditorLine C-Win
PROCEDURE getEditorLine :
/*------------------------------------------------------------------------------
 Purpose:     Get the current line in a given source-code editor
 Rely on SCREEN-VALUE when length < 31000, otherwise, use select_line method
 Avoid using select_line method as this operation will be added to the undo
 stack  => actually, with SOURCE-COMMAND('undo', ''), I can remove the action
 from the undo stack, but it adds a little flash.  So I will use the SCREEN-VALUE
 when below the 32k limit

------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor  AS HANDLE     NO-UNDO.
DEFINE OUTPUT PARAMETER opcLine   AS CHARACTER  NO-UNDO.

DEFINE VARIABLE iCursorLine AS INTEGER    NO-UNDO.
DEFINE VARIABLE iReturnCode AS INTEGER    NO-UNDO.


/* A performance test shows that using entry(3000, SCREEN-VALUE, "~n") is very fast indeed */
IF phEditor:LENGTH < 31000 THEN ASSIGN
 iCursorLine = phEditor:CURSOR-LINE
 /* this might eat some CPU when we are working at a high line number.... */
 opcLine = ENTRY(iCursorLine, phEditor:SCREEN-VALUE, "~n").
ELSE DO:
    RUN lockWindowUpdate  (INPUT phEditor:FRAME:HWND, OUTPUT iReturnCode).
    phEditor:SOURCE-COMMAND('select_line', '').
    opcLine = phEditor:SELECTION-TEXT.
    opcLine = RIGHT-TRIM(opcLine, CHR(10)). /* remove "~n" at the end that comes from the selection process */
    opcLine = RIGHT-TRIM(opcLine, CHR(13)). /* remove "~n" at the end that comes from the selection process */
/*     phEditor:SOURCE-COMMAND('deselect', ''). */
/*     phEditor:SOURCE-COMMAND('undo', '').     */
    phEditor:SOURCE-COMMAND('undo', '').
    RUN lockWindowUpdate (INPUT 0, OUTPUT iReturnCode).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getEditorPrevWord C-Win
PROCEDURE getEditorPrevWord :
/*------------------------------------------------------------------------------
  Purpose:     Actually the current word in front of the cursor...
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor    AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcOptn      AS CHARACTER   NO-UNDO. /* option */
DEFINE OUTPUT PARAMETER opcPrevWord AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cLine       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCursorChar AS INTEGER    NO-UNDO.
DEFINE VARIABLE iPrevBreak  AS INTEGER    NO-UNDO.

iCursorChar = phEditor:CURSOR-CHAR.

RUN getEditorLine(phEditor, OUTPUT cLine).

RUN extractWordN (0              /* word's position */
                 ,cLine
                 ,iCursorChar /* to cursor pos */
                 ,pcOptn
                 ,OUTPUT opcPrevWord)
                NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getInfoWinProfile C-Win
PROCEDURE getInfoWinProfile :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT  PARAMETER oplInfoWinTopOnly AS LOGICAL     NO-UNDO.
DEFINE OUTPUT  PARAMETER opiInfoWinHeight  AS INTEGER     NO-UNDO.
DEFINE OUTPUT  PARAMETER opiInfoWinWidth   AS INTEGER     NO-UNDO.
DEFINE OUTPUT  PARAMETER opiInfoWinX       AS INTEGER     NO-UNDO.
DEFINE OUTPUT  PARAMETER opiInfoWinY       AS INTEGER     NO-UNDO.
DEFINE OUTPUT  PARAMETER opiInfoWinBgcolor AS INTEGER     NO-UNDO.
DEFINE OUTPUT  PARAMETER opiInfoWinFgcolor AS INTEGER     NO-UNDO.

ASSIGN
 oplInfoWinTopOnly = glInfoWinTopOnly
 opiInfoWinHeight  = giInfoWinHeight
 opiInfoWinWidth   = giInfoWinWidth
 opiInfoWinX       = giInfoWinX
 opiInfoWinY       = giInfoWinY
 opiInfoWinBgcolor = giInfoWinBgcolor
 opiInfoWinFgcolor = giInfoWinFgcolor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetInputWidgetInXY C-Win
PROCEDURE GetInputWidgetInXY :
/*------------------------------------------------------------------------------
  Purpose:     I call it recursivelly to get the last frame in the wdget tree
              in a given
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phContainer  AS HANDLE     NO-UNDO.
DEFINE OUTPUT PARAMETER ophWidget     AS HANDLE     NO-UNDO.


DEFINE VARIABLE mMousePos  AS MEMPTR     NO-UNDO.
DEFINE VARIABLE hWidget    AS HANDLE     NO-UNDO.
DEFINE VARIABLE iMouseX    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iMouseY    AS INTEGER    NO-UNDO.
DEFINE VARIABLE hParent    AS HANDLE     NO-UNDO.

SET-SIZE(mMousePos) = 16.
RUN GetCursorPos (INPUT-OUTPUT mMousePos).
RUN ScreenToClient (INPUT phContainer:HWND, INPUT mMousePos).
ASSIGN
 iMouseX = GET-LONG( mMousePos, 1)
 iMouseY = GET-LONG( mMousePos, 5).
SET-SIZE(mMousePos) = 0.

ophWidget = phContainer. /* set it so it returns itself if it can't another child frame in x y */
hWidget  = phContainer:FIRST-CHILD.

/* this will be necessary when phContainer was a frame, but not when a it is window */
IF hWidget:TYPE = "FIELD-GROUP" THEN hWidget = hWidget:FIRST-CHILD.

DO WHILE hWidget <> ?:
    IF hWidget:TYPE = "FRAME"
    /* ignore Smart Folder Frame that does not contain the page frames */
     AND NOT hWidget:NAME MATCHES "*folder*"
     AND hWidget:VISIBLE /* ignore hidden frames, note I am not sure we have a way to
                            catch visible frame partially hidden behind another */
     AND CAN-QUERY(hWidget, "X")
     AND CAN-QUERY(hWidget, "Y")
     AND CAN-QUERY(hWidget, "WIDTH-PIXELS")
     AND CAN-QUERY(hWidget, "HEIGHT-PIXELS")
     AND iMouseX > hWidget:X
     AND iMouseX < hWidget:X + hWidget:WIDTH-PIXELS
     AND iMouseY > hWidget:Y
     AND iMouseY < hWidget:Y + hWidget:HEIGHT-PIXELS
     THEN DO:
        ophWidget = hWidget.
        RUN getInputWidgetInXY (ophWidget, OUTPUT ophWidget).
        RETURN.
    END.
    hWidget = hWidget:NEXT-SIBLING.
END.

/* we get there when we did not discover a new child frame on the way */
/* at this point, ophWidget is the last child frame below the pointer
 and iMouseX and iMouseY are relative to it */
ophWidget = ophWidget:FIRST-CHILD. /* first field group */
ophWidget = ophWidget:FIRST-CHILD.
DO WHILE ophWidget <> ?:
    IF   CAN-QUERY(ophWidget, "VISIBLE")
     AND ophWidget:VISIBLE
     AND (   CAN-QUERY(ophWidget, "SCREEN-VALUE")
          OR CAN-QUERY(ophWidget, "LABEL")) /* 03-APR-2007 sla: adding support for buttons */
     AND CAN-QUERY(ophWidget, "X")
     AND CAN-QUERY(ophWidget, "Y")
     AND CAN-QUERY(ophWidget, "WIDTH-PIXELS")
     AND CAN-QUERY(ophWidget, "HEIGHT-PIXELS")
     AND iMouseX > ophWidget:X
     AND iMouseX < ophWidget:X + ophWidget:WIDTH-PIXELS
     AND iMouseY > ophWidget:Y
     AND iMouseY < ophWidget:Y + ophWidget:HEIGHT-PIXELS
     THEN RETURN.

    ophWidget = ophWidget:NEXT-SIBLING.
END.

/* note that ophWidget will be ? if we get here */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSectionLayoutWinProfile C-Win
PROCEDURE getSectionLayoutWinProfile :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT  PARAMETER oplSectionLayoutWinTopOnly AS LOGICAL     NO-UNDO.
DEFINE OUTPUT  PARAMETER opiSectionLayoutWinHeight  AS INTEGER     NO-UNDO.
DEFINE OUTPUT  PARAMETER opiSectionLayoutWinWidth   AS INTEGER     NO-UNDO.
DEFINE OUTPUT  PARAMETER opiSectionLayoutWinX       AS INTEGER     NO-UNDO.
DEFINE OUTPUT  PARAMETER opiSectionLayoutWinY       AS INTEGER     NO-UNDO.

ASSIGN
 oplSectionLayoutWinTopOnly = glSectionLayoutWinTopOnly
 opiSectionLayoutWinHeight  = giSectionLayoutWinHeight
 opiSectionLayoutWinWidth   = giSectionLayoutWinWidth
 opiSectionLayoutWinX       = giSectionLayoutWinX
 opiSectionLayoutWinY       = giSectionLayoutWinY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hideWindow C-Win
PROCEDURE hideWindow :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:       21-AUG-2007 sla: implemented change submitted by Jan Keirse
------------------------------------------------------------------------------*/
IF {&WINDOW-NAME}:VISIBLE = NO THEN DO:
    ASSIGN
     {&WINDOW-NAME}:WINDOW-STATE = WINDOW-MINIMIZED
     {&WINDOW-NAME}:VISIBLE = YES
     {&WINDOW-NAME}:WINDOW-STATE = WINDOW-NORMAL.
     {&WINDOW-NAME}:MOVE-TO-TOP().

    IF VALID-HANDLE(ghSectionLayoutWin)
     AND VALID-HANDLE(ghSectionLayoutWin:CURRENT-WINDOW)
     THEN ghSectionLayoutWin:CURRENT-WINDOW:VISIBLE = YES.
END. ELSE DO:
    ASSIGN
     {&WINDOW-NAME}:WINDOW-STATE = WINDOW-MINIMIZED
     {&WINDOW-NAME}:VISIBLE      = NO.
    IF VALID-HANDLE(ghSectionLayoutWin)
     AND VALID-HANDLE(ghSectionLayoutWin:CURRENT-WINDOW)
     THEN ghSectionLayoutWin:CURRENT-WINDOW:VISIBLE = NO.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeABHack C-Win
PROCEDURE initializeABHack :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    /* 14-MAY-2007 sla: see code in protools\abhack\openFileInAB.p (WIDGET-POOL cleanup) */
    PUBLISH "ABHackOpenFileInABCleanUp".  /* ask running instances to cleanup */

    RUN setTempDirAndImportFile NO-ERROR. /* 26-MAY-2007 sla: we need to work with a temp privatge directory.  See detail in this procedure */
    IF ERROR-STATUS:ERROR THEN DO:
        RUN disable_UI.
        RETURN ERROR.
    END.

    /* 12-MAR-2007 sla: first restore the setting so the loadttAttr takes glLowerCaseCompletion into account */
    RUN restoreSetting IN TARGET-PROCEDURE.
    IF gcDumpedResourceFileRoot = "" OR gcDumpedResourceFileRoot = ?
     THEN gcDumpedResourceFileRoot = "Type a valid root full path here".

    IF gcGlobalResCat = ? OR gcGlobalResCat = "" THEN gcGlobalResCat = "disabled".

    IF glKeepTrackListingBuffer = ? THEN glKeepTrackListingBuffer = YES.

    gcDumpedResourceFileRoot:SCREEN-VALUE IN FRAME fMisc = gcDumpedResourceFileRoot.

    /* 30-AUG-2007 sla: restructured with new parser library */
    RUN protools/abhack/abhackParser.p PERSISTENT SET ghParser
     (THIS-PROCEDURE
     ,cmon:HANDLE IN FRAME {&FRAME-NAME}).


    RUN loadttCustAlias IN TARGET-PROCEDURE.
    RUN loadttAttr IN TARGET-PROCEDURE.
    RUN loadttAPITooltip IN TARGET-PROCEDURE.


    /* 07-SEP-2007 sla: note I unchecked the 'DISPLAY' option in the porperty sheet
     of the cmon editor, so the screen-value I was maintaining before running
     enable_ui is not lost. */
    RUN enable_UI.

    /* 04-SEP-2007 sla: do that after running enable_UI  so the variable values are in synch with their screen-value
      plus, it is better to have abhackparser.p running before connecting to abhack.db */
    APPLY 'VALUE-CHANGED' TO gcGlobalResCat IN FRAME fMisc.

    RUN setTooltips IN TARGET-PROCEDURE.

    /* handle the image of the button that started ABHack if applicable */
    IF VALID-HANDLE(SELF) /* 07-DEC-2006 sla: Added for people that start ABHack from _adeevnt.p */
     AND SELF:TYPE = "Button"
     AND SELF:FLAT-BUTTON THEN DO:
        ghProtoolsButton = SELF.
        ghProtoolsButton:LOAD-IMAGE(REPLACE(ghProtoolsButton:IMAGE, ".bmp", "On.bmp")) NO-ERROR.
        gcProtoolsButtonTooltip = ghProtoolsButton:TOOLTIP.
        ghProtoolsButton:TOOLTIP = ghProtoolsButton:TOOLTIP + "~nABHack is running".
    END.

    {&WINDOW-NAME}:VISIBLE = YES.
    {&WINDOW-NAME}:MAX-HEIGHT-PIXELS = FRAME fMain:HEIGHT-PIXELS - 3.     /* this 3 is required, don't ask me why... */
    giWinHeightPixelsMax = {&WINDOW-NAME}:HEIGHT-PIXELS /*- 6 */ . /* -6  to fix little gap due to small title */
    giWinWidthPixelsMax  = {&WINDOW-NAME}:WIDTH-PIXELS.

    RUN findAppBuilder.
    APPLY 'VALUE-CHANGED' TO glUseSchemaCache IN FRAME fMisc.
    APPLY 'CHOOSE' TO btnExpand.  /* restore the ... */
    APPLY 'RIGHT-MOUSE-CLICK' TO btnExpand.  /* ... previous state */
    IF CAN-DO("<<,>>>", btnExpand:LABEL IN FRAME {&FRAME-NAME}) THEN APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
    RUN repositionAppBuilder. /* 29-OCT-2013 jcc: move AppBuilder so that it is not overlapped by ABHack */

    APPLY 'VALUE-CHANGED' TO glTopOnly.
    APPLY 'VALUE-CHANGED' TO giQueryTooltipTimeSuccess.
    APPLY 'VALUE-CHANGED' TO giQueryTooltipTimeFail.
    APPLY 'VALUE-CHANGED' TO glCompile IN FRAME fKeys.
    APPLY 'VALUE-CHANGED' TO glFilterGuessObjectType IN FRAME fComplColon.
    APPLY 'VALUE-CHANGED' TO gcFancyInABWin.
    APPLY 'VALUE-CHANGED' TO glStopStartTimerOnCtrlAltO.
    APPLY 'VALUE-CHANGED' TO glCopyUIBelowMouse.
    APPLY 'VALUE-CHANGED' TO glDrawBoxAroundCurrentLine.
    APPLY 'VALUE-CHANGED' TO giRightMarginIndicatorCol.
    APPLY 'CHOOSE' TO BtnCompletion. /* 09-AUG-2007 sla: changed form btnKeys to BtnCompletion */
    APPLY 'VALUE-CHANGED' TO gcSectionOutlineMode.
    APPLY 'VALUE-CHANGED' TO glAbhackFind.
    /* APPLY 'VALUE-CHANGED' TO gcDumpedResourceFileRoot.    04-SEP-2007 sla: don't do that now... otherwise it will reset the gcglobalCat to disable if the XML path is not valid   */
    FILE-INFO:FILE-NAME = "protools/abhack".
    gcABHackedPicRootPath = FILE-INFO:FULL-PATHNAME.
    gcABHackedPicRootPath = REPLACE(gcABHackedPicRootPath, "\", "/").
    gcABHackedPicRootPath = REPLACE(gcABHackedPicRootPath, "protools/abhack", "").


    RUN registerAllEditorWindows. /* 22-AUG-2007 sla: new feature */

    /* 24-OCT-2013 jcc: restore opened files */
    RUN restoreOpenedFiles.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initVlx C-Win
PROCEDURE initVlx :
/*------------------------------------------------------------------------------
  Purpose:     create the user.vlx & reload-lex.e files in SESSION:TEMP-DIRECTORY + "proedit"
  Parameters:  <none>
  Notes:       used to highlight the selected word. 14-MAR-2014 jcc
------------------------------------------------------------------------------*/
DEFINE VARIABLE cDlcDir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lCopy   AS LOGICAL     NO-UNDO.

IF SEARCH(SESSION:TEMP-DIRECTORY + "proedit/user.vlx") = ? THEN DO:
    cDlcDir = REPLACE(SEARCH("progress.cfg"), "\", "/").
    cDlcDir = SUBSTRING(cDlcDir, 1, R-INDEX(cDlcDir, "/") - 1).

    IF SEARCH(cDlcDir + "/proedit/vslick.vlx") = ? THEN RETURN.

    INPUT FROM VALUE(cDlcDir + "/proedit/vslick.vlx") BINARY NO-MAP NO-CONVERT.
    OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "proedit/user.vlx") BINARY NO-MAP NO-CONVERT.
    REPEAT:
        IMPORT UNFORMATTED cLine.
        IF cLine = "[Progress 4GL]" THEN lCopy = YES.
        ELSE IF SUBSTRING(cLine, 1, 1) = "[" THEN lCopy = NO.
        IF lCopy THEN
            PUT UNFORMATTED cLine SKIP.
    END.
    OUTPUT CLOSE.
    INPUT CLOSE.
END.

IF SEARCH(SESSION:TEMP-DIRECTORY + "proedit/reload-lex.e") = ? THEN
    OS-COPY VALUE(SEARCH("protools/abhack/reload-lex.e")) VALUE(SESSION:TEMP-DIRECTORY + "proedit/reload-lex.e").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE insertAlias C-Win
PROCEDURE insertAlias :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER pcAlias  AS CHARACTER  NO-UNDO.

DEFINE BUFFER ttCustAlias FOR ttCustAlias.
DEFINE BUFFER ttFreeList  FOR ttFreeList.

DEFINE VARIABLE cDate           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cExp            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFilePattern    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hEditorListProc AS HANDLE      NO-UNDO.
DEFINE VARIABLE iBackward       AS INTEGER     NO-UNDO.

/* use the MATCHES operator the other way round so we can define aliases with '*' */
FIND FIRST ttCustAlias WHERE pcAlias MATCHES ttCustAlias.cAlias NO-ERROR.
IF AVAILABLE ttCustAlias THEN DO:
    cmon:INSERT-STRING(STRING(TIME, "hh:mm:ss") + " alias found: " + ttCustAlias.cAlias) IN FRAME fMain.
    phEditor:SOURCE-COMMAND('delete-prev-word', '').
    cDate = REPLACE(STRING(04/29/73, "99/99/9999"), "/", "-"). /* 1st Feb 2003 string in date-format of the SESSION (not only american) */
    cDate = REPLACE(cDate, "04", ENTRY(MONTH(TODAY), "JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC")).
    cDate = REPLACE(cDate, "29", STRING(DAY(TODAY), "99")).
    cDate = REPLACE(cDate, "1973", STRING(YEAR(TODAY), "9999")).

    cExp = REPLACE(ttCustAlias.cExp, '~~n', CHR(13) + CHR(10)).
    cExp = REPLACE(cExp, "%\d", cDate).  /* at last this one is not hardcoded with american format */
    iBackward = R-INDEX(cExp, "%\c").
    cExp = REPLACE(cExp, "%\c", "").

    IF INDEX(ttCustAlias.cExp, "%\fileName") > 0 THEN ASSIGN
     cFilePattern = "*.*" /* 21-APR-2007 sla: perhaps we could do something smarter to retrieve a pattern after the %\fileName directive
                             Any idea Jan ? */
     iBackward    = R-INDEX(cExp, "%\fileName")
     cExp         = REPLACE(cExp, "%\fileName", "").

    phEditor:INSERT-STRING(cExp).
    IF iBackWard > 0 THEN DO iBackWard = LENGTH(cExp) - iBackWard TO 0 BY -1:
        phEditor:SOURCE-COMMAND('cursor-left', '').
    END.

    IF cFilePattern > "" THEN DO:
        RUN prepareFileList IN TARGET-PROCEDURE
         (cFilePattern, "2OSFile", INPUT-OUTPUT TABLE ttFreeList).

        cFilePattern = REPLACE(cFilePattern, "*.", ".").

        FIND FIRST ttFreeList NO-ERROR.
        IF AVAILABLE ttFreeList THEN DO:
            THIS-PROCEDURE:ADM-DATA = (IF THIS-PROCEDURE:ADM-DATA = ? THEN "" ELSE THIS-PROCEDURE:ADM-DATA)
                                       + ",EditorListShouldNotSpyEditor".
            RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).
            THIS-PROCEDURE:ADM-DATA = REPLACE(THIS-PROCEDURE:ADM-DATA, ",EditorListShouldNotSpyEditor", "").

            IF VALID-HANDLE(hEditorListProc)  /* invalid if it decided to kill itself */
             THEN RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty,ProcedureMode,LeaveCurlyBracket").
        END.

    END.

    RETURN "noApply".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE insertClosingGuy C-Win
PROCEDURE insertClosingGuy :
/*------------------------------------------------------------------------------
  Purpose:     insert closing ) or ] or ' or "
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor     AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcGuyToClose AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cBreakChar                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cClosingGuy                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLineEndCommentStrippedOut AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNextChar                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrevChar                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSelectedTextToWrap        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCursorChar                AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCursorCharMax             AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLineEndBeforeComments     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNextChar                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iOpeningParentheses        AS INTEGER     NO-UNDO.

PUBLISH "KillEditorList" FROM THIS-PROCEDURE (phEditor). /* 11-APR-2007 sla: this is usually what we want... */


IF phEditor:TEXT-SELECTED THEN DO:
    DEFINE VARIABLE iRestoreColWrap AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iRestoreRowWrap AS INTEGER     NO-UNDO.

    cSelectedTextToWrap = phEditor:SELECTION-TEXT NO-ERROR. /* 19-NOV-2007 sla: ignore > 32k limited */
    phEditor:EDIT-CLEAR().
    ASSIGN
     iRestoreColWrap = phEditor:CURSOR-CHAR
     iRestoreRowWrap = phEditor:CURSOR-LINE.
END.

iCursorChar = phEditor:CURSOR-CHAR.
RUN getEditorLine IN TARGET-PROCEDURE (phEditor, OUTPUT cLine).

/* 27-JAN-2007 sla: new feature to disable popup when the carret is in a quoted string */
IF glNoCompletionInStrings AND carretInQuotedString(cLine, iCursorChar) THEN RETURN.

cPrevChar = SUBSTRING(cLine, iCursorChar - 1, 1) NO-ERROR.

cMon:SCREEN-VALUE IN FRAME fMain = STRING(TIME, "hh:mm:ss") + " Insert closing guy for " + pcGuyToClose
 + "?  (prevChar= '" + cPrevChar + "')".

CASE pcGuyToClose:
    WHEN '(' THEN cClosingGuy = ")".
    WHEN '[' THEN cClosingGuy = "]".
    WHEN '~{' THEN cClosingGuy = "~}".
    /* do not annoy us when we want to type '"' or "'"  */
    WHEN '"' THEN IF cPrevChar <> "'" THEN cClosingGuy = '"'.
    WHEN "'" THEN IF cPrevChar <> '"' THEN cClosingGuy = "'".
END CASE.

IF   glInsertClosingGuys
 AND cClosingGuy > ""
 AND chCtrlSpy:PSTimerSpy:ENABLED  /* 01-FEB-2008 sla: improvment so people can disable A4GBL completion in javascripts */
 THEN DO:
    /* search appropriate place to insert the closing guy */
    ASSIGN
     cBreakChar     = " ,)" + cClosingGuy
     iCursorCharMax = LENGTH(RIGHT-TRIM(cLine))
     iLineEndBeforeComments = R-INDEX(cline, "/~*").
    IF iLineEndBeforeComments > iCursorChar THEN ASSIGN
     cLineEndCommentStrippedOut = removeLineComments(cLine, iLineEndBeforeComments)
     iLineEndBeforeComments = LENGTH(RIGHT-TRIM(cLineEndCommentStrippedOut)). /* remove possible spaces between full stop and comments at end of line */
    ELSE iLineEndBeforeComments = iCursorCharMax.

    DO iNextChar = iCursorChar TO iCursorCharMax + 1:
        cNextChar = SUBSTRING(cLine, iNextChar, 1).
        phEditor:CURSOR-CHAR = iNextChar.

        /* 10-MAR-2007 sla: improvement to not include a line full stop between the two guys */
        IF iNextChar = iLineEndBeforeComments AND (cNextChar = "." OR cNextChar = ":")  THEN LEAVE. /* 18-MAR-2007 sla: added ':', well spotted Paul  */

        /* 18-JAN-2007 sla: Enhanced version to insert ')' */
        IF cClosingGuy = ")" AND glEnhancedCloseParenthese THEN DO:
            IF cNextChar = "(" THEN iOpeningParentheses = iOpeningParentheses + 1.
            IF cNextChar = ")" THEN iOpeningParentheses = iOpeningParentheses - 1.

            IF iOpeningParentheses = 0
             AND INDEX(cBreakChar, cNextChar) > 0
             THEN LEAVE. /* we are not close to a word, insert the closing guy right here */
            NEXT.
        END.

        /* the following chars should stop the searching process  */
        IF INDEX(cBreakChar, cNextChar) > 0 THEN LEAVE. /* we are not close to a word, insert the closing guy right here */
    END.
    IF cSelectedTextToWrap > "" THEN DO:
        phEditor:INSERT-STRING(cClosingGuy).
        phEditor:CURSOR-CHAR = iRestoreColWrap.
        phEditor:INSERT-STRING(cSelectedTextToWrap). /* 19-NOV-2007 sla: new feature */
        ASSIGN
         phEditor:CURSOR-LINE = iRestoreRowWrap
         phEditor:CURSOR-CHAR = iRestoreColWrap.
    END.

    ELSE DO:
        phEditor:INSERT-STRING(cClosingGuy).
        phEditor:CURSOR-CHAR = iCursorChar. /* restore cursor position */
    END.
END.

/* Now call this guy from here */
IF pcGuyToClose = '(' THEN RUN leftParenthesePressed IN TARGET-PROCEDURE (phEditor).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE insertParams C-Win
PROCEDURE insertParams :
/*------------------------------------------------------------------------------
  Purpose:     called from procEditorList when running in procedureMode
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER phEditor     AS HANDLE      NO-UNDO.
DEFINE INPUT PARAMETER pcProcName   AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER pcParameters AS CHARACTER   NO-UNDO. /* 15-AUG-2007 sla: new option to receive the parameters from the caller.  Especially interesting when we deal with another class */

DEFINE VARIABLE cLine             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCursorChar       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCursorLine       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iINPos            AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLibHandleEndPos  AS INTEGER     NO-UNDO.
DEFINE VARIABLE lFunctionOrMethod AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lIsExtFunction    AS LOGICAL     NO-UNDO.

DEFINE BUFFER ttFunc   FOR ttFunc.
DEFINE BUFFER ttProc   FOR ttProc.
DEFINE BUFFER ttMethod FOR ttMethod.

ASSIGN
 lFunctionOrMethod = pcProcName MATCHES "*()"
 iCursorChar       = phEditor:CURSOR-CHAR
 iCursorLine       = phEditor:CURSOR-LINE.

RUN getEditorLine (phEditor, OUTPUT cLine).


IF pcParameters = "" THEN DO:
    IF INDEX(cLine, "IN TARGET-PROCEDURE", iCursorChar) > 0 THEN ASSIGN
     iCursorChar = INDEX(cLine, "IN TARGET-PROCEDURE", iCursorChar) + 19
     phEditor:CURSOR-CHAR = iCursorChar.

    IF lFunctionOrMethod THEN DO:
        FIND FIRST ttFunc WHERE ttFunc.hEditor = phEditor
                            AND ttFunc.cName   = REPLACE(pcProcName, "()", "") NO-ERROR.

        IF AVAILABLE ttFunc THEN DO:
            IF ttFunc.cParameters = "" THEN RETURN. /* nothing to insert */
            pcParameters = ttFunc.cParameters.
        END.
        /* no function, then try to find a method */
        IF NOT AVAILABLE ttFunc THEN DO:
            FIND FIRST ttMethod WHERE ttMethod.hEditor = phEditor
                                AND ttMethod.cName   = REPLACE(pcProcName, "()", "") NO-ERROR.
            IF AVAILABLE ttMethod THEN DO:
                IF ttMethod.cParameters = "" THEN RETURN. /* nothing to insert */
                pcParameters = ttMethod.cParameters.
            END.
            ELSE RETURN.  /* no FUNCTION and no method, nothing to do */
        END.


        IF NUM-ENTRIES(pcParameters) > giSplitParamInCalls THEN DO:
            CASE gcSplitParamCommaStyle:
                WHEN "leadingComma"  THEN pcParameters = REPLACE(pcParameters, ",", "~n" + FILL(" ", iCursorChar - 2) + ",").
                WHEN "trailingComma" THEN pcParameters = REPLACE(pcParameters, ",", ",~n" + FILL(" ", iCursorChar - 1)).
            END CASE.
        END.
        ELSE pcParameters = REPLACE(pcParameters, ",", ", ").
        IF NOT glIntProcInputInSig THEN pcParameters = REPLACE(pcParameters, "INPUT ", "").

        phEditor:INSERT-STRING(pcParameters).
        phEditor:CURSOR-LINE = iCursorLine. /* restore carret pos where it was so we can replace the param vars by the values to call with*/
        phEditor:CURSOR-CHAR = iCursorChar.
        RETURN.
    END.

    /* Procedure case */
    FIND FIRST ttProc WHERE ttProc.hEditor = phEditor
                        AND ttProc.cName   = pcProcName NO-ERROR.

    IF NOT AVAILABLE ttProc OR ttProc.cParameters = "" THEN RETURN. /* nothing to insert */

    pcParameters = ttProc.cParameters.
END. /* IF pcThisParam = "" THEN DO: */


IF lFunctionOrMethod THEN DO:
    IF   INDEX(cLine, "DYNAMIC-FUNCTION(") > 0  /* 15-SEP-2008 sla: this condition was missing */
     AND INDEX(cLine, "DYNAMIC-FUNCTION(") < iCursorChar
     THEN DO:
        iCursorChar = INDEX(cLine, ')', iCursorChar).
        IF iCursorChar = 0 THEN iCursorChar = MAX(1, LENGTH(RIGHT-TRIM(cLine))).
        lIsExtFunction = YES.
    END.
    ELSE iCursorChar = iCursorChar - 2.
END.

IF cLine MATCHES "*RUN * IN *" THEN DO:
    iINPos = INDEX(cLine, " IN ").
    iLibHandleEndPos = iINPos + 4.
    IF iINPos > 1 THEN DO WHILE TRUE:
        iLibHandleEndPos = iLibHandleEndPos + 1.
        IF iLibHandleEndPos > LENGTH(cLine) THEN DO:
            iLibHandleEndPos = 0.
            LEAVE.
        END.
        IF SUBSTRING(cLine, iLibHandleEndPos, 1) = " " THEN NEXT.
        LEAVE.
    END.
    IF iLibHandleEndPos > 0 THEN DO WHILE TRUE:
        iLibHandleEndPos = iLibHandleEndPos + 1.
        IF iLibHandleEndPos > LENGTH(cLine) + 1 THEN DO:
            iLibHandleEndPos = 0.
            LEAVE.
        END.
        IF SUBSTRING(cLine, iLibHandleEndPos, 1) = " " THEN LEAVE.
    END.

    /* 04-SEP-2007 sla: remove dot after HandleVar */
    IF iLibHandleEndPos > 1
     AND SUBSTRING(cLine, iLibHandleEndPos - 1, 1) = "."
     THEN DO:
        phEditor:CURSOR-CHAR = iLibHandleEndPos.
        phEditor:SOURCE-COMMAND('p4gl-maybe-case-backspace', '').
        iLibHandleEndPos = iLibHandleEndPos - 1.
     END.
    IF iLibHandleEndPos > 0 THEN ASSIGN
     iCursorChar          = iLibHandleEndPos
     phEditor:CURSOR-CHAR = iLibHandleEndPos.
END.

IF NUM-ENTRIES(pcParameters) > giSplitParamInCalls THEN DO:
    CASE gcSplitParamCommaStyle:
        WHEN "leadingComma"  THEN pcParameters = REPLACE(pcParameters, ",", "~n" + FILL(" ", iCursorChar) + ",").
        WHEN "trailingComma" THEN pcParameters = REPLACE(pcParameters, ",", ",~n" + FILL(" ", iCursorChar + 1)).
    END CASE.
END.
ELSE pcParameters = REPLACE(pcParameters, ",", ", ").

IF NOT glIntProcInputInSig THEN pcParameters = REPLACE(pcParameters, "INPUT ", "").

/* 04-JUN-2008 sla: support of external functions */
IF lIsExtFunction THEN DO:
    phEditor:CURSOR-CHAR = iCursorChar.
    phEditor:INSERT-STRING(" ,").
END.

/* 15-AUG-2007 sla: we get here when inserting a method for another class (not this-object) */
IF lFunctionOrMethod THEN phEditor:INSERT-STRING(pcParameters).
ELSE phEditor:INSERT-STRING(" (" + pcParameters + ").").
phEditor:CURSOR-LINE = iCursorLine. /* restore carret pos where it was so we can replace the param vars by the values to call with */
phEditor:CURSOR-CHAR = iCursorChar + 2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JumpMruSection C-Win
PROCEDURE JumpMruSection :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcUpDown AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cCurrentEvent   AS CHARACTER         NO-UNDO.
DEFINE VARIABLE cCurrentSection AS CHARACTER         NO-UNDO.
DEFINE VARIABLE cCurrentWidget  AS CHARACTER         NO-UNDO.
DEFINE VARIABLE iMruSequence    LIKE ttsection.iMruSequence NO-UNDO.

DEFINE BUFFER ttedt     FOR ttedt.
DEFINE BUFFER ttsection FOR ttsection.
DEFINE BUFFER ttMark    FOR ttMark.

IF NOT glJumpMruSection THEN RETURN.

FIND FIRST ttedt WHERE ttedt.hEditor = phEditor NO-ERROR.
IF NOT AVAILABLE ttedt THEN RETURN.

/* 05-JUL-2007 sla: improvement when running in non structured mode */
IF ttedt.lManageableSections = NO THEN DO:
    FIND LAST ttMark WHERE ttMark.hEditor = ttedt.hEditor AND ttMark.iLine <= ttEdt.iCursorLine NO-ERROR.

    IF NOT AVAILABLE ttMark THEN RETURN.

    CASE pcUpDown:
        WHEN "down" THEN FIND NEXT ttMark WHERE
                 ttMark.hEditor = ttEdt.hEditor
             AND ttMark.cBlockType <> "main"
             USE-INDEX editorLine NO-ERROR.
        WHEN "up" THEN FIND PREV ttMark WHERE
                 ttMark.hEditor = ttEdt.hEditor
             AND ttMark.cBlockType <> "main"
             USE-INDEX editorLine NO-ERROR.
    END CASE.
    IF AVAILABLE ttMark THEN DO:
        RUN JumpToSectionMark (phEditor, STRING(ROWID(ttMark))).
        /* weird, this makes the focus go to the AppBuilder Window */
        APPLY "ENTRY" TO ttedt.hEditor.
    END.
    RETURN.
END.

ASSIGN
 cCurrentSection = ttedt.hSectionCombo:SCREEN-VALUE
 cCurrentEvent   = ttEdt.hEventCombo:SCREEN-VALUE
 cCurrentWidget  = ttEdt.hWidgetName:SCREEN-VALUE.

DEFINE VARIABLE iFillHoles AS INTEGER     NO-UNDO.
FOR EACH ttsection WHERE ttsection.hEditor  = ttEdt.hEditor BY ttsection.iMruSequence:
    ASSIGN
     iFillHoles = iFillHoles + 1
     ttsection.iMruSequence = iFillHoles.
END.

FIND ttsection WHERE
     ttsection.hEditor  = ttEdt.hEditor
 AND ttsection.cSection = cCurrentSection
 AND (   CAN-DO("Definitions,Main Block", cCurrentSection)
      OR ttsection.cEvent = cCurrentEvent)
 AND (   cCurrentSection <> "Triggers"
      OR ttsection.cWidget = cCurrentWidget) NO-ERROR.
IF NOT AVAILABLE ttsection THEN DO:
    cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME, "hh:mm:ss")
     + " currently in an unrecorded section... this feature works only if the main spying timer is working.  Jump to MRU section Action canceled".
    RETURN.
END.
cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME, "hh:mm:ss")
 + " Jump to MRU section Action " + pcUpDown + " Current MruSequence is " + STRING(ttsection.iMruSequence).

CASE pcUpDown:
    WHEN "up" THEN iMruSequence = ttsection.iMruSequence - 1.
    WHEN "down" THEN iMruSequence = ttsection.iMruSequence + 1.
END CASE.

FIND LAST ttsection WHERE
     ttsection.hEditor = phEditor
 AND ttsection.iMruSequence = iMruSequence NO-ERROR.


IF NOT AVAILABLE ttsection THEN DO:
    cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME, "hh:mm:ss")
     + " You seem to have reached the " + (IF pcUpDown = "up" THEN "top" ELSE "bottom")
     + " of the Section MRU Stack.  Jump to MRU section Action canceled"
     + "  (iMruSequence=" + STRING(iMruSequence) + ")".
    RETURN.
END.

/* go back to the guy */
RUN JumpToSection (BUFFER ttedt, ttsection.cSection, ttsection.cWidget, ttsection.cEvent).
IF RETURN-VALUE = "BadSection" THEN DELETE ttsection.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JumpToSection C-Win
PROCEDURE JumpToSection :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ttEdt     FOR ttEdt.
DEFINE INPUT  PARAMETER pcSection AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pcWidget  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pcEvent   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cCurrentEvent    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCurrentSection  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCurrentWidget   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTheWidgetItem   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWidgetFirstWord AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWidgetList      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iWidget          AS INTEGER     NO-UNDO.

IF pcSection = "definition" THEN pcSection = "Definitions".
IF pcSection = "trigger"    THEN pcSection = "triggers".
IF pcSection = "procedure"  THEN pcSection = "Procedures".
IF pcSection = "function"   THEN pcSection = "Functions".
IF pcSection = "main"       THEN pcSection = "Main Block".
IF pcSection = "MainBlock"  THEN pcSection = "Main Block".

ASSIGN
 cCurrentSection = ttedt.hSectionCombo:SCREEN-VALUE
 cCurrentEvent   = ttEdt.hEventCombo:SCREEN-VALUE
 cCurrentWidget  = ttEdt.hWidgetName:SCREEN-VALUE.

IF ttedt.hSectionCombo:SCREEN-VALUE <> pcSection THEN DO:
    ttedt.hSectionCombo:SCREEN-VALUE = pcSection NO-ERROR.
    IF ttedt.hSectionCombo:SCREEN-VALUE <> pcSection THEN DO:
        ttedt.hSectionCombo:SCREEN-VALUE = cCurrentSection.
        ttedt.hSectionCombo:MODIFIED = YES.
        APPLY 'VALUE-CHANGED' TO ttedt.hSectionCombo.
        RETURN "BadSection: " + QUOTER(pcSection) + "  pcEvent: " + QUOTER(pcEvent) + " pcWidget: " + QUOTER(pcWidget).
    END.
    ttedt.hSectionCombo:MODIFIED = YES.
    APPLY 'VALUE-CHANGED' TO ttedt.hSectionCombo.
END.

IF   pcSection = "Triggers"
 AND pcWidget > ""
 AND ttEdt.hWidgetName:SCREEN-VALUE <> pcWidget THEN DO:
    /* 02-JUN-2007 sla: improvement, most of the widget appear with their label/title in the combo-box
     So we should go through the list to search for the corect item  */
    DO iWidget = ttEdt.hWidgetName:NUM-ITEMS TO 1 BY -1:
        cWidgetFirstWord = ENTRY(1, ttEdt.hWidgetName:ENTRY(iWidget), " ").
        /* 26-OCT-2007 sla: remove database name for Field UI events  */
        IF NUM-ENTRIES(cWidgetFirstWord, ".") > 2 THEN cWidgetFirstWord = SUBSTRING(cWidgetFirstWord, INDEX(cWidgetFirstWord, ".") + 1).
        IF cWidgetFirstWord <> pcWidget THEN NEXT.
        cTheWidgetItem = ttEdt.hWidgetName:ENTRY(iWidget).
        LEAVE.
    END.

    IF cTheWidgetItem > "" THEN ttEdt.hWidgetName:SCREEN-VALUE = cTheWidgetItem NO-ERROR.
    IF ttedt.hWidgetName:SCREEN-VALUE <> cTheWidgetItem THEN DO:
        ttEdt.hWidgetName:SCREEN-VALUE = cCurrentWidget NO-ERROR. /* roll back */
        ttedt.hSectionCombo:SCREEN-VALUE = cCurrentSection.
        ttedt.hSectionCombo:MODIFIED = YES.
        APPLY 'VALUE-CHANGED' TO ttedt.hSectionCombo.
        RETURN "BadSection: " + QUOTER(pcSection) + "  pcEvent: " + QUOTER(pcEvent) + " pcWidget: " + QUOTER(pcWidget).
    END.

    ttedt.hWidgetName:MODIFIED = YES.
    APPLY 'VALUE-CHANGED' TO ttedt.hWidgetName.
END.

IF NOT CAN-DO("Definitions,Main block", pcSection)
 AND ttEdt.hEventCombo:SCREEN-VALUE <> pcEvent THEN  DO:
    ttEdt.hEventCombo:SCREEN-VALUE = pcEvent NO-ERROR.
    IF ttedt.hEventCombo:SCREEN-VALUE <> pcEvent THEN DO:
        ttEdt.hEventCombo:SCREEN-VALUE = cCurrentEvent NO-ERROR. /* roll back */
        ttedt.hSectionCombo:SCREEN-VALUE = cCurrentSection.
        ttedt.hSectionCombo:MODIFIED = YES.
        APPLY 'VALUE-CHANGED' TO ttedt.hSectionCombo.
        IF ttEdt.hWidgetName:VISIBLE THEN DO:
            ttEdt.hWidgetName:SCREEN-VALUE   = cCurrentWidget.
            ttEdt.hWidgetName:MODIFIED = YES.
            APPLY 'VALUE-CHANGED' TO ttedt.hWidgetName.
        END.
        RETURN "BadSection: " + QUOTER(pcSection) + "  pcEvent: " + QUOTER(pcEvent) + " pcWidget: " + QUOTER(pcWidget).
    END.
    ttedt.hEventCombo:MODIFIED = YES.
    APPLY 'VALUE-CHANGED' TO ttedt.hEventCombo.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JumpToSectionMark C-Win
PROCEDURE JumpToSectionMark :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor                AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcSectionOutlineNodeKey AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cEvent          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSection        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWidget         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hFocussedWindow AS HANDLE      NO-UNDO.
DEFINE VARIABLE iOfWord         AS INTEGER     NO-UNDO.


DEFINE VARIABLE cString AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOldString AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttMark FOR ttMark.
DEFINE BUFFER ttEdt  FOR ttEdt.

&SCOPED-DEFINE innerTrim DO WHILE cOldString <> cString :   cOldString = cString.   cString = REPLACE(cOldString, "  ", " ").  END.

hFocussedWindow = FOCUS:WINDOW NO-ERROR.

FIND FIRST ttMark WHERE ROWID(ttMark) = TO-ROWID(pcSectionOutlineNodeKey) NO-ERROR.
IF NOT AVAILABLE ttMark THEN DO:
    cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
     "ERROR IN SectionOutlineJumpToMark: Got unvalid pcSectionOutlineNodeKey "
     + QUOTER(pcSectionOutlineNodeKey).
    RETURN.
END.

FIND ttEdt WHERE ttEdt.hEditor = phEditor NO-ERROR.
IF NOT AVAILABLE ttEdt THEN DO:
    MESSAGE "The attached editor is no longer there!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

IF ttEdt.lManageableSections THEN DO:
    cSection = ttMark.cBlockType.
    IF cSection = "TRIGGER" THEN DO:
        cString = ttMark.cBlockName.
        {&innerTrim}
        iOfWord = LOOKUP("OF", ttMark.cBlockName, " ").
        IF iOfWord > 0 THEN cWidget = ENTRY(iOfWord + 1, ttMark.cBlockName, " ") NO-ERROR.
        IF cWidget = "" THEN DO:
            cmon:SCREEN-VALUE = "SectionOutlineJumpToMark structured procedure, problem: can't locate widgetName in: " + QUOTER(ttMark.cBlockName).
            RETURN.
        END.
        cEvent = ENTRY(iOfWord - 1, ttMark.cBlockName, " ") NO-ERROR.
        IF cEvent = "" THEN DO:
            cmon:SCREEN-VALUE = "SectionOutlineJumpToMark structured procedure, problem: can't locate trigger event in: " + QUOTER(ttMark.cBlockName).
            RETURN.
        END.
    END.
    ELSE cEvent = ttMark.cBlockName.

    RUN JumpToSection (BUFFER ttedt, cSection, cWidget, cEvent).
    IF RETURN-VALUE > "" THEN cmon:SCREEN-VALUE = "JumpToSection fired for structured procedure returned error: " + QUOTER(RETURN-VALUE).
    RETURN.
END.

/* trick to make the currentline appear at the middle of the view-port */
ghCurrentEditor:CURSOR-LINE = ttMark.iLine + ghCurrentEditor:HEIGHT-CHARS / 1.3 NO-ERROR.
ghCurrentEditor:CURSOR-LINE = ttMark.iLine NO-ERROR.

IF VALID-HANDLE(hFocussedWindow) AND hFocussedWindow <> ghCurrentEditor
 THEN APPLY "ENTRY" TO hFocussedWindow.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE keepTrackEdt C-Win
PROCEDURE keepTrackEdt :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:


------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.
DEFINE PARAMETER BUFFER ttEdt FOR ttEdt.

DEFINE VARIABLE hWidget             AS HANDLE    NO-UNDO.
DEFINE VARIABLE iABHackWinRightEdge AS INTEGER   NO-UNDO.

/* 24-AUG-2007 sla: should never happen but it seems the new mecanism to register all editor can put a duplicate here... */
FIND FIRST ttEdt WHERE ttEdt.hEditor = phEditor NO-ERROR.
IF AVAILABLE ttEdt THEN DELETE ttEdt.

CREATE ttEdt.
ASSIGN
 ttEdt.hEditor   = phEditor
 ttEdt.hWin      = phEditor:WINDOW
 ttEdt.cWinTitle = ttEdt.hWin:TITLE.

/* 26-OCT-2007 sla: new feature to move windows away from the abhack window to the right or to the left */
IF glMoveNewWindowsAway THEN DO:
    iABHackWinRightEdge = {&WINDOW-NAME}:X + {&WINDOW-NAME}:WIDTH-PIXELS.
    /* abhack on left side */
    IF iABHackWinRightEdge < SESSION:WIDTH-PIXELS / 2 THEN DO:
        IF ttEdt.hWin:X < iABHackWinRightEdge
        THEN ttEdt.hWin:X = MIN(iABHackWinRightEdge + 7 , SESSION:WIDTH-PIXELS - ttEdt.hWin:WIDTH-PIXELS). /* don't shift it too far on the right */
    END.
    /* abhack on right side */
    ELSE DO:
        IF ttEdt.hWin:X + ttEdt.hWin:WIDTH-PIXELS > {&WINDOW-NAME}:X
         THEN ttEdt.hWin:X = MAX(1, {&WINDOW-NAME}:X - ttEdt.hWin:WIDTH-PIXELS - 7). /* don't shift it too far on the left */
    END.
END.


hWidget = phEditor:FRAME:FIRST-CHILD:FIRST-CHILD.
DO WHILE hWidget <> ?:
    IF hWidget:TYPE = "combo-box" AND hWidget:NAME = "se_event"
     THEN ASSIGN
      hWidget:INNER-LINES  = 46 /* 24-NOV-2006 sla: aaaahhhh ! */
      ttedt.hEventCombo    = hWidget /* keep track of it */
      hWidget:FONT = IF giNameComboFont = -1
                      THEN ?
                      ELSE giNameComboFont.

    IF hWidget:TYPE = "combo-box" AND hWidget:NAME = "isection" THEN ttedt.hSectionCombo = hWidget.

    IF hWidget:TYPE = "combo-box" AND hWidget:NAME = "wname" THEN ASSIGN
     hWidget:INNER-LINES = 46 /* 03-APR-2007 sla: How did I manage to miss this one ? */
     ttedt.hWidgetName   = hWidget.

    hWidget = hWidget:NEXT-SIBLING.
END.


ttedt.lManageableSections = VALID-HANDLE(ttedt.hSectionCombo) AND VALID-HANDLE(ttedt.hEventCombo).

RUN updateEdtFileName (BUFFER ttEdt).

RUN bedtBrowseSynch (phEditor).

/* 10-AUG-2007 sla: new feature to fast reload definition prepared in an xml file */
IF       gcGlobalResCat <> "disabled"
 AND NOT ttEdt.cWinTitle MATCHES "*Untitled:*"
 THEN DO:
    RUN loadGlobalFromCat IN ghParser (phEditor) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN cmon:SCREEN-VALUE IN FRAME fmain = "CatLoadErrors: " + QUOTER(RETURN-VALUE).
END.

IF giMaxEmptyLinesBeforeShrinking > 0
 AND ttEdt.lManageableSections = NO
 AND ttEdt.hEditor:PRIVATE-DATA <> ? /* 09-NOV-2007 sla: added this condition for some custom tools that use the vslick editor */
 THEN RUN shrinkWindow (BUFFER ttEdt).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE leftParenthesePressed C-Win
PROCEDURE leftParenthesePressed :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

DEFINE VARIABLE cFunctionName   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLine           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrevPrevWord   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrevWord       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCursorChar     AS INTEGER    NO-UNDO.

IF NOT gltooltipOnOpenParenthese THEN RETURN.

DEFINE BUFFER ttAPI FOR ttAPI.


iCursorChar = phEditor:CURSOR-CHAR.

RUN getEditorLine IN TARGET-PROCEDURE (phEditor, OUTPUT cLine).
RUN extractWordN IN TARGET-PROCEDURE (0, cLine, iCursorChar, "doNotPassCursor,(PartOfWords", OUTPUT cPrevWord) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.
/*
RUN extractWordN IN TARGET-PROCEDURE (-1, cLine, iCursorChar - 1, "", OUTPUT cPrevPrevWord) NO-ERROR.

cMon:SCREEN-VALUE IN FRAME fMain = "cPrevPrevWord: " + cPrevPrevWord + " cPrevWord:" + cPrevWord.
  */
IF cPrevWord MATCHES "*(" THEN cPrevWord = SUBSTRING(cPrevWord, 1, LENGTH(cPrevWord) - 1).
cMon:SCREEN-VALUE IN FRAME fMain = " cPrevWord:" + cPrevWord.

FIND FIRST ttAPI WHERE ttAPI.cAPI = cPrevWord NO-ERROR.
IF NOT AVAILABLE ttAPI THEN RETURN.

IF disabledApiTooltip(cPrevWord) THEN RETURN.

RUN showTooltip IN TARGET-PROCEDURE (phEditor, ttAPI.chelp, "canDisableApiTooltip=" + cPrevWord /*"ScrollHoriz"*/ ).

cMon:SCREEN-VALUE IN FRAME fMain = "API Tooltip:" + cFunctionName.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadDbTableCache C-Win
PROCEDURE loadDbTableCache :
/*------------------------------------------------------------------------------
  Purpose:     18-MAY-2007 sla: new local cache for database tables
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cDbName     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDbNameList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hbFile      AS HANDLE      NO-UNDO.
DEFINE VARIABLE hqFile      AS HANDLE      NO-UNDO.
DEFINE VARIABLE iDbNum      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iStartEtime AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTable      AS INTEGER     NO-UNDO.
DEFINE VARIABLE lMthRtn     AS LOGICAL     NO-UNDO.

DEFINE BUFFER ttDbTableCache FOR ttDbTableCache.

iStartEtime = ETIME.

EMPTY TEMP-TABLE ttDbTableCache.

DO iDbNum = 1 TO NUM-DBS:
    IF CAN-DO("PROGRESS,OpenEdge", DBTYPE(iDbNum))
     THEN cDbNameList = cDbNameList + "," + LDBNAME(iDbNum).
END.

cDbNameList = SUBSTRING(cDbNameList, 2). /* does not raise any error if empty*/

DO iDbNum = 1 TO NUM-ENTRIES(cDbNameList):
    cDbName = ENTRY(iDbNum, cDbNameList).

    CREATE BUFFER hbFile FOR TABLE cDbName + "._file" BUFFER-NAME "yo".
    CREATE QUERY hqFile.
    hqFile:SET-BUFFERS(hbFile).
    lMthRtn = hqFile:QUERY-PREPARE("FOR EACH yo NO-LOCK") NO-ERROR.
    IF lMthRtn THEN hqFile:QUERY-OPEN() NO-ERROR.
    IF lMthRtn THEN DO WHILE TRUE:
        hqFile:GET-NEXT().
        IF hqFile:QUERY-OFF-END THEN LEAVE.

        CREATE ttDbTableCache.
        ASSIGN
         iTable = iTable + 1
         ttDbTableCache.cDataBase = cDbName
         ttDbTableCache.cFLA      = hbFile:BUFFER-FIELD("_Dump-name"):BUFFER-VALUE
         ttDbTableCache.cTable    = hbFile:BUFFER-FIELD("_file-name"):BUFFER-VALUE.
    END.
    DELETE OBJECT hqFile.
    DELETE OBJECT hbFile.
END.

cmon:INSERT-STRING(STRING(TIME, "hh:mm:ss") + " : Table Schema Cache loaded in "
                   + STRING(ETIME - iStartEtime) + "  (" + STRING(iTable) + " tables)") IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadGlobalResources C-Win
PROCEDURE loadGlobalResources :
/*=====================================================================
 30-AUG-2007 sla: the logic has now been move to abhackparser.p to
 leverage the code of abahckWin.w
======================================================================*/

DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.


DEFINE VARIABLE cOrigPicFileName AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iGlobStartEtime  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE lCompileOK       AS LOGICAL     NO-UNDO.

DEFINE BUFFER ttEdt            FOR ttEdt.

IF NOT VALID-HANDLE(phEditor) THEN RETURN. /* securtiy, can occur if pressing the button before we got in any source editor */

FIND ttEdt WHERE ttEdt.hEditor = phEditor.

IF  VALID-HANDLE(ghABHackedText) THEN DO:
    IF ghABHackedText:TYPE = "TEXT"  THEN ghABHackedText:SCREEN-VALUE = "Loading...". /* to show loading process is starting */
    IF ghABHackedText:TYPE = "IMAGE" THEN DO:
        cOrigPicFileName = ghABHackedText:IMAGE.
        ghABHackedText:LOAD-IMAGE("protools/abhack/pics/babytux.bmp").
    END.
END.

ttEdt.iNumLines = phEditor:NUM-LINES. /* 21-SEP-2007 sla: very important to reset this guy to appropriate value, otherwise, the next loadLocal will believe that lines have been inserted, and will mess up the ttMark records */

iGlobStartEtime = DECIMAL(ETIME).

RUN loadGlobalResources IN ghParser (phEditor, NO, OUTPUT lCompileOK).
/* 21-SEP-2007 sla: solve an issue with mark records modified during the next loadLocal */
ttEdt.iNumLines   = phEditor:NUM-LINES.

IF VALID-HANDLE(ghABHackedText) THEN DO:
    IF ghABHackedText:TYPE = "TEXT"  THEN ghABHackedText:SCREEN-VALUE = " ABHacked". /* reset */
    IF ghABHackedText:TYPE = "IMAGE" THEN ghABHackedText:LOAD-IMAGE(cOrigPicFileName).
END.



/* 06-FEB-2007 sla: After loading the global, then load the local to at least refresh the used buffers
 The following will allow us to not loose the monitoring info of the global load */
DEFINE VARIABLE cMonGlobalLoadMsg AS CHARACTER   NO-UNDO.
cMonGlobalLoadMsg = cMon:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
/* prepare to display total time of GLOBAL load PROCESS in the first line  */
cMonGlobalLoadMsg = REPLACE(cMonGlobalLoadMsg, " GlobalLoadFired ", " GlobalLoad in " + STRING(ETIME - iGlobStartEtime) + " ms:").

cMon:CURSOR-LINE  = cMon:NUM-LINES.

RUN manageMonitorEdtColor IN TARGET-PROCEDURE (BUFFER ttEdt).

IF VALID-HANDLE(ghABHackedText) THEN DO:
    IF ghABHackedText:TYPE = "TEXT"  THEN ghABHackedText:SCREEN-VALUE = " ABHacked". /* reset */
    IF ghABHackedText:TYPE = "IMAGE" THEN ghABHackedText:LOAD-IMAGE(cOrigPicFileName).
END.


/* 15-JUN-2007 sla: do that now, so the next loadLocalResources will position the Section Outline treeview */
IF  ttEdt.lManageableSections = NO
 OR glSectionLayoutIgnoreStructProcs = NO
 THEN PUBLISH "ListSections" (phEditor).

RUN loadLocalResources (phEditor).

cmon:SCREEN-VALUE = cMonGlobalLoadMsg + "~n" + cmon:SCREEN-VALUE.

ttEdt.iMarkOffset = INTEGER(phEditor:LENGTH) NO-ERROR. /* keep track of that to update marks later when the lenghts changes */
IF ERROR-STATUS:ERROR THEN ttEdt.iMarkOffset = 0.

RUN bedtBrowseSynch (phEditor). /* display updated number of lines */

IF   lCompileOK = NO
 AND NOT ttEdt.cFullPathName MATCHES "*~~~.i" /* a .i might often fail to compile */
  THEN DO:
    DEFINE VARIABLE iFlash      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iMonBgColor AS INTEGER   NO-UNDO.
    iMonBgColor = cmon:BGCOLOR.

    DO iFlash = 1 TO 4:
        IF iFlash > 1 THEN RUN Sleep (100).
        cmon:BGCOLOR = 0.
        PROCESS EVENTS.
        RUN Sleep (100).
        cmon:BGCOLOR = iMonBgColor.
        PROCESS EVENTS.
    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadLocalResources C-Win
PROCEDURE loadLocalResources :
/*------------------------------------------------------------------------------
  Purpose:     Load local resources for a given editor handle.
            Local resources means variables buffer and other stuf scoped to
            a trigger or internal procedure or user defined function, in other
            words, code seen in an AppBuilder section editor

25-JAN-2007 sla: load of function params reviewed

21-MAY-2007 sla: major change: now I wor against an editor saved-file rather than
the screen-value of the editor.
------------------------------------------------------------------------------*/
/* 14-AUG-2007 sla: move the definition of preprocessor here for Version 9 !! */
&SCOPED-DEFINE UBPrevWordsExt 8

DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

DEFINE VARIABLE cBlockLabel         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBlockLabelLine     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBuffer             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFor                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFuncMethodCase     AS CHARACTER   INITIAL "notDoneYet" NO-UNDO.
DEFINE VARIABLE cImportFileName     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cName               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOldString          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrevVar            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUBPrevWords        AS CHARACTER   EXTENT {&UBPrevWordsExt} INITIAL ["EACH ","FIRST ","NEXT ","FIND ","LAST ","PREV ","BUFFER ", "CREATE "] NO-UNDO.
DEFINE VARIABLE cVar                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hUBValidBuffer      AS HANDLE      NO-UNDO.
DEFINE VARIABLE iAddedLines         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iColonPos           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCount              AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCursorLine         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iEditorLength       AS INTEGER     NO-UNDO. /* required to avoid errors when empty editor */.
DEFINE VARIABLE iJumpToOffset       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLine               AS INTEGER     NO-UNDO.
DEFINE VARIABLE iOffsetToAdd        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSectionFirstLine   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSectionLastLine    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iStartTime          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iStopAtLine         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iUBFound            AS INTEGER     NO-UNDO.
DEFINE VARIABLE iUBIndex            AS INTEGER     NO-UNDO.
DEFINE VARIABLE iUBPrevWord         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWordPos            AS INTEGER     NO-UNDO.
DEFINE VARIABLE lModifiedBeforeSave AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lUBValidBuffer      AS LOGICAL     NO-UNDO.

DEFINE BUFFER ttVar         FOR ttVar.
DEFINE BUFFER ttgVar        FOR ttgVar.
DEFINE BUFFER ttbuffer      FOR ttbuffer.
DEFINE BUFFER ttgbuffer     FOR ttgbuffer.
DEFINE BUFFER ttUsedBuffers FOR ttUsedBuffers.
DEFINE BUFFER ttEdt         FOR ttEdt.
DEFINE BUFFER ttMark        FOR ttMark.
DEFINE BUFFER ttBlockLabels FOR ttBlockLabels.
DEFINE BUFFER tttt          FOR tttt.
DEFINE BUFFER ttLibHandle   FOR ttLibHandle.
DEFINE BUFFER ttPreproc     FOR ttPreproc.

&SCOPED-DEFINE clearLocalResources   EMPTY TEMP-TABLE ttVar.   EMPTY TEMP-TABLE ttbuffer.   EMPTY TEMP-TABLE ttUsedBuffers.   EMPTY TEMP-TABLE ttBlocklabels.  EMPTY TEMP-TABLE ttLibHandle. ~
FOR EACH ttPreproc WHERE ttPreproc.hEditor = phEditor ~
 AND ttPreproc.iLineNum >= iSectionFirstLine ~
 AND ttPreproc.iLineNum <= iSectionLastLine:  DELETE ttPreproc. END.

FIND ttEdt WHERE ttEdt.hEditor = phEditor.

/* 26-OCT-2007 sla: better monitoring when a previous GLOBAL load failed for a class, as we will probably not be able to parse any local resource */
IF cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} BEGINS "CatLoadErrors:"
 AND ttEdt.isClass THEN DO:
    {&clearLocalResources}
    RETURN.
END.

IF cMon:SCREEN-VALUE BEGINS "CatLoad"
 THEN cmon:SCREEN-VALUE = ENTRY(1, cmon:SCREEN-VALUE, "~n") + "~n".
 ELSE cmon:SCREEN-VALUE = "". /* the point is to keep the time it took to load a glob resource xml file */
cMon:SCREEN-VALUE = cMon:SCREEN-VALUE + STRING(TIME, "hh:mm:ss") +  " Local load: ".


ASSIGN
 iStartTime  = DECIMAL(ETIME)
 iCursorLine = phEditor:CURSOR-LINE.

IF ttEdt.lManageableSections
AND ttEdt.hSectionCombo:SCREEN-VALUE = "Functions"
 THEN cFuncMethodCase = "ManageFunc#MethParams".

iEditorLength = INTEGER(phEditor:LENGTH) NO-ERROR.
IF ERROR-STATUS:ERROR THEN iEditorLength = 0.  /* happens when the editor is empty in 10.1B */

IF ttEdt.lManageableSections THEN DO:
    RUN StructuredFetchCurrentTtMark (BUFFER ttEdt, BUFFER ttMark).

    /* now needed to maintain the preprocessors */
    ASSIGN
     iSectionFirstLine = IF AVAILABLE ttMark THEN ttMark.iLine ELSE 0.
     iSectionLastLine = iSectionFirstLine + phEditor:NUM-LINES.

    PUBLISH "SectionSelectMark" (STRING(ROWID(ttMark))).
    IF ttEdt.iNumLines = phEditor:NUM-LINES THEN DO:
        cmon:SCREEN-VALUE = cmon:SCREEN-VALUE + "LocalLoad Structured mode: line " + STRING(iCursorLine) + " nothing to do until the number of line varies in the editor".
        RETURN.
    END.
    ttEdt.iNumLines = phEditor:NUM-LINES.
END.  ELSE DO:
    /* update marks if lines have been inserted */
    IF ttEdt.iNumLines <> phEditor:NUM-LINES THEN DO:
        iAddedLines = phEditor:NUM-LINES - ttEdt.iNumLines.
        cmon:SCREEN-VALUE = cmon:SCREEN-VALUE + STRING(iAddedLines) + (IF iAddedLines > 0 THEN " added" ELSE " removed")
         + " lines.  Updating marks above " + STRING(giUpdateMarksAboveLine) + " ".
        iOffsetToAdd = iEditorLength - ttEdt.iMarkOffset.
        FOR EACH ttMark WHERE
             ttMark.hEditor = phEditor
         AND ttMark.iLine >= ttEdt.iCursorLine
         BY ttMark.iLine DESCENDING: /* avoid infinite loop */
            ASSIGN
             ttMark.iLine = ttMark.iLine + iAddedLines
             ttMark.iOffset = ttMark.iOffset + iOffsetToAdd.
        END.
        ttEdt.iMarkOffset = iEditorLength NO-ERROR.
        IF ttEdt.iMarkOffset = 0 THEN ttEdt.iMarkOffset = 1. /* I don't like the idea of having a 0 there */
    END.

    FIND LAST ttMark WHERE ttMark.hEditor = phEditor AND ttMark.iLine <= iCursorLine USE-INDEX editorLine NO-ERROR.
    IF NOT AVAILABLE ttMark THEN DO:
        IF ttEdt.isClass THEN RETURN. /* 25-OCT-2007 sla: don't consider local resources of a class file in the ifrst header lines */
        IF phEditor:NUM-LINES < 2000 THEN DO:
            cmon:SCREEN-VALUE = cmon:SCREEN-VALUE + "  No mark before line " + STRING(iCursorLine)
                                + ".  Considering a simple procedure file without internal entries.  About to consider everything as local resources.  => Do a load global if you have internal entries and real global resources (temp-tables...) ".
            RUN loadGlobRecordMark IN ghParser (phEditor, "main", "main", 0, 0).
            FIND FIRST ttMark WHERE ttMark.hEditor = phEditor AND ttMark.iLine <= iCursorLine USE-INDEX editorLine.  /*should exist now*/
        END.  ELSE DO:
        cmon:SCREEN-VALUE = cmon:SCREEN-VALUE + "  No mark before line " + STRING(iCursorLine)
                            + ".  Probably in main block, or global defs have not been loaded yet.  => Abord".
        {&clearLocalResources}
        RETURN.
        END.
    END.

    PUBLISH "SectionSelectMark" (STRING(ROWID(ttMark))).

    ASSIGN
     iSectionFirstLine    = ttMark.iLine
     iJumpToOffset        = ttMark.iOffset.

    IF   ttEdt.currentSection = ttMark.cBlockType
     AND ttEdt.currentEvent   = ttMark.cBlockName
     AND ttMark.iEditorLength = iEditorLength
      THEN DO:
        cmon:SCREEN-VALUE = "LocalLoad: line " + STRING(iCursorLine) + " nothing to do (still in "
                           + SUBSTRING(ttEdt.currentSection,1, 4) + " [" + STRING(iSectionFirstLine) + ";" + STRING(iSectionLastLine) + "] "
                           + ttEdt.currentEvent + ")".
        RETURN.
    END.
    ASSIGN
     ttEdt.currentEvent   = ttMark.cBlockName
     ttEdt.currentSection = ttMark.cBlockType
     ttMark.iEditorLength = iEditorLength.

    IF CAN-DO("FUNCTION,METHOD", ttMark.cBlockType) THEN cFuncMethodCase = "ManageFunc#MethParams".
    /* IF CAN-DO("FUNCTION", ttMark.cBlockType) THEN cFuncMethodCase = "ManageFunc#MethParams". */

    iStopAtLine = 99999. /* default value to load up to last line if we are in the last section */
    FIND NEXT ttMark WHERE ttMark.hEditor = phEditor AND ttMark.iLine > iCursorLine USE-INDEX editorLine NO-ERROR.
    IF AVAILABLE ttMark THEN ASSIGN
     iSectionLastLine = ttMark.iLine
     iStopAtLine      = ttMark.iLine - iSectionFirstLine.
    cmon:SCREEN-VALUE = cmon:SCREEN-VALUE + " line " + STRING(iCursorLine) + " in "
                       + SUBSTRING(ttEdt.currentSection,1, 4) + " [" + STRING(iSectionFirstLine) + ";" + STRING(iSectionLastLine) + "] "
                       + ttEdt.currentEvent.
    FOR EACH ttPreproc WHERE ttPreproc.hEditor = phEditor
                         AND ttPreproc.iLineNum > iSectionFirstLine
                         BY ttPreproc.iLineNum DESCENDING: /* avoid infinite loop du to the update */
        IF AVAIL ttMark THEN DO:
            IF ttMark.iLine > ttPreproc.iLineNum + iAddedLines THEN DELETE ttPreproc.
            ELSE ttPreproc.iLineNum = ttPreproc.iLineNum + iAddedLines.
        END.
        ELSE DELETE ttPreproc.
    END.
END.

{&clearLocalResources}

/* IF TRIM(cTxt) MATCHES "RETURNS *END FUNCTION~~." */
/*  THEN cFuncMethodCase = "ManageFunc#MethParams".   */
cImportFileName = gcImportDirName + "localLoad.p".
lModifiedBeforeSave = phEditor:MODIFIED.
phEditor:SAVE-FILE(cImportFileName).
phEditor:MODIFIED = lModifiedBeforeSave.  /* it seems the save-file switches the modified attribute to NO... */

INPUT STREAM edtImport FROM VALUE(cImportFileName).
IF iJumpToOffset > 0 THEN SEEK STREAM edtImport TO iJumpToOffset.

lineLoop:
REPEAT:
    iLine = iLine + 1.
    IF iLine = iStopAtLine THEN LEAVE lineLoop.

    IMPORT STREAM edtImport UNFORMATTED cLine.

    IF cLine = "" THEN NEXT lineLoop.  /* speed it up by ignoring blank lines  */

    ASSIGN
     cLine   = LEFT-TRIM(cLine)
     cVar    = ""
     cBuffer = "".

    /* 28-NOV-2006 sla: support of function parameters */
    IF cFuncMethodCase = "ManageFunc#MethParams" THEN DO:
        /* 05-FEB-2007 sla: Better handling of the 'INPUT' or 'OUTPUT' keywords */

        IF INDEX(cLine, "(") > 0 THEN cLine = ENTRY(2, cLine, "(").
        cName = LEFT-TRIM(cLine, ",").

        IF cName BEGINS "FUNCTION " THEN NEXT lineLoop. /* 04-JUN-2007 sla: If the first line does not include any param def, then do not take the FUNCTION as a parameter */

        IF cName BEGINS "INPUT " OR cName BEGINS "INPUT-OUTPUT " OR cName BEGINS "OUTPUT "
         THEN ASSIGN
          ENTRY(1, cName, " ") = "" /* remove first word 'INPUT' */
          cName = LEFT-TRIM(cName). /* remove leading spaces */

        IF  cName BEGINS "BUFFER "
         OR cName BEGINS "DATASET "
         OR cName BEGINS "DATASET-HANDLE "
         OR cName BEGINS "TABLE "
         OR cName BEGINS "TABLE-HANDLE "
         THEN ASSIGN
          ENTRY(1, cName, " ") = "" /* remove thisword */
          cName = LEFT-TRIM(cName). /* remove leading spaces */

        cName = ENTRY(1, cName, " ") NO-ERROR. /* take right remaining first word */
        IF ERROR-STATUS:ERROR THEN NEXT lineLoop.

        cName = ENTRY(1, cName, ")").

        IF INDEX(cLine, " ") > 0 THEN DO:
            CREATE ttVar.
            ASSIGN
             ttVar.cVar      = cName
             ttVar.cDataType = "Don't know". /* default value */
             iCount          = iCount + 1.
             /* 05-JUL-2007 sla: starting to parse data type of those resources */
             iWordPos = LOOKUP("AS", cLine, " ").
            IF iWordPos > 0 THEN ttVar.cDataType = ENTRY(iWordPos + 1, cLine, " ") NO-ERROR.
            IF ttVar.cDataType = "CLASS" THEN ttVar.cDataType = ENTRY(iWordPos + 2, cLine, " ") NO-ERROR.
            ttVar.cDataType = ENTRY(1, ttVar.cDataType, ")").
        END.
        IF INDEX(cLine, ":") > 0 THEN cFuncMethodCase = "funcMethDefinitionPassed".
        NEXT lineLoop.
    END.

    /* should not happen if ':' on the same line as the last defined parameter& */
    IF cFuncMethodCase = "ManageFunc#MethParams"
     AND cLine MATCHES "*)*:*" THEN cFuncMethodCase = "funcMethDefinitionPassed".


    /* This Process will be able to detect used buffer after "FIND" (unique) "EACH" "FIRST"
    "NEXT" "PREV" "LAST" and "BUFFER" (parameter).  One limitation is that I will assume that each of this
    option can be used only once in a line */
    UsedBufferAttemp:
    DO iUBPrevWord = 1 TO {&UBPrevWordsExt}:
        iUBIndex = INDEX(cLine, cUBPrevWords[ iUBPrevWord ]).
        IF iUBIndex = 0 THEN NEXT UsedBufferAttemp.

        IF cLine MATCHES "*CAN-FIND(*" + cUBPrevWords[ iUBPrevWord ] + "*" THEN NEXT UsedBufferAttemp.

        cName = LEFT-TRIM(SUBSTRING(cLine, iUBIndex + LENGTH(cUBPrevWords[ iUBPrevWord ]) )).
        cName = SUBSTRING(cName, 1, INDEX(cName, " ") - 1).
        cName = TRIM(RIGHT-TRIM(RIGHT-TRIM(cName, "."), ":"), ",").
        IF cName = "" THEN NEXT UsedBufferAttemp. /* no buffer word */

        IF cUBPrevWords[ iUBPrevWord ] = "FIND" /* FIND unique ? */
         AND CAN-DO("FIRST,LAST,NEXT,PREV", cName) THEN NEXT UsedBufferAttemp. /* this FIND was not a unique find */
        /* is cName a valid buffer name ? */
        lUBValidBuffer = CAN-FIND(FIRST ttbuffer WHERE ttbuffer.cName = cName).
        IF NOT lUBValidBuffer THEN lUBValidBuffer = CAN-FIND(FIRST tttt NO-LOCK WHERE tttt.hEditor = phEditor AND tttt.cttname = cName).
        IF NOT lUBValidBuffer THEN lUBValidBuffer = CAN-FIND(FIRST ttgbuffer WHERE ttgbuffer.hEditor = phEditor AND ttgbuffer.cName = cName).
        IF NOT lUBValidBuffer THEN DO:
            CREATE BUFFER hUBValidBuffer FOR TABLE cName NO-ERROR.
            IF VALID-HANDLE(hUBValidBuffer) THEN DO:
                lUBValidBuffer = YES.
                DELETE OBJECT hUBValidBuffer.
            END.
        END.
       IF NOT lUBValidBuffer THEN NEXT UsedBufferAttemp.

        CREATE ttUsedBuffers.
        ASSIGN
         ttUsedBuffers.cName = cName
         iUBFound            = iUBFound + 1
         ttUsedBuffers.iLine = iLine. /* The higher the line number, the upper it should appear in a popup list */

        /* 19-OCT-2007 sla: trick to make a smarter ordering in buffer popups
         Indeed, a "buffer-usage" with just a DEFINE BUFFER or DEFINE PARAMETER BUFFER is not a real buffer usage.
         These type of definition should not be handled as a lifo in the popup, but as fifo */
        IF cLine MATCHES "def* buffer *" THEN ttUsedBuffers.iLine = - ttUsedBuffers.iLine.


        IF NOT ttEdt.lManageableSections AND iSectionFirstLine > 0 THEN ttUsedBuffers.iLine = ttUsedBuffers.iLine + iSectionFirstLine - 1.
    END. /* UsedBufferAttemp: */


    /* 03-MAR-2007 sla: support of block labels */
    IF INDEX(cLine + " ", ": ") > 0 THEN DO:
        /* ignore ': ' in comments */
        cBlockLabelLine = " " + removeLineComments(cLine + " " , 0).  /* add a space at front for the case of a label at the very begining of a line, this makes the detection of word easier */
        iColonPos = INDEX(cBlockLabelLine, ": ").
        IF iColonPos > 0 THEN DO:
            iWordPos = R-INDEX(cBlockLabelLine, " ", iColonPos - 1).
            IF iWordPos > 0 THEN DO:
                cBlockLabel = SUBSTRING(cBlockLabelLine, iWordPos + 1, iColonPos - iWordPos - 1).   /* NO-ERROR.   there should never be any error since I added a space at the beginning of the line */
                IF isValidBLockLabel(cBlockLabel, cBlockLabelLine) THEN DO:
                    CREATE ttBlocklabels.
                    ASSIGN
                     ttBlocklabels.cName = cBlockLabel
                     ttBlocklabels.iLine = iLine.
                END.
            END.
        END.
    END.

    /* 21-AUG-2007 sla: track ttLibHandle */
    IF cLine MATCHES "*RUN * PERSIST* SET *" THEN DO:
        /* I also need to Inner trim here */
        DO WHILE cOldString <> cLine :
            cOldString = cLine.
            cLine = REPLACE(cOldString, "  ", " ").
        END.
        ASSIGN
         iWordPos = LOOKUP("RUN", cLine, " ")
         cName    = ENTRY(iWordPos + 1, cLine, " ")
         iWordPos = LOOKUP("SET", cLine, " ")
         cVar     = RIGHT-TRIM(ENTRY(iWordPos + 1, cLine, " "), ".")
         NO-ERROR.
        IF ERROR-STATUS:ERROR THEN NEXT lineLoop.

        cVar = ENTRY(1, cVar, "(").

        IF   NOT CAN-FIND(FIRST ttVar WHERE ttVar.cVar = cVar)
         AND NOT CAN-FIND(FIRST ttgVar WHERE ttgVar.hEditor = phEditor AND ttgVar.cVar = cVar)
         THEN NEXT lineLoop.

        CREATE ttLibHandle.
        ASSIGN
         ttLibHandle.cLibFile = cName
         ttLibHandle.cVar     = cVar
         ttLibHandle.iLine    = iLine.
    END.

    /* 20-OCT-2008 sla: support Error Object reference defined in a catch block
     like: CATCH e AS progress.lang.ProError: */
    IF cLine MATCHES "CATCH * AS *"  THEN DO:
        /* Inner trim spaces only for definition lines to affect performance as less as possible */
        DO WHILE cOldString <> cLine :
            cOldString = cLine.
            cLine = REPLACE(cOldString, "  ", " ").
        END.

        DEFINE VARIABLE cErrorType AS CHARACTER   NO-UNDO.

        ASSIGN
         cLine      = RIGHT-TRIM(cLine, ".")
         cLine      = RIGHT-TRIM(cLine, ":")
         cVar       = ENTRY(2, cLine, " ")
         cErrorType = ENTRY(4, cLine, " ")
         NO-ERROR. /* an error can occur if the line was "CATCH e AS :"  (no type defined, ':' taken as 4th word) */

          /* no error can occur with the MATCH pattern used above */
        IF ERROR-STATUS:ERROR OR CAN-FIND(ttVar WHERE ttVar.cVar = cVar) THEN NEXT lineLoop.
        CREATE ttVar.
        ASSIGN
         ttVar.cVar      = cVar
         ttVar.cDataType = cErrorType.

        NEXT lineLoop.
    END.
    /* end 20-OCT-2008 sla: support Error Object reference defined in a catch block */


    /* at this point, we can speed up this way */
    IF NOT cLine BEGINS "def" THEN NEXT lineLoop. /* cLine was left-trimed so we can check that with the BEGINS operator */

    /* Inner trim spaces only for definition lines to affect performance as less as possible */
    DO WHILE cOldString <> cLine :
        cOldString = cLine.
        cLine = REPLACE(cOldString, "  ", " ").
    END.

    /* 27-DEC-2007 sla: starting to track definition of preprocessors */
    IF   glPreprocessors
     AND (   cLine BEGINS "&SCOPED"
          OR cLine BEGINS "&GLOBAL"
          OR cLine BEGINS "&UNDEFINE")
     AND NUM-ENTRIES(cLine, " ") > 2 /* added this condition to avoid error if the line just contains "&UNDEFINE" (yes it's bad, but it can happen) */
     THEN DO:
        CREATE ttPreproc.
        ASSIGN
         ttPreproc.hEditor   = ttEdt.hEditor
         ttPreproc.cFileName = ttEdt.cFileName
         ttPreproc.iLineNum  = iLine + iSectionFirstLine - 1
         ttPreproc.cName     = ENTRY(2, cLine, " ")
         ttPreproc.cValue    = SUBSTRING(cLine, INDEX(cLine, " ") + 1)
         ttPreproc.cValue    = SUBSTRING(ttPreproc.cValue, INDEX(ttPreproc.cValue, " ") + 1)
         ttPreproc.cType     = IF cLine BEGINS "&SCOPED" THEN "SCOPED"
                                  ELSE IF cLine BEGINS "&GLOBAL" THEN "GLOBAL"
                                  ELSE "UNDEFINE".
        NEXT lineLoop.
    END.

    IF cLine MATCHES "DEF* BUFFER*" THEN DO:
        cLine = REPLACE(cLine, "PARAMETER ", ""). /* 28-NOV-2006 sla: support of PARAMETER BUFFER */
        ASSIGN
         cBuffer = ENTRY(3, cLine, " ")
         cfor    = TRIM(ENTRY(5, cLine, " "), '.')
         NO-ERROR.
        IF ERROR-STATUS:ERROR THEN NEXT lineLoop.
    END.

    /* let's try to catch temp-tables names this way ;)  It's better than nothing */
    ELSE IF cLine MATCHES "DEF* PARAM*TABLE FOR*" THEN DO:
        ASSIGN
         cBuffer = ENTRY(5, cLine, " ")
         cfor    = ENTRY(5, cLine, " ")
         NO-ERROR.
        IF ERROR-STATUS:ERROR THEN NEXT lineLoop.
    END.

    IF cBuffer > ""
     /* avoid duplicates */
     AND NOT CAN-FIND(FIRST ttbuffer NO-LOCK WHERE ttbuffer.cName = cBuffer) THEN DO:
        CREATE ttbuffer.
        ASSIGN
         ttbuffer.cName = cBuffer
         ttbuffer.cfor  = cFor
         iCount         = iCount + 1.
        NEXT lineLoop.
    END.


    /* ERROR may occur if line is "DEFINE VARIABLE" without a third word ;)  */
    cVar = "".  /* reset for case of error */
    IF cLine MATCHES "DEF* VAR*"              THEN cVar = ENTRY(3, cLine, " ") NO-ERROR.
    ELSE IF cLine MATCHES "DEF* PARAM*"       THEN cVar = ENTRY(4, cLine, " ") NO-ERROR.
    ELSE IF cLine MATCHES "DEF* DATASET *"    THEN cVar = ENTRY(3, cLine, " ") NO-ERROR.
    ELSE IF cLine MATCHES "DEF* DATA-SOURCE*" THEN cVar = ENTRY(3, cLine, " ") NO-ERROR.

    IF cVar = "" THEN NEXT lineLoop.
    /* avoid duplicates */
    IF CAN-FIND(FIRST ttVar NO-LOCK WHERE ttVar.cVar = cVar) THEN NEXT lineLoop.
    CREATE ttVar.
    ASSIGN
     ttVar.cVar      = cVar
     /* 05-JUL-2007 sla: starting to parse data type of those resources */
     ttVar.cDataType = "ABHackDidNotCatch" /* default value */
     iCount          = iCount + 1
     iWordPos        = LOOKUP("AS", cLine, " ").  /* 15-AUG-2007 sla: will try to catch LIKE later  */
    IF iWordPos > 0 THEN ttVar.cDataType = ENTRY(iWordPos + 1, cLine, " ") NO-ERROR.
    CASE ttVar.cDataType:
        WHEN "CLASS" THEN ttVar.cDataType = ENTRY(iWordPos + 2, cLine, " ") NO-ERROR.
        /* aaabreviaaaaaation */
        WHEN "CHAR"  THEN ttVar.cDataType = "CHARACTER".
        WHEN "INT"   THEN ttVar.cDataType = "INTEGER".
        WHEN "DEC"   THEN ttVar.cDataType = "DECIMAL".
        WHEN "LOG"   THEN ttVar.cDataType = "LOGICAL".
        WHEN "DAT"   THEN ttVar.cDataType = "DATE".
    END CASE.

    /* 12-DEC-2007 sla: let's try to catch LIKE now */
    IF iWordPos = 0 THEN DO:
        iWordPos = LOOKUP("LIKE", cLine, " ").
        IF iWordPos > 0 THEN DO:
            DEFINE VARIABLE cLikeField AS CHARACTER   NO-UNDO.
            DEFINE VARIABLE cLikeTable AS CHARACTER   NO-UNDO.
            DEFINE VARIABLE hLikeTable AS HANDLE      NO-UNDO.
            DEFINE VARIABLE hLikeField AS HANDLE      NO-UNDO.
            DEFINE BUFFER likettVar FOR ttVar.
            cLikeField = ENTRY(iWordPos + 1, cLine, " ") NO-ERROR.

            IF ERROR-STATUS:ERROR = NO THEN DO:
                 cLikeField = RIGHT-TRIM(cLikeField, ".").
                 /* variable defined like another */
                 IF NUM-ENTRIES(cLikeField, ".") = 1 THEN DO:
                     FIND FIRST likettVar WHERE likettVar.cVar = cLikeField NO-ERROR.
                     IF AVAIL likettVar THEN ttVar.cDataType = likettVar.cDataType.
                 END.
                 ELSE IF NUM-ENTRIES(cLikeField, ".") > 1 THEN DO:
                     cLikeTable = SUBSTRING(cLikeField, 1, R-INDEX(cLikeField, ".") - 1).
                     cLikeField = SUBSTRING(cLikeField, R-INDEX(cLikeField, ".") + 1).
                     hLikeTable = ?.
                     hLikeTable = createTTorDBBuffer(phEditor, cLikeTable, "").
                     IF VALID-HANDLE(hLikeTable) THEN DO:
                         hLikeField = hLikeTable:BUFFER-FIELD(cLikeField) NO-ERROR.
                         IF VALID-HANDLE(hLikeField) THEN ttVar.cDataType = CAPS(hLikeField:DATA-TYPE).
                         IF   VALID-HANDLE(hLikeTable:TABLE-HANDLE)
                          AND hLikeTable:TABLE-HANDLE:TYPE = "temp-table" THEN DELETE OBJECT hLikeTable:TABLE-HANDLE NO-ERROR.
                         ELSE DELETE OBJECT hLikeTable NO-ERROR.
                     END.
                 END.
            END. /* ERROR-STATUS:ERROR = no */
        END. /* LOOKUP("LIKE", cLine, " ") > 0 */
    END. /* LOOKUP("AS", cLine, " ") = 0 */
END.


INPUT STREAM edtImport CLOSE.

cMon:SCREEN-VALUE = cMon:SCREEN-VALUE + "  " +  STRING(DECIMAL(ETIME) - iStartTime) + "ms to scan "
 + STRING(iLine) + " lines. " + STRING(iCount) + " var loaded. " + STRING(iUBFound) + " used buffer found".

&UNDEFINE UBPrevWordsExt
&UNDEFINE clearLocalResources

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadttAPITooltip C-Win
PROCEDURE loadttAPITooltip :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cLine      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iSeparator AS INTEGER    NO-UNDO.
DEFINE VARIABLE cFileName  AS CHARACTER  NO-UNDO.

DEFINE BUFFER ttAPI FOR ttAPI.

EMPTY TEMP-TABLE ttAPI.

cFileName = SEARCH("protools/abhack/customAPITooltip.txt").
IF cFileName = ? THEN DO:
    gltooltipOnOpenParenthese:CHECKED IN FRAME fMisc = NO.
    gltooltipOnOpenParenthese = NO.
    RETURN.
END.

INPUT FROM VALUE(cFileName).
REPEAT:
    IMPORT UNFORMATTED cLine.
    IF cLine BEGINS "# " THEN NEXT.
    iSeparator = INDEX(cLine, " ").
    IF iSeparator = 0 THEN NEXT.
    CREATE ttAPI.
    ASSIGN
     ttAPI.cAPI  = SUBSTR(cLine, 1, iSeparator - 1)
     ttAPI.cHelp = SUBSTR(cLine, iSeparator + 1).
END.
INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadttAttr C-Win
PROCEDURE loadttAttr :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cLine      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFileName  AS CHARACTER  NO-UNDO.

DEFINE BUFFER ttAttr FOR ttAttr.

EMPTY TEMP-TABLE ttAttr.
cFileName = SEARCH("protools/abhack/attrTabCompletion" + ENTRY(1, PROVERSION, ".") + ".txt").

IF cFileName = ? THEN DO:
    glCompleteAttrOnTab:CHECKED IN FRAME fComplColon = NO.
    glCompleteAttrOnTab = NO.
    RETURN.
END.
INPUT FROM VALUE(cFileName).
REPEAT:
    IMPORT UNFORMATTED cLine.
    IF cLine BEGINS "# " THEN NEXT.
    IF cLine = "" THEN NEXT.
    CREATE ttAttr.
    ASSIGN
     ttAttr.cAttr     = ENTRY(1, cLine, " ")
     ttAttr.cObjTypes = SUBSTRING(cLine, INDEX(cLine, " ") + 1).

    IF glLowerCaseCompletion THEN ttAttr.cAttr = LC(ttAttr.cAttr).
END.
INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadttCustAlias C-Win
PROCEDURE loadttCustAlias :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cFileName    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLargeTextID AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSeparator   AS INTEGER     NO-UNDO.
DEFINE VARIABLE rttcustAlias AS ROWID       NO-UNDO.

DEFINE BUFFER ttAbortCompl FOR ttAbortCompl.
DEFINE BUFFER ttcustalias  FOR ttcustalias.

EMPTY TEMP-TABLE ttCustAlias.
EMPTY TEMP-TABLE ttAbortCompl.

cFileName = SEARCH("protools/abhack/customTabCompletion.txt").
IF cFileName = ? THEN DO:
    glCustomCompletionOnTab:CHECKED IN FRAME fComplCustom = NO.
    glCustomCompletionOnTab = NO.
    RETURN.
END.

INPUT FROM VALUE(cFileName).
REPEAT:
    IMPORT UNFORMATTED cLine.
    IF cLine BEGINS "# " THEN NEXT.

    iCount = iCount + 1.

    IF cLargeTextID > "" THEN DO:
        IF cLine BEGINS "</LargeText " + cLargeTextID THEN cLargeTextID = "".
        ELSE DO:
            FIND ttcustalias WHERE ROWID(ttcustalias) = rttcustAlias.
            ttcustalias.cLargeText = ttcustalias.cLargeText + cLine + "~n".
        END.
        NEXT.
    END.

    IF cLine BEGINS "<LargeText " THEN ASSIGN
     cLargeTextID = TRIM(ENTRY(2, cLine, " "), ">")
     /* the point is to CREATE an alias with the largeTextID and a content of "<LargeText> <optional comment>" */
     ENTRY(1, cLine, " ") = cLargeTextID
     ENTRY(2, cLine, " ") = "<LargeText> " + cLargeTextID
     cline                = RIGHT-TRIM(cLine, ">").

    iSeparator = INDEX(cLine, " ").
    IF iSeparator = 0 THEN NEXT.
    CREATE ttCustAlias.
    ASSIGN
     ttCustAlias.cAlias = SUBSTR(cLine, 1, iSeparator - 1)
     ttCustAlias.cExp   = SUBSTR(cLine, iSeparator + 1)
     rttcustAlias       = ROWID(ttcustalias).
END.
INPUT CLOSE.

/* 24-MAR-2007 sla: improvement to give the ability to maintain a personal alias file
 One can also DISABLE the usage of an alias with the "@disabled@" keyword */
cFileName = SEARCH("protools/abhack/customTabCompletion.ofMine.txt").
IF cFileName = ? THEN RUN createCustomTabCompOfMineFile. /* 30-MAR-2007 sla: let's create file at run time if not existing (see comment in procedure) */

cFileName = SEARCH("protools/abhack/customTabCompletion.ofMine.txt").
IF cFileName <> ? THEN DO:
    INPUT FROM VALUE(cFileName).
    REPEAT:
        IMPORT UNFORMATTED cLine.
        IF cLine BEGINS "# " THEN NEXT.

        iCount = iCount + 1.

        IF cLargeTextID > "" THEN DO:
            IF cLine BEGINS "</LargeText " + cLargeTextID THEN cLargeTextID = "".
            ELSE DO:
                FIND ttcustalias WHERE ROWID(ttcustalias) = rttcustAlias.
                ttcustalias.cLargeText = ttcustalias.cLargeText + cLine + "~n".
            END.
            NEXT.
        END.

        IF cLine BEGINS "<LargeText " THEN ASSIGN
         cLargeTextID = TRIM(ENTRY(2, cLine, " "), ">")
         /* the point is to CREATE an alias with the largeTextID and a content of "<LargeText> <optional comment>" */
         ENTRY(1, cLine, " ") = cLargeTextID
         ENTRY(2, cLine, " ") = "<LargeText> " + cLargeTextID
         cline                = RIGHT-TRIM(cLine, ">").


        iSeparator = INDEX(cLine, " ").
        IF iSeparator = 0 THEN NEXT.

        FIND FIRST ttcustalias WHERE ttcustalias.cAlias = SUBSTR(cLine, 1, iSeparator - 1) NO-ERROR.
        IF NOT AVAILABLE ttcustalias THEN CREATE ttCustAlias.

        ASSIGN
         ttCustAlias.cAlias = SUBSTR(cLine, 1, iSeparator - 1)
         ttCustAlias.cExp   = SUBSTR(cLine, iSeparator + 1)
         rttcustAlias       = ROWID(ttcustalias).

        IF ttCustAlias.cExp = "@dislabledAlias@" THEN DELETE ttCustAlias.

        ELSE IF ttCustAlias.cExp = "@abortCompletionProcess@" THEN DO:
            CREATE ttAbortCompl.
            ttAbortCompl.cForThatWord = ttCustAlias.cAlias.
            DELETE ttCustAlias.
        END.
    END.
    INPUT CLOSE.
END.

cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Loaded " + STRING(iCount) + " aliases".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadttGuessObjType C-Win
PROCEDURE loadttGuessObjType :
/*------------------------------------------------------------------------------
  Purpose:   03-JAN-2007 sla:  load the attrGuessObjType<X>.txt file
------------------------------------------------------------------------------*/
DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cGuesses  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLine     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iGuess    AS INTEGER    NO-UNDO.

DEFINE BUFFER ttObjType      FOR ttObjType.
DEFINE BUFFER ttGuessObjType FOR ttGuessObjType.

EMPTY TEMP-TABLE ttObjType.
EMPTY TEMP-TABLE ttGuessObjType.

cFileName = SEARCH("protools/abhack/attrGuessObjType" + ENTRY(1, PROVERSION, ".") + ".txt").
IF cFileName = ? THEN DO:
    glFilterGuessObjectType:CHECKED IN FRAME fComplColon = NO.
    glFilterGuessObjectType = NO.
    RETURN.
END.

INPUT FROM VALUE(cFileName).
REPEAT:
    IMPORT UNFORMATTED cLine.
    IF cLine BEGINS "# " THEN NEXT.
    IF cLine = "" THEN NEXT.
    CREATE ttObjType.
    ttObjType.cObjType = ENTRY(1, cLine, " ").

    cGuesses = TRIM(SUBSTRING(cLine, INDEX(cLine, " ") + 1)).

    DO iGuess = 1 TO NUM-ENTRIES(cGuesses):
        CREATE ttGuessObjType.
        ASSIGN
         ttGuessObjType.cObjType     = ttObjType.cObjType
         ttGuessObjType.cGuess       = ENTRY(iGuess, cGuesses)
         ttGuessObjType.iGuessLength = LENGTH(ttGuessObjType.cGuess).
    END.
END.
INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE manageMonitorEdtColor C-Win
PROCEDURE manageMonitorEdtColor :
/*------------------------------------------------------------------------------
  Purpose:    Make the monitoring editor green if we find out that global variables
            have already been loaded for the current editor, and yellow otherwise.
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ttEdt FOR ttEdt.

DEFINE VARIABLE cMonTooltip                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lFoundSuspiciousGlobalBuffer AS LOGICAL     NO-UNDO.

DEFINE BUFFER ttgbuffer       FOR ttgbuffer.
DEFINE BUFFER ttReferedBuffer FOR ttReferedBuffer.

IF ttEdt.dLastLoadGlobalTime > 0.0 THEN ASSIGN
 cMon:BGCOLOR IN FRAME fMain = 10
 giABHackedTextColors[1]     = 2
 giABHackedTextColors[2]     = 2
 giABHackedTextColors[3]     = 2
 giABHackedTextColors[4]     = 2
 giABHackedTextColors[5]     = 2
 giABHackedTextColors[6]     = 2
 giABHackedTextColors[7]     = 2
 giABHackedTextColors[8]     = 10.
ELSE ASSIGN
 cMon:BGCOLOR IN FRAME fMain = 14
 giABHackedTextColors[1]     = 0
 giABHackedTextColors[2]     = 0
 giABHackedTextColors[3]     = 0
 giABHackedTextColors[4]     = 0
 giABHackedTextColors[5]     = 0
 giABHackedTextColors[6]     = 7
 giABHackedTextColors[7]     = 7
 giABHackedTextColors[8]     = 8.

FOR EACH ttReferedBuffer WHERE
     ttReferedBuffer.hEditor    = ttEdt.hEditor
 AND ttReferedBuffer.cBlockType = "mainBlock":
    FIND FIRST ttgbuffer WHERE ttgbuffer.hEditor = ttEdt.hEditor AND ttgbuffer.cName = ttReferedBuffer.cBufferName NO-ERROR.
    IF AVAILABLE ttgbuffer THEN NEXT.  /* this global buffer is defined explicitelly, so we can suppose the developper knows what he is doing */
    lFoundSuspiciousGlobalBuffer = YES.
    LEAVE.
END.

IF lFoundSuspiciousGlobalBuffer THEN ASSIGN
 cmon:FGCOLOR = 12
 cMonTooltip = "~nRed text => suspicious buffers with global scope are reported by a COMPILE LISTING."
  + "~n                    => See the Section Layout Treeview in the Main node"
  + "~nGood quality code is to avoid global buffers by redefining them localy in internal entries (DEFINE BUFFER customer FOR customer), or rely on PARAMETER BUFFER to 'share' them when needed."
  + "~nNote: at the time of writing (AUG-2008), COMPILE LISTING may report a global buffer for a simple CAN-FIND(myTable) or EMPTY TEMP-TABLE (which is wrong, see the release notes)".

ELSE cmon:FGCOLOR = ?. /* default color */

cmon:TOOLTIP = ENTRY(1, cmon:TOOLTIP, "~n") + cMonTooltip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openFile C-Win
PROCEDURE openFile :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcFileName AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttEdt FOR ttEdt.

/*  /* 09-JAN-2007 sla: This is all what I have found for now
 Opening a Window .w or structured .p is sooo complicated with the ADE
 wil try to find out later.... */
RUN adecomm/_pwmain.p ( "", pcFileName , "").*/

/* 22-AUG-2007 sla: new feature: if abhack is aware of an editor window already open for pcFileName
 then just apply entry to it */
FIND FIRST ttEdt WHERE ttEdt.cFileName = pcFileName AND ttEdt.lManageableSections = NO NO-ERROR.
IF AVAILABLE ttEdt
 AND VALID-HANDLE(ttEdt.hEditor)
 AND ttEdt.hEditor:TYPE = "editor"
 AND VALID-HANDLE(ttEdt.hWin)
 AND ttEdt.hWin:TYPE = "WINDOW"
 THEN DO:
    APPLY "ENTRY" TO ttEdt.hwin.
    RETURN.
END.

/* 10-JAN-2007 sla: found how to open structure .p and .w's' */
IF NOT VALID-HANDLE(ghMainAppBuilderWindow) THEN RUN findAppBuilder.

IF NOT VALID-HANDLE(ghMainAppBuilderWindow)
 THEN RUN protools/abhack/openFileInAB.p PERSISTENT (pcFileName, THIS-PROCEDURE:CURRENT-WINDOW).

ELSE RUN protools/abhack/openFileInAB.p PERSISTENT (pcFileName, ghMainAppBuilderWindow).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openInProcedureWindow C-Win
PROCEDURE openInProcedureWindow :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.
DEFINE VARIABLE cFileName         AS CHARACTER  NO-UNDO.

IF NOT glOpenPw THEN RETURN.

RUN getEditorFileName(phEditor, OUTPUT cFileName) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    cMon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cMon:SCREEN-VALUE + "~n can find this file on disk, aborting!".
    RETURN "invalid file".
END.

RUN adecomm/_pwmain.p ( "", cFileName , "").
cMon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cFileName + " opened in a Simple Procedure Editor Window".

RUN registerAllEditorWindows.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE plusPressed C-Win
PROCEDURE plusPressed :
/*------------------------------------------------------------------------------
  Purpose:     transform  a++ into a = a +
  Parameters:  <none>
  Notes:       There are a few problems with the Numeric Key Pad Plus Key with the
               source code editor, as the '+' gets inserted before we can trap
               it.  Hence the special handlings with the lNumericKeyPadPlus flag

                 => We shall get rid off al this as soon as we stop using
                 anywhere Triggers but use a spying time with getKeyboardState
                 instead.
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

DEFINE VARIABLE cLine                 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cMayBeNoApply         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrevPrevChar         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrevWord             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cToAdd                AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCursorChar           AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCharToDelete         AS INTEGER    NO-UNDO.
DEFINE VARIABLE iSrcEditorReturnValue AS INTEGER    NO-UNDO.
DEFINE VARIABLE lNumericKeyPadPlus    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mSrcEditorKBState     AS MEMPTR     NO-UNDO.


cMon:SCREEN-VALUE IN FRAME fMain = "pluspressed begins".

SET-SIZE(mSrcEditorKBState) = 256.
RUN GetKeyboardState(GET-POINTER-VALUE(mSrcEditorKBState), OUTPUT iSrcEditorReturnValue).
lNumericKeyPadPlus = GET-BYTE(mSrcEditorKBState, 108) > 127.
SET-SIZE(mSrcEditorKBState) = 0.

IF lNumericKeyPadPlus THEN cMayBeNoApply = "NoApply".

IF  glPlusplus = NO
 OR chCtrlSpy:PSTimerSpy:ENABLED = NO  /* 01-FEB-2008 sla: improvment so people can disable A4GBL completion in javascripts */
 THEN RETURN cMayBeNoApply.

iCursorChar = phEditor:CURSOR-CHAR.
/* IF lNumericKeyPadPlus THEN iCursorChar = iCursorChar - 1. */

RUN getEditorLine IN TARGET-PROCEDURE (phEditor, OUTPUT cLine).

/* 27-JAN-2007 sla: new feature to disable popup when the carret is in a quoted string */
IF glNoCompletionInStrings AND carretInQuotedString(cLine, iCursorChar) THEN RETURN cMayBeNoApply.

RUN extractWordN IN TARGET-PROCEDURE (0, cLine, iCursorChar, "allowEmptyWord,doNotPassCursor,commaPartOfWords,periodPartOfWords,(PartOfWords,)PartOfWords,:PartOfWords"
                                     ,OUTPUT cPrevWord) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

IF lNumericKeyPadPlus THEN cPrevWord = SUBSTRING(cPrevWord, 1, LENGTH(cPrevWord) - 1).

cMon:SCREEN-VALUE IN FRAME fMain = "cPrevWord: " + cPrevWord + "  iCursorChar:" + STRING(iCursorChar)
 + "  lNumericKeyPadPlus:" + STRING(lNumericKeyPadPlus).

IF ERROR-STATUS:ERROR OR cPrevWord = "" THEN RETURN cMayBeNoApply.


IF LENGTH(cPrevWord) <= 1 THEN RETURN cMayBeNoApply.


IF cPrevWord MATCHES "*+" THEN cToAdd = " + ".

IF cPrevWord MATCHES "*-" THEN ASSIGN
 cPrevWord = SUBSTRING(cPrevWord, 1, LENGTH(cPrevWord) - 1)
 cToAdd    = " - ".

IF cPrevWord MATCHES "*," THEN ASSIGN
 cPrevWord = SUBSTRING(cPrevWord, 1, LENGTH(cPrevWord) - 1)
 cToAdd = ' + "," + '.

IF cPrevWord MATCHES "*|" THEN ASSIGN
 cPrevWord = SUBSTRING(cPrevWord, 1, LENGTH(cPrevWord) - 1)
 cToAdd    = ' + "|" + '.

IF cPrevWord MATCHES "*;" THEN ASSIGN
 cPrevWord = SUBSTRING(cPrevWord, 1, LENGTH(cPrevWord) - 1)
 cToAdd    = ' + ";" + '.

IF cPrevWord MATCHES "*1" THEN ASSIGN
 cPrevWord = SUBSTRING(cPrevWord, 1, LENGTH(cPrevWord) - 1)
 cToAdd    = " + 1".

IF  cPrevWord MATCHES "*CHR(.)"
 OR cPrevWord MATCHES "*CHR(..)"
 OR cPrevWord MATCHES "*CHR(...)"
 THEN ASSIGN
  cToAdd    = " + " + CAPS(SUBSTRING(cPrevWord, R-INDEX(cPrevWord, "CHR("))) + " + "
  cPrevWord = SUBSTRING(cPrevWord, 1, R-INDEX(cPrevWord, "CHR(") - 1).

IF cToAdd = "" THEN RETURN cMayBeNoApply.

/* Remove the first '+' */
phEditor:SOURCE-COMMAND('p4gl-maybe-case-backspace', '').
IF lNumericKeyPadPlus THEN phEditor:SOURCE-COMMAND('p4gl-maybe-case-backspace', '').

/* Special case for ' + CHR(x) + ' */
IF cToAdd BEGINS " + CHR(" THEN DO iCharToDelete = 1 TO LENGTH(cToAdd) - 7 :
    phEditor:SOURCE-COMMAND('p4gl-maybe-case-backspace', '').
END.

phEditor:INSERT-STRING(' = ' + RIGHT-TRIM(cPrevWord, "+") + cToAdd).

RETURN "noApply".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareAliasList C-Win
PROCEDURE prepareAliasList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER pcAlias  AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttFreeList.

DEFINE BUFFER ttCustAlias FOR ttCustAlias.
DEFINE BUFFER ttFreeList  FOR ttFreeList.


EMPTY TEMP-TABLE ttFreeList.

/* use the MATCHES operator the other way round so we can define aliases with '*' */
/* FOR EACH ttCustAlias WHERE REPLACE(ttCustAlias.cAlias, "*", "") BEGINS pcAlias: */
/* 15-NOV-2007 sla: try something new more simple and complient with the new hypes' strikes */
FOR EACH ttCustAlias WHERE pcAlias MATCHES ttCustAlias.cAlias:
    CREATE ttFreeList.
    ttFreeList.cItem = refinedAlias(ttCustAlias.cExp).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareBlockList C-Win
PROCEDURE prepareBlockList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcBegins     AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER piCursorLine AS INTEGER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttFreeList.

DEFINE BUFFER ttBlocklabels FOR ttBlocklabels.
DEFINE BUFFER ttFreeList    FOR ttFreeList.

DEFINE VARIABLE iItemNum AS INTEGER     NO-UNDO.

/* use the MATCHES operator the other way round so we can define aliases with '*' */
FOR EACH ttBlocklabels WHERE
     ttBlocklabels.cName BEGINS pcBegins
 AND ttBlocklabels.cName > pcBegins /* do not put an item if already typed in front of the carret */
 AND ttBlocklabels.iLine < piCursorLine
 BY ttBlocklabels.iLine DESCENDING:
    CREATE ttFreeList.
    ASSIGN
     ttFreeList.cItem       = ttBlocklabels.cName
     iItemNum               = iItemNum + 1
     ttfreelist.cSortOption = STRING(iItemNum, ">>>>>>9").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareClassResource C-Win
PROCEDURE prepareClassResource :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor            AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcExtendedType      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER plStaticMembersOnly AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttFreeList.

DEFINE VARIABLE cClassFileName     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFullClassFileName AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttFreeList FOR ttFreeList.
DEFINE BUFFER ttUsing    FOR ttUsing.

EMPTY TEMP-TABLE ttFreeList.

/* 15-OCT-2008 sla: now support Progress.Lang.* objects */
IF pcExtendedType BEGINS "Progress.Lang." THEN cFullClassFileName = pcExtendedType.

/* resolve class name with cls file on disk */
ELSE DO:
    /* 08-SEP-2008 sla: attempt to support multi level call resource (objA:getObjB(): */
    cClassFileName = REPLACE(pcExtendedType, "\", "/").
    IF   NOT cClassFileName MATCHES "*~~~.cls"
     THEN cClassFileName = cClassFileName.
    FILE-INFO:FILE-NAME = cClassFileName + ".cls".
    /* 07-JAN-2014 jcc: make it work with simple files setup */
    IF FILE-INFO:FULL-PATHNAME = ? THEN
        FILE-INFO:FILE-NAME = REPLACE(cClassFileName, ".", "/") + ".cls".

    IF FILE-INFO:FULL-PATHNAME = ?
     THEN FOR EACH ttUsing WHERE ttUsing.hEditor = phEditor:
        FILE-INFO:FILE-NAME = REPLACE(ttUsing.cUsing, ".*", "") + "/" + cClassFileName + ".cls".
        IF FILE-INFO:FULL-PATHNAME = ? THEN NEXT.

        cFullClassFileName = FILE-INFO:FULL-PATHNAME.
        LEAVE.
    END.
    ELSE cFullClassFileName = FILE-INFO:FULL-PATHNAME.

    IF cFullClassFileName = "" THEN DO:
        cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME, "hh:mm:ss") + " could not locate class file for type " + QUOTER(pcExtendedType).
        RETURN.
    END.
END.

RUN loadPropMethFromCat IN ghParser
 (cFullClassFileName
 ,IF plStaticMembersOnly THEN "StaticMembersOnly" ELSE ""
 ,OUTPUT TABLE ttFreeList)
 NO-ERROR.
IF ERROR-STATUS:ERROR THEN cmon:SCREEN-VALUE = STRING(TIME, "hh:mm:ss") + " error returned by loadPropMethFromCat: " + QUOTER(RETURN-VALUE).
RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareExtFuncList C-Win
PROCEDURE prepareExtFuncList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor      AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcExternalLib AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttFreeList.

DEFINE VARIABLE cFullLibFileName AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttFreeList FOR ttFreeList.

EMPTY TEMP-TABLE ttFreeList.

FILE-INFO:FILE-NAME = pcExternalLib.
cFullLibFileName = REPLACE(FILE-INFO:FULL-PATHNAME, "\", "/").

IF cFullLibFileName = "" THEN DO:
    cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME, "hh:mm:ss") + " could not locate abhack xml file for library " + QUOTER(pcExternalLib).
    RETURN.
END.

RUN LoadLibFuncsFromCat IN ghParser (cFullLibFileName
                                    ,""
                                    ,OUTPUT TABLE ttFreeList)
                                    NO-ERROR.
IF ERROR-STATUS:ERROR THEN cmon:SCREEN-VALUE = STRING(TIME, "hh:mm:ss") + " error returned by LoadLibFuncsFromCat: " + QUOTER(RETURN-VALUE).
RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareExtProcList C-Win
PROCEDURE prepareExtProcList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor      AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcExternalLib AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttFreeList.

DEFINE VARIABLE cFullLibFileName AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttFreeList FOR ttFreeList.

EMPTY TEMP-TABLE ttFreeList.


FILE-INFO:FILE-NAME = pcExternalLib.
cFullLibFileName = REPLACE(FILE-INFO:FULL-PATHNAME, "\", "/").

IF cFullLibFileName = "" THEN DO:
    cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME, "hh:mm:ss") + " could not locate abhack xml file for library " + QUOTER(pcExternalLib).
    RETURN.
END.

RUN LoadLibProcsFromCat IN ghParser (cFullLibFileName
                                    ,""
                                    ,OUTPUT TABLE ttFreeList)
                                    NO-ERROR.
IF ERROR-STATUS:ERROR THEN cmon:SCREEN-VALUE = STRING(TIME, "hh:mm:ss") + " error returned by LoadLibProcsFromCat: " + QUOTER(RETURN-VALUE).
RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareFileList C-Win
PROCEDURE prepareFileList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER pcMatches AS CHARACTER  NO-UNDO.
DEFINE INPUT        PARAMETER pcSort    AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttFreeList.

DEFINE VARIABLE cAttrFile         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBeginPath        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDirBegins        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDirToIgnore      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDlcDir           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cExtProcIgnoreDir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFileBoundary     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFileName         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFullPath         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOSDir            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPath             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPropath          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE detimeLoadingMax  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iDirToIgnore      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iDirToIgnoreMax   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iFile             AS INTEGER     NO-UNDO.
DEFINE VARIABLE iFileMax          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPath             AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPathMax          AS INTEGER     NO-UNDO.

DEFINE BUFFER ttFreeList FOR ttFreeList.

/* to do implement yet another preference to tune these two guys */
detimeLoadingMax = ETIME + giMaxEtimeToLoadFiles.

IF INDEX(pcMatches, "/") > 0 THEN DO:
   ASSIGN
    cBeginPath = SUBSTRING(pcMatches, 1, R-INDEX(pcMatches, "/"))
    pcMatches  = SUBSTRING(pcMatches, R-INDEX(pcMatches, "/") + 1).
   IF INDEX(cBeginPath, ":") = 0 THEN cBeginPath = "/" + cBeginPath. /* do not prepend '/' for windoze full path like c:/ */
END.

ASSIGN
 cPropath      = "," + REPLACE( REPLACE(PROPATH, ";", ","), "\", "/") /* *NIX rules */
 iPathMax      = NUM-ENTRIES(cPropath)
 cDirBegins    = ENTRY(1, ENTRY(1, pcMatches, "."), "*")
 cFileBoundary = cDirBegins + "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ".

IF  pcMatches MATCHES "*~~~.p"
 /* OR pcMatches MATCHES "*.~~~.w" */ /* 18-SEP-2007 sla: removed suspecious '.' */
 OR pcMatches MATCHES "*~~~.w"
 /* OR pcMatches MATCHES "*.~~~.i" */ /* 18-SEP-2007 sla: removed suspecious '.' */
 OR pcMatches MATCHES "*~~~.i"
 THEN ASSIGN
  cDlcDir           = REPLACE(SEARCH("progress.cfg"), "\", "/")
  cDlcDir           = SUBSTRING(cDlcDir, 1, R-INDEX(cDlcDir, "/") - 1)
  cExtProcIgnoreDir = REPLACE(gcExtProcIgnoreDir, "%DLC%", cDlcDir)
  iDirToIgnoreMax   = NUM-ENTRIES(cExtProcIgnoreDir).

pcMatches = REPLACE(pcMatches, ".", "~~~.").


dirLoop:
DO iPath = 1 TO iPathMax:
    cPath = ENTRY(iPath, cPropath).

    /* It seems to be hard to support pl files... Anyway, developpers should
     have source files corresponding to those pl in their propath at dev time */
    IF cPath MATCHES "*~~~.pl" THEN NEXT dirLoop.

    DO iDirToIgnore = 1 TO iDirToIgnoreMax:
        cDirToIgnore = ENTRY(iDirToIgnore, cExtProcIgnoreDir).
        IF cPath MATCHES cDirToIgnore THEN NEXT dirLoop.
    END.

    cPath = cPath + cBeginPath.

    FILE-INFO:FILE-NAME = RIGHT-TRIM(cPath, "/").
    IF FILE-INFO:FULL-PATHNAME = ? THEN NEXT. /* unvalid entry in propath ? => bad but ignore it */

    /* 17-SEP-2007 sla: will soon try to implement a new feature: work against the DB to go faster than scanning network drives */
    IF glFindOSFilesWithAbhackDB AND gcGlobalResCat = "DB" THEN DO:
        /* This procedure adds records to the shared ttFreeList temp-table */
        RUN addSourceFilesFromCatDb IN ghParser (RIGHT-TRIM(FILE-INFO:FULL-PATHNAME)
                                                ,pcMatches
                                                ,pcSort
                                                ,INPUT-OUTPUT iFile
                                                ,giMaxFilesToLoad
                                                ,detimeLoadingMax).
        NEXT.
    END.

    INPUT FROM OS-DIR(RIGHT-TRIM(cPath, "/")).
    fileLoop:
    REPEAT:
        IF iFile > giMaxFilesToLoad THEN LEAVE.
        IF DECIMAL(ETIME) > detimeLoadingMax THEN LEAVE. /* don't spend more than this time in this process' */

        IMPORT cFileName cFullPath cAttrFile.
        cFullPath = REPLACE(cFullPath, "\", "/").

        IF cFileName > cFileBoundary THEN LEAVE. /* 05-APR-2007 sla: perf improvement no need to bother with the rest of the files */
        IF cFileName = "." OR cFileName = ".." THEN NEXT fileLoop.

        IF INDEX(cAttrFile, "D") > 0 THEN DO:
            IF NOT cFileName BEGINS cDirBegins THEN NEXT.

            /* 11-JAN-2007 sla: ignore ade* dirs in DLC/gui */
            DO iDirToIgnore = 1 TO iDirToIgnoreMax:
                cDirToIgnore = ENTRY(iDirToIgnore, cExtProcIgnoreDir).
                IF cFullPath MATCHES cDirToIgnore THEN NEXT fileLoop.
            END.

            cFileName = cFileName + "/".
        END.
        ELSE IF NOT cFileName MATCHES pcMatches THEN NEXT fileLoop.

        /* avoid duplicates (may exist in multiple entries of propath) */
        IF CAN-FIND(FIRST ttFreeList WHERE ttFreeList.cItem = /*cBeginPath +*/ cFileName)
         THEN NEXT fileLoop.

        CREATE ttFreeList.
        ASSIGN
         ttFreeList.cItem       = cFileName
         ttFReeList.cSortOption = pcSort
         iFile                  = iFile + 1.
    END.

    INPUT CLOSE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareFuncMethList C-Win
PROCEDURE prepareFuncMethList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:     28-MAY-2007 sla:  added support of methods
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcBegins AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttFreeList.

DEFINE VARIABLE lSuperCurrentMethod AS LOGICAL     NO-UNDO.

DEFINE BUFFER ttFunc     FOR ttFunc.
DEFINE BUFFER ttMethod   FOR ttMethod.
DEFINE BUFFER ttFreeList FOR ttFreeList.

/* 30-OCT-2007 sla: new feature to insert currentMethod after 'SUPER:' */
IF pcBegins MATCHES "*&SuperCurrentMethod" THEN DO:
    pcBegins = SUBSTRING(pcBegins, 1, INDEX(pcBegins, "&SuperCurrentMethod") - 1).
    FIND FIRST ttMethod WHERE ttMethod.hEditor = phEditor AND ttMethod.cName = pcBegins NO-ERROR.
    IF NOT AVAILABLE ttMethod THEN RETURN.
    CREATE ttFreeList.
    ttFreeList.cItem = ttMethod.cName + "()".
    RETURN.
END.


FOR EACH ttFunc WHERE
     ttFunc.hEditor = phEditor
 AND ttFunc.cName BEGINS pcBegins
 AND ttFunc.cName <> pcBegins: /* already in the list */
    IF CAN-FIND(FIRST ttFreeList WHERE ttFreeList.cItem = ttFunc.cName + "()") THEN NEXT. /* 30-OCT-2007 sla:  already in the list => ignore */
    CREATE ttFreeList.
    ttFreeList.cItem = ttFunc.cName + "()".
END.

/* same routine to add methods.  Note that a OO Class can define prototyped functions (mapped to some procedure) */
FOR EACH ttMethod WHERE
     ttMethod.hEditor = phEditor
 AND ttMethod.cName BEGINS pcBegins
 AND ttMethod.cName <> pcBegins:  /* already in the list */
    IF CAN-FIND(FIRST ttFreeList WHERE ttFreeList.cItem = ttMethod.cName + "()") THEN NEXT. /* 30-OCT-2007 sla:  already in the list => ignore */
    CREATE ttFreeList.
    ttFreeList.cItem = ttMethod.cName + "()".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareGVarList C-Win
PROCEDURE prepareGVarList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcBegins AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttFreeList.

DEFINE BUFFER ttgvar     FOR ttgvar.
DEFINE BUFFER ttFreeList FOR ttFreeList.

FOR EACH ttgVar WHERE
     ttgVar.hEditor = phEditor
 AND ttgVar.cVar BEGINS pcBegins  /* 02-SEP-2007 sla: Do not use the MATCHES operator like for prepareProcList, otherwise the list can pop up too easily */
 AND ttgVar.cVar <> pcBegins :   /* already in the list */
    /* 27-NOV-2006 sla: avoid local/global duplicates in the list */
    IF CAN-FIND(ttFreeList WHERE ttFreeList.cItem = ttgVar.cVar) THEN NEXT.
    CREATE ttFreeList.
    ttFreeList.cItem = ttgVar.cVar.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareLibList C-Win
PROCEDURE prepareLibList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttFreeList.


DEFINE BUFFER ttFreeList   FOR ttFreeList.
DEFINE BUFFER ttgLibHandle FOR ttgLibHandle.
DEFINE BUFFER ttLibHandle  FOR ttLibHandle.


FOR EACH ttgLibHandle WHERE ttgLibHandle.hEditor = phEditor
 BREAK BY ttgLibHandle.cVar:
    IF NOT FIRST-OF(ttgLibHandle.cVar) THEN NEXT.
    CREATE ttFreeList.
    ttFreeList.cItem = ttgLibHandle.cVar.
END.

FOR EACH ttlibHandle BREAK BY ttlibHandle.cVar:
    IF NOT FIRST-OF(ttlibHandle.cVar) THEN NEXT.
    FIND FIRST ttFreeList WHERE ttFreeList.cItem = ttlibHandle.cVar NO-ERROR.
    IF AVAILABLE ttFreeList THEN NEXT.
    CREATE ttFreeList.
    ttFreeList.cItem = ttLibHandle.cVar.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preparePreprocList C-Win
PROCEDURE preparePreprocList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcPreviousWord AS CHARACTER   NO-UNDO. /* I may use this one later to make it smarter depending on the keyword */
DEFINE INPUT  PARAMETER phEditor       AS HANDLE      NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttFreeList.

DEFINE VARIABLE iSourceCurrentLine AS INTEGER   NO-UNDO.

DEFINE BUFFER ttPreproc      FOR ttPreproc.
DEFINE BUFFER ttUndefPreproc FOR ttPreproc.
DEFINE BUFFER ttEdt          FOR ttEdt.

FIND FIRST ttEdt WHERE ttEdt.hEditor = phEditor NO-ERROR.
IF NOT AVAILABLE ttEdt THEN RETURN.

iSourceCurrentLine = phEditor:CURSOR-LINE.

IF ttEdt.lManageableSections THEN DO:
    RUN StructuredFetchCurrentTtMark (BUFFER ttEdt, BUFFER ttMark).
    IF AVAILABLE ttMark THEN iSourceCurrentLine = iSourceCurrentLine + ttMark.iLine.
    ELSE iSourceCurrentLine = 99999999. /* this can occur when we deal with a new trigger or procedure that has not been saved yet.  In this case, then take all preprocessors */
END.

FOR EACH ttPreproc WHERE ttPreproc.hEditor = phEditor
                     AND ttPreproc.iLineNum < iSourceCurrentLine
                     AND ttPreproc.cName BEGINS pcPreviousWord
                     AND ttPreproc.cType <> "undefine":

    FIND FIRST ttFreeList WHERE ttFreeList.cItem = ttPreproc.cName NO-ERROR.
    IF AVAILABLE ttFreeList THEN NEXT. /* already in the list, ignore */

    /* As said in the parser, it is possible to undefine a global preprocessor defined at an above level */
    FIND FIRST ttUndefPreproc WHERE
         ttUndefPreproc.hEditor  = phEditor
     AND ttUndefPreproc.cName    = ttPreproc.cName
     AND ttUndefPreproc.cType    = "undefine"
     AND ttUndefPreproc.iLineNum < iSourceCurrentLine
     NO-ERROR.
    IF AVAILABLE ttUndefPreproc THEN NEXT.

    CREATE ttFreeList.
    ttFreeList.cItem = ttPreproc.cName.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareProcList C-Win
PROCEDURE prepareProcList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER pcBegins AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttFreeList.

DEFINE BUFFER ttProc       FOR ttProc.
DEFINE BUFFER ttFreeList   FOR ttFreeList.

FOR EACH ttProc WHERE
     ttProc.hEditor = phEditor
 /* 23-AUG-2007 sla: improve with matches operator   AND ttProc.cName BEGINS pcBegins */
 /* 02-SEP-2007 sla: BTW, do not do that for functions or variables otherwise they just pop up too often all the time */
 AND ttProc.cName MATCHES "*" + pcBegins + "*"
 AND ttProc.cName <> pcBegins:  /* already in the list */
 /* 23-AUG-2007 sla: why was I doing that ?  AND ttProc.cName > pcBegins: /* do not load the var name if it is already in the editor */  */
    CREATE ttFreeList.
    ttFreeList.cItem = ttProc.cName.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareVarList C-Win
PROCEDURE prepareVarList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcBegins AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttFreeList.

DEFINE BUFFER ttvar      FOR ttvar.
DEFINE BUFFER ttFreeList FOR ttFreeList.

FOR EACH ttVar WHERE
     ttVar.cVar  BEGINS pcBegins /* 02-SEP-2007 sla: Do not use the MATCHES operator like for prepareProcList, otherwise the list can pop up too easily */
 AND ttVar.cVar <> pcBegins:  /* already in the list */
    CREATE ttFreeList.
    ttFreeList.cItem = ttVar.cVar.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proposeOpenSelected C-Win
PROCEDURE proposeOpenSelected :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor     AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcFileToOpen AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hEditorListProc AS HANDLE      NO-UNDO.

RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).
RUN loadTableList IN hEditorListProc ("Open selected file", "openFileAction," + pcFileToOpen).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE queryInfo C-Win
PROCEDURE queryInfo :
/*------------------------------------------------------------------------------
  Purpose: Create a dyn query to test a given query phrase of FIND statement
           If valid, then report the indices that are used by each buffer

          Thanks to Richard Tardivon for the first base of the code this code.

 04-FEB-2007 sla: ABHack now knows the indices of temp-table and is able to
 create a TT or DB buffer on the fly very quickely.  Therefor it becomes possible
 to make this great query analyzer handle TT as as DB tables (with additional buffes)
 It should also make the code even simpler, as I just have to replace variable names
 by the unknown value (not the TT fields)
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE    NO-UNDO.
DEFINE INPUT  PARAMETER pcQuery AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cBufferName         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCurrentName        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDetailMessage      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrevName           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cQueryWithConstants AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cQueryWithSpaces    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTable              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTmpQueryString     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cWord               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hBuffer             AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTestField          AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTestBuffer         AS HANDLE     NO-UNDO.
DEFINE VARIABLE hQry                AS HANDLE     NO-UNDO.
DEFINE VARIABLE iBuffer             AS INTEGER    NO-UNDO.
DEFINE VARIABLE iIndex              AS INTEGER    NO-UNDO.
DEFINE VARIABLE iInFrameWordBegins  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iInFrameWordEnd     AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNumWords           AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTestInteger        AS INTEGER    NO-UNDO.
DEFINE VARIABLE iWord               AS INTEGER    NO-UNDO.
DEFINE VARIABLE lFindMode                    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lForFirstLastIgnoreByWarning AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lOk                          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lOneSingleIndexOnly          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lOneSingleIndexOnlyDone      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lQueryWithAddedForeignBuffer AS LOGICAL   NO-UNDO.

DEFINE BUFFER ttQueryInfoBuffer FOR ttQueryInfoBuffer.
DEFINE BUFFER tttt              FOR tttt.
DEFINE BUFFER ttfld             FOR ttfld.
DEFINE BUFFER ttgVar            FOR ttgVar.
DEFINE BUFFER ttVar             FOR ttVar.

CREATE WIDGET-POOL.

&SCOPED-DEFINE RETURN DO: DELETE WIDGET-POOL.  FOR EACH ttQueryInfoBuffer:    DELETE ttQueryInfoBuffer. END.  ERROR-STATUS:ERROR = NO.  RETURN.  END.
&SCOPED-DEFINE tweakedQueryWithForeignBuffer (IF lQueryWithAddedForeignBuffer THEN "The query to analyze has been tweaked to handle a foreign BUFFER refered with the OF operator" + CHR(10) ELSE "") +
/* Should not be necessary, but who knows */
FOR EACH ttQueryInfoBuffer: DELETE ttQueryInfoBuffer. END.

IF INDEX(pcQuery, "/*QueryWithAddedForeignBuffer*/") > 0 THEN ASSIGN
 lQueryWithAddedForeignBuffer = YES
 pcQuery = REPLACE(pcQuery, "/*QueryWithAddedForeignBuffer*/", "").

ASSIGN
 pcQuery = TRIM(pcQuery)
 pcQuery = TRIM(pcQuery, ":")  /* the last ':' pollutes the analyses of words */
 pcQuery = REPLACE(pcQuery, "~{", "")  /* this preprocessor characters can lead ... */
 pcQuery = REPLACE(pcQuery, "~}", "")  /* ... to severe instabilities */
 pcQuery = REPLACE(pcQuery, "&", "")  /* do not know if required, but sure removing this char will not harm */
 pcQuery = REPLACE(pcQuery ,CHR(10)," ")
 pcQuery = REPLACE(pcQuery, CHR(13)," ").


/* 02-OCT-2007 sla: remove comments from the query */
DO WHILE pcQuery NE cTmpQueryString:
    ASSIGN
     cTmpQueryString = pcQuery
     pcQuery = removeLineComments(pcQuery, 0).
END.


IF pcQuery MATCHES "*~~~." THEN pcQuery = SUBSTRING(pcQuery, 1, LENGTH(pcQuery) - 1). /* 08-DEC-2006 sla: remove last period if any, but only the last one */

/* inner-trim spaces */
DO WHILE pcQuery NE cTmpQueryString:
    ASSIGN
     cTmpQueryString = pcQuery
     pcQuery = REPLACE(pcQuery,"  "," ").
END.

/* 10-SEP-2007 sla: ignore "BREAK BY" that is not (yet) supported in a dynamic query (should be in 10.1C) */
ASSIGN
 pcQuery = REPLACE(pcQuery," BREAK BY"," BY")
 pcQuery = REPLACE(pcQuery," EXCLUSIVE-LOCK"," NO-LOCK")
 pcQuery = REPLACE(pcQuery," EXCLUSIVE"," NO-LOCK")  /* 14-DEC-2006 sla: Hmm ,this single 'exclusive' was missing... */
 pcQuery = REPLACE(pcQuery," SHARE-LOCK"," NO-LOCK")
 pcQuery = REPLACE(pcQuery," NO-WAIT"," ").    /* 01-OCT-2007 sla: a find with NO-WAIT will be analyzed with a query, so no NO-WAIT */


/* 04-JUL-2007 sla: prevent a possible core hang when the QUERY contains keyWords like "IN FRAME*/
cTmpQueryString = "".
pcQuery = pcQuery + " ".
DO WHILE pcQuery NE cTmpQueryString:
    cTmpQueryString = pcQuery.
    iInFrameWordEnd = 0. /* reset */
    iInFrameWordBegins = INDEX(pcQuery, "IN FRAME ").
    IF iInFrameWordBegins > 0 THEN iInFrameWordEnd = INDEX(pcQuery, " ", iInFrameWordBegins + 9).
    IF iInFrameWordEnd > 0 THEN SUBSTRING(pcQuery, iInFrameWordBegins, iInFrameWordEnd - iInFrameWordBegins) = "".
END.
pcQuery = TRIM(pcQuery).

/* 14-DEC-2006 sla: Hey, we could also support the CAN-FIND() couldn't we ?*/
IF pcQuery BEGINS "CAN-FIND("
 AND pcQuery MATCHES "*)"
 THEN ASSIGN /* amazing, I need to do that in two steps, otherwise it gets confused with the lenght of pcQuery */
  pcQuery = SUBSTRING(pcQuery, 10) /* remove leading CAN-FIND(  */
  pcQuery = "FIND " + SUBSTRING(pcQuery, 1, LENGTH(pcQuery) - 1). /* remove trainling ')' */

/* Cheet to kind-of support the analyses of a FIND statement.  Note that a FIND cannot use
 multiple indicies, so we will have to ignore index info after the first index of the hack query */
IF pcQuery BEGINS "FIND " THEN DO:
    ASSIGN
     lFindMode           = YES
     pcQuery             = "FOR " + SUBSTRING(pcQuery, 6)
     lOneSingleIndexOnly = YES.
    IF CAN-DO("FIRST,LAST,NEXT,PREV*", ENTRY(2, pcQuery, " " ))
     THEN ENTRY(2, pcQuery, " ") = "EACH".
    ELSE ENTRY(2, pcQuery, " ") = "EACH " + ENTRY(2, pcQuery, " ").
    pcQuery = REPLACE(pcQuery, "NO-ERROR", "").
END.

/* Cheet to support simple FOR FIRST and FOR LAST Statement.  Indeed a regular query cannot support
 FIRST or LAST for the first table.
  30-NOV-2006 sla: compared with result on an XREF, a FOR LAST seem to be able to use mutiple */
IF pcQuery BEGINS "FOR LAST" OR pcQuery BEGINS "FOR FIRST" THEN DO:
    ASSIGN
     ENTRY(2, pcQuery, " ") = "EACH".
     /* lOneSingleIndexOnly    = YES. */  /* 22-JUN-2007 sla: FOR FIRST and FOR LAST  */
    IF pcQuery MATCHES "* BY *"
     OR pcQuery MATCHES "*~nBY *"
     THEN lForFirstLastIgnoreByWarning = YES.
END.


/* determine the buffers used by this query */
/* it's assummed we don't use any duplicate tables in multiple databases */
ASSIGN
 iNumWords = NUM-ENTRIES(pcQuery," ")
 cCurrentName = "".

CREATE QUERY hQry.
DO iWord = 1 TO iNumWords:
    ASSIGN
     cPrevName    = cCurrentName
     cCurrentName = TRIM(ENTRY(iWord,pcQuery," "))
     cBufferName  = "".

    IF NOT CAN-DO("EACH,LAST,FIRST", LEFT-TRIM(cPrevName, ",")) THEN NEXT.

    IF lFindMode AND CAN-FIND(FIRST ttQueryInfoBuffer) THEN DO:
        cDetailMessage = {&tweakedQueryWithForeignBuffer} "Abort static FIND query analyses~n A FIND statement shall not be performed on more than one buffer"
                        + "~nWhat to do with '" + cCurrentName + "' in :~n " + "FIND " + SUBSTRING(pcQuery, 4).
        IF glQADispInABHWin THEN PUBLISH "abhackDisplayInfo" (cDetailMessage).
        ELSE RUN showTooltip IN TARGET-PROCEDURE (phEditor, cDetailMessage, "single,visibleTime=" + STRING(giQueryTooltipTimeFail)).
        {&RETURN}
    END.
    CREATE ttQueryInfoBuffer.
    /* cCurrentName = wordBuffer(cCurrentName, "removeDb,noField"). /* 04-FEB-2007 sla: is this still needed ? */ */
    /* cCurrentName = wordBuffer(cCurrentName, "").  /* MAR-15-2007 sla: no, it was even bad when a database name was supplied */ */
    cCurrentName = wordBuffer(cCurrentName, "noField").  /* 22-MAR-2007 sla: but "nofield" was really required for cases where the database is not qualified...  */
    ttQueryInfoBuffer.hBuffer = createTTorDBBuffer(phEditor, cCurrentName, "").

    /* if it not a valid table then tell the user we don't know what to do with it */
    IF   NOT VALID-HANDLE(ttQueryInfoBuffer.hBuffer)
/* 07-FEB-2007 sla:     AND KEYWORD-ALL(cCurrentName) EQ ? */
     THEN DO:
        cDetailMessage = {&tweakedQueryWithForeignBuffer} "Abort static query analyses" + CHR(10) + "Don't know what to do with '"
                        + cCurrentName + "' in query:~n " + pcQuery + "~n => Perhaps you should load or refresh the global resources.".
        IF glQADispInABHWin THEN PUBLISH "abhackDisplayInfo" (cDetailMessage).
        ELSE RUN showTooltip IN TARGET-PROCEDURE (phEditor , cDetailMessage , "single,visibleTime=" + STRING(giQueryTooltipTimeFail)).
        {&RETURN}
    END.

    /* 24-AUG-2007 sla: better warning against non Progress Databases */
    IF   ttQueryInfoBuffer.hBuffer:TYPE = "BUFFER"
     AND ttQueryInfoBuffer.hBuffer:DBNAME <> "PROGRESST"
     AND CAN-DO("PROGRESS,OpenEdge", DBTYPE(ttQueryInfoBuffer.hBuffer:DBNAME)) = NO THEN DO:
        cDetailMessage = {&tweakedQueryWithForeignBuffer} "Abort static query analyses" + CHR(10) + "Cannot analyze foreign database type " + QUOTER(DBTYPE(ttQueryInfoBuffer.hBuffer:DBNAME))
                        + " for buffer '" + cCurrentName + "' in query:~n " + pcQuery + "~n => Use a Progress Database Schema to perform this kind of analyses"
                        + '~nIf you always use foreign databases and if you are not interested in analysing Temp-table queries, then you should disable the "Analyse Text Selected Static Queries" feature (in the Misc Page)'.
        IF glQADispInABHWin THEN PUBLISH "abhackDisplayInfo" (cDetailMessage).
        ELSE RUN showTooltip IN TARGET-PROCEDURE (phEditor , cDetailMessage , "single,visibleTime=" + STRING(giQueryTooltipTimeFail)).
        {&RETURN}
    END.


    hQry:ADD-BUFFER(ttQueryInfoBuffer.hBuffer).
    ttQueryInfoBuffer.cTableName = ttQueryInfoBuffer.hBuffer:NAME.
END. /* ttQueryInfoBuffer iWord = 1 TO iNumWords */

/* 29-NOV-2006 sla: now let's try to replace variable names or tt-fields by the most general constant value
 which happens to be the unknown value '?' */
/* first let's have a phrase with space space separated words */

cQueryWithSpaces = REPLACE(pcQuery, "(", " ").
cQueryWithSpaces = REPLACE(cQueryWithSpaces, ")", " ").
cQueryWithConstants = pcQuery.
iNumWords = NUM-ENTRIES(cQueryWithSpaces, " ").


wordLoop:
DO iWord = 1 TO iNumWords:
    cWord = ENTRY(iWord, cQueryWithSpaces, " ").
    cWord = TRIM(cWord, ","). /* remove leading and trailing comma */
    IF cWord = '?' THEN NEXT.
    IF KEYWORD-ALL(cWord) <> ? THEN NEXT. /* Progress key word (function, or query word) */
    IF cWord MATCHES '"*"' OR cWord MATCHES "'*'" THEN NEXT. /* CHARACTER constant */
    iTestInteger = INTEGER(cWord) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN NEXT.  /* INTEGER constant, keep as is */
    /* table name or table.field ? */
    /*  MAR-15-2007 sla: better support of qualified dbName
     cTable = wordBuffer(cWord, "nofield"). /* if table.field => get table */ */
    cTable = wordBuffer(cWord, "removeDb"). /* if table.field => get table */
    cTable = wordBuffer(cWord, "nofield,removeDb"). /* if table.field => get table */ /* 27-MAR-2007 sla: the 'nofield' option was really important... */
    FIND FIRST ttQueryInfoBuffer WHERE ttQueryInfoBuffer.cTableName = cTable NO-ERROR.
    IF AVAILABLE ttQueryInfoBuffer THEN NEXT wordLoop. /* valid Buffer field used in query */

    /* at this point, cWord is likely to be a variable name, or a foreign temp-table field or database
    table field (not part of the query) */
    IF INDEX(cWord, ".") > 1 THEN DO:
        /* valid TT or DB buffer */
        cBufferName = wordBuffer(cWord, "").

        hTestBuffer = createTTorDBBuffer(phEditor, cBufferName, "").
        IF NOT VALID-HANDLE(hTestBuffer) THEN NEXT wordLoop. /* let it fail to show we do not know what to do with it */

        /* valid field name ? */
        cCurrentName = wordField(cWord).
        hTestField = ?. /* reset */
        hTestField = hTestBuffer:BUFFER-FIELD(cCurrentName) NO-ERROR.
        IF NOT VALID-HANDLE(hTestField) THEN NEXT wordLoop. /* show we do not know what to do with that */

        FIND FIRST ttQueryInfoBuffer WHERE ttQueryInfoBuffer.cTableName = cBufferName NO-ERROR.
        IF AVAILABLE ttQueryInfoBuffer THEN NEXT. /* that was valid buffer.field of the query */

        /* add this cBufferName foreign buffer to the beginning of the query */
        ENTRY(1, pcQuery, " ") = "/*QueryWithAddedForeignBuffer*/". /* Main point is to remove the first FOR */
        RUN queryInfo (phEditor, "FOR EACH " + cBufferName + " ," + pcQuery).
        {&RETURN}
    END.



    /* Valid variable name ? */
    ELSE DO:
        FIND FIRST ttgVar WHERE ttgVar.hEditor = phEditor AND ttgVar.cVar = cWord NO-ERROR.
        FIND FIRST ttVar WHERE ttVar.cVar = cWord NO-ERROR.
        IF    NOT AVAILABLE ttgVar
          AND NOT AVAILABLE ttVar
          /* 04-JUL-2007 sla: prevent a possible core hang when the QUERY contains keyWords like ":SCREEN-VALUE"  or "IN FRAME*/
          AND INDEX(cWord, ":SCREEN-VALUE") = 0   /* let it be replaced ... */
          AND INDEX(cWord, ":INPUT-VALUE") = 0     /* ... by unknown  */
          AND INDEX(cWord, ":BUFFER-VALUE") = 0     /* ... by unknown  */
          THEN DO: /* 22-DEC-2006 sla: fix regression introduced on 12-DEC-2006 (let QA replace global variables) */
            /* 12-DEC-2006 sla: are we facing with a foreign buffer used after a OF operator */
            IF iWord > 1 AND ENTRY(iWord - 1, cQueryWithSpaces, " ") = "OF" THEN DO:
                hTestBuffer = createTTorDBBuffer(phEditor, cWord, "").
                IF NOT VALID-HANDLE(hTestBuffer) THEN NEXT wordLoop. /* unvalid buffer name, leave as is to make query-prepare fail */

                /* OK that was a valid foreign buffer after an OF operator (foreign means not part of the query:GET-BUFFER-HANDLE(x))
                   so let's dare the following: */
                ENTRY(1, pcQuery, " ") = "/*QueryWithAddedForeignBuffer*/". /* Main point is to remove the first FOR */
                RUN queryInfo (phEditor, "FOR EACH " + cWord + " ," + pcQuery).
                {&RETURN}
            END.
            NEXT wordLoop. /* unvalid variable => leave as is to make query-prepare fail */
        END. /* IF NOT AVAILABLE ttgVar AND NOT AVAILABLE ttVar  */
    END. /* Valid variable name ? */

    /* At this point cWord is a word that should be replaced by '?' to let the query cQueryWithConstants prepare
     without error.  Notice that the buffers of cQueryWithConstants and their fields have not been replaced, so this
     query will use the indices that we are interested in */

    /* 07-DEC-2006 sla: refine so a simple variable like 'r' does not change WHERE into WHE?E
    ASSIGN
     cQueryWithConstants = REPLACE(cQueryWithConstants, cWord, "'?'")
     cQueryWithSpaces    = REPLACE(cQueryWithSpaces,    cWord, "'?'")*/

    cQueryWithConstants = queryReplaceWord(cQueryWithConstants, cWord, "'?'").
    cQueryWithSpaces    = queryReplaceWord(cQueryWithSpaces   , cWord, "'?'").
END.
/* 29-NOV-2006 sla: enhanced query analyses end */



lOk = hQry:QUERY-PREPARE(cQueryWithConstants) NO-ERROR.
IF NOT lOk OR ERROR-STATUS:ERROR THEN DO:
    IF glQADispInABHWin THEN PUBLISH "abhackDisplayInfo" (cDetailMessage).
    ELSE RUN showTooltip IN TARGET-PROCEDURE (phEditor, {&tweakedQueryWithForeignBuffer} "Unable to prepare the query~n"
        + "query string " + cQueryWithConstants
        + "~nERROR-MESSAGE" + ERROR-STATUS:GET-MESSAGE(1)
        + "~n   => Have the global vars been loaded OK?"
       , "single,visibleTime=" + STRING(giQueryTooltipTimeFail)).
    {&RETURN}
END.

iNumWords = hQry:NUM-BUFFERS.

/* 23-JUL-2008 sla: actually, it seems that calling query-prepare() is enough to know the INDEX-INFORMATION, no need to call QUERY-OPEN()
Not only it is not necessary, but it is actually rather bad when one develops against a large database, because opening the query can also keep the PROCESS very busy for nuts when the QUERY is not optimized (optimizing the QUERY is actually the prupose of the QUERY optimizer...)

lOk = hQry:QUERY-OPEN() NO-ERROR.
IF lOk = NO OR ERROR-STATUS:ERROR THEN DO:
    IF glQADispInABHWin THEN PUBLISH "abhackDisplayInfo" (cDetailMessage).
    ELSE RUN showTooltip IN TARGET-PROCEDURE (phEditor, {&tweakedQueryWithForeignBuffer} "Unable to open the query~n"
        + "~nquery string" + cQueryWithConstants
        + "~nERROR-MESSAGE" + ERROR-STATUS:GET-MESSAGE(1)
        + "~n   => Have the global vars been loaded OK?"
       , "single,visibleTime=" + STRING(giQueryTooltipTimeFail)).
    {&RETURN}
END.
*/

cdetailMessage = "Query analyzed successfully:".
DO iWord = 1 TO iNumWords:
    ASSIGN
     hBuffer = hQry:GET-BUFFER-HANDLE(iWord)
     cdetailMessage = cdetailMessage + "~n"
      + SUBSTITUTE("buffer &1 uses index&3:"
                  ,hBuffer:NAME
                  ,hBuffer:TABLE
                  ,(IF NUM-ENTRIES(hQry:INDEX-INFORMATION(iWord)) > 1 THEN "es" ELSE ""))
     NO-ERROR. /* 28-AUG-2007 sla: weird, some   FOR EACH   CREATE   block can lead to an error here  */
    DEFINE VARIABLE iNumIndexInfo AS INTEGER     NO-UNDO.
    /* 28-AUG-2007 sla: new trick to avoid possible errors with block of code that do a FOR EACH  CREATE  */
    INumIndexInfo = NUM-ENTRIES(hQry:INDEX-INFORMATION(iWord)) NO-ERROR.
    IF ERROR-STATUS:ERROR OR iNumIndexInfo = ? OR iNumIndexInfo = 0 THEN DO:
        {&RETURN}
    END.
    DO iIndex = 1 TO iNumIndexInfo:

        /* 30-NOV-2006 sla: compared result with XREF, a FOR FIRST or FOR LAST can indeed not use more than one index */
        IF lOneSingleIndexOnly
         AND lOneSingleIndexOnlyDone
         THEN NEXT.

        /* 06-DEC-2006 sla: For a FIND, ignore WHOLE-INDEX if there is another index info */
        IF lOneSingleIndexOnly
         AND ENTRY(iIndex, hQry:INDEX-INFORMATION(iWord)) = "WHOLE-INDEX"
         AND NUM-ENTRIES(hQry:INDEX-INFORMATION(iWord)) > iIndex
         THEN NEXT.

        cdetailMessage = cdetailMessage + "~n  " + ENTRY(iIndex, hQry:INDEX-INFORMATION(iWord)).

        /* 07-DEC-2006 sla: also handle the ROWID special case */
        IF NOT CAN-DO("WHOLE-INDEX,ROWID,RECID", ENTRY(iIndex, hQry:INDEX-INFORMATION(iWord)))
         THEN cdetailMessage = cdetailMessage + " " + queryIndexDetails( hBuffer, ENTRY(iIndex, hQry:INDEX-INFORMATION(iWord))).
        lOneSingleIndexOnlyDone = YES.
    END.
    IF lForFirstLastIgnoreByWarning THEN cDetailMessage = cDetailMessage + "~n  WARNING: FOR FIRST/LAST IGNORE 'BY' CLAUSES".
END.

IF glQADispInABHWin THEN PUBLISH "abhackDisplayInfo" (cDetailMessage).
ELSE RUN showTooltip IN TARGET-PROCEDURE (phEditor, {&tweakedQueryWithForeignBuffer} cdetailMessage, "single,visibleTime=" + STRING(giQueryTooltipTimeSuccess)).

hQry:QUERY-CLOSE().

{&RETURN}

&UNDEFINE RETURN
&UNDEFINE tweakedQueryWithForeignBuffer


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recompileAbhack C-Win
PROCEDURE recompileAbhack :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
/* OE 10 V9 compatible time stamps */
DEFINE VARIABLE cABHackRoot          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAttributes          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cdotRFileTimeStamp   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cExtension           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFileName            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFullPathName        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE csourceFileTimeStamp AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ddotRFileTimeStamp   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dsourceFileTimeStamp AS DECIMAL     NO-UNDO.

FILE-INFO:FILE-NAME = "protools/abhack".
cABHackRoot = FILE-INFO:FULL-PATHNAME.
ddotRFileTimeStamp = DECIMAL(12/31/7777) * 86400.0. /* will take place in a long long time */

INPUT FROM OS-DIR (cABHackRoot).
REPEAT:
    IMPORT cFileName cFullPathName cAttributes.

    IF INDEX(cAttributes, "F") = 0 THEN NEXT.
    cExtension = SUBSTRING(cFileName, R-INDEX(cFileName, ".") + 1).
    IF cExtension = "r" THEN DO:
        FILE-INFO:FILE-NAME = cFullPathName.
        IF (FILE-INFO:FILE-MOD-DATE - 08/20/2007) * 86400 + FILE-INFO:FILE-MOD-TIME
          < ddotRFileTimeStamp
         THEN ASSIGN
           ddotRFileTimeStamp = (FILE-INFO:FILE-MOD-DATE - 08/20/2007) * 86400 + FILE-INFO:FILE-MOD-TIME
           cdotRFileTimeStamp = cFileName + "  " + STRING(FILE-INFO:FILE-MOD-DATE) + " " + STRING(FILE-INFO:FILE-MOD-TIME, "hh:mm:ss").
    END.
    IF CAN-DO("i,w,p,cls", cExtension) THEN DO:
        FILE-INFO:FILE-NAME = cFullPathName.
        IF (FILE-INFO:FILE-MOD-DATE - 08/20/2007) * 86400 + FILE-INFO:FILE-MOD-TIME
          > dsourceFileTimeStamp
         THEN ASSIGN
           dsourceFileTimeStamp = (FILE-INFO:FILE-MOD-DATE - 08/20/2007) * 86400 + FILE-INFO:FILE-MOD-TIME
           csourceFileTimeStamp = cFileName + "  " + STRING(FILE-INFO:FILE-MOD-DATE) + " " + STRING(FILE-INFO:FILE-MOD-TIME, "hh:mm:ss").
    END.
END.
INPUT CLOSE.

IF   cdotRFileTimeStamp > ""
 AND (   ddotRFileTimeStamp > dsourceFileTimeStamp
      /* 06-SEP-2007 sla: allow people leaing on the west of France to apply a package with source files ahead of their times... */
      OR     ddotRFileTimeStamp   > (TODAY - 08/20/2007) * 86400 + TIME - 60 /* let them one minute after recomile... */
         AND dsourceFileTimeStamp > (TODAY - 08/20/2007) * 86400 + TIME    /* if some source files are ahead of their local time */
     )
 THEN DO:
    cmon:INSERT-STRING(
     STRING(TIME, "hh:mm:ss") + " Compile check OK: ABHack .r's are more recent than sources, or sources are ahead of local time with .r's not older than one minute"
      + "  Details: source: " + QUOTER(csourceFileTimeStamp)
      + "  .r: " + QUOTER(cdotRFileTimeStamp) + "~n"
     ) IN FRAME {&FRAME-NAME}.
    RETURN.
END.

VIEW FRAME {&FRAME-NAME} IN WINDOW {&WINDOW-NAME}.
{&WINDOW-NAME}:VISIBLE = YES.

IF cdotRFileTimeStamp = "" THEN cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No .r found for ABHack, about to recompile all".

cmon:SCREEN-VALUE = STRING(TIME, "hh:mm:ss") + " Compile check : Found at least one ABHack .r older than one source file => recompiling all..."
  + "~nsource: " + QUOTER(csourceFileTimeStamp) + "  .r: " + QUOTER(cdotRFileTimeStamp).
cmon:CURSOR-OFFSET = cmon:LENGTH.
PROCESS EVENTS.  /* flush the UI */

INPUT FROM OS-DIR (cABHackRoot).
REPEAT:
    IMPORT cFileName cFullPathName cAttributes.

    IF INDEX(cAttributes, "F") = 0 THEN NEXT.
    cExtension = SUBSTRING(cFileName, R-INDEX(cFileName, ".") + 1).
    IF cExtension <> "r" THEN NEXT.
    OS-DELETE VALUE(cFullPathName).
    cmon:INSERT-STRING("~n" + STRING(TIME, "hh:mm:ss") + " deleting " + cFileName + " ...").
END.
INPUT CLOSE.


INPUT FROM OS-DIR (cABHackRoot).
REPEAT:
    IMPORT cFileName cFullPathName cAttributes.

    IF INDEX(cAttributes, "F") = 0 THEN NEXT.
    cExtension = SUBSTRING(cFileName, R-INDEX(cFileName, ".") + 1).
    IF CAN-DO("w,p,cls", cExtension) THEN DO:
        /* these files require 10.0A+ */
        IF     cFileName MATCHES "*xml*"
          AND (PROVERSION BEGINS "9" OR PROVERSION  BEGINS "10.0")
          THEN NEXT.

        cmon:INSERT-STRING("~n" + STRING(TIME, "hh:mm:ss") + " Compiling " + cFileName + " ...").
        COMPILE VALUE(cFullPathName) SAVE NO-ERROR.
    END.
END.
INPUT CLOSE.

IF cdotRFileTimeStamp = "" THEN RETURN.  /* We were not running an old .r */

IF   OS-GETENV("username") <> "slacroix" /* I know what I'm doing... */
 AND OS-GETENV("username") <> "dries" /* me too ;-) */
 THEN MESSAGE "FYI, ABHack has been recompiled.  As this very procedure may have changed, it is going to be restarted." SKIP
      "Details: " SKIP
      "source:" QUOTER(csourceFileTimeStamp) SKIP
       ".r:" QUOTER(cdotRFileTimeStamp)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
RETURN "exitABHack".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refineDictWidget C-Win
PROCEDURE refineDictWidget :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

DEFINE VARIABLE hfg AS HANDLE     NO-UNDO.
DEFINE VARIABLE h   AS HANDLE     NO-UNDO.

hDictFrame = hwDict:FIRST-CHILD. /* 1st frame */
hfg = hDictFrame:FIRST-CHILD. /* 1st field group */

hwDict:RESIZABLE = YES.
hwDict:MAX-WIDTH-PIXELS = SESSION:WORK-AREA-WIDTH-PIXELS.
hwDict:MIN-WIDTH-PIXELS = hwDict:WIDTH-PIXELS.
hwDict:MAX-HEIGHT-PIXELS = SESSION:WORK-AREA-HEIGHT-PIXELS.
hwDict:MIN-HEIGHT-PIXELS = hwDict:HEIGHT-PIXELS. /* that should have been done at the very beginning */
ON 'WINDOW-RESIZED':U OF hwDict PERSISTENT RUN DictResized IN THIS-PROCEDURE.

h = hfg:FIRST-CHILD.
DO WHILE h <> ?:
    CASE h:NAME:
        WHEN "s_DbLbl2"         THEN  hs_txt_Dbs     = h.
        WHEN "s_DbFill"         THEN  hs_fil_Dbs     = h.
        WHEN "s_lst_Dbs"        THEN  hs_lst_Dbs     = h.

        WHEN "s_Lvl1Lbl"        THEN  hs_txt_Tbls    = h.
        WHEN "s_TblFill"        THEN  hs_fil_Tbls    = h.
        WHEN "s_lst_Tbls"       THEN  hs_lst_Tbls    = h.

        WHEN "s_SeqFill"        THEN  hs_fil_Seqs    = h.
        WHEN "s_lst_Seqs"       THEN  hs_lst_Seqs    = h.

        WHEN "s_FldFill"        THEN  hs_fil_Flds    = h.
        WHEN "s_Lvl2Lbl"        THEN  hs_txt_Flds    = h.
        WHEN "s_lst_Flds"       THEN  hs_lst_Flds    = h.

        WHEN "s_IdxFill"        THEN  hs_fil_Idxs    = h.
        WHEN "s_lst_Idxs"       THEN  hs_lst_Idxs    = h.

        WHEN "s_icn_Dbs"        THEN  hs_btn_Dbs     = h.
        WHEN "s_icn_Tbls"       THEN  hs_btn_Tbls    = h.
        WHEN "s_icn_Seqs"       THEN  hs_btn_Seqs    = h.
        WHEN "s_icn_Flds"       THEN  hs_btn_Flds    = h.
        WHEN "s_icn_Idxs"       THEN  hs_btn_Idxs    = h.

        WHEN "s_btn_Create"     THEN  hs_btn_Create  = h.
        WHEN "s_btn_Props"      THEN  hs_btn_Props   = h.
        WHEN "s_btn_Delete"     THEN  hs_btn_Delete  = h.
        WHEN "s_Browse_Stat"    THEN  hs_Browse_Stat = h.
    END CASE.
    h = h:NEXT-SIBLING.
END.

/* show that there is something new... */
APPLY 'MOUSE-SELECT-DOWN' TO hs_btn_Flds.
hwDict:HEIGHT-PIXELS = hwDict:HEIGHT-PIXELS + 200.
hwDict:WIDTH-PIXELS = hwDict:WIDTH-PIXELS + 50.
APPLY 'WINDOW-RESIZED' TO hwDict.

hwDict:TITLE = hwDict:TITLE + "  (made resizable by Seb's ABHack)".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE registerAllEditorWindows C-Win
PROCEDURE registerAllEditorWindows :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE hABHchildWin AS HANDLE   NO-UNDO.
DEFINE VARIABLE hEditor      AS HANDLE   NO-UNDO.
DEFINE VARIABLE hFrame       AS HANDLE   NO-UNDO.
DEFINE VARIABLE hWin         AS HANDLE   NO-UNDO.

chCtrlSpy:PSTimerSpy:ENABLED = NO.

cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Registering all Editor Windows~n" + cmon:SCREEN-VALUE.
PROCESS EVENTS. /* flush the UI */

DEFINE BUFFER ttEdt FOR ttEdt.

hWin = SESSION:FIRST-CHILD.
DO WHILE hWin <> ?:
    IF hwin:TITLE = "AppBuilder" THEN DO:
        hABHchildWin = hWin:FIRST-CHILD.
        DO WHILE hABHchildWin <> ?:
            hEditor = ?. /* reset */
            hFrame = hABHchildWin:FIRST-CHILD NO-ERROR.

            IF VALID-HANDLE(hFrame) AND hFrame:TYPE = "FRAME"
             THEN hEditor = hFrame:FIRST-CHILD:LAST-CHILD NO-ERROR.

            IF VALID-HANDLE(hEditor) THEN DO WHILE TRUE:
                IF VALID-HANDLE(hEditor) AND hEditor:TYPE = "EDITOR" AND hEditor:SOURCE-EDITOR THEN LEAVE.
                hEditor = hEditor:PREV-SIBLING NO-ERROR. /*14-DEC-2006 sla: Added NO-ERROR for case of window without frame*/
                IF hEditor = ? THEN DO:
                    hFrame = hFrame:NEXT-SIBLING NO-ERROR.
                    IF hFrame = ? THEN LEAVE.
                    hEditor = hFrame:FIRST-CHILD:LAST-CHILD NO-ERROR. /*14-DEC-2006 sla: case of window without frame*/
                    IF hEditor = ? THEN LEAVE.
                END.
            END.

            IF VALID-HANDLE(hEditor) AND hEditor:TYPE = "EDITOR"
             AND hEditor:SOURCE-EDITOR
             AND NOT CAN-FIND(FIRST ttEdt WHERE ttEdt.hEditor = hEditor) /* 26-OCT-2007 sla: added this condition so we can recall registerAllEditorWindows */
             AND hABHchildWin:VISIBLE /* 10-SEP-2007 sla: do not register hidden widows managed by the AppBuilder */
             THEN RUN keepTrackEdt (hEditor, BUFFER ttEdt).

            hABHchildWin = hABHchildWin:NEXT-SIBLING.
        END. /* DO WHILE hABHchildWin <> ? */
    END. /* manage AppBuilder children windows */


    hEditor = ?. /* reset */

    hFrame = hWin:FIRST-CHILD NO-ERROR.
    IF VALID-HANDLE(hFrame) AND hFrame:TYPE = "FRAME"
     THEN hEditor = hFrame:FIRST-CHILD:LAST-CHILD NO-ERROR.
    IF VALID-HANDLE(hEditor) THEN DO WHILE TRUE:
        IF VALID-HANDLE(hEditor) AND hEditor:TYPE = "EDITOR" AND hEditor:SOURCE-EDITOR THEN LEAVE.
        hEditor = hEditor:PREV-SIBLING NO-ERROR. /*14-DEC-2006 sla: Added NO-ERROR for case of window without frame*/
        IF hEditor = ? THEN DO:
            hFrame = hFrame:NEXT-SIBLING NO-ERROR.
            IF hFrame = ? THEN LEAVE.
            hEditor = hFrame:FIRST-CHILD:LAST-CHILD NO-ERROR. /*14-DEC-2006 sla: case of window without frame*/
            IF hEditor = ? THEN LEAVE.
        END.
    END.

    IF VALID-HANDLE(hEditor) AND hEditor:TYPE = "EDITOR"
     AND hEditor:SOURCE-EDITOR
     AND hWin:VISIBLE  /* 10-SEP-2007 sla: do not register hidden widows managed by the AppBuilder */
     AND NOT CAN-FIND(FIRST ttEdt WHERE ttEdt.hEditor = hEditor) /* 26-OCT-2007 sla: added this condition so we can recall registerAllEditorWindows */
     THEN RUN keepTrackEdt (hEditor, BUFFER ttEdt).

    hWin = hWin:NEXT-SIBLING.
END. /* DO WHILE hWin <> ?: */

chCtrlSpy:PSTimerSpy:ENABLED = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rememberOpenedFiles C-Win
PROCEDURE rememberOpenedFiles :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:       12-OCT-2013 jcc: save open editors to be able to reopen them at startup
------------------------------------------------------------------------------*/
DEFINE VARIABLE cCurrentEvent        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCurrentSection      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCurrentWidget       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOpenedFiles         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOpenedFilesGeometry AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOpenedFilesPosition AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOpenedFilesSection  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSection             AS CHARACTER   NO-UNDO.

DEFINE BUFFER bbttEdt FOR ttEdt.

IF NOT glSaveSession THEN RETURN.

/* 23-OCT-2013 jcc: if called during initialization (i.e. from registerAllEditorWindows) then return
                    as the initialization code will call us directly */
IF ENTRY(1, PROGRAM-NAME(5), " ") = "registerAllEditorWindows" THEN RETURN.

LOAD 'Software\PSC\PROGRESS\slacroixTools' BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
/* create it if it does not exist  ... well, would be surprising...*/
IF ERROR-STATUS:ERROR THEN LOAD 'Software\PSC\PROGRESS\slacroixTools' NEW BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN USE 'Software\PSC\PROGRESS\slacroixTools'.
cSection = 'ABHacker'.
/* GET-KEY-VALUE SECTION cSection KEY 'openedFiles' VALUE cOpenedFiles. */
FOR EACH bbttEdt:
    /* 24-OCT-2013 jcc: delete "closed" section editor (in fact it is only hidden) */
    IF VALID-HANDLE(bbttEdt.hEditor) AND bbttEdt.hEditor:TYPE = "EDITOR"
     AND bbttEdt.hEditor:SOURCE-EDITOR
     AND NOT bbttEdt.hWin:VISIBLE  /* "closed" section editor */
    THEN DO:
        DELETE bbttEdt.
        {&OPEN-QUERY-bedtBrowse}
        NEXT.
    END.

    IF bbttEdt.cFullPathName > "" AND LOOKUP(bbttEdt.cFullPathName, cOpenedFiles) = 0
      AND VALID-HANDLE(bbttEdt.hWin) THEN DO:
        cOpenedFiles = cOpenedFiles + "," + bbttEdt.cFullPathName.
        cOpenedFilesGeometry = cOpenedFilesGeometry
            + "," + STRING(bbttEdt.hWin:X)
            + "|" + STRING(bbttEdt.hWin:Y)
            + "|" + STRING(bbttEdt.hWin:WIDTH-PIXELS)
            + "|" + STRING(bbttEdt.hWin:HEIGHT-PIXELS).
        cOpenedFilesPosition = cOpenedFilesPosition + "," + STRING(bbttEdt.hEditor:CURSOR-LINE) + "|" + STRING(bbttEdt.hEditor:CURSOR-CHAR).
        /* store section for structured editors */
        IF bbttEdt.lManageableSections THEN DO:
            ASSIGN
             cCurrentSection = bbttEdt.hSectionCombo:SCREEN-VALUE
             cCurrentEvent   = bbttEdt.hEventCombo:SCREEN-VALUE
             cCurrentWidget  = bbttEdt.hWidgetName:SCREEN-VALUE.
            cOpenedFilesSection = cOpenedFilesSection + "," + cCurrentSection + "|" + cCurrentEvent + "|" + cCurrentWidget.
        END.
        ELSE
            cOpenedFilesSection = cOpenedFilesSection + ",".
    END.
END.
cOpenedFiles         = SUBSTRING(cOpenedFiles, 2).
cOpenedFilesGeometry = SUBSTRING(cOpenedFilesGeometry, 2).
cOpenedFilesPosition = SUBSTRING(cOpenedFilesPosition, 2).
cOpenedFilesSection  = SUBSTRING(cOpenedFilesSection, 2).
PUT-KEY-VALUE SECTION cSection KEY 'openedFiles'         VALUE cOpenedFiles.
PUT-KEY-VALUE SECTION cSection KEY 'openedFilesGeometry' VALUE cOpenedFilesGeometry.
PUT-KEY-VALUE SECTION cSection KEY 'openedFilesPosition' VALUE cOpenedFilesPosition.
PUT-KEY-VALUE SECTION cSection KEY 'openedFilesSection'  VALUE cOpenedFilesSection.
/* unload the environment to go back to default env */
UNLOAD 'Software\PSC\PROGRESS\slacroixTools' NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE replaceBufferDirective C-Win
PROCEDURE replaceBufferDirective :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor     AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcBufferName AS CHARACTER   NO-UNDO.

IF NOT VALID-HANDLE(phEditor)
 OR phEditor:TYPE <> "EDITOR" THEN RETURN.

/* 07-JUN-2007 sla: split the  %\w" + "Buffer" expression because it got replaced
  by abhack the last time I did something in this procedure ;) (indeed, I use ABHack to develop ABHack ;) */
phEditor:REPLACE("%\w" + "Buffer", pcBufferName, 9). /* FIND-NEXT-OCCURRENCE  + FIND-GLOBAL  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repositionAppBuilder C-Win
PROCEDURE repositionAppBuilder :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

IF VALID-HANDLE(ghMainAppbuilderWindow) THEN DO:

    APPLY "VALUE-CHANGED" TO gcFancyInAbWin IN FRAME fMisc.

    /* 21-OCT-2013 jcc: reposition AppBuilder */
    DEFINE VARIABLE iABHackWinRightEdge AS INTEGER   NO-UNDO.

    iABHackWinRightEdge = {&WINDOW-NAME}:X + {&WINDOW-NAME}:WIDTH-PIXELS.

    /* abhack on left side */
    IF iABHackWinRightEdge < SESSION:WIDTH-PIXELS / 2 THEN DO:
/*         MESSAGE ghMainAppBuilderWindow:X "<" iABHackWinRightEdge "?"                                            */
/*             SKIP "=>" MIN(iABHackWinRightEdge + 7 , SESSION:WIDTH-PIXELS - ghMainAppBuilderWindow:WIDTH-PIXELS) */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                  */
        IF ghMainAppBuilderWindow:X < iABHackWinRightEdge
        THEN ghMainAppBuilderWindow:X = MIN(iABHackWinRightEdge + 7 , SESSION:WIDTH-PIXELS - ghMainAppBuilderWindow:WIDTH-PIXELS). /* don't shift it too far on the right */
    END.
    /* abhack on right side */
    ELSE DO:
        IF ghMainAppBuilderWindow:X + ghMainAppBuilderWindow:WIDTH-PIXELS > {&WINDOW-NAME}:X
         THEN ghMainAppBuilderWindow:X = MAX(1, {&WINDOW-NAME}:X - ghMainAppBuilderWindow:WIDTH-PIXELS - 7). /* don't shift it too far on the left */
    END.
    /* 21-OCT-2013 jcc: end */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE requestExtProcParam C-Win
PROCEDURE requestExtProcParam :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pcExtProcName AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcParams     AS CHARACTER   NO-UNDO.

    RUN GetExtProcParam IN ghParser (pcExtProcName, OUTPUT opcParams) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN cmon:INSERT-STRING(STRING(TIME, "hh:mm:ss")
      + ": GetExtProcParam returned this error:" + QUOTER(RETURN-VALUE))
      IN FRAME fMain.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restartABHackSpyingTimer C-Win
PROCEDURE restartABHackSpyingTimer :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

cMon:SCREEN-VALUE IN FRAME fMain = STRING(TIME, "hh:mm:ss") + " Restarting Spying timer".

chCtrlSpy:PSTimerSpy:ENABLED = YES.

btnStopStart:LABEL = "&Stop".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restoreOpenedFiles C-Win
PROCEDURE restoreOpenedFiles :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:       23-OCT-2013 jcc: open last session's files
------------------------------------------------------------------------------*/
DEFINE VARIABLE cGeometry            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOpenedFiles         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOpenedFilesGeometry AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOpenedFilesPosition AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOpenedFilesSection  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSection             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSectionData         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hABHchildWin         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hWin                 AS HANDLE      NO-UNDO.
DEFINE VARIABLE i                    AS INTEGER     NO-UNDO.
DEFINE VARIABLE lOpened              AS LOGICAL     NO-UNDO.

DEFINE BUFFER bbttEdt FOR ttEdt.


IF NOT glSaveSession THEN RETURN.

chCtrlSpy:ENABLED = NO.
LOAD 'Software\PSC\PROGRESS\slacroixTools' BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
/* create it if it does not exist  ... well, would be surprising...*/
IF ERROR-STATUS:ERROR THEN LOAD 'Software\PSC\PROGRESS\slacroixTools' NEW BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN USE 'Software\PSC\PROGRESS\slacroixTools'.
cSection = 'ABHacker'.
GET-KEY-VALUE SECTION cSection KEY 'openedFiles'         VALUE cOpenedFiles.
GET-KEY-VALUE SECTION cSection KEY 'openedFilesGeometry' VALUE cOpenedFilesGeometry.
GET-KEY-VALUE SECTION cSection KEY 'openedFilesPosition' VALUE cOpenedFilesPosition.
GET-KEY-VALUE SECTION cSection KEY 'openedFilesSection'  VALUE cOpenedFilesSection.
/* unload the environment to go back to default env */
UNLOAD 'Software\PSC\PROGRESS\slacroixTools' NO-ERROR.
DO i = 1 TO NUM-ENTRIES(cOpenedFiles):
    IF NOT CAN-FIND(bbttEdt WHERE bbttEdt.cFullPathName = ENTRY(i, cOpenedFiles)
      AND VALID-HANDLE(bbttEdt.hWin)) THEN DO:
        RUN openFile (ENTRY(i, cOpenedFiles)).
        lOpened = YES.
      END.
END.
IF lOpened THEN DO:
    /* open section editors for .w files */
    hWin = SESSION:FIRST-CHILD.
    DO WHILE hWin <> ?:
        IF hwin:TITLE = "AppBuilder" THEN DO:
            hABHchildWin = hWin:FIRST-CHILD.
            DO WHILE hABHchildWin <> ?:
                IF hABHchildWin:TITLE MATCHES "*~~~.w" THEN DO:
                    APPLY "ENTRY" TO hABHchildWin.
                    APPLY "CTRL-S" TO hABHchildWin.
                END.
                hABHchildWin = hABHchildWin:NEXT-SIBLING.
            END. /* DO WHILE hABHchildWin <> ? */
        END. /* manage AppBuilder children windows */

        hWin = hWin:NEXT-SIBLING.
    END. /* DO WHILE hWin <> ?: */
    /* register all newly opened editors */
    RUN registerAllEditorWindows.
END.
IF cOpenedFilesGeometry > "" THEN FOR EACH bbttEdt:
    i = LOOKUP(bbttEdt.cFullPathName, cOpenedFiles).
    IF i = 0 THEN NEXT.
    hWin = bbttEdt.hWin.
    /* reposition structured editors */
    cSectionData = ENTRY(i, cOpenedFilesSection) NO-ERROR.
    IF bbttEdt.lManageableSections AND NUM-ENTRIES(cSectionData, "|") = 3 THEN
        RUN JumpToSection (BUFFER bbttEdt, ENTRY(1, cSectionData, "|"), ENTRY(3, cSectionData, "|"), ENTRY(2, cSectionData, "|")).
    /* restore geometry */
    cGeometry = ENTRY(i, cOpenedFilesGeometry) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN DO:
        hWin:X             = INTEGER(ENTRY(1, cGeometry, "|")).
        hWin:Y             = INTEGER(ENTRY(2, cGeometry, "|")).
        hWin:WIDTH-PIXELS  = INTEGER(ENTRY(3, cGeometry, "|")).
        hWin:HEIGHT-PIXELS = INTEGER(ENTRY(4, cGeometry, "|")).
        APPLY "WINDOW-RESIZED" TO hWin.
    END.
    bbttEdt.hEditor:CURSOR-LINE = INTEGER(ENTRY(1, ENTRY(i, cOpenedFilesPosition), "|")) NO-ERROR.
    bbttEdt.hEditor:CURSOR-CHAR = INTEGER(ENTRY(2, ENTRY(i, cOpenedFilesPosition), "|")) NO-ERROR.
END.
RUN rememberOpenedFiles.
chCtrlSpy:ENABLED = YES.
/* 23-OCT-2013 jcc: end */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restoreOrRecordSectionPostion C-Win
PROCEDURE restoreOrRecordSectionPostion :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ttedt FOR ttedt.
DEFINE INPUT  PARAMETER pcCurrentSection AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pcCurrentEvent   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pcCurrentWidget  AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttsection     FOR ttsection.
DEFINE BUFFER lastttsection FOR ttsection.
DEFINE BUFFER ttMark        FOR ttMark.

DEFINE VARIABLE iMruSequence AS INTEGER     NO-UNDO.

PUBLISH "KillEditorList" (ttedt.hEditor). /* 03-APR-2007 sla: get rid off running popup when we change the section */

FIND LAST lastttsection WHERE lastttsection.hEditor = ttedt.hEditor USE-INDEX heditorMruSeq NO-ERROR.
iMruSequence = IF AVAILABLE lastttsection THEN lastttsection.iMruSequence + 1 ELSE 1.

/* 25-FEB-2007 sla: Beware!! Because of the timer ticks, the screen-value is updated while
 we move the cursor over the comb items an d before the value changed is fired (and handled by the AppBuilder)
  => solution, ignore this dummy changes as long as the MODIFIED attribute of the 3 combo's are unset
  Once there are set, consider that the value changed has been caught and handled by the AppBuilder
  At this point we can take the values into account, and reset the MODIFIED attribute ;)
  By chance, it seems the AppBuilder does not use this attribute him self... */
IF   FOCUS = ttedt.hSectionCombo
 AND ttedt.hSectionCombo:MODIFIED = NO THEN RETURN. /* case reported above */

IF   FOCUS = ttedt.hEventCombo
 AND ttedt.hEventCombo:MODIFIED = NO THEN RETURN. /* case reported above */

IF   FOCUS = ttedt.hWidgetName
 AND ttedt.hWidgetName:MODIFIED = NO THEN RETURN. /* case reported above */

ttedt.hSectionCombo:MODIFIED = NO. /* reset now */
ttedt.hEventCombo:MODIFIED = NO. /* reset now */
ttedt.hWidgetName:MODIFIED = NO. /* reset now */

/* First update the ttedt buffer with current section info */
ttedt.currentSection = pcCurrentSection.
IF CAN-DO("Triggers,Procedures,Functions", pcCurrentSection)
 THEN ttedt.currentEvent = pcCurrentEvent.
IF pcCurrentSection = "Triggers" THEN ttedt.currentWidget = pcCurrentWidget.

/* 08-JUN-2007 sla: keep section layout in synch */
RUN StructuredFetchCurrentTtMark (BUFFER ttEdt, BUFFER ttMark).
PUBLISH "SectionSelectMark" (STRING(ROWID(ttMark))).


FIND ttsection WHERE
     ttsection.hEditor  = ttEdt.hEditor
 AND ttsection.cSection = pcCurrentSection
 AND (   CAN-DO("Definitions,Main Block", pcCurrentSection)
      OR ttsection.cEvent = pcCurrentEvent)
 AND (   pcCurrentSection <> "Triggers"
      OR ttsection.cWidget = pcCurrentWidget)
 NO-ERROR.

/* Section seen for the first time, just register the section and return
   Note we will record the row and col at next ocx.tick */
IF NOT AVAILABLE ttsection THEN DO:
    CREATE ttsection.
    ASSIGN
     ttsection.hEditor      = ttEdt.hEditor
     ttsection.cSection     = pcCurrentSection
     ttsection.iMruSequence = iMruSequence.

    IF CAN-DO("Triggers,Procedures,Functions", pcCurrentSection)
     THEN ttsection.cEvent = pcCurrentEvent.
    IF pcCurrentSection = "Triggers" THEN ttSection.cWidget = pcCurrentWidget.
    RETURN.
END.

/* at this point, we are back into a section that we have visited before */

/* 26-FEB-2007 sla: a trick to make the previous position appear in the
 middle of the viewport => first go to previous line + editor:HEIGHT-CHARS
 The go to the previous line*/
ttedt.hEditor:CURSOR-LINE = ttsection.cursorLine + ttedt.hEditor:HEIGHT-CHARS / 1.3 NO-ERROR.

/* then go to the previous position. */
ASSIGN
 ttedt.hEditor:CURSOR-LINE = ttsection.cursorLine
 ttEdt.hEditor:CURSOR-CHAR = ttsection.cursorChar
 NO-ERROR. /* do it kindly with a NO-ERROR, but normally no error should ever happen */

cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME, "hh:mm:ss") + " Restored Section Cursor position to line: "
 + QUOTER(ttsection.cursorLine) + " col:" + QUOTER(ttsection.cursorChar).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restoreSetting C-Win
PROCEDURE restoreSetting :
/*------------------------------------------------------------------------------
  Purpose:     This PROCEDURE is now too long to be edited with the Section Editor
           => use  Ctrl-Alt-E to edit in large mode instead
------------------------------------------------------------------------------*/
DEFINE VARIABLE ckv                           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cReleaseProgressVersion       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSection                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hRunningInfoWin               AS HANDLE      NO-UNDO.
DEFINE VARIABLE iMajorProgressVersion         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMinorProgressVersion         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSPLevelProgress              AS INTEGER     NO-UNDO.
DEFINE VARIABLE lSuggestBufferForNewGuysAvail AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lwasAnyRunningABHackWinInfo   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE chrCommandLine                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ptrToString                   AS MEMPTR      NO-UNDO.
DEFINE VARIABLE using-filterocxevents         AS LOGICAL     NO-UNDO.



&SCOPED-DEFINE gcSuggestBufferForNewGuysStampValue 25-FEB-2007

/* 07-DEC-2006 sla: retrieve DLC in default environment */
DEFINE VARIABLE cDLC       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVersionSP AS CHARACTER  NO-UNDO.
GET-KEY-VALUE SECTION "startup" KEY "DLC" VALUE cDLC.
IF SEARCH(cDLC + "/version.") <> ? THEN DO:
    INPUT FROM VALUE(cDLC + "/version.").
    IMPORT UNFORM cVersionSP.
    INPUT CLOSE.
    cVersionSP = ENTRY(3, cVersionSP, " ").
    ASSIGN
     iMajorProgressVersion   = INTEGER(ENTRY(1, cVersionSP, "."))
     iMinorProgressVersion   = INTEGER(SUBSTRING(ENTRY(2, cVersionSP, "."), 1, 1))
     cReleaseProgressVersion = SUBSTRING(ENTRY(2, cVersionSP, "."), 2, 1)
     NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'ABHack could not determine the version of Progress you are using' SKIP (2)
         "This could be annoying for the stability of ABHack"
         "ABHack relies on the version file located in the installation directory fo Progress"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    /* 23-JUL-2008 sla: now working out SP level in NO-ERROR don´t care mode so it does not complain with a special release like 10.2APT */
    iSPLevelProgress        = INTEGER(SUBSTRING(ENTRY(2, cVersionSP, "."), 3)) NO-ERROR. /* igonore error (keeping 0 will be fine) */
END.

LOAD 'Software\PSC\PROGRESS\slacroixTools' BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.

/* create it if it does not exist  ... well, would be surprising...*/
IF ERROR-STATUS:ERROR THEN LOAD 'Software\PSC\PROGRESS\slacroixTools' NEW BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN USE 'Software\PSC\PROGRESS\slacroixTools'.

cSection = 'ABHacker'.

GET-KEY-VALUE SECTION cSection KEY 'WinX' VALUE ckv.
IF ckv <> ? THEN {&WINDOW-NAME}:X = INT(ckv) NO-ERROR.
ELSE {&WINDOW-NAME}:X = 25 + SESSION:WIDTH-PIXELS - SESSION:WORK-AREA-WIDTH-PIXELS.   /* 21-OCT-2008 sla: now use my setting as default */

GET-KEY-VALUE SECTION cSection KEY 'WinY' VALUE ckv.
IF ckv <> ? THEN {&WINDOW-NAME}:Y = INT(ckv) NO-ERROR.
ELSE {&WINDOW-NAME}:Y = 0.  /* 21-OCT-2008 sla: now use my setting as default */

/* 21-OCT-2008 sla: this one used to be done much later */
GET-KEY-VALUE SECTION cSection KEY 'giWinHeight' VALUE ckv.
IF ckv <> ? THEN giWinHeight = INTEGER(ckv).
/* ELSE giWinHeight = {&WINDOW-NAME}:HEIGHT-PIXELS. */
ELSE giWinHeight = 264.  /* 21-OCT-2008 sla: now use my setting as default */



GET-KEY-VALUE SECTION cSection KEY 'glTopOnly' VALUE ckv.
IF ckv <> ? THEN glTopOnly = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glCompleteAttrOnTab' VALUE ckv.
IF ckv <> ? THEN glCompleteAttrOnTab = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glCompleteOnTab' VALUE ckv.
IF ckv <> ? THEN glCompleteOnTab = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glAutoComplete' VALUE ckv.
IF ckv <> ? THEN glAutoComplete = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'giAutoCompMinSize' VALUE ckv.
IF ckv <> ? THEN giAutoCompMinSize = INTEGER(ckv).  /* 14-DEC-2006 sla: was doing = INTEGER(giAutoCompMinSize) by mistake... */

GET-KEY-VALUE SECTION cSection KEY 'glCustomCompletionOnTab' VALUE ckv.
IF ckv <> ? THEN glCustomCompletionOnTab = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'gcComplDateFormat' VALUE ckv.
IF ckv <> ? THEN gcComplDateFormat = ckv.

GET-KEY-VALUE SECTION cSection KEY 'glVariableCompletion' VALUE ckv.
IF ckv <> ? THEN glVariableCompletion = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glCompleteTableOnFLA' VALUE ckv.
IF ckv <> ? THEN glCompleteTableOnFLA = ckv = "YES".
/* 16-MAY-2007 sla: at very first launch (no profile info stored in registry yet)
 Changed the initial value of this toggle-box to NO
 Default to YES (with the following code) only if we are connected to a icfdb database (dynamics) */
ELSE /* was ? (no profiled info stored yet in the registry) */ glCompleteTableOnFLA = CONNECTED("icfdb").


GET-KEY-VALUE SECTION cSection KEY 'glAddNoLock' VALUE ckv.
IF ckv <> ? THEN glAddNoLock = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glAddWhereAfterBuffer' VALUE ckv.
IF ckv <> ? THEN glAddWhereAfterBuffer = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glCompleteField' VALUE ckv.
IF ckv <> ? THEN glCompleteField = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'gcFieldSortedBy' VALUE ckv.
IF ckv <> ? THEN gcFieldSortedBy = ckv.

GET-KEY-VALUE SECTION cSection KEY 'glPlusplus' VALUE ckv.
IF ckv <> ? THEN glPlusplus = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glImprovedCtrlKeftRight' VALUE ckv.
IF ckv <> ? THEN glImprovedCtrlKeftRight = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glcommentOnDblSlash' VALUE ckv.
IF ckv <> ? THEN glcommentOnDblSlash = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glAlignEnabled' VALUE ckv.
IF ckv <> ? THEN glAlignEnabled = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glAlignVars' VALUE ckv.
IF ckv <> ? THEN glAlignVars = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glAlignTTFields' VALUE ckv.
IF ckv <> ? THEN glAlignTTFields = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glAlignTTFieldsSort' VALUE ckv.
IF ckv <> ? THEN glAlignTTFieldsSort = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glAlignAssign' VALUE ckv.
IF ckv <> ? THEN glAlignAssign = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'gltooltipOnOpenParenthese' VALUE ckv.
IF ckv <> ? THEN gltooltipOnOpenParenthese = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glAnalyseQuery' VALUE ckv.
IF ckv <> ? THEN glAnalyseQuery = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glAutoStopWhenNonEditorWin' VALUE ckv.
IF ckv <> ? THEN glAutoStopWhenNonEditorWin = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glResizableDictionary' VALUE ckv.
IF ckv <> ? THEN glResizableDictionary = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'giTabNumSpaces' VALUE ckv.
IF ckv <> ? THEN giTabNumSpaces = INTEGER(ckv).

GET-KEY-VALUE SECTION cSection KEY 'glAddEndAfterDo' VALUE ckv.
IF ckv <> ? THEN glAddEndAfterDo = ckv = "YES".

/* 07-DEC-2006 sla: make this one version dependent for people that have multiple version of Progess */
GET-KEY-VALUE SECTION cSection KEY 'glForceWaitFor' + cVersionSP VALUE ckv.
IF ckv <> ? THEN glForceWaitFor = ckv = "YES".

/* 10-SEP-2007 sla: make this one version dependent as well */
GET-KEY-VALUE SECTION cSection KEY 'glAutoStopDialog' + cVersionSP VALUE ckv.
IF ckv <> ? THEN glAutoStopDialog = ckv = "YES".


GET-KEY-VALUE SECTION cSection KEY 'glIntProcCompletion' VALUE ckv.
IF ckv <> ? THEN glIntProcCompletion = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glIntFuncCompletion' VALUE ckv.
IF ckv <> ? THEN glIntFuncCompletion = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glIntProcInputInSig' VALUE ckv.
IF ckv <> ? THEN glIntProcInputInSig = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glCtrlBackspacelDelPrevWord' VALUE ckv.
IF ckv <> ? THEN glCtrlBackspacelDelPrevWord = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glCtrlDelDelsWord' VALUE ckv.
IF ckv <> ? THEN glCtrlDelDelsWord = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'giQueryTooltipTimeSuccess' VALUE ckv.
IF ckv <> ? THEN giQueryTooltipTimeSuccess = INTEGER(ckv).

GET-KEY-VALUE SECTION cSection KEY 'giQueryTooltipTimeFail' VALUE ckv.
IF ckv <> ? THEN giQueryTooltipTimeFail = INTEGER(ckv).

GET-KEY-VALUE SECTION cSection KEY 'glOpenPW' VALUE ckv.
IF ckv <> ? THEN glOpenPW = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glCompileMinsize' VALUE ckv.
IF ckv <> ? THEN glCompileMinsize = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glCompile' VALUE ckv.
IF ckv <> ? THEN glCompile = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glListingOnCtrlAltT' VALUE ckv.
IF ckv <> ? THEN glListingOnCtrlAltT = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glDebugListingOnCtrlAltD' VALUE ckv.
IF ckv <> ? THEN glDebugListingOnCtrlAltD = ckv = "YES".


GET-KEY-VALUE SECTION cSection KEY 'glLowerCaseCompletion' VALUE ckv.
IF ckv <> ? THEN glLowerCaseCompletion = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glFilterOnViewAsWidgetType' VALUE ckv.
IF ckv <> ? THEN glFilterOnViewAsWidgetType = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glFilterGuessObjectType' VALUE ckv.
IF ckv <> ? THEN glFilterGuessObjectType = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glInsertClosingGuys' VALUE ckv.
IF ckv <> ? THEN glInsertClosingGuys = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glExtProcCompletion' VALUE ckv.
IF ckv <> ? THEN glExtProcCompletion = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'giMaxFilesToLoad' VALUE ckv.
IF ckv <> ? THEN giMaxFilesToLoad = INTEGER(ckv).

GET-KEY-VALUE SECTION cSection KEY 'giMaxEtimeToLoadFiles' VALUE ckv.
IF ckv <> ? THEN giMaxEtimeToLoadFiles = INTEGER(ckv).

GET-KEY-VALUE SECTION cSection KEY 'gcExtProcIgnoreDir' VALUE ckv.
IF ckv <> ? THEN gcExtProcIgnoreDir = ckv.
/* 11-JAN-2007 sla: Removed '%DLC%/gui'from gcExtProcIgnoreDir as this directory
 contains many useful includes like launch.i and dynlaunch.i (a little cheet from PSC for performance sake).
 ABHack will remove this directory by program if it find an old value in the registry
 That being said, I encourage the usage of %DLC%/gui/ade* to remove the numerous
 ade directories, which a developer might not be interested in most of the time */
DEFINE VARIABLE iEntryToReplace AS INTEGER     NO-UNDO.
iEntryToReplace = LOOKUP("%DLC%/gui", gcExtProcIgnoreDir).
IF iEntryToReplace > 0 THEN ENTRY(iEntryToReplace, gcExtProcIgnoreDir) = "%DLC%/gui/ade*".

GET-KEY-VALUE SECTION cSection KEY 'glCtrlF3NewProcWin' VALUE ckv.
IF ckv <> ? THEN glCtrlF3NewProcWin = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glProposeOpenSelectedFile' VALUE ckv.
IF ckv <> ? THEN glProposeOpenSelectedFile = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'gcFancyInABWin' VALUE ckv.
IF ckv <> ? THEN gcFancyInABWin = ckv.

GET-KEY-VALUE SECTION cSection KEY 'glHasSheSeenFrenchRomance' VALUE ckv.
IF ckv <> ? THEN glHasSheSeenFrenchRomance = ckv = "YES".
/* see code in saveSetting ... ;) */
IF NOT glHasSheSeenFrenchRomance THEN gcFancyInABWin = "viveLaFrance".

GET-KEY-VALUE SECTION cSection KEY 'glcommentSelectionOnSlash' VALUE ckv.
IF ckv <> ? THEN glcommentSelectionOnSlash = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glHideAllComments' VALUE ckv.
IF ckv <> ? THEN glHideAllComments = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glHideSelection' VALUE ckv.
IF ckv <> ? THEN glHideSelection = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glXrefAnal' VALUE ckv.
IF ckv <> ? THEN glXrefAnal = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glEnhancedCloseParenthese' VALUE ckv.
IF ckv <> ? THEN glEnhancedCloseParenthese = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glShowSpecialChars' VALUE ckv.
IF ckv <> ? THEN glShowSpecialChars = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glShowLineNumbers' VALUE ckv.
IF ckv <> ? THEN glShowLineNumbers = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'gcSuggestBufferFor' VALUE ckv.
IF ckv <> ? THEN gcSuggestBufferFor = ckv.

GET-KEY-VALUE SECTION cSection KEY 'gcSuggestBufferForNewGuysStamp' VALUE ckv.
IF ckv <> ? THEN gcSuggestBufferForNewGuysStamp = ckv.
IF gcSuggestBufferForNewGuysStamp <> "{&gcSuggestBufferForNewGuysStampValue}" THEN ASSIGN
 lSuggestBufferForNewGuysAvail = YES
 gcSuggestBufferForNewGuysStamp = "{&gcSuggestBufferForNewGuysStampValue}".


GET-KEY-VALUE SECTION cSection KEY 'glNoCompletionInStrings' VALUE ckv.
IF ckv <> ? THEN glNoCompletionInStrings = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glNoCompletionInComment' VALUE ckv.
IF ckv <> ? THEN glNoCompletionInComment = ckv = "YES".

/* 15-FEB-2007 sla: new guys for new abhackinfowin */
GET-KEY-VALUE SECTION cSection KEY 'glInfoWinTopOnly' VALUE ckv.
IF ckv <> ? THEN glInfoWinTopOnly = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'giInfoWinHeight' VALUE ckv.
IF ckv <> ? THEN giInfoWinHeight = INTEGER(ckv).
ELSE giInfoWinHeight = (SESSION:WORK-AREA-HEIGHT-PIXELS - giWinHeight) / 2 - 50.  /* 21-OCT-2008 sla: now use my setting as default */

GET-KEY-VALUE SECTION cSection KEY 'giInfoWinWidth' VALUE ckv.
IF ckv <> ? THEN giInfoWinWidth = INTEGER(ckv).
ELSE giInfoWinWidth = 225.  /* 21-OCT-2008 sla: now use my setting as default */

GET-KEY-VALUE SECTION cSection KEY 'giInfoWinX' VALUE ckv.
IF ckv <> ? THEN giInfoWinX = INTEGER(ckv).
ELSE giInfoWinX = SESSION:WIDTH-PIXELS - SESSION:WORK-AREA-WIDTH-PIXELS.  /* 21-OCT-2008 sla: now use my setting as default */

GET-KEY-VALUE SECTION cSection KEY 'giInfoWinY' VALUE ckv.
IF ckv <> ? THEN giInfoWinY = INTEGER(ckv).
ELSE giInfoWinY = SESSION:WORK-AREA-HEIGHT-PIXELS - giInfoWinHeight - 25.  /* 21-OCT-2008 sla: now use my setting as default */

GET-KEY-VALUE SECTION cSection KEY 'giInfoWinBgcolor' VALUE ckv.
IF ckv <> ? THEN giInfoWinBgcolor = INTEGER(ckv).

GET-KEY-VALUE SECTION cSection KEY 'giInfoWinFgcolor' VALUE ckv.
IF ckv <> ? THEN giInfoWinFgcolor = INTEGER(ckv).
/* 14-SEP-2007 sla: repair a black on black situation due to an initial value of 0 saved in the registry */
IF giInfoWinFgcolor = 0 AND giInfoWinBgcolor = 0 THEN giInfoWinFgcolor = 10.

GET-KEY-VALUE SECTION cSection KEY 'lisAnyRunningABHackWinInfo' VALUE ckv.
IF ckv <> ? THEN lwasAnyRunningABHackWinInfo = ckv = "YES".
ELSE lwasAnyRunningABHackWinInfo = YES. /* 21-OCT-2008 sla: now use my setting as default */

GET-KEY-VALUE SECTION cSection KEY 'glQADispInABHWin' VALUE ckv.
IF ckv <> ? THEN glQADispInABHWin = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glDisplaytextSelectedInfo' VALUE ckv.
IF ckv <> ? THEN glDisplaytextSelectedInfo = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glKeepTrackOfCursor' VALUE ckv.
glKeepTrackOfCursor = ckv = "YES" OR ckv = ?. /* 22-MAY-2008 sla: made sure initial default value is YES */

GET-KEY-VALUE SECTION cSection KEY 'glCompleteBlockLabels' VALUE ckv.
IF ckv <> ? THEN glCompleteBlockLabels = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glConvertDbTablesToLC' VALUE ckv.
IF ckv <> ? THEN glConvertDbTablesToLC = ckv = "YES".
/* yes, a hard coded cheat for the project were ABHack was born */
IF ckv = ? AND INDEX(PROPATH, "aladin") > 0 THEN glConvertDbTablesToLC = YES. /* default for our dear aladin project */

GET-KEY-VALUE SECTION cSection KEY 'glStopStartTimerOnCtrlAltO' VALUE ckv.
IF ckv <> ? THEN glStopStartTimerOnCtrlAltO = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glCopyUIBellowMouse' VALUE ckv.
IF ckv <> ? THEN glCopyUIBelowMouse = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glJumpMruSection' VALUE ckv.
IF ckv <> ? THEN glJumpMruSection = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glDrawBoxAroundCurrentLine' VALUE ckv.
IF ckv <> ? THEN
  glDrawBoxAroundCurrentLine = (ckv = "YES").
ELSE DO:
  /*  Johan Samyn
      Read value saved previously with key with typo in it.
      This else-block can then be removed in the next release. */
  GET-KEY-VALUE SECTION cSection KEY 'glDrawBoxAroudCurrentLine' VALUE ckv.
  IF ckv <> ? THEN glDrawBoxAroundCurrentLine = (ckv = "YES").
END.

GET-KEY-VALUE SECTION cSection KEY 'giRightMarginIndicatorCol' VALUE ckv.
IF ckv <> ? THEN giRightMarginIndicatorCol = INTEGER(ckv).

GET-KEY-VALUE SECTION cSection KEY 'gcDumpedResourceFileRoot' VALUE ckv.
IF ckv <> ? THEN gcDumpedResourceFileRoot = ckv.

GET-KEY-VALUE SECTION cSection KEY 'glKeepTrackListingBuffer' VALUE ckv.
/* IF ckv <> ? THEN glKeepTrackListingBuffer = ckv = "YES".
 16-SEP-2007 sla: trick to take change of initial value into account */
/* 21-OCT-2008 sla: this IF THEN was the reason why I was not getting set on very first time
   IF ckv <> ? THEN    */
glKeepTrackListingBuffer = ckv <> "NoReallyAskedExplicitly".

GET-KEY-VALUE SECTION cSection KEY 'giSplitParamInCalls' VALUE ckv.
IF ckv <> ? THEN giSplitParamInCalls = INTEGER(ckv).

GET-KEY-VALUE SECTION cSection KEY 'gcSplitParamCommaStyle' VALUE ckv.
IF ckv <> ? THEN gcSplitParamCommaStyle = ckv.

GET-KEY-VALUE SECTION cSection KEY 'gcGlobalResCat' VALUE ckv.
IF ckv <> ? THEN gcGlobalResCat = ckv.

/* 10-SEP-2007 sla: now use a versioned value */
GET-KEY-VALUE SECTION cSection KEY 'gcCatDbConnection' + cVersionSP VALUE ckv.
IF ckv <> ? THEN gcCatDbConnection = ckv.
ELSE DO:
    GET-KEY-VALUE SECTION cSection KEY 'gcCatDbConnection' VALUE ckv.
    IF ckv <> ? THEN gcCatDbConnection = ckv.
END.

GET-KEY-VALUE SECTION cSection KEY 'glClassNewCastAfterEqual' VALUE ckv.
IF ckv <> ? THEN glClassNewCastAfterEqual = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glRunInLibComp' VALUE ckv.
IF ckv <> ? THEN glRunInLibComp = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glFindOSFilesWithAbhackDB' VALUE ckv.
IF ckv <> ? THEN glFindOSFilesWithAbhackDB = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glMoveNewWindowsAway' VALUE ckv.
IF ckv <> ? THEN glMoveNewWindowsAway = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'giMaxEmptyLinesBeforeShrinking' VALUE ckv.
IF ckv <> ? THEN giMaxEmptyLinesBeforeShrinking = INTEGER(ckv).

GET-KEY-VALUE SECTION cSection KEY 'glAdaptWindowHeightOnCtrlW' VALUE ckv.
IF ckv <> ? THEN glAdaptWindowHeightOnCtrlW = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glCompleteOOSuperMethod' VALUE ckv.
IF ckv <> ? THEN glCompleteOOSuperMethod = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glHypeStrikeMode' VALUE ckv.
IF ckv <> ? THEN glHypeStrikeMode = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glPreprocessors' VALUE ckv.
glPreprocessors = ckv = "YES" OR ckv = ?. /* 22-MAY-2008 sla: made sure default initial value is YES */

GET-KEY-VALUE SECTION cSection KEY 'giNameComboFont' VALUE ckv.
IF ckv <> ? THEN giNameComboFont = INTEGER(ckv).

GET-KEY-VALUE SECTION cSection KEY 'glFindNextSelected' VALUE ckv.
IF ckv <> ? THEN glFindNextSelected = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glUseSchemaCache' VALUE ckv.
IF ckv <> ? THEN glUseSchemaCache = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'gcActionOnCtrlS' VALUE ckv.
IF ckv <> ? THEN gcActionOnCtrlS = ckv.

GET-KEY-VALUE SECTION cSection KEY 'btnExpand' VALUE ckv.
IF ckv <> ? THEN btnExpand:LABEL IN FRAME {&FRAME-NAME} = ckv.
ELSE btnExpand:LABEL = ">>>".  /* 21-OCT-2008 sla: now use my setting as default */

GET-KEY-VALUE SECTION cSection KEY 'gcSectionOutlineMode' VALUE ckv.
IF ckv <> ? THEN gcSectionOutlineMode = ckv.

GET-KEY-VALUE SECTION cSection KEY 'glSectionLayoutIgnoreStructProcs' VALUE ckv.
IF ckv <> ? THEN glSectionLayoutIgnoreStructProcs = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'glAbhackFind' VALUE ckv.
IF ckv <> ? THEN glAbhackFind = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'gcAbhackFindScope' VALUE ckv.
IF ckv <> ? THEN gcAbhackFindScope = ckv.

GET-KEY-VALUE SECTION cSection KEY 'gcAbhackFindMru' VALUE ckv.
IF ckv <> ? THEN gcAbhackFindMru = ckv.

/* 15-FEB-2007 sla: new guys for new Section Layout Window */
GET-KEY-VALUE SECTION cSection KEY 'glSectionLayoutWinTopOnly' VALUE ckv.
IF ckv <> ? THEN glSectionLayoutWinTopOnly = ckv = "YES".

GET-KEY-VALUE SECTION cSection KEY 'giSectionLayoutWinHeight' VALUE ckv.
IF ckv <> ? THEN giSectionLayoutWinHeight = INTEGER(ckv).
ELSE giSectionLayoutWinHeight = (SESSION:WORK-AREA-HEIGHT-PIXELS - giWinHeight) / 2 - 25.  /* 21-OCT-2008 sla: now use my setting as default */

GET-KEY-VALUE SECTION cSection KEY 'giSectionLayoutWinWidth' VALUE ckv.
IF ckv <> ? THEN giSectionLayoutWinWidth = INTEGER(ckv).
ELSE giSectionLayoutWinWidth = 225.  /* 21-OCT-2008 sla: now use my setting as default */

GET-KEY-VALUE SECTION cSection KEY 'giSectionLayoutWinX' VALUE ckv.
IF ckv <> ? THEN giSectionLayoutWinX = INTEGER(ckv).
ELSE giSectionLayoutWinX = SESSION:WIDTH-PIXELS - SESSION:WORK-AREA-WIDTH-PIXELS.  /* 21-OCT-2008 sla: now use my setting as default */

GET-KEY-VALUE SECTION cSection KEY 'giSectionLayoutWinY' VALUE ckv.
IF ckv <> ? THEN giSectionLayoutWinY = INTEGER(ckv).
ELSE giSectionLayoutWinY = giWinHeight + 1 + 25.  /* 21-OCT-2008 sla: now use my setting as default */

/* 25-OCT-2013 jcc: new setting to save/restore session */
GET-KEY-VALUE SECTION cSection KEY 'glSaveSession' VALUE ckv.
IF ckv <> ? THEN glSaveSession = ckv = "YES".

/* unload the environment to go back to default env */
UNLOAD 'Software\PSC\PROGRESS\slacroixTools' NO-ERROR.

PUBLISH "isAnyRunningABHackWinInfo" (OUTPUT hRunningInfoWin).
IF VALID-HANDLE(hRunningInfoWin) THEN RUN SubScribeEvents IN hRunningInfoWin.
ELSE IF lwasAnyRunningABHackWinInfo THEN APPLY 'CHOOSE' TO btnLaunchInfoFloatingWin IN FRAME {&FRAME-NAME}.

IF lSuggestBufferForNewGuysAvail THEN DO:
    IF gcSuggestBufferFor <> "" THEN /* 05-MAR-2007 sla: no message if running ABHack for the very first time */
     MESSAGE "A few new items are now available in the 'Suggest used buffers for these guys' feature (added on {&gcSuggestBufferForNewGuysStampValue})" SKIP
     "Until now, your selection was limited to the following selected:" gcSuggestBufferFor SKIP(2)
     "I am going to selected the entire list for now so you can test the new additions at least once.  The new selection is now:" SKIP
     gcSuggestBufferFor:LIST-ITEMS IN FRAME fComplSuggestUsedBuffers SKIP (2)
     "Feel free to refine the selection later on (this message will not come until new guys are added again"
      VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Please read".

    gcSuggestBufferFor:SCREEN-VALUE = gcSuggestBufferFor:LIST-ITEMS.
    gcSuggestBufferFor = gcSuggestBufferFor:LIST-ITEMS.
END.



/* actually, ABHack should not even compile before 9.1D */
IF iMajorProgressVersion < 10
 AND iMinorProgressVersion < 2
 AND cReleaseProgressVersion < "D"
 THEN MESSAGE "Beware, you are running an old version of Progress (" + cVersionSP
       + ") that has never been tested with ABHack"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
/* 9.1Dxx or 9.1E00 of 9.1E01 */
ELSE IF iMajorProgressVersion < 10
    AND iMinorProgressVersion < 2
    AND (    cReleaseProgressVersion < "E"
         OR     cReleaseProgressVersion = "E"
            AND iSPLevelProgress < 2)
  THEN DO:
    /* 06-SEP-2007 pjv: GET the command line that started the session and find out if
                        we are using the startup PARAMETER -filterocxevents           */
    RUN GetCommandLineA (OUTPUT ptrToString).
    chrCommandLine = GET-STRING(ptrToString,1).
    using-filterocxevents = INDEX(chrCommandLine," -filterocxevents") > 0.

    IF NOT using-filterocxevents  THEN DO:    /* 06-SEP-2007 pjv: let old versions who have -filterocxevents in the command line pass */
        IF glForceWaitFor AND glAutoStopDialog THEN DO:
            MESSAGE "You are using a rather old version of Progress (" + cVersionSP + ") with the satiblity options 'Force WAIT-FOR' and/or 'Stop when focus is in Dialog'"
             "but we found out a much better solution: use the -filterocxevents Startup Parameter" SKIP
             "ABHack now checks this parameter is used in the command line."
             " If you are already using this parameter in a pf file, then please put it in the command line of your shortcut/icon as well (ABHack does not parse your pf files)" SKIP(3)
             "Notes:" SKIP
             "  -ABHack works without any problem in version 9 since 9.1E02 (Service Pack 2 on top of 9.1E) and OE 10.0A+" SKIP
             "  -The force wait-for option can cause minor errors with a few AppBuilder facilities such as the MRU files, or not being able to launch standard tools from the 'Tool' menu, which is why it is much better to use -filterocxevents"
             VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE "PLEASE READ  (update on 10-SEP-2007)".
        END. ELSE DO:
            ASSIGN
             glForceWaitFor   = YES
             glAutoStopDialog = YES.
            MESSAGE "You are using a rather old version of Progress (" + cVersionSP + ") that can cause rather severe stability issues (like a hanging sessions)"
             "when a message view-as alert-box comes to the screen while a timer tries to spy a source code editor" SKIP(2)
             "ABHack has a work around option that consits in forcing a WAIT-FOR in its main block and stopping spying timer when the focus is coming into a dialog-box" SKIP
             "These options were not both switched on, so I just enabled them all" SKIP(2)
             "Update on 10-SEP-2007: A MUCH BETTER WORK AROUND IS TO USE THE STARTUP PARAMETER -filterocxevents.  ABHack now checks this parameter is used in the command line that started Progress"
             "  => If yes, then these options are disabled." SKIP
             " If you are already using this parameter in a pf file, then please put it in the command line of your shortcut as well." SKIP(2)
             "Notes:" SKIP
             "  -ABHack works without any problem in version 9 since 9.1E02 (Service Pack 2 on top of 9.1E) and OE 10.0A+" SKIP
             "  -The force wait-for option can cause minor errors with a few AppBuilder facilities such as the MRU files, or not being able to launch standard tools from the 'Tool' menu"
             "These errors are really minor and can be ignored safely if you really can't change your command line (they should not prevent you from using ABHack;)" SKIP
             "  -The minor errors mentioned above seem to be less annoying when ABHack and the AppBuilder are started this way:" SKIP
             "    1) Start Progress Desktop" SKIP
             "    2) From desktop: Tools -> Protools -> ABHack" SKIP
             "    3) Start the AppBuilder from the desktop" SKIP
             "    4) and again: have 'Force wait-for' and 'stop spying when focus in dialog' checked in the ABHack preferences"
                VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE "PLEASE READ  (update on 10-SEP-2007)" .
        END.
    END. /* IF NOT using-filterocxevents  */

    IF (   glForceWaitFor
        OR glAutoStopDialog)
     AND using-filterocxevents      /* 06-SEP-2007 pjv: let old versions who have -filterocxevents in the command line pass */
     THEN DO:
        ASSIGN
         glForceWaitFor   = NO
         glAutoStopDialog = NO.
        MESSAGE "You are using a rather old version of Progress (" + cVersionSP + ") with the satiblity options 'Force WAIT-FOR' and/or 'Stop when focus is in Dialog'"
         "but you are using the -filterocxevents Startup Parameter, so you actually do not need them" SKIP
         "ABHack is therefore going to disable these options, which can cause a few other bad side effects"
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "PLEASE READ  (update on 10-SEP-2007)".
     END.
END.

/* 9.1E02 and above    Note I do not know if tere are issue in OE 10.0A and OE 10.0B */
ELSE DO:
    IF  glForceWaitFor OR glAutoStopDialog THEN DO:
        ASSIGN
         glForceWaitFor   = NO
         glAutoStopDialog = NO.
        MESSAGE "PLEASE READ  (update on 10-SEP-2007)"  SKIP
         "You are currently using the satiblity options 'Force WAIT-FOR' and/or 'Stop when focus is in Dialog', which is/are not"
         "necessary in the version of Progress you are using (" + cVersionSP + ")" SKIP
         "These options can cause a few bad side effects, therefore ABHack is going to disable them"
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "PLEASE READ  (update on 10-SEP-2007)".
     END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveFile C-Win
PROCEDURE saveFile :
/*------------------------------------------------------------------------------
  Purpose:    Called on Ctrl-S and F6
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor  AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER pcFiredBy AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hEditorMenuBar AS HANDLE  NO-UNDO.
DEFINE VARIABLE hEditorWin     AS HANDLE  NO-UNDO.
DEFINE VARIABLE hSaveFileMI    AS HANDLE  NO-UNDO.
DEFINE VARIABLE lStarted       AS LOGICAL    NO-UNDO.

DEFINE BUFFER ttEdt      FOR ttEdt.
DEFINE BUFFER otherttEdt FOR ttEdt.
DEFINE VARIABLE lConfirmSave AS LOGICAL     NO-UNDO.

/* 14-MAR-2007 sla: kill a runnnig instance of popup list to avoid bad errors coming from the ADE when the file is being saved*/
PUBLISH "KillEditorList" FROM THIS-PROCEDURE (phEditor).


FIND FIRST otherttEdt WHERE otherttEdt.cFullPathName = bttEdt.cFullPathName
                        AND otherttEdt.dLastSaveOpenTime < bttEdt.dLastSaveOpenTime
                        /* 28-OCT-2007 sla: adding these conditions to ignore an editor that has been closed but is still present in the browse */
                        AND VALID-HANDLE(otherttEdt.hWin)
                        AND VALID-HANDLE(otherttEdt.hEditor)
                        AND otherttEdt.hEditor:TYPE = "editor"
                        NO-ERROR.
IF AVAILABLE otherttEdt THEN DO:
    MESSAGE "It seems you have another editor open for same source " + QUOTER(otherttEdt.cFullPathName)
          + " that was open earlier (" + QUOTER(otherttEdt.cLastSaveOpenTime) SKIP
            "Do you really want to save the current source file ?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lConfirmSave.
    IF NOT lConfirmSave THEN RETURN.
END.


cMon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".


/* if the editor window has a save menu-item, then choose it */
hEditorWin = phEditor:WINDOW.
IF NOT VALID-HANDLE(hEditorWin) THEN RETURN.
hEditorMenuBar = hEditorWin:MENU-BAR.
IF NOT VALID-HANDLE(hEditorMenuBar) THEN RETURN.
hSaveFileMI = hEditorMenuBar:FIRST-CHILD. /* file sub-menu */
IF VALID-HANDLE(hSaveFileMI) THEN hSaveFileMI = hSaveFileMI:FIRST-CHILD.
DO WHILE hSaveFileMI <> ? WITH FRAME {&FRAME-NAME}:
    IF REPLACE(hSaveFileMI:LABEL, "&", "") = "save" THEN DO:
        cmon:SCREEN-VALUE = STRING(TIME, "hh:mm:ss") + " save file " + hEditorWin:TITLE.
        APPLY 'choose' TO hSaveFileMI.
        FIND ttEdt WHERE ttEdt.hEditor = phEditor NO-ERROR.
        ttEdt.cWinTitle = ttEdt.hWin:TITLE. /* update it now so the Spying timer does not believe we deal with another source code IN this editor window */
        cmon:SCREEN-VALUE = STRING(TIME, "hh:mm:ss") + " save FILE done for " + ttEdt.cFullPathName.
        IF AVAILABLE ttEdt THEN RUN updateEdtFileName (BUFFER ttEdt).
        IF pcFiredBy = "Ctrl-s" AND gcActionOnCtrlS = "saveAndGlobalLoad" THEN RUN loadGlobalResources (phEditor).
        RETURN.
    END.
    hSaveFileMI = hSaveFileMI:NEXT-SIBLING.
END.

/* otherwise, we should be in a section editor of a structured .p or .w file
  => ask the AppBuilder to save the file */
IF NOT VALID-HANDLE(ghMainAppBuilderWindow) THEN RUN findAppBuilder.
IF NOT VALID-HANDLE(ghMainAppBuilderWindow) THEN RETURN.

hEditorMenuBar = ghMainAppBuilderWindow:MENU-BAR.
IF NOT VALID-HANDLE(hEditorMenuBar) THEN RETURN.
hSaveFileMI = hEditorMenuBar:FIRST-CHILD. /* file sub-menu */
IF VALID-HANDLE(hSaveFileMI) THEN hSaveFileMI = hSaveFileMI:FIRST-CHILD.
DO WHILE hSaveFileMI <> ?:
    IF REPLACE(hSaveFileMI:LABEL, "&", "") = "save" THEN DO:
        cmon:SCREEN-VALUE = STRING(TIME, "hh:mm:ss") + " save file " + hEditorWin:TITLE.

       /* 16-APR-2008 Jan Kierse  Disable the timer if it is enabled, so it does not fire during a compile error message, which disturbs the AppBuilder to scroll to the location of the error */
        lStarted = chCtrlSpy:PSTimerSpy:ENABLED .
        IF lStarted THEN chCtrlSpy:PSTimerSpy:ENABLED = NO.

        APPLY 'choose' TO hSaveFileMI.

       /* 16-APR-2008 Jan Kierse  reenable the timer if it was started in the first place */
        IF lStarted THEN chCtrlSpy:PSTimerSpy:ENABLED = YES.

        FIND ttEdt WHERE ttEdt.hEditor = phEditor NO-ERROR.
        IF AVAILABLE ttEdt THEN RUN updateEdtFileName (BUFFER ttEdt).
        ttEdt.cWinTitle = ttEdt.hWin:TITLE. /* update it now so the Spying timer does not believe we deal with another source code IN this editor window */
        cmon:SCREEN-VALUE = STRING(TIME, "hh:mm:ss") + " save FILE done for " + ttEdt.cFullPathName.
        IF pcFiredBy = "Ctrl-s" AND gcActionOnCtrlS = "saveAndGlobalLoad" THEN RUN loadGlobalResources (phEditor).
        RETURN.
    END.
    hSaveFileMI = hSaveFileMI:NEXT-SIBLING.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveSetting C-Win
PROCEDURE saveSetting :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cSection        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hRunningInfoWin AS HANDLE      NO-UNDO.


/* 07-DEC-2006 sla: retrieve DLC in default environment */
DEFINE VARIABLE cDLC       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVersionSP AS CHARACTER  NO-UNDO.
GET-KEY-VALUE SECTION "startup" KEY "DLC" VALUE cDLC.
IF SEARCH(cDLC + "/version.") <> ? THEN DO:
    INPUT FROM VALUE(cDLC + "/version.").
    IMPORT UNFORM cVersionSP.
    INPUT CLOSE.
    cVersionSP = ENTRY(3, cVersionSP, " ").
END.


LOAD 'Software\PSC\PROGRESS\slacroixTools' BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
/* create it if it does not exist  ... well, would be surprising...*/
IF ERROR-STATUS:ERROR THEN LOAD 'Software\PSC\PROGRESS\slacroixTools' NEW BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN USE 'Software\PSC\PROGRESS\slacroixTools'.

cSection = 'ABHacker'.

PUT-KEY-VALUE SECTION cSection KEY 'WinX' VALUE STRING({&WINDOW-NAME}:X + giPreferencesWinXOffset). /* 16-MAR-2007 sla: improvement if window was shifted on the left when expanded, and closed in this state */

PUT-KEY-VALUE SECTION cSection KEY 'WinY' VALUE STRING({&WINDOW-NAME}:Y).

PUT-KEY-VALUE SECTION cSection KEY 'glTopOnly'                   VALUE STRING(glTopOnly).
PUT-KEY-VALUE SECTION cSection KEY 'glCompleteAttrOnTab'         VALUE STRING(glCompleteAttrOnTab).
PUT-KEY-VALUE SECTION cSection KEY 'glCompleteOnTab'             VALUE STRING(glCompleteOnTab).
PUT-KEY-VALUE SECTION cSection KEY 'glAutoComplete'              VALUE STRING(glAutoComplete).
PUT-KEY-VALUE SECTION cSection KEY 'giAutoCompMinSize'           VALUE STRING(giAutoCompMinSize).
PUT-KEY-VALUE SECTION cSection KEY 'glCustomCompletionOnTab'     VALUE STRING(glCustomCompletionOnTab).
PUT-KEY-VALUE SECTION cSection KEY 'gcComplDateFormat'           VALUE STRING(gcComplDateFormat).
PUT-KEY-VALUE SECTION cSection KEY 'glVariableCompletion'        VALUE STRING(glVariableCompletion).
PUT-KEY-VALUE SECTION cSection KEY 'glCompleteTableOnFLA'        VALUE STRING(glCompleteTableOnFLA).
PUT-KEY-VALUE SECTION cSection KEY 'glAddNoLock'                 VALUE STRING(glAddNoLock).
PUT-KEY-VALUE SECTION cSection KEY 'glAddWhereAfterBuffer'       VALUE STRING(glAddWhereAfterBuffer).
PUT-KEY-VALUE SECTION cSection KEY 'glCompleteField'             VALUE STRING(glCompleteField).
PUT-KEY-VALUE SECTION cSection KEY 'gcFieldSortedBy'             VALUE STRING(gcFieldSortedBy).
PUT-KEY-VALUE SECTION cSection KEY 'glPlusplus'                  VALUE STRING(glPlusplus).
PUT-KEY-VALUE SECTION cSection KEY 'glImprovedCtrlKeftRight'     VALUE STRING(glImprovedCtrlKeftRight).
PUT-KEY-VALUE SECTION cSection KEY 'glcommentOnDblSlash'         VALUE STRING(glcommentOnDblSlash).
PUT-KEY-VALUE SECTION cSection KEY 'glAlignEnabled'              VALUE STRING(glAlignEnabled).
PUT-KEY-VALUE SECTION cSection KEY 'glAlignVars'                 VALUE STRING(glAlignVars).
PUT-KEY-VALUE SECTION cSection KEY 'glAlignTTFields'             VALUE STRING(glAlignTTFields).
PUT-KEY-VALUE SECTION cSection KEY 'glAlignTTFieldsSort'         VALUE STRING(glAlignTTFieldsSort).
PUT-KEY-VALUE SECTION cSection KEY 'glAlignAssign'               VALUE STRING(glAlignAssign).
PUT-KEY-VALUE SECTION cSection KEY 'gltooltipOnOpenParenthese'   VALUE STRING(gltooltipOnOpenParenthese).
PUT-KEY-VALUE SECTION cSection KEY 'glAnalyseQuery'              VALUE STRING(glAnalyseQuery).
PUT-KEY-VALUE SECTION cSection KEY 'glAutoStopWhenNonEditorWin'  VALUE STRING(glAutoStopWhenNonEditorWin).
PUT-KEY-VALUE SECTION cSection KEY 'glResizableDictionary'       VALUE STRING(glResizableDictionary).
PUT-KEY-VALUE SECTION cSection KEY 'giTabNumSpaces'              VALUE STRING(giTabNumSpaces).
PUT-KEY-VALUE SECTION cSection KEY 'glAddEndAfterDo'             VALUE STRING(glAddEndAfterDo).
PUT-KEY-VALUE SECTION cSection KEY 'glIntProcCompletion'         VALUE STRING(glIntProcCompletion).
PUT-KEY-VALUE SECTION cSection KEY 'glIntFuncCompletion'         VALUE STRING(glIntFuncCompletion).
PUT-KEY-VALUE SECTION cSection KEY 'glIntProcInputInSig'         VALUE STRING(glIntProcInputInSig).
PUT-KEY-VALUE SECTION cSection KEY 'glCtrlBackspacelDelPrevWord' VALUE STRING(glCtrlBackspacelDelPrevWord).
PUT-KEY-VALUE SECTION cSection KEY 'glCtrlDelDelsWord'           VALUE STRING(glCtrlDelDelsWord).
PUT-KEY-VALUE SECTION cSection KEY 'giQueryTooltipTimeSuccess'   VALUE STRING(giQueryTooltipTimeSuccess).
PUT-KEY-VALUE SECTION cSection KEY 'giQueryTooltipTimeFail'      VALUE STRING(giQueryTooltipTimeFail).
PUT-KEY-VALUE SECTION cSection KEY 'glCompile'                   VALUE STRING(glCompile).
PUT-KEY-VALUE SECTION cSection KEY 'glListingOnCtrlAltT'         VALUE STRING(glListingOnCtrlAltT).
PUT-KEY-VALUE SECTION cSection KEY 'glDebugListingOnCtrlAltD'    VALUE STRING(glDebugListingOnCtrlAltD).
PUT-KEY-VALUE SECTION cSection KEY 'glCompileMinsize'            VALUE STRING(glCompileMinsize).
PUT-KEY-VALUE SECTION cSection KEY 'glOpenPW'                    VALUE STRING(glOpenPW).
PUT-KEY-VALUE SECTION cSection KEY 'glLowerCaseCompletion'       VALUE STRING(glLowerCaseCompletion).
PUT-KEY-VALUE SECTION cSection KEY 'glFilterOnViewAsWidgetType'  VALUE STRING(glFilterOnViewAsWidgetType).
PUT-KEY-VALUE SECTION cSection KEY 'glFilterGuessObjectType'     VALUE STRING(glFilterGuessObjectType).
PUT-KEY-VALUE SECTION cSection KEY 'glInsertClosingGuys'         VALUE STRING(glInsertClosingGuys).
PUT-KEY-VALUE SECTION cSection KEY 'glExtProcCompletion'         VALUE STRING(glExtProcCompletion).
PUT-KEY-VALUE SECTION cSection KEY 'giMaxFilesToLoad'            VALUE STRING(giMaxFilesToLoad).
PUT-KEY-VALUE SECTION cSection KEY 'giMaxEtimeToLoadFiles'       VALUE STRING(giMaxEtimeToLoadFiles).
PUT-KEY-VALUE SECTION cSection KEY 'gcExtProcIgnoreDir'          VALUE STRING(gcExtProcIgnoreDir).
PUT-KEY-VALUE SECTION cSection KEY 'glCtrlF3NewProcWin'          VALUE STRING(glCtrlF3NewProcWin).
PUT-KEY-VALUE SECTION cSection KEY 'glProposeOpenSelectedFile'   VALUE STRING(glProposeOpenSelectedFile).
PUT-KEY-VALUE SECTION cSection KEY 'gcFancyInABWin'              VALUE STRING(gcFancyInABWin).
PUT-KEY-VALUE SECTION cSection KEY 'glcommentSelectionOnSlash'   VALUE STRING(glcommentSelectionOnSlash).
PUT-KEY-VALUE SECTION cSection KEY 'glHideAllComments'           VALUE STRING(glHideAllComments).
PUT-KEY-VALUE SECTION cSection KEY 'glHideSelection'             VALUE STRING(glHideSelection).
PUT-KEY-VALUE SECTION cSection KEY 'glXrefAnal'                  VALUE STRING(glXrefAnal).
PUT-KEY-VALUE SECTION cSection KEY 'glEnhancedCloseParenthese'   VALUE STRING(glEnhancedCloseParenthese).
PUT-KEY-VALUE SECTION cSection KEY 'glShowSpecialChars'                 VALUE STRING(glShowSpecialChars).
PUT-KEY-VALUE SECTION cSection KEY 'glShowLineNumbers'           VALUE STRING(glShowLineNumbers).
PUT-KEY-VALUE SECTION cSection KEY 'gcSuggestBufferFor'          VALUE STRING(gcSuggestBufferFor).
PUT-KEY-VALUE SECTION cSection KEY 'gcSuggestBufferForNewGuysStamp'  VALUE STRING(gcSuggestBufferForNewGuysStamp).
PUT-KEY-VALUE SECTION cSection KEY 'gcDumpedResourceFileRoot'    VALUE STRING(gcDumpedResourceFileRoot).
PUT-KEY-VALUE SECTION cSection KEY 'glKeepTrackListingBuffer'    VALUE IF glKeepTrackListingBuffer THEN "YES" ELSE "NoReallyAskedExplicitly". /* 16-SEP-2007 sla: new value for NO to make sure it was really unchecked explicitely */  /* 21-OCT-2008 sla: changed value for NO to make sure people have not missed it */
PUT-KEY-VALUE SECTION cSection KEY 'giSplitParamInCalls'         VALUE STRING(giSplitParamInCalls).
PUT-KEY-VALUE SECTION cSection KEY 'gcSplitParamCommaStyle'      VALUE STRING(gcSplitParamCommaStyle).
PUT-KEY-VALUE SECTION cSection KEY 'gcGlobalResCat'              VALUE STRING(gcGlobalResCat).
PUT-KEY-VALUE SECTION cSection KEY 'gcCatDbConnection' + cVersionSP VALUE STRING(gcCatDbConnection).
PUT-KEY-VALUE SECTION cSection KEY 'glClassNewCastAfterEqual'    VALUE STRING(glClassNewCastAfterEqual).
PUT-KEY-VALUE SECTION cSection KEY 'glRunInLibComp'              VALUE STRING(glRunInLibComp).
PUT-KEY-VALUE SECTION cSection KEY 'glFindOSFilesWithAbhackDB'   VALUE STRING(glFindOSFilesWithAbhackDB).
PUT-KEY-VALUE SECTION cSection KEY 'glMoveNewWindowsAway'        VALUE STRING(glMoveNewWindowsAway).
PUT-KEY-VALUE SECTION cSection KEY 'giMaxEmptyLinesBeforeShrinking' VALUE STRING(giMaxEmptyLinesBeforeShrinking).
PUT-KEY-VALUE SECTION cSection KEY 'glAdaptWindowHeightOnCtrlW'  VALUE STRING(glAdaptWindowHeightOnCtrlW).
PUT-KEY-VALUE SECTION cSection KEY 'glCompleteOOSuperMethod'     VALUE STRING(glCompleteOOSuperMethod).
PUT-KEY-VALUE SECTION cSection KEY 'glHypeStrikeMode'            VALUE STRING(glHypeStrikeMode).
PUT-KEY-VALUE SECTION cSection KEY 'glPreprocessors'             VALUE STRING(glPreprocessors).


/* they will have to close ABHack with the french flag at least once to get the glHasSheSeenFrenchRomance set ;) */
PUT-KEY-VALUE SECTION cSection KEY 'glHasSheSeenFrenchRomance'   VALUE STRING(glHasSheSeenFrenchRomance OR gcFancyInABWin = "viveLaFrance").

PUT-KEY-VALUE SECTION cSection KEY 'glNoCompletionInStrings'     VALUE STRING(glNoCompletionInStrings).
PUT-KEY-VALUE SECTION cSection KEY 'glNoCompletionInComment'     VALUE STRING(glNoCompletionInComment).
PUT-KEY-VALUE SECTION cSection KEY 'glConvertDbTablesToLC'       VALUE STRING(glConvertDbTablesToLC).
PUT-KEY-VALUE SECTION cSection KEY 'glKeepTrackOfCursor'         VALUE STRING(glKeepTrackOfCursor).
PUT-KEY-VALUE SECTION cSection KEY 'glCompleteBlockLabels'       VALUE STRING(glCompleteBlockLabels).
PUT-KEY-VALUE SECTION cSection KEY 'glStopStartTimerOnCtrlAltO'  VALUE STRING(glStopStartTimerOnCtrlAltO).
PUT-KEY-VALUE SECTION cSection KEY 'glCopyUIBellowMouse'         VALUE STRING(glCopyUIBelowMouse).
PUT-KEY-VALUE SECTION cSection KEY 'glJumpMruSection'            VALUE STRING(glJumpMruSection).
PUT-KEY-VALUE SECTION cSection KEY 'glDrawBoxAroundCurrentLine'  VALUE STRING(glDrawBoxAroundCurrentLine).
PUT-KEY-VALUE SECTION cSection KEY 'giRightMarginIndicatorCol'   VALUE STRING(giRightMarginIndicatorCol).
PUT-KEY-VALUE SECTION cSection KEY 'glFindNextSelected'          VALUE STRING(glFindNextSelected).
PUT-KEY-VALUE SECTION cSection KEY 'giNameComboFont'             VALUE STRING(giNameComboFont).
PUT-KEY-VALUE SECTION cSection KEY 'glUseSchemaCache'            VALUE STRING(glUseSchemaCache).
PUT-KEY-VALUE SECTION cSection KEY 'gcActionOnCtrlS'             VALUE STRING(gcActionOnCtrlS).
PUT-KEY-VALUE SECTION cSection KEY 'btnExpand'                   VALUE STRING(btnExpand:LABEL IN FRAME {&FRAME-NAME}).
PUT-KEY-VALUE SECTION cSection KEY 'gcSectionOutlineMode'        VALUE STRING(gcSectionOutlineMode).
PUT-KEY-VALUE SECTION cSection KEY 'glSectionLayoutIgnoreStructProcs' VALUE STRING(glSectionLayoutIgnoreStructProcs).
PUT-KEY-VALUE SECTION cSection KEY 'glAbhackFind'                VALUE STRING(glAbhackFind).
PUT-KEY-VALUE SECTION cSection KEY 'giWinHeight'                 VALUE STRING(giWinHeight).
PUT-KEY-VALUE SECTION cSection KEY 'gcAbhackFindScope'           VALUE STRING(gcAbhackFindScope).
PUT-KEY-VALUE SECTION cSection KEY 'gcAbhackFindMru'             VALUE STRING(gcAbhackFindMru).


/* 15-FEB-2007 sla: new profiled info for abhackinfowin */
PUT-KEY-VALUE SECTION cSection KEY 'glInfoWinTopOnly'           VALUE STRING(glInfoWinTopOnly).
PUT-KEY-VALUE SECTION cSection KEY 'giInfoWinHeight'            VALUE STRING(giInfoWinHeight).
PUT-KEY-VALUE SECTION cSection KEY 'giInfoWinWidth'             VALUE STRING(giInfoWinWidth).
PUT-KEY-VALUE SECTION cSection KEY 'giInfoWinX'                 VALUE STRING(giInfoWinX).
PUT-KEY-VALUE SECTION cSection KEY 'giInfoWinY'                 VALUE STRING(giInfoWinY).
PUT-KEY-VALUE SECTION cSection KEY 'glQADispInABHWin'           VALUE STRING(glQADispInABHWin).
PUT-KEY-VALUE SECTION cSection KEY 'glDisplaytextSelectedInfo'  VALUE STRING(glDisplaytextSelectedInfo).
PUT-KEY-VALUE SECTION cSection KEY 'giInfoWinBgcolor'           VALUE STRING(giInfoWinBgcolor).
PUT-KEY-VALUE SECTION cSection KEY 'giInfoWinFgcolor'           VALUE STRING(giInfoWinFgcolor).


PUT-KEY-VALUE SECTION cSection KEY 'glSectionLayoutWinTopOnly'  VALUE STRING(glSectionLayoutWinTopOnly).
PUT-KEY-VALUE SECTION cSection KEY 'giSectionLayoutWinHeight'   VALUE STRING(giSectionLayoutWinHeight).
PUT-KEY-VALUE SECTION cSection KEY 'giSectionLayoutWinWidth'    VALUE STRING(giSectionLayoutWinWidth).
PUT-KEY-VALUE SECTION cSection KEY 'giSectionLayoutWinX'        VALUE STRING(giSectionLayoutWinX).
PUT-KEY-VALUE SECTION cSection KEY 'giSectionLayoutWinY'        VALUE STRING(giSectionLayoutWinY).


PUBLISH "isAnyRunningABHackWinInfo" (OUTPUT hRunningInfoWin).
PUT-KEY-VALUE SECTION cSection KEY 'lisAnyRunningABHackWinInfo'  VALUE STRING(VALID-HANDLE(hRunningInfoWin)).

/* 07-DEC-2006 sla: make this one version dependent for people that have multiple version of Progess */
PUT-KEY-VALUE SECTION cSection KEY 'glForceWaitFor'   + cVersionSP VALUE STRING(glForceWaitFor).
PUT-KEY-VALUE SECTION cSection KEY 'glAutoStopDialog' + cVersionSP VALUE STRING(glAutoStopDialog).

/* 25-OCT-2013 jcc: new configuration option to save/restore the session */
PUT-KEY-VALUE SECTION cSection KEY 'glSaveSession'              VALUE STRING(glSaveSession).

/* unload the environment to go back to default env */
UNLOAD 'Software\PSC\PROGRESS\slacroixTools' NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAbhackFindProfile C-Win
PROCEDURE setAbhackFindProfile :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcAbhackFindScope AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pcAbhackFindMRU   AS CHARACTER   NO-UNDO.


ASSIGN
 gcAbhackFindScope = pcAbhackFindScope
 gcAbhackFindMru   = pcAbhackFindMru.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setCtrlFAction C-Win
PROCEDURE setCtrlFAction :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

DEFINE VARIABLE hMenuFind AS HANDLE      NO-UNDO.
hMenuFind = phEditor:WINDOW:MENU-BAR:FIRST-CHILD NO-ERROR.
DO WHILE hMenuFind <> ?:
    IF hMenuFind:LABEL = "&Search" THEN hMenuFind = hMenuFind:FIRST-CHILD.
    IF hMenuFind:LABEL = "&Find..."   THEN LEAVE.

    hMenuFind = hMenuFind:NEXT-SIBLING.
END.
IF VALID-HANDLE(hMenuFind)
 THEN hMenuFind:ACCELERATOR = IF glAbhackFind THEN "" ELSE "CTRL-F".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setInfoWinProfile C-Win
PROCEDURE setInfoWinProfile :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER plInfoWinTopOnly AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER piInfoWinHeight  AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER piInfoWinWidth   AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER piInfoWinX       AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER piInfoWinY       AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER piInfoWinBgcolor AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER piInfoWinFgcolor AS INTEGER     NO-UNDO.

ASSIGN
 glInfoWinTopOnly = plInfoWinTopOnly
 giInfoWinHeight  = piInfoWinHeight
 giInfoWinWidth   = piInfoWinWidth
 giInfoWinX       = piInfoWinX
 giInfoWinY       = piInfoWinY
 giInfoWinBgcolor = piInfoWinBgcolor
 giInfoWinFgcolor = piInfoWinFgcolor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setReturnValue C-Win
PROCEDURE setReturnValue :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcReturnValueToSet AS CHARACTER   NO-UNDO.

RETURN pcReturnValueToSet.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSectionLayoutWinProfile C-Win
PROCEDURE setSectionLayoutWinProfile :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER plSectionLayoutWinTopOnly AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER piSectionLayoutWinHeight  AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER piSectionLayoutWinWidth   AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER piSectionLayoutWinX       AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER piSectionLayoutWinY       AS INTEGER     NO-UNDO.

ASSIGN
 glSectionLayoutWinTopOnly = plSectionLayoutWinTopOnly
 giSectionLayoutWinHeight  = piSectionLayoutWinHeight
 giSectionLayoutWinWidth   = piSectionLayoutWinWidth
 giSectionLayoutWinX       = piSectionLayoutWinX
 giSectionLayoutWinY       = piSectionLayoutWinY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTempDirAndImportFile C-Win
PROCEDURE setTempDirAndImportFile :
/*------------------------------------------------------------------------------
  Purpose:     Create a abhackTmp directory then a subdirectory with name = PID
   We will be able to save files there with same name as the source file being
   edited.  The goal is to let a compile handle the usage {&FILE-NAME} directive.

   The files generated there will be deleted when:
     a) a global Load action is fired
     b) on a clean quit of ABHack

     In case of crash , the directory will remain
   However local load action will leave the files there
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iPID AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cTempRoot AS CHARACTER   NO-UNDO.

    RUN GetCurrentProcessId (OUTPUT iPID).

    cTempRoot = SESSION:TEMP-DIRECTORY + "abhackTmp".

    FILE-INFO:FILE-NAME = cTempRoot.
    IF FILE-INFO:FILE-TYPE = ? THEN DO:
        OS-CREATE-DIR VALUE(cTempRoot).
        IF OS-ERROR <> 0 THEN DO:
            MESSAGE "Serious Error: ABHack needs to work with a directory SESSION:TEMP-DIRECTORY + '/abhackTmp' (" + cTempRoot + ")" SKIP
             "Failed create this directory.  Please allocate sufficient permission to create a sub-direcrory and files there" SKIP
             "Note that SESSION:TEMP-DIRECTORY equals to the value of the -T Client Session Startup Param" SKIP(2)
             "About to quit ABHack"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN ERROR.
        END.
        FILE-INFO:FILE-NAME = cTempRoot.
    END.

    IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN DO:
        MESSAGE "Serious Error: ABHack needs to work with a directory SESSION:TEMP-DIRECTORY + '/abhackTmp' (" + cTempRoot + ")" SKIP
         "A file with that name exists but is not a directory.  Please delete this file and make sure you have sufficient permission to create a sub-direcrory and files there " SKIP
         "Note that SESSION:TEMP-DIRECTORY equals to the value of the -T Client Session Startup Param" SKIP(2)
         "About to quit ABHack"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN ERROR.
    END.

    ASSIGN
     gcPID            = STRING(iPID)
     gcImportDirName = cTempRoot + "\" + gcPID. /* make sure -T is *local*    Note, we need to use the .p extension if we want to keep the color scheme of ABL source code */


    RUN cleanupABHackTempFiles. /* first clean up the files, we will recreate the directory afterwards */


    FILE-INFO:FILE-NAME = gcImportDirName.
    IF FILE-INFO:FILE-TYPE = ? THEN DO:    /* normally, we should recreate this directroy each time now... */
        OS-CREATE-DIR VALUE(gcImportDirName).
        IF OS-ERROR <> 0 THEN DO:
            MESSAGE "Serious Error: ABHack needs to work with a directory SESSION:TEMP-DIRECTORY + '/abhackTmp/<PID>' ("
              gcImportDirName SKIP
             "Failed create this directory.  Please allocate sufficient permission to create a sub-direcrory and files there" SKIP
             "Note that SESSION:TEMP-DIRECTORY equals to the value of the -T Client Session Startup Param" SKIP(2)
             "About to quit ABHack"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN ERROR.
        END.
        FILE-INFO:FILE-NAME = cTempRoot.
    END.

    IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN DO:
        MESSAGE "Serious Error: ABHack needs to work with a directory SESSION:TEMP-DIRECTORY + '/abhackTmp/<PID>' (" + gcImportDirName SKIP
         "A file with that name exists but is not a directory.  Please delete this file and make sure you have sufficient permission to create a sub-direcrory and files there " SKIP
         "Note that SESSION:TEMP-DIRECTORY equals to the value of the -T Client Session Startup Param" SKIP(2)
         "About to quit ABHack"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN ERROR.
    END.

    gcImportDirName = gcImportDirName + "/".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTooltips C-Win
PROCEDURE setTooltips :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

DO WITH FRAME fMain:
    glCustomCompletionOnTab:TOOLTIP IN FRAME fComplCustom = glCustomCompletionOnTab:TOOLTIP
     + "~nprotools/abhack/customTabCompletion.txt is reloaded each time you recheck this toggle-box".

    glCompleteAttrOnTab:TOOLTIP IN FRAME fComplColon = glCompleteAttrOnTab:TOOLTIP
     + "~nprotools/abhack/attrTabCompletion.txt is reloaded each time you recheck this toggle-box".

    gltooltipOnOpenParenthese:TOOLTIP IN FRAME fMisc = gltooltipOnOpenParenthese:TOOLTIP
     + "~nSee keys that begin 'disabledApiTooltip' in HKEY_CURRENT_USER\Software\PSC\PROGRESS\slacroixTools\ABHacker~nin order to renable tooltips as wanted"
     + "~nA popup-menu is available on each individual API tooltip to not show it anymore".

    glPlusplus:TOOLTIP IN FRAME fMisc = 'myVar++              myVar = myVar +~nmyVar1+               myVar = myVar + 1~nmyVar-+               myVar = myVar -~nmyVar,+               myVar = myVar + "," +~nmyVar|+               myVar = myVar + "|" +~nmyVarCHR(x)+     myVar = myVar + CHR(x) +'.

    btnloadGlobalResources:TOOLTIP = btnloadGlobalResources:TOOLTIP + "~nYou can also press Ctrl-Alt-G or Alt-G from the procedure editor at any time"
     + "~nNote you need to FIRST SAVE your file if you want this process to find newly added global resources"
     + "~nThis operation is required to have popups on procedure and function names".

    glAutoStopWhenNonEditorWin:TOOLTIP = glAutoStopWhenNonEditorWin:TOOLTIP + "~nABHack will try to restart spying by itself on a keyboard action in an editor that has already been spied on".

    glImprovedCtrlKeftRight:TOOLTIP IN FRAME fKeys = glImprovedCtrlKeftRight:TOOLTIP
     + "~n  1) it does not ignore words made of digits"
     + "~n  2) goes to end of line before going to next word at next line"
     + "~n  3) it considers '_' as part of words".


    giTabNumSpaces:TOOLTIP IN FRAME fComplEnable = giTabNumSpaces:TOOLTIP + "~nBEWARE to keep it in synch with Option->EditingOptions->Tabs".

    glForceWaitFor:TOOLTIP = glForceWaitFor:TOOLTIP + "~nThis may cause a little error 4123 when you close ABHack before closing a .w file you just run, but it remains stable"
     + "~nNote that normal PERSISTENT tools should not do a WAIT-FOR, but the point is to work around a prowin32.exe core issue"
     + "~nNOTE ALSO IT IS BETTER TO RUN ABHack FROM THE PROGRESS DESKTOP BEFORE LAUNCHING THE AppBuilder (see release notes)".

    glIntProcCompletion:TOOLTIP IN FRAME fComplRun = glIntProcCompletion:TOOLTIP + "~nLoad global resources Action required for this feature".
    glIntFuncCompletion:TOOLTIP IN FRAME fComplVarUDF = glIntFuncCompletion:TOOLTIP + "~nLoad global resources Action required for this feature".

    /* the ~{ shall be inserted by program in order to avoid compilation problems */
    glExtProcCompletion:TOOLTIP = glExtProcCompletion:TOOLTIP + " ~{*.i".
    glExtProcCompletion:LABEL = glExtProcCompletion:LABEL + " ~{*.i".

    glHideAllComments:TOOLTIP = "Alt-C => native VSLick feature to hide & collapse all multi-line comments.~nPress Alt-C twice quickly to show all".
    glHideSelection:TOOLTIP = "Alt-H => native VSLick feature to hide & collapse a multi-line selection.~nBeware, the plus sign will appear at the line *above* the first selected line, so you might want to start your selection at the next line.~nSelect the line with the plus sign and press Alt-S to restore".

    glMulitpleClipboardDummyToggle:TOOLTIP = "Can't be disabled.  The point is to show that ABHack re-enables this nice native VSlick feature that has been broken in OE 10".
    glMaximizeRestore:TOOLTIP = "Can't be disabled.  The point is to show that ABHack can Maximize/Restore the editor window on Ctrl-M like Eclipse does".

    glCompleteWithSimilar:TOOLTIP = "VSlick native completion on Alt-Left/Right with a word located before/after carret that begins with same prefix"
     + "~nAlthough it cannot compete with other ABHack completions, I thought I *had* to implement this amazing VSLick native feature".

    glcommentSelectionOnSlash:TOOLTIP = glcommentSelectionOnSlash:TOOLTIP
     + "~nThis cannot work with the '/' key of the numeric keypad.  Read the release notes for explanations".

    btnOpenCustomCompletionFileOfMin:TOOLTIP = btnOpenCustomCompletionFileOfMin:TOOLTIP + " TO EXTEND THE STANDARD".

    glUseSchemaCache:TOOLTIP = glUseSchemaCache:TOOLTIP + "~nDo not use this option if your development session changes the connected databases with different schema (no problem if schemas are the same in databases with same name)".

    gcSplitParamCommaStyle:TOOLTIP = "leading comma's"
     + "~nRUN extractWordN:"
     + "~n                 (piWord"
     + "~n                 ,pcLine"
     + "~n                 ,piCursorChar"
     + "~n                 ,pcOptn"
     + "~n                 ,OUTPUT opcWord)"
     + "~n~ntrailing comma's:"
     + "~nRUN extractWordN"
     + "~n                 (piWord,"
     + "~n                  pcLine,"
     + "~n                  piCursorChar,"
     + "~n                  pcOptn,"
     + "~n                  OUTPUT opcWord)".

    glIntProcCompletion:TOOLTIP = glIntProcCompletion:TOOLTIP + "~nThis also affects UDF and method calls".
    glIntProcInputInSig:TOOLTIP = glIntProcInputInSig:TOOLTIP + "~nThis also affects UDF and method calls".
    giSplitParamInCalls:TOOLTIP = giSplitParamInCalls:TOOLTIP + "~nThis also affects UDF and method calls".

    glHypeStrikeMode:TOOLTIP = glHypeStrikeMode:TOOLTIP
     + "~nTo anticipate you want a popup item with a trailling space or any character in ',:()[].=<>+'".

    glNoCompletionInComment:TOOLTIP = glNoCompletionInComment:TOOLTIP + "~nNote this feature is limited to comments that begin and end in the current line".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showMatchingGuy C-Win
PROCEDURE showMatchingGuy :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcGuy    AS CHARACTER   NO-UNDO.
/* param to remove when I know how to implement this feature */
DEFINE INPUT  PARAMETER ThisParamWillMakeTheRunFail AS CHARACTER   NO-UNDO.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showTooltip C-Win
PROCEDURE showTooltip :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER pcText   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcOptn   AS CHARACTER  NO-UNDO.


IF CAN-DO(pcOptn, "single") THEN PUBLISH "KillEditorTooltip" FROM THIS-PROCEDURE (phEditor).

DEFINE VARIABLE hTooltipProc AS HANDLE     NO-UNDO.
RUN protools/abhack/procEditorTooltip.w PERSIST SET hTooltipProc (phEditor).

IF VALID-HANDLE(hTooltipProc) /* invalid if it decided to kill itself */
 THEN RUN loadText IN hTooltipProc (pcText, pcOptn).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE shrinkWindow C-Win
PROCEDURE shrinkWindow :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ttEdt FOR ttEdt.
    DEFINE VARIABLE iEditorLineHeightPixels AS INTEGER     NO-UNDO.

    iEditorLineHeightPixels = FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(ttEdt.hEditor:FONT).

    /* Shrink */
    /* 06-NOV-2007 sla: changed the + 2  into + 3 because of the automatic scrolling done by vslick when reaching the bottom of the viewport */
    IF MAXIMUM (giMaxEmptyLinesBeforeShrinking ,ttEdt.hEditor:NUM-LINES + 3) < (SESSION:WORK-AREA-HEIGHT-PIXELS - 130) / iEditorLineHeightPixels
     THEN DO:
        ttEdt.hWin:HEIGHT-PIXELS = MAXIMUM(giMaxEmptyLinesBeforeShrinking, ttEdt.hEditor:NUM-LINES + 4) * iEditorLineHeightPixels + 25.
        APPLY "WINDOW-RESIZED"  TO ttEdt.hWin.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slashPressed C-Win
PROCEDURE slashPressed :
/*------------------------------------------------------------------------------
  Purpose:     comment or uncomment a line when typing '//'
  Notes:       if you really want to type '//', then type a space in between
  and remove it

  /* 27-NOV-2006 sla: special handling for the '/' of the numeric keypad */
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

DEFINE VARIABLE cLine                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMayBeNoApply         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrevChar             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrevPrevChar         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSelectionText        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cStringAfterCursor    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hEditorListProc       AS HANDLE      NO-UNDO.
DEFINE VARIABLE iCursorChar           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSrcEditorReturnValue AS INTEGER     NO-UNDO.
DEFINE VARIABLE lNumericKeyPadSlash   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mSrcEditorKBState     AS MEMPTR      NO-UNDO.

IF chCtrlSpy:PSTimerSpy:ENABLED = NO THEN RETURN. /* 01-FEB-2008 sla: improvment so people can disable A4GBL completion in javascripts */


SET-SIZE(mSrcEditorKBState) = 256.
RUN GetKeyboardState(GET-POINTER-VALUE(mSrcEditorKBState), OUTPUT iSrcEditorReturnValue).
lNumericKeyPadSlash = GET-BYTE(mSrcEditorKBState, 112) > 127.
SET-SIZE(mSrcEditorKBState) = 0.

IF lNumericKeyPadSlash THEN cMayBeNoApply = "NoApply".

/* 13-JAN-2007 sla: New comment selection on '/' feature */
IF phEditor:TEXT-SELECTED THEN DO:
    IF NOT glcommentSelectionOnSlash THEN RETURN cMayBeNoApply.

    cSelectionText = phEditor:SELECTION-TEXT NO-ERROR.
    IF  LENGTH(cSelectionText) > 15000 THEN DO:
         MESSAGE "Selection is too large for abhack to comment it this way" SKIP
          "Please use the native comment/uncomment feature in the popup menu -> format selection"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN "NoApply".
     END.
    IF TRIM(cSelectionText) BEGINS "/~*"
     THEN phEditor:SOURCE-COMMAND('comment-erase', '').
    ELSE phEditor:SOURCE-COMMAND('comment', '').
    RETURN "NoApply".
END.

/* starting from this point, we deal with the comment line on '//' feature */
IF NOT glcommentOnDblSlash THEN RETURN cMayBeNoApply.

iCursorChar = phEditor:CURSOR-CHAR.
IF lNumericKeyPadSlash THEN iCursorChar = iCursorChar - 1.

RUN getEditorLine IN TARGET-PROCEDURE (phEditor, OUTPUT cLine).
IF  iCursorChar < 2
 OR LENGTH(TRIM(cLine)) < 1 /* should never happen */
 THEN RETURN cMayBeNoApply.

cPrevChar = SUBSTRING(cLine, iCursorChar - 1, 1).
cPrevPrevChar = SUBSTRING(cline, iCursorChar - 2, 1) NO-ERROR.
/* 08-JAN-2007 sla: Improvement, let us use double slash after colon like in http://  */
IF cPrevPrevChar = ":" THEN RETURN cMayBeNoApply.

cStringAfterCursor = SUBSTRING(cLine, phEditor:CURSOR-CHAR).

cMon:SCREEN-VALUE IN FRAME fMain = "/ pressed, prevChar = " + cPrevChar.

IF cPrevChar = "/" THEN DO:
    /* first remove the '/' */
    cLine = SUBSTRING(cLine, 1, iCursorChar - 2) + SUBSTRING(cLine, iCursorChar + IF lNumericKeyPadSlash THEN 1 ELSE 0).

    /* 06-DEC-2006 sla: Jean Christophe prefers to insert a comment at the end of a line and not move the cursor
      if // is typed at the end of a line (or if blank line, of course)
        => I believe he is right */
    IF TRIM(cStringAfterCursor) = "" THEN DO:
        phEditor:SOURCE-COMMAND('p4gl-maybe-case-backspace', '').
        IF lNumericKeyPadSlash THEN phEditor:INSERT-STRING("*  */").
        ELSE phEditor:INSERT-STRING("/*  */").
        phEditor:CURSOR-CHAR = phEditor:CURSOR-CHAR - 3.
        RETURN "noApply".
    END.

    cMon:SCREEN-VALUE = STRING(LENGTH(cLine)) + " - " + STRING(LENGTH(LEFT-TRIM(cLine))).
    cLine = FILL(" ", LENGTH(cLine) - LENGTH(LEFT-TRIM(cLine)))
           + IF TRIM(cLine) MATCHES "/~~~* * ~~~*~/"
              /* remove the comments */
              THEN SUBSTRING(TRIM(cLine), 4, LENGTH(TRIM(cLine)) - 6)
              ELSE IF TRIM(cLine) MATCHES "/~~~**~~~*~/"
                    /* remove the comments without space */
                    THEN SUBSTRING(TRIM(cLine), 4, LENGTH(TRIM(cLine)) - 6)
                    /* add the comments */
                    ELSE "/~* " + TRIM(cLine) + " *~/".
    phEditor:CURSOR-CHAR = 1. /* right place to replace from */
    phEditor:SOURCE-COMMAND('select_line', '').
    phEditor:REPLACE-SELECTION-TEXT(cLine + "~n").

    RETURN "noApply".
END.

RETURN cMayBeNoApply.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sortBufferList C-Win
PROCEDURE sortBufferList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER pcBufferList AS CHARACTER   NO-UNDO.
DEFINE INPUT        PARAMETER piCursorLine AS INTEGER     NO-UNDO.

DEFINE VARIABLE cSwapWith AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iSwapWith AS INTEGER     NO-UNDO.

DEFINE BUFFER ttusedbuffers   FOR ttusedbuffers.
DEFINE BUFFER ttsuggestbuffer FOR ttsuggestbuffer.

EMPTY TEMP-TABLE ttSuggestBuffer.
FOR EACH ttUsedBuffers WHERE ttUsedBuffers.iLine < piCursorLine
 BY ttUsedBuffers.iLine DESCENDING:
    FIND FIRST ttSuggestBuffer WHERE ttSuggestBuffer.cName = ttUsedBuffers.cName NO-ERROR.
    IF AVAILABLE ttSuggestBuffer THEN NEXT.
    CREATE ttSuggestBuffer.
    BUFFER-COPY ttUsedBuffers TO ttSuggestBuffer.
END.


/* 06-NOV-2007 sla: actually, this is how I want it to behave... */
FOR EACH ttsuggestbuffer BY ttSuggestBuffer.iLine:
    iSwapWith = LOOKUP(ttsuggestbuffer.cName, pcBufferList).
    IF iSwapWith <= 1 THEN NEXT. /* 1 means already first entry */

    /* move the buffer to the beginning of the list */
    ENTRY(iSwapWith, pcBufferList) = "##REMOVETHAT##".
    pcBufferList = ttsuggestbuffer.cName + "," + pcBufferList.
END.
pcBufferList = REPLACE(pcBufferList, "##REMOVETHAT##,", "").
pcBufferList = REPLACE(pcBufferList, ",##REMOVETHAT##", "").
pcBufferList = TRIM(pcBufferList, ",").


/* FOR EACH ttsuggestbuffer BY ttSuggestBuffer.iLine:                 */
/*     iSwapWith = LOOKUP(ttsuggestbuffer.cName, pcBufferList).       */
/*     IF iSwapWith <= 1 THEN NEXT. /* 1 means already first entry */ */
/*                                                                    */
/*     /* mov the buffer to the beginning of the list */              */
/*     cSwapWith = ENTRY(1, pcBufferList).                            */
/*     ENTRY(iSwapWith, pcBufferList) = cSwapWith.                    */
/*     ENTRY(1, pcBufferList) = ttsuggestbuffer.cName.                */
/* END.                                                               */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startStopSpyingTimer C-Win
PROCEDURE startStopSpyingTimer :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
APPLY "CHOOSE" TO btnStopStart IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stopABHackSpyingTimer C-Win
PROCEDURE stopABHackSpyingTimer :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

IF NOT glStopStartTimerOnCtrlAltO THEN RETURN.

cMon:SCREEN-VALUE IN FRAME fMain = STRING(TIME, "hh:mm:ss") + " Stopping Spying Timer".

chCtrlSpy:PSTimerSpy:ENABLED = NO.

btnStopStart:LABEL IN FRAME fMain = "&Start".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StructuredFetchCurrentTtMark C-Win
PROCEDURE StructuredFetchCurrentTtMark :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cRefinedBlockName AS CHARACTER   NO-UNDO.

DEFINE PARAMETER BUFFER ttEdt FOR ttEdt.
DEFINE PARAMETER BUFFER ttMark FOR ttMark.

IF NOT AVAILABLE ttEdt THEN RETURN.

IF ttEdt.currentSection = "Triggers" THEN DO:
    cRefinedBlockName = ttEdt.currentEvent + " OF ".
    IF NUM-ENTRIES(ttEdt.currentWidget, ".") > 2 THEN cRefinedBlockName = cRefinedBlockName + SUBSTRING(ENTRY(1, ttEdt.currentWidget, " "), INDEX(ttEdt.currentWidget, ".") + 1).
    ELSE cRefinedBlockName = cRefinedBlockName + ENTRY(1, ttEdt.currentWidget, " ").

    FIND FIRST ttMark WHERE
         ttMark.hEditor = ttEdt.hEditor
     AND ttMark.cBlockType = "Trigger"
     AND ttMark.cBlockName BEGINS cRefinedBlockName
     NO-ERROR.

    IF NOT AVAILABLE ttMark THEN FIND FIRST ttMark WHERE
         ttMark.hEditor = ttEdt.hEditor
     AND ttMark.cBlockType = "Trigger"
     AND ttMark.cBlockName BEGINS "ON " + cRefinedBlockName
     NO-ERROR.
END.

ELSE IF ttEdt.currentSection = "Main Block"
 THEN FIND FIRST ttMark WHERE
      ttMark.hEditor = ttEdt.hEditor
  AND ttMark.cBlockType = "MainBlock"
  NO-ERROR.

ELSE IF ttEdt.currentSection = "Definitions"
 THEN FIND FIRST ttMark WHERE
      ttMark.hEditor = ttEdt.hEditor
  AND ttMark.cBlockName = "definitions"
  NO-ERROR.

ELSE IF ttEdt.currentSection = "Procedures"
 THEN FIND FIRST ttMark WHERE
      ttMark.hEditor = ttEdt.hEditor
  AND ttMark.cBlockType = "procedure"
  AND ttMark.cBlockName = ttEdt.currentEvent
  NO-ERROR.

ELSE IF ttEdt.currentSection = "Functions"
 THEN FIND FIRST ttMark WHERE
      ttMark.hEditor = ttEdt.hEditor
  AND ttMark.cBlockType = "function"
  AND ttMark.cBlockName = ttEdt.currentEvent
  NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tabPressed C-Win
PROCEDURE tabPressed :
/*------------------------------------------------------------------------------
  Purpose:     Manage explicit completion on TAB
               This API is very closed to the valueChanged one.
               There are a few differences to keep in mind when the process is fired
               on the tab key however.
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

DEFINE VARIABLE cBufferList       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDatabaseName     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPreviousWord     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWordBeforeCarret AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hEditorListProc   AS HANDLE      NO-UNDO.
DEFINE VARIABLE iCursorChar       AS INTEGER     NO-UNDO.
DEFINE VARIABLE lReturnIfTrue     AS LOGICAL     NO-UNDO.

DEFINE BUFFER ttCustAlias FOR ttCustAlias.
DEFINE BUFFER ttFreeList  FOR ttFreeList.
DEFINE BUFFER ttVar       FOR ttVar.
DEFINE BUFFER ttgVar      FOR ttgVar.

iCursorChar = phEditor:CURSOR-CHAR.
IF iCursorChar = 1 THEN RETURN.


RUN getEditorLine IN TARGET-PROCEDURE (phEditor, OUTPUT cLine).

/* 27-JAN-2007 sla: new feature to disable popup when the carret is in a quoted string */
IF glNoCompletionInStrings AND carretInQuotedString(cLine, iCursorChar) THEN RETURN.

/* 18-DEC-2006 sla: performance improvement, as we were calling getEditorLine, we'd better call extractWordN than getPrevWord that would call getEditorLine again */
RUN extractWordN (0  /* word's position */
                 ,cLine
                 ,iCursorChar /* to cursor pos */
                 ,"allowEmptyWord,doNotPassCursor"
                 ,OUTPUT cWordBeforeCarret)
                NO-ERROR.
lReturnIfTrue = ERROR-STATUS:ERROR .

/* 18-DEC-2006 sla: Improvement to complete internal procedure names */
RUN extractWordN (-1  /* word's position */
                 ,cLine
                 ,iCursorChar /* to cursor pos */
                 ,"allowEmptyWord,doNotPassCursor"
                 ,OUTPUT cPreviousWord)
                NO-ERROR.

EMPTY TEMP-TABLE ttFreeList.

/* If RUN keyword is the previous word, then do this or nothing */
IF glIntProcCompletion AND cPreviousWord = "RUN" THEN DO:
    RUN prepareProcList IN TARGET-PROCEDURE (phEditor, cWordBeforeCarret, INPUT-OUTPUT TABLE ttFreeList).
    IF CAN-FIND(FIRST ttFreeList) THEN DO:
        RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).

        IF VALID-HANDLE(hEditorListProc) THEN DO:  /* invalid if it decided to kill itself */
            RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty").
            RETURN "noApply".
        END.
    END.
END.

IF lReturnIfTrue OR TRIM(cWordBeforeCarret) = "" THEN RETURN.


/* This one has priority against all */
IF glCustomCompletionOnTab
 AND CAN-FIND(FIRST ttCustAlias WHERE cWordBeforeCarret MATCHES ttCustAlias.cAlias) THEN DO:
    RUN prepareAliasList (phEditor, cWordBeforeCarret, OUTPUT TABLE ttFreeList).
    IF CAN-FIND(FIRST ttFreeList) THEN DO:
        RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).

        IF VALID-HANDLE(hEditorListProc) THEN DO:  /* invalid if it decided to kill itself */
            RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "aliasMode,exitWhenEmpty").
            RETURN "noApply".
        END.
    END.
END.

/* Starting from this point, various type of guys will be added to a ttFreeList Temp-table */
IF glVariableCompletion
 AND (   CAN-FIND(FIRST ttVar WHERE ttVar.cVar BEGINS cWordBeforeCarret)
      OR CAN-FIND(FIRST ttgVar WHERE ttgVar.hEditor = phEditor AND ttgVar.cVar BEGINS cWordBeforeCarret) )
 THEN DO:
    RUN prepareVarList IN TARGET-PROCEDURE (cWordBeforeCarret, INPUT-OUTPUT TABLE ttFreeList).
    RUN prepareGVarList IN TARGET-PROCEDURE (phEditor, cWordBeforeCarret, INPUT-OUTPUT TABLE ttFreeList).
END.

IF glIntFuncCompletion THEN RUN prepareFuncMethList IN TARGET-PROCEDURE (phEditor, cWordBeforeCarret, INPUT-OUTPUT TABLE ttFreeList).

/* popup content of ttFreeList is not empty */
IF CAN-FIND(FIRST ttFreeList) THEN DO:
    RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).

    IF VALID-HANDLE(hEditorListProc)  /* invalid if it decided to kill itself */
     THEN RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty").
    RETURN "NoApply".
END.

/* if previous word is a database name, then take it into acount */
IF   cWordBeforeCarret > ""
 AND iCursorChar - LENGTH(cWordBeforeCarret) > 2 /* avoid errors in the next SUBSTRING */
 AND SUBSTRING(cLine, iCursorChar - LENGTH(cWordBeforeCarret) - 1, 1 ) = "."
 AND KEYWORD-ALL(cPreviousWord) = ? /* perhaps not necessary */
 THEN cDatabaseName = cPreviousWord.

/* Table name Completion */
IF glCompleteTableOnFLA THEN DO:
    RUN getBufferByFLA IN TARGET-PROCEDURE (cWordBeforeCarret, cDatabaseName, OUTPUT cBufferList) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN "".
    IF cBufferList > ""
     /* 19-JAN-2007 sla: improvement when FLA is not well defined as equal to table-name */
     AND cWordBeforeCarret <> cBufferList THEN DO:
        cWordBeforeCarret = "". /* This cWordBeforeCarret is used later to insert only the remaining part of a buffer name
                               which does not work with FLA that should be just replaced */
        DEFINE VARIABLE iWordsToDelete AS INTEGER     NO-UNDO.
        DO iWordsToDelete = 1 TO NUM-ENTRIES(cWordBeforeCarret, "_"):
            phEditor:SOURCE-COMMAND('delete-prev-word', '').
        END.
    END.
END.

IF cBufferList = "" THEN DO:
    RUN getBufferList IN TARGET-PROCEDURE (phEditor, cWordBeforeCarret, cDatabaseName, OUTPUT cBufferList) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN "".
END.

/* 19-JAN-2007 sla: nothing to do if cWordBeforeCarret = cBufferList  => actually, normal tab */
IF cWordBeforeCarret = cBufferList THEN RETURN.

CASE NUM-ENTRIES(cBufferList):
    WHEN 0 THEN cMon:SCREEN-VALUE IN FRAME fMain = cMon:SCREEN-VALUE + "~nNo Valid buffer name".
    WHEN 1 THEN DO:
        cMon:SCREEN-VALUE IN FRAME fMain = cMon:SCREEN-VALUE + "~nOne single buffer name:" + cBufferList.
        IF LENGTH(cBufferList) > LENGTH(cWordBeforeCarret) THEN DO:
            cMon:SCREEN-VALUE = "cWordBeforeCarret: " + cWordBeforeCarret + "  cBufferList:" + cBufferList
                               + "  to insert:" + LC(SUBSTRING(cBufferList, LENGTH(cWordBeforeCarret) + 1)).
            phEditor:INSERT-STRING(LC(SUBSTRING(cBufferList, LENGTH(cWordBeforeCarret) + 1))).
            RUN addNoLockIfAppropriate IN TARGET-PROCEDURE (phEditor).
            RETURN "noApply".
        END.
    END. /* one single buffer name */
    OTHERWISE DO:
        RUN sortBufferList (INPUT-OUTPUT cBufferList, phEditor:CURSOR-LINE).

        RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).
        RUN loadTableList IN hEditorListProc (cBufferList, "prefix=" + cWordBeforeCarret).
        RETURN "noApply".
    END.
END CASE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trackEditor C-Win
PROCEDURE trackEditor :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ttEdt FOR ttEdt.

DEFINE BUFFER ttMark          FOR ttMark.
DEFINE BUFFER cleanupttEdt    FOR ttEdt.
DEFINE BUFFER ttProc          FOR ttProc.
DEFINE BUFFER ttFunc          FOR ttFunc.
DEFINE BUFFER ttMethod        FOR ttMethod.
DEFINE BUFFER ttReferedBuffer FOR ttReferedBuffer.

/* 07-JUN-2007 sla: take the opportunity to cleanup ressources of files that have been closed */
FOR EACH cleanupttEdt:
    IF VALID-HANDLE(cleanupttEdt.heditor)
     AND cleanupttEdt.heditor:TYPE = "EDITOR"
     AND cleanupttEdt.heditor:SOURCE-EDITOR THEN NEXT.

    RUN clearGlobalResources IN ghParser (cleanupttEdt.heditor).
    DELETE cleanupttEdt.
END.

ghCurrentEditor = ttEdt.hEditor.

/* 02-NOV-2009 sla: better handling of customTriggerProc */
IF VALID-HANDLE(gshprocEditorTriggers) THEN RUN killProcedure IN gshprocEditorTriggers.
RUN protools/abhack/procEditorTriggers.p  PERSIST SET gshprocEditorTriggers (ttEdt.hEditor).

ttEdt.iCursorLine = ttEdt.hEditor:CURSOR-LINE. /* 23-MAY-2007 sla: load only once */
ttEdt.iNumLines   = ttEdt.hEditor:NUM-LINES.

IF  ttEdt.lManageableSections = NO
 OR glSectionLayoutIgnoreStructProcs = NO
 THEN PUBLISH "ListSections" (ttEdt.hEditor).

RUN bedtBrowseSynch (ttEdt.hEditor).
RUN loadLocalResources (ttEdt.hEditor).

cMon:CURSOR-OFFSET IN FRAME {&FRAME-NAME} = cMon:LENGTH + 1 NO-ERROR. /*to insert at the end*/
cMon:INSERT-STRING( "~n" + STRING(TIME, "hh:mm:ss") + " back to hEditor " + STRING(ttEdt.hEditor)).
RUN manageMonitorEdtColor (BUFFER ttEdt).

PUBLISH "abhackFindTrackThis" (ttEdt.hEditor).

RUN setCtrlFAction (ttEdt.hEditor).

ttEdt.cIgnoreValueChanged = "JustLoadedFile".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateEdtFileName C-Win
PROCEDURE updateEdtFileName :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ttEdt FOR ttEdt.

ASSIGN  /* reset and refresh  */
 ttEdt.cFileName     = ""
 ttEdt.cFullPathName = ""
 ttEdt.dLastSaveOpenTime = TIME + (TODAY - 06/01/2007) * 3600 * 24 /* kind of dateTime for Progress 9 */
 ttEdt.cLastSaveOpenTime = STRING(TIME, "hh:mm:ss").

RUN getEditorFileName (ttEdt.hEditor, OUTPUT ttEdt.cFullPathName) NO-ERROR.
IF ERROR-STATUS:ERROR OR ttEdt.cFullPathName = "" THEN DO:
    ttEdt.cFileName = ttEdt.hEditor:WINDOW:TITLE. /* probably "untitled " */
    IF INDEX(ttEdt.cFileName, " - ") > 0
     THEN ttEdt.cFileName = SUBSTRING(ttEdt.cFileName, INDEX(ttEdt.cFileName, " - ") + 3).
    RETURN.
END.

ttEdt.cFullPathName = REPLACE(ttEdt.cFullPathName, "\", "/"). /* *nix rules */
ttEdt.cFileName = ENTRY(NUM-ENTRIES(ttEdt.cFullPathName, "/"), ttEdt.cFullPathName, "/") NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valueChanged C-Win
PROCEDURE valueChanged :
/*------------------------------------------------------------------------------
  Purpose:  This event procedure will try to fire completion by its own without
  pressing tab on ANY-PRINTABLE, BACKSPACE and DELETE-CHARACTER,
  Note the real VALUE-CHANGED event has not been implemented into the editor
  widget in source-editor mode, so I cannot easily catch mouse action.
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE     NO-UNDO.

DEFINE VARIABLE cBlockLabels               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBufferList                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cClassType                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDatabaseName              AS CHARACTER   NO-UNDO. /* MAR-15-2007 sla: support of database name for table completion */.
DEFINE VARIABLE cFilePattern               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLibFile                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMagicRunExternal          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNextNextWord              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNextWord                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPopupOptn                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPreviousWord              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrevPrevWord              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrevPrevWordDynFunc       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWordBeforeCarret          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hEditorListProc            AS HANDLE      NO-UNDO.
DEFINE VARIABLE iCursorChar                AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCursorLine                AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRunWord                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iStartEtime                AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWord                      AS INTEGER     NO-UNDO.
DEFINE VARIABLE lPopupHandlesValueChanged  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lReturnIfTrue              AS LOGICAL     NO-UNDO.

IF NOT glAutoComplete THEN RETURN.

DEFINE BUFFER ttEdt        FOR ttEdt.
DEFINE BUFFER ttAbortCompl FOR ttAbortCompl. /* 03-AUG-2007 sla: used only in a CAN-FIND, but the point is to not make this buffer appear in the GLOBAL scoped buffers list */
DEFINE BUFFER ttCustAlias  FOR ttCustAlias.
DEFINE BUFFER ttFreeList   FOR ttFreeList.
DEFINE BUFFER ttgVar       FOR ttgVar.
DEFINE BUFFER ttVar        FOR ttVar.
DEFINE BUFFER ttModified   FOR ttModified.
DEFINE BUFFER ttLibHandle  FOR ttLibHandle.
DEFINE BUFFER ttgLibHandle FOR ttgLibHandle.
DEFINE BUFFER tttt         FOR tttt.

FIND ttEdt WHERE ttEdt.hEditor = phEditor NO-ERROR. /* 14-SEP-2007 sla: no-error added as suggested by Jan keirse... */
IF NOT AVAILABLE ttEdt THEN RETURN.  /* ... important when people switch fast to an editor that has not been registered yet */


/* 02-SEP-2007 sla: moved the window title job here, as well as the maintenance of bold in the browse */
IF ttEdt.hEditor:MODIFIED
 AND NOT ttEdt.hWin:TITLE BEGINS " **  "
 THEN DO:
    ASSIGN
     ttEdt.hWin:TITLE = " **  " + ttEdt.hWin:TITLE
     ttEdt.cWinTitle = ttEdt.hWin:TITLE.
    IF NOT CAN-FIND(ttModified WHERE ttModified.hEditor = phEditor)
     THEN bedtBrowse:REFRESH() IN FRAME {&FRAME-NAME}.
END.

ELSE IF ttEdt.hEditor:MODIFIED = NO
 AND ttEdt.hWin:TITLE BEGINS " **  "
 THEN DO:
    ASSIGN
     ttEdt.hWin:TITLE = SUBSTRING(ttEdt.hWin:TITLE, 5)
     ttEdt.cWinTitle = ttEdt.hWin:TITLE.
    IF CAN-FIND(ttModified WHERE ttModified.hEditor = phEditor)
     THEN bedtBrowse:REFRESH() IN FRAME {&FRAME-NAME}.
END.

/* Trick to disable a possible popup in some circumstances, like when just choosing an
 item in a popup, or when pressing dot, which both result in a valueChanged situation but
 have already their own special handling */
IF ttEdt.cIgnoreValueChanged > "" THEN DO:
    ttEdt.cIgnoreValueChanged = "".
    RETURN.
END.


ASSIGN
 iCursorChar = phEditor:CURSOR-CHAR
 iCursorLine = phEditor:CURSOR-LINE.
IF iCursorChar = 1 THEN RETURN.

RUN getEditorLine IN TARGET-PROCEDURE (phEditor, OUTPUT cLine).
/* 07-DEC-2006 sla: do not handle value changed if a popup list is already doing this job */
/* 09-JAN-2007 sla: but if just type 'RUN' .p' then kill the running instance internal procedure
 to create a new one for external procedures  */
PUBLISH "IsPopupHandlingValueChanged" (phEditor, INPUT-OUTPUT lPopupHandlesValueChanged).
IF lPopupHandlesValueChanged THEN RETURN.

/* 27-JAN-2007 sla: new feature to disable popup when the caret is in a quoted string */
IF glNoCompletionInStrings
 AND carretInQuotedString(cLine, iCursorChar)
 AND (   iCursorChar < 19
      OR SUBSTRING(cLine, iCursorChar - 18, 18) <> "DYNAMIC-FUNCTION('")
 THEN DO:
    cmon:SCREEN-VALUE IN FRAME fMain  = STRING(TIME, "hh:mm:ss") + " caret in quoted string, do not popup completions! ".
    RETURN.
END.

/* 20-OCT-2008 sla: nice guys asked to disable completion in a commented string (but agreed it would be limited to comments that start and end in a single line) */
IF glNoCompletionInComment
 AND carretInComment(cLine, iCursorChar)
 THEN DO:
    cmon:SCREEN-VALUE IN FRAME fMain  = STRING(TIME, "hh:mm:ss") + " caret in comment that begins and ends in this line, do not popup completions! ".
    RETURN.
END.

/* little improvement if we type RUN .p or RUN .w => put the carret 2 cols backward */
IF  cLine MATCHES "*RUN ~~~.p"
 OR cLine MATCHES "*RUN ~~~.w"
 THEN cMagicRunExternal = SUBSTRING(cLine, iCursorChar - 6) NO-ERROR.

IF cLine MATCHES "*&PLIP*=*'~~~.p'"
 THEN cMagicRunExternal = SUBSTRING(cLine, iCursorChar - 3, 3) NO-ERROR.

IF cLine MATCHES "*~{~~~.i*"
 THEN cMagicRunExternal = SUBSTRING(cLine, iCursorChar - 3, 3) NO-ERROR.

IF  cMagicRunExternal MATCHES "RUN ~~~.."
 OR cMagicRunExternal = "'.p"
 OR cMagicRunExternal = "~{.i"
 THEN ASSIGN
  iCursorChar = iCursorChar - 2
  phEditor:CURSOR-CHAR = iCursorChar.

RUN extractWordN (2  /* word's position */
                 ,cLine
                 ,iCursorChar /* to cursor pos */
                 ,"/PartOfWords,:PartOfWords"
                 ,OUTPUT cNextNextWord)
                NO-ERROR.

RUN extractWordN (1  /* word's position */
                 ,cLine
                 ,iCursorChar /* to cursor pos */
                 ,"/PartOfWords,:PartOfWords"
                 ,OUTPUT cNextWord)
                NO-ERROR.

/* 18-DEC-2006 sla: performance improvement, as we were calling getEditorLine, we'd better call extractWordN than getPrevWord that would call getEditorLine again */
RUN extractWordN (0  /* word's position */
                 ,cLine
                 ,iCursorChar /* to cursor pos */
                 ,"allowEmptyWord,doNotPassCursor,/PartOfWords,:PartOfWords" /* 09-JAN-2007 sla: added :PartOfWords to support windoze ful path for external proc popup */
                 ,OUTPUT cWordBeforeCarret)
                NO-ERROR.
lReturnIfTrue = ERROR-STATUS:ERROR.


/* 08-DEC-2008 sla: better support of dbname.buffer as one single word */
RUN extractWordN (-1  /* word's position */
                 ,cLine
                 ,iCursorChar /* to cursor pos */
                 ,"periodPartOfWords,allowEmptyWord,doNotPassCursor,/PartOfWords,:PartOfWords" /* 09-JAN-2007 sla: added :PartOfWords to support windoze full path for external proc popup */
                 ,OUTPUT cPreviousWord)
                NO-ERROR.

/* 16-DEC-2008 sla: fixed possible error 1016 */
DEFINE VARIABLE lTestConnectedDB AS LOGICAL     NO-UNDO.
lTestConnectedDB = CONNECTED(ENTRY(1, cPreviousWord, ".")) NO-ERROR.

/* yes we are potentially facing a dbName.buffer */
IF NUM-ENTRIES(cPreviousWord, ".") > 1
 AND lTestConnectedDB
 THEN RUN extractWordN (-2  /* word's position */
                       ,cLine
                       ,iCursorChar /* to cursor pos */
                       ,"periodPartOfWords,allowEmptyWord,doNotPassCursor,/PartOfWords,:PartOfWords" /* 09-JAN-2007 sla: added :PartOfWords to support windoze full path for external proc popup */
                       ,OUTPUT cPrevPrevWord)
                      NO-ERROR.
ELSE DO:
    /* 18-DEC-2006 sla: Improvement to complete internal procedure names */
    RUN extractWordN (-1  /* word's position */
                     ,cLine
                     ,iCursorChar /* to cursor pos */
                     ,"allowEmptyWord,doNotPassCursor,/PartOfWords,:PartOfWords" /* 09-JAN-2007 sla: added :PartOfWords to support windoze full path for external proc popup */
                     ,OUTPUT cPreviousWord)
                    NO-ERROR.

    RUN extractWordN (-2  /* word's position */
                     ,cLine
                     ,iCursorChar /* to cursor pos */
                     ,"allowEmptyWord,doNotPassCursor,/PartOfWords,:PartOfWords" /* 09-JAN-2007 sla: added :PartOfWords to support windoze full path for external proc popup */
                     ,OUTPUT cPrevPrevWord)
                    NO-ERROR.
END.


RUN extractWordN (-2  /* word's position */
                 ,cLine
                 ,iCursorChar /* to cursor pos */
                 ,"allowEmptyWord,doNotPassCursor,(PartOfWords,SingleQuotePartOfWords,DoubleQuotePartOfWords" /* 09-JAN-2007 sla: added :PartOfWords to support windoze full path for external proc popup */
                 ,OUTPUT cPrevPrevWordDynFunc)
                NO-ERROR.

cmon:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME, "hh:mm:ss") + " value-changed "
 + " wordBeforeCarret:" + QUOTER(cWordBeforeCarret)
 + " previousWord:" + QUOTER(cPreviousWord)
 + " PrevPrevWord :" + QUOTER(cPrevPrevWord).
 /* use that for debugging if necessary
  + " phEditor:LENGTH: " + QUOTER(phEditor:LENGTH) + " ttEdt.iLength: " + QUOTER(ttEdt.iLength). */

IF CAN-FIND(ttAbortCompl WHERE ttAbortCompl.cForThatWord = cWordBeforeCarret) THEN DO:
    cmon:SCREEN-VALUE = cmon:SCREEN-VALUE + "~n@abortCompletionProcess@ word => abort".
    PUBLISH "KillEditorList" FROM THIS-PROCEDURE (phEditor).
    RETURN.
END.

EMPTY TEMP-TABLE ttFreeList.

/* 26-AUG-2007 sla: provide list of libraries handles after RUN IN */
IF   glRunInLibComp
 AND cPreviousWord = "IN"
 AND cPrevPrevWord = "RUN" THEN DO:
    RUN prepareLibList IN TARGET-PROCEDURE (phEditor, INPUT-OUTPUT TABLE ttFreeList).

    IF CAN-FIND(FIRST ttFreeList) THEN DO:
        RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).

        IF VALID-HANDLE(hEditorListProc)  /* invalid if it decided to kill itself */
         THEN RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty,insertSpaceAfterPrevRunOnChoose").
        RETURN.
    END.
END.

/* 04-JUN-2008 sla: provide list of libraries handles after DYNAMIC-FUNCTION('' IN) */
IF   glRunInLibComp
 AND cPreviousWord = "IN"
 AND cPrevPrevWordDynFunc = "DYNAMIC-FUNCTION(''"
  THEN DO:
    RUN prepareLibList IN TARGET-PROCEDURE (phEditor, INPUT-OUTPUT TABLE ttFreeList).

    IF CAN-FIND(FIRST ttFreeList) THEN DO:
        RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).

        IF VALID-HANDLE(hEditorListProc)  /* invalid if it decided to kill itself */
         THEN RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty,goAfterDynFuncOnChoose").
        RETURN.
    END.
END.


/* 27-AUG-2007 sla: if "RUN %\c IN <ValidLibHandle>" then propose procedures of this library  */
/* IF cPreviousWord = "RUN" */
IF cLine MATCHES "*RUN *IN*"
 AND cWordBeforeCarret = "" /* don't fire if we are just after the 'RUN' word, but let the user type space */
 AND cNextWord = "IN" THEN DO:
    FIND LAST ttLibHandle WHERE ttLibHandle.cVar = RIGHT-TRIM(cNextNextWord, ".") NO-ERROR.
    IF AVAILABLE ttLibHandle THEN cLibFile = ttLibHandle.cLibFile.
    ELSE DO:
        FIND LAST ttgLibHandle WHERE
             ttgLibHandle.hEditor = phEditor
         AND ttgLibHandle.cVar = cNextNextWord
         /* actaully, very hard to guess ...  AND ttgLibHandle.iLine < ttEdt.iCursorLine */
         NO-ERROR.
        IF AVAILABLE ttgLibHandle THEN cLibFile = ttgLibHandle.cLibFile.
    END.
    IF cLibFile > "" THEN DO:
        iStartEtime = ETIME.

        RUN prepareExtProcList (phEditor, cLibFile, OUTPUT TABLE ttFreeList).
        cmon:CURSOR-OFFSET = cmon:LENGTH.
        IF NOT CAN-FIND(FIRST ttFreeList) THEN DO:
            cmon:INSERT-STRING("~nNo Procedure found in external procedure " +  QUOTER(cLibFile)).
            RETURN.
        END.

        RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).
        RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty,InsertYourOwnParamIfProc").
        cmon:INSERT-STRING("~nResource prepared in " + STRING(ETIME - iStartEtime) + "ms"
                           + " for external procedure " + QUOTER(cLibFile)).
        RETURN.
    END.
END.

/* 04-JUN-2008 sla: support of DYNAMIC-FUNCTION('' IN hSomeLibrary ) */
IF cLine MATCHES "*DYNAMIC-FUNCTION('' IN*"
 AND cNextWord = "IN" THEN DO:
    FIND LAST ttLibHandle WHERE ttLibHandle.cVar = RIGHT-TRIM(RIGHT-TRIM(cNextNextWord, ")")) NO-ERROR.
    IF AVAILABLE ttLibHandle THEN cLibFile = ttLibHandle.cLibFile.
    ELSE DO:
        FIND LAST ttgLibHandle WHERE
             ttgLibHandle.hEditor = phEditor
         AND ttgLibHandle.cVar = cNextNextWord
         /* actaully, very hard to guess ...  AND ttgLibHandle.iLine < ttEdt.iCursorLine */
         NO-ERROR.
        IF AVAILABLE ttgLibHandle THEN cLibFile = ttgLibHandle.cLibFile.
    END.
    IF cLibFile > "" THEN DO:
        iStartEtime = ETIME.

        RUN prepareExtFuncList (phEditor, cLibFile, OUTPUT TABLE ttFreeList).
        cmon:CURSOR-OFFSET = cmon:LENGTH.
        IF NOT CAN-FIND(FIRST ttFreeList) THEN DO:
            cmon:INSERT-STRING("~nNo Function found in external procedure " +  QUOTER(cLibFile)).
            RETURN.
        END.

        RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).
        RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty,InsertYourOwnParamIfFunc").
        cmon:INSERT-STRING("~nResource prepared in " + STRING(ETIME - iStartEtime) + "ms"
                           + " for external procedure " + QUOTER(cLibFile)).
        RETURN.
    END.
END.



/* work out cFilePattern to guess if run internal or external  */
iRunWord = LOOKUP("RUN", cLine, " ").
IF cPreviousWord = "RUN" AND iRunWord > 0
 THEN cFilePattern = ENTRY(iRunWord + 1, cLine, " ") NO-ERROR.

/* 10-JAN-2007 sla: support of external procedures popup for launch.i with &PLIP */
IF cLine MATCHES "*&PLIP*=*'*~~~.p'*"
 AND SUBSTRING(cLine, iCursorChar) MATCHES "*~~~.p'*" /* carret is indeed in the filename */
 THEN RUN extractWordN (0  /* word's position */
                       ,cLine
                       ,iCursorChar /* to cursor pos */
                       ,"periodPartOfWords,/PartOfWords,:PartOfWords," /* 09-JAN-2007 sla: added :PartOfWords to support windoze ful path for external proc popup */
                       ,OUTPUT cFilePattern)
                       NO-ERROR.

IF cLine MATCHES "*~{*~~~.i*"
 AND SUBSTRING(cLine, iCursorChar) MATCHES "*~~~.i*" /* carret is indeed in the filename */
 THEN DO:
    RUN extractWordN (0  /* word's position */
                     ,cLine
                     ,iCursorChar /* to cursor pos */
                     ,"periodPartOfWords,/PartOfWords,:PartOfWords," /* 09-JAN-2007 sla: added :PartOfWords to support windoze ful path for external proc popup */
                     ,OUTPUT cFilePattern)
                     NO-ERROR.
    cFilePattern = LEFT-TRIM(cFilePattern, "~{").
    cFilePattern = RIGHT-TRIM(cFilePattern, "~}").
END.
IF cFilePattern > "" THEN cMon:SCREEN-VALUE = cMon:SCREEN-VALUE + " cFilePattern=" + QUOTER(cFilePattern).


/* if file pattern is not OK, then empty it */
IF   NOT cFilePattern MATCHES "*~~~.p"
 AND NOT cFilePattern MATCHES "*~~~.w"
 AND NOT cFilePattern MATCHES "*~~~.i"
 THEN cFilePattern = "".

/* If RUN keyword is the previous word, then manage internal proc list if appropriate */
IF   cPreviousWord = "RUN"
 AND glIntProcCompletion
 AND cFilePattern = ""
 AND cWordBeforeCarret = "" /* 19-NOV-2007 sla: make sure there is a space after RUN. this condition was missing, and could lead to unwanted and unexepcted completions with an IP list containing 'run' */
 THEN DO:
    RUN prepareProcList IN TARGET-PROCEDURE (phEditor, cWordBeforeCarret, INPUT-OUTPUT TABLE ttFreeList).

    IF CAN-FIND(FIRST ttFreeList) THEN DO:
        RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).

        IF VALID-HANDLE(hEditorListProc)  /* invalid if it decided to kill itself */
         THEN RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty,ProcedureMode").
        RETURN.
    END.
END.

/* If cFilePattern > "" then we have a RUN or launch &PLIP so manage external proc list if appropriate */
IF glExtProcCompletion AND cFilePattern > "" THEN DO:
    /* first kill the runng guy */
    PUBLISH "KillEditorList" FROM THIS-PROCEDURE (phEditor).

    /* if '*' not present in the pattern, then add it before the '.' */
    IF LOOKUP(cFilePattern, "*.p,*.w,*.i") = 0
     AND INDEX(cFilePattern, "*") = 0
     THEN cFilePattern = REPLACE(cFilePattern, ".", "*.").

    RUN prepareFileList IN TARGET-PROCEDURE
     (cFilePattern, "2OSFile", INPUT-OUTPUT TABLE ttFreeList).

    cFilePattern = REPLACE(cFilePattern, "*.", ".").
    IF INDEX(cFilePattern, "/") > 0 THEN cFilePattern = SUBSTRING(cFilePattern, R-INDEX(cFilePattern, "/") + 1).

    FIND FIRST ttFreeList WHERE ttFreeList.cItem <> cFilePattern
     /* AND NOT ttfreeList.cItem MATCHES "*" + "/" */ NO-ERROR.
    IF AVAILABLE ttFreeList THEN DO:
        /* 06-JAN-2007 sla: let ABHack spy the editor and completely repopulate the list so
           it can also handle external procedures */
        THIS-PROCEDURE:ADM-DATA = (IF THIS-PROCEDURE:ADM-DATA = ? THEN "" ELSE THIS-PROCEDURE:ADM-DATA)
                                   + ",EditorListShouldNotSpyEditor".
        RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).
        THIS-PROCEDURE:ADM-DATA = REPLACE(THIS-PROCEDURE:ADM-DATA, ",EditorListShouldNotSpyEditor", "").

        IF VALID-HANDLE(hEditorListProc)  /* invalid if it decided to kill itself */
         THEN RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty,ProcedureMode,LeaveCurlyBracket,ExternalProc").
        RETURN.
    END.
    ELSE EMPTY TEMP-TABLE ttFreeList. /* their was one single item = word already after run */
END.


/* 13-JAN-2008 sla: support completion for preprocessor names */
IF    glPreprocessors
 AND (   cWordBeforeCarret BEGINS "~{&"
      OR     cPreviousWord = "&UNDEFINE" /* add other conditions to make sure the is a space after the "&UNDEFINE" */
         AND cWordBeforeCarret = "")
 THEN DO:
    cWordBeforeCarret = SUBSTRING(cWordBeforeCarret, INDEX(cWordBeforeCarret, "~{&") + 2).
    RUN preparePreprocList IN TARGET-PROCEDURE (cWordBeforeCarret
                                               ,phEditor
                                               ,INPUT-OUTPUT TABLE ttFreeList).
    IF CAN-FIND(FIRST ttFreeList) THEN DO:
        RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).
        IF VALID-HANDLE(hEditorListProc)  /* invalid if it decided to kill itself */
         THEN RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty,preprocessorMode").
        RETURN.
    END.
END.


/* 16-NOV-2007 sla: propose no-lock or exclusive lock as appropriate */
IF glAddNoLock
 AND CAN-DO("FIND,FIRST,PREV*,LAST,NEXT,EACH", cPrevPrevWord)
 AND SUBSTRING(cLine, 1, iCursorChar - 1) MATCHES "* " /* end with space */
    /* 01-DEC-2006 sla: do not add NO-LOCK if already in the line */
 AND INDEX(cLine, "-LOCK") = 0
 AND INDEX(cLine, "EXCLUSIVE") = 0
 /* 28-NOV-2006 sla: do not add NO-LOCK for a temp-table */
 AND NOT CAN-FIND(FIRST tttt WHERE tttt.hEditor = phEditor AND tttt.cttname = cPreviousWord)
 THEN DO:
    /* 16-NOV-2007 sla: now check that the previous is indeed a valid buffer name */
    DEFINE VARIABLE hTryBuffer AS HANDLE     NO-UNDO.
    CREATE WIDGET-POOL.

    hTryBuffer = createTTorDBBuffer(phEditor
                                   ,cPreviousWord
                                   ,"ignoreIndices").

    IF VALID-HANDLE(hTryBuffer) THEN DO:
        IF hTryBuffer:DBNAME <> "PROGRESST" THEN DO:
            /* 16-NOV-2007 sla: better... now propose, don't push */
            /* iphEditor:INSERT-STRING(IF glLowerCaseCompletion THEN " no-lock" ELSE " NO-LOCK").  */
            /* RUN valueChanged (iphEditor).                                                       */
            EMPTY TEMP-TABLE ttFreeList.
            CREATE ttFreeList.
            ttFreeList.cItem = "NO-LOCK".
            ttFreeList.cSortOption = "1".
            IF glLowerCaseCompletion THEN ttFreeList.cItem = LC(ttFreeList.cItem).

            CREATE ttFreeList.
            ttFreeList.cItem = "EXCLUSIVE".
            ttFreeList.cSortOption = "2".
            IF glLowerCaseCompletion THEN ttFreeList.cItem = LC(ttFreeList.cItem).

            RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).

            IF VALID-HANDLE(hEditorListProc)  /* invalid if it decided to kill itself */
             THEN RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "aliasMode,exitWhenEmpty").
            RETURN.
        END. /* IF hTryBuffer:DBNAME <> "PROGRESST" THEN DO: */
    END.
    DELETE WIDGET-POOL.
END.


/* 16-NOV-2007 sla:  If prevWord matches *-LOCK and glAddTableAfterWhere, then propose 'WHERE' automatically */
IF   glAddWhereAfterBuffer
 AND INDEX(cLine, "WHERE") = 0
 AND cWordBeforeCarret = "" /* means there is a space after the wanted word */
 AND (       CAN-DO("EACH,FIRST,LAST,PREV,NEXT", cPrevPrevWord)
        AND SUBSTRING(cLine, 1, iCursorChar - 1) MATCHES "* "

     OR   (   SUBSTRING(cLine, 1, iCursorChar - 1) MATCHES "*-LOCK "
           OR SUBSTRING(cLine, 1, iCursorChar - 1) MATCHES "*EXCLUSIVE ")
         AND (   INDEX(SUBSTRING(cLine, 1, iCursorChar), "EACH ")  > 0
              OR INDEX(SUBSTRING(cLine, 1, iCursorChar), "FIRST ") > 0
              OR INDEX(SUBSTRING(cLine, 1, iCursorChar), "LAST ")  > 0
              OR INDEX(SUBSTRING(cLine, 1, iCursorChar), "NEXT ")  > 0
              OR INDEX(SUBSTRING(cLine, 1, iCursorChar), "PREV ")  > 0))
 THEN DO:
    EMPTY TEMP-TABLE ttFreeList.
    CREATE ttFreeList.
    ttFreeList.cItem = IF glLowerCaseCompletion THEN "where" ELSE "WHERE".

    RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).

    IF VALID-HANDLE(hEditorListProc)  /* invalid if it decided to kill itself */
     THEN RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "aliasMode,exitWhenEmpty").
    RETURN.
END.



/* 24-AUG-2007 sla: new feature to propose " NEW <classType>" or " CAST(%\c, <classType>" after a "classVar ="*/
IF glClassNewCastAfterEqual
 AND cWordBeforeCarret = "="
 AND cPreviousWord > "" THEN DO:
    FIND FIRST ttVar WHERE ttVar.cVar = cPreviousWord NO-ERROR.
    IF AVAILABLE ttVar AND NOT CAN-DO("{&BasicProgressTypes}", ttVar.cDataType)
     THEN cClassType = ttVar.cDataType.
    ELSE DO:
        FIND FIRST ttgVar WHERE ttgVar.hEditor = phEditor AND ttgVar.cVar = cPreviousWord NO-ERROR.
        IF AVAILABLE ttgVar AND NOT CAN-DO("{&BasicProgressTypes}", ttgVar.cDataType)
         THEN cClassType = ttgVar.cDataType.
    END.

    /* OK, we found a valid class type var right in front of an '=' sign */
    IF cClassType > "" THEN DO:
        EMPTY TEMP-TABLE ttFreeList.
        CREATE ttFreeList.
        ttFreeList.cItem = " NEW " + cClassType + "()". /* 14-SEP-2007 sla: added '()' => later we shall suggest the parameters of a constructor. */
        ttFreeList.cSortOption = "1".
        CREATE ttFreeList.
        ttFreeList.cItem = " CAST(%\c, " + cClassType + ")".
        ttFreeList.cSortOption = "2".

        RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).

        IF VALID-HANDLE(hEditorListProc)  /* invalid if it decided to kill itself */
         THEN RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "aliasMode,exitWhenEmpty").
        RETURN.
    END.

END.

/* 03-MAR-2007 sla: support of block label completion */
IF glCompleteBlockLabels
 AND (   CAN-DO("LEAVE,UNDO,RETRY", cPreviousWord)
      OR cPreviousWord = "NEXT" AND cPrevPrevWord <> "FIND") /* 30-NOV-2007 sla: required if NEXT is part of the gcSuggestBufferFor */
 THEN DO:
    RUN prepareBlockList IN TARGET-PROCEDURE (cWordBeforeCarret, iCursorLine, INPUT-OUTPUT TABLE ttFreeList).

    IF CAN-FIND(FIRST ttFreeList) THEN DO:
        RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).

        IF VALID-HANDLE(hEditorListProc)  /* invalid if it decided to kill itself */
         THEN RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty").
        RETURN.
    END.

END.



/* 22-JAN-2007 sla: New feature to suggest used buffers after a defined list of keywords */
IF   cWordBeforeCarret = "" /* means there is a space after the wanted word */
 AND CAN-DO(gcSuggestBufferFor, cPreviousWord)
 AND (   cPreviousWord <> "NEXT"                               /* 30-NOV-2007 sla: required if NEXT is...*/
      OR cPreviousWord = "NEXT" AND cPrevPrevWord = "FIND" )   /*...  part of the gcSuggestBufferFor */
 THEN DO:
    RUN buildSuggestBufferList IN TARGET-PROCEDURE
     (cPreviousWord
     ,phEditor
     ,iCursorLine
     ,cLine
     ,OUTPUT cBufferList)
     NO-ERROR.

    IF NOT ERROR-STATUS:ERROR AND cBufferList > "" THEN DO:
        cmon:SCREEN-VALUE = cmon:SCREEN-VALUE + "~Suggest buffers: " + QUOTER(cBufferList).
        RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).
        RUN loadTableList IN hEditorListProc (cBufferList, "prefix=" + cWordBeforeCarret + ",exitWhenEmpty").
        RETURN.
    END.
END.

IF lReturnIfTrue OR LENGTH(TRIM(cWordBeforeCarret)) < giAutoCompMinSize THEN RETURN.

/* This one has priority against all */
IF glCustomCompletionOnTab THEN DO:
    IF CAN-FIND(FIRST ttCustAlias WHERE cWordBeforeCarret MATCHES ttCustAlias.cAlias) THEN DO:
        RUN prepareAliasList (phEditor, cWordBeforeCarret, OUTPUT TABLE ttFreeList).
        IF CAN-FIND(FIRST ttFreeList) THEN DO:
            /* 22-DEC-2006 sla: ugly trick, but worth trying to pass a kind of run-value to procEditorList.w
               it will just have to query source-procedure:ADM-DATA */
            THIS-PROCEDURE:ADM-DATA = (IF THIS-PROCEDURE:ADM-DATA = ? THEN "" ELSE THIS-PROCEDURE:ADM-DATA)
                                       + ",EditorListShouldNotSpyEditor".
            RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).
            THIS-PROCEDURE:ADM-DATA = REPLACE(THIS-PROCEDURE:ADM-DATA, ",EditorListShouldNotSpyEditor", "").

            IF VALID-HANDLE(hEditorListProc)  /* invalid if it decided to kill itself */
             THEN RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "aliasMode,exitWhenEmpty").
            RETURN.
        END.
    END. /* IF CAN-FIND(FIRST ttCustAlias WHERE... */

    /* 24-MAR-2007 sla: little improvement, when running in alias mode, a list can remain present although no item
     longer matches the current word */
    ELSE PUBLISH "shouldAliasEditorListBeKilled" (phEditor, cWordBeforeCarret).
END. /* IF glCustomCompletionOnTab  */

/* Starting from this point, various type of guys will be added to a ttFreeList Temp-table */
IF   glVariableCompletion
 AND cWordBeforeCarret <> KEYWORD(cWordBeforeCarret)  /* 25-MAY-2007 sla: don't popup anything if the word before carret is a keyword like 'END' */
 AND (   CAN-FIND(FIRST ttVar WHERE ttVar.cVar BEGINS cWordBeforeCarret)
      OR CAN-FIND(FIRST ttgVar WHERE ttgVar.hEditor = phEditor AND ttgVar.cVar BEGINS cWordBeforeCarret))
 THEN DO:
    RUN prepareVarList IN TARGET-PROCEDURE (cWordBeforeCarret, INPUT-OUTPUT TABLE ttFreeList).
    RUN prepareGVarList IN TARGET-PROCEDURE (phEditor, cWordBeforeCarret, INPUT-OUTPUT TABLE ttFreeList).
END.


IF   glIntFuncCompletion
 AND cWordBeforeCarret <> KEYWORD(cWordBeforeCarret)
 THEN RUN prepareFuncMethList IN TARGET-PROCEDURE (phEditor, cWordBeforeCarret, INPUT-OUTPUT TABLE ttFreeList).

/* popup content of ttFreeList is not empty */
IF CAN-FIND(FIRST ttFreeList) THEN DO:
    RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).
    IF VALID-HANDLE(hEditorListProc)  /* invalid if it decided to kill itself */
     THEN RUN loadListFromTT IN hEditorListProc (TABLE ttFreeList, "exitWhenEmpty,trailingParenthesesMeansFunction").
    RETURN.
END.

/* At this point, we no longer play with the ttFreeList temp-table  */

/* Table name Completion */
IF cBufferList = ""
 AND cWordBeforeCarret <> KEYWORD(cWordBeforeCarret) /* 24-MAY-2007 sla: this addidional condition prevents a popup to come for a table 'ending' when we just type a simpe keyworks like 'END' */
 THEN DO:
    /* if previous word is a database name, then take it into acount */
    IF   cWordBeforeCarret > ""
     AND iCursorChar - LENGTH(cWordBeforeCarret) > 2 /* avoid errors in the next SUBSTRING */
     AND SUBSTRING(cLine, iCursorChar - LENGTH(cWordBeforeCarret) - 1, 1 ) = "."
     AND KEYWORD-ALL(cPreviousWord) = ? /* perhaps not necessary */
     THEN cDatabaseName = cPreviousWord.

    IF glCompleteTableOnFLA THEN
     RUN getBufferByFLA IN TARGET-PROCEDURE (cWordBeforeCarret, cDatabaseName, OUTPUT cBufferList) NO-ERROR.

    IF cBufferList = "" THEN RUN getBufferList IN TARGET-PROCEDURE (phEditor, cWordBeforeCarret,  cDatabaseName , OUTPUT cBufferList) NO-ERROR.
END.

IF cBufferList > ""
 AND cBufferList <> cWordBeforeCarret THEN DO:
    RUN sortBufferList (INPUT-OUTPUT cBufferList, phEditor:CURSOR-LINE).

    RUN protools/abhack/procEditorList.w PERSIST SET hEditorListProc (phEditor).
    RUN loadTableList IN hEditorListProc (cBufferList, "prefix=" + cWordBeforeCarret + ",exitWhenEmpty").
END.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE waitForEditorsKeyStroke C-Win
PROCEDURE waitForEditorsKeyStroke :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE BUFFER ttEdt FOR ttEdt.

btnStopStart:LABEL IN FRAME fMain = "&Start".

ghCurrentEditor = ?.  /* so we will relaunch gshprocEditorTriggers when the timer is stopped */
IF VALID-HANDLE(gshprocEditorTriggers) THEN RUN killProcedure IN gshprocEditorTriggers.
IF VALID-HANDLE(ghABHackSleepUntilPrintable) THEN DELETE PROCEDURE ghABHackSleepUntilPrintable. /* should never happen */
FOR EACH ttEdt:
    RUN protools/abhack/ABHackSleepUntilPrintable.p PERSIST SET ghABHackSleepUntilPrintable (ttEdt.hEditor).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION abReturnValue C-Win
FUNCTION abReturnValue RETURNS CHARACTER
  () :
/*------------------------------------------------------------------------------
  Purpose:  to build a (error) RETURN-VALUE String
            abReturnValue returns:
             a) the current RETURN-VALUE if not empty
             b) OTHERWISE it returns the possible Progress ABL error
                messages contained in the ERROR-STATUS system handle.

    Notes:  In case of non handled errors (like a RUN unexistingProc  without the
    NO-ERROR option), then the Progress ABL errors can't be retrieved from
    ERROR-STATUS System handle.  I could try to retrieve these errors with _msg()
    but I decided to not go that far, since this stack can contain unhandled
    errors that occurred earlier, which could be very confusing.
------------------------------------------------------------------------------*/
    IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.

    DEFINE VARIABLE iError            AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cErrorReturnValue AS CHARACTER  NO-UNDO.


    DO iError = 1 TO ERROR-STATUS:NUM-MESSAGES:
        cErrorReturnValue =  cErrorReturnValue + CHR(10)
                           + ERROR-STATUS:GET-MESSAGE(iError).
    END.
    RETURN LEFT-TRIM(cErrorReturnValue, CHR(10)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION carretInComment C-Win
FUNCTION carretInComment RETURNS LOGICAL
  (pcLine AS CHARACTER
  ,piPos  AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  Return yes when the carret (pipos) is inside a commented string

24-OCT-2008 sla: bug fix, do not replace commentStart and commentEnd by '( ' and ' )'
but rather by CHR(1) + " "  and " " + CHR(2)  so I do not feel inside a comment when
I am just between parentheses
------------------------------------------------------------------------------*/
    pcLine = SUBSTRING(pcLine, 1, MAX(1, piPos)).
    pcLine = REPLACE(pcLine, "/*", CHR(1) + " ").
    pcLine = REPLACE(pcLine, "*/", " " + CHR(2)).
    RETURN NUM-ENTRIES(pcLine, CHR(1)) > NUM-ENTRIES(pcLine, CHR(2)).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION carretInQuotedString C-Win
FUNCTION carretInQuotedString RETURNS LOGICAL
  (pcLine AS CHARACTER
  ,piPos  AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  Return yes when the carret (pipos) is inside a quoted string
    Notes:  Based on code sent by Jayson.   A nice algorythm ;)
------------------------------------------------------------------------------*/
DEFINE VARIABLE lSingleQuote AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lDoubleQuote AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iChar AS INTEGER     NO-UNDO.

IF piPos < 2 THEN RETURN NO.

ASSIGN
 lDoubleQuote = NO
 lSingleQuote = NO.  /* Not in a quoted string */

/* Search for a quotes */
DO iChar = 1 TO piPos - 1:
    IF   SUBSTRING(pcLine, iChar, 1) = '"'
     AND NOT lSingleQuote
     THEN lDoubleQuote = NOT lDoubleQuote.
    IF   SUBSTRING(pcLine, iChar, 1) = "'"
     AND NOT lDoubleQuote
     THEN lSingleQuote = NOT lSingleQuote.
END.

RETURN lSingleQuote OR lDoubleQuote.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cLinzDummy C-Win
FUNCTION cLinzDummy RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  the point of this dummy function is to show that a function name
   can be added to a popup list that has already other type of items (like variables)

   As I have many procedure that have a variable called cLine, then I expect this
   function to be added to the list.
   21-JAN-2007 sla: renamed it cLinzDummy so it no longer pops up when I type the
   very often used "cline" .
------------------------------------------------------------------------------*/

  RETURN "".   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createTTorDBBuffer C-Win
FUNCTION createTTorDBBuffer RETURNS HANDLE
  (phEditor     AS HANDLE
  ,pcBufferName AS CHARACTER
  ,pcOptn       AS CHARACTER) :
/* Create a buffer against TT or DB, based on the buffer name and all the resource we have in memory
   for the particular editor */

DEFINE VARIABLE cBufferName AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hRtnBuffer  AS HANDLE      NO-UNDO.

DEFINE BUFFER ttbuffer  FOR ttbuffer.
DEFINE BUFFER ttgbuffer FOR ttgbuffer.
DEFINE BUFFER tttt      FOR tttt.

cBufferName = pcBufferName. /* 11-MAY-2007 sla: default value  */

/* First try to find a global buffer */
FIND FIRST ttgbuffer NO-LOCK WHERE
     ttgbuffer.hEditor = phEditor
 AND ttgbuffer.cName   = pcBufferName NO-ERROR.
IF AVAILABLE ttgbuffer THEN cBufferName = ttgbuffer.cfor.

/* But the ABL allows you to redefine a local buffer with same name but for another table or tt
 so always give last word to local resource */
FIND FIRST ttbuffer NO-LOCK WHERE ttbuffer.cName = pcBufferName NO-ERROR.
IF AVAILABLE ttbuffer THEN cBufferName = ttbuffer.cfor.

IF NOT AVAILABLE ttbuffer AND NOT AVAILABLE ttgbuffer THEN cBufferName = pcBufferName.


/* At last, perhaps a temp-table wears the same name as cBufferName */
FIND FIRST tttt WHERE
     tttt.hEditor = phEditor
 AND tttt.cttname = cBufferName NO-ERROR.

IF AVAILABLE tttt THEN RUN createTTBuffer (BUFFER tttt, pcBufferName, pcOptn, OUTPUT hRtnBuffer) NO-ERROR.
ELSE DO: /* MAR-15-2007 sla: better support for qualified database name */
    /* 09-MAY-2007 sla: there was still a problem with qualified databaseNames */
    /* 11-MAY-2007 sla: hmmm, solving this problem once and for all
      Note a pcBufferName may have one or not an qualified database name
      a ttbuffer.cfor may also have a qualified database name
      The correct way to solve that is to satt with cBufferName = pcBufferName
      Then resolve cBufferName with the actual table if any ttbuffer or ttgbuffer applies
      Then we will definitely create a buffer for table cBufferName, but the name of the buffer
      will be the name of pcBufferName without qualified database name */

    IF NUM-ENTRIES(cBufferName, ".") > 2 THEN RETURN ?.
    CREATE BUFFER hRtnBuffer FOR TABLE cBufferName BUFFER-NAME ENTRY(NUM-ENTRIES(pcBufferName, "."), pcBufferName, ".") NO-ERROR.
END.

RETURN hRtnBuffer.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION disabledApiTooltip C-Win
FUNCTION disabledApiTooltip RETURNS LOGICAL
  (pcApiTooltip AS CHARACTER ) :

DEFINE VARIABLE cSection  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ckv       AS CHARACTER   NO-UNDO.

LOAD 'Software\PSC\PROGRESS\slacroixTools' BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
/* create it if it does not exist  ... well, would be surprising...*/
IF ERROR-STATUS:ERROR THEN LOAD 'Software\PSC\PROGRESS\slacroixTools' NEW BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN USE 'Software\PSC\PROGRESS\slacroixTools'.

cSection = 'ABHacker'.

GET-KEY-VALUE SECTION cSection KEY 'disabledApiTooltip' + pcApiTooltip VALUE ckv.

/* unload the environment to go back to default env */
UNLOAD 'Software\PSC\PROGRESS\slacroixTools' NO-ERROR.

IF ckv = "YES" THEN RETURN YES.


RETURN NO.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getgiAutoCompMinSize C-Win
FUNCTION getgiAutoCompMinSize RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN giAutoCompMinSize.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getglCompleteTableOnFLA C-Win
FUNCTION getglCompleteTableOnFLA RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN glCompleteTableOnFLA.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getglHypeStrikeMode C-Win
FUNCTION getglHypeStrikeMode RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN glHypeStrikeMode.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getglLowerCaseCompletion C-Win
FUNCTION getglLowerCaseCompletion RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN glLowerCaseCompletion.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPSTimerEnabled C-Win
FUNCTION getPSTimerEnabled RETURNS LOGICAL
 ( ):
   RETURN chCtrlSpy:PSTimerSpy:ENABLED.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION innerTrim C-Win
FUNCTION innerTrim RETURNS CHARACTER
  (cString AS CHARACTER
  ,cTrimChar AS CHARACTER  ) :
/*------------------------------------------------------------------------------
  Purpose:  Remove double CHARACTERs from a string
   example innerTrim("hello  world", "") = "hello word"
    Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cOldString AS CHARACTER  NO-UNDO.

DO WHILE cOldString <> cString :
    cOldString = cString.
    cString = REPLACE(cOldString, cTrimChar + cTrimChar, cTrimChar).
END.

RETURN cString.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isValidBLockLabel C-Win
FUNCTION isValidBLockLabel RETURNS LOGICAL
  (pcBlockLabel AS CHARACTER
  ,pcLine       AS CHARACTER) : /* to take words located before the block label into account */
/*------------------------------------------------------------------------------
  Purpose:  For now, the detection of a block label relies on having a colon at the end of
      of a word without anything afterwards.
      The point of this procedure is to filter out PROGRESS keywords like DO: REPEAT:  etc...

      A valid block label shall not have special characters like ')' or '}' or '.' and should contain
      at least one alpha character.
      Since there might be more special characters to check than the length of the block label
      and that we take care of having at least one alpha char, we will go through the all string
      and check each char one by one.

------------------------------------------------------------------------------*/
DEFINE VARIABLE cChar          AS CHARACTER   CASE-SENSITIVE NO-UNDO.
DEFINE VARIABLE cPreviousWord  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE eWord          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLength        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNumWords      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPos           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWord          AS INTEGER     NO-UNDO.
DEFINE VARIABLE lOneAlphaFound AS LOGICAL     NO-UNDO.

IF pcBlockLabel = "" THEN RETURN FALSE.

/* reserved ABL keywords */ /* 21-MAY-2007 sla: added "Purpose" because it usually appears in header comments of structured sections */
IF CAN-DO("DO,REPEAT,NO-LOCK,EXCLUSIVE*,TRUE,YES,SHARED-LOCK,Purpose" ,pcBlockLabel) THEN RETURN FALSE.

ASSIGN
 iLength   = LENGTH(pcBlockLabel)
 iNumWords = LOOKUP(pcBlockLabel + ":", pcLine, " ") - 1. /* start from word located before the block label */

/* check previous words.  If it does not ends with a full stop, then perhaps our line looks like 'DO iPos = 1 TO iLength:'
where 'iLength' is not a block label */
DO iWord = iNumWords TO 1 BY -1:
    eWord = ENTRY(iWord, pcLine, " ").
    IF eWord = "" THEN NEXT. /* empy words means there was multiple spaces before the block label => ignore them */
    IF eWord MATCHES "*~~~." THEN LEAVE. /* previous word ends with a full stop => OK the word after that could be a valid block label */
    RETURN FALSE.  /* a previous word => the line was w awhere clause or a DO WHILE phrase, anyway not a block label */
END.

/* check each character one by one */
DO iPos = 1 TO iLength:
    cChar = SUBSTRING(pcBlockLabel, iPos, 1).

    /* prohibited character in block label */
    IF INDEX(cChar, ",~"*.()'~}/[]") > 0 THEN RETURN FALSE.

    IF lOneAlphaFound THEN NEXT.
    lOneAlphaFound =   cChar > "A" AND cChar < "Z"
                    OR cChar > "a" AND cChar < "z".
END.

RETURN lOneAlphaFound.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION queryIndexDetails C-Win
FUNCTION queryIndexDetails RETURNS CHARACTER
  (phBuffer AS HANDLE
  ,pcIndex AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  report index details for a given buffer handle, like:
IS UNIQUE PRIMARY
  1  custnum
  2  shippedDate DESCENDING
------------------------------------------------------------------------------*/

DEFINE VARIABLE cIdxInfo      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRtnValue     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iFieldInfo    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iFieldInfoMax AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNumIdx       AS INTEGER     NO-UNDO.


IF NOT VALID-HANDLE(phBuffer) THEN RETURN "invalid buffer handle passed to queryIndexDetails()!".

DO WHILE TRUE:
    ASSIGN
     iNumIdx  = iNumIdx + 1
     cIdxInfo = phBuffer:INDEX-INFORMATION(iNumIdx).

    IF  cIdxInfo = ? THEN LEAVE.

    IF ENTRY(1, cIdxInfo) <> pcIndex THEN NEXT.

    IF ENTRY(2, cIdxInfo) = "1" THEN cRtnValue = cRtnValue + " UNIQUE".
    IF ENTRY(3, cIdxInfo) = "1" THEN cRtnValue = cRtnValue + " PRIMARY".
    IF ENTRY(4, cIdxInfo) = "1" THEN cRtnValue = cRtnValue + " WORD-INDEX".
    IF cRtnValue > "" THEN cRtnValue = "IS" + cRtnValue.

    iFieldInfoMax = NUM-ENTRIES(cIdxInfo).
    /* 13-FEB-2007 sla: Thanks to Jan Keirse ;) */
    IF iFieldInfoMax = 5 THEN RETURN " which contains no fields!".

    DO iFieldInfo = 5 TO iFieldInfoMax BY 2:
         /* 16-APR-2007 sla: added systematic carriage return for first field
        cRtnValue = (IF cRtnValue = "" THEN "" ELSE cRtnValue + "~n   ") /* beware, cRtnValue might contain something at the very first iteration */ */
        cRtnValue = cRtnValue + "~n   " /* beware, cRtnValue might contain something at the very first iteration */
         + STRING((iFieldInfo - 3) / 2 , ">9") + " "
         + ENTRY(iFieldInfo, cIdxInfo)
         + IF ENTRY(iFieldInfo + 1, cIdxInfo) = "0" THEN "" ELSE " DESCENDING".
    END.

    RETURN cRtnValue.
END.

RETURN " Seems to be an invalid index for buffer " + QUOTER(phBuffer:NAME).


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION queryReplaceWord C-Win
FUNCTION queryReplaceWord RETURNS CHARACTER
 (pcPhrase AS CHARACTER
 ,pcWord   AS CHARACTER
 ,pcBy     AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Replace 'r' by '?' so WHERE ROWID does not become  WHE'?'E '?'OWID
    Notes:  It is a bit more complicated than a simple
              RETURN REPLACE(pcPhrase, pcWord, pcBy).
            ... especially if pcWord is shorter than pcBy...
------------------------------------------------------------------------------*/

DEFINE VARIABLE cPhraseSpace AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRtn         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iStart       AS INTEGER    NO-UNDO.
DEFINE VARIABLE iOffset AS INTEGER    NO-UNDO.

cPhraseSpace = REPLACE(pcPhrase, ",", " ").
cPhraseSpace = REPLACE(cPhraseSpace, "(", " ").
cPhraseSpace = REPLACE(cPhraseSpace, ")", " ").
cPhraseSpace = REPLACE(cPhraseSpace, "[", " ").
cPhraseSpace = REPLACE(cPhraseSpace, "]", " ").
cPhraseSpace = REPLACE(cPhraseSpace, "=", " ").

cPhraseSpace = cPhraseSpace + " ".
cRtn = pcPhrase.

DO WHILE TRUE:
    iStart = INDEX(cPhraseSpace, " " + pcWord + " ", iStart + 1).
    IF iStart = 0 THEN LEAVE.

    SUBSTRING(cRtn, iStart + 1 + iOffset, LENGTH(pcWord)) = pcBy.

    iOffset = iOffset + LENGTH(pcBy) - LENGTH(pcWord).
END.

RETURN cRtn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION refinedAlias C-Win
FUNCTION refinedAlias RETURNS CHARACTER
  (pcAliasExp AS CHARACTER  ) :
/*------------------------------------------------------------------------------
  Purpose:  Refine an alias with all supported directives
    Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cCaseSensitiveText AS CHARACTER  CASE-SENSITIVE NO-UNDO.
DEFINE VARIABLE cDate              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSectionName       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUserName          AS CHARACTER   NO-UNDO.

/* 21-JAN-2009 Patrick Tingen: new %\codeblock directive
 * The directive %\codeblock looks at the section and sets the name
 * according to that. If it cannot find anything, it takes the name
 * of the file instead
 */
DEFINE BUFFER ttEdt FOR ttEdt.
FIND FIRST ttEdt WHERE ttEdt.hEditor = ghCurrentEditor NO-ERROR.

IF AVAILABLE ttEdt THEN
DO:
  ASSIGN cFileName = ttEdt.cFileName.
  CASE ttEdt.currentSection:
    WHEN 'main'                             THEN cSectionName = ttEdt.cFileName.
    WHEN 'Definitions' OR WHEN 'Main Block' THEN cSectionName = ttEdt.currentSection.
    WHEN 'Procedures'  OR WHEN 'Functions'  THEN cSectionName = ttEdt.currentEvent.
    WHEN 'Triggers'                         THEN cSectionName = ttEdt.currentEvent + ' OF ' + ttEdt.currentWidget.
    OTHERWISE cSectionName = ttEdt.cFileName.
  END CASE.
END.

/* 2007-07-03 Johan Samyn : custom date format */
/* 05-JUL-2007 sla: revised logic to support many types in a V9 compatible way */
cDate = gcComplDateFormat.
cDate = REPLACE(cDate, "mmm", ENTRY(MONTH(TODAY), "JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC")).
cDate = REPLACE(cDate, "ffff", ENTRY(MONTH(TODAY), "Janvier,Février,Mars,Avril,Mai,Juin,Juillet,Août,Septembre,Octobre,Novembre,Décembre")). /* s'il vous plaît ! */
cDate = REPLACE(cDate, "mm", STRING(MONTH(TODAY), "99")).
cDate = REPLACE(cDate, "dd", STRING(DAY(TODAY), "99")).
cDate = REPLACE(cDate, "yyyy", STRING(YEAR(TODAY), "9999")).
cDate = REPLACE(cDate, "yy", STRING(YEAR(TODAY) MODULO 100, "99")).

cUserName = OS-GETENV("userName").

/* 31-DEC-2006 sla: support of use Lower case, to apply when a key word is 100 caps */
cCaseSensitiveText = REPLACE(REPLACE(pcAliasExp, "%\d", "%\D"), "%\c", "%\C").
IF glLowerCaseCompletion AND CAPS(pcAliasExp) = cCaseSensitiveText
 THEN pcAliasExp = LC(pcAliasExp).

/* 07-AUG-2007 sla: new %\userName directive */
pcAliasExp = REPLACE(pcAliasExp, "%\userName", cUserName).
pcAliasExp = REPLACE(pcAliasExp, "%\fileName", cFileName).

/* 21-JAN-2009 Patrick Tingen: new %\codeblock and %\year directive */
pcAliasExp = REPLACE(pcAliasExp, "%\codeblock", cSectionName).
pcAliasExp = REPLACE(pcAliasExp, "%\year"     , STRING(YEAR(TODAY),'9999')).

pcAliasExp = REPLACE(pcAliasExp, "%\d", cDate).  /* at last this one is not hardcoded with american format */

RETURN pcAliasExp.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeLineComments C-Win
FUNCTION removeLineComments RETURNS CHARACTER
 (pcLine   AS CHARACTER
 ,piCIndex AS INTEGER ):
/*================================================================================
 Function optimized for performances
 Passing 0 to piCIndex means we do not know yet if there is an open comment tag
   in the line.  This function will find it out by itself
 Passing piCIndex > 0 means we know there is a open comment tag at this position

 => The idea is to use iPos = INDEX(c, "/" + "*") only once at the caller level.
    IF iPos is > 0 then we pass it to this function otherwise we do not call it (little performance gain)
================================================================================ */

DEFINE VARIABLE cRtn       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cToReplace AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iClosePos  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNumOpen   AS INTEGER     INITIAL 1 NO-UNDO.
DEFINE VARIABLE iOpenPos   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iStartFrom AS INTEGER     NO-UNDO.

/* if not passed by the caller, then work it out now */
IF piCIndex <= 0 THEN DO:
    piCIndex = INDEX(pcLine, "/*", iClosePos + 1).
    IF piCIndex = 0 THEN RETURN pcLine. /* no comments ;) */
END.

ASSIGN
 cRtn       = pcLine
 iNumOpen   = 1
 iStartFrom = piCIndex + 1.


DO WHILE TRUE:
    ASSIGN
     iClosePos = INDEX(pcLine, "*/", iStartFrom)
     iOpenPos  = INDEX(pcLine, "/*", iStartFrom).

    IF iClosePos = 0 THEN RETURN cRtn. /* missing close comment tag ? */

    IF   iNumOpen > 0
     AND iOpenPos > 0
     AND iOpenPos < iClosePos THEN DO:
        ASSIGN
         iNumOpen   = iNumOpen + 1
         iStartFrom = iOpenPos + 1.
        NEXT.
    END.

    ASSIGN
     iNumOpen   = iNumOpen - 1
     iStartFrom = iClosePos + 1.

    IF iNumOpen > 0 THEN NEXT.

    ASSIGN
     cToReplace = SUBSTRING(pcLine, piCIndex, iClosePos - piCIndex + 2)
     cRtn       = REPLACE(cRtn, cToReplace , "")
     piCIndex   = INDEX(pcLine, "/*", iClosePos + 1).

    IF piCIndex = 0 THEN LEAVE.

    ASSIGN
     iNumOpen   = iNumOpen + 1
     iStartFrom = piCIndex + 1.
END.

RETURN cRtn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION wordBuffer C-Win
FUNCTION wordBuffer RETURNS CHARACTER
  (pcWord AS CHARACTER
  ,pcOptn AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iNumDots AS INTEGER    NO-UNDO.

  IF pcOptn = ? THEN RETURN "?". /* should never happen */

  IF INDEX(pcWord, "/") > 0 OR INDEX(pcWord, " ") > 0 OR INDEX(pcWord, "\") > 0 THEN RETURN ?. /* 19-MAR-2007 sla: fix, avoid error 1016  */

  iNumDots = NUM-ENTRIES(pcWord, ".").

  IF iNumDots = 0 THEN RETURN "".
  IF iNumDots = 1 AND CAN-DO(pcOptn, "noField") THEN RETURN pcWord.
  IF iNumDots = 1 THEN RETURN "". /* a single field name was passed */

  IF iNumDots = 2 AND CAN-DO(pcOptn, "removeDb") AND CAN-DO(pcOptn, "noField") THEN RETURN ENTRY(2, pcWord, "."). /* only dbname.tableName was passed */
  IF iNumDots = 2 AND CAN-DO(pcOptn, "noField")  THEN RETURN pcWord. /* only dbname.tableName was passed, and we want both */
  IF iNumDots = 2 AND CONNECTED(ENTRY(1, pcWord, ".")) THEN RETURN pcWord.  /* MAR-15-2007 sla: little improvement */
  IF iNumDots = 2 THEN RETURN ENTRY(1, pcWord, ".").

  IF iNumDots = 3 AND CAN-DO(pcOptn, "removeDb") THEN RETURN ENTRY(2, pcWord, ".").
  IF iNumDots = 3 AND pcOptn = "" THEN RETURN ENTRY(1, pcWord, ".") + "." + ENTRY(2, pcWord, ".").

  RETURN "WordBuffer() says: Too many dots in this word: " + QUOTER(pcWord) + " with option: " + QUOTER(pcOptn).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION wordField C-Win
FUNCTION wordField RETURNS CHARACTER
  (pcWord AS CHARACTER):
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iNumDots AS INTEGER    NO-UNDO.

  IF pcWord = ? THEN RETURN "?".

  iNumDots = NUM-ENTRIES(pcWord, ".").

  IF iNumDots = 0 THEN RETURN "". /* security */

  RETURN ENTRY(iNumDots, pcWord, "."). /* just return last entry */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

