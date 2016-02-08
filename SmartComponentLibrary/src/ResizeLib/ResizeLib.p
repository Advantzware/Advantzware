&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : ResizeLib.p 
    Purpose     : General method for resize of Progress Windows

    Syntax      :

    Description :

    Author(s)   : Brynjar Hasle, Chemistry as. brynjar@chemistry.no
    Created     : 01.03.2002
    Modified:   : 01.04.2003: Added support for vertical and horizontal split bar
                  New functions: 
                    setSplitBarX: 
                        Called from initializeObject and END-MOVE 
                        of vertical split-bar button
                    setSplitBarY: 
                        Called from initializeObject and END-MOVE 
                        of horizontal split-bar button
                    DoWidgetSplitResize:
                        Called from setSplitBarX and setSplitBarY
                        
                  Also restructured setWidgetResize function and removed 
                  the recursive part of it to function DoWidgetResize
    Notes       : 
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* {JukeBoxControlsGeneral.i}  */
/* {JukeBoxProgressBar.i}      */
DEF VAR iProgBar            AS INT    NO-UNDO.
DEF VAR iSuccess            AS INT    NO-UNDO.
DEF VAR cProgBarState       AS CHAR   NO-UNDO.

DEFINE VARIABLE hMainHelp   AS HANDLE      NO-UNDO.

DEF VAR bOk                 AS LOG NO-UNDO.
DEF VAR ix                  AS INT NO-UNDO.
                            
DEF VAR bScandinavian       AS LOG NO-UNDO.
DEF VAR bDebug              AS LOG NO-UNDO.   
                            
DEF VAR iDeltaX             AS INT NO-UNDO.
DEF VAR iDeltaY             AS INT NO-UNDO.
DEF VAR iMinXmove           AS INT NO-UNDO.
DEF VAR iMinYmove           AS INT NO-UNDO.
                            
DEF VAR iAbsXsplitY         AS INT NO-UNDO.
DEF VAR iAbsXscopeY         AS INT NO-UNDO.
DEF VAR iAbsYsplitX         AS INT NO-UNDO.
DEF VAR iOrgSplitXpos       AS INT NO-UNDO.
DEF VAR iOrgSplitYpos       AS INT NO-UNDO.
                            
DEF VAR hCurrWindow         AS WIDGET-HANDLE NO-UNDO.
DEF VAR cMoveToTopList      AS CHAR   NO-UNDO.
DEF VAR cWidgetList         AS CHAR   NO-UNDO.
DEF VAR cFrameList          AS CHAR   NO-UNDO.
DEF VAR iCalcWidth          AS INT    NO-UNDO.
DEF VAR iCalcHeight         AS INT    NO-UNDO.
DEF VAR bReleaseWinLock     AS LOG    NO-UNDO INIT TRUE.
DEF VAR bUseCalcValues      AS LOG    NO-UNDO.
DEF VAR cAction             AS CHAR   NO-UNDO.
DEF VAR cDirectory          AS CHAR   NO-UNDO.
DEF VAR hTemp               AS HANDLE NO-UNDO.
DEF VAR hDebugObject        AS HANDLE NO-UNDO.
DEF VAR rBrowseBuffer       AS ROWID  NO-UNDO.
DEF VAR cTemp               AS CHAR   NO-UNDO.
DEF VAR bKeepOrgColour      AS LOG    NO-UNDO.
DEF VAR cResizeUser         AS CHAR   NO-UNDO.
DEF VAR cVisibleList        AS CHAR   NO-UNDO.
DEF VAR cLassoExceptionList AS CHAR   NO-UNDO.

DEF VAR cGlobMenuType       AS CHAR NO-UNDO INIT "bar".
DEF VAR bGlobEnableCommers  AS LOG NO-UNDO.
DEF VAR bGlobEnableColour   AS LOG NO-UNDO. /* INIT TRUE. */
DEF VAR bGlobEnableHelp     AS LOG NO-UNDO.
DEF VAR bGlobEnableAbout    AS LOG NO-UNDO.
DEF VAR bGlobEnableSave     AS LOG NO-UNDO. /* INIT TRUE. */
DEF VAR iGlobNumAboutMenu   AS INT NO-UNDO.
DEF VAR cGlobReservedColour AS CHAR NO-UNDO.
DEF VAR bGlobAutoSaveSettings AS LOG NO-UNDO.

DEF VAR bMyEnableSave       AS LOG NO-UNDO INIT ?.

DEF VAR bCustomWinSetting   AS LOG    NO-UNDO.
DEF VAR cSaveSettingsName   AS CHAR   NO-UNDO.
DEF VAR iLoadTime           AS INT    NO-UNDO INIT 1000.
DEF VAR bOverLoadSettings   AS LOG    NO-UNDO.
DEF VAR bResizeADM2panel    AS LOG    NO-UNDO.
DEF VAR hLockWindow         AS HANDLE NO-UNDO.
DEF VAR hWidgetWatch        AS HANDLE NO-UNDO.
DEF VAR hRepaintWindow      AS HANDLE NO-UNDO.
DEF VAR bManResSettingsMenu AS LOG    NO-UNDO.

DEF VAR iMinX               AS INT  NO-UNDO.
DEF VAR iMinY               AS INT  NO-UNDO.
DEF VAR iMaxX               AS INT  NO-UNDO.
DEF VAR iMaxY               AS INT  NO-UNDO.
DEF VAR bJBoxUserSetting    AS LOG  NO-UNDO.
DEF VAR cJBoxUserSettingReturnMsg AS CHAR NO-UNDO.    
DEF VAR cWidgetListingFilter      AS CHAR NO-UNDO FORMAT "x(50)".
DEF VAR cWatchLogFile       AS CHAR NO-UNDO.
DEF VAR iRetVal             AS INT NO-UNDO.
DEF VAR hLockWinForResize   AS HANDLE NO-UNDO.
DEF VAR bKeepWithinFrame    AS LOG    NO-UNDO.

DEF STREAM strWatchLog.

DEF TEMP-TABLE ttOrgWinSize 
    FIELD chWindow      AS CHAR
    FIELD iWidthPixels  AS INT
    FIELD iHeightPixels AS INT
    FIELD iMinXmove     AS INT
    FIELD iMinYmove     AS INT
    FIELD iMinX         AS INT
    FIELD iMinY         AS INT
    FIELD hProcedure    AS HANDLE
    .

DEF TEMP-TABLE ttCurrWinSize 
    FIELD chWindow      AS CHAR
    FIELD iWidthPixels  AS INT
    FIELD iHeightPixels AS INT
    field hWindow       as handle
    field hParentProc   as handle
    index idxWindow is primary unique hWindow 
    .

DEF TEMP-TABLE ttNoResizeX
    FIELD chWindow AS CHAR
    FIELD chFrame  AS CHAR
    FIELD cWidget  AS CHAR
    INDEX ixRes chFrame cWidget.

DEF TEMP-TABLE ttNoResizeY
    FIELD chWindow AS CHAR
    FIELD chFrame  AS CHAR
    FIELD cWidget  AS CHAR
    INDEX ixRes chFrame cWidget.

DEF TEMP-TABLE ttAddResizeX
    FIELD chWindow AS CHAR
    FIELD chFrame  AS CHAR
    FIELD cWidget  AS CHAR
    INDEX ixRes chFrame cWidget.

DEF TEMP-TABLE ttAddResizeY
    FIELD chWindow AS CHAR
    FIELD chFrame  AS CHAR
    FIELD cWidget  AS CHAR
    INDEX ixRes chFrame cWidget.

DEF TEMP-TABLE ttNoMoveX
    FIELD chWindow AS CHAR
    FIELD chFrame  AS CHAR
    FIELD cWidget  AS CHAR
    INDEX ixRes chFrame cWidget.

DEF TEMP-TABLE ttNoMoveY
    FIELD chWindow AS CHAR
    FIELD chFrame  AS CHAR
    FIELD cWidget  AS CHAR
    INDEX ixRes chFrame cWidget.

DEF TEMP-TABLE ttAddMoveX
    FIELD chWindow AS CHAR
    FIELD chFrame  AS CHAR
    FIELD cWidget  AS CHAR
    INDEX ixRes chFrame cWidget.

DEF TEMP-TABLE ttAddMoveY
    FIELD chWindow AS CHAR
    FIELD chFrame  AS CHAR
    FIELD cWidget  AS CHAR
    INDEX ixRes chFrame cWidget.

DEF TEMP-TABLE ttSplitBarX
    FIELD chWindow         AS CHAR
    FIELD hSplitBar        AS HANDLE 
    FIELD hSplitFrame      AS HANDLE 
    FIELD hMainFrame       AS HANDLE
    FIELD iPos             AS INT
    FIELD iOffset          AS INT
    FIELD fDistrX          AS DEC
    FIELD bRestrictResize  AS LOG
    FIELD iOrgPos          AS INT
    FIELD iOrgSplitBarX    AS INT        
    FIELD iMinX            AS INT 
    FIELD iMinXtoRightEdge AS INT
    INDEX ixRes chWindow hSplitBar
    INDEX ixHnd hSplitBar
    INDEX ixFrame hSplitFrame.

DEF TEMP-TABLE ttFollowSplitBarX
    FIELD chWindow        AS CHAR
    FIELD hSplitBar       AS HANDLE
    FIELD hWidget         AS WIDGET-HANDLE
    FIELD iDeltaSplitBar  AS INT
    INDEX ixRes chWindow hSplitBar.
DEF BUFFER bttFollowSplitBarX FOR ttFollowSplitBarX.

DEF TEMP-TABLE ttPrevSplitBarXpos
    FIELD chWindow        AS CHAR
    FIELD cFrameName      AS CHAR
    FIELD iPrevXpos       AS INT
    INDEX ixRes chWindow cFrameName.


DEF TEMP-TABLE ttSplitBarY
    FIELD chWindow             AS CHAR
    FIELD hSplitBar            AS HANDLE 
    FIELD hSplitFrame          AS HANDLE 
    FIELD hMainFrame           AS HANDLE
    FIELD iPos                 AS INT
    FIELD iOffset              AS INT
    FIELD fDistrY              AS DEC
    FIELD bRestrictResize      AS LOG
    FIELD iOrgPos              AS INT
    FIELD iOrgSplitBarY        AS INT        
    FIELD iOrgSplitWidth       AS INT
    FIELD iMinY                AS INT 
    FIELD iMinYtoBottom        AS INT
    INDEX ixRes chWindow hSplitBar
    INDEX ixHnd hSplitBar
    INDEX ixFrame hSplitFrame.

DEF TEMP-TABLE ttFollowSplitBarY
    FIELD chWindow        AS CHAR
    FIELD hSplitBar       AS HANDLE
    FIELD hWidget         AS WIDGET-HANDLE
    FIELD iDeltaSplitBar  AS INT
    INDEX ixRes chWindow hSplitBar.
DEF BUFFER bttFollowSplitBarY FOR ttFollowSplitBarY.

DEF TEMP-TABLE ttPrevSplitBarYpos
    FIELD chWindow        AS CHAR
    FIELD cFrameName      AS CHAR
    FIELD iPrevYpos       AS INT
    INDEX ixRes chWindow cFrameName.


DEF TEMP-TABLE ttResizeTypes
    FIELD chWindow        AS CHAR
    FIELD cTypes          AS CHAR
    INDEX ixRes chWindow.

DEF TEMP-TABLE ttBGcolour
    FIELD hFrame          AS HANDLE
    FIELD cColour         AS CHAR
    FIELD cColourDir      AS CHAR
    FIELD bAllFrames      AS LOG.

DEF TEMP-TABLE ttSettingsLoaded
    FIELD cColour         AS CHAR 
    FIELD cColourDir      AS CHAR 
    FIELD iCurrX          AS INT 
    FIELD iCurrY          AS INT
    FIELD iCurrDeltaX     AS INT
    FIELD iCurrDeltaY     AS INT
    FIELD iCurrSplitBarX  AS INT
    FIELD iCurrSplitBarY  AS INT
    FIELD cLastLoadProc   AS CHAR
    FIELD cCustomSetting  AS CHAR
    FIELD hWindow         AS HANDLE
    FIELD cSettingsName   AS CHAR.

DEF TEMP-TABLE ttLastInitFunction
    FIELD hWindow         AS HANDLE
    FIELD cFunction       AS CHAR.

DEF TEMP-TABLE ttMenuStruct
    FIELD hWindow     AS HANDLE
    FIELD cType       AS CHAR
    FIELD hParent     AS HANDLE
    FIELD cName       AS CHAR 
    FIELD cLabel      AS CHAR
    FIELD cAction     AS CHAR
    FIELD cParam      AS CHAR.
DEF BUFFER bttMenuStruct FOR ttMenuStruct.

DEF TEMP-TABLE ttAllFrames 
    FIELD hWindow     AS HANDLE
    FIELD hFrame      AS HANDLE
    FIELD bProcessed  AS LOG.

DEF TEMP-TABLE ttAddSpecialResize
    FIELD hProcedure AS HANDLE
    FIELD hFrame     AS HANDLE
    INDEX ixRes hProcedure hFrame.

DEF TEMP-TABLE ttResizeXgroup
    FIELD hWindow    AS HANDLE
    FIELD chFrame    AS CHAR
    FIELD cWidget    AS CHAR
    FIELD fPercent   AS DEC
    INDEX ixRes hWindow chFrame cWidget
    .
DEF BUFFER bttResizeXgroup FOR ttResizeXgroup.

DEF TEMP-TABLE ttResizeYgroup
    FIELD hWindow    AS HANDLE
    FIELD chFrame    AS CHAR
    FIELD cWidget    AS CHAR
    FIELD fPercent   AS DEC
    INDEX ixRes hWindow chFrame cWidget
    .
DEF BUFFER bttResizeYgroup FOR ttResizeYgroup.

DEF TEMP-TABLE ttMoveXgroup
    FIELD hWindow    AS HANDLE
    FIELD chFrame    AS CHAR
    FIELD cWidget    AS CHAR
    FIELD fPercent   AS DEC
    INDEX ixRes hWindow chFrame cWidget
    .
DEF BUFFER bttMoveXgroup FOR ttMoveXgroup.

DEF TEMP-TABLE ttMoveYgroup
    FIELD hWindow    AS HANDLE
    FIELD chFrame    AS CHAR
    FIELD cWidget    AS CHAR
    FIELD fPercent   AS DEC
    INDEX ixRes hWindow chFrame cWidget
    .
DEF BUFFER bttMoveYgroup FOR ttMoveYgroup.

DEF TEMP-TABLE ttAnchor
    FIELD hWindow       AS HANDLE
    FIELD hAnchor       AS HANDLE
    FIELD hWidget       AS WIDGET-HANDLE
    FIELD iAnchorDeltaX AS INT 
    FIELD iAnchorDeltaY AS INT
    INDEX ixRes    hWindow hAnchor hWidget
    INDEX ixWidget hWidget
    .

DEF TEMP-TABLE ttNeverResizeX
    FIELD hWindow       AS HANDLE
    FIELD hWidget       AS HANDLE
    INDEX idxNeverWindow hWindow
    INDEX idxNeverWidget hWidget.

DEF TEMP-TABLE ttNeverResizeY
    FIELD hWindow       AS HANDLE
    FIELD hWidget       AS HANDLE
    INDEX idxNeverWindow hWindow
    INDEX idxNeverWidget hWidget.

DEF TEMP-TABLE ttNeverMoveX
    FIELD hWindow       AS HANDLE
    FIELD hWidget       AS HANDLE
    INDEX idxNeverWindow hWindow
    INDEX idxNeverWidget hWidget.

DEF TEMP-TABLE ttNeverMoveY
    FIELD hWindow       AS HANDLE
    FIELD hWidget       AS HANDLE
    INDEX idxNeverWindow hWindow
    INDEX idxNeverWidget hWidget.

DEF TEMP-TABLE ttADMFolderRect
    FIELD hWindow       AS HANDLE
    FIELD hFolderFrm    AS HANDLE
    FIELD hRect         AS HANDLE
    FIELD ixPos         AS INT
    FIELD iyRelPos      AS INT
    FIELD iRelWidth     AS INT
    FIELD iRelHeight    AS INT
    INDEX ixFolderRect  hFolderFrm
    INDEX ixFolderWidget hRect
    .

DEF TEMP-TABLE ttTransMarker
    FIELD hWindow      AS HANDLE
    FIELD hMarker      AS HANDLE
    INDEX ixMarker  hWindow.

DEF TEMP-TABLE ttOrgWidgetPos
    FIELD hWindow      AS HANDLE
    FIELD hFrame       AS HANDLE
    FIELD hWidget      AS HANDLE
    FIELD iRow         AS INT
    FIELD iCol         AS INT
    FIELD iX           AS INT
    FIELD iY           AS INT
    FIELD iWidthChars  AS INT
    FIELD iWidthPix    AS INT
    FIELD iHeightPix   AS INT
    INDEX ixOrgPos  hWindow
    INDEX ixWidget  hWidget
    INDEX idxRow  IS PRIMARY hFrame iRow iCol.

DEF TEMP-TABLE ttOneTimeException
    FIELD hWidget AS HANDLE.

/* PROCEDURE LockWindowUpdate EXTERNAL "User32.dll":U:  */
/*   DEFINE INPUT  PARAMETER hWnd AS LONG.              */
/*   DEFINE RETURN PARAMETER iRetVal AS LONG.           */
/* END PROCEDURE.                                       */

PROCEDURE ShellExecuteA EXTERNAL "SHELL32.DLL" :
  DEFINE INPUT  PARAMETER hHandle       AS LONG.
  DEFINE INPUT  PARAMETER lpOperation   AS CHAR.
  DEFINE INPUT  PARAMETER lpFile        AS CHAR.
  DEFINE INPUT  PARAMETER lpParameters  AS CHAR.
  DEFINE INPUT  PARAMETER lpDirectory   AS CHAR.
  DEFINE INPUT  PARAMETER nShowCmd      AS LONG.
  DEFINE RETURN PARAMETER hInstance     AS LONG.
END PROCEDURE.

PROCEDURE RedrawWindow EXTERNAL "user32.dll":
    def input parameter v-hwnd as long no-undo.
    def input parameter v-rect as long no-undo.
    def input parameter v-rgn as long no-undo.
    def input parameter v-flags as long no-undo.
    def return parameter v-ret as long no-undo.
end procedure.

Procedure SendMessageW external "user32.dll".
  Define input parameter hhwnd as long.
  Define input parameter wmsg as long.
  Define input parameter wparam as long.
  Define input parameter lparam as long.
  def return parameter v-ret as long no-undo.
End procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-AddLanguageSubCat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddLanguageSubCat Procedure 
FUNCTION AddLanguageSubCat RETURNS CHARACTER
  ( INPUT icFile AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addSpecialResizeProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addSpecialResizeProc Procedure 
FUNCTION addSpecialResizeProc RETURNS CHARACTER
  ( INPUT iphProcedure AS HANDLE,
    INPUT iphFrame     AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addTranslationMarkerRule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addTranslationMarkerRule Procedure 
FUNCTION addTranslationMarkerRule RETURNS LOGICAL
  ( INPUT ihTransMarker AS HANDLE,
    INPUT ihFollow      AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CopyBrowseToClipboard) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CopyBrowseToClipboard Procedure 
FUNCTION CopyBrowseToClipboard RETURNS LOGICAL
  ( INPUT hBrowse AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateStatusBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateStatusBar Procedure 
FUNCTION CreateStatusBar RETURNS LOGICAL
  ( INPUT ihWindow         AS HANDLE,
    INPUT icFieldPixelList AS CHARACTER,
    INPUT iiNumFields      AS INTEGER,
    INPUT ibTopLine        AS LOGICAL,
    INPUT iiFont           AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteWinSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DeleteWinSettings Procedure 
FUNCTION DeleteWinSettings RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoAppendRule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoAppendRule Procedure 
FUNCTION DoAppendRule RETURNS LOGICAL
  ( INPUT ihRuleBuffer AS HANDLE,
    INPUT ihWidget     AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoCreateMenuStruct) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoCreateMenuStruct Procedure 
FUNCTION DoCreateMenuStruct RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE,
    INPUT icType   AS CHAR,
    INPUT icName   AS CHAR,
    INPUT icLabel  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoFollowSplitBarX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoFollowSplitBarX Procedure 
FUNCTION DoFollowSplitBarX RETURNS LOGICAL
  (INPUT cProcessTypes  AS CHAR,
   INPUT cSkipTypes     AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoFollowSplitBarY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoFollowSplitBarY Procedure 
FUNCTION DoFollowSplitBarY RETURNS LOGICAL
  (INPUT cProcessTypes  AS CHAR,
   INPUT cSkipTypes     AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoGetMenuLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoGetMenuLabel Procedure 
FUNCTION DoGetMenuLabel RETURNS CHARACTER
  ( INPUT ihWindow AS HANDLE,
    INPUT icName   AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoLoadSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoLoadSettings Procedure 
FUNCTION DoLoadSettings RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoRemoveBGcolour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoRemoveBGcolour Procedure 
FUNCTION DoRemoveBGcolour RETURNS LOGICAL
  ( INPUT hWidget AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoSetWinMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoSetWinMenu Procedure 
FUNCTION DoSetWinMenu RETURNS LOGICAL
  ( INPUT hWindow        AS HANDLE,
    INPUT bEnableCommers AS LOG,
    INPUT bEnableColour  AS LOG, 
    INPUT bEnableHelp    AS LOG,   
    INPUT bEnableAbout   AS LOG,  
    INPUT bEnableSave    AS LOG,
    INPUT icMenuLabel    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoWidgetResize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoWidgetResize Procedure 
FUNCTION DoWidgetResize RETURNS LOGICAL
  ( INPUT hWidget AS WIDGET-HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FindVisible) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FindVisible Procedure 
FUNCTION FindVisible RETURNS LOGICAL
  ( INPUT ihWidget AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAbsPosition) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAbsPosition Procedure 
FUNCTION getAbsPosition RETURNS INTEGER
  ( INPUT ihWidget AS HANDLE,
    INPUT cAxe     AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAllFrames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAllFrames Procedure 
FUNCTION getAllFrames RETURNS LOGICAL
  ( INPUT hWidget AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAnyWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAnyWidget Procedure 
FUNCTION getAnyWidget RETURNS HANDLE
  ( INPUT hWidget    AS HANDLE, 
    INPUT iLevel     AS INT,
    INPUT icName     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentDeltaX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentDeltaX Procedure 
FUNCTION getCurrentDeltaX RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentDeltaY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentDeltaY Procedure 
FUNCTION getCurrentDeltaY RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCustomWinSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCustomWinSettings Procedure 
FUNCTION getCustomWinSettings RETURNS CHARACTER
  ( INPUT ihWindow AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFrameWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameWidget Procedure 
FUNCTION getFrameWidget RETURNS HANDLE
  ( INPUT ihFrame  AS HANDLE,
    INPUT icWidget AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoadedSetting) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoadedSetting Procedure 
FUNCTION getLoadedSetting RETURNS CHARACTER
  (INPUT ihWindow  AS HANDLE,
   INPUT icType    AS CHAR  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOrgWigetPos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOrgWigetPos Procedure 
FUNCTION getOrgWigetPos RETURNS CHARACTER
  ( INPUT ihParent     AS HANDLE,
    INPUT icTypeList   AS CHAR,
    INPUT icNameList   AS CHAR,
    INPUT icExNameList AS CHAR,
    INPUT ihWidget     AS HANDLE,
    INPUT ihBuffer     AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProcHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProcHandle Procedure 
FUNCTION getProcHandle RETURNS HANDLE
  ( INPUT phFocus AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getResizeSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getResizeSettings Procedure 
FUNCTION getResizeSettings RETURNS HANDLE
  ( INPUT ihWindow AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSaveSettingName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSaveSettingName Procedure 
FUNCTION getSaveSettingName RETURNS CHARACTER
  ( INPUT ihWindow AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSiblingNames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSiblingNames Procedure 
FUNCTION getSiblingNames RETURNS CHARACTER
  ( INPUT hWidget          AS HANDLE, 
    INPUT icTypes          AS CHAR,
    INPUT icExceptionTypes AS CHAR,
    INPUT icExceptionNames AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSplitBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSplitBar Procedure 
FUNCTION getSplitBar RETURNS HANDLE
  ( INPUT ihWindow    AS HANDLE,
    INPUT icDirection AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSplitBarHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSplitBarHandle Procedure 
FUNCTION getSplitBarHandle RETURNS HANDLE
  ( INPUT ihWindow AS HANDLE,
    INPUT icDirection AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWidgetHandles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWidgetHandles Procedure 
FUNCTION getWidgetHandles RETURNS CHARACTER
  ( INPUT hWidget    AS HANDLE, 
    INPUT iLevel     AS INT,
    INPUT iMinX      AS INT,
    INPUT iMaxX      AS INT,
    INPUT iMinY      AS INT,
    INPUT iMaxY      AS INT,
    INPUT icTypes    AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWidgetNamesByLasso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWidgetNamesByLasso Procedure 
FUNCTION getWidgetNamesByLasso RETURNS CHARACTER
  ( INPUT hWidget    AS HANDLE, 
    INPUT iLevel     AS INT,
    INPUT icTypes    AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWidgetsByLasso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWidgetsByLasso Procedure 
FUNCTION getWidgetsByLasso RETURNS CHARACTER
  ( INPUT hWidget    AS HANDLE, 
    INPUT iLevel     AS INT,
    INPUT icTypes    AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWinMinXmove) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWinMinXmove Procedure 
FUNCTION getWinMinXmove RETURNS INTEGER
  ( INPUT ihWindow AS HANDLE,
    INPUT icXY     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-overridesWithinFrame) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD overridesWithinFrame Procedure 
FUNCTION overridesWithinFrame RETURNS LOGICAL
  ( INPUT icFrame     AS CHAR,
    INPUT icFrameName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-removeSplitBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeSplitBar Procedure 
FUNCTION removeSplitBar RETURNS LOGICAL
  ( INPUT hWindow AS HANDLE,
    INPUT hSplitBar AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReplaceInText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReplaceInText Procedure 
FUNCTION ReplaceInText RETURNS CHARACTER
  ( INPUT icText    AS CHAR,
    INPUT icReplace AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-resetSplitBarPos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD resetSplitBarPos Procedure 
FUNCTION resetSplitBarPos RETURNS LOGICAL
  ( INPUT hWindow AS HANDLE,
    INPUT iOrgDeltaX AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ResizeKeepsWindowLocked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ResizeKeepsWindowLocked Procedure 
FUNCTION ResizeKeepsWindowLocked RETURNS LOGICAL
  ( INPUT ihLockWinForResize AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveWinSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SaveWinSettings Procedure 
FUNCTION SaveWinSettings RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAddMoveX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAddMoveX Procedure 
FUNCTION setAddMoveX RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAddMoveY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAddMoveY Procedure 
FUNCTION setAddMoveY RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAddResizeX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAddResizeX Procedure 
FUNCTION setAddResizeX RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAddResizeY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAddResizeY Procedure 
FUNCTION setAddResizeY RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setADMFolderRect) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setADMFolderRect Procedure 
FUNCTION setADMFolderRect RETURNS LOGICAL
  ( INPUT ihFolderFrm AS HANDLE,
    INPUT icAction    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAnchor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAnchor Procedure 
FUNCTION setAnchor RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hAnchor     AS HANDLE,
    INPUT cWidgetList AS CHAR,
    INPUT icDirection AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppFont) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAppFont Procedure 
FUNCTION setAppFont RETURNS INTEGER
  ( INPUT icFont        AS CHAR,
    INPUT icEnvironment AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAutoSaveWinSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAutoSaveWinSettings Procedure 
FUNCTION setAutoSaveWinSettings RETURNS LOGICAL
  ( INPUT ibAutoSave AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBGcolour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBGcolour Procedure 
FUNCTION setBGcolour RETURNS LOGICAL
  ( INPUT ihFrame AS HANDLE,
    INPUT icColour AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCleanUpResize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCleanUpResize Procedure 
FUNCTION setCleanUpResize RETURNS LOGICAL
  ( INPUT hWindow AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCleanUpSplitBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCleanUpSplitBar Procedure 
FUNCTION setCleanUpSplitBar RETURNS LOGICAL
  ( INPUT hWindow AS HANDLE,
    INPUT ihSplitBar AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCurrWidthHeight) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCurrWidthHeight Procedure 
FUNCTION setCurrWidthHeight RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE,
    INPUT iiWidth  AS INT,
    INPUT iiHeight AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCustomWinSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCustomWinSettings Procedure 
FUNCTION setCustomWinSettings RETURNS LOGICAL
  ( INPUT ihWindow  AS HANDLE,
    INPUT icSetting AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDebugResize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDebugResize Procedure 
FUNCTION setDebugResize RETURNS LOGICAL
  ( INPUT ibDebug AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDeltaX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDeltaX Procedure 
FUNCTION setDeltaX RETURNS LOGICAL
  ( INPUT iiDeltaX AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDeltaY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDeltaY Procedure 
FUNCTION setDeltaY RETURNS LOGICAL
  ( INPUT iiDeltaY AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEnableColor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEnableColor Procedure 
FUNCTION setEnableColor RETURNS LOGICAL
  ( INPUT ibGlobEnableColour AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFollowPlaceHolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFollowPlaceHolder Procedure 
FUNCTION setFollowPlaceHolder RETURNS LOGICAL
  ( INPUT ihWindow      AS HANDLE,
    INPUT ihObject      AS HANDLE,
    INPUT ihPlaceHolder AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFollowSplitBarX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFollowSplitBarX Procedure 
FUNCTION setFollowSplitBarX RETURNS LOGICAL
  ( INPUT hWindow           AS HANDLE,
    INPUT hSplitBar         AS WIDGET-HANDLE,
    INPUT cWidgetHandleList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFollowSplitBarY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFollowSplitBarY Procedure 
FUNCTION setFollowSplitBarY RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hSplitBar   AS WIDGET-HANDLE,
    INPUT cWidgetHandleList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKeepResizeWithinFrame) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setKeepResizeWithinFrame Procedure 
FUNCTION setKeepResizeWithinFrame RETURNS LOGICAL
  ( INPUT ibKeepWithinFrame AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLassoExceptionList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLassoExceptionList Procedure 
FUNCTION setLassoExceptionList RETURNS LOGICAL
  ( INPUT icLassoExceptionList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLockWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLockWindow Procedure 
FUNCTION setLockWindow RETURNS LOGICAL
  ( INPUT ihLockWindow AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLockWindowUpdate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLockWindowUpdate Procedure 
FUNCTION setLockWindowUpdate RETURNS LOGICAL
  ( INPUT bOn     AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setManualResizeSettngsMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setManualResizeSettngsMenu Procedure 
FUNCTION setManualResizeSettngsMenu RETURNS LOGICAL
  ( INPUT ibManResSettingsMenu AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMinXYmove) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMinXYmove Procedure 
FUNCTION setMinXYmove RETURNS LOGICAL
  ( INPUT iiMinX AS INT,
    INPUT iiMinY AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMoveXgroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMoveXgroup Procedure 
FUNCTION setMoveXgroup RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS HANDLE,
    INPUT ifPercent   AS DEC,
    INPUT cWidgetList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMoveYgroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMoveYgroup Procedure 
FUNCTION setMoveYgroup RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS HANDLE,
    INPUT ifPercent   AS DEC,
    INPUT cWidgetList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMyEnableSaveSettingsMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMyEnableSaveSettingsMenu Procedure 
FUNCTION setMyEnableSaveSettingsMenu RETURNS LOGICAL
  ( INPUT ibMyEnableSave AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNewMinXYmove) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNewMinXYmove Procedure 
FUNCTION setNewMinXYmove RETURNS LOGICAL
  ( INPUT hWidget AS WIDGET-HANDLE,
    INPUT iiMinXmove     AS INT,
    INPUT iiMinYmove     AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNoMoveX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNoMoveX Procedure 
FUNCTION setNoMoveX RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNoMoveY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNoMoveY Procedure 
FUNCTION setNoMoveY RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNoResizeX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNoResizeX Procedure 
FUNCTION setNoResizeX RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNoResizeY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNoResizeY Procedure 
FUNCTION setNoResizeY RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOneTimeException) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setOneTimeException Procedure 
FUNCTION setOneTimeException RETURNS LOGICAL
  ( INPUT ihWidget AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOrgWidthHeight) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setOrgWidthHeight Procedure 
FUNCTION setOrgWidthHeight RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE,
    INPUT iiWidth  AS INT,
    INPUT iiHeight AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOrgWinSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setOrgWinSize Procedure 
FUNCTION setOrgWinSize RETURNS LOGICAL
  ( INPUT hWindow    AS WIDGET-HANDLE,
    INPUT iiMinXmove AS INT,
    INPUT iiMinYmove AS INT,
    INPUT iMinX      AS INT,
    INPUT iMinY      AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOverLoadSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setOverLoadSettings Procedure 
FUNCTION setOverLoadSettings RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRepaintWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setRepaintWindow Procedure 
FUNCTION setRepaintWindow RETURNS LOGICAL
  ( INPUT ihRepaintWindow AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetResizeADM2panel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetResizeADM2panel Procedure 
FUNCTION SetResizeADM2panel RETURNS LOGICAL
  ( INPUT ibResizeADM2panel AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setResizeTypes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setResizeTypes Procedure 
FUNCTION setResizeTypes RETURNS LOGICAL
  ( INPUT ihWidget AS WIDGET-HANDLE,
    INPUT icTypes  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setResizeXgroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setResizeXgroup Procedure 
FUNCTION setResizeXgroup RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS HANDLE,
    INPUT ifPercent   AS DEC,
    INPUT cWidgetList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setResizeYgroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setResizeYgroup Procedure 
FUNCTION setResizeYgroup RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS HANDLE,
    INPUT ifPercent   AS DEC,
    INPUT cWidgetList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSaveSettingName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSaveSettingName Procedure 
FUNCTION setSaveSettingName RETURNS LOGICAL
  ( INPUT icSettingsName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetScrollableFrames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetScrollableFrames Procedure 
FUNCTION SetScrollableFrames RETURNS LOGICAL
  ( INPUT hWidget AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSpecialBGcolour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSpecialBGcolour Procedure 
FUNCTION setSpecialBGcolour RETURNS LOGICAL
  ( INPUT ihFrame AS HANDLE,
    INPUT icColour AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSpesOrgWinSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSpesOrgWinSize Procedure 
FUNCTION setSpesOrgWinSize RETURNS LOGICAL
  ( INPUT hWindow        AS WIDGET-HANDLE,
    INPUT iMinXmove      AS INT,
    INPUT iMinYmove      AS INT,
    INPUT iMinX          AS INT,
    INPUT iMinY          AS INT,
    INPUT bEnableCommers AS LOG,
    INPUT bEnableColour  AS LOG, 
    INPUT bEnableHelp    AS LOG,   
    INPUT bEnableAbout   AS LOG,  
    INPUT bEnableSave    AS LOG
     )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSplitBarX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSplitBarX Procedure 
FUNCTION setSplitBarX RETURNS LOGICAL
  ( INPUT hWindow      AS HANDLE,
    INPUT hSplitBar    AS HANDLE,
    INPUT bRestrict    AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSplitBarXlimits) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSplitBarXlimits Procedure 
FUNCTION setSplitBarXlimits RETURNS LOGICAL
  ( INPUT hSplitBar        AS HANDLE,
    INPUT iMinX            AS INT,
    INPUT iMinXtoRightEdge AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSplitBarY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSplitBarY Procedure 
FUNCTION setSplitBarY RETURNS LOGICAL
  ( INPUT hWindow      AS HANDLE,
    INPUT hSplitBar    AS HANDLE,
    INPUT bRestrict    AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSplitBarYlimits) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSplitBarYlimits Procedure 
FUNCTION setSplitBarYlimits RETURNS LOGICAL
  ( INPUT hSplitBar     AS HANDLE,
    INPUT iMinY         AS INT,
    INPUT iMinYtoBottom AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWebDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWebDoc Procedure 
FUNCTION setWebDoc RETURNS CHARACTER
  ( INPUT pcType AS CHAR,
    INPUT pcDoc  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWebHelpFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWebHelpFile Procedure 
FUNCTION setWebHelpFile RETURNS LOGICAL
  ( INPUT ihWindow      AS HANDLE,
    INPUT icHelpFile AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWidgetResize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWidgetResize Procedure 
FUNCTION setWidgetResize RETURNS LOGICAL
  ( INPUT hWidget      AS WIDGET-HANDLE,
    INPUT hWindow      AS WIDGET-HANDLE,
    INPUT icAction     AS CHAR,
    INPUT cExtra       AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWidgetWatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWidgetWatch Procedure 
FUNCTION setWidgetWatch RETURNS LOGICAL
  ( INPUT ihWidgetWatch  AS HANDLE,
    INPUT icWatchLogFile AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShowMeTheWidgets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ShowMeTheWidgets Procedure 
FUNCTION ShowMeTheWidgets RETURNS LOGICAL
  ( INPUT hWidget    AS HANDLE, 
    INPUT iLevel     AS INT,
    INPUT cDirection AS CHAR,
    INPUT cOutDest   AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SpecialResize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SpecialResize Procedure 
FUNCTION SpecialResize RETURNS LOGICAL
  ( INPUT hWidget AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToExcelViaFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ToExcelViaFile Procedure 
FUNCTION ToExcelViaFile RETURNS LOGICAL
  ( INPUT ihObject   AS HANDLE,
    INPUT iiMaxCount AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WidgetWatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WidgetWatch Procedure 
FUNCTION WidgetWatch RETURNS LOGICAL
  ( INPUT ihWidget  AS HANDLE,
    INPUT icLogText AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 43.19
         WIDTH              = 94.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
SUBSCRIBE TO "NeverResize" ANYWHERE.
SUBSCRIBE TO "NeverMove" ANYWHERE.

bOk = DYNAMIC-FUNCTION("setUserSetting","","","","","") NO-ERROR.
IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN
  bJBoxUserSetting = ?.
ELSE DO:
  cJBoxUserSettingReturnMsg = DYNAMIC-FUNCTION("getTransactionMessage") NO-ERROR.
  IF cJBoxUserSettingReturnMsg NE "" THEN
    bJBoxUserSetting = ?.
  ELSE 
    bJBoxUserSetting = YES.
END.

CREATE ttResizeTypes.
ASSIGN ttResizeTypes.chWindow = ""
       ttResizeTypes.cTypes   = "FRAME,BROWSE,RECTANGLE,EDITOR,SELECTION-LIST,CONTROL-FRAME".

GET-KEY-VALUE SECTION "ResizeLib" KEY "SaveSettingsTo" VALUE cDirectory.

IF cDirectory = "" OR cDirectory = ? THEN cDirectory = SESSION:TEMP-DIR.
cDirectory = RIGHT-TRIM(cDirectory,"\") + "\".

/*
GET-KEY-VALUE SECTION "ResizeLib" KEY "MenuType" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN
  cGlobMenuType = cTemp.

GET-KEY-VALUE SECTION "ResizeLib" KEY "MenuLabel" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN DO:
  bGlobEnableSave = TRUE.
  DoCreateMenuStruct (?,"MENU","MenuLabel",cTemp).
END.
ELSE
  DoCreateMenuStruct (?,"MENU","MenuLabel","Options").

GET-KEY-VALUE SECTION "ResizeLib" KEY "ReservedColours" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN 
  cGlobReservedColour = cTemp.

GET-KEY-VALUE SECTION "ResizeLib" KEY "ColourMenu" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN DO:
  DoCreateMenuStruct (?,"SUB-MENU","ColourMenu",cTemp).
  bGlobEnableColour = TRUE.

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Normal" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Normal",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Green" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Green",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Blue" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Blue",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Yellow" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Yellow",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Gray" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Gray",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Red" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Red",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Turquoise" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Turquoise",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Turn" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Turn",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "AllFrames" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","AllFrames",cTemp).
END.

GET-KEY-VALUE SECTION "ResizeLib" KEY "SaveSettingsMenu" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN DO:
  DoCreateMenuStruct (?,"SUB-MENU","SaveSettingsMenu",cTemp).
  bGlobEnableSave = TRUE.

  GET-KEY-VALUE SECTION "ResizeLib" KEY "DeleteSettingsMenu" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN 
    DoCreateMenuStruct (?,"SUB-MENU","DeleteSettingsMenu",cTemp).
END.

GET-KEY-VALUE SECTION "ResizeLib" KEY "HelpMenu" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN DO:
  DoCreateMenuStruct (?,IF cGlobMenuType MATCHES "*help*" THEN "MENU" ELSE "SUB-MENU","HelpMenu",cTemp).
  bGlobEnableHelp = TRUE.

  GET-KEY-VALUE SECTION "ResizeLib" KEY "defaultHelp" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN 
    DoCreateMenuStruct (?,"MENU-ITEM","defaultHelp",cTemp).
END.

GET-KEY-VALUE SECTION "ResizeLib" KEY "AboutMenu" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN DO:
  DoCreateMenuStruct (?,IF cGlobMenuType MATCHES "*help*" THEN "MENU" ELSE "SUB-MENU","AboutMenu",cTemp).
  bGlobEnableAbout = TRUE.

  GET-KEY-VALUE SECTION "ResizeLib" KEY "about1" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN 
    DoCreateMenuStruct (?,"MENU-ITEM","about1",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "about2" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN 
    DoCreateMenuStruct (?,"MENU-ITEM","about2",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "about3" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN 
    DoCreateMenuStruct (?,"MENU-ITEM","about3",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "about4" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN 
    DoCreateMenuStruct (?,"MENU-ITEM","about4",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "about5" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN 
    DoCreateMenuStruct (?,"MENU-ITEM","about5",cTemp).
END.

GET-KEY-VALUE SECTION "ResizeLib" KEY "NoMenu" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN 
  ASSIGN bGlobEnableCommers  = FALSE
         bGlobEnableColour   = FALSE
         bGlobEnableHelp     = FALSE
         bGlobEnableAbout    = FALSE
         bGlobEnableSave     = FALSE.

GET-KEY-VALUE SECTION "ResizeLib" KEY "AutoSaveSettings" VALUE cTemp.
IF cTemp = "yes" THEN 
  ASSIGN bGlobAutoSaveSettings = TRUE
         bGlobEnableSave       = FALSE.

GET-KEY-VALUE SECTION "ResizeLib" KEY "LoadTime" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN 
  iLoadTime = INT(cTemp).

*/

/* DEF VAR bGlobEnableCommers  AS LOG NO-UNDO.           */
/* DEF VAR bGlobEnableColour   AS LOG NO-UNDO.           */
/* DEF VAR bGlobEnableHelp     AS LOG NO-UNDO.           */
/* DEF VAR bGlobEnableAbout    AS LOG NO-UNDO.           */
/* DEF VAR bGlobEnableSave     AS LOG NO-UNDO INIT TRUE. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-DoDeleteWinSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoDeleteWinSettings Procedure 
PROCEDURE DoDeleteWinSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWindow AS HANDLE NO-UNDO.
     
IF bJBoxUserSetting THEN DO:
  FIND FIRST ttOrgWinSize
       WHERE ttOrgWinSize.chWindow = STRING(ihWindow) NO-ERROR.
 
  IF AVAIL ttOrgWinSize THEN DO:
    IF NOT DYNAMIC-FUNCTION("setUserSetting",
                   ttOrgWinSize.hProcedure:FILE-NAME,
                   "resizelib",
                   DYNAMIC-FUNCTION("getAttribute",ttOrgWinSize.hProcedure:CURRENT-WINDOW,"usersettingcontext"),
                   "windowxpos,windowypos,deltaxgrowth,deltaygrowth,wincolor,windcolordir,splitbarxpos,splitbarypos",
                   "delete_setting") THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.
END.
ELSE DO:
  FIND FIRST ttSettingsLoaded
       WHERE ttSettingsLoaded.hWindow = ihWindow NO-ERROR.
  
  IF AVAIL ttSettingsLoaded THEN 
    OS-DELETE VALUE(cDirectory + "resizelib-" + cResizeUser + "-" + ttSettingsLoaded.cSettingsName + ".ini").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoOpenDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoOpenDoc Procedure 
PROCEDURE DoOpenDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcCommand AS CHARACTER  NO-UNDO. /* 'open' or 'print' */
DEFINE INPUT  PARAMETER pcDoc     AS CHARACTER  NO-UNDO. /* your document */

DEF VAR cMsg            AS CHAR NO-UNDO.
DEF VAR iStart          AS INT NO-UNDO INIT 1.
def var hCurrProc       as handle no-undo.
def var hB              as handle no-undo.
def var hWinInFocus     as handle no-undo.

def buffer bCWS for ttCurrWinSize.

assign 
  hWinInFocus = active-window
  hCurrProc   = ?
.

find first bCWS where bCWS.hWindow = hWinInFocus no-lock no-error.
if avail bCWS then
do:
  hCurrProc = bCWS.hParentProc.
end.

CASE pcCommand:
  WHEN "message" THEN DO: 
    DO ix = 1 TO LENGTH(pcDoc):
      IF SUBSTR(pcDoc,ix,2) = ". " THEN 
        ASSIGN cMsg   = cMsg + SUBSTR(pcDoc,iStart,ix - iStart + 1) + CHR(10)
               iStart = ix + 2.
    END.
    IF SUBSTR(pcDoc,LENGTH(pcDoc)) NE "." THEN
      cMsg   = cMsg + SUBSTR(pcDoc,iStart).
    MESSAGE cMsg.
  END.
  WHEN "web" THEN 
       setWebDoc("open",pcDoc).
  WHEN 'run' THEN
  DO:
    run value(pcDoc) in hCurrProc no-error.
    if error-status:error then
      MESSAGE error-status:get-message(1).
  END.

END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoSaveWinSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoSaveWinSettings Procedure 
PROCEDURE DoSaveWinSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWindow AS HANDLE NO-UNDO.
DEF VAR cColour       AS CHAR NO-UNDO.
DEF VAR cColourDir    AS CHAR NO-UNDO INIT "down".
DEF VAR iSplitBarXPos AS INT NO-UNDO.
DEF VAR iSplitBarYPos AS INT NO-UNDO.
DEF VAR hBogus        AS HANDLE NO-UNDO.
DEF VAR hRealFrame    AS HANDLE NO-UNDO.
DEF VAR bSaveSize     AS LOG NO-UNDO INIT TRUE.

IF ihWindow:WIDTH-PIXELS  > SESSION:WIDTH-PIXELS - 75 AND
   ihWindow:HEIGHT-PIXELS > SESSION:HEIGHT-PIXELS - 75 THEN
  bSaveSize = FALSE.

FIND FIRST ttOrgWinSize
     WHERE ttOrgWinSize.chWindow = STRING(ihWindow) NO-ERROR.
FIND FIRST ttLastInitFunction
     WHERE ttLastInitFunction.hWindow = ihWindow NO-ERROR.
FIND FIRST ttSettingsLoaded
     WHERE ttSettingsLoaded.hWindow = ihWindow NO-ERROR.

IF AVAIL ttOrgWinSize AND ((AVAIL ttLastInitFunction AND AVAIL ttSettingsLoaded) OR bJBoxUserSetting) THEN DO:

  /* It might not be that FIRST-CHILD is the main frame */
  ASSIGN hBogus     = ihWindow:FIRST-CHILD:NEXT-SIBLING
         hRealFrame = ihWindow:FIRST-CHILD 
         NO-ERROR.
  REPEAT WHILE hBogus NE ?:
    IF hBogus:WIDTH-PIXELS * hBogus:HEIGHT-PIXELS > hRealFrame:WIDTH-PIXELS * hRealFrame:HEIGHT-PIXELS THEN 
      hRealFrame = hBogus.
    hBogus = hBogus:NEXT-SIBLING.
  END.

  FIND FIRST ttBGcolour
       WHERE ttBGcolour.hFrame  = hRealFrame 
       NO-ERROR.
  IF AVAIL ttBGcolour THEN
    ASSIGN cColour    = ttBGcolour.cColour
           cColourDir = ttBGcolour.cColourDir.
  ELSE 
    cColour = "normal".

  FIND FIRST ttSplitBarX
       WHERE ttSplitBarX.chWindow = STRING(ihWindow) 
       NO-ERROR.
  IF AVAIL ttSplitBarX AND VALID-HANDLE(ttSplitBarX.hSplitBar) THEN
    iSplitBarXPos = ttSplitBarX.hSplitBar:X - ttSplitBarX.iOrgSplitBarX.
  ELSE DO:
    FIND FIRST ttPrevSplitBarXpos 
         WHERE ttPrevSplitBarXpos.chWindow = STRING(ihWindow)
         NO-ERROR.
    IF AVAIL ttPrevSplitBarXpos THEN
      iSplitBarXPos = ttPrevSplitBarXpos.iPrevXpos.
  END.
  FIND FIRST ttSplitBarY
       WHERE ttSplitBarY.chWindow = STRING(ihWindow) 
       NO-ERROR.
  IF AVAIL ttSplitBarY AND VALID-HANDLE(ttSplitBarY.hSplitBar) THEN
    iSplitBarYPos = ttSplitBarY.hSplitBar:Y - ttSplitBarY.iOrgSplitBarY.
  ELSE DO:
    FIND FIRST ttPrevSplitBarYpos 
         WHERE ttPrevSplitBarYpos.chWindow = STRING(ihWindow)
         NO-ERROR.
    IF AVAIL ttPrevSplitBarYpos THEN
      iSplitBarYPos = ttPrevSplitBarYpos.iPrevYpos.
  END.

  IF bJBoxUserSetting THEN DO:
    IF VALID-HANDLE(ttOrgWinSize.hProcedure) AND NOT DYNAMIC-FUNCTION("setUserSetting",
                     ttOrgWinSize.hProcedure:FILE-NAME,
                     "resizelib",
                     DYNAMIC-FUNCTION("getAttribute",ttOrgWinSize.hProcedure:CURRENT-WINDOW,"usersettingcontext"),
                     (IF bSaveSize THEN
                       "windowxpos,windowypos,deltaxgrowth,deltaygrowth"
                       + (IF iSplitBarXPos NE 0 THEN ",splitbarxpos" ELSE "")
                       + (IF iSplitBarYPos NE 0 THEN ",splitbarypos" ELSE "")
                      ELSE "") +
                     (IF AVAIL ttSettingsLoaded AND ttSettingsLoaded.cCustomSetting NE "" THEN
                       ",customwinsetting" ELSE "") +
                     (IF cColour NE "" THEN ",wincolor,windcolordir" ELSE "")
                    ,(IF bSaveSize THEN
                       STRING(ihWindow:X) + CHR(1) +
                       STRING(ihWindow:Y) + CHR(1) +
                       STRING(ihWindow:WIDTH-PIXELS - ttOrgWinSize.iWidthPixels) + CHR(1) +
                       STRING(ihWindow:HEIGHT-PIXELS - ttOrgWinSize.iHeightPixels - (IF ihWindow:STATUS-AREA THEN 5 ELSE 0)) +
                       (IF iSplitBarXPos NE 0 THEN CHR(1) + STRING(iSplitBarXPos) ELSE "") +
                       (IF iSplitBarYPos NE 0 THEN CHR(1) + STRING(iSplitBarYPos) ELSE "")
                      ELSE "") +
                     (IF AVAIL ttSettingsLoaded AND ttSettingsLoaded.cCustomSetting NE "" THEN
                       CHR(1) + ttSettingsLoaded.cCustomSetting
                      ELSE "") +
                     (IF cColour NE "" THEN CHR(1) + cColour + CHR(1) + cColourDir ELSE "")
                     ) THEN                          
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.
  ELSE DO:
    OUTPUT TO VALUE(cDirectory + "resizelib-" + cResizeUser + "-" + ttSettingsLoaded.cSettingsName + ".ini").
     
    EXPORT DELIMITER ";" 
        cColour
        cColourDir
        (IF bSaveSize THEN ihWindow:X ELSE ttSettingsLoaded.iCurrX)
        (IF bSaveSize THEN ihWindow:Y ELSE ttSettingsLoaded.iCurrY)
        (IF bSaveSize THEN ihWindow:WIDTH-PIXELS  - ttOrgWinSize.iWidthPixels ELSE ttSettingsLoaded.iCurrDeltaX)
        (IF bSaveSize THEN ihWindow:HEIGHT-PIXELS - ttOrgWinSize.iHeightPixels - 
                            (IF ihWindow:STATUS-AREA THEN 5 ELSE 0) ELSE ttSettingsLoaded.iCurrDeltaY)
        (IF bSaveSize THEN iSplitBarXPos ELSE ttSettingsLoaded.iCurrSplitBarX)
        (IF bSaveSize THEN iSplitBarYPos ELSE ttSettingsLoaded.iCurrSplitBarY)
        ttLastInitFunction.cFunction
        ttSettingsLoaded.cCustomSetting
        .
    OUTPUT CLOSE. 
  END.
END.
/* ELSE                                                                  */
/*   MESSAGE PROGRAM-NAME(1) SKIP                                        */
/*           "ihWindow: " ihWindow SKIP                                  */
/*           "AVAIL ttOrgWinSize: " AVAIL ttOrgWinSize SKIP              */
/*           "AVAIL ttLastInitFunction: " AVAIL ttLastInitFunction SKIP  */
/*           "AVAIL ttSettingsLoaded: " AVAIL ttSettingsLoaded           */
/*           VIEW-AS ALERT-BOX.                                          */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoSetColourAll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoSetColourAll Procedure 
PROCEDURE DoSetColourAll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWindow AS HANDLE NO-UNDO.

FIND FIRST ttBGcolour 
     WHERE ttBGcolour.hFrame = ihWindow:FIRST-CHILD
     NO-ERROR.
IF AVAIL ttBGcolour THEN DO:
  ttBGcolour.bAllFrames = NOT ttBGcolour.bAllFrames.
  IF NOT ttBGcolour.bAllFrames THEN
    EMPTY TEMP-TABLE ttAllFrames.
  setBGcolour (ihWindow:FIRST-CHILD,"").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoSetWebDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoSetWebDoc Procedure 
PROCEDURE DoSetWebDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcCommand AS CHARACTER  NO-UNDO. /* 'open' or 'print' */
DEFINE INPUT  PARAMETER pcDoc     AS CHARACTER  NO-UNDO. /* your document */

setWebDoc (pcCommand,pcDoc).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoSetWinColour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoSetWinColour Procedure 
PROCEDURE DoSetWinColour :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihFrame AS HANDLE NO-UNDO.
DEF INPUT PARAM icColour AS CHAR NO-UNDO.

setBGcolour(ihFrame,icColour).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NeverMove) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NeverMove Procedure 
PROCEDURE NeverMove :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWidget    AS HANDLE NO-UNDO.
DEF INPUT PARAM icDirection AS CHAR   NO-UNDO.

DEF VAR hWindow AS HANDLE NO-UNDO.

IF NOT VALID-HANDLE(ihWidget) THEN RETURN.

IF CAN-QUERY(ihWidget,"window") THEN hWindow = ihWidget:WINDOW.

IF icDirection = "X" THEN DO:
  FIND FIRST ttNeverMoveX 
       WHERE ttNeverMoveX.hWidget = ihWidget
       NO-ERROR.
  IF NOT AVAIL ttNeverMoveX THEN DO:
    CREATE ttNeverMoveX.
    ASSIGN ttNeverMoveX.hWidget = ihWidget
           ttNeverMoveX.hWindow = hWindow.
  END.
END.
ELSE DO:
  FIND FIRST ttNeverMoveY 
       WHERE ttNeverMoveY.hWidget = ihWidget
       NO-ERROR.
  IF NOT AVAIL ttNeverMoveY THEN DO:
    CREATE ttNeverMoveY.
    ASSIGN ttNeverMoveY.hWidget = ihWidget
           ttNeverMoveY.hWindow = hWindow.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NeverResize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NeverResize Procedure 
PROCEDURE NeverResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWidget    AS HANDLE NO-UNDO.
DEF INPUT PARAM icDirection AS CHAR   NO-UNDO.

DEF VAR hWindow AS HANDLE NO-UNDO.

IF NOT VALID-HANDLE(ihWidget) THEN RETURN.

IF CAN-QUERY(ihWidget,"window") THEN hWindow = ihWidget:WINDOW.

IF icDirection = "X" THEN DO:
  FIND FIRST ttNeverResizeX 
       WHERE ttNeverResizeX.hWidget = ihWidget
       NO-ERROR.
  IF NOT AVAIL ttNeverResizeX THEN DO:
    CREATE ttNeverResizeX.
    ASSIGN ttNeverResizeX.hWidget = ihWidget
           ttNeverResizeX.hWindow = hWindow.
  END.
END.
ELSE DO:
  FIND FIRST ttNeverResizeY 
       WHERE ttNeverResizeY.hWidget = ihWidget
       NO-ERROR.
  IF NOT AVAIL ttNeverResizeY THEN DO:
    CREATE ttNeverResizeY.
    ASSIGN ttNeverResizeY.hWidget = ihWidget
           ttNeverResizeY.hWindow = hWindow.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProgressBarTimeOut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProgressBarTimeOut Procedure 
PROCEDURE ProgressBarTimeOut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiProgBar AS INT NO-UNDO.

/* MESSAGE PROGRAM-NAME(1) SKIP            */
/*         "cProgBarState: " cProgBarState */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.  */
IF cProgBarState = "timer" THEN DO:
  RUN setProgBarProperty ("position","0").
  cProgBarState = "stop".
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReadResizeLibIniSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadResizeLibIniSettings Procedure 
PROCEDURE ReadResizeLibIniSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
GET-KEY-VALUE SECTION "ResizeLib" KEY "MenuType" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN
  cGlobMenuType = cTemp.

GET-KEY-VALUE SECTION "ResizeLib" KEY "MenuLabel" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN DO:
  bGlobEnableSave = TRUE.
  DoCreateMenuStruct (?,"MENU","MenuLabel",cTemp).
END.
ELSE
  DoCreateMenuStruct (?,"MENU","MenuLabel","Options").

GET-KEY-VALUE SECTION "ResizeLib" KEY "ReservedColours" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN
  cGlobReservedColour = cTemp.

GET-KEY-VALUE SECTION "ResizeLib" KEY "ColourMenu" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN DO:
  DoCreateMenuStruct (?,"SUB-MENU","ColourMenu",cTemp).
  bGlobEnableColour = TRUE.

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Normal" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Normal",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Green" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Green",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Blue" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Blue",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Yellow" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Yellow",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Gray" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Gray",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Red" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Red",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Turquoise" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Turquoise",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "Turn" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","Turn",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "AllFrames" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN
    DoCreateMenuStruct (?,"MENU-ITEM","AllFrames",cTemp).
END.

GET-KEY-VALUE SECTION "ResizeLib" KEY "SaveSettingsMenu" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN DO:
  DoCreateMenuStruct (?,"SUB-MENU","SaveSettingsMenu",cTemp).
  bGlobEnableSave = TRUE.

  GET-KEY-VALUE SECTION "ResizeLib" KEY "DeleteSettingsMenu" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN 
    DoCreateMenuStruct (?,"SUB-MENU","DeleteSettingsMenu",cTemp).
END.

GET-KEY-VALUE SECTION "ResizeLib" KEY "HelpMenu" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN DO:
  DoCreateMenuStruct (?,IF cGlobMenuType MATCHES "*help*" THEN "MENU" ELSE "SUB-MENU","HelpMenu",cTemp).
  bGlobEnableHelp = TRUE.

  GET-KEY-VALUE SECTION "ResizeLib" KEY "defaultHelp" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN 
    DoCreateMenuStruct (?,"MENU-ITEM","defaultHelp",cTemp).
END.

GET-KEY-VALUE SECTION "ResizeLib" KEY "AboutMenu" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN DO:
  DoCreateMenuStruct (?,IF cGlobMenuType MATCHES "*help*" THEN "MENU" ELSE "SUB-MENU","AboutMenu",cTemp).
  bGlobEnableAbout = TRUE.

  GET-KEY-VALUE SECTION "ResizeLib" KEY "about1" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN 
    DoCreateMenuStruct (?,"MENU-ITEM","about1",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "about2" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN 
    DoCreateMenuStruct (?,"MENU-ITEM","about2",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "about3" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN 
    DoCreateMenuStruct (?,"MENU-ITEM","about3",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "about4" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN 
    DoCreateMenuStruct (?,"MENU-ITEM","about4",cTemp).

  GET-KEY-VALUE SECTION "ResizeLib" KEY "about5" VALUE cTemp.
  IF cTemp NE "" AND cTemp NE ? THEN 
    DoCreateMenuStruct (?,"MENU-ITEM","about5",cTemp).
END.

GET-KEY-VALUE SECTION "ResizeLib" KEY "NoMenu" VALUE cTemp.
IF cTemp NE "" AND cTemp NE ? THEN 
  ASSIGN bGlobEnableCommers  = FALSE
         bGlobEnableColour   = FALSE
         bGlobEnableHelp     = FALSE
         bGlobEnableAbout    = FALSE
         bGlobEnableSave     = FALSE.

GET-KEY-VALUE SECTION "ResizeLib" KEY "AutoSaveSettings" VALUE cTemp.
IF cTemp = "yes" THEN 
  ASSIGN bGlobAutoSaveSettings = TRUE
         bGlobEnableSave       = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setProgBarProperty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setProgBarProperty Procedure 
PROCEDURE setProgBarProperty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Commented out for stand-alone version of resizelib 
------------------------------------------------------------------------------*/
DEF INPUT PARAM icProperty AS CHAR NO-UNDO.
DEF INPUT PARAM icValue    AS CHAR NO-UNDO.
/*
CASE icProperty:
  WHEN "text" THEN
    RUN ThreadProgBarSetStringProperty(iProgBar,{&PROGBAR_PROP-TEXT},
                                icValue,OUTPUT iSuccess).
  WHEN "position" THEN DO:
    RUN ThreadProgBarSetBooleanProperty(iProgBar, {&PROGBAR_PROP-MARQUEE},0,OUTPUT iSuccess).
    RUN ThreadProgBarSetBooleanProperty(iProgBar, {&PROGBAR_PROP-BARSTYLE},
                                           {&PROGBAR_STYLE-GRADIENT},OUTPUT iSuccess).
    RUN ThreadProgBarSetPosition(iProgBar,INTEGER(icValue),OUTPUT iSuccess).
  END.
  WHEN "marquee" THEN
    RUN ThreadProgBarSetBooleanProperty(iProgBar, {&PROGBAR_PROP-MARQUEE},1,OUTPUT iSuccess).
  WHEN "timer" THEN DO:
    RUN ThreadProgBarSetIntegerProperty(iProgBar,
                                        {&PROGBAR_PROP-TIMER-TIMEOUT},
                                        INT(icValue), /* msec delay.*/
                                        OUTPUT iSuccess).
    RUN ThreadProgBarSetBooleanProperty(iProgBar,
                                       {&PROGBAR_PROP-TIMER},
                                       {&GEN_ENABLE},
                                       OUTPUT iSuccess).
    cProgBarState = icProperty.
  END.

  WHEN "WindowStartup" THEN DO:
    RUN setProgBarProperty ("marquee","").
    cProgBarState = icProperty.
  END.
  WHEN "endWindowStartup" THEN DO:
    RUN setProgBarProperty ("position","0").
    cProgBarState = icProperty.
  END.
  WHEN "startQuery" THEN DO:
    IF NOT CAN-DO("WindowStartup,timer",cProgBarState) THEN DO:
      RUN setProgBarProperty ("marquee","").
      cProgBarState = icProperty.
    END.
  END.
  WHEN "endQuery" THEN DO:
    IF NOT CAN-DO("WindowStartup,timer",cProgBarState) THEN DO:
      RUN setProgBarProperty ("timer","500").
    END.
  END.
END CASE.
*/
/* hMainHelp:SCREEN-VALUE = icProperty + " > " + cProgBarState.  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-AddLanguageSubCat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddLanguageSubCat Procedure 
FUNCTION AddLanguageSubCat RETURNS CHARACTER
  ( INPUT icFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cUseHelpLanguageSubCat AS CHAR NO-UNDO.
DEF VAR cLanguage              AS CHAR NO-UNDO.
DEF VAR bWinServer             AS LOG  NO-UNDO.

cUseHelpLanguageSubCat = DYNAMIC-FUNCTION("getAttribute",SESSION,"UseHelpLanguageSubCat") NO-ERROR.
IF cUseHelpLanguageSubCat = "yes" THEN DO:
  cLanguage = LC(DYNAMIC-FUNCTION("getLanguageCode")) NO-ERROR.
  IF cLanguage NE "" AND cLanguage NE ? THEN DO:
    ix = R-INDEX(icFile,"/").
    IF ix = 0 THEN
      ASSIGN ix = R-INDEX(icFile,"\")
             bWinServer = TRUE.
    IF ix NE 0 THEN
      RETURN SUBSTR(icFile,1,ix) + cLanguage + (IF bWinServer THEN "\" ELSE "/") + SUBSTR(icFile,ix + 1).
  END.
END.
ELSE RETURN icFile. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addSpecialResizeProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addSpecialResizeProc Procedure 
FUNCTION addSpecialResizeProc RETURNS CHARACTER
  ( INPUT iphProcedure AS HANDLE,
    INPUT iphFrame     AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FOR EACH ttAddSpecialResize:
    IF NOT VALID-HANDLE(hProcedure) THEN DELETE ttAddSpecialResize.
  END.

  FIND FIRST ttAddSpecialResize WHERE hProcedure = iphProcedure
                                  AND hFrame     = iphFrame
                                NO-LOCK NO-ERROR.
  IF NOT AVAIL ttAddSpecialResize THEN 
  do: 
    CREATE ttAddSpecialResize.
    ASSIGN 
      hProcedure = iphProcedure
      hFrame     = iphFrame
    .
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addTranslationMarkerRule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addTranslationMarkerRule Procedure 
FUNCTION addTranslationMarkerRule RETURNS LOGICAL
  ( INPUT ihTransMarker AS HANDLE,
    INPUT ihFollow      AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR chFrame     AS CHAR   NO-UNDO.
DEF VAR chWindow    AS CHAR   NO-UNDO.
DEF VAR cTransName  AS CHAR   NO-UNDO.
DEF VAR cFollowName AS CHAR   NO-UNDO.
DEF VAR hSplit      AS HANDLE NO-UNDO.
DEF VAR iDelta      AS INT    NO-UNDO.
DEF VAR fPrc        AS DEC    NO-UNDO.

ASSIGN chFrame     = STRING(ihTransMarker:FRAME)
       chWindow    = STRING(ihTransMarker:WINDOW)
       cTransName  = ihTransMarker:NAME
       cFollowName = ihFollow:NAME.


FIND FIRST ttTransMarker
     WHERE ttTransMarker.hWindow = ihTransMarker:WINDOW
       AND ttTransMarker.hMarker = ihTransMarker
     NO-ERROR.
IF NOT AVAIL ttTransMarker THEN DO:
  CREATE ttTransMarker.
  ASSIGN ttTransMarker.hWindow = ihTransMarker:WINDOW
         ttTransMarker.hMarker = ihTransMarker.
END.

FIND FIRST ttNoResizeX WHERE ttNoResizeX.chFrame = chFrame
                         AND ttNoResizeX.cWidget = cTransName
     NO-ERROR.
IF NOT AVAIL ttNoResizeX THEN DO:
  CREATE ttNoResizeX.
  ASSIGN ttNoResizeX.chWindow = chWindow
         ttNoResizeX.chFrame  = chFrame
         ttNoResizeX.cWidget  = cTransName.
END.
FIND FIRST ttNoResizeY WHERE ttNoResizeY.chFrame = chFrame
                         AND ttNoResizeY.cWidget = cTransName
     NO-ERROR.
IF NOT AVAIL ttNoResizeY THEN DO:
  CREATE ttNoResizeY.
  ASSIGN ttNoResizeY.chWindow = chWindow
         ttNoResizeY.chFrame  = chFrame
         ttNoResizeY.cWidget  = cTransName.
END.
  
FIND FIRST ttAddMoveX WHERE ttAddMoveX.chFrame = chFrame
                        AND ttAddMoveX.cWidget = cFollowName
     NO-ERROR.
IF AVAIL ttAddMoveX THEN DO:
  CREATE ttAddMoveX.
  ASSIGN ttAddMoveX.chWindow = chWindow
         ttAddMoveX.chFrame  = chFrame
         ttAddMoveX.cWidget  = cTransName.
END.

FIND FIRST ttNoMoveX WHERE ttNoMoveX.chFrame = chFrame
                        AND ttNoMoveX.cWidget = cFollowName
     NO-ERROR.
IF AVAIL ttNoMoveX THEN DO:
  CREATE ttNoMoveX.
  ASSIGN ttNoMoveX.chWindow = chWindow
         ttNoMoveX.chFrame  = chFrame
         ttNoMoveX.cWidget  = cTransName.
END.

FIND FIRST ttAddMoveY WHERE ttAddMoveY.chFrame = chFrame
                        AND ttAddMoveY.cWidget = cFollowName
     NO-ERROR.
IF AVAIL ttAddMoveY THEN DO:
  CREATE ttAddMoveY.
  ASSIGN ttAddMoveY.chWindow = chWindow
         ttAddMoveY.chFrame  = chFrame
         ttAddMoveY.cWidget  = cTransName.
END.

FIND FIRST ttNoMoveY WHERE ttNoMoveY.chFrame = chFrame
                        AND ttNoMoveY.cWidget = cFollowName
     NO-ERROR.
IF AVAIL ttNoMoveY THEN DO:
  CREATE ttNoMoveY.
  ASSIGN ttNoMoveY.chWindow = chWindow
         ttNoMoveY.chFrame  = chFrame
         ttNoMoveY.cWidget  = cTransName.
END.

FIND FIRST ttFollowSplitBarX WHERE ttFollowSplitBarX.chWindow = chWindow
                               AND ttFollowSplitBarX.hWidget  = ihFollow
     NO-ERROR.
IF AVAIL ttFollowSplitBarX THEN DO:
  ASSIGN hSplit = ttFollowSplitBarX.hSplitBar
         iDelta = ttFollowSplitBarX.iDeltaSplitBar.
  CREATE ttFollowSplitBarX.
  ASSIGN ttFollowSplitBarX.chWindow       = chWindow
         ttFollowSplitBarX.hSplitBar      = hSplit
         ttFollowSplitBarX.hWidget        = ihTransMarker
         ttFollowSplitBarX.iDeltaSplitBar = iDelta.
END.

FIND FIRST ttFollowSplitBarY WHERE ttFollowSplitBarY.chWindow = chWindow
                               AND ttFollowSplitBarY.hWidget  = ihFollow
     NO-ERROR.
IF AVAIL ttFollowSplitBarY THEN DO:
  ASSIGN hSplit = ttFollowSplitBarY.hSplitBar
         iDelta = ttFollowSplitBarY.iDeltaSplitBar.
  CREATE ttFollowSplitBarY.
  ASSIGN ttFollowSplitBarY.chWindow       = chWindow
         ttFollowSplitBarY.hSplitBar      = hSplit
         ttFollowSplitBarY.hWidget        = ihTransMarker
         ttFollowSplitBarY.iDeltaSplitBar = iDelta.
END.

FIND FIRST ttResizeXgroup WHERE ttResizeXgroup.hWindow = ihFollow:WINDOW
                        AND ttResizeXgroup.chFrame = chFrame
                        AND ttResizeXgroup.cWidget = cFollowName
     NO-ERROR.
IF AVAIL ttResizeXgroup THEN DO:
  fPrc = ttResizeXgroup.fPercent.
  CREATE ttResizeXgroup.
  ASSIGN ttResizeXgroup.hWindow  = ihFollow:WINDOW
         ttResizeXgroup.chFrame  = chFrame
         ttResizeXgroup.cWidget  = cTransName
         ttResizeXgroup.fPercent = fPrc.
END.

FIND FIRST ttResizeYgroup WHERE ttResizeYgroup.hWindow = ihFollow:WINDOW
                        AND ttResizeYgroup.chFrame = chFrame
                        AND ttResizeYgroup.cWidget = cFollowName
     NO-ERROR.
IF AVAIL ttResizeYgroup THEN DO:
  fPrc = ttResizeYgroup.fPercent.
  CREATE ttResizeYgroup.
  ASSIGN ttResizeYgroup.hWindow  = ihFollow:WINDOW
         ttResizeYgroup.chFrame  = chFrame
         ttResizeYgroup.cWidget  = cTransName
         ttResizeYgroup.fPercent = fPrc.
END.

FIND FIRST ttMoveXgroup WHERE ttMoveXgroup.hWindow = ihFollow:WINDOW
                        AND ttMoveXgroup.chFrame = chFrame
                        AND ttMoveXgroup.cWidget = cFollowName
     NO-ERROR.
IF AVAIL ttMoveXgroup THEN DO:
  fPrc = ttMoveXgroup.fPercent.
  CREATE ttMoveXgroup.
  ASSIGN ttMoveXgroup.hWindow  = ihFollow:WINDOW
         ttMoveXgroup.chFrame  = chFrame
         ttMoveXgroup.cWidget  = cTransName
         ttMoveXgroup.fPercent = fPrc.
END.

FIND FIRST ttMoveYgroup WHERE ttMoveYgroup.hWindow = ihFollow:WINDOW
                        AND ttMoveYgroup.chFrame = chFrame
                        AND ttMoveYgroup.cWidget = cFollowName
     NO-ERROR.
IF AVAIL ttMoveYgroup THEN DO:
  fPrc = ttMoveYgroup.fPercent.
  CREATE ttMoveYgroup.
  ASSIGN ttMoveYgroup.hWindow  = ihFollow:WINDOW
         ttMoveYgroup.chFrame  = chFrame
         ttMoveYgroup.cWidget  = cTransName
         ttMoveYgroup.fPercent = fPrc.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CopyBrowseToClipboard) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CopyBrowseToClipboard Procedure 
FUNCTION CopyBrowseToClipboard RETURNS LOGICAL
  ( INPUT hBrowse AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hQuery          AS WIDGET-HANDLE NO-UNDO.
DEF VAR hBuffer         AS WIDGET-HANDLE NO-UNDO.
DEF VAR hField          AS WIDGET-HANDLE NO-UNDO.
DEF VAR hColumn         AS HANDLE NO-UNDO.
DEF VAR iColumn         AS INT NO-UNDO.

DEF VAR cCurrDateFormat AS CHAR NO-UNDO.
DEF VAR cCurrNumFormat  AS CHAR NO-UNDO.
DEF VAR cFileName       AS CHAR NO-UNDO.
DEF VAR cOutput         AS CHAR NO-UNDO.
DEF VAR fDec            AS DEC FORMAT "->>,>>>,>>>,>>9.99".      

ASSIGN cCurrDateFormat = SESSION:DATE-FORMAT
       cCurrNumFormat  = SESSION:NUMERIC-FORMAT
       cFileName       = SESSION:TEMP-DIR + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt"
       .

IF NOT VALID-HANDLE(hBrowse) THEN RETURN FALSE.

CLIPBOARD:MULTIPLE = TRUE.

DO iColumn = 1 TO hBrowse:NUM-COLUMNS:
  ASSIGN hColumn = hBrowse:GET-BROWSE-COLUMN(iColumn)
         NO-ERROR.
  cOutput = cOutput + hColumn:LABEL + "~t".
END.
PUT UNFORMATTED cOutput SKIP.
cOutput = "". 
  
bOK = SESSION:SET-WAIT-STATE("General").
IF NOT hBrowse:MULTIPLE OR (hBrowse:MULTIPLE AND hBrowse:NUM-SELECTED-ROWS = 0) THEN DO:      
  CREATE QUERY hQuery.
  
  hQuery = hBrowse:QUERY.
  
  IF hQuery:GET-FIRST() THEN
  IF NOT hQuery:QUERY-OFF-END THEN
  DO:
      DO WHILE TRUE:
        IF hQuery:QUERY-OFF-END THEN DO:
          LEAVE.
        END.
        DO iColumn = 1 TO hBrowse:NUM-COLUMNS:
          IF hBrowse:GET-BROWSE-COLUMN(iColumn):VISIBLE THEN DO:
            ASSIGN hColumn = hBrowse:GET-BROWSE-COLUMN(iColumn)
                   hField = hColumn:BUFFER-FIELD NO-ERROR.
            IF hField:DATA-TYPE = "integer" OR hField:DATA-TYPE = "decimal" THEN DO:
              fDec = hField:BUFFER-VALUE.
              cOutput = cOutput + (IF fDec NE ? THEN STRING(fDec) ELSE "0") + "~t".
            END.
            ELSE IF hField:DATA-TYPE = "DATE" THEN 
              cOutput = cOutput + (IF hField:STRING-VALUE NE ? THEN hField:STRING-VALUE ELSE "") + "~t".
            ELSE 
              cOutput = cOutput + (IF hField:BUFFER-VALUE NE ? THEN REPLACE(hField:BUFFER-VALUE,CHR(10)," ") ELSE "") + "~t".
          END.
        END.
        CLIPBOARD:VALUE = cOutput.
        cOutput = "". 
        IF NOT hQuery:GET-NEXT() THEN LEAVE.
      END.
  END.
END.
ELSE 
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
    DO iColumn = 1 TO hBrowse:NUM-COLUMNS:
      IF hBrowse:GET-BROWSE-COLUMN(iColumn):VISIBLE THEN DO:
        ASSIGN hColumn = hBrowse:GET-BROWSE-COLUMN(iColumn)
               hField = hColumn:BUFFER-FIELD NO-ERROR.
        IF hField:DATA-TYPE = "integer" OR hField:DATA-TYPE = "decimal" THEN DO:
          fDec = hField:BUFFER-VALUE.
          cOutput = cOutput + (IF fDec NE ? THEN STRING(fDec) ELSE "0") + "~t".
        END.
        ELSE IF hField:DATA-TYPE = "DATE" THEN 
          cOutput = cOutput + (IF hField:STRING-VALUE NE ? THEN hField:STRING-VALUE ELSE "") + "~t".
        ELSE 
          cOutput = cOutput + (IF hField:BUFFER-VALUE NE ? THEN REPLACE(hField:BUFFER-VALUE,CHR(10)," ") ELSE "") + "~t".
      END.
    END.
    CLIPBOARD:VALUE = cOutput.
    cOutput = "". 
  END.
END.

CLIPBOARD:MULTIPLE = FALSE.
bOK = SESSION:SET-WAIT-STATE("").

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateStatusBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateStatusBar Procedure 
FUNCTION CreateStatusBar RETURNS LOGICAL
  ( INPUT ihWindow         AS HANDLE,
    INPUT icFieldPixelList AS CHARACTER,
    INPUT iiNumFields      AS INTEGER,
    INPUT ibTopLine        AS LOGICAL,
    INPUT iiFont           AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Modified from adecomm/_status.p, created by Greg O'Connor, PSC    
           The status bar must be created before any JukeBox objects
------------------------------------------------------------------------------*/
DEFINE VARIABLE cAddMoveXList       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAddResizeXList     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNoMoveXList        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNoMoveYList        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNoResizeXList      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNoResizeYList      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTextHandleList     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTopColor           AS INTEGER     NO-UNDO INIT 15.
DEFINE VARIABLE hRecordCountWidget  AS HANDLE      NO-UNDO.
DEFINE VARIABLE hRecordSelectWidget AS HANDLE      NO-UNDO.
DEFINE VARIABLE iFields             AS INTEGER     NO-UNDO.
DEFINE VARIABLE iFirstTextPixels    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iFontSize           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTotal              AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTotPixels          AS INTEGER     NO-UNDO.
DEFINE VARIABLE ix                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE wFrame2             AS WIDGET      NO-UNDO.
DEFINE VARIABLE wRect1              AS WIDGET      NO-UNDO.
DEFINE VARIABLE wRectFrame          AS WIDGET      NO-UNDO.
DEFINE VARIABLE wText               AS WIDGET      NO-UNDO.
DEFINE VARIABLE hHelp               AS HANDLE      NO-UNDO.
&SCOPED-DEFINE topBottom 3
&SCOPED-DEFINE rightLeft 2  /* 6 */

DEF VAR bProgressBar AS LOG    NO-UNDO.
DEF VAR hFramePB     AS HANDLE NO-UNDO.

IF DYNAMIC-FUNCTION("getAttribute",ihWindow,"StatusBarFrame") NE "" THEN RETURN NO.

IF icFieldPixelList = "progress-bar" THEN
  ASSIGN bProgressBar  = YES
         icFieldPixelList = ",110"
         iiNumFields   = 2.

IF ENTRY(1,icFieldPixelList) = "" THEN DO:
  IF icFieldPixelList = "" THEN DO:
    IF iiNumFields NE 0 THEN
      icFieldPixelList = icFieldPixelList + FILL(",80",iiNumFields - 1).
    ELSE
      icFieldPixelList = ",40,60".
  END.
  DO ix = 2 TO NUM-ENTRIES(icFieldPixelList):
    iTotPixels = iTotPixels + INTEGER(ENTRY(ix,icFieldPixelList)) + {&rightLeft} * 4.
  END.
  ENTRY(1,icFieldPixelList) = STRING(INT(ihWindow:WIDTH-PIXELS - {&rightLeft} * 4 - iTotPixels + NUM-ENTRIES(icFieldPixelList) * 1.4 - 1)).
END.

ASSIGN
  cTopColor = IF ibTopLine THEN 15 ELSE 8
  iFontSize = IF iiFont = ? THEN 1 ELSE iiFont.

PUBLISH "AdjustFormHeight" (ihWindow,
                            (IF ((SESSION:PIXELS-PER-ROW -
                                 FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(iFontSize)) >
                                    {&topBottom})
                             THEN SESSION:PIXELS-PER-ROW + 1
                             ELSE FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(iFontSize) +
                                 ({&topBottom} * 2) + 3)
                            ).

ihWindow:HEIGHT-PIXELS = ihWindow:HEIGHT-PIXELS +
                      (IF ((SESSION:PIXELS-PER-ROW -
                           FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(iFontSize)) > 
                              {&topBottom}) 
                       THEN SESSION:PIXELS-PER-ROW + 1
                       ELSE FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(iFontSize) +
                           ({&topBottom} * 2) + 3).

CREATE FRAME wFrame2
  ASSIGN
    BOX        = FALSE
    OVERLAY    = TRUE
    PARENT     = ihWindow
    X          = 0
    Y          = 0
    THREE-D    = YES
    SCROLLABLE = NO
    VISIBLE    = YES
    NAME       = "StatusFrame"
   /*
    The min size a frame can be is the size of a fillin which is bigger than 
    the size of text because of decoration. If the status bar and the margin
    fit in the the fillin space the use it. Otherwise compute the hieght based
    on the size plus top and bottom margin + a pixel or two so the rectanges are
    not over written by passing a screen value to the text widgets at run time
    */
    HEIGHT-PIXELS  = (IF ((SESSION:PIXELS-PER-ROW -
                           FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(iFontSize)) > 
                              {&topBottom}) 
                       THEN SESSION:PIXELS-PER-ROW + 1
                       ELSE FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(iFontSize) +
                           ({&topBottom} * 2) + 3)
    WIDTH-PIXELS   = ihWindow:WIDTH-PIXELS.

wFrame2:Y = ihWindow:HEIGHT-PIXELS - wFrame2:HEIGHT-PIXELS NO-ERROR.
/*
IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
  DO ix = 1 TO 20:
    wFrame2:Y = ihWindow:HEIGHT-PIXELS - wFrame2:HEIGHT-PIXELS - ix NO-ERROR.
    IF ERROR-STATUS:GET-MESSAGE(1) = "" THEN DO:
      MESSAGE ix
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      LEAVE.  
    END. 
  END.
  IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN
    MESSAGE ERROR-STATUS:GET-MESSAGE(1) SKIP
            "Adjust frame to match window size"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.     
END.
*/
cNoResizeYList = wFrame2:NAME.

CREATE RECTANGLE wRectFrame
  ASSIGN
    X             = 0
    Y             = iTotal
    WIDTH-PIXELS  = wFrame2:WIDTH-PIXELS 
    HEIGHT-PIXELS = wFrame2:HEIGHT-PIXELS
    FRAME         = wFrame2
    fgcolor       = 8
    filled        = false
    VISIBLE       = YES
    NAME          = "RectStatusFrame"
    .
ASSIGN cNoResizeYList = cNoResizeYList + "," + wRectFrame:NAME
       cNoMoveYList   = wRectFrame:NAME
       .

CREATE RECTANGLE wRect1 /* Black top line */
  ASSIGN
    WIDTH-PIXELS  = wFrame2:WIDTH-PIXELS 
    HEIGHT-PIXELS = 1 
    FRAME         = wFrame2
    bgcolor       = cTopColor
    fgcolor       = cTopColor
    filled        = FALSE
    X             = 0
    Y             = 0
    VISIBLE       = YES
    NAME          = "RectBlackTop"
    .

ASSIGN cNoResizeYList = cNoResizeYList + "," + wRect1:NAME
       cNoMoveYList   = cNoMoveYList + "," + wRect1:NAME
       iTotal = 0.

DO ix = 1 TO NUM-ENTRIES (icFieldPixelList):

  iFields = iFields + 1.

  CREATE FILL-IN wText
    ASSIGN
      FRAME         = wFrame2
      HEIGHT-PIXELS = wFrame2:HEIGHT-PIXELS - (({&topbottom} + 1) * 2)
      WIDTH-PIXELS  = INTEGER(ENTRY(ix,icFieldPixelList))
      FORMAT        = "X(256)"
      FONT          = iFontSize
      bgcolor       = wRectFrame:bgcolor
      /* Make sure not to cover the rectangles. */
      X             = iTotal + {&rightLeft} + 2 + 1
      Y             = {&topBottom} + 1    
      VISIBLE       = YES
      NAME          = "StatusBarTextFillIn" + STRING(iFields)
      .
         
  cNoMoveYList      = cNoMoveYList + "," + wText:NAME.  

  DYNAMIC-FUNCTION("setAttribute",ihWindow,wText:NAME,STRING(wText)).

  IF iFields = 1 THEN
    ASSIGN cAddResizeXList = wText:NAME
           hHelp           = wText.
  ELSE 
    ASSIGN cNoResizeXList = cNoResizeXList + (IF cNoResizeXList NE "" THEN "," ELSE "") + wText:NAME
           cAddMoveXList  = cAddMoveXList + (IF cAddMoveXList NE "" THEN "," ELSE "") + wText:NAME.

  cTextHandleList = IF NUM-ENTRIES (cTextHandleList) = 0 THEN 
                      STRING (wText:HANDLE) 
                    ELSE 
                      cTextHandleList + "," + STRING (wText:HANDLE).
        
  IF ix > 1 AND ix = NUM-ENTRIES(icFieldPixelList) THEN
    hRecordCountWidget = wText.
  IF NUM-ENTRIES(icFieldPixelList) > 2 AND ix = NUM-ENTRIES(icFieldPixelList) - 1 THEN
    hRecordSelectWidget = wText.


  CREATE RECTANGLE wRect1 /* White (bottom and left line) */
    ASSIGN
      WIDTH-PIXELS  = wText:WIDTH-PIXELS  + ({&rightLeft} * 2)
      HEIGHT-PIXELS = wFrame2:HEIGHT-PIXELS - ({&topBottom} * 2) 
      FRAME         = wFrame2
      bgcolor       = 8
      fgcolor       = 8
      filled        = NO 
      X             = iTotal + {&rightLeft} + 1
      Y             = wText:Y - 1
      EDGE-PIXELS   = 1
      VISIBLE       = YES
      NAME          = "StatusBarRectangle" + STRING(iFields)
      . 

  DYNAMIC-FUNCTION("setAttribute",ihWindow,wRect1:NAME,STRING(wRect1)).

  ASSIGN cNoResizeYList = cNoResizeYList + "," + wRect1:NAME
         cNoMoveYList   = cNoMoveYList + "," + wRect1:NAME
         .

  IF iFields > 1 THEN
    ASSIGN cNoResizeXList = cNoResizeXList + "," + wRect1:NAME
           cAddMoveXList  = cAddMoveXList + "," + wRect1:NAME.
        

  iTotal =  wText:X + wText:WIDTH-PIXELS /* + {&topBottom} + 1*/.
END.

wFrame2:PRIVATE-DATA = cTextHandleList.

IF bProgressBar THEN DO:
  CREATE FRAME hFramePB
    ASSIGN BOX        = FALSE
           OVERLAY    = TRUE
           PARENT     = ihWindow
           X          = wText:X + 1
           Y          = wFrame2:Y + {&topBottom} - 1
           THREE-D    = YES
           SCROLLABLE = YES
           VISIBLE    = YES
           NAME       = "ProgressBarFrame"
           HEIGHT-PIXELS  = wText:HEIGHT-PIXELS - 5
           WIDTH-PIXELS   = wText:WIDTH-PIXELS - 1
           VIRTUAL-HEIGHT-PIXELS = wText:HEIGHT-PIXELS
           VIRTUAL-WIDTH-PIXELS  = wText:WIDTH-PIXELS - 1
/*            BGCOLOR = 12 */
    TRIGGERS:
      ON "/" PERSISTENT RUN ProgressBarTimeOut (iProgBar). 
    END.
  
  DYNAMIC-FUNCTION("setAddMoveX",ihWindow,hFramePB,hFramePB:NAME).
  DYNAMIC-FUNCTION("setAddMoveY",ihWindow,hFramePB,hFramePB:NAME).
  DYNAMIC-FUNCTION("setNoResizeX",ihWindow,hFramePB,hFramePB:NAME).
  DYNAMIC-FUNCTION("setNoResizeY",ihWindow,hFramePB,hFramePB:NAME).

  ASSIGN wText:HIDDEN = YES
         wRect1:HIDDEN = YES.

  RUN ThreadProgBarCreate(hFramePB:HWND, OUTPUT iProgBar).

  SUBSCRIBE TO "setProgBarProperty" ANYWHERE.

  hMainHelp = hHelp.
END.

DYNAMIC-FUNCTION("NewObject",ihWindow,ihWindow,"Window").

DYNAMIC-FUNCTION("setAddMoveX",ihWindow,wFrame2,cAddMoveXList).
DYNAMIC-FUNCTION("setAddMoveY",ihWindow,wFrame2,wFrame2:NAME).
DYNAMIC-FUNCTION("setNoMoveX",ihWindow,wFrame2,cNoMoveXList).
DYNAMIC-FUNCTION("setNoMoveY",ihWindow,wFrame2,cNoMoveYList).
DYNAMIC-FUNCTION("setNoResizeX",ihWindow,wFrame2,cNoResizeXList).
DYNAMIC-FUNCTION("setNoResizeY",ihWindow,wFrame2,cNoResizeYList).
DYNAMIC-FUNCTION("setAddResizeX",ihWindow,wFrame2,cAddResizeXList).

DYNAMIC-FUNCTION("setAttribute",ihWindow,"StatusBarFrame",STRING(wFrame2)).
DYNAMIC-FUNCTION("setAttribute",ihWindow,"HelpTextWidget",STRING(hHelp)).
IF VALID-HANDLE(hRecordCountWidget) THEN
  DYNAMIC-FUNCTION("setAttribute",ihWindow,"RecordCountWidget",STRING(hRecordCountWidget)).
IF VALID-HANDLE(hRecordSelectWidget) THEN
  DYNAMIC-FUNCTION("setAttribute",ihWindow,"RecordSelectWidget",STRING(hRecordSelectWidget)).


/* PUBLISH "AdjustFormHeight" (ihWindow,                                            */
/*                             (IF ((SESSION:PIXELS-PER-ROW -                       */
/*                                  FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(iFontSize)) > */
/*                                     {&topBottom})                                */
/*                              THEN SESSION:PIXELS-PER-ROW + 1                     */
/*                              ELSE FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(iFontSize) + */
/*                                  ({&topBottom} * 2) + 3)                         */
/*                             ).                                                   */


RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteWinSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DeleteWinSettings Procedure 
FUNCTION DeleteWinSettings RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RUN DoDeleteWinSettings (ihWindow).
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoAppendRule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoAppendRule Procedure 
FUNCTION DoAppendRule RETURNS LOGICAL
  ( INPUT ihRuleBuffer AS HANDLE,
    INPUT ihWidget     AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Handles only tables that can be appended without copying content
------------------------------------------------------------------------------*/

ihRuleBuffer:BUFFER-CREATE().
ASSIGN ihRuleBuffer:BUFFER-FIELD("chWindow"):BUFFER-VALUE = STRING(ihWidget:WINDOW) 
       ihRuleBuffer:BUFFER-FIELD("chFrame"):BUFFER-VALUE = STRING(ihWidget:FRAME) 
       ihRuleBuffer:BUFFER-FIELD("cWidget"):BUFFER-VALUE = ihWidget:NAME.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoCreateMenuStruct) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoCreateMenuStruct Procedure 
FUNCTION DoCreateMenuStruct RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE,
    INPUT icType   AS CHAR,
    INPUT icName   AS CHAR,
    INPUT icLabel  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttMenuStruct
     WHERE ttMenuStruct.hWindow = ihWindow
       AND ttMenuStruct.cName   = icName
     NO-ERROR.
IF NOT AVAIL ttMenuStruct THEN DO:
  CREATE ttMenuStruct.
  ASSIGN ttMenuStruct.hWindow = ihWindow
         ttMenuStruct.cName   = icName.
  IF icType = "MENU-ITEM" AND icName BEGINS "about" THEN
    iGlobNumAboutMenu = iGlobNumAboutMenu + 1.
END.
ASSIGN ttMenuStruct.cType   = icType
       ttMenuStruct.hParent = ?
       ttMenuStruct.cLabel  = icLabel
       ttMenuStruct.cAction = ""
       ttMenuStruct.cParam  = "".

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoFollowSplitBarX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoFollowSplitBarX Procedure 
FUNCTION DoFollowSplitBarX RETURNS LOGICAL
  (INPUT cProcessTypes  AS CHAR,
   INPUT cSkipTypes     AS CHAR ) :
/*------------------------------------------------------------------------------
   Purpose: When the widgets following av move of the splitbar are specified in a follow-table
            process the table rather than climbing the widget-tree to be able to handle frames, etc
     Usage: Internal call in library from setSplitBar*
      Note: DeltaX < 0 means that splitbar is moved from left to right(!)
            DeltaY < 0 means moving splitbar top-down 
            cProcessTypes: In this run: Only process these types
------------------------------------------------------------------------------*/
DEF VAR hWidget      AS WIDGET-HANDLE NO-UNDO.
DEF VAR hWidget2     AS WIDGET-HANDLE NO-UNDO.
DEF VAR hParentFrame AS WIDGET-HANDLE NO-UNDO.
DEF VAR iAbsoluteX   AS INT NO-UNDO.
DEF VAR iAbsoluteY   AS INT NO-UNDO.
DEF VAR cName        AS CHAR NO-UNDO.

DEF VAR bMon AS LOG NO-UNDO.

IF cProcessTypes = "FRAME" THEN cFrameList = "".

FOR EACH ttFollowSplitBarX
    WHERE ttFollowSplitBarX.chWindow  = STRING(hCurrWindow)
      AND ttFollowSplitBarX.hSplitBar = ttSplitBarX.hSplitBar
    :

/*   cName = ttFollowSplitBarX.hWidget:NAME.                             */
/*   IF ttFollowSplitBarX.hWidget:NAME = "btnSplitBarY" OR bMon THEN DO: */
/*     MESSAGE PROGRAM-NAME(1) SKIP 10 SKIP                              */
/*             ttFollowSplitBarX.hWidget:NAME SKIP                       */
/*             ttFollowSplitBarX.hWidget:TYPE SKIP                       */
/*             "monitor?"                                                */
/*             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bMon.    */
/*   END.                                                                */

  IF NOT VALID-HANDLE(ttFollowSplitBarX.hWidget) THEN NEXT.
  IF cProcessTypes NE "" AND NOT CAN-DO(cProcessTypes,ttFollowSplitBarX.hWidget:TYPE) THEN NEXT.
  IF cSkipTypes NE "" AND CAN-DO(cSkipTypes,ttFollowSplitBarX.hWidget:TYPE) THEN NEXT.

  IF cProcessTypes = "FRAME" THEN cFrameList = cFrameList + STRING(ttFollowSplitBarX.hWidget) + ",".

  hParentFrame = ?.
  IF ttFollowSplitBarX.hWidget:TYPE = "FRAME" THEN 
    hParentFrame = ttFollowSplitBarX.hWidget.
  /* Retrieve widget absolute position in window (and set parent frame - if not the widget is a frame). */
  ASSIGN hWidget2   = ttFollowSplitBarX.hWidget:PARENT
         iAbsoluteX = ttFollowSplitBarX.hWidget:X
         iAbsoluteY = ttFollowSplitBarX.hWidget:Y.
  REPEAT WHILE hWidget2:TYPE NE "WINDOW":
    IF ttFollowSplitBarX.hWidget:TYPE NE "FRAME" AND hWidget2:TYPE = "FRAME" AND hParentFrame = ? THEN 
      hParentFrame = hWidget2.
    IF hWidget2:TYPE = "FRAME" THEN
      ASSIGN iAbsoluteX = iAbsoluteX + hWidget2:X
             iAbsoluteY = iAbsoluteY + hWidget2:Y.
    hWidget2 = hWidget2:PARENT.
  END.
      
  /* Move widgets to the left of splitbar.
     Only done for widgets not embedded in a frame or the frame.
     When size increases move before resize */ 
  IF iAbsoluteX GE ttSplitBarX.iPos 
     AND iDeltaX > 0
     AND (NOT CAN-DO(cFrameList,STRING(ttFollowSplitBarX.hWidget:FRAME)) 
          OR ttFollowSplitBarX.hWidget:TYPE = "FRAME")
     THEN 
    ttFollowSplitBarX.hWidget:X = ttSplitBarX.hSplitBar:X + ttFollowSplitBarX.iDeltaSplitBar NO-ERROR.
  
  /* Resize: */
  IF CAN-DO(ttResizeTypes.cTypes,ttFollowSplitBarX.hWidget:TYPE) OR 
     CAN-FIND(FIRST ttAddResizeX
              WHERE ttAddResizeX.chWindow = STRING(hCurrWindow)
                AND ttAddResizeX.chFrame  = STRING(hParentFrame)
                AND ttAddResizeX.cWidget  = ttFollowSplitBarX.hWidget:NAME) 
    THEN DO:
        
    /* Resize widgets left of splitbar: */   
    IF iAbsoluteX LE ttSplitBarX.iPos     
       THEN DO:
                     
      ttFollowSplitBarX.hWidget:WIDTH-PIXELS = ttFollowSplitBarX.hWidget:WIDTH-PIXELS - iDeltaX NO-ERROR.
      IF ttFollowSplitBarX.hWidget:TYPE = "FRAME" THEN
        ttFollowSplitBarX.hWidget:VIRTUAL-WIDTH-PIXELS = ttFollowSplitBarX.hWidget:WIDTH-PIXELS NO-ERROR.
      
      /* Add to move-to-top list for proper display after resize: */
      IF hParentFrame NE ttSplitBarX.hMainFrame THEN
        ASSIGN cMoveToTopList = cMoveToTopList + STRING(hParentFrame) + ","
               NO-ERROR.
      cMoveToTopList = cMoveToTopList + STRING(ttFollowSplitBarX.hWidget) + ",".

      /* If this widget is a splitbar frame include resize of the splitbar: */
      IF AVAIL ttSplitBarY AND ttFollowSplitBarX.hWidget = ttSplitBarY.hSplitFrame THEN
        ttSplitBarY.hSplitBar:WIDTH-PIXELS = ttSplitBarY.hSplitBar:WIDTH-PIXELS - iDeltaX NO-ERROR.
    END.
                                  
    /* Resize widgets to the right of splitbar: */   
    ELSE IF iAbsoluteX GE ttSplitBarX.iPos 
      AND NOT CAN-FIND(FIRST ttNoResizeX WHERE ttNoResizeX.chWindow = STRING(hCurrWindow)
                                           AND ttNoResizeX.chFrame  = STRING(hParentFrame)
                                           AND ttNoResizeX.cWidget  = ttFollowSplitBarX.hWidget:NAME)
      AND NOT (ttFollowSplitBarX.hWidget:TYPE = "RECTANGLE" AND ttFollowSplitBarX.hWidget:WIDTH-PIXELS = 2 AND ttFollowSplitBarX.hWidget:HEIGHT-PIXELS = 25) /* Toolbar Rule */
       THEN DO:
      
      ttFollowSplitBarX.hWidget:WIDTH-PIXELS = ttFollowSplitBarX.hWidget:WIDTH-PIXELS + iDeltaX NO-ERROR.
      IF ttFollowSplitBarX.hWidget:TYPE = "FRAME" THEN
        ttFollowSplitBarX.hWidget:VIRTUAL-WIDTH-PIXELS = ttFollowSplitBarX.hWidget:WIDTH-PIXELS NO-ERROR.

      hWidget2 = ttFollowSplitBarX.hWidget:SIDE-LABEL-HANDLE NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
        hWidget2:X = ttFollowSplitBarX.hWidget:X - hWidget2:WIDTH-PIXELS NO-ERROR.

      /* Add to move-to-top list for proper display after resize: */
      IF hParentFrame NE ttSplitBarX.hMainFrame THEN
        ASSIGN cMoveToTopList = cMoveToTopList + STRING(hParentFrame) + ","
               NO-ERROR.
      cMoveToTopList = cMoveToTopList + STRING(ttFollowSplitBarX.hWidget) + ",".      

      IF AVAIL ttSplitBarY AND ttFollowSplitBarX.hWidget = ttSplitBarY.hSplitFrame THEN 
        ttSplitBarY.hSplitBar:WIDTH-PIXELS = ttSplitBarY.hSplitBar:WIDTH-PIXELS + iDeltaX NO-ERROR.

      FOR EACH ttAnchor
          WHERE ttAnchor.hWindow = hCurrWindow
            AND ttAnchor.hAnchor = ttFollowSplitBarX.hWidget
            AND ttAnchor.iAnchorDeltaX NE 0:
        ttAnchor.hWidget:X = ttAnchor.hAnchor:X + ttAnchor.hAnchor:WIDTH-PIXELS + ttAnchor.iAnchorDeltaX NO-ERROR.

        hWidget2 = ttAnchor.hWidget:SIDE-LABEL-HANDLE NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
          hWidget2:X = ttAnchor.hWidget:X - hWidget2:WIDTH-PIXELS NO-ERROR.
          
      END.
    END.    

    IF ttFollowSplitBarX.hWidget:TYPE = "BROWSE" THEN
      APPLY "end-move" TO ttFollowSplitBarX.hWidget.
  END.  
  ELSE IF iAbsoluteX LT ttSplitBarX.iPos THEN
    ttFollowSplitBarX.hWidget:X = ttSplitBarX.hSplitBar:X + ttFollowSplitBarX.iDeltaSplitBar NO-ERROR.

  /* Move widgets to the right of splitbar.
     Only done for widgets not embedded in a frame spesified in the follow-table or the frame.
     When size decreases move after resize */ 
  IF iAbsoluteX GE ttSplitBarX.iPos 
     AND iDeltaX < 0
     AND (NOT CAN-DO(cFrameList,STRING(ttFollowSplitBarX.hWidget:FRAME))
          OR ttFollowSplitBarX.hWidget:TYPE = "FRAME")
    THEN DO:

    ttFollowSplitBarX.hWidget:X = ttSplitBarX.hSplitBar:X + ttFollowSplitBarX.iDeltaSplitBar NO-ERROR.

    hWidget2 = ttFollowSplitBarX.hWidget:SIDE-LABEL-HANDLE NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      hWidget2:X = ttFollowSplitBarX.hWidget:X - hWidget2:WIDTH-PIXELS NO-ERROR.
  END.

  /* If any widgets within this frame are assigned to "AddMoveX" they should keep their absolute position
     when the splitbar is moved. Their movement is handeled by the resize function: */
  IF ttFollowSplitBarX.hWidget:TYPE = "FRAME" THEN DO:
    hWidget = ttFollowSplitBarX.hWidget:FIRST-CHILD:FIRST-CHILD.
    REPEAT WHILE VALID-HANDLE(hWidget):
      IF CAN-FIND(FIRST ttAddMoveX WHERE ttAddMoveX.chWindow = STRING(hCurrWindow)
                                     AND ttAddMoveX.chFrame  = STRING(ttFollowSplitBarX.hWidget) 
                                     AND ttAddMoveX.cWidget  = hWidget:NAME) THEN DO:

        hWidget:X = hWidget:X + iDeltaX.  
        hWidget2 = hWidget:SIDE-LABEL-HANDLE NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
          hWidget2:X = hWidget:X - hWidget2:WIDTH-PIXELS NO-ERROR.
      END.
      hWidget = hWidget:NEXT-SIBLING.
    END.
  END.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoFollowSplitBarY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoFollowSplitBarY Procedure 
FUNCTION DoFollowSplitBarY RETURNS LOGICAL
  (INPUT cProcessTypes  AS CHAR,
   INPUT cSkipTypes     AS CHAR ) :
/*------------------------------------------------------------------------------
   Purpose: When the widgets following av move of the splitbar are specified in a follow-table
            process the table rather than climbing the widget-tree to be able to handle frames, etc
     Usage: Internal call in library from setSplitBar*
      Note: DeltaX < 0 means that splitbar is moved from left to right(!)
            DeltaY < 0 means moving splitbar top-down 
            cProcessTypes: In this run: Only process these types
------------------------------------------------------------------------------*/
DEF VAR hWidget2     AS WIDGET-HANDLE NO-UNDO.
DEF VAR hParentFrame AS WIDGET-HANDLE NO-UNDO.
DEF VAR iAbsoluteX   AS INT NO-UNDO.
DEF VAR iAbsoluteY   AS INT NO-UNDO.

IF cProcessTypes = "FRAME" THEN cFrameList = "".

FOR EACH ttFollowSplitBarY
    WHERE ttFollowSplitBarY.chWindow  = STRING(hCurrWindow)
      AND ttFollowSplitBarY.hSplitBar = ttSplitBarY.hSplitBar
    :


  IF NOT VALID-HANDLE(ttFollowSplitBarY.hWidget) THEN NEXT.
  IF cProcessTypes NE "" AND NOT CAN-DO(cProcessTypes,ttFollowSplitBarY.hWidget:TYPE) THEN NEXT.
  IF cSkipTypes NE "" AND CAN-DO(cSkipTypes,ttFollowSplitBarY.hWidget:TYPE) THEN NEXT.

  /****** addSpecialResize for SmartPak ****/
  FIND FIRST ttAddSpecialResize WHERE ttAddSpecialResize.hFrame = hWidget NO-LOCK NO-ERROR. /* Smart Frame Flolder object (from Smartpak ) */
  IF AVAIL ttAddSpecialResize AND VALID-HANDLE(ttAddSpecialResize.hProcedure) THEN
  DO:
    ASSIGN
      hWidget:HIDDEN                = TRUE 
      hWidget:SCROLLABLE            = TRUE 
      hWidget:VIRTUAL-WIDTH-PIXELS  = hWidget:WIDTH-PIXELS 
      hWidget:VIRTUAL-HEIGHT-PIXELS = hWidget:HEIGHT-PIXELS 
      hWidget:WIDTH-PIXELS          = 
      IF CAN-FIND(FIRST ttNoResizeX WHERE 
                  ttNoResizeX.chFrame = string(ttAddSpecialResize.hFrame) 
              AND ttNoResizeX.cWidget = ttAddSpecialResize.hFrame:NAME) 
              THEN hWidget:WIDTH-PIXELS ELSE  hWidget:WIDTH-PIXELS  + iDeltaX

      hWidget:HEIGHT-PIXELS         = 
      IF CAN-FIND(FIRST ttNoResizeY WHERE 
                        ttNoResizeY.chFrame = string(ttAddSpecialResize.hFrame) 
                    AND ttNoResizeY.cWidget = ttAddSpecialResize.hFrame:NAME) 
                    THEN hWidget:HEIGHT-PIXELS ELSE hWidget:HEIGHT-PIXELS + iDeltaY
    .
   
    RUN resizeObject IN ttAddSpecialResize.hProcedure (hWidget:HEIGHT,  hWidget:WIDTH). 
    hWidget = hWidget:NEXT-SIBLING.       
    NEXT.
  END.
  /****** addSpecialResize ****/

  IF cProcessTypes = "FRAME" THEN cFrameList = cFrameList + STRING(ttFollowSplitBarY.hWidget) + ",".

  hParentFrame = ?.
  IF hWidget:TYPE = "FRAME" THEN hParentFrame = hWidget.
  /* Retrieve widget absolute position in window (and set parent frame - if not the widget is a frame). */
  ASSIGN hWidget2   = ttFollowSplitBarY.hWidget:PARENT
         iAbsoluteX = ttFollowSplitBarY.hWidget:X
         iAbsoluteY = ttFollowSplitBarY.hWidget:Y.
  REPEAT WHILE hWidget2:TYPE NE "WINDOW":
    IF ttFollowSplitBarY.hWidget:TYPE NE "FRAME" AND hWidget2:TYPE = "FRAME" AND hParentFrame = ? THEN 
      hParentFrame = hWidget2.
    IF hWidget2:TYPE = "FRAME" THEN
      ASSIGN iAbsoluteX = iAbsoluteX + hWidget2:X
             iAbsoluteY = iAbsoluteY + hWidget2:Y.
    hWidget2 = hWidget2:PARENT.
  END.
     
      
  /* Move widgets under splitbar.
     Only done for widgets not embedded in a frame or the frame.
     When size increases move before resize */ 
  IF iAbsoluteY GE ttSplitBarY.iPos 
     AND iDeltaY > 0
     AND (NOT CAN-DO(cFrameList,STRING(ttFollowSplitBarY.hWidget:FRAME))
          OR ttFollowSplitBarY.hWidget:TYPE = "FRAME")
     THEN DO:
    ttFollowSplitBarY.hWidget:Y = ttSplitBarY.hSplitBar:Y + ttFollowSplitBarY.iDeltaSplitBar NO-ERROR.
  
    hWidget2 = ttFollowSplitBarY.hWidget:SIDE-LABEL-HANDLE NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      hWidget2:Y = hWidget:Y NO-ERROR.
  END.


  /* Resize: */
  IF CAN-DO(ttResizeTypes.cTypes,ttFollowSplitBarY.hWidget:TYPE) OR 
     CAN-FIND(FIRST ttAddResizeY
              WHERE ttAddResizeY.chWindow = STRING(hCurrWindow)
                AND ttAddResizeY.chFrame  = STRING(hParentFrame)
                AND ttAddResizeY.cWidget  = ttFollowSplitBarY.hWidget:NAME)
     THEN DO:
        
    /* Resize browse on top of splitbar: */   
    IF iAbsoluteY LE ttSplitBarY.iPos     
       THEN DO:
      ttFollowSplitBarY.hWidget:HEIGHT-PIXELS = ttFollowSplitBarY.hWidget:HEIGHT-PIXELS - iDeltaY NO-ERROR.
      IF ttFollowSplitBarY.hWidget:TYPE = "FRAME" THEN
        ttFollowSplitBarY.hWidget:VIRTUAL-HEIGHT-PIXELS = ttFollowSplitBarY.hWidget:HEIGHT-PIXELS NO-ERROR.
      
      /* Add to move-to-top list for proper display after resize: */
      IF hParentFrame NE ttSplitBarY.hMainFrame THEN
        ASSIGN cMoveToTopList = cMoveToTopList + STRING(hParentFrame) + ","
               NO-ERROR.
      cMoveToTopList = cMoveToTopList + STRING(ttFollowSplitBarY.hWidget) + ",".

      /* If this widget is a splitbar frame include resize of the splitbar: */
      IF AVAIL ttSplitBarX AND ttFollowSplitBarY.hWidget = ttSplitBarX.hSplitFrame THEN
        ttSplitBarX.hSplitBar:HEIGHT-PIXELS = ttSplitBarX.hSplitBar:HEIGHT-PIXELS - iDeltaY NO-ERROR.
    END.
                                  
    /* Resize browse under splitbar: */   
    ELSE IF iAbsoluteY GE ttSplitBarY.iPos 
      AND NOT CAN-FIND(FIRST ttNoResizeY WHERE ttNoResizeY.chWindow = STRING(hCurrWindow)
                                           AND ttNoResizeY.chFrame  = STRING(hParentFrame)
                                           AND ttNoResizeY.cWidget  = ttFollowSplitBarY.hWidget:NAME)
      THEN DO:
      
      ttFollowSplitBarY.hWidget:HEIGHT-PIXELS = ttFollowSplitBarY.hWidget:HEIGHT-PIXELS + iDeltaY NO-ERROR.
      IF ttFollowSplitBarY.hWidget:TYPE = "FRAME" THEN
        ttFollowSplitBarY.hWidget:VIRTUAL-HEIGHT-PIXELS = ttFollowSplitBarY.hWidget:HEIGHT-PIXELS NO-ERROR.

      /* Add to move-to-top list for proper display after resize: */
      IF hParentFrame NE ttSplitBarY.hMainFrame THEN
        ASSIGN cMoveToTopList = cMoveToTopList + STRING(hParentFrame) + ","
               NO-ERROR.
      cMoveToTopList = cMoveToTopList + STRING(ttFollowSplitBarY.hWidget) + ",".      

      /* If this widget is a splitbar frame include resize of the splitbar: */
      IF AVAIL ttSplitBarX AND ttFollowSplitBarY.hWidget = ttSplitBarX.hSplitFrame THEN
        ttSplitBarX.hSplitBar:HEIGHT-PIXELS = ttSplitBarX.hSplitBar:HEIGHT-PIXELS + iDeltaY.


      FOR EACH ttAnchor
          WHERE ttAnchor.hWindow = hCurrWindow
            AND ttAnchor.hAnchor = ttFollowSplitBarY.hWidget
            AND ttAnchor.iAnchorDeltaY NE 0:
        ttAnchor.hWidget:Y = ttAnchor.hAnchor:Y + ttAnchor.hAnchor:HEIGHT-PIXELS + ttAnchor.iAnchorDeltaY NO-ERROR.

        hWidget2 = ttAnchor.hWidget:SIDE-LABEL-HANDLE NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
          hWidget2:Y = ttAnchor.hWidget:Y NO-ERROR.
      END.
    END.    

    IF ttFollowSplitBarY.hWidget:TYPE = "BROWSE" THEN
      APPLY "end-move" TO ttFollowSplitBarY.hWidget.
  END.  
  ELSE IF iAbsoluteY LT ttSplitBarY.iPos THEN DO:
    ttFollowSplitBarY.hWidget:Y = ttSplitBarY.hSplitBar:Y + ttFollowSplitBarY.iDeltaSplitBar NO-ERROR.
    hWidget2 = ttFollowSplitBarY.hWidget:SIDE-LABEL-HANDLE NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      hWidget2:Y = ttFollowSplitBarY.hWidget:Y NO-ERROR.
  END.

  /* Move widgets under splitbar.
     Only done for widgets not embedded in a frame or the frame.
     When size decreases move after resize */ 
  IF iAbsoluteY GE ttSplitBarY.iPos 
     AND iDeltaY < 0
     AND ttFollowSplitBarY.iDeltaSplitBar > 0
    AND (NOT CAN-DO(cFrameList,STRING(ttFollowSplitBarY.hWidget:FRAME))
         OR ttFollowSplitBarY.hWidget:TYPE = "FRAME")
    THEN DO:

    ttFollowSplitBarY.hWidget:Y = ttSplitBarY.hSplitBar:Y + ttFollowSplitBarY.iDeltaSplitBar NO-ERROR.

    hWidget2 = ttFollowSplitBarY.hWidget:SIDE-LABEL-HANDLE NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      hWidget2:Y = ttFollowSplitBarY.hWidget:Y NO-ERROR.
  END.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoGetMenuLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoGetMenuLabel Procedure 
FUNCTION DoGetMenuLabel RETURNS CHARACTER
  ( INPUT ihWindow AS HANDLE,
    INPUT icName   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttMenuStruct
     WHERE ttMenuStruct.hWindow = ihWindow
       AND ttMenuStruct.cName   = icName
     NO-LOCK NO-ERROR.
IF AVAIL ttMenuStruct THEN
  RETURN ttMenuStruct.cLabel.
ELSE DO:
  FIND FIRST ttMenuStruct
       WHERE ttMenuStruct.cName = icName
       NO-LOCK NO-ERROR.
  IF AVAIL ttMenuStruct THEN
    RETURN ttMenuStruct.cLabel.
  ELSE DO:
    CASE icName:
      WHEN "SaveSettingsMenu" THEN
        icName = "Save Settings". 
      WHEN "DeleteSettingsMenu" THEN
        icName = "Delete Settings". 
    END CASE.
    IF LENGTH(icName) > 4 AND SUBSTR(icName,LENGTH(icName) - 3) = "menu" THEN
      RETURN RIGHT-TRIM(icName,"Menu").
    ELSE RETURN icName.
  END.
END.

RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoLoadSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoLoadSettings Procedure 
FUNCTION DoLoadSettings RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  Load the window settings from file, and execute the settings after all initialisation is done
            regardless of initialisation-sequence as long as the last function call i unique (!)
    Notes:  When multiple frames it migh be that the main frame is not FIRST-CHILD 
            this is adjusted here and i setBGcolour
------------------------------------------------------------------------------*/
DEF VAR hBogus         AS HANDLE NO-UNDO. /* For purpose mentioned over */
DEF VAR cUserSettings  AS CHAR   NO-UNDO. 
DEF VAR cSettingValues AS CHAR   NO-UNDO.

IF NOT VALID-HANDLE(ihWindow) THEN RETURN FALSE.
IF NOT VALID-HANDLE(ihWindow:FIRST-CHILD) THEN RETURN FALSE.

ASSIGN ihWindow:VIRTUAL-WIDTH-PIXELS  = SESSION:WIDTH-PIXELS 
       ihWindow:VIRTUAL-HEIGHT-PIXELS = SESSION:HEIGHT-PIXELS.


IF cResizeUser = "" AND USERID(LDBNAME(1)) = ? THEN
  cResizeUser = OS-GETENV("Username").
ELSE IF cResizeUser = "" THEN
  cResizeUser = USERID(LDBNAME(1)).

IF NOT CAN-FIND(FIRST ttSettingsLoaded WHERE ttSettingsLoaded.hWindow = ihWindow) AND (ETIME > iLoadTime OR bJBoxUserSetting) THEN DO:
  CREATE ttSettingsLoaded.
  ASSIGN ttSettingsLoaded.hWindow       = ihWindow
         ttSettingsLoaded.cSettingsName = getSaveSettingName(ihWindow)
         hTemp                          = ihWindow
         cSaveSettingsName              = ""
         .

  IF bJBoxUserSetting THEN DO:
    cUserSettings = DYNAMIC-FUNCTION("getUserSetting",
                     IF AVAIL ttOrgWinSize THEN ttOrgWinSize.hProcedure:FILE-NAME ELSE "",
                     "resizelib",
                     DYNAMIC-FUNCTION("getAttribute",ihWindow,"usersettingcontext"),
                     "windowxpos,windowypos,deltaxgrowth,deltaygrowth,splitbarxpos,splitbarypos,customwinsetting,wincolor,wincolordir").
    IF cUserSettings NE "" THEN DO:
      ASSIGN cSettingValues = ENTRY(2,cUserSettings,"|")
             cUserSettings  = ENTRY(1,cUserSettings,"|").
      DO ix = 1 TO NUM-ENTRIES(cUserSettings):
        CASE ENTRY(ix,cUserSettings):
          WHEN "windowxpos"       THEN ttSettingsLoaded.iCurrX         = INT(ENTRY(ix,cSettingValues,CHR(1))) NO-ERROR.
          WHEN "windowypos"       THEN ttSettingsLoaded.iCurrY         = INT(ENTRY(ix,cSettingValues,CHR(1))) NO-ERROR.
          WHEN "deltaxgrowth"     THEN ttSettingsLoaded.iCurrDeltaX    = INT(ENTRY(ix,cSettingValues,CHR(1))) NO-ERROR.
          WHEN "deltaygrowth"     THEN ttSettingsLoaded.iCurrDeltaY    = INT(ENTRY(ix,cSettingValues,CHR(1))) NO-ERROR.
          WHEN "splitbarxpos"     THEN ttSettingsLoaded.iCurrSplitBarX = INT(ENTRY(ix,cSettingValues,CHR(1))) NO-ERROR.
          WHEN "splitbarypos"     THEN ttSettingsLoaded.iCurrSplitBarY = INT(ENTRY(ix,cSettingValues,CHR(1))) NO-ERROR.
          WHEN "customwinsetting" THEN ttSettingsLoaded.cCustomSetting = ENTRY(ix,cSettingValues,CHR(1)).
          WHEN "wincolor"         THEN ttSettingsLoaded.cColour        = ENTRY(ix,cSettingValues,CHR(1)).
          WHEN "wincolordir"      THEN ttSettingsLoaded.cColourDir     = ENTRY(ix,cSettingValues,CHR(1)).
        END CASE.
      END.
    END.
  END.
  ELSE IF SEARCH(cDirectory + "resizelib-" + cResizeUser + "-" + ttSettingsLoaded.cSettingsName + ".ini") NE ?
     THEN DO:
    
    INPUT FROM VALUE(cDirectory + "resizelib-" + cResizeUser + "-" + ttSettingsLoaded.cSettingsName + ".ini").

    IMPORT DELIMITER ";" 
           ttSettingsLoaded.cColour 
           ttSettingsLoaded.cColourDir
           ttSettingsLoaded.iCurrX 
           ttSettingsLoaded.iCurrY 
           ttSettingsLoaded.iCurrDeltaX 
           ttSettingsLoaded.iCurrDeltaY 
           ttSettingsLoaded.iCurrSplitBarX 
           ttSettingsLoaded.iCurrSplitBarY 
           ttSettingsLoaded.cLastLoadProc
           ttSettingsLoaded.cCustomSetting 
           .   
    INPUT CLOSE.
  END.
  ETIME(TRUE).
END.

IF hTemp NE ? AND hTemp NE ihWindow AND (ETIME < iLoadTime OR bJBoxUserSetting) THEN DO:
  FIND FIRST ttSettingsLoaded
       WHERE ttSettingsLoaded.hWindow = hTemp  
       NO-ERROR.
  IF AVAIL ttSettingsLoaded THEN
    ttSettingsLoaded.hWindow = ihWindow.
END.

FIND FIRST ttSettingsLoaded
     WHERE ttSettingsLoaded.hWindow = ihWindow 
     NO-ERROR.
IF AVAIL ttSettingsLoaded THEN DO:
  IF (ttSettingsLoaded.cLastLoadProc = PROGRAM-NAME(2) AND ETIME < iLoadTime) OR bJBoxUserSetting THEN DO:

    IF ttSettingsLoaded.iCurrX NE 0 THEN
      hWindow:X = ttSettingsLoaded.iCurrX.
    IF ttSettingsLoaded.iCurrY NE 0 THEN
      hWindow:Y = ttSettingsLoaded.iCurrY.

    IF ttSettingsLoaded.iCurrDeltaX NE 0 OR ttSettingsLoaded.iCurrDeltaY NE 0 THEN DO:
      ASSIGN hWindow:WIDTH-PIXELS  = hWindow:WIDTH-PIXELS + ttSettingsLoaded.iCurrDeltaX
             hWindow:HEIGHT-PIXELS = hWindow:HEIGHT-PIXELS + ttSettingsLoaded.iCurrDeltaY.
      setWidgetResize(hWindow,hWindow,"resize","").
    END.

    IF ttSettingsLoaded.iCurrSplitBarX NE 0 THEN DO:
      FIND FIRST ttSplitBarX
           WHERE ttSplitBarX.chWindow = STRING(ihWindow) 
           NO-ERROR.
      IF AVAIL ttSplitBarX THEN DO:
        ttSplitBarX.hSplitBar:X = ttSplitBarX.iOrgSplitBarX + ttSettingsLoaded.iCurrSplitBarX NO-ERROR.
        IF ERROR-STATUS:GET-NUMBER(1) = 5905 THEN
          ttSplitBarX.hSplitBar:X = ttSplitBarX.iOrgSplitBarX.
        setSplitBarX(hWindow,ttSplitBarX.hSplitBar,NO).
      END.
      /* The splitbar is not created (yet - possibly in a sub-folder) */ 
      ELSE DO:
        CREATE ttPrevSplitBarXpos.
        ASSIGN ttPrevSplitBarXpos.chWindow  = STRING(ihWindow)
               ttPrevSplitBarXpos.iPrevXpos = ttSettingsLoaded.iCurrSplitBarX.
      END.
    END.
    IF ttSettingsLoaded.iCurrSplitBarY NE 0 THEN DO:
      FIND FIRST ttSplitBarY
           WHERE ttSplitBarY.chWindow = STRING(ihWindow) 
           NO-ERROR.
      IF AVAIL ttSplitBarY THEN DO:
        ttSplitBarY.hSplitBar:Y = ttSplitBarY.iOrgSplitBarY + ttSettingsLoaded.iCurrSplitBarY NO-ERROR.
        IF ERROR-STATUS:GET-NUMBER(1) = 5905 THEN
          ttSplitBarY.hSplitBar:Y = ttSplitBarY.iOrgSplitBarY NO-ERROR.
        setSplitBarY(hWindow,ttSplitBarY.hSplitBar,NO).
      END.
      /* The splitbar is not created (yet - possibly in a sub-folder) */ 
      ELSE DO:
        CREATE ttPrevSplitBarYpos.
        ASSIGN ttPrevSplitBarYpos.chWindow  = STRING(ihWindow)
               ttPrevSplitBarYpos.iPrevYpos = ttSettingsLoaded.iCurrSplitBarY.
      END.
    END.
    
    FIND FIRST ttBGcolour
         WHERE ttBGcolour.hFrame = ihWindow:FIRST-CHILD 
         NO-ERROR.
    IF NOT AVAIL ttBGcolour THEN DO:
      CREATE ttBGcolour.
      ttBGcolour.hFrame     = ihWindow:FIRST-CHILD. 
    END.
    ASSIGN ttBGcolour.cColour    = ttSettingsLoaded.cColour
           ttBGcolour.cColourDir = ttSettingsLoaded.cColourDir
           .

    hBogus = ihWindow:FIRST-CHILD:NEXT-SIBLING NO-ERROR.
    REPEAT WHILE hBogus NE ?:
      IF hBogus:WIDTH-PIXELS * hBogus:HEIGHT-PIXELS > ihWindow:FIRST-CHILD:WIDTH-PIXELS * ihWindow:FIRST-CHILD:HEIGHT-PIXELS THEN 
        ttBGcolour.hFrame = hBogus.
      hBogus = hBogus:NEXT-SIBLING.
    END.

    RUN DoSetWinColour (hWindow:FIRST-CHILD,ttSettingsLoaded.cColour).

    FIND FIRST ttLastInitFunction
         WHERE ttLastInitFunction.hWindow = ihWindow
         NO-ERROR.
    IF NOT AVAIL ttLastInitFunction THEN DO:
      CREATE ttLastInitFunction.
      ttLastInitFunction.hWindow = ihWindow.
    END.
    ttLastInitFunction.cFunction = PROGRAM-NAME(2).

  END.
  ELSE IF (ETIME < iLoadTime OR bJBoxUserSetting) THEN DO:
    FIND FIRST ttLastInitFunction
         WHERE ttLastInitFunction.hWindow = ihWindow
         NO-ERROR.
    IF NOT AVAIL ttLastInitFunction THEN DO:
      CREATE ttLastInitFunction.
      ttLastInitFunction.hWindow = ihWindow.
    END.
    ttLastInitFunction.cFunction = PROGRAM-NAME(2).

    IF NOT CAN-FIND(FIRST ttBGcolour
                    WHERE ttBGcolour.hFrame = ihWindow:FIRST-CHILD) THEN DO:
      CREATE ttBGcolour.
      ASSIGN ttBGcolour.hFrame     = ihWindow:FIRST-CHILD 
             ttBGcolour.cColour    = "normal"
             ttBGcolour.cColourDir = "up"
             .
    END.

  END.
END.

RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoRemoveBGcolour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoRemoveBGcolour Procedure 
FUNCTION DoRemoveBGcolour RETURNS LOGICAL
  ( INPUT hWidget AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hNextSibling AS WIDGET-HANDLE NO-UNDO.

DO WHILE hWidget <> ? :

  IF CAN-QUERY(hWidget, 'FIRST-CHILD') AND hWidget:FIRST-CHILD  <> ? THEN
    bOK = DoRemoveBGcolour (hWidget:FIRST-CHILD).

  hNextSibling = hWidget:NEXT-SIBLING.

  IF hWidget:TYPE = "rectangle" THEN 
    DELETE WIDGET hWidget.
  
  
  hWidget = hNextSibling.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoSetWinMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoSetWinMenu Procedure 
FUNCTION DoSetWinMenu RETURNS LOGICAL
  ( INPUT hWindow        AS HANDLE,
    INPUT bEnableCommers AS LOG,
    INPUT bEnableColour  AS LOG, 
    INPUT bEnableHelp    AS LOG,   
    INPUT bEnableAbout   AS LOG,  
    INPUT bEnableSave    AS LOG,
    INPUT icMenuLabel    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hWindowSettings   AS HANDLE NO-UNDO.
DEF VAR hColour           AS HANDLE NO-UNDO.
DEF VAR hColourItem1      AS HANDLE NO-UNDO.
DEF VAR hColourItem2      AS HANDLE NO-UNDO.
DEF VAR hColourItem3      AS HANDLE NO-UNDO.
DEF VAR hColourItem4      AS HANDLE NO-UNDO.
DEF VAR hColourItem5      AS HANDLE NO-UNDO.
DEF VAR hColourItem6      AS HANDLE NO-UNDO.
DEF VAR hColourItem7      AS HANDLE NO-UNDO.
DEF VAR hColourDirection  AS HANDLE NO-UNDO.
DEF VAR hColourAllFrames  AS HANDLE NO-UNDO.
DEF VAR hSaveSettings     AS HANDLE NO-UNDO.
DEF VAR hDeleteSettings   AS HANDLE NO-UNDO.
DEF VAR hAbout            AS HANDLE NO-UNDO.
DEF VAR hAboutItem1       AS HANDLE NO-UNDO.
DEF VAR hAboutItem2       AS HANDLE NO-UNDO.
DEF VAR hAboutItem3       AS HANDLE NO-UNDO.
DEF VAR hAboutItem4       AS HANDLE NO-UNDO.
DEF VAR hAboutItem5       AS HANDLE NO-UNDO.
DEF VAR hChemistry        AS HANDLE NO-UNDO.
DEF VAR hProgress         AS HANDLE NO-UNDO.
DEF VAR hMenu             AS HANDLE NO-UNDO.
DEF VAR hFrame            AS HANDLE NO-UNDO.
DEF VAR hParentAbout      AS HANDLE NO-UNDO.
DEF VAR hHelpMenu         AS HANDLE NO-UNDO.
DEF VAR hHelp             AS HANDLE NO-UNDO.
DEF VAR cWinMenuLabel     AS CHAR NO-UNDO.
DEF VAR hWinMenuPlaceHold AS HANDLE NO-UNDO.
DEF VAR hTmpHelpMenu      AS HANDLE NO-UNDO.
DEF VAR hSubMenu          AS HANDLE NO-UNDO.
DEF VAR cHelpFile         AS CHAR   NO-UNDO.
DEF VAR cAbout1File       AS CHAR   NO-UNDO.
DEF VAR cAbout2File       AS CHAR   NO-UNDO.
DEF VAR cAbout3File       AS CHAR   NO-UNDO.
DEF VAR cAbout4File       AS CHAR   NO-UNDO.
DEF VAR cAbout5File       AS CHAR   NO-UNDO.

bScandinavian = DYNAMIC-FUNCTION("Scandinavian") NO-ERROR.

IF NOT bEnableCommers
   AND NOT bEnableColour 
   AND NOT bEnableHelp   
   AND NOT bEnableAbout  
   AND NOT bEnableSave 
   THEN
  RETURN TRUE.

hFrame = hWindow:FIRST-CHILD.

IF bEnableSave OR bEnableColour THEN DO:
  IF cGlobMenuType = "bar" AND icMenuLabel = "" THEN DO:
    CREATE BUTTON hMenu
           ASSIGN Y                = 1
                  X                = IF hWindow:FIRST-CHILD:WIDTH-PIXELS LE hWindow:WIDTH-PIXELS THEN
                                       hWindow:FIRST-CHILD:WIDTH-PIXELS - 5
                                     ELSE 
                                       hWindow:WIDTH-PIXELS - 5
                  WIDTH-PIXELS     = 4 
                  HEIGHT-PIXELS    = 50
                  BGCOLOR          = 0
                  PARENT           = hWindow:FIRST-CHILD:FIRST-CHILD
                  VISIBLE          = TRUE
                  SENSITIVE        = TRUE
                  NAME             = "savesettingsbutt"
                  .   
    
    CREATE ttAddMoveX.
    ASSIGN ttAddMoveX.chFrame  = STRING(hWindow:FIRST-CHILD)
           ttAddMoveX.chWindow = STRING(hWindow)
           ttAddMoveX.cWidget  = "savesettingsbutt".
  
    CREATE MENU hWindowSettings
           ASSIGN POPUP-ONLY = TRUE
                  TITLE = "Window settings"
                  .
    
    hMenu:POPUP-MENU = hWindowSettings.
  END.
  ELSE IF SUBSTR(cGlobMenuType,1,6) = "window" OR icMenuLabel NE "" THEN DO:
    IF NOT VALID-HANDLE(hWindow:MENUBAR) THEN DO:
      CREATE MENU hMenu.
      hWindow:MENUBAR = hMenu.
    END.
    ELSE DO:
      IF icMenuLabel = "" THEN
        cWinMenuLabel = DoGetMenuLabel(hWindow,"MenuLabel").
      ELSE cWinMenuLabel = icMenuLabel.

      hMenu = hWindow:MENUBAR.
      hWinMenuPlaceHold = hMenu:FIRST-CHILD.
      REPEAT WHILE VALID-HANDLE(hWinMenuPlaceHold):
        IF hWinMenuPlaceHold:LABEL = cWinMenuLabel THEN
          hWindowSettings = hWinMenuPlaceHold.
        hWinMenuPlaceHold = hWinMenuPlaceHold:NEXT-SIBLING.
      END.
    END.
  
    IF NOT VALID-HANDLE(hWindowSettings) THEN DO:
      CREATE SUB-MENU hWindowSettings
             ASSIGN PARENT = hMenu
                    LABEL = DoGetMenuLabel(hWindow,"MenuLabel")
                    NAME  = DoGetMenuLabel(hWindow,"MenuLabel")
                    .
  
      IF hWindowSettings:LABEL = "Settings" AND bScandinavian THEN hWindowSettings:LABEL = "Innstillinger".
      ELSE IF hWindowSettings:LABEL = "Innstillinger" AND NOT bScandinavian THEN hWindowSettings:LABEL = "Settings".
    END.
  END.
END.
ELSE DO:
  IF NOT VALID-HANDLE(hWindow:MENUBAR) THEN DO:
    CREATE MENU hMenu.
    hWindow:MENUBAR = hMenu.
  END.
  ELSE DO:
    cWinMenuLabel = DoGetMenuLabel(hWindow,"MenuLabel").
    hMenu = hWindow:MENUBAR.
    hWinMenuPlaceHold = hMenu:FIRST-CHILD.
    REPEAT WHILE VALID-HANDLE(hWinMenuPlaceHold):
      IF hWinMenuPlaceHold:LABEL = cWinMenuLabel THEN
        hWindowSettings = hWinMenuPlaceHold.
      hWinMenuPlaceHold = hWinMenuPlaceHold:NEXT-SIBLING.
    END.
  END.
END.

IF bEnableColour THEN DO:
  CREATE SUB-MENU hColour
         ASSIGN PARENT = hWindowSettings
                LABEL = DoGetMenuLabel(hWindow,"ColourMenu")
                NAME  = DoGetMenuLabel(hWindow,"ColourMenu")
                .
  
  IF NOT CAN-DO(cGlobReservedColour,"normal") THEN
    CREATE MENU-ITEM hColourItem1
          ASSIGN PARENT = hColour
                 LABEL = DoGetMenuLabel(hWindow,"Normal")
                 NAME  = DoGetMenuLabel(hWindow,"Normal")
                 TRIGGERS:
                   ON CHOOSE PERSISTENT RUN DoSetWinColour (hWindow:FIRST-CHILD,"normal").
                END TRIGGERS.
    
  IF NOT CAN-DO(cGlobReservedColour,"grey") THEN
    CREATE MENU-ITEM hColourItem2
          ASSIGN PARENT = hColour
                 LABEL = DoGetMenuLabel(hWindow,"Gray")
                 NAME  = DoGetMenuLabel(hWindow,"Gray")
                 TRIGGERS:
                   ON CHOOSE PERSISTENT RUN DoSetWinColour (hWindow:FIRST-CHILD,"gray").
                END TRIGGERS.
    
  IF NOT CAN-DO(cGlobReservedColour,"blue") THEN
    CREATE MENU-ITEM hColourItem3
          ASSIGN PARENT = hColour
                 LABEL = DoGetMenuLabel(hWindow,"Blue")
                 NAME  = DoGetMenuLabel(hWindow,"Blue")
                 TRIGGERS:
                   ON CHOOSE PERSISTENT RUN DoSetWinColour (hWindow:FIRST-CHILD,"blue").
                END TRIGGERS.
    
  IF NOT CAN-DO(cGlobReservedColour,"red") THEN
    CREATE MENU-ITEM hColourItem4
          ASSIGN PARENT = hColour
                 LABEL = DoGetMenuLabel(hWindow,"Red")
                 NAME  = DoGetMenuLabel(hWindow,"Red")
                 TRIGGERS:
                   ON CHOOSE PERSISTENT RUN DoSetWinColour (hWindow:FIRST-CHILD,"red").
                END TRIGGERS.
    
  IF NOT CAN-DO(cGlobReservedColour,"yellow") THEN
    CREATE MENU-ITEM hColourItem5
          ASSIGN PARENT = hColour
                 LABEL = DoGetMenuLabel(hWindow,"Yellow")
                 NAME  = DoGetMenuLabel(hWindow,"Yellow")
                 TRIGGERS:
                   ON CHOOSE PERSISTENT RUN DoSetWinColour (hWindow:FIRST-CHILD,"yellow").
                END TRIGGERS.
    
  IF NOT CAN-DO(cGlobReservedColour,"green") THEN
    CREATE MENU-ITEM hColourItem6
          ASSIGN PARENT = hColour
                 LABEL = DoGetMenuLabel(hWindow,"Green")
                 NAME  = DoGetMenuLabel(hWindow,"Green")
                 TRIGGERS:
                   ON CHOOSE PERSISTENT RUN DoSetWinColour (hWindow:FIRST-CHILD,"green").
                END TRIGGERS.
    
  IF NOT CAN-DO(cGlobReservedColour,"turquoise") THEN
    CREATE MENU-ITEM hColourItem7
          ASSIGN PARENT = hColour
                 LABEL = DoGetMenuLabel(hWindow,"Turquoise")
                 NAME  = DoGetMenuLabel(hWindow,"Turquoise")
                 TRIGGERS:
                   ON CHOOSE PERSISTENT RUN DoSetWinColour (hWindow:FIRST-CHILD,"purple").
                END TRIGGERS.
    
  IF NOT CAN-DO(cGlobReservedColour,"turn") THEN
    CREATE MENU-ITEM hColourDirection
          ASSIGN PARENT = hColour
                 LABEL = DoGetMenuLabel(hWindow,"Turn")
                 NAME  = DoGetMenuLabel(hWindow,"Turn")
                 TRIGGERS:
                   ON CHOOSE PERSISTENT RUN DoSetWinColour (hWindow:FIRST-CHILD,"turn").
                END TRIGGERS.
    
  FIND FIRST ttMenuStruct
       WHERE ttMenuStruct.cName = "AllFrames"
       NO-LOCK NO-ERROR.
  IF AVAIL ttMenuStruct THEN
    CREATE MENU-ITEM hColourAllFrames
          ASSIGN PARENT = hColour
                 LABEL  = ttMenuStruct.cLabel
                 TRIGGERS:
                   ON CHOOSE PERSISTENT RUN DoSetColourAll (hWindow).
                END TRIGGERS.
END.

IF bEnableAbout OR bEnableCommers OR bEnableHelp THEN DO:
  IF bEnableHelp THEN DO:
    IF cGlobMenuType MATCHES "*help*" OR NOT VALID-HANDLE(hWindowSettings) THEN 
      CREATE SUB-MENU hHelpMenu
             ASSIGN PARENT = hMenu
                    LABEL  = DoGetMenuLabel(hWindow,"HelpMenu")
                    NAME   = DoGetMenuLabel(hWindow,"HelpMenu")
                    .
    ELSE IF iGlobNumAboutMenu > 0 OR bEnableCommers THEN 
      CREATE SUB-MENU hHelpMenu
             ASSIGN PARENT = hWindowSettings
                    LABEL  = DoGetMenuLabel(hWindow,"HelpMenu")
                    NAME   = DoGetMenuLabel(hWindow,"HelpMenu")
                    .
    ELSE hHelpMenu = hWindowSettings.
         
    IF hHelpMenu:LABEL = "Help" AND bScandinavian THEN hHelpMenu:LABEL = "Hjelp".
    ELSE IF hHelpMenu:LABEL = "Hjelp" AND NOT bScandinavian THEN hHelpMenu:LABEL = "Help".
    
    FIND FIRST ttMenuStruct          
         WHERE ttMenuStruct.hWindow = hWindow
           AND ttMenuStruct.cName   = "defaultHelp"
         NO-LOCK NO-ERROR.
    IF AVAIL ttMenuStruct THEN
      cHelpFile = ENTRY(3,ttMenuStruct.cLabel,";").
    ELSE DO:
      FIND FIRST ttMenuStruct          
           WHERE ttMenuStruct.cName   = "defaultHelp"
           NO-LOCK NO-ERROR.
      cHelpFile = DYNAMIC-FUNCTION("getAttribute",SESSION,"defaulthelpfile") NO-ERROR.
      IF (cHelpFile = "" OR cHelpFile = ?) AND AVAIL ttMenuStruct THEN
        cHelpFile = ENTRY(3,ttMenuStruct.cLabel,";").
    END.
    IF AVAIL ttMenuStruct THEN DO:
      CREATE MENU-ITEM hHelp
            ASSIGN PARENT = hHelpMenu
                   LABEL = ENTRY(1,ttMenuStruct.cLabel,";")
                   NAME  = ENTRY(1,ttMenuStruct.cLabel,";")
                   TRIGGERS:
                     ON CHOOSE PERSISTENT RUN DoOpenDoc (ENTRY(2,ttMenuStruct.cLabel,";"),AddLanguageSubCat(cHelpFile)).
                  END TRIGGERS.
      IF hHelp:LABEL = "Help" AND bScandinavian THEN hHelp:LABEL = "Hjelp".
      ELSE IF hHelp:LABEL = "Hjelp" AND NOT bScandinavian THEN hHelp:LABEL = "Help".
    END.

    hParentAbout = hHelpMenu.
  END.
  ELSE hParentAbout = hWindowSettings.

  IF iGlobNumAboutMenu > 1 OR bEnableCommers THEN DO:
    CREATE SUB-MENU hAbout
           ASSIGN PARENT = hParentAbout
                  LABEL = DoGetMenuLabel(hWindow,"AboutMenu")
                  NAME  = DoGetMenuLabel(hWindow,"AboutMenu")
                  .
    IF hAbout:LABEL = "About" AND bScandinavian THEN hAbout:LABEL = "Om".
    ELSE IF hAbout:LABEL = "Om" AND NOT bScandinavian THEN hAbout:LABEL = "About".

    FIND FIRST ttMenuStruct
         WHERE ttMenuStruct.cName = "about1"
         NO-LOCK NO-ERROR.
    cAbout1File = DYNAMIC-FUNCTION("getAttribute",SESSION,"about1file") NO-ERROR.
    IF (cAbout1File = "" OR cAbout1File = ?) AND AVAIL ttMenuStruct THEN
      cAbout1File = ENTRY(3,ttMenuStruct.cLabel,";").
    IF AVAIL ttMenuStruct THEN
      CREATE MENU-ITEM hAboutItem1
            ASSIGN PARENT = hAbout
                   LABEL = ENTRY(1,ttMenuStruct.cLabel,";")
                   TRIGGERS:
                     ON CHOOSE PERSISTENT RUN DoOpenDoc (ENTRY(2,ttMenuStruct.cLabel,";"),AddLanguageSubCat(cAbout1File)).
                  END TRIGGERS.

    FIND FIRST ttMenuStruct
         WHERE ttMenuStruct.cName = "about2"
         NO-LOCK NO-ERROR.
    cAbout2File = DYNAMIC-FUNCTION("getAttribute",SESSION,"about2file") NO-ERROR.
    IF (cAbout2File = "" OR cAbout2File = ?) AND AVAIL ttMenuStruct THEN
      cAbout2File = ENTRY(3,ttMenuStruct.cLabel,";").
    IF AVAIL ttMenuStruct THEN
      CREATE MENU-ITEM hAboutItem2
            ASSIGN PARENT = hAbout
                   LABEL = ENTRY(1,ttMenuStruct.cLabel,";")
                   TRIGGERS:
                     ON CHOOSE PERSISTENT RUN DoOpenDoc (ENTRY(2,ttMenuStruct.cLabel,";"),AddLanguageSubCat(cAbout2File)).
                  END TRIGGERS.

    FIND FIRST ttMenuStruct
         WHERE ttMenuStruct.cName = "about3"
         NO-LOCK NO-ERROR.
    cAbout3File = DYNAMIC-FUNCTION("getAttribute",SESSION,"about3file") NO-ERROR.
    IF (cAbout3File = "" OR cAbout3File = ?) AND AVAIL ttMenuStruct THEN
      cAbout3File = ENTRY(3,ttMenuStruct.cLabel,";").
    IF AVAIL ttMenuStruct THEN
      CREATE MENU-ITEM hAboutItem3
            ASSIGN PARENT = hAbout
                   LABEL = ENTRY(1,ttMenuStruct.cLabel,";")
                   TRIGGERS:
                     ON CHOOSE PERSISTENT RUN DoOpenDoc (ENTRY(2,ttMenuStruct.cLabel,";"),AddLanguageSubCat(cAbout3File)).
                  END TRIGGERS.

    FIND FIRST ttMenuStruct
         WHERE ttMenuStruct.cName = "about4"
         NO-LOCK NO-ERROR.
    cAbout4File = DYNAMIC-FUNCTION("getAttribute",SESSION,"about4file") NO-ERROR.
    IF (cAbout4File = "" OR cAbout4File = ?) AND AVAIL ttMenuStruct THEN
      cAbout4File = ENTRY(3,ttMenuStruct.cLabel,";").
    IF AVAIL ttMenuStruct THEN
      CREATE MENU-ITEM hAboutItem4
            ASSIGN PARENT = hAbout
                   LABEL = ENTRY(1,ttMenuStruct.cLabel,";")
                   TRIGGERS:
                     ON CHOOSE PERSISTENT RUN DoOpenDoc (ENTRY(2,ttMenuStruct.cLabel,";"),AddLanguageSubCat(cAbout4File)).
                  END TRIGGERS.

    FIND FIRST ttMenuStruct
         WHERE ttMenuStruct.cName = "about5"
         NO-LOCK NO-ERROR.
    cAbout5File = DYNAMIC-FUNCTION("getAttribute",SESSION,"about5file") NO-ERROR.
    IF (cAbout5File = "" OR cAbout5File = ?) AND AVAIL ttMenuStruct THEN
      cAbout5File = ENTRY(3,ttMenuStruct.cLabel,";").
    IF AVAIL ttMenuStruct THEN
      CREATE MENU-ITEM hAboutItem5
            ASSIGN PARENT = hAbout
                   LABEL = ENTRY(1,ttMenuStruct.cLabel,";")
                   TRIGGERS:
                     ON CHOOSE PERSISTENT RUN DoOpenDoc (ENTRY(2,ttMenuStruct.cLabel,";"),AddLanguageSubCat(cAbout5File)).
                  END TRIGGERS.

  END.
  ELSE IF iGlobNumAboutMenu = 1 THEN DO:
    FIND FIRST ttMenuStruct
         WHERE ttMenuStruct.cName = "about1"
         NO-LOCK NO-ERROR.
    cAbout1File = DYNAMIC-FUNCTION("getAttribute",SESSION,"about1file") NO-ERROR.
    IF (cAbout1File = "" OR cAbout1File = ?) AND AVAIL ttMenuStruct THEN
      cAbout1File = ENTRY(3,ttMenuStruct.cLabel,";").
    IF AVAIL ttMenuStruct THEN DO:
      CREATE MENU-ITEM hAboutItem1
            ASSIGN PARENT = hParentAbout
                   LABEL = ENTRY(1,ttMenuStruct.cLabel,";")
                   TRIGGERS:
                     ON CHOOSE PERSISTENT RUN DoOpenDoc (ENTRY(2,ttMenuStruct.cLabel,";"),AddLanguageSubCat(cAbout1File)).
                  END TRIGGERS.
      IF hAboutItem1:LABEL = "About" AND bScandinavian THEN hAboutItem1:LABEL = "Om".
      ELSE IF hAboutItem1:LABEL = "Om" AND NOT bScandinavian THEN hAboutItem1:LABEL = "About".
    END.
  END.
  
  IF bEnableCommers THEN DO:
    CREATE MENU-ITEM hChemistry
          ASSIGN PARENT = hAbout
                 LABEL = "Chemistry"
                 TRIGGERS:
                   ON CHOOSE PERSISTENT RUN DoSetWebDoc ("open","www.chemistry.no").
                END TRIGGERS.
    
    CREATE MENU-ITEM hChemistry
          ASSIGN PARENT = hAbout
                 LABEL = "Progress"
                 TRIGGERS:
                   ON CHOOSE PERSISTENT RUN DoSetWebDoc ("open","www.progress.com").
                END TRIGGERS.
  END.
END.

IF bEnableSave THEN DO:
  CREATE MENU-ITEM hSaveSettings
        ASSIGN PARENT = hWindowSettings
               LABEL = DoGetMenuLabel(hWindow,"SaveSettingsMenu")
               NAME  = DoGetMenuLabel(hWindow,"SaveSettingsMenu")
               TRIGGERS:
                 ON CHOOSE PERSISTENT RUN DoSaveWinSettings (hWindow).
              END TRIGGERS.
  CREATE MENU-ITEM hDeleteSettings
        ASSIGN PARENT = hWindowSettings
               LABEL = DoGetMenuLabel(hWindow,"DeleteSettingsMenu")
               NAME  = DoGetMenuLabel(hWindow,"DeleteSettingsMenu")
               TRIGGERS:
                 ON CHOOSE PERSISTENT RUN DoDeleteWinSettings (hWindow).
              END TRIGGERS.
END.


RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoWidgetResize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoWidgetResize Procedure 
FUNCTION DoWidgetResize RETURNS LOGICAL
  ( INPUT hWidget AS WIDGET-HANDLE) :

DEF VAR hWidget2     AS WIDGET-HANDLE NO-UNDO.
DEF VAR chFrame      AS CHAR NO-UNDO.
DEF VAR iAbsoluteX   AS INT NO-UNDO.
DEF VAR iAbsoluteY   AS INT NO-UNDO.
DEF VAR bResizeXdone AS LOG NO-UNDO.

IF CAN-DO("window,field-group",hWidget:TYPE) AND AVAIL ttResizeTypes AND ttResizeTypes.chWindow NE STRING(hWidget:WINDOW)  THEN
  FIND FIRST ttResizeTypes WHERE ttResizeTypes.chWindow = STRING(hWidget:WINDOW) NO-LOCK NO-ERROR.

IF NOT AVAIL ttResizeTypes THEN
  FIND FIRST ttResizeTypes WHERE ttResizeTypes.chWindow = "" NO-LOCK.

DO WHILE hWidget <> ? :

  WidgetWatch(hWidget,"Start process. CAN-DO(ttResizeTypes.cTypes,hWidget:TYPE): " + STRING(CAN-DO(ttResizeTypes.cTypes,hWidget:TYPE))).

  IF hWidget:NAME = "folder-frm" THEN 
    setADMFolderRect(hWidget,"minimize").

  IF NOT VALID-HANDLE(hWidget) OR CAN-FIND(FIRST ttADMFolderRect WHERE ttADMFolderRect.hRect = hWidget) THEN DO:
    hWidget = hWidget:NEXT-SIBLING.
    NEXT.
  END.

  IF hWidget:TYPE = "WINDOW" AND NOT hWidget:VISIBLE THEN DO:
    hWidget = hWidget:NEXT-SIBLING.
    NEXT.
  END.

  bResizeXdone = NO.

  /* SmartPak,f.ex */
  IF SpecialResize(hWidget) THEN DO:
    hWidget = hWidget:NEXT-SIBLING.       
    NEXT.
  END.

  /* Avoid tab-images to be resized when using ADM1: */
  IF hWidget:TYPE = "IMAGE" AND hWidget:FRAME-NAME = "folder-frm" THEN DO:
    hWidget = hWidget:NEXT-SIBLING.
    NEXT.
  END.
  /* Avoid smart-panel rectangles to be resized when using ADM - unless custom panel: */
  IF NOT bResizeADM2panel AND (hWidget:TYPE = "RECTANGLE" OR hWidget:TYPE = "BUTTON") AND (hWidget:FRAME-NAME = "panel-frame" OR hWidget:NAME = ?) THEN DO:
    hWidget = hWidget:NEXT-SIBLING.
    NEXT.
  END.  
  ELSE IF bResizeADM2panel AND (hWidget:TYPE = "RECTANGLE" AND hWidget:WIDTH-PIXELS < 10 AND hWidget:FRAME-NAME = "panel-frame") THEN DO:
    hWidget = hWidget:NEXT-SIBLING.
    NEXT.
  END.  

  chFrame = ?.
  IF hWidget:TYPE = "FRAME" THEN chFrame = STRING(hWidget).
  /* Retrieve widget absolute position in window (and set parent frame - if not the widget is a frame). */
  ASSIGN hWidget2   = hWidget:PARENT
         iAbsoluteX = hWidget:X
         iAbsoluteY = hWidget:Y.
  REPEAT WHILE hWidget2:TYPE NE "WINDOW":
    IF hWidget:TYPE NE "FRAME" AND hWidget2:TYPE = "FRAME" AND chFrame = ? THEN
      chFrame = STRING(hWidget2).
    IF hWidget2:TYPE = "FRAME" THEN
      ASSIGN iAbsoluteX = iAbsoluteX + hWidget2:X
             iAbsoluteY = iAbsoluteY + hWidget2:Y.
    hWidget2 = hWidget2:PARENT.
  END.

  WidgetWatch(hWidget,"Absolute X/Y: " + STRING(iAbsoluteX) + "/" + STRING(iAbsoluteY) + "  DeltaX/Y: " + STRING(iDeltaX) + "/" + STRING(iDeltaY)).

  /* If DeltaY < 0 move before resize: */
  IF iDeltaY < 0 THEN DO:
    IF ((iAbsoluteY > iMinYmove
        AND NOT CAN-FIND(FIRST ttNoMoveY WHERE ttNoMoveY.chFrame  = chFrame
                                           AND ttNoMoveY.cWidget = hWidget:NAME))
        OR CAN-FIND(FIRST ttAddMoveY WHERE ttAddMoveY.chFrame  = chFrame
                                       AND ttAddMoveY.cWidget = hWidget:NAME)
        OR CAN-FIND(FIRST ttMoveYgroup WHERE ttMoveYgroup.hWindow = hCurrWindow
                                         AND ttMoveYgroup.chFrame = chFrame
                                         AND ttMoveYgroup.cWidget = hWidget:NAME)
        )
       AND hWidget:Y NE hWidget:Y + iDeltaY
       AND NOT CAN-DO("Rect-Left,Rect-Main,Rect-Right,Rect-Top",hWidget:NAME) /* ADM folder */
       AND NOT CAN-FIND(FIRST ttSplitBarX WHERE ttSplitBarX.hSplitFrame = hWidget)
       AND NOT CAN-FIND(FIRST ttSplitBarX WHERE ttSplitBarX.hSplitBar = hWidget)
       AND NOT CAN-FIND(FIRST ttSplitBarY WHERE ttSplitBarY.hSplitFrame = hWidget)
       AND NOT CAN-FIND(FIRST ttSplitBarY WHERE ttSplitBarY.hSplitBar = hWidget)
       AND NOT CAN-FIND(FIRST ttFollowSplitBarY
                        WHERE ttFollowSplitBarY.chWindow = STRING(hCurrWindow)
                          AND ttFollowSplitBarY.hWidget  = hWidget)
       AND NOT CAN-FIND(FIRST ttNeverMoveY WHERE ttNeverMoveY.hWidget = hWidget)
       THEN DO:
  
      WidgetWatch(hWidget,"Move Y before resize").

      FIND FIRST ttMoveYgroup WHERE ttMoveYgroup.hWindow = hCurrWindow
                                AND ttMoveYgroup.chFrame = chFrame
                                AND ttMoveYgroup.cWidget = hWidget:NAME NO-ERROR.
      IF AVAIL ttMoveYgroup THEN
        hWidget:Y = hWidget:Y + TRUNC((iDeltaY * ttMoveYgroup.fPercent) / 100,0) NO-ERROR.
      ELSE DO:
        hWidget:Y = hWidget:Y + iDeltaY NO-ERROR.
        IF ERROR-STATUS:GET-MESSAGE(1) NE "" AND hWidget:NAME = "folder-frm" THEN DO:
          hWidget:WIDTH-PIXELS = hWidget:WIDTH-PIXELS + iDeltaX NO-ERROR.
          hWidget:Y = hWidget:Y + iDeltaY NO-ERROR.
          IF ERROR-STATUS:GET-MESSAGE(1) = "" THEN bResizeXdone = YES.
        END.
      END.
  
      hWidget2 = hWidget:SIDE-LABEL-HANDLE NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
        hWidget2:Y = hWidget:Y NO-ERROR.
  
      IF hWidget:TYPE = "FRAME" AND NOT CAN-DO("folder-frm,ProgressBarFrame",hWidget:NAME) AND NOT overridesWithinFrame(chFrame,hWidget:NAME) THEN DO:
        hWidget = hWidget:NEXT-SIBLING.
        NEXT.
      END.
    END.
    ELSE WidgetWatch(hWidget,"No Move Y done before resize. Min move Y: " + STRING(iMinYmove)).
  END.

  IF (CAN-DO(ttResizeTypes.cTypes,hWidget:TYPE) OR 
      CAN-FIND(FIRST ttAddResizeX
               WHERE ttAddResizeX.chWindow = STRING(hCurrWindow)
                 AND ttAddResizeX.chFrame  = chFrame
                 AND ttAddResizeX.cWidget  = hWidget:NAME) OR
      CAN-FIND(FIRST ttAddResizeY
               WHERE ttAddResizeY.chWindow = STRING(hCurrWindow)
                 AND ttAddResizeY.chFrame  = chFrame
                 AND ttAddResizeY.cWidget  = hWidget:NAME) OR
      CAN-FIND(FIRST ttResizeXgroup
               WHERE ttResizeXgroup.hWindow = hCurrWindow
                 AND ttResizeXgroup.chFrame = chFrame
                 AND ttResizeXgroup.cWidget = hWidget:NAME) OR
      CAN-FIND(FIRST ttResizeYgroup
               WHERE ttResizeYgroup.hWindow = hCurrWindow
                 AND ttResizeYgroup.chFrame = chFrame
                 AND ttResizeYgroup.cWidget = hWidget:NAME) OR
      CAN-DO("rect-top,rect-bottom,rect-right,rect-left,rect-main",hWidget:NAME)) 
     AND (IF NOT bResizeADM2panel THEN hWidget:NAME NE "panel-frame" ELSE TRUE)
     AND (IF hWidget:TYPE = "RECTANGLE" AND hWidget:WIDTH-PIXELS = 2 AND hWidget:HEIGHT-PIXELS = 25 THEN FALSE ELSE TRUE) /* Toolbar Rule */
     AND NOT CAN-FIND(FIRST ttNeverResizeX WHERE ttNeverResizeX.hWidget = hWidget)
     THEN DO:                  

    IF hWidget:TYPE = "browse" AND hWidget:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
      rBrowseBuffer = hWidget:QUERY:GET-BUFFER-HANDLE(1):ROWID.
    ELSE rBrowseBuffer = ?.

    IF NOT CAN-FIND(FIRST ttNoResizeX WHERE ttNoResizeX.chFrame  = chFrame
                                        AND ttNoResizeX.cWidget = hWidget:NAME)
       AND hWidget:WIDTH-PIXELS NE hWidget:WIDTH-PIXELS + iDeltaX
       AND hWidget:NAME NE "rect-left" /* ADM folder rect */         
       AND hWidget:NAME NE "rect-right" /* ADM folder rect */
       /* If there is a vertical splitbar don't bother resize objects to the left of it: */
       AND (IF AVAIL ttSplitBarX AND NOT CAN-FIND(FIRST ttResizeXgroup WHERE ttResizeXgroup.hWindow = hCurrWindow) THEN
              iAbsoluteX + hWidget:WIDTH-PIXELS > ttSplitBarX.iPos
              OR iAbsoluteY < iAbsXsplitY
              OR iAbsoluteY > iAbsXsplitY + iAbsXscopeY
            ELSE TRUE)
       AND (IF hWidget:TYPE = "RECTANGLE" AND hWidget:FRAME-NAME = "folder-frm" THEN
             hWidget:NAME NE ?
            ELSE TRUE)
       THEN DO:
              
      WidgetWatch(hWidget,"Resize X").
              
      IF AVAIL ttSplitBarX 
         AND ttSplitBarX.hSplitFrame = hWidget 
         AND ttSplitBarX.bRestrictResize 
         AND (IF bUseCalcValues THEN hCurrWindow:WIDTH-PIXELS GE iCalcWidth ELSE TRUE)
         THEN DO:
        IF hWidget:WIDTH-PIXELS + iDeltaX > (IF bUseCalcValues THEN iCalcWidth ELSE hCurrWindow:WIDTH-PIXELS) / 2 - ttSplitBarX.hSplitFrame:X
           OR hWidget:WIDTH-PIXELS + iDeltaX < ttSplitBarX.hSplitBar:X THEN        
          hWidget:WIDTH-PIXELS = (IF bUseCalcValues THEN iCalcWidth ELSE hCurrWindow:WIDTH-PIXELS) / 2 - ttSplitBarX.hSplitFrame:X. 
        ELSE
          hWidget:WIDTH-PIXELS = hWidget:WIDTH-PIXELS + iDeltaX NO-ERROR.
      END.

      FIND FIRST ttResizeXgroup
           WHERE ttResizeXgroup.hWindow = hCurrWindow
             AND ttResizeXgroup.chFrame = chFrame
             AND ttResizeXgroup.cWidget = hWidget:NAME 
           NO-ERROR.
      IF AVAIL ttResizeXgroup THEN
        hWidget:WIDTH-PIXELS = hWidget:WIDTH-PIXELS + TRUNC((iDeltaX * ttResizeXgroup.fPercent) / 100,0) NO-ERROR.
      ELSE IF NOT bResizeXdone THEN 
        hWidget:WIDTH-PIXELS = hWidget:WIDTH-PIXELS + iDeltaX NO-ERROR.
        
      IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN  
        WidgetWatch(hWidget,"Error resize Y: " + ERROR-STATUS:GET-MESSAGE(1)).

      IF hWidget:TYPE = "FRAME" THEN DO:
        hWidget:VIRTUAL-WIDTH-PIXELS  = hWidget:WIDTH-PIXELS NO-ERROR.
        hWidget:WIDTH-PIXELS = hWidget:WIDTH-PIXELS NO-ERROR.
        WidgetWatch(hWidget,"After adjust virtual frame size, Resize X").
      END.

      hWidget2 = hWidget:SIDE-LABEL-HANDLE NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
        hWidget2:X = hWidget:X - hWidget2:WIDTH-PIXELS NO-ERROR.

    END.    

    IF NOT CAN-FIND(FIRST ttNoResizeY WHERE ttNoResizeY.chFrame  = chFrame
                                        AND ttNoResizeY.cWidget = hWidget:NAME)
        AND NOT CAN-DO("FILL-IN,COMBO-BOX",hWidget:TYPE)
        AND hWidget:HEIGHT-PIXELS NE hWidget:HEIGHT-PIXELS + iDeltaY
        AND hWidget:NAME NE "rect-top" /* ADM rect */
        AND hWidget:NAME NE "rect-bottom" /* ADM rect */
        AND hWidget:NAME NE "panel-frame"
        AND NOT CAN-FIND(FIRST ttNeverResizeY WHERE ttNeverResizeY.hWidget = hWidget)
       THEN DO:

      WidgetWatch(hWidget,"Resize Y").

      bOK = TRUE.
      IF hWidget:TYPE NE "FRAME" AND hWidget:FRAME-NAME = "panel-frame" THEN bOK = FALSE.
      IF hWidget:TYPE = "RECTANGLE" AND hWidget:FRAME-NAME = "folder-frm"
         AND hWidget:NAME = ? THEN 
        bOK = FALSE.
      IF bOK THEN DO:
        FIND FIRST ttResizeYgroup
             WHERE ttResizeYgroup.hWindow = hCurrWindow
               AND ttResizeYgroup.chFrame = chFrame
               AND ttResizeYgroup.cWidget = hWidget:NAME 
             NO-ERROR.
        IF AVAIL ttResizeYgroup THEN 
          hWidget:HEIGHT-PIXELS = hWidget:HEIGHT-PIXELS + TRUNC((iDeltaY * ttResizeYgroup.fPercent) / 100,0) NO-ERROR.
        ELSE
          hWidget:HEIGHT-PIXELS = hWidget:HEIGHT-PIXELS + iDeltaY NO-ERROR.

        IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN  
          WidgetWatch(hWidget,"Error resize Y: " + ERROR-STATUS:GET-MESSAGE(1)).
  
        IF hWidget:TYPE = "FRAME" THEN DO:
          hWidget:VIRTUAL-HEIGHT-PIXELS = hWidget:HEIGHT-PIXELS NO-ERROR.
          hWidget:HEIGHT-PIXELS = hWidget:HEIGHT-PIXELS NO-ERROR.
          WidgetWatch(hWidget,"After adjust virtual frame size, Resize Y").
        END.
      END.

      IF hWidget:TYPE = "browse" AND rBrowseBuffer NE ? THEN
        hWidget:QUERY:GET-BUFFER-HANDLE(1):FIND-BY-ROWID(rBrowseBuffer) NO-ERROR.
  
    END.
  END.

  /* Resize splitbar(s) in X/Y-direction (if existing) and that's it - it's repositioned later: */
  IF AVAIL ttSplitBarX
     AND ttSplitBarX.hSplitBar = hWidget
     AND NOT CAN-FIND(FIRST ttNoResizeY WHERE ttNoResizeY.chFrame = STRING(ttSplitBarX.hSplitFrame)
                                          AND ttNoResizeY.cWidget = ttSplitBarX.hSplitFrame:NAME)
     THEN DO:
    hWidget:HEIGHT-PIXELS = hWidget:HEIGHT-PIXELS + iDeltaY NO-ERROR.
    hWidget = hWidget:NEXT-SIBLING.
    NEXT.
  END.  
  IF AVAIL ttSplitBarY
     AND ttSplitBarY.hSplitBar = hWidget
     AND NOT CAN-FIND(FIRST ttNoResizeX WHERE ttNoResizeX.chFrame = STRING(ttSplitBarY.hSplitFrame)
                                          AND ttNoResizeX.cWidget = ttSplitBarY.hSplitFrame:NAME)
     AND (ttSplitBarY.hSplitFrame:WIDTH-PIXELS > hWidget:WIDTH-PIXELS + 10
          OR ttSplitBarY.hSplitFrame:WIDTH-PIXELS < hWidget:WIDTH-PIXELS - 10)
     THEN DO:
    hWidget:WIDTH-PIXELS = hWidget:WIDTH-PIXELS + iDeltaX NO-ERROR.
    hWidget = hWidget:NEXT-SIBLING.
    NEXT.
  END.

  /* Move in X/Y dir: */
  IF ((iAbsoluteX > iMinXmove
       AND NOT CAN-FIND(FIRST ttNoMoveX WHERE ttNoMoveX.chFrame  = chFrame
                                          AND ttNoMoveX.cWidget = hWidget:NAME))
       OR CAN-FIND(FIRST ttAddMoveX WHERE ttAddMoveX.chFrame  = chFrame
                                      AND ttAddMoveX.cWidget = hWidget:NAME)
       OR CAN-FIND(FIRST ttMoveXgroup WHERE ttMoveXgroup.hWindow = hCurrWindow
                                        AND ttMoveXgroup.chFrame = chFrame
                                        AND ttMoveXgroup.cWidget = hWidget:NAME)
       )
     AND hWidget:X NE hWidget:X + iDeltaX
     AND NOT CAN-FIND(FIRST ttSplitBarX WHERE ttSplitBarX.hSplitFrame = hWidget)
     AND NOT CAN-FIND(FIRST ttFollowSplitBarX
                      WHERE ttFollowSplitBarX.chWindow = STRING(hCurrWindow)
                        AND ttFollowSplitBarX.hWidget  = hWidget)
     AND NOT CAN-FIND(FIRST ttNeverMoveX WHERE ttNeverMoveX.hWidget = hWidget)
      OR hWidget:NAME = "rect-right"
     THEN DO:

    WidgetWatch(hWidget,"Move X after resize").
     
    FIND FIRST ttMoveXgroup WHERE ttMoveXgroup.hWindow = hCurrWindow
                              AND ttMoveXgroup.chFrame = chFrame
                              AND ttMoveXgroup.cWidget = hWidget:NAME NO-ERROR.
    IF AVAIL ttMoveXgroup THEN
      hWidget:X = hWidget:X + TRUNC((iDeltaX * ttMoveXgroup.fPercent) / 100,0) NO-ERROR.
    ELSE
      hWidget:X = hWidget:X + iDeltaX NO-ERROR.

    IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN  
      WidgetWatch(hWidget,"Error Move X after resize: " + ERROR-STATUS:GET-MESSAGE(1)).

    hWidget2 = hWidget:SIDE-LABEL-HANDLE NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      hWidget2:X = hWidget:X - hWidget2:WIDTH-PIXELS NO-ERROR.

    IF hWidget:TYPE = "FRAME" AND NOT CAN-DO("folder-frm,ProgressBarFrame",hWidget:NAME) AND NOT overridesWithinFrame(chFrame,hWidget:NAME) THEN DO:
      hWidget = hWidget:NEXT-SIBLING.
      NEXT.
    END.
  END.
  ELSE WidgetWatch(hWidget,"No Move X done. Min. move X: " + STRING(iMinXmove)).

  /* if deltaY > 0 move after resize: */
  IF iDeltaY > 0 THEN DO:
    IF ((iAbsoluteY > iMinYmove
        AND NOT CAN-FIND(FIRST ttNoMoveY WHERE ttNoMoveY.chFrame  = chFrame
                                           AND ttNoMoveY.cWidget = hWidget:NAME))
        OR CAN-FIND(FIRST ttAddMoveY WHERE ttAddMoveY.chFrame  = chFrame
                                       AND ttAddMoveY.cWidget = hWidget:NAME)
        OR CAN-FIND(FIRST ttMoveYgroup WHERE ttMoveYgroup.hWindow = hCurrWindow
                                         AND ttMoveYgroup.chFrame = chFrame
                                         AND ttMoveYgroup.cWidget = hWidget:NAME)
        )
       AND hWidget:Y NE hWidget:Y + iDeltaY
       AND NOT CAN-DO("Rect-Left,Rect-Main,Rect-Right,Rect-Top",hWidget:NAME) /* ADM folder */
       AND NOT CAN-FIND(FIRST ttSplitBarX WHERE ttSplitBarX.hSplitFrame = hWidget)
       AND NOT CAN-FIND(FIRST ttSplitBarX WHERE ttSplitBarX.hSplitBar = hWidget)
       AND (NOT CAN-FIND(FIRST ttSplitBarY WHERE ttSplitBarY.hSplitFrame = hWidget) 
            OR CAN-FIND(FIRST ttAddMoveY WHERE ttAddMoveY.chFrame  = chFrame
                                       AND ttAddMoveY.cWidget = hWidget:NAME))
       AND NOT CAN-FIND(FIRST ttSplitBarY WHERE ttSplitBarY.hSplitBar = hWidget)
       AND NOT CAN-FIND(FIRST ttFollowSplitBarY
                        WHERE ttFollowSplitBarY.chWindow = STRING(hCurrWindow)
                          AND ttFollowSplitBarY.hWidget  = hWidget)
       AND NOT CAN-FIND(FIRST ttNeverMoveY WHERE ttNeverMoveY.hWidget = hWidget)
       THEN DO:
  
      WidgetWatch(hWidget,"Move Y after resize").

      FIND FIRST ttMoveYgroup WHERE ttMoveYgroup.hWindow = hCurrWindow
                                AND ttMoveYgroup.chFrame = chFrame
                                AND ttMoveYgroup.cWidget = hWidget:NAME NO-ERROR.
      IF AVAIL ttMoveYgroup THEN
        hWidget:Y = hWidget:Y + TRUNC((iDeltaY * ttMoveYgroup.fPercent) / 100,0) NO-ERROR.
      ELSE
        hWidget:Y = hWidget:Y + iDeltaY NO-ERROR.

      IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN  
        WidgetWatch(hWidget,"Error Move Y after resize: " + ERROR-STATUS:GET-MESSAGE(1)).

      hWidget2 = hWidget:SIDE-LABEL-HANDLE NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
        hWidget2:Y = hWidget:Y NO-ERROR.
  
      IF hWidget:TYPE = "FRAME" AND hWidget:NAME NE "folder-frm" AND NOT overridesWithinFrame(chFrame,hWidget:NAME) THEN DO:
        WidgetWatch(hWidget,"Next - no overrides in frame").
        hWidget = hWidget:NEXT-SIBLING.
        NEXT.
      END.
    END.
    ELSE WidgetWatch(hWidget,"No Move Y done after resize. Min move Y: " + STRING(iMinYmove)).
  END.

  IF CAN-FIND(FIRST ttNoMoveX WHERE ttNoMoveX.chFrame  = chFrame
                                AND ttNoMoveX.cWidget = hWidget:NAME) THEN DO:
    hWidget2 = hWidget:SIDE-LABEL-HANDLE NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      hWidget2:X = hWidget:X - hWidget2:WIDTH-PIXELS NO-ERROR.
  END.
  IF CAN-FIND(FIRST ttNoMoveY WHERE ttNoMoveY.chFrame  = chFrame
                                AND ttNoMoveY.cWidget = hWidget:NAME) THEN DO:
    hWidget2 = hWidget:SIDE-LABEL-HANDLE NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      hWidget2:Y = hWidget:Y NO-ERROR.
  END.

  IF CAN-FIND(FIRST ttAddResizeY WHERE ttAddResizeY.chFrame = chFrame
                                   AND ttAddResizeY.cWidget = hWidget:NAME)
     AND hWidget:HEIGHT-PIXELS NE hWidget:HEIGHT-PIXELS + iDeltaY
     THEN DO:

    WidgetWatch(hWidget,"Add resize Y").
            
    hWidget:HEIGHT-PIXELS = hWidget:HEIGHT-PIXELS + iDeltaY NO-ERROR.
    IF hWidget:TYPE = "FRAME" THEN
      hWidget:VIRTUAL-HEIGHT-PIXELS = hWidget:HEIGHT-PIXELS NO-ERROR.
  END.
  
  WidgetWatch(hWidget,"Finished").

  IF CAN-QUERY(hWidget, 'FIRST-CHILD') AND hWidget:FIRST-CHILD  <> ? THEN
    bOK = DoWidgetResize (hWidget:FIRST-CHILD).
  
  IF hWidget:NAME = "folder-frm" THEN 
    setADMFolderRect(hWidget,"restore").

  IF NOT (bKeepWithinFrame AND CAN-DO("frame,field-group",hWidget:TYPE)) THEN
    hWidget = hWidget:NEXT-SIBLING.

END.
    
RETURN TRUE.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FindVisible) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FindVisible Procedure 
FUNCTION FindVisible RETURNS LOGICAL
  ( INPUT ihWidget AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
REPEAT WHILE VALID-HANDLE(ihWidget):      
  IF CAN-QUERY(ihWidget,"VISIBLE") AND ihWidget:TYPE NE "field-group"
     AND ihWidget:VISIBLE THEN
    cVisibleList = cVisibleList + STRING(ihWidget) + ",".
  IF CAN-QUERY(ihWidget,"first-child") THEN
    FindVisible(ihWidget:FIRST-CHILD).
  ihWidget = ihWidget:NEXT-SIBLING.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAbsPosition) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAbsPosition Procedure 
FUNCTION getAbsPosition RETURNS INTEGER
  ( INPUT ihWidget AS HANDLE,
    INPUT cAxe     AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hWidget2   AS HANDLE NO-UNDO.
DEF VAR iAbsoluteX AS INT NO-UNDO.
DEF VAR iAbsoluteY AS INT NO-UNDO.

ASSIGN hWidget2   = ihWidget:PARENT
       iAbsoluteX = ihWidget:X
       iAbsoluteY = ihWidget:Y.
REPEAT WHILE hWidget2:TYPE NE "WINDOW":
  IF hWidget2:TYPE = "FRAME" THEN
    ASSIGN iAbsoluteX = iAbsoluteX + hWidget2:X
           iAbsoluteY = iAbsoluteY + hWidget2:Y.
  hWidget2 = hWidget2:PARENT.
END.
IF cAxe = "X" THEN
  RETURN iAbsoluteX. 
ELSE
  RETURN iAbsoluteY. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAllFrames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAllFrames Procedure 
FUNCTION getAllFrames RETURNS LOGICAL
  ( INPUT hWidget AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  List widgets to file
    Notes:  NB! Use level = 0 for initial call
------------------------------------------------------------------------------*/
  
REPEAT WHILE hWidget NE ?:
  
  IF hWidget:TYPE = "FRAME" AND hWidget:NAME NE "folder-frm" /* AND hWidget:NAME NE "panel-frame" */ THEN DO:
    IF NOT CAN-FIND(FIRST ttAllFrames
                    WHERE ttAllFrames.hWindow = hCurrWindow
                      AND ttAllFrames.hFrame  = hWidget) THEN DO:
      CREATE ttAllFrames.
      ASSIGN ttAllFrames.hWindow = hCurrWindow
             ttAllFrames.hFrame  = hWidget.
    END.
  END.
  IF CAN-QUERY(hWidget, 'FIRST-CHILD') AND hWidget:FIRST-CHILD  <> ? THEN
     getAllFrames (hWidget:FIRST-CHILD).

  hWidget = hWidget:NEXT-SIBLING.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAnyWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAnyWidget Procedure 
FUNCTION getAnyWidget RETURNS HANDLE
  ( INPUT hWidget    AS HANDLE, 
    INPUT iLevel     AS INT,
    INPUT icName     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  Return handle to a named widget
    Notes:  NB! Use level = 0 for initial call
------------------------------------------------------------------------------*/
DEF VAR hReturn AS HANDLE NO-UNDO.

REPEAT WHILE hWidget NE ?:

  IF hWidget:NAME = icName THEN  
    RETURN hWidget. 

  IF CAN-QUERY(hWidget,'FIRST-CHILD') AND hWidget:FIRST-CHILD  <> ? THEN
    hReturn = GetAnyWidget (hWidget:FIRST-CHILD,iLevel + 1,icName).

  IF VALID-HANDLE(hReturn) AND hReturn:NAME = icName THEN  
    RETURN hReturn. 

  IF iLevel > 0 THEN hWidget = hWidget:NEXT-SIBLING.
  ELSE LEAVE.
END.

RETURN hReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentDeltaX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentDeltaX Procedure 
FUNCTION getCurrentDeltaX RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN iDeltaX.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentDeltaY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentDeltaY Procedure 
FUNCTION getCurrentDeltaY RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN iDeltaY.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCustomWinSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCustomWinSettings Procedure 
FUNCTION getCustomWinSettings RETURNS CHARACTER
  ( INPUT ihWindow AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttSettingsLoaded
     WHERE ttSettingsLoaded.hWindow = ihWindow
     NO-ERROR.
IF AVAIL ttSettingsLoaded THEN 
  RETURN ttSettingsLoaded.cCustomSetting.
ELSE 
  RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFrameWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameWidget Procedure 
FUNCTION getFrameWidget RETURNS HANDLE
  ( INPUT ihFrame  AS HANDLE,
    INPUT icWidget AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hWdgt AS HANDLE NO-UNDO.          
hWdgt = ihFrame:FIRST-CHILD:FIRST-CHILD.
REPEAT WHILE VALID-HANDLE(hWdgt):
  IF hWdgt:NAME = icWidget THEN LEAVE.
  hWdgt = hWdgt:NEXT-SIBLING.
END.
RETURN hWdgt.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoadedSetting) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoadedSetting Procedure 
FUNCTION getLoadedSetting RETURNS CHARACTER
  (INPUT ihWindow  AS HANDLE,
   INPUT icType    AS CHAR  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttSettingsLoaded
     WHERE ttSettingsLoaded.hWindow = ihWindow 
     NO-ERROR.
IF AVAIL ttSettingsLoaded THEN DO:
  CASE icType:
    WHEN "winX"   THEN RETURN STRING(ttSettingsLoaded.iCurrX).
    WHEN "winY"   THEN RETURN STRING(ttSettingsLoaded.iCurrY).
    WHEN "spX"    THEN RETURN STRING(ttSettingsLoaded.iCurrSplitBarX).
    WHEN "spY"    THEN RETURN STRING(ttSettingsLoaded.iCurrSplitBarY).
    WHEN "col"    THEN RETURN ttSettingsLoaded.cColour.
    WHEN "coldir" THEN RETURN ttSettingsLoaded.cColourDir.
    OTHERWISE RETURN "".
  END CASE.
END.
ELSE 
  MESSAGE PROGRAM-NAME(1) SKIP 
          "not avail" 
          VIEW-AS ALERT-BOX.
RETURN "".
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOrgWigetPos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOrgWigetPos Procedure 
FUNCTION getOrgWigetPos RETURNS CHARACTER
  ( INPUT ihParent     AS HANDLE,
    INPUT icTypeList   AS CHAR,
    INPUT icNameList   AS CHAR,
    INPUT icExNameList AS CHAR,
    INPUT ihWidget     AS HANDLE,
    INPUT ihBuffer     AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose: Return a list of org.positions for widgets 
    Notes: ihParent can either be window or frame
           ihBuffer is automatically created if valid 
------------------------------------------------------------------------------*/
DEF VAR cReturn AS CHAR NO-UNDO.

FOR EACH ttOrgWidgetPos
    WHERE (IF ihParent:TYPE = "window" THEN
             ttOrgWidgetPos.hWindow = ihParent
           ELSE ttOrgWidgetPos.hFrame = ihParent)
      AND (IF VALID-HANDLE(ihWidget) THEN 
             ttOrgWidgetPos.hWidget = ihWidget
           ELSE TRUE)
    :
  IF icTypeList   NE "" AND NOT CAN-DO(icTypeList,ttOrgWidgetPos.hWidget:TYPE) THEN NEXT.
  IF icNameList   NE "" AND NOT CAN-DO(icNameList,ttOrgWidgetPos.hWidget:NAME) THEN NEXT.
  IF icExNameList NE "" AND CAN-DO(icExNameList,ttOrgWidgetPos.hWidget:NAME) THEN NEXT.

  IF VALID-HANDLE(ihBuffer) THEN DO:
    ihBuffer:BUFFER-CREATE().
    ihBuffer:BUFFER-COPY(BUFFER ttOrgWidgetPos:HANDLE).
  END.
  ELSE 
    ASSIGN cReturn = cReturn + (IF cReturn NE "" THEN "|" ELSE "")
                   + hWidget:NAME 
                   + "," + STRING(ttOrgWidgetPos.iX)
                   + "," + STRING(ttOrgWidgetPos.iY)
           .
END.

RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProcHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProcHandle Procedure 
FUNCTION getProcHandle RETURNS HANDLE
  ( INPUT phFocus AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hW AS HANDLE.
DEF VAR vh AS HANDLE.

IF NOT VALID-HANDLE(phFocus) THEN RETURN ?.

IF CAN-QUERY(phFocus,"frame") AND VALID-HANDLE(phFocus:FRAME) THEN
  hW = phFocus:FRAME:WINDOW. /* this will be the window handle */
ELSE IF CAN-QUERY(phFocus,"window") AND VALID-HANDLE(phFocus:WINDOW) THEN
  hW = phFocus:WINDOW.
ELSE RETURN ?.

vh = hW.

/* going up until we will get an invalid handle or our procedure handle */
REPEAT :
vh = WIDGET-HANDLE( STRING( INTEGER( STRING( vh:HANDLE ) ) + 1 ) ).
IF VALID-HANDLE( vh )
AND vh:TYPE = "PROCEDURE":U
AND vh:CURRENT-WINDOW = hW
THEN RETURN vh. /* this is my proc handle */
ELSE IF NOT VALID-HANDLE( vh ) THEN LEAVE.
END.

vh = hW.

/* no it is not there so we have to go in the lower range */
REPEAT :
vh = WIDGET-HANDLE( STRING( INTEGER( STRING( vh:HANDLE ) ) - 1 ) ).
IF VALID-HANDLE( vh )
AND vh:TYPE = "PROCEDURE":U
AND vh:CURRENT-WINDOW = hW
THEN RETURN vh. /* this is my proc handle */
ELSE IF NOT VALID-HANDLE( vh ) THEN LEAVE.
END.

/* well if I am here I cannot get the proc handle */
RETURN ?. /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getResizeSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getResizeSettings Procedure 
FUNCTION getResizeSettings RETURNS HANDLE
  ( INPUT ihWindow AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hCurrBuffer     AS HANDLE NO-UNDO.
DEF VAR hCurrWidget     AS HANDLE NO-UNDO.
DEF VAR chWindow        AS CHAR   NO-UNDO.
DEF VAR hWindow         AS HANDLE NO-UNDO.
DEF VAR hTempTable      AS HANDLE NO-UNDO.
DEF VAR httBuffer       AS HANDLE NO-UNDO.
DEF VAR hField          AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR cTableList      AS CHAR   NO-UNDO.
DEF VAR cCurrBufferName AS CHAR   NO-UNDO.
DEF VAR hWdgt           AS HANDLE NO-UNDO.
DEF VAR hFrame          AS HANDLE NO-UNDO.

CREATE TEMP-TABLE hTempTable.
hTempTable:ADD-NEW-FIELD("hFrame","HANDLE").
hTempTable:ADD-NEW-FIELD("cWidgetName","CHARACTER").
hTempTable:ADD-NEW-FIELD("cSourceProc","CHARACTER").
hTempTable:ADD-NEW-FIELD("cSetting","CHARACTER").
hTempTable:ADD-NEW-FIELD("fPercent","DECIMAL").
hTempTable:ADD-NEW-FIELD("cFrameName","CHARACTER").
hTempTable:ADD-NEW-FIELD("cWidget","CHARACTER").
hTempTable:ADD-NEW-FIELD("hWidget","HANDLE").

hTempTable:TEMP-TABLE-PREPARE("ttResize").
httBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.

cTableList = "ttNoResizeX,ttNoResizeY,ttAddResizeX,ttAddResizeY,ttNoMoveX,ttNoMoveY,ttAddMoveX,ttAddMoveY,ttFollowSplitBarX,ttFollowSplitBarY,ttResizeXgroup,ttResizeYgroup,ttMoveXgroup,ttMoveYgroup,ttAnchor,ttNeverResizeX,ttNeverResizeY".

DO ix = 1 TO NUM-ENTRIES(cTableList):
  CREATE BUFFER hCurrBuffer FOR TABLE ENTRY(ix,cTableList).
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hCurrBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + hCurrBuffer:NAME).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  cCurrBufferName = ENTRY(ix,cTableList).
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    ASSIGN chWindow        = hCurrBuffer:BUFFER-FIELD("chWindow"):BUFFER-VALUE 
/*            cCurrBufferName = hCurrBuffer:NAME */
           NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      hWindow = WIDGET-HANDLE(chWindow).
    ELSE hWindow = hCurrBuffer:BUFFER-FIELD("hWindow"):BUFFER-VALUE.
    IF hWindow = ihWindow THEN DO:
      httBuffer:BUFFER-CREATE().
      httBuffer:BUFFER-FIELD("cSetting"):BUFFER-VALUE = SUBSTR(hCurrBuffer:NAME,3).
      IF SUBSTR(cCurrBufferName,1,6) = "ttFoll" OR CAN-DO("ttAnchor,ttNeverResizeX,ttNeverResizeY",cCurrBufferName) THEN DO:
        ASSIGN hCurrWidget = hCurrBuffer:BUFFER-FIELD("hWidget"):BUFFER-VALUE NO-ERROR.
        IF VALID-HANDLE(hCurrWidget) THEN
          ASSIGN httBuffer:BUFFER-FIELD("hFrame"):BUFFER-VALUE = (IF hCurrWidget:TYPE = "FRAME" THEN
                                                                    hCurrWidget
                                                                  ELSE hCurrWidget:FRAME)
                 httBuffer:BUFFER-FIELD("cWidgetName"):BUFFER-VALUE = hCurrWidget:NAME
                 httBuffer:BUFFER-FIELD("cWidget"):BUFFER-VALUE = STRING(hCurrWidget)
                 httBuffer:BUFFER-FIELD("hWidget"):BUFFER-VALUE = hCurrWidget
                 NO-ERROR.
      END.
      ELSE DO:
        ASSIGN httBuffer:BUFFER-FIELD("hFrame"):BUFFER-VALUE = WIDGET-HANDLE(hCurrBuffer:BUFFER-FIELD("chFrame"):BUFFER-VALUE)
               httBuffer:BUFFER-FIELD("cWidgetName"):BUFFER-VALUE = hCurrBuffer:BUFFER-FIELD("cWidget"):BUFFER-VALUE
               httBuffer:BUFFER-FIELD("cWidget"):BUFFER-VALUE = hCurrBuffer:BUFFER-FIELD("cWidget"):BUFFER-VALUE
               hFrame = httBuffer:BUFFER-FIELD("hFrame"):BUFFER-VALUE
               NO-ERROR.
        hWdgt = WIDGET-HANDLE(hCurrBuffer:BUFFER-FIELD("cWidget"):BUFFER-VALUE) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
          httBuffer:BUFFER-FIELD("hWidget"):BUFFER-VALUE = hWdgt.
        ELSE IF httBuffer:BUFFER-FIELD("cWidgetName"):BUFFER-VALUE = hFrame:NAME THEN  
          httBuffer:BUFFER-FIELD("hWidget"):BUFFER-VALUE = httBuffer:BUFFER-FIELD("hFrame"):BUFFER-VALUE.
        ELSE DO:
          hWdgt = hFrame:FIRST-CHILD:FIRST-CHILD.
          REPEAT WHILE VALID-HANDLE(hWdgt):
            IF hWdgt:NAME = hCurrBuffer:BUFFER-FIELD("cWidget"):BUFFER-VALUE THEN  LEAVE.
            hWdgt = hWdgt:NEXT-SIBLING.
          END.
          IF VALID-HANDLE(hWdgt) THEN
            httBuffer:BUFFER-FIELD("hWidget"):BUFFER-VALUE = hWdgt.
        END.
      END.
    END.                 
    hQuery:GET-NEXT().
  END.
  DELETE OBJECT hQuery.
  DELETE OBJECT hCurrBuffer.
END.
  
RETURN hTempTable.   
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSaveSettingName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSaveSettingName Procedure 
FUNCTION getSaveSettingName RETURNS CHARACTER
  ( INPUT ihWindow AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hProc      AS HANDLE NO-UNDO.
DEF VAR cProcList  AS CHAR NO-UNDO.
DEF VAR cProc      AS CHAR NO-UNDO.

IF cSaveSettingsName NE "" THEN 
  RETURN cSaveSettingsName.

DO ix = 1 TO 10:
  IF PROGRAM-NAME(ix) NE ? THEN
    cProcList = cProcList + PROGRAM-NAME(ix) + " ".
END.
DO ix = 1 TO NUM-ENTRIES(cProcList," ").
  IF R-INDEX(ENTRY(ix,cProcList," "),".w") > 0 
     OR R-INDEX(ENTRY(ix,cProcList," "),".r") > 0 THEN DO:
    cProc = ENTRY(ix,cProcList," ").
    LEAVE.
  END.
END.

IF cProc MATCHES "*\*" THEN
  RETURN SUBSTR(cProc,R-INDEX(cProc,"\") + 1).
ELSE IF cProc MATCHES "*/*" THEN
  RETURN SUBSTR(cProc,R-INDEX(cProc,"/") + 1).
ELSE
  RETURN cProc.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSiblingNames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSiblingNames Procedure 
FUNCTION getSiblingNames RETURNS CHARACTER
  ( INPUT hWidget          AS HANDLE, 
    INPUT icTypes          AS CHAR,
    INPUT icExceptionTypes AS CHAR,
    INPUT icExceptionNames AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  Get widget names contained in hWidget
    Notes:  NB! Use level = 0 for initial call
------------------------------------------------------------------------------*/
DEF VAR cDummy AS CHAR NO-UNDO.
DEF VAR iAbsX               AS INT  NO-UNDO.
DEF VAR iAbsY               AS INT  NO-UNDO.

IF icTypes = "" THEN
  icTypes = "BROWSE,BUTTON,COMBO-BOX,FILL-IN,EDITOR,FRAME,MENU-ITEM,RADIO-SET,RECTANGLE,SELECTION-LIST,SLIDER,SUB-MENU,TEXT,TOGGLE-BOX".

REPEAT WHILE hWidget NE ?:

  IF CAN-DO(icTypes,hWidget:TYPE)
  AND (IF icExceptionNames NE "" THEN NOT CAN-DO(icExceptionNames,hWidget:NAME) ELSE TRUE)
  AND (IF icExceptionTypes NE "" THEN NOT CAN-DO(icExceptionTypes,hWidget:TYPE) ELSE TRUE) THEN 
    cWidgetList = cWidgetList + hWidget:NAME + ",".

  hWidget = hWidget:NEXT-SIBLING.
END.

RETURN TRIM(cWidgetList,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSplitBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSplitBar Procedure 
FUNCTION getSplitBar RETURNS HANDLE
  ( INPUT ihWindow    AS HANDLE,
    INPUT icDirection AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icDirection = "x" THEN DO:
  FIND FIRST ttSplitBarX
       WHERE ttSplitBarX.chWindow  = STRING(ihWindow) 
       NO-ERROR.
  IF AVAIL ttSplitBarX THEN
    RETURN ttSplitBarX.hSplitBar.
END.
ELSE DO:
  FIND FIRST ttSplitBarY
       WHERE ttSplitBarY.chWindow  = STRING(ihWindow) 
       NO-ERROR.
  IF AVAIL ttSplitBarY THEN
    RETURN ttSplitBarY.hSplitBar.
END.

RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSplitBarHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSplitBarHandle Procedure 
FUNCTION getSplitBarHandle RETURNS HANDLE
  ( INPUT ihWindow AS HANDLE,
    INPUT icDirection AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icDirection = "x" OR icDirection = "" THEN DO:
  FIND FIRST ttSplitBarX 
       WHERE ttSplitBarX.chWindow = STRING(ihWindow)
       NO-ERROR.
  IF AVAIL ttSplitBarX THEN RETURN ttSplitBarX.hSplitBar.
END.

IF icDirection = "y" OR icDirection = "" THEN DO:
  FIND FIRST ttSplitBarY 
       WHERE ttSplitBarY.chWindow = STRING(ihWindow)
       NO-ERROR.
  IF AVAIL ttSplitBarY THEN RETURN ttSplitBarY.hSplitBar.
END.

RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWidgetHandles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWidgetHandles Procedure 
FUNCTION getWidgetHandles RETURNS CHARACTER
  ( INPUT hWidget    AS HANDLE, 
    INPUT iLevel     AS INT,
    INPUT iMinX      AS INT,
    INPUT iMaxX      AS INT,
    INPUT iMinY      AS INT,
    INPUT iMaxY      AS INT,
    INPUT icTypes    AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  List widgets to file
    Notes:  NB! Use level = 0 for initial call
------------------------------------------------------------------------------*/
DEF VAR cDummy AS CHAR NO-UNDO.

IF iLevel = 0 THEN
  cWidgetList = "".
IF icTypes = "" THEN
  icTypes = "BROWSE,BUTTON,COMBO-BOX,FILL-IN,EDITOR,FRAME,MENU-ITEM,RADIO-SET,SELECTION-LIST,SLIDER,SUB-MENU,TEXT,TOGGLE-BOX".

REPEAT WHILE hWidget NE ?:

  IF CAN-DO(icTypes,hWidget:TYPE) 
     AND hWidget:X GE iMinX
     AND hWidget:X + hWidget:WIDTH-PIXELS LE iMaxX
     AND hWidget:Y GE iMinY
     AND hWidget:Y + hWidget:HEIGHT-PIXELS LE iMaxY THEN 
    cWidgetList = cWidgetList + STRING(hWidget) + ",".

  IF CAN-QUERY(hWidget, 'FIRST-CHILD') AND hWidget:FIRST-CHILD  <> ? THEN
    cDummy = GetWidgetHandles ( hWidget:FIRST-CHILD,iLevel + 1,
                                iMinX,iMaxX,iMinY,iMaxY,icTypes).

  IF iLevel > 0 THEN hWidget = hWidget:NEXT-SIBLING.
  ELSE LEAVE.
END.

IF iLevel = 0 THEN
  RETURN TRIM(cWidgetList,",").
ELSE 
  RETURN cWidgetList.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWidgetNamesByLasso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWidgetNamesByLasso Procedure 
FUNCTION getWidgetNamesByLasso RETURNS CHARACTER
  ( INPUT hWidget    AS HANDLE, 
    INPUT iLevel     AS INT,
    INPUT icTypes    AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  Get widget names confined by hWidget
    Notes:  NB! Use level = 0 for initial call
------------------------------------------------------------------------------*/
DEF VAR cDummy AS CHAR NO-UNDO.
DEF VAR iAbsX               AS INT  NO-UNDO.
DEF VAR iAbsY               AS INT  NO-UNDO.


IF iLevel = 0 THEN DO:
  cWidgetList = "".
  ASSIGN iMinX = getAbsPosition(hWidget,"X") 
         iMinY = getAbsPosition(hWidget,"Y")
         iMaxX = iMinX + hWidget:WIDTH-PIXELS
         iMaxY = iMinY + hWidget:HEIGHT-PIXELS
         .
  IF hWidget:TYPE = "rectangle" THEN
    hWidget = hWidget:WINDOW.
END.
IF icTypes = "" THEN
  icTypes = "BROWSE,BUTTON,COMBO-BOX,FILL-IN,EDITOR,FRAME,MENU-ITEM,RADIO-SET,SELECTION-LIST,SLIDER,SUB-MENU,TEXT,TOGGLE-BOX".

REPEAT WHILE hWidget NE ?:

  IF CAN-DO(icTypes,hWidget:TYPE) THEN DO:
    ASSIGN iAbsX = getAbsPosition(hWidget,"X")
           iAbsY = getAbsPosition(hWidget,"Y")
           .
    IF iAbsX GE iMinX
       AND iAbsX + hWidget:WIDTH-PIXELS LE iMaxX
       AND iAbsY GE iMinY
       AND hWidget:NAME NE ?
       AND iAbsY + hWidget:HEIGHT-PIXELS LE iMaxY THEN 
    cWidgetList = cWidgetList + hWidget:NAME + ",".
  END.
  IF CAN-QUERY(hWidget,'FIRST-CHILD') AND hWidget:FIRST-CHILD  <> ? THEN
    cDummy = GetWidgetNamesByLasso ( hWidget:FIRST-CHILD,iLevel + 1,icTypes).

  IF iLevel > 0 THEN hWidget = hWidget:NEXT-SIBLING.
  ELSE LEAVE.

END.

IF iLevel = 0 THEN
  RETURN TRIM(cWidgetList,",").
ELSE 
  RETURN cWidgetList.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWidgetsByLasso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWidgetsByLasso Procedure 
FUNCTION getWidgetsByLasso RETURNS CHARACTER
  ( INPUT hWidget    AS HANDLE, 
    INPUT iLevel     AS INT,
    INPUT icTypes    AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  Get widget handles confined by hWidget
    Notes:  NB! Use level = 0 for initial call
------------------------------------------------------------------------------*/
DEF VAR cDummy AS CHAR NO-UNDO.
DEF VAR iAbsX               AS INT  NO-UNDO.
DEF VAR iAbsY               AS INT  NO-UNDO.


IF iLevel = 0 THEN DO:
  cWidgetList = "".
  ASSIGN iMinX = getAbsPosition(hWidget,"X") 
         iMinY = getAbsPosition(hWidget,"Y")
         iMaxX = iMinX + hWidget:WIDTH-PIXELS
         iMaxY = iMinY + hWidget:HEIGHT-PIXELS
         .
  IF hWidget:TYPE = "rectangle" THEN
    hWidget = hWidget:WINDOW.
END.
IF icTypes = "" THEN
  icTypes = "BROWSE,BUTTON,COMBO-BOX,FILL-IN,EDITOR,FRAME,MENU-ITEM,RADIO-SET,SELECTION-LIST,SLIDER,SUB-MENU,TEXT,TOGGLE-BOX".

REPEAT WHILE hWidget NE ?:
  IF NOT CAN-DO(cLassoExceptionList,STRING(hWidget)) THEN DO:     
    IF CAN-DO(icTypes,hWidget:TYPE) THEN DO:
      ASSIGN iAbsX = getAbsPosition(hWidget,"X")
             iAbsY = getAbsPosition(hWidget,"Y")
             .
      IF iAbsX GE iMinX
         AND iAbsX + hWidget:WIDTH-PIXELS LE iMaxX
         AND iAbsY GE iMinY
         AND iAbsY + hWidget:HEIGHT-PIXELS LE iMaxY THEN 
      cWidgetList = cWidgetList + STRING(hWidget) + ",".
    END.
    IF CAN-QUERY(hWidget,'FIRST-CHILD') AND hWidget:FIRST-CHILD  <> ? THEN
      cDummy = GetWidgetsByLasso ( hWidget:FIRST-CHILD,iLevel + 1,icTypes).
  END.

  IF iLevel > 0 THEN hWidget = hWidget:NEXT-SIBLING.
  ELSE LEAVE.
END.

IF iLevel = 0 THEN DO:
  cLassoExceptionList = "".
  RETURN TRIM(cWidgetList,",").
END.
ELSE 
  RETURN cWidgetList.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWinMinXmove) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWinMinXmove Procedure 
FUNCTION getWinMinXmove RETURNS INTEGER
  ( INPUT ihWindow AS HANDLE,
    INPUT icXY     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttOrgWiNSIZE 
     WHERE ttOrgWinSize.chWindow = STRING(ihWindow)
     NO-ERROR.
IF AVAIL TTORGWINSIZE THEN DO:
  IF icXY = "X" THEN
    RETURN ttOrgWinSize.iMinXmove.
  ELSE
    RETURN ttOrgWinSize.iMinYmove.
END.

RETURN 0. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-overridesWithinFrame) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION overridesWithinFrame Procedure 
FUNCTION overridesWithinFrame RETURNS LOGICAL
  ( INPUT icFrame     AS CHAR,
    INPUT icFrameName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF CAN-FIND(FIRST ttAddResizeX WHERE ttAddResizeX.chFrame = icFrame AND ttAddResizeX.cWidget NE icFrameName) THEN RETURN TRUE.
IF CAN-FIND(FIRST ttAddResizeY WHERE ttAddResizeY.chFrame = icFrame AND ttAddResizeY.cWidget NE icFrameName) THEN RETURN TRUE.
IF CAN-FIND(FIRST ttNoResizeX  WHERE ttNoResizeX.chFrame  = icFrame AND ttNoResizeX.cWidget  NE icFrameName) THEN RETURN TRUE.
IF CAN-FIND(FIRST ttNoResizeY  WHERE ttNoResizeY.chFrame  = icFrame AND ttNoResizeY.cWidget  NE icFrameName) THEN RETURN TRUE.
IF CAN-FIND(FIRST ttAddMoveX   WHERE ttAddMoveX.chFrame   = icFrame AND ttAddMoveX.cWidget   NE icFrameName) THEN RETURN TRUE.
IF CAN-FIND(FIRST ttAddMoveY   WHERE ttAddMoveY.chFrame   = icFrame AND ttAddMoveY.cWidget   NE icFrameName) THEN RETURN TRUE.
IF CAN-FIND(FIRST ttNoMoveX    WHERE ttNoMoveX.chFrame    = icFrame AND ttNoMoveX.cWidget    NE icFrameName) THEN RETURN TRUE.
IF CAN-FIND(FIRST ttNoMoveY    WHERE ttNoMoveY.chFrame    = icFrame AND ttNoMoveY.cWidget    NE icFrameName) THEN RETURN TRUE.

RETURN FALSE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-removeSplitBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeSplitBar Procedure 
FUNCTION removeSplitBar RETURNS LOGICAL
  ( INPUT hWindow AS HANDLE,
    INPUT hSplitBar AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FOR EACH ttSplitBarX 
    WHERE ttSplitBarX.chWindow = STRING(hWindow)
      AND ttSplitBarX.hSplitBar = hSplitBar:
  DELETE ttSplitBarX.
END.
FOR EACH ttFollowSplitBarX 
    WHERE ttFollowSplitBarX.chWindow = STRING(hWindow)
      AND ttFollowSplitBarX.hSplitBar = hSplitBar:
  DELETE ttFollowSplitBarX.
END.
FOR EACH ttSplitBarY 
    WHERE ttSplitBarY.chWindow = STRING(hWindow)
      AND ttSplitBarY.hSplitBar = hSplitBar:
  DELETE ttSplitBarY.
END.
FOR EACH ttFollowSplitBarY 
    WHERE ttFollowSplitBarY.chWindow = STRING(hWindow)
      AND ttFollowSplitBarY.hSplitBar = hSplitBar:
  DELETE ttFollowSplitBarY.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReplaceInText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReplaceInText Procedure 
FUNCTION ReplaceInText RETURNS CHARACTER
  ( INPUT icText    AS CHAR,
    INPUT icReplace AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: icReplace shoud be on form <src>¤<target>|<src>¤<target>|.. 
------------------------------------------------------------------------------*/
DEF VAR cNewText     AS CHAR NO-UNDO.
DEF VAR cReplaceText AS CHAR NO-UNDO INIT "CHR(10)¤ |<b>¤</b>¤<hr>".
DEF VAR ix           AS INT NO-UNDO.

IF icReplace = "" THEN
  cReplaceText = CHR(10) + "¤ |" + CHR(9) + "¤ |" + CHR(13) + "¤ |<b>¤|</b>¤|<hr>¤".
ELSE
  cReplaceText = icReplace.

cNewText = icText.

DO ix = 1 TO NUM-ENTRIES(cReplaceText,"|"):
  cNewText = REPLACE(cNewText,ENTRY(1,ENTRY(ix,cReplaceText,"|"),"¤"),ENTRY(2,ENTRY(ix,cReplaceText,"|"),"¤")).
END.

RETURN cNewText.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-resetSplitBarPos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION resetSplitBarPos Procedure 
FUNCTION resetSplitBarPos RETURNS LOGICAL
  ( INPUT hWindow AS HANDLE,
    INPUT iOrgDeltaX AS INT ) :
/*------------------------------------------------------------------------------
  Purpose: Pull back splitbar(s) to their original position 
    Notes: -  Called from setWidgetResize (when calltype = "resize")
           -  Called from procedure that suppresses a new frame into the window and then calls setWidgetResize with calltype "adjust" 
                    (iOrgDeltaX is then always zero)  
------------------------------------------------------------------------------*/
FIND FIRST ttSplitBarX
     WHERE ttSplitBarX.chWindow = STRING(hWindow) NO-ERROR.
FIND FIRST ttSplitBarY
     WHERE ttSplitBarY.chWindow = STRING(hWindow) NO-ERROR.

IF AVAIL ttSplitBarX THEN DO:
  ASSIGN iOrgSplitXpos           = ttSplitBarX.hSplitBar:X
         ttSplitBarX.hSplitBar:X = ttSplitBarX.iOrgSplitBarX.
  setSplitBarX(hWindow,ttSplitBarX.hSplitBar,?).
  iAbsXscopeY = ttSplitBarX.hSplitBar:HEIGHT-PIXELS.
END.
IF AVAIL ttSplitBarY THEN DO:
  ASSIGN iOrgSplitYpos           = ttSplitBarY.hSplitBar:Y
         ttSplitBarY.hSplitBar:Y = ttSplitBarY.iOrgSplitBarY NO-ERROR.
  setSplitBarY(hWindow,ttSplitBarY.hSplitBar,?).
  /* Adjust horizontal splitbar width according to the vertical: */
  IF cAction = "resize" THEN DO:
    IF AVAIL ttSplitBarX 
       AND ttSplitBarY.hSplitFrame:X < ttSplitBarX.hSplitFrame:X
       AND ttSplitBarY.hSplitFrame NE ttSplitBarY.hMainFrame THEN DO:
      ttSplitBarY.hSplitFrame:WIDTH-PIXELS = ttSplitBarY.iOrgSplitWidth.
      IF ttSplitBarY.hSplitBar:WIDTH-PIXELS GE ttSplitBarY.iOrgSplitWidth THEN DO:
        ttSplitBarY.hSplitBar:WIDTH-PIXELS = ttSplitBarY.iOrgSplitWidth - ttSplitBarY.hSplitBar:X - 2 NO-ERROR.
        ttSplitBarY.hSplitBar:X = 1.
      END.
      ttSplitBarY.hSplitBar:WIDTH-PIXELS = ttSplitBarY.iOrgSplitWidth - 2 NO-ERROR.
    END.
    ELSE IF ttSplitBarY.hSplitFrame = ttSplitBarY.hMainFrame THEN
      ttSplitBarY.hSplitBar:WIDTH-PIXELS = ttSplitBarY.hSplitBar:WIDTH-PIXELS + iOrgDeltaX NO-ERROR.
  END.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ResizeKeepsWindowLocked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ResizeKeepsWindowLocked Procedure 
FUNCTION ResizeKeepsWindowLocked RETURNS LOGICAL
  ( INPUT ihLockWinForResize AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hLockWinForResize = ihLockWinForResize.

IF ihLockWinForResize = ? THEN setLockWindowUpdate (NO).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveWinSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SaveWinSettings Procedure 
FUNCTION SaveWinSettings RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RUN DoSaveWinSettings (ihWindow).
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAddMoveX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAddMoveX Procedure 
FUNCTION setAddMoveX RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate widgets for exeption of general move / resize
    Notes:  
------------------------------------------------------------------------------*/
IF ENTRY(1,cWidgetList) NE "+" THEN
  FOR EACH ttAddMoveX
      WHERE ttAddMoveX.chFrame  = STRING(hFrame)
        AND ttAddMoveX.chWindow = STRING(hWindow):
    DELETE ttAddMoveX.
  END.

DO ix = 1 TO NUM-ENTRIES(cWidgetList):
  IF ENTRY(ix,cWidgetList) = "+" THEN NEXT.
  CREATE ttAddMoveX.
  ASSIGN ttAddMoveX.chFrame  = STRING(hFrame)
         ttAddMoveX.chWindow = STRING(hWindow)
         ttAddMoveX.cWidget  = ENTRY(ix,cWidgetList).
END.

IF bOverLoadSettings THEN DoLoadSettings (hWindow).

IF bDebug THEN PUBLISH "DebugResize" (SOURCE-PROCEDURE).

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAddMoveY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAddMoveY Procedure 
FUNCTION setAddMoveY RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate widgets for exeption of general move / resize
    Notes:  
------------------------------------------------------------------------------*/
IF ENTRY(1,cWidgetList) NE "+" THEN
  FOR EACH ttAddMoveY
      WHERE ttAddMoveY.chWindow = STRING(hWindow)
        AND ttAddMoveY.chFrame  = STRING(hFrame):
    DELETE ttAddMoveY.
  END.

DO ix = 1 TO NUM-ENTRIES(cWidgetList):
  IF ENTRY(ix,cWidgetList) = "+" THEN NEXT.
  CREATE ttAddMoveY.
  ASSIGN ttAddMoveY.chFrame  = STRING(hFrame)
         ttAddMoveY.chWindow = STRING(hWindow)
         ttAddMoveY.cWidget  = ENTRY(ix,cWidgetList).
END.

IF bOverLoadSettings THEN DoLoadSettings (hWindow).

IF bDebug THEN PUBLISH "DebugResize" (SOURCE-PROCEDURE).

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAddResizeX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAddResizeX Procedure 
FUNCTION setAddResizeX RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate widgets for exeption of general move / resize
    Notes:  
------------------------------------------------------------------------------*/
IF ENTRY(1,cWidgetList) NE "+" THEN
  FOR EACH ttAddResizeX
      WHERE ttAddResizeX.chWindow = STRING(hWindow)
        AND ttAddResizeX.chFrame  = STRING(hFrame):
    DELETE ttAddResizeX.
  END.

DO ix = 1 TO NUM-ENTRIES(cWidgetList):
  IF ENTRY(ix,cWidgetList) = "+" THEN NEXT.
  CREATE ttAddResizeX.
  ASSIGN ttAddResizeX.chFrame  = STRING(hFrame)
         ttAddResizeX.chWindow = STRING(hWindow)
         ttAddResizeX.cWidget  = ENTRY(ix,cWidgetList).
END.

IF bDebug THEN PUBLISH "DebugResize" (SOURCE-PROCEDURE).

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAddResizeY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAddResizeY Procedure 
FUNCTION setAddResizeY RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate widgets for exeption of general move / resize
    Notes:  
------------------------------------------------------------------------------*/
IF ENTRY(1,cWidgetList) NE "+" THEN
  FOR EACH ttAddResizeY
      WHERE ttAddResizeY.chWindow = STRING(hWindow)
        AND ttAddResizeY.chFrame  = STRING(hFrame):
    DELETE ttAddResizeY.
  END.

DO ix = 1 TO NUM-ENTRIES(cWidgetList):
  IF ENTRY(ix,cWidgetList) = "+" THEN NEXT.
  CREATE ttAddResizeY.
  ASSIGN ttAddResizeY.chFrame  = STRING(hFrame)
         ttAddResizeY.chWindow = STRING(hWindow)
         ttAddResizeY.cWidget  = ENTRY(ix,cWidgetList).
END.

IF bOverLoadSettings THEN DoLoadSettings (hWindow).

IF bDebug THEN PUBLISH "DebugResize" (SOURCE-PROCEDURE).

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setADMFolderRect) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setADMFolderRect Procedure 
FUNCTION setADMFolderRect RETURNS LOGICAL
  ( INPUT ihFolderFrm AS HANDLE,
    INPUT icAction    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hRectangle AS HANDLE NO-UNDO.
/* DEF VAR hBuff AS HANDLE NO-UNDO.       */
/*                                        */
/* hBuff = BUFFER ttADMfolderRect:HANDLE. */

IF icAction = "minimize" THEN DO:
  hRectangle = ihFolderFrm:FIRST-CHILD.
  hRectangle = hRectangle:FIRST-CHILD.
  REPEAT WHILE VALID-HANDLE(hRectangle):

    IF hRectangle:TYPE = "rectangle" THEN DO:
      IF NOT CAN-FIND(FIRST ttADMFolderRect WHERE ttADMFolderRect.hRect = hRectangle) THEN DO:
        CREATE ttADMFolderRect.
        ASSIGN ttADMFolderRect.hWindow    = ihFolderFrm:WINDOW
               ttADMFolderRect.hFolderFrm = ihFolderFrm
               ttADMFolderRect.hRect      = hRectangle
               ttADMFolderRect.ixPos      = IF hRectangle:NAME = "rect-right" THEN ihFolderFrm:WIDTH-PIXELS - hRectangle:X ELSE hRectangle:X
               ttADMFolderRect.iyRelPos   = IF hRectangle:NAME = "rect-bottom" THEN ihFolderFrm:HEIGHT-PIXELS - hRectangle:Y ELSE hRectangle:Y
               ttADMFolderRect.iRelWidth  = IF CAN-DO("rect-top,rect-bottom,rect-main",hRectangle:NAME) THEN 
                                              ihFolderFrm:WIDTH-PIXELS  - hRectangle:WIDTH-PIXELS
                                            ELSE hRectangle:WIDTH-PIXELS
               ttADMFolderRect.iRelHeight = IF CAN-DO("rect-left,rect-right,rect-main",hRectangle:NAME) THEN
                                              ihFolderFrm:HEIGHT-PIXELS - hRectangle:HEIGHT-PIXELS
                                            ELSE hRectangle:HEIGHT-PIXELS
               .
      END.
      ASSIGN hRectangle:WIDTH-PIXELS    = MIN(10,hRectangle:WIDTH-PIXELS)
             hRectangle:HEIGHT-PIXELS   = MIN(10,hRectangle:HEIGHT-PIXELS)
             hRectangle:X = 30
             hRectangle:Y = 30
             .
    END.
    hRectangle = hRectangle:NEXT-SIBLING.
  END.
/*   toexcelviafile(hBuff,0).  */
END.
ELSE IF icAction = "restore" THEN DO:
  FOR EACH ttADMFolderRect
      WHERE ttADMFolderRect.hFolderFrm = ihFolderFrm:
    IF iDeltaX > 0 THEN
      ASSIGN ttADMFolderRect.hRect:X             = IF ttADMFolderRect.hRect:NAME = "rect-right" THEN 
                                                     ttADMFolderRect.hFolderFrm:WIDTH-PIXELS - ttADMFolderRect.ixPos
                                                   ELSE ttADMFolderRect.ixPos
             ttADMFolderRect.hRect:WIDTH-PIXELS  = IF CAN-DO("rect-top,rect-bottom,rect-main",ttADMFolderRect.hRect:NAME) THEN 
                                                     ttADMFolderRect.hFolderFrm:WIDTH-PIXELS  - ttADMFolderRect.iRelWidth
                                                   ELSE ttADMFolderRect.iRelWidth.
    ELSE
      ASSIGN ttADMFolderRect.hRect:WIDTH-PIXELS  = MIN(10,ttADMFolderRect.hRect:WIDTH-PIXELS) 
             ttADMFolderRect.hRect:X             = IF ttADMFolderRect.hRect:NAME = "rect-right" THEN
                                                     ttADMFolderRect.hFolderFrm:WIDTH-PIXELS - ttADMFolderRect.ixPos
                                                   ELSE ttADMFolderRect.ixPos
             ttADMFolderRect.hRect:WIDTH-PIXELS  = IF CAN-DO("rect-top,rect-bottom,rect-main",ttADMFolderRect.hRect:NAME) THEN
                                                     ttADMFolderRect.hFolderFrm:WIDTH-PIXELS  - ttADMFolderRect.iRelWidth
                                                   ELSE ttADMFolderRect.iRelWidth.
    IF iDeltaY > 0 THEN
      ASSIGN ttADMFolderRect.hRect:Y             = IF ttADMFolderRect.hRect:NAME = "rect-bottom" THEN 
                                                     ttADMFolderRect.hFolderFrm:HEIGHT-PIXELS - ttADMFolderRect.iyRelPos
                                                   ELSE ttADMFolderRect.iyRelPos
             ttADMFolderRect.hRect:HEIGHT-PIXELS = IF CAN-DO("rect-left,rect-right,rect-main",ttADMFolderRect.hRect:NAME) THEN
                                                     ttADMFolderRect.hFolderFrm:HEIGHT-PIXELS - ttADMFolderRect.iRelHeight
                                                   ELSE ttADMFolderRect.iRelHeight.
    ELSE
      ASSIGN ttADMFolderRect.hRect:HEIGHT-PIXELS = MIN(10,ttADMFolderRect.hRect:HEIGHT-PIXELS)
             ttADMFolderRect.hRect:Y             = IF ttADMFolderRect.hRect:NAME = "rect-bottom" THEN 
                                                     ttADMFolderRect.hFolderFrm:HEIGHT-PIXELS - ttADMFolderRect.iyRelPos
                                                   ELSE ttADMFolderRect.iyRelPos
             ttADMFolderRect.hRect:HEIGHT-PIXELS = IF CAN-DO("rect-left,rect-right,rect-main",ttADMFolderRect.hRect:NAME) THEN
                                                     ttADMFolderRect.hFolderFrm:HEIGHT-PIXELS - ttADMFolderRect.iRelHeight
                                                   ELSE ttADMFolderRect.iRelHeight.

/*     IF NOT (ttADMFolderRect.hRect:Y = 0 AND ttADMFolderRect.hRect:NAME = "rect-top") THEN  */
    ttADMFolderRect.hRect:MOVE-TO-TOP().
  END.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAnchor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAnchor Procedure 
FUNCTION setAnchor RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hAnchor     AS HANDLE,
    INPUT cWidgetList AS CHAR,
    INPUT icDirection AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate widgets for exeption of general move / resize
    Notes:  If no direction is given the anchor will work both ways
            The anchor only works with splitbars
------------------------------------------------------------------------------*/
/* FOR EACH ttAnchor                     */
/*     WHERE ttAnchor.hWindow = hWindow  */
/*       AND ttAnchor.hAnchor = hAnchor: */
/*   DELETE ttAnchor.                    */
/* END.                                  */

DO ix = 1 TO NUM-ENTRIES(cWidgetList):
  CREATE ttAnchor.
  ASSIGN ttAnchor.hAnchor       = hAnchor
         ttAnchor.hWindow       = hWindow
         ttAnchor.hWidget       = WIDGET-HANDLE(ENTRY(ix,cWidgetList))
         ttAnchor.iAnchorDeltaX = IF icDirection = "Y" THEN 0 ELSE ttAnchor.hWidget:X - (hAnchor:X + hAnchor:WIDTH-PIXELS) 
         ttAnchor.iAnchorDeltaY = IF icDirection = "X" THEN 0 ELSE ttAnchor.hWidget:Y - (hAnchor:Y + hAnchor:HEIGHT-PIXELS)
         .
END.

IF bDebug THEN PUBLISH "DebugResize" (SOURCE-PROCEDURE).

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppFont) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAppFont Procedure 
FUNCTION setAppFont RETURNS INTEGER
  ( INPUT icFont        AS CHAR,
    INPUT icEnvironment AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Environment equals the name of the .ini file. 
           Default is progress.ini 
------------------------------------------------------------------------------*/
DEF VAR cValue AS CHAR NO-UNDO.
bOk = NO.
DO ix = 0 TO FONT-TABLE:NUM-ENTRIES - 1:
  GET-KEY-VALUE SECTION "fonts" KEY "font" + STRING(ix) VALUE cValue.
  bOk = cValue = icFont.
  IF bOk THEN 
    LEAVE.
END.
IF NOT bOk THEN DO:
   FONT-TABLE:NUM-ENTRIES = ix. 
   PUT-KEY-VALUE SECTION "fonts" KEY "font" + STRING(ix) VALUE icFont.
   IF icEnvironment = "" THEN icEnvironment = "progress".
   ELSE IF INDEX(icEnvironment,".") > 0 THEN
     icEnvironment = SUBSTR(icEnvironment,1,INDEX(icEnvironment,".") - 1).

   LOAD icEnvironment BASE-KEY "INI".
   USE icEnvironment.
END.
  
DYNAMIC-FUNCTION("setAttribute",SESSION,"appfont" + icFont,STRING(ix)) NO-ERROR.

RETURN ix.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAutoSaveWinSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAutoSaveWinSettings Procedure 
FUNCTION setAutoSaveWinSettings RETURNS LOGICAL
  ( INPUT ibAutoSave AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bGlobAutoSaveSettings = ibAutoSave.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBGcolour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBGcolour Procedure 
FUNCTION setBGcolour RETURNS LOGICAL
  ( INPUT ihFrame AS HANDLE,
    INPUT icColour AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
 For more information, see:
     http://www.4gl.fr        
-----------------------------------------------------------------*/
DEF VAR I-COLOR                 AS INT    NO-UNDO.
DEF VAR h_BackGround            AS HANDLE NO-UNDO.
DEF VAR h_Group                 AS HANDLE NO-UNDO.
DEF VAR h_Field                 AS HANDLE NO-UNDO.
DEF VAR Height_Rectangle_Pixels AS INT NO-UNDO.
DEF VAR Height_Frame            AS INT NO-UNDO.
DEF VAR N_Rectangles            AS INT NO-UNDO.
DEF VAR Red-Value               AS INT NO-UNDO.
DEF VAR Green-Value             AS INT NO-UNDO.
DEF VAR Blue-Value              AS INT NO-UNDO  initial 10.
DEF VAR AddRed                  AS INT NO-UNDO.
DEF VAR AddGreen                AS INT NO-UNDO.
DEF VAR AddBlue                 AS INT NO-UNDO  initial 4.        
DEF VAR GapColor                AS INT NO-UNDO INIT 6. /* initial 4. */
DEF VAR iOrgEntries             AS INT NO-UNDO.

DEF VAR iFirst          AS INT NO-UNDO.
DEF VAR iLast           AS INT NO-UNDO.
DEF VAR iPrevBGcol      AS INT NO-UNDO.
DEF VAR hFieldSpX       AS HANDLE NO-UNDO.
DEF VAR hBogus          AS HANDLE NO-UNDO.

IF NOT bGlobEnableColour THEN RETURN FALSE.

/* Look at this one - it might not be that FIRST-CHILD is the main frame.. */
hBogus = ihFrame:NEXT-SIBLING NO-ERROR.
REPEAT WHILE hBogus NE ?:
  IF hBogus:WIDTH-PIXELS * hBogus:HEIGHT-PIXELS > ihFrame:WIDTH-PIXELS * ihFrame:HEIGHT-PIXELS THEN 
    ihFrame = hBogus.
  hBogus = hBogus:NEXT-SIBLING.
END.

FIND FIRST ttBGcolour WHERE ttBGcolour.hFrame = ihFrame NO-ERROR.
IF NOT AVAIL ttBGcolour THEN RETURN FALSE.

IF ttBGcolour.bAllFrames THEN 
  getAllFrames(ihFrame:FIRST-CHILD).
ELSE DO:
  FIND FIRST ttSplitBarX
       WHERE ttSplitBarX.chWindow = STRING(hCurrWindow)
       NO-ERROR.
  IF AVAIL ttSplitBarX THEN DO:
    FIND FIRST ttAllFrames
         WHERE ttAllFrames.hWindow = hCurrWindow
           AND ttAllFrames.hFrame  = ttSplitBarX.hSplitFrame
         NO-ERROR.
    IF NOT AVAIL ttAllFrames THEN DO:
      CREATE ttAllFrames.
      ASSIGN ttAllFrames.hWindow = hCurrWindow
             ttAllFrames.hFrame  = ttSplitBarX.hSplitFrame.
    END.
  END.
  FIND FIRST ttSplitBarY
       WHERE ttSplitBarY.chWindow = STRING(hCurrWindow)
       NO-ERROR.
  IF AVAIL ttSplitBarY THEN DO:
    FIND FIRST ttAllFrames
         WHERE ttAllFrames.hWindow = hCurrWindow
           AND ttAllFrames.hFrame  = ttSplitBarY.hSplitFrame
         NO-ERROR.
    IF NOT AVAIL ttAllFrames THEN DO:
      CREATE ttAllFrames.
      ASSIGN ttAllFrames.hWindow = hCurrWindow
             ttAllFrames.hFrame  = ttSplitBarY.hSplitFrame.
    END.
  END.
END.

DoRemoveBGcolour (ihFrame:BACKGROUND).
FOR EACH ttAllFrames 
    WHERE ttAllFrames.hWindow = hCurrWindow
      AND VALID-HANDLE(ttAllFrames.hFrame):
  DoRemoveBGcolour (ttAllFrames.hFrame:BACKGROUND).
END.

IF NOT bKeepOrgColour AND icColour NE "turn" THEN DO:
  IF icColour = "normal" THEN 
    ttBGcolour.cColour = icColour.
  ELSE IF icColour = "org" THEN 
    icColour = ttBGcolour.cColour.
  ELSE DO:
    IF icColour NE "" THEN ttBGcolour.cColour = icColour.
    ELSE icColour = ttBGcolour.cColour.
  END.
END.
ELSE IF NOT bKeepOrgColour THEN
  ASSIGN ttBGcolour.cColourDir = IF ttBGcolour.cColourDir = "down" THEN "up" ELSE "down"
         icColour              = ttBGcolour.cColour
         .
        
/*  All is in the background container...
    ===================================*/
h_BackGround = ihFrame:BACKGROUND.           
/*  ==============================================*/
   
/* New from here: */
iOrgEntries = COLOR-TABLE:NUM-ENTRIES.

IF COLOR-TABLE:NUM-ENTRIES <> 256 THEN 
                        COLOR-TABLE:NUM-ENTRIES = 256.

IF icColour = "" THEN icColour = "normal".
        
IF icColour NE "normal" THEN DO:

  CASE icColour :
       WHEN "BLUE" THEN
              Assign   Red-Value     = 0
                       Green-Value   = 0
                       Blue-Value    = 0
                       AddRED        = 0
                       AddGREEN      = 0
                       AddBLUE       = GapColor
                       iFirst        = 16
                       iLast         = 55
                       .
      WHEN "RED" THEN
              ASSIGN  Red-Value      = 10
                      Green-Value    = 0
                      Blue-Value     = 0
                      AddRED         = GapColor
                      AddGREEN       = 0
                      AddBLUE        = 0
                      iFirst         = 56
                      iLast          = 95
                      .
      WHEN "GRAY" THEN
              ASSIGN  Red-Value      = 0
                      Green-Value    = 0
                      Blue-Value     = 0
                      AddRED         = GapColor
                      AddGREEN       = GapColor
                      AddBLUE        = GapColor
                      iFirst         = 96
                      iLast          = 135
                      .
      WHEN "GREEN" THEN
              ASSIGN  Red-Value     = 0
                      Green-Value   = 0
                      Blue-Value    = 0
                      AddRED        = 0
                      AddGREEN      = GapColor
                      AddBLUE       = 0
                      iFirst        = 136
                      iLast         = 175
                      .
      WHEN "PURPLE" THEN
              ASSIGN  Red-Value     = 0
                      Green-Value   = 0
                      Blue-Value    = 0
                      AddRED        = 0
                      AddGREEN      = GapColor
                      AddBLUE       = GapColor
                      iFirst        = 176
                      iLast         = 215
                      .   
      WHEN "YELLOW" THEN
              ASSIGN  Red-Value     = 0
                      Green-Value   = 0
                      Blue-Value    = 0
                      AddRED        = GapColor
                      AddGREEN      = GapColor
                      AddBLUE       = 0
                      iFirst        = 216
                      iLast         = 255
                      .                                
  END CASE.                                                
                  
  IF COLOR-TABLE:GET-RED-VALUE(   iLast - 1) <> RED-VALUE   + ( iLast - iFirst - 1) * AddRED
     OR COLOR-TABLE:GET-GREEN-VALUE( iLast - 1) <> GREEN-VALUE + ( iLast - iFirst - 1) * AddGREEN
     OR COLOR-TABLE:GET-BLUE-VALUE(  iLast - 1) <> BLUE-VALUE  + ( iLast - iFirst - 1) * AddBLUE
     THEN
    DO I-COLOR = iFirst to iLast :
      COLOR-TABLE:SET-DYNAMIC(I-COLOR, yes).
      COLOR-TABLE:Set-red-value(I-COLOR, Red-Value).
      COLOR-TABLE:Set-green-value(I-COLOR, Green-Value).
      COLOR-TABLE:Set-blue-value(I-COLOR,  Blue-Value).
      ASSIGN Red-Value   = Red-Value     + AddRED
             Green-Value = Green-Value   + AddGREEN
             Blue-Value  = Blue-Value    + AddBLUE.
    END.
  
  
  /*      Calculate the width of rectangles
          -------------------------------*/  
  Height_Rectangle_Pixels = ihFrame:HEIGHT-PIXELS   /  (iLast - iFirst ) /* * 1.5  */.
   
/*      # of rectangles to draw :
        =======================*/
   
  HEIGHT_FRAME = ihFrame:HEIGHT-pixels - ihFrame:BORDER-TOP-PIXELS
                                       - ihFrame:BORDER-BOTTOM-PIXELS.
                                         
  N_Rectangles =  TRUNCATE( HEIGHT_FRAME / Height_Rectangle_Pixels, 0). 

  IF HEIGHT_FRAME MOD Height_Rectangle_Pixels > 0 THEN
    N_Rectangles = N_Rectangles + 1.

/*   N_Rectangles = MIN(N_Rectangles,40). */
                        
   /*           Gradient already exists ?
                =======================*/                
  h_Field = h_BackGround:FIRST-CHILD.
 
  I-Color = 1.
   
  IF NOT valid-Handle(h_Field)
     OR h_Field:Type <> "RECTANGLE"
     OR h_Field:BGCOLOR < iFirst 
    THEN DO I-color = 1 TO n_Rectangles:

    CREATE RECTANGLE h_Field
           ASSIGN Y                = (I-COLOR - 1) * Height_Rectangle_Pixels
                  X                = 0
                  WIDTH-PIXELS     = ihFrame:Width-pixels 
                                     - ihFrame:BORDER-RIGHT-PIXELS
                                     - ihFrame:BORDER-LEFT-PIXELS
                  HEIGHT-PIXELS    = MIN( Height_Rectangle_Pixels,
                                          Height_FRAME - h_Field:Y
                                        )
                  FILLED           = TRUE 
                  BGCOLOR          = (IF i-color > 40 THEN 
                                        iPrevBGcol 
                                      ELSE IF ttBGcolour.cColourDir = "down" THEN 
                                        n_Rectangles + iFirst + 40 - n_Rectangles - I-COLOR 
                                      ELSE 
                                        MAX(iFirst,I-COLOR + iFirst - 1 + 40 - n_Rectangles))
                  EDGE-PIXELS      = 0 
                  PARENT           = h_BackGround
                  VISIBLE          = TRUE.     
    iPrevBGcol = h_Field:BGCOLOR.

    FOR EACH ttAllFrames 
        WHERE ttAllFrames.hWindow = hCurrWindow
          AND VALID-HANDLE(ttAllFrames.hFrame):
      IF h_field:Y GE getAbsPosition(ttAllFrames.hFrame,"Y")
         AND h_field:Y LE getAbsPosition(ttAllFrames.hFrame,"Y") + ttAllFrames.hFrame:HEIGHT-PIXELS - 1
         THEN DO:
        CREATE RECTANGLE hFieldSpX
          ASSIGN Y                = IF NOT ttAllFrames.bProcessed THEN 0 
                                    ELSE h_field:Y - (getAbsPosition(ttAllFrames.hFrame,"Y") - ihFrame:Y) 
                 X                = 0
                 WIDTH-PIXELS     = ttAllFrames.hFrame:WIDTH-PIXELS
                                      - ttAllFrames.hFrame:BORDER-RIGHT-PIXELS
                                      - ttAllFrames.hFrame:BORDER-LEFT-PIXELS
                 HEIGHT-PIXELS    = IF NOT ttAllFrames.bProcessed THEN 
                                      h_field:Y - (getAbsPosition(ttAllFrames.hFrame,"Y") - ihFrame:Y) + h_Field:HEIGHT-PIXELS
                                    ELSE IF getAbsPosition(ttAllFrames.hFrame,"Y") + ttAllFrames.hFrame:HEIGHT-PIXELS - h_field:Y < h_field:HEIGHT-PIXELS THEN
                                      MAX(1,ttAllFrames.hFrame:HEIGHT-PIXELS + getAbsPosition(ttAllFrames.hFrame,"Y") - h_field:Y - ttAllFrames.hFrame:BORDER-BOTTOM-PIXELS - ttAllFrames.hFrame:BORDER-TOP-PIXELS - 2)
                                    ELSE h_Field:HEIGHT-PIXELS                                      
                 FILLED           = TRUE 
                 BGCOLOR          = h_Field:BGCOLOR
                 EDGE-PIXELS      = 0 
                 PARENT           = ttAllFrames.hFrame:BACKGROUND
                 VISIBLE          = TRUE.     

        ttAllFrames.bProcessed = TRUE.
      END.
    END.
  END.  
END.  
/*      Set to 'transparent' background all litterals, radio, toggle, slider or text widgets
        ====================================================================================
                    Scanning is made on every object with an undefined (?) background color.
                    I will find the color of the most accurate rectangle in the background, 
                    then set the background color of the object to this value.
                                        ( My personal copyright ! )
                    If the foreground object color is undefined, setting of white color
                    for the foreground too.
        */
      
/* DoViewAllRectangles(ihFrame).  */

h_Group = ihFrame:FIRST-CHILD.
DO WHILE VALID-HANDLE(h_Group) :
  h_Field =  h_Group:FIRST-CHILD.
  DO WHILE VALID-HANDLE(h_Field) :
    IF CAN-DO("LITERAL,RADIO-SET,TOGGLE-BOX,SLIDER,TEXT", h_Field:TYPE)
       THEN DO:
      IF icColour NE "normal" THEN DO:
        /* What's the middle of object ?
           ===========================*/

        IF ttBGcolour.cColourDir = "down" THEN
          I-COLOR = ihFrame:HEIGHT-PIXELS - h_Field:Y + h_Field:HEIGHT-PIXELS / 2 + iFirst.    /* Middle of object */
        ELSE
          I-COLOR = h_Field:Y + h_Field:HEIGHT-PIXELS / 2 + iFirst.    /* Middle of object */
        /*  What's the best color for this object ?
            -------------------------------------*/
        h_Field:BGCOLOR = MIN(iLast,iFirst + ( (I-COLOR - iFirst) / Height_Rectangle_Pixels)).
        IF h_Field:FGCOLOR = ? THEN
          h_Field:FGCOLOR = 15.                       /* set to white foreground */
      END.
      ELSE ASSIGN h_Field:BGCOLOR = ?
                  h_field:FGCOLOR = ?.
    END.
    h_Field = h_Field:NEXT-SIBLING.
  END.
  h_Group = h_Group:NEXT-SIBLING.
END.                

FOR EACH ttAllFrames 
    WHERE ttAllFrames.hWindow = hCurrWindow
      AND VALID-HANDLE(ttAllFrames.hFrame):
  h_Group = ttAllFrames.hFrame:FIRST-CHILD.
  DO WHILE VALID-HANDLE(h_Group) :
    h_Field =  h_Group:FIRST-CHILD.
    DO WHILE VALID-HANDLE(h_Field) :
      IF CAN-DO("LITERAL,RADIO-SET,TOGGLE-BOX,SLIDER,TEXT", h_Field:TYPE)
         THEN DO:
        IF icColour NE "normal" THEN DO:
          /* What's the middle of object ?
             ===========================*/

          IF ttBGcolour.cColourDir = "down" THEN
            I-COLOR = ihFrame:HEIGHT-PIXELS - getAbsPosition(h_field,"Y") + h_Field:HEIGHT-PIXELS / 2 + iFirst.    /* Middle of object */
          ELSE
            I-COLOR = getAbsPosition(h_field,"Y") + h_Field:HEIGHT-PIXELS / 2 + iFirst.    /* Middle of object */
          /*  What's the best color for this object ?
              -------------------------------------*/
          h_Field:BGCOLOR = MIN(iLast,iFirst + ( (I-COLOR - iFirst) / Height_Rectangle_Pixels)).
          IF h_Field:FGCOLOR = ? THEN
            h_Field:FGCOLOR = 15.                       /* set to white foreground */
        END.
        ELSE ASSIGN h_Field:BGCOLOR = ?
                    h_field:FGCOLOR = ?.
      END.
      h_Field = h_Field:NEXT-SIBLING.
    END.
    h_Group = h_Group:NEXT-SIBLING.
  END.                
END.

FOR EACH ttAllFrames:
  ttAllFrames.bProcessed = FALSE.
END.

IF icColour = "normal" THEN
  COLOR-TABLE:NUM-ENTRIES = iOrgEntries.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCleanUpResize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCleanUpResize Procedure 
FUNCTION setCleanUpResize RETURNS LOGICAL
  ( INPUT hWindow AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Delete temp-table entries for window when it is closed 
    Usage: ON CLOSE OF THIS-PROCEDURE DO:
             DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
             RUN disable_UI.
           END.
    Notes: Not required. Sole purpose to minimize size of temp-tables
------------------------------------------------------------------------------*/
IF bGlobAutoSaveSettings THEN
  RUN DoSaveWinSettings (hWindow).

FOR EACH ttOrgWinSize
    WHERE ttOrgWinSize.chWindow = STRING(hWindow):
  DELETE ttOrgWinSize.
END.
FOR EACH ttCurrWinSize
    WHERE ttCurrWinSize.chWindow = STRING(hWindow):
  DELETE ttCurrWinSize.
END.
FOR EACH ttNoResizeX
    WHERE ttNoResizeX.chWindow = STRING(hWindow):
  DELETE ttNoResizeX.
END.
FOR EACH ttNoResizeY
    WHERE ttNoResizeY.chWindow = STRING(hWindow):
  DELETE ttNoResizeY.
END.
FOR EACH ttAddResizeX
    WHERE ttAddResizeX.chWindow = STRING(hWindow):
  DELETE ttAddResizeX.
END.
FOR EACH ttAddResizeY
    WHERE ttAddResizeY.chWindow = STRING(hWindow):
  DELETE ttAddResizeY.
END.
FOR EACH ttNoMoveX
    WHERE ttNoMoveX.chWindow = STRING(hWindow):
  DELETE ttNoMoveX.
END.
FOR EACH ttNoMoveY
    WHERE ttNoMoveY.chWindow = STRING(hWindow):
  DELETE ttNoMoveY.
END.
FOR EACH ttAddMoveX
    WHERE ttAddMoveX.chWindow = STRING(hWindow):
  DELETE ttAddMoveX.
END.
FOR EACH ttAddMoveY
    WHERE ttAddMoveY.chWindow = STRING(hWindow):
  DELETE ttAddMoveY.
END.

FOR EACH ttResizeTypes
    WHERE ttResizeTypes.chWindow = STRING(hWindow):
  DELETE ttResizeTypes.
END.

FOR EACH ttSplitBarX
    WHERE ttSplitBarX.chWindow = STRING(hWindow):
  DELETE ttSplitBarX.
END.
FOR EACH ttSplitBarY
    WHERE ttSplitBarY.chWindow = STRING(hWindow):
  DELETE ttSplitBarY.
END.
FOR EACH ttFollowSplitBarX
    WHERE ttFollowSplitBarX.chWindow = STRING(hWindow):
  DELETE ttFollowSplitBarX.
END.
FOR EACH ttFollowSplitBarY
    WHERE ttFollowSplitBarY.chWindow = STRING(hWindow):
  DELETE ttFollowSplitBarY.
END.
FOR EACH ttPrevSplitBarXpos
    WHERE ttPrevSplitBarXpos.chWindow = STRING(hWindow):
  DELETE ttPrevSplitBarXpos.
END.
FOR EACH ttPrevSplitBarYpos
    WHERE ttPrevSplitBarYpos.chWindow = STRING(hWindow):
  DELETE ttPrevSplitBarYpos.
END.

FOR EACH ttSettingsLoaded
    WHERE ttSettingsLoaded.hWindow = hWindow:
  DELETE ttSettingsLoaded.
END.
FOR EACH ttBGcolour
    WHERE ttBGcolour.hFrame = hWindow:FIRST-CHILD:
  DELETE ttBGcolour.
END.
FOR EACH ttLastInitFunction
    WHERE ttLastInitFunction.hWindow = hWindow:
  DELETE ttLastInitFunction.
END.
FOR EACH ttAllFrames
    WHERE ttAllFrames.hWindow = hWindow:
  DELETE ttAllFrames.
END.
FOR EACH ttMenuStruct
    WHERE ttMenuStruct.hWindow = hWindow:
  DELETE ttMenuStruct.
END.
FOR EACH ttResizeXgroup
    WHERE ttResizeXgroup.hWindow = hWindow:
  DELETE ttResizeXgroup.
END.
FOR EACH ttResizeYgroup
    WHERE ttResizeYgroup.hWindow = hWindow:
  DELETE ttResizeYgroup.
END.
FOR EACH ttMoveXgroup
    WHERE ttMoveXgroup.hWindow = hWindow:
  DELETE ttMoveXgroup.
END.
FOR EACH ttMoveYgroup
    WHERE ttMoveYgroup.hWindow = hWindow:
  DELETE ttMoveYgroup.
END.
FOR EACH ttADMFolderRect
    WHERE ttADMFolderRect.hWindow = hWindow:
  DELETE ttADMFolderRect.
END.
FOR EACH ttTransMarker
    WHERE ttTransMarker.hWindow = hWindow:
  DELETE ttTransMarker.
END.
FOR EACH ttNeverResizeX
    WHERE ttNeverResizeX.hWindow = hWindow:
  DELETE ttNeverResizeX.
END.
FOR EACH ttNeverResizeY
    WHERE ttNeverResizeY.hWindow = hWindow:
  DELETE ttNeverResizeY.
END.
FOR EACH ttOrgWidgetPos
    WHERE ttOrgWidgetPos.hWindow = hWindow:
  DELETE ttOrgWidgetPos.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCleanUpSplitBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCleanUpSplitBar Procedure 
FUNCTION setCleanUpSplitBar RETURNS LOGICAL
  ( INPUT hWindow AS HANDLE,
    INPUT ihSplitBar AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Delete temp-table entries for splitbar 
    Usage: ON CLOSE OF THIS-PROCEDURE DO:
             DYNAMIC-FUNCTION("setCleanUpSplitBar",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBar:HANDLE).
             RUN disable_UI.
           END.
    Notes: Used on close of a suppressed window containig a splitbar
------------------------------------------------------------------------------*/
FOR EACH ttSplitBarX
    WHERE ttSplitBarX.chWindow = STRING(hWindow)
      AND ttSplitBarX.hSplitBar = ihSplitBar:

  FIND FIRST ttPrevSplitBarXpos 
       WHERE ttPrevSplitBarXpos.chWindow = STRING(hWindow)
         AND ttPrevSplitBarXpos.cFrameName = ttSplitBarX.hSplitFrame:NAME
       NO-ERROR.
  IF NOT AVAIL ttPrevSplitBarXpos THEN DO:
    CREATE ttPrevSplitBarXpos.
    ASSIGN ttPrevSplitBarXpos.chWindow     = STRING(hWindow)
           ttPrevSplitBarXpos.cFrameName = ttSplitBarX.hSplitFrame:NAME.
  END.
  ttPrevSplitBarXpos.iPrevXpos = ttSplitBarX.hSplitBar:X.
  DELETE ttSplitBarX.
END.
FOR EACH ttSplitBarY
    WHERE ttSplitBarY.chWindow = STRING(hWindow)
      AND ttSplitBarY.hSplitBar = ihSplitBar:

  FIND FIRST ttPrevSplitBarYpos 
       WHERE ttPrevSplitBarYpos.chWindow = STRING(hWindow)
         AND ttPrevSplitBarYpos.cFrameName = ttSplitBarY.hSplitFrame:NAME
       NO-ERROR.
  IF NOT AVAIL ttPrevSplitBarYpos THEN DO:
    CREATE ttPrevSplitBarYpos.
    ASSIGN ttPrevSplitBarYpos.chWindow     = STRING(hWindow)
           ttPrevSplitBarYpos.cFrameName = ttSplitBarY.hSplitFrame:NAME.
  END.
  ttPrevSplitBarYpos.iPrevYpos = ttSplitBarY.hSplitBar:Y.
  DELETE ttSplitBarY.
END.
FOR EACH ttFollowSplitBarX
    WHERE ttFollowSplitBarX.chWindow = STRING(hWindow)
      AND ttSplitBarX.hSplitBar = ihSplitBar:
  DELETE ttFollowSplitBarX.
END.
FOR EACH ttFollowSplitBarY
    WHERE ttFollowSplitBarY.chWindow = STRING(hWindow)
      AND ttSplitBarY.hSplitBar = ihSplitBar:
  DELETE ttFollowSplitBarY.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCurrWidthHeight) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCurrWidthHeight Procedure 
FUNCTION setCurrWidthHeight RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE,
    INPUT iiWidth  AS INT,
    INPUT iiHeight AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttCurrWinSize 
     WHERE ttCurrWinSize.chWindow = STRING(ihWindow)
     NO-ERROR.
IF AVAIL ttCurrWinSize THEN DO:
  ASSIGN ttCurrWinSize.iWidthPixels  = iiWidth
         ttCurrWinSize.iHeightPixels = iiHeight.
  RETURN YES.
END.
  
RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCustomWinSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCustomWinSettings Procedure 
FUNCTION setCustomWinSettings RETURNS LOGICAL
  ( INPUT ihWindow  AS HANDLE,
    INPUT icSetting AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Enable custom settings for window (sorting, search-value..) 
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttSettingsLoaded
     WHERE ttSettingsLoaded.hWindow = ihWindow
     NO-ERROR.
IF AVAIL ttSettingsLoaded THEN DO:
  ttSettingsLoaded.cCustomSetting = icSetting.
  RETURN TRUE.   
END.
ELSE RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDebugResize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDebugResize Procedure 
FUNCTION setDebugResize RETURNS LOGICAL
  ( INPUT ibDebug AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

bDebug = ibDebug.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDeltaX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDeltaX Procedure 
FUNCTION setDeltaX RETURNS LOGICAL
  ( INPUT iiDeltaX AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
iDeltaX = iiDeltaX.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDeltaY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDeltaY Procedure 
FUNCTION setDeltaY RETURNS LOGICAL
  ( INPUT iiDeltaY AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
iDeltaY = iiDeltaY.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEnableColor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEnableColor Procedure 
FUNCTION setEnableColor RETURNS LOGICAL
  ( INPUT ibGlobEnableColour AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bGlobEnableColour = ibGlobEnableColour.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFollowPlaceHolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFollowPlaceHolder Procedure 
FUNCTION setFollowPlaceHolder RETURNS LOGICAL
  ( INPUT ihWindow      AS HANDLE,
    INPUT ihObject      AS HANDLE,
    INPUT ihPlaceHolder AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: If the placeholder already has resize-rules make sure that the object gets the same 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR chWindow    AS CHAR   NO-UNDO.
DEF VAR chFrame     AS CHAR   NO-UNDO.
DEF VAR cWidget     AS CHAR   NO-UNDO.
DEF VAR hRuleBuffer AS HANDLE NO-UNDO.

ASSIGN chWindow = STRING(ihWindow)
       chFrame  = STRING(ihPlaceHolder:FRAME)
       cWidget  = ihPlaceHolder:NAME 
       NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  MESSAGE "ERROR in setFollowPlaceHolder." SKIP
          "Window handle: " chWindow SKIP
          "Frame handle: " chFrame   SKIP
          "Placeholder: " cWidget    SKIP
          VIEW-AS ALERT-BOX ERROR.
  RETURN NO.
END.

IF CAN-FIND(FIRST ttNoResizeX
            WHERE ttNoResizeX.chWindow = chWindow
              AND ttNoResizeX.chFrame  = chFrame
              AND ttNoResizeX.cWidget  = cWidget) THEN DO:
  hRuleBuffer = BUFFER ttNoResizeX:HANDLE.                
  DoAppendRule(hRuleBuffer,ihObject).
END.
IF CAN-FIND(FIRST ttNoResizeY
            WHERE ttNoResizeY.chWindow = chWindow
              AND ttNoResizeY.chFrame  = chFrame
              AND ttNoResizeY.cWidget  = cWidget) THEN DO:
  hRuleBuffer = BUFFER ttNoResizeY:HANDLE.                
  DoAppendRule(hRuleBuffer,ihObject).
END.
IF CAN-FIND(FIRST ttAddResizeX
            WHERE ttAddResizeX.chWindow = chWindow
              AND ttAddResizeX.chFrame  = chFrame
              AND ttAddResizeX.cWidget  = cWidget) THEN DO:
  hRuleBuffer = BUFFER ttAddResizeX:HANDLE.                
  DoAppendRule(hRuleBuffer,ihObject).
END.
IF CAN-FIND(FIRST ttAddResizeY
            WHERE ttAddResizeY.chWindow = chWindow
              AND ttAddResizeY.chFrame  = chFrame
              AND ttAddResizeY.cWidget  = cWidget) THEN DO:
  hRuleBuffer = BUFFER ttAddResizeY:HANDLE.                
  DoAppendRule(hRuleBuffer,ihObject).
END.
IF CAN-FIND(FIRST ttNoMoveX
            WHERE ttNoMoveX.chWindow = chWindow
              AND ttNoMoveX.chFrame  = chFrame
              AND ttNoMoveX.cWidget  = cWidget) THEN DO:
  hRuleBuffer = BUFFER ttNoMoveX:HANDLE.                
  DoAppendRule(hRuleBuffer,ihObject).
END.
IF CAN-FIND(FIRST ttNoMoveY
            WHERE ttNoMoveY.chWindow = chWindow
              AND ttNoMoveY.chFrame  = chFrame
              AND ttNoMoveY.cWidget  = cWidget) THEN DO:
  hRuleBuffer = BUFFER ttNoMoveY:HANDLE.                
  DoAppendRule(hRuleBuffer,ihObject).
END.
IF CAN-FIND(FIRST ttAddMoveX
            WHERE ttAddMoveX.chWindow = chWindow
              AND ttAddMoveX.chFrame  = chFrame
              AND ttAddMoveX.cWidget  = cWidget) THEN DO:
  hRuleBuffer = BUFFER ttAddMoveX:HANDLE.                
  DoAppendRule(hRuleBuffer,ihObject).
END.
IF CAN-FIND(FIRST ttAddMoveY
            WHERE ttAddMoveY.chWindow = chWindow
              AND ttAddMoveY.chFrame  = chFrame
              AND ttAddMoveY.cWidget  = cWidget) THEN DO:
  hRuleBuffer = BUFFER ttAddMoveY:HANDLE.                
  DoAppendRule(hRuleBuffer,ihObject).
END.

FIND FIRST ttResizeXgroup
     WHERE ttResizeXgroup.hWindow = ihWindow
       AND ttResizeXgroup.chFrame = chFrame
       AND ttResizeXgroup.cWidget = cWidget
     NO-ERROR.
IF AVAIL ttResizeXgroup THEN DO:
  CREATE bttResizeXgroup.
  BUFFER-COPY ttResizeXgroup TO bttResizeXgroup
         ASSIGN bttResizeXGroup.cWidget = cWidget.
END.
FIND FIRST ttResizeYgroup
     WHERE ttResizeYgroup.hWindow = ihWindow
       AND ttResizeYgroup.chFrame = chFrame
       AND ttResizeYgroup.cWidget = cWidget
     NO-ERROR.
IF AVAIL ttResizeYgroup THEN DO:
  CREATE bttResizeYgroup.
  BUFFER-COPY ttResizeYgroup TO bttResizeYgroup
         ASSIGN bttResizeYGroup.cWidget = cWidget.
END.
FIND FIRST ttMoveXgroup
     WHERE ttMoveXgroup.hWindow = ihWindow
       AND ttMoveXgroup.chFrame = chFrame
       AND ttMoveXgroup.cWidget = cWidget
     NO-ERROR.
IF AVAIL ttMoveXgroup THEN DO:
  CREATE bttMoveXgroup.
  BUFFER-COPY ttMoveXgroup TO bttMoveXgroup
         ASSIGN bttMoveXGroup.cWidget = cWidget.
END.
FIND FIRST ttMoveYgroup
     WHERE ttMoveYgroup.hWindow = ihWindow
       AND ttMoveYgroup.chFrame = chFrame
       AND ttMoveYgroup.cWidget = cWidget
     NO-ERROR.
IF AVAIL ttMoveYgroup THEN DO:
  CREATE bttMoveYgroup.
  BUFFER-COPY ttMoveYgroup TO bttMoveYgroup
         ASSIGN bttMoveYGroup.cWidget = cWidget.
END.

FIND FIRST ttFollowSplitBarX
     WHERE ttFollowSplitBarX.chWindow = chWindow
       AND ttFollowSplitBarX.hWidget  = ihPlaceHolder
     NO-ERROR.
IF AVAIL ttFollowSplitBarX THEN DO:
  CREATE bttFollowSplitBarX.
  BUFFER-COPY ttFollowSplitBarX TO bttFollowSplitBarX
         ASSIGN bttFollowSplitBarX.hWidget = ihObject.
END.
FIND FIRST ttFollowSplitBarY
     WHERE ttFollowSplitBarY.chWindow = chWindow
       AND ttFollowSplitBarY.hWidget  = ihPlaceHolder
     NO-ERROR.
IF AVAIL ttFollowSplitBarY THEN DO:
  CREATE bttFollowSplitBarY.
  BUFFER-COPY ttFollowSplitBarY TO bttFollowSplitBarY
         ASSIGN bttFollowSplitBarY.hWidget = ihObject.
END.

IF CAN-FIND(FIRST ttNeverResizeX
            WHERE ttNeverResizeX.hWindow = ihWindow
              AND ttNeverResizeX.hWidget = ihPlaceHolder) THEN DO:
  CREATE ttNeverResizeX.
  ASSIGN ttNeverResizeX.hWindow = ihWindow
         ttNeverResizeX.hWidget = ihObject.
END.
IF CAN-FIND(FIRST ttNeverResizeY
            WHERE ttNeverResizeY.hWindow = ihWindow
              AND ttNeverResizeY.hWidget = ihPlaceHolder) THEN DO:
  CREATE ttNeverResizeY.
  ASSIGN ttNeverResizeY.hWindow = ihWindow
         ttNeverResizeY.hWidget = ihObject.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFollowSplitBarX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFollowSplitBarX Procedure 
FUNCTION setFollowSplitBarX RETURNS LOGICAL
  ( INPUT hWindow           AS HANDLE,
    INPUT hSplitBar         AS WIDGET-HANDLE,
    INPUT cWidgetHandleList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Define widgets that should follow movement of a vertical splitbar (X)
    Notes:  The widget list must be stringvalues of handles to the widget
------------------------------------------------------------------------------*/
IF ENTRY(1,cWidgetHandleList) NE "+" THEN
  FOR EACH ttFollowSplitBarX
      WHERE ttFollowSplitBarX.hSplitBar = hSplitBar
        AND ttFollowSplitBarX.chWindow  = STRING(hWindow):
    DELETE ttFollowSplitBarX.
  END.

DO ix = 1 TO NUM-ENTRIES(cWidgetHandleList):
  IF ENTRY(ix,cWidgetHandleList) = "+" THEN NEXT.
  CREATE ttFollowSplitBarX.
  ASSIGN ttFollowSplitBarX.chWindow       = STRING(hWindow)
         ttFollowSplitBarX.hSplitBar      = hSplitBar
         ttFollowSplitBarX.hWidget        = WIDGET-HANDLE(ENTRY(ix,cWidgetHandleList))
         ttFollowSplitBarX.iDeltaSplitBar = ttFollowSplitBarX.hWidget:X - ttFollowSplitBarX.hSplitBar:X
         .
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFollowSplitBarY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFollowSplitBarY Procedure 
FUNCTION setFollowSplitBarY RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hSplitBar   AS WIDGET-HANDLE,
    INPUT cWidgetHandleList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Define widgets that should follow movement of a vertical splitbar (Y)
    Notes:  The widget list must be stringvalues of handles to the widget.
            NB! The widget (frame!) must be set to its original position before this function is run!
------------------------------------------------------------------------------*/
IF ENTRY(1,cWidgetHandleList) NE "+" THEN
  FOR EACH ttFollowSplitBarY
      WHERE ttFollowSplitBarY.hSplitBar = hSplitBar
        AND ttFollowSplitBarY.chWindow  = STRING(hWindow):
    DELETE ttFollowSplitBarY.
  END.

DO ix = 1 TO NUM-ENTRIES(cWidgetHandleList):
  IF ENTRY(ix,cWidgetHandleList) = "+" THEN NEXT.
  CREATE ttFollowSplitBarY.
  ASSIGN ttFollowSplitBarY.chWindow       = STRING(hWindow)
         ttFollowSplitBarY.hSplitBar      = hSplitBar
         ttFollowSplitBarY.hWidget        = WIDGET-HANDLE(ENTRY(ix,cWidgetHandleList))
         ttFollowSplitBarY.iDeltaSplitBar = ttFollowSplitBarY.hWidget:Y - ttFollowSplitBarY.hSplitBar:Y
         .
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKeepResizeWithinFrame) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setKeepResizeWithinFrame Procedure 
FUNCTION setKeepResizeWithinFrame RETURNS LOGICAL
  ( INPUT ibKeepWithinFrame AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bKeepWithinFrame = ibKeepWithinFrame.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLassoExceptionList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLassoExceptionList Procedure 
FUNCTION setLassoExceptionList RETURNS LOGICAL
  ( INPUT icLassoExceptionList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cLassoExceptionList = icLassoExceptionList.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLockWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLockWindow Procedure 
FUNCTION setLockWindow RETURNS LOGICAL
  ( INPUT ihLockWindow AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hLockWindow = ihLockWindow.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLockWindowUpdate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLockWindowUpdate Procedure 
FUNCTION setLockWindowUpdate RETURNS LOGICAL
  ( INPUT bOn     AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose: Lock window from update while resize processes...
    Usage: Use in initializeObject prior to RUN SUPER when adjusting position to current size / current position of splitbar  
    Notes: NB! To use this function is a calculated risk since the window handle is not available before RUN SUPER
           in initializeObject. Assumes here that last manipulated window is the one that should be locked!  
------------------------------------------------------------------------------*/
DEF VAR hProc    AS HANDLE NO-UNDO.
DEF VAR hProcWin AS HANDLE NO-UNDO.

IF bOn THEN DO:
  IF VALID-HANDLE(hLockWindow) AND hLockWindow:HWND NE ? THEN 
    RUN SendMessageW(hLockWindow:HWND, 11, 0, 0, OUTPUT iRetVal).
/*     RUN LockWindowUpdate(hLockWindow:HWND,OUTPUT iRetVal) NO-ERROR. */
  ELSE IF VALID-HANDLE(hCurrWindow) AND hCurrWindow:HWND NE ? THEN
    RUN SendMessageW(hCurrWindow:HWND, 11, 0, 0, OUTPUT iRetVal).
/*     RUN LockWindowUpdate(hCurrWindow:HWND,OUTPUT iRetVal) NO-ERROR. */

END.
ELSE IF hLockWinForResize = ? THEN DO:
  IF VALID-HANDLE(hLockWindow) AND hLockWindow:HWND NE ? THEN DO:
    RUN SendMessageW(hLockWindow:HWND, 11, 1, 0, OUTPUT iRetVal).
    RUN RedrawWindow(hLockWindow:HWND, 0, 0, 4 + 1 + 128, OUTPUT iRetVal).
  END.
  IF hCurrWindow NE  hLockWindow AND VALID-HANDLE(hCurrWindow) AND hCurrWindow:HWND NE ? THEN DO:
    RUN SendMessageW(hCurrWindow:HWND, 11, 1, 0, OUTPUT iRetVal).
    RUN RedrawWindow(hCurrWindow:HWND, 0, 0, 4 + 1 + 128, OUTPUT iRetVal).
  END.
  IF hRepaintWindow NE hCurrWindow AND hRepaintWindow NE hLockWindow AND VALID-HANDLE(hRepaintWindow) AND hRepaintWindow:HWND NE ? THEN DO:
    RUN SendMessageW(hRepaintWindow:HWND, 11, 1, 0, OUTPUT iRetVal).
    RUN RedrawWindow(hRepaintWindow:HWND, 0, 0, 4 + 1 + 128, OUTPUT iRetVal).
  END.
     
  hProc = SESSION:FIRST-PROCEDURE.
  REPEAT WHILE VALID-HANDLE(hProc):
    hProcWin = hProc:CURRENT-WINDOW.
    IF VALID-HANDLE(hProcWin) AND hProcWin:HWND NE ? 
       AND hProcWin NE hLockWindow AND hProcWin NE hCurrWindow AND hProcWin NE hRepaintWindow THEN DO:
      RUN SendMessageW(hProcWin:HWND, 11, 1, 0, OUTPUT iRetVal).
      RUN RedrawWindow(hProcWin:HWND, 0, 0, 4 + 1 + 128, OUTPUT iRetVal).
    END.
    hProc = hProc:NEXT-SIBLING.
  END.

/*   RUN LockWindowUpdate(0,OUTPUT iRetVal).  */
END.
  
hLockWindow = ?.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setManualResizeSettngsMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setManualResizeSettngsMenu Procedure 
FUNCTION setManualResizeSettngsMenu RETURNS LOGICAL
  ( INPUT ibManResSettingsMenu AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bManResSettingsMenu = ibManResSettingsMenu.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMinXYmove) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMinXYmove Procedure 
FUNCTION setMinXYmove RETURNS LOGICAL
  ( INPUT iiMinX AS INT,
    INPUT iiMinY AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN iMinXmove = iiMinX
       iMinYmove = iiMinY
       .

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMoveXgroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMoveXgroup Procedure 
FUNCTION setMoveXgroup RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS HANDLE,
    INPUT ifPercent   AS DEC,
    INPUT cWidgetList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate widgets for exeption of general move / resize
    Notes:  
------------------------------------------------------------------------------*/
DO ix = 1 TO NUM-ENTRIES(cWidgetList):
  FIND FIRST ttMoveXgroup
       WHERE ttMoveXgroup.hWindow = hWindow
         AND ttMoveXgroup.chFrame = STRING(hFrame)
         AND ttMoveXgroup.cWidget = ENTRY(ix,cWidgetList)
       NO-ERROR.
  IF NOT AVAIL ttMoveXgroup THEN DO:
    CREATE ttMoveXgroup.
    ASSIGN ttMoveXgroup.cWidget = ENTRY(ix,cWidgetList)
           ttMoveXgroup.chFrame = STRING(hFrame)
           ttMoveXgroup.hWindow = hWindow.
  END.
  ttMoveXgroup.fPercent = ifPercent.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMoveYgroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMoveYgroup Procedure 
FUNCTION setMoveYgroup RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS HANDLE,
    INPUT ifPercent   AS DEC,
    INPUT cWidgetList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate widgets for exeption of general move / resize
    Notes:  
------------------------------------------------------------------------------*/
DO ix = 1 TO NUM-ENTRIES(cWidgetList):
  FIND FIRST ttMoveYgroup
       WHERE ttMoveYgroup.hWindow = hWindow
         AND ttMoveYgroup.chFrame = STRING(hFrame)
         AND ttMoveYgroup.cWidget = ENTRY(ix,cWidgetList)
       NO-ERROR.
  IF NOT AVAIL ttMoveYgroup THEN DO:
    CREATE ttMoveYgroup.
    ASSIGN ttMoveYgroup.cWidget = ENTRY(ix,cWidgetList)
           ttMoveYgroup.chFrame = STRING(hFrame)
           ttMoveYgroup.hWindow = hWindow.
  END.
  ttMoveYgroup.fPercent = ifPercent.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMyEnableSaveSettingsMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMyEnableSaveSettingsMenu Procedure 
FUNCTION setMyEnableSaveSettingsMenu RETURNS LOGICAL
  ( INPUT ibMyEnableSave AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bMyEnableSave = ibMyEnableSave.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNewMinXYmove) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNewMinXYmove Procedure 
FUNCTION setNewMinXYmove RETURNS LOGICAL
  ( INPUT hWidget AS WIDGET-HANDLE,
    INPUT iiMinXmove     AS INT,
    INPUT iiMinYmove     AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  Set new values for min X and min Y coordinates
    Notes:  Handle with care 
------------------------------------------------------------------------------*/
FIND FIRST ttOrgWinSize 
     WHERE ttOrgWinSize.chWindow = STRING(hWidget)
     NO-ERROR.
IF AVAIL ttOrgWinSize THEN
  ASSIGN ttOrgWinSize.iMinXmove     = iiMinXmove
         ttOrgWinSize.iMinYmove     = iiMinYmove.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNoMoveX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNoMoveX Procedure 
FUNCTION setNoMoveX RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate widgets for exeption of general move / resize
    Notes:  
------------------------------------------------------------------------------*/
IF ENTRY(1,cWidgetList) NE "+" THEN
  FOR EACH ttNoMoveX
      WHERE ttNoMoveX.chWindow = STRING(hWindow)
        AND ttNoMoveX.chFrame  = STRING(hFrame):
    DELETE ttNoMoveX.
  END.

DO ix = 1 TO NUM-ENTRIES(cWidgetList):
  IF ENTRY(ix,cWidgetList) = "+" THEN NEXT.
  CREATE ttNoMoveX.
  ASSIGN ttNoMoveX.chFrame  = STRING(hFrame)
         ttNoMoveX.chWindow = STRING(hWindow)
         ttNoMoveX.cWidget  = ENTRY(ix,cWidgetList).
END.

IF bOverLoadSettings THEN DoLoadSettings (hWindow).

IF bDebug THEN PUBLISH "DebugResize" (SOURCE-PROCEDURE).

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNoMoveY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNoMoveY Procedure 
FUNCTION setNoMoveY RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate widgets for exeption of general move / resize
    Notes:  
------------------------------------------------------------------------------*/
IF ENTRY(1,cWidgetList) NE "+" THEN
  FOR EACH ttNoMoveY
      WHERE ttNoMoveY.chWindow = STRING(hWindow)
        AND ttNoMoveY.chFrame  = STRING(hFrame):
    DELETE ttNoMoveY.
  END.

DO ix = 1 TO NUM-ENTRIES(cWidgetList):
  IF ENTRY(ix,cWidgetList) = "+" THEN NEXT.
  CREATE ttNoMoveY.
  ASSIGN ttNoMoveY.chFrame  = STRING(hFrame)
         ttNoMoveY.chWindow = STRING(hWindow)
         ttNoMoveY.cWidget  = ENTRY(ix,cWidgetList).
END.

IF bOverLoadSettings THEN DoLoadSettings (hWindow).

IF bDebug THEN PUBLISH "DebugResize" (SOURCE-PROCEDURE).

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNoResizeX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNoResizeX Procedure 
FUNCTION setNoResizeX RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate widgets for exeption of general move / resize
    Notes:  
------------------------------------------------------------------------------*/
IF ENTRY(1,cWidgetList) NE "+" THEN
  FOR EACH ttNoResizeX
      WHERE ttNoResizeX.chWindow = STRING(hWindow)
        AND ttNoResizeX.chFrame  = STRING(hFrame):
    DELETE ttNoResizeX.
  END.

DO ix = 1 TO NUM-ENTRIES(cWidgetList):
  IF ENTRY(ix,cWidgetList) = "+" THEN NEXT.
  CREATE ttNoResizeX.
  ASSIGN ttNoResizeX.chFrame  = STRING(hFrame)
         ttNoResizeX.chWindow = STRING(hWindow)
         ttNoResizeX.cWidget  = ENTRY(ix,cWidgetList).
END.

IF bOverLoadSettings THEN DoLoadSettings (hWindow).

IF bDebug THEN PUBLISH "DebugResize" (SOURCE-PROCEDURE).

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNoResizeY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNoResizeY Procedure 
FUNCTION setNoResizeY RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS WIDGET-HANDLE,
    INPUT cWidgetList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate widgets for exeption of general move / resize
    Notes:  
------------------------------------------------------------------------------*/
IF ENTRY(1,cWidgetList) NE "+" THEN
  FOR EACH ttNoResizeY
      WHERE ttNoResizeY.chWindow = STRING(hWindow)
        AND ttNoResizeY.chFrame  = STRING(hFrame):
    DELETE ttNoResizeY.
  END.
DO ix = 1 TO NUM-ENTRIES(cWidgetList):
  IF ENTRY(ix,cWidgetList) = "+" THEN NEXT.
  CREATE ttNoResizeY.
  ASSIGN ttNoResizeY.chFrame  = STRING(hFrame)
         ttNoResizeY.chWindow = STRING(hWindow)
         ttNoResizeY.cWidget  = ENTRY(ix,cWidgetList).
END.

IF bOverLoadSettings THEN DoLoadSettings (hWindow).

IF bDebug THEN PUBLISH "DebugResize" (SOURCE-PROCEDURE).

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOneTimeException) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setOneTimeException Procedure 
FUNCTION setOneTimeException RETURNS LOGICAL
  ( INPUT ihWidget AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(ihWidget) THEN RETURN NO.

FIND FIRST ttOneTimeException
     WHERE ttOneTimeException.hWidget = ihWidget
     NO-ERROR.
IF NOT AVAIL ttOneTimeException THEN DO:
  CREATE ttOneTimeException.
  ttOneTimeException.hWidget = ihWidget.
END.
ELSE DELETE ttOneTimeException.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOrgWidthHeight) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setOrgWidthHeight Procedure 
FUNCTION setOrgWidthHeight RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE,
    INPUT iiWidth  AS INT,
    INPUT iiHeight AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttOrgWinSize 
     WHERE ttOrgWinSize.chWindow = STRING(ihWindow)
     NO-ERROR.
IF AVAIL ttOrgWinSize THEN DO:
  ASSIGN ttOrgWinSize.iWidthPixels  = iiWidth
         ttOrgWinSize.iHeightPixels = iiHeight.
  RETURN YES.
END.
  
RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOrgWinSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setOrgWinSize Procedure 
FUNCTION setOrgWinSize RETURNS LOGICAL
  ( INPUT hWindow    AS WIDGET-HANDLE,
    INPUT iiMinXmove AS INT,
    INPUT iiMinYmove AS INT,
    INPUT iMinX      AS INT,
    INPUT iMinY      AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  Set window defaults
    Usage:  InitWindow (f.ex):
            DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,400,300,0,0).
    Params: hWindow:    Window handle
            iMinXmove:  All widgets with X (pixels) GE iMinXmove will be moved delta X
            iMinXmove:  All widgets with Y (pixels) GE iMinYmove will be moved delta Y
            iMinX:      Min X (pixels) size allowed for window. 0 is design size
            iMinY:      Min Y (pixels) size allowed for window. 0 is design size
------------------------------------------------------------------------------*/
DEF VAR iOrgWidth     AS INT NO-UNDO.
DEF VAR iOrgHeight    AS INT NO-UNDO.
DEF VAR bNotHidden    AS LOG NO-UNDO.
DEF VAR bEnableColour AS LOG NO-UNDO.
DEF VAR hWidget       AS HANDLE NO-UNDO.

IF NOT SOURCE-PROCEDURE:PERSISTENT THEN
  setRepaintWindow(hWindow).

IF bJBoxUserSetting = ? THEN DO:
  bOk = DYNAMIC-FUNCTION("setUserSetting","","","","","") NO-ERROR.    
  IF ERROR-STATUS:GET-MESSAGE(1) = "" THEN DO:
    cJBoxUserSettingReturnMsg = DYNAMIC-FUNCTION("getTransactionMessage") NO-ERROR.
    IF cJBoxUserSettingReturnMsg = "" THEN
      bJBoxUserSetting = YES.
  END.
END.

FOR EACH ttOrgWinSize
    WHERE ttOrgWinSize.chWindow = STRING(hWindow):
  DELETE ttOrgWinSize.
END.

FOR EACH ttCurrWinSize
    WHERE ttCurrWinSize.chWindow = STRING(hWindow):
  DELETE ttCurrWinSize.
END.

CREATE ttOrgWinSize.
ASSIGN ttOrgWinSize.chWindow      = STRING(hWindow)
       ttOrgWinSize.iWidthPixels  = hWindow:WIDTH-PIXELS
       ttOrgWinSize.iHeightPixels = hWindow:HEIGHT-PIXELS
       ttOrgWinSize.iMinXmove     = iiMinXmove
       ttOrgWinSize.iMinYmove     = iiMinYmove
       ttOrgWinSize.iMinX         = iMinX
       ttOrgWinSize.iMinY         = iMinY
       ttOrgWinSize.hProcedure    = SOURCE-PROCEDURE
       .

CREATE ttCurrWinSize.
ASSIGN ttCurrWinSize.chWindow      = STRING(hWindow)
       ttCurrWinSize.iWidthPixels  = hWindow:WIDTH-PIXELS
       ttCurrWinSize.iHeightPixels = hWindow:HEIGHT-PIXELS
       ttCurrWinSize.hWindow       = hWindow
       ttCurrWinSize.hParentProc   = SOURCE-PROCEDURE
       .

IF NOT hWindow:HIDDEN THEN DO:
  cVisibleList = "".
  FindVisible(hWindow:FIRST-CHILD).
  ASSIGN bNotHidden = TRUE
         hWindow:HIDDEN = TRUE.
END.

ASSIGN iOrgWidth                     = hWindow:WIDTH-PIXELS
       iOrgHeight                    = hWindow:HEIGHT-PIXELS
       hWindow:WIDTH-PIXELS          = SESSION:WIDTH-PIXELS
       hWindow:HEIGHT-PIXELS         = SESSION:HEIGHT-PIXELS 
       hWindow:VIRTUAL-WIDTH-PIXELS  = SESSION:WIDTH-PIXELS
       hWindow:VIRTUAL-HEIGHT-PIXELS = SESSION:HEIGHT-PIXELS 
       hWindow:WIDTH-PIXELS          = iOrgWidth
       hWindow:HEIGHT-PIXELS         = iOrgHeight
       .

IF bNotHidden THEN DO:    
  hWindow:HIDDEN = FALSE.
  cVisibleList = TRIM(cVisibleList,",").
  DO ix = 1 TO NUM-ENTRIES(cVisibleList):
    hWidget = WIDGET-HANDLE(ENTRY(ix,cVisibleList)) NO-ERROR.
    IF VALID-HANDLE(hWidget) THEN
      hWidget:VISIBLE = YES.
  END.
END.

SetScrollableFrames (hWindow).

IF NOT bCustomWinSetting AND NOT bManResSettingsMenu THEN DO:
  IF bGlobEnableColour THEN DO:
    ASSIGN hWidget = hWindow:FIRST-CHILD
           bEnableColour = TRUE.
    IF VALID-HANDLE(hWidget:NEXT-SIBLING) THEN
      bEnableColour = FALSE.
    ELSE IF VALID-HANDLE(hWidget:FIRST-CHILD) THEN DO: 
      hWidget = hWidget:FIRST-CHILD.
      REPEAT WHILE VALID-HANDLE(hWidget):
        IF hWidget:TYPE = "frame" THEN DO:
          bEnableColour = FALSE.
          LEAVE.
        END.
        ELSE hWidget = hWidget:NEXT-SIBLING.
      END.
    END.
  END.
  DoSetWinMenu(hWindow,
               bGlobEnableCommers,
               bEnableColour, 
               bGlobEnableHelp,   
               bGlobEnableAbout,  
               IF bMyEnableSave NE ? THEN bMyEnableSave ELSE bGlobEnableSave,
               ""
               ).
  bMyEnableSave = ?.
END.

hCurrWindow = hWindow.
DoLoadSettings (hWindow).

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOverLoadSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setOverLoadSettings Procedure 
FUNCTION setOverLoadSettings RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Backwords compatibility. Forces to load setting every time a override is set
    Notes:  
------------------------------------------------------------------------------*/
bOverLoadSettings = TRUE.

RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRepaintWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setRepaintWindow Procedure 
FUNCTION setRepaintWindow RETURNS LOGICAL
  ( INPUT ihRepaintWindow AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: To enable repaint of windows belonging to non-persistent procedures 
    Notes: Automatically assigned by setOrgWinSize if source-proc isn't persistent 
------------------------------------------------------------------------------*/
hRepaintWindow = ihRepaintWindow.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetResizeADM2panel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetResizeADM2panel Procedure 
FUNCTION SetResizeADM2panel RETURNS LOGICAL
  ( INPUT ibResizeADM2panel AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bResizeADM2panel = ibResizeADM2panel.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setResizeTypes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setResizeTypes Procedure 
FUNCTION setResizeTypes RETURNS LOGICAL
  ( INPUT ihWidget AS WIDGET-HANDLE,
    INPUT icTypes  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Set widget-types for default resize either for session or window
    Notes:  Session: ihWidget = ?
            Default for ResizeLib: "FRAME,BROWSE,RECTANGLE,EDITOR,SELECTION-LIST,CONTROL-FRAME"
------------------------------------------------------------------------------*/
IF ihWidget NE ? THEN DO:
  FIND FIRST ttResizeTypes 
       WHERE ttResizeTypes.chWindow = STRING(ihWidget)
       NO-ERROR.
  IF AVAIL ttResizeTypes THEN
    ASSIGN ttResizeTypes.cTypes = icTypes.
  ELSE DO:
    CREATE ttResizeTypes.
    ASSIGN ttResizeTypes.chWindow = STRING(ihWidget)
           ttResizeTypes.cTypes   = icTypes.
  END.
END.
ELSE DO:
  FIND FIRST ttResizeTypes 
       WHERE ttResizeTypes.chWindow = ""
       NO-ERROR.
  IF AVAIL ttResizeTypes THEN
    ASSIGN ttResizeTypes.cTypes = icTypes.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setResizeXgroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setResizeXgroup Procedure 
FUNCTION setResizeXgroup RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS HANDLE,
    INPUT ifPercent   AS DEC,
    INPUT cWidgetList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate widgets for exeption of general move / resize
    Notes:  
------------------------------------------------------------------------------*/
DO ix = 1 TO NUM-ENTRIES(cWidgetList):
  FIND FIRST ttResizeXgroup
       WHERE ttResizeXgroup.hWindow = hWindow
         AND ttResizeXgroup.chFrame = STRING(hFrame)
         AND ttResizeXgroup.cWidget = ENTRY(ix,cWidgetList)
       NO-ERROR.
  IF NOT AVAIL ttResizeXgroup THEN DO:
    CREATE ttResizeXgroup.
    ASSIGN ttResizeXgroup.cWidget = ENTRY(ix,cWidgetList)
           ttResizeXgroup.chFrame = STRING(hFrame)
           ttResizeXgroup.hWindow = hWindow.
  END.
  ttResizeXgroup.fPercent = ifPercent.

END.

IF bDebug THEN PUBLISH "DebugResize" (SOURCE-PROCEDURE).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setResizeYgroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setResizeYgroup Procedure 
FUNCTION setResizeYgroup RETURNS LOGICAL
  ( INPUT hWindow     AS HANDLE,
    INPUT hFrame      AS HANDLE,
    INPUT ifPercent   AS DEC,
    INPUT cWidgetList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate widgets for exeption of general move / resize
    Notes:  
------------------------------------------------------------------------------*/
DO ix = 1 TO NUM-ENTRIES(cWidgetList):
  FIND FIRST ttResizeYgroup
       WHERE ttResizeYgroup.hWindow = hWindow
         AND ttResizeYgroup.chFrame = STRING(hFrame)
         AND ttResizeYgroup.cWidget = ENTRY(ix,cWidgetList)
       NO-ERROR.
  IF NOT AVAIL ttResizeYgroup THEN DO:
    CREATE ttResizeYgroup.
    ASSIGN ttResizeYgroup.cWidget = ENTRY(ix,cWidgetList)
           ttResizeYgroup.chFrame = STRING(hFrame)
           ttResizeYgroup.hWindow = hWindow.
  END.
  ttResizeYgroup.fPercent = ifPercent.
END.

IF bDebug THEN PUBLISH "DebugResize" (SOURCE-PROCEDURE).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSaveSettingName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSaveSettingName Procedure 
FUNCTION setSaveSettingName RETURNS LOGICAL
  ( INPUT icSettingsName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Force part of file-name for save settings
    Notes:  Call before any other calls to resizelib in a window
------------------------------------------------------------------------------*/
cSaveSettingsName = icSettingsName.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetScrollableFrames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetScrollableFrames Procedure 
FUNCTION SetScrollableFrames RETURNS LOGICAL
  ( INPUT hWidget AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
REPEAT WHILE hWidget NE ?:

  IF hWidget:TYPE = "FRAME" THEN
    hWidget:SCROLLABLE = TRUE.
  IF CAN-QUERY(hWidget,"frame") AND hWidget:TYPE NE "field-group" AND NOT CAN-FIND(FIRST ttOrgWidgetPos WHERE ttOrgWidgetPos.hWidget = hWidget) THEN DO:
    CREATE ttOrgWidgetPos.
    ASSIGN ttOrgWidgetPos.hWindow     = hWidget:WINDOW
           ttOrgWidgetPos.hFrame      = hWidget:FRAME
           ttOrgWidgetPos.hWidget     = hWidget
           ttOrgWidgetPos.iRow        = hWidget:ROW
           ttOrgWidgetPos.iCol        = hWidget:COL
           ttOrgWidgetPos.iX          = hWidget:X
           ttOrgWidgetPos.iY          = hWidget:Y
           ttOrgWidgetPos.iWidthChars = hWidget:WIDTH-CHARS
           ttOrgWidgetPos.iWidthPix   = hWidget:WIDTH-PIXELS
           ttOrgWidgetPos.iHeightPix  = hWidget:HEIGHT-PIXELS
           NO-ERROR.
  END.

  IF CAN-QUERY(hWidget, 'FIRST-CHILD') AND hWidget:FIRST-CHILD  <> ? THEN
     SetScrollableFrames(hWidget:FIRST-CHILD).

  hWidget = hWidget:NEXT-SIBLING.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSpecialBGcolour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSpecialBGcolour Procedure 
FUNCTION setSpecialBGcolour RETURNS LOGICAL
  ( INPUT ihFrame AS HANDLE,
    INPUT icColour AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bKeepOrgColour = TRUE.
setBGcolour (ihFrame,icColour).
bKeepOrgColour = FALSE.
RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSpesOrgWinSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSpesOrgWinSize Procedure 
FUNCTION setSpesOrgWinSize RETURNS LOGICAL
  ( INPUT hWindow        AS WIDGET-HANDLE,
    INPUT iMinXmove      AS INT,
    INPUT iMinYmove      AS INT,
    INPUT iMinX          AS INT,
    INPUT iMinY          AS INT,
    INPUT bEnableCommers AS LOG,
    INPUT bEnableColour  AS LOG, 
    INPUT bEnableHelp    AS LOG,   
    INPUT bEnableAbout   AS LOG,  
    INPUT bEnableSave    AS LOG
     ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bCustomWinSetting = TRUE.

setOrgWinSize (hWindow,  
               iMinXmove,
               iMinYmove,
               iMinX,    
               iMinY
               ).    

bCustomWinSetting = FALSE.

DoSetWinMenu(hWindow,
             bEnableCommers,
             bEnableColour, 
             bEnableHelp,   
             bEnableAbout,  
             bEnableSave,
             ""
             ).

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSplitBarX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSplitBarX Procedure 
FUNCTION setSplitBarX RETURNS LOGICAL
  ( INPUT hWindow      AS HANDLE,
    INPUT hSplitBar    AS HANDLE,
    INPUT bRestrict    AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate splitbar position and adjust widgets when END-MOVE of splitbar button
    Notes:  bRestrict: Indicate wether to restrict resize of splitbar frame
------------------------------------------------------------------------------*/
DEF VAR hWidget     AS HANDLE NO-UNDO.
DEF VAR hWidget2    AS HANDLE NO-UNDO.
DEF VAR iTmpDeltaX  AS INT NO-UNDO.

ASSIGN hCurrWindow = hWindow
       iDeltaY     = 0.

FIND FIRST ttResizeTypes WHERE ttResizeTypes.chWindow = STRING(hWindow) NO-LOCK NO-ERROR.
IF NOT AVAIL ttResizeTypes THEN
  FIND FIRST ttResizeTypes WHERE ttResizeTypes.chWindow = "" NO-LOCK.

FIND FIRST ttSplitBarX
     WHERE ttSplitBarX.chWindow  = STRING(hWindow) 
       AND ttSplitBarX.hSplitBar = hSplitBar
     NO-ERROR.
IF NOT AVAIL ttSplitBarX THEN DO:
  CREATE ttSplitBarX.
  ASSIGN ttSplitBarX.chWindow        = STRING(hWindow)
         ttSplitBarX.hSplitBar       = hSplitBar
         ttSplitBarX.hSplitFrame     = hSplitBar:FRAME
         ttSplitBarX.hMainFrame      = ttSplitBarX.hSplitFrame:FRAME
         ttSplitBarX.fDistrX         = (ttSplitBarX.hSplitFrame:X + hSplitBar:X) / hWindow:WIDTH-PIXELS
         ttSplitBarX.bRestrictResize = bRestrict
         ttSplitBarX.iPos            = ttSplitBarX.hSplitFrame:X + hSplitBar:X
         ttSplitBarX.iOrgPos         = ttSplitBarX.iPos
         ttSplitBarX.iOrgSplitBarX   = hSplitBar:X
         hSplitBar:WIDTH-PIXELS      = 12    
         iDeltaX                     = 0
         .
  hSplitBar:LOAD-MOUSE-POINTER("Size-W").
  hSplitBar:MOVE-TO-TOP().
  ttSplitBarX.hSplitFrame:MOVE-TO-BOTTOM().
END.
ELSE DO: 
  IF hSplitBar:X < ttSplitBarX.iMinX THEN hSplitBar:X = ttSplitBarX.iMinX.
  ELSE IF hSplitBar:X > (IF bUseCalcValues THEN iCalcWidth ELSE hWindow:WIDTH-PIXELS) - ttSplitBarX.iMinXtoRightEdge THEN 
    hSplitBar:X = (IF bUseCalcValues THEN iCalcWidth ELSE hWindow:WIDTH-PIXELS)  - ttSplitBarX.iMinXtoRightEdge.
 
  iDeltaX = ttSplitBarX.iPos - ttSplitBarX.hSplitFrame:X - hSplitBar:X.
END.

IF iDeltaX NE 0 THEN DO:

  IF bRestrict NE ? AND bReleaseWinLock THEN
    setLockWindowUpdate(TRUE).

  /* Calculate absolute Y position for splitbar. 
     Needed to control that only widgets within Y-scope of splitbar should be pushed: */
  ASSIGN hWidget2    = hSplitBar:PARENT
         iAbsXsplitY = hSplitBar:Y.
  REPEAT WHILE hWidget2:TYPE NE "WINDOW":
    IF hWidget2:TYPE = "FRAME" THEN
      ASSIGN iAbsXsplitY = iAbsXsplitY + hWidget2:Y.
    hWidget2 = hWidget2:PARENT.
  END.
  
  FIND FIRST ttOrgWinSize
       WHERE ttOrgWinSize.chWindow = STRING(hWindow)
       NO-ERROR.
       
  ASSIGN iDeltaY = 0
         cMoveToTopList = "". 
           
  IF iDeltaX > 0 THEN 
    ASSIGN ttSplitBarX.iPos    = ttSplitBarX.hSplitFrame:X + hSplitBar:X
           ttSplitBarX.iOffset = ttSplitBarX.iOffset - iDeltaX
           .

  DoFollowSplitBarX("FRAME","").
  DoFollowSplitBarX("","FRAME").

  cMoveToTopList = cMoveToTopList + STRING(ttSplitBarX.hSplitBar).
  DO ix = 1 TO NUM-ENTRIES(cMoveToTopList):
    hWidget2 = WIDGET-HANDLE(ENTRY(ix,cMoveToTopList)).
    hWidget2:MOVE-TO-TOP() NO-ERROR.
  END.

END.

ASSIGN ttSplitBarX.iPos    = ttSplitBarX.hSplitFrame:X + hSplitBar:X
       ttSplitBarX.iOffset = ttSplitBarX.iPos - ttSplitBarX.iOrgPos
       .

/* Stunt to make sure that virtual height pixels are adjusted: */
IF bRestrict NE ? THEN DO:
  iTmpDeltaX = iDeltaX.
  IF iDeltaX > 0 THEN DO:
    ttSplitBarX.hSplitBar:X = ttSplitBarX.hSplitBar:X + 1 NO-ERROR.
    setSplitBarX (hWindow,hSplitBar,?).
    ttSplitBarX.hSplitBar:X = hSplitBar:X - 1 NO-ERROR.
    setSplitBarX (hWindow,hSplitBar,?).
  END.
  ELSE DO:
    ttSplitBarX.hSplitBar:X = ttSplitBarX.hSplitBar:X - 1 NO-ERROR.
    setSplitBarX (hWindow,hSplitBar,?).
    ttSplitBarX.hSplitBar:X = hSplitBar:X + 1 NO-ERROR.
    setSplitBarX (hWindow,hSplitBar,?).
  END.
  iDeltaX = iTmpDeltaX.
  
  PUBLISH "ResizeTabFrames" (hWindow).
  PUBLISH "ResizeBrowseColumns" (hWindow).

  PUBLISH "ResizeFormComponents" (hWindow).

  FOR EACH ttTransMarker
      WHERE ttTransMarker.hWindow = hWindow:
    IF VALID-HANDLE(ttTransMarker.hMarker) THEN
      ttTransMarker.hMarker:MOVE-TO-TOP().
  END.

  IF bReleaseWinLock THEN setLockWindowUpdate(FALSE).
END.


/* FIND FIRST ttSplitBarY                                                                     */
/*      WHERE ttSplitBarY.chWindow  = STRING(hWindow)                                         */
/*      NO-ERROR.                                                                             */
/* IF AVAIL ttSplitBarY THEN DO:                                                              */
/*   ttSplitBarY.hSplitFrame:MOVE-TO-BOTTOM().                                                */
/*   ttSplitBarY.hSplitBar:WIDTH-PIXELS = 10.                                                 */
/*   ttSplitBarY.hSplitBar:X = 1.                                                             */
/*   ttSplitBarY.hSplitBar:WIDTH-PIXELS = ttSplitBarY.hSplitFrame:WIDTH-PIXELS - 2 NO-ERROR.  */
/*   ttSplitBarY.hSplitBar:MOVE-TO-TOP().                                                     */
/* END.                                                                                       */


RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSplitBarXlimits) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSplitBarXlimits Procedure 
FUNCTION setSplitBarXlimits RETURNS LOGICAL
  ( INPUT hSplitBar        AS HANDLE,
    INPUT iMinX            AS INT,
    INPUT iMinXtoRightEdge AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

FIND ttSplitBarX
     WHERE ttSplitBarX.hSplitBar = hSplitbar NO-ERROR.
IF AVAIL ttSplitBarX THEN DO:
  ASSIGN ttSplitBarX.iMinX            = iMinX
         ttSplitBarX.iMinXtoRightEdge = iMinXtoRightEdge
         .
  RETURN TRUE.
END.

RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSplitBarY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSplitBarY Procedure 
FUNCTION setSplitBarY RETURNS LOGICAL
  ( INPUT hWindow      AS HANDLE,
    INPUT hSplitBar    AS HANDLE,
    INPUT bRestrict    AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  Initiate splitbar position and adjust widgets when END-MOVE of splitbar button
    Notes:  bRestrict: Indicate wether to restrict resize of splitbar frame
------------------------------------------------------------------------------*/
DEF VAR hWidget     AS HANDLE NO-UNDO.
DEF VAR hWidget2    AS HANDLE NO-UNDO.
DEF VAR iTmpDeltaY  AS INT    NO-UNDO.

ASSIGN hCurrWindow = hWindow
       iDeltaX     = 0.

FIND FIRST ttResizeTypes WHERE ttResizeTypes.chWindow = STRING(hWindow) NO-LOCK NO-ERROR.
IF NOT AVAIL ttResizeTypes THEN
  FIND FIRST ttResizeTypes WHERE ttResizeTypes.chWindow = "" NO-LOCK.

FIND FIRST ttSplitBarY
     WHERE ttSplitBarY.chWindow  = STRING(hWindow) 
       AND ttSplitBarY.hSplitBar = hSplitBar
     NO-ERROR.
IF NOT AVAIL ttSplitBarY THEN DO:
  CREATE ttSplitBarY.
  ASSIGN /* hSplitBar:Y                      = hSplitBar:Y + 1 */
         ttSplitBarY.chWindow             = STRING(hWindow)
         ttSplitBarY.hSplitBar            = hSplitBar         
         ttSplitBarY.hSplitFrame          = hSplitBar:FRAME
         ttSplitBarY.hMainFrame           = IF ttSplitBarY.hSplitFrame:FRAME NE ? THEN ttSplitBarY.hSplitFrame:FRAME ELSE hSplitBar:FRAME
         ttSplitBarY.fDistrY              = (ttSplitBarY.hSplitFrame:Y + hSplitBar:Y) / hWindow:WIDTH-PIXELS
         ttSplitBarY.bRestrictResize      = bRestrict
         ttSplitBarY.iPos                 = ttSplitBarY.hSplitFrame:Y + hSplitBar:Y 
         ttSplitBarY.iOrgPos              = ttSplitBarY.iPos
         ttSplitBarY.iOrgSplitBarY        = hSplitBar:Y /* + 1  */
         ttSplitBarY.iOrgSplitWidth       = hSplitBar:WIDTH-PIXELS
         hSplitBar:HEIGHT-PIXELS          = 12
         iDeltaY                          = 0.

  hSplitBar:LOAD-MOUSE-POINTER("Size-S").
  hSplitBar:MOVE-TO-TOP().
  ttSplitBarY.hSplitFrame:MOVE-TO-BOTTOM().

END.
ELSE DO: 

/*   IF hSplitBar:Y < ttSplitBarY.iMinY THEN hSplitBar:Y = ttSplitBarY.iMinY.     */
/*   ELSE IF hSplitBar:Y > hWindow:HEIGHT-PIXELS - ttSplitBarY.iMinYtoBottom THEN */
/*     hSplitBar:Y = hWindow:HEIGHT-PIXELS - ttSplitBarY.iMinYtoBottom.           */

  IF hSplitBar:Y < ttSplitBarY.iMinY THEN hSplitBar:Y = ttSplitBarY.iMinY.
  ELSE IF hSplitBar:Y > ttSplitBarY.hSplitFrame:HEIGHT-PIXELS - ttSplitBarY.iMinYtoBottom THEN
    hSplitBar:Y = ttSplitBarY.hSplitFrame:HEIGHT-PIXELS - ttSplitBarY.iMinYtoBottom.
 
  iDeltaY = ttSplitBarY.iPos - ttSplitBarY.hSplitFrame:Y - hSplitBar:Y NO-ERROR.
END.

IF iDeltaY NE 0 THEN DO:

  IF bRestrict NE ? AND bReleaseWinLock THEN
    setLockWindowUpdate(TRUE).

  /* Calculate absolute X position for splitbar. 
     Needed to control that only widgets within X-scope of splitbar should be pushed: */
  ASSIGN hWidget2    = hSplitBar:PARENT
         iAbsYsplitX = hSplitBar:X.
  REPEAT WHILE hWidget2:TYPE NE "WINDOW":
    IF hWidget2:TYPE = "FRAME" THEN
      ASSIGN iAbsYsplitX = iAbsYsplitX + hWidget2:X.
    hWidget2 = hWidget2:PARENT.
  END.
  FIND FIRST ttOrgWinSize
       WHERE ttOrgWinSize.chWindow = STRING(hWindow)
       NO-ERROR.
       
  ASSIGN iDeltaX        = 0
         cMoveToTopList = "".
           
  IF iDeltaY > 0 THEN 
    ASSIGN ttSplitBarY.iPos    = ttSplitBarY.hSplitFrame:Y + hSplitBar:Y
           ttSplitBarY.iOffset = ttSplitBarY.iOffset - iDeltaY
           .
  DoFollowSplitBarY("FRAME","").
  DoFollowSplitBarY("","FRAME").
   
  cMoveToTopList = cMoveToTopList + STRING(ttSplitBarY.hSplitBar).
  DO ix = 1 TO NUM-ENTRIES(cMoveToTopList):
    hWidget2 = WIDGET-HANDLE(ENTRY(ix,cMoveToTopList)).
    hWidget2:MOVE-TO-TOP() NO-ERROR.
  END.
END.

ASSIGN ttSplitBarY.iPos    = getAbsPosition(hSplitBar,"y") /* ttSplitBarY.hSplitFrame:Y + hSplitBar:Y */
       ttSplitBarY.iOffset = ttSplitBarY.iOffset - iDeltaY
       .

/* Stunt to make sure that virtual height pixels are adjusted + 
   1st time get hold of dependent widgets: */
IF bRestrict NE ? THEN DO:
  iTmpDeltaY = iDeltaY.
  IF iDeltaY > 0 THEN DO:
    ttSplitBarY.hSplitBar:Y = ttSplitBarY.hSplitBar:Y + 1 NO-ERROR.
    setSplitBarY (hWindow,hSplitBar,?).
    ttSplitBarY.hSplitBar:Y = hSplitBar:Y - 1 NO-ERROR.

    setSplitBarY (hWindow,hSplitBar,?).
  END.
  ELSE DO:
    ttSplitBarY.hSplitBar:Y = ttSplitBarY.hSplitBar:Y - 1 NO-ERROR.
    setSplitBarY (hWindow,hSplitBar,?).
    ttSplitBarY.hSplitBar:Y = hSplitBar:Y + 1 NO-ERROR.

    setSplitBarY (hWindow,hSplitBar,?).
  END.
  iDeltaY = iTmpDeltaY.
  
  PUBLISH "ResizeTabFrames" (hWindow).
  PUBLISH "ResizeBrowseColumns" (hWindow).
  
  PUBLISH "ResizeFormComponents" (hWindow).

  FOR EACH ttTransMarker
      WHERE ttTransMarker.hWindow = hWindow:
    IF VALID-HANDLE(ttTransMarker.hMarker) THEN
      ttTransMarker.hMarker:MOVE-TO-TOP().
  END.

  IF bReleaseWinLock THEN setLockWindowUpdate(FALSE).
END.

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSplitBarYlimits) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSplitBarYlimits Procedure 
FUNCTION setSplitBarYlimits RETURNS LOGICAL
  ( INPUT hSplitBar     AS HANDLE,
    INPUT iMinY         AS INT,
    INPUT iMinYtoBottom AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

FIND ttSplitBarY
     WHERE ttSplitBarY.hSplitBar = hSplitbar NO-ERROR.
IF AVAIL ttSplitBarY THEN DO:
  ASSIGN ttSplitBarY.iMinY         = iMinY
         ttSplitBarY.iMinYtoBottom = iMinYtoBottom
         .
  RETURN TRUE.
END.

RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWebDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWebDoc Procedure 
FUNCTION setWebDoc RETURNS CHARACTER
  ( INPUT pcType AS CHAR,
    INPUT pcDoc  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VARIABLE iInstance AS INTEGER    NO-UNDO.
DEFINE VARIABLE cMessage  AS CHARACTER  NO-UNDO.

IF (pcType EQ ?) OR (pcDoc EQ ?) THEN 
  RETURN "Unable to open document".

/* RUN ShellExecuteA (INPUT 0,            */
/*                    INPUT pcType,       */
/*                    INPUT 'iexplore',   */
/*                    INPUT pcDoc,        */
/*                    INPUT '',           */
/*                    INPUT 1,            */
/*                    OUTPUT iInstance).  */
RUN ShellExecuteA (INPUT 0,
                   INPUT pcType,
                   INPUT pcDoc,
                   INPUT '',
                   INPUT '',
                   INPUT 1,
                   OUTPUT iInstance).

IF (iInstance LT 0) OR (iInstance GT 32) THEN RETURN "".
ELSE DO:
  CASE iInstance :
      WHEN  0 THEN cMessage = "The operating system is out of memory or resources.".
      WHEN  2 THEN cMessage = "The specified file was not found".
      WHEN  3 THEN cMessage = "The specified path was not found.".
      WHEN  5 THEN cMessage = "Windows 95 only: The operating system denied "
                              + "access to the specified file".
      WHEN  8 THEN cMessage = "Windows 95 only: There was not enough memory to "
                + "complete the operation.".
      WHEN 10 THEN cMessage = "Wrong Windows version".
      WHEN 11 THEN cMessage = "The .EXE file is invalid (non-Win32.EXE or " + "error in .EXE image).".
      WHEN 12 THEN cMessage = "Application was designed for a different operating system".
      WHEN 13 THEN cMessage = "Application was designed for MS-DOS 4.0".
      WHEN 15 THEN cMessage = "Attempt to load a real-mode program".
      WHEN 16 THEN cMessage = "Attempt to load a second instance of "
                               + "an application with non-readonly data segments".
      WHEN 19 THEN cMessage = "Attempt to load a compressed application file".
      WHEN 20 THEN cMessage = "Dynamic-link library (DLL) file failure".
      WHEN 26 THEN cMessage = "A sharing violation occurred.".
      WHEN 27 THEN cMessage = "The filename association is incomplete or invalid.".
      WHEN 28 THEN cMessage = "The DDE transaction could not be completed "
                               + "because the request timed out.".
      WHEN 29 THEN cMessage = "The DDE transaction failed.".
      WHEN 30 THEN cMessage = "The DDE transaction could not be completed because " + "other DDE transactions were being processed.".
      WHEN 31 THEN cMessage = "There is no application associated with "
                               + "the given filename extension.".
      WHEN 32 THEN cMessage = "Windows 95 only: The specified dynamic-link "
                              + "library was not found.".
      OTHERWISE    cMessage = "undocumented".
  END.
END.
RETURN cMessage.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWebHelpFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWebHelpFile Procedure 
FUNCTION setWebHelpFile RETURNS LOGICAL
  ( INPUT ihWindow      AS HANDLE,
    INPUT icHelpFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Set pointer to custom help web-page for window
    Notes:  The help system must be "web" - assigned in progress.ini
------------------------------------------------------------------------------*/
FIND FIRST ttMenuStruct
     WHERE ttMenuStruct.hWindow = ihWindow
       AND ttMenuStruct.cName   = "defaultHelp"
     NO-ERROR.
IF NOT AVAIL ttMenuStruct THEN DO:
  FIND FIRST bttMenuStruct
       WHERE bttMenuStruct.cName   = "defaultHelp"
       NO-LOCK NO-ERROR.
  IF AVAIL bttMenuStruct THEN DO:
    CREATE ttMenuStruct.
    BUFFER-COPY bttMenuStruct TO ttMenuStruct.
    ttMenuStruct.hWindow = ihWindow.
  END.
  ELSE RETURN FALSE.
END.
  
ENTRY(3,ttMenuStruct.cLabel,";") = icHelpFile.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWidgetResize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWidgetResize Procedure 
FUNCTION setWidgetResize RETURNS LOGICAL
  ( INPUT hWidget      AS WIDGET-HANDLE,
    INPUT hWindow      AS WIDGET-HANDLE,
    INPUT icAction     AS CHAR,
    INPUT cExtra       AS CHAR ) :
/*------------------------------------------------------------------------------
   Purpose: Resize window or adjust frames to current window size
     Usage: WINDOW-RESIZED trigger:
              DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
            InitWindow (for suppressed window to adjust to current window size:
              DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:FIRST-CHILD,"Adjust","").
Parameters: hWidget: Target for resize
            hWindow: Resize: Parent window. Adjust: Parent frame
            cAction: "Resize" or "Adjust"
     Notes: Overrides are linked to combination of window and frame handles
            chFrame parameter is not in use (parent frame is now retrieved)  
            cExtra not in use
  Modified: 23.02.2012 by brynjar@chemistry.no:
            When embedding the window in a .net form there is no way to stop the form container to get smaller Y size than the window
            is configured to handle. The window height-pixel value cannot be set (increased) to it's minimum value either so we must
            avoid resize of widgets by setting DeltaY to zero. Also ttCurrWinSize.iHeightPixels must be kept at the minimum value
            so that when size increases the delta won't be calculated from a smaller value than minimum design value (see iCorrDeltaY)
------------------------------------------------------------------------------*/
DEF VAR hWidget2         AS WIDGET-HANDLE NO-UNDO.
DEF VAR chFrame          AS CHAR   NO-UNDO.
DEF VAR iOrgDeltaY       AS INT    NO-UNDO.
DEF VAR iOrgDeltaX       AS INT    NO-UNDO.
DEF VAR ix               AS INT    NO-UNDO.
DEF VAR iRealWidth       AS INT    NO-UNDO.
DEF VAR iRealHeight      AS INT    NO-UNDO.
DEF VAR hSettingsTable   AS HANDLE NO-UNDO.
DEF VAR hSettingsBuffer  AS HANDLE NO-UNDO.
DEF VAR hSettingsQuery   AS HANDLE NO-UNDO.
DEF VAR iCorrDeltaY      AS INT    NO-UNDO.

IF bDebug THEN
  MESSAGE PROGRAM-NAME(1) SKIP
          "Caller: " PROGRAM-NAME(2) SKIP
          VIEW-AS ALERT-BOX.

IF VALID-HANDLE(hWidgetWatch) THEN DO:  
  WidgetWatch(hWidgetWatch,"Begin trigger").
  hWidgetWatch:BGCOLOR = 12 NO-ERROR.
END.

ASSIGN hCurrWindow    = hWindow  
       bUseCalcValues = TRUE
       iCalcWidth     = 0
       cAction        = icAction
       cMoveToTopList = ""
       .

IF NOT bDebug THEN
/*   RUN LockWindowUpdate(hWindow:HWND,OUTPUT iRetVal) NO-ERROR. */
  RUN SendMessageW(hWindow:HWND, 11, 0, 0, output iRetVal).

FIND FIRST ttResizeTypes WHERE ttResizeTypes.chWindow = STRING(hWindow) NO-LOCK NO-ERROR.
IF NOT AVAIL ttResizeTypes THEN
  FIND FIRST ttResizeTypes WHERE ttResizeTypes.chWindow = "" NO-LOCK.

/* Calculate move (window handle is only supplied from procedure): */
IF hWindow NE ? THEN DO:
  FIND FIRST ttCurrWinSize
       WHERE ttCurrWinSize.chWindow = STRING(hWindow)
       NO-ERROR.
  FIND FIRST ttOrgWinSize
       WHERE ttOrgWinSize.chWindow = STRING(hWindow)
       NO-ERROR.
  FIND FIRST ttSplitBarX
       WHERE ttSplitBarX.chWindow = STRING(hWindow)
       NO-ERROR.
  
  IF NOT AVAIL ttCurrWinSize OR NOT AVAIL ttOrgWinSize THEN 
    RETURN FALSE.

  IF cAction = "Resize" THEN 
    ASSIGN iRealWidth    = hWindow:WIDTH-PIXELS
           iRealHeight   = hWindow:HEIGHT-PIXELS
           iCalcWidth   = IF ttOrgWinSize.iMinX = 0 THEN MAX(ttOrgWinSize.iWidthPixels,hWindow:WIDTH-PIXELS)   ELSE MAX(ttOrgWinSize.iMinX,hWindow:WIDTH-PIXELS)
           hWindow:HEIGHT-PIXELS = IF ttOrgWinSize.iMinY = 0 THEN MAX(ttOrgWinSize.iHeightPixels,hWindow:HEIGHT-PIXELS) ELSE MAX(ttOrgWinSize.iMinY,hWindow:HEIGHT-PIXELS)
           .
  ELSE iCalcWidth = MAX(ttOrgWinSize.iWidthPixels,hWindow:WIDTH-PIXELS).

  ASSIGN iMinXmove = ttOrgWinSize.iMinXmove
         iMinYmove = ttOrgWinSize.iMinYmove
         iDeltaX   = iCalcWidth  - (IF cAction = "Resize" THEN ttCurrWinSize.iWidthPixels  ELSE ttOrgWinSize.iWidthPixels)
         iDeltaY   = hWindow:HEIGHT-PIXELS - (IF cAction = "Resize" THEN ttCurrWinSize.iHeightPixels ELSE ttOrgWinSize.iHeightPixels)
         .

  IF iDeltaY < 0 AND 
     ((ttOrgWinSize.iMinY = 0 AND hWindow:HEIGHT-PIXELS < ttOrgWinSize.iHeightPixels) OR
      (ttOrgWinSize.iMinY NE 0 AND hWindow:HEIGHT-PIXELS < ttOrgWinSize.iMinY)) THEN
    ASSIGN iCorrDeltaY = iDeltaY
           iDeltaY     = 0.

/*          ttOrgWinSize.iMinXmove      = IF cAction = "resize" THEN ttOrgWinsize.iMinXmove + iDeltaX ELSE ttOrgWinSize.iMinXmove  */
  ASSIGN iOrgDeltaX                  = iDeltaX
         iOrgDeltaY                  = iDeltaY
         .
END.

bReleaseWinLock = FALSE.

IF VALID-HANDLE(hWidgetWatch) THEN
  WidgetWatch(hWidgetWatch,"Before adjust to frame").

/* Pull back splitbars to original positions (to avoid trouble when downsizing): */
IF cAction = "resize" THEN
  resetSplitBarPos(hWindow,iOrgDeltaX).

iDeltaX = iOrgDeltaX.
iDeltaY = iOrgDeltaY.

/* Adjust suppressed frame according to current window size: */
IF cAction = "Adjust" THEN DO:
  hWidget2 = hWidget:PARENT.

  IF hWidget2:TYPE = "FRAME" THEN DO:

    chFrame = STRING(hWidget2).
    
    IF NOT CAN-FIND(FIRST ttNoResizeX WHERE ttNoResizeX.chFrame  = STRING(hWidget2)
                                        AND ttNoResizeX.cWidget = hWidget2:NAME)
       AND hWidget2:WIDTH-PIXELS NE hWidget2:WIDTH-PIXELS + iDeltaX
       THEN DO:

      hWidget2:WIDTH-PIXELS = hWidget2:WIDTH-PIXELS + iDeltaX NO-ERROR.
      IF hWidget2:TYPE = "FRAME" THEN
        hWidget2:VIRTUAL-WIDTH-PIXELS  = hWidget2:WIDTH-PIXELS NO-ERROR.
    END.
    IF NOT CAN-FIND(FIRST ttNoResizeY WHERE ttNoResizeY.chFrame  = chFrame
                                        AND ttNoResizeY.cWidget = hWidget2:NAME)
       AND hWidget2:HEIGHT-PIXELS NE hWidget2:HEIGHT-PIXELS + iDeltaY
       THEN DO:
      hWidget2:HEIGHT-PIXELS = hWidget2:HEIGHT-PIXELS + iDeltaY NO-ERROR.
      IF hWidget2:TYPE = "FRAME" THEN
        hWidget2:VIRTUAL-HEIGHT-PIXELS = hWidget2:HEIGHT-PIXELS NO-ERROR.
    END.

    IF ((hWidget2:X > iMinXmove
         AND NOT CAN-FIND(FIRST ttNoMoveX WHERE ttNoMoveX.chFrame  = chFrame
                                            AND ttNoMoveX.cWidget = hWidget2:NAME))
         OR CAN-FIND(FIRST ttAddMoveX WHERE ttAddMoveX.chFrame  = chFrame
                                        AND ttAddMoveX.cWidget = hWidget2:NAME))
       AND hWidget2:X NE hWidget2:X + iDeltaX
       THEN DO:
      hWidget2:X = hWidget2:X + iDeltaX NO-ERROR.
    END.
    IF ((hWidget2:Y > iMinYmove
        AND NOT CAN-FIND(FIRST ttNoMoveY WHERE ttNoMoveY.chFrame  = chFrame
                                           AND ttNoMoveY.cWidget = hWidget2:NAME))
        OR CAN-FIND(FIRST ttAddMoveY WHERE ttAddMoveY.chFrame  = chFrame
                                       AND ttAddMoveY.cWidget = hWidget2:NAME))
       AND hWidget2:Y NE hWidget2:Y + iDeltaY
       THEN DO:
      hWidget2:Y = hWidget2:Y + iDeltaY NO-ERROR.
    END.
        
  END.
END.

/* Do the actual resize: */
iDeltaX = iOrgDeltaX.
iDeltaY = iOrgDeltaY.

IF VALID-HANDLE(hWidgetWatch) THEN
  WidgetWatch(hWidgetWatch,"Before resize").

DoWidgetResize (IF hWidget:TYPE = "WINDOW" THEN hWidget:FIRST-CHILD ELSE hWidget).

/* Adjust splitbar offset relative to new window size: */
IF AVAIL ttSplitBarX THEN DO:
  IF ttSplitBarX.hSplitBar:HEIGHT-PIXELS GE ttSplitBarX.hSplitFrame:HEIGHT-PIXELS THEN DO:
    ttSplitBarX.hSplitBar:HEIGHT-PIXELS = ttSplitBarX.hSplitFrame:HEIGHT-PIXELS - ttSplitBarX.hSplitBar:Y - 1.
    ttSplitBarX.hSplitBar:Y = 1.
  END.
  /* If this splitbar has been used before in this window and has now been re-created set to previous position: */ 
  FIND FIRST ttPrevSplitBarXpos
       WHERE ttPrevSplitBarXpos.chWindow = STRING(hWindow)
         AND ttPrevSplitBarXpos.cFrameName = ttSplitBarX.hSplitFrame:NAME
       NO-ERROR.
  IF NOT AVAIL ttPrevSplitBarXpos THEN
    FIND FIRST ttPrevSplitBarXpos
         WHERE ttPrevSplitBarXpos.chWindow = STRING(hWindow)
         NO-ERROR.
  IF AVAIL ttPrevSplitBarXpos THEN DO:
    ttSplitBarX.hSplitBar:X = MIN(ttPrevSplitBarXpos.iPrevXpos,ttSplitBarX.hSplitFrame:WIDTH-PIXELS - 15).
    DELETE ttPrevSplitBarXpos.
  END.
  ELSE
    ttSplitBarX.hSplitBar:X = MIN(iOrgSplitXpos * iCalcWidth / ttCurrWinSize.iWidthPixels,ttSplitBarX.hSplitFrame:WIDTH-PIXELS - 15) NO-ERROR.
  setSplitBarX(hWindow,ttSplitBarX.hSplitBar,IF iOrgDeltaX < 0 THEN ? ELSE NO).
END.
IF AVAIL ttSplitBarY THEN DO:
  IF ttSplitBarY.hSplitBar:WIDTH-PIXELS GE ttSplitBarY.hSplitFrame:WIDTH-PIXELS THEN DO:
    ttSplitBarY.hSplitBar:WIDTH-PIXELS = ttSplitBarY.hSplitFrame:WIDTH-PIXELS - ttSplitBarY.hSplitBar:X - 1.
    ttSplitBarY.hSplitBar:X = 1.
  END.
  /* If this splitbar has been used before in this window and has now been re-created set to previous position: */ 
  FIND FIRST ttPrevSplitBarYpos
       WHERE ttPrevSplitBarYpos.chWindow = STRING(hWindow)
         AND ttPrevSplitBarYpos.cFrameName = ttSplitBarY.hSplitFrame:NAME
       NO-ERROR.
  IF NOT AVAIL ttPrevSplitBarYpos THEN
    FIND FIRST ttPrevSplitBarYpos
         WHERE ttPrevSplitBarYpos.chWindow = STRING(hWindow)
         NO-ERROR.
  IF AVAIL ttPrevSplitBarYpos THEN DO:
    ttSplitBarY.hSplitBar:Y = MIN(ttPrevSplitBarYpos.iPrevYpos,ttSplitBarY.hSplitFrame:HEIGHT-PIXELS - 15).
    DELETE ttPrevSplitBarYpos.
  END.
  ELSE 
    ttSplitBarY.hSplitBar:Y = MIN(iOrgSplitYpos * hWindow:HEIGHT-PIXELS / ttCurrWinSize.iHeightPixels,ttSplitBarY.hSplitFrame:HEIGHT-PIXELS - 15) NO-ERROR.
  setSplitBarY(hWindow,ttSplitBarY.hSplitBar,IF iOrgDeltaY < 0 THEN ? ELSE NO).
END.

IF cAction = "Resize" THEN DO:
  IF iRealWidth < iCalcWidth THEN
    hWindow:WIDTH-PIXELS = iRealWidth.
  ELSE 
    hWindow:WIDTH-PIXELS = iCalcWidth.
END.

/* Extra stunt is needed because virtual size of frames won't schrink when negative sizing: */
IF iOrgDeltaX < 0 OR iOrgDeltaY < 0 THEN DO:
  getAllFrames(IF hWidget:TYPE = "WINDOW" THEN hWidget:FIRST-CHILD ELSE hWidget).
  FOR EACH ttAllFrames
      WHERE ttAllFrames.hWindow = hCurrWindow
        AND VALID-HANDLE(ttAllFrames.hFrame):
    ASSIGN ttAllFrames.hFrame:SCROLLABLE            = TRUE
           ttAllFrames.hFrame:VIRTUAL-WIDTH-PIXELS  = ttAllFrames.hFrame:WIDTH-PIXELS
           ttAllFrames.hFrame:VIRTUAL-HEIGHT-PIXELS = ttAllFrames.hFrame:HEIGHT-PIXELS
           NO-ERROR.
  END.
END.

/* Extra stunt is needed because virtual size of frames won't schrink when negative sizing: 
IF iOrgDeltaX < 0 OR iOrgDeltaY < 0 THEN DO:
  ASSIGN iDeltaX = 1
         iDeltaY = 1
         .
  DoWidgetResize (IF hWidget:TYPE = "WINDOW" THEN hWidget:FIRST-CHILD ELSE hWidget).   
  ASSIGN iDeltaX = -1
         iDeltaY = -1
         .
  DoWidgetResize (IF hWidget:TYPE = "WINDOW" THEN hWidget:FIRST-CHILD ELSE hWidget).   
  IF AVAIL ttSplitBarX THEN DO:
    ttSplitBarX.hSplitBar:X = ttSplitBarX.hSplitBar:X + 1.
    setSplitBarX(hWindow,ttSplitBarX.hSplitBar,?).
    ttSplitBarX.hSplitBar:X = ttSplitBarX.hSplitBar:X - 1.
    setSplitBarX(hWindow,ttSplitBarX.hSplitBar,?).

    IF ttSplitBarX.hSplitBar:HEIGHT-PIXELS GE ttSplitBarX.hSplitFrame:HEIGHT-PIXELS THEN DO:
      ttSplitBarX.hSplitBar:HEIGHT-PIXELS = ttSplitBarX.hSplitFrame:HEIGHT-PIXELS - 2.
      ttSplitBarX.hSplitBar:Y = 1.
    END.

  END.
  IF AVAIL ttSplitBarY THEN DO:
    ttSplitBarY.hSplitBar:Y = ttSplitBarY.hSplitBar:Y + 1.
    setSplitBarY(hWindow,ttSplitBarY.hSplitBar,?).
    ttSplitBarY.hSplitBar:Y = ttSplitBarY.hSplitBar:Y - 1.
    setSplitBarY(hWindow,ttSplitBarY.hSplitBar,?).

    IF ttSplitBarY.hSplitBar:WIDTH-PIXELS GE ttSplitBarY.hSplitFrame:WIDTH-PIXELS THEN DO:
      ttSplitBarY.hSplitBar:WIDTH-PIXELS = ttSplitBarY.hSplitFrame:WIDTH-PIXELS - 2.
      ttSplitBarY.hSplitBar:X = 1.
    END.
  END.
END.   
*/                                                                                    

ASSIGN ttCurrWinSize.iWidthPixels  = iCalcWidth
       ttCurrWinSize.iHeightPixels = hWindow:HEIGHT-PIXELS - iCorrDeltaY
       .

IF icAction = "resize" THEN
  setBGcolour(hWindow:FIRST-CHILD,"") NO-ERROR.

IF AVAIL ttSplitBarX THEN 
  ttSplitBarX.hSplitFrame:MOVE-TO-BOTTOM().

IF AVAIL ttSplitBarY THEN DO:
  ttSplitBarY.hSplitFrame:MOVE-TO-BOTTOM().
  ttSplitBarY.hSplitBar:WIDTH-PIXELS = 10.
  ttSplitBarY.hSplitBar:X = 1.
  ttSplitBarY.hSplitBar:WIDTH-PIXELS = ttSplitBarY.hSplitFrame:WIDTH-PIXELS - 2 NO-ERROR.
END.

DO ix = 1 TO NUM-ENTRIES(cMoveToTopList):
  hWidget2 = WIDGET-HANDLE(ENTRY(ix,cMoveToTopList)).
  IF hWidget2:WINDOW = hWindow THEN
    hWidget2:MOVE-TO-TOP() NO-ERROR.
END.

PUBLISH "ResizeTabFrames" (hWindow).
PUBLISH "ResizeBrowseColumns" (hWindow).

FOR EACH ttTransMarker
    WHERE ttTransMarker.hWindow = hWindow:
  IF VALID-HANDLE(ttTransMarker.hMarker) THEN
    ttTransMarker.hMarker:MOVE-TO-TOP().
END.


/* RUN LockWindowUpdate(0,OUTPUT iRetVal).  */
RUN SendMessageW(hWindow:HWND, 11, 1, 0, OUTPUT iRetVal).
RUN RedrawWindow(hWindow:HWND, 0, 0, 4 + 1 + 128, OUTPUT iRetVal).

bUseCalcValues  = FALSE.
bReleaseWinLock = TRUE.

IF cWatchLogFile NE "" THEN DO:
  hSettingsTable = getResizeSettings(hWindow).  
  hSettingsBuffer = hSettingsTable:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY hSettingsQuery.
  hSettingsQuery:SET-BUFFERS(hSettingsTable:DEFAULT-BUFFER-HANDLE).
  hSettingsQuery:QUERY-PREPARE("FOR EACH " + hSettingsTable:DEFAULT-BUFFER-HANDLE:NAME 
                              + " WHERE cWidget = '" + STRING(hWidgetWatch) + "'").
  hSettingsQuery:QUERY-OPEN().
  hSettingsQuery:GET-FIRST().
  REPEAT WHILE NOT hSettingsQuery:QUERY-OFF-END:
    PUT STREAM strWatchLog UNFORMATTED hSettingsTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD("cSetting"):BUFFER-VALUE.
    hSettingsQuery:GET-NEXT().
  END.
  DELETE OBJECT hSettingsQuery NO-ERROR.
  DELETE OBJECT hSettingsTable NO-ERROR.
  OUTPUT STREAM strWatchLog CLOSE.
END.

PUBLISH "ResizeFormComponents" (hWindow).

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWidgetWatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWidgetWatch Procedure 
FUNCTION setWidgetWatch RETURNS LOGICAL
  ( INPUT ihWidgetWatch  AS HANDLE,
    INPUT icWatchLogFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cWatchLogFile = icWatchLogFile.

IF cWatchLogFile NE "" THEN
  OUTPUT STREAM strWatchLog TO VALUE(cWatchLogFile).

hWidgetWatch = ihWidgetWatch.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShowMeTheWidgets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ShowMeTheWidgets Procedure 
FUNCTION ShowMeTheWidgets RETURNS LOGICAL
  ( INPUT hWidget    AS HANDLE, 
    INPUT iLevel     AS INT,
    INPUT cDirection AS CHAR,
    INPUT cOutDest   AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  List widgets to file
    Notes:  NB! Use level = 0 for initial call
------------------------------------------------------------------------------*/
DEF VAR cWidgetType AS CHAR   NO-UNDO.
DEF VAR hProc       AS HANDLE NO-UNDO.
DEF VAR cProcName   AS CHAR   NO-UNDO.

IF iLevel = 0 THEN DO:
  IF cOutDest = "" THEN RETURN TRUE.

  cWidgetListingFilter = "*".
  
  bOk = NO.
  MESSAGE PROGRAM-NAME(1) SKIP
          "View only frames? " 
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
   
  IF bOK THEN cWidgetListingFilter = "frame".

  OUTPUT TO VALUE(cOutDest).
  PUT UNFORMATTED 
      "LEVEL;"  
      "TYPE;"
      "NAME;" 
      "FRAME;"
      "X;"
      "AbsX;"
      "Y;"
      "AbsY;"
      "WIDTH-PIXELS;" 
      "HEIGHT-PIXELS;" 
      "TAB-POSITION;"
      "VISIBLE;"
      "VIRTUAL-WIDTH-PIXELS;"
      "VIRTUAL-HEIGHT-PIXELS;"
      "SCROLLABLE;"
      "PARENT;"
      "HANDLE;"
      "PROCEDURE;"
      SKIP.
END.
  
REPEAT WHILE hWidget NE ?:
  IF CAN-DO("BROWSE,BUTTON,COMBO-BOX,DIALOG-BOX,FILL-IN,EDITOR,FRAME,MENU-ITEM,RADIO-SET,RECTANGLE,SELECTION-LIST,SLIDER,SUB-MENU,TEXT,TOGGLE-BOX,WINDOW",hWidget:TYPE) 
     AND (IF hWidget:TYPE NE "window" THEN hWidget:NAME NE ? ELSE TRUE) THEN DO:

    IF hWidget:PARENT NE ? AND hWidget:PARENT:TYPE = "BROWSE" THEN 
      cWidgetType = "BROWSE-COLUMN".
    ELSE
      cWidgetType = hWidget:TYPE.

    IF cDirection = "bottom-up" AND CAN-QUERY(hWidget, 'FIRST-CHILD') AND hWidget:FIRST-CHILD  <> ? THEN
      ShowMeTheWidgets (hWidget:FIRST-CHILD,iLevel + 1,cDirection,"").
    IF cDirection = "bottom-up" AND CAN-QUERY(hWidget,'MENU-BAR') AND hWidget:MENU-BAR NE ? THEN 
      ShowMeTheWidgets(hWidget:MENU-BAR:FIRST-CHILD,iLevel + 1,cDirection,"").
    IF cDirection = "bottom-up" AND CAN-QUERY(hWidget,'FIRST-COLUMN') AND hWidget:FIRST-COLUMN NE ? THEN
      ShowMeTheWidgets(hWidget:FIRST-COLUMN,iLevel + 1,cDirection,"").
    IF cDirection = "bottom-up" AND CAN-QUERY(hWidget,'NEXT-COLUMN') AND hWidget:NEXT-COLUMN NE ? THEN
      ShowMeTheWidgets(hWidget:NEXT-COLUMN,iLevel + 1,cDirection,"").
 
    hProc = getProcHandle(hWidget).
    
    IF CAN-DO(cWidgetListingFilter,cWidgetType) THEN
      PUT UNFORMATTED 
        iLevel ";" 
        cWidgetType  ";"
        (IF hWidget:NAME NE ? THEN hWidget:NAME ELSE "")  ";" 
        (IF CAN-QUERY(hWidget,"FRAME") AND cWidgetType NE "frame" THEN hWidget:FRAME:NAME ELSE IF cWidgetType = "frame" THEN hWidget:NAME ELSE "") ";"
        (IF NOT CAN-DO("window,menu,sub-menu,menu-item",cWidgetType) THEN STRING(hWidget:X) ELSE "") ";"
        (IF NOT CAN-DO("window,menu,sub-menu,menu-item",cWidgetType) THEN STRING(getAbsPosition(hWidget,"X")) ELSE "") ";"
        (IF NOT CAN-DO("window,menu,sub-menu,menu-item",cWidgetType) THEN STRING(hWidget:Y) ELSE "") ";"
        (IF NOT CAN-DO("window,menu,sub-menu,menu-item",cWidgetType) THEN STRING(getAbsPosition(hWidget,"Y")) ELSE "") ";"
        (IF NOT CAN-DO("window,menu,sub-menu,menu-item",cWidgetType) THEN STRING(hWidget:WIDTH-PIXELS) ELSE "") ";"
        (IF NOT CAN-DO("window,menu,sub-menu,menu-item",cWidgetType) THEN STRING(hWidget:HEIGHT-PIXELS) ELSE "") ";"
        (IF NOT CAN-DO("window,menu,sub-menu,menu-item,browse-column",cWidgetType) AND hWidget:TAB-POSITION NE ? THEN STRING(hWidget:TAB-POSITION) ELSE "") ";"
        (IF cWidgetType NE "window" AND hWidget:VISIBLE NE ? THEN STRING(hWidget:VISIBLE) ELSE "") ";"
        (IF CAN-QUERY(hWidget,"VIRTUAL-WIDTH-PIXELS") THEN STRING(hWidget:VIRTUAL-WIDTH-PIXELS) ELSE "") ";"
        (IF CAN-DO("frame,window",cWidgetType) THEN STRING(hWidget:VIRTUAL-HEIGHT-PIXELS) ELSE "") ";"
        (IF CAN-DO("frame",cWidgetType) THEN STRING(hWidget:SCROLLABLE) ELSE "") ";"
        (IF VALID-HANDLE(hWidget:PARENT) THEN STRING(hWidget:PARENT) ELSE "") ";"
        (IF VALID-HANDLE(hWidget) THEN STRING(hWidget) ELSE "") ";"
        (IF VALID-HANDLE(hProc) THEN hProc:FILE-NAME ELSE "") ";"
        SKIP.
      
    IF cDirection NE "top-down" AND CAN-QUERY(hWidget, 'FIRST-CHILD') AND hWidget:FIRST-CHILD  <> ? THEN
       ShowMeTheWidgets(hWidget:FIRST-CHILD,iLevel + 1,cDirection,"").
    IF cDirection = "top-down" AND CAN-QUERY(hWidget,'MENU-BAR') AND hWidget:MENU-BAR NE ? THEN
      ShowMeTheWidgets(hWidget:MENU-BAR:FIRST-CHILD,iLevel + 1,cDirection,"").
    IF cDirection = "top-down" AND CAN-QUERY(hWidget,'FIRST-COLUMN') AND hWidget:FIRST-COLUMN NE ? THEN
      ShowMeTheWidgets(hWidget:FIRST-COLUMN,iLevel + 1,cDirection,"").
    IF cDirection = "top-down" AND CAN-QUERY(hWidget,'NEXT-COLUMN') AND hWidget:NEXT-COLUMN NE ? THEN
      ShowMeTheWidgets(hWidget:NEXT-COLUMN,iLevel + 1,cDirection,"").

  END.
  IF CAN-QUERY(hWidget,'FIRST-CHILD') AND hWidget:FIRST-CHILD  <> ? THEN
    ShowMeTheWidgets(hWidget:FIRST-CHILD,iLevel + 1,cDirection,"").

  IF CAN-QUERY(hWidget,'POPUP-MENU') AND hWidget:POPUP-MENU NE ? THEN
    ShowMeTheWidgets(hWidget:POPUP-MENU:FIRST-CHILD,iLevel + 1,cDirection,"").

  IF iLevel > 0 AND CAN-QUERY(hWidget,'NEXT-SIBLING') THEN hWidget = hWidget:NEXT-SIBLING.
  ELSE LEAVE.
END.

IF iLevel = 0 THEN OUTPUT CLOSE.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SpecialResize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SpecialResize Procedure 
FUNCTION SpecialResize RETURNS LOGICAL
  ( INPUT hWidget AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  Support SmartPak or other Smart stuff
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttAddSpecialResize WHERE ttAddSpecialResize.hFrame = hWidget NO-LOCK NO-ERROR. /* Smart Frame Flolder object (from Smartpak ) */
IF AVAIL ttAddSpecialResize AND VALID-HANDLE(ttAddSpecialResize.hProcedure) THEN DO: 
  ASSIGN
        hWidget:HIDDEN                = TRUE 
        hWidget:SCROLLABLE            = TRUE 
        hWidget:VIRTUAL-WIDTH-PIXELS  = hWidget:WIDTH-PIXELS 
        hWidget:VIRTUAL-HEIGHT-PIXELS = hWidget:HEIGHT-PIXELS 
        hWidget:WIDTH-PIXELS          = 
        IF CAN-FIND(FIRST ttNoResizeX WHERE 
                    ttNoResizeX.chFrame = string(ttAddSpecialResize.hFrame) 
                AND ttNoResizeX.cWidget = ttAddSpecialResize.hFrame:NAME) 
                THEN hWidget:WIDTH-PIXELS ELSE  hWidget:WIDTH-PIXELS  + iDeltaX
  
        hWidget:HEIGHT-PIXELS         = 
        IF CAN-FIND(FIRST ttNoResizeY WHERE 
                          ttNoResizeY.chFrame = string(ttAddSpecialResize.hFrame) 
                      AND ttNoResizeY.cWidget = ttAddSpecialResize.hFrame:NAME) 
                      THEN hWidget:HEIGHT-PIXELS ELSE hWidget:HEIGHT-PIXELS + iDeltaY
      .
  RUN resizeObject IN ttAddSpecialResize.hProcedure (hWidget:HEIGHT,  hWidget:WIDTH). 

  RETURN YES.
END.
ELSE DO:
  /* One time exception: */
  FIND FIRST ttOneTimeException 
       WHERE ttOneTimeException.hWidget = hWidget
       NO-ERROR.
  IF AVAIL ttOneTimeException THEN DO:
    DELETE ttOneTimeException.
    RETURN YES.
  END.
  ELSE RETURN NO.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToExcelViaFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ToExcelViaFile Procedure 
FUNCTION ToExcelViaFile RETURNS LOGICAL
  ( INPUT ihObject   AS HANDLE,
    INPUT iiMaxCount AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR chExcelInstance AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook      AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet     AS COM-HANDLE NO-UNDO.
DEF VAR iCount          AS INTEGER    NO-UNDO.
DEF VAR iColumn         AS INTEGER INITIAL 1 NO-UNDO.

DEF VAR hQuery          AS WIDGET-HANDLE NO-UNDO.
DEF VAR hBuffer         AS WIDGET-HANDLE NO-UNDO.
DEF VAR hField          AS WIDGET-HANDLE NO-UNDO.
DEF VAR hColumn         AS HANDLE NO-UNDO.

DEF VAR cFileName       AS CHAR NO-UNDO.
DEF VAR cOutput         AS CHAR NO-UNDO.
DEF VAR bOk             AS LOG  NO-UNDO.
DEF VAR fDec            AS DEC  FORMAT "->>,>>>,>>>,>>9.99".      
DEF VAR ix              AS INT  NO-UNDO.
DEF VAR cExcelFields    AS CHAR NO-UNDO.
DEF VAR iExtent         AS INT  NO-UNDO.
DEF VAR cDateFormat     AS CHAR NO-UNDO.

DEF VAR bKeepExcel      AS LOG  NO-UNDO INIT TRUE. /* Should be obtained from preferences set in JBoxUIlib.p */
DEF VAR cDelimiter      AS CHAR NO-UNDO.

IF DYNAMIC-FUNCTION("getAttribute",ihObject,"ExcelDelimiter") NE "" THEN
  cDelimiter = DYNAMIC-FUNCTION("getAttribute",ihObject,"ExcelDelimiter").
ELSE cDelimiter = "~t".

cDateFormat = DYNAMIC-FUNCTION("getAttribute",ihObject,"ExcelDateFormat").

IF DYNAMIC-FUNCTION("getAttribute",ihObject,"ExcelFileName") NE "" THEN
  cFileName = DYNAMIC-FUNCTION("getAttribute",ihObject,"ExcelFileName").
ELSE IF DYNAMIC-FUNCTION("getAttribute",ihObject,"ExcelFielName") NE "" THEN
  cFileName = DYNAMIC-FUNCTION("getAttribute",ihObject,"ExcelFielName").
ELSE DO:
  IF ihObject:TYPE = "browse" THEN
    cFileName = SESSION:TEMP-DIR 
                + (IF ihObject:NAME NE ? THEN 
                     ihObject:NAME
                   ELSE IF VALID-HANDLE(ihObject:WINDOW) THEN
                     REPLACE(REPLACE(REPLACE(ihObject:WINDOW:TITLE,"[",""),"]","")," ","")
                   ELSE DYNAMIC-FUNCTION("getAppTitle")).
  ELSE 
    cFileName = SESSION:TEMP-DIR + DYNAMIC-FUNCTION("getAppTitle").
  
  cFileName = cFileName + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt".
END.

IF NOT VALID-HANDLE(ihObject) THEN RETURN FALSE.

OUTPUT TO VALUE(cFileName) NO-CONVERT.

bOK = SESSION:SET-WAIT-STATE("General").

IF ihObject:TYPE = "BROWSE" THEN DO:

  iCount = 1.
  DO iColumn = 1 TO ihObject:NUM-COLUMNS:
    ASSIGN hColumn = ihObject:GET-BROWSE-COLUMN(iColumn)
           NO-ERROR.
    IF VALID-HANDLE(hColumn) AND hColumn:VISIBLE THEN
      cOutput = cOutput + REPLACE(DYNAMIC-FUNCTIO("getStrippedSortLabel",hColumn),"!"," ") + cDelimiter.
  END.
  PUT UNFORMATTED cOutput SKIP.
  cOutput = "". 
  
  iCount = 2.
    
  IF NOT ihObject:MULTIPLE OR (ihObject:MULTIPLE AND ihObject:NUM-SELECTED-ROWS = 0) THEN DO:      
    CREATE QUERY hQuery.    
    hQuery = ihObject:QUERY.
    hQuery:GET-FIRST().
    
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      DO iColumn = 1 TO ihObject:NUM-COLUMNS:
        ASSIGN hColumn = ihObject:GET-BROWSE-COLUMN(iColumn)
               hField = hColumn:BUFFER-FIELD NO-ERROR.
        IF VALID-HANDLE(hColumn) AND hColumn:VISIBLE THEN DO:
          IF hField:DATA-TYPE = "integer" OR hField:DATA-TYPE = "decimal" THEN DO:
            fDec = hField:BUFFER-VALUE.
            cOutput = cOutput + (IF fDec NE ? THEN STRING(fDec) ELSE "0") + cDelimiter.
          END.
          ELSE IF hField:DATA-TYPE = "DATE" AND cDateFormat = "yyyy-mm-dd" THEN 
            cOutput = cOutput + (IF hField:BUFFER-VALUE NE ? THEN STRING(YEAR(hField:BUFFER-VALUE)) + "-" + STRING(MONTH(hField:BUFFER-VALUE),"99") + "-" + STRING(DAY(hField:BUFFER-VALUE),"99") ELSE "") + cDelimiter.
          ELSE IF hField:DATA-TYPE = "DATE" OR hField:DATA-TYPE = "LOGICAL" THEN 
            cOutput = cOutput + (IF hField:STRING-VALUE NE ? THEN hField:STRING-VALUE ELSE "") + cDelimiter.
          ELSE 
            cOutput = cOutput + (IF hField:BUFFER-VALUE NE ? THEN ReplaceInText(hField:BUFFER-VALUE,"") ELSE "") + cDelimiter.
        END.
      END.
      PUT UNFORMATTED cOutput SKIP.
      cOutput = "". 
      iCount = iCount + 1.
      IF (iiMaxCount NE 0 AND iCount > iiMaxCount + 1) THEN LEAVE.
      hQuery:GET-NEXT().
    END.
  END.
  ELSE 
    DO ix = 1 TO ihObject:NUM-SELECTED-ROWS:
    bOk = ihObject:FETCH-SELECTED-ROW(ix).
    IF bOk THEN DO:
      DO iColumn = 1 TO ihObject:NUM-COLUMNS:
        ASSIGN hColumn = ihObject:GET-BROWSE-COLUMN(iColumn)
               hField = hColumn:BUFFER-FIELD NO-ERROR.
        IF VALID-HANDLE(hColumn) AND hColumn:VISIBLE THEN DO:
          IF hField:DATA-TYPE = "integer" OR hField:DATA-TYPE = "decimal" THEN DO:
            fDec = hField:BUFFER-VALUE.
            cOutput = cOutput + (IF fDec NE ? THEN STRING(fDec) ELSE "0") + cDelimiter.
          END.
          ELSE IF hField:DATA-TYPE = "DATE" AND cDateFormat = "yyyy-mm-dd" THEN 
            cOutput = cOutput + (IF hField:BUFFER-VALUE NE ? THEN STRING(YEAR(hField:BUFFER-VALUE)) + "-" + STRING(MONTH(hField:BUFFER-VALUE),"99") + "-" + STRING(DAY(hField:BUFFER-VALUE),"99") ELSE "") + cDelimiter.
          ELSE IF hField:DATA-TYPE = "DATE" OR hField:DATA-TYPE = "LOGICAL" THEN 
            cOutput = cOutput + (IF hField:STRING-VALUE NE ? THEN hField:STRING-VALUE ELSE "") + cDelimiter.
          ELSE 
            cOutput = cOutput + (IF hField:BUFFER-VALUE NE ? THEN REPLACE(REPLACE(REPLACE(hField:BUFFER-VALUE,CHR(10)," "),CHR(9)," "),CHR(13)," ") ELSE "") + cDelimiter.
        END.
      END.
      PUT UNFORMATTED cOutput SKIP.
      cOutput = "". 
      iCount = iCount + 1.  
    END.
  END.

END.

ELSE DO: /* <-- buffer */

  IF ihObject:TYPE = "query" THEN
    hBuffer = ihObject:GET-BUFFER-HANDLE(1).
  ELSE hBuffer = ihObject.

  cExcelFields = DYNAMIC-FUNCTION("getAttribute",ihObject,"excelfields").
  iCount = 1.
  IF cExcelFields NE "" THEN
    DO ix = 1 TO NUM-ENTRIES(cExcelFields):
      ASSIGN hColumn = hBuffer:BUFFER-FIELD(ENTRY(ix,cExcelFields))
             NO-ERROR.
      IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,"Error in definition of exportfield for Excel: " + ENTRY(ix,cExcelFields) + CHR(10) +
                                          ERROR-STATUS:GET-MESSAGE(1) + CHR(10) +
                                         "Programmers mistake in " + PROGRAM-NAME(2),"","").
        OUTPUT CLOSE.
        SESSION:SET-WAIT-STATE("").
        RETURN NO.
      END.
      cOutput = cOutput + hColumn:LABEL + cDelimiter.
    END.
  ELSE
    DO iColumn = 1 TO hBuffer:NUM-FIELDS:
      ASSIGN hColumn = hBuffer:BUFFER-FIELD(iColumn)
             NO-ERROR.
      DO ix = 0 TO hColumn:EXTENT:
        IF hColumn:EXTENT > 0 AND ix = 0 THEN NEXT.
        cOutput = cOutput + hColumn:LABEL + (IF ix > 0 THEN "[" + STRING(ix) + "]" ELSE "") + cDelimiter.
      END.
    END.
  PUT UNFORMATTED cOutput SKIP.
  cOutput = "". 
  
  iCount = 2.
    
  CREATE QUERY hQuery.
  
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " " + 
                        DYNAMIC-FUNCTION("getAttribute",ihObject,"excelquery") + " " + 
                        DYNAMIC-FUNCTION("getAttribute",ihObject,"excelsort")).
  hQuery:QUERY-OPEN().

  hQuery:GET-FIRST(). 
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    IF cExcelFields NE "" THEN
      DO iColumn = 1 TO NUM-ENTRIES(cExcelFields):
        hField = hBuffer:BUFFER-FIELD(ENTRY(iColumn,cExcelFields)) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
          DYNAMIC-FUNCTION("DoMessage",0,0,"Error in definition of exportfield for Excel: " + ENTRY(iColumn,cExcelFields) + CHR(10) +
                                           "Programmers mistake in " + PROGRAM-NAME(2),"","").
        END.

        DO ix = 0 TO hField:EXTENT:
          IF hField:EXTENT > 0 AND ix = 0 THEN NEXT.

          IF hField:DATA-TYPE = "integer" OR hField:DATA-TYPE = "decimal" THEN DO:
            fDec = hField:BUFFER-VALUE[ix].
            cOutput = cOutput + (IF fDec NE ? THEN STRING(fDec) ELSE "0") + cDelimiter.
          END.
          ELSE IF hField:DATA-TYPE = "DATE" AND cDateFormat = "yyyy-mm-dd" THEN 
            cOutput = cOutput + (IF hField:BUFFER-VALUE[ix] NE ? THEN STRING(YEAR(hField:BUFFER-VALUE[ix])) + "-" + STRING(MONTH(hField:BUFFER-VALUE[ix]),"99") + "-" + STRING(DAY(hField:BUFFER-VALUE[ix]),"99") ELSE "") + cDelimiter.
          ELSE IF hField:DATA-TYPE = "DATE" OR hField:DATA-TYPE = "LOGICAL" THEN 
            cOutput = cOutput + (IF hField:STRING-VALUE[ix] NE ? THEN hField:STRING-VALUE[ix] ELSE "") + cDelimiter.
          ELSE 
            cOutput = cOutput + (IF hField:BUFFER-VALUE[ix] NE ? THEN REPLACE(REPLACE(REPLACE(hField:BUFFER-VALUE[ix],CHR(10)," "),CHR(9)," "),CHR(13)," ") ELSE "") + cDelimiter.
        END.
      END.
    ELSE
      DO iColumn = 1 TO hBuffer:NUM-FIELDS:

        hField = hBuffer:BUFFER-FIELD(iColumn) NO-ERROR.

        DO ix = 0 TO hField:EXTENT:
          IF hField:EXTENT > 0 AND ix = 0 THEN NEXT.

          IF hField:DATA-TYPE = "integer" OR hField:DATA-TYPE = "decimal" THEN DO:
            fDec = hField:BUFFER-VALUE[ix].
            cOutput = cOutput + (IF fDec NE ? THEN STRING(fDec) ELSE "0") + cDelimiter.
          END.
          ELSE IF hField:DATA-TYPE = "DATE" AND cDateFormat = "yyyy-mm-dd" THEN 
            cOutput = cOutput + (IF hField:BUFFER-VALUE[ix] NE ? THEN STRING(YEAR(hField:BUFFER-VALUE[ix])) + "-" + STRING(MONTH(hField:BUFFER-VALUE[ix]),"99") + "-" + STRING(DAY(hField:BUFFER-VALUE[ix]),"99") ELSE "") + cDelimiter.
          ELSE IF hField:DATA-TYPE = "DATE" OR hField:DATA-TYPE = "LOGICAL" THEN 
            cOutput = cOutput + (IF hField:STRING-VALUE[ix] NE ? THEN hField:STRING-VALUE[ix] ELSE "") + cDelimiter.
          ELSE 
            cOutput = cOutput + (IF hField:BUFFER-VALUE[ix] NE ? THEN REPLACE(REPLACE(REPLACE(hField:BUFFER-VALUE[ix],CHR(10)," "),CHR(9)," "),CHR(13)," ") ELSE "") + cDelimiter.
        END.
      END.
    PUT UNFORMATTED cOutput SKIP.
    cOutput = "". 
    iCount = iCount + 1.
    IF (iiMaxCount NE 0 AND iCount > iiMaxCount + 1) THEN LEAVE.
    hQuery:GET-NEXT().
  END.
  DELETE OBJECT hQuery.
END.

OUTPUT CLOSE.

chExcelInstance = DYNAMIC-FUNCTION("getExcelHandle").


IF NOT VALID-HANDLE(chExcelInstance) THEN DO:
  IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
    MESSAGE "Could not open file: " cFileName VIEW-AS ALERT-BOX ERROR.
  bOK = SESSION:SET-WAIT-STATE("").
  RETURN TRUE.
END.
 
PUBLISH "ExcelPreLoadParams" (ihObject,chExcelInstance,iCount - 1).

chExcelInstance:Workbooks:OpenText(cFileName,2,,,,,TRUE).
 
ASSIGN chWorkbook                           = chExcelInstance:WorkBooks:ITEM(1)
       chWorkSheet                          = chExcelInstance:Sheets:ITEM(1)
       chWorkSheet:Rows(1):FONT:Bold        = TRUE
       .
ASSIGN
       chWorkSheet:PageSetup:LeftFooter     = DYNAMIC-FUNCTION("getAppTitle")
       chWorkSheet:PageSetup:CenterFooter   = STRING(TODAY,"99/99/9999")  + " " + STRING(TIME,"HH:MM") /*"&D &T"*/
       chWorkSheet:PageSetup:RightFooter    = "Page &P of &N"
       chWorkSheet:PageSetup:Orientation    = 2
       chWorkSheet:PageSetup:FitToPagesWide = 1
       NO-ERROR.

IF ihObject:TYPE = "browse" THEN
  chWorkSheet:NAME  =  IF ihObject:NAME NE ? THEN ihObject:NAME
                       ELSE IF VALID-HANDLE(ihObject:WINDOW) THEN
                         REPLACE(REPLACE(REPLACE(ihObject:WINDOW:TITLE,"[",""),"]","")," ","")
                       ELSE DYNAMIC-FUNCTION("getAppTitle").
ELSE IF DYNAMIC-FUNCTION("getAppTitle") NE "" THEN 
  chWorkSheet:NAME  =  DYNAMIC-FUNCTION("getAppTitle").

chWorkSheet:PageSetup:LeftHeader     = IF ihObject:TYPE = "browse" AND VALID-HANDLE(ihObject:WINDOW) THEN
                                         chWorkSheet:NAME
                                       ELSE DYNAMIC-FUNCTION("getAppTitle")
                                       NO-ERROR.


chWorkSheet:Columns("A:Z"):AutoFit().
chWorkSheet:Range("B2"):Select().
chExcelInstance:ActiveWindow:FreezePanes = True.

PUBLISH "ExcelSheetParams" (ihObject,chExcelInstance,chWorkbook,chWorksheet,iCount - 1).

IF DYNAMIC-FUNCTION("getAttribute",ihObject,"viewexcel") NE "no" THEN
  chExcelInstance:VISIBLE = TRUE NO-ERROR.

IF DYNAMIC-FUNCTION("getAttribute",ihObject,"excelpreview") NE "" THEN
  chWorkSheet:PrintPreview().

RELEASE OBJECT chWorksheet NO-ERROR.
IF NOT DYNAMIC-FUNCTION("getAttribute",ihObject,"enableExcelEvents") = "yes" THEN
  RELEASE OBJECT chWorkbook NO-ERROR.
IF NOT bKeepExcel THEN 
  RELEASE OBJECT chExcelInstance NO-ERROR.

bOK = SESSION:SET-WAIT-STATE("").

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WidgetWatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WidgetWatch Procedure 
FUNCTION WidgetWatch RETURNS LOGICAL
  ( INPUT ihWidget  AS HANDLE,
    INPUT icLogText AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iVirtWidth  AS INT NO-UNDO.
DEF VAR iVirtHeight AS INT NO-UNDO.

IF ihWidget = hWidgetWatch THEN DO:
  IF CAN-QUERY(ihWidget,"virtual-width-pixels") THEN 
    ASSIGN iVirtWidth = ihWidget:VIRTUAL-WIDTH-PIXELS
           iVirtHeight = ihWidget:VIRTUAL-HEIGHT-PIXELS.

  IF cWatchLogFile NE "" THEN DO:   

    IF SEEK(strWatchLog) = ? THEN 
      OUTPUT STREAM strWatchLog TO VALUE(cWatchLogFile).

    PUT STREAM strWatchLog UNFORMATTED ihWidget:NAME ": " icLogText SKIP
        "X: " ihWidget:X SKIP
        "Absolute X: " STRING(getAbsPosition(ihWidget,"X")) SKIP
        "WIDTH-PIXELS: " ihWidget:WIDTH-PIXELS 
        "  VIRTUAL-WIDTH-PIXELS: " iVirtWidth  SKIP
        "Y: " ihWidget:Y SKIP
        "Absolute Y: " STRING(getAbsPosition(ihWidget,"Y")) SKIP
        "HEIGHT-PIXELS: " ihWidget:HEIGHT-PIXELS 
        "  VIRTUAL-HEIGHT-PIXELS: " iVirtHeight  SKIP
        SKIP(1).
  END.
  ELSE
    MESSAGE PROGRAM-NAME(1) SKIP
            icLogText ": " ihWidget:NAME SKIP
            "X: " ihWidget:X SKIP
            "Absolute X: " STRING(getAbsPosition(ihWidget,"X")) SKIP
            "WIDTH-PIXELS: " ihWidget:WIDTH-PIXELS SKIP
            "Y: " ihWidget:Y SKIP
            "Absolute Y: " STRING(getAbsPosition(ihWidget,"Y")) SKIP
            "HEIGHT-PIXELS: " ihWidget:HEIGHT-PIXELS SKIP
            VIEW-AS ALERT-BOX.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

