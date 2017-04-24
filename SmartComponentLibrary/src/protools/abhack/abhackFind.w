&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH.
USING Consultingwerk.WindowIntegrationKit.Forms.* FROM PROPATH.

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
DEFINE INPUT  PARAMETER pcAbhackTempDir AS CHARACTER   NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE gcFindFile       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ghTargetEditor   AS HANDLE      NO-UNDO.
DEFINE VARIABLE ghWinToStick     AS HANDLE      NO-UNDO.
DEFINE VARIABLE ghABHackWin      AS HANDLE      NO-UNDO.
DEFINE VARIABLE lActualLeaveDone AS LOGICAL     NO-UNDO.

DEFINE VARIABLE gcDummyEdt AS CHARACTER
  VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
  SIZE 30 BY 4 NO-UNDO.

DEFINE FRAME fDummy gcDummyEdt AT ROW 2.43 COL 18 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
    SIDE-LABELS NO-UNDERLINE THREE-D
    AT COL 1 ROW 1
    SIZE 80 BY 16.

DEFINE VARIABLE ghFindEdt AS HANDLE      NO-UNDO.


PROCEDURE GetKeyboardState EXTERNAL "user32.dll":
   DEFINE INPUT  PARAMETER KBState AS LONG. /* memptr */
   DEFINE RETURN PARAMETER RetVal  AS LONG. /* bool   */
END PROCEDURE.

/* 14-MAR-2014 jcc: variable to hold the current selection */
DEFINE VARIABLE gcSelection AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnNext gcToFind gcScope fiEdtInfoPos ~
fiEdtInfoFile btnPrev
&Scoped-Define DISPLAYED-OBJECTS gcToFind gcScope fiEdtInfoPos ~
fiEdtInfoFile

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-gcToFind
       MENU-ITEM m_Clear_MRU    LABEL "Clear MRU"     .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE cfSpy AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chcfSpy AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrameRed AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrameRed AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnNext
     IMAGE-UP FILE "protools/abhack/pics/next.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Next"
     SIZE 10 BY .91.

DEFINE BUTTON btnPrev
     IMAGE-UP FILE "protools/abhack/pics/prev.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Prev"
     SIZE 10 BY .91.

DEFINE VARIABLE gcToFind AS CHARACTER
     LABEL "Find"
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN AUTO-COMPLETION
     SIZE 21 BY 1 TOOLTIP "Enter => Find Next, Shift/Enter => Find Prev,  Esc => Focus to editor" NO-UNDO.

DEFINE VARIABLE fiEdtInfoFile AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fiEdtInfoPos AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE gcScope AS CHARACTER INITIAL "local"
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS
          "&Local", "Local",
"&Global", "global"
     SIZE 20 BY .91 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnNext AT ROW 1 COL 29
     gcToFind AT ROW 1 COL 5 COLON-ALIGNED
     gcScope AT ROW 1 COL 51 NO-LABEL
     fiEdtInfoPos AT ROW 1 COL 73 NO-LABEL WIDGET-ID 2
     fiEdtInfoFile AT ROW 1 COL 107 NO-LABEL WIDGET-ID 6
     btnPrev AT ROW 1 COL 39
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 132.8 BY 1.19
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
         TITLE              = "AbHack Finder"
         HEIGHT             = 1.19
         WIDTH              = 132.8
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 183
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 183
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = no
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
/* SETTINGS FOR FILL-IN fiEdtInfoFile IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN
       fiEdtInfoFile:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiEdtInfoPos IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN
       fiEdtInfoPos:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN
       gcToFind:POPUP-MENU IN FRAME DEFAULT-FRAME       = MENU POPUP-MENU-gcToFind:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME




/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrameRed ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1
       COLUMN          = 60
       HEIGHT          = .86
       WIDTH           = 2.6
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1
       COLUMN          = 67
       HEIGHT          = .95
       WIDTH           = 3
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME cfSpy ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1
       COLUMN          = 70
       HEIGHT          = .95
       WIDTH           = 2
       WIDGET-ID       = 4
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrameRed OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimerRed */
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimerStickToWindow */
/* cfSpy OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimerSpyEditor */
      CtrlFrameRed:MOVE-AFTER(gcScope:HANDLE IN FRAME DEFAULT-FRAME).
      CtrlFrame:MOVE-AFTER(CtrlFrameRed).
      cfSpy:MOVE-AFTER(CtrlFrame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* AbHack Finder */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AbHack Finder */
DO:
  RETURN NO-APPLY.

  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext C-Win
ON CHOOSE OF btnNext IN FRAME DEFAULT-FRAME /* Next */
DO:
    IF   ghTargetEditor:TEXT-SELECTED
     AND ghTargetEditor:SELECTION-TEXT BEGINS gcToFind
     THEN ghTargetEditor:CURSOR-CHAR = ghTargetEditor:CURSOR-CHAR + LENGTH(ghTargetEditor:SELECTION-TEXT).
    RUN searchThat ("right").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrev C-Win
ON CHOOSE OF btnPrev IN FRAME DEFAULT-FRAME /* Prev */
DO:
    IF   ghTargetEditor:TEXT-SELECTED
     AND ghTargetEditor:SELECTION-TEXT BEGINS gcToFind
     THEN DO:
        IF ghTargetEditor:CURSOR-CHAR = 1 THEN DO:
            IF ghTargetEditor:CURSOR-LINE = 1 THEN RETURN. /* e are at the top ! */
            ghTargetEditor:CURSOR-LINE = ghTargetEditor:CURSOR-LINE - 1.
            ghTargetEditor:CURSOR-CHAR = 5000 NO-ERROR.
        END.

        ghTargetEditor:CURSOR-CHAR = MAXIMUM(1, ghTargetEditor:CURSOR-CHAR - LENGTH(ghTargetEditor:SELECTION-TEXT)).
     END.
    RUN searchThat ("left").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cfSpy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cfSpy C-Win OCX.Tick
PROCEDURE cfSpy.PSTimerSpyEditor.Tick .
/*------------------------------------------------------------------------------
  Purpose:     Display position within file, selection length, selection lines, file length, file lines
  Parameters:  None required for OCX.
  Notes:       12-MAR-2014 jcc: new functionality
------------------------------------------------------------------------------*/
DEFINE VARIABLE cHighLight AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSelection AS CHARACTER   NO-UNDO.

/* chcfSpy:PSTimerSpyEditor:ENABLED = NO. */

fiEdtInfoPos:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
    "Ln:" + STRING(ghTargetEditor:CURSOR-LINE) +
    " Col:" + STRING(ghTargetEditor:CURSOR-CHAR) +
    " Sel:" + IF ghTargetEditor:SELECTION-TEXT = ?
              THEN "-"
              ELSE (STRING(LENGTH(ghTargetEditor:SELECTION-TEXT)) +
                    IF INDEX(ghTargetEditor:SELECTION-TEXT, "~n") > 0
                    THEN " | " + STRING(LENGTH(RIGHT-TRIM(ghTargetEditor:SELECTION-TEXT, "~n")) - LENGTH(REPLACE(RIGHT-TRIM(ghTargetEditor:SELECTION-TEXT, "~n"), "~n", "")) + 1) ELSE "") NO-ERROR.

fiEdtInfoFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
    "Lines:" + STRING(ghTargetEditor:NUM-LINES) +
    " Len:" + STRING(ghTargetEditor:LENGTH) NO-ERROR.

/* 14-MAR-2014 jcc: update lexer so that the selected word gets highlighted */
/* The result is a Notepad++ like highlight of the selected word.           */
IF VALID-HANDLE(ghTargetEditor) THEN DO:
    cSelection = ghTargetEditor:SELECTION-TEXT NO-ERROR.
    IF cSelection = ? THEN cSelection = "".
    IF cSelection = gcSelection THEN LEAVE.
    ASSIGN
     gcSelection = cSelection
     cHighLight  = cSelection.
    IF INDEX(gcSelection, CHR(10)) > 0 OR INDEX(gcSelection, CHR(13)) > 0 OR INDEX(gcSelection, " ") > 0 THEN
        cHighLight = "".
    LOAD 'user.vlx' DIR SESSION:TEMP-DIRECTORY + "proedit" BASE-KEY "ini".
    USE 'user.vlx'.
    PUT-KEY-VALUE SECTION "Progress 4GL" KEY "userkeywords" VALUE cHighLight.
    UNLOAD 'user.vlx'.
    ghTargetEditor:SOURCE-COMMAND("reload-lex", "").
    /* Note: reload.lex is defined in reload-lex.e, which must be stored in SESSION:TEMP-DIRECTORY + "\proedit"
    and contain the following text:

    #include 'slick.sh'
    defmain() {
        _str lexer_filename;
        lexer_filename = _config_path():+USER_LEXER_FILENAME;
        _clex_load(lexer_filename);
    }
    */
END.

/* chcfSpy:PSTimerSpyEditor:ENABLED = YES. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimerStickToWindow.Tick .
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  None required for OCX.
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE iNewWidth AS INTEGER   NO-UNDO.
DEFINE VARIABLE iWinX     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iWinY     AS INTEGER   NO-UNDO.

DEFINE VARIABLE ghWinToStick_X AS INTEGER NO-UNDO .
DEFINE VARIABLE ghWinToStick_Y AS INTEGER NO-UNDO .

DEFINE VARIABLE oEmbeddedWindowForm AS IEmbeddedWindowForm NO-UNDO .


IF NOT VALID-HANDLE(ghWinToStick)
 OR ghWinToStick:TYPE <> "WINDOW"
 THEN  DO:
    chCtrlFrame:PSTimerStickToWindow:ENABLED = NO. /* we will set it again on next publish of abhackFindTrackThis */
    {&WINDOW-NAME}:VISIBLE = NO.
    RETURN.
END.

ASSIGN ghWinToStick_X = ghWinToStick:X
       ghWinToStick_Y = ghWinToStick:Y .

/* Mike Fechner, Consultingwerk Ltd. 01.05.2016
   Adjust Find Window Location relative to embedded procedure editor */
IF WinKitSettings:WinKitActive THEN DO:
    oEmbeddedWindowForm = WinKitForms:FromEmbeddedWindow(ghWinToStick) .

    IF VALID-OBJECT (oEmbeddedWindowForm) THEN
        ASSIGN ghWinToStick_X = oEmbeddedWindowForm:ClientArea:PointToScreen(NEW System.Drawing.Point (0,0)):X
               ghWinToStick_Y = oEmbeddedWindowForm:ClientArea:PointToScreen(NEW System.Drawing.Point (0,0)):Y .
END.

iWinX = ghWinToStick_X /*+ 10*/. /* 14-MAR-2014 jcc: commented + 10 */
/* 16-NOV-2007 sla: avoid flashings when the window is too far on the bottom */
iWinY = MINIMUM(ghWinToStick_Y + ghWinToStick:HEIGHT-PIXELS + 26 /*+ 32*/ /* 14-MAR-2014 jcc: changed 32 */
               ,SESSION:WORK-AREA-HEIGHT-PIXELS - {&WINDOW-NAME}:HEIGHT-PIXELS - 24). /* 16-NOV-2007 sla: don't ask me why 24... */

iNewWidth = MAXIMUM(ghWinToStick:WIDTH-PIXELS, 664).

IF   {&WINDOW-NAME}:X = iWinX
 AND {&WINDOW-NAME}:Y = iWinY
 AND {&WINDOW-NAME}:WIDTH-PIXELS = iNewWidth /* 14-MAR-2014 jcc: added */
 THEN RETURN.

/* if we don't do that, then the window might appear behind another */
{&WINDOW-NAME}:MOVE-TO-TOP().
ghWinToStick:MOVE-TO-TOP().

ASSIGN
 {&WINDOW-NAME}:X = iWinX
 {&WINDOW-NAME}:Y = iWinY
 NO-ERROR.

 /* 14-MAR-2014 jcc: added the following code to manage resize & reposition of widgets */
DO WITH FRAME {&FRAME-NAME}:
    IF iNewWidth > FRAME {&FRAME-NAME}:WIDTH-PIXELS THEN ASSIGN
        {&WINDOW-NAME}:WIDTH-PIXELS = iNewWidth
        FRAME {&FRAME-NAME}:WIDTH-PIXELS         = iNewWidth
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = iNewWidth
        NO-ERROR.
    ASSIGN
        fiEdtInfoFile:X = {&WINDOW-NAME}:WIDTH-PIXELS - fiEdtInfoFile:WIDTH-PIXELS - 5
        fiEdtInfoPos:X  = fiEdtInfoFile:X - fiEdtInfoPos:WIDTH-PIXELS
        NO-ERROR.
    IF iNewWidth < FRAME {&FRAME-NAME}:WIDTH-PIXELS THEN ASSIGN
        FRAME {&FRAME-NAME}:WIDTH-PIXELS         = iNewWidth
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = iNewWidth
        {&WINDOW-NAME}:WIDTH-PIXELS = iNewWidth
        NO-ERROR.
    ASSIGN
        fiEdtInfoFile:X = {&WINDOW-NAME}:WIDTH-PIXELS - fiEdtInfoFile:WIDTH-PIXELS - 5
        fiEdtInfoPos:X  = fiEdtInfoFile:X - fiEdtInfoPos:WIDTH-PIXELS
        NO-ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrameRed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrameRed C-Win OCX.Tick
PROCEDURE CtrlFrameRed.PSTimerRed.Tick .
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  None required for OCX.
  Notes:
------------------------------------------------------------------------------*/
    gcToFind:BGCOLOR IN FRAME {&FRAME-NAME} = ?.
    chCtrlFrameRed:PSTimerRed:ENABLED = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gcScope
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcScope C-Win
ON VALUE-CHANGED OF gcScope IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.

  APPLY "ENTRY" TO gcToFind.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gcToFind
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcToFind C-Win
ON END-ERROR OF gcToFind IN FRAME DEFAULT-FRAME /* Find */
DO:
  /* developpers appreciate the ability to go back to the editor with esc */
  IF VALID-HANDLE(ghTargetEditor) THEN APPLY 'ENTRY' TO ghTargetEditor.

  /* The APpBuilder may bring associated GUI window back to front, and in front of the ABHack finder */
  {&WINDOW-NAME}:MOVE-TO-TOP().
  ghTargetEditor:WINDOW:MOVE-TO-TOP().

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcToFind C-Win
ON ENTRY OF gcToFind IN FRAME DEFAULT-FRAME /* Find */
DO:
    IF NOT VALID-HANDLE(ghTargetEditor) THEN RETURN.
    RUN dumpTargetEditor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcToFind C-Win
ON RETURN OF gcToFind IN FRAME DEFAULT-FRAME /* Find */
DO:
DEFINE VARIABLE iGetKeyboardRtn AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLookup         AS INTEGER     NO-UNDO.
DEFINE VARIABLE lShiftPressed   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mKBState        AS MEMPTR      NO-UNDO.

SET-SIZE(mKBState) = 256.
RUN GetKeyboardState(GET-POINTER-VALUE(mKBState), OUTPUT iGetKeyboardRtn).
lShiftPressed = GET-BYTE(mKBState, 17) > 127. /*16+1=17=Shift*/
SET-SIZE(mKBState) = 0.


IF lShiftPressed THEN APPLY "CHOOSE" TO btnPrev.
ELSE APPLY "CHOOSE" TO btnNext.

IF {&SELF-NAME} = "" THEN RETURN.
iLookup = {&SELF-NAME}:LOOKUP({&SELF-NAME}).
IF iLookup = 0 THEN DO:
    IF {&SELF-NAME} = ? THEN RETURN.
    {&SELF-NAME}:ADD-FIRST({&SELF-NAME}).
    IF {&SELF-NAME}:NUM-ITEMS > 20 THEN {&SELF-NAME}:DELETE(21).
    IF {&SELF-NAME}:NUM-ITEMS > 20 THEN {&SELF-NAME}:DELETE(21). /* another waay to make sure we will never go more than 20 items */
END.
ELSE IF iLookup > 1 THEN DO:
    {&SELF-NAME}:DELETE(iLookup).
    {&SELF-NAME}:ADD-FIRST({&SELF-NAME}).
END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcToFind C-Win
ON VALUE-CHANGED OF gcToFind IN FRAME DEFAULT-FRAME /* Find */
DO:
    DEFINE VARIABLE cBegins      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEntry       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iEntry       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iTypedLength AS INTEGER     NO-UNDO.

    ASSIGN gcToFind.

    IF gcToFind = "" THEN RETURN.

    RUN searchThat ("right").

/* Actaully, just use the nice AUTO-COMPLETE feature  */
/*     DO iEntry = 1 TO {&SELF-NAME}:NUM-ITEMS:                              */
/*         cEntry = {&SELF-NAME}:ENTRY(iEntry).                              */
/*         IF cEntry BEGINS gcToFind AND gcToFind < cEntry THEN DO:          */
/*             iTypedLength = LENGTH(gcToFind).                              */
/*             gcToFind:SCREEN-VALUE = cEntry.                               */
/*             gcToFind:SET-SELECTION(iTypedLength + 1, LENGTH(cEntry) + 1). */
/*             gcToFind:CURSOR-OFFSET = iTypedLength.                        */
/*         END.                                                              */
/*     END.                                                                  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Clear_MRU
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Clear_MRU C-Win
ON CHOOSE OF MENU-ITEM m_Clear_MRU /* Clear MRU */
DO:
  gcToFind:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
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
ON CLOSE OF THIS-PROCEDURE DO:
    IF VALID-HANDLE(ghABHackWin) THEN RUN setAbhackFindProfile IN ghABHackWin (gcScope, gcToFind:LIST-ITEMS IN FRAME {&FRAME-NAME}).
    RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  ASSIGN
   gcFindFile  = pcAbhackTempDir + "findFile.p"
   ghFindEdt   = gcDummyEdt:HANDLE IN FRAME fDummy
   ghABHackWin = SOURCE-PROCEDURE
   gcToFind:DELIMITER IN FRAME {&FRAME-NAME} = CHR(10)
   {&WINDOW-NAME}:HEIGHT-PIXELS = gcToFind:HEIGHT-PIXELS.

  RUN restoreProfile.

  RUN enable_UI.

  SUBSCRIBE TO "abhackFindExit" ANYWHERE RUN-PROCEDURE "destroyObject".
  SUBSCRIBE TO "abhackFindTrackThis" IN ghABHackWin.
  SUBSCRIBE TO "abhackFindEntry" IN ghABHackWin.
  /* 18-AUG-2007 sla: new feature to limit the scope of find selected text */
  SUBSCRIBE TO "abhackFindGetFindScope" IN ghABHackWin.
  SUBSCRIBE TO "abhackFindBlinkRed" IN ghABHackWin.
  SUBSCRIBE TO "stopABHackTimers" IN ghABHackWin.
  SUBSCRIBE TO "restartABHackTimers" IN ghABHackWin.

  /* SUBSCRIBE PROCEDURE SOURCE-PROCEDURE TO "abhackFindThat" IN THIS-PROCEDURE. */
  SUBSCRIBE PROCEDURE ghABHackWin TO "getCurrentBlockBoundaries" IN THIS-PROCEDURE.


  IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
      MESSAGE "This procedure should be run persistently, about to exit"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      DELETE PROCEDURE THIS-PROCEDURE.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE abhackFindBlinkRed C-Win
PROCEDURE abhackFindBlinkRed :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    gcToFind:BGCOLOR IN FRAME {&FRAME-NAME} = 12.
    chCtrlFrameRed:PSTimerRed:ENABLED = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE abhackFindEntry C-Win
PROCEDURE abhackFindEntry :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pcProposeThisText AS CHARACTER   NO-UNDO.

    IF pcProposeThisText > "" THEN DO:
        gcToFind = pcProposeThisText.
        gcToFind:SCREEN-VALUE IN FRAME {&FRAME-NAME} = pcProposeThisText.
    END.

    APPLY "entry" TO gcToFind.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE abhackFindGetFindScope C-Win
PROCEDURE abhackFindGetFindScope :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opcScope AS CHARACTER   NO-UNDO.

opcScope = gcScope.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE abhackFindTrackThis C-Win
PROCEDURE abhackFindTrackThis :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

/* first reset */
ASSIGN
 ghTargetEditor = ?
 ghWinToStick   = ?.

IF  NOT VALID-HANDLE(phEditor)
 OR phEditor:TYPE <> "editor"
 OR phEditor:SOURCE-EDITOR <> YES THEN RETURN.

ASSIGN
 ghTargetEditor = phEditor
 ghWinToStick   = phEditor:WINDOW
 chCtrlFrame:PSTimerStickToWindow:ENABLED = YES
 {&WINDOW-NAME}:VISIBLE = YES.

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

OCXFile = SEARCH( "abhackFind.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chcfSpy = cfSpy:COM-HANDLE
    UIB_S = chcfSpy:LoadControls( OCXFile, "cfSpy":U)
    cfSpy:NAME = "cfSpy":U
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
    chCtrlFrameRed = CtrlFrameRed:COM-HANDLE
    UIB_S = chCtrlFrameRed:LoadControls( OCXFile, "CtrlFrameRed":U)
    CtrlFrameRed:NAME = "CtrlFrameRed":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "abhackFind.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject C-Win
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
APPLY 'CLOSE' TO THIS-PROCEDURE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dumpTargetEditor C-Win
PROCEDURE dumpTargetEditor :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE lModified AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lOK AS LOGICAL     NO-UNDO.

lModified = ghTargetEditor:MODIFIED.
ghTargetEditor:SAVE-FILE(gcFindFile) NO-ERROR.
ghTargetEditor:MODIFIED = lModified. /* it seems that SAVE-FILE() resets the modified attribute, it is absolutely essential to restore its value otherwize the AppBuilder might believe a source code has been saved */

lok = ghFindEdt:READ-FILE(gcFindFile) NO-ERROR.
IF NOT lOK THEN DO:
    MESSAGE "can't read " + QUOTER(gcFindFile) " !!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

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
  DISPLAY gcToFind gcScope fiEdtInfoPos fiEdtInfoFile
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnNext gcToFind gcScope fiEdtInfoPos fiEdtInfoFile btnPrev
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restartABHackTimers C-Win
PROCEDURE restartABHackTimers :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

chCtrlFrame:PSTimerStickToWindow:ENABLED = YES.
chcfSpy:PSTimerSpyEditor:ENABLED         = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restoreProfile C-Win
PROCEDURE restoreProfile :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cToFindListItems AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cScope           AS CHARACTER   NO-UNDO.

RUN getAbhackFindProfile IN ghABHackWin (OUTPUT cScope, OUTPUT cToFindListItems).

IF cScope > "" THEN gcScope = cScope.

IF cToFindListItems > "" THEN gcToFind:LIST-ITEMS IN FRAME {&FRAME-NAME} = cToFindListItems.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE searchThat C-Win
PROCEDURE searchThat :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcDirection AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cBlockName  AS CHARACTER   NO-UNDO. /* not necessary but for debugging */
DEFINE VARIABLE iMaxLine    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMinLine    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTargetCol  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTargetLine AS INTEGER     NO-UNDO.
DEFINE VARIABLE lFindable   AS LOGICAL     NO-UNDO.

ASSIGN
 iTargetLine = ghTargetEditor:CURSOR-LINE
 iTargetCol  = ghTargetEditor:CURSOR-CHAR.

IF gcScope = "local" THEN PUBLISH "getCurrentBlockBoundaries"
                                 (ghTargetEditor
                                 ,OUTPUT cBlockName
                                 ,OUTPUT iMinLine
                                 ,OUTPUT iMaxLine).

IF iMinLine = 0 THEN iMinLine = 1.
IF iMaxLine = 0 THEN iMaxLine = ghTargetEditor:NUM-LINES.

CASE pcDirection:
    WHEN 'left'  THEN DO:
/*         IF ghTargetEditor:CURSOR-CHAR  > 1 THEN ghTargetEditor:CURSOR-CHAR  = ghTargetEditor:CURSOR-CHAR  - 1. */
/*         ELSE IF ghTargetEditor:CURSOR-LINE <> 1 THEN ASSIGN                                                    */
/*          ghTargetEditor:CURSOR-LINE = ghTargetEditor:CURSOR-LINE - 1                                           */
/*          ghTargetEditor:CURSOR-CHAR  = 5000.                                                                   */
/*         ELSE ASSIGN                                                                                            */
/*          ghTargetEditor:CURSOR-LINE = ghTargetEditor:NUM-LINES                                                 */
/*          ghTargetEditor:CURSOR-CHAR  = 5000.                                                                   */

        lFindable = ghTargetEditor:SEARCH(gcToFind, FIND-PREV-OCCURRENCE + FIND-SELECT) NO-ERROR.
        IF lFindable
         AND gcScope = "local"
         AND ghTargetEditor:CURSOR-LINE < iMinLine THEN DO:
/* 18-AUG-2007 sla: If we can't find in the right scope, then just go back where you were, this is just better */
/*             ghTargetEditor:CURSOR-LINE = iMaxLine + 1 NO-ERROR.                                       */
/*             ghTargetEditor:CURSOR-CHAR = 1 NO-ERROR.                                                  */
/*             lFindable = ghTargetEditor:SEARCH(gcToFind, FIND-PREV-OCCURRENCE + FIND-SELECT) NO-ERROR. */
/*             IF ghTargetEditor:CURSOR-LINE > iMaxLine THEN lFindable = NO.                             */
            ASSIGN
             ghTargetEditor:CURSOR-LINE = iTargetLine
             ghTargetEditor:CURSOR-CHAR = iTargetCol
             lFindable                  = NO.
        END.

    END.
    WHEN 'right' THEN DO:
        IF ghTargetEditor:TEXT-SELECTED

         THEN ghTargetEditor:CURSOR-CHAR = ghTargetEditor:CURSOR-CHAR - LENGTH(ghTargetEditor:SELECTION-TEXT) NO-ERROR.

        lFindable = ghTargetEditor:SEARCH(gcToFind, FIND-NEXT-OCCURRENCE + FIND-SELECT) NO-ERROR.

        IF lFindable
         AND gcScope = "local"
         AND ghTargetEditor:CURSOR-LINE > iMaxLine THEN DO:
/* 18-AUG-2007 sla: If we can't find in the right scope, then just go back where you were, this is just better */
/*             ghTargetEditor:CURSOR-LINE = iMinLine - 1 NO-ERROR.                                       */
/*             ghTargetEditor:CURSOR-CHAR = 5000 NO-ERROR.                                               */
/*             lFindable = ghTargetEditor:SEARCH(gcToFind, FIND-NEXT-OCCURRENCE + FIND-SELECT) NO-ERROR. */
/*             IF ghTargetEditor:CURSOR-LINE > iMaxLine THEN lFindable = NO.                             */
            ASSIGN
             ghTargetEditor:CURSOR-LINE = iTargetLine
             ghTargetEditor:CURSOR-CHAR = iTargetCol
             lFindable                  = NO.
        END.
    END.
END CASE.


IF lFindable THEN  DO:
    gcToFind:BGCOLOR IN FRAME {&FRAME-NAME} = ?.
    IF VALID-HANDLE(ghABHackWin) THEN RUN loadLocalResources IN ghABHackWin (ghTargetEditor).
END.
ELSE RUN abhackFindBlinkRed.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stopABHackTimers C-Win
PROCEDURE stopABHackTimers :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

chCtrlFrame:PSTimerStickToWindow:ENABLED = NO.
chcfSpy:PSTimerSpyEditor:ENABLED         = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

