&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win
/*------------------------------------------------------------------------

  File: procEditorTooltip.w  by Sebastien Lacroix NOV-2006

A cut pseudo tooltip managed in pure 4GL in a frame that will be created in the
window of the passed iphEditor handle.  The frame will be moved-to-top() with
a little PSTimerMoveToTop timer

14-DEC-2006 sla: changed the interval of PSTimerMoveToTop so the tooltip
can popup on top of popup list
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

/* Parameters Definitions --- */
DEFINE INPUT PARAMETER iphEditor AS HANDLE      NO-UNDO.

/* Pseudo Parameters Definitions --- (Set by a first call to LoadXXX) */
DEFINE VARIABLE ipcInitBufferList   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ipcInitBufferPrefix AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ipcOptn             AS CHARACTER  NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE gcApiTooltip     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE gcRunningMode    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ghCaller         AS HANDLE     NO-UNDO. /* we will use it for various purposes like getPrevWord */
DEFINE VARIABLE ghEditorWin      AS HANDLE     NO-UNDO.
DEFINE VARIABLE glEnableUIRun    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE glSortedByName   AS LOGICAL    NO-UNDO.
DEFINE VARIABLE glTryAboveCarret AS LOGICAL    NO-UNDO.

PROCEDURE GetCaretPos EXTERNAL "user32.dll":
    DEFINE INPUT  PARAMETER HWND AS MEMPTR.
    DEFINE RETURN PARAMETER lOK  AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fEditorTooltip

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnClose

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU PMEdTooltip
       MENU-ITEM m_Copy_text_to_clipboard LABEL "Copy text to clipboard"
       MENU-ITEM mDoNotShowThisAgain LABEL "Do not show this tooltip again"
              DISABLED.


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrameEntryToEditor AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrameEntryToEditor AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrameExit AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrameExit AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose
     IMAGE-UP FILE "protools/abhack/closebutton.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 3"
     SIZE 3.2 BY .76.

DEFINE VARIABLE edTooltip AS CHARACTER
     VIEW-AS EDITOR
     SIZE 60 BY 5.95
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fEditorTooltip
     btnClose AT ROW 1 COL 10
     edTooltip AT ROW 1 COL 1 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 60 BY 6
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB)
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "simple frame no title"
         HEIGHT             = 6
         WIDTH              = 60
         MAX-HEIGHT         = 34.19
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 34.19
         VIRTUAL-WIDTH      = 204.8
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fEditorTooltip
   FRAME-NAME                                                           */
ASSIGN
       FRAME fEditorTooltip:SELECTABLE       = TRUE
       FRAME fEditorTooltip:MOVABLE          = TRUE
       FRAME fEditorTooltip:POPUP-MENU       = MENU PMEdTooltip:HANDLE.

/* SETTINGS FOR EDITOR edTooltip IN FRAME fEditorTooltip
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN
       edTooltip:READ-ONLY IN FRAME fEditorTooltip        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME




/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fEditorTooltip:HANDLE
       ROW             = 1.48
       COLUMN          = 4
       HEIGHT          = 1.43
       WIDTH           = 6
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrameEntryToEditor ASSIGN
       FRAME           = FRAME fEditorTooltip:HANDLE
       ROW             = 1.48
       COLUMN          = 13
       HEIGHT          = 1.43
       WIDTH           = 6
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrameExit ASSIGN
       FRAME           = FRAME fEditorTooltip:HANDLE
       ROW             = 1.48
       COLUMN          = 21
       HEIGHT          = 1.43
       WIDTH           = 6
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimerMoveToTop */
/* CtrlFrameEntryToEditor OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimerEntryEditor */
/* CtrlFrameExit OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimerExit */
      CtrlFrame:MOVE-AFTER(edTooltip:HANDLE IN FRAME fEditorTooltip).
      CtrlFrameEntryToEditor:MOVE-AFTER(CtrlFrame).
      CtrlFrameExit:MOVE-AFTER(CtrlFrameEntryToEditor).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* simple frame no title */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* simple frame no title */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fEditorTooltip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fEditorTooltip C-Win
ON END-ERROR OF FRAME fEditorTooltip
OR ENDKEY OF FRAME {&FRAME-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fEditorTooltip C-Win
ON ENTRY OF FRAME fEditorTooltip
DO:
  IF NOT chCtrlFrameEntryToEditor:PSTimerEntryEditor:enabled THEN
   chCtrlFrameExit:PSTimerExit:ENABLED = NO.
  FRAME fEditorTooltip:SELECTED = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME fEditorTooltip /* Button 3 */
DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimerMoveToTop.Tick .
/*------------------------------------------------------------------------------
  Purpose:     This trick is necessary to put the frame back to top after a few
               milliseconds
               Indeed, in some cases, when the frame is just realized, it
               goes to bottom when the flow of the program goes back to ade
               procedures, or when the focus goes back to the editor

  Parameters:  None required for OCX.
  Notes:
------------------------------------------------------------------------------*/

chCtrlFrame:PSTimerMoveToTop:enabled = NO.
FRAME fEditorTooltip:MOVE-TO-TOP().
iphEditor:FRAME:MOVE-TO-BOTTOM().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrameEntryToEditor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrameEntryToEditor C-Win OCX.Tick
PROCEDURE CtrlFrameEntryToEditor.PSTimerEntryEditor.Tick .
/*------------------------------------------------------------------------------
  Purpose:     This trick is necessary to put the frame back to top after a few
               milliseconds
               Indeed, in some cases, when the frame is just realized, it
               goes to bottom when the flow of the program goes back to ade
               procedures, or when the focus goes back to the editor

  Parameters:  None required for OCX.
  Notes:
------------------------------------------------------------------------------*/

chCtrlFrameEntryToEditor:PSTimerEntryEditor:enabled = NO.
APPLY 'ENTRY' TO iphEditor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrameExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrameExit C-Win OCX.Tick
PROCEDURE CtrlFrameExit.PSTimerExit.Tick .
/*------------------------------------------------------------------------------
  Purpose:     This trick is necessary to put the frame back to top after a few
               milliseconds
               Indeed, in some cases, when the frame is just realized, it
               goes to bottom when the flow of the program goes back to ade
               procedures, or when the focus goes back to the editor

  Parameters:  None required for OCX.
  Notes:
------------------------------------------------------------------------------*/

APPLY 'CLOSE' TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME edTooltip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edTooltip C-Win
ON DESELECTION OF edTooltip IN FRAME fEditorTooltip
DO:
  FRAME {&FRAME-NAME}:SELECTED = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mDoNotShowThisAgain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mDoNotShowThisAgain C-Win
ON CHOOSE OF MENU-ITEM mDoNotShowThisAgain /* Do not show this tooltip again */
DO:
  RUN disableApiTooltip IN TARGET-PROCEDURE (gcApiTooltip).
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copy_text_to_clipboard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copy_text_to_clipboard C-Win
ON CHOOSE OF MENU-ITEM m_Copy_text_to_clipboard /* Copy text to clipboard */
DO:
    CLIPBOARD:VALUE = edTooltip:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win


/* ***************************  Main Block  *************************** */


/* disable default, as we want the frame to appear in the window of the editor */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
/*ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} */
ghEditorWin = iphEditor:WINDOW.
THIS-PROCEDURE:CURRENT-WINDOW = ghEditorWin.

ghCaller = SOURCE-PROCEDURE:HANDLE. /* We will use it as Library */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE RUN disable_UI.


/* This event will help us to keep one single instance in the editor  */
SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "KillEditorTooltip" IN ghCaller.
SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "delayMoveToTop"    IN ghCaller.
/* 07-DEC-2006 sla: This subscribtion was missing */
SUBSCRIBE PROCEDURE ghCaller TO "DelayWindowEvent" IN THIS-PROCEDURE.

/*======================= Override a few triggers =========================*/
/* The pointof these tricks are to avoid weird size state or errors
 when resizing or closing the editor window while this procedure is active */
ON "WINDOW-RESIZED" OF ghEditorWin DO:
    PUBLISH "DelayWindowEvent" (ghEditorWin, "WINDOW-RESIZED").
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

ON "WINDOW-CLOSE" OF ghEditorWin DO:
    PUBLISH "DelayWindowEvent" (ghEditorWin, "WINDOW-CLOSE").
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.


/* 24-NOV-2006 sla: The point of the following code is to avoid an error on CTRL-Z or other copy/paste events
  Sadly the only way to avoid these errors is to kill the popup guys because the
  ADE intensively relies on the FIRST-CHILD attribute, and the moved-to-top() popup
  guys become the first child of the editor FRAME with a simple move-to-top()
  The only way to avoid that would be to use only dynamic frame and widgets
  for the popup guys, then I would not need to use the VIEW FRAME IN WINDOW heditor:WINDOW Statement
  04-DEC-2006 sla: the same for shift-F2 check syntax */
DEFINE VARIABLE hMenu AS HANDLE     NO-UNDO.
DEFINE VARIABLE hSubMenu AS HANDLE     NO-UNDO.
hMenu = ghEditorWin:MENU-BAR.
hSubMenu     = hMenu:FIRST-CHILD NO-ERROR. /* 14-DEC-2006 sla: NO-ERROR added for case of window without menu like POV Code Viewer */
DO WHILE hSubMenu <> ?:
    /* 04-DEC-2006 sla: nowtry to handle all submenus this way */
    IF VALID-HANDLE(hSubMenu)
     AND hSubMenu:TYPE = "SUB-MENU" THEN
     ON "MENU-DROP" OF hSubMenu DO:
        PUBLISH "DelayWindowEvent" (hSubMenu, "MENU-DROP").
        APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.
    hSubMenu = hSubMenu:NEXT-SIBLING.
END.


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  /*IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.*/

  IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    MESSAGE "Procedure" THIS-PROCEDURE:FILE-NAME "is designed to run persistenly only!" SKIP
     "About to apply close to it!"
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'CLOSE' TO THIS-PROCEDURE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

OCXFile = SEARCH( "procEditorTooltip.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
    chCtrlFrameEntryToEditor = CtrlFrameEntryToEditor:COM-HANDLE
    UIB_S = chCtrlFrameEntryToEditor:LoadControls( OCXFile, "CtrlFrameEntryToEditor":U)
    CtrlFrameEntryToEditor:NAME = "CtrlFrameEntryToEditor":U
    chCtrlFrameExit = CtrlFrameExit:COM-HANDLE
    UIB_S = chCtrlFrameExit:LoadControls( OCXFile, "CtrlFrameExit":U)
    CtrlFrameExit:NAME = "CtrlFrameExit":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "procEditorTooltip.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delayMoveToTop C-Win
PROCEDURE delayMoveToTop :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

chCtrlFrame:PSTimerMoveToTop:enabled = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableApiTooltip C-Win
PROCEDURE disableApiTooltip :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcApiTooltipToDisable AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cSection  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ckv       AS CHARACTER   NO-UNDO.

LOAD 'Software\PSC\PROGRESS\slacroixTools' BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
/* create it if it does not exist  ... well, would be surprising...*/
IF ERROR-STATUS:ERROR THEN LOAD 'Software\PSC\PROGRESS\slacroixTools' NEW BASE-KEY 'HKEY_CURRENT_USER' NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN USE 'Software\PSC\PROGRESS\slacroixTools'.

cSection = 'ABHacker'.

PUT-KEY-VALUE SECTION cSection KEY 'disabledApiTooltip' + pcApiTooltipToDisable VALUE "YES".

/* unload the environment to go back to default env */
UNLOAD 'Software\PSC\PROGRESS\slacroixTools' NO-ERROR.

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
  /* Hide all frames. */
  HIDE FRAME fEditorTooltip.
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
  RUN control_load.
  ENABLE btnClose
      WITH FRAME fEditorTooltip.
  {&OPEN-BROWSERS-IN-QUERY-fEditorTooltip}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KillEditorTooltip C-Win
PROCEDURE KillEditorTooltip :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER hEditor AS HANDLE     NO-UNDO.

IF hEditor <> iphEditor THEN RETURN.  /* let's allow multiple instances to run, but in multiple editors */

APPLY 'CLOSE' TO THIS-PROCEDURE.

APPLY 'ENTRY' TO iphEditor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadText C-Win
PROCEDURE loadText :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcText AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pcOptn AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cLine      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE eOptn      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iLine      AS INTEGER    NO-UNDO.
DEFINE VARIABLE iLineWidth AS INTEGER    NO-UNDO.
DEFINE VARIABLE iMaxWidth  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNumLines  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iOptn      AS INTEGER    NO-UNDO.

ipcOptn = pcOptn.
pcText = REPLACE(pcText, "~~n", "~n").
iNumLines = NUM-ENTRIES(pcText, "~n").

DO iline = 1 TO iNumLines:
    cLine = ENTRY(iLine, pcText, "~n").
    iLineWidth = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(cLine, edTooltip:FONT IN FRAME {&FRAME-NAME}).
    iMaxWidth = MAXIMUM(iLineWidth, iMaxWidth).
END.

iMaxWidth = MINIMUM(iMaxWidth + 12, MAXIMUM(100, iphEditor:WIDTH-PIXELS / 1.3), 500).

edTooltip:SCREEN-VALUE IN FRAME fEditorTooltip = pcText.

/* now manage the vertical size and position the guys */
RUN resizeAndLocatePopup (iMaxWidth).

IF edTooltip:INNER-LINES < NUM-ENTRIES(pcText, "~n") THEN
 edTooltip:TOOLTIP = pcText.

DO iOptn = 1 TO NUM-ENTRIES(ipcOptn):
    eOptn = ENTRY(iOptn, ipcOptn).
    IF eOptn MATCHES "visibleTime=*"
     THEN chCtrlFrameExit:PSTimerExit:INTERVAL = INTEGER(ENTRY(2, eOptn, "=")).

    IF eOptn MATCHES "canDisableApiTooltip=*" THEN ASSIGN
     gcApiTooltip = ENTRY(2, eOptn, "=")
     MENU-ITEM mDoNotShowThisAgain:SENSITIVE IN MENU PMedTooltip = YES.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeAndLocatePopup C-Win
PROCEDURE resizeAndLocatePopup :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER piMaxWidth AS INTEGER    NO-UNDO.

DEFINE VARIABLE hFrame           AS HANDLE   NO-UNDO.
DEFINE VARIABLE iHeight          AS INTEGER  NO-UNDO.
DEFINE VARIABLE iMaxVisibleLines AS INTEGER  NO-UNDO.
DEFINE VARIABLE iOK              AS INTEGER  NO-UNDO.
DEFINE VARIABLE iSpaceAbove      AS INTEGER  NO-UNDO.
DEFINE VARIABLE iSpaceBellow     AS INTEGER  NO-UNDO.
DEFINE VARIABLE iTextHeight      AS INTEGER  NO-UNDO.
DEFINE VARIABLE iX               AS INTEGER  NO-UNDO.
DEFINE VARIABLE iY               AS INTEGER  NO-UNDO.
DEFINE VARIABLE lBelow           AS LOGICAL  NO-UNDO.
DEFINE VARIABLE mPoint           AS MEMPTR   NO-UNDO.

/* 22-DEC-2006 sla: doing the GetCaretPos job before calling VIEW FRAME in WINDOW makes it work find on Linux with Wine */
/* Get position of the caret (text cursor) */
SET-SIZE(mPoint) = 8.
RUN GetCaretPos (mPoint, OUTPUT iOK).
iX = GET-LONG(mPoint, 1).
iY = GET-LONG(mPoint, 5).
SET-SIZE(mPoint) = 0.

/* now its time to realize the guys */
VIEW FRAME fEditorTooltip IN WINDOW iphEditor:WINDOW.
hFrame = FRAME fEditorTooltip:HANDLE. /* more convenient */
/* hFrame:FRAME = iphEditor:FRAME. */
hFrame:VISIBLE = NO.

/* 14-DEC-2006 sla: new TryAboveCarret feature */
IF INDEX(ipcOptn, "TryAboveCarret=") > 0
 THEN glTryAboveCarret = INDEX(ipcOptn, "TryAboveCarret=YES") > 0.
ELSE glTryAboveCarret = ?.


/* 02-OCT-2007 sla: fix bug when a very small tooltip comes (like unknown value) */
piMaxWidth = MAXIMUM(btnClose:X + btnClose:WIDTH-PIXELS + 2, piMaxWidth).

/* resize the list and its frame horizontally now to fit with the items */
IF piMaxWidth > hFrame:WIDTH-PIXELS THEN ASSIGN
 hFrame:WIDTH-PIXELS          = piMaxWidth
 hFrame:VIRTUAL-WIDTH-PIXELS  = piMaxWidth
 edTooltip:WIDTH-PIXELS       = piMaxWidth.
IF piMaxWidth < hFrame:WIDTH-PIXELS THEN ASSIGN
 edTooltip:WIDTH-PIXELS       = piMaxWidth
 hFrame:WIDTH-PIXELS          = piMaxWidth
 hFrame:VIRTUAL-WIDTH-PIXELS  = piMaxWidth.


iMaxVisibleLines = edTooltip:NUM-LINES.

/* Work out height of a line, and available space above and bellow */
iTextHeight  = FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(edTooltip:FONT IN FRAME fEditorTooltip).
iSpaceAbove  = iY - iphEditor:Y - iTextHeight - 2.  /* 14-DEC-2006 sla: was doing - 4   -2 seems to be enough */
iSpaceBellow = iphEditor:HEIGHT-PIXELS - iY - iTextHeight - 11. /* 14-DEC-2006 sla: was doing '- 4' before.  Got an error so make it '- 11' today' */

/* Try to make it fit bellow if possible */
IF iSpaceBellow / iTextHeight >= iMaxVisibleLines
 AND glTryAboveCarret = NO /*  this takes care of ? */
 THEN lBelow = YES.
/* otherwize put it above if possible */
ELSE IF iSpaceAbove / iTextHeight > iMaxVisibleLines
 AND glTryAboveCarret = YES /* this takes care of ? */
 THEN lBelow = NO.
/* Otherwize put it in hightest place */
ELSE DO:
    IF iSpaceBellow > iSpaceAbove THEN ASSIGN
     lBelow = YES             /* 14-DEC-2006 sla: for iMaxVisibleLines No need to work out a higher value than the original hEditor:NUM-LINES */
     iMaxVisibleLines = MINIMUM(iMaxVisibleLines, MAXIMUM(1, TRUNCATE(iSpaceBellow / iTextHeight, 0))).
    ELSE ASSIGN
     lBelow = NO
     iMaxVisibleLines = MINIMUM(iMaxVisibleLines, MAXIMUM(1, TRUNCATE(iSpaceAbove / iTextHeight, 0))).
END.

/* we cannot do that when the widget has already been realised...
IF iMaxVisibleLines < edTooltip:NUM-LINES THEN edTooltip:SCROLLBAR-VERTICAL = YES.*/

iHeight = iTextHeight * iMaxVisibleLines + 8.
iHeight = MAXIMUM(btnClose:HEIGHT-PIXELS + btnClose:Y, iHeight).

/* Now resize the editor and its frame */
IF iHeight > hFrame:HEIGHT-PIXELS THEN ASSIGN
 hFrame:HEIGHT-PIXELS         = iHeight
 hFrame:VIRTUAL-HEIGHT-PIXELS = iHeight
 edTooltip:HEIGHT-PIXELS      = iHeight.
IF iHeight < hFrame:HEIGHT-PIXELS THEN ASSIGN
 edTooltip:HEIGHT-PIXELS      = iHeight
 hFrame:HEIGHT-PIXELS         = iHeight
 hFrame:VIRTUAL-HEIGHT-PIXELS = iHeight.


IF lBelow THEN hFrame:Y = iY + iTextHeight + iphEditor:Y + 4.
ELSE hFrame:Y = iphEditor:Y + iY - iHeight - 1.

/* at last, position this guys above or bellow the caret, as determined above */
IF hFrame:WINDOW:VIRTUAL-WIDTH-PIXELS < iX + iphEditor:X + hFrame:WIDTH-PIXELS
 THEN hFrame:WINDOW:VIRTUAL-WIDTH-PIXELS = iX + iphEditor:X + hFrame:WIDTH-PIXELS.
hFrame:X = iX + iphEditor:X.
/*
IF hFrame:X <> iX + iphEditor:X /* happens when the frame could not fit in the editor */
 THEN hFrame:X = iphEditor:X + iphEditor:WIDTH-PIXELS - hFrame:WIDTH-PIXELS + 1.
*/
/* if too far on the right, then provoque scroll to the right ;) */
IF hFrame:X + hFrame:WIDTH-PIXELS > iphEditor:X + iphEditor:WIDTH-PIXELS
 AND CAN-DO(ipcOptn, "ScrollHoriz")
 THEN DO:
/*IF hFrame:X <> iX + iphEditor:X THEN DO:*/
    DEFINE VARIABLE iHowMuchToScroll AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iAttempt         AS INTEGER     NO-UNDO.
    DO iAttempt = 2 TO edTooltip:WIDTH-CHAR + 2 BY 3: /* let's give it a limit, by 3 will make it faster */
        /* go by iAttempt characters on the right to provoque a right scroll */
        DO iHowMuchToScroll = 1 TO iAttempt:
            iphEditor:source-command('CURSOR-RIGHT', '').
        END.
        /* then put the cursor where it was */
        DO iHowMuchToScroll = 1 TO iAttempt:
            iphEditor:source-command('CURSOR-LEFT', '').
        END.
        /* move the guys to new location of the cursor */
        SET-SIZE(mPoint) = 8.
        RUN GetCaretPos(mPoint, OUTPUT iOK).
        iX = GET-LONG(mPoint, 1).
        SET-SIZE(mPoint) = 0.

        hFrame:X = iX + iphEditor:X NO-ERROR.

        IF hFrame:X <> iX + + iphEditor:X THEN NEXT.
        IF hFrame:X + hFrame:WIDTH-PIXELS <= iphEditor:X + iphEditor:WIDTH-PIXELS THEN LEAVE.
    END.
END.
ELSE IF hFrame:X + hFrame:WIDTH-PIXELS > iphEditor:X + iphEditor:WIDTH-PIXELS
 THEN hFrame:X = MAXIMUM(1, iphEditor:X + iphEditor:WIDTH-PIXELS - hFrame:WIDTH-PIXELS).

hFrame:VISIBLE = YES.

/* 15-NOV-2006 sla: get rid off scrollbars */
ASSIGN
 hFrame:VIRTUAL-WIDTH-PIXELS  = hFrame:WIDTH-PIXELS
 hFrame:VIRTUAL-HEIGHT-PIXELS = hFrame:HEIGHT-PIXELS.

btnClose:X = hFrame:WIDTH-PIXELS - btnClose:WIDTH-PIXELS - 1.

IF NOT glEnableUIRun THEN DO:
    RUN enable_UI.
    glEnableUIRun = YES.
END.
btnClose:MOVE-TO-TOP().


PUBLISH "delayMoveToTop" FROM ghCaller.
/*chCtrlFrame:PSTimerMoveToTop:enabled = YES.*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

