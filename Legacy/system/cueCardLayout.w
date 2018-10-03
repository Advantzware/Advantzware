&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: system/cueCardLayout.w

  Description: Help Cue Card

  Input Parameters: Buffer cueCardText
  
  Output Parameters: <none>

  Author: Ron Stark

  Created: 9.28.2018

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

DEFINE PARAMETER BUFFER cueCardText FOR cueCardText.
DEFINE INPUT PARAMETER  iphFrame AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE iOrientation AS INTEGER   NO-UNDO.
DEFINE VARIABLE cOrientation AS CHARACTER NO-UNDO INITIAL
    "default_LeftUp,default_RightUp,default_RightDown,default_LeftDown".
DEFINE VARIABLE iPosition    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPosition    AS CHARACTER NO-UNDO INITIAL
    "arrow_join,arrow_right,arrow_cross,arrow_down".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnClose positionImage arrowImage nextCue ~
prevCue dontShowAgain cCueText gotIt 
&Scoped-Define DISPLAYED-OBJECTS dontShowAgain cCueText gotIt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btnClose 
&Scoped-define List-2 btnClose 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose 
     IMAGE-UP FILE "Graphics/16x16/delete.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Close" 
     SIZE 4.2 BY 1 TOOLTIP "Close".

DEFINE VARIABLE cCueText AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 44 BY 4.29 NO-UNDO.

DEFINE IMAGE arrowImage
     FILENAME "Graphics/24x24/default_leftup.gif":U TRANSPARENT
     SIZE 7 BY 1.67.

DEFINE IMAGE nextCue
     FILENAME "Graphics/24x24/default_sidebarexpand.gif":U TRANSPARENT
     SIZE 5 BY 1.19.

DEFINE IMAGE positionImage
     FILENAME "Graphics/24x24/arrow_join.png":U TRANSPARENT
     SIZE 5 BY 1.19.

DEFINE IMAGE prevCue
     FILENAME "Graphics/24x24/default_sidebarcollapse.gif":U TRANSPARENT
     SIZE 5 BY 1.19.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 53 BY 7.38.

DEFINE VARIABLE dontShowAgain AS LOGICAL INITIAL no 
     LABEL "Don't Show Again" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.6 BY .81 NO-UNDO.

DEFINE VARIABLE gotIt AS LOGICAL INITIAL no 
     LABEL "Got It" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnClose AT ROW 1.24 COL 48 HELP
          "Close" WIDGET-ID 72
     dontShowAgain AT ROW 1.24 COL 19 WIDGET-ID 38
     cCueText AT ROW 2.67 COL 9 NO-LABEL WIDGET-ID 10
     gotIt AT ROW 7.19 COL 26 WIDGET-ID 16
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 42
     positionImage AT ROW 1.24 COL 9 WIDGET-ID 44
     arrowImage AT ROW 1.24 COL 2 WIDGET-ID 2
     nextCue AT ROW 6.95 COL 48 WIDGET-ID 12
     prevCue AT ROW 6.95 COL 9 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 53.2 BY 7.43
         FGCOLOR 1  WIDGET-ID 100.


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
         TITLE              = "Cue Card Layout"
         HEIGHT             = 7.43
         WIDTH              = 53.2
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
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
       arrowImage:SELECTABLE IN FRAME DEFAULT-FRAME       = TRUE
       arrowImage:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

/* SETTINGS FOR BUTTON btnClose IN FRAME DEFAULT-FRAME
   1 2                                                                  */
ASSIGN 
       cCueText:SELECTABLE IN FRAME DEFAULT-FRAME       = TRUE
       cCueText:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE
       cCueText:RESIZABLE IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       dontShowAgain:SELECTABLE IN FRAME DEFAULT-FRAME       = TRUE
       dontShowAgain:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

ASSIGN 
       gotIt:SELECTABLE IN FRAME DEFAULT-FRAME       = TRUE
       gotIt:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

ASSIGN 
       nextCue:SELECTABLE IN FRAME DEFAULT-FRAME       = TRUE
       nextCue:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

ASSIGN 
       positionImage:SELECTABLE IN FRAME DEFAULT-FRAME       = TRUE
       positionImage:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

ASSIGN 
       prevCue:SELECTABLE IN FRAME DEFAULT-FRAME       = TRUE
       prevCue:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       RECT-1:RESIZABLE IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Cue Card Layout */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Cue Card Layout */
DO:
  /* This event will close the window and terminate the procedure.  */
    MESSAGE
        "Save this Cue Card?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
    UPDATE lSave AS LOGICAL.
    IF lSave THEN
    RUN pSave.
    ELSE IF lSave EQ ? THEN
    RETURN NO-APPLY.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Cue Card Layout */
DO:
    RUN pReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME arrowImage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arrowImage C-Win
ON MOUSE-SELECT-CLICK OF arrowImage IN FRAME DEFAULT-FRAME
DO:
    iOrientation = iOrientation + 1.
    IF iOrientation GT 4 THEN
    iOrientation = 1.
    arrowImage:LOAD-IMAGE("Graphics\24x24\" + ENTRY(iOrientation,cOrientation) + ".gif").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME positionImage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL positionImage C-Win
ON MOUSE-SELECT-CLICK OF positionImage IN FRAME DEFAULT-FRAME
DO:
    iPosition = iPosition + 1.
    IF iPosition GT 4 THEN
    iPosition = 1.
    positionImage:LOAD-IMAGE("Graphics\24x24\" + ENTRY(iPosition,cPosition) + ".png").
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
  ASSIGN
    FRAME {&FRAME-NAME}:GRID-SNAP    = YES
    FRAME {&FRAME-NAME}:GRID-VISIBLE = YES
    .
  RUN pDisplayCueCardLayout.
  RUN enable_UI.
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
  DISPLAY dontShowAgain cCueText gotIt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnClose positionImage arrowImage nextCue prevCue dontShowAgain 
         cCueText gotIt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayCueCardLayout C-Win 
PROCEDURE pDisplayCueCardLayout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dCol AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dRow AS DECIMAL NO-UNDO.
    
    /* calculate the cue card screen position */
    CASE cueCardText.cuePosition:
        /* arrows: 1=absolute */
        WHEN 1 THEN
        ASSIGN 
            dCol = cueCardText.frameCol
            dRow = cueCardText.frameRow + 1
            .
        /* arrows: 2=width */
        WHEN 2 THEN 
        ASSIGN 
            dCol = iphFrame:WIDTH - cueCardText.frameCol
            dRow = cueCardText.frameRow + 1
            .
        /* arrows: 3=height & width */
        WHEN 3 THEN 
        ASSIGN 
            dCol = iphFrame:WIDTH  - cueCardText.frameCol
            dRow = iphFrame:HEIGHT - cueCardText.frameRow + 1
            .
        /* arrows: 4=height */
        WHEN 4 THEN  
        ASSIGN 
            dCol = cueCardText.frameCol
            dRow = iphFrame:HEIGHT - cueCardText.frameRow + 1
            .
    END CASE.
    ASSIGN 
        dCol                               = dCol + iphFrame:COL - 1
        dRow                               = dRow + iphFrame:ROW - 1
        {&WINDOW-NAME}:COL                 = dCol
        {&WINDOW-NAME}:ROW                 = dRow
        {&WINDOW-NAME}:HEIGHT              = cueCardText.frameHeight
        {&WINDOW-NAME}:WIDTH               = cueCardText.frameWidth
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
        FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
        FRAME {&FRAME-NAME}:WIDTH          = {&WINDOW-NAME}:WIDTH
        FRAME {&FRAME-NAME}:FGCOLOR        = cueCardText.frameFGColor
        FRAME {&FRAME-NAME}:BGCOLOR        = cueCardText.frameBGColor
        btnClose:COL                       = FRAME {&FRAME-NAME}:WIDTH - 4.4
        btnClose:ROW                       = 1.24
        RECT-1:HEIGHT                      = cueCardText.frameHeight
        RECT-1:WIDTH                       = cueCardText.frameWidth
        cCueText:COL                       = cueCardText.textCol
        cCueText:ROW                       = cueCardText.textRow
        cCueText:HEIGHT                    = cueCardText.textHeight
        cCueText:WIDTH                     = cueCardText.textWidth
        cCueText:FONT                      = cueCardText.textFont
        cCueText                           = cueCardText.cueText
        arrowImage:COL                     = cueCardText.arrowCol
        arrowImage:ROW                     = cueCardText.arrowRow
        iOrientation                       = cueCardText.cueOrientation
        positionImage:COL                  = cueCardText.positionCol
        positionImage:ROW                  = cueCardText.positionRow
        iPosition                          = cueCardText.cuePosition
        prevCue:COL                        = cueCardText.prevCol
        prevCue:ROW                        = cueCardText.prevRow
        nextCue:COL                        = cueCardText.nextCol
        nextCue:ROW                        = cueCardText.nextRow
        dontShowAgain:COL                  = cueCardText.dontShowAgainCol
        dontShowAgain:ROW                  = cueCardText.dontShowAgainRow
        dontShowAgain:FONT                 = cueCardText.dontShowAgainFont
        gotIt:COL                          = cueCardText.gotItCol
        gotIt:ROW                          = cueCardText.gotItRow
        gotIt:FONT                         = cueCardText.gotItFont
        .
    arrowImage:LOAD-IMAGE("Graphics\24X24\" + ENTRY(iOrientation,cOrientation) + ".gif").
    positionImage:LOAD-IMAGE("Graphics\24x24\" + ENTRY(iPosition,cPosition) + ".png").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReSize C-Win 
PROCEDURE pReSize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget   AS HANDLE  NO-UNDO.
    DEFINE VARIABLE dMinHeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMinWidth  AS DECIMAL NO-UNDO.
    
    ASSIGN 
        hWidget = FRAME {&FRAME-NAME}:HANDLE
        hWidget = hWidget:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD  
        .
    DO WHILE VALID-HANDLE(hWidget):
        IF hWidget:TYPE NE "Rectangle" AND
           hWidget:NAME NE "btnClose"  THEN
        ASSIGN 
            dMinHeight = MAX(dMinHeight, hWidget:ROW + hWidget:HEIGHT)
            dMinWidth  = MAX(dMinWidth,  hWidget:COL + hWidget:WIDTH)
            .
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */
    dMinHeight = dMinHeight - .8.
    
    DO WITH FRAME {&FRAME-NAME}:
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT dMinHeight THEN
        {&WINDOW-NAME}:HEIGHT = dMinHeight.
        IF {&WINDOW-NAME}:WIDTH  LT dMinWidth  THEN
        {&WINDOW-NAME}:WIDTH  = dMinWidth.
        ASSIGN
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH  = {&WINDOW-NAME}:WIDTH
            btnClose:COL  = FRAME {&FRAME-NAME}:WIDTH - 4.4
            btnClose:ROW  = 1.24
            RECT-1:HEIGHT = {&WINDOW-NAME}:HEIGHT
            RECT-1:WIDTH  = {&WINDOW-NAME}:WIDTH
            .
        VIEW FRAME {&FRAME-NAME}.
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSave C-Win 
PROCEDURE pSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dCol AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dRow AS DECIMAL NO-UNDO.
    
    /* calculate the cue card screen position */
    CASE iPosition:
        /* arrows: 1=absolute */
        WHEN 1 THEN
        ASSIGN 
            dCol = {&WINDOW-NAME}:COL
            dRow = {&WINDOW-NAME}:ROW
            .
        /* arrows: 2=width */
        WHEN 2 THEN 
        ASSIGN 
            dCol = iphFrame:WIDTH - {&WINDOW-NAME}:COL
            dRow = {&WINDOW-NAME}:ROW
            .
        /* arrows: 3=height & width */
        WHEN 3 THEN 
        ASSIGN 
            dCol = iphFrame:WIDTH  - {&WINDOW-NAME}:COL
            dRow = iphFrame:HEIGHT - {&WINDOW-NAME}:ROW
            .
        /* arrows: 4=height */
        WHEN 4 THEN  
        ASSIGN 
            dCol = {&WINDOW-NAME}:COL
            dRow = iphFrame:HEIGHT - {&WINDOW-NAME}:ROW
            .
    END CASE.
    ASSIGN 
        dCol = dCol - iphFrame:COL
        dRow = dRow - iphFrame:ROW
        . 
    IF dCol LT 1 THEN dCol = 1.
    IF dRow LT 1 THEN dRow = 1.

    FIND CURRENT cueCardText EXCLUSIVE-LOCK.
    ASSIGN
        cueCardText.frameCol          = dCol
        cueCardText.frameRow          = dRow
        cueCardText.frameHeight       = FRAME {&FRAME-NAME}:HEIGHT
        cueCardText.frameWidth        = FRAME {&FRAME-NAME}:WIDTH
        cueCardText.frameFGColor      = FRAME {&FRAME-NAME}:FGCOLOR
        cueCardText.frameBGColor      = FRAME {&FRAME-NAME}:BGCOLOR
        cueCardText.textCol           = cCueText:COL      
        cueCardText.textRow           = cCueText:ROW      
        cueCardText.textHeight        = cCueText:HEIGHT   
        cueCardText.textWidth         = cCueText:WIDTH    
        cueCardText.textFont          = cCueText:FONT     
        cueCardText.cueText           = cCueText:SCREEN-VALUE
        cueCardText.arrowCol          = arrowImage:COL    
        cueCardText.arrowRow          = arrowImage:ROW    
        cueCardText.cueOrientation    = iOrientation      
        cueCardText.positionCol       = positionImage:COL 
        cueCardText.positionRow       = positionImage:ROW 
        cueCardText.cuePosition       = iPosition         
        cueCardText.prevCol           = prevCue:COL       
        cueCardText.prevRow           = prevCue:ROW       
        cueCardText.nextCol           = nextCue:COL       
        cueCardText.nextRow           = nextCue:ROW       
        cueCardText.dontShowAgainCol  = dontShowAgain:COL 
        cueCardText.dontShowAgainRow  = dontShowAgain:ROW 
        cueCardText.dontShowAgainFont = dontShowAgain:FONT
        cueCardText.gotItCol          = gotIt:COL         
        cueCardText.gotItRow          = gotIt:ROW         
        cueCardText.gotItFont         = gotIt:FONT        
        .
    FIND CURRENT cueCardText NO-LOCK.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

