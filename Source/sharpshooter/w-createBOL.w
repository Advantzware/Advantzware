&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: sharpshooter/w-createBol.w

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Do not remove these shared variables, as these are required for OrderProcs.p which called later */
{custom/globdefs.i}
{sys/inc/var.i "NEW SHARED"}
{sys/inc/varasgn.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompanyName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lShowTrailerRelease AS LOGICAL NO-UNDO.
DEFINE VARIABLE lShowTrailerTag     AS LOGICAL NO-UNDO.

/* Required for run_link.i */
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.

DEFINE VARIABLE gcScanTrailer                AS CHARACTER NO-UNDO INITIAL "None".
DEFINE VARIABLE gcSSBOLPrint                 AS CHARACTER NO-UNDO INITIAL "DoNotPrint".
DEFINE VARIABLE gcSSReleaseTrailerValidation AS CHARACTER NO-UNDO INITIAL "Error".
DEFINE VARIABLE glShowVirtualKeyboard        AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE gcShowVirtualKeyboard        AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcShowFGItemInquiry          AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcShowSettings               AS CHARACTER NO-UNDO.

DEFINE VARIABLE oLoadTag  AS Inventory.LoadTag     NO-UNDO.
DEFINE VARIABLE oKeyboard AS system.Keyboard       NO-UNDO.
DEFINE VARIABLE oReleaseHeader AS oe.ReleaseHeader NO-UNDO.
DEFINE VARIABLE oTrailer       AS ar.Truck         NO-UNDO.

ASSIGN
    oLoadTag  = NEW Inventory.LoadTag()
    oKeyboard = NEW system.Keyboard()
    oTrailer  = NEW ar.Truck()
    .

RUN spSetSettingContext.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btClear fiTag btnKeyboardTrailerRelease ~
btnKeyboardTrailer btnKeyboardTag btReset btnNumPad btDelete btPrintBOL ~
btnExitText btnClearText btnViewInquiryText btnDeleteText btnSettingsText ~
btnPrintBOLText 
&Scoped-Define DISPLAYED-OBJECTS fiTrailerRelease fiTag fiTrailerTag ~
fiTagTrailerMessage btnExitText btnClearText btnViewInquiryText ~
btnDeleteText statusMessage btnSettingsText btnPrintBOLText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_adjustwindowsize AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-releaseitems AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-releasetags AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatefirst AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatefirst-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatelast AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatelast-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatenext AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatenext-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigateprev AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigateprev-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_releasefilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_setting AS HANDLE NO-UNDO.
DEFINE VARIABLE h_viewfginquiry AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btChange 
     IMAGE-UP FILE "Graphics/32x32/pencil.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Change" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btClear 
     IMAGE-UP FILE "Graphics/32x32/back_white.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/back_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btDelete 
     IMAGE-UP FILE "Graphics/32x32/garbage_can.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 8 BY 1.91 TOOLTIP "Delete Selected Tag".

DEFINE BUTTON btnKeyboardTag 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnKeyboardTrailer 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnKeyboardTrailerRelease 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btPrintBOL 
     IMAGE-UP FILE "Graphics/32x32/print_new.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Print BOL" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btReset 
     IMAGE-UP FILE "Graphics/32x32/back_white.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/back_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91.

DEFINE VARIABLE btnClearText AS CHARACTER FORMAT "X(256)":U INITIAL "RESET" 
      VIEW-AS TEXT 
     SIZE 12 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnDeleteText AS CHARACTER FORMAT "X(256)":U INITIAL "DELETE" 
      VIEW-AS TEXT 
     SIZE 15 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnExitText AS CHARACTER FORMAT "X(256)":U INITIAL "EXIT" 
      VIEW-AS TEXT 
     SIZE 8 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnPrintBOLText AS CHARACTER FORMAT "X(256)":U INITIAL "PRINT BOL" 
      VIEW-AS TEXT 
     SIZE 20 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnSettingsText AS CHARACTER FORMAT "X(256)":U INITIAL "SETTINGS" 
      VIEW-AS TEXT 
     SIZE 18 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnViewInquiryText AS CHARACTER FORMAT "X(256)":U INITIAL "VIEW INQUIRY" 
      VIEW-AS TEXT 
     SIZE 26 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE fiTag AS CHARACTER FORMAT "X(256)":U 
     LABEL "TAG" 
     VIEW-AS FILL-IN 
     SIZE 64.2 BY 1.38 TOOLTIP "Enter Tag #"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiTagTrailerMessage AS CHARACTER FORMAT "X(256)":U INITIAL "(VALIDATE WITH RELEASE TRAILER)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiTrailerRelease AS CHARACTER FORMAT "X(256)":U 
     LABEL "TRAILER" 
     VIEW-AS FILL-IN 
     SIZE 27.4 BY 1.38 TOOLTIP "Enter Trailer #"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiTrailerTag AS CHARACTER FORMAT "X(256)":U 
     LABEL "TRAILER" 
     VIEW-AS FILL-IN 
     SIZE 27.4 BY 1.38 TOOLTIP "Enter Trailer #"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE statusMessage AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS MESSAGE" 
      VIEW-AS TEXT 
     SIZE 116 BY 1.43 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btClear AT ROW 3.19 COL 194.8 WIDGET-ID 146
     fiTrailerRelease AT ROW 2.91 COL 119.2 COLON-ALIGNED WIDGET-ID 140
     fiTag AT ROW 4.81 COL 20.2 COLON-ALIGNED WIDGET-ID 8
     fiTrailerTag AT ROW 4.81 COL 119.2 COLON-ALIGNED WIDGET-ID 10
     fiTagTrailerMessage AT ROW 6.24 COL 101.8 COLON-ALIGNED NO-LABEL WIDGET-ID 150
     btnKeyboardTrailerRelease AT ROW 2.86 COL 149.2 WIDGET-ID 144 NO-TAB-STOP 
     btnKeyboardTrailer AT ROW 4.81 COL 149.2 WIDGET-ID 138 NO-TAB-STOP 
     btnKeyboardTag AT ROW 4.81 COL 95.2 WIDGET-ID 136 NO-TAB-STOP 
     btReset AT ROW 4.57 COL 87.2 WIDGET-ID 18
     btnNumPad AT ROW 2.86 COL 160.2 WIDGET-ID 120 NO-TAB-STOP 
     btDelete AT ROW 31.71 COL 17 WIDGET-ID 16 NO-TAB-STOP 
     btChange AT ROW 2.67 COL 64.4 WIDGET-ID 14 NO-TAB-STOP 
     btPrintBOL AT ROW 31.71 COL 195 WIDGET-ID 12 NO-TAB-STOP 
     btnExitText AT ROW 1.24 COL 187 NO-LABEL WIDGET-ID 24
     btnClearText AT ROW 3.62 COL 182 NO-LABEL WIDGET-ID 148
     btnViewInquiryText AT ROW 5.05 COL 162.2 NO-LABEL WIDGET-ID 26
     btnDeleteText AT ROW 31.95 COL 2 NO-LABEL WIDGET-ID 20
     statusMessage AT ROW 31.95 COL 26.2 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     btnSettingsText AT ROW 31.95 COL 146 NO-LABEL WIDGET-ID 142
     btnPrintBOLText AT ROW 31.95 COL 173 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     RECT-2 AT ROW 2.67 COL 159.2 WIDGET-ID 130
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202.2 BY 32.62
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Create BOL"
         HEIGHT             = 32.62
         WIDTH              = 202.2
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = yes
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("Graphics/asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics/asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btChange IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btChange:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN btnClearText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN btnDeleteText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN btnExitText IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       btnKeyboardTag:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboardTrailer:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboardTrailerRelease:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN btnSettingsText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN btnViewInquiryText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiTagTrailerMessage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTrailerRelease IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTrailerTag IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN statusMessage IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Create BOL */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Create BOL */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btChange
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btChange W-Win
ON CHOOSE OF btChange IN FRAME F-Main /* Change */
DO:
    RUN pStatusMessage ("", 0).
    RUN pInvalidRelease.
    {methods/run_link.i "RELEASE-SOURCE" "EmptyRelease"}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btClear W-Win
ON CHOOSE OF btClear IN FRAME F-Main /* Reset */
DO:
    RUN pStatusMessage ("", 0).
    {methods/run_link.i "REL-ITEMS-SOURCE" "EmptyReleaseItems"}

    {methods/run_link.i "REL-TAGS-SOURCE" "EmptyReleaseTags"}
    
    RUN pInvalidRelease.
    
    {methods/run_link.i "RELEASE-SOURCE" "EmptyRelease"} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete W-Win
ON CHOOSE OF btDelete IN FRAME F-Main /* Delete */
DO:
    {methods/run_link.i "REL-TAGS-SOURCE" "DeleteReleaseTag"}    
    RUN pScanRelease.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearText W-Win
ON MOUSE-SELECT-CLICK OF btnClearText IN FRAME F-Main
DO:
    APPLY "CHOOSE" TO btClear.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeleteText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteText W-Win
ON MOUSE-SELECT-CLICK OF btnDeleteText IN FRAME F-Main
DO:
  APPLY "CHOOSE":U TO btDelete.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExitText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExitText W-Win
ON MOUSE-SELECT-CLICK OF btnExitText IN FRAME F-Main
DO:
    RUN dispatch ("exit").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboardTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardTag W-Win
ON CHOOSE OF btnKeyboardTag IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiTag.    
    
    oKeyboard:OpenKeyboardOverride (fiTag:HANDLE, "Qwerty"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboardTrailer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardTrailer W-Win
ON CHOOSE OF btnKeyboardTrailer IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiTrailerTag.    
    
    oKeyboard:OpenKeyboardOverride (fiTrailerTag:HANDLE, "Qwerty"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboardTrailerRelease
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardTrailerRelease W-Win
ON CHOOSE OF btnKeyboardTrailerRelease IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiTrailerRelease.    
    
    oKeyboard:OpenKeyboardOverride (fiTrailerRelease:HANDLE, "Qwerty"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNumPad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNumPad W-Win
ON CHOOSE OF btnNumPad IN FRAME F-Main /* NumPad */
DO:
    ASSIGN
        oKeyboard:DisplayKeyboard = NOT oKeyboard:DisplayKeyboard
        RECT-2:BGCOLOR = IF oKeyboard:DisplayKeyboard THEN 10 ELSE 12
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrintBOLText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrintBOLText W-Win
ON MOUSE-SELECT-CLICK OF btnPrintBOLText IN FRAME F-Main
DO:
    APPLY "CHOOSE":U TO btPrintBOL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSettingsText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSettingsText W-Win
ON MOUSE-SELECT-CLICK OF btnSettingsText IN FRAME F-Main
DO:
    RUN OpenSetting.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnViewInquiryText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnViewInquiryText W-Win
ON MOUSE-SELECT-CLICK OF btnViewInquiryText IN FRAME F-Main
DO:
    RUN pChooseBtViewFGInquiry IN h_viewfginquiry.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrintBOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrintBOL W-Win
ON CHOOSE OF btPrintBOL IN FRAME F-Main /* Print BOL */
DO:
    RUN state-changed (THIS-PROCEDURE, "print-bol").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReset W-Win
ON CHOOSE OF btReset IN FRAME F-Main /* Reset */
DO:
    fiTag:SCREEN-VALUE = "".    
    APPLY "ENTRY" TO fiTag.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON ENTRY OF fiTag IN FRAME F-Main /* TAG */
DO:
    SELF:SET-SELECTION (1, -1).
    ASSIGN
        fiTrailerTag:BGCOLOR = 15
        SELF:BGCOLOR         = 30
        .
    
    oKeyboard:OpenKeyboard (SELF, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON LEAVE OF fiTag IN FRAME F-Main /* TAG */
DO:
    DEFINE VARIABLE lValidTag AS LOGICAL NO-UNDO.

    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
    
    IF LASTKEY EQ -1 AND (VALID-OBJECT (oKeyboard) AND oKeyboard:LastKeyClicked NE "TAB" AND oKeyboard:LastKeyClicked NE "ENTER" ) THEN
        RETURN.
        
    IF SELF:SCREEN-VALUE EQ "EXIT" THEN DO:
        RUN state-changed (THIS-PROCEDURE, "tag-exit").        
        RETURN.
    END.
    
    IF SELF:SCREEN-VALUE EQ "PRINT" THEN DO:
        RUN state-changed (THIS-PROCEDURE, "print-bol").         
        SELF:SCREEN-VALUE = "".
        
        RETURN.
    END.
    
    lValidTag = oLoadTag:SetContext (cCompany, FALSE, SELF:SCREEN-VALUE).
    
    IF NOT lValidTag THEN DO:
        RUN pStatusMessage ("Invalid Tag '" + SELF:SCREEN-VALUE + "'", 3).
        
        SELF:SCREEN-VALUE = "".
        
        RETURN NO-APPLY.
    END.
    
    IF lShowTrailerTag THEN DO:
        SELF:BGCOLOR = 15.
        RETURN.        
    END.
    
    RUN state-changed (THIS-PROCEDURE, "scan-tag").  
    
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON TAB OF fiTag IN FRAME F-Main /* TAG */
DO:
    APPLY "LEAVE" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTrailerRelease
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTrailerRelease W-Win
ON ENTRY OF fiTrailerRelease IN FRAME F-Main /* TRAILER */
DO:
    SELF:SET-SELECTION (1, -1).      
    SELF:BGCOLOR = 30.

    oKeyboard:OpenKeyboard (SELF, "Qwerty").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTrailerRelease W-Win
ON LEAVE OF fiTrailerRelease IN FRAME F-Main /* TRAILER */
DO:
    RUN pValidateTrailer(SELF:SCREEN-VALUE) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        RETURN NO-APPLY.

    RUN pScanReleaseTrailer.
               
    SELF:BGCOLOR = 15.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTrailerTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTrailerTag W-Win
ON ENTRY OF fiTrailerTag IN FRAME F-Main /* TRAILER */
DO:
    SELF:SET-SELECTION (1, -1).      
    ASSIGN
        fiTag:BGCOLOR = 15
        SELF:BGCOLOR  = 30
        .     

    oKeyboard:OpenKeyboard (SELF, "Qwerty").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTrailerTag W-Win
ON LEAVE OF fiTrailerTag IN FRAME F-Main /* TRAILER */
DO:
    RUN pValidateTrailer(SELF:SCREEN-VALUE) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        RETURN NO-APPLY.
    
    RUN state-changed (THIS-PROCEDURE, "scan-tag").       
    SELF:BGCOLOR = 15.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
{sharpshooter/pStatusMessage.i}
{sharpshooter/ChangeWindowSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/adjustwindowsize.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_adjustwindowsize ).
       RUN set-position IN h_adjustwindowsize ( 1.00 , 165.80 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/releasefilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_releasefilter ).
       RUN set-position IN h_releasefilter ( 2.67 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 62.60 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/viewfginquiry.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_viewfginquiry ).
       RUN set-position IN h_viewfginquiry ( 4.81 , 188.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-releaseitems.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-releaseitems ).
       RUN set-position IN h_b-releaseitems ( 7.05 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-releaseitems ( 10.81 , 193.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatefirst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatefirst ).
       RUN set-position IN h_navigatefirst ( 8.48 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigateprev.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigateprev ).
       RUN set-position IN h_navigateprev ( 10.62 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatenext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatenext ).
       RUN set-position IN h_navigatenext ( 12.52 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatelast.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatelast ).
       RUN set-position IN h_navigatelast ( 14.43 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-releasetags.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-releasetags ).
       RUN set-position IN h_b-releasetags ( 17.76 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-releasetags ( 14.38 , 193.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatefirst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatefirst-2 ).
       RUN set-position IN h_navigatefirst-2 ( 18.00 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigateprev.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigateprev-2 ).
       RUN set-position IN h_navigateprev-2 ( 19.91 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatenext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatenext-2 ).
       RUN set-position IN h_navigatenext-2 ( 21.81 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatelast.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatelast-2 ).
       RUN set-position IN h_navigatelast-2 ( 23.71 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/setting.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_setting ).
       RUN set-position IN h_setting ( 31.81 , 164.40 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.60 ) */

       /* Links to SmartObject h_releasefilter. */
       RUN add-link IN adm-broker-hdl ( h_releasefilter , 'RELEASE':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_releasefilter , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_viewfginquiry. */
       RUN add-link IN adm-broker-hdl ( h_viewfginquiry , 'FGInquiry':U , THIS-PROCEDURE ).

       /* Links to SmartBrowser h_b-releaseitems. */
       RUN add-link IN adm-broker-hdl ( h_viewfginquiry , 'FGInq':U , h_b-releaseitems ).
       RUN add-link IN adm-broker-hdl ( h_b-releaseitems , 'REL-ITEMS':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_navigatefirst. */
       RUN add-link IN adm-broker-hdl ( h_b-releaseitems , 'NAV-FIRST':U , h_navigatefirst ).

       /* Links to SmartObject h_navigateprev. */
       RUN add-link IN adm-broker-hdl ( h_b-releaseitems , 'NAV-PREV':U , h_navigateprev ).

       /* Links to SmartObject h_navigatenext. */
       RUN add-link IN adm-broker-hdl ( h_b-releaseitems , 'NAV-NEXT':U , h_navigatenext ).

       /* Links to SmartObject h_navigatelast. */
       RUN add-link IN adm-broker-hdl ( h_b-releaseitems , 'NAV-LAST':U , h_navigatelast ).

       /* Links to SmartBrowser h_b-releasetags. */
       RUN add-link IN adm-broker-hdl ( h_b-releasetags , 'REL-TAGS':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_navigatefirst-2. */
       RUN add-link IN adm-broker-hdl ( h_b-releasetags , 'NAV-FIRST':U , h_navigatefirst-2 ).

       /* Links to SmartObject h_navigateprev-2. */
       RUN add-link IN adm-broker-hdl ( h_b-releasetags , 'NAV-PREV':U , h_navigateprev-2 ).

       /* Links to SmartObject h_navigatenext-2. */
       RUN add-link IN adm-broker-hdl ( h_b-releasetags , 'NAV-NEXT':U , h_navigatenext-2 ).

       /* Links to SmartObject h_navigatelast-2. */
       RUN add-link IN adm-broker-hdl ( h_b-releasetags , 'NAV-LAST':U , h_navigatelast-2 ).

       /* Links to SmartObject h_setting. */
       RUN add-link IN adm-broker-hdl ( h_setting , 'Setting':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_adjustwindowsize , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_releasefilter ,
             h_exit , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_viewfginquiry ,
             fiTrailerTag:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-releaseitems ,
             fiTagTrailerMessage:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatefirst ,
             h_b-releaseitems , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigateprev ,
             h_navigatefirst , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatenext ,
             h_navigateprev , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatelast ,
             h_navigatenext , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-releasetags ,
             h_navigatelast , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatefirst-2 ,
             h_b-releasetags , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigateprev-2 ,
             h_navigatefirst-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatenext-2 ,
             h_navigateprev-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatelast-2 ,
             h_navigatenext-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_setting ,
             h_navigatelast-2 , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY fiTrailerRelease fiTag fiTrailerTag fiTagTrailerMessage btnExitText 
          btnClearText btnViewInquiryText btnDeleteText statusMessage 
          btnSettingsText btnPrintBOLText 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btClear fiTag btnKeyboardTrailerRelease btnKeyboardTrailer 
         btnKeyboardTag btReset btnNumPad btDelete btPrintBOL btnExitText 
         btnClearText btnViewInquiryText btnDeleteText btnSettingsText 
         btnPrintBOLText 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetKeyboard W-Win 
PROCEDURE GetKeyboard :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opoKeyboard AS system.Keyboard NO-UNDO.

    opoKeyboard = oKeyboard.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Key_Stroke W-Win 
PROCEDURE Key_Stroke :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcKeyStroke AS CHARACTER NO-UNDO.
    
    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:KeyStroke(ipcKeyStroke).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy W-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-OBJECT(oLoadTag) THEN
      DELETE OBJECT oLoadTag.

  IF VALID-OBJECT(oKeyboard) THEN
      DELETE OBJECT oKeyboard.

  IF VALID-OBJECT(oReleaseHeader) THEN
      DELETE OBJECT oReleaseHeader.

  IF VALID-OBJECT(oTrailer) THEN
      DELETE OBJECT oTrailer.
      
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenSetting W-Win 
PROCEDURE OpenSetting :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN windows/setting-dialog.w.
    {sharpshooter/settingChangeDialog.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit W-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        RUN spGetSessionParam ("Company", OUTPUT cCompany).
        RUN spGetSessionParam ("CompanyName", OUTPUT cCompanyName).
        RUN spGetSessionParam ("Location", OUTPUT cLocation).
        RUN pStatusMessage ("", 0).
    
        {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                             + " - " + DYNAMIC-FUNCTION("sfVersion") + " - " 
                             + cCompanyName + " - " + cLocation
                             .
        
        oKeyboard:SetWindow({&WINDOW-NAME}:HANDLE).
        oKeyboard:SetProcedure(THIS-PROCEDURE).
        oKeyboard:SetFrame(FRAME {&FRAME-NAME}:HANDLE).
        
        RUN spGetSettingByName ("SSCreateBOLScanTrailer", OUTPUT gcScanTrailer).
        RUN spGetSettingByName ("SSCreateBOLPrint", OUTPUT gcSSBOLPrint).
        RUN spGetSettingByName ("SSCreateBOLTrailerValidation", OUTPUT gcSSReleaseTrailerValidation).
        RUN spGetSettingByName ("ShowVirtualKeyboard", OUTPUT gcShowVirtualKeyboard).
        RUN spGetSettingByName ("ShowFGItemInquiry", OUTPUT gcShowFGItemInquiry).
        RUN spGetSettingByName ("ShowSettings", OUTPUT gcShowSettings).
                        
        ASSIGN
            lShowTrailerTag                   = gcScanTrailer EQ "Tag" OR gcScanTrailer EQ "Both"
            lShowTrailerRelease               = gcScanTrailer EQ "Release" OR gcScanTrailer EQ "Both"
            fiTrailerTag:VISIBLE              = lShowTrailerTag
            fiTrailerRelease:VISIBLE          = lShowTrailerRelease
            fiTagTrailerMessage:VISIBLE       = lShowTrailerTag AND lShowTrailerRelease
            fiTrailerRelease:SENSITIVE        = FALSE
            glShowVirtualKeyboard             = LOGICAL (gcShowVirtualKeyboard)
            btnKeyboardTrailerRelease:VISIBLE = fiTrailerRelease:VISIBLE AND glShowVirtualKeyboard
            btnKeyboardTrailer:VISIBLE        = fiTrailerTag:VISIBLE AND glShowVirtualKeyboard
            btnKeyboardTag:VISIBLE            = glShowVirtualKeyboard
            btnNumPad:VISIBLE                 = glShowVirtualKeyboard
            RECT-2:VISIBLE                    = glShowVirtualKeyboard
            btnViewInquiryText:VISIBLE        = INDEX(gcShowFGItemInquiry, "Text") GT 0
            btnSettingsText:VISIBLE           = INDEX(gcShowSettings, "Text") GT 0
            .  
        
        IF INDEX(gcShowFGItemInquiry, "Icon") EQ 0 THEN
            {methods/run_link.i "FGInquiry-SOURCE" "HideFGInquiry"}

        IF INDEX(gcShowSettings, "Icon") EQ 0 THEN
            {methods/run_link.i "Setting-SOURCE" "HideSettings"}

        {methods/run_link.i "RELEASE-SOURCE" "ReleasePrintedValidation" "(TRUE)"}
        {methods/run_link.i "RELEASE-SOURCE" "ReleasePostedValidation" "(TRUE)"}
        {methods/run_link.i "RELEASE-SOURCE" "DisableErrorAlerts"}
        {methods/run_link.i "RELEASE-SOURCE" "SetKeyboard" "(oKeyboard)"}
        
        RUN pInvalidRelease.
        
        IF glShowVirtualKeyboard THEN
            RUN ShowKeyboard.
            
        /* If scan trailer is enabled */
        IF gcScanTrailer EQ "Tag" THEN
            btReset:TAB-STOP = FALSE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInValidRelease W-Win 
PROCEDURE pInValidRelease PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    ASSIGN
        fiTag:SENSITIVE                     = FALSE
        fiTag:SCREEN-VALUE                  = ""        
        fiTrailerTag:SENSITIVE              = FALSE
        fiTrailerRelease:SENSITIVE          = FALSE
        fiTrailerTag:SCREEN-VALUE           = ""
        fiTrailerRelease:SCREEN-VALUE       = ""
        btChange:HIDDEN                     = TRUE
        btChange:SENSITIVE                  = FALSE
        btReset:SENSITIVE                   = FALSE
        fiTag:BGCOLOR                       = 15
        fiTrailerTag:BGCOLOR                = 15
        .

    {methods/run_link.i "RELEASE-SOURCE" "EnableRelease"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintBOL W-Win 
PROCEDURE pPrintBOL PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iReleaseID         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBOLID             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cBOLPrintValueList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBOLPrintFieldList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPostBOL           AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE oBOLHeader AS bol.BOLHeader NO-UNDO.
        
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    {methods/run_link.i "RELEASE-SOURCE" "GetReleaseID" "(OUTPUT oReleaseHeader)"}
    
    iReleaseID = INTEGER(oReleaseHeader:GetValue("ReleaseID")).
    
    {methods/run_link.i "REL-TAGS-SOURCE" "CreateBOLFromRelease" "(INPUT cCompany, INPUT iReleaseID, OUTPUT lSuccess, OUTPUT cMessage, OUTPUT iBOLID)"}

    IF NOT lSuccess THEN DO:
        RUN pStatusMessage (cMessage, 3).
        RETURN.
    END.    
    
    oBOLHeader = NEW bol.BOLHeader().
    
    oBOLHeader:SetContext(cCompany, iBOLID).
    
    IF NOT oBOLHeader:IsAvailable() THEN DO:
        RUN pStatusMessage ("INVALID BOL '" + STRING(iBOLID) + "'", 3).      
        RETURN.        
    END.
    
    IF gcSSBOLPrint NE "DoNotPrint" THEN DO:
        IF gcSSBOLPrint EQ "ShowBOLPrintScreen" THEN DO:
            SESSION:SET-WAIT-STATE ("GENERAL").

            ASSIGN
                cBOLPrintFieldList = 'begin_cust,end_cust,begin_bol#,end_bol#,begin_ord#,end_ord#,tb_reprint,tb_posted,rd_bolcert,begin_date,end_date'
                cBOLPrintValueList = oBOLHeader:GetValue("CustomerID") + ',' + oBOLHeader:GetValue("CustomerID") + ',' 
                                   + oBOLHeader:GetValue("BOLID") + ',' + oBOLHeader:GetValue("BOLID")
                                   + ',,99999999,' 
                                   + oBOLHeader:GetValue("Printed") + ',' 
                                   + oBOLHeader:GetValue("Posted") + ',BOL' + ',' 
                                   + oBOLHeader:GetValue("BOLDate") + ',' 
                                   + oBOLHeader:GetValue("BOLDate")
                .
                
            RUN custom/setUserPrint.p (
                oBOLHeader:GetValue("Company"),
                'oe-boll_.',
                cBOLPrintFieldList,
                cBOLPrintValueList
                ).
      
            RUN listobjs/oe-boll_.w.

            SESSION:SET-WAIT-STATE ("").
        END.
        ELSE IF gcSSBOLPrint EQ "PrintSilently" OR gcSSBOLPrint EQ "PrintAndPostSilently" THEN DO:
            SESSION:SET-WAIT-STATE ("GENERAL").
            
            lPostBOL = gcSSBOLPrint EQ "PrintAndPostSilently".
             
            RUN bol/printBol.p (
                oBOLHeader:GetValue("Company"),
                cLocation,
                oBOLHeader:GetValue("CustomerID"),
                oBOLHeader:GetValue("BOLID"),
                oBOLHeader:GetValue("Printed"),
                oBOLHeader:GetValue("Posted"),
                lPostBOL
                ).

            SESSION:SET-WAIT-STATE ("").
        END.
    END.

    RUN pInValidRelease.
    
    {methods/run_link.i "REL-ITEMS-SOURCE" "EmptyReleaseItems"}

    {methods/run_link.i "REL-TAGS-SOURCE" "EmptyReleaseTags"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReleaseError W-Win 
PROCEDURE pReleaseError PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStatusMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStatusMessageType AS INTEGER   NO-UNDO.
    
    {methods/run_link.i "RELEASE-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
    
    RUN pStatusMessage (cStatusMessage, iStatusMessageType).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pScanRelease W-Win 
PROCEDURE pScanRelease :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iReleaseID AS INTEGER NO-UNDO.    
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    {methods/run_link.i "REL-ITEMS-SOURCE" "EmptyReleaseItems"}
    {methods/run_link.i "REL-TAGS-SOURCE" "EmptyReleaseTags"}    
    {methods/run_link.i "RELEASE-SOURCE" "GetReleaseID" "(OUTPUT oReleaseHeader)"}
    
    RUN pStatusMessage ("", 0).

    IF NOT VALID-OBJECT(oReleaseHeader) THEN DO:
        RUN pStatusMessage ("INVALID RELEASE", 3).
        RETURN.
    END.

    IF NOT oReleaseHeader:IsAvailable() THEN DO:
        RUN pStatusMessage ("INVALID RELEASE", 3).
        RETURN.
    END.    

    IF NOT LOGICAL(oReleaseHeader:GetValue("Printed")) THEN DO:
        RUN pStatusMessage ("RELEASE '" + oReleaseHeader:GetValue("ReleaseID") + "' IS NOT PRINTED", 3).
        RETURN.
    END.

    IF LOGICAL(oReleaseHeader:GetValue("Posted")) THEN DO:
        RUN pStatusMessage ("RELEASE '" + oReleaseHeader:GetValue("ReleaseID") + "' IS ALREADY POSTED", 3).
        RETURN.
    END.
    
    iReleaseID = INTEGER(oReleaseHeader:GetValue("ReleaseID")).
    
    {methods/run_link.i "REL-ITEMS-SOURCE" "BuildReleaseItems" "(INPUT cCompany, INPUT iReleaseID)"}
    {methods/run_link.i "REL-TAGS-SOURCE" "BuildReleaseTags" "(INPUT cCompany, INPUT iReleaseID)"}
    
    ASSIGN
        fiTag:SCREEN-VALUE        = ""
        fiTrailerTag:SCREEN-VALUE = ""
        .
    
    APPLY "ENTRY":U TO fiTag.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pScanReleaseTrailer W-Win 
PROCEDURE pScanReleaseTrailer PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStatuseMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValid          AS LOGICAL   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
            
    ASSIGN
        fiTag:SENSITIVE                     = TRUE
        fiTrailerTag:SENSITIVE              = TRUE
        fiTrailerRelease:SENSITIVE          = FALSE
        btChange:HIDDEN                     = FALSE
        btChange:SENSITIVE                  = TRUE
        btReset:SENSITIVE                   = TRUE
        .

    {methods/run_link.i "RELEASE-SOURCE" "DisableRelease"}
        
    RUN pScanRelease. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pScanTag W-Win 
PROCEDURE pScanTag PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lError     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iReleaseID AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    {methods/run_link.i "RELEASE-SOURCE" "GetReleaseID" "(OUTPUT oReleaseHeader)"}
    
    iReleaseID = INTEGER(oReleaseHeader:GetValue("ReleaseID")).
    
    {methods/run_link.i "REL-TAGS-SOURCE" "CreateReleaseTag" "(INPUT cCompany, INPUT iReleaseID, INPUT fiTag:SCREEN-VALUE, INPUT fiTrailerRelease:SCREEN-VALUE, INPUT fiTrailerTag:SCREEN-VALUE, OUTPUT lError, OUTPUT cMessage)"}

    IF lError THEN DO:
        RUN pStatusMessage (cMessage, 3).
        
        ASSIGN
            fiTag:SCREEN-VALUE        = ""
            fiTrailerTag:SCREEN-VALUE = ""
            .

        APPLY "ENTRY" TO fiTag.

        RETURN.
    END.    
    
    RUN pScanRelease.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidateTrailer W-Win 
PROCEDURE pValidateTrailer PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcTrailerID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cStatusMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValid         AS LOGICAL   NO-UNDO.
    
    IF ipcTrailerID NE "" THEN DO:
        lValid = oTrailer:SetContext(cCompany, ipcTrailerID).
        
        IF NOT lValid THEN DO:
            RUN pStatusMessage("Invalid Trailer '" + ipcTrailerID + "'", 3).
            RETURN ERROR.
        END.
        
        {methods/run_link.i "RELEASE-SOURCE" "GetReleaseID" "(OUTPUT oReleaseHeader)"}
        
        IF ipcTrailerID NE oReleaseHeader:GetValue("Trailer") THEN DO:
            cStatusMessage = "TRAILER '" + ipcTrailerID + "' DOES NOT MATCH RELEASE TRAILER '" + oReleaseHeader:GetValue("Trailer") + "'".
            IF gcSSReleaseTrailerValidation EQ "Error" THEN DO:
                RUN pStatusMessage(cStatusMessage, 3).
                RETURN ERROR.
            END.
            ELSE IF gcSSReleaseTrailerValidation EQ "Warning" THEN DO:
                RUN pStatusMessage(cStatusMessage, 2).
            END.
        END.
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidRelease W-Win 
PROCEDURE pValidRelease PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    IF lShowTrailerRelease THEN DO:
        fiTrailerRelease:SENSITIVE = TRUE.

        {methods/run_link.i "RELEASE-SOURCE" "GetReleaseID" "(OUTPUT oReleaseHeader)"}
        
        IF VALID-OBJECT (oReleaseHeader) THEN
            fiTrailerRelease:SCREEN-VALUE = oReleaseHeader:GetValue("Trailer").
                                 
        RETURN.
    END.
        
    ASSIGN
        fiTag:SENSITIVE                     = TRUE
        fiTrailerTag:SENSITIVE              = TRUE
        fiTrailerRelease:SENSITIVE          = FALSE
        btnKeyboardTag:SENSITIVE            = TRUE
        btnKeyboardTrailer:SENSITIVE        = TRUE
        btnKeyboardTrailerRelease:SENSITIVE = FALSE        
        btChange:HIDDEN                     = FALSE
        btChange:SENSITIVE                  = TRUE
        btReset:SENSITIVE                   = TRUE
        .

    {methods/run_link.i "RELEASE-SOURCE" "DisableRelease"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize W-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dCol    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dColTmp AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dRow    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dHeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWidth  AS DECIMAL NO-UNDO.
    
    SESSION:SET-WAIT-STATE("General").

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            btPrintBOL:ROW                       = {&WINDOW-NAME}:HEIGHT - 1.1
            btPrintBOL:COL                       = {&WINDOW-NAME}:WIDTH  - btPrintBOL:WIDTH - 1
            btnPrintBOLText:ROW                  = {&WINDOW-NAME}:HEIGHT - .86
            btnPrintBOLText:COL                  = btPrintBOL:COL - btnPrintBOLText:WIDTH - 1
            btDelete:ROW                         = {&WINDOW-NAME}:HEIGHT - 1.1
            btnDeleteText:ROW                    = {&WINDOW-NAME}:HEIGHT - .86
            statusMessage:ROW                    = {&WINDOW-NAME}:HEIGHT - .86
            dCol                                 = {&WINDOW-NAME}:WIDTH  - 8
            btnExitText:COL                      = dCol - 9
            btnClearText:COL                     = dCol - 12
            btClear:COL                          = dCol
            btnSettingsText:ROW                  = {&WINDOW-NAME}:HEIGHT - .86
            btnSettingsText:COL                  = btnPrintBOLText:COL - btnSettingsText:WIDTH - 10
            .
        RUN set-position IN h_exit ( 1.00 , dCol ) NO-ERROR.
        RUN set-position IN h_setting ( {&WINDOW-NAME}:HEIGHT - 1.1 , btnSettingsText:COL + 18 ) NO-ERROR.
        RUN get-position IN h_navigatefirst ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
        RUN set-position IN h_navigatefirst ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigateprev ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatenext ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatelast ( dRow , dCol ) NO-ERROR.
        RUN get-size IN h_b-releaseitems ( OUTPUT dHeight , OUTPUT dWidth ) NO-ERROR.
        dWidth = dCol - 2.
        RUN set-size IN h_b-releaseitems ( dHeight , dWidth ) NO-ERROR.
        ASSIGN
            dRow    = dHeight + 7.25
            dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
            .
        RUN set-position IN h_b-releasetags ( dRow , ? ) NO-ERROR.
        RUN set-size IN h_b-releasetags ( dHeight , dWidth ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatefirst-2 ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigateprev-2 ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatenext-2 ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatelast-2 ( dRow , dCol ) NO-ERROR.
        RUN set-position IN h_adjustwindowsize ( 1.00 , dCol - 45 ) NO-ERROR.
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Select_Exit W-Win 
PROCEDURE Select_Exit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN dispatch ("exit").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus W-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {methods/run_link.i "RELEASE-SOURCE" "Set-Focus"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowKeyboard W-Win
PROCEDURE ShowKeyboard:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    {methods/run_link.i "RELEASE-SOURCE" "ShowKeyboard"}

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.
  
    DEFINE VARIABLE cStatusMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStatusMessage AS INTEGER   NO-UNDO.

    IF p-state BEGINS "Status-Message" THEN
    ASSIGN
        iStatusMessage = INTEGER(ENTRY(3,p-state,"|"))
        cStatusMessage = ENTRY(2,p-state,"|")
        p-state        = ENTRY(1,p-state,"|")
        .
    CASE p-state:
        WHEN "release-error" THEN
            RUN pReleaseError.
        WHEN "release-invalid" THEN DO:
            RUN pInValidRelease.
        END.
        WHEN "release-valid" THEN DO:
            RUN pValidRelease.            
            RUN pScanRelease.
        END.
        WHEN "scan-tag" THEN DO:
            RUN pScanTag.
        END.
        WHEN "print-bol" THEN DO:
            RUN pPrintBOL.
        END.
        WHEN "release-exit" OR WHEN "tag-exit" THEN DO:
            RUN Select_Exit.
        END.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

