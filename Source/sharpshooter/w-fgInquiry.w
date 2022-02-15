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

  File: sharpshooter/w-fgInquiry.w

  Description: FG Inquiry and quantity adjustment

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
// {system/sysconst.i}
{wip/keyboardDefs.i}

/* Required for run_link.i */
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.

DEFINE VARIABLE cCompany   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemID    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustItem  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSellUOM   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWarehouse AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobNo     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iJobNo2    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lError     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lHasAccess AS LOGICAL   NO-UNDO.

DEFINE VARIABLE cCharHandle  AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdItemBins   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdItemLoc    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdItemLocBin AS HANDLE    NO-UNDO.

DEFINE VARIABLE iWarehouseLength AS INTEGER   NO-UNDO.

/* As smart browsers are not instantiated until the page it contains is selected,
   the below variables will track if a page containing the browser is already selected
   and decides if queries on smart browsers should be run or not */
DEFINE VARIABLE lLocQueryRan     AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBinsQueryRan    AS LOGICAL NO-UNDO.
DEFINE VARIABLE lLocBinsQueryRan AS LOGICAL NO-UNDO.

DEFINE VARIABLE hdFGProcs AS HANDLE NO-UNDO.
&SCOPED-DEFINE SORTBY-PHRASE BY ttBrowseInventory.tag

DEFINE VARIABLE glShowVirtualKeyboard AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE gcShowSettings        AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcShowAdjustQuantity  AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcShowVirtualKeyboard AS CHARACTER NO-UNDO.

DEFINE VARIABLE oKeyboard AS system.Keyboard       NO-UNDO.

oKeyboard = NEW system.Keyboard().

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
&Scoped-Define ENABLED-OBJECTS btnPage1 btnPage2 btnPage3 btItemHelp ~
btItemNameHelp btLocHelp btnKeyboardItem btnNumPad fiFGItem fiCustItem ~
fiLocation btnKeyboardLoc btnKeyboardItemName btnExitText btnAdjustQtyText ~
btnAdjustQtyText-2 btClear btnClearText btnSettingsText 
&Scoped-Define DISPLAYED-OBJECTS fiFGItem fiCustItem fiLocation fiSellUOM ~
btnExitText statusMessage btnAdjustQtyText btnAdjustQtyText-2 btnClearText ~
btnSettingsText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_adjustqty AS HANDLE NO-UNDO.
DEFINE VARIABLE h_adjustqty-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_adjustwindowsize AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-fginqbins AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-fginqloc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-fginqlocbin AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatefirst AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatefirst-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatefirst-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatelast AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatelast-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatelast-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatenext AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatenext-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatenext-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigateprev AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigateprev-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigateprev-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_setting AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btClear 
     IMAGE-UP FILE "Graphics/32x32/back_white.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/back_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btItemHelp 
     IMAGE-UP FILE "Graphics/32x32/search_new.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 6.6 BY 1.57.

DEFINE BUTTON btItemNameHelp 
     IMAGE-UP FILE "Graphics/32x32/search_new.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 6.6 BY 1.52.

DEFINE BUTTON btLocHelp 
     IMAGE-UP FILE "Graphics/32x32/search_new.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 6.6 BY 1.57.

DEFINE BUTTON btnKeyboardItem 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnKeyboardItemName 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnKeyboardLoc 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnPage1  NO-FOCUS
     LABEL "DETAIL" 
     SIZE 18 BY 1.43
     FONT 38.

DEFINE BUTTON btnPage2  NO-FOCUS
     LABEL "LOCATION" 
     SIZE 20 BY 1.43
     FONT 36.

DEFINE BUTTON btnPage3  NO-FOCUS
     LABEL "BIN" 
     SIZE 18 BY 1.43
     FONT 36.

DEFINE VARIABLE btnAdjustQtyText AS CHARACTER FORMAT "X(256)":U INITIAL "ADJUST QTY" 
      VIEW-AS TEXT 
     SIZE 24 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnAdjustQtyText-2 AS CHARACTER FORMAT "X(256)":U INITIAL "ADJUST QTY" 
      VIEW-AS TEXT 
     SIZE 24 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnClearText AS CHARACTER FORMAT "X(256)":U INITIAL "RESET" 
      VIEW-AS TEXT 
     SIZE 12 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnExitText AS CHARACTER FORMAT "X(256)":U INITIAL "EXIT" 
      VIEW-AS TEXT 
     SIZE 8 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnSettingsText AS CHARACTER FORMAT "X(256)":U INITIAL "SETTINGS" 
      VIEW-AS TEXT 
     SIZE 18 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE fiCustItem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49.6 BY 1.43
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiFGItem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49.6 BY 1.43
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiLocation AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32.2 BY 1.43
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiSellUOM AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1.43
     BGCOLOR 21 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE statusMessage AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS MESSAGE" 
      VIEW-AS TEXT 
     SIZE 116 BY 1.43 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 59 BY 2.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnPage1 AT ROW 5.05 COL 118 WIDGET-ID 182
     btnPage2 AT ROW 5.05 COL 137 WIDGET-ID 178
     btnPage3 AT ROW 5.05 COL 157 WIDGET-ID 180
     btItemHelp AT ROW 2.52 COL 82.6 WIDGET-ID 138 NO-TAB-STOP 
     btItemNameHelp AT ROW 4.81 COL 82 WIDGET-ID 140 NO-TAB-STOP 
     btLocHelp AT ROW 2.43 COL 151 WIDGET-ID 146 NO-TAB-STOP 
     btnKeyboardItem AT ROW 2.52 COL 90 WIDGET-ID 136 NO-TAB-STOP 
     btnNumPad AT ROW 4.81 COL 178 WIDGET-ID 120 NO-TAB-STOP 
     fiFGItem AT ROW 2.57 COL 30.4 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     fiCustItem AT ROW 4.81 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fiLocation AT ROW 2.48 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 142
     btnKeyboardLoc AT ROW 2.43 COL 158.6 WIDGET-ID 144 NO-TAB-STOP 
     btnKeyboardItemName AT ROW 4.81 COL 89.6 WIDGET-ID 134 NO-TAB-STOP 
     fiSellUOM AT ROW 2.43 COL 175 COLON-ALIGNED NO-LABEL WIDGET-ID 148 NO-TAB-STOP 
     btnExitText AT ROW 1.24 COL 187 NO-LABEL WIDGET-ID 24
     statusMessage AT ROW 31.95 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     btnAdjustQtyText AT ROW 31.95 COL 169 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     btnAdjustQtyText-2 AT ROW 31.95 COL 169 COLON-ALIGNED NO-LABEL WIDGET-ID 184
     btClear AT ROW 3.19 COL 194.8 WIDGET-ID 186
     btnClearText AT ROW 3.38 COL 182 NO-LABEL WIDGET-ID 188
     btnSettingsText AT ROW 31.95 COL 141.6 NO-LABEL WIDGET-ID 190
     " View Inventory By" VIEW-AS TEXT
          SIZE 21 BY .67 AT ROW 4.1 COL 119 WIDGET-ID 154
          BGCOLOR 21 FGCOLOR 15 FONT 22
     "FG ITEM:" VIEW-AS TEXT
          SIZE 16 BY 1.1 AT ROW 2.67 COL 16 WIDGET-ID 166
          BGCOLOR 21 FGCOLOR 15 FONT 38
     "CUSTOMER PART:" VIEW-AS TEXT
          SIZE 31 BY 1.1 AT ROW 5.05 COL 1 WIDGET-ID 168
          BGCOLOR 21 FGCOLOR 15 FONT 38
     "LOCATION:" VIEW-AS TEXT
          SIZE 20.4 BY 1.1 AT ROW 2.67 COL 97.6 WIDGET-ID 170
          BGCOLOR 21 FGCOLOR 15 FONT 38
     "UOM:" VIEW-AS TEXT
          SIZE 10.2 BY 1.1 AT ROW 2.67 COL 166 WIDGET-ID 172
          BGCOLOR 21 FGCOLOR 15 FONT 38
     RECT-34 AT ROW 4.57 COL 117 WIDGET-ID 152
     RECT-2 AT ROW 4.57 COL 177 WIDGET-ID 130
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202 BY 32.81
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
         TITLE              = "FG Inquiry"
         HEIGHT             = 32.81
         WIDTH              = 202
         MAX-HEIGHT         = 272.38
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 272.38
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       btnAdjustQtyText-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN btnClearText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN btnExitText IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       btnKeyboardItem:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboardItemName:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboardLoc:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN btnSettingsText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiSellUOM IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-34 IN FRAME F-Main
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
ON END-ERROR OF W-Win /* FG Inquiry */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* FG Inquiry */
DO:
    /* This ADM code must be left here in order for the SmartWindow
       and its descendents to terminate properly on exit. */
    
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.

    IF VALID-HANDLE(hdFGProcs) THEN
        DELETE PROCEDURE hdFGProcs.
        
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btClear W-Win
ON CHOOSE OF btClear IN FRAME F-Main /* Reset */
DO:
    RUN pStatusMessage ("", 0).
    
    RUN pClearRecords.
    
    ASSIGN
        fiFGItem:SCREEN-VALUE   = ""
        fiCustItem:SCREEN-VALUE = ""
        fiLocation:SCREEN-VALUE = ""
        fiSellUOM:SCREEN-VALUE  = ""
        .
    
    APPLY "ENTRY" TO fiFGItem.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btItemHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btItemHelp W-Win
ON CHOOSE OF btItemHelp IN FRAME F-Main
DO:
    APPLY "HELP" TO fiFGItem.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btItemNameHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btItemNameHelp W-Win
ON CHOOSE OF btItemNameHelp IN FRAME F-Main
DO:
    APPLY "HELP" TO fiCustItem.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLocHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLocHelp W-Win
ON CHOOSE OF btLocHelp IN FRAME F-Main
DO:
    APPLY "HELP" TO fiLocation.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdjustQtyText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdjustQtyText W-Win
ON MOUSE-SELECT-CLICK OF btnAdjustQtyText IN FRAME F-Main
DO:
    RUN AdjustQuantity IN h_b-fginqbins.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdjustQtyText-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdjustQtyText-2 W-Win
ON MOUSE-SELECT-CLICK OF btnAdjustQtyText-2 IN FRAME F-Main
DO:
    RUN AdjustQuantity IN h_b-fginqloc.
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


&Scoped-define SELF-NAME btnExitText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExitText W-Win
ON MOUSE-SELECT-CLICK OF btnExitText IN FRAME F-Main
DO:
    RUN dispatch ("exit").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboardItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardItem W-Win
ON CHOOSE OF btnKeyboardItem IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiFGItem.
    
    RUN pKeyboard (
        INPUT fiFGItem:HANDLE,
        INPUT "Qwerty"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboardItemName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardItemName W-Win
ON CHOOSE OF btnKeyboardItemName IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiCustItem.
    RUN pKeyboard (
        INPUT fiCustItem:HANDLE,
        INPUT "Qwerty"
        ).        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboardLoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardLoc W-Win
ON CHOOSE OF btnKeyboardLoc IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiLocation.
    
    RUN pKeyboard (
        INPUT fiLocation:HANDLE,
        INPUT "Qwerty"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNumPad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNumPad W-Win
ON CHOOSE OF btnNumPad IN FRAME F-Main /* NumPad */
DO:
    ASSIGN
        lKeyboard = NOT lKeyboard
        RECT-2:BGCOLOR = IF lKeyboard THEN 10 ELSE 12
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPage1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPage1 W-Win
ON CHOOSE OF btnPage1 IN FRAME F-Main /* DETAIL */
DO:
    RUN select-page(1).
    ASSIGN
        btnPage1:FONT = 38
        btnPage2:FONT = 36
        btnPage3:FONT = 36
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPage2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPage2 W-Win
ON CHOOSE OF btnPage2 IN FRAME F-Main /* LOCATION */
DO:
    RUN select-page(2).
    ASSIGN
        btnPage1:FONT = 36
        btnPage2:FONT = 38
        btnPage3:FONT = 36
        .
    IF NOT lLocQueryRan THEN
        RUN pScanItem.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPage3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPage3 W-Win
ON CHOOSE OF btnPage3 IN FRAME F-Main /* BIN */
DO:
    RUN select-page(3).
    ASSIGN
        btnPage1:FONT = 36
        btnPage2:FONT = 36
        btnPage3:FONT = 38
        .
    IF NOT lLocBinsQueryRan THEN
        RUN pScanItem.
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


&Scoped-define SELF-NAME fiCustItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustItem W-Win
ON RETURN OF fiCustItem IN FRAME F-Main
DO:
    APPLY "LEAVE" TO SELF.
    
    APPLY "ENTRY" TO fiLocation.
    
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustItem W-Win
ON ENTRY OF fiCustItem IN FRAME F-Main
DO:
    SELF:BGCOLOR = 30.
    
    hFocusField = SELF.
    
    IF lKeyboard THEN
        RUN pKeyboard (
            INPUT SELF, 
            INPUT "Qwerty"
            ).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustItem W-Win
ON HELP OF fiCustItem IN FRAME F-Main
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        INPUT  "",  /* company */ 
        INPUT  "",  /* lookup field */
        INPUT  149, /* Subject ID */
        INPUT  "",  /* User ID */
        INPUT  0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN DO:
        ASSIGN
            fiCustItem:SCREEN-VALUE = IF NUM-ENTRIES(returnFields,"|") GE 2 THEN
                                          ENTRY(2, returnFields, "|")
                                      ELSE
                                          ""
            fiFGItem:SCREEN-VALUE   = IF NUM-ENTRIES(returnFields,"|") GE 4 THEN
                                          ENTRY(4, returnFields, "|")
                                      ELSE
                                          "".
        
        APPLY "LEAVE" TO SELF.
        
        APPLY "ENTRY" TO fiLocation.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustItem W-Win
ON LEAVE OF fiCustItem IN FRAME F-Main
DO:
    DEFINE VARIABLE lHasMutlipleItems AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cItemSelected     AS CHARACTER NO-UNDO.
    
    IF LASTKEY EQ -1 THEN
        RETURN.

    IF fiFGItem:SCREEN-VALUE EQ "" THEN
        RUN pClearRecords.
    
    IF SELF:SCREEN-VALUE EQ "" THEN DO:
        cCustItem = "".
        RETURN.    
    END.
    
    ASSIGN
        cCustItem = SELF:SCREEN-VALUE
        cItemID   = fiFGItem:SCREEN-VALUE
        .

    RUN FG_HasMultipleFGItemsForCustPart IN hdFGProcs (
        INPUT  cCompany,
        INPUT  cItemID,
        INPUT  cCustItem,
        OUTPUT cItemID,
        OUTPUT lHasMutlipleItems
        ).
    IF lHasMutlipleItems THEN DO:
        cItemID = fiFGItem:SCREEN-VALUE.
        
        RUN fg/d-fgItemPrompt.w (
            INPUT  cCompany,
            INPUT  cItemID,
            INPUT  cCustItem,
            OUTPUT cItemSelected
            ).
        
        IF cItemSelected EQ "" THEN DO:
            RUN pStatusMessage ("PLEASE SELECT A VALID ITEM FOR CUST PART " + cCustItem,2).
            cCustItem = "".            
            RETURN.
        END.        
        cItemID = cItemSelected.
    END.
    
    RUN pScanItem.

    FINALLY:    
        SELF:BGCOLOR = 15.   
    END FINALLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFGItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFGItem W-Win
ON RETURN OF fiFGItem IN FRAME F-Main
DO:
    APPLY "LEAVE" TO SELF.
    
    APPLY "ENTRY" TO fiCustItem.
    
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFGItem W-Win
ON ENTRY OF fiFGItem IN FRAME F-Main
DO:
    SELF:BGCOLOR = 30.
    
    hFocusField = SELF.
    
    IF lKeyboard THEN
        RUN pKeyboard (
            INPUT SELF, 
            INPUT "Qwerty"
            ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFGItem W-Win
ON HELP OF fiFGItem IN FRAME F-Main
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        INPUT  "",  /* company */ 
        INPUT  "",  /* lookup field */
        INPUT  25,  /* Subject ID */
        INPUT  "",  /* User ID */
        INPUT  0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN DO:
        fiFGItem:SCREEN-VALUE = IF NUM-ENTRIES(returnFields,"|") GE 2 THEN
                                    ENTRY(2, returnFields, "|")
                                ELSE
                                    "".
        
        APPLY "LEAVE" TO SELF.
        
        APPLY "ENTRY" TO fiCustItem.
    END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFGItem W-Win
ON LEAVE OF fiFGItem IN FRAME F-Main
DO:
    IF cItemID EQ SELF:SCREEN-VALUE AND LASTKEY NE -1 THEN
        RETURN.

    RUN pClearRecords.
    
    IF SELF:SCREEN-VALUE EQ "" THEN DO:
        cItemID = "".
        RETURN.
    END.
    
    ASSIGN        
        cItemID                 = SELF:SCREEN-VALUE
        fiCustItem:SCREEN-VALUE = ""
        cCustItem               = ""
        .

    RUN pScanItem. 
    
    FINALLY:    
        SELF:BGCOLOR = 15.   
    END FINALLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON RETURN OF fiLocation IN FRAME F-Main
DO:
    APPLY "LEAVE" TO SELF.
    
    APPLY "ENTRY" TO fiFGItem.
    
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON ENTRY OF fiLocation IN FRAME F-Main
DO:
    SELF:BGCOLOR = 30.
    
    hFocusField = SELF.
    
    IF lKeyboard THEN
        RUN pKeyboard (
            INPUT SELF, 
            INPUT "Qwerty"
            ).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON HELP OF fiLocation IN FRAME F-Main
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        INPUT  "",  /* company */ 
        INPUT  "",  /* lookup field */
        INPUT  150, /* Subject ID */
        INPUT  "",  /* User ID */
        INPUT  0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN DO:
        fiLocation:SCREEN-VALUE = IF NUM-ENTRIES(returnFields,"|") GE 2 THEN
                                       ENTRY(2, returnFields, "|")
                                  ELSE
                                      "".
        
        APPLY "LEAVE" TO SELF.
        
        APPLY "ENTRY" TO fiFGItem.
    END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON LEAVE OF fiLocation IN FRAME F-Main
DO:        
    IF cWarehouse EQ TRIM(SUBSTRING(fiLocation:SCREEN-VALUE, 1, iWarehouseLength)) AND cLocation EQ TRIM(SUBSTRING(fiLocation:SCREEN-VALUE, iWarehouseLength + 1)) THEN
        RETURN.
        
    ASSIGN
        cWarehouse = TRIM(SUBSTRING(fiLocation:SCREEN-VALUE, 1, iWarehouseLength))
        cLocation  = TRIM(SUBSTRING(fiLocation:SCREEN-VALUE, iWarehouseLength + 1))
        .        

    RUN pScanItem.

    FINALLY:    
        SELF:BGCOLOR = 15.   
    END FINALLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
/* Include custom  Main Block code for SmartWindows. */

{src/adm/template/windowmn.i}

{sharpshooter/smartobj/windowExit.i}
{wip/pKeyboard.i}
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
             INPUT  'smartobj/setting.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_setting ).
       RUN set-position IN h_setting ( 31.76 , 160.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.60 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/adjustwindowsize.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_adjustwindowsize ).
       RUN set-position IN h_adjustwindowsize ( 1.00 , 160.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/exit.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       /* Links to SmartObject h_setting. */
       RUN add-link IN adm-broker-hdl ( h_setting , 'SETTING':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             fiFGItem:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigateprev.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigateprev ).
       RUN set-position IN h_navigateprev ( 9.57 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-fginqbins.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-fginqbins ).
       RUN set-position IN h_b-fginqbins ( 6.95 , 2.20 ) NO-ERROR.
       RUN set-size IN h_b-fginqbins ( 24.52 , 192.80 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatelast.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatelast ).
       RUN set-position IN h_navigatelast ( 13.14 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatefirst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatefirst ).
       RUN set-position IN h_navigatefirst ( 7.67 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatenext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatenext ).
       RUN set-position IN h_navigatenext ( 11.24 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/adjustqty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_adjustqty ).
       RUN set-position IN h_adjustqty ( 31.71 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       /* Links to SmartObject h_navigateprev. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqbins , 'NAV-PREV':U , h_navigateprev ).

       /* Links to SmartBrowser h_b-fginqbins. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Bins':U , h_b-fginqbins ).

       /* Links to SmartObject h_navigatelast. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqbins , 'NAV-LAST':U , h_navigatelast ).

       /* Links to SmartObject h_navigatefirst. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqbins , 'NAV-FIRST':U , h_navigatefirst ).

       /* Links to SmartObject h_navigatenext. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqbins , 'NAV-NEXT':U , h_navigatenext ).

       /* Links to SmartObject h_adjustqty. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqbins , 'ADJUST':U , h_adjustqty ).
       RUN add-link IN adm-broker-hdl ( h_adjustqty , 'ADJUST':U , THIS-PROCEDURE ).

    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigateprev.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigateprev-2 ).
       RUN set-position IN h_navigateprev-2 ( 9.57 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-fginqloc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-fginqloc ).
       RUN set-position IN h_b-fginqloc ( 6.95 , 2.20 ) NO-ERROR.
       RUN set-size IN h_b-fginqloc ( 24.52 , 192.80 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatelast.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatelast-2 ).
       RUN set-position IN h_navigatelast-2 ( 13.14 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatenext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatenext-2 ).
       RUN set-position IN h_navigatenext-2 ( 11.24 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/adjustqty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_adjustqty-2 ).
       RUN set-position IN h_adjustqty-2 ( 31.71 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatefirst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatefirst-2 ).
       RUN set-position IN h_navigatefirst-2 ( 7.67 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       /* Links to SmartObject h_navigateprev-2. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqloc , 'NAV-PREV':U , h_navigateprev-2 ).

       /* Links to SmartBrowser h_b-fginqloc. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Loc':U , h_b-fginqloc ).

       /* Links to SmartObject h_navigatelast-2. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqloc , 'NAV-LAST':U , h_navigatelast-2 ).

       /* Links to SmartObject h_navigatenext-2. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqloc , 'NAV-NEXT':U , h_navigatenext-2 ).

       /* Links to SmartObject h_adjustqty-2. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqloc , 'ADJUST':U , h_adjustqty-2 ).
       RUN add-link IN adm-broker-hdl ( h_adjustqty-2 , 'ADJUST-2':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_navigatefirst-2. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqloc , 'NAV-FIRST':U , h_navigatefirst-2 ).

    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-fginqlocbin.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-fginqlocbin ).
       RUN set-position IN h_b-fginqlocbin ( 6.95 , 2.20 ) NO-ERROR.
       RUN set-size IN h_b-fginqlocbin ( 24.52 , 192.80 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatelast.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatelast-3 ).
       RUN set-position IN h_navigatelast-3 ( 13.14 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatenext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatenext-3 ).
       RUN set-position IN h_navigatenext-3 ( 11.24 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatefirst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatefirst-3 ).
       RUN set-position IN h_navigatefirst-3 ( 7.67 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigateprev.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigateprev-3 ).
       RUN set-position IN h_navigateprev-3 ( 9.57 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       /* Links to SmartBrowser h_b-fginqlocbin. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'LocBin':U , h_b-fginqlocbin ).

       /* Links to SmartObject h_navigatelast-3. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqlocbin , 'NAV-LAST':U , h_navigatelast-3 ).

       /* Links to SmartObject h_navigatenext-3. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqlocbin , 'NAV-NEXT':U , h_navigatenext-3 ).

       /* Links to SmartObject h_navigatefirst-3. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqlocbin , 'NAV-FIRST':U , h_navigatefirst-3 ).

       /* Links to SmartObject h_navigateprev-3. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqlocbin , 'NAV-PREV':U , h_navigateprev-3 ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 3 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

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
  DISPLAY fiFGItem fiCustItem fiLocation fiSellUOM btnExitText statusMessage 
          btnAdjustQtyText btnAdjustQtyText-2 btnClearText btnSettingsText 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btnPage1 btnPage2 btnPage3 btItemHelp btItemNameHelp btLocHelp 
         btnKeyboardItem btnNumPad fiFGItem fiCustItem fiLocation 
         btnKeyboardLoc btnKeyboardItemName btnExitText btnAdjustQtyText 
         btnAdjustQtyText-2 btClear btnClearText btnSettingsText 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page AS INTEGER NO-UNDO.
  DEFINE VARIABLE dCol             AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dColTmp          AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dRow             AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dHeight          AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dWidth           AS DECIMAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  DO WITH FRAME {&FRAME-NAME}:
      CASE adm-current-page:
          WHEN 1 THEN
              IF INDEX(gcShowAdjustQuantity, "Text") GT 0 THEN 
                  ASSIGN
                      btnAdjustQtyText:HIDDEN   = NO
                      btnAdjustQtyText-2:HIDDEN = YES
                      .
          WHEN 2 THEN DO:
            IF INDEX(gcShowAdjustQuantity, "Icon") EQ 0 THEN
                {methods/run_link.i "ADJUST-2-SOURCE" "HideAdjustQuantity"}            
            RUN get-position IN h_navigatefirst ( OUTPUT dRow , OUTPUT dCol ) NO-ERROR.
            RUN set-position IN h_navigatefirst-2 ( dRow , dCol ) NO-ERROR.
            RUN get-position IN h_navigateprev ( OUTPUT dRow , OUTPUT dCol ) NO-ERROR.
            RUN set-position IN h_navigateprev-2 ( dRow , dCol ) NO-ERROR.
            RUN get-position IN h_navigatenext ( OUTPUT dRow , OUTPUT dCol ) NO-ERROR.
            RUN set-position IN h_navigatenext-2 ( dRow , dCol ) NO-ERROR.
            RUN get-position IN h_navigatelast ( OUTPUT dRow , OUTPUT dCol ) NO-ERROR.
            RUN set-position IN h_navigatelast-2 ( dRow , dCol ) NO-ERROR.
            RUN get-position IN h_adjustqty ( OUTPUT dRow , OUTPUT dCol ) NO-ERROR.
            RUN set-position IN h_adjustqty-2 ( dRow , dCol ) NO-ERROR.
            ASSIGN
                btnAdjustQtyText-2:COL    = btnAdjustQtyText:COL
                btnAdjustQtyText-2:ROW    = btnAdjustQtyText:ROW
                .

            IF INDEX(gcShowAdjustQuantity, "Text") GT 0 THEN
                ASSIGN        
                    btnAdjustQtyText:HIDDEN   = YES
                    btnAdjustQtyText-2:HIDDEN = NO
                    .

            RUN get-position IN h_b-fginqloc ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
            RUN get-size IN h_b-fginqloc ( OUTPUT dHeight , OUTPUT dWidth ) NO-ERROR.
            ASSIGN
                dCol    = {&WINDOW-NAME}:WIDTH  - 8
                dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
                dWidth  = dCol - 3
                .
            RUN set-size IN h_b-fginqloc ( dHeight , dWidth ) NO-ERROR.
          END.
          WHEN 3 THEN DO:
            RUN get-position IN h_navigatefirst ( OUTPUT dRow , OUTPUT dCol ) NO-ERROR.
            RUN set-position IN h_navigatefirst-3 ( dRow , dCol ) NO-ERROR.
            RUN get-position IN h_navigateprev ( OUTPUT dRow , OUTPUT dCol ) NO-ERROR.
            RUN set-position IN h_navigateprev-3 ( dRow , dCol ) NO-ERROR.
            RUN get-position IN h_navigatenext ( OUTPUT dRow , OUTPUT dCol ) NO-ERROR.
            RUN set-position IN h_navigatenext-3 ( dRow , dCol ) NO-ERROR.
            RUN get-position IN h_navigatelast ( OUTPUT dRow , OUTPUT dCol ) NO-ERROR.
            RUN set-position IN h_navigatelast-3 ( dRow , dCol ) NO-ERROR.
            ASSIGN
                btnAdjustQtyText:HIDDEN   = YES
                btnAdjustQtyText-2:HIDDEN = YES
                .
            RUN get-position IN h_b-fginqlocbin ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
            RUN get-size IN h_b-fginqlocbin ( OUTPUT dHeight , OUTPUT dWidth ) NO-ERROR.
            ASSIGN
                dCol    = {&WINDOW-NAME}:WIDTH  - 8
                dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
                dWidth  = dCol - 3
                .
            RUN set-size IN h_b-fginqlocbin ( dHeight , dWidth ) NO-ERROR.
          END.
      END CASE. 
  END. /* with frame */

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
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.

    IF VALID-HANDLE(hdFGProcs) THEN
        DELETE PROCEDURE hdFGProcs.

    IF VALID-OBJECT (oKeyboard) THEN
        DELETE OBJECT oKeyboard.
              
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
    RUN pWinReSize.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.
    btnAdjustQtyText-2:HIDDEN IN FRAME {&FRAME-NAME} = YES.

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
 Notes:
------------------------------------------------------------------------------*/
    RUN windows/setting-dialog.w.
    {sharpshooter/settingChangeDialog.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClearRecords W-Win 
PROCEDURE pClearRecords :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(hdItemBins) THEN 
        RUN ClearRecords IN hdItemBins.

    IF VALID-HANDLE(hdItemLoc) THEN
        RUN ClearRecords IN hdItemLoc.

    IF VALID-HANDLE(hdItemLocBin) THEN
        RUN ClearRecords IN hdItemLocBin.
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
    DEFINE VARIABLE hdPgmSecurity    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cLoc             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
        
    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    RUN spGetSessionParam ("Location", OUTPUT cLoc).
    RUN pStatusMessage ("", 0).
    
    RUN spSetSettingContext.
    
    FIND FIRST company NO-LOCK 
         WHERE company.company EQ cCompany
         NO-ERROR .
    IF AVAILABLE company THEN
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                         + " - " + DYNAMIC-FUNCTION("sfVersion") + " - " 
                         + STRING(company.name) + " - " + cLoc.

    RUN spGetSettingByName ("ShowVirtualKeyboard", OUTPUT gcShowVirtualKeyboard).
    RUN spGetSettingByName ("ShowAdjustQuantity", OUTPUT gcShowAdjustQuantity).
    RUN spGetSettingByName ("ShowSettings", OUTPUT gcShowSettings).    
    
    glShowVirtualKeyboard = LOGICAL(gcShowVirtualKeyboard) NO-ERROR.

    ASSIGN        
        btnKeyboardItem:VISIBLE     = glShowVirtualKeyboard
        btnKeyboardItemName:VISIBLE = glShowVirtualKeyboard
        btnKeyboardLoc:VISIBLE      = glShowVirtualKeyboard
        btnNumPad:VISIBLE           = glShowVirtualKeyboard
        RECT-2:VISIBLE              = glShowVirtualKeyboard        
        btnSettingsText:VISIBLE     = INDEX(gcShowSettings, "Text") GT 0
        btnAdjustQtyText:VISIBLE    = INDEX(gcShowAdjustQuantity, "Text") GT 0
        btnAdjustQtyText-2:VISIBLE  = INDEX(gcShowAdjustQuantity, "Text") GT 0
        .  
        
    IF INDEX(gcShowSettings, "Icon") EQ 0 THEN
        {methods/run_link.i "Setting-SOURCE" "HideSettings"}    

    IF INDEX(gcShowAdjustQuantity, "Icon") EQ 0 THEN
        {methods/run_link.i "ADJUST-SOURCE" "HideAdjustQuantity"}   

    IF INDEX(gcShowAdjustQuantity, "Icon") EQ 0 THEN
        {methods/run_link.i "ADJUST-2-SOURCE" "HideAdjustQuantity"}  
                            
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    
    RUN Inventory_GetWarehouseLength IN hdInventoryProcs (
        INPUT  cCompany,
        OUTPUT iWarehouseLength
        ).
            
    DELETE PROCEDURE hdInventoryProcs.
    
    RUN "system/PgmMstrSecur.p" PERSISTENT SET hdPgmSecurity.

    RUN epCanAccess IN hdPgmSecurity (
        INPUT  "browsers/rm-ibin.w", 
        INPUT  "", 
        OUTPUT lHasAccess
        ).
        
    DELETE OBJECT hdPgmSecurity.    
    
    RUN fg/FGProcs.p PERSISTENT SET hdFGProcs.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pScanItem W-Win 
PROCEDURE pScanItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.    

    IF cCompany EQ "" THEN
        RETURN.
    
    IF cCustItem EQ "" AND cItemID EQ "" THEN
        RETURN.

    IF NOT VALID-HANDLE(hdItemBins) THEN DO:
        RUN get-link-handle IN adm-broker-hdl(
            INPUT  THIS-PROCEDURE,
            INPUT  "Bins-TARGET",
            OUTPUT cCharHandle
            ).
        hdItemBins = HANDLE(cCharHandle).
    END.
    
    IF NOT VALID-HANDLE(hdItemLoc) THEN DO:
        RUN get-link-handle IN adm-broker-hdl(
            INPUT  THIS-PROCEDURE,
            INPUT  "Loc-TARGET",
            OUTPUT cCharHandle
            ).
        hdItemLoc = HANDLE(cCharHandle).
    END.

    IF NOT VALID-HANDLE(hdItemLocBin) THEN DO:
        RUN get-link-handle IN adm-broker-hdl(
            INPUT  THIS-PROCEDURE,
            INPUT  "LocBin-TARGET",
            OUTPUT cCharHandle
            ).
        hdItemLocBin = HANDLE(cCharHandle).
    END.
    
    IF VALID-HANDLE(hdItemBins) THEN DO:
        lBinsQueryRan = TRUE.
        RUN ScanItem IN hdItemBins (
            INPUT  cCompany,
            INPUT  cWarehouse,
            INPUT  cLocation,
            INPUT  cItemID,
            INPUT  cJobNo,
            INPUT  iJobNo2,
            INPUT  FALSE,  /* Include Zero qty bins */
            INPUT  TRUE,   /* Include empty tag bins */
            OUTPUT cSellUOM,
            OUTPUT lError,
            OUTPUT cMessage
            ).
    END.

    IF VALID-HANDLE(hdItemLoc) THEN DO:
        lLocQueryRan = TRUE.
        RUN ScanItem IN hdItemLoc (
            INPUT  cCompany,
            INPUT  cItemID,
            INPUT  cWarehouse,
            OUTPUT lError,
            OUTPUT cMessage
            ).
    END.

    IF VALID-HANDLE(hdItemLocBin) THEN DO:
        lLocBinsQueryRan = TRUE.
        RUN ScanItem IN hdItemLocBin (
            INPUT  cCompany,
            INPUT  cItemID,
            INPUT  cWarehouse,
            INPUT  cLocation,
            OUTPUT lError,
            OUTPUT cMessage
            ).
    END.
    
    IF lError THEN
        RUN pStatusMessage (cMessage, 3).
            
    ASSIGN
        fiFGItem:SCREEN-VALUE   = cItemID
        fiCustItem:SCREEN-VALUE = cCustItem
        fiSellUOM:SCREEN-VALUE  = cSellUOM
        .
    
    APPLY "ENTRY" TO fiFGItem.
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
            statusMessage:ROW                  = {&WINDOW-NAME}:HEIGHT - .86
            dCol                               = {&WINDOW-NAME}:WIDTH  - 8
            btnExitText:COL                    = dCol - 9
            btnAdjustQtyText:COL               = dCol - btnAdjustQtyText:WIDTH - 1
            btnAdjustQtyText:ROW               = {&WINDOW-NAME}:HEIGHT - .86
            btnClearText:COL                   = dCol - 12
            btClear:COL                        = dCol
            btnSettingsText:ROW                = {&WINDOW-NAME}:HEIGHT - .86
            btnSettingsText:COL                = btnAdjustQtyText:COL - btnSettingsText:WIDTH - 10
            .

        RUN set-position IN h_setting ( {&WINDOW-NAME}:HEIGHT - 1.1 , btnSettingsText:COL + 18 ) NO-ERROR.
        RUN set-position IN h_exit ( 1.00 , dCol ) NO-ERROR.
        RUN get-position IN h_navigatefirst ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
        RUN set-position IN h_navigatefirst ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigateprev ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatenext ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatelast ( dRow , dCol ) NO-ERROR.
        dRow = {&WINDOW-NAME}:HEIGHT - 1.62.
        RUN set-position IN h_adjustqty ( dRow , dCol ) NO-ERROR.
        RUN get-position IN h_b-fginqbins ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
        RUN get-size IN h_b-fginqbins ( OUTPUT dHeight , OUTPUT dWidth ) NO-ERROR.
        ASSIGN
            dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
            dWidth  = dCol - 3
            .
        RUN set-size IN h_b-fginqbins ( dHeight , dWidth ) NO-ERROR.
        RUN set-position IN h_adjustwindowsize ( 1.00 , dCol - 45 ) NO-ERROR.
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScanItem W-Win 
PROCEDURE ScanItem :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWarehouse AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocation  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustItem  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2    AS INTEGER   NO-UNDO.

    ASSIGN
        cCompany   = ipcCompany  
        cWarehouse = ipcWarehouse
        cLocation  = ipcLocation 
        cItemID    = ipcItemID   
        cCustItem  = ipcCustItem 
        cJobNo     = ipcJobNo    
        iJobNo2    = ipiJobNo2
        .   

    RUN pScanItem.
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
 Notes:
------------------------------------------------------------------------------*/
    APPLY "ENTRY" TO fiFGItem IN FRAME {&FRAME-NAME}.

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
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

