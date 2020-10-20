&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: Inventory/w-fgInquiry.w

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
DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/var.i "NEW SHARED"}
{sys/inc/varasgn.i}

{system/sysconst.i}
{Inventory/ttInventory.i "NEW SHARED"}
{methods/defines/sortByDefs.i}
{wip/keyboardDefs.i}

DEFINE VARIABLE cItemID    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustItem  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWarehouse AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSuccess   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lHasAccess AS LOGICAL   NO-UNDO.

DEFINE VARIABLE hdInventoryProcs AS HANDLE NO-UNDO.
RUN Inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.

&SCOPED-DEFINE SORTBY-PHRASE BY ttBrowseInventory.tag

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 10/20/20 -  2:28 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME ttBrowseInventory

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttBrowseInventory

/* Definitions for BROWSE ttBrowseInventory                             */
&Scoped-define FIELDS-IN-QUERY-ttBrowseInventory fGetConcatJob () @ ttBrowseInventory.jobID ttBrowseInventory.poID fGetConcatLocation () @ ttBrowseInventory.locationID ttBrowseInventory.tag ttBrowseInventory.quantity   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttBrowseInventory   
&Scoped-define SELF-NAME ttBrowseInventory
&Scoped-define QUERY-STRING-ttBrowseInventory FOR EACH ttBrowseInventory     WHERE (ttBrowseInventory.warehouseID EQ cWarehouse OR cWarehouse EQ "")       AND (ttBrowseInventory.locationID EQ cLocation OR cLocation EQ "")  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-ttBrowseInventory OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory     WHERE (ttBrowseInventory.warehouseID EQ cWarehouse OR cWarehouse EQ "")       AND (ttBrowseInventory.locationID EQ cLocation OR cLocation EQ "")  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-ttBrowseInventory ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-ttBrowseInventory ttBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-ttBrowseInventory}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiFGItem fiCustItem fiLocation bt-exit ~
btItemHelp btnKeyboardItem btnKeyboardLoc btnNumPad btItemNameHelp ~
btnKeyboardItemName ttBrowseInventory btnFirst btnPrevious btnNext btnLast ~
RECT-33 
&Scoped-Define DISPLAYED-OBJECTS fiFGItem fiCustItem fiLocation 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetConcatJob W-Win 
FUNCTION fGetConcatJob RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetConcatLocation W-Win 
FUNCTION fGetConcatLocation RETURNS CHARACTER
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "" 
     SIZE 11 BY 2.62 TOOLTIP "Exit".

DEFINE BUTTON btAdjustQty 
     LABEL "Adjust" 
     SIZE 11 BY 2.62 TOOLTIP "Adjust Quantity"
     FONT 35.

DEFINE BUTTON btItemHelp 
     IMAGE-UP FILE "C:/Asigui/Environments/Devel/Resources/Graphics/32x32/magnifying_glass.ico":U
     LABEL "" 
     SIZE 8.4 BY 1.57.

DEFINE BUTTON btItemNameHelp 
     IMAGE-UP FILE "C:/Asigui/Environments/Devel/Resources/Graphics/32x32/magnifying_glass.ico":U
     LABEL "" 
     SIZE 8.4 BY 1.52.

DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "Graphics/32x32/navigate_up2.ico":U
     LABEL "First" 
     SIZE 11 BY 2.62 TOOLTIP "First".

DEFINE BUTTON btnKeyboardItem 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U
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

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "Graphics/32x32/navigate_down2.ico":U
     LABEL "Last" 
     SIZE 11 BY 2.62 TOOLTIP "Last".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "Graphics/32x32/navigate_down.ico":U
     LABEL "Next" 
     SIZE 11 BY 2.62 TOOLTIP "Next".

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnPrevious 
     IMAGE-UP FILE "Graphics/32x32/navigate_up.ico":U
     LABEL "Previous" 
     SIZE 11 BY 2.62 TOOLTIP "Previous".

DEFINE VARIABLE fiCustItem AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer Part #" 
     VIEW-AS FILL-IN 
     SIZE 66.2 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiFGItem AS CHARACTER FORMAT "X(256)":U 
     LABEL "FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 66.2 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiLocation AS CHARACTER FORMAT "X(256)":U 
     LABEL "Location" 
     VIEW-AS FILL-IN 
     SIZE 32.2 BY 1.43 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 197.8 BY .1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ttBrowseInventory FOR 
      ttBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE ttBrowseInventory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttBrowseInventory W-Win _FREEFORM
  QUERY ttBrowseInventory DISPLAY
      fGetConcatJob () @ ttBrowseInventory.jobID WIDTH 25 COLUMN-LABEL "Job #" LABEL-BGCOLOR 14
    ttBrowseInventory.poID WIDTH 25 COLUMN-LABEL "PO #" FORMAT ">>>>>>" LABEL-BGCOLOR 14
    fGetConcatLocation () @ ttBrowseInventory.locationID WIDTH 30 COLUMN-LABEL "Location" FORMAT "X(20)" LABEL-BGCOLOR 14
    ttBrowseInventory.tag WIDTH 60 COLUMN-LABEL "Tag #" FORMAT "X(30)" LABEL-BGCOLOR 14
    ttBrowseInventory.quantity WIDTH 25 COLUMN-LABEL "Qty On-Hand" LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-TAB-STOP SIZE 186 BY 27.86
         FONT 36 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiFGItem AT ROW 1.86 COL 26.8 COLON-ALIGNED WIDGET-ID 2
     fiCustItem AT ROW 3.52 COL 26.8 COLON-ALIGNED WIDGET-ID 4
     fiLocation AT ROW 1.86 COL 126.6 COLON-ALIGNED WIDGET-ID 142
     bt-exit AT ROW 1.71 COL 189.8 WIDGET-ID 84 NO-TAB-STOP 
     btItemHelp AT ROW 1.81 COL 96.4 WIDGET-ID 138 NO-TAB-STOP 
     btnKeyboardItem AT ROW 1.81 COL 106.4 WIDGET-ID 136 NO-TAB-STOP 
     btnKeyboardLoc AT ROW 1.81 COL 162.2 WIDGET-ID 144 NO-TAB-STOP 
     btnNumPad AT ROW 2.05 COL 178.6 WIDGET-ID 120 NO-TAB-STOP 
     btItemNameHelp AT ROW 3.48 COL 96.4 WIDGET-ID 140 NO-TAB-STOP 
     btnKeyboardItemName AT ROW 3.48 COL 106.6 WIDGET-ID 134 NO-TAB-STOP 
     ttBrowseInventory AT ROW 5.86 COL 3 WIDGET-ID 200
     btnFirst AT ROW 7.43 COL 189.6 WIDGET-ID 44 NO-TAB-STOP 
     btnPrevious AT ROW 12.91 COL 189.6 WIDGET-ID 40 NO-TAB-STOP 
     btAdjustQty AT ROW 18.86 COL 189.6 WIDGET-ID 110 NO-TAB-STOP 
     btnNext AT ROW 24.38 COL 189.6 WIDGET-ID 42 NO-TAB-STOP 
     btnLast AT ROW 29.81 COL 189.6 WIDGET-ID 46 NO-TAB-STOP 
     RECT-33 AT ROW 5.33 COL 3.2 WIDGET-ID 6
     RECT-2 AT ROW 1.86 COL 177.6 WIDGET-ID 130
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202 BY 32.81
         BGCOLOR 15 FGCOLOR 1 FONT 36 WIDGET-ID 100.


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
         MAX-HEIGHT         = 32.81
         MAX-WIDTH          = 202
         VIRTUAL-HEIGHT     = 32.81
         VIRTUAL-WIDTH      = 202
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* BROWSE-TAB ttBrowseInventory btnKeyboardItemName F-Main */
/* SETTINGS FOR BUTTON btAdjustQty IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnKeyboardItem:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboardItemName:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboardLoc:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       ttBrowseInventory:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttBrowseInventory
/* Query rebuild information for BROWSE ttBrowseInventory
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory
    WHERE (ttBrowseInventory.warehouseID EQ cWarehouse OR cWarehouse EQ "")
      AND (ttBrowseInventory.locationID EQ cLocation OR cLocation EQ "")  ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttBrowseInventory */
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
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit W-Win
ON CHOOSE OF bt-exit IN FRAME F-Main
DO:
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.

    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE OBJECT hdInventoryProcs.

    APPLY "CLOSE":U TO THIS-PROCEDURE.
    
    RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAdjustQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAdjustQty W-Win
ON CHOOSE OF btAdjustQty IN FRAME F-Main /* Adjust */
DO:
    /* If not automatically cleared by security level, ask for password */
    IF NOT lHasAccess THEN DO:
        RUN sys/ref/d-passwd.w (
            INPUT  10, 
            OUTPUT lHasAccess
            ). 
    END.

    IF NOT lHasAccess THEN
        RETURN.

    RUN pAdjustQty.    
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


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst W-Win
ON CHOOSE OF btnFirst IN FRAME F-Main /* First */
DO:
    RUN pNavigate (SELF).
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


&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast W-Win
ON CHOOSE OF btnLast IN FRAME F-Main /* Last */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext W-Win
ON CHOOSE OF btnNext IN FRAME F-Main /* Next */
DO:
    RUN pNavigate (SELF).
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


&Scoped-define SELF-NAME btnPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevious W-Win
ON CHOOSE OF btnPrevious IN FRAME F-Main /* Previous */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCustItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustItem W-Win
ON ENTRY OF fiCustItem IN FRAME F-Main /* Customer Part # */
DO:
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
ON HELP OF fiCustItem IN FRAME F-Main /* Customer Part # */
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
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustItem W-Win
ON LEAVE OF fiCustItem IN FRAME F-Main /* Customer Part # */
DO:
    IF cCustItem EQ SELF:SCREEN-VALUE AND cCustItem NE "" THEN
        RETURN.

    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    ASSIGN        
        cCustItem             = SELF:SCREEN-VALUE
        fiFGItem:SCREEN-VALUE = ""
        cItemID               = ""
        .

    RUN pItemScan.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFGItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFGItem W-Win
ON ENTRY OF fiFGItem IN FRAME F-Main /* FG Item# */
DO:
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
ON HELP OF fiFGItem IN FRAME F-Main /* FG Item# */
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
    END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFGItem W-Win
ON LEAVE OF fiFGItem IN FRAME F-Main /* FG Item# */
DO:
    IF cItemID EQ SELF:SCREEN-VALUE AND cItemID NE "" THEN
        RETURN.

    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    ASSIGN        
        cItemID                 = SELF:SCREEN-VALUE
        fiCustItem:SCREEN-VALUE = ""
        cCustItem               = ""
        .
    
    RUN pItemScan.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON LEAVE OF fiLocation IN FRAME F-Main /* Location */
DO:    
    ASSIGN
        cWarehouse = SUBSTRING(SELF:SCREEN-VALUE, 1, 5)
        cLocation  = SUBSTRING(SELF:SCREEN-VALUE, 6)
        .        

    {&OPEN-QUERY-{&BROWSE-NAME}}   

    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttBrowseInventory
&Scoped-define SELF-NAME ttBrowseInventory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttBrowseInventory W-Win
ON START-SEARCH OF ttBrowseInventory IN FRAME F-Main
DO:
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
        
        IF cColumnLabel EQ cSaveLabel THEN
            lAscending = NOT lAscending.
        IF VALID-HANDLE(hSaveLabel) THEN
            hSaveLabel:LABEL-BGCOLOR = ?.
    
        ASSIGN
            hColumnLabel = {&BROWSE-NAME}:CURRENT-COLUMN
            hColumnLabel:LABEL-BGCOLOR = 14
            hSaveLabel = hColumnLabel
            cSaveLabel = cColumnLabel
            .
        RUN pReopenBrowse.
    END.
    
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttBrowseInventory W-Win
ON VALUE-CHANGED OF ttBrowseInventory IN FRAME F-Main
DO:
    btAdjustQty:SENSITIVE = AVAILABLE ttBrowseInventory.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

{wip/pNavigate.i}
{wip/pKeyboard.i}

&Scoped-define sdBrowseName ttBrowseInventory
{methods/sortByProc.i "pByQuantity" "ttBrowseInventory.quantity"}
{methods/sortByProc.i "pByLocationID" "ttBrowseInventory.locationID"}
{methods/sortByProc.i "pByTag" "ttBrowseInventory.tag"}
{methods/sortByProc.i "pByJobID" "ttBrowseInventory.jobID"}
{methods/sortByProc.i "pByPOID" "ttBrowseInventory.poID"}

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
  DISPLAY fiFGItem fiCustItem fiLocation 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE fiFGItem fiCustItem fiLocation bt-exit btItemHelp btnKeyboardItem 
         btnKeyboardLoc btnNumPad btItemNameHelp btnKeyboardItemName 
         ttBrowseInventory btnFirst btnPrevious btnNext btnLast RECT-33 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
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
    RUn pInit.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAdjustQty W-Win 
PROCEDURE pAdjustQty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTotalQuantity   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSubUnitCount    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSubUnitsPerUnit AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dPartialQuantity AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cAdjReasonCode   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValueReturned   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dValue           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    
    IF AVAILABLE ttBrowseInventory THEN DO:
        RUN inventory/adjustQuantity.w (
            INPUT  ttBrowseInventory.quantity,
            INPUT  ttBrowseInventory.quantityOfSubUnits,
            INPUT  ttBrowseInventory.quantityPerSubUnit,
            INPUT  TRUE, /* Required Adj Reason  */
            OUTPUT dTotalQuantity,
            OUTPUT dSubUnitCount,
            OUTPUT dSubUnitsPerUnit,
            OUTPUT dPartialQuantity,
            OUTPUT cAdjReasonCode,
            OUTPUT lValueReturned,
            OUTPUT dValue
            ).

        IF lValueReturned THEN DO:
            IF ttBrowseInventory.quantity EQ dTotalQuantity THEN DO:
                MESSAGE "Adjusted quantity for tag " + ttBrowseInventory.tag +
                        " is same as existing quantity" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.

            MESSAGE "Adjust quantity of tag " + ttBrowseInventory.tag +
                    " to " + STRING(dTotalQuantity) "?" VIEW-AS ALERT-BOX QUESTION
                    BUTTON OK-CANCEL
                    TITLE "Adjust Quantity" UPDATE lContinue AS LOGICAL.
            IF lContinue THEN DO:
                RUN Inventory_AdjustFinishedGoodBinQty IN hdInventoryProcs (
                    INPUT  TO-ROWID(ttBrowseInventory.inventoryStockID), 
                    INPUT  dTotalQuantity - ttBrowseInventory.quantity,
                    INPUT  dPartialQuantity - ttBrowseInventory.quantityPartial,
                    INPUT  cAdjReasonCode,
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ).           
                
                IF NOT lSuccess THEN
                    MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
                ELSE
                    ttBrowseInventory.quantity = dTotalQuantity.
                    
                {&OPEN-QUERY-{&BROWSE-NAME}}   
            
                APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.                                
            END.          
        END.        
    END.    
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
    DEFINE VARIABLE hdPgmSecurity AS HANDLE  NO-UNDO.

    FIND FIRST company NO-LOCK 
         WHERE company.company EQ ipcCompany
         NO-ERROR .
    IF AVAILABLE company THEN
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                         + " - {&awversion}" + " - " 
                         + STRING(company.name) + " - " + ipcLocation.

    
    RUN "system/PgmMstrSecur.p" PERSISTENT SET hdPgmSecurity.

    RUN epCanAccess IN hdPgmSecurity (
        INPUT  "browsers/rm-ibin.w", 
        INPUT  "", 
        OUTPUT lHasAccess
        ).
        
    DELETE OBJECT hdPgmSecurity.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pItemScan W-Win 
PROCEDURE pItemScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    EMPTY TEMP-TABLE ttBrowseInventory.
    
    RUN Inventory_BuildFGBinForItem IN hdInventoryProcs (
        INPUT        ipcCompany,
        INPUT-OUTPUT cItemID,
        INPUT-OUTPUT cCustItem,
        INPUT        FALSE,  /* Include Zero qty bins */
        INPUT        TRUE,   /* Include empty tag bins */
        OUTPUT       lSuccess,
        OUTPUT       cMessage
        ).
    
    IF NOT lSuccess THEN
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.

    ASSIGN
        fiFGItem:SCREEN-VALUE   = cItemID
        fiCustItem:SCREEN-VALUE = cCustItem
        .

    {&OPEN-QUERY-{&BROWSE-NAME}}

    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse W-Win 
PROCEDURE pReopenBrowse PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CASE cColumnLabel:
        WHEN "quantity" THEN
            RUN pByQuantity.
        WHEN "locationID" THEN
            RUN pByLocationID.
        WHEN "tag" THEN
            RUN pByTag.
        WHEN "jobID" THEN
            RUN pByJobID.
        WHEN "poID" THEN
            RUN pByPOID.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.

    IF AVAILABLE ttBrowseInventory THEN
        APPLY "VALUE-CHANGED":U TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.

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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ttBrowseInventory"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetConcatJob W-Win 
FUNCTION fGetConcatJob RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cConcatJob AS CHARACTER NO-UNDO.
       
    IF AVAILABLE ttBrowseInventory AND ttBrowseInventory.jobID NE "" THEN DO:
        cConcatJob = ttBrowseInventory.jobID 
                   + FILL(" ", 6 - LENGTH(ttBrowseInventory.jobID)) 
                   + "-"
                   + STRING(ttBrowseInventory.jobID2,"99").
    END.
    
    RETURN cConcatJob.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetConcatLocation W-Win 
FUNCTION fGetConcatLocation RETURNS CHARACTER
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cConcatLocation AS CHARACTER NO-UNDO.
       
    IF AVAILABLE ttBrowseInventory THEN
        cConcatLocation = ttBrowseInventory.warehouseID 
                        + FILL(" ", 5 - LENGTH(ttBrowseInventory.warehouseID)) 
                        + ttBrowseInventory.locationID.

    RETURN cConcatLocation.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

