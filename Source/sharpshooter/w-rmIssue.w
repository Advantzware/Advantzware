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

  File: sharpshooter/w-rmIssue.w

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
{Inventory/ttInventory.i}
{Inventory/ttBrowseInventory.i}
{Inventory/ttInventoryStockDetails.i}
{methods/template/brwcustomdef.i}
{methods/defines/sortByDefs.i}

/* The following includes are required as few programs down the line using shared variables */
{custom/globdefs.i}
{sys/inc/var.i "NEW SHARED"}
{sys/inc/varasgn.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
 
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUser    AS CHARACTER NO-UNDO.

/* Required for run_link.i */
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.
DEFINE VARIABLE cJobNo   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iJobNo2  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iFormNo  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBlankNo AS INTEGER   NO-UNDO.

DEFINE VARIABLE hdInventoryProcs        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdJobProcs              AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdQuantityColumnLabel   AS HANDLE    NO-UNDO.
DEFINE VARIABLE lMoveToOnhand           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iTotTags                AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotOnHand              AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCount                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cValidateJobno          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilterBy               AS CHARACTER NO-UNDO.
DEFINE VARIABLE iWarehouseLength        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cStatusMessage          AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStatusMessageType      AS INTEGER   NO-UNDO.

DEFINE VARIABLE glAutoPost            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcShowSettings        AS CHARACTER NO-UNDO.
DEFINE VARIABLE glShowVirtualKeyboard AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcSSIssueDefaultRM    AS CHARACTER NO-UNDO.

DEFINE VARIABLE oKeyboard  AS system.Keyboard   NO-UNDO.
DEFINE VARIABLE oLoadTag   AS Inventory.Loadtag NO-UNDO.

RUN spGetSessionParam ("Company", OUTPUT cCompany).
    
oKeyboard = NEW system.Keyboard().

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
&Scoped-define BROWSE-NAME br-table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttBrowseInventory

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table ttBrowseInventory.quantity fGetConcatLocationID() @ ttBrowseInventory.warehouseID ttBrowseInventory.tag fGetConcatJobID() @ ttBrowseInventory.jobID ttBrowseInventory.inventoryStatus   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH ttBrowseInventory     WHERE ttBrowseInventory.inventoryStatus EQ cFilterBy     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory     WHERE ttBrowseInventory.inventoryStatus EQ cFilterBy     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br-table ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ttBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btJobReset btPost cbRMItem btTotal btScanned ~
btConsumed br-table btClear btnNumPad btnExitText btnClearText ~
btnAdjustQtyText btnSettingsText statusMessage 
&Scoped-Define DISPLAYED-OBJECTS cbRMItem fiUOM fiTotalQty fiScannedQty ~
fiConsumedQty btnExitText btnClearText btnAdjustQtyText btnSettingsText ~
statusMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetConcatJobID W-Win 
FUNCTION fGetConcatJobID RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetConcatLocationID W-Win 
FUNCTION fGetConcatLocationID RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_adjustqty AS HANDLE NO-UNDO.
DEFINE VARIABLE h_adjustwindowsize AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatefirst AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatelast AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatenext AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigateprev AS HANDLE NO-UNDO.
DEFINE VARIABLE h_setting AS HANDLE NO-UNDO.
DEFINE VARIABLE h_tagfilter AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btClear 
     IMAGE-UP FILE "Graphics/32x32/back_white.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/back_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btConsumed 
     LABEL "Consumed: 0" 
     SIZE 30 BY 2 TOOLTIP "Filter Consumed Tags".

DEFINE BUTTON btJobReset 
     IMAGE-UP FILE "Graphics/32x32/back_white.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/back_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btPost 
     LABEL "Post" 
     SIZE 19 BY 2.38
     FONT 37.

DEFINE BUTTON btScanned 
     LABEL "Scanned: 0" 
     SIZE 30 BY 2 TOOLTIP "Filter Scanned Tags".

DEFINE BUTTON btTotal 
     LABEL "On Hand: 0" 
     SIZE 30 BY 2 TOOLTIP "Filter All Tags".

DEFINE VARIABLE cbRMItem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 38 BY 1
     BGCOLOR 15 FGCOLOR 0 FONT 37 NO-UNDO.

DEFINE VARIABLE btnAdjustQtyText AS CHARACTER FORMAT "X(256)":U INITIAL "ADJUST QTY" 
      VIEW-AS TEXT 
     SIZE 22.8 BY 1.43
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

DEFINE VARIABLE fiConsumedQty AS CHARACTER FORMAT "X(256)":U INITIAL "Qty: 0" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     FONT 19 NO-UNDO.

DEFINE VARIABLE fiScannedQty AS CHARACTER FORMAT "X(256)":U INITIAL "Qty: 0" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     FONT 19 NO-UNDO.

DEFINE VARIABLE fiTotalQty AS CHARACTER FORMAT "X(256)":U INITIAL "Qty: 0" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     FONT 19.

DEFINE VARIABLE fiUOM AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.29
     FONT 36 NO-UNDO.

DEFINE VARIABLE statusMessage AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS MESSAGE" 
      VIEW-AS TEXT 
     SIZE 142.2 BY 1.43 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      ttBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table W-Win _FREEFORM
  QUERY br-table DISPLAY
      ttBrowseInventory.quantity WIDTH 25 COLUMN-LABEL "Qty On-hand" FORMAT "->,>>>,>>9.99<<<<"
      fGetConcatLocationID() @ ttBrowseInventory.warehouseID WIDTH 30 COLUMN-LABEL "Location" FORMAT "X(12)"
      ttBrowseInventory.tag WIDTH 70 COLUMN-LABEL "Tag #" FORMAT "X(30)"
      fGetConcatJobID() @ ttBrowseInventory.jobID WIDTH 30 COLUMN-LABEL "Job #" FORMAT "X(20)"
      ttBrowseInventory.inventoryStatus COLUMN-LABEL "Status" FORMAT "X(15)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 198 BY 9.52
         BGCOLOR 15 FGCOLOR 0 FONT 38 ROW-HEIGHT-CHARS .86 FIT-LAST-COLUMN TOOLTIP "Double click a tag to update status".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btJobReset AT ROW 3.38 COL 151.4 WIDGET-ID 180
     btPost AT ROW 3.24 COL 160.6 WIDGET-ID 38
     cbRMItem AT ROW 5.52 COL 16.6 COLON-ALIGNED NO-LABEL WIDGET-ID 152
     fiUOM AT ROW 5.62 COL 73.6 COLON-ALIGNED NO-LABEL WIDGET-ID 176
     btTotal AT ROW 6.48 COL 108 WIDGET-ID 162 NO-TAB-STOP 
     btScanned AT ROW 6.48 COL 139.6 WIDGET-ID 164
     btConsumed AT ROW 6.48 COL 171.4 WIDGET-ID 166
     fiTotalQty AT ROW 8.76 COL 106.2 COLON-ALIGNED NO-LABEL WIDGET-ID 170
     fiScannedQty AT ROW 8.76 COL 137.6 COLON-ALIGNED NO-LABEL WIDGET-ID 174
     fiConsumedQty AT ROW 8.76 COL 169.6 COLON-ALIGNED NO-LABEL WIDGET-ID 172
     br-table AT ROW 10.05 COL 1.4 WIDGET-ID 200
     btClear AT ROW 3.38 COL 194.8 WIDGET-ID 146
     btnNumPad AT ROW 3.48 COL 182.8 WIDGET-ID 120 NO-TAB-STOP 
     btnExitText AT ROW 1.24 COL 189 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     btnClearText AT ROW 3.57 COL 182 NO-LABEL WIDGET-ID 148
     btnAdjustQtyText AT ROW 19.81 COL 148 COLON-ALIGNED NO-LABEL WIDGET-ID 182
     btnSettingsText AT ROW 19.81 COL 180.2 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     statusMessage AT ROW 19.95 COL 2.8 NO-LABEL WIDGET-ID 66
     "UOM:" VIEW-AS TEXT
          SIZE 10.4 BY .95 AT ROW 5.76 COL 65 WIDGET-ID 178
     "RM ITEM:" VIEW-AS TEXT
          SIZE 17 BY .95 AT ROW 5.76 COL 1.6 WIDGET-ID 154
     RECT-2 AT ROW 3.29 COL 181.8 WIDGET-ID 130
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 207.8 BY 20.62
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Issue Materials"
         HEIGHT             = 20.62
         WIDTH              = 207.8
         MAX-HEIGHT         = 31.24
         MAX-WIDTH          = 236.8
         VIRTUAL-HEIGHT     = 31.24
         VIRTUAL-WIDTH      = 236.8
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
   FRAME-NAME                                                           */
/* BROWSE-TAB br-table fiConsumedQty F-Main */
ASSIGN 
       br-table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN btnClearText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiConsumedQty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScannedQty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotalQty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiUOM IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN statusMessage IN FRAME F-Main
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory
    WHERE ttBrowseInventory.inventoryStatus EQ cFilterBy
    ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Issue Materials */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Issue Materials */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table W-Win
ON DEFAULT-ACTION OF br-table IN FRAME F-Main
DO:
    IF AVAILABLE ttBrowseInventory AND ttBrowseInventory.tag NE "" THEN
        RUN pTagScan (cCompany, ttBrowseInventory.tag).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table W-Win
ON ROW-DISPLAY OF br-table IN FRAME F-Main
DO:
    {methods/template/brwrowdisplay.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table W-Win
ON START-SEARCH OF br-table IN FRAME F-Main
DO:
        {methods/template/sortindicator.i} 
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
        {methods/template/sortindicatorend.i} 
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btClear W-Win
ON CHOOSE OF btClear IN FRAME F-Main /* Reset */
DO:

    RUN pStatusMessage ("", 0).
    {methods/run_link.i "JOB-SOURCE" "Reset"}
    {methods/run_link.i "JOB-SOURCE" "EnableAll"}
    {methods/run_link.i "JOB-SOURCE" "Set-Focus"}
    
    {methods/run_link.i "TAG-SOURCE" "EmptyTag"}
    {methods/run_link.i "TAG-SOURCE" "DisableAll"}
    
    EMPTY TEMP-TABLE ttBrowseInventory.
    
    ASSIGN
        cbRMItem:LIST-ITEMS   = ""
        cbRMItem:SCREEN-VALUE = ""
        fiConsumedQty         = "Qty: 0"
        fiScannedQty          = "Qty: 0"
        fiTotalQty            = "Qty: 0"
        btConsumed:LABEL      = "Consumed: 0"
        btScanned:LABEL       = "Scanned: 0"
        btTotal:LABEL         = "OnHand: 0"               
        .
        
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btConsumed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConsumed W-Win
ON CHOOSE OF btConsumed IN FRAME F-Main /* Consumed: 0 */
DO:
    RUN pHighlightSelection (
        INPUT gcStatusStockConsumed
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btJobReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btJobReset W-Win
ON CHOOSE OF btJobReset IN FRAME F-Main /* Reset */
DO:
    RUN pStatusMessage ("", 0).
    {methods/run_link.i "JOB-SOURCE" "EnableAll"}
    {methods/run_link.i "JOB-SOURCE" "Set-Focus"}
    
    {methods/run_link.i "TAG-SOURCE" "EmptyTag"}
    {methods/run_link.i "TAG-SOURCE" "DisableAll"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdjustQtyText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdjustQtyText W-Win
ON MOUSE-SELECT-CLICK OF btnAdjustQtyText IN FRAME F-Main
DO:
    RUN AdjustQuantity.
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


&Scoped-define SELF-NAME btnSettingsText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSettingsText W-Win
ON MOUSE-SELECT-CLICK OF btnSettingsText IN FRAME F-Main
DO:
    RUN OpenSetting.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPost W-Win
ON CHOOSE OF btPost IN FRAME F-Main /* Post */
DO:
    DEFINE VARIABLE lSuccess  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lChoice   AS LOGICAL   NO-UNDO.
    
    FIND FIRST ttBrowseInventory 
         WHERE ttBrowseInventory.inventoryStatus EQ gcStatusStockScanned
         NO-ERROR.
    IF NOT AVAILABLE ttBrowseInventory THEN DO:
        RUN sharpShooter/messageDialog.w (
            INPUT  "No records available to post",
            INPUT  TRUE,
            INPUT  FALSE,
            INPUT  FALSE,
            OUTPUT lChoice
            ).

        RETURN NO-APPLY.
    END.
    
    RUN Inventory_PostRawMaterials IN hdInventoryProcs (
        INPUT  cCompany,
        INPUT  TODAY,
        OUTPUT lSuccess,
        OUTPUT cMessage,
        INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
        ).
    IF lSuccess THEN
        RUN pStatusMessage ("Posting completed", 1).
    ELSE
        RUN pStatusMessage (cMessage, 3).
    
    RUN pRebuildBrowse (
        INPUT cCompany,
        INPUT cJobNo,
        INPUT iJobNo2,
        INPUT iFormNo,
        INPUT iBlankNo,
        INPUT cbRMItem:SCREEN-VALUE,
        INPUT FALSE
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btScanned
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btScanned W-Win
ON CHOOSE OF btScanned IN FRAME F-Main /* Scanned: 0 */
DO:
    RUN pHighlightSelection (
        INPUT gcStatusStockScanned
        ).        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTotal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTotal W-Win
ON CHOOSE OF btTotal IN FRAME F-Main /* On Hand: 0 */
DO:
    RUN pHighlightSelection (
        INPUT gcStatusStockReceived
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbRMItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbRMItem W-Win
ON VALUE-CHANGED OF cbRMItem IN FRAME F-Main
DO:
    IF SELF:SCREEN-VALUE NE ? AND SELF:SCREEN-VALUE NE "" THEN
        {methods/run_link.i "JOB-SOURCE" "DisableAll"}        

    RUN pStatusMessage ("", 0).
    
    {methods/run_link.i "TAG-SOURCE" "EmptyTag"}
    {methods/run_link.i "TAG-SOURCE" "EnableAll"}
    {methods/run_link.i "TAG-SOURCE" "Set-Focus"}

    RUN pRebuildBrowse (
        INPUT cCompany,
        INPUT cJobNo,
        INPUT iJobNo2,
        INPUT iFormNo,
        INPUT iBlankNo,
        INPUT cbRMItem:SCREEN-VALUE,
        INPUT TRUE
        ).
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
{sharpshooter/smartobj/browseNavigate.i}
{methods/sortByProc.i "pByQuantity" "ttBrowseInventory.quantity"}
{methods/sortByProc.i "pByQuantityOriginal" "ttBrowseInventory.quantityOriginal"}
{methods/sortByProc.i "pByLocationID" "ttBrowseInventory.locationID"}
{methods/sortByProc.i "pByTag" "ttBrowseInventory.tag"}
{methods/sortByProc.i "pByJobID" "ttBrowseInventory.jobID"}
{methods/sortByProc.i "pByInventoryStatus" "ttBrowseInventory.inventoryStatus"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AdjustQuantity W-Win 
PROCEDURE AdjustQuantity:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lValueReturned  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dIssueQuantity  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-rm-bin  FOR rm-bin.
    DEFINE BUFFER bf-rm-rctd FOR rm-rctd.
     
    IF AVAILABLE ttBrowseInventory AND ttBrowseInventory.inventoryStatus EQ gcStatusStockScanned THEN DO:
        RUN inventory/adjustQuantityIssue2.w (
            INPUT  ttBrowseInventory.quantity,
            INPUT  ttBrowseInventory.quantityUOM,
            OUTPUT lValueReturned,
            OUTPUT dIssueQuantity
            ).
        
        IF NOT lValueReturned THEN
            RETURN.
        
        IF dIssueQuantity EQ ttBrowseInventory.quantity THEN DO:
            RUN pStatusMessage("ENTERED QUANTITY IS SAME AS EXISTING QUANTITY", 2).
            
            RETURN.
        END.
        
        IF dIssueQuantity LE 0 THEN DO:
            RUN pStatusMessage("QUANTITY TO ISSUE HAS TO BE GREATER THAN 0", 3).
            
            RETURN.        
        END.

        RUN Inventory_AdjustRMIssueTransactionQuantity IN hdInventoryProcs (
            TO-ROWID(ttBrowseInventory.inventoryStockID),
            dIssueQuantity,
            OUTPUT lError,
            OUTPUT cMessage
            ). 
        
        RUN pStatusMessage(cMessage, IF lError THEN 3 ELSE 1).   
            
        IF NOT lError THEN
            ASSIGN
                ttBrowseInventory.quantity      = dIssueQuantity
                ttBrowseInventory.lastTransTime = NOW
                .
        
        {&OPEN-QUERY-{&BROWSE-NAME}}            
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
       RUN set-position IN h_adjustwindowsize ( 1.00 , 158.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 32.00 ) */

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 200.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/jobfilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_jobfilter ).
       RUN set-position IN h_jobfilter ( 3.38 , 7.80 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 143.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/tagfilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_tagfilter ).
       RUN set-position IN h_tagfilter ( 7.10 , 7.80 ) NO-ERROR.
       /* Size in UIB:  ( 2.29 , 85.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatefirst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatefirst ).
       RUN set-position IN h_navigatefirst ( 10.10 , 200.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigateprev.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigateprev ).
       RUN set-position IN h_navigateprev ( 12.29 , 200.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatenext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatenext ).
       RUN set-position IN h_navigatenext ( 15.10 , 200.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatelast.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatelast ).
       RUN set-position IN h_navigatelast ( 17.43 , 200.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/setting.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_setting ).
       RUN set-position IN h_setting ( 19.57 , 200.80 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.60 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/adjustqty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_adjustqty ).
       RUN set-position IN h_adjustqty ( 19.67 , 173.40 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       /* Links to SmartObject h_jobfilter. */
       RUN add-link IN adm-broker-hdl ( h_jobfilter , 'JOB':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_jobfilter , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_tagfilter. */
       RUN add-link IN adm-broker-hdl ( h_tagfilter , 'State':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_tagfilter , 'TAG':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_navigatefirst. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'NAV-FIRST':U , h_navigatefirst ).

       /* Links to SmartObject h_navigateprev. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'NAV-PREV':U , h_navigateprev ).

       /* Links to SmartObject h_navigatenext. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'NAV-NEXT':U , h_navigatenext ).

       /* Links to SmartObject h_navigatelast. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'NAV-LAST':U , h_navigatelast ).

       /* Links to SmartObject h_setting. */
       RUN add-link IN adm-broker-hdl ( h_setting , 'SETTING':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_adjustqty. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'ADJUST':U , h_adjustqty ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_adjustwindowsize , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_jobfilter ,
             btPost:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_tagfilter ,
             btConsumed:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatefirst ,
             br-table:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigateprev ,
             h_navigatefirst , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatelast ,
             h_navigatenext , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_setting ,
             h_navigatelast , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_adjustqty ,
             h_setting , 'AFTER':U ).
    END. /* Page 1 */

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
  DISPLAY cbRMItem fiUOM fiTotalQty fiScannedQty fiConsumedQty btnExitText 
          btnClearText btnAdjustQtyText btnSettingsText statusMessage 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btJobReset btPost cbRMItem btTotal btScanned btConsumed br-table 
         btClear btnNumPad btnExitText btnClearText btnAdjustQtyText 
         btnSettingsText statusMessage 
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
  IF VALID-OBJECT(oKeyboard) THEN
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHighlightSelection W-Win 
PROCEDURE pHighlightSelection PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFilterType AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    cFilterBy = ipcFilterType.

    {methods/run_link.i "ADJUST-TARGET" "HideAdjustQuantity"}
    btnAdjustQtyText:VISIBLE = FALSE.
    
    CASE ipcFilterType:
        WHEN gcStatusStockReceived THEN
            hdQuantityColumnLabel:LABEL = "Qty On-Hand".
        WHEN gcStatusStockConsumed THEN
            hdQuantityColumnLabel:LABEL = "Qty Issued".
        WHEN gcStatusStockScanned THEN DO:
            hdQuantityColumnLabel:LABEL = "Qty Scanned".
            {methods/run_link.i "ADJUST-TARGET" "ShowAdjustQuantity"}
            btnAdjustQtyText:VISIBLE = TRUE.
        END.
    END.
    
    {&OPEN-QUERY-{&BROWSE-NAME}}
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
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
    DEFINE VARIABLE cReturnValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cSettingValue AS CHARACTER NO-UNDO.     
    DEFINE VARIABLE hdBrowse      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iColumn       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hdColumn      AS HANDLE    NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN spGetSessionParam("UserID", OUTPUT cUser).
    RUN pStatusMessage ("", 0).

    RUN spGetSettingByName ("ShowVirtualKeyboard", OUTPUT cSettingValue).
    glShowVirtualKeyboard = LOGICAL(cSettingValue) NO-ERROR.
    
    RUN spGetSettingByName ("ShowSettings", OUTPUT gcShowSettings).
    RUN spGetSettingByName ("SSIssueDefaultRM", OUTPUT gcSSIssueDefaultRM).
    RUN spGetSettingByName ("AutoPost", OUTPUT cSettingValue).
    glAutoPost = LOGICAL(cSettingValue).
    
    oKeyboard:SetWindow({&WINDOW-NAME}:HANDLE).
    oKeyboard:SetProcedure(THIS-PROCEDURE).
    oKeyboard:SetFrame(FRAME {&FRAME-NAME}:HANDLE).
    
    {methods/run_link.i "JOB-SOURCE" "Set-Focus"}
    {methods/run_link.i "TAG-SOURCE" "DisableAll"}
    {methods/run_link.i "JOB-SOURCE" "DisableErrorAlerts"}
    {methods/run_link.i "JOB-SOURCE" "DisplayForMaterials"}
    {methods/run_link.i "TAG-SOURCE" "DisableErrorAlerts"}

    ASSIGN
        btnSettingsText:VISIBLE          = INDEX(gcShowSettings, "Text") GT 0
        btnNumPad:VISIBLE                = glShowVirtualKeyboard
        RECT-2:VISIBLE                   = glShowVirtualKeyboard
        {&BROWSE-NAME}:BGCOLOR           = 25
        {&BROWSE-NAME}:FGCOLOR           = 0
        {&BROWSE-NAME}:SEPARATOR-FGCOLOR = 15
        {&BROWSE-NAME}:ROW-HEIGHT-CHARS  = 1
        {&BROWSE-NAME}:FONT              = 36
        {&BROWSE-NAME}:FIT-LAST-COLUMN   = TRUE
        btPost:HIDDEN                    = glAutoPost
        .  
        
    IF INDEX(gcShowSettings, "Icon") EQ 0 THEN
        {methods/run_link.i "Setting-SOURCE" "HideSettings"}        

    hdBrowse = BROWSE {&BROWSE-NAME}:HANDLE.

    {methods/run_link.i "ADJUST-TARGET" "HideAdjustQuantity"}
    btnAdjustQtyText:VISIBLE = FALSE.
    
    DO iColumn = 1 TO hdBrowse:NUM-COLUMNS :
        hdColumn = hdBrowse:GET-BROWSE-COLUMN (iColumn).
        
        IF hdColumn:NAME EQ "quantity" THEN DO:
            hdQuantityColumnLabel = hdColumn.
            LEAVE.
        END.
    END.
    
    cFilterBy = gcStatusStockReceived.
    
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.
    
    RUN Inventory_GetWarehouseLength IN hdInventoryProcs (
        INPUT  cCompany,
        OUTPUT iWarehouseLength
        ).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRebuildBrowse W-Win 
PROCEDURE pRebuildBrowse PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankno    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcRMItem     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplRebuild    AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE iScannedTags  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iConsumedTags AS INTEGER NO-UNDO.
    DEFINE VARIABLE iOnHandTags   AS INTEGER NO-UNDO.
    DEFINE VARIABLE dScannedQty   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dConsumedQty  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOnHandQty    AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE cItemName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRMItem   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cConsUOM  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError    AS LOGICAL   NO-UNDO.
    
    
    cRMItem = ipcRMItem.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF ipcRMItem EQ ? THEN
        ipcRMItem = "".

    IF iplRebuild THEN DO:        
        RUN Inventory_BuildRMBinForItem IN hdInventoryProcs (
            INPUT        ipcCompany,
            INPUT        "",
            INPUT        "",        
            INPUT-OUTPUT cRMItem,
            INPUT-OUTPUT cItemName,
            INPUT        ipcJobNo,
            INPUT        ipiJobNo2,
            INPUT        TRUE,   /* Include Zero qty bins */
            INPUT        TRUE,   /* Include empty tag bins */
            OUTPUT       cConsUOM,
            OUTPUT       lError,
            OUTPUT       cMessage,
            INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
            ).

        RUN Inventory_BuildRMTransactions IN hdInventoryProcs (
            INPUT  ipcCompany,
            INPUT  ipcJobno,
            INPUT  "", /* Blank Machine Code */
            INPUT  ipiJobno2,
            INPUT  ipiFormno,
            INPUT  ipiBlankno,
            INPUT  ipcRMItem,
            INPUT  "I",  /* Issue Transactions */
            INPUT  FALSE, /* Empty existing temp-table records */
            INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
            ).

        RUN Inventory_BuildRMHistory IN hdInventoryProcs (
            INPUT ipcCompany,
            INPUT ipcRMItem,
            INPUT "", /* Warehouse */
            INPUT "", /* Location */
            INPUT ipcJobNo,
            INPUT ipiJobNo2,
            INPUT "I",
            INPUT FALSE, /* Empty existing temp-table records */
            INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
            ).                    
    END.
    
    RUN Inventory_CalculateTagCountInTTbrowse IN hdInventoryProcs (
        INPUT  gcStatusStockConsumed,
        OUTPUT iConsumedTags,
        INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
        ).

    RUN Inventory_CalculateTagCountInTTbrowse IN hdInventoryProcs (
        INPUT  gcStatusStockScanned,
        OUTPUT iScannedTags,
        INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
        ).

    RUN Inventory_CalculateTagCountInTTbrowse IN hdInventoryProcs (
        INPUT  gcStatusStockReceived,
        OUTPUT iOnHandTags,
        INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
        ).

    RUN Inventory_CalculateTagQuantityInTTbrowse IN hdInventoryProcs (
        INPUT  gcStatusStockConsumed,
        OUTPUT dConsumedQty,
        INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
        ).

    RUN Inventory_CalculateTagQuantityInTTbrowse IN hdInventoryProcs (
        INPUT  gcStatusStockScanned,
        OUTPUT dScannedQty,
        INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
        ).        

    RUN Inventory_CalculateTagQuantityInTTbrowse IN hdInventoryProcs (
        INPUT  gcStatusStockReceived,
        OUTPUT dOnHandQty,
        INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
        ).

    ASSIGN
        btTotal:LABEL    = "On Hand: " + STRING(iOnHandTags)
        btScanned:LABEL  = "Scanned: " + STRING(iScannedTags)
        btConsumed:LABEL = "Consumed: " + STRING(iConsumedTags)
        .

    ASSIGN
        fiTotalQty:SCREEN-VALUE    = "Qty: " + STRING(dOnHandQty)
        fiScannedQty:SCREEN-VALUE  = "Qty: " + STRING(dScannedQty)
        fiConsumedQty:SCREEN-VALUE = "Qty: " + STRING(dConsumedQty)
        fiUOM:SCREEN-VALUE         = cConsUOM
        .
    
    {&OPEN-QUERY-{&BROWSE-NAME}}   
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse W-Win
PROCEDURE pReopenBrowse PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    CASE cColumnLabel:
        WHEN "quantity" THEN
            RUN pByQuantity.
        WHEN "quantityOriginal" THEN
            RUN pByQuantityOriginal.
        WHEN "locationID" THEN
            RUN pByLocationID.
        WHEN "tag" THEN
            RUN pByTag.
        WHEN "jobID" THEN
            RUN pByJobID.
        WHEN "inventoryStatus" THEN
            RUN pByInventoryStatus.
        OTHERWISE
            {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
    IF AVAILABLE ttBrowseInventory THEN
        APPLY "VALUE-CHANGED":U TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTagScan W-Win 
PROCEDURE pTagScan :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTag           AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lValidInv         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSuccess          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lChoice           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCreated          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cJobNoLocal       AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cRMItemLocal      AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE iJobNo2Local      AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE iFormNoLocal      AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE iBlankNoLocal     AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE lIsReturn         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dReturnQuantity   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dIssuedQty        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dUsedQty          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cIssuedQtyUOM     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValueReturned    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dIssueQuantity    AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-item FOR item.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    cMessage = "".

    RUN pStatusMessage("", 0). 

    RUN pGetInventoryStockDetails IN hdInventoryProcs (
        INPUT  ipcCompany,
        INPUT  ipcTag,
        OUTPUT lValidInv,
        OUTPUT cMessage,
        INPUT-OUTPUT TABLE ttInventoryStockDetails BY-REFERENCE
        ).
  
    IF lValidInv THEN DO:
        FIND FIRST ttInventoryStockDetails
             WHERE ttInventoryStockDetails.tag EQ ipcTag
             NO-ERROR.
        IF AVAILABLE ttInventoryStockDetails THEN DO:
            lIsReturn = ttInventoryStockDetails.isIssued AND ttInventoryStockDetails.quantityOnHand EQ 0.
                
            ASSIGN
                cJobNoLocal   = ttInventoryStockDetails.jobID
                cRMItemLocal  = ttInventoryStockDetails.rmItemID
                iJobNo2Local  = ttInventoryStockDetails.jobID2
                iFormNoLocal  = ttInventoryStockDetails.formNo
                iBlankNoLocal = ttInventoryStockDetails.blankNo
                .
        END.
        
        IF cbRMItem:SCREEN-VALUE NE cRMItemLocal THEN DO:
            system.SharedConfig:Instance:SetValue("Tag", ipcTag).
            system.SharedConfig:Instance:SetValue("RMItemID", cRMItemLocal).
            
            RUN displayMessageQuestionDialog("65",OUTPUT lChoice). 
            
            IF NOT lChoice THEN
                RETURN.
        END.
        
        IF ttInventoryStockDetails.itemCode EQ "E" AND NOT lIsReturn AND
           (cJobno   NE cJobNoLocal  OR
            iJobno2  NE iJobNo2Local OR
            iFormno  NE iFormNoLocal OR
            iBlankno NE iBlankNoLocal) AND
            cJobNoLocal NE "" THEN DO:
            RUN sharpShooter/messageDialog.w (
                INPUT  "Tag belongs to different Job. Do you want to continue?",
                INPUT  TRUE,
                INPUT  TRUE,
                INPUT  FALSE,
                OUTPUT lChoice
                ).
            IF NOT lChoice THEN
                RETURN.
        END.
        
        IF lIsReturn THEN DO:
            RUN Inventory_GetRMIssuedQuantity IN hdInventoryProcs (
                INPUT  ttInventoryStockDetails.company,
                INPUT  ttInventoryStockDetails.primaryID,
                INPUT  ttInventoryStockDetails.tag,
                OUTPUT dIssuedQty,
                OUTPUT cIssuedQtyUOM,
                OUTPUT lError,
                OUTPUT cMessage
                ).
            IF lError THEN DO:
                RUN pStatusMessage(cMessage, 3). 
                
                RETURN.
            END.
            
            RUN inventory/adjustQuantityReturn.w (
                INPUT  dIssuedQty,
                INPUT  cIssuedQtyUOM,
                OUTPUT lValueReturned,
                OUTPUT dReturnQuantity
                ).
                
            IF NOT lValueReturned THEN
                RETURN.

            IF dReturnQuantity NE 0 THEN DO:                
                RUN Inventory_CreateReturnFromTag IN hdInventoryProcs(
                    INPUT  ttInventoryStockDetails.company,
                    INPUT  ttInventoryStockDetails.tag,
                    INPUT  dReturnQuantity,
                    INPUT  TRUE,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).  
            END.
            ELSE
                cMessage = "Return quantity should be greater than 0".
                
            IF NOT lError THEN  
                cMessage = "Tag '" + ttInventoryStockDetails.tag + "' is returned".

            RUN pStatusMessage(INPUT cMessage, INPUT IF lError THEN 3 ELSE 1). 
        END.
        ELSE IF AVAILABLE ttInventoryStockDetails THEN DO:  
            IF glAutoPost THEN DO:
                RUN inventory/adjustQuantityIssue2.w (
                    INPUT  ttInventoryStockDetails.quantity,
                    INPUT  ttInventoryStockDetails.quantityUOM,
                    OUTPUT lValueReturned,
                    OUTPUT dIssueQuantity
                    ).
                IF NOT lValueReturned THEN
                    RETURN.            
            END.
            
            RUN Inventory_CreateRMIssueFromTag in hdInventoryProcs (
                INPUT  ipcCompany,
                INPUT  ttInventoryStockDetails.tag,
                INPUT  cJobNo,
                INPUT  iJobNo2,
                INPUT  iFormNo,
                INPUT  iBlankNo,
                INPUT  dIssueQuantity,
                INPUT  glAutoPost,  /* Override quantity */
                INPUT  glAutoPost,
                OUTPUT lCreated,                    
                OUTPUT cMessage,
                INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
                ).
            IF lCreated THEN
                cMessage = "Tag '" + ttInventoryStockDetails.tag + "' moved to '" + STRING(glAutoPost, "Consumed/Scanned") + "' status.".
            
            RUN pStatusMessage(INPUT cMessage, INPUT IF lCreated THEN 1 ELSE 3). 

            EMPTY TEMP-TABLE ttBrowseInventory.
        END.
    END.
    ELSE DO:
        cMessage = "Invalid Tag".
        RUN pStatusMessage(INPUT cMessage, INPUT 3). 
    END.

    RUN pRebuildBrowse (
        INPUT ipcCompany,
        INPUT cJobNo,
        INPUT iJobNo2,
        INPUT iFormNo,
        INPUT iBlankNo,
        INPUT cbRMItem:SCREEN-VALUE,
        INPUT TRUE
        ).

    FINALLY:
        {methods/run_link.i "TAG-SOURCE" "EmptyTag"}
        
        {methods/run_link.i "TAG-SOURCE" "ScanNextTag"}
    END FINALLY.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRMItemList W-Win 
PROCEDURE pUpdateRMItemList :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormno       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankno      AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE cRMListItems AS CHARACTER NO-UNDO.
        
    RUN GetRMItemsForJob IN hdJobProcs (
        INPUT  ipcCompany,
        INPUT  ipcJobNo,
        INPUT  ipiJobNo2,
        INPUT  ipiFormNo,
        INPUT  ipiBlankNo,
        OUTPUT cRMListItems
        ).
    
    cbRMItem:LIST-ITEMS IN FRAME {&FRAME-NAME} = cRMListitems.

    IF gcSSIssueDefaultRM NE "User Select" THEN DO:
        cbRMItem:SCREEN-VALUE = ENTRY(1, cRMListitems) NO-ERROR.
        
        APPLY "VALUE-CHANGED" TO cbRMItem. 
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize W-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
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
            btnSettingsText:ROW                = {&WINDOW-NAME}:HEIGHT - .86
            btnSettingsText:COL                = dCol - 20
            btnAdjustQtyText:ROW               = {&WINDOW-NAME}:HEIGHT - .86
            btnAdjustQtyText:COL               = dCol - 60            
            btnClearText:COL                   = dCol - 12
            btClear:COL                        = dCol
            BROWSE {&BROWSE-NAME}:HEIGHT       = {&WINDOW-NAME}:HEIGHT - BROWSE {&BROWSE-NAME}:ROW - 1.62
            BROWSE {&BROWSE-NAME}:WIDTH        = dCol - 2
            .
            
        RUN set-position IN h_exit ( 1.00 , dCol ) NO-ERROR.
        RUN set-position IN h_setting ( {&WINDOW-NAME}:HEIGHT - 1.1 , btnSettingsText:COL + 18 ) NO-ERROR.
        RUN set-position IN h_adjustqty ( {&WINDOW-NAME}:HEIGHT - 1.1 , btnAdjustQtyText:COL + 24 ) NO-ERROR.
        RUN get-position IN h_navigatefirst ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
        RUN set-position IN h_navigatefirst ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigateprev ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatenext ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatelast ( dRow , dCol ) NO-ERROR.
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
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY. 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Foucs W-Win 
PROCEDURE Set-Foucs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {methods/run_link.i "JOB-SOURCE" "Set-Focus"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowKeyboard W-Win 
PROCEDURE ShowKeyboard :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplShowKeyboard AS LOGICAL NO-UNDO.

    oplShowKeyboard = glShowVirtualKeyboard.

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

    DEFINE VARIABLE cStatusMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStatusMessageType AS INTEGER   NO-UNDO.
        
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    CASE p-state:
        WHEN "job-invalid" THEN DO:
            
        END.
        WHEN "job-valid" THEN DO:            
            {methods/run_link.i "JOB-SOURCE" "GetJob" "(OUTPUT cJobNo, OUTPUT iJobNo2, OUTPUT iFormNo, OUTPUT iBlankNo)"}
            
            RUN pUpdateRMItemList (cCompany, cJobNo, iJobNo2, iFormNo, iBlankNo).
        END.
        WHEN "job-error" THEN DO:
            {methods/run_link.i "JOB-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
            
            RUN pStatusMessage (cStatusMessage, iStatusMessageType).
        END.   
        WHEN "tag-valid" THEN DO:
            {methods/run_link.i "TAG-SOURCE" "GetTag" "(OUTPUT oLoadtag)"}
            
            IF VALID-OBJECT(oLoadtag) AND LOGICAL(oLoadTag:GetValue("ItemType")) THEN
                RUN pTagScan (cCompany, oLoadtag:GetValue("Tag")).
        END.
        WHEN "tag-invalid" THEN DO:
        END.
        WHEN "tag-error" THEN DO:
            {methods/run_link.i "TAG-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
            
            RUN pStatusMessage (cStatusMessage, iStatusMessageType).
        END.        
    END CASE. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetConcatJobID W-Win 
FUNCTION fGetConcatJobID RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    IF ttBrowseInventory.jobID EQ "" THEN
        RETURN "".
    ELSE
        RETURN STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', ttBrowseInventory.jobID, ttBrowseInventory.jobID2)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetConcatLocationID W-Win 
FUNCTION fGetConcatLocationID RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    RETURN ttBrowseInventory.warehouseID + " " 
           + FILL(" ", iWarehouseLength - LENGTH(ttBrowseInventory.warehouseID)) 
           + ttBrowseInventory.locationID.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

