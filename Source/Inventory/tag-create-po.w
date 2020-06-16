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

  File: 

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

/* Parameters Definitions ---                                           */
DEFINE VARIABLE ipcCompany  AS CHARACTER NO-UNDO INITIAL "001".
DEFINE VARIABLE ipcLocation AS CHARACTER NO-UNDO INITIAL "MAIN".

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hdInventoryProcs        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdJobProcs              AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdOutputProcs           AS HANDLE    NO-UNDO.
DEFINE VARIABLE cMessage                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cListItems              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutputFileName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPathTemplate           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValidatePONo           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValidateTag            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemType               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCreated                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iCount                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotTags                AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotOnHand              AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCopies                 AS INTEGER   NO-UNDO.

DEFINE VARIABLE gcPathDataFileDefault AS CHARACTER INITIAL "C:\BA\LABEL".

{system/sysconst.i}
{Inventory/ttInventory.i "NEW SHARED"}
{methods/defines/sortByDefs.i}
{wip/keyboardDefs.i}

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
&Scoped-define FIELDS-IN-QUERY-br-table ttBrowseInventory.quantity ttBrowseInventory.quantityOriginal ttBrowseInventory.quantityUOM ttBrowseInventory.locationID ttBrowseInventory.tag ttBrowseInventory.quantityPerSubUnit ttBrowseInventory.quantityOfUnits ttBrowseInventory.inventoryStatus   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH ttBrowseInventory     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br-table ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ttBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btNumPad-1 RECT-27 RECT-28 fiTag fiPONo ~
cbItemNo btCreate fiTotRunQty btKeyboard tbCreateMultiple br-table btExit ~
btPrintSelected btPrintAll btFirst btLast btNext btPrevious btnNumPad 
&Scoped-Define DISPLAYED-OBJECTS fiTag fiPO fiLine fiVendor fiPONo ~
fiItemType fiItem fiQtyPurchased fiJobNo cbItemNo fiQtyPerTag ~
fiTotalPOQtyLabel fiTotRunQty fiTotTags fiNumTags fiUOM fiMessage ~
tbCreateMultiple 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAdjQty 
     LABEL "Adjust Quantity" 
     SIZE 49 BY 3
     FONT 37.

DEFINE BUTTON btCreate 
     LABEL "Create" 
     SIZE 27 BY 3
     FONT 37.

DEFINE BUTTON btDelete 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_cross_disabled.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Delete".

DEFINE BUTTON btExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29.

DEFINE BUTTON btFirst 
     IMAGE-UP FILE "Graphics/32x32/navigate_up2.ico":U NO-FOCUS
     LABEL "First" 
     SIZE 9.6 BY 2.29 TOOLTIP "First".

DEFINE BUTTON btKeyboard 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btLast 
     IMAGE-UP FILE "Graphics/32x32/navigate_down2.ico":U NO-FOCUS
     LABEL "Last" 
     SIZE 9.6 BY 2.29 TOOLTIP "Last".

DEFINE BUTTON btNext 
     IMAGE-UP FILE "Graphics/32x32/navigate_down.ico":U NO-FOCUS
     LABEL "Next" 
     SIZE 9.6 BY 2.29 TOOLTIP "Next".

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btNumPad-1 
     IMAGE-UP FILE "Graphics/24x24/numeric_keypad.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad1" 
     SIZE 6.4 BY 1.52 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btNumPad-2 
     IMAGE-UP FILE "Graphics/24x24/numeric_keypad.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad1" 
     SIZE 6.4 BY 1.52 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btNumPad-3 
     IMAGE-UP FILE "Graphics/24x24/numeric_keypad.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad1" 
     SIZE 6.4 BY 1.52 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btPrevious 
     IMAGE-UP FILE "Graphics/32x32/navigate_up.ico":U NO-FOCUS
     LABEL "Previous" 
     SIZE 9.6 BY 2.29 TOOLTIP "Previous".

DEFINE BUTTON btPrintAll 
     LABEL "Print and Receive All" 
     SIZE 50 BY 3
     FONT 37.

DEFINE BUTTON btPrintSelected 
     LABEL "Print and Receive Selected" 
     SIZE 50 BY 3
     FONT 37.

DEFINE VARIABLE cbItemNo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 40 BY 1
     FGCOLOR 9 FONT 37 NO-UNDO.

DEFINE VARIABLE fiItem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 48.8 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiItemType AS CHARACTER FORMAT "X(256)":U INITIAL "FG Item #:" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiJobNo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22.4 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiLine AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 100.2 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiNumTags AS INTEGER FORMAT ">9":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1.38
     FGCOLOR 9 FONT 37 NO-UNDO.

DEFINE VARIABLE fiPO AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiPONo AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.38
     FGCOLOR 9 FONT 37 NO-UNDO.

DEFINE VARIABLE fiQtyPerTag AS DECIMAL FORMAT ">,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 29.6 BY 1.38
     FGCOLOR 9 FONT 37 NO-UNDO.

DEFINE VARIABLE fiQtyPurchased AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiTag AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1.38
     FGCOLOR 9 FONT 37 NO-UNDO.

DEFINE VARIABLE fiTotalPOQtyLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Total PO Qty:" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.38
     FGCOLOR 9 FONT 36 NO-UNDO.

DEFINE VARIABLE fiTotRunQty AS DECIMAL FORMAT ">,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 29.6 BY 1.38
     FGCOLOR 9 FONT 37 NO-UNDO.

DEFINE VARIABLE fiTotTags AS CHARACTER FORMAT "X(256)":U INITIAL "# Tags:" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.38
     FGCOLOR 9 FONT 36 NO-UNDO.

DEFINE VARIABLE fiUOM AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiVendor AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25.2 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 198 BY .05.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 92.4 BY 5.

DEFINE VARIABLE tbCreateMultiple AS LOGICAL INITIAL no 
     LABEL "Create Multiple Tags" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      ttBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table W-Win _FREEFORM
  QUERY br-table DISPLAY
      ttBrowseInventory.quantity WIDTH 20 COLUMN-LABEL "Qty On-Hand"
    ttBrowseInventory.quantityOriginal WIDTH 20 COLUMN-LABEL "Qty Original"
    ttBrowseInventory.quantityUOM WIDTH 8 COLUMN-LABEL "UOM"
    ttBrowseInventory.locationID WIDTH 25 COLUMN-LABEL "Location" FORMAT "X(12)"
    ttBrowseInventory.tag WIDTH 50 COLUMN-LABEL "Tag #" FORMAT "X(30)"
    ttBrowseInventory.quantityPerSubUnit WIDTH 15 COLUMN-LABEL "Sub Unit"
    ttBrowseInventory.quantityOfUnits WIDTH 15 COLUMN-LABEL "Unit"    
    ttBrowseInventory.inventoryStatus COLUMN-LABEL "Status" FORMAT "X(15)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 189 BY 17.38
         FONT 36 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btNumPad-1 AT ROW 9.1 COL 65 WIDGET-ID 124
     fiTag AT ROW 3 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 168
     fiPO AT ROW 3.86 COL 110.2 COLON-ALIGNED NO-LABEL WIDGET-ID 182
     fiLine AT ROW 3.86 COL 138 COLON-ALIGNED NO-LABEL WIDGET-ID 184
     fiVendor AT ROW 3.86 COL 161.8 COLON-ALIGNED NO-LABEL WIDGET-ID 188
     fiPONo AT ROW 4.81 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fiItemType AT ROW 5.29 COL 95.8 COLON-ALIGNED NO-LABEL WIDGET-ID 192
     fiItem AT ROW 5.29 COL 112 COLON-ALIGNED NO-LABEL WIDGET-ID 194
     fiQtyPurchased AT ROW 6.57 COL 124 COLON-ALIGNED NO-LABEL WIDGET-ID 196
     fiJobNo AT ROW 6.57 COL 159.6 COLON-ALIGNED NO-LABEL WIDGET-ID 200
     cbItemNo AT ROW 6.62 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 160
     btCreate AT ROW 9.1 COL 174.2 WIDGET-ID 108
     fiQtyPerTag AT ROW 9.14 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     btNumPad-2 AT ROW 9.1 COL 124.4 WIDGET-ID 126
     fiTotalPOQtyLabel AT ROW 9.14 COL 72.8 NO-LABEL WIDGET-ID 172
     fiTotRunQty AT ROW 9.14 COL 92.4 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     fiTotTags AT ROW 9.14 COL 131.4 COLON-ALIGNED NO-LABEL WIDGET-ID 174
     btNumPad-3 AT ROW 9.05 COL 165.2 WIDGET-ID 128
     fiNumTags AT ROW 9.14 COL 144.8 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     btKeyboard AT ROW 4.76 COL 63 WIDGET-ID 132
     fiUOM AT ROW 9.33 COL 56 COLON-ALIGNED NO-LABEL WIDGET-ID 210
     btDelete AT ROW 19.76 COL 192 WIDGET-ID 116
     fiMessage AT ROW 10.91 COL 69.8 COLON-ALIGNED NO-LABEL WIDGET-ID 142
     tbCreateMultiple AT ROW 11 COL 31 WIDGET-ID 206
     br-table AT ROW 12.43 COL 2 WIDGET-ID 200
     btExit AT ROW 3.24 COL 192 WIDGET-ID 84
     btAdjQty AT ROW 30.52 COL 2 WIDGET-ID 144
     btPrintSelected AT ROW 30.52 COL 71 WIDGET-ID 154
     btPrintAll AT ROW 30.52 COL 141.2 WIDGET-ID 112
     btFirst AT ROW 12.38 COL 191.8 WIDGET-ID 44
     btLast AT ROW 27.52 COL 191.8 WIDGET-ID 46
     btNext AT ROW 23.67 COL 192 WIDGET-ID 42
     btPrevious AT ROW 15.81 COL 192 WIDGET-ID 40
     btnNumPad AT ROW 5.05 COL 73.8 WIDGET-ID 120
     "PO #:" VIEW-AS TEXT
          SIZE 9.6 BY 1.33 AT ROW 4.81 COL 10.4 WIDGET-ID 12
          FGCOLOR 9 FONT 36
     "Quantity Purchased:" VIEW-AS TEXT
          SIZE 27 BY .95 AT ROW 6.57 COL 98 WIDGET-ID 198
          FGCOLOR 9 FONT 35
     "(Optional)" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 3.38 COL 84 WIDGET-ID 204
          FGCOLOR 9 
     "PO #:" VIEW-AS TEXT
          SIZE 8 BY .95 AT ROW 3.86 COL 104.2 WIDGET-ID 180
          FGCOLOR 9 FONT 35
     "Line:" VIEW-AS TEXT
          SIZE 6.6 BY .95 AT ROW 3.86 COL 132.4 WIDGET-ID 186
          FGCOLOR 9 FONT 35
     "Vendor #:" VIEW-AS TEXT
          SIZE 13 BY .95 AT ROW 3.86 COL 150 WIDGET-ID 190
          FGCOLOR 9 FONT 35
     "Quantity Per Tag:" VIEW-AS TEXT
          SIZE 26.4 BY 1.33 AT ROW 9.14 COL 1.6 WIDGET-ID 96
          FGCOLOR 9 FONT 36
     "Item #:" VIEW-AS TEXT
          SIZE 11 BY 1.33 AT ROW 6.62 COL 7.8 WIDGET-ID 166
          FGCOLOR 9 FONT 36
     "PO or Tag:" VIEW-AS TEXT
          SIZE 16.8 BY 1.33 AT ROW 3 COL 3.4 WIDGET-ID 170
          FGCOLOR 9 FONT 36
     "PO Details:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 2.67 COL 104 WIDGET-ID 178
          FGCOLOR 9 FONT 35
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 204.8 BY 32.81
         BGCOLOR 15  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "Job #:" VIEW-AS TEXT
          SIZE 8 BY .95 AT ROW 6.57 COL 153 WIDGET-ID 202
          FGCOLOR 9 FONT 35
     RECT-2 AT ROW 4.81 COL 72.8 WIDGET-ID 130
     RECT-27 AT ROW 8.67 COL 3 WIDGET-ID 138
     RECT-28 AT ROW 2.91 COL 97.2 WIDGET-ID 176
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 204.8 BY 32.81
         BGCOLOR 15  WIDGET-ID 100.


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
         TITLE              = "Finished Goods and Raw Material Tag Creation"
         HEIGHT             = 32.81
         WIDTH              = 204.8
         MAX-HEIGHT         = 36.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 36.57
         VIRTUAL-WIDTH      = 273.2
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
/* BROWSE-TAB br-table tbCreateMultiple F-Main */
ASSIGN 
       br-table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON btAdjQty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btDelete IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btKeyboard:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btNumPad-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btNumPad-3 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       cbItemNo:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiItem IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiItemType IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiJobNo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLine IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiMessage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiNumTags IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPO IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiQtyPerTag IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiQtyPurchased IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotalPOQtyLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiTotTags IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiUOM IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiVendor IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory
    ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Finished Goods and Raw Material Tag Creation */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Finished Goods and Raw Material Tag Creation */
DO:
    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE OBJECT hdInventoryProcs.
        
    IF VALID-HANDLE(hdJobProcs) THEN
        DELETE OBJECT hdJobProcs.
            
    IF VALID-HANDLE(hdOutputProcs) THEN
        DELETE OBJECT hdOutputProcs.

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
ON START-SEARCH OF br-table IN FRAME F-Main
DO:
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table W-Win
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
    ASSIGN
        btAdjQty:SENSITIVE = FALSE
        btPrintSelected:LABEL = "Print and Receive Selected"
        .
                              
    IF AVAILABLE ttBrowseInventory THEN DO:
        ASSIGN
            btAdjQty:SENSITIVE = ttBrowseInventory.inventoryStatus EQ gcStatusStockInitial
            btDelete:SENSITIVE = AVAILABLE ttBrowseInventory AND
                                 DYNAMIC-FUNCTION (
                                     "fCanDeleteInventoryStock" IN hdInventoryProcs, 
                                      ttBrowseInventory.inventoryStockID
                                 )
            .
        
        IF ttBrowseInventory.inventoryStatus NE gcStatusStockInitial THEN
            btPrintSelected:LABEL = "Re-Print Selected".
    END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAdjQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAdjQty W-Win
ON CHOOSE OF btAdjQty IN FRAME F-Main /* Adjust Quantity */
DO:
    DEFINE VARIABLE dTotalQuantity         AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dSubUnitCount          AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dSubUnitsPerUnit       AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE lValueReturned         AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE dValue                 AS DECIMAL    NO-UNDO.

    IF AVAILABLE ttBrowseInventory THEN DO:
        RUN inventory/adjustQuantity.w (
            ttBrowseInventory.quantityOriginal,
            ttBrowseInventory.quantityOfSubUnits,
            1,
            OUTPUT dTotalQuantity,
            OUTPUT dSubUnitCount,
            OUTPUT dSubUnitsPerUnit,
            OUTPUT lValueReturned,
            OUTPUT dValue
            ).

        IF lValueReturned THEN DO:
            IF ttBrowseInventory.quantityOriginal EQ dTotalQuantity THEN DO:
                MESSAGE "Adjusted quantity for tag " + ttBrowseInventory.tag +
                        " is same as existing quantity" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.
            
            MESSAGE "Adjust quantity of tag " + ttBrowseInventory.tag +
                    " to " + STRING(dTotalQuantity) "?" VIEW-AS ALERT-BOX QUESTION
                    BUTTON OK-CANCEL
                    TITLE "Adjust Quantity" UPDATE lContinue AS LOGICAL.
            IF lContinue THEN
                RUN pAdjustQuantity (
                    ipcCompany,
                    ttBrowseInventory.inventoryStockID,
                    dTotalQuantity - ttBrowseInventory.quantityOriginal,
                    ttBrowseInventory.quantityUOM
                    ).                     
        END.        
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCreate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCreate W-Win
ON CHOOSE OF btCreate IN FRAME F-Main /* Create */
DO:
    fiMessage:SCREEN-VALUE = "".
    
    IF DECIMAL(fiQtyPerTag:SCREEN-VALUE) EQ 0 THEN DO:
        cMessage = "Quantity Per Tag cannot be 0.00".
        RUN pUpdateMessageText (
            cMessage,    /* Message Text */
            TRUE,        /* Error */
            FALSE        /* Alert-box*/
            ).
        APPLY "ENTRY" TO fiQtyPerTag.
        RETURN.    
    END.
    
    IF tbCreateMultiple:CHECKED AND 
       DECIMAL(fiTotRunQty:SCREEN-VALUE) LT DECIMAL(fiQtyPerTag:SCREEN-VALUE) THEN DO:
        cMessage = "Total PO Qty " + STRING(fiTotRunQty:SCREEN-VALUE) +
                   " cannot be less than Quantity Per tag " + STRING(fiQtyPerTag:SCREEN-VALUE).
        RUN pUpdateMessageText (
            cMessage,    /* Message Text */
            TRUE,        /* Error */
            FALSE        /* Alert-box*/
            ).
        APPLY "ENTRY" TO fiTotRunQty.
        RETURN.
    END.    

    RUN CreateTransactionInitializedFromPO IN hdInventoryProcs (
        ipcCompany,
        INTEGER(fiPONo:SCREEN-VALUE),
        INTEGER(ENTRY(1,cbItemNo:SCREEN-VALUE,"-")),
        ENTRY(2,cbItemNo:SCREEN-VALUE,"-"),
        IF tbCreateMultiple:CHECKED THEN
            DECIMAL(fiTotRunQty:SCREEN-VALUE)
        ELSE
            DECIMAL(fiQtyPerTag:SCREEN-VALUE),
        DECIMAL(fiQtyPerTag:SCREEN-VALUE), /* Set Quantity Per Tags to Total Quantity*/
        1,      /* Sub Units */
        "EA",   /* UOM */     
        OUTPUT lCreated, 
        OUTPUT cMessage
        ).
    
    IF lCreated THEN DO:
        RUN pUpdateMessageText (
            cMessage,    /* Message Text */
            FALSE,       /* Error */
            FALSE        /* Alert-box*/
            ).    
    END.
    
    RUN pRebuildBrowse (
        ipcCompany,
        INTEGER(fiPONo:SCREEN-VALUE),
        INTEGER(ENTRY(1,cbItemNo:SCREEN-VALUE,"-")),
        ENTRY(2,cbItemNo:SCREEN-VALUE,"-"),
        cItemType
        ).
    
    ASSIGN
        fiQtyPerTag:SCREEN-VALUE = "0"
        fiTotRunQty:SCREEN-VALUE = "0"
        fiNumTags:SCREEN-VALUE = "1"
        .
        
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete W-Win
ON CHOOSE OF btDelete IN FRAME F-Main
DO:
    RUN pDelete.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit W-Win
ON CHOOSE OF btExit IN FRAME F-Main
DO:
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.
    APPLY "CLOSE":U TO THIS-PROCEDURE.    
    RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFirst W-Win
ON CHOOSE OF btFirst IN FRAME F-Main /* First */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btKeyboard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btKeyboard W-Win
ON CHOOSE OF btKeyboard IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiPONo.
    RUN pKeyboard (
        fiPONo:HANDLE, 
        "Qwerty"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLast W-Win
ON CHOOSE OF btLast IN FRAME F-Main /* Last */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNext W-Win
ON CHOOSE OF btNext IN FRAME F-Main /* Next */
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


&Scoped-define SELF-NAME btNumPad-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNumPad-1 W-Win
ON CHOOSE OF btNumPad-1 IN FRAME F-Main /* NumPad1 */
DO:
    APPLY "ENTRY":U TO fiQtyPerTag.
    RUN pKeyboard (
        fiQtyPerTag:HANDLE, 
        "Numeric"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNumPad-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNumPad-2 W-Win
ON CHOOSE OF btNumPad-2 IN FRAME F-Main /* NumPad1 */
DO:
    APPLY "ENTRY":U TO fiTotRunQty.
    RUN pKeyboard (
        fiTotRunQty:HANDLE, 
        "Numeric"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNumPad-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNumPad-3 W-Win
ON CHOOSE OF btNumPad-3 IN FRAME F-Main /* NumPad1 */
DO:
    APPLY "ENTRY":U TO fiNumTags.
    RUN pKeyboard (
        fiNumTags:HANDLE, 
        "Numeric"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrevious W-Win
ON CHOOSE OF btPrevious IN FRAME F-Main /* Previous */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrintAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrintAll W-Win
ON CHOOSE OF btPrintAll IN FRAME F-Main /* Print and Receive All */
DO: 
    EMPTY TEMP-TABLE ttPrintInventoryStockFG.
    EMPTY TEMP-TABLE ttPrintInventoryStockRM.
    
    FOR EACH ttBrowseInventory
       WHERE ttBrowseInventory.inventoryStatus EQ gcStatusStockInitial:

        RUN PostReceivedInventory IN hdInventoryProcs (
            INPUT ipcCompany,
            INPUT ttBrowseInventory.inventoryStockID
            ).
        
        IF ttBrowseInventory.itemType EQ gcItemTypeFG THEN
            RUN CreatePrintInventoryForFG IN hdInventoryProcs (
                INPUT ttBrowseInventory.inventoryStockID
                ).
        ELSE IF ttBrowseInventory.itemType EQ gcItemTypeRM THEN
            RUN CreatePrintInventoryForRM IN hdInventoryProcs (
                INPUT ttBrowseInventory.inventoryStockID
                ).
    END.
    
    RUN pPrintLabels.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrintSelected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrintSelected W-Win
ON CHOOSE OF btPrintSelected IN FRAME F-Main /* Print and Receive Selected */
DO:
    EMPTY TEMP-TABLE ttPrintInventoryStockFG.
    EMPTY TEMP-TABLE ttPrintInventoryStockRM.

    IF AVAILABLE ttBrowseInventory THEN DO:
        IF ttBrowseInventory.inventoryStatus EQ gcStatusStockInitial THEN
            RUN PostReceivedInventory IN hdInventoryProcs (
                INPUT ipcCompany,
                INPUT ttBrowseInventory.inventoryStockID
                ).
        
        IF ttBrowseInventory.itemType EQ gcItemTypeFG THEN
            RUN CreatePrintInventoryForFG in hdInventoryProcs (
                INPUT ttBrowseInventory.inventoryStockID
                ).
        ELSE IF ttBrowseInventory.itemType EQ gcItemTypeRM THEN
            RUN CreatePrintInventoryForRM in hdInventoryProcs (
                INPUT ttBrowseInventory.inventoryStockID
                ).
    
        RUN pPrintLabels.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbItemNo W-Win
ON VALUE-CHANGED OF cbItemNo IN FRAME F-Main
DO:
    IF SELF:SCREEN-VALUE NE "" THEN
        RUN pUpdatePODetails (
            ipcCompany,
            INTEGER(fiPONo:SCREEN-VALUE),
            INTEGER(ENTRY(1,SELF:SCREEN-VALUE,"-")),
            ENTRY(2,SELF:SCREEN-VALUE,"-")
            ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiNumTags
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiNumTags W-Win
ON ENTRY OF fiNumTags IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
        RUN pKeyboard (
            SELF, 
            "Numeric"
            ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiNumTags W-Win
ON LEAVE OF fiNumTags IN FRAME F-Main
DO:
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPONo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPONo W-Win
ON ENTRY OF fiPONo IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
    RUN pKeyboard (SELF, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPONo W-Win
ON LEAVE OF fiPONo IN FRAME F-Main
DO:        
    IF SELF:SCREEN-VALUE EQ cValidatePONo THEN
        RETURN.
        
    RUN pPOScan (
        ipcCompany,
        INTEGER(SELF:SCREEN-VALUE)
        ).
        
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiQtyPerTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiQtyPerTag W-Win
ON ENTRY OF fiQtyPerTag IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
        RUN pKeyboard (
            SELF, 
            "Numeric"
            ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiQtyPerTag W-Win
ON LEAVE OF fiQtyPerTag IN FRAME F-Main
DO: 
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.

    IF DECIMAL(fiTotRunQty:SCREEN-VALUE) EQ 0 THEN
        fiTotRunQty:SCREEN-VALUE = fiQtyPerTag:SCREEN-VALUE.
    
    IF DECIMAL(fiQtyPerTag:SCREEN-VALUE) NE 0 AND
       DECIMAL(fiTotRunQty:SCREEN-VALUE) NE 0 AND
       DECIMAL(fiQtyPerTag:SCREEN-VALUE) LT DECIMAL(fiTotRunQty:SCREEN-VALUE) THEN 
    DO:   
        ASSIGN
            fiNumTags:SCREEN-VALUE = STRING(INTEGER(TRUNC(DECIMAL(fiTotRunQty:SCREEN-VALUE) / DECIMAL(fiQtyPerTag:SCREEN-VALUE),0)))
            btCreate:LABEL        = "Create All".
    END.
    ELSE
        ASSIGN
            fiNumTags:SCREEN-VALUE = "1"
            btCreate:LABEL        = "Create".    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON ENTRY OF fiTag IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
    RUN pKeyboard (SELF, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON LEAVE OF fiTag IN FRAME F-Main
DO: 
    DEFINE VARIABLE iErrorValidation AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndex           AS INTEGER   NO-UNDO.
    
    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    IF SELF:SCREEN-VALUE EQ cValidateTag THEN
        RETURN.
            
    ASSIGN
        cbItemNo:SCREEN-VALUE       = ""
        fiPO:SCREEN-VALUE           = ""
        fiLine:SCREEN-VALUE         = ""
        fiVendor:SCREEN-VALUE       = ""
        fiQtyPurchased:SCREEN-VALUE = ""
        fiItem:SCREEN-VALUE         = ""
        fiJobNo:SCREEN-VALUE        = ""
        cValidateTag                = SELF:SCREEN-VALUE
        .

    IF INDEX(SELF:SCREEN-VALUE, "-") GT 0 THEN DO:

        iErrorValidation = INTEGER(ENTRY(1,SELF:SCREEN-VALUE,"-")) NO-ERROR.
        IF ERROR-STATUS:ERROR OR ENTRY(1,SELF:SCREEN-VALUE,"-") EQ "" THEN DO:
            cMessage = "Invalid PO Number".
            RUN pUpdateMessageText (
                cMessage,    /* Message Text */
                TRUE,        /* Error */
                FALSE        /* Alert-box*/
                ).
        
            RETURN.        
        END.    

        iErrorValidation = INTEGER(ENTRY(2,SELF:SCREEN-VALUE,"-")) NO-ERROR.
        IF ERROR-STATUS:ERROR OR ENTRY(2,SELF:SCREEN-VALUE,"-") EQ "" THEN DO:
            cMessage = "Invalid PO Line Number".
            RUN pUpdateMessageText (
                cMessage,    /* Message Text */
                TRUE,        /* Error */
                FALSE        /* Alert-box*/
                ).
        
            RETURN.        
        END.    
        
        RUN pPOScan (
            ipcCompany,
            INTEGER(ENTRY(1,SELF:SCREEN-VALUE,"-"))
            ).
        
        DO iIndex = 1 TO NUM-ENTRIES(cbItemNo:LIST-ITEMS):
            IF INTEGER(ENTRY(2,SELF:SCREEN-VALUE,"-")) EQ INTEGER(ENTRY(1,ENTRY(iIndex,cbItemNo:LIST-ITEMS),"-")) THEN DO:
                cbItemNo:SCREEN-VALUE = ENTRY(iIndex,cbItemNo:LIST-ITEMS).
                APPLY "VALUE-CHANGED" TO cbItemNo.
                LEAVE.
            END.
        END.
    END.
    ELSE DO:
        RUN pTagScan (
            ipcCompany,
            SELF:SCREEN-VALUE
            ).
    END.
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTotRunQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTotRunQty W-Win
ON ENTRY OF fiTotRunQty IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
    RUN pKeyboard (SELF, "Numeric").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTotRunQty W-Win
ON LEAVE OF fiTotRunQty IN FRAME F-Main
DO:    
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.    

    IF DECIMAL(fiQtyPerTag:SCREEN-VALUE) NE 0 AND
        DECIMAL(fiTotRunQty:SCREEN-VALUE) NE 0 AND
        DECIMAL(fiQtyPerTag:SCREEN-VALUE) LT DECIMAL(fiTotRunQty:SCREEN-VALUE) THEN 
    DO:    
        ASSIGN
            fiNumTags:SCREEN-VALUE = STRING(TRUNC(DECIMAL(fiTotRunQty:SCREEN-VALUE) / DECIMAL(fiQtyPerTag:SCREEN-VALUE),0))
            btCreate:LABEL         = "Create All".
    END.
    ELSE
        ASSIGN
            fiNumTags:SCREEN-VALUE = "1"
            btCreate:LABEL         = "Create".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbCreateMultiple
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbCreateMultiple W-Win
ON VALUE-CHANGED OF tbCreateMultiple IN FRAME F-Main /* Create Multiple Tags */
DO:
    RUN pToggleMultipleTags (
        NOT SELF:CHECKED
        ).
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
{methods/sortByProc.i "pByQuantity" "ttBrowseInventory.quantity"}
{methods/sortByProc.i "pByQuantityOriginal" "ttBrowseInventory.quantityOriginal"}
{methods/sortByProc.i "pByLocationID" "ttBrowseInventory.locationID"}
{methods/sortByProc.i "pByTag" "ttBrowseInventory.tag"}
{methods/sortByProc.i "pByJobID" "ttBrowseInventory.jobID"}
{methods/sortByProc.i "pByInventoryStatus" "ttBrowseInventory.inventoryStatus"}

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
  DISPLAY fiTag fiPO fiLine fiVendor fiPONo fiItemType fiItem fiQtyPurchased 
          fiJobNo cbItemNo fiQtyPerTag fiTotalPOQtyLabel fiTotRunQty fiTotTags 
          fiNumTags fiUOM fiMessage tbCreateMultiple 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btNumPad-1 RECT-27 RECT-28 fiTag fiPONo cbItemNo btCreate fiTotRunQty 
         btKeyboard tbCreateMultiple br-table btExit btPrintSelected btPrintAll 
         btFirst btLast btNext btPrevious btnNumPad 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAdjustQuantity W-Win 
PROCEDURE pAdjustQuantity :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantity         AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM      AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lCreated AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
                
    RUN CreateTransactionAdjustQuantity IN hdInventoryProcs (
        ipcCompany,
        ipcInventoryStockID,
        ipdQuantity,
        ipcQuantityUOM,
        TRUE, /* Post Transaction */
        OUTPUT lCreated,
        OUTPUT cMessage
        ).

    IF lCreated THEN DO:
        RUN pUpdateMessageText (
            cMessage,    /* Message Text */
            FALSE,       /* Error */
            FALSE        /* Alert-box*/
            ).        
    END.
        
    RUN pRebuildBrowse (
        ipcCompany,
        INTEGER(fiPONo:SCREEN-VALUE),
        INTEGER(ENTRY(1,cbItemNo:SCREEN-VALUE,"-")),
        ENTRY(2,cbItemNo:SCREEN-VALUE,"-"),
        cItemType
        ).     
        
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDelete W-Win 
PROCEDURE pDelete PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF AVAILABLE ttBrowseInventory THEN DO:
        MESSAGE
            "Delete Selection?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE lDelete AS LOGICAL.
        IF lDelete THEN DO:
            RUN DeleteInventoryStock IN hdInventoryProcs (
                ttBrowseInventory.inventoryStockID
                ).
            BROWSE {&BROWSE-NAME}:DELETE-CURRENT-ROW().
        END.        
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisableCreate W-Win 
PROCEDURE pDisableCreate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiQtyPerTag:SENSITIVE      = FALSE
        fiTotRunQty:SENSITIVE      = FALSE
        fiNumTags:SENSITIVE        = FALSE
        btCreate:SENSITIVE         = FALSE
        btNumpad-1:SENSITIVE       = FALSE
        btNumpad-2:SENSITIVE       = FALSE
        btNumpad-3:SENSITIVE       = FALSE
        tbCreateMultiple:SENSITIVE = FALSE
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEnableCreate W-Win 
PROCEDURE pEnableCreate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiQtyPerTag:SENSITIVE      = TRUE
        fiTotRunQty:SENSITIVE      = TRUE
        fiNumTags:SENSITIVE        = TRUE
        btCreate:SENSITIVE         = TRUE
        btNumpad-1:SENSITIVE       = TRUE
        btNumpad-2:SENSITIVE       = TRUE
        btNumpad-3:SENSITIVE       = TRUE
        tbCreateMultiple:SENSITIVE = TRUE
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings W-Win 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
     Purpose: Returns the key NK1 settings for printing FG Labels
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPathDataFile AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPathTemplate AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCopies       AS INTEGER   NO-UNDO.

    DEFINE VARIABLE cReturn AS CHARACTER.
    DEFINE VARIABLE lFound  AS LOGICAL.

    RUN sys/ref/nk1look.p (
        ipcCompany, 
        "WIPTAG", 
        "C", 
        NO, 
        NO, 
        "", 
        "", 
        OUTPUT cReturn, 
        OUTPUT lFound
        ).
         
    IF lFound THEN 
        opcPathTemplate = cReturn.

    RUN sys/ref/nk1look.p (
        ipcCompany, 
        "WIPTAG", 
        "I", 
        NO, 
        NO, 
        "", 
        "", 
        OUTPUT cReturn, 
        OUTPUT lFound
        ).
         
    IF lFound THEN 
        opiCopies = INTEGER(cReturn).
    IF opiCopies EQ 0 THEN 
        opiCopies = 1.
    
    RUN GetBarDirFilePath IN hdOutputProcs (
        ipcCompany, 
        "loadtag", 
        OUTPUT opcPathDataFile
        ).
    IF opcPathDataFile EQ "" THEN 
        opcPathDataFile = gcPathDataFileDefault.
    
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
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.
    RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
    
    FIND FIRST company NO-LOCK 
         WHERE company.company EQ ipcCompany NO-ERROR .
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - {&awversion}" + " - " 
                         + STRING(company.name) + " - " + ipcLocation.          
            
    RUN pGetSettings (
        ipcCompany, 
        OUTPUT cOutputFileName, 
        OUTPUT cPathTemplate, 
        OUTPUT iCopies
        ).
                         
    RUN pDisableCreate.  
    
    APPLY "VALUE-CHANGED" TO tbCreateMultiple.                       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPOScan W-Win 
PROCEDURE pPOScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPONo    AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE lValidPO AS LOGICAL  NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN pDisableCreate.
    
    ASSIGN
        fiPONo:SCREEN-VALUE         = STRING(ipiPONo)
        cListItems                  = ""
        cbItemNo:LIST-ITEMS         = cListItems
        cbItemNo:SCREEN-VALUE       = ""
        fiPO:SCREEN-VALUE           = ""
        fiLine:SCREEN-VALUE         = ""
        fiVendor:SCREEN-VALUE       = ""
        fiQtyPurchased:SCREEN-VALUE = ""
        fiItem:SCREEN-VALUE         = ""
        fiJobNo:SCREEN-VALUE        = ""
        cValidatePONo               = STRING(ipiPONo)
        fiMessage:SCREEN-VALUE      = ""
        .
        
    RUN ValidatePO IN hdInventoryProcs (
        ipcCompany,
        ipiPONo,
        OUTPUT lValidPO
        ).
    
    IF NOT lValidPO THEN DO:
        cMessage = "Invalid PO Number".
        RUN pUpdateMessageText (
            cMessage,    /* Message Text */
            TRUE,        /* Error */
            FALSE        /* Alert-box*/
            ).
        
        RETURN.
    END.
    
    RUN pUpdateItemList (
        ipcCompany,
        ipiPONo
        ).
                          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintLabels W-Win 
PROCEDURE pPrintLabels PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF SEARCH(cPathTemplate) NE ? THEN 
    DO:
        RUN PrintLabelMatrixFile IN hdOutputProcs(ipcCompany, cPathTemplate, "WIPTAG").
        MESSAGE "Label Matrix print process has been started." SKIP
            "This process will generate printed labels for your selected WIP Tags" SKIP(2)
            "     Data file: " cOutputFileName SKIP 
            "     Template file: " cPathTemplate
            VIEW-AS ALERT-BOX TITLE "Print Confirmation".
    END.
    ELSE  
        MESSAGE "Data file has been generated but system is unable to locate template file to print." SKIP (2)
            "     Data file: " cOutputFileName SKIP
            "     Template file: " cPathTemplate
            VIEW-AS ALERT-BOX TITLE "Invalid Template".
    
    IF cItemType EQ gcItemTypeFG THEN
        RUN TempTableToCSV IN hdOutputProcs (
            INPUT TEMP-TABLE ttPrintInventoryStockFG:HANDLE,
            INPUT cOutputFileName,
            INPUT TRUE,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).
    ELSE IF cItemType EQ gcItemTypeRM THEN
        RUN TempTableToCSV IN hdOutputProcs (
            INPUT TEMP-TABLE ttPrintInventoryStockRM:HANDLE,
            INPUT cOutputFileName,
            INPUT TRUE,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).

    RUN pRebuildBrowse (
        ipcCompany,
        INTEGER(fiPONo:SCREEN-VALUE),
        INTEGER(ENTRY(1,cbItemNo:SCREEN-VALUE,"-")),
        ENTRY(2,cbItemNo:SCREEN-VALUE,"-"),
        cItemType
        ).
                    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRebuildBrowse W-Win 
PROCEDURE pRebuildBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPONo       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiLine       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItem       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemType   AS CHARACTER NO-UNDO.

    RUN RebuildBrowseTTFromPO IN hdInventoryProcs (
        ipcCompany,
        ipiPONo,
        ipiLine,
        ipcItem,
        ipcItemType
        ).    
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTagScan W-Win 
PROCEDURE pTagScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTag     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lValidInvStock   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPrintAndReceive AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPrint           AS LOGICAL   NO-UNDO.    
    DEFINE VARIABLE cReturnMessage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex           AS INTEGER   NO-UNDO.
   
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN pGetInventoryStockDetails IN hdInventoryProcs (
        ipcCompany,
        ipcTag,
        OUTPUT lValidInvStock,
        OUTPUT cReturnMessage,
        INPUT-OUTPUT TABLE ttInventoryStockDetails
        ).        

    IF NOT lValidInvStock THEN DO:
        RUN pUpdateMessageText (
            cReturnMessage, /* Message Text */
            TRUE,           /* Error */
            FALSE           /* Alert-box*/
            ).
        RETURN.
    END.

    FIND FIRST ttInventoryStockDetails
         WHERE ttInventoryStockDetails.company EQ ipcCompany
           AND ttInventoryStockDetails.tag     EQ ipcTag
         NO-ERROR.
    IF AVAILABLE ttInventoryStockDetails THEN DO:
        IF ttInventoryStockDetails.inventoryStatus EQ gcStatusStockInitial THEN DO:
            MESSAGE "Print and Receive Tag: " ttInventoryStockDetails.tag "?"
                VIEW-AS ALERT-BOX QUESTION
                BUTTONS OK-CANCEL
                UPDATE lPrintAndReceive.
            
            IF lPrintAndReceive THEN    
                RUN PostReceivedInventory IN hdInventoryProcs (
                    INPUT ipcCompany,
                    INPUT ttInventoryStockDetails.inventoryStockID
                    ).
        END.
        ELSE
            MESSAGE "Tag: " ttInventoryStockDetails.tag " is already received."
                    "Re-print tag?"
                VIEW-AS ALERT-BOX QUESTION
                BUTTONS OK-CANCEL
                UPDATE lPrint.

        IF lPrintAndReceive OR lPrint THEN DO:
            EMPTY TEMP-TABLE ttPrintInventoryStockFG.
            EMPTY TEMP-TABLE ttPrintInventoryStockRM.
            
            IF ttInventoryStockDetails.itemType EQ gcItemTypeFG THEN DO:
                cItemType = gcItemTypeFG.
                RUN CreatePrintInventoryForFG in hdInventoryProcs (
                    INPUT ttInventoryStockDetails.inventoryStockID
                    ).
            END.
            ELSE IF ttInventoryStockDetails.itemType EQ gcItemTypeRM THEN DO:
                cItemType = gcItemTypeRM.            
                RUN CreatePrintInventoryForRM in hdInventoryProcs (
                    INPUT ttInventoryStockDetails.inventoryStockID
                    ).
            END.
                                
            RUN pPrintLabels.        
        END.
        
        IF ttInventoryStockDetails.poID NE 0 THEN DO:
            RUN pPOScan (
                ipcCompany,
                ttInventoryStockDetails.poID
                ).
            IF ttInventoryStockDetails.poLine NE 0 THEN DO:               
                DO iIndex = 1 TO NUM-ENTRIES(cbItemNo:LIST-ITEMS):
                    IF ttInventoryStockDetails.poLine EQ INTEGER(ENTRY(1,ENTRY(iIndex,cbItemNo:LIST-ITEMS),"-")) THEN DO:
                        cbItemNo:SCREEN-VALUE = ENTRY(iIndex,cbItemNo:LIST-ITEMS).
                        APPLY "VALUE-CHANGED" TO cbItemNo.
                        LEAVE.
                    END.
                END.
            END.
        END.        
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pToggleMultipleTags W-Win 
PROCEDURE pToggleMultipleTags :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplSwitch AS LOGICAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiTotalPOQtyLabel:HIDDEN  = iplSwitch
        fiTotRunQty:HIDDEN        = iplSwitch
        btNumPad-2:HIDDEN         = iplSwitch
        fiTotTags:HIDDEN          = iplSwitch
        fiNumTags:HIDDEN          = iplSwitch
        btNumPad-3:HIDDEN         = iplSwitch
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateItemList W-Win 
PROCEDURE pUpdateItemList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPONo    AS INTEGER   NO-UNDO.
    
    RUN GetItemListForPO IN hdInventoryProcs (
        ipcCompany,
        ipiPONo,
        "A", /* "A" - All, "O" - Opened Only, "C" - Closed Only */
        INPUT-OUTPUT cListItems
        ).
    
    cbItemNo:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListItems.
    
    IF cListItems NE "" THEN DO:
        cbItemNo:SCREEN-VALUE = ENTRY(1,cListItems).
        
        APPLY "VALUE-CHANGED" TO cbItemNo IN FRAME {&FRAME-NAME}.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateMessageText W-Win 
PROCEDURE pUpdateMessageText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMessage  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplError    AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplAlertBox AS LOGICAL   NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.

    fiMessage:SCREEN-VALUE = "".

    IF iplAlertBox THEN DO:
        MESSAGE ipcMessage
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    ASSIGN
        fiMessage:SCREEN-VALUE = ipcMessage
        fiMessage:FGCOLOR      = 2   /* Green */
        .

    IF iplError THEN
        ASSIGN
            fiMessage:SCREEN-VALUE = "**" + ipcMessage
            fiMessage:FGCOLOR      = 12  /* Red */
            .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdatePODetails W-Win 
PROCEDURE pUpdatePODetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPONo     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiLine     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemNo   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lValidOrderLine AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturnMessage  AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiPO:SCREEN-VALUE           = ""
        fiLine:SCREEN-VALUE         = ""
        fiVendor:SCREEN-VALUE       = ""
        fiQtyPurchased:SCREEN-VALUE = ""
        fiJobNo:SCREEN-VALUE        = ""
        cItemType                   = ""
        .
    
    RUN GetPOOrderLineDetails IN hdInventoryProcs (
        ipcCompany,
        ipiPONo,
        ipiLine,
        ipcItemNo,
        OUTPUT lValidOrderLine,
        OUTPUT cReturnMessage,
        INPUT-OUTPUT TABLE ttPOOrderLineDetails    
        ).
        
    IF NOT lValidOrderLine THEN DO:
        cMessage = "Invalid PO Order".
        RUN pUpdateMessageText (
            cMessage,    /* Message Text */
            TRUE,        /* Error */
            FALSE        /* Alert-box*/
            ).
        
        RETURN.    
    END.
    
    FIND FIRST ttPOOrderLineDetails 
         WHERE ttPOOrderLineDetails.company EQ ipcCompany
           AND ttPOOrderLineDetails.po-no   EQ ipiPONo
           AND ttPOOrderLineDetails.line    EQ ipiLine
           AND ttPOOrderLineDetails.i-no    EQ ipcItemNo NO-ERROR.
    IF AVAILABLE ttPOOrderLineDetails THEN DO:
        ASSIGN
            fiPO:SCREEN-VALUE           = STRING(ttPOOrderLineDetails.po-no)
            fiLine:SCREEN-VALUE         = STRING(ttPOOrderLineDetails.line,"99")
            fiVendor:SCREEN-VALUE       = STRING(ttPOOrderLineDetails.vend-no)
            fiQtyPurchased:SCREEN-VALUE = STRING(ttPOOrderLineDetails.ord-qty) + " " + STRING(ttPOOrderLineDetails.pr-uom)
            fiJobNo:SCREEN-VALUE        = IF ttPOOrderLineDetails.job-no NE "" THEN
                                              STRING(ttPOOrderLineDetails.job-no) + "-" +
                                              STRING(ttPOOrderLineDetails.job-no2,"99") + "." +
                                              STRING(ttPOOrderLineDetails.s-num,"99") + "." +
                                              STRING(ttPOOrderLineDetails.b-num,"99")
                                          ELSE
                                              ""
            fiUOM:SCREEN-VALUE          = IF ttPOOrderLineDetails.item-type THEN
                                              ttPOOrderLineDetails.cons-uom
                                          ELSE
                                              gcFGUOM
            fiItem:SCREEN-VALUE         = STRING(ttPOOrderLineDetails.i-no)
            fiItemType:SCREEN-VALUE     = IF ttPOOrderLineDetails.item-type THEN
                                              "RM Item #:"
                                          ELSE
                                              "FG Item #:"
            cItemType                   = IF ttPOOrderLineDetails.item-type THEN
                                              gcItemTypeRM
                                          ELSE
                                              gcItemTypeFG

            .
         
        RUN pEnableCreate.    

        RUN pRebuildBrowse (
                ipcCompany,
                ttPOOrderLineDetails.po-no,
                ttPOOrderLineDetails.line,
                ttPOOrderLineDetails.i-no,
                cItemType
            ).
     END.
     
     APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
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

