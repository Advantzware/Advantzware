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

  File: inventory/tag-create.w

  Description: Creates Tag for FG and WIP from a Job

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
DEFINE VARIABLE hdOutputProcs            AS HANDLE    NO-UNDO.
DEFINE VARIABLE cFormattedJobno         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessage                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobno2ListItems        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormnoListItems        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBlanknoListItems       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMachineListItems       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRMListItems            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValidateJobno          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutputFileName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPathTemplate           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCreated                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iCount                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotTags                AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotOnHand              AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCopies                 AS INTEGER   NO-UNDO.

DEFINE VARIABLE rsItemTypeFG            AS CHARACTER NO-UNDO INITIAL "1".
DEFINE VARIABLE rsItemTypeRM            AS CHARACTER NO-UNDO INITIAL "2".
DEFINE VARIABLE rsItemTypeWP            AS CHARACTER NO-UNDO INITIAL "3".

DEFINE VARIABLE rsValue                 AS CHARACTER NO-UNDO INITIAL "FG".

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
&Scoped-define FIELDS-IN-QUERY-br-table ttBrowseInventory.quantity ttBrowseInventory.quantityOriginal ttBrowseInventory.locationID ttBrowseInventory.tag ttBrowseInventory.jobID ttBrowseInventory.inventoryStatus   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH ttBrowseInventory     WHERE ttBrowseInventory.itemType EQ rsValue     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory     WHERE ttBrowseInventory.itemType EQ rsValue     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br-table ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ttBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-27 rsItemType fiJobno cbJobno2 cbFormno ~
cbBlankno cbMachine cbRMItem btKeyboard btCreate fiQtyPerTag fiTotRunQty ~
fiNumTags br-table btAdjQty btPrintSelected btPrintAll btNumPad-1 ~
btNumPad-2 btNumPad-3 btExit btFirst btLast btNext btPrevious btnNumPad 
&Scoped-Define DISPLAYED-OBJECTS rsItemType fiJobno cbJobno2 cbFormno ~
cbBlankno cbMachine cbRMItem fiRMItem fiQtyPerTag fiTotRunQty fiNumTags ~
fiMessage 

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

DEFINE VARIABLE cbBlankno AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cbFormno AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cbJobno2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cbMachine AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 38 BY 1.29
     FONT 37 NO-UNDO.

DEFINE VARIABLE cbRMItem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 38 BY 1.29
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiJobno AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 100.2 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiNumTags AS INTEGER FORMAT ">9":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiQtyPerTag AS DECIMAL FORMAT ">,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 29.6 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiRMItem AS CHARACTER FORMAT "X(256)":U INITIAL "RM Item #:" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.24
     FONT 36 NO-UNDO.

DEFINE VARIABLE fiTotRunQty AS DECIMAL FORMAT ">,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 29.6 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE rsItemType AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Finished Goods", 1,
"Raw Materials", 2,
"Work In Process", 3
     SIZE 102 BY 1.1
     FONT 37 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 198 BY .05.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      ttBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table W-Win _FREEFORM
  QUERY br-table DISPLAY
      ttBrowseInventory.quantity WIDTH 25 COLUMN-LABEL "Qty On-Hand"
    ttBrowseInventory.quantityOriginal WIDTH 25 COLUMN-LABEL "Qty Original"
    ttBrowseInventory.locationID WIDTH 25 COLUMN-LABEL "Location" FORMAT "X(12)"
    ttBrowseInventory.tag WIDTH 50 COLUMN-LABEL "Tag #" FORMAT "X(30)"
    ttBrowseInventory.jobID WIDTH 20 COLUMN-LABEL "Job #" FORMAT "X(20)"
    ttBrowseInventory.inventoryStatus COLUMN-LABEL "Status" FORMAT "X(15)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 189 BY 19.29
         FONT 36 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rsItemType AT ROW 1.95 COL 21 NO-LABEL WIDGET-ID 146
     fiJobno AT ROW 3.71 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cbJobno2 AT ROW 3.71 COL 71.2 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     cbFormno AT ROW 3.76 COL 97.8 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     cbBlankno AT ROW 3.81 COL 125 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     cbMachine AT ROW 5.43 COL 19.2 COLON-ALIGNED NO-LABEL WIDGET-ID 152
     cbRMItem AT ROW 5.43 COL 89.8 COLON-ALIGNED NO-LABEL WIDGET-ID 160
     fiRMItem AT ROW 5.48 COL 70.4 COLON-ALIGNED NO-LABEL WIDGET-ID 164
     btKeyboard AT ROW 3.67 COL 63 WIDGET-ID 132
     btCreate AT ROW 7.29 COL 174.2 WIDGET-ID 108
     fiQtyPerTag AT ROW 7.43 COL 29.2 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     fiTotRunQty AT ROW 7.43 COL 92 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     fiNumTags AT ROW 7.43 COL 144.8 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     fiMessage AT ROW 9.1 COL 69.8 COLON-ALIGNED NO-LABEL WIDGET-ID 142
     br-table AT ROW 10.52 COL 2 WIDGET-ID 200
     btAdjQty AT ROW 30.52 COL 2 WIDGET-ID 144
     btPrintSelected AT ROW 30.52 COL 71 WIDGET-ID 154
     btPrintAll AT ROW 30.52 COL 141.2 WIDGET-ID 112
     btNumPad-1 AT ROW 7.38 COL 61.8 WIDGET-ID 124
     btNumPad-2 AT ROW 7.38 COL 124.4 WIDGET-ID 126
     btNumPad-3 AT ROW 7.33 COL 165.2 WIDGET-ID 128
     btDelete AT ROW 18.86 COL 192 WIDGET-ID 116
     btExit AT ROW 2.29 COL 192 WIDGET-ID 84
     btFirst AT ROW 10.52 COL 191.8 WIDGET-ID 44
     btLast AT ROW 27.52 COL 191.8 WIDGET-ID 46
     btNext AT ROW 23.14 COL 192 WIDGET-ID 42
     btPrevious AT ROW 14.57 COL 192 WIDGET-ID 40
     btnNumPad AT ROW 2.43 COL 163 WIDGET-ID 120
     "Machine #:" VIEW-AS TEXT
          SIZE 16.6 BY 1.33 AT ROW 5.38 COL 3 WIDGET-ID 158
          FONT 36
     "Blank #:" VIEW-AS TEXT
          SIZE 14 BY 1.33 AT ROW 3.76 COL 111.8 WIDGET-ID 58
          FONT 36
     "Job #:" VIEW-AS TEXT
          SIZE 11 BY 1.33 AT ROW 3.62 COL 9.6 WIDGET-ID 12
          FONT 36
     "Form #:" VIEW-AS TEXT
          SIZE 13.8 BY 1.33 AT ROW 3.76 COL 85.2 WIDGET-ID 48
          FONT 36
     "Total Run Qty:" VIEW-AS TEXT
          SIZE 22 BY 1.33 AT ROW 7.48 COL 71.6 WIDGET-ID 100
          FONT 36
     "# Tags:" VIEW-AS TEXT
          SIZE 12.4 BY 1.33 AT ROW 7.43 COL 133.6 WIDGET-ID 104
          FONT 36
     "Quantity Per Tag:" VIEW-AS TEXT
          SIZE 26 BY 1.33 AT ROW 7.52 COL 4 WIDGET-ID 96
          FONT 36
     RECT-2 AT ROW 2.19 COL 162 WIDGET-ID 130
     RECT-27 AT ROW 7.1 COL 3 WIDGET-ID 138
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
/* BROWSE-TAB br-table fiMessage F-Main */
ASSIGN 
       br-table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON btDelete IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btKeyboard:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btNumPad-1:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btNumPad-2:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btNumPad-3:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       cbRMItem:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiMessage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiRMItem IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiRMItem:HIDDEN IN FRAME F-Main           = TRUE.

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
    WHERE ttBrowseInventory.itemType EQ rsValue
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
    
    RUN inventory/adjustQuantity.w (
        1020,
        10,
        5,
        OUTPUT dTotalQuantity,
        OUTPUT dSubUnitCount,
        OUTPUT dSubUnitsPerUnit,
        OUTPUT lValueReturned,
        OUTPUT dValue
        ).
        
    MESSAGE "Total Quantity: " dTotalQuantity SKIP
            "Sub Unit Count: " dSubUnitCount SKIP
            "Sub Units Per Unit: " dSubUnitsPerUnit 
        VIEW-AS ALERT-BOX.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCreate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCreate W-Win
ON CHOOSE OF btCreate IN FRAME F-Main /* Create */
DO:
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
    
    IF DECIMAL(fiTotRunQty:SCREEN-VALUE) LT DECIMAL(fiQtyPerTag:SCREEN-VALUE) THEN DO:
        cMessage = "Total Run Qty " + STRING(fiTotRunQty:SCREEN-VALUE) +
                   " cannot be less than Quantity Per tag " + STRING(fiQtyPerTag:SCREEN-VALUE).
        RUN pUpdateMessageText (
            cMessage,    /* Message Text */
            TRUE,        /* Error */
            FALSE        /* Alert-box*/
            ).
        APPLY "ENTRY" TO fiTotRunQty.
        RETURN.
    END.
    
    IF rsItemType:SCREEN-VALUE EQ rsItemTypeWP THEN
        RUN CreateTransactionInitializedFromJob IN hdInventoryProcs (
            ipcCompany,
            cFormattedJobno,
            cbMachine:SCREEN-VALUE,
            cbJobno2:SCREEN-VALUE,
            cbFormno:SCREEN-VALUE,
            cbBlankno:SCREEN-VALUE,
            "",  /* Blank RM Item */            
            DECIMAL(fiTotRunQty:SCREEN-VALUE),
            DECIMAL(fiQtyPerTag:SCREEN-VALUE),
            1,
            "EA",
            gcItemTypeWIP,
            OUTPUT lCreated, 
            OUTPUT cMessage
            ).    
    ELSE IF rsItemType:SCREEN-VALUE EQ rsItemTypeFG THEN
        RUN CreateTransactionInitializedFromJob IN hdInventoryProcs (
            ipcCompany,
            cFormattedJobno,
            cbMachine:SCREEN-VALUE,
            cbJobno2:SCREEN-VALUE,
            cbFormno:SCREEN-VALUE,
            cbBlankno:SCREEN-VALUE,
            "",  /* Blank RM Item */
            DECIMAL(fiTotRunQty:SCREEN-VALUE),
            DECIMAL(fiQtyPerTag:SCREEN-VALUE),
            1,
            "EA",
            gcItemTypeFG,
            OUTPUT lCreated, 
            OUTPUT cMessage
            ).    
    ELSE IF rsItemType:SCREEN-VALUE EQ rsItemTypeRM THEN
        RUN CreateTransactionInitializedFromJob IN hdInventoryProcs (
            ipcCompany,
            cFormattedJobno,
            cbMachine:SCREEN-VALUE,
            cbJobno2:SCREEN-VALUE,
            cbFormno:SCREEN-VALUE,
            cbBlankno:SCREEN-VALUE,
            DECIMAL(fiTotRunQty:SCREEN-VALUE),
            DECIMAL(fiQtyPerTag:SCREEN-VALUE),
            1,
            "EA",
            gcItemTypeRM,
            OUTPUT lCreated, 
            OUTPUT cMessage
            ).    


    RUN pRebuildBrowse (
        INPUT ipcCompany,
        INPUT cFormattedJobno,
        INPUT cbMachine:SCREEN-VALUE,
        INPUT cbJobno2:SCREEN-VALUE,
        INPUT cbFormno:SCREEN-VALUE,
        INPUT cbBlankno:SCREEN-VALUE
        ).          
        
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
    APPLY "ENTRY":U TO fiJobno.
    RUN pKeyboard (
        fiJobno:HANDLE, 
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
    EMPTY TEMP-TABLE ttPrintInventoryStock.
    
    FOR EACH ttBrowseInventory
       WHERE ttBrowseInventory.inventoryStatus EQ gcStatusStockInitial
         AND ttBrowseInventory.itemType        EQ rsValue:

        RUN PostReceivedInventory IN hdInventoryProcs (
            INPUT ipcCompany,
            INPUT ttBrowseInventory.inventoryStockID
            ).

        RUN CreatePrintInventoryForFG IN hdInventoryProcs (
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
    EMPTY TEMP-TABLE ttPrintInventoryStock.

    IF AVAILABLE ttBrowseInventory THEN DO:
        IF ttBrowseInventory.inventoryStatus EQ gcStatusStockInitial THEN
            RUN PostReceivedInventory IN hdInventoryProcs (
                INPUT ipcCompany,
                INPUT ttBrowseInventory.inventoryStockID
                ).
    
        RUN CreatePrintInventoryForFG in hdInventoryProcs (
            INPUT ttBrowseInventory.inventoryStockID
            ).
    
        RUN pPrintLabels.
        
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbBlankno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbBlankno W-Win
ON VALUE-CHANGED OF cbBlankno IN FRAME F-Main
DO:
    RUN pOnValueChangedOfJobDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbFormno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFormno W-Win
ON VALUE-CHANGED OF cbFormno IN FRAME F-Main
DO:
    RUN pOnValueChangedOfJobDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbJobno2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbJobno2 W-Win
ON VALUE-CHANGED OF cbJobno2 IN FRAME F-Main
DO:
    RUN pOnValueChangedOfJobDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbMachine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbMachine W-Win
ON VALUE-CHANGED OF cbMachine IN FRAME F-Main
DO:
    RUN pOnValueChangedOfJobDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbRMItem
&Scoped-define SELF-NAME fiJobno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiJobno W-Win
ON ENTRY OF fiJobno IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
    RUN pKeyboard (SELF, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiJobno W-Win
ON HELP OF fiJobno IN FRAME F-Main
DO:
    DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

    RUN system/openlookup.p (ipcCompany, "job-no", 0, "", 0, OUTPUT cFieldsValue, OUTPUT cFoundValue, OUTPUT recFoundRecID).
    SELF:SCREEN-VALUE = cFoundValue.
    APPLY "LEAVE":U TO SELF.
    ASSIGN
        cbJobno2:SCREEN-VALUE  = ENTRY(3,cFieldsValue,"|")
        cbFormno:SCREEN-VALUE  = ENTRY(4,cFieldsValue,"|")
        cbBlankno:SCREEN-VALUE = ENTRY(5,cFieldsValue,"|")
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiJobno W-Win
ON LEAVE OF fiJobno IN FRAME F-Main
DO:
    DEFINE VARIABLE cJobNo     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo2    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormNo    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBlankNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lParse     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidJob  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iJobFormat AS INTEGER   NO-UNDO INITIAL 6.
        
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.

    IF cValidateJobno EQ fiJobno:SCREEN-VALUE THEN
        RETURN.
    
    RUN pDisableCreate.
    
    ASSIGN 
        cJobno2ListItems  = ""
        cFormnoListItems  = ""
        cBlanknoListitems = ""
        cMessage          = ""
        .            
    
    RUN JobParser IN hdJobProcs (
        SELF:SCREEN-VALUE,
        OUTPUT cJobNo,
        OUTPUT cJobNo2,
        OUTPUT cFormNo,
        OUTPUT cBlankNo,
        OUTPUT lParse,
        OUTPUT cMessage
        ).

    IF cMessage NE "" THEN DO:
        RUN pUpdateMessageText (
            cMessage,    /* Message Text */
            TRUE,        /* Error */
            FALSE        /* Alert-box*/
            ).
        RETURN.
    END.
    
    cFormattedJobno = DYNAMIC-FUNCTION (
                      "fAddSpacesToString" IN hdJobProcs, SELF:SCREEN-VALUE, 6, TRUE
                      ).                                  

    IF lParse THEN
        cFormattedJobno = DYNAMIC-FUNCTION (
                          "fAddSpacesToString" IN hdJobProcs, cJobNo, 6, TRUE
                          ).
    
    RUN pUpdateComboBoxes.
            
    IF lParse THEN
        ASSIGN
            cbJobNo2:SCREEN-VALUE  = IF INDEX(cJobno2ListItems,cJobNo2) GT 0 THEN 
                                         cJobNo2
                                     ELSE
                                         ENTRY(1,cJobno2ListItems)
            cbFormNo:SCREEN-VALUE  = IF INDEX(cFormnoListItems,cFormNo) GT 0 THEN 
                                         cFormNo
                                     ELSE
                                         ENTRY(1,cFormnoListItems)
            cbBlankNo:SCREEN-VALUE = IF INDEX(cBlanknoListitems,cBlankNo) GT 0 THEN 
                                         cBlankNo
                                     ELSE
                                         ENTRY(1,cBlanknoListitems)
            cbMachine:SCREEN-VALUE = ENTRY(1,cMachineListItems)
            .
    ELSE
        ASSIGN 
            cbJobno2:SCREEN-VALUE  = ENTRY(1,cJobno2ListItems)
            cbFormno:SCREEN-VALUE  = ENTRY(1,cFormnoListItems)
            cbBlankno:SCREEN-VALUE = ENTRY(1,cBlanknoListItems)
            cbMachine:SCREEN-VALUE = ENTRY(1,cMachineListItems)
            .            

    RUN ValidateJob IN hdJobProcs (
        ipcCompany,
        cFormattedJobno,
        cbMachine:SCREEN-VALUE,
        INTEGER(cbJobNo2:SCREEN-VALUE),
        INTEGER(cbFormNo:SCREEN-VALUE),
        INTEGER(cbBlankNo:SCREEN-VALUE),
        OUTPUT lValidJob
        ).

    RUN pUpdateRMItemList (
        ipcCompany,
        cFormattedJobno,
        INTEGER(cbJobNo2:SCREEN-VALUE),
        INTEGER(cbFormNo:SCREEN-VALUE),
        INTEGER(cbBlankNo:SCREEN-VALUE)
        ).
        
    IF lValidJob THEN
        RUN pEnableCreate.
    
    cValidateJobno = fiJobno:SCREEN-VALUE.                                                            

    RUN pRebuildBrowse (
        INPUT ipcCompany,
        INPUT cFormattedJobno,
        INPUT cbMachine:SCREEN-VALUE,
        INPUT cbJobno2:SCREEN-VALUE,
        INPUT cbFormno:SCREEN-VALUE,
        INPUT cbBlankno:SCREEN-VALUE
        ).    
        
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.    
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


&Scoped-define SELF-NAME rsItemType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsItemType W-Win
ON VALUE-CHANGED OF rsItemType IN FRAME F-Main
DO: 
    
    ASSIGN
        fiRMItem:HIDDEN    = TRUE
        cbRMItem:HIDDEN    = TRUE
        cbRMItem:SENSITIVE = FALSE
        .

    CASE SELF:SCREEN-VALUE:
        WHEN rsItemTypeFG THEN
            rsValue = gcItemTypeFG.
        WHEN rsItemTypeRM THEN DO:
            ASSIGN
                rsValue            = gcItemTypeRM
                fiRMItem:HIDDEN    = FALSE
                cbRMItem:HIDDEN    = FALSE
                cbRMItem:SENSITIVE = TRUE
                .
        END.
        WHEN rsItemTypeWP THEN
            rsValue = gcItemTypeWIP.
    END CASE.
    
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
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
  DISPLAY rsItemType fiJobno cbJobno2 cbFormno cbBlankno cbMachine cbRMItem 
          fiRMItem fiQtyPerTag fiTotRunQty fiNumTags fiMessage 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-27 rsItemType fiJobno cbJobno2 cbFormno cbBlankno cbMachine 
         cbRMItem btKeyboard btCreate fiQtyPerTag fiTotRunQty fiNumTags 
         br-table btAdjQty btPrintSelected btPrintAll btNumPad-1 btNumPad-2 
         btNumPad-3 btExit btFirst btLast btNext btPrevious btnNumPad 
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
        fiQtyPerTag:SENSITIVE = FALSE
        fiTotRunQty:SENSITIVE = FALSE
        fiNumTags:SENSITIVE   = FALSE
        btCreate:SENSITIVE    = FALSE
        btNumpad-1:SENSITIVE  = FALSE
        btNumpad-2:SENSITIVE  = FALSE
        btNumpad-3:SENSITIVE  = FALSE
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
        fiQtyPerTag:SENSITIVE = TRUE
        fiTotRunQty:SENSITIVE = TRUE
        fiNumTags:SENSITIVE   = TRUE
        btCreate:SENSITIVE    = TRUE
        btNumpad-1:SENSITIVE  = TRUE
        btNumpad-2:SENSITIVE  = TRUE
        btNumpad-3:SENSITIVE  = TRUE
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
                         + STRING(company.name) + " - " + ipcLocation  .

    ASSIGN
        fiRMItem:HIDDEN    = TRUE
        cbRMItem:HIDDEN    = TRUE
        cbRMItem:SENSITIVE = FALSE
        .

    rsItemType:DISABLE("Raw Materials").
            
    RUN pGetSettings (
        ipcCompany, 
        OUTPUT cOutputFileName, 
        OUTPUT cPathTemplate, 
        OUTPUT iCopies
        ).
                         
    RUN pDisableCreate.                         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOnValueChangedOfJobDetails W-Win 
PROCEDURE pOnValueChangedOfJobDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lValidJob AS LOGICAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    RUN pDisableCreate.
    
    RUN ValidateJob IN hdJobProcs (
        ipcCompany,
        cFormattedJobno,
        cbMachine:SCREEN-VALUE,
        INTEGER(cbJobNo2:SCREEN-VALUE),
        INTEGER(cbFormNo:SCREEN-VALUE),
        INTEGER(cbBlankNo:SCREEN-VALUE),
        OUTPUT lValidJob
        ).

    RUN pUpdateRMItemList (
        ipcCompany,
        cFormattedJobno,
        INTEGER(cbJobNo2:SCREEN-VALUE),
        INTEGER(cbFormNo:SCREEN-VALUE),
        INTEGER(cbBlankNo:SCREEN-VALUE)
        ).

    IF lValidJob THEN
        RUN pEnableCreate.  

    RUN pRebuildBrowse (
        INPUT ipcCompany,
        INPUT cFormattedJobno,
        INPUT cbMachine:SCREEN-VALUE,
        INPUT cbJobno2:SCREEN-VALUE,
        INPUT cbFormno:SCREEN-VALUE,
        INPUT cbBlankno:SCREEN-VALUE
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

    RUN Output_TempTableToCSV IN hdOutputProcs ( 
        INPUT TEMP-TABLE ttPrintInventoryStockFG:HANDLE,
        INPUT cOutputFileName,
        INPUT TRUE,
        INPUT FALSE, /* Auto increment File name */
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).
        
    RUN pRebuildBrowse (
        INPUT ipcCompany,
        INPUT cFormattedJobno,
        INPUT cbMachine:SCREEN-VALUE,
        INPUT cbJobno2:SCREEN-VALUE,
        INPUT cbFormno:SCREEN-VALUE,
        INPUT cbBlankno:SCREEN-VALUE
        ).         
        
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME} IN FRAME  {&FRAME-NAME}.
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
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobno   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcMachine AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobno2  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormno  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankno AS INTEGER   NO-UNDO.
    
    RUN RebuildWIPBrowseTT IN hdInventoryProcs (
        ipcCompany,
        ipcJobno,
        ipcMachine,
        ipiJobno2,
        ipiFormno,
        ipiBlankno,
        OUTPUT iTotTags,
        OUTPUT iTotOnHand
        ).
            
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateComboBoxes W-Win 
PROCEDURE pUpdateComboBoxes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        cJobno2ListItems  = ""
        cFormnoListItems  = ""
        cBlanknoListItems = ""
        cMachineListItems = ""
        .
        
    RUN GetSecondaryJobForJob IN hdJobProcs (
        ipcCompany,
        cFormattedJobno,
        INPUT-OUTPUT cJobno2ListItems
        ).
    
    DO iCount = 1 TO NUM-ENTRIES(cJobno2ListItems):
        RUN GetFormnoForJob IN hdJobProcs (
            ipcCompany,
            cFormattedJobno,
            INTEGER(ENTRY(iCount, cJobno2ListItems)),
            INPUT-OUTPUT cFormnoListItems
            ).
    
        RUN GetBlanknoForJob IN hdJobProcs (
            ipcCompany,
            cFormattedJobno,
            INTEGER(ENTRY(iCount, cJobno2ListItems)),
            INPUT-OUTPUT cBlanknoListItems
            ).

        RUN GetOperationsForJob IN hdJobProcs (
            ipcCompany,
            cFormattedJobno,
            INTEGER(ENTRY(iCount, cJobno2ListItems)),
            INPUT-OUTPUT cMachineListItems
            ).
    END.
    
    IF cJobno2ListItems EQ "" THEN
        ASSIGN 
            cJobno2ListItems       = "00"
            cbJobno2:LIST-ITEMS    = cJobno2ListItems 
            cbJobno2:SCREEN-VALUE  = "00".
    ELSE
        cbJobno2:LIST-ITEMS  = cJobno2ListItems.
 
    IF cFormnoListItems EQ "" THEN
        ASSIGN
            cFormnoListItems       = "00"
            cbFormno:LIST-ITEMS    = cFormnoListItems 
            cbFormno:SCREEN-VALUE  = "00".
    ELSE
        cbFormno:LIST-ITEMS  = cFormnoListItems.

    IF cBlanknoListItems EQ "" THEN
        ASSIGN
            cBlanknoListItems       = "00"
            cbBlankno:LIST-ITEMS    = cBlanknoListItems
            cbBlankno:SCREEN-VALUE  = "00".
    ELSE
        cbBlankno:LIST-ITEMS  = cBlanknoListItems.

    IF cMachineListItems EQ "" THEN
        ASSIGN
            cMachineListItems       = ""
            cbMachine:LIST-ITEMS    = cMachineListItems
            cbMachine:SCREEN-VALUE  = "".
    ELSE
        cbMachine:LIST-ITEMS  = cMachineListItems.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRMItemList W-Win 
PROCEDURE pUpdateRMItemList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormno       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankno      AS INTEGER   NO-UNDO.
        
    RUN GetRMItemsForJob IN hdJobProcs (
        ipcCompany,
        ipcJobNo,
        ipiJobNo2,
        ipiFormNo,
        ipiBlankNo,
        OUTPUT cRMListItems
        ).
    
    cbRMItem:LIST-ITEMS IN FRAME {&FRAME-NAME} = cRMListitems.
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

