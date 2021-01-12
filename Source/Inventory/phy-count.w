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

  File: phy-count.w

  Description: Physical Count Scan And Location

  Input Parameters:
      ipcCompany   : Company Code
      ipcLocation  : Location

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
DEFINE INPUT PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLocation    AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE lCreated              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage              AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdInventoryProcs      AS HANDLE    NO-UNDO.
DEFINE VARIABLE cLocationID           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWarehouseID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cColumnHandles        AS CHARACTER NO-UNDO.

{system/sysconst.i}
{wip/keyboardDefs.i}
{Inventory/ttInventory.i "NEW SHARED"}
{methods/template/brwcustomdef.i}

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
&Scoped-define INTERNAL-TABLES ttPhysicalBrowseInventory

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table ttPhysicalBrowseInventory.tag ttPhysicalBrowseInventory.itemID ttPhysicalBrowseInventory.quantity ttPhysicalBrowseInventory.origQuantity ttPhysicalBrowseInventory.location ttPhysicalBrowseInventory.origLocation ttPhysicalBrowseInventory.inventoryStatus   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH ttPhysicalBrowseInventory BY ttPhysicalBrowseInventory.lastTransTime DESCENDING
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH ttPhysicalBrowseInventory BY ttPhysicalBrowseInventory.lastTransTime DESCENDING.
&Scoped-define TABLES-IN-QUERY-br-table ttPhysicalBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ttPhysicalBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btDelete btnNumPad btFirst btLast btNext ~
btPrevious bt-exit rcNotScanned btnKeyboard-2 rcNotScannedConf ~
rcCompleteMatch btnKeyboard-3 rcLocationChange rcQuantityChange ~
rcQuantityLocationChange rcTagNotFound fiTag fiLocation btSubmit ~
cbWarehouse fiBin br-table btAdjustQty 
&Scoped-Define DISPLAYED-OBJECTS fiTag fiItemno fiItemType fiLocation ~
fiCustno cbWarehouse fiBin fiTagMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29.

DEFINE BUTTON btAdjustQty 
     LABEL "Adjust Quantity" 
     SIZE 54 BY 3
     FGCOLOR 1 FONT 37.

DEFINE BUTTON btDelete 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_cross_disabled.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Delete".

DEFINE BUTTON btFirst 
     IMAGE-UP FILE "Graphics/32x32/navigate_up2.ico":U NO-FOCUS
     LABEL "First" 
     SIZE 9.6 BY 2.29 TOOLTIP "First".

DEFINE BUTTON btLast 
     IMAGE-UP FILE "Graphics/32x32/navigate_down2.ico":U NO-FOCUS
     LABEL "Last" 
     SIZE 9.6 BY 2.29 TOOLTIP "Last".

DEFINE BUTTON btNext 
     IMAGE-UP FILE "Graphics/32x32/navigate_down.ico":U NO-FOCUS
     LABEL "Next" 
     SIZE 9.6 BY 2.29 TOOLTIP "Next".

DEFINE BUTTON btnKeyboard-2 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnKeyboard-3 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btPrevious 
     IMAGE-UP FILE "Graphics/32x32/navigate_up.ico":U NO-FOCUS
     LABEL "Previous" 
     SIZE 9.6 BY 2.29 TOOLTIP "Previous".

DEFINE BUTTON btSubmit 
     LABEL "Submit" 
     SIZE 24.8 BY 2
     FONT 37.

DEFINE VARIABLE cbWarehouse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 19 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiBin AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23.8 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiCustno AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiItemno AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiItemType AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiLocation AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiTag AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 65 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiTagMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     FGCOLOR 12 FONT 35 NO-UNDO.

DEFINE RECTANGLE rcCompleteMatch
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 3 BY .71
     FGCOLOR 12 .

DEFINE RECTANGLE rcLocationChange
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 3 BY .71
     FGCOLOR 12 .

DEFINE RECTANGLE rcNotScanned
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 3 BY .71
     FGCOLOR 12 .

DEFINE RECTANGLE rcNotScannedConf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 3 BY .71
     FGCOLOR 12 .

DEFINE RECTANGLE rcQuantityChange
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 3 BY .71
     FGCOLOR 12 .

DEFINE RECTANGLE rcQuantityLocationChange
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 3 BY .71
     FGCOLOR 12 .

DEFINE RECTANGLE rcTagNotFound
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 3 BY .71
     FGCOLOR 12 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 80.2 BY 5
     FGCOLOR 1 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      ttPhysicalBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table W-Win _FREEFORM
  QUERY br-table DISPLAY
      ttPhysicalBrowseInventory.tag WIDTH 45 COLUMN-LABEL "Tag #" FORMAT "X(30)"
ttPhysicalBrowseInventory.itemID WIDTH 35 COLUMN-LABEL "Item" FORMAT "X(15)"
ttPhysicalBrowseInventory.quantity WIDTH 23 COLUMN-LABEL "Qty"
ttPhysicalBrowseInventory.origQuantity WIDTH 23 COLUMN-LABEL "Original Qty"
ttPhysicalBrowseInventory.location WIDTH 26 COLUMN-LABEL "Location" FORMAT "X(12)"
ttPhysicalBrowseInventory.origLocation WIDTH 26 COLUMN-LABEL "Original Location" FORMAT "X(12)"
ttPhysicalBrowseInventory.inventoryStatus COLUMN-LABEL "Status" FORMAT "X(25)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 188 BY 20.48
         FONT 36 ROW-HEIGHT-CHARS 1.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btDelete AT ROW 19 COL 192 WIDGET-ID 116
     btnNumPad AT ROW 2.19 COL 97 WIDGET-ID 120
     btFirst AT ROW 10 COL 192 WIDGET-ID 128
     btLast AT ROW 28.24 COL 192 WIDGET-ID 130
     btNext AT ROW 23.62 COL 192.2 WIDGET-ID 132
     btPrevious AT ROW 14.38 COL 192.2 WIDGET-ID 134
     bt-exit AT ROW 1.24 COL 192 WIDGET-ID 84
     btnKeyboard-2 AT ROW 2.05 COL 85 WIDGET-ID 142
     btnKeyboard-3 AT ROW 3.81 COL 66.2 WIDGET-ID 144
     fiTag AT ROW 2.1 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     fiItemno AT ROW 2.91 COL 119 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fiItemType AT ROW 2.91 COL 175 COLON-ALIGNED NO-LABEL WIDGET-ID 148
     fiLocation AT ROW 3.86 COL 23.2 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     fiCustno AT ROW 4.24 COL 125.2 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     btSubmit AT ROW 5.43 COL 82 WIDGET-ID 160
     cbWarehouse AT ROW 5.76 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 154
     fiBin AT ROW 5.76 COL 53.6 COLON-ALIGNED NO-LABEL WIDGET-ID 164
     fiTagMessage AT ROW 5.76 COL 109.4 COLON-ALIGNED NO-LABEL WIDGET-ID 162
     br-table AT ROW 10.05 COL 2 WIDGET-ID 200
     btAdjustQty AT ROW 30.95 COL 2 WIDGET-ID 110
     "Tag:" VIEW-AS TEXT
          SIZE 8.2 BY 1.19 AT ROW 2.19 COL 10.4 WIDGET-ID 22
          BGCOLOR 15 FGCOLOR 1 FONT 36
     "Customer #:" VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 4.29 COL 111.2 WIDGET-ID 78
          FGCOLOR 1 FONT 34
     "Tag Not Found" VIEW-AS TEXT
          SIZE 19.2 BY .81 AT ROW 8.86 COL 6.8 WIDGET-ID 196
          FONT 35
     "Not Scanned - Confirmed" VIEW-AS TEXT
          SIZE 33 BY .62 AT ROW 7.81 COL 29 WIDGET-ID 176
          FONT 35
     "Location Scan:" VIEW-AS TEXT
          SIZE 21 BY 1.19 AT ROW 3.95 COL 3 WIDGET-ID 32
          FGCOLOR 1 FONT 36
     "Quantity Change" VIEW-AS TEXT
          SIZE 23 BY .81 AT ROW 7.76 COL 128 WIDGET-ID 188
          FONT 35
     "Bin:" VIEW-AS TEXT
          SIZE 6.8 BY 1.19 AT ROW 5.81 COL 48.4 WIDGET-ID 158
          FGCOLOR 1 FONT 36
     "Location Change" VIEW-AS TEXT
          SIZE 23.2 BY .81 AT ROW 7.81 COL 98.8 WIDGET-ID 184
          FONT 35
     "Item Type:" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 2.95 COL 163.2 WIDGET-ID 150
          FGCOLOR 1 FONT 34
     "Complete Match" VIEW-AS TEXT
          SIZE 22.2 BY .81 AT ROW 7.81 COL 68.8 WIDGET-ID 180
          FONT 35
     "Item #:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 2.95 COL 110.6 WIDGET-ID 74
          FGCOLOR 1 FONT 34
     "Tag Details" VIEW-AS TEXT
          SIZE 16.2 BY .76 AT ROW 1.76 COL 113.8 WIDGET-ID 28
          FGCOLOR 1 FONT 35
     "Not Scanned" VIEW-AS TEXT
          SIZE 17.2 BY .62 AT ROW 7.81 COL 6.8 WIDGET-ID 172
          FONT 35
     "Warehouse:" VIEW-AS TEXT
          SIZE 17.4 BY 1.19 AT ROW 5.76 COL 6.6 WIDGET-ID 152
          FGCOLOR 1 FONT 36
     "Quantity and Location Change" VIEW-AS TEXT
          SIZE 41.6 BY .81 AT ROW 7.76 COL 157.4 WIDGET-ID 192
          FONT 35
     RECT-27 AT ROW 2.19 COL 109.6 WIDGET-ID 26
     RECT-2 AT ROW 1.95 COL 96 WIDGET-ID 146
     rcNotScanned AT ROW 7.81 COL 2.6 WIDGET-ID 166
     rcNotScannedConf AT ROW 7.81 COL 24.8 WIDGET-ID 174
     rcCompleteMatch AT ROW 7.81 COL 64.6 WIDGET-ID 178
     rcLocationChange AT ROW 7.81 COL 94.6 WIDGET-ID 182
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 204 BY 36.19
         BGCOLOR 15  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     rcQuantityChange AT ROW 7.81 COL 123.8 WIDGET-ID 186
     rcQuantityLocationChange AT ROW 7.81 COL 153.2 WIDGET-ID 190
     rcTagNotFound AT ROW 8.91 COL 2.6 WIDGET-ID 194
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 204 BY 36.19
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
         TITLE              = "Scan Physical Counts and Location"
         HEIGHT             = 32.81
         WIDTH              = 204
         MAX-HEIGHT         = 36.57
         MAX-WIDTH          = 204
         VIRTUAL-HEIGHT     = 36.57
         VIRTUAL-WIDTH      = 204
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
/* BROWSE-TAB br-table fiTagMessage F-Main */
ASSIGN 
       btnKeyboard-2:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboard-3:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiCustno IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiItemno IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiItemType IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTagMessage IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       rcCompleteMatch:PRIVATE-DATA IN FRAME F-Main     = 
                "Complete Match".

ASSIGN 
       rcLocationChange:PRIVATE-DATA IN FRAME F-Main     = 
                "Location Change".

ASSIGN 
       rcNotScanned:PRIVATE-DATA IN FRAME F-Main     = 
                "Not Scanned".

ASSIGN 
       rcNotScannedConf:PRIVATE-DATA IN FRAME F-Main     = 
                "Not Scanned - Confirmed".

ASSIGN 
       rcQuantityChange:PRIVATE-DATA IN FRAME F-Main     = 
                "Quantity Change".

ASSIGN 
       rcQuantityLocationChange:PRIVATE-DATA IN FRAME F-Main     = 
                "Quantity and Location Change".

ASSIGN 
       rcTagNotFound:PRIVATE-DATA IN FRAME F-Main     = 
                "Tag Not Found".

/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-27 IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttPhysicalBrowseInventory
BY ttPhysicalBrowseInventory.lastTransTime DESCENDING.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Scan Physical Counts and Location */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Scan Physical Counts and Location */
DO:
    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE OBJECT hdInventoryProcs.
    
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
ON ROW-DISPLAY OF br-table IN FRAME F-Main
DO:
    &scoped-define exclude-row-display true 
    {methods/template/brwrowdisplay.i}    
    DEFINE VARIABLE iColor  AS INTEGER  NO-UNDO.
    
    IF AVAILABLE ttPhysicalBrowseInventory THEN DO:
        iColor = DYNAMIC-FUNCTION (
                 "fGetRowBGColor" IN hdInventoryProcs,
                 INPUT ttPhysicalBrowseInventory.inventoryStatus
                 ).
                 
        RUN pUpdateRowColor (
            iColor
            ).
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit W-Win
ON CHOOSE OF bt-exit IN FRAME F-Main
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    
    RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAdjustQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAdjustQty W-Win
ON CHOOSE OF btAdjustQty IN FRAME F-Main /* Adjust Quantity */
DO:
    DEFINE VARIABLE lReturned AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lValue    AS DECIMAL NO-UNDO.
    
    IF AVAILABLE ttPhysicalBrowseInventory THEN DO:
        RUN inventory/calculatorKeypad.w (
            OUTPUT lReturned,
            OUTPUT lValue
            ).
    
        IF lReturned THEN DO:
            IF ttPhysicalBrowseInventory.quantity EQ lValue THEN DO:
                MESSAGE "Adjusted quantity for tag " + ttPhysicalBrowseInventory.tag +
                        " is same as existing quantity" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.
            
            MESSAGE "Adjust quantity of tag " + ttPhysicalBrowseInventory.tag +
                    " to " + STRING(lValue) "?" VIEW-AS ALERT-BOX QUESTION
                    TITLE "Adjust Quantity" UPDATE lContinue AS LOGICAL.
            IF lContinue THEN
                RUN pAdjustQuantity (
                    ipcCompany,
                    ttPhysicalBrowseInventory.tag,
                    lValue
                    ).                     
        END.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete W-Win
ON CHOOSE OF btDelete IN FRAME F-Main
DO:          
    RUN pDelete.
        
    {&OPEN-BROWSERS-IN-QUERY-F-Main}  
    
    APPLY "VALUE-CHANGED" TO br-table.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFirst W-Win
ON CHOOSE OF btFirst IN FRAME F-Main /* First */
DO:
    RUN pNavigate (
        SELF
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLast W-Win
ON CHOOSE OF btLast IN FRAME F-Main /* Last */
DO:
    RUN pNavigate (
        SELF
        ).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNext W-Win
ON CHOOSE OF btNext IN FRAME F-Main /* Next */
DO:
    RUN pNavigate (
        SELF
        ).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboard-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboard-2 W-Win
ON CHOOSE OF btnKeyboard-2 IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY" TO fiTag.
    RUN pKeyboard (
        fiTag:HANDLE,
        "Qwerty"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboard-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboard-3 W-Win
ON CHOOSE OF btnKeyboard-3 IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY" TO fiLocation.
    RUN pKeyboard (
        fiLocation:HANDLE, 
        "Qwerty"
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


&Scoped-define SELF-NAME btPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrevious W-Win
ON CHOOSE OF btPrevious IN FRAME F-Main /* Previous */
DO:
    RUN pNavigate (
        SELF
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSubmit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSubmit W-Win
ON CHOOSE OF btSubmit IN FRAME F-Main /* Submit */
DO:
    RUN pSubmitScan (
        INPUT ipcCompany,
        INPUT cbWarehouse:SCREEN-VALUE,
        INPUT fiBin:SCREEN-VALUE,
        INPUT fiTag:SCREEN-VALUE
        ).
                        
    {&OPEN-BROWSERS-IN-QUERY-F-Main}             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBin W-Win
ON ENTRY OF fiBin IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
        RUN pKeyboard (
            SELF, 
            "Qwerty"
            ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON ENTRY OF fiLocation IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
        RUN pKeyboard (
            SELF, 
            "Qwerty"
            ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON LEAVE OF fiLocation IN FRAME F-Main
DO:
    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
    
    RUN LocationParser IN hdInventoryProcs (
        INPUT  fiLocation:SCREEN-VALUE,    
        OUTPUT cWarehouseID,
        OUTPUT cLocationID
        ).
        
    RUN pLocationScan (
        INPUT ipcCompany,
        INPUT cWarehouseID,
        INPUT cLocationID,
        INPUT fiTag:SCREEN-VALUE
        ).                        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON ENTRY OF fiTag IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
        RUN pKeyboard (
            SELF, 
            "Qwerty"
            ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON LEAVE OF fiTag IN FRAME F-Main
DO:
    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    RUN pTagScan (
        ipcCompany,
        SELF:SCREEN-VALUE
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
{methods/template/brwcustom.i}

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

{wip/pKeyboard.i}

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
  DISPLAY fiTag fiItemno fiItemType fiLocation fiCustno cbWarehouse fiBin 
          fiTagMessage 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btDelete btnNumPad btFirst btLast btNext btPrevious bt-exit 
         rcNotScanned btnKeyboard-2 rcNotScannedConf rcCompleteMatch 
         btnKeyboard-3 rcLocationChange rcQuantityChange 
         rcQuantityLocationChange rcTagNotFound fiTag fiLocation btSubmit 
         cbWarehouse fiBin br-table btAdjustQty 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init W-Win 
PROCEDURE init :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cWarehouseListItems AS CHARACTER NO-UNDO.
    
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.

    FIND FIRST company NO-LOCK 
         WHERE company.company EQ ipcCompany
         NO-ERROR .
    IF AVAILABLE company THEN
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                         + " - {&awversion}" + " - " 
                         + STRING(company.name) + " - " + ipcLocation.
    RUN pAddLegend.

    RUN GetWarehouseList IN hdInventoryProcs (
        "",   /* Company. Pass empty if needed list of all warehouses across all companies are required */
        TRUE, /* Active location only */
        OUTPUT cWarehouseListItems
        ).
        
    cbWarehouse:LIST-ITEMS IN FRAME {&FRAME-NAME} = cWarehouseListItems.
    
    RUN BuildPhyScanBrowseFromTransactionUser IN hdInventoryProcs (
        ipcCompany,
        USERID(gcDBUser),           /* User ID */
        gcTransactionTypeCompare /* Compare Transaction Type */
        ).
            
    RUN pStoreColHandles.
                
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
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
  RUN init.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddLegend W-Win 
PROCEDURE pAddLegend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        rcNotScanned:BGCOLOR = DYNAMIC-FUNCTION (
                               "fGetRowBGColor" IN hdInventoryProcs,
                               rcNotScanned:PRIVATE-DATA
                               )
        rcNotScannedConf:BGCOLOR = DYNAMIC-FUNCTION (
                               "fGetRowBGColor" IN hdInventoryProcs,
                               rcNotScannedConf:PRIVATE-DATA
                               )
        rcCompleteMatch:BGCOLOR = DYNAMIC-FUNCTION (
                               "fGetRowBGColor" IN hdInventoryProcs,
                               rcCompleteMatch:PRIVATE-DATA
                               )
        rcLocationChange:BGCOLOR = DYNAMIC-FUNCTION (
                               "fGetRowBGColor" IN hdInventoryProcs,
                               rcLocationChange:PRIVATE-DATA
                               )
        rcQuantityChange:BGCOLOR = DYNAMIC-FUNCTION (
                               "fGetRowBGColor" IN hdInventoryProcs,
                               rcQuantityChange:PRIVATE-DATA
                               )
        rcQuantityLocationChange:BGCOLOR = DYNAMIC-FUNCTION (
                               "fGetRowBGColor" IN hdInventoryProcs,
                               rcQuantityLocationChange:PRIVATE-DATA
                               )
        rcTagNotFound:BGCOLOR = DYNAMIC-FUNCTION (
                               "fGetRowBGColor" IN hdInventoryProcs,
                               rcTagNotFound:PRIVATE-DATA
                               )
        .                       
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
    DEFINE INPUT  PARAMETER ipcTag              AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantity         AS DECIMAL   NO-UNDO.
    
    RUN pAdjustTransactionQuantity IN hdInventoryProcs (
        ipcCompany,
        ipcTag,
        ipdQuantity, /* Zeroing out */
        OUTPUT lCreated,
        OUTPUT cMessage
        ).
        
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    
    APPLY "VALUE-CHANGED" TO br-table IN FRAME {&FRAME-NAME}.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDelete W-Win 
PROCEDURE pDelete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF AVAILABLE ttPhysicalBrowseInventory THEN DO:
        MESSAGE "Delete Selection?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE lDelete AS LOGICAL.
            
        IF lDelete THEN DO:
            FIND FIRST inventoryTransaction NO-LOCK
                 WHERE inventoryTransaction.company         EQ ttPhysicalBrowseInventory.company
                   AND inventoryTransaction.tag             EQ ttPhysicalBrowseInventory.tag
                   AND inventoryTransaction.transactionType EQ gcTransactionTypeCompare 
                   NO-ERROR.
            IF AVAILABLE inventoryTransaction THEN
                RUN DeleteInventoryTransaction IN hdInventoryProcs (
                    inventoryTransaction.inventoryTransactionID
                    ).    
                            
            ASSIGN
                ttPhysicalBrowseInventory.lastTransTime   = NOW
                ttPhysicalBrowseInventory.quantity        = 0
                ttPhysicalBrowseInventory.locationID      = ""
                ttPhysicalBrowseInventory.warehouseID     = ""
                ttPhysicalBrowseInventory.location        = ""
                ttPhysicalBrowseInventory.inventoryStatus = gcStatusSnapshotNotScanned
                .
        END.                       
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationScan W-Win 
PROCEDURE pLocationScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWarehouseID  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTag          AS CHARACTER NO-UNDO.
    
    RUN pSubmitScan (
        INPUT ipcCompany,
        INPUT ipcWarehouseID,
        INPUT ipcLocationID,
        INPUT ipcTag
        ).
                        
    {&OPEN-BROWSERS-IN-QUERY-F-Main}           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNavigate W-Win 
PROCEDURE pNavigate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphNavPanel AS HANDLE NO-UNDO.
    
    IF AVAILABLE ttPhysicalBrowseInventory THEN DO:
        CASE iphNavPanel:LABEL:
            WHEN "First" THEN
            APPLY "HOME":U TO BROWSE {&BROWSE-NAME}.
            WHEN "Previous" THEN
            BROWSE {&BROWSE-NAME}:SELECT-PREV-ROW().
            WHEN "Next" THEN
            BROWSE {&BROWSE-NAME}:SELECT-NEXT-ROW().
            WHEN "Last" THEN
            APPLY "END":U TO BROWSE {&BROWSE-NAME}.
        END CASE.
        APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.
    END. /* if avail */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pStoreColHandles W-Win 
PROCEDURE pStoreColHandles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdCurrColHdl AS HANDLE NO-UNDO.
    
    hdCurrColHdl = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.

    DO WHILE VALID-HANDLE(hdCurrColHdl):
        ASSIGN 
            cColumnHandles = IF cColumnHandles <> "":U THEN
                                 cColumnHandles + ",":U + STRING(hdCurrColHdl)
                             ELSE
                                 STRING(hdCurrColHdl)
            hdCurrColHdl   = hdCurrColHdl:NEXT-COLUMN
            .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSubmitScan W-Win 
PROCEDURE pSubmitScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWarehouseID  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTag          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lValidLoc            AS LOGICAL NO-UNDO.
        
    DO WITH FRAME {&FRAME-NAME}:
    END.
        
    IF ipcTag EQ "" THEN DO:
        MESSAGE "Blank Tag" VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
         
    IF ipcWarehouseID EQ "" THEN DO:
        MESSAGE "Blank Warehouse" VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.    

    IF ipcLocationID EQ "" THEN DO:
        MESSAGE "Blank Bin" VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.    

    RUN ValidateLoc IN hdInventoryProcs (
        ipcCompany,
        ipcWarehouseID,
        OUTPUT lValidLoc
        ).
        
    IF NOT lValidLoc THEN DO:
        MESSAGE "Invalid WarehouseID " + ipcWarehouseID
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    RUN SubmitPhysicalCountScan IN hdInventoryProcs (
        ipcCompany,
        ipcWarehouseID,
        ipcLocationID,
        ipcTag,
        TRUE, /* Set input parameter location to record */
        OUTPUT lCreated,
        OUTPUT cMessage
        ).
    
    IF NOT lCreated AND cMessage NE "" THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        IF cMessage = "Invalid Tag" THEN
            fiTagMessage:SCREEN-VALUE = "** Tag Not Found".
        RETURN.
    END.
    
    FIND FIRST ttPhysicalBrowseInventory NO-LOCK
         WHERE ttPhysicalBrowseInventory.company EQ ipcCompany
           AND ttPhysicalBrowseInventory.tag     EQ ipcTag NO-ERROR.
    IF AVAILABLE ttPhysicalBrowseInventory THEN        
        ASSIGN
            fiLocation:SCREEN-VALUE      = ttPhysicalBrowseInventory.location
            fiItemno:SCREEN-VALUE        = ttPhysicalBrowseInventory.itemID
            fiItemType:SCREEN-VALUE      = ttPhysicalBrowseInventory.itemType
            fiCustno:SCREEN-VALUE        = ttPhysicalBrowseInventory.customerID
            cbWarehouse:SCREEN-VALUE     = ttPhysicalBrowseInventory.warehouseID
            fiBin:SCREEN-VALUE           = ttPhysicalBrowseInventory.locationID
            fiTagMessage:SCREEN-VALUE    = ""
            .                  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTagScan W-Win 
PROCEDURE pTagScan :
/*------------------------------------------------------------------------------
  Purpose: Tag Scan
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTag     AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiLocation:SCREEN-VALUE   = ""
        fiItemno:SCREEN-VALUE     = ""
        fiItemType:SCREEN-VALUE   = ""
        fiCustno:SCREEN-VALUE     = ""    
        fiTag:SCREEN-VALUE        = ipcTag
        fiBin:SCREEN-VALUE        = ""
        fiTagMessage:SCREEN-VALUE = ""
        .

    FIND FIRST ttPhysicalBrowseInventory NO-LOCK
         WHERE ttPhysicalBrowseInventory.company EQ ipcCompany
           AND ttPhysicalBrowseInventory.tag     EQ ipcTag 
		 NO-ERROR.

    FIND FIRST inventoryStockSnapshot NO-LOCK
         WHERE inventoryStockSnapshot.company EQ ipcCompany
           AND inventoryStockSnapshot.tag     EQ ipcTag
		 NO-ERROR.
    
    IF AVAILABLE ttPhysicalBrowseInventory THEN DO:
        ASSIGN
            fiLocation:SCREEN-VALUE      = ttPhysicalBrowseInventory.location
            fiItemno:SCREEN-VALUE        = ttPhysicalBrowseInventory.itemID
            fiItemType:SCREEN-VALUE      = ttPhysicalBrowseInventory.itemType
            fiCustno:SCREEN-VALUE        = ttPhysicalBrowseInventory.customerID
            cbWarehouse:SCREEN-VALUE     = ttPhysicalBrowseInventory.warehouseID
            fiBin:SCREEN-VALUE           = ttPhysicalBrowseInventory.locationID
            fiTagMessage:SCREEN-VALUE    = ""
            .        
    END.
    ELSE IF AVAILABLE inventoryStockSnapshot THEN DO:
        ASSIGN
            fiLocation:SCREEN-VALUE      = inventoryStockSnapshot.warehouseID +
                                           FILL(" ", 5 - LENGTH(inventoryStockSnapshot.warehouseID)) +
                                           inventoryStockSnapshot.locationID
            fiItemno:SCREEN-VALUE        = IF inventoryStockSnapshot.fgItemID NE "" THEN
                                               inventoryStockSnapshot.fgItemID
                                           ELSE IF inventoryStockSnapshot.rmItemID NE "" THEN
                                               inventoryStockSnapshot.rmItemID
                                           ELSE
                                               inventoryStockSnapshot.wipItemID
            fiItemType:SCREEN-VALUE      = inventoryStockSnapshot.itemType
            fiCustno:SCREEN-VALUE        = inventoryStockSnapshot.customerID
            cbWarehouse:SCREEN-VALUE     = inventoryStockSnapshot.warehouseID
            fiBin:SCREEN-VALUE           = inventoryStockSnapshot.locationID
            .
    END. 
    ELSE DO:        
        FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company   EQ ipcCompany
               AND loadtag.item-type EQ YES
               AND loadtag.tag-no    EQ ipcTag
             NO-ERROR.
        IF NOT AVAILABLE loadtag THEN
            FIND FIRST loadtag NO-LOCK
                 WHERE loadtag.company   EQ ipcCompany
                   AND loadtag.item-type EQ NO
                   AND loadtag.tag-no    EQ ipcTag
                 NO-ERROR.
                 
        IF NOT AVAILABLE loadtag THEN DO:
            MESSAGE "Invalid Tag" VIEW-AS ALERT-BOX ERROR.
            fiTagMessage:SCREEN-VALUE = "* Tag Not Found".
            RETURN.
        END.
        
        ASSIGN
            fiLocation:SCREEN-VALUE      = loadtag.loc +
                                           FILL(" ", 5 - LENGTH(loadtag.loc)) +
                                           loadtag.loc-bin
            fiItemno:SCREEN-VALUE        = loadtag.i-no
            fiItemType:SCREEN-VALUE      = IF loadtag.item-type THEN
                                               "RM"
                                           ELSE
                                               "FG"
            cbWarehouse:SCREEN-VALUE     = loadtag.loc
            fiBin:SCREEN-VALUE           = loadtag.loc-bin
            .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRowColor W-Win 
PROCEDURE pUpdateRowColor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiColor   AS INTEGER NO-UNDO.

    DEFINE VARIABLE iNumCols AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCounter AS INTEGER NO-UNDO.
    DEFINE VARIABLE hdCurCol AS HANDLE  NO-UNDO.
    
    iNumCols = NUM-ENTRIES(cColumnHandles).
 
    DO iCounter = 1 TO iNumCols:
        ASSIGN 
            hdCurCol         = WIDGET-HANDLE(ENTRY(iCounter,cColumnHandles))
            hdCurCol:BGCOLOR = ipiColor.
    END.
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
  {src/adm/template/snd-list.i "ttPhysicalBrowseInventory"}

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

