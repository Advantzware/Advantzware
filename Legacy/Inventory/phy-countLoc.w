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

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ipcCompany          AS CHARACTER NO-UNDO INITIAL "001".
DEFINE VARIABLE ipcLocation         AS CHARACTER NO-UNDO INITIAL "MAIN".

DEFINE VARIABLE lCreated              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage              AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdInventoryProcs      AS HANDLE    NO-UNDO.
DEFINE VARIABLE cLocationID           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWarehouseID          AS CHARACTER NO-UNDO.

{system/sysconst.i}
{wip/keyboardDefs.i}
{Inventory/ttInventory.i "NEW SHARED"}

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
&Scoped-define FIELDS-IN-QUERY-br-table ttPhysicalBrowseInventory.stockIDAlias ttPhysicalBrowseInventory.itemID ttPhysicalBrowseInventory.quantity ttPhysicalBrowseInventory.location ttPhysicalBrowseInventory.inventoryStatus   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH ttPhysicalBrowseInventory     WHERE ( IF rsFilter:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1" THEN                 ttPhysicalBrowseInventory.inventoryStatus NE gcStatusSnapshotNotScanned             ELSE IF rsFilter:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "2" THEN                 ttPhysicalBrowseInventory.inventoryStatus EQ gcStatusSnapshotNotScanned             ELSE                 TRUE) BY ttPhysicalBrowseInventory.lastTransTime DESCENDING
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH ttPhysicalBrowseInventory     WHERE ( IF rsFilter:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1" THEN                 ttPhysicalBrowseInventory.inventoryStatus NE gcStatusSnapshotNotScanned             ELSE IF rsFilter:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "2" THEN                 ttPhysicalBrowseInventory.inventoryStatus EQ gcStatusSnapshotNotScanned             ELSE                 TRUE) BY ttPhysicalBrowseInventory.lastTransTime DESCENDING.
&Scoped-define TABLES-IN-QUERY-br-table ttPhysicalBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ttPhysicalBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btDelete btnKeyboard-4 RECT-28 fiLocation ~
btChange btnKeyboard-1 rsFilter cbWarehouse fiBin fiTag br-table ~
btnKeyboard-3 btnNumPad btFirst btLast bt-exit btNext btPrevious 
&Scoped-Define DISPLAYED-OBJECTS fiLocation rsFilter cbWarehouse fiBin ~
fiTag fiItemNo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29.

DEFINE BUTTON btAdjustQty 
     LABEL "Adjust Quantity" 
     SIZE 54 BY 3
     FGCOLOR 1 FONT 37.

DEFINE BUTTON btChange 
     LABEL "Change" 
     SIZE 29 BY 3
     FGCOLOR 1 FONT 37.

DEFINE BUTTON btConfirmAllNotScanned 
     LABEL "Confirm All Not Scanned" 
     SIZE 54 BY 3
     FGCOLOR 1 FONT 37.

DEFINE BUTTON btConfirmNotScanned 
     LABEL "Confirm Not Scanned" 
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

DEFINE BUTTON btnKeyboard-1 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnKeyboard-3 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnKeyboard-4 
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

DEFINE VARIABLE cbWarehouse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 25 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiBin AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiItemNo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 67.4 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiLocation AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiTag AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 65 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE rsFilter AS CHARACTER INITIAL "3" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Only Scanned", "1",
"Only Not Scanned", "2",
"All Tags in Location", "3"
     SIZE 43 BY 3.33
     FONT 37 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 80.2 BY 3.62
     FGCOLOR 1 .

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 197 BY .05.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      ttPhysicalBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table W-Win _FREEFORM
  QUERY br-table DISPLAY
      ttPhysicalBrowseInventory.stockIDAlias WIDTH 45 COLUMN-LABEL "Tag #" FORMAT "X(30)"
ttPhysicalBrowseInventory.itemID WIDTH 38 COLUMN-LABEL "Item" FORMAT "X(15)"
ttPhysicalBrowseInventory.quantity WIDTH 25 COLUMN-LABEL "Qty"
ttPhysicalBrowseInventory.location WIDTH 28 COLUMN-LABEL "Location" FORMAT "X(12)"
ttPhysicalBrowseInventory.inventoryStatus COLUMN-LABEL "Status" FORMAT "X(15)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 188 BY 19.05
         FONT 37 ROW-HEIGHT-CHARS 1.05 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btDelete AT ROW 19.57 COL 192 WIDGET-ID 116
     btnKeyboard-4 AT ROW 7.29 COL 85 WIDGET-ID 142
     fiLocation AT ROW 1.52 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     btChange AT ROW 1.71 COL 3 WIDGET-ID 164
     btnKeyboard-1 AT ROW 1.48 COL 98 WIDGET-ID 144
     rsFilter AT ROW 1.71 COL 133.8 NO-LABEL WIDGET-ID 166
     cbWarehouse AT ROW 3.62 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 172
     fiBin AT ROW 3.62 COL 93 COLON-ALIGNED NO-LABEL WIDGET-ID 160
     fiTag AT ROW 7.33 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     fiItemNo AT ROW 8.05 COL 118 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     br-table AT ROW 11.48 COL 2 WIDGET-ID 200
     btAdjustQty AT ROW 30.95 COL 2 WIDGET-ID 110
     btConfirmNotScanned AT ROW 31 COL 70.4 WIDGET-ID 174
     btConfirmAllNotScanned AT ROW 31 COL 136 WIDGET-ID 176
     btnKeyboard-3 AT ROW 3.57 COL 122 WIDGET-ID 158
     btnNumPad AT ROW 7.43 COL 97 WIDGET-ID 120
     btFirst AT ROW 11.52 COL 192 WIDGET-ID 128
     btLast AT ROW 28.24 COL 192 WIDGET-ID 130
     bt-exit AT ROW 1.24 COL 192 WIDGET-ID 84
     btNext AT ROW 24.86 COL 192.2 WIDGET-ID 132
     btPrevious AT ROW 14.76 COL 192.2 WIDGET-ID 134
     "Location:" VIEW-AS TEXT
          SIZE 14 BY 1.19 AT ROW 1.71 COL 41.8 WIDGET-ID 32
          FGCOLOR 1 FONT 36
     "Tag:" VIEW-AS TEXT
          SIZE 8.2 BY 1.19 AT ROW 7.43 COL 10.4 WIDGET-ID 22
          BGCOLOR 15 FGCOLOR 1 FONT 36
     "Tag Details" VIEW-AS TEXT
          SIZE 16.2 BY .76 AT ROW 7 COL 113.8 WIDGET-ID 28
          FGCOLOR 1 FONT 35
     "Bin:" VIEW-AS TEXT
          SIZE 6.4 BY 1.19 AT ROW 3.62 COL 87.6 WIDGET-ID 162
          FGCOLOR 1 FONT 36
     "Warehouse:" VIEW-AS TEXT
          SIZE 17.8 BY 1.19 AT ROW 3.62 COL 38 WIDGET-ID 156
          FGCOLOR 1 FONT 36
     "Item:" VIEW-AS TEXT
          SIZE 7.2 BY .81 AT ROW 8.1 COL 112.6 WIDGET-ID 150
          FGCOLOR 1 FONT 34
     RECT-27 AT ROW 7.43 COL 109.6 WIDGET-ID 26
     RECT-2 AT ROW 7.19 COL 96 WIDGET-ID 146
     RECT-28 AT ROW 5.91 COL 3.4 WIDGET-ID 170
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 204.8 BY 35.29
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
         TITLE              = "Scan Physical Counts By Location"
         HEIGHT             = 35.29
         WIDTH              = 204.8
         MAX-HEIGHT         = 36.57
         MAX-WIDTH          = 210.4
         VIRTUAL-HEIGHT     = 36.57
         VIRTUAL-WIDTH      = 210.4
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
/* BROWSE-TAB br-table fiItemNo F-Main */
/* SETTINGS FOR BUTTON btAdjustQty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btConfirmAllNotScanned IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btConfirmNotScanned IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnKeyboard-1:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboard-3:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboard-4:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiItemNo IN FRAME F-Main
   NO-ENABLE                                                            */
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
    WHERE ( IF rsFilter:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1" THEN
                ttPhysicalBrowseInventory.inventoryStatus NE gcStatusSnapshotNotScanned
            ELSE IF rsFilter:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "2" THEN
                ttPhysicalBrowseInventory.inventoryStatus EQ gcStatusSnapshotNotScanned
            ELSE
                TRUE)
BY ttPhysicalBrowseInventory.lastTransTime DESCENDING.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Scan Physical Counts By Location */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Scan Physical Counts By Location */
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
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:    
    ASSIGN
        btAdjustQty:SENSITIVE         = AVAILABLE ttPhysicalBrowseInventory
        btConfirmNotScanned:SENSITIVE = AVAILABLE ttPhysicalBrowseInventory AND ttPhysicalBrowseInventory.inventoryStatus EQ gcStatusSnapshotNotScanned
        .
        
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


&Scoped-define SELF-NAME btChange
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btChange W-Win
ON CHOOSE OF btChange IN FRAME F-Main /* Change */
DO:
    RUN pEnableContext.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboard-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboard-1 W-Win
ON CHOOSE OF btnKeyboard-1 IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY" TO fiLocation.
    RUN pKeyboard (fiLocation:HANDLE, "Qwerty").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboard-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboard-3 W-Win
ON CHOOSE OF btnKeyboard-3 IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY" TO fiLocation.
    RUN pKeyboard (fiBin:HANDLE, "Qwerty").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboard-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboard-4 W-Win
ON CHOOSE OF btnKeyboard-4 IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY" TO fiTag.
    RUN pKeyboard (fiTag:HANDLE, "Qwerty").
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


&Scoped-define SELF-NAME cbWarehouse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbWarehouse W-Win
ON VALUE-CHANGED OF cbWarehouse IN FRAME F-Main
DO:
    RUN pLocationScan (
        INPUT  ipcCompany,
        INPUT  cbWarehouse:SCREEN-VALUE,
        INPUT  fiBin:SCREEN-VALUE,
        OUTPUT cMessage
        ).

    IF cMessage NE "" THEN
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBin W-Win
ON ENTRY OF fiBin IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
        RUN pKeyboard (SELF, "Qwerty").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBin W-Win
ON LEAVE OF fiBin IN FRAME F-Main
DO:
    RUN pLocationScan (
        INPUT  ipcCompany,
        INPUT  cbWarehouse:SCREEN-VALUE,
        INPUT  fiBin:SCREEN-VALUE,
        OUTPUT cMessage
        ).

    IF cMessage NE "" THEN
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON ENTRY OF fiLocation IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
        RUN pKeyboard (SELF, "Qwerty").  
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
        INPUT  ipcCompany,
        INPUT  cWarehouseID,
        INPUT  cLocationID,
        OUTPUT cMessage
        ).

    IF cMessage NE "" THEN
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
             
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
    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    RUN pTagScan (
        INPUT ipcCompany,
        INPUT SELF:SCREEN-VALUE
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsFilter W-Win
ON VALUE-CHANGED OF rsFilter IN FRAME F-Main
DO:
    {&OPEN-BROWSERS-IN-QUERY-F-Main}  
    
    APPLY "VALUE-CHANGED" TO br-table.
    
    btConfirmAllNotScanned:SENSITIVE = FALSE.
    
    IF SELF:SCREEN-VALUE EQ "2" THEN
        btConfirmAllNotScanned:SENSITIVE = TRUE.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

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
  DISPLAY fiLocation rsFilter cbWarehouse fiBin fiTag fiItemNo 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btDelete btnKeyboard-4 RECT-28 fiLocation btChange btnKeyboard-1 
         rsFilter cbWarehouse fiBin fiTag br-table btnKeyboard-3 btnNumPad 
         btFirst btLast bt-exit btNext btPrevious 
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
    RUN pEnableContext.
        
    RUN GenerateSnapshotRecords IN hdInventoryProcs (
        "FG",              /* Item Type */
        "001",             /* Company */
        "MAIN",            /* Warehouse */
        "A-103",           /* Location */
        OUTPUT lCreated,
        OUTPUT cMessage
        ).
    
    RUN GenerateSnapshotRecords IN hdInventoryProcs (
        "FG",              /* Item Type */
        "001",             /* Company */
        "MAIN",            /* Warehouse */
        "A-201",           /* Location */
        OUTPUT lCreated,
        OUTPUT cMessage
        ).

    RUN GetWarehouseList IN hdInventoryProcs (
        "", /* Company. Pass empty if needed list of all warehouses across all companies are required */
        TRUE, /* Active location only */        
        OUTPUT cWarehouseListItems
        ).
        
    cbWarehouse:LIST-ITEMS IN FRAME {&FRAME-NAME} = cWarehouseListItems.
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildBrowseTable W-Win 
PROCEDURE pBuildBrowseTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcWarehouseID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID  AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttPhysicalBrowseInventory.
    
    FOR EACH inventoryStockSnapshot NO-LOCK
        WHERE inventoryStockSnapshot.warehouseID EQ ipcWarehouseID
          AND inventoryStockSnapshot.locationID  EQ ipcLocationID:
        
        FIND FIRST inventoryTransaction  NO-LOCK
             WHERE inventoryTransaction.stockIDAlias    EQ inventoryStockSnapshot.stockIDAlias
               AND inventoryTransaction.transactionType EQ gcTransactionTypeCompare NO-ERROR.
               
        CREATE ttPhysicalBrowseInventory.
        ASSIGN
            ttPhysicalBrowseInventory.company          = inventoryStockSnapshot.company
            ttPhysicalBrowseInventory.inventoryStockID = inventoryStockSnapshot.inventoryStockID
            ttPhysicalBrowseInventory.stockIDAlias     = inventoryStockSnapshot.stockIDAlias
            ttPhysicalBrowseInventory.itemType         = inventoryStockSnapshot.itemType
            ttPhysicalBrowseInventory.itemID           = IF inventoryStockSnapshot.fgItemID NE "" THEN
                                                             inventoryStockSnapshot.fgItemID
                                                         ELSE IF inventoryStockSnapshot.rmItemID NE "" THEN
                                                             inventoryStockSnapshot.rmItemID
                                                         ELSE
                                                             inventoryStockSnapshot.wipItemID
            ttPhysicalBrowseInventory.quantity         = IF AVAILABLE inventoryTransaction THEN
                                                             inventoryTransaction.quantityChange
                                                         ELSE    
                                                             inventoryStockSnapshot.quantity
            ttPhysicalBrowseInventory.customerID       = inventoryStockSnapshot.customerID
            ttPhysicalBrowseInventory.lastTransTime    = NOW
            ttPhysicalBrowseInventory.locationID       = IF AVAILABLE inventoryTransaction THEN
                                                             inventoryTransaction.locationID
                                                         ELSE    
                                                             inventoryStockSnapshot.locationID
            ttPhysicalBrowseInventory.warehouseID      = IF AVAILABLE inventoryTransaction THEN
                                                             inventoryTransaction.warehouseID
                                                         ELSE    
                                                             inventoryStockSnapshot.warehouseID
            ttPhysicalBrowseInventory.location         = IF AVAILABLE inventoryTransaction THEN
                                                             inventoryTransaction.warehouseID +
                                                             FILL(" ", 5 - LENGTH(inventoryTransaction.warehouseID)) +
                                                             inventoryTransaction.locationID            
                                                         ELSE    
                                                             inventoryStockSnapshot.warehouseID +
                                                             FILL(" ", 5 - LENGTH(inventoryStockSnapshot.warehouseID)) +
                                                             inventoryStockSnapshot.locationID            
            ttPhysicalBrowseInventory.inventoryStatus  = IF AVAILABLE inventoryTransaction THEN
                                                             DYNAMIC-FUNCTION (
                                                             "fGetSnapshotCompareStatus" IN hdInventoryProcs, 
                                                             ttPhysicalBrowseInventory.company,
                                                             ttPhysicalBrowseInventory.stockIDAlias,
                                                             ttPhysicalBrowseInventory.quantity,
                                                             ttPhysicalBrowseInventory.warehouseID,
                                                             ttPhysicalBrowseInventory.locationID
                                                             )
                                                         ELSE
                                                            gcStatusSnapshotNotScanned.    
                                                         
            .
          
    END.
    
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    
    APPLY "VALUE-CHANGED" TO br-table.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisbaleContext W-Win 
PROCEDURE pDisbaleContext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiLocation:SENSITIVE    = FALSE
        cbWarehouse:SENSITIVE   = FALSE
        fiBin:SENSITIVE         = FALSE
        btnKeyboard-1:SENSITIVE = FALSE
        btnKeyboard-3:SENSITIVE = FALSE
        fiTag:SENSITIVE         = TRUE
        btnKeyboard-4:SENSITIVE = TRUE. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEnableContext W-Win 
PROCEDURE pEnableContext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiLocation:SENSITIVE    = TRUE
        cbWarehouse:SENSITIVE   = TRUE
        fiBin:SENSITIVE         = TRUE
        btnKeyboard-1:SENSITIVE = TRUE
        btnKeyboard-3:SENSITIVE = TRUE
        fiTag:SENSITIVE         = FALSE
        btnKeyboard-4:SENSITIVE = FALSE. 

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
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouseID  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocationID   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lValidLoc AS LOGICAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF ipcWarehouseID EQ "" THEN DO:
        opcMessage = "Blank Warehouse".
        RETURN.
    END.    

    IF ipcLocationID EQ "" THEN DO:
        opcMessage = "Blank Bin".
        RETURN.
    END.    

    RUN ValidateLoc IN hdInventoryProcs (
        ipcCompany,
        ipcWarehouseID,
        OUTPUT lValidLoc
        ).
        
    IF NOT lValidLoc THEN DO:
        opcMessage = "Invalid WarehouseID " + ipcWarehouseID.
        RETURN.
    END.
    
    RUN pBuildBrowseTable (
        INPUT ipcWarehouseID,
        INPUT ipcLocationID
        ).

    ASSIGN
        fiLocation:SCREEN-VALUE      = ipcWarehouseID +
                                       FILL(" ", 5 - LENGTH(ipcWarehouseID)) +
                                       ipcLocationID
        cbWarehouse:SCREEN-VALUE     = ipcWarehouseID
        fiBin:SCREEN-VALUE           = ipcLocationID
        .
   
   RUN pDisbaleContext.     
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
          
    FIND FIRST inventoryStockSnapshot NO-LOCK
         WHERE inventoryStockSnapshot.company      EQ ipcCompany
           AND inventoryStockSnapshot.stockIDAlias EQ ipcTag  NO-ERROR.
           
    FIND FIRST ttPhysicalBrowseInventory EXCLUSIVE-LOCK
         WHERE ttPhysicalBrowseInventory.company         EQ ipcCompany
           AND ttPhysicalBrowseInventory.stockIDAlias    EQ ipcTag NO-ERROR.

    IF NOT AVAILABLE inventoryStockSnapshot THEN DO:
        FIND FIRST inventoryStockAlias NO-LOCK
             WHERE inventoryStockAlias.company      EQ ipcCompany
               AND inventoryStockAlias.stockIDAlias EQ ipcTag NO-ERROR.
        IF AVAILABLE inventoryStockAlias THEN
            FIND FIRST inventoryStockSnapshot NO-LOCK
                 WHERE inventoryStockSnapshot.inventoryStockID EQ inventoryStockAlias.inventoryStockID
                 NO-ERROR.             
    END.
    
    IF AVAILABLE inventoryStockSnapshot THEN DO:
        IF NOT AVAILABLE ttPhysicalBrowseInventory THEN DO:
            CREATE ttPhysicalBrowseInventory.
            ASSIGN
                ttPhysicalBrowseInventory.company          = inventoryStockSnapshot.company
                ttPhysicalBrowseInventory.inventoryStockID = inventoryStockSnapshot.inventoryStockID
                ttPhysicalBrowseInventory.stockIDAlias     = inventoryStockSnapshot.stockIDAlias
                ttPhysicalBrowseInventory.itemType         = inventoryStockSnapshot.itemType
                ttPhysicalBrowseInventory.itemID           = IF inventoryStockSnapshot.fgItemID NE "" THEN
                                                                 inventoryStockSnapshot.fgItemID
                                                             ELSE IF inventoryStockSnapshot.rmItemID NE "" THEN
                                                                 inventoryStockSnapshot.rmItemID
                                                             ELSE
                                                                 inventoryStockSnapshot.wipItemID
                ttPhysicalBrowseInventory.quantity         = inventoryStockSnapshot.quantity
                ttPhysicalBrowseInventory.customerID       = inventoryStockSnapshot.customerID
                .
        END.             

    END. 
    ELSE DO:
        
        FIND FIRST loadtag NO-LOCK
             WHERE loadtag.tag-no = ipcTag NO-ERROR.
        IF NOT AVAILABLE loadtag THEN DO:
            MESSAGE "Invalid Tag" VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
        
        CREATE ttPhysicalBrowseInventory.
        ASSIGN            
            ttPhysicalBrowseInventory.company          = loadtag.company
            ttPhysicalBrowseInventory.stockIDAlias     = ipcTag
            ttPhysicalBrowseInventory.quantity         = 0
            ttPhysicalBrowseInventory.itemID           = loadtag.i-no            
            .
    
    END.

    ASSIGN
        ttPhysicalBrowseInventory.lastTransTime   = NOW
        ttPhysicalBrowseInventory.locationID      = ipcLocationID
        ttPhysicalBrowseInventory.warehouseID     = ipcWarehouseID
        ttPhysicalBrowseInventory.location        = ipcWarehouseID +
                                                    FILL(" ", 5 - LENGTH(ipcWarehouseID)) +
                                                    ipcLocationID
        ttPhysicalBrowseInventory.inventoryStatus = DYNAMIC-FUNCTION (
                                                    "fGetSnapshotCompareStatus" IN hdInventoryProcs, 
                                                    ipcCompany,
                                                    ttPhysicalBrowseInventory.stockIDAlias,
                                                    ttPhysicalBrowseInventory.quantity,
                                                    ipcWarehouseID,
                                                    ipcLocationID
                                                    ).
            
    fiItemno:SCREEN-VALUE        = ttPhysicalBrowseInventory.itemID.
                
    RUN CreateTransactionCompare IN hdInventoryProcs (
        ipcCompany,
        ipcTag,
        ttPhysicalBrowseInventory.quantity,
        "",
        ipcWarehouseID,
        ipcLocationID,
        FALSE, /* Post transaction */
        OUTPUT lCreated,
        OUTPUT cMessage
        ).
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

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    fiItemno:SCREEN-VALUE     = "".
            
    RUN pSubmitScan (
        INPUT ipcCompany,
        INPUT cbWarehouse:SCREEN-VALUE,
        INPUT fiBin:SCREEN-VALUE,
        INPUT ipcTag
        ).
                
    {&OPEN-BROWSERS-IN-QUERY-F-Main}

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

