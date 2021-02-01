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

  File: tag-transfer.w

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
        ipcComapny  : Company Code
        ipcLocation : Location

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
DEFINE VARIABLE hdInventoryProcs    AS HANDLE    NO-UNDO.
DEFINE VARIABLE cMessage            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWarehouseListItems AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocationID         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWarehouseID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCreated            AS LOGICAL   NO-UNDO.

{system/sysconst.i}
{Inventory/ttInventory.i "NEW SHARED"}
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
&Scoped-define QUERY-STRING-br-table FOR EACH ttBrowseInventory BY ttBrowseInventory.lastTransTime DESCENDING
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory BY ttBrowseInventory.lastTransTime DESCENDING.
&Scoped-define TABLES-IN-QUERY-br-table ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ttBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btLocationLookup btKeyboard-1 RECT-27 fiTag ~
fiLocation btSubmit fiBin cbWarehouse br-table btKeyboard-2 btKeyboard-3 ~
btExit btFirst btLast btNext btPrevious btnNumPad 
&Scoped-Define DISPLAYED-OBJECTS fiTag fiItemType fiID fiLocation fiLastOp ~
fiBin cbWarehouse fiMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29.

DEFINE BUTTON btFirst 
     IMAGE-UP FILE "Graphics/32x32/navigate_up2.ico":U NO-FOCUS
     LABEL "First" 
     SIZE 9.6 BY 2.29 TOOLTIP "First".

DEFINE BUTTON btKeyboard-1 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btKeyboard-2 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btKeyboard-3 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btLast 
     IMAGE-UP FILE "Graphics/32x32/navigate_down2.ico":U NO-FOCUS
     LABEL "Last" 
     SIZE 9.6 BY 2.29 TOOLTIP "Last".

DEFINE BUTTON btLocationLookup 
     IMAGE-UP FILE "Graphics/32x32/binocular2.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Location Lookup" 
     SIZE 7.6 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btNext 
     IMAGE-UP FILE "Graphics/32x32/navigate_down.ico":U NO-FOCUS
     LABEL "Next" 
     SIZE 9.6 BY 2.29 TOOLTIP "Next".

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btPost 
     LABEL "Post" 
     SIZE 40 BY 2.38
     FONT 37.

DEFINE BUTTON btPrevious 
     IMAGE-UP FILE "Graphics/32x32/navigate_up.ico":U NO-FOCUS
     LABEL "Previous" 
     SIZE 9.6 BY 2.29 TOOLTIP "Previous".

DEFINE BUTTON btSubmit 
     LABEL "Submit" 
     SIZE 24.2 BY 1.67
     FONT 37.

DEFINE VARIABLE cbWarehouse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 20.8 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiBin AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29.2 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiID AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50.8 BY 1
     BGCOLOR 15 FONT 35 NO-UNDO.

DEFINE VARIABLE fiItemType AS CHARACTER FORMAT "X(256)":U INITIAL "WP ID:" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15 FONT 35 NO-UNDO.

DEFINE VARIABLE fiLastOp AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 15 FONT 35 NO-UNDO.

DEFINE VARIABLE fiLocation AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 71 BY 1
     FGCOLOR 12 FONT 35 NO-UNDO.

DEFINE VARIABLE fiTag AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1.38
     FONT 37 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 202 BY 32.86
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 72 BY 3.33.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 199 BY .1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      ttBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table W-Win _FREEFORM
  QUERY br-table DISPLAY
      ttBrowseInventory.quantity WIDTH 25 COLUMN-LABEL "Qty On-hand"
      ttBrowseInventory.quantityOriginal WIDTH 25 COLUMN-LABEL "Qty Original"      
      ttBrowseInventory.locationID WIDTH 30 COLUMN-LABEL "Location" FORMAT "X(12)"
      ttBrowseInventory.tag WIDTH 50 COLUMN-LABEL "Tag #" FORMAT "X(30)"
      ttBrowseInventory.jobID WIDTH 25 COLUMN-LABEL "Job #" FORMAT "X(20)"
      ttBrowseInventory.inventoryStatus COLUMN-LABEL "Status" FORMAT "X(15)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 189 BY 22.91
         FONT 36 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btLocationLookup AT ROW 3.76 COL 78.4 WIDGET-ID 178
     btKeyboard-1 AT ROW 2 COL 86 WIDGET-ID 142
     fiTag AT ROW 2.1 COL 19.2 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     fiItemType AT ROW 2.76 COL 121.2 NO-LABEL WIDGET-ID 162
     fiID AT ROW 2.76 COL 132 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     fiLocation AT ROW 3.86 COL 19.2 COLON-ALIGNED NO-LABEL WIDGET-ID 158
     fiLastOp AT ROW 3.91 COL 141.8 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     btSubmit AT ROW 5.48 COL 90.8 WIDGET-ID 174
     fiBin AT ROW 5.62 COL 49.2 COLON-ALIGNED NO-LABEL WIDGET-ID 168
     cbWarehouse AT ROW 5.67 COL 19.2 COLON-ALIGNED NO-LABEL WIDGET-ID 164
     fiMessage AT ROW 5.76 COL 118 COLON-ALIGNED NO-LABEL WIDGET-ID 176
     br-table AT ROW 7.67 COL 2 WIDGET-ID 200
     btPost AT ROW 31.05 COL 151 WIDGET-ID 38
     btKeyboard-2 AT ROW 3.76 COL 70.2 WIDGET-ID 156
     btKeyboard-3 AT ROW 5.52 COL 82.4 WIDGET-ID 172
     btExit AT ROW 2 COL 192 WIDGET-ID 84
     btFirst AT ROW 7.62 COL 192 WIDGET-ID 128
     btLast AT ROW 28.29 COL 192 WIDGET-ID 130
     btNext AT ROW 23.91 COL 192.2 WIDGET-ID 132
     btPrevious AT ROW 11.67 COL 192.2 WIDGET-ID 134
     btnNumPad AT ROW 2.05 COL 98 WIDGET-ID 138
     "Tag:" VIEW-AS TEXT
          SIZE 6.8 BY 1.19 AT ROW 2.24 COL 13.4 WIDGET-ID 22
          BGCOLOR 15 FGCOLOR 1 FONT 36
     "Warehouse:" VIEW-AS TEXT
          SIZE 18.2 BY 1.19 AT ROW 5.71 COL 2.8 WIDGET-ID 166
          BGCOLOR 15 FGCOLOR 1 FONT 36
     "Location:" VIEW-AS TEXT
          SIZE 14.2 BY 1.19 AT ROW 3.95 COL 7 WIDGET-ID 160
          BGCOLOR 15 FGCOLOR 1 FONT 36
     "Last Operation:" VIEW-AS TEXT
          SIZE 21 BY .81 AT ROW 4 COL 121.2 WIDGET-ID 70
          BGCOLOR 15 FONT 35
     "Bin:" VIEW-AS TEXT
          SIZE 7 BY 1.19 AT ROW 5.67 COL 44.2 WIDGET-ID 170
          BGCOLOR 15 FGCOLOR 1 FONT 36
     "Job Details" VIEW-AS TEXT
          SIZE 15.8 BY .62 AT ROW 1.71 COL 123.4 WIDGET-ID 16
          BGCOLOR 15 FONT 35
     RECT-2 AT ROW 1.81 COL 97 WIDGET-ID 140
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 126
     RECT-25 AT ROW 2 COL 119.2 WIDGET-ID 14
     RECT-27 AT ROW 7.33 COL 2.2 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202 BY 32.86
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
         TITLE              = "Transfer Tag"
         HEIGHT             = 32.86
         WIDTH              = 202
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
       btKeyboard-1:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btKeyboard-2:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btKeyboard-3:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btLocationLookup:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btPost IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btPost:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiID IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiItemType IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiLastOp IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiMessage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-25 IN FRAME F-Main
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
BY ttBrowseInventory.lastTransTime DESCENDING.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Transfer Tag */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Transfer Tag */
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


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit W-Win
ON CHOOSE OF btExit IN FRAME F-Main
DO:
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


&Scoped-define SELF-NAME btKeyboard-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btKeyboard-1 W-Win
ON CHOOSE OF btKeyboard-1 IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiTag.
    RUN pKeyboard (fiTag:HANDLE, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btKeyboard-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btKeyboard-2 W-Win
ON CHOOSE OF btKeyboard-2 IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiLocation.
    RUN pKeyboard (fiLocation:HANDLE, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btKeyboard-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btKeyboard-3 W-Win
ON CHOOSE OF btKeyboard-3 IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiBin.
    RUN pKeyboard (fiBin:HANDLE, "Qwerty").
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


&Scoped-define SELF-NAME btLocationLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLocationLookup W-Win
ON CHOOSE OF btLocationLookup IN FRAME F-Main /* Location Lookup */
DO:
    APPLY "HELP":U TO fiLocation.
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


&Scoped-define SELF-NAME btPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrevious W-Win
ON CHOOSE OF btPrevious IN FRAME F-Main /* Previous */
DO:
    RUN pNavigate (SELF).
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
    RUN pKeyboard (SELF, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBin W-Win
ON LEAVE OF fiBin IN FRAME F-Main
DO:
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.

    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
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
    
    fiTag:HANDLE:MOVE-AFTER-TAB-ITEM(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON HELP OF fiLocation IN FRAME F-Main
DO:
    DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

    RUN system/openlookup.p (
        ipcCompany, 
        "loc",  /* Job No lookup ID */
        0, "", 0,
        OUTPUT cFieldsValue, 
        OUTPUT cFoundValue, 
        OUTPUT recFoundRecID
        ).
    
    IF cFoundValue NE "" THEN DO:
        SELF:SCREEN-VALUE = ENTRY(2,cFieldsValue,"|") +
                            FILL(" ", 5 - LENGTH(ENTRY(2,cFieldsValue,"|"))) +
                            ENTRY(3,cFieldsValue,"|").
        APPLY "LEAVE" TO SELF.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON LEAVE OF fiLocation IN FRAME F-Main
DO:
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.

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
    RUN pKeyboard (SELF, "Qwerty").
    
    fiLocation:HANDLE:MOVE-AFTER-TAB-ITEM(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON LEAVE OF fiTag IN FRAME F-Main
DO:
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.

    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
    
    RUN pTagScan (
        ipcCompany,
        SELF:SCREEN-VALUE
        ).        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-table
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

{wip/pNavigate.i}
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
  DISPLAY fiTag fiItemType fiID fiLocation fiLastOp fiBin cbWarehouse fiMessage 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btLocationLookup btKeyboard-1 RECT-27 fiTag fiLocation btSubmit fiBin 
         cbWarehouse br-table btKeyboard-2 btKeyboard-3 btExit btFirst btLast 
         btNext btPrevious btnNumPad 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit W-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
           
    FIND FIRST company NO-LOCK 
         WHERE company.company EQ ipcCompany NO-ERROR .
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - {&awversion}" + " - " 
                         + STRING(company.name) + " - " + ipcLocation  .

    RUN GetWarehouseList IN hdInventoryProcs (
        "", /* Company. Pass empty if needed list of all warehouses across all companies are required */
        TRUE, /* Active location only */
        OUTPUT cWarehouseListItems
        ).
        
    cbWarehouse:LIST-ITEMS IN FRAME {&FRAME-NAME} = cWarehouseListItems.

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

    DEFINE VARIABLE lValidLoc            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidBin            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidInvStock       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lTransactionCreated  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturnMessage       AS CHARACTER NO-UNDO.
        
    DO WITH FRAME {&FRAME-NAME}:
    END.
            
    IF ipcTag EQ "" THEN DO:
        cMessage = "Blank Tag".
        RUN pUpdateMessageText (
            cMessage, /* Message Text */
            TRUE,     /* Error */
            FALSE     /* Alert-box*/
            ).        
        RETURN.
    END.
         
    IF ipcWarehouseID EQ "" THEN DO:
        cMessage = "Blank Warehouse".
        RUN pUpdateMessageText (
            cMessage, /* Message Text */
            TRUE,     /* Error */
            FALSE     /* Alert-box*/
            ).        
        RETURN.
    END.    

    IF ipcLocationID EQ "" THEN DO:
        cMessage = "Blank Bin".
        RUN pUpdateMessageText (
            cMessage, /* Message Text */
            TRUE,     /* Error */
            FALSE     /* Alert-box*/
            ).        
        RETURN.
    END.

    RUN ValidateLoc IN hdInventoryProcs (
        ipcCompany,
        ipcWarehouseID,
        OUTPUT lValidLoc
        ).
        
    IF NOT lValidLoc THEN DO:
        cMessage = "Invalid WarehouseID " + ipcWarehouseID.
        RUN pUpdateMessageText (
            cMessage, /* Message Text */
            TRUE,     /* Error */
            FALSE     /* Alert-box*/
            ).        
        RETURN.
    END.
 
    RUN ValidateBin IN hdInventoryProcs (
        ipcCompany,
        ipcWarehouseID,
        ipcLocationID,
        OUTPUT lValidBin
        ).
        
    IF NOT lValidBin THEN DO:
        cMessage = "Invalid Bin " + ipcLocationID.
        RUN pUpdateMessageText (
            cMessage, /* Message Text */
            TRUE,     /* Error */
            FALSE     /* Alert-box*/
            ).        
        RETURN.
    END.

    RUN pGetInventoryStockDetails IN hdInventoryProcs (
        ipcCompany,
        ipcTag,
        OUTPUT lValidInvStock,
        OUTPUT cReturnMessage,
        INPUT-OUTPUT TABLE ttInventoryStockDetails
        ).
    
    IF NOT lValidInvStock THEN DO:
        cMessage = "Invalid Bin " + ipcLocationID.
        RUN pUpdateMessageText (
            cReturnMessage, /* Message Text */
            TRUE,           /* Error */
            FALSE           /* Alert-box*/
            ).        
        RETURN.
    END.       
    
    FIND FIRST ttInventoryStockDetails
         WHERE ttInventoryStockDetails.tag EQ ipcTag
		 NO-ERROR.
    IF AVAILABLE ttInventoryStockDetails THEN DO:
        IF ttInventoryStockDetails.warehouseID EQ ipcWarehouseID AND
           ttInventoryStockDetails.locationID  EQ ipcLocationID THEN DO:
            cMessage = "Scanned location is same as existing".
            RUN pUpdateMessageText (
                cMessage, /* Message Text */
                TRUE,     /* Error */
                FALSE     /* Alert-box*/
                ).        
            RETURN.
        END.

        RUN CreateTransactionTransfer IN hdInventoryProcs (
            ipcCompany, 
            ipcTag, 
            ipcWarehouseID, 
            ipcLocationID, 
            "YES",  /* Post transaction */
            OUTPUT lTransactionCreated, 
            OUTPUT cReturnMessage
            ).

        IF lTransactionCreated THEN DO:
            FIND FIRST ttBrowseInventory
                 WHERE ttBrowseInventory.tag EQ ipcTag
				 NO-ERROR.
            IF NOT AVAILABLE ttBrowseInventory THEN DO:
                CREATE ttBrowseInventory.
                BUFFER-COPY ttInventoryStockDetails EXCEPT ttInventoryStockDetails.locationID TO ttBrowseInventory.
            END.
            
            ASSIGN
                ttBrowseinventory.locationID = ipcWarehouseID +
                                               FILL(" ", 5 - LENGTH(ipcWarehouseID)) +
                                               ipcLocationID
                fiLocation:SCREEN-VALUE      = ttBrowseinventory.locationID
                cbWarehouse:SCREEN-VALUE     = ipcWarehouseID
                fiBin:SCREEN-VALUE           = ipcLocationID
                cMessage                     = "Tag Transferred"
                .

            RUN pUpdateMessageText (
                cMessage, /* Message Text */
                FALSE,    /* Error */
                FALSE     /* Alert-box*/
                ).                        
        END.
    END.                  
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
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lValidInvStock     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturnMessage     AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF ipcTag EQ "" THEN DO:
        cMessage = "Empty Tag".
        RUN pUpdateMessageText (
            cMessage, /* Message Text */
            TRUE,     /* Error */
            FALSE     /* Alert-box*/
            ).        
        RETURN.
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
         WHERE ttInventoryStockDetails.tag EQ ipcTag
		 NO-ERROR.
    IF AVAILABLE ttInventoryStockDetails THEN
        ASSIGN
            cbWarehouse:SCREEN-VALUE = ttInventoryStockDetails.warehouseID
            fiBin:SCREEN-VALUE       = ttInventoryStockDetails.locationID
            fiItemType:SCREEN-VALUE  = ttInventoryStockDetails.itemType + " " + "ID:" /* Concatenate ID: to item Type */
            fiLastOp:SCREEN-VALUE    = ttInventoryStockDetails.machineID
            fiID:SCREEN-VALUE        = IF ttInventoryStockDetails.itemType EQ gcItemTypeFG THEN
                                           ttInventoryStockDetails.fgItemID
                                       ELSE IF ttInventoryStockDetails.itemType EQ gcItemTypeRM THEN
                                           ttInventoryStockDetails.rmItemID
                                       ELSE
                                           ttInventoryStockDetails.wipItemID
            fiLocation:SCREEN-VALUE  = ttInventoryStockDetails.warehouseID +
                                       FILL(" ", 5 - LENGTH(ttInventoryStockDetails.warehouseID)) +
                                       ttInventoryStockDetails.locationID
            .
            
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

