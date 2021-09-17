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

  File: sharpshooter/w-fgtrans.w

  Description: Create fg transfer/receipt transactions

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

{inventory/ttBrowseInventory.i}
{jc/jcgl-sh.i  NEW}
{fg/fg-post3.i NEW}

DEFINE VARIABLE oLoadTag         AS Inventory.Loadtag NO-UNDO.
DEFINE VARIABLE oFGBin           AS fg.FGBin          NO-UNDO.
DEFINE VARIABLE oItemFG          AS fg.ItemFG         NO-UNDO.
DEFINE VARIABLE oKeyboard        AS system.Keyboard   NO-UNDO.

DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cTag             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iWarehouseLength AS INTEGER   NO-UNDO.

DEFINE VARIABLE gcLocationSource AS CHARACTER NO-UNDO INITIAL "LoadTag".
DEFINE VARIABLE glCloseJob       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glAutoPost       AS LOGICAL   NO-UNDO INITIAL TRUE.

ASSIGN
    oLoadTag  = NEW Inventory.LoadTag()
    oKeyboard = NEW system.Keyboard()
    oFGBin   = NEW fg.FGBin()
    oItemFG  = NEW fg.ItemFG()
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttBrowseInventory

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 ttBrowseInventory.fgItemID fGetConcatLocationID() @ ttBrowseInventory.warehouseID ttBrowseInventory.tag ttBrowseInventory.quantity fGetInventoryStatus() @ ttBrowseInventory.inventoryStatus ttBrowseInventory.emptyColumn   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH ttBrowseInventory
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 ttBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnKeyboardlOCATION fiTag btnKeyboardTAG ~
btReset fiLocation btnNumPad btClearRecords btnFirst BROWSE-1 btnLast ~
btnNext btnPrevious btExit btnExitText 
&Scoped-Define DISPLAYED-OBJECTS fiTag fiLocation fiMessage btnExitText ~
statusMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetConcatLocationID W-Win 
FUNCTION fGetConcatLocationID RETURNS CHARACTER PRIVATE
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetInventoryStatus W-Win 
FUNCTION fGetInventoryStatus RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btClearRecords 
     LABEL "CLEAR RECORDS" 
     SIZE 31 BY 1.38 TOOLTIP "Clear all the scanned data from grid".

DEFINE BUTTON btExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Exit" 
     SIZE 8 BY 1.91 TOOLTIP "Exit".

DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "Graphics/32x32/first.png":U NO-FOCUS FLAT-BUTTON
     LABEL "First" 
     SIZE 8 BY 1.91 TOOLTIP "First".

DEFINE BUTTON btnKeyboardlOCATION 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnKeyboardTAG 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "Graphics/32x32/last.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Last" 
     SIZE 8 BY 1.91 TOOLTIP "Last".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "Graphics/32x32/next.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Next" 
     SIZE 8 BY 1.91 TOOLTIP "Next".

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnPrevious 
     IMAGE-UP FILE "Graphics/32x32/previous.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Prev" 
     SIZE 8 BY 1.91 TOOLTIP "Previous".

DEFINE BUTTON btPost 
     LABEL "POST" 
     SIZE 19 BY 1.43.

DEFINE BUTTON btReset 
     LABEL "RESET" 
     SIZE 20 BY 1.48.

DEFINE BUTTON btTransfer 
     LABEL "TRANSFER" 
     SIZE 20 BY 1.43.

DEFINE VARIABLE btnExitText AS CHARACTER FORMAT "X(256)":U INITIAL "EXIT" 
      VIEW-AS TEXT 
     SIZE 8 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE fiLocation AS CHARACTER FORMAT "X(256)":U 
     LABEL "LOCATION" 
     VIEW-AS FILL-IN 
     SIZE 71 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 89 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiTag AS CHARACTER FORMAT "X(256)":U 
     LABEL "TAG" 
     VIEW-AS FILL-IN 
     SIZE 71 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE statusMessage AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS MESSAGE" 
      VIEW-AS TEXT 
     SIZE 116 BY 1.43 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      ttBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 W-Win _FREEFORM
  QUERY BROWSE-1 DISPLAY
      ttBrowseInventory.fgItemID WIDTH 40 COLUMN-LABEL "Item #" FORMAT "X(30)"
      fGetConcatLocationID() @ ttBrowseInventory.warehouseID WIDTH 30 COLUMN-LABEL "Location" FORMAT "X(12)"
      ttBrowseInventory.tag WIDTH 50 COLUMN-LABEL "Tag #" FORMAT "X(30)"
      ttBrowseInventory.quantity WIDTH 25 COLUMN-LABEL "Quantity" FORMAT "->,>>>,>>9.99<<<<"
      fGetInventoryStatus() @ ttBrowseInventory.inventoryStatus COLUMN-LABEL "Status" FORMAT "X(30)"
      ttBrowseInventory.emptyColumn COLUMN-LABEL ""
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-SCROLLBAR-VERTICAL NO-TAB-STOP SIZE 180 BY 20.95
         BGCOLOR 15 FGCOLOR 0 FONT 36 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnKeyboardlOCATION AT ROW 4.33 COL 93 WIDGET-ID 138 NO-TAB-STOP 
     fiTag AT ROW 2.67 COL 19 COLON-ALIGNED WIDGET-ID 4
     btnKeyboardTAG AT ROW 2.67 COL 93 WIDGET-ID 144 NO-TAB-STOP 
     btReset AT ROW 2.67 COL 100 WIDGET-ID 32 NO-TAB-STOP 
     fiLocation AT ROW 4.33 COL 19 COLON-ALIGNED WIDGET-ID 6
     btnNumPad AT ROW 2.86 COL 148 WIDGET-ID 120 NO-TAB-STOP 
     btTransfer AT ROW 4.33 COL 100 WIDGET-ID 8
     fiMessage AT ROW 6 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     btPost AT ROW 6 COL 100 WIDGET-ID 20 NO-TAB-STOP 
     btClearRecords AT ROW 6 COL 137 WIDGET-ID 28 NO-TAB-STOP 
     btnFirst AT ROW 8.38 COL 182 WIDGET-ID 44
     BROWSE-1 AT ROW 7.67 COL 2 WIDGET-ID 200
     btnLast AT ROW 14.1 COL 182 WIDGET-ID 46
     btnNext AT ROW 12.19 COL 182 WIDGET-ID 42
     btnPrevious AT ROW 10.29 COL 182 WIDGET-ID 40
     btExit AT ROW 1 COL 182 WIDGET-ID 26 NO-TAB-STOP 
     btnExitText AT ROW 1.24 COL 174 NO-LABEL WIDGET-ID 36
     statusMessage AT ROW 28.86 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     RECT-2 AT ROW 2.67 COL 147 WIDGET-ID 130
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 189.6 BY 29.62
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
         TITLE              = "Finished Goods Receive/Transfer"
         HEIGHT             = 29.62
         WIDTH              = 189.2
         MAX-HEIGHT         = 29.62
         MAX-WIDTH          = 212.6
         VIRTUAL-HEIGHT     = 29.62
         VIRTUAL-WIDTH      = 212.6
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = yes
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
/* BROWSE-TAB BROWSE-1 btnFirst F-Main */
/* SETTINGS FOR FILL-IN btnExitText IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       btnKeyboardlOCATION:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboardTAG:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btPost IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btPost:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btTransfer IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btTransfer:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiMessage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN statusMessage IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Finished Goods Receive/Transfer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Finished Goods Receive/Transfer */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE OBJECT hdInventoryProcs.

    IF VALID-OBJECT(oKeyboard) THEN
        DELETE OBJECT oKeyboard.
    
    APPLY "CLOSE":U TO THIS-PROCEDURE.
   
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btClearRecords
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btClearRecords W-Win
ON CHOOSE OF btClearRecords IN FRAME F-Main /* CLEAR RECORDS */
DO:
    EMPTY TEMP-TABLE ttBrowseInventory.
    
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit W-Win
ON CHOOSE OF btExit IN FRAME F-Main /* Exit */
DO:
    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE OBJECT hdInventoryProcs.
    
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    
    RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExitText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExitText W-Win
ON MOUSE-SELECT-CLICK OF btnExitText IN FRAME F-Main
DO:
    APPLY "CHOOSE":U TO btExit.
    RETURN NO-APPLY.
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


&Scoped-define SELF-NAME btnKeyboardlOCATION
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardlOCATION W-Win
ON CHOOSE OF btnKeyboardlOCATION IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiLocation.    
    oKeyboard:OpenKeyboard (fiLocation:HANDLE, "Qwerty"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboardTAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardTAG W-Win
ON CHOOSE OF btnKeyboardTAG IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiTag.    
    oKeyboard:OpenKeyboard (fiTag:HANDLE, "Qwerty"). 
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
        oKeyboard:DisplayKeyboard = NOT oKeyboard:DisplayKeyboard
        RECT-2:BGCOLOR = IF oKeyboard:DisplayKeyboard THEN 10 ELSE 12
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevious W-Win
ON CHOOSE OF btnPrevious IN FRAME F-Main /* Prev */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReset W-Win
ON CHOOSE OF btReset IN FRAME F-Main /* RESET */
DO:
    fiTag:SCREEN-VALUE = "".
    
    APPLY "ENTRY" TO fiTag.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON ENTRY OF fiLocation IN FRAME F-Main /* LOCATION */
DO:
    oKeyboard:OpenKeyboard (SELF, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON LEAVE OF fiLocation IN FRAME F-Main /* LOCATION */
DO:
    DEFINE VARIABLE lError     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWarehouse AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocation  AS CHARACTER NO-UNDO.
    
    IF TRIM(fiTag:SCREEN-VALUE) EQ "" OR LASTKEY EQ -1 THEN
        RETURN.
    
    ASSIGN
        cWarehouse = TRIM(SUBSTRING(SELF:SCREEN-VALUE, 1, iWarehouseLength))
        cLocation  = TRIM(SUBSTRING(SELF:SCREEN-VALUE, iWarehouseLength + 1))
        .
    
    IF cWarehouse EQ "" THEN DO:
        RUN pStatusMessage ("WAREHOUSE CANNOT BE EMPTY", 3).
        RETURN.  
    END.

    IF cLocation EQ "" THEN DO:
        RUN pStatusMessage ("LOCATION CANNOT BE EMPTY", 3).
        RETURN. 
    END.
        
    RUN pLocationScan (
        INPUT  fiTag:SCREEN-VALUE,
        INPUT  cWarehouse,
        INPUT  cLocation,
        OUTPUT lError,
        OUTPUT cMessage
        ). 
    IF lError THEN
        RUN pStatusMessage (CAPS(cMessage), 3).
    ELSE
        ASSIGN
            SELF:SCREEN-VALUE  = ""
            fiTag:SCREEN-VALUE = ""
            .
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON ENTRY OF fiTag IN FRAME F-Main /* TAG */
DO:
    ASSIGN
        btTransfer:VISIBLE     = FALSE
        fiMessage:SCREEN-VALUE = ""
        .  
    oKeyboard:OpenKeyboard (SELF, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON LEAVE OF fiTag IN FRAME F-Main /* TAG */
DO:
    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lIsTransfer AS LOGICAL   NO-UNDO.
    
    IF SELF:SCREEN-VALUE EQ "" OR LASTKEY EQ -1 THEN
        RETURN.
        
    RUN pTagScan (
        INPUT  SELF:SCREEN-VALUE,
        OUTPUT lIsTransfer,
        OUTPUT lError,
        OUTPUT cMessage
        ).

    {&OPEN-QUERY-{&BROWSE-NAME}}
      
    IF lError THEN DO:
        RUN pStatusMessage (CAPS(cMessage), 3).
        
        RETURN NO-APPLY.
    END.
    
    IF NOT lIsTransfer THEN DO:
        SELF:SCREEN-VALUE = "".
        
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

{sharpshooter/pStatusMessage.i}

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
  DISPLAY fiTag fiLocation fiMessage btnExitText statusMessage 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btnKeyboardlOCATION fiTag btnKeyboardTAG btReset fiLocation btnNumPad 
         btClearRecords btnFirst BROWSE-1 btnLast btnNext btnPrevious btExit 
         btnExitText 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
 Purpose:
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
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN pWinReSize.
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.    
    RUN spGetSessionParam ("Company", OUTPUT cCompany).    
    RUN Inventory_GetWarehouseLength IN hdInventoryProcs (
        INPUT  cCompany,
        OUTPUT iWarehouseLength
        ).
    RUN pStatusMessage ("", 0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationScan W-Win 
PROCEDURE pLocationScan PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcTag       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouse AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE riFGRctd AS ROWID     NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cItemID  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iQuantity         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSubUnits         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSubUnitsPerUnit  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPartial          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCurrentWarehouse AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCurrentLocation  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lLocationConfirm  AS LOGICAL   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.    

    IF TRIM(ipcTag) EQ "" THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Scanned tag is empty"
            .
        RETURN.
    END.
    
    oLoadTag:SetContext (cCompany, FALSE /* ItemType */, ipcTag).

    IF NOT oLoadTag:IsAvailable() THEN DO:
        ASSIGN
            oplError = TRUE
            opcMessage = "Invalid tag '" + ipcTag + "'"
            .
        RETURN.
    END.

    cItemID = oLoadTag:GetValue("ItemID").
    
    oItemFG:SetContext(cCompany, cItemID).
    IF NOT oItemFG:IsAvailable() THEN DO:
        ASSIGN
            oplError = TRUE
            opcMessage = "Invalid item # '" + cItemID + "'"
            .
        RETURN.        
    END.
 
    oFGBin:SetContext (cCompany, cItemID, ipcTag).
    IF NOT oFGBin:IsAvailable() THEN DO:
        ASSIGN
            oplError = TRUE
            opcMessage = "FG Bin not available for tag '" + ipcTag + "'"
            .
        RETURN.
    END.
        
    ASSIGN
        iSubUnits         = INTEGER(oFGBin:GetValue("QuantityInSubUnit"))
        iSubUnitsPerUnit  = INTEGER(oFGBin:GetValue("SubUnitsPerUnit"))
        iPartial          = INTEGER(oFGBin:GetValue("Partial"))
        iQuantity         = iSubUnits * iSubUnitsPerUnit + iPartial
        cCurrentWarehouse = oFGBin:GetValue("Warehouse")
        cCurrentLocation  = oFGBin:GetValue("Location")
        .
    
    IF cCurrentWarehouse EQ ipcWarehouse AND cCurrentLocation EQ ipcLocation THEN
        ASSIGN
            lSuccess         = TRUE
            lLocationConfirm = TRUE
            .
    ELSE         
        RUN api/inbound/CreateInventoryTransfer.p (
            INPUT  cCompany, 
            INPUT  ipcWarehouse,
            INPUT  ipcLocation, 
            INPUT  ipcTag,
            INPUT  cItemID,
            INPUT  "FG",  /* Item Type */
            INPUT  USERID("ASI"), 
            INPUT  FALSE, /* Post */
            OUTPUT riFGRctd,
            OUTPUT lSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.
    
    IF lSuccess THEN DO:
        CREATE ttBrowseInventory.
        ASSIGN
            ttBrowseInventory.company          = cCompany
            ttBrowseInventory.fgItemID         = cItemID
            ttBrowseInventory.tag              = ipcTag
            ttBrowseInventory.warehouse        = ipcWarehouse
            ttBrowseInventory.location         = ipcLocation
            ttBrowseInventory.quantity         = iQuantity
            ttBrowseInventory.inventoryStockID = STRING(riFGRctd)
            ttBrowseInventory.transactionType  = "Transfer"
            ttBrowseInventory.inventoryStatus  = "Created"
            .
        
        IF lLocationConfirm THEN
            ttBrowseInventory.inventoryStatus = "Confirmed".
    END.
    ELSE DO:
        oplError = TRUE.
        RETURN.
    END.
        
    IF glAutoPost AND NOT lLocationConfirm THEN
        RUN pPost.
    ELSE
        fiMessage:SCREEN-VALUE = "Receipt Transaction created".
        
    {&OPEN-QUERY-{&BROWSE-NAME}}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNavigate W-Win 
PROCEDURE pNavigate :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphNavPanel AS HANDLE NO-UNDO.
    
    IF AVAILABLE ttBrowseInventory THEN
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPost W-Win 
PROCEDURE pPost PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    FOR EACH ttBrowseInventory NO-LOCK
        WHERE ttBrowseInventory.inventoryStatus = "Created":
        RUN PostFinishedGoodsForFGRctd IN hdInventoryProcs (
            INPUT  TO-ROWID(ttBrowseInventory.inventoryStockID),
            INPUT  glCloseJob,
            OUTPUT lError,
            OUTPUT cMessage
            ).
        IF lError THEN
            RUN pStatusMessage (CAPS(cMessage), 3).
            
        IF NOT lError AND AVAILABLE ttBrowseInventory THEN
            ASSIGN
                ttBrowseInventory.inventoryStatus = "Posted"
                fiMessage:SCREEN-VALUE            = "Transaction posted successfully"
                .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTagScan W-Win 
PROCEDURE pTagScan PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcTag        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsTransfer AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cItemID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocation        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWarehouse       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityUOM     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPOID            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPOLineID        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE riFGRctd         AS ROWID     NO-UNDO.
    DEFINE VARIABLE cJobID           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobID2          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dNewQuantity     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iQuantity        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSubUnits        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSubUnitsPerUnit AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPartial         AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:    
    END.
    
    IF TRIM(ipcTag) EQ "" THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Scanned tag is empty"
            .
        RETURN.
    END.
    
    oLoadTag:SetContext (cCompany, FALSE /* ItemType */, ipcTag).

    IF NOT oLoadTag:IsAvailable() THEN DO:
        ASSIGN
            oplError = TRUE
            opcMessage = "Invalid tag '" + ipcTag + "'"
            .
        RETURN.
    END.

    cItemID = oLoadTag:GetValue("ItemID").
    
    oItemFG:SetContext(cCompany, cItemID).
    IF NOT oItemFG:IsAvailable() THEN DO:
        ASSIGN
            oplError = TRUE
            opcMessage = "Invalid item # '" + cItemID + "'"
            .
        RETURN.        
    END.
    
    oplIsTransfer = oFGBin:SetContext (cCompany, cItemID, ipcTag).

    ASSIGN
        cWarehouse = oLoadTag:GetValue("Warehouse")
        cLocation  = oLoadTag:GetValue("Location")
        .

    IF gcLocationSource EQ "FGItem" THEN
        ASSIGN
            cWarehouse = oItemFG:GetValue("Warehouse")
            cLocation  = oItemFG:GetValue("Location")
            .
    ELSE IF gcLocationSource EQ "UserDefault" THEN DO:
        RUN Inventory_GetDefaultWhse IN hdInventoryProcs (
            INPUT  cCompany,
            OUTPUT cWarehouse
            ).

        RUN Inventory_GetDefaultBin IN hdInventoryProcs (
            INPUT  cCompany,
            OUTPUT cLocation
            ).
    END.
    
    fiLocation:SCREEN-VALUE = cWarehouse 
                            + FILL(" ", iWarehouseLength - LENGTH(cWarehouse)) 
                            + cLocation.      

    IF oplIsTransfer THEN DO:
        fiLocation:SENSITIVE = TRUE.
        
        APPLY "ENTRY" TO fiLocation.
        
        RETURN.
    END.
    /* Is Receipt */ 
    ELSE DO:
        ASSIGN
            cQuantityUOM     = oItemFG:GetValue("PurchaseUOM")
            iPOID            = INTEGER(oLoadTag:GetValue("PO"))
            iPOLineID        = INTEGER(oLoadTag:GetValue("PoLine"))
            cJobID           = oLoadTag:GetValue("JobID")
            iJobID2          = INTEGER(oLoadTag:GetValue("JobID2"))
            iSubUnits        = INTEGER(oLoadTag:GetValue("QuantityInSubUnit"))
            iSubUnitsPerUnit = INTEGER(oLoadTag:GetValue("SubUnitsPerUnit"))
            iPartial         = INTEGER(oLoadTag:GetValue("Partial"))
            iQuantity        = iSubUnits * iSubUnitsPerUnit + iPartial
            .
        
        RUN api\inbound\CreateInventoryReceipt.p (
            INPUT        cCompany, 
            INPUT        ipcTag,
            INPUT        iQuantity,  /* Quantity */
            INPUT        cQuantityUOM,
            INPUT-OUTPUT iPOID,
            INPUT        iPOLineID,
            INPUT-OUTPUT cJobID,                  
            INPUT        STRING(iJobID2),                 
            INPUT        iSubUnits,  /* Sub Units */     
            INPUT        iSubUnitsPerUnit,  /* Sub Units per Unit */
            INPUT        cWarehouse,            
            INPUT        cLocation, 
            INPUT        "no", /* Post */            
            INPUT        USERID("ASI"),
            OUTPUT       riFGRctd,
            OUTPUT       dNewQuantity,
            OUTPUT       lSuccess,
            OUTPUT       opcMessage
            )NO-ERROR.
        
        IF NOT lSuccess THEN DO:
            oplError = TRUE.
            RETURN.
        END.
        
        CREATE ttBrowseInventory.
        ASSIGN
            ttBrowseInventory.company          = cCompany
            ttBrowseInventory.fgItemID         = cItemID
            ttBrowseInventory.tag              = ipcTag
            ttBrowseInventory.warehouse        = cWarehouse
            ttBrowseInventory.location         = cLocation
            ttBrowseInventory.quantity         = iQuantity
            ttBrowseInventory.inventoryStockID = STRING(riFGRctd)
            ttBrowseInventory.transactionType  = "Receipt"
            ttBrowseInventory.inventoryStatus  = "Created"
            .
    END.
    
    IF glAutoPost THEN
        RUN pPost.
    ELSE
        fiMessage:SCREEN-VALUE = "Receipt Transaction created".
        
    {&OPEN-QUERY-{&BROWSE-NAME}}
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
    DEFINE VARIABLE dRow    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dHeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWidth  AS DECIMAL NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            {&WINDOW-NAME}:ROW                 = 1
            {&WINDOW-NAME}:COL                 = 1
            {&WINDOW-NAME}:VIRTUAL-HEIGHT      = SESSION:HEIGHT - 1
            {&WINDOW-NAME}:VIRTUAL-WIDTH       = SESSION:WIDTH  - 1
            {&WINDOW-NAME}:HEIGHT              = {&WINDOW-NAME}:VIRTUAL-HEIGHT
            {&WINDOW-NAME}:WIDTH               = {&WINDOW-NAME}:VIRTUAL-WIDTH
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH          = {&WINDOW-NAME}:WIDTH
            statusMessage:ROW                  = {&WINDOW-NAME}:HEIGHT - .86
            dCol                               = {&WINDOW-NAME}:WIDTH  - 8
            btnExitText:COL                    = dCol - 9
            btExit:COL                         = dCol
            btnFirst:COL                       = dCol
            btnPrevious:COL                    = dCol
            btnNext:COL                        = dCol
            btnLast:COL                        = dCol
            BROWSE {&BROWSE-NAME}:HEIGHT       = {&WINDOW-NAME}:HEIGHT - BROWSE {&BROWSE-NAME}:ROW - 1
            BROWSE {&BROWSE-NAME}:WIDTH        = dCol - 2
            .
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus W-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    APPLY "ENTRY" TO fiTag.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetConcatLocationID W-Win 
FUNCTION fGetConcatLocationID RETURNS CHARACTER PRIVATE
  ( ) :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetInventoryStatus W-Win 
FUNCTION fGetInventoryStatus RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN ttBrowseInventory.transactionType + " - " + ttBrowseInventory.inventoryStatus.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

