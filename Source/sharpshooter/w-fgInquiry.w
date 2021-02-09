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
{system/sysconst.i}
{wip/keyboardDefs.i}
{inventory/ttInventory.i "NEW SHARED"}

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
&Scoped-Define ENABLED-OBJECTS rdView fiFGItem fiCustItem fiLocation ~
btItemHelp btnKeyboardItem btnKeyboardLoc btnNumPad btItemNameHelp ~
btnKeyboardItemName btLocHelp RECT-33 RECT-34 
&Scoped-Define DISPLAYED-OBJECTS rdView fiFGItem fiCustItem fiLocation ~
fiSellUOM 

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

/* Definitions of the field level widgets                               */
DEFINE BUTTON btItemHelp 
     IMAGE-UP FILE "Graphics/32x32/magnifying_glass.ico":U
     LABEL "" 
     SIZE 6.6 BY 1.57.

DEFINE BUTTON btItemNameHelp 
     IMAGE-UP FILE "Graphics/32x32/magnifying_glass.ico":U
     LABEL "" 
     SIZE 6.6 BY 1.52.

DEFINE BUTTON btLocHelp 
     IMAGE-UP FILE "Graphics/32x32/magnifying_glass.ico":U
     LABEL "" 
     SIZE 6.6 BY 1.57.

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

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE VARIABLE fiCustItem AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer Part #" 
     VIEW-AS FILL-IN 
     SIZE 49.6 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiFGItem AS CHARACTER FORMAT "X(256)":U 
     LABEL "FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 49.6 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiLocation AS CHARACTER FORMAT "X(256)":U 
     LABEL "Location" 
     VIEW-AS FILL-IN 
     SIZE 32.2 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiSellUOM AS CHARACTER FORMAT "X(256)":U 
     LABEL "UOM" 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1.43 NO-UNDO.

DEFINE VARIABLE rdView AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Detail", 1,
"Warehouse", 2,
"Bin Location", 3
     SIZE 63 BY 1.33 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 197.8 BY .1.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 65.8 BY 1.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rdView AT ROW 3.76 COL 96.6 NO-LABEL WIDGET-ID 160 NO-TAB-STOP 
     fiFGItem AT ROW 1.38 COL 25.4 COLON-ALIGNED WIDGET-ID 2
     fiCustItem AT ROW 3.62 COL 25.4 COLON-ALIGNED WIDGET-ID 4
     fiLocation AT ROW 1.38 COL 106.4 COLON-ALIGNED WIDGET-ID 142
     btItemHelp AT ROW 1.33 COL 77.6 WIDGET-ID 138 NO-TAB-STOP 
     btnKeyboardItem AT ROW 1.33 COL 85 WIDGET-ID 136 NO-TAB-STOP 
     btnKeyboardLoc AT ROW 1.33 COL 149 WIDGET-ID 144 NO-TAB-STOP 
     btnNumPad AT ROW 1.57 COL 178.6 WIDGET-ID 120 NO-TAB-STOP 
     btItemNameHelp AT ROW 3.57 COL 77.6 WIDGET-ID 140 NO-TAB-STOP 
     btnKeyboardItemName AT ROW 3.57 COL 85.2 WIDGET-ID 134 NO-TAB-STOP 
     btLocHelp AT ROW 1.33 COL 141.6 WIDGET-ID 146 NO-TAB-STOP 
     fiSellUOM AT ROW 1.48 COL 164.2 COLON-ALIGNED WIDGET-ID 148 NO-TAB-STOP 
     "View Inventory By" VIEW-AS TEXT
          SIZE 21 BY .67 AT ROW 3.14 COL 97.8 WIDGET-ID 154
          FONT 6
     RECT-33 AT ROW 5.62 COL 2.6 WIDGET-ID 6
     RECT-34 AT ROW 3.38 COL 95 WIDGET-ID 152
     RECT-2 AT ROW 1.38 COL 177.6 WIDGET-ID 130
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
   Design Page: 1
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
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.57
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       btnKeyboardItem:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboardItemName:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboardLoc:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiSellUOM IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
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
    
    IF VALID-HANDLE(hdFGProcs) THEN
        DELETE PROCEDURE hdFGProcs.
        
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
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


&Scoped-define SELF-NAME fiCustItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustItem W-Win
ON ANY-KEY OF fiCustItem IN FRAME F-Main /* Customer Part # */
DO:
    /* Apply fg item scan on press enter key */
    IF KEY-LABEL(LASTKEY) EQ "ENTER" THEN
        APPLY "LEAVE" TO SELF.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
    DEFINE VARIABLE lHasMutlipleItems AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cItemSelected     AS CHARACTER NO-UNDO.

    RUN pClearRecords.
    
    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    ASSIGN        
        cCustItem             = SELF:SCREEN-VALUE
        fiFGItem:SCREEN-VALUE = ""
        cItemID               = ""
        .

    RUN FG_HasMultipleFGItemsForCustPart IN hdFGProcs (
        INPUT  cCompany,
        INPUT  cCustItem,
        OUTPUT lHasMutlipleItems
        ).
    IF lHasMutlipleItems THEN DO:
        RUN fg/d-fgItemPrompt.w (
            INPUT  cCompany,
            INPUT  cCustItem,
            OUTPUT cItemSelected
            ).
        
        IF cItemSelected EQ "" THEN DO:
            MESSAGE "Please select a valid item for cust part # " + cCustItem
            VIEW-AS ALERT-BOX ERROR. 
            
            cCustItem = "".
            
            RETURN.
        END.
        
        cItemID = cItemSelected.
    END.
    
    RUN pScanItem.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFGItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFGItem W-Win
ON ANY-KEY OF fiFGItem IN FRAME F-Main /* FG Item# */
DO:
    /* Apply fg item scan on press enter key */
    IF KEY-LABEL(LASTKEY) EQ "ENTER" THEN
        APPLY "LEAVE" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
        
    RUN pClearRecords.
    
    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    ASSIGN        
        cItemID                 = SELF:SCREEN-VALUE
        fiCustItem:SCREEN-VALUE = ""
        cCustItem               = ""
        .
    
    RUN pScanItem.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON ANY-KEY OF fiLocation IN FRAME F-Main /* Location */
DO:
    /* Apply location scan on press enter key */
    IF KEY-LABEL(LASTKEY) EQ "ENTER" THEN
        APPLY "LEAVE" TO SELF.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON ENTRY OF fiLocation IN FRAME F-Main /* Location */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON HELP OF fiLocation IN FRAME F-Main /* Location */
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
    END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON LEAVE OF fiLocation IN FRAME F-Main /* Location */
DO:    
    IF cWarehouse EQ TRIM(SUBSTRING(fiLocation:SCREEN-VALUE, 1, iWarehouseLength)) AND cLocation EQ TRIM(SUBSTRING(fiLocation:SCREEN-VALUE, iWarehouseLength + 1)) THEN
        RETURN.
        
    ASSIGN
        cWarehouse = TRIM(SUBSTRING(fiLocation:SCREEN-VALUE, 1, iWarehouseLength))
        cLocation  = TRIM(SUBSTRING(fiLocation:SCREEN-VALUE, iWarehouseLength + 1))
        .        

    RUN pScanItem.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rdView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rdView W-Win
ON VALUE-CHANGED OF rdView IN FRAME F-Main
DO:
    IF SELF:SCREEN-VALUE EQ "2" THEN DO:    
        RUN select-page(2).
        IF NOT lLocQueryRan THEN
            RUN pScanItem.
    END.
    ELSE IF SELF:SCREEN-VALUE EQ "3" THEN DO:    
        RUN select-page(3).
        IF NOT lLocBinsQueryRan THEN
            RUN pScanItem.
    END.
    ELSE
        RUN select-page(1).    
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
             INPUT  'sharpshooter/smartobj/exit.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.33 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             fiFGItem:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-fginqbins.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-fginqbins ).
       RUN set-position IN h_b-fginqbins ( 6.00 , 2.20 ) NO-ERROR.
       RUN set-size IN h_b-fginqbins ( 27.62 , 187.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatefirst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatefirst ).
       RUN set-position IN h_navigatefirst ( 6.00 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatelast.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatelast ).
       RUN set-position IN h_navigatelast ( 31.00 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatenext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatenext ).
       RUN set-position IN h_navigatenext ( 26.00 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigateprev.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigateprev ).
       RUN set-position IN h_navigateprev ( 10.76 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/adjustqty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_adjustqty ).
       RUN set-position IN h_adjustqty ( 18.38 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       /* Links to SmartBrowser h_b-fginqbins. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Bins':U , h_b-fginqbins ).

       /* Links to SmartObject h_navigatefirst. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqbins , 'NAV-FIRST':U , h_navigatefirst ).

       /* Links to SmartObject h_navigatelast. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqbins , 'NAV-LAST':U , h_navigatelast ).

       /* Links to SmartObject h_navigatenext. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqbins , 'NAV-NEXT':U , h_navigatenext ).

       /* Links to SmartObject h_navigateprev. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqbins , 'NAV-PREV':U , h_navigateprev ).

       /* Links to SmartObject h_adjustqty. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqbins , 'ADJUST':U , h_adjustqty ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatelast ,
             h_navigatefirst , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatenext ,
             h_navigatelast , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigateprev ,
             h_navigatenext , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_adjustqty ,
             h_navigateprev , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-fginqloc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-fginqloc ).
       RUN set-position IN h_b-fginqloc ( 6.00 , 2.20 ) NO-ERROR.
       RUN set-size IN h_b-fginqloc ( 27.62 , 187.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/adjustqty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_adjustqty-2 ).
       RUN set-position IN h_adjustqty-2 ( 18.38 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatelast.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatelast-2 ).
       RUN set-position IN h_navigatelast-2 ( 31.00 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatefirst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatefirst-2 ).
       RUN set-position IN h_navigatefirst-2 ( 6.00 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatenext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatenext-2 ).
       RUN set-position IN h_navigatenext-2 ( 26.00 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigateprev.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigateprev-2 ).
       RUN set-position IN h_navigateprev-2 ( 10.76 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       /* Links to SmartBrowser h_b-fginqloc. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Loc':U , h_b-fginqloc ).

       /* Links to SmartObject h_adjustqty-2. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqloc , 'ADJUST':U , h_adjustqty-2 ).

       /* Links to SmartObject h_navigatelast-2. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqloc , 'NAV-LAST':U , h_navigatelast-2 ).

       /* Links to SmartObject h_navigatefirst-2. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqloc , 'NAV-FIRST':U , h_navigatefirst-2 ).

       /* Links to SmartObject h_navigatenext-2. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqloc , 'NAV-NEXT':U , h_navigatenext-2 ).

       /* Links to SmartObject h_navigateprev-2. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqloc , 'NAV-PREV':U , h_navigateprev-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatelast-2 ,
             h_adjustqty-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatefirst-2 ,
             h_navigatelast-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatenext-2 ,
             h_navigatefirst-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigateprev-2 ,
             h_navigatenext-2 , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigateprev.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigateprev-3 ).
       RUN set-position IN h_navigateprev-3 ( 10.76 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatelast.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatelast-3 ).
       RUN set-position IN h_navigatelast-3 ( 31.00 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatenext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatenext-3 ).
       RUN set-position IN h_navigatenext-3 ( 26.00 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-fginqlocbin.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-fginqlocbin ).
       RUN set-position IN h_b-fginqlocbin ( 6.00 , 2.20 ) NO-ERROR.
       RUN set-size IN h_b-fginqlocbin ( 27.62 , 187.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatefirst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatefirst-3 ).
       RUN set-position IN h_navigatefirst-3 ( 6.00 , 190.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 11.00 ) */

       /* Links to SmartObject h_navigateprev-3. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqlocbin , 'NAV-PREV':U , h_navigateprev-3 ).

       /* Links to SmartObject h_navigatelast-3. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqlocbin , 'NAV-LAST':U , h_navigatelast-3 ).

       /* Links to SmartObject h_navigatenext-3. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqlocbin , 'NAV-NEXT':U , h_navigatenext-3 ).

       /* Links to SmartBrowser h_b-fginqlocbin. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'LocBin':U , h_b-fginqlocbin ).

       /* Links to SmartObject h_navigatefirst-3. */
       RUN add-link IN adm-broker-hdl ( h_b-fginqlocbin , 'NAV-FIRST':U , h_navigatefirst-3 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatenext-3 ,
             h_navigatelast-3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-fginqlocbin ,
             h_navigatenext-3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatefirst-3 ,
             h_b-fginqlocbin , 'AFTER':U ).
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
  DISPLAY rdView fiFGItem fiCustItem fiLocation fiSellUOM 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE rdView fiFGItem fiCustItem fiLocation btItemHelp btnKeyboardItem 
         btnKeyboardLoc btnNumPad btItemNameHelp btnKeyboardItemName btLocHelp 
         RECT-33 RECT-34 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects W-Win 
PROCEDURE local-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy W-Win
PROCEDURE local-destroy:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    IF VALID-HANDLE(hdFGProcs) THEN
        DELETE PROCEDURE hdFGProcs.
        
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClearRecords W-Win
PROCEDURE pClearRecords:
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
    
    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    RUN spGetSessionParam ("Location", OUTPUT cLoc).
    
    FIND FIRST company NO-LOCK 
         WHERE company.company EQ cCompany
         NO-ERROR .
    IF AVAILABLE company THEN
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                         + " - {&awversion}" + " - " 
                         + STRING(company.name) + " - " + cLoc.

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
            INPUT        cCompany,
            INPUT        cWarehouse,
            INPUT        cLocation,
            INPUT-OUTPUT cItemID,
            INPUT-OUTPUT cCustItem,
            INPUT        cJobNo,
            INPUT        iJobNo2,
            INPUT        FALSE,  /* Include Zero qty bins */
            INPUT        TRUE,   /* Include empty tag bins */
            OUTPUT       cSellUOM,
            OUTPUT       lError,
            OUTPUT       cMessage
            ).
    END.

    IF VALID-HANDLE(hdItemLoc) THEN DO:
        lLocQueryRan = TRUE.
        RUN ScanItem IN hdItemLoc (
            INPUT  cCompany,
            INPUT  cItemID,
            INPUT  cCustItem,
            OUTPUT lError,
            OUTPUT cMessage
            ).
    END.

    IF VALID-HANDLE(hdItemLocBin) THEN DO:
        lLocBinsQueryRan = TRUE.
        RUN ScanItem IN hdItemLocBin (
            INPUT  cCompany,
            INPUT  cItemID,
            INPUT  cCustItem,
            INPUT  cWarehouse,
            INPUT  cLocation,
            OUTPUT lError,
            OUTPUT cMessage
            ).
    END.
    
    ASSIGN
        fiFGItem:SCREEN-VALUE   = cItemID
        fiCustItem:SCREEN-VALUE = cCustItem
        fiSellUOM:SCREEN-VALUE  = cSellUOM
        .
    
    APPLY "ENTRY" TO fiFGItem.
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

