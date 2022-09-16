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

  File: sharpshooter/w-phyCount.w

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
{Inventory/ttInventory.i}
{Inventory/ttPhysicalBrowseInventory.i}
{methods/template/brwcustomdef.i}

DEFINE VARIABLE cCompany              AS CHARACTER       NO-UNDO.
DEFINE VARIABLE cUser                 AS CHARACTER       NO-UNDO.
DEFINE VARIABLE iWarehouseLength      AS INTEGER         NO-UNDO.
DEFINE VARIABLE hdInventoryProcs      AS HANDLE          NO-UNDO.
DEFINE VARIABLE hdStatusColumn        AS HANDLE          NO-UNDO.

/* Required for run_link.i */
DEFINE VARIABLE char-hdl              AS CHARACTER       NO-UNDO.
DEFINE VARIABLE pHandle               AS HANDLE          NO-UNDO.
DEFINE VARIABLE iSnapShotID           AS INTEGER         NO-UNDO.

DEFINE VARIABLE glShowVirtualKeyboard     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcShowSettings            AS CHARACTER NO-UNDO.
DEFINE VARIABLE glEnableSnapshotSelection AS LOGICAL   NO-UNDO.

DEFINE VARIABLE oKeyboard             AS system.Keyboard NO-UNDO.

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
&Scoped-define INTERNAL-TABLES ttPhysicalBrowseInventory

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table ttPhysicalBrowseInventory.tag ttPhysicalBrowseInventory.itemType ttPhysicalBrowseInventory.itemID ttPhysicalBrowseInventory.quantity ttPhysicalBrowseInventory.origQuantity ttPhysicalBrowseInventory.quantityUOM ttPhysicalBrowseInventory.location ttPhysicalBrowseInventory.origLocation ttPhysicalBrowseInventory.inventoryStatus   
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
&Scoped-Define ENABLED-OBJECTS btClear btnNumPad fiLocation br-table ~
btnExitText btnClearText statusMessage btnAdjustQtyText btnSettingsText 
&Scoped-Define DISPLAYED-OBJECTS cbSnapshot fiLocation btnExitText ~
btnClearText statusMessage btnAdjustQtyText btnSettingsText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_adjustqty AS HANDLE NO-UNDO.
DEFINE VARIABLE h_adjustwindowsize AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
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

DEFINE BUTTON btNextLoc 
     LABEL "Next Location" 
     SIZE 33 BY 1.57.

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE VARIABLE cbSnapshot AS CHARACTER FORMAT "X(256)":U 
     LABEL "Snapshot" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 67 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE btnAdjustQtyText AS CHARACTER FORMAT "X(256)":U INITIAL "ADJUST QTY" 
      VIEW-AS TEXT 
     SIZE 23 BY 1.43
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

DEFINE VARIABLE fiLocation AS CHARACTER FORMAT "X(256)":U 
     LABEL "LOCATION" 
     VIEW-AS FILL-IN 
     SIZE 67.2 BY 1.38
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
DEFINE QUERY br-table FOR 
      ttPhysicalBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table W-Win _FREEFORM
  QUERY br-table DISPLAY
      ttPhysicalBrowseInventory.tag WIDTH 45 COLUMN-LABEL "Tag #" FORMAT "X(30)"
    ttPhysicalBrowseInventory.itemType WIDTH 8 COLUMN-LABEL "Type" 
    ttPhysicalBrowseInventory.itemID WIDTH 40 COLUMN-LABEL "Item" FORMAT "X(15)"
    ttPhysicalBrowseInventory.quantity WIDTH 28 COLUMN-LABEL "Scanned Quantity"
    ttPhysicalBrowseInventory.origQuantity WIDTH 28 COLUMN-LABEL "System Quantity"
    ttPhysicalBrowseInventory.quantityUOM WIDTH 10 COLUMN-LABEL "UOM"
    ttPhysicalBrowseInventory.location WIDTH 28 COLUMN-LABEL "Scanned Location" FORMAT "X(12)"
    ttPhysicalBrowseInventory.origLocation WIDTH 28 COLUMN-LABEL "System Location" FORMAT "X(12)"
    ttPhysicalBrowseInventory.inventoryStatus COLUMN-LABEL "Status" FORMAT "X(40)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 194.8 BY 12.38
         BGCOLOR 15 FGCOLOR 0 FONT 36 ROW-HEIGHT-CHARS 1.05 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btClear AT ROW 3.19 COL 194.8 WIDGET-ID 146
     btnNumPad AT ROW 2.38 COL 129 WIDGET-ID 120 NO-TAB-STOP 
     cbSnapshot AT ROW 3.14 COL 22.8 COLON-ALIGNED WIDGET-ID 152
     btNextLoc AT ROW 4.86 COL 97 WIDGET-ID 154
     fiLocation AT ROW 4.95 COL 22.8 COLON-ALIGNED WIDGET-ID 6
     br-table AT ROW 9.1 COL 2 WIDGET-ID 200
     btnExitText AT ROW 1.24 COL 189 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     btnClearText AT ROW 3.38 COL 182 NO-LABEL WIDGET-ID 148
     statusMessage AT ROW 21.86 COL 41.4 NO-LABEL WIDGET-ID 66
     btnAdjustQtyText AT ROW 21.95 COL 6.6 NO-LABEL WIDGET-ID 156
     btnSettingsText AT ROW 21.95 COL 179 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     RECT-2 AT ROW 2.19 COL 128 WIDGET-ID 130
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 207.8 BY 23.29
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
         TITLE              = "Physical Count"
         HEIGHT             = 23.33
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
/* BROWSE-TAB br-table fiLocation F-Main */
/* SETTINGS FOR FILL-IN btnAdjustQtyText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN btnClearText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR BUTTON btNextLoc IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btNextLoc:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR COMBO-BOX cbSnapshot IN FRAME F-Main
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
OPEN QUERY {&SELF-NAME} FOR EACH ttPhysicalBrowseInventory
BY ttPhysicalBrowseInventory.lastTransTime DESCENDING.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Physical Count */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
DO:
    /* This case occurs when the user presses the "Esc" key.
       In a persistently run window, just ignore this.  If we did not, the
       application would exit. */
    IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Physical Count */
DO:
    /* This ADM code must be left here in order for the SmartWindow
       and its descendents to terminate properly on exit. */
    IF VALID-HANDLE (hdInventoryProcs) THEN
        DELETE PROCEDURE hdInventoryProcs.
    
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
    RUN AdjustQuantity.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table W-Win
ON ROW-DISPLAY OF br-table IN FRAME F-Main
DO:
&scoped-define exclude-row-display true 
    {methods/template/brwrowdisplay.i} 
   
    DEFINE VARIABLE iColor AS INTEGER NO-UNDO.

    IF AVAILABLE ttPhysicalBrowseInventory  THEN 
    DO:
        iColor = DYNAMIC-FUNCTION (
            "fGetRowBGColor" IN hdInventoryProcs,
            INPUT ttPhysicalBrowseInventory.inventoryStatus
            ).
             
        RUN pUpdateColor (iColor).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table W-Win
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:    
/*     ASSIGN */
/*         btAdjustQty:SENSITIVE         = AVAILABLE ttPhysicalBrowseInventory */
/*         btConfirmNotScanned:SENSITIVE = AVAILABLE ttPhysicalBrowseInventory AND ttPhysicalBrowseInventory.inventoryStatus EQ gcStatusSnapshotNotScanned */
/*         btDelete:SENSITIVE            = AVAILABLE ttPhysicalBrowseInventory AND ttPhysicalBrowseInventory.inventoryStatus NE gcStatusSnapshotNotScanned */
/*         . */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btClear W-Win
ON CHOOSE OF btClear IN FRAME F-Main /* Reset */
DO:
    RUN pStatusMessage ("", 0).

    RUN pResetWarning NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN NO-APPLY.
    
    RUN Reset.
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
    RUN WindowExit.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNextLoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNextLoc W-Win
ON CHOOSE OF btNextLoc IN FRAME F-Main /* Next Location */
DO:
    DEFINE VARIABLE cChoice   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cBin      AS CHARACTER NO-UNDO.
    
    RUN pStatusMessage ("", 0).

    IF CAN-FIND (FIRST ttPhysicalBrowseInventory
                 WHERE ttPhysicalBrowseInventory.inventoryStatus EQ gcStatusSnapshotNotScanned
                    OR ttPhysicalBrowseInventory.inventoryStatus EQ gcStatusSnapshotNotScannedCountingZero) THEN DO:
        ASSIGN
            cLocation = TRIM(SUBSTRING(fiLocation:SCREEN-VALUE, 1, iWarehouseLength))
            cBin      = TRIM(SUBSTRING(fiLocation:SCREEN-VALUE, iWarehouseLength + 1))
            .

        FOR EACH ttPhysicalBrowseInventory
            WHERE ttPhysicalBrowseInventory.inventoryStatus EQ gcStatusSnapshotNotScanned:
            ASSIGN
                ttPhysicalBrowseInventory.lastTransTime   = NOW
                ttPhysicalBrowseInventory.inventoryStatus = gcStatusSnapshotNotScannedCountingZero
                ttPhysicalBrowseInventory.quantity        = 0 
                ttPhysicalBrowseInventory.warehouseID     = CAPS(cLocation)  
                ttPhysicalBrowseInventory.locationID      = CAPS(cBin)
                ttPhysicalBrowseInventory.location        = ttPhysicalBrowseInventory.warehouseID 
                                                          + FILL(" ", iWarehouseLength - LENGTH(ttPhysicalBrowseInventory.warehouseID)) 
                                                          + ttPhysicalBrowseInventory.locationID  
                .
        END.
        
        {&OPEN-QUERY-{&BROWSE-NAME}} 
        
        RUN sharpshooter/messageDialogCustom.w (
            INPUT  "Some tags in the system snapshot have not been scanned.~nDo you want to confirm that these are not in this location or re-check?",
            INPUT  "CONFIRM,RE-CHECK",
            OUTPUT cChoice
            ).
        
        IF cChoice EQ "RE-CHECK" THEN DO: 
            {methods/run_link.i "TAG-SOURCE" "Set-Focus"}
            
            RETURN.
        END.
    END.

    RUN pCreateCounts.
    
    RUN Reset.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNumPad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNumPad W-Win
ON CHOOSE OF btnNumPad IN FRAME F-Main /* NumPad */
DO:
    ASSIGN
        oKeyboard:DisplayKeyboard = NOT oKeyboard:DisplayKeyboard
        RECT-2:BGCOLOR            = IF oKeyboard:DisplayKeyboard THEN 10 ELSE 12
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


&Scoped-define SELF-NAME cbSnapshot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbSnapshot W-Win
ON VALUE-CHANGED OF cbSnapshot IN FRAME F-Main /* Snapshot */
DO:
    iSnapShotID = INTEGER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON ENTER OF fiLocation IN FRAME F-Main /* LOCATION */
DO:
    RUN pLocationScan NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON ENTRY OF fiLocation IN FRAME F-Main /* LOCATION */
DO:
    SELF:BGCOLOR = 30.

    oKeyboard:OpenKeyboard (SELF, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON LEAVE OF fiLocation IN FRAME F-Main /* LOCATION */
DO:
    SELF:BGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON TAB OF fiLocation IN FRAME F-Main /* LOCATION */
DO:
    RUN pLocationScan NO-ERROR.
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AdjustQuantity W-Win 
PROCEDURE AdjustQuantity :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lValueReturned AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dValue         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cChoice        AS CHARACTER NO-UNDO.
    
    IF AVAILABLE ttPhysicalBrowseInventory THEN DO:
        RUN sharpshooter/modifyQuantity.w (ttPhysicalBrowseInventory.quantity, OUTPUT dValue, OUTPUT lValueReturned).
        IF lValueReturned THEN DO:
            IF ttPhysicalBrowseInventory.quantity EQ dValue THEN DO:
                RUN sharpshooter/messageDialogCustom.w (
                    INPUT  "Adjusted quantity for tag " + ttPhysicalBrowseInventory.tag + " is same as existing quantity",
                    INPUT  "OK",
                    OUTPUT cChoice
                    ).
                RETURN.
            END.

            RUN pAdjustQuantity (ttPhysicalBrowseInventory.company, ttPhysicalBrowseInventory.tag, dValue).        
        END.
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
       RUN set-position IN h_adjustwindowsize ( 1.00 , 157.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 32.00 ) */

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/exit.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.10 , 200.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/tagfilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_tagfilter ).
       RUN set-position IN h_tagfilter ( 6.57 , 14.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.29 , 85.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatefirst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatefirst ).
       RUN set-position IN h_navigatefirst ( 9.57 , 200.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigateprev.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigateprev ).
       RUN set-position IN h_navigateprev ( 11.76 , 200.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatenext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatenext ).
       RUN set-position IN h_navigatenext ( 14.57 , 200.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatelast.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatelast ).
       RUN set-position IN h_navigatelast ( 16.91 , 200.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/adjustqty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_adjustqty ).
       RUN set-position IN h_adjustqty ( 21.67 , 30.60 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/setting.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_setting ).
       RUN set-position IN h_setting ( 21.67 , 200.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.60 ) */

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

       /* Links to SmartObject h_adjustqty. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'ADJUST':U , h_adjustqty ).

       /* Links to SmartObject h_setting. */
       RUN add-link IN adm-broker-hdl ( h_setting , 'SETTING':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_tagfilter ,
             fiLocation:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatefirst ,
             br-table:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigateprev ,
             h_navigatefirst , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatenext ,
             h_navigateprev , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatelast ,
             h_navigatenext , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_adjustqty ,
             h_navigatelast , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_setting ,
             h_adjustqty , 'AFTER':U ).
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
  DISPLAY cbSnapshot fiLocation btnExitText btnClearText statusMessage 
          btnAdjustQtyText btnSettingsText 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btClear btnNumPad fiLocation br-table btnExitText btnClearText 
         statusMessage btnAdjustQtyText btnSettingsText 
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

    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE OBJECT hdInventoryProcs.
      
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAdjustQuantity W-Win 
PROCEDURE pAdjustQuantity PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag              AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantity         AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE lCreated AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN pAdjustTransactionQuantity IN hdInventoryProcs (
        INPUT  ipcCompany,
        INPUT  iSnapshotID,
        INPUT  ipcTag,
        INPUT  ipdQuantity, /* Zeroing out */
        OUTPUT lCreated,
        OUTPUT cMessage,
        INPUT-OUTPUT TABLE ttPhysicalBrowseInventory BY-REFERENCE
        ).

    {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateCounts W-Win 
PROCEDURE pCreateCounts PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iSequeceID AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE dQuantityOfSubUnits AS DECIMAL NO-UNDO.
    
    RUN pStatusMessage("Saving scans..." ,1).
    
    FOR EACH ttPhysicalBrowseInventory:
        dQuantityOfSubUnits = IF ttPhysicalBrowseInventory.itemType EQ "RM" THEN 
                                  ttPhysicalBrowseInventory.quantity 
                              ELSE IF ttPhysicalBrowseInventory.inventoryStatus EQ gcStatusSnapshotNotScannedCountingZero THEN
                                  0
                              ELSE
                                  ttPhysicalBrowseInventory.quantityOfSubUnits.
        
        RUN api\inbound\CreateInventoryCount.p (
            INPUT        ttPhysicalBrowseInventory.company,
            INPUT        ttPhysicalBrowseInventory.warehouseID,
            INPUT        ttPhysicalBrowseInventory.locationID,
            INPUT        "",
            INPUT        0,
            INPUT        0,
            INPUT-OUTPUT ttPhysicalBrowseInventory.jobID,
            INPUT-OUTPUT ttPhysicalBrowseInventory.jobID2,
            INPUT        ttPhysicalBrowseInventory.itemID,
            INPUT        ttPhysicalBrowseInventory.itemType,
            INPUT        ttPhysicalBrowseInventory.tag,
            INPUT        dQuantityOfSubUnits,
            INPUT        IF ttPhysicalBrowseInventory.itemType EQ "RM" THEN 1 ELSE ttPhysicalBrowseInventory.quantityPerSubUnit,
            INPUT        IF ttPhysicalBrowseInventory.itemType EQ "RM" THEN 0 ELSE ttPhysicalBrowseInventory.quantityPartial,
            INPUT        ttPhysicalBrowseInventory.quantityUOM,
            INPUT        FALSE,
            INPUT        FALSE,
            OUTPUT       iSequeceID,
            OUTPUT       lSuccess,
            OUTPUT       cMessage
            ) NO-ERROR.            
    END.
    
    RUN pStatusMessage("Save complete" ,1).     
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
    DEFINE VARIABLE iBrowseCol    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hdBrowseCol   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cChoice       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSnapshotList AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN spGetSessionParam ("UserID", OUTPUT cUser).
    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    
    RUN pStatusMessage ("", 0).

    RUN spGetSettingByName ("ShowVirtualKeyboard", OUTPUT cSettingValue).
    glShowVirtualKeyboard = LOGICAL(cSettingValue) NO-ERROR.
    
    RUN spGetSettingByName ("ShowSettings", OUTPUT gcShowSettings).
    
    oKeyboard:SetWindow({&WINDOW-NAME}:HANDLE).
    oKeyboard:SetProcedure(THIS-PROCEDURE).
    oKeyboard:SetFrame(FRAME {&FRAME-NAME}:HANDLE).

    ASSIGN
        btnSettingsText:VISIBLE = INDEX(gcShowSettings, "Text") GT 0
        btnNumPad:VISIBLE       = glShowVirtualKeyboard
        RECT-2:VISIBLE          = glShowVirtualKeyboard
        .  

    ASSIGN
        {&BROWSE-NAME}:BGCOLOR           = 25
        {&BROWSE-NAME}:FGCOLOR           = 0
        {&BROWSE-NAME}:SEPARATOR-FGCOLOR = 15
        {&BROWSE-NAME}:ROW-HEIGHT-CHARS  = 1
        {&BROWSE-NAME}:FONT              = 36
        {&BROWSE-NAME}:FIT-LAST-COLUMN   = TRUE
        .

    hColumnRowColor = {&BROWSE-NAME}:FIRST-COLUMN.
    DO WHILE VALID-HANDLE(hColumnRowColor):
        ASSIGN
            cColHandList    = cColHandList + ","  + string(hColumnRowColor)
            hColumnRowColor = hColumnRowColor:NEXT-COLUMN
            .
    END. /* do while */
    cColHandList = TRIM(cColHandList, ",").

    IF NOT CAN-FIND(FIRST inventorySnapshot NO-LOCK
                    WHERE inventorySnapshot.company EQ cCompany) THEN DO:
        RUN sharpshooter/messageDialogCustom.w (
            INPUT  "This application requires an Inventory Snapshot to be created.",
            INPUT  "OK",
            OUTPUT cChoice
            ).
            
        APPLY "CLOSE" TO THIS-PROCEDURE.
        RETURN.
    END.
    
    DO iBrowseCol = 1 TO BROWSE {&BROWSE-NAME}:NUM-COLUMNS:
        hdBrowseCol = BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseCol).
        
        IF hdBrowseCol:NAME EQ "inventoryStatus" THEN
            hdStatusColumn = hdBrowseCol.
    END.
                        
    IF INDEX(gcShowSettings, "Icon") EQ 0 THEN
        {methods/run_link.i "Setting-SOURCE" "HideSettings"}        

    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.    

    RUN Inventory_GetWarehouseLength IN hdInventoryProcs (
        INPUT  cCompany,
        OUTPUT iWarehouseLength
        ).
    
    {methods/run_link.i "TAG-SOURCE" "DisableAll"}
    {methods/run_link.i "TAG-SOURCE" "DisableErrorAlerts"}
         
    FOR EACH inventorySnapshot NO-LOCK
        WHERE inventorySnapshot.company EQ cCompany
        BY inventorySnapshot.inventorySnapshotID DESCENDING:
        cSnapshotList = cSnapshotList + "," + inventorySnapshot.snapshotDesc + "," + STRING(inventorySnapshot.inventorySnapshotID). 
    END.
    
    ASSIGN
        cSnapshotList              = TRIM(cSnapshotList, ",")
        cbSnapshot:LIST-ITEM-PAIRS = cSnapshotList
        cbSnapshot:SCREEN-VALUE    = ENTRY(2, cSnapshotList)
        cbSnapshot:SENSITIVE       = glEnableSnapshotSelection
        iSnapShotID                = INTEGER(cbSnapshot:SCREEN-VALUE) 
        .
        
    APPLY "ENTRY" TO fiLocation.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationScan W-Win 
PROCEDURE pLocationScan :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lError         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWarehouse     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocation      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValidLocation AS LOGICAL   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN pStatusMessage ("", 0).

    IF (LASTKEY EQ -1 OR (LASTKEY GE 609 AND LASTKEY LE 652)) AND NOT (VALID-OBJECT (oKeyboard) AND oKeyboard:IsKeyboardOpen()) THEN
        RETURN.

    IF fiLocation:SCREEN-VALUE EQ "" THEN
        RETURN.
            
    ASSIGN
        cWarehouse = TRIM(SUBSTRING(fiLocation:SCREEN-VALUE, 1, iWarehouseLength))
        cLocation  = TRIM(SUBSTRING(fiLocation:SCREEN-VALUE, iWarehouseLength + 1))
        .
    
    IF cWarehouse EQ "" THEN 
    DO:
        RUN pStatusMessage ("WAREHOUSE CANNOT BE EMPTY", 3).
        RETURN ERROR.  
    END.

    IF cLocation EQ "" THEN 
    DO:
        RUN pStatusMessage ("LOCATION CANNOT BE EMPTY", 3).
        RETURN ERROR. 
    END.
    
    RUN ValidateLoc IN hdInventoryProcs (
        INPUT  cCompany,
        INPUT  cWarehouse,
        OUTPUT lValidLocation
        ).
    IF NOT lValidLocation THEN 
    DO:
        RUN pStatusMessage ("INVALID LOCATION '" + cWarehouse + "'", 3).
        RETURN ERROR.     
    END.
        
    RUN Inventory_GetSnapShotForLocation IN hdInventoryProcs (
        INPUT cCompany,
        INPUT iSnapShotID,
        INPUT cWarehouse,
        INPUT cLocation,
        INPUT-OUTPUT TABLE ttPhysicalBrowseInventory BY-REFERENCE
        ).
    
    IF NOT TEMP-TABLE ttPhysicalBrowseInventory:HAS-RECORDS THEN DO:
        RUN pStatusMessage ("NO SNAPSHOT RECORDS EXIST FOR SCANNED LOCATION", 3).
        RETURN ERROR.    
    END.
    
    ASSIGN
        fiLocation:SENSITIVE = FALSE
        fiLocation:BGCOLOR   = 15
        btNextLoc:VISIBLE    = TRUE
        btNextLoc:SENSITIVE  = TRUE
        .
           
    {methods/run_link.i "TAG-SOURCE" "EnableAll"}
    {methods/run_link.i "TAG-SOURCE" "Set-Focus"}
    
    {&OPEN-QUERY-{&BROWSE-NAME}}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResetWarning W-Win 
PROCEDURE pResetWarning PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lChoice AS LOGICAL NO-UNDO.
    
    IF CAN-FIND(FIRST ttPhysicalBrowseInventory
                WHERE ttPhysicalBrowseInventory.inventoryStatus NE gcStatusSnapshotNotScanned) THEN DO:
        RUN sharpshooter/messageDialog.w (
            INPUT  "Some tags have been scanned. Changes will not be saved. Are you sure you want to proceed?",
            INPUT  TRUE,
            INPUT  TRUE,
            INPUT  FALSE,
            OUTPUT lChoice
            ).
        
        IF NOT lChoice THEN 
            RETURN ERROR.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTagScan W-Win 
PROCEDURE pTagScan :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTag      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cBin      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCreated  AS LOGICAL   NO-UNDO.    
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
        
    {methods/run_link.i "TAG-SOURCE" "GetScannedTag" "(OUTPUT cTag)"}

    ASSIGN
        cLocation = TRIM(SUBSTRING(fiLocation:SCREEN-VALUE, 1, iWarehouseLength))
        cBin      = TRIM(SUBSTRING(fiLocation:SCREEN-VALUE, iWarehouseLength + 1))
        .
            
    RUN SubmitPhysicalCountScan IN hdInventoryProcs (
        cCompany,
        iSnapShotID,
        cLocation,
        cBin,
        cTag,
        TRUE, /* Set input parameter location to record */
        OUTPUT lCreated,
        OUTPUT cMessage,
        INPUT-OUTPUT TABLE ttPhysicalBrowseInventory BY-REFERENCE
        ).

    {&OPEN-QUERY-{&BROWSE-NAME}}          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateColor W-Win 
PROCEDURE pUpdateColor :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiColor   AS INTEGER NO-UNDO.

    hdStatusColumn:BGCOLOR = ipiColor.
    
    IF ipiColor EQ 12 THEN
        hdStatusColumn:FGCOLOR = 15.
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
            statusMessage:ROW            = {&WINDOW-NAME}:HEIGHT - .86
            dCol                         = {&WINDOW-NAME}:WIDTH  - 8
            btnExitText:COL              = dCol - 9
            btnAdjustQtyText:ROW         = {&WINDOW-NAME}:HEIGHT - .86
            btnSettingsText:ROW          = {&WINDOW-NAME}:HEIGHT - .86
            btnSettingsText:COL          = dCol - 20
            btnClearText:COL             = dCol - 12
            btClear:COL                  = dCol            
            BROWSE {&BROWSE-NAME}:HEIGHT = {&WINDOW-NAME}:HEIGHT - BROWSE {&BROWSE-NAME}:ROW - 1.62
            BROWSE {&BROWSE-NAME}:WIDTH  = dCol - 2
            .

        RUN set-position IN h_adjustqty ({&WINDOW-NAME}:HEIGHT - 1.1, btnAdjustQtyText:COL + 23).
        RUN set-position IN h_exit ( 1.00 , dCol ) NO-ERROR.
        RUN set-position IN h_setting ( {&WINDOW-NAME}:HEIGHT - 1.1 , btnSettingsText:COL + 18 ) NO-ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reset W-Win 
PROCEDURE Reset :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiLocation:SCREEN-VALUE = ""
        fiLocation:SENSITIVE    = TRUE
        fiLocation:BGCOLOR      = 30
        btNextLoc:VISIBLE       = FALSE
        .
    
    APPLY "ENTRY" TO fiLocation.    

    {methods/run_link.i "TAG-SOURCE" "DisableAll"}

    EMPTY TEMP-TABLE ttPhysicalBrowseInventory.

    {&OPEN-QUERY-{&BROWSE-NAME}}  
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
  {src/adm/template/snd-list.i "ttPhysicalBrowseInventory"}

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
    {methods/run_link.i "RETURN-SOURCE" "Set-Focus"}
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
        WHEN "tag-valid" THEN 
            DO:
                RUN pTagScan.
            
                {methods/run_link.i "TAG-SOURCE" "EmptyTag"}
                {methods/run_link.i "TAG-SOURCE" "ScanNextTag"}
            END.
        WHEN "tag-error" THEN 
            DO:
                {methods/run_link.i "TAG-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
            
                RUN pStatusMessage (cStatusMessage, iStatusMessageType).
            END.              
    END CASE. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WindowExit W-Win 
PROCEDURE WindowExit :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN pResetWarning NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN NO-APPLY.
        
    RUN dispatch ("exit").
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

