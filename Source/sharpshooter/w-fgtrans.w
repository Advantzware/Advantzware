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

{inventory/ttInventory.i "NEW SHARED"}
{jc/jcgl-sh.i  NEW}

DEFINE VARIABLE oLoadTag         AS Inventory.Loadtag NO-UNDO.
DEFINE VARIABLE oFGBin           AS fg.FGBin          NO-UNDO.
DEFINE VARIABLE oItemFG          AS fg.ItemFG         NO-UNDO.

DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cTag             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany         AS CHARACTER NO-UNDO.

DEFINE VARIABLE gcLocationSource AS CHARACTER NO-UNDO INITIAL "LoadTag".
DEFINE VARIABLE glCloseJob       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glAutoPost       AS LOGICAL   NO-UNDO INITIAL TRUE.

RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.

RUN spGetSessionParam ("Company", OUTPUT cCompany).

oLoadTag = NEW Inventory.Loadtag().
oFGBin   = NEW fg.FGBin().
oItemFG  = NEW fg.ItemFG().

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttBrowseInventory

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttBrowseInventory.fgItemID fGetConcatLocationID() @ ttBrowseInventory.warehouseID ttBrowseInventory.tag ttBrowseInventory.quantity fGetInventoryStatus() @ ttBrowseInventory.inventoryStatus   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttBrowseInventory
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-33 fiTag btExit fiLocation ~
btClearRecords BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS fiTag fiLocation fiMessage 

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
     LABEL "Clear Records" 
     SIZE 21.6 BY 1.14 TOOLTIP "Clear all the scanned data from grid".

DEFINE BUTTON btExit AUTO-END-KEY 
     IMAGE-UP FILE "C:/Asigui/Environments/Devel/Resources/Graphics/32x32/door_exit.ico":U
     LABEL "Exit" 
     SIZE 11 BY 2.62 TOOLTIP "Exit".

DEFINE BUTTON btPost 
     LABEL "Post" 
     SIZE 15 BY 2.62.

DEFINE BUTTON btTransfer 
     LABEL "Transfer" 
     SIZE 28 BY 1.43.

DEFINE VARIABLE fiLocation AS CHARACTER FORMAT "X(256)":U 
     LABEL "Location" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 85 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiTag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tag" 
     VIEW-AS FILL-IN 
     SIZE 71 BY 1.38 NO-UNDO.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 210 BY .05.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 W-Win _FREEFORM
  QUERY BROWSE-2 DISPLAY
      ttBrowseInventory.fgItemID WIDTH 40 COLUMN-LABEL "Item #"
      fGetConcatLocationID() @ ttBrowseInventory.warehouseID WIDTH 30 COLUMN-LABEL "Location" FORMAT "X(12)"
      ttBrowseInventory.tag WIDTH 70 COLUMN-LABEL "Tag #" FORMAT "X(30)"
      ttBrowseInventory.quantity WIDTH 25 COLUMN-LABEL "Quantity" FORMAT "->,>>>,>>9.99<<<<"
      fGetInventoryStatus() @ ttBrowseInventory.inventoryStatus COLUMN-LABEL "Status" FORMAT "X(30)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-TAB-STOP SIZE 207 BY 23.33
         FONT 17 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiTag AT ROW 1.48 COL 19 COLON-ALIGNED WIDGET-ID 4
     btPost AT ROW 1.57 COL 179.2 WIDGET-ID 20 NO-TAB-STOP 
     btExit AT ROW 1.57 COL 200.2 WIDGET-ID 26 NO-TAB-STOP 
     btTransfer AT ROW 3.29 COL 64.2 WIDGET-ID 8
     fiLocation AT ROW 3.33 COL 19 COLON-ALIGNED WIDGET-ID 6
     fiMessage AT ROW 5 COL 5 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     btClearRecords AT ROW 5.14 COL 189.6 WIDGET-ID 28 NO-TAB-STOP 
     BROWSE-2 AT ROW 7.05 COL 4 WIDGET-ID 200
     RECT-33 AT ROW 6.57 COL 2.4 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 212.6 BY 29.62
         BGCOLOR 15 FONT 36 WIDGET-ID 100.


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
         WIDTH              = 212.6
         MAX-HEIGHT         = 29.62
         MAX-WIDTH          = 212.6
         VIRTUAL-HEIGHT     = 29.62
         VIRTUAL-WIDTH      = 212.6
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
/* BROWSE-TAB BROWSE-2 btClearRecords F-Main */
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
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
    
    APPLY "CLOSE":U TO THIS-PROCEDURE.
   
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btClearRecords
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btClearRecords W-Win
ON CHOOSE OF btClearRecords IN FRAME F-Main /* Clear Records */
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


&Scoped-define SELF-NAME fiLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON LEAVE OF fiLocation IN FRAME F-Main /* Location */
DO:
    DEFINE VARIABLE lError     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWarehouse AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocation  AS CHARACTER NO-UNDO.
    
    ASSIGN
        cWarehouse = TRIM(SUBSTRING(SELF:SCREEN-VALUE, 1, 5))
        cLocation  = TRIM(SUBSTRING(SELF:SCREEN-VALUE, 6))
        .
    
    IF cWarehouse EQ "" THEN DO:
        MESSAGE "Warehouse cannot be empty"
        VIEW-AS ALERT-BOX ERROR.    
    END.

    IF cLocation EQ "" THEN DO:
        MESSAGE "Location cannot be empty"
        VIEW-AS ALERT-BOX ERROR.    
    END.
        
    RUN pLocationScan (
        INPUT  fiTag:SCREEN-VALUE,
        INPUT  cWarehouse,
        INPUT  cLocation,
        OUTPUT lError,
        OUTPUT cMessage
        ). 
    IF lError THEN
        MESSAGE cMessage
        VIEW-AS ALERT-BOX ERROR.
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
ON ENTRY OF fiTag IN FRAME F-Main /* Tag */
DO:
    ASSIGN
        btTransfer:VISIBLE     = FALSE
        fiMessage:SCREEN-VALUE = ""
        .  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON LEAVE OF fiTag IN FRAME F-Main /* Tag */
DO:
    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lIsTransfer AS LOGICAL   NO-UNDO.
    
    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    RUN pTagScan (
        INPUT  SELF:SCREEN-VALUE,
        OUTPUT lIsTransfer,
        OUTPUT lError,
        OUTPUT cMessage
        ).

    {&OPEN-QUERY-{&BROWSE-NAME}}
      
    IF lError THEN DO:
        MESSAGE cMessage 
            VIEW-AS ALERT-BOX ERROR.
        
        RETURN NO-APPLY.
    END.
    
    IF NOT lIsTransfer THEN DO:
        SELF:SCREEN-VALUE = "".
        
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

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
  DISPLAY fiTag fiLocation fiMessage 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-33 fiTag btExit fiLocation btClearRecords BROWSE-2 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
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
 
    oFGBin:SetContext (cCompany, cItemID, ipcTag).
    IF NOT oFGBin:IsAvailable() THEN DO:
        ASSIGN
            oplError = TRUE
            opcMessage = "FG Bin not available for tag '" + ipcTag + "'"
            .
        RETURN.
    END.
        
    ASSIGN
        iSubUnits        = INTEGER(oFGBin:GetValue("QuantityInSubUnit"))
        iSubUnitsPerUnit = INTEGER(oFGBin:GetValue("SubUnitsPerUnit"))
        iPartial         = INTEGER(oFGBin:GetValue("Partial"))
        iQuantity        = iSubUnits * iSubUnitsPerUnit + iPartial
        .
                                  
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
    END.
    ELSE DO:
        oplError = TRUE.
        RETURN.
    END.
        
    IF glAutoPost THEN
        RUN pPost.
    ELSE
        fiMessage:SCREEN-VALUE = "Receipt Transaction created".
        
    {&OPEN-QUERY-{&BROWSE-NAME}}    
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
            MESSAGE cMessage
                VIEW-AS ALERT-BOX ERROR.
            
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
                            + FILL(" ", 5 - LENGTH(cWarehouse)) 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetConcatLocationID W-Win 
FUNCTION fGetConcatLocationID RETURNS CHARACTER PRIVATE
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    RETURN ttBrowseInventory.warehouseID + " " 
           + FILL(" ", 5 - LENGTH(ttBrowseInventory.warehouseID)) 
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

