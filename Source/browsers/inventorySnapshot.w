&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/inventorySnapshot.w

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

&SCOPED-DEFINE yellowColumnsName inventorySnapshot
&SCOPED-DEFINE noSortByField
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}
{methods/template/brwcustomdef.i}
{methods/defines/sortByDefs.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cCompany    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lMoveColumn AS LOGICAL   NO-UNDO INITIAL TRUE.

{custom/globdefs.i}
{sys/inc/var.i new shared}
{sys/inc/varasgn.i}

RUN spGetSessionParam ("Company", OUTPUT cCompany).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES inventorySnapshot

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table ~
inventorySnapshot.inventorySnapshotID inventorySnapshot.snapshotDesc ~
inventorySnapshot.snapshotType inventorySnapshot.itemType ~
inventorySnapshot.warehouseID inventorySnapshot.locationID ~
inventorySnapshot.inventoryStockStatus inventorySnapshot.snapshotUser ~
inventorySnapshot.snapshotTime 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH inventorySnapshot WHERE ~{&KEY-PHRASE} ~
      AND inventorySnapshot.company EQ cCompany NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH inventorySnapshot WHERE ~{&KEY-PHRASE} ~
      AND inventorySnapshot.company EQ cCompany NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table inventorySnapshot
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table inventorySnapshot


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Delete_invSnapshot 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE BUTTON Btn_Delete_invSnapshot 
     LABEL "&Delete" 
     SIZE 23 BY 1.29
     BGCOLOR 14 FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 25 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 25 BY 1.76
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      inventorySnapshot
    FIELDS(inventorySnapshot.inventorySnapshotID
      inventorySnapshot.snapshotDesc
      inventorySnapshot.snapshotType
      inventorySnapshot.itemType
      inventorySnapshot.warehouseID
      inventorySnapshot.locationID
      inventorySnapshot.inventoryStockStatus
      inventorySnapshot.snapshotUser
      inventorySnapshot.snapshotTime) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      inventorySnapshot.inventorySnapshotID COLUMN-LABEL "ID" FORMAT "->,>>>,>>9":U
            LABEL-BGCOLOR 14
      inventorySnapshot.snapshotDesc FORMAT "x(40)":U LABEL-BGCOLOR 14
      inventorySnapshot.snapshotType COLUMN-LABEL "Type" FORMAT "x(8)":U
            LABEL-BGCOLOR 14
      inventorySnapshot.itemType COLUMN-LABEL "Item Type" FORMAT "x(8)":U
            LABEL-BGCOLOR 14
      inventorySnapshot.warehouseID COLUMN-LABEL "Warehouse" FORMAT "x(8)":U
            LABEL-BGCOLOR 14
      inventorySnapshot.locationID COLUMN-LABEL "Location" FORMAT "x(8)":U
            LABEL-BGCOLOR 14
      inventorySnapshot.inventoryStockStatus COLUMN-LABEL "Stock Status" FORMAT "x(8)":U
            LABEL-BGCOLOR 14
      inventorySnapshot.snapshotUser COLUMN-LABEL "User" FORMAT "x(8)":U
            LABEL-BGCOLOR 14
      inventorySnapshot.snapshotTime COLUMN-LABEL "Time" FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 32.2 LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 163 BY 15.71
         BGCOLOR 15 FGCOLOR 0 FONT 6 ROW-HEIGHT-CHARS .76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 7.91 COL 51 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 7.91 COL 87 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 7.91 COL 110 HELP
          "CLEAR AUTO FIND Value"
     Btn_Delete_invSnapshot AT ROW 17.24 COL 2 HELP
          "DELETE Inventory Snapshot"
     RECT-4 AT ROW 17 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 17.76
         WIDTH              = 163.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}
{custom/yellowColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "1"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON Btn_Clear_Find IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Delete_invSnapshot:PRIVATE-DATA IN FRAME F-Main     = 
                "panel-image".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.inventorySnapshot"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "inventorySnapshot.company EQ cCompany"
     _FldNameList[1]   > ASI.inventorySnapshot.inventorySnapshotID
"inventorySnapshotID" "ID" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.inventorySnapshot.snapshotDesc
"snapshotDesc" ? "x(40)" "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.inventorySnapshot.snapshotType
"snapshotType" "Type" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.inventorySnapshot.itemType
"itemType" "Item Type" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.inventorySnapshot.warehouseID
"warehouseID" "Warehouse" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.inventorySnapshot.locationID
"locationID" "Location" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.inventorySnapshot.inventoryStockStatus
"inventoryStockStatus" "Stock Status" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.inventorySnapshot.snapshotUser
"snapshotUser" "User" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.inventorySnapshot.snapshotTime
"snapshotTime" "Time" ? "datetime" ? ? ? 14 ? ? no ? no no "32.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
    /* This code displays initial values for newly added or copied rows. */
    {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
    {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
    {methods/template/sortindicator.i}
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        ASSIGN
            cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME
            cColLabel    = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:LABEL
            .
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
    {methods/template/sortindicatorend.i}
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Delete_invSnapshot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Delete_invSnapshot B-table-Win
ON CHOOSE OF Btn_Delete_invSnapshot IN FRAME F-Main /* Delete */
DO: 
    DEFINE VARIABLE hHandle AS HANDLE NO-UNDO.
    
    MESSAGE
        "Are you sure you want to remove this snapshot and all corresponding snapshot data?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
    UPDATE lAnswer AS LOGICAL.
    IF lAnswer THEN DO:
        SESSION:SET-WAIT-STATE("General").
        IF NOT VALID-HANDLE(hHandle) THEN
        RUN Inventory/InventoryProcs.p PERSISTENT SET hHandle.
        FIND CURRENT InventorySnapshot NO-LOCK NO-ERROR.                    
        IF AVAILABLE InventorySnapshot THEN
        RUN Inventory_DeleteInventorySnapshot IN hHandle (ROWID(InventorySnapshot), YES).
        SESSION:SET-WAIT-STATE("").                
    END.
    IF VALID-HANDLE(hHandle) THEN
    DELETE PROCEDURE hHandle.
    {&OPEN-QUERY-Browser-Table}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

{sys/inc/f3help.i}

ASSIGN
    auto_find:HIDDEN      = YES
    browse-order:HIDDEN   = YES
    Btn_Clear_Find:HIDDEN = YES
    .
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

&SCOPED-DEFINE cellColumnDat inventorySnapshot.w 
/*{methods/browsers/setCellColumns.i}*/
/*lAutoSave = YES.                   */

{methods/sortByProc.i "pBySnapshotID" "inventorySnapshot.inventorySnapshotID"}
{methods/sortByProc.i "pByDescription" "inventorySnapshot.snapshotDesc"}
{methods/sortByProc.i "pByType" "inventorySnapshot.snapshotType"}
{methods/sortByProc.i "pByItemType" "inventorySnapshot.itemType"}
{methods/sortByProc.i "pByWarehouse" "inventorySnapshot.warehouseID"}
{methods/sortByProc.i "pByLocation" "inventorySnapshot.locationID"}
{methods/sortByProc.i "pByStockStatus" "inventorySnapshot.inventoryStockStatus"}
{methods/sortByProc.i "pByUser" "inventorySnapshot.snapshotUser"}
{methods/sortByProc.i "pByTime" "inventorySnapshot.snapshotTime"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns B-table-Win 
PROCEDURE move-columns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            {&BROWSE-NAME}:COLUMN-MOVABLE   = lMoveColumn
            {&BROWSE-NAME}:COLUMN-RESIZABLE = lMoveColumn
            lMoveColumn = NOT lMoveColumn
            .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse B-table-Win 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CASE cColumnLabel:
        WHEN "inventorySnapshotID" THEN
            RUN pBySnapshotID.
        WHEN "snapshotDesc" THEN
            RUN pByDescription.
        WHEN "snapshotType" THEN
            RUN pByType.
        WHEN "itemType" THEN
            RUN pByItemType. 
        WHEN "warehouseID" THEN
            RUN pByWarehouse.
        WHEN "locationID" THEN
            RUN pByLocation.    
        WHEN "inventoryStockStatus" THEN
            RUN pByStockStatus.    
        WHEN "snapshotUser" THEN
            RUN pByUser.    
        WHEN "snapshotTime" THEN
            RUN pByTime.    
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "inventorySnapshot"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

    CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
    {src/adm/template/bstates.i}
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

