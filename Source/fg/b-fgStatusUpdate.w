&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: fg/b-fgStatusUpdate.w

  Description: Browser to view/enquire statusID of FG Bins

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
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

{api/inbound/ttItem.i}
{custom/globdefs.i}
{inventory/ttInventory.i "NEW SHARED"}.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cItemDescDisp    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobIDDisp       AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE lColMove         AS LOGICAL   NO-UNDO.

RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.

DEF STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttItem

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 inventoryStockID quantity PoID WareHouseID LocationID tagStatus StatusDescription onHold PrimaryID ItemDesc fGetJobID() @ JobNo customerID   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttItem
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttItem.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttItem
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttItem


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiItemId fiPOID fiJobID fiJobID2 ~
fiWarehouseID fiLocationID fiTag fiStatusID rsOnHold tbZeroQty btSearch ~
BROWSE-2 RECT-1 RECT-32 
&Scoped-Define DISPLAYED-OBJECTS fiItemId fiPOID fiJobID fiJobID2 ~
fiWarehouseID fiLocationID fiTag fiStatusID rsOnHold tbZeroQty 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS>
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetJobID B-table-Win 
FUNCTION fGetJobID RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btSearch 
     IMAGE-UP FILE "Graphics/32x32/magnifying_glass.ico":U
     LABEL "Search" 
     SIZE 11 BY 2.62.

DEFINE BUTTON btUpdate 
     LABEL "Update" 
     SIZE 25 BY 1.52.

DEFINE BUTTON btUpdateAll 
     LABEL "Update All" 
     SIZE 25 BY 1.52.

DEFINE VARIABLE fiItemId AS CHARACTER FORMAT "x(15)" 
     LABEL "Item #" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE fiJobID AS CHARACTER FORMAT "X(6)":U 
     LABEL "Job #" 
     VIEW-AS FILL-IN 
     SIZE 16.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiJobID2 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fiLocationID AS CHARACTER FORMAT "X(256)":U 
     LABEL "Location" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiPOID AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     LABEL "PO #" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiStatusID AS CHARACTER FORMAT "X(4)":U 
     LABEL "Status ID" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiTag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tag #" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE fiWarehouseID AS CHARACTER FORMAT "X(5)":U 
     LABEL "Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE rsOnHold AS CHARACTER INITIAL "3" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Both", "3",
"On Hold", "1",
"Not On Hold", "2"
     SIZE 52 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 176 BY 2.86.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 53.4 BY 2.05.

DEFINE VARIABLE tbZeroQty AS LOGICAL INITIAL no 
     LABEL "NoTag/Qty=0" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttItem SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 B-table-Win _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      inventoryStockID   COLUMN-LABEL "Tag #"            WIDTH 28 FORMAT "X(30)"
      quantity            COLUMN-LABEL "Quantity"         WIDTH 16 FORMAT "->>>>>>>>9"
      PoID                COLUMN-LABEL "PO #"             WIDTH 12 FORMAT ">>>>>>>>>"
      WareHouseID         COLUMN-LABEL "Warehouse"        WIDTH 14
      LocationID          COLUMN-LABEL "Location"         WIDTH 10
      tagStatus           COLUMN-LABEL "Tag Status"       WIDTH 12
      StatusDescription   COLUMN-LABEL "Tag Description"  WIDTH 30 FORMAT "X(50)"
      onHold              COLUMN-LABEL "On Hold"          WIDTH 9  FORMAT "Yes/No"
      PrimaryID           COLUMN-LABEL "Item #"           WIDTH 20 FORMAT "X(30)"
      ItemDesc            COLUMN-LABEL "Item Description" WIDTH 30 FORMAT "X(50)"
      fGetJobID() @ JobNo COLUMN-LABEL "Job #"            WIDTH 12 
      customerID          COLUMN-LABEL "Customer"         WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 176 BY 18.33
         FONT 6 ROW-HEIGHT-CHARS .76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiItemId AT ROW 1.29 COL 10 COLON-ALIGNED HELP
          "Enter F/G Item Number" WIDGET-ID 34
     fiPOID AT ROW 1.29 COL 51.2 COLON-ALIGNED WIDGET-ID 42
     fiJobID AT ROW 1.29 COL 76.8 COLON-ALIGNED WIDGET-ID 36
     fiJobID2 AT ROW 1.29 COL 94 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     fiWarehouseID AT ROW 1.29 COL 115.2 COLON-ALIGNED WIDGET-ID 48
     fiLocationID AT ROW 1.29 COL 141.6 COLON-ALIGNED WIDGET-ID 40
     fiTag AT ROW 2.67 COL 10 COLON-ALIGNED WIDGET-ID 44
     fiStatusID AT ROW 2.67 COL 56 COLON-ALIGNED WIDGET-ID 46
     rsOnHold AT ROW 2.67 COL 79 NO-LABEL WIDGET-ID 52
     tbZeroQty AT ROW 2.67 COL 144 WIDGET-ID 60
     btSearch AT ROW 1.24 COL 166 WIDGET-ID 32
     BROWSE-2 AT ROW 4.1 COL 2 WIDGET-ID 200
     btUpdate AT ROW 22.91 COL 63 WIDGET-ID 62
     btUpdateAll AT ROW 22.91 COL 89 WIDGET-ID 64
     "-" VIEW-AS TEXT
          SIZE 1 BY .62 AT ROW 1.43 COL 95 WIDGET-ID 56
     RECT-1 AT ROW 1.1 COL 2 WIDGET-ID 50
     RECT-32 AT ROW 22.67 COL 61.6 WIDGET-ID 66
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
         HEIGHT             = 23.95
         WIDTH              = 177.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Size-to-Fit Custom                                        */
/* BROWSE-TAB BROWSE-2 btSearch F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btUpdate IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btUpdateAll IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttItem.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 B-table-Win
ON DEFAULT-ACTION OF BROWSE-2 IN FRAME F-Main
DO:
    RUN pUpdateBinStatus.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 B-table-Win
ON VALUE-CHANGED OF BROWSE-2 IN FRAME F-Main
DO:
    ASSIGN
        btUpdate:SENSITIVE    = AVAILABLE ttItem
        btUpdateAll:SENSITIVE = AVAILABLE ttitem
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fiLocationID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocationID B-table-Win
ON HELP OF fiLocationID IN FRAME F-Main /* Location */
DO:
    DEF VAR char-val AS cha NO-UNDO.
    
    RUN windows/l-fgbin.w (g_company,fiWarehouseID:screen-value, fiLocationID:screen-value,OUTPUT char-val).
    IF char-val <> "" THEN 
    DO :
        ASSIGN 
        fiLocationID:SCREEN-VALUE = ENTRY(1,char-val)  .    
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSearch B-table-Win
ON CHOOSE OF btSearch IN FRAME F-Main /* Search */
DO:
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOnHold  AS LOGICAL   NO-UNDO.
    
    SESSION:SET-WAIT-STATE ("GENERAL").
    
    IF rsOnHold:SCREEN-VALUE EQ "1" THEN
        lOnHold = TRUE.
    ELSE IF rsOnHold:SCREEN-VALUE EQ "2" THEN
        lOnHold = FALSE.
    ELSE
        lOnHold = ?.

    EMPTY TEMP-TABLE ttItem.

    RUN pLoadFGBinData (
        INPUT  g_company,
        INPUT  fiItemID:SCREEN-VALUE,
        INPUT  INTEGER(fiPOID:SCREEN-VALUE),
        INPUT  fiJobID:SCREEN-VALUE,
        INPUT  INTEGER(fiJobID2:SCREEN-VALUE),
        INPUT  fiWarehouseID:SCREEN-VALUE,
        INPUT  fiLocationID:SCREEN-VALUE,
        INPUT  fiTag:SCREEN-VALUE,
        INPUT  NOT tbZeroQty:CHECKED,
        INPUT  NOT tbZeroQty:CHECKED,
        INPUT  fiStatusID:SCREEN-VALUE,
        INPUT  lOnHold,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).

    SESSION:SET-WAIT-STATE ("").

    IF NOT lSuccess OR cMessage NE "" THEN
        MESSAGE cMessage
            VIEW-AS ALERT-BOX ERROR.
            
    RUN dispatch IN THIS-PROCEDURE (
        INPUT 'open-query':U
        ).
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdate B-table-Win
ON CHOOSE OF btUpdate IN FRAME F-Main /* Update */
DO:
    RUN pUpdateBinStatus.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdateAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdateAll B-table-Win
ON CHOOSE OF btUpdateAll IN FRAME F-Main /* Update All */
DO:
    RUN pUpdateAllBinStatus.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
/*{methods/browsers/setCellColumns.i}*/

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    /* Commenting the below code intentionally as it will reset the displayed
       object (widgets) values to initial value on every call to open-query */
    /* RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) . */

    /* Code placed here will execute AFTER standard behavior.    */

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
            lColMove                        = NOT lColMove
            {&BROWSE-NAME}:COLUMN-MOVABLE   = lColMove
            {&BROWSE-NAME}:COLUMN-RESIZABLE = lColMove
            .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadFGBinData B-table-Win 
PROCEDURE pLoadFGBinData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoID        AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobID       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobID2      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouseID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocationID  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplEmptyTag    AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplZeroQty     AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStatusID    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplOnHold      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.
        
    RUN api\inbound\GetInventoryDetails.p (
        INPUT  ipcCompany, 
        INPUT  ipcWareHouseID,
        INPUT  ipcLocationID, 
        INPUT  ipcTag,
        INPUT  ipcItemID,
        INPUT  ipcJobID,
        INPUT  ipiJobID2,
        INPUT  "",     /* Customer ID */
        INPUT  ipiPoID,
        INPUT  iplZeroQty,
        INPUT  iplEmptyTag,
        INPUT  ipcStatusID,
        INPUT  iplOnHold,
        INPUT  "FG",
        OUTPUT oplSuccess,
        OUTPUT opcMessage,
        OUTPUT TABLE ttItem
        ).  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateAllBinStatus B-table-Win 
PROCEDURE pUpdateAllBinStatus PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lUpdateStatus   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturnStatusID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lReturnOnHold   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttItem FOR ttItem.
    
    IF AVAILABLE ttItem THEN DO:
        RUN fg/d-fgStatusUpdate (
            INPUT  "",            /* Bin Status ID */
            INPUT  NO,            /* On Hold */
            OUTPUT lUpdateStatus,
            OUTPUT cReturnStatusID,
            OUTPUT lReturnOnHold
            ).
    
        IF lUpdateStatus THEN DO:
            FOR EACH bf-ttItem:
                IF bf-ttItem.TagStatus NE cReturnStatusID THEN DO:
                    RUN Inventory_UpdateFGBinStatusID IN hdInventoryProcs (
                        INPUT  bf-ttItem.sourceRowID,
                        INPUT  cReturnStatusID,
                        OUTPUT lSuccess,
                        OUTPUT cMessage
                        ).
                    IF NOT lSuccess THEN DO:
                        MESSAGE cMessage
                            VIEW-AS ALERT-BOX ERROR.
                        RETURN.
                    END.
                    
                    bf-ttItem.TagStatus = cReturnStatusID.
                END.
                
                IF bf-ttItem.OnHold NE lReturnOnHold THEN DO:
                    RUN Inventory_UpdateFGBinOnHold IN hdInventoryProcs (
                        INPUT  bf-ttItem.sourceRowID,
                        INPUT  lReturnOnHold,
                        OUTPUT lSuccess,
                        OUTPUT cMessage
                        ).
                    IF NOT lSuccess THEN DO:
                        MESSAGE cMessage
                            VIEW-AS ALERT-BOX ERROR.
                        RETURN.
                    END.
                    
                    bf-ttItem.onHold = lReturnOnHold.
                END.
                
                RUN Inventory_GetStatusDescription IN hdInventoryProcs (
                    INPUT  bf-ttItem.tagStatus,
                    OUTPUT bf-ttItem.StatusDescription
                    ).            
            END.

            RUN dispatch IN THIS-PROCEDURE (
                INPUT 'open-query':U
                ) .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateBinStatus B-table-Win 
PROCEDURE pUpdateBinStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lUpdateStatus   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturnStatusID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lReturnOnHold   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
    
    IF AVAILABLE ttItem THEN DO:
        RUN fg/d-fgStatusUpdate.w (
            INPUT  ttItem.TagStatus,
            INPUT  ttItem.onHold,
            OUTPUT lUpdateStatus,
            OUTPUT cReturnStatusID,
            OUTPUT lReturnOnHold
            ).
    
        IF lUpdateStatus THEN DO:
            IF ttItem.TagStatus NE cReturnStatusID THEN DO:
                RUN Inventory_UpdateFGBinStatusID IN hdInventoryProcs (
                    INPUT  ttItem.sourceRowID,
                    INPUT  cReturnStatusID,
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ).
                IF NOT lSuccess THEN DO:
                    MESSAGE cMessage
                        VIEW-AS ALERT-BOX ERROR.
                    RETURN.
                END.
                
                ttItem.TagStatus = cReturnStatusID.
            END.
            
            IF ttItem.OnHold NE lReturnOnHold THEN DO:
                RUN Inventory_UpdateFGBinOnHold IN hdInventoryProcs (
                    INPUT  ttItem.sourceRowID,
                    INPUT  lReturnOnHold,
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ).
                IF NOT lSuccess THEN DO:
                    MESSAGE cMessage
                        VIEW-AS ALERT-BOX ERROR.
                    RETURN.
                END.
                
                ttItem.onHold = lReturnOnHold.
            END.

            RUN Inventory_GetStatusDescription IN hdInventoryProcs (
                INPUT  ttItem.tagStatus,
                OUTPUT ttItem.StatusDescription
                ).
                            
            RUN dispatch IN THIS-PROCEDURE (
                INPUT 'open-query':U
                ) .
        END.
    END.
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
  {src/adm/template/snd-list.i "ttItem"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(50)" INITIAL "c:~\tmp~\FgStatusInquiry.csv" NO-UNDO.
  DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .
  DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cExcelDisplay AS CHARACTER NO-UNDO.

  RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

  OUTPUT STREAM excel TO VALUE(cFileName).
  excelheader = "Tag#,Quantity,PO#,Warehouse,Location,Tag Status,Tag Description,On Hold,Item#,Item Description,Job#,Customer".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.

       FOR EACH ttItem NO-LOCK:
             cExcelDisplay = "".
             cExcelDisplay = cExcelDisplay + quoter(ttItem.inventoryStockID) + ",".
             cExcelDisplay = cExcelDisplay + quoter(ttItem.quantity) + ",".
             cExcelDisplay = cExcelDisplay + quoter(ttItem.PoID) + ",".
             cExcelDisplay = cExcelDisplay + quoter(ttItem.WareHouseID) + ",".
             cExcelDisplay = cExcelDisplay + quoter(ttItem.LocationID) + ",".
             cExcelDisplay = cExcelDisplay + quoter(ttItem.tagStatus) + ",".
             cExcelDisplay = cExcelDisplay + quoter(ttItem.StatusDescription) + ",".
             cExcelDisplay = cExcelDisplay + quoter(ttItem.onHold) + ",".
             cExcelDisplay = cExcelDisplay + quoter(ttItem.PrimaryID) + ",".
             cExcelDisplay = cExcelDisplay + quoter(ttItem.ItemDesc) + ",".
             cExcelDisplay = cExcelDisplay + quoter(STRING(ttItem.jobNo + "-" + TRIM(STRING(ttItem.jobNo2,">9")))) + ",".
             cExcelDisplay = cExcelDisplay + quoter(ttItem.customerID) + ",".
             
             PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
             
       END.
            
  OUTPUT STREAM excel CLOSE.   
  OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetJobID B-table-Win 
FUNCTION fGetJobID RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    IF ttItem.jobNo NE "" THEN
        RETURN ttItem.jobNo + "-" + TRIM(STRING(ttItem.jobNo2,">9")).
    ELSE
        RETURN "".
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

