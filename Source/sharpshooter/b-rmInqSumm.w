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

  File:  sharpshooter/b-rmInqSumm.w

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE lHasAccess AS LOGICAL NO-UNDO.
DEFINE VARIABLE iWarehouseLength AS INTEGER   NO-UNDO.
&SCOPED-DEFINE exclude-brwCustom
{Inventory/ttBrowseInventory.i}
{methods/defines/sortByDefs.i}
{sys/inc/var.i}

DEFINE VARIABLE hdInventoryProcs AS HANDLE NO-UNDO.
RUN Inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.

DEFINE VARIABLE hdPgmSecurity AS HANDLE  NO-UNDO.
RUN system/PgmMstrSecur.p PERSISTENT SET hdPgmSecurity.

RUN epCanAccess IN hdPgmSecurity (
    INPUT  "sharpshooter/b-rmInqSumm.w", 
    INPUT  "", 
    OUTPUT lHasAccess
    ).
    
DELETE OBJECT hdPgmSecurity.    

&SCOPED-DEFINE SORTBY-PHRASE BY ttBrowseInventory.warehouseID

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME ttBrowseInventory

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttBrowseInventory

/* Definitions for BROWSE ttBrowseInventory                             */
&Scoped-define FIELDS-IN-QUERY-ttBrowseInventory ttBrowseInventory.quantityOnHand ttBrowseInventory.quantityOnOrder ttBrowseInventory.quantityAllocated ttBrowseInventory.quantityAvailable ttBrowseInventory.quantityBackOrder ttBrowseInventory.emptyColumn   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttBrowseInventory   
&Scoped-define SELF-NAME ttBrowseInventory
&Scoped-define QUERY-STRING-ttBrowseInventory FOR EACH ttBrowseInventory ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-ttBrowseInventory OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-ttBrowseInventory ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-ttBrowseInventory ttBrowseInventory


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ttBrowseInventory 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetConcatJob B-table-Win 
FUNCTION fGetConcatJob RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetConcatLocation B-table-Win 
FUNCTION fGetConcatLocation RETURNS CHARACTER PRIVATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ttBrowseInventory FOR 
      ttBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE ttBrowseInventory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttBrowseInventory B-table-Win _FREEFORM
  QUERY ttBrowseInventory DISPLAY
      ttBrowseInventory.quantityOnHand WIDTH 35 COLUMN-LABEL "Qty On-Hand" FORMAT "->,>>>,>>>,>>9" LABEL-BGCOLOR 14
    ttBrowseInventory.quantityOnOrder WIDTH 35 COLUMN-LABEL "Qty On-Order" FORMAT "->,>>>,>>>,>>9" LABEL-BGCOLOR 14
    ttBrowseInventory.quantityAllocated WIDTH 35 COLUMN-LABEL "Qty Committed" FORMAT "->,>>>,>>>,>>9" LABEL-BGCOLOR 14
    ttBrowseInventory.quantityAvailable WIDTH 35 COLUMN-LABEL "Qty Available" FORMAT "->,>>>,>>>,>>9" LABEL-BGCOLOR 14
    ttBrowseInventory.quantityBackOrder WIDTH 35 COLUMN-LABEL "Qty BackOrder" FORMAT "->,>>>,>>>,>>9" LABEL-BGCOLOR 14
    ttBrowseInventory.emptyColumn COLUMN-LABEL ""
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-SCROLLBAR-VERTICAL NO-TAB-STOP SIZE 179 BY 26.91
         FONT 36 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ttBrowseInventory AT ROW 1 COL 1 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


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
         HEIGHT             = 27.05
         WIDTH              = 179.2.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB ttBrowseInventory 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       ttBrowseInventory:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttBrowseInventory
/* Query rebuild information for BROWSE ttBrowseInventory
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE ttBrowseInventory */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME ttBrowseInventory
&Scoped-define SELF-NAME ttBrowseInventory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttBrowseInventory B-table-Win
ON START-SEARCH OF ttBrowseInventory IN FRAME F-Main
DO:
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
        
        IF cColumnLabel EQ cSaveLabel THEN
            lAscending = NOT lAscending.
        IF VALID-HANDLE(hSaveLabel) THEN
            hSaveLabel:LABEL-BGCOLOR = ?.
    
        ASSIGN
            hColumnLabel               = {&BROWSE-NAME}:CURRENT-COLUMN
            hColumnLabel:LABEL-BGCOLOR = 14
            hSaveLabel                 = hColumnLabel
            cSaveLabel                 = cColumnLabel
            .
        RUN pReopenBrowse.
    END.
    
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttBrowseInventory B-table-Win
ON VALUE-CHANGED OF ttBrowseInventory IN FRAME F-Main
DO:
    //btAdjustQty:SENSITIVE = AVAILABLE ttBrowseInventory.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{methods/template/brwcustomSharpShooter.i}
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{sharpshooter/smartobj/browseNavigate.i}

&Scoped-define sdBrowseName ttBrowseInventory
{methods/sortByProc.i "pByQuantityOnHand" "ttBrowseInventory.quantityOnHand"}
{methods/sortByProc.i "pByQuantityOnOrder" "ttBrowseInventory.quantityOnOrder"}
{methods/sortByProc.i "pByQuantityBackOrder" "ttBrowseInventory.quantityBackOrder"}
{methods/sortByProc.i "pByQuantityAvailable" "ttBrowseInventory.quantityAvailable"}
{methods/sortByProc.i "pByQuantityAllocated" "ttBrowseInventory.quantityAllocated"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy B-table-Win
PROCEDURE local-destroy:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(hdInventoryProcs) THEN
  DELETE PROCEDURE hdInventoryProcs.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
        WHEN "quantityOnHand" THEN
            RUN pByQuantityOnHand.
        WHEN "quantityOnOrder" THEN
            RUN pByQuantityOnOrder.
        WHEN "quantityBackOrder" THEN
            RUN pByQuantityBackOrder.
        WHEN "quantityAvailable" THEN
            RUN pByQuantityAvailable.
        WHEN "quantityAllocated" THEN
            RUN pByQuantityAllocated.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.

    IF AVAILABLE ttBrowseInventory THEN
        APPLY "VALUE-CHANGED":U TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScanItem B-table-Win 
PROCEDURE ScanItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttBrowseInventory.

    RUN Inventory_BuildRMBinSummaryForItem IN hdInventoryProcs (
        INPUT  ipcCompany,
        INPUT  ipcItemID,
        OUTPUT oplError,
        OUTPUT opcMessage,
        INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
        ).
    
    IF oplError THEN
        MESSAGE opcMessage VIEW-AS ALERT-BOX ERROR.

    {&OPEN-QUERY-{&BROWSE-NAME}}

    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
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
  {src/adm/template/snd-list.i "ttBrowseInventory"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetConcatJob B-table-Win 
FUNCTION fGetConcatJob RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cConcatJob AS CHARACTER NO-UNDO.
       
    IF AVAILABLE ttBrowseInventory AND ttBrowseInventory.jobID NE "" THEN DO:
        cConcatJob = ttBrowseInventory.jobID 
                   + FILL(" ", iJobLen - LENGTH(ttBrowseInventory.jobID)) 
                   + "-"
                   + STRING(ttBrowseInventory.jobID2,"999").
    END.
    
    RETURN cConcatJob.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetConcatLocation B-table-Win 
FUNCTION fGetConcatLocation RETURNS CHARACTER PRIVATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cConcatLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompany         AS CHARACTER NO-UNDO.
    
    RUN spGetSessionParam ("Company", OUTPUT cCompany).

    RUN Inventory_GetWarehouseLength IN hdInventoryProcs (
        INPUT  cCompany,
        OUTPUT iWarehouseLength
        ).   
    
    IF AVAILABLE ttBrowseInventory THEN
        cConcatLocation = ttBrowseInventory.warehouseID 
                        + FILL(" ", iWarehouseLength - LENGTH(ttBrowseInventory.warehouseID)) 
                        + ttBrowseInventory.locationID.

    RETURN cConcatLocation.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

