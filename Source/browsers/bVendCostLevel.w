&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/bVendCostLevel.w

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

/*&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}*/

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}

{custom/b-ebfgDefs.i}

DEFINE VARIABLE lVendItemUseDeviation AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lRecFound             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cRtnChar              AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFirstRow             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFirst                AS LOGICAL   NO-UNDO INIT YES.
DEFINE VARIABLE hdVendorCostProcs     AS HANDLE    NO-UNDO.

RUN sys/ref/nk1look.p (INPUT g_company, "VendItemUseDeviation", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lVendItemUseDeviation = LOGICAL(cRtnChar) NO-ERROR.


RUN system\VendorCostProcs.p PERSISTENT SET hdVendorCostProcs.    

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES vendItemCost
&Scoped-define FIRST-EXTERNAL-TABLE vendItemCost


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR vendItemCost.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES vendItemCostLevel

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table vendItemCostLevel.quantityFrom vendItemCostLevel.quantityTo vendItemCostLevel.costPerUom vendItemCostLevel.costSetup vendItemCostLevel.costDeviation vendItemCostLevel.leadTimeDays  
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table    
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table vendItemCostLevel
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table vendItemCostLevel
&Scoped-define SELF-NAME Browser-Table
&Scoped-define QUERY-STRING-Browser-Table FOR EACH vendItemCostLevel WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID NO-LOCK BY vendItemCostLevel.quantityBase
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY {&SELF-NAME} FOR EACH vendItemCostLevel WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID NO-LOCK  BY vendItemCostLevel.quantityBase.
&Scoped-define TABLES-IN-QUERY-Browser-Table vendItemCostLevel
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table vendItemCostLevel


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS  Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS  Browser-Table

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fIsBestCost B-table-Win
FUNCTION fIsBestCost RETURNS CHARACTER PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME







/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */


/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      vendItemCostLevel SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _FREEFORM
  QUERY Browser-Table NO-LOCK DISPLAY
      vendItemCostLevel.quantityFrom COLUMN-LABEL "From"     FORMAT "->>>>>>>>>9.999999":U WIDTH 20
      vendItemCostLevel.quantityTo   COLUMN-LABEL "Up To"   FORMAT "->>>>>>>>>9.999999":U WIDTH 20
      vendItemCostLevel.costPerUom                        COLUMN-LABEL "Cost Per" FORMAT "->>>>>>9.99":U WIDTH 15
      fIsBestCost()                                       COLUMN-LABEL "B"        FORMAT "X(1)":U        WIDTH 2 
      vendItemCostLevel.costSetup                         COLUMN-LABEL "Setup"    FORMAT "->>>>>>9.9<":U WIDTH 15      
      vendItemCostLevel.costDeviation                     COLUMN-LABEL "Devi"     FORMAT "->>>>>>9.9<":U WIDTH 15
      vendItemCostLevel.leadTimeDays                      COLUMN-LABEL "Lead"     FORMAT "->>>>>>9.9<":U WIDTH 15
      vendItemCostLevel.useForBestCost                    COLUMN-LABEL "Sel"      FORMAT "Y/N":U         WIDTH 5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-ROW-MARKERS SEPARATORS SIZE 86 BY 10.91
         FONT 2 ROW-HEIGHT-CHARS .76 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: asi.vendItemCost
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
         HEIGHT             = 9.1
         WIDTH              = 61.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table lv_i-coat-p F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "7"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       Browser-Table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.
IF NOT lVendItemUseDeviation THEN
    ASSIGN 
      vendItemCostLevel.costDeviation:VISIBLE IN BROWSE Browser-Table = FALSE.


/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH vendItemCostLevel WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID NO-LOCK  BY vendItemCostLevel.quantityBase.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,,"
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
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main /* title value set in defs section (browseTitle) */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  /*btnSave:LABEL = '&Save'.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF Browser-Table IN FRAME F-Main
DO:
   
   DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO .
   DEFINE VARIABLE lAllowUpdate AS LOGICAL NO-UNDO .
   DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

    run get-link-handle in adm-broker-hdl  (this-procedure,"bottom-TARGET", output char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
       RUN vendcost-newitem IN WIDGET-HANDLE(char-hdl) (OUTPUT lAllowUpdate).

   IF NOT lAllowUpdate THEN
   IF AVAIL vendItemCost AND AVAIL vendItemCostLevel THEN do:
       lFirstRow = BROWSE {&BROWSE-NAME}:FOCUSED-ROW EQ 1.
       
       RUN viewers/dVendCostLevel.w(
       INPUT  ROWID(vendItemCost),
       INPUT  ROWID(vendItemCostLevel),
       INPUT  "UPDATE",
       INPUT  lFirstRow, /* IF YES do not change quantityFrom */
       OUTPUT lv-Rowid
       ). 
       
       RUN reopen-query (ROWID(vendItemCost),ROWID(vendItemCostLevel)).
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main /* title value set in defs section (browseTitle) */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main /* title value set in defs section (browseTitle) */
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{methods/ctrl-a_browser.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

//{methods/winReSize.i}

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "vendItemCost"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "vendItemCost"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ebfgProcedures B-table-Win 
PROCEDURE ebfgProcedures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/*{custom/b-ebfg.i itemfg}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  {methods/winReSizeLocInit.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win
PROCEDURE local-view:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  lFirst = YES.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */



END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query B-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipRowidLevel AS ROWID NO-UNDO.

  FIND FIRST vendItemCost NO-LOCK
      WHERE vendItemCost.company EQ cocode 
        AND rowid(vendItemCost) EQ ip-rowid NO-ERROR .
  lFirst = YES.
  
  {&OPEN-QUERY-Browser-Table}

  IF ipRowidLevel NE ? THEN
  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO ROWID ipRowidLevel NO-ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 /* RUN ebfgBuild. /* found in custom/b-ebfg.i */*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO .
DEFINE VARIABLE cReturnMessage AS CHARACTER NO-UNDO .
DEFINE VARIABLE rwRowidLevel AS ROWID NO-UNDO .

DEFINE VARIABLE lAllowUpdate AS LOGICAL NO-UNDO .
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

    run get-link-handle in adm-broker-hdl  (this-procedure,"bottom-TARGET", output char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
       RUN vendcost-newitem IN WIDGET-HANDLE(char-hdl) (OUTPUT lAllowUpdate).

IF NOT lAllowUpdate THEN do:

  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.
  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 IF AVAIL vendItemCost THEN
     RUN RecalculateFromAndTo IN hdVendorCostProcs (vendItemCost.vendItemCostID, OUTPUT lReturnError ,OUTPUT cReturnMessage ) .

IF AVAIL vendItemCost THEN
    RUN reopen-query (ROWID(vendItemCost),rwRowidLevel).
END. /* NOT lAllowUpdate*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE price-change V-table-Win 
PROCEDURE price-change :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE dPercentage as DECIMAL no-undo.   
   DEFINE VARIABLE rwRowid AS ROWID NO-UNDO.
   DEFINE VARIABLE ip-parms     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE op-values    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lError       AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
   
   DEFINE BUFFER bf-vendItemCostLevel for vendItemCostLevel. 
   
   status default "Processing Vendor Cost Matrix: " + string(vendItemCost.itemID).
   
   if AVAIL vendItemCostLevel then
   rwRowid = rowid(vendItemCostLevel).

   dPercentage = 0.
   
   ip-parms = 
    /* price percentage */
    "|type=literal,name=label6,row=3.2,col=20,enable=false,width=44,scrval=" + "By what percentage:" + ",FORMAT=x(60)"
    + "|type=fill-in,name=perprice,row=3,col=42,enable=true,width=14,data-type=decimal,initial=" + STRING(dPercentage)
    + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true,width=12,height=3 " 
    /* Box Title */
    + "|type=win,name=fi3,enable=true,label=  Update Price?,FORMAT=X(30),height=9".

    RUN custom/d-prompt.w (INPUT "", ip-parms, "", OUTPUT op-values).
    IF op-values NE "" THEN
        dPercentage = INTEGER(ENTRY(2, op-values)) . 
    
    IF op-values NE "" THEN
       IF ENTRY(4, op-values) EQ "Cancel" THEN RETURN NO-APPLY .
    IF dPercentage EQ 0 THEN RETURN NO-APPLY. 
   
   RUN Vendor_VendItemCostWithPercentage(INPUT ROWID(vendItemCost), INPUT dPercentage, OUTPUT lError, OUTPUT cMessage).
     
   RUN reopen-query (ROWID(vendItemCost),rwRowid).
  
   
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
  {src/adm/template/snd-list.i "vendItemCostLevel"}
  
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCopyRecord B-table-Win 
PROCEDURE pCopyRecord :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
 DEFINE VARIABLE rwRowid AS ROWID NO-UNDO .
 DEFINE VARIABLE lAllowUpdate AS LOGICAL NO-UNDO .
 DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

    run get-link-handle in adm-broker-hdl  (this-procedure,"bottom-TARGET", output char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
       RUN vendcost-newitem IN WIDGET-HANDLE(char-hdl) (OUTPUT lAllowUpdate).

    IF NOT lAllowUpdate THEN
    IF AVAIL vendItemCost AND AVAIL vendItemCostLevel THEN do:
       lFirstRow = BROWSE {&BROWSE-NAME}:FOCUSED-ROW EQ 1.
       
       RUN viewers/dVendCostLevel.w(
       INPUT  ROWID(vendItemCost),
       INPUT  ROWID(vendItemCostLevel),
       INPUT  "Copy",
       INPUT  lFirstRow, /* IF YES do not change quantityFrom */
       OUTPUT rwRowid
       ).
       RUN reopen-query (ROWID(vendItemCost),rwRowid).
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddRecord B-table-Win 
PROCEDURE pAddRecord :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO .
    DEFINE VARIABLE rwRowid AS ROWID NO-UNDO .
    DEFINE VARIABLE lAllowUpdate AS LOGICAL NO-UNDO .
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

    run get-link-handle in adm-broker-hdl  (this-procedure,"bottom-TARGET", output char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
       RUN vendcost-newitem IN WIDGET-HANDLE(char-hdl) (OUTPUT lAllowUpdate).

    IF NOT lAllowUpdate THEN
    IF AVAIL vendItemCost  THEN do:
       RUN viewers/dVendCostLevel.w(
           INPUT  ROWID(vendItemCost),
           INPUT  lv-rowid,
           INPUT  "Create", 
           INPUT  NO,  /* Do not change quantity From for add record */
           OUTPUT rwRowid) .
       
       RUN reopen-query (ROWID(vendItemCost),rwRowid).
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRecord B-table-Win 
PROCEDURE pUpdateRecord :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
 DEFINE VARIABLE rwRowid AS ROWID NO-UNDO .
 DEFINE VARIABLE lAllowUpdate AS LOGICAL NO-UNDO .
 DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

    run get-link-handle in adm-broker-hdl  (this-procedure,"bottom-TARGET", output char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
       RUN vendcost-newitem IN WIDGET-HANDLE(char-hdl) (OUTPUT lAllowUpdate).

    IF NOT lAllowUpdate THEN
    IF AVAIL vendItemCost AND AVAIL vendItemCostLevel THEN DO:
       lFirstRow = BROWSE {&BROWSE-NAME}:FOCUSED-ROW EQ 1.
       
       RUN viewers/dVendCostLevel.w(
           INPUT  ROWID(vendItemCost),
           INPUT  ROWID(vendItemCostLevel),
           INPUT  "UPDATE",
           INPUT  lFirstRow, /* If yes, Dont change the quantityFrom */
           OUTPUT rwRowid
           ).
       
       RUN reopen-query (ROWID(vendItemCost),rwRowid).
   END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pViewRecord B-table-Win 
PROCEDURE pViewRecord :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
 DEFINE VARIABLE rwRowid AS ROWID NO-UNDO .
 DEFINE VARIABLE lAllowUpdate AS LOGICAL NO-UNDO .
 DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

    run get-link-handle in adm-broker-hdl  (this-procedure,"bottom-TARGET", output char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
       RUN vendcost-newitem IN WIDGET-HANDLE(char-hdl) (OUTPUT lAllowUpdate).

    IF NOT lAllowUpdate THEN
    IF AVAIL vendItemCost THEN DO:
       lFirstRow = BROWSE {&BROWSE-NAME}:FOCUSED-ROW EQ 1.
       
       RUN viewers/dVendCostLevel.w(
           INPUT  ROWID(vendItemCost),
           INPUT  ROWID(vendItemCostLevel),
           INPUT  "view",
           INPUT  lFirstRow, /* If yes, Dont change the quantityFrom */
           OUTPUT rwRowid
           ).

       /*RUN reopen-query (ROWID(vendItemCost)).*/
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fIsBestCost B-table-Win
FUNCTION fIsBestCost RETURNS CHARACTER PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF vendItemCostLevel.useForBestCost THEN 
        RETURN "*".
    ELSE 
        RETURN "".  
END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


