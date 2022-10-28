&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: browsers\locw.w 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

USING system.SharedConfig. 
     
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{methods/defines/hndldefs.i}
{sys/inc/var.i new shared}
{fg/fullset.i NEW}

ASSIGN
    cocode = g_company
    locode = g_loc
    .
DEFINE VARIABLE ll-show-zero-bins AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lc-pass-loc       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lUnspecified      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFirst            AS LOGICAL   NO-UNDO INIT YES.
DEFINE VARIABLE lCalculate        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iPrevPage         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCurrentPage      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lRecalcOnHand     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lRecalcOnOrder    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lRecalcAllocated  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lRecalcBackOrder  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-col-move        AS LOGICAL   NO-UNDO INIT TRUE.
{sys/inc/oeinq.i}

{methods/defines/w-job.i}
DEFINE TEMP-TABLE w-jobs LIKE w-job.
DEFINE TEMP-TABLE hold-job LIKE w-job.
DEFINE TEMP-TABLE tt-ids 
    FIELD tt-rowid AS ROWID
    .
DEFINE VARIABLE lv-sort-by     AS CHARACTER NO-UNDO INITIAL "Tag".
DEFINE VARIABLE lv-sort-by-lab AS CHARACTER NO-UNDO INITIAL "Tag".
DEFINE VARIABLE ll-sort-asc    AS LOG       NO-UNDO.
DEFINE VARIABLE li-pallets     AS INTEGER   NO-UNDO.
DEFINE VARIABLE li-qty-pal     AS INTEGER   NO-UNDO.
DEFINE VARIABLE h_w-inqord     AS HANDLE    NO-UNDO.
DEFINE VARIABLE cPrintAvailQty AS CHARACTER NO-UNDO INITIAL "2".
DEFINE VARIABLE cFGBinInquiry  AS CHARACTER NO-UNDO.

DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.

DEFINE VARIABLE hdInventoryProcs AS HANDLE NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "FGDefaultQtyDisplay", "I" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).

IF lRecFound THEN do:
    IF INTEGER(cRtnChar) GT 0 AND INTEGER(cRtnChar) LE 4 THEN
        cPrintAvailQty = cRtnChar NO-ERROR. 
    ELSE cPrintAvailQty = "2" .
END.

DEFINE VARIABLE lAccess AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAccessClose AS LOGICAL NO-UNDO.
DEFINE VARIABLE cAccessList AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAccessRelButton AS LOGICAL NO-UNDO .
DEFINE VARIABLE scInstance  AS CLASS system.SharedConfig NO-UNDO.

RUN methods/prgsecur.p
	    (INPUT "loc.",
	     INPUT "ALL", /* based on run, create, update, delete or all */
	     INPUT NO,    /* use the directory in addition to the program */
	     INPUT NO,    /* Show a message if not authorized */
	     INPUT NO,    /* Group overrides user security? */
	     OUTPUT lAccess, /* Allowed? Yes/NO */
	     OUTPUT lAccessClose, /* used in template/windows.i  */
	     OUTPUT cAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */

RUN methods/prgsecur.p
	    (INPUT "w-oerel.",
	     INPUT "ALL", /* based on run, create, update, delete or all */
	     INPUT NO,    /* use the directory in addition to the program */
	     INPUT NO,    /* Show a message if not authorized */
	     INPUT NO,    /* Group overrides user security? */
	     OUTPUT lAccessRelButton, /* Allowed? Yes/NO */
	     OUTPUT lAccessClose, /* used in template/windows.i  */
	     OUTPUT cAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */

RUN Inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs .

&SCOPED-DEFINE for-each1    ~
    FOR EACH w-jobs WHERE ((w-jobs.qtyAvailable NE 0 AND cPrintAvailQty EQ "2" ) ~
     OR (w-jobs.qtyAvailable LT 0 AND cPrintAvailQty EQ "3") OR (w-jobs.qtyAvailable EQ 0 AND cPrintAvailQty EQ "4" ) ~
     OR (cPrintAvailQty EQ "1") )

&SCOPED-DEFINE sortby-log                                               ~
    IF lv-sort-by EQ "loc"           THEN w-job.loc                ELSE ~
    IF lv-sort-by EQ "loc-bin"       THEN w-job.loc-bin            ELSE ~
    STRING(w-job.loc-bin)     

&SCOPED-DEFINE sortby BY w-job.tag

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES itemfg
&Scoped-define FIRST-EXTERNAL-TABLE itemfg


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR itemfg.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES w-jobs

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table w-jobs.loc w-jobs.loc-desc w-jobs.onHand w-jobs.onOrder w-jobs.allocated w-jobs.backOrder w-jobs.qtyAvailable w-jobs.ord-level w-jobs.ord-min w-jobs.ord-max w-jobs.lead-days w-jobs.onHoldQty   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table w-jobs.loc   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table w-jobs
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table w-jobs
&Scoped-define SELF-NAME br_table
&Scoped-define OPEN-QUERY-br_table /* OPEN QUERY {&SELF-NAME} */ /*   FOR EACH w-jobs       */ /*       BY w-jobs.loc.    */       OPEN QUERY {&browse-name}      ~           {&for-each1}.
&Scoped-define TABLES-IN-QUERY-br_table w-jobs
&Scoped-define FIRST-TABLE-IN-QUERY-br_table w-jobs


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br_table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnBinDetails btnJobs btnPO btnAllocated ~
btnRelease btnAddLocation br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddLocation 
     LABEL "+ Add New Location" 
     SIZE 24 BY 1 TOOLTIP "Click to Add New Location". 

DEFINE BUTTON btnRelease 
     LABEL "View Releases" 
     SIZE 17 BY 1 TOOLTIP "Click to View Release".

DEFINE BUTTON btnAllocated 
     LABEL "View Allocated" 
     SIZE 17 BY 1 TOOLTIP "Click to View Allocated".

DEFINE BUTTON btnBinDetails 
     LABEL "View Bin Details" 
     SIZE 24 BY 1 TOOLTIP "Click to View Bin Details".

DEFINE BUTTON btnJobs 
     LABEL "View Jobs" 
     SIZE 17 BY 1 TOOLTIP "Click to View Jobs".

DEFINE BUTTON btnLocationDetails 
     LABEL "View Location Details" 
     SIZE 24 BY 1.

DEFINE BUTTON btnPO 
     LABEL "View POs" 
     SIZE 17 BY 1 TOOLTIP "Click to View POs".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      w-jobs SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      w-jobs.loc LABEL "Whse" WIDTH 10
    w-jobs.loc-desc LABEL "Name"
    w-jobs.onHand
    w-jobs.onOrder
    w-jobs.allocated
    w-jobs.backOrder
    w-jobs.qtyAvailable
    w-jobs.ord-level
    w-jobs.ord-min  
    w-jobs.ord-max
    w-jobs.lead-days
    w-jobs.onHoldQty
  ENABLE w-jobs.loc
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS NO-VALIDATE SIZE 156 BY 20.24
         FONT 0
         TITLE "Location Details".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnLocationDetails AT ROW 1 COL 1 HELP
          "View Location Details" WIDGET-ID 12
     btnBinDetails AT ROW 1 COL 25 HELP
          "View Bin Details" WIDGET-ID 10
     btnJobs AT ROW 1 COL 56 HELP
          "View Jobs" WIDGET-ID 14
     btnPO AT ROW 1 COL 73 HELP
          "View POs" WIDGET-ID 16
     btnAllocated AT ROW 1 COL 90 HELP
          "View Allocated" WIDGET-ID 18
     btnRelease AT ROW 1 COL 107 HELP
          "View Releases" 
     btnAddLocation AT ROW 1 COL 130 HELP
          "Add New Location" WIDGET-ID 20
     br_table AT ROW 1.95 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.itemfg
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 21.19
         WIDTH              = 156.
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
/* BROWSE-TAB br_table btnAddLocation F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2
       br_table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON btnLocationDetails IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
/* OPEN QUERY {&SELF-NAME} */
/*   FOR EACH w-jobs       */
/*       BY w-jobs.loc.    */
      OPEN QUERY {&browse-name}      ~
          {&for-each1}
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON DEFAULT-ACTION OF br_table IN FRAME F-Main /* Location Details */
DO:
    DEFINE VARIABLE lUpdated AS LOGICAL NO-UNDO.
    
    FIND FIRST itemfg-loc NO-LOCK
         WHERE itemfg-loc.company EQ itemfg.company 
           AND itemfg-loc.i-no    EQ itemfg.i-no
           AND itemfg-loc.loc     EQ w-jobs.loc
         NO-ERROR.
    IF AVAILABLE itemfg AND AVAILABLE itemfg-loc THEN DO: 
        RUN fg/fglevels.w (ROWID(itemfg), ROWID(itemfg-loc), OUTPUT lUpdated).
        IF lUpdated THEN 
        RUN dispatch ("open-query").
        ELSE 
        RETURN NO-APPLY.
    END. /* if avail */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main /* Location Details */
DO:
  /* This code displays initial values for newly added or copied rows. */
    {src/adm/template/brsentry.i}  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main /* Location Details */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
        {src/adm/template/brsleave.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main /* Location Details */
DO:
	{methods/template/sortindicator.i} 
    DEFINE VARIABLE lh-column     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-column-nam AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-column-lab AS CHARACTER NO-UNDO.
  
    ASSIGN
        lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
        lv-column-nam = lh-column:NAME
        lv-column-lab = lh-column:LABEL
        .
    IF lv-sort-by EQ lv-column-nam THEN
    ll-sort-asc = NOT ll-sort-asc.
    ELSE
    ASSIGN
        lv-sort-by     = lv-column-nam
        lv-sort-by-lab = lv-column-lab
        .
    APPLY 'END-SEARCH' TO {&BROWSE-NAME}.
    RUN resort-query.
	{methods/template/sortindicatorend.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main /* Location Details */
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
    {src/adm/template/brschnge.i}
    lc-pass-loc = w-jobs.loc:screen-value IN BROWSE {&browse-name}.
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "container-source", OUTPUT char-hdl).
    IF VALID-HANDLE(HANDLE(char-hdl)) THEN
        RUN set-loc IN HANDLE(char-hdl) (INPUT lc-pass-loc) NO-ERROR.
    PUBLISH "SelectReorder" (INPUT lc-pass-loc).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRelease
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRelease B-table-Win
ON CHOOSE OF btnRelease IN FRAME F-Main /* View Releases */
DO:
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO .
    DEFINE VARIABLE lRecordFound AS LOGICAL NO-UNDO.
    IF AVAIL w-jobs THEN
       cLocation =  w-jobs.loc .
   
    RUN pCheckRelease(INPUT cLocation, OUTPUT lRecordFound).                                     
    
    IF lRecordFound THEN
    RUN oeinq/b-relinfo.w(ROWID(itemfg), cLocation).
    ELSE DO:
       scInstance = SharedConfig:instance.
       scInstance:SetValue("Item",TRIM(STRING(itemfg.i-no))).
       scInstance:SetValue("Loc",TRIM(STRING(cLocation))).
       RUN displayMessage("68").    
    END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnAddLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddLocation B-table-Win
ON CHOOSE OF btnAddLocation IN FRAME F-Main /* + Add New Location */
DO:
    DEFINE VARIABLE lLocationAdded AS LOGICAL NO-UNDO.
    
    lLocationAdded = NO.
    IF AVAILABLE itemfg THEN
    RUN windows/addfgloc.w (ROWID(itemfg), OUTPUT lLocationAdded).
    IF lLocationAdded THEN 
    RUN dispatch ('open-query').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAllocated
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAllocated B-table-Win
ON CHOOSE OF btnAllocated IN FRAME F-Main /* View Allocated */
DO:
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO .
    IF AVAIL w-jobs THEN
       cLocation =  w-jobs.loc .
       
    IF itemfg.q-alloc EQ 0 THEN
    do:
       scInstance = SharedConfig:instance.
       scInstance:SetValue("Item",TRIM(STRING(itemfg.i-no))).
       scInstance:SetValue("Loc",TRIM(STRING(cLocation))).
       RUN displayMessage("68").
    END.
    IF itemfg.q-alloc NE 0 THEN
        IF cFGBinInquiry EQ "Yes" THEN
        RUN AOA/dynGrid.p (13,
            "company^" + itemfg.company +
            "|fgItem^" + itemfg.i-no
            ).
        ELSE DO:
           RUN oeinq/b-ordinfo.w(ROWID(itemfg), cLocation).
        END. /* else */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBinDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBinDetails B-table-Win
ON CHOOSE OF btnBinDetails IN FRAME F-Main /* View Bin Details */
DO:
    {methods/run_link.i "ViewDetail-TARGET" "pViewDetail" "(14)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJobs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJobs B-table-Win
ON CHOOSE OF btnJobs IN FRAME F-Main /* View Jobs */
DO:
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO .
    IF AVAIL w-jobs THEN
       cLocation =  w-jobs.loc .
    IF NOT AVAILABLE itemfg THEN RETURN NO-APPLY.   
      
   
        FIND FIRST job-hdr NO-LOCK
             WHERE job-hdr.company EQ itemfg.company
               AND job-hdr.i-no    EQ itemfg.i-no
               AND job-hdr.opened  EQ YES
               AND CAN-FIND(FIRST job
                            WHERE job.company EQ job-hdr.company
                              AND job.job     EQ job-hdr.job
                              AND job.job-no  EQ job-hdr.job-no
                              AND job.job-no2 EQ job-hdr.job-no2
                              AND job.opened EQ YES
                              AND (job.loc EQ cLocation OR job.shipFromLocation EQ cLocation OR cLocation EQ "*All")
                              )
             NO-ERROR.
        IF AVAILABLE job-hdr THEN
        DO:        
            IF cFGBinInquiry EQ "Yes" THEN
            RUN AOA/dynGrid.p (15,
                "company^" + itemfg.company +
                "|fgItem^" + itemfg.i-no
                ).
            ELSE
              RUN jcinq/b-jobinfo.w(ROWID(itemfg),cLocation).                   
        END.
        ELSE DO:
              scInstance = SharedConfig:instance.
              scInstance:SetValue("Item",TRIM(STRING(itemfg.i-no))).
              scInstance:SetValue("Loc",TRIM(STRING(cLocation))).
              RUN displayMessage("68").
        END.              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPO B-table-Win
ON CHOOSE OF btnPO IN FRAME F-Main /* View POs */
DO:
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO .
    IF AVAIL w-jobs THEN
       cLocation =  w-jobs.loc .
    IF NOT AVAILABLE itemfg THEN RETURN NO-APPLY.
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company   EQ itemfg.company
           AND po-ordl.i-no      EQ itemfg.i-no
           AND po-ordl.item-type EQ NO
           AND lookup(po-ordl.stat, "o,p,u,a") > 0
           AND CAN-FIND(FIRST po-ord
                        WHERE po-ord.company EQ po-ordl.company
                          AND po-ord.po-no   EQ po-ordl.po-no 
                          AND (po-ord.loc EQ cLocation OR cLocation EQ "*All" )
                          AND lookup(po-ord.stat, "N,O,R,U,H") > 0)
         NO-ERROR.
    IF AVAILABLE po-ordl THEN
    do:
        IF cFGBinInquiry EQ "Yes" THEN
        RUN AOA/dynGrid.p (14,
            "company^" + itemfg.company +
            "|fgItem^" + itemfg.i-no
            ).
        ELSE
        /*RUN po/w-inqpo.w (ROWID(itemfg), YES).*/
         RUN poinq/b-poinfo.w (ROWID(itemfg), cLocation).
    END.
    ELSE      
    DO:
       scInstance = SharedConfig:instance.
       scInstance:SetValue("Item",TRIM(STRING(itemfg.i-no))).
       scInstance:SetValue("Loc",TRIM(STRING(cLocation))).
       RUN displayMessage("68").     
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
{methods/ctrl-a_browser.i}

&SCOPED-DEFINE cellColumnDat locw.w 
{methods/browsers/setCellColumns.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}
{methods/build-table.i}

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
  {src/adm/template/row-list.i "itemfg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "itemfg"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE apply-arrow B-table-Win 
PROCEDURE apply-arrow :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    APPLY 'down-arrow' TO BROWSE br_table.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-q-avail B-table-Win 
PROCEDURE calc-q-avail :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /*DO WITH FRAME {&FRAME-NAME}:
    itemfg.q-avail:SCREEN-VALUE = STRING(DEC(itemfg.q-onh:SCREEN-VALUE) +
                                         DEC(itemfg.q-ono:SCREEN-VALUE) -
                                         DEC(itemfg.q-alloc:SCREEN-VALUE),
                                         itemfg.q-avail:FORMAT).
  END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-qty B-table-Win 
PROCEDURE calc-qty :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  def var char-hdl as cha no-undo.

  IF AVAIL itemfg THEN
    run fg/d-reqtys.w (ROWID(itemfg), yes).

  run get-link-handle in adm-broker-hdl (this-procedure, "repo-query-source", output char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
  run repo-query in widget-handle(char-hdl) (ROWID(itemfg)).

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
DEFINE VARIABLE lcItemFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcItemTo   AS CHAR NO-UNDO.
DEFINE VARIABLE lcLocFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcLocTo   AS CHAR NO-UNDO.
 
IF AVAIL itemfg THEN
    ASSIGN
        lcItemFrom = itemfg.i-no
        lcItemTo = itemfg.i-no .
IF AVAIL w-jobs THEN
    ASSIGN
        lcLocFrom = w-jobs.loc
        lcLocTo = w-jobs.loc.

    RUN fg/rd-locwexp.w (lcItemFrom,
                       lcItemTo,
                       lcLocFrom,
                       lcLocTo
                       ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterLocMain B-table-Win 
PROCEDURE filterLocMain :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcShowRec AS CHARACTER   NO-UNDO.
  ASSIGN cPrintAvailQty = ipcShowRec .

  RUN dispatch ("open-query"). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
    
    /* Code placed here will execute PRIOR to standard behavior. */
    RUN sys/ref/nk1look.p (
        g_company,"FGBinInquiry","L",NO,NO,"","",
        OUTPUT cFGBinInquiry, OUTPUT lFound
        ).

    /* Dispatch standard ADM method.                             */
    RUN setCellColumns.
    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

    w-jobs.loc:READ-ONLY IN BROWSE {&browse-name} = YES .
    
    IF QUERY br_table:NUM-RESULTS NE ? THEN DO:  
        BROWSE br_table:MOVE-TO-TOP() NO-ERROR.
        BROWSE br_table:SELECT-FOCUSED-ROW() NO-ERROR.
        BROWSE br_table:SELECT-ROW(1) NO-ERROR.
    END.

    IF NOT lAccess THEN
        btnAddLocation:SENSITIVE IN FRAME {&FRAME-NAME} = NO .
    IF NOT lAccessRelButton THEN
         btnRelease:SENSITIVE IN FRAME {&FRAME-NAME} = NO .

    APPLY "FOCUS":U TO btnBinDetails IN FRAME {&FRAME-NAME}.
    /*
    APPLY "ENTRY":U TO BROWSE br_table.
    */

    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    {methods/winReSizeLocInit.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-rowid AS ROWID     NO-UNDO.
    DEFINE VARIABLE ll-zero  AS LOG       INIT YES NO-UNDO.

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN build-table.
  
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

/* Code placed here will execute AFTER standard behavior.    */
    IF iCurrentpage EQ 15
        THEN lfirst = NO.
    RUN pGetPOLocation.
    RUN pCheckUnspecified(
        OUTPUT lUnspecified,
        OUTPUT lRecalcOnHand,
        OUTPUT lRecalcOnOrder,
        OUTPUT lRecalcAllocated,
        OUTPUT lRecalcBackOrder
        ).
    OPEN QUERY {&browse-name} {&for-each1}.    
    IF lFirst AND iCurrentpage NE 15 AND lUnspecified THEN DO:
        RUN pDisplayRecalculateMsg(
            INPUT lRecalcOnHand,
            INPUT lRecalcOnOrder,
            INPUT lRecalcAllocated,
            INPUT lRecalcBackOrder
            ).
        OPEN QUERY {&browse-name} {&for-each1}.    
        lFirst = NO.  
    END. 
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
      {&BROWSE-NAME}:COLUMN-MOVABLE = v-col-move
         {&BROWSE-NAME}:COLUMN-RESIZABLE = v-col-move
        v-col-move = NOT v-col-move.
     /*   FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
     DISPLAY FI_moveCol.*/
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win
PROCEDURE local-view:
/*------------------------------------------------------------------------------
 Purpose: If unspecified location exists then show unspecified location pop up message
          after displaying the records
 Notes:
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN GET-ATTRIBUTE IN adm-broker-hdl ('PreviousPage'). 
  iPrevPage = INTEGER(RETURN-VALUE).
  RUN GET-ATTRIBUTE IN adm-broker-hdl ('CurrentPage'). 
  iCurrentPage = INTEGER(RETURN-VALUE).
  IF NOT lFirst AND iprevPage EQ 1 THEN DO: 
    RUN pCheckUnspecified(
        OUTPUT lUnspecified,
        OUTPUT lRecalcOnHand,
        OUTPUT lRecalcOnOrder,
        OUTPUT lRecalcAllocated,
        OUTPUT lRecalcBackOrder
        ).
      IF lUnspecified THEN 
        RUN pDisplayRecalculateMsg(
            INPUT lRecalcOnHand,
            INPUT lRecalcOnOrder,
            INPUT lRecalcAllocated,
            INPUT lRecalcBackOrder
            ). 
      OPEN QUERY {&browse-name} {&for-each1}. 
  END.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE override-qty B-table-Win 
PROCEDURE override-qty :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckRelease B-table-Win
PROCEDURE pCheckRelease:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipLocation       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplReturnRelease AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER oe-ordl FOR oe-ordl.
    DEFINE BUFFER oe-ord  FOR oe-ord.
    DEFINE BUFFER oe-rel  FOR oe-rel.

    MAIN-LOOP:
    FOR EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ cocode          
          AND oe-ordl.opened  EQ YES 
          AND oe-ordl.i-no    EQ itemfg.i-no 
          AND (oe-ordl.stat   NE "C"
           OR  oe-ordl.stat   EQ ""), 
        FIRST oe-ord NO-LOCK                             
        WHERE oe-ord.company EQ oe-ordl.company     
          AND oe-ord.ord-no  EQ oe-ordl.ord-no     
          AND oe-ord.opened  EQ YES,             
        EACH oe-rel NO-LOCK
        WHERE oe-rel.company       EQ oe-ordl.company   
          AND oe-rel.ord-no        EQ oe-ordl.ord-no                 
          AND oe-rel.i-no          EQ oe-ordl.i-no                     
          AND oe-rel.line          EQ oe-ordl.line                     
          AND (oe-rel.spare-char-1 EQ ipLocation
           OR  ipLocation          EQ "*All" ) 
          AND LOOKUP(oe-rel.s-code,"B,S") NE 0
          AND LOOKUP(oe-rel.stat,"S,A,L,B,Z") NE 0
        :
        oplReturnRelease = YES.
        LEAVE MAIN-LOOP.
    END.    

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckUnspecified B-table-Win
PROCEDURE pCheckUnspecified PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: To check and delete the unspecified location record from the workfile 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplUnspecified      AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecalcOnHand     AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecalcOnOrder    AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecalcAllocated  AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecalcBackOrder  AS LOGICAL NO-UNDO.

    FIND FIRST w-jobs NO-LOCK
         WHERE w-jobs.i-no EQ itemfg.i-no
           AND w-jobs.loc  EQ "*UNSP"
            NO-ERROR.
    IF AVAILABLE w-jobs THEN DO:        
        IF w-jobs.onHand           EQ 0      
           AND w-jobs.onOrder      EQ 0
           AND w-jobs.allocated    EQ 0               
           AND w-jobs.backOrder    EQ 0 
           AND w-jobs.qtyAvailable EQ 0 THEN DO:  
                  
           DELETE w-jobs.
           
           ASSIGN 
               oplRecalcOnHand    = NO
               oplRecalcOnOrder   = NO
               oplRecalcAllocated = NO
               oplRecalcBackOrder = NO    
               oplUnspecified     = NO
               .
        END. 
        ELSE IF w-jobs.onHand       EQ 0 
            AND w-jobs.onOrder      EQ 0
            AND w-jobs.allocated    EQ 0
            AND w-jobs.backOrder    EQ 0
            AND w-jobs.qtyAvailable NE 0 THEN 
            ASSIGN 
               oplRecalcOnHand    = YES
               oplRecalcOnOrder   = YES
               oplRecalcAllocated = YES
               oplRecalcBackOrder = YES
               oplUnspecified     = YES
               .  
        ELSE
            ASSIGN 
               oplRecalcOnHand    = w-jobs.onHand       NE 0
               oplRecalcOnOrder   = w-jobs.onOrder      NE 0
               oplRecalcAllocated = w-jobs.allocated    NE 0
               oplRecalcBackOrder = w-jobs.backOrder    NE 0   
               oplUnspecified     = YES
               .
    END.               
        
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCorrectQuantities B-table-Win
PROCEDURE pCorrectQuantities PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: To Put the the Unspecified location quantities into correct location
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcItem          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocation      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiReceivedQty   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiQuanityonOrd  AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-w-jobs FOR w-jobs.
    
    FIND FIRST bf-w-jobs NO-LOCK
         WHERE bf-w-jobs.i-no EQ ipcItem 
           AND bf-w-jobs.loc  EQ ipcLocation 
            NO-ERROR.
    IF AVAILABLE bf-w-jobs THEN DO: 
        IF ipiQuanityonOrd  LT 0 THEN
            ASSIGN 
                bf-w-jobs.onOrder      = bf-w-jobs.onOrder       - ipiReceivedQty 
                bf-w-jobs.qtyavailable = bf-w-jobs.qtyavailable  - ipiReceivedQty
                . 
        ELSE
            ASSIGN
                bf-w-jobs.onOrder      = bf-w-jobs.onOrder       + ipiReceivedQty 
                bf-w-jobs.qtyavailable = bf-w-jobs.qtyavailable  + ipiReceivedQty 
                .                                
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayRecalculateMsg B-table-Win
PROCEDURE pDisplayRecalculateMsg PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: To Display a message box to recalculate the quantity
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplRecalcOnHand    AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplRecalcOnOrder   AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplRecalcAllocated AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplRecalcBackOrder AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-itemfg FOR itemfg.
        
    RUN displayMessageQuestionLOG(
        INPUT  "20", 
        OUTPUT lCalculate
        ).
        
    IF lCalculate THEN DO:
        RUN Inventory_AddLocFromHist IN hdInventoryProcs(
            INPUT ROWID(itemfg),
            INPUT cocode
            ).
        RUN Inventory_RecalculateQuantities IN hdInventoryProcs(
            INPUT ROWID(itemfg),
            INPUT iplRecalcOnHand,   
            INPUT iplRecalcOnOrder,
            INPUT iplRecalcAllocated,
            INPUT iplRecalcBackOrder
            ).    
        IF itemfg.isaset THEN DO:
            FOR EACH tt-fg-set NO-LOCK:
                DELETE tt-fg-set.
            END.
             
            RUN fg/fullset.p(
                INPUT ROWID(itemfg)
                ).   
            FOR EACH tt-fg-set,
                FIRST bf-itemfg NO-LOCK 
                WHERE bf-itemfg.company EQ cocode 
                  AND bf-itemfg.i-no    EQ tt-fg-set.part-no:  
                RUN fg/fg-rst2.p(
                    INPUT RECID(bf-itemfg)
                    ).
            END.
        END.    
        RUN build-table.
        RUN pGetPOLocation.
        RUN pCheckUnspecified(
            OUTPUT lUnspecified,
            OUTPUT iplRecalcOnHand,
            OUTPUT iplRecalcOnOrder,
            OUTPUT iplRecalcAllocated,
            OUTPUT iplRecalcBackOrder
            ).
        IF lUnspecified THEN
            RUN displayMessage(
                INPUT "21"
                ). 
    END.     
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetPOLocation B-table-Win
PROCEDURE pGetPOLocation PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: To delete the unspecified POs locations and show them into corrrect 
          locations
 Notes:
------------------------------------------------------------------------------*/    
    FIND FIRST w-jobs NO-LOCK
         WHERE w-jobs.loc EQ "*UNSP" 
         NO-ERROR.
    IF AVAILABLE w-jobs AND w-jobs.onorder NE 0 THEN DO:
        FOR EACH po-ordl NO-LOCK
            WHERE po-ordl.company                 EQ cocode
              AND po-ordl.i-no                    EQ w-jobs.i-no
              AND lookup(po-ordl.stat, "O,P,U,A") GT 0  
              AND po-ordl.item-type               EQ NO 
              AND po-ordl.t-rec-qty               NE 0, 
            FIRST po-ord NO-LOCK
            WHERE po-ord.company                   EQ po-ordl.company 
              AND po-ord.po-no                     EQ po-ordl.po-no 
              AND lookup(po-ord.stat, "N,O,R,U,H") GT 0:
            IF w-jobs.onOrder LT 0 THEN DO:
                RUN pCorrectQuantities(
                    INPUT w-jobs.i-no,
                    INPUT po-ord.loc,
                    INPUT po-ordl.t-rec-qty,
                    INPUT w-jobs.onOrder
                    ).
                ASSIGN
                    w-jobs.onorder      = w-jobs.onorder      + po-ordl.t-rec-qty
                    w-jobs.qtyAvailable = w-jobs.qtyAvailable + po-ordl.t-rec-qty
                    . 
                    
            END.         
            ELSE IF w-jobs.onOrder GT 0 THEN DO:
                RUN pCorrectQuantities(
                    INPUT w-jobs.i-no, 
                    INPUT po-ord.loc ,
                    INPUT po-ordl.t-rec-qty,
                    INPUT w-jobs.onOrder
                    ).
                ASSIGN 
                    w-jobs.onOrder      = w-jobs.onOrder      - po-ordl.t-rec-qty
                    w-jobs.qtyAvailable = w-jobs.qtyAvailable - po-ordl.t-rec-qty
                    .
            END.           
        END.      
    END.  
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resort-query B-table-Win 
PROCEDURE resort-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE open-query          ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               
        
    RUN dispatch ("row-changed").
   
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
  {src/adm/template/snd-list.i "itemfg"}
  {src/adm/template/snd-list.i "w-jobs"}

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

