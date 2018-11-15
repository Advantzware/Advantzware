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

ASSIGN
    cocode = g_company
    locode = g_loc
    .
DEFINE VARIABLE ll-show-zero-bins AS LOG       NO-UNDO.
DEFINE VARIABLE lc-pass-loc       AS CHARACTER NO-UNDO.
{sys/inc/oeinq.i}
 
DEFINE NEW SHARED TEMP-TABLE w-job NO-UNDO
    FIELD job-no-disp AS CHARACTER
    FIELD job-no      LIKE job-hdr.job-no
    FIELD job-no2     LIKE job-hdr.job-no2
    FIELD po-no       LIKE fg-bin.po-no
    FIELD i-no        LIKE job-hdr.i-no
    FIELD j-no        LIKE job-hdr.j-no
    FIELD loc         LIKE fg-bin.loc
    FIELD loc-desc      AS CHARACTER FORMAT "x(20)"
    FIELD loc-bin     LIKE fg-bin.loc-bin
    FIELD tag         LIKE fg-bin.tag
    FIELD lead-days   LIKE itemfg-loc.lead-days FORMAT ">>>"
    FIELD ord-level   LIKE itemfg-loc.ord-level FORMAT ">>>,>>>,>>>.<<<"
    FIELD ord-max     LIKE itemfg-loc.ord-max   FORMAT ">>>,>>>,>>>.<<"
    FIELD ord-min     LIKE itemfg-loc.ord-min   FORMAT ">>>,>>>,>>>.<<<"
    FIELD onHand        AS INTEGER FORMAT "->>,>>>,>>9" LABEL "OnHand"
    FIELD onOrder       AS INTEGER FORMAT "->>,>>>,>>9" LABEL "Jobs/POs"
    FIELD allocated     AS INTEGER FORMAT "->>,>>>,>>9" LABEL "Allocated"
    FIELD backOrder     AS INTEGER FORMAT "->>,>>>,>>9" LABEL "BackOrder"
    FIELD qtyAvailable  AS INTEGER FORMAT "->>,>>>,>>9" LABEL "Available"
    FIELD beg-date      AS DATE    FORMAT "99/99/9999"  LABEL "BeginDate"
        INDEX w-job
            job-no
            job-no2
            loc
            loc-bin
            tag
            .
DEFINE TEMP-TABLE w-jobs LIKE w-job.
DEFINE TEMP-TABLE hold-job LIKE w-job.
DEFINE TEMP-TABLE tt-ids 
    FIELD tt-rowid AS ROWID
    .
DEFINE VARIABLE lv-sort-by     AS CHARACTER INIT "tag" NO-UNDO.
DEFINE VARIABLE lv-sort-by-lab AS CHARACTER INIT "Tag" NO-UNDO.
DEFINE VARIABLE ll-sort-asc    AS LOG       NO-UNDO.
DEFINE VARIABLE li-pallets     AS INTEGER   NO-UNDO.
DEFINE VARIABLE li-qty-pal     AS INTEGER   NO-UNDO.
DEFINE VARIABLE h_w-inqord     AS HANDLE    NO-UNDO.

&SCOPED-DEFINE for-each1    ~
    FOR EACH w-jobs

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
&Scoped-define FIELDS-IN-QUERY-br_table w-jobs.loc w-jobs.loc-desc w-jobs.onHand w-jobs.onOrder w-jobs.allocated w-jobs.backOrder w-jobs.qtyAvailable w-jobs.ord-level w-jobs.ord-min w-jobs.ord-max w-jobs.lead-days    
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
btnAddLocation br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddLocation 
     LABEL "+ Add New Location" 
     SIZE 24 BY 1 TOOLTIP "Click to Add New Location".

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
  ENABLE w-jobs.loc
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 157 BY 22.24
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
     btnAddLocation AT ROW 1 COL 115 HELP
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
         HEIGHT             = 23.19
         WIDTH              = 157.
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
    IF itemfg.q-alloc NE 0 THEN
    RUN oe/w-inqord.w PERSISTENT SET h_w-inqord (ROWID(itemfg), YES).
    IF VALID-HANDLE(h_w-inqord) THEN
    RUN adm-initialize IN h_w-inqord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBinDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBinDetails B-table-Win
ON CHOOSE OF btnBinDetails IN FRAME F-Main /* View Bin Details */
DO:
    {methods/run_link.i "ViewDetail-TARGET" "pViewDetail" "(13)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJobs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJobs B-table-Win
ON CHOOSE OF btnJobs IN FRAME F-Main /* View Jobs */
DO:
    IF NOT AVAILABLE itemfg THEN RETURN NO-APPLY.
    IF itemfg.q-ono NE 0 THEN DO:
        FIND FIRST job-hdr NO-LOCK
             WHERE job-hdr.company EQ itemfg.company
               AND job-hdr.i-no    EQ itemfg.i-no
               AND job-hdr.opened  EQ YES
               AND CAN-FIND(FIRST job
                            WHERE job.company EQ job-hdr.company
                              AND job.job     EQ job-hdr.job
                              AND job.job-no  EQ job-hdr.job-no
                              AND job.job-no2 EQ job-hdr.job-no2)
             NO-ERROR.
        IF AVAILABLE job-hdr THEN 
            RUN jc/w-inqjob.w (ROWID(itemfg), YES).
        ELSE DO:
            FIND FIRST fg-set NO-LOCK
                 WHERE fg-set.company EQ itemfg.company
                   AND fg-set.part-no EQ itemfg.i-no
                 NO-ERROR.
            IF AVAILABLE fg-set THEN
            RUN jc/w-inqjbc.w (ROWID(itemfg), YES).
        END.
    END.          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPO B-table-Win
ON CHOOSE OF btnPO IN FRAME F-Main /* View POs */
DO:
    IF NOT AVAILABLE itemfg THEN RETURN NO-APPLY.
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company   EQ itemfg.company
           AND po-ordl.i-no      EQ itemfg.i-no
           AND po-ordl.item-type EQ NO
           AND po-ordl.opened    EQ YES
           AND CAN-FIND(FIRST po-ord
                        WHERE po-ord.company EQ po-ordl.company
                          AND po-ord.po-no   EQ po-ordl.po-no)
         NO-ERROR.
    IF AVAILABLE po-ordl THEN
    RUN po/w-inqpo.w (ROWID(itemfg), YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table B-table-Win 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:                 /** BUILD JOB WORK FILE **/
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iAlloc      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBack       AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotOnHand  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotOnOrder AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotAlloc   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotBack    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotAvail   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotReOrder   AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE w-jobs.
    EMPTY TEMP-TABLE w-job.
    
    IF NOT AVAILABLE itemfg THEN
        RETURN.
    FIND FIRST oe-ctrl NO-LOCK
         WHERE oe-ctrl.company EQ itemfg.company
         NO-ERROR.

    FOR EACH itemfg-loc NO-LOCK
        WHERE itemfg-loc.company EQ itemfg.company
          AND itemfg-loc.i-no    EQ itemfg.i-no,
        FIRST loc NO-LOCK
        WHERE loc.company EQ itemfg-loc.company
          AND loc.loc     EQ itemfg-loc.loc
        :
        RUN fg/calcqabl.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT iAlloc, OUTPUT iBack). 
        CREATE w-jobs.
        ASSIGN 
            w-jobs.i-no         = itemfg.i-no
            w-jobs.loc          = itemfg-loc.loc    
            w-jobs.lead-days    = itemfg-loc.lead-days
            w-jobs.ord-level    = itemfg-loc.ord-level
            w-jobs.ord-max      = itemfg-loc.ord-max
            w-jobs.ord-min      = itemfg-loc.ord-min
            w-jobs.onHand       = itemfg-loc.q-onh
            w-jobs.onOrder      = itemfg-loc.q-ono
            w-jobs.allocated    = iAlloc
            w-jobs.backOrder    = iBack
            w-jobs.qtyAvailable = w-jobs.onHand
                                + w-jobs.onOrder
                                - w-jobs.allocated
            iTotOnHand          = iTotOnHand  + w-jobs.onHand
            iTotonOrder         = iTotOnOrder + w-jobs.onOrder
            iTotAlloc           = iTotAlloc   + w-jobs.allocated
            iTotBack            = iTotBack    + w-jobs.backOrder
            iTotAvail           = iTotAvail   + w-jobs.qtyAvailable
            iTotReOrder         = iTotReOrder + w-jobs.ord-level
            .
        IF AVAILABLE loc THEN
            w-jobs.loc-desc = loc.dscr.      

        RELEASE w-jobs.
    END. /* each itemfg-loc */
    CREATE w-jobs.
    ASSIGN 
        w-jobs.i-no         = itemfg.i-no
        w-jobs.loc          = "*ALL"
        w-jobs.loc-desc     = "ALL Locations"
        w-jobs.lead-days    = itemfg.lead-days
        w-jobs.ord-level    = itemfg.ord-level
        w-jobs.ord-max      = itemfg.ord-max
        w-jobs.ord-min      = itemfg.ord-min
        w-jobs.onHand       = itemfg.q-onh
        w-jobs.onOrder      = itemfg.q-ono
        w-jobs.allocated    = itemfg.q-alloc
        w-jobs.backOrder    = itemfg.q-back
        w-jobs.qtyAvailable = itemfg.q-avail
        .
    IF iTotAlloc NE itemfg.q-alloc 
        OR iTotOnHand NE itemfg.q-onh 
        OR iTotOnOrder NE itemfg.q-ono
        OR iTotBack NE itemfg.q-back
        OR iTotAvail NE itemfg.q-avail THEN DO:
        CREATE w-jobs.
        ASSIGN 
            w-jobs.i-no         = itemfg.i-no
            w-jobs.loc          = "*UNSP"
            w-jobs.loc-desc     = "Unspecified Locations"
            w-jobs.lead-days    = 0
            w-jobs.ord-level    = 0
            w-jobs.ord-max      = 0
            w-jobs.ord-min      = 0
            w-jobs.onHand       = itemfg.q-onh - iTotOnHand
            w-jobs.onOrder      = itemfg.q-ono - iTotOnOrder
            w-jobs.allocated    = itemfg.q-alloc - iTotAlloc
            w-jobs.backOrder    = itemfg.q-back - iTotBack
            w-jobs.qtyAvailable = itemfg.q-avail - iTotAvail
            .
    END.

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
  
    w-jobs.loc:READ-ONLY IN BROWSE {&browse-name} = YES .
    
    RUN local-open-query.

    IF QUERY br_table:NUM-RESULTS NE ? THEN DO:  
        BROWSE br_table:MOVE-TO-TOP().
        BROWSE br_table:SELECT-FOCUSED-ROW().
        BROWSE br_table:SELECT-ROW(1).
    END.
    APPLY 'entry' TO BROWSE br_table.

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

