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
    FIELD lead-days   LIKE itemfg-loc.lead-days
    FIELD ord-level   LIKE itemfg-loc.ord-level
    FIELD ord-max     LIKE itemfg-loc.ord-max
    FIELD ord-min     LIKE itemfg-loc.ord-min
    FIELD onHand        AS INTEGER FORMAT "->>,>>>,>>9" LABEL "OnHand"
    FIELD onOrder       AS INTEGER FORMAT "->>,>>>,>>9" LABEL "Jobs/POs"
    FIELD allocated     AS INTEGER FORMAT "->>,>>>,>>9" LABEL "Allocated"
    FIELD backOrder     AS INTEGER FORMAT "->>,>>>,>>9" LABEL "BackOrder"
    FIELD qtyAvailable  AS INTEGER FORMAT "->>,>>>,>>9" LABEL "Available"
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
&Scoped-Define ENABLED-OBJECTS br_table btn_onh btn_ono btn_all 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_all 
     LABEL "Alloc to Orders" 
     SIZE 16 BY 1.43.

DEFINE BUTTON btn_onh 
     LABEL "On Hand" 
     SIZE 16 BY 1.43.

DEFINE BUTTON btn_ono 
     LABEL "Job/PO On Ord" 
     SIZE 16 BY 1.43.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 18 BY 4.76
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      w-jobs SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      w-jobs.loc LABEL "Whse" WIDTH 10 LABEL-BGCOLOR 14
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
    WITH NO-ASSIGN NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 139 BY 4.76
         FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     btn_onh AT ROW 1.24 COL 141 WIDGET-ID 2
     btn_ono AT ROW 2.67 COL 141 WIDGET-ID 4
     btn_all AT ROW 4.1 COL 141 WIDGET-ID 6
     RECT-26 AT ROW 1 COL 140 WIDGET-ID 8
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
         HEIGHT             = 4.76
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
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2.

/* SETTINGS FOR RECTANGLE RECT-26 IN FRAME F-Main
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
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
    {src/adm/template/brsentry.i}  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
        {src/adm/template/brsleave.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main
DO:
        DEFINE VARIABLE lh-column     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE lv-column-nam AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lv-column-lab AS CHARACTER NO-UNDO.
  
        ASSIGN
            lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
            lv-column-nam = lh-column:NAME
            lv-column-lab = lh-column:LABEL
            .
        IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.
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
ON VALUE-CHANGED OF br_table IN FRAME F-Main
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


&Scoped-define SELF-NAME btn_all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_all B-table-Win
ON CHOOSE OF btn_all IN FRAME F-Main /* Alloc to Orders */
DO:
  IF itemfg.q-alloc NE 0 THEN RUN oe/w-inqord.w PERSISTENT SET h_w-inqord (ROWID(itemfg), YES).
  IF VALID-HANDLE(h_w-inqord) THEN
    RUN adm-initialize IN h_w-inqord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_onh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_onh B-table-Win
ON CHOOSE OF btn_onh IN FRAME F-Main /* On Hand */
DO:
  IF itemfg.q-onh NE 0 THEN
  RUN fg/w-inqonh.w (ROWID(itemfg), NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ono
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ono B-table-Win
ON CHOOSE OF btn_ono IN FRAME F-Main /* Job/PO On Ord */
DO:
  IF itemfg.q-ono NE 0 THEN DO:
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ itemfg.company
          AND job-hdr.i-no    EQ itemfg.i-no
          AND job-hdr.opened  EQ YES
          AND CAN-FIND(FIRST job WHERE job.company EQ job-hdr.company
                                   AND job.job     EQ job-hdr.job
                                   AND job.job-no  EQ job-hdr.job-no
                                   AND job.job-no2 EQ job-hdr.job-no2)
        NO-LOCK NO-ERROR.
    IF AVAIL job-hdr THEN 
        RUN jc/w-inqjob.w (ROWID(itemfg), YES).
    ELSE DO:
        FIND FIRST fg-set WHERE fg-set.company EQ itemfg.company
                            AND fg-set.part-no EQ itemfg.i-no
                          NO-LOCK NO-ERROR.
        IF AVAIL fg-set THEN
        RUN jc/w-inqjbc.w (ROWID(itemfg), YES).
    END.

    FIND FIRST po-ordl
        WHERE po-ordl.company   EQ itemfg.company
          AND po-ordl.i-no      EQ itemfg.i-no
          AND po-ordl.item-type EQ NO
          AND po-ordl.opened    EQ YES
          AND CAN-FIND(FIRST po-ord WHERE po-ord.company EQ po-ordl.company
                                      AND po-ord.po-no   EQ po-ordl.po-no)
        NO-LOCK NO-ERROR.
    IF AVAIL po-ordl THEN
    RUN po/w-inqpo.w (ROWID(itemfg), YES).
  END.
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
            .
        IF AVAILABLE loc THEN
            w-jobs.loc-desc = loc.dscr.      

        RELEASE w-jobs.
    END. /* each itemfg-loc */
    CREATE w-jobs.
    ASSIGN 
        w-jobs.i-no         = "ALL"
        w-jobs.loc          = "ALL"
        w-jobs.loc-desc     = "ALL Locations"
        w-jobs.lead-days    = ?
        w-jobs.ord-level    = ?
        w-jobs.ord-max      = ?
        w-jobs.ord-min      = ?     
        w-jobs.onHand       = iTotOnHand
        w-jobs.onOrder      = iTotOnOrder
        w-jobs.allocated    = iTotAlloc
        w-jobs.backOrder    = iTotBack
        w-jobs.qtyAvailable = iTotAvail
        .

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

