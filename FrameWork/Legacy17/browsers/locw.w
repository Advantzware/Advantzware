&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: browsers\fgijob.w 

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

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

DEF VAR ll-show-zero-bins AS LOG NO-UNDO.
DEF VAR lc-pass-loc AS CHAR NO-UNDO.
{sys/inc/oeinq.i}
 
def new shared temp-table w-job no-undo
  field job-no-disp as char
  field job-no like job-hdr.job-no
  field job-no2 like job-hdr.job-no2
  FIELD po-no LIKE fg-bin.po-no
  field i-no like job-hdr.i-no
  field j-no like job-hdr.j-no
  field loc like fg-bin.loc
  FIELD loc-desc AS CHAR FORMAT "x(20)"
  field loc-bin like fg-bin.loc-bin
  field tag like fg-bin.tag
  FIELD lead-days LIKE itemfg-loc.lead-days
  FIELD ord-level LIKE itemfg-loc.ord-level
  FIELD ord-max   LIKE itemfg-loc.ord-max
  FIELD ord-min   LIKE itemfg-loc.ord-min
 /* FIELD cust-no LIKE fg-bin.cust-no
  FIELD cases AS INT
  field case-count like fg-bin.case-count
  field cases-unit like fg-bin.cases-unit
  field qty as int format "->>>,>>9"
  field std-tot-cost like  job-hdr.std-tot-cost
  field std-mat-cost like  job-hdr.std-mat-cost
  field std-lab-cost like  job-hdr.std-lab-cost
  field std-var-cost like  job-hdr.std-var-cost
  field std-fix-cost like  job-hdr.std-fix-cost
  field last-cost like fg-bin.last-cost
  field sell-uom like itemfg.sell-uom
  FIELD partial-count LIKE fg-bin.partial-count
  field rel-qty as int format "->>>,>>9"
  field bol-qty as int format "->>>,>>9"
  field avl-qty as int format "->>>,>>9"
  FIELD tot-wt like fg-bin.tot-wt  */
  INDEX w-job job-no job-no2 loc loc-bin tag.

def temp-table w-jobs LIKE w-job.

def temp-table hold-job LIKE w-job.

DEF TEMP-TABLE tt-ids FIELD tt-rowid AS ROWID.

DEF VAR lv-sort-by AS CHAR INIT "tag" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Tag" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR li-pallets AS INT NO-UNDO.
DEF VAR li-qty-pal AS INT NO-UNDO.

/*-sort-by = NOT oeinq.*/

&SCOPED-DEFINE for-each1    ~
    FOR EACH w-jobs
     
    /*~
      BY (IF oeinq THEN w-job.job-no ELSE "") DESC
      BY (IF oeinq THEN w-job.job-no2 ELSE 0) DESC
      BY w-job.job-no
      BY w-job.job-no2
      BY w-job.loc
      BY w-job.loc-bin
      BY w-job.tag.                          
    */


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
/*
DO TRANSACTION:
   {sys\inc\fgsecur.i}
END.
*/

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
&Scoped-define FIELDS-IN-QUERY-br_table w-jobs.loc w-jobs.loc-desc w-jobs.ord-level w-jobs.ord-min w-jobs.ord-max w-jobs.lead-days   
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
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      w-jobs SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      w-jobs.loc label "Whse" WIDTH 10                     LABEL-BGCOLOR 14
      w-jobs.loc-desc LABEL "Name"
      w-jobs.ord-level
      w-jobs.ord-min  
      w-jobs.ord-max
      w-jobs.lead-days

     
ENABLE w-jobs.loc
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS NO-SCROLLBAR-VERTICAL SIZE 117 BY 8.57
         FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
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
         HEIGHT             = 8.95
         WIDTH              = 118.4.
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
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.

  
  ASSIGN
   lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

  ELSE
    ASSIGN
     lv-sort-by     = lv-column-nam
     lv-sort-by-lab = lv-column-lab.

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  /*APPLY "choose" TO btn_go.*/
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
  lc-pass-loc = w-jobs.loc:screen-value in browse {&browse-name}.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "container-source", OUTPUT char-hdl).
  IF VALID-HANDLE(HANDLE(char-hdl)) THEN
      RUN set-loc IN HANDLE(char-hdl) (INPUT lc-pass-loc) NO-ERROR.
  PUBLISH "SelectReorder" (INPUT lc-pass-loc).
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
  EMPTY TEMP-TABLE w-jobs.
  EMPTY TEMP-TABLE w-job.
  IF NOT AVAIL itemfg THEN
      RETURN.
  FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ itemfg.company NO-LOCK NO-ERROR.

  for each itemfg-loc
      where itemfg-loc.company eq itemfg.company
        and itemfg-loc.i-no    eq itemfg.i-no
      no-lock:
      FIND loc WHERE loc.company EQ itemfg-loc.company
                 AND loc.loc     EQ itemfg-loc.loc
               NO-LOCK NO-ERROR.
    
      create w-jobs.
      assign w-jobs.i-no  = itemfg.i-no
             w-jobs.loc  = itemfg-loc.loc    
             w-jobs.lead-days = itemfg-loc.lead-days
             w-jobs.ord-level = itemfg-loc.ord-level
             w-jobs.ord-max   = itemfg-loc.ord-max
             w-jobs.ord-min   = itemfg-loc.ord-min             
             .
      IF AVAIL loc THEN
          w-jobs.loc-desc = loc.dscr.
      

       RELEASE w-jobs.
  end. /* each itemfg-loc */
  CREATE w-jobs.
  ASSIGN w-jobs.i-no = "ALL"
         w-jobs.loc = "ALL"
         w-jobs.loc-desc = "ALL Locations"
         w-jobs.lead-days = itemfg.lead-days
         w-jobs.ord-level = itemfg.ord-level
         w-jobs.ord-max   = itemfg.ord-max
         w-jobs.ord-min   = itemfg.ord-min     
         .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-table B-table-Win 
PROCEDURE create-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  
  DEF INPUT PARAM ip-lab LIKE job-hdr.std-lab-cost NO-UNDO.
  DEF INPUT PARAM ip-mat LIKE job-hdr.std-mat-cost NO-UNDO.
  DEF INPUT PARAM ip-var LIKE job-hdr.std-var-cost NO-UNDO.
  DEF INPUT PARAM ip-fix LIKE job-hdr.std-fix-cost NO-UNDO.

  IF NOT CAN-FIND(FIRST w-jobs
                  WHERE w-jobs.job-no  EQ job.job-no
                    AND w-jobs.job-no2 EQ job.job-no2) THEN DO:
    CREATE w-jobs.
    ASSIGN
     w-jobs.i-no         = itemfg.i-no
     w-jobs.loc          = job.loc
     w-jobs.std-lab-cost = ip-lab
     w-jobs.std-mat-cost = ip-mat
     w-jobs.std-var-cost = ip-var
     w-jobs.std-fix-cost = ip-fix
     w-jobs.std-tot-cost = w-jobs.std-lab-cost + w-jobs.std-mat-cost +
                           w-jobs.std-var-cost + w-jobs.std-fix-cost
     w-jobs.sell-uom     = "M"  .
  END.
  */
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
  
  ASSIGN 
         w-jobs.loc:READ-ONLY IN BROWSE {&browse-name} = YES .
  RUN local-open-query.
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

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
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ll-zero AS LOG INIT YES NO-UNDO.


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
             
  /*
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
    */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-read-only B-table-Win 
PROCEDURE set-read-only :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-log AS LOG NO-UNDO.
/*
  DO WITH FRAME {&FRAME-NAME}:
     w-jobs.loc:READ-ONLY IN BROWSE {&browse-name} = ip-log.
     
  END. */
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

