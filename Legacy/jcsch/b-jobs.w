&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

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



DEF VAR lv-die-no LIKE eb.die-no NO-UNDO.
DEF VAR lv-board LIKE ef.board NO-UNDO.
DEF VAR lv-po-no LIKE po-ord.po-no NO-UNDO.
DEF VAR lv-color AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-due-date AS DATE NO-UNDO.
DEF VAR lv-mr-stime AS cha NO-UNDO.
DEF VAR lv-mr-etime AS cha NO-UNDO.
DEF VAR lv-run-stime AS cha NO-UNDO.
DEF VAR lv-run-etime AS cha NO-UNDO.
DEF VAR lv-mr-hr AS cha NO-UNDO.
DEF VAR lv-run-hr AS cha NO-UNDO.

DEF BUFFER bf-job-mch FOR job-mch.
DEF BUFFER bf-job-hdr FOR job-hdr.
DEF BUFFER bf-job FOR job.

DEF TEMP-TABLE tt-sch FIELD rec-id AS RECID
                      FIELD seq-no LIKE job-mch.seq-no
                      FIELD board LIKE ef.board
                      FIELD die LIKE   eb.die-no
                      FIELD plate LIKE eb.plate-no
                      FIELD colour AS cha EXTENT 10
                      FIELD START-date AS DATE FORM "99/99/9999"
                      .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES mach
&Scoped-define FIRST-EXTERNAL-TABLE mach


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mach.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-sch job-mch job-hdr

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table tt-sch.seq-no job-mch.job-no job-mch.job-no2 tt-sch.start-date /* display-due-date() @ lv-due-date display-board() @ lv-board display-eb() @ lv-die-no display-color() @ lv-color display-po-no() @ lv-po-no */ job-mch.start-date-su cvt-time-to-string('',job-mch.start-time-su,0.00) @ lv-mr-stime job-mch.end-date-su cvt-time-to-string('END',job-mch.start-time-su,job-mch.mr-hr) @ lv-mr-etime cvt-hour-to-string(job-mch.mr-hr) @ lv-mr-hr job-mch.start-date cvt-time-to-string('',job-mch.start-time,0.00) @ lv-run-stime job-mch.end-date cvt-time-to-string('END',job-mch.start-time,job-mch.run-hr) @ lv-mr-etime cvt-hour-to-string(job-mch.run-hr) @ lv-run-hr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table tt-sch.seq-no ~
job-mch.start-date-su ~
job-mch.end-date-su ~
job-mch.start-date ~
job-mch.end-date   
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table tt-sch job-mch
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table tt-sch
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table job-mch
&Scoped-define SELF-NAME Browser-Table
&Scoped-define QUERY-STRING-Browser-Table FOR EACH tt-sch, ~
               EACH job-mch WHERE RECID(job-mch) = tt-sch.rec-id NO-LOCK, ~
               FIRST job-hdr WHERE job-hdr.company = job-mch.company                       AND job-hdr.job = job-mch.job NO-LOCK                       BY tt-sch.start-date BY tt-sch.seq
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY {&SELF-NAME}     FOR EACH tt-sch, ~
               EACH job-mch WHERE RECID(job-mch) = tt-sch.rec-id NO-LOCK, ~
               FIRST job-hdr WHERE job-hdr.company = job-mch.company                       AND job-hdr.job = job-mch.job NO-LOCK                       BY tt-sch.start-date BY tt-sch.seq.
&Scoped-define TABLES-IN-QUERY-Browser-Table tt-sch job-mch job-hdr
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table tt-sch
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table job-mch
&Scoped-define THIRD-TABLE-IN-QUERY-Browser-Table job-hdr


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-start-date btn-go Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS lv-start-date 

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
</FOREIGN-KEYS
><EXECUTING-CODE>
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cvt-hour-to-string B-table-Win 
FUNCTION cvt-hour-to-string RETURNS CHARACTER
  ( INPUT ip-hour AS dec  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cvt-time-to-string B-table-Win 
FUNCTION cvt-time-to-string RETURNS CHARACTER
  (INPUT ip-type AS CHAR, INPUT ip-stime AS INT, INPUT ip-hour AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-board B-table-Win 
FUNCTION display-board RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-color B-table-Win 
FUNCTION display-color RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-die B-table-Win 
FUNCTION display-die RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-due-date B-table-Win 
FUNCTION display-due-date RETURNS DATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-po-no B-table-Win 
FUNCTION display-po-no RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-go 
     LABEL "Go" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-start-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      tt-sch, 
      job-mch, 
      job-hdr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _FREEFORM
  QUERY Browser-Table NO-LOCK DISPLAY
      tt-sch.seq-no COLUMN-LABEL "Seq#" FORMAT ">>9":U
      job-mch.job-no COLUMN-LABEL "Job #" FORMAT "x(6)":U
      job-mch.job-no2 COLUMN-LABEL "" FORMAT ">9":U
      tt-sch.start-date FORMAT "99/99/9999":U
  /*    display-due-date() @ lv-due-date COLUMN-LABEL "Due Date"
      display-board() @ lv-board COLUMN-LABEL "Board"
      display-eb() @ lv-die-no COLUMN-LABEL "Die#"
      display-color() @ lv-color COLUMN-LABEL "Colors"
      display-po-no() @ lv-po-no COLUMN-LABEL "PO#"
   */   
      job-mch.start-date-su COLUMN-LABEL "Setup!Start Date" FORMAT "99/99/9999":U
      cvt-time-to-string('',job-mch.start-time-su,0.00) @ lv-mr-stime COLUMN-LABEL "Setup!Start Time"
      job-mch.end-date-su COLUMN-LABEL "Setup!End Date" FORMAT "99/99/9999":U
      cvt-time-to-string('END',job-mch.start-time-su,job-mch.mr-hr) @ lv-mr-etime COLUMN-LABEL "Setup!End Time"
      cvt-hour-to-string(job-mch.mr-hr) @ lv-mr-hr COLUMN-LABEL "MR Hour"
      job-mch.start-date COLUMN-LABEL "Run!Start Date" FORMAT "99/99/9999":U
      cvt-time-to-string('',job-mch.start-time,0.00) @ lv-run-stime COLUMN-LABEL "Run!Start Time"
      job-mch.end-date COLUMN-LABEL "Run!End Date" FORMAT "99/99/9999":U
      cvt-time-to-string('END',job-mch.start-time,job-mch.run-hr) @ lv-mr-etime COLUMN-LABEL "Run!End Time"
      cvt-hour-to-string(job-mch.run-hr) @ lv-run-hr COLUMN-LABEL "Run Hour"
  ENABLE
      tt-sch.seq-no
      job-mch.start-date-su
      job-mch.end-date-su
      job-mch.start-date
      job-mch.end-date
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 129 BY 16.19
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     lv-start-date AT ROW 1.71 COL 21 COLON-ALIGNED
     btn-go AT ROW 1.71 COL 59
     Browser-Table AT ROW 3.14 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: asi.mach
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
         HEIGHT             = 18.48
         WIDTH              = 155.2.
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table btn-go F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
    FOR EACH tt-sch,
        EACH job-mch WHERE RECID(job-mch) = tt-sch.rec-id NO-LOCK,
        FIRST job-hdr WHERE job-hdr.company = job-mch.company
                      AND job-hdr.job = job-mch.job NO-LOCK
                      BY tt-sch.start-date BY tt-sch.seq.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ", FIRST USED, FIRST USED"
     _OrdList          = "ASI.job.start-date|yes,ASI.job-mch.seq-no|yes"
     _JoinCode[1]      = "ASI.job-mch.company = ASI.mach.company
  AND ASI.job-mch.m-code = ASI.mach.m-code"
     _JoinCode[2]      = "ASI.job-hdr.company = ASI.job-mch.company
  AND ASI.job-hdr.job = ASI.job-mch.job"
     _JoinCode[3]      = "ASI.job.company = ASI.job-hdr.company
  AND ASI.job.job = ASI.job-hdr.job"
     _Where[3]         = "(ASI.job.start-date >= lv-start-date or lv-start-date = ?)"
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
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-go B-table-Win
ON CHOOSE OF btn-go IN FRAME F-Main /* Go */
DO:
    ASSIGN lv-START-date.
    {&OPEN-QUERY-{&browse-name}}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-start-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-start-date B-table-Win
ON LEAVE OF lv-start-date IN FRAME F-Main /* Start Date */
DO:
   ASSIGN lv-start-date.
   APPLY "choose" TO btn-go.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-start-date B-table-Win
ON VALUE-CHANGED OF lv-start-date IN FRAME F-Main /* Start Date */
DO:
  IF LENGTH(SELF:SCREEN-VALUE) = 5 THEN DO:
       MESSAGE SELF:SCREEN-VALUE VIEW-AS ALERT-BOX.
      self:SCREEN-VALUE = SELF:SCREEN-VALUE + STRING(YEAR(TODAY)).
      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

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
  {src/adm/template/row-list.i "mach"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mach"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table B-table-Win 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH tt-sch.
      DELETE tt-sch.
  END.

  DEF VAR i AS INT NO-UNDO.

  FOR EACH bf-job-mch WHERE bf-job-mch.company = ASI.mach.company
                         AND bf-job-mch.m-code = ASI.mach.m-code NO-LOCK,
      FIRST bf-job-hdr WHERE bf-job-hdr.company = bf-job-mch.company
                          AND bf-job-hdr.job = bf-job-mch.job NO-LOCK,
      FIRST bf-job WHERE bf-job.company = bf-job-hdr.company
                         AND bf-job.job = bf-job-hdr.job
                         AND (bf-job.start-date >= lv-start-date or lv-start-date = ?) NO-LOCK
               BY bf-job.start-date 
               BY bf-job-mch.seq-no:
      CREATE tt-sch.
      ASSIGN tt-sch.rec-id = RECID(job-mch)
             tt-sch.seq = job-mch.seq-no
             tt-sch.board = ef.board
             tt-sch.die = eb.die-no
             tt-sch.plate = eb.plate-no
             tt-sch.start-date = job.start-date.

      DO i = 1 TO 10:
         tt-sch.colour[i] = eb.i-code[i].
      END.
             
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN build-table.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


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
  {src/adm/template/snd-list.i "mach"}
  {src/adm/template/snd-list.i "tt-sch"}
  {src/adm/template/snd-list.i "job-mch"}
  {src/adm/template/snd-list.i "job-hdr"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cvt-hour-to-string B-table-Win 
FUNCTION cvt-hour-to-string RETURNS CHARACTER
  ( INPUT ip-hour AS dec  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   RETURN STRING( int(ip-hour * 3600),"HH:MM").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cvt-time-to-string B-table-Win 
FUNCTION cvt-time-to-string RETURNS CHARACTER
  (INPUT ip-type AS CHAR, INPUT ip-stime AS INT, INPUT ip-hour AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF ip-type = "END" THEN DO:
     DEF VAR li-end-time AS INT NO-UNDO.
     li-end-time = ip-stime + ip-hour * 3600.
     RETURN STRING(li-end-time,"HH:MM").
  END.
  ELSE
  RETURN STRING(ip-stime,"HH:MM").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-board B-table-Win 
FUNCTION display-board RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST ef WHERE ef.company = job-hdr.company
                  AND ef.est-no = job-hdr.est-no
                  AND ef.form-no = job-mch.frm
                  NO-LOCK NO-ERROR.

  IF AVAIL ef THEN RETURN ef.board.
  ELSE RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-color B-table-Win 
FUNCTION display-color RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   FIND FIRST eb WHERE eb.company = job-hdr.company
                  AND eb.est-no = job-hdr.est-no
                  AND eb.form-no = job-mch.frm
                  /*AND eb.blank-no = job-mch.blank-no */
                  NO-LOCK NO-ERROR.
  IF AVAIL eb THEN do:
     DEF VAR ii AS INT NO-UNDO.
     DEF VAR tmp-color AS cha NO-UNDO.
     DO ii = 1 TO eb.i-col:
        tmp-color = tmp-color + IF tmp-color <> "" THEN ("," + eb.i-code[ii] )
                                ELSE eb.i-code[ii].
     END.
     RETURN tmp-color.
  END.
  ELSE RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-die B-table-Win 
FUNCTION display-die RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   FIND FIRST eb WHERE eb.company = job-hdr.company
                  AND eb.est-no = job-hdr.est-no
                  AND eb.form-no = job-mch.frm
                  /*AND eb.blank-no = job-mch.blank-no */
                  NO-LOCK NO-ERROR.
  IF AVAIL eb THEN RETURN eb.die-no.
  ELSE RETURN "".   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-due-date B-table-Win 
FUNCTION display-due-date RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST oe-ord WHERE oe-ord.company EQ job-mch.company
          AND oe-ord.job-no  EQ job-mch.job-no
          AND oe-ord.job-no2 EQ job-mch.job-no2 no-lock no-error.
   IF AVAIL oe-ord THEN   RETURN oe-ord.due-date.
   ELSE RETURN ?.
   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-po-no B-table-Win 
FUNCTION display-po-no RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   find first po-ordl
          where po-ordl.company eq job-hdr.company            
            and po-ordl.job-no  eq job-mch.job-no
            and po-ordl.job-no2 eq job-mch.job-no2
            and po-ordl.s-num   eq job-mch.frm
            and po-ordl.i-no    eq job-mch.i-no
            NO-LOCK NO-ERROR.
 


  IF AVAIL po-ordl THEN RETURN po-ordl.po-no.
  ELSE RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

