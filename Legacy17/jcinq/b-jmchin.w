&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admBrowserUsing.i} /* added by script _admBrowsers.p on 04.07.2017 @  2:08:04 pm */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: jcinq\b-jmchin.w 

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

&SCOPED-DEFINE yellowColumnsName b-jmchin
&SCOPED-DEFINE noSortByField
&SCOPED-DEFINE SORTBY-PHRASE ~
BY STRING(mch.form-no,"999") BY STRING(mch.blank-no,"999") BY STRING(mch.line,"999")
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

define TEMP-TABLE mch NO-UNDO
   field line like job-mch.line
   field form-no like mch-act.frm
   field blank-no like mch-act.blank-no
   FIELD pass LIKE mch-act.pass
   field m-code like mch-act.m-code
   field i-no   like job-mch.i-no
   field dept like job-mch.dept
   field run-hr like job-mch.run-hr
   field run-act like job-mch.run-hr
   field run-var as dec format '->>9.99'
   field mr-hr like job-mch.mr-hr
   field mr-act like job-mch.mr-hr
   field mr-var as dec format '->>9.99'.

define TEMP-TABLE x-mch NO-UNDO
   field line like job-mch.line
   field form-no like mch-act.frm
   field blank-no like mch-act.blank-no
   FIELD pass LIKE mch-act.pass
   field m-code like mch-act.m-code
   field i-no   like job-mch.i-no
   field dept like job-mch.dept
   field run-hr like job-mch.run-hr
   field run-act like job-mch.run-hr
   field run-var as dec format '->>9.99'
   field mr-hr like job-mch.mr-hr
   field mr-act like job-mch.mr-hr
   field mr-var as dec format '->>9.99'
   field est-speed like job-mch.speed
   field act-qty as DEC.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES job
&Scoped-define FIRST-EXTERNAL-TABLE job


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR job.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mch

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table mch.form-no mch.blank-no mch.pass mch.m-code mch.i-no mch.run-hr mch.run-act mch.run-var mch.mr-hr mch.mr-act mch.mr-var   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH mch ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH mch ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table mch
&Scoped-define FIRST-TABLE-IN-QUERY-br_table mch


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      mch SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      mch.form-no  COLUMN-LABEL "Sheet" FORMAT ">>>":U LABEL-BGCOLOR 14
      mch.blank-no COLUMN-LABEL "Blank" FORMAT ">>>":U LABEL-BGCOLOR 14
      mch.pass     COLUMN-LABEL "P#"    FORMAT ">>":U LABEL-BGCOLOR 14 
      mch.m-code   COLUMN-LABEL "Machine" LABEL-BGCOLOR 14
      mch.i-no     COLUMN-LABEL "FG Item#" FORMAT "x(15)":U LABEL-BGCOLOR 14
      mch.run-hr   COLUMN-LABEL "Std Run Hrs" FORMAT "->>>,>>9.9<":U LABEL-BGCOLOR 14
      mch.run-act  COLUMN-LABEL "Act Run Hrs" FORMAT "->>>,>>9.9<":U LABEL-BGCOLOR 14
      mch.run-var  COLUMN-LABEL "Run Hrs Var" FORMAT "->>>,>>9.9<":U LABEL-BGCOLOR 14
      mch.mr-hr    COLUMN-LABEL "Std MR Hrs" FORMAT "->>>,>>9.9<":U LABEL-BGCOLOR 14
      mch.mr-act   COLUMN-LABEL "Act MR Hrs" FORMAT "->>>,>>9.9<":U LABEL-BGCOLOR 14
      mch.mr-var   COLUMN-LABEL "MR Hrs Var" FORMAT "->>>,>>9.9<":U LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 129 BY 13.57
         BGCOLOR 8 FONT 2
         TITLE BGCOLOR 8 "Machine Hours Variance".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.job
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
         HEIGHT             = 14.43
         WIDTH              = 129.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{custom/yellowColumns.i}

{Advantzware/WinKit/dataGridProc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH mch ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
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
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main /* Machine Hours Variance */
DO:
  IF mch.m-code:SCREEN-VALUE IN BROWSE {&browse-name} NE "All" THEN DO:
    RUN jcinq\b-updmac.w(INPUT job.job-no,
                         INPUT job.job-no2,
                         INPUT mch.form-no:screen-value in BROWSE {&browse-name},
                         INPUT mch.blank-no:screen-value in BROWSE {&browse-name},
                         INPUT mch.m-code:screen-value in BROWSE {&browse-name}).

    RUN local-open-query.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main /* Machine Hours Variance */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main /* Machine Hours Variance */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main /* Machine Hours Variance */
DO:
  RUN startSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main /* Machine Hours Variance */
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
  {src/adm/template/row-list.i "job"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "job"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var v-pct as dec init 1.00 no-undo.
  def var v-std-tot AS DEC NO-UNDO EXTENT 2.
  def var v-act-tot AS DEC NO-UNDO EXTENT 2.
  def var v-var-tot AS DEC NO-UNDO EXTENT 2.


  /* Code placed here will execute PRIOR to standard behavior. */
  EMPTY TEMP-TABLE mch.
  EMPTY TEMP-TABLE x-mch.

  for each job-mch
      where job-mch.company = cocode
        and job-mch.job = job.job
      no-lock:
    create x-mch.
    assign x-mch.form-no  = job-mch.frm
           x-mch.line     = job-mch.line
           x-mch.blank-no = job-mch.blank-no
           x-mch.pass     = job-mch.pass
           x-mch.m-code   = job-mch.m-code
           x-mch.i-no     = job-mch.i-no
           x-mch.dept     = job-mch.dept
           x-mch.est-speed = job-mch.speed.
    IF job-mch.j-no EQ 0 THEN
      ASSIGN
       x-mch.run-hr = job-mch.run-hr
       x-mch.mr-hr  = job-mch.mr-hr.
  end.

  for each mch-act where mch-act.company = cocode and
                       mch-act.job = job.job
                       no-lock:
    v-pct = 1.
    find first x-mch where x-mch.form-no  = mch-act.frm and
                           x-mch.blank-no = mch-act.blank-no and
                           x-mch.m-code   = mch-act.m-code AND
                           x-mch.pass     = mch-act.pass
                           no-error.
    if not available x-mch THEN do:
      create x-mch.
      assign x-mch.form-no  = mch-act.frm
             x-mch.line     = x
             x-mch.blank-no = mch-act.blank-no
             x-mch.pass     = mch-act.pass
             x-mch.m-code   = mch-act.m-code
             x-mch.i-no     = mch-act.i-no
             x-mch.run-hr   = 0
             x-mch.mr-hr    = 0.
      x = x - 1.
      find mach where mach.company = cocode and
                      mach.loc     = locode and
                      mach.m-code  = mch-act.m-code
                      no-lock no-error.
      if available mach then
        x-mch.dept = mach.dept[1].
    end.
    find job-code where job-code.code = mch-act.code
                        no-lock no-error.

    if not available job-code then next.

    if job-code.cat = "MR" then
       x-mch.mr-act = x-mch.mr-act + (mch-act.hours * v-pct).
    else if job-code.cat = "RUN" then
       assign x-mch.run-act = x-mch.run-act + (mch-act.hours * v-pct)
              x-mch.act-qty = x-mch.act-qty +
                              ((mch-act.qty /*+ mch-act.waste*/) * v-pct).
  end.

  ASSIGN
   v-std-tot = 0
   v-act-tot = 0
   v-var-tot = 0.

  for each x-mch break by x-mch.line:
    create mch.
    BUFFER-COPY x-mch TO mch.

    if mch.run-act > 0 and x-mch.est-speed <> 0 then
    DO:
       IF CAN-FIND(FIRST mach WHERE
          mach.company EQ cocode AND
          mach.loc     EQ locode AND
          mach.m-code  EQ mch.m-code AND
          mach.therm   EQ YES AND
          (mach.p-type EQ "R" OR mach.dept[1] EQ "LM")) THEN
          FOR EACH job-mat FIELDS(i-no len) WHERE
              job-mat.company eq cocode AND
              job-mat.job = job.job AND
              job-mat.frm EQ x-mch.form-no AND
              job-mat.frm GT 0 AND
              job-mat.len GT 0
              no-lock,
              first ITEM FIELDS(mat-type) WHERE
                    item.company eq cocode AND
                    item.i-no eq job-mat.i-no
                    no-lock

              BREAK BY job-mat.frm
                    BY item.mat-type
                    BY job-mat.j-no
                    BY job-mat.rec_key:

              mch.run-hr = (x-mch.act-qty * job-mat.len / 12) / x-mch.est-speed.
              LEAVE.
          END.
       ELSE
          mch.run-hr = x-mch.act-qty / x-mch.est-speed.
    END.

    assign
     mch.run-var  = mch.run-act - mch.run-hr
     mch.mr-var   = mch.mr-act  - mch.mr-hr
     v-std-tot[1] = v-std-tot[1] + mch.run-hr
     v-act-tot[1] = v-act-tot[1] + mch.run-act
     v-var-tot[1] = v-var-tot[1] + mch.run-var
     v-std-tot[2] = v-std-tot[2] + mch.mr-hr
     v-act-tot[2] = v-act-tot[2] + mch.mr-act
     v-var-tot[2] = v-var-tot[2] + mch.mr-var.
  end.

  FIND FIRST mch
      WHERE mch.form-no  EQ 0
        AND mch.blank-no EQ 0
        AND mch.m-code   EQ "ALL"
      NO-ERROR.
  IF AVAIL mch THEN DELETE mch.

  CREATE mch.
  ASSIGN
   mch.form-no  = 0
   mch.blank-no = 0
   mch.pass     = 0
   mch.m-code   = "ALL"
   mch.run-hr   = v-std-tot[1]
   mch.run-act  = v-act-tot[1]
   mch.run-var  = v-var-tot[1]
   mch.mr-hr    = v-std-tot[2]
   mch.mr-act   = v-act-tot[2]
   mch.mr-var   = v-var-tot[2].

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  {methods/winReSizeLocInit.i}

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
  {src/adm/template/snd-list.i "job"}
  {src/adm/template/snd-list.i "mch"}

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

