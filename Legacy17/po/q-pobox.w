&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS q-tables 
/*------------------------------------------------------------------------

  File:  

  Description: from QUERY.W - Template For Query objects in the ADM

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartQuery
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,Navigation-Target

&Scoped-define QUERY-NAME Query-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES po-ordl
&Scoped-define FIRST-EXTERNAL-TABLE po-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR po-ordl.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES job-mat job est ef eb box-design-hdr

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for QUERY Query-Main                                     */
&Scoped-define QUERY-STRING-Query-Main FOR EACH job-mat WHERE job-mat.company    eq po-ordl.company ~
      and job-mat.rm-i-no    eq po-ordl.i-no ~
      and job-mat.job-no     eq string(fill(" ",6 - ~
                                            length(trim(po-ordl.job-no)))) + ~
                                trim(po-ordl.job-no) ~
      and job-mat.job-no2    eq po-ordl.job-no2 ~
      and job-mat.i-no       eq po-ordl.i-no ~
      and ((job-mat.frm      eq po-ordl.s-num and po-ordl.s-num ne 0) or ~
           po-ordl.s-num     eq 0) ~
      and ((job-mat.blank-no eq po-ordl.b-num and po-ordl.b-num ne 0) or ~
           po-ordl.b-num     eq 0) NO-LOCK, ~
      EACH job WHERE job.company eq job-mat.company ~
      and job.job     eq job-mat.job ~
      and job.job-no  eq job-mat.job-no ~
      and job.job-no2 eq job-mat.job-no2 NO-LOCK, ~
      EACH est WHERE est.company eq job.company ~
      AND est.est-no  EQ job.est-no NO-LOCK, ~
      EACH ef WHERE ef.company eq est.company ~
      AND ef.est-no  EQ est.est-no ~
      and ef.form-no eq job-mat.frm NO-LOCK, ~
      EACH eb OF ef NO-LOCK, ~
      EACH box-design-hdr WHERE box-design-hdr.design-no = 0  ~
  AND box-design-hdr.company = eb.company  ~
  AND box-design-hdr.est-no = eb.est-no  ~
  AND box-design-hdr.form-no = eb.form-no  ~
  AND box-design-hdr.blank-no = eb.blank-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH job-mat WHERE job-mat.company    eq po-ordl.company ~
      and job-mat.rm-i-no    eq po-ordl.i-no ~
      and job-mat.job-no     eq string(fill(" ",6 - ~
                                            length(trim(po-ordl.job-no)))) + ~
                                trim(po-ordl.job-no) ~
      and job-mat.job-no2    eq po-ordl.job-no2 ~
      and job-mat.i-no       eq po-ordl.i-no ~
      and ((job-mat.frm      eq po-ordl.s-num and po-ordl.s-num ne 0) or ~
           po-ordl.s-num     eq 0) ~
      and ((job-mat.blank-no eq po-ordl.b-num and po-ordl.b-num ne 0) or ~
           po-ordl.b-num     eq 0) NO-LOCK, ~
      EACH job WHERE job.company eq job-mat.company ~
      and job.job     eq job-mat.job ~
      and job.job-no  eq job-mat.job-no ~
      and job.job-no2 eq job-mat.job-no2 NO-LOCK, ~
      EACH est WHERE est.company eq job.company ~
      AND est.est-no  EQ job.est-no NO-LOCK, ~
      EACH ef WHERE ef.company eq est.company ~
      AND ef.est-no  EQ est.est-no ~
      and ef.form-no eq job-mat.frm NO-LOCK, ~
      EACH eb OF ef NO-LOCK, ~
      EACH box-design-hdr WHERE box-design-hdr.design-no = 0  ~
  AND box-design-hdr.company = eb.company  ~
  AND box-design-hdr.est-no = eb.est-no  ~
  AND box-design-hdr.form-no = eb.form-no  ~
  AND box-design-hdr.blank-no = eb.blank-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Query-Main job-mat job est ef eb ~
box-design-hdr
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main job-mat
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main job
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main est
&Scoped-define FOURTH-TABLE-IN-QUERY-Query-Main ef
&Scoped-define FIFTH-TABLE-IN-QUERY-Query-Main eb
&Scoped-define SIXTH-TABLE-IN-QUERY-Query-Main box-design-hdr


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" q-tables _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&QUERY-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|ASI.job.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" q-tables _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&QUERY-NAME
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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      job-mat, 
      job, 
      est, 
      ef, 
      eb, 
      box-design-hdr SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartQuery
   External Tables: ASI.po-ordl
   Allow: Basic,Query
   Frames: 1
   Add Fields to: NEITHER
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
  CREATE WINDOW q-tables ASSIGN
         HEIGHT             = 1.33
         WIDTH              = 22.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB q-tables 
/* ************************* Included-Libraries *********************** */

{src/adm/method/query.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW q-tables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for QUERY Query-Main
     _TblList          = "ASI.job-mat WHERE ASI.po-ordl <external> ... ...,ASI.job WHERE ASI.job-mat ...,ASI.est WHERE ASI.job  ...,ASI.ef WHERE ASI.est  ...,ASI.eb OF ASI.ef,ASI.box-design-hdr WHERE ASI.eb  ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "job-mat.company    eq po-ordl.company
      and job-mat.rm-i-no    eq po-ordl.i-no
      and job-mat.job-no     eq string(fill("" "",6 -
                                            length(trim(po-ordl.job-no)))) +
                                trim(po-ordl.job-no)
      and job-mat.job-no2    eq po-ordl.job-no2
      and job-mat.i-no       eq po-ordl.i-no
      and ((job-mat.frm      eq po-ordl.s-num and po-ordl.s-num ne 0) or
           po-ordl.s-num     eq 0)
      and ((job-mat.blank-no eq po-ordl.b-num and po-ordl.b-num ne 0) or
           po-ordl.b-num     eq 0)"
     _JoinCode[2]      = "job.company eq job-mat.company
      and job.job     eq job-mat.job
      and job.job-no  eq job-mat.job-no
      and job.job-no2 eq job-mat.job-no2"
     _JoinCode[3]      = "est.company eq job.company
      AND est.est-no  EQ job.est-no"
     _JoinCode[4]      = "ef.company eq est.company
      AND ef.est-no  EQ est.est-no
      and ef.form-no eq job-mat.frm"
     _JoinCode[6]      = "box-design-hdr.design-no = 0 
  AND box-design-hdr.company = eb.company 
  AND box-design-hdr.est-no = eb.est-no 
  AND box-design-hdr.form-no = eb.form-no 
  AND box-design-hdr.blank-no = eb.blank-no"
     _Design-Parent    is WINDOW q-tables @ ( 1.1 , 9.8 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK q-tables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases q-tables  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&QUERY-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available q-tables  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "po-ordl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "po-ordl"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI q-tables  _DEFAULT-DISABLE
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
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key q-tables  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "job" "company"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records q-tables  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "po-ordl"}
  {src/adm/template/snd-list.i "job-mat"}
  {src/adm/template/snd-list.i "job"}
  {src/adm/template/snd-list.i "est"}
  {src/adm/template/snd-list.i "ef"}
  {src/adm/template/snd-list.i "eb"}
  {src/adm/template/snd-list.i "box-design-hdr"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed q-tables 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/qstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

