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
def var li-cur-page as int no-undo.
def var li-cur-across as int no-undo.

{custom/persist.i}
{custom/gcompany.i}
{custom/gloc.i}
assign gcompany = g_company
       gloc = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartQuery
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,Navigation-Target

&Scoped-define QUERY-NAME Query-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES mstd
&Scoped-define FIRST-EXTERNAL-TABLE mstd


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mstd.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mmty

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for QUERY Query-Main                                     */
&Scoped-define SELF-NAME Query-Main
&Scoped-define QUERY-STRING-Query-Main FOR EACH mmty OF mstd NO-LOCK where {&key-phrase}     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY {&SELF-NAME} FOR EACH mmty OF mstd NO-LOCK where {&key-phrase}     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Query-Main mmty
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main mmty


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
      mmty SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartQuery
   External Tables: ASI.mstd
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
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH mmty OF mstd NO-LOCK where {&key-phrase}
    ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Design-Parent    is WINDOW q-tables @ ( 1.1 , 9.8 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Query-Main
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK q-tables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  {src/adm/template/row-list.i "mstd"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mstd"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation q-tables 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation q-tables 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-recid q-tables 
PROCEDURE get-recid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 def output param lv-recid as recid.
 
 lv-recid = recid(mstd).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-down q-tables 
PROCEDURE proc-down :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def buffer bf-mmty for mmty .
   def buffer bf2-mmty for mmty.
   def var i as int no-undo.
   
   assign li-cur-page = mmty.page-no
          li-cur-across = mmty.across-no.
          
    /*get next {&query-name}.
    run dispatch ('row-available').
    */
   
    find first bf-mmty of mstd where bf-mmty.page-no > mmty.page-no and
                                   bf-mmty.across-no = mmty.across-no
                                   no-lock no-error.
             
    if /*num-results("{&query-name}") = (mmty.page-no + 1)   not working */
       not avail bf-mmty 
       and mmty.row-value[15] <> 0 then do:
       create bf-mmty.
       assign bf-mmty.company = mstd.company
              bf-mmty.loc = mstd.loc
              bf-mmty.m-code = mstd.m-code
              bf-mmty.dept = mstd.dept
              bf-mmty.style = mstd.style
              bf-mmty.r-title = mmty.r-title
              bf-mmty.c-title = mmty.c-title
              bf-mmty.page-no = mmty.page-no + 1
              bf-mmty.across-no = mmty.across-no
              .
       do i = 1 to 16:
              bf-mmty.rtit[i] = mmty.rtit[i].
       end.       
       do i = 1 to 10:
           bf-mmty.col-value[i] = mmty.col-value[i].
       end.       
       if bf-mmty.across-no <> 0 then do:
          find first bf2-mmty of mstd where bf2-mmty.page-no = mmty.page-no
                                        and bf2-mmty.across-no = 0
                                        no-lock no-error.
          do i = 1 to 15:
             bf-mmty.row-value[i] = bf2-mmty.row-value[i].
          end.                               
       end.       
    end.
 
 if mmty.row-value[15] <> 0 then do:
    &scoped-define key-phrase mmty.page-no > li-cur-page and mmty.across-no = li-cur-across 
/*    run dispatch ('open-query'). */
    OPEN QUERY {&query-name} FOR EACH mmty OF mstd NO-LOCK where /*{&key-phrase}*/
             mmty.page-no > li-cur-page and mmty.across-no = li-cur-across .

    /*~{&SORTBY-PHRASE}.  */
    RUN dispatch('get-first':U). 
/*    message "down" mmty.page-no mmty.across-no  "curr: " li-cur-page "," li-cur-across skip
              "{&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}"  num-results("{&query-name}") skip
              "key:   {&key-phrase}"
      .
*/      

end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-first q-tables 
PROCEDURE proc-first :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  run dispatch ('get-first').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-last q-tables 
PROCEDURE proc-last :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  run dispatch ('get-last').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-left q-tables 
PROCEDURE proc-left :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  assign li-cur-page = mmty.page-no
         li-cur-across = mmty.across-no.

  
  &scoped-define key-phrase mmty.page-no = li-cur-page and mmty.across-no < li-cur-across 
  run dispatch ('open-query').
  run dispatch ('get-prev').
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-right q-tables 
PROCEDURE proc-right :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-mmty for mmty.
  def buffer bf2-mmty for mmty.
  def var i as int no-undo.

  assign li-cur-page = mmty.page-no
         li-cur-across = mmty.across-no.

  find first bf-mmty of mstd where bf-mmty.page-no = mmty.page-no and
                                   bf-mmty.across-no > mmty.across-no
                                   no-lock no-error.
  if not avail bf-mmty and mmty.col-val[10] <> 0 then do:
       create bf-mmty.
       assign bf-mmty.company = mstd.company
              bf-mmty.loc = mstd.loc
              bf-mmty.m-code = mstd.m-code
              bf-mmty.dept = mstd.dept
              bf-mmty.style = mstd.style
              bf-mmty.r-title = mmty.r-title
              bf-mmty.c-title = mmty.c-title
              bf-mmty.page-no = mmty.page-no 
              bf-mmty.across-no = mmty.across-no + 1
              .
       do i = 1 to 16:
              bf-mmty.rtit[i] = mmty.rtit[i].
       end.       
       do i = 1 to 10:
           bf-mmty.col-value[i] = mmty.col-value[i].
       end.       
       if bf-mmty.across-no <> 0 then do:
          find first bf2-mmty of mstd where bf2-mmty.page-no = mmty.page-no
                                        and bf2-mmty.across-no = 0
                                        no-lock no-error.
          do i = 1 to 15:
             bf-mmty.row-value[i] = bf2-mmty.row-value[i].
          end.                               
       end.       
       if bf-mmty.page-no <> 0 then do:
          find first bf2-mmty of mstd where bf2-mmty.page-no = 0
                                        and bf2-mmty.across-no = mmty.across-no
                                        no-lock no-error.
          do i = 1 to 10:
             bf-mmty.col-value[i] = bf2-mmty.col-value[i].
          end.       
       end.
  end.                                 

  if mmty.col-val[10] <> 0 then do:
  &scoped-define key-phrase mmty.page-no = li-cur-page and mmty.across-no > li-cur-across 
  run dispatch ('open-query').
  run dispatch ('get-next').

/*      message "down" mmty.page-no mmty.across-no
              "{&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}"  num-results("{&query-name}") skip
              "key:   {&key-phrase}"
      .
*/

  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-up q-tables 
PROCEDURE proc-up :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  assign li-cur-page = mmty.page-no
         li-cur-across = mmty.across-no.

  
  &scoped-define key-phrase mmty.page-no < li-cur-page and mmty.across-no = li-cur-across 
  run dispatch ('open-query').

  RUN dispatch('get-prev':U). 
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
  {src/adm/template/snd-list.i "mstd"}
  {src/adm/template/snd-list.i "mmty"}

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

