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
&Scoped-define INTERNAL-TABLES mmtx

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for QUERY Query-Main                                     */
&Scoped-define QUERY-STRING-Query-Main FOR EACH mmtx OF mstd WHERE ~{&KEY-PHRASE} ~
      AND mmtx.mr-run = True NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH mmtx OF mstd WHERE ~{&KEY-PHRASE} ~
      AND mmtx.mr-run = True NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Query-Main mmtx
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main mmtx


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
      mmtx SCROLLING.
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
     _TblList          = "ASI.mmtx OF ASI.mstd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.mmtx.mr-run = True"
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
   def buffer bf-mmtx for mmtx .
   def buffer bf2-mmtx for mmtx.
   def var i as int no-undo.
   
   assign li-cur-page = mmtx.page-no
          li-cur-across = mmtx.across-no.
          
    /*get next {&query-name}.
    run dispatch ('row-available').
    */
   
    find first bf-mmtx of mstd where bf-mmtx.page-no > mmtx.page-no and
                                   bf-mmtx.across-no = mmtx.across-no AND
                                   bf-mmtx.mr-run = True
                                   no-lock no-error.
             
    if /*num-results("{&query-name}") = (mmtx.page-no + 1)   not working */
       not avail bf-mmtx 
       and mmtx.row-value[15] <> 0 then do:

       def var li-mmtx-no as int no-undo.
       find last bf2-mmtx use-index mmtx-no no-lock no-error.
       li-mmtx-no = if avail bf2-mmtx then bf2-mmtx.mmtx-no + 1 else 1.

       create bf-mmtx.
       assign bf-mmtx.company = mstd.company
              bf-mmtx.loc = mstd.loc
              bf-mmtx.m-code = mstd.m-code
              bf-mmtx.dept = mstd.dept
              bf-mmtx.style = mstd.style
              bf-mmtx.r-title = mmtx.r-title
              bf-mmtx.c-title = mmtx.c-title
              bf-mmtx.page-no = mmtx.page-no + 1
              bf-mmtx.across-no = mmtx.across-no
              bf-mmtx.mmtx-no = li-mmtx-no
              bf-mmtx.mr-run = True.
       do i = 1 to 16:
              bf-mmtx.rtit[i] = mmtx.rtit[i].
       end.       
       do i = 1 to 10:
           bf-mmtx.col-value[i] = mmtx.col-value[i].
       end.       
       if bf-mmtx.across-no <> 0 then do:
          find first bf2-mmtx of mstd where bf2-mmtx.page-no = mmtx.page-no
                                        and bf2-mmtx.across-no = 0
                                        AND bf2-mmtx.mr-run = True
                                        no-lock no-error.
          do i = 1 to 15:
             bf-mmtx.row-value[i] = bf2-mmtx.row-value[i].
          end.                               
       end.       
    end.
if mmtx.row-value[15] <> 0 then do:
    &scoped-define key-phrase mmtx.page-no > li-cur-page and mmtx.across-no = li-cur-across AND mmtx.mr-run = True
/*    run dispatch ('open-query'). */
    OPEN QUERY {&query-name} FOR EACH mmtx OF mstd NO-LOCK where /*{&key-phrase}*/
             mmtx.page-no > li-cur-page and mmtx.across-no = li-cur-across
             AND mmtx.mr-run = True.

    /*~{&SORTBY-PHRASE}.  */
    RUN dispatch('get-first':U). 
/*    message "down" mmtx.page-no mmtx.across-no  "curr: " li-cur-page "," li-cur-across skip
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
  assign li-cur-page = mmtx.page-no
         li-cur-across = mmtx.across-no.

  
  &scoped-define key-phrase mmtx.page-no = li-cur-page and mmtx.across-no < li-cur-across AND mmtx.mr-run = True
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
  def buffer bf-mmtx for mmtx.
  def buffer bf2-mmtx for mmtx.
  def var i as int no-undo.

  assign li-cur-page = mmtx.page-no
         li-cur-across = mmtx.across-no.

  find first bf-mmtx of mstd where bf-mmtx.page-no = mmtx.page-no and
                                   bf-mmtx.across-no > mmtx.across-no AND
                                   bf-mmtx.mr-run = True
                                   no-lock no-error.
  if not avail bf-mmtx and mmtx.col-val[10] <> 0 then do:
       def var li-mmtx-no as int no-undo.
       find last bf2-mmtx use-index mmtx-no no-lock no-error.
       li-mmtx-no = if avail bf2-mmtx then bf2-mmtx.mmtx-no + 1 else 1.

       create bf-mmtx.
       assign bf-mmtx.company = mstd.company
              bf-mmtx.loc = mstd.loc
              bf-mmtx.m-code = mstd.m-code
              bf-mmtx.dept = mstd.dept
              bf-mmtx.style = mstd.style
              bf-mmtx.r-title = mmtx.r-title
              bf-mmtx.c-title = mmtx.c-title
              bf-mmtx.page-no = mmtx.page-no 
              bf-mmtx.across-no = mmtx.across-no + 1
              bf-mmtx.mmtx-no = li-mmtx-no
              bf-mmtx.mr-run = True.

       do i = 1 to 16:
              bf-mmtx.rtit[i] = mmtx.rtit[i].
       end.       
       do i = 1 to 10:
           bf-mmtx.col-value[i] = mmtx.col-value[i].
       end.       
       if bf-mmtx.across-no <> 0 then do:
          find first bf2-mmtx of mstd where bf2-mmtx.page-no = mmtx.page-no
                                        and bf2-mmtx.across-no = 0
                                        AND bf2-mmtx.mr-run = True
                                        no-lock no-error.
          do i = 1 to 15:
             bf-mmtx.row-value[i] = bf2-mmtx.row-value[i].
          end.                               
       end.       
       if bf-mmtx.page-no <> 0 then do:
          find first bf2-mmtx of mstd where bf2-mmtx.page-no = 0
                                        and bf2-mmtx.across-no = mmtx.across-no
                                        AND bf2-mmtx.mr-run = True
                                        no-lock no-error.
          do i = 1 to 10:
             bf-mmtx.col-value[i] = bf2-mmtx.col-value[i].
          end.       
       end.
  end.                                 

if mmtx.col-val[10] <> 0 then do:  
  &scoped-define key-phrase mmtx.page-no = li-cur-page and mmtx.across-no > li-cur-across AND mmtx.mr-run = True
  run dispatch ('open-query').
  run dispatch ('get-next').

/*      message "down" mmtx.page-no mmtx.across-no
              "{&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}"  num-results("{&query-name}") skip
              "key:   {&key-phrase}"
      .
*/
END.
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
  assign li-cur-page = mmtx.page-no
         li-cur-across = mmtx.across-no.

  
  &scoped-define key-phrase mmtx.page-no < li-cur-page and mmtx.across-no = li-cur-across AND mmtx.mr-run = True
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
  {src/adm/template/snd-list.i "mmtx"}

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

