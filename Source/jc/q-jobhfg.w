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
{custom/globdefs.i}

DEF VAR lv-i-no LIKE itemfg.i-no NO-UNDO.
DEF VAR lv-navi-selected AS LOG NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartQuery
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,Navigation-Target

&Scoped-define QUERY-NAME Query-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES job-hdr
&Scoped-define FIRST-EXTERNAL-TABLE job-hdr


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR job-hdr.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES itemfg oe-ordl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for QUERY Query-Main                                     */
&Scoped-define QUERY-STRING-Query-Main FOR EACH itemfg OF job-hdr WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST oe-ordl WHERE rowid(oe-ordl) eq lv-rowid OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH itemfg OF job-hdr WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST oe-ordl WHERE rowid(oe-ordl) eq lv-rowid OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Query-Main itemfg oe-ordl
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main itemfg
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main oe-ordl


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
company||y|ASI.itemfg.company
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
      itemfg, 
      oe-ordl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartQuery
   External Tables: asi.job-hdr
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
     _TblList          = "asi.itemfg OF asi.job-hdr,asi.oe-ordl WHERE asi.job-hdr ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST OUTER"
     _JoinCode[2]      = "rowid(oe-ordl) eq lv-rowid"
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
  {src/adm/template/row-list.i "job-hdr"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "job-hdr"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-navigation q-tables 
PROCEDURE disable-navigation :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-navigation q-tables 
PROCEDURE Enable-navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query q-tables 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  IF NOT lv-navi-selected THEN 
      RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RELEASE oe-ordl.

  IF lv-navi-selected THEN DO:
     OPEN QUERY {&query-name}
          FOR EACH  ASI.itemfg NO-LOCK WHERE itemfg.company = g_company,
              FIRST oe-ordl WHERE ROWID(oe-ordl) EQ lv-rowid OUTER-JOIN NO-LOCK.

  END.

  ELSE
  IF AVAIL job-hdr THEN DO:
    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ job-hdr.company
          AND oe-ordl.ord-no  EQ job-hdr.ord-no
          AND oe-ordl.job-no  EQ job-hdr.job-no
          AND oe-ordl.job-no2 EQ job-hdr.job-no2
          AND oe-ordl.i-no    EQ job-hdr.i-no
        NO-LOCK NO-ERROR.

    IF job-hdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ job-hdr.company
          AND oe-ordl.ord-no  EQ job-hdr.ord-no
          AND oe-ordl.i-no    EQ job-hdr.i-no
        NO-LOCK NO-ERROR.

    IF job-hdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ job-hdr.company
          AND oe-ordl.ord-no  EQ job-hdr.ord-no
        NO-LOCK NO-ERROR.

    IF AVAIL oe-ordl THEN DO:
      lv-rowid = ROWID(oe-ordl).
      RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Navigate-FG q-tables 
PROCEDURE Navigate-FG :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-navi-to AS cha NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.
  lv-navi-selected = YES.

  lv-i-no = IF AVAIL itemfg THEN itemfg.i-no ELSE "".
  CASE ip-navi-to:
      WHEN "NEXT" THEN DO:
          FIND FIRST itemfg USE-INDEX i-no
               WHERE itemfg.company = g_company AND itemfg.i-no > lv-i-no NO-LOCK NO-ERROR.
          IF AVAIL itemfg THEN DO:
              RUN dispatch ("row-changed").    
              RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"navigation-source", OUTPUT char-hdl).
              IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:             
                 RUN set-attribute-list ('Query-position=not-first-or-last').
                 RUN set-buttons IN WIDGET-HANDLE(char-hdl) ("enable-all").
              END.
          END.
          ELSE RUN navigate-fg ("LAST").
      END.
      WHEN "PREV" THEN DO:
          FIND last itemfg USE-INDEX i-no 
               WHERE itemfg.company = g_company AND itemfg.i-no < lv-i-no NO-LOCK NO-ERROR.
          IF AVAIL itemfg THEN DO:
              RUN dispatch ("row-changed").    
              RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"navigation-source", OUTPUT char-hdl).
              IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
                  RUN set-attribute-list ('Query-position=not-first-or-last').
                  RUN set-buttons IN WIDGET-HANDLE(char-hdl) ("enable-all").
              END.
          END.
          ELSE RUN navigate-fg ("first").
      END.
      WHEN "LAST" THEN DO:
          FIND last itemfg USE-INDEX i-no 
               WHERE itemfg.company = g_company NO-LOCK NO-ERROR.
          IF AVAIL itemfg THEN DO:
              RUN dispatch ("row-changed").    
              RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"navigation-source", OUTPUT char-hdl).
              IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
                 RUN set-attribute-list ('Query-position=last-record').
                 RUN set-buttons IN WIDGET-HANDLE(char-hdl) ("LAST").
              END.
                 
          END.
      END.
      WHEN "FIRST" THEN DO:
          FIND first itemfg USE-INDEX i-no 
               WHERE itemfg.company = g_company NO-LOCK NO-ERROR.
          IF AVAIL itemfg THEN DO:
              RUN dispatch ("row-changed").    
              RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"navigation-source", OUTPUT char-hdl).
              IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
                 RUN set-attribute-list ('Query-position=first-record').
                 RUN set-buttons IN WIDGET-HANDLE(char-hdl) ("FIRST").
              END.
          END.
      END.
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query q-tables 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-rowid as rowid no-undo.
  
  
  run dispatch in this-procedure ("open-query").
  
  /*reposition {&browse-name} to rowid ip-rowid no-error. */

  run dispatch in this-procedure ("row-changed").
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
  {src/adm/template/sndkycas.i "company" "itemfg" "company"}

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
  {src/adm/template/snd-list.i "job-hdr"}
  {src/adm/template/snd-list.i "itemfg"}
  {src/adm/template/snd-list.i "oe-ordl"}

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

