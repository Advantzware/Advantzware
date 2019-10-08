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
{custom/globdefs.i}

DEF TEMP-TABLE tt-cat LIKE smanbcat.

DEF VAR v-prd AS INT NO-UNDO.
DEF VAR char-hdl AS cha NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES smanbugt
&Scoped-define FIRST-EXTERNAL-TABLE smanbugt


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR smanbugt.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-cat

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-cat.procat tt-cat.budget-amt tt-cat.msf tt-cat.tons   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table tt-cat.budget-amt ~
   tt-cat.msf ~
   tt-cat.procat tt-cat.tons   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table tt-cat
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table tt-cat
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH tt-cat NO-LOCK     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH tt-cat NO-LOCK     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table tt-cat
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-cat


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
      tt-cat SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      tt-cat.procat FORMAT "x(5)":U
      tt-cat.budget-amt LABEL "Budget $" FORM ">>,>>>,>>9"
      tt-cat.msf LABEL "Budget MSF" FORM ">>>>,>>9"      
      tt-cat.tons LABEL "Budget Tons" FORM ">>>>,>>9"
      ENABLE tt-cat.budget-amt
             tt-cat.msf
             tt-cat.procat tt-cat.tons
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 66 BY 10.48
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: asi.smanbugt
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
         HEIGHT             = 10.67
         WIDTH              = 66.
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
OPEN QUERY {&SELF-NAME} FOR EACH tt-cat NO-LOCK
    ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "asi.smanbcat.company = asi.smanbugt.company
  AND asi.smanbcat.sman = asi.smanbugt.sman
  AND asi.smanbcat.budget-yr = asi.smanbugt.budget-yr
  "
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
ON HELP OF br_table IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.
   CASE FOCUS:NAME:
        when "procat" then do:
             run windows/l-fgcat.w (g_company,focus:screen-value, output char-val).
             if char-val <> "" then 
                assign focus:screen-value = entry(1,char-val)
                       /*ocat-desc:screen-value = entry(2,char-val)*/
                       .
        end.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
ON VALUE-CHANGED OF br_table IN FRAME F-Main
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
ON 'entry':U OF  tt-cat.procat
DO:
    IF NOT adm-new-record THEN DO:
       APPLY 'tab' TO SELF.
       RETURN NO-APPLY.
    END.
    RETURN.
END.
   
ON 'leave':U OF tt-cat.procat
DO:
    IF LASTKEY <>  -1 AND
       NOT CAN-FIND(FIRST fgcat WHERE fgcat.company = g_company 
                    AND fgcat.procat = tt-cat.procat:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
        MESSAGE "Invalid category.  Try help." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    RETURN.

END.


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
  {src/adm/template/row-list.i "smanbugt"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "smanbugt"}

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
  FOR EACH tt-cat:
      DELETE tt-cat.
  END.

  IF AVAIL smanbugt THEN DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
     RUN get-period IN WIDGET-HANDLE(char-hdl) (OUTPUT v-prd).

     FOR EACH smanbcat NO-LOCK WHERE smanbcat.company = smanbugt.company
                                 AND smanbcat.sman = smanbugt.sman
                                 AND smanbcat.budget-yr = smanbugt.budget-yr
                                 AND smanbcat.budget-period = v-prd:
         CREATE tt-cat.
         BUFFER-COPY smanbcat TO tt-cat.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-procat B-table-Win 
PROCEDURE get-procat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-procat AS cha.

  FIND FIRST smanbcat WHERE smanbcat.company = tt-cat.company
                           AND smanbcat.sman = tt-cat.sman
                           AND smanbcat.budget-yr = tt-cat.budget-yr
                           AND smanbcat.budget-period = tt-cat.budget-period
                           AND smanbcat.procat = tt-cat.procat  NO-ERROR.
  IF AVAIL smanbcat THEN op-procat = tt-cat.procat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-procat-prev AS cha NO-UNDO.
  DEF buffer bf-bugt FOR smanbugt.
  DEF buffer bf-bcat FOR smanbcat.

  /* Code placed here will execute PRIOR to standard behavior. */

  v-procat-prev = IF AVAIL tt-cat THEN tt-cat.procat ELSE "".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST smanbcat WHERE smanbcat.company = tt-cat.company
                        AND smanbcat.sman = tt-cat.sman
                        AND smanbcat.budget-yr = tt-cat.budget-yr
                        AND smanbcat.budget-period = tt-cat.budget-period
                        AND smanbcat.procat = tt-cat.procat  NO-ERROR.
  IF NOT AVAIL smanbcat AND adm-new-record THEN DO:
     CREATE smanbcat.
     BUFFER-COPY tt-cat TO smanbcat.     
  END.
  
  ASSIGN smanbcat.budget-amt = tt-cat.budget-amt
         smanbcat.msf = tt-cat.msf
         smanbcat.tons = tt-cat.tons.

  /* update smanbugt from smanbcat */
  FIND FIRST bf-bugt WHERE bf-bugt.company = smanbcat.company
                       AND bf-bugt.sman = smanbcat.sman
                       AND bf-bugt.budget-yr = smanbcat.budget-yr NO-ERROR.
  ASSIGN bf-bugt.budget-amt[smanbcat.budget-period] = 0
         bf-bugt.msf[smanbcat.budget-period] = 0
         bf-bugt.ton[smanbcat.budget-period] = 0
         .
  FOR EACH bf-bcat WHERE bf-bcat.company = smanbcat.company
                     AND bf-bcat.sman = smanbcat.sman
                     AND bf-bcat.budget-yr = smanbcat.budget-yr
                     AND bf-bcat.budget-period = smanbcat.budget-period NO-LOCK:

      ASSIGN bf-bugt.budget-amt[bf-bcat.budget-period] = bf-bugt.budget-amt[bf-bcat.budget-period] +
                                                         bf-bcat.budget-amt
             bf-bugt.msf[bf-bcat.budget-period] = bf-bugt.msf[bf-bcat.budget-period] +
                                                         bf-bcat.msf
             bf-bugt.ton[bf-bcat.budget-period] = bf-bugt.ton[bf-bcat.budget-period] +
                                                         bf-bcat.ton
             .

      
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN get-period IN WIDGET-HANDLE(char-hdl) (OUTPUT v-prd).

  ASSIGN tt-cat.company = g_company
         tt-cat.sman = smanbugt.sman
         tt-cat.budget-yr = smanbugt.budget-yr
         tt-cat.budget-period = v-prd.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF buffer bf-bugt FOR smanbugt.
  DEF buffer bf-bcat FOR smanbcat.

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/askdel.i}
  IF AVAIL tt-cat THEN
     FIND FIRST smanbcat WHERE smanbcat.company = tt-cat.company
                           AND smanbcat.sman = tt-cat.sman
                           AND smanbcat.budget-yr = tt-cat.budget-yr
                           AND smanbcat.budget-period = tt-cat.budget-period
                           AND smanbcat.procat = tt-cat.procat  NO-ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  IF AVAIL smanbcat THEN DELETE smanbcat.
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN get-period IN WIDGET-HANDLE(char-hdl) (OUTPUT v-prd).

  FIND FIRST bf-bugt WHERE bf-bugt.company = smanbugt.company
                       AND bf-bugt.sman = smanbugt.sman
                       AND bf-bugt.budget-yr = smanbugt.budget-yr NO-ERROR.
  
  ASSIGN bf-bugt.budget-amt[v-prd] = 0
         bf-bugt.msf[v-prd] = 0
         bf-bugt.ton[v-prd] = 0
         .
                          
  FOR EACH bf-bcat WHERE bf-bcat.company = smanbugt.company
                     AND bf-bcat.sman = smanbugt.sman
                     AND bf-bcat.budget-yr = smanbugt.budget-yr
                     AND bf-bcat.budget-period = v-prd NO-LOCK:

      ASSIGN bf-bugt.budget-amt[bf-bcat.budget-period] = bf-bugt.budget-amt[bf-bcat.budget-period] +
                                                         bf-bcat.budget-amt
             bf-bugt.msf[bf-bcat.budget-period] = bf-bugt.msf[bf-bcat.budget-period] +
                                                         bf-bcat.msf
             bf-bugt.ton[bf-bcat.budget-period] = bf-bugt.ton[bf-bcat.budget-period] +
                                                         bf-bcat.ton
             .
  END.
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(smanbugt)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY 'entry' TO tt-cat.procat IN BROWSE {&browse-name}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  RUN dispatch ('open-query').

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT CAN-FIND(FIRST fgcat WHERE fgcat.company = g_company 
                    AND fgcat.procat = tt-cat.procat:SCREEN-VALUE IN BROWSE {&browse-name})
  THEN DO:
        MESSAGE "Invalid category.  Try help." VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO tt-cat.procat IN BROWSE {&browse-name}.
        RETURN NO-APPLY.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(smanbugt)).
  
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
  {src/adm/template/snd-list.i "smanbugt"}
  {src/adm/template/snd-list.i "tt-cat"}

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

