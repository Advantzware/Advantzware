&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartNavBrowser Object Template

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
{methods/defines/hndldefs.i}
{methods/defines/globdefs.i}
{methods/prgsecd3.i "p-tchupd."}

DEF TEMP-TABLE tt-tran LIKE machtran
                       FIELD rec-id AS RECID
                       FIELD startx AS cha
                       FIELD endx AS cha
                       FIELD totalx AS cha
                       FIELD tot-run AS INT.

DEFINE VARIABLE ttRowID AS ROWID NO-UNDO.
DEFINE VARIABLE start-time AS CHARACTER NO-UNDO.
DEFINE VARIABLE end-time AS CHARACTER NO-UNDO.
DEFINE VARIABLE total-time AS CHARACTER NO-UNDO.
DEFINE VARIABLE cellColumn AS HANDLE NO-UNDO EXTENT 20.
DEFINE VARIABLE company_code AS CHARACTER NO-UNDO.
DEFINE VARIABLE machine_code AS CHARACTER NO-UNDO.
DEFINE VARIABLE job_number AS CHARACTER NO-UNDO.
DEFINE VARIABLE job_sub AS CHARACTER NO-UNDO.
DEFINE VARIABLE form_number AS CHARACTER NO-UNDO.
DEFINE VARIABLE blank_number AS CHARACTER NO-UNDO.
DEFINE VARIABLE pass_sequence AS CHARACTER NO-UNDO.
DEFINE VARIABLE label_language AS CHARACTER NO-UNDO.
DEFINE VARIABLE language_list AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-run-total AS INTEGER
  LABEL "Total Run" FORMAT "->>,>>>,>>9" NO-UNDO.
DEF VAR v-rec-key-list AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-tran machtran

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table machtran.charge_code tt-tran.job_number tt-tran.job_sub NO-LABEL tt-tran.start_date tt-tran.startx tt-tran.end_date tt-tran.shift tt-tran.endx tt-tran.run_qty tt-tran.waste_qty tt-tran.totalx tt-tran.completed tt-tran.tot-run   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table tt-tran.start_date tt-tran.startx ~
   tt-tran.end_date tt-tran.endx ~
   tt-tran.run_qty ~
   tt-tran.waste_qty tt-tran.completed   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table tt-tran
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table tt-tran
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH tt-tran, ~
            EACH machtran WHERE RECID(machtran) = tt-tran.rec-id
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH tt-tran, ~
            EACH machtran WHERE RECID(machtran) = tt-tran.rec-id.
&Scoped-define TABLES-IN-QUERY-br_table tt-tran machtran
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-tran
&Scoped-define SECOND-TABLE-IN-QUERY-br_table machtran


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
      tt-tran, 
      machtran SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      machtran.charge_code
     tt-tran.job_number
     tt-tran.job_sub NO-LABEL 
     tt-tran.start_date
     tt-tran.startx LABEL "Log In" /*FORM "99:99"*/
     tt-tran.end_date
     tt-tran.shift FORM "99"
     tt-tran.endx LABEL "Log Out" /*FORM "99:99"*/
     tt-tran.run_qty
     tt-tran.waste_qty
     tt-tran.totalx LABEL "Total" /*FORM "99:99"*/
     tt-tran.completed
     tt-tran.tot-run  LABEL "Total Run"
     ENABLE tt-tran.start_date tt-tran.startx 
            tt-tran.end_date tt-tran.endx
            tt-tran.run_qty
            tt-tran.waste_qty tt-tran.completed
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 115 BY 7.86
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
   Type: SmartNavBrowser
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
         HEIGHT             = 7.86
         WIDTH              = 115.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/navbrows.i}
{touch/translations.i}

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
OPEN QUERY {&SELF-NAME} FOR EACH tt-tran,
     EACH machtran WHERE RECID(machtran) = tt-tran.rec-id.
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
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
    RUN select-page IN WIDGET-HANDLE(char-hdl) (2).
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
     by pressing the Save button on an Update SmartPanel.
   {src/adm/template/brsleave.i} */

    if keyfunction(lastkey) = "page-up" or 
      keyfunction(lastkey) = "page-down" or
      keyfunction(lastkey) = "cursor-up" or
      keyfunction(lastkey) = "cursor-down" 
   then do:
  
      return no-apply.
   end.
 
   {est/brsleave.i}   /* same but update will be like add 
                         need to run set-attribute-list ("adm-new-record = 'no' ")
                         in local-update-record  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  ttRowID = IF AVAILABLE tt-tran THEN ROWID(tt-tran) ELSE ?.

  IF AVAIL tt-tran THEN
     RUN paper-clip-image-proc(INPUT tt-tran.rec_key).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


ASSIGN
      tt-tran.start_date:READ-ONLY IN BROWSE {&browse-name} = YES
      tt-tran.startx:READ-ONLY IN BROWSE {&browse-name} = YES
      tt-tran.end_date:READ-ONLY IN BROWSE {&browse-name} = YES
      tt-tran.endx:READ-ONLY IN BROWSE {&browse-name} = YES.

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

 lv-run-total = 0.
/*
 {methods/run_link.i "CONTAINER" "Get-Values"
    "(OUTPUT company_code,OUTPUT machine_code,OUTPUT job_number,OUTPUT job_sub,
      OUTPUT form_number, OUTPUT blank_number,OUTPUT pass_sequence)"}.
*/
 FOR EACH tt-tran.
     DELETE tt-tran.
 END.

 RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source", OUTPUT char-hdl).
 RUN get-values IN WIDGET-HANDLE(char-hdl) (OUTPUT company_code,OUTPUT machine_code,OUTPUT job_number,OUTPUT job_sub,
      OUTPUT form_number, OUTPUT blank_number,OUTPUT pass_sequence,OUTPUT label_language).

FOR EACH machtran WHERE machtran.company = company_code AND
                        machtran.machine = machine_code AND
                        machtran.job_number = job_number AND
                        machtran.job_sub = INTEGER(job_sub) AND
                        machtran.form_number = INTEGER(form_number) AND
                        machtran.blank_number = INTEGER(blank_number) AND
                        machtran.pass_sequence = INTEGER(pass_sequence) NO-LOCK:
    
    CREATE tt-tran.
    BUFFER-COPY machtran TO tt-tran.
    ASSIGN lv-run-total    = lv-run-total + machtran.run_qty
           tt-tran.rec-id  = RECID(machtran)
           tt-tran.startx  = DYNAMIC-FUNCTION('sfCommon_TimeDisplay', machtran.start_time, YES, NO)
           tt-tran.endx    = DYNAMIC-FUNCTION('sfCommon_TimeDisplay', machtran.end_time, YES, NO)
           tt-tran.totalx  = DYNAMIC-FUNCTION('sfCommon_TimeDisplay', machtran.total_time, NO, NO)
           tt-tran.tot-run = lv-run-total
           .
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-navigation B-table-Win 
PROCEDURE Disable-navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-navigation B-table-Win 
PROCEDURE Enable-navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCellColumns B-table-Win 
PROCEDURE getCellColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE cellWidth AS DECIMAL NO-UNDO.

  DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i).
    IF cellColumn[i]:LABEL EQ ? OR
       cellColumn[i]:LABEL EQ translate(cellColumn[i]:LABEL,NO) THEN NEXT.
    cellColumn[i]:LABEL = translate(cellColumn[i]:LABEL,NO).
    cellWidth = LENGTH(cellColumn[i]:LABEL) * 1.5.
    IF cellWidth GT cellColumn[i]:WIDTH-CHARS THEN
    cellColumn[i]:WIDTH-CHARS = cellWidth.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Value B-table-Win 
PROCEDURE Get_Value :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipFieldName AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opLabelLanguage AS CHARACTER NO-UNDO.

  opLabelLanguage = label_language.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-hour AS INT NO-UNDO.
  DEF VAR v-min AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF machtran.run_qty <> tt-tran.run_qty OR
     machtran.waste_qty <> tt-tran.waste_qty OR
     machtran.completed <> tt-tran.completed
  THEN DO:
      FIND CURRENT machtran EXCLUSIVE-LOCK.

      IF machtran.run_qty <> tt-tran.run_qty THEN machtran.run_qty = tt-tran.run_qty.
      IF machtran.waste_qty <> tt-tran.waste_qty THEN machtran.waste_qty = tt-tran.waste_qty.
      IF machtran.completed <> tt-tran.completed THEN machtran.completed = tt-tran.completed.

      RUN dispatch ('row-changed').
    END.
    RUN updateRouting (machtran.company,machtran.machine,machtran.job_number,
                       machtran.job_sub,machtran.form_number,machtran.blank_number).

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
  DEFINE VARIABLE allow-update AS LOGICAL NO-UNDO.

  {methods/run_link.i "CONTAINER-SOURCE" "Allow-Update" "(OUTPUT allow-update)"}
  IF NOT allow-update THEN
  RETURN "ADM-ERROR":U.

  IF tt-tran.posted THEN DO:
     MESSAGE "It's already Posted. Can't Update." VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "entry" TO tt-tran.start_date IN BROWSE {&browse-name}.

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
  RUN getCellColumns.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY 'VALUE-CHANGED':U TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-changed B-table-Win 
PROCEDURE local-row-changed :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-changed':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ttRowID = IF AVAILABLE tt-tran THEN ROWID(tt-tran) ELSE ?.

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  /*To update total run column*/
  RUN build-table.
  {&open-query-{&browse-name}}
  
  IF ttRowID <> ? THEN DO:
     REPOSITION {&browse-name} TO ROWID ttRowID NO-ERROR.
     RUN dispatch ('row-change').
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE paper-clip-image-proc B-table-Win 
PROCEDURE paper-clip-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.
   
   DEF VAR v-i-no AS CHAR NO-UNDO.
   DEF VAR v-est-no AS cha NO-UNDO.
   DEF VAR v-att AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

   {sys/ref/attachlogic.i}
  
   IF v-est-no <> "" AND v-i-no <> "" THEN
      v-att = CAN-FIND(FIRST asi.attach WHERE
              attach.company = company_code and
              (LOOKUP(attach.rec_key,v-rec-key-list) gt 0 AND
              (trim(attach.est-no) = trim(v-est-no)) or 
               (index(v-i-no,attach.i-no) > 0))).
   ELSE
      IF v-est-no <> "" /*AND v-i-no EQ ""*/ THEN
         v-att = CAN-FIND(FIRST asi.attach WHERE
              attach.company = company_code and
              (LOOKUP(attach.rec_key,v-rec-key-list) gt 0 AND
              (trim(attach.est-no) = trim(v-est-no)))).
   ELSE
      IF v-est-no EQ "" AND v-i-no <> "" THEN
         v-att = CAN-FIND(FIRST asi.attach WHERE
              attach.company = company_code and
              (index(v-i-no,attach.i-no) > 0)).

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-target':U, OUTPUT char-hdl).
  
   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN paper-clip-image IN WIDGET-HANDLE(char-hdl) (INPUT v-att).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query B-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID .

  RUN build-table.
  {&open-query-{&browse-name}}
  IF ttRowID <> ? THEN DO:
     REPOSITION {&browse-name} TO ROWID ttRowID NO-ERROR.
     RUN dispatch ('row-change').
  END.
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
  {src/adm/template/snd-list.i "tt-tran"}
  {src/adm/template/snd-list.i "machtran"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set_Value B-table-Win 
PROCEDURE Set_Value :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipFieldName AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipLanguageList AS CHARACTER NO-UNDO.

  language_list = ipLanguageList.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRouting B-table-Win 
PROCEDURE updateRouting :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {touch/updateRouting.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-proc B-table-Win 
PROCEDURE value-changed-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

