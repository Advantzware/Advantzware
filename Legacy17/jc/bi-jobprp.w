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

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR v-dscr LIKE prep-dscr NO-UNDO.
DEF VAR v-cost AS DECIMAL NO-UNDO.

DEF VAR v-acceptable-codes AS CHAR NO-UNDO.
ASSIGN
   v-acceptable-codes = "MisM1,MisM2,MisM3,MisM4,MisM5,MisM6,MisM7,MisM8,MisM9,MisL1,MisL2,MisL3,MisL4,MisL5,MisL6,MisL7,MisL8,MisL9".

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
&Scoped-define EXTERNAL-TABLES job
&Scoped-define FIRST-EXTERNAL-TABLE job


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR job.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES job-prep

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table job-prep.frm job-prep.blank-no ~
job-prep.qty job-prep.code print-dscr (1) @ v-dscr job-prep.std-cost ~
job-prep.ml job-prep.simon job-prep.cost-m print-cost (1) @ v-cost
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table job-prep.frm ~
job-prep.blank-no job-prep.qty job-prep.code job-prep.std-cost job-prep.ml ~
job-prep.simon job-prep.cost-m 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table job-prep
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table job-prep
&Scoped-define QUERY-STRING-br_table FOR EACH job-prep WHERE job-prep.company eq job.company and ~
job-prep.job eq job.job and ~
job-prep.job-no eq job.job-no and ~
job-prep.job-no2 eq job.job-no2 NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH job-prep WHERE job-prep.company eq job.company and ~
job-prep.job eq job.job and ~
job-prep.job-no eq job.job-no and ~
job-prep.job-no2 eq job.job-no2 NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table job-prep
&Scoped-define FIRST-TABLE-IN-QUERY-br_table job-prep


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
company||y|ASI.job-prep.company
code||y|ASI.job-prep.code
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,code"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD print-dscr B-table-Win 
FUNCTION print-dscr RETURNS CHARACTER
  ( INPUT ip-type AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD print-cost B-table-Win 
FUNCTION print-cost RETURNS DECIMAL
  ( INPUT ip-type AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      job-prep SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      job-prep.frm COLUMN-LABEL "S" FORMAT ">>>":U WIDTH 4 COLUMN-FONT 0
      job-prep.blank-no COLUMN-LABEL "B" FORMAT ">>>":U WIDTH 4
            COLUMN-FONT 0
      job-prep.qty FORMAT "->>>,>>>,>>9.9<<<<<":U WIDTH 12 COLUMN-FONT 0
      job-prep.code FORMAT "x(15)":U WIDTH 16
      print-dscr (1) @ v-dscr COLUMN-LABEL "Description" FORMAT "x(20)":U
            WIDTH 30
      job-prep.std-cost COLUMN-LABEL "Std Cost" FORMAT "->>>,>>9.99<<":U
            WIDTH 16
      job-prep.ml FORMAT "M/L":U
      job-prep.simon FORMAT "X":U
      job-prep.cost-m COLUMN-LABEL "Std Cost/M" FORMAT "->>>,>>9.99<<":U
      print-cost (1) @ v-cost COLUMN-LABEL "Actual Cost" FORMAT "->>>,>>9.99<<":U
         

  ENABLE
      job-prep.frm
      job-prep.blank-no
      job-prep.qty
      job-prep.code
      job-prep.std-cost
      job-prep.ml
      job-prep.simon
      job-prep.cost-m
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 12.62
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
   External Tables: ASI.job
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
         HEIGHT             = 13.1
         WIDTH              = 145.6.
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
     _TblList          = "ASI.job-prep WHERE ASI.job ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "job-prep.company eq job.company and
job-prep.job eq job.job and
job-prep.job-no eq job.job-no and
job-prep.job-no2 eq job.job-no2"
     _FldNameList[1]   > ASI.job-prep.frm
"job-prep.frm" "S" ">>>" "integer" ? ? 0 ? ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.job-prep.blank-no
"job-prep.blank-no" "B" ">>>" "integer" ? ? 0 ? ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.job-prep.qty
"job-prep.qty" ? ? "decimal" ? ? 0 ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.job-prep.code
"job-prep.code" ? "x(15)" "character" ? ? ? ? ? ? yes ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"print-dscr (1) @ v-dscr" "Description" "x(20)" ? ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.job-prep.std-cost
"job-prep.std-cost" "Std Cost" "->>>,>>9.99<<" "decimal" ? ? ? ? ? ? yes ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.job-prep.ml
"job-prep.ml" ? ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.job-prep.simon
"job-prep.simon" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.job-prep.cost-m
"job-prep.cost-m" "Std Cost/M" "->>>,>>9.99<<" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
_FldNameList[10]   > "_<CALC>"
"print-cost (1) @ v-cost" "Actual Cost" "x(20)" ? ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
  DEF VAR hlp-recid AS RECID NO-UNDO.
  DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.

   
  lw-focus = FOCUS.

  CASE lw-focus:NAME :
    WHEN "code" THEN DO:
      RUN windows/l-prep.w (cocode, lw-focus:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND lw-focus:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
        RUN new-code.
      END.
    END.
  END.

  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME job-prep.frm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-prep.frm br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-prep.frm IN BROWSE br_table /* S */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-frm NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-prep.blank-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-prep.blank-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-prep.blank-no IN BROWSE br_table /* B */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-blank-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-prep.code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-prep.code br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-prep.code IN BROWSE br_table /* Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-prep.code br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF job-prep.code IN BROWSE br_table /* Code */
DO:
  RUN new-code.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-prep.simon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-prep.simon br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-prep.simon IN BROWSE br_table /* SIMON */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-simon NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
DO:
 RUN pc/w-mismat.w .
 
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

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST prep
      WHERE prep.company EQ cocode
        AND prep.code    EQ job-prep.code
      NO-LOCK NO-ERROR.
  IF AVAIL prep THEN job-prep.sc-uom = prep.uom.

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
  create job-prep.
  assign
   job-prep.company = job.company
   job-prep.job     = job.job
   job-prep.job-no  = job.job-no
   job-prep.job-no2 = job.job-no2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  {methods/winReSizeLocInit.i}

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT AVAIL job-prep THEN RUN dispatch ('open-query').
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
  RUN valid-frm NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-blank-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  RUN valid-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  RUN valid-simon NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-code B-table-Win 
PROCEDURE new-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
   IF NOT CAN-DO(v-acceptable-codes, TRIM(job-prep.code:SCREEN-VALUE IN BROWSE {&browse-name})) THEN DO:
      FIND prep WHERE prep.company EQ cocode
                  AND prep.loc     EQ locode
                  AND prep.code    EQ job-prep.code:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
      IF AVAIL prep THEN
         ASSIGN
            job-prep.simon:SCREEN-VALUE IN BROWSE {&browse-name}    = prep.simon
            job-prep.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(prep.cost)
            job-prep.ml:SCREEN-VALUE IN BROWSE {&browse-name}       = STRING(prep.ml)
            v-dscr                                                  = print-dscr (2).
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "job-prep" "company"}
  {src/adm/template/sndkycas.i "code" "job-prep" "code"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "job-prep"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-blank-no B-table-Win 
PROCEDURE valid-blank-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF INT(job-prep.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:
      RELEASE ef.

      FIND FIRST job-hdr
          WHERE job-hdr.company  EQ cocode
            AND job-hdr.job      EQ job-prep.job
            AND job-hdr.job-no   EQ job-prep.job-no
            AND job-hdr.job-no2  EQ job-prep.job-no2
            AND job-hdr.frm      EQ INT(job-prep.frm:SCREEN-VALUE IN BROWSE {&browse-name})
            AND job-hdr.blank-no EQ INT(job-prep.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.

      IF NOT AVAIL job-hdr AND job.est-no NE "" THEN
      FIND FIRST eb
          WHERE eb.company  EQ cocode
            AND eb.est-no   EQ job.est-no
            AND eb.form-no  EQ INT(job-prep.frm:SCREEN-VALUE IN BROWSE {&browse-name})
            AND eb.blank-no EQ INT(job-prep.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.

      IF NOT AVAIL job-hdr AND NOT AVAIL ef THEN DO:
        MESSAGE "Must enter a valid blank..."
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO job-prep.blank-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-code B-table-Win 
PROCEDURE valid-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    job-prep.code:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(job-prep.code:SCREEN-VALUE IN BROWSE {&browse-name}).

   IF NOT CAN-DO(v-acceptable-codes, TRIM(job-prep.code:SCREEN-VALUE IN BROWSE {&browse-name})) THEN DO:
      FIND FIRST prep WHERE prep.company EQ cocode
                        AND prep.loc     EQ locode
                        AND prep.code    EQ job-prep.code:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
      IF NOT AVAIL prep THEN DO:
         MESSAGE TRIM(job-prep.code:LABEL IN BROWSE {&browse-name}) +
              " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO job-prep.code IN BROWSE {&browse-name}.
         RETURN ERROR.
      END.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-frm B-table-Win 
PROCEDURE valid-frm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    RELEASE ef.

    FIND FIRST job-hdr
        WHERE job-hdr.company EQ cocode
          AND job-hdr.job     EQ job-prep.job
          AND job-hdr.job-no  EQ job-prep.job-no
          AND job-hdr.job-no2 EQ job-prep.job-no2
          AND job-hdr.frm     EQ INT(job-prep.frm:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.

    IF NOT AVAIL job-hdr AND job.est-no NE "" THEN
    FIND FIRST ef
        WHERE ef.company EQ cocode
          AND ef.est-no  EQ job.est-no
          AND ef.form-no EQ INT(job-prep.frm:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.

    IF NOT AVAIL job-hdr AND NOT AVAIL ef THEN DO:
      MESSAGE "Must enter a valid form..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job-prep.frm IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-simon B-table-Win 
PROCEDURE valid-simon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    job-prep.simon:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(job-prep.simon:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF INDEX("SIMON",job-prep.simon:SCREEN-VALUE IN BROWSE {&browse-name}) LE 0
    THEN DO:
      MESSAGE TRIM(job-prep.simon:LABEL IN BROWSE {&browse-name}) +
              " must be S, I, M, O, or N..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job-prep.simon IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION print-cost B-table-Win 
FUNCTION print-cost RETURNS DECIMAL
  ( INPUT ip-type AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN v-cost = 0 .
for each misc-act
    where misc-act.company eq cocode
      and misc-act.job     eq job.job
      AND misc-act.m-code  EQ job-prep.CODE 
    no-lock:

    assign
        v-cost = v-cost + misc-act.cost.
   
END.

 RETURN v-cost.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION print-dscr B-table-Win 
FUNCTION print-dscr RETURNS CHARACTER
  ( INPUT ip-type AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

FIND FIRST prep WHERE prep.company EQ cocode
                  AND prep.loc     EQ locode
                  AND prep.code    EQ IF ip-type EQ 1 THEN job-prep.CODE ELSE job-prep.CODE:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.

IF AVAIL prep THEN 
   v-dscr = prep.dscr.
ELSE DO:
   v-dscr = "".
   FIND FIRST ef WHERE ef.est-no  EQ job.est-no
                   AND ef.company EQ job.company
                   AND ef.form-no EQ job-prep.frm NO-LOCK NO-ERROR.
      
   IF AVAIL ef THEN DO:
      IF SUBSTR(job-prep.code,1,3)  EQ "MIS"  AND
        (SUBSTR(job-prep.code,5,1)  GE "1"    AND
         SUBSTR(job-prep.code,5,1)  LE "5")   THEN
         v-dscr = ef.mis-cost[int(substr(job-prep.code,5,1))].
      
      IF SUBSTR(job-prep.code,4,1) EQ "M" THEN
         v-dscr = v-dscr + " - Mat".
      ELSE
         IF SUBSTR(job-prep.code,4,1) EQ "L" THEN
            v-dscr = v-dscr + " - Lab".
   END.
END.

RETURN v-dscr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
