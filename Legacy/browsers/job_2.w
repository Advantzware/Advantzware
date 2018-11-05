&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/job.w

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
DEFINE VARIABLE job-number AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttbl NO-UNDO
  FIELD job-no AS CHARACTER FORMAT 'X(9)' LABEL 'Job'
  FIELD est-no AS CHARACTER FORMAT 'X(5)' LABEL 'Estimate'
  FIELD job-rowid AS ROWID
        INDEX ttbl IS PRIMARY UNIQUE
              job-no
              est-no.


DEF BUFFER bf-ttbl FOR ttbl.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttbl

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table ttbl.job-no ttbl.est-no   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH ttbl NO-LOCK
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH ttbl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br_table ttbl
&Scoped-define FIRST-TABLE-IN-QUERY-br_table ttbl


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table lv-job-no btn_showall lv-est-no ~
CloseDate selected-option 
&Scoped-Define DISPLAYED-OBJECTS closed-jobs lv-job-no lv-est-no CloseDate ~
selected-option 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn_Selections Btn_All 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|asi.job.company
e-num||y|asi.job.e-num
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,e-num"':U).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Jobs_Exist B-table-Win 
FUNCTION Jobs_Exist RETURNS LOGICAL
  (job_no AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_All 
     LABEL "&All Jobs" 
     SIZE 15 BY 1.14 TOOLTIP "All".

DEFINE BUTTON Btn_Selections 
     LABEL "&Selections" 
     SIZE 15 BY 1.14 TOOLTIP "Selections".

DEFINE BUTTON btn_showall 
     LABEL "Show All" 
     SIZE 10 BY 1.67.

DEFINE VARIABLE selected-option AS CHARACTER FORMAT "X(256)":U INITIAL "Select Option ..." 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "?"
     DROP-DOWN-LIST
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CloseDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Closed Date" 
     VIEW-AS FILL-IN 
     SIZE 16.2 BY .91
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lv-est-no AS CHARACTER FORMAT "X(5)":U 
     LABEL "Est#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-job-no AS CHARACTER FORMAT "xxxxxx-xx":U 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE closed-jobs AS LOGICAL INITIAL no 
     LABEL "Show Closed Jobs" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      ttbl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      ttbl.job-no
      ttbl.est-no
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 30 BY 15.48
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     closed-jobs AT ROW 14.1 COL 5
     lv-job-no AT ROW 16.48 COL 15 COLON-ALIGNED
     btn_showall AT ROW 16.71 COL 1
     lv-est-no AT ROW 17.43 COL 15 COLON-ALIGNED
     CloseDate AT ROW 18.38 COL 2.2
     selected-option AT ROW 19.33 COL 1 NO-LABEL
     Btn_Selections AT ROW 20.52 COL 1 HELP
          "ADD selected Job to Both CADCAM and Scheduling Database"
     Btn_All AT ROW 20.52 COL 16 HELP
          "ADD selected Job to Both CADCAM and Scheduling Database"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 
         TITLE "AdvanceWare Jobs".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
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
         HEIGHT             = 21.67
         WIDTH              = 39.
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

/* SETTINGS FOR BUTTON Btn_All IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON Btn_Selections IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR TOGGLE-BOX closed-jobs IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       closed-jobs:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN CloseDate IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX selected-option IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttbl NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _TblOptList       = "USED"
     _Where[1]         = "NOT CAN-DO('c,z',asi.job.stat)"
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


&Scoped-define SELF-NAME Btn_All
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_All B-table-Win
ON CHOOSE OF Btn_All IN FRAME F-Main /* All Jobs */
DO:
  MESSAGE 'Depending on the number of Jobs, this process may be time consuming'
      SKIP(1) 'Continue?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE continue AS LOGICAL.
  IF continue THEN
  RUN Selected_Option (SELF:TOOLTIP,selected-option:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Selections
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Selections B-table-Win
ON CHOOSE OF Btn_Selections IN FRAME F-Main /* Selections */
DO:
  RUN Selected_Option (SELF:TOOLTIP,selected-option:SCREEN-VALUE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME closed-jobs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL closed-jobs B-table-Win
ON VALUE-CHANGED OF closed-jobs IN FRAME F-Main /* Show Closed Jobs */
DO:
  RUN Build_TTBL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CloseDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CloseDate B-table-Win
ON HELP OF CloseDate IN FRAME F-Main /* Closed Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CloseDate B-table-Win
ON LEAVE OF CloseDate IN FRAME F-Main /* Closed Date */
DO:
  IF SELF:MODIFIED THEN
    RUN Build_TTBL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CloseDate B-table-Win
ON RETURN OF CloseDate IN FRAME F-Main /* Closed Date */
DO:
  APPLY "LEAVE" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-job-no B-table-Win
ON LEAVE OF lv-job-no IN FRAME F-Main /* Job# */
DO:
  DEF VAR v-job-no AS cha NO-UNDO.
  DEF VAR v-job-no2 AS INT NO-UNDO.
  ASSIGN v-job-no = substring(self:screen-value,1,index("-",SELF:SCREEN-VALUE) - 1)
         v-job-no2 = INT(SUBSTRING(SELF:SCREEN-VALUE,index("-",SELF:SCREEN-VALUE) + 1,2))
         .
  FIND FIRST bf-ttbl WHERE bf-ttbl.job-no = lv-job-no:SCREEN-VALUE .
  IF AVAIL bf-ttbl THEN REPOSITION {&browse-name} TO ROWID ROWID(bf-ttbl) NO-ERROR.
  ELSE DO:
      FIND FIRST job WHERE job.company = g_company
                       AND job.job-no = v-job-no AND job.job-no2 = v-job-no2 
                       NO-LOCK NO-ERROR.
      IF AVAIL job THEN DO:
         IF (index("CZ",job.stat) eq 0
             or job.close-date gt closedate
             or job.close-date eq ? )  THEN DO:
         END.
         ELSE DO:
             MESSAGE "Job's closed." VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
         END.

         FIND jobs WHERE jobs.job = job.job-no + '-' + STRING(job.job-no2)
              NO-LOCK NO-ERROR.
         IF avail jobs THEN
         CASE optconfg.loadtype:
              WHEN 'CADCAM' THEN IF jobs.cadcam_status NE '' THEN RETURN NO-APPLY.
              WHEN 'BOTH' THEN 
                    IF jobs.cadcam_status NE '' OR jobs.scheduling_status NE '' 
                       THEN RETURN NO-APPLY.
               WHEN 'SCHEDULING' THEN IF jobs.scheduling_status NE '' THEN RETURN NO-APPLY.
         END CASE.
         CREATE bf-ttbl.
         ASSIGN
             bf-ttbl.job-no = job.job-no + '-' + STRING(job.job-no2)
             bf-ttbl.est-no = job.est-no
             bf-ttbl.job-rowid = ROWID(job).
         {&OPEN-QUERY-{&BROWSE-NAME}}
         REPOSITION {&browse-name} TO ROWID ROWID(bf-ttbl) NO-ERROR.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selected-option
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selected-option B-table-Win
ON VALUE-CHANGED OF selected-option IN FRAME F-Main
DO:
  RUN Build_TTBL.
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Build_TTBL B-table-Win 
PROCEDURE Build_TTBL :
/*------------------------------------------------------------------------------
  Purpose:     build table ttbl used by browser based on selected-option value
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i AS INT NO-UNDO.

  ENABLE {&LIST-6} WITH FRAME {&FRAME-NAME}.
  FOR EACH ttbl EXCLUSIVE-LOCK:
    DELETE ttbl.
  END.
  assign CloseDate.
  FIND optconfg
       WHERE optconfg.description = selected-option:SCREEN-VALUE IN FRAME {&FRAME-NAME}
       NO-LOCK NO-ERROR.
  IF avail optconfg THEN
  FOR EACH job
      WHERE (job.job-no <> "" OR job.est-no <> "")
         AND (index("CZ",job.stat) eq 0
         or job.close-date gt closedate
         or job.close-date eq ? )
      NO-LOCK WITH FRAME {&FRAME-NAME} BY job.job-no DESCENDING:

    FIND jobs WHERE jobs.job = job.job-no + '-' + STRING(job.job-no2)
              NO-LOCK NO-ERROR.
    IF avail jobs THEN
    CASE optconfg.loadtype:
      WHEN 'CADCAM' THEN
        IF jobs.cadcam_status NE '' THEN
        NEXT.
      WHEN 'BOTH' THEN
        IF jobs.cadcam_status NE '' OR jobs.scheduling_status NE '' THEN
        NEXT.
      WHEN 'SCHEDULING' THEN
        IF jobs.scheduling_status NE '' THEN
        NEXT.
    END CASE.
    CREATE ttbl.
    ASSIGN
      ttbl.job-no = job.job-no + '-' + STRING(job.job-no2)
      ttbl.est-no = job.est-no
      ttbl.job-rowid = ROWID(job).
    i = i + 1.
    IF i > 30 THEN LEAVE.  /* get last 30 jobs */
  END.
  ELSE
  DISABLE {&LIST-6} WITH FRAME {&FRAME-NAME}.
  {&OPEN-QUERY-{&BROWSE-NAME}}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Options B-table-Win 
PROCEDURE Get_Options :
/*------------------------------------------------------------------------------
  Purpose:     load option configurations
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE list-items AS CHARACTER NO-UNDO.


  /*list-items = 'Select Option ...'.*/
  list-items = "".
  FOR EACH optconfg NO-LOCK WHERE optconfg.export_job = yes:
    list-items = if list-items = "" then optconfg.description
                                    else list-items + ',' + optconfg.description.
  END.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      selected-option:LIST-ITEMS = list-items
      selected-option:SCREEN-VALUE = selected-option:ENTRY(1)
      selected-option:INNER-LINES = selected-option:NUM-ITEMS
      CloseDate:SCREEN-VALUE = string(TODAY,"99/99/9999").
      apply "value-changed" to selected-option.
  END.

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
  IF selected-option:NUM-ITEMS IN FRAME {&FRAME-NAME} = 0 THEN
  RUN Get_Options.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Selected_Option B-table-Win 
PROCEDURE Selected_Option :
/*------------------------------------------------------------------------------
  Purpose:     Add selected Job to database and process selected option
  Parameters:  Input Selected Option
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER button-selected AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER option-selected AS CHARACTER NO-UNDO.

  DEFINE VARIABLE phandle AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
  DEFINE VARIABLE jobs-rowid AS ROWID NO-UNDO.
  
  ldummy = SESSION:SET-WAIT-STATE('General').
  IF VALID-HANDLE(adm-broker-hdl) THEN
  DO WITH FRAME {&FRAME-NAME}:
    RUN get-link-handle IN adm-broker-hdl
        (THIS-PROCEDURE,'CONTAINER':U,OUTPUT char-hdl).
    phandle = WIDGET-HANDLE(char-hdl).
    IF VALID-HANDLE(phandle) THEN
    CASE button-selected:
      WHEN 'Selections' THEN
      DO i = 1 TO {&BROWSE-NAME}:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
        ldummy = {&BROWSE-NAME}:FETCH-SELECTED-ROW(i).
        RUN Job_Load IN phandle (option-selected,ttbl.job-rowid,OUTPUT jobs-rowid) no-error.
        if error-status:error then next.  /* ysk 10/26/01 error check */
      END.
      WHEN 'All' THEN
      FOR EACH ttbl NO-LOCK:
        RUN Job_Load IN phandle (option-selected,ttbl.job-rowid,OUTPUT jobs-rowid) no-error.
        if error-status:error then next.  /* ysk 10/26/01 error check */
      END.
    END CASE.
  END.
  IF VALID-HANDLE(phandle) THEN  RUN Open_Jobs_Query IN phandle (jobs-rowid).
  RUN Build_TTBL.
  ldummy = SESSION:SET-WAIT-STATE('').

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
  {src/adm/template/sndkycas.i "company" "job" "company"}
  {src/adm/template/sndkycas.i "e-num" "job" "e-num"}

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
  {src/adm/template/snd-list.i "ttbl"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-all B-table-Win 
PROCEDURE show-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 ENABLE {&LIST-6} WITH FRAME {&FRAME-NAME}.
  FOR EACH ttbl EXCLUSIVE-LOCK:
    DELETE ttbl.
  END.
  assign CloseDate.
  FIND optconfg
       WHERE optconfg.description = selected-option:SCREEN-VALUE IN FRAME {&FRAME-NAME}
       NO-LOCK NO-ERROR.
  IF avail optconfg THEN
  FOR EACH job
      WHERE (job.job-no <> "" OR job.est-no <> "")
         AND (index("CZ",job.stat) eq 0
         or job.close-date gt closedate
         or job.close-date eq ? )
      NO-LOCK WITH FRAME {&FRAME-NAME}:

    FIND jobs WHERE jobs.job = job.job-no + '-' + STRING(job.job-no2)
              NO-LOCK NO-ERROR.
    IF avail jobs THEN
    CASE optconfg.loadtype:
      WHEN 'CADCAM' THEN
        IF jobs.cadcam_status NE '' THEN
        NEXT.
      WHEN 'BOTH' THEN
        IF jobs.cadcam_status NE '' OR jobs.scheduling_status NE '' THEN
        NEXT.
      WHEN 'SCHEDULING' THEN
        IF jobs.scheduling_status NE '' THEN
        NEXT.
    END CASE.
    CREATE ttbl.
    ASSIGN
      ttbl.job-no = job.job-no + '-' + STRING(job.job-no2)
      ttbl.est-no = job.est-no
      ttbl.job-rowid = ROWID(job).
  END.
  ELSE
  DISABLE {&LIST-6} WITH FRAME {&FRAME-NAME}.
  {&OPEN-QUERY-{&BROWSE-NAME}}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Jobs_Exist B-table-Win 
FUNCTION Jobs_Exist RETURNS LOGICAL
  (job_no AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN CAN-FIND(jobs WHERE jobs.job = job_no).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

