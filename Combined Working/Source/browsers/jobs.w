&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          jobs             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/jobs.w

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

DEFINE VARIABLE jobs_cadcam_status AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobs_scheduling_status AS CHARACTER NO-UNDO.
define buffer bf-jobs for jobs.  /* for auto-find */

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
&Scoped-define INTERNAL-TABLES jobs

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table jobs.job jobs.estimate ~
jobs.cadcam_status jobs.scheduling_status jobs.customer jobs.name 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH jobs ~
      WHERE ((jobs.cadcam_status = "Pending" ~
 OR jobs.scheduling_status = "Pending") ~
and toggle-1 = no) ~
 OR ((jobs.cadcam_status = "" ~
 AND jobs.scheduling_status = "") ~
and toggle-1 = no) ~
or ~
((jobs.cadcam_status = "Exported" or jobs.cadcam_status = "Printed")  ~
 AND jobs.scheduling_status <> "Pending" ~
and toggle-1 = yes)  ~
OR ~
 ((jobs.cadcam_status <> "Pending"  ~
AND (jobs.scheduling_status = "Exported" or scheduling_status = "Printed")) ~
and toggle-1 = yes) ~
 NO-LOCK
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH jobs ~
      WHERE ((jobs.cadcam_status = "Pending" ~
 OR jobs.scheduling_status = "Pending") ~
and toggle-1 = no) ~
 OR ((jobs.cadcam_status = "" ~
 AND jobs.scheduling_status = "") ~
and toggle-1 = no) ~
or ~
((jobs.cadcam_status = "Exported" or jobs.cadcam_status = "Printed")  ~
 AND jobs.scheduling_status <> "Pending" ~
and toggle-1 = yes)  ~
OR ~
 ((jobs.cadcam_status <> "Pending"  ~
AND (jobs.scheduling_status = "Exported" or scheduling_status = "Printed")) ~
and toggle-1 = yes) ~
 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br_table jobs
&Scoped-define FIRST-TABLE-IN-QUERY-br_table jobs


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table btn-clear auto-find TOGGLE-1 
&Scoped-Define DISPLAYED-OBJECTS auto-find TOGGLE-1 

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Get-Status B-table-Win 
FUNCTION Get-Status RETURNS CHARACTER
  (jobs_cadcam-status AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-clear 
     LABEL "Clear Search" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE auto-find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search  Job" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 TOOLTIP "Enter Job#." NO-UNDO.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL no 
     LABEL "Completed Jobs" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      jobs
    FIELDS(jobs.job
      jobs.estimate
      jobs.cadcam_status
      jobs.scheduling_status
      jobs.customer
      jobs.name) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      jobs.job FORMAT "X(9)":U
      jobs.estimate FORMAT "X(5)":U
      jobs.cadcam_status COLUMN-LABEL "CADCAM" FORMAT "X(8)":U
      jobs.scheduling_status COLUMN-LABEL "Scheduling" FORMAT "X(8)":U
      jobs.customer FORMAT "X(8)":U
      jobs.name FORMAT "X(31)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 115 BY 7.86
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     btn-clear AT ROW 8.76 COL 101
     auto-find AT ROW 8.86 COL 70 COLON-ALIGNED HELP
          "Enter Job#."
     TOGGLE-1 AT ROW 9.1 COL 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 
         TITLE "CADCAM Jobs".


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
         HEIGHT             = 9.91
         WIDTH              = 115.4.
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
     _TblList          = "jobs"
     _Options          = "NO-LOCK"
     _TblOptList       = "USED"
     _Where[1]         = "((jobs.cadcam_status = ""Pending""
 OR jobs.scheduling_status = ""Pending"")
and toggle-1 = no)
 OR ((jobs.cadcam_status = """"
 AND jobs.scheduling_status = """")
and toggle-1 = no)
or
((jobs.cadcam_status = ""Exported"" or jobs.cadcam_status = ""Printed"") 
 AND jobs.scheduling_status <> ""Pending""
and toggle-1 = yes) 
OR
 ((jobs.cadcam_status <> ""Pending"" 
AND (jobs.scheduling_status = ""Exported"" or jobs.scheduling_status = ""Printed""))
and toggle-1 = yes)
"
     _FldNameList[1]   = jobs.job
     _FldNameList[2]   = jobs.estimate
     _FldNameList[3]   > jobs.cadcam_status
"jobs.cadcam_status" "CADCAM" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > jobs.scheduling_status
"jobs.scheduling_status" "Scheduling" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   = jobs.customer
     _FldNameList[6]   > jobs.name
"jobs.name" ? "X(31)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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

&Scoped-define SELF-NAME auto-find
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL auto-find B-table-Win
ON return OF auto-find IN FRAME F-Main /* Search  Job */
DO:
   assign auto-find.
   def var lv-recid as recid no-undo.
 
 /*  &scoped-define key-phrase trim(jobs.job) begins auto-find
 */

   
   find first bf-jobs where trim(bf-jobs.job) begins auto-find no-lock no-error.
   lv-recid = if avail bf-jobs then recid(bf-jobs) else ?.
  
   if lv-recid <> ? then do:
       reposition {&browse-name} to recid lv-recid.
       apply "value-changed" to {&browse-name}.
       
    /*   auto-find = "".   */
       disp auto-find with frame {&frame-name}.
   end.
   else do:
        message "There is no job begins with " auto-find view-as alert-box error.
      /*  auto-find:screen-value = "". */
        return no-apply.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ANY-PRINTABLE OF br_table IN FRAME F-Main
DO:
    auto-find:screen-value in frame {&frame-name} = auto-find:screen-value + keylabel(lastkey).
    apply "return" to auto-find.

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


&Scoped-define SELF-NAME btn-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-clear B-table-Win
ON CHOOSE OF btn-clear IN FRAME F-Main /* Clear Search */
DO:
    assign auto-find = "".
    display auto-find with frame {&frame-name}.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-1 B-table-Win
ON VALUE-CHANGED OF TOGGLE-1 IN FRAME F-Main /* Completed Jobs */
DO:
  assign toggle-1.
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Check_Jobs B-table-Win 
PROCEDURE Check_Jobs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input  parameter ip-rowid as rowid no-undo.
  def output parameter op-valid as log no-undo.
  

  do with frame {&frame-name}:
    reposition {&browse-name} to rowid ip-rowid no-error.
  
    op-valid = not error-status:error.
    
    if op-valid then {&browse-name}:delete-current-row().
  end.  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Position_Jobs B-table-Win 
PROCEDURE Position_Jobs :
/*------------------------------------------------------------------------------
  Purpose:     position browser to most recently added record
  Parameters:  INPUT jobs ROWID
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER jobs-rowid AS ROWID NO-UNDO.
  
  {&OPEN-QUERY-{&BROWSE-NAME}}
    
  IF jobs-rowid NE ? THEN
  REPOSITION {&BROWSE-NAME} TO ROWID jobs-rowid.
  run dispatch ('row-changed').
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
  {src/adm/template/snd-list.i "jobs"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Get-Status B-table-Win 
FUNCTION Get-Status RETURNS CHARACTER
  (jobs_cadcam-status AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  Get Jobs CADCAM Status
    Notes:  
------------------------------------------------------------------------------*/
  CASE jobs_cadcam-status:
    WHEN 'S' THEN
    RETURN 'Submitted'.
    WHEN 'C' THEN
    RETURN 'Completed'.
    WHEN '' THEN
    RETURN 'N/A'.
    OTHERWISE
    RETURN 'ERROR'.
  END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

