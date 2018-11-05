&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: transactions.w

  Description: display of unposted machine transactions

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 11.14.2017

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

DEFINE VARIABLE start-time   AS CHARACTER NO-UNDO.
DEFINE VARIABLE end-time     AS CHARACTER NO-UNDO.
DEFINE VARIABLE total-time   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cColumnLabel AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAscending   AS LOGICAL   NO-UNDO INITIAL YES.

{custom/globdefs.i}
{custom/gcompany.i}

gcompany = "001".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME MachTrans

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES machtran

/* Definitions for BROWSE MachTrans                                     */
&Scoped-define FIELDS-IN-QUERY-MachTrans machtran.machine ~
machtran.job_number machtran.job_sub machtran.form_number ~
machtran.blank_number machtran.pass_sequence machtran.charge_code ~
machtran.start_date Time_String(machtran.start_time,YES) @ start-time ~
machtran.end_date Time_String(machtran.end_time,YES) @ end-time ~
machtran.shift machtran.run_qty ~
Time_String(machtran.total_time,NO) @ total-time 
&Scoped-define ENABLED-FIELDS-IN-QUERY-MachTrans 
&Scoped-define QUERY-STRING-MachTrans FOR EACH machtran ~
      WHERE machtran.company EQ gcompany ~
AND machtran.posted EQ NO NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-MachTrans OPEN QUERY MachTrans FOR EACH machtran ~
      WHERE machtran.company EQ gcompany ~
AND machtran.posted EQ NO NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-MachTrans machtran
&Scoped-define FIRST-TABLE-IN-QUERY-MachTrans machtran


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-MachTrans}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS MachTrans 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Time_String C-Win 
FUNCTION Time_String RETURNS CHARACTER
  (ip-time AS INTEGER,ip-clock-time AS LOGICAL) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY MachTrans FOR 
      machtran SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE MachTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS MachTrans C-Win _STRUCTURED
  QUERY MachTrans NO-LOCK DISPLAY
      machtran.machine FORMAT "x(6)":U WIDTH 8 LABEL-BGCOLOR 14
      machtran.job_number FORMAT "X(6)":U WIDTH 8 LABEL-BGCOLOR 14
      machtran.job_sub FORMAT ">9":U
      machtran.form_number FORMAT ">>9":U
      machtran.blank_number FORMAT ">9":U
      machtran.pass_sequence FORMAT ">>9":U
      machtran.charge_code FORMAT "X(5)":U LABEL-BGCOLOR 14
      machtran.start_date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      Time_String(machtran.start_time,YES) @ start-time COLUMN-LABEL "Log In" FORMAT "x(8)":U
            WIDTH 9
      machtran.end_date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      Time_String(machtran.end_time,YES) @ end-time COLUMN-LABEL "Log Out" FORMAT "x(8)":U
            WIDTH 9
      machtran.shift FORMAT "X":U LABEL-BGCOLOR 14
      machtran.run_qty COLUMN-LABEL "Run Qty" FORMAT "->>,>>>,>>9":U
            LABEL-BGCOLOR 14
      Time_String(machtran.total_time,NO) @ total-time COLUMN-LABEL "Total" FORMAT "x(5)":U
            WIDTH 6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 119 BY 14.76 ROW-HEIGHT-CHARS .62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     MachTrans AT ROW 1 COL 1 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119 BY 14.76 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Job Checklist Unposted Machine Transactions"
         COLUMN             = 3.2
         ROW                = 15.33
         HEIGHT             = 14.76
         WIDTH              = 119
         MAX-HEIGHT         = 14.76
         MAX-WIDTH          = 119
         VIRTUAL-HEIGHT     = 14.76
         VIRTUAL-WIDTH      = 119
         MAX-BUTTON         = no
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB MachTrans 1 DEFAULT-FRAME */
ASSIGN 
       MachTrans:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE MachTrans
/* Query rebuild information for BROWSE MachTrans
     _TblList          = "ASI.machtran"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _Where[1]         = "machtran.company EQ gcompany
AND machtran.posted EQ NO"
     _FldNameList[1]   > ASI.machtran.machine
"machine" ? ? "character" ? ? ? 14 ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.machtran.job_number
"job_number" ? ? "character" ? ? ? 14 ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = ASI.machtran.job_sub
     _FldNameList[4]   = ASI.machtran.form_number
     _FldNameList[5]   = ASI.machtran.blank_number
     _FldNameList[6]   = ASI.machtran.pass_sequence
     _FldNameList[7]   > ASI.machtran.charge_code
"charge_code" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.machtran.start_date
"start_date" ? ? "date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"Time_String(machtran.start_time,YES) @ start-time" "Log In" "x(8)" ? ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.machtran.end_date
"end_date" ? ? "date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"Time_String(machtran.end_time,YES) @ end-time" "Log Out" "x(8)" ? ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.machtran.shift
"shift" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.machtran.run_qty
"run_qty" "Run Qty" ? "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"Time_String(machtran.total_time,NO) @ total-time" "Total" "x(5)" ? ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE MachTrans */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Job Checklist Unposted Machine Transactions */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Job Checklist Unposted Machine Transactions */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME MachTrans
&Scoped-define SELF-NAME MachTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MachTrans C-Win
ON START-SEARCH OF MachTrans IN FRAME DEFAULT-FRAME
DO:
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? AND
       {&BROWSE-NAME}:CURRENT-COLUMN:LABEL-BGCOLOR EQ 14 THEN DO:
        cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
        lAscending = NOT lAscending.
        RUN pReopenBrowse.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE MachTrans 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByChargeCode C-Win 
PROCEDURE pByChargeCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY machtran.charge_code
    {&OPEN-QUERY-{&BROWSE-NAME}}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY machtran.charge_code DESCENDING
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByEndDate C-Win 
PROCEDURE pByEndDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY machtran.end_date BY machtran.end_time
    {&OPEN-QUERY-{&BROWSE-NAME}}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY machtran.end_date DESCENDING BY machtran.end_time DESCENDING
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByJobNumber C-Win 
PROCEDURE pByJobNumber :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY machtran.job_number
    {&OPEN-QUERY-{&BROWSE-NAME}}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY machtran.job_number DESCENDING
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByMacine C-Win 
PROCEDURE pByMacine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY machtran.machine
    {&OPEN-QUERY-{&BROWSE-NAME}}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY machtran.machine DESCENDING
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByRunQty C-Win 
PROCEDURE pByRunQty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY machtran.run_qty
    {&OPEN-QUERY-{&BROWSE-NAME}}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY machtran.run_qty DESCENDING
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByShift C-Win 
PROCEDURE pByShift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY machtran.shift
    {&OPEN-QUERY-{&BROWSE-NAME}}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY machtran.shift DESCENDING
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByStartDate C-Win 
PROCEDURE pByStartDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY machtran.start_date BY machtran.start_time
    {&OPEN-QUERY-{&BROWSE-NAME}}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY machtran.start_date DESCENDING BY machtran.start_time DESCENDING
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse C-Win 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CASE cColumnLabel:
        WHEN "machine" THEN
        RUN pByMacine.
        WHEN "job_number" THEN
        RUN pByJobNumber.
        WHEN "charge_code" THEN
        RUN pByChargeCode.
        WHEN "start_date" THEN
        RUN pByStartDate.
        WHEN "end_date" THEN
        RUN pByEndDate.
        WHEN "shift" THEN
        RUN pByShift.
        WHEN "run_qty" THEN
        RUN pByRunQty.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Time_String C-Win 
FUNCTION Time_String RETURNS CHARACTER
  (ip-time AS INTEGER,ip-clock-time AS LOGICAL):
/*------------------------------------------------------------------------------
  Purpose:  return time in string format
    Notes:  
------------------------------------------------------------------------------*/
  IF ip-time = 0 AND machtran.end_date EQ ? THEN
  RETURN ''.
  ELSE
  IF ip-clock-time THEN
  RETURN STRING(ip-time,'HH:MM am').
  ELSE
  RETURN STRING(ip-time,'HH:MM').

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

