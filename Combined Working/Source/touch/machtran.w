&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: touch/machtran.w

  Description: Machine Transactions

  Input Parameters: Window Handle of Calling Procedure.

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 8.31.2000

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

&IF DEFINED(UIB_is_Running) = 0 &THEN
DEFINE INPUT PARAMETER h_calling_window AS WIDGET-HANDLE NO-UNDO.
&ELSE
DEFINE VARIABLE h_calling_window AS WIDGET-HANDLE NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE start-time AS CHARACTER NO-UNDO.
DEFINE VARIABLE end-time AS CHARACTER NO-UNDO.
DEFINE VARIABLE total-time AS CHARACTER NO-UNDO.

DEFINE VARIABLE company_code AS CHARACTER NO-UNDO.
DEFINE VARIABLE machine_code AS CHARACTER NO-UNDO.
DEFINE VARIABLE job_number AS CHARACTER NO-UNDO.
DEFINE VARIABLE job_sub AS CHARACTER NO-UNDO.
DEFINE VARIABLE form_number AS CHARACTER NO-UNDO.
DEFINE VARIABLE blank_number AS CHARACTER NO-UNDO.
DEFINE VARIABLE pass_sequence AS CHARACTER NO-UNDO.

RUN Get_Value IN h_calling_window ('company_code',OUTPUT company_code).
RUN Get_Value IN h_calling_window ('machine_code',OUTPUT machine_code).
RUN Get_Value IN h_calling_window ('job_number',OUTPUT job_number).
RUN Get_Value IN h_calling_window ('job_sub',OUTPUT job_sub).
RUN Get_Value IN h_calling_window ('form_number',OUTPUT form_number).
RUN Get_Value IN h_calling_window ('blank_number',OUTPUT blank_number).
RUN Get_Value IN h_calling_window ('pass_sequence',OUTPUT pass_sequence).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES machtran

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 machtran.charge_code ~
machtran.start_date STRING(machtran.start_time,'HH:MM am') @ start-time ~
machtran.end_date machtran.shift ~
Time_String(machtran.end_time,yes) @ end-time machtran.run_qty ~
machtran.waste_qty Time_String(machtran.total_time,no) @ total-time 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define FIELD-PAIRS-IN-QUERY-BROWSE-1
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH machtran ~
      WHERE machtran.company = company_code AND ~
machtran.machine = machine_code AND ~
machtran.job_number = job_number AND ~
machtran.job_sub = INTEGER(job_sub) AND ~
machtran.form_number = INTEGER(form_number) AND ~
machtran.blank_number = INTEGER(blank_number) AND ~
machtran.pass_sequence = INTEGER(pass_sequence) NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 machtran
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 machtran


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 

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
DEFINE QUERY BROWSE-1 FOR 
      machtran
    FIELDS(machtran.charge_code
      machtran.start_date
      machtran.start_time
      machtran.end_date
      machtran.shift
      machtran.end_time
      machtran.run_qty
      machtran.waste_qty
      machtran.total_time) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      machtran.charge_code
      machtran.start_date
      STRING(machtran.start_time,'HH:MM am') @ start-time COLUMN-LABEL "Log In" FORMAT "X(8)"
      machtran.end_date
      machtran.shift
      Time_String(machtran.end_time,yes) @ end-time COLUMN-LABEL "Log Out" FORMAT "X(8)"
      machtran.run_qty COLUMN-LABEL "Run Qty"
      machtran.waste_qty
      Time_String(machtran.total_time,no) @ total-time COLUMN-LABEL "Total" FORMAT "X(5)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 109 BY 11.43
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 109 BY 11.57.


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
         TITLE              = "Machine Transactions"
         COLUMN             = 7.6
         ROW                = 15
         HEIGHT             = 11.48
         WIDTH              = 109.2
         MAX-HEIGHT         = 14.38
         MAX-WIDTH          = 109.8
         VIRTUAL-HEIGHT     = 14.38
         VIRTUAL-WIDTH      = 109.8
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

IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-1 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "machtran"
     _Options          = "NO-LOCK"
     _TblOptList       = "USED"
     _Where[1]         = "machtran.company = company_code AND
machtran.machine = machine_code AND
machtran.job_number = job_number AND
machtran.job_sub = INTEGER(job_sub) AND
machtran.form_number = INTEGER(form_number) AND
machtran.blank_number = INTEGER(blank_number) AND
machtran.pass_sequence = INTEGER(pass_sequence)"
     _FldNameList[1]   = machtran.charge_code
     _FldNameList[2]   = machtran.start_date
     _FldNameList[3]   > "_<CALC>"
"STRING(machtran.start_time,'HH:MM am') @ start-time" "Log In" "X(8)" ? ? ? ? ? ? ? no ?
     _FldNameList[4]   = machtran.end_date
     _FldNameList[5]   = machtran.shift
     _FldNameList[6]   > "_<CALC>"
"Time_String(machtran.end_time,yes) @ end-time" "Log Out" "X(8)" ? ? ? ? ? ? ? no ?
     _FldNameList[7]   > machtran.run_qty
"run_qty" "Run Qty" ? "decimal" ? ? ? ? ? ? no ?
     _FldNameList[8]   = machtran.waste_qty
     _FldNameList[9]   > "_<CALC>"
"Time_String(machtran.total_time,no) @ total-time" "Total" "X(5)" ? ? ? ? ? ? ? no ?
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Machine Transactions */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Machine Transactions */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  ENABLE BROWSE-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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
  IF ip-time = 0 THEN
  RETURN ''.
  ELSE
  IF ip-clock-time THEN
  RETURN STRING(ip-time,'HH:MM am').
  ELSE
  RETURN STRING(ip-time,'HH:MM').

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


