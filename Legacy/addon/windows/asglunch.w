&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: addon\windows\asglunch.w

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
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}


def stream st-mach.
def stream st-emp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rfq-ctrl

/* Definitions for FRAME FRAME-A                                        */
&Scoped-define QUERY-STRING-FRAME-A FOR EACH rfq-ctrl SHARE-LOCK
&Scoped-define OPEN-QUERY-FRAME-A OPEN QUERY FRAME-A FOR EACH rfq-ctrl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FRAME-A rfq-ctrl
&Scoped-define FIRST-TABLE-IN-QUERY-FRAME-A rfq-ctrl


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS end_emp begin_emp begin_shift end_shift ~
begin_date end_date btn_ok btn_cancel RECT-27 
&Scoped-Define DISPLAYED-OBJECTS end_emp begin_emp begin_shift end_shift ~
begin_date end_date 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 end_emp begin_emp begin_shift end_shift 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 20 BY 1.14.

DEFINE BUTTON btn_ok 
     LABEL "Create &Lunch Time" 
     SIZE 23 BY 1.14.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_emp AS CHARACTER FORMAT "X(5)" 
     LABEL "From Employee ID" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE begin_shift AS CHARACTER FORMAT "X(8)" 
     LABEL "From Shift" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_emp AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
     LABEL "To Employee ID" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE end_shift AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Shift" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91 BY 5.24.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY FRAME-A FOR 
      rfq-ctrl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     end_emp AT ROW 1.95 COL 56 COLON-ALIGNED
     begin_emp AT ROW 2.19 COL 24 COLON-ALIGNED
     begin_shift AT ROW 3.62 COL 23 COLON-ALIGNED
     end_shift AT ROW 3.62 COL 55 COLON-ALIGNED
     begin_date AT ROW 5.05 COL 19 COLON-ALIGNED
     end_date AT ROW 5.05 COL 51 COLON-ALIGNED
     btn_ok AT ROW 7.19 COL 22
     btn_cancel AT ROW 7.19 COL 52
     RECT-27 AT ROW 1.24 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 1.24 COL 7
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.4 ROW 1
         SIZE 94.8 BY 9.19
         DEFAULT-BUTTON btn_cancel.


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
         TITLE              = "Assign Lunch Time"
         HEIGHT             = 8.05
         WIDTH              = 95
         MAX-HEIGHT         = 24.91
         MAX-WIDTH          = 100.2
         VIRTUAL-HEIGHT     = 24.91
         VIRTUAL-WIDTH      = 100.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* SETTINGS FOR FILL-IN begin_emp IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN begin_shift IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN end_emp IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN end_shift IN FRAME FRAME-A
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _TblList          = "rfq.rfq-ctrl"
     _Query            is OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Assign Lunch Time */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Assign Lunch Time */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON HELP OF FRAME FRAME-A
DO:
   def var char-val as cha no-undo.
   
   if focus:name = "begin_emp" or focus:name = "end_emp" then do:
      run windows/l-emp.w (input gcompany, focus:screen-value, output char-val).
      if char-val <> "" then focus:screen-value = char-val.   
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* From Date */
DO:
    assign {&self-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_emp C-Win
ON LEAVE OF begin_emp IN FRAME FRAME-A /* From Employee ID */
DO:
      assign {&self-name}.
      find employee where employee.employee = {&self-name} no-lock no-error.
      if not avail employee then do:
         message "Invalid Employee ID. Please enter correct employee id."
                   view-as alert-box error.
         return no-apply.
      end.
      assign end_emp:screen-value = {&self-name}
             end_emp = {&self-name}
              .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_shift C-Win
ON LEAVE OF begin_shift IN FRAME FRAME-A /* From Shift */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_cancel C-Win
ON CHOOSE OF btn_cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok C-Win
ON CHOOSE OF btn_ok IN FRAME FRAME-A /* Create Lunch Time */
DO:
  if begin_emp > end_emp then do:
     message "To Employee ID can not be less than From Employee ID. "
              view-as alert-box error.
     return no-apply.
  end.
  if begin_date > end_date then do:
     message "To Date not be less than From Date. "
              view-as alert-box error.
     return no-apply.
  end.
  if begin_shift > end_shift then do:
     message "To Shift not be less than From Shift. "
              view-as alert-box error.
     return no-apply.
  end.

  run assign-lunch-hour.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* To Date */
DO:
    assign {&self-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_emp C-Win
ON LEAVE OF end_emp IN FRAME FRAME-A /* To Employee ID */
DO:
      assign {&self-name}.
      find employee where employee.employee = {&self-name} no-lock no-error.
      if not avail employee then do:
         message "Invalid Employee ID. Please enter correct employee id."
                   view-as alert-box error.
         return no-apply.
      end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shift C-Win
ON LEAVE OF end_shift IN FRAME FRAME-A /* To Shift */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
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
 
   find first employee no-lock no-error.
   begin_emp = if avail employee then employee.employee else "".
   find last employee no-lock no-error.
   end_emp = if avail employee then employee.employee else "".
   
   
   RUN enable_UI.
  
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-lunch-hour C-Win 
PROCEDURE assign-lunch-hour :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define buffer bf-machtran for machtran.
  def var machtran-rowid as rowid no-undo.
  def var run-qty like machtran.run_qty no-undo.
  def var waste-qty like machtran.waste_qty no-undo.
  def var lv-date as date no-undo.
   
  message "Lunch Hour Transactions will be created " skip
           "for Employee " begin_emp
          " - " end_emp " and for shifte " begin_shift " - " end_shift
          " for Date " begin_date " - " end_date "."
          skip
          "Are you sure?" 
          view-as alert-box question button yes-no update ll-ans as log.
   if ll-ans then do:  
      for each machshft no-lock where machshft.company = gcompany 
                                  and machshft.shift >= begin_shift 
                                  and machshft.shift <= end_shift,
          each empmach no-lock where empmach.company = machshft.company
                                 and empmach.machine = machshft.machine                        
                                 and empmach.employee >= begin_emp
                                 and empmach.employee <= end_emp
                         break by machshft.machine by empmach.employee
                                 :                                 
       if first-of(machshft.machine) then do:                          
          do lv-date = begin_date to end_date:                       
             find first machtran where machtran.company = machshft.company
                                and machtran.machine = machshft.machine
                                and machtran.start_date = lv-date
                                and machtran.charge_code = "LUNCH"
                                no-lock no-error.                                
             if not avail machtran then do:
                create machtran.
                ASSIGN machtran.company = empmach.company
                       machtran.machine = empmach.machine
                       machtran.job_number = "" 
                       machtran.job_sub = 0
                       machtran.form_number = 0
                       machtran.blank_number = 0
                       machtran.pass_sequence = 0
                       machtran.start_date = lv-date
                       machtran.start_time = shifts.lunch_start
                       machtran.jobseq = 0
                       machtran.charge_code = "Lunch"
                       machtran.end_date = lv-date
                       machtran.end_time = shifts.lunch_end
                       machtran.shift = machshft.shift
                       machtran.run_qty = 0
                       machtran.waste_qty = 0
                       machtran-rowid = ROWID(machtran)
                       .
                {custom/calctime.i &file="machtran"}                          

             end. /* not avail machtran */
             if first-of(empmach.employee) then do:
                find first machemp where machemp.table_rec_key = machtran.rec_key
                                  and machemp.employee = empmach.employee
                                  and machemp.start_date = lv-date
                                  no-lock no-error.
                if not avail machemp then do:
                   create machemp.                       
                   ASSIGN machemp.table_rec_key = machtran.rec_key
                       machemp.employee = empmach.employee
                       machemp.start_date = machtran.start_date
                       machemp.start_time = machtran.start_time
                       machemp.shift = machtran.shift
                       machemp.ratetype = if shifts.lunch_paid then 'Standard' else ""
                       machemp.rate_usage = employee.rate_usage
                       machemp.end_date = machtran.end_date
                       machemp.end_time = machtran.end_time
                            .
                   RUN Employee-Rate(machtran.company,machemp.employee,machemp.shift,machtran.machine,
                          machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
                   {custom/calctime.i &file="machemp"}

                   RELEASE machemp.
                end.  /* not avail machemp */
             end. /* first-of(empmach.employee) */
          end.  /* do lv-date */
       end.     /* first-of(machine) */   
      end.  /* machshft */
   
   /* ==================
    /* ======= all employee for the machine ===========*/
      for each empmach no-lock where empmach.company = machtran.company
                                 and empmach.employee >= begin_emp
                                 and empmach.employee <= end_emp
                               /*  and empmach.machine = begin_machine
                                 and empmach.machine = end_machine*/ ,
          first emplogin where emplogin.company = empmach.company 
                           and emplogin.employee = empmach.employee
                           and emplogin.start_date >= begin_date
                           and emplogin.start_date <= end_date
                               no-lock 
                               break by empmach.employee by empmach.machine:       
  
          if first-of(empmach.employee) then do:   
             find shifts where shifts.company = emplogin.company and
                               shifts.shift = emplogin.shift
                               no-lock no-error.

             CREATE machtran.
             ASSIGN machtran.company = empmach.company
           machtran.machine = empmach.machine
           machtran.job_number = "" 
           machtran.job_sub = 0
           machtran.form_number = 0
           machtran.blank_number = 0
           machtran.pass_sequence = 0
           machtran.start_date = lunch-date
           machtran.start_time = shifts.lunch_start /*time-hour * 3600 + time-minute * 60 + ampm*/
           machtran.jobseq = 0
           machtran.charge_code = "Lunch"
           machtran.end_date = lunch-date
           machtran.end_time = shifts.lunch_end
           machtran.shift = emplogin.shift
           machtran-rowid = ROWID(machtran)
           .
        {custom/calctime.i &file="machtran"}                          

/*      RUN Get-Shift(company_code,machine_code,machtran.start_time,job_sequence,
             OUTPUT machtran.shift). 
      {methods/run_link.i "CONTAINER" "Set_MachTran_Rowid" "(bf-machtran-rowid)"}
*/    
       
            find employee of empmach no-lock .
            CREATE machemp.
            ASSIGN machemp.table_rec_key = machtran.rec_key
                machemp.employee = empmach.employee
                machemp.start_date = machtran.start_date
                machemp.start_time = machtran.start_time
                machemp.shift = machtran.shift
                machemp.ratetype = if shifts.lunch_paid then 'Standard' else ""
                machemp.rate_usage = employee.rate_usage
                machemp.end_date = machtran.end_date
                machemp.end_time = machtran.end_time
                machtran.run_qty = 0
                machtran.waste_qty = 0
                .
             RUN Employee-Rate(machtran.company,machemp.employee,machemp.shift,machtran.machine,
                          machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
             {custom/calctime.i &file="machemp"}
                          

   /* =============== 
     /* get active employees logged into this machine */
      FOR EACH emplogin NO-LOCK
          WHERE emplogin.company = machtran.company
            AND emplogin.machine = machtran.machine
            AND emplogin.end_time = 0
            AND emplogin.total_time = 0,
          FIRST employee OF emplogin NO-LOCK:
         CREATE machemp.
         ASSIGN machemp.table_rec_key = bf-machtran.rec_key
                machemp.employee = emplogin.employee
                machemp.start_date = bf-machtran.start_date
                machemp.start_time = bf-machtran.start_time
                machemp.shift = bf-machtran.shift
                machemp.ratetype = if shifts.lunch_paid then 'Standard' else ""
                machemp.rate_usage = employee.rate_usage
                machemp.end_date = bf-machtran.end_date
                machemp.end_time = bf-machtran.end_time
                bf-machtran.run_qty = run-qty  /*???*/
                bf-machtran.waste_qty = waste-qty  /*???*/
                .
          RUN Employee-Rate(machtran.company,machemp.employee,machemp.shift,machtran.machine,
                          machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
          {custom/calctime.i &file="machemp"}                          
      END. /* each emplogin */
    ================================================*/


      end.  /* first-of*/  
      END. /* each empmach */
=======================================================*/

message "create lunch trans " shifts.shift  machtran.machine.


   end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

  {&OPEN-QUERY-FRAME-A}
  GET FIRST FRAME-A.
  DISPLAY end_emp begin_emp begin_shift end_shift begin_date end_date 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE end_emp begin_emp begin_shift end_shift begin_date end_date btn_ok 
         btn_cancel RECT-27 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

