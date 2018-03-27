&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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

DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE locode AS CHARACTER NO-UNDO.

DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmp-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
DEFINE STREAM excel.

ASSIGN
 cocode = gcompany
 locode = gloc.

DEFINE TEMP-TABLE tt-note NO-UNDO
  FIELD employee LIKE emplogin.employee
  FIELD rec_key LIKE nosweat.notes.rec_key
  FIELD note_date LIKE nosweat.notes.note_date
  FIELD note_title LIKE nosweat.notes.note_title
  FIELD note_src AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-6 begin_employee v-password ~
begin_end_date end_end_date rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_employee v-password begin_end_date ~
end_end_date rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
lv-font-name tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_employee AS CHARACTER FORMAT "X(5)" 
     LABEL "Beginning Employee" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE begin_end_date AS DATE FORMAT "99/99/9999" 
     LABEL "Beginning Login Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_end_date AS DATE FORMAT "99/99/9999" 
     LABEL "Ending Login Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-emptrn.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE v-password AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 5.48.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 8.57.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_employee AT ROW 2.43 COL 28.4 COLON-ALIGNED HELP
          "Enter Beginning Employee"
     v-password AT ROW 3.62 COL 28.4 COLON-ALIGNED BLANK 
     begin_end_date AT ROW 5.05 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Employee"
     end_end_date AT ROW 5.05 COL 69 COLON-ALIGNED HELP
          "Enter Ending Employee"
     rd-dest AT ROW 8.38 COL 5 NO-LABEL
     lv-ornt AT ROW 8.38 COL 28 NO-LABEL
     lines-per-page AT ROW 8.38 COL 81 COLON-ALIGNED
     lv-font-no AT ROW 10.52 COL 32 COLON-ALIGNED
     td-show-parm AT ROW 10.52 COL 50
     lv-font-name AT ROW 11.48 COL 26 COLON-ALIGNED NO-LABEL
     tb_excel AT ROW 12.91 COL 45.2
     tb_runExcel AT ROW 12.91 COL 88 RIGHT-ALIGNED
     fi_file AT ROW 14 COL 43 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 15.76 COL 17
     btn-cancel AT ROW 15.76 COL 55
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 7.19 COL 2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.48 COL 5
     RECT-2 AT ROW 1.24 COL 1
     RECT-6 AT ROW 6.95 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 93.4 BY 16.38.


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
         TITLE              = "Employee In/Out Transaction Report"
         HEIGHT             = 16.67
         WIDTH              = 94.4
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
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
   FRAME-NAME L-To-R                                                    */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_employee:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       begin_end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       end_end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Employee In/Out Transaction Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Employee In/Out Transaction Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  ASSIGN {&displayed-objects}.

  FIND employee WHERE employee.company = g_company
                  AND employee.employee = begin_employee
                NO-LOCK NO-ERROR.
  IF NOT AVAILABLE employee THEN DO:
     MESSAGE "Invalid Employee#. Try help." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO begin_employee.
     RETURN NO-APPLY.
  END.

  IF v-password NE employee.passwd THEN DO:
     MESSAGE "Invalid Password. Try again. " VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO v-password.
     RETURN NO-APPLY.
  END.

  RUN run-report.

  CASE rd-dest:
    WHEN 1 THEN RUN output-to-printer.
    WHEN 2 THEN RUN output-to-screen.
    WHEN 3 THEN RUN output-to-file.
    WHEN 4 THEN DO:
      /*run output-to-fax.*/
      {custom/asifax.i &type="Employee In/Out Transaction Report"
                       &begin_cust=begin_employee
                       &end_cust=begin_employee
                       &fax-subject=CURRENT-WINDOW:TITLE
                       &fax-body=CURRENT-WINDOW:TITLE
                       &fax-file=list-name}
    END. 
    WHEN 5 THEN DO:
      IF is-xprint-form THEN DO:
        {custom/asimail.i &type="Employee In/Out Transaction Report"
                          &begin_cust=begin_employee
                          &end_cust=begin_employee
                          &mail-subject=CURRENT-WINDOW:TITLE
                          &mail-body=CURRENT-WINDOW:TITLE
                          &mail-file=list-name}
      END.
      ELSE DO:
        {custom/asimailr.i &type="Employee In/Out Transaction Report"
                           &begin_cust=begin_employee
                           &end_cust=begin_employee
                           &mail-subject=CURRENT-WINDOW:TITLE
                           &mail-body=CURRENT-WINDOW:TITLE
                           &mail-file=list-name}
      END.
    END.
    WHEN 6 THEN RUN output-to-port.
  END CASE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
  DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

  RUN windows/l-fonts.w (FOCUS:SCREEN-VALUE,OUTPUT char-val).
  IF char-val NE "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                lv-font-name:SCREEN-VALUE = ENTRY(2,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON LEAVE OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
  ASSIGN lv-font-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON LEAVE OF lv-ornt IN FRAME FRAME-A
DO:
  ASSIGN lv-ornt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON VALUE-CHANGED OF lv-ornt IN FRAME FRAME-A
DO:
  {custom/chgfont.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
/*{sys/inc/f3helpw.i} */
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

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.
  RUN init-proc.
  RUN enable_UI.
 /* DO WITH FRAME {&frame-name}:
    {custom/usrprint.i}
  END.
  */
  {methods/nowait.i}
  APPLY 'ENTRY' TO begin_employee IN FRAME {&FRAME-NAME}.
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
  DISPLAY begin_employee v-password begin_end_date end_end_date rd-dest lv-ornt 
          lines-per-page lv-font-no td-show-parm lv-font-name tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-2 RECT-6 begin_employee v-password begin_end_date end_end_date 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc C-Win 
PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    begin_employee = ''
    begin_end_date = TODAY
    end_end_date = TODAY
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file C-Win 
PROCEDURE output-to-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {custom/out2file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-port C-Win 
PROCEDURE output-to-port :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN custom/d-print.w (list-name).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
  DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
  DEFINE VARIABLE result AS LOGICAL NO-UNDO.

  RUN custom/prntproc.p (list-name,INT(lv-font-no), lv-ornt).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-screen C-Win 
PROCEDURE output-to-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*==== Report main body procedure ================================*/

  DEFINE VARIABLE lv-hour AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-min AS INTEGER NO-UNDO.
  DEF VAR li-day-time AS INT NO-UNDO.
  DEF VAR li-start-time AS INT NO-UNDO.
  DEF VAR li-end-time AS INT NO-UNDO.

  SESSION:SET-WAIT-STATE('general').

  {sys/form/r-top3w.f}

  FORM HEADER SKIP(1) WITH FRAME r-top.

  ASSIGN
     str-tit2 = c-win:TITLE
     {sys/inc/ctrtext.i str-tit2 112}
     str-tit3 = FILL('-',132)
     {sys/inc/ctrtext.i str-tit3 132}.

  {sys/inc/print1.i}
  {sys/inc/outprint.i VALUE(lines-per-page)}

  IF td-show-parm THEN RUN show-param.

  VIEW FRAME r-top.
  IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(fi_file).
    ASSIGN excelheader = "Employee,Name,Start Date,End Date,Shift,Login Time,Logout Time,Worked,OT @ 1.5, OT @ 2".
    PUT STREAM excel UNFORMATTED excelheader SKIP.
  END.

  for each emplogin no-lock where
      emplogin.company EQ cocode AND
      emplogin.employee = begin_employee AND
      emplogin.start_date >= begin_end_date AND
      emplogin.start_date <= end_end_date
      BREAK by emplogin.employee by emplogin.start_date by start_time:
      if first-of(emplogin.employee) then do:
         find employee where employee.employee = emplogin.employee  no-lock no-error.
         if not avail employee then next.
         put "Employee: " employee.employee "  " employee.first_name employee.last_name skip
             "====================================================================" skip.           
         put "                            Login      Logout       Total                            " skip  
             "Start Date End   Date Shift Time       Time         Hours" skip
             "---------- ---------- ----- ---------- ---------- ---------  " skip.
      end.                      
      assign li-day-time = emplogin.total_time
             li-start-time = /*if li-start-time = 0 then emplogin.start_time else li-start-time*/
                             emplogin.START_time.
             li-end-time = emplogin.end_time.
      accumulate li-day-time (total by emplogin.employee by emplogin.start_date).
      ASSIGN
         lv-hour = truncate(li-day-time / 3600,0)
         lv-min = ((li-day-time MOD 3600) / 60).
      disp emplogin.start_date
           emplogin.end_date
           emplogin.shift form "x(5)"
           string(li-start-time,"HH:MM AM") /* column-label "Login!Time" */ form "x(10)" 
           string(li-end-time,"HH:MM AM")  /*column-label "Logout!Time" */ form "x(10)"
           string(lv-hour,">>>>9") + ":" + string(lv-min,"99") form "x(10)"
           with frame det stream-io  down no-label.
      IF tb_excel THEN PUT STREAM excel UNFORMATTED
          employee.employee ","
          employee.first_name " " employee.last_name ","
          (IF emplogin.start_date = ? THEN "" ELSE string(emplogin.start_date))  ","
          (IF emplogin.end_date = ? THEN "" ELSE string(emplogin.end_date))  ","
          emplogin.shift  ","
          string(li-start-time,"HH:MM AM")  ","
          string(li-end-time,"HH:MM AM")   ","
          string(lv-hour,">>>>9") + ":" + string(lv-min,"99")  SKIP.

      IF LAST-OF(emplogin.employee) THEN DO:
         lv-hour = truncate((ACCUM TOTAL BY emplogin.employee li-day-time) / 3600,0).
         lv-min = (((ACCUM TOTAL BY emplogin.employee li-day-time) MOD 3600) / 60).
         PUT "                                                  ---------  " skip
             "Total" string(lv-hour,">>>>9") + ":" + STRING(lv-min,"99") AT 51 
             .
         IF tb_excel THEN PUT STREAM excel UNFORMATTED "Total,,,,,,," 
            string(lv-hour,">>>>9") + ":" + STRING(lv-min,"99") SKIP.
      END.
  END.

  OUTPUT CLOSE.
  IF tb_excel THEN DO:
    OUTPUT STREAM excel CLOSE.

    IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
  END.
  SESSION:SET-WAIT-STATE('').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-frame-hdl as handle no-undo.
  def var lv-group-hdl as handle no-undo.
  def var lv-field-hdl as handle no-undo.
  def var lv-field2-hdl as handle no-undo.
  def var parm-fld-list as cha no-undo.
  def var parm-lbl-list as cha no-undo.
  def var i as int no-undo.
  def var lv-label as cha.

  lv-frame-hdl = frame {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:first-child.
  lv-field-hdl = lv-group-hdl:first-child .

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," 
                     .
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     .
              lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name then do:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".
                  end.
                  lv-field2-hdl = lv-field2-hdl:next-sibling.                 
              end.       
           end.                 
        end.            
     lv-field-hdl = lv-field-hdl:next-sibling.   
  end.

  put space(28)
      "< Selection Parameters >"
      skip(1).

  do i = 1 to num-entries(parm-fld-list,","):
    if entry(i,parm-fld-list) ne "" or
       entry(i,parm-lbl-list) ne "" then do:

      lv-label = fill(" ",34 - length(trim(entry(i,parm-lbl-list)))) +
                 trim(entry(i,parm-lbl-list)) + ":".

      put lv-label format "x(35)" at 5
          space(1)
          trim(entry(i,parm-fld-list)) format "x(40)"
          skip.              
    end.
  end.

  put fill("-",80) format "x(80)" skip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

