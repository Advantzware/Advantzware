&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: addon/touch/r-emplog.w

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
  FIELD note_src AS CHARACTER
  INDEX noteindex employee note_date.

DEFINE TEMP-TABLE tt-emp NO-UNDO
    FIELD emp AS CHAR.


DEF TEMP-TABLE tt-emp-time NO-UNDO
    FIELD t-empid  LIKE employee.employee 
    FIELD t-fname  LIKE employee.first_name 
    FIELD t-lname  LIKE employee.last_name 
    FIELD t-lgStDt LIKE emplogin.start_date
    FIELD t-mach   LIKE emplogin.machine
    FIELD t-shift  LIKE emplogin.shift
    FIELD t-lgInTm   AS INT 
    FIELD t-lgOutTm  AS INT 
    FIELD t-HrWrk    AS INT 
    FIELD t-OT       AS INT
    FIELD t-OT2      AS INT
    FIELD t-MlgInTm  AS INT
    FIELD t-MlgOutTm AS INT
    FIELD t-MHrWrk   AS INT
    FIELD t-MOT      AS INT
    FIELD t-MOT2     AS INT
    .

DEF VAR lv-shift-list AS CHAR NO-UNDO.
DEF VAR lv-shifts AS CHAR NO-UNDO.
DEF VAR ll-shifts AS LOG NO-UNDO.

/* RUN pcrep/defshift.p (cocode, OUTPUT lv-shift-list). */

ASSIGN ll-shifts = lv-shift-list NE "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-6 RECT-3 begin_employee ~
end_employee begin_end_date end_end_date select-shift scr-plant-clock ~
lv-ornt lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS selected-company begin_employee ~
end_employee begin_end_date end_end_date lbl_select-shift select-shift ~
scr-plant-clock lv-ornt lines-per-page rd-dest lv-font-no td-show-parm ~
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
     LABEL "Beginning Labor Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_employee AS CHARACTER FORMAT "X(5)" 
     LABEL "Ending Employee" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE end_end_date AS DATE FORMAT "99/99/9999" 
     LABEL "Ending Labor Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-Unrptlabor.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_select-shift AS CHARACTER FORMAT "X(256)":U INITIAL "Select/Deselect Shifts" 
     VIEW-AS FILL-IN 
     SIZE 27 BY .95
     FONT 6 NO-UNDO.

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

DEFINE VARIABLE selected-company AS CHARACTER FORMAT "X(3)":U 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE shifts AS CHARACTER FORMAT "X(256)":U 
     LABEL "Shifts" 
     VIEW-AS FILL-IN 
     SIZE 1 BY 1 NO-UNDO.

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

DEFINE VARIABLE scr-plant-clock AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Plant Machines", 1,
"Clock", 2
     SIZE 29 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 11.91.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 6.19.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.05.

DEFINE VARIABLE select-shift AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 28 BY 4.52 NO-UNDO.

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
     selected-company AT ROW 2.19 COL 24 COLON-ALIGNED
     begin_employee AT ROW 3.62 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Employee"
     end_employee AT ROW 3.62 COL 63 COLON-ALIGNED HELP
          "Enter Ending Employee"
     begin_end_date AT ROW 5.05 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Employee"
     end_end_date AT ROW 5.05 COL 63 COLON-ALIGNED HELP
          "Enter Ending Employee"
     lbl_select-shift AT ROW 6.95 COL 63.6 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     select-shift AT ROW 8.1 COL 65.4 HELP
          "Enter description of this Material Type." NO-LABEL WIDGET-ID 18
     scr-plant-clock AT ROW 9.14 COL 15 NO-LABEL WIDGET-ID 4
     shifts AT ROW 11.24 COL 60 COLON-ALIGNED WIDGET-ID 20
     lv-ornt AT ROW 14 COL 29.4 NO-LABEL
     lines-per-page AT ROW 14 COL 82.4 COLON-ALIGNED
     rd-dest AT ROW 14.24 COL 6.4 NO-LABEL
     lv-font-no AT ROW 16.14 COL 33.4 COLON-ALIGNED
     td-show-parm AT ROW 16.14 COL 51.4
     lv-font-name AT ROW 17.33 COL 27.4 COLON-ALIGNED NO-LABEL
     tb_excel AT ROW 19.1 COL 47.4
     tb_runExcel AT ROW 19.1 COL 90.2 RIGHT-ALIGNED
     fi_file AT ROW 20.62 COL 45.2 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 22.14 COL 18
     btn-cancel AT ROW 22.14 COL 56
     "Time Calculation Basis:" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 7.95 COL 7 WIDGET-ID 8
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.29 COL 3.8
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.95 COL 3.4
     RECT-2 AT ROW 1 COL 1
     RECT-6 AT ROW 12.91 COL 1
     RECT-3 AT ROW 6.71 COL 1 WIDGET-ID 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 94.4 BY 22.71.


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
         TITLE              = "Unreported Labor"
         HEIGHT             = 22.95
         WIDTH              = 95.6
         MAX-HEIGHT         = 46.48
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 46.48
         VIRTUAL-WIDTH      = 256
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
       end_employee:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       end_end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_select-shift IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN selected-company IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       selected-company:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR FILL-IN shifts IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       shifts:HIDDEN IN FRAME FRAME-A           = TRUE
       shifts:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Unreported Labor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Unreported Labor */
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
   DEF VAR I AS INT NO-UNDO.

   ASSIGN {&displayed-objects}.

   ASSIGN shifts = "" lv-shifts = "".

   DO WITH FRAME {&FRAME-NAME}:
       DO i = 1 TO select-shift:NUM-ITEMS:
        IF select-shift:IS-SELECTED(i)
          THEN
            lv-shifts = lv-shifts + 
                        TRIM(SUBSTR(select-shift:ENTRY(i),1,5)) + ",".
       END.

       IF SUBSTR(lv-shifts,LENGTH(TRIM(lv-shifts)),1) EQ "," 
         THEN SUBSTR(lv-shifts,LENGTH(TRIM(lv-shifts)),1) = "".

       shifts = lv-shifts.

       DO i = 1 TO LENGTH(shifts):
          IF SUBSTR(shifts,i,1) EQ "," THEN SUBSTR(shifts,i,1) = " ".
       END.

       DISPLAY shifts.    

    shifts:HIDDEN = YES.

  END.  

  RUN run-report.

  CASE rd-dest:
    WHEN 1 THEN RUN output-to-printer.
    WHEN 2 THEN RUN output-to-screen.
    WHEN 3 THEN RUN output-to-file.
    WHEN 4 THEN DO:
      /*run output-to-fax.*/
      {custom/asifax.i &type="Labor Report"
                       &begin_cust=begin_employee
                       &end_cust=end_employee
                       &fax-subject=CURRENT-WINDOW:TITLE
                       &fax-body=CURRENT-WINDOW:TITLE
                       &fax-file=list-name}
    END. 
    WHEN 5 THEN DO:
      IF is-xprint-form THEN DO:
        {custom/asimail.i &type="Labor Report"
                          &begin_cust=begin_employee
                          &end_cust=end_employee
                          &mail-subject=CURRENT-WINDOW:TITLE
                          &mail-body=CURRENT-WINDOW:TITLE
                          &mail-file=list-name}
      END.
      ELSE DO:
        {custom/asimailr.i &type="Labor Report"
                           &begin_cust=begin_employee
                           &end_cust=end_employee
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


&Scoped-define SELF-NAME select-shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL select-shift C-Win
ON VALUE-CHANGED OF select-shift IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shifts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shifts C-Win
ON LEAVE OF shifts IN FRAME FRAME-A /* Shifts */
DO:
  assign {&self-name}.
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
{sys/inc/f3helpw.i} 
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

  FOR EACH shifts NO-LOCK WHERE shifts.company EQ COCODE:
      lv-shift-list = lv-shift-list + 
                      STRING(shifts.shift,"x(5)") + " " +
                      shifts.description + ",".
  END.
  IF SUBSTR(lv-shift-list,LENGTH(TRIM(lv-shift-list)),1) EQ "," THEN
     SUBSTR(lv-shift-list,LENGTH(TRIM(lv-shift-list)),1) = "".


 ASSIGN select-shift:LIST-ITEMS IN FRAME {&FRAME-NAME} = lv-shift-list.

  RUN enable_UI.


  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}          
  END. 

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
  DISPLAY selected-company begin_employee end_employee begin_end_date 
          end_end_date lbl_select-shift select-shift scr-plant-clock lv-ornt 
          lines-per-page rd-dest lv-font-no td-show-parm lv-font-name tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-2 RECT-6 RECT-3 begin_employee end_employee begin_end_date 
         end_end_date select-shift scr-plant-clock lv-ornt lines-per-page 
         rd-dest lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok 
         btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-clock-time C-Win 
PROCEDURE get-clock-time :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-emp AS CHAR NO-UNDO.

DEF VAR li-overtime      AS INT LABEL "OT @x1.5" NO-UNDO.
DEF VAR li-2time         AS INT LABEL "OT @x2"   NO-UNDO.
DEF VAR li-day-time      AS INT                  NO-UNDO. 
DEF VAR li-lunch-time    AS INT                  NO-UNDO. 
DEF VAR li-lunch-emp     AS INT                  NO-UNDO. 
DEF VAR li-start-time    AS INT                  NO-UNDO. 
DEF VAR li-end-time      AS INT                  NO-UNDO. 
DEF VAR li-emp-over      AS INT                  NO-UNDO. 
DEF VAR li-emp-2time     AS INT                  NO-UNDO. 
DEF VAR li-emp-tot       AS INT                  NO-UNDO. 
DEF VAR li-emp-tot-hr    AS INT                  NO-UNDO. 
DEF VAR li-emp-tot-min   AS INT                  NO-UNDO. 
DEF VAR li-over-tot-hr   AS INT                  NO-UNDO. 
DEF VAR li-over-tot-min  AS INT                  NO-UNDO. 
DEF VAR li-2time-tot-hr  AS INT                  NO-UNDO. 
DEF VAR li-2time-tot-min AS INT                  NO-UNDO. 
DEF VAR ll-first-emp     AS LOG                  NO-UNDO. 

FOR EACH emplogin NO-LOCK 
    WHERE emplogin.company    EQ cocode 
      AND emplogin.employee   EQ ip-emp 
      AND emplogin.start_date GE begin_end_date 
      AND emplogin.START_date LE end_end_date 
      AND emplogin.machine    EQ "CLOCK"
      AND CAN-DO(lv-shifts,emplogin.shift)
     BREAK BY emplogin.employee BY emplogin.start_date BY start_time:


    IF FIRST-OF(emplogin.employee) 
      THEN  ASSIGN ll-first-emp = TRUE.

    ASSIGN 
        li-day-time = emplogin.total_time
        li-overtime = 0 li-2time = 0 
        li-start-time = IF li-start-time = 0 
                          THEN emplogin.start_time
                          ELSE li-start-time
        li-end-time = emplogin.end_time.

    ACCUMULATE li-day-time (TOTAL BY emplogin.employee BY emplogin.start_date).

    /* ===== daily total display */
    IF LAST-OF(emplogin.start_date) THEN DO:

        FIND FIRST employee NO-LOCK 
            WHERE employee.company EQ emplogin.company 
              AND employee.employee EQ emplogin.employee NO-ERROR.

        FIND shifts WHERE shifts.company = emplogin.company 
                      AND shifts.shift = emplogin.shift NO-LOCK NO-ERROR.
        IF AVAIL shifts AND NOT shifts.lunch_paid THEN DO:
            IF emplogin.end_time <= shifts.lunch_start 
              THEN li-lunch-time = 0.  
              ELSE 
               IF emplogin.start_time >= shifts.lunch_end 
                 THEN li-lunch-time = 0.
                 ELSE 
                  IF emplogin.start_time <= shifts.lunch_start AND
                     emplogin.end_time >= shifts.lunch_start   AND
                     emplogin.end_time <= shifts.lunch_end 
                    THEN li-lunch-time = emplogin.end_time - shifts.lunch_start.
                    ELSE 
                     IF emplogin.start_time >= shifts.lunch_start AND
                        emplogin.start_time <= shifts.lunch_end   AND
                        emplogin.end_time >= shifts.lunch_end 
                       THEN li-lunch-time = shifts.lunch_end - emplogin.start_time .
                       ELSE 
                        IF emplogin.start_time >= shifts.lunch_start AND
                           emplogin.end_time <= shifts.lunch_end 
                          THEN li-lunch-time = emplogin.end_time - emplogin.start_time.
                          ELSE li-lunch-time = shifts.lunch_end - shifts.lunch_start. 
        END.
        ELSE li-lunch-time = 0.

        IF employee.lunch_paid THEN ASSIGN li-lunch-time = 0. 

        ASSIGN li-lunch-emp = li-lunch-emp + li-lunch-time.

        /*12 Hr  (x 3600) */
        IF (ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time > 43200   
          THEN  
           ASSIGN li-2time = (ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time - 43200 li-overtime = 14400.

          ELSE 
            IF ((ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time) > 28800  AND 
               ((ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time)  <= 43200
              THEN 
               ASSIGN 
                 li-2time = 0
                 li-overtime = (ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time - 28800. /* 8 Hr */

        ASSIGN 
          li-emp-over = li-emp-over + li-overtime 
          li-emp-2time = li-emp-2time + li-2time .

        FIND FIRST tt-emp-time 
                WHERE tt-emp-time.t-empid  EQ emplogin.employee 
                  AND tt-emp-time.t-lgStDt EQ emplogin.start_date NO-ERROR.
            IF NOT AVAIL tt-emp-time THEN DO:
                CREATE tt-emp-time.                           
                ASSIGN                                        
                    tt-emp-time.t-empid  = emplogin.employee   
                    tt-emp-time.t-lgStDt = emplogin.start_date
                    tt-emp-time.t-fname  = employee.first_name 
                    tt-emp-time.t-lname  = employee.last_name
                    tt-emp-time.t-shift  = emplogin.shift. 
            END.


            ASSIGN
              tt-emp-time.t-mach    = emplogin.machine
              tt-emp-time.t-lgInTm  = li-start-time
              tt-emp-time.t-lgOutTm = li-end-time
              tt-emp-time.t-HrWrk   = ((ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time)
              tt-emp-time.t-OT      = li-overtime 
              tt-emp-time.t-OT2     = li-2time.


            ASSIGN ll-first-emp = FALSE.

        ASSIGN li-start-time = 0.  
    END. /* IF LAST-OF(emplogin.start_date)  */ 

    ASSIGN 
        li-lunch-emp = 0 li-emp-over = 0 li-emp-2time = 0.

END. /* FOR EACH LOGIN */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-mach-time C-Win 
PROCEDURE get-mach-time :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-emp AS CHAR NO-UNDO.

DEF VAR li-overtime      AS INT LABEL "OT @x1.5" NO-UNDO.
DEF VAR li-2time         AS INT LABEL "OT @x2"   NO-UNDO.
DEF VAR li-day-time      AS INT                  NO-UNDO.
DEF VAR li-lunch-time    AS INT                  NO-UNDO.
DEF VAR li-lunch-emp     AS INT                  NO-UNDO.
DEF VAR li-start-time    AS INT                  NO-UNDO.
DEF VAR li-end-time      AS INT                  NO-UNDO.
DEF VAR li-emp-over      AS INT                  NO-UNDO.
DEF VAR li-emp-2time     AS INT                  NO-UNDO.
DEF VAR li-emp-tot       AS INT                  NO-UNDO.
DEF VAR li-emp-tot-hr    AS INT                  NO-UNDO.
DEF VAR li-emp-tot-min   AS INT                  NO-UNDO.
DEF VAR li-over-tot-hr   AS INT                  NO-UNDO.
DEF VAR li-over-tot-min  AS INT                  NO-UNDO.
DEF VAR li-2time-tot-hr  AS INT                  NO-UNDO.
DEF VAR li-2time-tot-min AS INT                  NO-UNDO.
DEF VAR ll-first-emp     AS LOG                  NO-UNDO.
DEF VAR v-date           AS DATE                 NO-UNDO.
DEF VAR v-date-2         AS DATE                 NO-UNDO.

DEF BUFFER bf-employee FOR employee.            

FOR EACH machemp NO-LOCK 
    WHERE machemp.employee   EQ ip-emp 
      AND machemp.start_date GE begin_end_date 
      AND machemp.START_date LE end_end_date 
      AND CAN-DO(lv-shifts,machemp.shift)
     BREAK BY machemp.employee  BY machemp.start_date  BY start_time:

    IF FIRST-OF(machemp.employee) THEN ASSIGN ll-first-emp = TRUE.

    ASSIGN
        li-day-time   = machemp.total_time
        li-overtime   = 0 li-2time = 0
        li-start-time = IF li-start-time = 0 
                         THEN machemp.start_time 
                         ELSE li-start-time
        li-end-time   = machemp.end_time.

    ACCUMULATE li-day-time (TOTAL BY machemp.employee 
                                  BY machemp.start_date).

    /* ===== daily total display */
    IF LAST-OF(machemp.start_date) THEN DO:

        /* GET LUNCH TIME */
        FIND shifts NO-LOCK
            WHERE shifts.company = cocode
              AND shifts.shift = machemp.shift NO-ERROR.
        IF AVAIL shifts AND 
           NOT shifts.lunch_paid THEN DO:

            IF machemp.end_time LE shifts.lunch_start 
              THEN li-lunch-time = 0.  
              ELSE
               IF machemp.start_time GE shifts.lunch_end 
                 THEN li-lunch-time = 0.
                 ELSE 
                  IF machemp.start_time LE shifts.lunch_start AND
                     machemp.end_time   GE shifts.lunch_start   AND
                     machemp.end_time   LE shifts.lunch_end
                    THEN li-lunch-time = (machemp.end_time - shifts.lunch_start).
                    ELSE 
                     IF machemp.start_time GE shifts.lunch_start AND
                        machemp.start_time LE shifts.lunch_end   AND
                        machemp.end_time   GE shifts.lunch_end
                       THEN li-lunch-time = (shifts.lunch_end - machemp.start_time).
                       ELSE 
                        IF machemp.start_time GE shifts.lunch_start AND
                           machemp.end_time   LE shifts.lunch_end
                          THEN li-lunch-time = (machemp.end_time - machemp.start_time).
                          ELSE li-lunch-time = shifts.lunch_end - shifts.lunch_start .
        END. /* NOT shifts.lunch_paid */
        ELSE li-lunch-time = 0.

        FIND FIRST employee NO-LOCK 
            WHERE employee.company EQ cocode 
              AND employee.employee EQ machemp.employee NO-ERROR.

        IF employee.lunch_paid THEN ASSIGN li-lunch-time = 0.

        ASSIGN li-lunch-emp = li-lunch-emp + li-lunch-time.

        /*12 Hr  (x 3600) */ 
        IF (ACCUM TOTAL BY machemp.start_date li-day-time) - li-lunch-time GT 43200  
          THEN  
            ASSIGN 
              li-2time    = (ACCUM TOTAL BY machemp.start_date li-day-time) - li-lunch-time - 43200 
              li-overtime = 14400.
          ELSE 
           IF ((ACCUM TOTAL BY machemp.start_date li-day-time) - li-lunch-time) GT 28800  AND 
              ((ACCUM TOTAL BY machemp.start_date li-day-time) - li-lunch-time)  LE 43200
             THEN 
              ASSIGN 
               li-2time = 0
               /* 8 Hr */
               li-overtime = (ACCUM TOTAL by machemp.start_date li-day-time) - li-lunch-time - 28800. 

        ASSIGN 
          li-emp-over  = li-emp-over + li-overtime 
          li-emp-2time = li-emp-2time + li-2time.



            FIND FIRST tt-emp-time 
                WHERE tt-emp-time.t-empid  EQ machemp.employee 
                  AND tt-emp-time.t-lgStDt EQ machemp.start_date NO-ERROR.
            IF NOT AVAIL tt-emp-time THEN DO:
                CREATE tt-emp-time.                           
                ASSIGN                                        
                    tt-emp-time.t-empid  = machemp.employee   
                    tt-emp-time.t-lgStDt = machemp.start_date
                    tt-emp-time.t-fname  = employee.first_name 
                    tt-emp-time.t-lname  = employee.last_name
                    tt-emp-time.t-shift  = machemp.shift . 
            END.


            ASSIGN
              tt-emp-time.t-MlgInTm  = li-start-time
              tt-emp-time.t-MlgOutTm = li-end-time
              tt-emp-time.t-MHrWrk   = ((ACCUM TOTAL BY machemp.start_date li-day-time) - li-lunch-time)
              tt-emp-time.t-MOT      = li-overtime 
              tt-emp-time.t-MOT2     = li-2time.

            ASSIGN ll-first-emp = FALSE.


        ASSIGN li-start-time = 0.  

    END. /* IF LAST-OF(machemp.start_date) */

END. /* for each machemp */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-plantmach-time C-Win 
PROCEDURE get-plantmach-time :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-emp AS CHAR NO-UNDO.

DEF VAR li-overtime      AS INT LABEL "OT @x1.5" NO-UNDO.
DEF VAR li-2time         AS INT LABEL "OT @x2"   NO-UNDO.
DEF VAR li-day-time      AS INT                  NO-UNDO.
DEF VAR li-lunch-time    AS INT                  NO-UNDO.
DEF VAR li-lunch-emp     AS INT                  NO-UNDO.
DEF VAR li-start-time    AS INT                  NO-UNDO.
DEF VAR li-end-time      AS INT                  NO-UNDO.
DEF VAR li-emp-over      AS INT                  NO-UNDO.
DEF VAR li-emp-2time     AS INT                  NO-UNDO.
DEF VAR li-emp-tot       AS INT                  NO-UNDO.
DEF VAR li-emp-tot-hr    AS INT                  NO-UNDO.
DEF VAR li-emp-tot-min   AS INT                  NO-UNDO.
DEF VAR li-over-tot-hr   AS INT                  NO-UNDO.
DEF VAR li-over-tot-min  AS INT                  NO-UNDO.
DEF VAR li-2time-tot-hr  AS INT                  NO-UNDO.
DEF VAR li-2time-tot-min AS INT                  NO-UNDO.
DEF VAR ll-first-emp     AS LOG                  NO-UNDO.
DEF VAR v-date           AS DATE                 NO-UNDO.
DEF VAR v-date-2         AS DATE                 NO-UNDO.

DEF BUFFER bf-employee FOR employee.            
DEF BUFFER bf-machemp  FOR machemp.                      


FOR EACH emplogin NO-LOCK 
    WHERE emplogin.company    EQ cocode 
      AND emplogin.employee   EQ ip-emp 
      AND emplogin.start_date GE begin_end_date 
      AND emplogin.START_date LE end_end_date 
      AND emplogin.machine    NE "CLOCK"
      AND CAN-DO(lv-shifts,emplogin.shift)
     BREAK BY emplogin.employee  BY emplogin.start_date  BY start_time:    

    IF FIRST-OF(emplogin.employee) THEN ASSIGN ll-first-emp = TRUE.

    ASSIGN
        li-day-time   = emplogin.total_time
        li-overtime   = 0 li-2time = 0
        li-start-time = IF li-start-time = 0 
                         THEN emplogin.start_time 
                         ELSE li-start-time
        li-end-time   = emplogin.end_time.

    ACCUMULATE li-day-time (TOTAL BY emplogin.employee 
                                  BY emplogin.start_date).

    /* ===== daily total display */
    IF LAST-OF(emplogin.start_date) THEN DO:

        FIND FIRST employee NO-LOCK 
            WHERE employee.company EQ emplogin.company 
              AND employee.employee EQ emplogin.employee NO-ERROR.

        /* GET LUNCH TIME */
        FIND shifts NO-LOCK
            WHERE shifts.company = emplogin.company 
              AND shifts.shift = emplogin.shift NO-ERROR.
        IF AVAIL shifts AND 
           NOT shifts.lunch_paid THEN DO:

            IF emplogin.end_time LE shifts.lunch_start 
              THEN li-lunch-time = 0.  
              ELSE
               IF emplogin.start_time GE shifts.lunch_end 
                 THEN li-lunch-time = 0.
                 ELSE 
                  IF emplogin.start_time LE shifts.lunch_start AND
                     emplogin.end_time   GE shifts.lunch_start   AND
                     emplogin.end_time   LE shifts.lunch_end
                    THEN li-lunch-time = (emplogin.end_time - shifts.lunch_start).
                    ELSE 
                     IF emplogin.start_time GE shifts.lunch_start AND
                        emplogin.start_time LE shifts.lunch_end   AND
                        emplogin.end_time   GE shifts.lunch_end
                       THEN li-lunch-time = (shifts.lunch_end - emplogin.start_time).
                       ELSE 
                        IF emplogin.start_time GE shifts.lunch_start AND
                           emplogin.end_time   LE shifts.lunch_end
                          THEN li-lunch-time = (emplogin.end_time - emplogin.start_time).
                          ELSE li-lunch-time = shifts.lunch_end - shifts.lunch_start .
        END. /* NOT shifts.lunch_paid */
        ELSE li-lunch-time = 0.


        IF employee.lunch_paid THEN ASSIGN li-lunch-time = 0. 

        ASSIGN li-lunch-emp = li-lunch-emp + li-lunch-time.

        /*12 Hr  (x 3600) */ 
        IF (ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time GT 43200  
          THEN  
            ASSIGN 
              li-2time    = (ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time - 43200 
              li-overtime = 14400.
          ELSE 
           IF ((ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time) GT 28800  AND 
              ((ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time)  LE 43200
             THEN 
              ASSIGN 
               li-2time = 0
               /* 8 Hr */
               li-overtime = (ACCUM TOTAL by emplogin.start_date li-day-time) - li-lunch-time - 28800. 

        ASSIGN 
          li-emp-over  = li-emp-over + li-overtime 
          li-emp-2time = li-emp-2time + li-2time.


            FIND FIRST tt-emp-time 
                WHERE tt-emp-time.t-empid  EQ emplogin.employee 
                  AND tt-emp-time.t-lgStDt EQ emplogin.start_date NO-ERROR.
            IF NOT AVAIL tt-emp-time THEN DO:
                CREATE tt-emp-time.                           
                ASSIGN                                        
                    tt-emp-time.t-empid  = emplogin.employee   
                    tt-emp-time.t-lgStDt = emplogin.start_date
                    tt-emp-time.t-fname  = employee.first_name 
                    tt-emp-time.t-lname  = employee.last_name
                    tt-emp-time.t-shift  = emplogin.shift . 
            END.


            ASSIGN
              tt-emp-time.t-mach    = emplogin.machine
              tt-emp-time.t-lgInTm  = li-start-time
              tt-emp-time.t-lgOutTm = li-end-time
              tt-emp-time.t-HrWrk   = ((ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time)
              tt-emp-time.t-OT      = li-overtime 
              tt-emp-time.t-OT2     = li-2time.

            ASSIGN ll-first-emp = FALSE.

        ASSIGN li-start-time = 0.  



    END. /* IF LAST-OF(emplogin.start_date) */

END. /* for each tt-emp-time */




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
    selected-company = g_company
    begin_employee = ''
    end_employee = 'zzzzzzzz'
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
DEF VAR li-overtime      AS INT LABEL "OT @x1.5" NO-UNDO.
DEF VAR li-2time         AS INT LABEL "OT @x2"   NO-UNDO.
DEF VAR li-day-time      AS INT                  NO-UNDO.
DEF VAR li-lunch-time    AS INT                  NO-UNDO.
DEF VAR li-lunch-emp     AS INT                  NO-UNDO.
DEF VAR li-start-time    AS INT                  NO-UNDO.
DEF VAR li-end-time      AS INT                  NO-UNDO.
DEF VAR li-emp-over      AS INT                  NO-UNDO.
DEF VAR li-emp-2time     AS INT                  NO-UNDO.
DEF VAR li-emp-tot       AS INT                  NO-UNDO.
DEF VAR li-emp-tot-hr    AS INT                  NO-UNDO.
DEF VAR li-emp-tot-min   AS INT                  NO-UNDO.
DEF VAR li-over-tot-hr   AS INT                  NO-UNDO.
DEF VAR li-over-tot-min  AS INT                  NO-UNDO.
DEF VAR li-2time-tot-hr  AS INT                  NO-UNDO.
DEF VAR li-2time-tot-min AS INT                  NO-UNDO.
DEF VAR ll-first-emp     AS LOG                  NO-UNDO.
DEF VAR v-date           AS DATE                 NO-UNDO.
DEF VAR v-date-2         AS DATE                 NO-UNDO.

DEF BUFFER bf-employee FOR employee.
DEF BUFFER bf-machemp  FOR machemp.

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

    ASSIGN 
      excelheader = 
        "EmployeeId,Employee Name,Start Date,Shift,Login Time,Logout Time," +
        "Hrs Worked,OT @x1.5,OT @x2,Login  Mach Time,Logout Mach Time,"   +
        "Total Mach Time" .

    PUT STREAM excel UNFORMATTED excelheader SKIP.

  END.

EMPTY TEMP-TABLE tt-emp.
FOR EACH employee FIELDS(employee) 
  WHERE employee.company EQ cocode 
    AND employee.employee >= begin_employee 
    AND employee.employee <= END_employee NO-LOCK:

    CREATE tt-emp.
    ASSIGN tt-emp.emp = employee.employee.
    RELEASE tt-emp.
END.

EMPTY TEMP-TABLE tt-emp-time.
/* GET ALL EMPLOYEE LOGIN TIME */
FOR EACH tt-emp NO-LOCK:   
    IF scr-plant-clock = 1 
      /* PLANT MACHINE TIME */
      THEN RUN get-plantmach-time (INPUT tt-emp.emp).
      /* CLOCK TIME */
      ELSE RUN get-clock-time (INPUT tt-emp.emp).

    /* MACHINE TIME */
    RUN get-mach-time (INPUT tt-emp.emp).
END. /*end tt-emp-time*/


FOR EACH tt-emp-time 
    WHERE tt-emp-time.t-MHrWrk  ne 0
    BREAK BY tt-emp-time.t-empid BY tt-emp-time.t-lgStDt: 

    IF (tt-emp-time.t-MlgOutTm - tt-emp-time.t-MlgInTm) NE
        tt-emp-time.t-MHrWrk
      THEN
        ASSIGN
            tt-emp-time.t-MHrWrk = tt-emp-time.t-MlgOutTm -
                                   tt-emp-time.t-MlgInTm.

      IF FIRST-OF(tt-emp-time.t-empid) THEN DO:      

          PUT 
            "Employee: " tt-emp-time.t-empid "  " tt-emp-time.t-fname tt-emp-time.t-lname 
           SKIP
            "===================================================================================================="
           SKIP 
           .
          PUT
             "                   Login      Logout    Hrs                             Login      Logout    Total    "
             SKIP
             "Start Date Shift   Time       Time      Worked     OT @x1.5   OT @x2  Mach Time  Mach Time  Mach Time " 
             SKIP
             "---------- ----- ---------- ---------- ---------  ---------- -------- ---------- ---------- --------- "
             SKIP.                       

      END.  /* First-OF */

      PUT 
          tt-emp-time.t-lgStDt                                    SPACE
          tt-emp-time.t-shift                      FORMAT "x(6)"  SPACE

          IF tt-emp-time.t-lgInTm <> 0 
            THEN STRING(tt-emp-time.t-lgInTm,"HH:MM AM")   
            ELSE ""                                FORMAT "x(10)" SPACE
          IF tt-emp-time.t-lgOutTm <> 0 
            THEN STRING(tt-emp-time.t-lgOutTm,"HH:MM AM") 
            ELSE ""                                FORMAT "x(10)" SPACE
          IF tt-emp-time.t-HrWrk <> 0
            THEN STRING(tt-emp-time.t-HrWrk,"HH:MM")    
              ELSE ""                              FORMAT "x(9)"  SPACE
          IF tt-emp-time.t-OT <> 0
            THEN STRING(tt-emp-time.t-OT , "HH:MM")
            ELSE ""                                FORMAT "x(10)" SPACE
          IF tt-emp-time.t-OT2 <> 0
            THEN STRING(tt-emp-time.t-OT2, "HH:MM")      
            ELSE ""                                FORMAT "x(8)"  SPACE

          IF tt-emp-time.t-MlgInTm NE 0 
            THEN STRING(tt-emp-time.t-MlgInTm, "HH:MM AM")      
            ELSE ""                                FORMAT "x(10)" SPACE
          IF tt-emp-time.t-MlgOutTm NE 0 
            THEN STRING(tt-emp-time.t-MlgOutTm, "HH:MM AM")      
            ELSE ""                                FORMAT "x(10)" SPACE

          IF tt-emp-time.t-MHrWrk GT 0 
           THEN STRING(tt-emp-time.t-MHrWrk,"HH:MM") 
           ELSE ""                                 FORMAT "x(9)"  SPACE
         SKIP.

         ASSIGN
          li-day-time =  li-day-time + tt-emp-time.t-HrWrk
          li-emp-tot  =  li-emp-tot  + 
                         IF tt-emp-time.t-MHrWrk GT 0 
                           THEN tt-emp-time.t-MHrWrk ELSE 0.

         IF tb_excel THEN DO:
             EXPORT STREAM excel DELIMITER ","
                 tt-emp-time.t-empid 
                 (tt-emp-time.t-fname + " " + tt-emp-time.t-lname)
                 tt-emp-time.t-lgStDt
                 tt-emp-time.t-shift
                 STRING(tt-emp-time.t-lgInTm,"HH:MM AM")
                 STRING(tt-emp-time.t-lgOutTm,"HH:MM AM")
                 STRING(tt-emp-time.t-HrWrk,"HH:MM")
                 STRING(tt-emp-time.t-OT,"HH:MM")
                 STRING(tt-emp-time.t-OT2,"HH:MM")
                 STRING(tt-emp-time.t-MlgInTm,"HH:MM AM")
                 STRING(tt-emp-time.t-MlgOutTm, "HH:MM AM")
                 STRING(tt-emp-time.t-MHrWrk,"HH:MM").

         END.



      IF LAST-OF(tt-emp-time.t-empid) THEN DO:

          PUT 
              SKIP(1)

                 "Total" 
                SPACE (35)
                 IF li-day-time NE 0 
                  THEN 
                     STRING(li-day-time,"HH:MM") 
                  ELSE "" FORMAT "x(5)"
                SPACE (47)                  
                 IF li-emp-tot NE 0 
                   THEN 
                     STRING(li-emp-tot,"HH:MM")                  
                   ELSE "" FORMAT "x(5)"
                SKIP (3).



          ASSIGN 
              li-day-time = 0 li-emp-tot = 0.
      END.


END.
OUTPUT CLOSE.

IF tb_excel THEN DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_runExcel THEN
        OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname,FRAME {&FRAME-NAME}:HANDLE).

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

