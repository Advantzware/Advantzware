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
  FIELD rec_key LIKE ASI.notes.rec_key
  FIELD note_date LIKE ASI.notes.note_date
  FIELD note_title LIKE ASI.notes.note_title
  FIELD note_src AS CHARACTER
  INDEX noteindex employee note_date.

DEFINE TEMP-TABLE tt-emp NO-UNDO
    FIELD emp AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-6 begin_employee end_employee ~
begin_type end_type begin_end_date end_end_date begin_note-date ~
end_note-date lv-labor-only scr-plant-clock lv-note-only rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS selected-company begin_employee ~
end_employee begin_type end_type begin_end_date end_end_date ~
begin_note-date end_note-date lv-labor-only scr-plant-clock lv-note-only ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm lv-font-name ~
tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_note-date AS DATE FORMAT "99/99/9999" 
     LABEL "Beginning Note Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_type AS CHARACTER FORMAT "X(5)" 
     LABEL "Beginning Employee Type" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE end_employee AS CHARACTER FORMAT "X(5)" 
     LABEL "Ending Employee" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE end_end_date AS DATE FORMAT "99/99/9999" 
     LABEL "Ending Labor Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_note-date AS DATE FORMAT "99/99/9999" 
     LABEL "Ending Note Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_type AS CHARACTER FORMAT "X(5)" INITIAL "zzz" 
     LABEL "Ending Employee Type" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-emplog.csv" 
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

DEFINE VARIABLE selected-company AS CHARACTER FORMAT "X(3)":U 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

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
"Clock", 2,
"All", 3
     SIZE 40 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.48.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 9.29.

DEFINE VARIABLE lv-labor-only AS LOGICAL INITIAL yes 
     LABEL "Print Labor" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE lv-note-only AS LOGICAL INITIAL yes 
     LABEL "Print Notes" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

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
     selected-company AT ROW 2.19 COL 28 COLON-ALIGNED
     begin_employee AT ROW 3.62 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Employee"
     end_employee AT ROW 3.62 COL 67 COLON-ALIGNED HELP
          "Enter Ending Employee"
     begin_type AT ROW 4.81 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Employee Type" WIDGET-ID 10
     end_type AT ROW 4.81 COL 67 COLON-ALIGNED HELP
          "Enter Ending Employee Type" WIDGET-ID 12
     begin_end_date AT ROW 6 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Employee"
     end_end_date AT ROW 6 COL 67 COLON-ALIGNED HELP
          "Enter Ending Employee"
     begin_note-date AT ROW 7.43 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Employee"
     end_note-date AT ROW 7.43 COL 67 COLON-ALIGNED HELP
          "Enter Ending Employee"
     lv-labor-only AT ROW 8.86 COL 30
     scr-plant-clock AT ROW 9.95 COL 50 NO-LABEL WIDGET-ID 4
     lv-note-only AT ROW 10.05 COL 30
     rd-dest AT ROW 13.86 COL 6 NO-LABEL
     lv-ornt AT ROW 13.86 COL 29 NO-LABEL
     lines-per-page AT ROW 13.86 COL 82 COLON-ALIGNED
     lv-font-no AT ROW 16 COL 33 COLON-ALIGNED
     td-show-parm AT ROW 16 COL 51
     lv-font-name AT ROW 16.95 COL 27 COLON-ALIGNED NO-LABEL
     tb_excel AT ROW 18.38 COL 47
     tb_runExcel AT ROW 18.38 COL 89.8 RIGHT-ALIGNED
     fi_file AT ROW 19.43 COL 44.8 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 21.67 COL 18
     btn-cancel AT ROW 21.67 COL 56
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Time Calculation Basis:" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 9.33 COL 50 WIDGET-ID 8
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.67 COL 3
     RECT-2 AT ROW 1.24 COL 1
     RECT-6 AT ROW 11.95 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 22.81.


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
         TITLE              = "Labor Report"
         HEIGHT             = 22.43
         WIDTH              = 96
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

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
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
       begin_note-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       begin_type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       end_employee:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       end_end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       end_note-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       end_type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lv-font-name:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       lv-font-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       lv-labor-only:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       lv-note-only:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR FILL-IN selected-company IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       selected-company:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       td-show-parm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Labor Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Labor Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_end_date C-Win
ON LEAVE OF begin_end_date IN FRAME FRAME-A /* Beginning Labor Date */
DO:
  begin_note-date:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  ASSIGN {&displayed-objects}.

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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_end_date C-Win
ON LEAVE OF end_end_date IN FRAME FRAME-A /* Ending Labor Date */
DO:
  end_note-date:SCREEN-VALUE = SELF:SCREEN-VALUE.
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
{sys/inc/f3helpw.i} 
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

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
  DO WITH FRAME {&frame-name}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
  END.
  {methods/nowait.i}
  APPLY 'ENTRY' TO begin_employee IN FRAME {&FRAME-NAME}.
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY selected-company begin_employee end_employee begin_type end_type 
          begin_end_date end_end_date begin_note-date end_note-date 
          lv-labor-only scr-plant-clock lv-note-only rd-dest lv-ornt 
          lines-per-page lv-font-no td-show-parm lv-font-name tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-2 RECT-6 begin_employee end_employee begin_type end_type 
         begin_end_date end_end_date begin_note-date end_note-date 
         lv-labor-only scr-plant-clock lv-note-only rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel 
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
    selected-company = g_company
    begin_employee = ''
    end_employee = 'zzzzzzzz'
    end_end_date = TODAY
    end_note-date = TODAY.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-2 C-Win 
PROCEDURE proc-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ip-emp AS CHAR NO-UNDO.

    DEFINE VARIABLE li-overtime AS INTEGER LABEL "OT @x1.5" NO-UNDO.
    DEFINE VARIABLE li-2time AS INTEGER LABEL "OT @x2" NO-UNDO.
    DEFINE VARIABLE li-day-time AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-lunch-time AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-lunch-emp AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-start-time AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-end-time AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-over AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-2time AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot-hr AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot-min AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-over-tot-hr AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-over-tot-min AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-2time-tot-hr AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-2time-tot-min AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll-first-emp AS LOGICAL NO-UNDO.

    for each emplogin no-lock WHERE 
        emplogin.company EQ cocode AND
        emplogin.employee = ip-emp and
        emplogin.start_date >= begin_end_date AND
        emplogin.START_date <= end_end_date AND
        emplogin.machine EQ "CLOCK"
        break by emplogin.employee by emplogin.start_date by start_time:

        if first-of(emplogin.employee) then do:
           find employee where employee.employee = emplogin.employee  no-lock no-error.
           if not avail employee then next.
         /*  put "Employee: " employee.employee "  " employee.first_name employee.last_name skip
               "====================================================================" skip.                    
           IF tb_excel THEN PUT STREAM excel UNFORMATTED 
              employee.employee "," replace(employee.first_name + " " + employee.last_name,",",".") ",".
           */
           if lv-labor-only then 
           put            "                 Login      Logout     Hrs                            " skip  
               "Start Date Shift Time       Time       Worked     OT @x1.5   OT @x2" skip
               "---------- ----- ---------- ---------- ---------  ---------- -------" skip.
           ASSIGN ll-first-emp = TRUE.
        end.                      

        assign li-day-time = emplogin.total_time
               li-overtime = 0
               li-2time = 0
               li-start-time = if li-start-time = 0 then emplogin.start_time else li-start-time.
               li-end-time = emplogin.end_time.
        accumulate li-day-time (total by emplogin.employee by emplogin.start_date).
        /* ===== daily total display */
        if last-of(emplogin.start_date) then do:
           find shifts where shifts.company = emplogin.company and
                            shifts.shift = emplogin.shift
                            no-lock no-error.
           if avail shifts and not shifts.lunch_paid then do:
              if  emplogin.end_time <= shifts.lunch_start then li-lunch-time = 0.  
              else if emplogin.start_time >= shifts.lunch_end then li-lunch-time = 0.
              else if emplogin.start_time <= shifts.lunch_start and
                      emplogin.end_time >= shifts.lunch_start and
                      emplogin.end_time <= shifts.lunch_end 
                   then li-lunch-time = emplogin.end_time - shifts.lunch_start.
              else if emplogin.start_time >= shifts.lunch_start and
                      emplogin.start_time <= shifts.lunch_end and
                      emplogin.end_time >= shifts.lunch_end 
                   then li-lunch-time = shifts.lunch_end - emplogin.start_time .
              else if emplogin.start_time >= shifts.lunch_start and
                      emplogin.end_time <= shifts.lunch_end 
                   then li-lunch-time = emplogin.end_time - emplogin.start_time.                    
              else li-lunch-time = shifts.lunch_end - shifts.lunch_start .          
           end.
           else li-lunch-time = 0.
           if employee.lunch_paid then li-lunch-time = 0. 
           li-lunch-emp = li-lunch-emp + li-lunch-time.
           if (accum total by emplogin.start_date li-day-time) - li-lunch-time > 43200  /*12 Hr  (x 3600) */ 
                 then  assign li-2time = (accum total by emplogin.start_date li-day-time) - li-lunch-time - 43200
                              li-overtime = 14400.
           else if ((accum total by emplogin.start_date li-day-time) - li-lunch-time) > 28800  and 
                   ((accum total by emplogin.start_date li-day-time) - li-lunch-time)  <= 43200
                 then assign li-2time = 0
                             li-overtime = (accum total by emplogin.start_date li-day-time) - li-lunch-time - 28800. /* 8 Hr */
           assign li-emp-over = li-emp-over + li-overtime 
                  li-emp-2time = li-emp-2time + li-2time .
           if lv-labor-only THEN DO:        
              disp emplogin.start_date
                   emplogin.shift form "x(5)"
                   string(li-start-time,"HH:MM AM") /* column-label "Login!Time" */ form "x(10)" 
                   string(li-end-time,"HH:MM AM")  /*column-label "Logout!Time" */ form "x(10)"
                   if (accum total by emplogin.start_date li-day-time) <> 0 then 
                   string((accum total by emplogin.start_date li-day-time) - li-lunch-time, "HH:MM")
                   else "0"      @ li-day-time
                   string(li-overtime, "HH:MM") when li-overtime <> 0 @ li-overtime
                   string(li-2time, "HH:MM") when li-2time <> 0 @ li-2time
                with frame det stream-io  down no-label.
               IF tb_excel THEN PUT STREAM excel UNFORMATTED
                  (IF ll-first-emp THEN "" ELSE ",,")
                  (IF emplogin.start_date = ? THEN "" ELSE string(emplogin.start_date))  ","
                  emplogin.shift ","
                  string(li-start-time,"HH:MM AM")  ","
                  string(li-end-time,"HH:MM AM")  ","
                  (if  (accum total by emplogin.start_date li-day-time) <> 0
                   AND (accum total by emplogin.start_date li-day-time) <> ?
                       then string((accum total by emplogin.start_date li-day-time) - li-lunch-time, "HH:MM")
                       else "0") ","
                  string(li-overtime, "HH:MM") ","
                  string(li-2time, "HH:MM")  SKIP.
               ASSIGN ll-first-emp = FALSE.
           END.
           li-start-time = 0.  
        end. 

        if last-of(emplogin.employee) then do:
           if lv-labor-only then do:
              down with frame det.
              assign li-emp-tot = if (accum total by emplogin.employee li-day-time) <> 0 then ((accum total by emplogin.employee li-day-time) - li-lunch-emp ) else 0 
                     li-emp-tot = IF li-emp-tot = ? THEN 0 ELSE li-emp-tot
                     li-emp-over = if li-emp-over <> 0 then (li-emp-over /*- li-lunch-emp*/) else 0
                     li-emp-2time = if li-emp-2time <> 0 then (li-emp-2time /*- li-lunch-emp*/ ) else 0
                     li-emp-tot-hr = truncate(li-emp-tot / 3600,0)
                     li-emp-tot-min = truncate((li-emp-tot mod 3600) / 60,0)
                     li-over-tot-hr = if li-emp-over > 0 then truncate(li-emp-over / 3600,0) else 0
                     li-over-tot-min = if li-emp-over > 0 then truncate((li-emp-over mod 3600) / 60,0) else 0
                     li-2time-tot-hr = if li-emp-2time > 0 then truncate(li-emp-2time / 3600,0) else 0
                     li-2time-tot-min = if li-emp-2time > 0 then truncate((li-emp-2time mod 3600) / 60,0) else 0.
              disp "Total" @ emplogin.start_date
                string(li-emp-tot-hr,">99") + ":" + string(li-emp-tot-min,"99") when li-emp-tot <> 0 @ li-day-time
                string(li-over-tot-hr,">99") + ":" + string(li-over-tot-min,"99") when li-emp-over <> 0 @ li-overtime
                string(li-2time-tot-hr,">99") + ":" + string(li-2time-tot-min,"99") when li-emp-2time <> 0 @ li-2time
                with frame det down.
              put skip(1).  
              IF tb_excel THEN PUT STREAM excel UNFORMATTED
                 ",,Total,,,,"
                 string(li-emp-tot-hr,">99") + ":" + string(li-emp-tot-min,"99")  "," 
                 string(li-over-tot-hr,">99") + ":" + string(li-over-tot-min,"99")  "," 
                 string(li-2time-tot-hr,">99") + ":" + string(li-2time-tot-min,"99")
                 SKIP.
           end.  
           assign li-lunch-emp = 0
                  li-emp-over = 0
                  li-emp-2time = 0.
           /*if lv-note-only then do:
              put "Notes:" skip .
              for each tt-note where tt-note.employee = emplogin.employee
                                  by tt-note.note_date:
                  put "     " tt-note.note_date " " tt-note.note_title skip.
              end.                       
              put skip(1).
           end.
          */ 
        end.           
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-all C-Win 
PROCEDURE proc-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ip-emp AS CHAR NO-UNDO.

    DEFINE VARIABLE li-overtime AS INTEGER LABEL "OT @x1.5" NO-UNDO.
    DEFINE VARIABLE li-2time AS INTEGER LABEL "OT @x2" NO-UNDO.
    DEFINE VARIABLE li-day-time AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-lunch-time AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-lunch-emp AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-start-time AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-end-time AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-over AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-2time AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot-hr AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot-min AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-over-tot-hr AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-over-tot-min AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-2time-tot-hr AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-2time-tot-min AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll-first-emp AS LOGICAL NO-UNDO.

    for each emplogin no-lock WHERE 
        emplogin.company EQ cocode AND
        emplogin.employee = ip-emp and
        emplogin.start_date >= begin_end_date AND
        emplogin.START_date <= end_end_date 
        break by emplogin.employee by emplogin.start_date by start_time:

        if first-of(emplogin.employee) then do:
           find employee where employee.employee = emplogin.employee  no-lock no-error.
           if not avail employee then next.
         /*  put "Employee: " employee.employee "  " employee.first_name employee.last_name skip
               "====================================================================" skip.                    
           IF tb_excel THEN PUT STREAM excel UNFORMATTED 
              employee.employee "," replace(employee.first_name + " " + employee.last_name,",",".") ",".
           */
           if lv-labor-only then 
           put            "                 Login      Logout     Hrs                            " skip  
               "Start Date Shift Time       Time       Worked     OT @x1.5   OT @x2" skip
               "---------- ----- ---------- ---------- ---------  ---------- -------" skip.
           ASSIGN ll-first-emp = TRUE.
        end.                      

        assign li-day-time = emplogin.total_time
               li-overtime = 0
               li-2time = 0
               li-start-time = if li-start-time = 0 then emplogin.start_time else li-start-time.
               li-end-time = emplogin.end_time.
        accumulate li-day-time (total by emplogin.employee by emplogin.start_date).
        /* ===== daily total display */
        if last-of(emplogin.start_date) then do:
           find shifts where shifts.company = emplogin.company and
                            shifts.shift = emplogin.shift
                            no-lock no-error.
           if avail shifts and not shifts.lunch_paid then do:
              if  emplogin.end_time <= shifts.lunch_start then li-lunch-time = 0.  
              else if emplogin.start_time >= shifts.lunch_end then li-lunch-time = 0.
              else if emplogin.start_time <= shifts.lunch_start and
                      emplogin.end_time >= shifts.lunch_start and
                      emplogin.end_time <= shifts.lunch_end 
                   then li-lunch-time = emplogin.end_time - shifts.lunch_start.
              else if emplogin.start_time >= shifts.lunch_start and
                      emplogin.start_time <= shifts.lunch_end and
                      emplogin.end_time >= shifts.lunch_end 
                   then li-lunch-time = shifts.lunch_end - emplogin.start_time .
              else if emplogin.start_time >= shifts.lunch_start and
                      emplogin.end_time <= shifts.lunch_end 
                   then li-lunch-time = emplogin.end_time - emplogin.start_time.                    
              else li-lunch-time = shifts.lunch_end - shifts.lunch_start .          
           end.
           else li-lunch-time = 0.
           if employee.lunch_paid then li-lunch-time = 0. 
           li-lunch-emp = li-lunch-emp + li-lunch-time.
           if (accum total by emplogin.start_date li-day-time) - li-lunch-time > 43200  /*12 Hr  (x 3600) */ 
                 then  assign li-2time = (accum total by emplogin.start_date li-day-time) - li-lunch-time - 43200
                              li-overtime = 14400.
           else if ((accum total by emplogin.start_date li-day-time) - li-lunch-time) > 28800  and 
                   ((accum total by emplogin.start_date li-day-time) - li-lunch-time)  <= 43200
                 then assign li-2time = 0
                             li-overtime = (accum total by emplogin.start_date li-day-time) - li-lunch-time - 28800. /* 8 Hr */
           assign li-emp-over = li-emp-over + li-overtime 
                  li-emp-2time = li-emp-2time + li-2time .
           if lv-labor-only THEN DO:        
              disp emplogin.start_date
                   emplogin.shift form "x(5)"
                   string(li-start-time,"HH:MM AM") /* column-label "Login!Time" */ form "x(10)" 
                   string(li-end-time,"HH:MM AM")  /*column-label "Logout!Time" */ form "x(10)"
                   if (accum total by emplogin.start_date li-day-time) <> 0 then 
                   string((accum total by emplogin.start_date li-day-time) - li-lunch-time, "HH:MM")
                   else "0"      @ li-day-time
                   string(li-overtime, "HH:MM") when li-overtime <> 0 @ li-overtime
                   string(li-2time, "HH:MM") when li-2time <> 0 @ li-2time
                with frame det stream-io  down no-label.
               IF tb_excel THEN PUT STREAM excel UNFORMATTED
                  (IF ll-first-emp THEN "" ELSE ",,")
                  (IF emplogin.start_date = ? THEN "" ELSE string(emplogin.start_date))  ","
                  emplogin.shift ","
                  string(li-start-time,"HH:MM AM")  ","
                  string(li-end-time,"HH:MM AM")  ","
                  (if  (accum total by emplogin.start_date li-day-time) <> 0
                   AND (accum total by emplogin.start_date li-day-time) <> ?
                       then string((accum total by emplogin.start_date li-day-time) - li-lunch-time, "HH:MM")
                       else "0") ","
                  string(li-overtime, "HH:MM") ","
                  string(li-2time, "HH:MM")  SKIP.
               ASSIGN ll-first-emp = FALSE.
           END.
           li-start-time = 0.  
        end. 

        if last-of(emplogin.employee) then do:
           if lv-labor-only then do:
              down with frame det.
              assign li-emp-tot = if (accum total by emplogin.employee li-day-time) <> 0 then ((accum total by emplogin.employee li-day-time) - li-lunch-emp ) else 0 
                     li-emp-tot = IF li-emp-tot = ? THEN 0 ELSE li-emp-tot
                     li-emp-over = if li-emp-over <> 0 then (li-emp-over /*- li-lunch-emp*/) else 0
                     li-emp-2time = if li-emp-2time <> 0 then (li-emp-2time /*- li-lunch-emp*/ ) else 0
                     li-emp-tot-hr = truncate(li-emp-tot / 3600,0)
                     li-emp-tot-min = truncate((li-emp-tot mod 3600) / 60,0)
                     li-over-tot-hr = if li-emp-over > 0 then truncate(li-emp-over / 3600,0) else 0
                     li-over-tot-min = if li-emp-over > 0 then truncate((li-emp-over mod 3600) / 60,0) else 0
                     li-2time-tot-hr = if li-emp-2time > 0 then truncate(li-emp-2time / 3600,0) else 0
                     li-2time-tot-min = if li-emp-2time > 0 then truncate((li-emp-2time mod 3600) / 60,0) else 0.
              disp "Total" @ emplogin.start_date
                string(li-emp-tot-hr,">99") + ":" + string(li-emp-tot-min,"99") when li-emp-tot <> 0 @ li-day-time
                string(li-over-tot-hr,">99") + ":" + string(li-over-tot-min,"99") when li-emp-over <> 0 @ li-overtime
                string(li-2time-tot-hr,">99") + ":" + string(li-2time-tot-min,"99") when li-emp-2time <> 0 @ li-2time
                with frame det down.
              put skip(1).  
              IF tb_excel THEN PUT STREAM excel UNFORMATTED
                 ",,Total,,,,"
                 string(li-emp-tot-hr,">99") + ":" + string(li-emp-tot-min,"99")  "," 
                 string(li-over-tot-hr,">99") + ":" + string(li-over-tot-min,"99")  "," 
                 string(li-2time-tot-hr,">99") + ":" + string(li-2time-tot-min,"99")
                 SKIP.
           end.  
           assign li-lunch-emp = 0
                  li-emp-over = 0
                  li-emp-2time = 0.
           /*if lv-note-only then do:
              put "Notes:" skip .
              for each tt-note where tt-note.employee = emplogin.employee
                                  by tt-note.note_date:
                  put "     " tt-note.note_date " " tt-note.note_title skip.
              end.                       
              put skip(1).
           end.
          */ 
        end.           
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*==== Report main body procedure ================================*/

  DEFINE VARIABLE li-overtime AS INTEGER LABEL "OT @x1.5" NO-UNDO.
  DEFINE VARIABLE li-2time AS INTEGER LABEL "OT @x2" NO-UNDO.
  DEFINE VARIABLE li-day-time AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-lunch-time AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-lunch-emp AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-start-time AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-end-time AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-emp-over AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-emp-2time AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-emp-tot AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-emp-tot-hr AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-emp-tot-min AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-over-tot-hr AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-over-tot-min AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-2time-tot-hr AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-2time-tot-min AS INTEGER NO-UNDO.
  DEFINE VARIABLE ll-first-emp AS LOGICAL NO-UNDO.
  DEF VAR v-date AS DATE NO-UNDO.
  DEF VAR v-date-2 AS DATE NO-UNDO.

  DEFINE BUFFER bf-employee for employee.
  DEFINE BUFFER bf-machemp for machemp.

  SESSION:SET-WAIT-STATE('general').

  EMPTY TEMP-TABLE tt-note.

  {sys/form/r-top3w.f}

  FORM HEADER SKIP(1) WITH FRAME r-top.

  ASSIGN
     str-tit2 = c-win:TITLE + "  (T-R-9)" 
     {sys/inc/ctrtext.i str-tit2 112}
     str-tit3 = FILL('-',132)
     {sys/inc/ctrtext.i str-tit3 132}.

  {sys/inc/print1.i}
  {sys/inc/outprint.i VALUE(lines-per-page)}

  IF td-show-parm THEN RUN show-param.

  VIEW FRAME r-top.

  IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(fi_file).
    ASSIGN excelheader = "Employee,Name".
    IF lv-labor-only THEN ASSIGN excelheader = excelheader +
      ",Start Date,Shift,Login Time,Logout Time,Worked,OT @ 1.5, OT @ 2".
    PUT STREAM excel UNFORMATTED excelheader SKIP.
  END.

  EMPTY TEMP-TABLE tt-emp.

  FOR EACH employee FIELDS(employee) WHERE
      employee.company EQ cocode AND
      employee.employee >= begin_employee AND
      employee.employee <= END_employee AND
      employee.emp_type >= begin_type AND
      employee.emp_type <= end_type
      NO-LOCK:

      CREATE tt-emp.
      ASSIGN tt-emp.emp = employee.employee.
      RELEASE tt-emp.
  END.

  for EACH tt-emp BREAK BY tt-emp.emp:
    STATUS DEFAULT "Processing .. for employee " + tt-emp.emp.

    IF FIRST-OF(tt-emp.emp) THEN DO:
      find employee where employee.employee = tt-emp.emp  no-lock no-error.
      put "Employee: " .
      IF AVAIL employee THEN 
            PUT employee.employee "  " employee.first_name employee.last_name.
      ELSE tt-emp.emp.
      PUT SKIP "====================================================================" skip.     
      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED 
             employee.employee "," replace(employee.first_name + " " + employee.last_name,",",".") ",".
    END.
    if lv-note-only then do:
       for each bf-employee FIELDS(rec_key employee) where
          bf-employee.company EQ cocode AND
          bf-employee.employee = tt-emp.emp and
          CAN-FIND(FIRST nosweat.note WHERE nosweat.note.rec_key = bf-employee.rec_key) NO-LOCK:

          DO v-date-2 = begin_note-date TO end_note-date:
             FOR each nosweat.note FIELDS(rec_key note_date note_title) where
                 nosweat.note.rec_key = bf-employee.rec_key and
                 nosweat.note.note_date = v-date-2 NO-LOCK:

                create tt-note.
                assign tt-note.employee = bf-employee.employee
                       tt-note.rec_key = nosweat.note.rec_key
                       tt-note.note_date = nosweat.note.note_date
                       tt-note.note_title = nosweat.note.note_title
                       tt-note.note_src = "Employee".                
             END.
          END.
       end.
       FOR each emplogin FIELDS(rec_key employee) where
              emplogin.company EQ cocode AND
              emplogin.employee = tt-emp.emp and
              can-find(FIRST nosweat.note WHERE nosweat.note.rec_key = emplogin.rec_key
                       AND nosweat.note.note_date >= begin_note-date 
                       AND nosweat.note.note_date <= END_note-date) NO-LOCK:
           DO v-date-2 = begin_note-date TO end_note-date:
              FOR each nosweat.note FIELDS(rec_key note_date note_title) where
                     nosweat.note.rec_key = emplogin.rec_key and 
                     nosweat.note.note_date = v-date-2 no-lock:

                     create tt-note.
                     assign tt-note.employee = emplogin.employee
                            tt-note.rec_key = nosweat.note.rec_key
                            tt-note.note_date = nosweat.note.note_date
                            tt-note.note_title = nosweat.note.note_title
                            tt-note.note_src = "Log In/Out".

              END.              
           END.
       END. /*end for each emplogin*/

       for each bf-machemp FIELDS(rec_key employee) where
              bf-machemp.employee = tt-emp.emp and
              can-find(FIRST nosweat.note WHERE nosweat.note.rec_key = bf-machemp.rec_key
                       AND nosweat.note.note_date >= begin_note-date
                       AND nosweat.note.note_date <= END_note-date) NO-LOCK:
           DO v-date-2 = begin_note-date TO end_note-date:
              FOR each nosweat.note FIELDS(rec_key note_date note_title) where
                     nosweat.note.rec_key = bf-machemp.rec_key and
                     nosweat.note.note_date = v-date-2 NO-LOCK:
                     create tt-note.
                     assign tt-note.employee = bf-machemp.employee
                            tt-note.rec_key = nosweat.note.rec_key
                            tt-note.note_date = nosweat.note.note_date
                            tt-note.note_title = nosweat.note.note_title
                            tt-note.note_src = "Emp. Transaction".

              END.
           END.
       END.              
    END. /* lv-note-only*/

    IF lv-labor-only THEN DO:
       IF scr-plant-clock = 1 THEN
      for each emplogin no-lock WHERE 
        emplogin.company EQ cocode AND
        emplogin.employee = tt-emp.emp and
        emplogin.start_date >= begin_end_date AND
        emplogin.START_date <= end_end_date AND
        emplogin.machine NE "CLOCK"
        break by emplogin.employee by emplogin.start_date by start_time:

        if first-of(emplogin.employee) then do:
          /* find employee where employee.employee = emplogin.employee  no-lock no-error.
           if not avail employee then next.
           put "Employee: " employee.employee "  " employee.first_name employee.last_name skip
               "====================================================================" skip.           
           IF tb_excel THEN PUT STREAM excel UNFORMATTED 
              employee.employee "," replace(employee.first_name + " " + employee.last_name,",",".") ",".
           */
           if lv-labor-only then 
           put            "                 Login      Logout     Hrs                            " skip  
               "Start Date Shift Time       Time       Worked     OT @x1.5   OT @x2" skip
               "---------- ----- ---------- ---------- ---------  ---------- -------" skip.
           ASSIGN ll-first-emp = TRUE.
        end.                      

        assign li-day-time = emplogin.total_time
               li-overtime = 0
               li-2time = 0
               li-start-time = if li-start-time = 0 then emplogin.start_time else li-start-time.
               li-end-time = emplogin.end_time.
        accumulate li-day-time (total by emplogin.employee by emplogin.start_date).
        /* ===== daily total display */
        if last-of(emplogin.start_date) then do:
           find shifts where shifts.company = emplogin.company and
                            shifts.shift = emplogin.shift
                            no-lock no-error.
           if avail shifts and not shifts.lunch_paid then do:
              if  emplogin.end_time <= shifts.lunch_start then li-lunch-time = 0.  
              else if emplogin.start_time >= shifts.lunch_end then li-lunch-time = 0.
              else if emplogin.start_time <= shifts.lunch_start and
                      emplogin.end_time >= shifts.lunch_start and
                      emplogin.end_time <= shifts.lunch_end 
                   then li-lunch-time = emplogin.end_time - shifts.lunch_start.
              else if emplogin.start_time >= shifts.lunch_start and
                      emplogin.start_time <= shifts.lunch_end and
                      emplogin.end_time >= shifts.lunch_end 
                   then li-lunch-time = shifts.lunch_end - emplogin.start_time .
              else if emplogin.start_time >= shifts.lunch_start and
                      emplogin.end_time <= shifts.lunch_end 
                   then li-lunch-time = emplogin.end_time - emplogin.start_time.                    
              else li-lunch-time = shifts.lunch_end - shifts.lunch_start .          
           end.
           else li-lunch-time = 0.
           if employee.lunch_paid then li-lunch-time = 0. 
           li-lunch-emp = li-lunch-emp + li-lunch-time.
           if (accum total by emplogin.start_date li-day-time) - li-lunch-time > 43200  /*12 Hr  (x 3600) */ 
                 then  assign li-2time = (accum total by emplogin.start_date li-day-time) - li-lunch-time - 43200
                              li-overtime = 14400.
           else if ((accum total by emplogin.start_date li-day-time) - li-lunch-time) > 28800  and 
                   ((accum total by emplogin.start_date li-day-time) - li-lunch-time)  <= 43200
                 then assign li-2time = 0
                             li-overtime = (accum total by emplogin.start_date li-day-time) - li-lunch-time - 28800. /* 8 Hr */
           assign li-emp-over = li-emp-over + li-overtime 
                  li-emp-2time = li-emp-2time + li-2time .
           if lv-labor-only THEN DO:        
              disp emplogin.start_date
                   emplogin.shift form "x(5)"
                   string(li-start-time,"HH:MM AM") /* column-label "Login!Time" */ form "x(10)" 
                   string(li-end-time,"HH:MM AM")  /*column-label "Logout!Time" */ form "x(10)"
                   if (accum total by emplogin.start_date li-day-time) <> 0 then 
                   string((accum total by emplogin.start_date li-day-time) - li-lunch-time, "HH:MM")
                   else "0"      @ li-day-time
                   string(li-overtime, "HH:MM") when li-overtime <> 0 @ li-overtime
                   string(li-2time, "HH:MM") when li-2time <> 0 @ li-2time
                with frame det stream-io  down no-label.
               IF tb_excel THEN PUT STREAM excel UNFORMATTED
                  (IF ll-first-emp THEN "" ELSE ",,")
                  (IF emplogin.start_date = ? THEN "" ELSE string(emplogin.start_date))  ","
                  emplogin.shift ","
                  string(li-start-time,"HH:MM AM")  ","
                  string(li-end-time,"HH:MM AM")  ","
                  (if  (accum total by emplogin.start_date li-day-time) <> 0
                   AND (accum total by emplogin.start_date li-day-time) <> ?
                       then string((accum total by emplogin.start_date li-day-time) - li-lunch-time, "HH:MM")
                       else "0") ","
                  string(li-overtime, "HH:MM") ","
                  string(li-2time, "HH:MM")  SKIP.
               ASSIGN ll-first-emp = FALSE.
           END.
           li-start-time = 0.  
        end. 

        if last-of(emplogin.employee) then do:
           if lv-labor-only then do:
              down with frame det.
              assign li-emp-tot = if (accum total by emplogin.employee li-day-time) <> 0 then ((accum total by emplogin.employee li-day-time) - li-lunch-emp ) else 0 
                     li-emp-tot = IF li-emp-tot = ? THEN 0 ELSE li-emp-tot
                     li-emp-over = if li-emp-over <> 0 then (li-emp-over /*- li-lunch-emp*/) else 0
                     li-emp-2time = if li-emp-2time <> 0 then (li-emp-2time /*- li-lunch-emp*/ ) else 0
                     li-emp-tot-hr = truncate(li-emp-tot / 3600,0)
                     li-emp-tot-min = truncate((li-emp-tot mod 3600) / 60,0)
                     li-over-tot-hr = if li-emp-over > 0 then truncate(li-emp-over / 3600,0) else 0
                     li-over-tot-min = if li-emp-over > 0 then truncate((li-emp-over mod 3600) / 60,0) else 0
                     li-2time-tot-hr = if li-emp-2time > 0 then truncate(li-emp-2time / 3600,0) else 0
                     li-2time-tot-min = if li-emp-2time > 0 then truncate((li-emp-2time mod 3600) / 60,0) else 0.
              disp "Total" @ emplogin.start_date
                string(li-emp-tot-hr,">99") + ":" + string(li-emp-tot-min,"99") when li-emp-tot <> 0 @ li-day-time
                string(li-over-tot-hr,">99") + ":" + string(li-over-tot-min,"99") when li-emp-over <> 0 @ li-overtime
                string(li-2time-tot-hr,">99") + ":" + string(li-2time-tot-min,"99") when li-emp-2time <> 0 @ li-2time
                with frame det down.
              put skip(1).  
              IF tb_excel THEN PUT STREAM excel UNFORMATTED
                 ",,Total,,,,"
                 string(li-emp-tot-hr,">99") + ":" + string(li-emp-tot-min,"99")  "," 
                 string(li-over-tot-hr,">99") + ":" + string(li-over-tot-min,"99")  "," 
                 string(li-2time-tot-hr,">99") + ":" + string(li-2time-tot-min,"99")
                 SKIP.
           end.  
           assign li-lunch-emp = 0
                  li-emp-over = 0
                  li-emp-2time = 0.
           /*if lv-note-only then do:
              put "Notes:" skip .
              for each tt-note where tt-note.employee = emplogin.employee
                                  by tt-note.note_date:
                  put "     " tt-note.note_date " " tt-note.note_title skip.
              end.                       
              put skip(1).
           end.
           */
        end.           
      end.
      ELSE IF scr-plant-clock = 2 THEN RUN proc-2(INPUT tt-emp.emp).
      ELSE RUN proc-all (tt-emp.emp).

    END.  /*lv-labor-only*/

    IF LAST-OF(tt-emp.emp) THEN DO:
       if lv-note-only then do:
          put "Notes:" skip .
          for each tt-note where tt-note.employee = tt-emp.emp
                                  by tt-note.note_date:
                  put "     " tt-note.note_date " " tt-note.note_title skip.
          end.                       
          put skip(1).
       end.
    END.
  END. /*end tt-emp*/
  STATUS DEFAULT "".

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
     if lookup(lv-field-hdl:private-data,"save") > 0
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param1 C-Win 
PROCEDURE show-param1 :
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

  MESSAGE 
      num-entries(parm-fld-list,",") SKIP
      parm-fld-list
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  do i = 1 to num-entries(parm-fld-list,","):
    if entry(i,parm-fld-list) ne "" or
       entry(i,parm-lbl-list) ne "" then do:

        MESSAGE entry(i,parm-lbl-list)
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

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

