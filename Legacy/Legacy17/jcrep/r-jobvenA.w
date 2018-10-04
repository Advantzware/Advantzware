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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF STREAM excel.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

{jc/rep/job-sum.i new}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_jstat begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_cust end_cust begin_date ~
end_date rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lbl_jstat rd_jstat begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_cust end_cust begin_date ~
end_date rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-jobven.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_jstat AS CHARACTER FORMAT "X(256)":U INITIAL "Job Status?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .95 NO-UNDO.

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
"To E-Mail", 4
     SIZE 23 BY 4.76 NO-UNDO.

DEFINE VARIABLE rd_jstat AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All", "All"
     SIZE 29 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 8.57.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 8.57.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     lbl_jstat AT ROW 2.91 COL 28 COLON-ALIGNED NO-LABEL
     rd_jstat AT ROW 2.91 COL 43 NO-LABEL
     begin_job-no AT ROW 5.05 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 5.05 COL 36 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 5.05 COL 67 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 5.05 COL 79 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_cust AT ROW 6 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Customer"
     end_cust AT ROW 6 COL 67 COLON-ALIGNED HELP
          "Enter Ending Customer"
     begin_date AT ROW 6.95 COL 24 COLON-ALIGNED
     end_date AT ROW 6.95 COL 67 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     rd-dest AT ROW 11.24 COL 7 NO-LABEL
     lv-ornt AT ROW 11.48 COL 31 NO-LABEL
     lines-per-page AT ROW 11.48 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 12.91 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 13.86 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.05 COL 30.2
     tb_excel AT ROW 16.14 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 16.14 COL 71 RIGHT-ALIGNED
     fi_file AT ROW 16.95 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 18.86 COL 19
     btn-cancel AT ROW 18.86 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.29 COL 4
     RECT-6 AT ROW 10.05 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 20.33.


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
         TITLE              = "Job/Vendor Analysis Report"
         HEIGHT             = 19.43
         WIDTH              = 95.6
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
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_jstat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_jstat".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
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
ON END-ERROR OF C-Win /* Job/Vendor Analysis Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Job/Vendor Analysis Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       WHEN 4 THEN
       DO:
          {custom/asimailr.i &TYPE = ''
                             &begin_cust= begin_job-no
                             &END_cust=end_job-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
       END.
  end case.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_jstat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_jstat C-Win
ON VALUE-CHANGED OF rd_jstat IN FRAME FRAME-A
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
    assign {&self-name}.
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

  ASSIGN
      begin_date  = DATE (1,1,YEAR(TODAY))
      END_date    = DATE (12,31,year(TODAY)).

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_job-no.
  END.

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
  DISPLAY lbl_jstat rd_jstat begin_job-no begin_job-no2 end_job-no end_job-no2 
          begin_cust end_cust begin_date end_date rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 rd_jstat begin_job-no begin_job-no2 end_job-no 
         end_job-no2 begin_cust end_cust begin_date end_date rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
  */
  RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ----------------------------------------------- jc/rep/jobvend.p 12/98 JLF */
/* Job/Vendor Analysis                                                        */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

def buffer b-jh for job-hdr.

def var v-stat  as   char format "!"          init "O".
def var v-fjob  like job.job-no.
def var v-tjob  like v-fjob                   init "zzzzzz".
def var v-fjob2 like job.job-no2.
def var v-tjob2 like v-fjob2                  init 99.
def var v-fcust like job-hdr.cust-no          init "".
def var v-tcust like v-fcust                  init "zzzzzzzz".
def var v-fdate as   date format "99/99/9999" init 01/01/0001.
def var v-tdate like v-fdate                  init 12/31/9999.

def var v-up     like eb.num-up.
def var v-on     like v-up.

def var v-job     as   char.
def var v-frst    as   log.
def var v-qty     as   dec.
def var v-mat-qty like rm-rdtlh.qty format ">>>>>>>9".
def var v-inv-qty like rm-rdtlh.qty.
def var v-fg-qty  as   dec.
def var v-cost    as   dec.
def var v-diff    like rm-rdtlh.qty.
DEF VAR excelheader AS CHAR NO-UNDO.

form header skip(1)
            " P&P QTY"              to 105
            "   VEND $"             to 125
            "$ PER"                 to 131
            "    JOB #"             to 9
            "FG ITEM"               at 11
            "QUANTITY"              to 34
            "   PRICE"              to 45
            "UOM"                   at 47
            "VENDOR NAME"           at 51
            "INVOICE"               at 67
            " INV QTY"              to 87
            " REC QTY"              to 96
            "PRODUCED"              to 105
            "    DIFF"              to 114
            "W/O SETUP"             to 125
            "SHEET"                 to 131
            fill("-",131)           format "x(131)"
         with frame r-top.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

  v-stat        = SUBSTR(rd_jstat,1,1)

  v-fjob        = fill(" ",6 - length(trim(begin_job-no))) +
                  trim(begin_job-no) + string(int(begin_job-no2),"99")
  v-tjob        = fill(" ",6 - length(trim(end_job-no)))   +
                  trim(end_job-no)   + string(int(end_job-no2),"99")  

  v-fcust       = begin_cust
  v-tcust       = END_cust
  v-fdate       = begin_date
  v-tdate       = END_date. 

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "CUST #,CUST NAME,JOB #,FG ITEM,QUANTITY,PRICE,UOM,VENDOR NAME,INVOICE,"
              + "INV QTY,REC QTY,P & P QTY PRODUCED,DIFF,VEND $ W/O SETUP,$ PER SHEET".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

display "" with frame r-top.

 {sa/sa-sls01.i}

    for each job-hdr
        where job-hdr.company eq cocode
          and job-hdr.cust-no ge v-fcust
          and job-hdr.cust-no le v-tcust
          and job-hdr.job-no  ge substr(v-fjob,1,6)
          and job-hdr.job-no  le substr(v-tjob,1,6)
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) + string(job-hdr.job-no2,"99")
                          ge v-fjob
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) + string(job-hdr.job-no2,"99")
                          le v-tjob
        use-index cust-idx no-lock,

        first job
        where job.company eq cocode
          and job.job     eq job-hdr.job
          and job.job-no  eq job-hdr.job-no
          and job.job-no2 eq job-hdr.job-no2
          and (v-stat     eq "A"                    or
               (v-stat    eq "O" and job.opened)    or
               (v-stat    eq "C" and NOT job.opened))
        use-index job no-lock,

        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq job-hdr.i-no
        no-lock:

      release oe-ordl.
      if job-hdr.ord-no ne 0 then
      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq job-hdr.ord-no
            and oe-ordl.i-no    eq job-hdr.i-no
          no-lock no-error.

      if avail oe-ordl and
         (oe-ordl.req-date lt v-fdate or
          oe-ordl.req-date gt v-tdate) then next.

      else
      if job.start-date + 60 lt v-fdate or
         job.start-date + 60 gt v-tdate then next.

      v-job = fill(" ",6 - length(trim(job.job-no))) +
              trim(job.job-no) + "-" + string(job.job-no2,"99").

      create report.
      assign
       report.term-id = v-term
       report.key-01  = "1"
       report.key-02  = job-hdr.cust-no
       report.key-03  = v-job
       report.key-04  = job-hdr.i-no
       report.rec-id  = recid(job-hdr).
    end.

    for each report
        where report.term-id eq v-term
          and report.key-01  eq "1",

        first job-hdr where recid(job-hdr) eq report.rec-id no-lock,

        first job
        where job.company eq cocode
          and job.job     eq job-hdr.job
          and job.job-no  eq job-hdr.job-no
          and job.job-no2 eq job-hdr.job-no2
        no-lock

        break by report.key-02
              by report.key-03
              by report.key-04:

      if first-of(report.key-02) then do:
        if first(report.key-02) then
        DO:
           put skip.
           IF tb_excel THEN
              PUT STREAM excel UNFORMATTED SKIP(1).
        END.
        find first cust
            where cust.company eq cocode
              and cust.cust-no eq job-hdr.cust-no
            no-lock no-error.
        put skip(1) "Customer:" space(1) job-hdr.cust-no.

        if avail cust then put space(1) cust.name.
        put skip(1).
      end.

      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq job-hdr.ord-no
            and oe-ordl.i-no    eq job-hdr.i-no
          no-lock no-error.

      display report.key-03         to 9    format "x(9)"
              job-hdr.i-no          at 11
              job-hdr.qty           to 34   format ">>>>>>>9"
              oe-ordl.price         to 45   format ">>>>>>9.99<<<<"
                                    when avail oe-ordl
              oe-ordl.pr-uom        at 47
                                    when avail oe-ordl

          with frame det STREAM-IO width 132 no-box no-labels down.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
            SKIP
            '"' IF first-of(report.key-02) THEN
                   job-hdr.cust-no ELSE ""                '",'
            '"' IF FIRST-OF(report.key-02) AND
                   AVAIL cust THEN cust.NAME ELSE ""      '",'
            '"' report.key-03                             '",'
            '"' job-hdr.i-no                              '",'
            '"' STRING(job-hdr.qty,">>>>>>>9")            '",'
            '"' IF AVAIL oe-ordl THEN
                   STRING(oe-ordl.price,">>>>>>9.99<<<<")
                ELSE ""                                   '",'
            '"' IF AVAIL oe-ordl THEN oe-ordl.pr-uom
                ELSE ""                                   '",'.

      v-on = 1.

      find est where est.company EQ job-hdr.company
               AND   est.est-no  EQ job-hdr.est-no
               no-lock no-error.

      if avail est then do:
        run sys/inc/numup.p (est.company, est.est-no, job-hdr.frm, output v-on).

        find first ef
            where ef.company   EQ est.company
              AND ef.est-no    EQ ef.est-no
              and ef.form-no eq job-hdr.frm
            no-lock no-error.

        IF AVAIL ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
      end.

      v-fg-qty = 0.

      for each fg-act
          where fg-act.company eq cocode
            and fg-act.job     eq job-hdr.job
            and fg-act.job-no  eq job-hdr.job-no
            and fg-act.job-no2 eq job-hdr.job-no2
            and fg-act.i-no    eq job-hdr.i-no
          no-lock:
        v-fg-qty = v-fg-qty + fg-act.qty.
      end.

      {sys/inc/roundup.i v-fg-qty}

      assign
       v-mat-qty = 0
       v-cost    = 0.

      for each rm-rdtlh
          where rm-rdtlh.company   eq cocode
            and rm-rdtlh.job-no    eq job-hdr.job-no
            and rm-rdtlh.job-no2   eq job-hdr.job-no2
            and rm-rdtlh.rita-code eq "I"
          use-index job no-lock,

          first rm-rcpth
          where rm-rcpth.r-no      eq rm-rdtlh.r-no
            and rm-rcpth.po-no     ne ""
            and rm-rcpth.rita-code eq "I"
          no-lock,

          first item
          where item.company  eq cocode
            and item.i-no     eq rm-rcpth.i-no
            and item.mat-type eq "B"
          no-lock

          break by rm-rcpth.po-no:

        v-mat-qty = v-mat-qty + rm-rdtlh.qty.

        if last-of(rm-rcpth.po-no) then do:
          do i = 1 to length(trim(rm-rcpth.po-no)):
            if substr(rm-rcpth.po-no,i,1) lt "0" or
               substr(rm-rcpth.po-no,i,1) gt "9" then do:
              i = 0.
              leave.
            end.
          end.

          release po-ord.
          release po-ordl.
          if i ne 0 then
          find first po-ord
              where po-ord.company eq cocode
                and po-ord.po-no   eq int(rm-rcpth.po-no)
              no-lock no-error.

          if avail po-ord then
          find first po-ordl
              where po-ordl.company eq cocode
                and po-ordl.po-no   eq po-ord.po-no
                and po-ordl.i-no    eq rm-rcpth.i-no
                and po-ordl.job-no  eq rm-rdtlh.job-no
                and po-ordl.job-no2 eq rm-rdtlh.job-no2
              use-index po-no no-lock no-error.

          if avail po-ordl then do:
            if item.cons-uom ne "EA" then
              run sys/ref/convquom.p(item.cons-uom, "EA", item.basis-w,
                                     po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                     v-mat-qty, output v-mat-qty).

            for each ap-inv
                where ap-inv.company eq cocode
                  and ap-inv.vend-no eq po-ord.vend-no
                  and ap-inv.po-no   eq po-ordl.po-no
                  and ap-inv.posted  eq yes
                use-index ap-inv no-lock,

                each ap-invl
                where ap-invl.i-no eq ap-inv.i-no
                  and ap-invl.line eq po-ordl.line
                no-lock:

              v-cost = v-cost + ap-invl.amt.

              create xreport.
              assign
               xreport.term-id = v-term
               xreport.key-01  = "2"
               xreport.key-02  = po-ord.vend-no
               xreport.key-03  = ap-inv.inv-no
               xreport.key-04  = po-ordl.i-no
               xreport.rec-id  = recid(ap-invl).
            end.

            for each ap-inv
                where ap-inv.company eq cocode
                  and ap-inv.vend-no eq po-ord.vend-no
                  and ap-inv.po-no   eq 0
                  and ap-inv.posted  eq yes
                use-index ap-inv no-lock,

                each ap-invl
                where ap-invl.i-no       eq ap-inv.i-no
                  and ap-invl.po-no      eq po-ordl.po-no
                  and {ap/invlline.i -1} eq po-ordl.line
                no-lock:

              v-cost = v-cost + ap-invl.amt.

              create xreport.
              assign
               xreport.term-id = v-term
               xreport.key-01  = "2"
               xreport.key-02  = po-ord.vend-no
               xreport.key-03  = ap-inv.inv-no
               xreport.key-04  = po-ordl.i-no
               xreport.rec-id  = recid(ap-invl).
            end.
          end.
        end.
      end.

      find first xreport
          where xreport.term-id eq v-term
            and xreport.key-01  eq "2"
          no-lock no-error.
      if not avail report then do:
        create xreport.
        assign
         xreport.term-id = v-term
         xreport.key-01  = "2".
      end.

      v-inv-qty = 0.

      for each xreport
          where xreport.term-id eq v-term
            and xreport.key-01  eq "2",

          first job-mat
          where job-mat.company eq cocode
            and job-mat.job     eq job-hdr.job
            and (job-mat.frm    eq job-hdr.frm or
                 (avail est and (est.est-type eq 2 or est.est-type eq 6)))
            and job-mat.i-no    eq xreport.key-04
          no-lock

          break by xreport.key-02
                by xreport.key-03:

        find ap-invl where recid(ap-invl) eq xreport.rec-id no-lock no-error.
        if avail ap-invl then do:
          find first ap-inv where ap-inv.i-no eq ap-invl.i-no no-lock.
          find first vend
              where vend.company eq cocode
                and vend.vend-no eq ap-inv.vend-no
              no-lock no-error.

          if ap-invl.cons-uom eq "EA" then
            v-qty = ap-invl.qty.
          else
            run sys/ref/convquom.p(ap-invl.cons-uom, "EA", job-mat.basis-w,
                                   job-mat.len, job-mat.wid, job-mat.dep,
                                   ap-invl.qty, output v-qty).

          display vend.name             at 51   format "x(15)"
                                        when avail vend
                                         and first-of(xreport.key-02)
                  ap-inv.inv-no         at 67
                  v-qty                 to 87   format ">>>>>>>9"

              with frame det.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 '"' IF first-of(xreport.key-02) AND
                        AVAIL vend THEN vend.NAME   
                        ELSE ""                      '",'
                 '"' ap-inv.inv-no                   '",'
                 '"' ( IF last(xreport.key-02) AND
                          not first(xreport.key-02) THEN
                          STRING(v-inv-qty,">>>>>>>9")
                       ELSE 
                          STRING(v-qty,">>>>>>>9")) '",'.

          v-inv-qty = v-inv-qty + v-qty.
        end.

        if last(xreport.key-02) then do:
          if not first(xreport.key-02) then do:
            down with frame det.
            display v-inv-qty @ v-qty with frame det.
          end.

          display v-mat-qty                     to 96
                  v-fg-qty                      to 105  format ">>>>>>>9"
                  v-mat-qty - (v-fg-qty / v-on) to 114  format "->>>>>>9"
                  v-cost                        to 125  format ">>>>>>9.99"
                  v-cost / (v-fg-qty / v-on)    to 131  format ">9.99"
                                                when v-fg-qty gt 0
                                                 and v-cost gt 0
              with frame det.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 '"' STRING(v-mat-qty,">>>>>>>9")                     '",'
                 '"' STRING(v-fg-qty,">>>>>>>9")                      '",'
                 '"' STRING(v-mat-qty - (v-fg-qty / v-on),"->>>>>>9") '",'
                 '"' STRING(v-cost,">>>>>>9.99")                      '",'
                 '"' IF v-fg-qty GT 0 AND v-cost GT 0 THEN
                        STRING(v-cost / (v-fg-qty / v-on),">9.99")
                     ELSE ""                                          '",'.
        end.

        down with frame det.

        delete xreport.
      end.

      if last-of(report.key-02) then
      DO:
         put skip(1).

         IF tb_excel THEN
            PUT STREAM excel UNFORMATTED SKIP(1).
      END.

      delete report.
    end.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

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

