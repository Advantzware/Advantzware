&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jcrep\r-wipbct.w

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

DEF TEMP-TABLE tt-report LIKE report.
DEF STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_inc-clsd clsd_date tb_detailed rd_mch-ind ~
rd_mch-rate begin_cat end_cat begin_job-no begin_job-no2 end_job-no ~
end_job-no2 rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok ~
btn-cancel tb_excel tb_runExcel fi_file tb_act-zero RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS tb_inc-clsd clsd_date tb_detailed ~
lbl_mch-ind rd_mch-ind lbl_mch-rate rd_mch-rate begin_cat end_cat ~
begin_job-no begin_job-no2 end_job-no end_job-no2 rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel ~
fi_file tb_act-zero 

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

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(5)" 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE clsd_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "As Of" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-wipbct.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_mch-ind AS CHARACTER FORMAT "X(256)":U INITIAL "Machine Industry?" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_mch-rate AS CHARACTER FORMAT "X(256)":U INITIAL "Machine Rate?" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

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
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE VARIABLE rd_mch-ind AS CHARACTER INITIAL "Both" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Folding", "Folding",
"Corrugated", "Corrugated",
"Both", "Both"
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE rd_mch-rate AS CHARACTER INITIAL "Total" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Direct", "Direct",
"Total", "Total"
     SIZE 29 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 8.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 10.95.

DEFINE VARIABLE tb_act-zero AS LOGICAL INITIAL no 
     LABEL "Actuals as 0 if Qty Rec. Within Underrun?" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .81 NO-UNDO.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_inc-clsd AS LOGICAL INITIAL no 
     LABEL "Include Closed Jobs?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

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
     tb_inc-clsd AT ROW 2.91 COL 17
     clsd_date AT ROW 2.91 COL 63 COLON-ALIGNED
     tb_detailed AT ROW 4.57 COL 54 RIGHT-ALIGNED
     lbl_mch-ind AT ROW 5.76 COL 18 COLON-ALIGNED NO-LABEL
     rd_mch-ind AT ROW 5.76 COL 39 NO-LABEL
     lbl_mch-rate AT ROW 6.71 COL 20 COLON-ALIGNED NO-LABEL
     rd_mch-rate AT ROW 6.71 COL 39 NO-LABEL
     begin_cat AT ROW 8.62 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Category"
     end_cat AT ROW 8.62 COL 65 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_job-no AT ROW 9.57 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 9.57 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 9.57 COL 65 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 9.57 COL 77 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     rd-dest AT ROW 13.62 COL 5 NO-LABEL
     lv-ornt AT ROW 13.86 COL 30 NO-LABEL
     lines-per-page AT ROW 13.86 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 14.81 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 15.95 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.19 COL 29
     btn-ok AT ROW 20.71 COL 18
     btn-cancel AT ROW 20.71 COL 56
     tb_excel AT ROW 18.14 COL 29
     tb_runExcel AT ROW 18.14 COL 71 RIGHT-ALIGNED
     fi_file AT ROW 19.1 COL 27 COLON-ALIGNED HELP
          "Enter File Name"
     tb_act-zero AT ROW 3.81 COL 17
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.67 COL 4
     RECT-6 AT ROW 12.43 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.57.


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
         TITLE              = "W.I.P. By Product Category"
         HEIGHT             = 21.81
         WIDTH              = 95.8
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
   Custom                                                               */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       clsd_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
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

/* SETTINGS FOR FILL-IN lbl_mch-ind IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_mch-ind:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_mch-ind".

/* SETTINGS FOR FILL-IN lbl_mch-rate IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_mch-rate:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_mch-rate".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_mch-ind:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_mch-rate:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_act-zero:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_detailed IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_inc-clsd:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* W.I.P. By Product Category */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* W.I.P. By Product Category */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cat C-Win
ON LEAVE OF begin_cat IN FRAME FRAME-A /* Beginning Category */
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
  STATUS DEFAULT "Processing Complete".

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.
  SESSION:SET-WAIT-STATE (""). 

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME clsd_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL clsd_date C-Win
ON LEAVE OF clsd_date IN FRAME FRAME-A /* Not Closed By */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cat C-Win
ON LEAVE OF end_cat IN FRAME FRAME-A /* Ending Category */
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


&Scoped-define SELF-NAME rd_mch-ind
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_mch-ind C-Win
ON VALUE-CHANGED OF rd_mch-ind IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_mch-rate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_mch-rate C-Win
ON VALUE-CHANGED OF rd_mch-rate IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
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

  clsd_date = TODAY.   

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO clsd_date.
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
  DISPLAY tb_inc-clsd clsd_date tb_detailed lbl_mch-ind rd_mch-ind lbl_mch-rate 
          rd_mch-rate begin_cat end_cat begin_job-no begin_job-no2 end_job-no 
          end_job-no2 rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file tb_act-zero 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE tb_inc-clsd clsd_date tb_detailed rd_mch-ind rd_mch-rate begin_cat 
         end_cat begin_job-no begin_job-no2 end_job-no end_job-no2 rd-dest 
         lv-ornt lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
         tb_excel tb_runExcel fi_file tb_act-zero RECT-6 RECT-7 
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
/* ----------------------------------------------- jc/rep/jc-summ.p 07/98 JLF */
/* Job Cost Summary tt-report                                                    */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}
def buffer b-jh for job-hdr.

def var v-clos    as   log format "Y/N"             init NO NO-UNDO.
def var v-date    as   date format "99/99/9999"     init TODAY NO-UNDO.
def var v-detl    as   log format "Detail/Summary"  init NO NO-UNDO.
def var v-indus   as   char format "!"              init "B" NO-UNDO.
def var v-d-lab   as   log format "Direct/Total"    init NO NO-UNDO.
def var v-fcat    like itemfg.procat NO-UNDO.
def var v-tcat    like v-fcat                       init "zzzzzz" NO-UNDO.
def var v-fjob    like job.job-no NO-UNDO.
def var v-tjob    like v-fjob                       init "zzzzzz" NO-UNDO.
def var v-fjob2   like job.job-no2 NO-UNDO.
def var v-tjob2   like v-fjob2                      init 99 NO-UNDO.

def var v-pct     as   DEC NO-UNDO.
def var v-qty     as   DEC NO-UNDO.
def var v-cost    as   DEC NO-UNDO.
def var v-rate    as   DEC NO-UNDO.
def var v-fg-qty  as   DEC FORMAT "->>>,>>>,>>9.99" no-undo.
DEF VAR v-rec-qty AS   DEC FORMAT "->,>>>,>>>,>>9" EXTENT 4 NO-UNDO.
DEF VAR v-order-qty AS DEC FORMAT "->,>>>,>>>,>>9" EXTENT 4 NO-UNDO.
def var v-t-lab   as   dec format "->,>>>,>>>,>>9.99" extent 4 NO-UNDO.
def var v-t-mat   like v-t-lab NO-UNDO.
def var v-t-msf   like v-t-lab format "->,>>9.99" NO-UNDO.
def var v-mattype like item.mat-type NO-UNDO.

DEF VAR ll-act-rate AS LOG NO-UNDO.
DEF VAR ld-tot-rate AS DEC NO-UNDO. 
DEF VAR ll-wip AS LOG NO-UNDO.
DEF VAR excelheader AS CHARACTER NO-UNDO.
DEF VAR lv-within-underrun AS LOG NO-UNDO.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

  v-clos    = tb_inc-clsd
  v-date    = clsd_date
  v-detl    = tb_detailed
  v-indus   = SUBSTR(rd_mch-ind,1,1)
  v-d-lab   = rd_mch-rate EQ "Direct"
  v-fcat    = begin_cat
  v-tcat    = END_cat
  v-fjob    = fill(" ",6 - length(trim(begin_job-no))) +
               trim(begin_job-no) + string(int(begin_job-no2),"99")
  v-tjob    = fill(" ",6 - length(trim(end_job-no)))   +
               trim(end_job-no)   + string(int(end_job-no2),"99")
  str-tit3 = "FOR JOBS AS OF " + string(v-date)          + "  " +
                "Category:" + trim(v-fcat) + " - " + trim(v-tcat)   + "  " +
                "Job #:"    + trim(v-fjob) + " - " + trim(v-tjob)   + "  " +
                "Using "    + trim(string(v-d-lab,"Direct/Total"))  +
                " Labor Rate"
     x = (132 - length(str-tit3)) / 2
     str-tit3 = fill(" ",x) + str-tit3.


{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN 
DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "PRODUCT CATEGORY,CUSTOMER,FG ITEM #,CUSTOMER PART #,JOB #,"
              + "ACT LABOR,ACT MAT'L,MSF,RCPT QTY,ORDER QTY".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

view frame r-top.

EMPTY TEMP-TABLE tt-report.

FOR EACH job
    WHERE job.opened EQ YES
      AND (job.stat                                 EQ "W" OR
           CAN-FIND(FIRST mat-act
                    WHERE mat-act.company EQ job.company
                      AND mat-act.job     EQ job.job
                      AND mat-act.job-no  EQ job.job-no
                      AND mat-act.job-no2 EQ job.job-no2)  OR
           CAN-FIND(FIRST mch-act
                    WHERE mch-act.company EQ job.company
                      AND mch-act.job     EQ job.job
                      AND mch-act.job-no  EQ job.job-no
                      AND mch-act.job-no2 EQ job.job-no2)  OR
           CAN-FIND(FIRST misc-act
                    WHERE misc-act.company EQ job.company
                      AND misc-act.job     EQ job.job
                      AND misc-act.job-no  EQ job.job-no
                      AND misc-act.job-no2 EQ job.job-no2))
      AND job.start-date LE v-date 
  {jc/rep/wipbycat.i "use-index opened"}

IF v-clos THEN
FOR EACH job
    WHERE job.opened     EQ NO
      AND job.close-date GT v-date
  {jc/rep/wipbycat.i "use-index opened"}

for each tt-report,
    first job-hdr where recid(job-hdr) eq tt-report.rec-id no-lock,
    first itemfg
    where itemfg.company eq job-hdr.company
      and itemfg.i-no    eq job-hdr.i-no
    no-lock

    break by tt-report.key-01
          by tt-report.key-02
          by tt-report.key-03

    transaction:

    {custom/statusMsg.i " 'Processing Job#  '  + tt-report.key-05 "}

  assign
   v-pct  = 1
   ll-wip = no.

  find est where est.company EQ job-hdr.company
             AND est.est-no  EQ job-hdr.est-no
           no-lock no-error.

  if avail est and est.est-type eq 3 then do:
    v-qty = 0.

    for each b-jh
        where b-jh.job     eq job-hdr.job
          and b-jh.job-no  eq job-hdr.job-no
          and b-jh.job-no2 eq job-hdr.job-no2
        no-lock:

      v-qty = v-qty + b-jh.qty.
    end.

    v-pct = job-hdr.qty / v-qty.
  end.

  if not avail est or est.est-type eq 4 or est.est-type eq 8 then
    v-pct = job-hdr.sq-in / 100.

  assign
   v-t-lab[1] = 0
   v-t-mat[1] = 0
   v-t-msf[1] = 0
   lv-within-underrun = NO
   v-order-qty[1] = 0.

  for each mch-act
      where mch-act.company   eq job-hdr.company
        and mch-act.job       eq job-hdr.job
        and mch-act.job-no    eq job-hdr.job-no
        and mch-act.job-no2   eq job-hdr.job-no2
        and mch-act.op-date   le v-date
        and (mch-act.frm      eq job-hdr.frm or
             tt-report.key-06 eq "SET")
        and (mch-act.blank-no eq job-hdr.blank-no or
             mch-act.blank-no eq 0)
      use-index job no-lock,

      first mach
      where mach.company eq mch-act.company
        and mach.m-code  eq mch-act.m-code
      no-lock,

      first job-code where job-code.code eq mch-act.code no-lock:

    RUN jc/getactrt.p (ROWID(mch-act), OUTPUT ll-act-rate, OUTPUT ld-tot-rate).

    if job-code.cat eq "RUN" or
       job-code.cat eq "DT"  then
      v-rate = (IF ll-act-rate THEN ld-tot-rate
                ELSE mach.run-rate) +
               if v-d-lab then 0 else
               (mach.run-varoh + mach.run-fixoh).
    else
    if job-code.cat eq "MR"  then
      v-rate = (IF ll-act-rate THEN ld-tot-rate
                ELSE mach.mr-rate) +
               if v-d-lab then 0 else
               (mach.mr-varoh + mach.mr-fixoh).
    else v-rate = 0.

    v-t-lab[1] = v-t-lab[1] + (mch-act.hours * v-rate *
                               if mch-act.blank-no eq 0 then v-pct else 1).
  end.

  for each mat-act
      where mat-act.company   eq job-hdr.company
        and mat-act.job       eq job-hdr.job
        and mat-act.job-no    eq job-hdr.job-no
        and mat-act.job-no2   eq job-hdr.job-no2
        and mat-act.mat-date  le v-date
        and (mat-act.s-num    eq job-hdr.frm or
             tt-report.key-06 eq "SET")
      use-index job no-lock:

    ll-wip = YES.

    find first item
        where item.company  eq mat-act.company
          and item.i-no     eq mat-act.i-no
        no-lock no-error.
    v-mattype = if avail item then item.mat-type else "".

    find first job-mat
        where job-mat.company  eq mat-act.company
          and job-mat.job      eq mat-act.job
          and job-mat.frm      eq mat-act.s-num
          and job-mat.blank-no eq mat-act.b-num
          and job-mat.i-no     eq mat-act.i-no
        use-index seq-idx no-lock no-error.

    if not avail job-mat then
    for each job-mat
        where job-mat.company  eq mat-act.company
          and job-mat.job      eq mat-act.job
          and job-mat.frm      eq mat-act.s-num
          and job-mat.blank-no eq mat-act.b-num
        use-index seq-idx no-lock,

        first item
        where item.company  eq job-mat.company
          and item.i-no     eq job-mat.i-no
          and item.mat-type eq v-mattype
        no-lock:
      leave.
    end.

    if not avail job-mat then next.

    if job-mat.qty-uom eq "EA" then
      v-qty = mat-act.qty.
    else
      run sys/ref/convquom.p(job-mat.qty-uom, "EA", job-mat.basis-w,
                             job-mat.len, job-mat.wid, job-mat.dep,
                             mat-act.qty, output v-qty).

    if job-mat.sc-uom eq "EA" then
      v-cost = mat-act.cost.
    else
      run sys/ref/convcuom.p(job-mat.sc-uom, "EA", job-mat.basis-w,
                             job-mat.len, job-mat.wid, job-mat.dep,
                             mat-act.cost, output v-cost).

    if v-cost eq ? then v-cost = 0.

    IF mat-act.ext-cost EQ 0 OR mat-act.ext-cost EQ ? THEN
      v-t-mat[1] = v-t-mat[1] + (v-qty * v-cost * v-pct).
    ELSE
      v-t-mat[1] = v-t-mat[1] + (mat-act.ext-cost * v-pct).

    if job-mat.qty-uom eq "MSF" then
      v-qty = mat-act.qty.
    else  
      run sys/ref/convquom.p(job-mat.qty-uom, "MSF", job-mat.basis-w,
                             job-mat.len, job-mat.wid, job-mat.dep,
                             mat-act.qty, output v-qty).

    v-t-msf[1] = v-t-msf[1] + (v-qty * v-pct).
  end.

  for each misc-act
      where misc-act.company   eq job-hdr.company
        and misc-act.job       eq job-hdr.job
        and misc-act.job-no    eq job-hdr.job-no
        and misc-act.job-no2   eq job-hdr.job-no2
        and misc-act.misc-date le v-date
        and (misc-act.frm      eq job-hdr.frm or
             tt-report.key-06  eq "SET")
        and (misc-act.blank-no eq job-hdr.blank-no or
             misc-act.blank-no eq 0)
      no-lock:

    v-cost = misc-act.cost * if misc-act.blank-no eq 0 then v-pct else 1.

    if misc-act.ml then
      v-t-mat[1] = v-t-mat[1] + v-cost.
    else
      v-t-lab[1] = v-t-lab[1] + v-cost.
  end.

  ASSIGN
     v-fg-qty = 0
     v-rec-qty[1] = 0.

  FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
      WHERE fg-rcpth.company    EQ job-hdr.company
        AND fg-rcpth.job-no     EQ job-hdr.job-no
        AND fg-rcpth.job-no2    EQ job-hdr.job-no2
        AND fg-rcpth.i-no       EQ job-hdr.i-no
        AND fg-rcpth.rita-code  EQ "R"
        AND fg-rcpth.trans-date LE v-date
      USE-INDEX job,

      EACH fg-rdtlh FIELDS(qty) NO-LOCK
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:

    ASSIGN
       v-fg-qty = v-fg-qty + fg-rdtlh.qty
       v-rec-qty[1] = v-rec-qty[1] + fg-rdtlh.qty.
  END.

  FIND FIRST oe-ordl WHERE
       oe-ordl.company EQ job-hdr.company AND
       oe-ordl.ord-no EQ job-hdr.ord-no AND
       oe-ordl.i-no   EQ job-hdr.i-no
       NO-LOCK NO-ERROR.

  IF AVAIL oe-ordl THEN
  DO:
     v-order-qty[1] = oe-ordl.qty.

     IF tb_act-zero AND v-rec-qty[1] GE
        oe-ordl.qty - (oe-ordl.qty * (oe-ordl.under-pct / 100.0)) THEN
        lv-within-underrun = YES.

     RELEASE oe-ordl.
  END.

  FOR EACH b-jh NO-LOCK
      WHERE b-jh.company EQ job-hdr.company
        AND b-jh.job-no  EQ job-hdr.job-no
        AND b-jh.job-no2 EQ job-hdr.job-no2
        AND b-jh.i-no    EQ job-hdr.i-no:
    ACCUMULATE b-jh.qty (TOTAL).
  END.
  v-fg-qty = v-fg-qty * (job-hdr.qty / (ACCUM TOTAL b-jh.qty)).

  assign
     v-t-lab[1] = v-t-lab[1] - (v-fg-qty *
                                (job-hdr.std-lab-cost + if v-d-lab then 0
                                  else (job-hdr.std-var-cost +
                                       job-hdr.std-fix-cost)) / 1000)
     v-t-mat[1] = v-t-mat[1] - (v-fg-qty * job-hdr.std-mat-cost / 1000).

  v-t-msf[1] = v-t-msf[1] - (v-fg-qty * itemfg.t-sqft / 1000).

  if v-t-lab[1] lt 0 or v-t-lab[1] eq ? then v-t-lab[1] = 0.
  if v-t-mat[1] lt 0 or v-t-mat[1] eq ? then v-t-mat[1] = 0.
  if v-t-msf[1] lt 0 or v-t-msf[1] eq ? then v-t-msf[1] = 0.

  IF tb_act-zero AND lv-within-underrun THEN
     ASSIGN v-t-lab[1] = 0 v-t-mat[1] = 0.

  if v-detl and (v-t-lab[1] ne 0 or v-t-mat[1] ne 0 or v-t-msf[1] ne 0 OR
     v-rec-qty[1] NE 0 OR v-order-qty[1] NE 0 OR itemfg.q-ono NE 0) then DO:

    display tt-report.key-01    column-label "PROD!CAT"
                                format "x(5)"    
            tt-report.key-02    column-label "CUSTOMER"
                                format "x(8)"
            tt-report.key-03    column-label "FG ITEM #"
                                format "x(15)"
            tt-report.key-04    column-label "CUSTOMER PART #"
                                format "x(15)"
            tt-report.key-05    column-label "    JOB #"
                                format "x(9)"
            v-t-lab[1]          COLUMN-LABEL "ACT LABOR" 
            v-t-mat[1]          column-label "ACT MAT'L"
            v-t-msf[1]          column-label "MSF"
            v-rec-qty[1]        COLUMN-LABEL "RCPT QTY"
            v-order-qty[1]      COLUMN-LABEL "ORDER QTY"

        with frame det STREAM-IO width 132 no-box down.

    IF tb_excel THEN
      PUT STREAM excel UNFORMATTED
          '"' tt-report.key-01                       '",'
          '"' tt-report.key-02                       '",'
          '"' tt-report.key-03                       '",'
          '"' tt-report.key-04                       '",'
          '"' tt-report.key-05                       '",'
          '"' STRING(v-t-lab[1],"->,>>>,>>>,>>9.99") '",'
          '"' STRING(v-t-mat[1],"->,>>>,>>>,>>9.99") '",'
          '"' STRING(v-t-msf[1],"->,>>9.99") '",'
          '"' STRING(v-rec-qty[1],"->,>>>,>>>,>>9")  '",'
          '"' STRING(v-order-qty[1],"->,>>>,>>>,>>9") '",'
         SKIP.
  END.

  assign
   v-t-lab[2] = v-t-lab[2] + v-t-lab[1]
   v-t-mat[2] = v-t-mat[2] + v-t-mat[1]
   v-t-msf[2] = v-t-msf[2] + v-t-msf[1]
   v-rec-qty[2] = v-rec-qty[2] + v-rec-qty[1]
   v-order-qty[2] = v-order-qty[2] + v-order-qty[1].

  if last-of(tt-report.key-03) then do:
    IF v-t-lab[2] NE 0 OR v-t-mat[2] NE 0 OR v-t-msf[2] NE 0 OR
       v-rec-qty[2] NE 0 OR v-order-qty[2] NE 0 THEN DO:
      if v-detl then
        underline tt-report.key-04
                  v-t-lab[1]
                  v-t-mat[1]
                  v-t-msf[1]
                  v-rec-qty[1]
                  v-order-qty[1]
            with frame det.

      display tt-report.key-01    when not v-detl
              tt-report.key-02    when not v-detl
              tt-report.key-03    when not v-detl
              tt-report.key-04    when not v-detl
              "    Item Totals"   when v-detl
                                  @ tt-report.key-04
              "      ALL"         when not v-detl
                                  @ tt-report.key-05
              v-t-lab[2]          @ v-t-lab[1]
              v-t-mat[2]          @ v-t-mat[1]
              v-t-msf[2]          @ v-t-msf[1]
              v-rec-qty[2]        @ v-rec-qty[1]
              v-order-qty[2]      @ v-order-qty[1]
          with frame det STREAM-IO width 132 no-box down.

      if v-detl then put skip(1).    

      IF tb_excel THEN 
        PUT STREAM excel UNFORMATTED
             '"' (IF NOT v-detl THEN tt-report.key-01 ELSE "") '",'
             '"' (IF NOT v-detl THEN tt-report.key-02 ELSE "") '",'
             '"' (IF NOT v-detl THEN tt-report.key-03 ELSE "") '",'
             '"' (IF NOT v-detl THEN tt-report.key-04 ELSE "    Item Totals") '",'
             '"' (IF NOT v-detl THEN "      ALL" ELSE "")      '",'
             '"' STRING(v-t-lab[2],"->,>>>,>>>,>>9.99") '",'
             '"' STRING(v-t-mat[2],"->,>>>,>>>,>>9.99") '",'
             '"' STRING(v-t-msf[2],"->,>>9.99") '",'
             '"' STRING(v-rec-qty[2],"->,>>>,>>>,>>9") '",'
             '"' STRING(v-order-qty[2],"->,>>>,>>>,>>9") '",'
             SKIP.

      assign
       v-t-lab[3] = v-t-lab[3] + v-t-lab[2]
       v-t-mat[3] = v-t-mat[3] + v-t-mat[2]
       v-t-msf[3] = v-t-msf[3] + v-t-msf[2]
       v-rec-qty[3] = v-rec-qty[3] + v-rec-qty[2]
       v-order-qty[3] = v-order-qty[3] + v-order-qty[2]
       v-t-lab[2] = 0
       v-t-mat[2] = 0
       v-t-msf[2] = 0
       v-rec-qty[2] = 0
       v-order-qty[2] = 0.
    END.
  end.      

  if last-of(tt-report.key-01) then do:
    IF v-t-lab[3] NE 0 OR v-t-mat[3] NE 0 OR v-t-msf[3] NE 0 OR
       v-rec-qty[3] NE 0 OR v-order-qty[3] NE 0 THEN DO:
      underline tt-report.key-04
                v-t-lab[1]
                v-t-mat[1]
                v-t-msf[1]
                v-rec-qty[1]
                v-order-qty[1]
          with frame det.

      display "Category Totals" @ tt-report.key-04
              v-t-lab[3]        @ v-t-lab[1]
              v-t-mat[3]        @ v-t-mat[1]
              v-t-msf[3]        @ v-t-msf[1]
              v-rec-qty[3]      @ v-rec-qty[1]
              v-order-qty[3]    @ v-order-qty[1]
          with frame det.

      put skip(1).

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
             '"' "" '",'
             '"' "" '",'
             '"' "" '",'
             '"' "Category Totals" '",'
             '"' "" '",'
             '"' STRING(v-t-lab[3],"->,>>>,>>>,>>9.99") '",'
             '"' STRING(v-t-mat[3],"->,>>>,>>>,>>9.99") '",'
             '"' STRING(v-t-msf[3],"->,>>9.99") '",'
             '"' STRING(v-rec-qty[3],"->,>>>,>>>,>>9") '",'
             '"' STRING(v-order-qty[3],"->,>>>,>>>,>>9") '",'
            SKIP.

      assign
       v-t-lab[4] = v-t-lab[4] + v-t-lab[3]
       v-t-mat[4] = v-t-mat[4] + v-t-mat[3]
       v-t-msf[4] = v-t-msf[4] + v-t-msf[3]
       v-rec-qty[4] = v-rec-qty[4] + v-rec-qty[3]
       v-order-qty[4] = v-order-qty[4] + v-order-qty[3]
       v-t-lab[3] = 0
       v-t-mat[3] = 0
       v-t-msf[3] = 0
       v-rec-qty[3] = 0
       v-order-qty[3] = 0.
    END.
  end.

  if last(tt-report.key-01) then do:
    underline tt-report.key-04
              v-t-lab[1]
              v-t-mat[1]
              v-t-msf[1]
              v-rec-qty[1]
              v-order-qty[1]
        with frame det.

    display "   Grand Totals" @ tt-report.key-04
            v-t-lab[4]        @ v-t-lab[1]
            v-t-mat[4]        @ v-t-mat[1]
            v-t-msf[4]        @ v-t-msf[1]
            v-rec-qty[4]      @ v-rec-qty[1]
            v-order-qty[4]    @ v-order-qty[1]
        with frame det.

    put skip(1).

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
           '"' "" '",'
           '"' "" '",'
           '"' "" '",'
           '"' "   Grand Totals" '",'
           '"' "" '",'
           '"' STRING(v-t-lab[4],"->,>>>,>>>,>>9.99") '",'
           '"' STRING(v-t-mat[4],"->,>>>,>>>,>>9.99") '",'
           '"' STRING(v-t-msf[4],"->,>>9.99") '",'
           '"' STRING(v-rec-qty[4],"->,>>>,>>>,>>9") '",'
           '"' STRING(v-order-qty[4],"->,>>>,>>>,>>9") '",'
           SKIP.
  end.
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

