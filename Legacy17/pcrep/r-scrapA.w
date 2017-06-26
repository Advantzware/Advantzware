&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: pcrep\r-scrap.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
     /* Mod 01     Task  10091314    */ 

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

def TEMP-TABLE job-item NO-UNDO field i-no like job-hdr.i-no.
def stream s-temp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_jstat begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_date end_date tb_off tb_flex ~
tb_grav tb_let tb_silk rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lbl_jstat rd_jstat begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_date end_date tb_off tb_flex ~
tb_grav lbl_prt-type tb_let tb_silk rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

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

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-scrap.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_jstat AS CHARACTER FORMAT "X(256)":U INITIAL "Job Status?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_prt-type AS CHARACTER FORMAT "X(256)":U INITIAL "Printer Types?" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .95 NO-UNDO.

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
     SIZE 94 BY 9.05.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_flex AS LOGICAL INITIAL yes 
     LABEL "Flexo" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .71 NO-UNDO.

DEFINE VARIABLE tb_grav AS LOGICAL INITIAL yes 
     LABEL "Gravure" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .71 NO-UNDO.

DEFINE VARIABLE tb_let AS LOGICAL INITIAL yes 
     LABEL "Letterpress" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .71 NO-UNDO.

DEFINE VARIABLE tb_off AS LOGICAL INITIAL yes 
     LABEL "Offset" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .71 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_silk AS LOGICAL INITIAL yes 
     LABEL "Silkscreen" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .71 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     lbl_jstat AT ROW 2.91 COL 28 COLON-ALIGNED NO-LABEL
     rd_jstat AT ROW 2.91 COL 43 NO-LABEL
     begin_job-no AT ROW 4.33 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 4.33 COL 37 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 4.33 COL 67 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 4.33 COL 79 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_date AT ROW 5.52 COL 24 COLON-ALIGNED
     end_date AT ROW 5.52 COL 67 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     tb_off AT ROW 7.67 COL 39
     tb_flex AT ROW 7.67 COL 58
     tb_grav AT ROW 7.67 COL 71
     lbl_prt-type AT ROW 7.91 COL 18 COLON-ALIGNED NO-LABEL
     tb_let AT ROW 8.38 COL 39
     tb_silk AT ROW 8.38 COL 58
     rd-dest AT ROW 11.86 COL 6 NO-LABEL
     lv-ornt AT ROW 12.1 COL 31 NO-LABEL
     lines-per-page AT ROW 12.1 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 13.05 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 14 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.19 COL 31.4
     tb_excel AT ROW 16.48 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 16.48 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 17.29 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 19.81 COL 19
     btn-cancel AT ROW 19.81 COL 57
     "(Note: This is Start Date for Open Jobs and Closed Date for Closed Jobs)" VIEW-AS TEXT
          SIZE 78 BY .62 AT ROW 6.48 COL 14 WIDGET-ID 2
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.91 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     RECT-6 AT ROW 10.52 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.6 BY 21.57.


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
         TITLE              = "Scrap Report"
         HEIGHT             = 21.86
         WIDTH              = 96.2
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
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


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

/* SETTINGS FOR FILL-IN lbl_prt-type IN FRAME FRAME-A
   NO-ENABLE                                                            */
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

ASSIGN 
       tb_flex:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_grav:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_let:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_off:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_silk:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Scrap Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Scrap Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
  assign rd-dest.
 DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.     

  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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


&Scoped-define SELF-NAME tb_flex
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_flex C-Win
ON VALUE-CHANGED OF tb_flex IN FRAME FRAME-A /* Flexo */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_grav
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_grav C-Win
ON VALUE-CHANGED OF tb_grav IN FRAME FRAME-A /* Gravure */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_let
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_let C-Win
ON VALUE-CHANGED OF tb_let IN FRAME FRAME-A /* Letterpress */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_off
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_off C-Win
ON VALUE-CHANGED OF tb_off IN FRAME FRAME-A /* Offset */
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


&Scoped-define SELF-NAME tb_silk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_silk C-Win
ON VALUE-CHANGED OF tb_silk IN FRAME FRAME-A /* Silkscreen */
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
/* Mod 01     Task  10091314    */ 
     DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
        {custom/usrprint.i}
        APPLY "entry" TO begin_job-no .
     END.    /* Mod 01     Task  10091314    */ 

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
          begin_date end_date tb_off tb_flex tb_grav lbl_prt-type tb_let tb_silk 
          rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 
          tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 rd_jstat begin_job-no begin_job-no2 end_job-no 
         end_job-no2 begin_date end_date tb_off tb_flex tb_grav tb_let tb_silk 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel 
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
/*      DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.      */
/*                                                         */
/*      if init-dir = "" then init-dir = "c:\temp" .       */
/*      SYSTEM-DIALOG GET-FILE list-name                   */
/*          TITLE      "Enter Listing Name to SAVE AS ..." */
/*          FILTERS    "Listing Files (*.rpt)" "*.rpt",    */
/*                     "All Files (*.*)" "*.*"             */
/*          INITIAL-DIR init-dir                           */
/*          ASK-OVERWRITE                                  */
/*          SAVE-AS                                        */
/*          USE-FILENAME                                   */
/*                                                         */
/*          UPDATE OKpressed.                              */
/*                                                         */
/*      IF NOT OKpressed THEN  RETURN NO-APPLY.            */

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
/* ------------------------------------------------- pc/rep/scrap.p 04/00 JLF */
/* Scrap Report                                                               */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

def buffer bjob-mch for job-mch.

def var v-fjob  like mch-act.job-no NO-UNDO.
def var v-tjob  like v-fjob  init "zzzzzz" NO-UNDO.
def var v-fjob2 like mch-act.job-no2 NO-UNDO.
def var v-tjob2 like v-fjob2            init 99 NO-UNDO.
def var v-fdate like job.start-date     init 01/01/0001     format "99/99/9999" NO-UNDO.
def var v-tdate like v-fdate            init 12/31/9999 NO-UNDO.
def var v-stat  as   char format "!"    init "O" NO-UNDO.
def var v-type  as   char format "x(5)" init "OLFSG" NO-UNDO.

def var str-tit4    as   CHAR NO-UNDO.
def var v-pr-list   as   CHAR NO-UNDO.
def var v-est-type  like est.est-type NO-UNDO.
def var v-up        as   INT NO-UNDO.
def var v-on        as   INT NO-UNDO.
def var v-out       as   INT NO-UNDO.
def var v-dept      like job-mch.dept NO-UNDO.
DEF VAR v-qty       AS DEC NO-UNDO.

def var v-sheet     as   int format ">9" NO-UNDO.
def var v-blank     as   dec format ">9.999" NO-UNDO.
def var v-sheets    as   int format "->,>>>,>>9" extent 4 NO-UNDO.
def var v-blanks    as   int format "->,>>>,>>9" extent 4 NO-UNDO.
def var str_buffa   as   char no-undo.
def var v-hdr       as   CHAR NO-UNDO initial
"Job#,Customer,Machine,#ofLayout,Avg#upontheJob,PlanSheets,SheetsIssuedtoPressbySheeter,
SheetsIssuedtoCuttingbyPress,SheetsCutbyCuttingIssuedtoFinishing,PlanWasteforCartons,
CartonsProducedbyJob,FGReceived,CustomerOrderQty".
def var v-comma as char format "x" initial "," no-undo.

SESSION:SET-WAIT-STATE ("general").   

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112} .

   DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
  v-stat        = SUBSTR(rd_jstat,1,1)

  v-fjob        = fill(" ",6 - length(trim(begin_job-no))) +
                  trim(begin_job-no) + string(int(begin_job-no2),"99")
  v-tjob        = fill(" ",6 - length(trim(end_job-no)))   +
                  trim(end_job-no)   + string(int(end_job-no2),"99")

  v-fdate       = DATE(begin_date:SCREEN-VALUE)
  v-tdate       = date(END_date:SCREEN-VALUE)
  v-type        = TRIM(string(tb_off,"O/"))   + TRIM(string(tb_flex,"F/"))   +
                  TRIM(string(tb_let,"L/"))   + TRIM(string(tb_silk,"S/"))   +
                  TRIM(string(tb_grav,"G/")). 
 END.

for each mach FIELDS(m-code)
    where mach.company               eq cocode
      and index(v-type,mach.pr-type) gt 0
    no-lock
    by mach.m-code:
  v-pr-list = v-pr-list + trim(mach.m-code) + "," .   
end.
if v-pr-list ne ""                                    and
   substr(v-pr-list,length(trim(v-pr-list)),1) eq "," then
  substr(v-pr-list,length(trim(v-pr-list)),1) = "".

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.

  if tb_excel then
  do:
    output stream s-temp TO VALUE(fi_file).
    assign str_buffa = "".
    {sys/inc/outstrPL.i v-hdr 1 218}.
    PUT STREAM s-temp UNFORMATTED str_buffa SKIP.
  end.

  if v-pr-list ne "" then
  for each job
      where job.company            eq cocode
        and job.job-no             ge substr(v-fjob,1,6)
        and job.job-no             le substr(v-tjob,1,6)
        and fill(" ",6 - length(trim(job.job-no))) +
            trim(job.job-no) +  string(job.job-no2,"99")
                                   ge v-fjob
        and fill(" ",6 - length(trim(job.job-no))) +
            trim(job.job-no) +  string(job.job-no2,"99")
                                   le v-tjob
        and (v-stat                eq "A"                   or
             (v-stat               eq "O" and job.opened)   or
             (v-stat               eq "C" and NOT job.opened))
        and ((job.close-date       ge v-fdate and
              job.close-date       le v-tdate and
              NOT job.opened)                       or
             (job.start-date       ge v-fdate and
              job.start-date       le v-tdate and
              job.opened))
      use-index job-no no-lock,

      first est where est.company EQ job.company
                  AND est.est-no  EQ job.est-no
                no-lock,

      each job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job     eq job.job
        and job-hdr.job-no  eq job.job-no
        and job-hdr.job-no2 eq job.job-no2
      no-lock,

      first ef
      where ef.company   EQ est.company
        AND ef.est-no    EQ est.est-no
        and ef.form-no   EQ job-hdr.frm
      no-lock,

      first eb
      where eb.company    EQ est.company
        AND eb.est-no     EQ est.est-no
        and eb.form-no    EQ ef.form-no
        and (eb.blank-no  EQ job-hdr.blank-no or job-hdr.blank-no eq 0)
      no-lock,

      first bjob-mch
      where bjob-mch.company eq cocode
        and bjob-mch.job     eq job-hdr.job
        and bjob-mch.job-no  eq job-hdr.job-no
        and bjob-mch.job-no2 eq job-hdr.job-no2
        and bjob-mch.frm     eq job-hdr.frm
        and lookup(bjob-mch.m-code,v-pr-list) gt 0
      no-lock  

      break by job.job-no
            by job.job-no2
/*          by bjob-mch.m-code */
            by job-hdr.frm
            by job-hdr.i-no:

    v-est-type = est.est-type - (if est.est-type gt 4 then 4 else 0).        

    if first-of(job-hdr.frm) then do:
      v-up = 0.
      for each job-mat
          where job-mat.company eq cocode
            and job-mat.job     eq job-hdr.job
            and job-mat.job-no  eq job-hdr.job-no
            and job-mat.job-no2 eq job-hdr.job-no2
            and job-mat.frm     eq job-hdr.frm
          use-index seq-idx no-lock,

          first item
          where item.company  eq cocode
            and item.i-no     eq job-mat.i-no
            and lookup(item.mat-type,"1,2,3,4,P,B") > 0 
          NO-LOCK:
          FOR EACH mat-act
              WHERE mat-act.company eq cocode
                AND mat-act.job EQ job-mat.job
                AND mat-act.job-no EQ job-mat.job-no
                AND mat-act.job-no2 EQ job-mat.job-no2
                AND mat-act.s-num EQ job-mat.frm
                AND mat-act.b-num   eq job-mat.blank-no
                AND mat-act.i-no    eq job-mat.i-no
              use-index job no-lock:
                run sys/ref/convquom.p(job-mat.qty-uom, "EA",
                               job-mat.basis-w, job-mat.len,
                               job-mat.wid, item.s-dep,
                               mat-act.qty, output v-qty).

                v-sheets[1] = v-sheets[1] + v-qty.


          END.
          ASSIGN v-up = v-up + job-mat.n-up.

      END.
      IF v-up EQ 0 THEN
          v-up = 1.
/*       ELSE DO:                                      */
/*             IF v-up > 0 THEN DO:                    */
/*               v-sheet = v-sheet / v-up .            */
/*           END.                                      */
/*       END.       /* Mod 01     Task  10091314    */ */

      ASSIGN
/*        v-up    = v-up * v-out */
       v-sheet = v-sheet + 1
       v-blank = v-blank + v-up.

        for each mch-act
          where mch-act.company eq cocode
            and mch-act.job     eq job-hdr.job
            and mch-act.job-no  eq job-hdr.job-no
            and mch-act.job-no2 eq job-hdr.job-no2
            and mch-act.frm     eq job-hdr.frm
          no-lock:

        i = lookup(mch-act.dept,"RS,PR,DC,RC") + 1.
        IF i = 5 THEN i = 2.
        if i gt 1 then v-sheets[i] = v-sheets[i] +
                                     (IF mch-act.qty EQ ? THEN 0 ELSE (mch-act.qty /*/ v-out*/)).
      end.      
    end.

/*     v-blanks[1] = v-blanks[1] - job-hdr.qty. */

    find last job-mch
        where job-mch.company  eq cocode
          and job-mch.job      eq job-hdr.job
          and job-mch.job-no   eq job-hdr.job-no
          and job-mch.job-no2  eq job-hdr.job-no2
          and job-mch.frm      eq job-hdr.frm
          and job-mch.blank-no eq job-hdr.blank-no
        use-index line-idx no-lock no-error.

    if not avail job-mch then    
    find last job-mch
        where job-mch.company  eq cocode
          and job-mch.job      eq job-hdr.job
          and job-mch.job-no   eq job-hdr.job-no
          and job-mch.job-no2  eq job-hdr.job-no2
          and job-mch.frm      eq job-hdr.frm
        use-index line-idx no-lock no-error.

    if avail job-mch then do:
                  find first mach
          where mach.company eq cocode
            and mach.m-code  eq job-mch.m-code
          no-lock no-error.

      if avail mach then do:
        assign
         v-on  = 1
         v-out = 1.

        if mach.p-type ne "A" then do:
          find first est-op
              where est-op.company    EQ est.company
                AND est-op.est-no     EQ est.est-no 
                and est-op.s-num      EQ job-mch.frm
                and est-op.b-num      EQ job-mch.blank-no
                and est-op.m-code     EQ job-mch.m-code
                and est-op.op-pass    EQ job-mch.pass
                and est-op.dept       EQ job-mch.dept
                and est-op.line       LT 500
              no-lock no-error.

          if ((avail est-op) and est-op.op-sb)           or
             ((not avail est-op) and mach.p-type ne "B") then do:

            if avail est-op then
              run sys/inc/numout.p (recid(est-op), output v-out).

            v-on = eb.num-up * v-out.
          end.
        end.

        for each mch-act
            where mch-act.company eq cocode
              and mch-act.job     eq job-hdr.job
              and mch-act.job-no  eq job-hdr.job-no
              and mch-act.job-no2 eq job-hdr.job-no2
              and mch-act.frm     eq job-hdr.frm
              and mch-act.m-code  eq job-mch.m-code
              and mch-act.pass    eq job-mch.pass
            no-lock:
          v-blanks[2] = v-blanks[2] + (mch-act.qty * v-on).
        end.
      end.
    end.

    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq job-hdr.ord-no
          and oe-ordl.i-no    eq job-hdr.i-no
        no-lock no-error.
    if not avail oe-ordl then v-blanks[4] = v-blanks[4] + job-hdr.qty.

    find first job-item where job-item.i-no eq job-hdr.i-no no-error.
    if not avail job-item then do:
      create job-item.
      job-item.i-no = job-hdr.i-no.

      for each fg-act
          where fg-act.company eq cocode
            and fg-act.job     eq job.job
            and fg-act.job-no  eq job.job-no
            and fg-act.job-no2 eq job.job-no2
            and fg-act.i-no    eq job-hdr.i-no
          no-lock:
        v-blanks[3] = v-blanks[3] + fg-act.qty.
      end.

      if avail oe-ordl then v-blanks[4] = v-blanks[4] + oe-ordl.qty.
    end.

    if last-of(job.job-no2) then do:
      v-blank     = v-blank / v-sheet .
      v-blanks[1] = v-sheets[1] * v-blank - v-blanks[3].
/*       v-blanks[1] = v-sheets[3] - (v-blanks[3] / v-sheet) .  /* Mod 01     Task  10091314    */ */


      display trim(job.job-no) + "-" + string(job.job-no2,"99") format "x(9)"
                    column-label "Job#"
              job-hdr.cust-no
                    column-label "Customer"
              bjob-mch.m-code  
                    column-label "Machine"
              v-sheet         
                    column-label "# FG/!Job"
              v-blank
                    column-label "Avg # up!on the Job"
              v-sheets[1]
                    column-label "Issued!Sheets"
              v-sheets[2]
                    column-label "Sheets!Issued to!Press by!Sheeter"
              v-sheets[3]
                    column-label "Sheets!Issued to!Cutting by!Press"
              v-sheets[4]
                    column-label "Sheets Cut!by Cutting!Issued to!Finishing"
              v-blanks[1]
                    column-label "Plan!Waste for!Cartons"
              v-blanks[2]
                    column-label "Cartons!Produced!by Job"
              v-blanks[3]
                    column-label "FG!Received"
              v-blanks[4]
                    column-label "Customer!Order Qty"
              skip(1)

          with frame scrap no-attr-space no-box down STREAM-IO width 132.

      if tb_excel then
      do:
        assign str_buffa = "".
        assign str_buffa = trim(job.job-no) + "-" + string(job.job-no2,"99") + v-comma   /* Mod 01     Task  10091314    */ 
                         + trim(job-hdr.cust-no)                             + v-comma
                         + trim(bjob-mch.m-code)                             + v-comma
                         + trim(string(v-sheet,'->9'))                        + v-comma 
                         + trim(string(v-blank,'->9.999'))                    + v-comma
                         + trim(string(v-sheets[1],'->>>>>>>9'))            + v-comma
                         + trim(string(v-sheets[2],'->>>>>>>9'))            + v-comma
                         + trim(string(v-sheets[3],'->>>>>>>9'))            + v-comma
                         + trim(string(v-sheets[4],'->>>>>>>9'))            + v-comma
                         + trim(string(v-blanks[1],'->>>>>>>9'))            + v-comma
                         + trim(string(v-blanks[2],'->>>>>>>9'))            + v-comma
                         + trim(string(v-blanks[3],'->>>>>>>9'))            + v-comma
                         + trim(string(v-blanks[4],'->>>>>>>9')).      
        PUT STREAM s-temp UNFORMATTED str_buffa SKIP.
      end.

      assign
       v-sheet  = 0
       v-blank  = 0
       v-sheets = 0
       v-blanks = 0.

      EMPTY TEMP-TABLE job-item.  
    end.
  end.


  IF tb_excel THEN DO:
    OUTPUT STREAM s-temp CLOSE.
    IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
  END.

  SESSION:SET-WAIT-STATE("").

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).   /* Mod 01     Task  10091314    */ 

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
  def var lv-label as cha NO-UNDO.

  ASSIGN
  lv-frame-hdl = frame {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:first-child
  lv-field-hdl = lv-group-hdl:first-child.

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

