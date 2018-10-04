&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jcrep\r-wipaud.w

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

 def TEMP-TABLE work-aud NO-UNDO
   field tran-date as date
   field procat like item.procat
   field job-no like job-mat.job-no
   field job-no2 like job-mat.job-no2
   field frm like misc-act.frm
   field blank-no like misc-act.blank-no
   field i-no like mat-act.i-no
   field m-code like mach.m-code
   field dscr like item.i-dscr
   field qty as dec format ">>>>>>9.99-"
   field waste like mch-act.waste
   field code like mch-act.code
   field hours like mch-act.hours
   field complete like mch-act.complete.

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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_jstat begin_job-no ~
begin_job-no2 end_job-no end_job-no2 tb_wip rd-dest lv-ornt lines-per-page ~
lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lbl_jstat rd_jstat begin_job-no ~
begin_job-no2 end_job-no end_job-no2 tb_wip rd-dest lv-ornt lines-per-page ~
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

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-wipaud.csv" 
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
"To Email", 5
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
     SIZE 94 BY 8.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 9.52.

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

DEFINE VARIABLE tb_wip AS LOGICAL INITIAL no 
     LABEL "Show Only Open WIP?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     lbl_jstat AT ROW 2.67 COL 28 COLON-ALIGNED NO-LABEL
     rd_jstat AT ROW 2.67 COL 43 NO-LABEL
     begin_job-no AT ROW 4.33 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 4.33 COL 37 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 4.33 COL 67 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 4.33 COL 79 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     tb_wip AT ROW 6 COL 62 RIGHT-ALIGNED
     rd-dest AT ROW 12.43 COL 5 NO-LABEL
     lv-ornt AT ROW 12.67 COL 32 NO-LABEL
     lines-per-page AT ROW 12.67 COL 85 COLON-ALIGNED
     lv-font-no AT ROW 14.1 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 15.05 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.24 COL 31
     tb_excel AT ROW 17.19 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 17.19 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 18 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 19.57 COL 18
     btn-cancel AT ROW 19.57 COL 56
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.48 COL 4
     RECT-6 AT ROW 11 COL 1
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
         TITLE              = "W.I.P. Job Audit Trail"
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
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
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

/* SETTINGS FOR TOGGLE-BOX tb_wip IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_wip:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* W.I.P. Job Audit Trail */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* W.I.P. Job Audit Trail */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
       WHEN 5 THEN
       DO:
          DEF VAR lv-tmp AS CHAR INIT "-0" NO-UNDO.

          {custom/asimailr.i &TYPE="Customer"
                             &begin_cust=lv-tmp
                             &END_cust=lv-tmp
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
       END.

  end case. 

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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


&Scoped-define SELF-NAME tb_wip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_wip C-Win
ON VALUE-CHANGED OF tb_wip IN FRAME FRAME-A /* Show Only Open WIP? */
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
          tb_wip rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 rd_jstat begin_job-no begin_job-no2 end_job-no 
         end_job-no2 tb_wip rd-dest lv-ornt lines-per-page lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-job-totals-proc C-Win 
PROCEDURE excel-job-totals-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-text      AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-brd-job AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-mch-job AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-wst-job AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-hrs-job AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-fg-job  AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-v-oth-job AS DEC NO-UNDO.

   PUT STREAM excel UNFORMATTED
       SKIP(1).

   RUN excel-spaces-proc(INPUT 5).

   PUT STREAM excel UNFORMATTED
       '"' ip-text                           '",'.

   PUT STREAM excel UNFORMATTED
       '"' "BOARD TOTALS - "                 '",'
       '"' STRING(ip-v-brd-job,">>>>>>>>9-") '",'
       SKIP.

   RUN excel-spaces-proc(INPUT 6).

   PUT STREAM excel UNFORMATTED
       '"' "MACHINE TOTALS - "               '",'
       '"' STRING(ip-v-mch-job,">>>>>>>>9-") '",'
       '"' STRING(ip-v-wst-job,">>>>>>9-")   '",'
       '"' STRING(ip-v-hrs-job,">>>>9.99-")  '",'
       SKIP.

   RUN excel-spaces-proc(INPUT 6).

   PUT STREAM excel UNFORMATTED
       '"' "FINISHED GOODS TOTALS:"          '",'
       '"' STRING(ip-v-fg-job,">>>>>>>>9-")  '",'
       SKIP.

   RUN excel-spaces-proc(INPUT 6).

   PUT STREAM excel UNFORMATTED
       '"' "OTHER MATERIAL TOTALS:"          '",'
       '"' STRING(ip-v-oth-job,">>>>>>>>9-") '",'
       SKIP(1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-spaces-proc C-Win 
PROCEDURE excel-spaces-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-spaces AS INT NO-UNDO.

   DEF VAR viLoop AS INT NO-UNDO.

   DO viLoop = 1 TO ip-spaces:
      PUT STREAM excel UNFORMATTED
          '"' "" '",'.
   END.
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
/* ------------------------------------------------ jc/rep/wip-aud.p  8/94 gb */
/* WIP Job Audit Listing Report                                               */
/* ---------------------------------------------------------------------------*/

{sys/form/r-topw.f}

def var v-job-no like job.job-no extent 2 init ["","zzzzzz"] no-undo.
def var v-job-no2 like job.job-no2 extent 2 init [00,99] no-undo.
def var v-stat as char no-undo.
def var v-only-opn as logical format "Y/N" no-undo.
def var v-brd-job as int format ">>>>>>>>9-" no-undo.
def var v-brd-tot as int format ">>>>>>>>9-" no-undo.
def var v-oth-job as int format ">>>>>>>>9-" no-undo.
def var v-oth-tot as int format ">>>>>>>>9-" no-undo.
def var v-mch-job as int format ">>>>>>>>9-" no-undo.
def var v-mch-tot as int format ">>>>>>>>9-" no-undo.
def var v-fg-job as int format ">>>>>>>>9-" no-undo.
def var v-fg-tot as int format ">>>>>>>>9-" no-undo.
def var v-hrs-job as dec format ">>>>9.99-" no-undo.
def var v-hrs-tot as dec format ">>>>9.99-" no-undo.
def var v-wst-job as int format ">>>>>>9-" no-undo.
def var v-wst-tot as int format ">>>>>>9-" no-undo.


def var hdr-tit as char no-undo.
def var hdr-tit2 as char no-undo.
def var hdr-tit3 as char no-undo.
DEF VAR excelheader AS CHAR NO-UNDO.

FORM HEADER
     hdr-tit format "x(132)" skip
     hdr-tit2 format "x(132)" skip
     hdr-tit3 format "x(132)"

    WITH FRAME r-top.

form work-aud.procat
     work-aud.tran-date
     work-aud.job-no space(0) "-" space(0)
     work-aud.job-no2 format "99"
     work-aud.frm space(0) "/" space(0)
     work-aud.blank-no
     work-aud.i-no
     work-aud.dscr
     work-aud.qty
     with frame fr-mat down no-attr-space no-box no-labels STREAM-IO width 132.

form work-aud.procat
     work-aud.tran-date
     work-aud.job-no space(0) "-" space(0)
     work-aud.job-no2 format "99"
     work-aud.frm format ">9" space(0) "/" space(0)
     work-aud.blank-no
     "               "
     work-aud.dscr
     work-aud.qty
     work-aud.waste format ">>>>>>9-"
     work-aud.hours format ">>>>9.99-"
     work-aud.m-code
     work-aud.code
     work-aud.complete
     with frame fr-mch down no-attr-space no-box no-labels STREAM-IO width 132.

form work-aud.procat
     work-aud.tran-date
     work-aud.job-no space(0) "-" space(0)
     work-aud.job-no2 format "99"
     work-aud.frm space(0) "/" space(0)
     work-aud.blank-no
     work-aud.i-no
     work-aud.dscr
     work-aud.qty
     with frame fr-fg down no-attr-space no-box no-labels STREAM-IO width 132.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

  v-stat        = SUBSTR(rd_jstat,1,1)
  v-job-no[1]   = fill(" ",6 - length(trim(begin_job-no))) +
                  trim(begin_job-no) + string(int(begin_job-no2),"99")
  v-job-no[2]   = fill(" ",6 - length(trim(end_job-no)))   +
                  trim(end_job-no)   + string(int(end_job-no2),"99") 
  v-only-opn    = tb_wip

    /*
 v-job-no[1]    = begin_job-no
 v-job-no[2]    = end_job-no
 v-job-no2[1]   = begin_rel
 v-job-no2[2]   = end_rel
  */

       hdr-tit = "TRANS  TRANS      JOB                                      " +
             "                     QUANTITY     WASTE      MACH MACH   JOB     "
      hdr-tit2 = "TYPE    DATE      NUMBER  S/ B ITEM NUMBER     DESCRIPTION " +
             "                       POSTED       QTY     HOURS CODE   CODE  C ".
      hdr-tit3 = fill("-", 131).



{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Trans Type,Trans Date,Job Number,S,B,Item Number,"
              + "Description,Quantity Posted,Waste Qty,Mach Hours,"
              + "Mach Code,Job Code,C".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

display "" with frame r-top.


    for each job
          where job.company eq cocode
            and job.job-no  ge SUBSTR(v-job-no[1],1,6)
            and job.job-no  le SUBSTR(v-job-no[2],1,6)
            AND fill(" ",6 - length(trim(job.job-no))) +
                trim(job.job-no) + string(int(job.job-no2),"99") GE v-job-no[1] 
            AND fill(" ",6 - length(trim(job.job-no))) +
                trim(job.job-no) + string(int(job.job-no2),"99") LE v-job-no[2]
            and (v-stat  eq "A"  or
                 (v-stat eq "O" and job.opened) or
                 (v-stat eq "C" and not job.opened))
          no-lock:

         for each work-aud:
            delete work-aud.
         end.

         for each mch-act
             where mch-act.company eq cocode
               and mch-act.job     eq job.job
             use-index job no-lock:
            if v-only-opn and not mch-act.opn then next.

            find mach
                where mach.company eq cocode
                  and mach.loc     eq locode
                  and mach.m-code  eq mch-act.m-code
                no-lock no-error.
            if not available mach then next.

            create work-aud.
            assign work-aud.job-no = mch-act.job-no
                   work-aud.tran-date = mch-act.op-date
                   work-aud.procat = "HRS"
                   work-aud.job-no2 = mch-act.job-no2
                   work-aud.frm = mch-act.frm
                   work-aud.blank-no = mch-act.blank-no
                   work-aud.qty = mch-act.qty
                   work-aud.waste = mch-act.waste
                   work-aud.hours = mch-act.hours
                   work-aud.m-code = mch-act.m-code
                   work-aud.dscr = mach.m-dscr
                   work-aud.code = mch-act.code
                   work-aud.complete = mch-act.complete.
         end.

         for each mat-act
             where mat-act.company eq cocode
               and mat-act.job     eq job.job
             use-index job no-lock:
            if v-only-opn and not mat-act.opn then next. 

            find item
                where item.company eq cocode
                  and item.i-no    eq mat-act.i-no
                no-lock no-error.
            if not available item then next.

            create work-aud.
            assign work-aud.job-no = mat-act.job-no
                   work-aud.job-no2 = mat-act.job-no2
                   work-aud.procat = item.procat
                   work-aud.tran-date = mat-act.mat-date
                   work-aud.frm = mat-act.s-num
                   work-aud.blank-no = mat-act.b-num
                   work-aud.i-no = mat-act.i-no
                   work-aud.dscr = item.i-dscr
                   work-aud.qty = mat-act.qty.
         end.

         for each fg-act
             where fg-act.company eq cocode
               and fg-act.job     eq job.job
             use-index job-idx no-lock:
            if v-only-opn and not fg-act.opn then next.

            find itemfg
                where itemfg.company eq cocode
                  and itemfg.i-no    eq fg-act.i-no
                no-lock no-error.
            if not available itemfg then next.

            create work-aud.
            assign work-aud.job-no = fg-act.job-no
                   work-aud.job-no2 = fg-act.job-no2
                   work-aud.procat = "F.G."
                   work-aud.tran-date = fg-act.fg-date
                   work-aud.frm = fg-act.s-num
                   work-aud.blank-no = fg-act.b-num
                   work-aud.i-no = fg-act.i-no
                   work-aud.dscr = itemfg.i-name
                   work-aud.qty = fg-act.qty.
         end.

         for each misc-act
             where misc-act.company eq cocode
               and misc-act.job     eq job.job
             use-index misc-idx no-lock:
            if v-only-opn and not misc-act.opn then next.

            create work-aud.
            assign work-aud.job-no = misc-act.job-no
                   work-aud.job-no2 = misc-act.job-no2
                   work-aud.frm = misc-act.frm
                   work-aud.blank-no = misc-act.blank-no
                   work-aud.tran-date = misc-act.misc-date
                   work-aud.dscr = misc-act.dscr.
            if misc-act.ml then
               assign work-aud.qty = misc-act.cost
                      work-aud.procat = "MSC-M".
            else
               assign work-aud.qty = misc-act.cost
                      work-aud.procat = "MSC-H"
                      work-aud.m-code = misc-act.m-code.
         end.

         assign v-brd-job = 0
                v-mch-job = 0
                v-fg-job  = 0
                v-oth-job = 0
                v-wst-job = 0
                v-hrs-job = 0.

         for each work-aud break by tran-date:

            if work-aud.procat = "HRS" or work-aud.procat = "MSC-H" then do:
               display work-aud.procat
                       work-aud.tran-date
                       work-aud.job-no
                       work-aud.job-no2
                       work-aud.frm
                       work-aud.blank-no
                       work-aud.dscr
                       work-aud.qty
                       work-aud.waste
                       work-aud.hours
                       work-aud.m-code
                       work-aud.code
                       work-aud.complete
                    with frame fr-mch.
               down with frame fr-mch.

               IF tb_excel THEN
                  PUT STREAM excel UNFORMATTED
                      '"' work-aud.procat                    '",'
                      '"' work-aud.tran-date                 '",'
                      '"' work-aud.job-no + "-" +
                          STRING(work-aud.job-no2,"99")      '",'
                      '"' work-aud.frm                       '",'
                      '"' work-aud.blank-no                  '",'
                      '"' ""                                 '",'
                      '"' work-aud.dscr                      '",'
                      '"' STRING(work-aud.qty,">>>>>>9.99-") '",'
                      '"' STRING(work-aud.waste,">>>>9-")    '",'
                      '"' STRING(work-aud.hours,">>9.99-")   '",'
                      '"' work-aud.m-code                    '",'
                      '"' work-aud.code                      '",'
                      '"' work-aud.complete                  '",'
                      SKIP.

               assign v-mch-job = v-mch-job + work-aud.qty
                      v-wst-job = v-wst-job + work-aud.waste
                      v-hrs-job = v-hrs-job + work-aud.hours
                      v-mch-tot = v-mch-tot + work-aud.qty
                      v-wst-tot = v-wst-tot + work-aud.waste
                      v-hrs-tot = v-hrs-tot + work-aud.hours.
            end.
            else
            if work-aud.procat = "F.G." then do:
               display work-aud.procat
                       work-aud.tran-date
                       work-aud.job-no
                       work-aud.job-no2
                       work-aud.frm
                       work-aud.blank-no
                       work-aud.i-no
                       work-aud.dscr
                       work-aud.qty
                    with frame fr-fg.
               down with frame fr-fg.

               IF tb_excel THEN
                  PUT STREAM excel UNFORMATTED
                      '"' work-aud.procat                   '",'
                      '"' work-aud.tran-date                '",'
                      '"' work-aud.job-no + "-" +
                          STRING(work-aud.job-no2,"99")     '",'
                      '"' work-aud.frm                      '",'
                      '"' work-aud.blank-no                 '",'
                      '"' work-aud.i-no                     '",'
                      '"' work-aud.dscr                     '",'
                      '"' STRING(work-aud.qty,">>>>>>9.99-") '",'
                      SKIP.

               assign v-fg-job = v-fg-job + work-aud.qty
                      v-fg-tot = v-fg-tot + work-aud.qty.
            end.
            else do:
               display work-aud.procat
                       work-aud.tran-date
                       work-aud.job-no
                       work-aud.job-no2
                       work-aud.frm
                       work-aud.blank-no
                       work-aud.i-no
                       work-aud.dscr
                       work-aud.qty
                       with frame fr-mat.
               down with frame fr-mat.

               IF tb_excel THEN
                  PUT STREAM excel UNFORMATTED
                      '"' work-aud.procat                   '",'
                      '"' work-aud.tran-date                '",'
                      '"' work-aud.job-no + "-" +
                          STRING(work-aud.job-no2,"99")     '",'
                      '"' work-aud.frm                      '",'
                      '"' work-aud.blank-no                 '",'
                      '"' work-aud.i-no                     '",'
                      '"' work-aud.dscr                     '",'
                      '"' STRING(work-aud.qty,">>>>>>9.99-") '",'
                      SKIP.

               if work-aud.procat ne "MSC-M" then do:
                  find item
                      where item.company eq cocode
                        and item.i-no    eq work-aud.i-no
                      no-lock.

                  if item.mat-type = "B" then
                     assign v-brd-job = v-brd-job + work-aud.qty
                            v-brd-tot = v-brd-tot + work-aud.qty.
                  else
                     assign v-oth-job = v-oth-job + work-aud.qty
                            v-oth-tot = v-oth-tot + work-aud.qty.
               end.
            end.
            if last-of(work-aud.tran-date) then
            DO:
               put skip(1) "JOB TOTALS - " at 20 job.job-no
                   space(0) "-" space(0) job.job-no2 format "99"
                   "         BOARD TOTALS: " at 56 v-brd-job skip
                   "       MACHINE TOTALS: " at 56 v-mch-job " " v-wst-job " "
                   v-hrs-job skip
                   "FINISHED GOODS TOTALS: " at 56 v-fg-job skip
                   "OTHER MATERIAL TOTALS: " at 56 v-oth-job skip(2).

               IF tb_excel THEN
                  RUN excel-job-totals-proc(INPUT "JOB TOTALS - " + job.job-no +
                                            "-" + STRING(job.job-no2,"99"),
                                            INPUT v-brd-job, INPUT v-mch-job,
                                            INPUT v-wst-job, INPUT v-hrs-job,
                                            INPUT v-fg-job, INPUT v-oth-job).
            END.
         end.
      end.
      put skip(1) "REPORT TOTALS" at 20
             "         BOARD TOTALS: " at 56 v-brd-tot skip
             "       MACHINE TOTALS: " at 56 v-mch-tot " " v-wst-tot " "
                                             v-hrs-tot skip
             "FINISHED GOODS TOTALS: " at 56 v-fg-tot skip
             "OTHER MATERIAL TOTALS: " at 56 v-oth-tot skip.

      IF tb_excel THEN
         RUN excel-job-totals-proc(INPUT "REPORT TOTALS",
                                   INPUT v-brd-tot, INPUT v-mch-tot,
                                   INPUT v-wst-tot, INPUT v-hrs-tot,
                                   INPUT v-fg-tot, INPUT v-oth-tot).

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

