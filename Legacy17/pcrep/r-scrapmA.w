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


DEF TEMP-TABLE tt-report 
    FIELD job-no AS CHAR
    FIELD job-no2 AS INT
    FIELD sheet AS INT 
    FIELD blanks AS INT
    FIELD scraps AS INT .

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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_jstat begin_mach end_mach ~
begin_shift end_shift begin_date end_date rd_print rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lbl_jstat rd_jstat begin_mach end_mach ~
begin_shift end_shift lbl_beg-date lbl_end-date begin_date end_date ~
lbl_print rd_print rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_shift AS INTEGER FORMAT ">>" INITIAL 1 
     LABEL "Beginning Shift" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_shift AS INTEGER FORMAT ">>" INITIAL 99 
     LABEL "Ending Shift" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-scrapm.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_beg-date AS CHARACTER FORMAT "X(256)":U INITIAL "Beginning Job Close Date:" 
     VIEW-AS FILL-IN 
     SIZE 27 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_end-date AS CHARACTER FORMAT "X(256)":U INITIAL "Ending Job Close Date:" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_jstat AS CHARACTER FORMAT "X(256)":U INITIAL "Job Status?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_print AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_print AS CHARACTER INITIAL "Qty" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Qty", "Qty",
"MSF", "MSF"
     SIZE 29 BY 1 NO-UNDO.

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

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     lbl_jstat AT ROW 2.67 COL 30 COLON-ALIGNED NO-LABEL
     rd_jstat AT ROW 2.67 COL 45 NO-LABEL
     begin_mach AT ROW 3.86 COL 26.8 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_mach AT ROW 3.86 COL 69 COLON-ALIGNED HELP
          "Enter Ending Machine"
     begin_shift AT ROW 4.81 COL 26.8 COLON-ALIGNED HELP
          "Enter Beginning Shift"
     end_shift AT ROW 4.81 COL 69 COLON-ALIGNED HELP
          "Enter Ending Shift"
     lbl_beg-date AT ROW 5.71 COL 2 NO-LABEL WIDGET-ID 2
     lbl_end-date AT ROW 5.71 COL 48 NO-LABEL WIDGET-ID 4
     begin_date AT ROW 5.76 COL 26.8 COLON-ALIGNED NO-LABEL
     end_date AT ROW 5.76 COL 69 COLON-ALIGNED HELP
          "Enter Ending Due Date" NO-LABEL
     lbl_print AT ROW 7.43 COL 35 COLON-ALIGNED NO-LABEL
     rd_print AT ROW 7.43 COL 45 NO-LABEL
     rd-dest AT ROW 11.67 COL 6 NO-LABEL
     lv-ornt AT ROW 11.91 COL 31 NO-LABEL
     lines-per-page AT ROW 11.91 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 12.95 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 13.91 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.05 COL 31.2
     tb_excel AT ROW 16.29 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 16.29 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 17.1 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 19.81 COL 19
     btn-cancel AT ROW 19.81 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.71 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 10.52 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 94.8 BY 21.57.


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
         WIDTH              = 96.6
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
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_shift:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_shift:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_beg-date IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lbl_end-date IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lbl_jstat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_jstat".

/* SETTINGS FOR FILL-IN lbl_print IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_print:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_print".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_print:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON LEAVE OF begin_date IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_shift C-Win
ON LEAVE OF begin_shift IN FRAME FRAME-A /* Beginning Shift */
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
  end case.
  SESSION:SET-WAIT-STATE (""). 

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shift C-Win
ON LEAVE OF end_shift IN FRAME FRAME-A /* Ending Shift */
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

  IF rd_jstat:SCREEN-VALUE = "All" THEN
      ASSIGN lbl_beg-date:SCREEN-VALUE = " Beginning Job Start/Close:" 
             lbl_end-date:SCREEN-VALUE = "Ending Job Start/Close:" .
  ELSE  IF rd_jstat:SCREEN-VALUE = "Open" THEN
      ASSIGN lbl_beg-date:SCREEN-VALUE = "  Beginning Job Start Date:" 
             lbl_end-date:SCREEN-VALUE = " Ending Job Start date:" .
  ELSE
    ASSIGN lbl_beg-date:SCREEN-VALUE = " Beginning Job Close Date:" 
             lbl_end-date:SCREEN-VALUE = "Ending Job Close date:" .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_print C-Win
ON VALUE-CHANGED OF rd_print IN FRAME FRAME-A
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
    IF rd_jstat:SCREEN-VALUE = "All" THEN
        ASSIGN lbl_beg-date:SCREEN-VALUE = " Beginning Job Start/Close:" 
               lbl_end-date:SCREEN-VALUE = "Ending Job Start/Close:" .
    ELSE  IF rd_jstat:SCREEN-VALUE = "Open" THEN
        ASSIGN lbl_beg-date:SCREEN-VALUE = "  Beginning Job Start Date:" 
               lbl_end-date:SCREEN-VALUE = " Ending Job Start date:" .
    ELSE
        ASSIGN lbl_beg-date:SCREEN-VALUE = " Beginning Job Close Date:" 
               lbl_end-date:SCREEN-VALUE = "Ending Job Close date:" .

    APPLY "entry" TO begin_mach.
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
  DISPLAY lbl_jstat rd_jstat begin_mach end_mach begin_shift end_shift 
          lbl_beg-date lbl_end-date begin_date end_date lbl_print rd_print 
          rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 
          tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 rd_jstat begin_mach end_mach begin_shift end_shift 
         begin_date end_date rd_print rd-dest lv-ornt lines-per-page lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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
/*     /*     CREATE-TEST-FILE*/                           */
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
/* ------------------------------------------------ pc/rep/scrap1.p 04/01 JLF */
/* Scrap Report - by Machine                                                  */
/* ---------------------------------------------------------------------------*/
{sys/form/r-top3w.f}

def buffer bjob-mch for job-mch.
DEF BUFFER bf-eb FOR eb. 

def var v-fmch   like mch-act.m-code.
def var v-tmch   like v-fmch                             init "zzzzzz".
def var v-fshf   like mch-act.shift format ">>"          init 1.
def var v-tshf   like v-fshf                             init 99.
def var v-fdat   like job.start-date format "99/99/9999" init 01/01/0001.
def var v-tdat   like v-fdat                             init 12/31/9999.
def var v-q-m    as   log format "Qty/MSF"               init yes.
def var v-stat   as   char format "!"                    init "O".

def var v-up        as   int.
def var v-sheetup        as   DEC.
def var v-on        as   int.
def var v-out       as   int.
def var v-qty       as   dec.

def var v-sheets    as   dec format "->>>>,>>>,>>9" extent 4.
def var v-blanks    as   dec format "->>>>,>>>,>>9" extent 4.
def var v-scraps    as   dec format "->>>>,>>>,>>9" extent 4.
def var v-job-qty   as   dec format "->>>>,>>>,>>9".
def var v-pct       as   dec format "->>,>>9.999"       extent 2.
def var v-forms     as   int.
def var v-first-24  as   char format "x(24)".

def var v-lab       as   char format "x(13)" extent 4.

def var str_buffa   as   char no-undo.
def var v-hdr       as   char init
"Machine#,Shift,Job#,Form#,#ofSheetsReceived,#Up,#ofFGReceived,#ScrapSheets,%TotScrap,JobQty,RecVariance,%Received".
def var v-hdr1      as   char init
"Machine#,Shift,Job#,Form#,SheetsReceivedMSF,#Up,FGReceivedMSF,ScrapMSF,%TotScrap,JobMSF,RecVariance,%Received".
def var v-comma as char format "x" init "," no-undo.

 FOR EACH tt-report NO-LOCK :
          DELETE tt-report.
  END.


form header skip(1)
            "        "
            "     "
            "         "
            "     "
            v-lab[1]
            " FG/"
            v-lab[2]
            "             "
            "           "
            "             "
            "     Received"
            "           "
            skip

            "Machine#"
            "Shift"
            "Job#     "
            "Form#"
            "     Received"
            " Sht"
            "     Received"
            v-lab[3]
            "% Tot Scrap"
            v-lab[4]
            "     Variance"
            " % Received"
            skip

            "--------"
            "-----"
            "---------"
            "-----"
            "-------------"
            "----"
            "-------------"
            "-------------"
            "-----------"
            "-------------"
            "-------------"
            "-----------"
            SKIP

    with frame r-top.


SESSION:SET-WAIT-STATE ("general").

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-stat     = SUBSTR(rd_jstat,1,1)
 v-fmch     = begin_mach
 v-tmch     = end_mach
 v-fshf     = begin_shift
 v-tshf     = end_shift
 v-q-m      = rd_print EQ "Qty"
 v-fdat     = begin_date
 v-tdat     = end_date. 

if v-q-m then
  assign
   v-lab[1] = "  # of Sheets"
   v-lab[2] = "      # of FG"
   v-lab[3] = "#Scrap Sheets"
   v-lab[4] = "      Job Qty".

else
  assign
   v-lab[1] = "MSF of Sheets"
   v-lab[2] = "    MSF of FG"
   v-lab[3] = "    Scrap MSF"
   v-lab[4] = "      Job MSF".

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

VIEW FRAME r-top.

  if tb_excel then do:
    output stream s-temp to value(fi_file).
    str_buffa = "".

    if v-q-m then do:
      {sys/inc/outstrPL.i v-hdr 1 115}.
    end.

    else do:
      {sys/inc/outstrPL.i v-hdr1 1 103}.
    end.

    put stream s-temp unformatted str_buffa skip.
  end.

  for each mch-act
      where mch-act.company eq cocode
        and mch-act.m-code  ge v-fmch
        and mch-act.m-code  le v-tmch
        and mch-act.shift   ge v-fshf
        and mch-act.shift   le v-tshf
      use-index operation no-lock,

      first job
      where job.company            eq cocode
        and job.job                eq mch-act.job
        and job.job-no             eq mch-act.job-no
        and job.job-no2            eq mch-act.job-no2
        and (v-stat                eq "A"                   or
             (v-stat               eq "O" and job.opened)   or
             (v-stat               eq "C" and NOT job.opened))
        and ((job.close-date       ge v-fdat and
              job.close-date       le v-tdat and
              NOT job.opened)                       or
             (job.start-date       ge v-fdat and
              job.start-date       le v-tdat and
              job.opened))
      use-index job no-lock

      break by mch-act.m-code
            by mch-act.shift
            by mch-act.job
            by mch-act.job-no
            by mch-act.job-no2
            by mch-act.frm
            by mch-act.blank-no:

       {custom/statusMsg.i "'Processing Machine Code ' + string(mch-act.m-code)"} 

    if first-of(mch-act.m-code) then page.        

    find first est
        where est.company eq job.company
          and est.est-no  eq job.est-no
        no-lock no-error.

    if last-of(mch-act.blank-no) then do:
         IF NOT v-q-m THEN
            FIND FIRST itemfg
                WHERE itemfg.company EQ mch-act.company
                  AND itemfg.i-no EQ mch-act.i-no
            NO-LOCK NO-ERROR.
         RUN fg/GetProductionQty.p (INPUT mch-act.company,
                                INPUT mch-act.job-no,
                                INPUT mch-act.job-no2,
                                INPUT mch-act.i-no,
                                INPUT NO,
                                OUTPUT v-blanks[1]).
        IF NOT v-q-m THEN 
            v-blanks[1] = v-blanks[1] * (IF AVAIL itemfg
                                                   THEN itemfg.t-sqft
                                                   ELSE 1) / 1000.
      for each job-hdr
          where job-hdr.company   eq cocode
            and job-hdr.job       eq mch-act.job
            and job-hdr.job-no    eq mch-act.job-no
            and job-hdr.job-no2   eq mch-act.job-no2
            and job-hdr.frm       eq mch-act.frm
            and (job-hdr.blank-no eq mch-act.blank-no or mch-act.blank-no eq 0)
          no-lock:

        if v-q-m then v-job-qty = v-job-qty + job-hdr.qty.

        else do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq job-hdr.i-no
              no-lock no-error.
          v-job-qty = v-job-qty +
                      (job-hdr.qty * (if avail itemfg then itemfg.t-sqft
                                                      else 1) / 1000).
        end.

/*         for each fg-act                                                     */
/*             where fg-act.company eq cocode                                  */
/*               and fg-act.job     eq job-hdr.job                             */
/*               and fg-act.job-no  eq job-hdr.job-no                          */
/*               and fg-act.job-no2 eq job-hdr.job-no2                         */
/*               and fg-act.i-no    eq job-hdr.i-no                            */
/*              no-lock:                                                       */
/*                                                                             */
/*           if v-q-m then v-blanks[1] = v-blanks[1] + fg-act.qty.             */
/*                                                                             */
/*           else do:                                                          */
/*             find first itemfg                                               */
/*                 where itemfg.company eq cocode                              */
/*                   and itemfg.i-no    eq job-hdr.i-no                        */
/*                 no-lock no-error.                                           */
/*             v-blanks[1] = v-blanks[1] +                                     */
/*                           (fg-act.qty * (if avail itemfg then itemfg.t-sqft */
/*                                                          else 1) / 1000).   */
/*           end.                                                              */
/*         end.                                                                */
      end.

      if avail est then
      for each eb
          where eb.company   eq est.company
            and eb.est-no    eq est.est-no
            and eb.form-no   eq mch-act.frm
            and (eb.blank-no eq mch-act.blank-no or mch-act.blank-no eq 0)
          no-lock:

        v-up = v-up + eb.num-up.
      end.

      else v-up = v-up + 1.
    end.

    if last-of(mch-act.frm) then do:
      for each job-mat
          where job-mat.company eq cocode
            and job-mat.job     eq mch-act.job
            and job-mat.job-no  eq mch-act.job-no
            and job-mat.job-no2 eq mch-act.job-no2
            and job-mat.frm     eq mch-act.frm
          use-index seq-idx no-lock,

          first item
          where item.company  eq cocode
            and item.i-no     eq job-mat.i-no
            and item.mat-type eq "B"
          no-lock,

          each mat-act
          where mat-act.company eq cocode
            and mat-act.job     eq job-mat.job
            and mat-act.job-no  eq job-mat.job-no
            and mat-act.job-no2 eq job-mat.job-no2
            and mat-act.s-num   eq job-mat.frm
            and mat-act.b-num   eq job-mat.blank-no
            and mat-act.i-no    eq job-mat.i-no
          use-index job no-lock:

        run sys/ref/convquom.p(job-mat.qty-uom, if v-q-m then "EA" else "MSF",
                               job-mat.basis-w, job-mat.len,
                               job-mat.wid, item.s-dep,
                               mat-act.qty, output v-qty).

        v-sheets[1] = v-sheets[1] + v-qty.
        v-sheetup   = job-mat.n-up .
end.

      IF v-sheets[1] EQ 0 THEN      /* get sheets from slitter */
        RUN sys/inc/slitshts.p (ROWID(job),mch-act.frm, OUTPUT v-sheets[1]).

      release ef.

      if avail est then
      find first ef
          where ef.company eq est.company
            and ef.est-no  eq est.est-no
            and ef.form-no eq mch-act.frm
          no-lock no-error.

      if v-q-m then do:
        {sys/inc/roundup.i v-sheets[1]}
      end.

      if v-up eq 0 then v-up = 1.

      IF AVAIL ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-out).

      v-up  = v-up * v-out.

      IF v-sheetup = 0 THEN
          ASSIGN v-sheetup = 1.

      /*2 pc box test*/
      IF AVAIL est AND est.est-type EQ 2 OR est.est-type EQ 6 
          THEN DO:
        FIND FIRST eb 
            WHERE eb.company EQ est.company 
              AND eb.est-no = est.est-no 
              AND eb.form-no = 0
            NO-LOCK NO-ERROR.
        IF AVAIL eb THEN
           FIND FIRST bf-eb WHERE bf-eb.company = eb.company
                            AND bf-eb.est-no = eb.est-no
                            AND bf-eb.form-no = 1
                            AND bf-eb.stock-no EQ eb.stock-no
                            AND bf-eb.part-no EQ eb.part-no
                            AND recid(bf-eb) <>  RECID(eb) 
            NO-LOCK NO-ERROR.
        IF AVAIL bf-eb AND bf-eb.cust-% > 0 THEN
            v-sheetup =  v-sheetup / bf-eb.cust-%.
        ELSE IF AVAIL bf-eb AND bf-eb.yld-qty > 0 THEN
            v-sheetup =  v-sheetup / bf-eb.yld-qty.
      END. /*2 pc box test*/

      assign
       v-scraps[1] = v-sheets[1] - (v-blanks[1] / (if v-q-m then v-sheetup else 1))
       v-pct[1]    = v-scraps[1] / v-sheets[1] * 100
       v-pct[2]    = v-blanks[1] / v-job-qty * 100.

      if v-pct[1] eq ? then v-pct[1] = 0.
      if v-pct[2] eq ? then v-pct[2] = 0.

      v-first-24 = string(mch-act.m-code,"x(8)") + " "         +
                   string(mch-act.shift,">>")    + fill(" ",4) +
                   string(trim(job.job-no) + "-" +
                          string(job.job-no2,"99"),"x(9)").

      display v-first-24
              mch-act.frm                                       format ">>>>>"
              v-sheets[1]
              v-sheetup                                              format ">9.9"
              v-blanks[1]
              v-scraps[1]
              v-pct[1]
              v-job-qty
              v-job-qty - v-blanks[1]                format "->>,>>>,>>9.9<<<"
              v-pct[2]

          with frame scrap no-attr-space no-box no-labels down stream-io width 132.
      down with frame scrap.

      if tb_excel then do:
        str_buffa = trim(mch-act.m-code)                                + "," +
                    trim(string(mch-act.shift,">>"))                    + "," + 
                    trim(job.job-no) + "-" + string(job.job-no2,"99")   + "," +
                    trim(string(mch-act.frm))                           + "," +
                    trim(string(v-sheets[1]))                           + "," +
                    trim(string(v-sheetup))                                  + "," +
                    trim(string(v-blanks[1]))                           + "," +
                    trim(string(v-scraps[1]))                           + "," +
                    trim(string(v-pct[1]))                              + "," +
                    trim(string(v-job-qty))                             + "," +
                    trim(string(v-job-qty - v-blanks[1]))               + "," +
                    trim(string(v-pct[2])).

        put stream s-temp unformatted str_buffa skip.
      end.

      assign
       v-sheets[2] = v-sheets[2] + v-sheets[1]
       v-blanks[2] = v-blanks[2] + v-blanks[1]
       v-scraps[2] = v-scraps[2] + v-scraps[1]

       v-sheets[1] = 0
       v-blanks[1] = 0
       v-scraps[1] = 0
       v-job-qty   = 0
       v-up        = 0
       v-sheetup   = 0
       v-forms     = v-forms + 1.
    end.

    if last-of(mch-act.job-no2) then do:
      if v-forms gt 1 then do:
        underline v-sheets[1] v-blanks[1] v-scraps[1] v-pct[1] with frame scrap.

        v-pct[1] = v-scraps[2] / v-sheets[2] * 100.

        if v-pct[1] eq ? then v-pct[1] = 0.

        display "              Job Totals" @ v-first-24
                v-sheets[2]                @ v-sheets[1]
                v-blanks[2]                @ v-blanks[1]
                v-scraps[2]                @ v-scraps[1]
                v-pct[1]

          with frame scrap.
        down with frame scrap.
      end.

      put skip(1).

      CREATE tt-report .
      ASSIGN tt-report.job-no = mch-act.job-no 
          tt-report.job-no2 = mch-act.job-no2 
          tt-report.sheet = v-sheets[2] 
          tt-report.blanks = v-blanks[2]  
          tt-report.scraps = v-scraps[2] .

      assign
       v-sheets[3] = v-sheets[3] + v-sheets[2]
       v-blanks[3] = v-blanks[3] + v-blanks[2]
       v-scraps[3] = v-scraps[3] + v-scraps[2]

       v-sheets[2] = 0
       v-blanks[2] = 0
       v-scraps[2] = 0
       v-forms     = 0.
    end.

    if last-of(mch-act.shift) then do:
      underline v-sheets[1] v-blanks[1] v-scraps[1] v-pct[1] with frame scrap.
      underline v-sheets[1] v-blanks[1] v-scraps[1] v-pct[1] with frame scrap.

      v-pct[1] = v-scraps[3] / v-sheets[3] * 100.

      if v-pct[1] eq ? then v-pct[1] = 0.

      display "            Shift Totals" @ v-first-24
              v-sheets[3]                @ v-sheets[1]
              v-blanks[3]                @ v-blanks[1]
              v-scraps[3]                @ v-scraps[1]
              v-pct[1]

          with frame scrap.
      down with frame scrap.

      put skip(1).

      assign
     /*  v-sheets[4] =  v-sheets[4] + v-sheets[3] /* Task# 11011002 */
       v-blanks[4] =  v-blanks[4] + v-blanks[3] /* Task# 11011002 */ 
       v-scraps[4] = v-scraps[4] + v-scraps[3] */

       v-sheets[3] = 0
       v-blanks[3] = 0
       v-scraps[3] = 0.
    end.

    if last-of(mch-act.m-code) then do:
      underline v-sheets[1] v-blanks[1] v-scraps[1] v-pct[1] with frame scrap.
      underline v-sheets[1] v-blanks[1] v-scraps[1] v-pct[1] with frame scrap.

      FOR EACH tt-report NO-LOCK
          BREAK BY tt-report.job-no
          BY tt-report.job-no2:
          IF FIRST-OF(tt-report.job-no2) THEN
              ASSIGN
              v-sheets[4] = v-sheets[4] + tt-report.sheet 
              v-blanks[4] = v-blanks[4] + tt-report.blanks 
              v-scraps[4] = v-scraps[4] + tt-report.scraps .
      END.

      v-pct[1] = v-scraps[4] / v-sheets[4] * 100.

      if v-pct[1] eq ? then v-pct[1] = 0.

      display "          Machine Totals" @ v-first-24
              v-sheets[4]                @ v-sheets[1]
              v-blanks[4]                @ v-blanks[1]
              v-scraps[4]                @ v-scraps[1]
              v-pct[1]

          with frame scrap.
      down with frame scrap.

      FOR EACH tt-report NO-LOCK :
          DELETE tt-report.
      END.

      put skip(1).

      assign
       v-sheets[4] = 0
       v-blanks[4] = 0
       v-scraps[4] = 0.
    end.
  end.

  IF tb_excel THEN DO:
  OUTPUT STREAM s-temp CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

SESSION:SET-WAIT-STATE("").

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).  

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

