&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aprep\r-recrep.w

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

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD check-act LIKE ap-pay.check-act
    FIELD check-no AS CHAR FORMAT "x(13)"
    FIELD vend-no AS CHAR FORMAT "X(30)"
    FIELD rpt-date LIKE ap-pay.check-date
    FIELD period LIKE ap-pay.period
    FIELD check-amt LIKE ap-pay.check-amt
    FIELD cleared AS CHAR
    FIELD misc-chk AS CHAR FORMAT "X(10)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 bank_code begin_vend end_vend ~
begin_chk-date end_chk-date begin_chk-no end_chk-no begin_j-no end_j-no ~
tb_dep tb_jrn rd_rec tb_clr tb_unclr tb_vend-sort rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS bank_code begin_vend end_vend ~
begin_chk-date end_chk-date begin_chk-no end_chk-no begin_j-no end_j-no ~
tb_dep tb_jrn lbl_rec rd_rec tb_clr tb_unclr tb_vend-sort rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE bank_code AS CHARACTER FORMAT "X(8)" 
     LABEL "Enter Bank Code or Leave Blank For All" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_chk-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Check Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_chk-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Check#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_j-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Beginning Journal #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_chk-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Check Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_chk-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Check#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_j-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "Ending Journal #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_rec AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
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
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_rec AS CHARACTER INITIAL "Unreconciled" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Unreconciled", "Unreconciled",
"Reconciled", "Reconciled",
"All", "All"
     SIZE 46 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 14.05.

DEFINE VARIABLE tb_clr AS LOGICAL INITIAL no 
     LABEL "Clear Unreconciled?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tb_dep AS LOGICAL INITIAL yes 
     LABEL "Include Deposits?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE tb_jrn AS LOGICAL INITIAL yes 
     LABEL "Include Journals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE tb_unclr AS LOGICAL INITIAL no 
     LABEL "Unclear Reconciled?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tb_vend-sort AS LOGICAL INITIAL no 
     LABEL "Sort by Vendor#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     bank_code AT ROW 2.19 COL 57 COLON-ALIGNED HELP
          "Enter Bank code or Leave Blank For all"
     begin_vend AT ROW 3.62 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Vendor"
     end_vend AT ROW 3.62 COL 69 COLON-ALIGNED HELP
          "Enter Ending Vendor"
     begin_chk-date AT ROW 4.81 COL 26 COLON-ALIGNED
     end_chk-date AT ROW 4.81 COL 69 COLON-ALIGNED HELP
          "Enter Ending Check Date"
     begin_chk-no AT ROW 6 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Check Number"
     end_chk-no AT ROW 6 COL 69 COLON-ALIGNED HELP
          "Enter Ending Check Number"
     begin_j-no AT ROW 7.19 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Journal Number"
     end_j-no AT ROW 7.19 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     tb_dep AT ROW 8.62 COL 34
     tb_jrn AT ROW 9.81 COL 34
     lbl_rec AT ROW 11 COL 20 COLON-ALIGNED NO-LABEL
     rd_rec AT ROW 11 COL 29 NO-LABEL
     tb_clr AT ROW 12.43 COL 18
     tb_unclr AT ROW 12.43 COL 49
     tb_vend-sort AT ROW 13.62 COL 34
     rd-dest AT ROW 16.24 COL 5 NO-LABEL
     lv-ornt AT ROW 16.95 COL 32 NO-LABEL
     lines-per-page AT ROW 16.95 COL 85 COLON-ALIGNED
     lv-font-no AT ROW 18.38 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 19.33 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 21 COL 31
     btn-ok AT ROW 23.62 COL 19
     btn-cancel AT ROW 23.62 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.29 COL 2
     RECT-6 AT ROW 15.05 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 94.4 BY 24.24.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Reconciliation Report"
         HEIGHT             = 24.05
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
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       bank_code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_chk-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_chk-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_j-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_chk-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_chk-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_j-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_rec IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_rec:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_rec".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_rec:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_clr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_dep:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_jrn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_unclr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_vend-sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Reconciliation Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Reconciliation Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bank_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bank_code C-Win
ON LEAVE OF bank_code IN FRAME FRAME-A /* Enter Bank Code or Leave Blank For All */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_chk-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_chk-date C-Win
ON LEAVE OF begin_chk-date IN FRAME FRAME-A /* Beginning Check Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_chk-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_chk-no C-Win
ON LEAVE OF begin_chk-no IN FRAME FRAME-A /* Beginning Check# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_j-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_j-no C-Win
ON LEAVE OF begin_j-no IN FRAME FRAME-A /* Beginning Journal # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
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
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust= bank_code
                            &END_cust= bank_code
                            &fax-subject="Reconcilitation Report"
                            &fax-body="Reconcilitation Report"
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust= bank_code
                             &END_cust= bank_code
                             &mail-subject="Reconcilitation Report"
                             &mail-body="Reconcilitation Report"
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust= bank_code
                                  &END_cust= bank_code
                                  &mail-subject="Reconcilitation Report"
                                  &mail-body="Reconcilitation Report"
                                  &mail-file=list-name }
           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_chk-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_chk-date C-Win
ON LEAVE OF end_chk-date IN FRAME FRAME-A /* Ending Check Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_chk-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_chk-no C-Win
ON LEAVE OF end_chk-no IN FRAME FRAME-A /* Ending Check# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_j-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_j-no C-Win
ON LEAVE OF end_j-no IN FRAME FRAME-A /* Ending Journal # */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
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


&Scoped-define SELF-NAME rd_rec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_rec C-Win
ON VALUE-CHANGED OF rd_rec IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_clr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_clr C-Win
ON VALUE-CHANGED OF tb_clr IN FRAME FRAME-A /* Clear Unreconciled? */
DO:
  /*IF {&self-name}:SCREEN-VALUE EQ "yes" THEN
    tb_unclr:SCREEN-VALUE = "no".*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_dep C-Win
ON VALUE-CHANGED OF tb_dep IN FRAME FRAME-A /* Include Deposits? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_jrn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_jrn C-Win
ON VALUE-CHANGED OF tb_jrn IN FRAME FRAME-A /* Include Journals? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_unclr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_unclr C-Win
ON VALUE-CHANGED OF tb_unclr IN FRAME FRAME-A /* Unclear Reconciled? */
DO:
  /*IF {&self-name}:SCREEN-VALUE EQ "yes" THEN
    tb_clr:SCREEN-VALUE = "no".*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_vend-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_vend-sort C-Win
ON VALUE-CHANGED OF tb_vend-sort IN FRAME FRAME-A /* Sort by Vendor#? */
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

  assign
   begin_chk-date = DATE(01,01,YEAR(TODAY))
   end_chk-date   = DATE(12,31,YEAR(TODAY)).

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO bank_code.
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
  DISPLAY bank_code begin_vend end_vend begin_chk-date end_chk-date begin_chk-no 
          end_chk-no begin_j-no end_j-no tb_dep tb_jrn lbl_rec rd_rec tb_clr 
          tb_unclr tb_vend-sort rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 bank_code begin_vend end_vend begin_chk-date 
         end_chk-date begin_chk-no end_chk-no begin_j-no end_j-no tb_dep tb_jrn 
         rd_rec tb_clr tb_unclr tb_vend-sort rd-dest lv-ornt lines-per-page 
         lv-font-no td-show-parm btn-ok btn-cancel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-port C-Win 
PROCEDURE output-to-port :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN custom/dprint.w (list-name).

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
/* --------------------------------------------------- ap/ap-chkr.p 11/94 gb  */
/*                                                                            */
/* a/p - Check Reconciliation Report                                          */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3.f}

DEF BUFFER ar-mcash-ref FOR reftable.
DEF BUFFER b-ap-pay FOR ap-pay.

def var v-chkno like ap-chk.check-no format ">>>>>9" init 0 NO-UNDO.
def var v-chkno2 like v-chkno init 999999 NO-UNDO.
def var v-s-date like ap-chk.check-date format "99/99/9999" init "01/01/0001" NO-UNDO.
def var v-e-date like v-s-date init TODAY NO-UNDO.
def var v-bank-code like bank.bank-code no-undo.
def var v-rep-type as ch format "x" no-undo init "U".
def var v-head-1 as ch format 'x(20)' no-undo.
def var v-bank-act like bank.actnum no-undo.
def var v-bank-hld like bank.actnum no-undo.
def var v-tot-amt as dec init 0 format "->,>>>,>>9.99" no-undo.
def var save_id as RECID NO-UNDO.
def var time_stamp as ch NO-UNDO.
DEF VAR v-amt AS DEC NO-UNDO.
DEF VAR ll-skipped AS LOG NO-UNDO.

form tt-report.check-act    column-label "Bank Account #"
     tt-report.check-no     column-label "Chk/Jrnl#"
     tt-report.vend-no      column-label "Vendor#"
     tt-report.rpt-date     column-label "Date"
     tt-report.period       column-label "P"
     tt-report.check-amt    column-label "Amount"
     tt-report.cleared      column-label "CLR"
                            format "x(4)"

    with STREAM-IO width 102 frame a no-attr-space no-box down.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 56}

 v-bank-code  = bank_code 
 v-bank-act   = ""
 v-s-date     = begin_chk-date
 v-e-date     = end_chk-date
 v-chkno      = begin_chk-no
 v-chkno2     = end_chk-no 
 v-rep-type   = substr(rd_rec, 1,1).

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.

SESSION:SET-WAIT-STATE ("general").

  EMPTY TEMP-TABLE tt-report.

  IF v-bank-code NE "" THEN DO:
    FIND FIRST bank NO-LOCK
        WHERE bank.company   EQ cocode
          AND bank.bank-code EQ v-bank-code
        NO-ERROR.
    v-bank-act = IF AVAIL bank THEN bank.actnum ELSE FILL("z",100).
  END.

  FOR EACH ap-pay
      WHERE ap-pay.company    EQ cocode
        AND ap-pay.vend-no    GE begin_vend
        AND ap-pay.vend-no    LE end_vend
        AND ap-pay.check-no   GE v-chkno
        AND ap-pay.check-no   LE v-chkno2
        AND (ap-pay.check-act EQ v-bank-act OR v-bank-act EQ "")
        AND (ap-pay.bank-code EQ v-bank-code OR v-bank-code EQ "")
        AND ap-pay.posted
        AND ((v-rep-type      EQ "U" AND NOT ap-pay.cleared) OR
             (v-rep-type      EQ "R" AND ap-pay.cleared) OR
             v-rep-type       EQ "A")
        AND ap-pay.memo       EQ NO
      USE-INDEX ap-pay

      TRANSACTION:

    RELEASE ap-ledger.
    FIND FIRST ap-ledger NO-LOCK
        WHERE ap-ledger.company  EQ ap-pay.company
          AND ap-ledger.vend-no  EQ ap-pay.vend-no
          AND ap-ledger.refnum   EQ "AC" + STRING(ap-pay.check-no, "999999")
          AND ap-ledger.ref-date EQ ap-pay.check-date
        USE-INDEX ap-ledger NO-ERROR.

    IF NOT AVAIL ap-ledger THEN
    DO:
       IF v-bank-code EQ "" THEN
          find first bank where
               bank.company = ap-pay.company AND
               bank.bank-code = ap-pay.bank-code
               NO-LOCK NO-ERROR.

       IF AVAIL bank THEN
          FIND FIRST ap-ledger NO-LOCK
               WHERE ap-ledger.company  EQ ap-pay.company
                 AND ap-ledger.vend-no  EQ ap-pay.vend-no
                 AND ap-ledger.refnum   EQ "CHK# " + string(ap-pay.check-no) +
                                           " CD#" + bank.bank-code
                 AND ap-ledger.ref-date EQ ap-pay.check-date
               USE-INDEX ap-ledger NO-ERROR.

       IF v-bank-code EQ "" THEN
          RELEASE bank.
    END.

    ll-skipped = ap-pay.d-no NE 0 AND
                 NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no).

    IF AVAIL ap-ledger               AND
       ap-ledger.tr-date GE v-s-date AND
       ap-ledger.tr-date LE v-e-date THEN DO:
      CREATE tt-report.

      ASSIGN
       tt-report.rec-id    = RECID(ap-pay)
       tt-report.key-01    = ap-pay.check-act
       tt-report.key-02    = (IF tb_vend-sort THEN ap-pay.vend-no ELSE "")
       tt-report.key-03    = STRING(ap-pay.check-no,"9999999999")
       tt-report.check-act = ap-pay.check-act
       tt-report.check-no  = STRING(ap-pay.check-no)
       tt-report.vend-no   = ap-pay.vend-no
       tt-report.rpt-date  = ap-ledger.tr-date
       tt-report.period    = ap-pay.period
       tt-report.check-amt = IF NOT ll-skipped THEN ap-pay.check-amt * -1 ELSE ap-pay.check-amt
       tt-report.cleared   = IF ll-skipped     THEN "Void" ELSE
                             IF ap-pay.cleared THEN "***"  ELSE
                             IF tb_clr         THEN "CLR"  ELSE "".

      IF ap-pay.vend-no EQ "" THEN
      DO:
          FIND FIRST ap-dis WHERE
               ap-dis.d-no = ap-pay.d-no
               NO-LOCK NO-ERROR.

          IF AVAIL ap-dis THEN
          DO:
             tt-report.vend-no = ap-dis.payee.
             RELEASE ap-dis.
          END.
      END.

      IF tb_clr AND NOT ap-pay.cleared THEN ap-pay.cleared = YES.

      IF tb_unclr AND ap-pay.cleared THEN ap-pay.cleared = NO.
    END.

    IF ap-pay.reconciled EQ ? AND NOT ll-skipped THEN DO:
      FIND FIRST ap-ledger NO-LOCK
          WHERE ap-ledger.company EQ ap-pay.company
            AND ap-ledger.vend-no EQ ap-pay.vend-no
            AND ap-ledger.refnum  EQ "VOIDED CHECK" +
                                     STRING(ap-pay.check-no,"zzzzzzz9")
          NO-ERROR.
      IF AVAIL ap-ledger               AND
         ap-ledger.tr-date GE v-s-date AND
         ap-ledger.tr-date LE v-e-date THEN DO:

        CREATE tt-report.
        ASSIGN
         tt-report.rec-id    = RECID(ap-pay)
         tt-report.key-01    = ap-pay.check-act
         tt-report.key-02    = (IF tb_vend-sort THEN ap-pay.vend-no ELSE "")
         tt-report.key-03    = STRING(ap-pay.check-no,"9999999999")
         tt-report.check-act = ap-pay.check-act
         tt-report.check-no  = STRING(ap-pay.check-no)
         tt-report.vend-no   = ap-pay.vend-no
         tt-report.rpt-date  = ap-ledger.tr-date
         tt-report.period    = ap-ledger.period
         tt-report.check-amt = ap-pay.check-amt
         tt-report.cleared   = "VOID".

        IF ap-pay.vend-no EQ "" THEN
        DO:
           FIND FIRST ap-dis WHERE
                ap-dis.d-no = ap-pay.d-no
                NO-LOCK NO-ERROR.

           IF AVAIL ap-dis THEN
           DO:
              tt-report.vend-no = ap-dis.payee.
              RELEASE ap-dis.
           END.
        END.
      END.
    END.
  END.

  IF tb_dep THEN DO:
    FOR EACH ar-cash
        WHERE ar-cash.company    EQ cocode
          AND ar-cash.reconciled EQ NO
          AND ar-cash.posted     EQ YES
          AND ar-cash.memo       EQ NO
          AND (ar-cash.bank-code EQ v-bank-code OR v-bank-code EQ "")
          AND ar-cash.check-date GE v-s-date
          AND ar-cash.check-date LE v-e-date
          AND ((v-rep-type       EQ "U" AND NOT ar-cash.cleared) OR
               (v-rep-type       EQ "R" AND ar-cash.cleared) OR
               v-rep-type        EQ "A")
        USE-INDEX reconciled,
        FIRST ar-ledger NO-LOCK
        WHERE ar-ledger.company  EQ ar-cash.company
          AND ar-ledger.cust-no  EQ ar-cash.cust-no
          AND ar-ledger.ref-date EQ ar-cash.check-date
          AND ar-ledger.ref-num  EQ "CHK# " + STRING(ar-cash.check-no,"9999999999"),
        FIRST bank NO-LOCK
        WHERE bank.company   EQ ar-cash.company
          AND bank.bank-code EQ ar-cash.bank-code
          AND (bank.actnum   EQ v-bank-act OR v-bank-act EQ "")
        BREAK BY ar-cash.bank-code
              BY ar-ledger.tr-date
              BY ar-ledger.tr-num

        TRANSACTION:

      v-amt = v-amt + ar-cash.check-amt.

      IF LAST-OF(ar-ledger.tr-num) THEN DO:
        CREATE tt-report.
        ASSIGN
         tt-report.rec-id    = RECID(ar-cash)
         tt-report.key-01    = bank.actnum
         tt-report.key-02    = ""
         tt-report.key-03    = "Dep" + STRING(ar-ledger.tr-num,"9999999999")
         tt-report.check-act = bank.actnum
         tt-report.check-no  = "Deposit"
         tt-report.vend-no   = ""
         tt-report.rpt-date  = ar-ledger.tr-date
         tt-report.check-amt = v-amt
         tt-report.cleared   = IF ar-cash.cleared THEN "***" ELSE
                               IF tb_clr THEN "CLR" ELSE ""
         v-amt               = 0.
      END.

      IF tb_clr AND NOT ar-cash.cleared THEN ar-cash.cleared = YES.

      IF tb_unclr AND ar-cash.cleared THEN ar-cash.cleared = NO.
    END.

    FOR EACH ar-mcash NO-LOCK
        WHERE ar-mcash.company    EQ cocode
          AND ar-mcash.posted     EQ YES
          AND (ar-mcash.bank-code EQ v-bank-code OR v-bank-code EQ "")
          AND ar-mcash.check-date GE v-s-date
          AND ar-mcash.check-date LE v-e-date,
        FIRST ar-ledger NO-LOCK
        WHERE ar-ledger.company  EQ ar-mcash.company
          AND ar-ledger.cust-no  EQ ""
          AND ar-ledger.ref-date EQ ar-mcash.check-date
          AND ar-ledger.ref-num  EQ STRING(ar-mcash.m-no) + " " + ar-mcash.payer,
        FIRST ar-mcash-ref
        WHERE ar-mcash-ref.rec_key  EQ ar-mcash.rec_key
          AND ar-mcash-ref.reftable EQ "ar-mcash-ref"
          AND ar-mcash-ref.company  EQ "ar-mcash"
          AND ar-mcash-ref.val[1]   EQ 0
          AND ((v-rep-type       EQ "U" AND ar-mcash-ref.val[2] EQ 0) OR
               (v-rep-type       EQ "R" AND ar-mcash-ref.val[2] NE 0) OR
               v-rep-type        EQ "A")
        USE-INDEX rec_key,
        FIRST bank NO-LOCK
        WHERE bank.company   EQ ar-mcash.company
          AND bank.bank-code EQ ar-mcash.bank-code
          AND (bank.actnum   EQ v-bank-act OR v-bank-act EQ "")
        BREAK BY ar-mcash.bank-code
              BY ar-ledger.tr-date

        TRANSACTION:

      FIND FIRST reftable NO-LOCK
      WHERE reftable.reftable = "AR-MCASH"       
        AND reftable.company  = ar-mcash.company
        AND reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9")
        AND reftable.code     = ar-mcash.rec_key NO-ERROR.

      CREATE tt-report.
      ASSIGN
       tt-report.rec-id    = RECID(ar-mcash)
       tt-report.key-01    = bank.actnum
       tt-report.key-02    = ""
       tt-report.key-03    = "Misc Cash"
       tt-report.check-act = bank.actnum
       tt-report.check-no  = "Misc Cash"
       tt-report.vend-no   = ""
       tt-report.rpt-date  = ar-ledger.tr-date
       tt-report.check-amt = ar-mcash.check-amt
       tt-report.cleared   = IF ar-mcash-ref.val[2] NE 0 THEN "***" ELSE
                             IF tb_clr THEN "CLR" ELSE ""

       tt-report.misc-chk  = IF AVAIL reftable AND 
                               (TRIM(reftable.code2) NE "" OR
                                reftable.code2 NE "0000000000")
                               THEN reftable.code2 ELSE "".

      IF tb_clr AND ar-mcash-ref.val[2] EQ 0 THEN ar-mcash-ref.val[2] = 1.
      IF tb_unclr AND ar-mcash-ref.val[2] NE 0 THEN ar-mcash-ref.val[2] = 0.
    END.
  END.

  v-amt = 0.

  IF tb_jrn THEN
  FOR EACH gl-jrn
      WHERE gl-jrn.company    EQ cocode
        AND gl-jrn.reconciled EQ NO
        AND gl-jrn.posted     EQ YES
        AND gl-jrn.tr-date    GE v-s-date
        AND gl-jrn.tr-date    LE v-e-date
        AND ((v-rep-type      EQ "U" AND NOT gl-jrn.cleared) OR
             (v-rep-type      EQ "R" AND gl-jrn.cleared) OR
             v-rep-type       EQ "A")
      USE-INDEX reconciled,
      EACH gl-jrnl NO-LOCK
      WHERE gl-jrnl.j-no    EQ gl-jrn.j-no
        AND (gl-jrnl.actnum EQ v-bank-act OR v-bank-act EQ ""),
      FIRST bank NO-LOCK
      WHERE bank.company EQ gl-jrn.company
        AND bank.actnum  EQ gl-jrnl.actnum
      BREAK BY gl-jrnl.actnum
            BY gl-jrnl.j-no

      TRANSACTION:

    v-amt = v-amt + gl-jrnl.tr-amt.

    IF LAST-OF(gl-jrnl.j-no) THEN DO:

      CREATE tt-report.
      ASSIGN
       tt-report.rec-id    = RECID(gl-jrn)
       tt-report.key-01    = bank.actnum
       tt-report.key-02    = ""
       tt-report.key-03    = STRING(gl-jrn.journal,"9999999999")
       tt-report.check-act = bank.actnum
       tt-report.check-no  = STRING(gl-jrn.journal)
       tt-report.vend-no   = "Journal"
       tt-report.rpt-date  = gl-jrn.tr-date
       tt-report.check-amt = v-amt
       tt-report.cleared   = IF gl-jrn.cleared THEN "***" ELSE
                             IF tb_clr THEN "CLR" ELSE ""
       v-amt               = 0.
    END.

    IF tb_clr AND NOT gl-jrn.cleared THEN gl-jrn.cleared = YES.

    IF tb_unclr AND gl-jrn.cleared THEN gl-jrn.cleared = NO.
  END.

  FOR EACH tt-report /*,
      FIRST ap-pay WHERE RECID(ap-pay) EQ tt-report.rec-id NO-LOCK*/
      BREAK BY tt-report.key-01
            BY tt-report.key-02
            BY tt-report.key-03
            BY tt-report.rpt-date:

    IF v-bank-hld NE tt-report.check-act THEN DO:
      IF v-bank-hld GT "" THEN PAGE.
      v-bank-hld = tt-report.check-act.
    END.

    DISPLAY tt-report.check-act
            tt-report.check-no
            tt-report.vend-no
            tt-report.rpt-date
            tt-report.period
            tt-report.check-amt
            tt-report.cleared
           SPACE(22)
            tt-report.misc-chk NO-LABEL
        WITH FRAME a.
    DOWN WITH FRAME a.

    v-tot-amt = v-tot-amt + tt-report.check-amt.
  END.  /* for each loop */

  DISPLAY "Total Amount:" AT 73 v-tot-amt WITH STREAM-IO width 102 FRAME tot NO-LABELS.

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

