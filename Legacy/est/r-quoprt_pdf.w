&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-quoprt.w

  Description: Quote Printing

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: JLF

  Created: 09/20/02

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
DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

{est/printquo.i NEW}

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

{custom/xprint.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust end_cust begin_quo# end_quo# ~
tb_inst begin_dept end_dept tb_note rd_sort tb_comm tb_prt-box rd-dest ~
lv-ornt lines-per-page lv-font-no td-show-parm btn-ok btn-cancel RECT-6 ~
RECT-7 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_quo# end_quo# ~
tb_inst begin_dept end_dept tb_note lbl_sort rd_sort tb_comm tb_prt-box ~
rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 

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
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_dept AS CHARACTER FORMAT "X(2)" 
     LABEL "Beginning Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_quo# AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Beginning Quote#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_dept AS CHARACTER FORMAT "X(2)" INITIAL "zz" 
     LABEL "Ending Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_quo# AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "Ending Quote#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort by?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
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
"To Email", 5
     SIZE 23 BY 5.95 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Quote#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Estimate#", "Estimate#",
"Cust Part#", "Cust Part#",
"Quote#", "Quote#"
     SIZE 48 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 92 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY .95.

DEFINE VARIABLE tb_comm AS LOGICAL INITIAL no 
     LABEL "Print Salesman Commission?" 
     VIEW-AS TOGGLE-BOX
     SIZE 81 BY 1 NO-UNDO.

DEFINE VARIABLE tb_inst AS LOGICAL INITIAL no 
     LABEL "Print Department Manufacturing Instructions?" 
     VIEW-AS TOGGLE-BOX
     SIZE 81 BY 1 NO-UNDO.

DEFINE VARIABLE tb_note AS LOGICAL INITIAL yes 
     LABEL "Print Notes per Item or Form?" 
     VIEW-AS TOGGLE-BOX
     SIZE 81 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-box AS LOGICAL INITIAL no 
     LABEL "Print Box Design?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust AT ROW 2.43 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 2.43 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_quo# AT ROW 3.38 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Quote Number"
     end_quo# AT ROW 3.38 COL 69 COLON-ALIGNED HELP
          "Enter Ending QuoteNumber"
     tb_inst AT ROW 4.57 COL 7
     begin_dept AT ROW 5.52 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Department"
     end_dept AT ROW 5.52 COL 69 COLON-ALIGNED HELP
          "Enter Endng Department"
     tb_note AT ROW 6.95 COL 87 RIGHT-ALIGNED
     lbl_sort AT ROW 7.91 COL 18 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 7.91 COL 30 NO-LABEL
     tb_comm AT ROW 9.33 COL 87 RIGHT-ALIGNED
     tb_prt-box AT ROW 10.76 COL 7
     rd-dest AT ROW 13.14 COL 5 NO-LABEL
     lv-ornt AT ROW 13.38 COL 30 NO-LABEL
     lines-per-page AT ROW 13.38 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 14.81 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 15.76 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.81 COL 5
     btn-ok AT ROW 21.24 COL 21
     btn-cancel AT ROW 21.24 COL 60
     RECT-6 AT ROW 11.95 COL 2
     RECT-7 AT ROW 1 COL 1
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.19 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
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
         TITLE              = "Print Quotes"
         HEIGHT             = 21.81
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\xRemove.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\xRemove.ico"
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
ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_quo#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_quo#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_comm IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_comm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_inst:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_note IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_note:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Print Quotes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Print Quotes */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_dept C-Win
ON LEAVE OF begin_dept IN FRAME FRAME-A /* Beginning Department */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_quo#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_quo# C-Win
ON LEAVE OF begin_quo# IN FRAME FRAME-A /* Beginning Quote# */
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  assign rd-dest tb_prt-box.
  /*
  IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "SouthPak"
       THEN is-xprint-form = YES.     
  ELSE is-xprint-form = NO.
  */
  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &TYPE="CUSTOMER"
                            &begin_cust=begin_cust
                            &END_cust=END_cust
                            &fax-subject="Quote"
                            &fax-body="Quote"
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_cust
                             &END_cust=end_cust
                             &mail-subject="Quote"
                             &mail-body="Quote"
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_cust
                                  &END_cust=end_cust
                                  &mail-subject="Quote"
                                  &mail-body="Quote"
                                  &mail-file=list-name }

           END.
 
       END. 
  end case. 
  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_dept C-Win
ON LEAVE OF end_dept IN FRAME FRAME-A /* Ending Department */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_quo#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_quo# C-Win
ON LEAVE OF end_quo# IN FRAME FRAME-A /* Ending Quote# */
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


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_comm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_comm C-Win
ON VALUE-CHANGED OF tb_comm IN FRAME FRAME-A /* Print Salesman Commission? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inst C-Win
ON VALUE-CHANGED OF tb_inst IN FRAME FRAME-A /* Print Department Manufacturing Instructions? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_note
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_note C-Win
ON VALUE-CHANGED OF tb_note IN FRAME FRAME-A /* Print Notes per Item or Form? */
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  FIND FIRST quotehd WHERE ROWID(quotehd) EQ ip-rowid NO-LOCK NO-ERROR.
  IF AVAIL quotehd THEN
  FIND FIRST est
      WHERE est.company EQ quotehd.company
        AND est.est-no  EQ quotehd.est-no
      NO-LOCK NO-ERROR.
  
  
  /*IF NOT AVAIL est THEN RETURN.*/

  ASSIGN
   begin_cust = quotehd.cust-no
   end_cust   = begin_cust
   begin_quo# = quotehd.q-no
   end_quo#   = begin_quo#.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "QUOPRINT"
      no-lock no-error.
  if not avail sys-ctrl then
  do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "QUOPRINT"
     sys-ctrl.descrip = "Print Quote Headers on Quote Form".
    message sys-ctrl.descrip update sys-ctrl.log-fld.
  end.
  ASSIGN
   v-print-fmt = sys-ctrl.char-fld
   v-log       = sys-ctrl.log-fld.

  /*IF v-print-fmt EQ "10 Pitch" AND
     AVAIL est AND est.est-type LE 4 THEN
    ASSIGN
     v-program      = "ce/quote/prtquo10.p"
     lines-per-page = 56.
 
  ELSE*/
  IF /* v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "SouthPak"*/
    INDEX("Pacific,Xprint,SouthPak,ABox,Midwest,century",v-print-fmt) > 0
       THEN is-xprint-form = YES.     
  ELSE is-xprint-form = NO.

  IF v-print-fmt EQ "HOP" THEN
    ASSIGN
     v-program      = "cec/quote/quohop.p"
     lines-per-page = 37.

  ELSE
  IF v-print-fmt EQ "LandScap" THEN
    ASSIGN
     v-program      = "ce/quote/landquo.p"
     lines-per-page = 56.

  ELSE
  IF v-print-fmt EQ "ContSrvc" OR
     v-print-fmt EQ "Triad"    THEN
    ASSIGN
     v-program      = "cec/quote/quocsc.p"
     lines-per-page = 56.

  ELSE
  IF /*v-print-fmt EQ "Midwest" OR got new xprint format*/
     v-print-fmt EQ "RFC"     THEN
    ASSIGN
     v-program      = "cec/quote/quomidw.p"
     lines-per-page = 58.

  ELSE
  IF v-print-fmt EQ "Rudd" THEN
    ASSIGN
     v-program      = "cec/quote/quorudd.p"
     lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "General" THEN
    ASSIGN
     v-program      = "cec/quote/quogener.p"
     lines-per-page = 56.

  ELSE
  IF v-print-fmt EQ "10 Pitch" /*AND AVAIL est AND est.est-type GT 4*/ THEN
    ASSIGN
     v-program      = "cec/quote/prtquo10.p"
     lines-per-page = 56.

  ELSE
  IF v-print-fmt EQ "Brick" THEN
    ASSIGN
     v-program      = "cec/quote/quobrick.p"
     lines-per-page = 38.

  ELSE
  IF v-print-fmt EQ "Fibre" THEN 
    ASSIGN
     v-program      = "cec/quote/quofibre.p"
     lines-per-page = 52.
    
  ELSE
  IF v-print-fmt EQ "Harwell" THEN
    ASSIGN
     v-program      = "cec/quote/quoharwl.p"
     lines-per-page = 56.
  ELSE
  IF v-print-fmt EQ "Pacific" THEN
    ASSIGN
     v-program      = "cec/quote/quopacif.p"
     lines-per-page = 66.  
  ELSE
  IF v-print-fmt EQ "Abox" THEN
    ASSIGN
     v-program      = "cec/quote/quoabox.p"
     lines-per-page = 66.  

  ELSE
  IF v-print-fmt EQ "Xprint" THEN
    ASSIGN
     v-program      = "cec/quote/quoxprnt.p"
     lines-per-page = 66.  

  ELSE IF v-print-fmt EQ "SouthPak" THEN
    ASSIGN
     v-program      = "cec/quote/quosthpk.p"
     lines-per-page = 66.  

  ELSE IF v-print-fmt EQ "Midwest" THEN
    ASSIGN
     v-program      = "cec/quote/quomwest.p"
     lines-per-page = 66.  

  ELSE IF v-print-fmt EQ "Century" THEN
    ASSIGN
     v-program      = "cec/quote/quocentx.p"
     lines-per-page = 66.  

  ELSE
  IF AVAIL est AND est.est-type GT 4 THEN
    ASSIGN
     v-program      = "cec/quote/quoasi.p"
     lines-per-page = 56.

  ELSE
    ASSIGN
     v-program      = "ce/quote/quoasi.p"
     lines-per-page = IF v-log THEN 50 ELSE 56.

 
  RUN enable_UI.

  DO WITH FRAME {&frame-name}:
    DISABLE lines-per-page.
    IF AVAIL est AND est.est-type LE 4 THEN DISABLE tb_note rd_sort tb_comm.
    IF NOT AVAIL est THEN  DISABLE  tb_note rd_sort tb_comm.

    IF v-print-fmt NE "10 Pitch" THEN DISABLE tb_note.
    IF v-print-fmt NE "Brick" AND
       v-print-fmt NE "ASI" AND v-print-fmt NE "PACIFIC"
        THEN DISABLE tb_comm.
    
    IF is-xprint-form = NO THEN DISABLE tb_prt-box.

  END.
  
  {methods/nowait.i}
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
  DISPLAY begin_cust end_cust begin_quo# end_quo# tb_inst begin_dept end_dept 
          tb_note lbl_sort rd_sort tb_comm tb_prt-box rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_cust end_cust begin_quo# end_quo# tb_inst begin_dept end_dept 
         tb_note rd_sort tb_comm tb_prt-box rd-dest lv-ornt lines-per-page 
         lv-font-no td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-fax C-Win 
PROCEDURE output-to-fax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  

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
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
          
     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
    /*     CREATE-TEST-FILE*/
         SAVE-AS
         USE-FILENAME
   
         UPDATE OKpressed.
         
     IF NOT OKpressed THEN  RETURN NO-APPLY.


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

 IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
 END.
 ELSE RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).

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
  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
  
  /*ELSE run scr-rpt.w (list-name,c-win:title). /* open file-name, title */  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* -------------------------------------------- cec/quote/printquo.p 8/94 rd  */
/* print quotes                                                               */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.i}

ASSIGN
 fcust    = begin_cust
 tcust    = end_cust
 fquote   = begin_quo#
 tquote   = end_quo#
 ch-multi = fquote NE tquote
 v-prt-box = tb_prt-box.

IF NOT ch-multi THEN
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
   tb_note:SCREEN-VALUE = "YES"
   rd_sort:SCREEN-VALUE = "Estimate#"
   tb_note
   rd_sort.
END.

ASSIGN
 ch-inst  = tb_inst
 fdept    = begin_dept
 tdept    = end_dept
 ch-note  = tb_note
 ch-sort  = SUBSTR(rd_sort,1,1)
 v-comm   = tb_comm
 ch-multi = fquote NE tquote.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

{sa/sa-sls01.i}

FIND FIRST quotehd WHERE ROWID(quotehd) EQ ip-rowid NO-LOCK NO-ERROR.
IF AVAIL quotehd THEN
   FIND FIRST est WHERE est.company EQ quotehd.company
         AND est.est-no  EQ quotehd.est-no
         NO-LOCK NO-ERROR.

IF NOT ch-multi AND quotehd.est-no <> "" THEN
for each quotehd
    where quotehd.company eq quotehd.company
      and quotehd.cust-no ge fcust
      and quotehd.cust-no le tcust
      and quotehd.q-no    ge fquote
      and quotehd.q-no    le tquote
    no-lock,

    first quoteitm OF quotehd no-lock,

    first est
    where est.company eq quotehd.company
      AND est.est-no  EQ quotehd.est-no
    no-lock,

    first sman
    where sman.company eq quotehd.company
      and sman.sman    eq quotehd.sman
    no-lock,

    first carrier
    where carrier.company eq quotehd.company
      and carrier.carrier eq quotehd.carrier
    no-lock,

    first terms
    where terms.company eq quotehd.company
      and terms.t-code  eq quotehd.terms
    no-lock,

    first cust
    where cust.company eq quotehd.company
      and cust.cust-no eq quotehd.cust-no
    no-lock

    transaction:
  
  create report.
  assign
   report.term-id = v-term
   report.key-01  = quotehd.cust-no
   report.key-02  = if ch-sort eq "E" then quotehd.est-no   else
                    if ch-sort eq "C" then quoteitm.part-no else ""
   report.key-03  = string(quotehd.q-no,"9999999999")
   report.rec-id  = recid(quotehd).
end.

ELSE    
for each quotehd
    where quotehd.company eq quotehd.company
      and quotehd.cust-no ge fcust
      and quotehd.cust-no le tcust
      and quotehd.q-no    ge fquote
      and quotehd.q-no    le tquote
    no-lock,

    first quoteitm OF quotehd no-lock,
    /*
    first est
    where est.company eq quotehd.company
      AND est.est-no  EQ quotehd.est-no
    no-lock,
    */
    first sman
    where sman.company eq quotehd.company
      and sman.sman    eq quotehd.sman
    no-lock,

    first carrier
    where carrier.company eq quotehd.company
      and carrier.carrier eq quotehd.carrier
    no-lock,

    first terms
    where terms.company eq quotehd.company
      and terms.t-code  eq quotehd.terms
    no-lock,

    first cust
    where cust.company eq quotehd.company
      and cust.cust-no eq quotehd.cust-no
    NO-LOCK
    transaction:
   
    IF quotehd.est-no <> "" THEN DO:
       FIND first est where est.company eq quotehd.company
                   AND est.est-no  EQ quotehd.est-no nO-LOCK NO-ERROR.
       IF NOT AVAIL est THEN  NEXT.
    END.

  create report.
  assign
   report.term-id = v-term
   report.key-01  = quotehd.cust-no
   report.key-02  = if ch-sort eq "E" then quotehd.est-no   else
                    if ch-sort eq "C" then quoteitm.part-no else ""
   report.key-03  = string(quotehd.q-no,"9999999999")
   report.rec-id  = recid(quotehd).
end.


ASSIGN
 v-term-id        = v-term
 v-lines-per-page = lines-per-page.
/*
IF is-xprint-form AND rd-dest = 2 THEN PUT "<PREVIEW>".
ELSE  IF is-xprint-form AND rd-dest = 1 THEN PUT "<PRINTER?>".
*/
IF IS-xprint-form THEN DO:
    CASE rd-dest:
        WHEN 1 THEN PUT  "<PRINTER?>".
        WHEN 2 THEN PUT "<PREVIEW>".        
        WHEN  4 THEN do:
              ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                  /*(IF is-xprint-form THEN ".xpr" ELSE ".txt").*/
              PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".
        END.
        WHEN 5 THEN PUT "<PREVIEW><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(60)".
    END CASE.
END.
RUN value(v-program).
for each report where report.term-id eq v-term-id:
  delete report.
end.

ASSIGN
   tb_note:SCREEN-VALUE = "YES"
   rd_sort:SCREEN-VALUE = "Quote#"
   tb_note
   rd_sort.

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
  PAGE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

