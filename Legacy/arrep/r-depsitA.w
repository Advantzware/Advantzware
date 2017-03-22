&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: arrep\r-depsit.w

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
DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

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

/*{sys/inc/custlistform.i ""AR8"" }*/

{sys/ref/CustList.i NEW}

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.

DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

DEF VAR lv-check-no LIKE ar-cash.check-no NO-UNDO.
DEF VAR lv-inv-no   LIKE ar-cashl.inv-no NO-UNDO.
DEF VAR lv-amt      LIKE ar-cash.check-amt NO-UNDO.
DEF VAR lv-cust-no  LIKE cust.cust-no NO-UNDO.
DEF VAR lv-name     LIKE cust.name NO-UNDO.

DEF TEMP-TABLE tt-gltrans NO-UNDO LIKE gltrans
    FIELD VOID AS LOG.

DEF STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date begin_bank ~
end_bank tb_cust-list btnCustList begin_cust end_cust tb_det rd-dest ~
lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_bank end_bank ~
tb_cust-list begin_cust end_cust tb_det rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel /*AUTO-END-KEY */
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

DEFINE VARIABLE begin_bank AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Bank" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Cust#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_bank AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Bank" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Cust#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-depsit.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.57.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.8 BY .95 NO-UNDO.

DEFINE VARIABLE tb_det AS LOGICAL INITIAL no 
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

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
     begin_date AT ROW 3.38 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 3.38 COL 64 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_bank AT ROW 4.62 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Bank"
     end_bank AT ROW 4.62 COL 64 COLON-ALIGNED HELP
          "Enter Ending Bank"
     tb_cust-list AT ROW 6.14 COL 29.4 WIDGET-ID 6
     btnCustList AT ROW 6.24 COL 63 WIDGET-ID 8
     begin_cust AT ROW 7.43 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 7.43 COL 64 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     tb_det AT ROW 9.05 COL 41
     rd-dest AT ROW 11.91 COL 7 NO-LABEL
     lv-ornt AT ROW 12.19 COL 31 NO-LABEL
     lines-per-page AT ROW 12.19 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 13.62 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 14.57 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.76 COL 30.2
     tb_excel AT ROW 17.91 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 17.91 COL 71 RIGHT-ALIGNED
     fi_file AT ROW 18.71 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 20.76 COL 19
     btn-cancel AT ROW 20.76 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.48 COL 3
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11 COL 3
     RECT-6 AT ROW 10.76 COL 1
     RECT-7 AT ROW 1.95 COL 1
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
         TITLE              = "Bank Deposit Report"
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
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_bank:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_bank:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_det:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Bank Deposit Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Bank Deposit Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bank C-Win
ON LEAVE OF begin_bank IN FRAME FRAME-A /* Beginning Bank */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Cust# */
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
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
      EMPTY TEMP-TABLE ttCustList.
      RUN BuildCustList(INPUT cocode,
                        INPUT tb_cust-list AND glCustListActive,
                        INPUT begin_cust,
                        INPUT END_cust).
 END.

  run run-report. 
  STATUS DEFAULT "Processing Complete".
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_bank
                            &END_cust=END_bank
                            &fax-subject="Bank Deposit List"
                            &fax-body="Bank Deposit List"
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_bank
                             &END_cust=end_bank
                             &mail-subject="Bank Deposit List"
                             &mail-body="Bank Deposit List"
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_bank
                                  &END_cust=end_bank
                                  &mail-subject="Bank Deposit List"
                                  &mail-body="Bank Deposit List"
                                  &mail-file=list-name }
           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
  SESSION:SET-WAIT-STATE ("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_bank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bank C-Win
ON LEAVE OF end_bank IN FRAME FRAME-A /* Ending Bank */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Cust# */
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

    RUN WINDOWS/l-fonts.w ({&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON HELP OF begin_cust IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON HELP OF end_cust IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

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


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
  assign {&self-name}.
  EMPTY TEMP-TABLE ttCustList.
  RUN SetCustRange(INPUT tb_cust-list).
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

  assign
   begin_date = date(1,1,year(today))
   end_date   = today.

  find first bank where bank.company eq gcompany no-lock no-error.
  if avail bank then
    assign
     begin_bank = bank.bank-code
     end_bank   = bank.bank-code.

  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "AR8",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_bank.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'AR8',
                          INPUT NO,
                          OUTPUT glCustListActive).

 {sys/inc/chblankcust.i ""AR8""}

  IF ou-log THEN DO:
      ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
        tb_cust-list = YES 
        .
      RUN SetCustRange(INPUT tb_cust-list).
  END.
  ELSE
      ASSIGN
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        .
  IF ou-log AND ou-cust-int = 0 THEN do:
       ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
        tb_cust-list = NO
        .
      RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
   END.


  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildCustList C-Win 
PROCEDURE BuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Builds the temp table of customers   
  Parameters:  Company Code, Customer list logical and/or customer range
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplList AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginCust AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndCust AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-cust FOR cust.

DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.

IF iplList THEN DO:
    RUN sys/ref/CustList.p (INPUT ipcCompany,
                            INPUT 'AR8',
                            INPUT YES,
                            OUTPUT lActive).
END.
ELSE DO:
    FOR EACH bf-cust
        WHERE bf-cust.company EQ ipcCompany
          AND bf-cust.cust-no GE ipcBeginCust
          AND bf-cust.cust-no LE ipcEndCust
        NO-LOCK:
        CREATE ttCustList.
        ASSIGN 
            ttCustList.cust-no = bf-cust.cust-no
            ttCustList.log-fld = YES
        .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
/*------------------------------------------------------------------------------
  Purpose:  Display a UI of selected customers   
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
                                  INPUT 'AR8').


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY begin_date end_date begin_bank end_bank tb_cust-list begin_cust 
          end_cust tb_det rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date end_date begin_bank end_bank tb_cust-list 
         btnCustList begin_cust end_cust tb_det rd-dest lv-ornt lines-per-page 
         lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-detail-values C-Win 
PROCEDURE get-detail-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-jrnl LIKE gltrans.jrnl    NO-UNDO.
  DEF INPUT PARAM ip-dscr LIKE gltrans.tr-dscr NO-UNDO.
  DEF INPUT PARAM ip-amt  LIKE gltrans.tr-amt  NO-UNDO.
  DEF INPUT PARAM ip-void AS LOG               NO-UNDO.

  ASSIGN
   lv-check-no = 0
   lv-inv-no   = 0
   lv-amt      = 0
   lv-cust-no  = ""
   lv-name     = "".

  IF ip-jrnl EQ "CASHR" THEN DO:
     lv-check-no = INT(SUBSTR(ip-dscr,INDEX(ip-dscr," Inv# ") - 10,10)) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN lv-check-no = 0.

     lv-inv-no = INT(SUBSTR(ip-dscr,INDEX(ip-dscr," Inv# ") + 6,10)) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN lv-inv-no = 0.

     RELEASE ar-inv.
     IF lv-inv-no NE 0 THEN
     FIND FIRST ar-inv NO-LOCK
         WHERE ar-inv.company EQ cocode
           AND ar-inv.inv-no  EQ lv-inv-no
         NO-ERROR.

     IF lv-check-no NE 0 THEN
     FOR EACH ar-cash FIELDS(company cust-no c-no) NO-LOCK
         WHERE ar-cash.company  EQ cocode
           AND ar-cash.memo     EQ NO
           AND ar-cash.check-no EQ lv-check-no
           AND (NOT AVAIL ar-inv OR ar-cash.cust-no EQ ar-inv.cust-no),
         FIRST cust FIELDS(NAME)
         WHERE cust.company EQ ar-cash.company
           AND cust.cust-no EQ ar-cash.cust-no
         NO-LOCK:

       ASSIGN
        lv-cust-no = ar-cash.cust-no
        lv-name    = cust.name.

       IF lv-inv-no EQ 0 OR ip-void THEN lv-amt = ip-amt.

       ELSE
       FOR EACH ar-cashl FIELDS(amt-paid)
           WHERE ar-cashl.c-no   EQ ar-cash.c-no
             AND ar-cashl.inv-no EQ lv-inv-no
           NO-LOCK:

         lv-amt = ar-cashl.amt-paid.
         LEAVE.
       END.
     END.
  END.

  ELSE DO:      
     FIND FIRST ar-mcash WHERE ar-mcash.m-no EQ INT(ip-dscr) NO-LOCK NO-ERROR.
     IF AVAIL ar-mcash THEN
        ASSIGN
           lv-check-no = ar-mcash.m-no
           lv-name     = ar-mcash.payer
           lv-amt      = ar-mcash.check-amt.
  END.

  IF lv-name EQ "" THEN lv-name = lv-cust-no.

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
/* ---------------------------------------------- gl/rep/gl-recon.p 06/97 FWK */
/* Bank Statement                                                             */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.f}

def var v-s-date as date format "99/99/9999" no-undo label "From Date" init ?.
def var v-e-date as date format "99/99/9999" no-undo label "To Date" init TODAY.
def var v-s-bank like bank.bank-code label "From Bank" no-undo.
def var v-e-bank like bank.bank-code label "To Bank" no-undo.
def var date-loop as DATE NO-UNDO.
def var tot-daily as dec format "->>,>>>,>>9.99" init 0 EXTENT 2 no-undo.
def var bank-tot as dec format "->>,>>>,>>9.99" init 0 no-undo.
def var dep-tot as dec format "->>,>>>,>>9.99" init 0 no-undo.
def var v-frst as log init yes no-undo.
def var v-date as date no-undo.
DEF VAR v-tr-num AS CHAR FORMAT "x(26)" NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR v-void AS LOG NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.
DEF VAR fcust AS CHAR NO-UNDO.
DEF VAR tcust AS CHAR NO-UNDO.
FORM
    bank.bank-code AT 1 COLUMN-LABEL "BANK"
    bank.actnum AT 10 FORMAT "x(20)" COLUMN-LABEL "ACCOUNT NUMBER"
    v-date AT 31 FORMAT "99/99/99" COLUMN-LABEL "DATE"
    v-tr-num AT 40 COLUMN-LABEL "TRANSACTION#"
    tot-daily[2] TO 80 COLUMN-LABEL "DAILY TOTAL"
    WITH FRAME day-log NO-BOX STREAM-IO WIDTH 80 DOWN.

SESSION:SET-WAIT-STATE ("general").

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 56}

 v-s-date = begin_date
 v-e-date = end_date
 v-s-bank = begin_bank
 v-e-bank = end_bank
 fcust    = begin_cust
 tcust    = END_cust
 lSelected = tb_cust-list.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   excelheader = "BANK,ACCT/CUST NUMBER,DATE,TRANSACTION#,DAILY TOTAL".
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

IF lselected THEN DO:
    FIND FIRST ttCustList NO-LOCK WHERE ttCustList.log-fld EQ TRUE NO-ERROR.
    IF AVAIL ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
    FIND LAST ttCustList NO-LOCK WHERE ttCustList.log-fld EQ TRUE NO-ERROR.
    IF AVAIL ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
END.

if td-show-parm then run show-param.

display str-tit with frame r-top.

IF tb_det THEN
DO WITH FRAME day-log:
   bank.actnum:LABEL = "ACCT/CUST NUMBER".
END.

SESSION:SET-WAIT-STATE ("general").

EMPTY TEMP-TABLE tt-gltrans.

FOR EACH ar-cash NO-LOCK
    WHERE ar-cash.company   EQ cocode
      AND ar-cash.posted    EQ YES
      AND ar-cash.bank-code GE v-s-bank
      AND ar-cash.bank-code LE v-e-bank
      AND ar-cash.cust-no   GE fcust
      AND ar-cash.cust-no   LE tcust
    :
  IF lselected AND
     NOT CAN-FIND(FIRST ttCustList
                  WHERE ttCustList.cust-no EQ ar-cash.cust-no
                    AND ttCustList.log-fld) THEN NEXT.

  FOR EACH ar-cashl NO-LOCK
      WHERE ar-cashl.c-no EQ ar-cash.c-no
      :
      {custom/statusMsg.i " 'Processing Bank#  '  + ar-cash.bank-code "}
      v-void = CAN-FIND(FIRST reftable
                        WHERE reftable.reftable = "ARCASHLVDDATE"
                          AND reftable.rec_key = ar-cashl.rec_key
                        USE-INDEX rec_key).

      IF NOT v-void THEN
        FIND FIRST ar-ledger NO-LOCK
             WHERE ar-ledger.company EQ ar-cash.company
               AND ar-ledger.cust-no EQ ar-cash.cust-no
               AND ar-ledger.ref-num EQ "CHK# " + STRING(ar-cash.check-no,"9999999999")
               AND ar-ledger.tr-date GE v-s-date
               AND ar-ledger.tr-date LE v-e-date
             NO-ERROR.
      ELSE
        FIND FIRST ar-ledger NO-LOCK
             WHERE ar-ledger.company EQ ar-cash.company
               AND ar-ledger.cust-no EQ ar-cash.cust-no
               AND ar-ledger.ref-num EQ "VOIDED CHK# " + STRING(ar-cash.check-no,"9999999999")
               AND ar-ledger.tr-date GE v-s-date
               AND ar-ledger.tr-date LE v-e-date
             NO-ERROR.

      IF NOT AVAIL ar-ledger THEN NEXT.

      CREATE tt-gltrans.
      ASSIGN
        tt-gltrans.company = ar-cash.company
        tt-gltrans.actnum  = ar-cash.bank-code
        tt-gltrans.jrnl    = "CASHR"
        tt-gltrans.tr-dscr = ar-cash.cust-no + " " +
                             STRING(ar-cash.check-no,"9999999999") +
                             " Inv# " + STRING(ar-cashl.inv-no)
        tt-gltrans.tr-date = ar-ledger.tr-date
        tt-gltrans.tr-amt  = ar-cashl.amt-paid
        tt-gltrans.trnum   = ar-ledger.tr-num
        tt-gltrans.VOID    = v-void
        .
      RELEASE tt-gltrans.
  END. /* each ar-cashl */
END.

IF TRIM(begin_cust) EQ "" AND
   end_cust EQ "zzzzzzzz" AND NOT lselected THEN
   FOR EACH ar-mcash
      WHERE ar-mcash.company   EQ cocode
         AND ar-mcash.posted    EQ YES
         AND ar-mcash.bank-code GE v-s-bank
         AND ar-mcash.bank-code LE v-e-bank
       NO-LOCK,
       FIRST ar-ledger
       WHERE ar-ledger.company EQ ar-mcash.company
         AND ar-ledger.cust-no EQ ""
         AND ar-ledger.ref-num EQ STRING(ar-mcash.m-no) + " " + ar-mcash.payer
         AND INDEX(ar-ledger.ref-num,ar-mcash.payer) GT 0
         AND ar-ledger.tr-date GE v-s-date
         AND ar-ledger.tr-date LE v-e-date
       NO-LOCK:
          {custom/statusMsg.i " 'Processing Bank#  '  + ar-mcash.bank-code "}
       CREATE tt-gltrans.
       assign
        tt-gltrans.company = ar-mcash.company
        tt-gltrans.actnum  = ar-mcash.bank-code
        tt-gltrans.jrnl    = "MCSHREC"
        tt-gltrans.tr-dscr = string(ar-mcash.m-no)
        tt-gltrans.tr-date = ar-ledger.tr-date
        tt-gltrans.tr-amt  = ar-ledger.amt
        tt-gltrans.trnum   = ar-ledger.tr-num.
       RELEASE tt-gltrans.
END.

FOR EACH bank
    WHERE bank.company   EQ cocode
      AND bank.bank-code GE v-s-bank
      AND bank.bank-code LE v-e-bank
    NO-LOCK                     
    BREAK BY bank.bank-code:

  assign
   v-frst   = yes
   bank-tot = 0.

  do date-loop = v-s-date to v-e-date:
    for each tt-gltrans
        where tt-gltrans.company eq bank.company
          and tt-gltrans.actnum  eq bank.bank-code
          and tt-gltrans.tr-date eq date-loop
        no-lock

        break by tt-gltrans.tr-date
              BY tt-gltrans.trnum
              BY tt-gltrans.tr-dscr:

        {custom/statusMsg.i " 'Processing Bank#  '  + tt-gltrans.actnum "}

      IF v-frst THEN
      DO:
        display bank.bank-code when v-frst
                bank.actnum when v-frst

            with frame day-log.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' (IF v-frst THEN bank.bank-code ELSE "") '",'
              '"' (IF v-frst THEN bank.actnum ELSE "")    '",'
              SKIP.
      END.

      IF tb_det THEN DO:
         IF v-frst THEN DOWN WITH FRAME day-log.

         RUN get-detail-values (tt-gltrans.jrnl, tt-gltrans.tr-dscr, tt-gltrans.tr-amt,
                                tt-gltrans.VOID).

         IF lv-amt EQ 0 THEN lv-amt = tt-gltrans.tr-amt.

         IF lv-check-no NE 0 THEN DO:
           DISPLAY lv-name     @ bank.actnum
                   "Chk " + TRIM(STRING(lv-check-no,">>>>>>>>>>")) + " " +
                   (IF lv-inv-no NE 0 THEN
                      "Inv " + TRIM(STRING(lv-inv-no,">>>>>>>")) ELSE "")
                               @ v-tr-num
                   lv-amt      @ tot-daily[2]

               WITH FRAME day-log.
           DOWN WITH FRAME day-log.

           IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 '"' ""      '",'
                 '"' lv-name '",'
                 '"' ""      '",'
                 '"' "Chk " + TRIM(STRING(lv-check-no,">>>>>>>>>>")) + " " +
                     (IF lv-inv-no NE 0 THEN
                     "Inv " + TRIM(STRING(lv-inv-no,">>>>>>>")) ELSE "")  '",'
                 '"' STRING(lv-amt,"->>,>>>,>>9.99") '",'
                SKIP.
         END.
      END.

      ASSIGN
         v-frst = NO
         tot-daily[1] = tot-daily[1] + tt-gltrans.tr-amt.

      IF LAST-OF(tt-gltrans.trnum) THEN DO:
        IF tb_det THEN
        DO:
          UNDERLINE tot-daily[2] WITH FRAME day-log.

          if tb_excel then
            PUT STREAM excel UNFORMATTED
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "----------------------" '",'
               SKIP.
        END.

        display tt-gltrans.tr-date @ v-date
                TRIM(STRING(tt-gltrans.trnum,">>>>>>>>>>")) @ v-tr-num
                tot-daily[1] @ tot-daily[2]

            with frame day-log.
        down with frame day-log.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' "" '",'
              '"' "" '",'
              '"' (IF tt-gltrans.tr-date NE ? THEN STRING(tt-gltrans.tr-date)
                   ELSE "") '",'
              '"' TRIM(STRING(tt-gltrans.trnum,">>>>>>>>>>")) '",'
              '"' STRING(tot-daily[1],"->>,>>>,>>9.99") '",'
             SKIP.

        IF tb_det AND NOT LAST-OF(tt-gltrans.tr-date) THEN PUT SKIP(1). 

        ASSIGN
         tot-daily[2] = tot-daily[2] + tot-daily[1]
         tot-daily[1] = 0.
      END.

      if last-of(tt-gltrans.tr-date) then do:
        UNDERLINE tot-daily[2] WITH FRAME day-log.
        IF tb_det THEN DO:
          down with frame day-log.
          UNDERLINE tot-daily[2] WITH FRAME day-log.

          IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
                '"' ""           '",'
                '"' ""           '",'
                '"' ""           '",'
                '"' ""           '",'
                '"' "----------------------" '",'
                SKIP.
        END.

        display tot-daily[2]

            with frame day-log.
        down with frame day-log.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
               '"' ""           '",'
               '"' ""           '",'
               '"' ""           '",'
               '"' ""           '",'
               '"' STRING(tot-daily[2],"->>,>>>,>>9.99") '",'
              SKIP.

        PUT SKIP(1).

        assign
         bank-tot     = bank-tot + tot-daily[2]
         tot-daily[2] = 0.
      end.
    end.
  end. /* date-loop */

  if bank-tot ne 0 then
  DO:
    put skip
        "--------------"  to 80
        skip
        "Total Deposits"  at 40
        bank-tot          to 80
        skip(2).

    IF tb_excel THEN
    DO:
      PUT STREAM excel UNFORMATTED SKIP(1).
      PUT STREAM excel UNFORMATTED
          '"' ""               '",'
          '"' ""               '",'
          '"' ""               '",'
          '"' "Total Deposits" '",'
          '"' STRING(bank-tot,"->>,>>>,>>9.99") '",'
         SKIP(2).
    END.
  END.


  dep-tot = dep-tot + bank-tot.
end.

if dep-tot ne 0 then put "Total Deposits for All Banks" dep-tot to 80 skip.

IF tb_excel THEN
  PUT STREAM excel UNFORMATTED
       '"' "Total Deposits for All Banks"   '",'
       '"' ""                               '",'
       '"' ""                               '",'
       '"' ""                               '",'
       '"' STRING(dep-tot,"->>,>>>,>>9.99") '",'
      SKIP.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCustRange C-Win 
PROCEDURE SetCustRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
        begin_cust:SENSITIVE = NOT iplChecked
        end_cust:SENSITIVE = NOT iplChecked
        begin_cust:VISIBLE = NOT iplChecked
        end_cust:VISIBLE = NOT iplChecked
        btnCustList:SENSITIVE = iplChecked
       .
  END.

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
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + ",".
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

