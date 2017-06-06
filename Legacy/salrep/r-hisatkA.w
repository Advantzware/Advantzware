&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-hisatk.w

  Description: High Sales Tracking

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

/*{sys/inc/custlistform.i ""HR1"" }*/

{sys/ref/CustList.i NEW}

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD dec1 AS DEC FIELD dec2 AS DEC
    INDEX key-01 key-01 key-02
    INDEX key-02 key-02.

DEF TEMP-TABLE tt-report2 NO-UNDO LIKE report
    FIELD dec1 AS DEC FIELD dec2 AS DEC
    INDEX high-sales key-01 dec1 DESC dec2 DESC.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 as-of-date prt-period ~
srt-period cust rd_print tb_cust-list btnCustList begin_cust end_cust ~
begin_slsmn end_slsmn tb_prt-cust rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS as-of-date prt-period srt-period cust ~
lbl_sort rd_print tb_cust-list begin_cust end_cust begin_slsmn end_slsmn ~
tb_prt-cust rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel /*AUTO-END-KEY*/ 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "As Of Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Cust#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE cust AS INTEGER FORMAT ">>>>>" INITIAL 99999 
     LABEL "Customers To Print" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Cust#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-hisatk.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

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

DEFINE VARIABLE prt-period AS INTEGER FORMAT "->9" INITIAL 0 
     LABEL "Periods To Print" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE srt-period AS INTEGER FORMAT "->9" INITIAL 0 
     LABEL "Sort By Period" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

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

DEFINE VARIABLE rd_print AS CHARACTER INITIAL "Customer" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer", "Customer",
"Salesrep", "Salesrep"
     SIZE 34 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 12.62.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_prt-cust AS LOGICAL INITIAL yes 
     LABEL "Print Customers w/Zero Balance?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

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
     as-of-date AT ROW 2.43 COL 32 COLON-ALIGNED HELP
          "Enter As Of Date"
     prt-period AT ROW 3.62 COL 32 COLON-ALIGNED HELP
          "Enter Periods to Print"
     srt-period AT ROW 4.81 COL 32 COLON-ALIGNED HELP
          "Enter Periods to Sort By"
     cust AT ROW 6 COL 32 COLON-ALIGNED HELP
          "Customers To Print"
     lbl_sort AT ROW 7.19 COL 21 COLON-ALIGNED NO-LABEL
     rd_print AT ROW 7.19 COL 34 NO-LABEL
     tb_cust-list AT ROW 8.33 COL 31.8 WIDGET-ID 6
     btnCustList AT ROW 8.33 COL 64.2 WIDGET-ID 8
     begin_cust AT ROW 9.48 COL 24.6 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 9.48 COL 63.8 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_slsmn AT ROW 10.71 COL 24.6 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number" WIDGET-ID 6
     end_slsmn AT ROW 10.71 COL 63.8 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number" WIDGET-ID 8
     tb_prt-cust AT ROW 12.14 COL 31
     rd-dest AT ROW 15.19 COL 5 NO-LABEL
     lv-ornt AT ROW 15.67 COL 31 NO-LABEL
     lines-per-page AT ROW 15.67 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 17.57 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 18.52 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.71 COL 30
     tb_excel AT ROW 21 COL 50.2 RIGHT-ALIGNED
     tb_runExcel AT ROW 21 COL 71.2 RIGHT-ALIGNED
     fi_file AT ROW 21.81 COL 28.2 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 24.29 COL 19
     btn-cancel AT ROW 24.29 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "(Enter 99 For YTD)" VIEW-AS TEXT
          SIZE 22 BY 1 AT ROW 4.81 COL 51
          FGCOLOR 9 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.24 COL 3
     RECT-6 AT ROW 13.76 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 25.05.


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
         TITLE              = "High Sales Tracking"
         HEIGHT             = 25.29
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
       as-of-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_print".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       prt-period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_print:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       srt-period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prt-cust:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* High Sales Tracking */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* High Sales Tracking */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME as-of-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as-of-date C-Win
ON LEAVE OF as-of-date IN FRAME FRAME-A /* As Of Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON HELP OF begin_cust IN FRAME FRAME-A /* Beginning Cust# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Cust# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Salesrep# */
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
                    INPUT tb_cust-list AND glCustListActive ,
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
           {custom/asifax.i &begin_cust=begin_slsmn
                            &END_cust=END_slsmn
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "salesman"
                             &begin_cust= begin_slsmn
                             &END_cust=end_slsmn
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Salesman"
                                  &begin_cust= begin_slsmn
                                  &END_cust=end_slsmn
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
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


&Scoped-define SELF-NAME cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust C-Win
ON LEAVE OF cust IN FRAME FRAME-A /* Customers To Print */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON HELP OF end_cust IN FRAME FRAME-A /* Ending Cust# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Cust# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Salesrep# */
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


&Scoped-define SELF-NAME prt-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prt-period C-Win
ON LEAVE OF prt-period IN FRAME FRAME-A /* Periods To Print */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME rd_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_print C-Win
ON VALUE-CHANGED OF rd_print IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME srt-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL srt-period C-Win
ON LEAVE OF srt-period IN FRAME FRAME-A /* Sort By Period */
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


&Scoped-define SELF-NAME tb_prt-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-cust C-Win
ON VALUE-CHANGED OF tb_prt-cust IN FRAME FRAME-A /* Print Customers w/Zero Balance? */
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

  find first period
     where period.company eq cocode
       and period.pst     le TODAY
       and period.pend    ge TODAY
     no-lock.  

  assign
   as-of-date = TODAY
   prt-period = 3
   srt-period = period.pnum
   cust       = 99999.

  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "HR1",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

   DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
/*
    ASSIGN
     as-of-date:SCREEN-VALUE = STRING(as-of-date)
     prt-period:SCREEN-VALUE   = STRING(prt-period)
     srt-period:SCREEN-VALUE = STRING(srt-period)
     cust:SCREEN-VALUE   = STRING(cust) 
     rd_print:SCREEN-VALUE = STRING(rd_print) 
     END_slmn:SCREEN-VALUE   = STRING(end_slmn) 
     begin_slmn:SCREEN-VALUE = STRING(begin_slmn)
     .
  */
   APPLY "entry" TO as-of-date IN FRAME {&FRAME-NAME}.
  END.
   RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'HR1',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""HR1""}

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
                            INPUT 'HR1',
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
                                  INPUT 'HR1').


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
  DISPLAY as-of-date prt-period srt-period cust lbl_sort rd_print tb_cust-list 
          begin_cust end_cust begin_slsmn end_slsmn tb_prt-cust rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 as-of-date prt-period srt-period cust rd_print 
         tb_cust-list btnCustList begin_cust end_cust begin_slsmn end_slsmn 
         tb_prt-cust rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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
 /*    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY. */

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
/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var fdate     as   date extent 20 NO-UNDO.
def var tdate     as   date extent 20 NO-UNDO.
def var fsman     like cust.sman            init "" NO-UNDO.
def var tsman     like fsman                init "zzz" NO-UNDO.
def var fcus     like cust.cust-no            init "" NO-UNDO.
def var tcus     like cust.cust-no            init "" NO-UNDO.
def var v-date    as   date format "99/99/9999" NO-UNDO.
def var v-per-1   as   int                  format ">9" NO-UNDO.
def var v-per-2   as   int                  format ">9" NO-UNDO.
def var v-custs   as   int                  format ">>,>>9" NO-UNDO.
def var v-sort    as   log      init yes    format "Customer/Salesrep" NO-UNDO.
def var v-inc     as   log      init YES NO-UNDO.

def var v-amt1    as   DEC NO-UNDO.
def var v-slsm    like ar-invl.sman  extent 1 NO-UNDO.
def var v-slsp    like ar-invl.s-pct extent 1 NO-UNDO.

def var v-amt     as   dec  extent 21 NO-UNDO.
def var v-tot-amt as   dec  extent 21 NO-UNDO.
def var v-label   as   char extent 4 format "x(17)" NO-UNDO.

def var v-yr      like period.yr NO-UNDO.
def var v         as   INT NO-UNDO.
def var v1        as   INT NO-UNDO.
def var v-ytd     as   LOG NO-UNDO.
def var v-prt     as   INT NO-UNDO.
def var v-j       as   DEC NO-UNDO.
DEF VAR lv-sman   AS   CHAR NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.
DEFINE VARIABLE lp-zero AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE d-gr-tot-amt as   DECIMAL  EXTENT 21 NO-UNDO.

FORM HEADER
     "Salesrep:"
     lv-sman        FORMAT "x(40)"

    WITH PAGE-TOP FRAME r-top2 STREAM-IO WIDTH 180.

form cust.cust-no
     cust.name
     v-amt[17]                                      format "->,>>>,>>>,>>9.99"
     v-amt[18]                                      format "->,>>>,>>>,>>9.99"
     v-amt[19]                                      format "->,>>>,>>>,>>9.99"
     v-amt[20]                                      format "->,>>>,>>>,>>9.99"
     v-amt[21]      column-label "YTD"              format "->,>>>,>>>,>>9.99"

    header skip(1)
           "Customer Name                          "
           v-label[1]
           v-label[2]
           v-label[3]
           v-label[4]
           "          YTD Amt"                                          skip

    with no-box no-labels frame custx down STREAM-IO width 180.


SESSION:SET-WAIT-STATE ("general").

EMPTY TEMP-TABLE tt-report.
EMPTY TEMP-TABLE tt-report2.

v-date = today.

find first company where company.company eq cocode no-lock.

find first period
    where period.company eq cocode
      and period.pst     le v-date
      and period.pend    ge v-date
    no-lock.

assign
 v-per-1 = 3
 v-per-2 = period.pnum
 v-custs = 99999
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}
 v-date     = as-of-date
 v-per-1    = prt-period
 v-per-2    = srt-period
 v-custs    = cust
 v-sort     = rd_print EQ "Customer"
 fsman      = begin_slsmn
 tsman      = end_slsmn
 v-inc      = tb_prt-cust
 lSelected  = tb_cust-list
 fcus       =  begin_cust
 tcus       =  END_cust.

find last period
    where period.company eq cocode
      and period.pst     le v-date
      and period.pend    ge v-date
    no-lock.

v-yr = period.yr.

if v-per-2 eq 99 then do:
  find last period
      where period.company eq cocode
        and period.yr      eq v-yr
        and period.pnum    le v-per-1
      no-lock.

  assign
   v-ytd   = yes
   v-per-2 = period.pnum.
end.

assign
 v-date = period.pend
 v1     = v-per-2 - v-per-1 + 1.

if v1 lt 1 then v1 = 1.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Customer,Name".
END.

for each period
    where period.company eq cocode
      and period.yr      eq v-yr
      and period.pnum    le v-per-2
    no-lock:

  assign
   fdate[period.pnum] = period.pst
   tdate[period.pnum] = period.pend.

  if period.pnum ge v1 then do:
    assign
     v          = v + 1
     v-label[v] = v-label[v] +
                  (if v-label[v] eq "" then "Period " else "/") +
                  trim(string(period.pnum,">9")).

    IF tb_excel THEN
       excelheader = excelheader + ",Period " + STRING(period.pnum,">9").

    if v ge 4 then v = 0.
  end.
end.

do i = 1 to 4:
   v-label[i] = fill(" ",17 - length(trim(v-label[i]))) + trim(v-label[i]).
end.
IF lselected THEN DO:
      FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
      IF AVAIL ttCustList THEN ASSIGN fcus = ttCustList.cust-no .
      FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
      IF AVAIL ttCustList THEN ASSIGN tcus = ttCustList.cust-no .
END.

IF tb_excel THEN
DO:
   excelheader = excelheader + ",YTD Amt".
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.
  
FOR EACH ar-inv
    WHERE ar-inv.company  EQ cocode
      AND ar-inv.cust-no  GE fcus
      AND ar-inv.cust-no  LE tcus
      AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-inv.cust-no
      AND ttCustList.log-fld no-lock) else true) 
      AND ar-inv.inv-date GE fdate[1]
      AND ar-inv.inv-date LE v-date
      AND ar-inv.posted   EQ YES
    USE-INDEX inv-date NO-LOCK,
    FIRST cust
    WHERE cust.company EQ ar-inv.company
      AND cust.cust-no EQ ar-inv.cust-no
    NO-LOCK:

  for each ar-invl
      where ar-invl.x-no    eq ar-inv.x-no
        and (ar-invl.billable or not ar-invl.misc)
      no-lock:

      {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
    do i = 1 to 3:
      assign
       v-amt     = 0
       v-slsm[1] = if ar-invl.sman[i] eq "" and i eq 1 AND NOT ar-invl.misc then
                     cust.sman else ar-invl.sman[i].

      if v-slsm[1]   lt fsman                         or
         v-slsm[1]   gt tsman                         or
         (i ne 1 and
          (v-slsm[1] eq "" or ar-invl.s-pct[i] eq 0)) then next.

      assign
       v-slsp[1] = if ar-invl.sman[i] eq ""              or
                      (ar-invl.s-pct[i] eq 0 and i eq 1) then 100
                   else ar-invl.s-pct[i]
       v-amt1    = ar-invl.amt * v-slsp[1] / 100.

      if ar-inv.inv-date ge fdate[v-per-2] and
         ar-inv.inv-date le tdate[v-per-2] then
        v-amt[1] = v-amt[1] + v-amt1.

      v-amt[2] = v-amt[2] + v-amt1.

      create tt-report.
      assign
       tt-report.key-01  = if v-sort then "" else v-slsm[1]
       tt-report.key-02  = cust.cust-no
       tt-report.dec1    = v-amt[1]
       tt-report.dec2    = v-amt[2]
       tt-report.key-10  = "ar-invl"
       tt-report.rec-id  = recid(ar-invl).
       RELEASE tt-report.
    end.
  end.
end.
FOR each cust where cust.company eq cocode
      AND cust.cust-no  GE fcus
      AND cust.cust-no  LE tcus
      AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
      AND ttCustList.log-fld no-lock) else true) no-lock,

    each ar-cash
    where ar-cash.company    eq cocode
      and ar-cash.cust-no    eq cust.cust-no
      and ar-cash.check-date ge fdate[1]
      and ar-cash.check-date le v-date
      and ar-cash.posted     eq YES
    use-index ar-cash no-lock,

    EACH ar-cashl
    WHERE ar-cashl.c-no    EQ ar-cash.c-no
      AND ar-cashl.posted  EQ YES
      AND ar-cashl.memo    EQ YES
      AND CAN-FIND(FIRST account
                   WHERE account.company EQ ar-cashl.company
                     AND account.actnum  EQ ar-cashl.actnum
                     AND account.type    EQ "R")
    NO-LOCK:
  {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
  release ar-invl.
 
  RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

  if avail oe-retl then
  find first ar-invl
      where ar-invl.company eq cocode
        and ar-invl.cust-no eq cust.cust-no
        and ar-invl.inv-no  eq ar-cashl.inv-no
        and ar-invl.i-no    eq oe-retl.i-no
        and (ar-invl.billable or not ar-invl.misc)
      no-lock no-error.

  do i = 1 to 3:
     assign
      v-amt     = 0
      v-slsm[1] = if (not avail ar-invl)                or
                     (ar-invl.sman[i] eq "" and i eq 1) then
                    cust.sman else ar-invl.sman[i].

     if v-slsm[1]   lt fsman                         or
        v-slsm[1]   gt tsman                         or
        (i ne 1 and
         (v-slsm[1] eq "" or ar-invl.s-pct[i] eq 0)) then next.

     assign
      v-slsp[1] = if (not avail ar-invl)                or
                     ar-invl.sman[i] eq ""              or
                     (ar-invl.s-pct[i] eq 0 and i eq 1) then 100
                  else ar-invl.s-pct[i]
      v-amt1    = (ar-cashl.amt-paid - ar-cashl.amt-disc) *
                  v-slsp[1] / 100.

     if ar-cash.check-date ge fdate[v-per-2] and
        ar-cash.check-date le tdate[v-per-2] then
       v-amt[1] = v-amt[1] + v-amt1.

     v-amt[2] = v-amt[2] + v-amt1.

     create tt-report.
     assign
      tt-report.key-01  = if v-sort then "" else v-slsm[1]
      tt-report.key-02  = cust.cust-no
      tt-report.dec1    = v-amt[1]
      tt-report.dec2    = v-amt[2]
      tt-report.key-10  = "ar-cashl"
      tt-report.rec-id  = recid(ar-cashl).
     RELEASE tt-report.

     if not avail ar-invl then leave.
  end.
end.

IF v-inc THEN
   FOR each cust where cust.company eq cocode 
      AND cust.cust-no  GE fcus
      AND cust.cust-no  LE tcus
      AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
      AND ttCustList.log-fld no-lock) else true) AND
       NOT CAN-FIND(FIRST tt-report WHERE
           tt-report.key-02 EQ cust.cust-no)
       NO-LOCK:

       CREATE tt-report.
       ASSIGN
          tt-report.key-01  = if v-sort then "" else cust.sman
          tt-report.key-02  = cust.cust-no.
       RELEASE tt-report.
   END.


v-amt = 0.

for each tt-report no-lock

    break by tt-report.key-01
          by tt-report.key-02:

  assign
   v-amt[1] = v-amt[1] + tt-report.dec1
   v-amt[2] = v-amt[2] + tt-report.dec2.

  if last-of(tt-report.key-02) then do:
     if v-amt[1] ne 0 or v-inc or v-ytd then do:
       create tt-report2.
       assign
        tt-report2.key-01 = tt-report.key-01
        tt-report2.key-02 = tt-report.key-02
        tt-report2.dec1   = if v-ytd then 0 else v-amt[1]
        tt-report2.dec2   = v-amt[2].
     end.

     v-amt = 0.
  end.
end.

for each tt-report2,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq tt-report2.key-02
    no-lock

    break by tt-report2.key-01
          by tt-report2.dec1 DESC
          by tt-report2.dec2 DESC

    with frame custx:
     {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
  if first-of(tt-report2.key-01) then do:
    v-prt = 0.

    IF FIRST(tt-report2.key-01) THEN VIEW FRAME r-top.

    if not v-sort then do:
      find first sman
          where sman.company eq cocode
            and sman.sman    eq tt-report2.key-01
          no-lock no-error.
      lv-sman = TRIM(tt-report2.key-01) + " " +
                TRIM(IF AVAIL sman THEN sman.sname ELSE "Not on file").

      IF FIRST(tt-report2.key-01) THEN
         VIEW FRAME r-top2.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             SKIP(1)
             '"' "Salesrep: " '",'
             '"' lv-sman      '",' SKIP(1).

      PAGE.
    END.
    ELSE
    IF FIRST(tt-report2.key-01) THEN PAGE.
  end.

  assign
   v-amt = 0
   v-prt = v-prt + 1.
  

  for each tt-report
      where tt-report.key-01 eq tt-report2.key-01
        and tt-report.key-02 eq tt-report2.key-02:

    if tt-report.key-10 eq "ar-invl" then do:
       find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock.
       find ar-inv  where ar-inv.x-no    eq ar-invl.x-no  no-lock.

       do v = v1 to v-per-2:
         if ar-inv.inv-date ge fdate[v] and
            ar-inv.inv-date le tdate[v] then
           v-amt[v] = v-amt[v] + dec(tt-report.dec2).
       end.

       v-amt[21] = v-amt[21] + dec(tt-report.dec2).
    end.

    else
    if tt-report.key-10 eq "ar-cashl" then do:
       find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
       find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

       do v = v1 to v-per-2:
         if ar-cash.check-date ge fdate[v] and
            ar-cash.check-date le tdate[v] then
           v-amt[v] = v-amt[v] + dec(tt-report.dec2).
       end.

       v-amt[21] = v-amt[21] + dec(tt-report.dec2).
    end.
  end.
  ASSIGN lp-zero = YES.
  DO i = 1 TO 21: 
      IF v-amt[i] NE 0 THEN
        ASSIGN lp-zero = NO.      
  END. 

IF v-inc OR (NOT v-inc AND (lp-zero EQ NO)) THEN DO:
  if v-prt le v-custs then do:
     v = 0.

     do i = v1 to v-per-2:
        v = v + 1.

        if v modulo 4 eq 0 or
           i eq v-per-2   then do:

          v-j = v / 4.

          {sys/inc/roundup.i v-j}

          assign
           j = v-j
           j = v1 + (4 * (j - 1)).

          display cust.cust-no          when v eq 4 or
                                             (i eq v-per-2 and v lt 4)
                  cust.name             when v eq 4 or
                                             (i eq v-per-2 and v lt 4)
                  v-amt[j]              when v-label[1] ne "" and
                                             j     le v-per-2     @ v-amt[17]
                  v-amt[j + 1]          when v-label[2] ne "" and
                                             j + 1 le v-per-2     @ v-amt[18]
                  v-amt[j + 2]          when v-label[3] ne "" and
                                             j + 2 le v-per-2     @ v-amt[19]
                  v-amt[j + 3]          when v-label[4] ne "" and
                                             j + 3 le v-per-2     @ v-amt[20]
                  v-amt[21]             when v eq 4 or
                                             (i eq v-per-2 and v lt 4).
          down.
        end.
     end.

     IF tb_excel THEN
     DO:
        PUT STREAM excel UNFORMATTED
             '"' cust.cust-no '",'
             '"' cust.name    '",'.

        do i = v1 to v-per-2:

           PUT STREAM excel UNFORMATTED
               '"' STRING(v-amt[i],"->,>>>,>>>,>>9.99") '",'.
        END.

        PUT STREAM excel UNFORMATTED
            '"' STRING(v-amt[21],"->,>>>,>>>,>>9.99")  '",'.
     END.

     put skip(1).
     IF tb_excel THEN
        PUT STREAM excel UNFORMATTED SKIP(1).
  end.

  do i = 1 to 21:
     v-tot-amt[i] = v-tot-amt[i] + v-amt[i].
  end.

  if last-of(tt-report2.key-01) or v-prt eq v-custs then do:
     v = 0.

     underline cust.name.
     if v-label[1] ne "" then underline v-amt[17].
     if v-label[2] ne "" then underline v-amt[18].
     if v-label[3] ne "" then underline v-amt[19].
     if v-label[4] ne "" then underline v-amt[20].
     underline v-amt[21].
     down.

     do i = v1 to v-per-2:
       v = v + 1.

       if v modulo 4 eq 0 or
          i eq v-per-2   then do:

         v-j = v / 4.

         {sys/inc/roundup.i v-j}

         assign
          j = v-j
          j = v1 + (4 * (j - 1)).

         display "Totals"              when v eq 4 or
                                            (i eq v-per-2 and v lt 4)
                                                                 @ cust.name
                 "Totals (Printed Customers)"
                                       when (not last-of(tt-report2.key-01))
                                        and (v eq 4 or
                                             (i eq v-per-2 and v lt 4))
                                                                 @ cust.name
                 v-tot-amt[j]          when v-label[1] ne "" and
                                            j     le v-per-2     @ v-amt[17]
                 v-tot-amt[j + 1]      when v-label[2] ne "" and
                                            j + 1 le v-per-2     @ v-amt[18]
                 v-tot-amt[j + 2]      when v-label[3] ne "" and
                                            j + 2 le v-per-2     @ v-amt[19]
                 v-tot-amt[j + 3]      when v-label[4] ne "" and
                                            j + 3 le v-per-2     @ v-amt[20]
                 v-tot-amt[21]         when v eq 4 or
                                            (i eq v-per-2 and v lt 4)
                                                                 @ v-amt[21].


         down.

       end.
     end.

     IF tb_excel THEN
     DO:
        PUT STREAM excel UNFORMATTED
          '"' ""                                               '",'
          '"' IF not last-of(tt-report2.key-01) THEN
                 "Totals (Printed Customers)"
              ELSE "Totals"                                    '",'.


        do i = v1 to v-per-2:
           PUT STREAM excel UNFORMATTED
             '"' STRING(v-tot-amt[i],"->,>>>,>>>,>>9.99")     '",'.
        END.

        PUT STREAM excel UNFORMATTED
          '"' STRING(v-tot-amt[21],"->,>>>,>>>,>>9.99")      '",'
          SKIP.

     END.

     DO i = 1 to 21:
      d-gr-tot-amt[i] = d-gr-tot-amt[i] + v-tot-amt[i].
     end.

     if last-of(tt-report2.key-01) then v-tot-amt = 0.
  end.
end.
END.
if not v-sort then do:
         PUT SPACE(9) "------------------------------ ----------------- ----------------- ----------------- ----------------- -----------------" SKIP.
         PUT space(9) "Grand Totals" SPACE(19)  d-gr-tot-amt[1] FORMAT "->,>>>,>>>,>>9.99" SPACE(1)
                d-gr-tot-amt[2] FORMAT "->,>>>,>>>,>>9.99" SPACE(1)
                d-gr-tot-amt[3] FORMAT "->,>>>,>>>,>>9.99" SPACE(1)
                d-gr-tot-amt[4] FORMAT "->,>>>,>>>,>>9.99" SPACE(1)
                d-gr-tot-amt[21]FORMAT "->,>>>,>>>,>>9.99" SKIP.
         PUT space(40) d-gr-tot-amt[5] FORMAT "->,>>>,>>>,>>9.99" SPACE(1)
                             d-gr-tot-amt[6] FORMAT "->,>>>,>>>,>>9.99" SPACE(1)
                             d-gr-tot-amt[7] FORMAT "->,>>>,>>>,>>9.99" SPACE(1)
                             d-gr-tot-amt[8] FORMAT "->,>>>,>>>,>>9.99" SPACE(1)   SKIP.
         PUT  space(40) d-gr-tot-amt[9] FORMAT "->,>>>,>>>,>>9.99" SPACE(1)
              d-gr-tot-amt[10] FORMAT "->,>>>,>>>,>>9.99" SPACE(1)
              d-gr-tot-amt[11] FORMAT "->,>>>,>>>,>>9.99" SPACE(1)   SKIP.


     IF tb_excel THEN
     DO:
        PUT STREAM excel UNFORMATTED
          '"' ""                                               '",'
          '"' /*IF not last-of(tt-report2.key-01) THEN
                 "Grand Totals (Printed Customers)"
              ELSE*/ "Grand Totals"                                    '",'.


        do i = v1 to v-per-2:
           PUT STREAM excel UNFORMATTED
             '"' STRING(d-gr-tot-amt[i],"->,>>>,>>>,>>9.99")     '",'.
        END.

        PUT STREAM excel UNFORMATTED
          '"' STRING(d-gr-tot-amt[21],"->,>>>,>>>,>>9.99")      '",'
          SKIP.

     END.
END.

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

