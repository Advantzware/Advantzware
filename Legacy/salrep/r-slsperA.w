&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-slsper.w

  Description: Sales by Period

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


{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.
DEF TEMP-TABLE tt-report LIKE report
    FIELD dec3 AS DEC FIELD date1 AS DATE.

DEFINE TEMP-TABLE tt-cust-sales 
   FIELD  cust-no        AS CHARACTER       
   FIELD op-zero-ty      AS LOGICAL    
   FIELD data-string-ty  AS CHARACTER
   FIELD excel-string-ty AS CHARACTER
   FIELD dTotSales       AS DECIMAL
   FIELD op-zero-ly      AS LOGICAL 
   FIELD data-string-ly  AS CHARACTER
   FIELD excel-string-ly AS CHARACTER   
    .

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR tyfdate     AS   DATE EXTENT 13 INIT 12/31/9999 NO-UNDO.
DEF VAR tytdate     AS   DATE EXTENT 13 INIT 12/31/9999 NO-UNDO.
DEF VAR lyfdate     AS   DATE EXTENT 13 INIT 12/31/9999 NO-UNDO.
DEF VAR lytdate     AS   DATE EXTENT 13 INIT 12/31/9999 NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tb_cust-list btnCustList ~
begin_cust-no end_cust-no as-of-date tb_last-year tb_prt-cust tb_tot-rnu ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust-no end_cust-no ~
as-of-date tb_last-year tb_prt-cust tb_tot-rnu rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel ~
fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-slsper.csv" 
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
     SIZE 93 BY 8.76.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 7.62.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_last-year AS LOGICAL INITIAL no 
     LABEL "Print Last Year Sales?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.2 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-cust AS LOGICAL INITIAL yes 
     LABEL "Print Customers w/Zero Balance?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_tot-rnu AS LOGICAL INITIAL no 
     LABEL "Sort by Total Revenue?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_cust-list AT ROW 1.57 COL 31 WIDGET-ID 6
     btnCustList AT ROW 1.62 COL 64 WIDGET-ID 8
     begin_cust-no AT ROW 2.91 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.91 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     as-of-date AT ROW 4 COL 28 COLON-ALIGNED HELP
          "Enter As Of Date"
     tb_last-year AT ROW 5.29 COL 29.8
     tb_prt-cust AT ROW 6.24 COL 29.8
     tb_tot-rnu AT ROW 7.29 COL 29.8 WIDGET-ID 10
     rd-dest AT ROW 9.76 COL 6 NO-LABEL
     lv-ornt AT ROW 10 COL 31 NO-LABEL
     lines-per-page AT ROW 10 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 11.91 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 12.86 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 14.1 COL 30
     tb_excel AT ROW 15.57 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 15.57 COL 71 RIGHT-ALIGNED
     fi_file AT ROW 16.38 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 18.57 COL 19
     btn-cancel AT ROW 18.57 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 8.81 COL 5
     RECT-6 AT ROW 9.1 COL 2
     RECT-7 AT ROW 1 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 19.57.


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
         TITLE              = "Sales Analysis - Sales by Period"
         HEIGHT             = 19.57
         WIDTH              = 95.8
         MAX-HEIGHT         = 19.81
         MAX-WIDTH          = 95.8
         VIRTUAL-HEIGHT     = 19.81
         VIRTUAL-WIDTH      = 95.8
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
       as-of-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_last-year:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prt-cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_tot-rnu:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sales Analysis - Sales by Period */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sales Analysis - Sales by Period */
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


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
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

  SESSION:SET-WAIT-STATE("general").

  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust-no,
                    INPUT end_cust-no).
  END.
  run run-report. 

  STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE(""). 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_cust-no
                            &END_cust=END_cust-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= begin_cust-no
                             &END_cust=end_cust-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer"
                                  &begin_cust= begin_cust-no
                                  &END_cust=end_cust-no
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case.
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


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
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


&Scoped-define SELF-NAME tb_last-year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_last-year C-Win
ON VALUE-CHANGED OF tb_last-year IN FRAME FRAME-A /* Print Last Year Sales? */
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


&Scoped-define SELF-NAME tb_tot-rnu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tot-rnu C-Win
ON VALUE-CHANGED OF tb_tot-rnu IN FRAME FRAME-A /* Sort by Total Revenue? */
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
     RETURN.
  END.

  as-of-date = TODAY.

  RUN enable_UI.

  RUN sys/inc/CustListForm.p ( "HR15",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust-no.
  END.

  {methods/nowait.i}

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'HR15',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""HR15""}

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
                            INPUT 'HR15',
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
                                  INPUT 'HR15').


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
  DISPLAY tb_cust-list begin_cust-no end_cust-no as-of-date tb_last-year 
          tb_prt-cust tb_tot-rnu rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tb_cust-list btnCustList begin_cust-no end_cust-no 
         as-of-date tb_last-year tb_prt-cust tb_tot-rnu rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generate-data C-Win 
PROCEDURE generate-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-mode AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-from-date AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ip-to-date AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER v1 AS INT NO-UNDO.
  DEFINE OUTPUT PARAMETER op-zero AS LOG INIT YES NO-UNDO.
  DEFINE OUTPUT PARAMETER op-data-string AS CHAR FORMAT "X(198)" NO-UNDO.
  DEFINE OUTPUT PARAMETER op-excel-string AS CHAR FORMAT "X(198)" NO-UNDO.
  DEFINE OUTPUT PARAMETER op-amount AS DECIMAL NO-UNDO.

  DEF VAR v-amt     AS   DEC  EXTENT 14 NO-UNDO.
  DEF VAR v-slsp    LIKE ar-invl.s-pct EXTENT 1 NO-UNDO.
  DEF VAR v-slsm    LIKE ar-invl.sman  EXTENT 1 NO-UNDO.
  DEF VAR v-amt1    AS   DEC NO-UNDO.
  DEF VAR i AS INT NO-UNDO.

  EMPTY TEMP-TABLE tt-report.

  FOR EACH ar-inv WHERE
      ar-inv.company  EQ cocode AND
      ar-inv.inv-date GE ip-from-date AND
      ar-inv.inv-date LE ip-to-date AND
      ar-inv.posted   EQ YES AND
      ar-inv.cust-no EQ cust.cust-no
      NO-LOCK,
      EACH ar-invl WHERE
           ar-invl.x-no EQ ar-inv.x-no AND
           (ar-invl.billable OR NOT ar-invl.misc)
        NO-LOCK:

      DO i = 1 TO 3:
        ASSIGN
         v-amt     = 0
         v-slsm[1] = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN
                       cust.sman ELSE ar-invl.sman[i].

        IF i NE 1 AND
           (v-slsm[1] EQ "" OR ar-invl.s-pct[i] EQ 0) THEN NEXT.

        ASSIGN
         v-slsp[1] = IF ar-invl.sman[i] EQ ""              OR
                        (ar-invl.s-pct[i] EQ 0 AND i EQ 1) THEN 100
                     ELSE ar-invl.s-pct[i]
         v-amt1    = ar-invl.amt * v-slsp[1] / 100.

        IF v-amt1 EQ ? THEN v-amt1 = 0.

        CREATE tt-report.
        ASSIGN
         tt-report.dec3    = v-amt1
         tt-report.date1   = ar-inv.inv-date.
      END.
   END.

   FOR EACH ar-cash WHERE
       ar-cash.company    EQ cocode AND
       ar-cash.cust-no    EQ cust.cust-no AND
       ar-cash.check-date GE ip-from-date AND
       ar-cash.check-date LE ip-to-date AND
       ar-cash.posted     EQ YES
       NO-LOCK,
       EACH ar-cashl WHERE
            ar-cashl.c-no    EQ ar-cash.c-no AND
            ar-cashl.posted  EQ YES AND
            ar-cashl.memo    EQ YES AND
            CAN-FIND(FIRST account WHERE
                     account.company EQ ar-cashl.company AND
                     account.actnum  EQ ar-cashl.actnum AND
                     account.type    EQ "R")
       NO-LOCK:

     RELEASE ar-invl.

     RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

     IF AVAIL oe-retl THEN
     FIND FIRST ar-invl
         WHERE ar-invl.company EQ cocode
           AND ar-invl.cust-no EQ cust.cust-no
           AND ar-invl.inv-no  EQ ar-cashl.inv-no
           AND ar-invl.i-no    EQ oe-retl.i-no
           AND (ar-invl.billable OR NOT ar-invl.misc)
         NO-LOCK NO-ERROR.

     DO i = 1 TO 3:
       ASSIGN
        v-amt     = 0
        v-slsm[1] = IF (NOT AVAIL ar-invl)                OR
                       (ar-invl.sman[i] EQ "" AND i EQ 1) THEN
                      cust.sman ELSE ar-invl.sman[i].

       IF i NE 1 AND
           (v-slsm[1] EQ "" OR ar-invl.s-pct[i] EQ 0) THEN NEXT.

       ASSIGN
        v-slsp[1] = IF (NOT AVAIL ar-invl)                OR
                       ar-invl.sman[i] EQ ""              OR
                       (ar-invl.s-pct[i] EQ 0 AND i EQ 1) THEN 100
                    ELSE ar-invl.s-pct[i]
        v-amt1    = (ar-cashl.amt-paid - ar-cashl.amt-disc) *
                    v-slsp[1] / 100.

       IF v-amt1 EQ ? THEN v-amt1 = 0.

       CREATE tt-report.
       ASSIGN
        tt-report.dec3    = v-amt1
        tt-report.date1   = ar-cash.check-date.

       IF NOT AVAIL ar-invl THEN LEAVE.
     END.
   END.

   v-amt = 0.

   FOR EACH tt-report
       NO-LOCK:

       IF ip-mode EQ "THIS YEAR" THEN
          DO i = 1 TO 13:
             IF tt-report.date1 GE tyfdate[i] AND
                tt-report.date1 LE tytdate[i] THEN
                v-amt[i] = v-amt[i] + tt-report.dec3.
          END.
       ELSE
          DO i = 1 TO 13:
             IF tt-report.date1 GE lyfdate[i] AND
                tt-report.date1 LE lytdate[i] THEN
                v-amt[i] = v-amt[i] + tt-report.dec3.
          END.
   END.

   /*Total*/
   DO i = 1 TO 13:
      v-amt[14] = v-amt[14] + v-amt[i].
      IF v1 GE i THEN
      DO:
         IF v-amt[i] NE 0 THEN
            op-zero = NO.

         ASSIGN
            op-data-string = op-data-string + STRING(v-amt[i],"$->>>,>>>,>>9") + " ".

         IF tb_excel THEN
            op-excel-string = op-excel-string + '"' + STRING(v-amt[i],"$->>>,>>>,>>9") + '",'.
      END.
      ELSE LEAVE.
   END.

   op-data-string = op-data-string + STRING(v-amt[14],"$->>>,>>>,>>9").

   IF tb_excel THEN
      op-excel-string = op-excel-string + '"' + STRING(v-amt[14],"$->>>,>>>,>>9") + '",'.

   op-amount = v-amt[14] .

   DOWN.

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
SESSION:SET-WAIT-STATE ("general").
{sys/form/r-topw.f}

def var fcust as ch init "" NO-UNDO.
def var tcust like fcust init "zzzzzzzz" NO-UNDO.

DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR v         AS   INT NO-UNDO.
DEF VAR v-ly      AS   INT NO-UNDO.
DEF VAR v-yr      LIKE period.yr NO-UNDO.
DEF VAR v-yr-ly   LIKE period.yr NO-UNDO.
DEF VAR v1        AS   INT NO-UNDO.
DEF VAR v1-ly     AS   INT NO-UNDO.
DEF VAR v2        AS   INT NO-UNDO.
DEF VAR i         AS   INT NO-UNDO.
DEF VAR lv-period-header AS CHAR NO-UNDO.
DEF VAR lv-period-excel-header AS CHAR NO-UNDO.
DEF VAR ly-period-header AS CHAR NO-UNDO.
DEF VAR ly-period-excel-header AS CHAR NO-UNDO.
DEF VAR as-of-date-ly AS DATE NO-UNDO.
DEF VAR op-zero-ty AS LOG NO-UNDO.
DEF VAR op-zero-ly AS LOG NO-UNDO.
DEF VAR cust-string AS CHAR FORMAT "X(40)" NO-UNDO.
DEF VAR ip-header AS CHAR NO-UNDO.
DEF VAR data-string-ty AS CHAR FORMAT "X(198)" NO-UNDO.
DEF VAR data-string-ly AS CHAR FORMAT "X(198)" NO-UNDO.
DEF VAR excel-string-ty AS CHAR FORMAT "X(198)" NO-UNDO.
DEF VAR excel-string-ly AS CHAR FORMAT "X(198)" NO-UNDO.
DEF VAR ip-mode AS CHAR NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.
DEFINE VARIABLE dTotSales AS DECIMAL NO-UNDO .
DEFINE VARIABLE dTotSales2 AS DECIMAL NO-UNDO .

{custom/statusMsg.i "'Processing...'"} 

 EMPTY TEMP-TABLE tt-cust-sales .

FORM cust-string FORMAT "X(40)"
     WITH NO-BOX NO-LABELS FRAME custs DOWN STREAM-IO WIDTH 200.

FORM ip-header FORMAT "X(198)" SKIP data-string-ty FORMAT "X(198)"
     WITH NO-BOX NO-LABELS FRAME custx DOWN STREAM-IO WIDTH 200.

FORM ip-mode FORMAT "X(8)"
     WITH NO-BOX NO-LABELS FRAME mode DOWN STREAM-IO WIDTH 200.

 ASSIGN
  str-tit2 = c-win:title
  {sys/inc/ctrtext.i str-tit2 112}
  fcust = begin_cust-no
  tcust = end_cust-no
  lSelected  = tb_cust-list.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN
  OUTPUT STREAM excel TO VALUE(fi_file).

if td-show-parm then run show-param.

display "" with frame r-top.

SESSION:SET-WAIT-STATE ("general").

FIND LAST period NO-LOCK
    WHERE period.company EQ cocode
      AND period.pst     LE as-of-date
      AND period.pend    GE as-of-date.

ASSIGN
 v    = period.pnum
 v-yr = period.yr.

FOR EACH period NO-LOCK
    WHERE period.company EQ cocode
      AND period.yr      EQ v-yr
    BY period.pnum DESC:

  v1 = period.pnum.
  LEAVE.
END.

ASSIGN
  i = 1
  lv-period-header = FILL(" ",5).

FOR EACH period WHERE
    period.company EQ cocode AND
    period.yr      EQ v-yr
    NO-LOCK
    BY period.pnum:

  ASSIGN
     tyfdate[i] = period.pst
     tytdate[i] = period.pend
     i = i + 1
     lv-period-header = lv-period-header
                      + "Period " + STRING(period.pnum)
                      + (IF period.pnum LE 8 THEN FILL(" ",6)
                         ELSE FILL(" ",5)).

   IF tb_excel THEN
      lv-period-excel-header = lv-period-excel-header
                            + '"' + "Period " + STRING(period.pnum) + '",'.
END.

ASSIGN
   lv-period-header = lv-period-header + "    TOTAL".

IF tb_excel THEN
   lv-period-excel-header = lv-period-excel-header
                          + '"' + "TOTAL" + '",'.

IF tb_last-year THEN
DO:
   as-of-date-ly = DATE(MONTH(as-of-date),DAY(as-of-date),YEAR(as-of-date) - 1).

   FIND LAST period WHERE
        period.company EQ cocode AND
        period.pst     LE as-of-date-ly AND
        period.pend    GE as-of-date-ly
        NO-LOCK NO-ERROR.

   ASSIGN
      v-ly    = period.pnum
      v-yr-ly = period.yr.

   FOR EACH period WHERE
       period.company EQ cocode AND
       period.yr      EQ v-yr-ly
       NO-LOCK
       BY period.pnum DESC:

       v1-ly = period.pnum.
       LEAVE.
   END.

   ASSIGN
      i = 1
      ly-period-header = FILL(" ",5).

   FOR EACH period WHERE
       period.company EQ cocode AND
       period.yr      EQ v-yr-ly
       NO-LOCK
       BY period.pnum:

       ASSIGN
          lyfdate[i] = period.pst
          lytdate[i] = period.pend
          i = i + 1
          ly-period-header = ly-period-header
                           + "Period " + STRING(period.pnum)
                           + (IF period.pnum LE 8 THEN FILL(" ",6)
                              ELSE FILL(" ",5)).

        IF tb_excel THEN
           ly-period-excel-header = ly-period-excel-header
                                  + '"' + "Period " + STRING(period.pnum) + '",'.
   END.

   ASSIGN
      ly-period-header = ly-period-header + "    TOTAL".

   IF tb_excel THEN
      ly-period-excel-header = ly-period-excel-header
                             + '"' + "TOTAL" + '",'.
END.
IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
END.


PUT SPACE(58) "Sales By Period" SKIP
    SPACE(58) "As of " as-of-date SKIP.

FOR each cust FIELDS(NAME cust-no sman) WHERE
    cust.company eq cocode 
    AND cust.cust-no GE fcust
    AND cust.cust-no LE tcust
    AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
    AND ttCustList.log-fld no-lock) else true)
    AND cust.active NE "I"
    no-lock:

    {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

    op-zero-ly = YES.

    RUN generate-data(INPUT "THIS YEAR",
                      INPUT tyfdate[1],
                      INPUT as-of-date,
                      INPUT v1,
                      OUTPUT op-zero-ty,
                      OUTPUT data-string-ty,
                      OUTPUT excel-string-ty,
                      OUTPUT dTotSales).

    IF tb_last-year THEN
       RUN generate-data(INPUT "LAST YEAR",
                         INPUT lyfdate[1],
                         INPUT as-of-date-ly,
                         INPUT v1-ly,
                         OUTPUT op-zero-ly,
                         OUTPUT data-string-ly,
                         OUTPUT excel-string-ly,
                         OUTPUT dTotSales2).

    CREATE tt-cust-sales .
         ASSIGN
             tt-cust-sales.cust-no         = cust.cust-no 
             tt-cust-sales.op-zero-ty      = op-zero-ty
             tt-cust-sales.data-string-ty  = data-string-ty
             tt-cust-sales.excel-string-ty = excel-string-ty
             tt-cust-sales.dTotSales       = dTotSales
             tt-cust-sales.op-zero-ly      = op-zero-ly
             tt-cust-sales.data-string-ly  = data-string-ly
             tt-cust-sales.excel-string-ly = excel-string-ly .
         IF NOT tb_tot-rnu THEN
                tt-cust-sales.dTotSales = 0 .


    /*IF tb_prt-cust OR (NOT tb_prt-cust AND NOT(op-zero-ty AND op-zero-ly)) THEN
    DO:
       DISPLAY cust.NAME + "/" + cust.cust-no @ cust-string SKIP(1) WITH FRAME custs.

       IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' cust.NAME + "/" + cust.cust-no '",'
              SKIP(1).

       DISPLAY "THIS YEAR" @ ip-mode FORM "X(9)" SKIP(1) WITH FRAME mode.

       IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' "THIS YEAR" '",'
              SKIP(1)
              lv-period-excel-header SKIP
              excel-string-ty SKIP(2).

       DISPLAY lv-period-header @ ip-header FORM "X(198)" SKIP data-string-ty FORM "X(198)" skip(2) WITH FRAME custx.

       IF tb_last-year THEN
       DO:
          DISPLAY "LAST YEAR" @ ip-mode FORM "X(9)" SKIP(1) WITH FRAME mode.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 '"' "LAST YEAR" '",'
                 SKIP(1)
                 ly-period-excel-header SKIP.

           DISPLAY ly-period-header @ ip-header FORM "X(198)" SKIP data-string-ly @ data-string-ty FORM "X(198)" skip(2) WITH FRAME custx.

           IF tb_excel THEN
              PUT STREAM excel UNFORMATTED
                  excel-string-ly SKIP(2).
       END.
    END.*/
end.

FOR EACH tt-cust-sales NO-LOCK BREAK BY tt-cust-sales.dTotSales DESC :
    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ cocode 
          AND cust.cust-no EQ tt-cust-sales.cust-no  NO-ERROR .

    IF tb_prt-cust OR (NOT tb_prt-cust AND NOT(tt-cust-sales.op-zero-ty AND tt-cust-sales.op-zero-ly)) THEN
    DO:
       DISPLAY cust.NAME + "/" + cust.cust-no @ cust-string SKIP(1) WITH FRAME custs.

       IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' cust.NAME + "/" + cust.cust-no '",'
              SKIP(1).

       DISPLAY "THIS YEAR" @ ip-mode FORM "X(9)" SKIP(1) WITH FRAME mode.

       IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' "THIS YEAR" '",'
              SKIP(1)
              lv-period-excel-header SKIP
              tt-cust-sales.excel-string-ty SKIP(2).

       DISPLAY lv-period-header @ ip-header FORM "X(198)" SKIP tt-cust-sales.data-string-ty @ data-string-ty FORM "X(198)" skip(2) WITH FRAME custx.

       IF tb_last-year THEN
       DO:
          DISPLAY "LAST YEAR" @ ip-mode FORM "X(9)" SKIP(1) WITH FRAME mode.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 '"' "LAST YEAR" '",'
                 SKIP(1)
                 ly-period-excel-header SKIP.

           DISPLAY ly-period-header @ ip-header FORM "X(198)" SKIP tt-cust-sales.data-string-ly @ data-string-ty FORM "X(198)" skip(2) WITH FRAME custx.

           IF tb_excel THEN
              PUT STREAM excel UNFORMATTED
                  tt-cust-sales.excel-string-ly SKIP(2).
       END.
    END.
END. /* for each tt-cust-sales*/

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
        begin_cust-no:SENSITIVE = NOT iplChecked
        end_cust-no:SENSITIVE = NOT iplChecked
        begin_cust-no:VISIBLE = NOT iplChecked
        end_cust-no:VISIBLE = NOT iplChecked
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

