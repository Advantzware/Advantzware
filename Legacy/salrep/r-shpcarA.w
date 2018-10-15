&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-shpcar.w

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

def var v-per-rpt   as   log format "PTD/YTD" init YES NO-UNDO.

def var v-date      as   date extent 2 init [01/01/01, today] NO-UNDO.
def var v-carr      like ar-inv.carrier extent 2 init ["","zzzzz"] NO-UNDO.
def var v-sumdet    as   log format "Summary/Detail" init YES NO-UNDO.

def var v-frst      as   log extent 2 NO-UNDO.
def var v-prof      like ar-invl.amt NO-UNDO.
def var v-gp        as   dec format ">>9.99" NO-UNDO.
def var v-year      as   INTEGER NO-UNDO.
def var v-inv-no    like ar-invl.inv-no NO-UNDO.
def var v-procat    like itemfg.procat NO-UNDO.
def var v-amt       like ar-invl.amt NO-UNDO.
def var v-cost      like ar-invl.t-cost NO-UNDO.
def var v-cust-part like ar-invl.part-no no-undo.

def var v-tot-samt  as   dec format "->>>>>>>9.99" extent 3 NO-UNDO.
def var v-tot-cost  as   dec format "->>>>>>>9.99" extent 3 NO-UNDO.

def var v-head      as   character format "x(132)" extent 3 NO-UNDO.

def TEMP-TABLE w-carr    no-undo
       field carrier like ar-inv.carrier
       field samt    like ar-invl.amt
       field cost    like ar-invl.amt.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_ptd begin_period begin_date ~
end_date begin_carr end_carr tb_sum-det rd-dest lv-ornt lines-per-page ~
lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lbl_ptd rd_ptd begin_period begin_date ~
end_date begin_carr end_carr tb_sum-det rd-dest lv-ornt lines-per-page ~
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

DEFINE VARIABLE begin_carr AS CHARACTER FORMAT "X(5)" 
     LABEL "Beginning Carrier" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE begin_period AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "For Period?" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE end_carr AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
     LABEL "Ending Carrier" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-shpcar.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_ptd AS CHARACTER FORMAT "X(256)":U INITIAL "PTD / YTD?" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_ptd AS CHARACTER INITIAL "PTD" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "PTD", "PTD",
"YTD", "YTD"
     SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.76.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.24.

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

DEFINE VARIABLE tb_sum-det AS LOGICAL INITIAL yes 
     LABEL "Summary?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     lbl_ptd AT ROW 3.14 COL 12 COLON-ALIGNED NO-LABEL
     rd_ptd AT ROW 3.14 COL 28 NO-LABEL
     begin_period AT ROW 5.52 COL 26 COLON-ALIGNED
     begin_date AT ROW 6.71 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 6.71 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_carr AT ROW 7.67 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Carrier"
     end_carr AT ROW 7.67 COL 69 COLON-ALIGNED HELP
          "Enter Ending Carrier"
     tb_sum-det AT ROW 9.57 COL 28
     rd-dest AT ROW 13.14 COL 8 NO-LABEL
     lv-ornt AT ROW 13.38 COL 31 NO-LABEL
     lines-per-page AT ROW 13.38 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 14.81 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 15.76 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17 COL 30
     tb_excel AT ROW 19.1 COL 50.2 RIGHT-ALIGNED
     tb_runExcel AT ROW 19.1 COL 71.2 RIGHT-ALIGNED
     fi_file AT ROW 19.91 COL 28.2 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 21.67 COL 19
     btn-cancel AT ROW 21.67 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.19 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 11.71 COL 1
     RECT-7 AT ROW 1.24 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 23.38.


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
         TITLE              = "Shipping Carrier Report"
         HEIGHT             = 23.62
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
       begin_carr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_carr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_ptd IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_ptd:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_ptd".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_ptd:PRIVATE-DATA IN FRAME FRAME-A     = 
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

ASSIGN 
       tb_sum-det:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Shipping Carrier Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Shipping Carrier Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_carr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_carr C-Win
ON LEAVE OF begin_carr IN FRAME FRAME-A /* Beginning Carrier */
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


&Scoped-define SELF-NAME begin_period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_period C-Win
ON LEAVE OF begin_period IN FRAME FRAME-A /* For Period? */
DO:
  assign {&self-name}.

  run show-period-dates.
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
 SESSION:SET-WAIT-STATE ("general").

  SESSION:SET-WAIT-STATE("general").
  run run-report. 

  STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_carr
                            &END_cust=END_carr
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust= begin_carr
                             &END_cust=end_carr
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust= begin_carr
                                  &END_cust=end_carr
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


&Scoped-define SELF-NAME end_carr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_carr C-Win
ON LEAVE OF end_carr IN FRAME FRAME-A /* Ending Carrier */
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


&Scoped-define SELF-NAME rd_ptd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_ptd C-Win
ON VALUE-CHANGED OF rd_ptd IN FRAME FRAME-A
DO:
  assign {&self-name}.

  if rd_ptd eq "YTD" then do:
    find first period
        where period.company eq gcompany
          and period.yr      eq v-year
        no-lock no-error.

    begin_date = if avail period then period.pst
                                 else date(1,1,year(today)).

    display begin_date WITH FRAME FRAME-A IN WINDOW C-Win.
  end.

  run show-period-dates.
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


&Scoped-define SELF-NAME tb_sum-det
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sum-det C-Win
ON VALUE-CHANGED OF tb_sum-det IN FRAME FRAME-A /* Summary? */
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
   begin_date = today
   end_date   = today.

  find first period
      where period.company eq gcompany
        and period.pst     le today
        and period.pend    ge today
        and period.pstat
    no-lock no-error.

  if available period then
    assign
     begin_period = period.pnum
     v-year       = period.yr
     begin_date   = period.pst.

  else
    assign
     begin_period = month(today)
     v-year       = year(today).

  RUN enable_UI.

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO rd_ptd IN FRAME {&FRAME-NAME}.
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
  DISPLAY lbl_ptd rd_ptd begin_period begin_date end_date begin_carr end_carr 
          tb_sum-det rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 rd_ptd begin_period begin_date end_date begin_carr 
         end_carr tb_sum-det rd-dest lv-ornt lines-per-page lv-font-no 
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
/* ------------------------------------------------ oe/rep/sa-comm.p 3/96 JLF */
/* Commission Summary / Detail Report                                         */
/* -------------------------------------------------------------------------- */
SESSION:SET-WAIT-STATE ("general").
{sys/form/r-top3w.f}

 def var v-per-rpt   as   log format "PTD/YTD" init YES NO-UNDO.
 def var v-period    like uperiod init 1 NO-UNDO.
 def var v-date      as   date extent 2 init [01/01/01, today] NO-UNDO.
 def var v-carr      like ar-inv.carrier extent 2 init ["","zzzzz"] NO-UNDO.
 def var v-sumdet    as   log format "Summary/Detail" init YES NO-UNDO. 
 def var v-frst      as   log extent 2 NO-UNDO.
 def var v-prof      like ar-invl.amt NO-UNDO.
 def var v-gp        as   dec format ">>9.99" NO-UNDO.
 def var v-year      as   INTEGER NO-UNDO.
 def var v-inv-no    like ar-invl.inv-no NO-UNDO.
 def var v-procat    like itemfg.procat NO-UNDO.
 def var v-amt       like ar-invl.amt NO-UNDO.
 def var v-cost      like ar-invl.t-cost NO-UNDO.
 def var v-cust-part like ar-invl.part-no no-undo.

 def var v-tot-samt  as   dec format "->>>>>>>9.99" extent 3 NO-UNDO.
 def var v-tot-cost  as   dec format "->>>>>>>9.99" extent 3 NO-UNDO.
 def var v-head      as   character format "x(132)" extent 3 NO-UNDO.
 DEF VAR v-current-page AS INT NO-UNDO.

 {custom/statusMsg.i "'Processing...'"} 

 DO WITH FRAME {&FRAME-NAME}:
    assign
     tb_excel tb_runExcel fi_file
     begin_carr begin_date begin_period end_carr end_date 
     lines-per-page lv-font-name lv-font-no 
     tb_sum-det td-show-parm.
 END.

    format header
           v-head[1]                skip
           v-head[2]                skip
           v-head[3]

        with no-labels no-box no-underline STREAM-IO width 132 frame f-top page-top.

    {sa/sa-sls01.i}
     ASSIGN
     str-tit2 = c-win:title
     {sys/inc/ctrtext.i str-tit2 112}

       v-per-rpt   = rd_ptd EQ "PTD"
       v-period    = begin_period 
       v-date[1]   = begin_date 
       v-date[2]   = end_date
       v-carr[1]   = begin_carr
       v-carr[2]   = END_carr
       v-sumdet    = tb_sum-det.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF tb_excel THEN DO:
       OUTPUT STREAM excel TO VALUE(fi_file).
       /*PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.*/
    END.

    if td-show-parm then run show-param.

    DISPLAY WITH FRAME r-top.
    DISPLAY WITH frame f-top.

   find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
  for each cust
      where cust.company eq cocode
      no-lock:

    for each ar-inv
        where ar-inv.company  eq cocode
          and ar-inv.posted   eq yes
          and ar-inv.cust-no  eq cust.cust-no
          and ar-inv.inv-date ge v-date[1]
          and ar-inv.inv-date le v-date[2]
          and ar-inv.carrier  ge v-carr[1]
          and ar-inv.carrier  le v-carr[2]
        no-lock,

        each ar-invl
        where ar-invl.x-no eq ar-inv.x-no
          and ar-invl.misc eq no
        no-lock

        transaction:

        {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

      create report.
      assign
       report.term-id = v-term
       report.key-01  = ar-inv.carrier
       report.key-02  = cust.cust-no
       report.key-03  = string(ar-inv.inv-no,"999999")
       report.key-04  = ar-invl.i-no
       report.key-10  = "ar-invl"
       report.rec-id  = recid(ar-invl).
    end.
  END.

    input-work:
  for each report
      where report.term-id eq v-term,

      first cust
      where cust.company eq cocode
        and cust.cust-no eq report.key-02
      no-lock

      break by report.key-01
            by report.key-02
            by report.key-03 WITH FRAME detail:

      {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

    if first(report.key-01)    then v-frst[1] = yes.
    if first-of(report.key-01) then v-frst[2] = yes.

    release ar-invl.
    release ar-cashl.

    if report.key-10 eq "ar-invl" then
    find ar-invl where recid(ar-invl) eq report.rec-id no-lock no-error.

    if avail ar-invl then do:
      release itemfg.
      if not ar-invl.misc then
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq ar-invl.i-no
          no-lock no-error.

      assign
       v-inv-no    = ar-invl.inv-no
       v-procat    = if ar-invl.misc then "MISC" else
                     if avail itemfg then itemfg.procat else "ARINV"
       v-amt       = ar-invl.amt
       v-cost      = ar-invl.t-cost
       v-cust-part = ar-invl.part-no.
    end.

    else
    if report.key-10 eq "ar-cashl" then
    find ar-cashl where recid(ar-cashl) eq report.rec-id no-lock no-error.

    if avail ar-cashl then do:
      RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

      if avail oe-retl then do:
        release itemfg.

        find first ar-invl
            where ar-invl.company eq cocode
              and ar-invl.cust-no eq cust.cust-no
              and ar-invl.inv-no  eq ar-cashl.inv-no
              and ar-invl.i-no    eq oe-retl.i-no
              and (ar-invl.billable or not ar-invl.misc)
            no-lock no-error.

        if avail ar-invl then do:
          if not ar-invl.misc then
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq oe-retl.i-no
              no-lock no-error.

          assign
           v-inv-no  = ar-invl.inv-no
           v-procat  = if ar-invl.misc then "MISC" else
                       if avail itemfg then itemfg.procat else "CRMEMO"
           v-amt     = (ar-cashl.amt-paid - ar-cashl.amt-disc) 
           v-cost    = - (oe-retl.cost * oe-retl.tot-qty-return).
        end.
      end.
    END.

    if v-cost eq ? then v-cost = 0.

    assign
     v-prof    = v-amt - v-cost
     v-gp      = round(v-prof / v-amt * 100,2)

     v-tot-samt[1] = v-tot-samt[1] + v-amt
     v-tot-cost[1] = v-tot-cost[1] + v-cost.

    if v-gp eq ? then v-gp = 0.

    if first-of(report.key-01) AND NOT FIRST(report.key-01) then page.

    if not v-sumdet then
    DO:
      display report.key-01           when first-of(report.key-01)
                                      format "x(5)"
                                      COLUMN-LABEL "Carrier"
              space(4)
              report.key-02           when first-of(report.key-02)
                                      COLUMN-LABEL "Cust #"
              space(2)
              cust.name               when first-of(report.key-02)
                                      format "x(22)"
                                      COLUMN-LABEL "Name"
              space(2)
              v-cust-part             COLUMN-LABEL "Part #"
              space(2)
              v-inv-no                COLUMN-LABEL "Invoice #"
              space(2)
              v-procat                COLUMN-LABEL "Cat."
              space(2)
              v-amt                   format "->>>>>>>9.99"
                                      COLUMN-LABEL "Amount"
              space(2)
              v-cost                  format "->>>>>>>9.99"
                                      COLUMN-LABEL "Cost"
              space(2)
              v-gp                    format "->>>9.99"
                                      COLUMN-LABEL "GP"

          with frame detail NO-BOX STREAM-IO width 132.
        DOWN WITH FRAME detail.
      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' IF first-of(report.key-01) THEN report.key-01
                 ELSE ""                                       '",'
             '"' IF first-of(report.key-02) THEN report.key-02
                 ELSE ""                                       '",'
             '"' IF first-of(report.key-02) THEN cust.NAME
                 ELSE ""                                       '",'
             '"' v-cust-part                                   '",'
             '"' v-inv-no                                      '",'
             '"' v-procat                                      '",'
             '"' STRING(v-amt,"->>>>>>>9.99")                  '",'
             '"' STRING(v-cost,"->>>>>>>9.99")                 '",'
             '"' STRING(v-gp,"->>>9.99")                       '",'
             SKIP.
    END.

    if last-of(report.key-02) then do:

      v-cost = (v-tot-samt[1] - v-tot-cost[1]) / v-tot-samt[1] * 100.

      if v-cost = ? then v-cost = 0.

      if v-sumdet then
      DO:
        IF PAGE-NUMBER <> v-current-page THEN DO:
            v-current-page = PAGE-NUMBER.
            PUT "Carrier" SPACE(1) 
                "Cust #  " SPACE(2)
                "Name                                " SPACE(2)
                "Amount       " SPACE(2)
                "Cost     " SPACE(2)
                "GP %    "
                SKIP.
            PUT "-----" SPACE(3) 
                "--------" SPACE(2)
                "------------------------------" SPACE(2)
                " -----------" SPACE(2)
                "    -------" SPACE(2)
                " --------"
                SKIP.
        END.
        PUT report.key-01         format "x(5)"
                space(3)
                report.key-02         
                space(2)
                cust.NAME             
                space(2)
                v-tot-samt[1]         
                space(2)
                v-tot-cost[1]         
                space(2)
                v-cost                format "->>>9.99"

                SKIP.
            /* with frame summary no-box STREAM-IO width 132. */

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               '"' report.key-01                         '",'
               '"' report.key-02                         '",'
               '"' cust.NAME                                '",'
               '"' ""                                       '",'
               '"' ""                                       '",'
               '"' ""                                       '",'
               '"' STRING(v-tot-samt[1],"->>>>>>>9.99")     '",'
               '"' STRING(v-tot-cost[1],"->>>>>>>9.99")     '",'
               '"' STRING(v-cost,"->>>9.99")                '",'
               SKIP.
      END.

      else do:
        find first w-carr
            where w-carr.carr eq report.key-01
            no-error.
        if not avail w-carr then do:
          create w-carr.
          w-carr.carr = report.key-01.
        end.

        assign
         w-carr.samt = w-carr.samt + v-tot-samt[1]
         w-carr.cost = w-carr.cost + v-tot-cost[1].
      end.

      assign
       v-tot-samt[2] = v-tot-samt[2] + v-tot-samt[1]
       v-tot-cost[2] = v-tot-cost[2] + v-tot-cost[1]
       v-tot-samt[1] = 0
       v-tot-cost[1] = 0.
    end.

    if v-sumdet               and
       last-of(report.key-01) then do:

      v-cost = (v-tot-samt[2] - v-tot-cost[2]) / v-tot-samt[2] * 100.

      if v-cost = ? then v-cost = 0.

      if ((not v-frst[2]) and (not last(report.key-01))) or
         ((not v-frst[1]) and last(report.key-01))       then
      DO:
        display skip(1)
                "Carrier Totals:"
                space(35)
                v-tot-samt[2]
                space(2)
                v-tot-cost[2]
                space(2)
                v-cost              format "->>>9.99"
                skip(1)

            with frame carrier-sum no-box no-labels STREAM-IO width 132.

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               SKIP(1)
               '"' "Carrier Totals:"                        '",'
               '"' ""                                       '",'
               '"' ""                                       '",'
               '"' ""                                       '",'
               '"' ""                                       '",'
               '"' ""                                       '",'
               '"' STRING(v-tot-samt[2],"->>>>>>>9.99")     '",'
               '"' STRING(v-tot-cost[2],"->>>>>>>9.99")     '",'
               '"' STRING(v-cost,"->>>9.99")                '",'
               SKIP(1).
      END.

      assign
       v-frst[1]     = no
       v-tot-samt[3] = v-tot-samt[3] + v-tot-samt[2]
       v-tot-cost[3] = v-tot-cost[3] + v-tot-cost[2]
       v-tot-samt[2] = 0
       v-tot-cost[2] = 0.
    end.

    if last-of(report.key-02) then v-frst[2] = no.

    delete report.
  end.  /* input-work */

  if not v-sumdet then do:
    assign
     str-tit2 = "S A L E S   B Y   C A R R I E R  -  R E C A P"
     str-tit3 = (if v-per-rpt then "P" else "Y") +
                "TD (" + string(v-date[1]) + "-" + string(v-date[2]) + ")"

     v-head[2] = "Carrier                                           Total Sa" +
                 "les $        Cost $      GP %"
     v-head[3] = fill("-",87)

     x = (115 - length(str-tit2)) / 2
     str-tit2 = fill(" ",x) + str-tit2
     x = (132 - length(str-tit3)) / 2
     str-tit3 = fill(" ",x) + str-tit3.

    page.

    assign
     v-tot-samt[3] = 0
     v-tot-cost[3] = 0.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
           SKIP(1)
           '"' "Carrier"                                '",'
           '"' ""                                       '",'
           '"' ""                                       '",'
           '"' ""                                       '",'
           '"' ""                                       '",'
           '"' ""                                       '",'
           '"' "Total Sales $"                          '",'
           '"' "Total Cost $"                           '",'
           '"' "Total GP %"                             '",'
           SKIP.

    recap-work:

    for each w-carr
        break by w-carr.carr:

      assign
       v-cost = (w-carr.samt - w-carr.cost) / w-carr.samt * 100

       v-tot-samt[3] = v-tot-samt[3] + w-carr.samt
       v-tot-cost[3] = v-tot-cost[3] + w-carr.cost.

      if v-cost = ? then v-cost = 0.

      display w-carr.carr         format "x(5)"
              space(46)
              w-carr.samt         format "->>>>>>>9.99"
              space(2)
              w-carr.cost         format "->>>>>>>9.99"
              space(2)
              v-cost              format "->>>9.99"

          with frame carrier-det no-box no-labels STREAM-IO width 132.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' w-carr.carr                              '",'
             '"' ""                                       '",'
             '"' ""                                       '",'
             '"' ""                                       '",'
             '"' ""                                       '",'
             '"' ""                                       '",'
             '"' STRING(w-carr.samt,"->>>>>>>9.99")     '",'
             '"' STRING(w-carr.cost,"->>>>>>>9.99")     '",'
             '"' STRING(v-cost,"->>>9.99")                '",'
             SKIP.
    end.  /* recap-work */
  end.

  v-cost = (v-tot-samt[3] - v-tot-cost[3]) / v-tot-samt[3] * 100.

  if v-cost = ? then v-cost = 0.

  display skip(1)
          "Grand Totals:"
          space(37)
          v-tot-samt[3]
          space(2)
          v-tot-cost[3]
          space(2)
          v-cost                format "->>>9.99"

      with frame grand-tot no-box no-labels STREAM-IO width 132.

  IF tb_excel THEN
     PUT STREAM excel UNFORMATTED
         SKIP(1)
         '"' "Grand Totals:"                          '",'
         '"' ""                                       '",'
         '"' ""                                       '",'
         '"' ""                                       '",'
         '"' ""                                       '",'
         '"' ""                                       '",'
         '"' STRING(v-tot-samt[3],"->>>>>>>9.99")     '",'
         '"' STRING(v-tot-cost[3],"->>>>>>>9.99")     '",'
         '"' STRING(v-cost,"->>>9.99")                '",'
         SKIP.

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
SESSION:SET-WAIT-STATE ("").
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
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + ",".
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name THEN
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-period-dates C-Win 
PROCEDURE show-period-dates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  if rd_ptd eq "PTD" then do:
    find first period
        where period.company eq gcompany
          and period.yr      eq v-year
          and period.pnum    eq begin_period
        no-lock no-error.

    if avail period then do:
      assign
       v-year     = period.yr
       begin_date = period.pst
       end_date   = if period.pend lt today then period.pend else today.

      display begin_date end_date WITH FRAME FRAME-A IN WINDOW C-Win.
    end.

    else
    if lastkey ne -1 then do: 
      message begin_period "is not a valid period. "
              view-as alert-box error.
      return no-apply.
    end.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

