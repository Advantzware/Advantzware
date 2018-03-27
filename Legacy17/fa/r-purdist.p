&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aprep\r-taxcus.w

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

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_year end_year ~
begin_period end_period rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_year end_year begin_period ~
end_period rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
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

DEFINE VARIABLE begin_period AS INTEGER FORMAT ">9":U INITIAL 1 
     LABEL "From Period" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_year AS INTEGER FORMAT ">>>>":U INITIAL 9999 
     LABEL "From Year" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE end_period AS INTEGER FORMAT ">9":U INITIAL 1 
     LABEL "To Period" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE end_year AS INTEGER FORMAT ">>>>":U INITIAL 9999 
     LABEL "To Year" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-taxcus.csv" 
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

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.62.

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
     begin_year AT ROW 3.38 COL 28 COLON-ALIGNED HELP
          "Enter Fiscal Year"
     end_year AT ROW 3.38 COL 59 COLON-ALIGNED HELP
          "Enter Fiscal Year" WIDGET-ID 4
     begin_period AT ROW 4.57 COL 28 COLON-ALIGNED HELP
          "Enter Reporting Period"
     end_period AT ROW 4.57 COL 59 COLON-ALIGNED HELP
          "Enter Reporting Period" WIDGET-ID 2
     rd-dest AT ROW 10.05 COL 6 NO-LABEL
     lv-ornt AT ROW 11 COL 31 NO-LABEL
     lines-per-page AT ROW 11 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 12.43 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 13.38 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 14.48 COL 30.4
     tb_excel AT ROW 15.76 COL 50.4 RIGHT-ALIGNED
     tb_runExcel AT ROW 15.76 COL 71.4 RIGHT-ALIGNED
     fi_file AT ROW 16.57 COL 28.4 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 18.14 COL 19
     btn-cancel AT ROW 18.14 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 9.33 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 8.86 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 19.71.


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
         TITLE              = "FA Distribution Purge"
         HEIGHT             = 20.05
         WIDTH              = 95.8
         MAX-HEIGHT         = 45.05
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.05
         VIRTUAL-WIDTH      = 256
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
       begin_period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_year:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_year:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
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
ON END-ERROR OF C-Win /* FA Distribution Purge */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* FA Distribution Purge */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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

  run run-report. 
  STATUS DEFAULT "Processing Complete".
  SESSION:SET-WAIT-STATE ("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= ''
                            &begin_cust= "begin_date"
                            &END_cust= "begin_date" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = ''
                             &begin_cust=''
                             &END_cust='' 
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust='' 
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  END CASE.
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
   begin_year   = year(today)
   begin_period = month(today)
   begin_date   = today
   end_date     = today.

  find first period
      where period.company eq cocode
        and period.yr      eq begin_year
        and period.pst     le today
        and period.pend    ge today
        and period.pstat
      no-lock no-error.

  if avail period then
    assign
     begin_period = period.pnum
     begin_year   = period.yr
     begin_date   = period.pst.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_year.
  END.

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
  DISPLAY begin_year end_year begin_period end_period rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_year end_year begin_period end_period rd-dest 
         lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel 
         fi_file btn-ok btn-cancel 
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
/* ---------------------------------------------- ap/rep/taxsched.p 07/99 JLF */
/* Tax Distribution Schedule by Customer                                      */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

def var v-period        like uperiod init 1 NO-UNDO.
def var v-date          as   date extent 2 format "99/99/9999"
                             init [01/01/0001, today] NO-UNDO.                       
def var v-year          as   INT NO-UNDO.

def var v-tax-gl        as   CHAR NO-UNDO.
def var v-tax-dscr      like stax.tax-dscr NO-UNDO.
def var v-sal-gro       as   dec format "->>,>>>,>>9.99" extent 3 NO-UNDO.
def var v-tax-amt       as   dec format "->>,>>>,>>9.99" extent 3 NO-UNDO.
def var v-freight       as   dec format "->>,>>>,>>9.99" extent 3 NO-UNDO.
def var v-actnum        like ar-cashl.actnum NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

/* gdm - */
def var v-grtot         as   dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-grfrght       as   dec format "->>,>>>,>>9.99" NO-UNDO.

/* aj */
def var v-rate          as   DEC NO-UNDO.
def var v-frtr          as   DEC NO-UNDO.
def var v-rate-t        as   DEC NO-UNDO.
def var v-frtr-t        as   DEC NO-UNDO.
def var v-inv-tax       as   DEC NO-UNDO.
def var v-frt-tax       as   DEC NO-UNDO.
DEF VAR v-found         AS   logi NO-UNDO.
DEF VAR ld              AS   DEC NO-UNDO.

def buffer b-stax       for stax.

format header
       skip(1)
       "Tax Authority:"
       v-tax-dscr[1]
       skip(1)
       "Cust Name"
       "Date"                       at 32
       "Invoice"                    to 47
       "Gross Sales $"              to 62
       "Tax $"                      to 77
       "Freight $"                  to 92
       "Net Sales $"                to 107
       fill("-",107)                format "x(107)"

    with frame r-top.

{sa/sa-sls01.i}

SESSION:SET-WAIT-STATE ("general").

EMPTY TEMP-TABLE tt-report.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-period  = begin_period
 v-date[1] = begin_date
 v-date[2] = end_date
  str-tit3 = "(" + string(v-date[1]) + "-" + string(v-date[2]) + ")"
 {sys/inc/ctrtext.i str-tit3 132}. 

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   excelheader = "Tax Authority,Cust Name,Date,Invoice,Gross Sales $,"
               + "Tax $,Freight $,Net Sales $".
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

    FOR each cust where cust.company eq cocode no-lock:

      {custom/statusMsg.i " 'Processing Cust #  '  + string(cust.cust-no) "}

      for each ar-inv
          where ar-inv.company        eq cocode
            and ar-inv.inv-date       ge v-date[1]
            and ar-inv.inv-date       le v-date[2]
            and ar-inv.cust-no        eq cust.cust-no
            and ar-inv.tax-code       ne ""
            and ar-inv.posted         eq yes
          use-index inv-date no-lock:

          create tt-report.
          assign
           tt-report.term-id = v-term
           tt-report.key-01  = ar-inv.tax-code
           tt-report.key-02  = cust.name
           tt-report.key-03  = string(ar-inv.inv-no,"9999999999")
           tt-report.rec-id  = recid(ar-inv).
          RELEASE tt-report.
      end.

      for each ar-cash
          where ar-cash.company    eq cocode
            and ar-cash.cust-no    eq cust.cust-no
            and ar-cash.check-date ge v-date[1]
            and ar-cash.check-date le v-date[2]
            and ar-cash.posted     eq yes
          use-index ar-cash no-lock:

          v-actnum = "".

          for each ar-cashl
              where ar-cashl.c-no   eq ar-cash.c-no
                and ar-cashl.posted eq yes
                and ar-cashl.memo   eq yes
              use-index c-no no-lock,

              first ar-inv
              where ar-inv.company  eq cocode
                and ar-inv.inv-no   eq ar-cashl.inv-no
                and ar-inv.tax-code ne ""
              no-lock:

              find first stax
                  {sys/ref/staxW.i}
                    and stax.tax-group  eq stax.tax-code[1]
                    and stax.tax-acc[1] eq ar-cashl.actnum
                  no-lock no-error.

              if avail stax then do:
                 v-actnum = stax.tax-acc[1].
                 leave.
              end.
          end.

          for each ar-cashl
              where ar-cashl.c-no   eq ar-cash.c-no
                and ar-cashl.posted eq yes
                and ar-cashl.memo   eq yes
              use-index c-no no-lock,

              first ar-inv
              where ar-inv.company  eq cocode
                and ar-inv.inv-no   eq ar-cashl.inv-no
                and ar-inv.tax-code ne ""
              no-lock,

              first stax
              {sys/ref/staxW.i}
                and stax.tax-group eq ar-inv.tax-code
                and stax.tax-group eq stax.tax-code[1]
              no-lock:

              create tt-report.
              assign
               tt-report.term-id = v-term
               tt-report.key-01  = ar-inv.tax-code
               tt-report.key-02  = cust.name
               tt-report.key-03  = string(ar-cashl.inv-no,"9999999999")
               tt-report.key-04  = if v-actnum ne "" then v-actnum
                                                     else stax.tax-acc[1]
               tt-report.rec-id  = recid(ar-cashl).
              RELEASE tt-report.
          end.
      end.
    end.    

    VIEW FRAME r-top.

    for each stax
        {sys/ref/staxW.i}
          and stax.tax-group eq stax.tax-code[1]
        no-lock
        by stax.tax-acc[1]:

      v-tax-dscr[1] = stax.tax-dscr[1].

      page.            

      for each tt-report
        where tt-report.term-id eq v-term
/*             and tt-report.key-01  eq stax.tax-group gdm - */
        BREAK BY tt-report.key-02
              by tt-report.key-01              
              by tt-report.key-03:

        release ar-inv.
        release ar-cashl.
        release ar-cash.        

        {custom/statusMsg.i " 'Processing Cust #  '  + string(tt-report.key-02) 
                     + 'Tax Authority ' +  STRING(tt-report.key-01)  "}

        /* if first-of(tt-report.key-02) then do: gdm - */
        if first-of(tt-report.key-01) then do:         
           assign
            v-found  = NO
            v-rate   = 0
            v-frtr   = 0
            v-rate-t = 0
            v-frtr-t = 0.

           FIND FIRST b-stax
               WHERE b-stax.company   EQ cocode
                 AND b-stax.tax-group EQ tt-report.key-01
               NO-LOCK.
           do i = 1 to EXTENT(stax.tax-code1):
              if b-stax.tax-code1[i] eq stax.tax-group then do:
                assign
                 v-found       = YES 
                 v-rate        = v-rate + b-stax.tax-rate1[i].
                if b-stax.tax-frt1[i] then
                   v-frtr = v-frtr + b-stax.tax-rate1[i].
              end.
              v-rate-t = v-rate-t + b-stax.tax-rate1[i].
              if b-stax.tax-frt1[i] then
                 v-frtr-t = v-frtr-t + b-stax.tax-rate1[i].
           end. 
        end. /* first-of */        

        /* gdm - */
        if v-found then do:

           find first ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock no-error.

           if avail ar-inv then do:

              if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
                 ld = ar-inv.net.
              else
                 ld = ar-inv.gross.

              assign
                v-sal-gro[1] = v-sal-gro[1] + (ld - ar-inv.tax-amt).

              if ar-inv.f-bill AND v-frtr NE 0 then 
                 if ld - ar-inv.tax-amt ne 0 then
                    assign
                       v-inv-tax = ar-inv.tax-amt *
                                   ((ld - ar-inv.tax-amt - ar-inv.freight) /
                                   (ld - ar-inv.tax-amt))
                       v-frt-tax = ar-inv.tax-amt *
                                   (ar-inv.freight / (ld - ar-inv.tax-amt)).
                 ELSE
                    ASSIGN 
                       v-inv-tax = 0
                       v-frt-tax = 0.

              else
                 assign
                    v-inv-tax    = ar-inv.tax-amt
                    v-frt-tax    = 0.

              IF v-inv-tax EQ ? THEN v-inv-tax = 0.
              IF v-frt-tax EQ ? THEN v-frt-tax = 0.

              if v-rate-t ne 0 then
                 v-tax-amt[1] = v-tax-amt[1] +
                               (v-inv-tax * (v-rate / v-rate-t)).

              if v-frtr-t ne 0 then
                 v-tax-amt[1] = v-tax-amt[1] +
                               (v-frt-tax * (v-frtr / v-frtr-t)).

              v-freight[1] = IF ar-inv.f-bill THEN ar-inv.freight ELSE 0.  /* OLD actual code */

           end.

           else
           if tt-report.key-04 eq stax.tax-acc[1] then do:
              find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.
              find first ar-cash
                  where ar-cash.c-no eq ar-cashl.c-no
                  no-lock no-error.

              if ar-cashl.actnum eq stax.tax-acc[1] then
               /* aj old v-tax-amt[1] = ar-cashl.amt-paid - ar-cashl.amt-disc.  */
                 v-tax-amt[1] = v-tax-amt[1] + 
                                ((ar-cashl.amt-paid - ar-cashl.amt-disc) *
                                 (v-rate / v-rate-t) ).
              else
                 v-sal-gro[1] = v-sal-gro[1] + ar-cashl.amt-paid - ar-cashl.amt-disc.
           end.      

        END. /* gdm - */

        IF /*last-of(tt-report.key-01) AND*/ /* gdm - */
           (v-sal-gro[1] ne 0 or
            v-tax-amt[1] ne 0 or
            v-freight[1] ne 0) then
        DO:
           display tt-report.key-02      
                 /* when first-of(tt-report.key-02)  gdm - */
                                         format "x(30)" 
                   ar-inv.inv-date       when avail ar-inv
                                         FORMAT "99/99/99"
                   ar-cash.check-date    when avail ar-cash  @ ar-inv.inv-date
                 space(2)
                   ar-inv.inv-no         when avail ar-inv
                   ar-cashl.inv-no       when avail ar-cashl @ ar-inv.inv-no
                   v-sal-gro[1]
                   v-tax-amt[1]
                   v-freight[1]
                   v-sal-gro[1] - /* - v-tax-amt[1] */ v-freight[1]
                                         format "->>,>>>,>>9.99"

               with frame detail no-box no-labels stream-io width 132.
           DOWN with frame detail no-box no-labels stream-io width 132.

           IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
               '"' (IF FIRST-OF(tt-report.key-02) THEN v-tax-dscr[1]
                    ELSE "")                                             '",'
               '"' (IF FIRST-OF(tt-report.key-02) THEN tt-report.key-02
                    ELSE "")                                             '",'
               '"' (IF AVAIL ar-cash AND ar-cash.check-date NE ? THEN
                       STRING(ar-cash.check-date,"99/99/9999")
                    ELSE IF AVAIL ar-inv AND ar-inv.inv-date NE ? THEN
                       STRING(ar-inv.inv-date,"99/99/9999")
                    ELSE "")                                             '",'
               '"' (IF avail ar-cashl THEN STRING(ar-cashl.inv-no)
                    ELSE IF AVAIL ar-inv THEN STRING(ar-inv.inv-no)
                    ELSE "")                                             '",'
               '"' STRING(v-sal-gro[1],"->>,>>>,>>9.99")                 '",'
               '"' STRING(v-tax-amt[1],"->>,>>>,>>9.99")                 '",'
               '"' STRING(v-freight[1],"->>,>>>,>>9.99")                 '",'
               '"' STRING(v-sal-gro[1]  - v-freight[1],"->>,>>>,>>9.99")  '",'
               SKIP.         

            ASSIGN 
               v-sal-gro[2] = v-sal-gro[2] + v-sal-gro[1]  
               v-tax-amt[2] = v-tax-amt[2] + v-tax-amt[1]  
               v-freight[2] = v-freight[2] + v-freight[1].
        END.

        ASSIGN
         v-sal-gro[1] = 0
         v-tax-amt[1] = 0
         v-freight[1] = 0.
      end.

      clear frame totals1 no-pause.

      display skip(1)
              "TOTALS:"                 to 47
              v-sal-gro[2]
              v-tax-amt[2]
              v-freight[2]
              v-sal-gro[2]  /* - v-tax-amt[2] */ - v-freight[2]
                                       format "->>,>>>,>>9.99"

          with frame totals1 no-box no-labels stream-io width 132.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
          '"' ""                                                    '",'
          '"' ""                                                    '",'
          '"' ""                                                    '",'
          '"' "TOTALS:"                                             '",'
          '"' STRING(v-sal-gro[2],"->>,>>>,>>9.99")                 '",'
          '"' STRING(v-tax-amt[2],"->>,>>>,>>9.99")                 '",'
          '"' STRING(v-freight[2],"->>,>>>,>>9.99")                 '",'
          '"' STRING(v-sal-gro[2] - v-freight[2],"->>,>>>,>>9.99")  '",'
          SKIP(1).

      ASSIGN               
        v-sal-gro[3] = v-sal-gro[3] + v-sal-gro[2] 
        v-tax-amt[3] = v-tax-amt[3] + v-tax-amt[2]
        v-freight[3] = v-freight[3] + v-freight[2]
        v-sal-gro[2] = 0
        v-tax-amt[2] = 0
        v-freight[2] = 0.
    end.  /* for each stax */

    display skip(5)
            "GRAND TOTALS:"             to 47
            v-sal-gro[3]
            v-tax-amt[3]
            v-freight[3]
            v-sal-gro[3] /* - v-tax-amt[3] */ - v-freight[3]
                                        format "->>,>>>,>>9.99"

          with frame totals2 no-box no-labels stream-io width 132.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
         '"' ""                                                    '",'
         '"' ""                                                    '",'
         '"' ""                                                    '",'
         '"' "GRAND TOTALS:"                                       '",'
         '"' STRING(v-sal-gro[3],"->>,>>>,>>9.99")                 '",'
         '"' STRING(v-tax-amt[3],"->>,>>>,>>9.99")                 '",'
         '"' STRING(v-freight[3],"->>,>>>,>>9.99")                 '",'
         '"' STRING(v-sal-gro[3] - v-freight[3] ,"->>,>>>,>>9.99")  '",'
         SKIP(1).

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

