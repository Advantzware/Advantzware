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

DEF TEMP-TABLE tt-report LIKE report.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_year begin_period ~
begin_date end_date rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_year begin_period begin_date ~
end_date rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_period AS INTEGER FORMAT ">9":U INITIAL 1 
     LABEL "For Period" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_year AS INTEGER FORMAT ">>>>":U INITIAL 9999 
     LABEL "For Year" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-mtdsub.csv" 
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
     SIZE 93 BY 8.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 7.62.

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
     begin_period AT ROW 4.57 COL 28 COLON-ALIGNED HELP
          "Enter Reporting Period"
     begin_date AT ROW 5.76 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 5.76 COL 63 COLON-ALIGNED HELP
          "Enter Ending Date"
     rd-dest AT ROW 10.05 COL 5 NO-LABEL
     lv-ornt AT ROW 10.05 COL 31 NO-LABEL
     lines-per-page AT ROW 10.05 COL 85 COLON-ALIGNED
     lv-font-no AT ROW 12.43 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 13.38 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 14.48 COL 31
     tb_excel AT ROW 15.76 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 15.76 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 16.57 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 18.14 COL 19
     btn-cancel AT ROW 18.14 COL 58
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 9.33 COL 3
     RECT-6 AT ROW 9.1 COL 2
     RECT-7 AT ROW 1.24 COL 2
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
         TITLE              = "MTD Destination Subtotals"
         HEIGHT             = 20.38
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
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_year:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* MTD Destination Subtotals */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* MTD Destination Subtotals */
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


&Scoped-define SELF-NAME begin_period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_period C-Win
ON VALUE-CHANGED OF begin_period IN FRAME FRAME-A /* For Period */
DO:
  RUN show-period-dates.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_year C-Win
ON VALUE-CHANGED OF begin_year IN FRAME FRAME-A /* For Year */
DO:
  RUN show-period-dates.
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

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
       THEN is-xprint-form = YES.     
  ELSE is-xprint-form = NO.

  run run-report. 
  STATUS DEFAULT "Processing Complete".
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
  end case.
  SESSION:SET-WAIT-STATE("").
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
  DISPLAY begin_year begin_period begin_date end_date rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_year begin_period begin_date end_date rd-dest 
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
/*     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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
/* ---------------------------------------------- ap/rep/monthdst.p 10/96 JLF */
/* MTD Destination Code Subtotals                                             */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

def buffer b-stax       for stax.

def var v-period        like uperiod init 1.
def var v-date          as   date extent 2 format "99/99/9999"
                             init [01/01/0001, today].                       
def var v-year          as   int.

def var v-tax-gl        as   char.
def var v-msf           as   int format ">>,>>9".
def var v-tons          as   int format ">>,>>9".
def var v-index         as   int format ">>>>9".
def var v-sal-amt       as   dec format "->>,>>>,>>9.99" extent 2.
def var v-mis-amt       as   dec format "->>,>>>,>>9.99" extent 2.
def var v-tax-amt       as   dec format "->>,>>>,>>9.99" extent 2.
def var v-actnum        like ar-cashl.actnum.
DEF VAR ld              AS   DEC.
DEF VAR excelheader AS CHAR NO-UNDO.
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

format header
       skip(1)
       "Dest Code"
       "Tax Jurisdiction Name"      at 11
       "Trade $"                    to 50
       "MSF"                        to 57
       "Tons"                       to 64
       "Index"                      to 70
       "Misc $"                     to 85
       "Sales Tax"                  to 100
       "Total $"                    to 115
       fill("-",132)                format "x(132)"

    with no-labels no-box no-underline stream-io width 132 frame f-top page-top.

{sa/sa-sls01.i}


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

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN tb_excel
         tb_runExcel
         fi_file.
END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(cFileName).
  excelheader = "Dest Code,Tax Jurisdiction Name,Trade $,MSF,Tons,"
              + "Index,Misc $,Sales Tax,Total $".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

    SESSION:SET-WAIT-STATE ("general").

    display "" with frame r-top.
    display "" with frame f-top.

    assign
     v-sal-amt = 0
     v-mis-amt = 0
     v-tax-amt = 0.

    for each cust where cust.company eq cocode no-lock:

        {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}
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
         tt-report.rec-id  = recid(ar-inv).
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
                and stax.tax-group  eq stax.tax-code1[1]
                and stax.tax-acc1[1] eq ar-cashl.actnum
              no-lock no-error.

          if avail stax then do:
            v-actnum = stax.tax-acc1[1].
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
            no-lock:

          create tt-report.
          assign
           tt-report.term-id = v-term
           tt-report.key-01  = ar-inv.tax-code
           tt-report.key-02  = if v-actnum ne "" then v-actnum else stax.tax-acc1[1]
           tt-report.rec-id  = recid(ar-cashl).
        end.
      end.
    end.

    for each tt-report
        where tt-report.term-id eq v-term
        break by tt-report.key-01:

      find first stax
          {sys/ref/staxW.i}
            and stax.tax-group eq tt-report.key-01
          no-lock no-error.

      find first ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock no-error.

      if avail ar-inv then do:
          {custom/statusMsg.i " 'Processing Invoice#  '  + string(ar-inv.inv-no) "}

        if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
          ld = ar-inv.net.
        else
          ld = ar-inv.gross.

        assign
         v-sal-amt[1] = v-sal-amt[1] + (ld - ar-inv.tax-amt)
         v-tax-amt[1] = v-tax-amt[1] + ar-inv.tax-amt
         v-tons       = v-tons       + ar-inv.t-weight.

        for each ar-invl
            where ar-invl.company       eq ar-inv.company
              and ar-invl.cust-no       eq ar-inv.cust-no
              and ar-invl.inv-no        eq ar-inv.inv-no
              and ar-invl.posted
            no-lock:
          if ar-invl.misc then
            assign
             v-sal-amt[1] = v-sal-amt[1] - ar-invl.amt
             v-mis-amt[1] = v-mis-amt[1] + ar-invl.amt.

          else do:
            /*if ar-invl.disc ne 0 then
              v-sal-amt[1] = v-sal-amt[1] -
                             ((ar-invl.amt / (1 - (ar-invl.disc / 100))) -
                              ar-invl.amt).*/

            if ar-inv.t-weight eq 0 then do:
              find first itemfg
                  where itemfg.company eq cocode
                    and itemfg.i-no    eq ar-invl.i-no
                  no-lock no-error.
              if avail itemfg then
                v-tons = v-tons + (itemfg.weight-100 * (ar-invl.inv-qty / 100)).
            end.
          end.
        end.
      end.

      else
      if tt-report.key-02 eq stax.tax-acc1[1] then do:
        find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.

        if ar-cashl.actnum eq stax.tax-acc1[1] then
          v-tax-amt[1] = v-tax-amt[1] + (ar-cashl.amt-paid - ar-cashl.amt-disc).
        else
          v-sal-amt[1] = v-sal-amt[1] + (ar-cashl.amt-paid - ar-cashl.amt-disc).
      end.

      if last-of(tt-report.key-01) then do:

        v-tons = round(v-tons / 2000,0).

        if v-sal-amt[1] ne 0 then
        DO:
          display tt-report.key-01
                  stax.tax-dscr1[1]    at 11
                  v-sal-amt[1]
                  v-msf
                  v-tons
                  v-index
                  v-mis-amt[1]
                  v-tax-amt[1]
                  v-sal-amt[1] + v-mis-amt[1] + v-tax-amt[1]
                                                format "->>,>>>,>>9.99"
                  fill("-",132)                 format "x(132)"

            with frame detail no-box no-labels stream-io width 132.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
               '"'  tt-report.key-01                                     '",'
               '"'  stax.tax-dscr1[1]                                     '",'
               '"'  STRING(v-sal-amt[1],"->>,>>>,>>9.99")                '",'
               '"'  STRING(v-msf,">>,>>9")                               '",'
               '"'  STRING(v-tons,">>,>>9")                              '",'
               '"'  STRING(v-index,">>>>9")                              '",'
               '"'  STRING(v-mis-amt[1],"->>,>>>,>>9.99")                '",'
               '"'  STRING(v-tax-amt[1],"->>,>>>,>>9.99")                '",'
               '"'  STRING(v-sal-amt[1] + v-mis-amt[1] + v-tax-amt[1],"->>,>>>,>>9.99") '",'
               SKIP.
        END.

        assign
         v-sal-amt[2] = v-sal-amt[2] + v-sal-amt[1]
         v-mis-amt[2] = v-mis-amt[2] + v-mis-amt[1]
         v-tax-amt[2] = v-tax-amt[2] + v-tax-amt[1]

         v-tons       = 0
         v-sal-amt[1] = 0
         v-mis-amt[1] = 0
         v-tax-amt[1] = 0.
      end.
    end.

    clear frame totals no-pause.

    display skip(1)
            "TOTALS:"                 at 11
            v-sal-amt[2]              to 50
            v-mis-amt[2]              to 85
            v-tax-amt[2]              to 100
            v-sal-amt[2] + v-mis-amt[2] + v-tax-amt[2]
                                      to 115    format "->>,>>>,>>9.99"

        with frame totals no-box no-labels stream-io width 132.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
         '"'  ""                                                   '",'
         '"'  "TOTALS:"                                            '",'
         '"'  STRING(v-sal-amt[2],"->>,>>>,>>9.99")                '",'
         '"'  ""                                                   '",'
         '"'  ""                                                   '",'
         '"'  ""                                                   '",' 
         '"'  STRING(v-mis-amt[2],"->>,>>>,>>9.99")                '",'
         '"'  STRING(v-tax-amt[2],"->>,>>>,>>9.99")                '",'
         '"'  STRING(v-sal-amt[2] + v-mis-amt[2] + v-tax-amt[2],"->>,>>>,>>9.99") '",'
         SKIP.

    IF tb_excel THEN DO:
       OUTPUT STREAM excel CLOSE.
       IF tb_runExcel THEN
          OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-period-dates C-Win 
PROCEDURE show-period-dates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST period
        WHERE period.company EQ gcompany
          AND period.yr      EQ INT(begin_year:SCREEN-VALUE)
          AND period.pnum    EQ INT(begin_period:SCREEN-VALUE)
        NO-LOCK NO-ERROR.

    IF AVAIL period THEN
      ASSIGN
       begin_date:SCREEN-VALUE = STRING(period.pst)
       end_date:SCREEN-VALUE   = STRING(IF period.pend LT TODAY THEN period.pend
                                                                ELSE TODAY).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

