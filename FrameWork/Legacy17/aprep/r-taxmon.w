&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aprep\r-taxmon.w

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

def TEMP-TABLE w-tax no-undo
     field tax-gr like ar-inv.tax-code
     field rec-id as   recid.

DEF TEMP-TABLE tt-stax NO-UNDO LIKE stax.
DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

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

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-taxmon.csv" 
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
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.29.

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
     begin_period AT ROW 4.57 COL 28 COLON-ALIGNED HELP
          "Enter Reporting Period"
     begin_date AT ROW 5.76 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 5.76 COL 63 COLON-ALIGNED HELP
          "Enter Ending Date"
     rd-dest AT ROW 9.81 COL 6 NO-LABEL
     lv-ornt AT ROW 10.05 COL 32 NO-LABEL
     lines-per-page AT ROW 10.05 COL 85 COLON-ALIGNED
     lv-font-no AT ROW 11.48 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 12.43 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 13.67 COL 31
     tb_excel AT ROW 15.57 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 15.57 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 16.38 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 19.1 COL 19
     btn-cancel AT ROW 19.1 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 9.1 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     RECT-6 AT ROW 8.57 COL 1.4
     RECT-7 AT ROW 1 COL 1.4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.48.


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
         TITLE              = "Monthly Tax Report"
         HEIGHT             = 21.76
         WIDTH              = 95.8
         MAX-HEIGHT         = 53.57
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 53.57
         VIRTUAL-WIDTH      = 384
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
ON END-ERROR OF C-Win /* Monthly Tax Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Monthly Tax Report */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  {&WINDOW-NAME}:WINDOW-STATE = WINDOW-minIMIZE.    

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.


  assign rd-dest.
  IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
       THEN is-xprint-form = YES.     
  ELSE is-xprint-form = NO.

  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= 'begin_cust=begin_year'
                            &begin_cust= "begin_year"
                            &END_cust= "begin_year" 
                            &fax-subject="Monthly Tax Report"
                            &fax-body="Monthly Tax Report"
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = ''
                             &begin_cust=''
                             &END_cust='' 
                             &mail-subject="Monthly Tax Report"
                             &mail-body="Monthly Tax Report"
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust='' 
                                  &mail-subject="Monthly Tax Report"
                                  &mail-body="Monthly Tax Report"
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  end case.
   current-window:WINDOW-STATE  = WINDOW-NORMAL.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_year.
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
/* ---------------------------------------------- ap/rep/monthtax.p 10/96 JLF */
/* Monthly Tax tt-report                                                         */
/* -------------------------------------------------------------------------- */

   {sys/form/r-topw2.f}

   def var v-period        like uperiod init 1.
   def var v-date          as   date extent 2 format "99/99/9999"
                                init [01/01/0001, today].                       
   def var v-year          as   int no-undo.
   def var v-tax-gl        as   char.
   def var v-tax-gr        like stax.tax-gr extent 10.
   def var v-head1         as   char format "x(30)"  extent 5.
   def var v-head2         as   char format "x(30)"  extent 5.
   def var v-head3         as   char format "x(30)"  extent 5.
   def var v-sal-amt       as   dec format "->>,>>>,>>9.99" extent 2.
   def var v-taxable       as   dec format "->>,>>>,>>9.99" extent 10.
   def var v-tax-amt       as   dec format "->>,>>>,>>9.99" extent 10.
   def var v-rate          like stax.tax-rate1.
   def var v-frtr          like stax.tax-rate1.
   def var v-frtr-s        AS   DEC.
   def var v-rate-t        as   dec.
   DEF VAR v-rate-tt       AS   DEC EXTENT 5.
   DEF VAR v-frtr-tt       AS   DEC EXTENT 5.
   def var v-frtr-t        as   dec.
   def var v-inv-tax       as   dec.
   def var v-frt-tax       as   dec.
   def var v-actnum        like ar-cashl.actnum.
   DEF VAR ld              AS   DEC.
   DEF VAR excelheader1    AS CHAR NO-UNDO.
   DEF VAR excelheader2    AS CHAR NO-UNDO.
   DEF VAR v-tot-tax       AS DEC NO-UNDO.
   DEF VAR v-tot-taxable   AS DEC NO-UNDO.



   IF tb_excel THEN
     OUTPUT STREAM excel TO VALUE(fi_file).

   format header
      skip(1)
      v-head1[1]        at 43
      space(0)
      v-head1[2]
      space(0)
      v-head1[3] 
      space(0)
      v-head1[4]
      space(0)
      v-head1[5]      
      skip
      "Code Name"
      "Gross Sales $"   at 30
      space(0)
      v-head2[1]        at 43
      space(0)
      v-head2[2]
      space(0)
      v-head2[3] 
      space(0)
      v-head2[4] 
      space(0)
      v-head2[5]       
      skip
      fill("-",42)      format "x(42)"
      space(0)
      v-head3[1]        at 43
      space(0)
      v-head3[2]
      space(0)
      v-head3[3]               
      space(0)
      v-head3[4]    
      space(0)
      v-head3[5]       
      SKIP
      WITH FRAME r-top.

   {sa/sa-sls01.i}


   assign
      str-tit2 = c-win:title
      {sys/inc/ctrtext.i str-tit2 172}

      v-period  = begin_period
      v-date[1] = begin_date
      v-date[2] = end_date. 

   {sys/inc/print1.i}

   {sys/inc/outprint.i value(lines-per-page)}

   if td-show-parm then run show-param.

   SESSION:SET-WAIT-STATE ("general").

   for each stax
            {sys/ref/staxW.i}
            and stax.tax-group  eq stax.tax-code1[1]
            and stax.tax-acc1[1] GT v-tax-gl no-lock
             by stax.tax-acc1[1]:
      CREATE tt-stax.
      BUFFER-COPY stax TO tt-stax NO-ERROR.
   END.

   for each cust where cust.company eq cocode no-lock:
      for each ar-inv where ar-inv.company        eq cocode
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
      END. /* for each ar-inv */

      for each ar-cash where ar-cash.company    eq cocode
                         and ar-cash.cust-no    eq cust.cust-no
                         and ar-cash.check-date ge v-date[1]
                         and ar-cash.check-date le v-date[2]
                         and ar-cash.posted     eq yes
                         use-index ar-cash no-lock:

         v-actnum = "".

         for each ar-cashl where ar-cashl.c-no   eq ar-cash.c-no
                             and ar-cashl.posted eq yes
                             and ar-cashl.memo   eq yes
                             use-index c-no no-lock,
            first ar-inv where ar-inv.company  eq cocode
                           and ar-inv.inv-no   eq ar-cashl.inv-no
                           and ar-inv.tax-code ne "" no-lock:

            find first stax
                       {sys/ref/staxW.i}
                       and stax.tax-group  eq stax.tax-code1[1]
                       and stax.tax-acc1[1] eq ar-cashl.actnum no-lock no-error.

            if avail stax then do:
               v-actnum = stax.tax-acc1[1].
               leave.
            end.
         end. /* for each ar-cashl */

         for each ar-cashl where ar-cashl.c-no   eq ar-cash.c-no
                             and ar-cashl.posted eq yes
                             and ar-cashl.memo   eq yes
                             use-index c-no no-lock,
            first ar-inv where ar-inv.company  eq cocode
                           and ar-inv.inv-no   eq ar-cashl.inv-no
                           and ar-inv.tax-code ne "" no-lock,
            first stax
                  {sys/ref/staxW.i}
                  and stax.tax-group eq ar-inv.tax-code no-lock:

            create tt-report.
            assign
               tt-report.term-id = v-term
               tt-report.key-01  = ar-inv.tax-code
               tt-report.key-02  = if v-actnum ne "" then v-actnum else stax.tax-acc1[1]
               tt-report.rec-id  = recid(ar-cashl).
         end. /* for each ar-cashl */
      end. /* for each ar-cash */
   end. /* for each cust */

   VIEW FRAME r-top.

   do while true:
      assign
         v-tax-gr = ""
         v-head1  = ""
         v-head2  = ""
         v-head3  = ""
         i        = 0.
      for each tt-stax WHERE tt-stax.company = gcompany
                by tt-stax.tax-acc1[1]:

         assign
            i           = i + 1
            x           = (27 - length(trim(substr(tt-stax.tax-dscr[1],1,23)))) / 2
            v-head1[i]  = "   " + fill("*",x - 1) + " " +
                                  trim(substr(tt-stax.tax-dscr[1],1,23)) +
                                  " " + fill("*",x - 1)
            v-head2[i]  = "      Taxable $          Tax $"
            v-head3[i]  = fill("-",30)
            v-tax-gr[i] = tt-stax.tax-group
            v-tax-gl    = tt-stax.tax-acc1[1].

         DELETE tt-stax.
         if i gt 4 then leave.

      end. /* for each stax */

      if v-tax-gr[1] eq "" then leave.

      IF tb_excel THEN DO:
         ASSIGN
            excelheader1 = ",,," + v-head1[1] + ",," + v-head1[2] + ",," + v-head1[3] + ",," + v-head1[4] + ",," + v-head1[5]
            excelheader2 = "Code,Name,Gross Sales,Taxable $,Tax $,Taxable $,Tax $," + "Taxable $,Tax $," + "Taxable $,Tax $," + "Taxable $,Tax $".
         PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader1,',','","') '"' SKIP.
         PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader2,',','","') '"' SKIP.
      END.

      page.

      assign
         v-sal-amt = 0
         v-taxable = 0
         v-tax-amt = 0.

      for each tt-report where tt-report.term-id eq v-term
                      break by tt-report.key-01:

         if first-of(tt-report.key-01) then do:
            assign
               v-rate   = 0
               v-frtr   = 0.

         find first stax
                    {sys/ref/staxW.i}
                    and stax.tax-group eq tt-report.key-01 no-lock no-error.

         DO i = 1 TO 5:
            IF v-tax-gr[i] NE "" THEN DO:
               ASSIGN
                  v-rate-t = 0
                  v-frtr-t = 0
                  v-frtr-s = 0.
               DO j = 1 TO 5:
                  IF v-tax-gr[i] EQ stax.tax-code1[j] THEN DO:
                     v-rate[i] = v-rate[i] + stax.tax-rate1[j].
                     IF stax.tax-frt1[j] THEN DO:
                        ASSIGN 
                          v-frtr[i] = v-frtr[i] + stax.tax-rate1[j]
                          v-frtr-s = v-frtr-s + stax.tax-rate1[j].
                     END.

                  END.
                  ASSIGN
                     v-rate-t = v-rate-t + stax.tax-rate1[j]
                     v-frtr-t = v-frtr-t + stax.tax-rate1[j].
                 END.  
                 v-rate-tt[i] = v-rate-t.
                 IF stax.tax-frt1[i] THEN                   
                    v-frtr-tt[i] = v-frtr-t.
               END.
            END.
         END.

         find first ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock no-error.

         if avail ar-inv then do:
            if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
               ld = ar-inv.net.
            else
               ld = ar-inv.gross.
            v-sal-amt[1] = v-sal-amt[1] + (ld - ar-inv.tax-amt).
            DO i = 1 TO 5:
               IF v-rate[i] EQ 0 THEN NEXT.

               v-taxable[i] = v-taxable[i] + (ld - ar-inv.tax-amt).
               IF ar-inv.f-bill AND v-frtr-s > 0 THEN
                  IF ld - ar-inv.tax-amt NE 0 THEN DO:

                     assign
                        v-inv-tax = ar-inv.tax-amt * ((ld - ar-inv.tax-amt - ar-inv.freight) / (ld - ar-inv.tax-amt))
                        v-frt-tax = ar-inv.tax-amt * (ar-inv.freight / (ld - ar-inv.tax-amt)).
                  END.
                  else.

               else
                  assign
                     v-inv-tax    = ar-inv.tax-amt
                     v-frt-tax    = 0.

               if v-rate-tt[i] ne 0 then
                  v-tax-amt[i] = v-tax-amt[i] + (v-inv-tax * (v-rate[i] / v-rate-tt[i])).

               if v-frtr-tt[i] ne 0 THEN DO:
                   v-tax-amt[i] = v-tax-amt[i] + (v-frt-tax * (v-frtr[i] / v-frtr-tt[i])).
              END.

            END. /* DO i = 1 TO 5: */


            for each ar-invl FIELDS(amt tax t-freight) where ar-invl.company       eq ar-inv.company
                                               and ar-invl.cust-no       eq ar-inv.cust-no
                                               and ar-invl.inv-no        eq ar-inv.inv-no
                                               and ar-invl.posted no-lock:

               do i = 1 to 5:
                  if v-rate[i] eq 0 then next.
                  if not ar-invl.tax THEN DO:

                     v-taxable[i] = v-taxable[i] - ar-invl.amt.

                  END.
/*                   ELSE                                                 */
/*                       IF v-frtr[i] = 0 AND ar-inv.freight > 0 THEN     */
/*                          v-taxable[i] = v-taxable[i] - ar-inv.freight. */
               end.
            end. /* for each ar-invl */
         end. /* avail ar-inv */
         else
            if tt-report.key-02 eq stax.tax-acc1[1] then do:
/*        Per Julie, take out anything related to ar-cashl in this column - reversed with 04221408*/
               find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.
/*             
               if ar-cashl.actnum eq stax.tax-acc1[1] then
                  do i = 1 to 5:
                     if v-rate[i] eq 0 then next.

                     v-inv-tax = (ar-cashl.amt-paid - ar-cashl.amt-disc).

                     if v-rate-tt[i] ne 0 then
                        v-tax-amt[i] = v-tax-amt[i] + (v-inv-tax * (v-rate[i] / v-rate-tt[i])).
                  end.
               else do:*/
                  v-sal-amt[1] = v-sal-amt[1] + (ar-cashl.amt-paid - ar-cashl.amt-disc).
/*                  do i = 1 to 5:
                     if v-rate[i] eq 0 then next.

                     if v-rate-tt[i] ne 0 then
                        v-taxable[i] = v-taxable[i] + ((ar-cashl.amt-paid - ar-cashl.amt-disc) * (v-rate[i] / v-rate-tt[i])).

                  end.
               end. /* else do: */*/
            end. /* if tt-report.key-02 eq stax.tax-acc1[1] */

         if last-of(tt-report.key-01) then do:
            display 
               stax.tax-group    format "x(4)"
               stax.tax-dscr[1]  at 6    format "x(22)"
               v-sal-amt[1]
               v-taxable[1]
               v-tax-amt[1]
               v-taxable[2]      when v-tax-gr[2] ne ""
               v-tax-amt[2]      when v-tax-gr[2] ne ""
               v-taxable[3]      when v-tax-gr[3] ne ""
               v-tax-amt[3]      when v-tax-gr[3] ne ""
               v-taxable[4]      when v-tax-gr[4] ne ""
               v-tax-amt[4]      when v-tax-gr[4] ne ""
               v-taxable[5]      when v-tax-gr[5] ne ""
               v-tax-amt[5]      when v-tax-gr[5] ne ""
                  with frame detail no-box no-labels stream-io width 250.

            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                  '"' stax.tax-group                           '",'
                  '"' stax.tax-dscr[1]                         '",'
                  '"' STRING(v-sal-amt[1],"->>,>>>,>>9.99")    '",'
                  '"' STRING(v-taxable[1],"->>,>>>,>>9.99")    '",'
                  '"' STRING(v-tax-amt[1],"->>,>>>,>>9.99")    '",'
                  '"' IF v-tax-gr[2] NE "" THEN
                        STRING(v-taxable[2],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  '"' IF v-tax-gr[2] NE "" THEN
                        STRING(v-tax-amt[2],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  '"' IF v-tax-gr[3] NE "" THEN
                        STRING(v-taxable[3],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  '"' IF v-tax-gr[3] NE "" THEN
                        STRING(v-tax-amt[3],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  '"' IF v-tax-gr[4] NE "" THEN
                        STRING(v-taxable[4],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  '"' IF v-tax-gr[4] NE "" THEN
                        STRING(v-tax-amt[4],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  '"' IF v-tax-gr[5] NE "" THEN
                        STRING(v-taxable[5],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  '"' IF v-tax-gr[5] NE "" THEN
                        STRING(v-tax-amt[5],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  SKIP.

            assign
               v-sal-amt[2] = v-sal-amt[2] + v-sal-amt[1]
               v-sal-amt[1] = 0.

            do i = 1 to 5:
               ASSIGN
                  v-taxable[i + 5] = v-taxable[i + 5] + v-taxable[i]
                  v-tax-amt[i + 5] = v-tax-amt[i + 5] + v-tax-amt[i]
                  v-taxable[i] = 0
                  v-tax-amt[i] = 0.

            END.
         end. /* last-of(tt-report.key-01) then do: */
      end. /* for each tt-report */

      clear frame totals no-pause.

      display skip(1)
         "TOTALS:"                 at 6
         v-sal-amt[2]              to 42
         v-taxable[6]
         v-tax-amt[6]
         v-taxable[7]              when v-tax-gr[2] ne ""
         v-tax-amt[7]              when v-tax-gr[2] ne ""
         v-taxable[8]              when v-tax-gr[3] ne ""
         v-tax-amt[8]              when v-tax-gr[3] ne ""
         v-taxable[9]              when v-tax-gr[4] ne ""
         v-tax-amt[9]              when v-tax-gr[4] ne ""
         v-taxable[10]              when v-tax-gr[5] ne ""
         v-tax-amt[10]              when v-tax-gr[5] ne ""

         with frame totals no-box no-labels stream-io width 250.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
            SKIP(1)
            '"' "TOTALS:"                                '",'
            '"' ""                                       '",'
            '"' STRING(v-sal-amt[2],"->>,>>>,>>9.99")    '",'
            '"' STRING(v-taxable[6],"->>,>>>,>>9.99")    '",'
            '"' STRING(v-tax-amt[6],"->>,>>>,>>9.99")    '",'
            '"' IF v-tax-gr[2] NE "" THEN
                  STRING(v-taxable[7],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            '"' IF v-tax-gr[2] NE "" THEN
                  STRING(v-tax-amt[7],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            '"' IF v-tax-gr[3] NE "" THEN
                  STRING(v-taxable[8],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            '"' IF v-tax-gr[3] NE "" THEN
                  STRING(v-tax-amt[8],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            '"' IF v-tax-gr[4] NE "" THEN
                  STRING(v-taxable[9],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            '"' IF v-tax-gr[4] NE "" THEN
                  STRING(v-tax-amt[9],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            '"' IF v-tax-gr[5] NE "" THEN
                  STRING(v-taxable[10],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            '"' IF v-tax-gr[5] NE "" THEN
                  STRING(v-tax-amt[10],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            SKIP(1).

   end.   /* do while true */

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

