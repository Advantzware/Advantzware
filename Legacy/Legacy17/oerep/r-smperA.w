&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-smperf.w

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

def TEMP-TABLE w-data NO-UNDO
  field w-sman-no  as   char format "x(3)"
  field w-sname    as   char format "x(19)"
  field w-sqft     like itemfg.t-sqft
  field w-amt      as   dec format "->,>>>,>>>,>>9.99" 
  field w-ptd-sqft like w-sqft
  field w-ptd-amt  like w-amt
  field w-ytd-sqft like w-sqft
  field w-ytd-amt  like w-amt.

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
def {1} SHARED var v-print-fmt  as char NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEFINE STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_ord-date begin_slsmn ~
end_slsmn rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_ord-date begin_slsmn end_slsmn ~
rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm ~
tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "From Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "To Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-smperf.csv" 
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
     SIZE 92 BY 10.48.

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
     begin_ord-date AT ROW 3.14 COL 28 COLON-ALIGNED
     begin_slsmn AT ROW 4.33 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 4.33 COL 65 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     rd-dest AT ROW 10.76 COL 6 NO-LABEL
     lv-ornt AT ROW 11.24 COL 31 NO-LABEL
     lines-per-page AT ROW 11.24 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 13.14 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 14.1 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16 COL 30
     tb_excel AT ROW 17.19 COL 30
     tb_runExcel AT ROW 17.19 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 18.14 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 20.76 COL 17
     btn-cancel AT ROW 20.76 COL 61
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.05 COL 5
     RECT-6 AT ROW 9.81 COL 2
     RECT-7 AT ROW 1 COL 1
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
         TITLE              = "Sales Rep Performance"
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
       begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
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
ON END-ERROR OF C-Win /* Salesmen Performance */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Salesmen Performance */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-date C-Win
ON LEAVE OF begin_ord-date IN FRAME FRAME-A /* Order Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* From Sales Rep# */
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
  SESSION:SET-WAIT-STATE ("general").

  assign rd-dest.

 run run-report. 
 STATUS DEFAULT "Processing Complete".
  SESSION:SET-WAIT-STATE ("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= " "
                            &begin_cust=begin_slsmn
                            &END_cust= begin_slsmn
                            &fax-subject="Sales Rep Performance"
                            &fax-body="Job Log"
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = " "
                             &begin_cust= begin_slsmn
                             &END_cust=begin_slsmn
                             &mail-subject="Sales Rep Performance"
                             &mail-body="Sales Rep Performance"
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = " "
                                  &begin_cust= begin_slsmn
                                  &END_cust=begin_slsmn
                                  &mail-subject="Sales Rep Performance"
                                  &mail-body="Sales Rep Performance"
                                  &mail-file=list-name }

           END.
       END. 
      WHEN 6 THEN RUN output-to-port.
  end case. 
  SESSION:SET-WAIT-STATE ("").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* To Sales Rep# */
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

  begin_ord-date = today.

  RUN enable_UI.

  {methods/nowait.i}

  APPLY "entry" TO begin_ord-date IN FRAME {&FRAME-NAME}.

    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images1.p */
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
  DISPLAY begin_ord-date begin_slsmn end_slsmn rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_ord-date begin_slsmn end_slsmn rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel 
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

     IF NOT OKpressed THEN  RETURN NO-APPLY.
   */
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
 RUN custome/d-print.w (list-name).
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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/dev/asi/oe/rep
**       By: Christopher A. Heins, 07.14.95
** Descript: Sales Rep Performance daily, period and year to date.
**
*****************************************************************************
\***************************************************************************/

{sys/form/r-topw.f}

def var fdate as date format "99/99/9999".
def var tdate as date format "99/99/9999".
def var fsman as char format "x(3)".
def var tsman as char format "x(3)".
def var v-sq-ft like itemfg.t-sqft  format "->>>,>>>,>>9.999".
def var v-gtot-sqft      like w-sqft.
def var v-gtot-amt       like w-amt.
def var v-gtot-ptd-sqft  like v-gtot-sqft.
def var v-gtot-ptd-amt   like v-gtot-amt.
def var v-gtot-ytd-sqft  like v-gtot-sqft.
def var v-gtot-ytd-amt   like v-gtot-amt.
def var v-sman-no like w-sman-no.
def var v-sqft like itemfg.t-sqft.
def var v-amt  like oe-ord.t-revenue.
def var v-ptd-first as date.
def var v-ptd-last  as date.
def var v-ytd-first as date.
def var v-ytd-last  as date.
def var v-period as int.
def var v-year as int format "9999".
def var v-qty like oe-ordl.qty format "->>>,>>9.99".
def var cnt as int.
def var v-exclude as log.
def var v-pct as dec format "99.99".
def var v-misc as log.

DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

format w-sman-no    column-label "No"
       w-sname      column-label "Name"
       w-sqft       column-label "Sq Ft/M"
       w-amt        column-label "Amount"
       space(2)
       w-ptd-sqft   column-label "Sq Ft/M"
       w-ptd-amt    column-label "Amount"
       space(2)
       w-ytd-sqft   column-label "Sq Ft/M"
       w-ytd-amt    column-label "Amount"

    header skip(1)
           "    Sales Rep"
           "---------Daily-----------" at 25 space(2)
           "-----Period to date------" space(2)
           "-------Year to date------"

    with no-box frame itemx down stream-io width 132.

assign
 fdate      = today
 tdate      = today
 v-ytd-last = today.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 fdate = begin_ord-date
 fsman = begin_slsmn
 tsman = end_slsmn.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

EMPTY TEMP-TABLE w-data.

IF tb_excel THEN 
DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = 
  "No,Sales Rep Name,Daily Sq Ft/M,Amount,PTD Sq Ft/M,Amount,YTD Sq Ft/M,Amount".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

display "" with frame r-top.

    find first period
        where period.company eq cocode
          and period.pst     le tdate
          and period.pend    ge tdate
        no-lock no-error.
    if avail period then
      assign
       v-ptd-first = period.pst
       v-ptd-last  = period.pend
       v-period    = period.pnum
       v-year      = period.yr.
    else
      assign
       v-ptd-first = tdate
       v-ptd-last  = tdate
       v-period    = month(today) 
       v-year      = year(today).

    find first company where company.company eq cocode no-lock.

    find first period
        where period.company eq cocode
          and period.yr      eq v-year
        no-lock no-error.

    assign
     v-ytd-first = if avail period then period.pst
                                   else date(1,1,year(tdate))
     v-ytd-last  = tdate.

    for each oe-ord
        where oe-ord.company  eq cocode
          and oe-ord.ord-date ge v-ytd-first
          and oe-ord.ord-date le v-ytd-last,

        each oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-ord.ord-no
        no-lock:

      v-exclude = yes.
      do cnt = 1 to 3:
        if v-exclude                   and
           oe-ordl.s-man[cnt] ge fsman and
           oe-ordl.s-man[cnt] le tsman then v-exclude = no.
      end. /* do cnt.. */
      if v-exclude then next.

      /* At this point we have either 1, 2 or 3 valid Sales Rep, in any  */
      /* combination of the array. */

      v-misc = no.
      do cnt = 1 to 3:
        if v-misc then leave.

        if oe-ordl.s-man[cnt] lt fsman or
           oe-ordl.s-man[cnt] gt tsman then next.

        /* If no Sales Rep number then assign to misc, ie, blank no */
        if cnt eq 1               and 
           oe-ordl.s-man[1] eq "" and
           oe-ordl.s-man[2] eq "" and
           oe-ordl.s-man[3] eq "" then
          assign
           v-sman-no = "MISC"
           v-amt     = oe-ordl.t-price
           v-pct     = 1
           v-misc    = yes.

        else   /* If blank Sales Rep # then ignore */
        if oe-ordl.s-man[cnt] eq "" then next.

        /* There must be at least 1 Sales Rep in either pos'n 1, 2 or 3 */
        else do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq oe-ordl.i-no
              no-lock no-error.
          if avail itemfg then
            assign
             v-sman-no = oe-ordl.s-man[cnt]
             v-pct     = oe-ordl.s-pct[cnt] / 100
             v-qty     = oe-ordl.qty / 1000
             v-sqft    = itemfg.t-sqft * (v-qty * v-pct)
             v-amt     = (oe-ordl.t-price * v-pct).
        end.
        {custom/statusMsg.i "'Processing Sales Rep # ' + v-sman-no"}
        find first w-data where w-sman-no eq v-sman-no no-error.
        if not avail w-data then do:
          find first sman
              where sman.company eq cocode
                and sman.sman    eq v-sman-no
              no-lock no-error.
          create w-data.
          assign
           w-sman-no = v-sman-no
           w-sname   = if avail sman then sman.sname else "NOT ON FILE".
        end.

        assign
         w-ytd-sqft = w-ytd-sqft + v-sqft
         w-ytd-amt  = w-ytd-amt  + v-amt.

        if oe-ord.ord-date ge v-ptd-first and
           oe-ord.ord-date le v-ptd-last  then
          assign
           w-ptd-sqft = w-ptd-sqft + v-sqft
           w-ptd-amt  = w-ptd-amt  + v-amt.

        if tdate eq oe-ord.ord-date then
          assign
           w-sqft = w-sqft + v-sqft
           w-amt  = w-amt  + v-amt.
      end. /* do cnt = 1 to 3... */
    end.  /* for each oe-ord */


    for each w-data where w-sman-no ne "MISC" break by w-sname
        with frame itemx:
      {custom/statusMsg.i "'Processing Sales Rep # ' + w-sman-no"}
      put skip(1).
      display w-sman-no
              w-sname
              w-sqft
              w-amt
              w-ptd-sqft
              w-ptd-amt
              w-ytd-sqft
              w-ytd-amt

         with frame itemx down.
      down with frame itemx.

      IF tb_excel THEN 
        EXPORT STREAM excel DELIMITER "," w-sman-no w-sname w-sqft 
                                          w-amt w-ptd-sqft w-ptd-amt 
                                          w-ytd-sqft w-ytd-amt.

      assign
       v-gtot-sqft     = v-gtot-sqft     + w-sqft
       v-gtot-amt      = v-gtot-amt      + w-amt
       v-gtot-ptd-sqft = v-gtot-ptd-sqft + w-ptd-sqft
       v-gtot-ptd-amt  = v-gtot-ptd-amt  + w-ptd-amt
       v-gtot-ytd-sqft = v-gtot-ytd-sqft + w-ytd-sqft
       v-gtot-ytd-amt  = v-gtot-ytd-amt  + w-ytd-amt.
    end.  /* for each ... */

    /* print totals the first time */
    underline w-sqft
              w-amt
              w-ptd-sqft
              w-ptd-amt
              w-ytd-sqft
              w-ytd-amt
        with frame itemx.

    display "   TOTAL SALES"    @ w-sname
            v-gtot-sqft         @ w-sqft
            v-gtot-amt          @ w-amt
            v-gtot-ptd-sqft     @ w-ptd-sqft
            v-gtot-ptd-amt      @ w-ptd-amt
            v-gtot-ytd-sqft     @ w-ytd-sqft
            v-gtot-ytd-amt      @ w-ytd-amt

        with frame itemx.
   ASSIGN v-gtot-sqft = 0
          v-gtot-amt = 0
          v-gtot-ptd-sqft = 0
          v-gtot-ptd-amt = 0
          v-gtot-ytd-sqft = 0
          v-gtot-ytd-amt = 0.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.
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

