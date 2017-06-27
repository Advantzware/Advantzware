&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ar\r-mise&p.w

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
DEF VAR list-name as CHAR no-undo.
DEF VAR init-dir AS CHAR NO-UNDO.
DEF VAR lv-comp-curr AS CHAR NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

assign
 cocode = gcompany
 locode = gloc.

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
IF AVAIL company THEN lv-comp-curr = company.curr-code.

DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.

def var time_stamp as ch NO-UNDO.
def var qfirst as l NO-UNDO.
def var post as logical format "Yes/No"
                        label "   Post to G/L files?   " initial no NO-UNDO.
def var xtrnum as int NO-UNDO.
def var xcs-acct as char NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.

def TEMP-TABLE w-bank NO-UNDO
   field bank   like bank.bank-code  
   field actnum like account.actnum  
   field bal    like bank.bal  .

DEF TEMP-TABLE tt-post NO-UNDO FIELD row-id AS ROWID
                               FIELD ex-rate LIKE currency.ex-rate INIT 1
                               FIELD curr-amt LIKE ar-cash.check-amt
                               FIELD actnum LIKE account.actnum.

DO TRANSACTION:
  {sys/inc/postdate.i}
END.

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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date rd-dest ~
lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date rd-dest lv-ornt ~
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
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-mishst.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
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
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.81.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date AT ROW 4.81 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date"
     end_date AT ROW 4.81 COL 70 COLON-ALIGNED HELP
          "Enter Ending Invoice Date"
     rd-dest AT ROW 11.48 COL 3 NO-LABEL
     lv-ornt AT ROW 11.71 COL 30 NO-LABEL
     lines-per-page AT ROW 11.71 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 13.14 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 14.1 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.52 COL 10
     tb_excel AT ROW 15.52 COL 67 RIGHT-ALIGNED WIDGET-ID 2
     tb_runExcel AT ROW 15.52 COL 90 RIGHT-ALIGNED WIDGET-ID 4
     fi_file AT ROW 16.71 COL 45 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 6
     btn-ok AT ROW 18.38 COL 23
     btn-cancel AT ROW 18.38 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.52 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 10.05 COL 1
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
         TITLE              = "Miscellaneous Cash Receipts Report"
         HEIGHT             = 20.19
         WIDTH              = 95.4
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
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
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


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Miscellaneous Cash Receipts Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Miscellaneous Cash Receipts Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Receipt Date */
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
  DEF VAR lv-post AS LOG NO-UNDO.
  DEF VAR op-error AS LOG NO-UNDO.

  /* run check-date. 
  if v-invalid then
     return no-apply.*/

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN {&DISPLAYED-OBJECTS}.
  END.
/*
  DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
    /* gdm - 11050906 */
    loop:
    REPEAT:
       FIND FIRST gl-ctrl EXCLUSIVE-LOCK
         WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.

       IF AVAIL gl-ctrl THEN DO:
          ASSIGN xtrnum        = gl-ctrl.trnum + 1
                 gl-ctrl.trnum = xtrnum.
          FIND CURRENT gl-ctrl NO-LOCK.
          RELEASE gl-ctrl.
          LEAVE loop.
       END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  END.
*/                                 


  run run-report(OUTPUT op-error).
  STATUS DEFAULT "Processing Complete".

  IF op-error = NO THEN
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust= fi_file
                            &END_cust= fi_file
                            &fax-subject="Miscellaneous Cash Receipts Report"
                            &fax-body="Miscellaneous Cash Receipts Report"
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust= fi_file
                             &END_cust= fi_file
                             &mail-subject="Miscellaneous Cash Receipts Report"
                             &mail-body="Miscellaneous Cash Receipts Report"
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust= fi_file
                                  &END_cust= fi_file
                                  &mail-subject="Miscellaneous Cash Receipts Report"
                                  &mail-body="Miscellaneous Cash Receipts Report"
                                  &mail-file=list-name }
           END.

       END. 
  END CASE.
/*
  IF v-postable THEN DO:
     lv-post = NO.

     MESSAGE "Post Miscellaneous Cash Receipts?"
             VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
             UPDATE lv-post.

     IF lv-post THEN do:      
        RUN post-gl.
        MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
     END.
     ELSE RUN undo-trnum.  
  END.

  ELSE DO:
     MESSAGE "No Miscellaneous Cash Receipts available for posting..."
        VIEW-AS ALERT-BOX ERROR.
     RUN undo-trnum.
  END.
*/  
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Receipt Date */
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

  RUN init-proc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  RUN enable_UI.

  DO WITH FRAME {&frame-name}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
/*
    IF postdate-log THEN DO:
      ASSIGN
       tran-date:SCREEN-VALUE = STRING(TODAY)
       tran-date              = TODAY.
      RUN check-date.
    END.

    ELSE
      ASSIGN
       tran-date:SCREEN-VALUE   = ""
       tran-period:SCREEN-VALUE = "".

    APPLY "entry" TO tran-date.
    */
  END.

  {methods/nowait.i}
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  DO with frame {&frame-name}:
    v-invalid = no.

    find first period                   
        where period.company eq cocode
          and period.pst     le tran-date
          and period.pend    ge tran-date
        no-lock no-error.
    if avail period then do:
       IF NOT period.pstat THEN DO:
          MESSAGE "Period Already Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.
        tran-period:SCREEN-VALUE = string(period.pnum).
    END.

    ELSE DO:
      message "No Defined Period Exists for" tran-date view-as alert-box error.
      v-invalid = yes.
    end.
  END. */
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
  DISPLAY begin_date end_date rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date end_date rd-dest lv-ornt lines-per-page 
         lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc C-Win 
PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

do :
 find first ar-ctrl where ar-ctrl.company = cocode NO-LOCK.
 if not available ar-ctrl then return.
 xcs-acct = ar-ctrl.cash-act.
 release ar-ctrl.
end.

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
/*      DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.      */
/*                                                         */
/*      if init-dir = "" then init-dir = "c:\temp" .       */
/*      SYSTEM-DIALOG GET-FILE list-name                   */
/*          TITLE      "Enter Listing Name to SAVE AS ..." */
/*          FILTERS    "Listing Files (*.rpt)" "*.rpt",    */
/*                     "All Files (*.*)" "*.*"             */
/*          INITIAL-DIR init-dir                           */
/*          ASK-OVERWRITE                                  */
/*     /*     CREATE-TEST-FILE*/                           */
/*          SAVE-AS                                        */
/*          USE-FILENAME                                   */
/*                                                         */
/*          UPDATE OKpressed.                              */
/*                                                         */
/*      IF NOT OKpressed THEN  RETURN NO-APPLY.            */

{custom/out2file.i}

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
     IF NOT RESULT THEN v-postable = NO.
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
/* ---------------------------------------------------- ar/ar-creg.p 10/94 gb */
/* AR Cash  - Edit Register & Post Transactions                   */
/* -------------------------------------------------------------------------- */

DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

def var g1 as dec format "->>>,>>>,>>9.99" NO-UNDO.
def var g2 as dec format "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR v-first AS LOG NO-UNDO.

{custom/statusMsg.i " 'Processing...  '"}

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   excelHeader = 'Rec#,NAME,DATE,AMOUNT,G/L DISTRIBUTION,'.
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelHeader,',','","') '"' SKIP.
END. /* if tb_excel */

IF td-show-parm THEN RUN show-param.

v-postable = NO.

form header
"Rec#      NAME                                  DATE        "
"AMOUNT                 G/L DISTRIBUTION" skip fill("_",131) format "x(131)"
with no-labels no-box no-underline frame f-top page-top width 132 STREAM-IO.

ASSIGN
time_stamp = string(time, "HH:MMam")
tmpstore   = fill("_",125).

{sys/form/r-top3w.f}
SESSION:SET-WAIT-STATE("general").

   assign
   str-tit  = coname + " - " + loname
   str-tit2 = "MISCELLANEOUS CASH RECEIPTS REPORT " 
   str-tit3 = ""
   x = (112 - length(str-tit)) / 2
   str-tit  = fill(" ",x) + str-tit
   x = (114 - length(str-tit2)) / 2
   str-tit2 = fill(" ",x) + str-tit2
   x = (132 - length(str-tit3)) / 2
   str-tit3 = fill(" ",x) + str-tit3
   z = 0.

   display "" with frame r-top.
   display "" with frame f-top.

   EMPTY TEMP-TABLE tt-post.

   FOR EACH ar-mcash NO-LOCK
       WHERE ar-mcash.company    EQ cocode
         AND ar-mcash.posted     EQ YES /* NO */
         AND ar-mcash.check-date GE begin_date
         AND ar-mcash.check-date LE end_date
       BREAK BY ar-mcash.bank-code
       WITH FRAME a1:

       {custom/statusMsg.i " 'Processing Account#  '  + ar-mcash.actnum "}

      IF FIRST-OF(ar-mcash.bank-code) THEN DO:
         ASSIGN g1 = 0.
         FIND FIRST bank WHERE bank.company = cocode AND
                           bank.bank-code = ar-mcash.bank-code
                           NO-LOCK NO-ERROR.
         IF AVAIL bank THEN
         DO:
            PUT bank.bank-name bank.actnum SKIP.
            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                   '"' bank.bank-name + " " + bank.actnum '",' SKIP.
         END.

         ELSE DO:
            MESSAGE "No Bank Record Available." VIEW-AS ALERT-BOX ERROR.       
            IF tb_excel THEN
               OUTPUT STREAM excel CLOSE.
            op-error = YES.
            RETURN.
         END.

         z = z + 1.
      END.

      CREATE tt-post.
      ASSIGN
       tt-post.row-id   = ROWID(ar-mcash)
       tt-post.curr-amt = ar-mcash.check-amt.

      RELEASE currency.
      IF lv-comp-curr NE "" AND lv-comp-curr NE ar-mcash.curr-code[1] THEN
      FIND FIRST currency NO-LOCK
          WHERE currency.company     EQ ar-mcash.company
            AND currency.c-code      EQ ar-mcash.curr-code[1]
            AND currency.ar-ast-acct NE ""
            AND currency.ex-rate     GT 0
          NO-ERROR.

      IF AVAIL currency THEN
        ASSIGN
         tt-post.actnum   = currency.ar-ast-acct
         tt-post.ex-rate  = currency.ex-rate
         tt-post.curr-amt = tt-post.curr-amt * tt-post.ex-rate.

      ASSIGN g1 = g1 + tt-post.curr-amt.

      PUT ar-mcash.m-no
          ar-mcash.payer
          ar-mcash.check-date AT 50
          tt-post.curr-amt    AT 62
          ar-mcash.actnum     AT 85  SPACE(1)
          tt-post.curr-amt    TO 125 SKIP.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' STRING(ar-mcash.m-no) + " " + ar-mcash.payer '",'
             '"' "" '",'
             '"' STRING(ar-mcash.check-date,"99/99/99") '",'
             '"' STRING(tt-post.curr-amt,"->>,>>>,>>9.99") '",'
             '"' ar-mcash.actnum '",'
             '"' STRING(tt-post.curr-amt,"->>,>>>,>>9.99") '",'
             SKIP.

      IF LAST-OF(ar-mcash.bank-code) THEN DO:
         PUT "**  TOTAL  "  at 85  g1 to 125 SKIP.

         IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "TOTAL" '",'
                '"' STRING(g1,"->>>,>>>,>>9.99") '",'
                SKIP(2).

         ASSIGN
            g2 = g2 + g1
            g1 = 0.
      END.

      v-postable = YES.
   END. /* each invoice */

   if z > 1 then
   DO:
       display  "** GRAND TOTAL  "  at 85  g2 to 125
         with no-labels no-underline width 132 frame gt.

       IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' "" '",'
              '"' "" '",'
              '"' "" '",'
              '"' "" '",'
              '"' "GRAND TOTAL" '",'
              '"' STRING(g2,"->>>,>>>,>>9.99") '",'
              SKIP(2).
   END.

   hide frame f-top.
/*  Summary by account not included per specs

   ASSIGN
      str-tit3 = "Summary by Account"
      x = (132 - length(str-tit3)) / 2
      str-tit3 = fill(" ",x) + str-tit3 .
   page.
   form header
   "ACCCOUNT                                  DATE     Rec.# PAID BY"
   "                               AMOUNT"
   skip
   fill("_",132) format "x(130)"
   with no-labels no-box no-underline frame f-top2 page-top width 132.

   display "" with frame f-top2.

   IF tb_excel THEN DO:
      excelHeader = 'ACCOUNT,DATE,Rec.#,PAID BY,AMOUNT,'.
      PUT STREAM excel UNFORMATTED '"' REPLACE(excelHeader,',','","') '"' SKIP.
   END. /* if tb_excel */

   FOR EACH tt-post,
       FIRST ar-mcash NO-LOCK WHERE ROWID(ar-mcash) EQ tt-post.row-id
       BREAK BY ar-mcash.actnum
             BY ar-mcash.m-no
       WITH WIDTH 132 NO-LABELS:

      IF FIRST-OF(ar-mcash.actnum) THEN DO:
         FIND FIRST account WHERE account.company = cocode AND
                                  account.actnum  = ar-mcash.actnum
                                  NO-LOCK NO-ERROR.

         IF AVAIL account THEN
         DO:
            PUT SKIP ar-mcash.actnum + " - " + account.dscr FORMAT "x(39)".
            IF tb_excel THEN
            DO:
               v-first = YES.
               PUT STREAM excel UNFORMATTED
                   '"' ar-mcash.actnum + " - " + account.dscr '",'.
            END.
         END.
         ELSE
         DO:
            MESSAGE "No Account Record Available for Account #: " +
                    ar-mcash.actnum + " for Receipt #: " + STRING(ar-mcash.m-no)
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            IF tb_excel THEN
               OUTPUT STREAM excel CLOSE.

            op-error = YES.
            RETURN.
         END.
      END.

      PUT ar-mcash.check-date AT 41     SPACE(1)
          ar-mcash.m-no                 SPACE(1)
          ar-mcash.payer                SPACE(1)
          ar-mcash.check-amt           .

      IF tb_excel THEN
      DO:
         IF v-first = NO THEN
            PUT STREAM excel UNFORMATTED
                '"' "" '",'
                '"' ar-mcash.check-date '",'
                '"' ar-mcash.m-no '",'
                '"' ar-mcash.payer '",'
                '"' STRING(ar-mcash.check-amt,"->>,>>>,>>9.99") '",'
                SKIP.
         ELSE
         DO:
            PUT STREAM excel UNFORMATTED
                '"' ar-mcash.check-date '",'
                '"' ar-mcash.m-no '",'
                '"' ar-mcash.payer '",'
                '"' STRING(ar-mcash.check-amt,"->>,>>>,>>9.99") '",'
                SKIP.
            v-first = NO.
         END.
      END.

      ACCUMULATE ar-mcash.check-amt (TOTAL BY ar-mcash.actnum).
      ACCUMULATE ar-mcash.check-amt (TOTAL).

      IF LAST-OF(ar-mcash.actnum) THEN
      DO:
         PUT SKIP "** TOTAL "  TO 100
             (ACCUM TOTAL BY ar-mcash.actnum ar-mcash.check-amt)
             FORMAT "->>>,>>>,>>9.99" TO 125 SKIP(1).

         IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "** TOTAL" '",'
                '"' STRING((ACCUM TOTAL BY ar-mcash.actnum ar-mcash.check-amt),"->>>,>>>,>>9.99") '",'
                SKIP(2).
      END.
   END.

   FOR EACH tt-post WHERE tt-post.actnum NE "",
       FIRST ar-mcash NO-LOCK WHERE ROWID(ar-mcash) EQ tt-post.row-id
       BREAK BY tt-post.actnum
             BY ar-mcash.m-no
       WITH WIDTH 132 NO-LABELS:

      IF FIRST-OF(tt-post.actnum) THEN DO:
         FIND FIRST account WHERE account.company = cocode AND
                                  account.actnum  = tt-post.actnum
                                  NO-LOCK NO-ERROR.
         IF AVAIL account THEN
         DO:
            PUT SKIP tt-post.actnum + " - " + account.dscr FORMAT "x(39)".
            IF tb_excel THEN
            DO:
               v-first = YES.
               PUT STREAM excel UNFORMATTED
                   '"' tt-post.actnum + " - " + account.dscr '",'.
            END.
         END.
         ELSE
         DO:
            MESSAGE "No Account Record Available for Account #: " +
                    ar-mcash.actnum + " for Receipt #: " + STRING(ar-mcash.m-no)
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            IF tb_excel THEN
               OUTPUT STREAM excel CLOSE.
            op-error = YES.
            RETURN.
         END.
      END.

      PUT  ar-mcash.check-date AT 41     SPACE(1)
           ar-mcash.m-no                 SPACE(1)
           ar-mcash.payer                SPACE(1)
           tt-post.curr-amt - ar-mcash.check-amt
                                         FORMAT "->>,>>>,>>9.99".

      IF tb_excel THEN
      DO:
         IF v-first = NO THEN
            PUT STREAM excel UNFORMATTED
                '"' "" '",'
                '"' ar-mcash.check-date '",'
                '"' ar-mcash.m-no '",'
                '"' ar-mcash.payer '",'
                '"' STRING(tt-post.curr-amt - ar-mcash.check-amt,"->>,>>>,>>9.99") '",'
                SKIP.
         ELSE
         DO:
            PUT STREAM excel UNFORMATTED
                '"' ar-mcash.check-date '",'
                '"' ar-mcash.m-no '",'
                '"' ar-mcash.payer '",'
                '"' STRING(tt-post.curr-amt - ar-mcash.check-amt,"->>,>>>,>>9.99") '",'
                SKIP.
            v-first = NO.
         END.
      END.

      ACCUMULATE tt-post.curr-amt - ar-mcash.check-amt (TOTAL BY tt-post.actnum).
      ACCUMULATE tt-post.curr-amt - ar-mcash.check-amt (TOTAL).

      IF LAST-OF(tt-post.actnum) THEN
      DO:
         PUT SKIP "** TOTAL "  TO 100
             (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-mcash.check-amt)
             FORMAT "->>>,>>>,>>9.99" TO 125 SKIP(1).

         IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "** TOTAL" '",'
                '"' STRING((ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-mcash.check-amt),"->>>,>>>,>>9.99") '",'
                SKIP(2).
      END.
   END.

   PUT "***** TOTAL FOR ALL ACCOUNTS " TO 100
       (ACCUM TOTAL ar-mcash.check-amt) +
         (ACCUM TOTAL tt-post.curr-amt - ar-mcash.check-amt)
       FORMAT "->>>,>>>,>>9.99" TO 125 SKIP(1).

   IF tb_excel THEN
   DO:
      PUT STREAM excel UNFORMATTED
          '"' "" '",'
          '"' "" '",' 
          '"' "" '",'
          '"' "" '",'
          '"' "***** TOTAL FOR ALL ACCOUNTS" '",'
          '"' STRING((ACCUM TOTAL ar-mcash.check-amt) +
                     (ACCUM TOTAL tt-post.curr-amt - ar-mcash.check-amt),"->>>,>>>,>>9.99") '",'
          SKIP.

      OUTPUT STREAM excel CLOSE.

      IF tb_runExcel THEN
         OS-COMMAND NO-WAIT start excel.exe VALUE(SEARCH(fi_file)).
   END.
*/
  IF tb_excel THEN DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
  END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").

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

