&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ar\r-apme&p.w

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
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.

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

def buffer b-line for ar-cashl.
/*
if v-return then return.
*/
DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.

def var g1 as dec format "->>,>>>,>>9.99" NO-UNDO.
def var g2 like g1 NO-UNDO.
def var t3 like g1 NO-UNDO.
def var v1 like g1 NO-UNDO.
def var v2 like g1 NO-UNDO.
def var t2 like g1 NO-UNDO.
def var invo-sub like ar-inv.net NO-UNDO.
def var disc-sub like ar-inv.disc-taken NO-UNDO.
def var xtrnum    as int NO-UNDO.
def var xar-acct  as char NO-UNDO.
def var xdis-acct as char NO-UNDO.
def var xcust like cust.cust-no NO-UNDO.
def var xchk  like ar-cashl.check-no NO-UNDO.
def var tmp-amt-paid like ar-cashl.amt-paid NO-UNDO.
def var alf-check as char format "x(10)" no-undo.
def var v-non-zero as log no-undo.
def var fcust like ar-cashl.cust-no init "" NO-UNDO.
def var tcust like fcust            init "zzzzzzzz" NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
def var time_stamp as ch NO-UNDO.
def var dsc     as decimal format "->>,>>>,>>9.99" NO-UNDO.
def var net-cr  as decimal format "->>,>>>,>>9.99" NO-UNDO.
def var ck-onac as decimal format "->>,>>>,>>9.99" NO-UNDO.

DO TRANSACTION:
  {sys/inc/postdate.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_cust end_cust ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_cust end_cust ~
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

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

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

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Post Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

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
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 6.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.81.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 3.14 COL 39 COLON-ALIGNED
     tran-period AT ROW 4.33 COL 39 COLON-ALIGNED
     begin_cust AT ROW 6.48 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 6.48 COL 68 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     rd-dest AT ROW 11.48 COL 9 NO-LABEL
     lv-ornt AT ROW 11.71 COL 30 NO-LABEL
     lines-per-page AT ROW 11.71 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 13.14 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 14.1 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.29 COL 9
     btn-ok AT ROW 18.38 COL 23
     btn-cancel AT ROW 18.38 COL 58
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.52 COL 4
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
         TITLE              = "On Account Application Register"
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
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
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
ON END-ERROR OF C-Win /* On Account Application Register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* On Account Application Register */
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

  run check-date.
  if v-invalid then return no-apply. 

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
   /* gdm - 11050906 */
   REPEAT:
     FIND FIRST gl-ctrl EXCLUSIVE-LOCK
       WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
     IF AVAIL gl-ctrl THEN DO:
       ASSIGN xtrnum        = gl-ctrl.trnum + 1
              gl-ctrl.trnum = xtrnum.
       RELEASE gl-ctrl.
       LEAVE.
     END. /* IF AVAIL gl-ctrl */
   END. /* REPEAT */
   /* gdm - 11050906 */
  END.

  run run-report.

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.

  IF v-postable THEN DO:

    lv-post = NO.

    MESSAGE "Post Invoices?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

    IF lv-post THEN do:      
      RUN post-gl.
      RUN copy-report-to-audit-dir.
      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
    END.
    ELSE RUN undo-trnum.  
  END.

  ELSE do:
      MESSAGE "No Cash Receipts available for posting..." VIEW-AS ALERT-BOX ERROR.
      RUN undo-trnum.
  END.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Post Date */
DO:
  assign {&self-name}.

  if lastkey ne -1 then do:
    run check-date.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
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
    {custom/usrprint.i}

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
  END.

  {methods/nowait.i}
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE alf-check C-Win 
PROCEDURE alf-check :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  assign
   alf-check  = fill(" ",10 - length(trim(alf-check))) + trim(alf-check)
   v-non-zero = no.

  do i = 1 to length(alf-check):
    if substr(alf-check,i,1) ne "0" and
       substr(alf-check,i,1) ne " " then v-non-zero = yes.
    else
    if not v-non-zero then substr(alf-check,i,1) = " ".
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO with frame {&frame-name}:
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
  END.
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
  DISPLAY tran-date tran-period begin_cust end_cust rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_cust end_cust rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
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
  find first ar-ctrl where ar-ctrl.company eq cocode NO-LOCK.
  assign
   xar-acct   = ar-ctrl.receivables
   xdis-acct  = ar-ctrl.discount.
  release ar-ctrl.
  find first account
      where account.company eq cocode
        and account.actnum  eq xar-acct
      no-lock no-error.
  if not avail account or account.actnum = "" then do:
     message " Receivables Account is blank or is not on file for this Company."
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  end.
  find first account
      where account.company eq cocode
        and account.actnum  eq xdis-acct
      no-lock no-error.
  if not avail account or account.actnum = "" then do:    
    message " Discount Account is blank or is not on file for this Company." VIEW-AS ALERT-BOX ERROR.    
    RETURN ERROR.
  end.
end.

find first sys-ctrl where
    sys-ctrl.company eq cocode AND
    sys-ctrl.name    eq "AUDITDIR"
    no-lock no-error.

  if not avail sys-ctrl then DO TRANSACTION:
     create sys-ctrl.
     assign
        sys-ctrl.company = cocode
        sys-ctrl.name    = "AUDITDIR"
        sys-ctrl.descrip = "Audit Trails directory"
        sys-ctrl.char-fld = ".\AUDIT TRAILS".
  end.

  lv-audit-dir = sys-ctrl.char-fld.

  IF LOOKUP(SUBSTR(lv-audit-dir,LENGTH(lv-audit-dir),1),"/,\") > 0 THEN
     lv-audit-dir = SUBSTR(lv-audit-dir,1,LENGTH(lv-audit-dir) - 1).

  RELEASE sys-ctrl.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-gl C-Win 
PROCEDURE post-gl :
DO TRANSACTION:
      for each ar-cashl
          where ar-cashl.company    eq cocode
            and ar-cashl.cust-no    ge fcust
            and ar-cashl.cust-no    le tcust
            and ar-cashl.on-account eq yes
            and ar-cashl.inv-no     ne 0,

          first ar-cash
          where ar-cash.c-no eq ar-cashl.c-no
          no-lock

          break by ar-cashl.cust-no
                by ar-cashl.check-no
                by ar-cashl.inv-no:

        {ar/ar-oreg.i ar-cashl 1}

        ar-cashl.posted = YES.
      end.
 end. /* postit: transaction */

 FIND CURRENT ar-cashl NO-LOCK NO-ERROR.
 FIND CURRENT ar-inv NO-LOCK NO-ERROR.
 FIND CURRENT cust NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- ar/ar-creg.p 10/94 gb */
/* AR Cash  - Edit Register & Post Transactions                   */
/* -------------------------------------------------------------------------- */

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

v-postable = NO.

form header
    "CUST.#   NAME                                    CHECK#      DATE         "
    "ON ACCOUNT       INVOICE #          APPLIED     DISCOUNT"
    skip fill("_",132) format "x(132)"

  with no-labels no-box no-underline frame f-top page-top width 132 STREAM-IO.

assign
 time_stamp = string(time, "HH:MMam")
 tmpstore   = fill("_",132).

{sys/form/r-top3w.f}

ASSIGN fcust = begin_cust
       tcust = END_cust.
SESSION:SET-WAIT-STATE("general").

    assign
     str-tit  = coname + " - " + loname
     str-tit2 = "CASH ON ACCOUNT APPLICATION  -  EDIT REGISTER " +
                string(xtrnum)
     str-tit3 = "Period " + string(tran-period,"99") + " - " + string(tran-date)
     x = (112 - length(str-tit)) / 2
     str-tit  = fill(" ",x) + str-tit
     x = (114 - length(str-tit2)) / 2
     str-tit2 = fill(" ",x) + str-tit2
     x = (132 - length(str-tit3)) / 2
     str-tit3 = fill(" ",x) + str-tit3.


    display "" with frame r-top.
    display "" with frame f-top.

    for each ar-cashl
        where ar-cashl.company    eq cocode
          and ar-cashl.cust-no    ge fcust
          and ar-cashl.cust-no    le tcust
          and ar-cashl.on-account eq yes
          and ar-cashl.inv-no     ne 0
          and (ar-cashl.amt-paid  ne 0 or
               ar-cashl.amt-disc  ne 0)
        no-lock,

        first ar-cash
        where ar-cash.c-no eq ar-cashl.c-no
        no-lock,

        first cust
        where cust.company eq ar-cash.company
          and cust.cust-no eq ar-cash.cust-no
        no-lock

        break by ar-cashl.cust-no by ar-cashl.check-no

        with frame a1:

      if first-of(ar-cashl.cust-no) then do:
         xcust = cust.cust-no.
         put cust.cust-no space(1)
             cust.name.
      end.

      ELSE if first-of(ar-cashl.check-no) then put skip(1).

      if first-of(ar-cashl.check-no) then do:
        put skip(1).
        ASSIGN
        xchk = ar-cashl.check-no
        ck-onac = 0.
        for each b-line FIELDS(amt-paid amt-disc)
            where b-line.company    eq cocode
              and b-line.cust-no    eq xcust
              and b-line.on-account eq yes
              and b-line.check-no   eq xchk:
            ck-onac = ck-onac + b-line.amt-paid - b-line.amt-disc.
        end.

        alf-check = string(ar-cashl.check-no,"9999999999").

        run alf-check.

        do i = 1 to length(alf-check):
          if substr(alf-check,i,1) ne "0" then v-non-zero = yes.
          else
          if not v-non-zero then substr(alf-check,i,1) = " ".
        end.

        put alf-check to 55
            ar-cash.check-date  at 60
            ck-onac at 72
            ar-cashl.inv-no at 95.

        v2 = v2 + ck-onac.
      end.

      else put ar-cashl.inv-no at 95.

      if ar-cashl.amt-paid ne 0 then put space(3) ar-cashl.amt-paid.
      else put space(17).

      if ar-cashl.amt-disc ne 0 then put space(3) ar-cashl.amt-disc skip.
      else put skip.

      assign
       invo-sub = invo-sub + ar-cashl.amt-paid
       disc-sub = disc-sub + ar-cashl.amt-disc
       dsc      = dsc      + ar-cashl.amt-disc
       net-cr   = net-cr   + ar-cashl.amt-paid - ar-cashl.amt-disc.

      if last-of(ar-cashl.cust-no) then do:
        display  "*  CUSTOMER TOTALS"  to 70
                 v2                    at 72
                 invo-sub              at 104 space(3)
                 disc-sub
                 "*"                   to 132 skip(1)

            with frame vtot no-box no-labels width 132 STREAM-IO.

        assign
         g1       = g1 + invo-sub
         g2       = g2 + v2
         invo-sub = 0
         disc-sub = 0
         v1       = 0
         v2       = 0.
      end.
      v-postable = YES.
    end. /* each cashline */

    do with frame gt width 132 no-labels NO-UNDERLINE STREAM-IO:
      display  "** BEGINNING CASH ON ACCOUNT"  to 101 g2     to 118
               "** GROSS AMOUNT APPLIED"       to 101 net-cr to 118
               "** TOTAL DISCOUNTS"            to 101 dsc    to 118
               "** NET AMOUNT APPLIED"         to 101 net-cr - dsc    to 118
               "** ENDING CASH ON ACCOUNT"     to 101 g2 - (g1 - dsc) to 118

          with no-labels no-underline width 132 frame gt.
      ASSIGN G1 = 0
             G2 = 0
             NET-CR = 0
             DSC = 0
             .
    end.

    hide frame f-top.

    assign
     str-tit3 = "Period " + string(tran-period,"99") + " " +
                string(tran-date) + " - " + "Summary by Account"
     x = (132 - length(str-tit3)) / 2
     str-tit3 = fill(" ",x) + str-tit3.

    page.

    form header
         "ACCOUNT                                   DATE   CUST #   NAME"
         "                           CHECK#     INV#           AMOUNT" skip
         fill("_",132) format "x(130)"

        with no-labels no-box no-underline frame f-top2 page-top width 132 STREAM-IO.

    display "" with frame f-top2.

    for each ar-cashl NO-LOCK
        where ar-cashl.company    eq cocode
          and ar-cashl.cust-no    ge fcust
          and ar-cashl.cust-no    le tcust
          and ar-cashl.on-account eq yes
          and ar-cashl.inv-no     ne 0,

        first ar-cash
        where ar-cash.c-no eq ar-cashl.c-no
        no-lock,

        first cust
        where cust.company eq ar-cash.company
          and cust.cust-no eq ar-cash.cust-no
        no-lock

        break by ar-cashl.actnum
              by ar-cashl.cust-no
              by ar-cashl.check-no
              by ar-cashl.inv-no

        with width 132 no-labels:

      if first-of(ar-cashl.actnum) then do:
        find first account
            where account.company eq cocode
              and account.actnum  eq ar-cashl.actnum
            no-lock no-error.
        if avail account then
           put ar-cashl.actnum + " - " + account.dscr format "x(39)" .
        else
           put ar-cashl.actnum.
      end.

      if ar-cashl.dscr = "credit" then
        tmp-amt-paid = (- ar-cashl.amt-paid).
      else
        tmp-amt-paid = ar-cashl.amt-paid.

      alf-check = string(ar-cash.check-no,"9999999999").

      run alf-check.

      put  ar-cash.check-date at 41 space(1)
           ar-cash.cust-no          space(1)
           cust.name                space(1)
           alf-check space(1)
           ar-cashl.inv-no format ">>>>>9"  space(1)
           tmp-amt-paid      format "->>,>>>,>>9.99"
           skip.

      accumulate ar-cashl.amt-paid (total by ar-cashl.actnum).
      accumulate ar-cashl.amt-paid (total).
      accumulate ar-cashl.amt-disc (total).

      if last-of(ar-cashl.actnum) then
        put skip
            accum total by ar-cashl.actnum ar-cashl.amt-paid
                format "->>,>>>,>>9.99" to 117 " *" skip.

      if last(ar-cashl.actnum) then
        put skip(1)
            "***** TOTAL FOR ALL ACCOUNTS " to 95
            accum total ar-cashl.amt-paid to 117 " **" skip
            "***** TOTAL DISCOUNTS " to 95
            accum total ar-cashl.amt-disc to 117.
    end.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undo-trnum C-Win 
PROCEDURE undo-trnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO TRANSACTION:
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        IF xtrnum EQ gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
        FIND CURRENT gl-ctrl NO-LOCK.
        RELEASE gl-ctrl.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-report-to-audit-dir C-Win 
PROCEDURE copy-report-to-audit-dir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR targetfile AS CHAR FORMAT "X(50)" NO-UNDO.
  DEF VAR dirname1 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR dirname2 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR dirname3 AS CHAR FORMAT "X(20)" NO-UNDO.

  ASSIGN targetfile = lv-audit-dir + "\AR\AC6\Run#"
                    + STRING(xtrnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AR"
         dirname3 = lv-audit-dir + "\AR\AC6".

  OS-COPY VALUE(list-name) VALUE (targetfile).

  IF SEARCH(targetfile) EQ ? THEN DO:
    OS-CREATE-DIR VALUE(dirname1).
    OS-CREATE-DIR VALUE(dirname2).
    OS-CREATE-DIR VALUE(dirname3).
    OS-COPY VALUE(list-name) VALUE (targetfile).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
