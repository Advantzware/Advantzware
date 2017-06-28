&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ar\r-cshe&p.w

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
DEF VAR lv-comp-curr AS cha NO-UNDO.


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

FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
IF AVAIL company THEN lv-comp-curr = company.curr-code.

DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.

def var v-from-date  as date format "99/99/9999" init today.
def var v-to-date    as date format "99/99/9999" init today.
def var xtrnum as INT NO-UNDO.
def var xar-acct  as CHAR NO-UNDO.
def var xdis-acct as CHAR NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR ld-curr AS DEC NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-post NO-UNDO FIELD row-id AS ROWID
                               FIELD ex-rate LIKE currency.ex-rate INIT 1
                               FIELD curr-amt LIKE ar-cash.check-amt
                               FIELD actnum LIKE account.actnum.

DO TRANSACTION:
  {sys/inc/postdate.i}
  find first sys-ctrl where
        sys-ctrl.company eq cocode AND
        sys-ctrl.name    eq "AUDITDIR"
        no-lock no-error.

   if not avail sys-ctrl THEN DO:
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
begin_date end_date rd_sort rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_cust end_cust ~
begin_date end_date lbl_sort rd_sort rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer", "Customer",
"Sequence", "Sequence"
     SIZE 31 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 8.81.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.43 COL 44 COLON-ALIGNED
     tran-period AT ROW 3.62 COL 44 COLON-ALIGNED
     begin_cust AT ROW 5.29 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 5.29 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_date AT ROW 6.24 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date"
     end_date AT ROW 6.24 COL 70 COLON-ALIGNED HELP
          "Enter Ending Invoice Date"
     lbl_sort AT ROW 8.14 COL 34 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 8.14 COL 46 NO-LABEL
     rd-dest AT ROW 11 COL 5 NO-LABEL
     lv-ornt AT ROW 11 COL 31 NO-LABEL
     lines-per-page AT ROW 11 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 13.62 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 14.57 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.48 COL 31
     btn-ok AT ROW 19.33 COL 23
     btn-cancel AT ROW 19.33 COL 58
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.29 COL 2
     RECT-6 AT ROW 10.05 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 20.81.


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
         TITLE              = "Cash Receipts Edit/Post Register"
         HEIGHT             = 21.14
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
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tran-period:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Cash Receipts Edit/Post Register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Cash Receipts Edit/Post Register */
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
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= 'Customer'
                            &begin_cust= "begin_cust"
                            &END_cust= "begin_cust" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE="Customer"
                             &begin_cust= "begin_cust"
                             &END_cust= "begin_cust" 
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE="Customer"
                                  &begin_cust= "begin_cust"
                                  &END_cust= "begin_cust" 
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  end case.

  IF v-postable THEN DO:
    lv-post = NO.

    MESSAGE "Post Cash Receipts?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE lv-post.

    IF lv-post THEN do:      
      RUN post-gl.
      RUN copy-report-to-audit-dir.
      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
    END.
    ELSE RUN undo-trnum.  
  END.

  ELSE DO:
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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Receipt Date */
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


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
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

  {methods/nowait.i}
  DO WITH FRAME {&frame-name}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
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

  ASSIGN targetfile = lv-audit-dir + "\AR\AC2\Run#"
                    + STRING(xtrnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AR"
         dirname3 = lv-audit-dir + "\AR\AC2".

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
  DISPLAY tran-date tran-period begin_cust end_cust begin_date end_date lbl_sort 
          rd_sort rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_cust end_cust begin_date end_date 
         rd_sort rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok 
         btn-cancel 
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

  find first ar-ctrl where ar-ctrl.company eq cocode NO-LOCK.
  xar-acct   = ar-ctrl.receivables.
  xdis-acct  = ar-ctrl.discount.

  find first account where account.company eq cocode
                       and account.actnum  eq xar-acct no-lock no-error.
  if not avail account or account.actnum eq "" then do:
    message " Receivables Account is blank or is not on file for this Company." VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  end.
  find first account where account.company eq cocode
                       and account.actnum  eq xdis-acct
      no-lock no-error.
  if not avail account or account.actnum eq "" then do:

    message " Discount Account is blank or is not on file for this Company." VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
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

     IF NOT OKpressed THEN  RETURN NO-APPLY.  */

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
     IF NOT RESULT THEN v-postable = NO.
*/
RUN custom/prntproc.p (list-name,INT(lv-font-no), lv-ornt).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-gl C-Win 
PROCEDURE post-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR t1 AS DEC NO-UNDO.
  DEF VAR xar-cashl AS CHAR NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR li-iter AS INT NO-UNDO.

  DEF BUFFER b-cashl FOR ar-cashl.
  DEF BUFFER ar-c-memo FOR reftable.

 DO WHILE CAN-FIND(FIRST tt-post) AND li-iter LE 100000:
  li-iter = li-iter + 1.

  RELEASE tt-post.

  FOR EACH tt-post,
      FIRST ar-cash WHERE ROWID(ar-cash) EQ tt-post.row-id EXCLUSIVE
      BREAK BY tt-post.actnum
      TRANSACTION:

    FIND FIRST bank
        WHERE bank.company   EQ cocode
          AND bank.bank-code EQ ar-cash.bank-code
        EXCLUSIVE NO-WAIT NO-ERROR.
    IF NOT AVAIL bank THEN NEXT.

    FIND FIRST cust
        {sys/ref/custW.i}
          AND cust.cust-no EQ ar-cash.cust-no
        EXCLUSIVE NO-WAIT NO-ERROR.
    IF NOT AVAIL cust THEN NEXT.

    ASSIGN
     xar-cashl = bank.actnum
     bank.bal  = bank.bal + tt-post.curr-amt
     t1        = 0.

    FOR EACH ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no:
      ar-cashl.posted = YES.

      RELEASE b-cashl.

      IF ar-cashl.inv-no NE 0 AND ar-cashl.on-account EQ NO THEN DO:
        FIND FIRST ar-inv
            WHERE ar-inv.company EQ cocode
              AND ar-inv.inv-no  EQ ar-cashl.inv-no
            NO-ERROR.
        IF AVAIL ar-inv THEN DO:
          ASSIGN
           ar-inv.disc-taken = ar-inv.disc-taken + ar-cashl.amt-disc
           ar-inv.paid       = ar-inv.paid + ar-cashl.amt-paid
           ar-inv.due        = ar-inv.due - ar-cashl.amt-paid
           ar-inv.pay-date   = ar-cash.check-date.

          IF ar-inv.due LE 0 THEN DO:
            RUN sys/inc/daytopay.p (RECID(ar-cashl)).
            cust.num-inv = cust.num-inv + 1.
          END.

          FIND CURRENT ar-inv NO-LOCK NO-ERROR.
        END.

        FIND FIRST ar-c-memo NO-LOCK
            WHERE ar-c-memo.reftable EQ "ar-cashl.ar-cashl"
              AND ar-c-memo.company  EQ ar-cash.company
              AND ar-c-memo.loc      EQ ""
              AND ar-c-memo.code     EQ ar-cashl.rec_key
            NO-ERROR.

        IF AVAIL ar-c-memo THEN
        FIND FIRST b-cashl
            WHERE b-cashl.c-no EQ INT(ar-c-memo.val[1])
              AND b-cashl.line EQ INT(ar-c-memo.val[2])
            NO-ERROR.
      END.

      IF AVAIL b-cashl THEN DO:
        ASSIGN
         b-cashl.inv-no     = ar-cashl.inv-no
         b-cashl.inv-date   = ar-cashl.inv-date
         b-cashl.amt-due    = ar-cashl.amt-due
         b-cashl.on-account = NO.
        DELETE ar-cashl.
      END.

      ELSE DO:
        IF ar-cashl.inv-no EQ 0 AND ar-cashl.on-account EQ YES THEN
          cust.on-account = cust.on-account + ar-cashl.amt-paid.

        CREATE gltrans.
        ASSIGN
         t1 = t1 + ar-cashl.amt-paid

         gltrans.company   = cocode
         gltrans.actnum    = ar-cashl.actnum
         gltrans.jrnl      = "CASHR"
         gltrans.tr-dscr   = cust.cust-no + " " +
                             STRING(ar-cash.check-no,"9999999999") +
                             " Inv# " + STRING(ar-cashl.inv-no)
         gltrans.tr-date   = tran-date
         gltrans.tr-amt    = ar-cashl.amt-paid - ar-cashl.amt-disc
         gltrans.period    = tran-period
         gltrans.trnum     = xtrnum
         ar-cashl.amt-paid = ar-cashl.amt-paid - ar-cashl.amt-disc.

        RELEASE gltrans.

        IF ar-cashl.amt-disc NE 0 THEN DO:
          CREATE gltrans.
          ASSIGN
           gltrans.company = cocode
           gltrans.actnum  = xdis-acct
           gltrans.jrnl    = "CRDIS"
           gltrans.tr-dscr = cust.cust-no + " " +
                             STRING(ar-cash.check-no,"9999999999") +
                             " Inv# " + STRING(ar-cashl.inv-no)
           gltrans.tr-date = tran-date
           gltrans.tr-amt  = ar-cashl.amt-disc
           gltrans.period  = tran-period
           gltrans.trnum   = xtrnum.

          RELEASE gltrans.

          CREATE ar-ledger.
          ASSIGN
           ar-ledger.company  = cocode
           ar-ledger.cust-no  = ar-cash.cust-no
           ar-ledger.amt      = ar-cashl.amt-disc
           ar-ledger.ref-num  = "DISC " +
                                STRING(ar-cash.check-no,"9999999999") +
                                "-" + STRING(ar-cashl.line,"9999999999")
           ar-ledger.ref-date = ar-cash.check-date
           ar-ledger.tr-date  = tran-date
           ar-ledger.tr-num   = xtrnum.
          RELEASE ar-ledger.
        END.
      END.
    END.  /* each line */

    ASSIGN
     cust.acc-bal   = cust.acc-bal - t1
     cust.lpay      = t1
     cust.lpay-date = ar-cash.check-date.

    IF cust.acc-bal GE cust.hibal THEN
      ASSIGN
       cust.hibal      = cust.acc-bal
       cust.hibal-date = ar-cash.check-date.

    IF t1 NE 0 THEN DO:
      FIND gltrans WHERE ROWID(gltrans) EQ lv-rowid NO-ERROR.
      IF NOT AVAIL gltrans THEN DO:
        CREATE gltrans.
        ASSIGN
         gltrans.company = cocode
         gltrans.actnum  = xar-acct
         gltrans.jrnl    = "CASHR"
         gltrans.tr-dscr = "CASH RECEIPTS"
         gltrans.tr-date = tran-date
         gltrans.period  = tran-period
         gltrans.trnum   = xtrnum
         lv-rowid        = ROWID(gltrans).
      END.
      gltrans.tr-amt = gltrans.tr-amt - t1.
      RELEASE gltrans.
    END.

    CREATE ar-ledger.
    ASSIGN
     ar-ledger.company  = cocode
     ar-ledger.cust-no  = ar-cash.cust-no
     ar-ledger.amt      = ar-cash.check-amt
     ar-ledger.ref-num  = "CHK# " + STRING(ar-cash.check-no,"9999999999")
     ar-ledger.ref-date = ar-cash.check-date
     ar-ledger.tr-date  = tran-date
     ar-ledger.tr-num   = xtrnum
     ar-cash.posted     = YES.
    RELEASE ar-ledger.

    RELEASE cust.
    RELEASE bank.

    ACCUM tt-post.curr-amt - ar-cash.check-amt (TOTAL BY tt-post.actnum).

    IF LAST-OF(tt-post.actnum) AND tt-post.actnum NE "" THEN DO:
      CREATE gltrans.
      ASSIGN
       gltrans.company = cocode
       gltrans.actnum  = tt-post.actnum
       gltrans.jrnl    = "CASHR"
       gltrans.tr-dscr = "CASH RECEIPTS CURRENCY GAIN/LOSS"
       gltrans.tr-date = tran-date
       gltrans.period  = tran-period
       gltrans.trnum   = xtrnum
       gltrans.tr-amt  = (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-cash.check-amt).

      RELEASE gltrans.

      CREATE gltrans.
      ASSIGN
       gltrans.company = cocode
       gltrans.actnum  = tt-post.actnum
       gltrans.jrnl    = "CASHR"
       gltrans.tr-dscr = "CASH RECEIPTS CURRENCY GAIN/LOSS"
       gltrans.tr-date = tran-date
       gltrans.period  = tran-period
       gltrans.trnum   = xtrnum
       gltrans.tr-amt  = - (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-cash.check-amt).
      RELEASE gltrans.
    END.

    DELETE tt-post.
  END.
 END.  /* DO WHILE */

 RELEASE ar-cash.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- ar/ar-creg.p 10/94 gb */
/* AR Cash  - Edit Register & Post Transactions                   */
/* -------------------------------------------------------------------------- */

{sys/FORM/r-top3w.f}

def var dsc     as dec format "->>,>>>,>>9.99".
def var net-cr  as dec format "->>,>>>,>>9.99".
def var save_id as recid.
def var time_stamp as ch.
def var qfirst as l.
def var post as log format "Yes/No"
                            label "   Post to G/L & Customer files?   " init no.
def var g1 as dec format "->>,>>>,>>9.99".
def var t1 like g1.
def var g2 like g1.
def var t3 like g1.
def var v1 like g1.
def var v2 like g1.
def var t2 like g1.


def var v-amt-due-sub as dec format "->,>>>,>>9.99" no-undo.
def var v-amt-due-tot as dec format "->,>>>,>>9.99" no-undo.
def var v-amt-paid-sub as dec format "->,>>>,>>9.99" no-undo.
def var v-amt-paid-tot as dec format "->,>>>,>>9.99" no-undo.
def var v-disc-sub as dec format "->>>,>>9.99" no-undo.
def var v-disc-tot as dec format "->>>,>>9.99" no-undo.
def var v-on-act-amt as dec format "->>>>>>9.99" no-undo.
def var v-on-act-sub as dec format "->>>>>>9.99" no-undo.
def var v-on-act-tot as dec format "->>>>>>9.99" no-undo.
def var archk as dec format ">>>>>>>>>>".
def var sort-by-cust as log init yes format "Customer/Sequence" no-UNDO.
/*
DEF VAR tmp-dir AS cha NO-UNDO. 
DEF VAR str-tit AS cha FORM "x(50)" NO-UNDO.
DEF VAR str-tit2 AS cha FORM "x(50)" NO-UNDO.
DEF VAR str-tit3 AS cha FORM "x(50)" NO-UNDO. 
DEF VAR coname AS cha NO-UNDO. 
DEF VAR loname AS cha NO-UNDO.   */

SESSION:SET-WAIT-STATE("general").

assign sort-by-cust = rd_sort = "customer"
       v-from-date  = begin_date
       v-to-date    = END_date
       .

form header
"CUSTOMER      NAME                   CHECK       DATE     CASH       INVOICE         AMOUNT       AMOUNT     DISCOUNT     ON ACCOUNT"
"   #                                 NUMBER               RECEIVED   NUMBER          DUE          APPLIED                   PAYMENTS"
skip fill("_",132) format "x(132)"
    with no-labels no-box no-underline frame f-top page-top WIDTH 200 STREAM-IO.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

/* str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 132}. */
  assign
   str-tit  = coname + " - " + loname
   str-tit2 = "CASH RECEIPTS  -  EDIT REGISTER " + string(xtrnum)
   str-tit3 = "Period " + string(tran-period,"99") + " - " + string(tran-date) 
   x = (112 - length(str-tit)) / 2
   str-tit  = fill(" ",x) + str-tit
   x = (114 - length(str-tit2)) / 2
   str-tit2 = fill(" ",x) + str-tit2
   x = (132 - length(str-tit3)) / 2
   str-tit3 = fill(" ",x) + str-tit3.

  assign v-amt-due-tot = 0
         v-amt-paid-tot = 0
         v-disc-tot = 0
         v-on-act-tot = 0.

  DISPLAY "" with frame r-top.
  DISPLAY "" with frame f-top.

  EMPTY TEMP-TABLE tt-post.

  if sort-by-cust then do:
    {ar/ar-creg.i cust-no 1}
  end.
  else do:
    {ar/ar-creg.i c-no 2}
  end.

  ASSIGN
   str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date) + " - " +
              "Summary by Account".
   x = (132 - length(str-tit3)) / 2.
   str-tit3 = fill(" ",x) + str-tit3.

  page.

  format header
    "ACCOUNT                                  DATE   CUSTOMER   MEMO #    "
    "LINE DESCRIPTION                QTY   UNIT PRICE           AMOUNT" skip
    fill("_",132) format "x(132)"
    with no-labels no-box no-underline frame f-top2 page-top WIDTH 200 STREAM-IO.

  display "" with frame f-top2.

  g2 = 0.

  FOR EACH tt-post,
      FIRST ar-cash WHERE ROWID(ar-cash) EQ tt-post.row-id NO-LOCK,
      EACH ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no NO-LOCK
      BREAK BY ar-cashl.actnum BY ar-cashl.c-no:

    IF FIRST-OF(ar-cashl.actnum) THEN DO:
      FIND FIRST account NO-LOCK
          WHERE account.company EQ cocode
            AND account.actnum  EQ ar-cashl.actnum
          NO-ERROR.
      IF AVAIL account THEN
        PUT ar-cashl.actnum + " - " + account.dscr FORMAT "x(39)".
      ELSE
        PUT ar-cashl.actnum.
    END.
    v-postable = YES.

    archk = ar-cash.check-no.

    PUT ar-cash.check-date AT 41        SPACE(1)
        ar-cash.cust-no                 SPACE(1)
        archk                           SPACE(1)
        ar-cashl.line   FORMAT ">>>>"   SPACE(1)
        ar-cashl.dscr   FORMAT "x(20)"  SPACE(1)
        ar-cashl.amt-paid - ar-cashl.amt-disc FORMAT "->,>>>,>>9.99" TO 132
        SKIP.

    ACCUM ar-cashl.amt-paid - ar-cashl.amt-disc (TOTAL BY ar-cashl.actnum).

    IF LAST-OF(ar-cashl.actnum) THEN DO:
      PUT "** TOTAL "                           TO 116
          (ACCUM TOTAL BY ar-cashl.actnum ar-cashl.amt-paid - ar-cashl.amt-disc)
                       FORMAT "->,>>>,>>9.99"   TO 132
          SKIP(1).

      g2 = g2 +
           (ACCUM TOTAL BY ar-cashl.actnum ar-cashl.amt-paid - ar-cashl.amt-disc).
    END.
  END.

  FOR EACH tt-post WHERE tt-post.actnum NE "",
      FIRST ar-cash WHERE ROWID(ar-cash) EQ tt-post.row-id NO-LOCK
      BREAK BY tt-post.actnum BY ar-cash.c-no:

    IF FIRST-OF(tt-post.actnum) THEN DO:
      FIND FIRST account NO-LOCK
          WHERE account.company EQ cocode
            AND account.actnum  EQ tt-post.actnum
          NO-ERROR.
      IF AVAIL account THEN
        PUT tt-post.actnum + " - " + account.dscr FORMAT "x(39)".
      ELSE
        PUT tt-post.actnum.
    END.

    archk = ar-cash.check-no.

    PUT ar-cash.check-date AT 41        SPACE(1)
        ar-cash.cust-no                 SPACE(1)
        archk                           SPACE(1)
        0               FORMAT ">>>>"   SPACE(1)
        ""              FORMAT "x(20)"  SPACE(1)
        tt-post.curr-amt - ar-cash.check-amt FORMAT "->,>>>,>>9.99" TO 132
        SKIP.

    ACCUM tt-post.curr-amt - ar-cash.check-amt (TOTAL BY tt-post.actnum).

    IF LAST-OF(tt-post.actnum) THEN DO:
      PUT "** TOTAL "                           TO 116
          (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-cash.check-amt)
                        FORMAT "->,>>>,>>9.99"  TO 132
          SKIP(1).

      g2 = g2 +
           (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-cash.check-amt).
    END.
  END.

  IF v-postable THEN
    DISPLAY "***** TOTAL FOR ALL ACCOUNTS "   TO 116
            g2 FORMAT "->,>>>,>>9.99"         TO 132
        WITH NO-LABELS NO-BOX STREAM-IO WIDTH 200.

SESSION:SET-WAIT-STATE("").

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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

