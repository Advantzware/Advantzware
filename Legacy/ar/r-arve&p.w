&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ar\r-arve&p.w

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
DEF INPUT PARAM ip-post AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR lv-comp-curr AS cha NO-UNDO.
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

FIND FIRST company WHERE company.company = gcompany NO-LOCK NO-ERROR.
IF AVAIL company THEN lv-comp-curr = company.curr-code.

def new shared var v-trnum as INT NO-UNDO.

DEF NEW SHARED TEMP-TABLE wkdistrib NO-UNDO
  FIELD ACTNUM  LIKE account.actnum COLUMN-LABEL "Account"
  FIELD tr-dscr LIKE gltrans.tr-dscr
  FIELD AMOUNT  AS DECIMAL FORMAT "->>>,>>>,>>>.99"
  FIELD debit   AS LOG 
  FIELD recs    AS INTEGER COLUMN-LABEL "Records" FORMAT ">>,>>>"
  FIELD tons    AS DEC.

DEF VAR ws_debit  LIKE wkdistrib.amount COLUMN-LABEL "Debit" NO-UNDO.
DEF VAR ws_credit LIKE wkdistrib.amount COLUMN-LABEL "Credit" NO-UNDO.
DEF VAR ws_net    LIKE wkdistrib.amount COLUMN-LABEL "Net"   NO-UNDO.

DEF VAR posting AS LOGICAL NO-UNDO INITIAL FALSE.
DEF VAR v-invalid AS LOG NO-UNDO.
def var v-postable as log init NO NO-UNDO.
DEF VAR xar-acct LIKE account.actnum NO-UNDO.
DEF VAR xar-stax LIKE account.actnum NO-UNDO.
DEF VAR xar-freight LIKE account.actnum NO-UNDO.
DEF VAR xar-cur-acct LIKE account.actnum NO-UNDO.  /* currency ar account */
DEF VAR xar-pd-acct LIKE account.actnum NO-UNDO.  /* currency ar account */
DEF VAR lorow AS INT NO-UNDO INIT 21.
DEF VAR v-post-ok AS LOG NO-UNDO.
def var total-msf like ar-invl.amt-msf NO-UNDO.
def var v-s-inv-no like inv-head.inv-no init 0 no-undo.
def var v-e-inv-no like v-s-inv-no init 999999 NO-UNDO.
def var v-s-date   like inv-head.inv-date format "99/99/9999"
                                          init 01/01/0001 no-undo.
def var v-e-date   like v-s-date init TODAY NO-UNDO.
def var export_opt      as char no-undo initial "ASI".
def var t-rec-written   as int  no-undo.
def var v-rec-written   as int  no-undo.
DEF NEW SHARED VAR v-term-id AS cha NO-UNDO.
DEF VAR v-print-fmt AS cha NO-UNDO.
DEF VAR v-ftp-done AS LOG NO-UNDO.
DEF VAR v-sort AS LOGICAL INIT YES FORMAT "Y/N" NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "INVPRINT"
      no-lock no-error.
v-print-fmt = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".

/*IF PROGRAM-NAME(2) MATCHES "*r-inve&p*" THEN
posting = TRUE.
ELSE
posting = FALSE.
*/
posting = IF ip-post THEN YES ELSE NO.

DO TRANSACTION:
  {sys/inc/postdate.i}
  {sys/inc/inexport.i}
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_inv end_inv ~
begin_date end_date tb_sort tb_ton tb_export lines-per-page rd-dest lv-ornt ~
lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_inv end_inv ~
begin_date end_date tb_sort tb_ton tb_export lines-per-page rd-dest lv-ornt ~
lv-font-no td-show-parm lv-font-name 

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
     LABEL "Beginning Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_inv AS INTEGER FORMAT ">>>>>>>>>" INITIAL 0 
     LABEL "Beginning Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv AS INTEGER FORMAT ">>>>>>>>>" INITIAL 999999999 
     LABEL "Ending Invoice#" 
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
     SIZE 99 BY 6.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 11.19.

DEFINE VARIABLE tb_export AS LOGICAL INITIAL no 
     LABEL "Export/FTP  Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sort AS LOGICAL INITIAL yes 
     LABEL "Sort by Customer Name?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_ton AS LOGICAL INITIAL no 
     LABEL "Print $/Ton?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.43 COL 48 COLON-ALIGNED
     tran-period AT ROW 3.62 COL 48 COLON-ALIGNED
     begin_inv AT ROW 5.29 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number"
     end_inv AT ROW 5.29 COL 72 COLON-ALIGNED HELP
          "Enter Ending Invoice Number"
     begin_date AT ROW 6.24 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date"
     end_date AT ROW 6.24 COL 72 COLON-ALIGNED HELP
          "Enter Ending Invoice Date"
     tb_sort AT ROW 7.91 COL 37
     tb_ton AT ROW 8.86 COL 37
     tb_export AT ROW 10.05 COL 37
     lines-per-page AT ROW 13.38 COL 66 COLON-ALIGNED
     rd-dest AT ROW 13.62 COL 9 NO-LABEL
     lv-ornt AT ROW 15.05 COL 41 NO-LABEL
     lv-font-no AT ROW 16.71 COL 41 COLON-ALIGNED
     td-show-parm AT ROW 17.67 COL 8
     lv-font-name AT ROW 17.67 COL 34 COLON-ALIGNED NO-LABEL
     btn-ok AT ROW 20.05 COL 23
     btn-cancel AT ROW 20.05 COL 58
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.67 COL 6
     RECT-6 AT ROW 12.43 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 100.2 BY 21.38.


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
         TITLE              = "A/R Invoice Edit/Posting Register"
         HEIGHT             = 21.67
         WIDTH              = 102
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
       begin_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_export:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_export:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_ton:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
ON END-ERROR OF C-Win /* A/R Invoice Edit/Posting Register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* A/R Invoice Edit/Posting Register */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Invoice Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv C-Win
ON LEAVE OF begin_inv IN FRAME FRAME-A /* Beginning Invoice# */
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
  DEF VAR lv-post AS LOG NO-UNDO.

  run check-date.
  if v-invalid then return no-apply.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  ASSIGN v-s-inv-no = begin_inv
         v-e-inv-no = END_inv
         v-s-date   = begin_date
         v-e-date   = END_date   
         v-sort     = tb_sort.

  v-postable = NO.

  DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        ASSIGN v-trnum       = gl-ctrl.trnum + 1
               gl-ctrl.trnum = v-trnum.
        FIND CURRENT gl-ctrl NO-LOCK.
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

  IF v-postable AND ip-post THEN DO:

    lv-post = NO.

    MESSAGE "Post Invoices?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

    IF lv-post THEN do:
      RUN post-inv.
      RUN copy-report-to-audit-dir.
      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.      
    END.
    ELSE RUN undo-trnum.
  END.

  ELSE RUN undo-trnum.
/*
  ELSE MESSAGE "No Invoices available for posting..." VIEW-AS ALERT-BOX ERROR.
*/
  IF v-ftp-done THEN MESSAGE "File Export/FTP is completed." VIEW-AS ALERT-BOX INFORMATION.

  SESSION:SET-WAIT-STATE("").

  IF ip-post THEN APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Invoice Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv C-Win
ON LEAVE OF end_inv IN FRAME FRAME-A /* Ending Invoice# */
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
  ASSIGN lines-per-page = 68.
  DISP lines-per-page WITH FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME tb_export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_export C-Win
ON VALUE-CHANGED OF tb_export IN FRAME FRAME-A /* Export/FTP  Invoices? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort C-Win
ON VALUE-CHANGED OF tb_sort IN FRAME FRAME-A /* Sort by Customer Name? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ton
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ton C-Win
ON VALUE-CHANGED OF tb_ton IN FRAME FRAME-A /* Print $/Ton? */
DO:
  IF {&self-name}:SCREEN-VALUE EQ "Yes" THEN
    lv-ornt:SCREEN-VALUE = "L".
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

  RUN init-proc.

  RUN enable_UI.

  IF NOT ip-post THEN
      ASSIGN tran-date:HIDDEN IN FRAME {&FRAME-NAME} = YES
             tran-period:HIDDEN IN FRAME {&FRAME-NAME} = YES
             tran-date = TODAY.

  DO WITH FRAME {&frame-name}:
    {custom/usrprint.i}

    ASSIGN
     end_date:SCREEN-VALUE = STRING(TODAY)
     end_date              = TODAY.

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

    IF v-print-fmt = "Frankstn" OR v-print-fmt = "MIRPKG" OR v-print-fmt = "ContSrvc"
        THEN tb_export:SENSITIVE = YES.
    ELSE ASSIGN tb_export = NO
                tb_export:SCREEN-VALUE = "NO" 
                tb_export:SENSITIVE = NO.

    APPLY "entry" TO tran-date.
  END.

  {methods/nowait.i}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-ar C-Win 
PROCEDURE clear-ar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each ar-inv where ar-inv.company  eq cocode
                  and ar-inv.posted   eq no
                  /*and ar-inv.printed  eq NO */ :

    IF NOT CAN-FIND(FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no) THEN
       DELETE ar-inv.
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

  ASSIGN targetfile = lv-audit-dir + "\AR\AU4\Run#"
                    + STRING(v-trnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AR"
         dirname3 = lv-audit-dir + "\AR\AU4".

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
  DISPLAY tran-date tran-period begin_inv end_inv begin_date end_date tb_sort 
          tb_ton tb_export lines-per-page rd-dest lv-ornt lv-font-no 
          td-show-parm lv-font-name 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_inv end_inv begin_date end_date tb_sort 
         tb_ton tb_export lines-per-page rd-dest lv-ornt lv-font-no 
         td-show-parm btn-ok btn-cancel 
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

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "AREXP"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "AREXP"
   sys-ctrl.descrip = "A/R Export option"
   sys-ctrl.char-fld = "ASI".
  message "System control record NOT found.  Please enter A/R Export Option".
  update sys-ctrl.char-fld.
end.
export_opt = sys-ctrl.char-fld.

RELEASE sys-ctrl.

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

DO :
  FIND FIRST ar-ctrl WHERE ar-ctrl.company = cocode NO-LOCK.
  xar-acct = ar-ctrl.receivables.
  FIND FIRST account WHERE
    account.company = cocode AND
    account.actnum  = ar-ctrl.receivables NO-ERROR.
  IF NOT AVAILABLE account THEN
  DO:    
    MESSAGE "A/R Control Files has a null or invalid Receivables Account." VIEW-AS ALERT-BOX ERROR.    
  END.
  xar-freight = ar-ctrl.freight.
  FIND FIRST account WHERE
    account.company = cocode AND
    account.actnum  = ar-ctrl.freight NO-ERROR.
  IF NOT AVAILABLE account THEN
  DO:    
    MESSAGE "A/R Control Files has a null or invalid Freight Account." VIEW-AS ALERT-BOX ERROR.    
  END.
  xar-stax = ar-ctrl.stax.
  FIND FIRST account WHERE
    account.company = cocode AND
    account.actnum  = ar-ctrl.stax NO-ERROR.
  IF NOT AVAILABLE account THEN
  DO:    
    MESSAGE "A/R Control Files has a null or invalid Sales Tax Account." VIEW-AS ALERT-BOX ERROR.    
  END.
  RELEASE ar-ctrl.
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-gl C-Win 
PROCEDURE post-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
  post-2:
  do transaction:
    if not v-post-ok then leave post-2.

    for each wkdistrib:
      /*create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = IF wkdistrib.use-cur-act THEN xar-cur-acct ELSE xar-acct
       gltrans.jrnl    = "ARINV"
       gltrans.tr-dscr = "ACCOUNTS RECEIVABLE INVOICE"
       gltrans.tr-date = tran-date
       gltrans.tr-amt  = (-1 * wkdistrib.amount)
       gltrans.period  = tran-period
       gltrans.trnum   = v-trnum.*/

      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = wkdistrib.actnum
       gltrans.jrnl    = "ARINV"
       gltrans.tr-dscr = wkdistrib.tr-dscr
       gltrans.tr-date = tran-date
       gltrans.tr-amt  = wkdistrib.amount
       gltrans.period  = tran-period
       gltrans.trnum   = v-trnum.
      RELEASE gltrans.
    end.    /* each wkdistrib */

    if export_opt = "SONOCO" then  /* 9812 CAH */
      run ar/sonoinv.p ("total", t-rec-written, output v-rec-written).
  end. /* post-2 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-inv C-Win 
PROCEDURE post-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
post-1:
do transaction on error undo with width 255:

  v-post-ok = yes.

  for each ar-inv
      where ar-inv.company  eq cocode
        and ar-inv.posted   eq no
        and ar-inv.printed  eq yes
        and ar-inv.inv-no   ge v-s-inv-no
        and ar-inv.inv-no   le v-e-inv-no
        and ar-inv.inv-date ge v-s-date
        and ar-inv.inv-date le v-e-date
      on error undo post-1, leave post-1:

    put screen row lorow columns 70 string(ar-inv.inv-no,">>>>>9") .

    assign
     ar-inv.period = tran-period
     ar-inv.posted = yes.

     run oe/sonofile.p (1,recid(ar-inv)).

     if export_opt = "SONOCO" then do:  /* 9812 CAH */
       run ar/sonoinv.p ("ar-inv", recid(ar-inv), output v-rec-written).
       assign t-rec-written = t-rec-written + v-rec-written.
     end.
     
      /* Create eddoc for invoice if required */
      FIND FIRST edmast NO-LOCK
          WHERE edmast.cust EQ ar-inv.cust-no
          NO-ERROR.
      /* ar-inv.spare-int-1 indicates selected for EDI */
      IF AVAILABLE edmast AND ar-inv.spare-int-1 EQ 1 THEN DO:   
          FIND FIRST eddoc NO-LOCK 
            WHERE eddoc.setid EQ '810'
              AND eddoc.partner EQ edmast.partner
              AND eddoc.docid = STRING(ar-inv.inv-no) 
            NO-ERROR.
          IF NOT AVAILABLE eddoc THEN DO:   
            RUN ed/asi/o810hook.p (recid(ar-inv), no, no).     
            FIND FIRST edcode NO-LOCK
                WHERE edcode.partner EQ edmast.partner
                NO-ERROR.
            IF NOT AVAIL edcode THEN 
               FIND FIRST edcode NO-LOCK
                  WHERE edcode.partner EQ edmast.partnerGrp
                  NO-ERROR.
            IF AVAIL edcode AND edcode.sendFileOnPrint THEN    
              RUN ed/asi/write810.p (INPUT cocode, INPUT ar-inv.inv-no).
          END. /* If eddoc not available */    
      END. /* If edi 810 customer */
    find first cust
        {sys/ref/custW.i}
          and cust.cust-no eq ar-inv.cust-no
        use-index cust exclusive-lock .
    assign
     cust.sales[tran-period]   = cust.sales[tran-period]   +
                             (ar-inv.net - ar-inv.tax-amt)
     cust.n-sales[tran-period] = cust.n-sales[tran-period] + 1
/*     cust.sales[13]        = cust.sales[13]        +
                             (ar-inv.net  - ar-inv.tax-amt)
*/
     cust.ytd-sales        = cust.ytd-sales        +
                             (ar-inv.net  - ar-inv.tax-amt)
     cust.n-sales[13]      = cust.n-sales[13]      + 1
     cust.acc-bal          = cust.acc-bal          + ar-inv.net.

    if cust.acc-bal ge cust.hibal then
      assign
       cust.hibal      = cust.acc-bal
       cust.hibal-date = ar-inv.inv-date.

    for each ar-invl where ar-invl.x-no eq ar-inv.x-no:

      assign
       ar-invl.period  = tran-period
       ar-invl.posted  = yes
       ar-invl.inv-qty = ar-invl.qty
       total-msf       = total-msf + ar-invl.amt-msf.

      /*cost entered from A-U-1*/
      IF ar-invl.t-cost EQ 0 AND ar-invl.cost NE 0 THEN
      DO:
         IF ar-invl.dscr[1] EQ "M" OR ar-invl.dscr[1] EQ ""  THEN /*M*/
            ar-invl.t-cost = ar-invl.cost * (ar-invl.inv-qty / 1000).
         ELSE /*EA*/
            ar-invl.t-cost = ar-invl.cost * ar-invl.inv-qty.
      END.

      if export_opt eq "Sonoco" then run oe/sonofile.p (2,recid(ar-invl)).
      else
      if export_opt eq "Inland" then run ar/jdedward.p (recid(ar-invl)).
    end. /* for each ar-invl */

    assign
     cust.ptd-msf[tran-period] = cust.ptd-msf[tran-period] + total-msf
     cust.ytd-msf          = cust.ytd-msf          + total-msf.

    create ar-ledger.
    assign
     ar-ledger.company  = cocode
     ar-ledger.cust-no  = ar-inv.cust-no
     ar-ledger.amt      = - ar-inv.net
     ar-ledger.ref-num  = "INV# " + string(ar-inv.inv-no)
     ar-ledger.ref-date = ar-inv.inv-date
     ar-ledger.tr-num   = v-trnum
     ar-ledger.tr-date  = tran-date.
    RELEASE ar-ledger.
  end. /* for each ar-inv */

  FIND CURRENT cust NO-LOCK NO-ERROR.

  RUN post-gl.
  RUN clear-ar.
end. /* post-1 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- oe/invpost.p 10/94 gb */
/* Invoicing  - Edit Register & Post Invoicing Transactions                   */
/* -------------------------------------------------------------------------- */

 FOR EACH wkdistrib:
     DELETE wkdistrib.
 END.

FORM
  WITH FRAME f-distrib DOWN WIDTH 140 NO-BOX STREAM-IO.

FORM
  WITH FRAME f-distrib-t DOWN WIDTH 140 NO-BOX STREAM-IO.

  DEF VAR time_stamp AS ch NO-UNDO.
  DEF VAR g1 AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
  DEF VAR g2 LIKE g1 NO-UNDO.
  DEF VAR g3 LIKE g1 NO-UNDO.
  DEF VAR g4 LIKE g1 NO-UNDO.
  DEF VAR v1 LIKE g1 NO-UNDO.
  DEF VAR v2 LIKE g1 NO-UNDO.
  DEF VAR tot LIKE g1 NO-UNDO.
  DEF VAR ld-tons AS DEC EXTENT 3 NO-UNDO.
  DEF VAR ld-pton AS DEC NO-UNDO.
  /*DEF VAR v-sort AS LOGICAL /*INIT YES*/ FORMAT "Y/N" NO-UNDO.*/
  DEF VAR ld-gl-amt AS DEC NO-UNDO.

  def var ws_debit  like wkdistrib.amount column-label "Debit" no-undo.
def var ws_credit like wkdistrib.amount column-label "Credit" no-undo.
def var ws_net    like wkdistrib.amount column-label "Net"   no-undo.
def var num-recs  like wkdistrib.recs no-undo.
def var tot-debit  like wkdistrib.amount no-undo.
def var tot-credit like wkdistrib.amount no-undo.

{sys/form/r-top3w.f}

DEF VAR lv-head AS CHAR FORMAT 'X(78)' NO-UNDO.
IF NOT posting THEN
lv-head =
"C U S T O M E R   I N V O I C E   E D I T   R E G I S T E R".
ELSE
lv-head =
"I N V O I C E   P O S T I N G".
/* {sys/inc/ctrtext.i head 80}. */


  FIND first period                   
      where period.company eq gcompany
        and period.pst     le tran-date
        and period.pend    ge tran-date
      no-lock no-error.


  FORMAT HEADER
  "CUST.#   Name                          INVOICE# INV.DATE"
  "        AMOUNT  LINE DESCRIPTION" SKIP
  FILL("_",130) FORMAT "x(130)"
  WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top PAGE-TOP WIDTH 132 STREAM-IO.

  FORMAT HEADER
  "CUST.#   Name                          INVOICE# INV.DATE"
  "        AMOUNT  LINE DESCRIPTION                        "
  "             $/TON      TONS"
  SKIP FILL("_",142) FORMAT "x(142)"
  WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top-t PAGE-TOP WIDTH 142 STREAM-IO.

ASSIGN
   v-sort = (IF posting THEN YES ELSE v-sort) /* for posting */
   time_stamp = STRING(TIME, "HH:MMam")
   tmpstore   = FILL("_",125).

  {sys/inc/print1.i}

  {sys/inc/outprint.i VALUE(lines-per-page)}

  IF td-show-parm THEN RUN show-param.
ASSIGN v1 = 0
       v2 = 0
       g1 = 0
       g2 = 0
       g3 = 0
       g4 = 0
       .

DO: /* REPEAT: 9508 cah */
  SESSION:SET-WAIT-STATE("general").  /*{sys/msg/rproc.i} */
  ASSIGN
    str-tit  = coname + " - " + loname
    str-tit2 = "CUSTOMER INVOICES  -  "
    + (IF posting THEN "POSTING REPORT" ELSE "EDIT REGISTER") + "  " + STRING(v-trnum)
    str-tit3 = STRING(tran-date) + " - Period " + STRING(tran-period,"99")
    {sys/inc/ctrtext.i str-tit 112}
    {sys/inc/ctrtext.i str-tit2 114}
    {sys/inc/ctrtext.i str-tit3 132}.


  DISPLAY "" WITH FRAME r-top.
  IF tb_ton THEN
    DISPLAY "" WITH FRAME f-top-t.
  ELSE
    DISPLAY "" WITH FRAME f-top.

  {sa/sa-sls01.i}
  v-term-id = v-term.

  FOR EACH cust {sys/ref/custW.i} no-lock,
      EACH ar-inv
      WHERE ar-inv.company  EQ cocode
        AND ar-inv.posted   EQ NO
        AND (IF posting THEN ar-inv.printed EQ TRUE ELSE TRUE)
        AND ar-inv.cust-no  EQ cust.cust-no
        AND ar-inv.loc      NE "Zqw"
        AND ar-inv.inv-no   GE v-s-inv-no
        AND ar-inv.inv-no   LE v-e-inv-no
        AND ar-inv.inv-date GE v-s-date
        AND ar-inv.inv-date LE v-e-date
        AND CAN-FIND(FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no
                      /*AND ar-invl.amt NE 0*/ )
      NO-LOCK

      BREAK BY (IF v-sort THEN cust.name ELSE "")
            BY cust.cust-no
            BY ar-inv.inv-no
      WITH FRAME f-det:

   CREATE report.
   ASSIGN
   report.term-id = v-term-id
   report.key-01  = STRING(ar-inv.inv-no,"9999999999")
   report.rec-id  = RECID(ar-inv).  

IF cust.factored THEN
   for each ar-invl no-lock where ar-invl.x-no = ar-inv.x-no:
       IF CAN-FIND(FIRST itemfg WHERE itemfg.company  EQ ar-inv.company
                              AND itemfg.i-no     EQ ar-invl.i-no
                              AND itemfg.factored = yes)
           OR ar-invl.i-no = ""
       THEN DO:
            report.key-02 = "Factored".  /* for oe/rep/expfrank.p task#  09200521*/
            LEAVE.
       END.
   END.

    put screen row lorow columns 70 string(ar-inv.inv-no,">>>>>9") .
    if first-of(cust.cust-no) then put cust.cust-no space(1) cust.name.


    PUT ar-inv.inv-no   TO 47
        ar-inv.inv-date AT 49 FORM "99/99/99"
        ar-inv.net      AT 58.

    ASSIGN
       v-postable = YES
       v2 = v2 + net
       v1 = v1 + ar-inv.disc-taken.

    RELEASE currency.
    IF lv-comp-curr NE "" AND lv-comp-curr NE ar-inv.curr-code[1] THEN
    FIND FIRST currency NO-LOCK
        WHERE currency.company     EQ ar-inv.company
          AND currency.c-code      EQ ar-inv.curr-code[1]
          AND currency.ar-ast-acct NE ""
          AND currency.ex-rate     GT 0
        NO-ERROR.        

    {sys/inc/gldstsum.i xar-acct "ar-inv.gross" YES "HEADER"}

    IF AVAIL currency THEN DO:
      ld-gl-amt = (ar-inv.gross * currency.ex-rate) - ar-inv.gross.
      {sys/inc/gldstsum.i currency.ar-ast-acct ld-gl-amt YES "HEADER"}
    END.

    FOR EACH ar-invl NO-LOCK WHERE ar-invl.x-no EQ ar-inv.x-no
        BREAK BY ar-invl.line
        WITH FRAME f-det NO-BOX NO-LABELS WIDTH 132:

      FIND FIRST itemfg
          WHERE itemfg.company EQ ar-inv.company
            AND itemfg.i-no    EQ ar-invl.i-no
            AND ar-invl.i-no   NE ""
            AND ar-invl.misc   EQ NO
          NO-LOCK NO-ERROR.

      ld-gl-amt = ar-invl.amt * -1 *
                  (IF AVAIL currency THEN currency.ex-rate ELSE 1).

      {sys/inc/gldstsum.i ar-invl.actnum ld-gl-amt NO "LINE"}

      ASSIGN
       ld-tons[1] = IF ar-invl.t-weight NE 0 THEN ar-invl.t-weight
                    ELSE
                    IF AVAIL itemfg THEN (itemfg.weight-100 * ar-invl.inv-qty / 100)
                    ELSE 0
       ld-tons[1] = ld-tons[1] / 2000.

      IF ld-tons[1] EQ ? THEN ld-tons[1] = 0.

      assign
       wkdistrib.tons = ld-tons[1]
       ld-pton        = ar-invl.amt / ld-tons[1]
       ld-tons[2]     = ld-tons[2] + ld-tons[1].

      IF ld-pton EQ ? THEN ld-pton = 0.

      PUT ar-invl.line                      AT 75   FORMAT ">>9"
          ar-invl.i-name                    AT 79
          SPACE(1)
          ar-invl.amt.

      IF tb_ton THEN
        PUT ld-pton                         TO 132  FORMAT "->>>>9.99"
            ld-tons[1]                      TO 142  FORMAT "->>>9.999".

      PUT SKIP.

      IF ar-invl.i-dscr NE "" THEN PUT ar-invl.i-dscr AT 79 SKIP.
    END. /* each ar-invl */

    IF ar-inv.freight NE 0 THEN DO:
      PUT "FREIGHT" AT 79 SPACE(1)
          ar-inv.freight FORMAT "->>,>>>,>>9.99" AT 110 SKIP.

      ld-gl-amt = ar-inv.freight * -1 *
                  (IF AVAIL currency THEN currency.ex-rate ELSE 1).

      {sys/inc/gldstsum.i xar-freight ld-gl-amt NO "FREIGHT"}
    END.

    IF ar-inv.tax-amt NE 0 THEN DO:
      FIND FIRST stax
          WHERE stax.company EQ ar-inv.company
            AND stax.tax-group = ar-inv.tax-code
          NO-LOCK NO-ERROR.

      PUT "TAX" AT 79 SPACE(1)
          ar-inv.tax-amt FORMAT "->>,>>>,>>9.99" AT 110 SKIP.

      IF AVAIL stax THEN DO:
        DEF VAR tot-tax AS DECIMAL NO-UNDO.
        DEF VAR ws_taxacct AS CHAR NO-UNDO.
        DEF VAR last_one AS INTEGER NO-UNDO.
        DEF VAR v-jd-taxamt AS DECIMAL NO-UNDO.
        DEF VAR v-tax-rate AS DECIMAL NO-UNDO DECIMALS 8.

        ASSIGN
           v-tax-rate = 0
           tot-tax = ar-inv.tax-amt.

        DO i = 1 TO 3:
          v-tax-rate = v-tax-rate + stax.tax-rate[i].
          IF stax.tax-rate[i] NE 0 THEN last_one = i.
        END.
        DO i = 1 TO 3:
          IF stax.tax-rate[i] NE 0 THEN DO:
            FIND account NO-LOCK
                WHERE account.company = cocode
                  AND account.actnum = stax.tax-acc[i]
                NO-ERROR.
            ASSIGN
             ws_taxacct  = IF AVAIL account THEN stax.tax-acc[i] ELSE xar-stax
             v-jd-taxamt = ROUND((stax.tax-rate[i] / v-tax-rate) * ar-inv.tax-amt,2)
             tot-tax     = tot-tax - v-jd-taxamt.
            /* add in any residual amount */
            IF i EQ last_one THEN v-jd-taxamt = v-jd-taxamt + tot-tax.

            ld-gl-amt = v-jd-taxamt * -1 *
                       (IF AVAIL currency THEN currency.ex-rate ELSE 1).

            {sys/inc/gldstsum.i ws_taxacct ld-gl-amt NO "TAX"}
          END.
          RELEASE wkdistrib.
        END.
      END.

      ELSE DO:
        ld-gl-amt = ar-inv.tax-amt * -1 *
                    (IF AVAIL currency THEN currency.ex-rate ELSE 1).

        {sys/inc/gldstsum.i xar-stax ld-gl-amt NO "TAX"}
      END.
    END.  /* non-zero tax amount */

    IF LAST-OF(cust.cust-no) THEN DO:
      ld-pton = v2 / ld-tons[2].
      IF ld-pton EQ ? THEN ld-pton = 0.

      IF tb_ton THEN
        DISPLAY "*  CUSTOMER TOTALS"      TO 56
                v2                        AT 58 " *"
                ld-pton    WHEN tb_ton    TO 132      FORMAT "->>>>9.99"
                ld-tons[2] WHEN tb_ton    TO 142      FORMAT "->>>9.999"
                SKIP(1)

          WITH FRAME vtot{&frame}-t NO-BOX NO-LABELS WIDTH 142 STREAM-IO.

      ELSE
        DISPLAY "*  CUSTOMER TOTALS"      TO 56
                v2                        AT 58 " *"
                SKIP(1)

          WITH FRAME vtot{&frame} NO-BOX NO-LABELS WIDTH 132 STREAM-IO.

      ASSIGN
        g1 = g1 + v1
        g2 = g2 + v2
        ld-tons[3] = ld-tons[3] + ld-tons[2]
        v1 = 0
        v2 = 0
        ld-tons[2] = 0.
    END.

    ASSIGN
       g3 = g3 + ar-inv.tax-amt
       g4 = g4 + ar-inv.freight.
  END. /* each invoice */

  ld-pton = g2 / ld-tons[3].
  IF ld-pton EQ ? THEN ld-pton = 0.

  IF tb_ton THEN
    DISPLAY "** GRAND TOTAL  "        TO 54
            g2                        AT 58 " **"
            ld-pton    WHEN tb_ton    TO 132            FORMAT "->>>>9.99"
            ld-tons[3] WHEN tb_ton    TO 142            FORMAT "->>>9.999"

        WITH NO-BOX NO-LABELS NO-UNDERLINE WIDTH 142 FRAME gt-t STREAM-IO.

  ELSE
    DISPLAY "** GRAND TOTAL  "        TO 54
            g2                        AT 58 " **"

        WITH NO-BOX NO-LABELS NO-UNDERLINE WIDTH 132 FRAME gt STREAM-IO.

  HIDE FRAME f-top.
  HIDE FRAME f-top-t.

  str-tit3 = STRING(tran-date) + " - Period " + STRING(tran-period,"99") + " - " +
    "Summary by Account".
  {sys/inc/ctrtext.i str-tit3 132}.

  PAGE.

  DO WITH FRAME f-distrib:
    ASSIGN
     num-recs = 0
     ws_net = 0
     ws_debit = 0
     ws_credit = 0
     ld-tons = 0.

    /*
    FOR EACH wkdistrib BREAK BY wkdistrib.actnum:
      num-recs = num-recs + 1.

      IF (NOT wkdistrib.use-cur-act AND wkdistrib.actnum EQ xar-acct) OR
         (wkdistrib.use-cur-act AND wkdistrib.actnum EQ xar-cur-acct) OR
         (wkdistrib.use-cur-act AND wkdistrib.actnum EQ xar-pd-acct)  THEN
        ASSIGN
         ws_credit  = ws_credit + (-1 * wkdistrib.amount)
         tot-credit = tot-credit + ( -1 * wkdistrib.amount)
         ws_debit   = ws_debit + (-1 * wkdistrib.amount)
         tot-debit  = tot-debit + (-1 * wkdistrib.amount).

      ELSE
        ASSIGN
         ws_debit  = ws_debit + (-1 * wkdistrib.amount) 
         tot-debit = tot-debit + (-1 * wkdistrib.amount).

      IF LAST-OF(wkdistrib.actnum)  THEN DO:
         find account where account.company eq cocode
              and account.actnum  eq wkdistrib.actnum
          no-lock no-error.
          display wkdistrib.actnum
              account.dscr when avail account
              num-recs
              ws_debit when ws_debit ne 0
              ws_credit when ws_credit ne 0
           with frame f-distrib.
          if NOT avail account then DO:
              display "*NOT IN ACCOUNT FILE*" @ account.dscr
              with frame f-distrib.         
          END.
          down 1 with frame f-distrib.
          ASSIGN num-recs = 0
                 ws_debit = 0
                 ws_credit = 0.
      END.
    end.

    if avail wkdistrib then do:
      IF wkdistrib.use-cur-act THEN 
          find account
          where account.company eq cocode
            and account.actnum  eq xar-cur-acct
          no-lock no-error.
      ELSE find account
          where account.company eq cocode
            and account.actnum  eq xar-acct
          no-lock no-error.
      display xar-acct @ wkdistrib.actnum
              account.dscr when avail account
              num-recs
              ws_debit when ws_debit ne 0
              ws_credit when ws_credit ne 0
           with frame f-distrib.
      if not avail account then DO:
              display "*NOT IN ACCOUNT FILE*" @ account.dscr
           with frame f-distrib.

      END.
      down 1 with frame f-distrib.
    end.*/

    for each wkdistrib break by wkdistrib.actnum:
      if first-of(wkdistrib.actnum) then
        assign
         num-recs = 0
         ws_debit = 0
         ws_credit = 0
         ld-tons[2] = 0.

      num-recs = num-recs + 1.

      IF wkdistrib.debit then
        ASSIGN
         ws_debit   = ws_debit + wkdistrib.amount
         tot-debit  = tot-debit + wkdistrib.amount.
      ELSE
        ASSIGN
         ws_credit  = ws_credit + ( -1 * wkdistrib.amount)
         tot-credit = tot-credit + ( -1 * wkdistrib.amount)
         ld-tons[1] = wkdistrib.tons
         ld-tons[2] = ld-tons[2] + ld-tons[1]
         ld-tons[3] = ld-tons[3] + ld-tons[1].

      find first account
          where account.company eq cocode
            and account.actnum  eq wkdistrib.actnum
          no-lock no-error.
      if last-of(wkdistrib.actnum) then do:
        ld-pton = ws_credit / ld-tons[2].
        IF ld-pton EQ ? THEN ld-pton = 0.

        IF tb_ton THEN DO WITH FRAME f-distrib-t:
          display wkdistrib.actnum
                  account.dscr            when avail account
                  num-recs
                  ws_debit                when ws_debit ne 0
                  ws_credit               when ws_credit ne 0
                  ld-pton                 when ws_credit ne 0
                                          COLUMN-LABEL "$/Ton"
                                          FORMAT "->>>>9.99"
                  ld-tons[2]              when ws_credit ne 0
                                          COLUMN-LABEL "Tons"
                                          FORMAT "->>>9.999".

          if not avail account then
            display "*NOT IN ACCOUNT FILE*" @ account.dscr.
          down 1.
        END.

        ELSE
        DO WITH FRAME f-distrib:
          display wkdistrib.actnum
                  account.dscr            when avail account
                  num-recs
                  ws_debit                when ws_debit ne 0
                  ws_credit               when ws_credit ne 0.

          if not avail account then
            display "*NOT IN ACCOUNT FILE*" @ account.dscr.
          down 1.
        END.
      end.
    end. /* for each */

    ASSIGN
       ws_net = tot-debit - tot-credit
       ld-pton = tot-credit / ld-tons[3].

    IF ld-pton EQ ? THEN ld-pton = 0.

    IF tb_ton THEN DO WITH FRAME f-distrib-t:
      underline ws_debit ws_credit ld-pton ld-tons[2].
      down 1.

      display "Totals" @ account.dscr
              tot-debit @ ws_debit
              tot-credit @ ws_credit
              ld-pton
              ld-tons[3] @ ld-tons[2].
      down 1.

      display "Net" @ account.dscr.
      if ws_net gt 0 then display ws_net @ ws_debit.
      else display (-1 * ws_net) @ ws_credit.
      down 1.
    END.

    ELSE
    DO WITH FRAME f-distrib:
      underline ws_debit ws_credit.
      down 1.

      display "Totals" @ account.dscr
              tot-debit @ ws_debit
              tot-credit @ ws_credit.
      down 1.

      display "Net" @ account.dscr.
      if ws_net gt 0 then display ws_net @ ws_debit.
      else display (-1 * ws_net) @ ws_credit.
      down 1.
    END.
  end.
END.

HIDE ALL NO-PAUSE.
OUTPUT CLOSE.

/*IF v-print-fmt <> "frankstn" AND v-print-fmt <> "Mirpkg" THEN  tb_export = NO.*/
v-ftp-done = NO.
IF tb_export AND inexport-log THEN DO:    
   DEF VAR v-exp-file AS cha NO-UNDO.
   v-exp-file = inexport-desc +  
                "ARINV_" + trim(v-print-fmt) + 
                    substr(string(year(today),"9999"),3,2) +
                    string(month(today),"99") +
                    string(day(today),"99") +
                    substr(string(time,"HH:MM:SS"),1,2) +
                    substr(string(time,"HH:MM:SS"),4,2) +
                    substr(string(time,"HH:MM:SS"),7,2) + ".dat".
   OUTPUT TO VALUE(v-exp-file).
   IF inexport-cha EQ "CIT" THEN DO:
      RUN ar/rep/expfrank.p .
      OUTPUT CLOSE.
      OUTPUT TO VALUE(".\ar\ftpcmd2.txt").     /* ftp text file */
      PUT UNFORMATTED 
       /*"open cs.ftptst.citonline.com" SKIP  /* ftp test server ip address */   */
       "open cs.ftp.citonline.com" SKIP  /* ftp Production server ip address */
       "ftpa1526" SKIP  /* userid*/
       "none" SKIP  /* password */
       "put " v-exp-file " " '"' "$$ ID=EP003F BID='DI1526' PASSWORD=NARF" '"' SKIP         /* file to transfer */
       "quit" .
      OUTPUT CLOSE.
/*      OS-COMMAND SILENT value("ftp -v -i -s:.\oe\ftpcmd2.txt"). */
      v-ftp-done = YES.
   END.
   ELSE IF inexport-cha EQ "ContSrvc" THEN DO:
        OUTPUT TO VALUE(v-exp-file).
        RUN ar/rep/expconts.p .
        OUTPUT CLOSE.
   END.
END.

FOR EACH report WHERE report.term-id EQ v-term-id: 
  DELETE report.
END.

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
/* gdm - 11050906 */
REPEAT:
 FIND FIRST gl-ctrl EXCLUSIVE-LOCK
   WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
 IF AVAIL gl-ctrl THEN DO:
   IF v-trnum = gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
   release gl-ctrl.
   LEAVE.
 END. /* IF AVAIL gl-ctrl */
END. /* REPEAT */
/* gdm - 11050906 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

