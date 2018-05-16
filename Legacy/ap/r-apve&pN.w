&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap\r-apve&pN.w

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
DEF VAR ll-secure AS LOG NO-UNDO.

DEFINE BUFFER bf-chk FOR ap-chk.

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

def new shared var v-post as log init NO NO-UNDO.
def new shared var v-trnum as INT NO-UNDO.

def var v-unline as char format "x(80)" init
  "--------------- ------------------------- ------- ----------- ---".
def var time_stamp as ch.

DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-invalid-inv AS LOG NO-UNDO.
DEF VAR v-frt-acct LIKE ap-ctrl.freight NO-UNDO.
DEF VAR v-cash-acct AS CHAR NO-UNDO.
DEF VAR xap-acct LIKE account.actnum NO-UNDO.
DEF VAR xap-stax LIKE account.actnum NO-UNDO.
DEF VAR lv-frt-total AS DEC NO-UNDO.  /* accum total */
def var v-postable as log init NO NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-post AS LOG NO-UNDO.
DEF VAR ll-warned AS LOG NO-UNDO.
DEF VAR v-bank-code AS CHAR NO-UNDO.
DEF VAR lv-bank-acct AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD actnum LIKE account.actnum
    FIELD ex-rate LIKE currency.ex-rate INIT 1
    FIELD curr-amt LIKE ap-inv.net.

DEF VAR v-fgpostgl AS LOG NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.
DEF VAR lv-fgpost-dir AS LOG NO-UNDO.

DO TRANSACTION :
  {sys/inc/fgpostgl.i}
  v-fgpostgl = fgpostgl NE "None".
  {sys/inc/rmpostgl.i}
  {sys/inc/postdate.i}
  {sys/inc/apsecure.i}
  {sys/inc/apautocheck.i}
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
DO TRANSACTION:

   find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "GLPOST"
    no-lock no-error.


   if not avail sys-ctrl then do:
      create sys-ctrl.
      assign
         sys-ctrl.company  = cocode
         sys-ctrl.name     = "GLPOST"
         sys-ctrl.log-fld  = NO
         sys-ctrl.descrip  = "Post AP Invoices within current period only? Default to NO "
         sys-ctrl.char-fld = "User Defined Password to be entered when Logical Value = YES ".
      END.
END.      
      lv-fgpost-dir = sys-ctrl.log-fld .

DEF TEMP-TABLE tt-ap-invl NO-UNDO
                          FIELD row-id AS ROWID
                          FIELD actnum LIKE account.actnum
                          FIELD unit-pr LIKE ap-invl.unit-pr
                          FIELD amt LIKE ap-invl.amt
                          INDEX row-id row-id.

DEF TEMP-TABLE tt-ap-tax  NO-UNDO
                          FIELD row-id AS ROWID
                          FIELD actnum LIKE account.actnum
                          FIELD amt LIKE ap-invl.amt
                          FIELD curr-amt LIKE ap-invl.amt
                          INDEX row-id row-id.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_vend end_vend ~
begin_date end_date begin_user end_user tb_sort rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_vend end_vend ~
begin_date end_date begin_user end_user lbl_sort tb_sort rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE begin_user AS CHARACTER FORMAT "x(8)" 
     LABEL "Beginning User ID" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "x(8)" 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_user AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending User ID" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Print G/L Acount Description?" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

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

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.52.

DEFINE VARIABLE tb_sort AS LOGICAL INITIAL no 
     LABEL "Print G/L Account Description?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.43 COL 44 COLON-ALIGNED
     tran-period AT ROW 3.62 COL 44 COLON-ALIGNED
     begin_vend AT ROW 4.81 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend AT ROW 4.81 COL 69 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     begin_date AT ROW 5.76 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date"
     end_date AT ROW 5.76 COL 69 COLON-ALIGNED HELP
          "Enter Ending Invoice Date"
     begin_user AT ROW 6.71 COL 27 COLON-ALIGNED HELP
          "Enter Beginning User ID"
     end_user AT ROW 6.71 COL 69 COLON-ALIGNED HELP
          "Enter Ending User ID"
     lbl_sort AT ROW 8.62 COL 30 COLON-ALIGNED NO-LABEL
     tb_sort AT ROW 8.62 COL 62
     rd-dest AT ROW 12.19 COL 4 NO-LABEL
     lv-ornt AT ROW 12.43 COL 28 NO-LABEL
     lines-per-page AT ROW 12.43 COL 81 COLON-ALIGNED
     lv-font-no AT ROW 13.86 COL 31 COLON-ALIGNED
     lv-font-name AT ROW 14.81 COL 25 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.19 COL 27
     btn-ok AT ROW 20.76 COL 22
     btn-cancel AT ROW 20.76 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.24 COL 2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 11 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 22.05.


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
         TITLE              = "Vendor Invoices Edit/Post Register"
         HEIGHT             = 22.24
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
       begin_user:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_user:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Vendor Invoices Edit/Post Register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vendor Invoices Edit/Post Register */
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


&Scoped-define SELF-NAME begin_user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_user C-Win
ON LEAVE OF begin_user IN FRAME FRAME-A /* Beginning User ID */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
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

  IF lv-fgpost-dir THEN DO:
  RUN check-inv-date(begin_date:SCREEN-VALUE).
  if v-invalid-inv then return no-apply. 
  RUN check-inv-date(end_date:SCREEN-VALUE).
  if v-invalid-inv then return no-apply.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
    /* gdm - 11050906 */
    loop:
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
          ASSIGN v-trnum       = gl-ctrl.trnum + 1
                 gl-ctrl.trnum = v-trnum.
          FIND CURRENT gl-ctrl NO-LOCK.
          LEAVE loop.
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
           {custom/asifax.i &type= ''
                            &begin_cust="begin_date"
                            &END_cust="begin_date"
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
       WHEN 6 THEN RUN output-to-port.
  end case.

  find first ap-ctrl where ap-ctrl.company eq cocode no-lock no-error.
  if not avail ap-ctrl then return.
  IF AVAIL ap-ctrl  AND ap-ctrl.payables EQ "" THEN DO:
      MESSAGE "No AP control record found. " VIEW-AS ALERT-BOX .
      RETURN.
  END.

  IF v-postable THEN DO:
    lv-post = NO.

    MESSAGE "Post Invoices?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE lv-post.

    IF lv-post THEN do:

      SESSION:SET-WAIT-STATE("general").
      RUN post-gl.
      RUN copy-report-to-audit-dir.
      RUN clear-ap.

      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
      APPLY "close" TO THIS-PROCEDURE.
    END.
    ELSE RUN undo-trnum.
  END.

  ELSE do:
      MESSAGE "No Invoices available for posting..." VIEW-AS ALERT-BOX ERROR.
      RUN undo-trnum.
  END.
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


&Scoped-define SELF-NAME end_user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_user C-Win
ON LEAVE OF end_user IN FRAME FRAME-A /* Ending User ID */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
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


&Scoped-define SELF-NAME tb_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort C-Win
ON VALUE-CHANGED OF tb_sort IN FRAME FRAME-A /* Print G/L Account Description? */
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
  if lastkey ne -1 then do:
    run check-date.
    if v-invalid then return no-apply.
    RUN valid-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON VALUE-CHANGED OF tran-date IN FRAME FRAME-A /* Post Date */
DO:
  ll-warned = NO.
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
  {methods/nowait.i}

  DO WITH FRAME {&frame-name}:
    {custom/usrprint.i}

    IF postdate-log THEN DO:
      tran-date:SCREEN-VALUE = STRING(TODAY).
      RUN check-date.
    END.
    ELSE
      ASSIGN
       tran-date:SCREEN-VALUE   = ""
       tran-period:SCREEN-VALUE = "".

    APPLY "entry" TO tran-date.
  END.


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
  DEF VAR lv-period LIKE period.pnum NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN sys/inc/valtrndt.p (cocode,
                            DATE(tran-date:SCREEN-VALUE),
                            OUTPUT lv-period) NO-ERROR.
    v-invalid = ERROR-STATUS:ERROR.
    IF v-invalid THEN APPLY "entry" TO tran-date.
    ELSE tran-period:SCREEN-VALUE = STRING(lv-period).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-inv-date C-Win 
PROCEDURE check-inv-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-period LIKE period.pnum NO-UNDO.
  DEFINE INPUT PARAMETER ip-date AS CHAR.
  DEF VAR lv-msg AS CHAR NO-UNDO.

  DEF VAR v-month AS CHAR . 
  ASSIGN  lv-msg = ""
           v-invalid-inv = NO 
            v-month = SUBSTRING(STRING(TODAY),1,2) .

      FIND FIRST period                   
         WHERE period.company EQ cocode
         AND period.pst     LE date(ip-date)
         AND period.pend    GE date(ip-date)
          AND period.pnum   EQ INT(v-month)
       NO-LOCK NO-ERROR.

      IF NOT AVAIL period THEN
         DO:
            lv-msg = "CAN NOT POST OUT OF PERIOD, ENTER SECURITY PASSWORD OR  ENTER TO RETURN".
         END.

         ELSE IF NOT period.pstat THEN
            lv-msg = "Period for " + TRIM(STRING(ip-date)) + " is already closed, enter security password or enter to return".
         ELSE
            lv-msg = "".

         IF lv-msg NE "" THEN DO:
             IF NOT ll-secure THEN do:
                 MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
             END.
         END.

         IF lv-msg NE "" THEN DO:
            IF NOT ll-secure THEN do:  
               RUN sys/ref/d-passwd.w (1, OUTPUT ll-secure). 
               IF NOT ll-secure THEN v-invalid-inv = YES .
            END.
         END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-ap C-Win 
PROCEDURE clear-ap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  for each ap-inv
    where ap-inv.company  eq cocode
      and ap-inv.posted   eq no
      AND ap-inv.user-id  EQ USERID("nosweat")
      /*
      and xap-inv.inv-date ge v-s-date
      and xap-inv.inv-date le v-e-date 
      and xap-inv.vend-no  ge v-s-vend
      and xap-inv.vend-no  le v-e-vend
      AND CAN-FIND(FIRST ap-invl where ap-invl.i-no eq xap-inv.i-no USE-INDEX i-no)
    use-index posted no-lock
    transaction:
    */   :

      IF NOT CAN-FIND(FIRST ap-invl WHERE ap-invl.i-no = ap-inv.i-no) THEN
         DELETE ap-inv.
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

  ASSIGN targetfile = lv-audit-dir + "\AP\VU3\Run#"
                    + STRING(v-trnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AP"
         dirname3 = lv-audit-dir + "\AP\VU3".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-manual-check-proc C-Win 
PROCEDURE create-manual-check-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR X AS INT NO-UNDO.
  DEF VAR v-check-no AS INT NO-UNDO.

  v-check-no = INTEGER(ap-inv.receiver-no).

  IF CAN-FIND(FIRST ap-pay WHERE
     ap-pay.company = cocode AND
     ap-pay.check-no = v-check-no AND
     (ap-pay.check-act = lv-bank-acct OR 
      ap-pay.bank-code = v-bank-code) AND
      ap-pay.posted) THEN
      DO:
         MESSAGE "Check Number " STRING(v-check-no) " has already been posted." SKIP
                 "Cannot Create Manual Check."
            VIEW-AS ALERT-BOX ERROR.
         LEAVE.
      END.

  FOR EACH bf-chk NO-LOCK BY bf-chk.c-no DESCENDING:
      X = bf-chk.c-no.
      LEAVE.
  END.

  /* Code placed here will execute AFTER standard behavior.    */

  CREATE ap-chk.
  ASSIGN ap-chk.bank-code = v-bank-code
         ap-chk.check-no = v-check-no
         ap-chk.man-check = YES   
         ap-chk.check-date = ap-inv.inv-date
         ap-chk.c-no = X + 1
         ap-chk.company = cocode
         ap-chk.vend-no = ap-inv.vend-no
         ap-chk.check-amt = ap-inv.due.

  FIND FIRST bank WHERE
       bank.company = cocode AND
       bank.bank-code = ap-chk.bank-code
       NO-ERROR.

  IF AVAIL bank THEN DO:
     IF ap-chk.check-no > bank.last-chk THEN
        bank.last-chk = ap-chk.check-no.

     ap-chk.check-act = bank.actnum.

     RELEASE bank.
  END.

  CREATE ap-sel.
  ASSIGN ap-sel.company = cocode
         ap-sel.vend-no = ap-chk.vend-no
         ap-sel.check-no = ap-chk.check-no
         ap-sel.bank-code = ap-chk.bank-code
         ap-sel.man-check = YES
         ap-sel.pre-date = ap-chk.check-date
         ap-sel.actnum = IF lv-bank-acct <> "" THEN lv-bank-acct
                         ELSE "NO Account"
         ap-sel.inv-no = ap-inv.inv-no
         ap-sel.due-date = ap-inv.due-date
         ap-sel.inv-bal = ap-inv.due
         ap-sel.amt-paid = ap-inv.due.

  IF ap-sel.pre-date - ap-inv.inv-date LE ap-inv.disc-days THEN
     ap-sel.disc-amt = round(ap-inv.disc-% * ap-inv.net / 100,2).

  RELEASE ap-sel.
  RELEASE ap-chk.

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
  DISPLAY tran-date tran-period begin_vend end_vend begin_date end_date 
          begin_user end_user lbl_sort tb_sort rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_vend end_vend begin_date end_date 
         begin_user end_user tb_sort rd-dest lv-ornt lines-per-page lv-font-no 
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

do:
  find first ap-ctrl where ap-ctrl.company eq cocode no-lock no-error.
  if not avail ap-ctrl then return.
  assign xap-acct = ap-ctrl.payables
         xap-stax = ap-ctrl.stax
         v-frt-acct = ap-ctrl.freight
         v-cash-acct = ap-ctrl.cash-act.
  release ap-ctrl.

  FIND FIRST bank WHERE
       bank.company = g_company AND
       bank.actnum = v-cash-acct
       NO-LOCK NO-ERROR.

  IF AVAIL bank THEN
  DO:
     ASSIGN
        v-bank-code = bank.bank-code
        lv-bank-acct = bank.actnum.
     RELEASE bank.
  END.
end.

END_date = TODAY.

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
    IF NOT RESULT THEN v-postable = NO.
  */
    run custom/prntproc.p (list-name,(lv-font-no),lv-ornt). /* open file-name, title */ 
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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR g2 AS dec NO-UNDO.
DEF VAR t1 AS DEC NO-UNDO.
DEF VAR v-upd AS LOG NO-UNDO.
DEF var v-po-no like fg-rcpth.po-no NO-UNDO.
def var total-msf like ap-invl.amt-msf NO-UNDO.
def var v-qty like ap-invl.qty.
def var v-qty1 like v-qty.
def var v-qty2 like v-qty.
def var v-qty3 like v-qty.
def var v-cost like fg-rdtlh.cost.
DEF VAR v-wid AS DEC NO-UNDO.
DEF VAR v-len AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.
DEF VAR v-bwt AS DEC NO-UNDO.
DEF VAR ll-rcpth AS LOG NO-UNDO.

  /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
  postit:
do transaction on error undo postit:
  g2 = 0.

  for each tt-report
      where can-find(first ap-inv where recid(ap-inv) eq tt-report.rec-id
                                    and ap-inv.posted eq no)
      BREAK BY tt-report.actnum:

    find first ap-inv
        where recid(ap-inv) eq tt-report.rec-id
        exclusive-lock no-error no-wait.

    if not avail ap-inv then do:
      message "Unable to Post due to Invoice Record being Locked.  " +
              "Please Try again Later".
      pause.
      hide message no-pause.
      undo postit, leave postit.
    end.

    ap-inv.period = tran-period.

    find first vend
        where vend.company eq cocode
          and vend.vend-no eq ap-inv.vend-no
        use-index vend no-lock.

    for each ap-invl where ap-invl.i-no eq ap-inv.i-no,

        first tt-ap-invl where tt-ap-invl.row-id eq rowid(ap-invl):

      create gltrans.
      assign
       t1              = t1 + ap-invl.amt
       g2              = g2 + ap-invl.amt
       total-msf       = total-msf + ap-invl.amt-msf
       gltrans.company = cocode
       gltrans.actnum  = tt-ap-invl.actnum
       gltrans.jrnl    = "ACPAY"
       gltrans.tr-dscr = vend.name  + "  " + string(ap-inv.inv-date)
       gltrans.tr-date = tran-date
       gltrans.tr-amt  = tt-ap-invl.amt
       gltrans.trnum   = v-trnum
       gltrans.period  = tran-period
       ap-invl.posted  = yes.      

      RELEASE gltrans.

      find first po-ordl
          where po-ordl.company eq cocode
            and po-ordl.po-no   eq (if ap-invl.po-no eq 0 then ap-inv.po-no
                                                          else ap-invl.po-no)
            and po-ordl.line    eq {ap/invlline.i -1}
          use-index po-no no-error.

      if avail po-ordl then do:
        find first reftable
            {ap/apreftbw.i po-ordl.po-no}
              and reftable.code2 eq string(ap-invl.i-no,"9999999999")
            no-lock no-error.
        if not avail reftable then do:
          {ap/addreftb.i po-ordl.po-no}
          RELEASE reftable.
        end.

        po-ordl.t-inv-qty = po-ordl.t-inv-qty + ap-invl.qty.

        RELEASE item.
        IF po-ordl.item-type THEN
        FIND FIRST item
            WHERE item.company EQ po-ordl.company
              AND item.i-no    EQ po-ordl.i-no
            NO-LOCK NO-ERROR.

        IF AVAIL item           AND
           item.i-code EQ "R"   AND
           INDEX("MOXY789@",ITEM.mat-type) GT 0 AND
           item.stocked EQ NO   THEN DO:

          ll-rcpth = NO.

          FOR EACH rm-rcpth NO-LOCK
              WHERE rm-rcpth.company   EQ po-ordl.company
                AND rm-rcpth.po-no     EQ STRING(po-ordl.po-no)
                AND rm-rcpth.i-no      EQ po-ordl.i-no
                AND rm-rcpth.rita-code EQ "R",
              EACH rm-rdtlh NO-LOCK
              WHERE rm-rdtlh.r-no    EQ rm-rcpth.r-no
                AND rm-rdtlh.job-no  EQ po-ordl.job-no
                AND rm-rdtlh.job-no2 EQ po-ordl.job-no2
                AND rm-rdtlh.s-num   EQ po-ordl.s-num
              BREAK BY rm-rcpth.company:

            IF FIRST(rm-rcpth.company) THEN
              ASSIGN
               po-ordl.t-rec-qty = 0
               ll-rcpth          = YES.

            v-qty = rm-rdtlh.qty.

            IF rm-rcpth.pur-uom NE po-ordl.cons-uom THEN
              RUN sys/ref/convquom.p (rm-rcpth.pur-uom, po-ordl.cons-uom,
                                    item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                    v-qty, OUTPUT v-qty).

            IF po-ordl.cons-uom EQ "EA" THEN DO:
              {sys/inc/roundup.i v-qty}
            END.

            po-ordl.t-rec-qty = po-ordl.t-rec-qty + v-qty.
          END.

          IF NOT ll-rcpth THEN DO:
            v-dep = item.s-dep.          
            {po/pol-dims.i}

            v-qty = ap-invl.qty.

            IF po-ordl.pr-qty-uom NE po-ordl.cons-uom THEN
              RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, po-ordl.cons-uom,
                                      v-bwt, v-len, v-wid, v-dep,
                                      v-qty, OUTPUT v-qty).

            po-ordl.t-rec-qty = po-ordl.t-rec-qty + v-qty.
          END.

          RUN rm/polclose.p (ROWID(po-ordl), ap-invl.qty, po-ordl.pr-qty-uom).
        END.

        RUN po/closechk.p (ROWID(po-ordl)).

        /* Ensure receipts = payables */
        if not po-ordl.item-type and v-fgpostgl then do:
          release prod.
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq po-ordl.i-no
              no-error.

          if avail itemfg then
          find first prodl
              where prodl.company eq cocode
                and prodl.procat  eq itemfg.procat
                and can-find(first prod
                             where prod.company eq cocode
                               and prod.prolin  eq prodl.prolin)
              no-lock no-error.

          if avail prodl then
          find first prod
              where prod.company eq cocode
                and prod.prolin  eq prodl.prolin
              no-lock no-error.

          if avail itemfg then do:
            run sys/ref/convquom.p (po-ordl.pr-qty-uom, "EA", 0, 0, 0, 0,
                                    ap-invl.qty, output v-qty1).

            assign
             v-po-no = trim(string(po-ordl.po-no,">>>>>>>>>>"))
             v-qty   = 0
             v-cost  = ap-invl.amt / (v-qty1 / 1000).

            for each fg-rcpth
                where fg-rcpth.company   eq cocode
                  and fg-rcpth.i-no      eq po-ordl.i-no
                  and fg-rcpth.po-no     eq v-po-no
                  and fg-rcpth.rita-code eq "R"
                  and ((fg-rcpth.b-no    eq ap-invl.i-no and v-fgpostgl) or
                       (fg-rcpth.b-no    eq 0        and not v-fgpostgl))
                use-index item-po,

                each fg-rdtlh where fg-rdtlh.r-no eq fg-rcpth.r-no

                break by fg-rcpth.trans-date
                      BY fg-rdtlh.trans-time
                      by fg-rcpth.r-no
                      by recid(fg-rdtlh):

              assign
               v-qty         = v-qty + fg-rdtlh.qty
               fg-rdtlh.cost = v-cost
               fg-rcpth.b-no = ap-invl.i-no.

              if last(fg-rcpth.trans-date) and
                 v-qty ne v-qty1           then do:

/*                find first fg-bin                           */
/*                    where fg-bin.company eq cocode          */
/*                      and fg-bin.i-no    eq fg-rcpth.i-no   */
/*                      and fg-bin.loc     eq fg-rdtlh.loc    */
/*                      and fg-bin.loc-bin eq fg-rdtlh.loc-bin*/
/*                      and fg-bin.tag     eq fg-rdtlh.tag    */
/*                      and fg-bin.job-no  eq fg-rcpth.job-no */
/*                      and fg-bin.job-no2 eq fg-rcpth.job-no2*/
/*                    no-error.                               */
/*                                                            */
/*                if not avail fg-bin then do:                */
/*                  create fg-bin.                            */
/*                  assign                                    */
/*                   fg-bin.company      = fg-rdtlh.company   */
/*                   fg-bin.job-no       = fg-rcpth.job-no    */
/*                   fg-bin.job-no2      = fg-rcpth.job-no2   */
/*                   fg-bin.loc          = fg-rdtlh.loc       */
/*                   fg-bin.loc-bin      = fg-rdtlh.loc-bin   */
/*                   fg-bin.tag          = fg-rdtlh.tag       */
/*                   fg-bin.i-no         = fg-rcpth.i-no      */
/*                   fg-bin.case-count   = itemfg.case-count  */
/*                   fg-bin.cases-unit   = 1                  */
/*                   fg-bin.aging-date   = fg-rcpth.trans-date*/
/*                   fg-bin.pur-uom      = "M"                */
/*                   fg-bin.std-tot-cost = fg-rdtlh.cost      */
/*                   fg-bin.std-mat-cost = fg-bin.std-tot-cost*/
/*                   fg-bin.std-lab-cost = 0                  */
/*                   fg-bin.std-var-cost = 0                  */
/*                   fg-bin.std-fix-cost = 0.                 */
/*                end.                                        */

                assign
                 v-qty1         = v-qty1 - v-qty
                 fg-rdtlh.qty   = fg-rdtlh.qty + v-qty1
                 fg-rdtlh.cases = trunc(fg-rdtlh.qty / fg-rdtlh.qty-case,0)
/*                 fg-bin.qty     = fg-bin.qty + v-qty1*/
                 itemfg.q-onh   = itemfg.q-onh + v-qty1.

                RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT fg-rdtlh.loc).
                FIND FIRST itemfg-loc 
                    WHERE itemfg-loc.company EQ itemfg.company
                      AND itemfg-loc.i-no    EQ itemfg.i-no
                      AND itemfg-loc.loc     EQ fg-rdtlh.loc
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL itemfg-loc THEN
                  itemfg-loc.q-onh = itemfg-loc.q-onh + v-qty1.
                FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
              end.
            end.

            for each fg-rcpth
                where fg-rcpth.company   eq cocode
                  and fg-rcpth.i-no      eq po-ordl.i-no
                  and fg-rcpth.po-no     eq v-po-no
                  and fg-rcpth.rita-code eq "R"
                use-index item-po no-lock,

                each fg-rdtlh where fg-rdtlh.r-no eq fg-rcpth.r-no

                break by fg-rcpth.job-no
                      by fg-rcpth.job-no2
                      by fg-rdtlh.loc
                      by fg-rdtlh.loc-bin
                      by fg-rdtlh.tag:

              if first-of(fg-rdtlh.tag) then
                assign
                 v-qty  = 0
                 v-cost = 0.

              assign
               v-qty  = v-qty + fg-rdtlh.qty
               v-cost = v-cost + (fg-rdtlh.qty / 1000 * fg-rdtlh.cost).

/*              if last-of(fg-rdtlh.tag) then do:                               */
/*                find first fg-bin                                             */
/*                    where fg-bin.company eq cocode                            */
/*                      and fg-bin.i-no    eq fg-rcpth.i-no                     */
/*                      and fg-bin.loc     eq fg-rdtlh.loc                      */
/*                      and fg-bin.loc-bin eq fg-rdtlh.loc-bin                  */
/*                      and fg-bin.tag     eq fg-rdtlh.tag                      */
/*                      and fg-bin.job-no  eq fg-rcpth.job-no                   */
/*                      and fg-bin.job-no2 eq fg-rcpth.job-no2                  */
/*                    no-error.                                                 */
/*                                                                              */
/*                if not avail fg-bin then do:                                  */
/*                  create fg-bin.                                              */
/*                  assign                                                      */
/*                   fg-bin.company      = fg-rdtlh.company                     */
/*                   fg-bin.job-no       = fg-rcpth.job-no                      */
/*                   fg-bin.job-no2      = fg-rcpth.job-no2                     */
/*                   fg-bin.loc          = fg-rdtlh.loc                         */
/*                   fg-bin.loc-bin      = fg-rdtlh.loc-bin                     */
/*                   fg-bin.tag          = fg-rdtlh.tag                         */
/*                   fg-bin.i-no         = fg-rcpth.i-no                        */
/*                   fg-bin.case-count   = itemfg.case-count                    */
/*                   fg-bin.cases-unit   = 1                                    */
/*                   fg-bin.aging-date   = fg-rcpth.trans-date                  */
/*                   fg-bin.pur-uom      = "M"                                  */
/*                   fg-bin.std-tot-cost = fg-rdtlh.cost                        */
/*                   fg-bin.std-mat-cost = fg-bin.std-tot-cost                  */
/*                   fg-bin.std-lab-cost = 0                                    */
/*                   fg-bin.std-var-cost = 0                                    */
/*                   fg-bin.std-fix-cost = 0.                                   */
/*                end.                                                          */
/*                                                                              */
/*                v-cost = v-cost / (v-qty / 1000).                             */
/*                                                                              */
/*                if fg-bin.pur-uom eq "M" then                                 */
/*                  fg-bin.std-tot-cost = v-cost.                               */
/*                else                                                          */
/*                  run sys/ref/convcuom.p ("M", fg-bin.pur-uom, 0, 0, 0, 0,    */
/*                                          v-cost, output fg-bin.std-tot-cost).*/
/*                                                                              */
/*                assign                                                        */
/*                 fg-bin.std-mat-cost = fg-bin.std-tot-cost                    */
/*                 fg-bin.std-lab-cost = 0                                      */
/*                 fg-bin.std-var-cost = 0                                      */
/*                 fg-bin.std-fix-cost = 0.                                     */
/*              end.                                                            */
            end.
          end.

          run fg/updfgcst.p (po-ordl.i-no).
        end.
      end.

      IF ap-invl.actnum NE "" THEN
      FIND FIRST bank
          WHERE bank.company EQ cocode
            AND bank.actnum  EQ ap-invl.actnum
          EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL bank THEN bank.bal = bank.bal + ap-invl.amt.
      RELEASE bank.
    end.  /* each line */

    find first vend
        where vend.company eq cocode
          and vend.vend-no eq ap-inv.vend-no
        use-index vend exclusive-lock.

    assign
     vend.purch[tran-period]   = vend.purch[tran-period] + t1
     vend.n-purch[tran-period] = vend.n-purch[tran-period] + 1
     vend.purch[13]        = vend.purch[13] + t1
     vend.n-purch[13]      = vend.n-purch[13] + 1
     vend.ptd-msf[tran-period] = vend.ptd-msf[tran-period] + total-msf
     vend.ytd-msf          = vend.ytd-msf + total-msf
     vend.acc-bal          = vend.acc-bal + t1 + ap-inv.tax-amt.

    if vend.acc-bal ge vend.hibal then
      assign
       vend.hibal      = vend.acc-bal
       vend.hibal-date = ap-inv.inv-date.

    FIND CURRENT vend NO-LOCK NO-ERROR.
    FIND CURRENT po-ordl NO-LOCK NO-ERROR.
    FIND CURRENT itemfg NO-LOCK NO-ERROR.
/*    FIND CURRENT fg-bin NO-LOCK NO-ERROR.*/
    FIND CURRENT fg-rdtlh NO-LOCK NO-ERROR.
    FIND CURRENT fg-rcpth NO-LOCK NO-ERROR.

    create ap-ledger.
    assign
     ap-ledger.company  = cocode
     ap-ledger.vend-no  = ap-inv.vend-no
     ap-ledger.amt      = ap-inv.net
     ap-ledger.refnum   = "INV# " + ap-inv.inv-no
     ap-ledger.ref-date = ap-inv.inv-date
     ap-ledger.trnum    = v-trnum
     ap-ledger.period   = tran-period
     ap-ledger.tr-date  = tran-date.

    RELEASE ap-ledger.

    assign
     t1            = 0
     ap-inv.posted = yes.

    IF apautocheck-log AND ap-inv.receiver-no NE "0" THEN
       RUN create-manual-check-proc.

    ACCUM ap-inv.net (TOTAL BY tt-report.actnum).

    ACCUM tt-report.curr-amt - (ap-inv.net + ap-inv.freight) (TOTAL BY tt-report.actnum).

    ACCUM ap-inv.freight * tt-report.ex-rate (TOTAL).

    IF LAST-OF(tt-report.actnum) AND
       tt-report.actnum NE ""    AND
       (ACCUM TOTAL BY tt-report.actnum tt-report.curr-amt - (ap-inv.net + ap-inv.freight))
                        NE 0    THEN DO:
      CREATE gltrans.
      ASSIGN
       gltrans.company = cocode
       gltrans.actnum  = tt-report.actnum
       gltrans.jrnl    = "ACPAY"
       gltrans.tr-dscr = "ACCOUNTS PAYABLE CURRENCY GAIN/LOSS"
       gltrans.tr-date = tran-date
       gltrans.tr-amt  = (ACCUM TOTAL BY tt-report.actnum tt-report.curr-amt - (ap-inv.net + ap-inv.freight)) * -1
       gltrans.period  = tran-period
       gltrans.trnum   = v-trnum.
      RELEASE gltrans.
    END.
  end. /* for each ap-inv */

  g2 = g2 + lv-frt-total.

  if lv-frt-total ne 0 then do:
    create gltrans.
    assign
     gltrans.company = cocode
     gltrans.actnum  = v-frt-acct
     gltrans.jrnl    = "ACPAY"
     gltrans.tr-dscr = "ACCOUNTS PAYABLE FREIGHT"
     gltrans.tr-date = tran-date
     gltrans.tr-amt  = (ACCUM TOTAL ap-inv.freight * tt-report.ex-rate)
     gltrans.period  = tran-period
     gltrans.trnum   = v-trnum.
    RELEASE gltrans.
  end.

  FOR EACH tt-ap-tax BREAK BY tt-ap-tax.actnum:
    ACCUM tt-ap-tax.curr-amt (TOTAL BY tt-ap-tax.actnum).

    g2 = g2 + tt-ap-tax.amt.

    IF LAST-OF(tt-ap-tax.actnum) THEN DO:
      CREATE gltrans.
      ASSIGN
       gltrans.company = cocode
       gltrans.actnum  = tt-ap-tax.actnum
       gltrans.jrnl    = "ACPAY"
       gltrans.tr-dscr = "ACCOUNTS PAYABLE TAX"
       gltrans.tr-date = tran-date
       gltrans.tr-amt  = (ACCUM TOTAL BY tt-ap-tax.actnum tt-ap-tax.curr-amt)
       gltrans.period  = tran-period
       gltrans.trnum   = v-trnum.
      RELEASE gltrans.
    END.
  END.

  create gltrans.
  assign
   gltrans.company = cocode
   gltrans.actnum  = xap-acct
   gltrans.jrnl    = "ACPAY"
   gltrans.tr-dscr = "ACCOUNTS PAYABLE INVOICE"
   gltrans.tr-date = tran-date
   gltrans.tr-amt  = - g2
   gltrans.period  = tran-period
   gltrans.trnum   = v-trnum.
  RELEASE gltrans.


end. /* postit: transaction */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- ap/ap-inreg.p 10/94 gb */
/* Invoicing  - Edit Register & Post Invoicing TRANSACTIONs                   */
/* -------------------------------------------------------------------------- */
def var g1 as dec format "->>,>>>,>>9.99" NO-UNDO.
def var g2 like g1 NO-UNDO.
def var t1 like g1 NO-UNDO.
def var t2 like g1 NO-UNDO.
def var t3 like g1 NO-UNDO.
def var v1 like g1 NO-UNDO.
def var v2 like g1 NO-UNDO.

def var total-msf like ap-invl.amt-msf NO-UNDO.
def var v-s-date like inv-head.inv-date format "99/99/9999" init 01/01/0001 NO-UNDO.
def var v-e-date like v-s-date init today NO-UNDO.
def var v-prt-dscr as log init no no-undo.
def var v-s-vend like vend.vend-no initial "First" no-undo.
def var v-e-vend like vend.vend-no initial "Last" no-undo.
DEF BUFFER xap-inv FOR ap-inv.
def var v-loop as int init 1 no-undo.
DEF VAR v-upd AS LOG NO-UNDO.
DEF var v-po-no like fg-rcpth.po-no NO-UNDO.
def var v-dscr          like account.dscr.
def var v-disp-actnum   like account.actnum.
def var v-disp-amt      as   dec format ">>,>>>,>>9.99cr".
  DEF VAR ld-gl-amt AS DEC NO-UNDO.

{sys/form/r-top3w.f}
time_stamp = string(time,"hh:mmam").

form header
     "VENDOR#  Name                              INVOICE #       INV.DATE    DUE DATE         AMOUNT " 
     "    G/L DISTRIBUTION" skip fill("_",130) format "x(130)"
    with no-labels no-box no-underline frame f-top page-top width 132 STREAM-IO.

form v-disp-actnum label "G/L ACCOUNT NUMBER"
     v-dscr        label "DESCRIPTION"
     udate         label "DATE"   
     v-disp-amt    label "AMOUNT" skip

    with down STREAM-IO width 130 frame gldetail.


SESSION:SET-WAIT-STATE ("general").

tmpstore = fill("_",125).

ASSIGN v-s-vend   = begin_vend
       v-e-vend   = END_vend
       v-s-date   = begin_date
       v-e-date   = END_date
       v-prt-dscr = tb_sort.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

assign
 g1 = 0
 g2 = 0
 t1 = 0
 t2 = 0
 t3 = 0
 v1 = 0
 v2 = 0
 total-msf = 0.

assign
 str-tit  = coname + " - " + loname
 str-tit2 = "VENDOR INVOICES  -  EDIT REGISTER " + string(v-trnum)
 str-tit3 = "Period " + string(tran-period,"99") +
            " - TRANSACTION Date Entered: " + string(tran-date)
 x = (112 - length(str-tit)) / 2
 str-tit  = fill(" ",x) + str-tit
 x = (114 - length(str-tit2)) / 2
 str-tit2 = fill(" ",x) + str-tit2
 x = (132 - length(str-tit3)) / 2
 str-tit3 = fill(" ",x) + str-tit3.

EMPTY TEMP-TABLE tt-report.

EMPTY TEMP-TABLE tt-ap-invl.

EMPTY TEMP-TABLE tt-ap-tax.

display "" with frame r-top.
display "" with frame f-top.

{ap/r-apve&p.i}

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

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
   IF gl-ctrl.trnum = v-trnum then gl-ctrl.trnum = v-trnum - 1.
   RELEASE gl-ctrl.
   LEAVE.
 END. /* IF AVAIL gl-ctrl */
END. /* REPEAT */
/* gdm - 11050906 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-date C-Win 
PROCEDURE valid-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF NOT ll-warned THEN DO:
      ll = NO.

      FOR EACH period NO-LOCK
          WHERE period.company EQ cocode
            AND period.pst     LE TODAY
            AND period.pend    GE TODAY
          BY period.pst:

        IF period.pst  GT DATE(tran-date:SCREEN-VALUE) OR
           period.pend LT DATE(tran-date:SCREEN-VALUE) THEN DO:
          ll = YES.
          MESSAGE TRIM(tran-date:LABEL) + " is not in current period, " +
                  "would you like to re-enter..."
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll.
        END.

        IF ll THEN DO:
          APPLY "entry" TO tran-date.
          RETURN ERROR.
        END.

        LEAVE.
      END.

      ll-warned = YES.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

