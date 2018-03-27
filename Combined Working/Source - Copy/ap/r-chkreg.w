&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap\r-chkreg.w

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
DEF VAR list-name AS cha NO-UNDO.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR lv-comp-curr AS cha NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
IF AVAIL company THEN lv-comp-curr = company.curr-code.

DEF NEW SHARED VAR v-trnum AS INT NO-UNDO.
DEF VAR v-unline AS CHAR FORMAT "x(80)" INIT
  "--------------- ------------------------- ------- ----------- ---"  NO-UNDO.
DEF VAR time_stamp AS ch NO-UNDO.
DEF VAR v-postable AS LOG INIT NO NO-UNDO.
DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR save_id AS RECID NO-UNDO.
DEF VAR pct-paid AS DEC NO-UNDO.
DEF VAR gtot0 LIKE ap-sel.inv-bal NO-UNDO.
DEF VAR gtot1 LIKE ap-sel.disc-amt NO-UNDO.
DEF VAR gtot2 LIKE ap-sel.amt-paid NO-UNDO.
DEF VAR ndisc AS DEC NO-UNDO.
DEF VAR wckdate AS DATE NO-UNDO.
DEF VAR bal AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR bal1 AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR tot-of-inv AS de FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR v-fst-chk AS LOG NO-UNDO.
DEF VAR v-lst-chk AS LOG NO-UNDO.
DEF VAR tot0 AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR tot1 AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR tot2 AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR c1  AS DEC NO-UNDO.
DEF VAR op AS LOG FORMAT "Yes/No" NO-UNDO.
DEF VAR ctr AS INT LABEL "NUMBER OF CHECKS WRITTEN " NO-UNDO.
DEF VAR credit AS DEC NO-UNDO.
DEF VAR tdisc AS DEC NO-UNDO.
DEF VAR wcash AS CHAR FORMAT "x(25)" LABEL "CASH ACCOUNT" NO-UNDO.
DEF VAR v-frt-acct LIKE ap-ctrl.freight NO-UNDO.
DEF VAR ap-acct LIKE ap-ctrl.payables NO-UNDO.
DEF VAR v-loop-count AS INT NO-UNDO INITIAL 10 .
DEF VAR post-manual AS LOG FORMAT "Manual/Automatic"
  LABEL "Post Manual or Automatic (M/A)?" INITIAL NO NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.

/* gdm - 05210901 */
DEF VAR v-fileFormat AS CHAR NO-UNDO.
DEF STREAM checkFile.
DEF VAR v-amt-paid LIKE ap-sel.amt-paid NO-UNDO.

DEF VAR lLocked AS LOG NO-UNDO.
DEF VAR cUsr AS INT NO-UNDO.
DEF VAR cName AS CHAR NO-UNDO.
DEF VAR cDevice AS CHAR NO-UNDO.

DEF STREAM excel.

DEF TEMP-TABLE tt-post NO-UNDO FIELD row-id    AS ROWID
                               FIELD ex-rate   LIKE currency.ex-rate INIT 1
                               FIELD curr-bal  LIKE ap-sel.inv-bal
                               FIELD curr-disc LIKE ap-sel.disc-amt
                               FIELD curr-paid LIKE ap-sel.amt-paid
                               FIELD actnum    LIKE account.actnum.

FIND FIRST ap-ctrl WHERE ap-ctrl.company = cocode
      NO-LOCK NO-WAIT NO-ERROR.
IF AVAIL ap-ctrl THEN
    ASSIGN wcash   = ap-ctrl.cash-act
           ap-acct = ap-ctrl.payables
           v-frt-acct = ap-ctrl.freight.

RELEASE ap-ctrl.


DO TRANSACTION:
   {sys/inc/postdate.i}
   {sys/inc/aplockbx.i}   
   FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ cocode AND
        sys-ctrl.name    EQ "AUDITDIR"
        NO-LOCK NO-ERROR.

   IF NOT AVAIL sys-ctrl THEN DO:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company = cocode
         sys-ctrl.name    = "AUDITDIR"
         sys-ctrl.descrip = "Audit Trails directory"
         sys-ctrl.char-fld = ".\AUDIT TRAILS".
   END.

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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date rd_sort tb_prt-acc ~
tb_void fi_CheckFile tb_APcheckFile lv-ornt rd-dest lines-per-page ~
lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period lbl_sort rd_sort ~
tb_prt-acc tb_void fi_CheckFile tb_APcheckFile lv-ornt rd-dest ~
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

DEFINE VARIABLE fi_CheckFile AS CHARACTER FORMAT "X(50)" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1
     BGCOLOR 15 FGCOLOR 9 .

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-chkreg.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Post Automatic or Manual Checks?" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

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
     SIZE 20 BY 3.81 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Automatic" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Manual", "Manual",
"Automatic", "Automatic"
     SIZE 34 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.38.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.29.

DEFINE VARIABLE tb_APcheckFile AS LOGICAL INITIAL NO 
     LABEL "Create AP Check File?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_prt-acc AS LOGICAL INITIAL NO 
     LABEL "Print Invoice & GL Account Detail?" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL NO 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_void AS LOGICAL INITIAL NO 
     LABEL "Void Skipped Checks?" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.57 COL 41 COLON-ALIGNED
     tran-period AT ROW 3.76 COL 41 COLON-ALIGNED
     lbl_sort AT ROW 5.19 COL 13 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 5.19 COL 51 NO-LABEL
     tb_prt-acc AT ROW 6.48 COL 20
     tb_void AT ROW 7.67 COL 20
     fi_CheckFile AT ROW 8.86 COL 45.4 COLON-ALIGNED HELP
          "Enter File Name" NO-LABEL WIDGET-ID 2
     tb_APcheckFile AT ROW 8.95 COL 20.2 WIDGET-ID 4
     lv-ornt AT ROW 11.05 COL 29 NO-LABEL
     rd-dest AT ROW 11.95 COL 8 NO-LABEL
     lines-per-page AT ROW 12.19 COL 82 COLON-ALIGNED
     lv-font-no AT ROW 12.48 COL 32 COLON-ALIGNED
     lv-font-name AT ROW 13.43 COL 26 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 14.57 COL 28
     tb_excel AT ROW 15.57 COL 48.2 RIGHT-ALIGNED
     tb_runExcel AT ROW 15.57 COL 69.2 RIGHT-ALIGNED
     fi_file AT ROW 16.38 COL 26 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 18.14 COL 24
     btn-cancel AT ROW 18.38 COL 58
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11 COL 3
     RECT-6 AT ROW 10.52 COL 1
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
         TITLE              = "A/P Checks Register"
         HEIGHT             = 20.19
         WIDTH              = 95.4
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
       fi_CheckFile:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

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
ON END-ERROR OF C-Win /* A/P Checks Register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* A/P Checks Register */
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
   APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEF VAR lv-post AS LOG NO-UNDO.
  DEF VAR lv-bank-file AS cha NO-UNDO.

  /* gdm - 05210901 */
  ASSIGN  fi_CheckFile:SCREEN-VALUE = fi_CheckFile.

  RUN check-date.
  IF v-invalid THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  post-manual = rd_sort EQ "Manual".

  DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl NO-LOCK
         WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      ASSIGN lLocked = NO
             cUsr = 0
             cName = ""
             cDevice = "".
      IF AVAIL gl-ctrl THEN DO:
          RUN checkLock(INPUT "gl-ctrl",
                        INPUT ROWID(gl-ctrl),
                        OUTPUT lLocked,
                        OUTPUT cUsr,
                        OUTPUT cName,
                        OUTPUT cDevice).
        IF lLocked THEN DO:
            /* to dispaly regular progress lock message */
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
               WHERE gl-ctrl.company EQ cocode .
            RELEASE gl-ctrl.
/*             MESSAGE "The General Ledger Control Record is locked by " cName SKIP */
/*                     "Press OK to try again."                                     */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                                 */
        END.

      END. /* avail gl-ctrl */

      RELEASE gl-ctrl.

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

  RUN run-report.

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
  END CASE.

  FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
  IF NOT AVAIL ap-ctrl THEN RETURN.
  IF AVAIL ap-ctrl  AND ap-ctrl.payables EQ "" THEN DO:
      MESSAGE "No AP control record found. " VIEW-AS ALERT-BOX .
      RETURN.
  END.

  IF v-postable THEN DO:    
    lv-post = NO.

    MESSAGE "Post to Vendor & G/L Files?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

    IF lv-post THEN DO:  
      IF aplockbx-log THEN DO:
         RUN create-bank-file (OUTPUT lv-bank-file).
         MESSAGE "Check Register/Lock Box file is created into " 
              aplockbx-path + lv-bank-file
            VIEW-AS ALERT-BOX INFO.
      END.
      RUN post-gl.
      RUN copy-report-to-audit-dir.
      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.

    END.
    ELSE RUN undo-trnum.
  END.

  ELSE DO:
      MESSAGE "No A/P Checks available for posting..." VIEW-AS ALERT-BOX ERROR.
      RUN undo-trnum.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_CheckFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_CheckFile C-Win
ON LEAVE OF fi_CheckFile IN FRAME FRAME-A
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
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
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_APcheckFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_APcheckFile C-Win
ON VALUE-CHANGED OF tb_APcheckFile IN FRAME FRAME-A /* Create AP Check File? */
DO:
    ASSIGN {&self-name}.

    IF TRIM(v-fileFormat) NE "" AND 
       {&self-name}
      THEN ASSIGN fi_CheckFile:HIDDEN = NO.
      ELSE ASSIGN fi_CheckFile:HIDDEN = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-acc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-acc C-Win
ON VALUE-CHANGED OF tb_prt-acc IN FRAME FRAME-A /* Print Invoice  GL Account Detail? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_void
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_void C-Win
ON VALUE-CHANGED OF tb_void IN FRAME FRAME-A /* Void Skipped Checks? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Post Date */
DO:
  ASSIGN {&self-name}.

  IF LASTKEY NE -1 THEN DO:
    RUN check-date.
    IF v-invalid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
DO:
  ASSIGN {&self-name}.
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

  tran-date = TODAY.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}

    IF postdate-log THEN DO:
      tran-date:SCREEN-VALUE = STRING(TODAY).
      RUN check-date.
    END.

    ELSE
      ASSIGN
       tran-date:SCREEN-VALUE   = ""
       tran-period:SCREEN-VALUE = "".

    ASSIGN 
          tb_APcheckFile = IF tb_APcheckFile:SCREEN-VALUE EQ "YES" 
                             THEN YES ELSE NO.         

    APPLY "entry" TO tran-date.
  END.

  /* gdm - 05210901 */
  ASSIGN tb_APcheckFile:HIDDEN IN FRAME {&FRAME-NAME} = YES
         fi_CheckFile:HIDDEN IN FRAME {&FRAME-NAME}  = YES.

  FIND FIRST sys-ctrl NO-LOCK 
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "APCheckFile" NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN 
    DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "APCheckFile"
        sys-ctrl.descrip = "Download Text format when printing AP Checks?"
        sys-ctrl.log-fld = YES.
  END.

  FIND FIRST sys-ctrl-shipto NO-LOCK
    WHERE sys-ctrl-shipto.company      EQ cocode
      AND sys-ctrl-shipto.NAME         EQ "APCheckFile"
      AND sys-ctrl-shipto.cust-vend    EQ YES 
      AND sys-ctrl-shipto.cust-vend-no EQ "" NO-ERROR.
  IF NOT AVAIL sys-ctrl-shipto THEN 
    DO TRANSACTION:
      CREATE sys-ctrl-shipto.
      ASSIGN
        sys-ctrl-shipto.company      = cocode       
        sys-ctrl-shipto.NAME         = "APCheckFile"
        sys-ctrl-shipto.cust-vend    = YES          
        sys-ctrl-shipto.cust-vend-no = "". 

      MESSAGE 
        "System control record not found. Update APCheckFile file path."
      UPDATE sys-ctrl-shipto.char-fld FORMAT "x(50)".

  END.    

  ASSIGN v-fileFormat = sys-ctrl.char-fld.

  IF AVAIL sys-ctrl AND
     sys-ctrl.log-fld  AND
     TRIM(v-fileFormat) NE ""
    THEN DO:

     IF sys-ctrl-shipto.char-fld NE "" THEN DO:

       IF SUBSTR(TRIM(sys-ctrl-shipto.char-fld),
                 LENGTH(sys-ctrl-shipto.char-fld) - 3 ,4) NE ".txt" 
         THEN DO:

          ASSIGN fi_CheckFile = STRING(TODAY,"99999999") + 
                                STRING(TIME) + ".txt".

          IF SUBSTR(sys-ctrl-shipto.char-fld,
                    LENGTH(sys-ctrl-shipto.char-fld),1) NE "/" AND
             SUBSTR(sys-ctrl-shipto.char-fld,
                    LENGTH(sys-ctrl-shipto.char-fld),1) NE "\"
            THEN 
             ASSIGN fi_CheckFile = TRIM(sys-ctrl-shipto.char-fld) + "\" + 
                                   fi_CheckFile.
            ELSE
             ASSIGN fi_CheckFile = TRIM(sys-ctrl-shipto.char-fld) + fi_CheckFile.
       END.
       ELSE
        ASSIGN fi_CheckFile = TRIM(sys-ctrl-shipto.char-fld).

     END.
     ELSE
      ASSIGN fi_CheckFile = "C:\tmp\" +
                            STRING(TODAY,"99999999") + 
                            STRING(TIME) + ".txt".       

     ASSIGN tb_APcheckFile:HIDDEN IN FRAME {&FRAME-NAME} = NO
            fi_CheckFile:HIDDEN   IN FRAME {&FRAME-NAME} = NO.

     ASSIGN tb_APcheckFile:HIDDEN    IN FRAME {&FRAME-NAME} = NO 
            tb_APcheckFile:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

     IF fi_CheckFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" 
       THEN 
        ASSIGN fi_CheckFile = "C:\tmp\" +
                               STRING(TODAY,"99999999") + 
                               STRING(TIME) + ".txt"
               fi_CheckFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi_CheckFile.

     ASSIGN fi_CheckFile:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

     IF tb_APcheckFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "YES" 
       THEN ASSIGN fi_CheckFile:HIDDEN IN FRAME {&FRAME-NAME} = NO.
       ELSE ASSIGN fi_CheckFile:HIDDEN IN FRAME {&FRAME-NAME} = YES.

     ASSIGN fi_CheckFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi_CheckFile.
  END.
  /* gdm - 05210901 end */

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

  DO WITH FRAME {&FRAME-NAME}:
    v-invalid = NO.

    FIND FIRST period NO-LOCK                   
        WHERE period.company EQ cocode
          AND period.pst     LE DATE(tran-date:SCREEN-VALUE)
          AND period.pend    GE DATE(tran-date:SCREEN-VALUE)
        NO-ERROR.
    IF AVAIL period THEN DO:
      IF NOT period.pstat THEN DO:
        MESSAGE "Period Already Closed..." VIEW-AS ALERT-BOX ERROR.
        v-invalid = YES.
      END.
      tran-period:SCREEN-VALUE = STRING(period.pnum).
    END.

    ELSE DO:
      MESSAGE "No Defined Period Exists for" tran-date
          VIEW-AS ALERT-BOX ERROR.
      v-invalid = YES.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkLock C-Win 
PROCEDURE checkLock :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Check for a locked record and return locking user
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER ipcTable AS CHAR NO-UNDO.
DEF INPUT PARAMETER iprRow   AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER oplLocked AS LOG NO-UNDO.
DEF OUTPUT PARAMETER opcUsr AS INT NO-UNDO.
DEF OUTPUT PARAMETER opcName AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER opcDevice AS CHAR NO-UNDO.

DEFINE VARIABLE wrecid AS INTEGER NO-UNDO.
DEFINE VARIABLE wtable AS INTEGER NO-UNDO.

/* Find the recid of the record */
CASE ipcTable:
   WHEN "gl-ctrl" THEN DO:
     FIND gl-ctrl WHERE ROWID(gl-ctrl) = iprRow NO-LOCK.
     ASSIGN wrecid = RECID(gl-ctrl).
   END.
END CASE.


FIND asi._file WHERE asi._file._file-name = ipcTable NO-LOCK NO-ERROR.
IF NOT AVAIL asi._file THEN
    RETURN.
ASSIGN wtable = asi._file._file-num.

/* Use repeat loop - More efficient than FIND ... WHERE
  due to lack of suitable index on _lock table */
FIND FIRST asi._lock NO-LOCK NO-ERROR.
REPEAT:

  IF NOT AVAIL asi._lock THEN
      LEAVE.

  IF _lock-recid = wrecid AND 
     _lock-table = wtable AND
     _lock-flag MATCHES "*X*" 
     /* typically we're interested in any form of exclusive lock
         so we test for X lock flag */
  THEN LEAVE.

  FIND NEXT asi._lock NO-LOCK NO-ERROR.
  IF AVAILABLE asi._lock AND asi._lock._lock-recid = ? THEN DO:
      RELEASE asi._lock.
      LEAVE.
  END.

END.

IF AVAILABLE(asi._lock) THEN DO:

    FIND FIRST asi._connect WHERE _connect-usr = _lock-usr NO-LOCK.
    IF AVAIL(asi._connect) AND asi._connect._connect-name = "" THEN
        FIND nosweat._user 
          WHERE nosweat._user._user_number EQ asi._connect._connect-usr
        NO-LOCK NO-ERROR.
    ASSIGN oplLocked = YES
           opcUsr    = asi._connect._connect-usr
           opcName   = _connect-name
           opcDevice = _connect-device.

    IF asi._connect._connect-name EQ "" THEN
        FIND nosweat._connect WHERE nosweat._connect._connect-usr EQ asi._connect._connect-usr NO-LOCK.

    IF AVAIL(nosweat._connect) THEN
       opcName = nosweat._connect._connect-name.    

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

  ASSIGN targetfile = lv-audit-dir + "\AP\VC3\Run#"
                    + STRING(v-trnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AP"
         dirname3 = lv-audit-dir + "\AP\VC3".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-bank-file C-Win 
PROCEDURE create-bank-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER op-data-file AS cha NO-UNDO.

  DEF VAR targetfile AS CHAR FORMAT "X(50)" NO-UNDO.
  DEF VAR dirname1 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR v-account AS CHAR NO-UNDO.
  DEF VAR v-ref AS cha NO-UNDO.
  DEF VAR v-check-date AS DATE NO-UNDO.
  DEF VAR v-check-date-string AS cha NO-UNDO.
  DEF VAR v-total-amt AS DEC NO-UNDO.
  DEF VAR v-check-total-amt AS DEC NO-UNDO.

  ASSIGN targetfile = aplockbx-path +
                     "CheckRegister" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") +
                      STRING(DAY(TODAY),"99") + STRING(TIME) + ".txt"
         dirname1 = aplockbx-path
         .

  IF SEARCH(dirname1) EQ ? THEN DO:
    OS-CREATE-DIR VALUE(dirname1).
  END.

  OUTPUT TO VALUE(targetfile).
  PUT UNFORMATTED "01021226C3648091" SKIP.
  v-total-amt = 0.
  FOR EACH tt-post,
      FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK,
      FIRST ap-chk NO-LOCK WHERE ap-chk.company   EQ ap-sel.company
                             AND ap-chk.check-no  EQ ap-sel.check-no,
      FIRST ap-inv NO-LOCK WHERE ap-inv.company EQ ap-sel.company
                             AND ap-inv.vend-no EQ ap-sel.vend-no
                             AND ap-inv.inv-no  EQ ap-sel.inv-no,    
      FIRST vend NO-LOCK WHERE vend.company EQ ap-inv.company
                           AND vend.vend-no EQ ap-inv.vend-no USE-INDEX vend 
         BREAK BY ap-sel.bank-code BY ap-sel.check-no:

      IF FIRST-OF(ap-sel.check-no) THEN v-check-total-amt = 0.

      v-check-total-amt = v-check-total-amt + ap-sel.amt-paid.
      IF LAST-OF(ap-sel.check-no) THEN DO:
          FIND FIRST bank WHERE bank.company = ap-sel.company AND
                             bank.bank-code = ap-sel.bank-code NO-ERROR.

          ASSIGN
          v-account = IF AVAIL bank THEN bank.bk-act ELSE ""
          v-ref = SUBSTRING(vend.name,1,12)
          v-check-date = IF ap-sel.man-check THEN ap-sel.pre-date ELSE ap-sel.check-date
          v-check-date-string = STRING(MONTH(v-check-date),"99") +
                                STRING(DAY(v-check-date),"99") + 
                                SUBstring(STRING(YEAR(v-check-date),"9999"),3,2)
          v-total-amt = v-total-amt + v-check-total-amt.
          PUT UNFORMATTED "V"
              ap-chk.check-no FORM "9999999999"
              v-account FORM "99999999999999"
              v-check-total-amt * 100 FORM "9999999999"
              v-ref FORM  "x(12)"
              v-check-date-string FORM "x(6)"
              SKIP.
      END.

  END.
  PUT UNFORMATTED "T          "
      v-account FORM "99999999999999"
      v-total-amt * 100 FORM "9999999999"
      SKIP.

  OUTPUT CLOSE.
  op-data-file = targetfile.

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
  DISPLAY tran-date tran-period lbl_sort rd_sort tb_prt-acc tb_void fi_CheckFile 
          tb_APcheckFile lv-ornt rd-dest lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date rd_sort tb_prt-acc tb_void fi_CheckFile 
         tb_APcheckFile lv-ornt rd-dest lines-per-page lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

     IF init-dir = "" THEN init-dir = "c:\temp" .
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
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/
    /*Use Progress Print. Always use Font#9 in Registry (set above) */
/*     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
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
  RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME  

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PositivePay-knight C-Win 
PROCEDURE PositivePay-knight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: gdm - 05210901
------------------------------------------------------------------------------*/
DEFINE VARIABLE v-check-date AS DATE NO-UNDO .
IF tb_APcheckFile THEN DO:
 
     FIND FIRST bank NO-LOCK
        WHERE bank.company EQ cocode
          AND bank.bank-code EQ ap-sel.bank-code NO-ERROR.

     v-check-date = IF ap-sel.man-check THEN ap-sel.pre-date ELSE ap-sel.check-date .

     PUT STREAM checkFile UNFORMATTED
     IF ap-sel.vend-no EQ "VOID"
          THEN "V" ELSE "I"      ","                                 /* Void Indicator */
     STRING(INT(REPLACE(bank.bk-act,"-","")), "9999999999")    ","    /* Account Number */
     STRING(INT(ap-sel.check-no))               ","            /* Check Number   */
     IF TRIM(vend.name) NE ""
       THEN
        TRIM(vend.name) 
       ELSE FILL(" ",10)                        ","            /* Payee NAME     */

     TRIM(STRING(v-amt-paid,"->>>>>>>9.99"))         ","      /* Amount         */  
     IF v-check-date NE ? 
        THEN STRING(v-check-date,"99/99/9999")   
        ELSE "00000000"                                        /* Issue Date     */
     FILL(" ",30)                                              /* Additinal data */ 
     
    SKIP.

END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PositivePay C-Win 
PROCEDURE PositivePay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: gdm - 05210901
------------------------------------------------------------------------------*/

IF tb_APcheckFile THEN DO:

    FIND FIRST bank NO-LOCK
        WHERE bank.company EQ cocode
          AND bank.bank-code EQ ap-sel.bank-code NO-ERROR.

   PUT STREAM checkFile UNFORMATTED
    "D"
     STRING(INT(ap-sel.bank-code),"999")                         /* Bank Number    */
     STRING(INT(REPLACE(bank.bk-act,"-","")), "9999999999")      /* Account Number */
     STRING(INT(ap-sel.check-no),"9999999999")                   /* Check Number   */
     STRING(INT(REPLACE(STRING(v-amt-paid,"->>,>>>,>>9.99"),".","")), "9999999999999") 
                                                                 /* Amount         */ 
     IF ap-sel.check-date NE ? 
        THEN STRING(YEAR(ap-sel.check-date),"9999") +  
             STRING(MONTH(ap-sel.check-date),"99")  +     
             STRING(DAY(ap-sel.check-date),"99")   
        ELSE "00000000"                                          /* Issue Date     */
     FILL(" ",30)                                                /* Additinal data */ 
     IF TRIM(vend.name) NE ""
       THEN
        TRIM(vend.name) + FILL(" ",(80 - LENGTH(TRIM(vend.name))))
       ELSE FILL(" ",80)                                         /* Payee NAME     */
     IF ap-sel.vend-no EQ "VOID"
          THEN "V" ELSE FILL(" ",1)                                       /* Void Indicator */
    SKIP.

END.



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

DEF BUFFER b-ap-pay FOR ap-pay.  

DEF VAR lv-check-no LIKE ap-chk.check-no NO-UNDO.


/** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
/*DO TRANSACTION:*/
  FOR EACH tt-post WHERE tt-post.actnum NE "",

      FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK

      BREAK BY tt-post.actnum:

    ACCUM tt-post.curr-paid - ap-sel.amt-paid (TOTAL BY tt-post.actnum). 

    IF LAST-OF(tt-post.actnum) THEN DO:
      CREATE gltrans.
      ASSIGN
      gltrans.company = cocode
      gltrans.actnum  = tt-post.actnum
      gltrans.jrnl    = "APCKR"
      gltrans.tr-dscr = "AP CHECK REGISTER CURRENCY GAIN/LOSS"
      gltrans.tr-date = tran-date
      gltrans.tr-amt  = (ACCUM TOTAL BY tt-post.actnum tt-post.curr-paid - ap-sel.amt-paid)
      gltrans.period  = tran-period
      gltrans.trnum   = v-trnum.
      RELEASE gltrans.

      CREATE gltrans.
      ASSIGN
      gltrans.company = cocode
      gltrans.actnum  = tt-post.actnum
      gltrans.jrnl    = "APCKR"
      gltrans.tr-dscr = "AP CHECK REGISTER CURRENCY GAIN/LOSS"
      gltrans.tr-date = tran-date
      gltrans.tr-amt  = - (ACCUM TOTAL BY tt-post.actnum tt-post.curr-paid - ap-sel.amt-paid)
      gltrans.period  = tran-period
      gltrans.trnum   = v-trnum.
      RELEASE gltrans.
    END.
  END.

  FOR EACH tt-post,

      FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id
            EXCLUSIVE-LOCK,

      FIRST ap-chk EXCLUSIVE-LOCK
      WHERE ap-chk.company   EQ ap-sel.company
        AND ap-chk.check-no  EQ ap-sel.check-no,

      FIRST ap-inv EXCLUSIVE-LOCK
      WHERE ap-inv.company EQ ap-sel.company
        AND ap-inv.vend-no EQ ap-sel.vend-no
        AND ap-inv.inv-no  EQ ap-sel.inv-no,

      FIRST vend EXCLUSIVE-LOCK
      WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no
      USE-INDEX vend

    BREAK BY ap-sel.bank-code
          BY ap-sel.check-no:

    IF ap-sel.man-check AND ap-sel.check-date EQ ? THEN
       ap-sel.check-date = ap-sel.pre-date.

    ACCUM ap-sel.amt-paid (TOTAL BY ap-sel.check-no).
    ACCUM ap-sel.amt-paid (TOTAL BY ap-sel.bank-code).

    IF FIRST-OF(ap-sel.check-no) THEN i = 0.

    lv-check-no = ap-sel.check-no.

    IF NOT ap-sel.man-check AND tb_void THEN DO:
      FIND LAST ap-pay NO-LOCK
          WHERE ap-pay.company   EQ ap-sel.company
            AND ap-pay.check-act EQ ap-sel.actnum
            AND ap-pay.check-no  LT ap-sel.check-no
          NO-ERROR.
      lv-check-no = (IF AVAIL ap-pay THEN ap-pay.check-no ELSE 0) + 1.
    END.

    DO lv-check-no = lv-check-no TO ap-sel.check-no:
      FIND FIRST ap-pay
          WHERE ap-pay.company   EQ ap-sel.company
            AND ap-pay.check-act EQ ap-sel.actnum
            AND ap-pay.check-no  EQ lv-check-no
          NO-ERROR.

      IF NOT AVAIL ap-pay THEN DO:
        FIND LAST ap-pay USE-INDEX c-no NO-ERROR.
        x = IF AVAIL ap-pay THEN ap-pay.c-no ELSE 0.

        CREATE ap-pay.
        ASSIGN
         ap-pay.company   = cocode
         ap-pay.check-act = ap-sel.actnum
         ap-pay.check-no  = lv-check-no
         ap-pay.period    = tran-period
         ap-pay.c-no      = x + 1
         ap-pay.vend-no   = ap-sel.vend-no
         ap-pay.bank-code = ap-sel.bank-code.

        IF ap-pay.check-no NE ap-sel.check-no THEN
          ASSIGN
           ap-pay.posted     = YES
           ap-pay.d-no       = ap-sel.check-no
           ap-pay.cleared    = NO.
      END.
    END.

    IF ap-sel.man-check THEN
      ASSIGN
       ap-pay.check-date  = ap-sel.pre-date
       ap-pay.man-check   = YES.
    ELSE
      ap-pay.check-date = ap-sel.check-date.

    ap-pay.posted = YES.
    IF ap-pay.check-date EQ ? THEN ap-pay.check-date = TODAY.

    FOR EACH b-ap-pay
        WHERE b-ap-pay.company EQ ap-pay.company
          AND b-ap-pay.d-no    EQ ap-pay.check-no
          AND NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ b-ap-pay.c-no)
          EXCLUSIVE-LOCK
        USE-INDEX d-no:
      b-ap-pay.check-date = ap-pay.check-date.
    END.

    FIND LAST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no USE-INDEX c-no NO-ERROR.
    i = IF AVAIL ap-payl THEN ap-payl.line ELSE 0.

    CREATE ap-payl.
    ASSIGN
     ap-payl.posted    = YES
     ap-payl.c-no      = ap-pay.c-no
     ap-payl.check-no  = ap-sel.check-no
     ap-payl.line      = i + 1
     ap-payl.inv-no    = ap-sel.inv-no
     ap-payl.due-date  = ap-sel.due-date
     ap-payl.amt-disc  = ap-sel.disc-amt
     ap-payl.amt-paid  = ap-sel.amt-paid
     ap-payl.vend-no   = ap-sel.vend-no
     ap-payl.man-check = ap-sel.man-check
     ap-payl.actnum    = ap-sel.actnum.

    ASSIGN
     ap-inv.paid       = ap-inv.paid + ap-sel.amt-paid
     ap-inv.check-no   = ap-sel.check-no
     ap-inv.pay-date   = ap-sel.check-date
     ap-inv.disc-taken = ap-inv.disc-taken + ap-sel.disc-amt
     ap-inv.due        = (ap-inv.net + ap-inv.freight) -
                          ap-inv.paid - ap-inv.disc-taken
     ap-payl.amt-due   = ap-inv.due.

    vend.acc-bal = vend.acc-bal - ap-sel.disc-amt - ap-sel.amt-paid.

    IF ap-inv.due      LE 0 AND
       ap-inv.pay-date NE ? AND
       ap-inv.inv-date NE ? THEN
      ASSIGN
       vend.avg-pay = ((vend.avg-pay * vend.num-inv) +
                       (ap-inv.pay-date - ap-inv.inv-date)) /
                      (vend.num-inv + 1)
       vend.num-inv = vend.num-inv + 1. /* number of invoices paid */

    FIND FIRST bank WHERE
         bank.company   = cocode AND
         bank.bank-code = ap-sel.bank-code
         EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL bank THEN bank.bal = bank.bal - ap-sel.amt-paid.

    IF LAST-OF(ap-sel.check-no) THEN DO:
      ASSIGN
       vend.last-pay    = ap-sel.check-date
       ap-pay.check-amt = (ACCUM TOTAL BY ap-sel.check-no ap-sel.amt-paid)
       vend.lpay        = ap-pay.check-amt
       vend.lpay-date   = ap-sel.check-date.

      CREATE ap-ledger.
      ASSIGN
       ap-ledger.company   = ap-sel.company
       ap-ledger.vend-no   = ap-sel.vend-no
       ap-ledger.refnum    = "AC" + STRING(ap-sel.check-no, "999999")
       ap-ledger.ref-date  = ap-sel.check-date
       ap-ledger.tr-date   = tran-date
       ap-ledger.trnum     = v-trnum
       ap-ledger.period    = tran-period
       ap-ledger.amt       = (ACCUM TOTAL BY ap-sel.check-no ap-sel.amt-paid)
       ap-ledger.actnum    = wcash.
      RELEASE ap-ledger.
    END.

    /*** Moved gltrans create here so bank's actnum can be used ***/
    IF LAST-OF(ap-sel.bank-code) THEN DO:
      CREATE gltrans.
      ASSIGN
      gltrans.company = cocode
      gltrans.actnum  = bank.actnum
      gltrans.jrnl    = "APCKR"
      gltrans.tr-dscr = "AP CHECK REGISTER"
      gltrans.tr-date = tran-date
      gltrans.tr-amt  = - ACCUM TOTAL BY ap-sel.bank-code ap-sel.amt-paid
      gltrans.period  = tran-period
      gltrans.trnum   = v-trnum.
      RELEASE gltrans.
    END.

    DELETE ap-sel.

    IF NOT CAN-FIND(FIRST ap-sel WHERE ap-sel.company   EQ ap-chk.company
                                   AND ap-sel.check-no  EQ ap-chk.check-no
                                   AND ap-sel.man-check EQ ap-chk.man-check)
    THEN DELETE ap-chk.
  END. /* for each tt-post */

  RELEASE vend.
  RELEASE bank.
  RELEASE ap-pay.
  RELEASE ap-payl.
  RELEASE ap-inv.

  FOR EACH ap-sel
      WHERE ap-sel.company      EQ cocode
        AND ap-sel.check-no     NE ?
        AND ap-sel.man-check    EQ post-manual
        AND TRIM(ap-sel.inv-no) LE ""
      EXCLUSIVE-LOCK:
    DELETE ap-sel.
  END.

  FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.

  CREATE gltrans.
  ASSIGN
    gltrans.company = cocode
    gltrans.actnum  = ap-ctrl.payables
    gltrans.jrnl    = "APCKR"
    gltrans.tr-dscr = "AP CHECK REGISTER"
    gltrans.tr-date = tran-date
    gltrans.tr-amt  = gtot1 + gtot2
    gltrans.period  = tran-period
    gltrans.trnum   = v-trnum.
  RELEASE gltrans.

  IF gtot1 NE 0 THEN DO:
    CREATE gltrans.
    ASSIGN
     gltrans.company = cocode
     gltrans.actnum  = ap-ctrl.discount
     gltrans.jrnl    = "APCKR"
     gltrans.tr-dscr = "AP CHECK REGISTER"
     gltrans.tr-date = tran-date
     gltrans.tr-amt  = - gtot1
     gltrans.period  = tran-period
     gltrans.trnum   = v-trnum.
    RELEASE gltrans.
  END.
/*END. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- oe/invpost.p 10/94 gb */
/* Invoicing  - Edit Register & Post Invoicing Transactions                   */
/* -------------------------------------------------------------------------- */

ASSIGN
time_stamp = STRING(TIME, "hh:mmam")
tmpstore   = FILL("_",125).

{sys/form/r-top3w.f}

DEF VAR v-line-amt LIKE ap-invl.amt NO-UNDO.
DEF VAR v-frgt-amt LIKE ap-inv.freight NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.


FORMAT HEADER
       "Check    Vendor"
       "Invoice                                 Discount" AT 55
       "   Check   Pre-Issued" AT 110 SKIP
       "Number   Number   Name       Check Date"
       "Date      Number                  Due      Taken" AT 55
       "Amt Paid   Date"       AT 110 SKIP
       FILL("_",132) FORMAT "x(130)"      SKIP
    WITH FRAME f-top WIDTH 132 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO.


SESSION:SET-WAIT-STATE("general").

ASSIGN
 str-tit  = coname + " - " + loname
 str-tit2 = "A/P CHECKS REGISTER " + STRING(v-trnum)
 str-tit3 = "Period " + STRING(tran-period,"99") + " " + STRING(tran-date)
 x = (112 - LENGTH(str-tit)) / 2
 str-tit  = FILL(" ",x) + str-tit
 x = (114 - LENGTH(str-tit2)) / 2
 str-tit2 = FILL(" ",x) + str-tit2
 x = (132 - LENGTH(str-tit3)) / 2
 str-tit3 = FILL(" ",x) + str-tit3
 gtot0 = 0
 gtot1 = 0
 gtot2 = 0
 ctr   = 0.

EMPTY TEMP-TABLE tt-post.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Check Number,Vendor Number,Name,Check Date,Invoice Date,"
              + "Number,Due,Discount Taken,Check Amt Paid,Pre-Issued Date".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

/* gdm - 05210901 */
IF tb_APcheckFile 
  THEN OUTPUT STREAM checkFile TO VALUE(fi_CheckFile).
/* gdm - 05210901 end */

  IF td-show-parm THEN RUN show-param.

  DISPLAY "" WITH FRAME r-top.
  DISPLAY "" WITH FRAME f-top.

  FOR EACH ap-sel NO-LOCK
      WHERE ap-sel.company      EQ cocode
        AND ap-sel.check-no     NE ?
        AND ap-sel.check-no     GT 0
        AND ap-sel.man-check    EQ post-manual
      AND TRIM(ap-sel.inv-no) GT ""
      AND CAN-FIND(FIRST ap-chk
                   WHERE ap-chk.company   EQ ap-sel.company
                     AND ap-chk.check-no  EQ ap-sel.check-no
                     AND ap-chk.man-check EQ ap-sel.man-check),

    FIRST ap-inv NO-LOCK
    WHERE ap-inv.company EQ ap-sel.company
      AND ap-inv.vend-no EQ ap-sel.vend-no
      AND ap-inv.inv-no  EQ ap-sel.inv-no,

    FIRST vend NO-LOCK
    WHERE vend.company EQ ap-sel.company
      AND vend.vend-no EQ ap-sel.vend-no

    BREAK BY ap-sel.check-no WITH STREAM-IO WIDTH 132 NO-BOX NO-ATTR-SPACE NO-LABELS:

  IF FIRST-OF(ap-sel.check-no) THEN v-fst-chk = YES.
  IF LAST-OF(ap-sel.check-no)  THEN V-lst-chk = YES.

  IF ap-sel.vend-no EQ "VOID" THEN DO:
    DISPLAY ap-sel.check-no    FORMAT "zzzzzzz9"
            ap-sel.vend-no
            SKIP(1)
        WITH FRAME vv NO-BOX NO-LABELS NO-ATTR-SPACE STREAM-IO.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
           '"' STRING(ap-sel.check-no,"zzzzzzz9") '",'
           '"' ap-sel.vend-no                     '",'
           SKIP.
/***************************************************************************
  gdm - 05210901 - PROCEDURE NAME MUST BE THE SAME AS THE CHARACTER VALUE 
                   WITH OUT SPACES
                   E.G 
                    CHAR VALUE = "Positive Pay"
                    PROCEDURE  = "PositivePay"
******************************************************************************/
    IF tb_APcheckFile THEN  RUN VALUE(REPLACE(TRIM(v-fileFormat)," ","")).  
    ASSIGN v-amt-paid = 0.
/* gdm - 05210901 end */

    NEXT.
  END.

  CREATE tt-post.
  ASSIGN
   tt-post.row-id    = ROWID(ap-sel)
   tt-post.curr-bal  = ap-sel.inv-bal
   tt-post.curr-disc = ap-sel.disc-amt
   tt-post.curr-paid = ap-sel.amt-paid.

  RELEASE currency.
  IF lv-comp-curr NE "" AND lv-comp-curr NE ap-inv.curr-code[1] THEN
  FIND FIRST currency NO-LOCK
      WHERE currency.company     EQ ap-inv.company
        AND currency.c-code      EQ ap-inv.curr-code[1]
        AND currency.ar-ast-acct NE ""
        AND currency.ex-rate     GT 0
      NO-ERROR.

  IF AVAIL currency THEN
    ASSIGN
     tt-post.actnum    = currency.ar-ast-acct
     tt-post.ex-rate   = currency.ex-rate
     tt-post.curr-disc = tt-post.curr-disc * tt-post.ex-rate
     tt-post.curr-paid = tt-post.curr-paid * tt-post.ex-rate
     tt-post.curr-bal  = tt-post.curr-bal  * tt-post.ex-rate.

  v-postable = YES.

  IF v-fst-chk THEN
  DO:
    DISPLAY TRIM(STRING(ap-sel.check-no,">>>>>>>>")) FORMAT "x(8)"
            vend.vend-no
            vend.name
            SPACE(6)
        WITH FRAME a STREAM-IO.

    IF tb_excel THEN
    DO:
       FIND FIRST ap-chk WHERE
          ap-chk.company  EQ ap-sel.company AND
          ap-chk.check-no EQ ap-sel.check-no
          NO-LOCK NO-ERROR.

       PUT STREAM excel UNFORMATTED
           '"' TRIM(STRING(ap-sel.check-no,">>>>>>>>")) '",'
           '"' vend.vend-no                             '",'
           '"' vend.NAME                                '",'
           '"' ap-chk.check-date                        '",'.
    END.
  END.
  ELSE IF tb_excel THEN
     PUT STREAM excel UNFORMATTED
         '"' "" '",'
         '"' "" '",'
         '"' "" '",'
         '"' "" '",'.

  DISPLAY ap-inv.inv-date           FORMAT "99/99/99"
          SPACE(2)
          ap-sel.inv-no
          tt-post.curr-bal  TO 91
          tt-post.curr-disc TO 102
          tt-post.curr-paid TO 117

      WITH FRAME a STREAM-IO WIDTH 132 NO-LABELS NO-BOX.

  IF tb_excel THEN
  DO:
     PUT STREAM excel UNFORMATTED
         '"' ap-inv.inv-date                            '",'
         '"' ap-sel.inv-no                              '",'
         '"' STRING(tt-post.curr-bal,"->>,>>>,>>9.99")  '",'
         '"' STRING(tt-post.curr-disc,"->>,>>9.99 ")    '",'
         '"' STRING(tt-post.curr-paid,"->>,>>>,>>9.99") '",'.
  END.

  IF ap-sel.man-check THEN
  DO:
     DISPLAY ap-sel.pre-date TO 130 WITH FRAME a.
     IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' ap-sel.pre-date '",'.
  END.

  IF tb_excel THEN
     PUT STREAM excel UNFORMATTED SKIP.

  ASSIGN
   tot0 = tot0 + tt-post.curr-bal 
   tot1 = tot1 + tt-post.curr-disc
   tot2 = tot2 + tt-post.curr-paid.

  /* gdm - 05210901 */
  ASSIGN v-amt-paid = v-amt-paid + ap-sel.amt-paid.

  IF v-lst-chk THEN DO:
    FIND FIRST ap-chk
        WHERE ap-chk.company  EQ ap-sel.company
          AND ap-chk.check-no EQ ap-sel.check-no
        NO-LOCK NO-ERROR.

    DISPLAY ap-chk.check-date AT 30     FORMAT "99/99/99"
            "** CHECK TOTAL"  AT 51
            tot0              TO 91
            tot1              TO 102
            tot2              TO 117 "*"
            SKIP(1)

        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME ctot WIDTH 132 STREAM-IO.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
           SKIP(1)
           '"' ""                            '",'
           '"' ""                            '",'
           '"' ""                            '",'
           '"' ""                            '",'
           '"' "** CHECK TOTAL"              '",'
           '"' ""                            '",'
           '"' STRING(tot0,"->>,>>>,>>9.99") '",'
           '"' STRING(tot1,"->>,>>9.99 ")    '",'
           '"' STRING(tot2,"->>,>>>,>>9.99") '",'
           '"' "*"                           '",'
           SKIP(1).

/***************************************************************************
  gdm - 05210901 - PROCEDURE NAME MUST BE THE SAME AS THE CHARACTER VALUE 
                   WITH OUT SPACES
                   E.G 
                    CHAR VALUE = "Positive Pay"
                    PROCEDURE  = "PositivePay"
******************************************************************************/
  IF tb_APcheckFile THEN RUN VALUE(REPLACE(TRIM(v-fileFormat)," ","")).  
  ASSIGN v-amt-paid = 0.
/* gdm - 05210901 end */

    ASSIGN
     ctr   = ctr + 1
     gtot0 = gtot0 + tot0
     gtot1 = gtot1 + tot1
     gtot2 = gtot2 + tot2
     tot0  = 0
     tot1  = 0
     tot2  = 0
     v-amt-paid = 0.
  END.

  ASSIGN
   v-fst-chk = NO
   v-lst-chk = NO.

  ACCUM ap-sel.disc-amt (TOTAL).
  ACCUM ap-sel.amt-paid (TOTAL).
END. /* each ap-sel */

DISPLAY "*** GRAND TOTALS ***" AT 50
        gtot0                  TO 91
        gtot1                  TO 102
        gtot2                  TO 117 "**" SKIP (1)
        "NUMBER OF CHECKS WRITTEN " ctr
    WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME b WIDTH 132 STREAM-IO.

IF tb_APcheckFile 
  THEN OUTPUT STREAM checkFile CLOSE.

IF tb_excel THEN
   PUT STREAM excel UNFORMATTED
       '"' ""                             '",'
       '"' ""                             '",'
       '"' ""                             '",'
       '"' ""                             '",'
       '"' "*** GRAND TOTALS ***"         '",'
       '"' ""                             '",'
       '"' STRING(gtot0,"->>,>>>,>>9.99") '",'
       '"' STRING(gtot1,"->>,>>9.99 ")    '",'
       '"' STRING(gtot2,"->>,>>>,>>9.99") '",'
       '"' "**"                           '",'
       SKIP.

ASSIGN   /* For posting without currency exchange rate */
 gtot1 = (ACCUM TOTAL ap-sel.disc-amt)
 gtot2 = (ACCUM TOTAL ap-sel.amt-paid).

IF tb_prt-acc THEN DO:
  HIDE FRAME f-top.

  ASSIGN
    str-tit3 = "Period " + STRING(tran-period,"99") + " - " + "Summary by Account"
    x = (132 - LENGTH(str-tit3)) / 2
    str-tit3 = FILL(" ",x) + str-tit3.

  PAGE.

  FORM HEADER
       "ACCOUNT                             PO#   DATE   VENDOR#  INVOICE#    "
       "LINE DESCRIPTION              QTY    UNIT PRICE     AMT PAID" SKIP
       FILL("_",132) FORMAT "x(132)"

      WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top2 PAGE-TOP WIDTH 132 STREAM-IO.

  DISPLAY "" WITH FRAME f-top2.

  FOR EACH tt-post,

      FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK,

      FIRST ap-inv NO-LOCK
      WHERE ap-inv.company EQ ap-sel.company
        AND ap-inv.vend-no EQ ap-sel.vend-no
        AND ap-inv.inv-no  EQ ap-sel.inv-no
        AND ap-inv.freight NE 0,

      FIRST vend NO-LOCK
      WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no
      USE-INDEX vend

      BREAK BY ap-inv.vend-no:

    IF FIRST(ap-inv.vend-no) THEN DO:
      FIND FIRST account NO-LOCK
          WHERE account.company EQ ap-inv.company
            AND account.actnum  EQ v-frt-acct
          NO-ERROR.
      PUT v-frt-acct + " - " +
          (IF AVAIL account THEN account.dscr ELSE "Not on file")
                                FORMAT "x(40)" SKIP.
    END.

    v-frgt-amt = ap-sel.amt-paid *
                 (ap-inv.freight / (ap-inv.net + ap-inv.freight)).

    PUT ap-inv.inv-date         AT 41   FORMAT "99/99/99"
        SPACE(1)
        ap-inv.vend-no
        SPACE(1)
        ap-inv.inv-no
        SPACE(6)
        "Freight"                       FORMAT "x(18)"
        SPACE(7)
        1.0                             FORMAT "9.9"
        SPACE(1)
        ap-inv.freight          TO 118
        v-frgt-amt              TO 131
        SKIP.

    ACCUM v-frgt-amt (TOTAL).

    IF LAST(ap-inv.vend-no) THEN
      PUT "** TOTAL " TO 114
          (ACCUM TOTAL v-frgt-amt) FORMAT "->>,>>>,>>9.99" TO 128
          " *" SKIP(1).
  END.

  FOR EACH tt-post,

      FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK,

      FIRST ap-inv NO-LOCK
      WHERE ap-inv.company EQ ap-sel.company
        AND ap-inv.vend-no EQ ap-sel.vend-no
        AND ap-inv.inv-no  EQ ap-sel.inv-no,

      FIRST vend NO-LOCK
      WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no
      USE-INDEX vend,

      EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no USE-INDEX i-no NO-LOCK

      BREAK BY ap-invl.actnum
            BY ap-invl.inv-no
            BY ap-invl.line

      WITH WIDTH 132 NO-LABELS:

    IF FIRST-OF(ap-invl.actnum) THEN DO:
      FIND FIRST account
          WHERE account.company EQ ap-inv.company
            AND account.actnum  EQ ap-invl.actnum
          NO-LOCK NO-ERROR.

      PUT ap-invl.actnum + " - " +
              (IF AVAIL account THEN account.dscr ELSE "Not on file")
                            FORMAT "x(40)" SKIP.
    END.

    v-line-amt = ap-sel.amt-paid * (ap-invl.amt / (ap-inv.net + ap-inv.freight)).

    PUT ap-invl.po-no         AT 34
        SPACE(1)
        ap-inv.inv-date       FORMAT "99/99/99"
        SPACE(1)
        ap-inv.vend-no
        SPACE(1)
        ap-inv.inv-no
        SPACE(1)
        {ap/invlline.i -1}    FORMAT ">>>9"
        SPACE(1)
        ap-invl.dscr          FORMAT "x(18)"
        SPACE(1)
        ap-invl.qty           FORMAT "->>,>>9.9<<"
        SPACE(1)
        ap-invl.unit-pr
        SPACE(1)
        v-line-amt
        SPACE(1)
        SKIP.

    ACCUM v-line-amt (TOTAL BY ap-invl.actnum).
    ACCUM v-line-amt (TOTAL).

    IF LAST-OF(ap-invl.actnum) THEN
      PUT "** TOTAL " TO 114
          (ACCUM TOTAL BY ap-invl.actnum v-line-amt)
                        FORMAT "->>,>>>,>>9.99" TO 128
          " *" SKIP(1).
  END.

  FOR EACH tt-post WHERE tt-post.actnum NE "",

      FIRST ap-sel WHERE ROWID(ap-sel) EQ tt-post.row-id NO-LOCK,

      FIRST ap-inv NO-LOCK
      WHERE ap-inv.company EQ ap-sel.company
        AND ap-inv.vend-no EQ ap-sel.vend-no
        AND ap-inv.inv-no  EQ ap-sel.inv-no

      BREAK BY tt-post.actnum

      WITH WIDTH 132 NO-LABELS:

    IF FIRST-OF(tt-post.actnum) THEN DO:
      FIND FIRST account
          WHERE account.company EQ ap-sel.company
            AND account.actnum  EQ tt-post.actnum
          NO-LOCK NO-ERROR.

      PUT tt-post.actnum + " - " +
              (IF AVAIL account THEN account.dscr ELSE "Not on file")
                            FORMAT "x(40)" SKIP.
    END.

    PUT ap-inv.inv-date         AT 41   FORMAT "99/99/99"
        SPACE(1)
        ap-inv.vend-no
        SPACE(1)
        ap-inv.inv-no
        SPACE(6)
        "Currency"                      FORMAT "x(18)"
        SPACE(7)
        1.0                             FORMAT "9.9"
        tt-post.curr-paid - ap-sel.amt-paid TO 131
        SKIP.

    ACCUM tt-post.curr-paid - ap-sel.amt-paid (TOTAL BY tt-post.actnum).
    ACCUM tt-post.curr-paid - ap-sel.amt-paid (TOTAL).

    IF LAST-OF(tt-post.actnum) THEN
      PUT "** TOTAL " TO 114
          (ACCUM TOTAL BY tt-post.actnum tt-post.curr-paid - ap-sel.amt-paid)
                        FORMAT "->>,>>>,>>9.99" TO 128
          " *" SKIP(1).
  END.

  PUT "***** TOTAL for ALL ACCOUNTS " TO 116
      (ACCUM TOTAL v-line-amt) +
       (ACCUM TOTAL v-frgt-amt) +
        (ACCUM TOTAL tt-post.curr-paid - ap-sel.amt-paid)
                        FORMAT "->>,>>>,>>9.99" TO 130
      SKIP(2).
END.

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
  DEF VAR lv-frame-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field2-hdl AS HANDLE NO-UNDO.
  DEF VAR parm-fld-list AS cha NO-UNDO.
  DEF VAR parm-lbl-list AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-label AS cha.

  lv-frame-hdl = FRAME {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                     .
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     .
              lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                  END.
                  lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
              END.       
           END.                 
        END.            
     lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
  END.

  PUT SPACE(28)
      "< Selection Parameters >"
      SKIP(1).

  DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
    IF ENTRY(i,parm-fld-list) NE "" OR
       entry(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                 trim(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

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
/* GET next G/L TRANS. POSTING # **/
  /* gdm - 11050906 */
DEF VAR lLocked AS LOG NO-UNDO.
DEF VAR cUsr AS INT NO-UNDO.
DEF VAR cName AS CHAR NO-UNDO.
DEF VAR cDevice AS CHAR NO-UNDO.


REPEAT:

    FIND FIRST gl-ctrl NO-LOCK
       WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.

    IF AVAIL gl-ctrl THEN DO:
        RUN checkLock(INPUT "gl-ctrl",
                      INPUT ROWID(gl-ctrl),
                      OUTPUT lLocked,
                      OUTPUT cUsr,
                      OUTPUT cName,
                      OUTPUT cDevice).
      IF lLocked THEN DO:
       /* To display standard progress locking message */
        FIND FIRST gl-ctrl EXCLUSIVE-LOCK
          WHERE gl-ctrl.company EQ cocode .
        RELEASE gl-ctrl.
      END.
/*           MESSAGE "The General Ledger Control Record is locked by " cName SKIP */
/*                   "Press OK to try again."                                     */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                               */
    END. /* avail gl-ctrl */
    RELEASE gl-ctrl.
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




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

