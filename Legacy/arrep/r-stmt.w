&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : arrep\r-stmt.w

  Description       : Statements

  Input Parameters  : None

  Output Parameters : None

  Author            : Ron Stark

  Created           : 01/12/2000

  History           : dgd 06/08/2007  - Task# 05300713: Batch E-Mail

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
DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

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

/*{sys/inc/custlistform.i ""AR4"" }*/

{sys/ref/CustList.i NEW}

def NEW SHARED temp-table tt-inv no-undo
  field inv-date as date
  field sort-fld as char
  field trans-date as date
  field inv-no like ar-inv.inv-no
  field type as char format 'x(4)'
  field description as char format 'x(15)'
  field amount  as dec format '->>,>>>,>>>.99'
  FIELD inv-amt LIKE ar-inv.gross
  FIELD cust-no AS CHAR
  FIELD po-no LIKE ar-invl.po-no
  FIELD bol-no AS CHAR 
  FIELD old-day AS INT
  index tt-inv cust-no inv-date sort-fld trans-date.

DEF NEW SHARED TEMP-TABLE tt-cust-excel NO-UNDO
    FIELD cust-no AS CHAR
    FIELD contact AS CHAR
    FIELD addr    AS CHAR EXTENT 5
    FIELD aged    AS DEC EXTENT 5
    INDEX excel cust-no ASC.
DEF TEMP-TABLE tt-cust-check NO-UNDO
    FIELD cust-no AS CHAR
    FIELD contact AS CHAR
    FIELD log-fld AS LOGICAL
    INDEX check1 cust-no ASC.

DEF VAR is-xprint-form  AS LOG NO-UNDO.
DEF VAR ls-fax-file     AS cha NO-UNDO.
DEF VAR lv-pdf-file     AS cha NO-UNDO.
DEF VAR lv-fax-image    AS cha NO-UNDO.  /* fax imge file */
DEF VAR vlSkipRec       AS LOG NO-UNDO.
DEF NEW SHARED VAR LvOutputSelection AS CHAR NO-UNDO.
DEF VAR v-stmt-char AS cha NO-UNDO.
DEF VAR v-pdf-camp AS LOG NO-UNDO.
def var v-print-hdr like sys-ctrl.log-fld no-undo.
def var v-use-cust as log no-undo.
DEF VAR vcDefaultForm AS CHAR NO-UNDO.
DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

{custom/xprint.i}

/* Buffers */
DEF BUFFER b1-cust  FOR cust.
DEF BUFFER b-cust   FOR cust.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 stmt-date tb_cust-list ~
btnCustList begin_cust-no end_cust-no stmt-msg fi_contact tb_detailed ~
tb_past-due tb_curr-bal tb_HideDialog rd-dest tb_BatchMail tb_emailpdf ~
lines-per-page lv-ornt lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS stmt-date tb_cust-list begin_cust-no ~
end_cust-no stmt-msg fi_contact lbl_detailed tb_detailed lbl_past-due ~
tb_past-due lbl_curr-bal tb_curr-bal tb_HideDialog rd-dest tb_BatchMail ~
tb_emailpdf lines-per-page lv-ornt lv-font-no lv-font-name td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD formatDate C-Win 
FUNCTION formatDate RETURNS CHARACTER
  ( INPUT ip-date AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_contact AS CHARACTER FORMAT "X(40)":U 
     LABEL "Attention" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_curr-bal AS CHARACTER FORMAT "X(256)":U INITIAL "Include customers with 0 current balance?" 
     VIEW-AS FILL-IN 
     SIZE 44 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_detailed AS CHARACTER FORMAT "X(256)":U INITIAL "Detailed?" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_past-due AS CHARACTER FORMAT "X(256)":U INITIAL "Print Past Due Only?" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .95 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE stmt-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Statement Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE stmt-msg AS CHARACTER FORMAT "X(40)":U 
     LABEL "Statement Message" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 NO-UNDO.

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
     SIZE 19 BY 7 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 9.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.

DEFINE VARIABLE tb_BatchMail AS LOGICAL INITIAL no 
     LABEL "&Batch Mail" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_curr-bal AS LOGICAL INITIAL no 
     LABEL "Include customers with 0 current balance?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE tb_emailpdf AS LOGICAL INITIAL no 
     LABEL "&Email PDF (PDFCamp)" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_HideDialog AS LOGICAL INITIAL no 
     LABEL "&Hide Dialog-Box" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_past-due AS LOGICAL INITIAL no 
     LABEL "Print Past Due Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     stmt-date AT ROW 1.95 COL 27 COLON-ALIGNED
     tb_cust-list AT ROW 2.91 COL 29.2 WIDGET-ID 6
     btnCustList AT ROW 2.91 COL 61.6 WIDGET-ID 8
     begin_cust-no AT ROW 3.86 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 3.86 COL 66 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     stmt-msg AT ROW 5.05 COL 27 COLON-ALIGNED
     fi_contact AT ROW 6.29 COL 27 COLON-ALIGNED WIDGET-ID 2
     lbl_detailed AT ROW 7.62 COL 37 COLON-ALIGNED NO-LABEL
     tb_detailed AT ROW 7.62 COL 51
     lbl_past-due AT ROW 8.81 COL 27 COLON-ALIGNED NO-LABEL
     tb_past-due AT ROW 8.81 COL 51
     lbl_curr-bal AT ROW 9.86 COL 7 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     tb_curr-bal AT ROW 9.86 COL 51 WIDGET-ID 12
     tb_HideDialog AT ROW 12.14 COL 46.6
     rd-dest AT ROW 13.14 COL 6 NO-LABEL
     tb_BatchMail AT ROW 13.19 COL 31
     tb_emailpdf AT ROW 13.19 COL 67.2
     lines-per-page AT ROW 14.67 COL 84 COLON-ALIGNED
     lv-ornt AT ROW 14.71 COL 31 NO-LABEL
     lv-font-no AT ROW 16.33 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 17.29 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.05 COL 31
     btn-ok AT ROW 20.81 COL 20
     btn-cancel AT ROW 20.81 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.62 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 4
          BGCOLOR 2 
     RECT-6 AT ROW 11 COL 2
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
         TITLE              = "Statements"
         HEIGHT             = 21.62
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
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_contact:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_curr-bal IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_detailed IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_past-due IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       stmt-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       stmt-msg:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_curr-bal:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_emailpdf:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_HideDialog:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_past-due:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Statements */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Statements */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
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
  DEF VAR v-excel AS LOG NO-UNDO.
  DEF VAR v-cust-mode AS CHAR NO-UNDO.
  DEF VAR v-excel-message AS LOG NO-UNDO.

  SESSION:SET-WAIT-STATE ("GENERAL").

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN {&displayed-objects}.
  END.

  IF rd-dest = 4 OR (rd-dest = 5 AND NOT tb_BatchMail:CHECKED)
     AND begin_cust-no <> end_cust-no THEN DO:

    IF rd-dest = 4 THEN
       MESSAGE "Beginning Customer and Ending Customer must be the same for Fax."
          VIEW-AS ALERT-BOX ERROR.

    ELSE IF rd-dest = 5 THEN
        MESSAGE "Beginning Customer and Ending Customer must be the same for E-Mail."
           VIEW-AS ALERT-BOX ERROR.

    APPLY "entry" TO end_cust-no .
    RETURN NO-APPLY.
  END.
  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT tb_cust-list OR NOT AVAIL ttCustList THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive,
                    INPUT begin_cust-no,
                    INPUT END_cust-no).
  END.
  IF tb_cust-list THEN DO:
      FOR EACH tt-cust-check WHERE tt-cust-check.log-fld  NO-LOCK:
          FIND FIRST ttCustList EXCLUSIVE-LOCK
               WHERE ttCustList.cust-no EQ tt-cust-check.cust-no NO-ERROR .
          IF AVAIL ttCustList THEN
              ASSIGN
              ttCustList.log-fld = YES.
      END.
  END.

  IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
     sys-ctrl-shipto.company = cocode AND
     sys-ctrl-shipto.NAME = "STMTPRINT") THEN
     DO:
        FOR EACH b-cust FIELDS(cust-no) WHERE
            b-cust.company EQ cocode AND
            b-cust.cust-no GE begin_cust-no AND
            b-cust.cust-no LE end_cust-no
            NO-LOCK:

            FIND FIRST sys-ctrl-shipto WHERE
                 sys-ctrl-shipto.company = cocode AND
                 sys-ctrl-shipto.NAME = "STMTPRINT" AND
                 sys-ctrl-shipto.cust-vend = YES AND
                 sys-ctrl-shipto.cust-vend-no = b-cust.cust-no AND
                 sys-ctrl-shipto.char-fld > ''
                 NO-LOCK NO-ERROR.

            IF AVAIL sys-ctrl-shipto THEN
               v-stmt-char = sys-ctrl-shipto.char-fld.
            ELSE 
               v-stmt-char = sys-ctrl.char-fld.
            /*ELSE
               v-stmt-char = vcDefaultForm.*/
               FIND FIRST ttCustList NO-LOCK
                   WHERE ttCustList.cust-no EQ b-cust.cust-no
                       AND ttCustList.log-fld = YES NO-ERROR .
               IF NOT AVAIL ttCustList THEN NEXT .

            IF v-stmt-char EQ "ASIExcel" THEN
            DO:
               v-excel = YES.

               CASE rd-dest:
                  WHEN 1 THEN
                     LvOutputSelection = "Printer".
                  WHEN 2 THEN
                     LvOutputSelection = "Screen". 
                  WHEN 3 THEN
                     LvOutputSelection = "File". 
                  WHEN 4 THEN
                     LvOutputSelection = "Fax". 
                  WHEN 5 THEN
                     LvOutputSelection = "Email".
                  WHEN 6 THEN
                     LvOutputSelection = "Port".
               END CASE.
            END.
            ELSE
               v-excel = NO.

            IF v-excel AND tb_BatchMail:CHECKED THEN
            DO:
               IF v-excel-message = NO THEN
               DO:
                  v-excel-message = YES.
                  MESSAGE "Cannot Run Batch Mail Mode for Excel Document."
                      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
               END.
               NEXT.
            END.

            IF v-excel OR NOT rd-dest = 5 THEN
               run run-report(b-cust.cust-no, TRUE).

            IF NOT v-excel THEN
               RUN GenerateReport(b-cust.cust-no,b-cust.cust-no).
            ELSE IF rd-dest EQ 5 THEN /*excel*/
            DO:   
               v-cust-mode = IF NOT tb_HideDialog:CHECKED THEN "Customer"
                             ELSE "Customer1".
               RUN SendMail-1 (b-cust.cust-no, v-cust-mode,  v-dir + "\stmt.pdf").
            END.
            FOR EACH ttCustList NO-LOCK
                    WHERE ttCustList.cust-no EQ b-cust.cust-no :
                   ASSIGN ttCustList.log-fld = NO  .
            END.
           
        END. /*each b-cust*/

     END. /*if sys-ctrl-shipto found*/
  ELSE IF rd-dest = 5 AND tb_BatchMail:CHECKED THEN  /*if no sys-ctrl-shipto found*/
  DO:
  FOR EACH ttCustList NO-LOCK  :
  
          /* find first ar-inv where ar-inv.company eq cocode    and
                            ar-inv.cust-no eq ttCustList.cust-no and
                            ar-inv.posted                and
                            ar-inv.due ne 0              and
                            ar-inv.inv-date le stmt-date and
                            ar-inv.due-date le stmt-date no-lock no-error.

           if not avail ar-inv THEN next.*/

     v-stmt-char = vcDefaultForm.
     IF v-stmt-char EQ "ASIExcel" THEN
     DO:
        v-excel = YES.

        CASE rd-dest:
           WHEN 1 THEN
              LvOutputSelection = "Printer".
           WHEN 2 THEN
              LvOutputSelection = "Screen". 
           WHEN 3 THEN
              LvOutputSelection = "File". 
           WHEN 4 THEN
              LvOutputSelection = "Fax". 
           WHEN 5 THEN
              LvOutputSelection = "Email".
           WHEN 6 THEN
              LvOutputSelection = "Port".
        END CASE.
     END.
     ELSE
        v-excel = NO.

     IF v-excel AND tb_BatchMail:CHECKED THEN
     DO:
        MESSAGE "Cannot Run Batch Mail Mode for Excel Document."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN NO-APPLY.
     END.

     IF v-excel OR NOT rd-dest = 5 THEN
        run run-report("", FALSE).

     IF NOT v-excel THEN
       RUN GenerateReport( ttCustList.cust-no, ttCustList.cust-no).

     ELSE IF rd-dest EQ 5 THEN /*excel*/
     DO:
        v-cust-mode = IF NOT tb_HideDialog:CHECKED THEN "Customer"
                      ELSE "Customer1".
        RUN SendMail-1 (begin_cust-no, v-cust-mode, v-dir + "\stmt.pdf").
     END.
     END. /* cust */
  END. /*end sys-ctrl-shipto not found*/


  IF CAN-FIND(FIRST ttCustList WHERE
     ttCustList.cust-no NE "" AND ttCustList.log-fld = YES ) AND rd-dest NE 5 THEN
  DO: 

     IF v-stmt-char EQ "ASIExcel" THEN
     DO:
        v-excel = YES.

        CASE rd-dest:
           WHEN 1 THEN
              LvOutputSelection = "Printer".
           WHEN 2 THEN
              LvOutputSelection = "Screen". 
           WHEN 3 THEN
              LvOutputSelection = "File". 
           WHEN 4 THEN
              LvOutputSelection = "Fax". 
           WHEN 5 THEN
              LvOutputSelection = "Email".
           WHEN 6 THEN
              LvOutputSelection = "Port".
        END CASE.
     END.
     ELSE
        v-excel = NO.

     IF v-excel AND tb_BatchMail:CHECKED THEN
     DO:
        MESSAGE "Cannot Run Batch Mail Mode for Excel Document."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN NO-APPLY.
     END.
     v-stmt-char = vcDefaultForm.
     IF v-excel OR NOT rd-dest = 5 THEN
        run run-report("", FALSE).

     IF NOT v-excel THEN
        RUN GenerateReport(begin_cust-no,end_cust-no).

     ELSE IF rd-dest EQ 5 THEN /*excel*/
     DO:
        v-cust-mode = IF NOT tb_HideDialog:CHECKED THEN "Customer"
                      ELSE "Customer1".
        RUN SendMail-1 (begin_cust-no, v-cust-mode, v-dir + "\stmt.pdf").
     END.

  END. /*end sys-ctrl-shipto not found*/

  /*current-window:WINDOW-STATE  = WINDOW-MINIMIZE. */   
  /*current-window:WINDOW-STATE  = WINDOW-Normal.*/
  STATUS DEFAULT "Processing Complete".
  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  SESSION:SET-WAIT-STATE ("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
     RUN setAttentionDefault.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_contact
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_contact C-Win
ON ENTRY OF fi_contact IN FRAME FRAME-A /* Attention */
DO:
  IF begin_cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE END_cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN DO:
    MESSAGE "Attention line can only be changed when statement is run for a single customer."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
     fi_contact:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
     fi_contact = "".
     APPLY 'entry' TO begin_cust-no.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_contact C-Win
ON LEAVE OF fi_contact IN FRAME FRAME-A /* Attention */
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

    RUN WINDOWS/l-fonts.w ({&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
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
  RUN SetEmailBoxes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stmt-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stmt-date C-Win
ON LEAVE OF stmt-date IN FRAME FRAME-A /* Statement Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stmt-msg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stmt-msg C-Win
ON LEAVE OF stmt-msg IN FRAME FRAME-A /* Statement Message */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_BatchMail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_BatchMail C-Win
ON VALUE-CHANGED OF tb_BatchMail IN FRAME FRAME-A /* Batch Mail */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_curr-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_curr-bal C-Win
ON VALUE-CHANGED OF tb_curr-bal IN FRAME FRAME-A /* Include customers with 0 current balance? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
  assign {&self-name}.
  EMPTY TEMP-TABLE ttCustList.
  RUN SetCustRange(INPUT tb_cust-list).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_emailpdf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_emailpdf C-Win
ON VALUE-CHANGED OF tb_emailpdf IN FRAME FRAME-A /* Email PDF (PDFCamp) */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_HideDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_HideDialog C-Win
ON VALUE-CHANGED OF tb_HideDialog IN FRAME FRAME-A /* Hide Dialog-Box */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_past-due
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_past-due C-Win
ON VALUE-CHANGED OF tb_past-due IN FRAME FRAME-A /* Print Past Due Only? */
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

  stmt-date = today.

  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "AR4",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .
  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    ASSIGN stmt-date = today
           stmt-date:SCREEN-VALUE = STRING(TODAY).

    find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "STMTPRINT"
    no-lock no-error.
    if not avail sys-ctrl then
    do transaction:
       create sys-ctrl.
       assign
          sys-ctrl.company = cocode
          sys-ctrl.name    = "STMTPRINT"
          sys-ctrl.descrip = "Print Statement Headers on Statement Form?".
       message sys-ctrl.descrip
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE sys-ctrl.log-fld.
    end.

    ASSIGN
       v-print-hdr = sys-ctrl.log-fld
       v-use-cust  = sys-ctrl.char-fld eq "CUST S"
       v-stmt-char = sys-ctrl.char-fld
       vcDefaultForm = v-stmt-char.

    IF (v-stmt-char EQ "" OR v-stmt-char EQ "ASI") AND
       lookup("PDFCamp Printer",SESSION:GET-PRINTERS()) GT 0 THEN
       v-pdf-camp = YES.
    IF (v-stmt-char EQ "Protagon" OR v-stmt-char = "Soule" OR v-stmt-char = "StdStatement10" OR v-stmt-char = "SouleMed") THEN DO:
        fi_contact:HIDDEN = NO.
        RUN setAttentionDefault.
    END.
    ELSE
        fi_contact:HIDDEN = YES.

    RUN SetEmailBoxes.

    FIND FIRST users WHERE
         users.user_id EQ USERID("NOSWEAT")
         NO-LOCK NO-ERROR.

    IF AVAIL users AND users.user_program[2] NE "" THEN
       v-dir = users.user_program[2].
    ELSE
       v-dir = "c:\tmp".

    APPLY "entry" TO stmt-date.
  END.
  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'AR4',
                          INPUT NO,
                          OUTPUT glCustListActive).
{sys/inc/chblankcust.i ""AR4""}

  IF ou-log THEN DO:
      ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
        tb_cust-list = YES 
        .
      RUN SetCustRange(INPUT tb_cust-list).
  END.
  ELSE
      ASSIGN
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        .
   IF ou-log AND ou-cust-int = 0 THEN do:
       ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
        tb_cust-list = NO
        .
      RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
   END.



  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BatchMail C-Win 
PROCEDURE BatchMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAM icBegCustNo  AS CHAR NO-UNDO.
  DEFINE INPUT PARAM icEndCustNo  AS CHAR NO-UNDO.
  DEFINE INPUT PARAM icBatchMode  AS LOG  NO-UNDO.

  FOR EACH b1-cust NO-LOCK
     WHERE b1-cust.company = cocode
       AND b1-cust.cust-no GE icBegCustNo
       AND b1-cust.cust-no LE icEndCustNo:

     IF icBatchMode THEN
     DO:
        vlSkipRec = YES.

        FOR EACH phone    NO-LOCK 
           WHERE phone.table_rec_key     = b1-cust.rec_key:

          IF CAN-FIND (FIRST emaildtl NO-LOCK 
                       WHERE emaildtl.emailcod  BEGINS 'R-STMT'
                         AND emaildtl.table_rec_key  = phone.rec_key) THEN 
          DO:

            vlSkipRec = NO.
            LEAVE.
          END.
        END.

        IF vlSkipRec THEN NEXT.
     END.

     EMPTY TEMP-TABLE tt-inv.

     RUN run-report-mail (b1-cust.cust-no).

     IF NOT CAN-FIND (FIRST tt-inv) THEN NEXT.

     STATUS DEFAULT 'Now processing CUST: ' + b1-cust.cust-no + '....'.

     RUN GenerateMail.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildCustList C-Win 
PROCEDURE BuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Builds the temp table of customers   
  Parameters:  Company Code, Customer list logical and/or customer range
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplList AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginCust AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndCust AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-cust FOR cust.

DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.

IF iplList THEN DO:
    RUN sys/ref/CustList.p (INPUT ipcCompany,
                            INPUT 'AR4',
                            INPUT YES,
                            OUTPUT lActive).
    FOR EACH ttCustList NO-LOCK 
         WHERE ttCustList.log-fld EQ YES :
         CREATE tt-cust-check .
        ASSIGN tt-cust-check.cust-no = ttCustList.cust-no 
           tt-cust-check.log-fld = YES .
    END.
END.
ELSE DO:
    FOR EACH bf-cust
        WHERE bf-cust.company EQ ipcCompany
          AND bf-cust.cust-no GE ipcBeginCust
          AND bf-cust.cust-no LE ipcEndCust
        NO-LOCK:
        CREATE ttCustList.
        ASSIGN 
            ttCustList.cust-no = bf-cust.cust-no
            ttCustList.log-fld = YES
        .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
/*------------------------------------------------------------------------------
  Purpose:  Display a UI of selected customers   
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
                                  INPUT 'AR4').
    FOR EACH ttCustList NO-LOCK 
         WHERE ttCustList.log-fld EQ YES : 
         CREATE tt-cust-check .
        ASSIGN tt-cust-check.cust-no = ttCustList.cust-no 
           tt-cust-check.log-fld = YES .
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
  DISPLAY stmt-date tb_cust-list begin_cust-no end_cust-no stmt-msg fi_contact 
          lbl_detailed tb_detailed lbl_past-due tb_past-due lbl_curr-bal 
          tb_curr-bal tb_HideDialog rd-dest tb_BatchMail tb_emailpdf 
          lines-per-page lv-ornt lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 stmt-date tb_cust-list btnCustList begin_cust-no 
         end_cust-no stmt-msg fi_contact tb_detailed tb_past-due tb_curr-bal 
         tb_HideDialog rd-dest tb_BatchMail tb_emailpdf lines-per-page lv-ornt 
         lv-font-no td-show-parm btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateMail C-Win 
PROCEDURE GenerateMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-lv-ornt AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

     IF is-xprint-form THEN DO:

      RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

      IF tb_HideDialog:CHECKED THEN RUN SendMail-1 (b1-cust.cust-no, 'Customer1', list-name + '.pdf').
                               ELSE RUN SendMail-1 (b1-cust.cust-no, 'Customer',  list-name + '.pdf').
    END.

    ELSE DO:

      /*Print PDF attachment*/
      IF (v-stmt-char EQ "" OR v-stmt-char EQ "ASI") THEN
      DO:
         IF tb_emailpdf:CHECKED THEN
         DO:
            RUN custom/printaspdf.p(INPUT list-name,
                                    INPUT INT(lv-font-no),
                                    INPUT lv-ornt).

            OS-COMMAND SILENT VALUE("copy /y " + v-dir + "\asi.pdf " + v-dir + "\statement.pdf").

            list-name = v-dir + "\statement.pdf".
         END.
         ELSE
         DO:
            OS-COMMAND SILENT VALUE("copy /y " + list-name + " " + list-name + ".txt").
            list-name = list-name + ".txt".
         END.
      END.

      IF tb_HideDialog:CHECKED THEN RUN SendMail-1 (b1-cust.cust-no, 'Customer1', list-name).
                               ELSE RUN SendMail-1 (b1-cust.cust-no, 'Customer',  list-name).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateReport C-Win 
PROCEDURE GenerateReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-cust-from AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-cust-to AS CHAR NO-UNDO.

   case rd-dest:
         when 1 then run output-to-printer.
         when 2 then run output-to-screen.
         when 3 then run output-to-file.
         when 4 then do:
             /*run output-to-fax.*/
             IF is-xprint-form THEN DO:
                RUN output-to-fax-prt. /* create tif file */              
                {custom/asifaxm3.i &TYPE="customer"
                              &begin_cust=ip-cust-from
                              &END_cust=ip-cust-to
                              &fax-subject="Statement"
                              &fax-body="statement"
                              &fax-file=lv-fax-image
                              &end-widget=end_cust-no}      
             END.
             ELSE DO:
                {custom/asifax3.i &TYPE = "CUSTOMER"
                      &begin_cust=ip-cust-from
                      &END_cust=ip-cust-to
                      &fax-subject=c-win:title
                      &fax-body=c-win:title
                      &fax-file=list-name
                      &end-widget=end_cust-no }
             END.     
         END.
         WHEN 5 THEN
            RUN output-to-mail(INPUT ip-cust-from, INPUT ip-cust-to).

         WHEN 6 THEN run output-to-port.
    end case.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-fax-prt C-Win 
PROCEDURE output-to-fax-prt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-file-name AS cha FORM "x(60)" NO-UNDO.
  DEF VAR lv-xpr-file AS cha FORM "x(60)" NO-UNDO.

  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     lv-xpr-file = FILE-INFO:FULL-PATHNAME.

     RUN printfile (lv-xpr-file).
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
 {custom/out2file.i} 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-mail C-Win 
PROCEDURE output-to-mail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-cust-no-from AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-cust-no-to AS CHAR NO-UNDO.

  IF NOT tb_BatchMail:CHECKED IN FRAME {&FRAME-NAME} THEN
     RUN BatchMail (ip-cust-no-from, ip-cust-no-from, NO).
  ELSE
     RUN BatchMail (ip-cust-no-from, ip-cust-no-to, YES).

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
  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt). 

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
 IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
 END.
 ELSE run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-asistmt C-Win 
PROCEDURE run-asistmt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

DEF VAR v-first AS LOG NO-UNDO.
DEF VAR v-remitto AS cha FORM "x(50)" EXTENT 4 NO-UNDO.

{sys/form/r-top.f}

def var v-stmt-date     as date format "99/99/9999" no-undo label "Statement Date".
def var v-lo-cust like cust.cust-no label "From Customer#" no-undo.
def var v-hi-cust  like cust.cust-no label "Thru Customer#" no-undo.
def var v-msg as char no-undo format 'x(40)' label "Statement Message".
def var v-detail as log format "yes/no" label "Print Detail?" no-undo.
def var v-past-due as Log no-undo format "yes/no" label "Print Past Due Only?".

def var xx as int no-undo.
def var yy as int no-undo.
def var zz as int no-undo.
def var v-print-align   as log format "Y/N" no-undo.
def var v-align-ok      as log format "Y/N" no-undo.
def var save_id         as recid no-undo.
def var v-balance as dec label "Balance" format '->>,>>>,>>>.99CR' NO-UNDO.
def var v-age as int no-undo. /* number of days old */
def var v-per as int no-undo. /* hash of v-age into aging periods */
def var v-aged as dec no-undo extent 5 format ">>,>>>,>>>.99CR" .   /* aging buckets */
def var v-days-in-per as int no-undo init 30.
def var ln-total as int no-undo init 48.
def var adv as int no-undo.
def var ws_letterhead as char format 'x(80)' no-undo extent 6.
def var ws_addr as char format 'x(35)' no-undo extent 6.
def var code-legend as char format 'X(80)' no-undo.
DEF VAR v-asi-excel AS LOG NO-UNDO.

DEF VAR v-last-amt  AS DECI NO-UNDO.
DEF VAR v-last-ref# AS CHAR NO-UNDO.
DEF VAR v-last-paydate AS DATE NO-UNDO.

DEF VAR ld-due AS DEC NO-UNDO.
DEF VAR terms_dscr AS CHAR FORMAT "x(30)" NO-UNDO .

/* 07.11.95 by CAH @ASI:
1.  There is no ar transaction type file in system, so the following
vars have been added to support structured definition via lookups.
*/
def var msgx as int no-undo.
def var v-inv-type-descr as char format 'x(30)' no-undo.
def var v-inv-type-list as char no-undo init "I,CR,DR,P,DA,FC,R".
def var v-inv-type-max  as int no-undo.
v-inv-type-max = num-entries(v-inv-type-list).
def var v-inv-type-array as char no-undo extent 7 init
  ["Invoice",
  "CR Memo",
  "DR Memo",
  "Payment",
  "Disc Allowed",
  "Finance Chg",
  "Return"].

code-legend = "CODES: ".
do xx = 1 to v-inv-type-max:
  code-legend = code-legend + entry(xx, v-inv-type-list) + '-'
    + v-inv-type-array[xx] + ' '.
end.

v-asi-excel = v-stmt-char EQ "ASIExcel".

form
  ws_letterhead[1]    skip
  ws_letterhead[2]    skip
  ws_letterhead[3]    skip
  ws_letterhead[4]    skip
  ws_letterhead[5]    skip(1)
  skip(5)
  ws_addr[1]    at 11 /*was 3*/
  "Statement date" at 50   "Account#" at 65 skip
  ws_addr[2]    at 11
  "--------------" at 50   "--------" at 65 skip
  ws_addr[3]    at 11
  v-stmt-date      at 53    cust.cust-no at 65 skip
  ws_addr[4]    at 11 skip
  ws_addr[5]    at 11 skip
  skip(1)
  "=============================== S T A T E M E N T ============================"
  skip
  with frame stmt-header no-box no-labels stream-io width 80.

form
  tt-inv.trans-date 
  tt-inv.type FORM "x(3)"
  tt-inv.inv-no      
  tt-inv.description FORM "x(25)"
  tt-inv.old-day format ">>>9"
  tt-inv.amount      
  v-balance       
  with frame stmt-line no-box stream-io width 85 DOWN NO-LABEL.

form
  v-msg at 15
  v-balance  at 63
  with frame stmt-total-line no-box no-labels stream-io.

form
  ws_letterhead[1]    skip
  ws_letterhead[2]    skip
  ws_letterhead[3]    skip
  ws_letterhead[4]    skip
  ws_letterhead[5]    skip(1)
  skip(5)
  ws_addr[1]    at 11 /*3*/
  ws_addr[2]    at 11
  ws_addr[3]    at 11
  v-stmt-date      at 53    cust.cust-no at 65 skip
  ws_addr[4]    at 11 skip
  ws_addr[5]    at 11 skip
  skip(4)
  skip
  with frame no-stmt-header no-box no-labels stream-io width 80.

form
  tt-inv.trans-date
  tt-inv.type
  tt-inv.inv-no 
  tt-inv.DESCRIPTION FORM "x(25)"
  tt-inv.amount
  v-balance
  with frame no-stmt-line no-box no-labels stream-io width 80 down.

form
  v-msg at 15
  v-balance  at 63
  with frame no-stmt-total-line no-box no-labels stream-io.

form
  skip(3)
  skip
  v-aged[1 for 5] skip(1)
  code-legend skip
  with frame no-stmt-total no-box no-labels stream-io width 80.

DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.

find first company where company.company eq cocode no-lock no-error.

IF v-stmt-char = "Badger" THEN do:
    IF company.company EQ "003" THEN
        ASSIGN ls-image1 =  "images\Badger_CA.jpg" 
            FILE-INFO:FILE-NAME = ls-image1
            ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
    ELSE
        ASSIGN ls-image1 =  "images\badger statement.JPG" 
            FILE-INFO:FILE-NAME = ls-image1
            ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
END.
ELSE do:
ASSIGN ls-image1 = IF v-stmt-char = "Premier" THEN "images\premierinv.jpg"
                   ELSE IF v-stmt-char = "LoyLang" THEN "images\loystmt.jpg"
                   ELSE IF v-stmt-char = "Printers" THEN "images\loyprinters.jpg"
                 /*  ELSE IF v-stmt-char = "Badger" THEN "images\badger statement.JPG" */
                   ELSE IF v-stmt-char = "RFC" THEN "images\RFC.JPG"
                   ELSE "images\asilogo.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
END.

if v-use-cust then
   find first cust WHERE
        cust.company eq cocode AND
        cust.active  eq "S"
        no-lock no-error.

if v-print-hdr and avail company then do:
  yy = 1.

  if company.name    ne "" then
    assign
     ws_letterhead[yy] = company.name
     yy                = yy + 1.

  if company.addr[1] ne "" then
    assign
     ws_letterhead[yy] = company.addr[1]
     yy                = yy + 1.

  if company.addr[2] ne "" then
    assign
     ws_letterhead[yy] = company.addr[2]
     yy                = yy + 1.

  if company.city    ne "" or
     company.state   ne "" or
     company.zip     ne "" then
    assign
     ws_letterhead[yy] = company.city + ', ' + company.state
                         + '  ' + company.zip
     yy                = yy + 1.

  do xx = 1 to 6:
    if ws_letterhead[xx] gt '' then
      ws_letterhead[xx] = fill(" ", int((80 - length(ws_letterhead[xx])) / 2))
                          + ws_letterhead[xx].
  end.
end.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 56}

 v-stmt-date = stmt-date
 v-msg       = stmt-msg
 v-detail    = tb_detailed
 v-past-due  = tb_past-due.

IF ip-sys-ctrl-shipto THEN
   ASSIGN
      v-lo-cust = ip-cust-no
      v-hi-cust = ip-cust-no.
ELSE
   ASSIGN
      v-lo-cust = ""
      v-hi-cust = "" .

{sys/inc/print1.i}

{sys/inc/outprint.i  value(lines-per-page)}

if td-show-parm then run show-param.

IF NOT v-asi-excel THEN
   is-xprint-form = YES.
ELSE
   is-xprint-form = NO.

IF is-xprint-form THEN DO:
   CASE rd-dest :
        WHEN 1 THEN PUT "<PRINTER?>" FORM "x(30)".
        WHEN 2 THEN do:
            IF NOT lBussFormModle THEN        
              PUT "<PREVIEW><MODAL=NO>" FORM "x(30)". 
            ELSE
              PUT "<PREVIEW>" FORM "x(30)".
        END.
        WHEN 4 THEN do:
              ls-fax-file = "c:\tmp\fx" + STRING(TIME) + ".tif".
              PUT UNFORMATTED "<PRINT=NO><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
        END.        
        WHEN 5 THEN DO:
            IF NOT tb_BatchMail:CHECKED IN FRAME {&FRAME-NAME} THEN
               PUT "<PREVIEW><PDF-LEFT=3mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".              
            ELSE
               PUT "<PREVIEW=PDF><PDF-LEFT=3mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".
        END.
   END CASE.
   PUT "</PROGRESS>".
END.


SESSION:SET-WAIT-STATE ("general").
v-first = YES.

EMPTY TEMP-TABLE tt-inv.
EMPTY TEMP-TABLE tt-cust-excel.

FOR EACH ttCustList
    WHERE ttCustList.log-fld
    NO-LOCK,
    FIRST cust no-lock
        WHERE cust.company eq cocode
          AND cust.cust-no EQ ttCustList.cust-no
           AND (cust.cust-no EQ v-lo-cust OR v-lo-cust = "")
           AND (cust.cust-no EQ v-hi-cust  OR v-hi-cust = "")
           AND ((cust.acc-bal ne 0 AND NOT tb_curr-bal) OR (tb_curr-bal))
        BREAK BY cust.cust-no
    transaction:

  IF NOT v-asi-excel THEN
     EMPTY TEMP-TABLE tt-inv.

  ASSIGN v-last-amt  = 0
         v-last-ref# = ""
         v-last-paydate = ?.

  if v-past-due then
  do:
    find first ar-inv where ar-inv.company eq cust.company    and
                            ar-inv.cust-no eq cust.cust-no and
                            ar-inv.posted                and
                            ar-inv.due ne 0              and
                            ar-inv.inv-date le v-stmt-date and
                            ar-inv.due-date le v-stmt-date no-lock no-error.
    if not avail ar-inv THEN next.
  end.

  for each ar-inv
      where ar-inv.company  eq cust.company
        and ar-inv.cust-no  eq cust.cust-no
        and ar-inv.posted   eq yes
        /*and ar-inv.due      ne 0           */
        and ar-inv.terms    ne "CASH"      
        and ar-inv.inv-date le v-stmt-date
      no-lock:

    FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no AND ar-invl.po-no <> "" USE-INDEX X-no NO-LOCK NO-ERROR.

    if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
      ld-due = ar-inv.net.
    else
      ld-due = ar-inv.gross.

    for each ar-cashl
        where ar-cashl.company  eq ar-inv.company
          and ar-cashl.posted   eq yes  
          and ar-cashl.cust-no  eq ar-inv.cust-no
          and ar-cashl.inv-no   eq ar-inv.inv-no
        use-index inv-no no-lock,

        each ar-cash
        where ar-cash.c-no       eq ar-cashl.c-no
          and ar-cash.check-date le v-stmt-date
        use-index c-no no-lock:

      if ar-cashl.memo THEN
        if ar-cashl.amt-disc ne 0 then
          ld-due = ld-due - ar-cashl.amt-disc.
        else
        if ar-cashl.amt-paid + ar-cashl.amt-disc gt 0 then
          ld-due = ld-due + (ar-cashl.amt-paid + ar-cashl.amt-disc).
        else
          ld-due = ld-due + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
      else
        ld-due = ld-due + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
    end.

    IF ld-due NE 0 THEN DO:
        FIND FIRST tt-inv 
            WHERE tt-inv.cust-no EQ cust.cust-no
              AND tt-inv.DESCRIPTION EQ "No Balance Due"
            NO-ERROR.
        IF AVAIL tt-inv THEN DELETE tt-inv.
      create tt-inv.
      assign
       tt-inv.cust-no    = cust.cust-no
       tt-inv.sort-fld   = "0" + STRING(ar-inv.inv-no,"9999999999") + "0"
       tt-inv.inv-date   = ar-inv.inv-date
       tt-inv.trans-date = ar-inv.inv-date
       tt-inv.old-day    = (TODAY - ar-inv.inv-date)
       tt-inv.inv-no     = ar-inv.inv-no
       tt-inv.type       = if ar-inv.type gt ' ' then ar-inv.type else 'I'
       tt-inv.amount     = if v-detail then
                           if ar-inv.net     eq
                              ar-inv.gross   +
                              ar-inv.freight +
                              ar-inv.tax-amt then ar-inv.net else ar-inv.gross
                          else ar-inv.due
       tt-inv.po-no      = (IF AVAIL ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "")
       tt-inv.bol-no     = (IF AVAIL ar-invl AND ar-invl.bol-no <> 0 THEN string(ar-invl.bol-no,">>>>>>>>") ELSE "").

     IF v-stmt-char = "Printers" THEN
          tt-inv.bol-no = (IF AVAIL ar-invl AND ar-invl.job-no NE "" 
                                THEN "  " + ar-invl.job-no
                           ELSE IF AVAIL ar-invl AND ar-invl.ord-no <> 0 
                                THEN string(ar-invl.ord-no,">>>>>>>>") 
                           ELSE "").
      if v-detail then
      for each ar-cashl
          where ar-cashl.company  eq ar-inv.company
            and ar-cashl.posted   eq yes
            and ar-cashl.cust-no  eq ar-inv.cust-no
            and ar-cashl.inv-no   eq ar-inv.inv-no
          use-index inv-no no-lock,

          each ar-cash
          where ar-cash.c-no       eq ar-cashl.c-no
            and ar-cash.check-date le v-stmt-date
          use-index c-no no-lock:

        create tt-inv.
        assign
         tt-inv.cust-no     = cust.cust-no
         tt-inv.sort-fld    = "0" + STRING(ar-cashl.inv-no,"9999999999") + "1"
         tt-inv.inv-date    = ar-inv.inv-date
         tt-inv.old-day    = (TODAY - ar-inv.inv-date)
         tt-inv.trans-date  = ar-cash.check-date
         tt-inv.inv-no      = ar-cashl.inv-no
         tt-inv.description = ar-cashl.dscr
         tt-inv.po-no       = (IF AVAIL ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "")
         tt-inv.bol-no     = (IF AVAIL ar-invl AND ar-invl.bol-no <> 0 THEN string(ar-invl.bol-no,">>>>>>>>") ELSE "").

        IF v-stmt-char = "Printers" THEN
          tt-inv.bol-no = (IF AVAIL ar-invl AND ar-invl.job-no NE "" 
                                THEN "  " + ar-invl.job-no 
                           ELSE IF AVAIL ar-invl AND ar-invl.ord-no <> 0 
                                THEN string(ar-invl.ord-no,">>>>>>>>") 
                           ELSE "").

        if ar-cashl.memo then
          if ar-cashl.amt-disc ne 0 then
            assign
             tt-inv.type   = "R"
             tt-inv.amount = ar-cashl.amt-disc * -1.

          else  
          if ar-cashl.amt-paid + ar-cashl.amt-disc le 0 then
            assign
             tt-inv.type   = "CM"
             tt-inv.amount = ar-cashl.amt-paid + ar-cashl.amt-disc.

          else
            assign
             tt-inv.type   = "DM"
             tt-inv.amount = ar-cashl.amt-paid - ar-cashl.amt-disc.

        else
          assign
           tt-inv.type   = "P"
           tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1.

      end.
    END.
    ELSE IF tb_curr-bal THEN DO:
        IF NOT CAN-FIND(FIRST tt-inv WHERE tt-inv.cust-no EQ cust.cust-no
                        AND tt-inv.DESCRIPTION EQ "No Balance Due")
        THEN DO: 
            CREATE tt-inv.
            ASSIGN
                tt-inv.cust-no = cust.cust-no
                tt-inv.DESCRIPTION = "No Balance Due"
                .
        END.
    END.
  end.

  for each ar-cashl
      where ar-cashl.company    eq cust.company
        and ar-cashl.cust-no    eq cust.cust-no
        and ar-cashl.inv-no     eq 0
        and (ar-cashl.inv-date  le v-stmt-date or
             ar-cashl.inv-date  eq ?)
        and ar-cashl.posted     eq yes
        and ar-cashl.on-account eq YES
        and ar-cashl.amt-paid   ne 0
      no-lock,

      each ar-cash
      where ar-cash.c-no       eq ar-cashl.c-no
        and ar-cash.check-date le v-stmt-date
      use-index c-no no-lock:

    create tt-inv.
    assign
     tt-inv.cust-no     = cust.cust-no
     tt-inv.sort-fld    = "1" + STRING(ar-cashl.inv-no,"9999999999")
     tt-inv.inv-date    = ar-cashl.inv-date
     tt-inv.old-day    = (TODAY - ar-cashl.inv-date)
     tt-inv.trans-date  = ar-cash.check-date
     tt-inv.inv-no      = ar-cashl.inv-no
     tt-inv.description = ar-cashl.dscr
     tt-inv.po-no       = "" /*(IF AVAIL ar-invl THEN ar-invl.po-no ELSE "")*/ /* on-account dont display PO# */
     tt-inv.bol-no     = (IF AVAIL ar-invl AND ar-invl.bol-no <> 0 THEN string(ar-invl.bol-no,">>>>>>>>") ELSE "").

    if ar-cashl.memo then
      assign
       tt-inv.amount = ar-cashl.amt-paid
       tt-inv.type   = if tt-inv.amount lt 0 then "CR" else "DR".

    else
      assign
       tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1
       tt-inv.type   = "P".

    /*IF tt-inv.TYPE = "P" THEN
       IF tt-inv.trans-date > v-last-paydate OR v-last-paydate = ? THEN 
          ASSIGN v-last-amt     = tt-inv.amount
                 v-last-ref#    = string(tt-inv.inv-no)
                 v-last-paydate = tt-inv.trans-date.  
    */                 
  end.                                                

  /* to get last payment amt, check, date */
  for each ar-cashl
      where ar-cashl.company    eq cust.company
        and ar-cashl.cust-no    eq cust.cust-no
        and (ar-cashl.inv-date  le v-stmt-date or
             ar-cashl.inv-date  eq ?)
        and ar-cashl.posted     eq yes
        /*and ar-cashl.on-account eq YES*/
        and ar-cashl.amt-paid   ne 0
        AND ar-cashl.memo       EQ NO
      no-lock,

      each ar-cash
      where ar-cash.c-no       eq ar-cashl.c-no
        and ar-cash.check-date le v-stmt-date
      use-index c-no no-lock:

      IF ar-cash.check-date > v-last-paydate OR v-last-paydate = ? THEN 
         ASSIGN v-last-amt     = ar-cashl.amt-paid
                v-last-ref#    = string(ar-cash.check-no)
                v-last-paydate = ar-cash.check-date.  
      ELSE
      IF v-last-ref#  = string(ar-cash.check-no) AND v-last-ref# <> "" THEN
         ASSIGN v-last-amt     = v-last-amt + ar-cashl.amt-paid.
  END.

  ASSIGN
  v-balance = 0 /* reset running balance */
  v-aged = 0. /* clear aging buckets */
  clear frame stmt-header no-pause.
  clear frame stmt-line all no-pause.
  clear frame stmt-total no-pause.
  ws_addr = ''.
  if avail cust then
  assign
    ws_addr[1] = cust.name
    ws_addr[2] = cust.addr[1]
    ws_addr[3] = cust.addr[2]
    ws_addr[4] = cust.city + ', ' + cust.state + '  ' + cust.zip.
  do yy = 1 to 6:
    do zz = yy + 1 to 6:
      if ws_addr[yy] eq '' and ws_addr[zz] gt ''
        then
      assign ws_addr[yy] = ws_addr[zz] ws_addr[zz] = ''.
    end.
  end.

  for each tt-inv WHERE (tt-inv.amount NE 0 OR tt-inv.DESCRIPTION EQ "No Balance Due")
      break by "1"
            BY tt-inv.cust-no
            by tt-inv.inv-date
            by tt-inv.sort-fld
            by tt-inv.trans-date:

    IF v-asi-excel AND tt-inv.cust-no NE cust.cust-no THEN
       NEXT.

    IF AVAIL cust THEN
    FIND FIRST terms WHERE terms.company EQ cocode
        AND terms.t-code EQ cust.terms NO-LOCK NO-ERROR .
    IF AVAIL terms THEN
        ASSIGN terms_dscr = terms.dscr .

    if NOT v-asi-excel AND (first-of ("1") or (line-counter gt ln-total)) then do:
       IF NOT v-first THEN page.
       IF v-stmt-char = "Premier" THEN
          ASSIGN v-remitto[1] = "<C40>Remit To: PREMIER PACKAGING"
                 v-remitto[2] = "<C40>          3254 RELIABLE PARKWAY"
                 v-remitto[3] = "<C40>          CHICAGO, IL 60686".
       ELSE v-remitto = "".

       IF v-stmt-char = "Premier" THEN
       PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
           "<=1><R+6><C+54><B><P22>Statement</B><P12>" SKIP
           "<=1><R+9><C+52><FROM><C+13><LINE>" SKIP
           "<=1><R+9><C+68><FROM><C+10><LINE>" 
           "<=1><R+9><C+52>" v-stmt-date
           "<=1><R+9><C+68>" cust.cust-no SKIP
           "<=1><R+11><C1>" ws_addr[1] v-remitto[1] skip
           "<=1><R+12><C1>" ws_addr[2] v-remitto[2] skip 
           "<=1><R+13><C1>" ws_addr[3] v-remitto[3] skip
           "<=1><R+14><C1>" ws_addr[4] v-remitto[4] skip
           /*"<=1><R+15><C1>" ws_addr[5] skip*/
           "<=1><R+15><C1>Terms : " terms_dscr skip
           "<=1><R+17>Date     Code  Ref#  Description   <C56>Amount        Balance" SKIP
           "<=1><R+18><FROM><C+80><LINE>"
           . 
       ELSE IF v-stmt-char = "LoyLang" THEN
       PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
           "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
           "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
           "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
           "<=1><R+10><C+68><FROM><C+10><LINE>" 
           "<=1><R+10><C+52>" v-stmt-date
           "<=1><R+10><C+68>" cust.cust-no SKIP
           "<=1><R+11><C1>" ws_addr[1] skip
           "<=1><R+12><C1>" ws_addr[2] v-remitto[1] 
           "<C40>Last Payment $Amt Date     Check/Ref#" SKIP
           "<=1><R+13><C1>" ws_addr[3] v-remitto[2] SKIP
           "<=1><R+13><C40><FROM><C+17><LINE>"
           "<=1><R+13><C58><FROM><C+8><LINE>" 
           "<=1><R+13><C67><FROM><C+10><LINE>"      skip 
           "<=1><R+13><C40>" v-last-amt 
           "<=1><R+13><C58>" v-last-paydate
           "<=1><R+13><C69>" v-last-ref#            skip
           "<=1><R+14><C1>" ws_addr[4] v-remitto[3] skip
           "<=1><R+15><C1>" ws_addr[5] v-remitto[4] skip
           "<=1><R+17>         Trans"               SKIP
           "<=1><R+18>Date     Type   Ref#     BOL#   Customer PO#    <C56>Amount        Balance" SKIP
           "<=1><R+19><FROM><C+80><LINE>"
           .
       ELSE IF v-stmt-char = "Printers" THEN
       PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
           "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
           "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
           "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
           "<=1><R+10><C+68><FROM><C+10><LINE>" 
           "<=1><R+10><C+52>" v-stmt-date
           "<=1><R+10><C+68>" cust.cust-no SKIP
           "<=1><R+11><C1>" ws_addr[1] skip
           "<=1><R+12><C1>" ws_addr[2] v-remitto[1] 
           "<C40>Last Payment $Amt Date     Check/Ref#" SKIP
           "<=1><R+13><C1>" ws_addr[3] v-remitto[2] SKIP
           "<=1><R+13><C40><FROM><C+17><LINE>"
           "<=1><R+13><C58><FROM><C+8><LINE>" 
           "<=1><R+13><C67><FROM><C+10><LINE>"      skip 
           "<=1><R+13><C40>" v-last-amt 
           "<=1><R+13><C58>" v-last-paydate
           "<=1><R+13><C69>" v-last-ref#            skip
           "<=1><R+14><C1>" ws_addr[4] v-remitto[3] skip
           "<=1><R+15><C1>" ws_addr[5] v-remitto[4] skip
           "<=1><R+17>         Trans"               SKIP
           "<=1><R+18>Date     Type   Inv#     Job#   Customer PO#    <C56>Amount        Balance" SKIP
           "<=1><R+19><FROM><C+80><LINE>"
           . 
       ELSE IF v-stmt-char = "Badger" THEN
           PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
           "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
           "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
           "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
           "<=1><R+10><C+68><FROM><C+10><LINE>" 
           "<=1><R+10><C+52>" v-stmt-date
           "<=1><R+10><C+68>" cust.cust-no SKIP
           "<=1><R+11><C1>" ws_addr[1] skip
           "<=1><R+12><C1>" ws_addr[2] v-remitto[1] skip 
           "<=1><R+13><C1>" ws_addr[3] v-remitto[2] skip
           "<=1><R+14><C1>" ws_addr[4] v-remitto[3] skip
           "<=1><R+15><C1>" ws_addr[5] v-remitto[4] skip
           "<=1><R+17>Date     Code  Inv# Description               Days <C60>Amount        Balance" SKIP
           "<=1><R+18><FROM><C+82><LINE>"
           . 

       ELSE IF v-stmt-char = "RFC" THEN          /* task 12231305 */
       PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
           "<=1><R+3><C+26> 2066 S. East Avenue" 
           "<=1><R+4><C+26> Vineland, NJ 08360" 
           "<=1><R+5><C+26> Phone: 856-692-0404" 
           "<=1><R+6><C+26> Fax: (856) 692-2085" 
           "<=1><R+10><C+54><B><P22>Statement</B><P12>" SKIP
           "<=1><R+12>" "<C53>Statement Date  Account #" SKIP
           "<=1><R+13><C+52><FROM><C+13><LINE>" SKIP
           "<=1><R+13><C+68><FROM><C+10><LINE>" 
           "<=1><R+13><C+52>" v-stmt-date
           "<=1><R+13><C+68>" cust.cust-no SKIP
           "<=1><R+11><C18>Attn:" cust.contact SKIP
           "<=1><R+13><C18>" ws_addr[1] skip                    /*Task# 01031416*/
           "<=1><R+14><C18>" ws_addr[2] v-remitto[1] skip 
           "<=1><R+15><C18>" ws_addr[3] v-remitto[2] skip
           "<=1><R+16><C18>" ws_addr[4] v-remitto[3] skip
           "<=1><R+17><C18>" ws_addr[5] v-remitto[4] skip
           "<=1><R+20>Date     Code  Ref#  Description   <C56>Amount        Balance" SKIP
           "<=1><R+21><FROM><C+80><LINE>"
           . 
       ELSE
           PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
           "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
           "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
           "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
           "<=1><R+10><C+68><FROM><C+10><LINE>" 
           "<=1><R+10><C+52>" v-stmt-date
           "<=1><R+10><C+68>" cust.cust-no SKIP
           "<=1><R+11><C1>" ws_addr[1] skip
           "<=1><R+12><C1>" ws_addr[2] v-remitto[1] skip 
           "<=1><R+13><C1>" ws_addr[3] v-remitto[2] skip
           "<=1><R+14><C1>" ws_addr[4] v-remitto[3] skip
           "<=1><R+15><C1>" ws_addr[5] v-remitto[4] skip
           "<=1><R+17>Date     Code  Ref#  Description   <C56>Amount        Balance" SKIP
           "<=1><R+18><FROM><C+80><LINE>"
           . 

       v-first = NO.
    end.

    if tt-inv.description eq '' then do:
      msgx = lookup(tt-inv.type,v-inv-type-list).
      if msgx eq 0 then
      msgx = 1.    /* assume invoice */
      tt-inv.description =
      if msgx gt 0 and msgx le v-inv-type-max
        then
      v-inv-type-array[msgx]
      else
      ''.
    end.

    v-balance = v-balance + tt-inv.amount.

    IF NOT v-asi-excel THEN
    DO:
        if v-stmt-char = "Badger" then do:
           display
             tt-inv.trans-date
             tt-inv.type
             tt-inv.inv-no  when tt-inv.inv-no gt 0                                        
            (IF v-stmt-char = "LoyLang" OR v-stmt-char = "Printers" THEN tt-inv.bol-no + " " + string(tt-inv.po-no) ELSE tt-inv.description) @ tt-inv.description
            /*(IF v-stmt-char = "LoyLang" THEN string(tt-inv.po-no) ELSE tt-inv.description) @ tt-inv.description*/
             tt-inv.old-day
             tt-inv.amount
             v-balance 
             with frame stmt-line .
           down 1 with frame stmt-line.
       end.
       else do:
           display
             tt-inv.trans-date
             tt-inv.type
             tt-inv.inv-no  when tt-inv.inv-no gt 0
             /*tt-inv.description*/
             (IF v-stmt-char = "LoyLang" OR v-stmt-char = "Printers" THEN tt-inv.bol-no + " " + string(tt-inv.po-no) ELSE tt-inv.description) @ tt-inv.description
             /*(IF v-stmt-char = "LoyLang" THEN string(tt-inv.po-no) ELSE tt-inv.description)*/
             tt-inv.amount
             v-balance
             with frame no-stmt-line.
           down 1 with frame no-stmt-line.
       end.
    END.

    v-age = v-stmt-date - tt-inv.inv-date.
    if v-age = ? or v-age lt 0 then v-age = 0.
    if v-stmt-char = "Badger" then 
        v-per = trunc(v-age / (v-days-in-per + 1), 0) + 1.
    else        
        v-per = trunc(v-age / v-days-in-per, 0) + 1.
    if v-per gt 5 then
       v-per = 5.
    v-aged[v-per] = v-aged[v-per] + tt-inv.amount.

    if last-of ("1") then do:

      IF NOT v-asi-excel THEN
      DO:
         PUT SKIP(1).

         if v-print-hdr then
         display
           v-msg
           v-balance
           with frame stmt-total-line.

         else
         display
           v-msg
           v-balance
           with frame no-stmt-total-line.

         IF v-stmt-char = "Badger" THEN 
         PUT "<R57><C1><#2>"SKIP
         "<=2>      Current             31 - 60             61 - 90            >90 Days" skip
         "<=2><R+1.3><FROM><C+80><LINE>" SKIP
         "<=2><R+2>" v-aged[1] AT 12 v-aged[2] AT 30  v-aged[3] AT 50  (v-aged[4] + v-aged[5]) AT 70
         skip(1).
         ELSE
            PUT "<R57><C1><#2>"SKIP
         "<=2>      Current         30 Days         60 Days         90 Days        >90 Days" skip
         "<=2><R+1.3><FROM><C+80><LINE>" SKIP
         "<=2><R+2>" v-aged[1 for 5]
         skip(1).

         IF v-stmt-char = "LoyLang" OR v-stmt-char = "Printers" THEN 
         PUT "<R62><C1><#3>"SKIP
         "<=3><R+1><C1>" code-legend skip
         "<R+1><C+80><RECT#3>" 
         SKIP. 

      END.
      ELSE
      DO:
         CREATE tt-cust-excel.

         ASSIGN tt-cust-excel.cust-no = tt-inv.cust-no
                tt-cust-excel.contact = cust.contact
                tt-cust-excel.addr[1] = ws_addr[1]
                tt-cust-excel.addr[2] = ws_addr[2]
                tt-cust-excel.addr[3] = ws_addr[3]
                tt-cust-excel.addr[4] = ws_addr[4]
                tt-cust-excel.addr[5] = ws_addr[5]
                tt-cust-excel.aged[1] = v-aged[1]
                tt-cust-excel.aged[2] = v-aged[2]
                tt-cust-excel.aged[3] = v-aged[3]
                tt-cust-excel.aged[4] = v-aged[4]
                tt-cust-excel.aged[5] = v-aged[5].
         RELEASE tt-cust-excel.
      END.
    end.
  end.  /* for each tt-inv */

end. /* for each cust record */

IF v-asi-excel THEN
   RUN arrep\asiexlstmt.p(INPUT v-stmt-date, INPUT v-msg).

SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-asistmt-mail C-Win 
PROCEDURE run-asistmt-mail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAM icCustNo AS CHARACTER NO-UNDO.

DEF VAR v-first AS LOG NO-UNDO.
DEF VAR v-remitto AS cha FORM "x(50)" EXTENT 4 NO-UNDO.

{sys/form/r-top.f}

def var v-stmt-date     as date format "99/99/9999" no-undo label "Statement Date".
def var v-lo-cust like cust.cust-no label "From Customer#" no-undo.
def var v-hi-cust  like cust.cust-no label "Thru Customer#" no-undo.
def var v-msg as char no-undo format 'x(40)' label "Statement Message".
def var v-detail as log format "yes/no" label "Print Detail?" no-undo.
def var v-past-due as Log no-undo format "yes/no" label "Print Past Due Only?".

def var xx as int no-undo.
def var yy as int no-undo.
def var zz as int no-undo.
def var v-print-align   as log format "Y/N" no-undo.
def var v-align-ok      as log format "Y/N" no-undo.
def var v-print         like ar-inv.printed NO-UNDO.
def var save_id         as recid no-undo.
def var v-balance as dec label "Balance" format '->>,>>>,>>>.99CR'.
def var v-age as int no-undo. /* number of days old */
def var v-per as int no-undo. /* hash of v-age into aging periods */
def var v-aged as dec no-undo extent 5 format ">>,>>>,>>>.99CR" .   /* aging buckets */
def var v-days-in-per as int no-undo init 30.
def var ln-total as int no-undo init 48.
def var adv as int no-undo.
def var ws_letterhead as char format 'x(80)' no-undo extent 6.
def var ws_addr as char format 'x(35)' no-undo extent 6.
def var code-legend as char format 'X(80)' no-undo.

DEF VAR ld-due AS DEC NO-UNDO.
DEF VAR v-last-amt  AS DECI NO-UNDO.
DEF VAR v-last-ref# AS CHAR NO-UNDO.
DEF VAR v-last-paydate AS DATE NO-UNDO.

DEF VAR terms_dscr AS CHAR FORMAT "x(30)" NO-UNDO.

/* 07.11.95 by CAH @ASI:
1.  There is no ar transaction type file in system, so the following
vars have been added to support structured definition via lookups.
*/
def var msgx as int no-undo.
def var v-inv-type-descr as char format 'x(30)' no-undo.
def var v-inv-type-list as char no-undo init "I,CR,DR,P,DA,FC,R".
def var v-inv-type-max  as int no-undo.
v-inv-type-max = num-entries(v-inv-type-list).
def var v-inv-type-array as char no-undo extent 7 init
  ["Invoice",
  "CR Memo",
  "DR Memo",
  "Payment",
  "Disc Allowed",
  "Finance Chg",
  "Return"].

code-legend = "CODES: ".
do xx = 1 to v-inv-type-max:
  code-legend = code-legend + entry(xx, v-inv-type-list) + '-'
    + v-inv-type-array[xx] + ' '.
end.

form
  ws_letterhead[1]    skip
  ws_letterhead[2]    skip
  ws_letterhead[3]    skip
  ws_letterhead[4]    skip
  ws_letterhead[5]    skip(1)
  skip(5)
  ws_addr[1]    at 11 /*was 3*/
  "Statement date" at 50   "Account#" at 65 skip
  ws_addr[2]    at 11
  "--------------" at 50   "--------" at 65 skip
  ws_addr[3]    at 11
  v-stmt-date      at 53    cust.cust-no at 65 skip
  ws_addr[4]    at 11 skip
  ws_addr[5]    at 11 skip
  skip(1)
  "=============================== S T A T E M E N T ============================"
  skip
  with frame stmt-header no-box no-labels stream-io width 80.

form
  tt-inv.trans-date 
  tt-inv.type   FORM "x(3)"  
  tt-inv.inv-no  
  tt-inv.description FORM "x(25)"
  tt-inv.old-day format ">>>9"
  tt-inv.amount      
  v-balance       
  with frame stmt-line no-box stream-io width 85 DOWN NO-LABEL.

form
  tt-inv.trans-date 
  tt-inv.type FORM "x(3)"
  tt-inv.inv-no      
  tt-inv.description FORM "x(20)"
  tt-inv.old-day format ">>>9"
  tt-inv.amount      
  v-balance       
  with frame stmt-line-badger no-box stream-io width 80 DOWN NO-LABEL.



form
  v-msg at 15
  v-balance  at 63
  with frame stmt-total-line no-box no-labels STREAM-IO.
/*
form
  skip(2)
  "      Current         30 Days         60 Days         90 Days        >90 Days"
  skip
  v-aged[1 for 5] skip(1)
  code-legend skip
  with frame stmt-total no-box no-labels stream-io width 80.
*/
form
  ws_letterhead[1]    skip
  ws_letterhead[2]    skip
  ws_letterhead[3]    skip
  ws_letterhead[4]    skip
  ws_letterhead[5]    skip(1)
  skip(5)
  ws_addr[1]    at 11 /*3*/
  ws_addr[2]    at 11
  ws_addr[3]    at 11
  v-stmt-date      at 53    cust.cust-no at 65 skip
  ws_addr[4]    at 11 skip
  ws_addr[5]    at 11 skip
  skip(4)
  skip
  with frame no-stmt-header no-box no-labels stream-io width 80.

form
  tt-inv.trans-date
  tt-inv.type
  tt-inv.inv-no
  tt-inv.DESCRIPTION FORM "x(25)"
  tt-inv.amount
  v-balance
  with frame no-stmt-line no-box no-labels stream-io width 80 down.

form
  v-msg at 15
  v-balance  at 63
  with frame no-stmt-total-line no-box no-labels STREAM-IO.

form
  skip(3)
  skip
  v-aged[1 for 5] skip(1)
  code-legend skip
  with frame no-stmt-total no-box no-labels stream-io width 80.

      DEF VAR ls-image1 AS cha NO-UNDO.
      DEF VAR ls-image2 AS cha NO-UNDO.
      DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
      DEF VAR ls-full-img2 AS cha FORM "x(200)" NO-UNDO.

      find first company where company.company eq cocode no-lock no-error.


      IF v-stmt-char = "Badger" THEN do:
          IF company.company EQ "003" THEN
              ASSIGN ls-image1 =  "images\Badger_CA.jpg"  .
          ELSE
              ASSIGN ls-image1 =  "images\badger statement.JPG"  .
      END.
      ELSE do:
      ASSIGN ls-image1 = IF v-stmt-char = "Premier" THEN "images\premierinv.jpg"
                         ELSE IF v-stmt-char = "LoyLang" THEN "images\loystmt.jpg"
                         ELSE IF v-stmt-char = "Printers" THEN "images\loyprinters.jpg"
                     /*    ELSE IF v-stmt-char = "Badger" THEN "images\badger statement.JPG" */
                         ELSE IF v-stmt-char = "RFC" THEN "images\RFC.JPG"
                         ELSE "images\asilogo.jpg" .
      END.

      FILE-INFO:FILE-NAME = ls-image1.
      ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
      FILE-INFO:FILE-NAME = ls-image2.

      ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".


if v-use-cust then
find first cust
    where cust.company eq cocode
      and cust.active  eq "S"
    no-lock no-error.

if v-print-hdr and avail company then do:
  yy = 1.

  if company.name    ne "" then
    assign
     ws_letterhead[yy] = company.name
     yy                = yy + 1.

  if company.addr[1] ne "" then
    assign
     ws_letterhead[yy] = company.addr[1]
     yy                = yy + 1.

  if company.addr[2] ne "" then
    assign
     ws_letterhead[yy] = company.addr[2]
     yy                = yy + 1.

  if company.city    ne "" or
     company.state   ne "" or
     company.zip     ne "" then
    assign
     ws_letterhead[yy] = company.city + ', ' + company.state
                         + '  ' + company.zip
     yy                = yy + 1.

  do xx = 1 to 6:
    if ws_letterhead[xx] gt '' then
      ws_letterhead[xx] = fill(" ", int((80 - length(ws_letterhead[xx])) / 2))
                          + ws_letterhead[xx].
  end.
end.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 56}

 v-stmt-date = stmt-date
 v-lo-cust   = IF tb_BatchMail:CHECKED IN FRAME {&frame-name} 
                  THEN icCustNo 
                  ELSE /*begin_cust-no*/ ""
 v-hi-cust   = IF tb_BatchMail:CHECKED IN FRAME {&frame-name} 
                  THEN icCustNo 
                  ELSE /*end_cust-no*/ ""
 v-msg       = stmt-msg
 v-detail    = tb_detailed
 v-past-due  = tb_past-due.

{sys/inc/print1.i}

{sys/inc/outprint.i  value(lines-per-page)}

if td-show-parm then run show-param.
is-xprint-form = YES.

IF is-xprint-form THEN DO:
   CASE rd-dest :
        WHEN 1 THEN PUT "<PRINTER?>" FORM "x(30)".
        WHEN 2 THEN do:
            IF NOT lBussFormModle THEN        
              PUT "<PREVIEW><MODAL=NO>" FORM "x(30)". 
            ELSE
              PUT "<PREVIEW>" FORM "x(30)".
        END.
        WHEN 4 THEN do:
              ls-fax-file = "c:\tmp\fx" + STRING(TIME) + ".tif".
              PUT UNFORMATTED "<PRINT=NO><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
        END.        
        WHEN 5 THEN DO:
            PUT "<PDF=DIRECT><PDF-LEFT=3mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".              
        END.
   END CASE.
   PUT "</PROGRESS>".
END.

SESSION:SET-WAIT-STATE ("general").
v-first = YES.
FOR EACH ttCustlist 
    WHERE ttCustList.log-fld
    NO-LOCK,
FIRST cust no-lock
    where cust.company eq cocode 
      AND cust.cust-no EQ ttCustList.cust-no
      AND (cust.cust-no EQ v-lo-cust OR v-lo-cust = "")
      AND (cust.cust-no EQ v-hi-cust  OR v-hi-cust = "")
      AND ((cust.acc-bal ne 0 AND NOT tb_curr-bal) OR (tb_curr-bal))
    BREAK BY cust.cust-no
    transaction:

  for each tt-inv:
    delete tt-inv.
  end.    /* clear workfile */

  ASSIGN v-last-amt  = 0
         v-last-ref# = ""
         v-last-paydate = ?.

  if v-past-due then
  do:
    find first ar-inv where ar-inv.company eq cust.company    and
                            ar-inv.cust-no eq cust.cust-no and
                            ar-inv.posted                and
                            ar-inv.due ne 0              and
                            ar-inv.inv-date le v-stmt-date and
                            ar-inv.due-date le v-stmt-date no-lock no-error.
    if not avail ar-inv THEN next.
  end.

  for each ar-inv
      where ar-inv.company  eq cust.company
        and ar-inv.cust-no  eq cust.cust-no
        and ar-inv.posted   eq yes
        /*and ar-inv.due      ne 0*/
        and ar-inv.terms    ne "CASH"
        and ar-inv.inv-date le v-stmt-date
      no-lock:

    FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no AND ar-invl.po-no <> "" USE-INDEX X-no NO-LOCK NO-ERROR.

    if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
      ld-due = ar-inv.net.
    else
      ld-due = ar-inv.gross.

    for each ar-cashl
        where ar-cashl.company  eq ar-inv.company
          and ar-cashl.posted   eq yes
          and ar-cashl.cust-no  eq ar-inv.cust-no
          and ar-cashl.inv-no   eq ar-inv.inv-no
        use-index inv-no no-lock,

        each ar-cash
        where ar-cash.c-no       eq ar-cashl.c-no
          and ar-cash.check-date le v-stmt-date
        use-index c-no no-lock:

      if ar-cashl.memo THEN
        if ar-cashl.amt-disc ne 0 then
          ld-due = ld-due - ar-cashl.amt-disc.
        else
        if ar-cashl.amt-paid + ar-cashl.amt-disc gt 0 then
          ld-due = ld-due + (ar-cashl.amt-paid + ar-cashl.amt-disc).
        else
          ld-due = ld-due + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
      else
        ld-due = ld-due + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
    end.

    IF ld-due NE 0 THEN DO:
        FIND FIRST tt-inv 
            WHERE tt-inv.cust-no EQ cust.cust-no
              AND tt-inv.DESCRIPTION EQ "No Balance Due"
            NO-ERROR.
        IF AVAIL tt-inv THEN DELETE tt-inv.
      create tt-inv.
      assign
       tt-inv.sort-fld   = "0" + STRING(ar-inv.inv-no,"9999999999") + "0"
       tt-inv.inv-date   = ar-inv.inv-date
       tt-inv.old-day    = (TODAY - ar-inv.inv-date)
       tt-inv.trans-date = ar-inv.inv-date
       tt-inv.inv-no     = ar-inv.inv-no
       tt-inv.type       = if ar-inv.type gt ' ' then ar-inv.type else 'I'
       tt-inv.amount     = if v-detail then
                           if ar-inv.net     eq
                              ar-inv.gross   +
                              ar-inv.freight +
                              ar-inv.tax-amt then ar-inv.net else ar-inv.gross
                          else ar-inv.due
       tt-inv.po-no      = (IF AVAIL ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "").                              .

      if v-detail then
      for each ar-cashl
          where ar-cashl.company  eq ar-inv.company
            and ar-cashl.posted   eq yes
            and ar-cashl.cust-no  eq ar-inv.cust-no
            and ar-cashl.inv-no   eq ar-inv.inv-no
          use-index inv-no no-lock,

          each ar-cash
          where ar-cash.c-no       eq ar-cashl.c-no
            and ar-cash.check-date le v-stmt-date
          use-index c-no no-lock:

        create tt-inv.
        assign
         tt-inv.sort-fld    = "0" + STRING(ar-cashl.inv-no,"9999999999") + "1"
         tt-inv.inv-date    = ar-inv.inv-date
         tt-inv.old-day    = (TODAY - ar-inv.inv-date)
         tt-inv.trans-date  = ar-cash.check-date
         tt-inv.inv-no      = ar-cashl.inv-no
         tt-inv.description = ar-cashl.dscr
         tt-inv.po-no       = (IF AVAIL ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "").

        if ar-cashl.memo then
          if ar-cashl.amt-disc ne 0 then
            assign
             tt-inv.type   = "R"
             tt-inv.amount = ar-cashl.amt-disc * -1.

          else  
          if ar-cashl.amt-paid + ar-cashl.amt-disc le 0 then
            assign
             tt-inv.type   = "CM"
             tt-inv.amount = ar-cashl.amt-paid + ar-cashl.amt-disc.

          else
            assign
             tt-inv.type   = "DM"
             tt-inv.amount = ar-cashl.amt-paid - ar-cashl.amt-disc.

        else
          assign
           tt-inv.type   = "P"
           tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1.

      end.
    END.
    ELSE IF tb_curr-bal THEN DO:
        IF NOT CAN-FIND(FIRST tt-inv WHERE tt-inv.cust-no EQ cust.cust-no
                        AND tt-inv.DESCRIPTION EQ "No Balance Due")
        THEN DO: 
            CREATE tt-inv.
            ASSIGN
                tt-inv.cust-no = cust.cust-no
                tt-inv.DESCRIPTION = "No Balance Due"
                .
        END.
    END.
  end.

  for each ar-cashl
      where ar-cashl.company    eq cust.company
        and ar-cashl.cust-no    eq cust.cust-no
        and ar-cashl.inv-no     eq 0
        and (ar-cashl.inv-date  le v-stmt-date or
             ar-cashl.inv-date  eq ?)
        and ar-cashl.posted     eq yes
        and ar-cashl.on-account eq yes
        and ar-cashl.amt-paid   ne 0
      no-lock,

      each ar-cash
      where ar-cash.c-no       eq ar-cashl.c-no
        and ar-cash.check-date le v-stmt-date
      use-index c-no no-lock:

    create tt-inv.
    assign
     tt-inv.sort-fld    = "1" + STRING(ar-cashl.inv-no,"9999999999")
     tt-inv.inv-date    = ar-cashl.inv-date
     tt-inv.old-day    = (TODAY - ar-cashl.inv-date)
     tt-inv.trans-date  = ar-cash.check-date
     tt-inv.inv-no      = ar-cashl.inv-no
     tt-inv.description = ar-cashl.dscr
     tt-inv.po-no       = "" /*(IF AVAIL ar-invl THEN ar-invl.po-no ELSE "")*/.

    if ar-cashl.memo then
      assign
       tt-inv.amount = ar-cashl.amt-paid
       tt-inv.type   = if tt-inv.amount lt 0 then "CR" else "DR".

    else
      assign
       tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1
       tt-inv.type   = "P".                             
  end.

  /* to get last payment amt, check, date */
  for each ar-cashl
      where ar-cashl.company    eq cust.company
        and ar-cashl.cust-no    eq cust.cust-no
        and (ar-cashl.inv-date  le v-stmt-date or
             ar-cashl.inv-date  eq ?)
        and ar-cashl.posted     eq yes
        /*and ar-cashl.on-account eq YES*/
        and ar-cashl.amt-paid   ne 0
        AND ar-cashl.memo       EQ NO
      no-lock,

      each ar-cash
      where ar-cash.c-no       eq ar-cashl.c-no
        and ar-cash.check-date le v-stmt-date
      use-index c-no no-lock:

      IF ar-cash.check-date > v-last-paydate OR v-last-paydate = ? THEN 
         ASSIGN v-last-amt     = ar-cashl.amt-paid
                v-last-ref#    = string(ar-cash.check-no)
                v-last-paydate = ar-cash.check-date.  
      ELSE
      IF v-last-ref#  = string(ar-cash.check-no) AND v-last-ref# <> "" THEN
         ASSIGN v-last-amt     = v-last-amt + ar-cashl.amt-paid.

  END.

  v-balance = 0. /* reset running balance */
  v-aged = 0. /* clear aging buckets */
  clear frame stmt-header no-pause.
  clear frame stmt-line all no-pause.
  clear frame stmt-line-badger all no-pause.
  clear frame stmt-total no-pause.
  ws_addr = ''.
  if avail cust then
  assign
    ws_addr[1] = cust.name
    ws_addr[2] = cust.addr[1]
    ws_addr[3] = cust.addr[2]
    ws_addr[4] = cust.city + ', ' + cust.state + '  ' + cust.zip.
  do yy = 1 to 6:
    do zz = yy + 1 to 6:
      if ws_addr[yy] eq '' and ws_addr[zz] gt ''
        then
      assign ws_addr[yy] = ws_addr[zz] ws_addr[zz] = ''.
    end.
  end.  

  for each tt-inv WHERE (tt-inv.amount NE 0 OR tt-inv.DESCRIPTION EQ "No Balance Due")
      break by "1"
            BY tt-inv.cust-no
            by tt-inv.inv-date
            by tt-inv.sort-fld
            by tt-inv.trans-date:

    IF AVAIL cust THEN
    FIND FIRST terms WHERE terms.company EQ cocode
        AND terms.t-code EQ cust.terms NO-LOCK NO-ERROR .
    IF AVAIL terms THEN
        ASSIGN terms_dscr = terms.dscr .

    if first-of ("1") or (line-counter gt ln-total) then do:
       IF NOT v-first THEN page.
       IF v-stmt-char = "Premier" THEN
          ASSIGN v-remitto[1] = "<C40>Remit To: PREMIER PACKAGING"
                 v-remitto[2] = "<C40>          3254 RELIABLE PARKWAY"
                 v-remitto[3] = "<C40>          CHICAGO, IL 60686".
       ELSE v-remitto = "".

       IF v-stmt-char = "Premier" THEN
       PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
           "<=1><R+6><C+54><B><P22>Statement</B><P12>" SKIP
           /*"<=1><R+8>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP*/
           "<=1><R+9><C+52><FROM><C+13><LINE>" SKIP
           "<=1><R+9><C+68><FROM><C+10><LINE>" 
           "<=1><R+9><C+52>" v-stmt-date
           "<=1><R+9><C+68>" cust.cust-no SKIP
           "<=1><R+11><C1>" ws_addr[1] v-remitto[1] skip
           "<=1><R+12><C1>" ws_addr[2] v-remitto[2] skip 
           "<=1><R+13><C1>" ws_addr[3] v-remitto[3] skip
           "<=1><R+14><C1>" ws_addr[4] v-remitto[4] skip
         /*  "<=1><R+15><C1>" ws_addr[5] skip*/
           "<=1><R+15><C1>Terms : " terms_dscr skip
           "<=1><R+17>Date     Code  Ref#  Description   <C56>Amount        Balance" SKIP
           "<=1><R+18><FROM><C+80><LINE>"
           . 
       ELSE IF v-stmt-char = "LoyLang" THEN
       PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
           "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
           "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
           "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
           "<=1><R+10><C+68><FROM><C+10><LINE>" 
           "<=1><R+10><C+52>" v-stmt-date
           "<=1><R+10><C+68>" cust.cust-no SKIP
           "<=1><R+11><C1>" ws_addr[1] skip
           "<=1><R+12><C1>" ws_addr[2] v-remitto[1] 
           "<C40>Last Payment $Amt Date     Check/Ref#" SKIP
           "<=1><R+13><C1>" ws_addr[3] v-remitto[2] SKIP
           "<=1><R+13><C40><FROM><C+17><LINE>"
           "<=1><R+13><C58><FROM><C+8><LINE>" 
           "<=1><R+13><C67><FROM><C+10><LINE>"      skip 
           "<=1><R+13><C40>" v-last-amt 
           "<=1><R+13><C58>" v-last-paydate
           "<=1><R+13><C69>" v-last-ref#            skip
           "<=1><R+14><C1>" ws_addr[4] v-remitto[3] skip
           "<=1><R+15><C1>" ws_addr[5] v-remitto[4] skip
           "<=1><R+17>         Trans"               SKIP
           "<=1><R+18>Date     Type   Ref#     BOL#   Customer PO#    <C56>Amount        Balance" SKIP
           "<=1><R+19><FROM><C+80><LINE>"
           . 
       ELSE IF v-stmt-char = "Printers" THEN
       PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
           "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
           "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
           "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
           "<=1><R+10><C+68><FROM><C+10><LINE>" 
           "<=1><R+10><C+52>" v-stmt-date
           "<=1><R+10><C+68>" cust.cust-no SKIP
           "<=1><R+11><C1>" ws_addr[1] skip
           "<=1><R+12><C1>" ws_addr[2] v-remitto[1] 
           "<C40>Last Payment $Amt Date     Check/Ref#" SKIP
           "<=1><R+13><C1>" ws_addr[3] v-remitto[2] SKIP
           "<=1><R+13><C40><FROM><C+17><LINE>"
           "<=1><R+13><C58><FROM><C+8><LINE>" 
           "<=1><R+13><C67><FROM><C+10><LINE>"      skip 
           "<=1><R+13><C40>" v-last-amt 
           "<=1><R+13><C58>" v-last-paydate
           "<=1><R+13><C69>" v-last-ref#            skip
           "<=1><R+14><C1>" ws_addr[4] v-remitto[3] skip
           "<=1><R+15><C1>" ws_addr[5] v-remitto[4] skip
           "<=1><R+17>         Trans"               SKIP
           "<=1><R+18>Date     Type   Inv#     Job#   Customer PO#    <C56>Amount        Balance" SKIP
           "<=1><R+19><FROM><C+80><LINE>"
           . 

       ELSE IF v-stmt-char = "Badger" THEN
           PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
           "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
           "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
           "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
           "<=1><R+10><C+68><FROM><C+10><LINE>" 
           "<=1><R+10><C+52>" v-stmt-date
           "<=1><R+10><C+68>" cust.cust-no SKIP
           "<=1><R+11><C1>" ws_addr[1] skip
           "<=1><R+12><C1>" ws_addr[2] v-remitto[1] skip 
           "<=1><R+13><C1>" ws_addr[3] v-remitto[2] skip
           "<=1><R+14><C1>" ws_addr[4] v-remitto[3] skip
           "<=1><R+15><C1>" ws_addr[5] v-remitto[4] skip
           "<=1><R+17>Date     Code  Inv# Description          Days <C55>Amount        Balance" SKIP
           "<=1><R+18><FROM><C+80><LINE>"
           . 

       ELSE IF v-stmt-char = "RFC" THEN          /* task 12231305 */
       PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
           "<=1><R+3><C+26> 2066 S. East Avenue" 
           "<=1><R+4><C+26> Vineland, NJ 08360" 
           "<=1><R+5><C+26> Phone: 856-692-0404" 
           "<=1><R+6><C+26> Fax: (856) 692-2085" 
           "<=1><R+10><C+54><B><P22>Statement</B><P12>" SKIP
           "<=1><R+12>" "<C53>Statement Date  Account #" SKIP
           "<=1><R+13><C+52><FROM><C+13><LINE>" SKIP
           "<=1><R+13><C+68><FROM><C+10><LINE>" 
           "<=1><R+13><C+52>" v-stmt-date
           "<=1><R+13><C+68>" cust.cust-no SKIP
           "<=1><R+11><C18>Attn:" cust.contact SKIP
           "<=1><R+13><C18>" ws_addr[1] skip                        /*Task# 01031416*/
           "<=1><R+14><C18>" ws_addr[2] v-remitto[1] skip 
           "<=1><R+15><C18>" ws_addr[3] v-remitto[2] skip
           "<=1><R+16><C18>" ws_addr[4] v-remitto[3] skip
           "<=1><R+17><C18>" ws_addr[5] v-remitto[4] skip
           "<=1><R+20>Date     Code  Ref#  Description   <C56>Amount        Balance" SKIP
           "<=1><R+21><FROM><C+80><LINE>"
           . 

       ELSE
       PUT "<C1><#1><R+11><C+45><IMAGE#1=" ls-full-img1 SKIP
           "<=1><R+7><C+54><B><P22>Statement</B><P12>" SKIP
           "<=1><R+9>Attn:" cust.contact "<C53>Statement Date  Account #" SKIP
           "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
           "<=1><R+10><C+68><FROM><C+10><LINE>" 
           "<=1><R+10><C+52>" v-stmt-date
           "<=1><R+10><C+68>" cust.cust-no SKIP
           "<=1><R+11><C1>" ws_addr[1] skip
           "<=1><R+12><C1>" ws_addr[2] v-remitto[1] skip 
           "<=1><R+13><C1>" ws_addr[3] v-remitto[2] skip
           "<=1><R+14><C1>" ws_addr[4] v-remitto[3] skip
           "<=1><R+15><C1>" ws_addr[5] v-remitto[4] skip
           "<=1><R+17>Date     Code  Ref#  Description   <C56>Amount        Balance" SKIP
           "<=1><R+18><FROM><C+80><LINE>"
           . 

       v-first = NO.
    end.

    if tt-inv.description eq '' then do:
      msgx = lookup(tt-inv.type,v-inv-type-list).
      if msgx eq 0 then
      msgx = 1.    /* assume invoice */
      tt-inv.description =
      if msgx gt 0 and msgx le v-inv-type-max
        then
      v-inv-type-array[msgx]
      else
      ''.
    end.

    v-balance = v-balance + tt-inv.amount.

    if v-stmt-char = "Badger" then do:
        display
          tt-inv.trans-date
          tt-inv.type
          tt-inv.inv-no  when tt-inv.inv-no gt 0
          tt-inv.description
          tt-inv.old-day
          tt-inv.amount
          v-balance 
          with frame stmt-line-badger .
        down 1 with frame stmt-line-badger.
    end.
    else do:
        display
          tt-inv.trans-date
          tt-inv.type
          tt-inv.inv-no  when tt-inv.inv-no gt 0
          tt-inv.description
          tt-inv.amount
          v-balance
          with frame no-stmt-line.
        down 1 with frame no-stmt-line.
    end.

    v-age = v-stmt-date - tt-inv.inv-date.
    if v-age = ? or v-age lt 0 then v-age = 0.
        if v-stmt-char = "Badger" then 
        v-per = trunc(v-age / (v-days-in-per + 1), 0) + 1.
    else        
        v-per = trunc(v-age / v-days-in-per, 0) + 1.
    if v-per gt 5 then
    v-per = 5.
    v-aged[v-per] = v-aged[v-per] + tt-inv.amount.

    if last-of ("1") then do:
      /*adv = ln-total - line-counter.
      put skip(adv).
      */
      PUT SKIP(1).

      if v-print-hdr then
      display
        v-msg
        v-balance
        with frame stmt-total-line.

      else
      display
        v-msg
        v-balance
        with frame no-stmt-total-line.
/*
      if v-print-hdr then
      display
        v-aged[1 for 5]
        code-legend
        with frame stmt-total.

      else
      display
        v-aged[1 for 5]
        code-legend
        with frame no-stmt-total.
*/
      IF v-stmt-char = "Badger" THEN 
         PUT "<R57><C1><#2>"SKIP
         "<=2>      Current             31 - 60             61 - 90            >90 Days" skip
         "<=2><R+1.3><FROM><C+80><LINE>" SKIP
         "<=2><R+2>" v-aged[1] AT 12 v-aged[2] AT 30  v-aged[3] AT 50  (v-aged[4] + v-aged[5]) AT 70
         skip(1).
      ELSE 
      PUT "<R57><C1><#2>"SKIP
      "<=2>      Current         30 Days         60 Days         90 Days        >90 Days" skip
      "<=2><R+1.3><FROM><C+80><LINE>" SKIP
      "<=2><R+2>" v-aged[1 for 5]
      skip(1).

     IF v-stmt-char = "LoyLang" OR v-stmt-char = "Printers" THEN 
         PUT "<R62><C1><#3>"SKIP
         "<=3><R+1><C1>" code-legend skip
         "<R+1><C+80><RECT#3>" 
         SKIP. 

    end.
  end.  /* for each tt-inv */

end. /* for each cust record */

SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-protagonstmt C-Win 
PROCEDURE run-protagonstmt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.
DEFINE INPUT PARAMETER ipl-email AS LOG NO-UNDO.

DEF VAR v-first AS LOG NO-UNDO.
DEF VAR v-remitto AS cha FORM "x(50)" EXTENT 4 NO-UNDO.

{sys/form/r-top.f}

def var v-stmt-date     as DATE format "99/99/9999" no-undo label "Statement Date".
def var v-lo-cust like cust.cust-no label "From Customer#" no-undo.
def var v-hi-cust  like cust.cust-no label "Thru Customer#" no-undo.
def var v-msg as char no-undo format 'x(40)' label "Statement Message".
def var v-detail as log format "yes/no" label "Print Detail?" no-undo.
def var v-past-due as Log no-undo format "yes/no" label "Print Past Due Only?".

def var xx as int no-undo.
def var yy as int no-undo.
def var zz as int no-undo.
def var v-print-align   as log format "Y/N" no-undo.
def var v-align-ok      as log format "Y/N" no-undo.
def var save_id         as recid no-undo.
def var v-balance as dec label "Balance" format '->>,>>>,>>>.99CR' NO-UNDO.
def var v-age as int no-undo. /* number of days old */
def var v-per as int no-undo. /* hash of v-age into aging periods */
def var v-aged as dec no-undo extent 5 format ">>,>>>,>>>.99CR" .   /* aging buckets */
def var v-days-in-per as int no-undo init 30.
def var ln-total as int no-undo init 51.
def var adv as int no-undo.
def var ws_letterhead as char format 'x(80)' no-undo extent 6.
def var ws_addr as char format 'x(35)' no-undo extent 6.
def var code-legend as char format 'X(80)' no-undo.
DEF VAR v-asi-excel AS LOG NO-UNDO.

DEF VAR v-last-amt  AS DECI NO-UNDO.
DEF VAR v-last-ref# AS CHAR NO-UNDO.
DEF VAR v-last-paydate AS DATE NO-UNDO.

DEF VAR ld-due AS DEC NO-UNDO.

DEF VAR lv-curr LIKE currency.c-desc NO-UNDO.
DEF VAR lv-terms LIKE terms.dscr NO-UNDO.
DEF VAR lc-attn AS CHAR NO-UNDO.
DEFINE VARIABLE lCheckCaseInv AS LOGICAL INITIAL NO NO-UNDO .

DEF BUFFER lb-cust FOR cust.
def var msgx as int no-undo.
def var v-inv-type-descr as char format 'x(30)' no-undo.
def var v-inv-type-list as char no-undo init "I,CR,DR,P,DA,FC,R".
def var v-inv-type-max  as int no-undo.
v-inv-type-max = num-entries(v-inv-type-list).
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
def var v-inv-type-array as char no-undo extent 7 init
  ["Invoice",
  "CR Memo",
  "DR Memo",
  "Payment",
  "Disc Allowed",
  "Finance Chg",
  "Return"].

code-legend = "CODES: ".
do xx = 1 to v-inv-type-max:
  code-legend = code-legend + entry(xx, v-inv-type-list) + '-'
    + v-inv-type-array[xx] + ' '.
end.

v-asi-excel = v-stmt-char EQ "ASIExcel".

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).

form
  tt-inv.trans-date COLUMN-LABEL "Date"
  tt-inv.inv-no COLUMN-LABEL "Ref#"
  tt-inv.description FORM "x(10)"  COLUMN-LABEL "Desc."
  tt-inv.po-no COLUMN-LABEL "Customer PO"
  tt-inv.inv-amt COLUMN-LABEL "Original!Invoice"
  tt-inv.amount COLUMN-LABEL "Invoice!Balance"
  v-balance COLUMN-LABEL "Balance"
  with frame stmt-line no-box stream-io width 90 DOWN NO-LABEL.

form
  v-msg at 15
  v-balance  at 73
  with frame stmt-total-line no-box no-labels STREAM-IO WIDTH 90.

form
  tt-inv.trans-date COLUMN-LABEL "Date"
  tt-inv.inv-no COLUMN-LABEL "Ref#"
  tt-inv.description FORM "x(12)"  COLUMN-LABEL "Description"
  tt-inv.po-no COLUMN-LABEL "Customer PO"
  tt-inv.inv-amt COLUMN-LABEL "Original!Invoice"
  tt-inv.amount COLUMN-LABEL "Invoice!Balance"
  v-balance COLUMN-LABEL "Balance"  
  with frame no-stmt-line no-box no-labels stream-io width 90 down.

form
  v-msg at 15
  v-balance  at 73
  with frame no-stmt-total-line no-box no-labels STREAM-IO WIDTH 90.

form
  skip(3)
  skip
  v-aged[1 for 4] skip(1)
  code-legend skip
  with frame no-stmt-total no-box no-labels stream-io width 80.

DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(200)" NO-UNDO.
ASSIGN ls-image1 = (IF v-stmt-char = "Protagon" THEN "images\protinv.jpg"
                   ELSE IF v-stmt-char = "SouleMed" THEN "images\Soulemedical.jpg" 
                   ELSE IF v-stmt-char =  "StdStatement10" THEN cRtnChar 
                    ELSE "images\Soule.jpg") . 


    ASSIGN
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".


ASSIGN ls-image2 = "images\protinvfoot.jpg"
       FILE-INFO:FILE-NAME = ls-image2
       ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

if v-use-cust then
   find first cust WHERE
        cust.company eq cocode AND
        cust.active  eq "S"
        no-lock no-error.

find first company where company.company eq cocode no-lock no-error.

if v-print-hdr and avail company then do:
  yy = 1.

  if company.name    ne "" then
    assign
     ws_letterhead[yy] = company.name
     yy                = yy + 1.

  if company.addr[1] ne "" then
    assign
     ws_letterhead[yy] = company.addr[1]
     yy                = yy + 1.

  if company.addr[2] ne "" then
    assign
     ws_letterhead[yy] = company.addr[2]
     yy                = yy + 1.

  if company.city    ne "" or
     company.state   ne "" or
     company.zip     ne "" then
    assign
     ws_letterhead[yy] = company.city + ', ' + company.state
                         + '  ' + company.zip
     yy                = yy + 1.

  do xx = 1 to 6:
    if ws_letterhead[xx] gt '' then
      ws_letterhead[xx] = fill(" ", int((80 - length(ws_letterhead[xx])) / 2))
                          + ws_letterhead[xx].
  end.
end.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 56}

 v-stmt-date = stmt-date
 v-msg       = stmt-msg
 v-detail    = tb_detailed
 v-past-due  = tb_past-due.

IF ip-sys-ctrl-shipto THEN
   ASSIGN
      v-lo-cust = ip-cust-no
      v-hi-cust = ip-cust-no.
ELSE
   ASSIGN
      v-lo-cust = ""
      v-hi-cust = "".
 
{sys/inc/print1.i}

{sys/inc/outprint.i  value(lines-per-page)}

if td-show-parm then run show-param.

IF NOT v-asi-excel THEN
   is-xprint-form = YES.
ELSE
   is-xprint-form = NO.

IF is-xprint-form THEN DO:
   CASE rd-dest :
        WHEN 1 THEN PUT "<PRINTER?>" FORM "x(30)".
        WHEN 2 THEN do:
            IF NOT lBussFormModle THEN        
              PUT "<PREVIEW><MODAL=NO>" FORM "x(30)". 
            ELSE
              PUT "<PREVIEW>" FORM "x(30)".
        END.
        WHEN 4 THEN do:
              ls-fax-file = "c:\tmp\fx" + STRING(TIME) + ".tif".
              PUT UNFORMATTED "<PRINT=NO><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
        END.        
        WHEN 5 THEN DO:
            IF NOT tb_BatchMail:CHECKED IN FRAME {&FRAME-NAME} THEN
               PUT "<PREVIEW><PDF-LEFT=3mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".              
            ELSE
               PUT "<PREVIEW=PDF><PDF-LEFT=3mm><PDF-TOP=2mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".
        END.
   END CASE.
   PUT "</PROGRESS>".
END.


SESSION:SET-WAIT-STATE ("general").
v-first = YES.

EMPTY TEMP-TABLE tt-inv.
EMPTY TEMP-TABLE tt-cust-excel.

FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
FIRST cust no-lock
    where cust.company eq cocode
      AND cust.cust-no EQ ttCustList.cust-no
      AND (cust.cust-no EQ v-lo-cust OR v-lo-cust = "")             
      AND (cust.cust-no EQ v-hi-cust OR v-hi-cust = "")            
      AND ((cust.acc-bal ne 0 AND NOT tb_curr-bal) OR (tb_curr-bal))
    BREAK BY cust.cust-no
    transaction:

    IF NOT v-asi-excel THEN
     EMPTY TEMP-TABLE tt-inv.

  ASSIGN v-last-amt  = 0
         v-last-ref# = ""
         v-last-paydate = ?.

  if v-past-due then
  do:
    find first ar-inv where ar-inv.company eq cust.company    and
                            ar-inv.cust-no eq cust.cust-no and
                            ar-inv.posted                and
                            ar-inv.due ne 0              and
                            ar-inv.inv-date le v-stmt-date and
                            ar-inv.due-date le v-stmt-date no-lock no-error.
    if not avail ar-inv THEN next.
  end.

  for each ar-inv
      where ar-inv.company  eq cust.company
        and ar-inv.cust-no  eq cust.cust-no
        and ar-inv.posted   eq yes
        /*and ar-inv.due      ne 0*/
        and ar-inv.terms    ne "CASH"
        and ar-inv.inv-date le v-stmt-date
      no-lock:

    FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no AND ar-invl.po-no <> "" USE-INDEX X-no NO-LOCK NO-ERROR.

    if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
      ld-due = ar-inv.net.
    else
      ld-due = ar-inv.gross.
    ASSIGN lCheckCaseInv = NO .
    for each ar-cashl
        where ar-cashl.company  eq ar-inv.company
          and ar-cashl.posted   eq yes
          and ar-cashl.cust-no  eq ar-inv.cust-no
          and ar-cashl.inv-no   eq ar-inv.inv-no
        use-index inv-no no-lock,

        each ar-cash
        where ar-cash.c-no       eq ar-cashl.c-no
          and ar-cash.check-date le v-stmt-date
        use-index c-no no-lock:

      if ar-cashl.memo THEN
        if ar-cashl.amt-disc ne 0 then
          ld-due = ld-due - ar-cashl.amt-disc.
        else
        if ar-cashl.amt-paid + ar-cashl.amt-disc gt 0 then
          ld-due = ld-due + (ar-cashl.amt-paid + ar-cashl.amt-disc).
        else
          ld-due = ld-due + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
      else
        ld-due = ld-due + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
        ASSIGN lCheckCaseInv = YES .
    end.

    IF ld-due NE 0 THEN DO:
        FIND FIRST tt-inv 
            WHERE tt-inv.cust-no EQ cust.cust-no
              AND tt-inv.DESCRIPTION EQ "No Balance Due"
            NO-ERROR.
        IF AVAIL tt-inv THEN DELETE tt-inv.
      create tt-inv.
      assign
       tt-inv.cust-no    = cust.cust-no
       tt-inv.sort-fld   = "0" + STRING(ar-inv.inv-no,"9999999999") + "0"
       tt-inv.inv-date   = ar-inv.inv-date
       tt-inv.trans-date = ar-inv.inv-date
       tt-inv.inv-no     = ar-inv.inv-no
       tt-inv.type       = if ar-inv.type gt ' ' then ar-inv.type else 'I'
       tt-inv.inv-amt    = ar-inv.gross
       tt-inv.amount     = if v-detail OR  NOT lCheckCaseInv then
                           if ar-inv.net     eq
                              ar-inv.gross   +
                              ar-inv.freight +
                              ar-inv.tax-amt then ar-inv.net else ar-inv.gross
                          else ar-inv.due
       tt-inv.po-no      = (IF AVAIL ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "")
       tt-inv.bol-no     = (IF AVAIL ar-invl AND ar-invl.bol-no <> 0 THEN string(ar-invl.bol-no,">>>>>>>>") ELSE "").

     IF v-stmt-char = "Printers" THEN
          tt-inv.bol-no = (IF AVAIL ar-invl AND ar-invl.job-no NE "" 
                                THEN "  " + ar-invl.job-no
                           ELSE IF AVAIL ar-invl AND ar-invl.ord-no <> 0 
                                THEN string(ar-invl.ord-no,">>>>>>>>") 
                           ELSE "").
      if v-detail then
      for each ar-cashl
          where ar-cashl.company  eq ar-inv.company
            and ar-cashl.posted   eq yes
            and ar-cashl.cust-no  eq ar-inv.cust-no
            and ar-cashl.inv-no   eq ar-inv.inv-no
          use-index inv-no no-lock,

          each ar-cash
          where ar-cash.c-no       eq ar-cashl.c-no
            and ar-cash.check-date le v-stmt-date
          use-index c-no no-lock:

        create tt-inv.
        assign
         tt-inv.cust-no     = cust.cust-no
         tt-inv.sort-fld    = "0" + STRING(ar-cashl.inv-no,"9999999999") + "1"
         tt-inv.inv-date    = ar-inv.inv-date
         tt-inv.trans-date  = ar-cash.check-date
         tt-inv.inv-no      = ar-cashl.inv-no
         tt-inv.inv-amt     = ar-inv.gross
         tt-inv.description = ar-cashl.dscr
         tt-inv.po-no       = (IF AVAIL ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "")
         tt-inv.bol-no     = (IF AVAIL ar-invl AND ar-invl.bol-no <> 0 THEN string(ar-invl.bol-no,">>>>>>>>") ELSE "").

        IF v-stmt-char = "Printers" THEN
          tt-inv.bol-no = (IF AVAIL ar-invl AND ar-invl.job-no NE "" 
                                THEN "  " + ar-invl.job-no 
                           ELSE IF AVAIL ar-invl AND ar-invl.ord-no <> 0 
                                THEN string(ar-invl.ord-no,">>>>>>>>") 
                           ELSE "").

        if ar-cashl.memo then
          if ar-cashl.amt-disc ne 0 then
            assign
             tt-inv.type   = "R"
             tt-inv.amount = ar-cashl.amt-disc * -1.

          else  
          if ar-cashl.amt-paid + ar-cashl.amt-disc le 0 then
            assign
             tt-inv.type   = "CM"
             tt-inv.amount = ar-cashl.amt-paid + ar-cashl.amt-disc.

          else
            assign
             tt-inv.type   = "DM"
             tt-inv.amount = ar-cashl.amt-paid - ar-cashl.amt-disc.

        else
          assign
           tt-inv.type   = "P"
           tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1.

      end.
    END.
    ELSE IF tb_curr-bal THEN DO:
        IF NOT CAN-FIND(FIRST tt-inv WHERE tt-inv.cust-no EQ cust.cust-no
                        AND tt-inv.DESCRIPTION EQ "No Balance Due")
        THEN DO: 
            CREATE tt-inv.
            ASSIGN
                tt-inv.cust-no = cust.cust-no
                tt-inv.DESCRIPTION = "No Balance Due"
                .
        END.
    END.
  end.

  for each ar-cashl
      where ar-cashl.company    eq cust.company
        and ar-cashl.cust-no    eq cust.cust-no
        and ar-cashl.inv-no     eq 0
        and (ar-cashl.inv-date  le v-stmt-date or
             ar-cashl.inv-date  eq ?)
        and ar-cashl.posted     eq yes
        and ar-cashl.on-account eq YES
        and ar-cashl.amt-paid   ne 0
      no-lock,

      each ar-cash
      where ar-cash.c-no       eq ar-cashl.c-no
        and ar-cash.check-date le v-stmt-date
      use-index c-no no-lock:

    create tt-inv.
    assign
     tt-inv.cust-no     = cust.cust-no
     tt-inv.sort-fld    = "1" + STRING(ar-cashl.inv-no,"9999999999")
     tt-inv.inv-date    = ar-cashl.inv-date
     tt-inv.trans-date  = ar-cash.check-date
     tt-inv.inv-no      = ar-cashl.inv-no
     tt-inv.description = ar-cashl.dscr
     tt-inv.po-no       = (IF AVAIL ar-invl THEN ar-invl.po-no ELSE "")
     tt-inv.bol-no     = (IF AVAIL ar-invl AND ar-invl.bol-no <> 0 THEN string(ar-invl.bol-no,">>>>>>>>") ELSE "").

    if ar-cashl.memo then
      assign
       tt-inv.amount = ar-cashl.amt-paid
       tt-inv.type   = if tt-inv.amount lt 0 then "CR" else "DR".

    else
      assign
       tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1
       tt-inv.type   = "P".

    end.                                                

  /* to get last payment amt, check, date */
  for each ar-cashl
      where ar-cashl.company    eq cust.company
        and ar-cashl.cust-no    eq cust.cust-no
        and (ar-cashl.inv-date  le v-stmt-date or
             ar-cashl.inv-date  eq ?)
        and ar-cashl.posted     eq yes
        /*and ar-cashl.on-account eq YES*/
        and ar-cashl.amt-paid   ne 0
        AND ar-cashl.memo       EQ NO
      no-lock,

      each ar-cash
      where ar-cash.c-no       eq ar-cashl.c-no
        and ar-cash.check-date le v-stmt-date
      use-index c-no no-lock:

      IF ar-cash.check-date > v-last-paydate OR v-last-paydate = ? THEN 
         ASSIGN v-last-amt     = ar-cashl.amt-paid
                v-last-ref#    = string(ar-cash.check-no)
                v-last-paydate = ar-cash.check-date.  
      ELSE
      IF v-last-ref#  = string(ar-cash.check-no) AND v-last-ref# <> "" THEN
         ASSIGN v-last-amt     = v-last-amt + ar-cashl.amt-paid.
  END.

  ASSIGN
  v-balance = 0 /* reset running balance */
  v-aged = 0. /* clear aging buckets */
  clear frame stmt-header no-pause.
  clear frame stmt-line all no-pause.
  clear frame stmt-total no-pause.
  ws_addr = ''.
  if avail cust then
  assign
    ws_addr[1] = cust.name
    ws_addr[2] = cust.addr[1]
    ws_addr[3] = cust.addr[2]
    ws_addr[4] = cust.city + ', ' + cust.state + '  ' + cust.zip.
  FIND FIRST currency WHERE currency.company = cust.company 
      AND currency.c-code = cust.curr-code NO-LOCK NO-ERROR.
  IF AVAIL currency THEN
      lv-curr = currency.c-desc.
  FIND FIRST terms WHERE terms.company = cust.company 
      AND terms.t-code = cust.terms NO-LOCK NO-ERROR.
  IF AVAIL terms THEN
      lv-terms = terms.dscr.
  do yy = 1 to 6:
    do zz = yy + 1 to 6:
      if ws_addr[yy] eq '' and ws_addr[zz] gt ''
        then
      assign ws_addr[yy] = ws_addr[zz] ws_addr[zz] = ''.
    end.
  end.

  for each tt-inv WHERE (tt-inv.amount NE 0 OR tt-inv.DESCRIPTION EQ "No Balance Due")
      break by "1"
            BY tt-inv.cust-no
            by tt-inv.inv-date
            by tt-inv.sort-fld
            by tt-inv.trans-date:

    IF v-asi-excel AND tt-inv.cust-no NE cust.cust-no THEN
       NEXT.
    IF fi_contact NE "" AND begin_cust-no = end_cust-no THEN DO:
        FIND FIRST lb-cust WHERE lb-cust.company = cocode 
            AND lb-cust.cust-no = begin_cust-no.
        IF AVAIL lb-cust THEN lb-cust.contact = fi_contact.
        RELEASE lb-cust.
        lc-attn = fi_contact.

    END.
    ELSE
        lc-attn = cust.contact.
    if NOT v-asi-excel AND (first-of ("1") or (line-counter gt ln-total)) then do:
       IF NOT v-first THEN page.
       v-remitto = "".
       IF v-stmt-char = "Protagon" THEN
       PUT "<C1><#1><R+9><C+45><IMAGE#1=" ls-full-img1 SKIP
           "<=1><R+7><C+54><B><P22>Statement</B><P11>" SKIP
           "<=1><R+10><C+52><FROM><C+13><LINE>" SKIP
           "<=1><R+10><C+68><FROM><C+10><LINE>" 
           "<=1><R+9><C53>Statement Date   Customer #" SKIP
           "<=1><R+10><C+52>" formatDate(v-stmt-date) FORMAT "X(20)"
           "<=1><R+10><C+68>" cust.cust-no SKIP
           "<=1><R+10><C1>" ws_addr[1] skip
           "<=1><R+11><C1>" ws_addr[2] v-remitto[1] "<C62>Terms" skip 
           "<=1><R+12><C+52><FROM><C+26><LINE>"
           "<=1><R+12><C1>" ws_addr[3] v-remitto[2] "<C53>" lv-terms skip
           "<=1><R+13><C1>" ws_addr[4] v-remitto[3] "<C62>Funds" skip
           "<=1><R+14><C+52><FROM><C+26><LINE>"
           "<=1><R+14><C1>" ws_addr[5] v-remitto[4] "<C53>" lv-curr skip
           "<=1><R+15>Attn: " lc-attn FORMAT "x(30)"
           "<=1><R+16>                                                Original<C60>Invoice" SKIP
           "<=1><R+17>Date       Ref# Desc.      Customer PO           Invoice<C60>Balance        Balance" SKIP
           "<=1><R+18><FROM><C+80><LINE>"
           . 
       IF v-stmt-char = "Soule" OR v-stmt-char = "SouleMed"  THEN
           PUT "<C1><#1><R+9><C+45><IMAGE#1=" ls-full-img1 SKIP
           "<=1><R+4><C+52><B><P22>Statement</B><P11>" SKIP
           "<=1><R+7><C+50><FROM><C+31><LINE>" SKIP
           "<=1><R+6><C51><b>Statement Date:</b> " formatDate(v-stmt-date) FORMAT "X(20)" SKIP
           "<=1><R+7.5><C+50><b>Customer #:</b>" cust.cust-no SKIP
           "<=1><R+8.5><C+50><FROM><C+31><LINE>"
           "<=1><R+9><C+50><b>Terms:</b> " lv-terms SKIP
           "<=1><R+10><C+50><FROM><C+33><LINE>"
           "<=1><R+10><C1>" ws_addr[1] skip
           "<=1><R+11><C1>" ws_addr[2] v-remitto[1]  skip 
           "<=1><R+12><C1>" ws_addr[3] v-remitto[2]  skip
           "<=1><R+13><C1>" ws_addr[4] v-remitto[3]  skip
           "<=1><R+14><C1>" ws_addr[5] v-remitto[4]  skip
           "<=1><R+15>Attn: " lc-attn FORMAT "x(30)"
           "<=1><R+16>                                                Original<C60>Invoice" SKIP
           "<=1><R+17>Date       Ref# Desc.      Customer PO           Invoice<C60>Balance        Balance" SKIP
           "<=1><R+18><FROM><C+80><LINE>"
            .
       
       IF v-stmt-char = "StdStatement10" THEN DO:
           PUT "<C2><R2><#1><R+8><C+47><IMAGE#1=" ls-full-img1 SKIP
           "<P11><R4><C50><#3><FROM><R7><C80><RECT><||3>" SKIP
           "<R5><C50><FROM><R5><C80><LINE><||3>" SKIP
           "<R6><C50><FROM><R6><C80><LINE><||3>" SKIP
           "<R4><C65><FROM><R5><C65><LINE><||3>" SKIP
           "<R5><C65><FROM><R6><C65><LINE><||3>" SKIP
           "<R6><C65><FROM><R7><C65><LINE><||3>" SKIP.
           PUT "<P22><=#3><C50><R-2> <B><P22>Statement</B> <P11>" " <B> PAGE: </B>" string(PAGE-NUM,">>9") SKIP
               "<=#3><R+0>  Customer ID      " cust.cust-no
               "<=#3><R+1>  Terms            " lv-terms
               "<=#3><R+2>  Statement Date   " v-stmt-date . 
          
          PUT "<=1><R+10><C1>" ws_addr[1] skip
           "<=1><R+11><C1>" ws_addr[2] v-remitto[1]  skip 
           "<=1><R+12><C1>" ws_addr[3] v-remitto[2]  skip
           "<=1><R+13><C1>" ws_addr[4] v-remitto[3]  skip
           "<=1><R+14><C1>" ws_addr[5] v-remitto[4]  skip
           "<=1><R+15>Attn: " lc-attn FORMAT "x(30)"
           "<=1><R+16>                                                Original<C60>Invoice" SKIP
           "<=1><R+17>Date       Ref# Desc.      Customer PO           Invoice<C60>Balance        Balance" SKIP
           "<=1><R+18><FROM><C+80><LINE>"
            .
       END.

       v-first = NO.
    end.

    if tt-inv.description eq '' then do:
      msgx = lookup(tt-inv.type,v-inv-type-list).
      if msgx eq 0 then
      msgx = 1.    /* assume invoice */
      tt-inv.description =
      if msgx gt 0 and msgx le v-inv-type-max
        then
      v-inv-type-array[msgx]
      else
      ''.
    end.

    v-balance = v-balance + tt-inv.amount.

    IF NOT v-asi-excel THEN
    DO:
       if v-print-hdr then do:
           display
             tt-inv.trans-date
             tt-inv.inv-no  when tt-inv.inv-no gt 0  
             tt-inv.DESCRIPTION
             tt-inv.po-no
             tt-inv.inv-amt
             tt-inv.amount
             v-balance
             with frame stmt-line .
           down 1 with frame stmt-line.
       end.
       else do:
           display
             tt-inv.trans-date
             tt-inv.inv-no  when tt-inv.inv-no gt 0
             tt-inv.DESCRIPTION
             tt-inv.po-no
             tt-inv.inv-amt
             tt-inv.amount
             v-balance
             with frame no-stmt-line.
           down 1 with frame no-stmt-line.
       end.
    END.

    v-age = v-stmt-date - tt-inv.inv-date.
    if v-age = ? or v-age lt 0 then v-age = 0.
    v-per = trunc(v-age / v-days-in-per, 0) + 1.
    if v-per gt 4 then
       v-per = 4.
    v-aged[v-per] = v-aged[v-per] + tt-inv.amount.

    if last-of ("1") then do:

      IF NOT v-asi-excel THEN
      DO:
         PUT SKIP(1).

         if v-print-hdr then
         display
           v-msg
           v-balance
           with frame stmt-total-line.

         else
         display
           v-msg
           v-balance
           with frame no-stmt-total-line.

         PUT "<R56><C1><#2>"SKIP
         "<=2><C13>    0-30 Days     31-60 Days     61-90 Days       >90 Days" skip
         "<=2><R+1.3><FROM><C+80><LINE>" SKIP
         "<=2><R+2><C13>" v-aged[1 for 4]
         SKIP.
         IF v-stmt-char = "Protagon" THEN
         PUT "<C14><R59.5><#3><R+6><C+53><IMAGE#3=" ls-full-img2 SKIP.

         IF v-stmt-char = "Soule" OR v-stmt-char = "StdStatement10" OR v-stmt-char = "SouleMed" THEN
         PUT "<C14><R59.5><#3><R+4><C+7> <b> THANK YOU - YOUR BUSINESS IS APPRECIATED </b>"  SKIP.

         IF v-stmt-char = "LoyLang" OR v-stmt-char = "Printers" THEN 
         PUT "<R62><C1><#3>"SKIP
         "<=3><R+1><C1>" code-legend skip
         "<R+1><C+80><RECT#3>" 
         SKIP. 

      END.
      ELSE
      DO:
         CREATE tt-cust-excel.

         ASSIGN tt-cust-excel.cust-no = tt-inv.cust-no
                tt-cust-excel.contact = cust.contact
                tt-cust-excel.addr[1] = ws_addr[1]
                tt-cust-excel.addr[2] = ws_addr[2]
                tt-cust-excel.addr[3] = ws_addr[3]
                tt-cust-excel.addr[4] = ws_addr[4]
                tt-cust-excel.addr[5] = ws_addr[5]
                tt-cust-excel.aged[1] = v-aged[1]
                tt-cust-excel.aged[2] = v-aged[2]
                tt-cust-excel.aged[3] = v-aged[3]
                tt-cust-excel.aged[4] = v-aged[4]
                tt-cust-excel.aged[5] = v-aged[5].
         RELEASE tt-cust-excel.
      END.
    end.
  end.  /* for each tt-inv */

end. /* for each cust record */

IF v-asi-excel THEN
   RUN arrep\asiexlstmt.p(INPUT v-stmt-date, INPUT v-msg).

IF NOT ipl-email THEN SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ ar/rep/stmt.p 07/95 CAH   */
/* A/R Statment Print Program - A/R Module                                    */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

IF lookup(v-stmt-char,"ASIXprnt,stmtprint 1,stmtprint 2,RFC,Premier,ASIExcel,Loylang,Printers,Badger") > 0 THEN DO:
   RUN run-asistmt(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
   RETURN.
END.

IF lookup(v-stmt-char,"Protagon,Soule,StdStatement10,SouleMed") > 0 THEN DO:
    RUN run-protagonstmt (ip-cust-no, ip-sys-ctrl-shipto, NO).
    RETURN.
END.

{sys/form/r-top.f}

def var v-stmt-date     as date format "99/99/9999" no-undo label "Statement Date".
def var v-lo-cust like cust.cust-no label "From Customer#" no-undo.
def var v-hi-cust  like cust.cust-no label "Thru Customer#" no-undo.
def var v-msg as char no-undo format 'x(40)' label "Statement Message".
def var v-detail as log format "yes/no" label "Print Detail?" no-undo.
def var v-past-due as Log no-undo format "yes/no" label "Print Past Due Only?".

def var xx as int no-undo.
def var yy as int no-undo.
def var zz as int no-undo.
def var v-print-align   as log format "Y/N" no-undo.
def var v-align-ok      as log format "Y/N" no-undo.
def var save_id         as recid no-undo.
def var v-balance as dec label "Balance" format '->>,>>>,>>>.99CR'.
def var v-age as int no-undo. /* number of days old */
def var v-per as int no-undo. /* hash of v-age into aging periods */
def var v-aged as dec no-undo extent 5
  format ">>,>>>,>>>.99CR" .   /* aging buckets */
def var v-days-in-per as int no-undo init 30.
def var ln-total as int no-undo init 51.
def var adv as int no-undo.
def var ws_letterhead as char format 'x(80)' no-undo extent 6.
def var ws_addr as char format 'x(35)' no-undo extent 6.
def var code-legend as char format 'X(80)' no-undo.

DEF VAR ld-due AS DEC NO-UNDO.

/* 07.11.95 by CAH @ASI:
1.  There is no ar transaction type file in system, so the following
vars have been added to support structured definition via lookups.
*/
def var msgx as int no-undo.
def var v-inv-type-descr as char format 'x(30)' no-undo.
def var v-inv-type-list as char no-undo init "I,CR,DR,P,DA,FC,R".
def var v-inv-type-max  as int no-undo.
v-inv-type-max = num-entries(v-inv-type-list).
def var v-inv-type-array as char no-undo extent 7 init
  ["Invoice",
  "CR Memo",
  "DR Memo",
  "Payment",
  "Disc Allowed",
  "Finance Chg",
  "Return"].

code-legend = "CODES: ".
do xx = 1 to v-inv-type-max:
  code-legend = code-legend + entry(xx, v-inv-type-list) + '-'
              + v-inv-type-array[xx] + ' '.
end.

form
  ws_letterhead[1]    skip
  ws_letterhead[2]    skip
  ws_letterhead[3]    skip
  ws_letterhead[4]    skip
  ws_letterhead[5]    skip(1)
  skip(5)
  ws_addr[1]    at 11 /*was 3*/
  "Statement date" at 50   "Account#" at 65 skip
  ws_addr[2]    at 11
  "--------------" at 50   "--------" at 65 skip
  ws_addr[3]    at 11
  v-stmt-date      at 53    cust.cust-no at 65 skip
  ws_addr[4]    at 11 skip
  ws_addr[5]    at 11 skip
  skip(1)
  "=============================== S T A T E M E N T ============================"
  skip
  with frame stmt-header no-box no-labels stream-io width 80.

form
  tt-inv.trans-date column-label "Date"
  tt-inv.type     column-label "Code"
  tt-inv.inv-no  column-label "Ref no"
  tt-inv.description column-label "Description"
  tt-inv.amount      column-label "Amount"
  v-balance       column-label "Balance"
  with frame stmt-line no-box stream-io width 80 down.

form
  v-msg at 15
  v-balance  at 63
  with frame stmt-total-line no-box no-labels stream-io.

form
  skip(2)
  "      Current         30 Days         60 Days         90 Days        >90 Days"
  skip
  v-aged[1 for 5] skip(1)
  code-legend skip
  with frame stmt-total no-box no-labels stream-io width 80.

form
  ws_letterhead[1]    skip
  ws_letterhead[2]    skip
  ws_letterhead[3]    skip
  ws_letterhead[4]    skip
  ws_letterhead[5]    skip(1)
  skip(5)
  ws_addr[1]    at 11 /*3*/
  ws_addr[2]    at 11
  ws_addr[3]    at 11
  v-stmt-date      at 53    cust.cust-no at 65 skip
  ws_addr[4]    at 11 skip
  ws_addr[5]    at 11 skip
  skip(4)
  skip
  with frame no-stmt-header no-box no-labels stream-io width 80.

form
  tt-inv.trans-date
  tt-inv.type
  tt-inv.inv-no
  tt-inv.description
  tt-inv.amount
  v-balance
  with frame no-stmt-line no-box no-labels stream-io width 80 down.

form
  v-msg at 15
  v-balance  at 63
  with frame no-stmt-total-line no-box no-labels stream-io.

form
  skip(3)
  skip
  v-aged[1 for 5] skip(1)
  code-legend skip
  with frame no-stmt-total no-box no-labels stream-io width 80.

if v-use-cust then
   find first cust WHERE
        cust.company eq cocode AND
        cust.active  eq "S"
        no-lock no-error.

find first company where company.company eq cocode no-lock no-error.

if v-print-hdr and avail company then do:
  yy = 1.

  if company.name    ne "" then
    assign
     ws_letterhead[yy] = company.name
     yy                = yy + 1.

  if company.addr[1] ne "" then
    assign
     ws_letterhead[yy] = company.addr[1]
     yy                = yy + 1.

  if company.addr[2] ne "" then
    assign
     ws_letterhead[yy] = company.addr[2]
     yy                = yy + 1.

  if company.city    ne "" or
     company.state   ne "" or
     company.zip     ne "" then
    assign
     ws_letterhead[yy] = company.city + ', ' + company.state
                         + '  ' + company.zip
     yy                = yy + 1.

  do xx = 1 to 6:
    if ws_letterhead[xx] gt '' then
      ws_letterhead[xx] = fill(" ", int((80 - length(ws_letterhead[xx])) / 2))
                          + ws_letterhead[xx].
  end.
end.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 56}

 v-stmt-date = stmt-date
 v-msg       = stmt-msg
 v-detail    = tb_detailed
 v-past-due  = tb_past-due.

IF ip-sys-ctrl-shipto THEN
   ASSIGN
      v-lo-cust = ip-cust-no
      v-hi-cust = ip-cust-no.
ELSE
   ASSIGN
      v-lo-cust = ""
      v-hi-cust = "".

{sys/inc/print1.i}

{sys/inc/outprint.i  value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

page.

FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
    first cust no-lock
    where cust.company eq cocode
      AND cust.cust-no EQ ttCustList.cust-no
      AND (cust.cust-no EQ v-lo-cust OR v-lo-cust = "")
      AND (cust.cust-no EQ v-hi-cust  OR v-hi-cust = "")
      AND ((cust.acc-bal ne 0 AND NOT tb_curr-bal) OR (tb_curr-bal))
    transaction:

  for each tt-inv:
      delete tt-inv.
  end.    /* clear workfile */

  if v-past-due then
  do:
    find first ar-inv where ar-inv.company eq cust.company    and
                            ar-inv.cust-no eq cust.cust-no and
                            ar-inv.posted                and
                            ar-inv.due ne 0              and
                            ar-inv.inv-date le v-stmt-date and
                            ar-inv.due-date le v-stmt-date no-lock no-error.
    if not avail ar-inv THEN next.
  end.

  for each ar-inv
      where ar-inv.company  eq cust.company
        and ar-inv.cust-no  eq cust.cust-no
        and ar-inv.posted   eq yes
        and ar-inv.terms    ne "CASH"
        and ar-inv.inv-date le v-stmt-date
      no-lock:

    FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no AND ar-invl.po-no <> "" USE-INDEX X-no NO-LOCK NO-ERROR.

    if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
      ld-due = ar-inv.net.
    else
      ld-due = ar-inv.gross.

    for each ar-cashl
        where ar-cashl.company  eq ar-inv.company
          and ar-cashl.posted   eq yes
          and ar-cashl.cust-no  eq ar-inv.cust-no
          and ar-cashl.inv-no   eq ar-inv.inv-no
        use-index inv-no no-lock,

        each ar-cash
        where ar-cash.c-no       eq ar-cashl.c-no
          and ar-cash.check-date le v-stmt-date
        use-index c-no no-lock:

      if ar-cashl.memo THEN
        if ar-cashl.amt-disc ne 0 then
          ld-due = ld-due - ar-cashl.amt-disc.
        else
        if ar-cashl.amt-paid + ar-cashl.amt-disc gt 0 then
          ld-due = ld-due + (ar-cashl.amt-paid + ar-cashl.amt-disc).
        else
          ld-due = ld-due + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
      else
        ld-due = ld-due + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
    end.

    IF ld-due NE 0 THEN DO:
        FIND FIRST tt-inv 
            WHERE tt-inv.cust-no EQ cust.cust-no
              AND tt-inv.DESCRIPTION EQ "No Balance Due"
            NO-ERROR.
        IF AVAIL tt-inv THEN DELETE tt-inv.
      create tt-inv.
      assign
       tt-inv.sort-fld   = "0" + STRING(ar-inv.inv-no,"9999999999") + "0"
       tt-inv.inv-date   = ar-inv.inv-date
       tt-inv.trans-date = ar-inv.inv-date
       tt-inv.inv-no     = ar-inv.inv-no
       tt-inv.type       = if ar-inv.type gt ' ' then ar-inv.type else 'I'
       tt-inv.amount     = if v-detail then
                           if ar-inv.net     eq
                              ar-inv.gross   +
                              ar-inv.freight +
                              ar-inv.tax-amt then ar-inv.net else ar-inv.gross
                          else ar-inv.due
       tt-inv.po-no      = (IF AVAIL ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "").                              

      if v-detail then
      for each ar-cashl
          where ar-cashl.company  eq ar-inv.company
            and ar-cashl.posted   eq yes
            and ar-cashl.cust-no  eq ar-inv.cust-no
            and ar-cashl.inv-no   eq ar-inv.inv-no
          use-index inv-no no-lock,

          each ar-cash
          where ar-cash.c-no       eq ar-cashl.c-no
            and ar-cash.check-date le v-stmt-date
          use-index c-no no-lock:

        create tt-inv.
        assign
         tt-inv.sort-fld    = "0" + STRING(ar-cashl.inv-no,"9999999999") + "1"
         tt-inv.inv-date    = ar-inv.inv-date
         tt-inv.trans-date  = ar-cash.check-date
         tt-inv.inv-no      = ar-cashl.inv-no
         tt-inv.description = ar-cashl.dscr
         tt-inv.po-no       = (IF AVAIL ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "").

        if ar-cashl.memo then
          if ar-cashl.amt-disc ne 0 then
            assign
             tt-inv.type   = "R"
             tt-inv.amount = ar-cashl.amt-disc * -1.

          else  
          if ar-cashl.amt-paid + ar-cashl.amt-disc le 0 then
            assign
             tt-inv.type   = "CM"
             tt-inv.amount = ar-cashl.amt-paid + ar-cashl.amt-disc.

          else
            assign
             tt-inv.type   = "DM"
             tt-inv.amount = ar-cashl.amt-paid - ar-cashl.amt-disc.

        else
          assign
           tt-inv.type   = "P"
           tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1.
      end.
    END.
    ELSE IF tb_curr-bal THEN DO:
        IF NOT CAN-FIND(FIRST tt-inv WHERE tt-inv.cust-no EQ cust.cust-no
                        AND tt-inv.DESCRIPTION EQ "No Balance Due")
        THEN DO: 
            CREATE tt-inv.
            ASSIGN
                tt-inv.cust-no = cust.cust-no
                tt-inv.DESCRIPTION = "No Balance Due"
                .
        END.
    END.
  end.

  for each ar-cashl
      where ar-cashl.company    eq cust.company
        and ar-cashl.cust-no    eq cust.cust-no
        and ar-cashl.inv-no     eq 0
        and (ar-cashl.inv-date  le v-stmt-date or
             ar-cashl.inv-date  eq ?)
        and ar-cashl.posted     eq yes
        and ar-cashl.on-account eq yes
        and ar-cashl.amt-paid   ne 0
      no-lock,

      each ar-cash
      where ar-cash.c-no       eq ar-cashl.c-no
        and ar-cash.check-date le v-stmt-date
      use-index c-no no-lock:

    create tt-inv.
    assign
     tt-inv.sort-fld    = "1" + STRING(ar-cashl.inv-no,"9999999999")
     tt-inv.inv-date    = ar-cashl.inv-date
     tt-inv.trans-date  = ar-cash.check-date
     tt-inv.inv-no      = ar-cashl.inv-no
     tt-inv.description = ar-cashl.dscr
     tt-inv.po-no       = "" /*(IF AVAIL ar-invl THEN ar-invl.po-no ELSE "")*/.

    if ar-cashl.memo then
      assign
       tt-inv.amount = ar-cashl.amt-paid
       tt-inv.type   = if tt-inv.amount lt 0 then "CR" else "DR".

    else
      assign
       tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1
       tt-inv.type   = "P".
  end.

  ASSIGN
    v-balance = 0 /* reset running balance */
    v-aged = 0. /* clear aging buckets */
  clear frame stmt-header no-pause.
  clear frame stmt-line all no-pause.
  clear frame stmt-total no-pause.
  ws_addr = ''.
  if avail cust then
  assign
    ws_addr[1] = cust.name
    ws_addr[2] = cust.addr[1]
    ws_addr[3] = cust.addr[2]
    ws_addr[4] = cust.city + ', ' + cust.state + '  ' + cust.zip.
  do yy = 1 to 6:
    do zz = yy + 1 to 6:
      if ws_addr[yy] eq '' and ws_addr[zz] gt ''
        then
      assign ws_addr[yy] = ws_addr[zz] ws_addr[zz] = ''.
    end.
  end.

  for each tt-inv WHERE (tt-inv.amount NE 0 OR tt-inv.DESCRIPTION EQ "No Balance Due")
      break by "1"
            by tt-inv.inv-date
            by tt-inv.sort-fld
            by tt-inv.trans-date:
    if first-of ("1") or (line-counter gt ln-total) then do:

      page.
      if v-print-hdr then
      display
        ws_letterhead[1]
        ws_letterhead[2]
        ws_letterhead[3]
        ws_letterhead[4]
        ws_letterhead[5]
        ws_addr[1]
        ws_addr[2]
        ws_addr[3]
        ws_addr[4]
        ws_addr[5]
        v-stmt-date
        cust.cust-no
        with frame stmt-header.
      else
      display
        ws_letterhead[1]
        ws_letterhead[2]
        ws_letterhead[3]
        ws_letterhead[4]
        ws_letterhead[5]
        ws_addr[1]
        ws_addr[2]
        ws_addr[3]
        ws_addr[4]
        ws_addr[5]
        v-stmt-date
        cust.cust-no
        with frame no-stmt-header.

    end.

    if tt-inv.description eq '' then do:
      msgx = lookup(tt-inv.type,v-inv-type-list).
      if msgx eq 0 then
      msgx = 1.    /* assume invoice */
      tt-inv.description =
      if msgx gt 0 and msgx le v-inv-type-max then
         v-inv-type-array[msgx]
      else
         ''.
    end.

    v-balance = v-balance + tt-inv.amount.

    if v-print-hdr then do:
    display
      tt-inv.trans-date
      tt-inv.type
      tt-inv.inv-no  when tt-inv.inv-no gt 0
      tt-inv.description
      tt-inv.amount
      v-balance
      with frame stmt-line.
    down 1 with frame stmt-line.
    end.
    else do:
    display
      tt-inv.trans-date
      tt-inv.type
      tt-inv.inv-no  when tt-inv.inv-no gt 0
      tt-inv.description
      tt-inv.amount
      v-balance
      with frame no-stmt-line.
    down 1 with frame no-stmt-line.
    end.

    v-age = v-stmt-date - tt-inv.inv-date.
    if v-age = ? or v-age lt 0 then v-age = 0.
    v-per = trunc(v-age / v-days-in-per, 0) + 1.
    if v-per gt 5 then
       v-per = 5.

    v-aged[v-per] = v-aged[v-per] + tt-inv.amount.

    if last-of ("1") then do:
      adv = ln-total - line-counter.

      put skip(adv).

      if v-print-hdr then
      display
        v-msg
        v-balance
        with frame stmt-total-line.

      else
      display
        v-msg
        v-balance
        with frame no-stmt-total-line.

      if v-print-hdr then
      display
        v-aged[1 for 5]
        code-legend
        with frame stmt-total.

      else
      display
        v-aged[1 for 5]
        code-legend
        with frame no-stmt-total.

      put skip(1).
    end.
  end.  /* for each tt-inv */

end. /* for each cust record */

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-mail C-Win 
PROCEDURE run-report-mail :
/* ------------------------------------------------ ar/rep/stmt.p 07/95 CAH   */
/* A/R Statment Print Program - A/R Module                                    */
/* -------------------------------------------------------------------------- */
DEFINE INPUT PARAM icCustNo   AS CHARACTER NO-UNDO.

IF lookup(v-stmt-char,"ASIXprnt,stmtprint 1,stmtprint 2,Loylang,RFC,Premier,Badger,Printers") > 0 THEN DO:
   RUN run-asistmt-mail (icCustNo).
   RETURN.
END.
IF lookup(v-stmt-char,"Protagon,Soule,StdStatement10,SouleMed") > 0 THEN DO:
    RUN run-protagonstmt (icCustNo, NO, YES).
    RETURN.
END.


{sys/form/r-top.f}

def var v-stmt-date     as date format "99/99/9999" no-undo label "Statement Date".
def var v-lo-cust like cust.cust-no label "From Customer#" no-undo.
def var v-hi-cust  like cust.cust-no label "Thru Customer#" no-undo.
def var v-msg as char no-undo format 'x(40)' label "Statement Message".
def var v-detail as log format "yes/no" label "Print Detail?" no-undo.
def var v-past-due as Log no-undo format "yes/no" label "Print Past Due Only?".

def var xx as int no-undo.
def var yy as int no-undo.
def var zz as int no-undo.
def var v-print-align   as log format "Y/N" no-undo.
def var v-align-ok      as log format "Y/N" no-undo.
def var save_id         as recid no-undo.
def var v-balance as dec label "Balance" format '->>,>>>,>>>.99CR'.
def var v-age as int no-undo. /* number of days old */
def var v-per as int no-undo. /* hash of v-age into aging periods */
def var v-aged as dec no-undo extent 5
  format ">>,>>>,>>>.99CR" .   /* aging buckets */
def var v-days-in-per as int no-undo init 30.
def var ln-total as int no-undo init 51.
def var adv as int no-undo.
def var ws_letterhead as char format 'x(80)' no-undo extent 6.
def var ws_addr as char format 'x(35)' no-undo extent 6.
def var code-legend as char format 'X(80)' no-undo.

DEF VAR ld-due AS DEC NO-UNDO.

/* 07.11.95 by CAH @ASI:
1.  There is no ar transaction type file in system, so the following
vars have been added to support structured definition via lookups.
*/
def var msgx as int no-undo.
def var v-inv-type-descr as char format 'x(30)' no-undo.
def var v-inv-type-list as char no-undo init "I,CR,DR,P,DA,FC,R".
def var v-inv-type-max  as int no-undo.
v-inv-type-max = num-entries(v-inv-type-list).
def var v-inv-type-array as char no-undo extent 7 init
  ["Invoice",
  "CR Memo",
  "DR Memo",
  "Payment",
  "Disc Allowed",
  "Finance Chg",
  "Return"].

code-legend = "CODES: ".
do xx = 1 to v-inv-type-max:
  code-legend = code-legend + entry(xx, v-inv-type-list) + '-'
    + v-inv-type-array[xx] + ' '.
end.

form
  ws_letterhead[1]    skip
  ws_letterhead[2]    skip
  ws_letterhead[3]    skip
  ws_letterhead[4]    skip
  ws_letterhead[5]    skip(1)
  skip(5)
  ws_addr[1]    at 11 /*was 3*/
  "Statement date" at 50   "Account#" at 65 skip
  ws_addr[2]    at 11
  "--------------" at 50   "--------" at 65 skip
  ws_addr[3]    at 11
  v-stmt-date      at 53    cust.cust-no at 65 skip
  ws_addr[4]    at 11 skip
  ws_addr[5]    at 11 skip
  skip(1)
  "=============================== S T A T E M E N T ============================"
  skip
  with frame stmt-header no-box no-labels stream-io width 80.

form
  tt-inv.trans-date column-label "Date"
  tt-inv.type     column-label "Code"
  tt-inv.inv-no  column-label "Ref no"
  tt-inv.description column-label "Description"
  tt-inv.amount      column-label "Amount"
  v-balance       column-label "Balance"
  with frame stmt-line no-box stream-io width 80 down.

form
  v-msg at 15
  v-balance  at 63
  with frame stmt-total-line no-box no-labels stream-io.

form
  skip(2)
  "      Current         30 Days         60 Days         90 Days        >90 Days"
  skip
  v-aged[1 for 5] skip(1)
  code-legend skip
  with frame stmt-total no-box no-labels stream-io width 80.

form
  ws_letterhead[1]    skip
  ws_letterhead[2]    skip
  ws_letterhead[3]    skip
  ws_letterhead[4]    skip
  ws_letterhead[5]    skip(1)
  skip(5)
  ws_addr[1]    at 11 /*3*/
  ws_addr[2]    at 11
  ws_addr[3]    at 11
  v-stmt-date      at 53    cust.cust-no at 65 skip
  ws_addr[4]    at 11 skip
  ws_addr[5]    at 11 skip
  skip(4)
  skip
  with frame no-stmt-header no-box no-labels stream-io width 80.

form
  tt-inv.trans-date
  tt-inv.type
  tt-inv.inv-no
  tt-inv.description
  tt-inv.amount
  v-balance
  with frame no-stmt-line no-box no-labels stream-io width 80 down.

form
  v-msg at 15
  v-balance  at 63
  with frame no-stmt-total-line no-box no-labels stream-io.

form
  skip(3)
  skip
  v-aged[1 for 5] skip(1)
  code-legend skip
  with frame no-stmt-total no-box no-labels stream-io width 80.

if v-use-cust then
   find first cust WHERE
        cust.company eq cocode AND
        cust.active  eq "S"
        no-lock no-error.

find first company where company.company eq cocode no-lock no-error.

if v-print-hdr and avail company then do:
  yy = 1.

  if company.name    ne "" then
    assign
     ws_letterhead[yy] = company.name
     yy                = yy + 1.

  if company.addr[1] ne "" then
    assign
     ws_letterhead[yy] = company.addr[1]
     yy                = yy + 1.

  if company.addr[2] ne "" then
    assign
     ws_letterhead[yy] = company.addr[2]
     yy                = yy + 1.

  if company.city    ne "" or
     company.state   ne "" or
     company.zip     ne "" then
    assign
     ws_letterhead[yy] = company.city + ', ' + company.state
                         + '  ' + company.zip
     yy                = yy + 1.

  do xx = 1 to 6:
    if ws_letterhead[xx] gt '' then
      ws_letterhead[xx] = fill(" ", int((80 - length(ws_letterhead[xx])) / 2))
                          + ws_letterhead[xx].
  end.
end.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 56}

 v-stmt-date = stmt-date
 v-lo-cust   = IF tb_BatchMail:CHECKED IN FRAME {&frame-name} 
                  THEN icCustNo 
                  ELSE begin_cust-no
 v-hi-cust   = IF tb_BatchMail:CHECKED IN FRAME {&frame-name} 
                  THEN icCustNo 
                  ELSE end_cust-no
 v-msg       = stmt-msg
 v-detail    = tb_detailed
 v-past-due  = tb_past-due.

{sys/inc/print1.i}

{sys/inc/outprint.i  value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

page.

FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
    first cust no-lock
    where cust.company eq cocode 
      AND cust.cust-no EQ ttCustList.cust-no
/*     cust.cust-no ge v-lo-cust and */
/*     cust.cust-no le v-hi-cust and */
      AND ((cust.acc-bal ne 0 AND NOT tb_curr-bal) OR (tb_curr-bal))
    transaction:

  for each tt-inv:
      delete tt-inv.
  end.    /* clear workfile */

  if v-past-due then
  do:
    find first ar-inv where ar-inv.company eq cust.company    and
                            ar-inv.cust-no eq cust.cust-no and
                            ar-inv.posted                and
                            ar-inv.due ne 0              and
                            ar-inv.inv-date le v-stmt-date and
                            ar-inv.due-date le v-stmt-date no-lock no-error.
    if not avail ar-inv THEN next.
  end.

  for each ar-inv
      where ar-inv.company  eq cust.company
        and ar-inv.cust-no  eq cust.cust-no
        and ar-inv.posted   eq yes
        and ar-inv.terms    ne "CASH"
        and ar-inv.inv-date le v-stmt-date
      no-lock:

    FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no AND ar-invl.po-no <> "" USE-INDEX X-no NO-LOCK NO-ERROR.

    if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
       ld-due = ar-inv.net.
    else
       ld-due = ar-inv.gross.

    for each ar-cashl
        where ar-cashl.company  eq ar-inv.company
          and ar-cashl.posted   eq yes
          and ar-cashl.cust-no  eq ar-inv.cust-no
          and ar-cashl.inv-no   eq ar-inv.inv-no
        use-index inv-no no-lock,

        each ar-cash
        where ar-cash.c-no       eq ar-cashl.c-no
          and ar-cash.check-date le v-stmt-date
        use-index c-no no-lock:

      if ar-cashl.memo THEN
        if ar-cashl.amt-disc ne 0 then
          ld-due = ld-due - ar-cashl.amt-disc.
        else
        if ar-cashl.amt-paid + ar-cashl.amt-disc gt 0 then
          ld-due = ld-due + (ar-cashl.amt-paid + ar-cashl.amt-disc).
        else
          ld-due = ld-due + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
      else
        ld-due = ld-due + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
    end.

    IF ld-due NE 0 THEN DO:
        FIND FIRST tt-inv 
            WHERE tt-inv.cust-no EQ cust.cust-no
              AND tt-inv.DESCRIPTION EQ "No Balance Due"
            NO-ERROR.
        IF AVAIL tt-inv THEN DELETE tt-inv.
      create tt-inv.
      assign
       tt-inv.sort-fld   = "0" + STRING(ar-inv.inv-no,"9999999999") + "0"
       tt-inv.inv-date   = ar-inv.inv-date
       tt-inv.trans-date = ar-inv.inv-date
       tt-inv.inv-no     = ar-inv.inv-no
       tt-inv.type       = if ar-inv.type gt ' ' then ar-inv.type else 'I'
       tt-inv.amount     = if v-detail then
                           if ar-inv.net     eq
                              ar-inv.gross   +
                              ar-inv.freight +
                              ar-inv.tax-amt then ar-inv.net else ar-inv.gross
                          else ar-inv.due
       tt-inv.po-no      = (IF AVAIL ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "").

      if v-detail then
      for each ar-cashl
          where ar-cashl.company  eq ar-inv.company
            and ar-cashl.posted   eq yes
            and ar-cashl.cust-no  eq ar-inv.cust-no
            and ar-cashl.inv-no   eq ar-inv.inv-no
          use-index inv-no no-lock,

          each ar-cash
          where ar-cash.c-no       eq ar-cashl.c-no
            and ar-cash.check-date le v-stmt-date
          use-index c-no no-lock:

        create tt-inv.
        assign
         tt-inv.sort-fld    = "0" + STRING(ar-cashl.inv-no,"9999999999") + "1"
         tt-inv.inv-date    = ar-inv.inv-date
         tt-inv.trans-date  = ar-cash.check-date
         tt-inv.inv-no      = ar-cashl.inv-no
         tt-inv.description = ar-cashl.dscr
         tt-inv.po-no       = (IF AVAIL ar-invl AND ar-invl.inv-no <> 0 THEN ar-invl.po-no ELSE "").

        if ar-cashl.memo then
          if ar-cashl.amt-disc ne 0 then
            assign
             tt-inv.type   = "R"
             tt-inv.amount = ar-cashl.amt-disc * -1.

          else  
          if ar-cashl.amt-paid + ar-cashl.amt-disc le 0 then
            assign
             tt-inv.type   = "CM"
             tt-inv.amount = ar-cashl.amt-paid + ar-cashl.amt-disc.

          else
            assign
             tt-inv.type   = "DM"
             tt-inv.amount = ar-cashl.amt-paid - ar-cashl.amt-disc.

        else
          assign
           tt-inv.type   = "P"
           tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1.
      end.
    END.
    ELSE IF tb_curr-bal THEN DO:
        IF NOT CAN-FIND(FIRST tt-inv WHERE tt-inv.cust-no EQ cust.cust-no
                        AND tt-inv.DESCRIPTION EQ "No Balance Due")
        THEN DO: 
            CREATE tt-inv.
            ASSIGN
                tt-inv.cust-no = cust.cust-no
                tt-inv.DESCRIPTION = "No Balance Due"
                .
        END.
    END.
  end.

  for each ar-cashl
      where ar-cashl.company    eq cust.company
        and ar-cashl.cust-no    eq cust.cust-no
        and ar-cashl.inv-no     eq 0
        and (ar-cashl.inv-date  le v-stmt-date or
             ar-cashl.inv-date  eq ?)
        and ar-cashl.posted     eq yes
        and ar-cashl.on-account eq yes
        and ar-cashl.amt-paid   ne 0
      no-lock,

      each ar-cash
      where ar-cash.c-no       eq ar-cashl.c-no
        and ar-cash.check-date le v-stmt-date
      use-index c-no no-lock:

    create tt-inv.
    assign
     tt-inv.sort-fld    = "1" + STRING(ar-cashl.inv-no,"9999999999")
     tt-inv.inv-date    = ar-cashl.inv-date
     tt-inv.trans-date  = ar-cash.check-date
     tt-inv.inv-no      = ar-cashl.inv-no
     tt-inv.description = ar-cashl.dscr
     tt-inv.po-no       = "" /*(IF AVAIL ar-invl THEN ar-invl.po-no ELSE "")*/.

    if ar-cashl.memo then
      assign
       tt-inv.amount = ar-cashl.amt-paid
       tt-inv.type   = if tt-inv.amount lt 0 then "CR" else "DR".

    else
      assign
       tt-inv.amount = (ar-cashl.amt-paid + ar-cashl.amt-disc) * -1
       tt-inv.type   = "P".
  end.

  ASSIGN
     v-balance = 0 /* reset running balance */
     v-aged = 0. /* clear aging buckets */
  clear frame stmt-header no-pause.
  clear frame stmt-line all no-pause.
  clear frame stmt-total no-pause.
  ws_addr = ''.
  if avail cust then
  assign
    ws_addr[1] = cust.name
    ws_addr[2] = cust.addr[1]
    ws_addr[3] = cust.addr[2]
    ws_addr[4] = cust.city + ', ' + cust.state + '  ' + cust.zip.
  do yy = 1 to 6:
    do zz = yy + 1 to 6:
      if ws_addr[yy] eq '' and ws_addr[zz] gt ''
        then
      assign ws_addr[yy] = ws_addr[zz] ws_addr[zz] = ''.
    end.
  end.

  for each tt-inv WHERE (tt-inv.amount NE 0 OR tt-inv.DESCRIPTION EQ "No Balance Due")
      break by "1"
            by tt-inv.inv-date
            by tt-inv.sort-fld
            by tt-inv.trans-date:
    if first-of ("1") or (line-counter gt ln-total) then do:

      page.
      if v-print-hdr then
      display
        ws_letterhead[1]
        ws_letterhead[2]
        ws_letterhead[3]
        ws_letterhead[4]
        ws_letterhead[5]
        ws_addr[1]
        ws_addr[2]
        ws_addr[3]
        ws_addr[4]
        ws_addr[5]
        v-stmt-date
        cust.cust-no
        with frame stmt-header.
      else
      display
        ws_letterhead[1]
        ws_letterhead[2]
        ws_letterhead[3]
        ws_letterhead[4]
        ws_letterhead[5]
        ws_addr[1]
        ws_addr[2]
        ws_addr[3]
        ws_addr[4]
        ws_addr[5]
        v-stmt-date
        cust.cust-no
        with frame no-stmt-header.

    end.

    if tt-inv.description eq '' then do:
      msgx = lookup(tt-inv.type,v-inv-type-list).
      if msgx eq 0 then
      msgx = 1.    /* assume invoice */
      tt-inv.description =
      if msgx gt 0 and msgx le v-inv-type-max THEN
         v-inv-type-array[msgx]
      else
         ''.
    end.

    v-balance = v-balance + tt-inv.amount.

    if v-print-hdr then do:
    display
      tt-inv.trans-date
      tt-inv.type
      tt-inv.inv-no  when tt-inv.inv-no gt 0
      tt-inv.description
      tt-inv.amount
      v-balance
      with frame stmt-line.
    down 1 with frame stmt-line.
    end.
    else do:
    display
      tt-inv.trans-date
      tt-inv.type
      tt-inv.inv-no  when tt-inv.inv-no gt 0
      tt-inv.description
      tt-inv.amount
      v-balance
      with frame no-stmt-line.
    down 1 with frame no-stmt-line.
    end.

    v-age = v-stmt-date - tt-inv.inv-date.
    if v-age = ? or v-age lt 0 then v-age = 0.
    v-per = trunc(v-age / v-days-in-per, 0) + 1.
    if v-per gt 5 then
       v-per = 5.

    v-aged[v-per] = v-aged[v-per] + tt-inv.amount.

    if last-of ("1") then do:
      adv = ln-total - line-counter.

      put skip(adv).

      if v-print-hdr then
      display
        v-msg
        v-balance
        with frame stmt-total-line.

      else
      display
        v-msg
        v-balance
        with frame no-stmt-total-line.

      if v-print-hdr then
      display
        v-aged[1 for 5]
        code-legend
        with frame stmt-total.

      else
      display
        v-aged[1 for 5]
        code-legend
        with frame no-stmt-total.

      put skip(1).
    end.
  end.  /* for each tt-inv */

end. /* for each cust record */



/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMail-1 C-Win 
PROCEDURE SendMail-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAM icIdxKey   AS CHAR NO-UNDO.
  DEFINE INPUT PARAM icRecType  AS CHAR NO-UNDO.
  DEFINE INPUT PARAM icFileName AS CHAR NO-UNDO.

  DEFINE VARIABLE vcSubject   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMailBody  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.

  IF v-stmt-char EQ "ASIExcel" THEN
     ASSIGN icFileName =  v-dir + "\stmt.xls"      .

  ASSIGN vcSubject   = "STATEMENT" + '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
         vcMailBody  = "Please review attached statement(s).".

  RUN custom/xpmail2.p   (input   icRecType,
                          input   'R-STMT.',
                          input   icFileName,
                          input   icIdxKey,
                          input   vcSubject,
                          input   vcMailBody,
                          OUTPUT  vcErrorMsg).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAttentionDefault C-Win 
PROCEDURE setAttentionDefault :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER lbf-cust FOR cust.

IF (v-stmt-char = "Protagon" OR v-stmt-char = "Soule" OR v-stmt-char = "StdStatement10" OR v-stmt-char = "SouleMed")
    AND begin_cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ end_cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
    AND begin_cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
    FIND FIRST lbf-cust WHERE lbf-cust.company = cocode
        AND lbf-cust.cust-no = begin_cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
    IF AVAIL lbf-cust THEN 
        ASSIGN
            fi_contact:SCREEN-VALUE IN FRAME {&FRAME-NAME} = lbf-cust.contact
            fi_contact = lbf-cust.contact.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCustRange C-Win 
PROCEDURE SetCustRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
        begin_cust-no:SENSITIVE = NOT iplChecked
        end_cust-no:SENSITIVE = NOT iplChecked
        begin_cust-no:VISIBLE = NOT iplChecked
        end_cust-no:VISIBLE = NOT iplChecked
        btnCustList:SENSITIVE = iplChecked
       .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetEmailBoxes C-Win 
PROCEDURE SetEmailBoxes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF rd-dest:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '5' THEN
  DO:
     ASSIGN tb_BatchMail:SENSITIVE = YES
            tb_HideDialog:SENSITIVE = YES.

     IF v-pdf-camp THEN
        tb_emailpdf:SENSITIVE = YES.
  END.

  ELSE
     ASSIGN tb_BatchMail:SENSITIVE  = no
            tb_BatchMail:CHECKED    = no
            tb_HideDialog:SENSITIVE = no
            tb_HideDialog:CHECKED   = no
            tb_emailpdf:SENSITIVE = no
            tb_emailpdf:CHECKED = no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetStmtForm C-Win 
PROCEDURE SetStmtForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAM icPrintFormat AS CHAR NO-UNDO.

   v-stmt-char = icPrintFormat.





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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION formatDate C-Win 
FUNCTION formatDate RETURNS CHARACTER
  ( INPUT ip-date AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VAR out-date AS CHAR NO-UNDO.
    DEFINE VAR cMonth AS CHAR EXTENT 12 NO-UNDO INIT
    [ "January",    "February",     "March", 
      "April",      "May",          "June", 
      "July",       "August",       "September",
      "October",    "November",     "December" ]. 

    out-date = cmonth[MONTH(ip-date)] + " " + STRING(DAY(ip-date)) + " " + STRING(YEAR(ip-date)).
    RETURN out-date.  /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

