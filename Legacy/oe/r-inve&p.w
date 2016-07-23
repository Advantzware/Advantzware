&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oe\r-inve&p.w

  Description: Invoice Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 05/07/02

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
DEF VAR oeprep-char AS CHAR NO-UNDO.
        DEF VAR v-prof AS DEC NO-UNDO.
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
    
def new shared buffer xoe-relh for oe-relh.
def new shared buffer yoe-relh for oe-relh.
def new shared buffer xoe-rell for oe-rell.
def new shared buffer inv-line for inv-line.

def buffer xinv-line for inv-line.
def buffer tmp-oe-boll for oe-boll.
def buffer xoe-ord for oe-ord.
DEF BUFFER b-oe-ordl FOR oe-ordl.

def new shared var v-ar-acct like ar-ctrl.receivables.
def new shared var v-ar-freight like ar-ctrl.freight.
def new shared var v-ar-stax like ar-ctrl.stax.
def new shared var v-ar-sales like ar-ctrl.sales.
def new shared var v-ar-disc like ar-ctrl.discount.
def new shared var v-return as log init no.
def new shared var v-start2-compress as char.
def new shared var v-end2-compress as char.
def new shared var v-post as log init no.
def new shared var v-trnum as int.
def new shared var v-back like itemfg.q-back.
def new shared var v-balance as dec format ">>>,>>>,>>9.99cr".
def new shared var v-reduce-ord-bal like cust.ord-bal no-undo.
def new shared var v-invline as recid.
def new shared var v-invhead as recid.
def new shared var v-detail   as   log format "Detail/Summary" init no no-undo.
def new shared var v-gldetail as   log format "Detail/Summary" init no no-undo.

def var v-fr-tax as log init no.
def var v-postable as log init no.

def var v-xno like ar-inv.x-no. /* Unique Internial # for header */
def var v-xline as int.     /* Unique Internail # for lines */

def var v-inv-qty like oe-ordl.inv-qty.
def var v-ord-no like inv-line.ord-no.
def var v-ord-date as date.
def var v-inv-disc as dec format "->>,>>9.99".
def var v-inv-disc-w AS DEC NO-UNDO.
def var ld-temp-amt as dec.
def var v-tax-rate as dec extent 4.

def var v-uninv-ordl-amt like oe-ordl.t-price no-undo init 0.
def var v-u-inv like oe-ctrl.u-inv init false.
def var v-tmp-tax-rate as dec format ">,>>9.99<<<".

def var v-line-tot like inv-line.t-price.
def var v-misc-tot like inv-misc.amt.
def var v-line-tot-w AS DEC NO-UNDO.
def var v-post-zero-cgs as log no-undo.
def var v-export like sys-ctrl.char-fld no-undo.
def var v-rec-written as int no-undo.
def var t-rec-written as int no-undo.
def var v-s-inv-no like inv-head.inv-no init 0 no-undo.
def var v-e-inv-no like v-s-inv-no init 999999.
def var v-s-date   like inv-head.inv-date format "99/99/9999"
                                          init 01/01/0001 no-undo.
def var v-e-date   like v-s-date init today.
def var v-cost as dec extent 4.
def var v-cas-cnt like itemfg.case-count.

def var v-close-qty like oe-ordl.qty.
def var v-dcr-val      like oe-ordl.cost INIT 0.
def var v-uom-rate     as   int.
def var v-sum-rel-qty as dec no-undo.
def var v-tax as dec.
DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR lv-list-name LIKE list-name NO-UNDO.
DEF VAR v-ftp-done AS LOG NO-UNDO.
DEF VAR v-print-fmt AS cha NO-UNDO.
DEF VAR ll-warned AS LOG NO-UNDO.
DEF VAR v-ttl-tax AS DEC NO-UNDO.
DEF VAR v-ttl-rate AS DEC NO-UNDO.

DEF TEMP-TABLE w-report NO-UNDO LIKE report.

DEF TEMP-TABLE tt-gl NO-UNDO FIELD row-id AS ROWID.
DEF TEMP-TABLE tt-custbal NO-UNDO
  FIELD cust-no AS CHAR
  FIELD ord-bal AS DEC
  INDEX i1 cust-no.

DEF BUFFER b-inv-head FOR inv-head.
DEF BUFFER save-line FOR reftable.

{oe/invwork.i new}

{oe/closchk.i new}

run oe/getacct.p.

FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK.

if v-return then return.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "INVPOST"
    no-lock no-error.
if not available sys-ctrl then do transaction:
  message "Creating new System Control record (INVPOST).".
  create sys-ctrl.
  assign
    sys-ctrl.company = cocode
    sys-ctrl.name = "INVPOST"
    sys-ctrl.log-fld = no
    sys-ctrl.descrip = "Post cost-of-goods sold when cost is zero?".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
v-post-zero-cgs = sys-ctrl.log-fld.
find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "INVPRINT" no-lock no-error.
v-print-fmt = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".

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
v-export = sys-ctrl.char-fld.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
assign
 v-fr-tax = oe-ctrl.f-tax
 v-u-inv  = oe-ctrl.u-inv.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.

DO TRANSACTION:
  {sys/inc/postdate.i}
  {sys/inc/oeprep.i}
  {sys/inc/oeclose.i}
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

&SCOPED-DEFINE use-factored

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
begin_date end_date tb_detailed tb_detailed-2 tb_ton tb_export rd-dest ~
lv-ornt lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_inv end_inv ~
begin_date end_date tb_detailed tb_detailed-2 tb_ton tb_export rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 

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
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 11.43.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Invoice Report Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_detailed-2 AS LOGICAL INITIAL no 
     LABEL "G/L Report Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_export AS LOGICAL INITIAL no 
     LABEL "Export/FTP  Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE tb_ton AS LOGICAL INITIAL no 
     LABEL "Print $/Ton?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.43 COL 44 COLON-ALIGNED
     tran-period AT ROW 3.62 COL 44 COLON-ALIGNED
     begin_inv AT ROW 5.29 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number"
     end_inv AT ROW 5.29 COL 69 COLON-ALIGNED HELP
          "Enter Ending Invoice Number"
     begin_date AT ROW 6.24 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date"
     end_date AT ROW 6.24 COL 69 COLON-ALIGNED HELP
          "Enter Ending Invoice Date"
     tb_detailed AT ROW 7.67 COL 36
     tb_detailed-2 AT ROW 8.86 COL 36
     tb_ton AT ROW 10.05 COL 36
     tb_export AT ROW 11 COL 36
     rd-dest AT ROW 13.86 COL 5 NO-LABEL
     lv-ornt AT ROW 14.1 COL 29 NO-LABEL
     lines-per-page AT ROW 14.1 COL 82 COLON-ALIGNED
     lv-font-no AT ROW 16.48 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 17.43 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.86 COL 30
     btn-ok AT ROW 21.71 COL 23
     btn-cancel AT ROW 21.71 COL 58
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .95 AT ROW 12.91 COL 3
     RECT-6 AT ROW 12.67 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 94.4 BY 22.95.


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
         TITLE              = "Invoice Posting"
         HEIGHT             = 23.19
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
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_detailed-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_export:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Invoice Posting */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Invoice Posting */
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
  DEF VAR v-close-line AS LOG NO-UNDO.
  DEFINE VARIABLE cStatus AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cReason AS CHARACTER   NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  run check-date.
  if v-invalid then return no-apply.

  SESSION:SET-WAIT-STATE ("general").   

  ASSIGN
   rd-dest
   tran-period
   tran-date.


  DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
        IF AVAIL gl-ctrl THEN DO:

        /*
         FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode 
           EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF LOCKED(gl-ctrl) THEN
         DO:
          MESSAGE "The General Ledger Control File is Currently Locked" SKIP
                  "by another user. Posting can NOT proceed at this Time!!"
                VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
         END.
        */
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
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_inv
                            &END_cust=END_inv
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE=''
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE=''
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
 
       END. 
       WHEN 6 THEN run output-to-port.
  end case. 

  IF v-postable THEN DO:
    
    lv-post = NO.

    IF v-balance = 0 then
       MESSAGE "Post Invoices?"
           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
           UPDATE lv-post.

    IF lv-post THEN DO:

      RUN list-post-inv ("post").

      RUN post-gl.
      RUN copy-report-to-audit-dir.

      FOR EACH tt-report:
          DELETE tt-report.
      END.

      EMPTY TEMP-TABLE tt-gl.

      IF v-export EQ "Sonoco" THEN
         RUN ar/sonoinv.p ("total", t-rec-written, OUTPUT v-rec-written).
          
      FOR EACH w-ord BREAK BY w-ord.ord-no:
          IF NOT FIRST-OF(w-ord.ord-no) THEN DELETE w-ord.
      END.

      order-close1:
      FOR EACH w-ord,
          FIRST oe-ord NO-LOCK
          WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ w-ord.ord-no
          BREAK BY oe-ord.cust-no:

        RELEASE cust.
        RUN oe/calcordt.p (ROWID(oe-ord)).
        IF LAST-OF(oe-ord.cust-no) THEN DO:

           FIND FIRST tt-custbal WHERE tt-custbal.cust-no EQ oe-ord.cust-no
              NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-custbal THEN DO:
             CREATE tt-custbal.
             ASSIGN tt-custbal.cust-no = oe-ord.cust-no.
           END.

           FIND FIRST cust /* EXCLUSIVE */
               WHERE cust.company EQ oe-ord.company
                 AND cust.cust-no EQ oe-ord.cust-no
               NO-LOCK NO-ERROR.

           IF AVAIL cust THEN DO:
              RUN ar/updcust1.p (NO, BUFFER cust, OUTPUT tt-custbal.ord-bal).

              /* IF cust.ord-bal LT 0 THEN cust.ord-bal = 0. */

              FIND CURRENT cust NO-LOCK.
           END.
        END.


      END. /* Each w-ord */
      
      cust-bal:
      FOR EACH tt-custbal,
        FIRST cust WHERE cust.company EQ cocode
          AND cust.cust-no EQ tt-custbal.cust-no
          EXCLUSIVE-LOCK.
        /* RUN ar/updcust1.p (BUFFER cust, OUTPUT cust.ord-bal). */
        cust.ord-bal = tt-custbal.ord-bal.         

        IF cust.ord-bal LT 0 THEN cust.ord-bal = 0.

      END.

      order-close2:
      FOR EACH w-ord,
          FIRST oe-ord NO-LOCK
          WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ w-ord.ord-no
          BREAK BY oe-ord.cust-no:

        RELEASE cust.



        IF NOT oeclose-log THEN
        DO:
           /*RUN close-order-lines.*/
           FOR EACH oe-ordl WHERE
               oe-ordl.company EQ oe-ord.company AND
               oe-ordl.ord-no  EQ oe-ord.ord-no AND
               oe-ordl.stat    NE "C"
               NO-LOCK:

               RUN oe/CloseOrder(INPUT ROWID(oe-ordl),
                                 INPUT NO,
                                 OUTPUT cStatus,
                                 OUTPUT cReason).
/*                RUN oe/clslnchkinv.p (BUFFER oe-ordl, OUTPUT v-close-line). */
/*                IF v-close-line THEN */
               IF cStatus EQ 'C' THEN
                  RUN oe/closelin.p (INPUT ROWID(oe-ordl),YES).
           END.

           RUN close-order.
        END.
      END. /* Each w-ord */

       
      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.

      IF oeclose-log THEN
      DO:
         RUN oe/closchkinv.p (0).

         IF CAN-FIND (FIRST w-ord) THEN
            RUN oe/d-close.w.
      END.
    END.
  END.

  ELSE MESSAGE "No Invoices available for posting..." VIEW-AS ALERT-BOX ERROR.

  FOR EACH save-line WHERE save-line.reftable EQ "save-line" + STRING(v-trnum,"9999999999"):
      RUN undo-save-line.
  END.

  IF NOT v-postable OR NOT lv-post THEN DO TRANSACTION:
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:

        IF gl-ctrl.trnum EQ v-trnum THEN gl-ctrl.trnum = v-trnum - 1.
        FIND CURRENT gl-ctrl NO-LOCK.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  END.

  IF v-ftp-done THEN MESSAGE "File Export/FTP is completed." VIEW-AS ALERT-BOX INFORMATION.

  SESSION:SET-WAIT-STATE("").
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


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Invoice Report Detailed? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_detailed-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed-2 C-Win
ON VALUE-CHANGED OF tb_detailed-2 IN FRAME FRAME-A /* G/L Report Detailed? */
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

  do transaction:
    {sys/inc/inexport.i}
  end.

  find first inv-head
      where inv-head.company eq cocode
        and inv-head.posted  eq NO
        and inv-head.printed eq YES
        and inv-head.stat    ne "H"
      use-index prnt no-lock no-error.
  if avail inv-head then begin_inv = inv-head.inv-no.
   
  end_date = TODAY.
  
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

    IF lookup(v-print-fmt,"Frankstn,MIRPKG,ContSrvc,CSC-GA") GT 0
        THEN tb_export:SENSITIVE = YES.
    ELSE ASSIGN tb_export = NO
                tb_export:SCREEN-VALUE = "NO" 
                tb_export:SENSITIVE = NO.

    APPLY "entry" TO tran-date.
  END.

  if v-u-inv then
    MESSAGE "This will ONLY post invoices that have been PRINTED!" skip(1)
            "If you want your inventory to be updated from this posting,"
            "The 'Update Inventory When Posting' flag in the Order"
            "Entry control file must be set to 'INV', Otherwise"
            "inventory will be updated when Bills of Lading are posted."
            VIEW-AS ALERT-BOX.
  else
    MESSAGE "This will ONLY post invoices that have been PRINTED!" skip(1)
            "Your inventory will be updated from this posting, the"
            "'Update Inventory When Posting' flag in the Order Entry"
            "control file is set to 'INV'."
            VIEW-AS ALERT-BOX.
  
  {methods/nowait.i}


  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-tax-gr C-Win 
PROCEDURE calc-tax-gr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipr-head-row AS ROWID.
DEF INPUT PARAMETER ipi-inv-no LIKE inv-head.inv-no NO-UNDO.

/*DEF VAR v-ttl-tax AS DEC NO-UNDO.
DEF VAR v-ttl-rate AS DEC NO-UNDO.

def var v-tax-rate as dec extent 4.     */
DEF BUFFER bf-currency FOR currency.
DEF BUFFER bf-inv-head FOR inv-head.

DEF VAR k AS INT.
DEF VAR dAccum AS DEC NO-UNDO.

FIND bf-inv-head WHERE ROWID(bf-inv-head) = ipr-head-row NO-LOCK NO-ERROR.

IF NOT AVAIL bf-inv-head THEN
    RETURN.

    FIND FIRST bf-currency NO-LOCK
        WHERE bf-currency.company     EQ bf-inv-head.company
          AND bf-currency.c-code      EQ bf-inv-head.curr-code[1]
          AND bf-currency.ar-ast-acct NE ""
          AND bf-currency.ex-rate     GT 0
        NO-ERROR.

    assign v-ttl-tax  = 0
           v-ttl-rate = 0.
    find first stax
        {sys/ref/stax1W.i}
          and {sys/ref/taxgroup.i stax} eq bf-inv-head.tax-gr
        no-lock no-error.
    if not avail stax then
    find first stax
        where stax.company = bf-inv-head.company AND
        stax.tax-group eq bf-inv-head.tax-gr
        no-lock no-error.
    dAccum = 1.
    if avail stax then do:
      do i = 1 to extent(stax.tax-rate1):
        if stax.tax-rate1[i] = 0 then next.
        v-tax-rate[i] = stax.tax-rate1[i].
        IF stax.accum-tax THEN DO: 
        /*##PN - must find effective rate since this is accumulated*/
            dAccum = dAccum  * (1 + v-tax-rate[i] / 100).
            v-tax-rate[i] = 100 * (dAccum - (v-ttl-rate / 100) - 1).
        END.
        if stax.company eq "yes" and i gt 1 then
        do k = 1 to i - 1:
          v-tax-rate[i] = v-tax-rate[i] +
                          (v-tax-rate[i] * (stax.tax-rate1[k] / 100)).
        end.
        v-ttl-rate = v-ttl-rate + v-tax-rate[i].
      end.
      
      do i = 1 to extent(stax.tax-rate1):
        if stax.tax-rate1[i] = 0 then next.
        ASSIGN v-tax-rate[i] = round(v-tax-rate[i] / v-ttl-rate *
                                     bf-inv-head.t-inv-tax,2)
               v-ttl-tax = v-ttl-tax + v-tax-rate[i].
      end.
      
      if bf-inv-head.t-inv-tax ne v-ttl-tax then
        v-tax-rate[1] = v-tax-rate[1] +
                        (bf-inv-head.t-inv-tax - v-ttl-tax).
      
      do i = 1 to extent(stax.tax-rate1):
        if stax.tax-rate1[i] = 0 then next.
        find first account
            where account.company eq cocode
              and account.actnum  eq stax.tax-acc1[i]
            no-lock no-error.
            
        if avail account AND v-tax-rate[i] NE 0 then do:
          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = "work-tax"
           tt-report.key-02  = account.actnum
           tt-report.key-03  = string(ipi-inv-no,"999999")
           tt-report.key-04  = bf-inv-head.tax-gr
           tt-report.key-05  = string(v-tax-rate[i] *
                                      (IF AVAIL bf-currency  THEN
                                         bf-currency.ex-rate ELSE 1))
           tt-report.weight  = v-line-tot-w *
                               (v-tax-rate[i] / bf-inv-head.t-inv-tax).
        end. /* avail account */

      end. /* 1 to 3 */

    end. /* avail stax */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-tons C-Win 
PROCEDURE calc-tons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM ip-i-no LIKE itemfg.i-no NO-UNDO.
  DEF INPUT  PARAM ip-qty AS DEC NO-UNDO.
  DEF OUTPUT PARAM op-weight AS DEC NO-UNDO.

  DEF BUFFER b-itemfg FOR itemfg.


  FIND FIRST b-itemfg
      WHERE b-itemfg.company EQ cocode
        AND b-itemfg.i-no    EQ ip-i-no
      NO-LOCK NO-ERROR.
  IF AVAIL b-itemfg AND b-itemfg.weight-100 NE 0 THEN
    op-weight = b-itemfg.weight-100 * ip-qty / 100.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-order C-Win 
PROCEDURE close-order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF CAN-FIND(FIRST oe-ordl WHERE 
                     oe-ordl.company = oe-ord.company AND
                     oe-ordl.ord-no = oe-ord.ord-no AND 
                     oe-ordl.stat NE "C") THEN RETURN.

   RUN oe\close.p(RECID(oe-ord), YES).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-order-lines C-Win 
PROCEDURE close-order-lines :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* mdp adds logic to close lines from clslinchk.p */
   
   DEFINE BUFFER lb-oe-ordl FOR oe-ordl.

   FOR EACH oe-ordl NO-LOCK
      WHERE oe-ordl.company EQ oe-ord.company
        AND oe-ordl.ord-no  EQ oe-ord.ord-no:

      FIND itemfg OF oe-ordl NO-LOCK NO-ERROR.
    
      IF (oe-ordl.inv-qty  GE oe-ordl.qty * (1 - (oe-ordl.under-pct / 100)) OR
          oe-ordl.is-a-component)                                               AND
         (oe-ordl.ship-qty GE oe-ordl.qty * (1 - (oe-ordl.under-pct / 100)) OR
          CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})  OR
         (AVAIL itemfg AND NOT itemfg.stocked)) THEN                               
      DO:
         FIND lb-oe-ordl WHERE ROWID(lb-oe-ordl) = ROWID(oe-ordl)
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
          IF AVAIL lb-oe-ordl THEN lb-oe-ordl.stat = "C".
      END.
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
  
  ASSIGN targetfile = lv-audit-dir + "\OP\OB4\Run#"
                    + STRING(v-trnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\OP"
         dirname3 = lv-audit-dir + "\OP\OB4".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-save-line C-Win 
PROCEDURE create-save-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    DISABLE TRIGGERS FOR LOAD OF inv-line.
    DISABLE TRIGGERS FOR LOAD OF inv-misc.

    FOR EACH inv-line WHERE inv-line.r-no EQ b-inv-head.r-no:
      CREATE save-line.
      ASSIGN
       save-line.reftable = "save-line" + STRING(v-trnum,"9999999999")
       save-line.val[1]   = inv-line.r-no
       save-line.val[2]   = inv-head.r-no
       save-line.val[3]   = INT(RECID(inv-line))
       inv-line.r-no      = inv-head.r-no.
    END.

    FOR EACH inv-misc WHERE inv-misc.r-no EQ b-inv-head.r-no:
      CREATE save-line.
      ASSIGN
       save-line.reftable = "save-line" + STRING(v-trnum,"9999999999")
       save-line.val[1]   = inv-misc.r-no
       save-line.val[2]   = inv-head.r-no
       save-line.val[3]   = INT(RECID(inv-misc))
       inv-misc.r-no      = inv-head.r-no.
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
  DISPLAY tran-date tran-period begin_inv end_inv begin_date end_date 
          tb_detailed tb_detailed-2 tb_ton tb_export rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_inv end_inv begin_date end_date 
         tb_detailed tb_detailed-2 tb_ton tb_export rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-lot-no C-Win 
PROCEDURE get-lot-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       THIS IS TO ASSIGN THE LOT NUMBER FROM THE REFTABLE TO 
------------------------------------------------------------------------------*/
 FIND FIRST reftable WHERE
      reftable.reftable EQ "inv-line.lot-no" AND
      reftable.rec_key  EQ inv-line.rec_key
      USE-INDEX rec_key NO-LOCK NO-ERROR.

 IF AVAIL reftable THEN DO:
     ASSIGN ar-invl.lot-no = TRIM(reftable.CODE).
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-tr-dscr C-Win 
PROCEDURE get-tr-dscr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM ip-inv-no LIKE ar-inv.inv-no NO-UNDO.
  DEF OUTPUT PARAM op-dscr LIKE gltrans.tr-dscr NO-UNDO.


  RELEASE ar-inv.
  RELEASE cust.

  FIND FIRST ar-inv
      WHERE ar-inv.company EQ cocode
        AND ar-inv.inv-no  EQ ip-inv-no
      NO-LOCK NO-ERROR.
  IF AVAIL ar-inv THEN
  FIND FIRST cust
      WHERE cust.company EQ ar-inv.company
        AND cust.cust-no EQ ar-inv.cust-no
      NO-LOCK NO-ERROR.
  op-dscr = TRIM(IF AVAIL cust THEN cust.name ELSE "Cust not on file") +
            " Inv# " + STRING(ip-inv-no,"99999999").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE list-gl C-Win 
PROCEDURE list-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{sys/form/r-top3w.f} 

def var v-gl-sales as dec format ">>,>>>,>>9.99cr" NO-UNDO.
def var v-dscr like account.dscr NO-UNDO.
def var v-disp-actnum like account.actnum NO-UNDO.
def var v-disp-amt as dec format ">>,>>>,>>9.99cr" NO-UNDO.
def var v-tmp-amt as dec format ">>,>>>,>>9.99cr" NO-UNDO.
def var v-empty as dec format ">>,>>>,>>9.99cr" NO-UNDO.
DEF VAR ld-t AS DEC FORMAT "->>>>9.99" EXTENT 3 NO-UNDO.
DEF VAR ld-pton AS DEC FORMAT "->>>9.999" NO-UNDO.
DEF VAR lv-label-ton AS CHAR FORMAT "x(19)" EXTENT 2 NO-UNDO.
def var v-recid as recid init ?.
DEF VAR lv-rowid AS ROWID NO-UNDO.

def buffer b-tt-report for tt-report.

FORMAT HEADER
       "G/L ACCOUNT NUMBER       "
       "DESCRIPTION                                  "
       "DATE      "
       "         AMOUNT"
       lv-label-ton[1]
       SKIP
       "-------------------------"
       "---------------------------------------------"
       "----------"
       "---------------"
       lv-label-ton[2]

    WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top-s PAGE-TOP WIDTH 150 STREAM-IO.

FORMAT HEADER
       "G/L ACCOUNT NUMBER       "
       "DESCRIPTION                                  "
       "INVOICE#"
       "ITEM#          "
       "         AMOUNT"
       "          TOTAL"
       lv-label-ton[1]
       SKIP
       "-------------------------"
       "---------------------------------------------"
       "--------"
       "---------------"
       "---------------"
       "---------------"
       lv-label-ton[2]

    WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top-d PAGE-TOP WIDTH 150 STREAM-IO.

FORM v-disp-actnum
     v-dscr
     tran-date      FORMAT "99/99/9999"
     v-disp-amt
     ld-pton
     ld-t[2]
     skip
    WITH DOWN NO-BOX NO-LABELS STREAM-IO WIDTH 150 FRAME gl-sum.

FORM account.actnum     
     v-dscr                  
     inv-head.inv-no   FORMAT ">>>>>>>>"           
     inv-line.i-no                  
     v-tmp-amt                     
     v-empty 
     ld-pton 
     ld-t[1]
     SKIP                   

    WITH DOWN NO-BOX NO-LABELS STREAM-IO WIDTH 150 FRAME gl-det.


  FIND first period                   
      where period.company eq gcompany
        and period.pst     le tran-date
        and period.pend    ge tran-date
      no-lock no-error.
 
  assign
   str-tit2 = TRIM(c-win:TITLE) + " - GL POSTING REGISTER - RUN# " + TRIM(STRING(v-trnum))
   {sys/inc/ctrtext.i str-tit2 112}
 
   str-tit3 = "Period " + string(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (string(period.pst) + " to " + string(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 132}.

  IF tb_ton THEN
    ASSIGN
     lv-label-ton[1] = "    $/TON      TONS"
     lv-label-ton[2] = "--------- ---------".

  post-print: DO WHILE TRUE.
    SESSION:SET-WAIT-STATE ("general").

    list-name = TRIM(lv-list-name) + ".001".

    {sys/inc/outprint.i value(lines-per-page)}
  
    VIEW FRAME r-top.

    IF v-gldetail THEN VIEW FRAME f-top-d.
                  ELSE VIEW FRAME f-top-s.

    PAGE.

    /** LIST G/L FOR LINE ITEMS **/
    {oe/r-inve&2.i work-line "ITEMS"}

    /** LIST G/L FOR MISC. **/
    {oe/r-inve&2.i work-misc "MISC."}

    /** LIST G/L FOR SALES TAX **/
    {oe/r-inve&2.i work-tax "SALES TAX"}

    /** LIST G/L FOR CURRENCY GAIN/LOSS **/
    {oe/r-inve&2.i work-curr "CURRENCY GAIN/LOSS"}
    
    /** LIST G/L FOR FG/COGS **/
    if v-gldetail then do:
      assign
       v-disp-amt = 0
       ld-t[2]    = 0.

      for each tmp-work-job break by tmp-work-job.actnum
                                  by tmp-work-job.inv-no:
        find first account where account.company = cocode and
                               account.actnum  = tmp-work-job.actnum
                               no-lock no-error.
        if avail account then
          assign v-dscr = account.dscr.
        else
          assign v-dscr = "ACCOUNT NOT FOUND - " + tmp-work-job.actnum.

        accumulate tmp-work-job.amt (total by tmp-work-job.actnum).
        ld-t[1] = tmp-work-job.weight / 2000.

        if tmp-work-job.fg then
          assign v-tmp-amt  = - tmp-work-job.amt
                 v-disp-amt = v-disp-amt - tmp-work-job.amt
                 ld-t[1]    = - ld-t[1].
        else
          assign v-tmp-amt = tmp-work-job.amt
                 v-disp-amt = v-disp-amt + tmp-work-job.amt.

        assign
         ld-t[2] = ld-t[2] + ld-t[1]
         ld-pton = v-tmp-amt / ld-t[1].

        IF ld-pton EQ ? THEN ld-pton = 0.

        display tmp-work-job.actnum @ account.actnum
            v-dscr
            tmp-work-job.inv-no @ inv-head.inv-no
            tmp-work-job.i-no   @ inv-line.i-no
            v-tmp-amt
            ld-pton WHEN tb_ton
            ld-t[1] WHEN tb_ton
            with frame gl-det.
        down with frame gl-det.

        if last-of(tmp-work-job.actnum) then do:
          put v-disp-amt to 128.
          IF tb_ton THEN DO:
            ld-pton = v-disp-amt / ld-t[2].
            IF ld-pton EQ ? THEN ld-pton = 0.
            PUT ld-pton TO 138 ld-t[2] TO 148 SKIP(1).
          END.
          ELSE PUT skip.
          assign
           v-disp-amt = 0
           ld-t[2]    = 0.
        end.
      end.
    end.

    for each work-job break by work-job.actnum:
      find first account where account.company = cocode and
                               account.actnum  = work-job.actnum
                               no-lock no-error.
      if avail account then
        assign v-dscr = account.dscr.
      else
        assign v-dscr = "ACCOUNT NOT FOUND - " + work-job.actnum.

      assign v-disp-actnum = work-job.actnum
             ld-t[2]       = work-job.weight / 2000.

      if work-job.fg then
        assign v-disp-amt = - work-job.amt
               ld-t[2]    = - ld-t[2].
      else
        assign v-disp-amt = work-job.amt.

      ld-pton = v-disp-amt / ld-t[2].

      IF ld-pton EQ ? THEN ld-pton = 0.

      if not v-gldetail then do:
        disp v-disp-actnum
             v-dscr
             tran-date
             v-disp-amt
             ld-pton WHEN tb_ton
             ld-t[2] WHEN tb_ton
           with frame gl-sum.
        down with frame gl-sum.
      end.

      assign
       v-balance = v-balance + v-disp-amt
       ld-t[3]   = ld-t[3] + ld-t[2].
    end. /* each work-job */
                                                  /** POST FREIGHT TO G/L **/
    find first account
         where account.company eq cocode
           and account.actnum  eq v-ar-freight
         no-lock no-error.
    assign
      v-dscr     = if avail account then account.dscr
                   else "ACCOUNT NOT FOUND - FREIGHT"
      v-disp-amt = 0
      ld-t[2]    = 0.

    if v-gldetail then do:
      for each tt-report
          where tt-report.term-id eq ""
            and tt-report.key-01  eq "work-freight"
          no-lock
          break by tt-report.key-02:

        assign
         ld-t[1]    = tt-report.weight / 2000
         v-disp-amt = v-disp-amt + dec(tt-report.key-05)
         ld-t[2]    = ld-t[2] + ld-t[1].

        if dec(tt-report.key-05) ne 0 then do:
          ld-pton = dec(tt-report.key-05) / ld-t[1].

          IF ld-pton EQ ? THEN ld-pton = 0.

          display v-ar-freight          @ account.actnum
                  v-dscr
                  int(tt-report.key-02) @ inv-head.inv-no
                  "FREIGHT"             @ inv-line.i-no
                  dec(tt-report.key-05) @ v-tmp-amt
                  ld-pton WHEN tb_ton
                  ld-t[1] WHEN tb_ton
              with frame gl-det.
          down with frame gl-det.
        end.
      end.

      if v-disp-amt ne 0 then do:
        put v-disp-amt to 128.
        IF tb_ton THEN DO:
          ld-pton = v-disp-amt / ld-t[2].
          IF ld-pton EQ ? THEN ld-pton = 0.
          PUT ld-pton TO 138 ld-t[2] TO 148 SKIP(1).
        END.
        ELSE PUT skip.
        assign
         v-disp-amt = 0
         ld-t[2]    = 0.
      end.
    end.

    assign
     v-disp-actnum = v-ar-freight
     v-disp-amt    = v-post-freight
     ld-t[2]       = v-post-freight-w / 2000.

    if not v-gldetail then do:
      ld-pton = v-disp-amt / ld-t[2].

      IF ld-pton EQ ? THEN ld-pton = 0.

      display v-disp-actnum
              v-dscr
              tran-date
              v-disp-amt
              ld-pton WHEN tb_ton
              ld-t[2] WHEN tb_ton
          with frame gl-sum.
      down with frame gl-sum.
    end.

    v-balance = v-balance + v-post-freight.
                                                  /** POST DISCOUNT TO G/L **/
    find first account
         where account.company eq cocode
           and account.actnum  eq v-ar-disc
         no-lock no-error.
    assign
      v-dscr     = if avail account then account.dscr
                   else "ACCOUNT NOT FOUND - DISCOUNT"
      v-disp-amt = 0
      ld-t[2]    = 0.

    if v-gldetail then do:
      for each tt-report
          where tt-report.term-id eq ""
            and tt-report.key-01  eq "work-disc"
          no-lock
          break by tt-report.key-02:

        assign
         ld-t[1]    = tt-report.weight / 2000
         v-disp-amt = v-disp-amt + dec(tt-report.key-05)
         ld-t[2]    = ld-t[2] + ld-t[1].

        if dec(tt-report.key-05) ne 0 then do:
          ld-pton = dec(tt-report.key-05) / ld-t[1].

          IF ld-pton EQ ? THEN ld-pton = 0.

          display v-ar-disc             @ account.actnum
                  v-dscr
                  int(tt-report.key-02) @ inv-head.inv-no
                  "DISCOUNT"            @ inv-line.i-no
                  dec(tt-report.key-05) @ v-tmp-amt
                  ld-pton WHEN tb_ton
                  ld-t[1] WHEN tb_ton
              with frame gl-det.
          down with frame gl-det.
        end.
      end.

      if v-disp-amt ne 0 then do:
        put v-disp-amt to 128.
        IF tb_ton THEN DO:
          ld-pton = v-disp-amt / ld-t[2].
          IF ld-pton EQ ? THEN ld-pton = 0.
          PUT ld-pton TO 138 ld-t[2] TO 148 SKIP(1).
        END.
        ELSE PUT skip.
        assign
         v-disp-amt = 0
         ld-t[2]    = 0.
      end.
    end.

    assign
     v-disp-actnum = v-ar-disc
     v-disp-amt    = v-post-disc
     ld-t[2]       = v-post-disc-w / 2000.

    if not v-gldetail then do:
      ld-pton = v-disp-amt / ld-t[2].

      IF ld-pton EQ ? THEN ld-pton = 0.

      display v-disp-actnum
              v-dscr
              tran-date
              v-disp-amt
              ld-pton WHEN tb_ton
              ld-t[2] WHEN tb_ton
          with frame gl-sum.
      down with frame gl-sum.
    end.

    v-balance = v-balance + v-disp-amt.
                                                     /** POST CASH TO G/L **/
    if v-post-cash ne 0 then do:
      find first account
          where account.company eq cocode
            and account.actnum  eq ar-ctrl.cash-act
          no-lock no-error.
      v-dscr = if avail account then account.dscr
               else "ACCOUNT NOT FOUND - CASH".

      if v-gldetail then do:
        assign
         v-disp-amt = 0
         ld-t[2]    = 0.

        for each tt-report
            where tt-report.term-id eq ""
              and tt-report.key-01  eq "work-cash"
            no-lock
            break by tt-report.key-02:

          assign
           ld-t[1]    = tt-report.weight / 2000
           v-disp-amt = v-disp-amt + dec(tt-report.key-05)
           ld-t[2]    = ld-t[2] + ld-t[1].

          if dec(tt-report.key-05) ne 0 then do:
            ld-pton = dec(tt-report.key-05) / ld-t[1].

            IF ld-pton EQ ? THEN ld-pton = 0.

            display ar-ctrl.cash-act    @ account.actnum
                    v-dscr
                    int(tt-report.key-02)  @ inv-head.inv-no
                    "CASH INVOICE"      @ inv-line.i-no
                    dec(tt-report.key-05)  @ v-tmp-amt
                    ld-pton WHEN tb_ton
                    ld-t[1] WHEN tb_ton
                with frame gl-det.
            down with frame gl-det.
          end.
        end.

        if v-disp-amt ne 0 then do:
          put v-disp-amt to 128.
          IF tb_ton THEN DO:
            ld-pton = v-disp-amt / ld-t[2].
            IF ld-pton EQ ? THEN ld-pton = 0.
            PUT ld-pton TO 138 ld-t[2] TO 148 SKIP(1).
          END.
          ELSE PUT skip.
          assign
           v-disp-amt = 0
           ld-t[2]    = 0.
        end.
      end.

      assign
       v-disp-actnum = ar-ctrl.cash-act
       v-disp-amt    = v-post-cash
       ld-t[2]       = v-post-cash-w / 2000.

      if not v-gldetail then do:
        ld-pton = v-disp-amt / ld-t[2].

        IF ld-pton EQ ? THEN ld-pton = 0.

        display v-disp-actnum
                v-dscr
                tran-date
                v-disp-amt
                ld-pton WHEN tb_ton
                ld-t[2] WHEN tb_ton 
            with frame gl-sum.
        down with frame gl-sum.
      end.

      v-balance = v-balance + v-disp-amt.
    end.  
                                                  /** OFFSET ENTRY TO G/L **/
    find first account
        where account.company = cocode
          and account.actnum  = v-ar-acct
        no-lock no-error.
    assign
     v-dscr        = if avail account then account.dscr
                     else "ACCOUNT NOT FOUND - OFFSET"
     v-disp-actnum = v-ar-acct
     v-disp-amt    = v-post-total.

    if v-gldetail then do:
      assign
       ld-t[1]       = v-post-total-w / 2000
       ld-pton       = v-disp-amt / ld-t[1].

      IF ld-pton EQ ? THEN ld-pton = 0.

      display v-ar-acct     @ account.actnum
              v-dscr
              v-disp-amt    @ v-tmp-amt
              ld-pton WHEN tb_ton
              ld-t[1] WHEN tb_ton 
          with frame gl-det.
      down with frame gl-det.
    end.

    else do:
      assign
       ld-t[2]       = v-post-total-w / 2000
       ld-pton       = v-disp-amt / ld-t[2].

      IF ld-pton EQ ? THEN ld-pton = 0.

      display v-disp-actnum
              v-dscr
              tran-date
              v-disp-amt
              ld-pton WHEN tb_ton
              ld-t[2] WHEN tb_ton 
          with frame gl-sum.
      down with frame gl-sum.
    end.

    v-balance = v-balance + v-post-total.   
    if v-gldetail then
      put v-disp-amt to 128 skip
          "---------------"  to 128 skip
          "Total:" at 86 v-balance to 128 SKIP.
    else
      put "---------------"  to 104 skip
          "Total:" at 79 v-balance to 104 SKIP.

    SESSION:SET-WAIT-STATE ("").

    IF ROUND(v-balance,2) NE 0 THEN DO:
      OUTPUT CLOSE.

      FIND FIRST tt-report {oe/invpost7.i} NO-LOCK NO-ERROR.
      lv-rowid = IF AVAIL tt-report THEN ROWID(tt-report) ELSE ?.

      RUN oe/invpost7.p (ROUND(v-balance,2), INPUT-OUTPUT lv-rowid).

      FIND tt-report WHERE ROWID(tt-report) EQ lv-rowid NO-LOCK NO-ERROR.

      IF AVAIL tt-report THEN DO:
        CREATE b-tt-report. 
        ASSIGN
         b-tt-report.term-id = ""
         b-tt-report.key-01  = tt-report.key-01
         b-tt-report.key-02  = tt-report.key-02
         v-balance           = v-balance * -1
         b-tt-report.key-05  = STRING(ROUND(v-balance,2),"-999,999,999.99")
         v-recid             = RECID(b-tt-report).

        NEXT post-print.
      END.
    END.

    leave.
  end. /* post-print */

  OUTPUT CLOSE.

  IF OPSYS EQ "unix" THEN DO:
    UNIX SILENT cat VALUE(list-name) >> VALUE(lv-list-name).
    UNIX SILENT rm  VALUE(list-name).
  END.

  ELSE DO:
    DOS SILENT TYPE VALUE(list-name) >> VALUE(lv-list-name).
    DOS SILENT DEL  VALUE(list-name).
  END.

  list-name = lv-list-name.

  SESSION:SET-WAIT-STATE ("general").

  FOR EACH tt-report WHERE RECID(tt-report) NE v-recid:
    DELETE tt-report.
  END.

  SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE list-post-inv C-Win 
PROCEDURE list-post-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-list-post AS CHAR NO-UNDO.

  DEF BUFFER b-oe-boll FOR oe-boll.

  DEF VAR ld-t AS DEC FORMAT "->>>>9.99" EXTENT 3 NO-UNDO.
  DEF VAR ld-pton AS DEC FORMAT "->>>9.999" NO-UNDO.
  DEFINE VARIABLE v-close-line-ok AS LOGICAL INITIAL NO.
  DEFINE VAR v-first AS LOG INIT YES.
  DEFINE VAR v-tot-frt AS DEC NO-UNDO.
  format
    inv-head.inv-no at 1
    inv-head.inv-date at 8 FORMAT "99/99/99"
    inv-head.cust-no at 17
    inv-head.cust-name format "x(25)" at 26
    v-ord-no to 59
    v-inv-qty
    inv-head.t-inv-freight format "->,>>9.99"
    inv-head.t-inv-tax format "->,>>9.99"
    v-misc-tot format "->>>>9.99"
    v-line-tot format "->>>>>>9.99"
    inv-head.t-inv-rev to 131
    ld-pton
    ld-t[2]
    with STREAM-IO width 151 no-labels no-box no-underline frame inv.

  format
    w-inv-line.i-no at 10 label "Item"
    w-inv-line.i-name format "x(25)" label "Description"
    w-inv-line.qty format "->>,>>>,>>9" label "Order"
    w-inv-line.inv-qty format "->>,>>>,>>9" column-label "Quantities!Invoiced "
    w-inv-line.ship-qty format "->>,>>>,>>9" label "Shipped"
    w-inv-line.t-cost format "->>>,>>9.99<<<<" label "Cost"
    w-inv-line.price format "->>>,>>9.99<<<<" label "Price"
    w-inv-line.uom label "UOM"
    w-inv-line.t-price column-label "Extended! Price"
    v-prof  FORMAT "->>>9.99%" COLUMN-LABEL "Profit"
    with down no-box STREAM-IO width 161 frame invl.

  format
    w-inv-line.i-no at 10 label "Item"
    w-inv-line.i-name format "x(25)" label "Description"
    w-inv-line.qty format "->>,>>>,>>9" label "Order"
    w-inv-line.inv-qty format "->>,>>>,>>9" column-label "Quantities!Invoiced "
    w-inv-line.ship-qty format "->>,>>>,>>9" label "Shipped"
    w-inv-line.t-cost format "->>>,>>9.99<<<<" label "Cost"
    w-inv-line.price format "->>>,>>9.99<<<<" label "Price"
    w-inv-line.uom label "UOM"
    w-inv-line.t-price column-label "Extended! Price"
    ld-pton column-label "!     $/Ton"
    ld-t[1] column-label "!      Tons"
    v-prof  FORMAT "->>>9.99%" COLUMN-LABEL "Profit"
    with down no-box STREAM-IO width 171 frame invlt.

  format
    w-ord-misc.charge at 10 label "Charge"
    w-ord-misc.dscr label "Description"
    w-ord-misc.amt format "->>>,>>9.99" to 71 label "Price" skip
    with STREAM-IO down no-box frame invm.
  
  SESSION:SET-WAIT-STATE ("general").

  RUN oe/invpostd.p ("").
      
  v-post = ip-list-post EQ "post".

  DISABLE TRIGGERS FOR LOAD OF inv-head.
  DISABLE TRIGGERS FOR LOAD OF inv-line.
  DISABLE TRIGGERS FOR LOAD OF oe-ord.
  DISABLE TRIGGERS FOR LOAD OF oe-ordl.
  DISABLE TRIGGERS FOR LOAD OF itemfg.
  DISABLE TRIGGERS FOR LOAD OF oe-relh.
  DISABLE TRIGGERS FOR LOAD OF oe-rell.

  ordblock:
  FOR EACH w-report WHERE w-report.term-id EQ "" NO-LOCK,
    
      FIRST inv-head WHERE RECID(inv-head) EQ w-report.rec-id
        
      TRANSACTION
      
      BY w-report.key-01:

    {oe/r-inve&p.i}
  END.

  FIND CURRENT inv-head NO-LOCK NO-ERROR.
  FIND CURRENT inv-line NO-LOCK NO-ERROR.
  FIND CURRENT itemfg NO-LOCK NO-ERROR.
  FIND CURRENT oe-ordl NO-LOCK NO-ERROR.
  FIND CURRENT ar-invl NO-LOCK NO-ERROR.
  FIND CURRENT oe-ordm NO-LOCK NO-ERROR.
  FIND CURRENT cust NO-LOCK NO-ERROR.

  SESSION:SET-WAIT-STATE ("").

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
  DEF VAR lv-dscr LIKE gltrans.tr-dscr NO-UNDO. 
   

  /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
  DO TRANSACTION:
    FOR EACH tt-gl,
        FIRST gltrans WHERE ROWID(gltrans) EQ tt-gl.row-id:
      DELETE gltrans.
    END.

    EMPTY TEMP-TABLE tt-gl.
                                         /** POST LINE ITEMS TO G/L TRANS **/
    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "work-line"
        no-lock
        break by tt-report.key-02
              by tt-report.key-03:

      accumulate dec(tt-report.key-05) (total by tt-report.key-03).

      if last-of(tt-report.key-03) then do:
        RUN get-tr-dscr (INT(tt-report.key-03), OUTPUT lv-dscr).

        create tt-gl.
        create gltrans.
        assign
         tt-gl.row-id    = rowid(gltrans)
         gltrans.company = cocode
         gltrans.actnum  = tt-report.key-02
         gltrans.jrnl    = "OEINV"
         gltrans.tr-dscr = TRIM(lv-dscr) + " LINE"
         gltrans.tr-date = tran-date
         gltrans.tr-amt  = - (accumulate total by tt-report.key-03 dec(tt-report.key-05))
         gltrans.period  = tran-period
         gltrans.trnum   = v-trnum.
        RELEASE gltrans.
      end. /* last actnum */
    end. /* each work-line */
                                              /** POST MISC. TO G/L TRANS **/
    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "work-misc"
        no-lock
        break by tt-report.key-02
              by tt-report.key-03:

      accumulate dec(tt-report.key-05) (total by tt-report.key-03).

      if last-of(tt-report.key-03) then do:
        RUN get-tr-dscr (INT(tt-report.key-03), OUTPUT lv-dscr).

        create tt-gl.
        create gltrans.
        assign
         tt-gl.row-id    = rowid(gltrans)
         gltrans.company = cocode
         gltrans.jrnl    = "OEINV"
         gltrans.tr-dscr = TRIM(lv-dscr) + " MISC"
         gltrans.tr-date = tran-date
         gltrans.actnum  = tt-report.key-02
         gltrans.tr-amt  = - (accumulate total by tt-report.key-03 dec(tt-report.key-05))
         gltrans.period  = tran-period
         gltrans.trnum   = v-trnum.
      end. /* last actnum */
    end. /* each work-misc */
                                           /** POST SALES TAX TO G/L TRANS **/
    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "work-tax"
        no-lock
        break by tt-report.key-02
              by tt-report.key-03:

      accumulate dec(tt-report.key-05) (total by tt-report.key-03).

      if last-of(tt-report.key-03) then do:
        RUN get-tr-dscr (INT(tt-report.key-03), OUTPUT lv-dscr).

        create tt-gl.
        create gltrans.
        assign
         tt-gl.row-id    = rowid(gltrans)
         gltrans.company = cocode
         gltrans.actnum  = tt-report.key-02
         gltrans.jrnl    = "OEINV"
         gltrans.tr-dscr = TRIM(lv-dscr) + " TAX"
         gltrans.tr-date = tran-date
         gltrans.tr-amt  = - (accumulate total by tt-report.key-03 dec(tt-report.key-05))
         gltrans.period  = tran-period
         gltrans.trnum   = v-trnum.
        RELEASE gltrans.
      end. /* last actnum */
    end. /* each work-tax */
                                           /** POST CURRENCY TO G/L TRANS **/
    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "work-curr"
        no-lock
        break by tt-report.key-02:

      accumulate dec(tt-report.key-05) (total by tt-report.key-02).

      if last-of(tt-report.key-02) then do:
        create tt-gl.
        create gltrans.
        assign
         tt-gl.row-id    = rowid(gltrans)
         gltrans.company = cocode
         gltrans.actnum  = tt-report.key-02
         gltrans.jrnl    = "OEINV"
         gltrans.tr-dscr = "ORDER ENTRY INVOICE CURRENCY GAIN/LOSS"
         gltrans.tr-date = tran-date
         gltrans.tr-amt  = - (accumulate total by tt-report.key-02 dec(tt-report.key-05))
         gltrans.period  = tran-period
         gltrans.trnum   = v-trnum.

        RELEASE gltrans.
      end. /* last actnum */
    end. /* each work-tax */

    for each tmp-work-job
        break by tmp-work-job.fg
              by tmp-work-job.actnum
              by tmp-work-job.inv-no:

      accumulate tmp-work-job.amt (total by tmp-work-job.inv-no).

      if last-of(tmp-work-job.inv-no) then do:
        RUN get-tr-dscr (tmp-work-job.inv-no, OUTPUT lv-dscr).

        create tt-gl.
        create gltrans.
        assign
         tt-gl.row-id    = rowid(gltrans)
         gltrans.company = cocode
         gltrans.actnum  = tmp-work-job.actnum
         gltrans.jrnl    = "OEINV"
         gltrans.tr-date = tran-date
         gltrans.period  = tran-period
         gltrans.trnum   = v-trnum.

        if tmp-work-job.fg then
          assign
           gltrans.tr-amt  = - (accumulate total by tmp-work-job.inv-no tmp-work-job.amt)
           gltrans.tr-dscr = TRIM(lv-dscr) + " FG".
        else
          assign
           gltrans.tr-amt  = (accumulate total by tmp-work-job.inv-no tmp-work-job.amt)
           gltrans.tr-dscr = TRIM(lv-dscr) + " COGS".

        RELEASE gltrans.
      end.
    end. /* each work-job */

                                          /** POST FREIGHT TO G/L TRANS **/
    create tt-gl.
    create gltrans.
    assign
     tt-gl.row-id    = rowid(gltrans)
     gltrans.company = cocode
     gltrans.actnum  = v-ar-freight
     gltrans.jrnl    = "OEINV"
     gltrans.tr-dscr = "ORDER ENTRY INVOICE FREIGHT"
     gltrans.tr-date = tran-date
     gltrans.tr-amt  = v-post-freight
     gltrans.period  = tran-period
     gltrans.trnum   = v-trnum.
    RELEASE gltrans. 
                                           /** POST DISCOUNT TO G/L TRANS **/
    create tt-gl.
    create gltrans.
    assign
     tt-gl.row-id    = rowid(gltrans) 
     gltrans.company = cocode
     gltrans.actnum  = v-ar-disc
     gltrans.jrnl    = "OEINV"
     gltrans.tr-dscr = "ORDER ENTRY INVOICE DISCOUNT"
     gltrans.tr-date = tran-date
     gltrans.tr-amt  = v-post-disc
     gltrans.period  = tran-period
     gltrans.trnum   = v-trnum.
    RELEASE gltrans.

                                           /** POST CASH TO G/L TRANS **/
    if v-post-cash ne 0 then do:
      create tt-gl.
      create gltrans.
      assign
       tt-gl.row-id    = rowid(gltrans)
       gltrans.company = cocode
       gltrans.actnum  = ar-ctrl.cash-act
       gltrans.jrnl    = "CASHR"
       gltrans.tr-dscr = "CASH RECEIPT - INVOICE"
       gltrans.tr-date = tran-date
       gltrans.tr-amt  = v-post-cash
       gltrans.period  = tran-period
       gltrans.trnum   = v-trnum
       v-post-cash = - v-post-cash.
      RELEASE gltrans.
    end.
                                                  /** OFFSET ENTRY TO G/L **/
    create tt-gl.
    create gltrans.
    assign
     tt-gl.row-id    = rowid(gltrans)
     gltrans.company = cocode
     gltrans.actnum  = v-ar-acct
     gltrans.jrnl    = "OEINV"
     gltrans.tr-dscr = "ORDER ENTRY INVOICE"
     gltrans.tr-date = tran-date
     gltrans.tr-amt  = v-post-total
     gltrans.period  = tran-period
     gltrans.trnum   = v-trnum.
    RELEASE gltrans.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- oe/invpost.p 10/94 gb */
/* Invoicing  - Edit Register & Post Invoicing Transactions                   */
/* -------------------------------------------------------------------------- */
  DEF BUFFER xinv-head FOR inv-head.
  DEF VAR str-tit4 AS CHAR FORMAT "x(20)" NO-UNDO.
  DEF VAR lv-label-ton AS CHAR FORMAT "x(20)" EXTENT 2 NO-UNDO.
  DEF VAR v-contsrvc-export-found AS LOG NO-UNDO.
  DEF VAR v-goodman-export-found AS LOG NO-UNDO.


  {sys/form/r-top3w.f}

  format header
    str-tit4 AT 58
    SKIP(1)
    "  - Invoice - " skip
    "Number"  "Date" at 10  "Cust#" at 17 "Customer Name" at 26 "Order#" to 59
    "Quantity" to 74 "Frt" to 84 "Tax" to 94
    "Misc" to 104 "Items" to 116
    "Total" to 131 
    lv-label-ton[1] TO 151
    fill("=",131) format "x(131)"
    lv-label-ton[2] TO 151
    with frame r-top WIDTH 151.
  
  FIND first period                   
      where period.company eq gcompany
        and period.pst     le tran-date
        and period.pend    ge tran-date
      no-lock no-error.
 
  ASSIGN
   str-tit2 = TRIM(c-win:TITLE) + " - " +
              TRIM(STRING(tb_detailed,"Detail/Summary")) +
              " - RUN# " + TRIM(STRING(v-trnum))
   {sys/inc/ctrtext.i str-tit2 112}
 
   str-tit3 = "Period " + string(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 132}
   
   str-tit4 = "Post Date " + STRING(tran-date, "99/99/99")
   v-s-inv-no = begin_inv
   v-e-inv-no = end_inv
   v-s-date   = begin_date
   v-e-date   = end_date
   v-detail   = tb_detailed
   v-gldetail = tb_detailed-2
   v-postable = NO.

  IF tb_ton THEN
    ASSIGN
     lv-label-ton[1] = "     $/Ton      Tons"
     lv-label-ton[2] = "====================".

  SESSION:SET-WAIT-STATE ("general").

  EMPTY TEMP-TABLE w-report.

  for each inv-head NO-LOCK
      where inv-head.company  eq cocode
        and inv-head.printed  eq yes
        and inv-head.inv-no   gt 0
        and inv-head.inv-no   ge v-s-inv-no
        and inv-head.inv-no   le v-e-inv-no
        and inv-head.inv-date ge v-s-date
        and inv-head.inv-date le v-e-date
        and inv-head.stat     ne "H"
      use-index prnt,
             
      first cust NO-LOCK
      where cust.company eq cocode
        AND cust.cust-no eq inv-head.cust-no
        AND ((cust.inv-meth EQ ? AND inv-head.multi-invoice) OR
             (cust.inv-meth NE ? AND NOT inv-head.multi-invoice))
      TRANSACTION:

    find FIRST xinv-head where recid(xinv-head) eq recid(inv-head)
        exclusive-lock NO-WAIT no-error.
    if avail xinv-head then do:
      create w-report.
      assign
       w-report.term-id = ""
       w-report.key-01  = STRING(xinv-head.inv-no,"9999999999")
       w-report.rec-id  = RECID(xinv-head).
      create tt-report.
      assign
       tt-report.term-id = ""
       tt-report.key-01  = STRING(xinv-head.inv-no,"9999999999")
       tt-report.rec-id  = recid(xinv-head).
           
      IF inv-head.multi-invoice THEN
        IF CAN-FIND(FIRST b-inv-head
                    WHERE b-inv-head.company       EQ inv-head.company
                      AND b-inv-head.cust-no       EQ inv-head.cust-no
                      AND b-inv-head.inv-no        EQ inv-head.inv-no
                      AND b-inv-head.multi-invoice EQ NO) THEN
        FOR EACH b-inv-head NO-LOCK
            WHERE b-inv-head.company       EQ inv-head.company
              AND b-inv-head.cust-no       EQ inv-head.cust-no
              AND b-inv-head.inv-no        EQ inv-head.inv-no
              AND b-inv-head.multi-invoice EQ NO:

          RUN create-save-line.
        END.

        ELSE DO:
          DELETE tt-report.
          DELETE w-report.
          DELETE xinv-head.
          NEXT.
        END.

      IF cust.factored THEN
      for each inv-line no-lock where inv-line.r-no = inv-head.r-no:
           IF CAN-FIND(FIRST reftable WHERE reftable.reftable EQ "FACTORED"
                             AND reftable.company  EQ inv-head.company
                             AND reftable.loc      EQ ""
                             AND reftable.code     EQ inv-line.i-no)
            THEN DO:
                tt-report.key-02 = "Factored".  /* for oe/rep/expfrank.p task#  09200521*/
                LEAVE.
            END.
      END.
    end.
  end.

  SESSION:SET-WAIT-STATE ("").

  {sys/inc/print1.i}

  {sys/inc/outprint.i VALUE(lines-per-page)}

  IF td-show-parm THEN RUN show-param.

  lv-list-name = list-name.
  
  DISPLAY WITH FRAME r-top.

  RUN list-post-inv ("list").
  
  OUTPUT CLOSE.

  /* export invoices to factor */   
  v-ftp-done = NO.

  IF tb_export AND inexport-log THEN DO:    
     DEF VAR v-exp-file AS cha NO-UNDO.
     v-exp-file = inexport-desc +  
                "INVOICE_" + /*trim(v-print-fmt) + */
                    substr(string(year(today),"9999"),3,2) +
                    string(month(today),"99") +
                    string(day(today),"99") +
                    substr(string(time,"HH:MM:SS"),1,2) +
                    substr(string(time,"HH:MM:SS"),4,2) +
                    substr(string(time,"HH:MM:SS"),7,2) + ".dat".

     IF (v-print-fmt = "Frankstn" OR v-print-fmt = "MIRPKG" ) AND inexport-cha EQ "CIT" THEN DO:
        OUTPUT TO VALUE(v-exp-file).
        RUN oe/rep/expfrank.p .
        OUTPUT CLOSE.
        OUTPUT TO VALUE(".\oe\ftpcmd2.txt").     /* ftp text file */
        PUT UNFORMATTED 
           "open cs.ftp.citonline.com" SKIP  /* ftp server ip address */
           "ftpa1526" SKIP  /* userid*/
           "none" SKIP  /* password */
           "put " v-exp-file " " '"' "$$ ID=EP003F BID='DI1526' PASSWORD=NARF" '"' SKIP         /* file to transfer */
           "quit" .
         OUTPUT CLOSE.
         OS-COMMAND SILENT value("ftp -v -i -s:.\oe\ftpcmd2.txt"). 
         v-ftp-done = YES.
     END.
     ELSE
     IF inexport-cha EQ "ContSrvc" THEN DO:
        v-ftp-done = YES.

        for each tt-report NO-LOCK where tt-report.term-id eq "",
            first inv-head where recid(inv-head) eq tt-report.rec-id no-lock,
            FIRST cust WHERE cust.company = inv-head.company AND
                  cust.cust-no = inv-head.cust-no AND
                  cust.an-edi-cust AND
                  NOT CAN-FIND(FIRST sys-ctrl-shipto WHERE
                      sys-ctrl-shipto.company EQ cust.company AND
                      sys-ctrl-shipto.NAME EQ "INEXPORT" AND
                      sys-ctrl-shipto.cust-vend EQ YES AND
                      sys-ctrl-shipto.cust-vend-no EQ cust.cust-no AND
                      sys-ctrl-shipto.char-fld EQ "GOODMAN")
             NO-LOCK:

             v-contsrvc-export-found = YES.
             LEAVE.
        END.

        IF v-contsrvc-export-found THEN
        DO:
           OUTPUT TO VALUE(v-exp-file).
           RUN oe/rep/expconts.p .
           OUTPUT CLOSE.
        END.

        for each tt-report NO-LOCK where tt-report.term-id eq "",
            first inv-head where recid(inv-head) eq tt-report.rec-id no-lock,
            FIRST cust WHERE cust.company = inv-head.company AND
                  cust.cust-no = inv-head.cust-no AND
                  cust.an-edi-cust AND
                  CAN-FIND(FIRST sys-ctrl-shipto WHERE
                      sys-ctrl-shipto.company EQ cust.company AND
                      sys-ctrl-shipto.NAME EQ "INEXPORT" AND
                      sys-ctrl-shipto.cust-vend EQ YES AND
                      sys-ctrl-shipto.cust-vend-no EQ cust.cust-no AND
                      sys-ctrl-shipto.char-fld EQ "GOODMAN")
             NO-LOCK:
             v-goodman-export-found = YES.
             LEAVE.
        END.

        IF v-goodman-export-found THEN
        DO:
           v-exp-file = inexport-desc +  
                        "INVOICEG_" + 
                        substr(string(year(today),"9999"),3,2) +
                        string(month(today),"99") +
                        string(day(today),"99") +
                        substr(string(time,"HH:MM:SS"),1,2) +
                        substr(string(time,"HH:MM:SS"),4,2) +
                        substr(string(time,"HH:MM:SS"),7,2) + ".dat".
           OUTPUT TO VALUE(v-exp-file).
           RUN oe/rep/expgoodman.p .
           OUTPUT CLOSE.
        END.
     END.

  END.
  /* end of export */

  IF v-postable THEN RUN list-gl.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  EMPTY TEMP-TABLE tt-report.

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
  def var lv-label as cha NO-UNDO.
  
  ASSIGN
  lv-frame-hdl = frame {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:first-child
  lv-field-hdl = lv-group-hdl:first-child.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undo-save-line C-Win 
PROCEDURE undo-save-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DISABLE TRIGGERS FOR LOAD OF inv-line.
  DISABLE TRIGGERS FOR LOAD OF inv-misc.


  RELEASE inv-line.
  RELEASE inv-misc.

  FIND FIRST inv-line WHERE RECID(inv-line) EQ INT(save-line.val[3]) NO-ERROR.

  IF AVAIL inv-line THEN inv-line.r-no = save-line.val[1].

  ELSE
  FIND FIRST inv-misc WHERE RECID(inv-misc) EQ INT(save-line.val[3]) NO-ERROR.

  IF AVAIL inv-misc THEN inv-misc.r-no = save-line.val[1].

  DELETE save-line.

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

