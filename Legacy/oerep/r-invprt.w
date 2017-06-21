&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : oerep\r-invprt.w

  Description       : Print Invoices

  Input Parameters  : None

  Output Parameters : None

  Author            : ASI

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
/* problem with of statements and checkbol include, soldno, multi-invoice */
&scoped-define LINE ar-invl
&scoped-define head ar-inv
&scoped-define bolno inv-no
&scoped-define multiinvoice exported
&scoped-define soldno spare-char-1
&scoped-define rno inv-no
&scoped-define miscrno r-no
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF    VAR      list-name  AS cha       NO-UNDO.
DEFINE VARIABLE init-dir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cActualPdf AS CHARACTER NO-UNDO.
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

{oe/rep/invoice.i "new"}

DEF VAR v-program      AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG  NO-UNDO.
DEF VAR ls-fax-file    AS cha  NO-UNDO.
{custom/xprint.i}

DEF VAR lv-multi-faxout AS LOG  NO-UNDO.  /*for faxing to multiple receipents */
DEF VAR lv-fax-image    AS cha  NO-UNDO.  /* fax imge file */
DEF VAR lv-prt-bypass   AS LOG  NO-UNDO.  /* bypass window's printer driver */

DEF VAR v-ftp-done      AS LOG  NO-UNDO.
DEF VAR vcInvNums       AS CHAR NO-UNDO.
DEF VAR lv-pdf-file     AS CHAR NO-UNDO.
DEF VAR vcDefaultForm   AS CHAR NO-UNDO.
DEF VAR vcBOLFiles      AS CHAR NO-UNDO.
DEF VAR vcBOLSignDir    AS CHAR NO-UNDO.
DEF VAR v-rec-found     AS LOG  NO-UNDO.



DEF BUFFER b-{&head}1   FOR {&head}.
DEF BUFFER save-line    FOR reftable.
DEF BUFFER b1-cust      FOR cust.
DEF BUFFER b-ar-inv     FOR ar-inv.
DEF BUFFER buf-{&head} FOR {&head}.
DEF BUFFER b2-{&head}  FOR {&head}.
DEF BUFFER b-cust       FOR cust.
DEF BUFFER b-broker-bol FOR reftable.

/* gdm - 12080817 */
DEF NEW SHARED    VAR      nsv_setcomp      AS LOGICAL NO-UNDO.
DEF NEW SHARED    VAR      s-print-zero-qty AS LOG     NO-UNDO.

/* br Task 12081002 - to pass which item to print on invoice */
DEFINE NEW SHARED VARIABLE svi-print-item   AS INTEGER INITIAL 1 NO-UNDO.

{ar/rep/invoice2.i "new"}

DO WITH TRANSACTION:
    {sys\inc\invpass.i}
END.

RUN sys/ref/nk1look.p (cocode, "BOLSign", "C", NO, NO, "", "", 
    OUTPUT vcBOLSignDir, OUTPUT v-rec-found).

DEFINE VARIABLE retcode        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL   NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.                       

/* Build a Table to keep sequence of pdf files */
DEFINE NEW SHARED TEMP-TABLE tt-filelist
    FIELD tt-FileCtr  AS INT
    FIELD tt-FileName AS CHAR
    INDEX filelist IS PRIMARY TT-FILECTR.

/* Output selection for the report */
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHAR      NO-UNDO.
DEFINE NEW SHARED VARIABLE CallingParameter  AS CHAR      NO-UNDO.

DEFINE            VARIABLE vcBegCustNo       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE vcEndCustNo       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE vlSkipRec         AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE vcHoldStats       AS CHAR      INIT "H,W" NO-UNDO.

DEFINE            VARIABLE glPaperless       AS LOGICAL   NO-UNDO.

{XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XMLInvoice &Company=cocode} /* rstark 05181205 */
{XMLOutput/XMLOutput.i &NEW=NEW &cXMLSysCtrl=cXMLInvoice &Company=cocode &c=c} /* rstark 05291402 */

DEF VAR vSoldToNo AS CHAR NO-UNDO.  /* to hold soldto# for email */
DEF VAR vShipToNo AS CHAR NO-UNDO.  /* to hold shipto# for email */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_cust end_cust begin_inv ~
end_inv begin_date end_date begin_bol end_bol tb_reprint tb_posted ~
tb_prt-inst tb_setcomp rd_sort tb_BatchMail tb_HideDialog tb_attachBOL ~
rd-dest lv-ornt lines-per-page lv-font-no tb_email-orig tb_override-email ~
td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_inv end_inv ~
begin_date end_date begin_bol end_bol tb_reprint tb_posted tb_prt-inst ~
tb_setcomp lbl_sort rd_sort tb_BatchMail tb_HideDialog tb_attachBOL rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name tb_email-orig ~
tb_override-email td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetPaperlessLogical C-Win 
FUNCTION GetPaperlessLogical RETURNS LOGICAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE begin_bol         AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    LABEL "Beginning BOL#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_cust        AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_date        AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning BOL Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_inv         AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    LABEL "Beginning Invoice#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_bol           AS INTEGER   FORMAT ">>>>>>>9" INITIAL 99999999 
    LABEL "Ending BOL#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust          AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date          AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending BOL Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv           AS INTEGER   FORMAT ">>>>>>>>" INITIAL 99999999 
    LABEL "Ending Invoice#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_broker-bol     AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    LABEL "Broker BOL#" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1.

DEFINE VARIABLE fi_depts          AS CHARACTER FORMAT "X(100)" 
    VIEW-AS FILL-IN 
    SIZE 41 BY 1.

DEFINE VARIABLE lbl_sort          AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
    VIEW-AS FILL-IN 
    SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page    AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name      AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no        AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-scr-num-copies AS INTEGER   FORMAT ">>9":U INITIAL 1 
    LABEL "# of Copies" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt           AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest           AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To File", 3,
    "To Fax", 4,
    "To Email", 5,
    "To Port Directly", 6
    SIZE 20 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_sort           AS CHARACTER INITIAL "BOL" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer", "Customer",
    "BOL", "BOL"
    SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE rs_no_PN          AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Item #", 1,
    "Customer PN", 2
    SIZE 32.8 BY .86 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 8.33.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 13.1.

DEFINE VARIABLE tb_attachBOL      AS LOGICAL INITIAL NO 
    LABEL "Attach Signed BOL" 
    VIEW-AS TOGGLE-BOX
    SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE tb_BatchMail      AS LOGICAL INITIAL NO 
    LABEL "&Batch E-Mail" 
    VIEW-AS TOGGLE-BOX
    SIZE 19.4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_collate        AS LOGICAL INITIAL NO 
    LABEL "Collate?" 
    VIEW-AS TOGGLE-BOX
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-copy      AS LOGICAL INITIAL NO 
    LABEL "Customer Copy?" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tb_email-orig     AS LOGICAL INITIAL NO 
    LABEL "Email as Original?" 
    VIEW-AS TOGGLE-BOX
    SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE tb_HideDialog     AS LOGICAL INITIAL NO 
    LABEL "&Hide Dialog-Box" 
    VIEW-AS TOGGLE-BOX
    SIZE 19.4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_office-copy    AS LOGICAL INITIAL NO 
    LABEL "Office Copy?" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tb_override-email AS LOGICAL INITIAL YES 
    LABEL "Ignore Paperless Setting?" 
    VIEW-AS TOGGLE-BOX
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_posted         AS LOGICAL INITIAL NO 
    LABEL "Reprint Posted Invoices?" 
    VIEW-AS TOGGLE-BOX
    SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_print-dept     AS LOGICAL INITIAL NO 
    LABEL "Print Dept Notes?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-inst       AS LOGICAL INITIAL YES 
    LABEL "Print Instructions?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-zero-qty   AS LOGICAL INITIAL YES 
    LABEL "Print if Inv/Ship Qty = 0?" 
    VIEW-AS TOGGLE-BOX
    SIZE 28.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_reprint        AS LOGICAL INITIAL NO 
    LABEL "Reprint Invoices?" 
    VIEW-AS TOGGLE-BOX
    SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE tb_setcomp        AS LOGICAL INITIAL NO 
    LABEL "Print Set Component?" 
    VIEW-AS TOGGLE-BOX
    SIZE 25.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_sman-copy      AS LOGICAL INITIAL NO 
    LABEL "SalesRep Copy?" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm      AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_cust AT ROW 2.43 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust AT ROW 2.43 COL 69 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_inv AT ROW 3.38 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Invoice Number"
    end_inv AT ROW 3.38 COL 69 COLON-ALIGNED HELP
    "Enter Ending Invoice Number"
    begin_date AT ROW 4.33 COL 26 COLON-ALIGNED HELP
    "Enter Beginning BOL Date"
    end_date AT ROW 4.33 COL 69 COLON-ALIGNED HELP
    "Enter Ending BOL Date"
    begin_bol AT ROW 5.29 COL 26 COLON-ALIGNED HELP
    "Enter Beginning BOL Number"
    end_bol AT ROW 5.29 COL 69 COLON-ALIGNED HELP
    "Enter Ending BOL Number"
    tb_reprint AT ROW 6.71 COL 28.2
    lv-scr-num-copies AT ROW 6.71 COL 68 COLON-ALIGNED
    tb_posted AT ROW 7.67 COL 28.2
    tb_collate AT ROW 7.67 COL 57.6
    tb_prt-inst AT ROW 8.62 COL 49 RIGHT-ALIGNED
    tb_setcomp AT ROW 8.62 COL 57.4 WIDGET-ID 2
    tb_print-dept AT ROW 9.48 COL 49 RIGHT-ALIGNED
    fi_depts AT ROW 9.48 COL 48.4 COLON-ALIGNED HELP
    "Enter Departments separated by commas" NO-LABEL
    tb_prt-zero-qty AT ROW 10.48 COL 56 RIGHT-ALIGNED WIDGET-ID 12
    lbl_sort AT ROW 11.52 COL 26.2 COLON-ALIGNED NO-LABEL
    rd_sort AT ROW 11.52 COL 39 NO-LABEL
    fi_broker-bol AT ROW 11.52 COL 75 COLON-ALIGNED HELP
    "Enter Beginning Invoice Number" WIDGET-ID 6
    rs_no_PN AT ROW 12.52 COL 28.2 NO-LABEL WIDGET-ID 8
    tb_cust-copy AT ROW 13.38 COL 12
    tb_office-copy AT ROW 13.38 COL 39
    tb_sman-copy AT ROW 13.38 COL 64
    tb_BatchMail AT ROW 15.05 COL 48.8 RIGHT-ALIGNED
    tb_HideDialog AT ROW 15.05 COL 48
    tb_attachBOL AT ROW 15.05 COL 69 WIDGET-ID 16
    rd-dest AT ROW 15.52 COL 5 NO-LABEL
    lv-ornt AT ROW 16.24 COL 30 NO-LABEL
    lines-per-page AT ROW 16.24 COL 83 COLON-ALIGNED
    lv-font-no AT ROW 17.48 COL 33 COLON-ALIGNED
    lv-font-name AT ROW 18.43 COL 27 COLON-ALIGNED NO-LABEL
    tb_email-orig AT ROW 19.76 COL 30.2 WIDGET-ID 14
    tb_override-email AT ROW 19.81 COL 60 WIDGET-ID 18
    td-show-parm AT ROW 21.14 COL 30
    btn-ok AT ROW 22.95 COL 23
    btn-cancel AT ROW 22.95 COL 56
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.43 COL 4
    BGCOLOR 2 
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 14.57 COL 4
    RECT-6 AT ROW 14.38 COL 1.4
    RECT-7 AT ROW 1.24 COL 1.4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.6 ROW 1.24
    SIZE 95.2 BY 23.76.


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
        TITLE              = "Invoicing"
        HEIGHT             = 24
        WIDTH              = 95.8
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
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".


ASSIGN
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".


ASSIGN 
    begin_bol:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_inv:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_bol:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_inv:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN fi_broker-bol IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    fi_broker-bol:HIDDEN IN FRAME FRAME-A       = TRUE
    fi_broker-bol:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN fi_depts IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    fi_depts:HIDDEN IN FRAME FRAME-A       = TRUE
    fi_depts:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-scr-num-copies IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-scr-num-copies:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR RADIO-SET rs_no_PN IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    rs_no_PN:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_BatchMail IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_BatchMail:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_collate IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_collate:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_collate:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_cust-copy IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_cust-copy:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    tb_email-orig:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_HideDialog:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_office-copy IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_office-copy:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    tb_override-email:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_posted:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_print-dept IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_print-dept:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_print-dept:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-inst IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_prt-inst:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-zero-qty IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_prt-zero-qty:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_prt-zero-qty:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_reprint:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_sman-copy IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_sman-copy:HIDDEN IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Invoicing */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Invoicing */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol C-Win
ON LEAVE OF begin_bol IN FRAME FRAME-A /* Beginning BOL# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning BOL Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv C-Win
ON LEAVE OF begin_inv IN FRAME FRAME-A /* Beginning Invoice# */
    DO:
        ASSIGN {&self-name}.
        IF tb_posted THEN END_inv:SCREEN-VALUE = SELF:SCREEN-VALUE.

        RUN set-broker-bol-proc.
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
        DEF    VAR      lv-fax-type    AS cha     NO-UNDO.
        DEF    VAR      vlSkipRec      AS LOG     NO-UNDO.
        DEF    VAR      ll-secure      AS LOG     INIT YES NO-UNDO.
        DEFINE VARIABLE lCheckHoldStat AS LOGICAL NO-UNDO .
        DEFINE BUFFER bf-cust FOR cust.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}
                tb_collate lv-scr-num-copies
                tb_cust-copy tb_office-copy tb_sman-copy
                /* gdm - 12080817 */ tb_setcomp.

            IF fi_broker-bol:SENSITIVE THEN
                ASSIGN fi_broker-bol.
        END.

        IF rs_no_PN:HIDDEN = NO THEN
            ASSIGN rs_no_PN
                svi-print-item = rs_no_PN.

        IF tb_prt-zero-qty:HIDDEN = NO THEN
            ASSIGN tb_prt-zero-qty
                s-print-zero-qty = tb_prt-zero-qty.
        /*   Ticket - 18922  */
        /*IF tb_posted AND begin_inv <> END_inv THEN DO:
           MESSAGE "Beginning Invoice# and Ending Invoice# must be the same for Reprint Posted Invoice."
               VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO begin_inv.
           RETURN .
        END.*/

        IF fi_broker-bol:SENSITIVE AND
            fi_broker-bol:SCREEN-VALUE NE "" AND
            begin_inv NE end_inv THEN
        DO:
            MESSAGE "For Broker BOL# to be used, Beginning and Ending Invoice# must be the same."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            LEAVE.
        END.

        /* gdm - 12080817 */
        ASSIGN
            tb_setcomp      = LOGICAL(tb_setcomp:SCREEN-VALUE)
            lv-multi-faxout = IF rd-dest = 4 AND begin_cust <> END_cust THEN YES 
                       ELSE NO.

        IF is-xprint-form AND rd-dest = 4 THEN lv-multi-faxout = YES.

        lv-fax-type = IF lv-multi-faxout THEN "MULTI" ELSE "CUSTOMER".

        IF lv-multi-faxout AND rd_sort <> "Customer" THEN 
        DO:
            MESSAGE "Invoice must be sorted by Customer for Fax ." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO rd_sort.
            RETURN.
        END.

        IF v-invpass THEN
        DO:
            RUN sys/ref/d-passwd.w(6, OUTPUT ll-secure).
            IF NOT ll-secure THEN LEAVE.
        END.

        IF begin_bol EQ end_bol THEN 
        DO:

            FOR EACH buf-{&head} WHERE
                buf-{&head}.company EQ cocode AND
                buf-{&head}.cust-no GE begin_cust AND
                buf-{&head}.cust-no LE end_cust AND
                      INDEX(vcHoldStats, buf-{&head}.stat) EQ 0 AND
                ((NOT tb_reprint AND buf-{&head}.inv-no EQ 0) OR
                (tb_reprint AND buf-{&head}.inv-no NE 0 AND
                buf-{&head}.inv-no GE begin_inv AND
                buf-{&head}.inv-no LE end_inv)) AND
                buf-{&head}.{&bolno} EQ begin_bol
                NO-LOCK:
                ASSIGN 
                    lCheckHoldStat = YES .
                LEAVE .
            END.
            IF NOT lCheckHoldStat THEN
                FOR EACH buf-{&head} WHERE
                    buf-{&head}.company EQ cocode AND
                    buf-{&head}.cust-no GE begin_cust AND
                    buf-{&head}.cust-no LE end_cust AND
                      INDEX(vcHoldStats, buf-{&head}.stat) <> 0 AND
                    ((NOT tb_reprint AND buf-{&head}.inv-no EQ 0) OR
                    (tb_reprint AND buf-{&head}.inv-no NE 0 AND
                    buf-{&head}.inv-no GE begin_inv AND
                    buf-{&head}.inv-no LE end_inv)) AND
                    buf-{&head}.{&bolno} EQ begin_bol
                    NO-LOCK:
                    MESSAGE "Invoice " + STRING(buf-{&head}.inv-no) + " with Bol " + 
                        STRING(buf-{&head}.{&bolno}) + " will not print, status must be approved" 
                        VIEW-AS ALERT-BOX ERROR.
                    APPLY "entry" TO begin_inv.
                    RETURN.
                END.
        END.

        CASE rd-dest:
            WHEN 1 THEN 
                ASSIGN 
                    LvOutputSelection = "Printer".
            WHEN 2 THEN 
                ASSIGN 
                    LvOutputSelection = "Screen". 
            WHEN 3 THEN 
                ASSIGN 
                    LvOutputSelection = "File".
            WHEN 4 THEN 
                ASSIGN 
                    LvOutputSelection = "Fax".
            WHEN 5 THEN 
                ASSIGN 
                    LvOutputSelection = "Email".
            WHEN 6 THEN 
                ASSIGN 
                    LvOutputSelection = "Port".
        END CASE.

        /*#BL# - If not emailing and a customer in the range is "Paperless" then abort */
        IF rd-dest <> 5 
            /*       AND CAN-FIND(FIRST cust                    */
            /*                 WHERE cust.company EQ cocode     */
            /*                   AND cust.cust-no GE begin_cust */
            /*                   AND cust.cust-no LE end_cust   */
            /*                   AND cust.log-field[1])         */
            AND NOT tb_override-email
            THEN 
        DO:
            IF tb_posted THEN 
            DO:
                FOR EACH b-ar-inv FIELDS(company cust-no ship-id) WHERE
                    b-ar-inv.company EQ cocode AND
                    b-ar-inv.inv-no GE begin_inv AND
                    b-ar-inv.inv-no LE end_inv AND
                    b-ar-inv.cust-no GE begin_cust AND
                    b-ar-inv.cust-no LE end_cust AND
                    b-ar-inv.printed EQ tb_reprint
                    NO-LOCK
                    BREAK BY b-ar-inv.company
                    BY b-ar-inv.cust-no:
                    IF FIRST-OF(b-ar-inv.cust-no) THEN 
                    DO:      
                        FIND FIRST bf-cust NO-LOCK 
                            WHERE bf-cust.company EQ cocode
                            AND bf-cust.cust-no EQ b-ar-inv.cust-no 
                            AND bf-cust.log-field[1] NO-ERROR.
                        IF AVAIL bf-cust THEN 
                        DO:
                            MESSAGE 'Customer ' bf-cust.cust-no ' is set as "Paperless Invoice".' SKIP
                                'Please select "Output To Email" or check "Ignore Paperless Setting".'
                                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                            RETURN.
                        END.
                    END.
                END.  /* for each b-ar-inv */
            END.  /* tb_posted */
            ELSE 
            DO:
                FOR EACH buf-{&head} WHERE
                    buf-{&head}.company EQ cocode AND
                    buf-{&head}.cust-no GE begin_cust AND
                    buf-{&head}.cust-no LE end_cust AND
                  INDEX(vcHoldStats, buf-{&head}.stat) EQ 0 AND
                    ((NOT tb_reprint AND buf-{&head}.inv-no EQ 0) OR
                    (tb_reprint AND buf-{&head}.inv-no NE 0 AND
                    buf-{&head}.inv-no GE begin_inv AND
                    buf-{&head}.inv-no LE end_inv)) AND
                    buf-{&head}.{&bolno} GE begin_bol AND
                    buf-{&head}.{&bolno} LE end_bol
                    NO-LOCK
                    BREAK BY buf-{&head}.company
                    BY buf-{&head}.cust-no:
                    IF FIRST-OF(buf-{&head}.cust-no) THEN 
                    DO:
                        FIND FIRST bf-cust NO-LOCK
                            WHERE bf-cust.company EQ cocode
                            AND bf-cust.cust-no EQ buf-{&head}.cust-no 
                            AND bf-cust.log-field[1] NO-ERROR.
                        IF AVAIL bf-cust THEN 
                        DO:
                            MESSAGE 'Customer ' bf-cust.cust-no ' is set as "Paperless Invoice".' SKIP
                                'Please select "Output To Email" or check "Ignore Paperless Setting".'
                                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                            RETURN.
                        END.
                    END.
                END.  /* for each buf-{&head} */

            END.    /* else do tb_posted */
        END.


        IF NOT rd-dest:SCREEN-VALUE = '5' AND NOT rd-dest:SCREEN-VALUE = '1' THEN
        DO:

            IF tb_posted THEN
            DO:
                /* If sys-ctrl-shipto "INVPRINT" found, then do this. */
                IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "INVPRINT") THEN
                DO:
                    FOR EACH b-ar-inv FIELDS(company cust-no ship-id) WHERE
                        b-ar-inv.company EQ cocode AND
                        b-ar-inv.inv-no GE begin_inv AND
                        b-ar-inv.inv-no LE end_inv AND
                        b-ar-inv.cust-no GE begin_cust AND
                        b-ar-inv.cust-no LE end_cust AND
                        b-ar-inv.printed EQ tb_reprint
                        NO-LOCK
                        BREAK BY b-ar-inv.company
                        BY b-ar-inv.cust-no
                        BY b-ar-inv.ship-id:

                        IF FIRST-OF(b-ar-inv.ship-id) THEN
                        DO:
                            /* Find INVPRINT shipto for customer, ship location and a form name. */
                            FIND FIRST sys-ctrl-shipto WHERE
                                sys-ctrl-shipto.company = cocode AND
                                sys-ctrl-shipto.NAME = "INVPRINT" AND
                                sys-ctrl-shipto.cust-vend = YES AND
                                sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                                sys-ctrl-shipto.ship-id = b-ar-inv.ship-id AND
                                sys-ctrl-shipto.char-fld > ''
                                NO-LOCK NO-ERROR.

                            /* If not found, then find INVPRINT shipto for customer and a form name. */
                            IF NOT AVAIL sys-ctrl-shipto THEN
                                FIND FIRST sys-ctrl-shipto WHERE
                                    sys-ctrl-shipto.company = cocode AND
                                    sys-ctrl-shipto.NAME = "INVPRINT" AND
                                    sys-ctrl-shipto.cust-vend = YES AND
                                    sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                                    sys-ctrl-shipto.ship-id = '' AND /* stacey */
                                    sys-ctrl-shipto.char-fld > ''
                                    NO-LOCK NO-ERROR.

                            IF AVAIL sys-ctrl-shipto THEN
                            DO:
                                RUN SetInvPostForm(sys-ctrl-shipto.char-fld).
                                v-print-fmt = sys-ctrl-shipto.char-fld.
                            END.
                            ELSE
                            DO:
                                RUN SetInvPostForm(vcDefaultForm).
                                v-print-fmt = vcDefaultForm.
                            END.

                            RUN run-report-posted(b-ar-inv.cust-no, TRUE).
                            RUN GenerateReport(INPUT lv-fax-type,
                                INPUT b-ar-inv.cust-no,
                                INPUT b-ar-inv.cust-no,
                                INPUT lv-fax-image).
                        END.
                    END.
                END. /*sys-ctrl-ship-to*/
                ELSE
                DO:
                    RUN SetInvPostForm(vcDefaultForm).
                    v-print-fmt = vcDefaultForm.

                    RUN run-report-posted("", FALSE).
                    RUN GenerateReport(INPUT lv-fax-type,
                        INPUT begin_cust,
                        INPUT end_cust,
                        INPUT lv-fax-image).

                END.
            END.
            ELSE /* not posted*/
            DO:

                IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "INVPRINT") THEN
                DO:
                    FOR EACH buf-{&head} WHERE
                        buf-{&head}.company EQ cocode AND
                        buf-{&head}.cust-no GE begin_cust AND
                        buf-{&head}.cust-no LE end_cust AND
                  INDEX(vcHoldStats, buf-{&head}.stat) EQ 0 AND
                        ((NOT tb_reprint AND buf-{&head}.inv-no EQ 0) OR
                        (tb_reprint AND buf-{&head}.inv-no NE 0 AND
                        buf-{&head}.inv-no GE begin_inv AND
                        buf-{&head}.inv-no LE end_inv)) AND
                        buf-{&head}.{&bolno} GE begin_bol AND
                        buf-{&head}.{&bolno} LE end_bol
                        NO-LOCK,
                        FIRST b-cust WHERE
                        b-cust.company EQ cocode AND
                        b-cust.cust-no EQ buf-{&head}.cust-no AND
                        ((b-cust.inv-meth EQ ? AND buf-{&head}.{&multiinvoice}) OR
                        (b-cust.inv-meth NE ? AND NOT buf-{&head}.{&multiinvoice})) 
                        NO-LOCK
                        BREAK BY buf-{&head}.company
                        BY buf-{&head}.cust-no
                        BY buf-{&head}.{&soldno}:

                        IF FIRST-OF(buf-{&head}.{&soldno}) THEN
                        DO:
                            /* Find INVPRINT shipto for customer, ship location and a form name. */
                            FIND FIRST sys-ctrl-shipto WHERE
                                sys-ctrl-shipto.company = cocode AND
                                sys-ctrl-shipto.NAME = "INVPRINT" AND
                                sys-ctrl-shipto.cust-vend = YES AND
                                sys-ctrl-shipto.cust-vend-no = buf-{&head}.cust-no AND
                                sys-ctrl-shipto.ship-id = buf-{&head}.{&soldno} AND
                                sys-ctrl-shipto.char-fld > ''
                                NO-LOCK NO-ERROR.

                            /* If not found, then find INVPRINT shipto for customer and a form name. */
                            IF NOT AVAIL sys-ctrl-shipto THEN
                                FIND FIRST sys-ctrl-shipto WHERE
                                    sys-ctrl-shipto.company = cocode AND
                                    sys-ctrl-shipto.NAME = "INVPRINT" AND
                                    sys-ctrl-shipto.cust-vend = YES AND
                                    sys-ctrl-shipto.cust-vend-no = buf-{&head}.cust-no AND
                                    sys-ctrl-shipto.ship-id = '' AND /* stacey */
                                    sys-ctrl-shipto.char-fld > ''
                                    NO-LOCK NO-ERROR.

                            IF AVAIL sys-ctrl-shipto THEN
                            DO:
                                RUN SetInvForm(sys-ctrl-shipto.char-fld).
                                v-print-fmt = sys-ctrl-shipto.char-fld.
                            END.
                            ELSE
                            DO:
                                RUN SetInvForm(vcDefaultForm).
                                v-print-fmt = vcDefaultForm.
                            END.

                            RUN run-report(buf-{&head}.cust-no,buf-{&head}.{&soldno}, TRUE).
                            RUN GenerateReport(INPUT lv-fax-type,
                                INPUT buf-{&head}.cust-no,
                                INPUT buf-{&head}.cust-no,
                                INPUT lv-fax-image).
                        END.
                    END.
                END. /*can-find sys-ctrl*/
                ELSE
                DO:
                    RUN SetInvForm(vcDefaultForm).
                    v-print-fmt = vcDefaultForm.

                    RUN run-report("","", FALSE).
                    RUN GenerateReport(INPUT lv-fax-type,
                        INPUT begin_cust,
                        INPUT end_cust,
                        INPUT lv-fax-image).
                END.
            END.
        END. /*not rd-dest = 5*/

        IF rd-dest:SCREEN-VALUE = '5' THEN 
        DO:

            IF NOT tb_BatchMail:CHECKED THEN 
            DO:
                IF begin_cust <> end_cust THEN 
                DO:

                    /*          IF NOT tb_BatchMail:SENSITIVE THEN DO: */

                    MESSAGE 'Please check Batch E-Mail to send to multiple customers in the specified range.'
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    APPLY 'ENTRY':U TO end_cust.
                    RETURN NO-APPLY.
                /* These statements will not run due to the return no-apply           */
                /*          END. */       
                /*         FIND FIRST b1-cust NO-LOCK                                 */
                /*              WHERE b1-cust.company = cocode                        */
                /*                AND b1-cust.active  = 'X' NO-ERROR.                 */
                /*                                                                    */
                /*         IF AVAIL b1-cust THEN RUN output-to-mail (b1-cust.cust-no).*/
                /*                                                                    */
                /*         ELSE DO:                                                   */
                /*           MESSAGE 'In-House Customer not defined.'                 */
                /*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                     */
                /*           RETURN.                                                  */
                /*         END.                                                       */
                END.

                ELSE RUN BatchMail (begin_cust, begin_cust).

            END.

            ELSE RUN BatchMail (begin_cust, end_cust).
        END.

        IF rd-dest:SCREEN-VALUE = '1' THEN 
        DO:
            IF tb_posted THEN 
            DO:
                FIND FIRST b-ar-inv  WHERE
                    b-ar-inv.company EQ cocode AND
                    b-ar-inv.inv-no GE begin_inv AND
                    b-ar-inv.inv-no LE end_inv AND
                    b-ar-inv.cust-no GE begin_cust AND
                    b-ar-inv.cust-no LE end_cust AND
                    b-ar-inv.printed EQ tb_reprint
                    NO-LOCK NO-ERROR.

                FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "INVPRINT" AND
                    sys-ctrl-shipto.cust-vend = YES AND
                    sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                    sys-ctrl-shipto.ship-id = b-ar-inv.ship-id AND
                    sys-ctrl-shipto.char-fld > ''
                    NO-LOCK NO-ERROR.

                IF NOT AVAIL sys-ctrl-shipto THEN
                    FIND FIRST sys-ctrl-shipto WHERE
                        sys-ctrl-shipto.company = cocode AND
                        sys-ctrl-shipto.NAME = "INVPRINT" AND
                        sys-ctrl-shipto.cust-vend = YES AND
                        sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                        sys-ctrl-shipto.ship-id = '' AND /* stacey */
                        sys-ctrl-shipto.char-fld > ''
                        NO-LOCK NO-ERROR.

                IF AVAIL sys-ctrl-shipto THEN
                DO:
                    RUN SetInvPostForm(sys-ctrl-shipto.char-fld).
                    v-print-fmt = sys-ctrl-shipto.char-fld.
                END.
                ELSE
                DO:
                    RUN SetInvPostForm(vcDefaultForm).
                    v-print-fmt = vcDefaultForm.
                END.

                RUN run-report-posted("", FALSE).
                RUN GenerateReport(INPUT lv-fax-type,
                    INPUT begin_cust,
                    INPUT end_cust,
                    INPUT lv-fax-image).
            END.

            ELSE 
            DO:
                FIND FIRST buf-{&head} WHERE
                    buf-{&head}.company EQ cocode AND
                    buf-{&head}.cust-no GE begin_cust AND
                    buf-{&head}.cust-no LE end_cust AND
             INDEX(vcHoldStats, buf-{&head}.stat) EQ 0 AND
                    ((NOT tb_reprint AND buf-{&head}.inv-no EQ 0) OR
                    (tb_reprint AND buf-{&head}.inv-no NE 0 AND
                    buf-{&head}.inv-no GE begin_inv AND
                    buf-{&head}.inv-no LE end_inv))
                    NO-LOCK NO-ERROR.

                FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "INVPRINT" AND
                    sys-ctrl-shipto.cust-vend = YES AND
                    sys-ctrl-shipto.cust-vend-no = buf-{&head}.cust-no AND
                    sys-ctrl-shipto.ship-id = buf-{&head}.{&soldno} AND
                    sys-ctrl-shipto.char-fld > ''
                    NO-LOCK NO-ERROR.

                IF NOT AVAIL sys-ctrl-shipto THEN
                    FIND FIRST sys-ctrl-shipto WHERE
                        sys-ctrl-shipto.company = cocode AND
                        sys-ctrl-shipto.NAME = "INVPRINT" AND
                        sys-ctrl-shipto.cust-vend = YES AND
                        sys-ctrl-shipto.cust-vend-no = buf-{&head}.cust-no AND
                        sys-ctrl-shipto.ship-id = '' AND /* stacey */
                        sys-ctrl-shipto.char-fld > ''
                        NO-LOCK NO-ERROR.

                IF AVAIL sys-ctrl-shipto THEN
                DO:
                    RUN SetInvForm(sys-ctrl-shipto.char-fld).
                    v-print-fmt = sys-ctrl-shipto.char-fld.
                END.
                ELSE 
                DO:
                    RUN SetInvForm(vcDefaultForm).
                    v-print-fmt = vcDefaultForm.
                END.

                RUN run-report("","", FALSE).
                RUN GenerateReport(INPUT lv-fax-type,
                    INPUT begin_cust,
                    INPUT end_cust,
                    INPUT lv-fax-image).
            END.
        END.  /* rd-dest:Screen-value = 1*/


        IF v-ftp-done THEN MESSAGE "File Export/FTP is completed." VIEW-AS ALERT-BOX INFORMATION.
        OS-DELETE VALUE(init-dir + "\Invoice.pdf").
        RELEASE {&head} .
        RELEASE {&line} .
        RELEASE inv-misc .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_bol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bol C-Win
ON LEAVE OF end_bol IN FRAME FRAME-A /* Ending BOL# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending BOL Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv C-Win
ON LEAVE OF end_inv IN FRAME FRAME-A /* Ending Invoice# */
    DO:
        ASSIGN {&self-name}.
        IF begin_inv = END_inv THEN 
        DO:
            FIND FIRST {&head} WHERE {&head}.company = g_company
                AND {&head}.inv-no = begin_inv NO-LOCK NO-ERROR.
            IF AVAIL {&head} THEN ASSIGN begin_cust:SCREEN-VALUE = {&head}.cust-no
                    end_cust:SCREEN-VALUE   = {&head}.cust-no.

            IF AVAIL {&head} AND {&head}.{&bolno} NE 0 THEN
                FIND FIRST oe-bolh
                    WHERE oe-bolh.company EQ cocode
                    AND oe-bolh.{&bolno}  EQ {&head}.{&bolno}
                    NO-LOCK NO-ERROR.

            IF AVAIL oe-bolh THEN
                ASSIGN begin_date:SCREEN-VALUE = STRING(oe-bolh.bol-date)
                    end_date:SCREEN-VALUE   = STRING(oe-bolh.bol-date).

            RUN set-broker-bol-proc.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_depts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_depts C-Win
ON LEAVE OF fi_depts IN FRAME FRAME-A
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
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE        = ENTRY(1,char-val)
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


&Scoped-define SELF-NAME lv-scr-num-copies
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-scr-num-copies C-Win
ON LEAVE OF lv-scr-num-copies IN FRAME FRAME-A /* # of Copies */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
        RUN SetEmailBoxes.
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


&Scoped-define SELF-NAME rs_no_PN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs_no_PN C-Win
ON VALUE-CHANGED OF rs_no_PN IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_BatchMail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_BatchMail C-Win
ON VALUE-CHANGED OF tb_BatchMail IN FRAME FRAME-A /* Batch E-Mail */
    DO:
        ASSIGN {&self-name}.
    /*   tb_Override-email:SCREEN-VALUE = IF tb_BatchMail THEN "Yes" ELSE "No". */

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_collate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_collate C-Win
ON VALUE-CHANGED OF tb_collate IN FRAME FRAME-A /* Collate? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_email-orig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_email-orig C-Win
ON VALUE-CHANGED OF tb_email-orig IN FRAME FRAME-A /* Email as Original? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_HideDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_HideDialog C-Win
ON VALUE-CHANGED OF tb_HideDialog IN FRAME FRAME-A /* Hide Dialog-Box */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_override-email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_override-email C-Win
ON VALUE-CHANGED OF tb_override-email IN FRAME FRAME-A /* Ignore Paperless Setting? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_posted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_posted C-Win
ON VALUE-CHANGED OF tb_posted IN FRAME FRAME-A /* Reprint Posted Invoices? */
    DO:
        ASSIGN {&self-name}.
        IF tb_posted THEN ASSIGN tb_reprint              = YES
                tb_reprint:SCREEN-VALUE = "YES".
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-dept C-Win
ON VALUE-CHANGED OF tb_print-dept IN FRAME FRAME-A /* Print Dept Notes? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-inst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-inst C-Win
ON VALUE-CHANGED OF tb_prt-inst IN FRAME FRAME-A /* Print Instructions? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-zero-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-zero-qty C-Win
ON VALUE-CHANGED OF tb_prt-zero-qty IN FRAME FRAME-A /* Print if Inv/Ship Qty = 0? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint C-Win
ON VALUE-CHANGED OF tb_reprint IN FRAME FRAME-A /* Reprint Invoices? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_setcomp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_setcomp C-Win
ON VALUE-CHANGED OF tb_setcomp IN FRAME FRAME-A /* Print Set Component? */
    DO:
        ASSIGN tb_setcomp.
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
    /*Line Spacing for GUI forms divide by 7 multiply by 6 */
    /* security check need {methods/prgsecur.i} in definition section */
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "INVPRINT"
        NO-LOCK NO-ERROR.
    IF NOT AVAIL sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.name    = "INVPRINT"
            sys-ctrl.descrip = "Print Invoice Headers on Invoice Form?".
        MESSAGE sys-ctrl.descrip
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE sys-ctrl.log-fld.
    END.
    ASSIGN
        v-print-head  = sys-ctrl.log-fld
        v-print-fmt   = sys-ctrl.char-fld
        vcDefaultForm = v-print-fmt.

    IF sys-ctrl.date-fld NE ? THEN end_date = sys-ctrl.date-fld.

    glPaperless  = GetPaperlessLogical().

    FIND FIRST users WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-LOCK NO-ERROR.

    IF AVAIL users AND users.user_program[2] NE "" THEN
        init-dir = users.user_program[2].
    ELSE
        init-dir = "c:\tmp".

    ASSIGN
        begin_date = TODAY
        end_date   = TODAY.

    IF LOOKUP(v-print-fmt,"Boxtech,Imperial") GT 0 THEN lv-prt-bypass = YES.

    IF v-print-fmt EQ "XPRINT" OR v-print-fmt EQ "Boss" OR v-print-fmt EQ "Simkins" OR v-print-fmt EQ "CapCityIn" THEN
        ASSIGN tb_print-dept:HIDDEN    = NO
            tb_print-dept:SENSITIVE = YES
            fi_depts:HIDDEN         = NO
            fi_depts:SENSITIVE      = YES.

    IF (v-print-fmt EQ "Packrite" OR  
        v-print-fmt EQ "Hughes" OR
        v-print-fmt EQ "NStock") THEN
        ASSIGN tb_setcomp:HIDDEN = NO.
    /* 20130718 JAD Task 02071304 Add Capitol to v-print-fmt */      
    IF LOOKUP(v-print-fmt,"Capitol,APC,ALLWEST,Bell,Loylang,PrestigeLLB,RFCX,Soule,SouleMed,SoulePO,LoylangJIT,LoylangBSF,Printers,Protagon,Protagon2") GT 0 THEN
        ASSIGN
            fi_broker-bol:SENSITIVE = YES
            fi_broker-bol:HIDDEN    = NO.

    IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "Coburn" OR v-print-fmt EQ "PremierS" OR v-print-fmt EQ "Axis" THEN
        ASSIGN
            tb_prt-zero-qty:SENSITIVE = YES
            tb_prt-zero-qty:HIDDEN    = NO.

    RUN enable_UI.

    {methods/nowait.i}

    IF LOOKUP(v-print-fmt,"Peachtreefgl3,SouleMed,SoulePO,Peachtree") GT 0 THEN
        ASSIGN rs_no_PN:HIDDEN    = FALSE
            rs_no_PN:SENSITIVE = TRUE.

    DO WITH FRAME {&FRAME-NAME}:

        IF LOOKUP(v-print-fmt,"PremierX,Coburn,Axis,BlueRx,ColoniaX,ABC,Knight,Knight1,Central,ACPI,ColorX,ColonialLot#,Carded,CCCFGLot,CCCFGL3,Peachtreefgl3,Peachtree,PremierS") > 0 THEN
            ASSIGN
                tb_cust-copy:HIDDEN      = NO
                tb_cust-copy:SENSITIVE   = YES
                tb_office-copy:HIDDEN    = NO
                tb_office-copy:SENSITIVE = YES
                tb_sman-copy:HIDDEN      = NO
                tb_sman-copy:SENSITIVE   = YES.

        IF v-print-fmt EQ "Fibrex" THEN
            ASSIGN
                tb_collate:HIDDEN           = NO
                tb_collate:SENSITIVE        = YES
                lv-scr-num-copies:HIDDEN    = NO
                lv-scr-num-copies:SENSITIVE = YES.
        IF v-print-fmt EQ "Protagon" OR v-print-fmt EQ "Protagon2"  THEN
            ASSIGN
                tb_setcomp:HIDDEN    = YES
                tb_setcomp:SENSITIVE = NO
                tb_setcomp           = NO.
        DISABLE lines-per-page.

        {custom/usrprint.i}

lines-per-page:SCREEN-VALUE = STRING(lines-per-page).
IF glPaperless THEN 
    tb_override-email:CHECKED = FALSE.

IF INT(begin_bol:SCREEN-VALUE) NE 0                         AND
    INT(begin_bol:SCREEN-VALUE) EQ INT(end_bol:SCREEN-VALUE) THEN 
DO:

    /* Multi Invoice Customer must print all of their Invoices */
    FOR FIRST oe-bolh NO-LOCK
        WHERE oe-bolh.company EQ cocode
        AND oe-bolh.{&bolno}  EQ INT(begin_bol:SCREEN-VALUE):

        ASSIGN
            begin_cust:SCREEN-VALUE = oe-bolh.cust-no
            end_cust:SCREEN-VALUE   = oe-bolh.cust-no.

        FOR EACH {&head} NO-LOCK
            WHERE {&head}.company EQ oe-bolh.company
            AND {&head}.cust-no EQ oe-bolh.cust-no
            AND {&head}.inv-no GE INT(begin_inv:SCREEN-VALUE)
            AND {&head}.inv-no LE INT(end_inv:SCREEN-VALUE)
            AND {&head}.{&multiinvoice}              
            AND INDEX(vcHoldStats, {&head}.stat) EQ 0:

            ASSIGN
                tb_reprint:SCREEN-VALUE = STRING({&head}.printed)
                tb_posted:SCREEN-VALUE  = STRING({&head}.posted).

            FOR EACH b-{&head}1 NO-LOCK
                WHERE b-{&head}1.company EQ {&head}.company
                AND b-{&head}1.cust-no EQ {&head}.cust-no
                AND b-{&head}1.inv-no  EQ {&head}.inv-no
                AND b-{&head}1.{&multiinvoice} EQ NO                
                AND INDEX(vcHoldStats, b-{&head}1.stat) EQ 0:

                IF b-{&head}1.{&bolno} LT INT(begin_bol:SCREEN-VALUE) THEN
                    begin_bol:SCREEN-VALUE = STRING(b-{&head}1.{&bolno}).

                IF b-{&head}1.{&bolno} GT INT(end_bol:SCREEN-VALUE) THEN
                    end_bol:SCREEN-VALUE = STRING(b-{&head}1.{&bolno}).
            END.
            IF int(begin_bol:SCREEN-VALUE) EQ 0 THEN begin_bol:SCREEN-VALUE = "0".
            IF int(end_bol:SCREEN-VALUE) EQ 0 THEN end_bol:SCREEN-VALUE = "99999999".
        END.
    END.

    FOR EACH oe-bolh NO-LOCK
        WHERE oe-bolh.company EQ cocode
        AND oe-bolh.{&bolno}  GE INT(begin_bol:SCREEN-VALUE)
        AND oe-bolh.{&bolno}  LE INT(end_bol:SCREEN-VALUE):

        IF oe-bolh.bol-date LT DATE(begin_date:SCREEN-VALUE) THEN
            begin_date:SCREEN-VALUE = STRING(oe-bolh.bol-date).

        IF oe-bolh.bol-date GT DATE(end_date:SCREEN-VALUE) THEN
            end_date:SCREEN-VALUE = STRING(oe-bolh.bol-date).
    END.
END.

/* 05301305 - IF from-to invoice is completed, then the from-to bol# is */
/* irrelevant and was causing problems                                  */
IF INT(begin_inv:SCREEN-VALUE) GT 0 AND INT(end_inv:SCREEN-VALUE) GT 0 THEN
    ASSIGN begin_bol:SCREEN-VALUE = "" end_bol:SCREEN-VALUE   = "99999999".
RUN SetEmailBoxes.

fi_broker-bol:SCREEN-VALUE = "".

RUN set-broker-bol-proc.

APPLY "entry" TO begin_cust.

ASSIGN 
    tb_email-orig:SCREEN-VALUE = "NO"
    tb_email-orig              = NO.
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

    DEFINE BUFFER b1-{&head}2 FOR {&head}.
    DEFINE BUFFER b2-cust     FOR cust.
    DEFINE BUFFER b1-ar-inv   FOR ar-inv.
    DEF VAR lEmailed AS LOG NO-UNDO.

    ASSIGN                   
        finv         = begin_inv
        tinv         = end_inv
        fdate        = begin_date
        tdate        = end_date
        fbol         = begin_bol
        tbol         = end_bol
        v-reprint    = tb_reprint
        v-sort       = rd_sort BEGINS "Customer"
        v-prntinst   = tb_prt-inst
        v-print-dept = tb_print-dept.

    IF fi_depts:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
        ASSIGN
            v-print-dept = LOGICAL(tb_print-dept:SCREEN-VALUE)
            v-depts      = fi_depts:SCREEN-VALUE.

    IF tb_posted  THEN 
    DO:

        FOR EACH  b1-ar-inv
            WHERE  b1-ar-inv.company              EQ cocode
            AND  b1-ar-inv.inv-no               GE finv
            AND  b1-ar-inv.inv-no               LE tinv  
            AND  b1-ar-inv.cust-no              GE icBegCustNo
            AND  b1-ar-inv.cust-no              LE icEndCustNo
            /*AND (b1-ar-inv.posted               EQ NO OR
                 b1-ar-inv.posted               EQ v-posted)
            AND  b1-ar-inv.printed              EQ v-print                                           */
            AND  b1-ar-inv.printed  = tb_reprint
            AND CAN-FIND(FIRST ar-invl WHERE ar-invl.x-no = b1-ar-inv.x-no /*AND ar-invl.amt <> 0*/ )
            USE-INDEX inv-no NO-LOCK,
            FIRST b2-cust OF b1-ar-inv WHERE (b2-cust.log-field[1] OR NOT tb_Batchmail OR tb_override-email) NO-LOCK                    
            BREAK BY b1-ar-inv.cust-no:

            IF FIRST-OF (b1-ar-inv.cust-no) THEN 
            DO:

                ASSIGN  
                    vlSkipRec   = YES
                    vcBegCustNo = b1-ar-inv.cust-no
                    vcEndCustNo = b1-ar-inv.cust-no.
                FIND FIRST ar-invl WHERE ar-invl.x-no = b1-ar-inv.x-no
                    NO-LOCK NO-ERROR.
                ASSIGN 
                    vSoldToNo = ""
                    vShipToNo = "". 
                IF AVAIL ar-invl THEN 
                DO:
                    FIND oe-ord WHERE oe-ord.company = b1-ar-inv.company
                        AND oe-ord.ord-no = ar-invl.ord-no
                        NO-LOCK NO-ERROR.
                    vSoldToNo = IF AVAIL oe-ord THEN oe-ord.sold-id ELSE "". 
                    vShipToNo = b1-ar-inv.ship-id.
                END.
                RUN output-to-mail (b1-ar-inv.cust-no).
            END.
            lEmailed = YES.
        END.
        /* =-=== end of posted invoices =====*/

        FOR EACH b1-{&head}2
            WHERE b1-{&head}2.company         EQ cocode
            AND b1-{&head}2.cust-no         GE icBegCustNo
            AND b1-{&head}2.cust-no         LE icEndCustNo         
            AND INDEX(vcHoldStats, b1-{&head}2.stat) EQ 0
            AND (((NOT v-reprint) AND b1-{&head}2.inv-no EQ 0) OR
            (v-reprint AND b1-{&head}2.inv-no NE 0 AND
            b1-{&head}2.inv-no        GE finv AND
            b1-{&head}2.inv-no        LE tinv))
            NO-LOCK,

            FIRST b2-cust
            WHERE b2-cust.company EQ cocode
            AND b2-cust.cust-no EQ b1-{&head}2.cust-no
            AND (b2-cust.log-field[1] OR NOT tb_Batchmail OR tb_override-email)
            AND ((b2-cust.inv-meth EQ ? AND b1-{&head}2.{&multiinvoice}) OR
            (b2-cust.inv-meth NE ? AND NOT b1-{&head}2.{&multiinvoice}))

            /*AND CAN-FIND(FIRST {&line} OF b1-{&head}2)*/
            NO-LOCK
            BREAK BY b2-cust.cust-no:

            IF FIRST-OF (b2-cust.cust-no) THEN 
            DO:

                ASSIGN  
                    vlSkipRec   = YES
                    vcBegCustNo = b1-{&head}2.cust-no
                    vcEndCustNo = b1-{&head}2.cust-no.
                vSoldToNo = "".
                IF b1-{&head}2.{&multiinvoice} THEN 
                DO: 
                    FIND FIRST b2-{&head} 
                        WHERE b2-{&head}.company       EQ b1-{&head}2.company
                        AND b2-{&head}.cust-no       EQ b1-{&head}2.cust-no
                        AND b2-{&head}.inv-no        EQ b1-{&head}2.inv-no
                        AND b2-{&head}.{&multiinvoice} EQ NO            
                        AND INDEX(vcHoldStats, b2-{&head}.stat) EQ 0 NO-LOCK NO-ERROR.
                    IF AVAIL b2-{&head} THEN
                        FIND FIRST {&line} /* wfk OF b2-{&head} */ NO-LOCK WHERE {&line}.ord-no NE 0 NO-ERROR.  
                END.  
                ELSE FIND FIRST {&line} /* OF b1-{&head}2 */ NO-LOCK WHERE {&line}.ord-no NE 0 NO-ERROR.
                IF AVAIL {&line} THEN 
                DO:
                    FIND oe-ord WHERE oe-ord.company = b1-{&head}2.company
                        AND oe-ord.ord-no = {&line}.ord-no
                        NO-LOCK NO-ERROR.
                    vSoldToNo = IF AVAIL oe-ord THEN oe-ord.sold-id ELSE "". 
                    vShipToNo = b1-{&head}2.{&soldno}.
                END.

                RUN output-to-mail (b1-{&head}2.cust-no).
            END.
            lEmailed = YES.
        END.

    END.

    ELSE 
    DO:  /* not tb_post */

        FOR EACH b1-{&head}2
            WHERE b1-{&head}2.company         EQ cocode
            AND b1-{&head}2.cust-no         GE icBegCustNo
            AND b1-{&head}2.cust-no         LE icEndCustNo         
            AND INDEX(vcHoldStats, b1-{&head}2.stat) EQ 0
            AND (((NOT v-reprint) AND b1-{&head}2.inv-no EQ 0) OR
            (v-reprint AND b1-{&head}2.inv-no NE 0 AND
            b1-{&head}2.inv-no        GE finv AND
            b1-{&head}2.inv-no        LE tinv))
            NO-LOCK,

            FIRST b2-cust
            WHERE b2-cust.company EQ cocode
            AND b2-cust.cust-no EQ b1-{&head}2.cust-no
            AND (b2-cust.log-field[1] OR NOT tb_Batchmail OR tb_override-email)
            AND ((b2-cust.inv-meth EQ ? AND b1-{&head}2.{&multiinvoice}) OR
            (b2-cust.inv-meth NE ? AND NOT b1-{&head}2.{&multiinvoice}))
            /*AND CAN-FIND(FIRST {&line} OF b1-{&head}2)*/
            NO-LOCK
            BREAK BY b2-cust.cust-no :

            IF FIRST-OF (b2-cust.cust-no) THEN 
            DO:

                ASSIGN  
                    vlSkipRec   = YES
                    vcBegCustNo = b1-{&head}2.cust-no
                    vcEndCustNo = b1-{&head}2.cust-no.
                vSoldToNo = "".
                IF b1-{&head}2.{&multiinvoice} THEN 
                DO: 
                    FIND FIRST b2-{&head} 
                        WHERE b2-{&head}.company       EQ b1-{&head}2.company
                        AND b2-{&head}.cust-no       EQ b1-{&head}2.cust-no
                        AND b2-{&head}.inv-no        EQ b1-{&head}2.inv-no
                        AND b2-{&head}.{&multiinvoice} EQ NO            
                        AND INDEX(vcHoldStats, b2-{&head}.stat) EQ 0 NO-LOCK NO-ERROR.
                    IF AVAIL b2-{&head} THEN
                        FIND FIRST {&line} /*OF b2-{&head} */ NO-LOCK WHERE {&line}.ord-no NE 0 NO-ERROR.  
                END.  
                ELSE FIND FIRST {&line} /* OF b1-{&head}2 */ NO-LOCK WHERE {&line}.ord-no NE 0 NO-ERROR.
                IF AVAIL {&line} THEN 
                DO:
                    FIND oe-ord WHERE oe-ord.company = b1-{&head}2.company
                        AND oe-ord.ord-no = {&line}.ord-no
                        NO-LOCK NO-ERROR.
                    vSoldToNo = IF AVAIL oe-ord THEN oe-ord.sold-id ELSE "". 
                    vShipToNo = b1-{&head}2.{&soldno}.
                END.
                /*           MESSAGE "3 batchMail:  " AVAIL {&line} "   soldTo:"  vSoldToNo "," {&line}.ord-no      */
                /*               "," AVAIL oe-ord "," oe-ord.sold-no RECID(oe-ord) SKIP                               */
                /*               "cust: " b2-cust.cust-no  "  inv#: " b1-{&head}2.inv-no  " r-no:  " b1-{&head}2.{&rno} */
                /*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                   */

                RUN output-to-mail (b1-{&head}2.cust-no).
            END.
            lEmailed = YES.
        END.
    END.

    IF NOT lEmailed AND NOT tb_override-email THEN 
    DO:
        FIND FIRST b2-cust WHERE b2-cust.company EQ cocode
            AND b2-cust.cust-no GE begin_cust
            AND b2-cust.cust-no LE end_cust
            AND b2-cust.log-field[1] = NO
            NO-LOCK NO-ERROR.
        IF AVAIL b2-cust THEN
            MESSAGE 'Only customers with "Paperless Invoice" checked can be emailed.  To email all customers in the range, please check "Ignore Paperless Setting" or update the customer file.'
                VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    /*      FIND b2-cust WHERE b2-cust.company = cocode                       */
    /*                     AND b2-cust.cust-no = begin_cust NO-LOCK NO-ERROR. */
    /*      IF AVAIL b2-cust AND b2-cust.log-field[1] THEN                                                                                         */
    /*         MESSAGE "Sorry, Invoice will not print,  the customer is email only because the Email Only toggle box in customer file is checked." */
    /*             VIEW-AS ALERT-BOX WARNING BUTTONS OK.                                                                                           */
    /*      ELSE                                                                                                                                   */
    /*          IF AVAIL b2-cust AND NOT b2-cust.log-field[1] THEN                                           */
    /*          MESSAGE "Customer is Mail Only because Email Only toggle box in customer file is unchecked." */
    /*              VIEW-AS ALERT-BOX WARNING BUTTONS OK.                                                    */

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

    DISABLE TRIGGERS FOR LOAD OF {&line}.
    DISABLE TRIGGERS FOR LOAD OF inv-misc.

    FOR EACH {&line} WHERE {&line}.{&rno} EQ b-{&head}1.{&rno}:
        CREATE save-line.
        ASSIGN
            save-line.reftable = "save-line" + v-term-id
            save-line.val[1]   = {&line}.{&rno}
            save-line.val[2]   = {&head}.{&rno}
            save-line.val[3]   = INT(RECID({&line}))
            {&line}.{&rno}      = {&head}.{&rno}.
    END.

    FOR EACH inv-misc WHERE inv-misc.{&miscrno} EQ b-{&head}1.{&rno}:
        CREATE save-line.
        ASSIGN
            save-line.reftable = "save-line" + v-term-id
            save-line.val[1]   = inv-misc.{&miscrno}
            save-line.val[2]   = {&head}.{&rno}
            save-line.val[3]   = INT(RECID(inv-misc))
            inv-misc.{&miscrno}      = {&head}.{&rno}.
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
    DISPLAY begin_cust end_cust begin_inv end_inv begin_date end_date begin_bol 
        end_bol tb_reprint tb_posted tb_prt-inst tb_setcomp lbl_sort rd_sort 
        tb_BatchMail tb_HideDialog tb_attachBOL rd-dest lv-ornt lines-per-page 
        lv-font-no lv-font-name tb_email-orig tb_override-email td-show-parm 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_cust end_cust begin_inv end_inv begin_date 
        end_date begin_bol end_bol tb_reprint tb_posted tb_prt-inst tb_setcomp 
        rd_sort tb_BatchMail tb_HideDialog tb_attachBOL rd-dest lv-ornt 
        lines-per-page lv-font-no tb_email-orig tb_override-email td-show-parm 
        btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateEmail C-Win 
PROCEDURE GenerateEmail :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icCustNo AS CHAR NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

        IF vlSkipRec THEN RETURN.

        IF v-print-fmt EQ "Southpak-xl" OR v-print-fmt EQ "PrystupExcel" THEN
        DO:
            OS-DELETE VALUE(init-dir + "\Invoice").
            OS-COPY VALUE(init-dir + "\Invoice.pdf") VALUE(init-dir + "\Invoice").
            ASSIGN 
                list-name = init-dir + "\Invoice".
        END.

        lv-pdf-file = lv-pdf-file + vcInvNums + '.pdf'.

        IF is-xprint-form THEN 
        DO:
            IF v-print-fmt NE "Southpak-xl" AND v-print-fmt NE "PrystupExcel" THEN
                RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

            IF cActualPDF NE lv-pdf-file AND SEARCH(cActualPDF) NE ? THEN 
            DO:
                OS-COPY VALUE(cActualPDF) VALUE(lv-pdf-file).
                OS-DELETE VALUE(cActualPDF).           
            END.
            IF tb_HideDialog:CHECKED THEN RUN SendMail-1 (icCustNo, 'Customer1').
            ELSE RUN SendMail-1 (icCustNo, 'Customer').
        END.
        ELSE
            IF tb_HideDialog:CHECKED THEN RUN SendMail-1 (icCustNo, 'Customer1').
            ELSE RUN SendMail-1 (icCustNo, 'Customer').
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
    DEFINE INPUT PARAMETER lv-fax-type AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER ip-begin-cust AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER ip-end-cust AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER lv-fax-image AS CHAR NO-UNDO.

    IF v-print-fmt <> "Southpak-xl" AND v-print-fmt <> "PrystupExcel" THEN
    DO:
        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
            WHEN 4 THEN 
                DO:
                    IF lv-fax-type = "MULTI" THEN 
                    DO:
                        RUN output-to-fax-prt. /* create tif file */              
                        {custom/asifaxm3.i &TYPE="MULTI"
                             &begin_cust=ip-begin-cust
                             &END_cust=ip-end-cust
                             &fax-subject="Invoice"
                             &fax-body="Invoice"
                             &fax-file=lv-fax-image
                             &end-widget=end_cust }      
                    END.
                    ELSE 
                    DO:
                        {custom/asifax3.i &TYPE="CUSTOMER"
                     &begin_cust=ip-begin-cust
                     &END_cust=ip-end-cust
                     &fax-subject="Invoice"
                     &fax-body="Invoice"
                     &fax-file=lv-fax-image
                     &end-widget=end_cust }      
                    END.
                END.
            WHEN 6 THEN RUN OUTPUT-to-port.
        END CASE.
    END.
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
    DEF VAR lv-xpr-file  AS cha FORM "x(60)" NO-UNDO.

    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        INPUT FROM OS-DIR ("C:\temp\fax") NO-ECHO.
        REPEAT:
            SET lv-file-name.
            IF lv-file-name <> "." AND lv-file-name <> ".."  AND lv-file-name MATCHES "*xpr*" 
                THEN 
            DO:
                lv-xpr-file = "c:\temp\fax\" + lv-file-name.             
                RUN printfile (lv-xpr-file).
            END.
            lv-file-name = "".   
        END.

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
    DEFINE INPUT PARAM icCustNo AS CHAR NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

        IF tb_posted THEN
        DO:
            IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
                sys-ctrl-shipto.company = cocode AND
                sys-ctrl-shipto.NAME = "INVPRINT") THEN
                FOR EACH b-ar-inv FIELDS(company cust-no ship-id ) WHERE
                    b-ar-inv.company EQ cocode AND
                    b-ar-inv.inv-no GE begin_inv AND
                    b-ar-inv.inv-no LE end_inv AND
                    ((b-ar-inv.cust-no GE begin_cust AND b-ar-inv.cust-no LE end_cust AND NOT tb_BatchMail)
                    OR 
                    (b-ar-inv.cust-no EQ icCustNo AND tb_BatchMail)
                    ) AND
                    b-ar-inv.printed EQ tb_reprint
                    NO-LOCK
                    BREAK BY b-ar-inv.company
                    BY b-ar-inv.cust-no:

                    IF FIRST-OF(b-ar-inv.cust-no) THEN
                    DO:
                        FIND FIRST sys-ctrl-shipto WHERE
                            sys-ctrl-shipto.company = cocode AND
                            sys-ctrl-shipto.NAME = "INVPRINT" AND
                            sys-ctrl-shipto.cust-vend = YES AND
                            sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                            sys-ctrl-shipto.ship-id = b-ar-inv.ship-id AND
                            sys-ctrl-shipto.char-fld > ''
                            NO-LOCK NO-ERROR.

                        IF NOT AVAIL sys-ctrl-shipto THEN
                            FIND FIRST sys-ctrl-shipto WHERE
                                sys-ctrl-shipto.company = cocode AND
                                sys-ctrl-shipto.NAME = "INVPRINT" AND
                                sys-ctrl-shipto.cust-vend = YES AND
                                sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                                sys-ctrl-shipto.char-fld > ''
                                NO-LOCK NO-ERROR.

                        IF AVAIL sys-ctrl-shipto THEN
                        DO:
                            RUN SetInvPostForm(sys-ctrl-shipto.char-fld).
                            v-print-fmt = sys-ctrl-shipto.char-fld.
                        END.
                        ELSE
                        DO:
                            RUN SetInvPostForm(vcDefaultForm).
                            v-print-fmt = vcDefaultForm.
                        END.

                        ASSIGN
                            vcInvNums   = ""
                            lv-pdf-file = init-dir + "\Inv".

                        RUN run-report-posted(b-ar-inv.cust-no, TRUE).
                        FIND FIRST ar-invl WHERE ar-invl.x-no = b-ar-inv.x-no
                            NO-LOCK NO-ERROR.
                        vSoldToNo = "". 
                        IF AVAIL ar-invl THEN 
                        DO:
                            FIND oe-ord WHERE oe-ord.company = b-ar-inv.company
                                AND oe-ord.ord-no = ar-invl.ord-no
                                NO-LOCK NO-ERROR.
                            vSoldToNo = IF AVAIL oe-ord THEN oe-ord.sold-id ELSE "". 
                        END.
                        vShipToNo = b-ar-inv.ship-id.
                        RUN GenerateEmail(b-ar-inv.cust-no).
                    END.
                END.
            ELSE
            DO:
                RUN SetInvPostForm(vcDefaultForm).

                ASSIGN
                    vcInvNums   = ""
                    lv-pdf-file = init-dir + "\Inv"
                    v-print-fmt = vcDefaultForm.

                RUN run-report-posted("", FALSE).
                RUN GenerateEmail(icCustNo).
            END.
        END.
        ELSE /*not posted*/
        DO:         
            IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
                sys-ctrl-shipto.company = cocode AND
                sys-ctrl-shipto.NAME = "INVPRINT") THEN
                FOR EACH buf-{&head} FIELDS(company cust-no sold-no) WHERE
                    buf-{&head}.company EQ cocode AND
                    ((buf-{&head}.cust-no GE begin_cust AND buf-{&head}.cust-no LE end_cust AND NOT tb_BatchMail)
                    OR
                    (buf-{&head}.cust-no EQ icCustNo AND tb_BatchMail)
                    ) AND
               INDEX(vcHoldStats, buf-{&head}.stat) EQ 0 AND 
                    (((NOT tb_reprint) AND buf-{&head}.inv-no EQ 0) OR
                    (tb_reprint AND buf-{&head}.inv-no NE 0 AND
                    buf-{&head}.inv-no GE begin_inv AND
                    buf-{&head}.inv-no LE end_inv))
                    NO-LOCK,
                    FIRST b-cust WHERE
                    b-cust.company EQ cocode AND
                    b-cust.cust-no EQ buf-{&head}.cust-no AND
                    ((b-cust.inv-meth EQ ? AND buf-{&head}.{&multiinvoice}) OR
                    (b-cust.inv-meth NE ? AND NOT buf-{&head}.{&multiinvoice}))
                    NO-LOCK
                    BREAK BY buf-{&head}.company
                    BY buf-{&head}.cust-no:

                    IF FIRST-OF(buf-{&head}.cust-no) THEN
                    DO:
                        FIND FIRST sys-ctrl-shipto WHERE
                            sys-ctrl-shipto.company = cocode AND
                            sys-ctrl-shipto.NAME = "INVPRINT" AND
                            sys-ctrl-shipto.cust-vend = YES AND
                            sys-ctrl-shipto.cust-vend-no = buf-{&head}.cust-no AND
                            sys-ctrl-shipto.ship-id = buf-{&head}.{&soldno} AND
                            sys-ctrl-shipto.char-fld > ''
                            NO-LOCK NO-ERROR.

                        IF NOT AVAIL sys-ctrl-shipto THEN
                            FIND FIRST sys-ctrl-shipto WHERE
                                sys-ctrl-shipto.company = cocode AND
                                sys-ctrl-shipto.NAME = "INVPRINT" AND
                                sys-ctrl-shipto.cust-vend = YES AND
                                sys-ctrl-shipto.cust-vend-no = buf-{&head}.cust-no AND
                                sys-ctrl-shipto.char-fld > ''
                                NO-LOCK NO-ERROR.

                        IF AVAIL sys-ctrl-shipto THEN
                        DO:
                            RUN SetInvForm(sys-ctrl-shipto.char-fld).
                            v-print-fmt = sys-ctrl-shipto.char-fld.
                        END.
                        ELSE
                        DO:
                            RUN SetInvForm(vcDefaultForm).
                            v-print-fmt = vcDefaultForm.
                        END.

                        ASSIGN
                            vcInvNums   = ""
                            lv-pdf-file = init-dir + "\Inv".
                        RUN run-report(buf-{&head}.cust-no,"", TRUE).
                        ASSIGN 
                            vSoldToNo = ""
                            vShipToNo = "".
                        IF buf-{&head}.{&multiinvoice} THEN 
                        DO: 
                            FIND FIRST b2-{&head} 
                                WHERE b2-{&head}.company       EQ buf-{&head}.company
                                AND b2-{&head}.cust-no       EQ buf-{&head}.cust-no
                                AND b2-{&head}.inv-no        EQ buf-{&head}.inv-no
                                AND b2-{&head}.{&multiinvoice} EQ NO            
                                AND INDEX(vcHoldStats, b2-{&head}.stat) EQ 0 NO-LOCK NO-ERROR.
                            IF AVAIL b2-{&head} THEN
                                FIND FIRST {&line} /* wfk OF b2-{&head} */ NO-LOCK WHERE {&line}.ord-no NE 0 NO-ERROR.  
                        END.  
                        ELSE         
                            FIND FIRST {&line} /* wfk of buf-{&head} */ NO-LOCK NO-ERROR.
                        IF AVAIL {&line} THEN 
                        DO:
                            FIND oe-ord WHERE oe-ord.company = buf-{&head}.company
                                AND oe-ord.ord-no = {&line}.ord-no
                                NO-LOCK NO-ERROR.
                            vSoldToNo = IF AVAIL oe-ord THEN oe-ord.sold-id ELSE "". 
                        END.
                        vShipToNo = buf-{&head}.{&soldno}.
                        RUN GenerateEmail(buf-{&head}.cust-no).
                    END.
                END.

            ELSE
            DO:
                RUN SetInvForm(vcDefaultForm).

                ASSIGN
                    v-print-fmt = vcDefaultForm
                    vcInvNums   = ""
                    lv-pdf-file = init-dir + "\Inv".

                RUN run-report("","", FALSE).
                RUN GenerateEmail(icCustNo).
            END.
        END.
    END.

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
    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE IF lv-prt-bypass THEN 
        DO:
            RUN custom/d-print.w (list-name).
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
    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE
        RUN custom/scr-rpt2.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt,lv-prt-bypass).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------ oe/rep/invoice.p  9/94 RM */
    /* PRINT INVOICE - O/E MODULE                                                 */
    /* -------------------------------------------------------------------------- */
    DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER ip-sold-no AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

    DEF BUFFER bf-{&line} FOR {&line}.

    DEF VAR ll-consolidate AS LOG NO-UNDO.
    DEF VAR lv-copy#       AS INT NO-UNDO.      
    DEF VAR dtl-ctr        AS INT NO-UNDO.

    {sys/form/r-top.i}

    ASSIGN                   
        finv           = begin_inv
        tinv           = end_inv
        fdate          = begin_date
        tdate          = end_date
        fbol           = begin_bol
        tbol           = end_bol
        v-reprint      = tb_reprint
        v-sort         = rd_sort BEGINS "Customer"
        v-prntinst     = tb_prt-inst
        v-print-dept   = tb_print-dept
        ll-consolidate = rd_sort EQ "Customer2".

    /* gdm - 12080807 */
    ASSIGN 
        nsv_setcomp = tb_setcomp.

    IF ip-sys-ctrl-shipto THEN
        ASSIGN
            fcust = ip-cust-no
            tcust = ip-cust-no.
    ELSE
    DO:
        IF rd-dest:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '5' AND 
            tb_BatchMail:CHECKED THEN
            ASSIGN
                vcBegCustNo = vcBegCustNo
                vcEndCustNo = vcEndCustNo.
        ELSE
            ASSIGN
                vcBegCustNo = begin_cust
                vcEndCustNo = end_cust.

        ASSIGN
            fcust = vcBegCustNo
            tcust = vcEndCustNo.
    END.

    IF fi_depts:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
        ASSIGN
            v-print-dept = LOGICAL(tb_print-dept:SCREEN-VALUE)
            v-depts      = fi_depts:SCREEN-VALUE.

        {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    {sa/sa-sls01.i}

    v-term-id = v-term + USERID("nosweat").

    SESSION:SET-WAIT-STATE ("general").

    build-report:
    FOR EACH {&head}
        WHERE {&head}.company         EQ cocode
        AND {&head}.cust-no         GE fcust
        AND {&head}.cust-no         LE tcust 
        AND ({&head}.{&soldno}         EQ ip-sold-no OR ip-sold-no = "")
        AND INDEX(vcHoldStats, {&head}.stat) EQ 0
        AND (((NOT v-reprint) AND {&head}.inv-no EQ 0) OR
        (v-reprint AND {&head}.inv-no NE 0 AND
        {&head}.inv-no        GE finv AND
        {&head}.inv-no        LE tinv))
        /* gdm - 10150909*/
        AND (({&head}.{&multiinvoice} =  NO AND {&head}.{&bolno} GE fbol AND
        {&head}.{&bolno} LE tbol) OR {&head}.{&multiinvoice} EQ YES)
        /* gdm - 10150909 end*/
        NO-LOCK,

        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ {&head}.cust-no
        AND ((cust.inv-meth EQ ? AND {&head}.{&multiinvoice}) OR
        (cust.inv-meth NE ? AND NOT {&head}.{&multiinvoice}))
        NO-LOCK BY {&head}.{&bolno} :

        vcBOLFiles = "".    
        IF {&head}.{&multiinvoice} THEN 
        DO:
            dtl-ctr = 0.
            FOR EACH b-{&head}1 NO-LOCK
                WHERE b-{&head}1.company       EQ {&head}.company
                AND b-{&head}1.cust-no       EQ {&head}.cust-no
                AND b-{&head}1.inv-no        EQ {&head}.inv-no
                AND b-{&head}1.{&multiinvoice} EQ NO            
                AND INDEX(vcHoldStats, b-{&head}1.stat) EQ 0:

                /*wfk {oe/rep/bolcheck.i b-{&head}1 build-report} */
IF tb_attachBOL AND SEARCH(vcBOLSignDir + "\" + string(b-{&head}1.{&bolno}) + ".pdf") NE ?  THEN 
    vcBOLFiles = vcBOLFiles + "," + SEARCH(vcBOLSignDir + "\" + string(b-{&head}1.{&bolno}) + ".pdf").
RUN create-save-line.
ASSIGN 
    dtl-ctr = dtl-ctr + 1.
END.
END.

  ELSE DO:
/* {oe/rep/bolcheck.i {&head} build-report} */
IF tb_attachBOL AND SEARCH(vcBOLSignDir + "\" + string({&head}.{&bolno}) + ".pdf") NE ?  THEN 
    vcBOLFiles = vcBOLFiles + "," + SEARCH(vcBOLSignDir + "\" + string({&head}.{&bolno}) + ".pdf").

END.


/* dont include {&head} on printing when it's a MASTER invoice (multi-inv) AND 
   no SLAVE invoices found which is NOT on HOLD Status ({&head}.stat NE "H") AH 07/08/10 */
IF cust.inv-meth EQ ? AND {&head}.{&multiinvoice} AND dtl-ctr LE 0 THEN NEXT.

vlSkipRec = NO.

/* WFK - 15063 - Removed this change since it was causing lots of problems */
/*  IF  NOT v-reprint OR {&head}.inv-no EQ 0 THEN do:*/
/*        RUN oe/get-inv#.p (ROWID({&head})).        */
/*        v-reprint = YES  .                          */
/*  END.                                              */

CREATE report.
ASSIGN
    report.term-id = v-term-id
    report.key-01  = IF v-sort THEN
                      IF v-sort-name THEN cust.name
                      ELSE {&head}.cust-no
                    ELSE ""
    report.key-02  = STRING({&head}.{&bolno},"9999999999")
    report.rec-id  = RECID({&head})
    vcInvNums      = vcInvNums + '-' + STRING ({&head}.inv-no)
    vcInvNums      = LEFT-TRIM (vcInvNums, '-')  
    report.key-03  = IF v-sort THEN STRING({&head}.inv-no,"9999999999") ELSE ""  .

IF vcInvNums MATCHES '*-*' THEN
    vcInvNums = RIGHT-TRIM (SUBSTRING (vcInvNums, 1, INDEX (vcInvNums,'-')), '-') + SUBSTRING (vcInvNums, R-INDEX (vcInvNums, '-')).

/* update loadtag status - Bill of lading task#: 10190414 */
IF NOT {&head}.printed THEN
    FOR EACH bf-{&line} /* wfk  OF {&head} */ NO-LOCK:
        FOR EACH oe-boll WHERE oe-boll.company EQ bf-{&line}.company
            AND oe-boll.b-no    EQ bf-{&line}.b-no
            AND oe-boll.ord-no  EQ bf-{&line}.ord-no
            AND oe-boll.i-no    EQ bf-{&line}.i-no
            AND oe-boll.line    EQ bf-{&line}.line
            AND oe-boll.po-no   EQ bf-{&line}.po-no
            AND CAN-FIND(FIRST oe-bolh WHERE oe-bolh.b-no   EQ bf-{&line}.b-no
            AND oe-bolh.posted EQ YES) NO-LOCK:

            FIND FIRST loadtag EXCLUSIVE-LOCK WHERE loadtag.company EQ {&head}.company
                AND loadtag.item-type EQ NO
                AND loadtag.i-no EQ bf-{&line}.i-no
                AND loadtag.job-no EQ oe-boll.job-no
                AND loadtag.job-no2 EQ oe-boll.job-no2
                AND loadtag.tag-no EQ oe-boll.tag NO-ERROR.

            IF AVAIL loadtag THEN loadtag.sts = "Invoiced".
            RELEASE loadtag.
        END.
    END.
END.

FOR EACH save-line WHERE save-line.reftable EQ "save-line" + v-term-id,
    FIRST {&head}
    WHERE {&head}.{&rno} EQ INT(save-line.val[2])
    AND NOT CAN-FIND(FIRST report
    WHERE report.term-id EQ v-term-id
    AND report.rec-id  EQ RECID({&head})):
    RUN undo-save-line.
END.

v-lines-per-page = lines-per-page.

IF v-print-fmt NE "Fibrex" THEN
DO:
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ cocode AND
        sys-ctrl.name    EQ "INVCOPYS"
        NO-LOCK NO-ERROR.

    lv-copy# = IF AVAIL sys-ctrl AND sys-ctrl.int-fld <> 0 THEN sys-ctrl.int-fld ELSE 1.
END.
ELSE
    lv-copy# = lv-scr-num-copies.

/* 20130718 JAD Task 02071304 Add Capitol to v-print-fmt */
IF fi_broker-bol:SENSITIVE = YES AND
    LOOKUP(v-print-fmt,"Capitol,APC,ALLWEST,Bell,LoyLang,PrestigeLLB,RFCX,Soule,SouleMed,SoulePO,LoylangJIT,LoylangBSF,Printers,Protagon,Protagon2") GT 0 AND
    begin_inv EQ end_inv THEN
DO:
    FIND FIRST b-broker-bol WHERE
        b-broker-bol.reftable EQ "brokerbol" AND
        b-broker-bol.CODE EQ STRING(begin_inv)
        NO-ERROR.

    IF NOT AVAIL b-broker-bol AND
        fi_broker-bol:SCREEN-VALUE NE "" THEN
    DO:
        CREATE b-broker-bol.
        ASSIGN
            b-broker-bol.reftable = "brokerbol"
            b-broker-bol.CODE     = STRING(begin_inv).
    END.

    IF AVAIL b-broker-bol THEN
    DO:
        b-broker-bol.code2 = fi_broker-bol:SCREEN-VALUE.
        RELEASE b-broker-bol.
    END.
END.

IF is-xprint-form THEN 
DO:

    IF v-print-fmt EQ "Fibrex" AND
        tb_collate:HIDDEN EQ NO AND tb_collate THEN
        PUT "<COLLATE=YES,ALWAYS>".

    CASE rd-dest :

        WHEN 1 THEN 
            PUT "<COPIES=" + string(lv-copy#) + "><PRINTER?>" FORM "x(30)".
        WHEN 2 THEN 
            DO:
                IF NOT lBussFormModle THEN
                    PUT "<COPIES=" + string(lv-copy#) + "><PREVIEW><MODAL=NO>" FORM "x(30)".
                ELSE
                    PUT "<COPIES=" + string(lv-copy#) + "><PREVIEW>" FORM "x(30)".
            END.
        WHEN 5 THEN 
            DO:
                IF vcInvNums = "0" OR vcInvNums = "0-0" THEN 
                    vcInvNums = STRING(RANDOM(1, 1000)).
                IF v-print-fmt EQ "CentBox" THEN
                DO:
                    IF NOT tb_BatchMail:CHECKED THEN
                        PUT "<PREVIEW><FORMAT=LETTER><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                    ELSE 
                        PUT "<PREVIEW=PDF><FORMAT=LETTER><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                    cActualPDF = lv-pdf-file + vcInvNums  + ".pdf".
                END.
                ELSE IF v-print-fmt EQ "Southpak-XL" OR v-print-fmt EQ "PrystupExcel" THEN 
                    DO:
                        PUT "<PDF=DIRECT><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".
                        cActualPDF = list-name + ".pdf".
                    END.
                    ELSE IF v-print-fmt EQ "Protagon" OR v-print-fmt = "Protagon2" THEN 
                        DO:
                            PUT "<PDF=DIRECT><FORMAT=LETTER><PDF-LEFT=0.5mm><PDF-TOP=-0.5mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                            cActualPDF = lv-pdf-file + vcInvNums + ".pdf".
                        END.
                        ELSE IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "Coburn" OR v-print-fmt = "PremierS" OR v-print-fmt = "Axis" THEN 
                            DO:
                                PUT "<PDF=DIRECT><FORMAT=LETTER><PDF-LEFT=5mm><PDF-TOP=7mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                                cActualPDF = lv-pdf-file + vcInvNums + ".pdf".
                            END.
                            ELSE 
                            DO: 
                                PUT "<PDF=DIRECT><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                                cActualPDF = lv-pdf-file + vcInvNums + ".pdf".
                            END.  
            END.

    END CASE.

    PUT "</PROGRESS>".
END.

ASSIGN 
    lXMLOutput  = rd-dest EQ iXMLOutput /* rstark 05181205 */
    clXMLOutput = NO /* rstark 05291402 */
    .

IF LOOKUP(v-print-fmt,"SOUTHPAK,southpak-xl,PrystupExcel,ASIXprnt,Southpakl,Badger,Badger-Emailed") > 0 THEN 
DO: 
    RUN value(v-program) (lv-multi-faxout,lines-per-page). 
END.
ELSE IF v-print-fmt EQ "1/2 Page" AND rd-dest = 6 THEN 
    DO:
        PUT CONTROL CHR(27) CHR(67) CHR(44). 
        RUN value(v-program). 
        PUT CONTROL CHR(18).
    END.

    ELSE IF LOOKUP(v-print-fmt,"BlueRX,ColoniaX,ABC,Knight,Knight1,Central,Rosmar,ACPI,ColonialLot#,Carded,CCCFGLot,CCCFGL3,Peachtreefgl3,Peachtree") > 0 THEN 
        DO:
            RUN value(v-program) (""). 
            v-reprint = YES.
            IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy").
            IF tb_office-copy THEN RUN value(v-program) ("Office Copy").
            IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy").
        END.
        ELSE IF LOOKUP(v-print-fmt,"ColorX") > 0 THEN 
            DO:
                v-reprint = YES.
                IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy").
                IF tb_office-copy THEN RUN value(v-program) ("Office Copy").
                IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy").
            END.
            ELSE IF LOOKUP(v-print-fmt,"PremierX,Coburn,Axis") > 0 THEN 
                DO: 
                    RUN value(v-program) ("",NO). 
                    v-reprint = YES.
                    IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy",NO).
                    IF tb_office-copy THEN RUN value(v-program) ("Office Copy",NO).
                    IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy",NO).
                END.
                ELSE IF LOOKUP(v-print-fmt,"PremierS") > 0 THEN 
                    DO:    
                        RUN value(v-program) ("",YES). 
                        v-reprint = YES.
                        IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy",YES).
                        IF tb_office-copy THEN RUN value(v-program) ("Office Copy",YES).
                        IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy",YES).
                    END.
                    ELSE RUN value(v-program). 

vcInvNums = "".
FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,        
    FIRST {&head} WHERE RECID({&head}) EQ report.rec-id NO-LOCK
    BREAK BY {&head}.inv-no:

    ASSIGN 
        vcInvNums = vcInvNums + '-' + STRING ({&head}.inv-no)
        vcInvNums = LEFT-TRIM (vcInvNums, '-').

    /* Extract first and last inv# with '-' in between */
    IF vcInvNums MATCHES '*-*' THEN
        vcInvNums = RIGHT-TRIM (SUBSTRING (vcInvNums, 1, INDEX (vcInvNums,'-')), '-') +     
            SUBSTRING (vcInvNums, R-INDEX (vcInvNums, '-')).

END.

FOR EACH save-line WHERE save-line.reftable EQ "save-line" + v-term-id:
    RUN undo-save-line.
END.

FOR EACH report WHERE report.term-id EQ v-term-id: 
    DELETE report.
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-posted C-Win 
PROCEDURE run-report-posted :
    /* ------------------------------------------------ ar/rep/invoice.p  9/94 RM */
    /* PRINT INVOICE - A/R MODULE                                                 */
    /* -------------------------------------------------------------------------- */

    DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-ship AS LOG NO-UNDO.

    DEF VAR lv-copy# AS INT NO-UNDO.

{sys/form/r-top.i}

    ASSIGN
        finv         = begin_inv
        tinv         = end_inv
        v-print      = tb_reprint
        v-posted     = tb_posted
        v-prntinst   = tb_prt-inst
        v-print-dept = tb_print-dept.

    IF fi_depts:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
        ASSIGN
            v-print-dept = LOGICAL(tb_print-dept:SCREEN-VALUE)
            v-depts      = fi_depts:SCREEN-VALUE.

    IF ip-sys-ctrl-ship THEN
        ASSIGN
            fcust = ip-cust-no
            tcust = ip-cust-no.
    ELSE
    DO:
        IF rd-dest:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '5' AND 
            tb_BatchMail:CHECKED THEN
            ASSIGN vcBegCustNo = vcBegCustNo
                vcEndCustNo = vcEndCustNo.
        ELSE
            ASSIGN vcBegCustNo = begin_cust
                vcEndCustNo = end_cust.

        ASSIGN
            fcust = vcBegCustNo
            tcust = vcEndCustNo.
    END.

{sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

{sa/sa-sls01.i}

    v-term-id = v-term.

    SESSION:SET-WAIT-STATE ("general").

    FOR EACH ar-inv
        WHERE ar-inv.company                EQ cocode
        AND ar-inv.inv-no                 GE finv
        AND ar-inv.inv-no                 LE tinv
        AND ar-inv.cust-no                GE fcust
        AND ar-inv.cust-no                LE tcust
        AND ar-inv.printed                EQ v-print
        AND CAN-FIND(FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no)
        USE-INDEX inv-no NO-LOCK:

        IF NOT(ar-inv.posted EQ NO OR
            ar-inv.posted EQ v-posted) THEN NEXT.

        CREATE report.
        ASSIGN
            report.term-id = v-term-id
            report.key-01  = STRING(ar-inv.inv-no,"9999999999")
            report.rec-id  = RECID(ar-inv)
            vlSkipRec      = NO
            vcInvNums      = vcInvNums + '-' + STRING (ar-inv.inv-no)
            vcInvNums      = LEFT-TRIM (vcInvNums, '-')  
            report.key-03  = IF v-sort THEN STRING(ar-inv.inv-no,"9999999999") ELSE "" .

        IF vcInvNums MATCHES '*-*' THEN
            vcInvNums = RIGHT-TRIM (SUBSTRING (vcInvNums, 1, INDEX (vcInvNums,'-')), '-') + SUBSTRING (vcInvNums, R-INDEX (vcInvNums, '-')).
    END.

    v-lines-per-page = lines-per-page.

    IF v-print-fmt NE "Fibrex" THEN
    DO:
        FIND FIRST sys-ctrl WHERE
            sys-ctrl.company EQ cocode AND
            sys-ctrl.name    EQ "INVCOPYS"
            NO-LOCK NO-ERROR.

        lv-copy# = IF AVAIL sys-ctrl AND sys-ctrl.int-fld <> 0 THEN sys-ctrl.int-fld ELSE 1.
    END.
    ELSE
        lv-copy# = lv-scr-num-copies.

    /* 20130718 JAD Task 02071304 Add Capitol to v-print-fmt */
    IF fi_broker-bol:SENSITIVE = YES AND
        lookup(v-print-fmt,"Capitol,APC,ALLWEST,Bell,Loylang,PrestigeLLB,RFCX,Soule,SouleMed,SoulePO,LoylangJIT,LoylangBSF,Printers,Protagon,Protagon2") GT 0 AND
        begin_inv EQ end_inv THEN
    DO:
        FIND FIRST b-broker-bol WHERE
            b-broker-bol.reftable EQ "brokerbol" AND
            b-broker-bol.CODE EQ STRING(begin_inv)
            NO-ERROR.

        IF NOT AVAIL b-broker-bol AND
            fi_broker-bol:SCREEN-VALUE NE "" THEN
        DO:
            CREATE b-broker-bol.
            ASSIGN
                b-broker-bol.reftable = "brokerbol"
                b-broker-bol.CODE     = STRING(begin_inv).
        END.

        IF AVAIL b-broker-bol THEN
        DO:
            b-broker-bol.code2 = fi_broker-bol:SCREEN-VALUE.
            RELEASE b-broker-bol.
        END.
    END.

    IF is-xprint-form THEN 
    DO:

        IF v-print-fmt EQ "Fibrex" AND
            tb_collate:HIDDEN EQ NO AND tb_collate THEN
            PUT "<COLLATE=YES,ALWAYS>".

        CASE rd-dest :
            WHEN 1 THEN 
                PUT "<COPIES=" + string(lv-copy#) + "><PRINTER?>" FORM "x(30)".
            WHEN 2 THEN 
                DO:
                    IF NOT lBussFormModle THEN
                        PUT "<COPIES=" + string(lv-copy#) + "><PREVIEW>" FORM "x(30)".
                    ELSE
                        PUT "<COPIES=" + string(lv-copy#) + "><PREVIEW>" FORM "x(30)".
                END.
            WHEN 4 THEN 
                DO:
                    ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                    PUT UNFORMATTED 
                        "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".
                END.
            WHEN 5 THEN 
                DO:
                    IF v-print-fmt = "CENTBOX" THEN
                    DO:
                        IF NOT tb_BatchMail:CHECKED THEN
                            PUT "<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                        ELSE
                            PUT "<PREVIEW=PDF><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                    END.
                    ELSE IF v-print-fmt = "CSCIN" OR v-print-fmt = "CSCINStamp" THEN
                            PUT "<PREVIEW><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                        ELSE
                            PUT "<PREVIEW><PDF-LEFT=5mm><PDF-TOP=1mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".              

                END.
        END CASE.

        PUT "</PROGRESS>".
    END.

    IF LOOKUP(v-print-fmt,"SOUTHPAK,southpak-xl,PrystupExcel,ASIXprnt,Badger,Badger-Emailed,Southpakl") > 0 THEN 
    DO: 
        RUN value(v-program) (lv-multi-faxout,lines-per-page).
    END.
    ELSE IF v-print-fmt = "1/2 Page" AND rd-dest = 6 THEN 
        DO:
            PUT CONTROL CHR(27) CHR(67) CHR(44). 
            RUN value(v-program). 
            PUT CONTROL CHR(18).
        END.

        ELSE IF LOOKUP(v-print-fmt,"BlueRX,ColoniaX,ABC,Knight,Knight1,Central,Rosmar,ACPI,ColorX,ColonialLot#,Carded,CCCFGLot,CCCFGL3,Peachtreefgl3,Peachtree") > 0 THEN 
            DO:  
                RUN value(v-program) ("").
                IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy").
                IF tb_office-copy THEN RUN value(v-program) ("Office Copy").
                IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy").
            END.
            ELSE IF LOOKUP(v-print-fmt,"PremierX,Coburn,Axis") > 0 THEN 
                DO:    
                    RUN value(v-program) ("", NO).
                    IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy", NO).
                    IF tb_office-copy THEN RUN value(v-program) ("Office Copy", NO).
                    IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy", NO).
                END.
                ELSE IF LOOKUP(v-print-fmt,"PremierS") > 0 THEN 
                    DO:
                        RUN value(v-program) ("", YES).
                        IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy", YES).
                        IF tb_office-copy THEN RUN value(v-program) ("Office Copy", YES).
                        IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy", YES).
                    END.
                    ELSE RUN value(v-program). 
    OUTPUT CLOSE.

    FOR EACH report WHERE report.term-id EQ v-term-id: 
        DELETE report.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

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

    DEFINE VARIABLE vcSubject  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcMailBody AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcErrorMsg AS CHARACTER NO-UNDO.

    ASSIGN  
        vcSubject  = "INVOICE:" + vcInvNums + '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
        vcSubject  = IF tb_reprint AND NOT tb_email-orig THEN '[REPRINT] ' + vcSubject ELSE vcSubject
        vcMailBody = "Please review attached Invoice(s) for Invoice #: " + vcInvNums.


    IF NOT SEARCH (list-name) = ? THEN 
    DO: 

        IF NOT is-xprint-form AND NOT v-print-fmt EQ "Southpak-xl" AND NOT v-print-fmt EQ "PrystupExcel" THEN 
        DO:

            OS-RENAME VALUE (SEARCH (list-name)) VALUE (SEARCH (list-name) + '.txt').

            IF OS-ERROR NE 0 THEN 
            DO:
                MESSAGE 'Failed to rename TEMP file.' SKIP
                    'OS-ERROR : ' OS-ERROR
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            END.

            ELSE
                list-name = list-name + '.txt'.
        END.

        ELSE
            IF v-print-fmt NE "Southpak-XL" AND v-print-fmt <> "PrystupExcel" THEN
                list-name = lv-pdf-file.
            ELSE
                list-name = list-name + '.pdf'.
    /* Process attached BOL form */

    END.

    ELSE 
    DO:
        MESSAGE 'File attachment is missing.' SKIP
            'FileName: ' list-name
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    IF tb_attachBOL THEN
        list-name = list-name + "," + TRIM(vcBOLfiles, ",").

    IF vSoldToNo <> "" THEN 
        ASSIGN icRecType = "SoldTo"     
            icIdxKey  = icIdxKey + "|" + (vSoldToNo) +
                       IF vShipToNo <> "" THEN "|" + vShipToNo ELSE "".
    RUN custom/xpmail2.p   (INPUT   icRecType,
        INPUT   'R-INVPRT.',
        INPUT   list-name,
        INPUT   icIdxKey,
        INPUT   vcSubject,
        INPUT   vcMailBody,
        OUTPUT  vcErrorMsg).
/* for email by sold-to: need type "SoldTo" and icIdxKey(cust-no) and new key
   to have sold-no */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-broker-bol-proc C-Win 
PROCEDURE set-broker-bol-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:

        IF fi_broker-bol:SENSITIVE AND
            begin_inv:SCREEN-VALUE EQ end_inv:SCREEN-VALUE THEN
        DO:
            FIND FIRST b-broker-bol WHERE
                b-broker-bol.reftable EQ "brokerbol" AND
                b-broker-bol.CODE EQ STRING(begin_inv:SCREEN-VALUE)
                NO-LOCK NO-ERROR.

            IF AVAIL b-broker-bol THEN
            DO:
                fi_broker-bol:SCREEN-VALUE = b-broker-bol.code2.
                RELEASE b-broker-bol.
            END.
            ELSE
                fi_broker-bol:SCREEN-VALUE = "".
        END.
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
        ASSIGN tb_BatchMail:SENSITIVE  = YES
            tb_HideDialog:SENSITIVE = TRUE
            tb_attachBOL:SENSITIVE  = TRUE
            .

    ELSE ASSIGN tb_BatchMail:SENSITIVE  = FALSE
            tb_BatchMail:CHECKED    = FALSE
            tb_HideDialog:SENSITIVE = FALSE
            tb_HideDialog:CHECKED   = FALSE
            tb_attachBOL:SENSITIVE  = FALSE
            tb_attachBOL:CHECKED    = FALSE.
    IF glPaperless THEN
        tb_override-email:CHECKED = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetInvForm C-Win 
PROCEDURE SetInvForm :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icFormName AS CHAR NO-UNDO.

    is-xprint-form = NO.

    CASE icFormName:
        WHEN "Allpkg" THEN
            ASSIGN
                v-program      = "oe/rep/invallpk.p"
                lines-per-page = 60.
        WHEN "Argrov" THEN
            ASSIGN
                v-program      = "oe/rep/invargrv.p"
                lines-per-page = 66.
        WHEN "1/2 page" THEN
            ASSIGN
                v-program      = "oe/rep/invhalfp.p"
                lines-per-page = 44.
        WHEN "Livngstn" THEN
            ASSIGN
                v-program      = "oe/rep/invhalfp.p"
                lines-per-page = 66.
        WHEN "TriState" THEN
            ASSIGN
                v-program      = "oe/rep/invhalfp.p"
                lines-per-page = 41.
        WHEN "Clev 1/2" THEN
            ASSIGN
                v-program      = "oe/rep/invhalfp.p"
                lines-per-page = 42.
        WHEN "Phoenix" THEN
            ASSIGN
                v-program      = "oe/rep/invphx.p"
                lines-per-page = 62.
        WHEN "Color" THEN
            ASSIGN
                v-program      = "oe/rep/color.p"
                lines-per-page = 60.
        WHEN "Interpac" THEN
            ASSIGN
                v-program      = "oe/rep/invinter.p"
                lines-per-page = 60.
        WHEN "Royal" THEN
            ASSIGN
                v-program      = "oe/rep/invroyal.p"
                lines-per-page = 66.
        WHEN "ContSrvc" THEN
            ASSIGN
                v-program      = "oe/rep/inv-csc.p"
                lines-per-page = 66.
        WHEN "Blueridg" THEN
            ASSIGN
                v-program      = "oe/rep/invblue.p"
                lines-per-page = 66.
        WHEN "Brick" THEN
            ASSIGN
                v-program      = "oe/rep/invbrick.p"
                lines-per-page = 59.
        WHEN "Rudd" THEN
            ASSIGN
                v-program      = "oe/rep/invrudd.p"
                lines-per-page = 66.
        WHEN "Premier" THEN
            ASSIGN
                v-program      = "oe/rep/invprem.p"
                lines-per-page = 66.
        WHEN "PremierX" THEN
            ASSIGN
                v-program      = "oe/rep/invpremx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Coburn" THEN
            ASSIGN
                v-program      = "oe/rep/invcobrn.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Axis" THEN
            ASSIGN
                v-program      = "oe/rep/invaxis.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "PremierS" THEN
            ASSIGN
                v-program      = "oe/rep/invpremx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ColoniaX" THEN
            ASSIGN
                v-program      = "oe/rep/invcolnx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CCCFGLot" THEN
            ASSIGN
                v-program      = "oe/rep/invcccfg.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CCCFGL3" THEN
            ASSIGN
                v-program      = "oe/rep/invcfgl3.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Carded" THEN
            ASSIGN
                v-program      = "oe/rep/invcardx.p"
                lines-per-page = 66
                is-xprint-form = YES.

        WHEN "ABC" THEN
            ASSIGN
                v-program      = "oe/rep/invabcx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "BlueRX" THEN
            ASSIGN
                v-program      = "oe/rep/invbluex.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "MultiWll" THEN
            ASSIGN
                v-program      = "oe/rep/invmulti.p"
                lines-per-page = 57.
        WHEN "PAC 1/2" THEN
            ASSIGN
                v-program      = "oe/rep/invpack.p"
                lines-per-page = 44.
        WHEN "Triad" THEN
            ASSIGN
                v-program      = "oe/rep/invtriad.p"
                lines-per-page = 62.
        WHEN "Danbury" THEN
            ASSIGN
                v-program      = "oe/rep/invdnbry.p"
                lines-per-page = 41.
        WHEN "Sonoco" THEN
            ASSIGN
                v-program      = "oe/rep/invsono.p"
                lines-per-page = 62.
        WHEN "Empire" THEN
            ASSIGN
                v-program      = "oe/rep/invempir.p"
                lines-per-page = 60.
        WHEN "Acme" THEN
            ASSIGN
                v-program      = "oe/rep/invacme.p"
                lines-per-page = 66.
        WHEN "HOP" THEN
            ASSIGN
                v-program      = "oe/rep/invhop.p"
                lines-per-page = 42.
        WHEN "MaxPak" THEN
            ASSIGN
                v-program      = "oe/rep/invmaxpk.p"
                lines-per-page = 42.
        WHEN "Fibre" THEN
            ASSIGN
                v-program      = "oe/rep/invfibre.p"
                lines-per-page = 50.
        WHEN "Abox" THEN
            ASSIGN
                v-program      = "oe/rep/invabox.p"
                lines-per-page = 60.
        WHEN "ABOX-Xp" THEN
            ASSIGN
                v-program      = "oe/rep/invxabox.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Harwell" THEN
            ASSIGN
                v-program      = "oe/rep/invharwl.p"
                lines-per-page = 63.
        WHEN "P&P" THEN
            ASSIGN
                v-program      = "oe/rep/invpnp.p"
                lines-per-page = 62.
        WHEN "CorrCart" THEN
            ASSIGN
                v-program      = "oe/rep/invcorrc.p"
                lines-per-page = 62.
        WHEN "Chillic" THEN
            ASSIGN
                v-program      = "oe/rep/invchill.p"
                lines-per-page = 45.
        WHEN "Pacific" THEN
            ASSIGN
                v-program      = "oe/rep/invpacif.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Xprint" OR 
        WHEN "invprint 1" OR 
        WHEN "invprint 2" THEN
            ASSIGN
                v-program      = "oe/rep/invxprnt.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Boss" THEN
            ASSIGN
                v-program      = "oe/rep/invboss.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Keystone" THEN
            ASSIGN
                v-program      = "oe/rep/invkeystone.p"  /*Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Fibrex" THEN
            ASSIGN
                v-program      = "oe/rep/invfibrex.p"   /*Xprint format*/
                lines-per-page = 69
                is-xprint-form = YES.
        WHEN "ImperiaX" THEN
            ASSIGN
                v-program      = "oe/rep/invximp.p"   /*Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ConsBox" THEN
            ASSIGN
                v-program      = "oe/rep/invconsb.p"   
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "APC" THEN
            ASSIGN
                v-program      = "oe/rep/invxapc.p"   /*APC format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Allpkgx" THEN
            ASSIGN
                v-program      = "oe/rep/invalpkx.p"   /*Allpkgx Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSCIN" THEN
            ASSIGN
                v-program      = "oe/rep/invcscin.p"   /*CSCIN  format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSCINStamp" THEN
            ASSIGN
                v-program      = "oe/rep/invcstmp.p"   /*CSCINSTAMP  format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "RUDDX" THEN
            ASSIGN
                v-program      = "oe/rep/invruddx.p"   /*Rudd Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Sonocox" THEN
            ASSIGN
                v-program      = "oe/rep/invsonox.p"   /*Sonoco Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ASIXprnt" THEN
            ASSIGN
                v-program      = "oe/rep/invxasi.p"   /*ASIXprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "midwest" THEN
            ASSIGN
                v-program      = "oe/rep/invmidws.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Accord" THEN
            ASSIGN
                v-program      = "oe/rep/invaccrd.p"
                lines-per-page = 72
                is-xprint-form = YES.
        WHEN "mwbox" THEN
            ASSIGN
                v-program      = "oe/rep/invmwbox.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Southpak" THEN
            ASSIGN
                v-program      = "oe/rep/invsthpk.p" /*Southpak format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Southpak-xl" THEN
            ASSIGN
                v-program      = "oe/rep/invsthpk-xl.p" /*Southpak excel format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "PrystupExcel" THEN
            ASSIGN
                v-program      = "oe/rep/invpryst-xl.p" /*PrystupExcel excel format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Hughes" THEN
            ASSIGN
                v-program      = "oe/rep/invhughs.p"  /*Hughes format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "NStock" THEN
            ASSIGN
                v-program      = "oe/rep/invnstok.p"  /*NStock format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Hughes2" THEN
            ASSIGN
                v-program      = "oe/rep/invhugh2.p"  /*Hughes format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Concepts" THEN
            ASSIGN
                v-program      = "oe/rep/invxcorc.p"  /*Corrugate Concepts format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSC" THEN
            ASSIGN
                v-program      = "oe/rep/invxcsc.p"  /*Container Service format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Elite" THEN
            ASSIGN
                v-program      = "oe/rep/invelite.p"  /*Elite format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Adapt" THEN
            ASSIGN
                v-program      = "oe/rep/invadapt.p"  /*Adapt format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSC-GA" THEN
            ASSIGN
                v-program      = "oe/rep/invcscga.p"  /*CSC GA format*/
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "CSC-GASummary" THEN
            ASSIGN
                v-program      = "oe/rep/invcscgsm.p"  /*CSC GASummary format*/
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "ARGROVX" THEN
            ASSIGN
                v-program      = "oe/rep/invxargv.p"  /*ArgrovX format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Indiana" THEN
            ASSIGN
                v-program      = "oe/rep/invindc.p"  /*Indiana <= Elite format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Imperial" THEN
            ASSIGN
                v-program      = "oe/rep/invimper.p"
                lines-per-page = 62.
        WHEN "RFC" OR 
        WHEN "AgMach" THEN
            ASSIGN
                v-program      = "oe/rep/invrfc.p"
                lines-per-page = IF v-print-fmt EQ "RFC" THEN 62 ELSE 66.
        WHEN "Herman" THEN
            ASSIGN
                v-program      = "oe/rep/invhermn.p"
                lines-per-page = 60.
        WHEN "Century" THEN
            ASSIGN
                v-program      = "oe/rep/invcntry.p"
                lines-per-page = 60.
        WHEN "CENTBOX" THEN
            ASSIGN
                v-program      = "oe/rep/invcentx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Oracle" THEN
            ASSIGN
                v-program      = "oe/rep/invoracl.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "TriLakes" THEN
            ASSIGN
                v-program      = "oe/rep/invtri.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "TriLakesBroker" THEN
            ASSIGN
                v-program      = "oe/rep/invtribrk.p"
                lines-per-page = 66
                is-xprint-form = YES.   /* TriLakesBroker */
        WHEN "frankstn" OR 
        WHEN "Mirpkg" THEN
            ASSIGN
                v-program      = "oe/rep/invfrank.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "DEE" THEN
            ASSIGN
                v-program      = "oe/rep/invdee.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "PPI" THEN
            ASSIGN
                v-program      = "oe/rep/invppi.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Dayton" THEN
            ASSIGN
                v-program      = "oe/rep/invdaytn.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Unipak" THEN
            ASSIGN
                v-program      = "oe/rep/invunipk.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Colonial" THEN
            ASSIGN
                v-program      = "oe/rep/invasi.p"
                lines-per-page = 60.
        WHEN "HPB" THEN
            ASSIGN
                v-program      = "oe/rep/invhpb.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Knight" THEN
            ASSIGN
                v-program      = "oe/rep/invknight.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Knight1" THEN
            ASSIGN
                v-program      = "oe/rep/invknight1.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Central" THEN                                  /*task# 12041303*/
            ASSIGN
                v-program      = "oe/rep/invcentral.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Southpakl" THEN
            ASSIGN
                v-program      = "oe/rep/invsthpklg.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Androp" THEN
            ASSIGN
                v-program      = "oe/rep/invandrop.p"   
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Packrite" THEN
            ASSIGN
                v-program      = "oe/rep/invpkrt.p"  
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Rosmar" THEN
            ASSIGN
                v-program      = "oe/rep/invrosmr.p"
                lines-per-page = 66
                is-xprint-form = YES.

        WHEN "Badger" THEN
            ASSIGN
                v-program      = "oe/rep/invbadger.p"   
                lines-per-page = 66
                is-xprint-form = YES.


        WHEN "Badger-Emailed" THEN
            ASSIGN
                v-program      = "oe/rep/invbadgereml.p"   
                lines-per-page = 66
                is-xprint-form = YES.

        /* 20130718 JAD Task 02071304 Add Capitol to v-print-fmt */
        WHEN "capitol" THEN
            ASSIGN
                v-program      = "oe/rep/invcapitol.p"
                lines-per-page = 71
                is-xprint-form = YES.

        WHEN "allwest" THEN
            ASSIGN
                v-program      = "oe/rep/invallws.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "Bell" THEN
            ASSIGN
                v-program      = "oe/rep/invbell.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "Simkins" THEN
            ASSIGN
                v-program      = "oe/rep/invsmkct.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CapCityIn" THEN 
            ASSIGN
                v-program      = "oe/rep/invcapcin.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ACPI" THEN
            ASSIGN
                v-program      = "oe/rep/invacpi.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ColorX" THEN
            ASSIGN
                v-program      = "oe/rep/invcolrx.p"
                lines-per-page = 66
                is-xprint-form = YES.

        WHEN "Loylang" THEN /* LOYLANG gmd 11200902*/
            ASSIGN
                v-program      = "oe/rep/invloyln.p"
                lines-per-page = 78
                is-xprint-form = YES.
        WHEN "PrestigeLLB" THEN /* Task# 08271402*/
            ASSIGN
                v-program      = "oe/rep/invprstl.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "RFCX" THEN /*Task# 11061302*/
            ASSIGN
                v-program      = "oe/rep/invrfcx.p"
                lines-per-page = 71
                is-xprint-form = YES.  
        WHEN "LoylangBSF" THEN /* LoylangBSF - same report, just different price UOM*/
            ASSIGN
                v-program      = "oe/rep/invloyln.p"
                lines-per-page = 78
                is-xprint-form = YES.
        WHEN "Protagon" THEN /* Copied from LoyLangBSF*/
            ASSIGN
                v-program      = "oe/rep/invprot.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "Protagon2" THEN /* Copied from Protagon */
            ASSIGN
                v-program      = "oe/rep/invprot2.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "Soule" THEN       /* LOYLANG Format */
            ASSIGN
                v-program      = "oe/rep/invsoule.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "SouleMed" THEN   /* LOYLANG Format */
            ASSIGN
                v-program      = "oe/rep/invsoulemed.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "SoulePO" THEN   /* LOYLANG Format */
            ASSIGN
                v-program      = "oe/rep/invsoulepo.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "Printers" THEN       /* LOYLANG Format */
            ASSIGN
                v-program      = "oe/rep/invprnts.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "LoylangJIT" THEN
            ASSIGN
                v-program      = "oe/rep/invloyjit.p"
                lines-per-page = 76
                is-xprint-form = YES.

        WHEN "ColonialLot#" THEN
            ASSIGN
                v-program      = "oe/rep/invcolnx2.p"
                lines-per-page = 71   /* Task 10181309 */
                is-xprint-form = YES.
        WHEN "Peachtreefgl3" THEN
            ASSIGN
                v-program      = "oe/rep/invptreefgl3.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Peachtree" THEN
            ASSIGN
                v-program      = "oe/rep/invptreelot.p"
                lines-per-page = 66
                is-xprint-form = YES.
        OTHERWISE
        ASSIGN
            v-program      = "oe/rep/invasi.p"
            lines-per-page = 55.

    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetInvPostForm C-Win 
PROCEDURE SetInvPostForm :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icFormName AS CHAR NO-UNDO.

    is-xprint-form = NO.

    CASE icFormName:
        WHEN "Allpkg" THEN
            ASSIGN
                v-program      = "ar/rep/invallpk.p"
                lines-per-page = 62.
        WHEN "1/2 page" THEN
            ASSIGN
                v-program      = "ar/rep/invhalfp.p"
                lines-per-page = 44.
        WHEN "Livngstn" THEN
            ASSIGN
                v-program      = "ar/rep/invhalfp.p"
                lines-per-page = 66.
        WHEN "TriState" THEN
            ASSIGN
                v-program      = "ar/rep/invhalfp.p"
                lines-per-page = 41.
        WHEN "Clev 1/2" THEN
            ASSIGN
                v-program      = "ar/rep/invhalfp.p"
                lines-per-page = 42.
        WHEN "Phoenix" THEN
            ASSIGN
                v-program      = "ar/rep/invphx.p"
                lines-per-page = 62.
        WHEN "Color" THEN
            ASSIGN
                v-program      = "ar/rep/color.p"
                lines-per-page = 60.
        WHEN "Interpac" THEN
            ASSIGN
                v-program      = "ar/rep/invinter.p"
                lines-per-page = 60.
        WHEN "Brick" THEN
            ASSIGN
                v-program      = "ar/rep/invbrick.p"
                lines-per-page = 62.
        WHEN "Rudd" THEN
            ASSIGN
                v-program      = "ar/rep/invrudd.p"
                lines-per-page = 66.
        WHEN "Premier" THEN
            ASSIGN
                v-program      = "ar/rep/invprem.p"
                lines-per-page = 66.
        WHEN "PremierX" THEN
            ASSIGN
                v-program      = "ar/rep/invpremx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Coburn" THEN
            ASSIGN
                v-program      = "ar/rep/invcobrn.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Axis" THEN
            ASSIGN
                v-program      = "ar/rep/invaxis.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "PremierS" THEN
            ASSIGN
                v-program      = "ar/rep/invpremx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ColoniaX" THEN
            ASSIGN
                v-program      = "ar/rep/invcolnx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CCCFGLot" THEN
            ASSIGN
                v-program      = "ar/rep/invcccfg.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CCCFGL3" THEN
            ASSIGN
                v-program      = "ar/rep/invcfgl3.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Carded" THEN
            ASSIGN
                v-program      = "ar/rep/invcardx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ABC" THEN
            ASSIGN
                v-program      = "ar/rep/invabcx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "BlueRX" THEN
            ASSIGN
                v-program      = "ar/rep/invbluex.p"
                lines-per-page = 66
                is-xprint-form = YES.

        WHEN "PAC 1/2" THEN
            ASSIGN
                v-program      = "ar/rep/invpack.p"
                lines-per-page = 44.
        WHEN "Triad" THEN
            ASSIGN
                v-program      = "ar/rep/invtriad.p"
                lines-per-page = 62.
        WHEN "Danbury" THEN
            ASSIGN
                v-program      = "ar/rep/invdnbry.p"
                lines-per-page = 41.
        WHEN "Sonoco" THEN
            ASSIGN
                v-program      = "ar/rep/invsono.p"
                lines-per-page = 62.
        WHEN "Empire" THEN
            ASSIGN
                v-program      = "ar/rep/invempir.p"
                lines-per-page = 62.
        WHEN "HOP" THEN
            ASSIGN
                v-program      = "ar/rep/invhop.p"
                lines-per-page = 42.
        WHEN "MaxPak" THEN
            ASSIGN
                v-program      = "ar/rep/invmaxpk.p"
                lines-per-page = 42.
        WHEN "Fibre" THEN
            ASSIGN
                v-program      = "ar/rep/invfibre.p"
                lines-per-page = 50.
        WHEN "Abox" THEN
            ASSIGN
                v-program      = "ar/rep/invabox.p"
                lines-per-page = 60.
        WHEN "ABOX-Xp" THEN
            ASSIGN
                v-program      = "ar/rep/invxabox.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Harwell" THEN
            ASSIGN
                v-program      = "ar/rep/invharwl.p"
                lines-per-page = 63.
        WHEN "Chillic" THEN
            ASSIGN
                v-program      = "ar/rep/invchill.p"
                lines-per-page = 45.
        WHEN "Pacific" THEN
            ASSIGN
                v-program      = "ar/rep/invpacif.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Xprint" OR 
        WHEN "invprint 1" OR 
        WHEN "invprint 2" THEN
            ASSIGN
                v-program      = "ar/rep/invxprnt.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Boss" THEN
            ASSIGN
                v-program      = "ar/rep/invboss.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Keystone" THEN
            ASSIGN
                v-program      = "ar/rep/invkeystone.p"  /*Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Fibrex" THEN
            ASSIGN
                v-program      = "ar/rep/invfibrex.p"   /*Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ImperiaX" THEN
            ASSIGN
                v-program      = "ar/rep/invximp.p"   /*Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ConsBox" THEN
            ASSIGN
                v-program      = "ar/rep/invconsb.p"   
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "APC" THEN
            ASSIGN
                v-program      = "ar/rep/invxapc.p"   /*APC format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSCIN" THEN
            ASSIGN
                v-program      = "ar/rep/invcscin.p"   /*CSCIN  format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSCINStamp" THEN
            ASSIGN
                v-program      = "ar/rep/invcstmp.p"   /*CSCINSTAMP  format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "RUDDX" THEN
            ASSIGN
                v-program      = "ar/rep/invruddx.p"   /*Rudd Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Sonocox" THEN
            ASSIGN
                v-program      = "ar/rep/invsonox.p"   /*Sonoco Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ASIXprnt" THEN
            ASSIGN
                v-program      = "ar/rep/invxasi.p"   /*ASIXprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "midwest" THEN
            ASSIGN
                v-program      = "ar/rep/invmidws.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Accord" THEN
            ASSIGN
                v-program      = "ar/rep/invaccrd.p"
                lines-per-page = 72
                is-xprint-form = YES.
        WHEN "mwbox" THEN
            ASSIGN
                v-program      = "ar/rep/invmwbox.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Southpak" THEN
            ASSIGN
                v-program      = "ar/rep/invsthpk.p" /*Southpak format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Southpak-xl" THEN
            ASSIGN
                v-program      = "ar/rep/invsthpk-xl.p" /*Southpak excel format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "PrystupExcel" THEN
            ASSIGN
                v-program      = "ar/rep/invpryst-xl.p" /*PrystupExcel excel format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Hughes" THEN
            ASSIGN
                v-program      = "ar/rep/invhughs.p"  /*Hughes format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "NStock" THEN
            ASSIGN
                v-program      = "ar/rep/invnstok.p"  /*NStock format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Hughes2" THEN
            ASSIGN
                v-program      = "ar/rep/invhugh2.p"  /*Hughes format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Concepts" THEN
            ASSIGN
                v-program      = "ar/rep/invxcorc.p"  /*Corrugate Concepts format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSC" THEN
            ASSIGN
                v-program      = "ar/rep/invxcsc.p"  /*Container Service format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Elite" THEN
            ASSIGN
                v-program      = "ar/rep/invelite.p"  /*Elite format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Adapt" THEN
            ASSIGN
                v-program      = "ar/rep/invadapt.p"  /*Adapt format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSC-GA" THEN
            ASSIGN
                v-program      = "ar/rep/invcscga.p"  /*CSC-GA format*/
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "CSC-GASummary" THEN
            ASSIGN
                v-program      = "ar/rep/invcscgsm.p"  /*CSC-GASummary format*/
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "ARGROVX" THEN
            ASSIGN
                v-program      = "ar/rep/invxargv.p"  /*ArgrovX format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Indiana" THEN
            ASSIGN
                v-program      = "ar/rep/invindc.p"  /*Indiana <= Elite format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Imperial" THEN
            ASSIGN
                v-program      = "ar/rep/invimper.p"
                lines-per-page = 62.
        WHEN "RFC" OR 
        WHEN "AgMach" THEN
            ASSIGN
                v-program      = "ar/rep/invrfc.p"
                lines-per-page = IF v-print-fmt EQ "RFC" THEN 62 ELSE 66.
        WHEN "Herman" THEN
            ASSIGN
                v-program      = "ar/rep/invhermn.p"
                lines-per-page = 62.
        WHEN "CENTBOX" THEN
            ASSIGN
                v-program      = "ar/rep/invcentx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Oracle" THEN
            ASSIGN
                v-program      = "ar/rep/invoracl.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "TriLakes" THEN
            ASSIGN
                v-program      = "ar/rep/invtri.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "TriLakesBroker" THEN
            ASSIGN
                v-program      = "ar/rep/invtribrk.p"
                lines-per-page = 66
                is-xprint-form = YES.      /*  TriLakesBroker  */
        WHEN "frankstn" OR 
        WHEN "Mirpkg" THEN
            ASSIGN
                v-program      = "ar/rep/invfrank.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "DEE" THEN
            ASSIGN
                v-program      = "ar/rep/invdee.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "PPI" THEN
            ASSIGN
                v-program      = "ar/rep/invppi.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Dayton" THEN
            ASSIGN
                v-program      = "ar/rep/invdaytn.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Unipak" THEN
            ASSIGN
                v-program      = "ar/rep/invunipk.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "HPB" THEN
            ASSIGN
                v-program      = "ar/rep/invhpb.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Allpkgx" THEN
            ASSIGN
                v-program      = "ar/rep/invalpkx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ILWALKER" THEN
            ASSIGN
                v-program      = "oe/rep/invilwalkp.p"
                lines-per-page = 55.
        WHEN "Knight" THEN
            ASSIGN
                v-program      = "ar/rep/invknight.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Southpakl" THEN
            ASSIGN
                v-program      = "ar/rep/invsthpklg.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Androp" THEN
            ASSIGN
                v-program      = "ar/rep/invandrop.p"   
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Knight1" THEN
            ASSIGN
                v-program      = "ar/rep/invknight1.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Central" THEN                                  /*task# 12041303*/
            ASSIGN
                v-program      = "ar/rep/invcentral.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Packrite" THEN
            ASSIGN
                v-program      = "ar/rep/invpkrt.p"  
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Rosmar" THEN
            ASSIGN
                v-program      = "ar/rep/invrosmr.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Badger" THEN
            ASSIGN
                v-program      = "ar/rep/invbadger.p"   
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Badger-Emailed" THEN
            ASSIGN
                v-program      = "ar/rep/invbadgereml.p"   
                lines-per-page = 66
                is-xprint-form = YES.

        /* 20130718 JAD Task 02071304 Add Capitol to v-print-fmt */
        WHEN "capitol" THEN
            ASSIGN
                v-program      = "ar/rep/invcapitol.p"
                lines-per-page = 71
                is-xprint-form = YES.

        WHEN "allwest" THEN
            ASSIGN
                v-program      = "ar/rep/invallws.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "Bell" THEN
            ASSIGN
                v-program      = "ar/rep/invbell.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "Simkins" THEN
            ASSIGN
                v-program      = "ar/rep/invsmkct.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CapCityIn" THEN 
            ASSIGN
                v-program      = "ar/rep/invcapcin.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ACPI" THEN
            ASSIGN
                v-program      = "ar/rep/invacpi.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ColorX" THEN
            ASSIGN
                v-program      = "ar/rep/invcolrx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "loylang" THEN /* LOYLANG gmd 11200902 */
            ASSIGN
                v-program      = "ar/rep/invloyln.p"
                lines-per-page = 78             
                is-xprint-form = YES.
        WHEN "PrestigeLLB" THEN /* Task# 08271402*/
            ASSIGN
                v-program      = "ar/rep/invprstl.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "RFCX" THEN /*Task# 11061302*/
            ASSIGN
                v-program      = "ar/rep/invrfcx.p"
                lines-per-page = 71             
                is-xprint-form = YES.  
        WHEN "LoylangBSF" THEN /* small mod to Loylang with Price/BSF instead of price */
            ASSIGN
                v-program      = "ar/rep/invloyln.p"
                lines-per-page = 78             
                is-xprint-form = YES.
        WHEN "Protagon" THEN /* Copied form LoyLangBSF */
            ASSIGN
                v-program      = "ar/rep/invprot.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "Protagon2" THEN /* Copied from Protagon */
            ASSIGN
                v-program      = "ar/rep/invprot2.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "Soule" THEN /* LOYLANG Format */
            ASSIGN
                v-program      = "ar/rep/invsoule.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "SouleMed" THEN /* LOYLANG Format */
            ASSIGN
                v-program      = "ar/rep/invsoulemed.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "SoulePO" THEN /* LOYLANG Format */
            ASSIGN
                v-program      = "ar/rep/invsoulepo.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "Printers" THEN /* LOYLANG Format */
            ASSIGN
                v-program      = "ar/rep/invprnts.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "loylangjit" THEN
            ASSIGN
                v-program      = "ar/rep/invloyjit.p"
                lines-per-page = 76
                is-xprint-form = YES.
        WHEN "ColonialLot#" THEN
            ASSIGN
                v-program      = "ar/rep/invcolnx2.p"
                lines-per-page = 71     /* Task 10181309   */
                is-xprint-form = YES.
        WHEN "Peachtreefgl3" THEN
            ASSIGN
                v-program      = "ar/rep/invptreefgl3.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Peachtree" THEN
            ASSIGN
                v-program      = "ar/rep/invptreelot.p"
                lines-per-page = 66
                is-xprint-form = YES.
        OTHERWISE
        ASSIGN
            v-program      = "ar/rep/invasi.p"
            lines-per-page = 66.

    END CASE.
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
    DEF VAR lv-frame-hdl  AS HANDLE NO-UNDO.
    DEF VAR lv-group-hdl  AS HANDLE NO-UNDO.
    DEF VAR lv-field-hdl  AS HANDLE NO-UNDO.
    DEF VAR lv-field2-hdl AS HANDLE NO-UNDO.
    DEF VAR parm-fld-list AS cha    NO-UNDO.
    DEF VAR parm-lbl-list AS cha    NO-UNDO.
    DEF VAR i             AS INT    NO-UNDO.
    DEF VAR lv-label      AS cha.

    lv-frame-hdl = FRAME {&frame-name}:handle.
    lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
    lv-field-hdl = lv-group-hdl:FIRST-CHILD .

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
            THEN 
        DO:
            IF lv-field-hdl:LABEL <> ? THEN 
                ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                    .
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    .
                lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN 
                    DO:
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
            entry(i,parm-lbl-list) NE "" THEN 
        DO:

            lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                trim(ENTRY(i,parm-lbl-list)) + ":".

            PUT lv-label FORMAT "x(35)" AT 5
                SPACE(1)
                TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
                SKIP.              
        END.
    END.

    PUT FILL("-",80) FORMAT "x(80)" SKIP.

    PAGE.

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
    DISABLE TRIGGERS FOR LOAD OF {&line}.
    DISABLE TRIGGERS FOR LOAD OF inv-misc.


    RELEASE {&line}.
    RELEASE inv-misc.

    FIND FIRST {&line} WHERE RECID({&line}) EQ INT(save-line.val[3]) NO-ERROR.

    IF AVAIL {&line} THEN {&line}.{&rno} = save-line.val[1].

    ELSE
        FIND FIRST inv-misc WHERE RECID(inv-misc) EQ INT(save-line.val[3]) NO-ERROR.

    IF AVAIL inv-misc THEN inv-misc.{&miscrno} = save-line.val[1].

    DELETE save-line.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetPaperlessLogical C-Win 
FUNCTION GetPaperlessLogical RETURNS LOGICAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  Return the value of the N-K Paperless Log value
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lPaperless AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound     AS LOGICAL   NO-UNDO.

    lPaperless = NO.
    RUN sys/ref/nk1look.p (cocode, "PAPERLESS", "L", NO, NO, "", "", 
        OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN
        lPaperless = cReturn EQ "YES".

    RETURN lPaperless.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

