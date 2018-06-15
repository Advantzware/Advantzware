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
&IF DEFINED(head) = 0 &THEN 
&global-define LINE ar-invl
&global-define head ar-inv
&global-define bolno inv-no
&global-define multiinvoice exported
&global-define soldno sold-no
&global-define rno x-no
&global-define miscrno r-no
&global-define vprgmname "r-invprt."
&ENDIF

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipcInvoiceType AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcPrgmnameOverride AS CHAR NO-UNDO.


/* Local Variable Definitions ---                                       */
DEF    VAR      list-name  AS cha       NO-UNDO.
DEFINE VARIABLE init-dir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cActualPdf AS CHARACTER NO-UNDO.
DEFINE VARIABLE hSuperProc AS HANDLE.
{methods/defines/hndldefs.i}

{custom/prgsecur.i}

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

DEF BUFFER save-line    FOR reftable.
DEF BUFFER b1-cust      FOR cust.
DEF BUFFER b-ar-inv     FOR ar-inv.

DEF BUFFER b-cust       FOR cust.
DEF BUFFER b-broker-bol FOR reftable.

DEFINE VARIABLE begin_cust-screen-value AS CHARACTER NO-UNDO.
DEFINE VARIABLE end_cust-screen-value   AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_reprint-screen-value AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_posted-screen-value  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE begin_bol-screen-value  AS CHARACTER NO-UNDO.
DEFINE VARIABLE end_bol-screen-value    AS CHARACTER NO-UNDO.
/* bol date */  
DEFINE VARIABLE begin_date-screen-value AS CHARACTER NO-UNDO. 
DEFINE VARIABLE end_date-screen-value   AS CHARACTER NO-UNDO. 

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
/* Allows for other names besides r-invprt. */
v-prgmname = ipcPrgmnameOverride.

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
end_inv begin_date end_date tb_reprint tb_posted tb_setcomp tb_prt-inst ~
tb_qty-all rd_sort tb_BatchMail tb_HideDialog tb_attachBOL rd-dest lv-ornt ~
lines-per-page lv-font-no tb_email-orig tb_override-email td-show-parm ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_inv end_inv ~
begin_date end_date tb_reprint tb_posted tb_setcomp tb_prt-inst tb_qty-all ~
lbl_sort rd_sort tb_BatchMail tb_HideDialog tb_attachBOL rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name tb_email-orig tb_override-email ~
td-show-parm tb_splitPDF fiEndDateLabel fiBeginDateLabel 

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

DEFINE VARIABLE begin_bol AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Beginning BOL#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_inv AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Beginning Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_bol AS INTEGER FORMAT ">>>>>>>9" INITIAL 99999999 
     LABEL "Ending BOL#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "Ending Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fiBeginDateLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Beginning Bol Date:" 
      VIEW-AS TEXT 
     SIZE 19 BY .62 NO-UNDO.

DEFINE VARIABLE fiEndDateLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Ending Bol Date:" 
      VIEW-AS TEXT 
     SIZE 17 BY .62 NO-UNDO.

DEFINE VARIABLE fi_broker-bol AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Broker BOL#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE fi_depts AS CHARACTER FORMAT "X(100)" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

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

DEFINE VARIABLE lv-scr-num-copies AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "# of Copies" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "BOL" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer", "Customer",
"BOL", "BOL"
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE rs_no_PN AS INTEGER INITIAL 1 
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
     SIZE 94 BY 13.81.

DEFINE VARIABLE tbPostedAR AS LOGICAL INITIAL no 
     LABEL "Posted AR Invoices" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.8 BY .81 NO-UNDO.

DEFINE VARIABLE tb_attachBOL AS LOGICAL INITIAL no 
     LABEL "Attach Signed BOL" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE tb_BatchMail AS LOGICAL INITIAL no 
     LABEL "&Batch E-Mail" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_collate AS LOGICAL INITIAL no 
     LABEL "Collate?" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-copy AS LOGICAL INITIAL no 
     LABEL "Customer Copy?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tb_email-orig AS LOGICAL INITIAL no 
     LABEL "Email as Original?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE tb_HideDialog AS LOGICAL INITIAL no 
     LABEL "&Hide Dialog-Box" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_office-copy AS LOGICAL INITIAL no 
     LABEL "Office Copy?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tb_override-email AS LOGICAL INITIAL yes 
     LABEL "Ignore Paperless Setting?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_posted AS LOGICAL INITIAL no 
     LABEL "Reprint Posted Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_print-dept AS LOGICAL INITIAL no 
     LABEL "Print Dept Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-inst AS LOGICAL INITIAL yes 
     LABEL "Print Instructions?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-zero-qty AS LOGICAL INITIAL yes 
     LABEL "Print if Inv/Ship Qty = 0?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_qty-all AS LOGICAL INITIAL no 
     LABEL "Print Enhanced Quantities (Ordered, Shipped, Invoiced)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 61 BY .81 NO-UNDO.

DEFINE VARIABLE tb_reprint AS LOGICAL INITIAL no 
     LABEL "Reprint Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE tb_setcomp AS LOGICAL INITIAL no 
     LABEL "Print Set Component?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_sman-copy AS LOGICAL INITIAL no 
     LABEL "SalesRep Copy?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tb_splitPDF AS LOGICAL INITIAL no 
     LABEL "PDF Per Invoice" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
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
          "Enter Beginning BOL Date" NO-LABEL
     end_date AT ROW 4.33 COL 69 COLON-ALIGNED HELP
          "Enter Ending BOL Date" NO-LABEL
     begin_bol AT ROW 5.29 COL 26 COLON-ALIGNED HELP
          "Enter Beginning BOL Number"
     end_bol AT ROW 5.29 COL 69 COLON-ALIGNED HELP
          "Enter Ending BOL Number"
     tb_reprint AT ROW 6.62 COL 28.2
     lv-scr-num-copies AT ROW 6.62 COL 69.8 COLON-ALIGNED
     tb_posted AT ROW 7.48 COL 28.2
     tb_collate AT ROW 7.48 COL 59.4
     tbPostedAR AT ROW 8.38 COL 28.2 WIDGET-ID 24
     tb_setcomp AT ROW 8.43 COL 59.2 WIDGET-ID 2
     tb_prt-inst AT ROW 9.1 COL 49 RIGHT-ALIGNED
     tb_print-dept AT ROW 9.95 COL 49 RIGHT-ALIGNED
     fi_depts AT ROW 9.95 COL 48.4 COLON-ALIGNED HELP
          "Enter Departments separated by commas" NO-LABEL
     tb_qty-all AT ROW 10.14 COL 28 WIDGET-ID 28
     tb_prt-zero-qty AT ROW 10.95 COL 56 RIGHT-ALIGNED WIDGET-ID 12
     lbl_sort AT ROW 12 COL 26.2 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 12 COL 39 NO-LABEL
     fi_broker-bol AT ROW 12 COL 75 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number" WIDGET-ID 6
     rs_no_PN AT ROW 13 COL 28.2 NO-LABEL WIDGET-ID 8
     tb_cust-copy AT ROW 13.91 COL 12
     tb_office-copy AT ROW 13.91 COL 39
     tb_sman-copy AT ROW 13.91 COL 64
     tb_BatchMail AT ROW 15.86 COL 48.8 RIGHT-ALIGNED
     tb_HideDialog AT ROW 15.86 COL 48
     tb_attachBOL AT ROW 15.86 COL 69 WIDGET-ID 16
     rd-dest AT ROW 16.33 COL 5 NO-LABEL
     lv-ornt AT ROW 17.05 COL 30 NO-LABEL
     lines-per-page AT ROW 17.05 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 18.29 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 19.24 COL 27 COLON-ALIGNED NO-LABEL
     tb_email-orig AT ROW 20.57 COL 30.2 WIDGET-ID 14
     tb_override-email AT ROW 20.62 COL 60 WIDGET-ID 18
     td-show-parm AT ROW 21.95 COL 30
     tb_splitPDF AT ROW 21.95 COL 60 WIDGET-ID 26
     btn-ok AT ROW 23.86 COL 23
     btn-cancel AT ROW 23.86 COL 56
     fiEndDateLabel AT ROW 4.52 COL 51.4 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     fiBeginDateLabel AT ROW 4.57 COL 6.8 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.43 COL 4
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.38 COL 4
     RECT-6 AT ROW 15.19 COL 1.4
     RECT-7 AT ROW 1.24 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 24.43.


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
         HEIGHT             = 24.67
         WIDTH              = 96.2
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
/* SETTINGS FOR FILL-IN begin_bol IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       begin_bol:HIDDEN IN FRAME FRAME-A           = TRUE
       begin_bol:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

/* SETTINGS FOR FILL-IN end_bol IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       end_bol:HIDDEN IN FRAME FRAME-A           = TRUE
       end_bol:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fiBeginDateLabel IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       fiBeginDateLabel:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR FILL-IN fiEndDateLabel IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       fiEndDateLabel:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR FILL-IN fi_broker-bol IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_broker-bol:HIDDEN IN FRAME FRAME-A           = TRUE
       fi_broker-bol:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fi_depts IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_depts:HIDDEN IN FRAME FRAME-A           = TRUE
       fi_depts:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-scr-num-copies IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-scr-num-copies:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR RADIO-SET rs_no_PN IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rs_no_PN:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tbPostedAR IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tbPostedAR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_BatchMail IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_BatchMail:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_collate IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_collate:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_collate:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_cust-copy IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_cust-copy:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tb_email-orig:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_HideDialog:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_office-copy IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_office-copy:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tb_override-email:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_posted:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_print-dept IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
       tb_print-dept:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_print-dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-inst IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_prt-inst:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-zero-qty IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
       tb_prt-zero-qty:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_prt-zero-qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_reprint:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_sman-copy IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_sman-copy:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_splitPDF IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_splitPDF:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_splitPDF:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

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
        /* Used by calling procedure (e.g. v-oeinv.w to refresh values) */
        PUBLISH "eventInvoicePrinted".
        
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
ON LEAVE OF begin_date IN FRAME FRAME-A
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
  
        DEFINE    VARIABLE      lv-fax-type       AS cha     NO-UNDO.
        DEFINE    VARIABLE      vlSkipRec         AS LOG     NO-UNDO.
        DEFINE    VARIABLE      ll-secure         AS LOG     INIT YES NO-UNDO.
        DEFINE    VARIABLE      lBadStatus        AS LOGICAL   NO-UNDO.
        DEFINE    VARIABLE      cBadStatusInvoice AS CHARACTER NO-UNDO.
        DEFINE    VARIABLE      cBadStatusBol     AS CHARACTER NO-UNDO.
        DEFINE    VARIABLE      lCheckHoldStat    AS LOGICAL NO-UNDO .
        DEFINE BUFFER bf-cust FOR cust.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}
                tb_collate lv-scr-num-copies
                tb_cust-copy tb_office-copy tb_sman-copy
            /* gdm - 12080817 */ tb_setcomp tbPostedAR
                .
            IF begin_bol:SENSITIVE THEN 
                ASSIGN begin_bol end_bol.

            IF fi_broker-bol:SENSITIVE THEN
                ASSIGN fi_broker-bol.
        END.

        IF rs_no_PN:HIDDEN = NO THEN
            ASSIGN rs_no_PN
                svi-print-item = rs_no_PN.

        IF tb_prt-zero-qty:HIDDEN = NO THEN
            ASSIGN tb_prt-zero-qty
                s-print-zero-qty = tb_prt-zero-qty.
                
        /* In case this was set and now are printing to screen */
        IF rd-dest EQ 2 OR rd-dest EQ 1 THEN
           tb_splitPDF = NO.
           
        IF fi_broker-bol:SENSITIVE AND
            fi_broker-bol:SCREEN-VALUE NE "" AND
            begin_inv NE end_inv THEN
        DO:
            MESSAGE "For Broker BOL# to be used, Beginning and Ending Invoice# must be the same."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            LEAVE.
        END.
     
        ASSIGN
            tb_setcomp      = LOGICAL(tb_setcomp:SCREEN-VALUE)
            lv-multi-faxout = IF rd-dest = 4 AND begin_cust <> END_cust THEN YES 
                       ELSE NO.

        IF is-xprint-form AND rd-dest = 4 THEN lv-multi-faxout = YES.

        lv-fax-type = IF lv-multi-faxout THEN "MULTI" ELSE "CUSTOMER".

        /* To indicate whether to use this or tb_posted */
        IF tbPostedAR:HIDDEN = YES THEN 
          tbPostedAR = ?.

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
        
        RUN assignScreenValues
            (fi_depts:HIDDEN           ,
            tb_print-dept:SCREEN-VALUE,
            fi_depts:SCREEN-VALUE     ,
            FRAME {&frame-name}:HANDLE ,
            rd-dest:SCREEN-VALUE      ,
            tb_batchMail:CHECKED      , 
            fi_broker-bol:SENSITIVE   ,
            fi_broker-bol:SCREEN-VALUE,
            tb_collate:HIDDEN         ,
            tb_HideDialog:CHECKED     ,
            c-win:TITLE               ,
            lCheckHoldStat            ,
            list-name                 ,
            init-dir                  ,
            cActualPdf                ,
            vcDefaultForm             ,
            v-prgmname                ,
            ipcInvoiceType            ,
            THIS-PROCEDURE:HANDLE).
        
        RUN assignSelections
            (begin_bol          ,
            begin_cust         ,
            begin_date         ,
            begin_inv          ,
            end_bol            ,
            end_cust           ,
            end_date           ,
            end_inv            ,
            fi_broker-bol      ,
            fi_depts           ,
            lbl_sort           ,
            lines-per-page     ,
            lv-font-name       ,
            lv-font-no         ,
            lv-scr-num-copies  ,
            lv-ornt            ,
            rd-dest            ,
            rd_sort            ,
            rs_no_PN           ,
            tb_attachBOL       ,
            tb_BatchMail       ,
            tb_collate         ,
            tb_cust-copy       ,
            tb_email-orig      ,
            tb_HideDialog      ,
            tb_office-copy     ,
            tb_override-email  ,
            tb_posted          ,
            tb_print-dept      ,
            tb_prt-inst        ,
            tb_prt-zero-qty    ,
            tb_reprint         ,
            tb_setcomp         ,
            tb_sman-copy       ,
            td-show-parm       ,
            tbPostedAR         ,
            tb_splitPDF        ,
            tb_qty-all
            ).

        IF begin_bol EQ end_bol THEN 
        DO:
            
            RUN bolValidate (OUTPUT lBadStatus, OUTPUT cBadStatusInvoice, OUTPUT cBadStatusBol).
            IF lBadStatus THEN DO:        
                MESSAGE "Invoice " + cBadStatusInvoice + " with Bol " +                             
                    cBadStatusBol + " will not print, status must be approved" 
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
       /* IF rd-dest <> 5 
            /*       AND CAN-FIND(FIRST cust                    */
            /*                 WHERE cust.company EQ cocode     */
            /*                   AND cust.cust-no GE begin_cust */
            /*                   AND cust.cust-no LE end_cust   */
            /*                   AND cust.log-field[1])         */
            AND NOT tb_override-email
            THEN DO:

           RUN validateCustPaper.

        END.*/  /* ticket 22557 */ 


        IF NOT rd-dest:SCREEN-VALUE = '5' AND NOT rd-dest:SCREEN-VALUE = '1' THEN
        DO:

          RUN runReport5 (INPUT lv-fax-type).
          
        END. /*not rd-dest = 5*/

        IF rd-dest:SCREEN-VALUE = '5' THEN 
        DO:

            IF NOT tb_BatchMail:CHECKED THEN 
            DO:
                IF begin_cust <> end_cust THEN 
                DO:

                    /*          IF NOT tb_BatchMail:SENSITIVE THEN DO: */

                    MESSAGE 'Please check Batch E-Mail to send to multiple customers in the specified range.'
                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
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
           RUN runReport1 (INPUT lv-fax-type).            
            
        END.  /* rd-dest:Screen-value = 1*/


        IF v-ftp-done THEN MESSAGE "File Export/FTP is completed." VIEW-AS ALERT-BOX INFORMATION.
        OS-DELETE VALUE(init-dir + "\Invoice.pdf").
        /* Implement in persistent procedure 
        RELEASE {&head} .
        RELEASE {&line} .
        */
        RELEASE inv-misc .
        
 /*   END. */
    
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
ON LEAVE OF end_date IN FRAME FRAME-A
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv C-Win
ON LEAVE OF end_inv IN FRAME FRAME-A /* Ending Invoice# */
DO:

       DEFINE VARIABLE cBeginBolDate AS CHARACTER   NO-UNDO.
       DEFINE VARIABLE cEndBolDate AS CHARACTER   NO-UNDO.
       DEFINE VARIABLE cBeginCustomer AS CHARACTER   NO-UNDO.
       DEFINE VARIABLE cEndCustomer AS CHARACTER   NO-UNDO.


       ASSIGN {&self-name}.

       IF begin_inv = END_inv THEN DO:

          RUN setBolDates (INPUT begin_inv:screen-value, INPUT end_inv:screen-value, INPUT begin_cust:SCREEN-VALUE, INPUT end_cust:SCREEN-VALUE, 
                           OUTPUT cBeginBolDate, OUTPUT cEndBolDate,
                           OUTPUT cBeginCustomer, OUTPUT cEndCustomer).

          IF cBeginBolDate NE ? THEN ASSIGN begin_date:SCREEN-VALUE = cBeginBolDate
                                            end_date:SCREEN-VALUE   = cEndBolDate.
          IF cBeginCustomer NE ? AND cEndCustomer NE ? THEN 
             ASSIGN begin_cust:SCREEN-VALUE = cBeginCustomer
                    end_cust:SCREEN-VALUE   = cEndCustomer.
                    
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
    IF rd-dest EQ 2 THEN 
      ASSIGN 
          tb_splitPDF:HIDDEN    = YES
          tb_splitPDF:SENSITIVE = NO
          .        
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
        IF tb_BatchMail THEN
          ASSIGN 
             tb_splitPDF:HIDDEN = NO
             tb_splitPDF:SENSITIVE = YES
             .
        ELSE
          ASSIGN 
             tb_splitPDF:HIDDEN = YES
             tb_splitPDF:SENSITIVE = NO
             .
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
    IF tb_posted THEN 
      ASSIGN tb_reprint              = YES
             tb_reprint:SCREEN-VALUE = "YES".
    IF VALID-HANDLE(hSuperProc) THEN DO:
       THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE (hSuperProc).
       DELETE OBJECT hSuperProc.
    END.
  IF ipcInvoiceType EQ "inv-head" AND NOT tb_posted THEN DO:

    ASSIGN fiBeginDateLabel:SCREEN-VALUE = "Beginning Bol Date:"
           fiEndDateLabel:SCREEN-VALUE = "Ending Bol Date:"
           tbPostedAr:HIDDEN = YES
           tbPostedAR:SENSITIVE = NO
           .
    RUN oerep/r-invprtOESuper.p PERSISTENT SET hSuperProc.
  END.


  ELSE DO:
   RUN oerep/r-invprtARSuper.p PERSISTENT SET hSuperProc.
   ASSIGN fiBeginDateLabel:SCREEN-VALUE = "Beginning Inv Date:"
          fiEndDateLabel:SCREEN-VALUE = "Ending Inv Date:".
   /* Posted AR not needed from A-U-3 */
   IF ipcInvoiceType EQ "ar-inv"  THEN
     ASSIGN
            tbPostedAr:HIDDEN = YES
            tbPostedAR:SENSITIVE = NO
            .
   ELSE DO:
        /* using inv-head and tb_posted is yes */
        ASSIGN
            tbPostedAr:HIDDEN = NO 
            tbPostedAR:SENSITIVE = YES
            .
   END.

  END.

    
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hSuperProc).
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


&Scoped-define SELF-NAME tb_qty-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_qty-all C-Win
ON VALUE-CHANGED OF tb_qty-all IN FRAME FRAME-A /* Print Enhanced Quantities (Ordered, Shipped, Invoiced)? */
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
ON CLOSE OF THIS-PROCEDURE DO:
    
    DELETE OBJECT hSuperProc.
    RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* {oerep/r-invprt.i} */
IF ipcInvoiceType EQ "inv-head" THEN DO:
  ASSIGN fiBeginDateLabel:SCREEN-VALUE = "Beginning Bol Date:"
         fiEndDateLabel:SCREEN-VALUE = "Ending Bol Date:"
         .
  RUN oerep/r-invprtOESuper.p PERSISTENT SET hSuperProc.
END.


ELSE DO:
 RUN oerep/r-invprtARSuper.p PERSISTENT SET hSuperProc.
 ASSIGN fiBeginDateLabel:SCREEN-VALUE = "Beginning Inv Date:"
        fiEndDateLabel:SCREEN-VALUE = "Ending Inv Date:"
        .

END.


THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hSuperProc).

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
        
    IF ipcInvoiceType EQ "inv-head" THEN 
      ASSIGN begin_bol:HIDDEN = NO
             begin_bol:SENSITIVE = YES
             end_bol:HIDDEN = NO
             end_bol:SENSITIVE = YES
            .            
    ELSE
      ASSIGN tb_posted:HIDDEN = NO
             tb_posted:SENSITIVE = YES
             tbPostedAr:HIDDEN = YES
             tbPostedAR:SENSITIVE = NO
             .
     
    IF LOOKUP(v-print-fmt,"Boxtech,Imperial") GT 0 THEN lv-prt-bypass = YES.

    IF v-print-fmt EQ "XPRINT" OR v-print-fmt EQ "lovepac" OR v-print-fmt EQ "invprint10-CAN" OR v-print-fmt EQ "Boss" OR v-print-fmt EQ "Simkins" OR v-print-fmt EQ "CapCityIn" THEN
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

        IF LOOKUP(v-print-fmt,"PremierX,Coburn,Axis,BlueRx,ColoniaX,ABC,Nosco,Nosco1,Central,ACPI,ColorX,ColonialLot#,Carded,CCCFGLot,CCCFGL3,Peachtreefgl3,Peachtree,PremierS") > 0 THEN
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
        
        IF ipcInvoiceType EQ "inv-head" THEN
          ASSIGN tb_posted:SCREEN-VALUE = "NO"
                 fiBeginDateLabel:SCREEN-VALUE = "Beginning Bol Date:"
                 fiEndDateLabel:SCREEN-VALUE = "Ending Bol Date:"          
                 .
        IF ipcInvoiceType NE "inv-head" THEN 
         ASSIGN fiBeginDateLabel:SCREEN-VALUE = "Beginning Inv Date:"
                fiEndDateLabel:SCREEN-VALUE = "Ending Inv Date:"
                .


          IF tb_BatchMail:SCREEN-VALUE = "YES" THEN
  ASSIGN 
     tb_splitPDF:HIDDEN = NO
     tb_splitPDF:SENSITIVE = YES
     .
ELSE
  ASSIGN 
     tb_splitPDF:HIDDEN = YES
     tb_splitPDF:SENSITIVE = NO
     .        
  IF v-print-fmt EQ "invprint 10" OR v-print-fmt EQ  "invprint 20" THEN
         ASSIGN tb_qty-all:HIDDEN = NO .
     ELSE tb_qty-all:HIDDEN = YES .

lines-per-page:SCREEN-VALUE = STRING(lines-per-page).
IF glPaperless THEN 
    tb_override-email:CHECKED = FALSE.



/* Include file */
    RUN setBOLRange (INPUT begin_bol:SCREEN-VALUE ,  
                    INPUT end_bol:SCREEN-VALUE,
                    INPUT begin_cust:SCREEN-VALUE,
                    INPUT end_cust:SCREEN-VALUE,
                    INPUT begin_inv:SCREEN-VALUE ,
                    INPUT end_inv:SCREEN-VALUE ,    
                    INPUT tb_reprint:SCREEN-VALUE,
                    INPUT begin_date:SCREEN-VALUE,
                    INPUT end_date:SCREEN-VALUE,
                    INPUT tb_posted:SCREEN-VALUE ,                        
                    OUTPUT begin_cust-screen-value ,
                    OUTPUT end_cust-screen-value,
                    OUTPUT tb_reprint-screen-value,
                    OUTPUT tb_posted-screen-value, 
                    OUTPUT begin_bol-screen-value,
                    OUTPUT end_bol-screen-value,
                    OUTPUT begin_date-screen-value ,
                    OUTPUT end_date-screen-value 
                    ).

IF begin_cust-screen-value NE ? THEN begin_cust:SCREEN-VALUE = begin_cust-SCREEN-VALUE.
IF end_cust-screen-value   NE ? THEN end_cust:SCREEN-VALUE   = end_cust-SCREEN-VALUE .
IF tb_reprint-screen-value NE ? THEN tb_reprint:SCREEN-VALUE = tb_reprint-SCREEN-VALUE.
IF tb_posted-screen-value  NE ? THEN tb_posted:SCREEN-VALUE  = tb_posted-SCREEN-VALUE. 
IF begin_bol-screen-value  NE ? THEN begin_bol:SCREEN-VALUE  = begin_bol-SCREEN-VALUE. 
IF end_bol-screen-value    NE ? THEN end_bol:SCREEN-VALUE    = end_bol-SCREEN-VALUE.                                                            
IF begin_date-screen-value NE ? THEN begin_date:SCREEN-VALUE = begin_date-SCREEN-VALUE.
IF end_date-screen-value   NE ? THEN end_date:SCREEN-VALUE   = end_date-SCREEN-VALUE. 

/*END. */

/* 05301305 - IF from-to invoice is completed, then the from-to bol# is */
/* irrelevant and was causing problems                                  */
IF INT(begin_inv:SCREEN-VALUE  IN FRAME frame-a) GT 0 AND INT(end_inv:SCREEN-VALUE  IN FRAME frame-a) GT 0 THEN
    ASSIGN begin_bol:SCREEN-VALUE  IN FRAME frame-a = "" end_bol:SCREEN-VALUE  IN FRAME frame-a   = "99999999".
RUN SetEmailBoxes.

fi_broker-bol:SCREEN-VALUE  IN FRAME frame-a = "".

RUN set-broker-bol-proc.

APPLY "entry" TO begin_cust  IN FRAME frame-a.

ASSIGN 
    tb_email-orig:SCREEN-VALUE  IN FRAME frame-a = "NO"
    tb_email-orig                                = NO.
END.

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
  DISPLAY begin_cust end_cust begin_inv end_inv begin_date end_date tb_reprint 
          tb_posted tb_setcomp tb_prt-inst tb_qty-all lbl_sort rd_sort 
          tb_BatchMail tb_HideDialog tb_attachBOL rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name tb_email-orig tb_override-email td-show-parm 
          tb_splitPDF fiEndDateLabel fiBeginDateLabel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_cust end_cust begin_inv end_inv begin_date 
         end_date tb_reprint tb_posted tb_setcomp tb_prt-inst tb_qty-all 
         rd_sort tb_BatchMail tb_HideDialog tb_attachBOL rd-dest lv-ornt 
         lines-per-page lv-font-no tb_email-orig tb_override-email td-show-parm 
         btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateReport C-Win 
PROCEDURE GenerateReport :
/*------------------------------------------------------------------------------
     Purpose:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-broker-bol-proc C-Win 
PROCEDURE set-broker-bol-proc :
/*------------------------------------------------------------------------------
     Purpose:
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

