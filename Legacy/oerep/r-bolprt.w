&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : r-bolprt.w

  Description       : BOL Printing

  Author            : JLF

  Created           : 04/23/02

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

/* Variables */
def var list-name         as char no-undo.
def var init-dir          as char no-undo.
DEF VAR v-EDIBOLPost-log AS LOG NO-UNDO.
DEF VAR v-EDIBOLPost-char AS CHAR FORMAT "X(200)" NO-UNDO.

DEF VAR lr-rel-lib AS HANDLE NO-UNDO.

/* Includes */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
{sys/inc/var.i new shared}
{oe/rep/oe-lad.i NEW}
{oe/oe-bolpi.i NEW}  
{oe/bolcheck.i NEW}  
{oe/closchk.i NEW}
{custom/formtext.i NEW}
{oerep/r-bolx.i NEW}

ASSIGN
  cocode = gcompany
  locode = gloc.

{XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XMLBOL &Company=cocode} /* rstark 05181205 */
{XMLOutput/XMLOutput.i &NEW=NEW &cXMLSysCtrl=cXMLASN &Company=cocode &c=c} /* rstark 05291402 */

FIND FIRST sys-ctrl WHERE
     sys-ctrl.company EQ cocode AND
     sys-ctrl.name    EQ 'EDIBOLPost'
     NO-LOCK NO-ERROR.

IF AVAIL sys-ctrl THEN
   ASSIGN
      v-EDIBOLPost-log = sys-ctrl.log-fld
      v-EDIBOLPost-char = sys-ctrl.char-fld.

/* Buffers */
DEF NEW SHARED BUFFER xoe-ord       FOR oe-ord.
DEFINE BUFFER         bf-oe-boll    FOR oe-boll.
DEFINE BUFFER         b1-cust       FOR cust.
DEFINE BUFFER         b1-oe-bolh    FOR oe-bolh.
DEFINE BUFFER         b1-oe-boll    FOR oe-boll.
DEFINE BUFFER         b1-shipto     FOR shipto.
DEFINE BUFFER         b-oe-bolh     FOR oe-bolh.
DEFINE BUFFER         b-cust        FOR cust.

DEF STREAM barcode.

DEF TEMP-TABLE tt-post NO-UNDO 
    FIELD row-id AS ROWID.

DEF TEMP-TABLE tt-packslip NO-UNDO
    FIELD b-no AS INT.


def var v-print-fmt     as char NO-UNDO.
DEF VAR v-print-fmt-int AS INT  NO-UNDO.
def var v-headers       as log  no-undo.
def var v-print-coc     as log  no-undo.
def var v-check-qty     as log  no-undo.
DEF VAR v-program       AS CHAR NO-UNDO.
DEF VAR is-xprint-form  AS LOG  NO-UNDO.
DEF VAR ls-fax-file     AS CHAR NO-UNDO.
DEF VAR lv-pdf-file     AS CHAR NO-UNDO.
DEF VAR vcBOLNums       AS CHAR NO-UNDO.
DEF VAR vcMailMode      AS CHAR NO-UNDO.
DEF VAR vcDefaultForm   AS CHAR NO-UNDO.
DEF VAR vcDefaultBOLX   AS CHAR NO-UNDO.
DEF VAR v-def-coc-fmt   AS CHAR NO-UNDO.

DEF VAR v-rtn-char      AS CHAR NO-UNDO.
DEF VAR v-rec-found     AS LOG  NO-UNDO.
DEF VAR invstatus-char  AS CHAR NO-UNDO.
DEF VAR invstatus-log   AS LOG  NO-UNDO.
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
DEFINE VARIABLE llBlockPost AS LOG NO-UNDO.
DEFINE VARIABLE cPgmList AS CHARACTER   NO-UNDO.
{custom/xprint.i}
DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VAR v-print-unassembled AS LOG NO-UNDO.
 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.


DEF VAR lv-prt-bypass     AS LOG NO-UNDO.  /* bypass window's printer driver */
DEF VAR lv-run-bol        AS char no-undo.
DEF VAR lv-run-commercial AS char no-undo.
DEF VAR v-packslip AS CHAR FORMAT "X(100)" NO-UNDO.

DEF VAR td-pck-lst AS LOG INIT NO NO-UNDO.
DEFINE VARIABLE d-print-fmt-dec  AS DECIMAL NO-UNDO.

/* gdm - 07240906 */
DEF VAR v-tglflg   AS LOG NO-UNDO INIT YES.
DEF NEW SHARED VAR v-ship-inst AS LOG NO-UNDO.
/* Build a Table to keep sequence of pdf files */
DEF NEW SHARED TEMP-TABLE tt-filelist NO-UNDO
                    FIELD tt-FileCtr    AS INT
                    FIELD tt-FileName   AS CHAR
                    INDEX filelist      IS PRIMARY 
                          TT-FILECTR.

DEF TEMP-TABLE tt-ci-form NO-UNDO
    FIELD form-name AS CHAR
    FIELD total-pallets LIKE oe-bolh.tot-pallets
    INDEX tt-ci-form form-name ASC.

def NEW SHARED TEMP-TABLE w-comm-bol NO-UNDO
    field bol-no as INT
    INDEX bol-no bol-no.

/* Output selection for the report */
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.

FIND FIRST sys-ctrl WHERE
     sys-ctrl.company EQ gcompany AND
     sys-ctrl.name    EQ "PACKSLIP"
     NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN
   DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = gcompany
         sys-ctrl.name     = "PACKSLIP"
         sys-ctrl.descrip  = "C:\BA\Packslip\".
   END.

v-packslip = sys-ctrl.descrip.

IF v-packslip = "" THEN
   v-packslip = "c:~\ba~\label~\packslip.txt".
ELSE do:
   IF NOT(SUBSTRING(v-packslip,LENGTH(v-packslip),1) = "/" OR
      SUBSTRING(v-packslip,LENGTH(v-packslip),1) = "\") THEN
      v-packslip = v-packslip + "/".

   v-packslip = v-packslip + "packslip.txt".
END.

DO TRANSACTION:
   {sys/inc/asnsps.i} 
END.

RELEASE sys-ctrl.


/* Invstatus to determine invoice status when created  */
RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "L", no, no, "", "", 
                      Output v-rtn-char, output v-rec-found).
invstatus-log = LOGICAL(v-rtn-char).
/* Invstatus to determine invoice status when created  */
RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "C", no, no, "", "", 
                      Output invstatus-char, output v-rec-found).


DEF TEMP-TABLE tt-email NO-UNDO
      FIELD tt-recid AS RECID
      FIELD bol-no LIKE oe-boll.bol-no
      FIELD ord-no LIKE oe-boll.ord-no
      FIELD i-no LIKE itemfg.i-no
      FIELD qty AS INT
      FIELD cust-no AS cha
      INDEX tt-cust IS PRIMARY cust-no DESCENDING .

DEF STREAM st-email.

{oe/EDIRelBL.i}
DEFINE STREAM ediBOL.

DEFINE TEMP-TABLE ediOutFile NO-UNDO
  FIELD custNo AS CHAR
  FIELD poNo AS CHAR
  FIELD poLine AS INT
  FIELD partNo AS CHAR
  FIELD qty AS DEC
  FIELD lotNo AS CHAR
  FIELD bolDate AS DATE
  FIELD relNo AS INT
  FIELD carrier AS CHAR
  FIELD trailer AS CHAR
  FIELD bolNo AS INT
    INDEX ediOutFile IS PRIMARY custNo bolNo carrier trailer.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 begin_cust end_cust begin_bol# ~
begin_ord# end_ord# begin_date end_date tb_reprint tb_pallet tb_posted ~
tb_print-component tb_print-shipnote tb_barcode tb_print_ship ~
tb_print-barcode tb_print-unassemble-component tb_print-binstags ~
rd_bol-sort fi_specs tb_print-spec rd_bolcert tb_per-bol-line ~
tb_EMailAdvNotice rd-dest tb_MailBatchMode tb_ComInvoice tb_freight-bill ~
tb_footer lv-ornt lines-per-page lv-font-no tb_post-bol td-show-parm btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_bol# begin_ord# ~
end_ord# begin_date end_date tb_reprint tb_pallet tb_posted ~
tb_print-component tb_print-shipnote tb_barcode tb_print_ship ~
tb_print-barcode tb_print-unassemble-component tb_print-binstags ~
lbl_bolsort rd_bol-sort fi_specs tb_print-spec lbl_bolcert rd_bolcert ~
tb_per-bol-line tb_EMailAdvNotice rd-dest tb_MailBatchMode tb_ComInvoice ~
tb_freight-bill tb_footer lv-ornt lines-per-page lv-font-no lv-font-name ~
tb_post-bol td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
  ( ipField AS CHARACTER )  FORWARD.

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

DEFINE VARIABLE begin_bol# AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Beginning BOL#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_ord# AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_bol# AS INTEGER FORMAT ">>>>>>>9" INITIAL 99999999 
     LABEL "Ending BOL#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord# AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_depts AS CHARACTER FORMAT "X(100)" 
     VIEW-AS FILL-IN 
     SIZE 38.4 BY 1.

DEFINE VARIABLE fi_specs AS CHARACTER FORMAT "X(100)" 
     VIEW-AS FILL-IN 
     SIZE 38.4 BY 1.

DEFINE VARIABLE lbl_bolcert AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_bolsort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
     VIEW-AS FILL-IN 
     SIZE 9.4 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 47.6 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
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
     SIZE 23 BY 7.86 NO-UNDO.

DEFINE VARIABLE rd_bol-sort AS CHARACTER INITIAL "Item #" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item #", "Item #",
"Job #", "Job #"
     SIZE 24.4 BY 1 NO-UNDO.

DEFINE VARIABLE rd_bolcert AS CHARACTER INITIAL "BOL" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "BOL", "BOL",
"Certificate of Compliance", "Certificate of Compliance"
     SIZE 39 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 9.52.

DEFINE VARIABLE tb_barcode AS LOGICAL INITIAL no 
     LABEL "Print Bar Coded Pack List?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tb_ComInvoice AS LOGICAL INITIAL no 
     LABEL "Commercial Invoice (Excel)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE tb_EMailAdvNotice AS LOGICAL INITIAL no 
     LABEL "E-Mail &Advanced Ship Notice?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tb_footer AS LOGICAL INITIAL no 
     LABEL "Print Footer?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE tb_freight-bill AS LOGICAL INITIAL no 
     LABEL "Print Freight Bill / Logo?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE tb_MailBatchMode AS LOGICAL INITIAL no 
     LABEL "Hide E-Mail Dialog Box" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE tb_pallet AS LOGICAL INITIAL no 
     LABEL "Print Number Of Pallets?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tb_per-bol-line AS LOGICAL INITIAL no 
     LABEL "Per BOL Line" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_post-bol AS LOGICAL INITIAL no 
     LABEL "Post BOL?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_posted AS LOGICAL INITIAL no 
     LABEL "Reprint Posted BOL?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-barcode AS LOGICAL INITIAL no 
     LABEL "Print Barcode by Part Number?" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-binstags AS LOGICAL INITIAL no 
     LABEL "Print Bins/Tags?" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-component AS LOGICAL INITIAL no 
     LABEL "Print Assembled Components?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-dept AS LOGICAL INITIAL no 
     LABEL "Print Dept Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-shipnote AS LOGICAL INITIAL no 
     LABEL "Print Ship Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-spec AS LOGICAL INITIAL no 
     LABEL "Print Spec Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-unassemble-component AS LOGICAL INITIAL no 
     LABEL "Print Unassembled Set?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print_ship AS LOGICAL INITIAL no 
     LABEL "Print Shipping Inst?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tb_reprint AS LOGICAL INITIAL no 
     LABEL "Reprint Bill Of Ladings?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust AT ROW 1.95 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 1.95 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_bol# AT ROW 2.91 COL 26 COLON-ALIGNED HELP
          "Enter Beginning BOL Number"
     end_bol# AT ROW 2.91 COL 69 COLON-ALIGNED HELP
          "Enter Ending BOL Number"
     begin_ord# AT ROW 3.86 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord# AT ROW 3.86 COL 69 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_date AT ROW 4.81 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 4.81 COL 69.2 COLON-ALIGNED HELP
          "Enter Ending Date"
     tb_reprint AT ROW 5.91 COL 34
     tb_pallet AT ROW 6.86 COL 62 RIGHT-ALIGNED
     tb_posted AT ROW 7.81 COL 34
     tb_print-component AT ROW 8.76 COL 34
     tb_print-shipnote AT ROW 9.71 COL 34
     tb_barcode AT ROW 10.57 COL 34
     fi_depts AT ROW 11.33 COL 53.6 COLON-ALIGNED HELP
          "Enter Dept Codes separated by commas" NO-LABEL
     tb_print-dept AT ROW 11.43 COL 34
     tb_print_ship AT ROW 11.43 COL 34 WIDGET-ID 4
     tb_print-barcode AT ROW 12.29 COL 34 WIDGET-ID 2
     tb_print-unassemble-component AT ROW 13.1 COL 34 WIDGET-ID 18
     tb_print-binstags AT ROW 13.24 COL 34 WIDGET-ID 6
     lbl_bolsort AT ROW 13.86 COL 21.6 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     rd_bol-sort AT ROW 13.86 COL 34.6 NO-LABEL WIDGET-ID 14
     fi_specs AT ROW 14 COL 53.6 COLON-ALIGNED HELP
          "Enter Dept Codes separated by commas" NO-LABEL WIDGET-ID 8
     tb_print-spec AT ROW 14.1 COL 34 WIDGET-ID 10
     lbl_bolcert AT ROW 15.1 COL 24 COLON-ALIGNED NO-LABEL
     rd_bolcert AT ROW 15.1 COL 34.6 NO-LABEL
     tb_per-bol-line AT ROW 15.19 COL 73.4 WIDGET-ID 20
     tb_EMailAdvNotice AT ROW 16.67 COL 67 RIGHT-ALIGNED
     rd-dest AT ROW 17.48 COL 5 NO-LABEL
     tb_MailBatchMode AT ROW 17.57 COL 59 RIGHT-ALIGNED
     tb_ComInvoice AT ROW 18.48 COL 64 RIGHT-ALIGNED
     tb_freight-bill AT ROW 19.38 COL 61 RIGHT-ALIGNED
     tb_footer AT ROW 20.29 COL 61 RIGHT-ALIGNED
     lv-ornt AT ROW 21.1 COL 34 NO-LABEL
     lines-per-page AT ROW 21.1 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 22.62 COL 32 COLON-ALIGNED
     lv-font-name AT ROW 22.62 COL 39.4 COLON-ALIGNED NO-LABEL
     tb_post-bol AT ROW 24.1 COL 69.8
     td-show-parm AT ROW 24.14 COL 34
     btn-ok AT ROW 26 COL 20
     btn-cancel AT ROW 26 COL 61
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 2
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 16.52 COL 5
     RECT-6 AT ROW 16.29 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 26.91.


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
         TITLE              = "Print Bills of Lading"
         HEIGHT             = 27.14
         WIDTH              = 95.8
         MAX-HEIGHT         = 53.71
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 53.71
         VIRTUAL-WIDTH      = 384
         RESIZE             = no
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
       begin_bol#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

/* SETTINGS FOR FILL-IN end_bol# IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       end_bol#:HIDDEN IN FRAME FRAME-A           = TRUE
       end_bol#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fi_depts IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_depts:HIDDEN IN FRAME FRAME-A           = TRUE
       fi_depts:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_specs:HIDDEN IN FRAME FRAME-A           = TRUE
       fi_specs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_bolcert IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_bolcert:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_bolcert".

/* SETTINGS FOR FILL-IN lbl_bolsort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_bolsort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_bolcert".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lv-font-name:READ-ONLY IN FRAME FRAME-A        = TRUE.

ASSIGN 
       rd_bol-sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_bolcert:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_barcode:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_ComInvoice IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_ComInvoice:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_EMailAdvNotice IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_EMailAdvNotice:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_footer IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_footer:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_freight-bill IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_freight-bill:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_MailBatchMode IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_MailBatchMode:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_pallet IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_pallet:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_per-bol-line:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_post-bol:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_posted:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print-barcode:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print-binstags:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_print-binstags:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print-component:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_print-dept IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_print-dept:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_print-dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print-shipnote:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print-spec:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_print-spec:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print-unassemble-component:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print_ship:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_reprint:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Print Bills of Lading */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Print Bills of Lading */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol# C-Win
ON VALUE-CHANGED OF begin_bol# IN FRAME FRAME-A /* Beginning BOL# */
DO:
  RUN new-bol#.
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
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
DO:
  if date(end_date:SCREEN-VALUE) eq end_date THEN
    end_date:SCREEN-VALUE = begin_date:SCREEN-VALUE.

  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord# C-Win
ON LEAVE OF begin_ord# IN FRAME FRAME-A /* Beginning Order# */
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
   DEF VAR retCode     AS INT NO-UNDO.  
   DEF VAR ll          AS LOG NO-UNDO.
   DEF VAR v-format-str AS CHAR NO-UNDO.
   DEF VAR lv-exception AS LOG NO-UNDO.
   /* Initilize temp-table */
   EMPTY TEMP-TABLE tt-filelist.
   EMPTY TEMP-TABLE tt-post.

   IF tb_barcode:CHECKED THEN
      EMPTY TEMP-TABLE tt-packslip.

   DO WITH FRAME {&FRAME-NAME}:
     ASSIGN {&displayed-objects}.
   END.
   IF begin_date:SCREEN-VALUE EQ "" OR end_date:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE "Release date may not be left blank..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO begin_date.
      RETURN NO-APPLY.
  END.

   IF invstatus-char EQ "One Bol Only" THEN
       ASSIGN END_bol# = begin_bol#
              END_bol#:SCREEN-VALUE = begin_bol#:SCREEN-VALUE.
   ELSE
       ASSIGN END_bol#.

   IF rd_bolcert EQ "BOL" THEN
   DO:
      IF NOT tb_freight-bill THEN
         v-format-str = "BOLFMT".
      ELSE
      DO:
         v-format-str = "BOLFMTX".
         RUN oerep/r-bolx.w.
      END.
   END.
   ELSE
      v-format-str = "BOLCERT".


   CASE rd-dest:
      WHEN 1 THEN ASSIGN LvOutputSelection = "Printer".
      WHEN 2 THEN ASSIGN LvOutputSelection = "Screen". 
      WHEN 3 THEN ASSIGN LvOutputSelection = "File". 
      WHEN 4 THEN ASSIGN LvOutputSelection = "Fax". 
      WHEN 5 THEN ASSIGN LvOutputSelection = "Email".
      WHEN 6 THEN ASSIGN LvOutputSelection = "Port".
   END CASE.

   IF NOT rd-dest = 5 THEN
   DO:
      IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
         sys-ctrl-shipto.company = cocode AND
         sys-ctrl-shipto.NAME = v-format-str) THEN
         DO:
            IF CAN-FIND(FIRST b-oe-bolh WHERE
               b-oe-bolh.company EQ cocode AND
               b-oe-bolh.bol-no  GE begin_bol# AND
               b-oe-bolh.bol-no  LE end_bol# AND
               b-oe-bolh.cust-no GE begin_cust AND
               b-oe-bolh.cust-no LE end_cust AND
               b-oe-bolh.bol-date GE begin_date AND
               b-oe-bolh.bol-date LE end_date AND
               b-oe-bolh.printed EQ tb_reprint AND
               b-oe-bolh.posted  EQ tb_posted AND
               CAN-FIND (FIRST oe-boll
                         WHERE oe-boll.company EQ b-oe-bolh.company
                           AND oe-boll.b-no    EQ b-oe-bolh.b-no
                           AND oe-boll.ord-no  GE begin_ord#
                           AND oe-boll.ord-no  LE end_ord#)) THEN
               FOR EACH b-oe-bolh WHERE
                   b-oe-bolh.company EQ cocode AND
                   b-oe-bolh.bol-no  GE begin_bol# AND
                   b-oe-bolh.bol-no  LE end_bol# AND
                   b-oe-bolh.cust-no GE begin_cust AND
                   b-oe-bolh.cust-no LE end_cust AND
                   b-oe-bolh.bol-date GE begin_date AND
                   b-oe-bolh.bol-date LE end_date AND
                   b-oe-bolh.printed EQ tb_reprint AND
                   b-oe-bolh.posted  EQ tb_posted AND
                   CAN-FIND (FIRST oe-boll
                             WHERE oe-boll.company EQ b-oe-bolh.company
                               AND oe-boll.b-no    EQ b-oe-bolh.b-no
                               AND oe-boll.ord-no  GE begin_ord#
                               AND oe-boll.ord-no  LE end_ord#)
                   NO-LOCK
                   BREAK BY b-oe-bolh.company
                         BY b-oe-bolh.cust-no:

                   IF FIRST-OF(b-oe-bolh.cust-no) THEN
                   DO:
                      FIND FIRST sys-ctrl-shipto WHERE
                           sys-ctrl-shipto.company = cocode AND
                           sys-ctrl-shipto.NAME = v-format-str AND
                           sys-ctrl-shipto.cust-vend = YES AND
                           sys-ctrl-shipto.cust-vend-no = b-oe-bolh.cust-no AND
                           sys-ctrl-shipto.ship-id      = b-oe-bolh.ship-id AND
                           sys-ctrl-shipto.char-fld > ''
                           NO-LOCK NO-ERROR.

                      IF NOT AVAIL sys-ctrl-shipto THEN
                         FIND FIRST sys-ctrl-shipto WHERE
                           sys-ctrl-shipto.company = cocode AND
                           sys-ctrl-shipto.NAME = v-format-str AND
                           sys-ctrl-shipto.cust-vend = YES AND
                           sys-ctrl-shipto.cust-vend-no = b-oe-bolh.cust-no AND
                           sys-ctrl-shipto.ship-id EQ '' AND
                           sys-ctrl-shipto.char-fld > ''
                           NO-LOCK NO-ERROR.

                      IF AVAIL sys-ctrl-shipto THEN
                      DO:
                         RUN SetBolForm(sys-ctrl-shipto.char-fld).
                         v-print-fmt = sys-ctrl-shipto.char-fld.
                         d-print-fmt-dec = sys-ctrl-shipto.dec-fld.
                            
                      END.
                      ELSE
                      DO:
                         IF rd_bolcert EQ "BOL" THEN
                         DO:
                            IF NOT tb_freight-bill THEN
                            DO:
                               RUN SetBolForm (vcDefaultForm).
                               v-print-fmt = vcDefaultForm.
                            END.
                            ELSE
                            DO:
                               RUN SetBolForm (vcDefaultBOLX).
                               v-print-fmt = vcDefaultBOLX.
                            END.
                         END.
                         ELSE
                         DO:
                            RUN SetBolForm (v-def-coc-fmt).
                            v-print-fmt = v-def-coc-fmt.
                         END.
                      END.

                      RUN SetVariables.
                      RUN run-report(b-oe-bolh.cust-no,YES).
                      RUN GenerateReport(b-oe-bolh.cust-no,YES).

                      IF v-print-fmt EQ "Badger"  OR v-print-fmt EQ "BadgerSoldTo" THEN DO:
                          IF begin_bol#:SCREEN-VALUE EQ end_bol#:SCREEN-VALUE THEN DO:

                              MESSAGE " Do you want to print a Packing List?  "  
                                  VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO 
                                  UPDATE td-pck-lst . 

                              IF td-pck-lst EQ YES THEN do:
                                  RUN run-packing-list(b-oe-bolh.cust-no,YES) .
                                  RUN GenerateReport(b-oe-bolh.cust-no,YES).
                              END.
                          END.
                     END. /* IF v-print-fmt EQ "Badger"  OR v-print-fmt EQ "BadgerSoldTo" THEN */

                   END. /*first-of*/
               END. /*each b-oe-bolh*/
         END.
      ELSE /*not find first sys-ctrl-shipto*/
      DO:
         IF rd_bolcert EQ "BOL" THEN
         DO:
            IF NOT tb_freight-bill THEN
               v-print-fmt = vcDefaultForm.
            ELSE
               v-print-fmt = vcDefaultBOLX.
         END.
         ELSE
            v-print-fmt = v-def-coc-fmt.

         RUN SetBolForm(v-print-fmt).
         RUN SetVariables.
         RUN run-report("",NO).
         RUN GenerateReport(begin_cust,NO).

         IF v-print-fmt EQ "Badger"  OR v-print-fmt EQ "BadgerSoldTo" THEN DO:
               IF begin_bol#:SCREEN-VALUE EQ end_bol#:SCREEN-VALUE THEN DO:

                   MESSAGE " Do you want to print a Packing List?  "  
                       VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO 
                       UPDATE td-pck-lst . 

                   IF td-pck-lst EQ YES THEN do:
                       RUN run-packing-list("",NO) .
                       RUN GenerateReport(begin_cust,NO).
                   END.
               END.
          END. /* IF v-print-fmt EQ "Badger"  OR v-print-fmt EQ "BadgerSoldTo" THEN */
      END.

      if tb_EmailAdvNotice:CHECKED IN FRAME {&frame-name} AND
         tb_MailBatchMode then
         MESSAGE "Your E-Mails have been sent in Silent Mode."  skip
                 "Please verify transmission in your SENT folder."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END. /*not rd-dest eq 5*/
   ELSE /*rd-dest eq 5*/
   DO:
      IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
         sys-ctrl-shipto.company = cocode AND
         sys-ctrl-shipto.NAME = v-format-str) THEN
         DO:
            IF CAN-FIND(FIRST b-oe-bolh WHERE
               b-oe-bolh.company eq cocode AND
               b-oe-bolh.bol-no  ge begin_bol# AND
               b-oe-bolh.bol-no  le end_bol# AND
               b-oe-bolh.cust-no GE begin_cust AND
               b-oe-bolh.cust-no LE end_cust AND
               b-oe-bolh.bol-date GE begin_date AND
               b-oe-bolh.bol-date LE end_date AND
               b-oe-bolh.printed eq tb_reprint AND
               b-oe-bolh.posted  eq tb_posted AND
               can-find (FIRST b1-oe-boll
                         WHERE b1-oe-boll.company EQ b-oe-bolh.company AND
                               b1-oe-boll.b-no    EQ b-oe-bolh.b-no AND
                               b1-oe-boll.ord-no  GE begin_ord# AND
                               b1-oe-boll.ord-no  LE end_ord#)) THEN
               FOR EACH b-oe-bolh WHERE
                   b-oe-bolh.company eq cocode AND
                   b-oe-bolh.bol-no  ge begin_bol# AND
                   b-oe-bolh.bol-no  le end_bol# AND
                   b-oe-bolh.cust-no GE begin_cust AND
                   b-oe-bolh.cust-no LE end_cust AND
                   b-oe-bolh.bol-date GE begin_date AND
                   b-oe-bolh.bol-date LE end_date AND
                   b-oe-bolh.printed eq tb_reprint AND
                   b-oe-bolh.posted  eq tb_posted AND
                   can-find (FIRST b1-oe-boll WHERE
                                   b1-oe-boll.company EQ b-oe-bolh.company AND
                                   b1-oe-boll.b-no    EQ b-oe-bolh.b-no AND
                                   b1-oe-boll.ord-no  GE begin_ord# AND
                                   b1-oe-boll.ord-no  LE end_ord#)
                   NO-LOCK
                   USE-INDEX post
                   BREAK BY b-oe-bolh.company
                         BY b-oe-bolh.cust-no:

                   IF FIRST-OF(b-oe-bolh.cust-no) THEN
                   DO:
                      FIND FIRST sys-ctrl-shipto WHERE
                           sys-ctrl-shipto.company = cocode AND
                           sys-ctrl-shipto.NAME = v-format-str AND
                           sys-ctrl-shipto.cust-vend = YES AND
                           sys-ctrl-shipto.cust-vend-no = b-oe-bolh.cust-no AND
                           sys-ctrl-shipto.ship-id = b-oe-bolh.ship-id AND
                           sys-ctrl-shipto.char-fld > ''
                           NO-LOCK NO-ERROR.

                      IF NOT AVAIL sys-ctrl-shipto THEN
                         FIND FIRST sys-ctrl-shipto WHERE
                              sys-ctrl-shipto.company = cocode AND
                              sys-ctrl-shipto.NAME = v-format-str AND
                              sys-ctrl-shipto.cust-vend = YES AND
                              sys-ctrl-shipto.cust-vend-no = b-oe-bolh.cust-no AND
                              sys-ctrl-shipto.char-fld > ''
                              NO-LOCK NO-ERROR.

                      IF AVAIL sys-ctrl-shipto THEN
                      DO:
                         RUN SetBolForm(sys-ctrl-shipto.char-fld).
                         v-print-fmt = sys-ctrl-shipto.char-fld.
                         d-print-fmt-dec = sys-ctrl-shipto.dec-fld.
                      END.
                      ELSE
                      DO:
                         IF rd_bolcert EQ "BOL" THEN
                         DO:
                            IF NOT tb_freight-bill THEN
                            DO:
                               RUN SetBolForm (vcDefaultForm).
                               v-print-fmt = vcDefaultForm.
                            END.
                            ELSE
                            DO:
                               RUN SetBolForm (vcDefaultBOLX).
                               v-print-fmt = vcDefaultBOLX.
                            END.
                         END.
                         ELSE
                         DO:
                            RUN SetBolForm (v-def-coc-fmt).
                            v-print-fmt = v-def-coc-fmt.
                         END.
                      END.

                      RUN SetVariables.
                      RUN output-to-mail(INPUT b-oe-bolh.cust-no,YES,b-oe-bolh.ship-id).
                   END.
               END. /*end for each*/
         END. /*sys-ctrl-shipto found*/
      ELSE /*no sys-ctrl-shipto found*/
      DO:
         IF rd_bolcert EQ "BOL" THEN
         DO:
            IF NOT tb_freight-bill THEN
               v-print-fmt = vcDefaultForm.
            ELSE
               v-print-fmt = vcDefaultBOLX.
         END.
         ELSE
            v-print-fmt = v-def-coc-fmt.

         RUN SetBOLForm(v-print-fmt).
         RUN SetVariables.
         RUN output-to-mail(INPUT "",NO,"").
      END.

      if tb_MailBatchMode then
         MESSAGE "Your E-Mails have been sent in Silent Mode."  skip
                 "Please verify transmission in your SENT folder."
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.

   SESSION:SET-WAIT-STATE ("").

   IF tb_barcode:CHECKED THEN
      RUN barcode-proc.

   IF lv-run-commercial = "YES" AND NOT IS-xprint-form THEN 
      RUN run-report-ci.

   IF tb_ComInvoice:CHECKED THEN
      RUN CommercialInvoice.

   EMPTY TEMP-TABLE tt-fg-bin.
   EMPTY TEMP-TABLE tt-email.

   ll = tb_post-bol AND NOT tb_posted.

   IF ll AND oe-ctrl.u-inv AND v-check-qty THEN
      FOR EACH tt-post,
          FIRST oe-bolh NO-LOCK 
          WHERE ROWID(oe-bolh) EQ tt-post.row-id:

          RUN oe/bolcheck.p (ROWID(oe-bolh)).
          FIND FIRST w-except WHERE w-except.bol-no EQ oe-bolh.bol-no
                              NO-LOCK NO-ERROR.
          IF AVAIL(w-except) THEN DO:

            MESSAGE "BOL has insufficient inventory and cannot be posted..."
                 VIEW-AS ALERT-BOX ERROR.

            lv-exception = YES.
            MESSAGE "  Bill(s) of Lading have been found that do not have  "     skip
                    "  sufficient inventory for posting to be completed.   "     skip
                    "  Do you wish to print the exception report?          "
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-exception.
            IF lv-exception THEN do:
              run exception-rpt.
              case rd-dest:
                when 1 then run output-exception-printer.
                when 2 then run output-exception-screen.
                when 3 then run output-exception-file.
              end case.
            END.

            ll = NO.
            LEAVE.
          END.
   END.

   IF ll THEN DO:
       FOR EACH tt-post,
           FIRST oe-bolh NO-LOCK 
           WHERE ROWID(oe-bolh) EQ tt-post.row-id,
           EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company
                          AND oe-boll.bol-no  EQ oe-bolh.bol-no: 

          IF oe-bolh.stat = "H" THEN DO:  
            MESSAGE "BOL is on HOLD status, cannot post..."
                    VIEW-AS ALERT-BOX ERROR.
            ll = NO.
            LEAVE.
          END.

          /* 04301302 - If customer 'x' and shipto = shipfrom, don't post */
          FIND cust 
            WHERE cust.company EQ oe-bolh.company
              AND cust.cust-no EQ oe-bolh.cust-no 
            NO-LOCK NO-ERROR.

          IF AVAIL(cust) AND cust.ACTIVE EQ "X"
              AND oe-bolh.ship-id = oe-boll.loc THEN DO:
            MESSAGE "Cannot transfer to the same location..."
                    VIEW-AS ALERT-BOX ERROR.
            ll = NO.
            LEAVE.
          END.

       END.
   END.



   IF ll THEN DO:
      ll = NO.
      MESSAGE "Post BOL?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE ll.
   END.

   IF ll THEN do:
      RUN post-bol.
      FIND FIRST tt-email NO-LOCK NO-ERROR.
      IF AVAIL tt-email THEN RUN email-reorderitems.

      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_bol#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bol# C-Win
ON VALUE-CHANGED OF end_bol# IN FRAME FRAME-A /* Ending BOL# */
DO:
  RUN new-bol#.
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
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
  IF LASTKEY NE -1 THEN do:
    IF end_date:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE "Ending Date may not be left blank..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO {&self-name}.
      RETURN NO-APPLY.
    END.

    assign {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord# C-Win
ON LEAVE OF end_ord# IN FRAME FRAME-A /* Ending Order# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_depts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_depts C-Win
ON LEAVE OF fi_depts IN FRAME FRAME-A
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_specs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_specs C-Win
ON LEAVE OF fi_specs IN FRAME FRAME-A
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
    DEF VAR char-val AS char no-undo.

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
  if int (self:screen-value) = 5 then do:
    assign tb_MailBatchMode:sensitive  = true.
    apply 'value-changed':u to tb_MailBatchMode.
  end.

  ELSE DO:

    assign tb_MailBatchMode:sensitive  = false.

    APPLY 'VALUE-CHANGED':U TO tb_EmailAdvNotice.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_bol-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_bol-sort C-Win
ON VALUE-CHANGED OF rd_bol-sort IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_bolcert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_bolcert C-Win
ON VALUE-CHANGED OF rd_bolcert IN FRAME FRAME-A
DO:
  assign {&self-name}.
  IF rd_bolcert EQ "BOL" THEN
            tb_per-bol-line:SENSITIVE = NO.
        ELSE tb_per-bol-line:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_EMailAdvNotice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_EMailAdvNotice C-Win
ON VALUE-CHANGED OF tb_EMailAdvNotice IN FRAME FRAME-A /* E-Mail Advanced Ship Notice? */
DO:

  IF NOT rd-dest:SCREEN-VALUE EQ '5' THEN DO:

    IF SELF:CHECKED THEN DO:
      ASSIGN tb_MailBatchMode:sensitive  = true.
      APPLY 'value-changed':u to tb_MailBatchMode.
    end.

    ELSE
      assign  tb_MailBatchMode:sensitive  = false.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_pallet C-Win
ON VALUE-CHANGED OF tb_pallet IN FRAME FRAME-A /* Print Number Of Pallets? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_post-bol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_post-bol C-Win
ON VALUE-CHANGED OF tb_post-bol IN FRAME FRAME-A /* Post BOL? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_posted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_posted C-Win
ON VALUE-CHANGED OF tb_posted IN FRAME FRAME-A /* Reprint Posted BOL? */
DO:
  assign {&self-name}.
  IF tb_posted THEN do:
     ASSIGN tb_reprint = YES
            END_bol#:SCREEN-VALUE = begin_bol#:SCREEN-VALUE
          /*  END_ord#:SCREEN-VALUE = begin_ord#:SCREEN-VALUE
            END_cust:SCREEN-VALUE = begin_cust:SCREEN-VALUE*/.
     DISP tb_reprint     WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint C-Win
ON VALUE-CHANGED OF tb_reprint IN FRAME FRAME-A /* Reprint Bill Of Ladings? */
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
PROCEDURE mail EXTERNAL "xpMail.dll" :
      DEF INPUT PARAM mailTo AS CHAR.
      DEF INPUT PARAM mailsubject AS CHAR.
      DEF INPUT PARAM mailText AS CHAR.
      DEF INPUT PARAM mailFiles AS CHAR.
      DEF INPUT PARAM mailDialog AS LONG.
      DEF OUTPUT PARAM retCode AS LONG.
END.

{sys/inc/f3helpw.i}
DEF VAR choice AS LOG NO-UNDO.

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

  llBlockPost = FALSE.

  cPgmList = PROGRAM-NAME(1) 
        + (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
        + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
        + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
        + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
        + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
        + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE "").
   IF index(cPgmList, "fgpstall") GT 0 THEN
        llBlockPost = TRUE.    


  find first company where company.company eq cocode no-lock no-error.

  find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

  find first sys-ctrl
       where sys-ctrl.company eq cocode
         and sys-ctrl.name    eq "BOLPRINT"
      no-lock no-error.

  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "BOLPRINT"
     sys-ctrl.descrip = "Print Bill of Lading Headers on Bill of Lading Form?".
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  end.
  v-print-hdgs = sys-ctrl.log-fld.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "BOLFMT"
      NO-LOCK no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "BOLFMT"
     sys-ctrl.descrip  = "Bill of lading format"
     sys-ctrl.char-fld = "ASI".
    message "System control record not found. Update BOL Print format"
    update sys-ctrl.char-fld.
  end.
  assign
   v-print-fmt = sys-ctrl.char-fld
   v-headers   = sys-ctrl.log-fld
   v-print-fmt-int = sys-ctrl.int-fld
   d-print-fmt-dec = sys-ctrl.dec-fld.

  find first sys-ctrl
       where sys-ctrl.company eq cocode
         and sys-ctrl.name    eq "BOLCERT"
      no-lock no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "BOLCERT"
     sys-ctrl.descrip  = "Print Certificate of Compliance forms?"
     sys-ctrl.log-fld  = no.

  end.
  assign
   v-print-coc = sys-ctrl.log-fld
   v-coc-fmt   = sys-ctrl.char-fld
   v-def-coc-fmt = sys-ctrl.char-fld.

  find first sys-ctrl
       where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "BOLPOST"
      no-lock no-error.

  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "BOLPOST"
     sys-ctrl.descrip = "Post BOL if BOL Qty > Bin Qty"
     choice           = yes.

    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE choice.

    if not choice then sys-ctrl.char-fld eq "Bin>Qty".
  end.
  v-check-qty = sys-ctrl.char-fld eq "Bin>Qty".

  IF NOT CAN-FIND (FIRST sys-ctrl 
                   WHERE sys-ctrl.company = cocode
                   AND sys-ctrl.NAME    = 'CINVOICE') THEN
  DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN sys-ctrl.company = cocode
             sys-ctrl.NAME    = 'CINVOICE'
             sys-ctrl.descrip = 'Commercial Invoice Forms'.
      RELEASE sys-ctrl.
  END.
  DO TRANSACTION:
     {sys/inc/fgreorder.i}
  END.

  FIND FIRST users WHERE
       users.user_id EQ USERID("ASI")
       NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     init-dir = users.user_program[2].
  ELSE
     init-dir = "c:\tmp".

  RUN SetBolForm(INPUT v-print-fmt).
  vcDefaultForm = v-print-fmt.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "BOLFMTX"
      NO-LOCK no-error.
  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "BOLFMTX"
     sys-ctrl.descrip  = "Freight Bill of lading Format"
     sys-ctrl.char-fld = "FIBREX".
  end.

  vcDefaultBOLX = sys-ctrl.char-fld.

  IF v-print-fmt EQ "XPRINT"   OR
     v-print-fmt EQ "bolfmt 1"   OR
     v-print-fmt EQ "bolfmt 10"   OR
      v-print-fmt EQ "Wingate-BOL"   OR
     v-print-fmt EQ "bolfmt10-CAN"   OR
     v-print-fmt EQ "Lakeside"   OR
     v-print-fmt EQ "ACCORDBC"   OR
     v-print-fmt EQ "Protagon" OR
     v-print-fmt = "CapCityIN" OR 
     v-print-fmt = "Axis" OR 
     v-print-fmt EQ "Allwest"  OR
     v-print-fmt EQ "PackRite"  OR
     v-print-fmt EQ "Badger"   OR
     v-print-fmt EQ "BadgerSoldTo"   OR
     v-print-fmt EQ "MidwestX"   OR
     v-print-fmt EQ "Loylang"  OR
     v-print-fmt EQ "Printers"  OR
     v-print-fmt EQ "Printers2"  OR
     v-print-fmt EQ "Multicell"
    THEN 
     ASSIGN
        tb_print-dept:HIDDEN = NO
        fi_depts:HIDDEN = NO
        tb_print-dept:SENSITIVE = YES
        fi_depts:SENSITIVE = YES.

  IF InvStatus-char NE "One BOL Only" THEN
      ASSIGN END_bol#:HIDDEN = NO
             END_bol#:SENSITIVE = YES.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:

    {custom/usrprint.i}

    APPLY "entry" TO begin_cust.

    lines-per-page:SCREEN-VALUE = STRING(v-lines-per-page).
    DISABLE lines-per-page.

    IF NOT PROGRAM-NAME(1) BEGINS "listobjs/oe-boll_." OR llBlockPost THEN
      ASSIGN
       tb_post-bol:SCREEN-VALUE = "no"
       tb_post-bol:HIDDEN       = YES.

    IF LOOKUP(v-print-fmt,"SouthPak,Xprint,bolfmt 1,bolfmt 10,Wingate-BOL,bolfmt10-CAN,Lakeside,Soule,SouleMed,Accordbc,Protagon,Delta,Xprint2,bolfmt 2,bolfmt 20,LancoYork,Chillicothe,NSTOCK,Frankstn,Fibre,Ottpkg,Consbox,CapitolBC,ContSrvc,CapCityIN,Axis,Allwest,COLOR,AllPkg2,Loylang,Printers,Printers2,PEACHTREE,PeachTreeBC,Multicell") LE 0 THEN DO:
      tb_print-component:SCREEN-VALUE = "no".
      DISABLE tb_print-component.
      tb_print-unassemble-component:SCREEN-VALUE = "no".
      DISABLE tb_print-unassemble-component.
      
    END.


    IF v-print-fmt = "Xprint"    or
       v-print-fmt = "bolfmt 1"    or
       v-print-fmt = "bolfmt 10"    or
        v-print-fmt = "Wingate-BOL"    or
       v-print-fmt = "bolfmt10-CAN"    or
       v-print-fmt = "Lakeside"    or
       v-print-fmt = "Accordbc"    or
       v-print-fmt = "Protagon"  or
       v-print-fmt = "CapCityIN" or 
       v-print-fmt = "Axis" or 
       v-print-fmt = "Peachtree" OR
       v-print-fmt = "PeachtreeBC" OR
       v-print-fmt = "MidwestX" or
       v-print-fmt = "Allwest"   or 
       v-print-fmt = "Badger"    OR
       v-print-fmt = "BadgerSoldTo"   OR
       v-print-fmt = "Loylang"   OR
       v-print-fmt EQ "Printers" OR
       v-print-fmt EQ "Printers2" OR
       v-print-fmt = "Multicell" OR 
       v-print-fmt = "SouleMed"  OR
       v-print-fmt = "Soule"
      THEN tb_print-shipnote:SENSITIVE = YES.
      ELSE tb_print-shipnote:SENSITIVE = NO.

    IF v-print-fmt <> "NSTOCK" THEN
        tb_print_ship:HIDDEN       = YES.
    ELSE
        tb_print_ship:HIDDEN       = NO .

    IF v-coc-fmt EQ "BOLCERT10" THEN DO:
        assign
            tb_per-bol-line:HIDDEN       = NO.
            tb_per-bol-line:screen-value = "NO".
            tb_per-bol-line:SENSITIVE = NO.
    END.
    ELSE DO:
        tb_per-bol-line:HIDDEN       = YES .
    END.

    IF v-print-fmt = "ACCORDBC" AND v-print-fmt-int = 1 THEN
      tb_print-binstags:HIDDEN = NO.
    ELSE
      tb_print-binstags:HIDDEN = YES.
    IF v-print-fmt = "Protagon" OR v-print-fmt = "Axis" THEN
      ASSIGN 
        tb_print-spec:HIDDEN = NO
        fi_specs:HIDDEN = NO.
    ELSE
      ASSIGN 
        tb_print-spec:HIDDEN = YES
        fi_specs:HIDDEN = YES.

    IF v-print-fmt = "bolfmt10-can" THEN
      ASSIGN 
        tb_print-unassemble-component:HIDDEN = NO.
    ELSE
      ASSIGN 
        tb_print-unassemble-component:HIDDEN = YES.

   IF v-print-fmt = "XPrint2" OR v-print-fmt = "Delta" OR v-print-fmt = "bolfmt 2" OR v-print-fmt = "bolfmt 20" OR v-print-fmt = "LancoYork" THEN  /* task 01121601 */
       ASSIGN
        lbl_bolsort:HIDDEN = NO
        rd_bol-sort:HIDDEN = NO .
   ELSE
       ASSIGN
        lbl_bolsort:HIDDEN = YES
        rd_bol-sort:HIDDEN = YES .

      IF v-print-fmt NE "PremierXFooter"    THEN
         ASSIGN  tb_footer:HIDDEN = YES
                 tb_footer:SCREEN-VALUE = "No" .

    RUN new-bol#.

    tb_EMailAdvNotice:SENSITIVE = YES.
    APPLY 'value-changed':u TO rd-dest.

  END.


  /* gdm - 07240906 */
  ASSIGN v-tglflg = YES.

  RUN check-bol-security (INPUT "r-bolpst.",
                          OUTPUT v-tglflg).

  IF v-tglflg THEN
     RUN check-bol-security (INPUT "postbol.",
                             OUTPUT v-tglflg).

  IF NOT v-tglflg OR llBlockPost THEN DO:

    ASSIGN tb_post-bol = NO
           tb_post-bol:HIDDEN IN FRAME {&FRAME-NAME} = YES.
  END.
  ELSE
     ASSIGN tb_post-bol = NO
            tb_post-bol:HIDDEN IN FRAME {&FRAME-NAME} = NO.
     /* gdm - 07240906 end */

   td-show-parm:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO" .
   td-show-parm:HIDDEN IN FRAME {&FRAME-NAME} = YES .

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AdvancedNotice C-Win 
PROCEDURE AdvancedNotice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-ship-to AS LOG NO-UNDO.

  DEFINE BUFFER b1-cust       FOR cust.
  DEFINE BUFFER b1-oe-bolh    FOR oe-bolh.
  DEFINE BUFFER b1-oe-boll    FOR oe-boll.
  DEFINE BUFFER b1-in-house-cust FOR cust.
  DEFINE BUFFER bl-phone      FOR phone.
  DEFINE BUFFER bl-emaildtl   FOR emaildtl.

  DEFINE VARIABLE vcMailMode AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vlSkipRec  AS LOGICAL    NO-UNDO.

  assign
    v-s-bol             = begin_bol#
    v-e-bol             = end_bol#
    v-s-ord             = begin_ord#
    v-e-ord             = end_ord#
    v-s-date             = begin_date
    v-e-date             = end_date
    v-printed           = tb_reprint
    v-print-pal         = tb_pallet
    v-print-bol         = rd_bolcert EQ "BOL"
    v-print-components  = tb_print-component
    v-print-shipnotes   = tb_print-shipnote
    lv-run-bol          = ""
    lv-run-commercial   = ""
    v-print-unassembled = tb_print-unassemble-component
    v-footer            = tb_footer.

  IF ip-sys-ctrl-ship-to THEN
     ASSIGN
        v-s-cust = ip-cust-no
        v-e-cust = ip-cust-no.
  ELSE
     ASSIGN 
        v-s-cust = begin_cust
        v-e-cust = end_cust.

  IF fi_depts:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        v-print-dept = LOGICAL(tb_print-dept:SCREEN-VALUE)
        v-depts = fi_depts:SCREEN-VALUE.

  FOR EACH b1-cust NO-LOCK
     WHERE b1-cust.company EQ cocode
       AND b1-cust.cust-no GE v-s-cust
       AND b1-cust.cust-no LE v-e-cust,

      EACH b1-shipto NO-LOCK OF b1-cust,

     FIRST b1-oe-bolh NO-LOCK
     where b1-oe-bolh.company eq cocode
       and b1-oe-bolh.bol-no  ge v-s-bol
       and b1-oe-bolh.bol-no  le v-e-bol
       and b1-oe-bolh.bol-date  ge v-s-date
       and b1-oe-bolh.bol-date  le v-e-date
       and b1-oe-bolh.cust-no EQ b1-cust.cust-no
       and b1-oe-bolh.printed eq YES
       and b1-oe-bolh.posted  eq tb_posted
       and can-find (FIRST b1-oe-boll
                     WHERE b1-oe-boll.company EQ b1-oe-bolh.company
                       AND b1-oe-boll.b-no    EQ b1-oe-bolh.b-no
                       AND b1-oe-boll.ord-no  GE v-s-ord
                       AND b1-oe-boll.ord-no  LE v-e-ord)
    USE-INDEX post
    BREAK BY b1-cust.cust-no
          BY b1-shipto.ship-id:

    IF FIRST-OF (b1-shipto.ship-id) THEN DO:

      STATUS DEFAULT 'Processing SHIPTO Contacts for: ' + b1-shipto.ship-id + '....'.

      ASSIGN
          vlSkipRec = YES
          vcBOLNums   = '' 
          lv-pdf-file = init-dir + '\BOL'
          vcMailMode  = if tb_MailBatchMode then 'ShipTo1'  /* Silent Mode */
                                            else 'ShipTo'.  /* Dialog Box */

      FOR EACH bl-phone WHERE
          bl-phone.table_rec_key EQ b1-shipto.rec_key
          NO-LOCK:

          IF CAN-FIND(FIRST bl-emaildtl WHERE
             bl-emaildtl.emailcod EQ 'r-bolprt.' AND
             bl-emaildtl.table_rec_key EQ bl-phone.rec_key) THEN
             DO:
                vlSkipRec = NO.
                LEAVE.
             END.
      END.

      IF NOT vlSkipRec THEN
         RUN GenerateMail (INPUT b1-cust.cust-no, 
                           INPUT b1-shipto.rec_key,
                           INPUT 2,
                           INPUT vcMailMode).
    END.
  END. /* each cust */

  /*in-house cust shiptos*/
  FOR EACH b1-cust NO-LOCK
     WHERE b1-cust.company EQ cocode
       AND b1-cust.cust-no GE v-s-cust
       AND b1-cust.cust-no LE v-e-cust,
     FIRST b1-oe-bolh NO-LOCK
     where b1-oe-bolh.company eq cocode
       and b1-oe-bolh.bol-no  ge v-s-bol
       and b1-oe-bolh.bol-no  le v-e-bol
       and b1-oe-bolh.cust-no EQ b1-cust.cust-no
       and b1-oe-bolh.bol-date  ge v-s-date
       and b1-oe-bolh.bol-date  le v-e-date
       and b1-oe-bolh.printed eq YES
       and b1-oe-bolh.posted  eq tb_posted
       and can-find (FIRST b1-oe-boll
                     WHERE b1-oe-boll.company EQ b1-oe-bolh.company
                       AND b1-oe-boll.b-no    EQ b1-oe-bolh.b-no
                       AND b1-oe-boll.ord-no  GE v-s-ord
                       AND b1-oe-boll.ord-no  LE v-e-ord)
      USE-INDEX post,
      EACH b1-in-house-cust WHERE
           b1-in-house-cust.company EQ cocode AND
           b1-in-house-cust.active  EQ "X"
           NO-LOCK,
      FIRST b1-shipto NO-LOCK WHERE
            b1-shipto.company EQ cocode AND
            b1-shipto.cust-no EQ b1-in-house-cust.cust-no AND
            b1-shipto.ship-id EQ b1-oe-bolh.ship-id
    BREAK BY b1-cust.cust-no
          BY b1-shipto.ship-id:

    IF FIRST-OF (b1-shipto.ship-id) THEN DO:

      STATUS DEFAULT 'Processing SHIPTO Contacts for: ' + b1-shipto.ship-id + '....'.

      ASSIGN
          vlSkipRec = YES
          vcBOLNums   = '' 
          lv-pdf-file = init-dir + '\BOL'
          vcMailMode  = if tb_MailBatchMode then 'ShipTo1'  /* Silent Mode */
                                            else 'ShipTo'.  /* Dialog Box */

      FOR EACH bl-phone WHERE
          bl-phone.table_rec_key EQ b1-shipto.rec_key
          NO-LOCK:

          IF CAN-FIND(FIRST bl-emaildtl WHERE
             bl-emaildtl.emailcod EQ 'r-bolprt.' AND
             bl-emaildtl.table_rec_key EQ bl-phone.rec_key) THEN
             DO:
                vlSkipRec = NO.
                LEAVE.
             END.
      END.

      IF NOT vlSkipRec THEN
         RUN GenerateMail (INPUT b1-cust.cust-no, 
                           INPUT b1-shipto.rec_key,
                           INPUT 2,
                           INPUT vcMailMode).
    END.
  END. /* each cust */

  status default 'Enter data or ESC to end.'.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ASIMail C-Win 
PROCEDURE ASIMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  def input param icMailMode  as char no-undo.
  def input param icCustNo    as char no-undo.
  def input param icSubBody   as char no-undo.

  {custom/asimail2.i  &TYPE           = value (icMailMode)
                      &group-title    = 'r-bolprt.'
                      &begin_cust     = icCustNo
                      &END_cust       = icCustNo
                      &mail-subject   = icSubBody
                      &mail-body      = icSubBody
                      &mail-file      = lv-pdf-file}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE barcode-proc C-Win 
PROCEDURE barcode-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-ln AS INT NO-UNDO.
   DEF VAR v-ol-uom AS CHAR NO-UNDO.
   DEF VAR v-lot-no AS CHAR NO-UNDO.
   DEF VAR v-i-no AS CHAR NO-UNDO.
   DEF VAR lv-text AS CHAR NO-UNDO.
   DEF VAR v-ps1-note AS cha FORM "x(80)" EXTENT 8 NO-UNDO.
   DEF VAR v-ps2-note AS cha FORM "x(80)" EXTENT 8 NO-UNDO.
   DEF VAR v-ps3-note AS cha FORM "x(80)" EXTENT 8 NO-UNDO.


   {sys/form/r-top3.f}
   {sys/inc/print1.i}
   {sys/inc/outprint.i value(lines-per-page)}

   VIEW FRAME r-top.
   VIEW FRAME top.

   OUTPUT STREAM barcode TO VALUE(v-packslip).    
   PUT STREAM barcode UNFORMATTED
       "BOL#,Release#,CustBillTo,CustBillToName,Shipto,ShiptoName,Carrier,"
       "CustPart#,FGItem#,FGName,Desc1,Desc2,PO#,LN#,QtyShipped,Tag#,Lot#," +
       "PS11,PS12,PS13,PS14,PS15,PS16,PS17,PS18," +
       "PS21,PS22,PS23,PS24,PS25,PS26,PS27,PS28," +
       "PS31,PS32,PS33,PS34,PS35,PS36,PS37,PS38," +
       "Barcode,OrdLnUOM,BOLDate,Units,Qty/Unit,PartialCount," +
       "P/C,TotalPallets,TotalWeight,Order,Job#,ProdDate,RecDateByJob," SKIP.

   SESSION:SET-WAIT-STATE ("general").

   FOR EACH tt-packslip,
       FIRST oe-bolh WHERE 
       oe-bolh.b-no EQ tt-packslip.b-no
       NO-LOCK,
       EACH oe-boll WHERE
            oe-boll.company EQ oe-bolh.company AND
            oe-boll.b-no = oe-bolh.b-no
            NO-LOCK:

        PUT STREAM barcode UNFORMATTED 
            "~"" removeChars(STRING(oe-bolh.bol-no,">>>>>>>9"))  "~","
            "~"" removeChars(STRING(oe-bolh.release#)) "~","
            "~"" removeChars(oe-bolh.cust-no) "~",".

        FIND FIRST cust WHERE
             cust.company EQ oe-bolh.company AND
             cust.cust-no EQ oe-bolh.cust-no
             NO-LOCK NO-ERROR.

        IF AVAIL cust THEN
        DO:
           PUT STREAM barcode UNFORMATTED
               "~"" removeChars(cust.name) "~",".
           RELEASE cust.
        END.
        ELSE
           PUT STREAM barcode UNFORMATTED
               "~"" "~",".

        PUT STREAM barcode UNFORMATTED 
            "~"" STRING(removeChars(oe-bolh.ship-id))  "~",".

        FIND FIRST shipto WHERE
             shipto.company EQ oe-bolh.company AND
             shipto.cust-no EQ oe-bolh.cust-no AND
             shipto.ship-id EQ oe-bolh.ship-id
             NO-LOCK NO-ERROR.

        IF AVAIL shipto THEN
        DO:
           PUT STREAM barcode UNFORMATTED 
               "~"" removeChars(shipto.ship-name)  "~",".
           RELEASE shipto.
        END.
        ELSE
           PUT STREAM barcode UNFORMATTED
               "~"" "~",".

        PUT STREAM barcode UNFORMATTED 
            "~"" removeChars(oe-bolh.carrier)  "~",".

        FIND FIRST oe-ordl WHERE
             oe-ordl.company EQ oe-boll.company AND
             oe-ordl.ord-no EQ oe-boll.ord-no AND
             oe-ordl.LINE EQ oe-boll.LINE
             NO-LOCK NO-ERROR.

        IF AVAIL oe-ordl THEN
        DO:
           PUT STREAM barcode UNFORMATTED 
               "~"" removeChars(oe-ordl.part-no) "~","
               "~"" removeChars(oe-ordl.i-no) "~","
               "~"" removeChars(oe-ordl.i-name) "~","
               "~"" removeChars(oe-ordl.part-dscr1) "~","
               "~"" removeChars(oe-ordl.part-dscr2) "~",".

           ASSIGN
              v-i-no = oe-ordl.i-no
              v-ln = oe-ordl.e-num
              v-ol-uom = oe-ordl.pr-uom.

           RELEASE oe-ordl.
        END.
        ELSE
        DO:
           ASSIGN
              v-i-no = ""
              v-ln = 0
              v-ol-uom = "".

           PUT STREAM barcode UNFORMATTED 
               "~"" "~","
               "~"" "~","
               "~"" "~","
               "~"" "~","
               "~"" "~","
               "~"" "~",".
        END.

/*         FIND FIRST reftable WHERE                      */
/*              reftable.reftable EQ "oe-boll.lot-no" AND */
/*              reftable.rec_key = STRING(RECID(oe-boll)) */
/*              USE-INDEX rec_key                         */
/*              NO-LOCK NO-ERROR.                         */
/*                                                        */
/*         IF AVAIL reftable THEN                         */
/*         DO:                                            */
/*            v-lot-no = reftable.CODE.                   */
/*            RELEASE reftable.                           */
/*         END.                                           */
/*         ELSE                                           */
/*            v-lot-no = "".                              */
        v-lot-no = oe-boll.lot-no.

        PUT STREAM barcode UNFORMATTED 
            "~"" removeChars(oe-boll.po-no) "~","
            "~"" STRING(v-ln,">>>") "~","
            "~"" removeChars(STRING(oe-boll.qty)) "~","
            "~"" removeChars(oe-boll.tag) "~","
            "~"" removeChars(v-lot-no) "~",".

        EMPTY TEMP-TABLE tt-formtext.

        ASSIGN
           lv-text = ""
           v-ps1-note = ""
           v-ps2-note = ""
           v-ps3-note = "".

        find first itemfg where
             itemfg.company eq oe-boll.company AND
             itemfg.i-no    eq v-i-no
             no-lock no-error.

        if avail itemfg THEN DO:        

           FOR EACH notes WHERE
               notes.rec_key = itemfg.rec_key AND
               notes.note_code = "PS1"
               NO-LOCK:

               lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
           END.
           DO li = 1 TO 8:
               CREATE tt-formtext.
               ASSIGN tt-line-no = li
                      tt-length  = 80.
           END.
           RUN custom/formtext.p (lv-text).
           i = 0.           
           FOR EACH tt-formtext:
               i = i + 1.
               IF  i <= 8 THEN v-ps1-note[i] = tt-formtext.tt-text.      
           END.

           EMPTY TEMP-TABLE tt-formtext.
           lv-text = "".

           FOR EACH notes WHERE
               notes.rec_key = itemfg.rec_key AND
               notes.note_code = "PS2"
               NO-LOCK:

               lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
           END.
           DO li = 1 TO 8:
               CREATE tt-formtext.
               ASSIGN tt-line-no = li
                      tt-length  = 80.
           END.
           RUN custom/formtext.p (lv-text).
           i = 0.           
           FOR EACH tt-formtext:
               i = i + 1.
               IF  i <= 8 THEN v-ps2-note[i] = tt-formtext.tt-text.      
           END.

           EMPTY TEMP-TABLE tt-formtext.
           lv-text = "".

           FOR EACH notes WHERE
               notes.rec_key = itemfg.rec_key AND
               notes.note_code = "PS3"
               NO-LOCK:

               lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
           END.
           DO li = 1 TO 8:
               CREATE tt-formtext.
               ASSIGN tt-line-no = li
                      tt-length  = 80.
           END.
           RUN custom/formtext.p (lv-text).
           i = 0.           
           FOR EACH tt-formtext:
               i = i + 1.
               IF  i <= 8 THEN v-ps3-note[i] = tt-formtext.tt-text.      
           END.
        END.  /* avail itemfg */

        PUT STREAM barcode UNFORMATTED
            "~"" removeChars(v-ps1-note[1]) "~","
            "~"" removeChars(v-ps1-note[2]) "~","
            "~"" removeChars(v-ps1-note[3]) "~","
            "~"" removeChars(v-ps1-note[4]) "~","
            "~"" removeChars(v-ps1-note[5]) "~","
            "~"" removeChars(v-ps1-note[6]) "~","
            "~"" removeChars(v-ps1-note[7]) "~","
            "~"" removeChars(v-ps1-note[8]) "~","
            "~"" removeChars(v-ps2-note[1]) "~","
            "~"" removeChars(v-ps2-note[2]) "~","
            "~"" removeChars(v-ps2-note[3]) "~","
            "~"" removeChars(v-ps2-note[4]) "~","
            "~"" removeChars(v-ps2-note[5]) "~","
            "~"" removeChars(v-ps2-note[6]) "~","
            "~"" removeChars(v-ps2-note[7]) "~","
            "~"" removeChars(v-ps2-note[8]) "~","
            "~"" removeChars(v-ps3-note[1]) "~","
            "~"" removeChars(v-ps3-note[2]) "~","
            "~"" removeChars(v-ps3-note[3]) "~","
            "~"" removeChars(v-ps3-note[4]) "~","
            "~"" removeChars(v-ps3-note[5]) "~","
            "~"" removeChars(v-ps3-note[6]) "~","
            "~"" removeChars(v-ps3-note[7]) "~","
            "~"" removeChars(v-ps3-note[8]) "~","
            "~"" "(k)" + removeChars(oe-boll.po-no) + "(+)" +
                 removeChars(STRING(v-ln)) + "(+)" +
                 removeChars(STRING(oe-boll.qty)) "~","
            "~"" removeChars(v-ol-uom) "~","
            "~"" removeChars(STRING(oe-bolh.bol-date)) "~","
            "~"" removeChars(STRING(oe-boll.cases)) "~","
            "~"" removeChars(STRING(oe-boll.qty-case)) "~","
            "~"" removeChars(STRING(oe-boll.partial)) "~","
            "~"" removeChars(STRING(oe-boll.p-c,"C/P")) "~","
            "~"" removeChars(STRING(oe-bolh.tot-pallets)) "~","
            "~"" removeChars(STRING(oe-bolh.tot-wt)) "~","
            "~"" removeChars(STRING(oe-boll.ord-no)) "~","
            "~"" removeChars(TRIM(oe-boll.job-no)) + "-" + removeChars(STRING(oe-boll.job-no2,"99")) "~",".

        FIND FIRST oe-ord WHERE
             oe-ord.company EQ oe-boll.company AND
             oe-ord.ord-no EQ oe-boll.ord-no
             NO-LOCK NO-ERROR.

        IF AVAIL oe-ord THEN
        DO:
           PUT STREAM barcode UNFORMATTED
               "~"" removeChars(STRING(oe-ord.prod-date)) "~",".
           RELEASE oe-ord.
        END.
        ELSE
           PUT STREAM barcode UNFORMATTED
               "~"" "~",".

        RELEASE fg-rcpth.

        IF oe-boll.job-no NE "" THEN
        DO:
           FOR EACH fg-rcpth WHERE
               fg-rcpth.company   EQ oe-boll.company AND
               fg-rcpth.job-no    EQ oe-boll.job-no AND
               fg-rcpth.job-no2   EQ oe-boll.job-no2 AND
               fg-rcpth.i-no      EQ oe-boll.i-no AND
               fg-rcpth.rita-code EQ "R"
               NO-LOCK
               USE-INDEX job
               BY fg-rcpth.trans-date
               BY fg-rcpth.r-no:
               LEAVE.
           END.
        END.

        IF AVAIL fg-rcpth THEN
           PUT STREAM barcode UNFORMATTED
               "~"" removeChars(STRING(fg-rcpth.trans-date)) "~",".
        ELSE
           PUT STREAM barcode UNFORMATTED
               "~"" "?" "~",".

        PUT STREAM barcode UNFORMATTED SKIP.
   END.

   OUTPUT STREAM barcode CLOSE.
   SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-work C-Win 
PROCEDURE build-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ic2ndKey  AS CHAR NO-UNDO.

  DEFINE VARIABLE reportKey10 AS LOGICAL NO-UNDO. /* 05291402 */

  build-work:
  FOR EACH oe-bolh
     WHERE oe-bolh.company EQ cocode
       AND oe-bolh.bol-no  GE v-s-bol
       AND oe-bolh.bol-no  LE v-e-bol
       AND oe-bolh.cust-no GE v-s-cust
       AND oe-bolh.cust-no LE v-e-cust 
       and oe-bolh.bol-date  ge v-s-date
       and oe-bolh.bol-date  le v-e-date
       AND oe-bolh.printed EQ v-printed
       AND oe-bolh.posted  EQ tb_posted
       AND CAN-FIND (FIRST oe-boll
                     WHERE oe-boll.company EQ oe-bolh.company
                       AND oe-boll.b-no    EQ oe-bolh.b-no
                       AND oe-boll.ord-no  GE v-s-ord
                       AND oe-boll.ord-no  LE v-e-ord)
     USE-INDEX post:
          
    RUN oe/custxship.p (oe-bolh.company,
        oe-bolh.cust-no,
          oe-bolh.ship-id,
          BUFFER shipto).
          
    IF NOT AVAILABLE shipto THEN 
    DO:
        MESSAGE "BOL has an invalid shipto."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
        NEXT build-work.            
    END. 
        
    IF NOT oe-ctrl.p-bol THEN
    FOR EACH oe-boll
       WHERE oe-boll.company EQ oe-bolh.company
         AND oe-boll.bol-no  EQ oe-bolh.bol-no
         AND CAN-FIND(FIRST oe-ord
                      WHERE oe-ord.company EQ oe-boll.company
                        AND oe-ord.ord-no  EQ oe-boll.ord-no
                        AND (oe-ord.stat    EQ "H" OR oe-ord.priceHold))
        NO-LOCK:

      IF begin_bol# EQ END_bol# THEN
         MESSAGE "Order on BOL is on hold, and BOL will not print."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

      NEXT build-work.
    END.

    IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "PremierXFooter" OR v-print-fmt EQ "RFCX" OR v-print-fmt = "PremierCX" OR 
       v-print-fmt = "PremierPX" OR v-print-fmt =  "PremierBroker"  THEN DO:
       IF AVAIL oe-bolh THEN do:
           IF oe-bolh.frt-pay EQ "B" AND oe-bolh.freight EQ 0 THEN
               MESSAGE "This Bill of Lading (" + STRING(oe-bolh.bol-no) +  ") has $0.00 Freight Cost but is entered as Freight Payment = 'B' for Billable Freight."
                "Please review Freight Cost." VIEW-AS ALERT-BOX WARNING BUTTONS OK .
       END.
   END.

    /* update loadtag status - Bill of lading task#: 10190414 */
    IF NOT oe-bolh.printed THEN
    FOR EACH bf-oe-boll NO-LOCK
       WHERE bf-oe-boll.company EQ oe-bolh.company 
         AND bf-oe-boll.b-no    EQ oe-bolh.b-no
         AND bf-oe-boll.tag     NE "",
       FIRST loadtag
       WHERE loadtag.company   EQ bf-oe-boll.company
         AND loadtag.item-type EQ NO
         AND loadtag.tag-no    EQ bf-oe-boll.tag
   USE-INDEX tag:
      loadtag.sts = "Bill of Lading".
    END.

    IF ic2ndKey NE ? AND ic2ndKey NE '' THEN DO:
      FIND FIRST shipto NO-LOCK
           WHERE shipto.rec_key = ic2ndKey 
             AND shipto.ship-id = oe-bolh.ship-id NO-ERROR.
      IF NOT AVAIL shipto THEN NEXT build-work.
    END.

    FIND FIRST sys-ctrl-shipto WHERE
         sys-ctrl-shipto.company      EQ oe-bolh.company AND
         sys-ctrl-shipto.name         EQ "BOLFMT" AND
         sys-ctrl-shipto.cust-vend    EQ YES AND
         sys-ctrl-shipto.cust-vend-no EQ oe-bolh.cust-no AND
         sys-ctrl-shipto.ship-id      EQ oe-bolh.ship-id
         NO-LOCK NO-ERROR.
    IF NOT AVAIL sys-ctrl-shipto THEN
        FIND FIRST sys-ctrl-shipto WHERE
         sys-ctrl-shipto.company      EQ oe-bolh.company AND
         sys-ctrl-shipto.name         EQ "BOLFMT" AND
         sys-ctrl-shipto.cust-vend    EQ YES AND
         sys-ctrl-shipto.cust-vend-no EQ oe-bolh.cust-no AND
         sys-ctrl-shipto.ship-id      EQ ''
         NO-LOCK NO-ERROR.


    ASSIGN
       reportKey10     = oe-bolh.printed /* 05291402 */
       oe-bolh.printed = YES
       vcBOLNums       = vcBOLNums + '-' + STRING (oe-bolh.bol-no)
       vcBOLNums       = LEFT-TRIM (vcBOLNums, '-').

    IF vcBOLNums MATCHES '*-*' THEN 
       vcBOLNums = RIGHT-TRIM (SUBSTRING (vcBOLNums, 1, INDEX (vcBOLNums,'-')), '-') + SUBSTRING (vcBOLNums, R-INDEX (vcBOLNums, '-')).

    IF NOT CAN-FIND(FIRST report WHERE
       report.term-id = v-term-id AND
       report.rec-id  = RECID(oe-bolh)) THEN
       DO:
          CREATE report.
          ASSIGN 
              report.term-id  = v-term-id
              report.key-01   = oe-bolh.cust-no
              report.key-02   = oe-bolh.ship-id
              report.rec-id   = RECID(oe-bolh)
              report.key-09   = STRING(oe-bolh.printed,"REVISED/ORIGINAL")
              report.key-10   = STRING(reportKey10) /* 05291402 */
              report.key-03   = IF AVAIL sys-ctrl-shipto AND  NOT sys-ctrl-shipto.log-fld THEN "C" /*commercial invoice only*/
                                ELSE IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN "B" /*commercial invoice and bol both*/
                                ELSE "N" /*BOL only*/ 
              report.key-04   = IF AVAIL sys-ctrl-shipto THEN sys-ctrl-shipto.char-fld ELSE "".
       END.

    status default 'Now Processing BOL: ' + string (oe-bolh.bol-no) + '....'.

    lv-run-bol        = IF lv-run-bol = "" AND report.key-04 = "FIBRECI" THEN  "NO" ELSE  "Yes" .
    IF lv-run-commercial = "" AND report.key-03 <> "N" AND v-coc-fmt <> "CCC" AND v-coc-fmt <> "CCCWPP" AND v-coc-fmt <> "CCCW" AND v-coc-fmt <> "CCC2" THEN
         lv-run-commercial = "YES".

    IF NOT CAN-FIND(FIRST tt-post WHERE tt-post.row-id = ROWID(oe-bolh)) THEN
    DO:
       CREATE tt-post.
       tt-post.row-id = ROWID(oe-bolh).
    END.

    IF tb_barcode:CHECKED IN FRAME {&frame-name} AND
       NOT CAN-FIND(FIRST tt-packslip where
       tt-packslip.b-no = oe-bolh.b-no) THEN
       DO:
          CREATE tt-packslip.
          tt-packslip.b-no = oe-bolh.b-no.
          RELEASE tt-packslip.
       END.
  END.

  v-lines-per-page = lines-per-page.

  status default ''.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-bol-security C-Win 
PROCEDURE check-bol-security :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: TASK 07240906
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-prg AS CHAR NO-UNDO.
DEF OUTPUT PARAM op-senflg AS LOG NO-UNDO INIT YES.

DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO INIT "ASI".
DEFINE VARIABLE Audit_File AS CHARACTER NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE num-groups AS INTEGER NO-UNDO.
DEFINE VARIABLE group-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL NO-UNDO.

FIND b-prgrms WHERE b-prgrms.prgmname = ip-prg NO-LOCK NO-ERROR.
IF AVAILABLE b-prgrms THEN
DO:

  DO num-groups = 1 TO NUM-ENTRIES(g_groups):
    IF NOT CAN-DO(TRIM(b-prgrms.can_run),ENTRY(num-groups,g_groups)) AND
       NOT CAN-DO(TRIM(b-prgrms.can_update),ENTRY(num-groups,g_groups)) AND
       NOT CAN-DO(TRIM(b-prgrms.can_create),ENTRY(num-groups,g_groups)) AND
       NOT CAN-DO(TRIM(b-prgrms.can_delete),ENTRY(num-groups,g_groups)) THEN
       NEXT.

    group-ok = yes.
    LEAVE.
  END.

  IF NOT CAN-DO(TRIM(b-prgrms.can_run),USERID("ASI")) AND
     NOT CAN-DO(TRIM(b-prgrms.can_update),USERID("ASI")) AND
     NOT CAN-DO(TRIM(b-prgrms.can_create),USERID("ASI")) AND
     NOT CAN-DO(TRIM(b-prgrms.can_delete),USERID("ASI")) AND NOT group-ok THEN
  DO:
    /*MESSAGE 
        "User access to POST BOL this Program Denied - Contact Systems Manager" 
       VIEW-AS ALERT-BOX ERROR.
    access-close = YES.    /* used later in methods/template/windows.i - local-initialize procedure */
    */
    ASSIGN op-senflg  = NO.
  END.

END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CommercialInvoice C-Win 
PROCEDURE CommercialInvoice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  SESSION:SET-WAIT-STATE ("general").

  EMPTY TEMP-TABLE tt-ci-form.

  RUN oerep\r-coinvbol.w(INPUT begin_bol#,
                         INPUT end_bol#).

  FOR EACH w-comm-bol,
      EACH b1-oe-bolh WHERE
           b1-oe-bolh.company eq cocode AND
           b1-oe-bolh.bol-no  EQ w-comm-bol.bol-no AND
           can-find (FIRST b1-oe-boll WHERE
                           b1-oe-boll.company EQ b1-oe-bolh.company AND
                           b1-oe-boll.b-no    EQ b1-oe-bolh.b-no)
           NO-LOCK,
      FIRST sys-ctrl-shipto WHERE
            sys-ctrl-shipto.company      EQ cocode AND
            sys-ctrl-shipto.name         EQ "CINVOICE" AND
            sys-ctrl-shipto.cust-vend    EQ YES AND
            sys-ctrl-shipto.cust-vend-no EQ b1-oe-bolh.cust-no AND
            sys-ctrl-shipto.ship-id      EQ b1-oe-bolh.ship-id
            NO-LOCK:

      FIND FIRST tt-ci-form WHERE
           tt-ci-form.form-name = sys-ctrl-shipto.char-fld
           NO-ERROR.

      IF NOT AVAIL tt-ci-form THEN
      DO:
         CREATE tt-ci-form.
         ASSIGN tt-ci-form.form-name = sys-ctrl-shipto.char-fld.
      END.

      tt-ci-form.total-pallets = tt-ci-form.total-pallets
                               + b1-oe-bolh.tot-pallets.
  END.

  FOR EACH tt-ci-form:
      RUN oerep\d-fibreci.w (INPUT tt-ci-form.form-name,
                             INPUT tt-ci-form.total-pallets).
  END.

  SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-reorder C-Win 
PROCEDURE create-reorder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAM ip-oeboll-rowid AS ROWID NO-UNDO.
 DEF BUFFER bf-oeboll FOR oe-boll.

 DEF VAR v-qty-onh AS INT NO-UNDO.
 DEF VAR v-qty-avail AS INT NO-UNDO.
 DEF VAR v-reord-qty AS INT NO-UNDO.

 FIND bf-oeboll WHERE ROWID(bf-oeboll) = ip-oeboll-rowid NO-LOCK.
 FIND itemfg WHERE itemfg.company = cocode AND
                   itemfg.i-no = bf-oeboll.i-no NO-LOCK.

 v-qty-onh = 0.
 FOR EACH fg-bin FIELDS(qty)
         WHERE fg-bin.company EQ itemfg.company
           AND fg-bin.i-no    EQ itemfg.i-no
           /*AND fg-bin.loc     GE begin_whse
           AND fg-bin.loc     LE end_whse*/ NO-LOCK:
      v-qty-onh = v-qty-onh + fg-bin.qty.
 END.

 ASSIGN
     v-qty-avail = v-qty-onh /*+ (if v-inconh then itemfg.q-ono else 0)*/
                    -  itemfg.q-alloc.

 if itemfg.ord-level gt v-qty-avail then do:
    v-reord-qty = itemfg.ord-level - v-qty-avail.

    if v-reord-qty lt itemfg.ord-min and
       itemfg.ord-min ne 0 then 
       v-reord-qty = itemfg.ord-min.

    if v-reord-qty gt itemfg.ord-max and
       itemfg.ord-max ne 0 then 
       v-reord-qty = itemfg.ord-max.
 end.
 else v-reord-qty = 0.

 IF v-reord-qty > 0 THEN DO:
    CREATE tt-email.
    ASSIGN tt-email.bol-no = bf-oeboll.bol-no
           tt-email.ord-no = bf-oeboll.ord-no
           tt-email.i-no = bf-oeboll.i-no
           tt-email.qty = v-reord-qty
           tt-email.cust-no = oe-bolh.cust-no.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE email-reorderitems C-Win 
PROCEDURE email-reorderitems :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR retcode AS INT NO-UNDO.
  DEF VAR ls-to-list AS cha NO-UNDO.
  DEF VAR lv-mailto AS cha NO-UNDO.
  DEF VAR lv-mailsubject AS cha NO-UNDO.
  DEF VAR lv-mailbody AS cha NO-UNDO.
  DEF VAR lv-mailattach AS cha NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.
  DEF VAR v-qty-onh AS INT NO-UNDO.
  DEF VAR v-qty-avail AS INT NO-UNDO.
  DEF VAR v-qty-alloc AS INT NO-UNDO.
  DEF VAR v-qty-onOrder AS INT NO-UNDO.

  FIND FIRST users WHERE
        users.user_id EQ USERID("ASI")
        NO-LOCK NO-ERROR.
  IF AVAIL users AND users.user_program[2] NE "" THEN v-dir = users.user_program[2] + "\".
  ELSE v-dir = "c:\tmp\".

  FOR EACH tt-email,
       FIRST cust NO-LOCK WHERE cust.company = cocode
                           AND cust.cust-no = tt-email.cust-no
                           AND cust.active = "E" BREAK BY tt-email.cust-no BY tt-email.i-no:
       IF FIRST-OF(tt-email.cust-no) THEN DO:
          v-fgemail-file = v-dir + trim(tt-email.cust-no) + ".txt".
          OUTPUT STREAM st-email TO VALUE(v-fgemail-file).
          PUT STREAM st-email "***** Reorder Point Item from BOL Posting *****" SKIP
                              "BOL#     Order#       FG Item#      ReOrder Qty     Avail Qty  On Hand Qty On Order Qty" SKIP
                              "======== ========== =============== ============ ============ ============ ============" SKIP.
       END.
       IF FIRST-OF(tt-email.i-no) THEN DO:
          /*v-qty-onh = 0.
          FOR EACH fg-bin WHERE fg-bin.company EQ cocode
                         AND fg-bin.i-no    EQ tt-email.i-no
                 /*AND fg-bin.loc     GE begin_whse
                 AND fg-bin.loc     LE end_whse*/  NO-LOCK:
              v-qty-onh = v-qty-onh + fg-bin.qty.
         END.
         */
         FIND itemfg WHERE itemfg.company = cocode
                       AND itemfg.i-no = tt-email.i-no NO-LOCK NO-ERROR.
         IF AVAIL itemfg THEN ASSIGN v-qty-onh = itemfg.q-onh
                                     v-qty-onOrder = itemfg.q-ono
                                     v-qty-alloc = itemfg.q-alloc.
         ELSE ASSIGN v-qty-onh = 0
                     v-qty-onOrder = 0
                     v-qty-alloc = 0.
         {sys/inc/oereordr.i}

         v-qty-avail = v-qty-onh +
                       (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE v-qty-onOrder) -
                       v-qty-alloc.

       END.

       PUT STREAM st-email UNFORMATTED
                 STRING(tt-email.bol-no) FORM "x(9)"
                 string(tt-email.ord-no) FORM "x(10)"
                 " " tt-email.i-no " " tt-email.qty FORM "->>>,>>>,>>9" 
                 " " v-qty-avail  FORM "->>>,>>>,>>9"
                 " " v-qty-onh FORM "->>>,>>>,>>9"
                 " " v-qty-onOrder FORM "->>>,>>>,>>9"
                 SKIP.
       IF LAST-OF(tt-email.cust-no) THEN do:
           OUTPUT STREAM st-email CLOSE.
           {custom/emailList.i &recKey=cust.rec_key &prgmName='r-bolpst.' &emailList=ls-to-list}   
           IF ls-to-list NE '' THEN DO:
             ASSIGN lv-mailto = "To:" + ls-to-list
                    lv-mailsubject = "Finished Goods Reorder Point item from BOL Post"
                    lv-mailbody = "Finished Goods Reorder Point item from BOL Post"
                    lv-mailattach = v-fgemail-file.
             RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,lv-mailattach,1,OUTPUT retcode).
           END.
       END. /* last-of(tt-email.cust-no) */
   END.

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
  DISPLAY begin_cust end_cust begin_bol# begin_ord# end_ord# begin_date end_date 
          tb_reprint tb_pallet tb_posted tb_print-component tb_print-shipnote 
          tb_barcode tb_print_ship tb_print-barcode 
          tb_print-unassemble-component tb_print-binstags lbl_bolsort 
          rd_bol-sort fi_specs tb_print-spec lbl_bolcert rd_bolcert 
          tb_per-bol-line tb_EMailAdvNotice rd-dest tb_MailBatchMode 
          tb_ComInvoice tb_freight-bill tb_footer lv-ornt lines-per-page 
          lv-font-no lv-font-name tb_post-bol td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 begin_cust end_cust begin_bol# begin_ord# end_ord# begin_date 
         end_date tb_reprint tb_pallet tb_posted tb_print-component 
         tb_print-shipnote tb_barcode tb_print_ship tb_print-barcode 
         tb_print-unassemble-component tb_print-binstags rd_bol-sort fi_specs 
         tb_print-spec rd_bolcert tb_per-bol-line tb_EMailAdvNotice rd-dest 
         tb_MailBatchMode tb_ComInvoice tb_freight-bill tb_footer lv-ornt 
         lines-per-page lv-font-no tb_post-bol td-show-parm btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exception-rpt C-Win 
PROCEDURE exception-rpt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* -------------------------------------------------- oe/oe-bolp7.p 11/01 JLF */
/* BOL posting Exception Report                                               */
/* -------------------------------------------------------------------------- */
DEF VAR td-full-tag AS LOG NO-UNDO.
DEF VAR tran-date AS DATE NO-UNDO.
DEF VAR tran-period AS INT NO-UNDO.
DEF VAR vtag AS INT NO-UNDO.
DEF VAR vtag2 AS INT NO-UNDO.
DEF VAR dis-tag AS CHAR NO-UNDO.
{sys/form/r-top3w.f}

     ASSIGN td-full-tag = YES.
    MESSAGE " Do you wish to print full tag value?  "  
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO 
        UPDATE td-full-tag .

  FORM HEADER SKIP(1) WITH FRAME r-top.

  FIND first period                   
      where period.company eq gcompany
        and period.pst     le tran-date
        and period.pend    ge tran-date
      no-lock no-error.

  assign
   str-tit2 = "BOL - Insufficient Inventory Report"
   {sys/inc/ctrtext.i str-tit2 112}

   str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 132}.

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  display with frame r-top.

  for each w-except,

      first oe-bolh
      where oe-bolh.company eq cocode
        and oe-bolh.bol-no  eq w-except.bol-no
      no-lock

     break by w-except.bol-no
           by w-except.ord-no
           by w-except.rel-no
           by w-except.b-ord-no:

    if first-of(w-except.bol-no) then do:
      display oe-bolh.bol-date FORMAT "99/99/9999" COLUMN-LABEL "Date"
              oe-bolh.bol-no format ">>>>>>>>" COLUMN-LABEL "   BOL #"
              oe-bolh.carrier FORMAT "X(5)" COLUMN-LABEL "Carrier"
              oe-bolh.trailer FORMAT "X(20)" COLUMN-LABEL "Trailer"
              oe-bolh.freight format "->>>,>>9.99" COLUMN-LABEL "    Freight"
              oe-bolh.cwt     COLUMN-LABEL "  Rate"
              oe-bolh.tot-wt  format "->>>,>>9" COLUMN-LABEL "   Tot WT"
              oe-bolh.cust-no COLUMN-LABEL "Cust#"
              oe-bolh.ship-id COLUMN-LABEL "Ship#"
              oe-bolh.deleted format "*DELETED*/ " COLUMN-LABEL "Deleted"
              SKIP(1)
          with frame bolh2 DOWN NO-BOX NO-ATTR-SPACE STREAM-IO WIDTH 150.
      down with frame bolh2.
    end.

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-except.i-no
        no-lock no-error.

       vtag = 0.
       vtag2 = 0.
       vtag = LENGTH(w-except.tag).
       vtag2 = vtag - 5 .

      IF NOT td-full-tag AND vtag <> 0 THEN ASSIGN  dis-tag =  SUBSTR(w-except.tag,vtag2,6) .
      ELSE ASSIGN dis-tag  = w-except.tag .

    display SPACE(5)
            w-except.i-no  COLUMN-LABEL "Item #"  
            w-except.qty   COLUMN-LABEL "Qty"
            w-except.dOnhQty  FORMAT "->>>,>>>,>>9" COLUMN-LABEL "On Hand Qty"
            dis-tag COLUMN-LABEL "Tag" FORMAT "X(22)"
            itemfg.i-name  FORMAT "X(20)" when avail itemfg COLUMN-LABEL "Item Name"
            w-except.po-no COLUMN-LABEL "P.O. #"    
            w-except.ord-no COLUMN-LABEL "  Ord#"   
            STRING(w-except.rel-no,">>9") + "-" + STRING(w-except.b-ord-no,"99") COLUMN-LABEL "Rel.#"    
            w-except.loc COLUMN-LABEL "Whse."
            w-except.loc-bin COLUMN-LABEL "Bin Loc"   

            w-except.cases format "->>>,>>9"   COLUMN-LABEL "   Cases"
            w-except.qty-case format "->>>,>>9" COLUMN-LABEL "Qty/Case" 
            w-except.partial format "->>>,>>9"  COLUMN-LABEL " Partial"
            w-except.weight format "->>>,>>9"   COLUMN-LABEL "  Weight"
        with frame boll2 DOWN NO-BOX NO-ATTR-SPACE STREAM-IO WIDTH 180.
    down with frame boll2.

    put skip(1).
  END.

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
  DEFINE INPUT PARAM ic1stKey AS CHAR NO-UNDO.
  DEFINE INPUT PARAM ic2ndKey AS CHAR NO-UNDO.
  DEFINE INPUT PARAM iiMode   AS INTE NO-UNDO.
  DEFINE INPUT PARAM icType   AS CHAR NO-UNDO.

  /* XPrint */
  IF is-xprint-form THEN DO:

    RUN run-report-mail (INPUT ic1stKey,
                         INPUT ic2ndKey,
                         INPUT iiMode,
                         INPUT YES).

    IF NOT vcBOLNums GT '' THEN RETURN.

    IF v-print-fmt = "SouthPak-XL" OR v-print-fmt = "Prystup-Excel" THEN do:
       ASSIGN lv-pdf-file = init-dir + "\" + string(b1-oe-bolh.bol-no) + ".pdf".

       END.


    ELSE DO:    
      lv-pdf-file = lv-pdf-file + vcBOLNums + '.pdf'.
      IF list-name NE ? AND list-name NE '' THEN
         RUN printPDF (list-name,   "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").                            
      ELSE
         RUN printPDF (lv-pdf-file, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").                            
    END.

    CASE icType:

      WHEN 'Customer1':U  THEN RUN SendMail-1 (ic1stKey, 'Customer1',"").
      WHEN 'Customer':U   THEN RUN SendMail-1 (ic1stKey, 'Customer',"").
      WHEN 'ShipTo1':U    THEN RUN SendMail-1 (ic2ndKey, 'ShipTo1',""). 
      WHEN 'ShipTo':U     THEN RUN SendMail-1 (ic2ndKey, 'ShipTo',""). 

    END CASE.
  END.

  /* Not XPrint */
  ELSE DO:

    RUN run-report-mail (INPUT ic1stKey,
                         INPUT ic2ndKey,
                         INPUT iiMode,
                         INPUT YES).

    IF NOT vcBOLNums GT '' THEN RETURN.

    CASE icType:

      WHEN 'Customer1':U  THEN RUN SendMail-2 (ic1stKey, 'Customer1').
      WHEN 'Customer':U   THEN RUN SendMail-2 (ic1stKey, 'Customer').
      WHEN 'ShipTo1':U    THEN RUN SendMail-2 (ic2ndKey, 'ShipTo1'). 
      WHEN 'ShipTo':U     THEN RUN SendMail-2 (ic2ndKey, 'ShipTo'). 

    END CASE.
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
   DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

   IF (v-print-bol AND v-print-fmt <> "SouthPak-XL" AND v-print-fmt <> "Prystup-Excel") OR
      (NOT v-print-bol AND v-coc-fmt <> "Unipak-XL" AND v-coc-fmt <> "ACPI" AND v-coc-fmt <> "CCC" AND v-coc-fmt <> "CCCWPP" AND v-coc-fmt <> "CCCW" AND v-coc-fmt <> "CCC2") THEN
      case rd-dest:
         when 1 then run output-to-printer(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
         when 2 then run output-to-screen(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
         when 3 then run output-to-file(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
         when 4 then run output-to-fax(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
         WHEN 6 THEN RUN output-to-port(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
      end case.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPrintBarTag C-Win 
PROCEDURE getPrintBarTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM opPrintBarTag AS LOG .

  opPrintBarTag = tb_print-barcode.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPrintBinsTags C-Win 
PROCEDURE getPrintBinsTags :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAM opPrintBinsTags AS LOG .

  opPrintBinsTags = tb_print-binstags.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPrintSpecNotes C-Win 
PROCEDURE getPrintSpecNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAM opPrintSpec AS LOG .
 DEF OUTPUT PARAM opSpecList AS CHAR.

  opPrintSpec = tb_print-spec.
  opSpecList = fi_specs.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-bol# C-Win 
PROCEDURE new-bol# :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF INT(begin_bol#:SCREEN-VALUE) NE 0                          AND
       INT(begin_bol#:SCREEN-VALUE) EQ INT(end_bol#:SCREEN-VALUE) THEN DO:
      FIND FIRST oe-bolh NO-LOCK
          WHERE oe-bolh.company EQ cocode
            AND oe-bolh.bol-no  EQ INT(begin_bol#:SCREEN-VALUE)
          NO-ERROR.
      IF AVAIL oe-bolh THEN
        ASSIGN
         tb_reprint:SCREEN-VALUE = STRING(oe-bolh.printed)
         tb_posted:SCREEN-VALUE  = STRING(oe-bolh.posted).
    END. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-exception-file C-Win 
PROCEDURE output-exception-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

     if init-dir = "" then init-dir = "c:\tmp" .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-exception-printer C-Win 
PROCEDURE output-exception-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-exception-screen C-Win 
PROCEDURE output-exception-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-fax C-Win 
PROCEDURE output-to-fax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    {custom/asifax3.i &type         = "Customer"
                     &begin_cust   = ip-cust-no
                     &END_cust     = ip-cust-no
                     &fax-subject  = "BOL"
                     &fax-body     = "BOL"
                     &fax-file     = list-name
                     &end-widget   = begin_cust}

    /* Intercept Advanced Ship Notice Job */
    IF tb_EMailAdvNotice:CHECKED IN FRAME {&FRAME-NAME} THEN
       RUN AdvancedNotice(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
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
  DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

  {custom/out2file.i}

  /* Intercept Advanced Ship Notice Job */
  IF tb_EMailAdvNotice:CHECKED IN FRAME {&FRAME-NAME} THEN
     RUN AdvancedNotice(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
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
  DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.
  DEFINE INPUT PARAMETER ip-ship-id AS CHARACTER NO-UNDO.

  ASSIGN
    v-s-bol             = begin_bol#
    v-e-bol             = end_bol#
    v-s-ord             = begin_ord#
    v-e-ord             = end_ord#
    v-s-date             = begin_date
    v-e-date             = end_date
    v-printed           = tb_reprint
    v-print-pal         = tb_pallet
    v-print-bol         = rd_bolcert EQ "BOL"
    v-print-components  = tb_print-component
    v-print-shipnotes   = tb_print-shipnote
    v-print-dept        = tb_print-dept
    lv-run-bol          = ""
    lv-run-commercial   = ""
    v-print-unassembled = tb_print-unassemble-component
    v-footer            = tb_footer.

  IF ip-sys-ctrl-shipto THEN
     ASSIGN
        v-s-cust = ip-cust-no
        v-e-cust = ip-cust-no.
  ELSE
     ASSIGN 
        v-s-cust = begin_cust
        v-e-cust = end_cust.

  IF fi_depts:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        v-print-dept = LOGICAL(tb_print-dept:SCREEN-VALUE)
        v-depts = fi_depts:SCREEN-VALUE.

  FOR EACH b1-cust NO-LOCK
     WHERE b1-cust.company EQ cocode
       AND b1-cust.cust-no GE v-s-cust
       AND b1-cust.cust-no LE v-e-cust,

     FIRST b1-oe-bolh NO-LOCK
     where b1-oe-bolh.company eq cocode
       and b1-oe-bolh.bol-no  ge v-s-bol
       and b1-oe-bolh.bol-no  le v-e-bol
       and b1-oe-bolh.cust-no EQ b1-cust.cust-no
       and b1-oe-bolh.bol-date  ge v-s-date
       and b1-oe-bolh.bol-date  le v-e-date
       and b1-oe-bolh.printed eq v-printed
       and b1-oe-bolh.posted  eq tb_posted
       and can-find (FIRST b1-oe-boll
                     WHERE b1-oe-boll.company EQ b1-oe-bolh.company
                       AND b1-oe-boll.b-no    EQ b1-oe-bolh.b-no
                       AND b1-oe-boll.ord-no  GE v-s-ord
                       AND b1-oe-boll.ord-no  LE v-e-ord)
    USE-INDEX post:

    ASSIGN  
      vcBOLNums   = '' 
      lv-pdf-file = init-dir + '\BOL'
      vcMailMode  = if tb_MailBatchMode then 'Customer1'  /* Silent Mode */
                                        else 'Customer'.  /* Dialog Box */
    /* XPrint */
    IF is-xprint-form THEN DO:

      RUN run-report-mail (INPUT b1-cust.cust-no,
                           INPUT '',
                           INPUT 1,
                           INPUT v-printed).

      IF v-print-fmt = "SouthPak-XL" OR v-print-fmt = "Prystup-Excel"  THEN do:
         ASSIGN lv-pdf-file = init-dir + "\bol" + ".pdf".
          /* RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").  */
      END.

      ELSE DO:    
        lv-pdf-file = lv-pdf-file + vcBOLNums + '.pdf'.
        IF list-name NE ? AND
           list-name NE ''
          THEN RUN printPDF (list-name,   "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
          ELSE RUN printPDF (lv-pdf-file, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").  
      END.

      if vcMailMode = 'Customer1' then RUN SendMail-1 (b1-cust.cust-no, 'Customer1', b1-oe-bolh.ship-id). /* Silent Mode */
                                  else RUN SendMail-1 (b1-cust.cust-no, 'Customer', b1-oe-bolh.ship-id).  /* Dialog Box */

    END.

    /* Not XPrint */
    ELSE DO:

      RUN run-report-mail (INPUT b1-cust.cust-no,
                           INPUT '',
                           INPUT 1,
                           INPUT v-printed).

      IF NOT v-print-bol AND (v-coc-fmt EQ "Unipak-XL" OR v-coc-fmt EQ "CCC" OR v-coc-fmt EQ "CCCWPP" OR v-coc-fmt EQ "CCCW" OR v-coc-fmt EQ "CCC2")  THEN
      DO:
         lv-pdf-file = init-dir + "\cofc.pdf".

         CASE vcMailMode:
            WHEN 'Customer1':U  THEN RUN send-mail-uni-xl(b1-cust.cust-no,'Customer1').
            WHEN 'Customer':U   THEN RUN send-mail-uni-xl(b1-cust.cust-no,'Customer'). 
         END CASE.
      END.
      ELSE
         CASE vcMailMode:

           WHEN 'Customer1':U  THEN RUN SendMail-2 (b1-cust.cust-no, 'Customer1').
           WHEN 'Customer':U   THEN RUN SendMail-2 (b1-cust.cust-no, 'Customer').
         END CASE.
    END.
  END. /* each cust */

  IF tb_EMailAdvNotice:CHECKED IN FRAME {&FRAME-NAME} THEN
     RUN AdvancedNotice(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).

  if tb_MailBatchMode then
     MESSAGE "Your E-Mails have been sent in Silent Mode."  skip
             "Please verify transmission in your SENT folder."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

  status default 'Enter data or ESC to end.'.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  SESSION:SET-WAIT-STATE ("").

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
  DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

  RUN custom/d-print.w(list-name).

  /* Intercept Advanced Ship Notice Job */
  IF tb_EMailAdvNotice:CHECKED IN FRAME {&FRAME-NAME} THEN
     RUN AdvancedNotice(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).

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
  DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE IF lv-prt-bypass THEN
     RUN custom/d-print.w (list-name).
  ELSE
     RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).

  /* Intercept Advanced Ship Notice Job */  
  IF tb_EMailAdvNotice:CHECKED IN FRAME {&FRAME-NAME} THEN 
     RUN AdvancedNotice(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).

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
  DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE
     run custom/scr-rpt2.w (list-name,c-win:title,int(lv-font-no),lv-ornt,lv-prt-bypass).

  /* Intercept Advanced Ship Notice Job */

 IF tb_EMailAdvNotice:CHECKED IN FRAME {&FRAME-NAME} THEN
    RUN AdvancedNotice(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-bol C-Win 
PROCEDURE post-bol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lotNo AS CHARACTER NO-UNDO.
  DEFINE VARIABLE trailerNo AS CHARACTER NO-UNDO.
  DEFINE VARIABLE outFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE d-out   AS DECIMAL NO-UNDO.
  DEF BUFFER bf-oe-ord FOR oe-ord.
  DEF BUFFER bf-oe-ordl FOR oe-ordl.
  {sa/sa-sls01.i}

  FOR EACH w-ord. DELETE w-ord. END.

  EMPTY TEMP-TABLE ediOutFile.

   RUN-PROC = "sbo/oerel-recalc-act.p".
/*    {methods/smartrun.i} */
   RUN VALUE(run-proc) PERSISTENT SET phandle NO-ERROR.
   lr-rel-lib = phandle.

  FOR EACH tt-post TRANSACTION:
    RELEASE oe-bolh.
    DO WHILE NOT AVAIL oe-bolh:
      FIND FIRST oe-bolh EXCLUSIVE WHERE ROWID(oe-bolh) EQ tt-post.row-id
          NO-WAIT NO-ERROR.

      IF AVAIL oe-bolh AND oe-bolh.posted EQ NO THEN DO:

        FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no,
          EACH oe-ordl NO-LOCK
             WHERE oe-ordl.company EQ oe-boll.company
               AND oe-ordl.ord-no EQ oe-boll.ord-no
                AND oe-ordl.line EQ oe-boll.LINE:
          FOR EACH oe-rel 
                WHERE oe-rel.company EQ oe-ordl.company
                  AND oe-rel.ord-no  EQ oe-ordl.ord-no
                  AND oe-rel.i-no    EQ oe-ordl.i-no
                  AND oe-rel.line    EQ oe-ordl.line
                  AND oe-rel.stat = "P"
                  AND oe-rel.link-no GT 0 
                  AND oe-rel.rel-no GT 0:


                IF AVAIL oe-rel /*AND avail(tt-rels) */ AND VALID-HANDLE(lr-rel-lib) THEN 
                   RUN recalc-act-qty IN lr-rel-lib (INPUT ROWID(oe-rel), OUTPUT d-out).

          END.
        END.

        FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no:

          RUN oe/bol-pre-post.p (ROWID(oe-boll), v-term, YES /* show msg */).
          FIND cust WHERE cust.company = oe-bolh.company
                      AND cust.cust-no = oe-bolh.cust-no NO-LOCK NO-ERROR.
          IF fgreorder-log AND cust.ACTIVE = "E" 
            THEN RUN create-reorder (ROWID(oe-boll)) .  


          IF v-EDIBOLPost-log THEN
          DO:
             &IF DEFINED(useLotNo) NE 0 &THEN lotNo = oe-boll.lot-no. &ELSE
             lotNo = STRING(INT(oe-bolh.ship-id),'99') NO-ERROR.
             IF ERROR-STATUS:ERROR OR INT(oe-bolh.ship-id) > 99 THEN lotNo = ''. ELSE
             lotNo = 'MG' + STRING(INT(oe-bolh.ship-id),'99') + STRING(DAY(oe-boll.bol-date),'99').
             &ENDIF

             FIND FIRST oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ oe-boll.company
                    AND oe-ordl.ord-no EQ oe-boll.ord-no
                    AND oe-ordl.line EQ oe-boll.line NO-ERROR.

             CREATE ediOutFile.
             ASSIGN
               ediOutFile.bolNo = oe-boll.bol-no
               ediOutFile.custNo = oe-bolh.cust-no
               ediOutFile.poNo = oe-boll.po-no
               ediOutFile.poLine = IF AVAIL(oe-ordl) THEN oe-ordl.e-num ELSE 0
               ediOutFile.partNo = IF AVAIL(oe-ordl) THEN oe-ordl.part-no ELSE ''
               ediOutFile.qty = oe-boll.qty
               ediOutFile.lotNo = lotNo
               ediOutFile.bolDate = oe-boll.bol-date
               ediOutFile.relNo = oe-boll.r-no
               ediOutFile.carrier = oe-bolh.carrier
               ediOutFile.trailer = IF oe-bolh.trailer EQ '' THEN '001'
                                    ELSE oe-bolh.trailer.
             RELEASE ediOutFile.
             RELEASE oe-ordl.
          END.
        END. /* each oe-boll */
      END.
    END.
  END.

  RUN oe/oe-bolp3.p (v-term).

  /* close transfer order here */
  RUN oe/closchk.p (0).

  /* wfk - 5/4/12 - run cleanup routine after posting instead of */
  /* fixing the actual problems                                  */

  FOR EACH tt-post TRANSACTION:
    RELEASE oe-bolh.
    DO WHILE NOT AVAIL oe-bolh:
      FIND FIRST oe-bolh EXCLUSIVE WHERE ROWID(oe-bolh) EQ tt-post.row-id
          NO-WAIT NO-ERROR.

      IF AVAIL oe-bolh THEN DO:

        FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no:

          FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ oe-boll.company
              AND oe-ordl.ord-no EQ oe-boll.ord-no
              AND oe-ordl.line EQ oe-boll.line 
          NO-ERROR.
           RUN oe/cleanrel.p (INPUT ROWID(oe-ordl)).        

        END.
      END.
    END.
  END.

  FOR EACH w-ord:
      RUN oe/close.p (w-ord.rec-id, YES).  
      /*
          FIND bf-oe-ord WHERE RECID(bf-oe-ord) EQ w-ord.rec-id NO-ERROR.
      FOR EACH bf-oe-ordl WHERE bf-oe-ordl.company = bf-oe-ord.company
                         AND bf-oe-ordl.ord-no  = bf-oe-ord.ord-no
                       NO-LOCK.
         {util/tmsg.i ""CleanOrdOn"" bf-oe-ordl.ord-no
             bf-oe-ordl.i-no }
         RUN oe/cleanrel.p (INPUT ROWID(bf-oe-ordl)).
      END.
      */
  END.

  IF v-EDIBOLPost-log THEN
  FOR EACH sys-ctrl-shipto NO-LOCK
      WHERE sys-ctrl-shipto.company EQ cocode
        AND sys-ctrl-shipto.name EQ 'EDIBOLPost'
        AND sys-ctrl-shipto.cust-vend EQ YES
        AND sys-ctrl-shipto.log-fld EQ YES:
    CASE sys-ctrl-shipto.char-fld:
      WHEN 'RHEEM' THEN
      FOR EACH ediOutFile WHERE ediOutFile.custNo EQ sys-ctrl-shipto.cust-vend-no
          BREAK BY ediOutFile.bolNo
                BY ediOutFile.carrier
                BY ediOutFile.trailer:
        IF FIRST-OF(ediOutFile.trailer) THEN DO:
          outFile = v-EDIBOLPost-char + '/' +
                    sys-ctrl-shipto.cust-vend-no + '/' +
                    STRING(ediOutFile.bolNo) + '-' +
                    ediOutFile.carrier + '-' +
                    ediOutFile.trailer + '.csv'.
          OUTPUT STREAM ediBOL TO VALUE(outFile) APPEND.
        END. /* first-of */
        PUT STREAM ediBOL UNFORMATTED
          ediOutFile.poNo ','
          ediOutFile.poLine ','
          ediOutFile.partNo ','
          ediOutFile.partNo ','
          ediOutFile.qty ','
          ediOutFile.lotNo ','
          ediOutFile.bolDate ','
          ediOutFile.bolNo ','
          ediOutFile.carrier ','
          ediOutFile.trailer SKIP.
        IF LAST-OF(ediOutFile.trailer) THEN DO:
          PUT STREAM ediBOL UNFORMATTED
              STRING(ediOutFile.relNo)  + '-' +
                     ediOutFile.carrier + '-' +
                     ediOutFile.trailer + '.csv: END' SKIP.
          OUTPUT STREAM ediBOL CLOSE.
        END. /* last-of */
      END. /* each edioutfile, when rheem */
    END CASE.
  END. /* each sys-ctrl-shipto */

  IF VALID-HANDLE(lr-rel-lib) THEN
     DELETE OBJECT lr-rel-lib.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-packing-list C-Win 
PROCEDURE run-packing-list :
/* --------------------------------------------- oe/rep/oe-lad.p 3/94 RM ---- */
/* print bill of ladings                                                      */
/* -------------------------------------------------------------------------- */
  DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-ship-to AS LOG NO-UNDO.

  {sys/form/r-top.i}

  assign
    v-s-bol             = begin_bol#
    v-e-bol             = end_bol#
    v-s-ord             = begin_ord#
    v-e-ord             = end_ord#
    v-s-date             = begin_date
    v-e-date             = end_date
    v-printed           = /*tb_reprint*/ YES
    v-print-pal         = tb_pallet
    v-print-bol         = rd_bolcert EQ "BOL"
    v-print-components  = tb_print-component
    v-print-shipnotes   = tb_print-shipnote
    v-print-dept        = tb_print-dept
    v-ship-inst         = tb_print_ship    
    lv-run-bol          = ""
    lv-run-commercial   = ""
    v-print-unassembled = tb_print-unassemble-component
    v-footer            = tb_footer
    lPerBolLine         = tb_per-bol-line.

  IF ip-sys-ctrl-ship-to THEN
     ASSIGN
        v-s-cust = ip-cust-no
        v-e-cust = ip-cust-no.
  ELSE
     ASSIGN 
        v-s-cust = begin_cust
        v-e-cust = end_cust.

  IF fi_depts:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        v-print-dept = LOGICAL(tb_print-dept:SCREEN-VALUE)
        v-depts = fi_depts:SCREEN-VALUE.

  IF tb_print_ship :HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        v-ship-inst = LOGICAL(tb_print_ship:SCREEN-VALUE) .

  IF tb_per-bol-line :HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        lPerBolLine = LOGICAL(tb_per-bol-line:SCREEN-VALUE) .
  

  /*if td-show-parm then run show-param.*/

  SESSION:SET-WAIT-STATE ("general").

  {sa/sa-sls01.i}

  v-term-id = v-term.

  run build-work ('').
  FIND FIRST report NO-LOCK WHERE report.term-id  = v-term-id NO-ERROR.
  IF NOT AVAIL report THEN LEAVE.

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  IF IS-xprint-form THEN DO:

      CASE rd-dest:
          WHEN 1 THEN DO: 
            IF v-print-fmt = "CCC" OR v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN
              PUT "<PRINTER?><LEFT=" + trim(STRING(7 + d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
            ELSE IF v-print-fmt = "Carded" OR v-print-fmt = "GPI2" THEN
              PUT "<PRINTER?><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
            ELSE IF d-print-fmt-dec > 0 THEN
              PUT "<PRINTER?><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
            ELSE
              PUT "<PRINTER?>".
          END.
          WHEN 2 THEN do:
           IF NOT lBussFormModle THEN do:
               IF v-print-fmt = "CCC" OR v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN
                   PUT "<PREVIEW><LEFT=" + trim(STRING(7 + d-print-fmt-dec)) + "mm><MODAL=NO>" FORMAT "x(120)".
               ELSE IF  v-print-fmt = "Carded" OR v-print-fmt = "GPI2" THEN
                   PUT "<PREVIEW><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm><MODAL=NO>" FORMAT "x(120)".
               ELSE IF d-print-fmt-dec > 0 THEN
                   PUT "<PREVIEW><LEFT=" + trim(string(d-print-fmt-dec)) + "mm><MODAL=NO>" FORMAT "x(120)".
               ELSE
                   PUT "<PREVIEW><MODAL=NO>". 
           END.
           ELSE DO:
             IF v-print-fmt = "CCC" OR v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN
                 PUT "<PREVIEW><LEFT=" + trim(STRING(7 + d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
             ELSE IF v-print-fmt = "Carded" OR v-print-fmt = "GPI2" THEN
                 PUT "<PREVIEW><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
             ELSE IF d-print-fmt-dec > 0 THEN
                 PUT "<PREVIEW><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
             ELSE
                 PUT "<PREVIEW>". 
           END.
          END.  /* when 2*/
          WHEN 4 THEN do:
                ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                IF v-print-fmt = "CCC" OR v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN
                  PUT UNFORMATTED "<PRINTER?><LEFT=" + trim(STRING(4 + d-print-fmt-dec)) + "mm><EXPORT=" Ls-fax-file ",BW>" FORMAT "x(120)".
                ELSE IF v-print-fmt = "Carded" OR v-print-fmt = "GPI2" THEN
                  PUT UNFORMATTED "<PRINTER?><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm><EXPORT=" Ls-fax-file ",BW>" FORMAT "x(120)".
                ELSE IF d-print-fmt-dec > 0 THEN
                  PUT UNFORMATTED "<PRINTER?><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm><EXPORT=" Ls-fax-file ",BW>" FORMAT "x(120)".
                ELSE
                  PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".
          END.
          WHEN 5 THEN do:
              IF v-print-fmt = "Century" THEN /*<PDF-LEFT=5mm><PDF-TOP=10mm>*/
                   PUT "<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=" + trim(STRING(2.5 + d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
              ELSE IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "PremierXFooter" OR v-print-fmt EQ "RFCX"  OR v-print-fmt = "PremierCX" OR v-print-fmt = "PremierPX" THEN
                   PUT "<PREVIEW><FORMAT=LETTER><PDF-EXCLUDE=MS Mincho><PDF-LEFT=" + trim(STRING(5 + d-print-fmt-dec)) + "mm><PDF-TOP=7mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
              ELSE IF v-print-fmt EQ "CCC" OR v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN PUT "<PREVIEW><LEFT=" + trim(STRING(4 + d-print-fmt-dec)) + "mm><PDF-LEFT=" + trim(STRING(2 + d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
              ELSE IF v-print-fmt EQ "Carded" OR v-print-fmt = "GPI2" THEN PUT "<PREVIEW><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm><PDF-LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
              ELSE IF d-print-fmt-dec > 0 THEN PUT "<PREVIEW><LEFT=" + trim(string(d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
              ELSE  PUT "<PREVIEW><PDF-LEFT=2mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
          END.
      END CASE.
  END.

  IF lv-run-bol = "YES" THEN DO:
    ASSIGN
      lXMLOutput = rd-dest EQ iXMLOutput /* rstark 05181205 */
      clXMLOutput = YES /* rstark 05291402 */
      .
    /*  IF v-print-fmt = "1/2 Page" AND rd-dest = 6 THEN DO:
          PUT CONTROL CHR(27) CHR(67) CHR(44).
          RUN VALUE(v-program). 
          PUT CONTROL CHR(18).
      END.
      ELSE DO:
         IF v-program = "oe/rep/cocprempkg.p" THEN
              RUN oe/rep/cocprempkg.p (?).
         IF v-program = "oe/rep/cocloylang.p" THEN
              RUN oe/rep/cocloylang.p (?).
         ELSE IF v-program = "oe/rep/cocprempkgu.p" THEN
              RUN oe/rep/cocprempkgu.p (?).
         ELSE IF v-program = "oe/rep/cocprempkgm.p" THEN
              RUN oe/rep/cocprempkgm.p (?).
         ELSE  RUN VALUE(v-program).
      END.     */

    IF v-print-fmt EQ "Badger"  THEN
        RUN oe/rep/bolbgrpl.p .

    IF v-print-fmt EQ "BadgerSoldTo" THEN do:
        RUN oe/rep/bolbstpl.p . 
    END.

  END.

  /*IF lv-run-commercial = "YES" AND IS-xprint-form THEN
     RUN oerep/runbolci.p. */

  ASSIGN td-pck-lst = NO . 

  for each report where report.term-id eq v-term-id:
      delete report.
  end.

  OUTPUT CLOSE.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  SESSION:SET-WAIT-STATE ("").

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* --------------------------------------------- oe/rep/oe-lad.p 3/94 RM ---- */
/* print bill of ladings                                                      */
/* -------------------------------------------------------------------------- */
  DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-ship-to AS LOG NO-UNDO.
  {sys/form/r-top.i}

  assign
    v-s-bol             = begin_bol#
    v-e-bol             = end_bol#
    v-s-ord             = begin_ord#
    v-e-ord             = end_ord#
    v-s-date             = begin_date
    v-e-date             = end_date
    v-printed           = tb_reprint
    v-print-pal         = tb_pallet
    v-print-bol         = rd_bolcert EQ "BOL"
    v-print-components  = tb_print-component
    v-print-shipnotes   = tb_print-shipnote
    v-print-dept        = tb_print-dept
    v-ship-inst         = tb_print_ship    
    lv-run-bol          = ""
    lv-run-commercial   = ""
    v-sort              = rd_bol-sort EQ "Item #"
    v-print-unassembled = tb_print-unassemble-component 
    v-footer            = tb_footer
    lPerBolLine         = tb_per-bol-line.

  IF ip-sys-ctrl-ship-to THEN
     ASSIGN
        v-s-cust = ip-cust-no
        v-e-cust = ip-cust-no.
  ELSE
     ASSIGN 
        v-s-cust = begin_cust
        v-e-cust = end_cust.

  IF fi_depts:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        v-print-dept = LOGICAL(tb_print-dept:SCREEN-VALUE)
        v-depts = fi_depts:SCREEN-VALUE.

  IF tb_print_ship :HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        v-ship-inst = LOGICAL(tb_print_ship:SCREEN-VALUE) .

  IF tb_per-bol-line :HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        lPerBolLine = LOGICAL(tb_per-bol-line:SCREEN-VALUE) .
  

  /*if td-show-parm then run show-param.*/

  SESSION:SET-WAIT-STATE ("general").

  {sa/sa-sls01.i}

  v-term-id = v-term.

  run build-work ('').
  FIND FIRST report NO-LOCK WHERE report.term-id  = v-term-id NO-ERROR.
  IF NOT AVAIL report THEN LEAVE.

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  IF IS-xprint-form THEN DO:

      CASE rd-dest:
          WHEN 1 THEN do: 
              IF v-print-fmt = "CCC" OR v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN
                  PUT "<PRINTER?><LEFT=" + trim(STRING(7 + d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
              ELSE IF v-print-fmt = "Carded" OR v-print-fmt = "GPI2" THEN
                  PUT "<PRINTER?><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
              ELSE IF d-print-fmt-dec > 0 THEN
                  PUT "<PRINTER?><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
              ELSE  PUT "<PRINTER?>".
          END.
          WHEN 2 THEN do:
           IF NOT lBussFormModle THEN do:
               IF v-print-fmt = "CCC" OR v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN
                   PUT "<PREVIEW><LEFT=" + trim(STRING(7 + d-print-fmt-dec)) + "mm><MODAL=NO>" FORMAT "x(120)".
               ELSE IF v-print-fmt = "Carded" OR v-print-fmt = "GPI2" THEN
                   PUT "<PREVIEW><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm><MODAL=NO>" FORMAT "x(120)".
               ELSE IF d-print-fmt-dec > 0 THEN 
                   PUT "<PREVIEW><LEFT=" + trim(string(d-print-fmt-dec)) + "mm><MODAL=NO>" FORMAT "x(120)".
               ELSE PUT "<PREVIEW><MODAL=NO>".
           END.
           ELSE do:
               IF v-print-fmt = "CCC" OR  v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN
                   PUT "<PREVIEW><LEFT=" + trim(STRING(7 + d-print-fmt-dec)) + "mm>" FORMAT "x(120)". 
               ELSE IF v-print-fmt = "Carded" OR v-print-fmt = "GPI2" THEN
                   PUT "<PREVIEW><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
               ELSE IF d-print-fmt-dec > 0 THEN
                   PUT "<PREVIEW><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
               ELSE
                   PUT "<PREVIEW>".  
           END.
          END. /*when 2*/
          WHEN 4 THEN do:
                ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                IF v-print-fmt = "CCC" OR  v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN
                    PUT UNFORMATTED "<PRINTER?><LEFT=" + trim(STRING(7 + d-print-fmt-dec)) + "mm><EXPORT=" Ls-fax-file ",BW>" FORMAT "x(120)".
                ELSE IF v-print-fmt = "Carded" OR v-print-fmt = "GPI2" THEN
                    PUT UNFORMATTED "<PRINTER?><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm><EXPORT=" Ls-fax-file ",BW>" FORMAT "x(120)".
                ELSE IF d-print-fmt-dec > 0 THEN
                    PUT UNFORMATTED "<PRINTER?><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm><EXPORT=" Ls-fax-file ",BW>" FORMAT "x(120)".
                ELSE PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".
          END.
          WHEN 5 THEN do:
              IF v-print-fmt = "Century" THEN /*<PDF-LEFT=5mm><PDF-TOP=10mm>*/
                   PUT "<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=" + trim(STRING(2.5 + d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
              ELSE IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "PremierXFooter" OR v-print-fmt EQ "RFCX"  OR v-print-fmt = "PremierCX" OR v-print-fmt = "PremierPX" THEN
                   PUT "<PREVIEW><FORMAT=LETTER><PDF-EXCLUDE=MS Mincho><PDF-LEFT=" + trim(STRING(5 + d-print-fmt-dec)) + "mm><PDF-TOP=7mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
              ELSE IF v-print-fmt EQ "CCC" OR  v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN PUT "<PREVIEW><LEFT=" + trim(STRING(4 + d-print-fmt-dec)) + "mm><PDF-LEFT=" + trim(STRING(2 + d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
              ELSE IF v-print-fmt EQ "Carded" OR v-print-fmt = "GPI2" THEN PUT "<PREVIEW><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm><PDF-LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
              ELSE IF d-print-fmt-dec > 0 THEN PUT "<PREVIEW><LEFT=" + trim(string(d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
              ELSE PUT "<PREVIEW><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
          END.
      END CASE.
  END.

  IF lv-run-bol = "YES" THEN DO:
    ASSIGN
      lXMLOutput = rd-dest EQ iXMLOutput /* rstark 05181205 */
      clXMLOutput = YES /* rstark 05291402 */
      .
      IF v-print-fmt = "1/2 Page" AND rd-dest = 6 THEN DO:
          PUT CONTROL CHR(27) CHR(67) CHR(44).
          RUN VALUE(v-program). 
          PUT CONTROL CHR(18).
      END.
      ELSE DO:
         IF v-program = "oe/rep/cocprempkg.p" THEN
              RUN oe/rep/cocprempkg.p (?).
         IF v-program = "oe/rep/cocloylang.p" THEN
              RUN oe/rep/cocloylang.p (?).
         ELSE IF v-program = "oe/rep/cocprempkgu.p" THEN
              RUN oe/rep/cocprempkgu.p (?).
         ELSE IF v-program = "oe/rep/cocprempkgm.p" THEN
              RUN oe/rep/cocprempkgm.p (?).
         ELSE IF v-program = "oe/rep/cocbcert10.p" THEN
              RUN oe/rep/cocbcert10.p (?).
         ELSE IF v-program = "oe/rep/coclanyork.p" THEN
              RUN oe/rep/coclanyork.p (?).
         ELSE RUN VALUE(v-program).
      END.
  END.

  IF lv-run-commercial = "YES" AND IS-xprint-form THEN
     RUN oerep/runbolci.p.

  ASSIGN td-pck-lst = NO .

  for each report where report.term-id eq v-term-id:
            
      IF asnsps-log THEN RUN oe/oe856gen.p (report.rec-id, yes,yes).
      
      delete report.
  end.

  OUTPUT CLOSE.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  SESSION:SET-WAIT-STATE ("").

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-ci C-Win 
PROCEDURE run-report-ci :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER  bf-oe-boll          FOR oe-boll.
DEF VAR     lv-run-bol          AS char NO-UNDO.
DEF VAR     lv-run-commercial   AS char no-undo.
DEF VAR     v-tmp-is-xprint     AS LOG  NO-UNDO.

{sys/form/r-top.i}

assign
  v-s-cust            = begin_cust
  v-e-cust            = end_cust
  v-s-bol             = begin_bol#
  v-e-bol             = end_bol#
  v-s-ord             = begin_ord#
  v-e-ord             = end_ord#
  v-s-date             = begin_date
  v-e-date             = end_date
  v-printed           = tb_reprint
  v-print-pal         = tb_pallet
  v-print-bol         = rd_bolcert EQ "BOL"
  v-print-components  = tb_print-component
  v-print-shipnotes   = tb_print-shipnote
  v-print-dept        = tb_print-dept
  lv-run-bol          = ""
  lv-run-commercial   = ""
  v-tmp-is-xprint     = IS-xprint-form
  is-xprint-form      = YES
  v-print-unassembled = tb_print-unassemble-component
  v-footer            = tb_footer
  lPerBolLine         = tb_per-bol-line.

IF fi_depts:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
   ASSIGN
     v-print-dept = LOGICAL(tb_print-dept:SCREEN-VALUE)
     v-depts = fi_depts:SCREEN-VALUE.
  IF tb_print_ship :HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        v-ship-inst = LOGICAL(tb_print_ship:SCREEN-VALUE) .

  IF tb_per-bol-line :HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        lPerBolLine = LOGICAL(tb_per-bol-line:SCREEN-VALUE) .

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.*/

SESSION:SET-WAIT-STATE ("general").

{sa/sa-sls01.i}

v-term-id = v-term.

build-work:
FOR EACH oe-bolh
   WHERE oe-bolh.company EQ cocode
     AND oe-bolh.bol-no  GE v-s-bol
     AND oe-bolh.bol-no  LE v-e-bol
     AND oe-bolh.cust-no GE v-s-cust
     AND oe-bolh.cust-no LE v-e-cust 
     and oe-bolh.bol-date  ge v-s-date
     and oe-bolh.bol-date  le v-e-date
     AND oe-bolh.printed EQ v-printed
     AND oe-bolh.posted  EQ tb_posted
     AND CAN-FIND (FIRST oe-boll
                   WHERE oe-boll.company EQ oe-bolh.company
                     AND oe-boll.b-no    EQ oe-bolh.b-no
                     AND oe-boll.ord-no  GE v-s-ord
                     AND oe-boll.ord-no  LE v-e-ord)
    USE-INDEX post.

  IF NOT oe-ctrl.p-bol THEN
  FOR EACH oe-boll
     WHERE oe-boll.company EQ oe-bolh.company
       AND oe-boll.bol-no  EQ oe-bolh.bol-no
       AND CAN-FIND (FIRST oe-ord
                     WHERE oe-ord.company EQ oe-boll.company
                       AND oe-ord.ord-no  EQ oe-boll.ord-no
                       AND (oe-ord.stat    EQ "H" OR oe-ord.priceHold))
      NO-LOCK:
    NEXT build-work.
  END.

  /* update loadtag status - Bill of lading task#: 10190414 */
  IF NOT oe-bolh.printed THEN
  FOR EACH bf-oe-boll NO-LOCK
     WHERE bf-oe-boll.company EQ oe-bolh.company 
       AND bf-oe-boll.b-no    EQ oe-bolh.b-no
       AND bf-oe-boll.tag     NE "",
     FIRST loadtag
     WHERE loadtag.company    EQ bf-oe-boll.company
       AND loadtag.item-type  EQ NO
       AND loadtag.tag-no     EQ bf-oe-boll.tag USE-INDEX tag:

    loadtag.sts = "Bill of Lading".
  END.

  FIND FIRST sys-ctrl-shipto NO-LOCK
       WHERE sys-ctrl-shipto.company      EQ oe-bolh.company
         AND sys-ctrl-shipto.name         EQ "BOLFMT"
         AND sys-ctrl-shipto.cust-vend    EQ YES
         AND sys-ctrl-shipto.cust-vend-no EQ oe-bolh.cust-no
         AND sys-ctrl-shipto.ship-id      EQ oe-bolh.ship-id NO-ERROR.

  IF NOT CAN-FIND(FIRST report WHERE
     report.term-id = v-term-id AND
     report.rec-id  = RECID(oe-bolh)) THEN
     DO:
        CREATE report.

        ASSIGN
         report.term-id  = v-term-id
         report.key-01   = oe-bolh.cust-no
         report.key-02   = oe-bolh.ship-id
         report.rec-id   = RECID(oe-bolh)
         report.key-09   = STRING(oe-bolh.printed,"REVISED/ORIGINAL")
         report.key-03   = IF AVAIL sys-ctrl-shipto AND NOT sys-ctrl-shipto.log-fld  THEN "C" /*commercial invoice only*/
                           ELSE IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld  THEN "B" /*commercial invoice and bol both*/
                           ELSE                                                                "N" /*BOL only*/ 
         report.key-04   = IF AVAIL sys-ctrl-shipto THEN    sys-ctrl-shipto.char-fld ELSE "".     
     END.

  IF lv-run-bol        EQ "" AND report.key-03 <> "C" THEN lv-run-bol        = "YES" .
  IF lv-run-commercial EQ "" AND report.key-03 <> "N" THEN lv-run-commercial = "YES".
end.

v-lines-per-page = lines-per-page.
/*
IF rd-dest = 2 AND is-xprint-form THEN PUT "<PREVIEW>".   
ELSE IF is-xprint-form AND rd-dest = 1 THEN PUT "<PRINTER?>".
*/
/*IF IS-xprint-form THEN */  DO:
    CASE rd-dest:
        WHEN 1 THEN do:
             IF v-print-fmt EQ "CCC" OR  v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN
                 PUT  "<PRINTER?><LEFT=" + trim(STRING(4 + d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
             ELSE IF v-print-fmt EQ "Carded" OR v-print-fmt = "GPI2" THEN
                 PUT  "<PRINTER?><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
             ELSE IF d-print-fmt-dec > 0 THEN
                 PUT "<PRINTER?><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
             ELSE
                PUT  "<PRINTER?>".
        END.
        WHEN 2 THEN do:
           IF NOT lBussFormModle THEN do:
               IF v-print-fmt EQ "CCC" OR  v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN
                   PUT "<PREVIEW><LEFT=" + trim(STRING(4 + d-print-fmt-dec)) + "mm><MODAL=NO>" FORMAT "x(120)". 
               ELSE IF v-print-fmt EQ "Carded" OR v-print-fmt = "GPI2" THEN
                   PUT "<PREVIEW><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm><MODAL=NO>" FORMAT "x(120)".
               ELSE IF d-print-fmt-dec > 0 THEN
                   PUT "<PREVIEW><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm><MODAL=NO>" FORMAT "x(120)".
               ELSE
                   PUT "<PREVIEW><MODAL=NO>".  
           END.
           ELSE do:
               IF v-print-fmt EQ "CCC" OR  v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN
                   PUT "<PREVIEW><LEFT=" + trim(STRING(4 + d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
               ELSE IF v-print-fmt EQ "Carded" OR v-print-fmt = "GPI2" THEN
                   PUT "<PREVIEW><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
               ELSE IF d-print-fmt-dec > 0 THEN
                   PUT "<PREVIEW><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm>" FORMAT "x(120)".
               ELSE
                   PUT "<PREVIEW>".
           END.
        END. /* when 2 */
        WHEN  4 THEN do:
              ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
              IF v-print-fmt EQ "CCC" OR  v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN
                  PUT UNFORMATTED "<PRINTER?><LEFT=" + trim(STRING(4 + d-print-fmt-dec)) + "mm><EXPORT=" Ls-fax-file ",BW>" FORMAT "x(120)".
              ELSE IF v-print-fmt EQ "Carded" OR v-print-fmt = "GPI2" THEN
                  PUT UNFORMATTED "<PRINTER?><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm><EXPORT=" Ls-fax-file ",BW>" FORMAT "x(120)".
              ELSE IF d-print-fmt-dec > 0 THEN
                  PUT UNFORMATTED "<PRINTER?><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm><EXPORT=" Ls-fax-file ",BW>" FORMAT "x(120)".
              ELSE PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".
        END.
        WHEN 5 THEN do:
            IF v-print-fmt = "Century" THEN /*<PDF-LEFT=5mm><PDF-TOP=10mm>*/
                 PUT "<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=" + trim(STRING(2.5 + d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
                 ELSE IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "PremierXFooter" OR v-print-fmt EQ "RFCX"  OR v-print-fmt = "PremierCX" OR v-print-fmt = "PremierPX" THEN
                   PUT "<PREVIEW><FORMAT=LETTER><PDF-EXCLUDE=MS Mincho><PDF-LEFT=" + trim(STRING(5 + d-print-fmt-dec)) + "mm><PDF-TOP=7mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
            ELSE IF v-print-fmt EQ "CCC" OR v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCCW" OR v-print-fmt EQ "CCC2" THEN PUT "<PREVIEW><LEFT=" + trim(STRING(4 + d-print-fmt-dec)) + "mm><PDF-LEFT=" + trim(STRING(2 + d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
            ELSE IF v-print-fmt EQ "Carded" OR v-print-fmt = "GPI2" THEN PUT "<PREVIEW><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm><PDF-LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
            ELSE IF d-print-fmt-dec > 0 THEN
                PUT "<PREVIEW><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
            ELSE PUT "<PREVIEW><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
        END.
    END CASE.
END.

IF lv-run-commercial = "YES" THEN DO:
   RUN oerep/runbolci.p.
END.


OUTPUT CLOSE.

DO WITH FRAME {&FRAME-NAME}:
  case rd-dest :
    when 1 then run output-to-printer(INPUT "", INPUT NO).
    when 2 then run output-to-screen(INPUT "", INPUT NO).
    when 3 then run output-to-file(INPUT "", INPUT NO).
    when 4 then do:

       {custom/asifax.i       &type         = "Customer"
                              &begin_cust   = begin_cust 
                              &end_cust     = begin_cust
                              &fax-subject  = "BOL"
                              &fax-body     = "BOL"
                              &fax-file     = list-name }
    END. 
    when 5 then do:
       IF is-xprint-form THEN DO:
          RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").                            
          {custom/asimail2.i  &TYPE         = "Customer"
                              &group-title  = 'r-bolprt.' /* v-prgmname */
                              &begin_cust   = begin_cust
                              &end_cust     = end_cust
                              &mail-subject = "BOL"
                              &mail-body    = "BOL"
                              &mail-file    = lv-pdf-file + ".pdf" }
       END.
       ELSE DO:
           {custom/asimailr2.i &TYPE        = "Customer"
                              &group-title  = 'r-bolprt.' /* v-prgmname */
                              &begin_cust   = begin_cust
                              &end_cust     = end_cust
                              &mail-subject = current-window:title
                              &mail-body    = CURRENT-WINDOW:TITLE
                              &mail-file    = list-name }

       END.
    END. 
    WHEN 6 THEN RUN output-to-port(INPUT "", INPUT NO).
  end case.
END.

for each report where report.term-id eq v-term-id:
    delete report.
end.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").
IS-xprint-form = v-tmp-is-xprint.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-mail C-Win 
PROCEDURE run-report-mail :
/* --------------------------------------------------------*/

  DEFINE INPUT PARAM icCustNo AS CHAR NO-UNDO.
  DEFINE INPUT PARAM ic2ndKey AS CHAR NO-UNDO.
  DEFINE INPUT PARAM iiMode   AS INTE NO-UNDO.
  DEFINE INPUT PARAM iLprinted AS LOG NO-UNDO.

  {sys/form/r-top.i}

  assign
    v-s-cust            = icCustNo
    v-e-cust            = icCustNo
    v-s-bol             = begin_bol#
    v-e-bol             = end_bol#
    v-s-ord             = begin_ord#
    v-e-ord             = end_ord#
    v-s-date             = begin_date
    v-e-date             = end_date
    v-printed           = iLprinted
    v-print-pal         = tb_pallet
    v-print-bol         = rd_bolcert EQ "BOL"
    v-print-components  = tb_print-component
    v-print-shipnotes   = tb_print-shipnote
    v-print-dept        = tb_print-dept
    lv-run-bol          = ""
    lv-run-commercial   = ""
    v-print-unassembled = tb_print-unassemble-component
    v-footer            = tb_footer
    lPerBolLine         = tb_per-bol-line.

  IF fi_depts:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        v-print-dept = LOGICAL(tb_print-dept:SCREEN-VALUE)
        v-depts = fi_depts:SCREEN-VALUE.

   IF tb_print_ship :HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        v-ship-inst = LOGICAL(tb_print_ship:SCREEN-VALUE) .

   IF tb_per-bol-line :HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
     ASSIGN
        lPerBolLine = LOGICAL(tb_per-bol-line:SCREEN-VALUE) .
    

  /*if td-show-parm then run show-param.*/

  SESSION:SET-WAIT-STATE ("general").

  {sa/sa-sls01.i}

  v-term-id = v-term.

  run build-work (ic2ndKey).
  FIND FIRST report NO-LOCK WHERE report.term-id  = v-term-id NO-ERROR.
  IF NOT AVAIL report THEN LEAVE.

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  IF NOT vcBOLNums > '' THEN RETURN.

  status default 'Processing... Please wait.'.

  if can-find (first report where report.term-id eq v-term-id) then
  do:

    IF IS-xprint-form THEN DO:
      IF v-print-fmt = "Century"                     /*<PDF-LEFT=5mm><PDF-TOP=10mm>*/
        THEN PUT "<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=" + trim(STRING(2.5 + d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>" FORM "x(180)".
        ELSE IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "PremierXFooter" OR v-print-fmt EQ "RFCX"  OR v-print-fmt = "PremierCX" OR v-print-fmt = "PremierPX" THEN
                   PUT "<PREVIEW><FORMAT=LETTER><PDF-EXCLUDE=MS Mincho><PDF-LEFT=" + trim(STRING(5 + d-print-fmt-dec)) + "mm><PDF-TOP=7mm><PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>" FORM "x(180)".
        ELSE IF v-print-fmt EQ "Prystup-Excel" THEN PUT "<PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>" FORM "x(180)".
        ELSE IF v-print-fmt EQ "CCC" OR v-print-fmt EQ "CCCWPP" OR v-print-fmt EQ "CCC2" THEN PUT "<PREVIEW><LEFT=" + trim(STRING(4 + d-print-fmt-dec)) + "mm><PDF-LEFT=" + trim(STRING(2 + d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>" FORM "x(180)".
        ELSE IF v-print-fmt EQ "Carded" OR v-print-fmt = "GPI2" THEN PUT "<PREVIEW><LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm><PDF-LEFT=" + trim(STRING(6 + d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>" FORM "x(180)".
        ELSE IF d-print-fmt-dec > 0 THEN
            PUT "<PREVIEW><LEFT=" + trim(STRING(d-print-fmt-dec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>" FORM "x(180)".
        ELSE PUT "<PREVIEW><PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>" FORM "x(180)".
    END.

    IF lv-run-bol = "YES" THEN DO:

      IF v-print-fmt = "1/2 Page" AND rd-dest = 6 THEN DO:
          PUT CONTROL CHR(27) CHR(67) CHR(44). 
          RUN value(v-program). 
          PUT CONTROL CHR(18).
      END.

      ELSE
      DO:
         IF v-program EQ "oe/rep/cocprempkg.p" THEN
            RUN oe/rep/cocprempkg.p (?).
         ELSE IF v-program EQ "oe/rep/cocloylang.p" THEN
            RUN oe/rep/cocloylang.p (?).
         ELSE IF v-program EQ "oe/rep/cocbcert10.p" THEN
            RUN oe/rep/cocbcert10.p (?).
         ELSE IF v-program EQ "oe/rep/coclanyork.p" THEN
            RUN oe/rep/coclanyork.p (?).
         ELSE
            RUN value(v-program).
      END.
    END.

    IF lv-run-commercial = "YES" AND 
       IS-xprint-form THEN DO:
       RUN oerep/runbolci.p.
    END.
  END.

  else do:
    MESSAGE 'No records to process. Job aborted.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    return.
  end.

  for each report 
     where report.term-id eq v-term-id:
    delete report.
  end.

  OUTPUT CLOSE.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-mail-uni-xl C-Win 
PROCEDURE send-mail-uni-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAM icIdxKey   AS CHAR NO-UNDO.
  DEFINE INPUT PARAM icRecType  AS CHAR NO-UNDO.    

  DEFINE VARIABLE vcSubject   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMailBody  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.

  IF SEARCH (lv-pdf-file) EQ ? THEN DO:
    MESSAGE 'Attachment File: ' lv-pdf-file ' is missing.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  ASSIGN  vcSubject   = "CofC for BOL: " + vcBOLNums 
          vcMailBody  = "Please review attached CofC for BOL #: " + vcBOLNums.

  RUN custom/xpmail2.p   (input   icRecType,
                          input   'R-BOLPRT.',
                          input   lv-pdf-file,
                          input   icIdxKey,
                          input   vcSubject,
                          input   vcMailBody,
                          OUTPUT  vcErrorMsg).
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
  DEFINE INPUT PARAMETER icShipId AS CHARACTER NO-UNDO.  

  DEFINE VARIABLE vcSubject   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMailBody  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.

  ASSIGN  vcSubject   = "BOL: " + vcBOLNums + '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
          vcSubject   = IF tb_reprint THEN '[REPRINT] ' + vcSubject ELSE vcSubject
          vcMailBody  = "Please review attached Bill of Lading(s) for BOL #: " + vcBOLNums.

  IF icShipId <> "" THEN icRecType = icRecType + "|" + icShipId. /* cust# + shipto */     

  RUN custom/xpmail2.p   (input   icRecType,
                          input   'R-BOLPRT.',
                          input   lv-pdf-file,
                          input   icIdxKey,
                          input   vcSubject,
                          input   vcMailBody,
                          OUTPUT  vcErrorMsg).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMail-2 C-Win 
PROCEDURE SendMail-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAM icIdxKey   AS CHAR NO-UNDO.
  DEFINE INPUT PARAM icRecType  AS CHAR NO-UNDO.    

  DEFINE VARIABLE vcSubject   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMailBody  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.

  IF SEARCH (list-name) NE ? THEN DO:
    IF NOT list-name MATCHES '*.txt' THEN DO:
      OS-RENAME VALUE (SEARCH (list-name)) VALUE (SEARCH (list-name) + '.txt').
      IF OS-ERROR NE 0 THEN DO:
        MESSAGE 'Failed to rename your temp file.'  SKIP
                'OS Error: ' OS-ERROR
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
      ELSE list-name = list-name + '.txt'.
    END.
  END.

  ELSE DO:
    MESSAGE 'Attachment File: ' list-name ' is missing.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  ASSIGN  vcSubject   = "BOL: " + vcBOLNums + '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
          vcSubject   = IF tb_reprint THEN '[REPRINT] ' + vcSubject ELSE vcSubject
          vcMailBody  = "Please review attached Bill of Lading(s) for BOL #: " + vcBOLNums.

  RUN custom/xpmail2.p   (input   icRecType,
                          input   'R-BOLPRT.',
                          input   list-name,
                          input   icIdxKey,
                          input   vcSubject,
                          input   vcMailBody,
                          OUTPUT  vcErrorMsg).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetBOLForm C-Win 
PROCEDURE SetBOLForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER icFormName AS CHAR NO-UNDO.

   IF rd_bolcert EQ "BOL" THEN
   DO:
      {sys/inc/bolform.i}
   END.
   ELSE
   DO:
      CASE icFormName:
         WHEN "Xprint" OR WHEN "bolcert 1" OR WHEN "bolcert 2" THEN
            ASSIGN 
               is-xprint-form = YES
               v-program      = "oe/rep/cocxprnt.p".

         WHEN "Prystup" THEN 
            ASSIGN 
               is-xprint-form = YES
               v-program      = "oe/rep/cocpryst.p".

         WHEN "PremierPkg" THEN
            ASSIGN
               is-xprint-form = YES
               v-program = "oe/rep/cocprempkg.p".

         WHEN "LoyLang" THEN
            ASSIGN
               is-xprint-form = YES
               v-program = "oe/rep/cocloylang.p".

         WHEN "PremierPkgU" THEN
            ASSIGN
               is-xprint-form = YES
               v-program = "oe/rep/cocprempkgu.p".

         WHEN "PremierPkgM" THEN
            ASSIGN
               is-xprint-form = YES
               v-program = "oe/rep/cocprempkgm.p".

         WHEN "" OR WHEN "Brick" THEN
            ASSIGN 
               is-xprint-form = NO
               v-program      = "oe/rep/cocbrick.p".

         WHEN "ACPI" THEN
              ASSIGN 
                is-xprint-form = NO
                v-program = "oe/rep/cocacpi.p".

         WHEN "CCC" OR WHEN "CCCWPP" OR WHEN "CCC2" OR WHEN "CCCW" THEN
              ASSIGN 
                is-xprint-form = NO
                v-program = "oe/rep/cocccc.p".
         WHEN "BOLCERT10" THEN
            ASSIGN
               is-xprint-form = YES
               v-program = "oe/rep/cocbcert10.p".
         WHEN "LancoYork" THEN
            ASSIGN
               is-xprint-form = YES
               v-program = "oe/rep/coclanyork.p".

         OTHERWISE
            ASSIGN
               is-xprint-form = NO
               v-program = "oe/rep/cocuni.p".
     END CASE.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetVariables C-Win 
PROCEDURE SetVariables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN 
      v-print-bol = rd_bolcert EQ "BOL".

   IF rd-dest = 5 THEN
   DO:
     IF NOT v-print-bol AND v-coc-fmt EQ "Unipak-XL" THEN
        lv-pdf-file = init-dir + "\cofc.pdf".
     ELSE
        lv-pdf-file = init-dir + "\BOL".
   END.
   ELSE
      IF NOT v-print-bol AND v-coc-fmt EQ "Unipak-XL" THEN
         lv-pdf-file = init-dir + "\cofc.pdf".
      ELSE
         lv-pdf-file = init-dir + "\BOL" + string(begin_bol#).
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
  def var parm-fld-list as char no-undo.
  def var parm-lbl-list as char no-undo.
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
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + ",".
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
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
  ( ipField AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE invalidChars AS CHARACTER NO-UNDO INITIAL "~",#".
   DEFINE VARIABLE replaceChars AS CHARACTER NO-UNDO INITIAL "'',".
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE k AS INTEGER NO-UNDO.

   k = NUM-ENTRIES(invalidChars).
   DO i = 1 TO k:
     ipField = REPLACE(ipField,ENTRY(i,invalidChars),ENTRY(i,replaceChars)).
   END.
   RETURN ipField.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

