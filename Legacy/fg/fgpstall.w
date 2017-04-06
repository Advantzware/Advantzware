&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fg\fgpstall.w

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
DEF INPUT PARAMETER ip-post-eom-date AS DATE NO-UNDO.
DEF INPUT PARAMETER ip-run-what AS CHAR NO-UNDO. /* "SETUP" from initial setup (addon/sshoot/sssetups.w),
                                                  else "" */

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

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

DEFINE SHARED VARIABLE choice AS LOG NO-UNDO.

DEF VAR v-fgpostgl AS CHAR NO-UNDO.
def var v-fg-value as dec format "->,>>>,>>9.99".
def var v-msf as dec format ">,>>9.999" extent 6.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF {1} SHARED var v-print-fmt  as char NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR fg-uom-list AS CHAR NO-UNDO.
DEF VAR lv-list-name LIKE list-name EXTENT 2 NO-UNDO.
DEF VAR t-setup AS LOG NO-UNDO.
DEF VAR lInvFrt AS LOG NO-UNDO.
DEF VAR dBillAmt AS DECIMAL NO-UNDO.
DEF VAR lEmailBol AS LOG NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.

DEFINE VARIABLE gv-fgemail AS LOGICAL NO-UNDO INIT ?.

DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd
    FIELD row-id   AS ROWID
    FIELD has-rec  AS LOG INIT NO
    FIELD invoiced AS LOG INIT NO
    FIELD old-tag AS CHAR
    FIELD ret-loc AS CHAR
    FIELD ret-loc-bin AS CHAR.

DEF TEMP-TABLE tt-email NO-UNDO 
    FIELD tt-recid AS RECID
    FIELD job-no LIKE job-hdr.job-no
    FIELD job-no2 LIKE job-hdr.job-no2
    FIELD i-no LIKE itemfg.i-no
    FIELD qty AS INT
    FIELD cust-no AS cha
    INDEX tt-cust IS PRIMARY cust-no DESCENDING .

DEFINE TEMP-TABLE tt-fgemail NO-UNDO
    FIELD i-no    LIKE itemfg.i-no
    FIELD po-no   LIKE oe-ordl.po-no
    FIELD ord-no  LIKE oe-ordl.ord-no
    FIELD qty-rec AS DEC
    FIELD recipient AS CHAR.

DEF TEMP-TABLE tt-posted-items
    FIELD i-no LIKE w-fg-rctd.i-no
    INDEX i-no i-no.

DEF TEMP-TABLE tt-set
    FIELD part-no LIKE fg-set.part-no
    INDEX i1 part-no.

{fg/fullset.i NEW}

{jc/jcgl-sh.i NEW}

{fg/fg-post3.i NEW}

{fg/invrecpt.i NEW}

{sys/ref/fgoecost.i}
DEF TEMP-TABLE tt-inv LIKE w-inv.
RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

DEF STREAM st-email.
DEF STREAM logFile.
DEF STREAM before.
DEF STREAM after.

DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
DEFINE STREAM excel.

DEFINE BUFFER b-fg-rctd FOR fg-rctd.
DEF BUFFER b2-fg-rctd FOR fg-rctd.

{sys/inc/ssfgretc.i}

DO TRANSACTION:
  {sys/inc/fgpost.i}   
END.

{oerep/r-loadtg.i NEW}  /*w-ord for loadtag reprint */

DEF VAR lvReturnChar AS CHAR NO-UNDO.
DEF VAR lvFound AS LOG NO-UNDO.
DEF VAR autofgissue-log AS LOGICAL NO-UNDO.

RUN sys/ref/nk1look.p (cocode, "AUTOFGISSUE", "L", no, no, "", "", 
    Output lvReturnChar, output lvFound).
IF lvFound THEN
    autofgissue-log = LOGICAL(lvReturnChar).

DEF VAR v-loadtag AS cha INIT "ASI" NO-UNDO.
DEF VAR v-mult AS INT NO-UNDO.
DEF VAR v-cas-lab AS LOG NO-UNDO.
DEF VAR v-tags AS DEC NO-UNDO.
def var form_fid        as char no-undo initial "barcode.frm" FORMAT "X(40)".
def var form#           as int  no-undo format "9" initial 3.
def var n               as int no-undo initial 0.
def var char_units      as char no-undo.
def var copy_count      as int no-undo initial 2.
DEF var v-out AS char FORMAT "x(40)" NO-UNDO.

def stream s-form.
def stream s-bar.
DEF var v-po-no-source AS char FORMAT "!" init "R".

def var stx as char format 'x(01)' no-undo initial "~002".
def var etx as char format 'x(01)' no-undo initial "~003".
def var esc as char format 'x(01)' no-undo initial "~033".
def var etb as char format 'x(01)' no-undo initial "~027".
def var cr  as char format 'x(01)' no-undo initial "~015".
def var can as char format 'x(01)' no-undo initial "~030".
def var rs  as char format 'x(01)' no-undo initial "~036".
def var us  as char format 'x(01)' no-undo initial "~037".
DEF VAR tb_16ths AS LOG NO-UNDO.
DEF BUFFER ref-lot-no FOR reftable.
DEF VAR SSLoadTag-log AS LOGICAL NO-UNDO.

RUN sys/ref/nk1look.p (cocode, "SSLoadTag", "L", no, no, "", "", 
    Output lvReturnChar, output lvFound).
IF lvFound THEN
    SSLoadTag-log = LOGICAL(lvReturnChar).
FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "CEMENU"
      NO-LOCK NO-ERROR.
  ASSIGN
   tb_16ths  = AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "Corrware".
DEF VAR v-uid-sec AS LOG NO-UNDO.
DEF VAR v-access-close AS LOG NO-UNDO.
DEF VAR v-access-list AS CHAR NO-UNDO.
DEF VAR v-source-handle AS HANDLE NO-UNDO.

/* Check if authorized to create PO's */
RUN methods/prgsecur.p
    (INPUT "FGPostUID",
     INPUT "ALL", /* based on run, create, update, delete or all */
     INPUT NO,    /* use the directory in addition to the program */
     INPUT NO,    /* Show a message if not authorized */
     INPUT NO,    /* Group overrides user security? */
     OUTPUT v-uid-sec, /* Allowed? Yes/NO */
     OUTPUT v-access-close, /* used in template/windows.i  */
     OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-30 RECT-31 RECT-32 ~
v-post-date begin_fg-r-no end_fg-r-no begin_userid end_userid ldt-from ~
ldt-to begin_job-no end_job-no begin_i-no end_i-no end_whs begin_whs ~
tg-recalc-cost rd_print rd-Itm#Cst# t-receipt rd-ItmPo t-ship rd-UOMJob ~
t-trans t-adj tb_glnum t-ret tb_grndtotal tb_totCstVal tgIssue td-show-parm ~
rd-dest lv-ornt lv-font-no lines-per-page tb_excel tb_runExcel fi_file ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS v-post-date begin_fg-r-no end_fg-r-no ~
begin_userid end_userid ldt-from ldt-to begin_job-no end_job-no begin_i-no ~
end_i-no end_whs begin_whs tg-recalc-cost v-trans-lbl tgl-itemCD rd_print ~
rd-Itm#Cst# t-receipt rd-ItmPo t-ship rd-UOMJob t-trans t-adj tb_glnum ~
t-ret tb_grndtotal tb_totCstVal tgIssue td-show-parm rd-dest lv-ornt ~
lv-font-no lines-per-page lv-font-name tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-act-rel-qty C-Win 
FUNCTION get-act-rel-qty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-prod C-Win 
FUNCTION get-prod RETURNS INTEGER
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-tot-rcv-qty C-Win 
FUNCTION get-tot-rcv-qty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-from-addons C-Win 
FUNCTION is-from-addons RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
  (ipField AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE begin_fg-r-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "From Seq#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "From FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "From Job#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_userid AS CHARACTER FORMAT "X(8)":U 
     LABEL "From Last Updated ID" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whs AS CHARACTER FORMAT "X(5)":U 
     LABEL "From Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_fg-r-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "To Seq#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "To Job#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_userid AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "To Last Updated ID" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_whs AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "To Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-fgpstall.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE ldt-from AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE ldt-to AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

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

DEFINE VARIABLE v-post-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Post Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE v-trans-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Transaction Types" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .91
     FONT 6 NO-UNDO.

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
     SIZE 20 BY 5.71 NO-UNDO.

DEFINE VARIABLE rd-Itm#Cst# AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "FG Item #", 1,
"Customer Part # ", 2
     SIZE 37.2 BY .91 NO-UNDO.

DEFINE VARIABLE rd-ItmPo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item Name", 1,
"P.O. # / Vendor", 2
     SIZE 36.2 BY .91 NO-UNDO.

DEFINE VARIABLE rd-UOMJob AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "UOM", 1,
"Job #", 2
     SIZE 31.6 BY .91 NO-UNDO.

DEFINE VARIABLE rd_print AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Cost", "C",
"Sell Value", "S"
     SIZE 36.4 BY .91 NO-UNDO.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34.4 BY 7.38.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.6 BY 7.38.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42.6 BY 7.38.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 7.38.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 17.38.

DEFINE VARIABLE t-adj AS LOGICAL INITIAL no 
     LABEL "Adjustments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-receipt AS LOGICAL INITIAL no 
     LABEL "Receipts" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-ret AS LOGICAL INITIAL no 
     LABEL "Credit Returns" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-ship AS LOGICAL INITIAL no 
     LABEL "Shipments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-trans AS LOGICAL INITIAL no 
     LABEL "Transfers" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_glnum AS LOGICAL INITIAL no 
     LABEL "Print GL Account Numbers?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE tb_grndtotal AS LOGICAL INITIAL no 
     LABEL "Grand Total" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_totCstVal AS LOGICAL INITIAL no 
     LABEL "Total Cost/Value" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .71 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg-recalc-cost AS LOGICAL INITIAL no 
     LABEL "Recalc Cost From History?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tgIssue AS LOGICAL INITIAL no 
     LABEL "Issue Farm Outs" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE tgl-itemCD AS LOGICAL INITIAL no 
     LABEL "Item Code" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     v-post-date AT ROW 2.91 COL 26 COLON-ALIGNED
     begin_fg-r-no AT ROW 3.86 COL 26 COLON-ALIGNED HELP
          "Enter the From Sequence Number"
     end_fg-r-no AT ROW 3.86 COL 69 COLON-ALIGNED HELP
          "Enter the To Sequence Number"
     begin_userid AT ROW 4.81 COL 26 COLON-ALIGNED HELP
          "Enter the From Created ID"
     end_userid AT ROW 4.81 COL 69 COLON-ALIGNED HELP
          "Enter the To Created ID"
     ldt-from AT ROW 5.76 COL 26 COLON-ALIGNED HELP
          "Enter the From Date"
     ldt-to AT ROW 5.76 COL 69 COLON-ALIGNED HELP
          "Enter the To Date"
     begin_job-no AT ROW 6.71 COL 26 COLON-ALIGNED HELP
          "Enter the From Job Number"
     end_job-no AT ROW 6.71 COL 69 COLON-ALIGNED HELP
          "Enter the To Job Number"
     begin_i-no AT ROW 7.67 COL 26 COLON-ALIGNED HELP
          "Enter the From FG Item Number"
     end_i-no AT ROW 7.67 COL 69 COLON-ALIGNED HELP
          "Enter the To FG Item Number"
     end_whs AT ROW 8.62 COL 69 COLON-ALIGNED HELP
          "Enter the To Warehouse" WIDGET-ID 4
     begin_whs AT ROW 8.67 COL 26 COLON-ALIGNED HELP
          "Enter the from Warehouse" WIDGET-ID 2
     tg-recalc-cost AT ROW 9.57 COL 28 WIDGET-ID 32
     v-trans-lbl AT ROW 11.71 COL 2 NO-LABEL
     tgl-itemCD AT ROW 11.71 COL 38 WIDGET-ID 16
     rd_print AT ROW 11.71 COL 57.2 NO-LABEL
     rd-Itm#Cst# AT ROW 12.57 COL 57.2 NO-LABEL WIDGET-ID 18
     t-receipt AT ROW 12.67 COL 2
     rd-ItmPo AT ROW 13.38 COL 57.2 NO-LABEL WIDGET-ID 26
     t-ship AT ROW 13.52 COL 2
     rd-UOMJob AT ROW 14.33 COL 57.2 NO-LABEL WIDGET-ID 22
     t-trans AT ROW 14.38 COL 2
     t-adj AT ROW 15.29 COL 2
     tb_glnum AT ROW 15.38 COL 57.2
     t-ret AT ROW 16.19 COL 2
     tb_grndtotal AT ROW 16.24 COL 79
     tb_totCstVal AT ROW 16.33 COL 57.2 WIDGET-ID 30
     tgIssue AT ROW 17.05 COL 2 WIDGET-ID 34
     td-show-parm AT ROW 19 COL 29
     rd-dest AT ROW 19.48 COL 2 NO-LABEL
     lv-ornt AT ROW 19.95 COL 29 NO-LABEL
     lv-font-no AT ROW 21.14 COL 32 COLON-ALIGNED
     lines-per-page AT ROW 21.14 COL 60 COLON-ALIGNED
     lv-font-name AT ROW 22.33 COL 32 COLON-ALIGNED NO-LABEL
     tb_excel AT ROW 23.43 COL 51
     tb_runExcel AT ROW 23.43 COL 95 RIGHT-ALIGNED
     fi_file AT ROW 24.38 COL 49 COLON-ALIGNED HELP
          "Enter File Name"
     Btn_OK AT ROW 25.76 COL 22
     Btn_Cancel AT ROW 25.81 COL 57
     "Trans Typ :" VIEW-AS TEXT
          SIZE 12 BY .95 AT ROW 10.76 COL 2 WIDGET-ID 6
     "This Procedure Will Post All Finished Goods Transactions" VIEW-AS TEXT
          SIZE 65 BY .95 AT ROW 1.71 COL 23.8
          FONT 6
     "Print Options :" VIEW-AS TEXT
          SIZE 15 BY .95 AT ROW 10.76 COL 55
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 18.43 COL 1
     "Sort Options :" VIEW-AS TEXT
          SIZE 15 BY .95 AT ROW 10.76 COL 37 WIDGET-ID 14
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 1.8
          BGCOLOR 2 
     RECT-6 AT ROW 18.33 COL 1
     RECT-7 AT ROW 1 COL 1
     RECT-30 AT ROW 10.67 COL 1.6 WIDGET-ID 8
     RECT-31 AT ROW 10.62 COL 35 WIDGET-ID 10
     RECT-32 AT ROW 10.67 COL 54 WIDGET-ID 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 96.4 BY 26.14.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Finished Goods Posting"
         HEIGHT             = 26.48
         WIDTH              = 97.8
         MAX-HEIGHT         = 45.05
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.05
         VIRTUAL-WIDTH      = 256
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
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       Btn_Cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       Btn_OK:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_fg-r-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_userid:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_fg-r-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_userid:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       ldt-from:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       ldt-to:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       t-adj:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-receipt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-ret:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-ship:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-trans:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_glnum:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_grndtotal:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_totCstVal:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tg-recalc-cost:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tgIssue:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tgl-itemCD IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       v-post-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN v-trans-lbl IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       v-trans-lbl:HIDDEN IN FRAME FRAME-A           = TRUE.

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
ON END-ERROR OF C-Win /* Finished Goods Posting */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finished Goods Posting */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME FRAME-A /* Cancel */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:42:46 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME FRAME-A /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.
  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
      NO-LOCK NO-ERROR.
  IF NOT AVAIL period THEN DO WITH FRAME {&FRAME-NAME}:
    MESSAGE "No period exists for this date..."
            VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO v-post-date.
    RETURN NO-APPLY.
  END.
  RUN print-and-post.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:42:46 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
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


&Scoped-define SELF-NAME rd-Itm#Cst#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-Itm#Cst# C-Win
ON VALUE-CHANGED OF rd-Itm#Cst# IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-ItmPo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-ItmPo C-Win
ON VALUE-CHANGED OF rd-ItmPo IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-UOMJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-UOMJob C-Win
ON VALUE-CHANGED OF rd-UOMJob IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-adj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-adj C-Win
ON VALUE-CHANGED OF t-adj IN FRAME FRAME-A /* Adjustments */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-receipt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-receipt C-Win
ON VALUE-CHANGED OF t-receipt IN FRAME FRAME-A /* Receipts */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-ship C-Win
ON VALUE-CHANGED OF t-ship IN FRAME FRAME-A /* Shipments */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-trans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-trans C-Win
ON VALUE-CHANGED OF t-trans IN FRAME FRAME-A /* Transfers */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
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


&Scoped-define SELF-NAME tgIssue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgIssue C-Win
ON VALUE-CHANGED OF tgIssue IN FRAME FRAME-A /* Issue Farm Outs */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-post-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-post-date C-Win
ON LEAVE OF v-post-date IN FRAME FRAME-A /* Post Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


IF ip-run-what EQ "" THEN DO:
  PROCEDURE mail EXTERNAL "xpMail.dll" :
      DEF INPUT PARAM mailTo AS CHAR.
      DEF INPUT PARAM mailsubject AS CHAR.
      DEF INPUT PARAM mailText AS CHAR.
      DEF INPUT PARAM mailFiles AS CHAR.
      DEF INPUT PARAM mailDialog AS LONG.
      DEF OUTPUT PARAM retCode AS LONG.
  END.

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
END.

ELSE DELETE WIDGET {&WINDOW-NAME}.

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

  DO TRANSACTION:
    {sys/inc/closejob.i FGPost}
    {sys/inc/fgpostgl.i}   

    {sys/inc/fgemails.i}
    {sys/inc/postdate.i}
  END.
     {sys/inc/adjustgl.i}
  ASSIGN
   v-fgpostgl  = fgpostgl
   tb_glnum    = v-fgpostgl NE "None" OR v-adjustgl
   tgl-itemCD  = YES   .

  IF ip-run-what EQ "" THEN DO WITH FRAME {&FRAME-NAME}:
    RUN enable_UI.

    {custom/usrprint.i}

    ASSIGN
      begin_userid:SCREEN-VALUE = USERID("nosweat")
      end_userid:SCREEN-VALUE   = USERID("nosweat")
      v-fgpostgl                = fgpostgl
      tb_glnum:SCREEN-VALUE     = STRING(v-fgpostgl NE "None" OR v-adjustgl).


    IF NOT LOGICAL(tb_glnum:SCREEN-VALUE) THEN DISABLE tb_glnum.
    IF NOT v-uid-sec THEN
      DISABLE begin_userid END_userid.

    IF postdate-log THEN
    DO:
      v-post-date:SCREEN-VALUE = STRING(TODAY).
      APPLY "ENTRY" TO begin_fg-r-no.
    END.
    ELSE
    DO:
      v-post-date:SCREEN-VALUE = "".
      APPLY "entry" TO v-post-date.
    END.

    RUN init-values.

    {methods/nowait.i}

    {methods/setButton.i Btn_Cancel "Cancel"} /* added by script _nonAdm1Images.p on 03.28.2017 @ 10:43:31 am */
    {methods/setButton.i Btn_OK "OK"} /* added by script _nonAdm1Images.p on 03.28.2017 @ 10:43:31 am */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:42:46 am */
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
  END.

  ELSE DO:
    /*FIND fg-rctd NO-LOCK WHERE RECID(fg-rctd) EQ INT(ip-run-what) NO-ERROR.

    IF AVAIL fg-rctd THEN
      ASSIGN
       ip-rowid    = ROWID(fg-rctd)
       ip-run-what = fg-rctd.rita-code.

    ELSE ip-rowid = ?.*/

    ASSIGN
     v-post-date  = TODAY
     t-receipt    = ip-run-what EQ "R"
     t-ship       = ip-run-what EQ "S"
     t-trans      = ip-run-what EQ "T"
     t-adj        = NO
     t-ret        = NO
     t-setup      = ip-run-what EQ "SETUP"
     begin_fg-r-no = 0
     end_fg-r-no   = 2147483647
     begin_i-no   = ""
     end_i-no     = "zzzzzzzzzzzzzzzzzzzzzzzz"
     ldt-from     = 01/01/0001
     ldt-to       = 12/31/9999
     begin_job-no = ""
     end_job-no   = "zzzzzzzzzzzzzzzzzzzzzzzz"
     begin_userid = USERID("nosweat")
     end_userid   = USERID("nosweat")
     tb_excel     = NO
     tb_runExcel  = NO .

    /*IF ip-run-what EQ "SETUP" THEN DO :
      ASSIGN
       t-receipt = NO
       t-ship   = NO
       t-trans   = NO
       t-adj     = NO
       t-ret     = NO
       tb_glnum  = NO

       t-receipt:HIDDEN   = YES
       t-ship:HIDDEN     = YES
       t-trans:HIDDEN     = YES
       t-adj:HIDDEN       = YES
       t-ret:HIDDEN       = YES
       tb_glnum:HIDDEN    = YES
       v-trans-lbl:HIDDEN = YES.
    END.*/

    RUN print-and-post.


  END.

/*   APPLY 'close' TO THIS-PROCEDURE. */
/*   RETURN.                          */
/*   QUIT.                            */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-rel-assign-logic C-Win 
PROCEDURE add-rel-assign-logic :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ipr-rel-row AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipi-qty     AS INT   NO-UNDO.
DEF VAR lv-rel-recid AS RECID NO-UNDO.
DEF VAR adm-new-record AS LOG NO-UNDO.
DEF BUFFER bf-oe-ordl FOR oe-ordl.
DEF BUFFER bf-oe-ord FOR oe-ord.
DEF BUFFER bf-oe-rel FOR oe-rel.

/* custom code */
DEF BUFFER s-code FOR reftable.
DEF VAR lv-stat AS CHAR NO-UNDO.
FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ipr-rel-row
                    NO-LOCK NO-ERROR.
IF NOT AVAIL bf-oe-rel THEN
    RETURN.
lv-rel-recid = RECID(bf-oe-rel).

 /* Local assign code from oe/b-ordrel.w, local-assign */
  def var ll-ans as log no-undo.
  def var ldt-ship as date form "99/99/9999" no-undo.
  def buffer bf-rel for oe-rel .
  def var ld-prev-rel-qty as int no-undo.
  def var v-qty-sum as int no-undo.
  DEF VAR ls-key-02 LIKE tt-report.key-02 NO-UNDO.

  DEF BUFFER b-ordl FOR oe-ordl.


  FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ bf-oe-rel.company
                       AND bf-oe-ordl.ord-no  EQ bf-oe-rel.ord-no
                       AND bf-oe-ordl.LINE    EQ bf-oe-rel.LINE
                     NO-LOCK NO-ERROR.
  /* Code placed here will execute PRIOR to standard behavior. */
  if not avail bf-oe-rel and lv-rel-recid <> ? then
     find bf-oe-rel where recid(bf-oe-rel) = lv-rel-recid.
  ld-prev-rel-qty = if adm-new-record then 0 else bf-oe-rel.qty.

  find bf-oe-ord of bf-oe-ordl no-lock.



  FIND b-ordl WHERE ROWID(b-ordl) EQ ROWID(bf-oe-ordl).
  b-ordl.t-rel-qty = b-ordl.t-rel-qty + bf-oe-rel.qty - ld-prev-rel-qty.
  FIND b-ordl WHERE ROWID(b-ordl) EQ ROWID(bf-oe-ordl) NO-LOCK.
  RUN fg/fgitmloc.p (INPUT bf-oe-rel.i-no, INPUT ROWID(bf-oe-rel)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-rel-for-qty C-Win 
PROCEDURE add-rel-for-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipr-rel-row AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipi-qty     AS INT   NO-UNDO.


DEF BUFFER bf-oe-ordl FOR oe-ordl.
DEF BUFFER bf-oe-ord  FOR oe-ord.
DEF BUFFER bf-orig-oe-rel FOR oe-rel.

/* Code added to implement the procedure below */
DEF VAR v-last-shipto AS CHAR.
DEF VAR lv-rel-recid AS RECID.
DEF BUFFER s-code FOR reftable.
DEF VAR lv-cust-x AS CHAR.

def var oereleas-log like sys-ctrl.log-fld no-undo.
def var oereleas-cha like sys-ctrl.char-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OERELEAS"
    no-lock no-error.
IF AVAIL sys-ctrl THEN
assign
 oereleas-log = sys-ctrl.log-fld
 oereleas-cha = sys-ctrl.char-fld.


/* custom code */
FIND bf-orig-oe-rel WHERE ROWID(bf-orig-oe-rel) EQ ipr-rel-row
                    NO-LOCK NO-ERROR.

IF NOT AVAIL bf-orig-oe-rel THEN
    RETURN.
FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ bf-orig-oe-rel.company
                        AND bf-oe-ordl.ord-no  EQ bf-orig-oe-rel.ord-no
                        AND bf-oe-ordl.i-no    EQ bf-orig-oe-rel.i-no
                      NO-LOCK NO-ERROR.

IF NOT AVAIL(bf-oe-ordl) THEN
    RETURN.
FIND bf-oe-ord WHERE bf-oe-ord.company EQ bf-oe-ordl.company
                 AND bf-oe-ord.ord-no  EQ bf-oe-ordl.ord-no
               NO-LOCK NO-ERROR.
IF NOT AVAIL bf-oe-ord THEN
    RETURN.


/* Code from oe/b-ordrel.w, local-create */
  def var v-qty-sum as int no-undo.
  def var v-nxt-r-no as int no-undo.
  def var v-lst-rel as date INIT TODAY no-undo.
  def var v-pct-chg as dec no-undo.
  def var v-ship-id like oe-rel.ship-id no-undo.
  def var v-carrier like oe-rel.carrier no-undo.
  def var v-num-shipto as int no-undo.
  def var v-qty-mod as log no-undo.
  def buffer bf-rel for oe-rel.
  DEF BUFFER bf-cust FOR cust.
  DEF VAR v-first-ship-id AS cha NO-UNDO.


  RUN oe/get-r-no.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).
  FIND FIRST bf-cust WHERE bf-cust.cust-no EQ bf-oe-ord.cust-no NO-LOCK NO-ERROR.
  ASSIGN
    v-ship-id = IF AVAIL oe-rel THEN oe-rel.ship-id ELSE ""
    v-carrier = IF AVAIL oe-rel THEN oe-rel.carrier ELSE ""
    .
  IF AVAIL(bf-cust) AND bf-cust.ACTIVE = "X" AND v-last-shipto GT "" THEN
      v-ship-id = v-last-shipto.
  FIND first bf-rel where bf-rel.company = bf-oe-ord.company
                       and bf-rel.ord-no = bf-oe-ord.ord-no
                       and bf-rel.i-no = bf-oe-ordl.i-no 
                       and bf-rel.LINE = bf-oe-ordl.LINE
                       NO-LOCK NO-ERROR.
  v-first-ship-id = IF AVAIL bf-rel THEN bf-rel.ship-id ELSE "".


  lv-rel-recid = recid(oe-rel).
  assign v-qty-sum  = 0.

  CREATE oe-rel.

  if avail bf-oe-ordl then do:

     FIND FIRST bf-oe-ord OF bf-oe-ordl NO-LOCK.
     for each bf-rel where bf-rel.company = bf-oe-ord.company
                       and bf-rel.ord-no = bf-oe-ord.ord-no
                       and bf-rel.i-no = bf-oe-ordl.i-no 
                       and bf-rel.LINE = bf-oe-ordl.LINE
                       NO-LOCK:
         FIND FIRST s-code
             WHERE s-code.reftable EQ "oe-rel.s-code"
               AND s-code.company  EQ STRING(bf-rel.r-no,"9999999999")
             NO-LOCK NO-ERROR.
         IF NOT AVAIL s-code OR CAN-DO("B,S",s-code.code) THEN
           v-qty-sum = v-qty-sum + bf-rel.qty. 
     end.

/*      if v-qty-sum GE bf-oe-ordl.qty + (bf-oe-ordl.qty * (bf-oe-ordl.over-pct / 100)) then */
/*         message "Total Planned release quantity will exceed the Or" +                     */
/*                         "der quantity + the Underrun %."                                  */
/*                 view-as alert-box warning.                                                */

     find first sys-ctrl where sys-ctrl.company eq cocode
                          and sys-ctrl.name    eq "OECARIER"
               no-lock no-error.
     if not avail sys-ctrl then do:
       create sys-ctrl.
       assign sys-ctrl.company  = cocode
             sys-ctrl.name     = "OECARIER"
             sys-ctrl.descrip  = "Default carrier from Header or ShipTo:"
             sys-ctrl.char-fld = "ShipTo".       
       do while true:
          message "Default Shipping Carrier from Header or Shipto?" update sys-ctrl.char-fld.
          if sys-ctrl.char-fld = "Header" or sys-ctrl.char-fld = "ShipTo" then leave. 
       end.
     end.

     RELEASE shipto.

     IF v-carrier = "" THEN DO:  /* NK1 OECARIER */
        IF sys-ctrl.char-fld EQ "ShipTo" THEN DO:
           FIND FIRST shipto NO-LOCK
           WHERE shipto.company EQ bf-oe-ord.company
             AND shipto.cust-no EQ bf-oe-ord.cust-no
             AND shipto.ship-id EQ v-ship-id NO-ERROR.
           v-carrier = IF AVAIL shipto THEN shipto.carrier ELSE "".
        END.
        ELSE IF sys-ctrl.char-fld EQ "Header" THEN v-carrier = bf-oe-ord.carrier.
     END.

/* wfk - not implemented since there is no tt-report */
/*     IF NOT AVAIL shipto THEN                                                         */
/*     FOR EACH shipto                                                                  */
/*         WHERE shipto.company  EQ cocode                                              */
/*            AND shipto.cust-no EQ (IF lv-cust-x NE ""         AND                     */
/*                                      tt-report.s-code EQ "T" THEN lv-cust-x          */
/*                                                              ELSE bf-oe-ord.cust-no) */
/*         NO-LOCK                                                                      */
/*         BREAK BY shipto.ship-no DESC:                                                */
/*       IF shipto.ship-id EQ bf-oe-ord.cust-no OR LAST(shipto.ship-no) THEN LEAVE.     */
/*     END.                                                                             */

    IF v-carrier EQ "" AND AVAIL shipto THEN v-carrier = shipto.carrier.

    assign oe-rel.company   = cocode
           oe-rel.loc       = locode
           oe-rel.ord-no    = bf-orig-oe-rel.ord-no
           oe-rel.i-no      = bf-orig-oe-rel.i-no
           oe-rel.cust-no   = bf-orig-oe-rel.cust-no
           oe-rel.po-no     = bf-orig-oe-rel.po-no 
           oe-rel.qty       = ipi-qty
           oe-rel.line      = bf-orig-oe-rel.line
           oe-rel.s-comm[1] = bf-orig-oe-rel.s-comm[1]
           oe-rel.s-comm[2] = bf-orig-oe-rel.s-comm[2]
           oe-rel.s-comm[3] = bf-orig-oe-rel.s-comm[3]
           oe-rel.s-name[1] = bf-orig-oe-rel.s-name[1]
           oe-rel.s-name[2] = bf-orig-oe-rel.s-name[2]
           oe-rel.s-name[3] = bf-orig-oe-rel.s-name[3]
           oe-rel.s-pct[1]  = bf-orig-oe-rel.s-pct[1]
           oe-rel.s-pct[2]  = bf-orig-oe-rel.s-pct[2]
           oe-rel.s-pct[3]  = bf-orig-oe-rel.s-pct[3]
           oe-rel.sman[1]   = bf-orig-oe-rel.sman[1]
           oe-rel.sman[2]   = bf-orig-oe-rel.sman[2]
           oe-rel.sman[3]   = bf-orig-oe-rel.sman[3]
           oe-rel.sold-no   = bf-orig-oe-rel.sold-no
           oe-rel.carrier   = bf-orig-oe-rel.carrier
           oe-rel.spare-char-1 = bf-orig-oe-rel.spare-char-1
           oe-rel.r-no      = v-nxt-r-no.


           oe-rel.rel-date = bf-orig-oe-rel.rel-date.


       assign oe-rel.ship-addr[1] = bf-orig-oe-rel.ship-addr[1]
              oe-rel.ship-city    = bf-orig-oe-rel.ship-city
              oe-rel.ship-state   = bf-orig-oe-rel.ship-state
              oe-rel.ship-zip     = bf-orig-oe-rel.ship-zip
              oe-rel.ship-no      = bf-orig-oe-rel.ship-no
              oe-rel.ship-id      = bf-orig-oe-rel.ship-id
              oe-rel.ship-i[1]    = bf-orig-oe-rel.ship-i[1]
              oe-rel.ship-i[2]    = bf-orig-oe-rel.ship-i[2]
              oe-rel.ship-i[3]    = bf-orig-oe-rel.ship-i[3]
              oe-rel.ship-i[4]    = bf-orig-oe-rel.ship-i[4].

       FIND FIRST ref-lot-no WHERE 
         ref-lot-no.reftable EQ "oe-rel.lot-no" AND
         ref-lot-no.company  EQ STRING(oe-rel.r-no,"9999999999")
       NO-ERROR.

  IF AVAIL ref-lot-no THEN
    ASSIGN
       oe-rel.lot-no = ref-lot-no.CODE.


    if oe-rel.qty lt 0 then oe-rel.qty = 0.

    oe-rel.tot-qty = oe-rel.qty.

    if oe-rel.rel-date le v-lst-rel then oe-rel.rel-date = v-lst-rel + 1.

/*     if avail shipto then                                                                                */
/*        assign oe-rel.ship-addr[1] = shipto.ship-addr[1]                                                 */
/*               oe-rel.ship-city    = shipto.ship-city                                                    */
/*               oe-rel.ship-state   = shipto.ship-state                                                   */
/*               oe-rel.ship-zip     = shipto.ship-zip                                                     */
/*               oe-rel.ship-no      = shipto.ship-no                                                      */
/*               oe-rel.ship-id      = IF v-first-ship-id <> "" THEN v-first-ship-id ELSE shipto.ship-id   */
/*               oe-rel.ship-i[1]    = shipto.notes[1]                                                     */
/*               oe-rel.ship-i[2]    = shipto.notes[2]                                                     */
/*               oe-rel.ship-i[3]    = shipto.notes[3]                                                     */
/*               oe-rel.ship-i[4]    = shipto.notes[4].                                                    */
/*     else assign oe-rel.ship-no   = bf-oe-ord.sold-no                                                    */
/*                 oe-rel.ship-id   = IF v-first-ship-id <> "" THEN v-first-ship-id ELSE bf-oe-ord.sold-id */
/*                 oe-rel.ship-i[1] = bf-oe-ord.ship-i[1]                                                  */
/*                 oe-rel.ship-i[2] = bf-oe-ord.ship-i[2]                                                  */
/*                 oe-rel.ship-i[3] = bf-oe-ord.ship-i[3]                                                  */
/*                 oe-rel.ship-i[4] = bf-oe-ord.ship-i[4].                                                 */
    RUN add-rel-assign-logic (INPUT ipr-rel-row, INPUT ipi-qty).

/*     RUN create-report-record-1 (NO, oe-rel.rel-date). */
  end.

/*   else do:                                                                     */
/*     message " Order Line item record is not avail..." view-as alert-box error. */
/*     return error.                                                              */
/*   end.                                                                         */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-comp-tables C-Win 
PROCEDURE build-comp-tables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-fg-rctd FOR fg-rctd.
DEF BUFFER bf-w-fg-rctd FOR w-fg-rctd.

/* Make sure all components are included in w-fg-rctd */
FOR EACH w-fg-rctd WHERE w-fg-rctd.rita-code EQ "R":

    /* Check using fg-rcpts, then reftable since either could be used */
    FOR EACH fg-rcpts 
        WHERE fg-rcpts.company EQ w-fg-rctd.company 
          AND fg-rcpts.linker EQ "fg-rctd: " + STRING(w-fg-rctd.r-no,"9999999999") 
        NO-LOCK.
      FIND FIRST fg-set WHERE fg-set.part-no = fg-rcpts.i-no 
            AND fg-set.set-no EQ w-fg-rctd.i-no
            AND fg-set.company = w-fg-rctd.company NO-LOCK NO-ERROR.
      FIND fg-rctd WHERE fg-rctd.r-no EQ fg-rcpts.r-no
          NO-LOCK NO-ERROR.
      IF AVAIL fg-rctd /* AND AVAIL fg-set */ THEN do:
          FIND FIRST bf-w-fg-rctd WHERE bf-w-fg-rctd.row-id EQ ROWID(fg-rctd)
            NO-LOCK NO-ERROR.
          IF NOT AVAIL bf-w-fg-rctd AND fg-rctd.rita-code EQ w-fg-rctd.rita-code THEN
            RUN build-tables.
      END.
    END.
END.
FOR EACH w-fg-rctd WHERE w-fg-rctd.rita-code EQ "R":
    /* Checking a second time using reftable */
    FOR EACH fg-set 
          WHERE fg-set.company EQ w-fg-rctd.company
            AND fg-set.set-no  EQ w-fg-rctd.i-no
         NO-LOCK:

        FIND FIRST reftable
          WHERE reftable.reftable EQ "fg-rctd.user-id"
            AND reftable.company  EQ w-fg-rctd.company
            AND reftable.loc      EQ STRING(w-fg-rctd.r-no,"9999999999")
          NO-LOCK NO-ERROR.

        IF AVAIL reftable THEN DO:

            FOR EACH bf-fg-rctd 
                  WHERE bf-fg-rctd.company EQ w-fg-rctd.company
                    AND bf-fg-rctd.i-no EQ fg-set.part-no
                    AND bf-fg-rctd.rita-code EQ "R" 
                  NO-LOCK:

                FOR EACH reftable 
                    WHERE reftable.reftable EQ "fg-rctd.user-id"
                      AND reftable.company  EQ bf-fg-rctd.company
                      AND reftable.loc      EQ STRING(bf-fg-rctd.r-no,"9999999999")        
                      AND (reftable.dscr EQ "fg-rctd: " + STRING(w-fg-rctd.r-no, "9999999999") AND reftable.dscr begins "fg-rctd: ")  
                    USE-INDEX loc   NO-LOCK .

                  FIND fg-rctd WHERE ROWID(fg-rctd) EQ ROWID(bf-fg-rctd)
                      NO-LOCK NO-ERROR.
                  IF AVAIL fg-rctd AND fg-rctd.rita-code NE "P" THEN DO:
                      FIND FIRST bf-w-fg-rctd WHERE bf-w-fg-rctd.row-id EQ ROWID(fg-rctd)
                          NO-LOCK NO-ERROR.
                      IF NOT AVAIL bf-w-fg-rctd AND fg-rctd.rita-code EQ w-fg-rctd.rita-code THEN
                        RUN build-tables.
                  END.

                END. /* each reftable */
            END. /* each bf-fg-rctd */
        END. /* avail reftable for header item */
    END. /* each fg-set */

END. /* each w-fg-rctd, check for set components */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-tables C-Win 
PROCEDURE build-tables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
DEF VAR li-max-qty AS INT NO-UNDO.
def var v-part-qty as dec no-undo.
def var v-set-qty as dec no-undo.
DEF VAR v-cost AS DEC NO-UNDO.

DEF BUFFER b-fg-rctd FOR fg-rctd.
DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER use-job FOR reftable.

FIND FIRST itemfg
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ fg-rctd.i-no
    NO-LOCK NO-ERROR.
FIND FIRST loc WHERE loc.company EQ itemfg.company
    AND loc.loc = itemfg.loc
  NO-LOCK NO-ERROR.
IF AVAIL itemfg /*AND AVAIL loc*/ THEN DO TRANSACTION:
  li-max-qty = fg-rctd.t-qty.

  IF li-max-qty GE fg-rctd.t-qty THEN DO:
    CREATE w-fg-rctd.
    BUFFER-COPY fg-rctd TO w-fg-rctd
    ASSIGN
     w-fg-rctd.row-id  = ROWID(fg-rctd)
     w-fg-rctd.has-rec = YES.

    IF ip-run-what EQ "SETUP" THEN
       ASSIGN
       w-fg-rctd.old-tag = fg-rctd.tag
       w-fg-rctd.ret-loc = fg-rctd.loc
       w-fg-rctd.ret-loc-bin = fg-rctd.loc-bin.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-partial C-Win 
PROCEDURE calc-partial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /*find first item finished goods based on the item number*/
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-fg-rctd.i-no
        use-index i-no no-lock no-error.

    if avail itemfg then do:
      find first uom
          where uom.uom  eq itemfg.sell-uom
            and uom.mult ne 0
          no-lock no-error.

      if itemfg.sell-uom begins "L" then
        v-fg-value = 0.

      else
      if itemfg.sell-uom eq "CS" then
        v-fg-value = 0.

      else
      if avail uom then
        v-fg-value = itemfg.sell-price * w-fg-rctd.partial / uom.mult.

      else
        v-fg-value = itemfg.sell-price * w-fg-rctd.partial / 1000.

      if w-fg-rctd.rita-code eq "R" then do:
        if v-msf[1] gt w-fg-rctd.partial * itemfg.t-sqft then
          v-msf[2] = v-msf[2] + (v-msf[1] - (w-fg-rctd.partial * itemfg.t-sqft)).

        v-msf[1] = w-fg-rctd.partial * itemfg.t-sqft.
      end.
    end. /* avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-total C-Win 
PROCEDURE calc-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /*find first item finished goods based on the item number*/
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-fg-rctd.i-no
        use-index i-no no-lock no-error.

    if avail itemfg then do:
      find first uom
          where uom.uom  eq itemfg.sell-uom
            and uom.mult ne 0
          no-lock no-error.

      if itemfg.sell-uom begins "L" then
        v-fg-value = itemfg.sell-price * IF w-fg-rctd.t-qty LT 0 THEN -1 ELSE 1.

      else
      if itemfg.sell-uom eq "CS" then
        v-fg-value = itemfg.sell-price * w-fg-rctd.cases.

      else
      if avail uom then
        v-fg-value = itemfg.sell-price * ((w-fg-rctd.cases * w-fg-rctd.qty-case) / uom.mult).

      else
        v-fg-value = itemfg.sell-price * ((w-fg-rctd.cases * w-fg-rctd.qty-case) / 1000).

      if w-fg-rctd.rita-code eq "R" then do:
        if v-msf[1] gt w-fg-rctd.t-qty * itemfg.t-sqft then
          v-msf[2] = v-msf[2] + (v-msf[1] - ((w-fg-rctd.cases * w-fg-rctd.qty-case) * itemfg.t-sqft)).

        v-msf[1] = (w-fg-rctd.cases * w-fg-rctd.qty-case) * itemfg.t-sqft.
      end.
    end. /* avail itemfg */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Check-Fgemail-Parm C-Win 
PROCEDURE Check-Fgemail-Parm :
/*------------------------------------------------------------------------------
  Purpose:    Get FGEMAILS option and create if it does not exist. 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE BUFFER buf-sys-ctrl FOR sys-ctrl.

 /* Find the FGEMAILS control record. */
 FIND FIRST buf-sys-ctrl NO-LOCK
      WHERE buf-sys-ctrl.company EQ g_company
        AND buf-sys-ctrl.name EQ 'FGEMAILS' NO-ERROR.

 /* If not found, prompt user whether to send these emails or not. */
 IF NOT AVAILABLE buf-sys-ctrl THEN DO:
       MESSAGE 'System Parameter FGEMAILS Does Not Exist' SKIP
         'Do you wish to send Customer Service Emails for Out of Stock Inventory?'
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
         UPDATE l-fgemail AS LOGICAL.

       /* Create the record. */
       CREATE buf-sys-ctrl.
       ASSIGN buf-sys-ctrl.company = g_company
              buf-sys-ctrl.name = 'FGEMAILS'
              buf-sys-ctrl.descrip = 'Customer Service Emails for Out of Stock Inventory'
              buf-sys-ctrl.int-fld = IF l-fgemail = YES THEN 1 ELSE 0.
 END.

 /* If found (or created), save the email option for later. */
 IF AVAILABLE buf-sys-ctrl THEN
     ASSIGN gv-fgemail = (IF buf-sys-ctrl.int-fld = 1 THEN YES ELSE NO).

 RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-loadtag C-Win 
PROCEDURE create-loadtag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT-OUTPUT PARAM io-tag-no AS INT NO-UNDO.
  DEF INPUT PARAM ip-total-unit LIKE w-ord.total-unit NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-phy-count-proc C-Win 
PROCEDURE create-phy-count-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-rno AS INT NO-UNDO.

   DEF BUFFER b-fg-bin FOR fg-bin.

   CREATE b2-fg-rctd.

   FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
   IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN
      lv-rno = b-fg-rctd.r-no.

   FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
   IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN
      lv-rno = fg-rcpth.r-no.

   DO WHILE TRUE:
      lv-rno = lv-rno + 1.
      IF CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no) OR
         CAN-FIND(FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd) THEN
         NEXT.
      LEAVE.
   END.

   /*task 06101005*/
   IF w-fg-rctd.rita-code EQ "I" THEN
      ASSIGN
         w-fg-rctd.job-no = ""
         w-fg-rctd.job-no2 = 0
         w-fg-rctd.cost = 0
         w-fg-rctd.std-cost = 0
         w-fg-rctd.ext-cost = 0.

   assign
      b2-fg-rctd.company = cocode
      b2-fg-rctd.r-no    = lv-rno
      b2-fg-rctd.rita-code = "C"
      b2-fg-rctd.s-num = 0
      b2-fg-rctd.rct-date = today
      b2-fg-rctd.trans-time = TIME 
      b2-fg-rctd.tag = w-fg-rctd.old-tag
      b2-fg-rctd.loc = w-fg-rctd.ret-loc
      b2-fg-rctd.loc-bin = w-fg-rctd.ret-loc-bin
      b2-fg-rctd.i-no = w-fg-rctd.i-no
      b2-fg-rctd.i-name = w-fg-rctd.i-name
      b2-fg-rctd.job-no = w-fg-rctd.job-no
      b2-fg-rctd.job-no2 = w-fg-rctd.job-no2
      b2-fg-rctd.t-qty = w-fg-rctd.inv-no
      b2-fg-rctd.cases = w-fg-rctd.cases
      b2-fg-rctd.cases-unit = w-fg-rctd.cases-unit
      b2-fg-rctd.qty-case = w-fg-rctd.qty-case
      b2-fg-rctd.std-cost = w-fg-rctd.std-cost
      b2-fg-rctd.cust-no  = w-fg-rctd.cust-no
      b2-fg-rctd.cost     = w-fg-rctd.cost
      b2-fg-rctd.cost-uom = w-fg-rctd.cost-uom
      b2-fg-rctd.ext-cost = w-fg-rctd.ext-cost
      b2-fg-rctd.tot-wt   = w-fg-rctd.tot-wt  .

   IF b2-fg-rctd.t-qty NE w-fg-rctd.t-qty AND
      b2-fg-rctd.qty-case NE 0 THEN
      ASSIGN
         b2-fg-rctd.cases = TRUNC(b2-fg-rctd.t-qty / b2-fg-rctd.qty-case,0)
         b2-fg-rctd.partial = b2-fg-rctd.t-qty - (b2-fg-rctd.cases * b2-fg-rctd.qty-case).

   FIND FIRST b-fg-bin 
      WHERE b-fg-bin.company EQ b2-fg-rctd.company
        AND b-fg-bin.i-no    EQ b2-fg-rctd.i-no
        AND b-fg-bin.job-no  EQ b2-fg-rctd.job-no
        AND b-fg-bin.job-no2 EQ b2-fg-rctd.job-no2
        AND b-fg-bin.loc     EQ b2-fg-rctd.loc
        AND b-fg-bin.loc-bin EQ b2-fg-rctd.loc-bin
        AND b-fg-bin.tag     EQ b2-fg-rctd.tag
        AND b-fg-bin.cust-no EQ b2-fg-rctd.cust-no
      NO-LOCK NO-ERROR.

  IF AVAIL b-fg-bin THEN
     ASSIGN
        b2-fg-rctd.ext-cost = b2-fg-rctd.t-qty /
                           (IF b-fg-bin.pur-uom EQ "M" THEN 1000 ELSE 1) *
                           b-fg-bin.std-tot-cost
        b2-fg-rctd.cost     = b2-fg-rctd.ext-cost / b2-fg-rctd.t-qty
        b2-fg-rctd.cost-uom = b-fg-bin.pur-uom.

  IF b2-fg-rctd.ext-cost EQ ? THEN b2-fg-rctd.ext-cost = 0.
  IF b2-fg-rctd.cost     EQ ? THEN b2-fg-rctd.cost = 0.

   FIND FIRST loadtag WHERE
        loadtag.company = g_company AND
        loadtag.item-type = NO AND
        loadtag.tag-no = b2-fg-rctd.tag
        NO-LOCK NO-ERROR.

   IF AVAIL loadtag  AND
      CAN-FIND(FIRST fg-bin WHERE
      fg-bin.company EQ cocode AND
      fg-bin.i-no    EQ b2-fg-rctd.i-no AND
      fg-bin.tag     EQ b2-fg-rctd.tag AND
      fg-bin.job-no  EQ b2-fg-rctd.job-no AND
      fg-bin.job-no2 EQ b2-fg-rctd.job-no2 AND
      (fg-bin.loc    NE b2-fg-rctd.loc OR
       fg-bin.loc-bin NE b2-fg-rctd.loc-bin)
       USE-INDEX tag) AND
       (loadtag.loc <> b2-fg-rctd.loc OR 
        loadtag.loc-bin <> b2-fg-rctd.loc-bin) THEN 
        RUN crt-transfer.

   RELEASE b2-fg-rctd.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-text-file C-Win 
PROCEDURE create-text-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i AS INT NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-text AS cha NO-UNDO.
  DEF VAR v-dept-note AS cha FORM "x(80)" EXTENT 18 NO-UNDO.
  DEF VAR lv-middlesex-job AS CHAR FORMAT "x(9)" NO-UNDO.
  DEF VAR lv-middlesex-po AS CHAR FORMAT "x(9)" NO-UNDO.
  DEF VAR lv-tag-no AS INT NO-UNDO.
  DEF VAR lv-how-many-tags AS INT NO-UNDO.

  /* gdm - 10160905*/
  DEF VAR v-fgdsc1 LIKE itemfg.part-dscr1 NO-UNDO.
  DEF VAR v-fgdsc2 LIKE itemfg.part-dscr2 NO-UNDO.
  DEF VAR v-fgdsc3 LIKE itemfg.part-dscr3 NO-UNDO.
  DEF VAR cRFIDTag AS cha NO-UNDO.
  DEF var v-job AS char FORMAT "x(9)" NO-UNDO.
  DEF VAR v-count AS INT NO-UNDO.

  FIND FIRST w-ord NO-ERROR.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company eq gcompany
        AND sys-ctrl.name    eq "LOADTAG"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = gcompany
     sys-ctrl.name     = "LOADTAG"
     sys-ctrl.descrip  = "Special Load tag print options, e.g. barcode printer"
     sys-ctrl.char-fld = "ASI".
    MESSAGE "System control record NOT found.  Please enter the load tag option"
            UPDATE sys-ctrl.char-fld.
    FIND CURRENT sys-ctrl NO-LOCK.
  END.

  ASSIGN v-loadtag = sys-ctrl.char-fld
         v-mult    = sys-ctrl.int-fld
         v-cas-lab = sys-ctrl.log-fld
         v-tags    = sys-ctrl.dec-fld.

   IF v-loadtag = "TRIAD" THEN DO:
        if form_fid > "" then do:   /* download the form file into the printer ~*/
          input stream s-form from value(form_fid) no-echo.
          _form: do while true:
                readkey stream s-form.
            if lastkey < 0 then leave _form.
              put stream s-bar CONTROL chr(lastkey).
          end.
          input stream s-form close.
        end.

        FOR EACH w-ord:
           v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99").
           IF v-job BEGINS "-" or v-job = ? /* 9901 CAH */
                THEN v-job = string(W-ORD.ORD-NO).   /* 9812 CAH in case blank */
           find first itemfg where itemfg.company = cocode
                    and itemfg.i-no = w-ord.i-no no-lock no-error.
        IF w-ord.total-tags gt -1 THEN DO:
          DO i = 1 TO (w-ord.total-tags + 1):
            /* select the form */
            put stream s-bar control stx esc "E" string(form#) ",1" can etx.
            /* 9901 CAH: done above ... 
            /* clear the variable data fields */
            put stream s-bar control stx can etx.
            */
            char_units = (if i <= w-ord.total-tags 
            then string(w-ord.total-unit) else "    ").  
            def var char_date as char format 'x(10)' no-undo.
            char_date = string(today,"99/99/9999").
            /* 9901 CAH: Only room for 19 chars in the standard 48 pt font */
            if length(w-ord.ship-name) > 19
            then w-ord.ship-name = substring(w-ord.ship-name,1,19).

            def var vcFGItem as char no-undo.
            vcFGItem = 
                if avail itemfg then itemfg.i-no else w-ord.i-no.
           do n = copy_count to 1 by -1:
            /* send the variable data to the printer */
            put stream s-bar unformatted
                stx w-ord.cust-po-no    cr etx
                stx w-ord.cust-po-no    cr etx
                stx w-ord.cust-part-no  cr etx
                stx w-ord.cust-part-no  cr etx
                stx char_units          cr etx
                stx char_units          cr etx
                stx char_date           cr etx
                stx v-job               cr etx
                stx w-ord.ord-qty       cr etx /* 9902 CAH was total-unit */
                stx string(i)           cr etx /* 08.20 was n */
                stx string(w-ord.total-tags + 1) cr etx /* 08.20 was copy_count */
                stx w-ord.ship-name     cr etx
                stx vcFGItem            cr etx.
            /* issue the print command */    
            put stream s-bar control     
                stx rs "1" us "1" etb etx.
           end.
          end.   /* tag count loop */
        end.  /* non zero */  
        END.    /* each w-ord */
      /*  {sys/inc/close.i "" "stream s-bar"} */
        OUTPUT CLOSE.
    END.    /* TRIAD INTERMEC BARCODE PRINT ROUTINE */
    ELSE
    DO:
      OUTPUT TO VALUE(v-out).
      PUT UNFORMATTED
          "CUSTOMER,ORDNUMBER,JOBNUMBER,ITEM,CUSTPARTNO,CUSTPONO,PCS,BUNDLE,TOTAL," +
          "SHIPCODE,SHIPNAME,SHIPADD1,SHIPADD2,SHIPCITY,SHIPSTATE,SHIPCOUNTRY,SHIPZIP," +
          "SOLDCODE,SOLDNAME,SOLDADD1,SOLDADD2,SOLDCITY,SOLDSTATE,SOLDCOUNTRY,SOLDZIP," +
          "INAME,DUEDATE,RELDATE,UPCNO,LENGTH,WIDTH,DEPTH,FLUTE,TEST,VENDOR,GROSSWGT," +
          "TAREWGT,NETWGT,SHEETWGT,UOM,STYLE,STYLEDESC,RELLOTNO,MIDDLESEXJOBNUMBER,MIDDLESEXCUSTPONO,"
          "TAG#,PARTIAL,CASECODE,SN1,SN2,SN3,SN4,SN5,SN6,SN7,SN8,PONO,DN1,DN2,DN3,DN4,"+
          "DN5,DN6,DN7,DN8,DN9,DN10,EST#,ORDDESC1,ORDDESC2".
      IF LOOKUP(v-loadtag,"ASI,SSLABEL") GT 0 THEN
         PUT UNFORMATTED ",COUNTER#,RFIDTag".

      PUT UNFORMATTED ",DUEDATEJOBLINE,DUEDATEJOB,LINE#,UnitWt,PalletWt,FGdesc1,FGdesc2,FGdesc3,FG Lot#".

      PUT SKIP.
      FOR EACH w-ord:

        IF tb_16ths THEN
          ASSIGN
           w-ord.box-len = ROUND((w-ord.box-len - TRUNC(w-ord.box-len,0)) / 6.25,2) +
                           TRUNC(w-ord.box-len,0)
           w-ord.box-wid = ROUND((w-ord.box-wid - TRUNC(w-ord.box-wid,0)) / 6.25,2) +
                           TRUNC(w-ord.box-wid,0)
           w-ord.box-dep = ROUND((w-ord.box-dep - TRUNC(w-ord.box-dep,0)) / 6.25,2) +
                           TRUNC(w-ord.box-dep,0).

        ASSIGN
        lv-text = ""
        v-dept-note = ""

        /* gdm - 10160905 */
        v-fgdsc1 = ""
        v-fgdsc2 = ""
        v-fgdsc3 = "".

        find first itemfg where itemfg.company eq cocode
                            and itemfg.i-no    eq w-ord.i-no no-lock no-error.
        if avail itemfg THEN DO:        
           ASSIGN w-ord.net-wt   = itemfg.weight-100 * w-ord.total-unit / 100
                  w-ord.sheet-wt = itemfg.weight-100 / 100 
                  w-ord.cust-part-no = itemfg.part-no.
/*
           FOR EACH tt-formtext:
               DELETE tt-formtext.
           END.
           FOR EACH notes NO-LOCK WHERE notes.rec_key = itemfg.rec_key
                                     AND notes.note_code = "SN" :
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
               IF  i <= 8 THEN v-dept-note[i] = tt-formtext.tt-text.      
           END.
*/
           /* gdm - 101610905 */
           ASSIGN v-fgdsc1 = itemfg.part-dscr1
                  v-fgdsc2 = itemfg.part-dscr2
                  v-fgdsc3 = itemfg.part-dscr3.

        END.  /* avail itemfg */
    /*    IF tb_dept-note THEN DO:
           lv-text = "".
           FOR EACH tt-formtext:
               DELETE tt-formtext.
           END.

           IF w-ord.ord-no NE 0 THEN
              FOR EACH job-hdr NO-LOCK
                  WHERE job-hdr.company EQ cocode
                    AND job-hdr.ord-no  EQ w-ord.ord-no 
                    AND job-hdr.job-no  EQ w-ord.job-no
                    AND job-hdr.job-no2 EQ w-ord.job-no2
                  BREAK BY job-hdr.job
                        BY job-hdr.job-no
                        BY job-hdr.job-no2:
                 IF LAST-OF(job-hdr.job-no2) THEN
                 FOR EACH job NO-LOCK
                     WHERE job.company EQ job-hdr.company
                       AND job.job     EQ job-hdr.job
                       AND job.job-no  EQ job-hdr.job-no
                       AND job.job-no2 EQ job-hdr.job-no2,
                     EACH notes NO-LOCK
                     WHERE notes.rec_key EQ job.rec_key
                       AND CAN-DO(v-dept-list,notes.note_code):
                    lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
                 END.
              END.
           IF lv-text NE "" THEN DO:
              DO li = 1 TO 10:
                 CREATE tt-formtext.
                 ASSIGN tt-line-no = li
                        tt-length  = 80.
              END.
              RUN custom/formtext.p (lv-text).
              i = 8.           
              FOR EACH tt-formtext:
                  i = i + 1.
                  IF i <= 18 THEN v-dept-note[i] = tt-formtext.tt-text.      
              END.
           END.
        END. /* tb_dept-note*/
  */
        ASSIGN
        w-ord.gross-wt = w-ord.net-wt + w-ord.tare-wt
        v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99").
        IF v-job BEGINS "-" THEN v-job = "".
        ASSIGN
         lv-middlesex-po  = SUBSTR(TRIM(w-ord.job-no),1,6)
         lv-middlesex-job = IF lv-middlesex-job EQ "" THEN "" ELSE
                            "%MX" +
                            FILL("0",6 - LENGTH(TRIM(lv-middlesex-job))) +
                            TRIM(lv-middlesex-job)
         lv-middlesex-po  = SUBSTR(TRIM(w-ord.cust-po-no),1,6)
         lv-middlesex-po  = IF lv-middlesex-po EQ "" THEN "" ELSE
                            "BNJ" +
                            FILL("0",6 - LENGTH(TRIM(lv-middlesex-po))) +
                            TRIM(lv-middlesex-po).

        IF w-ord.total-tags gt 0 THEN DO:
          lv-how-many-tags =  IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 OR w-ord.total-tags = 1 THEN w-ord.total-tags
                              ELSE (w-ord.total-tags - 1).
          DO i = 1 TO (lv-how-many-tags * w-ord.mult):
             /* loadtags generation */
            /* IF i MOD w-ord.mult = 1 OR i = 1 OR w-ord.mult = 1  THEN DO:
                IF i = 1 THEN lv-tag-no = i.
                RUN create-loadtag (INPUT-OUTPUT lv-tag-no, w-ord.total-unit).
             END.
             */
             PUT UNFORMATTED "~""  removeChars(w-ord.cust-name)  "~","
              w-ord.ord-no  ","
              "~""  v-job  "~","
              "~""  caps(removeChars(w-ord.i-no))  FORM "x(15)" "~","
              "~""  removeChars(w-ord.cust-part-no) "~","
              "~""  removeChars(w-ord.cust-po-no)  "~","
              w-ord.pcs  ","
              w-ord.bundle  ","
              w-ord.total-unit FORM ">>>>>>>9" ","
              "~""  removeChars(w-ord.ship-code)  "~","
              "~""  removeChars(w-ord.ship-name)  "~","
              "~""  removeChars(w-ord.ship-add1)  "~","
              "~""  removeChars(w-ord.ship-add2)  "~","
              "~""  removeChars(w-ord.ship-city)  "~","
              "~""  removeChars(w-ord.ship-state) "~","
              "~""  removeChars(w-ord.ship-ctry)  "~","
              "~""  removeChars(w-ord.ship-zip)   "~","
              "~""  removeChars(w-ord.sold-code)  "~","
              "~""  removeChars(w-ord.sold-name)  "~","
              "~""  removeChars(w-ord.sold-add1)  "~","
              "~""  removeChars(w-ord.sold-add2)  "~","
              "~""  removeChars(w-ord.sold-city)  "~","
              "~""  removeChars(w-ord.sold-state) "~","
              "~""  removeChars(w-ord.sold-ctry)  "~","
              "~""  removeChars(w-ord.sold-zip)   "~","
              "~""  removeChars(w-ord.i-name) FORMAT "X(30)"  "~","
              "~""  w-ord.due-date  "~","
              "~""  w-ord.rel-date  "~","
              "~""  w-ord.upc-no  "~","
              "~""  w-ord.box-len FORMAT ">>>9.99<<<" "~","
              "~""  w-ord.box-wid FORMAT ">>>9.99<<<" "~","
              "~""  w-ord.box-dep FORMAT ">>>9.99<<<" "~","
              "~""  w-ord.flute  "~","
              "~""  w-ord.test  "~","
              "~""  w-ord.vendor  "~","
              w-ord.gross-wt  ","
              w-ord.tare-wt  ","
              w-ord.net-wt  ","
              w-ord.sheet-wt  ","
              "~""  w-ord.uom  "~","
              "~""  removeChars(w-ord.style) "~","
              "~""  removeChars(w-ord.style-desc) "~","
              "~""  removeChars(w-ord.rel-lot#) "~","
              "~""  lv-middlesex-job  "~","
              "~""  lv-middlesex-po  "~","
              "~""  loadtag.tag-no "~"," 
              "~""  loadtag.partial "~","
              "~""  w-ord.cas-no  "~","
              "~""  removeChars(v-dept-note[1]) "~","
              "~""  removeChars(v-dept-note[2]) "~","
              "~""  removeChars(v-dept-note[3]) "~","
              "~""  removeChars(v-dept-note[4]) "~","
              "~""  removeChars(v-dept-note[5]) "~","
              "~""  removeChars(v-dept-note[6]) "~","
              "~""  removeChars(v-dept-note[7]) "~","
              "~""  removeChars(v-dept-note[8]) "~","
              w-ord.po-no ","
              "~""  removeChars(v-dept-note[9]) "~","
              "~""  removeChars(v-dept-note[10]) "~","
              "~""  removeChars(v-dept-note[11]) "~","
              "~""  removeChars(v-dept-note[12]) "~","
              "~""  removeChars(v-dept-note[13]) "~","
              "~""  removeChars(v-dept-note[14]) "~","
              "~""  removeChars(v-dept-note[15]) "~","
              "~""  removeChars(v-dept-note[16]) "~","   
              "~""  removeChars(v-dept-note[17]) "~","
              "~""  removeChars(v-dept-note[18]) "~","
              "~""  removeChars(w-ord.est-no) "~","
              "~""  removeChars(w-ord.ord-desc1)    "~","
              "~""  removeChars(w-ord.ord-desc2)    "~","
              .
             IF LOOKUP(v-loadtag,"ASI,SSLABEL") GT 0 THEN DO:

                FIND FIRST rfidtag OF loadtag NO-LOCK NO-ERROR.
                cRFIDTag = IF AVAIL rfidtag THEN rfidtag.rfidtag ELSE "".
                PUT UNFORMATTED "~"" SUBSTR(loadtag.tag-no,16,5) "~"," 
                               "~"" cRFIDTag "~"," .
             END.
             PUT UNFORMATTED 
                "~"" w-ord.due-date-jobhdr "~"," 
                "~"" w-ord.due-date-job "~","
             /* gdm - 08130804 */
                "~"" w-ord.linenum "~","
             /* gdm - 07170905 */
                "~"" w-ord.unit-wt  "~","
                "~"" w-ord.pallt-wt  "~","          

             /* gdm - 10160905 */
                "~"" removeChars(v-fgdsc1) "~","
                "~"" removeChars(v-fgdsc2) "~","
                "~"" removeChars(v-fgdsc3) "~","
                "~"" removeChars(w-ord.lot) "~",".

             PUT SKIP.

          end.
          IF lookup(v-loadtag,"SSLABEL,CentBox") = 0 THEN
          do v-count = 1 to w-ord.mult: /* for partial print */
                /* loadtags generation */
            /* IF v-count EQ 1 THEN RUN create-loadtag (INPUT-OUTPUT lv-tag-no, 0).
            */
             PUT UNFORMATTED "~""  removeChars(w-ord.cust-name)  "~","
              w-ord.ord-no  ","
              "~""  v-job  "~","
              "~""  caps(removeChars(w-ord.i-no))  FORM "x(15)" "~","
              "~""  removeChars(w-ord.cust-part-no)  "~","
              "~""  removeChars(w-ord.cust-po-no)  "~","
              w-ord.pcs  ","
              w-ord.bundle  ", ,"
              "~""  removeChars(w-ord.ship-code)  "~","
              "~""  removeChars(w-ord.ship-name)  "~","
              "~""  removeChars(w-ord.ship-add1)  "~","
              "~""  removeChars(w-ord.ship-add2)  "~","
              "~""  removeChars(w-ord.ship-city)  "~","
              "~""  removeChars(w-ord.ship-state) "~","
              "~""  removeChars(w-ord.ship-ctry)  "~","
              "~""  removeChars(w-ord.ship-zip)   "~","
              "~""  removeChars(w-ord.sold-code)  "~","
              "~""  removeChars(w-ord.sold-name)  "~","
              "~""  removeChars(w-ord.sold-add1)  "~","
              "~""  removeChars(w-ord.sold-add2)  "~","
              "~""  removeChars(w-ord.sold-city)  "~","
              "~""  removeChars(w-ord.sold-state) "~","
              "~""  removeChars(w-ord.sold-ctry)  "~","
              "~""  removeChars(w-ord.sold-zip)   "~","
              "~""  removeChars(w-ord.i-name) FORMAT "X(30)"  "~","
              "~""  w-ord.due-date  "~","
              "~""  w-ord.rel-date  "~","
              "~""  w-ord.upc-no  "~","
              "~""  w-ord.box-len FORMAT ">>>9.99<<<" "~","
              "~""  w-ord.box-wid FORMAT ">>>9.99<<<" "~","
              "~""  w-ord.box-dep FORMAT ">>>9.99<<<" "~","
              "~""  w-ord.flute  "~","
              "~""  w-ord.test  "~","
              "~""  w-ord.vendor  "~","
              w-ord.gross-wt  ","
              w-ord.tare-wt  ","
              w-ord.net-wt  ","
              w-ord.sheet-wt  ","
              "~""  w-ord.uom  "~","
              "~""  removeChars(w-ord.style) "~","
              "~""  removeChars(w-ord.style-desc) "~","
              "~""  removeChars(w-ord.rel-lot#) "~","
              "~""  lv-middlesex-job  "~","
              "~""  lv-middlesex-po  "~","
              "~""  loadtag.tag-no "~"," 
              "~""  loadtag.partial "~"," 
              "~""  w-ord.cas-no  "~","
              "~""  removeChars(v-dept-note[1]) "~","
              "~""  removeChars(v-dept-note[2]) "~","
              "~""  removeChars(v-dept-note[3]) "~","
              "~""  removeChars(v-dept-note[4]) "~","
              "~""  removeChars(v-dept-note[5]) "~","
              "~""  removeChars(v-dept-note[6]) "~","
              "~""  removeChars(v-dept-note[7]) "~","
              "~""  removeChars(v-dept-note[8]) "~","
              w-ord.po-no ","
              "~""  removeChars(v-dept-note[9]) "~","
              "~""  removeChars(v-dept-note[10]) "~","
              "~""  removeChars(v-dept-note[11]) "~","
              "~""  removeChars(v-dept-note[12]) "~","
              "~""  removeChars(v-dept-note[13]) "~","
              "~""  removeChars(v-dept-note[14]) "~","
              "~""  removeChars(v-dept-note[15]) "~","
              "~""  removeChars(v-dept-note[16]) "~","      
              "~""  removeChars(v-dept-note[17]) "~","
              "~""  removeChars(v-dept-note[18]) "~","
              "~""  removeChars(w-ord.est-no) "~","
              "~""  removeChars(w-ord.ord-desc1)    "~","
              "~""  removeChars(w-ord.ord-desc2)    "~","
              .   
             IF LOOKUP(v-loadtag,"ASI,SSLABEL") GT 0 THEN
               PUT UNFORMATTED "~"" SUBSTR(loadtag.tag-no,16,5) "~",".

             /* gdm - 11040801 */
             PUT UNFORMATTED
                "~"" w-ord.linenum "~","
             /* gdm - 07170905 */

                "~"" w-ord.unit-wt  "~","
                "~"" w-ord.pallt-wt  "~","
             /* gdm - 10160905 */

                "~"" removeChars(v-fgdsc1) "~","
                "~"" removeChars(v-fgdsc2) "~","
                "~"" removeChars(v-fgdsc3) "~","
                "~"" removeChars(w-ord.lot) "~",".

             PUT SKIP.
          end.
        end.
        delete w-ord.
      end.
      output close.
    end.    /* NOT TRIAD */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-w-ord C-Win 
PROCEDURE create-w-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-rel-date AS DATE NO-UNDO.
   DEF BUFFER b-job FOR job.
   DEF BUFFER b-job-hdr FOR job-hdr.

   FIND FIRST company WHERE company.company = loadtag.company NO-LOCK NO-ERROR.
   FIND FIRST itemfg WHERE itemfg.company = loadtag.company
                       AND itemfg.i-no = loadtag.i-no NO-LOCK NO-ERROR.
   FIND FIRST oe-ord WHERE oe-ord.company = loadtag.company
                       AND oe-ord.ord-no = loadtag.ord-no NO-LOCK NO-ERROR.
   IF AVAIL oe-ord THEN 
       FIND FIRST oe-ordl WHERE oe-ordl.company = loadtag.company
                           AND oe-ordl.ord-no = loadtag.ord-no
                           AND oe-ordl.i-no = loadtag.i-no NO-LOCK NO-ERROR.
   IF AVAIL oe-ord AND AVAIL oe-ordl THEN DO:

      FIND FIRST cust WHERE cust.company = loadtag.company
                        AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.

      FIND FIRST b-job NO-LOCK WHERE b-job.company = loadtag.company
                                 AND b-job.job-no  = loadtag.job-no
                                 AND b-job.job-no2 = loadtag.job-no2  NO-ERROR.
      IF AVAIL b-job THEN
         FIND FIRST b-job-hdr WHERE b-job-hdr.company EQ b-job.company
                                AND b-job-hdr.job     EQ b-job.job
                                AND b-job-hdr.job-no  EQ b-job.job-no
                                AND b-job-hdr.job-no2 EQ b-job.job-no2
                                AND b-job-hdr.i-no    EQ loadtag.i-no NO-LOCK NO-ERROR.

      CREATE w-ord.
      ASSIGN w-ord.ord-no      = loadtag.ord-no
            w-ord.job-no       = loadtag.job-no
            w-ord.job-no2      = loadtag.job-no2
            w-ord.cust-no      = oe-ord.cust-no
            w-ord.cust-name    = oe-ord.cust-name
            w-ord.i-no         = loadtag.i-no
            w-ord.cust-part-no = oe-ordl.part-no
            w-ord.ord-qty      = loadtag.qty
            w-ord.po-no        = oe-ordl.po-no-po
            w-ord.i-name       = loadtag.i-name
            w-ord.due-date     = if oe-ord.due-date ne ? then
                                   oe-ord.due-date
                                 else
                                 if oe-ordl.req-date ne ? then
                                   oe-ordl.req-date
                                 else today
            w-ord.est-no       = oe-ordl.est-no
            w-ord.form-no      = oe-ordl.form-no
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            w-ord.dont-run-set = oe-ordl.is-a-component
            w-ord.ord-desc1    = oe-ordl.part-dscr1
            w-ord.ord-desc2    = oe-ordl.part-dscr2
            w-ord.sold-code    = oe-ord.sold-id
            w-ord.sold-name    = oe-ord.sold-name
            w-ord.sold-add1    = oe-ord.sold-add[1]
            w-ord.sold-add2    = oe-ord.sold-add[2]
            w-ord.sold-city    = oe-ord.sold-city
            w-ord.sold-state   = oe-ord.sold-state
            w-ord.sold-zip     = oe-ord.sold-zip
            w-ord.linenum      = oe-ordl.e-num
            w-ord.lot          = loadtag.misc-char[2].

      IF AVAIL b-job-hdr THEN
         w-ord.due-date-jobhdr = IF b-job-hdr.due-date <> ? THEN STRING(b-job-hdr.due-date, "99/99/9999") ELSE "".
      IF AVAIL b-job THEN
         w-ord.due-date-job = IF b-job.due-date <> ? THEN STRING(b-job.due-date, "99/99/9999") ELSE "".

      RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                        OUTPUT w-ord.rel-date,
                        OUTPUT w-ord.rel-lot#).

      IF AVAIL itemfg THEN
         ASSIGN w-ord.upc-no  = itemfg.upc-no
             w-ord.box-len = itemfg.l-score[50]
             w-ord.box-wid = itemfg.w-score[50]
             w-ord.box-dep = itemfg.d-score[50]
             w-ord.flute   = itemfg.flute
             w-ord.test    = itemfg.test
             w-ord.pcs     = loadtag.qty-case
             w-ord.bundle  = loadtag.case-bundle
             w-ord.style   = itemfg.style.

      IF w-ord.style NE "" THEN
      DO:
         FIND FIRST style WHERE
              style.company EQ cocode AND
              style.style EQ w-ord.style
              NO-LOCK NO-ERROR.

         IF AVAIL style THEN
         DO:
            w-ord.style-desc = style.dscr.
            RELEASE style.
         END.
      END.

      FIND FIRST shipto WHERE shipto.company eq cocode
            AND shipto.cust-no eq oe-ord.cust-no
            AND shipto.ship-id eq oe-ord.cust-no
            USE-INDEX ship-id NO-LOCK NO-ERROR.
      IF AVAIL shipto THEN
         ASSIGN
            w-ord.ship-name  = shipto.ship-name
            w-ord.ship-add1  = shipto.ship-add[1]
            w-ord.ship-add2  = shipto.ship-add[2]
            w-ord.ship-city  = shipto.ship-city
            w-ord.ship-state = shipto.ship-state
            w-ord.ship-zip   = shipto.ship-zip.

          IF NOT AVAIL eb AND AVAIL itemfg AND itemfg.est-no NE "" THEN
          FIND FIRST eb
              WHERE eb.company  EQ itemfg.company
                AND eb.est-no   EQ itemfg.est-no
                AND eb.stock-no EQ itemfg.i-no
              NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
            ASSIGN
             w-ord.flute  = eb.flute
             w-ord.test   = eb.test
             w-ord.pcs    = eb.cas-cnt
             w-ord.bundle = eb.cas-pal
             w-ord.cas-no = eb.cas-no.

          ASSIGN w-ord.total-tags = 1
            w-ord.ord-qty = loadtag.qty 
            w-ord.pcs = loadtag.qty-case
            w-ord.bundle = loadtag.case-bundle
            w-ord.partial =loadtag.partial
            w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial .      
   END.  /* avail oe-ord*/
   ELSE IF loadtag.job-no <> "" THEN DO:
      FIND FIRST job NO-LOCK WHERE job.company = loadtag.company
                               AND job.job-no = loadtag.job-no
                               AND job.job-no2 = loadtag.job-no2  NO-ERROR.
      IF AVAIL job THEN
         FIND FIRST job-hdr WHERE job-hdr.company EQ job.company
                AND job-hdr.job     EQ job.job
                AND job-hdr.job-no  EQ job.job-no
                AND job-hdr.job-no2 EQ job.job-no2
                AND job-hdr.i-no    EQ loadtag.i-no NO-LOCK NO-ERROR.
      IF AVAIL job-hdr THEN DO:

         FIND FIRST cust WHERE cust.company eq cocode
                          AND cust.cust-no eq job-hdr.cust-no NO-LOCK NO-ERROR.
         FIND FIRST itemfg WHERE itemfg.company eq cocode
                            AND itemfg.i-no    eq job-hdr.i-no NO-LOCK NO-ERROR.

         CREATE w-ord.
         ASSIGN
            w-ord.ord-no       = job-hdr.ord-no
            w-ord.job-no       = job-hdr.job-no
            w-ord.job-no2      = job-hdr.job-no2
            w-ord.cust-no      = cust.cust-no
            w-ord.cust-name    = cust.name
            w-ord.i-no         = job-hdr.i-no
            w-ord.ord-qty      = job-hdr.qty
            w-ord.due-date     = job.start-date
            w-ord.est-no       = job.est-no
            w-ord.form-no      = job-hdr.frm
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            w-ord.lot          = loadtag.misc-char[2].

          IF AVAIL itemfg THEN
             ASSIGN
                w-ord.cust-part-no = itemfg.part-no
                w-ord.style        = itemfg.style
                w-ord.i-name       = itemfg.i-name
                w-ord.upc-no       = itemfg.upc-no
                w-ord.upc-no       = itemfg.upc-no
                w-ord.box-len      = itemfg.l-score[50]
                w-ord.box-wid      = itemfg.w-score[50]
                w-ord.box-dep      = itemfg.d-score[50].

          IF w-ord.style NE "" THEN
          DO:
             FIND FIRST style WHERE
                  style.company EQ cocode AND
                  style.style EQ w-ord.style
                  NO-LOCK NO-ERROR.

             IF AVAIL style THEN
             DO:
                w-ord.style-desc = style.dscr.
                RELEASE style.
             END.
          END.

          FIND FIRST shipto
              WHERE shipto.company eq cocode
                AND shipto.cust-no eq job-hdr.cust-no
                AND shipto.ship-id eq job-hdr.cust-no
              USE-INDEX ship-id NO-LOCK NO-ERROR.
          IF AVAIL shipto THEN
          ASSIGN
            w-ord.ship-name  = shipto.ship-name
            w-ord.ship-add1  = shipto.ship-add[1]
            w-ord.ship-add2  = shipto.ship-add[2]
            w-ord.ship-city  = shipto.ship-city
            w-ord.ship-state = shipto.ship-state
            w-ord.ship-zip   = shipto.ship-zip.

          FIND FIRST est WHERE est.company eq job.company
                AND est.est-no  eq job.est-no
              NO-LOCK NO-ERROR.
          RELEASE eb.
          IF AVAIL est THEN
          FIND FIRST eb
              WHERE eb.company   EQ est.company
                AND eb.est-no    EQ est.est-no
                AND eb.form-no   EQ job-hdr.frm
                AND (eb.blank-no EQ job-hdr.blank-no OR job-hdr.blank-no EQ 0)
              NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
            ASSIGN
             w-ord.flute      = eb.flute
             w-ord.test       = eb.test
             w-ord.pcs        = eb.cas-cnt
             w-ord.bundle     = eb.cas-pal
             w-ord.total-unit = w-ord.pcs * w-ord.bundle
             w-ord.partial    = 0 /* w-ord.ord-qty - w-ord.total-unit*/
             w-ord.cas-no     = eb.cas-no.

          ASSIGN w-ord.total-tags = 1
            w-ord.ord-qty = loadtag.qty 
            w-ord.pcs = loadtag.qty-case
            w-ord.bundle = loadtag.case-bundle
            w-ord.partial =loadtag.partial
            w-ord.total-unit = w-ord.pcs * w-ord.bundle  .      

       END.  /* avail job*/
   END. /* job-no <> "" */
   ELSE IF loadtag.po-no <> 0 THEN DO:
      FIND FIRST po-ord WHERE po-ord.company = loadtag.company
                           AND po-ord.po-no = loadtag.po-no NO-LOCK NO-ERROR.
      IF AVAIL po-ord THEN
         FIND FIRST po-ordl NO-LOCK WHERE po-ordl.company EQ po-ord.company
                                    AND po-ordl.po-no EQ po-ord.po-no
                                    AND po-ordl.i-no = loadtag.i-no
                                    USE-INDEX po-no  NO-ERROR.
      IF AVAIL po-ordl THEN DO:
         FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                                AND cust.cust-no EQ po-ord.cust-no NO-ERROR.
         FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                                AND vend.vend-no EQ po-ord.vend-no NO-ERROR.
         FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
                                  AND itemfg.i-no EQ po-ordl.i-no NO-ERROR.

         CREATE w-ord.
         ASSIGN
            w-ord.cust-name = IF AVAILABLE cust THEN cust.name ELSE ''
            w-ord.cust-no = po-ord.cust-no
            w-ord.due-date = po-ord.due-date
            w-ord.i-no = po-ordl.i-no
            w-ord.i-name = po-ordl.i-name
            w-ord.mult = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                         cust.int-field[1] ELSE v-mult
            w-ord.ord-qty = po-ordl.ord-qty
            w-ord.po-no = po-ord.po-no
            w-ord.tare-wt = 10
            w-ord.uom = 'EA'
            w-ord.vendor = IF AVAILABLE vend THEN vend.name ELSE ''
            w-ord.lot    = loadtag.misc-char[2]. 
         IF AVAILABLE itemfg THEN
            ASSIGN w-ord.est-no = itemfg.est-no
                w-ord.upc-no = itemfg.upc-no
                w-ord.box-len = itemfg.l-score[50]
                w-ord.box-wid = itemfg.w-score[50]
                w-ord.box-dep = itemfg.d-score[50]
                w-ord.flute = itemfg.flute
                w-ord.test = itemfg.test
                w-ord.pcs = itemfg.case-count
                w-ord.bundle = IF itemfg.case-pall NE 0 THEN itemfg.case-pall ELSE 1
                w-ord.style = itemfg.style.

         IF w-ord.style NE "" THEN
         DO:
            FIND FIRST style WHERE
                 style.company EQ cocode AND
                 style.style EQ w-ord.style
                 NO-LOCK NO-ERROR.

            IF AVAIL style THEN
            DO:
               w-ord.style-desc = style.dscr.
               RELEASE style.
            END.
         END.

         IF AVAILABLE itemfg AND itemfg.est-no NE '' THEN
            FIND FIRST eb NO-LOCK WHERE eb.company EQ itemfg.company
                              AND eb.est-no EQ itemfg.est-no
                              AND eb.stock-no EQ itemfg.i-no NO-ERROR.
         IF AVAILABLE eb THEN
             ASSIGN w-ord.flute = eb.flute
                    w-ord.test = eb.test
                    w-ord.pcs = eb.cas-cnt
                    w-ord.bundle = eb.cas-pal
                    w-ord.cas-no = eb.cas-no.

         FIND FIRST shipto NO-LOCK WHERE shipto.company EQ cocode
                                  AND shipto.cust-no EQ po-ord.cust-no
                                  AND shipto.ship-id EQ po-ord.cust-no
                                USE-INDEX ship-id NO-ERROR.
         IF AVAILABLE shipto THEN
            ASSIGN w-ord.ship-name = shipto.ship-name
                    w-ord.ship-add1 = shipto.ship-add[1]
                    w-ord.ship-add2 = shipto.ship-add[2]
                    w-ord.ship-city = shipto.ship-city
                    w-ord.ship-state = shipto.ship-state
                    w-ord.ship-zip = shipto.ship-zip.

         ASSIGN w-ord.total-tags = 1
            w-ord.ord-qty = loadtag.qty 
            w-ord.pcs = loadtag.qty-case
            w-ord.bundle = loadtag.case-bundle
            w-ord.partial =loadtag.partial
            w-ord.total-unit = w-ord.pcs * w-ord.bundle  .      

    END. /* AVAIL PO-ORDL */
   END. /* po-no <> ""*/
   ELSE DO:
       FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
                                 AND itemfg.i-no EQ loadtag.i-no NO-ERROR.
       IF AVAIL itemfg THEN DO:
          FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                              AND vend.vend-no EQ itemfg.vend-no NO-ERROR.
          FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                              AND cust.cust-no EQ itemfg.cust-no NO-ERROR.

          CREATE w-ord.
          ASSIGN w-ord.i-no = itemfg.i-no
                 w-ord.i-name = itemfg.i-name
                 w-ord.cust-no = itemfg.cust-no
                 w-ord.cust-name = itemfg.cust-name
                 w-ord.cust-part-no = itemfg.part-no
                 w-ord.mult = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                              cust.int-field[1] ELSE v-mult
                 w-ord.box-len = itemfg.l-score[50]
                 w-ord.box-wid = itemfg.w-score[50]
                 w-ord.box-dep = itemfg.d-score[50]
                 w-ord.flute = itemfg.flute
                 w-ord.upc-no = itemfg.upc-no
                 w-ord.test = itemfg.test
                 w-ord.vendor = IF AVAILABLE vend THEN vend.name ELSE company.name
                 w-ord.tare-wt = 10
                 w-ord.uom = "EA"
                 w-ord.pcs = itemfg.case-count
                 w-ord.bundle = itemfg.case-pall
                 w-ord.total-tags = 1
                 w-ord.ord-qty = loadtag.qty 
                 w-ord.pcs = loadtag.qty-case
                 w-ord.bundle = loadtag.case-bundle
                 w-ord.partial =loadtag.partial
                 w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial
                 w-ord.style        = itemfg.style
                 w-ord.lot          = loadtag.misc-char[2].

          IF w-ord.style NE "" THEN
          DO:
             FIND FIRST style WHERE
                  style.company EQ cocode AND
                  style.style EQ w-ord.style
                  NO-LOCK NO-ERROR.

             IF AVAIL style THEN
             DO:
                w-ord.style-desc = style.dscr.
                RELEASE style.
             END.
          END.
       END. /* avail itemfg */
   END.
   /* task 11230523 */
   /*IF tb_reprint-tag THEN DO:
      FIND FIRST fg-bin WHERE fg-bin.company = loadtag.company
                          AND fg-bin.i-no = w-ord.i-no
                          AND fg-bin.tag = fi_cas-lab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                          AND fg-bin.qty > 0 NO-LOCK NO-ERROR.
      IF AVAIL fg-bin AND AVAIL w-ord THEN
         ASSIGN w-ord.pcs = fg-bin.case-count
                w-ord.bundle = /*fg-bin.cases-unit*/ TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                w-ord.partial = fg-bin.partial-count
                w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial .      
   END.
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createComponentList C-Win 
PROCEDURE createComponentList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


DEFINE BUFFER bf-itemfg FOR itemfg.
DEFINE BUFFER bf-fg-set FOR fg-set.

FOR EACH bf-itemfg 
    WHERE bf-itemfg.company EQ cocode
      AND bf-itemfg.i-no GE begin_i-no
      AND bf-itemfg.i-no LE end_i-no
      AND bf-itemfg.isaset
    NO-LOCK,
 EACH bf-fg-set 
    WHERE bf-fg-set.company EQ bf-itemfg.company
      AND bf-fg-set.set-no EQ bf-itemfg.i-no
    NO-LOCK:

 FIND FIRST tt-set WHERE tt-set.part-no = bf-fg-set.part-no NO-ERROR.
 IF NOT AVAIL tt-set THEN DO:
     CREATE tt-set.
     ASSIGN tt-set.part-no = bf-fg-set.part-no.
 END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-transfer C-Win 
PROCEDURE crt-transfer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rno AS INT NO-UNDO.
  DEF BUFFER b-fg-rctd FOR fg-rctd.
  DEF VAR lv-rctd-rowid AS ROWID NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

  DO WHILE TRUE:
    lv-rno = lv-rno + 1.
    IF CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no) OR
       CAN-FIND(FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd) THEN
       NEXT.
    LEAVE.
  END.

  /*FOR EACH b-fg-rctd WHERE
      recid(b-fg-rctd) <> RECID(b2-fg-rctd) AND
      b-fg-rctd.i-no = b2-fg-rctd.i-no AND
      b-fg-rctd.tag = b2-fg-rctd.tag:
      DELETE b-fg-rctd.
  END.*/

  FOR EACH fg-bin WHERE
      fg-bin.company EQ cocode AND
      fg-bin.i-no    EQ b2-fg-rctd.i-no AND
      fg-bin.job-no  EQ b2-fg-rctd.job-no AND
      fg-bin.job-no2 EQ b2-fg-rctd.job-no2 AND
      fg-bin.tag     EQ b2-fg-rctd.tag
      NO-LOCK:

     IF fg-bin.loc NE b2-fg-rctd.loc OR
        fg-bin.loc-bin NE b2-fg-rctd.loc-bin THEN DO:
        CREATE b-fg-rctd.
        BUFFER-COPY b2-fg-rctd EXCEPT b2-fg-rctd.r-no TO b-fg-rctd
        ASSIGN b-fg-rctd.r-no = lv-rno
               b-fg-rctd.loc = fg-bin.loc
               b-fg-rctd.loc-bin = fg-bin.loc-bin
               b-fg-rctd.cases = 0
               b-fg-rctd.qty-case = 0
               b-fg-rctd.cases-unit = 0
               b-fg-rctd.partial = 0
               b-fg-rctd.t-qty = 0
               lv-rno = lv-rno + 1.
        RELEASE b-fg-rctd.
     END.
  END.  /* for each fg-bin*/
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
  DISPLAY v-post-date begin_fg-r-no end_fg-r-no begin_userid end_userid ldt-from 
          ldt-to begin_job-no end_job-no begin_i-no end_i-no end_whs begin_whs 
          tg-recalc-cost v-trans-lbl tgl-itemCD rd_print rd-Itm#Cst# t-receipt 
          rd-ItmPo t-ship rd-UOMJob t-trans t-adj tb_glnum t-ret tb_grndtotal 
          tb_totCstVal tgIssue td-show-parm rd-dest lv-ornt lv-font-no 
          lines-per-page lv-font-name tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 RECT-30 RECT-31 RECT-32 v-post-date begin_fg-r-no 
         end_fg-r-no begin_userid end_userid ldt-from ldt-to begin_job-no 
         end_job-no begin_i-no end_i-no end_whs begin_whs tg-recalc-cost 
         rd_print rd-Itm#Cst# t-receipt rd-ItmPo t-ship rd-UOMJob t-trans t-adj 
         tb_glnum t-ret tb_grndtotal tb_totCstVal tgIssue td-show-parm rd-dest 
         lv-ornt lv-font-no lines-per-page tb_excel tb_runExcel fi_file Btn_OK 
         Btn_Cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE farmOutComp C-Win 
PROCEDURE farmOutComp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR cJob AS CHAR NO-UNDO.
DEF VAR iJobNo2 AS INT NO-UNDO.
  cJob = "".
  iJobNo2 = 0.   

  IF (w-fg-rctd.job-no GT "" OR w-fg-rctd.po-no GT "") AND itemfg.pur-man THEN DO:

      /* Find a job for this po if this is a farmout */
      IF w-fg-rctd.job-no GT "" THEN
          ASSIGN cJob = w-fg-rctd.job-no
                 iJobNo2 = w-fg-rctd.job-no2.
      ELSE IF w-fg-rctd.po-no GT "" THEN  DO:
           FIND FIRST po-ordl WHERE po-ordl.company EQ w-fg-rctd.company
               AND po-ordl.po-no EQ INTEGER(w-fg-rctd.po-no)
               AND po-ordl.i-no  EQ w-fg-rctd.i-no
               NO-LOCK NO-ERROR.

           IF AVAIL(po-ordl) AND po-ordl.ord-no GT 0 THEN DO:

              FIND FIRST oe-ordl WHERE oe-ordl.company EQ g_company
                  AND oe-ordl.ord-no EQ po-ordl.ord-no
                  AND oe-ordl.i-no   EQ po-ordl.i-no
                  NO-LOCK NO-ERROR.
              /* assumption is that for farm jobs, order and job are always the same */
              /* This is to obtain the job-no2 since job-no is assumed to be the order # */
              IF NOT AVAIL oe-ordl THEN
                  FIND FIRST oe-ordl WHERE oe-ordl.company EQ g_company
                      AND oe-ordl.ord-no EQ po-ordl.ord-no
                      AND oe-ordl.job-no EQ string(po-ordl.ord-no)
                      NO-LOCK NO-ERROR.

              IF AVAIL oe-ordl AND oe-ordl.job-no GT "" THEN
                  ASSIGN cJob = oe-ordl.job-no
                         iJobNo2 = oe-ordl.job-no2.
           END.
           ELSE IF AVAIL(po-ordl) AND po-ordl.job-no GT "" THEN DO:
               ASSIGN cJob    = po-ordl.job-no
                      iJobNo2 = po-ordl.job-no2.
           END.

      END.


      FIND FIRST job WHERE job.company EQ w-fg-rctd.company
          AND job.job-no EQ cJob
          AND job.job-no2 EQ iJobNo2
          NO-LOCK NO-ERROR.

      IF AVAIL job AND cJob GT "" 
                   AND w-fg-rctd.rita-code EQ "R" 
                   AND w-fg-rctd.qty GT 0 THEN DO:             
          /* Copy fg-rctd for the jobs farmout tab */
          CREATE job-farm-rctd.
          BUFFER-COPY w-fg-rctd EXCEPT rec_key rita-code TO job-farm-rctd.
          ASSIGN job-farm-rctd.rita-code = "F"
                 job-farm-rctd.job-no = cJob
                 job-farm-rctd.job-no2 = iJobNo2.
          /* ASSIGN job-farm-rctd.job = job.job. */
          RUN jc/updJobFarmActual.p (INPUT ROWID(job), INPUT w-fg-rctd.i-no).
      END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fg-post C-Win 
PROCEDURE fg-post :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer b-fg-rcpts for fg-rcpts.
  def buffer b-fg-rdtl for fg-rdtl.
  def buffer b-fg-bin for fg-bin.
  DEF BUFFER b-itemfg FOR itemfg.
  def buffer b-itemfg1 for itemfg.
  def buffer ps-rctd for fg-rctd .
  def buffer b-po-ordl for po-ordl.
  def buffer b-oe-ordl for oe-ordl.
  DEF BUFFER b-w-fg-rctd FOR w-fg-rctd.
  def var v-one-item as log.
  def var v-dec as dec decimals 10.
  def var v-po-no like rm-rcpt.po-no no-undo.
  def var x as int no-undo.
  def var i as int no-undo.
  def var v-r-qty like fg-rctd.qty no-undo.
  def var v-i-qty like fg-rctd.qty no-undo.
  def var v-t-qty like fg-rctd.qty no-undo.
  def var v-overrun-qty like fg-rctd.qty no-undo.
  def var v-underrun-qty like fg-rctd.qty no-undo.
  DEF VAR v-reduce-qty AS INT NO-UNDO.
  DEF VAR v-est-no AS cha NO-UNDO.
  def var v-recid as recid no-undo.
  DEF VAR v-cost AS DEC NO-UNDO.
  DEF VAR v-binqty AS INT NO-UNDO.
  DEF VAR v-qty AS INT NO-UNDO.
  DEF VAR v-tagcost AS DEC NO-UNDO.
  def var ld-cvt-qty as dec no-undo.
  def var ld-cvt-cost as dec DECIMALS 10 no-undo.
  def var v-autobin  as cha no-undo.
  def var v-newhdr as log no-undo. 
  def var v-fin-qty as dec no-undo.
  def var choice as log no-undo.
  def var v-trnum like gl-ctrl.trnum no-undo.
  def var uperiod as int no-undo.
  def var sysdate as date init today no-undo.    
  def var v-date like sysdate no-undo.
  DEF VAR v-underrun AS DEC NO-UNDO.
  DEF VAR v-qty-received AS INT NO-UNDO.
  DEF VAR v-got-fgemail AS LOG NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR li-tag-no AS INT NO-UNDO.
  DEF VAR ll-qty-changed AS LOG NO-UNDO.
  DEF VAR ll-whs-item AS LOG NO-UNDO.
  DEF VAR v-calc-cost AS DEC NO-UNDO.
  DEF VAR cJob LIKE oe-ordl.job-no  NO-UNDO.
  DEF VAR iJobNo2 LIKE oe-ordl.job-no2 NO-UNDO.
  DEFINE VARIABLE fgPostLog AS LOGICAL NO-UNDO.
  /*##PN - variable for FGSetAssembly setting*/

  DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lFGSetAssembly AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cFGSetAssembly AS CHARACTER   NO-UNDO.

  fgPostLog = SEARCH('logs/fgpstall.log') NE ?.
  IF fgPostLog THEN
  OUTPUT STREAM logFile TO VALUE('logs/fgpstall.' +
         STRING(TODAY,'99999999') + '.' + STRING(TIME) + '.log').

  SESSION:SET-WAIT-STATE ("general").
  IF fgPostLog THEN RUN fgPostLog ('Started').
  FIND FIRST period NO-LOCK
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date.

  find first sys-ctrl  where sys-ctrl.company eq gcompany
                         and sys-ctrl.name    eq "AUTOPOST"
       no-lock no-error.
  v-autobin = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".
  RUN sys/ref/nk1look.p (INPUT cocode,
                         INPUT "FGSetAssembly",
                         INPUT "L",
                         INPUT NO,
                         INPUT NO,
                         INPUT "",
                         INPUT "",
                         OUTPUT cFGSetAssembly,
                         OUTPUT lFound).
  IF lFound THEN
      lFGSetAssembly = cFGSetAssembly EQ "YES".
  RUN sys/ref/nk1look.p (INPUT cocode,
                         INPUT "FGSetAssembly",
                         INPUT "C",
                         INPUT NO,
                         INPUT NO,
                         INPUT "",
                         INPUT "",
                         OUTPUT cFGSetAssembly,
                         OUTPUT lFound).

  DISABLE TRIGGERS FOR LOAD OF itemfg.
  DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.

  /* Handle Manually created job farm out records */
  FOR EACH w-fg-rctd WHERE w-fg-rctd.rita-code = "F":       
      RUN manualFarmOut.
      DELETE w-fg-rctd.
  END.
  FIND FIRST w-fg-rctd NO-ERROR.
  /* In case only processing rita-code F */
  IF NOT AVAIL w-fg-rctd THEN DO:   
      RETURN.
  END.



  /* Check for invalid transfers */
  FOR EACH w-fg-rctd WHERE w-fg-rctd.rita-code = "T"
      BY w-fg-rctd.tag
      BY w-fg-rctd.rct-date
      BY w-fg-rctd.trans-time
      BY w-fg-rctd.r-no:

      IF NOT CAN-FIND(FIRST itemfg WHERE
         itemfg.company EQ cocode AND
         itemfg.i-no    EQ w-fg-rctd.i-no) THEN
         NEXT.

      find first fg-bin where fg-bin.company eq cocode
                          and fg-bin.i-no    eq w-fg-rctd.i-no
                          and fg-bin.job-no  eq w-fg-rctd.job-no
                          and fg-bin.job-no2 eq w-fg-rctd.job-no2
                          and fg-bin.loc     eq w-fg-rctd.loc
                          and fg-bin.loc-bin eq w-fg-rctd.loc-bin
                          and fg-bin.tag     eq w-fg-rctd.tag
                          and fg-bin.cust-no eq w-fg-rctd.cust-no
                USE-INDEX co-ino no-error.
      IF NOT AVAIL fg-bin THEN DO:
          MESSAGE "A transfer exists for item " w-fg-rctd.i-no SKIP
              "with an invalid location:" SKIP
              "  Warehouse = " w-fg-rctd.loc SKIP
              "  Bin = " w-fg-rctd.loc-bin SKIP
              "  Tag = " w-fg-rctd.tag SKIP
              "Please correct and re-run the posting process." SKIP
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "Invalid Location".
      END.
      ELSE IF fg-bin.pur-uom EQ "" THEN DO:
          MESSAGE "A blank UOM exists for item bin " w-fg-rctd.i-no SKIP
              "with location:" SKIP
              "  Warehouse = " w-fg-rctd.loc SKIP
              "  Bin = " w-fg-rctd.loc-bin SKIP
              "  Tag = " w-fg-rctd.tag SKIP
              "Please correct and re-run the posting process." SKIP
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "Invalid Location".          
      END.
      ELSE IF itemfg.prod-uom EQ "" THEN DO:
          MESSAGE "A blank cost UOM exists for item " w-fg-rctd.i-no SKIP
              "Please correct and re-run the posting process." SKIP
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "Invalid Location".          
      END.
   END.
  /* #pn# Setting rita-code to A since the negative R was causing problems */
  /* #pn# task 08211305                                                    */   
  FOR EACH w-fg-rctd:
      FOR EACH b-w-fg-rctd WHERE b-w-fg-rctd.qty LT 0,

        FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" 
          AND reftable.company  EQ w-fg-rctd.company 
          AND reftable.loc      EQ STRING(b-w-fg-rctd.r-no,"9999999999")        
          AND (reftable.dscr EQ "fg-rctd: " + STRING(w-fg-rctd.r-no, "9999999999") AND reftable.dscr begins "fg-rctd: ")  
          use-index loc   NO-LOCK .

        FIND fg-rctd WHERE ROWID(fg-rctd) = b-w-fg-rctd.row-id EXCLUSIVE-LOCK NO-ERROR.
        FIND FIRST itemfg WHERE
         itemfg.company EQ cocode AND
         itemfg.i-no    EQ w-fg-rctd.i-no
         NO-LOCK NO-ERROR.

        IF AVAIL fg-rctd  THEN DO:
            /*##BL - FGSetAssembly requires the bin to match that of the character*/
            /*##BL of FGSetAssembly N-K.  If it doesn't, abort posting  */
            IF lFGSetAssembly 
              AND fg-rctd.loc-bin NE cFGSetAssembly 
              AND avail(itemfg) 
              AND itemfg.alloc  EQ NO THEN DO:
                MESSAGE "The Bin location for Component " fg-rctd.i-no " must be " cFGSetAssembly "." SKIP
                    "Please correct on the Set Parts tab of FG Receiving and re-run the posting process."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN ERROR.
            END.
            ASSIGN b-w-fg-rctd.rita-code = "A"
                   fg-rctd.rita-code     = "A".
        END.
        RELEASE fg-rctd.

      END.
  END.
  FOR EACH w-fg-rctd
      BY w-fg-rctd.tag
      BY w-fg-rctd.rct-date
      BY w-fg-rctd.trans-time
      BY w-fg-rctd.r-no:

    IF NOT CAN-FIND(FIRST itemfg WHERE
       itemfg.company EQ cocode AND
       itemfg.i-no    EQ w-fg-rctd.i-no) THEN
       NEXT.

    loop1:
    REPEAT:

       FIND FIRST itemfg WHERE
            itemfg.company EQ cocode AND
            itemfg.i-no    EQ w-fg-rctd.i-no
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

       IF AVAIL itemfg THEN
       DO:           
          /* If FGEMAIL is active and quantity on hand is zero and item is allocated,
             then process user data into a temp-table for processing emails later. */
          IF gv-fgemail = YES AND (itemfg.q-onh = 0 AND itemfg.q-alloc > 0) THEN
              RUN Process-FGemail-Data (INPUT itemfg.i-no, w-fg-rctd.t-qty,w-fg-rctd.po-no).

          IF fgPostLog THEN RUN fgPostLog ('Start fg/fg-post.i ' + TRIM(itemfg.i-no)).

          /* itemfg gets updated here. */
          {fg/fg-post.i w-fg-rctd w-fg-rctd}

          IF autofgissue-log THEN
            RUN farmOutComp.

          FIND CURRENT itemfg NO-LOCK NO-ERROR.
          FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
          FIND CURRENT po-ordl NO-LOCK NO-ERROR.
          FIND CURRENT fg-bin NO-LOCK NO-ERROR.
          LEAVE loop1.
       END. /* IF AVAIL itemfg */
    END. /* loop1 REPEAT */

    IF fgPostLog THEN RUN fgPostLog ('End fg/fg-post.i - Start fg/fgemails.i').
    IF w-fg-rctd.rita-code = "R" THEN DO:
       {fg/fgemails.i}
    END.

    IF fgPostLog THEN RUN fgPostLog ('End fg-bin - Start fg-rctd').

    FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-ERROR.

    IF AVAIL fg-rctd THEN DO:
      ASSIGN
       fg-rctd.rita-code = "P"  /* posted */
       fg-rctd.post-date = v-post-date
       fg-rctd.trans-time = TIME
       fg-rctd.tag2      = w-fg-rctd.tag2.

      FOR EACH fg-rcpts
          WHERE fg-rcpts.company EQ fg-rctd.company
            AND fg-rcpts.r-no    EQ fg-rctd.r-no:
        ASSIGN fg-rcpts.rita-code = fg-rctd.rita-code.
      END.

      FIND CURRENT fg-rctd NO-LOCK.
    END.

    IF fgPostLog THEN RUN fgPostLog ('End loop'). 
  END.  /* for each w-fg-rctd */


  IF fgPostLog THEN RUN fgPostLog ('End fg/fgemails.i - Start loadtag').
  FOR EACH w-fg-rctd
      BREAK BY w-fg-rctd.i-no
            BY w-fg-rctd.job-no
            BY w-fg-rctd.job-no2
            BY w-fg-rctd.loc
            BY w-fg-rctd.loc-bin
            BY w-fg-rctd.tag:

    IF FIRST-OF(w-fg-rctd.i-no) THEN DO:
        FIND FIRST tt-posted-items WHERE tt-posted-items.i-no = w-fg-rctd.i-no
                                   NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-posted-items THEN DO:
            CREATE tt-posted-items.
            ASSIGN tt-posted-items.i-no = w-fg-rctd.i-no.
        END.
    END.

    IF LAST-OF(w-fg-rctd.tag) THEN DO:
       IF TRIM(w-fg-rctd.tag) NE "" THEN 
       /* Ensure Bin/Tags Qty is correct.  Task 01270602 */
       FOR EACH fg-bin NO-LOCK
           WHERE fg-bin.company EQ g_company
             AND fg-bin.i-no    EQ w-fg-rctd.i-no
             AND fg-bin.tag     EQ w-fg-rctd.tag
           USE-INDEX tag:

         RUN fg/calcbinq.p (ROWID(fg-bin)).
       END.

       FIND FIRST loadtag
           WHERE loadtag.company   EQ g_company
             AND loadtag.item-type EQ NO
             AND loadtag.tag-no    EQ w-fg-rctd.tag
             AND loadtag.i-no      EQ w-fg-rctd.i-no
             AND loadtag.job-no    EQ w-fg-rctd.job-no
           USE-INDEX tag EXCLUSIVE-LOCK NO-ERROR.

       IF fgPostLog THEN RUN fgPostLog ('End loadtag - Start fg-bin').

       IF AVAIL loadtag THEN DO:
         FIND FIRST fg-bin
             WHERE fg-bin.company EQ g_company
               AND fg-bin.i-no    EQ loadtag.i-no
               AND fg-bin.tag     EQ loadtag.tag-no
               AND fg-bin.qty     GT 0
             USE-INDEX tag NO-LOCK NO-ERROR.
         IF AVAIL fg-bin AND w-fg-rctd.rita-code = "T" AND
            TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) = w-fg-rctd.cases THEN /* full qty transfer*/ 

           ASSIGN
            loadtag.loc          = w-fg-rctd.loc2   
            loadtag.loc-bin      = w-fg-rctd.loc-bin2
            loadtag.qty          = fg-bin.qty
            loadtag.pallet-count = fg-bin.qty
            loadtag.partial      = fg-bin.partial-count
            loadtag.tot-cases    = (loadtag.qty - loadtag.partial) / loadtag.qty-case.

         ELSE /*partial transfer */
           ASSIGN
            loadtag.loc     = w-fg-rctd.loc
            loadtag.loc-bin = w-fg-rctd.loc-bin.

         FIND CURRENT loadtag NO-LOCK.
       END.
    END.

/*  task 04041203 - cycle count record was not necessary per Joe */
/*     IF ip-run-what EQ "SETUP" AND ssfgretc-log AND                 */
/*        ( (w-fg-rctd.rita-code EQ "T" AND w-fg-rctd.inv-no NE 0) OR */
/*           w-fg-rctd.rita-code EQ "I" ) THEN                        */
/*        RUN create-phy-count-proc.                                  */
  END.

  FOR EACH w-inv:
      /* Save w-inv data to send email bol's */
      DELETE w-inv.
  END.

  IF fgPostLog THEN RUN fgPostLog ('End First - Start Second For Each w-fg-rctd').
  FOR EACH w-fg-rctd WHERE w-fg-rctd.invoiced,
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
      NO-LOCK:

    CREATE w-inv.
    w-inv.row-id = w-fg-rctd.row-id.
  END.
  IF fgPostLog THEN RUN fgPostLog ('End Second For Each w-fg-rctd').

  IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/invrecpt.p').
  RUN fg/invrecpt.p (?, 2).
  IF fgPostLog THEN RUN fgPostLog ('End Run fg/invrecpt.p').

  FOR EACH w-inv:
    /* Save w-inv data to send email bol's */
    CREATE tt-inv.
    BUFFER-COPY w-inv TO tt-inv.

  END.


  IF fgPostLog THEN RUN fgPostLog ('End First - Start Third For Each w-fg-rctd').

  FOR EACH w-fg-rctd WHERE (TRIM(w-fg-rctd.tag) EQ "" OR v-cost-from-receipt = "TransferCost"),
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
      NO-LOCK
      BREAK BY w-fg-rctd.i-no:



    IF LAST-OF(w-fg-rctd.i-no) THEN DO:


      IF fgPostLog THEN RUN fgPostLog ('Third loop  -  Start Last i-no').

      IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).
      RUN fg/updfgcs1.p (RECID(itemfg), NO).
      IF fgPostLog THEN RUN fgPostLog ('End Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).

      /* Calculate this once per item instead of per order line */
      IF v-cost-from-receipt = "TransferCost" AND itemfg.spare-dec-1 EQ 0 THEN DO:
          /* override for v-cost-from-receipt */
           IF w-fg-rctd.job-no GT "" THEN DO:
               FIND job-hdr WHERE job-hdr.company = cocode
                              AND job-hdr.job-no  = w-fg-rctd.job-no
                              AND job-hdr.job-no2 = w-fg-rctd.job-no2
                              AND job-hdr.i-no    = w-fg-rctd.i-no
                            NO-LOCK NO-ERROR.
           END. /* Job-no gt "" */
           IF w-fg-rctd.po-no GT "" THEN
               FIND FIRST po-ordl WHERE po-ordl.company EQ cocode
                                    AND po-ordl.po-no EQ INTEGER(w-fg-rctd.po-no)
                                    AND po-ordl.i-no  EQ w-fg-rctd.i-no
                                  NO-LOCK NO-ERROR.

           IF NOT ((AVAIL(job-hdr) AND job-hdr.ord-no GT 0) OR
                   (AVAIL(po-ordl) AND po-ordl.ord-no GT 0)) THEN

             RUN sys/ref/convcuom.p("EA",
                                     "M", 0, 0, 0, 0,
                                     w-fg-rctd.ext-cost / w-fg-rctd.t-qty, OUTPUT v-calc-cost).

      END. /* If v-cost-from-receipt = TransferCost */


      FOR EACH oe-ordl
          WHERE oe-ordl.company EQ cocode
            AND oe-ordl.opened  EQ YES
            AND oe-ordl.i-no    EQ w-fg-rctd.i-no
            AND oe-ordl.job-no  EQ ""
          /*  AND oe-ordl.cost    EQ 0*/
          USE-INDEX opened NO-LOCK
          BREAK BY oe-ordl.ord-no
          TRANSACTION:

        v-calc-cost = oe-ordl.cost.

        IF oe-ordl.cost NE 0 AND NOT v-cost-from-receipt = "TransferCost" THEN
            NEXT.

        /* Default to standard cost, or accept calculated value from code above */
        IF NOT (v-cost-from-receipt = "TransferCost" AND itemfg.spare-dec-1 EQ 0) THEN DO:
          IF oe-ordl.cost EQ 0 THEN DO:

            IF itemfg.prod-uom EQ "M" THEN
              v-calc-cost = itemfg.total-std-cost.
            ELSE
              RUN sys/ref/convcuom.p((IF LOOKUP(itemfg.prod-uom,fg-uom-list) GT 0
                                      THEN "EA" ELSE itemfg.prod-uom),
                                      "M", 0, 0, 0, 0,
                                      itemfg.total-std-cost, OUTPUT v-calc-cost).
          END. /* If cost EQ 0 */

        END. /* Not TransferCost */

        /* WFK - process is too slow, so only update if its available */
        FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) EXCLUSIVE NO-ERROR NO-WAIT.
        IF b-oe-ordl.cost NE v-calc-cost THEN
          b-oe-ordl.cost = v-calc-cost.

        IF fgPostLog THEN RUN fgPostLog ('Third loop - End Last i-no').

      END. /* each oe-ordl */
    END. /* last of i-no */
  END. /* each w-fg-rctd */

  IF fgPostLog THEN RUN fgPostLog ('Start process releases').
  /* If overage, reset quantity or create a new release */
  RUN process-releases.
  IF fgPostLog THEN RUN fgPostLog ('End process releases').

  IF fgPostLog THEN RUN fgPostLog ('End Third For Each w-fg-rctd').

  IF v-fgpostgl NE "None" THEN DO TRANSACTION:

    loop2:
    REPEAT:
       FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
       IF AVAIL gl-ctrl THEN
       DO:
          assign
             v-trnum       = gl-ctrl.trnum + 1
             gl-ctrl.trnum = v-trnum.
          FIND CURRENT gl-ctrl NO-LOCK.
          LEAVE loop2.
       END.
    END.

    IF fgPostLog THEN RUN fgPostLog ('Begin Run gl-from-work 1').
    RUN gl-from-work (1, v-trnum).
    IF fgPostLog THEN RUN fgPostLog ('End 1 - Begin Run gl-from-work 2').
    RUN gl-from-work (2, v-trnum).
    IF fgPostLog THEN RUN fgPostLog ('End Run gl-from-work 2').
  END.
  FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
  find first w-job no-error.
  if avail w-job THEN DO:
    IF fgPostLog THEN RUN fgPostLog ('Start jc/d-jclose.p').
    run jc/d-jclose.w.
    IF fgPostLog THEN RUN fgPostLog ('End jc/d-jclose.p').
  END.

  if v-adjustgl then do TRANSACTION:
    /** GET next G/L TRANS. POSTING # **/
    REPEAT:
       find first gl-ctrl where gl-ctrl.company eq cocode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

       IF AVAIL gl-ctrl THEN
       DO:
          assign
           v-trnum       = gl-ctrl.trnum + 1
           gl-ctrl.trnum = v-trnum.
          FIND CURRENT gl-ctrl NO-LOCK.
          LEAVE.
       END.
    END.

    IF fgPostLog THEN RUN fgPostLog ('Start For Each work-job').
    for each work-job break by work-job.actnum:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-job.actnum
       gltrans.jrnl    = "ADJUST"
       gltrans.tr-date = v-post-date
       gltrans.period  = period.pnum
       gltrans.trnum   = v-trnum.

      if work-job.fg then
        assign
         gltrans.tr-amt  = - work-job.amt
         gltrans.tr-dscr = "ADJUSTMENT FG".
      else
        assign
         gltrans.tr-amt  = work-job.amt
         gltrans.tr-dscr = "ADJUSTMENT COGS".

      RELEASE gltrans.
    end. /* each work-job */
    IF fgPostLog THEN RUN fgPostLog ('End For Each work-job').
  end.

  IF tg-recalc-cost THEN DO:
    FOR EACH tt-posted-items:        
        RUN fg/updfgcst.p (tt-posted-items.i-no).
    END.
  END.

  IF v-got-fgemail THEN DO:
    IF fgPostLog THEN RUN fgPostLog ('Start Run send-fgemail').
    RUN send-fgemail (v-fgemail-file).
    IF fgPostLog THEN RUN fgPostLog ('End Run send-fgemail').
  END.  

  FOR EACH w-fg-rctd ,
    FIRST itemfg
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ w-fg-rctd.i-no NO-LOCK , 
    EACH tt-inv WHERE tt-inv.row-id EQ w-fg-rctd.row-id 
    BREAK BY tt-inv.bol-no:

      FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-LOCK NO-ERROR.
      RUN get-ord-recs (ROWID(fg-rctd),
                        BUFFER po-ordl,
                        BUFFER po-ord,
                        BUFFER oe-ordl,
                        BUFFER oe-ord,
                        BUFFER reftable).
      IF AVAIL(reftable) AND (reftable.val[2] GT 0 OR reftable.val[3] EQ 1) THEN
      ASSIGN ll       = reftable.val[1] NE 0
             dBillAmt  = reftable.val[2]
             lEmailBol = reftable.val[3] EQ 1
             lInvFrt   = reftable.val[1] GT 0.
       IF lEmailBol AND last-of(tt-inv.bol-no) THEN DO:
         FIND FIRST oe-bolh WHERE oe-bolh.company EQ g_company
            AND oe-bolh.bol-no EQ tt-inv.bol-no NO-LOCK NO-ERROR.

         RUN custom/setUserPrint.p (g_company,'oe-boll_.',
               'begin_cust,end_cust,begin_bol#,end_bol#,begin_ord#,end_ord#,tb_reprint,tb_posted,rd_bolcert',
               oe-bolh.cust-no + ',' + oe-bolh.cust-no + ',' +
               STRING(oe-bolh.bol-no) + ',' + STRING(oe-bolh.bol-no) +
               ',,99999999,' + STRING(oe-bolh.printed) + ',' +
               STRING(oe-bolh.posted) + ',BOL').
         RUN listobjs/oe-boll_.w.

       END. /* If email bol */
  END. /* each w-fg-rctd */

  IF fgPostLog THEN RUN fgPostLog ('End').
  IF fgPostLog THEN OUTPUT STREAM logFile CLOSE.
  /* WFK - no error message was being returned, so set to no if */
  /*       no return error was encountered                      */
  ERROR-STATUS:ERROR = NO.

  SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgPostLog C-Win 
PROCEDURE fgPostLog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipLogText AS CHARACTER NO-UNDO.

  PUT STREAM logFile UNFORMATTED STRING(TODAY,'99.99.9999') ' '
    STRING(TIME,'hh:mm:ss am') ' : ' ipLogText SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix C-Win 
PROCEDURE get-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-first-disp as log no-undo.
  DEF OUTPUT PARAMETER ext-cost AS DEC NO-UNDO.
  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty as dec no-undo.
  def var lv-out-cost as dec no-undo.

  if not avail fg-rctd then return.  /* no records */

  cocode = fg-rctd.company.

 FOR EACH tt-email:
     DELETE tt-email.
 END.

if ip-first-disp  and avail fg-rctd and fg-rctd.i-no <> "" then do: /* for row-display */
  find itemfg  where itemfg.company eq cocode                           /* no screen-value used */
                     and itemfg.i-no  eq fg-rctd.i-no /*:screen-value in browse {&browse-name}*/
                     use-index i-no no-lock no-error.

  find first po-ordl where po-ordl.company = fg-rctd.company
                       and po-ordl.po-no = int(fg-rctd.po-no)
                       and po-ordl.i-no  = fg-rctd.i-no
                       and po-ordl.job-no = (fg-rctd.job-no)
                       and po-ordl.job-no2 = fg-rctd.job-no2
                       and po-ordl.item-type = no
                       no-lock no-error.

  if not avail po-ordl AND fg-rctd.po-no <> "" then return.

  lv-out-qty = fg-rctd.t-qty . /* fg-rctd.qty-case. ??? */
  /* convert cost pr-uom*/
  run rm/convcuom.p(fg-rctd.cost-uom, IF AVAIL po-ordl THEN po-ordl.cons-uom ELSE "EA",
                    0,0,0,0,fg-rctd.std-cost, output lv-out-cost).
  ext-cost = lv-out-qty * lv-out-cost.

end. /* avail fg-rctd */
/* ======================================================================= */
else if avail fg-rctd and fg-rctd.i-no <> "" then do: /* in update mode - use screen-value */
  find itemfg  where itemfg.company eq cocode
                and itemfg.i-no  eq fg-rctd.i-no
                use-index i-no no-lock no-error.
/*  if avail itemfg then v-dep = itemfg.s-dep.    */
  find first po-ordl where po-ordl.company = fg-rctd.company
                   /*    and po-ordl.po-no = integer(fg-rctd.po-no:screen-value in browse {&browse-name}) */
                       and po-ordl.i-no  = fg-rctd.i-no
                       and po-ordl.job-no = fg-rctd.job-no
                       and po-ordl.job-no2 = fg-rctd.job-no2
                       and po-ordl.item-type = no
                       no-lock no-error.

  if not avail po-ordl AND fg-rctd.po-no <> "" then return.

/*
  /* convert qty */
  run rm/convquom.p(fg-rctd.pur-uom:screen-value in browse {&browse-name} ,
                         po-ordl.cons-uom,
                         v-bwt,
                         v-len,
                         input v-wid,
                         input v-dep,
                         input fg-rctd.qty:screen-value in browse {&browse-name},
                         output lv-out-qty).
*/
  lv-out-qty = fg-rctd.t-qty  .
  /* convert cost */
  if avail po-ordl then assign v-len = po-ordl.s-len
                               v-wid = po-ordl.s-wid.
  else assign v-len = 0
              v-wid = 0.

  run rm/convcuom.p( fg-rctd.cost-uom,
                     IF AVAIL po-ordl THEN po-ordl.cons-uom ELSE "EA" ,
                             0,v-len,v-wid,0,
                             fg-rctd.std-cost, output lv-out-cost).

  ext-cost = lv-out-qty * lv-out-cost.

end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-ord-recs C-Win 
PROCEDURE get-ord-recs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAM ip-rowid1  AS  ROWID NO-UNDO.

  DEF PARAM BUFFER b-po-ordl FOR po-ordl.
  DEF PARAM BUFFER b-po-ord  FOR po-ord.
  DEF PARAM BUFFER b-oe-ordl FOR oe-ordl.
  DEF PARAM BUFFER b-oe-ord  FOR oe-ord.
  DEF PARAM BUFFER b-ref     FOR reftable.

  DEF BUFFER b-fg-rctd FOR fg-rctd.

  RELEASE b-po-ordl.
  RELEASE b-po-ord.
  RELEASE b-oe-ordl.
  RELEASE b-oe-ord.
  RELEASE b-ref.


  FIND b-fg-rctd WHERE ROWID(b-fg-rctd) EQ ip-rowid1 NO-LOCK NO-ERROR.

  IF AVAIL b-fg-rctd THEN DO:
    IF INT(b-fg-rctd.po-no) NE 0 AND b-fg-rctd.qty NE 0 THEN
    FIND FIRST b-po-ordl
        WHERE b-po-ordl.company   EQ b-fg-rctd.company
          AND b-po-ordl.po-no     EQ INT(b-fg-rctd.po-no)
          AND b-po-ordl.i-no      EQ b-fg-rctd.i-no
          AND b-po-ordl.job-no    EQ b-fg-rctd.job-no
          AND b-po-ordl.job-no2   EQ b-fg-rctd.job-no2
          AND b-po-ordl.item-type EQ NO
          AND b-po-ordl.ord-no    NE 0
        NO-LOCK NO-ERROR.

    IF AVAIL b-po-ordl THEN
    FIND FIRST b-po-ord
        WHERE b-po-ord.company EQ b-po-ordl.company
          AND b-po-ord.po-no   EQ b-po-ordl.po-no
          AND b-po-ord.type    EQ "D"
        NO-LOCK NO-ERROR.

    IF AVAIL b-po-ord THEN
    FIND FIRST b-oe-ordl
        WHERE b-oe-ordl.company  EQ b-po-ordl.company
          AND b-oe-ordl.ord-no   EQ b-po-ordl.ord-no
          AND b-oe-ordl.i-no     EQ b-po-ordl.i-no
          AND b-oe-ordl.vend-no  EQ b-po-ord.vend-no
          AND b-oe-ordl.po-no-po EQ b-po-ord.po-no
        NO-LOCK NO-ERROR.

    IF AVAIL b-oe-ordl THEN
    FIND FIRST b-oe-ord
        WHERE b-oe-ord.company EQ b-oe-ordl.company
          AND b-oe-ord.ord-no  EQ b-oe-ordl.ord-no
        NO-LOCK NO-ERROR.

    IF AVAIL oe-ord THEN
    FIND FIRST b-ref
        WHERE b-ref.reftable EQ "fg-rctd.user-id"
          AND b-ref.company  EQ b-fg-rctd.company
          AND b-ref.loc      EQ STRING(b-fg-rctd.r-no,"9999999999")
        NO-LOCK NO-ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rel-info C-Win 
PROCEDURE get-rel-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAM op-pono LIKE w-ord.cust-po-no NO-UNDO.
  DEF OUTPUT PARAM op-date LIKE w-ord.rel-date NO-UNDO.
  DEF OUTPUT PARAM op-lot# LIKE w-ord.rel-lot# NO-UNDO.


  RELEASE oe-rell.
  RELEASE oe-rel.

  IF v-po-no-source EQ "R" THEN DO:
    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.company  EQ oe-ordl.company
          AND oe-rell.ord-no   EQ oe-ordl.ord-no
          AND oe-rell.i-no     EQ oe-ordl.i-no
          AND oe-rell.line     EQ oe-ordl.line,

        FIRST oe-relh NO-LOCK
        WHERE oe-relh.r-no     EQ oe-rell.r-no
          AND oe-relh.posted   EQ NO
          AND oe-relh.rel-date GE ldt-from
          AND oe-relh.rel-date LE ldt-to
        BY oe-relh.rel-date
        BY oe-relh.r-no:

      ASSIGN
       op-pono = oe-rell.po-no
       op-date = oe-relh.rel-date.
      LEAVE.
    END.

    IF AVAIL oe-rell THEN
    FIND FIRST oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.

    ELSE
    FOR EACH oe-rel NO-LOCK
        WHERE oe-rel.company  EQ oe-ordl.company
          AND oe-rel.ord-no   EQ oe-ordl.ord-no
          AND oe-rel.i-no     EQ oe-ordl.i-no
          AND oe-rel.line     EQ oe-ordl.line
          AND oe-rel.rel-no   EQ 0
          AND oe-rel.rel-date GE ldt-from
          AND oe-rel.rel-date LE ldt-to
        BY oe-rel.rel-date
        BY oe-rel.r-no:

      ASSIGN
       op-pono = oe-rel.po-no
       op-date = oe-rel.rel-date.
      LEAVE.
    END.
  END.

  IF NOT AVAIL oe-rel THEN
  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company  EQ oe-ordl.company
        AND oe-rel.ord-no   EQ oe-ordl.ord-no
        AND oe-rel.i-no     EQ oe-ordl.i-no
        AND oe-rel.line     EQ oe-ordl.line
      BY oe-rel.rel-date
      BY oe-rel.r-no:

    op-date = oe-rel.rel-date.
    LEAVE.
  END.

  IF AVAIL oe-rel THEN DO:
    FIND FIRST ref-lot-no NO-LOCK
        WHERE ref-lot-no.reftable EQ "oe-rel.lot-no"
          AND ref-lot-no.company  EQ STRING(oe-rel.r-no,"9999999999")
        NO-ERROR.
    IF AVAIL ref-lot-no THEN op-lot# = ref-lot-no.code.
  END.

  IF v-po-no-source NE "R"                    OR
     (NOT AVAIL oe-rel AND NOT AVAIL oe-rell) THEN
    op-pono = IF v-po-no-source EQ "L" THEN oe-ordl.po-no
                                       ELSE IF AVAIL oe-ord THEN oe-ord.po-no
                                       ELSE "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gl-from-work C-Win 
PROCEDURE gl-from-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-run AS INT NO-UNDO.
  DEF INPUT PARAM ip-trnum AS INT NO-UNDO.

  def var credits as dec init 0 no-undo.
  def var debits as dec init 0 no-undo. 


  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
      NO-LOCK.

  for each work-gl 
      where (ip-run eq 1 and work-gl.job-no ne "")
         or (ip-run eq 2 and work-gl.job-no eq "")
      break by work-gl.actnum:

    assign
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    if last-of(work-gl.actnum) then do:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "FGPOST"
       gltrans.period  = period.pnum
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = v-post-date
       gltrans.tr-dscr = if work-gl.job-no ne "" then "FG Receipt from Job"
                                                 else "FG Receipt from PO"
       gltrans.trnum   = ip-trnum
       debits  = 0
       credits = 0.

      RELEASE gltrans.
    end.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-values C-Win 
PROCEDURE init-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /*FOR EACH fg-rctd NO-LOCK
      WHERE fg-rctd.company  EQ gcompany
        AND fg-rctd.rct-date GE TODAY - 15
      USE-INDEX rct-detail:

    IF fg-rctd.rita-code EQ "R" THEN t-receipt = YES.
    ELSE
    IF fg-rctd.rita-code EQ "I" THEN t-ship   = YES.
    ELSE
    IF fg-rctd.rita-code EQ "T" THEN t-trans   = YES.
    ELSE
    IF fg-rctd.rita-code EQ "A" THEN t-adj     = YES.
    ELSE
    IF fg-rctd.rita-code EQ "E" THEN t-ret     = YES.

    IF t-receipt AND t-ship AND t-trans AND t-adj AND t-ret THEN LEAVE.
  END. */

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     t-receipt:SCREEN-VALUE = STRING(CAN-FIND(FIRST fg-rctd
                                       WHERE fg-rctd.company   EQ gcompany
                                         AND fg-rctd.rita-code EQ "R"))
     t-ship:SCREEN-VALUE    = STRING(CAN-FIND(FIRST fg-rctd
                                       WHERE fg-rctd.company   EQ gcompany
                                         AND fg-rctd.rita-code EQ "S"))
     t-trans:SCREEN-VALUE   = STRING(CAN-FIND(FIRST fg-rctd
                                       WHERE fg-rctd.company   EQ gcompany
                                         AND fg-rctd.rita-code EQ "T"))
     t-adj:SCREEN-VALUE     = STRING(CAN-FIND(FIRST fg-rctd
                                       WHERE fg-rctd.company   EQ gcompany
                                         AND fg-rctd.rita-code EQ "A"))
     t-ret:SCREEN-VALUE     = STRING(CAN-FIND(FIRST fg-rctd
                                       WHERE fg-rctd.company   EQ gcompany
                                         AND fg-rctd.rita-code EQ "E")).
     tgIssue:SCREEN-VALUE   = STRING(CAN-FIND(FIRST fg-rctd
                                       WHERE fg-rctd.company   EQ gcompany
                                         AND fg-rctd.rita-code EQ "F")).
  END.

  /* Check and set FGEMAIL parameter. */
  RUN Check-Fgemail-Parm.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE manualFarmOut C-Win 
PROCEDURE manualFarmOut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cJob AS CHAR NO-UNDO.
DEF VAR iJobNo2 AS INT NO-UNDO.
DEF BUFFER bf-fg-rctd FOR fg-rctd.

  FIND itemfg WHERE itemfg.company EQ cocode 
      AND itemfg.i-no EQ w-fg-rctd.i-no
      NO-LOCK NO-ERROR.

  IF NOT AVAIL itemfg THEN
      RETURN.
  cJob = "".
  iJobNo2 = 0.   

  IF (w-fg-rctd.job-no GT "" OR w-fg-rctd.po-no GT "") AND itemfg.pur-man THEN DO:

      /* Find a job for this po if this is a farmout */
      IF w-fg-rctd.job-no GT "" THEN
          ASSIGN cJob = w-fg-rctd.job-no
                 iJobNo2 = w-fg-rctd.job-no2.
      ELSE IF w-fg-rctd.po-no GT "" THEN  DO:
           FIND FIRST po-ordl WHERE po-ordl.company EQ w-fg-rctd.company
               AND po-ordl.po-no EQ INTEGER(w-fg-rctd.po-no)
               AND po-ordl.i-no  EQ w-fg-rctd.i-no
               NO-LOCK NO-ERROR.

           IF AVAIL(po-ordl) AND po-ordl.ord-no GT 0 THEN DO:

              FIND FIRST oe-ordl WHERE oe-ordl.company EQ g_company
                  AND oe-ordl.ord-no EQ po-ordl.ord-no
                  AND oe-ordl.i-no   EQ po-ordl.i-no
                  NO-LOCK NO-ERROR.
              /* assumption is that for farm jobs, order and job are always the same */
              /* This is to obtain the job-no2 since job-no is assumed to be the order # */
              IF NOT AVAIL oe-ordl THEN
                  FIND FIRST oe-ordl WHERE oe-ordl.company EQ g_company
                      AND oe-ordl.ord-no EQ po-ordl.ord-no
                      AND oe-ordl.job-no EQ string(po-ordl.ord-no)
                      NO-LOCK NO-ERROR.

              IF AVAIL oe-ordl AND oe-ordl.job-no GT "" THEN
                  ASSIGN cJob = oe-ordl.job-no
                         iJobNo2 = oe-ordl.job-no2.
           END.

      END.

      FIND FIRST job WHERE job.company EQ w-fg-rctd.company
          AND job.job-no EQ cJob
          AND job.job-no2 EQ iJobNo2
          NO-LOCK NO-ERROR.

      IF AVAIL job AND cJob GT "" 
                   AND w-fg-rctd.rita-code EQ "F" 
                   THEN DO:             

          /* Copy fg-rctd for the jobs farmout tab */
          CREATE job-farm-rctd.
          BUFFER-COPY w-fg-rctd EXCEPT rec_key TO job-farm-rctd.
          ASSIGN job-farm-rctd.job-no = cJob
                 job-farm-rctd.job-no2 = iJobNo2.
          /* ASSIGN job-farm-rctd.job = job.job. */

          RUN jc/updJobFarmActual.p (INPUT ROWID(job), INPUT w-fg-rctd.i-no).
      END.
      FIND bf-fg-rctd WHERE ROWID(bf-fg-rctd) EQ w-fg-rctd.row-id
          EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL bf-fg-rctd THEN
          DELETE bf-fg-rctd.


  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE orig C-Win 
PROCEDURE orig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /*find first item finished goods based on the item number*/
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-fg-rctd.i-no
        use-index i-no no-lock no-error.

    if avail itemfg then do:
      find first uom
          where uom.uom  eq itemfg.sell-uom
            and uom.mult ne 0
          no-lock no-error.

      if itemfg.sell-uom begins "L" then
        v-fg-value = itemfg.sell-price * IF w-fg-rctd.t-qty LT 0 THEN -1 ELSE 1.

      else
      if itemfg.sell-uom eq "CS" then
        v-fg-value = itemfg.sell-price * w-fg-rctd.cases.

      else
      if avail uom then
        v-fg-value = itemfg.sell-price * w-fg-rctd.t-qty / uom.mult.

      else
        v-fg-value = itemfg.sell-price * w-fg-rctd.t-qty / 1000.

      if w-fg-rctd.rita-code eq "R" then do:
        if v-msf[1] gt w-fg-rctd.t-qty * itemfg.t-sqft then
          v-msf[2] = v-msf[2] + (v-msf[1] - (w-fg-rctd.t-qty * itemfg.t-sqft)).

        v-msf[1] = w-fg-rctd.t-qty * itemfg.t-sqft.
      end.
    end. /* avail itemfg */

    assign
     v-msf[1] = v-msf[1] / 1000
     v-msf[2] = v-msf[2] / 1000.

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
{custom\out2file.i}  

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

  run scr-rpt.w (list-name,frame {&frame-name}:title,int(lv-font-no),lv-ornt). /* open file-name, title */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-and-post C-Win 
PROCEDURE print-and-post :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-r-no LIKE rm-rctd.r-no NO-UNDO.
  DEF VAR lContinue AS LOGICAL NO-UNDO.
  /* 11111302 - Automatically include set components instead of this validate*/
  /* RUN ValidateFGItemRange(OUTPUT lContinue). */
/*   IF NOT lContinue THEN DO:                               */
/*       APPLY "entry" TO begin_i-no IN FRAME {&FRAME-NAME}. */
/*       RETURN.                                             */
/*   END.                                                    */
  FOR EACH w-fg-rctd:
    DELETE w-fg-rctd.
  END.

  FOR EACH work-gl:
    DELETE work-gl.
  END.

  FOR EACH work-job:
    DELETE work-job.
  END.

  RUN run-report.

  IF fgpost-cha EQ "Before" OR fgpost-cha EQ "Both" THEN RUN show-report (1).

  choice = CAN-FIND(FIRST w-fg-rctd WHERE w-fg-rctd.has-rec).

  IF choice THEN
  FOR EACH w-fg-rctd
      WHERE w-fg-rctd.has-rec
        AND NOT CAN-FIND(FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id):
    choice = NO.
    LEAVE.
  END.

  IF choice THEN DO:
    choice = NO.
    MESSAGE "Are you ready to post to finished goods?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE choice.
  END.

  ELSE MESSAGE "Sorry, nothing is available for posting..."
           VIEW-AS ALERT-BOX.

  IF choice THEN DO:
    FOR EACH w-fg-rctd
        WHERE w-fg-rctd.has-rec
          AND CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.r-no EQ w-fg-rctd.r-no),
        FIRST fg-rctd
        WHERE ROWID(fg-rctd)    EQ w-fg-rctd.row-id
          AND fg-rctd.rita-code NE "P":
      lv-r-no = fg-rctd.r-no.
      DO TRANSACTION:
        fg-rctd.r-no = 0.
      END.
      DO TRANSACTION:
        fg-rctd.r-no = lv-r-no.
        /* 06121406 - If r-no is changed by write trigger, must change linked records also */
        IF fg-rctd.r-no NE lv-r-no THEN DO:        
            FOR EACH fg-rcpts 
               WHERE fg-rcpts.company EQ fg-rctd.company 
                 AND fg-rcpts.linker EQ "fg-rctd: " + STRING(lv-r-no,"9999999999") 
               EXCLUSIVE-LOCK:

              fg-rcpts.linker = "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999").
              FOR EACH reftable 
                WHERE reftable.reftable EQ "fg-rctd.user-id"
                  AND reftable.company  EQ fg-rcpts.company
                  AND reftable.loc      EQ STRING(fg-rcpts.r-no,"9999999999")        /* component */
                  AND (reftable.dscr EQ "fg-rctd: " + STRING(lv-r-no, "9999999999")  /* set header r-no */
                       AND reftable.dscr begins "fg-rctd: ")  
                USE-INDEX loc   EXCLUSIVE-LOCK:
                reftable.dscr = "fg-rctd: " + STRING(fg-rctd.r-no, "9999999999").
              END. /* each reftable */
            END. /* each fg-rcpts */
        END. /* If r-no was changed by trigger */
      END. /* do trans */
      w-fg-rctd.r-no = fg-rctd.r-no.
    END. /* each w-fg-rctd */

    FOR EACH w-fg-rctd WHERE w-fg-rctd.has-rec,
        FIRST fg-rctd NO-LOCK WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id,
        FIRST fg-rcpth NO-LOCK WHERE fg-rcpth.r-no EQ fg-rctd.r-no:

      MESSAGE "Sorry, these FG Transactions cannot be processed because 1 or " +
              "more have already been posted by UserID: " +
              TRIM(fg-rcpth.user-id) SKIP
              "     Item " + fg-rctd.i-no + " / " fg-rcpth.i-no SKIP
              "     Sequence: " + string(fg-rctd.r-no)
          VIEW-AS ALERT-BOX ERROR.

      choice = NO.
      LEAVE.
    END.

    FOR EACH w-fg-rctd WHERE w-fg-rctd.has-rec,
        FIRST fg-rctd NO-LOCK WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id:

      FIND FIRST fg-bin 
         WHERE fg-bin.company = fg-rctd.company 
        AND fg-bin.loc = fg-rctd.loc 
        AND fg-bin.i-no = ""
        AND fg-bin.loc-bin = fg-rctd.loc-bin
        NO-LOCK NO-ERROR.
      IF NOT AVAIL fg-bin THEN DO:      
        MESSAGE "Sorry, these FG Transactions cannot be processed because 1 or " +
            "more have an invalid bin. Please correct and retry. "  SKIP          
            "     Item " + fg-rctd.i-no  SKIP
            "     Whse " + fg-rctd.loc   SKIP
            "      Bin " + fg-rctd.loc-bin SKIP
            "     Sequence: " + string(fg-rctd.r-no)
        VIEW-AS ALERT-BOX ERROR.

        choice = NO.
        LEAVE.
      END.
    END.

  END.

  IF choice THEN DO: 

    RUN fg-post. 

    IF NOT ERROR-STATUS:ERROR OR tgIssue THEN DO:

      MESSAGE "Posting completed..." VIEW-AS ALERT-BOX.
      IF fgpost-cha EQ "After" OR fgpost-cha EQ "Both" THEN RUN show-report (2).
      IF can-find(FIRST w-fg-rctd WHERE w-fg-rctd.rita-code EQ "T")
         THEN RUN ReprintTag.

    END.

  END.



  RUN Send-FGemail-Purchased.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Process-FGemail-Data C-Win 
PROCEDURE Process-FGemail-Data :
/*------------------------------------------------------------------------------
  Purpose:     Create FG email temp-table record for received item.
  Parameters:  item number, qty received
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER pc-i-no  AS CHAR NO-UNDO.
 DEFINE INPUT PARAMETER pi-qty   AS DEC NO-UNDO.
 DEFINE INPUT PARAMETER pc-po-no AS CHAR NO-UNDO.


 DEFINE BUFFER buf-oe-ordl FOR oe-ordl.
 DEFINE BUFFER buf-oe-ord  FOR oe-ord.
 DEFINE BUFFER bf-po-ordl FOR po-ordl.
 DEFINE VARIABLE cUserList AS CHAR NO-UNDO INIT "".
 DEFINE VARIABLE xOrdNo LIKE oe-ordl.ord-no NO-UNDO.


    FIND FIRST bf-po-ordl 
        WHERE bf-po-ordl.company EQ g_company
          AND bf-po-ordl.po-no EQ INT(pc-po-no)
          AND bf-po-ordl.i-no EQ pc-i-no
          AND bf-po-ordl.item-type EQ NO
        NO-LOCK NO-ERROR.

    IF AVAIL bf-po-ordl THEN DO:
        xOrdNo = bf-po-ordl.ord-no.
   /* Find user ID on each open order of item. */
        FOR EACH buf-oe-ordl   
            WHERE buf-oe-ordl.company   = g_company 
              AND buf-oe-ordl.opened    = YES 
              AND buf-oe-ordl.i-no      = pc-i-no
              AND buf-oe-ordl.ord-no    = xOrdNo
            NO-LOCK:

            FIND buf-oe-ord OF buf-oe-ordl NO-LOCK NO-ERROR.

            /* If no order header, then skip this orderline. */
            IF NOT AVAILABLE buf-oe-ord THEN NEXT.

            FIND FIRST users NO-LOCK WHERE
                       users.USER_id = buf-oe-ord.USER-ID NO-ERROR.

            /* If no user, then skip. */
            IF NOT AVAILABLE users THEN NEXT.

            /* If no user email address, then skip. */
            IF users.image_filename = "" OR users.image_filename = ? THEN NEXT.

            /* Create email record for this received item. */
            CREATE tt-fgemail.
            ASSIGN tt-fgemail.i-no    = pc-i-no
                   tt-fgemail.po-no   = pc-po-no
                   tt-fgemail.ord-no  = buf-oe-ordl.ord-no
                   tt-fgemail.qty-rec = pi-qty
                   tt-fgemail.Recipient = TRIM(users.image_filename).

        END.  /*each buf-oe-ordl*/
    END. /*Avail bf-po-ordl*/
/*     ASSIGN cUserList = TRIM(cUserList,","). */


 RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-rel-stat C-Win 
PROCEDURE process-rel-stat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipr-ordl-row AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER opi-stat AS INT NO-UNDO.
DEF OUTPUT PARAMETER opi-rel-qty AS INT NO-UNDO.
DEF OUTPUT PARAMETER opr-release AS ROWID NO-UNDO.

DEF VAR v-highest-stat AS INT NO-UNDO.
DEF VAR v-tot-rqty AS INT NO-UNDO.
DEF VAR stat-type AS INT NO-UNDO.
DEF VAR v-chosen-rel AS ROWID NO-UNDO.
DEF BUFFER bf-oe-ordl FOR oe-ordl.
FIND FIRST bf-oe-ordl WHERE ROWID(bf-oe-ordl) EQ ipr-ordl-row NO-LOCK NO-ERROR.

      v-highest-stat = 0.
      FOR EACH oe-rel WHERE oe-rel.company EQ bf-oe-ordl.company
                        AND oe-rel.ord-no  EQ bf-oe-ordl.ord-no
                        AND oe-rel.i-no    EQ bf-oe-ordl.i-no
                        AND oe-rel.LINE    EQ bf-oe-ordl.LINE
                      NO-LOCK.
          /* Determine the status of each to know how to proceed */
          FIND FIRST oe-rell WHERE oe-rell.company  EQ oe-rel.company
                               /* This doesn't work with normal release from ou1 */
                               /* AND oe-rell.r-no     EQ oe-rel.link-no */
                               AND oe-rell.ord-no   EQ oe-rel.ord-no
                               /* AND oe-rell.rel-no   EQ oe-rel.rel-no */
                               /* AND oe-rell.b-ord-no EQ oe-rel.b-ord-no */
                               AND oe-rell.i-no     EQ oe-rel.i-no
                               AND oe-rell.line     EQ oe-rel.line
                               /* AND oe-rell.po-no    EQ oe-rel.po-no */
                               AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
                             USE-INDEX ord-no NO-LOCK NO-ERROR.
          IF AVAIL oe-rel THEN
            v-tot-rqty = v-tot-rqty + oe-rel.qty.

            IF LOOKUP(oe-rel.stat, "S,I,L") GT 0 THEN
                stat-type = 1.
            ELSE DO:
                IF LOOKUP(oe-rel.stat, "A,B") GT 0 AND AVAIL oe-rell AND NOT oe-rell.printed THEN
                        stat-type = 2.
                IF LOOKUP(oe-rel.stat, "A,B,P,Z,C") GT 0 AND AVAIL oe-rell AND oe-rell.printed THEN
                        stat-type = 3.
            END.
            IF stat-type GT v-highest-stat THEN
                ASSIGN v-highest-stat = stat-type v-chosen-rel = ROWID(oe-rel).
      END.

      ASSIGN
        opi-stat = v-highest-stat
        opi-rel-qty = v-tot-rqty
        opr-release = v-chosen-rel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-releases C-Win 
PROCEDURE process-releases :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-tot-rqty AS INT NO-UNDO.
  DEF VAR v-rel-qty  AS INT NO-UNDO.
  DEF VAR v-found-order LIKE oe-ord.ord-no NO-UNDO.
  DEF VAR v-managed AS LOG NO-UNDO.
  DEF VAR stat-type AS INT NO-UNDO.
  DEF VAR new-qty AS INT NO-UNDO.
  DEF VAR add-qty AS INT NO-UNDO.
  DEF VAR v-highest-stat AS INT NO-UNDO.
  DEF VAR v-chosen-rel AS ROWID NO-UNDO.
  DEF VAR v-tot-rcv-qty AS INT NO-UNDO.

  /* To Be Implemented */
  DEF VAR nk-set AS LOG NO-UNDO.

  IF fgpost-int = 1 THEN
    nk-set = TRUE.

  FOR EACH w-fg-rctd
      BREAK BY w-fg-rctd.i-no:

      v-tot-rqty = v-tot-rqty + w-fg-rctd.qty.

      IF LAST-OF(w-fg-rctd.i-no) THEN DO:


        FIND FIRST job-hdr WHERE job-hdr.company = w-fg-rctd.company
                             AND job-hdr.job-no = w-fg-rctd.job-no
                             AND job-hdr.job-no2 = w-fg-rctd.job-no2
                           NO-LOCK NO-ERROR.
        IF AVAIL job-hdr THEN
        FIND FIRST oe-ord WHERE oe-ord.company = job-hdr.company
                            AND oe-ord.ord-no  = integer(job-hdr.job-no)
                          NO-LOCK NO-ERROR.
        IF AVAIL oe-ord THEN
          FIND FIRST oe-ordl WHERE oe-ordl.company = oe-ord.company
                               AND oe-ordl.ord-no  = oe-ord.ord-no
                               AND oe-ordl.i-no    = w-fg-rctd.i-no
                             NO-LOCK NO-ERROR.

        IF AVAIL oe-ordl THEN
            FIND FIRST oe-rel WHERE oe-rel.company = oe-ordl.company
                                AND oe-rel.ord-no  = oe-ordl.ord-no
                                AND oe-rel.LINE    = oe-ordl.LINE
                              NO-LOCK NO-ERROR.

        FIND FIRST itemfg WHERE itemfg.company EQ cocode 
                            AND itemfg.i-no    EQ w-fg-rctd.i-no
                          NO-LOCK NO-ERROR.
        IF AVAIL oe-ordl THEN
        RUN process-rel-stat (INPUT ROWID(oe-ordl), 
                           OUTPUT v-highest-stat, 
                           OUTPUT v-rel-qty,
                           OUTPUT v-chosen-rel).
       FIND oe-rel WHERE ROWID(oe-rel) EQ v-chosen-rel NO-LOCK NO-ERROR.
       v-tot-rcv-qty = get-tot-rcv-qty().

       /* Managed Inventory indicator */
       IF AVAIL(oe-ordl) AND avail(oe-ord) THEN
          v-managed = oe-ord.whsed OR oe-ordl.whsed.

       IF AVAIL(oe-rel) 
          AND v-managed = YES
          AND avail(oe-ordl) 
          AND nk-set 
          AND v-tot-rcv-qty GT oe-ordl.qty THEN DO WITH FRAME {&FRAME-NAME}:

            /* Set release qty to total received - scheduled release */
            add-qty = v-tot-rcv-qty - v-rel-qty.
            new-qty = v-tot-rcv-qty.
            stat-type = v-highest-stat.

            CASE stat-type:

                WHEN 1 THEN DO:              
                   /* Replace release qty */
                   RUN replace-rel-qty (INPUT ROWID(oe-rel), INPUT new-qty).

                END.
                WHEN 2 THEN DO:
                  FIND FIRST oe-rell
                      WHERE oe-rell.company  EQ oe-rel.company
                        /* AND oe-rell.r-no     EQ oe-rel.link-no */
                        AND oe-rell.ord-no   EQ oe-rel.ord-no
                        /* AND oe-rell.rel-no   EQ oe-rel.rel-no */
                        /* AND oe-rell.b-ord-no EQ oe-rel.b-ord-no */
                        AND oe-rell.i-no     EQ oe-rel.i-no
                        AND oe-rell.line     EQ oe-rel.line
                        /* AND oe-rell.po-no    EQ oe-rel.po-no */
                        AND oe-rell.printed  EQ NO
                        AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
                      USE-INDEX r-no NO-LOCK NO-ERROR.

                   /* Replace release qty and actual release qty */
                   RUN replace-rel-qty (INPUT ROWID(oe-rel), INPUT new-qty).
                   IF AVAIL oe-rell THEN
                     RUN replace-actrel-qty (INPUT ROWID(oe-rell), INPUT new-qty).

                END.
                WHEN 3 THEN DO:
                   /* Create Release from existing */
                   RUN add-rel-for-qty (INPUT ROWID(oe-rel), INPUT add-qty).

                END.

            END CASE.

            ASSIGN
              v-rel-qty     = 0
              v-tot-rcv-qty    = 0
              v-found-order = 0.

      END. /* all conditions met */
    END. /* last i-no */
  END. /* each w-fg-rctd */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE replace-actrel-qty C-Win 
PROCEDURE replace-actrel-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipr-rell-row AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipi-qty     AS INT   NO-UNDO.
/* wfk - 11/12 - did not find any extra logic that needs to be applied 
   when oe-rell.qty is changed */
DEF BUFFER bf-oe-rell FOR oe-rell.
FIND bf-oe-rell WHERE ROWID(bf-oe-rell) EQ ipr-rell-row
                EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL bf-oe-rell THEN
    RETURN.
bf-oe-rell.qty = ipi-qty.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE replace-rel-qty C-Win 
PROCEDURE replace-rel-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipr-rel-row AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipi-qty     AS INT   NO-UNDO.
DEF BUFFER bf-orig-oe-rel FOR oe-rel.
FIND bf-orig-oe-rel WHERE ROWID(bf-orig-oe-rel) EQ ipr-rel-row
                    EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL bf-orig-oe-rel THEN
    RETURN.

IF bf-orig-oe-rel.tot-qty EQ bf-orig-oe-rel.qty THEN DO:
  bf-orig-oe-rel.tot-qty = ipi-qty.

END.
  bf-orig-oe-rel.qty = ipi-qty.
RUN add-rel-assign-logic (INPUT ipr-rel-row, INPUT ipi-qty).
RELEASE bf-orig-oe-rel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReprintTag C-Win 
PROCEDURE ReprintTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv-found-recs AS LOG NO-UNDO.
 lv-found-recs = NO.
 FOR EACH w-fg-rctd WHERE w-fg-rctd.rita-code EQ "T"
      BY w-fg-rctd.tag
      BY w-fg-rctd.rct-date
      BY w-fg-rctd.trans-time
      BY w-fg-rctd.r-no:



     FIND FIRST loadtag WHERE loadtag.company     EQ cocode
                 AND loadtag.item-type   EQ NO
                 AND loadtag.tag-no  eq w-fg-rctd.tag2 NO-LOCK NO-ERROR.
  IF NOT AVAIL loadtag THEN NEXT.

  RUN create-w-ord.
  lv-found-recs = YES.
 END.

  SESSION:SET-WAIT-STATE ("general").
  FIND FIRST sys-ctrl NO-LOCK 
          WHERE sys-ctrl.company EQ gcompany
            AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
      IF NOT AVAIL sys-ctrl THEN
          DO TRANSACTION:
          CREATE sys-ctrl.
          ASSIGN
              sys-ctrl.company  = gcompany
              sys-ctrl.name     = "BARDIR"
              sys-ctrl.descrip  = "C:\BA\Label\".
          FIND CURRENT sys-ctrl NO-LOCK.
      END.
      v-out = sys-ctrl.descrip.

  IF v-out = "" THEN v-out = "c:~\ba~\label~\loadtag.txt".
  ELSE do:
     IF SUBSTRING(v-out,LENGTH(v-out),1) = "/" OR
        SUBSTRING(v-out,LENGTH(v-out),1) = "\" THEN .
     ELSE v-out = v-out + "/".
     v-out = v-out + "loadtag.txt".
  END.

  RUN create-text-file.

  IF (NOT is-from-addons() OR SSLoadTag-log = TRUE) AND lv-found-recs THEN
    MESSAGE "Loadtag reprint is completed." VIEW-AS ALERT-BOX INFORMATION.
  SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{sys/form/r-top.i}

{sys/inc/ctrtext.i str-tit 112}.

{sys/form/r-top3w1.f "Before"}

{sys/form/r-top3w1.f "After"}

DEF VAR ext-cost AS DEC NO-UNDO.
def var type as ch format "X" initial "R".
def var type-prt as ch format "X(11)" init "".
def var v-fg-qty like fg-rctd.t-qty.
def var v-fg-cost as dec format "->,>>>,>>9.99<<".
def var v-tot-qty as int format "->>>,>>>,>>9".
def var v-tot-cost as dec format "->>>,>>9.99<<".
def var v-grd-tot-qty as int format "->>>,>>>,>>9".
def var v-grd-tot-cost as dec format "->>,>>>,>>9.99<<".                     
def var v-grd-tot-value as dec format "->>,>>>,>>9.99<<".                     
def var v-tot-value as dec format "->>,>>>,>>9.99".
def var v-cum-tot as de.                                   
def var v-tran-type as char format "x(1)".      
def var v-entrytype as char initial "REC ,TRAN,ADJ ,SHIP,RET ,INIT".
def var v-on like eb.num-up.
def var v-qty-pallet as decimal format "->>,>>>,>>9" no-undo.
def var v-whse like fg-rctd.loc.            
def var v-one as integer format "->>,>>9" init 1.
def var v-ftime as logical init no.
def var v-dscr          like account.dscr.
def var v-disp-actnum   like account.actnum.
def var v-disp-amt      as   dec format ">>,>>>,>>9.99cr".
def var v-hdr as char format "x(12)".
def var v-postlst  as cha no-undo.
DEF VAR ll-wip AS LOG NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR li-loop AS INT NO-UNDO.
DEF VAR v-time AS CHAR FORMAT "X(5)" NO-UNDO.

DEF VAR v-itm-lbl  AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-itm-dsh  AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-desc-lbl AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-Po-lbl   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-vend-lbl AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-desc-dsh AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-Po-dsh   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-vend-dsh AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-uom-lbl  AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-uom-dsh  AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-cstprt   AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-pr-tots2 LIKE v-pr-tots         NO-UNDO.

IF rd-Itm#Cst# EQ 1 
  THEN ASSIGN v-itm-lbl = "ITEM"
              v-itm-dsh = "---------------".
  ELSE ASSIGN v-itm-lbl = "CUSTOMER PART #"
              v-itm-dsh = "---------------".

IF rd-ItmPo EQ 1   
  THEN ASSIGN v-desc-lbl = "DESCRIPTION                           "
              v-Po-lbl   = ""
              v-vend-lbl = ""
              v-desc-dsh = "------------------------------".

  ELSE ASSIGN v-desc-lbl = "DESCRIPTION"
              v-Po-lbl   = "P.O. #"
              v-vend-lbl = "VEND"
              v-desc-dsh = "-------------- --------- --------".

IF rd-UOMJob EQ 1 
  THEN ASSIGN v-uom-lbl = "UOM"
              v-uom-dsh = "----".
  ELSE ASSIGN v-uom-lbl = "JOB #"
              v-uom-dsh = "----------".

FORM HEADER
     SPACE(56) "PRE POST AUDIT TRAIL"
    WITH FRAME before STREAM-IO WIDTH 132 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

FORM HEADER
     SPACE(57) "POSTED AUDIT TRAIL"
    WITH FRAME after STREAM-IO WIDTH 132 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

FORM HEADER
     "WHSE:"
     v-whse
     SKIP    
     "         TOTAL"   at 128    
     "DATE"             at 1
     "TIME"             AT 10
     TRIM(v-itm-lbl)  FORMAT "x(15)"  at 16
     TRIM(v-desc-lbl) FORMAT "x(11)"  at 32
     TRIM(v-Po-lbl)     at 47
     TRIM(v-vend-lbl)  FORMAT "X(4)" at 57
     "T"                at 63
     "TAG #"            at 65
     "UNITS"            at 88  
     "COUNT"            at 97
     "TOTAL"            at 106
     "BIN"              at 112    
     TRIM(v-uom-lbl) FORMAT "x(10)" at 119
     v-hdr                  at 130
     "--------"             at 1                /*date*/
     "----"                 AT 10               /* time */                
     TRIM(v-itm-dsh)  FORMAT "x(15)" at 16       /*item*/
     TRIM(v-desc-dsh) FORMAT "x(30)" at 32      /*description p.o. # vendor*/
     "-"                    at 63               /*t*/
     "--------------------" at 65               /*tag # 8 -> 20*/
     "-------"              at 86               /*units*/
     "--------"             at 94               /*count*/
     "--------"             at 103              /*total 11->8*/
     "------"               at 112              /*bin  8 -> 6*/    
     TRIM(v-uom-dsh) FORMAT "x(10)" at 119              /*uom*/
     "------------"         at 130              /*total value 14 -> 12*/
    with frame r-top1 STREAM-IO width 170 no-labels no-box no-underline page-top.

/*form #1 Print cases / qty case for TOTAL COST*/
form w-fg-rctd.rct-date             format "99/99/99" 
     v-time                                           
     w-fg-rctd.i-no                 format "x(15)"    
     w-fg-rctd.i-name               format "x(14)"    
     w-fg-rctd.po-no                                  
     po-ord.vend-no                 FORMAT "x(5)"                                  
     v-tran-type                                      
     w-fg-rctd.tag                  FORM "x(20)"      
     w-fg-rctd.cases                format "->>,>>9"  
     w-fg-rctd.qty-case             format "->>>,>>9" 
     v-fg-qty                       format "->>>,>>9" 
     w-fg-rctd.loc-bin              FORM "x(6)"       
     w-fg-rctd.pur-uom              FORMAT "x(9)"     
     v-fg-cost
    with frame itemx no-box down STREAM-IO width 170 no-labels.

form w-fg-rctd.rct-date             format "99/99/99" AT 1  
     v-time                                           AT 10 
     w-fg-rctd.i-no                 format "x(15)"    AT 16 
     w-fg-rctd.i-name               format "x(27)"    AT 32 
     v-tran-type                                      AT 63 
     w-fg-rctd.tag                  FORM "x(20)"      AT 65 
     w-fg-rctd.cases                format "->>,>>9"  AT 86 
     w-fg-rctd.qty-case             format "->>>,>>9" AT 94 
     v-fg-qty                       format "->>>,>>9" AT 103
     w-fg-rctd.loc-bin              FORM "x(6)"       AT 112
     w-fg-rctd.pur-uom              FORMAT "x(9)"     AT 119
     v-fg-cost 
    with frame itemxA no-box down STREAM-IO width 170 no-labels.

/*form #2 Print 1 / partial for TOTAL COST*/
form w-fg-rctd.rct-date             format "99/99/99"
     v-time                                          
     w-fg-rctd.i-no                 format "x(15)"   
     w-fg-rctd.i-name               format "x(14)"   
     w-fg-rctd.po-no                                 
     po-ord.vend-no                 FORMAT "x(5)"                   
     v-tran-type                                     
     w-fg-rctd.tag                FORM "x(20)"       
     v-one                          format "->>,>>9" 
     w-fg-rctd.partial              format "->>>,>>9"
     v-fg-qty                       format "->>>,>>9"
     w-fg-rctd.loc-bin              FORM "x(6)"      
     w-fg-rctd.pur-uom              FORMAT "x(9)"    
     v-fg-cost  
    with frame itempx no-box down STREAM-IO width 170 no-labels.

form w-fg-rctd.rct-date             format "99/99/99"   AT 1  
     v-time                                             AT 10 
     w-fg-rctd.i-no                 format "x(15)"      AT 16 
     w-fg-rctd.i-name               format "x(27)"      AT 32 
     v-tran-type                                        AT 63 
     w-fg-rctd.tag                  FORM "x(20)"        AT 65 
     v-one                          format "->>,>>9"    AT 86 
     w-fg-rctd.partial              format "->>>,>>9"   AT 94 
     v-fg-qty                       format "->>>,>>9"   AT 103
     w-fg-rctd.loc-bin              FORM "x(6)"         AT 112
     w-fg-rctd.pur-uom              FORMAT "x(9)"       AT 119
     v-fg-cost                                       
    with frame itempxA no-box down STREAM-IO width 170 no-labels.

/*form #3 Print cases / qty case for TOTAL VALUE*/
form w-fg-rctd.rct-date             format "99/99/99"   
     v-time                                             
     w-fg-rctd.i-no                 format "x(15)"      
     w-fg-rctd.i-name               format "x(14)"      
     w-fg-rctd.po-no                                    
     po-ord.vend-no                 FORMAT "x(5)"                                   
     v-tran-type                                        
     w-fg-rctd.tag                  FORM "x(20)"        
     w-fg-rctd.cases                format "->>,>>9"    
     w-fg-rctd.qty-case             format "->>>,>>9"   
     v-fg-qty                       format "->>>,>>9"   
     w-fg-rctd.loc-bin              FORM "x(6)"         
     w-fg-rctd.pur-uom              FORMAT "x(9)"       
     v-fg-value 
    with frame itemy no-box down STREAM-IO width 170 no-labels.

form w-fg-rctd.rct-date             format "99/99/99"  AT 1  
     v-time                                            AT 10 
     w-fg-rctd.i-no                 format "x(15)"     AT 16 
     w-fg-rctd.i-name               format "x(27)"     AT 32 
     v-tran-type                                       AT 63 
     w-fg-rctd.tag                  FORM "x(20)"       AT 65 
     w-fg-rctd.cases                format "->>,>>9"   AT 86 
     w-fg-rctd.qty-case             format "->>>,>>9"  AT 94 
     v-fg-qty                       format "->>>,>>9"  AT 103
     w-fg-rctd.loc-bin              FORM "x(6)"        AT 112
     w-fg-rctd.pur-uom              FORMAT "x(9)"      AT 119
     v-fg-value                                      
    with frame itemyA no-box down STREAM-IO width 170 no-labels.

/*form #4 Print 1 / partial for TOTAL VALUE*/
form w-fg-rctd.rct-date             format "99/99/99"    
     v-time                                              
     w-fg-rctd.i-no                 format "x(15)"       
     w-fg-rctd.i-name               format "x(14)"       
     w-fg-rctd.po-no                                     
     po-ord.vend-no                 FORMAT "x(5)"                                  
     v-tran-type                                         
     w-fg-rctd.tag                                       
     v-one                          format "->>,>>9"     
     w-fg-rctd.partial              format "->>>,>>9"    
     v-fg-qty                       format "->>,>>>,>>9" 
     w-fg-rctd.loc-bin              FORM "x(6)"          
     w-fg-rctd.pur-uom              FORMAT "x(9)"        
     v-fg-value 
    with frame itempy no-box down STREAM-IO width 170 no-labels.

form w-fg-rctd.rct-date             format "99/99/99"    AT 1  
     v-time                                              AT 10 
     w-fg-rctd.i-no                 format "x(15)"       AT 16 
     w-fg-rctd.i-name               format "x(27)"       AT 32 
     v-tran-type                                         AT 63 
     w-fg-rctd.tag                                       AT 65 
     v-one                          format "->>,>>9"     AT 86 
     w-fg-rctd.partial              format "->>>,>>9"    AT 94 
     v-fg-qty                       format "->>,>>>,>>9" AT 103
     w-fg-rctd.loc-bin              FORM "x(6)"          AT 112
     w-fg-rctd.pur-uom              FORMAT "x(9)"        AT 119
     v-fg-value                                       
    with frame itempyA no-box down STREAM-IO width 170 no-labels.


form v-disp-actnum label "G/L ACCOUNT NUMBER"
     v-dscr        label "DESCRIPTION"
     v-post-date   label "DATE"   
     v-disp-amt    label "AMOUNT" SKIP
    with down STREAM-IO width 130 frame gldetail.    

{ce/msfcalc.i}

SESSION:SET-WAIT-STATE ("general").

IF length(begin_job-no) < 6 THEN
   begin_job-no = FILL(" ",6 - LENGTH(trim(begin_job-no))) + TRIM(begin_job-no).
IF length(end_job-no) < 6 THEN
   end_job-no = FILL(" ",6 - LENGTH(trim(end_job-no))) + TRIM(end_job-no).

IF ip-run-what EQ "" THEN
  DISPLAY begin_job-no END_job-no WITH FRAME {&FRAME-NAME}.

ASSIGN
 str-tit2 = CURRENT-WINDOW:TITLE
 {sys/inc/ctrtext.i str-tit2 112}
 str-tit3 = "Period Date: " + string(v-post-date,"99/99/9999") + "             Posted by: " + USERID('nosweat') + "  As of " + string(TODAY,"99/99/9999")
 {sys/inc/ctrtext.i str-tit3 132}

 v-postlst   = (IF t-receipt THEN "R," ELSE "") +
               (IF t-setup THEN "I," ELSE "") +
               (IF t-ship THEN "S," ELSE "") +
               (IF t-trans THEN "T," ELSE "") +
               (IF t-adj THEN "A," ELSE "") +
               (IF t-ret THEN "E," ELSE "") +
               (IF tgIssue THEN "F," ELSE "")
 v-cost-sell = rd_print EQ "C"
 v-pr-tots2  = tb_totCstVal
 v-pr-tots   = tb_grndtotal.

IF LENGTH(v-postlst) GT 0 AND
   SUBSTR(v-postlst,LENGTH(v-postlst),1) EQ "," THEN
   SUBSTR(v-postlst,LENGTH(v-postlst),1) = "".

DO li = 1 TO 2:
  {sys/inc/print1.i}
  lv-list-name[li] = list-name.
  PAUSE 1 NO-MESSAGE.
END.

OUTPUT STREAM before TO VALUE(lv-list-name[1]) PAGE-SIZE VALUE(lines-per-page).
OUTPUT STREAM after  TO VALUE(lv-list-name[2]) PAGE-SIZE VALUE(lines-per-page).

if td-show-parm then run show-param.

EMPTY TEMP-TABLE tt-set.
/* If not running for all items, check these items for components that must */
/* be included                                                              */
IF NOT (begin_i-no EQ "" AND end_i-no BEGINS "zzzzzzzzzzz")
     THEN RUN createComponentList.

DO li-loop = 1 TO NUM-ENTRIES(v-postlst):
  FOR EACH fg-rctd
      WHERE fg-rctd.company   EQ gcompany
        AND fg-rctd.rita-code EQ ENTRY(li-loop,v-postlst)
        AND fg-rctd.r-no      GE begin_fg-r-no
        AND fg-rctd.r-no      LE end_fg-r-no
        AND ((fg-rctd.i-no      GE begin_i-no
             AND fg-rctd.i-no      LE end_i-no)
             OR CAN-FIND(FIRST tt-set WHERE tt-set.part-no EQ fg-rctd.i-no))
        AND fg-rctd.rct-date  GE ldt-from
        AND fg-rctd.rct-date  LE ldt-to
        AND fg-rctd.job-no    GE begin_job-no
        AND fg-rctd.job-no    LE end_job-no
        AND fg-rctd.loc-bin   NE ""
        AND fg-rctd.loc       GE begin_whs
        AND fg-rctd.loc       LE end_whs
        AND ((begin_userid    LE "" AND
              end_userid      GE "") OR
             (fg-rctd.updated-by GE begin_userid 
              AND fg-rctd.updated-by LE end_userid))
      USE-INDEX rita-code:

    RUN build-tables.
  END.
END.

RUN build-comp-tables.

if v-cost-sell then do:
  v-hdr = "        COST".

  IF tb_excel THEN 
  DO:
    OUTPUT STREAM excel TO VALUE(fi_file).

    IF rd-ItmPo EQ 1
      THEN ASSIGN excelheader = "Date,Time,Item,Description,".
      ELSE ASSIGN excelheader = "Date,Time,Item,Description,Po No,Vendor,".

    ASSIGN excelheader = excelheader + 
                         "T,Tag No,Units,Count,Total,Bin,".
    IF rd-UOMJob EQ 1 
      THEN ASSIGN excelheader = excelheader + "UOM,Total Cost".
      ELSE ASSIGN excelheader = excelheader + "Job #,Total Cost".

    PUT STREAM excel UNFORMATTED excelheader SKIP.

  END.

   IF rd-ItmPo EQ 1 THEN DO:
     {fg/rep/fg-post.i "itemxA" "v-fg-cost" "itempxA" "v-tot-cost"}
   END.
   ELSE DO:
     {fg/rep/fg-post.i "itemx" "v-fg-cost" "itempx" "v-tot-cost"}
   END.
end.
else do:
  v-hdr = "       VALUE".

  IF tb_excel THEN 
  DO:
    OUTPUT STREAM excel TO VALUE(fi_file).

    IF rd-ItmPo EQ 1
      THEN ASSIGN excelheader = "Date,Time,Item,Description,".
      ELSE ASSIGN excelheader = "Date,Time,Item,Description,Po No,Vendor,".

    ASSIGN excelheader = excelheader + 
                         "T,Tag No,Units,Count,Total,Bin,".

    IF rd-UOMJob EQ 1 
      THEN ASSIGN excelheader = excelheader + "UOM,Total Value".
      ELSE ASSIGN excelheader = excelheader + "Job #,Total Value".

    PUT STREAM excel UNFORMATTED excelheader SKIP.
  END.

  IF rd-ItmPo EQ 1 THEN DO:
   {fg/rep/fg-post.i "itemyA" "v-fg-value" "itempyA" "v-tot-value"}
  END.
  ELSE DO:
   {fg/rep/fg-post.i "itemy" "v-fg-value" "itempy" "v-tot-value"}
  END.
end.

if v-pr-tots then do:
  if v-cost-sell then DO:                   
    PUT STREAM before
        " " to 124 skip       
        "MSF->  FG: " + trim(string(v-msf[5],">>,>>9.9<<")) +
        "  Wst: " + trim(string(v-msf[6],">>,>>9.9<<"))    +
        "  Tot: " + trim(string(v-msf[5] + v-msf[6],">>,>>9.9<<"))
                    format "x(59)" at 15
        "GRAND TOTALS:" to 97
        v-grd-tot-qty to 110 v-grd-tot-cost to 141 skip. 

    PUT STREAM after
        " " to 124 skip       
        "MSF->  FG: " + trim(string(v-msf[5],">>,>>9.9<<")) +
        "  Wst: " + trim(string(v-msf[6],">>,>>9.9<<"))    +
        "  Tot: " + trim(string(v-msf[5] + v-msf[6],">>,>>9.9<<"))
                    format "x(59)" at 15 
        "GRAND TOTALS:" to 97
        v-grd-tot-qty to 110 v-grd-tot-cost to 141 skip.     
  END.
  ELSE DO:
    PUT STREAM before
        " " to 124 skip       
        "MSF->  FG: " + trim(string(v-msf[5],">>,>>9.9<<")) +
        "  Wst: " + trim(string(v-msf[6],">>,>>9.9<<"))    +
        "  Tot: " + trim(string(v-msf[5] + v-msf[6],">>,>>9.9<<"))
                    format "x(59)" at 15 
        "GRAND TOTALS:" to 100
        v-grd-tot-qty to 113 v-grd-tot-value to 144 skip.

    PUT STREAM after
        " " to 124 skip       
        "MSF->  FG: " + trim(string(v-msf[5],">>,>>9.9<<")) +
        "  Wst: " + trim(string(v-msf[6],">>,>>9.9<<"))    +
        "  Tot: " + trim(string(v-msf[5] + v-msf[6],">>,>>9.9<<"))
                    format "x(59)" at 15 
        "GRAND TOTALS:" to 97
        v-grd-tot-qty to 110 v-grd-tot-value to 141 skip.
  END.
end. /* if v-pr-tots */

HIDE FRAME r-top1.

if tb_glnum THEN DO:
  PAGE STREAM before.
  PAGE STREAM after.

  for each work-gl break by work-gl.actnum:

    find first account
        where account.company eq cocode
          and account.actnum  eq work-gl.actnum
        no-lock no-error.

    assign
     v-dscr        = if avail account then account.dscr
                     else "ACCOUNT NOT FOUND - " + work-gl.actnum
     v-disp-actnum = work-gl.actnum
     v-disp-amt    = work-gl.debits - work-gl.credits.

    display STREAM before
            v-disp-actnum v-dscr v-post-date v-disp-amt
          with frame gldetail.
    down STREAM before with frame gldetail.

    display STREAM after
            v-disp-actnum v-dscr v-post-date v-disp-amt
          with frame gldetail.
    down STREAM after with frame gldetail.
  end. /* each work-job */

  for each work-job break by work-job.actnum:

    find first account
        where account.company eq cocode
          and account.actnum  eq work-job.actnum
        no-lock no-error.

    assign
     v-dscr        = if avail account then account.dscr
                     else "ACCOUNT NOT FOUND - " + work-job.actnum
     v-disp-actnum = work-job.actnum.

    if work-job.fg then
      v-disp-amt = - work-job.amt.
    else
      v-disp-amt = work-job.amt.

    display STREAM before
            v-disp-actnum v-dscr v-post-date v-disp-amt
          with frame gldetail.
    down STREAM before with frame gldetail.

    display STREAM after
            v-disp-actnum v-dscr v-post-date v-disp-amt
          with frame gldetail.
    down STREAM after with frame gldetail.
  end. /* each work-job */
END.

OUTPUT STREAM before CLOSE.
OUTPUT STREAM after  CLOSE.

IF tb_excel THEN 
  DO:
      OUTPUT STREAM excel CLOSE.
      IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

/* Only save screen selections if the screen was enabled */
IF ip-run-what EQ "" THEN
  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-fgemail C-Win 
PROCEDURE send-fgemail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-fgemail-file AS cha .

  DEF VAR retcode AS INT NO-UNDO.
  DEF VAR ls-to-list AS cha NO-UNDO.
  DEF VAR lv-mailto AS cha NO-UNDO.
  DEF VAR lv-mailsubject AS cha NO-UNDO.
  DEF VAR lv-mailbody AS cha NO-UNDO.
  DEF VAR lv-mailattach AS cha NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

  /* gdm - 12170901 */
  DEF BUFFER bf-job-hdr FOR job-hdr.
  DEF BUFFER bf-oe-ordl FOR oe-ordl.
  DEF BUFFER bf-itemfg FOR itemfg.

  FIND FIRST users WHERE
       users.user_id EQ USERID("NOSWEAT")
       NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     v-dir = users.user_program[2] + "\".
  ELSE
     v-dir = "c:\tmp\".

   FOR EACH tt-email,
       FIRST cust NO-LOCK WHERE cust.company = g_company
                           AND cust.cust-no = tt-email.cust-no
                           AND cust.active = "E" BREAK BY tt-email.cust-no:
       IF FIRST-OF(tt-email.cust-no) THEN DO:
          v-fgemail-file = v-dir + trim(tt-email.cust-no) + ".txt".
          OUTPUT STREAM st-email TO VALUE(v-fgemail-file).
          PUT STREAM st-email 
            "      Qty      JOB#       FG Item#          Part #          PO #            Item Name                 " SKIP
            "============ ========== =============== =============== =============== ==============================" SKIP.
       END.

       RELEASE bf-oe-ordl.

       /* gdm - 12170901 */
       FIND FIRST bf-job-hdr WHERE
            bf-job-hdr.company EQ g_company AND
            bf-job-hdr.job-no EQ tt-email.job-no AND
            bf-job-hdr.job-no2 EQ tt-email.job-no2 AND
            bf-job-hdr.i-no EQ tt-email.i-no AND
            bf-job-hdr.ord-no NE 0
            NO-LOCK NO-ERROR.

       IF AVAIL bf-job-hdr THEN
          FIND FIRST bf-oe-ordl WHERE
               bf-oe-ordl.company EQ g_company AND
               bf-oe-ordl.ord-no EQ bf-job-hdr.ord-no
               NO-LOCK NO-ERROR.

       FIND FIRST bf-itemfg WHERE
            bf-itemfg.company EQ g_company AND
            bf-itemfg.i-no EQ tt-email.i-no
            NO-LOCK NO-ERROR.

       PUT STREAM st-email UNFORMATTED
           tt-email.qty FORM "->>>,>>>,>>9" " " 
           tt-email.job-no + "-" + string(tt-email.job-no2,"99") FORM "x(10)"
           " " tt-email.i-no FORM "X(15)"
           " " (IF AVAIL bf-oe-ordl THEN bf-oe-ordl.part-no ELSE IF AVAIL bf-itemfg THEN bf-itemfg.part-no ELSE "") FORM "x(15)"
           " " (IF AVAIL bf-oe-ordl THEN bf-oe-ordl.po-no ELSE IF AVAIL bf-job-hdr THEN bf-job-hdr.po-no ELSE "") FORM "x(15)" 
           " " (IF AVAIL bf-oe-ordl THEN bf-oe-ordl.i-name ELSE IF AVAIL bf-itemfg THEN bf-itemfg.i-name ELSE "") FORM "x(30)"
           SKIP.

       IF LAST-OF(tt-email.cust-no) THEN do:
           OUTPUT STREAM st-email CLOSE.           
           {custom/emailList.i &recKey=cust.rec_key &emailList=ls-to-list}
           IF ls-to-list NE '' THEN DO:
             ASSIGN lv-mailto = "To:" + ls-to-list
                    lv-mailsubject = "Finished Goods Receipts have been posted"
                    lv-mailbody = "Finished Goods Receipts have been posted"
                    lv-mailattach = v-fgemail-file.
             RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,lv-mailattach,1,OUTPUT retcode).
           END.
       END. /* last-of(tt-email.cust-no) */
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Send-FGemail-Purchased C-Win 
PROCEDURE Send-FGemail-Purchased :
/*------------------------------------------------------------------------------
  Purpose:     Send emails for FGEMAIL purchased item receipts.
  Parameters:  <none>
  Notes:       Purchase Order#, Customer Order#, FG Item# and quantity Received to Inventory.
------------------------------------------------------------------------------*/
 DEFINE VARIABLE cRecipientList   AS CHAR NO-UNDO INIT "".
 DEFINE VARIABLE cEmailSubject    AS CHAR NO-UNDO INIT "".
 DEFINE VARIABLE cEmailBody       AS CHAR NO-UNDO INIT "".
 DEFINE VARIABLE cEmailAttachment AS CHAR NO-UNDO INIT "".
 DEFINE VARIABLE iReturn          AS INT  NO-UNDO INIT ?.


  /* Process emails by recipient. */
  FOR EACH tt-fgemail BREAK BY tt-fgemail.Recipient:

      /* Build email message for all items for this recipient. */
      ASSIGN cEmailBody = cEmailBody + " " + CHR(13) +
                          "     PO: " + STRING(tt-fgemail.po-no) + CHR(13) +
                          "  Order: " + STRING(tt-fgemail.ord-no) + CHR(13) +
                          "   Item: " + STRING(tt-fgemail.i-no) + CHR(13) +
                          "Rec Qty: " + STRING(tt-fgemail.qty-rec) + CHR(13).

      /* When last record of this recipient, send the email. */
      IF LAST-OF(tt-fgemail.recipient) THEN DO:

          ASSIGN cRecipientList = "To:" + tt-fgemail.Recipient
                 cEmailSubject  = "Finished Goods Receipts have been posted".

          RUN mail (INPUT cRecipientList,
                    INPUT cEmailSubject,
                    INPUT cEmailBody,
                    INPUT cEmailAttachment,
                    INPUT 1,
                    OUTPUT iReturn).

          /* Clear out the message body for next recipient. */
          ASSIGN cEmailBody = "".
      END. /* LAST-OF */

  END. /* FOR EACH tt-fgemail  */

  /* Empty the temp-table. */
  EMPTY TEMP-TABLE tt-fgemail.


  RETURN.

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

  PUT STREAM before space(28)
      "< Selection Parameters >"
      skip(1).

  do i = 1 to num-entries(parm-fld-list,","):
    if entry(i,parm-fld-list) ne "" or
       entry(i,parm-lbl-list) ne "" then do:

      lv-label = fill(" ",34 - length(trim(entry(i,parm-lbl-list)))) +
                 trim(entry(i,parm-lbl-list)) + ":".

      PUT STREAM before lv-label format "x(35)" at 5
          space(1)
          trim(entry(i,parm-fld-list)) format "x(40)"
          skip.              
    end.
  end.

  PUT STREAM before fill("-",80) format "x(80)" skip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-report C-Win 
PROCEDURE show-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.


  list-name = lv-list-name[ip-int].

  IF ip-run-what EQ "" THEN
  DO WITH FRAME {&FRAME-NAME}:
      case rd-dest :
          when 1 then run output-to-printer.
          when 2 then run output-to-screen.
          when 3 then run output-to-file.
          when 4 then do:
              /*run output-to-fax.*/
              {custom/asifax.i &type= "Customer"
                               &begin_cust=v-trans-lbl
                               &END_cust= v-trans-lbl
                               &fax-subject="FRAME {&FRAME-NAME}:TITLE"
                               &fax-body="FRAME {&FRAME-NAME}:TITLE"
                               &fax-file=list-name }
          END. 
          when 5 then do:
              IF is-xprint-form THEN DO:
                 {custom/asimail.i &TYPE = "Customer"
                                &begin_cust= v-trans-lbl
                                &END_cust=v-trans-lbl
                                &mail-subject="FRAME {&FRAME-NAME}:TITLE"
                                &mail-body="FRAME {&FRAME-NAME}:TITLE"
                                &mail-file=list-name }
              END.
              ELSE DO:
                  {custom/asimailr.i &TYPE = "Customer"
                                     &begin_cust= v-trans-lbl
                                     &END_cust=v-trans-lbl
                                     &mail-subject="FRAME {&FRAME-NAME}:TITLE"
                                     &mail-body="FRAME {&FRAME-NAME}:TITLE"
                                     &mail-file=list-name }

              END.
          END. 
         WHEN 6 THEN RUN output-to-port.
      end case. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateFGItemRange C-Win 
PROCEDURE ValidateFGItemRange :
/*------------------------------------------------------------------------------
  Purpose:  Determine if the FG ITem # Range includes Set header and    
  Parameters:  oplOK -> Yes/No if ok to continue
  Notes:       This could be replaced with code that automatically includes
               the set component if the range only includes the set header
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplOK AS LOGICAL NO-UNDO.

DEFINE BUFFER bf-itemfg FOR itemfg.
DEFINE BUFFER bf-fg-set FOR fg-set.

    oplOK = YES.
    IF begin_i-no NE "" AND NOT end_i-no BEGINS "zzzz" AND t-receipt THEN DO:
        FOR EACH bf-itemfg 
            WHERE bf-itemfg.company EQ cocode
              AND bf-itemfg.i-no GE begin_i-no
              AND bf-itemfg.i-no LE end_i-no
              AND bf-itemfg.isaset
            NO-LOCK,
         EACH bf-fg-set 
            WHERE bf-fg-set.company EQ bf-itemfg.company
              AND bf-fg-set.set-no EQ bf-itemfg.i-no
            NO-LOCK:

            IF bf-fg-set.part-no GT end_i-no OR bf-fg-set.part-no LT begin_i-no THEN DO:
                oplOK = NO.
                LEAVE.
            END.
        END.
        IF NOT oplOK THEN
            MESSAGE "FG Item # Range includes a set header but does not include all of the set's components. "
            "Please edit range to include components."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-act-rel-qty C-Win 
FUNCTION get-act-rel-qty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-stat AS CHAR NO-UNDO.

  IF AVAIL oe-ordl THEN
     FOR EACH oe-rel WHERE 
         oe-rel.company EQ cocode AND
         oe-rel.ord-no  EQ oe-ordl.ord-no AND
         oe-rel.i-no    EQ oe-ordl.i-no AND
         oe-rel.line    EQ oe-ordl.line
         NO-LOCK:

         RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

         IF INDEX("A,B,P",lv-stat) > 0 THEN
            li = li + oe-rel.qty.
      END.

  RETURN li.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-prod C-Win 
FUNCTION get-prod RETURNS INTEGER
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR op-bal AS INT NO-UNDO.
  IF AVAIL oe-ordl THEN
  DO:
     IF oe-ordl.job-no NE "" THEN
        FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
           WHERE fg-rcpth.company   EQ cocode
             AND fg-rcpth.job-no    EQ oe-ordl.job-no
             AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
             AND fg-rcpth.i-no      EQ oe-ordl.i-no
             AND fg-rcpth.rita-code EQ "R"
           USE-INDEX job,
           EACH fg-rdtlh FIELDS(qty) NO-LOCK
           WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
             AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
             li = li + fg-rdtlh.qty.
        END.
     ELSE
     DO:
        FOR EACH job-hdr FIELDS(job-no job-no2) WHERE
            job-hdr.company EQ cocode AND
            job-hdr.ord-no EQ oe-ordl.ord-no AND
            job-hdr.i-no EQ oe-ordl.i-no
            USE-INDEX ord-no
            NO-LOCK,
            EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
           WHERE fg-rcpth.company   EQ cocode
             AND fg-rcpth.job-no    EQ job-hdr.job-no
             AND fg-rcpth.job-no2   EQ job-hdr.job-no2
             AND fg-rcpth.i-no      EQ oe-ordl.i-no
             AND fg-rcpth.rita-code EQ "R"
           USE-INDEX job,
           EACH fg-rdtlh FIELDS(qty) NO-LOCK
           WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
             AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
             li = li + fg-rdtlh.qty.
        END.
     END.

     IF oe-ordl.po-no-po NE 0 THEN
        FOR EACH fg-rcpth FIELDS(r-no rita-code) WHERE
            fg-rcpth.company   EQ cocode AND
            fg-rcpth.po-no     EQ STRING(oe-ordl.po-no-po) AND
            fg-rcpth.i-no      EQ oe-ordl.i-no AND
            fg-rcpth.rita-code EQ "R"
            NO-LOCK,
            EACH fg-rdtlh FIELDS(qty) WHERE
                 fg-rdtlh.r-no EQ fg-rcpth.r-no AND
                 fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                 NO-LOCK:
                 li = li + fg-rdtlh.qty.
        END.
  END.

  op-bal = li.
  RETURN li.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-tot-rcv-qty C-Win 
FUNCTION get-tot-rcv-qty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR v-tot-qty AS INT NO-UNDO.           
FOR EACH fg-rcpth
              WHERE fg-rcpth.company    EQ oe-ordl.company
                AND fg-rcpth.i-no       EQ oe-ordl.i-no
                AND fg-rcpth.job-no     EQ oe-ordl.job-no
                AND fg-rcpth.rita-code  EQ "R"
              USE-INDEX tran NO-LOCK,
               EACH fg-rdtlh
              WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
              NO-LOCK:
              v-tot-qty = v-tot-qty + fg-rdtlh.qty.
           END.
  RETURN v-tot-qty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-from-addons C-Win 
FUNCTION is-from-addons RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
DEF VAR lWasFound AS LOG NO-UNDO.
lWasFound = NO.
hProc = SESSION:FIRST-PROCEDURE.
DO WHILE VALID-HANDLE(hProc):
    IF index(hProc:FILE-NAME, "addon") GT 0 THEN DO:
          lWasFound = YES.
          LEAVE. /* found it. */
    END.

    hProc = hProc:NEXT-SIBLING.
END.

RETURN lWasFound.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
  (ipField AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE invalidChars AS CHARACTER NO-UNDO INITIAL "~"".
  DEFINE VARIABLE replaceChars AS CHARACTER NO-UNDO INITIAL "'',".
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE k AS INTEGER NO-UNDO.

  /*k = NUM-ENTRIES(invalidChars).
  DO i = 1 TO k: */

    ipField = REPLACE(ipField,ENTRY(1,invalidChars),ENTRY(1,replaceChars)).
  /*END.*/
  RETURN ipField.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

