&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-loadtg.w

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
DEFINE VARIABLE scanAgain AS LOGICAL NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom\windows.i}
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}
{custom/xprint.i}
assign
 cocode = gcompany
 locode = gloc.

DEF VAR lines-per-page AS INT NO-UNDO.

DEF var save_id AS RECID.

DEF var time_stamp AS ch.
ASSIGN time_stamp = string(TIME, "hh:mmam").

DEF var v-ford-no AS int FORMAT ">>>>>>" extent 2 no-undo.
def var v-orders as char format "x(78)" extent 10.
DEF var v-fitem AS char FORMAT "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"].
DEF var v-po-no-source AS char FORMAT "!" init "R".
def var v-stat as char format "!" init "O".

DEF var v-out AS char FORMAT "x(40)" NO-UNDO.
DEF var v-job AS char FORMAT "x(9)" NO-UNDO.
DEF var num-rec AS int init 0 NO-UNDO.
DEF var by-release AS log init NO NO-UNDO.
DEF VAR lv-rd_print  AS CHAR NO-UNDO.

/* 9812 CAH: */
DEF VAR v-loadtag       AS CHAR NO-UNDO INIT "ASI".  /* sys ctrl option */
DEF VAR v-mult          AS INT  NO-UNDO INIT 0.  /* sys ctrl option */
DEF VAR v-cas-lab       AS LOG  NO-UNDO.  /* sys ctrl option */
DEF VAR v-tags          AS DEC  NO-UNDO INIT 0.  /* sys ctrl option */
DEF VAR v-count         AS INT  NO-UNDO INIT 0.
DEF VAR v-fgrecpt       AS LOG  NO-UNDO.  /* sys ctrl option */
DEFINE VARIABLE glOverrideMult AS LOGICAL     NO-UNDO.

/* mdp var used for posting to finish goods */

DEF VAR lv-r-no LIKE rm-rctd.r-no NO-UNDO.

/* 9812 CAH: Variables for Intermec Support */
def var stx as char format 'x(01)' no-undo initial "~002".
def var etx as char format 'x(01)' no-undo initial "~003".
def var esc as char format 'x(01)' no-undo initial "~033".
def var etb as char format 'x(01)' no-undo initial "~027".
def var cr  as char format 'x(01)' no-undo initial "~015".
def var can as char format 'x(01)' no-undo initial "~030".
def var rs  as char format 'x(01)' no-undo initial "~036".
def var us  as char format 'x(01)' no-undo initial "~037".

def stream s-form.
def stream s-bar.

def var form_fid        as char no-undo initial "barcode.frm" FORMAT "X(40)".
def var form#           as int  no-undo format "9" initial 3.
def var char_units      as char no-undo.
def var copy_count      as int no-undo initial 2.
def var n               as int no-undo initial 0.
DEF VAR var-display-warning AS LOG NO-UNDO.
def var fg-uom-list  as char NO-UNDO.

/* Vars for create-text-file */
DEF VAR lv-text AS cha NO-UNDO.
DEF VAR v-dept-note AS cha FORM "x(80)" EXTENT 18 NO-UNDO.
DEF VAR lv-middlesex-job AS CHAR FORMAT "x(9)" NO-UNDO.
DEF VAR lv-middlesex-po AS CHAR FORMAT "x(9)" NO-UNDO.
DEF VAR lv-tag-no AS INT NO-UNDO.
DEF VAR lv-how-many-tags AS INT NO-UNDO.
DEF VAR lv-total-tags AS INT NO-UNDO.
DEF VAR gvlCreateWithMaxPrompted AS LOG NO-UNDO.
DEF VAR gvcSkippedJob AS CHAR NO-UNDO.
DEF VAR gvcSkippedItem AS CHAR NO-UNDO.

/* gdm - 10160905*/
DEF VAR v-fgdsc1 LIKE itemfg.part-dscr1 NO-UNDO.
DEF VAR v-fgdsc2 LIKE itemfg.part-dscr2 NO-UNDO.
DEF VAR v-fgdsc3 LIKE itemfg.part-dscr3 NO-UNDO.

DEFINE VARIABLE cPrevFromItem AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrevToItem AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.

DEF TEMP-TABLE tt-ordjobs
    FIELD job-no LIKE job.job-no
    FIELD job-no2 LIKE job.job-no2.

DEF TEMP-TABLE tt-comps
  FIELD comp AS CHAR 
  INDEX i1 comp.
def workfile w-file field w-key AS ROWID.
DEF TEMP-TABLE tt-tag FIELD tt-recid AS RECID.
DEF WORKFILE w-shipto LIKE shipto
                      FIELD stat AS CHAR
                      FIELD row-id AS ROWID.

DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER ref-lot-no FOR reftable.

DEFINE TEMP-TABLE ttblJob NO-UNDO
  FIELD company AS CHARACTER
  FIELD job-no AS CHARACTER
  FIELD job-no2 AS INTEGER
  FIELD ord-no AS INTEGER
    INDEX ttblJob IS PRIMARY UNIQUE
      company job-no job-no2 ord-no
    INDEX ord-no company ord-no.
DEF TEMP-TABLE tt-fgrctd-created NO-UNDO
  FIELD fg-rctd-rowid AS ROWID
  FIELD is-component AS LOG.

DEF VAR SSLoadTag-log AS LOGICAL NO-UNDO.
DEF VAR lvReturnChar AS CHAR NO-UNDO.
DEF VAR lvFound AS LOG NO-UNDO.
RUN sys/ref/nk1look.p (cocode, "SSLoadTag", "L", no, no, "", "", 
    Output lvReturnChar, output lvFound).
IF lvFound THEN
    SSLoadTag-log = LOGICAL(lvReturnChar).
{oerep/r-loadtg.i NEW}

{fg/fullset.i NEW}

ASSIGN  
  tmpstore = FILL("_",50).

{sys/form/r-top3.f}

DEF VAR lv-ok-ran AS LOG NO-UNDO.
{custom/formtext.i NEW}

DEF WORKFILE w-fg-rctd LIKE fg-rctd FIELD row-id   AS ROWID
                                    FIELD invoiced AS LOG INIT NO.

{fg/fg-post3.i NEW}
DEF VAR v-fgpostgl AS CHAR NO-UNDO.
{jc/jcgl-sh.i NEW}

DEFINE VARIABLE ordNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobNo2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE poNopo AS INT NO-UNDO.

/* gdm - 04090909 */
DEF VAR v-barflg AS LOG NO-UNDO.
DEF VAR v-auto-print AS LOG NO-UNDO.
DEF VAR UserlabelPath AS cha NO-UNDO.

/* gdm - 06100901 */
DEF VAR v-txtflg AS LOG NO-UNDO.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

DO TRANSACTION:
   {sys/inc/fgpofrt.i}
   {sys/inc/fgrecpt.i} /* gdm - 12010901*/
   {sys/inc/rfidtag.i}
   {sys/inc/fgsetrec.i}
END.
DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lFGSetAssembly AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cFGSetAssembly AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cResult AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lGetBin AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cBarCodeProgram AS CHARACTER NO-UNDO .
DEFINE VARIABLE i-bardir-int AS INTEGER NO-UNDO .
DEFINE VARIABLE i-xprint-int AS INTEGER NO-UNDO .
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.   

RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "LoadTagXprintImage",
                       INPUT "C",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT ls-image1,
                       OUTPUT lFound).

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

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
DEF VAR lLabelMatrixLock AS LOG NO-UNDO.
RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "lmLock",
                       INPUT "L",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT cResult,
                       OUTPUT lFound).
IF lFound THEN
  lLabelMatrixLock = LOGICAL(cResult).

/* rstark - zoho13731 */
DEF VAR lSSCC AS LOG NO-UNDO.
RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "LoadTagSSCC",
                       INPUT "L",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT cResult,
                       OUTPUT lFound).
lSSCC = LOGICAL(cResult).

/* gdm - 09210907 */
DEF VAR v-bardir AS LOG NO-UNDO.
DEF VAR v-bardir-chr AS CHAR NO-UNDO.

DEF VAR iForm AS INT NO-UNDO.
DEF VAR lForm AS LOG NO-UNDO.

DEF VAR lcRtnChar AS CHAR NO-UNDO.
DEF VAR llRecFound AS LOG NO-UNDO.

DEF BUFFER bf-oe-ord  FOR oe-ord.
DEF BUFFER bf-oe-ordl FOR oe-ordl.

/* gdm - 0930916 */
DEF BUFFER bf-po-ord  FOR po-ord.
DEF BUFFER bf-po-ordl FOR po-ordl.

DEF BUFFER bf-jobhdr FOR job-hdr.
DEFINE TEMP-TABLE tt-word-print LIKE w-ord 
    FIELD tag-no AS CHARACTER .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbPartSelect loadtagFunction tb_ret ~
tb_reprint-tag v-ord-list v-job-list begin_ord-no end_ord-no begin_job ~
begin_job2 end_job end_job2 begin_i-no end_i-no rd_order-sts rd_print ~
begin_date end_date rd_comps tb_dept-note tb_rel tb_over tb_16ths ~
tb_ship-id scr-auto-print scr-freeze-label scr-label-file begin_labels ~
begin_form btn-ok btn-cancel tb_xfer-lot tb_override-mult begin_ship-to ~
end_ship-to tb_close tb_print-view RECT-7 RECT-8 RECT-11 RECT-12 
&Scoped-Define DISPLAYED-OBJECTS tbPartSelect loadtagFunction tb_ret ~
tb_reprint-tag v-ord-list v-job-list begin_ord-no end_ord-no begin_job ~
begin_job2 end_job end_job2 begin_i-no end_i-no rd_order-sts rd_print ~
begin_date end_date rd_comps v-dept-list tb_dept-note tb_rel tb_over ~
tb_16ths tb_ship-id v-ship-id scr-auto-print scr-freeze-label ~
scr-label-file begin_labels begin_form begin_filename typeLabel statusLabel ~
lbl_po-no tb_xfer-lot tb_override-mult begin_ship-to end_ship-to tb_close ~
tb_print-view 

/* Custom List Definitions                                              */
/* jobFields,NonReprint,List-3,List-4,List-5,F1                         */
&Scoped-define jobFields tb_ret v-job-list begin_job begin_job2 end_job ~
end_job2 tb_rel tb_over 
&Scoped-define NonReprint loadtagFunction tb_ret v-ord-list v-job-list ~
begin_ord-no end_ord-no begin_job begin_job2 end_job end_job2 begin_i-no ~
end_i-no rd_order-sts rd_print begin_date end_date rd_comps tb_rel tb_over ~
tb_16ths tb_ship-id v-ship-id begin_filename tb_xfer-lot tb_override-mult ~
begin_ship-to end_ship-to 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD win_normalizePath C-Win 
FUNCTION win_normalizePath RETURNS CHARACTER
  ( pcPath AS CHAR )  FORWARD.

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

DEFINE VARIABLE v-job-list AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 42 BY 3.1 NO-UNDO.

DEFINE VARIABLE v-ord-list AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 42 BY 3.1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "From" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_filename AS CHARACTER FORMAT "X(256)":U INITIAL "ccc" 
     LABEL "Text File Path" 
     VIEW-AS FILL-IN 
     SIZE 68 BY 1 NO-UNDO.

DEFINE VARIABLE begin_form AS INTEGER FORMAT ">>>":U INITIAL 1 
     LABEL "Printer Form#" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "From Item#" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job AS CHARACTER FORMAT "X(6)":U 
     LABEL "From Job#" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_labels AS INTEGER FORMAT ">>>>":U INITIAL 2 
     LABEL "# of Labels per Skid" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "From Order#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ship-to AS CHARACTER FORMAT "X(8)":U 
     LABEL "Ship To From" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To Item#" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE end_job AS CHARACTER FORMAT "X(6)":U 
     LABEL "To Job#" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE end_job2 AS INTEGER FORMAT "99":U INITIAL 99 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "To Order#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_ship-to AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cas-lab AS CHARACTER FORMAT "X(30)":U 
     LABEL "Scan Case Label" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_po-no AS CHARACTER FORMAT "X(256)":U INITIAL "Print PO from:" 
      VIEW-AS TEXT 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE scr-label-file AS CHARACTER FORMAT "X(256)":U 
     LABEL "Print Format" 
     VIEW-AS FILL-IN 
     SIZE 68 BY 1 NO-UNDO.

DEFINE VARIABLE statusLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Order/Job Status:" 
      VIEW-AS TEXT 
     SIZE 18 BY .76 NO-UNDO.

DEFINE VARIABLE typeLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Enter Orders separated by comma" 
      VIEW-AS TEXT 
     SIZE 42 BY .62 NO-UNDO.

DEFINE VARIABLE v-dept-list AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25.4 BY 1 NO-UNDO.

DEFINE VARIABLE v-ship-id AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE loadtagFunction AS CHARACTER INITIAL "Order" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Job/Order Receipt", "Order",
"Purchased Item Receipt", "PO"
     SIZE 29 BY 1.76 NO-UNDO.

DEFINE VARIABLE rd_comps AS CHARACTER INITIAL "B" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Assembled", "A",
"Unassembled", "U",
"Both", "B"
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE rd_order-sts AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "O",
"Closed", "C",
"All", "A"
     SIZE 47 BY .71 NO-UNDO.

DEFINE VARIABLE rd_print AS CHARACTER INITIAL "H" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Header", "H",
"Line", "L",
"Job#", "J",
"Release", "R"
     SIZE 38 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 6.43.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 4.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 3.33.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 8.33
     BGCOLOR 0 FGCOLOR 0 .

DEFINE VARIABLE scr-auto-print AS LOGICAL INITIAL no 
     LABEL "Auto Print Label?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.8 BY 1 NO-UNDO.

DEFINE VARIABLE scr-freeze-label AS LOGICAL INITIAL no 
     LABEL "Freeze Label File Choice?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tbPartSelect AS LOGICAL INITIAL no 
     LABEL "Select Components" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE tb_16ths AS LOGICAL INITIAL no 
     LABEL "Show LWD in 16ths?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_close AS LOGICAL INITIAL no 
     LABEL "Close?" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE tb_dept-note AS LOGICAL INITIAL no 
     LABEL "Print Department Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE tb_over AS LOGICAL INITIAL no 
     LABEL "Include Overrun?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE tb_override-mult AS LOGICAL INITIAL no 
     LABEL "Ignore Customer Labels per Skid" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE tb_print-view AS LOGICAL INITIAL no 
     LABEL "Preview?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_rel AS LOGICAL INITIAL no 
     LABEL "Print Posted Release in BOL File?" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE tb_reprint-tag AS LOGICAL INITIAL no 
     LABEL "&Reprint Tag?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE tb_ret AS LOGICAL INITIAL no 
     LABEL "Returns?" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tb_ship-id AS LOGICAL INITIAL no 
     LABEL "Print Ship ID?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tb_xfer-lot AS LOGICAL INITIAL no 
     LABEL "Transfer Release Lot#" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tbPartSelect AT ROW 15.86 COL 73.4 WIDGET-ID 32
     loadtagFunction AT ROW 2.19 COL 36 RIGHT-ALIGNED NO-LABEL
     tb_ret AT ROW 2.19 COL 41
     tb_reprint-tag AT ROW 2.19 COL 59
     fi_cas-lab AT ROW 3.14 COL 57 COLON-ALIGNED
     v-ord-list AT ROW 5.95 COL 8 NO-LABEL
     v-job-list AT ROW 5.95 COL 56 NO-LABEL
     begin_ord-no AT ROW 9.14 COL 20 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord-no AT ROW 9.14 COL 64 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_job AT ROW 10.1 COL 20 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job2 AT ROW 10.1 COL 35 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job AT ROW 10.1 COL 64 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job2 AT ROW 10.1 COL 79 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_i-no AT ROW 11.05 COL 20 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 11.05 COL 64 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     rd_order-sts AT ROW 12.24 COL 22.6 NO-LABEL
     rd_print AT ROW 13.86 COL 18 NO-LABEL
     begin_date AT ROW 13.81 COL 61.2 COLON-ALIGNED HELP
          "Enter Beginning Release Date"
     end_date AT ROW 13.81 COL 82.6 COLON-ALIGNED HELP
          "Enter Ending Release Date"
     rd_comps AT ROW 15.76 COL 31 NO-LABEL
     v-dept-list AT ROW 17.43 COL 30.6 COLON-ALIGNED NO-LABEL
     tb_dept-note AT ROW 17.52 COL 4
     tb_rel AT ROW 18.48 COL 4
     tb_over AT ROW 18.48 COL 45
     tb_16ths AT ROW 18.48 COL 73
     tb_ship-id AT ROW 17.38 COL 59.4 WIDGET-ID 24
     v-ship-id AT ROW 17.38 COL 75.8 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     scr-auto-print AT ROW 20.38 COL 18.4 WIDGET-ID 2
     scr-freeze-label AT ROW 20.38 COL 39.6 WIDGET-ID 4
     scr-label-file AT ROW 21.33 COL 24 COLON-ALIGNED WIDGET-ID 6
     begin_labels AT ROW 22.33 COL 24 COLON-ALIGNED
     begin_form AT ROW 22.29 COL 84 COLON-ALIGNED
     begin_filename AT ROW 23.38 COL 24 COLON-ALIGNED
     btn-ok AT ROW 24.76 COL 25
     btn-cancel AT ROW 24.76 COL 66
     typeLabel AT ROW 5.29 COL 6 COLON-ALIGNED NO-LABEL
     statusLabel AT ROW 12.14 COL 4.4 NO-LABEL
     lbl_po-no AT ROW 13.86 COL 4 HELP
          "Print Customer's PO Number from Header, Line item or Release" NO-LABEL
     tb_xfer-lot AT ROW 16.57 COL 4 WIDGET-ID 28
     tb_override-mult AT ROW 22.33 COL 35 WIDGET-ID 34
     begin_ship-to AT ROW 14.71 COL 61.2 COLON-ALIGNED HELP
          "Enter Beginning Release Shipto" WIDGET-ID 36
     end_ship-to AT ROW 14.71 COL 82.6 COLON-ALIGNED HELP
          "Enter Ending Release shipto" WIDGET-ID 38
     tb_close AT ROW 20.38 COL 69 WIDGET-ID 40
     tb_print-view AT ROW 20.38 COL 81.4 WIDGET-ID 42
     "Output Options:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 19.76 COL 4 WIDGET-ID 22
          FONT 6
     "Print Set Components for:" VIEW-AS TEXT
          SIZE 26 BY 1 AT ROW 15.71 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 3
          BGCOLOR 2 
     " Enter Jobs separated by comma" VIEW-AS TEXT
          SIZE 42 BY .62 AT ROW 5.29 COL 56
     "Data Parameters:" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 4.57 COL 4 WIDGET-ID 10
          FONT 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.2 BY 30.33.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
     "Print Options:" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 13.14 COL 4 WIDGET-ID 14
          FONT 6
     RECT-7 AT ROW 1.24 COL 2
     RECT-8 AT ROW 4.81 COL 2 WIDGET-ID 8
     RECT-11 AT ROW 13.38 COL 2 WIDGET-ID 18
     RECT-12 AT ROW 20.14 COL 2 WIDGET-ID 20
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.2 BY 30.33.


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
         TITLE              = "Loadtag Creation"
         HEIGHT             = 25.1
         WIDTH              = 103
         MAX-HEIGHT         = 53.71
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 53.71
         VIRTUAL-WIDTH      = 384
         MAX-BUTTON         = no
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN begin_date IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_filename IN FRAME FRAME-A
   NO-ENABLE 2                                                          */
ASSIGN 
       begin_filename:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_form:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_i-no IN FRAME FRAME-A
   2                                                                    */
/* SETTINGS FOR FILL-IN begin_job IN FRAME FRAME-A
   1 2                                                                  */
/* SETTINGS FOR FILL-IN begin_job2 IN FRAME FRAME-A
   1 2                                                                  */
ASSIGN 
       begin_labels:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_ord-no IN FRAME FRAME-A
   2                                                                    */
/* SETTINGS FOR FILL-IN begin_ship-to IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       begin_ship-to:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

/* SETTINGS FOR FILL-IN end_date IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_i-no IN FRAME FRAME-A
   2                                                                    */
/* SETTINGS FOR FILL-IN end_job IN FRAME FRAME-A
   1 2                                                                  */
/* SETTINGS FOR FILL-IN end_job2 IN FRAME FRAME-A
   1 2                                                                  */
/* SETTINGS FOR FILL-IN end_ord-no IN FRAME FRAME-A
   2                                                                    */
/* SETTINGS FOR FILL-IN end_ship-to IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       end_ship-to:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fi_cas-lab IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_cas-lab:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lbl_po-no IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       lbl_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_print".

/* SETTINGS FOR RADIO-SET loadtagFunction IN FRAME FRAME-A
   ALIGN-R 2                                                            */
/* SETTINGS FOR RADIO-SET rd_comps IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       rd_comps:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR RADIO-SET rd_order-sts IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       rd_order-sts:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR RADIO-SET rd_print IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       rd_print:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       scr-auto-print:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       scr-freeze-label:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       scr-label-file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN statusLabel IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_16ths IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       tb_16ths:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_close:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_dept-note:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_over IN FRAME FRAME-A
   1 2                                                                  */
ASSIGN 
       tb_over:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_override-mult IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       tb_override-mult:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print-view:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_rel IN FRAME FRAME-A
   1 2                                                                  */
ASSIGN 
       tb_rel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_ret IN FRAME FRAME-A
   1 2                                                                  */
ASSIGN 
       tb_ret:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_ship-id IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       tb_ship-id:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_xfer-lot IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       tb_xfer-lot:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN typeLabel IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-dept-list IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       v-dept-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR EDITOR v-job-list IN FRAME FRAME-A
   1 2                                                                  */
ASSIGN 
       v-job-list:RETURN-INSERTED IN FRAME FRAME-A  = TRUE.

/* SETTINGS FOR EDITOR v-ord-list IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       v-ord-list:RETURN-INSERTED IN FRAME FRAME-A  = TRUE.

/* SETTINGS FOR FILL-IN v-ship-id IN FRAME FRAME-A
   NO-ENABLE 2                                                          */
ASSIGN 
       v-ship-id:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Loadtag Creation */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Loadtag Creation */
DO:
   IF INDEX(program-name(4),"asiLogin") <> 0 THEN
       RUN system/userLogOut.p.
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON HELP OF FRAME FRAME-A
DO:
  DEF VAR lv-handle AS HANDLE NO-UNDO.
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR rec-val AS RECID NO-UNDO.
  DEFINE VARIABLE cCustNo AS CHARACTER NO-UNDO .
  DEF VAR lv-po-no AS CHAR NO-UNDO.

  ASSIGN
   begin_ord-no end_ord-no
   begin_job begin_job2
   end_job end_job2
   begin_i-no     
   end_i-no     

   .

  IF begin_job NE "" AND LENGTH(begin_job) LT 6 THEN
    begin_job = FILL(" ",6 - LENGTH(TRIM(begin_job))) + TRIM(begin_job).
  IF end_job NE "" AND LENGTH(end_job) LT 6 THEN
    end_job = FILL(" ",6 - LENGTH(TRIM(end_job))) + TRIM(end_job).

  lv-handle = FOCUS:HANDLE.

  CASE FOCUS:NAME:
    WHEN "begin_ord-no" THEN DO:
       /* gdm - 06250905 */
      IF begin_ord-no:LABEL = "From Order#" THEN DO:
        RUN windows/l-ordl.w (g_company, 
                              begin_ord-no:screen-value, 
                              output char-val, output rec-val). 
        FIND oe-ordl WHERE RECID(oe-ordl) EQ rec-val NO-LOCK NO-ERROR.
        IF AVAIL oe-ordl 
          THEN
           ASSIGN begin_ord-no:SCREEN-VALUE = STRING(oe-ordl.ord-no)
                  begin_job:SCREEN-VALUE    = oe-ordl.job-no
                  begin_job2:SCREEN-VALUE   = STRING(oe-ordl.job-no2)
                  begin_i-no:SCREEN-VALUE   = oe-ordl.i-no.
      END.
      ELSE DO:
        ASSIGN lv-po-no = begin_ord-no:SCREEN-VALUE.

        RUN windows/l-poopen.w (cocode, lv-po-no, OUTPUT char-val).
        IF char-val NE "" THEN lv-po-no = ENTRY(1,char-val).

         ASSIGN begin_ord-no:SCREEN-VALUE = lv-po-no.

         APPLY "entry" TO begin_ord-no.
      END.
      /* gdm - 06250905 end */
    END.
    WHEN "end_ord-no" THEN DO:
      /* gdm - 06250905 */
      IF end_ord-no:LABEL = "To Order#" THEN DO:
              RUN windows/l-ordl.w (g_company, 
                              begin_ord-no:screen-value, 
                              output char-val, output rec-val). 
        FIND oe-ordl WHERE RECID(oe-ordl) EQ rec-val NO-LOCK NO-ERROR.
        IF AVAIL oe-ordl 
         THEN ASSIGN end_ord-no:SCREEN-VALUE = STRING(oe-ordl.ord-no)
                     end_job:SCREEN-VALUE    = oe-ordl.job-no
                     end_job2:SCREEN-VALUE   = STRING(oe-ordl.job-no2)
                     end_i-no:SCREEN-VALUE   = oe-ordl.i-no.

      END.
      ELSE DO:
       ASSIGN lv-po-no = end_ord-no:SCREEN-VALUE.

        RUN windows/l-poopen.w (cocode, lv-po-no, OUTPUT char-val).
        IF char-val NE "" THEN lv-po-no = ENTRY(1,char-val).

         ASSIGN end_ord-no:SCREEN-VALUE = lv-po-no.

         APPLY "entry" TO end_ord-no.

      END.
      /* gdm - 06250905 end */
    END.
    WHEN "begin_i-no" THEN DO:
      RUN windows/l-itemf3.w (g_company,begin_ord-no,begin_job,begin_job2,begin_i-no, OUTPUT char-val, OUTPUT rec-val).
      IF char-val <> "" THEN begin_i-no:SCREEN-VALUE = ENTRY(1,char-val).
    END.
    WHEN "end_i-no" THEN DO:
      RUN windows/l-itemf3.w (g_company,end_ord-no,end_job,end_job2,end_i-no, OUTPUT char-val, OUTPUT rec-val).
      IF char-val <> "" THEN end_i-no:SCREEN-VALUE = ENTRY(1,char-val).
    END.
    WHEN "begin_ship-to" THEN DO:
      FIND FIRST oe-ord NO-LOCK
          WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ INT(begin_ord-no:SCREEN-VALUE)
          NO-ERROR.
        IF AVAIL oe-ord THEN
          cCustNo = oe-ord.cust-no.

      RUN windows/l-shipto.w (g_company,g_loc,cCustNo,begin_ship-to:SCREEN-VALUE,OUTPUT char-val).
      IF char-val <> "" THEN begin_ship-to:SCREEN-VALUE = ENTRY(1,char-val).
    END.
    WHEN "end_ship-to" THEN DO:
       FIND FIRST oe-ord NO-LOCK
          WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ INT(end_ord-no:SCREEN-VALUE)
          NO-ERROR.
        IF AVAIL oe-ord THEN
          cCustNo = oe-ord.cust-no.

      RUN windows/l-shipto.w (g_company,g_loc,cCustNo,end_ship-to:SCREEN-VALUE,OUTPUT char-val).
      IF char-val <> "" THEN end_ship-to:SCREEN-VALUE = ENTRY(1,char-val).
    END.
    OTHERWISE do:
      lv-handle = FOCUS:HANDLE.
      RUN applhelp.p.

      IF g_lookup-var NE "" THEN lv-handle:SCREEN-VALUE = g_lookup-var.
    END.  /* otherwise */
  END CASE.

  APPLY "entry" TO lv-handle.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_filename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_filename C-Win
ON HELP OF begin_filename IN FRAME FRAME-A /* Text File Path */
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.

   system-dialog get-dir ls-filename 
                 title "Select Path to save"
                 initial-dir begin_filename
                 UPDATE ll-ok.

    IF ll-ok THEN self:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_filename C-Win
ON LEAVE OF begin_filename IN FRAME FRAME-A /* Text File Path */
DO:
  assign begin_filename.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_form
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_form C-Win
ON LEAVE OF begin_form IN FRAME FRAME-A /* Printer Form# */
DO:
  assign begin_form.

  begin_filename = "barcode" + string(begin_form) + ".frm".

  display begin_filename WITH FRAME FRAME-A IN WINDOW C-Win.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_labels
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_labels C-Win
ON LEAVE OF begin_labels IN FRAME FRAME-A /* # of Labels per Skid */
DO:
  ASSIGN begin_labels
      v-mult = begin_labels.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   IF INDEX(program-name(4),"asiLogin") <> 0 THEN
       RUN system/userLogOut.p.
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:  
  ASSIGN {&displayed-objects}.

   ASSIGN
      cBarCodeProgram = IF scr-label-file MATCHES "*.xpr*" THEN "xprint" 
                        ELSE IF scr-label-file MATCHES "*.lwl" THEN "loftware" 
                        ELSE "".
   FOR EACH tt-word-print:
       DELETE tt-word-print .
   END.

  ASSIGN 
      v-auto-print = scr-auto-print
      glOverrideMult = tb_override-mult.
  IF v-mult LE 0 THEN v-mult = 1.

  IF tb_reprint-tag AND fi_cas-lab:SCREEN-VALUE = "" THEN DO:
     MESSAGE "Enter tag# to reprint loadtag." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO fi_cas-lab.
     RETURN NO-APPLY.
  END.
  IF tb_ship-id AND v-ship-id:SCREEN-VALUE = "" THEN DO:
     MESSAGE "Ship ID field cannot be blank." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO v-ship-id.
     RETURN NO-APPLY.
  END.
  ELSE IF tb_ship-id THEN

  IF scr-auto-print AND scr-label-file = "" THEN
  DO:
     MESSAGE "Label Matrix Label File cannot be blank."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY "ENTRY":U TO scr-label-file IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.


  IF NOT lv-ok-ran THEN RUN ok-button.
  lv-ok-ran = NO.
  IF fi_cas-lab:SCREEN-VALUE <> ""  THEN DO:
     APPLY "entry" TO fi_cas-lab.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* To Item# */
DO:
  ASSIGN begin_i-no end_i-no.

  IF v-auto-print AND LOGICAL(scr-freeze-label:SCREEN-VALUE) EQ NO THEN
  DO:

    DEF VAR v-cust-no AS CHAR NO-UNDO.

    ASSIGN
       begin_job
       end_job
       begin_job2
       end_job2
       begin_ord-no END_ord-no.

    FIND FIRST itemfg 
       WHERE itemfg.company EQ cocode
         AND itemfg.i-no EQ END_i-no NO-LOCK NO-ERROR.
    IF itemfg.alloc EQ YES THEN
      rd_comps:SCREEN-VALUE = "U".
    IF begin_job2 = 0 AND END_job2 = 0 THEN END_job2 = 99.
    FIND FIRST job-hdr NO-LOCK
      WHERE job-hdr.company EQ cocode
        AND job-hdr.job-no  GE begin_job
        AND job-hdr.job-no  LE end_job
        AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) + 
             TRIM(job-hdr.job-no) + STRING(job-hdr.job-no2,"99")  
                            GE (begin_job + STRING(begin_job2,"99"))
        AND FILL(" ",6 - length(trim(job-hdr.job-no))) +
             TRIM(job-hdr.job-no) + STRING(job-hdr.job-no2,"99")  
                            LE (end_job + STRING(end_job2,"99"))
        AND job-hdr.job-no2    EQ INT(begin_job2:SCREEN-VALUE) NO-ERROR.

    IF AVAIL job-hdr THEN
       v-cust-no = job-hdr.cust-no.
    ELSE DO:
       IF loadtagFunction:SCREEN-VALUE EQ "order" THEN
       DO: 
           FIND FIRST oe-ord WHERE 
            oe-ord.company EQ cocode AND
            oe-ord.ord-no  EQ INT(begin_ord-no:SCREEN-VALUE)
            NO-LOCK NO-ERROR.

        IF AVAIL oe-ord THEN
          v-cust-no = oe-ord.cust-no.
        END.     
        ELSE DO:
            IF AVAIL itemfg AND end_ord-no:SCREEN-VALUE NE "" THEN 
                FIND FIRST po-ordl 
                    WHERE po-ordl.company EQ itemfg.company 
                      AND po-ordl.po-no EQ int(END_ord-no:SCREEN-VALUE)
                      AND po-ordl.item-type EQ NO 
                      AND po-ordl.i-no EQ itemfg.i-no
                NO-LOCK NO-ERROR.
            IF AVAIL po-ordl AND po-ordl.ord-no NE 0 THEN
                FIND FIRST oe-ord 
                    WHERE oe-ord.company EQ po-ordl.company
                      AND oe-ord.ord-no  EQ po-ordl.ord-no
                NO-LOCK NO-ERROR.
            IF AVAIL oe-ord THEN
                v-cust-no = oe-ord.cust-no.
        END.
    END.
     IF v-cust-no NE "" THEN
        FIND FIRST reftable WHERE
             reftable.reftable EQ "cp-lab-p" AND
             reftable.company  EQ cocode AND
             reftable.loc      GE begin_i-no:SCREEN-VALUE AND
             reftable.loc      LE end_i-no:SCREEN-VALUE AND
             reftable.CODE     EQ v-cust-no
             NO-LOCK NO-ERROR.

     IF AVAIL reftable AND reftable.dscr NE "" THEN
        scr-label-file:SCREEN-VALUE = (IF reftable.dscr <> "" THEN reftable.dscr ELSE v-bardir-chr).
     ELSE
        IF INT(begin_ord-no:SCREEN-VALUE) NE 0 AND
           INT(end_ord-no:SCREEN-VALUE) NE 0 THEN
        DO:
           FIND FIRST oe-rel WHERE
                oe-rel.company EQ cocode AND
                oe-rel.i-no    GE begin_i-no:SCREEN-VALUE AND
                oe-rel.i-no    LE end_i-no:SCREEN-VALUE AND
                oe-rel.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) AND
                oe-rel.ord-no  LE INT(end_ord-no:SCREEN-VALUE)
                NO-LOCK NO-ERROR.

           IF AVAIL oe-rel THEN 
              FIND FIRST shipto NO-LOCK 
               WHERE shipto.company EQ cocode 
                 AND shipto.cust-no EQ oe-rel.cust-no 
                 AND shipto.ship-id EQ oe-rel.ship-id 
               USE-INDEX ship-id NO-ERROR.
           ELSE
              FIND FIRST shipto NO-LOCK
               WHERE shipto.company EQ cocode 
                 AND shipto.cust-no EQ v-cust-no 
                 AND shipto.ship-id EQ v-cust-no
                  USE-INDEX ship-id NO-ERROR.

              IF AVAIL shipto THEN DO:
                 IF AVAIL oe-rel THEN
                    v-cust-no = oe-rel.cust-no.

                 FIND FIRST sys-ctrl-shipto NO-LOCK
                   WHERE sys-ctrl-shipto.company      EQ cocode 
                     AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                     AND sys-ctrl-shipto.cust-vend    EQ YES 
                     AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                     AND sys-ctrl-shipto.ship-id      EQ shipto.ship-id 
                     AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                        AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                        AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                       scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl-shipto NO-LOCK 
                            WHERE sys-ctrl-shipto.company      EQ cocode 
                              AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                              AND sys-ctrl-shipto.cust-vend-no EQ ""
                              AND sys-ctrl-shipto.cust-vend    EQ YES 
                            NO-ERROR.
                       IF AVAIL sys-ctrl-shipto AND 
                          TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                          scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                       ELSE DO:
                          FIND FIRST sys-ctrl WHERE
                               sys-ctrl.company EQ cocode AND
                               sys-ctrl.name    EQ "BARDIR" 
                               NO-LOCK NO-ERROR.
                          IF AVAIL sys-ctrl THEN
                             scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                          ELSE
                             scr-label-file:SCREEN-VALUE = "".
                       END.
                    END.
                 END.
              END.
              ELSE
              DO:
                 FIND FIRST sys-ctrl-shipto NO-LOCK 
                   WHERE sys-ctrl-shipto.company      EQ cocode 
                     AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                     AND sys-ctrl-shipto.cust-vend    EQ YES 
                     AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                     AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                        AND sys-ctrl-shipto.cust-vend-no EQ ""
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                      NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                       scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl WHERE
                            sys-ctrl.company EQ cocode AND
                            sys-ctrl.name    EQ "BARDIR" 
                            NO-LOCK NO-ERROR.
                       IF AVAIL sys-ctrl THEN
                          scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                       ELSE
                          scr-label-file:SCREEN-VALUE = "".
                    END.
                 END.
              END.
        END.
        ELSE
        IF INT(begin_ord-no:SCREEN-VALUE) EQ 0 AND
           INT(end_ord-no:SCREEN-VALUE) EQ 0 THEN
           DO:
              FIND FIRST shipto WHERE
                   shipto.company EQ cocode AND
                   shipto.cust-no EQ v-cust-no AND
                   shipto.ship-id EQ v-cust-no
                   NO-LOCK NO-ERROR.

              IF AVAIL shipto THEN DO:

                 FIND FIRST sys-ctrl-shipto WHERE
                      sys-ctrl-shipto.company      EQ cocode AND
                      sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                      sys-ctrl-shipto.cust-vend    EQ YES AND
                      sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                      sys-ctrl-shipto.ship-id      EQ shipto.ship-id AND
                      sys-ctrl-shipto.char-fld     NE ''
                      NO-LOCK NO-ERROR.

                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto WHERE
                         sys-ctrl-shipto.company      EQ cocode AND
                         sys-ctrl-shipto.NAME         EQ "BARDIR" AND 
                         sys-ctrl-shipto.cust-vend    EQ YES AND
                         sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                         sys-ctrl-shipto.char-fld     NE ''
                         NO-LOCK NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                       scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl-shipto NO-LOCK 
                            WHERE sys-ctrl-shipto.company      EQ cocode 
                              AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                              AND sys-ctrl-shipto.cust-vend-no EQ ""
                              AND sys-ctrl-shipto.cust-vend    EQ YES 
                            NO-ERROR.
                       IF AVAIL sys-ctrl-shipto AND 
                          TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                          scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                       ELSE DO:
                          FIND FIRST sys-ctrl WHERE
                               sys-ctrl.company EQ cocode AND
                               sys-ctrl.name    EQ "BARDIR" 
                               NO-LOCK NO-ERROR.
                          IF AVAIL sys-ctrl THEN
                             scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                          ELSE
                             scr-label-file:SCREEN-VALUE = "".
                       END.
                    END.
                 END.
              END.
              ELSE
              DO:
                 FIND FIRST sys-ctrl-shipto NO-LOCK 
                   WHERE sys-ctrl-shipto.company      EQ cocode 
                     AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                     AND sys-ctrl-shipto.cust-vend    EQ YES 
                     AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                     AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                 ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                      WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                        AND sys-ctrl-shipto.cust-vend-no EQ ""
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                      NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                       TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                       scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                    ELSE DO:
                       FIND FIRST sys-ctrl WHERE
                            sys-ctrl.company EQ cocode AND
                            sys-ctrl.name    EQ "BARDIR" 
                            NO-LOCK NO-ERROR.
                       IF AVAIL sys-ctrl THEN
                          scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                       ELSE
                          scr-label-file:SCREEN-VALUE = "".
                    END.
                 END.
              END.


           END. /*begin_ord-no and end_ord-no eq 0*/
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job C-Win
ON LEAVE OF end_job IN FRAME FRAME-A /* To Job# */
DO:
   RUN leave-job-label.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job2 C-Win
ON LEAVE OF end_job2 IN FRAME FRAME-A /* - */
DO:  
   RUN leave-job-label.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* To Order# */
DO:
  DEF VAR v-lcnt AS INT NO-UNDO.
  DEF VAR v-cust-no AS CHAR NO-UNDO.

  IF v-auto-print AND LOGICAL(scr-freeze-label:SCREEN-VALUE) EQ NO THEN
  DO:

    IF INT(begin_ord-no:SCREEN-VALUE) NE 0 AND 
       INT(end_ord-no:SCREEN-VALUE) NE 0 THEN
       DO:
          v-lcnt = 0.
          FOR EACH oe-rel NO-LOCK 
            WHERE oe-rel.company EQ cocode 
              AND oe-rel.ord-no  GE INT(begin_ord-no:SCREEN-VALUE)
              AND oe-rel.ord-no  LE INT(end_ord-no:SCREEN-VALUE):

              v-lcnt = v-lcnt + 1.
              IF v-lcnt GT 1 THEN LEAVE.

          END.
          IF v-lcnt GT 1 AND 
              begin_i-no:SCREEN-VALUE EQ "" AND v-barflg 
            THEN DO:
              MESSAGE 
                  "Item # can not be blank. Please enter an Item #."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
              APPLY "ENTRY" TO begin_i-no IN FRAME {&FRAME-NAME}.
              RETURN NO-APPLY.
          END.

          FIND FIRST oe-ord NO-LOCK
            WHERE oe-ord.company EQ cocode 
              AND oe-ord.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) 
              AND oe-ord.ord-no  LE INT(end_ord-no:SCREEN-VALUE) NO-ERROR.

          IF AVAIL oe-ord THEN
          DO:
             v-cust-no = oe-ord.cust-no.

             FIND FIRST reftable NO-LOCK
               WHERE reftable.reftable EQ "cp-lab-p" 
                 AND reftable.company  EQ cocode 
                 AND reftable.loc      EQ begin_i-no:SCREEN-VALUE
                 AND reftable.loc      EQ end_i-no:SCREEN-VALUE
                 AND reftable.CODE     EQ oe-ord.cust-no NO-ERROR.

             IF AVAIL reftable AND reftable.code2 NE "" THEN
                scr-label-file:SCREEN-VALUE = (IF reftable.dscr <> "" THEN reftable.dscr ELSE v-bardir-chr).
             ELSE
             DO:
                IF begin_i-no:SCREEN-VALUE NE "" AND 
                   end_i-no:SCREEN-VALUE NE "" THEN

                   FIND FIRST oe-rel NO-LOCK 
                     WHERE oe-rel.company EQ cocode 
                       AND oe-rel.i-no    GE begin_i-no:SCREEN-VALUE 
                       AND oe-rel.i-no    LE end_i-no:SCREEN-VALUE 
                       AND oe-rel.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) 
                       AND oe-rel.ord-no  LE INT(end_ord-no:SCREEN-VALUE) 
                     NO-ERROR.
                ELSE
                   FIND FIRST oe-rel NO-LOCK 
                     WHERE oe-rel.company EQ cocode 
                       AND oe-rel.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) 
                       AND oe-rel.ord-no  LE INT(end_ord-no:SCREEN-VALUE) 
                     NO-ERROR.

                IF AVAIL oe-rel THEN 
                   FIND FIRST shipto NO-LOCK 
                     WHERE shipto.company EQ cocode 
                       AND shipto.cust-no EQ oe-rel.cust-no 
                       AND shipto.ship-id EQ oe-rel.ship-id 
                     USE-INDEX ship-id NO-ERROR.
                ELSE
                   FIND FIRST shipto NO-LOCK 
                        WHERE shipto.company EQ cocode 
                          AND shipto.cust-no EQ v-cust-no 
                          AND shipto.ship-id EQ v-cust-no
                        USE-INDEX ship-id NO-ERROR.

                IF AVAIL shipto THEN DO:

                    IF AVAIL oe-rel THEN
                      v-cust-no = oe-rel.cust-no.

                   FIND FIRST sys-ctrl-shipto NO-LOCK
                     WHERE sys-ctrl-shipto.company      EQ cocode 
                       AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                       AND sys-ctrl-shipto.cust-vend    EQ YES 
                       AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no
                       AND sys-ctrl-shipto.ship-id      EQ shipto.ship-id
                       AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.

                   IF AVAIL sys-ctrl-shipto AND
                      TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                      scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                   ELSE 
                    FIND FIRST sys-ctrl-shipto NO-LOCK  
                      WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                        AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                        AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.

                   IF AVAIL sys-ctrl-shipto AND 
                      TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                      scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.

                     IF scr-label-file:SCREEN-VALUE EQ "" THEN DO:

                        FIND FIRST sys-ctrl-shipto WHERE
                             sys-ctrl-shipto.company      EQ cocode AND
                             sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                             sys-ctrl-shipto.cust-vend-no EQ "" AND
                             sys-ctrl-shipto.cust-vend    EQ YES 
                             NO-LOCK NO-ERROR.

                        IF AVAIL sys-ctrl-shipto AND 
                           TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                           scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                        ELSE
                        DO:
                           FIND FIRST sys-ctrl NO-LOCK 
                                WHERE sys-ctrl.company EQ cocode 
                                  AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.

                           IF AVAIL sys-ctrl THEN
                              scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                           ELSE scr-label-file:SCREEN-VALUE = "".
                        END.
                     END.
                END.
                else
                DO:

                   FIND FIRST sys-ctrl-shipto NO-LOCK  
                        WHERE sys-ctrl-shipto.company      EQ cocode 
                          AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                          AND sys-ctrl-shipto.cust-vend    EQ YES 
                          AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                          AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.

                   IF AVAIL sys-ctrl-shipto AND 
                      TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                      scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.

                   IF scr-label-file:SCREEN-VALUE EQ "" THEN  DO:
                      FIND FIRST sys-ctrl-shipto WHERE
                             sys-ctrl-shipto.company      EQ cocode AND
                             sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                             sys-ctrl-shipto.cust-vend-no EQ "" AND
                             sys-ctrl-shipto.cust-vend    EQ YES 
                             NO-LOCK NO-ERROR.

                        IF AVAIL sys-ctrl-shipto AND 
                           TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                           scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                        ELSE
                        DO:
                           FIND FIRST sys-ctrl NO-LOCK 
                                WHERE sys-ctrl.company EQ cocode 
                                  AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.

                           IF AVAIL sys-ctrl THEN
                              scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                           ELSE scr-label-file:SCREEN-VALUE = "".
                        END.
                   END.
                END.
             END.
          END.
       END.
    END.
    IF end_ord-no:LABEL = "To PO#" THEN DO:

        FIND FIRST bf-po-ord NO-LOCK 
            WHERE bf-po-ord.company EQ cocode 
            AND bf-po-ord.po-no  EQ int(end_ord-no:SCREEN-VALUE) NO-ERROR.
        IF AVAIL bf-po-ord THEN DO:

                FIND FIRST bf-po-ordl NO-LOCK
                    WHERE bf-po-ordl.company EQ bf-po-ord.company
                    AND bf-po-ordl.po-no  EQ bf-po-ord.po-no 
                    AND bf-po-ordl.i-no   EQ begin_i-no:SCREEN-VALUE NO-ERROR.
                IF NOT AVAIL bf-po-ordl THEN
                    FIND FIRST bf-po-ordl NO-LOCK
                    WHERE bf-po-ordl.company EQ bf-po-ord.company
                    AND bf-po-ordl.po-no  EQ bf-po-ord.po-no  NO-ERROR.

                IF AVAIL bf-po-ordl THEN do:

                    ASSIGN begin_i-no:SCREEN-VALUE = bf-po-ordl.i-no
                        end_i-no:SCREEN-VALUE   = bf-po-ordl.i-no
                        v-cust-no = bf-po-ordl.cust-no.

                     FIND FIRST reftable NO-LOCK
                         WHERE reftable.reftable EQ "cp-lab-p" 
                         AND reftable.company  EQ cocode 
                         AND reftable.loc      EQ begin_i-no:SCREEN-VALUE
                         AND reftable.loc      EQ end_i-no:SCREEN-VALUE
                         AND reftable.CODE     EQ v-cust-no NO-ERROR.

                     IF AVAIL reftable  THEN
                         scr-label-file:SCREEN-VALUE = (IF reftable.dscr <> "" THEN reftable.dscr ELSE v-bardir-chr).


                END.

        END.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cas-lab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cas-lab C-Win
ON ENTRY OF fi_cas-lab IN FRAME FRAME-A /* Scan Case Label */
DO:
/*   ASSIGN scr-label-file:SCREEN-VALUE = "". */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cas-lab C-Win
ON HELP OF fi_cas-lab IN FRAME FRAME-A /* Scan Case Label */
DO:
  DEF VAR rec-val AS RECID NO-UNDO.
  DEF VAR char-val AS cha NO-UNDO.

  IF tb_reprint-tag THEN RUN addon/windows/l-ldtaga.w (cocode, NO, no, FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
  ELSE RUN addon/windows/l-ldtagc.w (cocode, NO, FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
  IF char-val NE "" THEN DO:
    fi_cas-lab:SCREEN-VALUE = ENTRY(1,char-val).
    RUN new-cas-lab.
    IF RETURN-VALUE EQ 'ERROR' THEN
    APPLY 'ENTRY':U TO tb_ret.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cas-lab C-Win
ON LEAVE OF fi_cas-lab IN FRAME FRAME-A /* Scan Case Label */
DO:
  IF SELF:SCREEN-VALUE NE "" AND SELF:MODIFIED THEN DO:
    RUN new-cas-lab.
    IF RETURN-VALUE NE 'ERROR' THEN DO:
      SELF:SCREEN-VALUE = ''.
      APPLY 'ENTRY':U TO SELF.
      RETURN NO-APPLY.
    END.
    ELSE DO:

      APPLY 'ENTRY':U TO tb_ret.
      RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cas-lab C-Win
ON RETURN OF fi_cas-lab IN FRAME FRAME-A /* Scan Case Label */
DO:
  lReturn = YES.
  APPLY 'TAB' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME loadtagFunction
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL loadtagFunction C-Win
ON VALUE-CHANGED OF loadtagFunction IN FRAME FRAME-A
DO:
  ASSIGN {&SELF-NAME}.
  CASE {&SELF-NAME}:
    WHEN 'PO' THEN DO WITH FRAME {&FRAME-NAME}: 
      ASSIGN
        typeLabel:SCREEN-VALUE = REPLACE(typeLabel:SCREEN-VALUE,'Order','PO')
        begin_ord-no:LABEL = REPLACE(begin_ord-no:LABEL,'Order','PO')
        end_ord-no:LABEL = REPLACE(end_ord-no:LABEL,'Order','PO')
        statusLabel:SCREEN-VALUE = REPLACE(statusLabel:SCREEN-VALUE,'Order','PO')
        v-job-list:SCREEN-VALUE = ''
        begin_job:SCREEN-VALUE = ''
        begin_job2:SCREEN-VALUE = ''
        end_job:SCREEN-VALUE = ''
        end_job2:SCREEN-VALUE = ''.
      DISABLE {&jobFields}.
    END.
    WHEN 'Order' THEN DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
        typeLabel:SCREEN-VALUE = REPLACE(typeLabel:SCREEN-VALUE,'PO','Order')
        begin_ord-no:LABEL = REPLACE(begin_ord-no:LABEL,'PO','Order')
        end_ord-no:LABEL = REPLACE(end_ord-no:LABEL,'PO','Order')
        statusLabel:SCREEN-VALUE = REPLACE(statusLabel:SCREEN-VALUE,'PO','Order').
      ENABLE {&jobFields}.
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_print C-Win
ON VALUE-CHANGED OF rd_print IN FRAME FRAME-A
DO:
  IF {&self-name}:SCREEN-VALUE EQ "R" THEN DO:
    ASSIGN
     begin_date:SENSITIVE = YES
     end_date:SENSITIVE   = YES
     begin_ship-to:SENSITIVE = YES
     end_ship-to:SENSITIVE   = YES
     tb_xfer-lot:SENSITIVE = YES.

    APPLY "entry" TO begin_date.
  END.
  ELSE
    ASSIGN
     begin_date:SENSITIVE = NO
     end_date:SENSITIVE   = NO
     begin_ship-to:SENSITIVE = NO
     end_ship-to:SENSITIVE   = NO
     tb_xfer-lot:SENSITIVE = NO
     tb_xfer-lot:CHECKED = NO
     tb_xfer-lot = NO.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-label-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-label-file C-Win
ON HELP OF scr-label-file IN FRAME FRAME-A /* Print Format */
DO:
   DEF VAR chFile AS CHAR FORMAT "X(80)" NO-UNDO.
   DEF VAR ll-ok AS LOG NO-UNDO.

   /* gdm - 11050804 */
   DEF VAR v-path AS CHAR NO-UNDO.


   ASSIGN v-path = TRIM(scr-label-file:SCREEN-VALUE).

    IF TRIM(v-path) EQ "" THEN DO:
        FIND FIRST sys-ctrl NO-LOCK 
            WHERE sys-ctrl.company EQ cocode
              AND sys-ctrl.name EQ "CASLABEL" NO-ERROR.
        IF AVAIL sys-ctrl THEN
            ASSIGN v-path = TRIM(sys-ctrl.char-fld).

    END.
    RUN sys\ref\char-fld-help.w(INPUT cocode,
                                INPUT v-path,
                                OUTPUT chFile).


   /* gdm - 11050804 end

   DO WITH FRAME {&FRAME-NAME}:
      system-dialog get-file chFile 
                    title "Select Label Matrix Label File"
                    filters "Label Matrix (*.qdf) " "*.qdf"
                    initial-dir v_path
                    MUST-EXIST
                    USE-FILENAME
                    UPDATE ll-ok.

      IF ll-ok THEN
   */   
      ASSIGN scr-label-file:SCREEN-VALUE = chFile.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbPartSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbPartSelect C-Win
ON VALUE-CHANGED OF tbPartSelect IN FRAME FRAME-A /* Select Components */
DO:
  DEFINE VARIABLE cChoice AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER     NO-UNDO.

  IF tbPartSelect:SCREEN-VALUE = "YES" THEN DO:


    ASSIGN cPrevFromItem = begin_i-no:SCREEN-VALUE
           cPrevToItem   = END_i-no:SCREEN-VALUE.  
    RUN windows/l-setcomp.w (INPUT cocode, INPUT begin_i-no:SCREEN-VALUE, OUTPUT cChoice).

    IF NUM-ENTRIES(cChoice) GT 0 THEN DO:
       DO i = 1 TO NUM-ENTRIES(cChoice):
         CREATE tt-comps.
         ASSIGN tt-comps.comp = ENTRY(i, cChoice).
       END.
    END.
    ELSE DO:
      tbPartSelect:SCREEN-VALUE = "NO".
      RETURN.
    END.
/*     FOR EACH tt-comps BREAK BY tt-comps.comp:    */
/*       IF FIRST-OF(tt-comps.comp) THEN            */
/*         begin_i-no:SCREEN-VALUE = tt-comps.comp. */
/*       IF LAST-OF(tt-comps.comp) THEN             */
/*         END_i-no:SCREEN-VALUE = tt-comps.comp.   */
/*     END.                                         */

  END.
  ELSE DO:
/*     ASSIGN  begin_i-no:SCREEN-VALUE = cPrevFromItem   */
/*             END_i-no:SCREEN-VALUE   = cPrevToItem   . */
    EMPTY TEMP-TABLE tt-comps.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_16ths
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_16ths C-Win
ON VALUE-CHANGED OF tb_16ths IN FRAME FRAME-A /* Show LWD in 16ths? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_dept-note
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_dept-note C-Win
ON VALUE-CHANGED OF tb_dept-note IN FRAME FRAME-A /* Print Department Notes? */
DO:
   IF SELF:SCREEN-VALUE = "Yes" THEN ENABLE v-dept-list WITH FRAME {&FRAME-NAME}.
   ELSE DISABLE v-dept-list WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_over C-Win
ON VALUE-CHANGED OF tb_over IN FRAME FRAME-A /* Include Overrun? */
DO:
  IF {&self-name}:SCREEN-VALUE EQ "yes" THEN
    rd_print:SCREEN-VALUE = "R".
  ELSE
  IF rd_print:SCREEN-VALUE EQ "R" THEN
    rd_print:SCREEN-VALUE = lv-rd_print.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_override-mult
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_override-mult C-Win
ON VALUE-CHANGED OF tb_override-mult IN FRAME FRAME-A /* Ignore Customer Labels per Skid */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rel C-Win
ON VALUE-CHANGED OF tb_rel IN FRAME FRAME-A /* Print Posted Release in BOL File? */
DO:
  /*IF {&self-name}:SCREEN-VALUE EQ "yes" THEN
    rd_print:SCREEN-VALUE = "R".
  ELSE
  IF rd_print:SCREEN-VALUE EQ "R" THEN
    rd_print:SCREEN-VALUE = lv-rd_print.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint-tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint-tag C-Win
ON VALUE-CHANGED OF tb_reprint-tag IN FRAME FRAME-A /* Reprint Tag? */
DO:
  ASSIGN {&SELF-NAME}.

  IF tb_reprint-tag THEN DO:
    ASSIGN
      fi_cas-lab:HIDDEN    = NO
      fi_cas-lab:SENSITIVE = YES
      fi_cas-lab:LABEL = "Tag#"
      fi_cas-lab:BGCOLOR = 14.
    DISABLE {&NonReprint} WITH FRAME {&FRAME-NAME}.
  END.
  ELSE DO:
    ASSIGN
      fi_cas-lab:HIDDEN = NOT v-cas-lab
      fi_cas-lab:LABEL = "Scan Case Label"
      fi_cas-lab:BGCOLOR = ?.
    ENABLE {&NonReprint} WITH FRAME {&FRAME-NAME}.
  END.
  APPLY "entry" TO fi_cas-lab.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ret
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ret C-Win
ON VALUE-CHANGED OF tb_ret IN FRAME FRAME-A /* Returns? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ship-id C-Win
ON VALUE-CHANGED OF tb_ship-id IN FRAME FRAME-A /* Print Ship ID? */
DO:
   IF SELF:SCREEN-VALUE = "Yes" THEN ENABLE v-ship-id WITH FRAME {&FRAME-NAME}.
   ELSE DISABLE v-ship-id WITH FRAME {&FRAME-NAME}.
   ASSIGN tb_ship-id.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_xfer-lot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_xfer-lot C-Win
ON VALUE-CHANGED OF tb_xfer-lot IN FRAME FRAME-A /* Transfer Release Lot# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-job-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-job-list C-Win
ON LEAVE OF v-job-list IN FRAME FRAME-A
DO: 
   IF loadtagFunction:SCREEN-VALUE EQ "order" AND
      NUM-ENTRIES(v-job-list:SCREEN-VALUE) EQ 1 AND v-bardir THEN
      RUN get-jobord-info.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-job-list C-Win
ON RETURN OF v-job-list IN FRAME FRAME-A
DO:
  APPLY 'tab' TO v-job-list.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ord-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ord-list C-Win
ON LEAVE OF v-ord-list IN FRAME FRAME-A
DO: 

    IF loadtagFunction:SCREEN-VALUE EQ "order" THEN
    DO:
       IF NUM-ENTRIES(v-ord-list:SCREEN-VALUE) EQ 1 AND v-bardir THEN
          RUN get-ordl-info.
    END.

    ELSE /*loadtagFunction:SCREEN-VALUE ne "order"*/
       IF NUM-ENTRIES(v-ord-list:SCREEN-VALUE) EQ 1 AND v-bardir THEN
          RUN get-po-info.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ship-id C-Win
ON HELP OF v-ship-id IN FRAME FRAME-A
DO:
    def var char-val as cha no-undo.
    IF (begin_ord-no:SCREEN-VALUE NE end_ord-no:SCREEN-VALUE 
       OR begin_ord-no:SCREEN-VALUE = "" 
       OR end_ord-no:SCREEN-VALUE = "") 
       AND SELF:SCREEN-VALUE NE "" THEN DO:
       MESSAGE "Ship ID can only be overridden when creating a loadtag for one order." 
       VIEW-AS ALERT-BOX ERROR.
       RETURN.
    END.
    IF loadtagFunction = "Order" THEN 
        FIND FIRST oe-ord WHERE oe-ord.company = g_company 
                        AND oe-ord.ord-no = INT(begin_ord-no:SCREEN-VALUE)
                        NO-LOCK NO-ERROR.
    ELSE
        FIND FIRST po-ord WHERE po-ord.company = g_company
                        AND po-ord.po-no = INT(begin_ord-no:SCREEN-VALUE)
                        NO-LOCK NO-ERROR.
    IF AVAIL oe-ord THEN DO:
        IF oe-ord.cust-no NE "" THEN run windows/l-shipto.w (g_company, g_loc, oe-ord.cust-no, focus:screen-value, output char-val).
        if char-val <> "" then self:screen-value = entry(1,char-val).
    END.
    ELSE IF AVAIL po-ord THEN DO:
        IF po-ord.cust-no NE "" THEN run windows/l-shipto.w (g_company, g_loc, po-ord.cust-no, focus:screen-value, output char-val).
        if char-val <> "" then self:screen-value = entry(1,char-val).
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ship-id C-Win
ON LEAVE OF v-ship-id IN FRAME FRAME-A
DO:
    IF SELF:SCREEN-VALUE = "" THEN RETURN.
    IF (begin_ord-no:SCREEN-VALUE NE end_ord-no:SCREEN-VALUE 
       OR begin_ord-no:SCREEN-VALUE = "" 
       OR end_ord-no:SCREEN-VALUE = "") 
       AND SELF:SCREEN-VALUE NE "" THEN DO:
       MESSAGE "Ship ID can only be overridden when creating a loadtag for one order or PO." 
       VIEW-AS ALERT-BOX ERROR.
       RETURN.
   END.
   IF loadtagFunction = "Order" THEN
        FIND FIRST oe-ord WHERE oe-ord.company = g_company 
                        AND oe-ord.ord-no = INT(begin_ord-no:SCREEN-VALUE)
                        NO-LOCK NO-ERROR.
   ELSE
        FIND FIRST po-ord WHERE po-ord.company = g_company
                        AND po-ord.po-no = INT(begin_ord-no:SCREEN-VALUE)
                        NO-LOCK NO-ERROR.
   IF AVAIL oe-ord THEN DO:
        FIND FIRST shipto WHERE shipto.company = g_company
                        AND shipto.cust-no = oe-ord.cust-no
                        AND shipto.ship-id = SELF:SCREEN-VALUE
                        NO-LOCK NO-ERROR.
        IF NOT AVAIL shipto THEN DO:
            MESSAGE "Invalid Ship ID. Please use F1 to lookup valid values." VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
        ELSE ASSIGN v-ship-id.
    END.
    ELSE IF AVAIL po-ord THEN DO:
        FIND FIRST shipto WHERE shipto.company = g_company
                        AND shipto.cust-no = po-ord.cust-no
                        AND shipto.ship-id = SELF:SCREEN-VALUE
                        NO-LOCK NO-ERROR.
        IF NOT AVAIL shipto THEN DO:
            MESSAGE "Invalid Ship ID. Please use F1 to lookup valid values." VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
        ELSE ASSIGN v-ship-id.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


PROCEDURE WinExec EXTERNAL "KERNEL32.DLL":
     DEFINE INPUT  PARAMETER ProgramName AS CHARACTER.
     DEFINE INPUT  PARAMETER VisualStyle AS LONG.
     DEFINE RETURN PARAMETER StatusCode  AS LONG.
END PROCEDURE.



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

  FIND FIRST company WHERE company.company EQ gcompany NO-LOCK.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "CEMENU"
      NO-LOCK NO-ERROR.
  ASSIGN
   tb_16ths  = AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "Corrware".

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

  /* gdm - 09210907 */
  FIND FIRST sys-ctrl NO-LOCK 
    WHERE sys-ctrl.company EQ gcompany
      AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
  IF AVAIL sys-ctrl THEN ASSIGN v-bardir = sys-ctrl.log-fld
                                v-bardir-chr = sys-ctrl.char-fld.
  /* gdm - 09210907 end */

  DO TRANSACTION:
     {sys/inc/closejob.i FGPost}
     {sys/inc/fgpostgl.i}   
     {sys/ref/oecount.i}
     {sys/inc/sspostfg.i}
     {sys/inc/bardir.i}     
  END.

  ASSIGN v-fgpostgl  = fgpostgl.

  if v-loadtag eq "TRIAD" then begin_form = 4.

  if v-mult le 0 then v-mult = 1.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "FGRECPT"
      NO-LOCK NO-ERROR.
  ASSIGN
   v-fgrecpt = AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "LoadTag".

  /* Override of sys/inc/closejob.i */
  IF SSPostFG-log AND SSPostFG-char EQ "LOADTAG" AND v-close-job EQ 0 THEN DO:


    RUN sys/ref/nk1look.p (cocode, "CLOSEJOB", "I", no, no, "", "", 
                          Output lcRtnChar, output llRecFound).

    /* Orign: v-close-job = int(sys-ctrl.char-fld eq "{1}") + sys-ctrl.int-fld. */
    /* Calculate v-close-job as though sys-ctrl.char-fld was not FGPOST */
    v-close-job = 0 + INT(lcRtnChar).
  END.
  EMPTY TEMP-TABLE tt-fgrctd-created.

  DO WITH FRAME {&FRAME-NAME}:
    RUN enable_UI.   
    ASSIGN
     v-ord-list:SCREEN-VALUE   = ""
     v-job-list:SCREEN-VALUE   = ""
     begin_ord-no:SCREEN-VALUE = ""
     end_ord-no:SCREEN-VALUE   = ""
     begin_job:SCREEN-VALUE    = ""
     end_job:SCREEN-VALUE      = ""
     begin_job2:SCREEN-VALUE   = ""
     end_job2:SCREEN-VALUE     = ""
     begin_i-no:SCREEN-VALUE   = ""
     end_i-no:SCREEN-VALUE     = ""
     begin_ship-to:SCREEN-VALUE   = ""
     end_ship-to:SCREEN-VALUE     = "zzzzzzzz"
     lv-rd_print               = rd_print:SCREEN-VALUE
     tb_ret:SCREEN-VALUE       = "NO"
     tb_reprint-tag:SCREEN-VALUE = "NO" 
     tb_reprint-tag = NO
     fi_cas-lab:SCREEN-VALUE = ""
     begin_filename:SCREEN-VALUE = bardir-desc
     userLabelPath = bardir-desc
     tbPartSelect:SCREEN-VALUE = "NO"
     .

    IF bardir-int = 1 THEN DO:
       FIND FIRST users WHERE users.user_id EQ USERID("NOSWEAT") NO-LOCK NO-ERROR.
       IF AVAIL users AND users.user_program[3] NE "" THEN
           ASSIGN begin_filename:SCREEN-VALUE = users.user_program[3]
                  userLabelPath = users.USER_program[3].       
    END.

    FIND FIRST sys-ctrl NO-LOCK 
      WHERE sys-ctrl.company EQ gcompany 
        AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
    IF AVAIL sys-ctrl 
      THEN ASSIGN v-barflg = sys-ctrl.log-fld
                  v-auto-print   = sys-ctrl.log-fld.
     scr-auto-print:SCREEN-VALUE = STRING(sys-ctrl.log-fld).
    /* gdm - 04090909 end */

    {custom/usrprint.i}   
    ASSIGN 
        tb_ship-id:SCREEN-VALUE = "NO"
        tb_ship-id = NO
        v-ship-id:SCREEN-VALUE = ""
        v-ship-id = ""
        tbPartSelect:SCREEN-VALUE = "NO"
        begin_labels:SCREEN-VALUE = STRING(v-mult)
        begin_labels = v-mult
        begin_ship-to:SCREEN-VALUE   = ""
        end_ship-to:SCREEN-VALUE     = "zzzzzzzz"
        .

    DISABLE v-ship-id.
    ASSIGN v-ord-list:SCREEN-VALUE = "".

    IF v-loadtag EQ "TRIAD" THEN
        begin_form = 4.
    ELSE
       begin_form:VISIBLE = NO.

     ASSIGN
       begin_filename:SCREEN-VALUE = bardir-desc.

    IF bardir-int = 1 THEN DO:
       FIND FIRST users WHERE users.user_id EQ USERID("NOSWEAT") NO-LOCK NO-ERROR.
       IF AVAIL users AND users.user_program[3] NE "" THEN
           ASSIGN begin_filename:SCREEN-VALUE = users.user_program[3].                  
    END.
    
     IF  PROGRAM-NAME(3) MATCHES "*/b-ordlt.*" THEN DO: 
      
      ASSIGN
        v-ord-list     = ""
        v-job-list     = ""
        v-ord-list:SCREEN-VALUE   = ""
        v-job-list:SCREEN-VALUE   = ""
        fi_cas-lab:SCREEN-VALUE   = "" .
      /* gdm - 06100901 end */

      IF  begin_i-no:SCREEN-VALUE NE '' THEN DO:  
        FIND FIRST po-ordl NO-LOCK
             WHERE po-ordl.company EQ cocode
               AND po-ordl.po-no = int(begin_ord-no:SCREEN-VALUE)  NO-ERROR.
        IF AVAIL po-ordl THEN DO:
          FIND FIRST itemfg NO-LOCK
               WHERE itemfg.company EQ cocode
                 AND itemfg.i-no EQ begin_i-no:SCREEN-VALUE
               NO-ERROR.
        END.

        IF (AVAIL itemfg AND itemfg.pur-man AND NOT itemfg.isaset)            
            AND AVAIL po-ordl THEN
          ASSIGN loadtagFunction:SCREEN-VALUE = "Po"
                 begin_job:SCREEN-VALUE       = ""
                 end_job:SCREEN-VALUE         = "".

      END. /* i-no ne '' */
      ELSE
        loadtagFunction:SCREEN-VALUE = "order".
     END.  /*   PROGRAM-NAME(3) MATCHES "b-ordlt." */

     IF  PROGRAM-NAME(3) MATCHES "*/b-trans.*" THEN DO:
      /*IF SEARCH('IU2-loadtag.txt') NE ? THEN DO:*/
      /* gdm - 06100901 end */
      ASSIGN
        v-ord-list     = ""
        v-job-list     = ""
        v-ord-list:SCREEN-VALUE     = ""
        v-job-list:SCREEN-VALUE     = ""
        begin_ord-no:SCREEN-VALUE = ""
        end_ord-no:SCREEN-VALUE   = "" 
        begin_job:SCREEN-VALUE    = ""
        begin_job:SCREEN-VALUE    = ""
        end_job:SCREEN-VALUE      = ""
        begin_job2:SCREEN-VALUE   = ""
        end_job2:SCREEN-VALUE     = ""  
        begin_i-no:SCREEN-VALUE   = ""  
        end_i-no:SCREEN-VALUE     = "" .
      /* gdm - 06100901 end */

        IF fi_cas-lab:SCREEN-VALUE NE "" THEN
         ASSIGN tb_reprint-tag = YES
             tb_reprint-tag:SCREEN-VALUE = "Yes"
             .
      loadtagFunction:SCREEN-VALUE = "order".
      /* APPLY 'value-changed' TO tb_reprint-tag. */
     END.       /* b-trans  */ 

    APPLY 'VALUE-CHANGED':U TO loadtagFunction.
    APPLY 'VALUE-CHANGED':U TO tb_dept-note.

    IF rd_print:SCREEN-VALUE NE "R" THEN DISABLE begin_date end_date begin_ship-to end_ship-to .

    {methods/nowait.i}    

    APPLY "entry" TO v-ord-list.

    IF v-cas-lab THEN DO:
      ASSIGN
       fi_cas-lab:HIDDEN    = NO
       fi_cas-lab:SENSITIVE = YES.

      APPLY "entry" TO fi_cas-lab.
    END.

    IF end_ord-no:SCREEN-VALUE NE "" 
      THEN APPLY "leave" TO end_ord-no.

    IF end_i-no:SCREEN-VALUE NE "" 
      THEN APPLY "leave" TO end_i-no.

    IF tb_reprint-tag:SCREEN-VALUE = "YES" THEN DO:
       tb_reprint-tag = YES.
       APPLY 'value-changed' TO tb_reprint-tag.
       /* tb_reprint-tag:SCREEN-VALUE = "yes". */
    END.

/*
    IF begin_filename:SCREEN-VALUE = "" AND userLabelPath <> "" THEN        
        begin_filename:SCREEN-VALUE = userLabelPath.
*/  
    /* gdm - 06100901 */  
    IF  PROGRAM-NAME(3) MATCHES "*/mainmenu.*" 
    /* Next phrase accounts for direct load from icon or combined launcher */
    or (program-name(1) matches "*r-loadtg.*"
        and not program-name(2) matches "*persist*") THEN do:

       /* gdm - 06050908 */
       ASSIGN
        v-ord-list:SCREEN-VALUE     = ""  
        v-job-list:SCREEN-VALUE     = ""  
        begin_ord-no:SCREEN-VALUE   = ""  
        end_ord-no:SCREEN-VALUE     = ""  
        begin_job:SCREEN-VALUE      = ""  
        end_job:SCREEN-VALUE        = ""  
        begin_job2:SCREEN-VALUE     = ""  
        end_job2:SCREEN-VALUE       = ""  
        begin_i-no:SCREEN-VALUE     = ""  
        end_i-no:SCREEN-VALUE       = ""  
        tb_reprint-tag:SCREEN-VALUE = "NO"
        fi_cas-lab:SCREEN-VALUE     = ""  

        v-ord-list     = ""
        v-job-list     = "" 
        begin_ord-no   = 0  
        end_ord-no     = 0  
        begin_job      = "" 
        end_job        = "" 
        begin_job2     = 0  
        end_job2       = 0  
        begin_i-no     = "" 
        end_i-no       = "" 
        tb_reprint-tag = NO 
        fi_cas-lab     = "".
       APPLY 'value-changed' TO tb_reprint-tag.
       /* gdm - 06050908 */
    END.
    /* gdm - 06100901 end */

  END.
  lForm = NO.
  iForm = 0.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.

  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE askNextPalletID C-Win 
PROCEDURE askNextPalletID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipc-cust AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER opl-error AS LOG NO-UNDO.

 MESSAGE "The Pallet ID has reached its limit." skip
         "Please reset it for customer " ipc-cust 
     VIEW-AS ALERT-BOX .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AutoPrint C-Win 
PROCEDURE AutoPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cBarDir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDB AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lUserSpecific AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cPath         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLockPath     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lLockWasRemoved AS LOGICAL     NO-UNDO.
    def var cProtocol        as char no-undo.
    def var cComputerName    as char no-undo.
    def var cSharedFolder    as char no-undo.
    def var cDrive           as char no-undo.
    def var cDir             as char no-undo.
    def var cFile            as char no-undo.
    def var cExt             as char no-undo.


/* Need to check if labelMatrix has removed the lockFile */
IF  lLabelMatrixLock THEN DO:
RUN win_breakPath (INPUT scr-label-file,
     OUTPUT cProtocol        ,
     OUTPUT cComputerName    ,
     OUTPUT cSharedFolder    ,
     OUTPUT cDrive           ,
     OUTPUT cDir             ,
     OUTPUT cFile            ,
     OUTPUT cExt             ).
  /* Put the lock file in the label matrix path */

  cLockPath = cDrive + cDir + "lm.lock".

  lLockWasRemoved = TRUE.
  IF SEARCH(cLockPath) NE ? THEN
    RUN oe/w-lockwait.w (INPUT cLockPath, OUTPUT lLockWasRemoved).


  /* Test if User Hit Cancel */
  IF NOT lLockWasRemoved THEN
    RETURN.

END.

IF scr-auto-print THEN DO:
    RUN sys/ref/GetBarDir.p (INPUT cocode,
                             INPUT "loadtag",
                             OUTPUT cBarDir,
                             OUTPUT cDB,
                             OUTPUT lUserSpecific).

    IF lUserSpecific THEN 
        RUN custom/lmprint.p (INPUT scr-label-file, 
                              INPUT cDB,
                              INPUT cBarDir).
    ELSE
        RUN custom/lmprint.p (INPUT scr-label-file,
                              INPUT "",
                              INPUT "").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-ext-cost C-Win 
PROCEDURE calc-ext-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty as dec no-undo.
  def var lv-out-cost as dec no-undo.
  DEF VAR lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.
  DEF VAR v-rec-qty AS INT NO-UNDO.


  if not avail fg-rctd then return.  /* no records */


find itemfg where itemfg.company eq cocode and itemfg.i-no  eq fg-rctd.i-no
            use-index i-no no-lock no-error.

ASSIGN
 lv-cost-uom = itemfg.prod-uom
 v-bwt       = 0
 v-len       = itemfg.t-len
 v-wid       = itemfg.t-wid
 v-dep       = 0.

  find first po-ordl where po-ordl.company = fg-rctd.company
                       and po-ordl.po-no = int(fg-rctd.po-no)
                       and po-ordl.i-no  = fg-rctd.i-no
                       and po-ordl.job-no = fg-rctd.job-no
                       and po-ordl.job-no2 = fg-rctd.job-no2
                       and po-ordl.item-type = no
                       no-lock no-error.

  IF AVAIL po-ordl THEN DO:
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid.
  END.

  ASSIGN lv-out-qty  = fg-rctd.t-qty
         lv-out-cost = fg-rctd.std-cost.

  IF fg-rctd.cost-uom NE lv-cost-uom THEN
    RUN rm/convcuom.p(fg-rctd.cost-uom, lv-cost-uom,                   
                      v-bwt, v-len, v-wid, v-dep,
                      fg-rctd.std-cost, OUTPUT lv-out-cost).

  IF lv-cost-uom NE "EA" THEN
     RUN rm/convquom.p("EA", lv-cost-uom,                   
                       v-bwt, v-len, v-wid, v-dep,
                       lv-out-qty, OUTPUT lv-out-qty).
ASSIGN fg-rctd.ext-cost = (lv-out-qty * lv-out-cost) + fg-rctd.frt-cost.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cas-lab-label-mat-file C-Win 
PROCEDURE cas-lab-label-mat-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-lcnt AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
  IF v-auto-print AND
     LOGICAL(scr-freeze-label:SCREEN-VALUE) EQ NO AND
     INT(begin_ord-no:SCREEN-VALUE) NE 0 AND
     INT(end_ord-no:SCREEN-VALUE) NE 0 THEN
     DO:
        FOR EACH oe-rel WHERE 
            oe-rel.company EQ cocode AND
            oe-rel.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) AND
            oe-rel.ord-no  LE INT(end_ord-no:SCREEN-VALUE)
            NO-LOCK:

            v-lcnt = v-lcnt + 1.
            IF v-lcnt GT 1 THEN LEAVE.

        END.
        IF v-lcnt GT 1 AND 
           begin_i-no:SCREEN-VALUE EQ "" AND v-barflg THEN
           DO:
              MESSAGE 
                 "Item # cannot be blank. Please enter an Item #."
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
              APPLY "ENTRY" TO begin_i-no IN FRAME {&FRAME-NAME}.
              RETURN NO-APPLY.
           END.

        FIND FIRST oe-ord WHERE
             oe-ord.company EQ cocode AND
             oe-ord.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) AND
             oe-ord.ord-no  LE INT(end_ord-no:SCREEN-VALUE)
             NO-LOCK NO-ERROR.

        IF AVAIL oe-ord THEN
        DO:
           FIND FIRST reftable WHERE
                reftable.reftable EQ "cp-lab-p" AND
                reftable.company  EQ cocode AND
                reftable.loc      EQ begin_i-no:SCREEN-VALUE AND
                reftable.loc      EQ end_i-no:SCREEN-VALUE AND
                reftable.CODE     EQ oe-ord.cust-no
                NO-LOCK NO-ERROR.
           IF AVAIL reftable AND reftable.code2 NE "" THEN
              scr-label-file:SCREEN-VALUE = (IF reftable.dscr <> "" THEN reftable.dscr ELSE bardir-chr).
           ELSE DO:
              IF begin_i-no:SCREEN-VALUE NE "" AND 
                 end_i-no:SCREEN-VALUE NE "" THEN
                 FIND FIRST oe-rel WHERE
                      oe-rel.company EQ cocode AND
                      oe-rel.i-no    GE begin_i-no:SCREEN-VALUE AND
                      oe-rel.i-no    LE end_i-no:SCREEN-VALUE AND
                      oe-rel.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) AND
                      oe-rel.ord-no  LE INT(end_ord-no:SCREEN-VALUE) 
                      NO-LOCK NO-ERROR.
              ELSE
                 FIND FIRST oe-rel WHERE
                      oe-rel.company EQ cocode AND
                      oe-rel.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) AND
                      oe-rel.ord-no  LE INT(end_ord-no:SCREEN-VALUE) 
                      NO-LOCK NO-ERROR.

              IF AVAIL oe-rel THEN
                 FIND FIRST shipto WHERE
                      shipto.company EQ cocode AND
                      shipto.cust-no EQ oe-rel.cust-no AND
                      shipto.ship-id EQ oe-rel.ship-id 
                      USE-INDEX ship-id NO-LOCK NO-ERROR.
              ELSE
                 FIND FIRST shipto WHERE
                      shipto.company EQ cocode AND
                      shipto.cust-no EQ oe-ord.cust-no AND
                      shipto.ship-id EQ oe-ord.ship-id
                    USE-INDEX ship-id NO-LOCK NO-ERROR.

              IF AVAIL shipto THEN DO:
                 FIND FIRST sys-ctrl-shipto WHERE
                      sys-ctrl-shipto.company EQ cocode AND
                      sys-ctrl-shipto.NAME    EQ "BARDIR" AND
                      sys-ctrl-shipto.cust-vend    EQ YES AND
                      sys-ctrl-shipto.cust-vend-no EQ oe-rel.cust-no AND
                      sys-ctrl-shipto.ship-id      EQ shipto.ship-id AND
                      sys-ctrl-shipto.char-fld     NE ''
                      NO-LOCK NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                     scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                 ELSE 
                    FIND FIRST sys-ctrl-shipto WHERE
                         sys-ctrl-shipto.company      EQ cocode AND
                         sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                         sys-ctrl-shipto.cust-vend    EQ YES AND
                         sys-ctrl-shipto.cust-vend-no EQ oe-rel.cust-no AND
                         sys-ctrl-shipto.char-fld     NE ''
                         NO-LOCK NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
              END.
              ELSE
              DO:
                 FIND FIRST sys-ctrl-shipto NO-LOCK 
                   WHERE sys-ctrl-shipto.company      EQ cocode 
                     AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                     AND sys-ctrl-shipto.cust-vend    EQ YES 
                     AND sys-ctrl-shipto.cust-vend-no EQ oe-ord.cust-no
                     AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                 IF AVAIL sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" 
                   THEN scr-label-file:SCREEN-VALUE = 
                                              sys-ctrl-shipto.char-fld.
                   ELSE DO:
                      FIND FIRST sys-ctrl-shipto NO-LOCK 
                        WHERE sys-ctrl-shipto.company      EQ cocode 
                          AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                          AND sys-ctrl-shipto.cust-vend-no EQ ""
                          AND sys-ctrl-shipto.cust-vend    EQ YES 
                        NO-ERROR.
                      IF AVAIL sys-ctrl-shipto AND 
                         TRIM(sys-ctrl-shipto.char-fld) NE "" 
                        THEN 
                          scr-label-file:SCREEN-VALUE = 
                                            sys-ctrl-shipto.char-fld.
                        ELSE DO:
                         FIND FIRST sys-ctrl NO-LOCK 
                           WHERE sys-ctrl.company EQ cocode 
                             AND sys-ctrl.name    EQ "BARDIR" 
                           NO-ERROR.
                         IF AVAIL sys-ctrl 
                           THEN
                            scr-label-file:SCREEN-VALUE = 
                                                    sys-ctrl.char-fld.
                           ELSE scr-label-file:SCREEN-VALUE = "".
                        END.
                   END.
              END.
           END.
        END.
        IF scr-label-file:SCREEN-VALUE EQ "" THEN DO:
           FIND FIRST sys-ctrl NO-LOCK 
                WHERE sys-ctrl.company EQ cocode 
                  AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
            IF AVAIL sys-ctrl THEN
               scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
        END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkReturns C-Win 
PROCEDURE checkReturns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF tb_ret:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "NO" THEN DO:
    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ cocode
          AND oe-ordl.ord-no  EQ loadtag.ord-no
          AND oe-ordl.i-no    EQ loadtag.i-no
          AND oe-ordl.job-no  EQ loadtag.job-no
          AND oe-ordl.job-no2 EQ loadtag.job-no2
        NO-LOCK NO-ERROR.
    IF AVAIL oe-ordl AND
       CAN-FIND(FIRST oe-rel
                WHERE oe-rel.company EQ oe-ordl.company
                  AND oe-rel.ord-no  EQ oe-ordl.ord-no
                  AND oe-rel.i-no    EQ oe-ordl.i-no
                  AND oe-rel.line    EQ oe-ordl.line
                  AND oe-rel.qty     LT 0) THEN DO:
      /* tb_ret:SCREEN-VALUE = "YES". */
      MESSAGE 'Negative Release / Returns Exist for this Order' SKIP(1)
        'If the New Load Tags are Re-Working the Returns,' SKIP
        'Check the Returns? Parameter' VIEW-AS ALERT-BOX.
      RETURN 'ERROR'.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-fields C-Win 
PROCEDURE clear-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN begin_ord-no:SCREEN-VALUE = ""
         begin_job:SCREEN-VALUE    = ""
         begin_job2:SCREEN-VALUE   = ""
         begin_i-no:SCREEN-VALUE   = ""
         end_ord-no:SCREEN-VALUE   = ""
         end_job:SCREEN-VALUE      = ""
         end_job2:SCREEN-VALUE     = ""
         end_i-no:SCREEN-VALUE     = "".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE component-qty-check C-Win 
PROCEDURE component-qty-check :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER iprRctdRow AS ROWID NO-UNDO.
DEF VAR li-max-qty AS DEC NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF BUFFER bf-fg-rctd FOR fg-rctd.

FIND bf-fg-rctd WHERE ROWID(bf-fg-rctd) EQ iprRctdRow 
  EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL bf-fg-rctd THEN
  RETURN ERROR.

  IF itemfg.isaset                                                        AND
     (itemfg.alloc EQ NO                OR
      (itemfg.alloc EQ YES      AND
       fgrecpt-char NE "Manual" AND
       TRIM(bf-fg-rctd.job-no) NE "")) THEN
  DO:
    ASSIGN
     bf-fg-rctd.t-qty =
         bf-fg-rctd.cases *
                 bf-fg-rctd.qty-case +
                 bf-fg-rctd.partial
     li-max-qty = bf-fg-rctd.t-qty.

    RUN fg/checksetb.p (ROWID(itemfg),
                       ROWID(bf-fg-rctd),
                       bf-fg-rctd.job-no,
                       INT(bf-fg-rctd.job-no2),
                       INPUT bf-fg-rctd.loc,
                       INPUT-OUTPUT li-max-qty).

    IF li-max-qty LT bf-fg-rctd.t-qty THEN DO:
      ll = NO.

      IF li-max-qty GT 0 AND NOT gvlCreateWithMaxPrompted THEN
      MESSAGE "Create receipt with maximum quantity available?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.

      /* Only prompt on the first tag */
      gvlCreateWithMaxPrompted = YES.
      IF ll THEN DO:

        ASSIGN
         bf-fg-rctd.t-qty  = li-max-qty
         bf-fg-rctd.cases  = 
              TRUNC((li-max-qty - DEC(bf-fg-rctd.partial)) /
                           DEC(bf-fg-rctd.qty-case),0)
         bf-fg-rctd.partial = 
                     li-max-qty - (DEC(bf-fg-rctd.cases) *
                                   DEC(bf-fg-rctd.qty-case)).
       /* Instead of 0 at 500 with -300 partial, make it */
       /* 1 at -300 with 0 partial 12101418 */
        IF fg-rctd.cases EQ 0 AND fg-rctd.partial NE 0 THEN
          ASSIGN 
              fg-rctd.cases = (IF  fg-rctd.partial LT 0 THEN -1 ELSE 1)
              fg-rctd.qty-case = (IF fg-rctd.partial LT 0 THEN - fg-rctd.partial ELSE fg-rctd.partial)
              fg-rctd.partial = 0
              .
      END. /* if ll */
      IF NOT ll OR li-max-qty EQ 0 THEN DO:   

        ASSIGN
          gvcSkippedJob = bf-fg-rctd.job-no + "-" + STRING(bf-fg-rctd.job-no2)
          gvcSkippedItem = itemfg.i-no.
        RETURN ERROR. 
      END.

    END. /* if over qty */
  END. /* if isaset */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convert-vend-comp-curr C-Win 
PROCEDURE convert-vend-comp-curr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-po-no AS INT NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ip-cost AS DEC DECIMALS 4 NO-UNDO.

   DEF BUFFER b-po-ord FOR po-ord.
   DEF BUFFER b-company FOR company.

   FIND FIRST b-po-ord WHERE
        b-po-ord.company EQ cocode AND
        b-po-ord.po-no EQ ip-po-no
        NO-LOCK NO-ERROR.

   IF AVAIL b-po-ord THEN
   DO:
      FIND FIRST vend WHERE
           vend.company EQ b-po-ord.company AND
           vend.vend-no EQ b-po-ord.vend-no
           NO-LOCK NO-ERROR.

      IF AVAIL vend THEN
      DO:
         FIND FIRST b-company WHERE
              b-company.company EQ cocode
              NO-LOCK.

         IF vend.curr-code NE b-company.curr-code THEN
         DO:
            FIND FIRST currency WHERE
                 currency.company EQ b-po-ord.company AND
                 currency.c-code EQ vend.curr-code
                 NO-LOCK NO-ERROR.

            IF AVAIL currency THEN
            DO:
               ip-cost = ip-cost * currency.ex-rate.
               RELEASE currency.
            END.
         END.

         RELEASE b-company.
         RELEASE vend.
      END.

      RELEASE b-po-ord.
   END.
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
 DEF VAR cRfidTag AS CHAR NO-UNDO.

  DEF BUFFER b-loadtag FOR loadtag.
  DEF BUFFER b-po-ordl FOR po-ordl.
  DEF VAR lvCalcCostUom LIKE fg-rctd.cost-uom NO-UNDO.
  DEF VAR lvCalcStdCost LIKE fg-rctd.std-cost NO-UNDO.
  DEF VAR lvCalcExtCost LIKE fg-rctd.ext-cost NO-UNDO.
  DEF VAR lvCalcFrtCost LIKE fg-rctd.frt-cost NO-UNDO.
  DEF VAR lvSetupPerCostUom AS DEC NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-got-job AS LOG NO-UNDO.
  DEF VAR lv-out-cost AS DEC DECIMALS 4 NO-UNDO.
  DEF VAR lv-out-qty as dec no-undo.
  DEF VAR lv-from-uom AS CHAR NO-UNDO.
  DEF VAR lv-cost-uom AS CHAR NO-UNDO.
  DEF VAR lv-ord-qty AS INT NO-UNDO.
  DEF VAR lv-ord-uom AS CHAR NO-UNDO.
  DEF VAR lv-setup-included AS LOG NO-UNDO.
  DEF VAR lv-setup-per-cost-uom AS DEC NO-UNDO.
  DEF VAR lv-full-qty AS DEC NO-UNDO.
  DEF VAR lv-adjusted-qty AS DEC NO-UNDO.
  DEF VAR lv-use-full-qty AS LOG NO-UNDO.
  def var v-bwt like po-ordl.s-len no-undo.
  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo.
  DEF VAR dRFIDTag AS DEC NO-UNDO.
/*   DEF BUFFER bf-eb FOR eb. */
  DEF BUFFER bf-itemfg FOR itemfg.

  IF tb_reprint-tag THEN DO:
     FIND FIRST loadtag NO-LOCK
         WHERE loadtag.company   EQ cocode
           AND loadtag.item-type EQ NO
           AND loadtag.tag-no    EQ TRIM(fi_cas-lab:SCREEN-VALUE IN FRAME {&FRAME-NAME})
         USE-INDEX tag NO-ERROR.
     IF AVAIL loadtag THEN
       io-tag-no = (IF AVAIL loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.
     RETURN.
  END.

  FIND FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-ord.i-no
      NO-ERROR.

  FIND LAST loadtag NO-LOCK
      WHERE loadtag.company     EQ cocode
        AND loadtag.item-type   EQ NO
        AND loadtag.is-case-tag EQ NO
        AND loadtag.tag-no      BEGINS w-ord.i-no 
        AND SUBSTR(loadtag.tag-no,1,15) EQ w-ord.i-no
      USE-INDEX tag NO-ERROR.
  io-tag-no = (IF AVAIL loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.

  /* rstark - zoho13731 */
  IF CAN-FIND(FIRST sys-ctrl
              WHERE sys-ctrl.company EQ cocode
                AND sys-ctrl.name EQ 'LoadTagSSCC'
                AND sys-ctrl.log-fld EQ YES) AND
     CAN-FIND(FIRST sys-ctrl-shipto
              WHERE sys-ctrl-shipto.company EQ cocode
                AND sys-ctrl-shipto.name EQ 'LoadTagSSCC'
                AND sys-ctrl-shipto.cust-vend EQ YES
                AND sys-ctrl-shipto.cust-vend-no EQ w-ord.cust-no
                AND sys-ctrl-shipto.log-fld EQ YES) THEN
  RUN oerep/ldtagSSCC.p (cocode,w-ord.cust-no,OUTPUT w-ord.SSCC).

  CREATE loadtag.
  ASSIGN
   loadtag.company      = cocode
   loadtag.tag-no       = /*string(io-tag-no,"99999") + STRING(w-ord.i-no,"x(15)") */
                          STRING(CAPS(w-ord.i-no),"x(15)") + STRING(io-tag-no,"99999") 
   loadtag.item-type    = NO /*FGitem*/
   loadtag.job-no       = w-ord.job-no
   loadtag.job-no2      = w-ord.job-no2
   loadtag.ord-no       = IF can-find(FIRST cust WHERE cust.company = cocode
                                      AND cust.cust-no = itemfg.cust-no
                                      AND cust.active = "X")
                          THEN 0 ELSE w-ord.ord-no /* task# 07120508*/
   loadtag.i-no         = CAPS(w-ord.i-no)
   loadtag.i-name       = w-ord.i-name
   loadtag.qty          = w-ord.ord-qty
   loadtag.qty-case     = w-ord.pcs
   loadtag.case-bundle  = w-ord.bundle
   loadtag.pallet-count = ip-total-unit /*w-ord.pcs * w-ord.bundle*/
   loadtag.partial      = w-ord.partial /*w-ord.total-unit MOD w-ord.pcs*/
   loadtag.sts = "Printed"  /* task 10190414 */
   loadtag.tag-date = TODAY
   loadtag.tag-time = TIME
   /* gdm - 07170905 */
   loadtag.misc-dec[1] = w-ord.unit-wt 
   loadtag.misc-dec[2] = w-ord.pallt-wt
   loadtag.misc-char[2] = w-ord.lot
   /* gdm - 07170905  end */
   loadtag.spare-char-1 = w-ord.SSCC
   .

   /* gdm - 08260916 */
   IF loadtagFunction EQ 'PO' 
    THEN loadtag.po-no = INT(w-ord.po-no).   

  IF v-fgrecpt AND tb_ret THEN loadtag.tot-cases  = (loadtag.pallet-COUNT - loadtag.partial) / loadtag.case-bundle.

  IF v-loadtag = "CentBox" THEN DO:
     ASSIGN loadtag.loc = itemfg.def-loc
            loadtag.loc-bin = itemfg.def-loc-bin.
     FIND FIRST fg-bin WHERE fg-bin.company EQ itemfg.company
                          AND fg-bin.i-no    EQ itemfg.i-no
                          AND fg-bin.job-no  EQ w-ord.job-no
                          AND fg-bin.tag = loadtag.tag-no
                         NO-LOCK NO-ERROR.
      IF AVAIL fg-bin THEN
         ASSIGN loadtag.loc     = fg-bin.loc
                loadtag.loc-bin = fg-bin.loc-bin.

  END.
  ELSE RUN fg/autopost.p (ROWID(itemfg), w-ord.job-no, w-ord.job-no2,
                         OUTPUT loadtag.loc , OUTPUT loadtag.loc-bin).

  IF RFIDTag-log THEN DO:

     RUN nextRfidTag (cocode , OUTPUT cRfidTag).

     CREATE rfidtag.
     ASSIGN rfidtag.company = loadtag.company
            rfidtag.item-type = loadtag.item-type
            rfidtag.tag-no = loadtag.tag-no
            rfidtag.rfidtag = cRfidTag /* string(dRFIDTag)*/.
     RELEASE oe-ctrl.
  END.

  IF v-fgrecpt AND NOT tb_ret THEN DO:
    IF AVAIL itemfg THEN DO:
      li = 0.
      FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
      IF AVAIL fg-rctd AND fg-rctd.r-no GT li THEN li = fg-rctd.r-no.

      FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
      IF AVAIL fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.

      DO WHILE TRUE:
        li = li + 1.
        FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ li USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAIL fg-rcpth THEN NEXT.
        FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ li USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAIL fg-rctd THEN NEXT.
        LEAVE.
      END.

      CREATE fg-rctd.
      ASSIGN
       fg-rctd.r-no       = li + 1
       fg-rctd.rct-date   = TODAY
       fg-rctd.trans-time = TIME
       fg-rctd.company    = cocode
       fg-rctd.rita-code  = "R"
       fg-rctd.i-name     = itemfg.i-name
       fg-rctd.i-no       = loadtag.i-no
       fg-rctd.job-no     = loadtag.job-no
       fg-rctd.job-no2    = loadtag.job-no2
       fg-rctd.t-qty      = loadtag.pallet-count /*loadtag.qty*/
       fg-rctd.pur-uom    = itemfg.prod-uom
       fg-rctd.cost-uom   = itemfg.prod-uom
  /*     fg-rctd.std-cost   = IF AVAIL fg-bin THEN fg-bin.std-tot-cost ELSE itemfg.std-tot-cost */
       fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
       fg-rctd.qty-case   = loadtag.qty-case
       fg-rctd.partial    = loadtag.partial
       fg-rctd.cases      = IF loadtag.qty-case NE 0 THEN TRUNC(fg-rctd.t-qty / loadtag.qty-case,0) ELSE 0
       fg-rctd.cases-unit = loadtag.case-bundle
       fg-rctd.loc        = loadtag.loc
       fg-rctd.loc-bin    = loadtag.loc-bin
       fg-rctd.tag        = loadtag.tag-no
       fg-rctd.stack-code = loadtag.misc-char[2]
       fg-rctd.tot-wt     = loadtag.misc-dec[1] .
       /* Instead of 0 at 500 with -300 partial, make it */
       /* 1 at -300 with 0 partial 12101418 */
        IF fg-rctd.cases EQ 0 AND fg-rctd.partial NE 0 THEN
          ASSIGN 
              fg-rctd.cases = (IF  fg-rctd.partial LT 0 THEN -1 ELSE 1)
              fg-rctd.qty-case = (IF fg-rctd.partial LT 0 THEN - fg-rctd.partial ELSE fg-rctd.partial)
              fg-rctd.partial = 0
              .
      CREATE tt-fgrctd-created.
      ASSIGN fg-rctd-rowid = ROWID(fg-rctd)
             tt-fgrctd-created.is-component = w-ord.is-component.

      IF loadtagFunction EQ 'PO' THEN DO:
        fg-rctd.po-no = TRIM(STRING(loadtag.po-no,">>>>>>>>>>")).
        /* Task 09051410 */
        IF loadtag.po-no GT 0 THEN
          ASSIGN fg-rctd.job-no = ""
                 fg-rctd.job-no2 = 0.
      END.


      RELEASE job.
      RELEASE reftable.

      IF TRIM(fg-rctd.job-no) NE "" THEN
      FIND FIRST job
          WHERE job.company EQ fg-rctd.company
            AND job.job-no  EQ fg-rctd.job-no
            AND job.job-no2 EQ fg-rctd.job-no2
          USE-INDEX job NO-LOCK NO-ERROR.

      IF AVAIL job THEN DO:
        FIND FIRST job-hdr NO-LOCK
            WHERE job-hdr.company EQ cocode
              AND job-hdr.job-no  EQ loadtag.job-no
              AND job-hdr.job-no2 EQ loadtag.job-no2
              AND job-hdr.i-no    EQ itemfg.i-no
            NO-ERROR.
        IF AVAIL job-hdr THEN fg-rctd.std-cost = job-hdr.std-tot-cost.

        ELSE
        FIND FIRST reftable
            WHERE reftable.reftable EQ "jc/jc-calc.p"
              AND reftable.company  EQ job.company
              AND reftable.loc      EQ ""
              AND reftable.code     EQ STRING(job.job,"999999999")
              AND reftable.code2    EQ fg-rctd.i-no
            USE-INDEX reftable NO-LOCK NO-ERROR.

        IF AVAIL reftable AND reftable.val[5] NE 0 THEN
          fg-rctd.std-cost = reftable.val[5].
      END.

      IF NOT AVAIL job-hdr AND NOT AVAIL reftable THEN DO:
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ itemfg.company
              AND fg-bin.i-no    EQ itemfg.i-no
              AND fg-bin.job-no  EQ loadtag.job-no
              AND fg-bin.job-no2 EQ loadtag.job-no2 
              /*AND fg-bin.tag = loadtag.tag-no*/
            NO-LOCK NO-ERROR.
        fg-rctd.std-cost = IF AVAIL fg-bin THEN fg-bin.std-tot-cost
                                           ELSE itemfg.std-tot-cost.
      END.   

      /* WFK - check here is qty avail for components is sufficient, so */
      /*       it can be changed before updating the PO line            */     
      IF NOT fgSetRec-Int EQ 1 THEN
        RUN component-qty-check (INPUT ROWID(fg-rctd)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
          /* User chose not to create the tag with a lower qty */
          /* so remove created record                          */
          IF AVAIL(fg-rctd) THEN
            DELETE fg-rctd.

          RETURN ERROR.
      END.


      IF w-ord.po-no NE 0 AND (loadtagFunction EQ 'Order' OR
                               loadtagFunction EQ 'PO') THEN
      DO:
         fg-rctd.po-no = TRIM(STRING(w-ord.po-no,">>>>>>>>>>")).

         FIND FIRST b-po-ordl WHERE
              b-po-ordl.company EQ cocode AND
              b-po-ordl.po-no EQ w-ord.po-no AND
              b-po-ordl.item-type EQ NO AND
              b-po-ordl.i-no EQ loadtag.i-no
              NO-LOCK NO-ERROR.

         IF AVAIL b-po-ordl THEN
         DO:
            /* Created task 09261318 to be used by receiving screens in addition */            
             RUN fg/calcRcptCostFromPO.p 
               (INPUT cocode ,
               INPUT ROWID(b-po-ordl),
               INPUT ROWID(fg-rctd),
               INPUT fg-rctd.qty-case,
               INPUT fg-rctd.cases,
               INPUT fg-rctd.partial,
               INPUT fg-rctd.job-no,
               INPUT fg-rctd.job-no2,
               INPUT fg-rctd.cost-uom,
               INPUT fg-rctd.t-qty,
               OUTPUT lv-use-full-qty,
               OUTPUT lv-full-qty,
               OUTPUT lvCalcCostUom,
               OUTPUT lvCalcStdCost,
               OUTPUT lvCalcExtCost,
               OUTPUT lvCalcFrtCost,
               OUTPUT lvSetupPerCostUom).

            ASSIGN
               fg-rctd.cost-uom = lvCalcCostUom
               fg-rctd.std-cost = lvCalcStdCost.
               fg-rctd.ext-cost = lvCalcExtCost.

            IF fgpofrt-log THEN 
              fg-rctd.frt-cost = lvCalcFrtCost.

            ASSIGN 
              lv-out-cost = lvCalcStdCost            
              lv-setup-per-cost-uom = lvSetupPerCostUom.

         END.
      END. /*info from PO on Order*/

      ELSE DO:
         RUN calc-ext-cost .
      END.

    END.  /* avail itemfg */
        /* mdp adds logic to post loadtags 07/24/08 */

    /* gdm - */
    IF v-fgrecpt AND w-ord.est-no NE "" AND AVAIL fg-rctd THEN DO:
/*       FIND FIRST bf-eb                                       */
/*         WHERE bf-eb.company  EQ cocode                       */
/*           AND bf-eb.est-no   EQ w-ord.est-no                 */
/*           AND bf-eb.stock-no EQ w-ord.i-no NO-LOCK NO-ERROR. */
/*       IF AVAIL bf-eb THEN DO:                                */
/*         IF bf-eb.pur-man THEN DO: */
        FIND FIRST bf-itemfg 
            WHERE bf-itemfg.company EQ cocode
                AND bf-itemfg.i-no EQ w-ord.i-no NO-LOCK NO-ERROR.
        IF AVAIL bf-itemfg THEN DO:
            IF bf-itemfg.pur-man THEN DO:
                IF TRIM(fg-rctd.job-no) NE "" AND TRIM(fg-rctd.po-no) NE "" AND loadtagFunction EQ 'PO' THEN
                ASSIGN fg-rctd.job-no = "" 
                    fg-rctd.job-no2 = 0
                    fg-rctd.po-no = TRIM(STRING(w-ord.po-no,">>>>>>>>>>")).
                ELSE IF TRIM(fg-rctd.job-no) NE "" AND TRIM(fg-rctd.po-no) NE "" AND loadtagFunction EQ 'Order' THEN
                    ASSIGN
                    fg-rctd.po-no   = ""
                    fg-rctd.job-no  = loadtag.job-no
                    fg-rctd.job-no2 = loadtag.job-no2.

            END.
/*         ELSE IF NOT bf-eb.pur-man THEN DO: */
        ELSE IF NOT bf-itemfg.pur-man THEN DO:
          IF TRIM(fg-rctd.po-no) NE "" AND TRIM(loadtag.job-no) NE "" AND loadtagFunction EQ 'Order' THEN
             ASSIGN fg-rctd.po-no   = ""
                    fg-rctd.job-no  = loadtag.job-no
                    fg-rctd.job-no2 = loadtag.job-no2.
          ELSE IF TRIM(fg-rctd.po-no) NE "" AND TRIM(loadtag.job-no) NE "" AND loadtagFunction EQ 'Po' THEN
              ASSIGN fg-rctd.job-no = "" 
                    fg-rctd.job-no2 = 0
                    fg-rctd.po-no = TRIM(STRING(w-ord.po-no,">>>>>>>>>>")).
        END.
      END.
    END.
    /* gdm - */
    /*BV - added the following call to add Set Parts to IU1 ( */
    IF NOT (FGSetRec-Int EQ 1 AND itemfg.alloc NE YES) THEN
      RUN fg/comprcpt.p (ROWID(fg-rctd)).
   /* mdp posting logic ends here */
  END.  /* v-fgrecpt */

  ELSE IF v-fgrecpt AND tb_ret AND AVAIL itemfg THEN DO:
       RUN post-return (RECID(fg-rctd)).
  END.

  /* Update the other tags with this new quantity */
  IF AVAIL fg-rctd AND lv-use-full-qty THEN
    RUN get-set-full-qty (INPUT fg-rctd.job-no, INPUT fg-rctd.job-no2, 
                          INPUT fg-rctd.i-no, INPUT 0 /* new qty */, 
                          INPUT fg-rctd.std-cost /* cost to set */, OUTPUT lv-full-qty).

  FIND CURRENT loadtag NO-LOCK NO-ERROR.
  FIND CURRENT fg-rctd NO-LOCK NO-ERROR.


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
  DEF VAR iPalletID AS INT NO-UNDO.
  DEF VAR iStartPalletID AS INT NO-UNDO.
  DEF VAR iEndPalletID AS INT NO-UNDO.
  DEF VAR cTotalUnit AS CHAR NO-UNDO.
  DEF VAR cRFIDTag AS CHAR NO-UNDO.
  DEF VAR vError AS LOG NO-UNDO.
  DEF VAR liTagCounter AS INT NO-UNDO.
  DEF VAR cTmpFile AS CHAR NO-UNDO.

  cTmpFile = SESSION:TEMP-DIRECTORY + "/" + USERID("NOSWEAT") + STRING(TIME).
  FIND FIRST w-ord NO-ERROR.
  DEF BUFFER bf-cust FOR cust.

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

        EACH-ORD:
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
      /* Output to temporary file first, then rename it at end to make sure */
      /* it is not picked up before it is complete */
      OUTPUT TO VALUE(cTmpFile).
      IF cBarCodeProgram NE "Loftware" THEN DO: 
        PUT UNFORMATTED
          "CUSTOMER,ORDNUMBER,JOBNUMBER,ITEM,CUSTPARTNO,CUSTPONO,PCS,BUNDLE,TOTAL,"
          "SHIPCODE,SHIPNAME,SHIPADD1,SHIPADD2,SHIPCITY,SHIPSTATE,SHIPCOUNTRY,SHIPZIP,"
          "SOLDCODE,SOLDNAME,SOLDADD1,SOLDADD2,SOLDCITY,SOLDSTATE,SOLDCOUNTRY,SOLDZIP,"
          "INAME,DUEDATE,RELDATE,UPCNO,LENGTH,WIDTH,DEPTH,FLUTE,TEST,VENDOR,GROSSWGT,"
          "TAREWGT,NETWGT,SHEETWGT,UOM,STYLE,STYLEDESC,RELLOTNO,MIDDLESEXJOBNUMBER,MIDDLESEXCUSTPONO,"
          "TAG#,PARTIAL,CASECODE,SN1,SN2,SN3,SN4,SN5,SN6,SN7,SN8,PONO,DN1,DN2,DN3,DN4,"
          "DN5,DN6,DN7,DN8,DN9,DN10,EST#,ORDDESC1,ORDDESC2".
      IF CAN-DO("ASI,SSLABEL",v-loadtag) THEN
         PUT UNFORMATTED ",COUNTER#,RFIDTag".

      PUT UNFORMATTED ",DUEDATEJOBLINE,DUEDATEJOB,LINE#,UnitWt,PalletWt,FGdesc1,FGdesc2,FGdesc3,FG Lot#,"
                       "PalletCode,PalletID,TagCounter,TagCountTotal,"
                       "RN1,RN2,RN3,RN4,WareHouse,Bin,JobQty".

      /* rstark - */
      IF lSSCC THEN PUT UNFORMATTED ",SSCC".

      PUT SKIP.
      END.
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
               /*   w-ord.cust-part-no = itemfg.part-no */ .

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

           /* gdm - 101610905 */
           ASSIGN v-fgdsc1 = itemfg.part-dscr1
                  v-fgdsc2 = itemfg.part-dscr2
                  v-fgdsc3 = itemfg.part-dscr3.

        END.  /* avail itemfg */

        IF tb_dept-note THEN DO:
           lv-text = "".
           FOR EACH tt-formtext:
               DELETE tt-formtext.
           END.

           IF w-ord.ord-no NE 0 THEN DO:
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
                    IF notes.note_form_no = 0 OR notes.note_form_no = w-ord.form-no THEN
                        lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
                 END.
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
          lv-how-many-tags =  IF CAN-DO("SSLABEL,CentBox",v-loadtag) OR w-ord.total-tags = 1 THEN w-ord.total-tags
                              ELSE (w-ord.total-tags - 1).
          FIND bf-cust WHERE bf-cust.company = cocode
                         AND bf-cust.cust-no EQ w-ord.cust-no
                       NO-LOCK NO-ERROR.

           RUN incrementPalletID (BUFFER bf-cust, lv-how-many-tags * w-ord.mult,
                                  OUTPUT iStartPalletID, OUTPUT iEndPalletID).
           IF iEndPalletID EQ -1 THEN DO:
              RUN askNextPalletID (INPUT w-ord.cust-no, OUTPUT vError).
              RETURN.
           END.

          iPalletId = iStartPalletID.
          DO i = 1 TO (lv-how-many-tags * w-ord.mult):
             /* loadtags generation */
             IF i MOD w-ord.mult = 1 OR i = 1 OR w-ord.mult = 1  THEN DO:
                 liTagCounter = liTagCounter + 1.
                IF i = 1 THEN lv-tag-no = i.
                /*  create-loadtag may prompt, so need to close default stream */
                OUTPUT CLOSE.
                RUN create-loadtag (INPUT-OUTPUT lv-tag-no, w-ord.total-unit) NO-ERROR.
                OUTPUT TO VALUE(cTmpFile) APPEND.

             END.

             IF CAN-DO("ASI,SSLABEL",v-loadtag) THEN DO:

                FIND FIRST rfidtag OF loadtag NO-LOCK NO-ERROR.
                cRFIDTag = IF AVAIL rfidtag THEN rfidtag.rfidtag ELSE "".

             END.
             cTotalUnit = string(w-ord.total-unit, ">>>>>>>9").
             RUN write-loadtag-line (INPUT cRFIDTag, INPUT cTotalUnit, INPUT iPalletID, INPUT liTagCounter).
             iPalletID = iPalletID + 1.
          end. /* DO i = 1 TO (lv-how-many-tags * w-ord.mult): */

          IF NOT CAN-DO("SSLABEL,CentBox",v-loadtag) THEN DO:
              RUN incrementPalletID (BUFFER bf-cust, w-ord.mult,
                       OUTPUT iStartPalletID, OUTPUT iEndPalletID).
              IF iEndPalletID EQ -1 THEN DO:
                   RUN askNextPalletID (INPUT w-ord.cust-no, OUTPUT vError).
                   RETURN.
              END.


              iPalletId = iStartPalletID.
              do v-count = 1 to w-ord.mult: /* for partial print */
                    /* loadtags generation */
                 IF v-count EQ 1 THEN DO:
                     liTagCounter = liTagCounter + 1.

                     /* Create-loadtag may prompt, so need to close default stream */
                     OUTPUT CLOSE.
                     RUN create-loadtag (INPUT-OUTPUT lv-tag-no, 0) NO-ERROR.
                     OUTPUT TO VALUE(cTmpFile) APPEND.

                 END.
                 cTotalUnit = "".
                 RUN write-loadtag-line (INPUT cRFIDTag, cTotalUnit, INPUT iPalletID, INPUT liTagCounter).
                 iPalletID = iPalletID + 1.
              end.
          END. /*not SSLABEL, Centbox*/

        end. /*w-ord.total-tags > 0*/
        delete w-ord.
      end.


      output close.
      IF SEARCH(v-out) NE ? THEN
          OS-DELETE VALUE(v-out).
      /* Rename to expected file name / location */
      IF cBarCodeProgram EQ ""  OR cBarCodeProgram EQ 'Loftware' THEN DO:
          OS-RENAME VALUE(cTmpFile) VALUE(v-out).
      END.
      ELSE DO:
        IF SEARCH(cTmpFile) NE ? THEN
        OS-DELETE VALUE(cTmpFile).
      END.


    end.    /* NOT TRIAD */
    IF ssPostFG-log AND SSPostFG-char = "Loadtag"  THEN
      RUN post-all.
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

   IF AVAIL oe-ord THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = loadtag.company
                           AND oe-ordl.ord-no = loadtag.ord-no
                           AND oe-ordl.i-no = loadtag.i-no NO-LOCK NO-ERROR.
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
            w-ord.cust-no      = IF AVAIL oe-ord THEN oe-ord.cust-no ELSE ""
            w-ord.cust-name    = IF AVAIL oe-ord THEN oe-ord.cust-name ELSE ""
            w-ord.i-no         = loadtag.i-no
            w-ord.cust-part-no = IF AVAIL oe-ordl THEN oe-ordl.part-no ELSE ""
            w-ord.ord-qty      = loadtag.qty
            w-ord.po-no        = IF AVAIL oe-ordl THEN oe-ordl.po-no-po ELSE 0
            w-ord.i-name       = loadtag.i-name
            w-ord.due-date     = if oe-ord.due-date ne ? then
                                   oe-ord.due-date
                                 else
                                 IF AVAIL oe-ordl THEN (if oe-ordl.req-date ne ? then
                                   oe-ordl.req-date
                                 else today) ELSE oe-ord.due-date
            w-ord.est-no       = IF AVAIL oe-ordl THEN oe-ordl.est-no ELSE ""
            w-ord.form-no      = IF AVAIL oe-ordl THEN oe-ordl.form-no ELSE 0
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = IF AVAIL cust AND cust.int-field[1] ne 0 AND NOT glOverrideMult then
                                   cust.int-field[1] else v-mult
            w-ord.dont-run-set = IF AVAIL oe-ordl THEN oe-ordl.is-a-component ELSE NO
            w-ord.ord-desc1    = IF AVAIL oe-ordl THEN oe-ordl.part-dscr1 ELSE ""
            w-ord.ord-desc2    = IF AVAIL oe-ordl THEN oe-ordl.part-dscr2 ELSE ""
            w-ord.sold-code    = oe-ord.sold-id
            w-ord.sold-name    = oe-ord.sold-name
            w-ord.sold-add1    = oe-ord.sold-add[1]
            w-ord.sold-add2    = oe-ord.sold-add[2]
            w-ord.sold-city    = oe-ord.sold-city
            w-ord.sold-state   = oe-ord.sold-state
            w-ord.sold-zip     = oe-ord.sold-zip
            w-ord.linenum      = IF AVAIL oe-ordl THEN oe-ordl.e-num ELSE 0
            w-ord.lot          = loadtag.misc-char[2].

      IF AVAIL b-job-hdr THEN
         w-ord.due-date-jobhdr = IF b-job-hdr.due-date <> ? THEN STRING(b-job-hdr.due-date, "99/99/9999") ELSE "".
      IF AVAIL b-job THEN
         w-ord.due-date-job = IF b-job.due-date <> ? THEN STRING(b-job.due-date, "99/99/9999") ELSE "".
        w-ord.job-qty = IF AVAIL b-job AND AVAIL b-job-hdr THEN b-job-hdr.qty ELSE 0 . 

      RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                        OUTPUT w-ord.rel-date,
                        OUTPUT w-ord.rel-lot#,
                        OUTPUT w-ord.ship-notes,
                        INPUT ROWID(b-job-hdr)).
      IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.

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
      IF v-ship-id EQ "" THEN v-ship-id = oe-ord.cust-no.
      FIND FIRST shipto WHERE shipto.company eq cocode
            AND shipto.cust-no eq oe-ord.cust-no
            AND shipto.ship-id eq v-ship-id
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
             w-ord.cas-no = eb.cas-no
             w-ord.pallt-no = eb.tr-no
             w-ord.part-dscr2 = eb.part-dscr2.

          ASSIGN w-ord.total-tags = 1
            w-ord.ord-qty = loadtag.qty 
            w-ord.pcs = loadtag.qty-case
            w-ord.bundle = loadtag.case-bundle
            w-ord.partial =loadtag.partial
            w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial .    

            IF AVAIL b-job AND AVAIL eb AND eb.est-type EQ 2 AND AVAIL b-job-hdr THEN
             w-ord.job-qty =  b-job-hdr.qty * (IF eb.cust-% GT 0 THEN eb.cust-% ELSE 1)  .
            ELSE IF AVAIL b-job AND AVAIL eb AND eb.est-type EQ 6 AND AVAIL b-job-hdr THEN
             w-ord.job-qty =  b-job-hdr.qty * (IF eb.quantityPerSet GT 0 THEN eb.quantityPerSet ELSE 1)  .

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
      IF NOT AVAIL job-hdr AND tb_reprint-tag THEN DO:
          FIND FIRST job-hdr WHERE job-hdr.company EQ job.company
                          AND job-hdr.job     EQ job.job
                          AND job-hdr.job-no  EQ job.job-no
                          AND job-hdr.job-no2 EQ job.job-no2
                          NO-LOCK NO-ERROR.
      END.
      IF AVAIL job-hdr THEN DO:

         FIND FIRST cust WHERE cust.company eq cocode
                          AND cust.cust-no eq job-hdr.cust-no NO-LOCK NO-ERROR.
         FIND FIRST itemfg WHERE itemfg.company eq cocode
                            AND itemfg.i-no    eq loadtag.i-no NO-LOCK NO-ERROR.

         CREATE w-ord.
         ASSIGN
            w-ord.ord-no       = job-hdr.ord-no
            w-ord.job-no       = job-hdr.job-no
            w-ord.job-no2      = job-hdr.job-no2
            w-ord.cust-no      = IF AVAIL cust THEN cust.cust-no ELSE ""
            w-ord.cust-name    = IF AVAIL cust THEN cust.NAME ELSE ""
            w-ord.i-no         = loadtag.i-no
            w-ord.ord-qty      = job-hdr.qty
            w-ord.due-date     = job.start-date
            w-ord.est-no       = job.est-no
            w-ord.form-no      = job-hdr.frm
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = IF AVAIL cust AND cust.int-field[1] ne 0 AND NOT glOverrideMult THEN
                                   cust.int-field[1] else v-mult
            w-ord.lot          = loadtag.misc-char[2].
            w-ord.job-qty      = job-hdr.qty   .

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

             FOR EACH cust-part NO-LOCK 
             WHERE cust-part.company EQ job-hdr.company   
               AND cust-part.i-no EQ loadtag.i-no 
               AND cust-part.cust-no EQ job-hdr.cust-no
               AND cust-part.part-no NE "" :
             ASSIGN  w-ord.cust-part-no = cust-part.part-no .
             LEAVE.
             END.

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
          IF v-ship-id EQ "" THEN v-ship-id = job-hdr.cust-no.
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
             w-ord.cas-no     = eb.cas-no
             w-ord.pallt-no   = eb.tr-no
             w-ord.part-dscr2 = eb.part-dscr2.

          ASSIGN w-ord.total-tags = 1
            w-ord.ord-qty = loadtag.qty 
            w-ord.pcs = loadtag.qty-case
            w-ord.bundle = loadtag.case-bundle
            w-ord.partial =loadtag.partial
            w-ord.total-unit = w-ord.pcs * w-ord.bundle  .   

            IF AVAIL eb AND eb.est-type EQ 2 THEN
             w-ord.job-qty =  job-hdr.qty * (IF eb.cust-% GT 0 THEN eb.cust-% ELSE 1)  .
            ELSE IF AVAIL eb AND eb.est-type EQ 6 THEN
             w-ord.job-qty =  job-hdr.qty * (IF eb.quantityPerSet GT 0 THEN eb.quantityPerSet ELSE 1)  .


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
                w-ord.style = itemfg.style
                w-ord.cust-part-no = itemfg.part-no .
         IF po-ordl.ord-no > 0 THEN  
             FOR EACH cust-part NO-LOCK 
                 WHERE cust-part.company EQ po-ord.company   
                   AND cust-part.i-no EQ po-ordl.i-no 
                   AND cust-part.cust-no EQ po-ord.cust-no
                   AND cust-part.part-no NE "" :
                 ASSIGN  w-ord.cust-part-no = cust-part.part-no .
                 LEAVE.
             END.
          

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
                    w-ord.cas-no = eb.cas-no
                    w-ord.pallt-no = eb.tr-no
                    w-ord.part-dscr2 = eb.part-dscr2.
         IF v-ship-id EQ "" THEN v-ship-id = po-ord.cust-no.
         FIND FIRST shipto NO-LOCK WHERE shipto.company EQ cocode
                                  AND shipto.cust-no EQ po-ord.cust-no
                                  AND shipto.ship-id EQ v-ship-id
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

          FOR EACH cust-part NO-LOCK 
             WHERE cust-part.company EQ cocode   
               AND cust-part.i-no EQ itemfg.i-no 
               AND cust-part.cust-no EQ itemfg.cust-no
               AND cust-part.part-no NE "" :
             ASSIGN  w-ord.cust-part-no = cust-part.part-no .
             LEAVE.
             END.

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
   IF tb_reprint-tag THEN DO:
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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateWOrdFromItem C-Win 
PROCEDURE CreateWOrdFromItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipBeginItem AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEndItem AS CHARACTER NO-UNDO.

  FOR EACH itemfg NO-LOCK WHERE itemfg.company EQ cocode
                            AND itemfg.i-no GE ipBeginItem
                            AND itemfg.i-no LE ipEndItem:
    FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                              AND vend.vend-no EQ itemfg.vend-no NO-ERROR.
    FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                              AND cust.cust-no EQ oe-ord.cust-no NO-ERROR.

    CREATE w-ord.
    ASSIGN
      w-ord.i-no = itemfg.i-no
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
      w-ord.style   = itemfg.style.
     
      FOR EACH cust-part NO-LOCK 
          WHERE cust-part.company EQ cocode   
          AND cust-part.i-no EQ itemfg.i-no 
          AND cust-part.cust-no EQ itemfg.cust-no
          AND cust-part.part-no NE "" :
          ASSIGN  w-ord.cust-part-no = cust-part.part-no .
          LEAVE.
      END.

   /* task 02081202 */
   IF tb_reprint-tag THEN DO:
      FIND FIRST fg-bin WHERE fg-bin.company = itemfg.company
                          AND fg-bin.i-no = w-ord.i-no
                          AND fg-bin.tag = fi_cas-lab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                          AND fg-bin.qty > 0 NO-LOCK NO-ERROR.
      IF AVAIL fg-bin AND AVAIL w-ord THEN
         ASSIGN w-ord.pcs = fg-bin.case-count
                w-ord.bundle = /*fg-bin.cases-unit*/ TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                w-ord.partial = fg-bin.partial-count
                w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial .      
   END.

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
  END. /* each itemfg */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dispJobInfo C-Win 
PROCEDURE dispJobInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------ ------------------------------------------------*/
DEF INPUT PARAMETER ipcCompany    AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER ipcJobNo  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiJobNo2 AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipiForm AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipiBlank AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER iplCheckBar AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER iplCheckBarBlank AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER oplCheckBar AS LOGICAL     NO-UNDO.

DEF BUFFER bf-job FOR job.
DEF BUFFER bf-job-hdr-2 FOR job-hdr.
DEF VAR v-lncnt AS INT NO-UNDO.
DEF VAR v-frstitem AS CHAR NO-UNDO.
DEF VAR v-lastitem AS CHAR NO-UNDO.
DEF VAR v-first-order AS INT NO-UNDO.
DEF VAR v-last-order AS INT NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:

   FIND FIRST bf-job WHERE
        bf-job.company EQ cocode AND
        bf-job.job-no EQ ipcJobNo AND
        bf-job.job-no2 EQ ipiJobNo2
        NO-LOCK NO-ERROR.

   IF AVAIL bf-job THEN
   DO:
      FOR EACH bf-job-hdr-2 FIELDS(i-no frm blank-no) NO-LOCK
          WHERE bf-job-hdr-2.company EQ bf-job.company
            AND bf-job-hdr-2.job-no  EQ bf-job.job-no
            AND bf-job-hdr-2.job-no2 EQ bf-job.job-no2
            AND ( bf-job-hdr-2.frm EQ ipiForm OR NOT iplCheckBar )
            AND ( bf-job-hdr-2.blank-no EQ ipiBlank OR NOT iplCheckBarBlank )
           BREAK BY bf-job-hdr-2.i-no:

           v-lncnt = v-lncnt + 1.

           IF FIRST-OF(bf-job-hdr-2.i-no) THEN
              v-frstitem = bf-job-hdr-2.i-no.
           IF LAST-OF(bf-job-hdr-2.i-no) THEN
              v-lastitem = bf-job-hdr-2.i-no.
      END.

      FOR EACH bf-job-hdr-2 FIELDS(ord-no frm blank-no) NO-LOCK
          WHERE bf-job-hdr-2.company EQ bf-job.company
            AND bf-job-hdr-2.job-no  EQ bf-job.job-no
            AND bf-job-hdr-2.job-no2 EQ bf-job.job-no2
            AND ( bf-job-hdr-2.frm EQ ipiForm OR NOT iplCheckBar )
            AND ( bf-job-hdr-2.blank-no EQ ipiBlank OR NOT iplCheckBarBlank )
           BREAK BY bf-job-hdr-2.ord-no:

           IF FIRST-OF(bf-job-hdr-2.ord-no) THEN
              v-first-order = bf-job-hdr-2.ord-no.
           IF LAST-OF(bf-job-hdr-2.ord-no) THEN
              v-last-order = bf-job-hdr-2.ord-no.
      END.

      ASSIGN
         begin_ord-no:SCREEN-VALUE = STRING(v-first-order)
         begin_job:SCREEN-VALUE    = ipcJobNo         
         begin_job2:SCREEN-VALUE   = STRING(ipiJobNo2,"99")
         end_ord-no:SCREEN-VALUE   = STRING(v-last-order)
         end_job:SCREEN-VALUE      = ipcJobNo     
         end_job2:SCREEN-VALUE     = STRING(ipiJobNo2,"99")
         begin_i-no:SCREEN-VALUE = v-frstitem
         end_i-no:SCREEN-VALUE   = v-lastitem.           

      APPLY "LEAVE" TO end_i-no.
      IF v-lncnt EQ 1 THEN
          oplCheckBar = YES . 
      IF v-lncnt GT 1 THEN
         MESSAGE "There are multiple FG Items on this order." skip
                 "Please select an FG Item."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
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
  DISPLAY tbPartSelect loadtagFunction tb_ret tb_reprint-tag v-ord-list 
          v-job-list begin_ord-no end_ord-no begin_job begin_job2 end_job 
          end_job2 begin_i-no end_i-no rd_order-sts rd_print begin_date end_date 
          rd_comps v-dept-list tb_dept-note tb_rel tb_over tb_16ths tb_ship-id 
          v-ship-id scr-auto-print scr-freeze-label scr-label-file begin_labels 
          begin_form begin_filename typeLabel statusLabel lbl_po-no tb_xfer-lot 
          tb_override-mult begin_ship-to end_ship-to tb_close tb_print-view 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE tbPartSelect loadtagFunction tb_ret tb_reprint-tag v-ord-list 
         v-job-list begin_ord-no end_ord-no begin_job begin_job2 end_job 
         end_job2 begin_i-no end_i-no rd_order-sts rd_print begin_date end_date 
         rd_comps tb_dept-note tb_rel tb_over tb_16ths tb_ship-id 
         scr-auto-print scr-freeze-label scr-label-file begin_labels begin_form 
         btn-ok btn-cancel tb_xfer-lot tb_override-mult begin_ship-to 
         end_ship-to tb_close tb_print-view RECT-7 RECT-8 RECT-11 RECT-12 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE final-update C-Win 
PROCEDURE final-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH w-ord:
    FIND FIRST cust
        WHERE cust.company eq cocode
          AND cust.cust-no eq w-ord.cust-no
        NO-LOCK NO-ERROR.

    IF v-cas-lab THEN DO:
      FIND FIRST loadtag
          WHERE loadtag.company     EQ cocode
            AND loadtag.tag-no      EQ fi_cas-lab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
            AND loadtag.item-type   EQ NO
            AND loadtag.is-case-tag EQ YES
          NO-LOCK NO-ERROR.
      IF AVAIL loadtag AND loadtag.tag-no NE "" THEN
        ASSIGN
         w-ord.pcs        = loadtag.qty-case
         w-ord.bundle     = loadtag.case-bundle
         w-ord.total-unit = w-ord.pcs * w-ord.bundle
         w-ord.lot        = loadtag.misc-char[2].
    END.

    IF v-tags EQ 0 THEN
       w-ord.total-tags = 1.
    ELSE
    IF v-tags EQ ? THEN w-ord.total-tags = 0.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE from-job C-Win 
PROCEDURE from-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-warning AS LOG NO-UNDO.

  DEF BUFFER b-job-hdr-2 FOR job-hdr.

  DEF VAR lv-rel-date AS DATE NO-UNDO.
  DEF VAR lv-tt-created AS LOG NO-UNDO.

  FIND FIRST job NO-LOCK
      WHERE ROWID(job) EQ ip-rowid
        AND (v-stat EQ "A"                      OR
             (v-stat EQ "C" AND NOT job.opened) OR
             (v-stat EQ "O" AND job.opened))
      NO-ERROR.

  IF NOT AVAILABLE job THEN RETURN.
  IF (v-ord-list NE '' OR begin_ord-no NE 0 OR end_ord-no NE 0) AND
     NOT CAN-FIND(ttblJob WHERE ttblJob.company EQ job.company
                            AND ttblJob.job-no EQ job.job-no
                            AND ttblJob.job-no2 EQ job.job-no2) THEN DO:

    FOR EACH b-job-hdr-2 FIELDS(company job-no job-no2 ord-no) WHERE
        b-job-hdr-2.company EQ job.company AND
        b-job-hdr-2.job     EQ job.job AND
        b-job-hdr-2.job-no  EQ job.job-no AND
        b-job-hdr-2.job-no2 EQ job.job-no2 AND
        b-job-hdr-2.i-no    GE v-fitem[1] AND
        b-job-hdr-2.i-no    LE v-fitem[2]
        NO-LOCK:

       IF NOT CAN-FIND(FIRST ttblJob WHERE
          ttblJob.company EQ b-job-hdr-2.company AND
          ttblJob.job-no EQ b-job-hdr-2.job-no AND
          ttblJob.job-no2 EQ b-job-hdr-2.job-no2 AND
          ttblJob.ord-no  EQ b-job-hdr-2.ord-no) THEN
          DO:
             CREATE ttblJob.
             ASSIGN
                ttblJob.company = b-job-hdr-2.company
                ttblJob.job-no = b-job-hdr-2.job-no
                ttblJob.job-no2 = b-job-hdr-2.job-no2
                ttblJob.ord-no  = b-job-hdr-2.ord-no
                lv-tt-created = YES.
             RELEASE ttblJob.
          END.
    END.

    IF lv-tt-created THEN
       RETURN.
  END.

    IF AVAIL job THEN
    FOR EACH job-hdr
        WHERE job-hdr.company EQ job.company
          AND job-hdr.job     EQ job.job
          AND job-hdr.job-no  EQ job.job-no
          AND job-hdr.job-no2 EQ job.job-no2
          AND job-hdr.i-no    GE v-fitem[1]
          AND job-hdr.i-no    LE v-fitem[2]
         /*AND job-hdr.ord-no  EQ 0
        USE-INDEX ord-no*/
        /*ESP - Task 04180703 don't look at order number */

        NO-LOCK,
        FIRST cust
        WHERE cust.company eq cocode
          AND cust.cust-no eq job-hdr.cust-no
        NO-LOCK,
        FIRST itemfg
        WHERE itemfg.company eq cocode
          AND itemfg.i-no    eq job-hdr.i-no
        NO-LOCK:

          CREATE w-ord.
          ASSIGN
            w-ord.ord-no       = job-hdr.ord-no
            w-ord.job-no       = job-hdr.job-no
            w-ord.job-no2      = job-hdr.job-no2
            w-ord.cust-no      = cust.cust-no
            w-ord.cust-name    = cust.name
            w-ord.i-no         = job-hdr.i-no
            w-ord.cust-part-no = itemfg.part-no
            w-ord.over-pct     = IF tb_over THEN cust.over-pct ELSE 0
            w-ord.qty-before   = job-hdr.qty
            w-ord.ord-qty      = w-ord.qty-before *
                                 (1 + (w-ord.over-pct / 100))
            w-ord.i-name       = itemfg.i-name
            w-ord.upc-no       = itemfg.upc-no
            w-ord.due-date     = job.start-date
            w-ord.est-no       = job.est-no
            w-ord.form-no      = job-hdr.frm
            w-ord.upc-no       = itemfg.upc-no
            w-ord.box-len      = itemfg.l-score[50]
            w-ord.box-wid      = itemfg.w-score[50]
            w-ord.box-dep      = itemfg.d-score[50]
            w-ord.style        = itemfg.style
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 AND NOT glOverrideMult then
                                   cust.int-field[1] else v-mult
            num-rec            = num-rec + 1
            w-ord.due-date-job = IF job.due-date <> ? THEN STRING(job.due-date, "99/99/9999") ELSE "".
            w-ord.due-date-jobhdr = IF job-hdr.due-date <> ? THEN STRING(job-hdr.due-date, "99/99/9999") ELSE "".
            w-ord.job-qty      = job-hdr.qty  .
            FOR EACH cust-part NO-LOCK 
             WHERE cust-part.company EQ cocode   
               AND cust-part.i-no EQ itemfg.i-no 
               AND cust-part.cust-no EQ cust.cust-no
               AND cust-part.part-no NE "" :
             ASSIGN  w-ord.cust-part-no = cust-part.part-no .
             LEAVE.
             END.


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

          IF job-hdr.ord-no NE 0 THEN
          DO:
             FIND FIRST oe-ordl WHERE
                  oe-ordl.company EQ cocode AND
                  oe-ordl.ord-no  EQ job-hdr.ord-no AND
                  oe-ordl.i-no    EQ job-hdr.i-no
                  NO-LOCK NO-ERROR.

             IF AVAIL oe-ordl THEN
             DO:
                FIND FIRST oe-ord WHERE
                     oe-ord.company EQ cocode AND
                     oe-ord.ord-no  EQ job-hdr.ord-no
                     NO-LOCK NO-ERROR.

                RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                                  OUTPUT w-ord.rel-date,
                                  OUTPUT w-ord.rel-lot#,
                                  OUTPUT w-ord.ship-notes,
                                  INPUT ROWID(job-hdr)).
                IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.

                ASSIGN
                 w-ord.ord-desc1    = oe-ordl.part-dscr1
                 w-ord.ord-desc2    = oe-ordl.part-dscr2.

                RELEASE oe-ordl.
                RELEASE oe-ord.
             END.
          END.
          ELSE DO:
           op-warning = YES.
           IF v-po-no-source = "J"  THEN
               RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                                 OUTPUT w-ord.rel-date,
                                 OUTPUT w-ord.rel-lot#,
                                 OUTPUT w-ord.ship-notes,
                                 INPUT ROWID(job-hdr)).
          END.

          IF NOT tb_ship-id THEN v-ship-id = job-hdr.cust-no.
          FOR EACH shipto
              WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ job-hdr.cust-no
              USE-INDEX ship-id NO-LOCK
              BREAK BY shipto.ship-no DESC:
            IF shipto.ship-id EQ v-ship-id OR
               LAST(shipto.ship-no)              THEN DO:
              ASSIGN
               w-ord.ship-code  = shipto.ship-id
               w-ord.ship-name  = shipto.ship-name
               w-ord.ship-add1  = shipto.ship-add[1]
               w-ord.ship-add2  = shipto.ship-add[2]
               w-ord.ship-city  = shipto.ship-city
               w-ord.ship-state = shipto.ship-state
               w-ord.ship-ctry  = shipto.country
               w-ord.ship-zip   = shipto.ship-zip.
              LEAVE.
            END.
          END.

          FIND FIRST est
              WHERE est.company eq job.company
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
             w-ord.cas-no     = eb.cas-no
             w-ord.pallt-no   = eb.tr-no
             w-ord.part-dscr2 = eb.part-dscr2.

             IF AVAIL eb AND eb.est-type EQ 2 THEN
               w-ord.job-qty =  job-hdr.qty * (IF eb.cust-% GT 0 THEN eb.cust-% ELSE 1)  .
             ELSE IF  AVAIL eb AND eb.est-type EQ 6  THEN
               w-ord.job-qty =  job-hdr.qty * (IF eb.quantityPerSet GT 0 THEN eb.quantityPerSet ELSE 1)  .


          IF NOT v-oecount THEN
            ASSIGN
             w-ord.pcs    = w-ord.total-unit
             w-ord.bundle = 1.

          /* Add .49 to round up and add 1 for extra tag   */
          w-ord.total-tags = ((w-ord.job-qty / w-ord.total-unit) + .49) +  IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE from-ord C-Win 
PROCEDURE from-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF VAR lv-got-shipto AS LOG NO-UNDO.
  DEF VAR lv-stat AS cha NO-UNDO.
  DEF VAR lv-over LIKE oe-ordl.over-pct NO-UNDO.
  DEF VAR lv-rel-date AS DATE NO-UNDO.
  DEF VAR lv-job-no2 LIKE job-hdr.job-no2 NO-UNDO.
  DEF VAR lv-job-no LIKE job.job-no NO-UNDO.
  DEF BUFFER b-job-hdr FOR job-hdr. /* rtc */
  DEF BUFFER b-job FOR job.         /* rtc */

  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER b-loadtag FOR loadtag. 

    FIND FIRST oe-ord
        WHERE ROWID(oe-ord) EQ ip-rowid
          AND (v-stat EQ "A"                                    OR
               (v-stat EQ "C" AND INDEX("CZ",oe-ord.stat) GT 0) OR
               (v-stat EQ "O" AND INDEX("CZ",oe-ord.stat) EQ 0))
        NO-LOCK NO-ERROR.

    IF AVAIL oe-ord THEN
    FIND FIRST cust
        WHERE cust.company eq cocode
          AND cust.cust-no eq oe-ord.cust-no
        NO-LOCK NO-ERROR.

    IF AVAIL oe-ord THEN
    FIND FIRST soldto NO-LOCK
        WHERE soldto.company EQ cocode
          AND soldto.cust-no EQ oe-ord.cust-no
          AND soldto.sold-id EQ oe-ord.sold-id
        USE-INDEX sold-id NO-ERROR. 

    IF AVAIL cust THEN
    FOR EACH oe-ordl
        WHERE oe-ordl.company eq oe-ord.company
          AND oe-ordl.ord-no  eq oe-ord.ord-no

          AND (oe-ordl.i-no    ge v-fitem[1]
          AND oe-ordl.i-no    le v-fitem[2]        
          AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
          AND (NOT CAN-FIND(FIRST ttblJob)
               OR (oe-ordl.job-no NE "" AND CAN-FIND(FIRST ttblJob WHERE ttblJob.company EQ oe-ordl.company
                                     AND ttblJob.job-no EQ oe-ordl.job-no
                                     AND ttblJob.job-no2 EQ oe-ordl.job-no2))
               OR (oe-ordl.job-no EQ "" AND
                   CAN-FIND(FIRST ttblJob WHERE ttblJob.company EQ oe-ordl.company
                                     AND ttblJob.ord-no EQ oe-ordl.ord-no)))
               OR CAN-FIND(FIRST tt-comps WHERE tt-comps.comp EQ oe-ordl.i-no))
        use-index ord-no NO-LOCK BREAK BY oe-ordl.i-no:
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq oe-ordl.i-no
          no-lock no-error.

      ASSIGN lv-job-no2 = 0 lv-job-no = "".
      FIND FIRST ttbljob WHERE ttbljob.company = cocode
                           AND ttbljob.ord-no = oe-ordl.ord-no
                           AND ttbljob.job-no = oe-ordl.job-no
                           AND ttbljob.job-no2 = oe-ordl.job-no2
                         NO-LOCK NO-ERROR.
      IF NOT AVAIL ttbljob THEN
        FIND FIRST ttbljob WHERE ttbljob.company = cocode
                           AND ttbljob.ord-no = oe-ordl.ord-no
                         NO-LOCK NO-ERROR.
      IF AVAIL ttbljob THEN
          FIND FIRST b-job-hdr WHERE b-job-hdr.company = cocode
                                   AND b-job-hdr.ord-no  = oe-ordl.ord-no
                                   AND b-job-hdr.job-no = ttbljob.job-no
                                   AND b-job-hdr.job-no2 = ttbljob.job-no2
                                   AND b-job-hdr.i-no    = oe-ordl.i-no NO-LOCK NO-ERROR.
      ELSE
        FIND FIRST b-job-hdr WHERE b-job-hdr.company = cocode 
                               AND b-job-hdr.ord-no  = oe-ordl.ord-no  
                               AND b-job-hdr.i-no    = oe-ordl.i-no NO-LOCK NO-ERROR.
      IF AVAIL b-job-hdr THEN
         FIND FIRST b-job WHERE b-job.company = b-job-hdr.company
                            AND b-job.job     = b-job-hdr.job
                            AND b-job.job-no  = b-job-hdr.job-no
                            AND b-job.job-no2 = b-job-hdr.job-no2 NO-LOCK NO-ERROR.

      IF lv-job-no = "" THEN DO:
        IF AVAIL b-job-hdr then
           FIND FIRST ttbljob WHERE ttbljob.company = b-job-hdr.company
                                AND ttbljob.job-no = b-job-hdr.job-no
                                AND ttbljob.job-no2 = b-job-hdr.job-no2
                                AND ttbljob.ord-no = oe-ordl.ord-no
                                NO-LOCK NO-ERROR.
        ELSE FIND FIRST ttblJob WHERE ttblJob.company EQ oe-ordl.company
                             AND ttblJob.ord-no EQ oe-ordl.ord-no
                        NO-LOCK NO-ERROR.

        IF AVAIL ttblJob THEN
            ASSIGN lv-job-no  = ttblJob.job-no
                   lv-job-no2 = ttblJob.job-no2.
        ELSE
            IF AVAIL b-job-hdr THEN
                ASSIGN lv-job-no = b-job-hdr.job-no
                       lv-job-no2 = b-job-hdr.job-no2.
      END.
      IF lv-job-no = "" THEN lv-job-no = oe-ordl.job-no.
      IF lv-job-no2 = 0 THEN lv-job-no2 = oe-ordl.job-no2.
      IF lv-job-no = "" THEN lv-job-no = oe-ord.job-no.
      IF lv-job-no2 = 0 THEN lv-job-no2 = oe-ord.job-no2.
      IF oe-ordl.est-no NE "" THEN
      FIND FIRST eb
          WHERE eb.company  EQ oe-ordl.company
            AND eb.est-no   EQ oe-ordl.est-no
            AND eb.stock-no EQ oe-ordl.i-no
          NO-LOCK NO-ERROR.

      lv-over = IF tb_over THEN oe-ordl.over-pct ELSE 0.

      IF NOT by-release OR NOT AVAIL oe-ordl THEN
      DO:
        IF FIRST-OF(oe-ordl.i-no) THEN
        DO:
          CREATE w-ord.
          ASSIGN
            w-ord.ord-no       = oe-ord.ord-no
            w-ord.job-no       = lv-job-no
            w-ord.job-no2      = lv-job-no2
            w-ord.cust-no      = oe-ord.cust-no
            w-ord.cust-name    = oe-ord.cust-name
            w-ord.i-no         = oe-ordl.i-no
            w-ord.cust-part-no = oe-ordl.part-no
            w-ord.over-pct     = lv-over
            w-ord.qty-before   = oe-ordl.qty
            w-ord.ord-qty      = w-ord.qty-before *
                                 (1 + (w-ord.over-pct / 100))
            w-ord.po-no        = oe-ordl.po-no-po
            w-ord.sold-code    = oe-ord.sold-id
            w-ord.sold-name    = oe-ord.sold-name
            w-ord.sold-add1    = oe-ord.sold-add[1]
            w-ord.sold-add2    = oe-ord.sold-add[2]
            w-ord.sold-city    = oe-ord.sold-city
            w-ord.sold-state   = oe-ord.sold-state
            w-ord.sold-zip     = oe-ord.sold-zip
            w-ord.i-name       = oe-ordl.i-name
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
            w-ord.mult         = IF AVAILABLE cust AND cust.int-field[1] ne 0 AND NOT glOverrideMult then
                                   cust.int-field[1] else v-mult
            w-ord.dont-run-set = oe-ordl.is-a-component
            w-ord.ord-desc1    = oe-ordl.part-dscr1
            w-ord.ord-desc2    = oe-ordl.part-dscr2

            /* gdm - 08130804*/
            w-ord.linenum      = oe-ordl.e-num

            num-rec            = num-rec + 1.

          IF AVAIL b-job-hdr THEN do:
              w-ord.due-date-jobhdr = IF b-job-hdr.due-date <> ? THEN STRING(b-job-hdr.due-date, "99/99/9999") ELSE "".
              w-ord.job-qty = IF AVAIL b-job AND AVAIL b-job-hdr THEN b-job-hdr.qty ELSE 0 .
               IF  AVAIL b-job AND AVAIL eb AND eb.est-type EQ 2 THEN
                   w-ord.job-qty =  b-job-hdr.qty * (IF eb.cust-% GT 0 THEN eb.cust-% ELSE 1)  .
               ELSE IF  AVAIL b-job AND AVAIL eb AND eb.est-type EQ 6 THEN
                    w-ord.job-qty =  b-job-hdr.qty * (IF eb.quantityPerSet GT 0 THEN eb.quantityPerSet ELSE 1)  .
          END.
          IF AVAIL b-job THEN
             w-ord.due-date-job = IF b-job.due-date <> ? THEN STRING(b-job.due-date, "99/99/9999") ELSE "".
          IF w-ord.job-no EQ "" AND fi_cas-lab:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN
          DO:
             FIND FIRST b-loadtag WHERE
                  b-loadtag.company EQ oe-ordl.company AND
                  b-loadtag.item-type EQ NO AND
                  b-loadtag.is-case-tag EQ YES AND
                  b-loadtag.tag-no EQ fi_cas-lab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                  NO-LOCK NO-ERROR.

             IF AVAIL b-loadtag THEN
             DO:

                ASSIGN
                   w-ord.job-no = b-loadtag.job-no
                   w-ord.job-no2 = b-loadtag.job-no2.

                RELEASE b-loadtag.
             END.
          END.
          RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                            OUTPUT w-ord.rel-date,
                            OUTPUT w-ord.rel-lot#,
                            OUTPUT w-ord.ship-notes,
                            INPUT ROWID(b-job-hdr)).
          IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.

          IF AVAIL itemfg THEN
            ASSIGN
             w-ord.upc-no  = itemfg.upc-no
             w-ord.box-len = itemfg.l-score[50]
             w-ord.box-wid = itemfg.w-score[50]
             w-ord.box-dep = itemfg.d-score[50]
             w-ord.flute   = itemfg.flute
             w-ord.test    = itemfg.test
             w-ord.pcs     = itemfg.case-count
             w-ord.bundle  = itemfg.case-pall
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
             w-ord.cas-no = eb.cas-no
             w-ord.form-no = eb.form-no
             w-ord.pallt-no = eb.tr-no
             w-ord.part-dscr2 = eb.part-dscr2.

          /* get it from order    task# 04120602 */
          ASSIGN w-ord.pcs    = oe-ordl.cas-cnt
                 w-ord.bundle = oe-ordl.cases-unit.

          /* get shipto from open oe-rel  */

          lv-got-shipto = NO.
          FOR EACH w-shipto:
            DELETE w-shipto.
          END.

          FOR EACH oe-rel NO-LOCK
              WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.line    EQ oe-ordl.line:
            IF NOT tb_ship-id THEN v-ship-id = oe-rel.ship-id.
            RUN oe/custxship.p (oe-rel.company,
                                oe-rel.cust-no,
                                v-ship-id,
                                BUFFER shipto).

            IF AVAIL shipto THEN DO:
              RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

              CREATE w-shipto.
              BUFFER-COPY shipto EXCEPT rec_key TO w-shipto
              ASSIGN
               w-shipto.stat   = lv-stat
               w-shipto.row-id = ROWID(oe-rel).
            END.
          END.

          FOR EACH w-shipto,
              FIRST oe-rel WHERE ROWID(oe-rel) EQ w-shipto.row-id NO-LOCK
              BREAK BY oe-rel.rel-date
                    BY oe-rel.po-no
                    BY oe-rel.ship-no 
                    BY oe-rel.qty:

            IF CAN-DO("L,S,I",w-shipto.stat) OR
               LAST(oe-rel.rel-date)          THEN DO:
              ASSIGN
               lv-got-shipto    = YES
               w-ord.ship-code  = w-shipto.ship-id
               w-ord.ship-name  = w-shipto.ship-name
               w-ord.ship-add1  = w-shipto.ship-add[1]
               w-ord.ship-add2  = w-shipto.ship-add[2]
               w-ord.ship-city  = w-shipto.ship-city
               w-ord.ship-state = w-shipto.ship-state
               w-ord.ship-ctry  = w-shipto.country
               w-ord.ship-zip   = w-shipto.ship-zip.
              LEAVE.
            END.
          END.
          FOR EACH w-shipto:
            DELETE w-shipto.
          END.

          IF NOT lv-got-shipto THEN
          FOR EACH shipto
              WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ oe-ord.cust-no
              USE-INDEX ship-id NO-LOCK
              BREAK BY shipto.ship-no DESC:
            IF NOT tb_ship-id THEN v-ship-id = oe-ord.cust-no.
            IF shipto.ship-id EQ v-ship-id OR
               LAST(shipto.ship-no)             THEN DO:
              ASSIGN
               w-ord.ship-code  = shipto.ship-id
               w-ord.ship-name  = shipto.ship-name
               w-ord.ship-add1  = shipto.ship-add[1]
               w-ord.ship-add2  = shipto.ship-add[2]
               w-ord.ship-city  = shipto.ship-city
               w-ord.ship-state = shipto.ship-state
               w-ord.ship-ctry  = shipto.country
               w-ord.ship-zip   = shipto.ship-zip.
              LEAVE.
            END.
          END.

          FIND FIRST soldto NO-LOCK
              WHERE soldto.company EQ cocode
                AND soldto.cust-no EQ oe-ord.cust-no
                AND soldto.sold-id EQ oe-ord.sold-id
              USE-INDEX sold-id NO-ERROR.

          IF AVAIL soldto THEN w-ord.sold-ctry = soldto.country.

          ASSIGN
            w-ord.total-unit = w-ord.pcs * w-ord.bundle
            /* Add .49 to round up and add 1 for extra tag   */
            w-ord.total-tags = ((oe-ordl.qty / w-ord.total-unit) + .49) +  (IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1).

        END.  /* first-of */
      END.  /* not by-release */

      ELSE
      FOR EACH oe-rel
          WHERE oe-rel.company eq cocode
          AND oe-rel.i-no      eq oe-ordl.i-no
          AND oe-rel.ord-no    eq oe-ordl.ord-no
          AND oe-rel.line      eq oe-ordl.line
          AND oe-rel.link-no   ne 0 NO-LOCK:

        CREATE w-ord.
        ASSIGN
          w-ord.ord-no       = oe-ord.ord-no
          w-ord.job-no       = oe-ordl.job-no
          w-ord.job-no2      = oe-ordl.job-no2
          w-ord.cust-no      = oe-ord.cust-no
          w-ord.cust-name    = oe-ord.cust-name
          w-ord.i-no         = oe-ordl.i-no
          w-ord.cust-part-no = oe-ordl.part-no
          w-ord.cust-po-no   = IF v-po-no-source eq "L" THEN oe-ordl.po-no
                                                        ELSE
                               IF v-po-no-source eq "R" THEN oe-rel.po-no
                                                        ELSE
                               IF v-po-no-source eq "J" AND AVAIL b-job-hdr THEN b-job-hdr.po-no 
                                   ELSE "" 
          w-ord.over-pct     = lv-over
          w-ord.qty-before   = oe-rel.qty
          w-ord.ord-qty      = w-ord.qty-before *
                               (1 + (w-ord.over-pct / 100))
          w-ord.po-no        = oe-ordl.po-no-po
          w-ord.ship-code    = oe-rel.ship-id
          w-ord.ship-add1    = oe-rel.ship-add[1]
          w-ord.ship-add2    = oe-rel.ship-add[2]
          w-ord.ship-city    = oe-rel.ship-city
          w-ord.ship-state   = oe-rel.ship-state
          w-ord.ship-zip     = oe-rel.ship-zip
          w-ord.ship-notes   = oe-rel.ship-i
          w-ord.sold-code    = oe-ord.sold-id
          w-ord.sold-name    = oe-ord.sold-name
          w-ord.sold-add1    = oe-ord.sold-add[1]
          w-ord.sold-add2    = oe-ord.sold-add[2]
          w-ord.sold-city    = oe-ord.sold-city
          w-ord.sold-state   = oe-ord.sold-state
          w-ord.sold-zip     = oe-ord.sold-zip
          w-ord.i-name       = oe-ordl.i-name
          w-ord.due-date     = 
            (if oe-ord.due-date <> ? 
             then oe-ord.due-date
             else if oe-ordl.req-date <> ?  /* 9901 CAH */
             then oe-ordl.req-date
             else today)
          w-ord.rel-date     = oe-rel.rel-date
          w-ord.est-no       = oe-ordl.est-no
          w-ord.form-no      = oe-ordl.form-no
          w-ord.vendor       = company.name
          w-ord.tare-wt      = 10
          w-ord.uom          = "EA"
          w-ord.mult         = IF AVAIL cust AND cust.int-field[1] ne 0 AND NOT glOverrideMult then
                                 cust.int-field[1] else v-mult
          w-ord.dont-run-set = oe-ordl.is-a-component
          w-ord.ord-desc1    = oe-ordl.part-dscr1
          w-ord.ord-desc2    = oe-ordl.part-dscr2

          /* gdm - 08130804*/
          w-ord.linenum      = oe-ordl.e-num.

          num-rec            = num-rec + 1.


        ASSIGN w-ord.rel-lot# = oe-rel.lot-no.
        IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.

        IF AVAIL itemfg THEN
          ASSIGN
           w-ord.upc-no  = itemfg.upc-no
           w-ord.box-len = itemfg.l-score[50]
           w-ord.box-wid = itemfg.w-score[50]
           w-ord.box-dep = itemfg.d-score[50]
           w-ord.flute   = itemfg.flute
           w-ord.test    = itemfg.test
           w-ord.pcs     = itemfg.case-count
           w-ord.bundle  = itemfg.case-pall
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
           w-ord.cas-no = eb.cas-no
           w-ord.pallt-no = eb.tr-no
           w-ord.part-dscr2 = eb.part-dscr2 . 

        RUN oe/custxship.p (oe-rel.company,
                            oe-rel.cust-no,
                            oe-rel.ship-id,
                            BUFFER shipto).

        IF AVAIL shipto THEN
          ASSIGN
           w-ord.ship-code  = shipto.ship-id
           w-ord.ship-name  = shipto.ship-name
           w-ord.ship-add1  = shipto.ship-add[1]
           w-ord.ship-add2  = shipto.ship-add[2]
           w-ord.ship-city  = shipto.ship-city
           w-ord.ship-state = shipto.ship-state
           w-ord.ship-ctry  = shipto.country
           w-ord.ship-zip   = shipto.ship-zip.

        IF AVAIL soldto THEN w-ord.sold-ctry = soldto.country.

        ASSIGN
          w-ord.pcs        = oe-rel.qty-case
          w-ord.bundle     = oe-rel.cases
          w-ord.total-unit = w-ord.pcs * w-ord.bundle
          /* Add .49 to round up and add 1 for extra tag   */
          w-ord.total-tags = ((oe-rel.qty / w-ord.total-unit) + .49) +  IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1.
      END.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE from-po C-Win 
PROCEDURE from-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH po-ordl NO-LOCK WHERE po-ordl.company EQ po-ord.company
                             AND po-ordl.po-no EQ po-ord.po-no
                             AND po-ordl.item-type EQ NO
                             AND po-ordl.i-no GE v-fitem[1]
                             AND po-ordl.i-no LE v-fitem[2]
                           USE-INDEX po-no BREAK BY po-ordl.i-no:

    IF FIRST-OF(po-ordl.i-no) THEN DO:
      FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                                AND cust.cust-no EQ po-ordl.cust-no NO-ERROR.
      FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                                AND vend.vend-no EQ po-ord.vend-no NO-ERROR.
      FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
                                  AND itemfg.i-no EQ po-ordl.i-no NO-ERROR.

      CREATE w-ord.
      ASSIGN
        w-ord.cust-name = IF AVAILABLE cust THEN cust.name ELSE ''
        w-ord.cust-no = po-ordl.cust-no
        w-ord.ship-code = po-ord.ship-id
        w-ord.due-date = po-ord.due-date
        w-ord.i-no = po-ordl.i-no
        w-ord.i-name = po-ordl.i-name
        w-ord.mult = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                     cust.int-field[1] ELSE v-mult
        w-ord.po-no = po-ord.po-no
        w-ord.tare-wt = 10
        w-ord.uom = 'EA'
        w-ord.vendor = IF AVAILABLE vend THEN vend.name ELSE ''
        num-rec = num-rec + 1.

    RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
                       "EA" ,
                       10 * itemfg.weight-100 / itemfg.t-sqft, /*convert to lb/MSF*/
                       itemfg.t-len,
                       itemfg.t-wid,
                       itemfg.t-dep,
                       po-ordl.ord-qty,
                       OUTPUT w-ord.ord-qty).
    w-ord.qty-before = w-ord.ord-qty.

/*04011307 Added because important information regarding order not included on loadtag*/
      IF po-ordl.ord-no > 0 THEN  
          FIND FIRST oe-ordl WHERE oe-ordl.company EQ po-ordl.company
            AND oe-ordl.ord-no = po-ordl.ord-no
            AND oe-ordl.i-no = po-ordl.i-no 
            NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN DO:
          ASSIGN 
              w-ord.cust-po-no = oe-ordl.po-no
              w-ord.ord-no = oe-ordl.ord-no.
          RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                            OUTPUT w-ord.rel-date,
                            OUTPUT w-ord.rel-lot#,
                            OUTPUT w-ord.ship-notes,
                            INPUT "").
          IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.
      END.

      IF AVAILABLE itemfg THEN
      ASSIGN
        w-ord.est-no = itemfg.est-no
        w-ord.upc-no = itemfg.upc-no
        w-ord.box-len = itemfg.l-score[50]
        w-ord.box-wid = itemfg.w-score[50]
        w-ord.box-dep = itemfg.d-score[50]
        w-ord.flute = itemfg.flute
        w-ord.test = itemfg.test
        w-ord.pcs = itemfg.case-count
        w-ord.bundle = IF itemfg.case-pall NE 0 THEN itemfg.case-pall ELSE 1
        w-ord.style   = itemfg.style
        w-ord.cust-part-no = itemfg.part-no .
      
    IF w-ord.ord-no > 0 THEN
       FOR EACH cust-part NO-LOCK 
             WHERE cust-part.company EQ po-ord.company   
               AND cust-part.i-no EQ po-ordl.i-no 
               AND cust-part.cust-no EQ po-ordl.cust-no 
               AND cust-part.part-no NE "":
             ASSIGN  w-ord.cust-part-no = cust-part.part-no .
             LEAVE.
         END.


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
      ASSIGN
        w-ord.flute = eb.flute
        w-ord.test = eb.test
        w-ord.pcs = eb.cas-cnt
        w-ord.bundle = eb.cas-pal
        w-ord.cas-no = eb.cas-no
        w-ord.pallt-no = eb.tr-no
        w-ord.part-dscr2 = eb.part-dscr2.
      IF NOT tb_ship-id THEN v-ship-id = po-ord.ship-id.
      FIND FIRST shipto NO-LOCK
          WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ po-ord.cust-no
            AND shipto.ship-id EQ v-ship-id
          USE-INDEX ship-id NO-ERROR.
      IF AVAIL shipto THEN
        ASSIGN
          w-ord.ship-name  = shipto.ship-name
          w-ord.ship-add1  = shipto.ship-add[1]
          w-ord.ship-add2  = shipto.ship-add[2]
          w-ord.ship-city  = shipto.ship-city
          w-ord.ship-state = shipto.ship-state
          w-ord.ship-ctry  = shipto.country
          w-ord.ship-zip   = shipto.ship-zip.

      ASSIGN
        w-ord.total-unit = w-ord.pcs * w-ord.bundle
        /* Add .49 to round up and add 1 for extra tag   */
        w-ord.total-tags = ((w-ord.ord-qty / w-ord.total-unit) + .49) +
                           (IF CAN-DO("SSLABEL,CentBox",v-loadtag) THEN 0 ELSE 1).
    END. /* first-of */
  END. /* each po-ordl */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-freight-cost C-Win 
PROCEDURE get-freight-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAM op-cost LIKE fg-rctd.frt-cost NO-UNDO.

   DEF BUFFER b-po-ordl-2 FOR po-ordl.

   FIND FIRST b-po-ordl-2 WHERE
        b-po-ordl-2.company   EQ fg-rctd.company AND
        b-po-ordl-2.po-no     EQ INT(fg-rctd.po-no) AND
        b-po-ordl-2.i-no      EQ fg-rctd.i-no AND
        b-po-ordl-2.job-no    EQ fg-rctd.job-no AND
        b-po-ordl-2.job-no2   EQ fg-rctd.job-no2 AND
        b-po-ordl-2.item-type EQ NO
        NO-LOCK NO-ERROR.

   IF AVAIL b-po-ordl-2 THEN
      RUN po/getfrtcs.p (ROWID(b-po-ordl-2),
                         fg-rctd.t-qty,
                         OUTPUT op-cost).

   RUN convert-vend-comp-curr(INPUT b-po-ordl-2.po-no, INPUT-OUTPUT op-cost).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-import-ord C-Win 
PROCEDURE get-import-ord :
/*------------------------------------------------------------------------------
  Purpose:     Get info for imported order # coming from OU1
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM v-order LIKE oe-ord.ord-no NO-UNDO.

DEF VAR v-lncnt AS INT NO-UNDO.

DEF VAR v-frstitem LIKE oe-ordl.i-no NO-UNDO.
DEF VAR v-lastitem LIKE oe-ordl.i-no NO-UNDO.
DEF VAR v-first-job LIKE job.job-no NO-UNDO.
DEF VAR v-last-job LIKE job.job-no NO-UNDO.
DEF VAR v-first-job2 LIKE job.job-no2 NO-UNDO.
DEF VAR v-last-job2 LIKE job.job-no2 NO-UNDO.

/* ASSIGN v-order = INT(ENTRY(1,v-ord-list:SCREEN-VALUE IN FRAME {&FRAME-NAME})).  */
DO WITH FRAME {&FRAME-NAME}:

  FIND FIRST bf-oe-ord NO-LOCK 
    WHERE bf-oe-ord.company EQ cocode 
      AND bf-oe-ord.ord-no  EQ v-order NO-ERROR.

  IF AVAIL bf-oe-ord THEN DO:
    ASSIGN v-lncnt = 0.
    FOR EACH bf-oe-ordl NO-LOCK
      WHERE bf-oe-ordl.company EQ bf-oe-ord.company
        AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
       BREAK BY bf-oe-ordl.i-no:
      ASSIGN v-lncnt = v-lncnt + 1.
      /* gdm - 09290903 */
      IF FIRST-OF(bf-oe-ordl.i-no) THEN ASSIGN v-frstitem = bf-oe-ordl.i-no.
      IF LAST-OF(bf-oe-ordl.i-no)  THEN ASSIGN v-lastitem = bf-oe-ordl.i-no.
    END.

    EMPTY TEMP-TABLE tt-ordjobs.
    FOR EACH bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ bf-oe-ord.company
          AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no:
       CREATE  tt-ordjobs.
       ASSIGN tt-ordjobs.job-no = bf-oe-ordl.job-no
              tt-ordjobs.job-no2 = bf-oe-ordl.job-no2.
    END.
    ASSIGN v-first-job = ""
           v-first-job2 = 0
           v-last-job = ""
           v-last-job2 = 0.
    FOR EACH tt-ordjobs BY tt-ordjobs.job-no:
       IF v-first-job = "" THEN
         v-first-job = tt-ordjobs.job-no.
       v-last-job = tt-ordjobs.job-no.
    END.
    FOR EACH tt-ordjobs BY tt-ordjobs.job-no2:
        IF v-first-job2 = 0 THEN
          v-first-job2 = tt-ordjobs.job-no2.
        v-last-job2 = tt-ordjobs.job-no2.
    END.

    ASSIGN begin_ord-no:SCREEN-VALUE = STRING(bf-oe-ord.ord-no) 
           begin_job:SCREEN-VALUE    = v-first-job
           begin_job2:SCREEN-VALUE   = STRING(v-first-job2)
           end_ord-no:SCREEN-VALUE   = STRING(bf-oe-ord.ord-no)  
           end_job:SCREEN-VALUE      = v-last-job
           end_job2:SCREEN-VALUE     = STRING(v-last-job2).

    FIND FIRST bf-oe-ordl NO-LOCK                             
     WHERE bf-oe-ordl.company EQ bf-oe-ord.company         
       AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no NO-ERROR.

    IF  v-lncnt EQ 1 THEN DO WITH FRAME {&FRAME-NAME}:
      IF AVAIL bf-oe-ordl THEN
        ASSIGN begin_i-no:SCREEN-VALUE = bf-oe-ordl.i-no
               end_i-no:SCREEN-VALUE   = bf-oe-ordl.i-no.           

  /*    
      FIND FIRST reftable NO-LOCK
        WHERE reftable.reftable EQ "cp-lab-p" 
          AND reftable.company  EQ cocode
          AND reftable.loc      EQ begin_i-no:SCREEN-VALUE
          AND reftable.loc      EQ end_i-no:SCREEN-VALUE
          AND reftable.CODE     EQ bf-oe-ord.cust-no NO-ERROR.

      IF AVAIL reftable 
        THEN ASSIGN scr-label-file:SCREEN-VALUE =  reftable.dscr.
  */
      APPLY "LEAVE" TO END_i-no.
    END.
    ELSE DO:

     FOR EACH bf-oe-ordl 
         WHERE bf-oe-ordl.company EQ bf-oe-ord.company
           AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
         NO-LOCK
         BREAK BY bf-oe-ordl.i-no:
         IF FIRST(bf-oe-ordl.i-no) THEN
             v-frstitem = bf-oe-ordl.i-no.
         IF LAST(bf-oe-ordl.i-no) THEN
             v-lastitem = bf-oe-ordl.i-no.
     END.

     ASSIGN begin_i-no:SCREEN-VALUE = v-frstitem
            end_i-no:SCREEN-VALUE   = v-lastitem.

     EMPTY TEMP-TABLE tt-ordjobs.
     FOR EACH bf-oe-ordl NO-LOCK
         WHERE bf-oe-ordl.company EQ bf-oe-ord.company
           AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no:
        CREATE  tt-ordjobs.
        ASSIGN tt-ordjobs.job-no = bf-oe-ordl.job-no
               tt-ordjobs.job-no2 = bf-oe-ordl.job-no2.
     END.
     ASSIGN v-first-job = ""
            v-first-job2 = 0
            v-last-job = ""
            v-last-job2 = 0.
     FOR EACH tt-ordjobs BY tt-ordjobs.job-no:
        IF v-first-job = "" THEN
          v-first-job = tt-ordjobs.job-no.
        v-last-job = tt-ordjobs.job-no.
     END.
     FOR EACH tt-ordjobs BY tt-ordjobs.job-no2:
         IF v-first-job2 = 0 THEN
           v-first-job2 = tt-ordjobs.job-no2.
         v-last-job2 = tt-ordjobs.job-no2.
     END.
     ASSIGN 
            begin_job:SCREEN-VALUE    = v-first-job
            begin_job2:SCREEN-VALUE   = STRING(v-first-job2)

            end_job:SCREEN-VALUE      = v-last-job
            end_job2:SCREEN-VALUE     = STRING(v-last-job2).

     APPLY "LEAVE" TO END_i-no.

      /* gdm - 09290903 */
        MESSAGE 
            "There are multiple Fg Items in this order." skip
            "     Please select an FG Item."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END. /* if > 1 items */

  END. /* avail bf-ord */

END. /* do with frame */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-jobord-info C-Win 
PROCEDURE get-jobord-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-order LIKE oe-ord.ord-no  NO-UNDO.
DEF VAR v-lncnt AS INT NO-UNDO.
DEF VAR v-frstitem LIKE oe-ordl.i-no NO-UNDO.
DEF VAR v-lastitem LIKE oe-ordl.i-no NO-UNDO.

DEF VAR v-first-order AS INT NO-UNDO.
DEF VAR v-last-order AS INT NO-UNDO.

DEF VAR ll AS INTEGER INIT 1 NO-UNDO.
DEF VAR lv-job-no AS CHAR NO-UNDO.
DEF VAR lv-job-no2 AS CHAR NO-UNDO.
DEF VAR v-job AS CHAR NO-UNDO.
DEF VAR v-job2 AS INT NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lcForm AS CHAR NO-UNDO.
DEFINE VARIABLE iForm AS CHARACTER NO-UNDO .
DEFINE VARIABLE iBlank-no AS CHARACTER NO-UNDO .
DEFINE VARIABLE lCheckForm AS LOGICAL INIT YES NO-UNDO .
DEFINE VARIABLE lCheckBlank AS LOGICAL INIT YES NO-UNDO .
DEFINE VARIABLE oplCheckForm AS LOGICAL INIT NO NO-UNDO .

DEF BUFFER bf-job FOR job.
DEF BUFFER bf-job-hdr-2 FOR job-hdr.


   v-job = ENTRY(1,v-job-list:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
/*    IF SUBSTRING(TRIM(v-job),LENGTH(TRIM(v-job)) - 5,1) = "-" THEN DO: */
/*        lcForm = SUBSTRING(TRIM(v-job),LENGTH(TRIM(v-job)) - 1,2).     */
/*        IF INT(lcForm) > 0 THEN DO:                                    */
/*            iForm = INT(lcForm).                                       */
/*            lForm = YES.                                               */
/*        END.                                                           */
/*        v-job = SUBSTRING(v-job,1,9).                                  */
/*    END.                                                               */
/*    ELSE lForm = NO.                                                   */
/*    MESSAGE lcForm  iForm lForm v-job                                  */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK.                             */
   DO li = 1 TO LENGTH(v-job):
      IF INDEX("/:-",SUBSTR(v-job,li,1)) GT 0 THEN
        ll = ll + 1.
         /*ELSE LEAVE.*/
      ELSE do:
         IF ll EQ 1 THEN lv-job-no = lv-job-no + SUBSTR(v-job,li,1).
         ELSE IF ll EQ 2 THEN lv-job-no2 = lv-job-no2 + SUBSTR(v-job,li,1).
         ELSE IF ll EQ 3 THEN iForm = iForm + SUBSTR(v-job,li,1) NO-ERROR .
         ELSE IF ll EQ 4 THEN iBlank-no = iBlank-no + SUBSTR(v-job,li,1) NO-ERROR .
      END.
   END.
   IF iForm EQ "" THEN
       lCheckForm = NO .
   IF iBlank-no EQ "" THEN
       lCheckBlank = NO .

   ASSIGN
      lv-job-no = FILL(" ",6 - LENGTH(TRIM(lv-job-no))) + lv-job-no
      v-job2 = INT(lv-job-no2).
   RUN dispJobInfo (INPUT cocode, INPUT lv-job-no, INPUT v-job2,INPUT iForm, INPUT iBlank-no, INPUT lCheckForm, INPUT lCheckBlank, OUTPUT oplCheckForm ).
  IF lCheckForm AND oplCheckForm THEN
      APPLY "choose" TO btn-ok.
  IF NOT oplCheckForm THEN do:
      MESSAGE "Please enter correct data..." skip
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
   /*
   FIND FIRST bf-job WHERE
        bf-job.company EQ cocode AND
        bf-job.job-no EQ lv-job-no AND
        bf-job.job-no2 EQ v-job2
        NO-LOCK NO-ERROR.

   IF AVAIL bf-job THEN
   DO:
      FOR EACH bf-job-hdr-2 FIELDS(i-no) NO-LOCK
          WHERE bf-job-hdr-2.company EQ bf-job.company
            AND bf-job-hdr-2.job-no  EQ bf-job.job-no
            AND bf-job-hdr-2.job-no2 EQ bf-job.job-no2
           BREAK BY bf-job-hdr-2.i-no:

           v-lncnt = v-lncnt + 1.

           IF FIRST-OF(bf-job-hdr-2.i-no) THEN
              v-frstitem = bf-job-hdr-2.i-no.
           IF LAST-OF(bf-job-hdr-2.i-no) THEN
              v-lastitem = bf-job-hdr-2.i-no.
      END.

      FOR EACH bf-job-hdr-2 FIELDS(ord-no) NO-LOCK
          WHERE bf-job-hdr-2.company EQ bf-job.company
            AND bf-job-hdr-2.job-no  EQ bf-job.job-no
            AND bf-job-hdr-2.job-no2 EQ bf-job.job-no2
           BREAK BY bf-job-hdr-2.ord-no:

           IF FIRST-OF(bf-job-hdr-2.ord-no) THEN
              v-first-order = bf-job-hdr-2.ord-no.
           IF LAST-OF(bf-job-hdr-2.ord-no) THEN
              v-last-order = bf-job-hdr-2.ord-no.
      END.

      ASSIGN
         begin_ord-no:SCREEN-VALUE = STRING(v-first-order)
         begin_job:SCREEN-VALUE    = lv-job-no         
         begin_job2:SCREEN-VALUE   = STRING(v-job2,"99")
         end_ord-no:SCREEN-VALUE   = STRING(v-last-order)
         end_job:SCREEN-VALUE      = lv-job-no     
         end_job2:SCREEN-VALUE     = STRING(v-job2,"99")
         begin_i-no:SCREEN-VALUE = v-frstitem
         end_i-no:SCREEN-VALUE   = v-lastitem.           

      APPLY "LEAVE" TO end_i-no.

      IF v-lncnt GT 1 THEN
         MESSAGE "There are multiple FG Items on this order." skip
                 "Please select an FG Item."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
   */
   /*
   FIND FIRST bf-jobhdr WHERE
        bf-jobhdr.company EQ cocode AND
        bf-jobhdr.job-no  EQ lv-job-no AND
        bf-jobhdr.job-no2 EQ v-job2 AND
        bf-jobhdr.ord-no NE 0
        NO-LOCK NO-ERROR.

   IF NOT AVAIL bf-jobhdr THEN
      FIND FIRST bf-jobhdr WHERE
           bf-jobhdr.company EQ cocode AND
           bf-jobhdr.job-no  EQ lv-job-no AND
           bf-jobhdr.job-no2 EQ v-job2
           NO-LOCK NO-ERROR.

   IF AVAIL bf-jobhdr THEN DO:
      FIND FIRST bf-oe-ord WHERE
           bf-oe-ord.company EQ cocode AND
           bf-oe-ord.ord-no  EQ bf-jobhdr.ord-no
           NO-LOCK NO-ERROR.

      IF AVAIL bf-oe-ord THEN DO:

         FOR EACH bf-oe-ordl FIELDS(i-no) NO-LOCK
           WHERE bf-oe-ordl.company EQ bf-oe-ord.company
             AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
            BREAK BY bf-oe-ordl.i-no:

            v-lncnt = v-lncnt + 1.

            IF FIRST-OF(bf-oe-ordl.i-no) THEN
               ASSIGN v-frstitem = bf-oe-ordl.i-no.
            IF LAST-OF(bf-oe-ordl.i-no) THEN
               ASSIGN v-lastitem = bf-oe-ordl.i-no.
         END.

         FIND FIRST bf-oe-ordl 
              WHERE bf-oe-ordl.company EQ bf-oe-ord.company
                AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
              NO-LOCK NO-ERROR.

         ASSIGN
            begin_ord-no:SCREEN-VALUE = STRING(bf-oe-ordl.ord-no) 
            begin_job:SCREEN-VALUE    = lv-job-no         
            begin_job2:SCREEN-VALUE   = STRING(v-job2,"99")
            end_ord-no:SCREEN-VALUE   = STRING(bf-oe-ordl.ord-no)  
            end_job:SCREEN-VALUE      = lv-job-no     
            end_job2:SCREEN-VALUE     = STRING(v-job2,"99").

         IF v-lncnt EQ 1 THEN DO WITH FRAME {&FRAME-NAME}:
            ASSIGN
               begin_i-no:SCREEN-VALUE = bf-oe-ordl.i-no
               end_i-no:SCREEN-VALUE   = bf-oe-ordl.i-no.           

            APPLY "LEAVE" TO end_i-no.
         END.
         ELSE DO:

            FIND FIRST bf-oe-ordl WHERE                             
                 bf-oe-ordl.company EQ bf-oe-ord.company AND
                 bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
                 NO-LOCK NO-ERROR.

            v-frstitem = bf-oe-ordl.i-no.

            FIND LAST bf-oe-ordl WHERE
                 bf-oe-ordl.company EQ bf-oe-ord.company AND
                 bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
                 NO-LOCK NO-ERROR.

            ASSIGN
               v-lastitem = bf-oe-ordl.i-no
               begin_i-no:SCREEN-VALUE = v-frstitem
               end_i-no:SCREEN-VALUE   = v-lastitem.

            APPLY "LEAVE" TO end_i-no.

            /* gdm - 09290903 */
            MESSAGE "There are multiple FG Items on this order." skip
                    "Please select an FG Item."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
         END.
      END. /* avail bf-oe-ord */
      ELSE /* not avail bf-oe-ord*/
      DO:
         ASSIGN
            begin_ord-no:SCREEN-VALUE = "0" 
            begin_job:SCREEN-VALUE    = lv-job-no         
            begin_job2:SCREEN-VALUE   = STRING(v-job2,"99")
            end_ord-no:SCREEN-VALUE   = "0"  
            end_job:SCREEN-VALUE      = lv-job-no     
            end_job2:SCREEN-VALUE     = STRING(v-job2,"99").

         FOR EACH bf-job-hdr-2 FIELDS(i-no) WHERE
             bf-job-hdr-2.company EQ bf-jobhdr.company AND
             bf-job-hdr-2.job-no  EQ bf-jobhdr.job-no AND
             bf-job-hdr-2.job-no2 EQ bf-jobhdr.job-no2
             NO-LOCK
             BREAK BY bf-job-hdr-2.i-no:

             v-lncnt = v-lncnt + 1.

            IF FIRST-OF(bf-job-hdr-2.i-no) THEN
               ASSIGN v-frstitem = bf-job-hdr-2.i-no.
            IF LAST-OF(bf-job-hdr-2.i-no) THEN
               ASSIGN v-lastitem = bf-job-hdr-2.i-no.
         END.

         DO WITH FRAME {&FRAME-NAME}:
            ASSIGN
               begin_i-no:SCREEN-VALUE = v-frstitem
               end_i-no:SCREEN-VALUE   = v-lastitem.           

            APPLY "LEAVE" TO end_i-no.

            IF v-lncnt GT 1 THEN
               MESSAGE "There are multiple FG Items on this order." skip
                      "Please select an FG Item."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
         END.
      END.
   END. /* avail bf-jobhdr */
   */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-ordl-info C-Win 
PROCEDURE get-ordl-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-order LIKE oe-ord.ord-no NO-UNDO.

DEF VAR v-lncnt AS INT NO-UNDO.

DEF VAR v-frstitem LIKE oe-ordl.i-no NO-UNDO.
DEF VAR v-lastitem LIKE oe-ordl.i-no NO-UNDO.
DEF VAR v-first-job LIKE job.job-no NO-UNDO.
DEF VAR v-last-job LIKE job.job-no NO-UNDO.
DEF VAR v-first-job2 LIKE job.job-no2 NO-UNDO.
DEF VAR v-last-job2 LIKE job.job-no2 NO-UNDO.

ASSIGN v-order = INT(ENTRY(1,v-ord-list:SCREEN-VALUE IN FRAME {&FRAME-NAME})).

FIND FIRST bf-oe-ord NO-LOCK 
  WHERE bf-oe-ord.company EQ cocode 
    AND bf-oe-ord.ord-no  EQ v-order NO-ERROR.
IF AVAIL bf-oe-ord THEN DO:
  ASSIGN v-lncnt = 0.
  FOR EACH bf-oe-ordl NO-LOCK
    WHERE bf-oe-ordl.company EQ bf-oe-ord.company
      AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
     BREAK BY bf-oe-ordl.i-no:
    ASSIGN v-lncnt = v-lncnt + 1.
    /* gdm - 09290903 */
    IF FIRST-OF(bf-oe-ordl.i-no) THEN ASSIGN v-frstitem = bf-oe-ordl.i-no.
    IF LAST-OF(bf-oe-ordl.i-no)  THEN ASSIGN v-lastitem = bf-oe-ordl.i-no.
  END.
  EMPTY TEMP-TABLE tt-ordjobs.
  FOR EACH bf-oe-ordl NO-LOCK
      WHERE bf-oe-ordl.company EQ bf-oe-ord.company
        AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no:
     CREATE  tt-ordjobs.
     ASSIGN tt-ordjobs.job-no = bf-oe-ordl.job-no
            tt-ordjobs.job-no2 = bf-oe-ordl.job-no2.
  END.
  ASSIGN v-first-job = ""
         v-first-job2 = 0
         v-last-job = ""
         v-last-job2 = 0.
  FOR EACH tt-ordjobs BY tt-ordjobs.job-no:
     IF v-first-job = "" THEN
       v-first-job = tt-ordjobs.job-no.
     v-last-job = tt-ordjobs.job-no.
  END.
  FOR EACH tt-ordjobs BY tt-ordjobs.job-no2:
      IF v-first-job2 = 0 THEN
        v-first-job2 = tt-ordjobs.job-no2.
      v-last-job2 = tt-ordjobs.job-no2.
  END.

  ASSIGN begin_ord-no:SCREEN-VALUE = STRING(bf-oe-ord.ord-no) 
         begin_job:SCREEN-VALUE    = v-first-job
         begin_job2:SCREEN-VALUE   = STRING(v-first-job2)
         end_ord-no:SCREEN-VALUE   = STRING(bf-oe-ord.ord-no)  
         end_job:SCREEN-VALUE      = v-last-job
         end_job2:SCREEN-VALUE     = STRING(v-last-job2).
  FIND FIRST bf-oe-ordl NO-LOCK                             
   WHERE bf-oe-ordl.company EQ bf-oe-ord.company         
     AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no NO-ERROR.

  IF v-lncnt EQ 1 THEN DO WITH FRAME {&FRAME-NAME}:
    IF AVAIL bf-oe-ordl THEN
      ASSIGN begin_i-no:SCREEN-VALUE = bf-oe-ordl.i-no
             end_i-no:SCREEN-VALUE   = bf-oe-ordl.i-no.           
/*    
    FIND FIRST reftable NO-LOCK
      WHERE reftable.reftable EQ "cp-lab-p" 
        AND reftable.company  EQ cocode
        AND reftable.loc      EQ begin_i-no:SCREEN-VALUE
        AND reftable.loc      EQ end_i-no:SCREEN-VALUE
        AND reftable.CODE     EQ bf-oe-ord.cust-no NO-ERROR.

    IF AVAIL reftable 
      THEN ASSIGN scr-label-file:SCREEN-VALUE =  reftable.dscr.
*/
    APPLY "LEAVE" TO END_i-no.
  END.
  ELSE DO:

   FOR EACH bf-oe-ordl 
       WHERE bf-oe-ordl.company EQ bf-oe-ord.company
         AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
       NO-LOCK
       BREAK BY bf-oe-ordl.i-no:
       IF FIRST(bf-oe-ordl.i-no) THEN
           v-frstitem = bf-oe-ordl.i-no.
       IF LAST(bf-oe-ordl.i-no) THEN
           v-lastitem = bf-oe-ordl.i-no.
   END.
   ASSIGN begin_i-no:SCREEN-VALUE = v-frstitem
          end_i-no:SCREEN-VALUE   = v-lastitem.

   EMPTY TEMP-TABLE tt-ordjobs.
   FOR EACH bf-oe-ordl NO-LOCK
       WHERE bf-oe-ordl.company EQ bf-oe-ord.company
         AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no:
      CREATE  tt-ordjobs.
      ASSIGN tt-ordjobs.job-no = bf-oe-ordl.job-no
             tt-ordjobs.job-no2 = bf-oe-ordl.job-no2.
   END.
   ASSIGN v-first-job = ""
          v-first-job2 = 0
          v-last-job = ""
          v-last-job2 = 0.
   FOR EACH tt-ordjobs BY tt-ordjobs.job-no:
      IF v-first-job = "" THEN
        v-first-job = tt-ordjobs.job-no.
      v-last-job = tt-ordjobs.job-no.
   END.
   FOR EACH tt-ordjobs BY tt-ordjobs.job-no2:
       IF v-first-job2 = 0 THEN
         v-first-job2 = tt-ordjobs.job-no2.
       v-last-job2 = tt-ordjobs.job-no2.
   END.
   ASSIGN 
          begin_job:SCREEN-VALUE    = v-first-job
          begin_job2:SCREEN-VALUE   = STRING(v-first-job2)

          end_job:SCREEN-VALUE      = v-last-job
          end_job2:SCREEN-VALUE     = STRING(v-last-job2).

   APPLY "LEAVE" TO END_i-no.

    /* gdm - 09290903 */
      MESSAGE 
          "There are multiple Fg Items in this order." skip
          "     Please select an FG Item."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-po-info C-Win 
PROCEDURE get-po-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-poord LIKE po-ord.po-no NO-UNDO.

DEF VAR v-lncnt AS INT NO-UNDO.

DEF VAR v-frstitem LIKE oe-ordl.i-no NO-UNDO INIT "ZZZZZZZZZZZZZZZZZ".
DEF VAR v-lastitem LIKE oe-ordl.i-no NO-UNDO INIT "".

ASSIGN v-poord = INT(ENTRY(1,v-ord-list:SCREEN-VALUE IN FRAME {&FRAME-NAME})).

FIND FIRST bf-po-ord NO-LOCK 
  WHERE bf-po-ord.company EQ cocode 
    AND bf-po-ord.po-no  EQ v-poord NO-ERROR.
IF AVAIL bf-po-ord THEN DO:
  ASSIGN v-lncnt = 0.
  FOR EACH bf-po-ordl NO-LOCK
    WHERE bf-po-ordl.company EQ bf-po-ord.company
      AND bf-po-ordl.po-no  EQ bf-po-ord.po-no
     BREAK BY bf-po-ordl.i-no:
    ASSIGN v-lncnt = v-lncnt + 1.

    IF bf-po-ordl.i-no LT v-frstitem THEN v-frstitem = bf-po-ordl.i-no.
    IF bf-po-ordl.i-no GT v-lastitem THEN v-lastitem = bf-po-ordl.i-no.                                                                           .


  END.

  FIND FIRST bf-po-ordl NO-LOCK
      WHERE bf-po-ordl.company EQ bf-po-ord.company
        AND bf-po-ordl.po-no  EQ bf-po-ord.po-no NO-ERROR.

  ASSIGN begin_ord-no:SCREEN-VALUE = STRING(bf-po-ordl.po-no) 
         end_ord-no:SCREEN-VALUE   = STRING(bf-po-ordl.po-no).

  IF v-lncnt EQ 1 THEN DO WITH FRAME {&FRAME-NAME}:

    ASSIGN begin_i-no:SCREEN-VALUE = bf-po-ordl.i-no
           end_i-no:SCREEN-VALUE   = bf-po-ordl.i-no.           
/*    
    FIND FIRST reftable NO-LOCK
      WHERE reftable.reftable EQ "cp-lab-p" 
        AND reftable.company  EQ cocode
        AND reftable.loc      EQ begin_i-no:SCREEN-VALUE
        AND reftable.loc      EQ end_i-no:SCREEN-VALUE
        AND reftable.CODE     EQ bf-po-ord.cust-no NO-ERROR.

    IF AVAIL reftable 
      THEN ASSIGN scr-label-file:SCREEN-VALUE =  reftable.dscr.
*/
    APPLY "LEAVE" TO END_i-no.

  END.
  ELSE DO:

/*    FIND FIRST bf-po-ordl NO-LOCK                         */
/*     WHERE bf-po-ordl.company EQ bf-po-ord.company        */
/*       AND bf-po-ordl.po-no  EQ bf-po-ord.po-no           */
/*      BY                                                  */
/*       NO-ERROR.                                          */
/*    ASSIGN v-frstitem = bf-po-ordl.i-no.                  */
/*                                                          */
/*    FIND LAST bf-po-ordl NO-LOCK                          */
/*     WHERE bf-po-ordl.company EQ bf-po-ord.company        */
/*       AND bf-po-ordl.po-no  EQ bf-po-ord.po-no NO-ERROR. */
/*    ASSIGN v-lastitem = bf-po-ordl.i-no.                  */

   ASSIGN begin_i-no:SCREEN-VALUE = v-frstitem
          end_i-no:SCREEN-VALUE   = v-lastitem.

   APPLY "LEAVE" TO END_i-no.

   MESSAGE 
      "There are multiple Fg Items in this PO." skip
      "     Please select an FG Item."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

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
  DEF OUTPUT PARAM opcShipNotes LIKE w-ord.ship-notes NO-UNDO.
  DEF INPUT PARAMETER ip-job AS ROWID NO-UNDO .
  DEF BUFFER b-job-hdr FOR job-hdr.
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
          AND oe-relh.rel-date GE begin_date
          AND oe-relh.rel-date LE end_date
          AND oe-relh.ship-id GE begin_ship-to
          AND oe-relh.ship-id LE end_ship-to
        BY oe-relh.rel-date
        BY oe-relh.r-no:

      ASSIGN
        op-pono = oe-rell.po-no
        op-date = oe-relh.rel-date
        opcShipNotes[1] = oe-relh.ship-i[1]
        opcShipNotes[2] = oe-relh.ship-i[2]
        opcShipNotes[3] = oe-relh.ship-i[3]
        opcShipNotes[4] = oe-relh.ship-i[4]
        .
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
          AND oe-rel.rel-date GE begin_date
          AND oe-rel.rel-date LE end_date
          AND oe-rel.ship-id GE begin_ship-to
          AND oe-rel.ship-id LE end_ship-to
        BY oe-rel.rel-date
        BY oe-rel.r-no:

      ASSIGN
        op-pono = (IF oe-rel.po-no GT "" THEN oe-rel.po-no ELSE oe-ordl.po-no)
        op-date = oe-rel.rel-date
        opcShipNotes[1] = oe-rel.ship-i[1]
        opcShipNotes[2] = oe-rel.ship-i[2]
        opcShipNotes[3] = oe-rel.ship-i[3]
        opcShipNotes[4] = oe-rel.ship-i[4]
       .
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

    ASSIGN 
        op-pono = (IF oe-rel.po-no GT "" THEN oe-rel.po-no ELSE oe-ordl.po-no)
        op-date = oe-rel.rel-date
        opcShipNotes[1] = oe-rel.ship-i[1]
        opcShipNotes[2] = oe-rel.ship-i[2]
        opcShipNotes[3] = oe-rel.ship-i[3]
        opcShipNotes[4] = oe-rel.ship-i[4]
        .
    LEAVE.
  END.
  
  IF AVAILABLE oe-rel THEN 
    ASSIGN op-lot# = oe-rel.lot-no.
  IF v-po-no-source EQ "J" THEN
      FIND FIRST b-job-hdr WHERE ROWID(b-job-hdr) EQ ip-job NO-LOCK NO-ERROR .

  IF v-po-no-source NE "R"                    OR
     (NOT AVAIL oe-rel AND NOT AVAIL oe-rell) THEN DO:
    IF  v-po-no-source NE "J" THEN
    op-pono = IF v-po-no-source EQ "L" AND AVAIL oe-ordl THEN oe-ordl.po-no
                                       ELSE IF v-po-no-source EQ "H" AND AVAIL oe-ord THEN oe-ord.po-no
                                       ELSE "".
    IF  v-po-no-source EQ "J" THEN 
       op-pono =  IF AVAIL b-job-hdr THEN b-job-hdr.po-no 
                                    ELSE "" .

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-set-full-qty C-Win 
PROCEDURE get-set-full-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER ipc-job-no     AS CHAR NO-UNDO.
  DEF INPUT PARAMETER ipi-job-no2    AS INT  NO-UNDO.
  DEF INPUT PARAMETER ipc-i-no       AS CHAR NO-UNDO.
  DEF INPUT PARAMETER ipd-current-qty AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-cost-to-set AS DEC NO-UNDO.
  def OUTPUT parameter op-out-qty as DEC no-undo.


  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty as dec no-undo.
  def var lv-out-cost as dec no-undo.
  DEF VAR lv-calc-cost AS DEC.
  DEF VAR lv-recalc-cost AS DEC.
  DEF VAR lv-ext-cost AS DEC.
  DEF VAR v-rec-qty AS INT NO-UNDO.
  DEF VAR ll-ea AS LOG NO-UNDO.

  DEF BUFFER b-fg-rctd FOR fg-rctd.
  DEF BUFFER b1-fg-rctd FOR fg-rctd.


  cocode = g_company.

  lv-out-qty = 0.
  FOR EACH b-fg-rctd WHERE b-fg-rctd.company eq g_company and
           (b-fg-rctd.rita-code eq "R" or b-fg-rctd.rita-code eq "E")
           AND trim(b-fg-rctd.job-no) = trim(ipc-job-no)
           AND b-fg-rctd.job-no2 = INT(ipi-job-no2)
           AND b-fg-rctd.i-no = ipc-i-no 
           NO-LOCK :

      lv-out-qty = lv-out-qty + b-fg-rctd.t-qty.     
      IF ip-cost-to-set GT 0 THEN DO:

          /* convert cost to b1-fg-rctd uom */

          FIND b1-fg-rctd WHERE ROWID(b1-fg-rctd) EQ ROWID(b-fg-rctd)
              EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL b1-fg-rctd THEN DO WITH FRAME {&FRAME-NAME}:

            find itemfg where itemfg.company eq cocode
                          and itemfg.i-no  eq b-fg-rctd.i-no
                        use-index i-no no-lock no-error.

            ASSIGN
              v-bwt             = 0
              v-dep             = 0.

            IF AVAIL itemfg THEN
              ASSIGN v-len       = itemfg.t-len
                     v-wid       = itemfg.t-wid.

            /* Always find just to get quantity */
            find first po-ordl where po-ordl.company = cocode
                                 and po-ordl.po-no   = int(b-fg-rctd.po-no)
                                 and po-ordl.i-no    = b-fg-rctd.i-no
                                 and po-ordl.job-no  = b-fg-rctd.job-no
                                 and po-ordl.job-no2 = b-fg-rctd.job-no2
                                 and po-ordl.item-type = no
                                 no-lock no-error.
            IF NOT AVAIL po-ordl THEN
                find first po-ordl where po-ordl.company = cocode
                                     and po-ordl.po-no   = integer(b-fg-rctd.po-no)
                                     and po-ordl.i-no    = b-fg-rctd.i-no
                                     and po-ordl.item-type = no
                                     no-lock no-error.


            IF AVAIL po-ordl THEN
              ASSIGN
                v-len = po-ordl.s-len
                v-wid = po-ordl.s-wid.
            lv-calc-cost = ip-cost-to-set.
            lv-recalc-cost = lv-calc-cost.
            IF fg-rctd.cost-uom EQ b-fg-rctd.cost-uom               OR
              (LOOKUP(fg-rctd.cost-uom,fg-uom-list) GT 0 AND
               LOOKUP(b-fg-rctd.cost-uom,fg-uom-list) GT 0)   THEN.
            ELSE
               RUN rm/convcuom.p(fg-rctd.cost-uom, b-fg-rctd.cost-uom, 
                                 v-bwt, v-len, v-wid, v-dep,
                                 lv-calc-cost, OUTPUT lv-recalc-cost).

            b1-fg-rctd.std-cost = lv-recalc-cost.
            ASSIGN
             lv-ext-cost = b1-fg-rctd.t-qty * b1-fg-rctd.std-cost                          
             b1-fg-rctd.ext-cost = lv-ext-cost + b1-fg-rctd.frt-cost.
          END.

      END.
  END.

  lv-out-qty = lv-out-qty + ipd-current-qty.

  op-out-qty = lv-out-qty.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE incrementPalletID C-Win 
PROCEDURE incrementPalletID :
/*------------------------------------------------------------------------------
  Purpose:     Increment the pallet number for a given customer and return the
                new value
  Parameters:  INPUT: cust buffer OUTPUT: next pallet #
  Notes:       Defaults value if not set for given cust
               Returns error code of -1   
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipb-cust FOR cust.
DEFINE INPUT PARAMETER ipi-tags AS INT NO-UNDO.
DEFINE OUTPUT PARAMETER op-start-pallet-no LIKE cust.spare-int-1.
DEFINE OUTPUT PARAMETER op-end-pallet-no LIKE cust.spare-int-1.
DEF BUFFER bf-cust FOR cust.
DEF VAR li AS INT INIT 0.
DEF VAR lj AS INT INIT 0.

FIND bf-cust WHERE ROWID(bf-cust) EQ ROWID(ipb-cust) EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL bf-cust THEN DO:
    op-end-pallet-no = 0.
    RETURN.
END.

IF bf-cust.spare-int-1 EQ 0 THEN DO:
    op-end-pallet-no = 0.
    RETURN.
END.
li = bf-cust.spare-int-1.
IF li MOD 1000000 = 999999 THEN DO:
        /*protection code*/
    op-end-pallet-no = -1.
    RETURN.
END.
op-start-pallet-no = li + 1.
DO lj = 1 TO ipi-tags:
    li = li + 1.
    IF li MOD 1000000 = 999999 THEN DO:
        /*protection code*/
        op-end-pallet-no = -1.
        RETURN.
    END.
END.

ASSIGN 
    op-end-pallet-no = li
    bf-cust.spare-int-1 = li.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE leave-job-label C-Win 
PROCEDURE leave-job-label :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-job-no AS CHAR NO-UNDO.
   DEF VAR v-job-no-end AS CHAR NO-UNDO.
   DEF VAR v-cust-no AS CHAR NO-UNDO.
   DEF BUFFER b-job-hdr-2 FOR job-hdr.
   DEFINE VARIABLE oplCheckForm AS LOGICAL INIT NO NO-UNDO .
   DO WITH FRAME {&FRAME-NAME}:
      IF scr-label-file:SCREEN-VALUE EQ "" AND
      begin_job:SCREEN-VALUE NE "" AND 
      end_job:SCREEN-VALUE NE "" AND
      begin_job:SCREEN-VALUE EQ end_job:SCREEN-VALUE AND
      INT(begin_job2:SCREEN-VALUE) EQ INT(end_job2:SCREEN-VALUE) AND
      v-auto-print AND LOGICAL(scr-freeze-label:SCREEN-VALUE) EQ NO THEN
      DO:
         RELEASE reftable.
         RELEASE oe-ord.
         RELEASE oe-rel.
         RELEASE job-hdr.
         RELEASE shipto.

         v-job-no = FILL(" ",6 - LENGTH(TRIM(begin_job:SCREEN-VALUE)))
                  + TRIM(begin_job:SCREEN-VALUE).

         IF begin_i-no:SCREEN-VALUE EQ end_i-no:SCREEN-VALUE AND
            begin_i-no:SCREEN-VALUE NE "" THEN
            FIND FIRST job-hdr WHERE
                 job-hdr.company EQ cocode AND
                 job-hdr.job-no EQ v-job-no AND
                 job-hdr.job-no2 EQ INT(begin_job2:SCREEN-VALUE) AND
                 job-hdr.i-no EQ begin_i-no:SCREEN-VALUE AND
                 job-hdr.ord-no NE 0
                 NO-LOCK NO-ERROR.
         ELSE
         DO:
            FIND FIRST job-hdr WHERE
                 job-hdr.company EQ cocode AND
                 job-hdr.job-no EQ v-job-no AND
                 job-hdr.job-no2 EQ INT(begin_job2:SCREEN-VALUE) AND
                 job-hdr.ord-no NE 0
                 NO-LOCK NO-ERROR.

            IF NOT AVAIL job-hdr THEN
               FIND FIRST job-hdr WHERE
                    job-hdr.company EQ cocode AND
                    job-hdr.job-no EQ v-job-no AND
                    job-hdr.job-no2 EQ INT(begin_job2:SCREEN-VALUE)
                    NO-LOCK NO-ERROR.
         END.

         IF AVAIL job-hdr THEN DO:
           v-cust-no = job-hdr.cust-no.
           IF begin_i-no:SCREEN-VALUE EQ "" THEN
             RUN dispJobInfo (INPUT cocode, INPUT v-job-no, INPUT INT(begin_job2:SCREEN-VALUE), 0,0, NO,NO, OUTPUT oplCheckForm).
         END.


        IF job-hdr.ord-no NE 0 THEN
           FIND FIRST oe-ord WHERE
                oe-ord.company EQ cocode AND
                oe-ord.ord-no  EQ job-hdr.ord-no
                NO-LOCK NO-ERROR.

        IF AVAIL oe-ord THEN
           v-cust-no = oe-ord.cust-no.

        FIND FIRST reftable WHERE
             reftable.reftable EQ "cp-lab-p" AND
             reftable.company  EQ cocode AND
             reftable.loc      EQ job-hdr.i-no AND
             reftable.CODE     EQ v-cust-no
             NO-LOCK NO-ERROR.

         IF AVAIL reftable AND reftable.code2 NE "" THEN
            scr-label-file:SCREEN-VALUE = (IF reftable.dscr <> "" THEN
                                              reftable.dscr
                                           ELSE bardir-chr).
         ELSE DO:
            IF AVAIL oe-ord THEN
               FIND FIRST oe-rel WHERE
                    oe-rel.company EQ cocode AND
                    oe-rel.i-no    EQ job-hdr.i-no AND
                    oe-rel.ord-no  EQ oe-ord.ord-no 
                    NO-LOCK NO-ERROR.

            IF AVAIL oe-rel THEN 
               FIND FIRST shipto WHERE
                    shipto.company EQ cocode AND
                    shipto.cust-no EQ oe-rel.cust-no AND
                    shipto.ship-id EQ oe-rel.ship-id 
                    USE-INDEX ship-id NO-LOCK NO-ERROR.
            ELSE
               FIND FIRST shipto WHERE
                    shipto.company EQ cocode AND
                    shipto.cust-no EQ v-cust-no AND
                    shipto.ship-id EQ v-cust-no
                    USE-INDEX ship-id NO-LOCK NO-ERROR.

            IF AVAIL shipto THEN DO:

               FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company EQ cocode AND
                    sys-ctrl-shipto.NAME EQ "BARDIR" AND
                    sys-ctrl-shipto.cust-vend EQ YES AND
                    sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                    sys-ctrl-shipto.ship-id      EQ shipto.ship-id AND
                    sys-ctrl-shipto.char-fld     NE ''
                    NO-LOCK NO-ERROR.

               IF AVAIL sys-ctrl-shipto AND
                  TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                  scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
               ELSE 
                  FIND FIRST sys-ctrl-shipto WHERE
                       sys-ctrl-shipto.company      EQ cocode AND
                       sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                       sys-ctrl-shipto.cust-vend    EQ YES AND
                       sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                       sys-ctrl-shipto.char-fld     NE ''
                       NO-LOCK NO-ERROR.

               IF AVAIL sys-ctrl-shipto AND 
                  TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                  scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.

               IF scr-label-file:SCREEN-VALUE EQ "" THEN
               DO:
                  FIND FIRST sys-ctrl-shipto WHERE
                       sys-ctrl-shipto.company      EQ cocode AND
                       sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                       sys-ctrl-shipto.cust-vend-no EQ "" AND
                       sys-ctrl-shipto.cust-vend    EQ YES 
                       NO-LOCK NO-ERROR.

                  IF AVAIL sys-ctrl-shipto AND 
                     TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                     scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                  ELSE DO:
                     FIND FIRST sys-ctrl NO-LOCK 
                          WHERE sys-ctrl.company EQ cocode 
                            AND sys-ctrl.name    EQ "BARDIR" 
                          NO-ERROR.
                      IF AVAIL sys-ctrl THEN
                         scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                      ELSE
                         scr-label-file:SCREEN-VALUE = "".
                  END.
               END.
            END.
            ELSE
            DO:
               FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company      EQ cocode AND
                    sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                    sys-ctrl-shipto.cust-vend    EQ YES AND
                    sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                    sys-ctrl-shipto.char-fld     NE ''
                    NO-LOCK NO-ERROR.
               IF AVAIL sys-ctrl-shipto AND 
                  TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                  scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
               ELSE DO:
                  FIND FIRST sys-ctrl-shipto NO-LOCK 
                    WHERE sys-ctrl-shipto.company      EQ cocode 
                      AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                      AND sys-ctrl-shipto.cust-vend-no EQ ""
                      AND sys-ctrl-shipto.cust-vend    EQ YES 
                    NO-ERROR.
                  IF AVAIL sys-ctrl-shipto AND 
                     TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                     scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                  ELSE DO:
                     FIND FIRST sys-ctrl WHERE
                          sys-ctrl.company EQ cocode AND
                          sys-ctrl.name    EQ "BARDIR" 
                          NO-LOCK NO-ERROR.
                     IF AVAIL sys-ctrl THEN
                        scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                     ELSE
                        scr-label-file:SCREEN-VALUE = "".
                  END.
               END.
            END.
         END.
      END.
   END.

   v-job-no = FILL(" ",6 - LENGTH(TRIM(begin_job:SCREEN-VALUE)))
         + TRIM(begin_job:SCREEN-VALUE).
   IF begin_i-no:SCREEN-VALUE EQ "" THEN
     RUN dispJobInfo (INPUT cocode, INPUT v-job-no, INPUT INT(begin_job2:SCREEN-VALUE),0,0,NO,NO,OUTPUT oplCheckForm).


   v-job-no = FILL(" ",6 - LENGTH(TRIM(begin_job:SCREEN-VALUE)))
            + TRIM(begin_job:SCREEN-VALUE).
   v-job-no-end = FILL(" ",6 - LENGTH(TRIM(end_job:SCREEN-VALUE)))
            + TRIM(end_job:SCREEN-VALUE).

   IF INT(end_job2:SCREEN-VALUE) > 0 AND TRIM(v-job-no) > ""  THEN DO:
       FIND FIRST b-job-hdr-2 WHERE b-job-hdr-2.job-no EQ v-job-no
           NO-LOCK NO-ERROR.

       FOR EACH b-job-hdr-2  WHERE
        b-job-hdr-2.company EQ cocode AND
        b-job-hdr-2.job-no  GE v-job-no AND
        b-job-hdr-2.job-no2 GE INT(begin_job2:SCREEN-VALUE) AND
        b-job-hdr-2.job-no  LE v-job-no-end AND                
        b-job-hdr-2.job-no2 LE INT(end_job2:SCREEN-VALUE) AND
        b-job-hdr-2.i-no    GE begin_i-no:SCREEN-VALUE AND
        b-job-hdr-2.i-no    LE end_i-no:SCREEN-VALUE
        NO-LOCK:
       IF NOT CAN-FIND(FIRST ttblJob WHERE
          ttblJob.company EQ b-job-hdr-2.company AND
          ttblJob.job-no  EQ b-job-hdr-2.job-no AND
          ttblJob.job-no2 EQ b-job-hdr-2.job-no2 AND
          ttblJob.ord-no  EQ b-job-hdr-2.ord-no) THEN
          DO:
             CREATE ttblJob.
             ASSIGN
                ttblJob.company = b-job-hdr-2.company
                ttblJob.job-no = b-job-hdr-2.job-no
                ttblJob.job-no2 = b-job-hdr-2.job-no2
                ttblJob.ord-no  = b-job-hdr-2.ord-no.
             RELEASE ttblJob.
          END.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-cas-lab C-Win 
PROCEDURE new-cas-lab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    IF fi_cas-lab:SCREEN-VALUE NE "" THEN DO:
      FIND FIRST loadtag
          WHERE loadtag.company     EQ cocode
            AND loadtag.tag-no      BEGINS TRIM(fi_cas-lab:SCREEN-VALUE)
            AND loadtag.item-type   EQ NO
            AND loadtag.is-case-tag EQ YES
          NO-LOCK NO-ERROR.
      IF AVAIL loadtag THEN DO:

          FIND FIRST oe-ord NO-LOCK
          WHERE oe-ord.company EQ loadtag.company
            AND oe-ord.ord-no  EQ loadtag.ord-no
            AND oe-ord.job-no  EQ loadtag.job-no
            AND oe-ord.job-no2 EQ loadtag.job-no2
            AND oe-ord.ord-no GE 0
            NO-ERROR.

         IF AVAIL oe-ord THEN do:
            ASSIGN
             begin_ord-no:SCREEN-VALUE = STRING(loadtag.ord-no)
             end_ord-no:SCREEN-VALUE   = STRING(loadtag.ord-no).
          END.
          ELSE
            ASSIGN
             begin_ord-no:SCREEN-VALUE = ""
             end_ord-no:SCREEN-VALUE   = ""
              .

            ASSIGN
             fi_cas-lab:SCREEN-VALUE   = loadtag.tag-no
             begin_job:SCREEN-VALUE    = loadtag.job-no
             end_job:SCREEN-VALUE      = loadtag.job-no
             begin_job2:SCREEN-VALUE   = STRING(loadtag.job-no2)
             end_job2:SCREEN-VALUE     = STRING(loadtag.job-no2)
             begin_i-no:SCREEN-VALUE   = loadtag.i-no
             end_i-no:SCREEN-VALUE     = loadtag.i-no.


            RUN cas-lab-label-mat-file.

            RUN checkReturns.
            IF RETURN-VALUE EQ 'ERROR' THEN RETURN 'ERROR'.
            IF tb_ret:SCREEN-VALUE EQ "NO" THEN
            RUN ok-button.

      END.
      ELSE IF tb_reprint-tag THEN DO: /* task# 09200517*/
           FIND FIRST loadtag WHERE loadtag.company     EQ cocode
                          AND loadtag.item-type   EQ NO
                          AND loadtag.tag-no  eq TRIM(fi_cas-lab:SCREEN-VALUE) NO-LOCK NO-ERROR.
           IF NOT AVAIL loadtag THEN DO:
              MESSAGE "Invalid Loadtag. Try Help." VIEW-AS ALERT-BOX ERROR.
              APPLY "entry" TO fi_cas-lab.
              RETURN ERROR.
           END.
           ASSIGN fi_cas-lab:SCREEN-VALUE   = loadtag.tag-no
                  begin_ord-no:SCREEN-VALUE = STRING(loadtag.ord-no)
                  end_ord-no:SCREEN-VALUE   = STRING(loadtag.ord-no)
                  begin_job:SCREEN-VALUE    = loadtag.job-no
                  end_job:SCREEN-VALUE      = loadtag.job-no
                  begin_job2:SCREEN-VALUE   = STRING(loadtag.job-no2)
                  end_job2:SCREEN-VALUE     = STRING(loadtag.job-no2)
                  begin_i-no:SCREEN-VALUE   = loadtag.i-no
                  end_i-no:SCREEN-VALUE     = loadtag.i-no.

           RUN cas-lab-label-mat-file.

           IF lReturn THEN DO:
           APPLY "choose" TO btn-ok.
             lReturn = NO.
           END.
           ELSE
           RUN ok-button. 
      END.
      ELSE MESSAGE "Invalid Loadtag. Try Help." VIEW-AS ALERT-BOX ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextRfidTag C-Win 
PROCEDURE nextRfidTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipcCompany AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER opcRfidTag AS CHAR NO-UNDO.
DEF VAR dRFIDTag AS DECIMAL NO-UNDO.
  FIND FIRST oe-ctrl WHERE oe-ctrl.company = ipcCompany NO-ERROR.
  dRFIDTag = IF AVAIL oe-ctrl AND oe-ctrl.spare-char-1 <> "" 
                    THEN dec(oe-ctrl.spare-char-1) ELSE 111110000000000000000001. 
  oe-ctrl.spare-char-1 = string(dRFIDTag + 1).

  opcRfidTag = string(dRFIDTag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok-button C-Win 
PROCEDURE ok-button :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF begin_filename:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" AND
     userLabelPath <> "" THEN        
     begin_filename:SCREEN-VALUE = userLabelPath.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  ASSIGN gvlCreateWithMaxPrompted = NO.
  /* gdm - 04090909 */
  ASSIGN v-out = begin_filename.
  IF v-out EQ ""  THEN DO:

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
  END.
  /* gdm - 04090909 end */

  FILE-INFO:FILE-NAME = begin_filename.
  if begin_filename <> "" AND FILE-INFO:FILE-type eq ? then do:
     message "Form file/path does not exist. Do you want to create it?" 
             view-as alert-box ERROR BUTTON YES-NO UPDATE v-ans AS LOG.
     IF v-ans THEN OS-CREATE-DIR VALUE(begin_filename).
     ELSE do:
         MESSAGE "Loadtag file path is not valid. Can't create."
             VIEW-AS ALERT-BOX ERROR.
         return no-apply.
     END.
   end.

  /* gdm - 09290903 */
  IF v-ord-list:SCREEN-VALUE NE "" THEN DO:
    ASSIGN begin_ord-no = 0
           end_ord-no   = 0
/*            begin_i-no   = "" */
/*            end_i-no     = "" */
           begin_job    = ""
           end_job      = "".
  END.
  /* gdm - 09290903 end*/
  /*wfk  */
  IF tb_reprint-tag THEN RUN reprint-tag.
  ELSE RUN run-report NO-ERROR. 

  IF NOT ERROR-STATUS:ERROR THEN lv-ok-ran = YES.
    IF gvcSkippedItem NE "" THEN 
      MESSAGE "The finished goods receipt was not created " skip
              "for item # " + gvcSkippedItem + ", Job # " + gvcSkippedJob SKIP
              "due to insufficient component inventory."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.



  APPLY "entry" TO fi_cas-lab IN FRAME {&FRAME-NAME}.

  IF lv-ok-ran AND NOT tb_reprint-tag AND tb_close THEN do:
     IF program-name(1) matches "*r-loadtg.*"
        and not program-name(2) matches "*persist*" THEN DO:
         RUN system/userLogOut.p.
     END.
      APPLY "close" TO THIS-PROCEDURE.
  END.
  /*
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 

 lv-ok-ran = YES.
 */

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

    RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
  */  
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
  run scr-rpt.w (list-name,c-win:title). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-all C-Win 
PROCEDURE post-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-fg-rctd FOR fg-rctd.


FOR EACH  tt-fgrctd-created:

  FIND fg-rctd WHERE ROWID(fg-rctd) = tt-fgrctd-created.fg-rctd-rowid
  EXCLUSIVE-LOCK NO-ERROR.

  IF NOT AVAIL fg-rctd THEN
  NEXT.


  IF AVAIL fg-rctd THEN DO:
    IF SSPostFG-log AND
    SSPostFG-char = "Loadtag" THEN DO:
      IF AVAIL fg-rctd THEN DO:
        ASSIGN
        lv-r-no  = fg-rctd.r-no
        fg-rctd.r-no      = 0
        fg-rctd.r-no      = lv-r-no
        fg-rctd.rita-code = "R"
        fg-rctd.post-date = TODAY.
        /** not quite sure what this does
        fg-rctd.tag2      = w-fg-rctd.tag2. **/

        FOR EACH fg-rcpts WHERE
          fg-rcpts.company EQ fg-rctd.company AND
          fg-rcpts.r-no    EQ fg-rctd.r-no EXCLUSIVE-LOCK:
          fg-rcpts.rita-code = fg-rctd.rita-code.
        END.
      END.
    END.
  END.

  FIND bf-fg-rctd WHERE ROWID(bf-fg-rctd) = tt-fgrctd-created.fg-rctd-rowid
  NO-LOCK NO-ERROR.

  IF FGSetRec-Int NE 1 THEN DO:

    FOR EACH fg-rcpts WHERE
      fg-rcpts.company EQ bf-fg-rctd.company
      AND  fg-rcpts.linker     = "fg-rctd: " + STRING(bf-fg-rctd.r-no,"9999999999")
      EXCLUSIVE-LOCK:

      FIND FIRST fg-rctd WHERE fg-rctd.r-no = fg-rcpts.r-no
      EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL fg-rctd THEN DO:


        RUN oerep/r-ltpost.p (INPUT ROWID(fg-rctd),NO).
        /*##BL - FGSetAssembly requires the bin to match that of the character*/
        /*##BL of FGSetAssembly N-K.  If it doesn't, abort posting  */
        /*             IF lFGSetAssembly AND fg-rctd.loc-bin NE cFGSetAssembly THEN DO:                               */
        /*                 MESSAGE "The Bin location for Component " fg-rctd.i-no " must be " cFGSetAssembly "." SKIP */
        /*                     "Please correct on the Set Parts tab of FG Receiving and re-run the posting process."  */
        /*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                     */
        /*                 RETURN ERROR.                                                                              */
        /*             END.                                                                                           */

        FIND FIRST fg-rctd WHERE fg-rctd.r-no = fg-rcpts.r-no
        EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL fg-rctd THEN DO:

          RUN oerep/r-ltpost.p (INPUT ROWID(fg-rctd),NO).
          /*##BL - FGSetAssembly requires the bin to match that of the character*/
          /*##BL of FGSetAssembly N-K.  If it doesn't, abort posting  */
          /*             IF lFGSetAssembly AND fg-rctd.loc-bin NE cFGSetAssembly THEN DO:                               */
          /*                 MESSAGE "The Bin location for Component " fg-rctd.i-no " must be " cFGSetAssembly "." SKIP */
          /*                     "Please correct on the Set Parts tab of FG Receiving and re-run the posting process."  */
          /*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                     */
          /*                 RETURN ERROR.                                                                              */
          /*             END.                                                                                           */

          /* These negative receipts should be changed to Rita=A per task 08211305 */
          FOR EACH fg-rcpth WHERE fg-rcpth.r-no EQ fg-rctd.r-no EXCLUSIVE-LOCK:
            fg-rcpth.rita-code = "A".
          END.

          FOR EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rctd.r-no EXCLUSIVE-LOCK:
            fg-rdtlh.rita-code = "A".
          END.

        END. /* Avail fg-rctd for component */
      END. /* if avail */
    END. /* Each fg-rcpts */
  END. /* If not NoAdjustments */
  FIND fg-rctd WHERE ROWID(fg-rctd) = tt-fgrctd-created.fg-rctd-rowid
  EXCLUSIVE-LOCK NO-ERROR.

  RUN oerep/r-ltpost.p (INPUT ROWID(fg-rctd),NO).

  DELETE tt-fgrctd-created.

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-return C-Win 
PROCEDURE post-return :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM ip-fg-recid AS RECID NO-UNDO.

   DEF BUFFER bf-fg-rctd FOR fg-rctd.
   DEF BUFFER b-fg-bin FOR fg-bin.
   DEF BUFFER b-itemfg FOR itemfg.

   DEF VAR li AS INT NO-UNDO.   
   def var v-dec as dec decimals 10 NO-UNDO.
   def var v-overrun-qty like fg-rctd.qty no-undo.
   def var v-underrun-qty like fg-rctd.qty no-undo.
   DEF VAR v-reduce-qty AS INT NO-UNDO.
   DEF var v-newhdr as log no-undo. 
   def var v-fin-qty as dec no-undo.
   DEF VAR v-est-no AS cha NO-UNDO.
   DEF VAR v-one-item AS LOG NO-UNDO.
   def var ld-cvt-qty as dec no-undo.
   def var ld-cvt-cost as dec no-undo.
   DEF VAR v-binqty AS INT NO-UNDO.
   DEF VAR v-qty AS INT NO-UNDO.
   DEF VAR v-tagcost AS DEC NO-UNDO.
   DEF VAR v-cost AS DEC NO-UNDO.
   DEF VAR choice AS LOG NO-UNDO.
   DEF VAR v-post-date AS DATE INIT TODAY NO-UNDO.
   DEF VAR fg-uom-list AS cha NO-UNDO.
   DEF VAR li-tag-no AS INT NO-UNDO.
   DEF VAR ll-qty-changed AS LOG NO-UNDO.
   DEF VAR ll-whs-item AS LOG NO-UNDO.


   RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

   FOR EACH w-fg-rctd:
       DELETE w-fg-rctd.
   END.

   /* create w/h transfer record*/   
   FIND FIRST itemfg WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ loadtag.i-no NO-ERROR.

   li = 1.
   FOR EACH bf-fg-rctd NO-LOCK BY bf-fg-rctd.r-no DESC:
       LEAVE.
   END.
   IF AVAIL bf-fg-rctd then li = bf-fg-rctd.r-no.

   FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
   IF AVAIL fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.
   FIND FIRST fg-bin WHERE fg-bin.company = cocode
                       AND fg-bin.i-no = loadtag.i-no
                       AND fg-bin.tag = ""
                       AND fg-bin.qty >= loadtag.pallet-count NO-LOCK NO-ERROR.
   IF NOT AVAIL fg-bin THEN RETURN.  

   CREATE bf-fg-rctd.
   ASSIGN
       bf-fg-rctd.r-no       = li + 1
       bf-fg-rctd.rct-date   = TODAY
       bf-fg-rctd.trans-time = TIME
       bf-fg-rctd.company    = cocode
       bf-fg-rctd.rita-code  = "I"
       bf-fg-rctd.i-name     = itemfg.i-name
       bf-fg-rctd.i-no       = loadtag.i-no
       bf-fg-rctd.job-no     = loadtag.job-no
       bf-fg-rctd.job-no2    = loadtag.job-no2
       bf-fg-rctd.t-qty      = loadtag.pallet-count /*loadtag.qty*/
       bf-fg-rctd.pur-uom    = itemfg.prod-uom
       bf-fg-rctd.cost-uom   = itemfg.prod-uom
  /*     bf-fg-rctd.std-cost   = IF AVAIL fg-bin THEN fg-bin.std-tot-cost ELSE itemfg.std-tot-cost */
       bf-fg-rctd.ext-cost   = (bf-fg-rctd.t-qty / 1000) * bf-fg-rctd.std-cost
       bf-fg-rctd.qty-case   = loadtag.qty-case

       bf-fg-rctd.partial    = loadtag.partial
       bf-fg-rctd.cases      = TRUNC(bf-fg-rctd.t-qty / bf-fg-rctd.qty-case,0)
       bf-fg-rctd.cases-unit = loadtag.case-bundle
       bf-fg-rctd.loc        = loadtag.loc
       bf-fg-rctd.loc-bin    = loadtag.loc-bin
       bf-fg-rctd.tag        = loadtag.tag-no
       bf-fg-rctd.loc2        = ""
       bf-fg-rctd.loc-bin2    = ""
       bf-fg-rctd.tag2        = ""
       .
 /* post later*/
   CREATE w-fg-rctd.
   BUFFER-COPY bf-fg-rctd TO w-fg-rctd.
   ASSIGN w-fg-rctd.row-id = ROWID(bf-fg-rctd).
   {fg/fg-post.i w-fg-rctd w-fg-rctd}

   FIND CURRENT po-ordl NO-LOCK NO-ERROR.
   FIND CURRENT fg-bin NO-LOCK NO-ERROR.
   FIND CURRENT itemfg NO-LOCK NO-ERROR.

   ASSIGN bf-fg-rctd.rita-code = "P"  /* posted */
          bf-fg-rctd.post-date = v-post-date
          bf-fg-rctd.tag2      = w-fg-rctd.tag2. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-loadtg C-Win 
PROCEDURE print-loadtg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE cEmail AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPhone AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFax   AS CHARACTER NO-UNDO.

    {sys/inc/print1.i}
    {sys/inc/outprint.i value(85)}

    SESSION:SET-WAIT-STATE ("general").
   
    IF tb_print-view THEN DO:
        IF NOT lBussFormModle THEN
           PUT "<PREVIEW><MODAL=NO></PROGRESS>" FORM "x(50)".
         ELSE
           PUT "<PREVIEW></PROGRESS>" FORM "x(50)".
    END.
    ELSE DO:
       PUT "<PRINTER?><FORMAT=LEGAL></PROGRESS>" FORM "x(50)".
    END.

    DO WITH FRAME {&FRAME-NAME}:
        FOR EACH tt-word-print NO-LOCK BREAK
                                BY tt-word-print.ord-no 
                                BY tt-word-print.i-no:
                                
           IF scr-label-file:SCREEN-VALUE EQ "loadtag.xpr" THEN DO:
               {oe/rep/lodxprntstd.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag1.xpr" THEN DO:
               {oe/rep/lodxprnt.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag2.xpr" THEN DO:
               {oe/rep/lodxprnt2.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag3.xpr" THEN DO:
               {oe/rep/lodxprnt3.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag4.xpr" THEN DO:
               {oe/rep/lodxprnt4.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag5.xpr" THEN DO:
               {oe/rep/lodxprnt5.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag6.xpr" THEN DO:
               {oe/rep/lodxprnt6.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag7.xpr" THEN DO:
               {oe/rep/lodxprnt7.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag8.xpr" THEN DO:
               {oe/rep/lodxprnt8.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag9.xpr" THEN DO:
               {oe/rep/lodxprnt9.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag10.xpr" THEN DO:
               {oe/rep/lodxprnt10.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag11.xpr" THEN DO:
               {oe/rep/lodxprnt11.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag12.xpr" THEN DO:
               {oe/rep/lodxprnt12.i}
           END.
           ELSE IF scr-label-file:SCREEN-VALUE EQ "loadtag13.xpr" THEN DO:
               {oe/rep/lodxprnt13.i}
           END.
    
         IF NOT LAST(tt-word-print.i-no) THEN PAGE .
        END.
    END.


    OUTPUT CLOSE.
    SESSION:SET-WAIT-STATE ("").

    FILE-INFO:FILE-NAME = list-name.
    RUN printfile (FILE-INFO:FILE-NAME).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reprint-tag C-Win 
PROCEDURE reprint-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLoadtagFile AS CHARACTER NO-UNDO.
  
 
  FIND FIRST loadtag WHERE loadtag.company     EQ cocode
                 AND loadtag.item-type   EQ NO
                 AND loadtag.is-case-tag EQ NO
                 AND loadtag.tag-no  eq TRIM(fi_cas-lab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
  IF NOT AVAIL loadtag THEN DO:
      MESSAGE "Invalid Loadtag. Try Help." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fi_cas-lab.
      RETURN ERROR.
  END.  

  ASSIGN
      cBarCodeProgram = IF scr-label-file MATCHES "*.xpr*" THEN "xprint" 
                        ELSE IF scr-label-file MATCHES "*.lwl" THEN "loftware" 
                        ELSE "".
  RUN create-w-ord.

  SESSION:SET-WAIT-STATE ("general").

  {sys/inc/print1.i}
  {sys/inc/outprint.i value(lines-per-page)} 
      VIEW FRAME r-top.
      VIEW FRAME top.

  IF cBarCodeProgram EQ 'Loftware' then 
    cLoadtagFile = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(TIME) + SUBSTRING(STRING(NOW),21,3) + '.csv'.
  ELSE cLoadtagFile = 'loadtag.txt'.
  IF v-out = "" THEN v-out = "c:~\ba~\label~\" + cLoadtagFile.
  ELSE do:
     IF SUBSTRING(v-out,LENGTH(v-out),1) = "/" OR
        SUBSTRING(v-out,LENGTH(v-out),1) = "\" THEN .
     ELSE v-out = v-out + "/".
     v-out = v-out + cLoadtagFile.
  END.

  RUN create-text-file.

  IF cBarCodeProgram EQ "" THEN do:    
      RUN AutoPrint.
  END.
  ELSE IF cBarCodeProgram EQ "xprint" AND scr-auto-print THEN do:
      PAUSE 1.
      RUN print-loadtg  .
  END.

  IF (NOT is-from-addons() OR SSLoadTag-log = TRUE) THEN 
      MESSAGE "Loadtag reprint is completed." VIEW-AS ALERT-BOX INFORMATION.
  SESSION:SET-WAIT-STATE ("").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-barone C-Win 
PROCEDURE run-barone :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM ip-TagText AS cha NO-UNDO.

   DEFINE VARIABLE iReturnResult AS INTEGER NO-UNDO.
   DEFINE VARIABLE cProgramName AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

   cProgramName =  "c:\program files\bar-one 6 pro-plus\labels.exe ".
   cFileName    = "R:\ASI_GUI9\SOURCE\custom\century.lab".


  /* OS-COPY VALUE(ip-tagtext) VALUE("r:\asi_gui9\source\custom\"). */
   RUN WinExec (INPUT cProgramName + CHR(32) + cFileName , INPUT 1, OUTPUT
   iReturnResult).
/*
   IF iReturnResult >= 32 THEN
     MESSAGE "Application was Started" VIEW-AS ALERT-BOX.
   ELSE
     MESSAGE "Application Failed:" iReturnResult VIEW-AS ALERT-BOX.

*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-lmw C-Win 
PROCEDURE run-lmw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM ip-TagText AS cha NO-UNDO.

   DEFINE VARIABLE iReturnResult AS INTEGER NO-UNDO.
   DEFINE VARIABLE cProgramName AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

/*   cProgramName =  "c:\program files\bar-one 6 pro-plus\labels.exe ".
*/
   cFileName    = "custom\interpack.qdf".
   FILE-INFO:FILE-NAME = cFileName.
   cFileName = FILE-INFO:FULL-PATHNAME.

   RUN custom/runlmw.p (OUTPUT cprogramname).

/*   OS-COPY VALUE(ip-tagtext) VALUE("c:\tmp\").*/

   RUN WinExec (INPUT cProgramName + CHR(32) + cFileName , INPUT 1, OUTPUT
   iReturnResult).
/*
   IF iReturnResult >= 32 THEN
     MESSAGE "Application was Started" VIEW-AS ALERT-BOX.
   ELSE
     MESSAGE "Application Failed:" iReturnResult VIEW-AS ALERT-BOX.

*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
{oerep/r-loadtg1.i}

IF cBarCodeProgram EQ "" THEN DO:    
    RUN AutoPrint.
END.
ELSE IF cBarCodeProgram EQ "xprint" AND scr-auto-print THEN do: 
    PAUSE 1.
    RUN print-loadtg .
END.
SESSION:SET-WAIT-STATE ("").
/*     IF scr-auto-print THEN                                                  */
/*     DO:                                                                     */
/*        DEF VAR v-int AS INT NO-UNDO.                                        */
/*        DEF VAR cFileName AS CHAR NO-UNDO.                                   */
/*        DEF VAR v-path AS CHARACTER NO-UNDO.                                 */
/*                                                                             */
/*        LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".                       */
/*        USE "SOFTWARE".                                                      */
/*        GET-KEY-VALUE SECTION "Teklynx\Label Matrix"                         */
/*                      KEY "PATH"                                             */
/*                      VALUE v-path.                                          */
/*        UNLOAD "SOFTWARE".                                                   */
/*                                                                             */
/*        ASSIGN                                                               */
/*           v-path = v-path + "\lmwprint.exe "                                */
/*           cFileName = "/L=" + scr-label-file.                               */
/*                                                                             */
/*           RUN WinExec (INPUT v-path + CHR(32) + cFileName , INPUT 1, OUTPUT */
/*                        v-int).                                              */
/*     END.                                                                    */


end procedure.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-create C-Win 
PROCEDURE temp-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  CREATE w-file.
  w-key = ip-rowid.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-job C-Win 
PROCEDURE temp-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-job-no LIKE job.job-no NO-UNDO.

  FOR EACH job
      WHERE job.company EQ cocode
        AND job.job-no  EQ SUBSTR(ip-job-no,1,6)
        AND job.job-no2 EQ INT(SUBSTR(ip-job-no,7,2))
        AND (v-stat EQ "A"                      OR
             (v-stat EQ "C" AND NOT job.opened) OR
             (v-stat EQ "O" AND job.opened))
      NO-LOCK:

    RUN temp-create (ROWID(job)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-ord C-Win 
PROCEDURE temp-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-ord-no LIKE oe-ord.ord-no NO-UNDO.

  FOR EACH oe-ord
      WHERE oe-ord.company EQ cocode
        AND oe-ord.ord-no  EQ ip-ord-no
        AND (v-stat EQ "A"                         OR
             (v-stat EQ "C" AND NOT oe-ord.opened) OR
             (v-stat EQ "O" AND oe-ord.opened))
      NO-LOCK:
    RUN temp-create (ROWID(oe-ord)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-po C-Win 
PROCEDURE temp-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-po-no LIKE po-ord.po-no NO-UNDO.

  FOR EACH po-ord NO-LOCK
      WHERE po-ord.company EQ cocode
        AND po-ord.po-no   EQ ip-po-no
        AND (v-stat EQ "A"                         OR
             (v-stat EQ "C" AND NOT po-ord.opened) OR
             (v-stat EQ "O" AND po-ord.opened)):
    RUN temp-create (ROWID(po-ord)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE win_breakPath C-Win 
PROCEDURE win_breakPath :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input    param pcPath            as char no-undo.
  define output   param pcProtocol        as char no-undo.
  define output   param pcComputerName    as char no-undo.
  define output   param pcSharedFolder    as char no-undo.
  define output   param pcDrive           as char no-undo.
  define output   param pcDir             as char no-undo.
  define output   param pcFile            as char no-undo.
  define output   param pcExt             as char no-undo.

    define var i as int no-undo.

    assign
        pcProtocol      = ""
        pcComputerName  = ""
        pcSharedFolder  = ""
        pcDrive         = ""
        pcDir           = ""
        pcFile          = ""
        pcExt           = "".

    /* assumes that if the call is from another procedure or function with in this library then the path has already been normalized. */

    if pcPath = ? then
        return.

    if source-procedure <> this-procedure then
       pcPath = win_normalizePath( pcPath ).

    if pcPath begins "~\~\" then do:

        assign
            pcProtocol = substr( pcPath, 1, 2 )
            substr( pcPath, 1, 2 ) = "".

        i = index( pcPath, "~\", 3 ).
        if i = 0 then i = length( pcPath ) + 1.

        assign
            pcComputerName = substr( pcPath, 1, i - 1 )
            substr( pcPath, 1, i - 1 ) = "".

        i = index( pcPath, "~\", 2 ).
        if i = 0 then i = length( pcPath ) + 1.

        assign
            pcSharedFolder = substr( pcPath, 1, i - 1 )
            substr( pcPath, 1, i - 1 ) = "".

    end. /* pcPath begins "\\" */

    else
    if  substr( pcPath, 1, 1 ) >= "a"
    and substr( pcPath, 1, 1 ) <= "z"
    and substr( pcPath, 2, 1 )  = ":" then do:

        assign
            pcDrive = substr( pcPath, 1, 2 )
            substr( pcPath, 1, 2 ) = "".

    end. /* else */

    i = r-index( pcPath, "~\" ). 
    if i > 0 then

    assign
        pcDir = substr( pcPath, 1, i )
        substr( pcPath, 1, i ) = "".

    i = r-index( pcPath, "." ).
    if i > 0 then

    assign
        pcExt = substr( pcPath, i )
        substr( pcPath, i, length( pcExt ) ) = "".

    pcFile = pcPath.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE write-loadtag-line C-Win 
PROCEDURE write-loadtag-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ipc-rfid AS CHAR NO-UNDO.
 DEF INPUT PARAMETER ipc-totalUnit AS CHAR NO-UNDO.
 DEF INPUT PARAMETER ipi-pallet-id AS INT NO-UNDO.
 DEF INPUT PARAMETER ipi-counter AS INT NO-UNDO.
 DEF VAR cLoftString AS CHAR NO-UNDO.
 DEF VAR iCtr AS INT NO-UNDO.

  IF cBarCodeProgram EQ "Loftware" THEN DO:
      
    /* HRMS Loftware Map */
        /*01  00000000.lwl = tag template   */
        /*02  Customer Name   30 characters   Customername                        */
        /*03  Customer Part ID or Number  30 characters   custpartid              */
        /*04  Customer PO Number  15 characters   Custpo                          */
        /*05  Customer Ship To City & State   30 characters   custshiptocityst    */
        /*06  File (Estimate) number  7 characters    filenumber                  */
        /*07  Lbs/M Pieces  - cust waste  6 characters    LbsperM                 */
        /*08  Plant Number    2 characters    plantnumber                         */
        /*09  this field contains “- -“   3 characters    Text0098                */
        /*10  Number of tags per unit 1 character **QUANTITY                      */
        /*11  Number of tags to print 5 characters    tagstoprint                 */
        /*12  Order Number    7 characters    ordernumber                         */
        /*13  Quantity per pallet 6 characters    qtypallet                       */
        /*14  Due date in format MM/DD/YY 8 characters    DueDate                 */
        /*15  Order Quantity  9 characters    Orderqty                            */
        /*16  Overrun Percent (% )    2 characters    Overrun                     */
        /*17  Underrun Percent (%)    2 characters    underrun                    */
        /*18  Critical Operation #1   2 characters    C01                         */
        /*19  Critical Operation #2   2 characters    C02                         */
        /*20  Critical Operation #3   2 characters    C03                         */
        /*21  Critical Operation #4   2 characters    C04                         */
        /*22  Critical Operation #5   2 characters    C05                         */
        /*23  Critical Operation #6   2 characters    C06                         */
        /*24  Customer and ShipTo Number in the format NNNNNSSS where 
                NNNNN = Customer Number and SSS = ShipTo Number 8 characters    
                custshiptonumber                                                  */
        /*25  B/L instructions        B/L                                         */
        /*26  RECORD      Record                                                  */
        /*27  Load(Unit)Number – Unitized Inventory not active 5 chars loadnumber */
        /*28  Number of bands (strap pattern) 6 characters    Straps              */
        /*29  Unit type   1 character unittype                                    */
        /*30  Customer ShipTo Name    60 characters   custshiptoname              */
        /*31  Unit ID (Serial number) 23 characters   UnitID                      */
        /*32  Material description    30 characters   matldesc                    */
        /*33  Sheet width 9 characters    shtwdth                                 */
        /*34  Sheet length    9 characters    shtleng.                            */
        /*35  Load Number - Unitized Inventory active   5 characters  Loadnumber  */
        /*36  Broker’s Customer Name  30 characters   brokercust                  */
        /*37  Customer ShipTo Address 30 characters   custshiptoadd               */
        /*38  Number of tags to print 3 characters    numbertagstoprint           */
        /*39  Number per bundle   5 characters    numberperbndl                   */
        /*40  Broker number   5 characters    brokernumber                        */
        /*41  Order entry message line 7  64 characters   msg7                    */
        /*42  Order entry message line 8  64 characters   msg8                    */
        /*43  Order entry message line 9  64 characters   msg9                    */
        /*44  Style description   15 characters   styledesc                       */
        /*45  Box length  9 characters    boxlen                                  */
        /*46  Box width   9 characters    boxwid                                  */
        /*47  Box depth   9 characters    boxdep                                  */
        /*48  Corrugator Scoring 1    6 characters    corrscore1                  */
        /*49  Corrugator Scoring 2    6 characters    corrscore2                  */
        /*50  Corrugator Scoring 3    6 characters    corrscore3                  */
        /*51  Corrugator Scoring 4    6 characters    corrscore4                  */
        /*52  Corrugator Scoring 5    6 characters    corrscore5                  */
        /*53  Corrugator Scoring 6    6 characters    corrscore6                  */
        /*54  Corrugator Scoring 7    6 characters    corrscore7                  */
        /*55  Corrugator Scoring 8    6 characters    corrscore8                  */
        /*56  Corrugator Scoring 9    6 characters    corrscore9                  */
        /*57  Cutoff Scoring 1    6 characters    cutscore1                       */
        /*58  Cutoff Scoring 2    6 characters    cutscore2                       */
        /*59  Cutoff Scoring 3    6 characters    cutscore3                       */
        /*60  Cutoff Scoring 4    6 characters    cutscore4                       */
        /*61  Cutoff Scoring 5    6 characters    cutscore5                       */
        /*62  Cutoff Scoring 6    6 characters    cutscore6                       */
        /*63  Cutoff Scoring 7    6 characters    cutscore7                       */
        /*64  Cutoff Scoring 8    6 characters    cutscore8                       */
        /*65  Cutoff Scoring 9    6 characters    cutscore9                       */
        /*66  Not used at this time   1 character Unused1                         */
        /*67  Number of ties  4 characters    numties                             */
        /*68  Number of bundles per layer 4 characters    numbdllyr               */
        /*69  Stacking pattern    10 characters   stackptn                        */
        /*70  Number of layers    4 characters    Numlayers                       */
        /*71  Dimensions  15 characters   dimensions                              */
        /*72  Cutting die number  10 characters   cuttingdie                      */
        /*73  Cutting die location    10 characters   cuttingdielocn              */
        /*74  Printing die number 10 characters   printingdie                     */
        /*75  Printing die location   10 characters   printdielocn                */
        /*76  1st down color  10 characters   color1                              */
        /*77  2nd down color  10 characters   color2                              */
        /*78  3rd down color  10 characters   color3                              */
        /*79  4th down color  10 characters   color4                              */
        /*80  Critical Operation 1 text   5 characters    LC01                    */
        /*81  Critical Operation 2 text   5 characters    LC02                    */
        /*82  Critical Operation 3 text   5 characters    LC03                    */
        /*83  Critical Operation 4 text   5 characters    LC04                    */
        /*84  Critical Operation 5 text   5 characters    LC05                    */
        /*85  Critical Operation 6 text   5 characters    LC06                    */
        /*86  1st Miscellaneous Billing Message   18 characters   MiscMsg1        */
        /*87  2nd Miscellaneous Billing Message   18 characters   MiscMsg2        */
    
    ASSIGN
        cLoftString = FILL(",",86)
        ENTRY(1,cLoftString) = "!" + scr-label-file
        ENTRY(2,cLoftString) = '"' + STRING(REPLACE(w-ord.cust-name,'"',""),"x(30)") + '"'
        ENTRY(3,cLoftString) = '"' + STRING(REPLACE(w-ord.cust-part-no,'"',""),"x(30)") + '"'
        ENTRY(4,cLoftString) = '"' + STRING(REPLACE(w-ord.cust-po-no,'"',""),"x(15)") + '"'
        ENTRY(5,cLoftString) = '"' + STRING(REPLACE(w-ord.ship-city,'"',"") + " " + REPLACE(w-ord.ship-state,'"',""),"x(30)") + '"'
        ENTRY(6,cLoftString) = '"' + STRING(w-ord.est-no,"9999999") + '"'
        ENTRY(7,cLoftString) = STRING(w-ord.gross-wt,">>>>9.99")
        ENTRY(9,cLoftString) = "- -"
        ENTRY(10,cLoftString) = "1"
        ENTRY(11,cLoftString) = "1"
        ENTRY(12,cLoftString) = STRING(w-ord.ord-no,">>>>>>9")
        ENTRY(13,cLoftString) = STRING(w-ord.total-unit,">>>>>9")
        ENTRY(14,cLoftString) = STRING(w-ord.due-date,"99/99/99")
        ENTRY(24,cLoftString) = '"' + STRING(REPLACE(w-ord.cust-no,'"',""),"x(5)") + STRING(REPLACE(w-ord.ship-code,'"',""),"x(3)") + '"'
        ENTRY(30,cLoftString) = '"' + STRING(REPLACE(w-ord.ship-name,'"',""),"x(60)") + '"'
        ENTRY(31,cLoftString) = '"' + STRING(REPLACE(w-ord.i-no,'"',""),"x(23)") + '"'
        ENTRY(32,cLoftString) = '"' + STRING(REPLACE(w-ord.i-name,'"',""),"x(30)") + '"'
        ENTRY(37,cLoftString) = '"' + STRING(REPLACE(w-ord.ship-add1,'"',"") + " " + REPLACE(w-ord.ship-add2,'"',""),"x(30)") + '"'
        ENTRY(38,cLoftString) = "1"
        ENTRY(39,cLoftString) = STRING(w-ord.pcs,">>>>9")
        ENTRY(41,cLoftString) = STRING(loadtag.tag-no,"x(64)")
        ENTRY(42,cLoftString) = '"' + STRING(REPLACE(w-ord.ord-desc1,'"',""),"x(64)") + '"'
        ENTRY(44,cLoftString) = '"' + STRING(REPLACE(w-ord.style-desc,'"',""),"x(15)") + '"'
        ENTRY(45,cLoftString) = STRING(w-ord.box-len,">>>9.99<<<")
        ENTRY(46,cLoftString) = STRING(w-ord.box-wid,">>>9.99<<<")
        ENTRY(47,cLoftString) = STRING(w-ord.box-dep,">>>9.99<<<")
        ENTRY(68,cLoftString) = STRING(w-ord.bundle,"x(4)")
         .
    
    PUT UNFORMATTED  
        cLoftString.
 END.
 ELSE DO:

    PUT UNFORMATTED 
        "~""  removeChars(w-ord.cust-name)  "~","
        w-ord.ord-no  ","
        "~""  v-job  "~","
        "~""  caps(removeChars(w-ord.i-no))  FORM "x(15)" "~","
        "~""  removeChars(w-ord.cust-part-no) "~","
        "~""  removeChars(w-ord.cust-po-no)  "~","
        w-ord.pcs  ","
        w-ord.bundle  ","
        trim(ipc-totalUnit) ","
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
		"~""  w-ord.upc-no FORMAT "x(20)" "~","
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
 
    IF CAN-DO("ASI,SSLABEL",v-loadtag) THEN DO:
        PUT UNFORMATTED 
            "~"" SUBSTR(loadtag.tag-no,16,5) "~"," 
            "~"" ipc-rfid "~"," .
    END.

    PUT UNFORMATTED 
        "~"" w-ord.due-date-jobhdr "~"," 
        "~"" w-ord.due-date-job "~","
        "~"" w-ord.linenum "~","
        "~"" w-ord.unit-wt  "~","
        "~"" w-ord.pallt-wt  "~","          
        "~"" removeChars(v-fgdsc1) "~","
        "~"" removeChars(v-fgdsc2) "~","
        "~"" removeChars(v-fgdsc3) "~","
        "~"" removeChars(w-ord.lot) "~","
        "~"" w-ord.pallt-no "~"," 
        "~"" ipi-pallet-id "~","
        "~"" ipi-counter "~","
        "~"" w-ord.total-tags "~","
        "~"" replace(w-ord.ship-notes[1],'"', '') "~","
        "~"" replace(w-ord.ship-notes[2],'"', '') "~","
        "~"" replace(w-ord.ship-notes[3],'"', '') "~","
        "~"" replace(w-ord.ship-notes[4],'"', '') "~","
        "~"" loadtag.loc "~","
        "~"" loadtag.loc-bin "~""
        .
    
    IF lSSCC THEN PUT UNFORMATTED ",~"" w-ord.sscc "~"".
END.

PUT UNFORMATTED SKIP.

/* temp table for xprint */
IF cBarCodeProgram EQ "xprint" THEN do:
    CREATE tt-word-print .
    BUFFER-COPY w-ord TO tt-word-print .
    ASSIGN 
        tt-word-print.tag-no = loadtag.tag-no .
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION win_normalizePath C-Win 
FUNCTION win_normalizePath RETURNS CHARACTER
  ( pcPath AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    define var cPath    as char no-undo.
    define var cRoot    as char no-undo.
    define var cDir     as char no-undo.
    define var iDir     as int no-undo.

    define var str      as char no-undo.
    define var i        as int no-undo.

    pcPath = trim( pcPath ).

    if pcPath = ""
    or pcPath = ? then
        return pcPath.

    pcPath = replace( pcPath, "/", "~\" ).

    do while index( pcPath, "~\~\", 2 ) <> 0:
        substr( pcPath, 2, length( pcPath ) - 1 ) = replace( substr( pcPath, 2, length( pcPath ) - 1 ), "~\~\", "~\" ).
    end.

    do while index( pcPath, "::" ) <> 0:
        pcPath = replace( pcPath, "::", ":" ).
    end.

    if lookup( ".", pcPath, "~\" ) > 0 or lookup( "..", pcPath, "~\" ) > 0 then do:

        assign
            cRoot = ""
            cPath = "".

        if pcPath begins "~\~\" then do:

            i = index( pcPath, "~\", 3 ).
            if i = 0 then i = length( pcPath ).

            assign
                cRoot = substr( pcPath, 1, i )
                substr( pcPath, 1, i ) = "".

            i = index( pcPath, "~\" ). 
            if i > 0 then

            assign
                cRoot = cRoot + substr( pcPath, 1, i )
                substr( pcPath, 1, i ) = "".

        end. /* pcPath begins "\\" */

        else
        if  substr( pcPath, 1, 1 ) >= "a"
        and substr( pcPath, 1, 1 ) <= "z"
        and substr( pcPath, 2, 1 )  = ":" then do:

            assign
               cRoot = substr( pcPath, 1, 2 )
               substr( pcPath, 1, 2 ) = "".

            if substr( pcPath, 1, 1 ) = "~\" then
            assign
               cRoot = cRoot + substr( pcPath, 1, 1 )
               substr( pcPath, 1, 1 ) = "".

        end. /* substr = ":" */



        do iDir = 1 to num-entries( pcPath, "~\" ):

            cDir = entry( iDir, pcPath, "~\" ).

            if cDir = "." then do:

                if cPath <> "" or cRoot <> "" then
                    next.

                else
                cPath = cPath
                      + ( if cPath <> "" then "~\" else "" )
                      + cDir.

            end. /* cDir = "." */

            else
            if cDir = ".." then do:

                if cPath <> "" and entry( num-entries( cPath, "~\" ), cPath, "~\" ) <> ".." then do:

                    str = "".

                    do i = 1 to num-entries( cPath, "~\" ) - 1:

                        str = str
                            + ( if str <> "" then "~\" else "" )
                            + entry( i, cPath, "~\" ).

                    end. /* 1 to num-entries */

                    cPath = str.

                end. /* else */

                else
                cPath = cPath
                      + ( if cPath <> "" then "~\" else "" )
                      + cDir.

            end. /* cDir = ".." */

            else
            cPath = cPath
                  + ( if cPath <> "" then "~\" else "" )
                  + cDir.

        end. /* 1 to num-entries */

        pcPath = cPath.

        if cRoot <> "" then
            pcPath = cRoot + pcPath.

    end. /* lookup( ".." ) > 0 */

    return pcPath.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

