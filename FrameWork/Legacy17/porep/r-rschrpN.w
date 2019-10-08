&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: porep\r-schrpt.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  DefINITions  ************************** */

/* Parameters DefINITions ---                                           */

/* Local Variable DefINITions ---                                       */
DEF VAR list-name AS CHAR NO-UNDO.
DEFINE VARIABLE INIT-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = gcompany
 locode = gloc.


DEF NEW SHARED VAR v-s-vend LIKE vend.vend-no INIT "".
DEF NEW SHARED VAR v-e-vend LIKE vend.vend-no INIT "zzzzzzzz".
DEF NEW SHARED VAR v-s-date LIKE po-ord.po-date INIT TODAY FORMAT "99/99/9999".
DEF NEW SHARED VAR v-e-date LIKE v-s-date INIT 12/31/9999.
DEF NEW SHARED VAR v-po-stat LIKE po-ord.stat INIT "O".

DEF TEMP-TABLE tt-sched NO-UNDO
    FIELD job-no LIKE po-ordl.job-no
    FIELD job-no2 LIKE po-ordl.job-no2
    FIELD i-no LIKE po-ordl.i-no
    FIELD i-name LIKE po-ordl.i-name
    FIELD vend-no LIKE po-ord.vend-no
    FIELD po-no LIKE po-ordl.po-no
    FIELD ord-no LIKE oe-ordl.ord-no
    FIELD po-date LIKE po-ord.po-date
    FIELD cons-uom LIKE po-ordl.cons-uom
    FIELD cons-qty LIKE po-ordl.cons-qty
    FIELD t-rec-qty LIKE po-ordl.t-rec-qty
    FIELD due-date LIKE po-ordl.due-date
    FIELD amt-msf LIKE ap-invl.amt-msf
    FIELD vend-name LIKE vend.name
    FIELD carrier LIKE po-ord.carrier
    FIELD loc-bin LIKE fg-rctd.loc-bin
    FIELD rct-date LIKE fg-rctd.rct-date
    FIELD rec_key LIKE po-ordl.rec_key
    INDEX job job-no job-no2
    INDEX i-no i-no
    INDEX vend vend-no.

DEF TEMP-TABLE tt-fgs NO-UNDO
    FIELD sched-rowid AS ROWID
    FIELD i-no LIKE po-ordl.i-no
    FIELD i-name LIKE po-ordl.i-name
    FIELD cust-no LIKE po-ord.cust-no
    FIELD cust-name LIKE po-ordl.i-name
    FIELD ord-no LIKE po-ordl.ord-no
    FIELD ord-qty LIKE po-ordl.ord-qty
    FIELD due-date LIKE po-ordl.due-date
    FIELD job-no LIKE job.job-no
    INDEX i1 sched-rowid.

DEF VAR v-print-fmt AS CHARACTER.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR stat-list AS CHAR NO-UNDO.
DEF VAR v-len LIKE po-ordl.s-len NO-UNDO.
DEF VAR v-wid LIKE po-ordl.s-wid NO-UNDO.
DEF VAR v-bwt LIKE item.basis-w NO-UNDO.
DEF VAR v-dep LIKE po-ordl.s-wid NO-UNDO.
DEF VAR v-qty AS DEC NO-UNDO.
DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR v-s-num LIKE po-ordl.s-num INIT 1 NO-UNDO.
DEF VAR v-start-rcv-date AS DATE NO-UNDO.
DEF VAR v-end-rcv-date AS DATE NO-UNDO.
DEF VAR v-show-posted AS LOG NO-UNDO.
DEF VAR v-rcv-date-selected AS LOG NO-UNDO.
DEF VAR v-tot AS DEC NO-UNDO.
DEF VAR v-ord-qty AS DEC NO-UNDO.
DEF VAR v-sort AS CHAR FORMAT "x" INIT "J" NO-UNDO.
DEF VAR lv-job-no AS CHAR NO-UNDO.

DEF VAR lv-uom AS CHAR NO-UNDO.
def var v-mattype-list          as   char format "x(36)" NO-UNDO.
def var v-mat-dscr              as   char format "x(20)" extent 21 NO-UNDO.
DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.
DEF VAR cColumnInit AS LOG INIT YES NO-UNDO.


ASSIGN cTextListToSelect = "VENDOR #,VENDOR NAME,ITEM NO,FG ITEM,BIN,ITEM NAME,ON ORDER FOR,P/O#,ORDER#,"
                         + "P/O DATE,QTY ORDER,QTY RECEIVED,REQ DATE,CARRIER,"
                         + "JOB NO,SIZE,UOM,CUSTOMER" 
       cFieldListToSelect = "vend,vend-name,i-no,fg-itm,bin,i-name,cust-nam,po,ord," +
                            "po-dt,qty-ord,qty-rcv,rfq-dt,carr," +
                            "job-no,size,uom,cust"
       cFieldLength = "8,30,15,15,8,30,30,8,8," + "8,15,15,8,7," + "10,20,3,8"
       cFieldType = "c,c,c,c,c,c,c,c,c," + "c,i,i,c,c," + "c,c,c,c"  
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "VENDOR #,VENDOR NAME,ITEM NO,FG ITEM,BIN,ITEM NAME,ON ORDER FOR,P/O#,ORDER#,"
                         + "P/O DATE,QTY ORDER,QTY RECEIVED,REQ DATE,CARRIER,"
                         + "JOB NO,SIZE,UOM,CUSTOMER"
                                                     .
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_vend-no end_vend-no ~
begin_due-date end_due-date begin_procat end_procat begin_cat end_cat ~
tg_receipts begin_receipt-date end_receipt-date rd_show select-mat rd_print ~
tb_late tb_printNotes rd-dest lines-per-page td-show-parm tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel btn_SelectColumns
&Scoped-Define DISPLAYED-OBJECTS begin_vend-no end_vend-no begin_due-date ~
end_due-date begin_procat end_procat begin_cat end_cat tg_receipts ~
begin_receipt-date end_receipt-date lbl_show rd_show select-mat lbl_print ~
rd_print tb_late mat-types tb_printNotes rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE )  FORWARD.

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

DEFINE BUTTON btn_SelectColumns 
     LABEL "Select Columns" 
     SIZE 43 BY 1.19.

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning FG Category" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_due-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning PO Due Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning RM Category" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_receipt-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending FG Category" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_due-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending PO Due Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending RM Category" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_receipt-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend-no AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-rschrp.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_print AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_show AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

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

DEFINE VARIABLE mat-types AS CHARACTER FORMAT "X(256)":U 
     LABEL "Material Types" 
     VIEW-AS FILL-IN 
     SIZE 1 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "L" 
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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_print AS CHARACTER INITIAL "Job" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Job", "Job",
"Item", "Item",
"Vendor", "Vendor"
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE rd_show AS CHARACTER INITIAL "Open" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed (Not Received)", "Closed (Not Received)",
"All PO's", "All PO's"
     SIZE 52 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 14.05.

DEFINE VARIABLE select-mat AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 28 BY 4.52 NO-UNDO.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_late AS LOGICAL INITIAL no 
     LABEL "Print Late Line Items Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE tb_printNotes AS LOGICAL INITIAL no 
     LABEL "Print Notes" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg_receipts AS LOGICAL INITIAL no 
     LABEL "Show posted receipts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_vend-no AT ROW 2.67 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend-no AT ROW 2.67 COL 69 COLON-ALIGNED HELP
          "Enter Ending Vendor number"
     begin_due-date AT ROW 3.67 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Due Date"
     end_due-date AT ROW 3.67 COL 69 COLON-ALIGNED HELP
          "Enter ending Due Date"
     begin_procat AT ROW 4.67 COL 28 COLON-ALIGNED
     end_procat AT ROW 4.67 COL 69 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_cat AT ROW 5.67 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Category"
     end_cat AT ROW 5.67 COL 69 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     tg_receipts AT ROW 6.71 COL 30 WIDGET-ID 2
     begin_receipt-date AT ROW 7.67 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Due Date" WIDGET-ID 4
     end_receipt-date AT ROW 7.67 COL 69 COLON-ALIGNED HELP
          "Enter ending Due Date" WIDGET-ID 6
     lbl_show AT ROW 9.33 COL 3 NO-LABEL
     rd_show AT ROW 9.33 COL 12 NO-LABEL
     select-mat AT ROW 10.29 COL 68 NO-LABEL
     lbl_print AT ROW 10.38 COL 3 NO-LABEL
     rd_print AT ROW 10.38 COL 11 NO-LABEL
     tb_late AT ROW 11.33 COL 15
     mat-types AT ROW 11.33 COL 63 COLON-ALIGNED
     tb_printNotes AT ROW 12.29 COL 15
     btn_SelectColumns AT ROW 13.48 COL 15 WIDGET-ID 10
     sl_avail AT ROW 16 COL 3 NO-LABEL WIDGET-ID 26
     rd-dest AT ROW 16.24 COL 7 NO-LABEL
     lv-ornt AT ROW 16.48 COL 32 NO-LABEL
     lines-per-page AT ROW 16.48 COL 85 COLON-ALIGNED
     sl_selected AT ROW 17.57 COL 15.8 NO-LABEL WIDGET-ID 28
     lv-font-no AT ROW 18.38 COL 36 COLON-ALIGNED
     lv-font-name AT ROW 19.33 COL 30 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 20.52 COL 32
     tb_excel AT ROW 21.95 COL 52 RIGHT-ALIGNED
     tb_runExcel AT ROW 21.95 COL 73 RIGHT-ALIGNED
     fi_file AT ROW 22.76 COL 30 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 24.33 COL 26
     btn-cancel AT ROW 24.33 COL 56
     "Select/Deselect RM Types" VIEW-AS TEXT
          SIZE 31 BY 1 AT ROW 9.1 COL 64
          FONT 6
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.29 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.48 COL 6
          BGCOLOR 2 
     RECT-6 AT ROW 15.05 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.4 BY 25.


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
         TITLE              = "Scheduled Receipts with Orders"
         HEIGHT             = 25.14
         WIDTH              = 96.8
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

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
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
       begin_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_receipt-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_receipt-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_print IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       lbl_print:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_print".

/* SETTINGS FOR FILL-IN lbl_show IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       lbl_show:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_show".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mat-types IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       mat-types:HIDDEN IN FRAME FRAME-A           = TRUE
       mat-types:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       sl_avail:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_selected IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       sl_selected:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       rd_print:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_show:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       select-mat:AUTO-RESIZE IN FRAME FRAME-A      = TRUE.

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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Scheduled Receipts with Orders */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Scheduled Receipts with Orders */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cat C-Win
ON LEAVE OF begin_cat IN FRAME FRAME-A /* Beginning FG Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_due-date C-Win
ON LEAVE OF begin_due-date IN FRAME FRAME-A /* Beginning PO Due Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat C-Win
ON LEAVE OF begin_procat IN FRAME FRAME-A /* Beginning RM Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_receipt-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_receipt-date C-Win
ON LEAVE OF begin_receipt-date IN FRAME FRAME-A /* Beginning Receipt Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no C-Win
ON LEAVE OF begin_vend-no IN FRAME FRAME-A /* Beginning Vendor# */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEF VAR v-valid AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  RUN GetSelectionList.
  run run-report(OUTPUT v-valid). 
  STATUS DEFAULT "Processing Complete".

  IF v-valid THEN
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_vend-no
                            &END_cust=END_vend-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust= begin_vend-no
                             &END_cust=end_vend-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust= begin_vend-no
                                  &END_cust=end_vend-no
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn_SelectColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_SelectColumns C-Win
ON CHOOSE OF btn_SelectColumns IN FRAME FRAME-A /* Select Columns */
DO:
    DEF VAR cTextSelected AS cha NO-UNDO.
    DEF VAR cTextListed AS cha NO-UNDO.

    RUN displaySelectionList2.

    ASSIGN cTextSelected = sl_selected:LIST-ITEMS
           cTextListed = sl_avail:LIST-ITEMS.

    IF NOT cColumnInit THEN RUN custom/d-rptsel.w (INPUT-OUTPUT cTextListed, INPUT-OUTPUT cTextSelected, INPUT-OUTPUT cTextListToDefault, INPUT-OUTPUT cTextListToSelect).

    ASSIGN sl_selected:LIST-ITEMS = cTextSelected
           sl_avail:LIST-ITEMS = cTextListed.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME end_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cat C-Win
ON LEAVE OF end_cat IN FRAME FRAME-A /* Ending FG Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_due-date C-Win
ON LEAVE OF end_due-date IN FRAME FRAME-A /* Ending PO Due Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_procat C-Win
ON LEAVE OF end_procat IN FRAME FRAME-A /* Ending RM Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_receipt-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_receipt-date C-Win
ON LEAVE OF end_receipt-date IN FRAME FRAME-A /* Ending Receipt Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-no C-Win
ON LEAVE OF end_vend-no IN FRAME FRAME-A /* Ending Vendor# */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME rd_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_print C-Win
ON VALUE-CHANGED OF rd_print IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_show C-Win
ON VALUE-CHANGED OF rd_show IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail C-Win
ON DEFAULT-ACTION OF sl_avail IN FRAME FRAME-A
DO:

   IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) OR
       sl_selected:NUM-ITEMS = 0)
   THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
               ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
              /* sl_selected:SCREEN-VALUE = sl_selected:ENTRY(sl_selected:NUM-ITEMS) */
               .


/* for pairs
    DEF VAR cSelectedList AS cha NO-UNDO.
    cSelectedList = sl_Selected:LIST-ITEM-PAIRS.
    DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
    IF sl_avail:IS-SELECTED(i) AND
      (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR
         sl_selected:NUM-ITEMS = 0) THEN
    /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
        cSelectedList = cSelectedList +
                        entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
    MESSAGE i sl_avail:IS-SELECTED(i) NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i))
        sl_selected:NUM-ITEMS
        SKIP cSelectedList
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
  sl_selected:LIST-ITEM-PAIRS = cSelectedList.
  sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected C-Win
ON DEFAULT-ACTION OF sl_selected IN FRAME FRAME-A
DO:
   DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN DO:
       ASSIGN ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
              ldummy = /*{&SELF-NAME}:DELETE(i)*/
                       {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
              .
    END.           
  END.
  IF {&SELF-NAME}:NUM-ITEMS NE 0 THEN
  ASSIGN
    {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(1)
    .


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


&Scoped-define SELF-NAME tb_late
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_late C-Win
ON VALUE-CHANGED OF tb_late IN FRAME FRAME-A /* Print Late Line Items Only? */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_printNotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_printNotes C-Win
ON VALUE-CHANGED OF tb_printNotes IN FRAME FRAME-A /* Print Notes */
DO:
  ASSIGN {&SELF-NAME}.
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
DEF VAR v-mat-list AS CHAR NO-UNDO.         
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

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

    assign            
     begin_due-date =  TODAY
     end_due-date   =  date(12,31,9999).

  RUN DisplaySelectionList.
  RUN enable_UI.

 for each mat:
    v-mat-list = v-mat-list + string(mat.mat,"x(5)") + " " + mat.dscr + ",".
  end.
  if substr(v-mat-list,length(trim(v-mat-list)),1) eq "," then
    substr(v-mat-list,length(trim(v-mat-list)),1) = "".

  select-mat:list-items = v-mat-list.

  do i = 1 to select-mat:num-items:
    if trim(substr(select-mat:entry(i),1,5)) eq "B" then do:
      select-mat:screen-value = entry(i,v-mat-list).
      leave.
    end.
  end.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_vend-no.
  END.

  cColumnInit   = NO .

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-fg-orders C-Win 
PROCEDURE add-fg-orders :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       IF po-ordl.ITEM-type THEN DO:
         FOR EACH job-hdr WHERE job-hdr.company = po-ordl.company
                            AND job-hdr.job-no = po-ordl.job-no
                            AND job-hdr.job-no2 = po-ordl.job-no2
                          NO-LOCK,
           EACH job WHERE job.company = job-hdr.company
                      AND job.job-no  = job-hdr.job-no
                      AND job.job-no2 = job-hdr.job-no2
                    NO-LOCK,
           EACH job-mat WHERE job-mat.company = job.company
                          AND job-mat.rm-i-no = po-ordl.i-no
                          AND job-mat.job-no  = job.job-no
                          AND job-mat.job-no2 = job.job-no2
                        NO-LOCK,
           EACH oe-ordl 
                        WHERE oe-ordl.company = job-mat.company
                          AND oe-ordl.i-no      EQ job-hdr.i-no
                          AND oe-ordl.stat NE "C" 
                        NO-LOCK,
           FIRST oe-ord WHERE oe-ord.company = oe-ordl.company
                          AND oe-ord.ord-no  = oe-ordl.ord-no
                        NO-LOCK:

           FIND cust WHERE cust.company = po-ordl.company
                       AND cust.cust-no = job-hdr.cust-no
                     NO-LOCK NO-ERROR.
           FIND itemfg WHERE itemfg.company = po-ordl.company
                         AND itemfg.i-no = job-hdr.i-no
                       NO-LOCK NO-ERROR.

           FIND FIRST tt-fgs WHERE tt-fgs.sched-rowid = ROWID(tt-sched)
                               AND tt-fgs.i-no        = job-hdr.i-no
                               AND tt-fgs.ord-no      = oe-ordl.ord-no
                             NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-fgs THEN DO:
             CREATE tt-fgs.
             ASSIGN 
               tt-fgs.sched-rowid = ROWID(tt-sched)
               tt-fgs.i-no        = job-hdr.i-no
               tt-fgs.cust-no     = job-hdr.cust-no
               tt-fgs.ord-no      = oe-ordl.ord-no
               tt-fgs.ord-qty     = oe-ordl.qty
               tt-fgs.due-date    = oe-ord.due-date
               tt-fgs.job-no      = STRING(po-ordl.s-wid) + " x " + STRING(po-ordl.s-len).
             IF AVAIL(itemfg) THEN
               tt-fgs.i-name      = itemfg.i-name.
             IF AVAIL cust THEN
               tt-fgs.cust-name   = cust.NAME.
           END.

         END.
       END.
       ELSE DO:
           FOR EACH oe-ordl 
                        WHERE oe-ordl.company = po-ordl.company
                          AND oe-ordl.i-no      EQ po-ordl.i-no
                          AND oe-ordl.stat NE "C" 
                        NO-LOCK,
             FIRST oe-ord WHERE oe-ord.company = oe-ordl.company
                          AND oe-ord.ord-no  = oe-ordl.ord-no
                        NO-LOCK:

             FIND cust WHERE cust.company = po-ordl.company
                         AND cust.cust-no = oe-ord.cust-no
                       NO-LOCK NO-ERROR.
             FIND itemfg WHERE itemfg.company = po-ordl.company
                           AND itemfg.i-no = po-ordl.i-no
                         NO-LOCK NO-ERROR.

             FIND FIRST tt-fgs WHERE tt-fgs.sched-rowid = ROWID(tt-sched)
                                 AND tt-fgs.i-no        = po-ordl.i-no
                                 AND tt-fgs.ord-no      = oe-ordl.ord-no
                               NO-LOCK NO-ERROR.
             IF NOT AVAIL tt-fgs THEN DO:
               CREATE tt-fgs.
               ASSIGN 
                 tt-fgs.sched-rowid = ROWID(tt-sched)
                 tt-fgs.i-no        = po-ordl.i-no
                 tt-fgs.cust-no     = oe-ord.cust-no
                 tt-fgs.ord-no      = oe-ordl.ord-no
                 tt-fgs.ord-qty     = oe-ordl.qty
                 tt-fgs.due-date    = oe-ord.due-date
                 tt-fgs.job-no      = STRING(po-ordl.s-wid) + " x " + STRING(po-ordl.s-len).
               IF AVAIL(itemfg) THEN
                 tt-fgs.i-name      = itemfg.i-name.
               IF AVAIL cust THEN
                 tt-fgs.cust-name   = cust.NAME.
             END.
           END.
       END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt C-Win 
PROCEDURE create-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH po-ord
      WHERE po-ord.company EQ cocode
        AND po-ord.vend-no GE v-s-vend
        AND po-ord.vend-no LE v-e-vend
        /*and (lookup(po-ord.stat,stat-list) gt 0 or v-po-stat eq "A")*/
        AND ((po-ord.opened AND v-po-stat EQ "O") OR
             (NOT po-ord.opened AND v-po-stat EQ "C") OR
             v-po-stat EQ "A")
      NO-LOCK,

      EACH po-ordl
      WHERE po-ordl.company  EQ po-ord.company
        AND po-ordl.po-no    EQ po-ord.po-no
        AND (CAN-DO(stat-list,po-ordl.stat) OR v-po-stat EQ "A")
        AND po-ordl.due-date GE v-s-date
        AND po-ordl.due-date LE v-e-date
        AND ((po-ordl.item-type AND
              CAN-FIND(FIRST item
                       WHERE item.company EQ po-ordl.company
                         AND item.i-no    EQ po-ordl.i-no
                         AND CAN-DO(v-mattype-list,item.mat-type)
                         AND item.procat  GE begin_procat
                         AND item.procat  LE end_procat
                         AND item.inv-by-cust EQ NO)) OR
             (NOT po-ordl.item-type AND
              CAN-FIND(FIRST itemfg
                       WHERE itemfg.company EQ po-ordl.company
                         AND itemfg.i-no    EQ po-ordl.i-no
                         AND itemfg.procat  GE begin_cat
                         AND itemfg.procat  LE end_cat)))
        AND (NOT tb_late OR po-ordl.due-date LT TODAY)
      NO-LOCK:

      {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}

    ASSIGN
     v-bwt = 0
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     v-dep = 0.

    IF po-ordl.item-type THEN DO:
       lv-uom = po-ordl.pr-qty-uom.

       FIND FIRST item NO-LOCK
           WHERE item.company EQ po-ordl.company
             AND item.i-no    EQ po-ordl.i-no
           NO-ERROR.

       IF AVAIL item THEN DO:
          v-dep = item.s-dep.
          {po/pol-dims.i}
       END.
    END.

    ELSE DO:
       lv-uom = "EA".

       FIND FIRST itemfg NO-LOCK
           WHERE itemfg.company EQ po-ordl.company
             AND itemfg.i-no    EQ po-ordl.i-no
           NO-ERROR.

       IF AVAIL itemfg THEN DO:
          IF v-len EQ 0 THEN v-len = itemfg.t-len.
          IF v-wid EQ 0 THEN v-wid = itemfg.t-wid.
          v-dep = itemfg.t-dep.
       END.
    END.

    ASSIGN
     v-qty  = po-ordl.t-rec-qty
     v-cost = po-ordl.cons-cost. 

    IF po-ordl.cons-uom NE lv-uom THEN DO:

       RUN sys/ref/convquom.p(po-ordl.cons-uom, lv-uom,
                              v-bwt, v-len, v-wid, v-dep,
                              v-qty, OUTPUT v-qty).

       RUN sys/ref/convcuom.p(po-ordl.cons-uom, lv-uom,
                              v-bwt, v-len, v-wid, v-dep,
                              v-cost, OUTPUT v-cost).
    END.


    IF lv-uom EQ "EA" THEN DO:
       {sys/inc/roundup.i v-qty}
    END.

    v-ord-qty = po-ordl.ord-qty.
    IF NOT po-ordl.item-type AND po-ordl.cons-uom NE lv-uom THEN 
       RUN sys/ref/convquom.p(po-ordl.cons-uom, lv-uom,
                              v-bwt, v-len, v-wid, v-dep,
                              po-ordl.ord-qty, OUTPUT v-ord-qty).

    /*IF po-ordl.ord-qty - v-qty GT 0 THEN DO:*/
    IF (v-ord-qty - v-qty GT 0 OR v-show-posted) THEN DO:

       IF v-cost EQ ? THEN v-cost = 0.

       CREATE tt-sched.
       ASSIGN
        tt-sched.job-no    = po-ordl.job-no
        tt-sched.job-no2   = po-ordl.job-no2
        tt-sched.i-no      = po-ordl.i-no
        tt-sched.i-name    = po-ordl.i-name
        tt-sched.vend-no   = po-ord.vend-no
        tt-sched.po-no     = po-ordl.po-no
        tt-sched.po-date   = po-ord.po-date
        tt-sched.cons-uom  = lv-uom
        tt-sched.cons-qty  = po-ordl.ord-qty
        tt-sched.t-rec-qty = (IF NOT po-ordl.item-type AND po-ordl.cons-uom NE lv-uom THEN po-ordl.t-rec-qty ELSE v-qty)
        tt-sched.due-date  = po-ordl.due-date
        tt-sched.carrier   = po-ord.carrier
        tt-sched.rec_key   = po-ordl.rec_key.
        IF v-ord-qty - v-qty GT 0 THEN
          v-tot = v-tot + ((po-ordl.ord-qty - v-qty) * v-cost).
        ELSE
          v-tot = v-tot + (v-qty * v-cost).

       /* Create tt record for fg's related to a rm */                       
       RUN add-fg-orders.

          FIND FIRST vend NO-LOCK
              WHERE vend.company EQ po-ord.company
                AND vend.vend-no EQ po-ord.vend-no
              NO-ERROR.

          ASSIGN
             tt-sched.vend-name = IF AVAIL vend THEN vend.name ELSE "" .

       IF v-sort EQ "V" THEN DO:
           ASSIGN
             v-s-num = po-ordl.s-num
             tt-sched.amt-msf = tt-sched.cons-qty - tt-sched.t-rec-qty.

          IF tt-sched.cons-uom NE "MSF" THEN
            RUN sys/ref/convquom.p(tt-sched.cons-uom, "MSF",
                                   v-bwt, v-len, v-wid, v-dep,
                                   tt-sched.amt-msf, OUTPUT tt-sched.amt-msf).
       END.
       FIND itemfg WHERE itemfg.company = po-ordl.company
              AND itemfg.i-no = po-ordl.i-no
            NO-LOCK NO-ERROR.
       IF AVAIL itemfg THEN
           tt-sched.loc-bin = itemfg.def-loc-bin.
       FIND ITEM WHERE ITEM.company = po-ordl.company
                   AND ITEM.i-no = po-ordl.i-no
                 NO-LOCK NO-ERROR.
       IF AVAIL ITEM THEN
           tt-sched.loc-bin = ITEM.loc-bin.

       FOR EACH fg-rctd WHERE fg-rctd.company = po-ord.company
                          AND fg-rctd.i-no = tt-sched.i-no
                          AND integer(fg-rctd.po-no) = po-ord.po-no
                        NO-LOCK.
          tt-sched.rct-date = fg-rctd.rct-date.
          IF fg-rctd.loc-bin > "" THEN
              tt-sched.loc-bin = fg-rctd.loc-bin.
       END.
       FOR EACH rm-rctd WHERE rm-rctd.company = po-ord.company
                   AND rm-rctd.i-no = tt-sched.i-no
                   AND integer(rm-rctd.po-no) = po-ord.po-no
                 NO-LOCK.
          tt-sched.rct-date = rm-rctd.rct-date.
           IF rm-rctd.loc-bin > "" THEN
            tt-sched.loc-bin = rm-rctd.loc-bin.
       END.
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault C-Win 
PROCEDURE DisplaySelectionDefault :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

     cListContents = cListContents +                   
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToDefault)   .
  END.            
  sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList C-Win 
PROCEDURE DisplaySelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:

     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    /* (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect) + "," +
                     ENTRY(1,cFieldListToSelect)
                     paris */

                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .
  END.

 /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */

  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 C-Win 
PROCEDURE DisplaySelectionList2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.
  DEF VAR cTmpList AS cha NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:
    RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    /* (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect) + "," +
                     ENTRY(1,cFieldListToSelect)
                     paris */

                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .
  END.

 /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */

  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

  cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

   DO iCount = 1 TO sl_selected:NUM-ITEMS:
       IF LOOKUP(ENTRY(iCount,cTmpList), cTextListToSelect) = 0 THEN
        ldummy = sl_selected:DELETE(ENTRY(iCount,cTmpList)).
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
  DISPLAY begin_vend-no end_vend-no begin_due-date end_due-date begin_procat 
          end_procat begin_cat end_cat tg_receipts begin_receipt-date 
          end_receipt-date lbl_show rd_show select-mat lbl_print rd_print 
          tb_late mat-types tb_printNotes rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_vend-no end_vend-no begin_due-date end_due-date 
         begin_procat end_procat begin_cat end_cat tg_receipts 
         begin_receipt-date end_receipt-date rd_show select-mat rd_print 
         tb_late tb_printNotes rd-dest lines-per-page td-show-parm tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel btn_SelectColumns
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList C-Win 
PROCEDURE GetSelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR cTmpList AS cha NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
 iColumnLength = 0.

 DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     

    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
           ttRptSelected.HeadingFromLeft = IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
           iColumnLength = iColumnLength + ttRptSelected.FieldLength + 1.
           .        

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
{custom/out2file.i}.

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
RUN cusotm/d-print.w (list-name).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printNotes C-Win 
PROCEDURE printNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRecKey AS CHARACTER NO-UNDO.

  IF CAN-FIND(FIRST notes WHERE notes.rec_key EQ ipRecKey) THEN
  FOR EACH notes NO-LOCK WHERE notes.rec_key EQ ipRecKey:
    PUT UNFORMATTED 'Note: ' AT 1 notes.note_title notes.note_text AT 1.
  END. /* each notes */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------- po/rep/sch-rcts.p 8/96 fwk  */
/* Scheduled Receipts Report                                                  */
/* -------------------------------------------------------------------------- */
 DEFINE OUTPUT PARAMETER op-valid AS LOG INIT TRUE NO-UNDO.

/*{sys/form/r-topw.f}*/


DEF VAR code-text AS CHAR NO-UNDO.

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-line AS cha FORM "x(300)" NO-UNDO.

{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF VAR excelheader AS CHAR NO-UNDO.

 stat-list = "".
 v-len = 0.
 v-wid = 0.
 v-bwt = 0.
 v-dep = 0.
 v-qty = 0.
 v-cost = 0.
 v-s-num =  1.
 v-start-rcv-date .
 v-end-rcv-date.
 v-show-posted.
 v-rcv-date-selected.
 v-tot = 0.
 v-ord-qty = 0.
 v-sort = "J" .

FORM HEADER
        "JOB NO/SIZE"
        "ITEM NO/FG ITEM" 
        "BIN     "
        "ITEM NAME/ON ORDER FOR"
        "VEND#/CUST"
        "P/O#   "
        " ORDER#"
        "P/O DATE"
        "UOM"
        "       QTY ORDER"
        "   QTY RECEIVED"
        "REQ DATE"
        "CARRIER"
        FILL("-",167) FORMAT "x(167)"
    WITH STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP
         FRAME sch-head-job.

FORM HEADER
        "ITEM NO/FG ITEM" AT 1
        "BIN     "
        "ITEM NAME/ON ORDER FOR "
        "VEND#/CUST"    
        "P/O#   "
        " ORDER#"
        "P/O DATE"
        "UOM"
        "       QTY ORDER"
        "   QTY RECEIVED"
        "REQ DATE"
        "CARRIER"
        "JOB NO/SIZE  "    
        FILL("-",167) FORMAT "x(167)"
    WITH STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP
         FRAME sch-head-item.


FORM HEADER
        "VEND NO   "
        "VENDOR NAME              "
        "ITEM NO/FG ITEM"
        "BIN     "
        "ITEM NAME/ON ORDER FOR  "
        "JOB NO/SIZE  "
        "P/O#   "
        " ORDER#"
        "P/O DATE"
        "       QTY ORDER"
        "    QTY RECEIVED"
        "REQ DATE "
        /* "   MSF" */
        "CARRIER"
        FILL("-",178) FORMAT "x(178)"
    WITH STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP
         FRAME sch-head-vend.

  FORM lv-job-no FORMAT "x(11)"
       tt-sched.i-no 
       tt-sched.loc-bin
       tt-sched.i-name FORMAT "x(22)"
       tt-sched.vend-no FORMAT "x(10)"
       tt-sched.po-no SPACE(3)
       tt-sched.ord-no SPACE(1)
       tt-sched.po-date FORMAT "99/99/99"
       tt-sched.cons-uom 
       tt-sched.cons-qty FORMAT "->>>,>>>,>>9.99"
       tt-sched.t-rec-qty FORMAT "->>>,>>>,>>9.99"
       tt-sched.due-date FORMAT "99/99/99"
       tt-sched.carrier 
       WITH DOWN STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE FRAME sch-rcts-job.

  FORM tt-sched.i-no
       tt-sched.loc-bin
       tt-sched.i-name FORMAT "x(23)"
       tt-sched.vend-no FORMAT "x(10)"
       tt-sched.po-no SPACE(3)
       tt-sched.ord-no
       tt-sched.po-date FORMAT "99/99/99"
       tt-sched.cons-uom
       tt-sched.cons-qty FORMAT "->>>,>>>,>>9.99"
       tt-sched.t-rec-qty FORMAT "->>>,>>>,>>9.99"
       tt-sched.due-date FORMAT "99/99/99" 
       tt-sched.carrier SPACE(2)
      lv-job-no FORMAT "x(13)"
       WITH DOWN STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE FRAME sch-rcts-item.

  FORM tt-sched.vend-no FORMAT "x(10)"
       tt-sched.vend-name FORMAT "x(25)"
       tt-sched.i-no
       tt-sched.loc-bin
       tt-sched.i-name FORMAT "x(24)"
       lv-job-no FORMAT "x(13)"
       tt-sched.po-no SPACE(3)
       tt-sched.ord-no
       tt-sched.po-date
       tt-sched.cons-qty FORMAT "->>>,>>>,>>9.99"
       tt-sched.t-rec-qty  FORMAT "->>>,>>>,>>9.99"
       tt-sched.due-date FORMAT "99/99/99" 
       /*tt-sched.amt-msf FORMAT "->,>>9.999" */ SPACE(2)
       tt-sched.carrier SKIP
       WITH DOWN STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE FRAME sch-rcts-vend.

  {ce/msfcalc.i}

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 v-s-vend   = begin_vend-no
 v-e-vend   = end_vend-no
 v-s-date   = begin_due-date
 v-e-date   = end_due-date
 v-po-stat  = SUBSTR(rd_show,1,1)
 v-sort     = SUBSTR(rd_print,1,1).
 v-show-posted = tg_receipts.
 v-start-rcv-date = begin_receipt-date.
 v-end-rcv-date   = end_receipt-date.

 IF v-start-rcv-date = 01/01/0001 AND v-end-rcv-date = 12/31/9999 THEN
     v-rcv-date-selected = NO.
 ELSE
     v-rcv-date-selected = YES.

do with frame {&frame-name}:          
  do i = 1 to select-mat:num-items:
    if select-mat:is-selected(i) then
       v-mattype-list = v-mattype-list + trim(substr(select-mat:entry(i),1,5)) + ",".
  end.

  IF length(TRIM(v-mattype-list)) EQ 0 THEN
  DO:
     MESSAGE "No Material Type Selected."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     op-valid = NO.
     LEAVE.
  END.

  if substr(v-mattype-list,length(trim(v-mattype-list)),1) eq "," then
     substr(v-mattype-list,length(trim(v-mattype-list)),1) = "".

  mat-types = v-mattype-list.

  do i = 1 to length(mat-types):
     if substr(mat-types,i,1) eq "," then substr(mat-types,i,1) = " ".
  end.

  display mat-types.
end.


DEF VAR cslist AS cha NO-UNDO.
 FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

   IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
   THEN ASSIGN str-tit4 = str-tit4 + ttRptSelected.TextList + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + "," .        
   ELSE 
   ASSIGN str-tit4 = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
          str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
          excelheader = excelHeader + ttRptSelected.TextList + ","
          .        
          cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
 /* IF v-sort EQ "J" THEN
     excelheader = "JOB NO/SIZE,ITEM NO/FG ITEM,BIN,ITEM NAME/ON ORDER FOR,VEND NO,P/O#,ORDER#,"
                 + "P/O DATE,UOM,QTY ORDER,QTY RECEIVED,REQ DATE,CARRIER".
  ELSE IF v-sort EQ "I" THEN
     excelheader = "ITEM NO/FG ITEM,BIN,ITEM NAME/ON ORDER FOR,VEND NO,P/O#,ORDER#,"
                 + "P/O DATE,UOM,QTY ORDER,QTY RECEIVED,REQ DATE,CARRIER,"
                 + "JOB NO/SIZE".
  ELSE
     excelheader = "VENDOR NO,VENDOR NAME,ITEM NO/FG ITEM,BIN,ITEM NAME/ON ORDER FOR,P/O#,ORDER#,"
                 + "P/O DATE,QTY ORDER,QTY RECEIVED,REQ DATE,CARRIER,"
                 + "JOB NO/SIZE".*/

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

IF td-show-parm THEN RUN show-param.

SESSION:SET-WAIT-STATE ("general").

DISPLAY "" WITH FRAME r-top.

 /* IF v-sort EQ "J" THEN
    DISPLAY WITH FRAME sch-head-job.
  ELSE
  IF v-sort eq "I" THEN
    DISPLAY WITH FRAME sch-head-item.
  else
    DISPLAY WITH FRAME sch-head-vend.*/

  EMPTY TEMP-TABLE tt-sched.
  EMPTY TEMP-TABLE tt-fgs.
  RUN create-tt. 
  stat-list = IF v-po-stat EQ "A" THEN ""             ELSE
              IF v-po-stat EQ "O" THEN "O,U,P,A,N,H"  ELSE "C,X,F".


  IF v-sort EQ "J" THEN
  FOR EACH tt-sched WHERE (IF v-rcv-date-selected = NO OR tt-sched.rct-date = ? THEN TRUE 
                           ELSE 
                           (tt-sched.rct-date GE v-start-rcv-date
                           AND tt-sched.rct-date LE v-end-rcv-date))
      USE-INDEX job BREAK BY tt-sched.job-no BY tt-sched.job-no2:

{custom/statusMsg.i " 'Processing PO#  '  + string(tt-sched.po-no) "}

     lv-job-no = IF tt-sched.job-no EQ "" THEN ""
                 ELSE TRIM(tt-sched.job-no) + "-" + STRING(tt-sched.job-no2,"99").

    /* DISPLAY lv-job-no
             tt-sched.i-no
             tt-sched.loc-bin
             tt-sched.i-name
             tt-sched.vend-no
             tt-sched.po-no
             tt-sched.po-date
             tt-sched.cons-uom
             tt-sched.cons-qty
             tt-sched.t-rec-qty
             tt-sched.due-date
             tt-sched.carrier
         WITH FRAME sch-rcts-job.

     IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
          '"' lv-job-no                                    '",'
          '"' tt-sched.i-no                                '",'
          '"' tt-sched.loc-bin                             '",'
          '"' tt-sched.i-name                              '",'
          '"' tt-sched.vend-no                             '",'
          '"' STRING(tt-sched.po-no)                       '",'
          '"' ""                                           '",'
          '"' (IF tt-sched.po-date NE ? THEN
                  STRING(tt-sched.po-date) ELSE "")        '",'
          '"' tt-sched.cons-uom                            '",'
          '"' STRING(tt-sched.cons-qty,"->>>,>>>,>>9.99")  '",'
          '"' STRING(tt-sched.t-rec-qty,"->>>,>>>,>>9.99") '",'
          '"' (IF tt-sched.due-date NE ? THEN
                  STRING(tt-sched.due-date) ELSE "")       '",'
          '"' tt-sched.carrier                             '",'
          SKIP. */

     ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                     WHEN "vend"        THEN cVarValue = string(tt-sched.vend-no,"x(8)")  .
                     WHEN "vend-name"   THEN cVarValue = string(tt-sched.vend-name,"x(30)")  .
                     WHEN "i-no"        THEN cVarValue = string(tt-sched.i-no,"x(15)") .
                     WHEN "fg-itm"      THEN cVarValue = "" .
                     WHEN "bin"         THEN cVarValue = string(tt-sched.loc-bin)  .
                     WHEN "i-name"      THEN cVarValue = STRING(tt-sched.i-name).
                     WHEN "cust-nam"    THEN cVarValue = "".
                     WHEN "po"          THEN cVarValue = STRING(tt-sched.po-no) .
                     WHEN "ord"         THEN cVarValue = "" .
                     WHEN "po-dt"       THEN cVarValue = IF tt-sched.po-date NE ? THEN STRING(tt-sched.po-date) ELSE "" .
                     WHEN "qty-ord"     THEN cVarValue = STRING(tt-sched.cons-qty,"->>>,>>>,>>9.99") .
                     WHEN "qty-rcv"     THEN cVarValue = STRING(tt-sched.t-rec-qty,"->>>,>>>,>>9.99") .
                     WHEN "rfq-dt"      THEN cVarValue = IF tt-sched.due-date NE ? THEN STRING(tt-sched.due-date) ELSE ""  .
                     WHEN "carr"        THEN cVarValue = STRING(tt-sched.carrier).
                     WHEN "job-no"      THEN cVarValue = string(lv-job-no).
                     WHEN "size"        THEN cVarValue = "" .
                     WHEN "uom"         THEN cVarValue = STRING(tt-sched.cons-uom) .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                           FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.

        PUT UNFORMATTED cDisplay SKIP.
        IF tb_excel THEN DO:
             PUT STREAM excel UNFORMATTED  
                   cExcelDisplay SKIP.
        END.


     IF tb_printNotes THEN RUN printNotes (tt-sched.rec_key).


     FOR EACH tt-fgs WHERE tt-fgs.sched-rowid = ROWID(tt-sched)
        /* WITH FRAME {1}*/
         BREAK BY tt-fgs.i-no
               BY tt-fgs.due-date:


             ASSIGN cDisplay = ""
                    cTmpField = ""
                    cVarValue = ""
                    cExcelDisplay = ""
                    cExcelVarValue = "".

             DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                     CASE cTmpField:             
                          WHEN "vend"        THEN cVarValue = "" .
                          WHEN "vend-name"   THEN cVarValue = ""  .
                          WHEN "i-no"        THEN cVarValue = "" .
                          WHEN "fg-itm"      THEN cVarValue = string(tt-fgs.i-no,"x(15)") .
                          WHEN "bin"         THEN cVarValue = ""  .
                          WHEN "i-name"      THEN cVarValue = "" .
                          WHEN "cust-nam"    THEN cVarValue = string(tt-fgs.cust-name).
                          WHEN "po"          THEN cVarValue = "" .
                          WHEN "ord"         THEN cVarValue = string(tt-fgs.ord-no) .
                          WHEN "po-dt"       THEN cVarValue = "" .
                          WHEN "qty-ord"     THEN cVarValue = STRING(tt-fgs.ord-qty,"->>>,>>>,>>9.99") .
                          WHEN "qty-rcv"     THEN cVarValue = "" .
                          WHEN "rfq-dt"      THEN cVarValue = IF tt-fgs.due-date NE ? THEN STRING(tt-fgs.due-date) ELSE ""  .
                          WHEN "carr"        THEN cVarValue = "" .
                          WHEN "job-no"      THEN cVarValue = "" .
                          WHEN "size"        THEN cVarValue = STRING(tt-fgs.job-no) .
                          WHEN "uom"         THEN cVarValue = ""  .
                          WHEN "cust"        THEN cVarValue = STRING(tt-fgs.cust-no)  .

                     END CASE.

                     cExcelVarValue = cVarValue.
                     cDisplay = cDisplay + cVarValue +
                                FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                     cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
             END.

             PUT UNFORMATTED cDisplay SKIP.
             IF tb_excel THEN DO:
                  PUT STREAM excel UNFORMATTED  
                        cExcelDisplay SKIP.
             END.
     END.

   /*  DOWN WITH FRAME sch-rcts-job.
     {porep\r-rschrp.i sch-rcts-job} */

  END.

  ELSE
  IF v-sort EQ "I" THEN
  FOR EACH tt-sched WHERE (IF v-rcv-date-selected = NO OR tt-sched.rct-date = ? THEN TRUE 
                           ELSE 
                           (tt-sched.rct-date GE v-start-rcv-date
                           AND tt-sched.rct-date LE v-end-rcv-date))
      USE-INDEX i-no BREAK BY tt-sched.i-no:
     lv-job-no = IF tt-sched.job-no EQ "" THEN ""
                 ELSE TRIM(tt-sched.job-no) + "-" + STRING(tt-sched.job-no2,"99").

   /*  DISPLAY lv-job-no
             tt-sched.i-no
             tt-sched.loc-bin
             tt-sched.i-name
             tt-sched.vend-no
             tt-sched.po-no
             tt-sched.po-date
             tt-sched.cons-uom
             tt-sched.cons-qty
             tt-sched.t-rec-qty
             tt-sched.due-date
             tt-sched.carrier
         WITH FRAME sch-rcts-item.

     IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
          '"' tt-sched.i-no                                '",'
          '"' tt-sched.loc-bin                             '",'
          '"' tt-sched.i-name                              '",'
          '"' tt-sched.vend-no                             '",'
          '"' STRING(tt-sched.po-no)                       '",'
          '"' ""                                           '",'
          '"' (IF tt-sched.po-date NE ? THEN
                  STRING(tt-sched.po-date) ELSE "")        '",'
          '"' tt-sched.cons-uom                            '",'
          '"' STRING(tt-sched.cons-qty,"->>>,>>>,>>9.99")  '",'
          '"' STRING(tt-sched.t-rec-qty,"->>>,>>>,>>9.99") '",'
          '"' (IF tt-sched.due-date NE ? THEN
                  STRING(tt-sched.due-date) ELSE "")       '",'
          '"' tt-sched.carrier                             '",'
          '"' lv-job-no                                    '",'
          SKIP. */

     ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                     WHEN "vend"        THEN cVarValue = string(tt-sched.vend-no,"x(8)") .
                     WHEN "vend-name"   THEN cVarValue = string(tt-sched.vend-name,"x(30)")  .
                     WHEN "i-no"        THEN cVarValue = string(tt-sched.i-no,"x(15)") .
                     WHEN "fg-itm"      THEN cVarValue = "" .
                     WHEN "bin"         THEN cVarValue = string(tt-sched.loc-bin)  .
                     WHEN "i-name"      THEN cVarValue = STRING(tt-sched.i-name).
                     WHEN "cust-nam"    THEN cVarValue = "".
                     WHEN "po"          THEN cVarValue = STRING(tt-sched.po-no) .
                     WHEN "ord"         THEN cVarValue = "" .
                     WHEN "po-dt"       THEN cVarValue = IF tt-sched.po-date NE ? THEN STRING(tt-sched.po-date) ELSE "" .
                     WHEN "qty-ord"     THEN cVarValue = STRING(tt-sched.cons-qty,"->>>,>>>,>>9.99") .
                     WHEN "qty-rcv"     THEN cVarValue = STRING(tt-sched.t-rec-qty,"->>>,>>>,>>9.99") .
                     WHEN "rfq-dt"      THEN cVarValue = IF tt-sched.due-date NE ? THEN STRING(tt-sched.due-date) ELSE ""  .
                     WHEN "carr"        THEN cVarValue = STRING(tt-sched.carrier).
                     WHEN "job-no"      THEN cVarValue = string(lv-job-no).
                     WHEN "size"        THEN cVarValue = "" .
                     WHEN "uom"         THEN cVarValue = STRING(tt-sched.cons-uom) .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                           FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.

        PUT UNFORMATTED cDisplay SKIP.
        IF tb_excel THEN DO:
             PUT STREAM excel UNFORMATTED  
                   cExcelDisplay SKIP.
        END.


     IF tb_printNotes THEN RUN printNotes (tt-sched.rec_key).


     FOR EACH tt-fgs WHERE tt-fgs.sched-rowid = ROWID(tt-sched)
        /* WITH FRAME {1}*/
         BREAK BY tt-fgs.i-no
               BY tt-fgs.due-date:


             ASSIGN cDisplay = ""
                    cTmpField = ""
                    cVarValue = ""
                    cExcelDisplay = ""
                    cExcelVarValue = "".

             DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                     CASE cTmpField:             
                          WHEN "vend"        THEN cVarValue = "" .
                          WHEN "vend-name"   THEN cVarValue = ""  .
                          WHEN "i-no"        THEN cVarValue = "" .
                          WHEN "fg-itm"      THEN cVarValue = string(tt-fgs.i-no,"x(15)") .
                          WHEN "bin"         THEN cVarValue = ""  .
                          WHEN "i-name"      THEN cVarValue = "" .
                          WHEN "cust-nam"    THEN cVarValue = string(tt-fgs.cust-name).
                          WHEN "po"          THEN cVarValue = "" .
                          WHEN "ord"         THEN cVarValue = string(tt-fgs.ord-no) .
                          WHEN "po-dt"       THEN cVarValue = "" .
                          WHEN "qty-ord"     THEN cVarValue = STRING(tt-fgs.ord-qty,"->>>,>>>,>>9.99") .
                          WHEN "qty-rcv"     THEN cVarValue = "" .
                          WHEN "rfq-dt"      THEN cVarValue = IF tt-fgs.due-date NE ? THEN STRING(tt-fgs.due-date) ELSE ""  .
                          WHEN "carr"        THEN cVarValue = "" .
                          WHEN "job-no"      THEN cVarValue = "" .
                          WHEN "size"        THEN cVarValue = STRING(tt-fgs.job-no) .
                          WHEN "uom"         THEN cVarValue = ""  .
                          WHEN "cust"        THEN cVarValue = STRING(tt-fgs.cust-no)  .

                     END CASE.

                     cExcelVarValue = cVarValue.
                     cDisplay = cDisplay + cVarValue +
                                FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                     cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
             END.

             PUT UNFORMATTED cDisplay SKIP.
             IF tb_excel THEN DO:
                  PUT STREAM excel UNFORMATTED  
                        cExcelDisplay SKIP.
             END.
     END.

    /* DOWN WITH FRAME sch-rcts-item.
     {porep\r-rschrp.i sch-rcts-item} */
  END.

  ELSE 
  FOR EACH tt-sched WHERE (IF v-rcv-date-selected = NO OR tt-sched.rct-date = ? THEN TRUE 
                           ELSE 
                           (tt-sched.rct-date GE v-start-rcv-date
                           AND tt-sched.rct-date LE v-end-rcv-date))
      USE-INDEX vend BREAK BY tt-sched.vend-no:
     lv-job-no = IF tt-sched.job-no EQ "" THEN ""
                 ELSE TRIM(tt-sched.job-no) + "-" + STRING(tt-sched.job-no2,"99").

     IF FIRST-OF(tt-sched.vend-no) THEN DO:
       /* PUT SKIP(1).
        DISPLAY tt-sched.vend-no tt-sched.vend-name WITH FRAME sch-rcts-vend. */
     END.


   /*  DISPLAY lv-job-no
             tt-sched.i-no
             tt-sched.loc-bin
             tt-sched.i-name
             tt-sched.po-no
             tt-sched.po-date
             tt-sched.cons-qty
             tt-sched.t-rec-qty
             tt-sched.due-date
             /*tt-sched.amt-msf */
             tt-sched.carrier
         WITH FRAME sch-rcts-vend.

     IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
          '"' (IF FIRST-OF(tt-sched.vend-no) THEN
                  tt-sched.vend-no ELSE "")                '",'
          '"' (IF FIRST-OF(tt-sched.vend-no) THEN
                  tt-sched.vend-name ELSE "")              '",'
          '"' tt-sched.i-no                                '",'
          '"' tt-sched.loc-bin                             '",'
          '"' tt-sched.i-name                              '",'
          '"' STRING(tt-sched.po-no)                       '",'
          '"' ""                                           '",'
          '"' (IF tt-sched.po-date NE ? THEN
                  STRING(tt-sched.po-date) ELSE "")        '",'
          '"' STRING(tt-sched.cons-qty,"->>>,>>>,>>9.99")  '",'
          '"' STRING(tt-sched.t-rec-qty,"->>>,>>>,>>9.99") '",'
          '"' (IF tt-sched.due-date NE ? THEN              
                  STRING(tt-sched.due-date) ELSE "")       '",'
         /* '"' STRING(tt-sched.amt-msf,"->,>>9.999")        '",' */
          '"' tt-sched.carrier                             '",'
          '"' lv-job-no                                    '",'
          SKIP. */

        ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                     WHEN "vend"        THEN cVarValue = IF FIRST-OF(tt-sched.vend-no) THEN string(tt-sched.vend-no,"x(8)") ELSE "" .
                     WHEN "vend-name"   THEN cVarValue = IF FIRST-OF(tt-sched.vend-no) THEN string(tt-sched.vend-name,"x(30)") ELSE "" .
                     WHEN "i-no"        THEN cVarValue = string(tt-sched.i-no,"x(15)") .
                     WHEN "fg-itm"      THEN cVarValue = "" .
                     WHEN "bin"         THEN cVarValue = string(tt-sched.loc-bin)  .
                     WHEN "i-name"      THEN cVarValue = STRING(tt-sched.i-name).
                     WHEN "cust-nam"    THEN cVarValue = "".
                     WHEN "po"          THEN cVarValue = STRING(tt-sched.po-no) .
                     WHEN "ord"         THEN cVarValue = "" .
                     WHEN "po-dt"       THEN cVarValue = IF tt-sched.po-date NE ? THEN STRING(tt-sched.po-date) ELSE "" .
                     WHEN "qty-ord"     THEN cVarValue = STRING(tt-sched.cons-qty,"->>>,>>>,>>9.99") .
                     WHEN "qty-rcv"     THEN cVarValue = STRING(tt-sched.t-rec-qty,"->>>,>>>,>>9.99") .
                     WHEN "rfq-dt"      THEN cVarValue = IF tt-sched.due-date NE ? THEN STRING(tt-sched.due-date) ELSE ""  .
                     WHEN "carr"        THEN cVarValue = STRING(tt-sched.carrier).
                     WHEN "job-no"      THEN cVarValue = string(lv-job-no).
                     WHEN "size"        THEN cVarValue = "" .
                     WHEN "uom"         THEN cVarValue = STRING(tt-sched.cons-uom) .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                           FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.

        PUT UNFORMATTED cDisplay SKIP.
        IF tb_excel THEN DO:
             PUT STREAM excel UNFORMATTED  
                   cExcelDisplay SKIP.
        END.


     IF tb_printNotes THEN RUN printNotes (tt-sched.rec_key).


     FOR EACH tt-fgs WHERE tt-fgs.sched-rowid = ROWID(tt-sched)
        /* WITH FRAME {1}*/
         BREAK BY tt-fgs.i-no
               BY tt-fgs.due-date:


             ASSIGN cDisplay = ""
                    cTmpField = ""
                    cVarValue = ""
                    cExcelDisplay = ""
                    cExcelVarValue = "".

             DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                     CASE cTmpField:             
                          WHEN "vend"        THEN cVarValue = "" .
                          WHEN "vend-name"   THEN cVarValue = ""  .
                          WHEN "i-no"        THEN cVarValue = "" .
                          WHEN "fg-itm"      THEN cVarValue = string(tt-fgs.i-no,"x(15)") .
                          WHEN "bin"         THEN cVarValue = ""  .
                          WHEN "i-name"      THEN cVarValue = "" .
                          WHEN "cust-nam"    THEN cVarValue = string(tt-fgs.cust-name).
                          WHEN "po"          THEN cVarValue = "" .
                          WHEN "ord"         THEN cVarValue = string(tt-fgs.ord-no) .
                          WHEN "po-dt"       THEN cVarValue = "" .
                          WHEN "qty-ord"     THEN cVarValue = STRING(tt-fgs.ord-qty,"->>>,>>>,>>9.99") .
                          WHEN "qty-rcv"     THEN cVarValue = "" .
                          WHEN "rfq-dt"      THEN cVarValue = IF tt-fgs.due-date NE ? THEN STRING(tt-fgs.due-date) ELSE ""  .
                          WHEN "carr"        THEN cVarValue = "" .
                          WHEN "job-no"      THEN cVarValue = "" .
                          WHEN "size"        THEN cVarValue = STRING(tt-fgs.job-no) .
                          WHEN "uom"         THEN cVarValue = ""  .
                          WHEN "cust"        THEN cVarValue = STRING(tt-fgs.cust-no)  .

                     END CASE.

                     cExcelVarValue = cVarValue.
                     cDisplay = cDisplay + cVarValue +
                                FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                     cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
             END.

             PUT UNFORMATTED cDisplay SKIP.
             IF tb_excel THEN DO:
                  PUT STREAM excel UNFORMATTED  
                        cExcelDisplay SKIP.
             END.
     END.

    /* DOWN WITH FRAME sch-rcts-vend. 
     {porep\r-rschrp.i sch-rcts-vend} */
  END. 

  IF CAN-FIND(FIRST tt-sched) THEN
     PUT SKIP(1) "Total Value:" AT 100 v-tot FORMAT ">>,>>>,>>9.99" SKIP(1).

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

  /* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*RETURN string(hField:BUFFER-VALUE, hField:FORMAT) */
  RETURN string(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

