&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: est\dAddEditComp.w
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-rowid  AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO.   /* add,update,view */
DEFINE INPUT PARAMETER ipcSetPart  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcSetPartName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcPartNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcProCat AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplAutoPart  AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER op-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}   
{oe\ttInputOrd.i }

ASSIGN 
    cocode = g_company
    locode = g_loc.

DEFINE VARIABLE char-val        AS CHARACTER NO-UNDO.

DEFINE VARIABLE lv-item-recid   AS RECID     NO-UNDO.
DEFINE VARIABLE ll-order-warned AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ll-new-record   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ilogic          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMaterialType   AS CHARACTER INITIAL "C,5,6,M,D" NO-UNDO .
DEFINE VARIABLE k_frac          AS DECIMAL   INIT 6.25 NO-UNDO.
DEFINE VARIABLE v-count         AS INTEGER   NO-UNDO.

{Inventory/ttInventory.i "NEW SHARED"}

{sys/inc/f16to32.i}

IF v-cecscrn-dec THEN
DO:
    DEFINE TEMP-TABLE tt-64-dec NO-UNDO
        FIELD DEC AS DECIMAL DECIMALS 6.

    DO v-count = 0 TO 63:
        CREATE tt-64-dec.
        tt-64-dec.DEC = v-count / 64.0.
        RELEASE tt-64-dec.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS est-no CustPart Qty Btn_OK Btn_Done ~
Btn_Cancel item-name qtyUom item item-desc-1 item-desc-2 item-desc-3 ~
cust-po po-line board-po vendor job-no job-no2 status2 tb_tax price pr-uom ~
discount qty-unit tot-price partial cost-per-m unit-pallet full-cost ~
type-code type-dscr custom prev-ord tb_man-inv tb_run-ship sman-1 sman-2 ~
sman-3 sales-per-1 sales-per-2 sales-per-3 comm-per-1 comm-per-2 comm-per-3 ~
req-code prom-code over-run under-run due-date prem-date job-start-date ~
RECT-21 RECT-38 RECT-40 RECT-31 RECT-41 
&Scoped-Define DISPLAYED-OBJECTS est-no CustPart Qty item-name qtyUom item ~
item-desc-1 item-desc-2 item-desc-3 cust-po po-line board-po vendor job-no ~
job-no2 status2 tb_tax price pr-uom discount qty-unit tot-price partial ~
cost-per-m unit-pallet full-cost type-code type-dscr custom prev-ord ~
tb_man-inv tb_run-ship fi_s-pct-lbl fi_s-comm-lbl fi_sman-lbl fi_sname-lbl ~
fi_sname-1 fi_sname-2 fi_sname-3 sman-1 sman-2 sman-3 sales-per-1 ~
sales-per-2 sales-per-3 comm-per-1 comm-per-2 comm-per-3 req-code prom-code ~
over-run under-run due-date prem-date job-start-date 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 fi_sname-1 fi_sname-2 fi_sname-3 sman-1 sman-2 sman-3 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE board-po AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Board PO" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE comm-per-1 AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE comm-per-2 AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE comm-per-3 AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cost-per-m AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cost/M" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cust-po AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cust Po#" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE custom AS CHARACTER FORMAT "X(32)":U 
     LABEL "Custom1" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE CustPart AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cust Part#" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE discount AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Discount" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE due-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Due Date" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE est-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_s-comm-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Comm.%" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi_s-pct-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "% of Sales" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi_sman-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi_sname-1 AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sname-2 AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1.

DEFINE VARIABLE fi_sname-3 AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1.

DEFINE VARIABLE fi_sname-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Name" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE full-cost AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Full Cost" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE item AS CHARACTER FORMAT "X(15)":U 
     LABEL "FG Item" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE item-desc-1 AS CHARACTER FORMAT "X(30)":U 
     LABEL "Desc 1" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE item-desc-2 AS CHARACTER FORMAT "X(30)":U 
     LABEL "Desc 2" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE item-desc-3 AS CHARACTER FORMAT "X(30)":U 
     LABEL "Desc 3" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE item-name AS CHARACTER FORMAT "X(30)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE job-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Jon Number" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE job-no2 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE job-start-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Job Start Date" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE over-run AS DECIMAL FORMAT ">>9.99%":U INITIAL 0 
     LABEL "Overrun%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE partial AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Partial" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE po-line AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Ln#" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE pr-uom AS CHARACTER FORMAT "X(3)":U 
     LABEL "UOM" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE prem-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Promise Date" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE prev-ord AS CHARACTER FORMAT "X(15)":U 
     LABEL "Prev Order" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE price AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL 0 
     LABEL "Price" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE prom-code AS CHARACTER FORMAT "XXXXX":U 
     LABEL "Priority" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE Qty AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Quantity" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE qty-unit AS INTEGER FORMAT ">>>,>>>":U INITIAL 0 
     LABEL "Qty/Unit" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE qtyUom AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE req-code AS CHARACTER FORMAT "XXXXX":U 
     LABEL "Priority" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE sales-per-1 AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE sales-per-2 AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE sales-per-3 AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE sman-1 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 7.8 BY 1 NO-UNDO.

DEFINE VARIABLE sman-2 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 7.8 BY 1 NO-UNDO.

DEFINE VARIABLE sman-3 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 7.8 BY 1 NO-UNDO.

DEFINE VARIABLE status2 AS CHARACTER FORMAT "X(2)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE tot-price AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total Price" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE type-code AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE type-dscr AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE under-run AS DECIMAL FORMAT ">>9.99%":U INITIAL 0 
     LABEL "Underrun%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE unit-pallet AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "Units/Pallet" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE vendor AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 19 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 78.8 BY 4.86.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 78.8 BY 11
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 65.8 BY 10.1
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 66 BY 5.81.

DEFINE VARIABLE tb_man-inv AS LOGICAL INITIAL no 
     LABEL "Managed Inventory" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_run-ship AS LOGICAL INITIAL no 
     LABEL "Run && Ship" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_tax AS LOGICAL INITIAL no 
     LABEL "Tax" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     est-no AT ROW 1.71 COL 15 COLON-ALIGNED WIDGET-ID 200
     CustPart AT ROW 4.62 COL 15 COLON-ALIGNED WIDGET-ID 176
     Qty AT ROW 2.71 COL 15 COLON-ALIGNED WIDGET-ID 178
     Btn_OK AT ROW 17.91 COL 128
     Btn_Done AT ROW 18.19 COL 129
     Btn_Cancel AT ROW 17.91 COL 137
     item-name AT ROW 5.57 COL 15 COLON-ALIGNED WIDGET-ID 208
     qtyUom AT ROW 2.71 COL 32.8 COLON-ALIGNED NO-LABEL WIDGET-ID 328
     item AT ROW 3.67 COL 15 COLON-ALIGNED WIDGET-ID 330
     item-desc-1 AT ROW 6.52 COL 15 COLON-ALIGNED WIDGET-ID 332
     item-desc-2 AT ROW 7.48 COL 15 COLON-ALIGNED WIDGET-ID 334
     item-desc-3 AT ROW 8.43 COL 15 COLON-ALIGNED WIDGET-ID 336
     cust-po AT ROW 10 COL 15 COLON-ALIGNED WIDGET-ID 338
     po-line AT ROW 9.95 COL 47 COLON-ALIGNED WIDGET-ID 340
     board-po AT ROW 11.19 COL 15 COLON-ALIGNED WIDGET-ID 342
     vendor AT ROW 11.14 COL 40.6 COLON-ALIGNED WIDGET-ID 344
     job-no AT ROW 1.71 COL 95.6 COLON-ALIGNED WIDGET-ID 348
     job-no2 AT ROW 1.71 COL 110.2 COLON-ALIGNED NO-LABEL WIDGET-ID 350
     status2 AT ROW 1.71 COL 130.8 COLON-ALIGNED WIDGET-ID 352
     tb_tax AT ROW 3 COL 136.8 WIDGET-ID 260
     price AT ROW 3.05 COL 95.6 COLON-ALIGNED WIDGET-ID 358
     pr-uom AT ROW 3.05 COL 124.8 COLON-ALIGNED WIDGET-ID 360
     discount AT ROW 4 COL 95.6 COLON-ALIGNED WIDGET-ID 362
     qty-unit AT ROW 4 COL 128.8 COLON-ALIGNED WIDGET-ID 364
     tot-price AT ROW 4.91 COL 95.6 COLON-ALIGNED WIDGET-ID 366
     partial AT ROW 4.91 COL 128.8 COLON-ALIGNED WIDGET-ID 368
     cost-per-m AT ROW 5.86 COL 95.6 COLON-ALIGNED WIDGET-ID 372
     unit-pallet AT ROW 5.86 COL 128.8 COLON-ALIGNED WIDGET-ID 370
     full-cost AT ROW 6.81 COL 95.6 COLON-ALIGNED WIDGET-ID 374
     type-code AT ROW 6.81 COL 114.4 COLON-ALIGNED NO-LABEL WIDGET-ID 376
     type-dscr AT ROW 6.81 COL 122 COLON-ALIGNED NO-LABEL WIDGET-ID 378
     custom AT ROW 8.05 COL 97 COLON-ALIGNED WIDGET-ID 380
     prev-ord AT ROW 9.71 COL 97 COLON-ALIGNED WIDGET-ID 382
     tb_man-inv AT ROW 9.19 COL 119.4 WIDGET-ID 384
     tb_run-ship AT ROW 10.29 COL 119.4 WIDGET-ID 386
     fi_s-pct-lbl AT ROW 12.81 COL 45.2 COLON-ALIGNED NO-LABEL WIDGET-ID 390
     fi_s-comm-lbl AT ROW 12.81 COL 60.2 COLON-ALIGNED NO-LABEL WIDGET-ID 388
     fi_sman-lbl AT ROW 12.76 COL 1.2 COLON-ALIGNED NO-LABEL WIDGET-ID 392
     fi_sname-lbl AT ROW 12.76 COL 18.2 COLON-ALIGNED NO-LABEL WIDGET-ID 394
     fi_sname-1 AT ROW 13.76 COL 13.2 COLON-ALIGNED NO-LABEL WIDGET-ID 398
     fi_sname-2 AT ROW 14.76 COL 13.2 COLON-ALIGNED NO-LABEL WIDGET-ID 400
     fi_sname-3 AT ROW 15.76 COL 13.2 COLON-ALIGNED NO-LABEL WIDGET-ID 402
     sman-1 AT ROW 13.76 COL 1.2 COLON-ALIGNED NO-LABEL WIDGET-ID 404
     sman-2 AT ROW 14.76 COL 1.2 COLON-ALIGNED NO-LABEL WIDGET-ID 406
     sman-3 AT ROW 15.76 COL 1.2 COLON-ALIGNED NO-LABEL WIDGET-ID 408
     sales-per-1 AT ROW 13.86 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 410
     sales-per-2 AT ROW 14.86 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 412
     sales-per-3 AT ROW 15.86 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 414
     comm-per-1 AT ROW 13.86 COL 60.6 COLON-ALIGNED NO-LABEL WIDGET-ID 416
     comm-per-2 AT ROW 14.86 COL 60.6 COLON-ALIGNED NO-LABEL WIDGET-ID 418
     comm-per-3 AT ROW 15.86 COL 60.6 COLON-ALIGNED NO-LABEL WIDGET-ID 420
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     req-code AT ROW 14.05 COL 91.6 COLON-ALIGNED WIDGET-ID 422
     prom-code AT ROW 15.14 COL 91.6 COLON-ALIGNED WIDGET-ID 424
     over-run AT ROW 11.86 COL 121 COLON-ALIGNED WIDGET-ID 426
     under-run AT ROW 12.95 COL 121 COLON-ALIGNED WIDGET-ID 428
     due-date AT ROW 14.05 COL 121 COLON-ALIGNED WIDGET-ID 430
     prem-date AT ROW 15.14 COL 121 COLON-ALIGNED WIDGET-ID 432
     job-start-date AT ROW 16.24 COL 121 COLON-ALIGNED WIDGET-ID 434
     "Order Line" VIEW-AS TEXT
          SIZE 14 BY .71 AT ROW 1 COL 5 WIDGET-ID 206
     RECT-21 AT ROW 17.67 COL 127
     RECT-38 AT ROW 1.43 COL 1.2
     RECT-40 AT ROW 1.38 COL 81 WIDGET-ID 346
     RECT-31 AT ROW 12.48 COL 1.4 WIDGET-ID 396
     RECT-41 AT ROW 11.57 COL 81 WIDGET-ID 10
     SPACE(0.79) SKIP(2.85)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Add/Update Order line".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
/*{methods/template/viewer.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_s-comm-lbl IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_s-pct-lbl IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sman-lbl IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sname-1 IN FRAME Dialog-Frame
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_sname-2 IN FRAME Dialog-Frame
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_sname-3 IN FRAME Dialog-Frame
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_sname-lbl IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sman-1 IN FRAME Dialog-Frame
   2                                                                    */
/* SETTINGS FOR FILL-IN sman-2 IN FRAME Dialog-Frame
   2                                                                    */
/* SETTINGS FOR FILL-IN sman-3 IN FRAME Dialog-Frame
   2                                                                    */
ASSIGN 
       tb_man-inv:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       tb_run-ship:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       tb_tax:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Add/Update Order line */
DO:
        DEFINE VARIABLE char-val   AS cha    NO-UNDO.
        DEFINE VARIABLE lv-handle  AS HANDLE NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID  NO-UNDO .
        
        CASE FOCUS:NAME :
            WHEN "rmItemID" THEN 
                DO:
                    
                END.
            
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Add/Update Order line */
ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Add/Update Order line */
DO:
            
        IF AVAILABLE ttInputOrdLine THEN
            op-rowid = ROWID(ttInputOrdLine) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST ttInputOrdLine EXCLUSIVE-LOCK
                WHERE RECID(ttInputOrdLine) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE ttInputOrdLine THEN DELETE ttInputOrdLine .
            op-rowid = ? .
        END.
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.

    /*APPLY "END-ERROR":U TO SELF.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME board-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL board-po Dialog-Frame
ON LEAVE OF board-po IN FRAME Dialog-Frame /* Board PO */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
        
    
        IF AVAILABLE ttInputOrdLine THEN
            op-rowid = ROWID(ttInputOrdLine) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST ttInputOrdLine EXCLUSIVE-LOCK
                WHERE RECID(ttInputOrdLine) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE ttInputOrdLine THEN DELETE ttInputOrdLine .
            op-rowid = ? .
        END. 
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
DO:
        
  &IF DEFINED (adm-panel) NE 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
        APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Save */
DO:
        DEFINE VARIABLE ld              AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE lValidateResult AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lError          AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE dCostStorage    AS DECIMAL   NO-UNDO .
        DEFINE VARIABLE dCostHandling   AS DECIMAL   NO-UNDO .
        DEFINE VARIABLE hftp            AS HANDLE    NO-UNDO.

        
        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
        
        RUN valid-part-no(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.
                
        RUN valid-procat(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.

        RUN valid-style(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.  
        
        RUN valid-Form-Blank(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY. 
        
        RUN valid-QtyPerSet(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY. 
        
       
        DO TRANSACTION:           

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&displayed-objects}.
            END.            
        END.
       /* IF ip-type EQ "Add" THEN
        DO:
            CREATE ttInputOrdLine.
            ASSIGN
                ttInputOrdLine.cEstType = "NewSetEstimate"
                ttInputOrdLine.cSetType = "Set"
                ttInputOrdLine.cCompany = cocode .
        END.
        
        ASSIGN
            ttInputOrdLine.iFormNo          = iForm
            ttInputOrdLine.iBlankNo         = iBlank             
            ttInputOrdLine.cPartID          = CustPart             
            ttInputOrdLine.cPartName        = item-name
            ttInputOrdLine.cPartDescription = item-dscr
            ttInputOrdLine.dLength          = len
            ttInputOrdLine.dWidth           = wid            
            ttInputOrdLine.dDepth           = dep
            ttInputOrdLine.cCategory        = fg-cat
            ttInputOrdLine.cBoard           = board
            ttInputOrdLine.cStyle           = style-cod
            ttInputOrdLine.dQtyPerSet       = dQtyPerSet
            ttInputOrdLine.lPurchased       = IF rd_show1 EQ "P" THEN TRUE ELSE FALSE
            .           
                    */
        op-rowid = ROWID(ttInputOrdLine).
        
        APPLY "go" TO FRAME {&FRAME-NAME}.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME comm-per-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL comm-per-1 Dialog-Frame
ON LEAVE OF comm-per-1 IN FRAME Dialog-Frame
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME comm-per-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL comm-per-2 Dialog-Frame
ON LEAVE OF comm-per-2 IN FRAME Dialog-Frame
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME comm-per-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL comm-per-3 Dialog-Frame
ON LEAVE OF comm-per-3 IN FRAME Dialog-Frame
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cost-per-m
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cost-per-m Dialog-Frame
ON LEAVE OF cost-per-m IN FRAME Dialog-Frame /* Cost/M */
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust-po Dialog-Frame
ON LEAVE OF cust-po IN FRAME Dialog-Frame /* Cust Po# */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME custom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL custom Dialog-Frame
ON LEAVE OF custom IN FRAME Dialog-Frame /* Custom1 */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME discount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL discount Dialog-Frame
ON LEAVE OF discount IN FRAME Dialog-Frame /* Discount */
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL due-date Dialog-Frame
ON LEAVE OF due-date IN FRAME Dialog-Frame /* Due Date */
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME full-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL full-cost Dialog-Frame
ON LEAVE OF full-cost IN FRAME Dialog-Frame /* Full Cost */
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-desc-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-desc-1 Dialog-Frame
ON LEAVE OF item-desc-1 IN FRAME Dialog-Frame /* Desc 1 */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-desc-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-desc-2 Dialog-Frame
ON LEAVE OF item-desc-2 IN FRAME Dialog-Frame /* Desc 2 */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-desc-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-desc-3 Dialog-Frame
ON LEAVE OF item-desc-3 IN FRAME Dialog-Frame /* Desc 3 */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-name Dialog-Frame
ON LEAVE OF item-name IN FRAME Dialog-Frame /* Name */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-start-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-start-date Dialog-Frame
ON LEAVE OF job-start-date IN FRAME Dialog-Frame /* Job Start Date */
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME over-run
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL over-run Dialog-Frame
ON LEAVE OF over-run IN FRAME Dialog-Frame /* Overrun% */
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL partial Dialog-Frame
ON LEAVE OF partial IN FRAME Dialog-Frame /* Partial */
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-line
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-line Dialog-Frame
ON LEAVE OF po-line IN FRAME Dialog-Frame /* Ln# */
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pr-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pr-uom Dialog-Frame
ON HELP OF pr-uom IN FRAME Dialog-Frame /* UOM */
DO:
       /* DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
        DEF VAR lv-rowid AS ROWID NO-UNDO.
        DEF VAR lv-ind LIKE style.industry NO-UNDO.
        DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
        
           FIND style WHERE style.company = cocode AND
                            style.style = style-cod:SCREEN-VALUE 
                            NO-LOCK NO-ERROR.   
           IF AVAIL style THEN lv-ind = style.industry.
           ELSE lv-ind = "".  
           IF AVAIL style AND style.type = "f" THEN  DO: /* foam */    
              RUN AOA/dynLookupSetParam.p (70, ROWID(style), OUTPUT char-val).
              IF char-val NE "" AND ENTRY(1,char-val) NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "i-no", char-val).                
                APPLY "ENTRY":U TO board.
              END.
           END.
           IF AVAIL style AND style.type = "W" THEN  DO: /* foam */    
              RUN system/openlookup.p (
                cocode, 
                "", /* lookup field */
                155,   /* Subject ID */
                "",  /* User ID */
                0,   /* Param value ID */
                OUTPUT returnFields, 
                OUTPUT lookupField, 
                OUTPUT recVal
                ). 
              IF lookupField NE "" AND lookupField NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = lookupField.                
                APPLY "ENTRY":U TO board.
              END.
           END.
           ELSE DO:
              RUN windows/l-board1.w (cocode,lv-ind,board:SCREEN-VALUE, OUTPUT lv-rowid).
              FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
              IF AVAIL ITEM AND ITEM.i-no NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = item.i-no.                
              END.
           END.  */       
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pr-uom Dialog-Frame
ON LEAVE OF pr-uom IN FRAME Dialog-Frame /* UOM */
DO:
        /*IF LASTKEY NE -1 THEN 
        DO:
            IF NOT CAN-FIND(item WHERE item.company = cocode
                AND item.i-no = board:SCREEN-VALUE)
                THEN 
            DO:
                MESSAGE "Invalid Board. Try Help. " VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO board.
                RETURN NO-APPLY.
            END.
            IF SELF:SCREEN-VALUE NE "" THEN 
            DO:
                FIND FIRST item WHERE item.company = cocode
                    AND item.i-no EQ board:SCREEN-VALUE NO-LOCK NO-ERROR .

                IF AVAILABLE ITEM THEN
                    ASSIGN board-dscr:SCREEN-VALUE = item.i-name .
            END.
                                                
        END. */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prem-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prem-date Dialog-Frame
ON LEAVE OF prem-date IN FRAME Dialog-Frame /* Promise Date */
DO:
       /* DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.  */   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prev-ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prev-ord Dialog-Frame
ON LEAVE OF prev-ord IN FRAME Dialog-Frame /* Prev Order */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL price Dialog-Frame
ON LEAVE OF price IN FRAME Dialog-Frame /* Price */
DO:
       /* DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.  */   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prom-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prom-code Dialog-Frame
ON HELP OF prom-code IN FRAME Dialog-Frame /* Priority */
DO:
        /*DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
        DEF VAR lv-rowid AS ROWID NO-UNDO.
        DEF VAR lv-ind LIKE style.industry NO-UNDO.
        DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
        
           FIND style WHERE style.company = cocode AND
                            style.style = style-cod:SCREEN-VALUE 
                            NO-LOCK NO-ERROR.   
           IF AVAIL style THEN lv-ind = style.industry.
           ELSE lv-ind = "".  
           IF AVAIL style AND style.type = "f" THEN  DO: /* foam */    
              RUN AOA/dynLookupSetParam.p (70, ROWID(style), OUTPUT char-val).
              IF char-val NE "" AND ENTRY(1,char-val) NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "i-no", char-val).                
                APPLY "ENTRY":U TO board.
              END.
           END.
           IF AVAIL style AND style.type = "W" THEN  DO: /* foam */    
              RUN system/openlookup.p (
                cocode, 
                "", /* lookup field */
                155,   /* Subject ID */
                "",  /* User ID */
                0,   /* Param value ID */
                OUTPUT returnFields, 
                OUTPUT lookupField, 
                OUTPUT recVal
                ). 
              IF lookupField NE "" AND lookupField NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = lookupField.                
                APPLY "ENTRY":U TO board.
              END.
           END.
           ELSE DO:
              RUN windows/l-board1.w (cocode,lv-ind,board:SCREEN-VALUE, OUTPUT lv-rowid).
              FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
              IF AVAIL ITEM AND ITEM.i-no NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = item.i-no.                
              END.
           END.*/         
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prom-code Dialog-Frame
ON LEAVE OF prom-code IN FRAME Dialog-Frame /* Priority */
DO:
        /*IF LASTKEY NE -1 THEN 
        DO:
            IF NOT CAN-FIND(item WHERE item.company = cocode
                AND item.i-no = board:SCREEN-VALUE)
                THEN 
            DO:
                MESSAGE "Invalid Board. Try Help. " VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO board.
                RETURN NO-APPLY.
            END.
            IF SELF:SCREEN-VALUE NE "" THEN 
            DO:
                FIND FIRST item WHERE item.company = cocode
                    AND item.i-no EQ board:SCREEN-VALUE NO-LOCK NO-ERROR .

                IF AVAILABLE ITEM THEN
                    ASSIGN board-dscr:SCREEN-VALUE = item.i-name .
            END.
                                                
        END. */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Qty Dialog-Frame
ON LEAVE OF Qty IN FRAME Dialog-Frame /* Quantity */
DO:
        /*DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
         RUN valid-QtyPerSet(OUTPUT lError) NO-ERROR.
         IF lError THEN RETURN NO-APPLY.           
        END.  */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME qty-unit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL qty-unit Dialog-Frame
ON LEAVE OF qty-unit IN FRAME Dialog-Frame /* Qty/Unit */
DO:
       /* DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.*/     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME req-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL req-code Dialog-Frame
ON HELP OF req-code IN FRAME Dialog-Frame /* Priority */
DO:
        /*DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
        DEF VAR lv-rowid AS ROWID NO-UNDO.
        DEF VAR lv-ind LIKE style.industry NO-UNDO.
        DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
        
           FIND style WHERE style.company = cocode AND
                            style.style = style-cod:SCREEN-VALUE 
                            NO-LOCK NO-ERROR.   
           IF AVAIL style THEN lv-ind = style.industry.
           ELSE lv-ind = "".  
           IF AVAIL style AND style.type = "f" THEN  DO: /* foam */    
              RUN AOA/dynLookupSetParam.p (70, ROWID(style), OUTPUT char-val).
              IF char-val NE "" AND ENTRY(1,char-val) NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "i-no", char-val).                
                APPLY "ENTRY":U TO board.
              END.
           END.
           IF AVAIL style AND style.type = "W" THEN  DO: /* foam */    
              RUN system/openlookup.p (
                cocode, 
                "", /* lookup field */
                155,   /* Subject ID */
                "",  /* User ID */
                0,   /* Param value ID */
                OUTPUT returnFields, 
                OUTPUT lookupField, 
                OUTPUT recVal
                ). 
              IF lookupField NE "" AND lookupField NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = lookupField.                
                APPLY "ENTRY":U TO board.
              END.
           END.
           ELSE DO:
              RUN windows/l-board1.w (cocode,lv-ind,board:SCREEN-VALUE, OUTPUT lv-rowid).
              FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
              IF AVAIL ITEM AND ITEM.i-no NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = item.i-no.                
              END.
           END.  */       
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL req-code Dialog-Frame
ON LEAVE OF req-code IN FRAME Dialog-Frame /* Priority */
DO:
       /* IF LASTKEY NE -1 THEN 
        DO:
            IF NOT CAN-FIND(item WHERE item.company = cocode
                AND item.i-no = board:SCREEN-VALUE)
                THEN 
            DO:
                MESSAGE "Invalid Board. Try Help. " VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO board.
                RETURN NO-APPLY.
            END.
            IF SELF:SCREEN-VALUE NE "" THEN 
            DO:
                FIND FIRST item WHERE item.company = cocode
                    AND item.i-no EQ board:SCREEN-VALUE NO-LOCK NO-ERROR .

                IF AVAILABLE ITEM THEN
                    ASSIGN board-dscr:SCREEN-VALUE = item.i-name .
            END.
                                                
        END.  */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sales-per-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sales-per-1 Dialog-Frame
ON LEAVE OF sales-per-1 IN FRAME Dialog-Frame
DO:
       /* DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.  */   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sales-per-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sales-per-2 Dialog-Frame
ON LEAVE OF sales-per-2 IN FRAME Dialog-Frame
DO:
        /*DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.*/     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sales-per-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sales-per-3 Dialog-Frame
ON LEAVE OF sales-per-3 IN FRAME Dialog-Frame
DO:
       /* DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.  */   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME status2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL status2 Dialog-Frame
ON HELP OF status2 IN FRAME Dialog-Frame /* Status */
DO:
        /*DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
        DEF VAR lv-rowid AS ROWID NO-UNDO.
        DEF VAR lv-ind LIKE style.industry NO-UNDO.
        DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
        
           FIND style WHERE style.company = cocode AND
                            style.style = style-cod:SCREEN-VALUE 
                            NO-LOCK NO-ERROR.   
           IF AVAIL style THEN lv-ind = style.industry.
           ELSE lv-ind = "".  
           IF AVAIL style AND style.type = "f" THEN  DO: /* foam */    
              RUN AOA/dynLookupSetParam.p (70, ROWID(style), OUTPUT char-val).
              IF char-val NE "" AND ENTRY(1,char-val) NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "i-no", char-val).                
                APPLY "ENTRY":U TO board.
              END.
           END.
           IF AVAIL style AND style.type = "W" THEN  DO: /* foam */    
              RUN system/openlookup.p (
                cocode, 
                "", /* lookup field */
                155,   /* Subject ID */
                "",  /* User ID */
                0,   /* Param value ID */
                OUTPUT returnFields, 
                OUTPUT lookupField, 
                OUTPUT recVal
                ). 
              IF lookupField NE "" AND lookupField NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = lookupField.                
                APPLY "ENTRY":U TO board.
              END.
           END.
           ELSE DO:
              RUN windows/l-board1.w (cocode,lv-ind,board:SCREEN-VALUE, OUTPUT lv-rowid).
              FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
              IF AVAIL ITEM AND ITEM.i-no NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = item.i-no.                
              END.
           END. */        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL status2 Dialog-Frame
ON LEAVE OF status2 IN FRAME Dialog-Frame /* Status */
DO:
       /* IF LASTKEY NE -1 THEN 
        DO:
            IF NOT CAN-FIND(item WHERE item.company = cocode
                AND item.i-no = board:SCREEN-VALUE)
                THEN 
            DO:
                MESSAGE "Invalid Board. Try Help. " VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO board.
                RETURN NO-APPLY.
            END.
            IF SELF:SCREEN-VALUE NE "" THEN 
            DO:
                FIND FIRST item WHERE item.company = cocode
                    AND item.i-no EQ board:SCREEN-VALUE NO-LOCK NO-ERROR .

                IF AVAILABLE ITEM THEN
                    ASSIGN board-dscr:SCREEN-VALUE = item.i-name .
            END.
                                                
        END.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tot-price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tot-price Dialog-Frame
ON LEAVE OF tot-price IN FRAME Dialog-Frame /* Total Price */
DO:
        /*DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END. */    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME under-run
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL under-run Dialog-Frame
ON LEAVE OF under-run IN FRAME Dialog-Frame /* Underrun% */
DO:
       /* DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.*/     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME unit-pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL unit-pallet Dialog-Frame
ON LEAVE OF unit-pallet IN FRAME Dialog-Frame /* Units/Pallet */
DO:
        /*DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.   */  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vendor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendor Dialog-Frame
ON LEAVE OF vendor IN FRAME Dialog-Frame /* Vendor */
DO:
       /* DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.  */   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i} 
SESSION:DATA-ENTRY-RETURN = YES.       

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    FIND FIRST eb NO-LOCK
        WHERE ROWID(eb) EQ ip-rowid NO-ERROR .
    
    IF ip-type EQ "copy" THEN lv-item-recid = ip-recid.

    /*IF ip-recid EQ ? THEN 
    DO:
        RUN create-item.
    END.
    ELSE FIND estPacking NO-LOCK WHERE RECID(estPacking) EQ ip-recid NO-ERROR.*/

    IF ip-type NE "view" THEN 
    DO: 
        
        RUN enable_UI.
        RUN display-item.

        ASSIGN 
            ll-order-warned = NO.
        btn_done:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE 
    DO:
        RUN display-item.
        ASSIGN 
            btn_done:HIDDEN IN FRAME {&FRAME-NAME} = NO.
        btn_done:SENSITIVE                        = YES.
        btn_ok:HIDDEN                             = YES.
        btn_cancel:HIDDEN                         = YES.
    END.
    /*FIND CURRENT estPacking NO-LOCK NO-ERROR .*/
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item Dialog-Frame 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
                      Purpose:     
                      PARAMs:  <none>
                      Notes:       
     ------------------------------------------------------------------------------*/   
    /*DEFINE BUFFER bf-ttInputOrdLine FOR ttInputOrdLine .
    FIND FIRST ttInputOrdLine WHERE RECID(ttInputOrdLine) EQ ip-recid NO-ERROR. 
    IF AVAILABLE ttInputOrdLine THEN 
        ASSIGN
            iForm      = ttInputOrdLine.iFormNo
            iBlank     = ttInputOrdLine.iBlankNo
            dQtyPerSet = ttInputOrdLine.dQtyPerSet
            cCustPart  = ttInputOrdLine.cPartID
            style-cod  = ttInputOrdLine.cStyle
            board      = ttInputOrdLine.cBoard
            fg-cat     = ttInputOrdLine.cCategory
            item-name  = ttInputOrdLine.cPartName
            item-dscr  = ttInputOrdLine.cPartDescription
            len        = ttInputOrdLine.dLength
            wid        = ttInputOrdLine.dWidth
            dep        = ttInputOrdLine.dDepth
            rd_show1   = IF ttInputOrdLine.lPurchased THEN "P" ELSE "M" .
    est-no = IF AVAILABLE eb THEN eb.est-no ELSE "".     
    cSetCustPart = ipcSetPart.
    set-item-name = ipcSetPartName.
    IF ip-type EQ "Add" THEN 
    DO:
        j = 0 .
        FOR EACH bf-ttInputOrdLine NO-LOCK BREAK BY bf-ttInputOrdLine.iForm:
            IF FIRST-OF(bf-ttInputOrdLine.iForm) THEN
                j  = j + 1.
        END.
          
        iForm       =  (j + 1).
        iBlank      =  1 .  
        IF iplAutoPart THEN 
        DO:
            cCustPart = SUBSTRING(ipcPartNo,1,12) + "-" + string(j + 1 ,"99").        
        END.  
        fg-cat = ipcProCat.
    END.
       
    FIND FIRST style NO-LOCK WHERE style.company = cocode
        AND style.style EQ style-cod NO-ERROR .
        
    IF AVAILABLE style THEN
        ASSIGN style-dscr = style.dscr .
    FIND FIRST fgcat NO-LOCK WHERE fgcat.company = cocode
        AND fgcat.procat EQ fg-cat NO-ERROR .
    IF AVAILABLE fgcat THEN
        ASSIGN cat-dscr = fgcat.dscr .
            
    FIND FIRST ITEM NO-LOCK WHERE item.company = cocode
        AND item.i-no EQ board NO-ERROR .
    IF AVAILABLE ITEM THEN
        ASSIGN board-dscr = item.i-name .        
    
    DISPLAY   
        est-no iForm iBlank cSetCustPart dQtyPerSet set-item-name cCustPart  
        style-cod style-dscr board fg-cat board-dscr cat-dscr item-name item-dscr 
        rd_show1 len wid dep
        WITH FRAME Dialog-Frame.  */     
   
        
    IF ip-type NE "view" THEN 
    DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.
    END.

    VIEW FRAME {&FRAME-NAME}. 
    APPLY "entry" TO FRAME {&FRAME-NAME}.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY est-no CustPart Qty item-name qtyUom item item-desc-1 item-desc-2 
          item-desc-3 cust-po po-line board-po vendor job-no job-no2 status2 
          tb_tax price pr-uom discount qty-unit tot-price partial cost-per-m 
          unit-pallet full-cost type-code type-dscr custom prev-ord tb_man-inv 
          tb_run-ship fi_s-pct-lbl fi_s-comm-lbl fi_sman-lbl fi_sname-lbl 
          fi_sname-1 fi_sname-2 fi_sname-3 sman-1 sman-2 sman-3 sales-per-1 
          sales-per-2 sales-per-3 comm-per-1 comm-per-2 comm-per-3 req-code 
          prom-code over-run under-run due-date prem-date job-start-date 
      WITH FRAME Dialog-Frame.
  ENABLE est-no CustPart Qty Btn_OK Btn_Done Btn_Cancel item-name qtyUom item 
         item-desc-1 item-desc-2 item-desc-3 cust-po po-line board-po vendor 
         job-no job-no2 status2 tb_tax price pr-uom discount qty-unit tot-price 
         partial cost-per-m unit-pallet full-cost type-code type-dscr custom 
         prev-ord tb_man-inv tb_run-ship sman-1 sman-2 sman-3 sales-per-1 
         sales-per-2 sales-per-3 comm-per-1 comm-per-2 comm-per-3 req-code 
         prom-code over-run under-run due-date prem-date job-start-date RECT-21 
         RECT-38 RECT-40 RECT-31 RECT-41 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit Dialog-Frame 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
             Purpose:
             Notes:
            ------------------------------------------------------------------------------*/


    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

/* Code placed here will execute AFTER standard behavior.    */
    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetBoardFromStyle Dialog-Frame 
PROCEDURE pGetBoardFromStyle :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcStyle AS CHARACTER NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
    
        /*FIND FIRST flute NO-LOCK
            WHERE flute.company EQ cocode NO-ERROR .
        IF AVAILABLE flute THEN
            FIND FIRST reftable WHERE reftable.reftable = "STYFLU" AND reftable.company = ipcStyle 
                AND reftable.loc = flute.code
                AND reftable.code = "BOARD"
                NO-LOCK NO-ERROR. 
        board:screen-value = IF AVAILABLE reftable AND AVAILABLE flute AND reftable.dscr NE "" THEN reftable.dscr ELSE board:screen-value.*/
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-Form-Blank Dialog-Frame 
PROCEDURE valid-Form-Blank :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    /*DEFINE BUFFER bf-ttInputOrdLine FOR ttInputOrdLine .
    
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST bf-ttInputOrdLine  NO-LOCK
            WHERE bf-ttInputOrdLine.cCompany  EQ cocode
            AND bf-ttInputOrdLine.iFormNo    EQ INTEGER(iForm:SCREEN-VALUE)
            AND bf-ttInputOrdLine.iBlankNo   EQ integer(iBlank:SCREEN-VALUE)
            AND RECID(bf-ttInputOrdLine) NE ip-recid NO-ERROR.
        IF AVAILABLE bf-ttInputOrdLine THEN 
        DO:
            MESSAGE "Form and blank already Entered ..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO iForm .
            oplOutError = YES .
        END.
    END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-no Dialog-Frame 
PROCEDURE valid-part-no :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
     /*   IF cCustPart:SCREEN-VALUE  EQ "" THEN 
        DO:
            MESSAGE "Customer Part # required..." VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO cCustPart .
            oplOutError = YES .
        END. */
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-procat Dialog-Frame 
PROCEDURE valid-procat :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        /*fg-cat:SCREEN-VALUE  = CAPS(fg-cat:SCREEN-VALUE).

        IF NOT CAN-FIND(FIRST fgcat
            WHERE fgcat.company EQ cocode
            AND fgcat.procat  EQ fg-cat:SCREEN-VALUE) THEN 
        DO:
            MESSAGE "Invalid FG Category, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fg-cat .
            oplOutError = YES .
        END.  */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-QtyPerSet Dialog-Frame 
PROCEDURE valid-QtyPerSet :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
      /*  IF integer(dQtyPerSet:SCREEN-VALUE)  LE 0 THEN 
        DO:
            MESSAGE "Qty Per Set must be greater then 0..." VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO dQtyPerSet .
            oplOutError = YES .
        END.     */
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-style Dialog-Frame 
PROCEDURE valid-style :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
       /* IF NOT CAN-FIND(FIRST style
            WHERE style.company  EQ cocode
            AND style.style    EQ style-cod:SCREEN-VALUE
            AND style.industry EQ "2")  THEN 
        DO:
            MESSAGE "Invalid Style Code, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO style-cod .
            oplOutError = YES .
        END. */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

