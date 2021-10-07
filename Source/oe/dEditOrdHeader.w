&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: est\dEditOrdHeader.w
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

{oe\ttInputOrd.i}

/* PARAMs Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttInputOrd.

DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLoc     AS CHARACTER NO-UNDO.
RUN spGetSessionParam ("Company", OUTPUT cCompany).
RUN spGetSessionParam ("Location", OUTPUT cLoc).


DEFINE VARIABLE char-val        AS CHARACTER NO-UNDO.

DEFINE VARIABLE lv-item-recid   AS RECID     NO-UNDO.
DEFINE VARIABLE ll-order-warned AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ll-new-record   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ilogic          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMaterialType   AS CHARACTER INITIAL "C,5,6,M,D" NO-UNDO .
DEFINE VARIABLE k_frac          AS DECIMAL   INIT 6.25 NO-UNDO.
DEFINE VARIABLE v-count         AS INTEGER   NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Done Btn_Cancel contact prev-ord ~
tb_man-inv sman-1 sman-2 sman-3 sales-per-1 sales-per-2 sales-per-3 ~
comm-per-1 comm-per-2 comm-per-3 req-code over-run under-run RECT-21 ~
RECT-38 RECT-31 RECT-41 cCustNo RECT-42 RECT-43 soldTo shipTo tax-code ~
terms RECT-44 ord-date due-date csr last-ship pro-date prom-date ~
po-rec-date tb_price-hold reason Btn_Validate btnCalendar-1 btnCalendar-2 ~
btnCalendar-3 btnCalendar-4 btnCalendar-5 rd_freight-terms carrier rd_fob pay-type ~
exp-date btnCalendar-6 vcode account ref 
&Scoped-Define DISPLAYED-OBJECTS contact prev-ord tb_man-inv fi_s-pct-lbl ~
fi_s-comm-lbl fi_sman-lbl fi_sname-lbl fi_sname-1 fi_sname-2 fi_sname-3 ~
sman-1 sman-2 sman-3 sales-per-1 sales-per-2 sales-per-3 comm-per-1 ~
comm-per-2 comm-per-3 req-code over-run under-run cCustNo cust-name ~
cust-address soldTo sold-name sold-address shipTo ship-name ship-address ~
tax-code terms terms-desc ord-date last-user due-date csr enterBy last-ship ~
pro-date prom-date po-rec-date hold-app-date ack-date cStatus tb_price-hold ~
reason rd_freight-terms carrier rd_fob pay-type exp-date vcode account ref 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 fi_sname-1 fi_sname-2 fi_sname-3 sman-1 sman-2 sman-3 
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 btnCalendar-3 ~
btnCalendar-4 btnCalendar-5 btnCalendar-6 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fBuildAddress V-table-Win 
FUNCTION fBuildAddress RETURNS CHARACTER
    ( ipcAdd1 AS CHARACTER, 
    ipcAdd2 AS CHARACTER, 
    ipcCity AS CHARACTER, 
    ipcState AS CHARACTER,
    ipcZip AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-3 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-4 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-5 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-6 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

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

DEFINE BUTTON Btn_Validate AUTO-END-KEY DEFAULT 
    LABEL "&Validate" 
    SIZE 20 BY 1.14
    BGCOLOR 8 .

DEFINE VARIABLE account          AS CHARACTER FORMAT "X(20)":U 
    LABEL "Account" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ack-date         AS DATE      FORMAT "99/99/9999":U 
    LABEL "Ack. Date" 
    VIEW-AS FILL-IN 
    SIZE 15.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE carrier          AS CHARACTER FORMAT "X(10)":U 
    LABEL "Carrier" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCustNo          AS CHARACTER FORMAT "X(8)":U 
    LABEL "Bill To" 
    VIEW-AS FILL-IN 
    SIZE 14.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE comm-per-1       AS DECIMAL   FORMAT ">>9.99":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 9.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE comm-per-2       AS DECIMAL   FORMAT ">>9.99":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 9.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE comm-per-3       AS DECIMAL   FORMAT ">>9.99":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 9.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE contact          AS CHARACTER FORMAT "X(32)":U 
    LABEL "Contact" 
    VIEW-AS FILL-IN 
    SIZE 27.2 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE csr              AS CHARACTER FORMAT "X(8)":U 
    LABEL "CSR" 
    VIEW-AS FILL-IN 
    SIZE 20.2 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cStatus          AS CHARACTER FORMAT "X(30)":U 
    LABEL "Status" 
    VIEW-AS FILL-IN 
    SIZE 27.2 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cust-name        AS CHARACTER FORMAT "X(30)":U 
    VIEW-AS FILL-IN 
    SIZE 42 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cust-address     AS CHARACTER FORMAT "X(50)":U 
    VIEW-AS FILL-IN 
    SIZE 57.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE due-date         AS DATE      FORMAT "99/99/9999":U 
    VIEW-AS FILL-IN 
    SIZE 15.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE enterBy          AS CHARACTER FORMAT "X(8)":U 
    LABEL "Entered By" 
    VIEW-AS FILL-IN 
    SIZE 20.2 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE exp-date         AS DATE      FORMAT "99/99/9999":U 
    LABEL "Exp" 
    VIEW-AS FILL-IN 
    SIZE 15.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_s-comm-lbl    AS CHARACTER FORMAT "X(256)":U INITIAL "Comm.%" 
    VIEW-AS FILL-IN 
    SIZE 11 BY .71
    FGCOLOR 9 NO-UNDO.

DEFINE VARIABLE fi_s-pct-lbl     AS CHARACTER FORMAT "X(256)":U INITIAL "% of Sales" 
    VIEW-AS FILL-IN 
    SIZE 14 BY .71
    FGCOLOR 9 NO-UNDO.

DEFINE VARIABLE fi_sman-lbl      AS CHARACTER FORMAT "X(256)":U INITIAL "Sales Rep" 
    VIEW-AS FILL-IN 
    SIZE 14 BY .71
    FGCOLOR 9 NO-UNDO.

DEFINE VARIABLE fi_sname-1       AS CHARACTER FORMAT "x(20)" 
    VIEW-AS FILL-IN 
    SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sname-2       AS CHARACTER FORMAT "x(20)" 
    VIEW-AS FILL-IN 
    SIZE 29 BY 1.

DEFINE VARIABLE fi_sname-3       AS CHARACTER FORMAT "x(20)" 
    VIEW-AS FILL-IN 
    SIZE 29 BY 1.

DEFINE VARIABLE fi_sname-lbl     AS CHARACTER FORMAT "X(256)":U INITIAL "Name" 
    VIEW-AS FILL-IN 
    SIZE 13 BY .71
    FGCOLOR 9 NO-UNDO.

DEFINE VARIABLE hold-app-date    AS DATE      FORMAT "99/99/9999":U 
    LABEL "Hold/Appr Date" 
    VIEW-AS FILL-IN 
    SIZE 15.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE last-ship        AS DATE      FORMAT "99/99/9999":U 
    LABEL "Last Ship" 
    VIEW-AS FILL-IN 
    SIZE 15.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE last-user        AS CHARACTER FORMAT "X(8)":U 
    LABEL "Last User" 
    VIEW-AS FILL-IN 
    SIZE 20.2 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ord-date         AS DATE      FORMAT "99/99/9999":U INITIAL ? 
    LABEL "Order Date" 
    VIEW-AS FILL-IN 
    SIZE 15.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE over-run         AS DECIMAL   FORMAT ">>9.99%":U INITIAL 0 
    LABEL "Overrun%" 
    VIEW-AS FILL-IN 
    SIZE 17.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE pay-type         AS CHARACTER FORMAT "X(10)":U 
    LABEL "Pay Type" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE po-rec-date      AS DATE      FORMAT "99/99/9999":U 
    LABEL "Po Received" 
    VIEW-AS FILL-IN 
    SIZE 15.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE prev-ord         AS CHARACTER FORMAT "X(15)":U 
    LABEL "Prev Order" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE pro-date         AS DATE      FORMAT "99/99/9999":U 
    LABEL "Production" 
    VIEW-AS FILL-IN 
    SIZE 15.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE prom-date        AS DATE      FORMAT "99/99/9999":U INITIAL ? 
    LABEL "Promise Date" 
    VIEW-AS FILL-IN 
    SIZE 15.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE reason           AS CHARACTER FORMAT "X(30)":U 
    LABEL "Reason" 
    VIEW-AS FILL-IN 
    SIZE 33.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ref              AS CHARACTER FORMAT "X(20)":U 
    LABEL "Ref#" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE req-code         AS CHARACTER FORMAT "XXXXX":U 
    LABEL "Due" 
    VIEW-AS FILL-IN 
    SIZE 7.8 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE sales-per-1      AS DECIMAL   FORMAT ">>9.99":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 9.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE sales-per-2      AS DECIMAL   FORMAT ">>9.99":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 9.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE sales-per-3      AS DECIMAL   FORMAT ">>9.99":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 9.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ship-name        AS CHARACTER FORMAT "X(30)":U 
    VIEW-AS FILL-IN 
    SIZE 42 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ship-address     AS CHARACTER FORMAT "X(50)":U 
    VIEW-AS FILL-IN 
    SIZE 57.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE shipTo           AS CHARACTER FORMAT "X(8)":U 
    LABEL "Ship To" 
    VIEW-AS FILL-IN 
    SIZE 14.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE sman-1           AS CHARACTER FORMAT "x(3)" 
    VIEW-AS FILL-IN 
    SIZE 7.8 BY 1 NO-UNDO.

DEFINE VARIABLE sman-2           AS CHARACTER FORMAT "x(3)" 
    VIEW-AS FILL-IN 
    SIZE 7.8 BY 1 NO-UNDO.

DEFINE VARIABLE sman-3           AS CHARACTER FORMAT "x(3)" 
    VIEW-AS FILL-IN 
    SIZE 7.8 BY 1 NO-UNDO.

DEFINE VARIABLE sold-name        AS CHARACTER FORMAT "X(30)":U 
    VIEW-AS FILL-IN 
    SIZE 42 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE sold-address     AS CHARACTER FORMAT "X(50)":U 
    VIEW-AS FILL-IN 
    SIZE 57.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE soldTo           AS CHARACTER FORMAT "X(8)":U 
    LABEL "Sold To" 
    VIEW-AS FILL-IN 
    SIZE 14.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE tax-code         AS CHARACTER FORMAT "X(3)":U 
    LABEL "Tax Code" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE terms            AS CHARACTER FORMAT "X(8)":U 
    LABEL "Pay Terms" 
    VIEW-AS FILL-IN 
    SIZE 14.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE terms-desc       AS CHARACTER FORMAT "X(30)":U 
    VIEW-AS FILL-IN 
    SIZE 42 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE under-run        AS DECIMAL   FORMAT ">>9.99%":U INITIAL 0 
    LABEL "Underrun%" 
    VIEW-AS FILL-IN 
    SIZE 17.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE vcode            AS CHARACTER FORMAT "X(10)":U 
    LABEL "VCode" 
    VIEW-AS FILL-IN 
    SIZE 9.2 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE rd_fob           AS CHARACTER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "DEST", "DEST",
    "ORIG", "ORIG"
    SIZE 27.6 BY 1.33 NO-UNDO.

DEFINE VARIABLE rd_freight-terms AS CHARACTER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Perpaid", "P",
    "Collect", "C",
    "Bill", "B",
    "3rd Party", "T"
    SIZE 53 BY 1.33 NO-UNDO.

DEFINE RECTANGLE RECT-21
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 19 BY 2.38
    BGCOLOR 15 .

DEFINE RECTANGLE RECT-31
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 78.8 BY 4.86.

DEFINE RECTANGLE RECT-38
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 158.8 BY 7.19
    BGCOLOR 15 .

DEFINE RECTANGLE RECT-41
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 79 BY 3.

DEFINE RECTANGLE RECT-42
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 78.8 BY 4.81.

DEFINE RECTANGLE RECT-43
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 79 BY 3.

DEFINE RECTANGLE RECT-44
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 79 BY 3.24.

DEFINE VARIABLE tb_man-inv    AS LOGICAL INITIAL NO 
    LABEL "Managed Inventory" 
    VIEW-AS TOGGLE-BOX
    SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_price-hold AS LOGICAL INITIAL NO 
    LABEL "Price Hold" 
    VIEW-AS TOGGLE-BOX
    SIZE 20 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    Btn_OK AT ROW 18.71 COL 142
    Btn_Done AT ROW 19 COL 143
    Btn_Cancel AT ROW 18.71 COL 151
    contact AT ROW 1.86 COL 89.8 COLON-ALIGNED WIDGET-ID 380
    prev-ord AT ROW 9.1 COL 55.4 COLON-ALIGNED WIDGET-ID 382
    tb_man-inv AT ROW 11.29 COL 50 WIDGET-ID 384
    fi_s-pct-lbl AT ROW 13.95 COL 45.2 COLON-ALIGNED NO-LABELS WIDGET-ID 390
    fi_s-comm-lbl AT ROW 13.95 COL 60.2 COLON-ALIGNED NO-LABELS WIDGET-ID 388
    fi_sman-lbl AT ROW 13.91 COL 1.2 COLON-ALIGNED NO-LABELS WIDGET-ID 392
    fi_sname-lbl AT ROW 13.91 COL 18.2 COLON-ALIGNED NO-LABELS WIDGET-ID 394
    fi_sname-1 AT ROW 14.91 COL 13.2 COLON-ALIGNED NO-LABELS WIDGET-ID 398
    fi_sname-2 AT ROW 15.91 COL 13.2 COLON-ALIGNED NO-LABELS WIDGET-ID 400
    fi_sname-3 AT ROW 16.91 COL 13.2 COLON-ALIGNED NO-LABELS WIDGET-ID 402
    sman-1 AT ROW 14.91 COL 1.2 COLON-ALIGNED NO-LABELS WIDGET-ID 404
    sman-2 AT ROW 15.91 COL 1.2 COLON-ALIGNED NO-LABELS WIDGET-ID 406
    sman-3 AT ROW 16.91 COL 1.2 COLON-ALIGNED NO-LABELS WIDGET-ID 408
    sales-per-1 AT ROW 15 COL 49 COLON-ALIGNED NO-LABELS WIDGET-ID 410
    sales-per-2 AT ROW 16 COL 49 COLON-ALIGNED NO-LABELS WIDGET-ID 412
    sales-per-3 AT ROW 17 COL 49 COLON-ALIGNED NO-LABELS WIDGET-ID 414
    comm-per-1 AT ROW 15 COL 60.6 COLON-ALIGNED NO-LABELS WIDGET-ID 416
    comm-per-2 AT ROW 16 COL 60.6 COLON-ALIGNED NO-LABELS WIDGET-ID 418
    comm-per-3 AT ROW 17 COL 60.6 COLON-ALIGNED NO-LABELS WIDGET-ID 420
    req-code AT ROW 2.95 COL 128.8 COLON-ALIGNED WIDGET-ID 422
    over-run AT ROW 9.19 COL 15 COLON-ALIGNED WIDGET-ID 426
    under-run AT ROW 10.29 COL 15 COLON-ALIGNED WIDGET-ID 428
    cCustNo AT ROW 1.86 COL 14.6 COLON-ALIGNED WIDGET-ID 436
    cust-name AT ROW 1.86 COL 30 COLON-ALIGNED NO-LABELS WIDGET-ID 438
    cust-address AT ROW 2.95 COL 14.6 COLON-ALIGNED NO-LABELS WIDGET-ID 440
    soldTo AT ROW 4.05 COL 14.6 COLON-ALIGNED WIDGET-ID 442
    sold-name AT ROW 4.05 COL 30 COLON-ALIGNED NO-LABELS WIDGET-ID 444
    sold-address AT ROW 5.14 COL 14.6 COLON-ALIGNED NO-LABELS WIDGET-ID 446
    shipTo AT ROW 6.24 COL 14.6 COLON-ALIGNED WIDGET-ID 452
    ship-name AT ROW 6.24 COL 30 COLON-ALIGNED NO-LABELS WIDGET-ID 448
    ship-address AT ROW 7.33 COL 14.6 COLON-ALIGNED NO-LABELS WIDGET-ID 450
    tax-code AT ROW 10.19 COL 55.4 COLON-ALIGNED WIDGET-ID 454
    terms AT ROW 12.24 COL 15 COLON-ALIGNED WIDGET-ID 458
    terms-desc AT ROW 12.24 COL 30 COLON-ALIGNED NO-LABELS WIDGET-ID 456
    ord-date AT ROW 1.86 COL 137 COLON-ALIGNED WIDGET-ID 462
    last-user AT ROW 2.95 COL 96.8 COLON-ALIGNED WIDGET-ID 464
    due-date AT ROW 2.95 COL 137 COLON-ALIGNED NO-LABELS WIDGET-ID 466
    csr AT ROW 4.05 COL 96.8 COLON-ALIGNED WIDGET-ID 468
    enterBy AT ROW 5.14 COL 96.8 COLON-ALIGNED WIDGET-ID 470
    last-ship AT ROW 4.05 COL 137 COLON-ALIGNED WIDGET-ID 472
    pro-date AT ROW 5.14 COL 137 COLON-ALIGNED WIDGET-ID 474
    prom-date AT ROW 6.24 COL 96.8 COLON-ALIGNED WIDGET-ID 476
    po-rec-date AT ROW 7.33 COL 96.8 COLON-ALIGNED WIDGET-ID 478
    hold-app-date AT ROW 6.24 COL 137 COLON-ALIGNED WIDGET-ID 480
    ack-date AT ROW 7.33 COL 137 COLON-ALIGNED WIDGET-ID 482
    cStatus AT ROW 8.91 COL 92 COLON-ALIGNED WIDGET-ID 484
    tb_price-hold AT ROW 10.38 COL 92 WIDGET-ID 486
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
    reason AT ROW 10.33 COL 121.6 COLON-ALIGNED WIDGET-ID 488
    Btn_Validate AT ROW 8.91 COL 129 WIDGET-ID 490
    btnCalendar-1 AT ROW 1.86 COL 154.8 WIDGET-ID 494
    btnCalendar-2 AT ROW 2.91 COL 154.8 WIDGET-ID 496
    btnCalendar-3 AT ROW 4 COL 154.8 WIDGET-ID 498
    btnCalendar-4 AT ROW 5.1 COL 154.8 WIDGET-ID 500
    btnCalendar-5 AT ROW 7.33 COL 114.6 WIDGET-ID 502
    rd_freight-terms AT ROW 12.14 COL 101.2 NO-LABELS WIDGET-ID 324
    carrier AT ROW 13.52 COL 91 COLON-ALIGNED WIDGET-ID 506
    rd_fob AT ROW 13.33 COL 130 NO-LABELS WIDGET-ID 508
    pay-type AT ROW 15.38 COL 93 COLON-ALIGNED WIDGET-ID 516
    exp-date AT ROW 15.29 COL 112.2 COLON-ALIGNED WIDGET-ID 520
    btnCalendar-6 AT ROW 15.29 COL 130 WIDGET-ID 518
    vcode AT ROW 15.38 COL 147 COLON-ALIGNED WIDGET-ID 522
    account AT ROW 16.91 COL 93 COLON-ALIGNED WIDGET-ID 524
    ref AT ROW 16.91 COL 129.4 COLON-ALIGNED WIDGET-ID 526
    "Order Header" VIEW-AS TEXT
    SIZE 16 BY .71 AT ROW 1 COL 5 WIDGET-ID 206
    "Freight Terms:" VIEW-AS TEXT
    SIZE 16 BY .71 AT ROW 12.38 COL 83 WIDGET-ID 504
    "Fob:" VIEW-AS TEXT
    SIZE 5.8 BY .71 AT ROW 13.62 COL 123.4 WIDGET-ID 512
    RECT-21 AT ROW 18.48 COL 141
    RECT-38 AT ROW 1.43 COL 1.2
    RECT-31 AT ROW 13.62 COL 1.2 WIDGET-ID 396
    RECT-41 AT ROW 8.71 COL 81 WIDGET-ID 10
    RECT-42 AT ROW 8.71 COL 1.2 WIDGET-ID 460
    RECT-43 AT ROW 11.91 COL 81 WIDGET-ID 492
    RECT-44 AT ROW 15.05 COL 81 WIDGET-ID 514
    SPACE(1.39) SKIP(2.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6
    TITLE "Edit Order Header".


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
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

/* SETTINGS FOR FILL-IN ack-date IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME Dialog-Frame
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME Dialog-Frame
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-3 IN FRAME Dialog-Frame
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-4 IN FRAME Dialog-Frame
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-5 IN FRAME Dialog-Frame
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-6 IN FRAME Dialog-Frame
   3                                                                    */
/* SETTINGS FOR FILL-IN cStatus IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust-address IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN enterBy IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
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
/* SETTINGS FOR FILL-IN hold-app-date IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN last-user IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
    rd_fob:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    rd_freight-terms:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

/* SETTINGS FOR FILL-IN ship-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship-address IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sman-1 IN FRAME Dialog-Frame
   2                                                                    */
/* SETTINGS FOR FILL-IN sman-2 IN FRAME Dialog-Frame
   2                                                                    */
/* SETTINGS FOR FILL-IN sman-3 IN FRAME Dialog-Frame
   2                                                                    */
/* SETTINGS FOR FILL-IN sold-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sold-address IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
    tb_man-inv:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    tb_price-hold:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

/* SETTINGS FOR FILL-IN terms-desc IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
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
ON HELP OF FRAME Dialog-Frame /* Edit Order Header */
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
ON RETURN OF FRAME Dialog-Frame /* Edit Order Header */
    ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Edit Order Header */
    DO:
                
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME account
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL account Dialog-Frame
ON LEAVE OF account IN FRAME Dialog-Frame /* Account */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ack-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ack-date Dialog-Frame
ON LEAVE OF ack-date IN FRAME Dialog-Frame /* Ack. Date */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 Dialog-Frame
ON CHOOSE OF btnCalendar-1 IN FRAME Dialog-Frame
    DO:
        {methods/btnCalendar.i ord-date }
        APPLY "entry" TO ord-date .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 Dialog-Frame
ON CHOOSE OF btnCalendar-2 IN FRAME Dialog-Frame
    DO:
        {methods/btnCalendar.i due-date }
        APPLY "entry" TO due-date .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-3 Dialog-Frame
ON CHOOSE OF btnCalendar-3 IN FRAME Dialog-Frame
    DO:
        {methods/btnCalendar.i last-ship }
        APPLY "entry" TO last-ship .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-4 Dialog-Frame
ON CHOOSE OF btnCalendar-4 IN FRAME Dialog-Frame
    DO:
        {methods/btnCalendar.i pro-date }
        APPLY "entry" TO pro-date .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-5 Dialog-Frame
ON CHOOSE OF btnCalendar-5 IN FRAME Dialog-Frame
    DO:
        {methods/btnCalendar.i po-rec-date }
        APPLY "entry" TO po-rec-date .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-6 Dialog-Frame
ON CHOOSE OF btnCalendar-6 IN FRAME Dialog-Frame
    DO:
        {methods/btnCalendar.i exp-date }
        APPLY "entry" TO exp-date .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ord-date Dialog-Frame
ON HELP OF ord-date IN FRAME Dialog-Frame /* order Date */
    DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL due-date Dialog-Frame
ON HELP OF due-date IN FRAME Dialog-Frame /* due Date */
    DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL last-ship Dialog-Frame
ON HELP OF last-ship IN FRAME Dialog-Frame /* last Date */
    DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pro-date Dialog-Frame
ON HELP OF pro-date IN FRAME Dialog-Frame /* pro Date */
    DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-rec-date Dialog-Frame
ON HELP OF po-rec-date IN FRAME Dialog-Frame /* Po Rec Date */
    DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL exp-date Dialog-Frame
ON HELP OF exp-date IN FRAME Dialog-Frame /* Exp Date */
    DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
    DO:
        
    
        /* IF AVAILABLE ttInputOrdLine THEN
             op-rowid = ROWID(ttInputOrdLine) .
 
         IF lv-item-recid NE ? THEN 
         DO:
             FIND FIRST ttInputOrdLine EXCLUSIVE-LOCK
                 WHERE RECID(ttInputOrdLine) EQ lv-item-recid  NO-ERROR.
             IF AVAILABLE ttInputOrdLine THEN DELETE ttInputOrdLine .
             op-rowid = ? .
         END. */
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
             ttInputOrdLine.lPurchased       = IF rd_freight-terms EQ "P" THEN TRUE ELSE FALSE
             .           
                     */
        //op-rowid = ROWID(ttInputOrdLine).
        
        APPLY "go" TO FRAME {&FRAME-NAME}.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Validate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Validate Dialog-Frame
ON CHOOSE OF Btn_Validate IN FRAME Dialog-Frame /* Validate */
    DO:
        
  &IF DEFINED (adm-panel) NE 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
        APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL carrier Dialog-Frame
ON LEAVE OF carrier IN FRAME Dialog-Frame /* Carrier */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustNo Dialog-Frame
ON LEAVE OF cCustNo IN FRAME Dialog-Frame /* Bill To */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME comm-per-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL comm-per-1 Dialog-Frame
ON LEAVE OF comm-per-1 IN FRAME Dialog-Frame
    DO:
           
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME comm-per-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL comm-per-2 Dialog-Frame
ON LEAVE OF comm-per-2 IN FRAME Dialog-Frame
    DO:
          
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME comm-per-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL comm-per-3 Dialog-Frame
ON LEAVE OF comm-per-3 IN FRAME Dialog-Frame
    DO:
           
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contact
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contact Dialog-Frame
ON LEAVE OF contact IN FRAME Dialog-Frame /* Contact */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME csr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL csr Dialog-Frame
ON LEAVE OF csr IN FRAME Dialog-Frame /* CSR */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStatus Dialog-Frame
ON LEAVE OF cStatus IN FRAME Dialog-Frame /* Status */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust-name Dialog-Frame
ON LEAVE OF cust-name IN FRAME Dialog-Frame
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust-address
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust-address Dialog-Frame
ON LEAVE OF cust-address IN FRAME Dialog-Frame
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL due-date Dialog-Frame
ON LEAVE OF due-date IN FRAME Dialog-Frame
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME enterBy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL enterBy Dialog-Frame
ON LEAVE OF enterBy IN FRAME Dialog-Frame /* Entered By */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME exp-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL exp-date Dialog-Frame
ON LEAVE OF exp-date IN FRAME Dialog-Frame /* Exp */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME hold-app-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL hold-app-date Dialog-Frame
ON LEAVE OF hold-app-date IN FRAME Dialog-Frame /* Hold/Appr Date */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME last-ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL last-ship Dialog-Frame
ON LEAVE OF last-ship IN FRAME Dialog-Frame /* Last Ship */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME last-user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL last-user Dialog-Frame
ON LEAVE OF last-user IN FRAME Dialog-Frame /* Last User */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ord-date Dialog-Frame
ON LEAVE OF ord-date IN FRAME Dialog-Frame /* Order Date */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME over-run
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL over-run Dialog-Frame
ON LEAVE OF over-run IN FRAME Dialog-Frame /* Overrun% */
    DO:
            
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pay-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pay-type Dialog-Frame
ON LEAVE OF pay-type IN FRAME Dialog-Frame /* Pay Type */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-rec-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-rec-date Dialog-Frame
ON LEAVE OF po-rec-date IN FRAME Dialog-Frame /* Po Received */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
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


&Scoped-define SELF-NAME pro-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pro-date Dialog-Frame
ON LEAVE OF pro-date IN FRAME Dialog-Frame /* Production */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prom-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prom-date Dialog-Frame
ON LEAVE OF prom-date IN FRAME Dialog-Frame /* Promise Date */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_fob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_fob Dialog-Frame
ON VALUE-CHANGED OF rd_fob IN FRAME Dialog-Frame
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_freight-terms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_freight-terms Dialog-Frame
ON VALUE-CHANGED OF rd_freight-terms IN FRAME Dialog-Frame
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME reason
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reason Dialog-Frame
ON LEAVE OF reason IN FRAME Dialog-Frame /* Reason */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ref Dialog-Frame
ON LEAVE OF ref IN FRAME Dialog-Frame /* Ref# */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME req-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL req-code Dialog-Frame
ON HELP OF req-code IN FRAME Dialog-Frame /* Due */
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
ON LEAVE OF req-code IN FRAME Dialog-Frame /* Due */
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
         
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sales-per-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sales-per-2 Dialog-Frame
ON LEAVE OF sales-per-2 IN FRAME Dialog-Frame
    DO:
          
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sales-per-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sales-per-3 Dialog-Frame
ON LEAVE OF sales-per-3 IN FRAME Dialog-Frame
    DO:
         
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ship-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ship-name Dialog-Frame
ON LEAVE OF ship-name IN FRAME Dialog-Frame
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ship-address
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ship-address Dialog-Frame
ON LEAVE OF ship-address IN FRAME Dialog-Frame
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shipTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipTo Dialog-Frame
ON LEAVE OF shipTo IN FRAME Dialog-Frame /* Ship To */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sold-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sold-name Dialog-Frame
ON LEAVE OF sold-name IN FRAME Dialog-Frame
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sold-address
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sold-address Dialog-Frame
ON LEAVE OF sold-address IN FRAME Dialog-Frame
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME soldTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL soldTo Dialog-Frame
ON LEAVE OF soldTo IN FRAME Dialog-Frame /* Sold To */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tax-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tax-code Dialog-Frame
ON LEAVE OF tax-code IN FRAME Dialog-Frame /* Tax Code */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME terms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL terms Dialog-Frame
ON LEAVE OF terms IN FRAME Dialog-Frame /* Pay Terms */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME terms-desc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL terms-desc Dialog-Frame
ON LEAVE OF terms-desc IN FRAME Dialog-Frame
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME under-run
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL under-run Dialog-Frame
ON LEAVE OF under-run IN FRAME Dialog-Frame /* Underrun% */
    DO:
            
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vcode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vcode Dialog-Frame
ON LEAVE OF vcode IN FRAME Dialog-Frame /* VCode */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
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
         
    RUN enable_UI.
    RUN display-item.

    ASSIGN 
        ll-order-warned                            = NO
        btn_done:HIDDEN IN FRAME {&FRAME-NAME}     = YES
        Btn_Validate:HIDDEN IN FRAME {&FRAME-NAME} = YES.   
   
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
            
    FIND FIRST ttInputOrd NO-LOCK NO-ERROR.         
    FIND FIRST eb NO-LOCK
        WHERE eb.company EQ cCompany
        AND eb.est-no EQ ttInputOrd.est-no NO-ERROR .
         
    /*ASSIGN
        contact         = oe-ord.contact
        prev-ord        = IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2  ELSE STRING(oe-ord.pord-no)
        tb_man-inv      = oe-ord.managed
        sman-1          = oe-ord.sman[1]
        sman-2          = oe-ord.sman[2]
        sman-3          = oe-ord.sman[3]
        fi_sname-1      = oe-ord.sname[1]
        fi_sname-2      = oe-ord.sname[2]
        fi_sname-3      = oe-ord.sname[3]
        sales-per-1     = oe-ord.s-pct[1]
        sales-per-2     = oe-ord.s-pct[2]
        sales-per-3     = oe-ord.s-pct[3]
        comm-per-1      = oe-ord.s-comm[1]
        comm-per-2      = oe-ord.s-comm[2]
        comm-per-3      = oe-ord.s-comm[3]
        req-code        = oe-ord.due-code
        over-run        = oe-ord.over-pct
        under-run       = oe-ord.under-pct
        cCustNo         = oe-ord.cust-no
        cust-name       = oe-ord.cust-name          
        soldTo          = oe-ord.sold-id
        sold-name       = oe-ord.sold-name         
        shipTo          = oe-ord.ship-id          
        tax-code        = oe-ord.tax-gr
        terms           = oe-ord.terms
        terms-desc      = oe-ord.terms-d
        ord-date        = oe-ord.ord-date
        last-user       = oe-ord.user-id
        due-date        = oe-ord.due-date
        csr             = oe-ord.csrUser_id
        enterBy         = oe-ord.entered-id
        last-ship       = oe-ord.last-date
        pro-date        = oe-ord.prod-date
        prom-date       = oe-ord.promiseDate
        po-rec-date     = oe-ord.poReceivedDate
        hold-app-date   = oe-ord.approved-date
        ack-date        = oe-ord.ack-prnt-date
         
        tb_price-hold   = oe-ord.priceHold
        reason          = oe-ord.priceHoldReason
        rd_freight-terms = oe-ord.frt-pay
        carrier         = oe-ord.carrier
        rd_fob          = oe-ord.fob-code
        pay-type        = oe-ord.cc-type
        exp-date        = oe-ord.cc-expiration
        vcode           = oe-ord.spare-char-1
        account         = oe-ord.cc-num
        ref             = oe-ord.cc-auth
        .    */
    ASSIGN
        contact          = ttInputOrd.contact
        prev-ord         = IF ttInputOrd.po-no2 NE "" THEN ttInputOrd.po-no2  ELSE STRING(ttInputOrd.pord-no)
        tb_man-inv       = ttInputOrd.managed
        sman-1           = ttInputOrd.sman[1]
        sman-2           = ttInputOrd.sman[2]
        sman-3           = ttInputOrd.sman[3]
        fi_sname-1       = ttInputOrd.sname[1]
        fi_sname-2       = ttInputOrd.sname[2]
        fi_sname-3       = ttInputOrd.sname[3]
        sales-per-1      = ttInputOrd.s-pct[1]
        sales-per-2      = ttInputOrd.s-pct[2]
        sales-per-3      = ttInputOrd.s-pct[3]
        comm-per-1       = ttInputOrd.s-comm[1]
        comm-per-2       = ttInputOrd.s-comm[2]
        comm-per-3       = ttInputOrd.s-comm[3]
        req-code         = ttInputOrd.due-code
        over-run         = ttInputOrd.over-pct
        under-run        = ttInputOrd.under-pct
        cCustNo          = ttInputOrd.cust-no
        cust-name        = ttInputOrd.cust-name          
        soldTo           = ttInputOrd.sold-id
        sold-name        = ttInputOrd.sold-name         
        shipTo           = ttInputOrd.ship-id          
        tax-code         = ttInputOrd.tax-gr
        terms            = ttInputOrd.terms
        terms-desc       = ttInputOrd.terms-d
        ord-date         = ttInputOrd.ord-date
        last-user        = ttInputOrd.user-id
        due-date         = ttInputOrd.due-date
        csr              = ttInputOrd.csrUser_id
        enterBy          = ttInputOrd.entered-id
        last-ship        = ttInputOrd.last-date
        pro-date         = ttInputOrd.prod-date
        prom-date        = ttInputOrd.promiseDate
        po-rec-date      = ttInputOrd.poReceivedDate
        hold-app-date    = ttInputOrd.approved-date
        ack-date         = ttInputOrd.ack-prnt-date
         
        tb_price-hold    = ttInputOrd.priceHold
        reason           = ttInputOrd.priceHoldReason
        rd_freight-terms = ttInputOrd.frt-pay
        carrier          = ttInputOrd.carrier
        rd_fob           = ttInputOrd.fob-code
        pay-type         = ttInputOrd.cc-type
        exp-date         = ttInputOrd.cc-expiration
        vcode            = ttInputOrd.spare-char-1
        account          = ttInputOrd.cc-num
        ref              = ttInputOrd.cc-auth
        .
    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ cCompany
        AND cust.cust-no EQ ttInputOrd.cust-no NO-ERROR.
              
    IF AVAILABLE cust THEN 
        cust-address    =  fBuildAddress(cust.addr[1],
            cust.addr[2],
            cust.city,
            cust.state,
            cust.zip).
    FIND FIRST soldto NO-LOCK
        WHERE soldto.company EQ cCompany
        AND soldto.cust-no EQ ttInputOrd.cust-no
        AND trim(soldto.sold-id) EQ trim(ttInputOrd.sold-id) NO-ERROR. 
    IF AVAILABLE soldto THEN
        sold-address    =  fBuildAddress(soldto.sold-addr[1],
            soldto.sold-addr[2],
            soldto.sold-city,
            soldto.sold-state,
            soldto.sold-zip).  
    FIND FIRST shipto NO-LOCK 
        WHERE shipto.company EQ cCompany 
        AND shipto.cust-no EQ ttInputOrd.cust-no
        AND TRIM(shipto.ship-id) = TRIM(ttInputOrd.ship-id)
        NO-ERROR.
    IF AVAILABLE shipto THEN 
    DO:
        ASSIGN                 
            ship-name    = shipto.ship-name
            ship-address = fBuildAddress(shipto.ship-addr[1],
                                                               shipto.ship-addr[2],
                                                               shipto.ship-city,
                                                               shipto.ship-state,
                                                               shipto.ship-zip).                                 
    END.   
      
    RUN oe/getStatusDesc.p (INPUT ttInputOrd.stat, OUTPUT cStatus).      
        
    DISPLAY   
        contact prev-ord tb_man-inv fi_s-pct-lbl fi_s-comm-lbl fi_sman-lbl 
        fi_sname-lbl fi_sname-1 fi_sname-2 fi_sname-3 sman-1 sman-2 sman-3 
        sales-per-1 sales-per-2 sales-per-3 comm-per-1 comm-per-2 comm-per-3 
        req-code over-run under-run cCustNo cust-name cust-address soldTo 
        sold-name sold-address shipTo ship-name ship-address tax-code terms 
        terms-desc ord-date last-user due-date csr enterBy last-ship pro-date 
        prom-date po-rec-date hold-app-date ack-date cStatus tb_price-hold 
        reason rd_freight-terms carrier rd_fob pay-type exp-date vcode account ref
        WITH FRAME Dialog-Frame.  
        
    ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.    

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
    DISPLAY contact prev-ord tb_man-inv fi_s-pct-lbl fi_s-comm-lbl fi_sman-lbl 
        fi_sname-lbl fi_sname-1 fi_sname-2 fi_sname-3 sman-1 sman-2 sman-3 
        sales-per-1 sales-per-2 sales-per-3 comm-per-1 comm-per-2 comm-per-3 
        req-code over-run under-run cCustNo cust-name cust-address soldTo 
        sold-name sold-address shipTo ship-name ship-address tax-code terms 
        terms-desc ord-date last-user due-date csr enterBy last-ship pro-date 
        prom-date po-rec-date hold-app-date ack-date cStatus tb_price-hold 
        reason rd_freight-terms carrier rd_fob pay-type exp-date vcode account ref 
        WITH FRAME Dialog-Frame.
    ENABLE Btn_OK Btn_Done Btn_Cancel contact prev-ord tb_man-inv sman-1 sman-2 
        sman-3 sales-per-1 sales-per-2 sales-per-3 comm-per-1 comm-per-2 
        comm-per-3 req-code over-run under-run RECT-21 RECT-38 RECT-31 RECT-41 
        cCustNo RECT-42 RECT-43 soldTo shipTo tax-code terms RECT-44 ord-date 
        due-date csr last-ship pro-date prom-date po-rec-date tb_price-hold 
        reason Btn_Validate btnCalendar-1 btnCalendar-2 btnCalendar-3 
        btnCalendar-4 btnCalendar-5 rd_freight-terms carrier rd_fob pay-type exp-date 
        btnCalendar-6 vcode account ref 
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fBuildAddress V-table-Win 
FUNCTION fBuildAddress RETURNS CHARACTER
    ( ipcAdd1 AS CHARACTER, 
    ipcAdd2 AS CHARACTER, 
    ipcCity AS CHARACTER, 
    ipcState AS CHARACTER,
    ipcZip AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Builds String for display in address fill-in
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cAddressLine AS CHARACTER NO-UNDO.
    cAddressLine = ipcAdd1.
    IF ipcAdd2 NE "" THEN cAddressLine = cAddressLine + " - " + ipcAdd2.
    cAddressLine = cAddressLine + " - " + ipcCity + ", " + ipcState + " " + ipcZip.
    RETURN cAddressLine. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
