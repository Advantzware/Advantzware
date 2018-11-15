&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-report NO-UNDO LIKE oe-ordl
    FIELD rec-id AS RECID.

&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: oe\d-ordrel.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ip-recid2 AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO .   /* add,update,view */
DEFINE OUTPUT PARAMETER op-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN 
    cocode = g_company.
ASSIGN 
    locode = g_loc.

DEFINE BUFFER ref-lot-no     FOR reftable.
DEFINE BUFFER ref-sell-price FOR reftable.



DEFINE VARIABLE char-hdl     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-rel-stat  AS cha       LABEL "" FORM "x" NO-UNDO.
DEFINE VARIABLE lv-rel-recid AS RECID     NO-UNDO.
DEFINE NEW SHARED BUFFER xoe-ordl FOR oe-ordl.
DEFINE NEW SHARED BUFFER xoe-ord  FOR oe-ord.
DEFINE NEW SHARED VARIABLE out-recid             AS RECID         NO-UNDO.
DEFINE NEW SHARED VARIABLE relh-recid            AS RECID         NO-UNDO.
DEFINE NEW SHARED VARIABLE v-auto                AS LOG           NO-UNDO.
DEFINE NEW SHARED VARIABLE nufile                AS LOG           NO-UNDO.   /* for jc-calc.p */
DEFINE NEW SHARED VARIABLE lv-qty                AS INTEGER       NO-UNDO.
DEFINE NEW SHARED VARIABLE fil_id                AS RECID         NO-UNDO.
DEFINE  VARIABLE li-ship-no            AS INTEGER       NO-UNDO.  /* if ship-to is changed */
DEFINE  VARIABLE v-inv-ship            AS LOG           INIT NO NO-UNDO .  /* Invoice only release */
DEFINE  VARIABLE v-qty-inv-only        AS INTEGER       NO-UNDO.
DEFINE  VARIABLE v-totqty-inv-only     AS INTEGER       NO-UNDO.

DEFINE  VARIABLE ll-unposted           AS LOG           NO-UNDO.
DEFINE  VARIABLE ls-po                 AS cha           NO-UNDO.
DEFINE  VARIABLE ll-canceled           AS LOG           NO-UNDO.
DEFINE  VARIABLE lv-stat               AS cha           NO-UNDO.
DEFINE  VARIABLE ld-date               AS DATE          NO-UNDO.
DEFINE  VARIABLE ll-skip               AS LOG           NO-UNDO.
DEFINE  VARIABLE lv-s-codes            AS CHARACTER     NO-UNDO.
DEFINE  VARIABLE lv-s-dscrs            AS CHARACTER     NO-UNDO.
DEFINE  VARIABLE lv-cust-x             LIKE cust.cust-no NO-UNDO.
DEFINE  VARIABLE ll-transfer           AS LOG           NO-UNDO.
DEFINE  VARIABLE v-browse-in-update    AS LOG           NO-UNDO.
DEFINE  VARIABLE v-cust-no             AS CHARACTER     NO-UNDO.
DEFINE  VARIABLE v-last-shipto         AS CHARACTER     NO-UNDO.
DEFINE  VARIABLE l-update-reason-perms AS LOG           NO-UNDO.
DEFINE  VARIABLE adm-cur-state         AS CHARACTER     NO-UNDO.
DEFINE  VARIABLE oeDateChange-log      AS LOG           NO-UNDO.
DEFINE  VARIABLE oeDateChange-char     AS CHARACTER     NO-UNDO.
DEFINE  VARIABLE v-rtn-char            AS CHARACTER     NO-UNDO.
DEFINE  VARIABLE v-rec-found           AS LOG           NO-UNDO.
DEFINE  VARIABLE v-disp-rel-qty        AS DECIMAL       NO-UNDO.
DEFINE  VARIABLE v-scr-s-code          AS CHARACTER     NO-UNDO.
DEFINE  VARIABLE vrRElh                AS ROWID         NO-UNDO.
DEFINE  VARIABLE oeDateAuto-log        AS LOG           NO-UNDO.
DEFINE  VARIABLE oeDateAuto-char       AS CHARACTER     NO-UNDO.
DEFINE  VARIABLE iTTReportLine         AS INTEGER       NO-UNDO.

DEFINE  VARIABLE iocPrompt             AS CHARACTER     NO-UNDO.
DEFINE  VARIABLE lr-rel-lib            AS HANDLE        NO-UNDO.

DEFINE  VARIABLE run-proc              AS CHARACTER.
DEFINE SHARED     VARIABLE Persistent-Handle     AS HANDLE.
DEFINE VARIABLE is-running            AS LOGICAL       NO-UNDO.
DEFINE VARIABLE phandle               AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE whColumn              AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE colNumber             AS INTEGER       NO-UNDO.
DEFINE VARIABLE dtPrevDueDate         AS DATE          NO-UNDO.
DEFINE VARIABLE cDueDateChgReason     AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lJustDeletedLine      AS LOGICAL       NO-UNDO.
DEFINE VARIABLE oeBolPrompt-char      AS CHARACTER     NO-UNDO .
DEFINE VARIABLE oeBolPrompt-log       AS LOGICAL       NO-UNDO .
DEFINE VARIABLE cRtnChar              AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lRecFound             AS LOGICAL       NO-UNDO .
DEFINE VARIABLE clvtext               AS CHARACTER     NO-UNDO .


RUN sys/ref/s-codes.p (OUTPUT lv-s-codes, OUTPUT lv-s-dscrs).

{oe/chkordl.i NEW}
{oe/relemail.i NEW}

DO TRANSACTION:
    {sys/inc/oeship.i}
    {sys/inc/oereleas.i}
    {sys/ref/relpost.i}
    {sys/inc/addxfer.i}
    {sys/inc/reltype.i}
END.
{sys/inc/funcToWorkDay.i}
DEFINE VARIABLE v-access-close AS LOG       NO-UNDO.
DEFINE VARIABLE v-access-list  AS CHARACTER NO-UNDO.

RUN methods/prgsecur.p
    (INPUT "OEDateChg",
    INPUT "ALL", /* based on run, create, update, delete or all */
    INPUT NO,    /* use the directory in addition to the program */
    INPUT NO,    /* Show a message if not authorized */
    INPUT NO,    /* Group overrides user security? */
    OUTPUT l-update-reason-perms, /* Allowed? Yes/NO */
    OUTPUT v-access-close, /* used in template/windows.i  */
    OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */


RUN sys/ref/nk1look.p (cocode, "oeDateChange", "L", NO, NO, "", "", 
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    oeDateChange-log = LOGICAL(v-rtn-char) NO-ERROR.

RUN sys/ref/nk1look.p (cocode, "oeDateChange", "C", NO, NO, "", "", 
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    oeDateChange-char = v-rtn-char NO-ERROR.
    
RUN sys/ref/nk1look.p (INPUT cocode, "OEDATEAUTO", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    oeDateAuto-log = LOGICAL(v-rtn-char) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "OEDATEAUTO", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    oeDateAuto-char = v-rtn-char NO-ERROR.    

RUN sys/ref/nk1look.p (INPUT cocode, "OEBOLPrompt", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    oeBolPrompt-log = LOGICAL(cRtnChar) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "OEBOLPrompt", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    oeBolPrompt-char = cRtnChar NO-ERROR. 


DEFINE VARIABLE lv-item-recid AS RECID   NO-UNDO.
DEFINE VARIABLE ll-new-record AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-rel tt-report

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame tt-report.opened oe-rel.s-code ~
oe-rel.ship-id oe-rel.stat oe-rel.carrier oe-rel.tot-qty oe-rel.qty ~
tt-report.po-no tt-report.lot-no tt-report.prom-date tt-report.stat ~
oe-rel.ship-addr[1] oe-rel.ship-city oe-rel.ship-state tt-report.price ~
tt-report.whsed tt-report.frt-pay ~
tt-report.flute oe-rel.spare-char-1 oe-rel.spare-char-2 oe-rel.spare-char-3 ~
tt-report.q-rel oe-rel.r-no oe-rel.link-no tt-report.job-start-date ~
tt-report.qty tt-report.prom-code tt-report.pr-uom 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame oe-rel.s-code oe-rel.ship-id ~
oe-rel.stat oe-rel.carrier oe-rel.tot-qty oe-rel.qty tt-report.po-no ~
tt-report.lot-no tt-report.prom-date tt-report.stat oe-rel.ship-addr[1] ~
oe-rel.ship-city oe-rel.ship-state tt-report.price tt-report.whsed ~
tt-report.frt-pay tt-report.flute ~
oe-rel.spare-char-1 oe-rel.spare-char-2 oe-rel.spare-char-3 tt-report.q-rel ~
oe-rel.r-no oe-rel.link-no tt-report.job-start-date tt-report.qty ~
tt-report.prom-code tt-report.pr-uom 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame oe-rel tt-report
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame oe-rel
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Dialog-Frame tt-report
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH oe-rel ~
      WHERE oe-rel.company eq cocode  SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH oe-rel ~
      WHERE oe-rel.company eq cocode  SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame oe-rel tt-report
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame oe-rel
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame tt-report


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-rel.s-code oe-rel.ship-id ~
oe-rel.stat oe-rel.carrier oe-rel.tot-qty oe-rel.qty tt-report.po-no ~
tt-report.lot-no tt-report.prom-date tt-report.stat oe-rel.ship-addr[1] ~
oe-rel.ship-city oe-rel.ship-state tt-report.price tt-report.whsed ~
tt-report.frt-pay tt-report.flute ~
oe-rel.spare-char-1 oe-rel.spare-char-2 oe-rel.spare-char-3 tt-report.q-rel ~
oe-rel.r-no oe-rel.link-no tt-report.job-start-date tt-report.qty ~
tt-report.prom-code tt-report.pr-uom 
&Scoped-define ENABLED-TABLES tt-report oe-rel 
&Scoped-define FIRST-ENABLED-TABLE tt-report
&Scoped-define SECOND-ENABLED-TABLE oe-rel

&Scoped-Define ENABLED-OBJECTS fi_discount fi_totprice btnCalendar-1 btnCalendar-2 ~
 Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 
&Scoped-Define DISPLAYED-FIELDS tt-report.opened oe-rel.s-code ~
oe-rel.ship-id oe-rel.stat oe-rel.carrier oe-rel.tot-qty oe-rel.qty ~
tt-report.po-no tt-report.lot-no tt-report.prom-date tt-report.stat ~
oe-rel.ship-addr[1] oe-rel.ship-city oe-rel.ship-state tt-report.price ~
tt-report.whsed  tt-report.frt-pay ~
tt-report.flute oe-rel.spare-char-1 oe-rel.spare-char-2 oe-rel.spare-char-3 ~
tt-report.q-rel oe-rel.r-no oe-rel.link-no tt-report.job-start-date ~
tt-report.qty tt-report.prom-code tt-report.pr-uom 
&Scoped-define DISPLAYED-TABLES tt-report oe-rel 
&Scoped-define FIRST-DISPLAYED-TABLE tt-report
&Scoped-define SECOND-DISPLAYED-TABLE oe-rel

&Scoped-Define DISPLAYED-OBJECTS fi_discount fi_totprice
&Scoped-define calendarPopup btnCalendar-1 btnCalendar-2
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-rel-qty Dialog-Frame 
FUNCTION get-rel-qty RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-rel-stat Dialog-Frame 
FUNCTION get-rel-stat RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-tot-qty Dialog-Frame 
FUNCTION get-tot-qty RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

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

DEFINE BUTTON Btn_Cancel 
    IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
    LABEL "Cancel" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
    IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
    LABEL "&Done" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
    IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
    LABEL "&Save" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE RECTANGLE RECT-21
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL ROUNDED  
    SIZE 19 BY 2.38
    BGCOLOR 15.

DEFINE RECTANGLE RECT-38
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED
    SIZE 142.7 BY 12.86
    BGCOLOR 15.

DEFINE VARIABLE fi_discount AS DECIMAL FORMAT ">>>,>>9.99":U 
    LABEL "Discount" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_totprice AS DECIMAL FORMAT "->>,>>>,>>9.99":U 
    LABEL "Ext Order Price" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
    oe-rel, 
    tt-report SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    oe-rel.s-code AT ROW 2.43 COL 63 COLON-ALIGNED
    LABEL "S" 
    VIEW-AS COMBO-BOX INNER-LINES 4
    LIST-ITEM-PAIRS "B-Both","B",
    "S-Ship","S",
    "I-Invoice","I",
    "T-Transfer","T"
    DROP-DOWN-LIST
    SIZE 20 BY 1
    BGCOLOR 15 FONT 1
    tt-report.opened AT ROW 2.43 COL 20 COLON-ALIGNED
    LABEL "Prt" FORMAT "Y/N"
    VIEW-AS FILL-IN 
    SIZE 20 BY 1
    BGCOLOR 15 FONT 1
    oe-rel.ship-id AT ROW 2.43 COL 106.2 COLON-ALIGNED
    LABEL "Ship To" FORMAT "x(8)"
    VIEW-AS FILL-IN 
    SIZE 24 BY 1
    BGCOLOR 15 FONT 1
    oe-rel.stat AT ROW 3.57 COL 20 COLON-ALIGNED
    VIEW-AS COMBO-BOX INNER-LINES 8
    LIST-ITEM-PAIRS "S-Scheduled","S",
    "L-Late","L",
    "I-Invoice Ready","I",
    "A-Actual","A",
    "P-Posted","P",
    "B-Backorder","B",
    "Z-Posted BOL","Z",
    "C-Completed","C"
    DROP-DOWN-LIST
    SIZE 20 BY 1
    BGCOLOR 15 FONT 1
    oe-rel.carrier AT ROW 3.57 COL 63 COLON-ALIGNED
    LABEL "Via" FORMAT "x(5)"
    VIEW-AS FILL-IN 
    SIZE 24 BY 1
    BGCOLOR 15 FONT 1
    oe-rel.tot-qty AT ROW 3.57 COL 106.2 COLON-ALIGNED
    LABEL "Sched Qty" FORMAT "->>,>>>,>>9"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    oe-rel.qty AT ROW 4.71 COL 20 COLON-ALIGNED
    LABEL "Actual Qty" FORMAT "->>,>>>,>>9"
    VIEW-AS FILL-IN 
    SIZE 20 BY 1
    BGCOLOR 15 FONT 1
    tt-report.po-no AT ROW 4.71 COL 63 COLON-ALIGNED
    LABEL "Customer PO#" FORMAT "x(15)"
    VIEW-AS FILL-IN 
    SIZE 24 BY 1
    BGCOLOR 15 FONT 1
    tt-report.lot-no AT ROW 4.71 COL 106.2 COLON-ALIGNED
    LABEL "Customer Lot #" FORMAT "x(15)"
    VIEW-AS FILL-IN 
    SIZE 24 BY 1
    BGCOLOR 15 FONT 1
    tt-report.stat AT ROW 5.86 COL 20 COLON-ALIGNED FORMAT "99/99/9999"
    LABEL "Rel Date" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 FONT 1
    btnCalendar-1 AT ROW 5.81 COL 37
    oe-rel.ship-addr[1] AT ROW 5.86 COL 63 COLON-ALIGNED
    LABEL "Ship To Address" FORMAT "x(26)"
    VIEW-AS FILL-IN 
    SIZE 24 BY 1
    BGCOLOR 15 FONT 1
    oe-rel.ship-city AT ROW 5.86 COL 106.2 COLON-ALIGNED
    LABEL "City" FORMAT "x(15)"
    VIEW-AS FILL-IN 
    SIZE 24 BY 1
    BGCOLOR 15 FONT 1
    oe-rel.ship-state AT ROW 7 COL 20 COLON-ALIGNED
    LABEL "State" FORMAT "x(15)"
    VIEW-AS FILL-IN 
    SIZE 20 BY 1
    BGCOLOR 15 FONT 1
    tt-report.price AT ROW 7 COL 63 COLON-ALIGNED
    LABEL "Sell Price" FORMAT ">>,>>>,>>9.99<<<<"
    VIEW-AS FILL-IN 
    SIZE 24 BY 1
    BGCOLOR 15 FONT 1
    tt-report.whsed AT ROW 7 COL 106.2 COLON-ALIGNED
    LABEL "$0" FORMAT "Y/N"
    VIEW-AS FILL-IN 
    SIZE 14 BY 1
    BGCOLOR 15 FONT 1
    fi_discount AT ROW 8.14 COL 20 COLON-ALIGNED
    BGCOLOR 15 FONT 1
    fi_totprice AT ROW 8.14 COL 63 COLON-ALIGNED
    BGCOLOR 15 FONT 1
    tt-report.frt-pay AT ROW 8.14 COL 106.2 COLON-ALIGNED
    LABEL "Frt Pay" FORMAT "x(8)"
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 FONT 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
    tt-report.flute AT ROW 8.14 COL 128.6 COLON-ALIGNED
    LABEL "FOB" FORMAT "x(1)"
    VIEW-AS FILL-IN 
    SIZE 8 BY 1
    BGCOLOR 15 FONT 1
    oe-rel.spare-char-1 AT ROW 9.29 COL 20 COLON-ALIGNED
    LABEL "Ship From" FORMAT "x(5)"
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 FONT 1
    oe-rel.spare-char-2 AT ROW 9.29 COL 63 COLON-ALIGNED
    LABEL "Dt Chg Reason" FORMAT "x(30)"
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 FONT 1
    oe-rel.spare-char-3 AT ROW 9.29 COL 106.2 COLON-ALIGNED
    LABEL "Dt Chg User" FORMAT "x(8)"
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 FONT 1
    tt-report.q-rel AT ROW 10.43 COL 20 COLON-ALIGNED
    LABEL "Release #" FORMAT ">>>>>>9"
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 FONT 1
    oe-rel.r-no AT ROW 10.43 COL 63 COLON-ALIGNED
    LABEL "Seq. #" FORMAT ">>>>>>>>9"
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 FONT 1
    oe-rel.link-no AT ROW 10.43 COL 106.2 COLON-ALIGNED
    LABEL "Int. Release" FORMAT ">>>>>>>9"
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 FONT 1
    tt-report.job-start-date AT ROW 11.57 COL 20 COLON-ALIGNED
    LABEL "Shp Date" FORMAT "99/99/9999"
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 FONT 1
    tt-report.qty AT ROW 11.57 COL 63 COLON-ALIGNED
    LABEL "Quantity" FORMAT "->>,>>>,>>9.9<<"
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 FONT 1
    tt-report.prom-date AT ROW 11.57 COL 106.2 COLON-ALIGNED FORMAT "99/99/9999"
    LABEL "Due Date" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 FONT 1
    btnCalendar-2 AT ROW 11.57 COL 123.2
    tt-report.prom-code AT ROW 12.71 COL 63 COLON-ALIGNED
    LABEL "Due Dt Chg Usr" FORMAT "x(5)"
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 FONT 1
    tt-report.pr-uom AT ROW 12.71 COL 20.2 COLON-ALIGNED
    LABEL "Due Dt Chg Rsn" FORMAT "x(4)"
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 FONT 1
    Btn_OK AT ROW 15.13 COL 126
    Btn_Done AT ROW 15.11 COL 130
    Btn_Cancel AT ROW 15.13 COL 135
    RECT-21 AT ROW 14.80 COL 125
    RECT-38 AT ROW 1.48 COL 2.1
    SPACE(0.99) SKIP(3.27)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6
    TITLE "Release - Order Maintenance".


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
/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN oe-rel.carrier IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */

/* SETTINGS FOR FILL-IN tt-report.flute IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN tt-report.frt-pay IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN tt-report.job-start-date IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-rel.link-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN tt-report.lot-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN tt-report.opened IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT  no-enable                                               */
/* SETTINGS FOR FILL-IN tt-report.po-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN tt-report.pr-uom IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN tt-report.price IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN tt-report.prom-code IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN tt-report.prom-date IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN tt-report.q-rel IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN tt-report.qty IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-rel.qty IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-rel.r-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-rel.ship-addr[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-rel.ship-city IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-rel.ship-id IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-rel.ship-state IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-rel.spare-char-1 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-rel.spare-char-2 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-rel.spare-char-3 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN tt-report.stat IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-rel.tot-qty IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN tt-report.whsed IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fi_discount IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fi_totprice IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "oe-rel,Temp-Tables.tt-report "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "oe-rel.company eq cocode "
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON CTRL-O OF FRAME Dialog-Frame
    OR ctrl-o OF btnCalendar-1 ANYWHERE
    DO:
        DEFINE VARIABLE char-hdl AS CHARACTER.
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Release - Order Maintenance */
    DO:
        DEFINE VARIABLE char-val AS cha           NO-UNDO.
        DEFINE VARIABLE rec-val  AS RECID         NO-UNDO.
        DEFINE VARIABLE lw-focus AS WIDGET-HANDLE NO-UNDO.

        IF NOT AVAILABLE oe-rel AND lv-rel-recid NE ? THEN
            FIND oe-rel WHERE RECID(oe-rel) EQ lv-rel-recid NO-LOCK.

        lw-focus = FOCUS.
        CASE lw-focus:NAME:
            WHEN "ship-id" THEN 
                DO:
                    FIND oe-ord NO-LOCK
                        WHERE oe-ord.company EQ oe-rel.company 
                        AND oe-ord.ord-no  EQ oe-rel.ord-no.
                    IF oe-rel.s-code:SCREEN-VALUE EQ "T" AND lv-cust-x NE "" THEN
                        RUN windows/l-shipt3.w (g_company, g_loc, oe-ord.cust-no, lw-focus:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
                    ELSE
                        RUN windows/l-shipt2.w (g_company, g_loc, oe-ord.cust-no, lw-focus:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
                    FIND shipto WHERE RECID(shipto) EQ rec-val NO-LOCK NO-ERROR. 
                    IF AVAILABLE shipto AND lw-focus:SCREEN-VALUE NE shipto.ship-id THEN 
                    DO:
                        lw-focus:SCREEN-VALUE = shipto.ship-id.
                        RUN new-ship-id.
                    END.
                END.
            WHEN "carrier" THEN 
                DO:
                    RUN windows/l-carrie.w (g_company, oe-rel.spare-char-1:SCREEN-VALUE, lw-focus:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
                    RETURN NO-APPLY.
                END.
            WHEN "s-code" THEN 
                DO:
                    RUN windows/l-cddscr.w ("Release Types", lv-s-codes, lv-s-dscrs, lw-focus:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val NE "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
                END.

            WHEN "zero-sprice" THEN
                RETURN NO-APPLY.         

            WHEN "spare-char-1" THEN 
                DO:
                    RUN windows/l-loc.w  (g_company,FOCUS:SCREEN-VALUE, OUTPUT char-val). 
                    IF char-val <> "" THEN 
                        FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = ENTRY(1,char-val).
                END.
            WHEN "spare-char-2" THEN 
                DO:
                    RUN windows/l-rejpo.w  (g_company,FOCUS:SCREEN-VALUE, OUTPUT char-val). 
                    IF char-val <> "" THEN 
                        FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = ENTRY(1,char-val).
                END.
        END CASE.

        RETURN NO-APPLY.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Release - Order Maintenance */
    ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Release - Order Maintenance */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 Dialog-Frame
ON CHOOSE OF btnCalendar-1 IN FRAME Dialog-Frame
    DO:
    {methods/btnCalendar.i tt-report.stat }
        APPLY "entry" TO tt-report.stat .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 Dialog-Frame
ON CHOOSE OF btnCalendar-2 IN FRAME Dialog-Frame
    DO:
    {methods/btnCalendar.i tt-report.prom-date}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
    DO:
        DISABLE TRIGGERS FOR LOAD OF oe-rel .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND oe-rel EXCLUSIVE-LOCK
                WHERE RECID(oe-rel) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE oe-rel THEN DELETE oe-rel .
       
            FIND FIRST tt-report EXCLUSIVE-LOCK
                WHERE tt-report.rec-id EQ RECID(oe-rel)
                NO-ERROR.
            IF AVAILABLE tt-report THEN DELETE tt-report .
        END.
        ELSE 
        DO:
            op-rowid = IF AVAILABLE oe-rel THEN ROWID(oe-rel) ELSE ? .
        END.
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
    DO:
        op-rowid = ROWID(oe-rel).

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
        DEFINE VARIABLE v-qty    AS DECIMAL NO-UNDO.
        DEFINE VARIABLE ll       AS LOGICAL NO-UNDO.
        DEFINE VARIABLE op-error AS LOGICAL NO-UNDO.

        DEFINE BUFFER bf-rel FOR oe-rel .
        DEFINE VARIABLE lv-repos-recid        AS RECID     NO-UNDO.
        DEFINE VARIABLE lv-key-02             LIKE tt-report.stat NO-UNDO.
        DEFINE VARIABLE lv-printed            LIKE tt-report.opened NO-UNDO.
        DEFINE VARIABLE char-hdl              AS cha       NO-UNDO.
        DEFINE VARIABLE i                     AS INTEGER   NO-UNDO.
        DEFINE VARIABLE v-qty-inv             AS INTEGER   INIT 0 NO-UNDO.
        DEFINE VARIABLE v-qty-ship            AS INTEGER   INIT 0 NO-UNDO.
        DEFINE VARIABLE v-date-change-reason  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE v-added-rowid         AS ROWID     NO-UNDO.
        DEFINE VARIABLE v-nxt-r-no            AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lMatchingSRecordFound AS LOG       NO-UNDO.
        DEFINE BUFFER bf-add-oe-rel FOR oe-rel.
        DEFINE VARIABLE cPreRelDate AS CHARACTER NO-UNDO.

        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
        /* Code placed here will execute PRIOR to standard behavior. */

        /* Code placed here will execute PRIOR to standard behavior. */
        RUN valid-s-code NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-ship-id NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      
        RUN valid-po-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-key-02 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
        IF oeDateAuto-log AND OeDateAuto-Char = "Colonial" THEN
            RUN valid-colonial-date NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    
        RUN valid-freight-pay NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-fob NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   
        RUN valid-ship-from NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-date-change NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  
        /* Task 11041309 */
        IF ll-new-record  THEN 
        DO:
            FOR EACH bf-rel WHERE bf-rel.company = oe-ord.company
                AND bf-rel.ord-no = oe-ord.ord-no
                AND bf-rel.i-no = oe-ordl.i-no 
                AND bf-rel.LINE = oe-ordl.LINE
                NO-LOCK:

                IF bf-rel.s-code = "" OR CAN-DO("I",bf-rel.s-code) THEN
                    ASSIGN
                        v-qty-inv = v-qty-inv + bf-rel.qty .
                IF bf-rel.s-code = "" OR CAN-DO("S",bf-rel.s-code) THEN
                    ASSIGN
                        v-qty-ship = v-qty-ship + bf-rel.qty .
            END.

            IF  oe-rel.s-code:SCREEN-VALUE EQ "B"  THEN 
            DO:
                IF  v-qty-inv > (v-qty-ship ) THEN 
                DO:
                    MESSAGE "Invoice Only Quantity Exceeds Ship Only Quantity." +
                        "Ship Only Release should be added to Offset Invoice Only Quantity." +
                        "Please Change Release Type to S for Ship Only."
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ansp AS LOG.
                    IF ll-ansp THEN 
                    DO:
                        APPLY "entry" TO oe-rel.s-code IN FRAME {&FRAME-NAME}.
                        RETURN NO-APPLY.
                    END.   
                END.
            END.
            IF  oe-rel.s-code:SCREEN-VALUE EQ "I"  THEN 
            DO:
                IF  (v-qty-inv + INT(oe-rel.qty:SCREEN-VALUE)) > oe-ordl.qty THEN 
                DO:
                    MESSAGE "Invoice Only Quantity Exceeds Order Quantity." +
                        " Do you want continue.."
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-invex AS LOG.
                    IF NOT ll-invex THEN 
                    DO:
                        APPLY "entry" TO oe-rel.qty IN FRAME {&FRAME-NAME}.
                        RETURN NO-APPLY.
                    END.   
                END.
            END.
        END.  /* Task 11041309 */

        FIND FIRST bf-rel WHERE bf-rel.company = oe-rel.company 
            AND bf-rel.ord-no = oe-rel.ord-no 
            AND bf-rel.line = oe-rel.line
            AND bf-rel.rel-date = ld-date
            AND bf-rel.ship-id = oe-rel.ship-id:SCREEN-VALUE
            /*bf-rel.ship-no = oe-rel.ship-no:screen-value */
            AND bf-rel.i-no = oe-rel.i-no
            AND recid(bf-rel) <> recid(oe-rel)
            NO-LOCK NO-ERROR.
        IF AVAILABLE bf-rel THEN 
        DO:
            MESSAGE "Shipto already exists for this date. Continue with unique PO#?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
            IF NOT ll-ans THEN 
            DO:
                APPLY "entry" TO oe-rel.s-code.
                RETURN NO-APPLY.
            END.   
        END.

 
        ASSIGN
            lv-repos-recid = RECID(oe-rel)
            lv-key-02      = tt-report.stat
            lv-printed     = tt-report.opened
            ll-skip        = YES
            dtPrevDueDate  = tt-report.prom-date
            cPreRelDate    = STRING(tt-report.stat)
            .

        RUN release-shared-buffers.

        /* Dispatch standard ADM method.                             */
        /* RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .*/
        DO TRANSACTION:
            FIND CURRENT oe-rel EXCLUSIVE-LOCK NO-ERROR.
            FIND CURRENT tt-report EXCLUSIVE-LOCK NO-ERROR.
            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
            END.
        END.

        RUN pUpdate-record(INPUT cPreRelDate).

        /* Code placed here will execute AFTER standard behavior.    */
        v-last-shipto = oe-rel.ship-id:SCREEN-VALUE.

        RUN release-shared-buffers.

        ll-skip = NO.

        IF tt-report.opened NE lv-printed THEN 
        DO:
        {oe/rel-stat.i lv-stat}

            IF AVAILABLE oe-relh AND INDEX("AB",lv-stat) GT 0 THEN 
            DO TRANSACTION:
                FIND CURRENT oe-relh.
                oe-relh.printed = tt-report.opened.
                FIND CURRENT oe-relh NO-LOCK.
            END.
        END.
  
        IF oeDateChange-log 
            AND  NOT ll-new-record
            AND  LOOKUP("Release Due Date", oeDateChange-char) GT 0
            AND  tt-report.prom-date NE dtPrevDueDate 
            AND  dtPrevDueDate NE ? 
            THEN 
        DO:
            FIND FIRST bf-rel WHERE ROWID(bf-rel) EQ ROWID(oe-rel) EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE bf-rel THEN 
            DO:    
                RUN oe/d-pdcnot.w /* PERSISTENT SET h_reasonWin */
                    (INPUT bf-rel.rec_key, INPUT "L", INPUT "", INPUT "", INPUT 0, INPUT "DRC", INPUT "",
                    OUTPUT v-date-change-reason, OUTPUT v-added-rowid)  .
                IF v-date-change-reason NE ? THEN
                    ASSIGN bf-rel.spare-char-4 = STRING(tt-report.prom-date) + "," 
                   + USERID("NOSWEAT") + "," + v-date-change-reason.  
            END. /* If avail bf-rel */
        END. /* If oeDateChange-log */
  
        IF tt-report.stat NE lv-key-02 AND NOT ll-new-record THEN 
        DO:  
            RUN update-dates.
        END.
  
        ASSIGN
            lv-rel-recid       = ?
            v-browse-in-update = NO.

        op-rowid = ROWID(oe-rel).

  

        APPLY "go" TO FRAME {&FRAME-NAME}.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rel.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.carrier Dialog-Frame
ON ENTRY OF oe-rel.carrier IN FRAME Dialog-Frame /* Via */
    DO:
        IF INDEX("AB",get-rel-stat()) GT 0 THEN 
        DO:
            APPLY "tab" TO {&self-name} IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.carrier Dialog-Frame
ON LEAVE OF oe-rel.carrier IN FRAME Dialog-Frame /* Via */
    DO:
        IF LASTKEY = -1 THEN RETURN.
    
        FIND FIRST carrier WHERE carrier.company = g_company AND
            carrier.carrier = SELF:screen-value
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE carrier THEN 
        DO:
            MESSAGE "Invalid Carrier. Try Help. " VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-report.flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.flute Dialog-Frame
ON ENTRY OF tt-report.flute IN FRAME Dialog-Frame /* FOB */
    DO:
        IF INDEX("AB",get-rel-stat()) GT 0 THEN 
        DO:
            APPLY "tab" TO {&self-name} IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.flute Dialog-Frame
ON LEAVE OF tt-report.flute IN FRAME Dialog-Frame /* FOB */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-fob NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END. 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-report.frt-pay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.frt-pay Dialog-Frame
ON ENTRY OF tt-report.frt-pay IN FRAME Dialog-Frame /* Frt Pay */
    DO:
        IF INDEX("AB",get-rel-stat()) GT 0 THEN 
        DO:
            APPLY "tab" TO {&self-name} IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.frt-pay Dialog-Frame
ON LEAVE OF tt-report.frt-pay IN FRAME Dialog-Frame /* Frt Pay */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-freight-pay NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END. 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-report.lot-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.lot-no Dialog-Frame
ON ENTRY OF tt-report.lot-no IN FRAME Dialog-Frame /* Customer Lot # */
    DO:
        IF INDEX("AB",get-rel-stat()) GT 0 THEN 
        DO:
            APPLY "tab" TO {&self-name} IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-report.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.po-no Dialog-Frame
ON ENTRY OF tt-report.po-no IN FRAME Dialog-Frame /* Customer PO# */
    DO:
        IF INDEX("AB",get-rel-stat()) GT 0 THEN 
        DO:
            APPLY "tab" TO {&self-name} IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.po-no Dialog-Frame
ON LEAVE OF tt-report.po-no IN FRAME Dialog-Frame /* Customer PO# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-po-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-report.price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.price Dialog-Frame
ON LEAVE OF tt-report.price IN FRAME Dialog-Frame /* Sell Price */
    DO:
        IF KEYFUNCTION(LASTKEY) EQ "BACK-TAB" THEN
        DO:
            IF DEC(tt-report.price:SCREEN-VALUE) <> 0 THEN
                ASSIGN tt-report.whsed:SCREEN-VALUE = "N".

            APPLY "ENTRY" TO tt-report.stat IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.

        IF DEC(tt-report.price:SCREEN-VALUE) <> 0 THEN
        DO:
            ASSIGN 
                tt-report.whsed:SCREEN-VALUE = "N".

            IF LASTKEY NE -1 THEN
            DO:
                APPLY 'entry' TO tt-report.frt-pay.
                RETURN NO-APPLY.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-report.prom-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.prom-date Dialog-Frame
ON LEAVE OF tt-report.prom-date IN FRAME Dialog-Frame /* Due Date */
    DO:
        IF LASTKEY NE -1 AND oeDateAuto-log AND OeDateAuto-Char = "Colonial" THEN 
        DO:
        {custom/pastDatePrompt.i SELF:SCREEN-VALUE} 

            RUN valid-colonial-date NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            IF INDEX("AB",lv-stat) GT 0 THEN 
            DO: 
                IF KEYFUNCTION(LASTKEY) EQ "BACK-TAB" THEN RETURN NO-APPLY.
                ELSE APPLY "choose" TO Btn_OK.  /*RUN dispatch ("update-record").*/
            END.

        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.prom-date Dialog-Frame
ON VALUE-CHANGED OF tt-report.prom-date IN FRAME Dialog-Frame /* Due Date */
    DO:
        IF oeDateAuto-log AND oeDateAuto-char EQ "Colonial" THEN 
            RUN new-due-date.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rel.s-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.s-code Dialog-Frame
ON ENTRY OF oe-rel.s-code IN FRAME Dialog-Frame /* S/I */
    DO:
        IF INDEX("AB",get-rel-stat()) GT 0 OR ll-transfer THEN 
        DO:
            /*  APPLY "tab" TO {&self-name} IN FRAME {&FRAME-NAME}.*/
            RETURN NO-APPLY.
        END.

    /*RUN calendarPlacement.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.s-code Dialog-Frame
ON LEAVE OF oe-rel.s-code IN FRAME Dialog-Frame /* S/I */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-code NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END. 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rel.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.ship-id Dialog-Frame
ON ENTRY OF oe-rel.ship-id IN FRAME Dialog-Frame /* Ship To */
    DO:
        IF INDEX("AB",get-rel-stat()) GT 0 THEN 
        DO:
            APPLY "tab" TO {&self-name} IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.ship-id Dialog-Frame
ON LEAVE OF oe-rel.ship-id IN FRAME Dialog-Frame /* Ship To */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-ship-id NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.             
  
        IF adm-adding-record THEN
            RUN new-ship-id.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.ship-id Dialog-Frame
ON VALUE-CHANGED OF oe-rel.ship-id IN FRAME Dialog-Frame /* Ship To */
    DO:
        RUN new-ship-id.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rel.spare-char-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.spare-char-1 Dialog-Frame
ON LEAVE OF oe-rel.spare-char-1 IN FRAME Dialog-Frame /* Ship From */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-ship-from NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rel.spare-char-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.spare-char-2 Dialog-Frame
ON ENTRY OF oe-rel.spare-char-2 IN FRAME Dialog-Frame /* Dt Chg Reason */
    DO:
    /* Can be implemented at a later date to display a button to show notes */
    /*     bt_addnote:FRAME = FRAME {&FRAME-NAME}:HANDLE. */
    /*     bt_addnote:VISIBLE = TRUE.                     */
    /*     bt_addnote:SENSITIVE = TRUE.                   */
    /*     bt_addnote:X = SELF:X + 1.                     */
    /*     bt_addnote:Y = SELF:Y + 14.                    */

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.spare-char-2 Dialog-Frame
ON LEAVE OF oe-rel.spare-char-2 IN FRAME Dialog-Frame /* Dt Chg Reason */
    DO:

        /*      Could be implemented at a later date */
        /*       bt_addnote:VISIBLE = FALSE. */
        /*     bt_addnote:SENSITIVE = FALSE. */
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-date-change NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
        oe-rel.spare-char-3:SCREEN-VALUE IN FRAME Dialog-Frame = USERID("NOSWEAT").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rel.stat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.stat Dialog-Frame
ON VALUE-CHANGED OF oe-rel.stat IN FRAME Dialog-Frame /* S */
    DO:
        DEFINE VARIABLE ll-create-oe-rell AS LOG NO-UNDO.

        FIND FIRST oe-rell
            WHERE oe-rell.company  EQ oe-rel.company
            AND oe-rell.r-no     EQ oe-rel.link-no
            AND oe-rell.ord-no   EQ oe-rel.ord-no
            AND oe-rell.rel-no   EQ oe-rel.rel-no
            AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
            AND oe-rell.i-no     EQ oe-rel.i-no
            AND oe-rell.line     EQ oe-rel.line
            AND oe-rell.po-no    EQ oe-rel.po-no
            AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
            USE-INDEX r-no NO-LOCK NO-ERROR.
        IF NOT AVAILABLE oe-rell THEN 
        DO:
            ll-create-oe-rell = YES.
            MESSAGE "Create missing oe-rell?" 
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE ll-create-oe-rell.
            IF ll-create-oe-rell THEN 
            DO:
                RUN create-oe-rell.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-report.stat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.stat Dialog-Frame
ON ENTRY OF tt-report.stat IN FRAME Dialog-Frame /* Rel Date */
    DO:  
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.stat Dialog-Frame
ON HELP OF tt-report.stat IN FRAME Dialog-Frame /* Rel Date */
    DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.prom-date Dialog-Frame
ON HELP OF tt-report.prom-date IN FRAME Dialog-Frame /* due Date */
    DO:
  {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.stat Dialog-Frame
ON LEAVE OF tt-report.stat IN FRAME Dialog-Frame /* Rel Date */
    DO:
        IF NOT ll-skip THEN 
        DO:

  
            IF INT(SUBSTR(tt-report.stat:SCREEN-VALUE,7,4)) LT 1 THEN
                tt-report.stat:SCREEN-VALUE = SUBSTR(tt-report.stat:SCREEN-VALUE,1,6) +
                    STRING(YEAR(TODAY),"9999").

            ELSE
                IF INT(SUBSTR(tt-report.stat:SCREEN-VALUE,7,4)) LT 90 THEN
                    tt-report.stat:SCREEN-VALUE = SUBSTR(tt-report.stat:SCREEN-VALUE,1,6) +
                        STRING(INT(SUBSTR(tt-report.stat:SCREEN-VALUE,7,4)) + 2000,"9999").

                ELSE
                    IF INT(SUBSTR(tt-report.stat:SCREEN-VALUE,7,4)) LE 99 THEN
                        tt-report.stat:SCREEN-VALUE = SUBSTR(tt-report.stat:SCREEN-VALUE,1,6) +
                            STRING(INT(SUBSTR(tt-report.stat:SCREEN-VALUE,7,4)) + 1900,"9999").

            IF LASTKEY NE -1 THEN 
            DO:
    {custom/pastDatePrompt.i SELF:SCREEN-VALUE}

                RUN valid-key-02 NO-ERROR.
                IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
                IF NOT(oeDateAuto-log AND OeDateAuto-Char = "Colonial") THEN 
                DO:
                    IF INDEX("AB",lv-stat) GT 0 THEN 
                    DO: 
                        IF KEYFUNCTION(LASTKEY) EQ "BACK-TAB" THEN RETURN NO-APPLY.
                        ELSE APPLY "choose" TO Btn_OK.  /*RUN dispatch ("update-record").*/
                    END.
                    ELSE 
                    DO: 
                        APPLY "entry" TO tt-report.price.
                        RETURN NO-APPLY.
                    END.
                END.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rel.tot-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rel.tot-qty Dialog-Frame
ON ENTRY OF oe-rel.tot-qty IN FRAME Dialog-Frame /* Sched Qty */
    DO:
        IF INDEX("AB",get-rel-stat()) GT 0 THEN 
        DO:
            APPLY "tab" TO {&self-name} IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-report.whsed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-report.whsed Dialog-Frame
ON ENTRY OF tt-report.whsed IN FRAME Dialog-Frame /* C$0 */
    DO:
        IF DEC(tt-report.price:SCREEN-VALUE) <> 0 THEN
        DO:
            ASSIGN 
                tt-report.whsed:SCREEN-VALUE = "N".
            IF KEYFUNCTION(LASTKEY) EQ "BACK-TAB" THEN
            DO:
                APPLY "ENTRY" TO tt-report.price IN FRAME {&FRAME-NAME}.
                RETURN NO-APPLY.
            END.
            ELSE
            DO:
                APPLY 'entry' TO tt-report.frt-pay.
                RETURN NO-APPLY.
            END.
        END.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i} 
{sys/inc/oeinq.i}
{sa/sa-sls01.i}
SESSION:DATA-ENTRY-RETURN = YES.       

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


    RUN-PROC = "sbo/oerel-recalc-act.p".
    {methods/smartrun.i}
    lr-rel-lib = phandle.    
    lv-cust-x = "".

    FOR EACH cust NO-LOCK
        WHERE cust.company EQ cocode
        AND cust.active  EQ "X":
        lv-cust-x = cust.cust-no.
        LEAVE.
    END.
    

    FIND FIRST  oe-ordl NO-LOCK 
        WHERE oe-ordl.company EQ cocode
        AND ROWID(oe-ordl)  EQ ip-recid2  NO-ERROR .

    IF ip-recid EQ ? THEN 
    DO:
        RUN pCreateNewRel.
    END.
    ELSE FIND FIRST oe-rel NO-LOCK 
            WHERE oe-rel.company EQ cocode 
            AND ROWID(oe-rel) EQ ip-recid NO-ERROR.

    IF ip-type NE "view" THEN 
    DO: 
        RUN enable_UI.
        RUN display-item.

        ASSIGN /*ll-order-warned                     = NO.*/
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

    DO WITH FRAME {&FRAME-NAME}:
        DISABLE oe-rel.ship-addr[1] oe-rel.ship-city oe-rel.ship-state oe-rel.spare-char-2 oe-rel.spare-char-3 oe-rel.r-no oe-rel.link-no tt-report.job-start-date tt-report.qty tt-report.prom-code tt-report.pr-uom .
        
        IF NOT (oeDateAuto-log AND OeDateAuto-Char EQ "Colonial") THEN 
        DO:
            DISABLE tt-report.prom-date tt-report.prom-code tt-report.pr-uom . 
            tt-report.prom-date:HIDDEN IN FRAME {&FRAME-NAME} = YES.
            tt-report.prom-code:HIDDEN IN FRAME {&FRAME-NAME} = YES.
            tt-report.pr-uom:HIDDEN IN FRAME {&FRAME-NAME} = YES.
            btnCalendar-2:HIDDEN IN FRAME {&FRAME-NAME} = YES.
        END.
        
        APPLY "entry" TO oe-rel.s-code .
    END.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-set-releases Dialog-Frame 
PROCEDURE add-set-releases :
    /*------------------------------------------------------------------------------
    Purpose:
    Parameters:  <none>
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcShipTo   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiRelQty   AS INTEGER  NO-UNDO.
    DEFINE INPUT PARAMETER ipdtRelDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER iplRelease  AS LOG  NO-UNDO.
    DEFINE INPUT PARAMETER ipdtDelDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipcPoNo     AS CHARACTER NO-UNDO.

    /* Create new releases */
    /* ********** code from d-oeitem ********* */

    DEFINE VARIABLE v-qty-sum    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-nxt-r-no   AS INTEGER   INIT 1 NO-UNDO.
    DEFINE VARIABLE v-lst-rel    AS DATE      NO-UNDO.
    DEFINE VARIABLE v-pct-chg    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-ship-id    LIKE oe-rel.ship-id NO-UNDO.
    DEFINE VARIABLE v-num-shipto AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-relType    AS cha       NO-UNDO.
    DEFINE VARIABLE v-ship-from  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-relflg2    AS LOGICAL   INIT YES NO-UNDO.

    DEFINE BUFFER bf-ordl FOR oe-ordl.
    DEFINE BUFFER bf-rel  FOR oe-rel.
    DEFINE BUFFER xoe-ord FOR oe-ord.

    IF NOT AVAILABLE oe-ordl THEN
        RETURN.

    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.
  
    IF AVAILABLE oe-ord THEN
        FIND xoe-ord WHERE ROWID(xoe-ord) = ROWID(oe-ord) NO-LOCK NO-ERROR.

    FOR EACH fg-set WHERE fg-set.set-no = oe-ordl.i-no
        AND fg-set.company = oe-ordl.company NO-LOCK.
        FIND FIRST bf-ordl WHERE bf-ordl.company EQ oe-ordl.company
            AND bf-ordl.ord-no EQ oe-ordl.ord-no
            AND bf-ordl.i-no   EQ fg-set.part-no
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bf-ordl THEN
            NEXT.
     
        ASSIGN 
            v-qty-sum = 0
            v-ship-id = "".

        /* Defaults taken from d-oeitem.w */
        v-ship-id = ipcShipTo.
        v-relflg2 = YES.
  
        /* prompt is in ask-release-questions */
  
        IF v-relflg2 THEN 
        DO:
        {oe/oe-rel.a &fil="bf-ordl"}.
    
        END.
    
        ASSIGN 
            lv-qty = bf-ordl.qty.
        /* If not multi ship-to */
  
        FIND FIRST shipto WHERE shipto.company EQ cocode 
            AND shipto.cust-no EQ xoe-ord.cust-no 
            AND shipto.ship-id EQ v-ship-id
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE shipto THEN
            FIND FIRST shipto WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ xoe-ord.cust-no
                NO-LOCK NO-ERROR.

        IF AVAILABLE shipto THEN 
        DO:
    
   
            IF v-relflg2 THEN
                ASSIGN  
                    oe-rel.ship-no      = shipto.ship-no
                    oe-rel.ship-id      = shipto.ship-id
                    oe-rel.ship-addr[1] = shipto.ship-addr[1]
                    oe-rel.ship-addr[2] = shipto.ship-addr[2]
                    oe-rel.ship-city    = shipto.ship-city
                    oe-rel.ship-state   = shipto.ship-state
                    oe-rel.ship-zip     = shipto.ship-zip
                    oe-rel.ship-i[1]    = shipto.notes[1]
                    oe-rel.ship-i[2]    = shipto.notes[2]
                    oe-rel.ship-i[3]    = shipto.notes[3]
                    oe-rel.ship-i[4]    = shipto.notes[4]
                    oe-rel.spare-char-1 = shipto.loc.
      
            /* check that itemfg-loc exists */
            IF oe-rel.spare-char-1 GT "" THEN
                RUN fg/chkfgloc.p (INPUT oe-rel.i-no, INPUT oe-rel.spare-char-1).
    
    
            /* if add mode then use default carrier */
            IF TRUE THEN 
            DO:
                FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                    AND sys-ctrl.NAME    EQ "OECARIER"
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE sys-ctrl THEN 
                DO:
                    CREATE sys-ctrl.
                    ASSIGN 
                        sys-ctrl.company  = cocode
                        sys-ctrl.NAME     = "OECARIER"
                        sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
                        sys-ctrl.char-fld = "ShipTo".
        
                    DO WHILE TRUE:
                        MESSAGE "Default Shipping Carrier from Header or Shipto?"
                            UPDATE sys-ctrl.char-fld.
                        IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "ShipTo" THEN LEAVE.
                    END.
                END.
     
                IF v-relflg2 THEN
                    oe-rel.carrier   = IF sys-ctrl.char-fld = "Shipto" THEN shipto.carrier
                    ELSE xoe-ord.carrier.
            END. /* If true */
    
        END. /* avail shipto */


        oe-rel.s-code          = "S".     

        /* Set oe-rel quantity based on set part quantity */
        oe-rel.tot-qty = ipiRelQty * fg-set.qtyPerSet.
        oe-rel.rel-date = ipdtRelDate.
        IF ipcPoNo GT "" THEN
            oe-rel.po-no = ipcPoNo.

        IF iplRelease THEN 
        DO:
            FOR EACH bf-rel WHERE bf-rel.company EQ oe-rel.company
                AND bf-rel.ord-no EQ oe-rel.ord-no
                AND bf-rel.LINE   EQ oe-rel.LINE
                AND bf-rel.i-no   EQ oe-rel.i-no
                AND bf-rel.rel-date EQ ipdtDelDate
                AND LOOKUP(bf-rel.stat, "S,I,L") GT 0
                EXCLUSIVE-LOCK:

                DELETE bf-rel.

            END.
        END.

    /* Run Freight calculation  */
    /*       RUN oe/oe-frtcl.p. */
    END. /* each set part-no to create releases for */
/* ********* end code from d-oeitem *******/
/* Remove old releases for this date */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report-record-1 Dialog-Frame 
PROCEDURE create-report-record-1 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-phantom AS LOG NO-UNDO.
    DEFINE INPUT PARAMETER ip-date AS DATE NO-UNDO.
  
    DEFINE VARIABLE v-reltype AS cha NO-UNDO.
    DEFINE BUFFER bf-oe-rel FOR oe-rel.
    /* To make record unique */
    iTTReportLine = iTTReportLine + 1.
    ASSIGN
        /* tt-report.term-id = v-term */
        tt-report.company  = oe-rel.company
        tt-report.ord-no   = oe-rel.ord-no
        tt-report.line     = iTTReportLine
        tt-report.rec-id   = RECID(oe-rel)
        ld-date            = ip-date
        tt-report.del-zone = STRING(YEAR(ld-date),"9999") +
                         STRING(MONTH(ld-date),"99")  +
                         STRING(DAY(ld-date),"99")
        tt-report.stat     = STRING(ld-date,"99999999")
        tt-report.tax      = ip-phantom
        tt-report.po-no    = /*IF AVAIL inv-line THEN inv-line.po-no
                         ELSE
                         IF AVAIL oe-boll THEN oe-boll.po-no
                         ELSE
                         IF AVAIL oe-rell THEN oe-rell.po-no
                         ELSE*/ oe-rel.po-no
        tt-report.qty      = oe-rel.qty
        tt-report.opened   = (AVAILABLE oe-relh AND oe-relh.printed) OR
                         INDEX("PCZ",lv-stat) GT 0
        tt-report.q-rel    = (IF AVAILABLE oe-relh THEN oe-relh.release# ELSE 0).
    tt-report.job-start-date = IF AVAILABLE oe-bolh THEN /* string(*/ oe-bolh.bol-date /* ,"99999999") */ ELSE ?
        . 
    IF oe-rel.spare-char-4 GT "" THEN 
        ASSIGN
            tt-report.prom-date = DATE(ENTRY(1, oe-rel.spare-char-4))
            tt-report.prom-code = ENTRY(2, oe-rel.spare-char-4)
            tt-report.pr-uom    = ENTRY(3, oe-rel.spare-char-4)
            .
    /* task 04011103*/
    FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name EQ "RelType" NO-LOCK NO-ERROR.
    IF AVAILABLE sys-ctrl THEN
        FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
            AND sys-ctrl-ship.ship-id = oe-rel.ship-id NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl-shipto THEN
        FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
            AND sys-ctrl-ship.ship-id = "" NO-LOCK NO-ERROR.
    IF AVAILABLE sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN v-reltype = sys-ctrl-shipto.char-fld.
    ELSE IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN v-reltype = sys-ctrl.char-fld.
        ELSE v-reltype = "B" .
    IF v-relType <> "" THEN 
    DO:
        IF oe-rel.s-code EQ '' THEN 
        DO:
            FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ROWID(oe-rel)
                EXCLUSIVE-LOCK.
            bf-oe-rel.s-code = IF ll-transfer THEN "T"
            ELSE IF oe-ordl.is-a-component THEN "S"
            ELSE SUBSTRING(v-relType,1,1).
            FIND CURRENT bf-oe-rel NO-LOCK.
            RELEASE bf-oe-rel.
        END.
    END.


    tt-report.s-basis[1] = IF v-reltype <> "" THEN oe-rel.s-code
    ELSE IF ll-transfer            THEN "T"
    ELSE
        IF oe-ordl.is-a-component AND
        (oe-rel.s-code = "" OR
        oe-rel.s-code NE "T")   THEN "S"
        ELSE
        IF oe-rel.s-code <> ""      THEN oe-rel.s-code
        ELSE
        IF AVAILABLE oe-rell          THEN oe-rell.s-code
        ELSE "B".
    IF ll-new-record AND ll-transfer /* (oe-ord.TYPE = "T") */ THEN
        ASSIGN tt-report.s-basis[1] = "T".
    IF ll-new-record AND RelType-int = 1 AND v-inv-ship = YES THEN 
    DO: /* task 07241401 */
        FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ROWID(oe-rel)
            EXCLUSIVE-LOCK.
        ASSIGN 
            tt-report.s-basis[1] = "S"
            bf-oe-rel.qty        = v-qty-inv-only 
            bf-oe-rel.tot-qty    = v-totqty-inv-only 
            v-inv-ship           = NO .
        RELEASE bf-oe-rel.
    END.
    ASSIGN
        tt-report.lot-no  = oe-rel.lot-no
        tt-report.frt-pay = oe-rel.frt-pay
        tt-report.flute   = oe-rel.fob-code.

    ASSIGN 
        tt-report.price = oe-rel.sell-price
        tt-report.whsed = oe-rel.zeroPrice > 0 .
      

    IF oeinq THEN 
        tt-report.del-zone = STRING(9999999999 - INT(tt-report.del-zone),"9999999999").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
    IF AVAILABLE oe-rel  THEN 
    DO: 
        RUN pBuildReport (ROWID(oe-rel), NO).

        FIND FIRST tt-report
            WHERE tt-report.rec-id EQ RECID(oe-rel)
            NO-ERROR.

        fi_discount = oe-ordl.disc .
        fi_totprice = oe-ordl.t-price .
        DISPLAY  tt-report.opened oe-rel.s-code 
            oe-rel.ship-id oe-rel.stat oe-rel.carrier oe-rel.tot-qty oe-rel.qty 
            tt-report.po-no tt-report.lot-no tt-report.prom-date tt-report.stat 
            oe-rel.ship-addr[1] oe-rel.ship-city oe-rel.ship-state tt-report.price 
            tt-report.whsed fi_discount fi_totprice tt-report.frt-pay 
            tt-report.flute oe-rel.spare-char-1 oe-rel.spare-char-2 oe-rel.spare-char-3 
            tt-report.q-rel oe-rel.r-no oe-rel.link-no tt-report.job-start-date 
            tt-report.qty tt-report.prom-code tt-report.pr-uom 
            WITH FRAME Dialog-Frame.
    END.

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
    IF AVAILABLE oe-ordl THEN 
        DISPLAY fi_discount fi_totprice 
            WITH FRAME Dialog-Frame.
    IF AVAILABLE oe-rel THEN 
        DISPLAY oe-rel.s-code oe-rel.ship-id oe-rel.stat oe-rel.carrier oe-rel.tot-qty 
            oe-rel.qty oe-rel.ship-addr[1] oe-rel.ship-city oe-rel.ship-state 
            oe-rel.spare-char-1 oe-rel.spare-char-2 oe-rel.spare-char-3 
            oe-rel.r-no oe-rel.link-no 
            WITH FRAME Dialog-Frame.
    IF AVAILABLE tt-report THEN 
        DISPLAY tt-report.opened tt-report.po-no tt-report.lot-no tt-report.prom-date 
            tt-report.stat tt-report.price tt-report.whsed tt-report.frt-pay 
            tt-report.flute tt-report.q-rel tt-report.job-start-date tt-report.qty 
            tt-report.prom-code tt-report.pr-uom 
            WITH FRAME Dialog-Frame.
    ENABLE oe-rel.s-code oe-rel.ship-id oe-rel.stat 
        oe-rel.carrier oe-rel.tot-qty oe-rel.qty tt-report.po-no 
        tt-report.lot-no tt-report.prom-date tt-report.stat  
        oe-rel.ship-addr[1] oe-rel.ship-city oe-rel.ship-state tt-report.price 
        tt-report.whsed tt-report.frt-pay 
        tt-report.flute oe-rel.spare-char-1 oe-rel.spare-char-2 
        oe-rel.spare-char-3 tt-report.q-rel oe-rel.r-no oe-rel.link-no 
        tt-report.job-start-date tt-report.qty tt-report.prom-code 
        tt-report.pr-uom btnCalendar-1 btnCalendar-2 Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildReport Dialog-Frame 
PROCEDURE pBuildReport :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ip-phantom AS LOG NO-UNDO.
      

    FIND oe-rel WHERE ROWID(oe-rel) EQ ip-rowid NO-LOCK NO-ERROR.

    IF AVAILABLE oe-rel THEN
        FIND FIRST oe-ord
            WHERE oe-ord.company EQ oe-rel.company 
            AND oe-ord.ord-no  EQ oe-rel.ord-no
            NO-LOCK NO-ERROR.

    IF AVAILABLE oe-ord THEN 
    DO:
        FIND FIRST tt-report
            WHERE tt-report.rec-id EQ RECID(oe-rel)
            NO-ERROR.

        IF NOT AVAILABLE tt-report THEN CREATE tt-report.

    {oe/rel-stat.i lv-stat}

        RELEASE inv-line.
        IF lv-stat EQ "Z" AND AVAILABLE oe-boll THEN
            FIND FIRST inv-line
                WHERE inv-line.company EQ oe-boll.company
                AND inv-line.b-no    EQ oe-boll.b-no
                AND inv-line.ord-no  EQ oe-boll.ord-no
                AND inv-line.i-no    EQ oe-boll.i-no
                AND inv-line.po-no   NE ""
                NO-LOCK NO-ERROR.

        RUN create-report-record-1 (ip-phantom,
            IF AVAILABLE oe-relh THEN oe-relh.rel-date
            ELSE oe-rel.rel-date).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateNewRel Dialog-Frame 
PROCEDURE pCreateNewRel :
    /*------------------------------------------------------------------------------
          Purpose:     
          PARAMs:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE v-qty-sum    AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-nxt-r-no   AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-lst-rel    AS DATE    INIT TODAY NO-UNDO.
    DEFINE VARIABLE v-pct-chg    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-ship-id    LIKE oe-rel.ship-id NO-UNDO.
    DEFINE VARIABLE v-carrier    LIKE oe-rel.carrier NO-UNDO.
    DEFINE VARIABLE v-shipfrom   LIKE loc.loc NO-UNDO.
    DEFINE VARIABLE v-num-shipto AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-qty-mod    AS LOG     NO-UNDO.
    DEFINE BUFFER bf-rel  FOR oe-rel.
    DEFINE BUFFER bf-cust FOR cust.
    DEFINE VARIABLE v-first-ship-id     AS cha     NO-UNDO.
    DEFINE VARIABLE v-qty-released      AS INTEGER NO-UNDO.
    DEFINE VARIABLE rShipTo             AS ROWID   NO-UNDO.
    DEFINE VARIABLE lFirstReleaseOfItem AS LOGICAL NO-UNDO.
  
    /* Code placed here will execute PRIOR to standard behavior. */

    RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).
    FIND FIRST bf-cust WHERE bf-cust.cust-no EQ oe-ord.cust-no NO-LOCK NO-ERROR.
    ASSIGN
        v-ship-id = IF AVAILABLE oe-rel THEN oe-rel.ship-id ELSE ""
        v-carrier = IF AVAILABLE oe-rel THEN oe-rel.carrier ELSE ""
        .
    IF AVAIL(bf-cust) AND bf-cust.ACTIVE = "X" AND v-last-shipto GT "" THEN
        v-ship-id = v-last-shipto.
    FIND FIRST bf-rel WHERE bf-rel.company = oe-ord.company
        AND bf-rel.ord-no = oe-ord.ord-no
        AND bf-rel.i-no = oe-ordl.i-no 
        AND bf-rel.LINE = oe-ordl.LINE
        NO-LOCK NO-ERROR.
    v-first-ship-id = IF AVAILABLE bf-rel THEN bf-rel.ship-id ELSE "".
    adm-cur-state = "ADD".
    
    CREATE oe-rel .
    ASSIGN 
        lv-item-recid = RECID(oe-rel).
    ll-new-record = YES.
   
    lv-rel-recid = RECID(oe-rel).
    ASSIGN 
        v-qty-sum      = 0
        v-qty-released = 0.

    IF AVAILABLE oe-ordl THEN 
    DO:
        FIND FIRST oe-ord OF oe-ordl NO-LOCK.

        FOR EACH bf-rel WHERE bf-rel.company = oe-ord.company
            AND bf-rel.ord-no = oe-ord.ord-no
            AND bf-rel.i-no = oe-ordl.i-no 
            AND bf-rel.LINE = oe-ordl.LINE
            NO-LOCK:

            IF bf-rel.s-code = "" OR CAN-DO("B,S",bf-rel.s-code) THEN 
            DO:
                v-qty-sum = v-qty-sum + bf-rel.qty.
                IF LOOKUP(bf-rel.stat, "C,Z,P,A,B") GT 0 THEN
                    v-qty-released = v-qty-released + bf-rel.qty .  /*Task 11011304  */ /* task  09021403*/
                ELSE
                    v-qty-released = v-qty-released + bf-rel.tot-qty .  /*Task 11011304  */ /* task  09021403*/
            END.
        END.

        IF v-qty-sum GE oe-ordl.qty + (oe-ordl.qty * (oe-ordl.over-pct / 100)) THEN
            MESSAGE "Total Planned release quantity will exceed the Or" +
                "der quantity + the Underrun %."
                VIEW-AS ALERT-BOX WARNING.

        FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name    EQ "OECARIER"
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE sys-ctrl THEN 
        DO:
            CREATE sys-ctrl.
            ASSIGN 
                sys-ctrl.company  = cocode
                sys-ctrl.name     = "OECARIER"
                sys-ctrl.descrip  = "Default carrier from Header or ShipTo:"
                sys-ctrl.char-fld = "ShipTo".       
            DO WHILE TRUE:
                MESSAGE "Default Shipping Carrier from Header or Shipto?" UPDATE sys-ctrl.char-fld.
                IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "ShipTo" THEN LEAVE. 
            END.
        END.

        RELEASE shipto.

        IF v-carrier = "" THEN 
        DO:  /* NK1 OECARIER */
            IF sys-ctrl.char-fld EQ "ShipTo" THEN 
            DO:
                FIND FIRST shipto NO-LOCK
                    WHERE shipto.company EQ oe-ord.company
                    AND shipto.cust-no EQ oe-ord.cust-no
                    AND shipto.ship-id EQ v-ship-id NO-ERROR.
                v-carrier = IF AVAILABLE shipto THEN shipto.carrier ELSE "".
            END.
            ELSE IF sys-ctrl.char-fld EQ "Header" THEN v-carrier = oe-ord.carrier.
        END.
    
        IF NOT AVAILABLE shipto THEN
            FOR EACH shipto
                WHERE shipto.company  EQ cocode
                AND shipto.cust-no EQ (IF lv-cust-x NE ""         AND
                oe-rel.s-code EQ "T" THEN lv-cust-x
                ELSE oe-ord.cust-no)
                NO-LOCK
                BREAK BY shipto.ship-no DESCENDING:
                IF shipto.ship-id EQ oe-ord.cust-no OR LAST(shipto.ship-no) THEN LEAVE.
            END.
     
        IF v-carrier EQ "" AND AVAILABLE shipto THEN v-carrier = shipto.carrier.
        IF v-shipfrom EQ "" AND AVAILABLE shipto THEN v-shipfrom = shipto.loc. 
        FIND FIRST bf-rel WHERE bf-rel.company EQ cocode
            AND bf-rel.ord-no EQ oe-ord.ord-no
            AND bf-rel.i-no EQ oe-ordl.i-no
            AND ROWID(bf-rel) NE ROWID(oe-rel)
            NO-LOCK NO-ERROR.
    
        lFirstReleaseOfItem = IF AVAILABLE bf-rel THEN NO ELSE YES.  

        ASSIGN 
            oe-rel.company      = cocode
            oe-rel.loc          = locode
            oe-rel.ord-no       = oe-ordl.ord-no
            oe-rel.i-no         = oe-ordl.i-no
            oe-rel.cust-no      = oe-ord.cust-no
            oe-rel.po-no        = IF oe-ordl.po-no NE "" THEN oe-ordl.po-no 
                                                     ELSE oe-ord.po-no
            oe-rel.qty          = 0 /*oe-ordl.qty - v-qty-sum*/
            oe-rel.line         = oe-ordl.line
            oe-rel.s-comm[1]    = oe-ord.s-comm[1]
            oe-rel.s-comm[2]    = oe-ord.s-comm[2]
            oe-rel.s-comm[3]    = oe-ord.s-comm[3]
            oe-rel.s-name[1]    = oe-ord.sname[1]
            oe-rel.s-name[2]    = oe-ord.sname[2]
            oe-rel.s-name[3]    = oe-ord.sname[3]
            oe-rel.s-pct[1]     = oe-ord.s-pct[1]
            oe-rel.s-pct[2]     = oe-ord.s-pct[2]
            oe-rel.s-pct[3]     = oe-ord.s-pct[3]
            oe-rel.sman[1]      = oe-ord.sman[1]
            oe-rel.sman[2]      = oe-ord.sman[2]
            oe-rel.sman[3]      = oe-ord.sman[3]
            oe-rel.sold-no      = oe-ord.sold-no
            oe-rel.carrier      = /*if sys-ctrl.char-fld = "Shipto" and avail shipto then shipto.carrier
                              else*/ v-carrier
            oe-rel.r-no         = v-nxt-r-no
            oe-rel.spare-char-1 = v-shipfrom.

        IF oereleas-cha EQ "LastShip" THEN
            oe-rel.rel-date = oe-ord.last-date.
        ELSE IF oereleas-cha EQ "Due Date" THEN
                oe-rel.rel-date = oe-ordl.req-date.
            ELSE /*DueDate+1Day*/
            DO:
                oe-rel.rel-date = oe-ordl.req-date + 1.
                IF WEEKDAY(oe-rel.rel-date) EQ 7 THEN
                    oe-rel.rel-date = oe-rel.rel-date + 2.
                ELSE
                    IF WEEKDAY(oe-rel.rel-date) EQ 1 THEN
                        oe-rel.rel-date = oe-rel.rel-date + 1.
            END.
            
        IF NOT (oeDateAuto-log AND OeDateAuto-Char EQ "Colonial") THEN 
        DO:
            RUN sys/ref/shipToOfRel.p (INPUT ROWID(oe-rel), OUTPUT rShipTo).
            FIND shipto WHERE ROWID(shipto) EQ rShipTo NO-LOCK NO-ERROR.
            
            IF AVAILABLE shipto THEN 
                oe-rel.rel-date = get-date(oe-ordl.req-date, INT(shipto.del-time), "-").
            
        END.
        
        /* stores oe-rel due date */
        IF lfirstReleaseofItem THEN 
            oe-rel.spare-char-4 = STRING(oe-ord.due-date) + ",,". 
      
                                  
        IF oe-rel.qty LT 0 THEN oe-rel.qty = 0.

        oe-rel.tot-qty = oe-ordl.qty - v-qty-released /*oe-rel.qty*/ .

        IF oe-rel.rel-date LE v-lst-rel THEN oe-rel.rel-date = v-lst-rel + 1.

        IF AVAILABLE shipto THEN
            ASSIGN oe-rel.ship-addr[1] = shipto.ship-addr[1]
                oe-rel.ship-city    = shipto.ship-city
                oe-rel.ship-state   = shipto.ship-state
                oe-rel.ship-zip     = shipto.ship-zip
                oe-rel.ship-no      = shipto.ship-no
                oe-rel.ship-id      = IF v-first-ship-id <> "" THEN v-first-ship-id ELSE oe-ord.ship-id
                oe-rel.ship-i[1]    = shipto.notes[1]
                oe-rel.ship-i[2]    = shipto.notes[2]
                oe-rel.ship-i[3]    = shipto.notes[3]
                oe-rel.ship-i[4]    = shipto.notes[4].
        ELSE ASSIGN oe-rel.ship-no   = oe-ord.sold-no
                oe-rel.ship-id   = IF v-first-ship-id <> "" THEN v-first-ship-id ELSE oe-ord.ship-id
                oe-rel.ship-i[1] = oe-ord.ship-i[1]
                oe-rel.ship-i[2] = oe-ord.ship-i[2]
                oe-rel.ship-i[3] = oe-ord.ship-i[3]
                oe-rel.ship-i[4] = oe-ord.ship-i[4].

        IF NOT CAN-FIND(FIRST shipto 
            WHERE shipto.company EQ cocode
            AND shipto.ship-id EQ oe-rel.ship-id) THEN 
        DO:
            
            FOR EACH shipto
                WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ oe-rel.cust-no NO-LOCK BY shipto.ship-id:

                IF AVAILABLE shipto THEN
                    ASSIGN 
                        oe-rel.ship-id      = shipto.ship-id
                        oe-rel.ship-addr[1] = shipto.ship-addr[1]
                        oe-rel.ship-city    = shipto.ship-city
                        oe-rel.ship-state   = shipto.ship-state
                        oe-rel.ship-zip     = shipto.ship-zip
                        oe-rel.ship-no      = shipto.ship-no
                        oe-rel.ship-i[1]    = shipto.notes[1]
                        oe-rel.ship-i[2]    = shipto.notes[2]
                        oe-rel.ship-i[3]    = shipto.notes[3]
                        oe-rel.ship-i[4]    = shipto.notes[4].
                LEAVE .
            END.
        END.
        
        FIND FIRST tt-report
            WHERE tt-report.rec-id EQ RECID(oe-rel)
            NO-ERROR.
        
        IF NOT AVAILABLE tt-report THEN CREATE tt-report.

        RUN create-report-record-1 (NO, oe-rel.rel-date).
    END.

    ELSE 
    DO:
        MESSAGE " Order Line item record is not avail..." VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prompt-set-release Dialog-Frame 
PROCEDURE prompt-set-release :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplCancel   AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcShipTo   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiRelQty   AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdtRelDate AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPO       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRelease  AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opdtDelDate AS DATE NO-UNDO.

    DEFINE VARIABLE lcUserPrompt     AS CHARACTER INIT "".

    DEFINE VARIABLE lcShipTo         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ip-parms         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE op-values        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE choice           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValid           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE liRelQty         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ldRelDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE ldDelDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE llReplace        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lcPO             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE opiNewOrder      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lvErrMsg         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lvcDefaultShipto AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lvcDefaultPo     AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-rel FOR oe-rel.

    /* Only looking at order # since release would not exist for the set header item */
    FIND FIRST bf-oe-rel WHERE bf-oe-rel.company EQ oe-ordl.company
        AND bf-oe-rel.ord-no EQ oe-ordl.ord-no
        NO-LOCK NO-ERROR.
    IF AVAILABLE bf-oe-rel THEN
        ASSIGN lvcDefaultShipto = bf-oe-rel.ship-id
            lvcDefaultPO     = bf-oe-rel.po-no.
    ELSE
        IF AVAILABLE oe-ordl THEN
            ASSIGN lvcDefaultShipto = oe-ordl.ship-id
                lvcDefaultPO     = oe-ordl.po-no.

    ip-parms = 
   /* Box Title */
        "type=literal,name=label11,row=2,col=18,enable=false,width=58,scrval=" + lcUserPrompt + ",FORMAT=X(58)" 
      
        /* Set Attributes */
        + "|type=attrib,name=cust-no,row=1,col=1,enable=false,width=2,inpval=" + oe-ordl.cust-no
        + "|type=attrib,name=company,row=1,col=1,enable=false,width=2,inpval=" + cocode
        + "|type=attrib,name=loc,row=1,col=1,enable=false,width=2,inpval=" + locode
    

        /* Ship To Code */
        + "|type=literal,name=label6,row=2.2,col=31,enable=false,width=58,scrval=" + "ShipTo Code:" + ",FORMAT=X(58)"
        + "|type=fill-in,name=custShipTo,row=2,col=45,enable=true,width=15,initial=" + lvcDefaultShipto

        /* Release Qty */
        + "|type=literal,name=label7,row=3.2,col=28,enable=false,width=58,scrval=" + "Set Release Qty:" + ",FORMAT=X(58)"
        + "|type=fill-in,name=fi_relqty,row=3,col=45,enable=true,width=15,data-type=integer"    

        /* Release Date */
        + "|type=literal,name=label8,row=4.2,col=27,enable=false,width=58,scrval=" + "Set Release Date:" + ",FORMAT=X(58)"
        + "|type=fill-in,name=fi_reldate,row=4,col=45,enable=true,width=15,data-type=date,initial=" + STRING(TODAY)

        /* PO # */
        + "|type=literal,name=label12,row=5.2,col=31,enable=false,width=30,scrval=" + "Customer PO:" + ",FORMAT=X(30)"
        + "|type=fill-in,name=po-no,row=5,col=45,enable=true,width=22,format=x(15),initial=" + lvcDefaultPO

        /* Replace existing releases toggle box */
        + "|type=literal,name=label9,row=6.5,col=23,enable=false,width=38,scrval=" + "Replace Existing Releases for date: " + ",FORMAT=X(58)"
        + "|type=toggle,name=tb_replace,row=6.5,col=20,enable=true,width=2,data-type=logical"

        /* Delete Release Date */
        /* + "|type=literal,name=label10,row=6,col=52,enable=false,width=9,scrval=" + "Del Date:" + ",FORMAT=X(9)" */
        + "|type=fill-in,name=fi_deldate,row=6.3,col=62,enable=true,width=15,data-type=date,deffield=fi_reldate,depfield=tb_replace,initial=" + STRING(TODAY)
    

        + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true,width=12,height=3 " 
        /* Box Title */
        + "|type=win,name=fi3,enable=true,label=         Set Quantity to Create Component Releases?,FORMAT=X(30),height=11".

    prompt-loop:
    DO WHILE TRUE:

        RUN custom/d-prompt.w (INPUT "", ip-parms, "", OUTPUT op-values).
    
        /* Process values using names given above */
        DO i = 1 TO NUM-ENTRIES(op-values) BY 2.
            IF ENTRY(i, op-values) EQ "default" THEN
                choice = ENTRY(i + 1, op-values) NO-ERROR.

            /* Ship To */
            IF ENTRY(i, op-values) EQ "custShipTo" THEN
                lcShipTo = ENTRY(i + 1, op-values) NO-ERROR.            

            /* Release Qty */
            IF ENTRY(i, op-values) EQ "fi_relqty" THEN
                liRelQty = INTEGER(ENTRY(i + 1, op-values)) NO-ERROR.            

            /* Release Date */
            IF ENTRY(i, op-values) EQ "fi_reldate" THEN
                ldRelDate = DATE(ENTRY(i + 1, op-values)) NO-ERROR. 

            /* Replace Existing */
            IF ENTRY(i, op-values) EQ "tb_replace" THEN
                llReplace = LOGICAL(ENTRY(i + 1, op-values)) NO-ERROR. 

            /* Delete Date */
            IF ENTRY(i, op-values) EQ "fi_deldate" THEN
                ldDElDate = DATE(ENTRY(i + 1, op-values)) NO-ERROR. 

            /* PO # */
            IF ENTRY(i, op-values) EQ "po-no" THEN
                lcPO = ENTRY(i + 1, op-values) NO-ERROR. 
        END.

        lvErrMsg = "".
        IF choice NE "CANCEL" THEN 
        DO:
    
            FIND FIRST shipto WHERE shipto.company = cocode
                AND shipto.cust-no EQ oe-ordl.cust-no
                AND shipto.ship-id EQ lcShipTo
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE shipto THEN
                lvErrMsg = "Please enter a valid ship to id".
            IF liRelQty LE 0 THEN
                lvErrMsg = "Please enter a release quantity that is more than zero.".
            IF ldRelDate EQ ? THEN
                lvErrMsg = "Please enter a release date.".

        END.

        IF lvErrMsg GT "" THEN 
            MESSAGE lvErrMsg VIEW-AS ALERT-BOX.
        ELSE
            LEAVE.
    END.

    ASSIGN
        oplCancel   = IF choice EQ "CANCEL" THEN TRUE ELSE FALSE
        opcShipTo   = lcShipTo
        opcPO       = lcPo
        opiRelQty   = liRelQty
        opdtRelDate = ldRelDate
        opdtDelDate = ldDelDate
        oplRelease  = llReplace.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdate-record Dialog-Frame 
PROCEDURE pUpdate-record :
    /*------------------------------------------------------------------------------
    Purpose:
    Parameters:  <none>
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-req-date AS CHARACTER NO-UNDO .

    DEFINE VARIABLE ll-ans   AS LOG  NO-UNDO.
    DEFINE VARIABLE ldt-ship AS DATE FORM "99/99/9999" NO-UNDO.
    DEFINE BUFFER bf-rel FOR oe-rel .
    DEFINE VARIABLE ld-prev-rel-qty      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-qty-sum            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ls-key-02            LIKE tt-report.stat NO-UNDO.
    DEFINE VARIABLE v-date-change-reason AS CHARACTER NO-UNDO.
    DEFINE VARIABLE h_reasonwin          AS HANDLE    NO-UNDO.
    DEFINE VARIABLE v-added-rowid        AS ROWID     NO-UNDO.
    DEFINE BUFFER b-ordl FOR oe-ordl.
    DEFINE VARIABLE val1        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE val2        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrigLoc    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lLocChanged AS LOG       NO-UNDO.
    DEFINE VARIABLE v-q-back    AS INTEGER   NO-UNDO.
    /* Code placed here will execute PRIOR to standard behavior. */
    IF NOT AVAILABLE oe-rel AND lv-rel-recid <> ? THEN
        FIND oe-rel WHERE RECID(oe-rel) = lv-rel-recid.
    ld-prev-rel-qty = IF ll-new-record THEN 0 ELSE oe-rel.qty.
  
    FIND oe-ord OF oe-ordl NO-LOCK.

    lLocChanged = NO.
    ldt-ship = oe-rel.rel-date.
    ls-po = tt-report.po-no.
    IF oe-rel.spare-char-1 NE oe-rel.spare-char-1:SCREEN-VALUE IN FRAME Dialog-Frame THEN
    DO:
        ASSIGN 
            cOrigLoc = oe-rel.spare-char-1.
        lLocChanged = YES.
        /* Make sure to create this location if it's not there */
        RUN fg/chkfgloc.p (INPUT oe-rel.i-no, INPUT oe-rel.spare-char-1:SCREEN-VALUE IN FRAME Dialog-Frame).
        /* Deallocate from inventory before changing the location */
        FIND itemfg-loc 
            WHERE itemfg-loc.company EQ oe-rel.company
            AND itemfg-loc.i-no    EQ oe-rel.i-no
            AND itemfg-loc.loc     EQ oe-rel.spare-char-1
            EXCLUSIVE-LOCK NO-ERROR.
        FIND CURRENT oe-rel EXCLUSIVE-LOCK.
        IF AVAIL(itemfg-loc) THEN
            ASSIGN itemfg-loc.q-alloc = itemfg-loc.q-alloc -  oe-rel.spare-dec-1
                oe-rel.spare-dec-1 = 0.
        FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
    END.


    val1 = ip-req-date .
    val2 = tt-report.stat:SCREEN-VALUE IN FRAME Dialog-Frame .
    val2 = SUBSTR(val2,1,2) + SUBSTR(val2,4,2)
        + SUBSTR(val2,7,4).
    val1 = TRIM(val1).
    val2 = TRIM(val2).

    IF val1 NE val2 
        AND oeDateChange-log 
        AND (oeDateChange-char EQ "" OR LOOKUP("release date", oeDateChange-char) GT 0)
        AND NOT ll-new-record THEN 
    DO:
    /* prompt user for reason for date change */.

        RUN oe/d-rsnnot.w
            (INPUT oe-rel.rec_key, INPUT "R", INPUT "", INPUT "", INPUT 0, INPUT "RDC", INPUT "",
            OUTPUT v-date-change-reason, OUTPUT v-added-rowid)  .
        /*
        RUN set-position IN h_reasonWin ( 5.00 , 154.20 ) NO-ERROR.
        IF VALID-HANDLE(h_reasonWin) THEN
            DELETE OBJECT h_reasonWin. */
        FIND CURRENT oe-rel EXCLUSIVE-LOCK.
        IF v-date-change-reason GT "" THEN
            ASSIGN oe-rel.spare-char-2:SCREEN-VALUE = v-date-change-reason
                oe-rel.spare-char-3:SCREEN-VALUE = USERID("NOSWEAT")
                oe-rel.spare-char-3              = USERID("NOSWEAT").
        FIND CURRENT oe-rel NO-LOCK.
    END.

    /* Allocate the quantity to inventory record per location */
    RUN fg/fgitmloc.p (INPUT oe-rel.i-no, INPUT ROWID(oe-rel)).

    IF lLocChanged THEN 
    DO:
        /* Recalculate the q-alloc on original itemfg-loc to make sure it's reduced */
        FIND itemfg 
            WHERE itemfg.company EQ oe-rel.company
            AND itemfg.i-no EQ oe-rel.i-no
            NO-LOCK NO-ERROR.
        FIND itemfg-loc 
            WHERE itemfg-loc.company EQ oe-rel.company
            AND itemfg-loc.i-no    EQ oe-rel.i-no
            AND itemfg-loc.loc     EQ cOrigLoc
            EXCLUSIVE-LOCK NO-ERROR.

        IF AVAILABLE itemfg AND avail(itemfg-loc) THEN
            RUN fg/calcqabl.p (ROWID(itemfg), cOrigLoc, OUTPUT itemfg-loc.q-alloc, OUTPUT v-q-back).
        FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
    END.


    /* Code placed here will execute AFTER standard behavior.    */
    oe-rel.po-no = tt-report.po-no.

  
    IF oe-ordl.is-a-component AND CAN-DO("B,I",oe-rel.s-code) THEN oe-rel.s-code = "S".
  
    /* Storing due date, due date change reason, due date change user */
    oe-rel.spare-char-4 = (IF tt-report.prom-date EQ ? THEN "" ELSE STRING(tt-report.prom-date)) + ","
        + USERID("nosweat") + ","
        + tt-report.pr-uom.

  
    ASSIGN
        oe-rel.lot-no   = tt-report.lot-no:SCREEN-VALUE
        oe-rel.frt-pay  = tt-report.frt-pay:SCREEN-VALUE
        oe-rel.fob-code = tt-report.flute:SCREEN-VALUE.

 

    ASSIGN 
        oe-rel.sell-price = DEC(tt-report.price:SCREEN-VALUE)
        oe-rel.zeroPrice  = (IF tt-report.whsed:SCREEN-VALUE BEGINS "Y" THEN 1 ELSE 0) .

    FIND CURRENT ref-lot-no NO-LOCK NO-ERROR.
    FIND CURRENT ref-sell-price NO-LOCK NO-ERROR.

    RELEASE ref-lot-no.
    RELEASE ref-sell-price.
   
    IF INDEX("AB",lv-stat) LE 0 THEN
        oe-rel.rel-date = DATE(INT(SUBSTR(tt-report.stat,1,2)),
            INT(SUBSTR(tt-report.stat,3,2)),
            INT(SUBSTR(tt-report.stat,5,4))).

    v-qty-sum = 0.
    FOR EACH bf-rel WHERE bf-rel.company = oe-ord.company
        AND bf-rel.ord-no = oe-ord.ord-no 
        AND bf-rel.LINE = oe-ordl.LINE    /* 01/20/03 YSK TASK 01170303*/
        AND bf-rel.i-no = oe-ordl.i-no NO-LOCK :
        RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT lv-stat).

        IF (bf-rel.s-code = "" OR INDEX("BS",bf-rel.s-code) GT 0) AND
            NOT CAN-DO("C,Z",lv-stat)                          THEN
            v-qty-sum = v-qty-sum + bf-rel.qty. 
    END.

    IF v-qty-sum + oe-ordl.ship-qty GT oe-ordl.qty + 
        (oe-ordl.qty * (oe-ordl.over-pct / 100)) 
        THEN MESSAGE "Total Planned release quantity will exceed the Or" +
            "der quantity + the Overrun %..."
            VIEW-AS ALERT-BOX WARNING.

    IF ldt-ship NE oe-rel.rel-date                      AND
        NOT ll-new-record                               AND
        CAN-FIND(FIRST bf-rel
        WHERE bf-rel.company  EQ oe-rel.company
        AND bf-rel.ord-no   EQ oe-rel.ord-no
        AND bf-rel.link-no  EQ 0
        AND bf-rel.rel-date EQ ldt-ship
        AND ROWID(bf-rel)   NE ROWID(oe-rel)) THEN 
    DO:
        MESSAGE "Update all other Scheduled Releases for this order with a" SKIP
            "release date of " + STRING(ldt-ship,"99/99/9999") + " to " +
            STRING(oe-rel.rel-date,"99/99/9999")
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll-ans.
        IF ll-ans THEN        
            FOR EACH bf-rel
                WHERE bf-rel.company  EQ oe-rel.company
                AND bf-rel.ord-no   EQ oe-rel.ord-no
                AND bf-rel.link-no  EQ 0
                AND bf-rel.rel-date EQ ldt-ship
                AND ROWID(bf-rel)   NE ROWID(oe-rel):
                RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT lv-stat).
                IF INDEX("SLI",lv-stat) GT 0 THEN bf-rel.rel-date = oe-rel.rel-date.
            END.        
    END.

    IF ls-po NE tt-report.po-no                AND
        CAN-FIND(FIRST bf-rel
        WHERE bf-rel.company  EQ oe-rel.company
        AND bf-rel.ord-no   EQ oe-rel.ord-no
        AND bf-rel.link-no  EQ 0
        AND ROWID(bf-rel)   NE ROWID(oe-rel)) THEN 
    DO:
        MESSAGE "Change item PO Number on all items? "
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.
        IF ll-ans THEN 
        DO:
            ll-ans = NO.
            MESSAGE "All ship dates?"
                VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans.
            IF NOT ll-ans THEN 
            DO:
                ldt-ship = oe-rel.rel-date.
                MESSAGE "Which ship date do you wish to update? " UPDATE ldt-ship.
            END.         
            FOR EACH  bf-rel
                WHERE bf-rel.company EQ oe-rel.company
                AND bf-rel.ord-no  EQ oe-rel.ord-no
                AND bf-rel.link-no EQ 0
                AND (ll-ans OR (bf-rel.rel-date EQ ldt-ship)):
                RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT lv-stat).
                IF INDEX("SLI",lv-stat) GT 0 THEN bf-rel.po-no = tt-report.po-no.               
            END.
        END.        
    END.

    IF (li-ship-no <> 0 AND li-ship-no <> oe-rel.ship-no) /* or
     adm-adding-record */ THEN 
    DO:
        FIND oe-ord WHERE oe-ord.company = oe-rel.company 
            AND oe-ord.ord-no = oe-rel.ord-no NO-LOCK .

        RUN oe/custxship.p (oe-rel.company,
            oe-ord.cust-no,
            oe-rel.ship-id,
            BUFFER shipto).

        IF AVAILABLE shipto THEN 
        DO:
            ASSIGN 
                oe-rel.ship-no      = shipto.ship-no
                oe-rel.ship-addr[1] = shipto.ship-addr[1]
                oe-rel.ship-addr[2] = shipto.ship-addr[2]
                oe-rel.ship-city    = shipto.ship-city
                oe-rel.ship-state   = shipto.ship-state
                oe-rel.ship-zip     = shipto.ship-zip
                oe-rel.ship-i[1]    = shipto.notes[1]
                oe-rel.ship-i[2]    = shipto.notes[2]
                oe-rel.ship-i[3]    = shipto.notes[3]
                oe-rel.ship-i[4]    = shipto.notes[4].
        END.
    END.   

    FIND b-ordl WHERE ROWID(b-ordl) EQ ROWID(oe-ordl).
    b-ordl.t-rel-qty = b-ordl.t-rel-qty + oe-rel.qty - ld-prev-rel-qty.
    FIND b-ordl WHERE ROWID(b-ordl) EQ ROWID(oe-ordl) NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-shared-buffers Dialog-Frame 
PROCEDURE release-shared-buffers :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    RELEASE xoe-ord.
    RELEASE xoe-ordl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-dates Dialog-Frame 
PROCEDURE update-dates :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-date AS DATE NO-UNDO.
    DEFINE VARIABLE ll      AS LOG  NO-UNDO.

    DEFINE BUFFER b-oe-ordl FOR oe-ordl.


    lv-date = DATE(INT(SUBSTR(tt-report.stat,1,2)),
        INT(SUBSTR(tt-report.stat,3,2)),
        INT(SUBSTR(tt-report.stat,5,4))).

  {oe/rel-stat.i lv-stat}

    IF INDEX("AB",lv-stat) GT 0 THEN 
    DO:
        IF AVAILABLE oe-relh THEN 
        DO TRANSACTION:
            FIND CURRENT oe-relh.
            oe-relh.rel-date = lv-date.
            FIND CURRENT oe-relh NO-LOCK.
        END.

        IF AVAILABLE oe-relh THEN RUN oe/d-dudate.w (ROWID(oe-relh)).
    END.

    ELSE
        IF INDEX("SLI",lv-stat) GT 0 THEN 
        DO:
            IF NOT (oeDateAuto-log AND OeDateAuto-Char EQ "Colonial") THEN 
            DO:
                ll = NO.
                IF AVAILABLE oe-ordl AND oe-ordl.req-date GT lv-date THEN
                    MESSAGE "Change order line item due date to " + TRIM(STRING(lv-date)) "?"
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                        UPDATE ll.
                IF ll THEN 
                DO:
                    DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.
                    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl).
                    b-oe-ordl.req-date = lv-date.
                    FIND CURRENT b-oe-ordl NO-LOCK NO-ERROR.
                END.
                ll = NO.
                FIND FIRST oe-ord OF oe-rel NO-LOCK NO-ERROR.
                IF AVAILABLE oe-ord AND oe-ord.due-date GT lv-date THEN
                    MESSAGE "Change order header due date to " + TRIM(STRING(lv-date)) "?"
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                        UPDATE ll.
                IF ll THEN 
                DO:
                    FIND CURRENT oe-ord NO-ERROR.
                    oe-ord.due-date = lv-date.
                    FIND CURRENT oe-ord NO-LOCK NO-ERROR.
                END.
            END.
        END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-ship-id Dialog-Frame 
PROCEDURE new-ship-id :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-shipto FOR shipto.
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST oe-ord
            WHERE oe-ord.company EQ oe-ordl.company 
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-LOCK.

        RUN oe/custxship.p (oe-ordl.company,
            oe-ord.cust-no,
            oe-rel.ship-id:SCREEN-VALUE ,
            BUFFER bf-shipto).

        IF AVAILABLE bf-shipto THEN                           
            ASSIGN
                oe-rel.ship-addr[1]:SCREEN-VALUE = bf-shipto.ship-addr[1]
                oe-rel.ship-city:SCREEN-VALUE    = bf-shipto.ship-city
                oe-rel.ship-state:SCREEN-VALUE   = bf-shipto.ship-state 
                oe-rel.carrier:SCREEN-VALUE      = bf-shipto.carrier
                oe-rel.spare-char-1:SCREEN-VALUE = bf-shipto.loc
                li-ship-no                       = bf-shipto.ship-no.
        IF oeDateAuto-log AND OeDateAuto-Char = "Colonial" THEN
            RUN new-due-date.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-due-date Dialog-Frame 
PROCEDURE new-due-date :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-shipto FOR shipto.
    DEFINE VARIABLE dTempDate AS DATE NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        
        dTempDate = DATE(tt-report.prom-date:SCREEN-VALUE ) NO-ERROR.
        /* Date being entered not yet complete */
        IF ERROR-STATUS:ERROR THEN 
            RETURN. 
    
        FIND FIRST oe-ord
            WHERE oe-ord.company EQ oe-ordl.company 
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-LOCK.

        RUN oe/custxship.p (oe-ordl.company,
            oe-ord.cust-no,
            oe-rel.ship-id:SCREEN-VALUE ,
            BUFFER bf-shipto).
        /* if oeDateAuto is 'colonial' then release date is due date - dock appt days */
        IF AVAILABLE bf-shipto THEN                           
            ASSIGN
                tt-report.stat:SCREEN-VALUE = STRING(
                get-date(DATE(tt-report.prom-date:SCREEN-VALUE ), bf-shipto.spare-int-2, "-")                
                ,"99/99/9999")
                .
      
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-oe-rell B-table-Win 
PROCEDURE create-oe-rell :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-whse            LIKE oe-rell.loc NO-UNDO.
    DEFINE VARIABLE v-loc-bin         LIKE oe-rell.loc-bin NO-UNDO.
    DEFINE VARIABLE v-fgfile          AS LOG       NO-UNDO.
    DEFINE VARIABLE v-none            AS LOG       INIT YES NO-UNDO.
    DEFINE VARIABLE lv-all-or-one     AS cha       NO-UNDO.
    DEFINE VARIABLE lv-rowids         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE li                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ll                AS LOG       NO-UNDO.
    DEFINE VARIABLE ll-bin-tag        AS LOG       NO-UNDO.
    DEFINE VARIABLE lv-job-no         AS CHARACTER FORMAT "x(9)" NO-UNDO.
    DEFINE VARIABLE lv-selected-value AS cha       NO-UNDO. /*all,one,notag*/
    DEFINE VARIABLE v-s-code          AS CHARACTER NO-UNDO.
    DEFINE BUFFER b-reftable FOR reftable.
    DEFINE BUFFER bf-oe-rel  FOR oe-rel.
    DEFINE BUFFER bf-notes   FOR notes .
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-relh FOR oe-relh .

    IF NOT AVAILABLE oe-rel THEN
        RETURN.

    FIND FIRST oe-relh WHERE oe-relh.company   = oe-rel.company
        AND oe-relh.cust-no   = oe-rel.cust-no
        /*
                         AND oe-relh.ship-no   = oe-rel.ship-no
                         AND oe-relh.ship-id   = oe-rel.ship-id */
        AND oe-relh.rel-date  = oe-rel.rel-date
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE oe-relh THEN 
    DO:
        RUN oe/cre-relh.p (INPUT RECID(oe-rel)).
        FIND FIRST bf-oe-ordl NO-LOCK
            WHERE bf-oe-ordl.company EQ oe-rel.company
            AND bf-oe-ordl.ord-no  EQ oe-rel.ord-no
            AND bf-oe-ordl.line    EQ oe-rel.line
            NO-ERROR.
        FIND bf-oe-relh WHERE RECID(bf-oe-relh) EQ relh-recid NO-LOCK.
        IF AVAILABLE bf-oe-ordl THEN 
            FOR EACH notes NO-LOCK WHERE notes.rec_key = bf-oe-ordl.rec_key 
                AND notes.note_type <> "S" AND notes.note_type <> "o"  :
                IF AVAILABLE bf-oe-relh THEN 
                DO:
                    CREATE bf-notes .
                    BUFFER-COPY notes EXCEPT rec_key TO bf-notes .
                    ASSIGN 
                        bf-notes.rec_key = bf-oe-relh.rec_key .
                END.
            END.

        FIND FIRST oe-relh WHERE oe-relh.company   = oe-rel.company
            AND oe-relh.cust-no   = oe-rel.cust-no
            /*
                             AND oe-relh.ship-no   = oe-rel.ship-no
                             AND oe-relh.ship-id   = oe-rel.ship-id */
            AND oe-relh.rel-date  = oe-rel.rel-date
            NO-LOCK NO-ERROR.

    END.
 
    {sys/inc/addrelse.i}

    DO TRANSACTION:
        {sys/inc/relmerge.i}

        FIND FIRST sys-ctrl
            WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name    EQ "BOLWHSE"
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE sys-ctrl THEN 
        DO:
            CREATE sys-ctrl.
            ASSIGN
                sys-ctrl.company = cocode
                sys-ctrl.name    = "BOLWHSE"
                sys-ctrl.descrip = "Default Warehouse for Adding Release/BOL"
                sys-ctrl.log-fld = NO.
            MESSAGE "System control record NOT found. " sys-ctrl.descrip
                UPDATE sys-ctrl.char-fld.
        END.
        IF AVAILABLE sys-ctrl THEN v-whse = sys-ctrl.char-fld.

        FIND FIRST sys-ctrl
            WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name    EQ "BOLPRINT"
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE sys-ctrl THEN 
        DO:
            CREATE sys-ctrl.
            ASSIGN
                sys-ctrl.company = cocode
                sys-ctrl.name    = "BOLPRINT"
                sys-ctrl.descrip = "Print Bill of Lading Headers on Plain Paper"
                sys-ctrl.log-fld = NO.
            MESSAGE "System control record NOT found. " sys-ctrl.descrip
                UPDATE sys-ctrl.char-fld.
        END.
        IF AVAILABLE sys-ctrl THEN v-loc-bin = sys-ctrl.char-fld.

        FIND FIRST sys-ctrl
            WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name    EQ "AUTOPOST"
            NO-LOCK NO-ERROR.
        v-fgfile = AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "FGFILE".
    END.

    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

    DEFINE BUFFER bf-rell FOR oe-rell .
    DEFINE VARIABLE li-nxt-rel-no AS INTEGER NO-UNDO.

    FOR EACH bf-rell
        WHERE bf-rell.company EQ cocode
        AND bf-rell.ord-no  EQ oe-rel.ord-no NO-LOCK 
        BY bf-rell.rel-no DESCENDING:
    
        li-nxt-rel-no =  bf-rell.rel-no.
        LEAVE.  
    END.
    li-nxt-rel-no = li-nxt-rel-no + 1.
    /*========== */

    DO TRANSACTION:
        FIND CURRENT oe-relh EXCLUSIVE.
        FIND CURRENT oe-rel EXCLUSIVE.
        ASSIGN
            oe-relh.printed = NO
            oe-rel.rel-no   = li-nxt-rel-no.
        oe-rel.b-ord-no = oe-relh.b-ord-no.
        FIND CURRENT oe-relh NO-LOCK.
        FIND CURRENT oe-rel NO-LOCK.
    END.

    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ cocode
        AND oe-ordl.ord-no  EQ oe-rel.ord-no
        AND oe-ordl.i-no    EQ oe-rel.i-no
        AND oe-ordl.line    EQ oe-rel.line
        NO-LOCK.

    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-rel.i-no
        NO-LOCK NO-ERROR.
    
    ll-bin-tag = AVAILABLE oe-ordl             AND
        addrelse-cha EQ "Bin/Tag" AND
        CAN-FIND(FIRST fg-bin
        WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ oe-ordl.i-no
        AND fg-bin.qty     GT 0).
    
    IF ll-bin-tag THEN 
    DO:
        ASSIGN
            ll        = NO
            lv-job-no = TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99").

        IF lv-job-no EQ "-00" THEN lv-job-no = "".


        v-s-code  = IF oe-rel.s-code <> "" THEN oe-rel.s-code ELSE
            IF oe-ordl.is-a-component THEN "S" ELSE
            IF AVAILABLE oe-ctrl AND oe-ctrl.ship-from THEN "B" ELSE "I".
        lv-selected-value = "NoTag".
  
        ASSIGN
            lv-all-or-one = /*STRING(ll,"ALL/ONE")*/ lv-selected-value  /*all,one,notag*/
            ll-bin-tag    = lv-all-or-one NE "ONE" OR
                   CAN-FIND(FIRST fg-bin
                            WHERE fg-bin.company EQ cocode
                              AND fg-bin.i-no    EQ oe-ordl.i-no
                              AND fg-bin.job-no  EQ oe-ordl.job-no
                              AND fg-bin.job-no2 EQ oe-ordl.job-no2
                              AND fg-bin.qty     GT 0).
    END.
  
    IF v-none THEN 
    DO TRANSACTION:

  
        CREATE oe-rell.
        ASSIGN
            out-recid          = RECID(oe-rell)
            oe-rell.company    = oe-rel.company
            oe-rell.r-no       = oe-relh.r-no
            oe-rell.rel-no     = li-nxt-rel-no
            oe-rell.loc        = locode
            oe-rell.ord-no     = oe-rel.ord-no
            oe-rell.qty        = oe-rel.qty
            oe-rell.i-no       = oe-rel.i-no
            oe-rell.job-no     = oe-ordl.job-no
            oe-rell.job-no2    = oe-ordl.job-no2
            oe-rell.po-no      = oe-rel.po-no
            oe-rell.line       = oe-rel.line
            oe-rell.printed    = NO
            oe-rell.posted     = NO
            oe-rell.deleted    = NO
            oe-rell.loc        = oe-rel.spare-char-1
            oe-rell.lot-no     = oe-rel.lot-no
            oe-rell.frt-pay    = oe-rel.frt-pay 
            oe-rell.fob-code   = oe-rel.fob-code
            oe-rell.sell-price = oe-rel.sell-price
            oe-rell.zeroPrice  = oe-rel.zeroPrice
            /** Set link to the planned releases **/
            oe-rell.link-no    = oe-rel.r-no
            oe-rell.s-code     = IF oe-rel.s-code <> "" THEN oe-rel.s-code ELSE
                     IF oe-ordl.is-a-component THEN "S" ELSE
                     IF AVAILABLE oe-ctrl AND oe-ctrl.ship-from THEN "B" ELSE "I".
        FIND bf-oe-rel WHERE RECID(bf-oe-rel) = RECID(oe-rel) EXCLUSIVE-LOCK.
        bf-oe-rel.link-no = oe-rell.r-no.
        RELEASE bf-oe-rel.
  
     
       
        IF v-whse EQ "SHIPTO" THEN 
        DO:
            FIND FIRST shipto
                WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ oe-rel.cust-no
                AND shipto.ship-no EQ oe-rel.ship-no
                USE-INDEX ship-no NO-LOCK NO-ERROR.
            IF AVAILABLE shipto THEN 
                ASSIGN
                    oe-rell.loc     = shipto.loc
                    oe-rell.loc-bin = shipto.loc-bin.
        END.
  
        ELSE 
        DO:
            FIND FIRST fg-bin
                WHERE fg-bin.company EQ cocode
                AND fg-bin.job-no  EQ oe-ordl.job-no
                AND fg-bin.job-no2 EQ oe-ordl.job-no2
                AND fg-bin.i-no    EQ oe-rell.i-no
                AND fg-bin.qty     GE oe-rell.qty
                USE-INDEX job NO-LOCK NO-ERROR.
      
            IF NOT AVAILABLE fg-bin THEN
                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.job-no  EQ oe-ordl.job-no
                    AND fg-bin.job-no2 EQ oe-ordl.job-no2
                    AND fg-bin.i-no    EQ oe-rell.i-no
                    AND fg-bin.qty     GT 0
                    USE-INDEX job NO-LOCK NO-ERROR.
        
            IF NOT AVAILABLE fg-bin THEN
                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.job-no  EQ oe-ordl.job-no
                    AND fg-bin.job-no2 EQ oe-ordl.job-no2
                    AND fg-bin.i-no    EQ oe-rell.i-no
                    USE-INDEX job NO-LOCK NO-ERROR.
        
            IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" THEN
                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ oe-rell.i-no
                    AND fg-bin.ord-no  EQ oe-rel.ord-no
                    AND fg-bin.qty     GT 0
                    USE-INDEX co-ino NO-LOCK NO-ERROR.
        
            IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" THEN
                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ oe-rell.i-no
                    AND fg-bin.ord-no  EQ oe-rel.ord-no
                    USE-INDEX co-ino NO-LOCK NO-ERROR.
        
            IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" THEN
                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ oe-rell.i-no
                    AND fg-bin.qty     GT 0
                    USE-INDEX co-ino NO-LOCK NO-ERROR.
        
            IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" THEN
                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ oe-rell.i-no
                    USE-INDEX co-ino NO-LOCK NO-ERROR.
        
            IF AVAILABLE fg-bin THEN 
            DO:
                ASSIGN
                    oe-rell.loc      = fg-bin.loc
                    oe-rell.loc-bin  = fg-bin.loc-bin
                    oe-rell.tag      = fg-bin.tag
                    oe-rell.job-no   = fg-bin.job-no
                    oe-rell.job-no2  = fg-bin.job-no2
                    oe-rell.qty-case = fg-bin.case-count.
       
            END.
                           
            ELSE 
                IF v-fgfile THEN 
                DO:
                    FIND FIRST itemfg
                        WHERE itemfg.company EQ cocode
                        AND itemfg.i-no    EQ oe-rell.i-no
                        NO-LOCK NO-ERROR.
                    ASSIGN
                        oe-rell.loc     = itemfg.def-loc
                        oe-rell.loc-bin = itemfg.def-loc-bin.
                END.
        END.
  
        IF oe-rell.loc EQ "" OR oe-rell.loc-bin EQ "" THEN 
        DO:
            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ oe-rell.i-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE itemfg THEN
                ASSIGN
                    oe-rell.loc     = itemfg.def-loc
                    oe-rell.loc-bin = itemfg.def-loc-bin.
            IF oe-rell.loc EQ "" OR oe-rell.loc-bin EQ "" THEN 
            DO:
                FIND FIRST cust WHERE cust.company EQ cocode
                    AND cust.active  EQ "X" 
                    NO-LOCK NO-ERROR.
                IF AVAILABLE cust THEN 
                DO:
                    FIND FIRST shipto WHERE shipto.company EQ cocode
                        AND shipto.cust-no EQ cust.cust-no
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE shipto THEN
                        ASSIGN   
                            oe-rell.loc     = shipto.loc
                            oe-rell.loc-bin = shipto.loc-bin.
                END.            
            END.
        END.

        IF oe-rell.loc-bin EQ "" THEN oe-rell.loc-bin = v-loc-bin.

        IF oe-rell.qty-case EQ 0 THEN
            oe-rell.qty-case = IF AVAILABLE itemfg AND itemfg.case-count GT 0
                THEN itemfg.case-count
                ELSE
                IF oe-ordl.cas-cnt GT 0 THEN oe-ordl.cas-cnt
                ELSE 1.

        ASSIGN
            oe-rell.cases   = TRUNC((oe-rell.qty - oe-rell.partial) /
                            oe-rell.qty-case,0)
            oe-rell.partial = oe-rell.qty - (oe-rell.cases * oe-rell.qty-case). 

        IF oe-rell.qty LT 0 OR lv-selected-value = "NoTag" THEN oe-rell.tag = "".
  
        RUN oe/rel-stat-upd.p (ROWID(oe-rell)).
    END. /* transaction */

    IF avail(oe-rell) AND oe-rell.s-code = "I" THEN 
    DO TRANSACTION:
        ASSIGN 
            oe-rell.loc-bin = ""
            oe-rell.job-no  = ""
            oe-rell.job-no2 = 0
            .
    END.

    DO TRANSACTION:  
        FIND CURRENT oe-rel EXCLUSIVE.
        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).
        FIND CURRENT oe-rel NO-LOCK.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-colonial-date Dialog-Frame 
PROCEDURE valid-colonial-date :
    /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lValid    AS LOGICAL NO-UNDO.
  
    DO WITH FRAME {&FRAME-NAME}:
        ld-date = DATE(INT(SUBSTR(tt-report.stat:SCREEN-VALUE,1,2)),
            INT(SUBSTR(tt-report.stat:SCREEN-VALUE,4,2)),
            INT(SUBSTR(tt-report.stat:SCREEN-VALUE,7,4))) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN 
        DO:
            MESSAGE ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO tt-report.stat IN FRAME {&FRAME-NAME}.
            RETURN ERROR.
        END.

        ELSE tt-report.stat:SCREEN-VALUE =
                STRING(ld-date,tt-report.stat:FORMAT IN FRAME {&FRAME-NAME}).
    
        RUN oe/dateFuture.p (INPUT cocode, INPUT ld-date, INPUT YES /* prompt */, OUTPUT lValid, OUTPUT lContinue).
        IF NOT lValid AND  NOT lContinue THEN 
        DO:
            APPLY "entry" TO tt-report.prom-date IN FRAME {&FRAME-NAME}.
            RETURN ERROR.
        END. 
      
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-date-change Dialog-Frame 
PROCEDURE valid-date-change :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        DEFINE VARIABLE v-reject-code AS CHARACTER NO-UNDO.
        v-reject-code = oe-rel.spare-char-2:SCREEN-VALUE.
                       
        FIND FIRST rejct-cd 
            WHERE rejct-cd.TYPE = "R" 
            AND rejct-cd.CODE = v-reject-code
            NO-LOCK NO-ERROR.
   

        IF NOT AVAILABLE rejct-cd AND v-reject-code GT "" THEN 
        DO:
            MESSAGE "Invalid " + TRIM(oe-rel.spare-char-2:LABEL IN FRAME {&FRAME-NAME}) +
                ", try help..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO oe-rel.spare-char-2 IN FRAME {&FRAME-NAME}.
            RETURN ERROR.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fob Dialog-Frame 
PROCEDURE valid-fob :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DO WITH FRAME {&FRAME-NAME}:
      
        IF NOT CAN-DO("O,D,",tt-report.flute:SCREEN-VALUE) THEN 
        DO:
            MESSAGE "Invalid FOB, please enter (D)est or (O)rig." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO tt-report.flute IN FRAME {&FRAME-NAME}.
            RETURN ERROR.
        END.
        ASSIGN 
            tt-report.flute:SCREEN-VALUE = CAPS(tt-report.flute:SCREEN-VALUE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-freight-pay Dialog-Frame 
PROCEDURE valid-freight-pay :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:

        IF NOT CAN-DO("P,C,B,T,",tt-report.frt-pay:SCREEN-VALUE) THEN 
        DO:
            MESSAGE "Invalid Freight Pay code, pls enter P,C,B or T" VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO tt-report.frt-pay IN FRAME {&FRAME-NAME}.
            RETURN ERROR.
        END.
        ASSIGN 
            tt-report.frt-pay:SCREEN-VALUE = CAPS(tt-report.frt-pay:SCREEN-VALUE). 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-key-02 Dialog-Frame 
PROCEDURE valid-key-02 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lValid    AS LOGICAL NO-UNDO.
  
    DO WITH FRAME {&FRAME-NAME}:
        ld-date = DATE(INT(SUBSTR(tt-report.stat:SCREEN-VALUE,1,2)),
            INT(SUBSTR(tt-report.stat:SCREEN-VALUE,4,2)),
            INT(SUBSTR(tt-report.stat:SCREEN-VALUE,7,4))) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN 
        DO:
            MESSAGE ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO tt-report.stat IN FRAME {&FRAME-NAME}.
            RETURN ERROR.
        END.

        ELSE tt-report.stat:SCREEN-VALUE =
                STRING(ld-date,tt-report.stat:FORMAT IN FRAME {&FRAME-NAME}).
    
        RUN oe/dateFuture.p (INPUT cocode, INPUT ld-date, INPUT YES /* prompt */, OUTPUT lValid, OUTPUT lContinue).
        IF NOT lValid AND  NOT lContinue THEN 
        DO:
            APPLY "entry" TO tt-report.stat IN FRAME {&FRAME-NAME}.
            RETURN ERROR.
        END. 
      
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no Dialog-Frame 
PROCEDURE valid-po-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER cust-po-mand FOR reftable.

  
    DO WITH FRAME {&FRAME-NAME}:
        RELEASE cust.

        FIND FIRST oe-ord NO-LOCK
            WHERE oe-ord.company EQ oe-rel.company
            AND oe-ord.ord-no  EQ oe-rel.ord-no
            NO-ERROR.

        IF AVAILABLE oe-ord THEN
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ oe-ord.company
                AND cust.cust-no EQ oe-ord.cust-no
                AND cust.po-mandatory
                NO-ERROR.
    
        IF AVAILABLE cust AND TRIM(tt-report.po-no:SCREEN-VALUE) EQ "" THEN 
        DO:
            MESSAGE "PO# is mandatory for this Customer..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO tt-report.po-no IN FRAME {&FRAME-NAME}.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-code Dialog-Frame 
PROCEDURE valid-s-code :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DO WITH FRAME {&FRAME-NAME}:
        IF LOOKUP(oe-rel.s-code:SCREEN-VALUE,lv-s-codes) LE 0 THEN 
        DO:
            MESSAGE "Invalid " + TRIM(oe-rel.s-code:LABEL IN FRAME {&FRAME-NAME}) +
                ", try help..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO oe-rel.s-code IN FRAME {&FRAME-NAME}.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ship-from Dialog-Frame 
PROCEDURE valid-ship-from :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST loc WHERE loc.company EQ cocode
            AND loc.loc     EQ TRIM(oe-rel.spare-char-1:SCREEN-VALUE)) THEN 
        DO:       
            MESSAGE "Invalid ship from entered."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO oe-rel.spare-char-1 IN FRAME {&FRAME-NAME}.
            RETURN ERROR.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ship-id Dialog-Frame 
PROCEDURE valid-ship-id :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST oe-ord
            WHERE oe-ord.company EQ oe-rel.company 
            AND oe-ord.ord-no  EQ oe-rel.ord-no
            NO-LOCK.

        /*RUN oe/custxship.p (oe-rel.company,
                            oe-ord.cust-no,
                            oe-rel.ship-id:SCREEN-VALUE,
        BUFFER shipto).*/   /* Task 10241303 */

        /* Task 10241303 */     
        FIND FIRST shipto NO-LOCK
            WHERE shipto.company EQ oe-rel.company
            AND shipto.cust-no EQ oe-ord.cust-no
            AND shipto.ship-id EQ oe-rel.ship-id:SCREEN-VALUE
            USE-INDEX ship-id NO-ERROR.

        IF NOT AVAILABLE shipto AND oe-rel.s-code:SCREEN-VALUE EQ "T" THEN 
        DO:
            FOR EACH cust NO-LOCK
                WHERE cust.company EQ oe-rel.company
                AND cust.active  EQ "X",
                EACH shipto
                WHERE shipto.company EQ cust.company
                AND shipto.cust-no EQ cust.cust-no
                AND shipto.ship-id EQ oe-rel.ship-id:SCREEN-VALUE:
                LEAVE.
            END.
            IF NOT AVAILABLE shipto THEN  
            DO:
                MESSAGE "Invalid " + TRIM(oe-rel.ship-id:LABEL IN FRAME {&FRAME-NAME}) +
                    ", try help..." VIEW-AS ALERT-BOX.
                APPLY "entry" TO oe-rel.ship-id IN FRAME {&FRAME-NAME}.
                RETURN ERROR.
            END.
        END.

        IF AVAILABLE shipto THEN li-ship-no = shipto.ship-no.
        ELSE
            IF NOT AVAILABLE shipto AND LOOKUP(oe-rel.s-code:SCREEN-VALUE,"B,I,S")  <> 0 THEN 
            DO:
                FOR EACH cust NO-LOCK
                    WHERE cust.company EQ oe-rel.company
                    AND cust.active  EQ "X",
                    EACH shipto
                    WHERE shipto.company EQ cust.company
                    AND shipto.cust-no EQ cust.cust-no
                    AND shipto.ship-id EQ oe-rel.ship-id:SCREEN-VALUE:
                    LEAVE.
                END.
                IF AVAILABLE shipto THEN 
                DO:
                    MESSAGE "Billable Releases must be shipped to Ship To Locations for Customer on Order.." VIEW-AS ALERT-BOX.
                    APPLY "entry" TO oe-rel.ship-id IN FRAME {&FRAME-NAME}.
                    RETURN ERROR.
                END.

                ELSE 
                DO:
                    MESSAGE "Invalid " + TRIM(oe-rel.ship-id:LABEL IN FRAME {&FRAME-NAME}) +
                        ", try help..." VIEW-AS ALERT-BOX.
                    APPLY "entry" TO oe-rel.ship-id IN FRAME {&FRAME-NAME}.
                    RETURN ERROR.
                END.     /* Task 10241303 */

            END.
    /*IF AVAILABLE shipto AND NOT DYNAMIC-FUNCTION("IsActive", shipto.rec_key) THEN 
          MESSAGE "Please note: Shipto " shipto.ship-id " is valid but currently inactive"
          VIEW-AS ALERT-BOX.*/
            
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-rel-qty Dialog-Frame 
FUNCTION get-rel-qty RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-oe-rel FOR oe-rel.
    IF AVAILABLE oe-rel THEN
        FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ROWID(oe-rel)
            NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bf-oe-rel THEN 
    DO:
        FIND bf-oe-rel WHERE RECID(bf-oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.
        IF AVAILABLE bf-oe-rel THEN
            FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(bf-oe-rel) NO-ERROR.
    END.

    RETURN IF /*(NOT oereleas-log AND INDEX("AB",get-rel-stat()) GT 0) OR*/
        (INDEX("SIL",get-rel-stat()) GT 0     
        OR oe-rel.stat:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "") THEN 0
         ELSE
         IF AVAIL tt-report                 AND
            INDEX("AB",get-rel-stat()) GT 0 THEN tt-report.qty
         ELSE
         IF AVAIL bf-oe-rel THEN bf-oe-rel.qty
         ELSE INT(oe-rel.qty:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-rel-stat Dialog-Frame 
FUNCTION get-rel-stat RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    IF NOT AVAILABLE oe-rel THEN
        FIND oe-rel WHERE RECID(oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.

    RUN oe/rel-stat.p (IF AVAILABLE oe-rel THEN ROWID(oe-rel) ELSE ?, OUTPUT lv-stat).

    RETURN lv-stat.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-tot-qty Dialog-Frame 
FUNCTION get-tot-qty RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    IF NOT AVAILABLE oe-rel THEN 
    DO:
        FIND oe-rel WHERE RECID(oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.
        IF AVAILABLE oe-rel THEN
            FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(oe-rel) NO-ERROR.
    END.

    RETURN IF NOT oereleas-log                AND
        AVAILABLE tt-report                 AND
            INDEX("AB",get-rel-stat()) GT 0 THEN tt-report.qty
        ELSE
        IF AVAILABLE oe-rel THEN oe-rel.tot-qty
        ELSE INT(oe-rel.tot-qty:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

