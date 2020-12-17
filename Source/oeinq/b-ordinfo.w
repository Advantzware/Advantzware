&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: oeinq/d-ordinfo.w

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.
&SCOPED-DEFINE yellowColumnsName d-ordinfo#

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER iprRowid AS ROWID NO-UNDO .
DEFINE INPUT PARAMETER ipLocation AS CHARACTER NO-UNDO .
/* Local Variable Definitions ---                                       */

/*{methods/prgsecur.i}    */           
{system/sysconst.i}
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}               
{sys/inc/VAR.i NEW SHARED}

ASSIGN 
    cocode = g_company
    locode = g_loc.

DEFINE NEW SHARED VARIABLE uperiod       AS INTEGER   NO-UNDO.  /* for gl-open.p */

DEFINE            VARIABLE v-show-disc   AS LOG       NO-UNDO.
DEFINE            VARIABLE choice        AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-pre-disc   AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lv-pre-paid   AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lv-first      AS LOG       INIT YES NO-UNDO.
DEFINE            VARIABLE lv-num-rec    AS INTEGER   NO-UNDO.
DEFINE            VARIABLE lv-in-add     AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-in-update  AS LOG       NO-UNDO.

DEFINE            VARIABLE v-paidflg     AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-t-rec-qty  LIKE po-ordl.t-rec-qty NO-UNDO.
DEFINE            VARIABLE cPoStatus     AS CHARACTER NO-UNDO .
DEFINE            VARIABLE cPoLineStatus AS CHARACTER NO-UNDO .
DEFINE NEW SHARED VARIABLE factor#       AS DECIMAL   NO-UNDO.

{sys/inc/oeinq.i}
{sys/inc/ou6brows.i}

DEFINE VARIABLE ll-first             AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE lv-sort-by           AS CHARACTER INIT "req-date" NO-UNDO.
DEFINE VARIABLE lv-sort-by-lab       AS CHARACTER INIT "Due Date" NO-UNDO.
DEFINE VARIABLE ll-sort-asc          AS LOG       NO-UNDO.
/*DEF VAR char-hdl AS CHAR NO-UNDO.*/
/*DEF VAR phandle AS HANDLE NO-UNDO.*/
DEFINE VARIABLE lv-frst-rowid        AS ROWID     NO-UNDO.
DEFINE VARIABLE lv-last-rowid        AS ROWID     NO-UNDO.
DEFINE VARIABLE lv-ord-no            LIKE oe-ord.ord-no NO-UNDO.
DEFINE VARIABLE lv-ord-qty           LIKE oe-ordl.qty NO-UNDO.
DEFINE VARIABLE lv-inv-qty           LIKE oe-ordl.inv-qty NO-UNDO.
DEFINE VARIABLE li-prod              AS INTEGER   NO-UNDO.
DEFINE VARIABLE li-bal               AS INTEGER   NO-UNDO.
DEFINE VARIABLE ld-cost              AS DECIMAL   FORMAT "->>,>>>,>>9.99<<<<" NO-UNDO.
DEFINE VARIABLE ld-cost-uom          AS CHARACTER NO-UNDO.
DEFINE VARIABLE fi_cad-no            AS CHARACTER NO-UNDO.
DEFINE VARIABLE ll-alloc             AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE ll-browse-first      AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE lv-show-prev         AS LOG       NO-UNDO.
DEFINE VARIABLE lv-show-next         AS LOG       NO-UNDO.
DEFINE VARIABLE lv-last-show-ord-no  AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-first-show-ord-no AS INTEGER   NO-UNDO.
DEFINE VARIABLE li-act-bol-qty       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-last-ord-no       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-first-ord-no      AS INTEGER   NO-UNDO.
DEFINE VARIABLE ll-show-all          AS LOG       NO-UNDO.
DEFINE VARIABLE li-wip               AS INTEGER   NO-UNDO.
DEFINE VARIABLE li-pct               AS INTEGER   NO-UNDO.
DEFINE VARIABLE li-qoh               AS INTEGER   NO-UNDO.
DEFINE VARIABLE li-act-rel-qty       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lc-fgitem            AS CHARACTER NO-UNDO FORMAT 'X(20)'.
DEFINE VARIABLE lc-ord-po            AS CHARACTER NO-UNDO FORMAT 'X(20)'.
DEFINE VARIABLE li-ship-qty          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ld-price             LIKE oe-ordl.price NO-UNDO.
DEFINE VARIABLE ld-xfer-qty          LIKE oe-ordl.ship-qty NO-UNDO.
DEFINE VARIABLE ld-uom               LIKE oe-ordl.pr-uom NO-UNDO.
DEFINE VARIABLE lr-rel-lib           AS HANDLE    NO-UNDO.
DEFINE VARIABLE ld-t-price           LIKE oe-ordl.t-price NO-UNDO.
DEFINE VARIABLE v-col-move           AS LOG       INIT TRUE NO-UNDO.
DEFINE VARIABLE v-print-fmt          AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-rec-key-list       AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-last-shipto        LIKE oe-ordl.ship-id NO-UNDO.
DEFINE VARIABLE dTotQtyRet           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dTotRetInv           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iHandQtyNoalloc      AS INTEGER   NO-UNDO .
DEFINE VARIABLE lActive              AS LOG       NO-UNDO.

DO TRANSACTION:
    {sys/ref/CustList.i NEW}
    {sys/inc/custlistform.i ""OQ1"" }
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no


/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-ordl oe-rel oe-ord

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 oe-ordl.ord-no oe-ordl.cust-no ~
oe-ord.cust-name oe-ordl.qty oe-ordl.ship-qty get-xfer-qty () @ ld-xfer-qty oe-ordl.req-date ~
get-price-disc () @ ld-price get-pr-uom() @ ld-uom ~
get-extended-price() @ ld-t-price oe-ordl.i-no oe-ordl.part-no ~
oe-ordl.po-no get-ord-po-no() @ lc-ord-po oe-ordl.est-no oe-ordl.job-no oe-ordl.job-no2 oe-ord.ord-date ~
oe-ord.stat get-ord-qty () @ lv-ord-qty get-ship-qty() @ li-ship-qty ~
oe-ordl.inv-qty get-prod (li-bal) @ li-prod get-bal (li-qoh) @ li-bal ~
get-act-rel-qty() @ li-act-rel-qty get-wip() @ li-wip ~
get-pct(li-bal) @ li-pct get-fgitem() @ lc-fgitem oe-ordl.i-name ~
oe-ordl.line oe-ordl.cost get-cost-uom() @ ld-cost-uom ~
oe-ordl.po-no-po get-cost() @ ld-cost ~
get-last-shipto() @ v-last-shipto ~
get-act-bol-qty() @ li-act-bol-qty  ~
oe-ordl.cust-no oe-ord.cust-name oe-ordl.qty oe-ordl.ship-qty ~
oe-ordl.req-date oe-ordl.i-no oe-ordl.part-no oe-ordl.po-no oe-ordl.est-no ~
oe-ordl.job-no oe-ordl.job-no2 oe-ord.ord-date oe-ord.stat oe-ordl.inv-qty ~
oe-ordl.i-name getTotalReturned() @ dTotQtyRet getReturnedInv() @ dTotRetInv ~
fget-qty-nothand(get-act-rel-qty() + get-act-bol-qty(),li-qoh) @ iHandQtyNoalloc
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH oe-ordl   ~
    WHERE oe-ordl.company EQ cocode ~
          AND ( (lookup(oe-ordl.cust-no,custcount) <> 0 AND oe-ordl.cust-no <> "") OR custcount = "") ~
          AND oe-ordl.opened  EQ YES AND oe-ordl.i-no EQ itemfg.i-no ~
          AND ( oe-ordl.stat    NE "C"  OR oe-ordl.stat EQ "") NO-LOCK, ~
    FIRST oe-ord NO-LOCK                             ~
        WHERE oe-ord.company  EQ oe-ordl.company      ~
          AND oe-ord.ord-no   EQ oe-ordl.ord-no     ~
          AND (oe-ord.opened EQ YES ),              ~
       FIRST oe-rel WHERE oe-rel.company = oe-ordl.company         ~
         AND oe-rel.ord-no = oe-ordl.ord-no                     ~
         AND oe-rel.i-no = oe-ordl.i-no                         ~
         AND oe-rel.line = oe-ordl.line                         ~
         AND (oe-rel.spare-char-1 EQ ipLocation OR ipLocation EQ "*All" ) NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH oe-ordl   ~
    WHERE oe-ordl.company EQ cocode ~
          AND ( (lookup(oe-ordl.cust-no,custcount) <> 0 AND oe-ordl.cust-no <> "") OR custcount = "") ~
          AND oe-ordl.opened  EQ YES AND oe-ordl.i-no EQ itemfg.i-no ~
          AND ( oe-ordl.stat    NE "C"  OR oe-ordl.stat EQ "") NO-LOCK, ~
    FIRST oe-ord NO-LOCK                             ~
        WHERE oe-ord.company  EQ oe-ordl.company      ~
          AND oe-ord.ord-no   EQ oe-ordl.ord-no     ~
          AND (oe-ord.opened EQ YES ),              ~
       FIRST oe-rel WHERE oe-rel.company = oe-ordl.company         ~
         AND oe-rel.ord-no = oe-ordl.ord-no                     ~
         AND oe-rel.i-no = oe-ordl.i-no                         ~
         AND oe-rel.line = oe-ordl.line                         ~
         AND (oe-rel.spare-char-1 EQ ipLocation OR ipLocation EQ "*All" ) NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 oe-ordl oe-rel oe-ord
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 oe-ordl
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 oe-ord
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-1 oe-rel


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 btn-ok RECT-1 
&Scoped-Define DISPLAYED-OBJECTS citem cLoc cItemName fi_sortby

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-act-bol-qty C-Win 
FUNCTION get-act-bol-qty RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-act-rel-qty C-Win 
FUNCTION get-act-rel-qty RETURNS INTEGER
    ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-bal C-Win 
FUNCTION get-bal RETURNS INTEGER
    (OUTPUT op-qoh AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-cost C-Win 
FUNCTION get-cost RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-cost-uom C-Win 
FUNCTION get-cost-uom RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-extended-price C-Win 
FUNCTION get-extended-price RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-fgitem C-Win 
FUNCTION get-fgitem RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-inv-qty C-Win 
FUNCTION get-inv-qty RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-last-shipto C-Win 
FUNCTION get-last-shipto RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-ord-po-no C-Win 
FUNCTION get-ord-po-no RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-ord-qty C-Win 
FUNCTION get-ord-qty RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-pct C-Win 
FUNCTION get-pct RETURNS INTEGER
    (ipBal AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-pr-uom C-Win 
FUNCTION get-pr-uom RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-price-disc C-Win 
FUNCTION get-price-disc RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-prod C-Win 
FUNCTION get-prod RETURNS INTEGER
    ( OUTPUT op-bal AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fget-qty-nothand C-Win 
FUNCTION fget-qty-nothand RETURNS INTEGER
    (ipBal AS INTEGER,ipHand AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-ship-qty C-Win 
FUNCTION get-ship-qty RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-wip C-Win 
FUNCTION get-wip RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-xfer-qty C-Win 
FUNCTION get-xfer-qty RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNumTempRecs C-Win 
FUNCTION getNumTempRecs RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME*/


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReturned C-Win
FUNCTION getReturned RETURNS DECIMAL 
    (ipcValueNeeded AS CHARACTER  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReturnedInv C-Win 
FUNCTION getReturnedInv RETURNS DECIMAL
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotalReturned C-Win 
FUNCTION getTotalReturned RETURNS DECIMAL
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-ok 
    IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
    LABEL "OK" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE VARIABLE citem     AS CHARACTER FORMAT "X(15)":U 
    LABEL "FG Item #" 
    VIEW-AS FILL-IN 
    SIZE 23.2 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cItemName AS CHARACTER FORMAT "X(30)":U 
    VIEW-AS FILL-IN 
    SIZE 36.4 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cLoc      AS CHARACTER FORMAT "X(10)":U 
    LABEL "Location" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
    SIZE 122.2 BY 2.14
    BGCOLOR 15 .

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 47 BY 1
    BGCOLOR 14 FONT 6 NO-UNDO.


/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
    oe-ordl, 
    oe-ord,
    oe-rel SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _FREEFORM
    QUERY BROWSE-1 DISPLAY
    oe-ordl.ord-no FORMAT ">>>>>9":U LABEL-BGCOLOR 14
    oe-ordl.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
    LABEL-BGCOLOR 14
    oe-ord.cust-name FORMAT "x(30)":U LABEL-BGCOLOR 14
    oe-ordl.qty COLUMN-LABEL "Order!Quantity" FORMAT "->>,>>>,>>>":U
    WIDTH 15.4 LABEL-BGCOLOR 14
    oe-ordl.ship-qty COLUMN-LABEL "Shipped!Quantity" FORMAT "->>,>>>,>>>":U
    LABEL-BGCOLOR 14
    get-xfer-qty () @ ld-xfer-qty COLUMN-LABEL "Transfer!Qty" FORMAT "->>,>>>,>>>":U
    oe-ordl.req-date COLUMN-LABEL "Due Date" FORMAT "99/99/9999":U
    LABEL-BGCOLOR 14
    get-price-disc () @ ld-price COLUMN-LABEL "    Sell Price" FORMAT ">>,>>>,>>9.99<<<<":U
    WIDTH 20
    get-pr-uom() @ ld-uom COLUMN-LABEL "UOM" FORMAT "X(4)":U
    get-extended-price() @ ld-t-price COLUMN-LABEL "Extended!Price" FORMAT "->>,>>>,>>9.99":U
    WIDTH 20
    oe-ordl.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U LABEL-BGCOLOR 14
    oe-ordl.part-no FORMAT "x(15)":U LABEL-BGCOLOR 14
    oe-ordl.po-no FORMAT "x(15)":U LABEL-BGCOLOR 14
    get-ord-po-no() @ lc-ord-po COLUMN-LABEL "Order PO#" FORMAT "X(15)":U
    WIDTH 21 LABEL-BGCOLOR 14
    oe-ordl.est-no COLUMN-LABEL "Est #" FORMAT "x(8)":U WIDTH 13.8
    LABEL-BGCOLOR 14
    oe-ordl.job-no COLUMN-LABEL "Job #" FORMAT "x(6)":U WIDTH 12
    LABEL-BGCOLOR 14
    oe-ordl.job-no2 COLUMN-LABEL "" FORMAT ">9":U 
    oe-ord.ord-date COLUMN-LABEL "Order Date" FORMAT "99/99/9999":U
    LABEL-BGCOLOR 14
    oe-ord.stat COLUMN-LABEL "Status" FORMAT "x":U LABEL-BGCOLOR 14
    get-ord-qty () @ lv-ord-qty COLUMN-LABEL "Order!Quantity" FORMAT "->>,>>>,>>>":U
    LABEL-BGCOLOR 14
    get-ship-qty() @ li-ship-qty COLUMN-LABEL "Shipped!Quantity" FORMAT "->>,>>>,>>>":U
    WIDTH 14
    oe-ordl.inv-qty COLUMN-LABEL "Invoice!Quantity" FORMAT "->>,>>>,>>>":U
    LABEL-BGCOLOR 14
    get-prod (li-bal) @ li-prod COLUMN-LABEL "Prod. Qty" FORMAT "->>,>>>,>>>":U
    WIDTH 14
    get-bal (li-qoh) @ li-bal COLUMN-LABEL "On Hand Qty" FORMAT "->>,>>>,>>>":U
    WIDTH 16
    get-act-rel-qty() @ li-act-rel-qty COLUMN-LABEL "Act. Rel.!Quantity" FORMAT "->>,>>>,>>>":U
    WIDTH 15.4
    get-wip() @ li-wip COLUMN-LABEL "Production!Balance" FORMAT "->>,>>>,>>>":U
    WIDTH 14.2
    get-pct(li-bal) @ li-pct COLUMN-LABEL "O/U%" FORMAT "->>>>>%":U
    get-fgitem() @ lc-fgitem COLUMN-LABEL "FG Item#" FORMAT "X(15)":U
    WIDTH 21
    oe-ordl.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
    LABEL-BGCOLOR 14
    oe-ordl.line FORMAT ">>99":U
    get-cost() @ ld-cost COLUMN-LABEL "Invoice Line Cost" WIDTH 24 
    get-cost-uom() @ ld-cost-uom COLUMN-LABEL "Cost!UOM"
    oe-ordl.po-no-po FORMAT ">>>>>9":U
    get-last-shipto() @ v-last-shipto COLUMN-LABEL "Last!ShipTo" FORMAT "x(8)":U
    get-act-bol-qty() @ li-act-bol-qty COLUMN-LABEL "Act. BOL!Qty" FORMAT "->>,>>>,>>>":U
    WIDTH 15.4    
    getTotalReturned() @ dTotQtyRet COLUMN-LABEL "Total Qty Returned" FORMAT ">>>,>>9":U
    getReturnedInv() @ dTotRetInv COLUMN-LABEL "Qty Ret. Inventory" FORMAT ">>>,>>9":U
    fget-qty-nothand(get-act-rel-qty() + get-act-bol-qty(),li-qoh) @ iHandQtyNoalloc COLUMN-LABEL "On Hand Qty not Allocated" FORMAT "->>>>>>>>":U
    oe-ordl.cost COLUMN-LABEL "Order Line Cost" WIDTH 23 LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 132.8 BY 15.76
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    BROWSE-1 AT ROW 4.81 COL 2.2
    btn-ok AT ROW 1.90 COL 126
    citem AT ROW 2.14 COL 13.8 COLON-ALIGNED WIDGET-ID 200
    cLoc AT ROW 2.14 COL 95.4 COLON-ALIGNED WIDGET-ID 198
    cItemName AT ROW 2.14 COL 39.6 COLON-ALIGNED NO-LABELS WIDGET-ID 318
    RECT-1 AT ROW 1.71 COL 1.8 WIDGET-ID 82
    fi_sortby AT ROW 3.76 COL 10 COLON-ALIGNED NO-LABELS
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.2 ROW 1
    SIZE 149 BY 23.71
    FGCOLOR 1 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Query,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "View Order"
        HEIGHT             = 20.57
        WIDTH              = 135.8
        MAX-HEIGHT         = 24.71
        MAX-WIDTH          = 156
        VIRTUAL-HEIGHT     = 24.71
        VIRTUAL-WIDTH      = 156
        RESIZE             = NO
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = ?
        FGCOLOR            = ?
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                         */
/* BROWSE-TAB BROWSE-1 1 F-Main */
ASSIGN 
    BROWSE-1:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

ASSIGN 
       BROWSE-1:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
    fi_sortby:HIDDEN IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN citem IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cItemName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cLoc IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH oe-ordl   ~
    WHERE oe-ordl.company EQ cocode ~
          AND ( (lookup(oe-ordl.cust-no,custcount) <> 0 AND oe-ordl.cust-no <> "") OR custcount = "") ~
          AND oe-ordl.opened  EQ YES AND oe-ordl.i-no EQ itemfg.i-no ~
          AND ( oe-ordl.stat    NE "C"  OR oe-ordl.stat EQ "") NO-LOCK, ~
    FIRST oe-ord NO-LOCK                             ~
        WHERE oe-ord.company  EQ oe-ordl.company      ~
          AND oe-ord.ord-no   EQ oe-ordl.ord-no     ~
          AND (oe-ord.opened EQ YES ),              ~
       FIRST oe-rel WHERE oe-rel.company = oe-ordl.company         ~
         AND oe-rel.ord-no = oe-ordl.ord-no                     ~
         AND oe-rel.i-no = oe-ordl.i-no                         ~
         AND oe-rel.line = oe-ordl.line                         ~
         AND (oe-rel.spare-char-1 EQ ipLocation OR ipLocation EQ "*All" ) NO-LOCK
                                 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

                 
/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* New Miscellaneous Product Estimate - Releases */
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
ON WINDOW-CLOSE OF C-Win /* New Miscellaneous Product Estimate - Releases */
    DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
  
        /*  APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
        */
        APPLY "choose" TO btn-ok IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME F-Main
    DO:
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON ROW-ENTRY OF BROWSE-1 IN FRAME F-Main
    DO:
    /* This code displays initial values for newly added or copied rows. */
    /*{src/adm/template/brsentry.i}*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON START-SEARCH OF BROWSE-1 IN FRAME F-Main
    DO:
        RUN startSearch.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME F-Main /* OK */
    DO:
    
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME BROWSE-1
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

    FIND FIRST itemfg WHERE ROWID(itemfg) EQ iprRowid  NO-LOCK NO-ERROR.

    {custom/yellowColumns.i}
    RUN enable_UI.
    {methods/nowait.i}
    /* Ticket# : 92946
       Hiding this widget for now, as browser's column label should be indicating the column which is sorted by */
    fi_sortby:HIDDEN  = TRUE.
    fi_sortby:VISIBLE = FALSE.
    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'OQ1',
        INPUT YES,
        OUTPUT lActive).
    {sys/inc/chblankcust.i ""OQ1""}

    CLOSE QUERY BROWSE-1.
    DO WITH FRAME {&FRAME-NAME}:
        IF AVAILABLE itemfg THEN 
            ASSIGN
                citem     = itemfg.i-no 
                cItemName = itemfg.i-name
                cLoc      = ipLocation
                .
        DISPLAY citem cItemName cLoc.
 
        /*   IF AVAILABLE itemfg THEN*/
        OPEN QUERY BROWSE-1 FOR EACH oe-ordl   
            WHERE oe-ordl.company EQ cocode
            AND ( (LOOKUP(oe-ordl.cust-no,custcount) <> 0 AND oe-ordl.cust-no <> "") OR custcount = "") 
            AND ( oe-ordl.opened  EQ YES)
            AND ( oe-ordl.stat    NE "C"  OR oe-ordl.stat EQ "")
            AND oe-ordl.i-no EQ itemfg.i-no NO-LOCK ,
            FIRST oe-ord NO-LOCK
            WHERE oe-ord.company  EQ oe-ordl.company
            AND oe-ord.ord-no   EQ oe-ordl.ord-no
            AND (oe-ord.opened EQ YES ),
            FIRST oe-rel WHERE oe-rel.company = oe-ordl.company 
            AND oe-rel.ord-no = oe-ordl.ord-no 
            AND oe-rel.i-no = oe-ordl.i-no 
            AND oe-rel.line = oe-ordl.line 
            AND (oe-rel.spare-char-1 EQ ipLocation OR ipLocation EQ "*All" ) NO-LOCK
            BY oe-ordl.ord-no BY oe-ordl.i-no.
    END.


    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects C-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

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
    DISPLAY btn-ok RECT-1 citem cLoc cItemName BROWSE-1 
        WITH FRAME F-Main IN WINDOW C-Win.
    ENABLE btn-ok RECT-1 BROWSE-1 
        WITH FRAME F-Main IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    VIEW FRAME F-Main IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit C-Win 
PROCEDURE local-exit :
    /* -----------------------------------------------------------
          Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
          Parameters:  <none>
          Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
        -------------------------------------------------------------*/
    APPLY "CLOSE":U TO THIS-PROCEDURE.
   
    RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed C-Win 
PROCEDURE state-changed :
    /* -----------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        -------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-act-bol-qty C-Win 
FUNCTION get-act-bol-qty RETURNS INTEGER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE liReturn AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-stat  AS CHARACTER NO-UNDO.

    IF AVAILABLE oe-ordl AND VALID-HANDLE(lr-rel-lib) THEN 
    DO:
        FOR EACH oe-rel WHERE
            oe-rel.company EQ cocode AND
            oe-rel.ord-no  EQ oe-ordl.ord-no AND
            oe-rel.i-no    EQ oe-ordl.i-no AND
            oe-rel.line    EQ oe-ordl.LINE AND
            LOOKUP(oe-rel.stat, "P") GT 0
            NO-LOCK:

            RUN get-act-qty IN lr-rel-lib (INPUT ROWID(oe-rel), OUTPUT liReturn).
            li = li + liReturn.

        END.
    END.

    RETURN li.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-act-rel-qty C-Win 
FUNCTION get-act-rel-qty RETURNS INTEGER
    () :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-stat AS CHARACTER NO-UNDO.

    IF AVAILABLE oe-ordl THEN
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-bal C-Win 
FUNCTION get-bal RETURNS INTEGER
    (OUTPUT op-qoh AS INTEGER) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iTotalJobOnHandQty AS INTEGER NO-UNDO.

    /*   IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN */
    FOR EACH job-hdr FIELDS(company job-no job-no2 i-no)
        WHERE job-hdr.company EQ oe-ordl.company 
        AND job-hdr.ord-no EQ oe-ordl.ord-no 
        AND job-hdr.i-no EQ oe-ordl.i-no
        USE-INDEX ord-no
        NO-LOCK
        BREAK BY job-hdr.job-no BY job-hdr.job-no2 BY job-hdr.i-no:
        IF LAST-OF(job-hdr.i-no) THEN 
        DO:    
            FOR EACH fg-bin FIELDS (qty)
                WHERE fg-bin.company EQ job-hdr.company
                AND fg-bin.job-no  EQ job-hdr.job-no
                AND fg-bin.job-no2 EQ job-hdr.job-no2
                AND fg-bin.i-no    EQ job-hdr.i-no
                NO-LOCK:
                iTotalJobOnHandQty = iTotalJobOnHandQty + fg-bin.qty.
            END.
        END.
    END.
    op-qoh = iTotalJobOnHandQty.
    RETURN iTotalJobOnHandQty.    /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-cost C-Win 
FUNCTION get-cost RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    FIND FIRST ar-invl NO-LOCK WHERE ar-invl.company EQ oe-ord.company
        AND ar-invl.ord-no EQ oe-ord.ord-no
        AND ar-invl.i-no EQ oe-ordl.i-no NO-ERROR.
    RETURN IF AVAILABLE ar-invl THEN ar-invl.cost ELSE 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-cost-uom C-Win 
FUNCTION get-cost-uom RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    FIND FIRST ar-invl NO-LOCK WHERE ar-invl.company EQ oe-ord.company
        AND ar-invl.ord-no EQ oe-ord.ord-no
        AND ar-invl.i-no EQ oe-ordl.i-no NO-ERROR.
    IF AVAILABLE ar-invl THEN
    DO:
        IF ar-invl.dscr[1] EQ "" THEN
            RETURN "M".
        ELSE
            RETURN ar-invl.dscr[1].
    END.
    ELSE
        RETURN "M".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-extended-price C-Win 
FUNCTION get-extended-price RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-oe-ordl FOR oe-ordl.

    DEFINE VARIABLE ld          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-tmp-price AS DECIMAL FORMAT "->,>>>,>>9.9999" NO-UNDO.
    DEFINE VARIABLE lv-t-price  AS DECIMAL NO-UNDO.

    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK.

    ld = b-oe-ordl.t-price.

    IF oeinq-char NE "Order Price" THEN
        FOR EACH ar-invl FIELDS(inv-no amt i-no unit-pr disc) WHERE
            ar-invl.company EQ cocode AND
            ar-invl.ord-no EQ b-oe-ordl.ord-no AND
            ar-invl.i-no EQ b-oe-ordl.i-no
            NO-LOCK
            BY ar-invl.inv-no DESCENDING:

            FIND FIRST itemfg
                {sys/look/itemfgrlW.i}
                AND itemfg.i-no EQ ar-invl.i-no
            NO-LOCK NO-ERROR.

            ASSIGN
                v-tmp-price = IF b-oe-ordl.pr-uom BEGINS "L" AND b-oe-ordl.pr-uom NE "LB" THEN
                       IF b-oe-ordl.qty LT 0 THEN -1 ELSE 1
                       ELSE
                       IF b-oe-ordl.pr-uom EQ "CS" THEN
                          b-oe-ordl.qty / (IF b-oe-ordl.cas-cnt NE 0 THEN b-oe-ordl.cas-cnt ELSE
                                          IF AVAILABLE itemfg AND itemfg.case-count NE 0
                                                         THEN itemfg.case-count ELSE
                                                              1)
                       ELSE
                       IF b-oe-ordl.pr-uom EQ "C" THEN
                          b-oe-ordl.qty / 100
                       ELSE
                       IF b-oe-ordl.pr-uom EQ "M" THEN
                         b-oe-ordl.qty / 1000
                       ELSE
                         b-oe-ordl.qty

                lv-t-price  = v-tmp-price * ar-invl.unit-pr
                ld          = IF v-print-fmt EQ "Dayton" THEN 
                (lv-t-price - ROUND(lv-t-price * ar-invl.disc / 100,2))
              ELSE
                ROUND(lv-t-price * (1 - (ar-invl.disc / 100)),2).

            LEAVE.
        END.

    RETURN ld.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-fgitem C-Win 
FUNCTION get-fgitem RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    RETURN oe-ordl.i-no.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-inv-qty C-Win 
FUNCTION get-inv-qty RETURNS INTEGER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-oe-ordl FOR oe-ordl.

    DEFINE VARIABLE lp-inv-qty AS INTEGER NO-UNDO.

    ASSIGN 
        lp-inv-qty = 0.

    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK.

    FOR EACH ar-invl  WHERE
        ar-invl.company EQ cocode AND
        ar-invl.ord-no EQ oe-ordl.ord-no AND
        ar-invl.i-no EQ oe-ordl.i-no
        NO-LOCK:

        lp-inv-qty = lp-inv-qty + ar-invl.inv-qty.
    END.

    RETURN lp-inv-qty.

/* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-last-shipto C-Win 
FUNCTION get-last-shipto RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  Task 09111201 
    Add new field called  "Last Shipto" on  O-Q-1.
    This will print the last or next shipto code form the Release Folder.
    
    When multiple release lines exist for the item, the program will look for a release in the following priority:
    Release Status C for Completed.
    Release Status Z for Invoiced.
    Release Status P for posted release that is in the bill of lading file.
    Release Status B for back ordered release
    Release Status A for Actual Release.
    Release Status I for Invoicable / past warehouse terms.
    Release Status L for late.
    Release Status S for released.
    
    The logic is to print the history of the shipment first back to the release status.
        Notes:  
    ------------------------------------------------------------------------------*/

    DEFINE BUFFER buf-oe-rel FOR oe-rel.

    FOR EACH buf-oe-rel NO-LOCK WHERE 
        buf-oe-rel.company EQ cocode AND
        buf-oe-rel.ord-no  EQ oe-ordl.ord-no AND
        buf-oe-rel.i-no    EQ oe-ordl.i-no   AND
        buf-oe-rel.line    EQ oe-ordl.LINE
        BY (IF buf-oe-rel.stat = "C" THEN 1 
        ELSE IF buf-oe-rel.stat = "Z" THEN 2 
        ELSE IF buf-oe-rel.stat = "P" THEN 3 
        ELSE IF buf-oe-rel.stat = "B" THEN 4 
        ELSE IF buf-oe-rel.stat = "A" THEN 5 
        ELSE IF buf-oe-rel.stat = "I" THEN 6 
        ELSE IF buf-oe-rel.stat = "L" THEN 7 
        ELSE IF buf-oe-rel.stat = "S" THEN 8         
        ELSE 12)
        :

        RETURN buf-oe-rel.ship-id.        

    END.

    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-ord-po-no C-Win 
FUNCTION get-ord-po-no RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    RETURN oe-ord.po-no.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-ord-qty C-Win 
FUNCTION get-ord-qty RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    RETURN IF AVAILABLE oe-ordl THEN oe-ordl.qty ELSE 0. 
/* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-pct C-Win 
FUNCTION get-pct RETURNS INTEGER
    (ipBal AS INTEGER) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

    IF AVAILABLE oe-ordl AND oe-ordl.qty NE 0 THEN 
    DO:

        rtnValue = ((ipBal / oe-ordl.qty) - 1) * 100.
        IF rtnValue EQ 0 THEN rtnValue = 100.
        IF rtnValue EQ -100 THEN rtnValue = 0.
    END.

    RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-pr-uom C-Win 
FUNCTION get-pr-uom RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-oe-ordl FOR oe-ordl.

    DEFINE VARIABLE lv-uom AS CHARACTER NO-UNDO.

    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK.

    lv-uom = b-oe-ordl.pr-uom.

    FOR EACH ar-invl FIELDS(inv-no pr-uom) WHERE
        ar-invl.company EQ cocode AND
        ar-invl.ord-no EQ b-oe-ordl.ord-no AND
        ar-invl.i-no EQ b-oe-ordl.i-no
        NO-LOCK
        BY ar-invl.inv-no DESCENDING:

        lv-uom = ar-invl.pr-uom.
        LEAVE.
    END.

    RETURN lv-uom.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-price-disc C-Win 
FUNCTION get-price-disc RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-oe-ordl FOR oe-ordl.

    DEFINE VARIABLE ld AS DECIMAL NO-UNDO.

    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK.

    ld = b-oe-ordl.price * (1 - (b-oe-ordl.disc / 100)).

    FOR EACH ar-invl FIELDS(inv-no unit-pr disc) WHERE
        ar-invl.company EQ cocode AND
        ar-invl.ord-no EQ b-oe-ordl.ord-no AND
        ar-invl.i-no EQ b-oe-ordl.i-no
        NO-LOCK
        BY ar-invl.inv-no DESCENDING:

        ld = ar-invl.unit-pr * (1 - (ar-invl.disc / 100)).
        LEAVE.
    END.

    RETURN ld.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-prod C-Win 
FUNCTION get-prod RETURNS INTEGER
    ( OUTPUT op-bal AS INTEGER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iTotalProdQty AS INTEGER NO-UNDO.
    DEFINE VARIABLE iJobProdQty   AS INTEGER NO-UNDO.


    IF AVAILABLE oe-ordl THEN
    DO:
        /*      IF oe-ordl.job-no NE "" THEN                          */
        /*         FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK   */
        /*            WHERE fg-rcpth.company   EQ cocode              */
        /*              AND fg-rcpth.job-no    EQ oe-ordl.job-no      */
        /*              AND fg-rcpth.job-no2   EQ oe-ordl.job-no2     */
        /*              AND fg-rcpth.i-no      EQ oe-ordl.i-no        */
        /*              AND fg-rcpth.rita-code EQ "R"                 */
        /*            USE-INDEX job,                                  */
        /*            EACH fg-rdtlh FIELDS(qty) NO-LOCK               */
        /*            WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no       */
        /*              AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code: */
        /*              iTotalProdQty = iTotalProdQty + fg-rdtlh.qty. */
        /*         END.                                               */
        /*      ELSE                                                  */
        FOR EACH job-hdr FIELDS(company job-no job-no2 i-no) 
            WHERE job-hdr.company EQ cocode 
            AND job-hdr.ord-no EQ oe-ordl.ord-no 
            AND job-hdr.i-no EQ oe-ordl.i-no
            USE-INDEX ord-no
            /*         NO-LOCK,                                       */
            /*         EACH fg-rcpth FIELDS(r-no rita-code)           */
            /*         WHERE fg-rcpth.company   EQ cocode             */
            /*           AND fg-rcpth.job-no    EQ job-hdr.job-no     */
            /*           AND fg-rcpth.job-no2   EQ job-hdr.job-no2    */
            /*           AND fg-rcpth.i-no      EQ oe-ordl.i-no       */
            /*           AND fg-rcpth.rita-code EQ "R"                */
            /*         USE-INDEX job                                  */
            /*         NO-LOCK,                                       */
            /*         EACH fg-rdtlh FIELDS(qty)                      */
            /*         WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no      */
            /*           AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code */
            NO-LOCK
            BREAK BY job-hdr.job-no
            BY job-hdr.job-no2:
            IF FIRST-OF(job-hdr.job-no2) THEN 
            DO:
                RUN fg/GetProductionQty.p (INPUT job-hdr.company,
                    INPUT job-hdr.job-no,
                    INPUT job-hdr.job-no2,
                    INPUT job-hdr.i-no,
                    INPUT NO,
                    OUTPUT iJobProdQty).
                iTotalProdQty = iTotalProdQty + iJobProdQty.
            END.
        END.

        IF oe-ordl.po-no-po NE 0 THEN
            FOR EACH fg-rcpth FIELDS(r-no rita-code)
                WHERE fg-rcpth.company   EQ cocode 
                AND fg-rcpth.po-no     EQ STRING(oe-ordl.po-no-po) 
                AND fg-rcpth.i-no      EQ oe-ordl.i-no 
                AND fg-rcpth.rita-code EQ "R"
                NO-LOCK,
                EACH fg-rdtlh FIELDS(qty) 
                WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no 
                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                NO-LOCK:
                iTotalProdQty = iTotalProdQty + fg-rdtlh.qty.
            END.
    END.

    op-bal = iTotalProdQty.
    RETURN iTotalProdQty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fget-qty-nothand C-Win 
FUNCTION fget-qty-nothand RETURNS INTEGER
    (ipBal AS INTEGER,ipHand AS INTEGER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE irtnValue AS INTEGER NO-UNDO.

    irtnValue = (ipHand  - ipBal ).


    RETURN irtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-ship-qty C-Win 
FUNCTION get-ship-qty RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    RETURN oe-ordl.ship-qty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-wip C-Win 
FUNCTION get-wip RETURNS INTEGER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

    DEFINE BUFFER b-oe-ordl FOR oe-ordl.


    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK NO-ERROR.

    rtnValue = oe-ordl.qty - (li-qoh + oe-ordl.ship-qty).
    IF rtnValue LT 0 OR
        rtnValue LT oe-ordl.qty * b-oe-ordl.under-pct / 100 THEN
        rtnValue = 0.
    RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-xfer-qty C-Win 
FUNCTION get-xfer-qty RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    
    Regardless of Customer Bill to.
    1)  Release Status must be Z or C to add the to the Total Transfer Qty.
    
    2) If Order Type = T then the Release S/I status is irrelevant.  (ie.  Customer X)
         Add Actual Qty transferred for the FG item with Status Z or C.
    
    3) If Order Type not = T.  (Any Customer except Customer X)
         Then Release S/I field MUST BE T and Release Status Z or C.
          Add all Actual Qty transferred for the FG item.
    
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER buf-oe-rel   FOR oe-rel.
    DEFINE BUFFER buf-reftable FOR reftable.
    DEFINE VARIABLE vTransfer-Qty LIKE oe-ordl.ship-qty NO-UNDO INIT 0.

    FOR EACH buf-oe-rel NO-LOCK WHERE 
        buf-oe-rel.company EQ cocode AND
        buf-oe-rel.ord-no  EQ oe-ordl.ord-no AND
        buf-oe-rel.i-no    EQ oe-ordl.i-no   AND
        buf-oe-rel.line    EQ oe-ordl.LINE AND
        (buf-oe-rel.stat = "C" OR buf-oe-rel.stat = "Z"):

        /* If Order Type = T, Skip release when status is NOT Z or C. */
        /*       IF oe-ord.TYPE = "T" AND LOOKUP(buf-oe-rel.stat,"Z,C") = 0  THEN NEXT.  */

        /* Get reftable.code */


        /* If order type NOT T, skip if S/I code is NOT "T". */
        IF oe-ord.TYPE <> "T" AND buf-oe-rel.s-code <> "T" THEN NEXT.


        ASSIGN 
            vTransfer-Qty = (vTransfer-Qty + buf-oe-rel.qty).
    END.

    RETURN vTransfer-Qty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNumTempRecs C-Win 
FUNCTION getNumTempRecs RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE icount AS INT NO-UNDO INIT 0.
  DEFINE BUFFER buf-ord FOR tt-ord.

  FOR EACH buf-ord NO-LOCK:
      ASSIGN iCount = iCount + 1.
  END.


  RETURN iCount.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME*/


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReturned C-Win
FUNCTION getReturned RETURNS DECIMAL 
    (ipcValueNeeded AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTotQtyRet AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotRetInv AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dResult    AS DECIMAL NO-UNDO.

    IF AVAILABLE oe-ordl THEN 
    DO:

        FOR EACH ar-invl WHERE ar-invl.company EQ oe-ordl.company
            AND ar-invl.ord-no EQ oe-ordl.ord-no
            AND ar-invl.i-no EQ oe-ordl.i-no
            NO-LOCK
            BREAK BY ar-invl.inv-no:

            IF FIRST-OF(ar-invl.inv-no) THEN 
            DO:

                FOR EACH oe-reth WHERE oe-reth.company EQ ar-invl.company
                    AND oe-reth.posted EQ TRUE
                    AND oe-reth.applied EQ TRUE 
                    AND oe-reth.cust-no EQ oe-ordl.cust-no
                    AND oe-reth.inv-no EQ ar-invl.inv-no
                    NO-LOCK,
                    EACH oe-retl 
                    WHERE oe-retl.company EQ oe-reth.company
                    AND oe-retl.r-no    EQ oe-reth.r-no
                    AND oe-retl.i-no    EQ ar-invl.i-no
                    NO-LOCK
                    :

                    ASSIGN
                        dTotQtyRet = dTotQtyRet + oe-retl.tot-qty-return        
                        dTotRetInv = dTotRetInv + oe-retl.qty-return-inv.
                END.  /* for each return */
            END. /* if first-of then do */
        END. /* for each ar-invl */
    END. /* avail? */
    IF ipcValueNeeded EQ "TotalReturned" THEN
        dResult = dTotQtyRet.
    ELSE
        dResult = dTotRetInv.


    RETURN dResult.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReturnedInv C-Win 
FUNCTION getReturnedInv RETURNS DECIMAL
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTotQtyRet AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotRetInv AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dResult    AS DECIMAL NO-UNDO.


    dResult = getReturned("ReturnedInv"). 

    RETURN dResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotalReturned C-Win 
FUNCTION getTotalReturned RETURNS DECIMAL
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTotQtyRet AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotRetInv AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dResult    AS DECIMAL NO-UNDO.


    dResult = getReturned("TotalREturned"). 

    RETURN dResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

