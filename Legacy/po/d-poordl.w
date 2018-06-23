&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: po\d-poordl.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.
DEFINE INPUT PARAMETER ip-ord-no LIKE po-ord.po-no NO-UNDO.
DEFINE INPUT PARAMETER ip-type AS cha NO-UNDO .   /* add,update,view */

{custom/globdefs.i}

{sys/inc/var.i new shared}


ASSIGN
    cocode = g_company
    locode = g_loc.

{sa/sa-sls01.i}

DEFINE NEW SHARED VARIABLE v-basis-w AS DECIMAL NO-UNDO. /* for po/po-adder2.p */
DEFINE NEW SHARED VARIABLE v-len     LIKE po-ordl.s-len NO-UNDO.
DEFINE NEW SHARED VARIABLE v-wid     LIKE po-ordl.s-wid NO-UNDO.
DEFINE NEW SHARED VARIABLE v-dep     LIKE po-ordl.s-len NO-UNDO.
DEFINE NEW SHARED VARIABLE v-adder   AS DECIMAL EXTENT 2 NO-UNDO.

DEFINE BUFFER xpo-ord  FOR po-ord.
DEFINE BUFFER xpo-ordl FOR po-ordl.
DEFINE BUFFER buf-item FOR ITEM.
DEFINE BUFFER b-vend   FOR vend.

DEFINE        VARIABLE addersText              AS CHARACTER NO-UNDO.
DEFINE        VARIABLE ll-new-record           AS LOG       NO-UNDO.
DEFINE        VARIABLE lv-item-recid           AS RECID     NO-UNDO.
DEFINE        VARIABLE v-ord-qty               AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE lv-uom-list             AS cha       INIT "C,CS,EA,L,LB,LF,LOT,M,MSF,SHT,TON,BF" NO-UNDO.
DEFINE        VARIABLE pr-uom-list             AS cha       NO-UNDO INIT "EA,LB,M,MSF,TON,BF".
DEFINE        VARIABLE cons-uom-list           AS CHA       NO-UNDO INIT "M,LF,EA,LB,TON".
DEFINE        VARIABLE lv-save-job             AS CHARACTER NO-UNDO.
DEFINE        VARIABLE lv-save-job2            AS CHARACTER NO-UNDO.
DEFINE        VARIABLE lv-save-s-num           AS CHARACTER NO-UNDO.
DEFINE        VARIABLE lv-save-b-num           AS CHARACTER NO-UNDO.
DEFINE        VARIABLE lv-save-ord-qty         AS CHARACTER NO-UNDO.
DEFINE        VARIABLE ld-prev-t-cost          AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE ld-prev-cost            AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE ld-prev-cons-cost       AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE ll-frm-enabled          AS LOG       NO-UNDO.
DEFINE        VARIABLE ll-blk-enabled          AS LOG       NO-UNDO.
DEFINE        VARIABLE lv-itemtype             AS cha       NO-UNDO.
DEFINE        VARIABLE lv-save-fld             AS CHARACTER NO-UNDO.
DEFINE        VARIABLE lv-save-cust-no         AS CHARACTER NO-UNDO.
DEFINE        VARIABLE lv-save-ord-no          AS INTEGER   NO-UNDO.
DEFINE        VARIABLE lv-ord-qty-entered      AS LOG       NO-UNDO.
DEFINE        VARIABLE dCost                   AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-wid-frac              AS CHARACTER NO-UNDO.
DEFINE        VARIABLE v-len-frac              AS CHARACTER NO-UNDO.
DEFINE        VARIABLE v-dep-frac              AS CHARACTER NO-UNDO.
DEFINE        VARIABLE v-number-rows-selected  AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-number-rows-selected2 AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-selected              AS LOGICAL   INIT NO.
DEFINE        VARIABLE ll-ord-no-override      AS LOG       NO-UNDO.

DEFINE SHARED VARIABLE factor#                 AS DECIMAL   NO-UNDO.
DEFINE SHARED VARIABLE v-default-gl-log        AS LOG       NO-UNDO.
DEFINE SHARED VARIABLE v-default-gl-cha        AS cha       NO-UNDO.
DEFINE SHARED VARIABLE v-po-qty                AS LOG       INITIAL TRUE NO-UNDO.
DEFINE SHARED VARIABLE v-po-msf                LIKE sys-ctrl.int-fld NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE item-chg NO-UNDO
    FIELD i-no   LIKE job-mat.i-no
    FIELD rec-id AS RECID.

DEFINE TEMP-TABLE tt-job-mat NO-UNDO LIKE job-mat
    FIELD orig-lot-cost-upd AS LOG
    FIELD orig-lot-cost     AS DECIMAL DECIMALS 6
    FIELD row-id            AS ROWID.

DEFINE TEMP-TABLE tt-s-num NO-UNDO 
    FIELD s-num  LIKE po-ordl.s-num
    FIELD row-id AS ROWID.

DEFINE TEMP-TABLE tt-ei NO-UNDO
    FIELD std-uom AS CHARACTER.

DEFINE TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty  AS DECIMAL   DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL   DECIMALS 4 EXTENT 20
    FIELD setups   AS DECIMAL   DECIMALS 2 EXTENT 20
    FIELD rec_key  AS CHARACTER.

DEFINE TEMP-TABLE tt-eiv-2 NO-UNDO
    FIELD run-qty  AS DECIMAL   DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL   DECIMALS 4 EXTENT 20
    FIELD setups   AS DECIMAL   DECIMALS 2 EXTENT 20
    FIELD rec_key  AS CHARACTER.



DEFINE NEW SHARED VARIABLE fil_id             AS RECID     NO-UNDO.
DEFINE NEW SHARED VARIABLE v-pocost1          AS CHARACTER.
DEFINE NEW SHARED VARIABLE v-hold-op1         AS LOG.

DEFINE            VARIABLE ll-item-validated  AS LOG       NO-UNDO.
DEFINE            VARIABLE ll-order-warned    AS LOG       NO-UNDO.
DEFINE            VARIABLE ll-vend-cost-ran   AS LOG       NO-UNDO.
DEFINE            VARIABLE ll-cost-changed    AS LOG       NO-UNDO.
DEFINE            VARIABLE ll-poord-warned    AS LOG       NO-UNDO.
DEFINE            VARIABLE ll-pojob-warned    AS LOG       NO-UNDO.
DEFINE            VARIABLE ll-new-job-mat     AS LOG       INIT YES NO-UNDO.
DEFINE            VARIABLE ll-ans             AS LOG       NO-UNDO.
DEFINE            VARIABLE fg-uom-list        AS CHARACTER NO-UNDO.
DEFINE            VARIABLE ll-rm-fg-set       AS LOG       NO-UNDO.
DEFINE            VARIABLE ld-prev-setup      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE ld-roll-len        AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-charge           LIKE surcharge.charge.
DEFINE            VARIABLE v-actnum           LIKE po-ordl.actnum.

/* Used in vend-cost */
DEFINE            VARIABLE v-qty              AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-cost             AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-pb-qty           AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-pb-stp           AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-pb-cst           AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-pb-cns           AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-save-qty         AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-setup            AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE li                 AS INTEGER   NO-UNDO.
DEFINE            VARIABLE lv-added-cost      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lv-added-cons-cost AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lv-adder-setup     AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lv-recid           AS RECID     NO-UNDO.
DEFINE            VARIABLE lv-t-cost          AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE ld-dim-charge      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-index            AS INTEGER   NO-UNDO.

/* gdm - 06040918 */
DEFINE BUFFER bf-itemfg        FOR itemfg.  
DEFINE BUFFER bf-e-itemfg      FOR e-itemfg.  
DEFINE BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.

{windows/l-jobmt1.i}

{fg/fullset.i NEW}

DO TRANSACTION:
    {sys/inc/pocostq.i}
    {sys/inc/poqty.i}
    {sys/inc/pouom.i}
    {sys/inc/aptax.i}
    {sys/inc/poscreen.i} /* Tab Order*/
    {sys/ref/postatus.i} 
END.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

FIND FIRST uom NO-LOCK WHERE uom.uom EQ "ROLL" NO-ERROR.
IF AVAILABLE uom THEN ld-roll-len = uom.mult.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES po-ordl item

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame po-ordl.i-no po-ordl.job-no ~
po-ordl.job-no2 po-ordl.s-num po-ordl.b-num po-ordl.due-date po-ordl.stat ~
po-ordl.i-name po-ordl.ord-qty po-ordl.pr-qty-uom po-ordl.cons-qty ~
po-ordl.dscr[1] po-ordl.cost po-ordl.pr-uom po-ordl.cons-cost ~
po-ordl.cons-uom po-ordl.dscr[2] po-ordl.setup po-ordl.s-wid po-ordl.s-len ~
po-ordl.disc po-ordl.actnum po-ordl.vend-i-no po-ordl.tax po-ordl.over-pct ~
po-ordl.under-pct po-ordl.cust-no po-ordl.ord-no po-ordl.t-cost ~
po-ordl.item-type 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame po-ordl.i-no ~
po-ordl.job-no po-ordl.job-no2 po-ordl.s-num po-ordl.b-num po-ordl.due-date ~
po-ordl.i-name po-ordl.ord-qty po-ordl.pr-qty-uom po-ordl.dscr[1] ~
po-ordl.cost po-ordl.pr-uom po-ordl.dscr[2] po-ordl.setup po-ordl.s-wid ~
po-ordl.s-len po-ordl.disc po-ordl.actnum po-ordl.vend-i-no po-ordl.tax ~
po-ordl.over-pct po-ordl.under-pct po-ordl.cust-no po-ordl.ord-no ~
po-ordl.item-type 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame po-ordl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame po-ordl
&Scoped-Define ENABLED-OBJECTS btnCalendar-1
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH po-ordl SHARE-LOCK, ~
      EACH item OF po-ordl SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH po-ordl SHARE-LOCK, ~
      EACH item OF po-ordl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame po-ordl item
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame po-ordl
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame item


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS po-ordl.i-no po-ordl.job-no po-ordl.job-no2 ~
po-ordl.s-num po-ordl.b-num po-ordl.due-date po-ordl.i-name po-ordl.ord-qty ~
po-ordl.pr-qty-uom po-ordl.dscr[1] po-ordl.cost po-ordl.pr-uom ~
po-ordl.dscr[2] po-ordl.setup po-ordl.s-wid po-ordl.s-len po-ordl.disc ~
po-ordl.actnum po-ordl.vend-i-no po-ordl.tax po-ordl.over-pct ~
po-ordl.under-pct po-ordl.cust-no po-ordl.ord-no po-ordl.item-type 
&Scoped-define ENABLED-TABLES po-ordl
&Scoped-define FIRST-ENABLED-TABLE po-ordl
&Scoped-Define ENABLED-OBJECTS btnCalendar-1 Btn_Done Btn_Cancel Btn_OK RECT-21 RECT-38 
&Scoped-Define DISPLAYED-FIELDS po-ordl.i-no po-ordl.job-no po-ordl.job-no2 ~
po-ordl.s-num po-ordl.b-num po-ordl.due-date po-ordl.stat po-ordl.i-name ~
po-ordl.ord-qty po-ordl.pr-qty-uom po-ordl.cons-qty po-ordl.dscr[1] ~
po-ordl.cost po-ordl.pr-uom po-ordl.cons-cost po-ordl.cons-uom ~
po-ordl.dscr[2] po-ordl.setup po-ordl.s-wid po-ordl.s-len po-ordl.disc ~
po-ordl.actnum po-ordl.vend-i-no po-ordl.tax po-ordl.over-pct ~
po-ordl.under-pct po-ordl.cust-no po-ordl.ord-no po-ordl.t-cost ~
po-ordl.item-type 
&Scoped-define DISPLAYED-TABLES po-ordl
&Scoped-define FIRST-DISPLAYED-TABLE po-ordl
&Scoped-Define DISPLAYED-OBJECTS fiCount fi_c-a-hdr fi_uom scr-cons-uom ~
v-tot-msf v-po-dep v-po-wid-frac v-po-len-frac v-po-dep-frac v-gl-desc ~
fi_pb-qty fi_pb-cst fi_q-onh fi_q-ono fi_q-comm fi_q-back fi_q-avail ~
fi_m-onh fi_m-ono fi_m-comm fi_m-back fi_m-avail fi_msf 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 fi_q-onh fi_q-ono fi_q-comm fi_q-back fi_q-avail ~
fi_m-onh fi_m-ono fi_m-comm fi_m-back fi_m-avail 
&Scoped-define calendarPopup btnCalendar-1

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON Btn_Cancel 
    LABEL "Cancel" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
    LABEL "&Done" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
    LABEL "&Save" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE VARIABLE adders        AS CHARACTER 
    VIEW-AS EDITOR SCROLLBAR-VERTICAL
    SIZE 56 BY 6.19
    BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE fiCount       AS INTEGER   FORMAT "->,>>>,>>9":U INITIAL 0 
    LABEL "Count" 
    VIEW-AS FILL-IN 
    SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fi_c-a-hdr    AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi_m-avail    AS DECIMAL   FORMAT "->,>>>,>>9.9<<<<" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1.

DEFINE VARIABLE fi_m-back     AS DECIMAL   FORMAT "->,>>>,>>9.9<<<<" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1.

DEFINE VARIABLE fi_m-comm     AS DECIMAL   FORMAT "->,>>>,>>9.9<<<<" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1.

DEFINE VARIABLE fi_m-onh      AS DECIMAL   FORMAT "->,>>>,>>9.9<<<<" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi_m-ono      AS DECIMAL   FORMAT "->,>>>,>>9.9<<<<" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1.

DEFINE VARIABLE fi_msf        AS CHARACTER FORMAT "X(256)":U INITIAL "MSF" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1
    FGCOLOR 9 NO-UNDO.

DEFINE VARIABLE fi_pb-cst     AS DECIMAL   FORMAT "->,>>>,>>9.99<<":U INITIAL 0 
    LABEL "Next Price Break Cost" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fi_pb-qty     AS DECIMAL   FORMAT "->>>,>>>,>>9.9<<":U INITIAL 0 
    LABEL "Next Price Break Qty" 
    VIEW-AS FILL-IN 
    SIZE 18.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi_q-avail    AS DECIMAL   FORMAT "->>>,>>>,>>9.9<<" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1.

DEFINE VARIABLE fi_q-back     AS DECIMAL   FORMAT "->>>,>>>,>>9.9<<" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1.

DEFINE VARIABLE fi_q-comm     AS DECIMAL   FORMAT "->>>,>>>,>>9.9<<" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1.

DEFINE VARIABLE fi_q-onh      AS DECIMAL   FORMAT "->>>,>>>,>>9.9<<" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi_q-ono      AS DECIMAL   FORMAT "->>>,>>>,>>9.9<<" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1.

DEFINE VARIABLE fi_uom        AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1
    FGCOLOR 9 NO-UNDO.

DEFINE VARIABLE scr-cons-uom  AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 6.8 BY 1.

DEFINE VARIABLE v-gl-desc     AS CHARACTER FORMAT "X(256)":U 
    LABEL "GL Desc" 
    VIEW-AS FILL-IN 
    SIZE 58 BY 1 NO-UNDO.

DEFINE VARIABLE v-po-dep      AS DECIMAL   FORMAT ">>>9.99<<":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE v-po-dep-frac AS CHARACTER FORMAT "X(10)":U 
    VIEW-AS FILL-IN 
    SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE v-po-len-frac AS CHARACTER FORMAT "X(10)":U 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE v-po-wid-frac AS CHARACTER FORMAT "X(10)":U 
    VIEW-AS FILL-IN 
    SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE v-tonnage     AS DECIMAL   FORMAT ">>>>,>>9.999":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE v-tot-msf     AS DECIMAL   FORMAT ">>>>,>>9.999":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 133.8 BY 3.57.

DEFINE RECTANGLE RECT-38
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 133.8 BY 15.76.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
    po-ordl, 
    item SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    fiCount AT ROW 7.67 COL 64 COLON-ALIGNED WIDGET-ID 4
    fi_c-a-hdr AT ROW 17 COL 58 COLON-ALIGNED NO-LABELS
    fi_uom AT ROW 17.95 COL 11 COLON-ALIGNED NO-LABELS
    po-ordl.i-no AT ROW 1.24 COL 17 COLON-ALIGNED
    VIEW-AS FILL-IN 
    SIZE 24 BY 1
    po-ordl.job-no AT ROW 1.24 COL 49.8 COLON-ALIGNED
    LABEL "Job #"
    VIEW-AS FILL-IN 
    SIZE 12 BY 1
    po-ordl.job-no2 AT ROW 1.24 COL 61.8 COLON-ALIGNED NO-LABELS
    VIEW-AS FILL-IN 
    SIZE 6 BY 1
    po-ordl.s-num AT ROW 1.24 COL 71.6 COLON-ALIGNED
    LABEL "S"
    VIEW-AS FILL-IN 
    SIZE 5 BY 1
    po-ordl.b-num AT ROW 1.24 COL 80.4 COLON-ALIGNED
    LABEL "B"
    VIEW-AS FILL-IN 
    SIZE 5 BY 1
    po-ordl.due-date AT ROW 1.24 COL 98.6 COLON-ALIGNED
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    btnCalendar-1 AT ROW 1.24 COL 117
    po-ordl.stat AT ROW 1.24 COL 128 COLON-ALIGNED
    LABEL "Stat"
    VIEW-AS FILL-IN 
    SIZE 3.2 BY 1
    po-ordl.i-name AT ROW 2.67 COL 9 COLON-ALIGNED
    VIEW-AS FILL-IN 
    SIZE 38 BY 1
    po-ordl.ord-qty AT ROW 2.91 COL 64 COLON-ALIGNED FORMAT "->>>,>>>,>>9.9<<<<<"
    VIEW-AS FILL-IN 
    SIZE 18.8 BY 1
    po-ordl.pr-qty-uom AT ROW 2.91 COL 84 COLON-ALIGNED NO-LABELS
    VIEW-AS FILL-IN 
    SIZE 9 BY 1
    po-ordl.cons-qty AT ROW 2.91 COL 105 COLON-ALIGNED HELP
    "Enter Consumption Quantity."
    LABEL "Qty"
    VIEW-AS FILL-IN 
    SIZE 19 BY .95
    scr-cons-uom AT ROW 2.91 COL 124 COLON-ALIGNED NO-LABELS WIDGET-ID 2
    po-ordl.dscr[1] AT ROW 3.62 COL 9 COLON-ALIGNED
    LABEL "Desc"
    VIEW-AS FILL-IN 
    SIZE 38 BY 1
    po-ordl.cost AT ROW 4.1 COL 64 COLON-ALIGNED
    LABEL "Cost" FORMAT "->,>>>,>>9.99<<<<"
    VIEW-AS FILL-IN 
    SIZE 19 BY 1
    po-ordl.pr-uom AT ROW 4.1 COL 84 COLON-ALIGNED NO-LABELS
    VIEW-AS FILL-IN 
    SIZE 9 BY 1
    po-ordl.cons-cost AT ROW 4.1 COL 105 COLON-ALIGNED NO-LABELS FORMAT "->,>>>,>>9.99<<<<"
    VIEW-AS FILL-IN 
    SIZE 19 BY 1
    po-ordl.cons-uom AT ROW 4.1 COL 124 COLON-ALIGNED NO-LABELS
    VIEW-AS FILL-IN 
    SIZE 6.8 BY 1
    po-ordl.dscr[2] AT ROW 4.57 COL 9 COLON-ALIGNED
    LABEL "Desc"
    VIEW-AS FILL-IN 
    SIZE 38 BY 1
    po-ordl.setup AT ROW 5.29 COL 64 COLON-ALIGNED
    LABEL "Setup"
    VIEW-AS FILL-IN 
    SIZE 19 BY 1
    v-tot-msf AT ROW 5.29 COL 105 COLON-ALIGNED NO-LABELS
    po-ordl.s-wid AT ROW 6.62 COL 3.4 COLON-ALIGNED NO-LABELS FORMAT ">>>9.99<<"
    VIEW-AS FILL-IN 
    SIZE 13 BY 1
    po-ordl.s-len AT ROW 6.62 COL 18 COLON-ALIGNED NO-LABELS FORMAT ">>,>>9.99<<"
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    v-po-dep AT ROW 6.62 COL 36 COLON-ALIGNED NO-LABELS
    po-ordl.disc AT ROW 6.48 COL 64 COLON-ALIGNED
    VIEW-AS FILL-IN 
    SIZE 19 BY 1
    v-tonnage AT ROW 6.38 COL 105 COLON-ALIGNED NO-LABELS
    adders AT ROW 7.60 COL 78 NO-LABELS
    v-po-wid-frac AT ROW 7.57 COL 3.4 COLON-ALIGNED NO-LABELS
    v-po-len-frac AT ROW 7.57 COL 18 COLON-ALIGNED NO-LABELS
    v-po-dep-frac AT ROW 7.57 COL 36 COLON-ALIGNED NO-LABELS
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
    po-ordl.actnum AT ROW 8.91 COL 18 COLON-ALIGNED HELP
    "Enter account number."
    LABEL "GL#"
    VIEW-AS FILL-IN 
    SIZE 38 BY 1
    v-gl-desc AT ROW 9.86 COL 18 COLON-ALIGNED
    po-ordl.vend-i-no AT ROW 11.29 COL 18 COLON-ALIGNED
    VIEW-AS FILL-IN 
    SIZE 25 BY 1
    po-ordl.tax AT ROW 11.52 COL 64 COLON-ALIGNED
    LABEL "Tax"
    VIEW-AS FILL-IN 
    SIZE 3.2 BY 1
    po-ordl.over-pct AT ROW 12.29 COL 18 COLON-ALIGNED
    VIEW-AS FILL-IN 
    SIZE 10.4 BY 1
    po-ordl.under-pct AT ROW 13.24 COL 18 COLON-ALIGNED
    VIEW-AS FILL-IN 
    SIZE 10.4 BY 1
    fi_pb-qty AT ROW 13.91 COL 104 COLON-ALIGNED
    po-ordl.cust-no AT ROW 14.19 COL 18 COLON-ALIGNED
    LABEL "Customer#" FORMAT "x(8)"
    VIEW-AS FILL-IN 
    SIZE 16 BY 1
    po-ordl.ord-no AT ROW 15.19 COL 18 COLON-ALIGNED
    LABEL "Order Number" FORMAT ">>>>>9"
    VIEW-AS FILL-IN 
    SIZE 12 BY 1
    po-ordl.t-cost AT ROW 15.1 COL 49 COLON-ALIGNED
    LABEL "Total Cost" FORMAT "->,>>>,>>9.99<<"
    VIEW-AS FILL-IN 
    SIZE 27 BY 1
    fi_pb-cst AT ROW 15.1 COL 104 COLON-ALIGNED
    Btn_Done AT ROW 21.05 COL 57
    Btn_Cancel AT ROW 21.05 COL 77.2
    Btn_OK AT ROW 21.1 COL 37
    po-ordl.item-type AT ROW 1.24 COL 3 NO-LABELS FORMAT "RM/FG"
    VIEW-AS FILL-IN 
    SIZE 5.2 BY 1
    fi_q-onh AT ROW 17.95 COL 18 COLON-ALIGNED HELP
    "Qty on Hand Updated by Purchase Receipts & Production Issues" NO-LABELS
    fi_q-ono AT ROW 17.95 COL 38 COLON-ALIGNED NO-LABELS
    fi_q-comm AT ROW 17.95 COL 58 COLON-ALIGNED NO-LABELS
    fi_q-back AT ROW 17.95 COL 78 COLON-ALIGNED NO-LABELS
    fi_q-avail AT ROW 17.95 COL 98 COLON-ALIGNED NO-LABELS
    fi_m-onh AT ROW 18.91 COL 18 COLON-ALIGNED HELP
    "Qty on Hand Updated by Purchase Receipts & Production Issues" NO-LABELS
    fi_m-ono AT ROW 18.91 COL 38 COLON-ALIGNED NO-LABELS
    fi_m-comm AT ROW 18.91 COL 58 COLON-ALIGNED NO-LABELS
    fi_m-back AT ROW 18.91 COL 78 COLON-ALIGNED NO-LABELS
    fi_m-avail AT ROW 18.91 COL 98 COLON-ALIGNED NO-LABELS
    fi_msf AT ROW 18.91 COL 11 COLON-ALIGNED NO-LABELS
    "MSF" VIEW-AS TEXT
    SIZE 6 BY .62 AT ROW 5.52 COL 127
    "Width" VIEW-AS TEXT
    SIZE 8 BY .62 AT ROW 5.71 COL 5.8
    "Length" VIEW-AS TEXT
    SIZE 8 BY .62 AT ROW 5.71 COL 20.4
    "Depth" VIEW-AS TEXT
    SIZE 8 BY .62 AT ROW 5.71 COL 38.2
    "On Order" VIEW-AS TEXT
    SIZE 18 BY 1 AT ROW 17 COL 40
    "On Hand" VIEW-AS TEXT
    SIZE 18 BY 1 AT ROW 17 COL 20
    "Backordered" VIEW-AS TEXT
    SIZE 18 BY 1 AT ROW 17 COL 80
    "Available" VIEW-AS TEXT
    SIZE 18 BY 1 AT ROW 17 COL 100
    "Tons" VIEW-AS TEXT
    SIZE 6 BY .62 AT ROW 6.57 COL 127
    RECT-21 AT ROW 16.76 COL 1
    RECT-38 AT ROW 1 COL 1
    SPACE(0.00) SKIP(6.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FONT 6
    TITLE "Purchase Order Item Update"
    DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
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

/* SETTINGS FOR FILL-IN po-ordl.actnum IN FRAME Dialog-Frame
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR EDITOR adders IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       adders:HIDDEN IN FRAME Dialog-Frame    = TRUE
       adders:READ-ONLY IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR FILL-IN po-ordl.b-num IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
ASSIGN 
       Btn_Done:HIDDEN IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR FILL-IN po-ordl.cons-cost IN FRAME Dialog-Frame
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN po-ordl.cons-qty IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR FILL-IN po-ordl.cons-uom IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN po-ordl.cost IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN po-ordl.cust-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN po-ordl.dscr[1] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN po-ordl.dscr[2] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fiCount IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_c-a-hdr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_m-avail IN FRAME Dialog-Frame
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi_m-back IN FRAME Dialog-Frame
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi_m-comm IN FRAME Dialog-Frame
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi_m-onh IN FRAME Dialog-Frame
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi_m-ono IN FRAME Dialog-Frame
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi_msf IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_pb-cst IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_pb-qty IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_q-avail IN FRAME Dialog-Frame
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi_q-back IN FRAME Dialog-Frame
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi_q-comm IN FRAME Dialog-Frame
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi_q-onh IN FRAME Dialog-Frame
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi_q-ono IN FRAME Dialog-Frame
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi_uom IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN po-ordl.item-type IN FRAME Dialog-Frame
   ALIGN-L EXP-LABEL EXP-FORMAT                                         */
/* SETTINGS FOR FILL-IN po-ordl.job-no IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN po-ordl.ord-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN po-ordl.ord-qty IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN po-ordl.s-len IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN po-ordl.s-num IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN po-ordl.s-wid IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN scr-cons-uom IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN po-ordl.setup IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN po-ordl.stat IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN po-ordl.t-cost IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN po-ordl.tax IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN v-gl-desc IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-po-dep IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-po-dep-frac IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-po-len-frac IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-po-wid-frac IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-tonnage IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       v-tonnage:HIDDEN IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR FILL-IN v-tot-msf IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.po-ordl,asi.item OF ASI.po-ordl"
     _Options          = "SHARE-LOCK"
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

  ll-ord-no-override = TRUE.
  /* Add with ctrl-o allows user to specify an order number */
  /* prior to getting the next sequence value for order number */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  RUN manual-apply-add IN WIDGET-HANDLE(char-hdl).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Purchase Order Item Update */
DO:
    DEFINE VARIABLE char-val     AS cha           NO-UNDO.
    DEFINE VARIABLE look-recid   AS RECID         NO-UNDO.
    DEFINE VARIABLE lv-job-no    LIKE po-ordl.job-no NO-UNDO.
    DEFINE VARIABLE uom-list     AS cha           NO-UNDO.
    DEFINE VARIABLE lv-i-no      AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE lv-item-type AS LOGICAL       NO-UNDO.
    DEFINE VARIABLE lw-focus     AS WIDGET-HANDLE NO-UNDO.


    ASSIGN
     lw-focus               = FOCUS
     lv-job-no              = FILL(" ", 6 - LENGTH(TRIM(po-ordl.job-no:SCREEN-VALUE))) +
                 TRIM(po-ordl.job-no:SCREEN-VALUE)
     v-number-rows-selected = 0.
    
    CASE lw-focus:NAME:
        WHEN "i-no" THEN DO:
            IF lv-job-no NE "" THEN DO:
              RUN windows/l-jobmt2.w (g_company, lv-job-no, INT(po-ordl.job-no2:SCREEN-VALUE), po-ordl.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid, OUTPUT v-number-rows-selected).              
              IF char-val NE "" THEN RUN new-job-mat (look-recid).              
            END.
            ELSE DO:
              RUN windows/l-itmtyp.w (OUTPUT lv-itemtype).
              IF lv-itemtype = "RM" THEN DO:
                RUN windows/l-itmall.w (g_company, "","", po-ordl.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
                IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO:                    
                  ASSIGN lw-focus:SCREEN-VALUE       = ENTRY(1,char-val)
                         po-ordl.i-name:SCREEN-VALUE = ENTRY(2,char-val).
                  RUN display-rmitem (look-recid).
                END.
              END.
              ELSE DO:  /* finished good */
                RUN windows/l-itemf2.w (g_company, "", po-ordl.i-no:screen-value, OUTPUT char-val, OUTPUT look-recid).
                IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO:
                  ASSIGN lw-focus:SCREEN-VALUE       = ENTRY(1,char-val)
                         po-ordl.i-name:screen-value = ENTRY(2,char-val).
                  RUN display-fgitem (look-recid) .                 
                END.                           
              END.
            END.
        END.
        WHEN "job-no" THEN DO:
            RUN lookup-job.
        END.
        WHEN "job-no2" THEN DO:
            RUN lookup-job.
        END.
        WHEN "s-num" THEN DO:
            IF lv-job-no NE "" THEN DO:
              RUN windows/l-jobmt2.w (g_company, lv-job-no, INT(po-ordl.job-no2:SCREEN-VALUE), po-ordl.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid, OUTPUT v-number-rows-selected).
              IF char-val NE "" THEN RUN new-job-s-b (look-recid).
            END.
        END.
        WHEN "b-num" THEN DO:
            IF lv-job-no NE "" THEN DO:
              RUN windows/l-jobmt2.w (g_company, lv-job-no, INT(po-ordl.job-no2:SCREEN-VALUE), po-ordl.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid, OUTPUT v-number-rows-selected).
              IF char-val NE "" THEN RUN new-job-s-b (look-recid).
            END.
        END.
        WHEN "actnum" THEN DO:
             RUN windows/l-acct3.w(g_company,"T",lw-focus:SCREEN-VALUE, OUTPUT char-val).
             IF char-val <> "" THEN ASSIGN lw-focus:SCREEN-VALUE  = ENTRY(1,char-val)
                                           v-gl-desc:SCREEN-VALUE = ENTRY(2,char-val).
        END.
        WHEN "cust-no" THEN DO:
            RUN windows/l-cust.w (g_company, po-ordl.cust-no:SCREEN-VALUE, OUTPUT char-val).
            IF char-val <> "" THEN ASSIGN po-ordl.cust-no:SCREEN-VALUE = ENTRY(1,char-val).
            APPLY 'ENTRY':U TO po-ordl.cust-no.
        END.
        WHEN "ord-no" THEN DO:            
            /* gdm - CHANGED TO ACCOMODATE RM vs FG LOOKUP */
            ASSIGN
              lv-item-type = po-ordl.item-type:SCREEN-VALUE EQ 'FG'
              lv-i-no      = po-ordl.i-no:SCREEN-VALUE.

            IF lv-item-type 
              THEN 
                RUN windows/l-ordlno.w(g_company, po-ordl.cust-no:SCREEN-VALUE,"",lv-i-no,lw-focus:SCREEN-VALUE, OUTPUT char-val,OUTPUT look-recid ).
              ELSE 
                RUN windows/l-ordmno.w(g_company, po-ordl.cust-no:SCREEN-VALUE,"",lv-i-no,lw-focus:SCREEN-VALUE, OUTPUT char-val,OUTPUT look-recid ).

            IF char-val <> "" THEN ASSIGN lw-focus:SCREEN-VALUE        = ENTRY(1,char-val)
                                          po-ordl.cust-no:SCREEN-VALUE = ENTRY(2,char-val).

        END.
        WHEN "pr-qty-uom" OR WHEN "pr-uom" THEN DO:
             FIND FIRST item WHERE item.company = g_company AND
                                   item.i-no = po-ordl.i-no:SCREEN-VALUE NO-LOCK NO-ERROR.
             IF AVAILABLE ITEM THEN DO:
                RUN sys/ref/uom-rm.p  (item.mat-type, OUTPUT uom-list).
                RUN windows/l-stduom.w (g_company,uom-list, lw-focus:SCREEN-VALUE, OUTPUT char-val).
             END.
             ELSE DO:
                RUN sys/ref/uom-fg.p  (NO, OUTPUT uom-list).
                RUN windows/l-stduom.w (g_company,uom-list, lw-focus:SCREEN-VALUE, OUTPUT char-val).              
             END.
             IF char-val <> "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
        END.
    END CASE.
    RUN check-workfile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 Dialog-Frame
ON CHOOSE OF btnCalendar-1 IN FRAME Dialog-Frame
DO:
  {methods/btnCalendar.i po-ordl.due-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME po-ordl.due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.due-date Dialog-Frame
ON HELP OF po-ordl.due-date IN FRAME Dialog-Frame /* Due Date */
DO:
  {methods/calpopup.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Purchase Order Item Update */
ANYWHERE
DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Purchase Order Item Update */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.actnum Dialog-Frame
ON LEAVE OF po-ordl.actnum IN FRAME Dialog-Frame /* GL# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    FIND FIRST account NO-LOCK
         WHERE account.company EQ g_company
           AND account.actnum  EQ SELF:SCREEN-VALUE NO-ERROR.
    v-gl-desc:SCREEN-VALUE = IF AVAILABLE account THEN account.dscr ELSE ''.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.b-num Dialog-Frame
ON ENTRY OF po-ordl.b-num IN FRAME Dialog-Frame /* B */
DO:
  IF lv-save-b-num NE "" THEN lv-save-b-num = {&self-name}:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.b-num Dialog-Frame
ON LEAVE OF po-ordl.b-num IN FRAME Dialog-Frame /* B */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-b-num NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.


    IF po-ordl.item-type:SCREEN-VALUE NE "rm" THEN DO:
      FIND FIRST itemfg WHERE itemfg.company EQ g_company
          AND itemfg.i-no EQ po-ordl.i-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.
      /* If a purchased fg component then pull info from farm tab */
      IF AVAILABLE itemfg AND itemfg.pur-man EQ TRUE THEN
        RUN getJobFarmInfo.
    END. /* If a finished good */

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.b-num Dialog-Frame
ON VALUE-CHANGED OF po-ordl.b-num IN FRAME Dialog-Frame /* B */
DO:
  ASSIGN
   ll-poord-warned = NO
   ll-pojob-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    DISABLE TRIGGERS FOR LOAD OF po-ordl.

    IF lv-item-recid <> ? THEN DO:
       FIND po-ordl EXCLUSIVE-LOCK WHERE RECID(po-ordl) = lv-item-recid  NO-ERROR.
       IF AVAILABLE po-ordl THEN DELETE po-ordl.
    END.
    APPLY 'GO':U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
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
  DEFINE VARIABLE ll       AS LOG     NO-UNDO.
  DEFINE VARIABLE op-error AS LOG     NO-UNDO.

  DEFINE BUFFER b-po-ordl FOR po-ordl.

  IF ip-type = "view" THEN DO: 
     APPLY "go" TO FRAME {&frame-name}.
     RETURN.
  END.

  IF NOT ll-item-validated THEN DO:
     RUN validate-i-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  RUN valid-job-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-job-no2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-s-num NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-b-num NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-uom ("pr-qty-uom") NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  RUN valid-uom ("pr-uom") NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-actnum NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  RUN validate-all NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-ord-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-vend-cost (YES) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-min-wid (YES) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-min-len(YES) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-sheet-board-proc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-max-po-cost(OUTPUT op-error).
  IF op-error THEN RETURN NO-APPLY.

  RUN update-shipto.

  RUN po/poordlup.p (RECID(po-ordl), -1, YES).

  lv-save-ord-no = po-ordl.ord-no.

  DO WITH FRAME {&FRAME-NAME}:
    FIND CURRENT po-ordl EXCLUSIVE-LOCK NO-ERROR.

    IF (po-ordl.vend-i-no:MODIFIED OR po-ordl.ord-qty:MODIFIED OR
        po-ordl.cost:MODIFIED OR po-ordl.pr-uom:MODIFIED OR
        po-ordl.i-name:MODIFIED OR po-ordl.disc:MODIFIED OR
        po-ordl.tax:MODIFIED OR po-ordl.due-date:MODIFIED )
        AND ip-type = "update" THEN DO:
      FIND CURRENT po-ord EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN
       /*po-ord.printed        = no*/
       po-ord.po-change-date = TODAY.
      IF po-ordl.stat NE "C" THEN DO:
        /*po-ordl.stat:SCREEN-VALUE   = "U".*/
        IF INDEX("HN", po-ord.stat) EQ 0 THEN po-ord.stat = "O".
      END.
    END.
    IF pr-qty-uom:SCREEN-VALUE EQ "CS" THEN DO:
      pr-qty-uom:SCREEN-VALUE = "EA".
      po-ordl.spare-int-1 = 1.
      po-ordl.ord-qty:SCREEN-VALUE = 
         STRING(DEC(po-ordl.ord-qty:SCREEN-VALUE) * DEC(fiCount:SCREEN-VALUE)).
    END.
    ELSE
      po-ordl.spare-int-1 = 0.

    IF pr-uom:SCREEN-VALUE EQ "CS" THEN DO:
      pr-uom:SCREEN-VALUE = "EA".
      po-ordl.spare-int-2 = 1.
      
      po-ordl.cost:SCREEN-VALUE = 
         STRING(DEC(po-ordl.cost:SCREEN-VALUE) / DEC(fiCount:SCREEN-VALUE)).
    END.
    ELSE
      po-ordl.spare-int-2 = 0.

    ASSIGN
     {&FIELDS-IN-QUERY-{&frame-name}}
     {po/calc10.i po-ordl.s-wid}
     {po/calc10.i po-ordl.s-len}
     v-dep = DEC(v-po-dep:SCREEN-VALUE)
     {po/calc10.i v-dep}.

     /* wfk - to make sure cons-qty was being updated */
    FIND CURRENT po-ordl EXCLUSIVE-LOCK NO-ERROR.
    {po/podisdet.i}
    
    
IF TRIM(po-ordl.job-no) EQ "" THEN po-ordl.job-no2 = 0.
FIND CURRENT po-ordl NO-LOCK NO-ERROR.
FIND FIRST reftable WHERE
    reftable.reftable EQ "POORDLDEPTH" AND
    reftable.company  EQ cocode AND
    reftable.loc      EQ STRING(ip-ord-no) AND
    reftable.code     EQ STRING(po-ordl.LINE)
    EXCLUSIVE-LOCK NO-ERROR.

IF NOT AVAILABLE reftable THEN 
DO:
    CREATE reftable.
    ASSIGN
        reftable.reftable = "POORDLDEPTH"
        reftable.company  = cocode 
        reftable.loc      = STRING(ip-ord-no)
        reftable.code     = STRING(po-ordl.LINE).
END.

reftable.code2 = STRING(v-dep).
FIND CURRENT reftable NO-LOCK NO-ERROR.
RELEASE reftable.
END.
FIND CURRENT po-ord NO-LOCK NO-ERROR.
FIND CURRENT po-ordl NO-LOCK NO-ERROR.
      
ll = NO.

IF po-ord.type EQ "D"               AND
    po-ord.cust-no NE ""             AND
    po-ordl.ord-no NE 0              AND
    po-ordl.ord-no NE lv-save-ord-no THEN
    FOR EACH oe-ordl NO-LOCK                                              
        WHERE oe-ordl.company  EQ po-ordl.company
        AND oe-ordl.ord-no   EQ po-ordl.ord-no
        AND oe-ordl.job-no   EQ po-ordl.job-no
        AND oe-ordl.job-no2  EQ po-ordl.job-no2
        AND (oe-ordl.job-no  EQ ""            OR
        oe-ordl.form-no EQ po-ordl.s-num OR
        po-ordl.s-num   EQ ?),
        FIRST oe-ord
        WHERE oe-ord.company EQ oe-ordl.company
        AND oe-ord.ord-no  EQ oe-ordl.ord-no
        AND oe-ord.frt-pay NE "P",
        FIRST oe-rel NO-LOCK
        WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line:

        IF oe-rel.carrier NE po-ord.carrier THEN
            MESSAGE "Update Freight Payment and Carrier from Customer's Order?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                UPDATE ll.

        LEAVE.
    END.

IF ll THEN 
DO :
    FIND CURRENT po-ord EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN
        po-ord.frt-pay = IF oe-ord.frt-pay EQ "T" THEN "B" ELSE oe-ord.frt-pay
        po-ord.carrier = oe-rel.carrier .
      IF trim(v-postatus-cha) = "Hold" THEN
          po-ord.stat    = "H"   .
    FIND CURRENT po-ord NO-LOCK NO-ERROR.
END.
ELSE DO:
    FIND CURRENT po-ord EXCLUSIVE-LOCK NO-ERROR.
    IF trim(v-postatus-cha) = "Hold" THEN
        ASSIGN
        po-ord.stat    = "H"   .
    FIND CURRENT po-ord NO-LOCK NO-ERROR.
END.


FOR EACH tt-job-mat:
    IF tt-job-mat.frm NE ? THEN 
    DO:
        FIND FIRST job-mat EXCLUSIVE-LOCK
            WHERE ROWID(job-mat)  EQ tt-job-mat.row-id
            AND job-mat.j-no    EQ 0
            NO-ERROR.

        ll = AVAILABLE job-mat.

        IF ll THEN RUN jc/maydeletejob-mat.p (BUFFER job-mat, OUTPUT ll).

        IF ll NE YES THEN CREATE job-mat.

        BUFFER-COPY tt-job-mat EXCEPT rec_key TO job-mat
            ASSIGN
            job-mat.blank-no = po-ordl.b-num
            job-mat.j-no     = 1
            job-mat.qty-all  = job-mat.qty.
        IF po-ordl.s-num NE ? THEN job-mat.frm = po-ordl.s-num.
        FIND CURRENT job-mat NO-LOCK NO-ERROR.
    END.
    DELETE tt-job-mat.
END.

RUN vend-cost (NO).

FOR EACH b-po-ordl EXCLUSIVE-LOCK
    WHERE b-po-ordl.company EQ po-ordl.company
    AND b-po-ordl.po-no   EQ po-ordl.po-no
    AND (b-po-ordl.line   LT 1 OR
    b-po-ordl.line   GE 99999999):
    DELETE b-po-ordl.
END.

RUN po/updordpo.p (BUFFER po-ordl).
 
RUN po/po-total.p (RECID(po-ord)).

RUN po/poordlup.p (RECID(po-ordl), 1, YES).


IF v-po-msf NE 0 THEN 
DO:
    ASSIGN 
        v-basis-w = 0
        v-dep     = 0.

    IF po-ordl.item-type THEN
        FIND FIRST ITEM NO-LOCK 
            WHERE item.company EQ g_company
            AND item.i-no    EQ po-ordl.i-no
            NO-ERROR.
    IF AVAILABLE ITEM THEN
        ASSIGN
            v-basis-w = item.basis-w
            v-dep     = item.s-dep.

    IF po-ordl.pr-qty-uom EQ "MSF" THEN
        v-qty = po-ordl.ord-qty.
    ELSE
        RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
            v-basis-w, po-ordl.s-len, po-ordl.s-wid, v-dep,
            po-ordl.ord-qty, OUTPUT v-qty).
                           
    IF v-qty GT v-po-msf THEN
        MESSAGE "Quantity ordered meets or exceeds " +
            TRIM(STRING(v-po-msf,">>>>>>>>>>")) + " MSF"
            VIEW-AS ALERT-BOX WARNING.
END.
  
IF po-ordl.item-type = NO THEN 
DO:
    FIND FIRST itemfg WHERE itemfg.company EQ po-ordl.company
        AND itemfg.i-no EQ po-ordl.i-no
        NO-LOCK NO-ERROR.
      
    /* If a purchased fg component then pull info from farm tab */
    IF AVAILABLE itemfg AND itemfg.pur-man = TRUE THEN
        RUN writeJobFarmInfo.
      
END. /* If a finished good */
APPLY "go" TO FRAME {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.cost Dialog-Frame
ON VALUE-CHANGED OF po-ordl.cost IN FRAME Dialog-Frame /* Cost */
    DO:
  FIND CURRENT po-ordl EXCLUSIVE-LOCK NO-ERROR.
  {po/podisdet.i} 
IF NOT ll-new-record THEN
    ll-cost-changed = YES.
  FIND CURRENT po-ordl NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.cust-no Dialog-Frame
ON ENTRY OF po-ordl.cust-no IN FRAME Dialog-Frame /* Customer# */
    DO:
        lv-save-cust-no = po-ordl.cust-no:SCREEN-VALUE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.cust-no Dialog-Frame
ON LEAVE OF po-ordl.cust-no IN FRAME Dialog-Frame /* Customer# */
    DO:
        DEFINE VARIABLE v-import AS LOG NO-UNDO.

        IF lv-save-cust-no NE {&self-name}:SCREEN-VALUE OR
      {&self-name}:SCREEN-VALUE NE "" THEN
   DO :

        FIND FIRST e-itemfg NO-LOCK
            WHERE e-itemfg.company EQ cocode
            AND e-itemfg.i-no    EQ po-ordl.i-no:SCREEN-VALUE
            NO-ERROR.
        IF AVAILABLE e-itemfg AND 
            po-ordl.cust-no:SCREEN-VALUE NE "" 
            THEN
            FIND FIRST e-itemfg-vend NO-LOCK
                WHERE e-itemfg-vend.company EQ e-itemfg.company
                AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                AND e-itemfg-vend.vend-no EQ po-ord.vend-no
                AND e-itemfg-vend.cust-no EQ po-ordl.cust-no:SCREEN-VALUE
                AND e-itemfg-vend.est-no eq ""  NO-ERROR.
        IF AVAILABLE e-itemfg-vend THEN 
        DO:
           IF e-itemfg-vend.vend-item NE "" THEN po-ordl.vend-i-no:SCREEN-VALUE = e-itemfg-vend.vend-item.
            MESSAGE 
                "Import Customer Cost?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                UPDATE v-import.

            IF v-import THEN RUN vend-cost (YES).
        END.
      
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.disc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.disc Dialog-Frame
ON VALUE-CHANGED OF po-ordl.disc IN FRAME Dialog-Frame /* Discount */
    DO:
  FIND CURRENT po-ordl EXCLUSIVE-LOCK NO-ERROR.
  {po/podisdet.i}
  FIND CURRENT po-ordl NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.i-no Dialog-Frame
ON LEAVE OF po-ordl.i-no IN FRAME Dialog-Frame /* Item# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN check-job-bnum . 
            RUN check-workfile.
            RUN validate-i-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            /* gdm - 06040918 */
            FIND FIRST bf-itemfg NO-LOCK 
                WHERE bf-itemfg.company EQ cocode
                AND bf-itemfg.i-no    EQ po-ordl.i-no:SCREEN-VALUE NO-ERROR.

            FIND FIRST bf-e-itemfg OF bf-itemfg NO-LOCK NO-ERROR.

            FIND FIRST bf-e-itemfg-vend OF bf-e-itemfg NO-LOCK NO-ERROR.

            /* gdm - 06040918 */     

            APPLY "leave" TO po-ordl.ord-qty IN FRAME {&FRAME-NAME}.

            IF ip-type EQ "add" AND (v-poscreen-char = "Job-Item" ) THEN 
            DO:
                IF po-ordl.s-num:SENSITIVE EQ YES  THEN
                APPLY "entry" TO po-ordl.s-num /* po-ordl.due-date*/ . 
                ELSE  APPLY "entry" TO po-ordl.due-date  . 
                RETURN NO-APPLY.
            END.

        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.i-no Dialog-Frame
ON VALUE-CHANGED OF po-ordl.i-no IN FRAME Dialog-Frame /* Item# */
    DO:
        ASSIGN
            ll-item-validated = NO
            ll-poord-warned   = NO
            ll-pojob-warned   = NO.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.item-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.item-type Dialog-Frame
ON MOUSE-SELECT-CLICK OF po-ordl.item-type IN FRAME Dialog-Frame /* Item Type */
    DO:
        {&self-name}:SCREEN-VALUE =
      STRING(NOT {&self-name}:SCREEN-VALUE EQ "RM",{&self-name}:FORMAT).

        APPLY "value-changed" TO {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.item-type Dialog-Frame
ON VALUE-CHANGED OF po-ordl.item-type IN FRAME Dialog-Frame /* Item Type */
    DO:
        ASSIGN
            ll-item-validated = NO
            ll-poord-warned   = NO
            ll-pojob-warned   = NO.

        RUN validate-i-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.job-no Dialog-Frame
ON ENTRY OF po-ordl.job-no IN FRAME Dialog-Frame /* Job # */
    DO:
        IF lv-save-job NE "" THEN lv-save-job = {&self-name}:SCREEN-VALUE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.job-no Dialog-Frame
ON LEAVE OF po-ordl.job-no IN FRAME Dialog-Frame /* Job # */
    DO:
        DEFINE BUFFER b-job-mat FOR job-mat.
        DEFINE BUFFER b-job-hdr FOR job-hdr.
        DEFINE BUFFER b-item    FOR ITEM.
        DEFINE VARIABLE v-len    AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE v-wid    AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE v-actnum AS CHARACTER NO-UNDO.

        IF LASTKEY NE -1 THEN 
        DO:
            {&self-name}:SCREEN-VALUE = FILL(" ", 6 - LENGTH(TRIM({&self-name}:SCREEN-VALUE))) +
                                TRIM({&self-name}:SCREEN-VALUE).
    
            RUN valid-job-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            ELSE 
            DO:

                FIND FIRST b-job-mat WHERE
                    b-job-mat.company EQ g_company AND
                    b-job-mat.job-no  EQ po-ordl.job-no:SCREEN-VALUE AND
                    b-job-mat.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE) AND
                    b-job-mat.rm-i-no EQ po-ordl.i-no:SCREEN-VALUE AND
                    b-job-mat.frm     EQ INT(po-ordl.s-num:SCREEN-VALUE) AND
                    b-job-mat.blank-no EQ INT(po-ordl.b-num:SCREEN-VALUE)
                    NO-LOCK NO-ERROR.

                IF AVAILABLE b-job-mat THEN 
                DO:

                    FIND FIRST b-item WHERE
                        b-item.company EQ g_company AND
                        b-item.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                        NO-LOCK NO-ERROR.

                    ASSIGN 
                        v-len                      = b-job-mat.len
                        v-wid                      = b-job-mat.wid
                        v-dep                      = b-job-mat.dep.
                    IF v-dep EQ 0 THEN 
                        v-dep                      = IF AVAILABLE b-item AND CAN-DO("C,5,6,D",b-item.mat-type) THEN b-item.case-d ELSE IF AVAILABLE b-item THEN b-item.s-dep ELSE 0
                       .
                   ASSIGN 
                        {po/calc16.i v-len}
                        {po/calc16.i v-wid}
                        {po/calc16.i v-dep}
                        po-ordl.s-wid:SCREEN-VALUE = STRING(v-wid)
                        po-ordl.s-len:SCREEN-VALUE = STRING(v-len)
                        v-po-dep:SCREEN-VALUE      = STRING(v-dep).

                    RUN sys\inc\decfrac2.p(INPUT DEC(po-ordl.s-wid:SCREEN-VALUE), INPUT 32, OUTPUT v-wid-frac).
                    RUN sys\inc\decfrac2.p(INPUT DEC(po-ordl.s-len:SCREEN-VALUE), INPUT 32, OUTPUT v-len-frac).
                    RUN sys\inc\decfrac2.p(INPUT DEC(v-po-dep:SCREEN-VALUE), INPUT 32, OUTPUT v-dep-frac).
                    ASSIGN
                        v-po-wid-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-wid-frac
                        v-po-len-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-len-frac
                        v-po-dep-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-dep-frac.

                    RELEASE b-job-mat.
                    RELEASE b-item.

                    IF po-ordl.item-type = NO THEN 
                    DO:
                        FIND FIRST itemfg WHERE itemfg.company EQ po-ordl.company
                            AND itemfg.i-no EQ po-ordl.i-no
                            NO-LOCK NO-ERROR.
                        /* If a purchased fg component then pull info from farm tab */
                        IF AVAILABLE itemfg AND itemfg.pur-man = TRUE THEN
                            RUN getJobFarmInfo.
                    END. /* If a finished good */

                END.

                /* populate GL# from job-hdr.i-no + itemfg tables, then reftable AH 02-24-10 */
                IF po-ordl.item-type:SCREEN-VALUE = "rm" THEN 
                DO:
                    ASSIGN 
                        v-actnum = "".
                    FIND FIRST b-job-hdr WHERE 
                        b-job-hdr.company EQ g_company AND
                        b-job-hdr.job-no  EQ po-ordl.job-no:SCREEN-VALUE AND
                        b-job-hdr.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)  AND
                        b-job-hdr.frm     EQ INT(po-ordl.s-num:SCREEN-VALUE) NO-LOCK NO-ERROR.
                    IF AVAILABLE b-job-hdr THEN
                        RUN get-itemfg-gl (INPUT b-job-hdr.company, b-job-hdr.i-no, OUTPUT v-actnum).
                    IF v-actnum <> "" THEN 
                    DO:
                        ASSIGN 
                            po-ordl.actnum:SCREEN-VALUE = v-actnum.
                        FIND FIRST account NO-LOCK
                            WHERE account.company EQ g_company
                            AND account.actnum   EQ v-actnum NO-ERROR.
                        v-gl-desc:SCREEN-VALUE = IF AVAILABLE account THEN account.dscr ELSE ''.
                    END.
        
                    RELEASE b-job-hdr.
                END.
            END. /* else do */

        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.job-no Dialog-Frame
ON VALUE-CHANGED OF po-ordl.job-no IN FRAME Dialog-Frame /* Job # */
    DO:
        ASSIGN
            ll-poord-warned = NO
            ll-pojob-warned = NO.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.job-no2 Dialog-Frame
ON ENTRY OF po-ordl.job-no2 IN FRAME Dialog-Frame /* Run # */
    DO:
        IF lv-save-job2 NE "" THEN lv-save-job2 = {&self-name}:SCREEN-VALUE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.job-no2 Dialog-Frame
ON LEAVE OF po-ordl.job-no2 IN FRAME Dialog-Frame /* Run # */
    DO:
        DEFINE BUFFER b-job-hdr FOR job-hdr.
  
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-job-no2 NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            ELSE 
            DO:

                /* populate GL# from job-hdr.i-no + itemfg tables, then reftable AH 02-24-10 */
                IF po-ordl.item-type:SCREEN-VALUE = "rm" THEN 
                DO:
                    ASSIGN 
                        v-actnum = "".
                    FIND FIRST b-job-hdr WHERE 
                        b-job-hdr.company EQ g_company AND
                        b-job-hdr.job-no  EQ po-ordl.job-no:SCREEN-VALUE AND
                        b-job-hdr.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)  AND
                        b-job-hdr.frm     EQ INT(po-ordl.s-num:SCREEN-VALUE) NO-LOCK NO-ERROR.
                    IF AVAILABLE b-job-hdr THEN
                        RUN get-itemfg-gl (INPUT b-job-hdr.company, b-job-hdr.i-no, OUTPUT v-actnum).
                    IF v-actnum <> "" THEN 
                    DO:
                        ASSIGN 
                            po-ordl.actnum:SCREEN-VALUE = v-actnum.
                        FIND FIRST account NO-LOCK
                            WHERE account.company EQ g_company
                            AND account.actnum   EQ v-actnum NO-ERROR.
                        v-gl-desc:SCREEN-VALUE = IF AVAILABLE account THEN account.dscr ELSE ''.
                    END.
        
                    RELEASE b-job-hdr.
                END.
                ELSE 
                DO: 
    
                    FIND FIRST itemfg WHERE itemfg.company EQ po-ordl.company
                        AND itemfg.i-no EQ po-ordl.i-no:SCREEN-VALUE
                        NO-LOCK NO-ERROR.


                    /* If a purchased fg component then pull info from farm tab */
                    IF AVAILABLE itemfg AND itemfg.pur-man = TRUE THEN
                        RUN getJobFarmInfo.
                END. /* If a finished good */
            END.
            /* APPLY "LEAVE" TO po-ordl.s-num.*/ /* ticket 13022 */
    
            IF ip-type EQ "add" AND ( v-poscreen-char = "Job-Item") THEN 
            DO:
                APPLY "entry" TO po-ordl.i-no.
                RETURN NO-APPLY.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.job-no2 Dialog-Frame
ON VALUE-CHANGED OF po-ordl.job-no2 IN FRAME Dialog-Frame /* Run # */
    DO:
        ASSIGN
            ll-poord-warned = NO
            ll-pojob-warned = NO.
      /*  ASSIGN 
            po-ordl.b-num:SCREEN-VALUE = ? . */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.ord-no Dialog-Frame
ON ENTRY OF po-ordl.ord-no IN FRAME Dialog-Frame /* Order Number */
    DO:
        lv-save-ord-no = INT(po-ordl.ord-no:SCREEN-VALUE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.ord-no Dialog-Frame
ON LEAVE OF po-ordl.ord-no IN FRAME Dialog-Frame /* Order Number */
    DO:
        DEFINE VARIABLE v-import AS LOG NO-UNDO.

        IF LASTKEY NE -1 THEN 
        DO:
            IF lv-save-ord-no NE INT(po-ordl.ord-no:SCREEN-VALUE) THEN
                ll-order-warned = NO.

            RUN valid-ord-no NO-ERROR.

            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            IF lv-save-ord-no NE INT(po-ordl.ord-no:SCREEN-VALUE) AND
                INT(po-ordl.ord-no:SCREEN-VALUE) NE 0 AND 
                po-ordl.item-type:SCREEN-VALUE EQ "FG" THEN
            DO:
                /* gdm - 02020908 */
                FIND FIRST item NO-LOCK
                    WHERE item.company EQ po-ordl.company
                    AND item.i-no EQ po-ordl.i-no NO-ERROR.
                IF AVAILABLE item THEN 
                    FIND FIRST e-itemfg OF ASI.itemfg NO-LOCK NO-ERROR.
                IF AVAILABLE e-itemfg THEN 
                    FIND FIRST e-itemfg-vend NO-LOCK
                        WHERE e-itemfg-vend.company EQ e-itemfg.company
                        AND e-itemfg-vend.cust-no EQ po-ordl.cust-no:SCREEN-VALUE
                        AND e-itemfg-vend.i-no    EQ e-itemfg.i-no 
                        AND e-itemfg-vend.est-no  EQ "" NO-ERROR.    
                IF AVAILABLE e-itemfg-vend THEN 
                DO:
                
                    MESSAGE 
                        "Import Customer Cost?"
                        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                        UPDATE v-import.

                    IF v-import THEN RUN vend-cost(YES).
                END. /* if avail */
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.ord-no Dialog-Frame
ON VALUE-CHANGED OF po-ordl.ord-no IN FRAME Dialog-Frame /* Order Number */
    DO:
        ll-poord-warned = NO.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.ord-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.ord-qty Dialog-Frame
ON ENTRY OF po-ordl.ord-qty IN FRAME Dialog-Frame /* Quantity */
    DO:
        lv-save-ord-qty = {&self-name}:SCREEN-VALUE.
        lv-ord-qty-entered = TRUE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.ord-qty Dialog-Frame
ON LEAVE OF po-ordl.ord-qty IN FRAME Dialog-Frame /* Quantity */
    DO:
        DEFINE VARIABLE v-import       AS LOG NO-UNDO.
        DEFINE VARIABLE v-was-imported AS LOG NO-UNDO.

        IF DEC({&self-name}:SCREEN-VALUE) NE DEC(lv-save-ord-qty) THEN DO:
     FIND CURRENT po-ordl EXCLUSIVE-LOCK NO-ERROR.
     {po/podisdet.i}
     FIND CURRENT po-ordl NO-LOCK NO-ERROR.


IF po-ordl.item-type:SCREEN-VALUE EQ "RM" THEN 
DO:
    FIND FIRST e-item NO-LOCK
        WHERE e-item.company EQ cocode
        AND e-item.i-no    EQ po-ordl.i-no:SCREEN-VALUE
        NO-ERROR.
      
    IF AVAILABLE e-item THEN 
    DO:
        CREATE tt-ei.
        ASSIGN 
            tt-ei.std-uom = e-item.std-uom.
      
        FIND FIRST e-item-vend NO-LOCK
            WHERE e-item-vend.company EQ e-item.company
            AND e-item-vend.i-no    EQ e-item.i-no
            AND e-item-vend.vend-no EQ po-ord.vend-no
            NO-ERROR.
      
    END.
END.
ELSE 
DO:
    

    FIND FIRST e-itemfg NO-LOCK
        WHERE e-itemfg.company EQ cocode
        AND e-itemfg.i-no    EQ po-ordl.i-no:SCREEN-VALUE
        NO-ERROR.

    IF AVAILABLE e-itemfg THEN 
    DO:


        IF po-ordl.cust-no:SCREEN-VALUE NE "" THEN
            FIND FIRST e-itemfg-vend NO-LOCK
                WHERE e-itemfg-vend.company EQ e-itemfg.company
                AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                AND e-itemfg-vend.vend-no EQ po-ord.vend-no
                AND e-itemfg-vend.cust-no EQ po-ordl.cust-no:SCREEN-VALUE
                AND e-itemfg-vend.est-no eq "" 
                NO-ERROR.
        IF po-ord.cust-no NE "" AND NOT AVAILABLE e-itemfg-vend THEN
            FIND FIRST e-itemfg-vend NO-LOCK
                WHERE e-itemfg-vend.company EQ e-itemfg.company
                AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                AND e-itemfg-vend.vend-no EQ po-ord.vend-no
                AND e-itemfg-vend.cust-no EQ po-ord.cust-no
                AND e-itemfg-vend.est-no eq "" 
                NO-ERROR.

        /* gdm - 06040918 - check for vendor */
        IF NOT AVAILABLE e-itemfg-vend THEN
            FIND FIRST e-itemfg-vend NO-LOCK
                WHERE e-itemfg-vend.company EQ e-itemfg.company
                AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                AND e-itemfg-vend.vend-no EQ po-ord.vend-no
                AND  e-itemfg-vend.est-no eq ""
                NO-ERROR.

        /* gdm - check for blank vendor */
        IF NOT AVAILABLE e-itemfg-vend THEN
            FIND FIRST e-itemfg-vend NO-LOCK
                WHERE e-itemfg-vend.company EQ e-itemfg.company
                AND e-itemfg-vend.i-no    EQ e-itemfg.i-no 
                AND e-itemfg-vend.vend-no EQ ""
                AND e-itemfg-vend.est-no eq "" NO-ERROR.


    END.
    
END.

IF ip-type NE "Update" THEN
    v-import = DEC(lv-save-ord-qty) NE DEC({&self-name}:SCREEN-VALUE).
ELSE IF AVAILABLE e-itemfg-vend OR AVAILABLE e-item-vend THEN 
    DO:

        MESSAGE 
            "Import Cost?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE v-import.
        v-was-imported = TRUE.
        RUN vend-cost (v-import).

    END.
END.

/* If a new record, let it default from the correct calculation */
IF ll-new-record 
    /*AND asi.po-ordl.job-no:SCREEN-VALUE = ""  */
    AND v-was-imported = NO
    AND ll-cost-changed = FALSE 
    AND lv-ord-qty-entered THEN 
DO:
    MESSAGE 
        "Import Cost?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE v-import.
    v-was-imported = TRUE.
    RUN vend-cost (v-import).
END.
lv-ord-qty-entered = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.pr-qty-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.pr-qty-uom Dialog-Frame
ON ENTRY OF po-ordl.pr-qty-uom IN FRAME Dialog-Frame /* Purchase Quantity Uom */
    DO:
        lv-save-fld = {&self-name}:SCREEN-VALUE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.pr-qty-uom Dialog-Frame
ON LEAVE OF po-ordl.pr-qty-uom IN FRAME Dialog-Frame /* Purchase Quantity Uom */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-uom (FOCUS:NAME) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
        IF TRIM(lv-save-fld) NE TRIM({&self-name}:SCREEN-VALUE) THEN DO:
    FIND CURRENT po-ordl EXCLUSIVE-LOCK NO-ERROR.
    {po/podisdet.i}
    FIND CURRENT po-ordl NO-LOCK NO-ERROR.
RUN vend-cost (NO).
END.
/* if a new record and uom changed, get correct cost */
IF ll-new-record 
    AND asi.po-ordl.job-no:SCREEN-VALUE = "" 
    AND ll-cost-changed = FALSE THEN
    RUN vend-cost (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.pr-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.pr-uom Dialog-Frame
ON ENTRY OF po-ordl.pr-uom IN FRAME Dialog-Frame /* Purchased UOM */
    DO:
        lv-save-fld = {&self-name}:SCREEN-VALUE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.pr-uom Dialog-Frame
ON LEAVE OF po-ordl.pr-uom IN FRAME Dialog-Frame /* Purchased UOM */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-uom (FOCUS:NAME) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
        IF TRIM(lv-save-fld) NE TRIM({&self-name}:SCREEN-VALUE) THEN DO:
            FIND CURRENT po-ordl EXCLUSIVE-LOCK NO-ERROR.
        {po/podisdet.i}
                FIND CURRENT po-ordl NO-LOCK NO-ERROR.
            /*RUN vend-cost (NOT ll-cost-changed).*/
                 
            RUN vend-cost (NO).
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.s-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.s-len Dialog-Frame
ON LEAVE OF po-ordl.s-len IN FRAME Dialog-Frame /* Sheet Len */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
  
            RUN sys\inc\decfrac2.p(INPUT DEC(po-ordl.s-len:SCREEN-VALUE), INPUT 32, OUTPUT v-len-frac).
            v-po-len-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-len-frac.

        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.s-len Dialog-Frame
ON VALUE-CHANGED OF po-ordl.s-len IN FRAME Dialog-Frame /* Sheet Len */
    DO:
        IF DEC({&self-name}:SCREEN-VALUE) - TRUNC(DEC({&self-name}:SCREEN-VALUE),0) GE factor# THEN DO:
        MESSAGE "Decimal must be less than " + STRING(factor#,">.9<") + "..."
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
  FIND CURRENT po-ordl EXCLUSIVE-LOCK NO-ERROR.
  {po/podisdet.i}
  FIND CURRENT po-ordl NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.s-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.s-num Dialog-Frame
ON ENTRY OF po-ordl.s-num IN FRAME Dialog-Frame /* S */
    DO:
        IF lv-save-s-num NE "" THEN lv-save-s-num = {&self-name}:SCREEN-VALUE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.s-num Dialog-Frame
ON LEAVE OF po-ordl.s-num IN FRAME Dialog-Frame /* S */
    DO:
        DEFINE BUFFER b-job-mat FOR job-mat.
        DEFINE BUFFER b-job-hdr FOR job-hdr.
  
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-num NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            ELSE 
            DO:

                /* populate GL# from job-hdr.i-no + itemfg tables, then reftable AH 02-24-10 */
                IF po-ordl.item-type:SCREEN-VALUE = "rm" THEN 
                DO:
                    ASSIGN 
                        v-actnum = "".
                    FIND FIRST b-job-hdr NO-LOCK WHERE 
                        b-job-hdr.company EQ g_company AND
                        b-job-hdr.job-no  EQ po-ordl.job-no:SCREEN-VALUE AND
                        b-job-hdr.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)  AND
                        b-job-hdr.frm     EQ INT(po-ordl.s-num:SCREEN-VALUE) NO-ERROR.
                    IF AVAILABLE b-job-hdr THEN
                        RUN get-itemfg-gl (INPUT b-job-hdr.company, b-job-hdr.i-no, OUTPUT v-actnum).
                    IF v-actnum <> "" THEN 
                    DO:
                        ASSIGN 
                            po-ordl.actnum:SCREEN-VALUE = v-actnum.
                        FIND FIRST account NO-LOCK
                            WHERE account.company EQ g_company
                            AND account.actnum   EQ v-actnum NO-ERROR.
                        v-gl-desc:SCREEN-VALUE = IF AVAILABLE account THEN account.dscr ELSE ''.
                    END.
        
                    RELEASE b-job-hdr.
                END.
                ELSE 
                DO:
                    FIND FIRST itemfg WHERE itemfg.company EQ g_company
                        AND itemfg.i-no EQ po-ordl.i-no:SCREEN-VALUE
                        NO-LOCK NO-ERROR.
                    /* If a purchased fg component then pull info from farm tab */
                    IF AVAILABLE itemfg AND itemfg.pur-man = TRUE THEN
                        RUN getJobFarmInfo.
                END. /* If a finished good */
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.s-num Dialog-Frame
ON VALUE-CHANGED OF po-ordl.s-num IN FRAME Dialog-Frame /* S */
    DO:
        ASSIGN
            ll-poord-warned = NO
            ll-pojob-warned = NO.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.s-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.s-wid Dialog-Frame
ON LEAVE OF po-ordl.s-wid IN FRAME Dialog-Frame /* Sheet Wid */
    DO:
    
        DO WITH FRAME {&FRAME-NAME}:
  
            RUN sys\inc\decfrac2.p(INPUT DEC(po-ordl.s-wid:SCREEN-VALUE), INPUT 32, OUTPUT v-wid-frac).
            v-po-wid-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-wid-frac.


        END.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.s-wid Dialog-Frame
ON VALUE-CHANGED OF po-ordl.s-wid IN FRAME Dialog-Frame /* Sheet Wid */
    DO:
        IF DEC({&self-name}:SCREEN-VALUE) - TRUNC(DEC({&self-name}:SCREEN-VALUE),0) GE factor# THEN DO:
        MESSAGE "Decimal must be less than " + STRING(factor#,">.9<") + "..."
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
  FIND CURRENT po-ordl EXCLUSIVE-LOCK NO-ERROR.
  {po/podisdet.i}
  FIND CURRENT po-ordl NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.setup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.setup Dialog-Frame
ON ENTRY OF po-ordl.setup IN FRAME Dialog-Frame /* Setup */
    DO:
        ld-prev-setup = DEC({&self-name}:SCREEN-VALUE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.setup Dialog-Frame
ON VALUE-CHANGED OF po-ordl.setup IN FRAME Dialog-Frame /* Setup */
    DO:
        RUN new-setup.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ordl.t-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.t-cost Dialog-Frame
ON ENTRY OF po-ordl.t-cost IN FRAME Dialog-Frame /* Total Cost */
    DO:
        ASSIGN
            ld-prev-t-cost    = DEC(po-ordl.t-cost:SCREEN-VALUE)
            ld-prev-cost      = DEC(po-ordl.cost:SCREEN-VALUE)
            ld-prev-cons-cost = DEC(po-ordl.cons-cost:SCREEN-VALUE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.t-cost Dialog-Frame
ON VALUE-CHANGED OF po-ordl.t-cost IN FRAME Dialog-Frame /* Total Cost */
    DO:
        DEFINE VARIABLE ld AS DECIMAL NO-UNDO.

        ASSIGN
            ld                             = DEC(po-ordl.t-cost:SCREEN-VALUE) / ld-prev-t-cost
            po-ordl.cost:SCREEN-VALUE      = STRING(ld-prev-cost * ld)
            po-ordl.cons-cost:SCREEN-VALUE = STRING(ld-prev-cons-cost * ld).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-po-dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-po-dep Dialog-Frame
ON LEAVE OF v-po-dep IN FRAME Dialog-Frame
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            IF DEC({&self-name}:SCREEN-VALUE) - TRUNC(DEC({&self-name}:SCREEN-VALUE),0) GE factor# THEN 
            DO:
                MESSAGE "Decimal must be less than " + STRING(factor#,">.9<") + "..."
                    VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
   FIND CURRENT po-ordl EXCLUSIVE-LOCK NO-ERROR.
    {po/podisdet.i}
    FIND CURRENT po-ordl NO-LOCK NO-ERROR.
   
RUN sys\inc\decfrac2.p(INPUT DEC(v-po-dep:SCREEN-VALUE), INPUT 32, OUTPUT v-dep-frac).
v-po-dep-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-dep-frac.
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

    FIND FIRST rm-ctrl WHERE rm-ctrl.company EQ cocode NO-LOCK.
    FIND FIRST fg-ctrl WHERE fg-ctrl.company EQ cocode NO-LOCK.

    RUN po/po-sysct.p.
    {sys/ref/pocost.i}
    ASSIGN
        v-pocost1  = v-pocost
        v-hold-op1 = v-hold-op.

    IF ip-type EQ "copy" THEN lv-item-recid = ip-recid.

    IF ip-recid = ? THEN 
    DO:
        RUN create-item.

        FIND po-ordl NO-LOCK WHERE RECID(po-ordl) = lv-item-recid NO-ERROR.
        ll-poord-warned = NO.
    END.
    ELSE FIND po-ordl WHERE RECID(po-ordl) = ip-recid NO-LOCK NO-ERROR.
    FIND po-ord NO-LOCK WHERE
        po-ord.company EQ po-ordl.company AND
        po-ord.po-no   EQ po-ordl.po-no
        .

    IF ip-type <> "view" THEN 
    DO: 
        RUN enable_UI.
        RUN display-item.
        RUN enable-disable-frm.
        RUN enable-disable-blk.
        RUN enable-disable-size.
        ASSIGN
            ll-item-validated                      = ip-recid NE ?
            ll-order-warned                        = NO
            btn_done:hidden IN FRAME {&frame-name} = YES.
    END.
    ELSE 
    DO:
        RUN display-item.
        ASSIGN 
            btn_done:hidden IN FRAME {&frame-name} = NO
            btn_done:sensitive                     = YES
            btn_ok:hidden                          = YES
            btn_cancel:hidden                      = YES.
    END.

    DO WITH FRAME {&FRAME-NAME}:
        IF ip-type EQ "update" THEN DISABLE po-ordl.item-type po-ordl.i-no.

        IF ip-type EQ "add" AND (v-poscreen-char = "Job-Item" ) THEN
            APPLY "entry" TO po-ordl.job-no .

        FIND FIRST account NO-LOCK 
            WHERE account.company EQ cocode
            AND account.actnum  EQ po-ordl.actnum:SCREEN-VALUE
            NO-ERROR.
        v-gl-desc:SCREEN-VALUE = IF AVAILABLE account THEN account.dscr ELSE ''. 

        scr-cons-uom:SCREEN-VALUE = po-ordl.cons-uom:SCREEN-VALUE.
    END.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adder-text Dialog-Frame 
PROCEDURE adder-text :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    IF addersText NE '' THEN 
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            adders:SCREEN-VALUE = 'Adder Charges     '
                         + FILL(" ",12 - LENGTH("Cost/" + po-ordl.pr-uom:SCREEN-VALUE))
                         + "Cost/" + po-ordl.pr-uom:SCREEN-VALUE
                         + '   SU' 
                         + CHR(10) + '==================================='
                         + CHR(10) + addersText
            adders:HIDDEN       = addersText EQ ''
            adders:SENSITIVE    = addersText NE ''.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE change-page-logic Dialog-Frame 
PROCEDURE change-page-logic :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /* Give focus to something in frame so that ctrl-o will be caught */
    /* This procedure called from change-page in w-order */
    
    APPLY 'entry' TO btnCalendar-1 IN FRAME Dialog-Frame.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-for-multi Dialog-Frame 
PROCEDURE check-for-multi :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-job-mat   FOR job-mat.
    DEFINE BUFFER b-w-po-ordl FOR w-po-ordl.
    DEFINE VARIABLE v-cnt    AS INTEGER.

    DEFINE VARIABLE ll-multi AS LOG     INIT YES NO-UNDO.


    ll-multi = AVAILABLE w-po-ordl.

    IF ll-multi THEN
        FIND FIRST job-mat WHERE ROWID(job-mat) EQ w-po-ordl.job-mat-rowid
            NO-LOCK NO-ERROR.

    ll-multi = AVAILABLE job-mat AND
        CAN-FIND(FIRST b-job-mat
        WHERE b-job-mat.company EQ job-mat.company
        AND b-job-mat.job     EQ job-mat.job
        AND b-job-mat.job-no  EQ job-mat.job-no
        AND b-job-mat.job-no2 EQ job-mat.job-no2
        AND b-job-mat.rm-i-no EQ job-mat.rm-i-no
        AND ROWID(b-job-mat)  NE ROWID(job-mat)).

    IF ll-multi THEN 
    DO WITH FRAME {&FRAME-NAME}:

        ASSIGN
            po-ordl.job-no:SCREEN-VALUE  = w-po-ordl.job-no
            po-ordl.job-no2:SCREEN-VALUE = STRING(w-po-ordl.job-no2)
            po-ordl.i-no:SCREEN-VALUE    = w-po-ordl.i-no
            po-ordl.s-num:SCREEN-VALUE   = "?"
            po-ordl.b-num:SCREEN-VALUE   = STRING(w-po-ordl.b-num).

        RUN create-multi-line.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-workfile Dialog-Frame 
PROCEDURE check-workfile :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-w-po-ordl FOR w-po-ordl.

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST w-po-ordl NO-ERROR.
    
        IF AVAILABLE w-po-ordl AND v-number-rows-selected > 1 THEN 
        DO:
            RUN check-for-multi.

            ASSIGN
                po-ordl.item-type:SCREEN-VALUE = "RM"
                po-ordl.i-no:SCREEN-VALUE      = w-po-ordl.i-no.
        
            FIND FIRST ITEM NO-LOCK 
                WHERE item.company EQ po-ordl.company
                AND item.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                NO-ERROR.
            IF AVAILABLE item THEN RUN display-rmitem (RECID(item)).

            ASSIGN
                po-ordl.job-no:SCREEN-VALUE  = w-po-ordl.job-no
                po-ordl.job-no2:SCREEN-VALUE = STRING(w-po-ordl.job-no2)
                po-ordl.s-num:SCREEN-VALUE   = STRING(w-po-ordl.s-num)
                po-ordl.b-num:SCREEN-VALUE   = STRING(w-po-ordl.b-num).

            FIND FIRST tt-job-mat NO-ERROR.
            IF AVAILABLE tt-job-mat THEN
                po-ordl.s-num:SCREEN-VALUE = STRING(tt-job-mat.frm).

            RUN display-job-mat.

            FIND FIRST w-po-ordl NO-ERROR.

            IF AVAILABLE w-po-ordl AND po-ordl.s-num:SCREEN-VALUE EQ "?" THEN 
            DO:
                w-po-ordl.s-num = ?.
                FOR EACH b-w-po-ordl
                    WHERE b-w-po-ordl.job-no  EQ w-po-ordl.job-no
                    AND b-w-po-ordl.job-no2 EQ w-po-ordl.job-no2
                    AND b-w-po-ordl.i-no    EQ w-po-ordl.i-no
                    AND ROWID(b-w-po-ordl)  NE ROWID(w-po-ordl):
                    DELETE b-w-po-ordl.
                END.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-workfile-2 Dialog-Frame 
PROCEDURE check-workfile-2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-w-po-ordl FOR w-po-ordl.

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST w-po-ordl NO-ERROR.
    
        IF AVAILABLE w-po-ordl AND v-number-rows-selected2 > 1 THEN 
        DO:
            RUN check-for-multi.

            ASSIGN
                po-ordl.item-type:SCREEN-VALUE = "RM"
                po-ordl.i-no:SCREEN-VALUE      = w-po-ordl.i-no.
        
            FIND FIRST ITEM NO-LOCK 
                WHERE item.company EQ po-ordl.company
                AND item.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                NO-ERROR.
            IF AVAILABLE item THEN RUN display-rmitem (RECID(item)).

            ASSIGN
                po-ordl.job-no:SCREEN-VALUE  = w-po-ordl.job-no
                po-ordl.job-no2:SCREEN-VALUE = STRING(w-po-ordl.job-no2)
                po-ordl.s-num:SCREEN-VALUE   = STRING(w-po-ordl.s-num)
                po-ordl.b-num:SCREEN-VALUE   = STRING(w-po-ordl.b-num).

            FIND FIRST tt-job-mat NO-ERROR.
            IF AVAILABLE tt-job-mat THEN
                po-ordl.s-num:SCREEN-VALUE = STRING(tt-job-mat.frm).

            RUN display-job-mat.

            FIND FIRST w-po-ordl NO-ERROR.

            IF AVAILABLE w-po-ordl AND po-ordl.s-num:SCREEN-VALUE EQ "?" THEN 
            DO:
                w-po-ordl.s-num = ?.
                FOR EACH b-w-po-ordl
                    WHERE b-w-po-ordl.job-no  EQ w-po-ordl.job-no
                    AND b-w-po-ordl.job-no2 EQ w-po-ordl.job-no2
                    AND b-w-po-ordl.i-no    EQ w-po-ordl.i-no
                    AND ROWID(b-w-po-ordl)  NE ROWID(w-po-ordl):
                    DELETE b-w-po-ordl.
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convertCSCost Dialog-Frame 
PROCEDURE convertCSCost :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:

        /* Convert from CS to EA */
        IF po-ordl.pr-qty-uom:SCREEN-VALUE EQ "CS" THEN 
        DO:

            /* Convert from CS to EA */
            v-ord-qty = v-ord-qty * INT(fiCount:SCREEN-VALUE).

            RUN sys/ref/convquom.p("EA",
                po-ordl.pr-uom:SCREEN-VALUE,
                v-basis-w, v-len, v-wid, v-dep,
                v-ord-qty, OUTPUT v-ord-qty).
        END.
        ELSE IF po-ordl.pr-uom:SCREEN-VALUE EQ "CS" THEN 
            DO:
                /* Convert from whatever it was to EA */
                RUN sys/ref/convquom.p(po-ordl.pr-qty-uom:SCREEN-VALUE,
                    "EA",
                    v-basis-w, v-len, v-wid, v-dep,
                    v-ord-qty, OUTPUT v-ord-qty).

                /* convert from EA to CS */
                v-ord-qty = v-ord-qty / INT(fiCount:SCREEN-VALUE).

            END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item Dialog-Frame 
PROCEDURE create-item :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    FIND FIRST po-ord NO-LOCK
        WHERE po-ord.company = g_company 
        AND po-ord.po-no = ip-ord-no
        NO-ERROR.

    IF AVAILABLE po-ord THEN 
    DO WITH FRAME {&FRAME-NAME}:
    
        FIND LAST po-ordl NO-LOCK WHERE
            po-ordl.company EQ po-ord.company AND
            po-ordl.po-no EQ po-ord.po-no
            NO-ERROR.

        z = IF AVAILABLE po-ordl THEN po-ordl.line + 1 ELSE 1.
   
        CREATE po-ordl.
        ASSIGN 
            lv-item-recid = RECID(po-ordl)
            ll-new-record = YES.
        FIND FIRST vend WHERE vend.company = po-ord.company AND vend.vend-no = po-ord.vend-no
            NO-LOCK NO-ERROR.

        ASSIGN
            po-ordl.company   = cocode
            po-ordl.po-no     = po-ord.po-no
            po-ordl.stat      = "O"
            po-ordl.ord-qty   = 1
            po-ordl.cons-qty  = 1
            po-ordl.line      = z
            po-ordl.due-date  = po-ord.due-date
            po-ordl.over-pct  = po-ord.over-pct
            po-ordl.under-pct = po-ord.under-pct
            po-ordl.vend-no   = po-ord.vend-no.

        IF AVAILABLE bf-itemfg 
            THEN
            ASSIGN
                po-ordl.pr-qty-uom = IF pouom-chr EQ "Purchase" 
                                 THEN bf-itemfg.pur-uom
                                 ELSE bf-itemfg.prod-uom
                po-ordl.pr-uom     = IF AVAILABLE bf-e-itemfg-vend
                                 THEN bf-e-itemfg-vend.std-uom
                                 ELSE bf-itemfg.prod-uom.

        IF AVAILABLE vend THEN 
        DO:
            ASSIGN
                po-ordl.disc = vend.disc-%
                po-ordl.tax  = vend.tax-gr NE "" AND aptax-chr EQ "Vendor".
     
            IF v-default-gl-log AND INDEX(v-default-gl-cha,"Vend") GT 0 THEN 
                ASSIGN po-ordl.actnum = vend.actnum.
       
        END.

        IF po-ord.printed OR po-ord.stat NE "N" THEN po-ordl.stat = "A".
        FIND CURRENT po-ordl NO-LOCK NO-ERROR.
    END. /* avail po-ord */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-multi-line Dialog-Frame 
PROCEDURE create-multi-line :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-job-mat  FOR job-mat.
    DEFINE BUFFER b-ref1     FOR reftable.
    DEFINE BUFFER b-ref2     FOR reftable.
    DEFINE BUFFER b-b-ref1   FOR reftable.
    DEFINE BUFFER b-b-ref2   FOR reftable.
    DEFINE BUFFER b2-job-mat FOR job-mat.
    DEFINE BUFFER b-item     FOR ITEM.
    DEFINE BUFFER b-job-hdr  FOR job-hdr.

    DEFINE VARIABLE ll-multi    AS LOG       INIT YES NO-UNDO.
    DEFINE VARIABLE li          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-corr      AS LOG       NO-UNDO.
    DEFINE VARIABLE v-wid       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-len       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-valid     AS LOG       INIT YES NO-UNDO.
    DEFINE VARIABLE v-lscore    AS DECIMAL   EXTENT 30 NO-UNDO.
    DEFINE VARIABLE v-wscore    AS DECIMAL   EXTENT 30 NO-UNDO.
    DEFINE VARIABLE lv-mat-type AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-rm-i-no  AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE tt-job-mat.

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST job
            WHERE job.company EQ po-ordl.company
            AND job.job-no  EQ po-ordl.job-no:SCREEN-VALUE
            AND job.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
            NO-LOCK NO-ERROR.

        /*? for sheet num entered on screen instead of F1*/
        /*can only combine same size boards*/

        IF po-ordl.s-num:SCREEN-VALUE EQ "?" AND
            po-ordl.s-num:SCREEN-VALUE NE STRING(po-ordl.s-num) AND
            NOT(po-ordl.s-num:screen-value  EQ "?" AND po-ordl.s-num EQ ?) AND
            AVAILABLE job AND NOT CAN-FIND(FIRST w-po-ordl) THEN
        DO:
    
            FOR EACH xreport WHERE xreport.term-id EQ v-term + USERID("nosweat"):
                DELETE xreport.
            END.

            FIND FIRST b-item WHERE
                b-item.company EQ g_company AND
                b-item.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                NO-LOCK NO-ERROR.

            IF AVAILABLE b-item THEN
                lv-mat-type = b-item.mat-type.

            lv-rm-i-no = po-ordl.i-no:SCREEN-VALUE.

            FOR EACH b2-job-mat
                WHERE b2-job-mat.company   EQ job.company
                AND b2-job-mat.job       EQ job.job
                AND b2-job-mat.job-no    EQ job.job-no
                AND b2-job-mat.job-no2   EQ job.job-no2
                AND b2-job-mat.rec_key   NE g_company
                AND CAN-FIND(FIRST item
                WHERE item.company   EQ b2-job-mat.company
                AND item.i-no      EQ b2-job-mat.rm-i-no
                AND (item.mat-type EQ lv-mat-type OR
                lv-mat-type   EQ ""))
                USE-INDEX job NO-LOCK:
        
                CREATE xreport.
                ASSIGN
                    xreport.term-id = v-term + USERID("nosweat")
                    xreport.rec_key = STRING(ROWID(b2-job-mat)).
            END.

            FIND FIRST b-job-hdr WHERE
                b-job-hdr.company EQ job.company AND
                b-job-hdr.job-no EQ job.job-no AND
                b-job-hdr.job-no2 EQ job.job-no2
                NO-LOCK NO-ERROR.
       
            IF AVAILABLE b-job-hdr THEN
            DO:
                FIND FIRST est WHERE
                    est.company EQ b-job-hdr.company AND
                    est.est-no EQ b-job-hdr.est-no
                    NO-LOCK NO-ERROR.
       
                IF AVAILABLE est AND est.est-type GT 4 THEN
                    v-corr = YES.
            END.
      
            FOR FIRST b2-job-mat WHERE
                b2-job-mat.company = job.company AND
                b2-job-mat.job = job.job AND
                b2-job-mat.job-no = job.job-no AND
                b2-job-mat.job-no2 = job.job-no2
                NO-LOCK,
                FIRST b-item WHERE
                b-item.company EQ b2-job-mat.company AND
                b-item.i-no = b2-job-mat.rm-i-no
                NO-LOCK,
                FIRST report WHERE
                report.rec_key = string(ROWID(b2-job-mat)) AND
                report.term-id EQ v-term + USERID("nosweat")
                NO-LOCK:
      
                li = li + 1.
      
                IF li = 1 THEN
                DO:
                    ASSIGN
                        v-wid = b2-job-mat.wid
                        v-len = b2-job-mat.len.
      
                    IF v-corr THEN
                    DO:
                        FOR EACH eb WHERE
                            eb.company EQ b2-job-mat.company AND
                            eb.est-no EQ est.est-no AND
                            eb.form-no EQ b2-job-mat.frm
                            NO-LOCK,
                            FIRST ef WHERE
                            ef.company EQ b2-job-mat.company AND
                            ef.est-no EQ est.est-no AND
                            ef.form-no EQ eb.form-no AND
                            ef.board = b2-job-mat.rm-i-no
                            NO-LOCK:
                 
                            LEAVE.
                        END.
                 
                        IF AVAILABLE eb THEN
                        DO:
                            DO i = 1 TO 30:
                                v-wscore[i] = eb.k-wid-array2[i].
                            END.
                 
                            DO i = 1 TO 30:
                                v-lscore[i] = eb.k-len-array2[i].
                            END.
                 
                            RELEASE eb.
                        END.
                    END.
                END.
      
                ELSE
                DO:
                    IF b2-job-mat.wid NE v-wid OR
                        b2-job-mat.len NE v-len THEN
                    DO:
                        v-valid = NO.
                        LEAVE.
                    END.
      
                    IF v-valid AND v-corr THEN
                    DO:
                        FOR EACH eb NO-LOCK WHERE
                            eb.company EQ b2-job-mat.company AND
                            eb.est-no EQ est.est-no AND
                            eb.form-no EQ b2-job-mat.frm
                            ,
                            FIRST ef NO-LOCK WHERE
                            ef.company EQ b2-job-mat.company AND
                            ef.est-no EQ est.est-no AND
                            ef.form-no EQ eb.form-no AND
                            ef.board = b2-job-mat.rm-i-no
                            :
                            LEAVE.
                        END.
      
                        IF AVAILABLE eb THEN
                        DO:
                            DO i = 1 TO 30:
      
                                IF v-wscore[i] NE eb.k-wid-array2[i] THEN
                                DO:
                                    v-valid = NO.
                                    RELEASE eb.
                                    LEAVE.
                                END.
                            END.
                 
                            IF v-valid THEN
                            DO i = 1 TO 30:
      
                                IF v-lscore[i] NE eb.k-len-array2[i] THEN
                                DO:
                                    v-valid = NO.
                                    RELEASE eb.
                                    LEAVE.
                                END.
                            END.
                        END.
                    END.
                END.
            END. /*each b2-job-mat*/ 
      
            IF v-valid THEN
            DO:
                v-number-rows-selected2 = 0.

                FOR EACH b2-job-mat FIELDS(company rm-i-no job-no job-no2
                    frm blank-no) NO-LOCK WHERE
                    b2-job-mat.company = job.company AND
                    b2-job-mat.job = job.job AND
                    b2-job-mat.job-no = job.job-no AND
                    b2-job-mat.job-no2 = job.job-no2
                    ,
                    FIRST b-item NO-LOCK WHERE
                    b-item.company EQ b2-job-mat.company AND
                    b-item.i-no = b2-job-mat.rm-i-no
                    ,
                    FIRST report NO-LOCK WHERE
                    report.rec_key = string(ROWID(b2-job-mat)) AND
                    report.term-id EQ v-term + USERID("nosweat")
                    :
      
                    CREATE w-po-ordl.
                    ASSIGN
                        w-po-ordl.i-no          = po-ordl.i-no:screen-value
                        w-po-ordl.job-no        = b2-job-mat.job-no
                        w-po-ordl.job-no2       = b2-job-mat.job-no2
                        w-po-ordl.s-num         = b2-job-mat.frm
                        w-po-ordl.b-num         = b2-job-mat.blank-no
                        w-po-ordl.job-mat-rowid = ROWID(b2-job-mat)
                        v-number-rows-selected2 = v-number-rows-selected2 + 1.
                    RELEASE w-po-ordl.
                END.
            END.

            RUN check-workfile-2.

        END.

        FIND FIRST w-po-ordl NO-ERROR.

        IF AVAILABLE job THEN
            FOR EACH job-mat NO-LOCK
                WHERE job-mat.company EQ job.company
                AND job-mat.job     EQ job.job
                AND job-mat.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                AND job-mat.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                AND AVAILABLE w-po-ordl
                AND ROWID(job-mat) EQ w-po-ordl.job-mat-rowid
        
                
                BY job-mat.frm:

                RUN po/po-ordls.p (RECID(job-mat)).

                {po/poordls1W.i}

                FOR EACH b-job-mat
                    WHERE b-job-mat.company EQ job-mat.company
                    AND b-job-mat.job     EQ job-mat.job
                    AND b-job-mat.job-no  EQ job-mat.job-no
                    AND b-job-mat.job-no2 EQ job-mat.job-no2
                    AND b-job-mat.rm-i-no EQ job-mat.rm-i-no
                    AND b-job-mat.n-up    EQ job-mat.n-up
                    AND ROWID(b-job-mat)  NE ROWID(job-mat)
                    NO-LOCK:

                    RUN po/po-ordls.p (RECID(b-job-mat)).

                    {po/poordls1W.i b-}

                    IF b-job-mat.wid EQ job-mat.wid AND
                        b-job-mat.len EQ job-mat.len THEN 
                    DO:

                        IF AVAILABLE b-ref1 THEN 
                        DO:
                            ll-multi = AVAILABLE b-b-ref1.
                            IF ll-multi THEN
                                BUFFER-COMPARE b-ref1 USING val TO b-b-ref1
                                    SAVE RESULT IN ll-multi.
                        END.

                        IF ll-multi AND AVAILABLE b-ref2 THEN 
                        DO:
                            ll-multi = AVAILABLE b-b-ref2.
                            IF ll-multi THEN
                                BUFFER-COMPARE b-ref2 USING val TO b-b-ref2
                                    SAVE RESULT IN ll-multi.
                        END.
                    END.

                    ELSE ll-multi = NO.
                    FIND CURRENT b-ref1 NO-LOCK NO-ERROR.
                    FIND CURRENT b-ref2 NO-LOCK NO-ERROR.
                    IF NOT ll-multi THEN LEAVE.
                END.

                IF ll-multi THEN 
                DO:
                    CREATE tt-job-mat.
                    BUFFER-COPY job-mat EXCEPT rec_key TO tt-job-mat
                        ASSIGN
                        tt-job-mat.rm-i-no = po-ordl.i-no:SCREEN-VALUE
                        tt-job-mat.frm     = ?.

                    RELEASE b-job-mat.

                    FOR EACH b-job-mat FIELDS(qty)
                        WHERE b-job-mat.company EQ job-mat.company
                        AND b-job-mat.job     EQ job-mat.job
                        AND b-job-mat.job-no  EQ job-mat.job-no
                        AND b-job-mat.job-no2 EQ job-mat.job-no2
                        AND b-job-mat.rm-i-no EQ job-mat.rm-i-no
                        AND b-job-mat.n-up    EQ job-mat.n-up
                        AND ROWID(b-job-mat)  NE ROWID(job-mat)
                        NO-LOCK:
                        tt-job-mat.qty = tt-job-mat.qty + b-job-mat.qty.
                    END.
                END.

                LEAVE.
            END.
    END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-fgitem Dialog-Frame 
PROCEDURE display-fgitem :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.
    DEFINE VARIABLE v-len     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-wid     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-dep     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-op-type AS LOG     NO-UNDO.
    DEFINE VARIABLE lv-cost   LIKE po-ordl.cost NO-UNDO.

    FIND FIRST itemfg NO-LOCK WHERE /*itemfg.company eq cocode and
                  itemfg.i-no eq po-ordl.i-no use-index i-no */
        RECID(itemfg) = ip-recid
        NO-ERROR.

    IF AVAILABLE itemfg THEN
    DO:

    
        ASSIGN 
            po-ordl.i-name:SCREEN-VALUE IN FRAME {&FRAME-NAME} = itemfg.i-name
            po-ordl.cons-uom:SCREEN-VALUE                      = itemfg.prod-uom
            scr-cons-uom:SCREEN-VALUE                          = po-ordl.cons-uom:SCREEN-VALUE
            /* gdm - 06040918 */
            po-ordl.pr-uom:SCREEN-VALUE                        = itemfg.prod-uom
            /* gdm - 06040918 itemfg.pur-uom */
            po-ordl.pr-qty-uom:SCREEN-VALUE                    = IF pouom-chr EQ "Purchase" THEN itemfg.pur-uom
                                                                         ELSE itemfg.prod-uom
            po-ordl.cons-cost:SCREEN-VALUE                     = STRING(itemfg.last-cost)
            po-ordl.dscr[1]:SCREEN-VALUE                       = itemfg.part-dscr1
            po-ordl.dscr[2]:SCREEN-VALUE                       = itemfg.part-dscr2
            po-ordl.item-type:SCREEN-VALUE                     = "FG"
            fiCount:SCREEN-VALUE                               = STRING(itemfg.case-COUNT)
            v-op-type                                          = FALSE
            .
            RUN po/GetFGDimsForPO.p (ROWID(itemfg), OUTPUT v-len, OUTPUT v-wid, OUTPUT v-dep).
            ASSIGN  
                {po/calc16.i v-len}
                {po/calc16.i v-wid}
                {po/calc16.i v-dep}.

        IF itemfg.taxable AND aptax-chr EQ "Item" THEN
            po-ordl.tax:SCREEN-VALUE = "yes".

        FIND FIRST e-itemfg WHERE e-itemfg.company EQ cocode 
            AND e-itemfg.i-no EQ itemfg.i-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE e-itemfg THEN
            FIND FIRST e-itemfg-vend OF e-itemfg NO-LOCK
                /* gdm - 06040918 
                  WHERE e-itemfg-vend.vend-no EQ po-ord.vend-no
                */  
                NO-ERROR.
     
        IF AVAILABLE e-itemfg-vend THEN
            po-ordl.pr-uom:SCREEN-VALUE = e-itemfg.std-uom.

        IF po-ordl.pr-qty-uom:SCREEN-VALUE EQ "" THEN po-ordl.pr-qty-uom:SCREEN-VALUE = "EA".
        IF po-ordl.pr-uom:SCREEN-VALUE     EQ "" THEN po-ordl.pr-uom:SCREEN-VALUE     = "EA".
        IF po-ordl.cons-uom:SCREEN-VALUE   EQ "" THEN
            ASSIGN 
                po-ordl.cons-uom:SCREEN-VALUE = "EA"
                scr-cons-uom:SCREEN-VALUE     = po-ordl.cons-uom:SCREEN-VALUE.

        IF po-ordl.pr-uom:SCREEN-VALUE EQ "CS" THEN 
        DO:
            /* First convert to EA */
            IF LOOKUP(po-ordl.cons-uom:SCREEN-VALUE,fg-uom-list) EQ 0 OR
                LOOKUP(po-ordl.pr-uom:SCREEN-VALUE,fg-uom-list)   EQ 0 THEN
                RUN sys/ref/convcuom.p(po-ordl.cons-uom:SCREEN-VALUE, "EA",
                    0, v-len, v-wid, v-dep,
                    DEC(po-ordl.cons-cost:SCREEN-VALUE), OUTPUT lv-cost).
      
            /* Now convert EA to cases */
            lv-cost = DEC(po-ordl.cons-cost:SCREEN-VALUE) * itemfg.case-count.
        END.
        ELSE
            IF LOOKUP(po-ordl.cons-uom:SCREEN-VALUE,fg-uom-list) EQ 0 OR
                LOOKUP(po-ordl.pr-uom:SCREEN-VALUE,fg-uom-list)   EQ 0 THEN
                RUN sys/ref/convcuom.p(po-ordl.cons-uom:SCREEN-VALUE, po-ordl.pr-uom:SCREEN-VALUE,
                    0, v-len, v-wid, v-dep,
                    DEC(po-ordl.cons-cost:SCREEN-VALUE), OUTPUT lv-cost).

        ASSIGN
            po-ordl.cost:SCREEN-VALUE  = STRING(lv-cost)
            po-ordl.s-wid:SCREEN-VALUE = STRING(v-wid)
            po-ordl.s-len:SCREEN-VALUE = STRING(v-len)
            v-po-dep:SCREEN-VALUE      = STRING(v-dep).

        RUN sys\inc\decfrac2.p(INPUT DEC(po-ordl.s-wid:SCREEN-VALUE), INPUT 32, OUTPUT v-wid-frac).
        RUN sys\inc\decfrac2.p(INPUT DEC(po-ordl.s-len:SCREEN-VALUE), INPUT 32, OUTPUT v-len-frac).
        RUN sys\inc\decfrac2.p(INPUT DEC(v-po-dep:SCREEN-VALUE), INPUT 32, OUTPUT v-dep-frac).
        ASSIGN
            v-po-wid-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-wid-frac
            v-po-len-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-len-frac
            v-po-dep-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-dep-frac.

        /* populate GL# from reftable if it exists using itemfg AH 02-23-10*/
        ASSIGN 
            v-charge = "".
        FIND FIRST surcharge NO-LOCK WHERE surcharge.company = g_company
            AND surcharge.charge <> "" NO-ERROR.
        IF AVAILABLE surcharge THEN
            ASSIGN v-charge = surcharge.charge.
        FIND FIRST reftable NO-LOCK WHERE reftable.reftable EQ "chargecode"
            AND reftable.company  EQ itemfg.company
            AND reftable.loc      EQ itemfg.procat
            /*AND reftable.code     EQ v-charge*/
            /* AND reftable.code2 = "" */
            NO-ERROR.
        IF AVAILABLE reftable AND reftable.dscr <> "" THEN 
            ASSIGN po-ordl.actnum:SCREEN-VALUE = reftable.dscr.
        /* AH */
        ELSE 
            FOR EACH prodl NO-LOCK
                WHERE prodl.company EQ cocode
                AND prodl.procat  EQ itemfg.procat
                ,
                FIRST prod NO-LOCK
                WHERE prod.company EQ cocode
                AND prod.prolin  EQ prodl.prolin
                :

                po-ordl.actnum:SCREEN-VALUE = prod.fg-mat.
                LEAVE.
            END.

        RELEASE reftable.

        FIND FIRST account NO-LOCK WHERE account.company EQ cocode AND
            account.actnum EQ po-ordl.actnum:SCREEN-VALUE NO-ERROR.
        v-gl-desc:SCREEN-VALUE = IF AVAILABLE account THEN account.dscr ELSE ''.

        IF AVAILABLE e-itemfg THEN
            FIND FIRST e-itemfg-vend OF e-itemfg NO-LOCK
                WHERE e-itemfg-vend.vend-no EQ po-ord.vend-no
                  AND e-itemfg-vend.est-no eq "" NO-ERROR.

        IF AVAILABLE e-itemfg-vend AND e-itemfg-vend.vend-item NE "" THEN po-ordl.vend-i-no:SCREEN-VALUE = e-itemfg-vend.vend-item.
        ELSE IF itemfg.vend-no EQ po-ord.vend-no THEN po-ordl.vend-i-no:SCREEN-VALUE = itemfg.vend-item.
            ELSE IF itemfg.vend2-no EQ po-ord.vend-no THEN po-ordl.vend-i-no:SCREEN-VALUE = itemfg.vend2-item.

        RUN fg-qtys (ROWID(itemfg)).
    END.
    FIND FIRST account WHERE account.company EQ cocode AND
        account.actnum EQ po-ordl.actnum:SCREEN-VALUE NO-LOCK NO-ERROR.
    v-gl-desc:SCREEN-VALUE = IF AVAILABLE account THEN account.dscr ELSE ''.

    RUN vend-cost (YES).

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
    DEFINE BUFFER bJob    FOR job.
    DEFINE BUFFER bJobMat FOR job-mat.
    DEFINE BUFFER xJobMat FOR job-mat.
    DEFINE BUFFER bItem   FOR item.
    
    IF AVAILABLE po-ordl THEN 
    DO:
  
        DISPLAY po-ordl.t-cost po-ordl.job-no po-ordl.cons-qty po-ordl.job-no2 
            po-ordl.cons-uom po-ordl.i-no po-ordl.due-date po-ordl.cons-cost 
            po-ordl.i-name po-ordl.ord-qty po-ordl.pr-qty-uom po-ordl.dscr[1] 
            po-ordl.dscr[2] po-ordl.cost po-ordl.pr-uom po-ordl.disc po-ordl.s-wid 
            po-ordl.s-len po-ordl.actnum po-ordl.vend-i-no po-ordl.tax 
            po-ordl.under-pct po-ordl.over-pct po-ordl.stat po-ordl.cust-no 
            po-ordl.ord-no po-ordl.item-type po-ordl.setup po-ordl.s-num
            po-ordl.b-num
            WITH FRAME Dialog-Frame.

        ASSIGN 
            scr-cons-uom = po-ordl.cons-uom:SCREEN-VALUE.
        IF NOT po-ordl.item-type THEN 
        DO:
            FIND itemfg 
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no EQ po-ordl.i-no
                NO-LOCK NO-ERROR.
  
            IF AVAILABLE itemfg THEN
                fiCount:SCREEN-VALUE = STRING(itemfg.case-count).
            IF po-ordl.spare-int-1 EQ 1 THEN 
            DO:
                pr-qty-uom:SCREEN-VALUE = "CS".            
                po-ordl.ord-qty:SCREEN-VALUE = 
                    STRING(DEC(po-ordl.ord-qty) / DEC(fiCount:SCREEN-VALUE)).
            END. /* If spare-int-1 eq 1 */
            IF po-ordl.spare-int-2 EQ 1 THEN 
            DO:
                /* Cost is stored as 'ea' for 'CS', so multiply to get it back */
                po-ordl.pr-uom:SCREEN-VALUE = "CS".            
                po-ordl.cost:SCREEN-VALUE = 
                    STRING(DEC(po-ordl.cost) * DEC(fiCount:SCREEN-VALUE)).
            END. /* If spare-int-1 eq 1 */
        END. /* If a FG */
    END. /* If avail po-ordl */

    IF AVAILABLE po-ordl THEN
        IF po-ordl.item-type THEN 
        DO:
            FIND item WHERE item.company = g_company AND
                item.i-no = po-ordl.i-no:SCREEN-VALUE NO-LOCK NO-ERROR.
            IF AVAILABLE item THEN RUN rm-qtys (ROWID(item)).
        END. /* item-type eq yes (RM) */
        ELSE 
        DO:
            FIND itemfg WHERE itemfg.company = g_company AND
                itemfg.i-no = po-ordl.i-no:SCREEN-VALUE NO-LOCK NO-ERROR.
            IF AVAILABLE itemfg THEN RUN fg-qtys (ROWID(itemfg)).
        END.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            v-wid                      = po-ordl.s-wid
            v-len                      = po-ordl.s-len
     {po/calc16.i v-wid}
     {po/calc16.i v-len}
            po-ordl.s-wid:SCREEN-VALUE = STRING(v-wid)
            po-ordl.s-len:SCREEN-VALUE = STRING(v-len)
            lv-save-job                = po-ordl.job-no:SCREEN-VALUE
            lv-save-job2               = po-ordl.job-no2:SCREEN-VALUE
            lv-save-s-num              = po-ordl.s-num:SCREEN-VALUE
            lv-save-b-num              = po-ordl.b-num:SCREEN-VALUE.

        FIND FIRST reftable NO-LOCK WHERE
            reftable.reftable EQ "POORDLDEPTH" AND
            reftable.company  EQ cocode AND
            reftable.loc      EQ STRING(ip-ord-no) AND
            reftable.code     EQ STRING(po-ordl.LINE)
            NO-ERROR.

        IF AVAILABLE reftable THEN 
        DO:
            ASSIGN
                v-dep                 = DEC(reftable.code2)
        {po/calc16.i v-dep}
                v-po-dep:SCREEN-VALUE = STRING(v-dep).
            RELEASE reftable.
        END.
 
        RUN sys\inc\decfrac2.p(INPUT DEC(po-ordl.s-wid:SCREEN-VALUE), INPUT 32, OUTPUT v-wid-frac).
        RUN sys\inc\decfrac2.p(INPUT DEC(po-ordl.s-len:SCREEN-VALUE), INPUT 32, OUTPUT v-len-frac).
        RUN sys\inc\decfrac2.p(INPUT DEC(v-po-dep:SCREEN-VALUE), INPUT 32, OUTPUT v-dep-frac).
        ASSIGN
            v-po-wid-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-wid-frac
            v-po-len-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-len-frac
            v-po-dep-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-dep-frac.
    END.
    /* Updates po-ordl */

    
    IF AVAILABLE po-ordl AND ip-type NE "view"  THEN 
        FIND CURRENT po-ordl EXCLUSIVE-LOCK.

  {po/podisdet.i}

FIND CURRENT po-ordl NO-LOCK NO-ERROR.

fi_pb-qty:HIDDEN IN FRAME Dialog-Frame = NOT poqty-log.

RUN vend-cost (?).

RUN check-workfile.

IF ip-type <> "View" THEN ENABLE Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.

VIEW FRAME {&frame-name}. 
  
APPLY "entry" TO FRAME {&frame-name}.

RUN valid-vend-cost (NO) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.  

RUN valid-min-wid (NO) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

RUN valid-min-len (NO) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.
 
RUN adder-text.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-job-mat Dialog-Frame 
PROCEDURE display-job-mat :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ll-new-mat     AS LOG     NO-UNDO.
    DEFINE VARIABLE ld-line-qty    LIKE po-ordl.ord-qty NO-UNDO.
    DEFINE VARIABLE ld-ord-qty     LIKE oe-ordl.qty NO-UNDO.
    DEFINE VARIABLE ld-job-qty     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ld-line-cst    LIKE po-ordl.cost NO-UNDO.
    DEFINE VARIABLE ld-part-qty    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ll-upd-job-qty AS LOG     NO-UNDO.
    DEFINE VARIABLE ll-update-cost AS LOG     NO-UNDO.
    DEFINE VARIABLE ll-qty-changed AS LOG     NO-UNDO.

    DEFINE BUFFER b-job-hdr FOR job-hdr.
  
    DO WITH FRAME {&FRAME-NAME}:
        RELEASE tt-job-mat.
        RELEASE job-mat.
    
        FIND FIRST job
            WHERE job.company EQ g_company
            AND job.job-no  EQ po-ordl.job-no:SCREEN-VALUE
            AND job.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
            NO-LOCK NO-ERROR.
    
        FIND FIRST item
            WHERE item.company EQ g_company
            AND item.i-no    EQ po-ordl.i-no:SCREEN-VALUE
            NO-LOCK NO-ERROR.
    
        IF AVAILABLE item AND po-ordl.job-no:SCREEN-VALUE NE "" THEN 
        DO:
            FIND tt-job-mat
                WHERE tt-job-mat.company EQ g_company
                AND tt-job-mat.job     EQ job.job
                AND tt-job-mat.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                AND tt-job-mat.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                AND tt-job-mat.rm-i-no EQ po-ordl.i-no:SCREEN-VALUE
                AND tt-job-mat.frm     EQ INT(po-ordl.s-num:SCREEN-VALUE) 
                NO-LOCK NO-ERROR.
       
            IF NOT AVAILABLE tt-job-mat OR po-ordl.s-num:SCREEN-VALUE EQ "?" THEN
                FIND FIRST job-mat
                    WHERE job-mat.company   EQ g_company
                    AND job-mat.job       EQ job.job
                    AND job-mat.job-no    EQ po-ordl.job-no:SCREEN-VALUE
                    AND job-mat.job-no2   EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                    AND (job-mat.rm-i-no  EQ po-ordl.i-no:SCREEN-VALUE        OR
                    po-ordl.i-no:SCREEN-VALUE EQ "")
                    AND (po-ordl.s-num:SCREEN-VALUE EQ "?"                    OR
                    job-mat.frm      EQ INT(po-ordl.s-num:SCREEN-VALUE)  OR
                    INT(po-ordl.s-num:SCREEN-VALUE) EQ 0)
                    AND (po-ordl.s-num:SCREEN-VALUE EQ "?"                    OR
                    job-mat.blank-no EQ INT(po-ordl.b-num:SCREEN-VALUE)  OR
                    INT(po-ordl.b-num:SCREEN-VALUE) EQ 0)
                    NO-LOCK NO-ERROR.
       
            IF (NOT AVAILABLE tt-job-mat OR
                po-ordl.s-num:SCREEN-VALUE EQ "?")  AND
                NOT AVAILABLE job-mat                    AND
                INT(po-ordl.s-num:SCREEN-VALUE) NE 0 THEN 
            DO:
                ll-new-mat = NO.
       
                MESSAGE "Update item on Job file?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE ll-new-mat.
       
                IF ll-new-mat THEN 
                DO:
                    RUN replace-job-mat.
                    IF NOT AVAILABLE tt-job-mat THEN
                        FIND tt-job-mat WHERE RECID(tt-job-mat) EQ fil_id NO-LOCK NO-ERROR.
                    ELSE
                        IF NOT AVAILABLE job-mat AND INDEX("MOXY789@",ITEM.mat-type) EQ 0 THEN
                            DELETE tt-job-mat.
                    IF AVAILABLE tt-job-mat THEN po-ordl.b-num:SCREEN-VALUE = STRING(tt-job-mat.blank-no).
                    RELEASE job-mat.
                END.
            END.
         
            IF AVAILABLE tt-job-mat OR AVAILABLE job-mat THEN 
            DO:
                IF NOT AVAILABLE tt-job-mat THEN 
                DO:
                    EMPTY TEMP-TABLE tt-job-mat.
                    CREATE tt-job-mat.
                    BUFFER-COPY job-mat EXCEPT rec_key TO tt-job-mat.
                END.
          
                ELSE
                    IF tt-job-mat.blank-no EQ 0 THEN
                        tt-job-mat.blank-no = INT(po-ordl.b-num:SCREEN-VALUE).
           
                FIND FIRST job-hdr NO-LOCK
                    WHERE job-hdr.company   EQ g_company
                    AND job-hdr.job       EQ tt-job-mat.job
                    AND job-hdr.job-no    EQ tt-job-mat.job-no
                    AND job-hdr.job-no2   EQ tt-job-mat.job-no2
                    AND job-hdr.frm       EQ tt-job-mat.frm
                    AND (job-hdr.blank-no EQ tt-job-mat.blank-no OR tt-job-mat.blank-no EQ 0)
                    NO-ERROR.
          
                IF NOT AVAILABLE job-hdr THEN
                    FIND FIRST job-hdr NO-LOCK 
                        WHERE job-hdr.company   EQ g_company
                        AND job-hdr.job       EQ tt-job-mat.job
                        AND job-hdr.job-no    EQ tt-job-mat.job-no
                        AND job-hdr.job-no2   EQ tt-job-mat.job-no2
                        NO-ERROR.
          
                IF AVAILABLE job-hdr THEN
                    FIND FIRST oe-ordl NO-LOCK
                        WHERE oe-ordl.company EQ job-hdr.company
                        AND oe-ordl.ord-no  EQ job-hdr.ord-no
                        AND oe-ordl.i-no    EQ job-hdr.i-no
                        NO-ERROR.
          
                IF AVAILABLE oe-ordl THEN
                    FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
          
                IF AVAILABLE oe-ord THEN
                    ASSIGN
                        po-ordl.cust-no:SCREEN-VALUE = oe-ord.cust-no
                        po-ordl.ord-no:SCREEN-VALUE  = STRING(oe-ord.ord-no).

                /* S-8-POQTY JOBQTY or NETSHTS */
                IF v-po-qty                        OR
                    tt-job-mat.n-up EQ 0            OR
                    NOT CAN-DO("B,P",item.mat-type) THEN
                    ld-line-qty = tt-job-mat.qty.  /* Job Qty */
          
                ELSE 
                DO:
                    ASSIGN
                        ld-line-qty = 0
                        ld-part-qty = 0
                        ld-ord-qty  = 0
                        ld-job-qty  = 0.
          
                    IF AVAILABLE job-hdr THEN
                        FIND FIRST job NO-LOCK 
                            WHERE job.company EQ tt-job-mat.company
                            AND job.job     EQ tt-job-mat.job
                            AND job.job-no  EQ tt-job-mat.job-no
                            AND job.job-no2 EQ tt-job-mat.job-no2
                            NO-ERROR.
          
                    IF AVAILABLE job  THEN
                        FIND FIRST est NO-LOCK
                            WHERE est.company EQ job.company
                            AND est.est-no  EQ job.est-no
                            NO-ERROR.
          
                    FOR EACH job-hdr NO-LOCK
                        WHERE job-hdr.company   EQ tt-job-mat.company
                        AND job-hdr.job       EQ tt-job-mat.job
                        AND job-hdr.job-no    EQ tt-job-mat.job-no
                        AND job-hdr.job-no2   EQ tt-job-mat.job-no2
                        AND (job-hdr.frm      EQ tt-job-mat.frm OR
                        (AVAILABLE est AND (est.est-type EQ 2 OR est.est-type EQ 6)))
                        BREAK BY job-hdr.i-no:
          
                        ld-job-qty = ld-job-qty + job-hdr.qty.
          
                        IF LAST-OF(job-hdr.i-no) THEN 
                        DO:
                            ld-ord-qty = 0.
                            FOR EACH b-job-hdr FIELDS(qty) NO-LOCK
                                WHERE b-job-hdr.company EQ job-hdr.company
                                AND b-job-hdr.job     EQ job-hdr.job
                                AND b-job-hdr.job-no  EQ job-hdr.job-no
                                AND b-job-hdr.job-no2 EQ job-hdr.job-no2
                                AND b-job-hdr.i-no    EQ job-hdr.i-no
                                AND b-job-hdr.ord-no  EQ job-hdr.ord-no:
          
                                ld-ord-qty = ld-ord-qty + b-job-hdr.qty.
                            END.

                            ASSIGN
                                ld-job-qty = ld-ord-qty / ld-job-qty
                                ld-ord-qty = 0.
          
                            FOR EACH oe-ordl FIELDS(qty job-no job-no2) NO-LOCK
                                WHERE oe-ordl.company EQ job-hdr.company
                                AND oe-ordl.ord-no  EQ job-hdr.ord-no
                                AND oe-ordl.i-no    EQ job-hdr.i-no
                                :
          
                                IF (oe-ordl.job-no EQ job-hdr.job-no AND
                                    oe-ordl.job-no2 EQ job-hdr.job-no2) OR 
                                    oe-ordl.job-no EQ "" THEN
                                    ld-ord-qty = ld-ord-qty + oe-ordl.qty.
                            END.
          
                            ASSIGN
                                ld-line-qty = ld-line-qty + (ld-ord-qty * ld-job-qty)
                                ld-job-qty  = 0.

                        END.
                    END.

                    IF AVAILABLE est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN
                    DO:
                        IF tt-job-mat.frm NE ? THEN
                            FOR EACH eb FIELDS(quantityPerSet) NO-LOCK
                                WHERE eb.company EQ job.company
                                AND eb.est-no  EQ job.est-no
                                AND eb.form-no EQ tt-job-mat.frm
                                :
                  
                                ld-part-qty = ld-part-qty +
                                    (ld-line-qty * IF eb.quantityPerSet LT 0 THEN (-1 / eb.quantityPerSet)
                                    ELSE eb.quantityPerSet).
                            END.
                        ELSE
                            FOR EACH w-po-ordl
                                BREAK BY w-po-ordl.s-num:
                    
                                IF FIRST-OF(w-po-ordl.s-num) THEN
                                    FOR EACH eb FIELDS(quantityPerSet) WHERE
                                        eb.company EQ job.company AND
                                        eb.est-no  EQ job.est-no AND
                                        eb.form-no EQ w-po-ordl.s-num
                                        NO-LOCK:
                    
                                        ld-part-qty = ld-part-qty +
                                            (ld-line-qty * IF eb.quantityPerSet LT 0 THEN (-1 / eb.quantityPerSet)
                                            ELSE eb.quantityPerSet).
                                    END.
                            END.
                 
                    END.
                    ELSE
                        ld-part-qty = ld-line-qty.
          
                    ld-line-qty = ld-part-qty / tt-job-mat.n-up.
                    IF ld-line-qty = 0 THEN 
                    DO:
                        ld-line-qty = tt-job-mat.qty.  /* Job Qty */
                        FIND FIRST sys-ctrl NO-LOCK 
                            WHERE sys-ctrl.company EQ cocode
                            AND sys-ctrl.name EQ "JOBQTYCUST"
                            NO-ERROR.
                        IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld AND avail(job-hdr) 
                            AND v-po-qty = NO THEN 
                        DO:
                            FIND cust NO-LOCK WHERE cust.cust-no = job-hdr.cust-no
                                NO-ERROR.
                            IF avail(cust) AND cust.over-pct > 0 THEN
                                ld-line-qty = ld-line-qty / (1 + (cust.over-pct / 100)).
                        END.
                    END.

                    IF po-ordl.pr-qty-uom:SCREEN-VALUE EQ "EA" THEN 
                    DO:
                    {sys/inc/roundup.i ld-line-qty}             
                    END.
                END.
          
                ASSIGN
                    po-ordl.job-no:SCREEN-VALUE  = tt-job-mat.job-no
                    po-ordl.job-no2:SCREEN-VALUE = STRING(tt-job-mat.job-no2)
                    po-ordl.s-num:SCREEN-VALUE   = STRING(tt-job-mat.frm)
                    po-ordl.b-num:SCREEN-VALUE   = STRING(tt-job-mat.blank-no)
                    lv-save-job                  = po-ordl.job-no:SCREEN-VALUE
                    lv-save-job2                 = po-ordl.job-no2:SCREEN-VALUE
                    lv-save-s-num                = po-ordl.s-num:SCREEN-VALUE
                    lv-save-b-num                = po-ordl.b-num:SCREEN-VALUE
                    v-len                        = tt-job-mat.len
                    v-wid                        = tt-job-mat.wid
                    v-dep                        = item.s-dep
                    ld-line-cst                  = tt-job-mat.std-cost.
          
                IF tt-job-mat.qty-uom NE po-ordl.pr-qty-uom:SCREEN-VALUE THEN
                    RUN sys/ref/convquom.p (tt-job-mat.qty-uom, po-ordl.pr-qty-uom:SCREEN-VALUE,
                        tt-job-mat.basis-w, v-len, v-wid, v-dep,
                        ld-line-qty, OUTPUT ld-line-qty).
          
                IF tt-job-mat.sc-uom NE po-ordl.pr-uom:SCREEN-VALUE THEN
                    RUN sys/ref/convcuom.p (tt-job-mat.sc-uom, po-ordl.pr-uom:SCREEN-VALUE,
                        tt-job-mat.basis-w, v-len, v-wid, v-dep,
                        ld-line-cst, OUTPUT ld-line-cst).
          
                ASSIGN
            {po/calc16.i v-len}
            {po/calc16.i v-wid}
            {po/calc16.i v-dep}
                    po-ordl.s-len:SCREEN-VALUE = STRING(v-len)
                    po-ordl.s-wid:SCREEN-VALUE = STRING(v-wid)
                    v-po-dep:SCREEN-VALUE      = STRING(v-dep)
                    ll-upd-job-qty             = YES
                    ll-qty-changed             = DEC(po-ordl.ord-qty:SCREEN-VALUE) NE ld-line-qty
                    ll-update-cost             = YES.

                IF ip-type EQ "Update" THEN
                    MESSAGE "Import Job Quantity?"
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                        UPDATE ll-upd-job-qty.

                IF ll-upd-job-qty EQ YES THEN
                    po-ordl.ord-qty:SCREEN-VALUE = STRING(ld-line-qty).

                IF ip-type EQ "Update" THEN
                DO:
                    ll-update-cost = NO.

                    IF ll-qty-changed THEN
                        MESSAGE "Import Cost?"
                            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                            UPDATE ll-update-cost.
                END.

                IF ll-update-cost THEN
                DO:
                    IF tt-job-mat.orig-lot-cost-upd EQ NO THEN
                        po-ordl.cost:SCREEN-VALUE  = STRING(ld-line-cst).
                    ELSE
                        po-ordl.cost:SCREEN-VALUE  = STRING(tt-job-mat.orig-lot-cost).
                END.
          
                RUN sys\inc\decfrac2.p(INPUT DEC(po-ordl.s-wid:SCREEN-VALUE), INPUT 32, OUTPUT v-wid-frac).
                RUN sys\inc\decfrac2.p(INPUT DEC(po-ordl.s-len:SCREEN-VALUE), INPUT 32, OUTPUT v-len-frac).
                RUN sys\inc\decfrac2.p(INPUT DEC(v-po-dep:SCREEN-VALUE), INPUT 32, OUTPUT v-dep-frac).
                ASSIGN
                    v-po-wid-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-wid-frac
                    v-po-len-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-len-frac
                    v-po-dep-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-dep-frac.

                IF ll-update-cost AND v-pocost1 BEGINS "Vendor" THEN 
                DO:
                    RUN vend-cost (YES).
                    RUN adder-text.
                END.
          
                IF NOT ll-new-mat THEN EMPTY TEMP-TABLE tt-job-mat.
          
                IF po-ordl.cost:SCREEN-VALUE EQ "?"    OR
                    DEC(po-ordl.cost:SCREEN-VALUE) EQ ? THEN
                    po-ordl.cost:SCREEN-VALUE = "0".
           
                IF ll-upd-job-qty THEN
                DO:
                    APPLY "value-changed" TO po-ordl.ord-qty.
                    APPLY "value-changed" TO po-ordl.pr-qty-uom.
                END.

                IF ll-update-cost THEN
                    APPLY "value-changed" TO po-ordl.cost.

                APPLY "value-changed" TO po-ordl.pr-uom.
                APPLY "value-changed" TO po-ordl.s-len.
                APPLY "value-changed" TO po-ordl.s-wid.
                APPLY "value-changed" TO v-po-dep.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-rmitem Dialog-Frame 
PROCEDURE display-rmitem :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.

    FIND ITEM WHERE RECID(ITEM) = ip-recid NO-LOCK NO-ERROR.
  
    IF AVAILABLE ITEM THEN 
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            po-ordl.dscr[1]:SCREEN-VALUE    = item.i-dscr
            po-ordl.dscr[2]:SCREEN-VALUE    = item.est-dscr 
            po-ordl.i-name:SCREEN-VALUE     = item.i-name
            po-ordl.pr-qty-uom:SCREEN-VALUE = IF pouom-chr EQ "Purchase" THEN item.pur-uom
                                                                      ELSE item.cons-uom
            po-ordl.cons-uom:SCREEN-VALUE   = item.cons-uom
            po-ordl.pr-uom:SCREEN-VALUE     = item.pur-uom
            po-ordl.cons-cost:SCREEN-VALUE  = STRING(item.last-cost)
            po-ordl.item-type:SCREEN-VALUE  = "RM"
            scr-cons-uom:SCREEN-VALUE       = po-ordl.cons-uom:SCREEN-VALUE.

        IF item.tax-rcpt AND aptax-chr EQ "Item" THEN
            po-ordl.tax:SCREEN-VALUE = "yes".

        IF po-ordl.pr-qty-uom:SCREEN-VALUE EQ "" THEN po-ordl.pr-qty-uom:SCREEN-VALUE = "EA".
        IF po-ordl.pr-uom:SCREEN-VALUE     EQ "" THEN po-ordl.pr-uom:SCREEN-VALUE     = "EA".
        IF po-ordl.cons-uom:SCREEN-VALUE   EQ "" THEN
            ASSIGN
                po-ordl.cons-uom:SCREEN-VALUE = "EA"
                scr-cons-uom:SCREEN-VALUE     = po-ordl.cons-uom:SCREEN-VALUE.

        FIND FIRST e-item-vend WHERE
            e-item-vend.company EQ cocode AND
            e-item-vend.i-no    EQ item.i-no AND
            e-item-vend.vend-no EQ po-ord.vend-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE e-item-vend AND e-item-vend.vend-item NE "" THEN po-ordl.vend-i-no:SCREEN-VALUE = e-item-vend.vend-item.
        ELSE IF item.vend-no EQ po-ord.vend-no THEN po-ordl.vend-i-no:SCREEN-VALUE = item.vend-item.
            ELSE IF item.vend2-no EQ po-ord.vend-no THEN po-ordl.vend-i-no:SCREEN-VALUE = item.vend2-item.
  
        ASSIGN
            v-basis-w = 0
            v-dep     = item.s-dep.   

        IF CAN-DO("B,P,1,2,3,4",item.mat-type) THEN 
        DO:
            ASSIGN
                v-basis-w = item.basis-w
                v-len     = item.s-len
                v-wid     = IF item.s-wid NE 0 THEN item.s-wid ELSE item.r-wid.

            IF v-len EQ 0 AND item.mat-type EQ "P"       AND
                po-ordl.pr-qty-uom:SCREEN-VALUE EQ "ROLL" THEN v-len = ld-roll-len.
        END.

        ELSE
            IF CAN-DO("C,5,6,D",item.mat-type) THEN
                ASSIGN
                    v-len = item.case-l
                    v-wid = item.case-w
                    v-dep = item.case-d
     {po/calc16.i v-dep}.
            ELSE
                ASSIGN
                    v-len = 0
                    v-wid = 0
                    v-dep = 0.
  
  ASSIGN
   {po/calc16.i v-len}
   {po/calc16.i v-wid}
   {po/calc16.i v-dep}.

        FIND FIRST e-item WHERE e-item.company EQ cocode 
            AND e-item.i-no EQ item.i-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE e-item THEN 
            ASSIGN 
                po-ordl.cons-uom:SCREEN-VALUE = IF LOOKUP(e-item.std-uom, "MSF,EA,M,MSH") > 0 THEN "EA"
                                      ELSE e-item.std-uom
                po-ordl.pr-uom:SCREEN-VALUE   = IF v-pocost1 EQ "Vendor/MSH" AND
                                         po-ord.type NE "S"        AND
                                         e-item.std-uom EQ "TON"   AND
                                         v-basis-w NE 0            AND
                                         v-wid NE 0                THEN "MSH"
                                      ELSE e-item.std-uom
                scr-cons-uom:SCREEN-VALUE     = po-ordl.cons-uom:SCREEN-VALUE.

        IF pouom-int EQ 1 AND item.mat-type EQ "P" THEN
            ASSIGN
                po-ordl.cons-uom:SCREEN-VALUE = "TON"
                scr-cons-uom:SCREEN-VALUE     = po-ordl.cons-uom:SCREEN-VALUE.
  
        ASSIGN 
            po-ordl.s-wid:SCREEN-VALUE = STRING(v-wid)
            po-ordl.s-len:SCREEN-VALUE = STRING(v-len)
            v-po-dep:SCREEN-VALUE      = STRING(v-dep).
        RUN sys\inc\decfrac2.p(INPUT DEC(po-ordl.s-wid:SCREEN-VALUE), INPUT 32, OUTPUT v-wid-frac).
        RUN sys\inc\decfrac2.p(INPUT DEC(po-ordl.s-len:SCREEN-VALUE), INPUT 32, OUTPUT v-len-frac).
        RUN sys\inc\decfrac2.p(INPUT DEC(v-po-dep:SCREEN-VALUE), INPUT 32, OUTPUT v-dep-frac).
        ASSIGN
            v-po-wid-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-wid-frac
            v-po-len-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-len-frac
            v-po-dep-frac:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-dep-frac.
  
        FIND FIRST costtype NO-LOCK
            WHERE costtype.company   EQ cocode
            AND costtype.loc       EQ po-ord.loc
            AND costtype.cost-type EQ item.cost-type
            NO-ERROR.
        IF AVAILABLE costtype AND v-default-gl-log THEN
            po-ordl.actnum:SCREEN-VALUE =
                IF v-default-gl-cha EQ "Asset"   THEN costtype.inv-asset
                ELSE
                IF v-default-gl-cha BEGINS "Exp"  AND
                (v-default-gl-cha EQ "Expense" OR costtype.cons-exp NE "")
                THEN costtype.cons-exp
                ELSE                                  po-ordl.actnum:SCREEN-VALUE.

        RUN rm-qtys (ROWID(item)).

        RUN vend-cost (YES).
    END.
    FIND FIRST account NO-LOCK WHERE account.company EQ cocode AND
        account.actnum EQ po-ordl.actnum:SCREEN-VALUE NO-ERROR.
    v-gl-desc:SCREEN-VALUE = IF AVAILABLE account THEN account.dscr ELSE ''.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-disable-blk Dialog-Frame 
PROCEDURE enable-disable-blk :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-blk LIKE job-mat.blank-no NO-UNDO.

    DEFINE BUFFER b-job-mat FOR job-mat.

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST job NO-LOCK 
            WHERE job.company EQ g_company
            AND job.job-no  EQ po-ordl.job-no:SCREEN-VALUE
            AND job.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
            NO-ERROR.

        ll-blk-enabled = po-ordl.job-no:SCREEN-VALUE NE "" AND
            INT(po-ordl.s-num:SCREEN-VALUE) NE 0 AND
            CAN-FIND(FIRST ITEM WHERE ITEM.company = g_company AND ITEM.i-no = po-ordl.i-no:SCREEN-VALUE AND
            ITEM.mat-type <> "B").

        FIND FIRST b-job-mat NO-LOCK 
            WHERE b-job-mat.company EQ g_company
            AND b-job-mat.job     EQ job.job
            AND b-job-mat.job-no  EQ po-ordl.job-no:SCREEN-VALUE
            AND b-job-mat.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
            AND (b-job-mat.frm    EQ INT(po-ordl.s-num:SCREEN-VALUE) OR
            po-ordl.s-num:SCREEN-VALUE EQ "?")
            AND b-job-mat.rm-i-no EQ po-ordl.i-no:SCREEN-VALUE
            NO-ERROR.

        IF ll-blk-enabled THEN
            FOR EACH job-mat NO-LOCK
                WHERE job-mat.company   EQ g_company
                AND job-mat.job       EQ job.job
                AND job-mat.job-no    EQ po-ordl.job-no:SCREEN-VALUE
                AND job-mat.job-no2   EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                AND (b-job-mat.frm    EQ INT(po-ordl.s-num:SCREEN-VALUE) OR
                po-ordl.s-num:SCREEN-VALUE EQ "?")
                AND (job-mat.rm-i-no  EQ po-ordl.i-no:SCREEN-VALUE OR
                NOT AVAILABLE b-job-mat)
                
                BREAK BY job-mat.blank-no:

                IF FIRST(job-mat.blank-no) THEN lv-blk = job-mat.blank-no.

                IF LAST(job-mat.blank-no) AND lv-blk EQ job-mat.blank-no THEN 
                DO:
                    /*         ll-blk-enabled = NO. */
                    po-ordl.b-num:SCREEN-VALUE = STRING(lv-blk).
                END.
            END.

        IF ll-blk-enabled THEN ENABLE  po-ordl.b-num.
        ELSE DISABLE po-ordl.b-num.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-disable-frm Dialog-Frame 
PROCEDURE enable-disable-frm :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-frm LIKE job-mat.frm NO-UNDO.
  
        
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST job NO-LOCK 
            WHERE job.company EQ g_company
            AND job.job-no  EQ po-ordl.job-no:SCREEN-VALUE
            AND job.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
            NO-ERROR.

        ll-frm-enabled = po-ordl.job-no:SCREEN-VALUE NE "".

        IF ll-frm-enabled THEN
            FOR EACH job-mat NO-LOCK
                WHERE job-mat.company   EQ g_company
                AND job-mat.job       EQ job.job
                AND job-mat.job-no    EQ po-ordl.job-no:SCREEN-VALUE
                AND job-mat.job-no2   EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                AND job-mat.frm       GT 0
                
                BREAK BY job-mat.frm:

                IF FIRST(job-mat.frm) THEN lv-frm = job-mat.frm.

                IF LAST(job-mat.frm) AND lv-frm EQ job-mat.frm THEN 
                DO:
                    /*         ll-frm-enabled = NO. */
                    IF INT(po-ordl.s-num:SCREEN-VALUE) EQ 0 THEN
                        po-ordl.s-num:SCREEN-VALUE = STRING(lv-frm).
                END.
            END.

        IF ll-frm-enabled THEN ENABLE  po-ordl.s-num.
        ELSE DISABLE po-ordl.s-num.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-disable-size Dialog-Frame 
PROCEDURE enable-disable-size :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /* disable width, length for real item task# 11280513 */
    DO WITH FRAME {&FRAME-NAME}:
        IF po-ordl.item-type:SCREEN-VALUE EQ "RM" OR po-ordl.item-type THEN 
        DO:
            FIND item WHERE item.company = g_company AND
                item.i-no = po-ordl.i-no:SCREEN-VALUE NO-LOCK NO-ERROR.
            IF AVAILABLE item AND item.i-code = "R" THEN 
            DO:
                /*IF ITEM.r-wid > 0 AND ITEM.s-len = 0 THEN DISABLE po-ordl.s-wid.
                ELSE*/ 
                DISABLE po-ordl.s-wid po-ordl.s-len.
                fiCount:HIDDEN = TRUE.
            END.
        END.
        ELSE 
        DO:
            fiCount:HIDDEN = FALSE.
            FIND itemfg NO-LOCK 
                WHERE itemfg.company EQ cocode 
                AND itemfg.i-no EQ po-ordl.i-no:SCREEN-VALUE
                NO-ERROR.
       
            IF AVAILABLE itemfg THEN
                fiCount:SCREEN-VALUE = STRING(itemfg.case-count).
        END.
    END.
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
    DISPLAY fiCount fi_c-a-hdr fi_uom scr-cons-uom v-tot-msf v-po-dep 
        v-po-wid-frac v-po-len-frac v-po-dep-frac v-gl-desc fi_pb-qty 
        fi_pb-cst fi_q-onh fi_q-ono fi_q-comm fi_q-back fi_q-avail fi_m-onh 
        fi_m-ono fi_m-comm fi_m-back fi_m-avail fi_msf 
        WITH FRAME Dialog-Frame.
    IF AVAILABLE po-ordl THEN 
        DISPLAY po-ordl.i-no po-ordl.job-no po-ordl.job-no2 po-ordl.s-num 
            po-ordl.b-num po-ordl.due-date po-ordl.stat po-ordl.i-name 
            po-ordl.ord-qty po-ordl.pr-qty-uom po-ordl.cons-qty po-ordl.dscr[1] 
            po-ordl.cost po-ordl.pr-uom po-ordl.cons-cost po-ordl.cons-uom 
            po-ordl.dscr[2] po-ordl.setup po-ordl.s-wid po-ordl.s-len po-ordl.disc 
            po-ordl.actnum po-ordl.vend-i-no po-ordl.tax po-ordl.over-pct 
            po-ordl.under-pct po-ordl.cust-no po-ordl.ord-no po-ordl.t-cost 
            po-ordl.item-type 
            WITH FRAME Dialog-Frame.
    ENABLE po-ordl.i-no po-ordl.job-no po-ordl.job-no2 po-ordl.s-num 
        po-ordl.b-num po-ordl.due-date po-ordl.i-name po-ordl.ord-qty 
        po-ordl.pr-qty-uom po-ordl.dscr[1] po-ordl.cost po-ordl.pr-uom 
        po-ordl.dscr[2] po-ordl.setup po-ordl.s-wid po-ordl.s-len po-ordl.disc 
        po-ordl.actnum po-ordl.vend-i-no po-ordl.tax po-ordl.over-pct 
        po-ordl.under-pct po-ordl.cust-no po-ordl.ord-no Btn_Done Btn_Cancel 
        Btn_OK po-ordl.item-type RECT-21 RECT-38 btnCalendar-1
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fg-qtys Dialog-Frame 
PROCEDURE fg-qtys :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.


    FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-LOCK NO-ERROR.

    IF AVAILABLE itemfg THEN 
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            fi_c-a-hdr = "Allocated"
            fi_uom     = "EA"
            fi_q-onh   = itemfg.q-onh
            fi_q-ono   = itemfg.q-ono
            fi_q-comm  = itemfg.q-alloc
            fi_q-back  = itemfg.q-back
            fi_q-avail = itemfg.q-avail.

        DISPLAY fi_c-a-hdr
            fi_uom
            fi_q-onh
            fi_q-ono
            fi_q-comm
            fi_q-back
            fi_q-avail.

        ASSIGN
            fi_msf:HIDDEN     = YES
            fi_m-onh:HIDDEN   = YES
            fi_m-ono:HIDDEN   = YES
            fi_m-comm:HIDDEN  = YES
            fi_m-back:HIDDEN  = YES
            fi_m-avail:HIDDEN = YES.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-itemfg-gl Dialog-Frame 
PROCEDURE get-itemfg-gl :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-comp LIKE job-hdr.company.
    DEFINE INPUT PARAMETER ip-i-no LIKE itemfg.i-no.
    DEFINE OUTPUT PARAMETER out-actnum LIKE po-ordl.actnum.
     
    /* populate GL# from reftable if it exists using itemfg AH 02-23-10 */

    FIND itemfg NO-LOCK WHERE itemfg.company = ip-comp
        AND itemfg.i-no = ip-i-no NO-ERROR.
    IF AVAILABLE itemfg THEN 
    DO:
        ASSIGN 
            v-charge = "".
        FIND FIRST surcharge NO-LOCK WHERE surcharge.company = ip-comp
            AND surcharge.charge <> "" NO-ERROR.
        IF AVAILABLE surcharge THEN
            ASSIGN v-charge = surcharge.charge.
        FIND FIRST reftable NO-LOCK WHERE reftable.reftable EQ "chargecode"
            AND reftable.company  EQ itemfg.company
            AND reftable.loc      EQ itemfg.procat
            AND reftable.code     EQ v-charge
            /* AND reftable.code2 = "" */
            NO-ERROR.

        IF AVAILABLE reftable AND reftable.code2 <> "" THEN 
            ASSIGN out-actnum = reftable.code2.
        RELEASE reftable.
    END.
/* AH */                                                               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getJobFarmInfo Dialog-Frame 
PROCEDURE getJobFarmInfo :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bfJob-farm FOR job-farm.
    DEFINE BUFFER bfJob-hdr  FOR job-hdr.
    DEFINE BUFFER bfItemfg   FOR itemfg.
    DEFINE VARIABLE lHeadFound AS LOG       NO-UNDO.
    DEFINE VARIABLE lcCust-NO  AS CHARACTER NO-UNDO.

    ASSIGN 
        lcCust-No  = ""
        lHeadFound = NO.

    DO WITH FRAME {&FRAME-NAME}:

        FIND FIRST bfJob-farm NO-LOCK 
            WHERE bfJob-farm.company EQ g_company 
            AND bfJob-farm.job-no  EQ po-ordl.job-no:SCREEN-VALUE 
            AND bfJob-farm.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE) 
            AND bfJob-farm.i-no EQ po-ordl.i-no:SCREEN-VALUE 
            AND bfJob-farm.frm     EQ INT(po-ordl.s-num:SCREEN-VALUE) 
            AND bfJob-farm.blank-no EQ INT(po-ordl.b-num:SCREEN-VALUE)
            NO-ERROR.
  
        IF AVAILABLE bfJob-Farm THEN 
        DO:
  
            FOR EACH bfJob-hdr NO-LOCK WHERE bfJob-hdr.company EQ bfJob-farm.company
                AND bfJob-hdr.job-no EQ bfJob-farm.job-no
                AND bfJob-hdr.job-no2 EQ bfJob-farm.job-no2
                ,
                FIRST bfItemfg NO-LOCK
                WHERE bfItemfg.company EQ bfJob-hdr.company
                AND bfItemfg.i-no    EQ bfJob-hdr.i-no
                AND bfItemfg.isaset  EQ TRUE
                .
                
                ASSIGN 
                    lHeadFound = TRUE
                    lcCust-no  = bfJob-hdr.cust-no.
            END.

            FIND FIRST bfJob-hdr NO-LOCK WHERE bfJob-hdr.company EQ bfJob-farm.company
                AND bfJob-hdr.job-no EQ bfJob-farm.job-no
                AND bfJob-hdr.job-no2 EQ bfJob-farm.job-no2
                AND bfJob-hdr.frm     EQ bfJob-farm.frm
                AND bfJob-hdr.blank-no EQ bfJob-farm.blank-no
                AND bfJob-hdr.i-no     EQ bfJob-farm.i-no
                NO-ERROR.
                           
     
            IF lHeadFound THEN 
            DO:

                IF AVAILABLE bfJob-hdr THEN
                    po-ordl.ord-no:SCREEN-VALUE = STRING(bfJob-hdr.ord-no).
               
            END.
      
            ASSIGN 
                po-ordl.cust-no:SCREEN-VALUE = lcCust-no
                po-ordl.ord-qty:SCREEN-VALUE = STRING(bfJob-farm.qty, po-ordl.ord-qty:FORMAT).
        END. /* avail bfJob-farm */
    END. /* do */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lookup-job Dialog-Frame 
PROCEDURE lookup-job :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-job-no  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE char-val   AS cha       NO-UNDO.
    DEFINE VARIABLE look-recid AS RECID     NO-UNDO.
    DEFINE BUFFER bf-itemfg FOR itemfg.

    DO WITH FRAME {&frame-name}:
        lv-job-no = FILL(" ", 6 - LENGTH(TRIM(po-ordl.job-no:SCREEN-VALUE))) +
            TRIM(po-ordl.job-no:SCREEN-VALUE).

        RELEASE job-mat.

        FIND FIRST job NO-LOCK 
            WHERE job.company EQ g_company
            AND job.job-no  EQ lv-job-no
            AND job.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
            NO-ERROR.

        IF lv-job-no NE "" AND AVAILABLE job THEN
            FIND FIRST job-mat NO-LOCK 
                WHERE job-mat.company  EQ g_company
                AND job-mat.job      EQ job.job
                AND job-mat.job-no   EQ job.job-no
                AND (job-mat.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE) OR FOCUS:NAME EQ "job-no")
                AND job-mat.rm-i-no  EQ po-ordl.i-no:SCREEN-VALUE
                NO-ERROR.

        IF AVAILABLE job-mat THEN 
        DO:
            RUN windows/l-jobmt2.w (g_company, job-mat.job-no, IF FOCUS:NAME EQ "job-no" THEN ? ELSE INT(po-ordl.job-no2:SCREEN-VALUE),job-mat.rm-i-no, OUTPUT char-val, OUTPUT look-recid, OUTPUT v-number-rows-selected).
            IF char-val NE "" THEN RUN new-job-mat (look-recid).
        END.

        ELSE 
        DO:
            /* If this is purchased, check the job-farm tab for items */
            FIND FIRST bf-itemfg WHERE bf-itemfg.company EQ g_company
                AND bf-itemfg.i-no EQ po-ordl.i-no:SCREEN-VALUE
                NO-LOCK NO-ERROR.
            IF AVAILABLE bf-itemfg AND bf-itemfg.pur-man THEN 
            DO:
      
                RUN windows/l-jobnop.w (g_company, lv-job-no, OUTPUT char-val, OUTPUT look-recid).
        
                FIND FIRST job-hdr WHERE RECID(job-hdr) EQ look-recid NO-LOCK NO-ERROR.
        
                IF AVAILABLE job-hdr THEN
                    FIND FIRST job-farm NO-LOCK WHERE job-farm.job EQ job-hdr.job 
                        AND job-farm.i-no EQ po-ordl.i-no:SCREEN-VALUE  NO-ERROR.
      
                /* char-val = "" to keep new-job-line from overriding s-num, b-num */
                IF AVAILABLE job-farm THEN
                    ASSIGN po-ordl.s-num:SCREEN-VALUE = STRING(job-farm.frm)
                        po-ordl.b-num:SCREEN-VALUE = STRING(job-farm.blank-no).

                IF char-val NE "" THEN RUN new-job-line-farm (look-recid).
            END.
            ELSE 
            DO:
                /* Original code */
                RUN windows/l-jobnoo.w (g_company, lv-job-no, OUTPUT char-val, OUTPUT look-recid).
                IF char-val NE "" THEN RUN new-job-line (look-recid).
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job Dialog-Frame 
PROCEDURE new-job :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    
    DO WITH FRAME {&FRAME-NAME}:
    
        FIND FIRST job-hdr NO-LOCK 
            WHERE job-hdr.company   EQ g_company
            AND job-hdr.job-no    EQ po-ordl.job-no:SCREEN-VALUE
            AND job-hdr.job-no2   EQ INT(po-ordl.job-no2:SCREEN-VALUE)
            AND (job-hdr.frm      EQ INT(po-ordl.s-num:SCREEN-VALUE) OR
            po-ordl.s-num:SCREEN-VALUE EQ "?"                   OR
            INT(po-ordl.s-num:SCREEN-VALUE) EQ 0)
            NO-ERROR.
        IF AVAILABLE job-hdr THEN 
        DO:
          
            FIND FIRST job-farm WHERE job-farm.job EQ job-hdr.job 
                AND job-farm.i-no EQ po-ordl.i-no:SCREEN-VALUE NO-LOCK NO-ERROR.
          
            /* char-val = "" to keep new-job-line from overriding s-num, b-num */
            IF AVAILABLE job-farm THEN 
            DO:
                ASSIGN 
                    po-ordl.s-num:SCREEN-VALUE = STRING(job-farm.frm)
                    po-ordl.b-num:SCREEN-VALUE = STRING(job-farm.blank-no).
                RUN new-job-farm (RECID(job-farm)).
            END.
            ELSE 
                RUN new-job-line (RECID(job-hdr)).
        END. /* AVail job-hdr */
        

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-farm Dialog-Frame 
PROCEDURE new-job-farm :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.
      
  
    FIND FIRST job-farm NO-LOCK WHERE RECID(job-farm) EQ ip-recid NO-ERROR.

    IF AVAILABLE job-farm THEN 
    DO WITH FRAME {&FRAME-NAME}:
        IF job-farm.i-no  NE po-ordl.i-no:SCREEN-VALUE            OR
            job-farm.job-no   NE po-ordl.job-no:SCREEN-VALUE          OR
            job-farm.job-no2  NE INT(po-ordl.job-no2:SCREEN-VALUE)    OR
            (job-farm.frm     NE INT(po-ordl.s-num:SCREEN-VALUE) AND
            po-ordl.s-num:SCREEN-VALUE NE "?")                      OR
            job-farm.blank-no NE INT(po-ordl.b-num:SCREEN-VALUE)      OR
            ll-new-job-mat                                           THEN 
        DO:
      
            ASSIGN
                ll-new-job-mat               = NO
                po-ordl.i-no:SCREEN-VALUE    = job-farm.i-no
                po-ordl.job-no:SCREEN-VALUE  = job-farm.job-no
                po-ordl.job-no2:SCREEN-VALUE = STRING(job-farm.job-no2)
                po-ordl.b-num:SCREEN-VALUE   = STRING(job-farm.blank-no).

            IF po-ordl.s-num:SCREEN-VALUE NE "?" THEN
                po-ordl.s-num:SCREEN-VALUE = STRING(job-farm.frm).
       
            /* APPLY "leave" TO po-ordl.i-no. */
            APPLY "Entry" TO po-ordl.due-date.

            RUN enable-disable-frm.
            RUN enable-disable-blk.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-line Dialog-Frame 
PROCEDURE new-job-line :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.

    DEFINE BUFFER b-po-ordl FOR po-ordl.

  
    FIND FIRST job-hdr WHERE RECID(job-hdr) EQ ip-recid NO-LOCK NO-ERROR.

    IF AVAILABLE job-hdr THEN 
    DO WITH FRAME {&FRAME-NAME}:
        IF job-hdr.job-no   NE po-ordl.job-no:SCREEN-VALUE          OR
            job-hdr.job-no2  NE INT(po-ordl.job-no2:SCREEN-VALUE)    OR
            (job-hdr.frm     NE INT(po-ordl.s-num:SCREEN-VALUE) AND
            po-ordl.s-num:SCREEN-VALUE NE "?")                      OR
            job-hdr.blank-no NE INT(po-ordl.b-num:SCREEN-VALUE)      THEN 
        DO:
                   
            ASSIGN
                po-ordl.job-no:SCREEN-VALUE  = job-hdr.job-no
                po-ordl.job-no2:SCREEN-VALUE = STRING(job-hdr.job-no2).

            IF po-ordl.s-num:SCREEN-VALUE NE "?" THEN
                po-ordl.s-num:SCREEN-VALUE = STRING(job-hdr.frm).
      
            IF v-number-rows-selected <> 1 THEN
         
                FIND FIRST job-mat NO-LOCK 
                    WHERE job-mat.company EQ g_company
                    AND job-mat.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                    AND job-mat.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                    AND job-mat.rm-i-no EQ po-ordl.i-no:SCREEN-VALUE
                    AND NOT CAN-FIND(FIRST b-po-ordl
                    WHERE b-po-ordl.company EQ job-mat.company
                    AND b-po-ordl.job-no  EQ job-mat.job-no
                    AND b-po-ordl.job-no2 EQ job-mat.job-no2
                    AND b-po-ordl.i-no    EQ job-mat.rm-i-no
                    AND (b-po-ordl.s-num  EQ job-mat.frm OR
                    b-po-ordl.s-num  EQ ?)
                    AND b-po-ordl.b-num   EQ job-mat.blank-no
                    AND RECID(b-po-ordl)  NE RECID(po-ordl))
                    NO-ERROR.
            ELSE
                FIND FIRST job-mat NO-LOCK 
                    WHERE job-mat.company EQ g_company
                    AND job-mat.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                    AND job-mat.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                    AND job-mat.rm-i-no EQ po-ordl.i-no:SCREEN-VALUE
                    AND job-mat.frm     EQ INT(po-ordl.s-num:SCREEN-VALUE)
                    AND job-mat.blank-no EQ INT(po-ordl.b-num:SCREEN-VALUE)
                    AND NOT CAN-FIND(FIRST b-po-ordl
                    WHERE b-po-ordl.company EQ job-mat.company
                    AND b-po-ordl.job-no  EQ job-mat.job-no
                    AND b-po-ordl.job-no2 EQ job-mat.job-no2
                    AND b-po-ordl.i-no    EQ job-mat.rm-i-no
                    AND (b-po-ordl.s-num  EQ job-mat.frm OR
                    b-po-ordl.s-num  EQ ?)
                    AND b-po-ordl.b-num   EQ job-mat.blank-no
                    AND RECID(b-po-ordl)  NE RECID(po-ordl))
                    NO-ERROR.


            IF AVAILABLE job-mat THEN 
            DO:
                ll-new-job-mat = YES.
                RUN new-job-mat (RECID(job-mat)).
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-line-farm Dialog-Frame 
PROCEDURE new-job-line-farm :
    /*------------------------------------------------------------------------------
      Purpose:     Special case for finished goods farmout
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.

    DEFINE BUFFER b-po-ordl FOR po-ordl.

  
    FIND FIRST job-hdr WHERE RECID(job-hdr) EQ ip-recid NO-LOCK NO-ERROR.
    IF AVAILABLE job-hdr THEN
        FIND FIRST job-farm WHERE job-farm.job EQ job-hdr.job 
            AND job-farm.i-no EQ po-ordl.i-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.

    IF AVAILABLE job-hdr AND avail(job-farm) THEN 
    DO WITH FRAME {&FRAME-NAME}:
        IF job-farm.job-no   NE po-ordl.job-no:SCREEN-VALUE          OR
            job-farm.job-no2  NE INT(po-ordl.job-no2:SCREEN-VALUE)    OR
            (job-farm.frm     NE INT(po-ordl.s-num:SCREEN-VALUE) AND
            po-ordl.s-num:SCREEN-VALUE NE "?")                      OR
            job-farm.blank-no NE INT(po-ordl.b-num:SCREEN-VALUE)      THEN 
        DO:
                   
            ASSIGN
                po-ordl.job-no:SCREEN-VALUE  = job-farm.job-no
                po-ordl.job-no2:SCREEN-VALUE = STRING(job-farm.job-no2).

            IF po-ordl.s-num:SCREEN-VALUE NE "?" THEN
                po-ordl.s-num:SCREEN-VALUE = STRING(job-farm.frm).
      
            IF v-number-rows-selected <> 1 THEN
         
                FIND FIRST job-farm NO-LOCK 
                    WHERE job-farm.company EQ g_company
                    AND job-farm.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                    AND job-farm.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                    AND job-farm.i-no EQ po-ordl.i-no:SCREEN-VALUE
                    AND NOT CAN-FIND(FIRST b-po-ordl
                    WHERE b-po-ordl.company EQ job-farm.company
                    AND b-po-ordl.job-no  EQ job-farm.job-no
                    AND b-po-ordl.job-no2 EQ job-farm.job-no2
                    AND b-po-ordl.i-no    EQ job-farm.i-no
                    AND (b-po-ordl.s-num  EQ job-farm.frm OR
                    b-po-ordl.s-num  EQ ?)
                    AND b-po-ordl.b-num   EQ job-farm.blank-no
                    AND RECID(b-po-ordl)  NE RECID(po-ordl))
                    NO-ERROR.
            ELSE
                FIND FIRST job-farm NO-LOCK 
                    WHERE job-farm.company EQ g_company
                    AND job-farm.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                    AND job-farm.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                    AND job-farm.i-no EQ po-ordl.i-no:SCREEN-VALUE
                    AND job-farm.frm     EQ INT(po-ordl.s-num:SCREEN-VALUE)
                    AND job-farm.blank-no EQ INT(po-ordl.b-num:SCREEN-VALUE)
                    AND NOT CAN-FIND(FIRST b-po-ordl
                    WHERE b-po-ordl.company EQ job-farm.company
                    AND b-po-ordl.job-no  EQ job-farm.job-no
                    AND b-po-ordl.job-no2 EQ job-farm.job-no2
                    AND b-po-ordl.i-no    EQ job-farm.i-no
                    AND (b-po-ordl.s-num  EQ job-farm.frm OR
                    b-po-ordl.s-num  EQ ?)
                    AND b-po-ordl.b-num   EQ job-farm.blank-no
                    AND RECID(b-po-ordl)  NE RECID(po-ordl))
                    NO-ERROR.

      
            IF AVAILABLE job-farm THEN 
            DO:
                ll-new-job-mat = YES.
                RUN new-job-farm (RECID(job-farm)).
            END.
        END.
    END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-mat Dialog-Frame 
PROCEDURE new-job-mat :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.
      
  
    FIND FIRST job-mat NO-LOCK WHERE RECID(job-mat) EQ ip-recid NO-ERROR.

    IF AVAILABLE job-mat THEN 
    DO WITH FRAME {&FRAME-NAME}:
        IF job-mat.rm-i-no  NE po-ordl.i-no:SCREEN-VALUE            OR
            job-mat.job-no   NE po-ordl.job-no:SCREEN-VALUE          OR
            job-mat.job-no2  NE INT(po-ordl.job-no2:SCREEN-VALUE)    OR
            (job-mat.frm     NE INT(po-ordl.s-num:SCREEN-VALUE) AND
            po-ordl.s-num:SCREEN-VALUE NE "?")                      OR
            job-mat.blank-no NE INT(po-ordl.b-num:SCREEN-VALUE)      OR
            ll-new-job-mat                                           THEN 
        DO:
      
            ASSIGN
                ll-new-job-mat               = NO
                po-ordl.i-no:SCREEN-VALUE    = job-mat.rm-i-no
                po-ordl.job-no:SCREEN-VALUE  = job-mat.job-no
                po-ordl.job-no2:SCREEN-VALUE = STRING(job-mat.job-no2)
                po-ordl.b-num:SCREEN-VALUE   = STRING(job-mat.blank-no).

            IF po-ordl.s-num:SCREEN-VALUE NE "?" THEN
                po-ordl.s-num:SCREEN-VALUE = STRING(job-mat.frm).

            APPLY "leave" TO po-ordl.i-no.

            RUN enable-disable-frm.
            RUN enable-disable-blk.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-s-b Dialog-Frame 
PROCEDURE new-job-s-b :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.
      
  
    FIND FIRST job-mat WHERE RECID(job-mat) EQ ip-recid NO-LOCK NO-ERROR.

    IF AVAILABLE job-mat THEN 
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            po-ordl.s-num:SCREEN-VALUE = STRING(job-mat.frm)
            po-ordl.b-num:SCREEN-VALUE = STRING(job-mat.blank-no).

        APPLY "leave" TO po-ordl.s-num.
        APPLY "leave" TO po-ordl.b-num.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-setup Dialog-Frame 
PROCEDURE new-setup :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ld AS DECIMAL NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        RUN set-dims.

        IF ld-prev-setup EQ 0                    OR
            DEC(po-ordl.t-cost:SCREEN-VALUE) NE 0 THEN 
        DO:
            ASSIGN
                ld                          = DEC(po-ordl.t-cost:SCREEN-VALUE) -
                                        ld-prev-setup +
                                        DEC(po-ordl.setup:SCREEN-VALUE)
                po-ordl.t-cost:SCREEN-VALUE = STRING(ld).
        /*po-ordl.cons-cost:SCREEN-VALUE = STRING(ld / DEC(po-ordl.cons-qty:SCREEN-VALUE))
        ld                             = DEC(po-ordl.cons-cost:SCREEN-VALUE).
       IF po-ordl.cons-uom:SCREEN-VALUE NE po-ordl.pr-uom:SCREEN-VALUE AND
          (po-ordl.item-type:SCREEN-VALUE EQ "yes"                OR
           LOOKUP(po-ordl.cons-uom:SCREEN-VALUE,fg-uom-list) EQ 0 OR
           LOOKUP(po-ordl.pr-uom:SCREEN-VALUE,fg-uom-list)   EQ 0)     THEN
         RUN sys/ref/convcuom.p(po-ordl.cons-uom:SCREEN-VALUE,
                                po-ordl.pr-uom:SCREEN-VALUE,
                                v-basis-w, v-len, v-wid, v-dep,
                                ld, OUTPUT ld).
       po-ordl.cost:SCREEN-VALUE = STRING(ld).*/
        END.

        ld-prev-setup = DEC(po-ordl.setup:SCREEN-VALUE).
    END.
  /* wfk - was not recalculating */
  FIND CURRENT po-ordl EXCLUSIVE-LOCK NO-ERROR.
  {po/podisdet.i}
  FIND CURRENT po-ordl NO-LOCK NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE po-adder2 Dialog-Frame 
PROCEDURE po-adder2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid  AS RECID.
    DEFINE INPUT PARAMETER ip-recid1 AS RECID.
    DEFINE INPUT PARAMETER ip-vend-no LIKE po-ord.vend-no NO-UNDO.
    DEFINE INPUT PARAMETER ip-qty AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-cost AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-cons-cost AS DECIMAL NO-UNDO.

    DEFINE OUTPUT PARAMETER op-cost AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER op-cons-cost AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER op-adder-setup AS DECIMAL NO-UNDO.

    DEFINE VARIABLE v-tot-cost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-cost     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-add-cost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-qty-comp AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-setup    LIKE e-item-vend.setup NO-UNDO.
    DEFINE VARIABLE v-adder    AS DECIMAL EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-index    AS INTEGER NO-UNDO.

    DEFINE BUFFER xjob-mat FOR job-mat.

    FIND xjob-mat WHERE RECID(xjob-mat) EQ ip-recid1 NO-LOCK.

    ASSIGN
        addersText   = ''
        op-cost      = ip-cost
        op-cons-cost = ip-cons-cost.


    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST item NO-LOCK WHERE 
            item.company  EQ job-mat.company AND
            item.i-no     EQ po-ordl.i-no:SCREEN-VALUE
            NO-ERROR.

        IF AVAILABLE ITEM AND
            ITEM.mat-type NE "B" THEN
            LEAVE.

        ASSIGN
            v-adder[1] = ip-cost
            v-adder[2] = ip-cons-cost.

        IF po-ordl.pr-uom:SCREEN-VALUE EQ "EA"                    OR
            (NOT po-ordl.item-type AND
            LOOKUP(po-ordl.pr-uom:SCREEN-VALUE,fg-uom-list) EQ 0) THEN
            v-tot-cost = ip-cost.

        ELSE
            RUN sys/ref/convcuom.p(po-ordl.pr-uom:SCREEN-VALUE, "EA",
                v-basis-w, v-len, v-wid, v-dep,
                ip-cost, OUTPUT v-tot-cost).
 
        FOR EACH job-mat NO-LOCK
            WHERE job-mat.company  EQ xjob-mat.company
            AND job-mat.job      EQ xjob-mat.job
            AND job-mat.frm      EQ xjob-mat.frm
            AND job-mat.job-no   EQ xjob-mat.job-no
            AND job-mat.job-no2  EQ xjob-mat.job-no2
            USE-INDEX seq-idx,

            FIRST item NO-LOCK
            WHERE item.company  EQ job-mat.company
            AND item.i-no     EQ job-mat.i-no
            AND item.mat-type EQ "A":

            FIND FIRST e-item NO-LOCK
                WHERE e-item.company EQ po-ordl.company
                AND e-item.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                NO-ERROR.
    
            FIND FIRST e-item-vend NO-LOCK
                WHERE e-item-vend.company EQ item.company
                AND e-item-vend.i-no    EQ item.i-no
                AND e-item-vend.vend-no EQ ip-vend-no
                NO-ERROR.

            IF AVAILABLE e-item AND AVAILABLE e-item-vend AND ip-vend-no NE "" THEN 
            DO:
                IF po-ordl.pr-qty-uom:SCREEN-VALUE EQ e-item.std-uom THEN
                    v-qty-comp = ip-qty.
                ELSE
                    RUN sys/ref/convquom.p(po-ordl.pr-qty-uom:SCREEN-VALUE, e-item.std-uom,
                        v-basis-w, v-len, v-wid, v-dep,
                        ip-qty, OUTPUT v-qty-comp).
        

                v-setup = 0.

                EMPTY TEMP-TABLE tt-eiv-2.
                CREATE tt-eiv-2.

                DO v-index = 1 TO 10:
                    ASSIGN
                        tt-eiv-2.run-qty[v-index]  = e-item-vend.run-qty[v-index]
                        tt-eiv-2.run-cost[v-index] = e-item-vend.run-cost[v-index]
                        tt-eiv-2.setups[v-index]   = e-item-vend.setups[v-index].
                END.


                IF AVAILABLE e-item-vend THEN
                DO:

      
                    DO v-index = 1 TO 10:
                        ASSIGN
                            tt-eiv-2.run-qty[v-index + 10]  = e-item-vend.runQtyXtra[v-index]
                            tt-eiv-2.run-cost[v-index + 10] = e-item-vend.runCostXtra[v-index]
                            tt-eiv-2.setups[v-index + 10]   = e-item-vend.setupsXtra[v-index].
                    END.
                END.

                DO i = 1 TO EXTENT(tt-eiv-2.run-qty):
                    IF v-qty-comp LE tt-eiv-2.run-qty[i] THEN
                        LEAVE.
                END.
                /*  if i eq 1 then v-setup = e-item-vend.setup. */
                IF i GT EXTENT(tt-eiv-2.run-qty) THEN i = EXTENT(tt-eiv-2.run-qty).
                ASSIGN
                    v-setup        = tt-eiv-2.setups[i]
                    op-adder-setup = op-adder-setup + v-setup
                    v-cost         = /*((*/tt-eiv-2.run-cost[i] /** v-qty-comp) + v-setup) / v-qty-comp*/ .
                /* This adds the Adder cost in */
                IF e-item.std-uom NE po-ordl.pr-uom:SCREEN-VALUE THEN
                    RUN sys/ref/convcuom.p(e-item.std-uom, po-ordl.pr-uom:SCREEN-VALUE, job-mat.basis-w,
                        job-mat.len, job-mat.wid, item.s-dep,
                        v-cost, OUTPUT v-cost).
            END.

            ELSE 
            DO:
                v-cost = job-mat.std-cost.
      
                IF job-mat.sc-uom NE po-ordl.pr-uom:SCREEN-VALUE THEN
                    RUN sys/ref/convcuom.p(job-mat.sc-uom, po-ordl.pr-uom:SCREEN-VALUE, job-mat.basis-w,
                        job-mat.len, job-mat.wid, item.s-dep,
                        job-mat.std-cost, OUTPUT v-cost).
            END.
            IF v-cost = ? THEN v-cost = 0.
            ASSIGN
                addersText = addersText + SUBSTR(item.i-name,1,18) +
                  FILL(' ',19 - LENGTH(SUBSTR(item.i-name,1,18))) +
                  STRING(v-cost,'-z,zz9.99') + STRING(v-setup,'-zzz9.99') + CHR(10)
                v-add-cost = v-add-cost + v-cost.

            /* gdm - */     
            IF v-cost NE 0                         
                THEN RUN po-adder3 (INPUT v-cost).   
        /* gdm - end */                                
        END.

        IF po-ordl.pr-uom:SCREEN-VALUE NE "EA" THEN 
            RUN sys/ref/convcuom.p("EA", po-ordl.pr-uom:SCREEN-VALUE,
                v-basis-w, v-len, v-wid, v-dep,
                v-tot-cost, OUTPUT v-tot-cost).
 
        op-cost = v-add-cost + v-tot-cost.

        IF po-ordl.pr-uom:SCREEN-VALUE NE po-ordl.cons-uom:SCREEN-VALUE THEN
            RUN sys/ref/convcuom.p(po-ordl.pr-uom:SCREEN-VALUE, po-ordl.cons-uom:SCREEN-VALUE,
                v-basis-w, v-len, v-wid, v-dep,
                ip-cost, OUTPUT op-cons-cost).

        /*  display po-ordl.cost po-ordl.cons-cost.  */

        ASSIGN
            v-adder[1] = op-cost      - v-adder[1]
            v-adder[2] = op-cons-cost - v-adder[2].
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE po-adder3 Dialog-Frame 
PROCEDURE po-adder3 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-cost     AS DECIMAL NO-UNDO.
    FIND FIRST po-ordl-add EXCLUSIVE-LOCK 
        WHERE po-ordl-add.company    EQ po-ordl.company
        AND po-ordl-add.po-no      EQ po-ordl.po-no  
        AND po-ordl-add.line       EQ po-ordl.LINE   
        AND po-ordl-add.adder-i-no EQ job-mat.i-no NO-ERROR.
    
    IF NOT AVAILABLE po-ordl-add THEN 
    DO:
    
        CREATE po-ordl-add.
        ASSIGN 
            po-ordl-add.company    = po-ordl.company
            po-ordl-add.po-no      = po-ordl.po-no
            po-ordl-add.line       = po-ordl.LINE
            po-ordl-add.adder-i-no = job-mat.i-no.
               
    /*****************************************************************
             THIS IN CASE WE NEED IT LATER
    *****************************************************************            
               po-ordl-add.cons-cost   =
               po-ordl-add.cons-qty    =
               po-ordl-add.cons-uom    =
               po-ordl-add.ord-qty     =
               po-ordl-add.pr-qty-uom  =
               po-ordl-add.pr-uom      = 
               po-ordl-add.t-cost      =
               po-ordl-add.setup       = .
    *****************************************************************/
        /*rec_key CREATION CALL */
        {custom/rec_key.i po-ordl-add}

    END.
    ELSE
        IF AVAILABLE po-ordl-add THEN 
        DO:
            /* IF THERE THE SAME ADDER IN A JOB - RECORD THE BIGGER COST */        
            IF ip-cost GT po-ordl-add.cost 
                THEN ASSIGN po-ordl-add.cost = ip-cost.
        END.
    FIND CURRENT po-ordl-add NO-ERROR.
    RELEASE po-ordl-add.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE replace-job-mat Dialog-Frame 
PROCEDURE replace-job-mat :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ld-job-up  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ld-job-qty LIKE job-hdr.qty NO-UNDO.
    DEFINE VARIABLE count-mat  AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-out      AS INTEGER NO-UNDO.
    DEFINE VARIABLE choice     AS LOG     NO-UNDO.
    DEFINE VARIABLE v-cost     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-sheet   LIKE po-ordl.s-num NO-UNDO.
    DEFINE VARIABLE lv-blank   LIKE po-ordl.b-num NO-UNDO.
    DEFINE VARIABLE ll-layout  AS LOG     NO-UNDO.

    DEFINE BUFFER xitem FOR item.


    FOR EACH tt-job-mat WHERE tt-job-mat.frm NE ?:
        DELETE tt-job-mat.
    END.

    EMPTY TEMP-TABLE tt-s-num.

    DO WITH FRAME {&FRAME-NAME}:
        IF po-ordl.s-num:SCREEN-VALUE EQ "?" THEN
            FOR EACH w-po-ordl:
                CREATE tt-s-num.
                ASSIGN
                    tt-s-num.s-num  = w-po-ordl.s-num
                    tt-s-num.row-id = w-po-ordl.job-mat-rowid.
            END.

        ELSE 
        DO:
            CREATE tt-s-num.
            tt-s-num.s-num = INT(po-ordl.s-num:SCREEN-VALUE).
        END.

        FOR EACH tt-s-num BREAK BY tt-s-num.s-num:
            FIND FIRST job NO-LOCK 
                WHERE job.company EQ g_company
                AND job.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                AND job.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                NO-ERROR.

            FIND FIRST ITEM  NO-LOCK 
                WHERE item.company EQ job.company
                AND item.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                NO-ERROR.

            ASSIGN
                fil_id    = ?
                ll-layout = CAN-DO("1,2,3,4,B,P,R",item.mat-type).

            FIND job-mat NO-LOCK WHERE ROWID(job-mat) EQ tt-s-num.row-id NO-ERROR.

            IF NOT AVAILABLE job-mat THEN 
            DO:
                EMPTY TEMP-TABLE item-chg.

                FOR EACH job-mat NO-LOCK
                    WHERE job-mat.company  EQ job.company
                    AND job-mat.job      EQ job.job
                    AND job-mat.job-no   EQ job.job-no
                    AND job-mat.job-no2  EQ job.job-no2
                    AND job-mat.frm      EQ tt-s-num.s-num                    
                    AND (job-mat.blank-no EQ INT(po-ordl.b-num:SCREEN-VALUE) OR
                    INT(po-ordl.b-num:SCREEN-VALUE) = 0 OR
                    job-mat.blank-no EQ 0)        
                    AND NOT CAN-FIND(FIRST xpo-ordl
                    WHERE xpo-ordl.company EQ job-mat.company
                    AND xpo-ordl.po-no   EQ po-ordl.po-no
                    AND xpo-ordl.job-no  EQ job-mat.job-no
                    AND xpo-ordl.job-no2 EQ job-mat.job-no2
                    AND xpo-ordl.i-no    EQ job-mat.rm-i-no
                    AND (xpo-ordl.s-num  EQ job-mat.frm OR
                    xpo-ordl.s-num  EQ ?)
                    AND xpo-ordl.b-num   EQ job-mat.blank-no
                    AND RECID(xpo-ordl)  NE RECID(po-ordl))                                        
                    ,
 
                    FIRST xitem NO-LOCK
                    WHERE xitem.company  EQ job-mat.company
                    AND xitem.i-no     EQ job-mat.rm-i-no
                    AND xitem.mat-type EQ item.mat-type
                    :

                    count-mat = count-mat + 1.
                    CREATE item-chg.
                    ASSIGN
                        item-chg.i-no   = xitem.i-no
                        item-chg.rec-id = RECID(job-mat)
                        fil_id          = RECID(item-chg).    
                END.

                IF count-mat GT 1 THEN RUN rm/g-itmchg.w.

                FIND FIRST item-chg WHERE RECID(item-chg) EQ fil_id NO-LOCK NO-ERROR.

                fil_id = ?.

                RELEASE job-mat.

                IF AVAILABLE item-chg THEN
                    FIND job-mat WHERE RECID(job-mat) EQ item-chg.rec-id NO-ERROR.
            END.

            CREATE tt-job-mat.

            ASSIGN
                ld-job-up  = 0
                ld-job-qty = 0.

            FIND FIRST est NO-LOCK 
                WHERE est.company EQ job.company
                AND est.est-no  EQ job.est-no
                NO-ERROR.

            IF AVAILABLE est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN
                FOR EACH job-hdr FIELDS(qty) NO-LOCK
                    WHERE job-hdr.company EQ job.company
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2
                    :
        
                    ld-job-qty = ld-job-qty + job-hdr.qty.
                END.
            ELSE
                FOR EACH job-hdr FIELDS(n-on qty) NO-LOCK
                    WHERE job-hdr.company EQ job.company
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2
                    AND job-hdr.frm    EQ tt-s-num.s-num
                    :
         
                    ASSIGN
                        ld-job-qty = ld-job-qty + job-hdr.qty
                        ld-job-up  = ld-job-up + job-hdr.n-on.
                END.
      
            IF AVAILABLE est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN
                FOR EACH eb FIELDS(num-up) NO-LOCK
                    WHERE eb.company EQ est.company
                    AND eb.est-no  EQ est.est-no
                    AND eb.form-no EQ tt-s-num.s-num
                    :
                    ld-job-up = ld-job-up + eb.num-up.
                END.

            IF ll-layout OR ld-job-up EQ 0 THEN ld-job-up = 1.

            IF FIRST(tt-s-num.s-num) THEN 
            DO:
                lv-sheet = INT(po-ordl.s-num:SCREEN-VALUE).

                IF AVAILABLE job-mat THEN
                    ASSIGN
                        lv-blank = job-mat.blank-no
                        v-out    = (job-mat.n-up / ld-job-up).
                ELSE
                    ASSIGN
                        lv-blank = INT(po-ordl.b-num:SCREEN-VALUE)
                        v-out    = 1.

                IF ll-layout THEN 
                DO:
                    RUN rm/g-iss2.w (lv-sheet, lv-blank, INPUT-OUTPUT v-out). 
                 
                    IF AVAILABLE job-mat THEN 
                    DO:
                        IF item.i-code EQ "R" THEN 
                        DO:
                            IF (item.r-wid NE 0 AND item.r-wid LT job-mat.wid) OR
                                (item.r-wid EQ 0 AND (item.s-wid LT job-mat.wid OR
                                item.s-len LT job-mat.len)) THEN 
                            DO:
                                choice = NO.

                                IF item.r-wid NE 0 THEN
                                    RUN rm/g-iss21.w (job-mat.len, job-mat.len, item.r-wid, job-mat.wid, job-mat.frm,
                                        OUTPUT choice)  .
                                ELSE
                                    RUN rm/g-iss21.w (item.s-len, job-mat.len, item.s-wid,job-mat.wid, job-mat.frm,
                                        OUTPUT choice)  .
                
                                IF NOT choice THEN DELETE tt-job-mat.
                            END.
                        END.
                    END.
                END.
            END.

            IF AVAILABLE job-mat THEN 
            DO:
                FIND FIRST xitem
                    WHERE xitem.company EQ cocode
                    AND xitem.i-no    EQ job-mat.rm-i-no
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE xitem THEN DELETE tt-job-mat.

                IF AVAILABLE tt-job-mat THEN
                    BUFFER-COPY job-mat TO tt-job-mat
                        ASSIGN tt-job-mat.row-id = ROWID(job-mat).
            END.

            ELSE 
            DO:
                ASSIGN
                    tt-job-mat.company  = job.company
                    tt-job-mat.job      = job.job
                    tt-job-mat.job-no   = job.job-no
                    tt-job-mat.job-no2  = job.job-no2
                    tt-job-mat.frm      = tt-s-num.s-num
                    tt-job-mat.blank-no = lv-blank
                    tt-job-mat.qty-uom  = item.cons-uom
                    tt-job-mat.n-up     = ld-job-up.

                IF po-ordl.pr-qty-uom:SCREEN-VALUE NE ""  AND
                    DEC(po-ordl.ord-qty:SCREEN-VALUE) NE 0 THEN
                    ASSIGN
                        tt-job-mat.qty-uom = po-ordl.pr-qty-uom:SCREEN-VALUE
                        tt-job-mat.qty     = DEC(po-ordl.ord-qty:SCREEN-VALUE).

                IF po-ordl.pr-uom:SCREEN-VALUE NE ""   AND
                    DEC(po-ordl.cost:SCREEN-VALUE) NE 0 THEN
                DO:
                    ASSIGN
                        tt-job-mat.sc-uom   = po-ordl.pr-uom:SCREEN-VALUE
                        tt-job-mat.std-cost = DEC(po-ordl.cost:SCREEN-VALUE).

                    IF LOOKUP(tt-job-mat.sc-uom,"L,LOT") GT 0 THEN
                        ASSIGN
                            tt-job-mat.orig-lot-cost-upd = YES
                            tt-job-mat.orig-lot-cost     = DEC(po-ordl.cost:SCREEN-VALUE).
                END.
            END.

            IF AVAILABLE tt-job-mat THEN 
            DO:
                IF tt-job-mat.sc-uom EQ tt-job-mat.qty-uom THEN
                    v-cost = tt-job-mat.std-cost.
                ELSE
                    IF LOOKUP(tt-job-mat.sc-uom,"L,LOT") EQ 0 THEN
                        RUN sys/ref/convcuom.p(tt-job-mat.sc-uom,
                            tt-job-mat.qty-uom,
                            tt-job-mat.basis-w,
                            tt-job-mat.len,
                            tt-job-mat.wid,
                            item.s-dep,
                            tt-job-mat.std-cost,
                            OUTPUT v-cost).
                    ELSE
                        IF LOOKUP(tt-job-mat.sc-uom,"L,LOT") GT 0 AND
                            tt-job-mat.qty NE 0 THEN
                            v-cost = tt-job-mat.std-cost / tt-job-mat.qty.

                v-cost = v-cost * tt-job-mat.qty.                       
                    
                IF tt-job-mat.n-up LE 0 THEN tt-job-mat.n-up = 1.
                IF ld-job-up LE 0 THEN ld-job-up = 1.
                IF v-out LE 0 THEN v-out = 1.

                ASSIGN                 
                    tt-job-mat.rm-i-no = item.i-no
                    tt-job-mat.i-no    = item.i-no
                    tt-job-mat.basis-w = item.basis-w
                    tt-job-mat.qty     = tt-job-mat.qty * tt-job-mat.n-up
                    tt-job-mat.n-up    = ld-job-up * v-out
                    tt-job-mat.qty     = tt-job-mat.qty / tt-job-mat.n-up
                    tt-job-mat.cost-m  = v-cost / (ld-job-qty / 1000).

                IF item.i-code EQ "R" OR NOT AVAILABLE job-mat THEN
                    ASSIGN
                        tt-job-mat.sc-uom = item.cons-uom
                        tt-job-mat.wid    = IF item.r-wid NE 0 THEN
                                item.r-wid ELSE item.s-wid
                        tt-job-mat.len    = IF item.r-wid NE 0 THEN
                                tt-job-mat.len ELSE item.s-len.
                     
                IF tt-job-mat.qty-uom EQ "EA" THEN 
                DO:
                {sys/inc/roundup.i tt-job-mat.qty}
                END.
                
                v-cost = v-cost / tt-job-mat.qty.
                IF v-cost = ? THEN v-cost = 0.

                IF tt-job-mat.qty-uom EQ tt-job-mat.sc-uom THEN
                    tt-job-mat.std-cost = v-cost.
                ELSE
                    RUN sys/ref/convcuom.p(tt-job-mat.qty-uom,
                        tt-job-mat.sc-uom,
                        tt-job-mat.basis-w,
                        tt-job-mat.len,
                        tt-job-mat.wid,
                        item.s-dep,
                        v-cost,
                        OUTPUT tt-job-mat.std-cost).
            END.

            DELETE tt-s-num.
        END.  /* each tt-s-num */

        FIND FIRST tt-job-mat
            WHERE tt-job-mat.frm EQ INT(po-ordl.s-num:SCREEN-VALUE)
            NO-ERROR.

        IF AVAILABLE tt-job-mat THEN fil_id = RECID(tt-job-mat).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rm-qtys Dialog-Frame 
PROCEDURE rm-qtys :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    DEFINE VARIABLE li AS INTEGER NO-UNDO.
    DEFINE VARIABLE ld AS DECIMAL NO-UNDO.


    FIND item NO-LOCK WHERE ROWID(item) EQ ip-rowid NO-ERROR.

    IF AVAILABLE item THEN 
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            fi_c-a-hdr = "Committed"
            fi_uom     = item.cons-uom
            fi_q-onh   = item.q-onh
            fi_q-ono   = item.q-ono
            fi_q-comm  = item.q-comm
            fi_q-back  = item.q-back
            fi_q-avail = item.q-avail.

        DISPLAY fi_c-a-hdr
            fi_uom
            fi_q-onh
            fi_q-ono
            fi_q-comm
            fi_q-back
            fi_q-avail.

        IF item.i-code EQ "R"   AND
            item.mat-type EQ "B" AND
            fi_uom NE "MSF"      THEN 
        DO:
            DO li = 1 TO 5:
                ld = IF li EQ 1 THEN fi_q-onh  ELSE
                    IF li EQ 2 THEN fi_q-ono  ELSE
                    IF li EQ 3 THEN fi_q-comm ELSE
                    IF li EQ 4 THEN fi_q-back ELSE fi_q-avail.

                RUN sys/ref/convquom.p(item.cons-uom, "MSF",
                    item.basis-w,
                    (IF item.r-wid NE 0 THEN 0          ELSE item.s-len),
                    (IF item.r-wid NE 0 THEN item.r-wid ELSE item.s-wid),
                    0,
                    ld, OUTPUT ld).

                CASE li:
                    WHEN 1 THEN 
                        fi_m-onh   = ld.
                    WHEN 2 THEN 
                        fi_m-ono   = ld.
                    WHEN 3 THEN 
                        fi_m-comm  = ld.
                    WHEN 4 THEN 
                        fi_m-back  = ld.
                    WHEN 5 THEN 
                        fi_m-avail = ld.
                END CASE.
            END.

            DISPLAY fi_msf
                fi_m-onh
                fi_m-ono
                fi_m-comm
                fi_m-back
                fi_m-avail.
        END.

        ELSE
            ASSIGN
                fi_msf:HIDDEN     = YES
                fi_m-onh:HIDDEN   = YES
                fi_m-ono:HIDDEN   = YES
                fi_m-comm:HIDDEN  = YES
                fi_m-back:HIDDEN  = YES
                fi_m-avail:HIDDEN = YES.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-dims Dialog-Frame 
PROCEDURE set-dims :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            v-len = DEC(po-ordl.s-len:SCREEN-VALUE)
            v-wid = DEC(po-ordl.s-wid:SCREEN-VALUE)
            {po/calc10.i v-len}
            {po/calc10.i v-wid}.

        FIND FIRST ITEM NO-LOCK 
            WHERE item.company EQ cocode
            AND item.i-no    EQ po-ordl.i-no:SCREEN-VALUE
            NO-ERROR.

        ASSIGN
            v-basis-w = IF AVAILABLE ITEM THEN item.basis-w ELSE 0
            v-dep     = IF AVAILABLE ITEM THEN item.s-dep ELSE 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-shipto Dialog-Frame 
PROCEDURE update-shipto :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ll-choice AS LOG       NO-UNDO.
    DEFINE VARIABLE lv-stat   AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
   
        IF po-ordl.item-type:SCREEN-VALUE = "FG" THEN 
        DO:
     
            FIND FIRST xpo-ord NO-LOCK WHERE
                xpo-ord.company EQ g_company AND
                xpo-ord.po-no   EQ ip-ord-no
                NO-ERROR.
     
            IF AVAILABLE xpo-ord AND
                xpo-ord.TYPE = "D" THEN 
            DO:
          
                FOR EACH oe-rel FIELDS(company ord-no i-no ship-id)  NO-LOCK WHERE
                    oe-rel.company EQ g_company AND
                    oe-rel.ord-no = INT(po-ordl.ord-no:SCREEN-VALUE) AND
                    oe-rel.i-no = po-ordl.i-no:SCREEN-VALUE
                    :
     
                    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
     
                    IF LOOKUP(lv-stat,"S,I,L") = 0 THEN NEXT.
     
                    IF oe-rel.ship-id NE xpo-ord.ship-id THEN
                        MESSAGE "PO Shipto does not match Shipto on Order Release." SKIP
                            "Update Shipto?"
                            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-choice.
            
                    IF ll-choice THEN 
                    DO:
     
                        FIND FIRST shipto NO-LOCK WHERE shipto.company EQ g_company AND
                            shipto.cust-no EQ xpo-ord.cust-no AND
                            shipto.ship-id EQ oe-rel.ship-id
                            NO-ERROR.
           
                        IF AVAILABLE shipto THEN 
                        DO:
              
                            FIND FIRST xpo-ord EXCLUSIVE-LOCK WHERE
                                xpo-ord.company EQ g_company AND
                                xpo-ord.po-no   EQ ip-ord-no
                                NO-ERROR.
     
                            IF AVAILABLE xpo-ord THEN 
                            DO:
                
                                ASSIGN 
                                    xpo-ord.ship-id      = oe-rel.ship-id
                                    xpo-ord.ship-name    = shipto.ship-name
                                    xpo-ord.ship-addr[1] = shipto.ship-addr[1]
                                    xpo-ord.ship-addr[2] = shipto.ship-addr[2]
                                    xpo-ord.ship-city    = shipto.ship-city
                                    xpo-ord.ship-state   = shipto.ship-state
                                    xpo-ord.ship-zip     = shipto.ship-zip
                                    xpo-ord.ship-no      = shipto.ship-no.
     
                                FIND FIRST xpo-ord NO-LOCK WHERE
                                    xpo-ord.company EQ g_company AND
                                    xpo-ord.po-no   EQ ip-ord-no
                                    NO-ERROR.
                            END.
                
                            RELEASE shipto.
                        END.
                    END.
     
                    LEAVE.
                END.
     
                RELEASE xpo-ord.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-actnum Dialog-Frame 
PROCEDURE valid-actnum :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DO WITH FRAME {&FRAME-NAME}:
        IF (po-ordl.actnum:SCREEN-VALUE EQ "" OR
            NOT CAN-FIND(FIRST account
            WHERE account.company EQ g_company
            AND account.actnum  EQ po-ordl.actnum:SCREEN-VALUE
            AND account.TYPE <> "T")) AND
            v-default-gl-log                                                     THEN 
        DO:
            IF po-ordl.actnum:SCREEN-VALUE EQ "" THEN
                MESSAGE "Account Number may not be spaces, try help..." VIEW-AS ALERT-BOX ERROR.
            ELSE
                MESSAGE "Invalid GL#, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO po-ordl.actnum.
            RETURN ERROR.
        END.
    END.
  
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-b-num Dialog-Frame 
PROCEDURE valid-b-num :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-msg AS CHARACTER INIT "" NO-UNDO.
  
    RELEASE xpo-ordl.
    DO WITH FRAME {&FRAME-NAME}:
        IF po-ordl.job-no:SCREEN-VALUE NE ""  AND
            po-ordl.s-num:SCREEN-VALUE  NE "?" THEN 
        DO:
            po-ordl.job-no:SCREEN-VALUE =
                FILL(" ",6 - LENGTH(TRIM(po-ordl.job-no:SCREEN-VALUE))) +
                TRIM(po-ordl.job-no:SCREEN-VALUE).
            IF NOT ll-pojob-warned THEN
                FIND FIRST xpo-ordl NO-LOCK
                    WHERE xpo-ordl.company EQ g_company
                    AND xpo-ordl.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                    AND xpo-ordl.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                    AND xpo-ordl.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                    AND xpo-ordl.s-num   EQ INT(po-ordl.s-num:SCREEN-VALUE)
                    AND xpo-ordl.b-num   EQ INT(po-ordl.b-num:SCREEN-VALUE)
                    AND xpo-ordl.deleted EQ NO
                    AND ROWID(xpo-ordl)  NE ROWID(po-ordl)
                    AND xpo-ordl.po-no   NE po-ordl.po-no
                    AND CAN-FIND(FIRST xpo-ord
                    WHERE xpo-ord.company EQ xpo-ordl.company
                    AND xpo-ord.po-no   EQ xpo-ordl.po-no)
                    USE-INDEX ITEM NO-ERROR.
            IF AVAILABLE xpo-ordl THEN 
            DO:
                ll-ans = NO.
                MESSAGE "Purchase order " +
                    TRIM(STRING(xpo-ordl.po-no,">>>>>>>>")) +
                    " already exists for Job/Item/Sheet/Blank, continue?"
                    VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans.
              
                IF ll-ans THEN ll-pojob-warned = ll-ans.
                ELSE lv-msg          = "job-mat".
            END.
            IF lv-msg EQ "" AND
                CAN-FIND(FIRST xpo-ordl
                WHERE xpo-ordl.company EQ g_company
                AND xpo-ordl.po-no   EQ po-ordl.po-no
                AND xpo-ordl.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                AND xpo-ordl.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                AND xpo-ordl.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                AND xpo-ordl.s-num   EQ INT(po-ordl.s-num:SCREEN-VALUE)
                AND xpo-ordl.b-num   EQ INT(po-ordl.b-num:SCREEN-VALUE)
                AND RECID(xpo-ordl)  NE RECID(po-ordl))
                THEN lv-msg = "PO line item alreadys exists for this PO/Item/Job/Sheet/Blank".
            IF lv-msg EQ "" THEN 
            DO:
                IF TRIM(po-ordl.job-no:SCREEN-VALUE)  NE TRIM(lv-save-job)   OR
                    TRIM(po-ordl.job-no2:SCREEN-VALUE) NE TRIM(lv-save-job2)  OR
                    TRIM(po-ordl.s-num:SCREEN-VALUE)   NE TRIM(lv-save-s-num) OR
                    TRIM(po-ordl.b-num:SCREEN-VALUE)   NE TRIM(lv-save-b-num) THEN 
                DO:
                    IF TRIM(po-ordl.job-no:SCREEN-VALUE)  NE TRIM(lv-save-job)   OR
                        TRIM(po-ordl.job-no2:SCREEN-VALUE) NE TRIM(lv-save-job2)  THEN 
                    DO:
                        lv-save-s-num = po-ordl.s-num:SCREEN-VALUE.
                        RUN new-job.
                    END.
                    RUN display-job-mat.
                END.
                IF NOT CAN-FIND(FIRST job-mat
                    WHERE job-mat.company  EQ g_company
                    AND job-mat.job-no   EQ po-ordl.job-no:SCREEN-VALUE
                    AND job-mat.job-no2  EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                    AND job-mat.frm      EQ INT(po-ordl.s-num:SCREEN-VALUE)
                    AND job-mat.blank-no EQ INT(po-ordl.b-num:SCREEN-VALUE)
                    AND job-mat.rm-i-no  EQ po-ordl.i-no:SCREEN-VALUE)        AND
                    NOT CAN-FIND(FIRST tt-job-mat
                    WHERE tt-job-mat.company  EQ g_company
                    AND tt-job-mat.job-no   EQ po-ordl.job-no:SCREEN-VALUE
                    AND tt-job-mat.job-no2  EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                    AND tt-job-mat.frm      EQ INT(po-ordl.s-num:SCREEN-VALUE)
                    AND tt-job-mat.rm-i-no  EQ po-ordl.i-no:SCREEN-VALUE)     AND
                    NOT CAN-FIND(FIRST job-farm
                    WHERE job-farm.company  EQ g_company
                    AND job-farm.job-no   EQ po-ordl.job-no:SCREEN-VALUE
                    AND job-farm.job-no2  EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                    AND job-farm.frm      EQ INT(po-ordl.s-num:SCREEN-VALUE)
                    AND job-farm.blank-no EQ INT(po-ordl.b-num:SCREEN-VALUE)
                    AND job-farm.i-no     EQ po-ordl.i-no:SCREEN-VALUE)   
                    THEN 
                DO: 
                    lv-msg = "Invalid sheet/blank/item for job".
                END.
            END.
            IF lv-msg EQ "" THEN 
            DO:
                RUN valid-job-mat NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                DO: 
                    lv-msg = "job-mat".
                END.
            END.
        END.
        IF lv-msg NE "" THEN 
        DO:
            IF lv-msg NE "job-mat" THEN MESSAGE lv-msg + "..." VIEW-AS ALERT-BOX ERROR.
            IF po-ordl.b-num:SENSITIVE THEN APPLY "entry" TO po-ordl.b-num.
            ELSE
                IF po-ordl.s-num:SENSITIVE THEN APPLY "entry" TO po-ordl.s-num.
                ELSE
                    IF po-ordl.job-no2:SENSITIVE THEN APPLY "entry" TO po-ordl.job-no2.
                    ELSE                              APPLY "entry" TO po-ordl.job-no2.
            RETURN ERROR.
        END.
        lv-save-b-num = po-ordl.b-num:SCREEN-VALUE.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-mat Dialog-Frame 
PROCEDURE valid-job-mat :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-po-ordl FOR po-ordl.
    DEFINE BUFFER b-item    FOR item.
    DEFINE VARIABLE lv-msg     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ld-est-qty AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-ord-qty AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ll         AS LOG       NO-UNDO.
    DEFINE VARIABLE lv-dep     AS DECIMAL   NO-UNDO.
    IF NOT ll-poord-warned THEN 
    DO WITH FRAME {&FRAME-NAME}:
        IF po-ordl.job-no:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST job NO-LOCK 
                WHERE job.company EQ g_company
                AND job.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                AND job.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                NO-ERROR.
            FIND FIRST item NO-LOCK 
                WHERE item.company EQ g_company
                AND item.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                NO-ERROR.
            IF AVAILABLE ITEM THEN 
            DO:
                FOR EACH job-mat NO-LOCK
                    WHERE job-mat.company  EQ g_company
                    AND job-mat.job      EQ job.job
                    AND job-mat.job-no   EQ po-ordl.job-no:SCREEN-VALUE
                    AND job-mat.job-no2  EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                    AND (job-mat.frm     EQ INT(po-ordl.s-num:SCREEN-VALUE) OR
                    po-ordl.s-num:SCREEN-VALUE EQ "?")
                    ,
                    FIRST b-item NO-LOCK
                    WHERE b-item.company  EQ job-mat.company
                    AND b-item.i-no     EQ job-mat.rm-i-no
                    AND b-item.mat-type EQ item.mat-type
                    :
   
                    ld = job-mat.qty.
                    IF job-mat.qty-uom NE "EA" THEN
                        RUN sys/ref/convquom.p(job-mat.qty-uom, "EA",
                            b-item.basis-w,
                            job-mat.len,
                            job-mat.wid,
                            b-item.s-dep,
                            ld, OUTPUT ld).
                    ld-est-qty = ld-est-qty + ld.
                END.
                FOR EACH b-po-ordl NO-LOCK
                    WHERE b-po-ordl.company EQ g_company
                    AND b-po-ordl.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                    AND b-po-ordl.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                    AND b-po-ordl.s-num   EQ INT(po-ordl.s-num:SCREEN-VALUE)
                    AND ROWID(b-po-ordl)  NE ROWID(po-ordl)
                    ,
                    FIRST b-item  NO-LOCK
                    WHERE b-item.company  EQ b-po-ordl.company
                    AND b-item.i-no     EQ b-po-ordl.i-no
                    AND b-item.mat-type EQ item.mat-type
                    :
                    ld = b-po-ordl.ord-qty.
                    IF b-po-ordl.pr-qty-uom NE "EA" THEN
                        RUN sys/ref/convquom.p(b-po-ordl.pr-qty-uom, "EA",
                            b-item.basis-w,
                            b-po-ordl.s-len,
                            b-po-ordl.s-wid,
                            item.s-dep,
                            ld, OUTPUT ld).
                    ld-ord-qty = ld-ord-qty + ld.
                END.
                IF job.opened EQ NO                               OR
                    (ld-ord-qty GT 0 AND ld-ord-qty GE ld-est-qty) THEN 
                DO:
                    ASSIGN
                        lv-msg = IF job.opened EQ NO THEN "Job is closed"
                    ELSE "Material has already been ordered for this job"
                        ll     = NO.
                    MESSAGE TRIM(lv-msg) + ", continue with this new purchase order line?"
                        VIEW-AS ALERT-BOX WARNING BUTTON YES-NO
                        UPDATE ll.
                    IF ll THEN ll-poord-warned = YES.
                    ELSE RETURN ERROR.
                END.
            END.
        END.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no Dialog-Frame 
PROCEDURE valid-job-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE sv-poord-warned LIKE ll-poord-warned NO-UNDO.
    DO WITH FRAME {&frame-name}:
        IF TRIM(po-ordl.job-no:SCREEN-VALUE) NE ""                      OR
            (po-ordl.item-type:SCREEN-VALUE EQ "RM" AND
            CAN-FIND(FIRST item
            WHERE item.company EQ g_company
            AND item.i-no    EQ po-ordl.i-no:SCREEN-VALUE
            AND item.i-code  EQ "E"))                        THEN 
        DO:
            po-ordl.job-no:SCREEN-VALUE =
                FILL(" ",6 - LENGTH(TRIM(po-ordl.job-no:SCREEN-VALUE))) +
                TRIM(po-ordl.job-no:SCREEN-VALUE).
            IF TRIM(po-ordl.job-no:SCREEN-VALUE) EQ ""                            OR
                NOT CAN-FIND(FIRST job-hdr
                WHERE job-hdr.company EQ g_company
                AND job-hdr.job-no  EQ po-ordl.job-no:SCREEN-VALUE) THEN 
            DO:
                MESSAGE "Invalid Job, try help..."
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO po-ordl.job-no.
                RETURN ERROR.
            END.
            IF TRIM(po-ordl.job-no:SCREEN-VALUE) NE TRIM(lv-save-job) THEN 
            DO:
                ASSIGN
                    sv-poord-warned = ll-poord-warned
                    ll-poord-warned = YES
                    lv-save-job     = po-ordl.job-no:SCREEN-VALUE.
                RUN new-job.
                ll-poord-warned = sv-poord-warned.
                RUN enable-disable-frm.
                RUN enable-disable-blk.
                ASSIGN
                    lv-save-job2  = ""
                    lv-save-s-num = ""
                    lv-save-b-num = "".
            END.
            lv-save-job = po-ordl.job-no:SCREEN-VALUE.
        END.
        RUN enable-dISABLE-size.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no2 Dialog-Frame 
PROCEDURE valid-job-no2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER xpo-ordl FOR po-ordl.
    DO WITH FRAME {&frame-name}:
        IF po-ordl.job-no:SCREEN-VALUE NE "" THEN 
        DO:
            po-ordl.job-no:SCREEN-VALUE =
                FILL(" ",6 - LENGTH(TRIM(po-ordl.job-no:SCREEN-VALUE))) +
                TRIM(po-ordl.job-no:SCREEN-VALUE).
            IF NOT CAN-FIND(FIRST job-hdr
                WHERE job-hdr.company EQ g_company
                AND job-hdr.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                AND job-hdr.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE))
                THEN 
            DO:
                MESSAGE "Invalid Job, try help..."
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO po-ordl.job-no.
                RETURN ERROR.
            END.
            IF TRIM(po-ordl.job-no:SCREEN-VALUE)  NE TRIM(lv-save-job)   OR
                TRIM(po-ordl.job-no2:SCREEN-VALUE) NE TRIM(lv-save-job2)  THEN 
            DO:
                lv-save-job2 = po-ordl.job-no2:SCREEN-VALUE.
                RUN new-job.
                RUN enable-disable-frm.
                RUN enable-disable-blk.
                IF NOT po-ordl.s-num:SENSITIVE AND
                    NOT po-ordl.b-num:SENSITIVE THEN RUN display-job-mat.
                ELSE
                    ASSIGN
                        lv-save-s-num = ""
                        lv-save-b-num = "".
            END.
            lv-save-job2 = po-ordl.job-no2:SCREEN-VALUE.
            IF NOT po-ordl.s-num:SENSITIVE THEN 
            DO:
                RUN valid-s-num NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                DO:
                    APPLY "entry" TO po-ordl.job-no2.
                    RETURN ERROR.
                END.
            END.
        END.
        RUN enable-dISABLE-size.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-max-po-cost Dialog-Frame 
PROCEDURE valid-max-po-cost :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.
    DEFINE VARIABLE v-tot-cost AS DECIMAL DECIMALS 4 NO-UNDO.
    FIND FIRST xpo-ord NO-LOCK WHERE  
        xpo-ord.company EQ g_company AND
        xpo-ord.po-no   EQ ip-ord-no
        NO-ERROR.
    IF AVAILABLE xpo-ord AND xpo-ord.stat NE "H" THEN
    DO:
        FIND FIRST b-vend NO-LOCK WHERE
            b-vend.company EQ g_company AND
            b-vend.vend-no EQ xpo-ord.vend-no
            NO-ERROR.
        IF AVAILABLE b-vend AND b-vend.rebate-% NE 0 THEN
        DO:
            v-tot-cost = DECIMAL(po-ordl.t-cost:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
            FOR EACH xpo-ordl FIELDS(t-cost)  NO-LOCK WHERE
                xpo-ordl.company EQ g_company AND
                xpo-ordl.po-no EQ xpo-ord.po-no AND
                ROWID(xpo-ordl) NE ROWID(po-ordl)
                :
                v-tot-cost = v-tot-cost + xpo-ordl.t-cost.
            END.
            IF v-tot-cost GT b-vend.rebate-% THEN
            DO:
                MESSAGE "Purchase Order Cost Has Exceeded Vendor's Max P.O. Cost." SKIP
                    "Purchase Order Will Be Placed On Hold."
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                FIND CURRENT xpo-ord.
                xpo-ord.stat = "H".
                FIND CURRENT xpo-ord NO-LOCK NO-ERROR.
            END.
        END.
        FIND CURRENT xpo-ord.
        RELEASE xpo-ord.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-min-len Dialog-Frame 
PROCEDURE valid-min-len :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipValidate AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lv-cost    AS DECIMAL EXTENT 3 NO-UNDO.
    DEFINE VARIABLE li         AS INTEGER NO-UNDO.
    DEFINE VARIABLE lj         AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll         AS LOG     NO-UNDO.
    DEFINE VARIABLE ld         AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-hld-add LIKE addersText NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST e-item-vend NO-LOCK
            WHERE e-item-vend.company EQ cocode
            AND e-item-vend.i-no    EQ po-ordl.i-no:SCREEN-VALUE
            AND e-item-vend.vend-no EQ po-ord.vend-no 
            NO-ERROR.
  
        IF AVAILABLE e-item-vend THEN 
        DO:
        
            IF int(STRING(po-ordl.s-len:SCREEN-VALUE)) < e-item-vend.roll-w[29]  THEN 
            DO:
                MESSAGE "Sheet Length " + "(" + string(po-ordl.s-len:SCREEN-VALUE)  + ")" "Less than Vend Cost Size " + "(" + string(e-item-vend.roll-w[29])  + "), continue?" 
                    VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll.
                IF NOT ll THEN 
                DO:
                    APPLY "entry" TO po-ordl.s-len.
                    RETURN ERROR.
                END.
            END.
        END.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-min-wid Dialog-Frame 
PROCEDURE valid-min-wid :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipValidate AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lv-cost    AS DECIMAL EXTENT 3 NO-UNDO.
    DEFINE VARIABLE li         AS INTEGER NO-UNDO.
    DEFINE VARIABLE lj         AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll         AS LOG     NO-UNDO.
    DEFINE VARIABLE ld         AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-hld-add LIKE addersText NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST e-item-vend NO-LOCK
            WHERE e-item-vend.company EQ cocode
            AND e-item-vend.i-no    EQ po-ordl.i-no:SCREEN-VALUE
            AND e-item-vend.vend-no EQ po-ord.vend-no 
            NO-ERROR.
  
        IF AVAILABLE e-item-vend THEN 
        DO:
            IF int(STRING(po-ordl.s-wid:SCREEN-VALUE)) < e-item-vend.roll-w[27]  THEN 
            DO:
                MESSAGE "Sheet Width " + "(" + string(po-ordl.s-wid:SCREEN-VALUE)  + ")" "Less than Vend Cost Size " + "(" + string(e-item-vend.roll-w[27])  + "), continue?" 
                    VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll.
                IF NOT ll THEN 
                DO:
                    APPLY "entry" TO po-ordl.s-wid.
                    RETURN ERROR.
                END.
            END.
        END.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ord-no Dialog-Frame 
PROCEDURE valid-ord-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-po-ord FOR po-ord.
    DO WITH FRAME {&FRAME-NAME}:
        /* FINISHED GOODS */
        IF INT(po-ordl.ord-no:SCREEN-VALUE) NE 0 AND
            po-ordl.item-type:SCREEN-VALUE EQ "FG" THEN 
        DO:
            FIND FIRST oe-ordl NO-LOCK 
                WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no  EQ INT(po-ordl.ord-no:SCREEN-VALUE)
                AND oe-ordl.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                NO-ERROR.
            IF NOT AVAILABLE oe-ordl THEN
                FOR EACH oe-ordl NO-LOCK
                    WHERE oe-ordl.company EQ cocode
                    AND oe-ordl.ord-no  EQ INT(po-ordl.ord-no:SCREEN-VALUE),
                    FIRST itemfg NO-LOCK
                    WHERE itemfg.company EQ oe-ordl.company
                    AND itemfg.i-no    EQ oe-ordl.i-no
                    AND itemfg.isaset:
                        
                    RUN fg/fullset.p (ROWID(itemfg)).
                    IF CAN-FIND(FIRST tt-fg-set
                        WHERE tt-fg-set.part-no EQ po-ordl.i-no:SCREEN-VALUE)
                        THEN LEAVE.
                END.
                
            FOR EACH tt-fg-set:
                DELETE tt-fg-set.
            END.
            IF NOT AVAILABLE oe-ordl THEN 
            DO:
                APPLY "entry" TO po-ordl.ord-no.
                MESSAGE "FG Item# not on order, please try again..."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN ERROR.
            END.
            IF NOT ll-order-warned THEN 
            DO:
                FIND FIRST xpo-ordl NO-LOCK
                    WHERE xpo-ordl.company EQ cocode
                    AND xpo-ordl.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                    AND xpo-ordl.ord-no  EQ INT(po-ordl.ord-no:SCREEN-VALUE)
                    AND CAN-FIND(FIRST b-po-ord
                    WHERE b-po-ord.company EQ cocode
                    AND b-po-ord.po-no   EQ xpo-ordl.po-no)
                    AND RECID(xpo-ordl)  NE RECID(po-ordl)               
                    USE-INDEX item NO-ERROR.
                  
                ll-ans = NOT AVAILABLE xpo-ordl.
              
                IF NOT ll-ans THEN
                    MESSAGE "Purchase order " +
                        TRIM(STRING(xpo-ordl.po-no,">>>>>>>>")) +
                        " already exists for order/item, continue?"
                        VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans.
              
                IF NOT ll-ans THEN 
                DO:
                    APPLY "entry" TO po-ordl.ord-no.
                    RETURN ERROR.
                END.
                ELSE
                    ASSIGN
                        ll-order-warned              = YES
                        po-ordl.cust-no:SCREEN-VALUE = oe-ordl.cust-no
                        po-ordl.ord-no:SCREEN-VALUE  = STRING(oe-ordl.ord-no).
            END.
        END. /* FG END */
        ELSE 
        DO: /* RAW MATERIALS */
        
            FIND FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ cocode
                AND oe-ord.ord-no  EQ INT(po-ordl.ord-no:SCREEN-VALUE) NO-ERROR.
            IF AVAILABLE oe-ord 
                THEN
                ASSIGN
                    ll-order-warned              = YES
                    po-ordl.cust-no:SCREEN-VALUE = oe-ord.cust-no
                    po-ordl.ord-no:SCREEN-VALUE  = STRING(oe-ord.ord-no).
        END.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-num Dialog-Frame 
PROCEDURE valid-s-num :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&frame-name}:
        IF po-ordl.job-no:SCREEN-VALUE NE "" THEN 
        DO:
            po-ordl.job-no:SCREEN-VALUE =
                FILL(" ",6 - LENGTH(TRIM(po-ordl.job-no:SCREEN-VALUE))) +
                TRIM(po-ordl.job-no:SCREEN-VALUE).
            IF po-ordl.s-num:SCREEN-VALUE EQ "?" AND
                NOT CAN-FIND(FIRST tt-job-mat)    THEN RUN create-multi-line.
            IF po-ordl.s-num:SCREEN-VALUE NE "?" AND
                NOT CAN-FIND(FIRST job-mat
                WHERE job-mat.company EQ po-ordl.company
                AND job-mat.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                AND job-mat.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                AND job-mat.frm     EQ INT(po-ordl.s-num:SCREEN-VALUE)) AND
                NOT CAN-FIND(FIRST job-farm
                WHERE job-farm.company EQ po-ordl.company
                AND job-farm.i-no    EQ po-ordl.i-no :SCREEN-VALUE
                AND job-farm.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                AND job-farm.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                AND job-farm.frm     EQ INT(po-ordl.s-num:SCREEN-VALUE)) AND
                NOT CAN-FIND(FIRST tt-job-mat
                WHERE tt-job-mat.company  EQ po-ordl.company
                AND tt-job-mat.job-no   EQ po-ordl.job-no:SCREEN-VALUE
                AND tt-job-mat.job-no2  EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                AND tt-job-mat.frm      EQ INT(po-ordl.s-num:SCREEN-VALUE)
                AND tt-job-mat.rm-i-no  EQ po-ordl.i-no:SCREEN-VALUE) 
                THEN 
            DO:
                MESSAGE "Invalid sheet for job..."
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO po-ordl.s-num.
                RETURN ERROR.
            END.
            IF TRIM(po-ordl.job-no:SCREEN-VALUE)  NE TRIM(lv-save-job)   OR
                TRIM(po-ordl.job-no2:SCREEN-VALUE) NE TRIM(lv-save-job2)  OR
                TRIM(po-ordl.s-num:SCREEN-VALUE)   NE TRIM(lv-save-s-num) THEN 
            DO:
        
                IF TRIM(po-ordl.job-no:SCREEN-VALUE)  NE TRIM(lv-save-job)   OR
                    TRIM(po-ordl.job-no2:SCREEN-VALUE) NE TRIM(lv-save-job2)  THEN 
                DO:
                    lv-save-s-num = po-ordl.s-num:SCREEN-VALUE.
                    RUN new-job.
                END.
                RUN enable-disable-blk.
                IF NOT po-ordl.b-num:SENSITIVE THEN RUN display-job-mat.
                ELSE lv-save-b-num = "".
            END.
            lv-save-s-num = po-ordl.s-num:SCREEN-VALUE.
            IF NOT po-ordl.b-num:SENSITIVE THEN 
            DO:
                RUN valid-b-num NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                DO:
                    APPLY "entry" TO po-ordl.s-num.
                    RETURN ERROR.
                END.
            END.
        END.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sheet-board-proc Dialog-Frame 
PROCEDURE valid-sheet-board-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-po-ordl FOR po-ordl.
    DEFINE BUFFER b-item    FOR ITEM.
    DEFINE VARIABLE v-board-count AS INTEGER NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
   
        IF po-ord.TYPE EQ "S" AND po-ordl.item-type:SCREEN-VALUE = "RM" THEN
        DO:
            FIND FIRST b-item NO-LOCK WHERE 
                b-item.company EQ g_company AND
                b-item.i-no EQ po-ordl.i-no:SCREEN-VALUE AND
                b-item.mat-type = "B"
                NO-ERROR.
     
            IF AVAILABLE b-item THEN
            DO:
                FOR EACH b-po-ordl FIELDS(i-no) NO-LOCK WHERE
                    b-po-ordl.company EQ g_company AND
                    b-po-ordl.po-no EQ ip-ord-no AND
                    b-po-ordl.item-type EQ YES AND
                    ROWID(b-po-ordl) NE ROWID(po-ordl)
                    ,
                    FIRST ITEM NO-LOCK WHERE
                    ITEM.company EQ g_company AND
                    ITEM.i-no EQ b-po-ordl.i-no AND
                    ITEM.mat-type EQ "B"
                    :
           
                    v-board-count = v-board-count + 1.
                END.
           
                IF v-board-count GE 2 THEN
                DO:
                    MESSAGE "Maximum of 2 Board Items Allowed on Sheeting PO."
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                    RETURN ERROR.
                END.
            END.
        END.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom Dialog-Frame 
PROCEDURE valid-uom :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-field AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-uom   LIKE uom.uom NO-UNDO.
    DEFINE VARIABLE uom-list AS CHARACTER INIT "" NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        RELEASE item.
        lv-uom = IF ip-field EQ "pr-uom" THEN po-ordl.pr-uom:SCREEN-VALUE
        ELSE po-ordl.pr-qty-uom:SCREEN-VALUE. 
        IF po-ordl.item-type:SCREEN-VALUE EQ "RM" THEN
            FIND FIRST ITEM NO-LOCK 
                WHERE item.company EQ g_company
                AND item.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                NO-ERROR.
        IF AVAILABLE item THEN RUN sys/ref/uom-rm.p (item.mat-type, OUTPUT uom-list).
        ELSE RUN sys/ref/uom-fg.p (NO, OUTPUT uom-list). /* for fgitem */
        IF uom-list EQ "" THEN
            uom-list = IF ip-field EQ "pr-uom" THEN pr-uom-list
            ELSE lv-uom-list.
        IF AVAILABLE item AND INDEX("MOXY789@",ITEM.mat-type) GT 0 AND ip-field EQ "pr-uom" THEN
            uom-list = uom-list + ",L".
        IF po-ordl.item-type:SCREEN-VALUE NE "RM" THEN
            uom-list = uom-list + ",CS".
        IF LOOKUP(lv-uom,uom-list) LE 0 THEN 
        DO:
            MESSAGE "UOM must be " + TRIM(uom-list) VIEW-AS ALERT-BOX ERROR.
            IF ip-field EQ "pr-uom" THEN APPLY "entry" TO po-ordl.pr-uom.
            ELSE APPLY "entry" TO po-ordl.pr-qty-uom.
            RETURN ERROR.
        END.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend-cost Dialog-Frame 
PROCEDURE valid-vend-cost :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipValidate AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lv-cost    AS DECIMAL EXTENT 3 NO-UNDO.
    DEFINE VARIABLE li         AS INTEGER NO-UNDO.
    DEFINE VARIABLE lj         AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll         AS LOG     NO-UNDO.
    DEFINE VARIABLE ld         AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-hld-add LIKE addersText NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        IF po-ordl.job-no:SCREEN-VALUE NE "" THEN 
        DO:
            RELEASE job-mat.
            FIND FIRST job NO-LOCK 
                WHERE job.company EQ po-ordl.company
                AND job.job-no  EQ po-ordl.job-no:SCREEN-VALUE
                AND job.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
                NO-ERROR.
            IF AVAILABLE job THEN
                FIND FIRST job-mat NO-LOCK 
                    WHERE job-mat.company  EQ job.company
                    AND job-mat.job      EQ job.job
                    AND job-mat.job-no   EQ job.job-no
                    AND job-mat.job-no2  EQ job.job-no2
                    AND job-mat.frm      EQ INT(po-ordl.s-num:SCREEN-VALUE)
                    AND job-mat.blank-no EQ INT(po-ordl.b-num:SCREEN-VALUE)
                    AND job-mat.rm-i-no  EQ po-ordl.i-no:SCREEN-VALUE
                    NO-ERROR.
            IF AVAILABLE job-mat THEN 
            DO:
                RUN set-dims.
                IF job-mat.sc-uom EQ po-ordl.pr-uom:SCREEN-VALUE THEN
                    lv-cost[1] = job-mat.std-cost.
                ELSE
                    RUN sys/ref/convcuom.p(job-mat.sc-uom,
                        po-ordl.pr-uom:SCREEN-VALUE,
                        job-mat.basis-w,
                        job-mat.len,
                        job-mat.wid,
                        v-dep,
                        job-mat.std-cost,
                        OUTPUT lv-cost[1]).
                lv-hld-add = addersText.
                RUN po-adder2 (RECID(po-ordl),
                    RECID(job-mat),
                    "",
                    DEC(po-ordl.ord-qty:SCREEN-VALUE),
                    lv-cost[1],
                    0,
                    OUTPUT lv-cost[1],
                    OUTPUT lv-cost[2],
                    OUTPUT lv-cost[3]).
                addersText = lv-hld-add.
      
                IF ipValidate THEN 
                DO:
                    ll = DEC(po-ordl.cost:SCREEN-VALUE) LE
                        DEC(STRING(lv-cost[1],po-ordl.cost:FORMAT)).
                    IF NOT ll THEN
                        MESSAGE "Vendor Cost ("                              +
                            TRIM(po-ordl.cost:SCREEN-VALUE)              +
                            ") is higher than estimated ("               +
                            TRIM(STRING(lv-cost[1],po-ordl.cost:FORMAT)) +
                            "), continue?"
                            VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll.
                    IF NOT ll THEN 
                    DO:
                        APPLY "entry" TO po-ordl.cost.
                        RETURN ERROR.
                    END.
                END.
            END.
        END.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-all Dialog-Frame 
PROCEDURE validate-all :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DO WITH FRAME {&FRAME-NAME} :
        IF po-ordl.actnum:SCREEN-VALUE = "" AND v-default-gl-log 
            THEN 
        DO:
            MESSAGE "Account Number is mandatory. Try Help. " VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO po-ordl.actnum.
            RETURN ERROR.
        END.
    END.
   
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-i-no Dialog-Frame 
PROCEDURE validate-i-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        IF CAN-FIND(FIRST w-po-ordl) THEN ll-item-validated = YES.
        DO WITH FRAME {&FRAME-NAME}:
            RELEASE item.
            RELEASE itemfg.
            IF NOT ll-rm-fg-set AND ip-recid EQ ?                          AND
                CAN-FIND(FIRST item
                WHERE item.company EQ g_company
                AND item.i-no    EQ po-ordl.i-no:SCREEN-VALUE)   AND
                CAN-FIND(FIRST itemfg
                WHERE itemfg.company EQ g_company
                AND itemfg.i-no    EQ po-ordl.i-no:SCREEN-VALUE) THEN 
            DO:
                RUN windows/l-itmtyp.w (OUTPUT lv-itemtype).
                po-ordl.item-type:SCREEN-VALUE = lv-itemtype.
                ll-rm-fg-set = YES.
            END.
            IF po-ordl.item-type:SCREEN-VALUE EQ "RM" OR
                NOT CAN-FIND(FIRST itemfg
                WHERE itemfg.company EQ g_company
                AND itemfg.i-no    EQ po-ordl.i-no:SCREEN-VALUE) THEN
                FIND FIRST ITEM NO-LOCK
                    WHERE item.company EQ g_company
                    AND item.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                    NO-ERROR.
            IF po-ordl.item-type:SCREEN-VALUE EQ "FG" OR
                NOT CAN-FIND(FIRST item
                WHERE item.company EQ g_company
                AND item.i-no    EQ po-ordl.i-no:SCREEN-VALUE) THEN
                FIND FIRST itemfg NO-LOCK
                    WHERE itemfg.company EQ g_company
                    AND itemfg.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                    NO-ERROR.
            IF po-ordl.i-no:SCREEN-VALUE EQ ""       OR
                (NOT AVAILABLE item AND NOT AVAILABLE itemfg) THEN 
            DO:
                MESSAGE "Invalid item#, try help..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO po-ordl.i-no.
                RETURN ERROR.
            END.
            IF NOT ll-item-validated THEN 
            DO:
                IF AVAILABLE item THEN RUN display-rmitem(RECID(item)).
                ELSE RUN display-fgitem(RECID(itemfg)).
                IF po-ordl.item-type:SCREEN-VALUE EQ "FG" AND  po-ordl.i-no:SCREEN-VALUE NE "" THEN 
                DO:
                    RUN fg/GetItemfgActInact.p (INPUT cocode,
                        INPUT po-ordl.i-no:SCREEN-VALUE,
                        OUTPUT lActive).
                    /*         FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"                */
                    /*                               AND reftable.company  EQ cocode                    */
                    /*                               AND reftable.loc      EQ ""                        */
                    /*                               AND reftable.code     EQ po-ordl.i-no:SCREEN-VALUE */
                    /*                               AND reftable.code2 = "I"                           */
                    /*                             NO-LOCK NO-ERROR.                                    */
                    /*         IF AVAIL reftable THEN DO:                                               */
                    IF NOT lActive THEN 
                    DO:
                        MESSAGE po-ordl.i-no:SCREEN-VALUE + " has InActive Status. Purchase Order cannot be placed for the Inactive Item."
                            VIEW-AS ALERT-BOX ERROR.
                        APPLY "entry" TO po-ordl.i-no.
                        RETURN ERROR.
                    END.     
                END.
                ll-item-validated = YES.
                APPLY 'VALUE-CHANGED' TO po-ordl.ord-qty.
            END.
            RUN enable-dISABLE-size.
        END.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vend-cost Dialog-Frame 
PROCEDURE vend-cost :
    /*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-calc-cost AS LOG NO-UNDO.  
    RUN zero-vend-cost-related.
    EMPTY TEMP-TABLE tt-ei.
    EMPTY TEMP-TABLE tt-eiv.
    DO WITH FRAME {&FRAME-NAME}:
        RUN set-dims.
        /* for adders */
        RELEASE job-mat.
        FIND FIRST job NO-LOCK
            WHERE job.company EQ po-ordl.company
            AND job.job-no  EQ po-ordl.job-no:SCREEN-VALUE
            AND job.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
            NO-ERROR.
        IF AVAILABLE job THEN
            FIND FIRST job-mat NO-LOCK
                WHERE job-mat.company  EQ job.company
                AND job-mat.job      EQ job.job
                AND job-mat.job-no   EQ job.job-no
                AND job-mat.job-no2  EQ job.job-no2
                AND job-mat.frm      EQ INT(po-ordl.s-num:SCREEN-VALUE)
                AND job-mat.blank-no EQ INT(po-ordl.b-num:SCREEN-VALUE) 
                USE-INDEX seq-idx NO-ERROR.
        
        IF AVAILABLE job-mat THEN lv-recid = RECID(job-mat).
        v-ord-qty = DEC(po-ordl.ord-qty:SCREEN-VALUE).
        IF po-ordl.item-type:SCREEN-VALUE EQ "RM" THEN 
        DO:
            FIND FIRST e-item NO-LOCK
                WHERE e-item.company EQ cocode
                AND e-item.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                NO-ERROR.
      
            IF AVAILABLE e-item THEN 
            DO:
                CREATE tt-ei.
                ASSIGN 
                    tt-ei.std-uom = e-item.std-uom.
      
                FIND FIRST e-item-vend NO-LOCK
                    WHERE e-item-vend.company EQ e-item.company
                    AND e-item-vend.i-no    EQ e-item.i-no
                    AND e-item-vend.vend-no EQ po-ord.vend-no
                    NO-ERROR.
      
                IF AVAILABLE e-item-vend THEN 
                DO:
                    CREATE tt-eiv.
                    tt-eiv.rec_key = e-item-vend.rec_key.
                    DO v-index = 1 TO 10:
                        ASSIGN
                            tt-eiv.run-qty[v-index]  = e-item-vend.run-qty[v-index]
                            tt-eiv.run-cost[v-index] = e-item-vend.run-cost[v-index]
                            tt-eiv.setups[v-index]   = e-item-vend.setups[v-index].
                    END.
                    
         
                    IF AVAILABLE e-item-vend THEN
                    DO:
                        
             
                        DO v-index = 1 TO 10:
                            ASSIGN
                                tt-eiv.run-qty[v-index + 10]  = e-item-vend.runQtyXtra[v-index]
                                tt-eiv.run-cost[v-index + 10] = e-item-vend.runCostXtra[v-index]
                                tt-eiv.setups[v-index + 10]   = e-item-vend.setupsXtra[v-index].
                        END.
                    END.
                END.
            END.
        END.
        ELSE 
        DO:
            FIND FIRST e-itemfg NO-LOCK
                WHERE e-itemfg.company EQ cocode
                AND e-itemfg.i-no    EQ po-ordl.i-no:SCREEN-VALUE
                NO-ERROR.
            IF AVAILABLE e-itemfg THEN 
            DO:
                CREATE tt-ei.
                ASSIGN 
                    tt-ei.std-uom = e-itemfg.std-uom.
                RELEASE e-itemfg-vend . 
                IF po-ordl.cust-no:SCREEN-VALUE NE "" THEN
                    FIND FIRST e-itemfg-vend NO-LOCK
                        WHERE e-itemfg-vend.company EQ e-itemfg.company
                        AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                        AND e-itemfg-vend.vend-no EQ po-ord.vend-no
                        AND e-itemfg-vend.cust-no EQ po-ordl.cust-no:SCREEN-VALUE
                        AND e-itemfg-vend.est-no EQ "" /*23592 avoid farm Tab costs from estimating*/
                        NO-ERROR.  
                IF NOT AVAILABLE e-itemfg-vend AND po-ord.cust-no NE "" THEN
                    FIND FIRST e-itemfg-vend NO-LOCK
                        WHERE e-itemfg-vend.company EQ e-itemfg.company
                        AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                        AND e-itemfg-vend.vend-no EQ po-ord.vend-no
                        AND e-itemfg-vend.cust-no EQ po-ord.cust-no
                        AND e-itemfg-vend.est-no EQ "" /*23592 avoid farm Tab costs from estimating*/
                        NO-ERROR.

                IF NOT AVAILABLE e-itemfg-vend THEN 
                    FIND FIRST e-itemfg-vend NO-LOCK
                        WHERE e-itemfg-vend.company EQ e-itemfg.company
                        AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                        AND e-itemfg-vend.vend-no EQ po-ord.vend-no
                        AND e-itemfg-vend.est-no EQ "" /*23592 avoid Farm Tab costs from estimating*/
                        NO-ERROR.
                /* gdm - check for blank vendor */
                IF NOT AVAILABLE e-itemfg-vend THEN
                    FIND FIRST e-itemfg-vend NO-LOCK
                        WHERE e-itemfg-vend.company EQ e-itemfg.company
                        AND e-itemfg-vend.i-no    EQ e-itemfg.i-no 
                        AND e-itemfg-vend.vend-no EQ "" 
                        AND e-itemfg-vend.est-no eq ""
                        NO-ERROR.
                IF AVAILABLE e-itemfg-vend THEN 
                DO:            
                    CREATE tt-eiv.
                    tt-eiv.rec_key = e-itemfg-vend.rec_key.
                    DO v-index = 1 TO 10:
                        ASSIGN
                            tt-eiv.run-qty[v-index]  = e-itemfg-vend.run-qty[v-index]
                            tt-eiv.run-cost[v-index] = e-itemfg-vend.run-cost[v-index]
                            tt-eiv.setups[v-index]   = e-itemfg-vend.setups[v-index].
                    END.
                    RELEASE e-itemfg-vend.
                END.
            END.
        END. /* if item-type ne RM */
        IF AVAILABLE tt-eiv THEN 
        DO:                
            ASSIGN
                v-cost = 0 /*DEC(po-ordl.cost:SCREEN-VALUE)*/
                v-qty  = DEC(po-ordl.ord-qty:SCREEN-VALUE).
            IF tt-ei.std-uom NE po-ordl.pr-qty-uom:SCREEN-VALUE          AND
                (po-ordl.item-type                                        OR
                LOOKUP(tt-ei.std-uom,fg-uom-list)                  EQ 0 OR
                LOOKUP(po-ordl.pr-qty-uom:SCREEN-VALUE,fg-uom-list) EQ 0)  THEN 
            DO:
                IF po-ordl.pr-qty-uom:SCREEN-VALUE EQ "CS" AND po-ordl.item-type:SCREEN-VALUE NE "RM" THEN 
                DO:
                    /* First convert to EA */
                    v-qty = v-qty * INT(fiCount:SCREEN-VALUE).
                    /* Now convert to std-uom */
                    RUN sys/ref/convquom.p("EA",
                        tt-ei.std-uom, v-basis-w,
                        v-len, v-wid, v-dep,
                        v-qty, OUTPUT v-qty).
                END.
                ELSE
                    RUN sys/ref/convquom.p(po-ordl.pr-qty-uom:SCREEN-VALUE,
                        tt-ei.std-uom, v-basis-w,
                        v-len, v-wid, v-dep,
                        v-qty, OUTPUT v-qty).
            END.
            v-save-qty = v-qty.
            IF po-ordl.job-no:SCREEN-VALUE NE "" THEN
                RUN po/groupcst.p (po-ordl.job-no:SCREEN-VALUE,
                    INT(po-ordl.job-no2:SCREEN-VALUE),
                    po-ordl.i-no:SCREEN-VALUE,
                    INT(po-ordl.s-num:SCREEN-VALUE),
                    INT(po-ordl.b-num:SCREEN-VALUE),
                    INPUT-OUTPUT v-qty).
            ASSIGN
                v-save-qty = v-qty - v-save-qty
                v-setup    = 0
                v-pb-qty   = 0.
            
            RUN est/dim-charge.p (tt-eiv.rec_key,
                v-wid,
                v-len,
                INPUT-OUTPUT ld-dim-charge).
     
            DO li = 1 TO EXTENT(tt-eiv.run-qty):
                IF tt-eiv.run-qty[li] LT v-qty THEN NEXT.
                ASSIGN
                    v-cost   = (tt-eiv.run-cost[li] + ld-dim-charge) * v-qty
                    v-setup  = tt-eiv.setups[li]
                    v-pb-qty = tt-eiv.run-qty[li] - v-save-qty.
                IF li LT EXTENT(tt-eiv.run-qty) THEN
                    ASSIGN
                        v-pb-cst = tt-eiv.run-cost[li + 1] + ld-dim-charge
                        v-pb-stp = tt-eiv.setups[li + 1].
                LEAVE.
            END.
            IF poqty-log THEN 
            DO:
                IF v-pb-qty GE 9999999 THEN v-pb-qty = 0.
                IF v-pb-qty EQ 0 THEN v-pb-cst = 0.
                ELSE 
                DO:
                    v-pb-qty = v-pb-qty + .001.
                    v-pb-cst = v-pb-cst * v-pb-qty.
                    IF v-pb-qty NE 0 THEN v-pb-cst = (v-pb-cst /*+ v-pb-stp*/) / v-pb-qty.  
                    ELSE v-pb-cst = (v-pb-cst /*+ v-pb-stp*/).
                END.
                IF tt-ei.std-uom NE po-ordl.pr-qty-uom:SCREEN-VALUE           AND
                    (po-ordl.item-type                                        OR
                    LOOKUP(tt-ei.std-uom,fg-uom-list)                  EQ 0 OR
                    LOOKUP(po-ordl.pr-qty-uom:SCREEN-VALUE,fg-uom-list) EQ 0)  THEN 
                DO:
                    IF po-ordl.pr-qty-uom:SCREEN-VALUE EQ "CS" AND po-ordl.item-type:SCREEN-VALUE NE "RM" THEN 
                    DO:
                        /* convert to EA */
                        RUN sys/ref/convquom.p(tt-ei.std-uom,
                            "EA",
                            v-basis-w, v-len, v-wid, v-dep,
                            v-pb-qty, OUTPUT v-pb-qty).
                        /* Then Convert to CS */
                        v-pb-qty = v-pb-qty / INT(fiCount:SCREEN-VALUE).
                    END.
              
                    ELSE
                        RUN sys/ref/convquom.p(tt-ei.std-uom,
                            po-ordl.pr-qty-uom:SCREEN-VALUE,
                            v-basis-w, v-len, v-wid, v-dep,
                            v-pb-qty, OUTPUT v-pb-qty).
                END.
                IF tt-ei.std-uom NE po-ordl.pr-uom:SCREEN-VALUE           AND
                    (po-ordl.item-type                                    OR
                    LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
                    LOOKUP(po-ordl.pr-uom:SCREEN-VALUE,fg-uom-list) EQ 0)  THEN 
                DO:
                    IF po-ordl.pr-uom:SCREEN-VALUE EQ "CS" AND po-ordl.item-type:SCREEN-VALUE NE "RM" THEN 
                    DO:
                        /* Convert to EA cost */
                        RUN sys/ref/convcuom.p(tt-ei.std-uom,
                            "EA", v-basis-w,
                            v-len, v-wid, v-dep,
                            v-pb-cst, OUTPUT v-pb-cst).
                        /* Convert to CS */
                        v-pb-cst = v-pb-cst * INT(fiCount:SCREEN-VALUE).
                    END.
                    ELSE
                        RUN sys/ref/convcuom.p(tt-ei.std-uom,
                            po-ordl.pr-uom:SCREEN-VALUE, v-basis-w,
                            v-len, v-wid, v-dep,
                            v-pb-cst, OUTPUT v-pb-cst).
                END.
                IF po-ordl.pr-uom:SCREEN-VALUE NE po-ordl.cons-uom:SCREEN-VALUE AND
                    (po-ordl.item-type                                      OR
                    LOOKUP(po-ordl.pr-uom:SCREEN-VALUE,fg-uom-list)   EQ 0 OR
                    LOOKUP(po-ordl.cons-uom:SCREEN-VALUE,fg-uom-list) EQ 0)     THEN 
                DO:
                    IF po-ordl.pr-uom:SCREEN-VALUE EQ "CS" AND po-ordl.item-type:SCREEN-VALUE NE "RM" THEN 
                    DO:
                        /* Convert Cases to EA */
                        v-pb-cst = v-pb-cst * INT(fiCount:SCREEN-VALUE).
                        /* Convert EA to cons-uom */
                        RUN sys/ref/convcuom.p("EA",
                            po-ordl.cons-uom:SCREEN-VALUE, v-basis-w,
                            v-len, v-wid, v-dep,
                            v-pb-cst, OUTPUT v-pb-cns).
                    END.
                    ELSE
                        RUN sys/ref/convcuom.p(po-ordl.pr-uom:SCREEN-VALUE,
                            po-ordl.cons-uom:SCREEN-VALUE, v-basis-w,
                            v-len, v-wid, v-dep,
                            v-pb-cst, OUTPUT v-pb-cns).
                END.
                fi_pb-qty:SCREEN-VALUE = IF v-pb-qty LE 0 THEN "" ELSE STRING(v-pb-qty).
            END.
            /*assumes v-qty in same uom as v-cost*/
            IF v-qty <> 0 THEN v-cost = (v-cost /*+ v-setup*/) / v-qty.  
            ELSE v-cost = (v-cost /*+ v-setup*/).
            IF ip-calc-cost NE ? THEN 
            DO:
                IF ip-calc-cost THEN 
                DO:            
                    IF tt-ei.std-uom NE po-ordl.pr-uom:SCREEN-VALUE           AND
                        (po-ordl.item-type                                    OR
                        LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
                        LOOKUP(po-ordl.pr-uom:SCREEN-VALUE,fg-uom-list) EQ 0)  THEN 
                    DO:
                        /* IF 'CS' then convert to EA first */
                        RUN sys/ref/convcuom.p(tt-ei.std-uom,
                            IF po-ordl.pr-uom:SCREEN-VALUE NE "CS" THEN
                            po-ordl.pr-uom:SCREEN-VALUE ELSE "EA", 
                            v-basis-w,
                            (IF po-ordl.pr-qty-uom:SCREEN-VALUE EQ "ROLL" THEN 12 ELSE v-len),
                            v-wid, v-dep,
                            v-cost, OUTPUT v-cost).
                        /* If cases, convert from EA to CS */
                        IF po-ordl.pr-uom:SCREEN-VALUE EQ "CS" AND po-ordl.item-type:SCREEN-VALUE NE "RM" THEN
                            v-cost = v-cost * INT(fiCount:SCREEN-VALUE).
                    END.
                    ASSIGN
                        ip-calc-cost               = YES
                        po-ordl.cost:SCREEN-VALUE  = STRING(v-cost,po-ordl.cost:FORMAT)
                        po-ordl.setup:SCREEN-VALUE = STRING(v-setup,po-ordl.setup:FORMAT).
                    IF po-ordl.pr-uom:SCREEN-VALUE NE po-ordl.cons-uom:SCREEN-VALUE AND
                        (po-ordl.item-type                                      OR
                        LOOKUP(po-ordl.pr-uom:SCREEN-VALUE,fg-uom-list)   EQ 0 OR
                        LOOKUP(po-ordl.cons-uom:SCREEN-VALUE,fg-uom-list) EQ 0)     THEN 
                    DO:
                        /* Convert cost from CS to EA first */
                        IF po-ordl.pr-uom:SCREEN-VALUE EQ "CS" AND po-ordl.item-type:SCREEN-VALUE NE "RM" THEN
                            v-cost = v-cost / INT(fiCount:SCREEN-VALUE).
           
                        RUN sys/ref/convcuom.p(IF po-ordl.pr-uom:SCREEN-VALUE NE "CS" THEN
                            po-ordl.pr-uom:SCREEN-VALUE ELSE "EA",
                            po-ordl.cons-uom:SCREEN-VALUE, v-basis-w,
                            (IF po-ordl.pr-qty-uom:SCREEN-VALUE EQ "ROLL" THEN 12 ELSE v-len),
                            v-wid, v-dep,
                            v-cost, OUTPUT v-cost).           
                    END.
                    po-ordl.cons-cost:SCREEN-VALUE = STRING(v-cost,po-ordl.cons-cost:FORMAT).     
          
                END. /* if calc cost */
                ELSE
                    IF v-hold-op1 AND po-ord.stat NE "H" THEN 
                    DO:
                        IF tt-ei.std-uom NE po-ordl.pr-uom:SCREEN-VALUE           AND
                            (po-ordl.item-type                                    OR
                            LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
                            LOOKUP(po-ordl.pr-uom:SCREEN-VALUE,fg-uom-list) EQ 0)  THEN 
                        DO:
                            /* If CS, convert to EA first */
                            RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                IF po-ordl.pr-uom:SCREEN-VALUE NE "CS" THEN
                                po-ordl.pr-uom:SCREEN-VALUE ELSE "EA", 
                                v-basis-w,
                                v-len, v-wid, v-dep,
                                v-cost, OUTPUT v-cost).    
                            /* Convert cost from EA to CS */
                            IF po-ordl.pr-uom:SCREEN-VALUE EQ "CS" AND po-ordl.item-type:SCREEN-VALUE NE "RM" THEN
                                v-cost = v-cost * INT(fiCount:SCREEN-VALUE).
                        END.          
                        IF AVAILABLE job-mat THEN
                            RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                                DEC(po-ordl.ord-qty:SCREEN-VALUE),
                                v-cost,
                                DEC(po-ordl.cons-cost:SCREEN-VALUE),
                                OUTPUT v-cost,
                                OUTPUT lv-added-cons-cost,
                                OUTPUT lv-adder-setup).
                        IF DEC(po-ordl.cost:SCREEN-VALUE) GT v-cost THEN 
                        DO:
                            FIND CURRENT po-ord.
                            po-ord.stat = "H".
                            FIND CURRENT po-ord NO-LOCK.
                        END.          
                    END. /* If not calc cost and stat ne "H" */
            END. /* ip calc cost ne ? */
        END. /* avail tt-eiv */
        IF AVAILABLE job-mat THEN 
        DO:
            IF poqty-log THEN
                RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                    DEC(fi_pb-qty:SCREEN-VALUE),
                    v-pb-cst,
                    v-pb-cns,
                    OUTPUT v-pb-cst,
                    OUTPUT v-pb-cns,
                    OUTPUT lv-adder-setup).
            RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                DEC(po-ordl.ord-qty:SCREEN-VALUE),
                DEC(po-ordl.cost:SCREEN-VALUE),
                DEC(po-ordl.cons-cost:SCREEN-VALUE),
                OUTPUT lv-added-cost,
                OUTPUT lv-added-cons-cost,
                OUTPUT lv-adder-setup).
            IF ip-calc-cost THEN do:
                ASSIGN
                    po-ordl.cost:SCREEN-VALUE      = STRING(lv-added-cost)
                    po-ordl.cons-cost:SCREEN-VALUE = STRING(lv-added-cons-cost ) .
                    IF lv-adder-setup GT 0 THEN
                    po-ordl.setup:SCREEN-VALUE  = STRING( DEC(po-ordl.setup:SCREEN-VALUE) + ( IF lv-adder-setup NE ? THEN lv-adder-setup ELSE 0) ) .
            END.
        END.
        IF poqty-log THEN 
        DO:
            IF CAN-DO("L,LOT",po-ordl.pr-uom:SCREEN-VALUE) THEN
                lv-t-cost = (v-pb-cst + v-pb-stp) *
                    IF po-ordl.ord-qty LT 0 THEN -1 ELSE 1.
            ELSE 
            DO:
                v-ord-qty = DEC(fi_pb-qty:SCREEN-VALUE).
                IF po-ordl.pr-qty-uom:SCREEN-VALUE NE po-ordl.pr-uom:SCREEN-VALUE AND
                    (po-ordl.item-type                                        OR
                    LOOKUP(po-ordl.pr-qty-uom:SCREEN-VALUE,fg-uom-list) EQ 0 OR
                    LOOKUP(po-ordl.pr-uom:SCREEN-VALUE,fg-uom-list)     EQ 0)     THEN
   
                    RUN sys/ref/convquom.p(po-ordl.pr-qty-uom:SCREEN-VALUE,
                        po-ordl.pr-uom:SCREEN-VALUE,
                        v-basis-w, v-len, v-wid, v-dep,
                        v-ord-qty, OUTPUT v-ord-qty).
     
                lv-t-cost = (v-ord-qty * v-pb-cst) + v-pb-stp.
            END.
            IF DEC(po-ordl.disc:SCREEN-VALUE) NE 0 THEN
                lv-t-cost = lv-t-cost * (1 - (DEC(po-ordl.disc:SCREEN-VALUE) / 100)).
            fi_pb-cst:SCREEN-VALUE = STRING(lv-t-cost).
            IF DEC(fi_pb-cst:SCREEN-VALUE) LE 0 THEN fi_pb-cst:SCREEN-VALUE = "".
        END.
        IF ip-calc-cost NE ? THEN 
        DO:
            IF CAN-DO("L,LOT",po-ordl.pr-uom:SCREEN-VALUE) THEN
                lv-t-cost = (DEC(po-ordl.cost:SCREEN-VALUE) +
                    DEC(po-ordl.setup:SCREEN-VALUE)) *
                    IF po-ordl.ord-qty LT 0 THEN -1 ELSE 1.
            ELSE 
            DO:
                v-ord-qty = DEC(po-ordl.ord-qty:SCREEN-VALUE).
                IF po-ordl.pr-qty-uom:SCREEN-VALUE NE po-ordl.pr-uom:SCREEN-VALUE AND
                    (po-ordl.item-type                                        OR
                    LOOKUP(po-ordl.pr-qty-uom:SCREEN-VALUE,fg-uom-list) EQ 0 OR
                    LOOKUP(po-ordl.pr-uom:SCREEN-VALUE,fg-uom-list)     EQ 0)     THEN 
                DO:
       
                    IF po-ordl.pr-qty-uom:SCREEN-VALUE EQ "CS" OR po-ordl.pr-uom:SCREEN-VALUE EQ "CS" THEN 
                    DO:
                        RUN convertCSCost.
                    END.
                    ELSE 
                        RUN sys/ref/convquom.p(po-ordl.pr-qty-uom:SCREEN-VALUE,
                            po-ordl.pr-uom:SCREEN-VALUE,
                            v-basis-w, v-len, v-wid, v-dep,
                            v-ord-qty, OUTPUT v-ord-qty).
                END.
                lv-t-cost = (v-ord-qty * DEC(po-ordl.cost:SCREEN-VALUE)) +
                    DEC(po-ordl.setup:SCREEN-VALUE).
            END.
            IF DEC(po-ordl.disc:SCREEN-VALUE) NE 0 THEN
                lv-t-cost = lv-t-cost * (1 - (DEC(po-ordl.disc:SCREEN-VALUE) / 100)).
            po-ordl.t-cost:SCREEN-VALUE = STRING(lv-t-cost).
        END.
    END.  
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE writeJobFarmInfo Dialog-Frame 
PROCEDURE writeJobFarmInfo :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bfJob-farm FOR job-farm.
    DEFINE BUFFER bfJob-hdr  FOR job-hdr.
    DEFINE BUFFER bfItemfg   FOR itemfg.
    DEFINE VARIABLE dQtyEa     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostM     LIKE po-ordl.cost.
    DEFINE VARIABLE lHeadFound AS LOG       NO-UNDO.
    DEFINE VARIABLE lcCust-NO  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJob       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lFromOrd   AS LOG       NO-UNDO.
    ASSIGN 
        lHeadFound = NO
        cJob       = ""
        iJobNo2    = 0
        lFromOrd   = FALSE.
    DO WITH FRAME {&FRAME-NAME}:
    
        IF po-ordl.job-no:SCREEN-VALUE GT "" THEN
            ASSIGN cJob    = po-ordl.job-no:SCREEN-VALUE
                iJobNo2 = INT(po-ordl.job-no2:SCREEN-VALUE).
        ELSE IF po-ordl.ord-no:SCREEN-VALUE GT "" THEN  
            DO:
      
                FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ g_company
                    AND oe-ordl.ord-no EQ INTEGER(po-ordl.ord-no:SCREEN-VALUE)
                    AND oe-ordl.i-no   EQ  po-ordl.i-no:SCREEN-VALUE
                    NO-ERROR.
                /* assumption is that for farm jobs, order and job are always the same */
                IF NOT AVAILABLE oe-ordl THEN
                    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ g_company
                        AND oe-ordl.ord-no EQ INTEGER(po-ordl.ord-no:SCREEN-VALUE)
                        AND oe-ordl.job-no   EQ  po-ordl.ord-no:SCREEN-VALUE
                        NO-ERROR.
                lFromOrd = TRUE.
                IF AVAILABLE oe-ordl AND oe-ordl.job-no GT "" THEN
                    ASSIGN cJob    = oe-ordl.job-no
                        iJobNo2 = oe-ordl.job-no2.
      
            END.
        IF cJob EQ "" THEN
            RETURN.
        FIND FIRST bfJob-farm EXCLUSIVE-LOCK  
            WHERE bfJob-farm.company EQ g_company 
            AND bfJob-farm.job-no  EQ cJob 
            AND bfJob-farm.job-no2 EQ iJobNo2 
            AND bfJob-farm.i-no EQ po-ordl.i-no:SCREEN-VALUE 
            AND (bfJob-farm.frm     EQ INT(po-ordl.s-num:SCREEN-VALUE) OR lFromOrd)
            AND (bfJob-farm.blank-no EQ INT(po-ordl.b-num:SCREEN-VALUE) OR lFromOrd)
            NO-ERROR.
  
        IF AVAILABLE bfJob-Farm THEN 
        DO:
            dQtyEa = po-ordl.ord-qty.
            IF po-ordl.pr-qty-uom NE "EA" THEN
                RUN sys/ref/convquom.p(INPUT po-ordl.pr-qty-uom,
                    INPUT "EA", INPUT 0,
                    INPUT po-ordl.s-len,
                    INPUT po-ordl.s-wid,
                    INPUT 0,
                    INPUT po-ordl.ord-qty,
                    OUTPUT dQtyEa).
            dCostM = po-ordl.cost.
            IF po-ordl.pr-uom NE "M" THEN
                RUN sys/ref/convcuom.p(po-ordl.pr-uom, "M", 0, 0, 0, 0,
                    po-ordl.cost, OUTPUT dCostM). 
    
            ASSIGN
                bfJob-farm.po-no       = STRING(po-ordl.po-no)
                /* bfJob-farm.vend-po-qty = po-ordl.qty */
                bfJob-farm.qty         = dQtyEa 
                bfJob-farm.vend-po-qty = dQtyEa
                bfJob-farm.pur-uom     = po-ordl.pr-uom
                bfJob-farm.po-cost     = dCostM
                bfJob-farm.po-setup    = po-ordl.setup     . 
        /*       bfJob-farm.act-tot-cost = (dCostM * dQtyEa) / 1000 + po-ordl.setup                    */
        /*       bfJob-farm.act-cost     = ((dQtyEa / 1000 * dCostM) + po-ordl.setup) / dQtyEa * 1000. */
        /*                                                                                             */
        /*     IF po-ordl.pr-uom NE "M" THEN DO:                                                  */
        /*                                                                                        */
        /*       run sys/ref/convcuom.p("M",po-ordl.pr-uom , 0, 0, 0, 0,                          */
        /*                              bfJob-farm.act-tot-cost, output bfJob-farm.act-tot-cost). */
        /*                                                                                        */
        /*       run sys/ref/convcuom.p("M",po-ordl.pr-uom , 0, 0, 0, 0,                          */
        /*                              bfJob-farm.act-cost, output bfJob-farm.act-cost).         */
        /*      END.                                                                              */
       
        END. /* avail bfJob-farm */
    END. /* do */
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE zero-vend-cost-related Dialog-Frame 
PROCEDURE zero-vend-cost-related :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    ASSIGN  
        v-qty              = 0
        v-cost             = 0
        v-pb-qty           = 0
        v-pb-stp           = 0
        v-pb-cst           = 0
        v-pb-cns           = 0
        v-save-qty         = 0
        v-setup            = 0
        li                 = 0
        lv-added-cost      = 0
        lv-added-cons-cost = 0
        lv-adder-setup     = 0
        lv-recid           = 0
        lv-t-cost          = 0
        ld-dim-charge      = 0
        v-index            = 0.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME             

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-job-bnum Dialog-Frame 
PROCEDURE check-job-bnum :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-job-no LIKE po-ordl.job-no NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:

        lv-job-no = FILL(" ", 6 - LENGTH(TRIM(po-ordl.job-no:SCREEN-VALUE))) +
            TRIM(po-ordl.job-no:SCREEN-VALUE).

        RELEASE job-mat.

        FIND FIRST job NO-LOCK 
            WHERE job.company EQ g_company
            AND job.job-no  EQ lv-job-no
            AND job.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE)
            NO-ERROR.

        IF lv-job-no NE "" AND AVAILABLE job THEN
            FIND FIRST job-mat NO-LOCK 
                WHERE job-mat.company  EQ g_company
                AND job-mat.job      EQ job.job
                AND job-mat.job-no   EQ job.job-no
                AND (job-mat.job-no2 EQ INT(po-ordl.job-no2:SCREEN-VALUE) OR FOCUS:NAME EQ "job-no")
                AND job-mat.rm-i-no  EQ po-ordl.i-no:SCREEN-VALUE
                NO-ERROR.
    END.

    IF AVAILABLE job-mat THEN 
    DO WITH FRAME {&FRAME-NAME}:
        IF job-mat.rm-i-no  NE po-ordl.i-no:SCREEN-VALUE            OR
            job-mat.job-no   NE po-ordl.job-no:SCREEN-VALUE          OR
            job-mat.job-no2  NE INT(po-ordl.job-no2:SCREEN-VALUE)    OR
            (job-mat.frm     NE INT(po-ordl.s-num:SCREEN-VALUE) AND
            po-ordl.s-num:SCREEN-VALUE NE "?")                      OR
            job-mat.blank-no NE INT(po-ordl.b-num:SCREEN-VALUE)      OR
            ll-new-job-mat                                           THEN 
        DO:
     
            ASSIGN 
                po-ordl.s-num:SCREEN-VALUE = STRING(job-mat.frm)
                po-ordl.b-num:SCREEN-VALUE = STRING(job-mat.blank-no)
                ll-new-job-mat             = NO .
        END.
    END.



END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME    
