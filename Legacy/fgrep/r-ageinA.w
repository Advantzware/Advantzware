&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fgrep\r-ageinv.w

  Description:  Aged Inventory
    - Run-report --> Build-report --> produce report
    - Build temp-table per fg-bin record
    - Process each individual transaction using calc_date procedure
      and put the quantity into one bucket
    - first-date-for-tag finds the earliest date for the quantity up to 
      that point
    - find-receipt processes the date for a quantity in the same way as
      IR2 to ensure the same result
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
DEF VAR list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

DEFINE VARIABLE lv-case-count AS INT NO-UNDO.
DEFINE VARIABLE v-tot-for-item AS INT NO-UNDO.
DEFINE VARIABLE v-tot-for-item-tags AS INT NO-UNDO.
DEFINE VARIABLE v-custom AS LOG NO-UNDO.
DEF NEW SHARED VAR age-days AS INT EXTENT 4 NO-UNDO.
DEF VAR v-q AS INT NO-UNDO.   
DEF VAR v-tot-positive AS INT NO-UNDO.
DEF VAR v-print-sls-tots AS LOG NO-UNDO.
DEF VAR v-print-grand-tots AS LOG NO-UNDO.
DEF VAR v-custom-user AS CHARACTER NO-UNDO.
DEF VAR IR12-log AS LOGICAL NO-UNDO.
DEF VAR IR12-char AS CHARACTER NO-UNDO.
DEF VAR v-rtn-char AS CHARACTER NO-UNDO.
DEF VAR v-rec-found AS LOG NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

/*{sys/inc/custlistform.i ""IR12"" }*/

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

RUN sys/ref/nk1look.p (cocode, "IR12", "L", NO, NO, "", "", 
                   OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    IR12-log = LOGICAL(v-rtn-char).
ELSE
    IR12-log = NO.

RUN sys/ref/nk1look.p (cocode, "IR12", "C", NO, NO, "", "", 
                   OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
IR12-char = v-rtn-char.

/* Default */
v-custom-user = USERID("NOSWEAT").

DEF VAR v-num-saved AS INT.
DEF NEW SHARED TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin
                   FIELD first-date AS DATE
                   FIELD aged-qty   AS DEC EXTENT 6.

DEF BUFFER bf-tt-fg-bin FOR tt-fg-bin.
DEF NEW SHARED TEMP-TABLE tt-file NO-UNDO
                       FIELD tt-sman LIKE sman.sman
                       FIELD tt-cust-no LIKE cust.cust-no
                       FIELD tt-i-no LIKE itemfg.i-no
                       FIELD tt-qohi AS DEC EXTENT 5
                       FIELD tt-cst AS DEC EXTENT 4
                       FIELD tt-val AS DEC EXTENT 4
                       FIELD tt-sell-price AS DEC EXTENT 5
                       FIELD tt-days AS INT
                       FIELD tt-loc AS CHARACTER .
DEF TEMP-TABLE tt-items 
    FIELD i-no LIKE itemfg.i-no
    FIELD job-no LIKE job.job-no
    FIELD job-no2 LIKE job.job-no2
    FIELD first-date AS DATE
    FIELD first-qty AS INT
    INDEX i1 i-no.

DEF NEW SHARED TEMP-TABLE tt-itemfg NO-UNDO
    FIELD row-id      AS   ROWID
    FIELD i-no        LIKE itemfg.i-no
    FIELD cust-no     LIKE itemfg.cust-no
    FIELD part-no     LIKE itemfg.part-no
    FIELD part-cust   AS   CHAR
    FIELD procat      LIKE itemfg.procat
    FIELD job-no      LIKE fg-rcpth.job-no
    FIELD job-no2     LIKE fg-rcpth.job-no2
    FIELD loc         LIKE fg-rdtlh.loc
    FIELD loc-bin     LIKE fg-rdtlh.loc-bin
    FIELD tag         LIKE fg-rdtlh.tag
    FIELD bin-cust-no LIKE fg-rdtlh.cust-no
    FIELD loc-bin-tag AS   CHAR
    INDEX i-no  i-no job-no job-no2 loc loc-bin tag bin-cust-no
    INDEX cust-no cust-no  i-no job-no job-no2 loc loc-bin tag bin-cust-no
    INDEX part-no part-cust  i-no job-no job-no2 loc loc-bin tag bin-cust-no
    INDEX procat procat  i-no job-no job-no2 loc loc-bin tag bin-cust-no
    INDEX loc-bin-tag loc-bin-tag  i-no job-no job-no2 loc loc-bin tag bin-cust-no.

DEF TEMP-TABLE tt-bin-date
  FIELD bin-row AS ROWID
  FIELD tag AS CHAR
  FIELD job-no LIKE job.job-no
  FIELD job-no2 LIKE job.job-no2
  FIELD start-qty AS INT
  FIELD rec-date AS DATE
  FIELD run-date AS DATE EXTENT 2000
  FIELD run-qty  AS INT EXTENT 2000
  FIELD i-no LIKE itemfg.i-no
  INDEX i1 bin-row start-qty
  INDEX i2 i-no.

DEF TEMP-TABLE tt-fghist
     FIELD rcpth-row AS ROWID
     FIELD rdtlh-row AS ROWID
     FIELD qty       AS INT
     FIELD trans-date LIKE fg-rcpth.trans-date
     FIELD trans-time LIKE fg-rdtlh.trans-time
     FIELD r-no       LIKE fg-rcpth.r-no.

DEF VAR vdat        AS   DATE                   FORMAT "99/99/9999" INIT TODAY NO-UNDO.
DEF VAR fslm        LIKE sman.sman NO-UNDO.
DEF VAR tslm        LIKE fslm                   INIT "zzz" NO-UNDO.
DEF VAR fcus        LIKE cust.cust-no NO-UNDO.
DEF VAR tcus        LIKE fcus                   INIT "zzzzzzzz" NO-UNDO.
DEF VAR fitm        LIKE itemfg.i-no NO-UNDO.
DEF VAR titm        LIKE fitm                   INIT "zzzzzzzzzzzzzzz" NO-UNDO.
DEF VAR fjob        LIKE fg-bin.job-no NO-UNDO.
DEF VAR tjob        LIKE fjob                   INIT "zzzzzz" NO-UNDO.
DEF VAR fjob2       LIKE fg-bin.job-no2         FORMAT "99" NO-UNDO.
DEF VAR tjob2       LIKE fjob2                  INIT 99 NO-UNDO.
DEF VAR v-q-or-v    AS   LOG                    FORMAT "Qty/Value" INIT YES NO-UNDO.
DEF VAR v-sub-t     AS   LOG                    FORMAT "Yes/No"    INIT NO NO-UNDO.
DEF VAR v-break     AS   LOG                    FORMAT "Yes/No"    INIT YES NO-UNDO.
DEF VAR v-cost      AS   LOG                    FORMAT "Yes/No"    INIT NO NO-UNDO.
DEF VAR v-curr      AS   LOG                    FORMAT "Yes/No"    INIT YES NO-UNDO.
DEF VAR v-cpart     AS   LOG                    FORMAT "Yes/No"    INIT NO NO-UNDO.
DEF VAR v-sdate     AS   LOG                    FORMAT "Yes/No"    INIT NO NO-UNDO.

DEF VAR v-label     AS   CHAR                   FORMAT "x(8)"  EXTENT 15 NO-UNDO.
DEF VAR v-label1    AS   CHAR                   FORMAT "x(13)" EXTENT 13 NO-UNDO.
DEF VAR v-label2    AS   CHAR                   FORMAT "x(13)" EXTENT 4 NO-UNDO.
DEF VAR v-label3    AS   CHAR                   FORMAT "x(4)" EXTENT 4 NO-UNDO.
DEF VAR v-qohj      LIKE tt-qohi                EXTENT 6 NO-UNDO.
DEF VAR v-qohi      LIKE v-qohj NO-UNDO.
DEF VAR v-qohc      LIKE v-qohj                 EXTENT 5 NO-UNDO.
DEF VAR v-qohs      LIKE v-qohj                 EXTENT 5 NO-UNDO.
DEF VAR v-qohg      LIKE v-qohj                 EXTENT 5 NO-UNDO.
DEF VAR v-qty       AS   INT NO-UNDO.
DEF VAR v-qty1      LIKE v-qty NO-UNDO.
DEF VAR v-qtyc      LIKE v-qty NO-UNDO.
DEF VAR v-red       LIKE v-qty NO-UNDO.
DEF VAR v           AS   INT NO-UNDO.
DEF VAR v-cus       LIKE cust.cust-no NO-UNDO.
DEF VAR v-val       AS   DEC          FORMAT "->,>>>,>>9.99"          EXTENT 4 NO-UNDO.
DEF VAR v-cst       AS   DEC                    EXTENT 4 NO-UNDO.
DEF VAR v-u-val     AS   DEC NO-UNDO.
DEF VAR v-u-cst     AS   DEC NO-UNDO.
DEF VAR v-date      AS   DATE NO-UNDO.
DEF VAR v-class     AS   CHAR NO-UNDO.
DEF VAR sort-opt AS CHAR NO-UNDO INIT "I" FORMAT "!".
DEF VAR lv-tag      LIKE fg-rdtlh.tag NO-UNDO.
DEF VAR ld-last     AS   DATE NO-UNDO.
DEF VAR lv-last-fld AS   CHAR FORMAT "x(13)" EXTENT 2 NO-UNDO.
DEF VAR lv-sname    LIKE sman.sname NO-UNDO.
DEF VAR excelheader AS   CHAR NO-UNDO.
DEF VAR alt-days    AS   INT NO-UNDO.
DEFINE VARIABLE invalidChars AS CHARACTER NO-UNDO INITIAL "~",#".
DEFINE VARIABLE replaceChars AS CHARACTER NO-UNDO INITIAL "'',".

DEF VAR ls-fax-file AS cha NO-UNDO.
    DEF VAR v-qohj-2 /* LIKE v-qohj */ AS DEC EXTENT 6 NO-UNDO.
    DEF VAR v-qohi-2 /* LIKE v-qohi */ AS DEC EXTENT 6 NO-UNDO.
    DEF VAR v-qty-2 AS DEC NO-UNDO.
    DEF VAR v-shipdt AS CHAR EXTENT 2 NO-UNDO.
    DEF VAR v-custpart AS CHAR EXTENT 2 NO-UNDO.    
    DEF VAR v-dates  AS DATE EXTENT 5 NO-UNDO.
    DEF VAR v-sell-price AS DEC EXTENT 6 NO-UNDO.
    DEF VAR v-price AS DEC NO-UNDO.
    DEF VAR v-dr AS INT NO-UNDO.
    DEF VAR v-last-dt AS DATE NO-UNDO.
    DEF VAR lv-num-matches AS INT NO-UNDO.
    DEF VAR v-match-date AS DATE NO-UNDO.

DEF TEMP-TABLE tt-tag
FIELD i-no   AS CHAR
FIELD tag-no AS CHAR FORMAT "x(25)"
FIELD job-no AS CHAR 
FIELD job-no2 AS INT
FIELD tag-qty AS INT 
FIELD qty AS INT
FIELD rec-date AS DATE
INDEX i1 i-no tag-no.

DEF NEW SHARED STREAM excel.
DEF STREAM sTest1.


DEF BUFFER b-f-rc FOR fg-rcpth.
DEF BUFFER b-f-rd FOR fg-rdtlh.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_cust-list btnCustList tb_grand_tots ~
tb_sls_tots as-of-date begin_slm end_slm begin_cust-no end_cust-no ~
begin_i-no end_i-no begin_job-no begin_job-no2 end_job-no end_job-no2 ~
begin_whse end_whse begin_loc-bin end_loc-bin list_class aged-days-1 ~
aged-days-2 aged-days-3 aged-days-4 rd_show rd_price rd_show2 rd_sort ~
tb_cust-whse tb_cost tb_break tb_neg-sale tb_curr tb_val-cust tb_custpart ~
tb_last-ship-date rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
tb_excel tb_runExcel fi_file btn-ok btn-cancel tb_include_old_items RECT-6 ~
RECT-7 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list tb_grand_tots tb_sls_tots ~
as-of-date begin_slm end_slm begin_cust-no end_cust-no begin_i-no end_i-no ~
begin_job-no begin_job-no2 end_job-no end_job-no2 begin_whse end_whse ~
begin_loc-bin end_loc-bin list_class aged-days-1 aged-days-2 aged-days-3 ~
aged-days-4 lbl_show rd_show rd_price lbl_show2 rd_show2 lbl_sort rd_sort ~
tb_cust-whse tb_cost tb_break tb_neg-sale tb_curr tb_val-cust tb_custpart ~
tb_last-ship-date rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file tb_include_old_items 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD itemStatus C-Win 
FUNCTION itemStatus RETURNS CHARACTER
  (ipcCompany AS CHAR, ipcIno AS CHAR )  FORWARD.

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
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

DEFINE VARIABLE aged-days-1 AS INTEGER FORMAT ">,>>>":U INITIAL 9999 
     LABEL "Aged Days 1" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE aged-days-2 AS INTEGER FORMAT ">,>>>":U INITIAL 9999 
     LABEL "2" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE aged-days-3 AS INTEGER FORMAT ">,>>>":U INITIAL 9999 
     LABEL "3" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE aged-days-4 AS INTEGER FORMAT ">,>>>":U INITIAL 9999 
     LABEL "4" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/01 
     LABEL "As of" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc-bin AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Bin" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slm AS CHARACTER FORMAT "XXX":U 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc-bin AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Bin" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_slm AS CHARACTER FORMAT "XXX":U INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-ageinv.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE lbl_show AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_show2 AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE list_class AS CHARACTER FORMAT "X(256)":U 
     LABEL "Inventory Class(es)" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_price AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Average Price", "Avg",
"Sell Price", "Sell"
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE rd_show AS CHARACTER INITIAL "Quantity" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Quantity", "Quantity",
"Value", "Value"
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE rd_show2 AS CHARACTER INITIAL "Comments" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Comments", "Comments",
"Days Old (From Last Receipt)", "Days Old"
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Item#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item#", "Item#",
"Customer Part#", "Customer Part#",
"Warehouse","Warehouse"
     SIZE 50 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 19.71.

DEFINE VARIABLE tb_break AS LOGICAL INITIAL YES 
     LABEL "Page Break ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cost AS LOGICAL INITIAL NO 
     LABEL "Print Cost?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tb_curr AS LOGICAL INITIAL YES 
     LABEL "Print Items <90 Days Old?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL NO 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cust-whse AS LOGICAL INITIAL NO 
     LABEL "Include Customer Owned Warehouse?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tb_custpart AS LOGICAL INITIAL YES 
     LABEL "Print Customer Part#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_grand_tots AS LOGICAL INITIAL NO 
     LABEL "Print Grand Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tb_include_old_items AS LOGICAL INITIAL NO 
     LABEL "Include Inactive Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tb_last-ship-date AS LOGICAL INITIAL NO 
     LABEL "Print Last Shipment Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE tb_neg-sale AS LOGICAL INITIAL NO 
     LABEL "Exclude Negative Sell Value from Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL NO 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_sls_tots AS LOGICAL INITIAL NO 
     LABEL "Print Salesrep Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE tb_val-cust AS LOGICAL INITIAL NO 
     LABEL "Subtotal Value By Customer?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL YES 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_cust-list AT ROW 3.38 COL 31.6 WIDGET-ID 6
     btnCustList AT ROW 3.43 COL 63.6 WIDGET-ID 8
     tb_grand_tots AT ROW 18.76 COL 49 WIDGET-ID 22
     tb_sls_tots AT ROW 18.76 COL 6 WIDGET-ID 24
     as-of-date AT ROW 1.24 COL 27.8 COLON-ALIGNED
     begin_slm AT ROW 2.19 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slm AT ROW 1.95 COL 73 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_cust-no AT ROW 4.48 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 4.24 COL 73 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_i-no AT ROW 5.43 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 5.19 COL 68 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_job-no AT ROW 6.38 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 6.38 COL 41 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 6.14 COL 68.2 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 6.14 COL 80.2 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_whse AT ROW 7.33 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Warehouse" WIDGET-ID 10
     end_whse AT ROW 7.1 COL 68.2 COLON-ALIGNED HELP
          "Enter Ending Warehouse"
     begin_loc-bin AT ROW 8.29 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Warehouse"
     end_loc-bin AT ROW 8.1 COL 68.2 COLON-ALIGNED HELP
          "Enter Beginning Warehouse" WIDGET-ID 18
     list_class AT ROW 9.38 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Inventory Class"
     aged-days-1 AT ROW 10.95 COL 25 COLON-ALIGNED
     aged-days-2 AT ROW 10.95 COL 43 COLON-ALIGNED
     aged-days-3 AT ROW 10.95 COL 60 COLON-ALIGNED
     aged-days-4 AT ROW 10.95 COL 77 COLON-ALIGNED
     lbl_show AT ROW 11.91 COL 19 COLON-ALIGNED NO-LABEL
     rd_show AT ROW 11.91 COL 28 NO-LABEL
     rd_price AT ROW 11.91 COL 55.2 NO-LABEL WIDGET-ID 6
     lbl_show2 AT ROW 13.1 COL 19 COLON-ALIGNED NO-LABEL
     rd_show2 AT ROW 13.1 COL 28 NO-LABEL
     lbl_sort AT ROW 14.29 COL 19 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 14.29 COL 28 NO-LABEL WIDGET-ID 14
     tb_cust-whse AT ROW 15.43 COL 6
     tb_cost AT ROW 15.48 COL 49
     tb_break AT ROW 16.29 COL 6
     tb_neg-sale AT ROW 16.29 COL 49
     tb_curr AT ROW 17.1 COL 6
     tb_val-cust AT ROW 17.1 COL 49
     tb_custpart AT ROW 17.91 COL 6
     tb_last-ship-date AT ROW 17.91 COL 49
     rd-dest AT ROW 21.52 COL 5 NO-LABEL
     lv-ornt AT ROW 21.76 COL 31 NO-LABEL
     lines-per-page AT ROW 21.76 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 23.14 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 24.24 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 25.62 COL 30
     tb_excel AT ROW 26.38 COL 66 RIGHT-ALIGNED
     tb_runExcel AT ROW 26.38 COL 91 RIGHT-ALIGNED
     fi_file AT ROW 27.38 COL 44 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 29.1 COL 22
     btn-cancel AT ROW 29.1 COL 58
     tb_include_old_items AT ROW 19.57 COL 6 WIDGET-ID 26
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 20.91 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.6 BY 29.95.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
     "(Blank for all Inventory Classes)" VIEW-AS TEXT
          SIZE 30.8 BY .62 AT ROW 10.43 COL 39
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 2
          BGCOLOR 2 
     RECT-6 AT ROW 20.71 COL 1
     RECT-7 AT ROW 1.05 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.6 BY 29.95.


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
         TITLE              = "Aged Inventory Report"
         HEIGHT             = 29.95
         WIDTH              = 95.4
         MAX-HEIGHT         = 47.91
         MAX-WIDTH          = 336
         VIRTUAL-HEIGHT     = 47.91
         VIRTUAL-WIDTH      = 336
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME Custom                                                    */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       aged-days-1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       aged-days-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       aged-days-3:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       aged-days-4:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       as-of-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_loc-bin:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_loc-bin:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_show IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_show:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_show".

/* SETTINGS FOR FILL-IN lbl_show2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_show2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_show2".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

ASSIGN 
       list_class:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_price:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_show:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_show2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_break:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cost:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_curr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_custpart:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_grand_tots:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_include_old_items:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_last-ship-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_neg-sale:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sls_tots:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_val-cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Aged Inventory Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Aged Inventory Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME aged-days-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL aged-days-1 C-Win
ON LEAVE OF aged-days-1 IN FRAME FRAME-A /* Aged Days 1 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME aged-days-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL aged-days-2 C-Win
ON LEAVE OF aged-days-2 IN FRAME FRAME-A /* 2 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME aged-days-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL aged-days-3 C-Win
ON LEAVE OF aged-days-3 IN FRAME FRAME-A /* 3 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME aged-days-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL aged-days-4 C-Win
ON LEAVE OF aged-days-4 IN FRAME FRAME-A /* 4 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME as-of-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as-of-date C-Win
ON LEAVE OF as-of-date IN FRAME FRAME-A /* As of */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_loc-bin C-Win
ON LEAVE OF begin_loc-bin IN FRAME FRAME-A /* Beginning Bin */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slm C-Win
ON LEAVE OF begin_slm IN FRAME FRAME-A /* Beginning Sales Rep# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whse C-Win
ON LEAVE OF begin_whse IN FRAME FRAME-A /* Beginning Warehouse */
DO:
  ASSIGN {&self-name}.
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
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  SESSION:SET-WAIT-STATE("general").
  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN DO:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust-no,
                    INPUT end_cust-no).
  END.
  RUN run-report. 

  STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE("").

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_cust-no
                            &END_cust=END_cust-no
                            &fax-subject= "Aged Inventory Report" 
                            &fax-body= "Aged Inventory Report" 
                            &fax-file=list-name }
       END.
       WHEN 5 THEN DO:
          {custom/asimailr2.i &TYPE = "CUSTOMER"
                             &group-title= 'r-Ageinv.'
                             &begin_cust= begin_cust-no
                             &END_cust=end_cust-no
                             &mail-subject= "Aged Inventory Report"
                             &mail-body= "Aged Inventory Report"
                             &mail-file=list-name }

       END. 
       WHEN 6 THEN RUN output-to-port.
  END CASE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_loc-bin C-Win
ON LEAVE OF end_loc-bin IN FRAME FRAME-A /* Ending Bin */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slm C-Win
ON LEAVE OF end_slm IN FRAME FRAME-A /* Ending Sales Rep# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whse C-Win
ON LEAVE OF end_whse IN FRAME FRAME-A /* Ending Warehouse */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
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


&Scoped-define SELF-NAME list_class
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL list_class C-Win
ON LEAVE OF list_class IN FRAME FRAME-A /* Inventory Class(es) */
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

    RUN WINDOWS/l-fonts.w ({&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

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
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_price C-Win
ON VALUE-CHANGED OF rd_price IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_show C-Win
ON VALUE-CHANGED OF rd_show IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_break
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_break C-Win
ON VALUE-CHANGED OF tb_break IN FRAME FRAME-A /* Page Break By Customer? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cost C-Win
ON VALUE-CHANGED OF tb_cost IN FRAME FRAME-A /* Print Cost? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_curr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_curr C-Win
ON VALUE-CHANGED OF tb_curr IN FRAME FRAME-A /* Print Items <90 Days Old? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
  ASSIGN {&self-name}.
  EMPTY TEMP-TABLE ttCustList.
  RUN SetCustRange(INPUT tb_cust-list).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_custpart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_custpart C-Win
ON VALUE-CHANGED OF tb_custpart IN FRAME FRAME-A /* Print Customer Part#? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_last-ship-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_last-ship-date C-Win
ON VALUE-CHANGED OF tb_last-ship-date IN FRAME FRAME-A /* Print Last Shipment Date? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_neg-sale
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_neg-sale C-Win
ON VALUE-CHANGED OF tb_neg-sale IN FRAME FRAME-A /* Exclude Negative Sell Value from Totals? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_val-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_val-cust C-Win
ON VALUE-CHANGED OF tb_val-cust IN FRAME FRAME-A /* Subtotal Value By Customer? */
DO:
  ASSIGN {&self-name}.
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

v-custom = YES.
v-custom = YES .

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

IF v-custom EQ ? THEN DO:
      APPLY 'CLOSE' TO THIS-PROCEDURE.
      RETURN.
END.
ELSE
   ASSIGN CURRENT-WINDOW:HIDDEN = FALSE
   THIS-PROCEDURE:CURRENT-WINDOW:VISIBLE = YES.

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

  ASSIGN
   as-of-date  = TODAY
   aged-days-1 = 30
   aged-days-2 = 60
   aged-days-3 = 90
   aged-days-4 = 120.

  RUN enable_UI.

  /* Check for custom report version */


  IF NOT v-custom AND ir12-log THEN
      RUN set-custom (INPUT v-custom).

  /* Standard report sets defaults from user defined in nk1=IR12 */
  IF NOT v-custom AND ir12-char GT "" AND ir12-log THEN
      v-custom-user = ir12-char.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "IR12",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:

    {custom/usrprntc.i "and true" v-custom-user}

    IF NOT AVAIL user-print THEN DO:
      FOR EACH itemfg WHERE itemfg.company EQ cocode NO-LOCK BREAK BY itemfg.class:
        IF FIRST-OF(itemfg.class) THEN
          list_class:SCREEN-VALUE = list_class:SCREEN-VALUE + TRIM(itemfg.class) + ",".
      END.
      IF SUBSTR(list_class:SCREEN-VALUE,LENGTH(TRIM(list_class:SCREEN-VALUE)),1) EQ "," THEN
        list_class:SCREEN-VALUE = SUBSTR(list_class:SCREEN-VALUE,1,LENGTH(TRIM(list_class:SCREEN-VALUE)) - 1).
    END.

    APPLY "entry" TO as-of-date.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'IR12',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""IR12""}

  IF v-custom THEN DO:
      IF ou-log THEN DO:
          ASSIGN 
              tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = YES
              tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
              tb_cust-list = YES .
          RUN SetCustRange(INPUT tb_cust-list).
      END.
      ELSE
          ASSIGN
            tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
            btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            .

       IF ou-log AND ou-cust-int = 0 THEN DO:
           ASSIGN 
            tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
            tb_cust-list = NO
            .
           RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
       END.
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-report C-Win 
PROCEDURE build-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* fg/rep/fg-aging2.p */
/* Refactor of r-ageinv.i and fg-aging2.i */
/* Input Parameter Definitions */

DEF INPUT PARAMETER aged-days-1 AS INTEGER FORMAT ">,>>>":U INITIAL 9999 
     LABEL "Aged Days 1" 
     NO-UNDO.

DEF INPUT PARAMETER aged-days-2 AS INTEGER FORMAT ">,>>>":U INITIAL 9999 
     LABEL "2" 

     NO-UNDO.

DEF INPUT PARAMETER aged-days-3 AS INTEGER FORMAT ">,>>>":U INITIAL 9999 
     LABEL "3" 

     NO-UNDO.

DEF INPUT PARAMETER aged-days-4 AS INTEGER FORMAT ">,>>>":U INITIAL 9999 
     LABEL "4" 

     NO-UNDO.

DEF INPUT PARAMETER as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/01 
     LABEL "As of" 

     NO-UNDO.

DEF INPUT PARAMETER begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 

     .

DEF INPUT PARAMETER begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 

     NO-UNDO.

DEF INPUT PARAMETER begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 

     NO-UNDO.

DEF INPUT PARAMETER begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 

     NO-UNDO.

DEF INPUT PARAMETER begin_loc-bin AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Bin" 

     NO-UNDO.

DEF INPUT PARAMETER begin_slm AS CHARACTER FORMAT "XXX":U 
     LABEL "Beginning Sales Rep#" 

     NO-UNDO.

DEF INPUT PARAMETER begin_whse AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Warehouse" 

      NO-UNDO.

DEF INPUT PARAMETER end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 

     .

DEF INPUT PARAMETER end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 

     NO-UNDO.

DEF INPUT PARAMETER end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 

     NO-UNDO.

DEF INPUT PARAMETER end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 

     NO-UNDO.

DEF INPUT PARAMETER end_loc-bin AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Bin" 

     NO-UNDO.

DEF INPUT PARAMETER end_slm AS CHARACTER FORMAT "XXX":U INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 

     NO-UNDO.

DEF INPUT PARAMETER end_whse AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 

     NO-UNDO.

DEF INPUT PARAMETER fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-ageinv.csv" 
     LABEL "If Yes, File Name" 

     .

DEF INPUT PARAMETER lbl_show AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 

     NO-UNDO.

DEF INPUT PARAMETER lbl_show2 AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 

     NO-UNDO.

DEF INPUT PARAMETER lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 

     NO-UNDO.

DEF INPUT PARAMETER lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 

     NO-UNDO.

DEF INPUT PARAMETER list_class AS CHARACTER FORMAT "X(256)":U 
     LABEL "Inventory Class(es)" 

     NO-UNDO.

DEF INPUT PARAMETER lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 

     NO-UNDO.

DEF INPUT PARAMETER lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 

     NO-UNDO.

DEF INPUT PARAMETER lv-ornt AS CHARACTER INITIAL "P" 
     NO-UNDO.

DEF INPUT PARAMETER rd-dest AS INTEGER INITIAL 1 
     NO-UNDO.

DEF INPUT PARAMETER rd_price AS CHARACTER 
      NO-UNDO.

DEF INPUT PARAMETER rd_show AS CHARACTER INITIAL "Quantity" 
     NO-UNDO.

DEF INPUT PARAMETER rd_show2 AS CHARACTER INITIAL "Comments" 
     NO-UNDO.

DEF INPUT PARAMETER rd_sort AS CHARACTER INITIAL "Item#" 
     NO-UNDO.


DEF INPUT PARAMETER tb_break AS LOGICAL INITIAL YES 
     LABEL "Page Break By Customer?" 
     NO-UNDO.

DEF INPUT PARAMETER tb_cost AS LOGICAL INITIAL NO 
     LABEL "Print Cost?" 
    NO-UNDO.

DEF INPUT PARAMETER tb_curr AS LOGICAL INITIAL YES 
     LABEL "Print Items <90 Days Old?" 
    NO-UNDO.

DEF INPUT PARAMETER tb_cust-whse AS LOGICAL INITIAL NO 
     LABEL "Include Customer Owned Warehouse?" 
    NO-UNDO.

DEF INPUT PARAMETER tb_custpart AS LOGICAL INITIAL YES 
     LABEL "Print Customer Part#?" 
    NO-UNDO.

DEF INPUT PARAMETER tb_excel AS LOGICAL INITIAL YES 
     LABEL "Export To Excel?" 
    NO-UNDO.

DEF INPUT PARAMETER tb_last-ship-date AS LOGICAL INITIAL NO 
     LABEL "Print Last Shipment Date?" NO-UNDO.

DEF INPUT PARAMETER tb_neg-sale AS LOGICAL INITIAL NO 
     LABEL "Exclude Negative Sell Value from Totals?" 
      NO-UNDO.

DEF INPUT PARAMETER tb_runExcel AS LOGICAL INITIAL NO 
     LABEL "Auto Run Excel?" NO-UNDO.

DEF INPUT PARAMETER tb_val-cust AS LOGICAL INITIAL NO 
     LABEL "Subtotal Value By Customer?" NO-UNDO.

DEF INPUT PARAMETER td-show-parm AS LOGICAL INITIAL YES 
     LABEL "Show Parameters?" NO-UNDO.
ASSIGN
 v-qohg = 0
 v-cst   = 0
 v-val   = 0
 lv-last-fld = ?.

/* OUTPUT STREAM sTest1 TO c:\temp\sTest2.txt. */

/* Main Block */
RUN produce-report.
STATUS DEFAULT "Processing...".
RUN print_report.
/* End fg-aging2.p */
STATUS DEFAULT "".

/* OUTPUT STREAM sTest1 CLOSE. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildCustList C-Win 
PROCEDURE BuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Builds the temp table of customers   
  Parameters:  Company Code, Customer list logical and/or customer range
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplList AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginCust AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndCust AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-cust FOR cust.

DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.

IF iplList THEN DO:
    RUN sys/ref/CustList.p (INPUT ipcCompany,
                            INPUT 'IR12',
                            INPUT YES,
                            OUTPUT lActive).
END.
ELSE DO:
    FOR EACH bf-cust
        WHERE bf-cust.company EQ ipcCompany
          AND bf-cust.cust-no GE ipcBeginCust
          AND bf-cust.cust-no LE ipcEndCust
        NO-LOCK:
        CREATE ttCustList.
        ASSIGN 
            ttCustList.cust-no = bf-cust.cust-no
            ttCustList.log-fld = YES
        .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check_negative C-Win 
PROCEDURE check_negative :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v AS INT.
DEF VAR v-qty AS INT.
DEF VAR l-found-positive AS LOG NO-UNDO.
DEF VAR v-neg-qty AS INT.
DEF VAR v-pos-qty AS INT.
DEF VAR v-qty-to-process AS INT.

REPEAT:
  ASSIGN
    l-found-positive = NO
    v-neg-qty = 0
    v-pos-qty = 0.
  DO v = 1 TO 5:
    IF v-qohi[v] GT 0 THEN
      ASSIGN l-found-positive = TRUE
             v-pos-qty = v-pos-qty + v-qohi[v].
    IF v-qohi[v] LT 0 THEN
      ASSIGN v-neg-qty = v-neg-qty + v-qohi[v].

  END.
  IF l-found-positive = TRUE AND v-neg-qty LT 0 THEN DO:
      /* Reduce the positive quantities by the negative quantity */
      /* to the degree possible */
      v-qty-to-process = MIN(- v-neg-qty, v-pos-qty).
      v-qty = v-qty-to-process.
      DO v = 5 TO 1 BY -1:
        IF v-qohi[v] GT 0 THEN
        DO:
            ASSIGN
              v-red     = min(v-qty,v-qohi[v])
              v-qohi[v] = v-qohi[v] - v-red
              v-qty     = v-qty     - v-red.

           IF rd_price = "Sell" THEN
              v-sell-price[v] = v-sell-price[v] - (v-price * v-red).
        END.

        IF v-qty LE 0 THEN LEAVE.
      END.

      IF v-qty GT 0 THEN v-qohi[6] = v-qohi[6] - v-qty.

      /* Reduce the negative quantity the same way */
      v-qty = v-qty-to-process.
      DO v = 5 TO 1 BY -1:
        IF v-qohi[v] LT 0 THEN
        DO:
            ASSIGN
              /* eg. if we have -20 and -10, only process the -10 */
              v-red     = MAX(- v-qty,v-qohi[v])
              /* v-red is a negative number, so this reduces the neg. 
                 value in v-qohi */
              v-qohi[v] = v-qohi[v] - v-red
              /* v-qty is positive, v-red is negative */
              v-qty     = v-qty + v-red.

           IF rd_price = "Sell" THEN
              v-sell-price[v] = v-sell-price[v] - (v-price * v-red).
        END.

        IF v-qty LE 0 THEN LEAVE.
      END.

      IF v-qty GT 0 THEN v-qohi[6] = v-qohi[6] + v-qty.

  END. /* negative found to reduce */

  LEAVE.
END. /* Repeat */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
/*------------------------------------------------------------------------------
  Purpose:  Display a UI of selected customers   
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
                                  INPUT 'IR12').


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
  DISPLAY tb_cust-list tb_grand_tots tb_sls_tots as-of-date begin_slm end_slm 
          begin_cust-no end_cust-no begin_i-no end_i-no begin_job-no 
          begin_job-no2 end_job-no end_job-no2 begin_whse end_whse begin_loc-bin 
          end_loc-bin list_class aged-days-1 aged-days-2 aged-days-3 aged-days-4 
          lbl_show rd_show rd_price lbl_show2 rd_show2 lbl_sort rd_sort 
          tb_cust-whse tb_cost tb_break tb_neg-sale tb_curr tb_val-cust 
          tb_custpart tb_last-ship-date rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
          tb_include_old_items 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE tb_cust-list btnCustList tb_grand_tots tb_sls_tots as-of-date 
         begin_slm end_slm begin_cust-no end_cust-no begin_i-no end_i-no 
         begin_job-no begin_job-no2 end_job-no end_job-no2 begin_whse end_whse 
         begin_loc-bin end_loc-bin list_class aged-days-1 aged-days-2 
         aged-days-3 aged-days-4 rd_show rd_price rd_show2 rd_sort tb_cust-whse 
         tb_cost tb_break tb_neg-sale tb_curr tb_val-cust tb_custpart 
         tb_last-ship-date rd-dest lv-ornt lines-per-page lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
         tb_include_old_items RECT-6 RECT-7 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
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
{custom/out2file.i}     

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
  RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE price-calc C-Win 
PROCEDURE price-calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Copied directly from fg-ibtag.i      
------------------------------------------------------------------------------*/
DEF BUFFER bf-tt-fg-bin FOR tt-fg-bin.
DEF INPUT PARAMETER ipr-tt-fg-bin-row AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER opd-sell-price   AS DEC NO-UNDO.
DEF OUTPUT PARAMETER opc-price-uom    AS CHAR NO-UNDO.

DEF VAR v-first AS LOG EXTENT 2.
DEF VAR v-tot-sum AS DEC NO-UNDO.
DEF VAR v-ext-sum AS DEC NO-UNDO.
DEF VAR v-qoh AS DEC NO-UNDO.
DEF VAR v-procat AS CHAR NO-UNDO.
DEF VAR v-bin AS LOG NO-UNDO.
DEF VAR v-tot-bin-sum AS DEC NO-UNDO.
DEF VAR v-ext-bin-sum AS DEC NO-UNDO.
DEF VAR v-bin-qoh AS DEC NO-UNDO.
DEF VAR v-bin-arq AS DEC NO-UNDO.
DEF VAR v-sort-by-cust AS CHAR NO-UNDO.
DEF VAR lv-rct-date AS DATE NO-UNDO.
DEF VAR v-costl AS DEC NO-UNDO.
DEF VAR v-cost1 AS DEC NO-UNDO.
DEF VAR v-costm AS DEC NO-UNDO.
DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR v-summ-bin AS LOG NO-UNDO.
DEF VAR v-ext AS DEC NO-UNDO.
DEF VAR v-found-job AS LOG NO-UNDO.
DEF VAR lv-sell-price LIKE itemfg.sell-price NO-UNDO.
DEF VAR lv-sell-uom   LIKE itemfg.sell-uom NO-UNDO.
DEF VAR lv-case-count LIKE itemfg.case-count NO-UNDO.
DEF VAR fg-lot-val AS CHAR NO-UNDO.
DEF VAR v-dl-mat AS LOG NO-UNDO.
DEF VAR v-fgprice AS LOG NO-UNDO.
DEF VAR v-prt-msf AS LOG NO-UNDO.

DEF VAR v-tot-qty AS DEC EXTENT 10 NO-UNDO.
DEF VAR v-tot-cst AS DEC EXTENT 10 NO-UNDO.
DEF VAR v-tot-ext AS DEC EXTENT 10 NO-UNDO.

FIND bf-tt-fg-bin WHERE ROWID(bf-tt-fg-bin) EQ ipr-tt-fg-bin-row NO-LOCK NO-ERROR.


    DO:
      ASSIGN
       v-first[1] = YES
       v-tot-sum  = 0
       v-ext-sum  = 0
       v-qoh      = 0.

      IF v-sort-by-cust EQ "Wh" THEN v-first[2] = YES.
    END.

    ASSIGN
     v-procat = itemfg.procat
     v-bin    = NO.

    IF v-summ-bin /* AND FIRST-OF(tt-itemfg.job-no2) */ THEN DO:
        ASSIGN v-tot-bin-sum  = 0
               v-ext-bin-sum  = 0
               v-bin-qoh      = 0
               v-bin-arq      = 0.
    END.  


      lv-rct-date = bf-tt-fg-bin.first-date.

      ASSIGN
       v-costl = bf-tt-fg-bin.std-lab-cost * bf-tt-fg-bin.qty
       v-costm = bf-tt-fg-bin.std-mat-cost * bf-tt-fg-bin.qty 
       v-cost1 = IF v-dl-mat THEN (bf-tt-fg-bin.std-lab-cost + bf-tt-fg-bin.std-mat-cost)
                             ELSE bf-tt-fg-bin.std-tot-cost
       v-cost  = v-cost1             * bf-tt-fg-bin.qty.

                                                  /* Calculate Cost */
      IF bf-tt-fg-bin.pur-uom EQ "CS" AND bf-tt-fg-bin.case-count NE 0 THEN
        ASSIGN
         v-costl = v-costl / bf-tt-fg-bin.case-count
         v-costm = v-costm / bf-tt-fg-bin.case-count
         v-cost  = v-cost  / bf-tt-fg-bin.case-count.
      ELSE
      IF bf-tt-fg-bin.pur-uom EQ "L" THEN
        ASSIGN
         v-costl = v-costl / bf-tt-fg-bin.qty
         v-costm = v-costm / bf-tt-fg-bin.qty
         v-cost  = v-costm / bf-tt-fg-bin.qty.
      ELSE DO:
        FIND FIRST uom
            WHERE uom.uom  EQ itemfg.prod-uom
              AND uom.mult NE 0
            NO-LOCK NO-ERROR.
        IF AVAIL uom THEN
          ASSIGN
           v-costl = v-costl / uom.mult
           v-costm = v-costm / uom.mult
           v-cost  = v-cost  / uom.mult.
        ELSE
          ASSIGN
           v-costl = v-costl / 1000
           v-costm = v-costm / 1000
           v-cost  = v-cost  / 1000.
      END.

      ASSIGN
       lv-sell-price = itemfg.sell-price
       lv-sell-uom   = itemfg.sell-uom
       lv-case-count = itemfg.case-count.

      IF bf-tt-fg-bin.po-no NE "" AND NOT v-fgprice THEN
      DO:
         FIND FIRST po-ordl WHERE
              po-ordl.company EQ bf-tt-fg-bin.company AND
              po-ordl.po-no EQ INT(bf-tt-fg-bin.po-no) AND
              po-ordl.i-no EQ bf-tt-fg-bin.i-no
              NO-LOCK NO-ERROR.

         IF AVAIL po-ordl THEN
         DO:
            FIND LAST oe-ordl WHERE
                 oe-ordl.company EQ bf-tt-fg-bin.company AND
                 oe-ordl.ord-no EQ po-ordl.ord-no AND
                 oe-ordl.i-no EQ bf-tt-fg-bin.i-no AND
                 (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                 NO-LOCK NO-ERROR.

            IF AVAIL oe-ordl THEN
               ASSIGN
                  lv-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100))
                  lv-sell-uom   = oe-ordl.pr-uom
                  lv-case-count = oe-ordl.cas-cnt.

         END.
      END.

      ELSE IF TRIM(bf-tt-fg-bin.job-no) NE "" AND NOT v-fgprice THEN
      DO:
         v-found-job = NO.

         FOR EACH job-hdr FIELDS(ord-no company i-no)
             WHERE job-hdr.company EQ bf-tt-fg-bin.company
               AND job-hdr.job-no  EQ bf-tt-fg-bin.job-no
               AND job-hdr.job-no2 EQ bf-tt-fg-bin.job-no2
               AND job-hdr.i-no    EQ bf-tt-fg-bin.i-no
               AND job-hdr.ord-no  NE 0
             USE-INDEX job-no NO-LOCK,
             FIRST oe-ordl FIELDS(ord-no price pr-uom cas-cnt disc)
             WHERE oe-ordl.company EQ job-hdr.company
               AND oe-ordl.ord-no  EQ job-hdr.ord-no
               AND oe-ordl.i-no    EQ job-hdr.i-no
               AND (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
             USE-INDEX item-ord NO-LOCK
             BY job-hdr.ord-no DESC:

           ASSIGN
            lv-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100))
            lv-sell-uom   = oe-ordl.pr-uom
            lv-case-count = oe-ordl.cas-cnt
            v-found-job = YES.

           LEAVE.
         END.

         IF v-found-job = NO THEN
         DO:
            FIND LAST oe-ordl WHERE
                 oe-ordl.company EQ bf-tt-fg-bin.company AND
                 oe-ordl.job-no EQ bf-tt-fg-bin.job-no AND
                 oe-ordl.job-no2 EQ bf-tt-fg-bin.job-no2 AND
                 oe-ordl.i-no EQ bf-tt-fg-bin.i-no AND
                 (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                 NO-LOCK NO-ERROR.

            IF AVAIL oe-ordl THEN
               ASSIGN
                  lv-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100))
                  lv-sell-uom   = oe-ordl.pr-uom
                  lv-case-count = oe-ordl.cas-cnt.
         END.
      END.
                                         /* Calculate Selling Price */
      IF lv-sell-uom EQ "CS" AND lv-case-count NE 0 THEN
        v-ext = (bf-tt-fg-bin.qty * lv-sell-price) / lv-case-count.
      ELSE DO:
        FIND FIRST uom
            WHERE uom.uom  EQ lv-sell-uom
              AND uom.mult NE 0
            NO-LOCK NO-ERROR.
        v-ext = bf-tt-fg-bin.qty * lv-sell-price /
                (IF AVAIL uom THEN uom.mult ELSE 1000).
      END.

      IF itemfg.sell-uom EQ "L" THEN
        IF bf-tt-fg-bin.qty LE 0 THEN v-ext = 0.
        ELSE v-ext = lv-sell-price.

      v-ext = ROUND(v-ext,2).  

      IF v-costl EQ ? THEN v-costl = 0.
      IF v-costm EQ ? THEN v-costm = 0.
      IF v-cost  EQ ? THEN v-cost  = 0.
      IF v-ext   EQ ? THEN v-ext   = 0.

      ASSIGN
       v-qoh     = bf-tt-fg-bin.qty
       v-tot-sum = IF v-dl-mat THEN v-costl ELSE v-cost
       v-ext-sum = IF v-dl-mat THEN v-costm ELSE v-ext.

      IF v-prt-msf THEN v-qoh = v-qoh * itemfg.t-sqft / 1000.

      ASSIGN
       v-bin-qoh = v-bin-qoh + v-qoh
       v-tot-bin-sum = v-tot-bin-sum + v-tot-sum
       v-ext-bin-sum = v-ext-bin-sum + v-ext-sum.

      ASSIGN
       v-tot-qty[1] = v-tot-qty[1] + v-qoh
       v-tot-cst[1] = v-tot-cst[1] + v-tot-sum
       v-tot-ext[1] = v-tot-ext[1] + v-ext-sum.
      ASSIGN
       opd-sell-price = lv-sell-price
       opc-price-uom  = lv-sell-uom.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print_report C-Win 
PROCEDURE print_report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-comm-tail AS CHAR NO-UNDO. /* end of comment */
{sys/form/r-topw.f}

FORM  HEADER
      SKIP(1)
      day_str
      str-tit  FORMAT "x(112)"
      "Page" AT 123
      PAGE-NUMBER FORMAT ">>9"
      SKIP
      tim_str
      str-tit2 FORMAT "x(112)"   "{1}" AT 123
      SKIP(1)

     WITH FRAME r-top2 ROW 1 COLUMN 1 STREAM-IO WIDTH 150
           NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

FIND FIRST tt-file NO-LOCK NO-ERROR.
ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 vdat      = as-of-date
 fslm      = begin_slm
 tslm      = END_slm
 fcus      = begin_cust-no
 tcus      = end_cust-no
 fitm      = begin_i-no
 titm      = end_i-no
 fjob      = FILL(" ",6 - length(TRIM(begin_job-no))) +
                trim(begin_job-no) + string(int(begin_job-no2),"99")
 tjob      = FILL(" ",6 - length(TRIM(end_job-no)))   +
                trim(end_job-no)   + string(int(end_job-no2),"99") 
 v-q-or-v  = rd_show EQ "Quantity"
 v-sub-t   = tb_val-cust
 v-break   = tb_break
 v-cost    = tb_cost
 v-curr    = tb_curr
 v-cpart   = tb_custpart
 v-sdate   = tb_last-ship-date

 v-label    = STRING(v-q-or-v,"    Qty/  Value")
 v-class    = list_class
 list_class = ""
 sort-opt    = SUBSTR(rd_sort,1,1) 
 v-print-grand-tots = tb_grand_tots
 v-print-sls-tots   = tb_sls_tots.

DO li = 1 TO NUM-ENTRIES(v-class):
  list_class = list_class + " " + (IF ENTRY(li,v-class) EQ "" THEN "Spaces"
                                   ELSE ENTRY(li,v-class)).
END.
list_class = TRIM(list_class).

DO WITH FRAME {&FRAME-NAME}:
  DISPLAY list_class.
  ASSIGN list_class.
END.

IF v-cpart AND v-sdate THEN
    ASSIGN
     v-label2[01] = "Cust Part # "
     v-label2[02] = "------------" 
     v-label2[03] = "Last Ship " 
     v-label2[04] = "----------".
ELSE IF v-cpart AND NOT(v-sdate) THEN
    ASSIGN
     v-label2[01] = "Cust Part # "      
     v-label2[02] = "------------" 
     v-label2[03] = ""      
     v-label2[04] = "" .
ELSE IF v-sdate AND NOT(v-cpart) THEN
    ASSIGN
     v-label2[01] = "Last Ship   "      
     v-label2[02] = "------------" 
     v-label2[03] = ""      
     v-label2[04] = "" .
ELSE
    ASSIGN
     v-label2[01] = ""      
     v-label2[02] = ""
     v-label2[03] = ""      
     v-label2[04] = ""    .

IF v-cost THEN
  ASSIGN
   v-label1[1] = "             "
   v-label1[2] = "      Selling"
   v-label1[3] = "         Cost"
   v-label1[4] = "      Value $"
   v-label1[5] = "-------------"
   v-label1[6] = "-------------"
   v-label1[7] = "             "
   v-label1[8] = IF rd_show2 BEGINS "Com" THEN "Comm"
                                          ELSE "Days"
   v-label1[9] = "--------     " 
   v-label1[10] = v-label2[01]
   v-label1[11] = v-label2[02] 
   v-label1[12] = v-label2[03]
   v-label1[13] = v-label2[04] .

ELSE
  ASSIGN
   v-label1[1]  = "      Selling"
   v-label1[2]  = "             "
   v-label1[3]  = "      Value $"
   v-label1[4]  = IF rd_show2 BEGINS "Com" THEN "Comm"
                                          ELSE "Days"
   v-label1[5]  = "-------------"
   v-label1[6]  = IF rd_show2 BEGINS "Com" THEN "--------     " ELSE "----"
   v-label1[7]  = ""
   v-label1[8]  = v-label2[01]
   v-label1[9]  = v-label2[02]
   v-label1[10] = v-label2[03]
   v-label1[11] = v-label2[04]
   v-label1[12] = ""
   v-label1[13] = "".

ASSIGN v-label3[1]  = IF rd_show2 BEGINS "Com" THEN "Comm" ELSE "Days"          /*Task# 02041406*/
   v-label3[2]  = "----" .


ASSIGN
 v-label[06] = "00-" + TRIM(STRING(aged-days-1 - 1,">,>99"))
 v-label[07] = TRIM(STRING(aged-days-1,">,>99")) + "-" +
               TRIM(STRING(aged-days-2 - 1,">,>99"))
 v-label[08] = TRIM(STRING(aged-days-2,">,>99")) + "-" +
               TRIM(STRING(aged-days-3 - 1,">,>99"))
 v-label[09] = TRIM(STRING(aged-days-3,">,>99")) + "-" +
               TRIM(STRING(aged-days-4 - 1,">,>99"))
 v-label[10] = TRIM(STRING(aged-days-4,">,>99")) + "+".

DO li = 6 TO 10:
  v-label[li] = FILL(" ",8 - LENGTH(TRIM(v-label[li]))) + TRIM(v-label[li]).
END.

IF v-curr THEN
  ASSIGN
   v-label[01] = v-label[01]
   v-label[02] = v-label[02]
   v-label[03] = v-label[03]
   v-label[04] = v-label[04]
   v-label[05] = v-label[05]
   v-label[11] = "--------"
   v-label[12] = "--------"
   v-label[13] = "--------"
   v-label[14] = "--------"
   v-label[15] = "--------".

ELSE
  ASSIGN
   v-label[01] = "        "
   v-label[02] = "        "
   v-label[03] = "        "
   v-label[04] = v-label[04]
   v-label[05] = v-label[05]
   v-label[11] = "        "
   v-label[12] = "        "
   v-label[13] = "        "
   v-label[14] = "--------"
   v-label[15] = "--------".

IF v-cost THEN DO:
FORM HEADER
     "As of:"
     vdat
     SKIP(1)

    /* "Salesperson:"
     tt-sman 
     lv-sname */                           SKIP

     "Sls"
     "            "
     "               "
     "                              "     
     v-label[01]
     v-label[02]
     v-label[03]
     v-label[04]
     v-label[05]
     v-label1[1]
     v-label1[2]
     v-label1[7]                        SKIP

     "Rep"
     "Customer    "
     "Item #         "
     "Description                   "     
     v-label[06]
     v-label[07]
     v-label[08]
     v-label[09]
     v-label[10]
     v-label1[3]
     v-label1[4]
     v-label3[1]  
     v-label1[10]
     v-label1[12]                       SKIP

     "---"
     "------------"
     "---------------"
     "------------------------------"     
     v-label[11]
     v-label[12]
     v-label[13]
     v-label[14]
     v-label[15]          
     v-label1[5]
     v-label1[6]
     v-label3[2]
     v-label1[11]
     v-label1[13]                       SKIP

     SKIP(1)

    WITH FRAME r-top WIDTH 200.
END.
ELSE DO:                                                /*Task# 02041406*/
    FORM HEADER
     "As of:"
     vdat
     SKIP(1)

    /* "Salesperson:"
     tt-sman 
     lv-sname */                           SKIP

     "Sls"
     "            "
     "               "
     "                              "     
     v-label[01]
     v-label[02]
     v-label[03]
     v-label[04]
     v-label[05]
     v-label1[1]
     v-label1[2]
     v-label1[7]                        SKIP

     "Rep"
     "Customer    "
     "Item #         "
     "Description                   "     
     v-label[06]
     v-label[07]
     v-label[08]
     v-label[09]
     v-label[10]
     v-label1[3]
     v-label3[1] 
     v-label1[8]  
     v-label1[10]
     v-label1[12]                       SKIP

     "---"
     "------------"
     "---------------"
     "------------------------------"     
     v-label[11]
     v-label[12]
     v-label[13]
     v-label[14]
     v-label[15]          
     v-label1[5]
     v-label3[2]
     v-label1[9]
     v-label1[11]
     v-label1[13]                       SKIP

     SKIP(1)

    WITH FRAME r-top2 WIDTH 200.
END.


{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF td-show-parm THEN RUN show-param.

DO WITH FRAME {&FRAME-NAME}:
  list_class:SCREEN-VALUE = v-class.
  ASSIGN list_class.
END.

READKEY PAUSE 0.

SESSION:SET-WAIT-STATE ("general").

IF v-cost THEN DO:
    IF tb_excel THEN DO:
        OUTPUT STREAM excel TO VALUE(fi_file).
        excelheader = "Salesperson,Customer,Item #,Description," + TRIM(v-label[1]) + " "
                      + TRIM(v-label[6]) + "," + TRIM(v-label[2]) + " " + TRIM(v-label[7])
                      + "," + TRIM(v-label[3]) + " " + TRIM(v-label[8]) + ","
                      + TRIM(v-label[4]) + " " + TRIM(v-label[9]) + "," + TRIM(v-label[5])
                      + " " + TRIM(v-label[10]) + "," + TRIM(v-label1[1]) + " "
                      + TRIM(v-label1[3]) + "," + TRIM(v-label1[2]) + " " + TRIM(v-label1[4])
                      + "," + TRIM(v-label1[7]) + " " + TRIM(v-label3[1]) + "," + TRIM(v-label1[10]) 
                      + "," + TRIM(v-label1[12]).
        PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.
END.
ELSE DO:
    IF tb_excel THEN DO:
        OUTPUT STREAM excel TO VALUE(fi_file).
        excelheader = "Salesperson,Customer,Item #,Description," + TRIM(v-label[1]) + " "
                      + TRIM(v-label[6]) + "," + TRIM(v-label[2]) + " " + TRIM(v-label[7])
                      + "," + TRIM(v-label[3]) + " " + TRIM(v-label[8]) + ","
                      + TRIM(v-label[4]) + " " + TRIM(v-label[9]) + "," + TRIM(v-label[5])
                      + " " + TRIM(v-label[10]) + "," + TRIM(v-label1[1]) + " "
                      + TRIM(v-label1[3]) + "," + TRIM(v-label1[2]) + " " + TRIM(v-label3[1])
                      + "," + TRIM(v-label1[7]) + " " + TRIM(v-label1[8]) + "," + TRIM(v-label1[10]) 
                      + "," + TRIM(v-label1[12]).
        PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.
END.

/*FOR EACH tt-file.
    IF tt-file.tt-cust = "" THEN DO:
        FIND FIRST cust WHERE cust.company = cocode NO-LOCK NO-ERROR.
        IF AVAIL cust THEN
            ASSIGN tt-file.tt-cust = cust.cust-no .
        IF AVAIL cust AND tt-file.tt-sman EQ "" THEN
            ASSIGN tt-file.tt-sman = s-man[1].
    END.
END.*/

FOR EACH tt-file WHERE
    (tt-qohi[1] NE 0 OR
    tt-qohi[2] NE 0 OR
    tt-qohi[3] NE 0 OR
    tt-qohi[4] NE 0 OR
    tt-qohi[5] NE 0)
/*    AND (tt-qohi[1] + tt-qohi[2] + tt-qohi[3] + tt-qohi[4] + tt-qohi[5]) GT
         0 */
    AND NOT (NOT v-curr AND (tt-qohi[1] + tt-qohi[2] + tt-qohi[3]) GT 0)
    AND tt-file.tt-sman GE fslm
    AND tt-file.tt-sman LE tslm ,    
    FIRST itemfg
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ tt-i-no
    NO-LOCK    
    BREAK BY tt-sman
          BY tt-cust-no
          /*by tt-i-no*/
          BY (IF sort-opt EQ "I" THEN itemfg.i-no ELSE IF sort-opt EQ "C" THEN  itemfg.part-no ELSE tt-file.tt-loc ):

  FIND FIRST cust NO-LOCK
       WHERE cust.company EQ cocode
         AND cust.cust-no EQ tt-cust-no
    NO-ERROR.

  FIND FIRST sman NO-LOCK
      WHERE sman.company EQ cocode
        AND sman.sman    EQ tt-sman
      NO-ERROR.

  lv-sname = IF AVAIL sman AND sman.sname NE "" THEN sman.sname ELSE
             IF AVAIL cust AND cust.sman EQ "" THEN "No Sales Rep Name" ELSE "Not on File".

  IF FIRST-OF(tt-sman)                  OR
     (FIRST-OF(tt-cust-no) AND v-break) THEN DO:
    IF FIRST(tt-sman) THEN DO: 
        IF v-cost THEN DISPLAY WITH FRAME r-top.
        ELSE DISPLAY WITH FRAME r-top2.
    END.
    /* ELSE page. 01031332 no longer breaking */
  END.

  /*if first-of(tt-cust-no) then*/ v-cus = IF AVAIL cust THEN cust.NAME ELSE "".

  FIND LAST fg-rcpth NO-LOCK 
      WHERE fg-rcpth.company EQ cocode 
        AND fg-rcpth.i-no EQ itemfg.i-no 
        AND fg-rcpth.rita-code EQ "S" NO-ERROR.

  ASSIGN
   v-cst[1]  = tt-cst[1]
   v-val[1]  = tt-val[1]
   v-qohi[1] = tt-qohi[1]
   v-qohi[2] = tt-qohi[2]
   v-qohi[3] = tt-qohi[3]
   v-qohi[4] = tt-qohi[4]
   v-qohi[5] = tt-qohi[5]

   tt-days = tt-file.tt-days
   lv-last-fld = IF rd_show2 BEGINS "Day" THEN STRING(tt-days,">>>9")
                                               ELSE ""
   v-shipdt = IF AVAIL fg-rcpth AND v-sdate THEN STRING(fg-rcpth.trans-date) 
                                               ELSE "" .  

   RUN reduce_negatives.

    IF NOT v-q-or-v /* AND rd_price = "Sell" */ THEN
        v-val[1] = v-qohi[1] + v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].


    IF v-q-or-v /* fix to avg sell  AND rd_price = "Sell" */ THEN
        v-val[1] = tt-sell-price[1] + 
                   tt-sell-price[2] + 
                   tt-sell-price[3] + 
                   tt-sell-price[4] + 
                   tt-sell-price[5].

    RUN SET_column_values.
{custom/statusMsg.i "'Printing Report: Item ' + itemfg.i-no"}
IF v-cost THEN DO:
  DISPLAY /* 11091201 - sales man on each line */
          tt-sman  FORMAT "x(3)"
          v-cus                   FORMAT "x(12)"
          itemfg.i-no             FORMAT "x(15)"
          itemfg.i-name           FORMAT "x(30)"
          v-qohi[1] WHEN v-curr   FORMAT "->>>>>>9"
          v-qohi[2] WHEN v-curr   FORMAT "->>>>>>9"
          v-qohi[3] WHEN v-curr   FORMAT "->>>>>>9"
          v-qohi[4]               FORMAT "->>>>>>9"
          v-qohi[5]               FORMAT "->>>>>>9"
          v-cst[1]  WHEN v-cost   FORMAT "->,>>>,>>9.99"
              v-val[1] WHEN NOT v-cost @ v-cst[1]
          lv-last-fld[1] FORMAT "x(4)" 
              v-val[1] WHEN v-cost     @ lv-last-fld[1]
          SPACE(1)
          /*lv-comm-tail WHEN rd_show2 BEGINS "COM"*/
          lv-last-fld[2] FORMAT "x(4)"                     /*Task# 02041406*/
          v-custpart[1] WHEN  v-cpart FORMAT "x(13)" 
          v-shipdt[1]  WHEN NOT v-cpart @ v-custpart[1]
          v-shipdt[2] WHEN v-cost
          /* 11091201 - customer name on 2nd line */
          SKIP          
      WITH FRAME detail NO-BOX NO-LABELS NO-ATTR-SPACE STREAM-IO WIDTH 200 DOWN.
  DOWN WITH FRAME detail.
END.
ELSE DO:
    DISPLAY /* 11091201 - sales man on each line */
          tt-sman  FORMAT "x(3)"
          v-cus                   FORMAT "x(12)"
          itemfg.i-no             FORMAT "x(15)"
          itemfg.i-name           FORMAT "x(30)"
          v-qohi[1] WHEN v-curr   FORMAT "->>>>>>9"
          v-qohi[2] WHEN v-curr   FORMAT "->>>>>>9"
          v-qohi[3] WHEN v-curr   FORMAT "->>>>>>9"
          v-qohi[4]               FORMAT "->>>>>>9"
          v-qohi[5]               FORMAT "->>>>>>9"
          v-cst[1]  WHEN v-cost   FORMAT "->,>>>,>>9.99"
              v-val[1] WHEN NOT v-cost @ v-cst[1] 
          lv-last-fld[1] FORMAT "x(4)" 
          SPACE(1)
          lv-last-fld[2] FORMAT "x(13)"                 /*Task# 02041406*/
          v-custpart[1] WHEN  v-cpart FORMAT "x(13)" 
          v-shipdt[1]  WHEN NOT v-cpart @ v-custpart[1]
          v-shipdt[2] WHEN v-cost
          /* 11091201 - customer name on 2nd line */
          SKIP          
      WITH FRAME detail2 NO-BOX NO-LABELS NO-ATTR-SPACE STREAM-IO WIDTH 200 DOWN.
  DOWN WITH FRAME detail2.
END.


  IF tb_excel THEN 
     PUT STREAM excel UNFORMATTED
         '"' tt-sman + " " + lv-sname                              '",'
         '"' v-cus                                                 '",'
         '"' itemfg.i-no                                           '",'
         '"' itemfg.i-name                                         '",'
         '"' (IF v-curr THEN STRING(v-qohi[1],"->>>>>>9") ELSE "") '",'
         '"' (IF v-curr THEN STRING(v-qohi[2],"->>>>>>9") ELSE "") '",'
         '"' (IF v-curr THEN STRING(v-qohi[3],"->>>>>>9") ELSE "") '",'
         '"' STRING(v-qohi[4],"->>>>>>9")                          '",'
         '"' STRING(v-qohi[5],"->>>>>>9")                          '",'
         '"' (IF v-cost THEN STRING(v-cst[1],"->,>>>,>>9.99")
              ELSE STRING(v-val[1],"->,>>>,>>9.99"))               '",'
         '"' (IF v-cost THEN STRING(v-val[1],"->,>>>,>>9.99") ELSE lv-last-fld[1]) '",'
         '"'  lv-last-fld[2]        '",'
         '"' (IF v-cpart  THEN  v-custpart[1] ELSE v-shipdt[1] )                             '",'
         '"' ( IF  v-sdate AND  v-cost  THEN  v-shipdt[2] ELSE "" )            '",'
         SKIP.

  ASSIGN
   v-cst[2]  = v-cst[2]  + v-cst[1]
   v-val[2]  = v-val[2]  + v-val[1]
   v-qohc[1] = v-qohc[1] + v-qohi[1]
   v-qohc[2] = v-qohc[2] + v-qohi[2]
   v-qohc[3] = v-qohc[3] + v-qohi[3]
   v-qohc[4] = v-qohc[4] + v-qohi[4]
   v-qohc[5] = v-qohc[5] + v-qohi[5]

   v-cus            = ""
   v-cst[1]         = 0
   v-val[1]         = 0
   v-sell-price     = 0
   v-qohi           = 0      
   lv-last-fld      = "".

  IF LAST-OF(tt-cust-no) THEN DO:
    IF v-sub-t THEN DO:
      DISPLAY "            Customer Subtotal:"  @ itemfg.i-name
              v-qohc[1] WHEN v-curr             @ v-qohi[1]
              v-qohc[2] WHEN v-curr             @ v-qohi[2]
              v-qohc[3] WHEN v-curr             @ v-qohi[3]
              v-qohc[4]                         @ v-qohi[4]
              v-qohc[5]                         @ v-qohi[5]
              v-cst[2]  WHEN v-cost             @ v-cst[1]
                  v-val[2] WHEN NOT v-cost      @ v-cst[1]
              lv-last-fld[1]
                  v-val[2] WHEN v-cost          @ lv-last-fld[1]

          WITH FRAME detail.
      DOWN WITH FRAME detail.
      
      IF NOT LAST-OF(tt-sman) THEN
      IF v-break  THEN  PAGE .

      IF tb_excel THEN 
         PUT STREAM excel UNFORMATTED
             SKIP(1)
             '"' ""                          '",'
             '"' ""                          '",'
             '"' ""                          '",'
             '"' "            Customer Subtotal:"                      '",'
             '"' (IF v-curr THEN STRING(v-qohc[1],"->>>>>>9") ELSE "") '",'
             '"' (IF v-curr THEN STRING(v-qohc[2],"->>>>>>9") ELSE "") '",'
             '"' (IF v-curr THEN STRING(v-qohc[3],"->>>>>>9") ELSE "") '",'
             '"' STRING(v-qohc[4],"->>>>>>9")                          '",'
             '"' STRING(v-qohc[5],"->>>>>>9")                          '",'
             '"' (IF v-cost THEN STRING(v-cst[2],"->,>>>,>>9.99")
                  ELSE STRING(v-val[2],"->,>>>,>>9.99"))               '",'
             '"' (IF v-cost THEN STRING(v-val[2],"->,>>>,>>9.99") ELSE lv-last-fld[1]) '",'
             SKIP(1).
    END.

    ASSIGN
     v-cst[3]  = v-cst[3]  + v-cst[2]
     v-val[3]  = v-val[3]  + v-val[2]
     v-qohs[1] = v-qohs[1] + v-qohc[1]
     v-qohs[2] = v-qohs[2] + v-qohc[2]
     v-qohs[3] = v-qohs[3] + v-qohc[3]
     v-qohs[4] = v-qohs[4] + v-qohc[4]
     v-qohs[5] = v-qohs[5] + v-qohc[5]

     v-cst[2] = 0
     v-val[2] = 0
     v-qohc   = 0.
  END.

  IF LAST-OF(tt-sman) THEN DO:
    IF  v-print-sls-tots THEN DO:
    /* 11091201 - option to supress sales rep totals */
    DISPLAY "         Salesperson Subtotal:"    @ itemfg.i-name
            v-qohs[1] WHEN v-sub-t AND v-curr   @ v-qohi[1]
            v-qohs[2] WHEN v-sub-t AND v-curr   @ v-qohi[2]
            v-qohs[3] WHEN v-sub-t AND v-curr   @ v-qohi[3]
            v-qohs[4] WHEN v-sub-t              @ v-qohi[4]
            v-qohs[5] WHEN v-sub-t              @ v-qohi[5]
            v-cst[3]  WHEN v-cost               @ v-cst[1]
            v-val[3] WHEN NOT v-cost        @ v-cst[1]
            lv-last-fld[1]
            v-val[3] WHEN v-cost            @ lv-last-fld[1]

        WITH FRAME detail.
    DOWN WITH FRAME detail.
    IF NOT LAST(tt-sman) THEN
        IF v-break  THEN  PAGE .
    IF tb_excel THEN 
       PUT STREAM excel UNFORMATTED
           SKIP(1)
           '"' ""                          '",'
           '"' ""                          '",'
           '"' ""                          '",'
           '"' "         Salesperson Subtotal:"                      '",'
           '"' (IF v-sub-t AND v-curr THEN STRING(v-qohs[1],"->>>>>>9") ELSE "") '",'
           '"' (IF v-sub-t AND v-curr THEN STRING(v-qohs[2],"->>>>>>9") ELSE "") '",'
           '"' (IF v-sub-t AND v-curr THEN STRING(v-qohs[3],"->>>>>>9") ELSE "") '",'
           '"' (IF v-sub-t THEN STRING(v-qohs[4],"->>>>>>9") ELSE "")                         '",'
           '"' (IF v-sub-t THEN STRING(v-qohs[5],"->>>>>>9") ELSE "")                         '",'
           '"' (IF v-cost THEN STRING(v-cst[3],"->,>>>,>>9.99")
                ELSE STRING(v-val[3],"->,>>>,>>9.99"))               '",'
           '"' (IF v-cost THEN STRING(v-val[3],"->,>>>,>>9.99") ELSE lv-last-fld[1]) '",'
           SKIP(1).
    END.

    ASSIGN
     v-cst[4]  = v-cst[4]  + v-cst[3]
     v-val[4]  = v-val[4]  + v-val[3]
     v-qohg[1] = v-qohg[1] + v-qohs[1]
     v-qohg[2] = v-qohg[2] + v-qohs[2]
     v-qohg[3] = v-qohg[3] + v-qohs[3]
     v-qohg[4] = v-qohg[4] + v-qohs[4]
     v-qohg[5] = v-qohg[5] + v-qohs[5]

     v-cst[3] = 0
     v-val[3] = 0
     v-qohs   = 0.
  END.
             DEF VAR v-all-tot AS DEC.
  /* 11091201 */
  IF LAST(tt-sman) AND v-print-grand-tots THEN DO:
    DISPLAY "                  Grand Total:"    @ itemfg.i-name
            v-qohg[1]  @ v-qohi[1]
            v-qohg[2]  @ v-qohi[2]
            v-qohg[3]  @ v-qohi[3]
            v-qohg[4]  @ v-qohi[4]
            v-qohg[5]  @ v-qohi[5]
            v-cst[4]  WHEN v-cost           @ v-cst[1]
            v-val[4] WHEN NOT v-cost        @ v-cst[1]
            lv-last-fld[1]
            v-val[4] WHEN v-cost            @ lv-last-fld[1]

        WITH FRAME detail.
    DOWN WITH FRAME detail.

    v-all-tot = v-qohg[1] + v-qohg[2] + v-qohg[3] + v-qohg[4] + v-qohg[5].
    IF v-all-tot EQ 0 THEN
        v-all-tot = 1.
     DISPLAY
     "             % of Grand Total:"    @ itemfg.i-name 
            STRING(v-qohg[1] / v-all-tot * 100, "->>9.99%")  @ v-qohi[1] 
            STRING(v-qohg[2] / v-all-tot * 100, "->>9.99%")  @ v-qohi[2]
            STRING(v-qohg[3] / v-all-tot * 100, "->>9.99%")  @ v-qohi[3]
            STRING(v-qohg[4] / v-all-tot * 100, "->>9.99%")  @ v-qohi[4]
            STRING(v-qohg[5] / v-all-tot * 100, "->>9.99%")  @ v-qohi[5]

        WITH FRAME detail.
    DOWN WITH FRAME detail.

    IF tb_excel THEN 
       PUT STREAM excel UNFORMATTED
           SKIP(1)
           '"' ""                          '",'
           '"' ""                          '",'
           '"' ""                          '",'
           '"' "                  Grand Total:"                      '",'
           '"' STRING(v-qohg[1],"->>>>>>9") '",'
           '"' STRING(v-qohg[2],"->>>>>>9") '",'
           '"' STRING(v-qohg[3],"->>>>>>9") '",'
           '"' STRING(v-qohg[4],"->>>>>>9")                         '",'
           '"' STRING(v-qohg[5],"->>>>>>9")                         '",'
           '"' (IF v-cost THEN STRING(v-cst[4],"->,>>>,>>9.99")
                ELSE STRING(v-val[4],"->,>>>,>>9.99"))               '",'
           '"' (IF v-cost THEN STRING(v-val[4],"->,>>>,>>9.99") ELSE lv-last-fld[1]) '",'
           SKIP(1).
  END.

  DELETE tt-file.
END. /* each tt-file */

/* If utilizing a special user id, don't save parameters to that record */
IF v-custom-user EQ USERID("NOSWEAT") THEN DO:
  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
END.
 IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
 END.

SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE produce-report C-Win 
PROCEDURE produce-report :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
  ------------------------------------------------------------------------------*/
  /* fgrep/r-ageinv.i

  Notes:
  - tt-file allows for break by  salesrep, customer, item
  - tt-fg-bin allows for break by tag, job, loc, bin, i-no

  */
  DEF VAR v-rec-date AS DATE NO-UNDO.
  DEF VAR l-first-bin AS LOG NO-UNDO.
  DEF VAR tot-qty AS INT NO-UNDO.
  DEF VAR tot-val AS DEC NO-UNDO.
  DEF VAR AVG-price AS DEC NO-UNDO.
  DEF VAR v-bin-price AS DEC NO-UNDO.
  DEF VAR v-price-uom AS CHAR NO-UNDO.
  DEF VAR v-null AS DEC NO-UNDO.
  DEF VAR v-ord-slsmn AS CHAR NO-UNDO.
  DEF VAR floc AS CHAR NO-UNDO.
  DEF VAR tloc AS CHAR NO-UNDO.
  DEF VAR floc-bin AS CHAR NO-UNDO.
  DEF VAR tloc-bin AS CHAR NO-UNDO.
  DEF VAR lvlIncludeOld AS LOG NO-UNDO.
  DEF VAR v-sales-rep AS CHAR NO-UNDO.
  DEFINE VARIABLE cNewRep AS CHARACTER NO-UNDO.
  DEF VAR lSelected AS LOG INIT YES NO-UNDO.

  ASSIGN
    vdat       = as-of-date
    fslm       = begin_slm
    tslm       = END_slm
    fcus       = begin_cust-no
    tcus       = end_cust-no
    fitm       = begin_i-no
    titm       = end_i-no
    floc       = begin_whse
    tloc       = END_whse
    floc-bin   = begin_loc-bin
    tloc-bin   = end_loc-bin
    fjob       = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) +
  TRIM(begin_job-no) + STRING(INT(begin_job-no2),"99")
    tjob       = FILL(" ",6 - LENGTH(TRIM(end_job-no))) +
  TRIM(end_job-no)   + STRING(INT(end_job-no2),"99")
    v-q-or-v   = rd_show EQ "Quantity"
    v-sub-t    = tb_val-cust
    v-break    = tb_break
    v-cost     = tb_cost
    v-curr     = tb_curr
    v-cpart    = tb_custpart
    v-sdate    = tb_last-ship-date
    v-label    = STRING(v-q-or-v," Qty/ Value")
    v-class    = list_class
    list_class = ""
    sort-opt   = SUBSTR(rd_sort,1,1) 
    lvlIncludeOld = tb_include_old_items.
    lSelected      = tb_cust-list .

    IF lselected THEN DO:
        FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
        IF AVAIL ttCustList THEN ASSIGN fcus = ttCustList.cust-no .
        FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
        IF AVAIL ttCustList THEN ASSIGN tcus = ttCustList.cust-no .
    END.

  DO li = 1 TO NUM-ENTRIES(v-class):
    list_class = list_class + " " + (IF ENTRY(li,v-class) EQ "" THEN "Spaces"
    ELSE ENTRY(li,v-class)).
  END.
  list_class = TRIM(list_class).
  STATUS DEFAULT "Processing...".
  EMPTY TEMP-TABLE tt-file.
  EMPTY TEMP-TABLE tt-fg-bin.
  EMPTY TEMP-TABLE tt-itemfg.


  ASSIGN
  v-cst[1]     = 0
  v-val[1]     = 0
  v-dates      = ?
  ld-last      = ?
  v-qohi       = 0
  v-tot-for-item = 0
  v-sell-price = 0
  ld-last      = 01/01/0001.

  EMPTY TEMP-TABLE tt-fg-bin.
  EMPTY TEMP-TABLE tt-items.


  IF tb_cust-list EQ NO AND (fcus EQ "" AND tcus BEGINS "zzz")
        THEN DO:

      IF fslm GT "" OR NOT tslm BEGINS "zzz" THEN 
      DO:     
          /* Create list of tt-items with all records within the from-to slsm */
          /* range. Actual sales rep for the item is determined in main loop */
          /* below                                                            */

          FOR EACH cust-part 
              WHERE cust-part.company EQ cocode 
              NO-LOCK:

              IF cust-part.spare-char-1 NE "" THEN 
              DO:
                  IF cust-part.spare-char-1 GE fslm
                      AND cust-part.spare-char-1 LE tslm THEN 
                  DO:

                      FIND FIRST tt-items WHERE tt-items.i-no EQ cust-part.i-no
                          NO-LOCK NO-ERROR.
                      IF NOT AVAIL tt-items THEN 
                      DO:
                          {custom/statusMsg.i "'Building Report: Item ' + cust-part.i-no"}
                          CREATE tt-items.
                          tt-items.i-no = cust-part.i-no.
                      END.

                  END.
              END.

          END. /* end of cust-part */

          FOR EACH itemfg WHERE itemfg.company = cocode 
              AND (itemfg.spare-char-3 GT "" OR itemfg.cust-no GT "" )
              AND (itemfg.stat EQ "A" OR lvlIncludeOld)
              NO-LOCK: 

              IF itemfg.cust-no GT "" THEN 
                  FIND cust WHERE cust.cust-no = itemfg.cust-no
                      AND cust.company = cocode NO-LOCK NO-ERROR.

              IF (AVAIL cust AND cust.ACTIVE NE "X" AND cust.sman GT ""
                  AND cust.sman GE fslm AND cust.sman LE tslm)
                  OR (itemfg.spare-char-3 GE fslm 
                  AND itemfg.spare-char-3 LE tslm) THEN 
              DO:

                  FIND FIRST tt-items WHERE tt-items.i-no EQ itemfg.i-no
                      NO-LOCK NO-ERROR.
                  IF NOT AVAIL tt-items THEN 
                  DO:
                      {custom/statusMsg.i "'Building Report: Item ' + itemfg.i-no"}
                      CREATE tt-items.
                      tt-items.i-no = itemfg.i-no.
                  END.  
              END.
          END.
      END.
      ELSE 
      DO:                 

          FOR EACH itemfg NO-LOCK
              WHERE itemfg.company        EQ cocode
              AND itemfg.i-no           GE fitm
              AND itemfg.i-no           LE titm
              AND (itemfg.stat EQ "A" OR lvlIncludeOld)
              USE-INDEX i-no.

              /*         /* Check for active status */                                                */
              /*         IF NOT lvlIncludeOld AND itemStatus(itemfg.company, itemfg.i-no) NE "A" THEN */
              /*             NEXT.                                                                    */
              /*                                                                                      */
              {custom/statusMsg.i "'Building Report: Item ' + itemfg.i-no"}
              CREATE tt-items.
              ASSIGN 
                  tt-items.i-no = itemfg.i-no.

          END.
      END.

  END.
  ELSE DO: 
      FOR EACH cust NO-LOCK
        WHERE cust.company        EQ cocode
        AND cust.cust-no          GE fcus
        AND cust.cust-no          LE tcus
        AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ cust.cust-no
        AND ttCustList.log-fld NO-LOCK) ELSE TRUE),
/*        AND cust.sman             GE fslm */
/*        AND cust.sman             LE tslm,*/

        EACH itemfg NO-LOCK
          WHERE itemfg.company        EQ cust.company
            AND itemfg.cust-no        EQ cust.cust-no
            AND itemfg.i-no           GE fitm
            AND itemfg.i-no           LE titm
            AND (itemfg.stat EQ "A" OR lvlIncludeOld)
            USE-INDEX customer:

/*           /* Check for active status */                                                */
/*           IF NOT lvlIncludeOld AND itemStatus(itemfg.company, itemfg.i-no) NE "A" THEN */
/*               NEXT.                                                                    */
          {custom/statusMsg.i "'Building Report: Item ' + itemfg.i-no"}
          CREATE tt-items.
          ASSIGN tt-items.i-no = itemfg.i-no.

      END.
  END.

  FOR EACH tt-items,
      FIRST itemfg WHERE itemfg.company = cocode
                     AND itemfg.i-no = tt-items.i-no
                   NO-LOCK:

    IF v-class NE "" AND
    LOOKUP(itemfg.class,v-class) EQ 0 THEN NEXT.


/********************** Start section copied from IR2 *******************/

    /* Don't know what these passed values should be */

    DEF VAR v-loc LIKE fg-bin.loc EXTENT 2 . /* begin/end warehouse */
    DEF VAR v-loc-bin LIKE fg-bin.loc-bin EXTENT 2. /* begin/end bin */
    DEF VAR zbal AS LOG INIT NO. /* include zero balances */
    DEF VAR fi_days-old AS INT INIT 0. /* periods to report, 0 matches IR2 */
    DEF VAR v-custown AS LOG .  /* include cust owned */.

    v-custown = tb_cust-whse.

      ASSIGN  v-loc[1]     = floc /* "" */        v-loc[2] = tloc /* "zzzzzzzzzz" */
              v-loc-bin[1] = floc-bin /* "" */    v-loc-bin[2] = tloc-bin /* "zzzzzzzzzzzzzz" */.
      ASSIGN
        age-days[1] = aged-days-1
        age-days[2] = aged-days-2
        age-days[3] = aged-days-3
        age-days[4] = aged-days-4.

      RUN fg/rep/tt-fgbin.p (BUFFER itemfg, vdat, "", "zzzzzzzzzz",
                             v-loc[1], v-loc[2], v-loc-bin[1], v-loc-bin[2],
                             zbal, fi_days-old, YES, v-custown).

      FOR EACH tt-fg-bin
          WHERE tt-fg-bin.company EQ itemfg.company
            AND tt-fg-bin.i-no    EQ itemfg.i-no
          /*     wfk - 10/1- don't know why commented out */
         /*   AND (v-custown OR tb_cust-whse OR
                 (tt-fg-bin.cust-no EQ "" AND tt-fg-bin.loc NE "CUST")) */
           /* Copied from IR2 but does not apply since there is no 'cust whse only' */
           AND (  tb_cust-whse OR
                 (tt-fg-bin.cust-no EQ "" AND tt-fg-bin.loc NE "CUST"))   
          USE-INDEX co-ino
          BREAK BY tt-fg-bin.i-no:

        RUN price-calc (INPUT ROWID(tt-fg-bin), OUTPUT v-bin-price, OUTPUT v-price-uom).

        IF FIRST(tt-fg-bin.i-no)  THEN DO:            
            tt-fg-bin.spare-dec-1 = 0.
        END.

            /*
        RUN sys/ref/convcuom.p(v-price-uom, "EA", 0, 0, 0, 0,
        v-null, OUTPUT v-bin-price ). */

        IF v-price-uom = "M" THEN
            v-bin-price = v-bin-price / 1000.
        ELSE IF v-price-uom EQ "CS" AND AVAIL itemfg THEN
            v-bin-price = v-bin-price / itemfg.case-count.

        tt-fg-bin.spare-dec-1 = tt-fg-bin.spare-dec-1 + (v-bin-price * tt-fg-bin.qty).

        IF tt-fg-bin.qty NE 0 OR zbal THEN DO:
          CREATE tt-itemfg.
          BUFFER-COPY itemfg TO tt-itemfg
          ASSIGN
           tt-itemfg.row-id      = ROWID(itemfg)
           tt-itemfg.job-no      = tt-fg-bin.job-no
           tt-itemfg.job-no2     = tt-fg-bin.job-no2
           tt-itemfg.loc         = tt-fg-bin.loc
           tt-itemfg.loc-bin     = tt-fg-bin.loc-bin
           tt-itemfg.tag         = tt-fg-bin.tag
           tt-itemfg.bin-cust-no = tt-fg-bin.cust-no
           tt-itemfg.part-cust   = STRING(tt-itemfg.part-no,"x(20)") +
                                   STRING(tt-itemfg.cust-no,"x(20)")
           tt-itemfg.loc-bin-tag = STRING(tt-itemfg.loc,"x(10)")         +
                                   STRING(tt-itemfg.loc-bin,"x(10)")     +
                                   STRING(tt-itemfg.tag,"x(20)").
        END.

        ELSE DELETE tt-fg-bin.
     END. /* each tt-fg-bin */

  END. /* each cust ... */

/********************** End section copied from IR2 *******************/
  DEF VAR v-max-days AS INT NO-UNDO.
  v-max-days = 0.
  /* Per IR2 code, now ready to report based on tt-fg-bin and tt-itemfg */
  FOR EACH tt-itemfg USE-INDEX cust-no NO-LOCK,
      FIRST itemfg WHERE ROWID(itemfg) EQ tt-itemfg.row-id NO-LOCK,
         EACH tt-fg-bin
        WHERE tt-fg-bin.company EQ itemfg.company
          AND tt-fg-bin.i-no    EQ itemfg.i-no
          AND tt-fg-bin.job-no  EQ tt-itemfg.job-no
          AND tt-fg-bin.job-no2 EQ tt-itemfg.job-no2
          AND tt-fg-bin.loc     EQ tt-itemfg.loc
          AND tt-fg-bin.loc-bin EQ tt-itemfg.loc-bin
          AND tt-fg-bin.tag     EQ tt-itemfg.tag
          AND tt-fg-bin.cust-no EQ tt-itemfg.bin-cust-no
        USE-INDEX co-ino NO-LOCK
      BREAK BY tt-itemfg.cust-no
            BY tt-itemfg.i-no
            BY tt-itemfg.loc
            BY tt-itemfg.loc-bin
            BY tt-itemfg.job-no
            BY tt-itemfg.job-no2
            :

        FIND FIRST cust WHERE cust.company EQ itemfg.company 
             AND cust.cust-no EQ itemfg.cust-no
             NO-LOCK NO-ERROR.

        /*IF NOT AVAIL cust THEN
            FIND FIRST cust WHERE cust.company = cocode NO-LOCK.*/
        DEF VAR lv-rct-date AS DATE.
        DEF VAR v-buck AS INT.

        v-sales-rep = "" .
        RUN fg/fgSlsRep.p (INPUT itemfg.company,
                     INPUT itemfg.cust-no,
                     INPUT "",
                     INPUT itemfg.i-no,
                     OUTPUT cNewRep).        
        IF AVAIL cust AND cust.ACTIVE NE "X" AND cNewRep EQ "" THEN DO:

          FOR EACH cust-part WHERE cust-part.company = itemfg.company   
              AND cust-part.i-no = itemfg.i-no
              AND cust-part.cust-no EQ cust.cust-no NO-LOCK:

              IF cust-part.spare-char-1 NE "" THEN DO:
                  FIND FIRST sman WHERE sman.company = itemfg.company
                      AND sman.sman = cust-part.spare-char-1 NO-LOCK NO-ERROR.
                  IF AVAIL sman THEN v-sales-rep = sman.sman.
                  LEAVE .
              END.
           END. /* end of cust-part */                 

          IF v-sales-rep EQ "" THEN DO:
            FIND FIRST sman WHERE sman.company = cust.company
                AND sman.sman = cust.sman NO-LOCK NO-ERROR.

            IF cNewRep EQ "" AND AVAIL sman THEN v-sales-rep = sman.sman.
          END.
        END.
        ELSE DO:
            IF AVAIL cust THEN
            FIND FIRST sman WHERE sman.company = cust.company
                AND sman.sman = cust.sman NO-LOCK NO-ERROR.

            IF cNewRep EQ "" AND AVAIL sman THEN v-sales-rep = sman.sman.
        END.
        IF v-sales-rep EQ "" AND cNewRep GT "" THEN 
          v-sales-rep = cNewRep.

        /* change 9/24 */
        IF FIRST-OF(tt-itemfg.i-no) THEN 
            ASSIGN v-qohi = 0.

        lv-rct-date = tt-fg-bin.first-date.

        v-buck = IF vdat - lv-rct-date LT aged-days-1 THEN 1 ELSE
                 IF vdat - lv-rct-date LT aged-days-2 THEN 2 ELSE
                 IF vdat - lv-rct-date LT aged-days-3 THEN 3 ELSE
                 IF vdat - lv-rct-date LT aged-days-4 THEN 4 ELSE 5.

        v-qohi[v-buck] = v-qohi[v-buck] + tt-fg-bin.qty.


        v-qty-2     = v-qohi[1] + v-qohi[2] + v-qohi[3] +
                      v-qohi[4] + v-qohi[5] + v-qohi[6].

        v-sell-price[v-buck] = v-sell-price[v-buck] + tt-fg-bin.spare-dec-1.

        IF itemfg.prod-uom EQ "EA" THEN
        v-u-cst = tt-fg-bin.std-tot-cost.
        ELSE
        RUN sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                               tt-fg-bin.std-tot-cost, OUTPUT v-u-cst).

        FIND LAST oe-ordl WHERE oe-ordl.company EQ cocode
                    AND oe-ordl.job-no  EQ tt-fg-bin.job-no
                    AND oe-ordl.job-no2 EQ tt-fg-bin.job-no2
                    AND oe-ordl.i-no    EQ tt-fg-bin.i-no
                    AND tt-fg-bin.job-no GT ""
        USE-INDEX ITEM 
        NO-LOCK NO-ERROR.

        IF tt-fg-bin.job-no GT "" THEN DO:
            FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
                                 AND job-hdr.job-no EQ tt-fg-bin.job-no
                                 AND job-hdr.job-no2 EQ tt-fg-bin.job-no2
                               NO-LOCK NO-ERROR.
            IF AVAIL job-hdr THEN
            DO:
              FIND FIRST oe-ordl WHERE
              oe-ordl.company EQ job-hdr.company AND
              oe-ordl.ord-no  EQ job-hdr.ord-no AND
              oe-ordl.i-no    EQ job-hdr.i-no
              NO-LOCK NO-ERROR.
              RELEASE job-hdr.
            END.
        END.
        IF NOT AVAIL oe-ordl THEN DO:
                    FIND LAST oe-ordl WHERE oe-ordl.company EQ cocode
                                AND oe-ordl.job-no  EQ tt-fg-bin.job-no
                                AND oe-ordl.job-no2 EQ tt-fg-bin.job-no2
                                AND oe-ordl.i-no    EQ tt-fg-bin.i-no
                    USE-INDEX ITEM 
                    NO-LOCK NO-ERROR.


        END.
        IF AVAIL oe-ordl THEN DO:

          ASSIGN v-u-val  = oe-ordl.t-price / oe-ordl.qty
                 lv-case-count = oe-ordl.cas-cnt.
          /* Always blank for now unless they make it a mod */
          v-ord-slsmn = "" /* oe-ordl.s-man[1] */.
        END.

        ELSE DO:
          lv-case-count = itemfg.case-count.

          IF itemfg.sell-uom EQ "EA" THEN
            v-u-val = itemfg.sell-price.
          ELSE
          IF itemfg.sell-uom = "CS" AND lv-case-count <> 0 THEN
            v-u-val = itemfg.sell-price / lv-case-count.
          ELSE
            RUN sys/ref/convcuom.p(itemfg.sell-uom, "EA", 0, 0, 0, 0,
                                   itemfg.sell-price, OUTPUT v-u-val).

        END.

        IF v-u-cst EQ ? THEN v-u-cst = 0.
        IF v-u-val EQ ? THEN v-u-val = 0.

        IF NOT tb_neg-sale OR (v-qty-2 * v-u-val) GT 0 THEN
        ASSIGN v-cst[1] = v-cst[1] + (tt-fg-bin.qty * v-u-cst)
               v-val[1] = v-val[1] + (tt-fg-bin.qty * v-u-val).

      /* End Code from r-ageinv.w before create of tt-file */
      IF vdat - tt-fg-bin.first-date GT v-max-days 
            AND tt-fg-bin.qty NE 0 /* 9/18 - wfk - trying this since was wrong */
          THEN DO:

          v-max-days = vdat - tt-fg-bin.first-date.
      END.
      IF LAST-OF(tt-itemfg.i-no) THEN DO:

          /* Hopefully these are calculated the same way as in 
             the original IR12 */

         /* Conversion of quantity to value */
        /* change 9/24 moved from other section since v-sell-price wasn't available there */
        IF NOT v-q-or-v THEN DO:
         v-qty = v-qohi[1] + v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].

         DO v = 1 TO 5:

            IF rd_price = "Avg" THEN
/*              v-qohi[v] = v-val[1] / v-qty * v-qohi[v]. */
                .
            ELSE 
              v-qohi[v] = v-sell-price[v].
            IF v-qohi[v] EQ ? THEN v-qohi[v] = 0.
         END.
        END.
         {custom/statusMsg.i "'Building Report Section 2: Item ' + tt-itemfg.i-no"}
         CREATE tt-file.
          ASSIGN
            tt-file.tt-sman    = (IF v-ord-slsmn GT "" THEN v-ord-slsmn ELSE v-sales-rep)
            tt-file.tt-cust-no = IF AVAIL cust THEN cust.cust-no ELSE ""
            tt-file.tt-i-no    = itemfg.i-no
            tt-file.tt-cst[1]  = v-cst[1]
            tt-file.tt-val[1]  = (IF v-qohi[1] NE 0 OR v-qohi[2] NE 0 OR v-qohi[3] NE 0 OR v-qohi[4] NE 0 OR v-qohi[5] NE 0 THEN v-val[1] ELSE 0)
            tt-file.tt-qohi[1] = v-qohi[1]
            tt-file.tt-qohi[2] = v-qohi[2]
            tt-file.tt-qohi[3] = v-qohi[3]
            tt-file.tt-qohi[4] = v-qohi[4]
            tt-file.tt-qohi[5] = v-qohi[5]
            tt-file.tt-days    = v-max-days /* vdat - tt-fg-bin.first-date */
            tt-file.tt-sell-price[1] = v-sell-price[1]
            tt-file.tt-sell-price[2] = v-sell-price[2]
            tt-file.tt-sell-price[3] = v-sell-price[3]
            tt-file.tt-sell-price[4] = v-sell-price[4]
            tt-file.tt-sell-price[5] = v-sell-price[5]
            .


         ASSIGN v-ord-slsmn = "" 
                    tot-qty = 0
                    tot-val = 0
                  avg-price = 0.

         IF  /* NOT v-q-or-v AND */ rd_price = "AVG" THEN DO:
             FOR EACH bf-tt-fg-bin WHERE bf-tt-fg-bin.i-no = tt-file.tt-i-no
                                    NO-LOCK.
                 tot-qty = tot-qty + bf-tt-fg-bin.qty.
                 tot-val = tot-val + bf-tt-fg-bin.spare-dec-1.
             END.
             IF tot-qty NE 0 THEN
               avg-price = tot-val / tot-qty.
             DO v = 1 TO 5:
                 IF NOT v-q-or-v  THEN
                   tt-file.tt-qohi[v] =  avg-price * tt-file.tt-qohi[v].
                 ELSE
                     /* test for fixing avg value */
                   tt-file.tt-sell-price[v] = avg-price * tt-file.tt-qohi[v].
             END.


         END.
            ASSIGN 
                v-cst = 0
                v-val = 0
                v-qohi = 0
                v-max-days = 0
                v-sell-price = 0.

      END.
  END. /* each tt-itemfg */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reduce_negatives C-Win 
PROCEDURE reduce_negatives :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Reducing negative quantities */
    DO v-q = 1 TO 5:
      IF v-qohi[v-q] LT 0 THEN DO:
          /* If can wipe out the negative then move it to [6] */
          v-tot-positive = 0.
          DO v = 1 TO 5:
            IF v-qohi[v] GT 0 THEN
               v-tot-positive = v-tot-positive + v-qohi[v].
          END.

          IF v-tot-positive GE abs(v-qohi[v-q]) THEN
            ASSIGN v-qohi[6]   = v-qohi[6] + v-qohi[v-q]
                   v-qohi[v-q] = 0.
      END.


      IF v-qohi[6] LT 0 THEN DO:

        ASSIGN
         v-qty     = v-qohi[6] * -1
         v-qohi[6] = 0.

        DO v = 5 TO 1 BY -1:
          IF v-qohi[v] GT 0 THEN
            ASSIGN
             v-red     = MIN(v-qty,v-qohi[v])
             v-qohi[v] = v-qohi[v] - v-red
             v-qty     = v-qty     - v-red.

          IF v-qty LE 0 THEN LEAVE.
        END.

        IF v-qty GT 0 THEN v-qohi[6] = v-qohi[6] - v-qty.

      END.      

    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
FOR EACH tt-items.
        DELETE tt-items.
  END.

  RUN build-report
     (  aged-days-1,
        aged-days-2,
        aged-days-3,
        aged-days-4,
        as-of-date,
        begin_cust-no,
        begin_i-no,
        begin_job-no,
        begin_job-no2,
        begin_loc-bin,
        begin_slm,
        begin_whse,
        end_cust-no,
        end_i-no,
        end_job-no,
        end_job-no2,
        end_loc-bin,
        end_slm,
        end_whse,
        fi_file,
        lbl_show,
        lbl_show2,
        lbl_sort,
        lines-per-page,
        list_class,
        lv-font-name,
        lv-font-no,
        lv-ornt,
        rd-dest,
        rd_price,
        rd_show,
        rd_show2,
        rd_sort,
        tb_break,
        tb_cost,
        tb_curr,
        tb_cust-whse,
        tb_custpart,
        tb_excel,
        tb_last-ship-date,
        tb_neg-sale,
        tb_runExcel,
        tb_val-cust,
        td-show-parm).

  SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-custom C-Win 
PROCEDURE set-custom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-custom AS LOG NO-UNDO.
DEF VAR v-custom AS LOG NO-UNDO.

v-custom = ip-custom.
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN    
     begin_cust-no:sensitive = v-custom
     end_cust-no:sensitive = v-custom
     begin_i-no:sensitive = v-custom
     end_i-no:sensitive = v-custom
     begin_job-no:sensitive = v-custom
     begin_job-no2:sensitive = v-custom
     end_job-no:sensitive = v-custom
     end_job-no2:sensitive = v-custom
     begin_whse:sensitive = v-custom
     end_whse:sensitive = v-custom
     begin_loc-bin:sensitive = v-custom
     end_loc-bin:sensitive = v-custom
     list_class:sensitive = v-custom
/*      aged-days-1:sensitive = v-custom */
/*      aged-days-2:sensitive = v-custom */
/*      aged-days-3:sensitive = v-custom */
/*      aged-days-4:sensitive = v-custom */
     lbl_show:sensitive = v-custom
     rd_show:sensitive = v-custom 
     rd_price:sensitive = v-custom
     lbl_show2:sensitive = v-custom
     rd_show2:sensitive = v-custom
     lbl_sort:sensitive = v-custom
     rd_sort:sensitive = v-custom
     tb_cust-whse:sensitive = v-custom
     tb_cost:sensitive = v-custom
     tb_break:sensitive = v-custom
     tb_neg-sale:sensitive = v-custom
     tb_curr:sensitive = v-custom
     tb_val-cust:sensitive = v-custom
     tb_last-ship-date:SENSITIVE = v-custom
     tb_custpart:sensitive = v-custom
     tb_cust-list:SENSITIVE = v-custom
     btnCustList:SENSITIVE = v-custom .
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCustRange C-Win 
PROCEDURE SetCustRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
        begin_cust-no:SENSITIVE = NOT iplChecked
        end_cust-no:SENSITIVE = NOT iplChecked
        begin_cust-no:VISIBLE = NOT iplChecked
        end_cust-no:VISIBLE = NOT iplChecked
        btnCustList:SENSITIVE = iplChecked
       .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set_column_values C-Win 
PROCEDURE set_column_values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT v-cost THEN DO:
        IF v-cpart AND v-sdate  THEN DO: 
            ASSIGN 
                lv-last-fld[2]  = itemfg.part-no 
                v-custpart[1]   = v-shipdt[1] .
         END.
         ELSE IF v-cpart AND NOT v-sdate  THEN DO:
             ASSIGN
                 lv-last-fld[2]  = itemfg.part-no
                 v-custpart[1] = "" .
         END.
         ELSE IF v-sdate THEN DO:
             ASSIGN 
                 lv-last-fld[2]  = v-shipdt[1]  
                 v-shipdt[1]     = ""
                 v-shipdt[2]     = ""  .
         END.
         ELSE IF NOT v-sdate AND NOT v-cpart THEN DO:
             ASSIGN
                 lv-last-fld[2] = "".
         END.
    END.

    IF v-cost THEN DO:
        IF v-cpart AND v-sdate THEN DO:
            ASSIGN
                v-custpart = itemfg.part-no .
        END.
        ELSE IF v-cpart AND NOT v-sdate THEN DO:
            ASSIGN
                v-custpart = itemfg.part-no .
        END.
        ELSE IF NOT v-cpart AND v-sdate THEN DO:
            ASSIGN
                v-custpart  = v-shipdt[1]  
                v-shipdt[2] = "".
        END.
    END.

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
  DEF VAR lv-frame-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field2-hdl AS HANDLE NO-UNDO.
  DEF VAR parm-fld-list AS cha NO-UNDO.
  DEF VAR parm-lbl-list AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-label AS cha NO-UNDO.

  ASSIGN
  lv-frame-hdl = FRAME {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD
  lv-field-hdl = lv-group-hdl:FIRST-CHILD.

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + ",".
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                  END.
                  lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
              END.       
           END.                 
        END.            
     lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
  END.

  PUT SPACE(28)
      "< Selection Parameters >"
      SKIP(1).

  DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
    IF ENTRY(i,parm-fld-list) NE "" OR
       entry(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                 trim(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE which-bucket C-Win 
PROCEDURE which-bucket PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-days AS INT NO-UNDO.
  DEF OUTPUT PARAM op-extent AS INT NO-UNDO.


  op-extent = IF ip-days LT aged-days-1 THEN 1 ELSE
              IF ip-days LT aged-days-2 THEN 2 ELSE
              IF ip-days LT aged-days-3 THEN 3 ELSE
              IF ip-days LT aged-days-4 THEN 4 ELSE 5.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION itemStatus C-Win 
FUNCTION itemStatus RETURNS CHARACTER
  (ipcCompany AS CHAR, ipcIno AS CHAR ) :
   
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  
  RETURN "".
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

