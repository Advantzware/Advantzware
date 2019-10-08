&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"ASI SmartNavBrowser Object Template with Wizard.

Use this template to create a new SmartNavBrowser object with the assistance of the SmartBrowser Wizard. When completed, this object can then be drawn onto any 'smart' container such as a SmartWindow, SmartDialog or SmartFrame."
*/
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  oeinq\b-oeinvl.w

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
&SCOPED-DEFINE browseOnly
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.
{methods\defines\hndldefs.i}
DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR lv-frst-rowid2 AS ROWID NO-UNDO.
DEF VAR lv-last-rowid2 AS ROWID NO-UNDO.
DEF VAR ld-cost LIKE ar-invl.cost NO-UNDO.
DEF VAR ld-cost-uom AS CHAR NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "inv-date" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Inv Date" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR ld-price LIKE ar-invl.unit-pr NO-UNDO.
DEF VAR v-col-move AS LOG INIT YES NO-UNDO.

&SCOPED-DEFINE key-phrase ar-invl.company EQ cocode AND ar-invl.posted EQ YES

&SCOPED-DEFINE for-each1                            ~
    FOR EACH ar-invl ~
        WHERE {&key-phrase}                      ~
          AND ar-invl.cust-no   >= fi_cust-no   ~
          AND ar-invl.i-no      BEGINS fi_i-no      ~
          AND ar-invl.est-no    >= fi_est-no    ~
          AND ar-invl.part-no   >= fi_part-no   ~
          AND ar-invl.po-no     >= fi_po-no     ~
          AND ar-invl.actnum    >= fi_actnum    ~
          AND (ar-invl.inv-no   EQ     fi_inv-no OR fi_inv-no EQ 0) ~
          AND (ar-invl.bol-no   EQ     fi_bol-no OR fi_bol-no EQ 0) ~
          AND (ar-invl.ord-no   EQ     fi_ord-no )

&SCOPED-DEFINE for-each11                           ~
    FOR EACH ar-invl ~
        WHERE {&key-phrase}

&SCOPED-DEFINE for-each2                     ~
    FIRST ar-inv                             ~
    WHERE ar-inv.x-no EQ ar-invl.x-no        ~
      AND ar-inv.inv-date GE fi_date         ~
      AND ((ar-inv.due GT 0 AND tb_open) OR  ~
           (ar-inv.due LE 0 AND tb_paid))    ~
    NO-LOCK,                                 ~
    FIRST cust OF ar-inv NO-LOCK

&SCOPED-DEFINE sortby-log                                                                                                                                ~
    IF lv-sort-by EQ "ord-no"  THEN STRING(ar-invl.ord-no,"9999999999") ELSE ~
    IF lv-sort-by EQ "actnum"  THEN ar-invl.actnum ELSE ~
    IF lv-sort-by EQ "cust-no" THEN ar-invl.cust-no ELSE ~
    IF lv-sort-by EQ "i-no"    THEN ar-invl.i-no ELSE ~
    IF lv-sort-by EQ "est-no"  THEN ar-invl.est-no ELSE ~
    IF lv-sort-by EQ "inv-no"  THEN STRING(ar-invl.inv-no,"9999999999") ELSE ~
    IF lv-sort-by EQ "inv-qty"  THEN STRING(ar-invl.inv-qty,"9999999999") ELSE ~
    IF lv-sort-by EQ "bol-no"  THEN STRING(ar-invl.bol-no,"9999999999") ELSE ~
    IF lv-sort-by EQ "po-no"   THEN ar-invl.po-no ELSE ~
    IF lv-sort-by EQ "part-no" THEN ar-invl.part-no ELSE ~
    IF lv-sort-by EQ "unit-pr" THEN STRING(get-price-disc(),"-9999999999") ELSE ~
    IF lv-sort-by EQ "pr-qty-uom" THEN ar-invl.pr-qty-uom ELSE ~
                                    STRING(YEAR(ar-inv.inv-date),"9999") + STRING(MONTH(ar-inv.inv-date),"99") + STRING(DAY(ar-inv.inv-date),"99")

&SCOPED-DEFINE sortby BY ar-invl.inv-no BY ar-invl.bol-no DESC BY ar-invl.i-no BY ar-invl.line

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES oe-ordl
&Scoped-define FIRST-EXTERNAL-TABLE oe-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ordl.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ar-invl ar-inv cust

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table ar-invl.inv-no ar-invl.bol-no ~
ar-invl.cust-no ar-inv.inv-date ar-invl.actnum ar-invl.i-no ar-invl.part-no ~
ar-invl.ord-no ar-invl.po-no ar-invl.est-no get-price-disc() @ ld-price ~
ar-invl.inv-qty ar-invl.pr-qty-uom getCost() @ ld-cost getCostUOM() @ ld-cost-uom 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ar-invl.inv-no ~
ar-invl.bol-no ar-invl.cust-no ar-inv.inv-date ar-invl.actnum ar-invl.i-no ~
ar-invl.part-no ar-invl.ord-no ar-invl.po-no ar-invl.est-no ~
ar-invl.pr-qty-uom 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table ar-invl ar-inv
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table ar-invl
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table ar-inv
&Scoped-define QUERY-STRING-Browser-Table FOR EACH ar-invl WHERE ar-invl.company = oe-ordl.company ~
  AND ar-invl.ord-no = oe-ordl.ord-no NO-LOCK, ~
      EACH ar-inv WHERE ar-inv.x-no = ar-invl.x-no NO-LOCK, ~
      EACH cust OF ar-inv NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH ar-invl WHERE ar-invl.company = oe-ordl.company ~
  AND ar-invl.ord-no = oe-ordl.ord-no NO-LOCK, ~
      EACH ar-inv WHERE ar-inv.x-no = ar-invl.x-no NO-LOCK, ~
      EACH cust OF ar-inv NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table ar-invl ar-inv cust
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table ar-invl
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table ar-inv
&Scoped-define THIRD-TABLE-IN-QUERY-Browser-Table cust


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_open tb_paid fi_inv-no fi_cust-no fi_date ~
fi_actnum fi_i-no fi_part-no fi_po-no fi_bol-no fi_est-no btn_go btn_show btn_move ~
Browser-Table RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_sort-by tb_open tb_paid fi_inv-no ~
fi_cust-no fi_date fi_actnum fi_i-no fi_part-no fi_ord-no fi_po-no ~
fi_bol-no fi_est-no FI_moveCol

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-price-disc B-table-Win 
FUNCTION get-price-disc RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCost B-table-Win 
FUNCTION getCost RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCostUOM B-table-Win 
FUNCTION getCostUOM RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_show 
     LABEL "&Show All" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_move 
     LABEL "&Move" 
     SIZE 12 BY 1.

DEFINE VARIABLE fi_actnum AS CHARACTER FORMAT "X(25)":U 
     LABEL "GL Acct#" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_bol-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "BOL#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_cust-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Inv Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_est-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Est#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_inv-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Order#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_part-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cust Part#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_po-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cust PO#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 5.

DEFINE VARIABLE tb_open AS LOGICAL INITIAL yes 
     LABEL "Open Invoices" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE tb_paid AS LOGICAL INITIAL yes 
     LABEL "Paid Invoices" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      ar-invl, 
      ar-inv, 
      cust SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      ar-invl.inv-no COLUMN-LABEL "Inv#" FORMAT ">>>>>>>>":U WIDTH 9 LABEL-BGCOLOR 14
      ar-invl.bol-no COLUMN-LABEL "BOL#" FORMAT ">>>>>>>>":U WIDTH 9 LABEL-BGCOLOR 14
      ar-invl.cust-no COLUMN-LABEL "Cust#" FORMAT "x(8)":U WIDTH 12 LABEL-BGCOLOR 14
      ar-inv.inv-date COLUMN-LABEL "Inv Date" FORMAT "99/99/9999":U
            WIDTH 15
      ar-invl.actnum COLUMN-LABEL "GL Acct#" FORMAT "x(25)":U WIDTH 30 LABEL-BGCOLOR 14
      ar-invl.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U WIDTH 20 LABEL-BGCOLOR 14
      ar-invl.part-no COLUMN-LABEL "Cust Part#" FORMAT "x(15)":U
            WIDTH 20 LABEL-BGCOLOR 14
      ar-invl.ord-no FORMAT ">>>>>>>>":U WIDTH 9 LABEL-BGCOLOR 14
      ar-invl.po-no COLUMN-LABEL "Cust PO#" FORMAT "x(15)":U WIDTH 20 LABEL-BGCOLOR 14
      ar-invl.est-no COLUMN-LABEL "Est#" FORMAT "x(8)":U WIDTH 12 LABEL-BGCOLOR 14
      get-price-disc() @ ld-price FORMAT ">>,>>>,>>9.99<<<<":U
	ar-invl.inv-qty COLUMN-LABEL "Qty" FORMAT ">>>>>>>>":U WIDTH 9 LABEL-BGCOLOR 14
      ar-invl.pr-qty-uom FORMAT "x(4)":U WIDTH 7
      getCost() @ ld-cost
      getCostUOM() @ ld-cost-uom COLUMN-LABEL "Cost!UOM" WIDTH 6
  ENABLE
      ar-invl.inv-no
      ar-invl.bol-no
      ar-invl.cust-no
      ar-inv.inv-date
      ar-invl.actnum
      ar-invl.i-no
      ar-invl.part-no
      ar-invl.ord-no
      ar-invl.po-no
      ar-invl.est-no
	ar-invl.inv-qty
      ar-invl.pr-qty-uom
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 178 BY 15
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_sort-by AT ROW 4.81 COL 73 COLON-ALIGNED
     "Browser Col. Mode:" VIEW-AS TEXT
          SIZE 22.6 BY .62 AT ROW 5.00 COL 110.2 WIDGET-ID 6
          FONT 6
     FI_moveCol AT ROW 4.81 COL 131.8 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     tb_open AT ROW 1.24 COL 4
     tb_paid AT ROW 2.43 COL 4
     fi_inv-no AT ROW 1.24 COL 32 COLON-ALIGNED
     fi_cust-no AT ROW 1.24 COL 59 COLON-ALIGNED
     fi_date AT ROW 1.24 COL 84 COLON-ALIGNED
     fi_actnum AT ROW 1.24 COL 112 COLON-ALIGNED
     fi_i-no AT ROW 2.43 COL 59 COLON-ALIGNED
     fi_part-no AT ROW 2.43 COL 97 COLON-ALIGNED
     fi_ord-no AT ROW 2.43 COL 130 COLON-ALIGNED
     fi_po-no AT ROW 3.62 COL 59 COLON-ALIGNED
     fi_bol-no AT ROW 3.62 COL 100 COLON-ALIGNED
     fi_est-no AT ROW 3.62 COL 130 COLON-ALIGNED
     btn_go AT ROW 4.81 COL 21
     btn_show AT ROW 4.81 COL 36
     btn_move AT ROW 4.81 COL 51
     Browser-Table AT ROW 6 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser Template
   External Tables: asi.oe-ordl
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 20.14
         WIDTH              = 148.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/navbrows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB Browser-Table btn_show F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       ar-invl.inv-no:AUTO-RESIZE IN BROWSE Browser-Table = TRUE
       ar-invl.bol-no:AUTO-RESIZE IN BROWSE Browser-Table = TRUE
       ar-invl.cust-no:AUTO-RESIZE IN BROWSE Browser-Table = TRUE
       ar-inv.inv-date:AUTO-RESIZE IN BROWSE Browser-Table = TRUE
       ar-invl.actnum:AUTO-RESIZE IN BROWSE Browser-Table = TRUE
       ar-invl.i-no:AUTO-RESIZE IN BROWSE Browser-Table = TRUE
       ar-invl.part-no:AUTO-RESIZE IN BROWSE Browser-Table = TRUE
       ar-invl.ord-no:AUTO-RESIZE IN BROWSE Browser-Table = TRUE
       ar-invl.po-no:AUTO-RESIZE IN BROWSE Browser-Table = TRUE
       ar-invl.est-no:AUTO-RESIZE IN BROWSE Browser-Table = TRUE.
	ar-invl.inv-qty:AUTO-RESIZE IN BROWSE Browser-Table = TRUE.

/* SETTINGS FOR FILL-IN fi_ord-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.ar-invl WHERE ASI.oe-ordl ...,ASI.ar-inv WHERE ASI.ar-invl ...,ASI.cust OF ASI.ar-inv"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ",,"
     _JoinCode[1]      = "ASI.ar-invl.company = ASI.oe-ordl.company
  AND ASI.ar-invl.ord-no = ASI.oe-ordl.ord-no"
     _JoinCode[2]      = "ASI.ar-inv.x-no = ASI.ar-invl.x-no"
     _FldNameList[1]   > ASI.ar-invl.inv-no
"ar-invl.inv-no" "Inv#" ">>>>>>>>" "integer" ? ? ? ? ? ? yes ? no no "9" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.ar-invl.bol-no
"ar-invl.bol-no" "BOL#" ">>>>>>>>" "integer" ? ? ? ? ? ? yes ? no no "9" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.ar-invl.cust-no
"ar-invl.cust-no" "Cust#" ? "character" ? ? ? ? ? ? yes ? no no "12" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.ar-inv.inv-date
"ar-inv.inv-date" "Inv Date" ? "date" ? ? ? ? ? ? yes ? no no "15" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.ar-invl.actnum
"ar-invl.actnum" "GL Acct#" ? "character" ? ? ? ? ? ? yes ? no no "30" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.ar-invl.i-no
"ar-invl.i-no" "FG Item#" ? "character" ? ? ? ? ? ? yes ? no no "20" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.ar-invl.part-no
"ar-invl.part-no" "Cust Part#" ? "character" ? ? ? ? ? ? yes ? no no "20" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.ar-invl.ord-no
"ar-invl.ord-no" ? ">>>>>>>>" "integer" ? ? ? ? ? ? yes ? no no "9" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.ar-invl.po-no
"ar-invl.po-no" "Cust PO#" ? "character" ? ? ? ? ? ? yes ? no no "20" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.ar-invl.est-no
"ar-invl.est-no" "Est#" "x(8)" "character" ? ? ? ? ? ? yes ? no no "12" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"get-price-disc() @ ld-price" ? ">>,>>>,>>9.99<<<<" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.ar-invl.pr-qty-uom
"ar-invl.pr-qty-uom" ? ? "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"getCost() @ ld-cost" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"getCostUOM() @ ld-cost-uom" "Cost!UOM" ? ? ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartBrowserCues" B-table-Win _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartBrowser,uib,49266
A SmartBrowser is a procedure object that retrieves and visualizes data using a browse.

CREATING A MASTER

Step 1
If necessary, specify an external table (steps 1a-1c). 

An external table completes join criteria for a query. For example, a query on 'Order OF Customer' requires the external table Customer. The external table is supplied by another procedure object--typically a SmartBrowser or SmartViewer.

Step 1a
In the UIB main window, Choose the Procedure button.

Step 1b
In the Procedure Settings dialog, choose Add.

Step 1c
From the Table Selector dialog, select the external table.

Step 2 
Double-click the browse to invoke the Query Builder.

Step 3
Using the Query Builder, specify the tables and fields for the browse.

Step 4 [Optional]
In the Code Section Editor, change the Foreign Keys and/or Sort Options for the browse query. Use the "List..." button to access these sections.

Step 5
Save and close the SmartBrowser master.

INSERTING AN INSTANCE

Step 1
Open or create a SmartContainer, such as a SmartWindow.

Step 2 
Choose the SmartBrowser master from the Object Palette.

Step 3
Draw the SmartBrowser instance into the SmartContainer.

Step 4
Add all necessary SmartLinks between the SmartBrowser and other SmartObjects. 

During assembly, the PROGRESS Advisor suggests links and creates them for you. However, you can also add and remove SmartLinks with the SmartLinks dialog box. To access this dialog box, choose the Procedure button from the UIB main window. Then choose the SmartLinks button from the Procedure Settings dialog box.
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:
  IF NOT CAN-FIND(FIRST sys-ctrl WHERE sys-ctrl.company EQ ar-invl.company
                                   AND sys-ctrl.name EQ 'OECOMM'
                                   AND sys-ctrl.log-fld EQ YES) THEN
  ASSIGN
    ld-cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ?
    ld-cost:BGCOLOR = 8
    ld-cost:FGCOLOR = 8
    ld-cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ?
    ld-cost-uom:BGCOLOR = 8
    ld-cost-uom:FGCOLOR = 8.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.


  ASSIGN
   lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

  IF lv-column-nam BEGINS "job-no" THEN
    ASSIGN
     lv-column-nam = "job-no"
     lv-column-lab = "Job#".

  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

  ELSE
    ASSIGN
     lv-sort-by     = lv-column-nam
     lv-sort-by-lab = lv-column-lab.

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  APPLY "choose" TO btn_go.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
      /* test */
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR phandle AS HANDLE NO-UNDO.
  {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
     "(cust.rec_key,{methods/headers/cust.i})"}
      /* =====*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     tb_open
     tb_paid
     fi_cust-no
     fi_i-no
     fi_po-no
     fi_inv-no
     fi_ord-no
     fi_bol-no
     fi_est-no
     fi_actnum
     fi_part-no
     fi_date.

    {arinq/j-arinq.i}

    RUN dispatch ("row-changed").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn_move
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_move IN FRAME F-Main /* Go */
DO:
  DO WITH FRAME {&FRAME-NAME}:
   RUN move-columns .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show B-table-Win
ON CHOOSE OF btn_show IN FRAME F-Main /* Show All */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     tb_open:SCREEN-VALUE    = "yes"
     tb_paid:SCREEN-VALUE    = "yes"
     fi_cust-no:SCREEN-VALUE = ""
     fi_po-no:SCREEN-VALUE   = ""
     fi_inv-no:SCREEN-VALUE  = ""
     fi_bol-no:SCREEN-VALUE  = ""
     fi_est-no:SCREEN-VALUE  = ""
     fi_actnum:SCREEN-VALUE  = ""
     fi_part-no:SCREEN-VALUE = ""
     fi_date:SCREEN-VALUE    = "01/01/0001".

    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_actnum B-table-Win
ON LEAVE OF fi_actnum IN FRAME F-Main /* GL Acct# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_bol-no B-table-Win
ON LEAVE OF fi_bol-no IN FRAME F-Main /* BOL# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON LEAVE OF fi_cust-no IN FRAME F-Main /* Customer# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON VALUE-CHANGED OF fi_cust-no IN FRAME F-Main /* Customer# */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_date B-table-Win
ON LEAVE OF fi_date IN FRAME F-Main /* Inv Date */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_est-no B-table-Win
ON LEAVE OF fi_est-no IN FRAME F-Main /* Est# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON LEAVE OF fi_i-no IN FRAME F-Main /* FG Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON VALUE-CHANGED OF fi_i-no IN FRAME F-Main /* FG Item# */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_inv-no B-table-Win
ON LEAVE OF fi_inv-no IN FRAME F-Main /* Invoice# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_ord-no B-table-Win
ON LEAVE OF fi_ord-no IN FRAME F-Main /* Order# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no B-table-Win
ON LEAVE OF fi_part-no IN FRAME F-Main /* Cust Part# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no B-table-Win
ON VALUE-CHANGED OF fi_part-no IN FRAME F-Main /* Cust Part# */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_po-no B-table-Win
ON LEAVE OF fi_po-no IN FRAME F-Main /* Cust PO# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_po-no B-table-Win
ON VALUE-CHANGED OF fi_po-no IN FRAME F-Main /* Cust PO# */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_open
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_open B-table-Win
ON VALUE-CHANGED OF tb_open IN FRAME F-Main /* Open Invoices */
DO:
  APPLY "choose" TO btn_go.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_paid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_paid B-table-Win
ON VALUE-CHANGED OF tb_paid IN FRAME F-Main /* Paid Invoices */
DO:
  APPLY "choose" TO btn_go.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
/*&SCOPED-DEFINE key-phrase oe-ordl.company EQ cocode*/
&SCOPED-DEFINE cellColumnDat b-oeinvl

/*{sys/inc/f3help.i}*/
{methods/browsers/setCellColumns.i}

/*  RUN-PROC = "sbo/oerel-recalc-act.p". */
/* {methods/smartrun.i}                  */
/* lr-rel-lib = phandle.                 */
/*RUN sbo/oerel-recalc-act.p PERSISTENT SET lr-rel-lib.*/

SESSION:DATA-ENTRY-RETURN = YES.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "oe-ordl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ordl"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation B-table-Win 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-note B-table-Win 
PROCEDURE disable-note :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAMETER op-enable-note AS LOG  NO-UNDO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation B-table-Win 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    fi_sort-by:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
                              TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
RUN setCellColumns.
  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN 
   ar-invl.inv-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.bol-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.i-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.est-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.ord-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES 
   ar-inv.inv-date:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.actnum:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.part-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.po-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.inv-qty:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.pr-qty-uom:READ-ONLY IN BROWSE {&browse-name} = YES.
    FI_moveCol = "Sort".
  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.
  RUN set-focus.
  APPLY "entry" TO fi_inv-no IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .*/

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL oe-ordl THEN DO:
   ASSIGN
    /*fi_i-no   = oe-ordl.i-no*/
    fi_ord-no = oe-ordl.ord-no.

    {arinq/j-arinq.i}

    RUN dispatch ("display-fields").

    RUN dispatch ("row-changed").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns B-table-Win 
PROCEDURE move-columns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
     Browser-Table:COLUMN-MOVABLE = v-col-move
     Browser-Table:COLUMN-RESIZABLE = v-col-move
     v-col-move = NOT v-col-move
     FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
  DISPLAY FI_moveCol.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigate-browser B-table-Win 
PROCEDURE navigate-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT  PARAMETER ip-nav-type AS CHAR.
  DEF OUTPUT PARAMETER op-nav-type AS CHAR.


  IF ip-nav-type NE "" THEN
  CASE ip-nav-type:
    WHEN "F" THEN RUN dispatch ('get-first':U).
    WHEN "L" THEN RUN dispatch ('get-last':U).
    WHEN "N" THEN RUN dispatch ('get-next':U).
    WHEN "P" THEN RUN dispatch ('get-prev':U).
  END CASE.

  IF ROWID(ar-invl) EQ lv-last-rowid THEN
    op-nav-type = "L".

  IF ROWID(ar-invl) EQ lv-frst-rowid THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigate-browser2 B-table-Win 
PROCEDURE navigate-browser2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT  PARAMETER ip-nav-type AS CHAR.
  DEF OUTPUT PARAMETER op-nav-type AS CHAR.

  DEF VAR hld-rowid AS ROWID NO-UNDO.


  hld-rowid = ROWID(ar-inv).

  IF ip-nav-type NE "" THEN
  CASE ip-nav-type:
    WHEN "F" THEN RUN dispatch ('get-first':U).
    WHEN "L" THEN RUN dispatch ('get-last':U).
    WHEN "N" THEN DO WHILE ROWID(ar-inv) EQ hld-rowid:
                    RUN dispatch ('get-next':U).
                  END.
    WHEN "P" THEN DO WHILE ROWID(ar-inv) EQ hld-rowid:
                    RUN dispatch ('get-prev':U).
                  END.
  END CASE.

  IF ROWID(ar-inv) EQ lv-last-rowid2 THEN
    op-nav-type = "L".

  IF ROWID(ar-inv) EQ lv-frst-rowid2 THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "oe-ordl"}
  {src/adm/template/snd-list.i "ar-invl"}
  {src/adm/template/snd-list.i "ar-inv"}
  {src/adm/template/snd-list.i "cust"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-focus B-table-Win 
PROCEDURE set-focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{methods/setfocus.i {&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-price-disc B-table-Win 
FUNCTION get-price-disc RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER b-ar-invl FOR ar-invl.

  /*to avoid fields phrase error*/
  FIND FIRST b-ar-invl WHERE ROWID(b-ar-invl) EQ ROWID(ar-invl) NO-LOCK.

  RETURN b-ar-invl.unit-pr * (1 - (b-ar-invl.disc / 100)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCost B-table-Win 
FUNCTION getCost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN IF CAN-FIND(FIRST sys-ctrl
                     WHERE sys-ctrl.company EQ ar-invl.company
                       AND sys-ctrl.name EQ 'OECOMM'
                       AND sys-ctrl.log-fld EQ YES) THEN ar-invl.cost ELSE ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCostUOM B-table-Win 
FUNCTION getCostUOM RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF CAN-FIND(FIRST sys-ctrl
              WHERE sys-ctrl.company EQ ar-invl.company
                AND sys-ctrl.name EQ 'OECOMM'
                AND sys-ctrl.log-fld EQ YES) THEN DO:

     IF ar-invl.dscr[1] EQ "" THEN
        RETURN "M".
     ELSE
        RETURN ar-invl.dscr[1].
  END.
  ELSE
     RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


