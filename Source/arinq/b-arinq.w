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

  File:  arinq\b-arinq.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */     

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR phandle AS HANDLE NO-UNDO.
DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR lv-frst-rowid2 AS ROWID NO-UNDO.
DEF VAR lv-last-rowid2 AS ROWID NO-UNDO.

DEF VAR lv-sort-by AS CHAR INIT "inv-no" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Inv# " NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR v-col-move AS LOG INIT TRUE NO-UNDO.
DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR lActive AS LOG NO-UNDO.

DEFINE VARIABLE lButtongoPressed AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iRecordLimit       AS INTEGER   NO-UNDO.
DEFINE VARIABLE dQueryTimeLimit    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cFirstRecKey       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLastRecKey        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lEnableShowAll     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lShowAll           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cBrowseWhereClause AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQueryBuffers      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldBuffer       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIsBreakByUsed     AS LOGICAL   NO-UNDO.

DO TRANSACTION:
     {sys/ref/CustList.i NEW}
    RUN sys/inc/custlistform.p ("AQ1",cocode,OUTPUT ou-log, OUTPUT ou-cust-int ) .
 END.

&SCOPED-DEFINE key-phrase ar-invl.company EQ cocode AND ar-invl.posted EQ YES

/*&SCOPED-DEFINE for-each1                            ~                                                                                                     */
/*    FOR EACH ar-invl                                ~                                                                                                     */
/*        WHERE {&key-phrase}                         ~                                                                                                     */
/*          AND ar-invl.cust-no   BEGINS fi_cust-no   ~                                                                                                     */
/*          AND ar-invl.i-no      BEGINS fi_i-no      ~                                                                                                     */
/*          AND ar-invl.est-no    BEGINS fi_est-no    ~                                                                                                     */
/*          AND ar-invl.part-no   BEGINS fi_part-no   ~                                                                                                     */
/*          AND ar-invl.po-no     BEGINS fi_po-no     ~                                                                                                     */
/*          AND ar-invl.actnum    BEGINS fi_actnum    ~                                                                                                     */
/*          AND ((lookup(ar-invl.cust-no,custcount) <> 0 AND ar-invl.cust-no <> "") OR custcount = "") ~                                                    */
/*          AND (ar-invl.inv-no   EQ     fi_inv-no OR fi_inv-no EQ 0) ~                                                                                     */
/*          AND (ar-invl.bol-no   EQ     fi_bol-no OR fi_bol-no EQ 0) ~                                                                                     */
/*          AND (ar-invl.ord-no   EQ     fi_ord-no OR fi_ord-no EQ 0)                                                                                       */
/*                                                                                                                                                          */
/*&SCOPED-DEFINE for-each11                           ~                                                                                                     */
/*    FOR EACH ar-invl                                ~                                                                                                     */
/*        WHERE ((lookup(ar-invl.cust-no,custcount) <> 0 AND ar-invl.cust-no <> "") OR custcount = "") AND ~                                                */
/*        {&key-phrase}                                                                                                                                     */
/*                                                                                                                                                          */
/*&SCOPED-DEFINE for-each2                     ~                                                                                                            */
/*    FIRST ar-inv                             ~                                                                                                            */
/*    WHERE ar-inv.x-no EQ ar-invl.x-no        ~                                                                                                            */
/*      AND (ar-inv.inv-date GE fi_bdate)        ~                                                                                                          */
/*      AND (ar-inv.inv-date LE fi_edate)        ~                                                                                                          */
/*      AND ((ar-inv.due NE 0 AND tb_open)      ~                                                                                                           */
/*       OR  (ar-inv.due EQ 0 AND NOT tb_open)  ~                                                                                                           */
/*       OR  (ar-inv.due EQ 0 AND tb_paid)      ~                                                                                                           */
/*       OR  (ar-inv.due NE 0 AND NOT tb_paid)) ~                                                                                                           */
/*    NO-LOCK,                                  ~                                                                                                           */
/*    FIRST cust OF ar-inv NO-LOCK                                                                                                                          */
/*                                                                                                                                                          */
/*&SCOPED-DEFINE sortby-log                                                                                                                                ~*/
/*    IF lv-sort-by EQ "ord-no"  THEN STRING(ar-invl.ord-no,"9999999999")                                                                             ELSE ~*/
/*    IF lv-sort-by EQ "actnum"  THEN ar-invl.actnum                                                                                                  ELSE ~*/
/*    IF lv-sort-by EQ "cust-no" THEN ar-invl.cust-no                                                                                                 ELSE ~*/
/*    IF lv-sort-by EQ "i-no"    THEN ar-invl.i-no                                                                                                    ELSE ~*/
/*    IF lv-sort-by EQ "est-no"  THEN ar-invl.est-no                                                                                                  ELSE ~*/
/*    IF lv-sort-by EQ "inv-no"  THEN STRING(ar-invl.inv-no,"9999999999")                                                                             ELSE ~*/
/*    IF lv-sort-by EQ "bol-no"  THEN STRING(ar-invl.bol-no,"9999999999")                                                                             ELSE ~*/
/*    IF lv-sort-by EQ "po-no"   THEN ar-invl.po-no                                                                                                   ELSE ~*/
/*    IF lv-sort-by EQ "part-no" THEN ar-invl.part-no                                                                                                 ELSE ~*/
/*                                    STRING(YEAR(ar-inv.inv-date),"9999") + STRING(MONTH(ar-inv.inv-date),"99") + STRING(DAY(ar-inv.inv-date),"99")        */
/*                                                                                                                                                          */
/*&SCOPED-DEFINE sortby BY ar-invl.inv-no BY ar-invl.bol-no DESC BY ar-invl.i-no BY ar-invl.line                                                            */
/*                                                                                                                                                          */
/*&SCOPED-DEFINE sortby-phrase-asc  ~                                                                                                                       */
/*    BY ({&sortby-log})            ~                                                                                                                       */
/*    {&sortby}                                                                                                                                             */
/*                                                                                                                                                          */
/*&SCOPED-DEFINE sortby-phrase-desc ~                                                                                                                       */
/*    BY ({&sortby-log}) DESC       ~                                                                                                                       */
/*    {&sortby}                                                                                                                                             */

DEF VAR ll-first AS LOG INIT YES NO-UNDO.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ar-invl ar-inv cust

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table ar-invl.inv-no ar-invl.bol-no ~
ar-invl.cust-no ar-inv.inv-date ar-invl.actnum ar-invl.i-no ar-invl.part-no ~
ar-invl.ord-no ar-invl.po-no ar-invl.est-no ar-invl.i-name ar-inv.gross ar-inv.due
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ar-invl.inv-no ~
ar-invl.bol-no ar-invl.cust-no ar-inv.inv-date ar-invl.actnum ar-invl.i-no ~
ar-invl.part-no ar-invl.ord-no ar-invl.po-no ar-invl.est-no ar-inv.gross ar-inv.due
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table ar-invl ar-inv
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table ar-invl
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table ar-inv
&Scoped-define QUERY-STRING-Browser-Table FOR EACH ar-invl WHERE ~{&KEY-PHRASE} ~
      AND ar-invl.x-no < 0 NO-LOCK, ~
      EACH ar-inv WHERE ar-inv.x-no = ar-invl.x-no NO-LOCK, ~
      EACH cust OF ar-inv NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH ar-invl WHERE ~{&KEY-PHRASE} ~
      AND ar-invl.x-no < 0 NO-LOCK, ~
      EACH ar-inv WHERE ar-inv.x-no = ar-invl.x-no NO-LOCK, ~
      EACH cust OF ar-inv NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table ar-invl ar-inv cust
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table ar-invl
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table ar-inv
&Scoped-define THIRD-TABLE-IN-QUERY-Browser-Table cust


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table tb_open tb_paid fi_inv-no ~
fi_cust-no fi_bdate btnCalendar-1 fi_edate btnCalendar-2 fi_actnum fi_i-no ~
fi_part-no fi_ord-no fi_po-no fi_bol-no fi_est-no btn_go btn_show RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_sortBy tb_open tb_paid fi_inv-no ~
fi_cust-no fi_bdate fi_edate fi_actnum fi_i-no fi_part-no fi_ord-no ~
fi_po-no fi_bol-no fi_est-no
// FI_moveCol 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pGetWhereCriteria B-table-Win
FUNCTION pGetWhereCriteria RETURNS CHARACTER 
  (ipcTable AS CHARACTER, ipcCustListCheck AS LOGICAL) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pGetSortCondition B-table-Win
FUNCTION pGetSortCondition RETURNS CHARACTER 
  (ipcSortBy AS CHARACTER,ipcSortLabel AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_show 
     LABEL "&Show All" 
     SIZE 12 BY 1.

DEFINE VARIABLE fi_actnum AS CHARACTER FORMAT "X(25)":U 
     LABEL "GL Acct#" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_bdate AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Begin Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
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

DEFINE VARIABLE fi_edate AS DATE FORMAT "99/99/9999" 
     LABEL "End Date" 
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

/*DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO. */

DEFINE VARIABLE fi_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Order#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_part-no AS CHARACTER FORMAT "X(30)":U 
     LABEL "Cust Part#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_po-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cust PO#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sortBy AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 5.

DEFINE VARIABLE tb_open AS LOGICAL INITIAL YES 
     LABEL "Open Invoices" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE tb_paid AS LOGICAL INITIAL YES 
     LABEL "Paid Invoices" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      ar-invl
    FIELDS(ar-invl.inv-no
      ar-invl.bol-no
      ar-invl.cust-no
      ar-invl.actnum
      ar-invl.i-no
      ar-invl.part-no
      ar-invl.ord-no
      ar-invl.po-no
      ar-invl.est-no
      ar-invl.i-name), 
      ar-inv, 
      cust SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      ar-invl.inv-no COLUMN-LABEL "Inv#" FORMAT ">>>>>>>>":U WIDTH 12
            LABEL-BGCOLOR 14
      ar-invl.bol-no COLUMN-LABEL "BOL#" FORMAT ">>>>>>>>":U WIDTH 9
            LABEL-BGCOLOR 14
      ar-invl.cust-no COLUMN-LABEL "Cust#" FORMAT "x(8)":U WIDTH 12
            LABEL-BGCOLOR 14
      ar-inv.inv-date COLUMN-LABEL "Inv Date" FORMAT "99/99/9999":U
            WIDTH 15 LABEL-BGCOLOR 14
      ar-invl.actnum COLUMN-LABEL "GL Acct#" FORMAT "x(25)":U WIDTH 30
            LABEL-BGCOLOR 14
      ar-invl.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U WIDTH 20
            LABEL-BGCOLOR 14
      ar-invl.part-no COLUMN-LABEL "Cust Part#" FORMAT "x(30)":U
            WIDTH 20 LABEL-BGCOLOR 14
      ar-invl.ord-no FORMAT ">>>>>>>>":U WIDTH 11 LABEL-BGCOLOR 14
      ar-invl.po-no COLUMN-LABEL "Cust PO#" FORMAT "x(15)":U WIDTH 20
            LABEL-BGCOLOR 14
      ar-invl.est-no COLUMN-LABEL "Est#" FORMAT "x(8)":U WIDTH 12
            LABEL-BGCOLOR 14
      ar-invl.i-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      ar-inv.gross COLUMN-LABEL "Inv Amount" FORMAT "->>,>>>,>>9.99":U LABEL-BGCOLOR 14
      ar-inv.due COLUMN-LABEL "Amt Due" FORMAT "->>,>>>,>>9.99":U LABEL-BGCOLOR 14
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
      ar-inv.gross
      ar-inv.due
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 15
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 6 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     fi_sortBy AT ROW 4.76 COL 55.2 COLON-ALIGNED
     tb_open AT ROW 1.24 COL 2
     tb_paid AT ROW 2.43 COL 2
     fi_inv-no AT ROW 1.24 COL 28 COLON-ALIGNED
     fi_cust-no AT ROW 1.24 COL 55 COLON-ALIGNED
     fi_bdate AT ROW 1.24 COL 87 COLON-ALIGNED
     btnCalendar-1 AT ROW 1.24 COL 105
     fi_edate AT ROW 1.24 COL 126 COLON-ALIGNED
     btnCalendar-2 AT ROW 1.24 COL 144
     fi_actnum AT ROW 3.62 COL 10 COLON-ALIGNED
     fi_i-no AT ROW 2.43 COL 55 COLON-ALIGNED
     fi_part-no AT ROW 2.43 COL 87 COLON-ALIGNED
     fi_ord-no AT ROW 2.43 COL 132 COLON-ALIGNED
     fi_po-no AT ROW 3.62 COL 55 COLON-ALIGNED
     fi_bol-no AT ROW 3.62 COL 87 COLON-ALIGNED
     fi_est-no AT ROW 3.62 COL 132 COLON-ALIGNED
     btn_go AT ROW 4.81 COL 17
     btn_show AT ROW 4.81 COL 32
   /*  FI_moveCol AT ROW 4.76 COL 135.8 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     "Browser Col. Mode:" VIEW-AS TEXT
          SIZE 22.6 BY .62 AT ROW 5 COL 114.6 WIDGET-ID 6
          FONT 6 */
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 
         DEFAULT-BUTTON btn_go.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY COMPILE
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
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

ASSIGN 
       btn_show:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi_sortBy IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.ar-invl,ASI.ar-inv WHERE ASI.ar-invl ...,ASI.cust OF ASI.ar-inv"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,,"
     _Where[1]         = "ar-invl.x-no < 0"
     _JoinCode[2]      = "ASI.ar-inv.x-no = ASI.ar-invl.x-no"
     _FldNameList[1]   > ASI.ar-invl.inv-no
"ar-invl.inv-no" "Inv#" ">>>>>>>>" "integer" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.ar-invl.bol-no
"ar-invl.bol-no" "BOL#" ">>>>>>>>" "integer" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.ar-invl.cust-no
"ar-invl.cust-no" "Cust#" ? "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.ar-inv.inv-date
"ar-inv.inv-date" "Inv Date" ? "date" ? ? ? 14 ? ? yes ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.ar-invl.actnum
"ar-invl.actnum" "GL Acct#" ? "character" ? ? ? 14 ? ? yes ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.ar-invl.i-no
"ar-invl.i-no" "FG Item#" ? "character" ? ? ? 14 ? ? yes ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.ar-invl.part-no
"ar-invl.part-no" "Cust Part#" ? "character" ? ? ? 14 ? ? yes ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.ar-invl.ord-no
"ar-invl.ord-no" ? ">>>>>>>>" "integer" ? ? ? 14 ? ? yes ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.ar-invl.po-no
"ar-invl.po-no" "Cust PO#" ? "character" ? ? ? 14 ? ? yes ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.ar-invl.est-no
"ar-invl.est-no" "Est#" "x(8)" "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.ar-invl.i-name
"ar-invl.i-name" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.ar-inv.gross
"ar-inv.gross" "Inv Amount" "->>,>>>,>>9.99" "decimal" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.ar-inv.due
"ar-inv.due" "Amt Due" "->>,>>>,>>9.99" "decimal" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  IF LASTKEY EQ -1 THEN DO:
    APPLY 'ENTRY' TO fi_inv-no IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
  END.
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
  {methods/template/sortindicator.i}
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

  RUN dispatch ("open-query").
  
  {methods/template/sortindicatorend.i}
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

  IF AVAIL cust THEN
  DO:
     {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
     "(cust.rec_key,{methods/headers/cust.i})"}
     {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
       "(CAN-FIND(FIRST notes WHERE notes.rec_key = cust.rec_key))"}
  END.
      /* =====*/
  IF AVAILABLE ar-inv THEN DO:
    IF CAN-FIND(FIRST notes WHERE notes.rec_key = ar-inv.rec_key AND notes.note_type = "i") THEN
     {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
       "(CAN-FIND(FIRST notes WHERE notes.rec_key = ar-inv.rec_key and notes.note_type = 'I'))"}

     RUN custom/setUserPrint.p (ar-inv.company,'ar-inv_.',
                             'begin_inv,end_inv,begin_cust,end_cust,begin_date,end_date,tb_reprint,tb_posted,begin_inv-id,end_inv-id',
                             STRING(ar-inv.inv-no) + ',' + STRING(ar-inv.inv-no) + ',' +
                             ar-inv.cust-no + ',' + ar-inv.cust-no + ',' +
                             STRING(ar-inv.inv-date) + ',' + STRING(ar-inv.inv-date) + ',' +
                             STRING(ar-inv.printed) + ',' + STRING(ar-inv.posted) + ',' + 
                             STRING(ar-inv.x-no) + ',' + STRING(ar-inv.x-no))
                             .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 B-table-Win
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i fi_bdate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 B-table-Win
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
DO:
  {methods/btnCalendar.i fi_edate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  DEF VAR v-cust-no AS CHAR NO-UNDO .
  DEF BUFFER bf-ar-invl  FOR ar-invl .
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
     fi_bdate
     fi_edate
     ll-first = NO
     lButtongoPressed = YES.
     IF fi_cust-no EQ "" AND fi_i-no EQ "" AND fi_po-no EQ "" AND
        fi_inv-no:SCREEN-VALUE EQ "" AND fi_ord-no:SCREEN-VALUE EQ "" AND fi_bol-no:SCREEN-VALUE EQ "" AND 
        fi_est-no:SCREEN-VALUE EQ "" AND fi_actnum:SCREEN-VALUE EQ "" AND fi_part-no EQ "" AND
        fi_bdate:SCREEN-VALUE  EQ "" AND fi_edate:SCREEN-VALUE  EQ "" THEN ll-first = YES.


    RUN dispatch ('open-query').

    GET FIRST Browser-Table .
     IF NOT AVAIL ar-invl AND custcount NE "" THEN DO:
         IF fi_cust-no <> "" THEN DO:
             v-cust-no = fi_cust-no .
         END.
         ELSE DO:                           
           RUN pCheckCustUserRecord(OUTPUT v-cust-no).   
         END.

         FIND FIRST cust WHERE cust.company = cocode 
             AND cust.cust-no = v-cust-no
             AND cust.cust-no NE "" NO-LOCK NO-ERROR. 
         IF AVAIL cust AND ou-log AND LOOKUP(cust.cust-no,custcount) = 0 THEN
             MESSAGE "Customer is not on Users Customer List.  "  SKIP
              "Please add customer to Network Admin - Users Customer List."  VIEW-AS ALERT-BOX WARNING BUTTONS OK.
     END.

    APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
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
     fi_i-no:SCREEN-VALUE    = ""
     fi_po-no:SCREEN-VALUE   = ""
     fi_inv-no:SCREEN-VALUE  = ""
     fi_ord-no:SCREEN-VALUE  = ""
     fi_bol-no:SCREEN-VALUE  = ""
     fi_est-no:SCREEN-VALUE  = ""
     fi_actnum:SCREEN-VALUE  = ""
     fi_part-no:SCREEN-VALUE = ""
     fi_bdate:SCREEN-VALUE  = "01/01/0001" 
     fi_edate:SCREEN-VALUE  = STRING(TODAY) 
     lShowAll = YES.

    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_actnum B-table-Win
ON LEAVE OF fi_actnum IN FRAME F-Main /* GL Acct# */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_bdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_bdate B-table-Win
ON HELP OF fi_bdate IN FRAME F-Main /* Begin Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_bdate B-table-Win
ON LEAVE OF fi_bdate IN FRAME F-Main /* Begin Date */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_bol-no B-table-Win
ON LEAVE OF fi_bol-no IN FRAME F-Main /* BOL# */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON LEAVE OF fi_cust-no IN FRAME F-Main /* Customer# */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF NOT AVAIL cust THEN DO:
       FIND FIRST cust WHERE cust.cust-no = fi_cust-no:SCREEN-VALUE 
           AND cust.company = cocode NO-LOCK NO-ERROR.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON VALUE-CHANGED OF fi_cust-no IN FRAME F-Main /* Customer# */
DO:
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_edate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_edate B-table-Win
ON HELP OF fi_edate IN FRAME F-Main /* End Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_edate B-table-Win
ON LEAVE OF fi_edate IN FRAME F-Main /* End Date */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_est-no B-table-Win
ON LEAVE OF fi_est-no IN FRAME F-Main /* Est# */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON LEAVE OF fi_i-no IN FRAME F-Main /* FG Item# */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON VALUE-CHANGED OF fi_i-no IN FRAME F-Main /* FG Item# */
DO:
  IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_inv-no B-table-Win
ON LEAVE OF fi_inv-no IN FRAME F-Main /* Invoice# */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_ord-no B-table-Win
ON LEAVE OF fi_ord-no IN FRAME F-Main /* Order# */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no B-table-Win
ON LEAVE OF fi_part-no IN FRAME F-Main /* Cust Part# */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no B-table-Win
ON VALUE-CHANGED OF fi_part-no IN FRAME F-Main /* Cust Part# */
DO:
  IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_po-no B-table-Win
ON LEAVE OF fi_po-no IN FRAME F-Main /* Cust PO# */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_po-no B-table-Win
ON VALUE-CHANGED OF fi_po-no IN FRAME F-Main /* Cust PO# */
DO:
  IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME






&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{methods/ctrl-a_browser.i}
&SCOPED-DEFINE cellColumnDat arinqb-arinq
{methods/browsers/setCellColumns.i}

RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'AQ1',
                            INPUT YES,
                            OUTPUT lActive).
{sys/inc/chblankcust.i ""AQ1""}

SESSION:DATA-ENTRY-RETURN = YES.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}
/* Ticket# : 92946
   Hiding this widget for now, as browser's column label should be indicating the column which is sorted by */
fi_sortby:HIDDEN  = TRUE.
fi_sortby:VISIBLE = FALSE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR CustFrom AS CHAR NO-UNDO.
 DEF VAR INvFrom AS INT NO-UNDO.
 DEF VAR itemFrom AS CHAR NO-UNDO.
 DEF VAR bolFrom AS INT NO-UNDO.
 DEF VAR poFrom AS CHAR NO-UNDO.
 DEF VAR CustTo AS CHAR NO-UNDO.
 DEF VAR INvTo AS INT NO-UNDO.
 DEF VAR itemTo AS CHAR NO-UNDO.
 DEF VAR bolTo AS INT NO-UNDO.
 DEF VAR poTo AS CHAR NO-UNDO.

    GET FIRST Browser-Table .                        
    IF AVAIL ar-invl THEN                              
      ASSIGN                                 
        CustFrom    = ar-invl.cust-no        
        invFrom     = ar-invl.inv-no         
        itemFrom    = ar-invl.i-no           
        bolFrom     = ar-invl.bol-no
        poFrom      = ar-invl.po-no .        

    GET LAST Browser-Table .
    IF AVAIL ar-invl THEN
      ASSIGN
        CustTo    = ar-invl.cust-no        
        invTo     = ar-invl.inv-no         
        itemTo    = ar-invl.i-no           
        bolTo     = ar-invl.bol-no
        poTo      = ar-invl.po-no . 

 RUN arinq/rd-invexp.w (CustFrom,invFrom,itemFrom,bolFrom,poFrom,"",CustTo,invTo,itemTo,bolTo,poTo,"","") .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-InvNoteKey B-table-Win 
PROCEDURE get-InvNoteKey :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-inv_rec_key LIKE notes.rec_key.
  DEF OUTPUT PARAM op-inv_header_key AS cha .

  ASSIGN op-inv_rec_key = ar-inv.rec_key
         op-inv_header_key = {methods/headers/ar-inv.i}
      .
  /*
{methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
        "(ar-inv.rec_key,{methods/headers/ar-inv.i})"}
    */
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
    fi_sortby:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
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
  
  RUN Browser_GetRecordAndTimeLimit(
    INPUT  cocode,
    INPUT  "AQ1",
    OUTPUT iRecordLimit,
    OUTPUT dQueryTimeLimit,
    OUTPUT lEnableShowAll
    ).

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
   ar-inv.gross:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-inv.due:READ-ONLY IN BROWSE {&browse-name} = YES
   .

 /* FI_moveCol = "Sort".
  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}. */ 

  /*RUN set-focus.*/
  APPLY 'VALUE-CHANGED':U TO BROWSE {&BROWSE-NAME}.
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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF ll-first THEN RUN set-defaults.
  
  IF lShowAll THEN do:         
        RUN pPrepareAndExecuteQueryForShowAll.
  END.      
  ELSE 
      RUN pPrepareAndExecuteQuery(
          INPUT IF lButtongoPressed THEN NO ELSE YES /* If Button go is pressed then only show the limit alert */ 
          ).

    IF lButtongoPressed THEN 
        lButtongoPressed = NO.
          
  ll-first = NO.
  
  IF AVAILABLE {&first-table-in-query-{&browse-name}} THEN 
  DO:
       GET LAST {&browse-name}.
        IF AVAILABLE {&first-table-in-query-{&browse-name}} THEN
            ASSIGN lv-last-rowid    = ROWID({&first-table-in-query-{&browse-name}})
                   lv-last-rowid2   = ROWID(ar-inv).    
        
        GET FIRST {&browse-name}.
        IF AVAILABLE {&first-table-in-query-{&browse-name}} THEN
            ASSIGN lv-frst-rowid    = ROWID({&first-table-in-query-{&browse-name}})
                lv-frst-rowid2      = ROWID(ar-inv).
  END.                

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT AVAIL cust THEN DO:
        FIND FIRST cust WHERE cust.cust-no = fi_cust-no:SCREEN-VALUE 
            AND cust.company = cocode NO-LOCK NO-ERROR.
    END.
  END.

  RUN dispatch ("display-fields").

  RUN dispatch ("row-changed").

   DO WITH FRAME {&FRAME-NAME}:
      APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}
  APPLY 'ENTRY':U TO fi_inv-no IN FRAME {&FRAME-NAME}.
  DO WITH FRAME {&FRAME-NAME}:
      APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
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
        v-col-move = NOT v-col-move.
    /*    FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
     DISPLAY FI_moveCol.*/
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
         IF ROWID(ar-inv) EQ hld-rowid THEN LEAVE.  /* No eternal loop task06140604*/
    END.
    WHEN "P" THEN DO WHILE ROWID(ar-inv) EQ hld-rowid:
          RUN dispatch ('get-prev':U).
          IF ROWID(ar-inv) EQ hld-rowid THEN LEAVE.  /* No eternal loop*/
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
  {src/adm/template/snd-list.i "ar-invl"}
  {src/adm/template/snd-list.i "ar-inv"}
  {src/adm/template/snd-list.i "cust"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssignCommonRecords B-table-Win
PROCEDURE pAssignCommonRecords PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Assign Common Records for the query
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopcQueryBuffer      AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcFieldBuffer      AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcFieldName        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplIsBreakByUsed    AS LOGICAL   NO-UNDO.  
          
    ASSIGN         
        iopcQueryBuffer   = "ar-invl,ar-inv,cust"
        iopcFieldBuffer   = "ar-invl"
        iopcFieldName     = "inv-no" 
        ioplIsBreakByUsed = YES 
        .
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrepareAndExecuteQueryForShowAll B-table-Win 
PROCEDURE pPrepareAndExecuteQueryForShowAll PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Private procedure to show all records in browse
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cShowAllQuery AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse     AS CHARACTER NO-UNDO.
               
    cShowAllQuery = "FOR EACH ar-invl NO-LOCK"
                    + " WHERE ar-invl.company EQ " + QUOTER(cocode)
                    + " AND ar-invl.posted EQ YES " 
                    + pGetWhereCriteria("ar-invl",YES)                    
                    + " ,FIRST ar-inv  no-lock"
                    + " WHERE ar-inv.x-no EQ ar-invl.x-no "
                    + " AND ar-inv.inv-date GE " + quoter(fi_bdate)    
                    + " AND ar-inv.inv-date LE " + quoter(fi_edate)
                    + " " + pGetWhereCriteria("ar-inv",YES)
                    + " ,FIRST cust OF ar-inv NO-LOCK "
                    + " BY " + pGetSortCondition(lv-sort-by,lv-sort-by-lab) + ( IF ll-sort-asc THEN  "" ELSE " DESC") +  " BY ar-invl.inv-no BY ar-invl.bol-no BY ar-invl.i-no BY ar-invl.line "
                    .             
    RUN Browse_PrepareAndExecuteBrowseQuery(
        INPUT  BROWSE {&BROWSE-NAME}:QUERY, /* Browse Query Handle */      
        INPUT  cShowAllQuery,               /* BRowse Query */             
        INPUT  NO,                          /* Show limit alert? */        
        INPUT  0,                           /* Record limit */             
        INPUT  0,                           /* Time Limit */               
        INPUT  lEnableShowAll,              /* Enable ShowAll Button */    
        OUTPUT cResponse                                                  
        ).
    lShowAll = NO.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrepareAndExecuteQuery B-table-Win 
PROCEDURE pPrepareAndExecuteQuery :
/*------------------------------------------------------------------------------
 Purpose: Private procedure to prepare and execute query in browse
 Notes:
------------------------------------------------------------------------------*/     
    DEFINE INPUT PARAMETER iplInitialLoad    AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cLimitingQuery        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBrowseQuery          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderTypeWhereClause AS CHARACTER NO-UNDO.
    
    RUN pAssignCommonRecords(
        INPUT-OUTPUT cQueryBuffers,
        INPUT-OUTPUT cFieldBuffer,
        INPUT-OUTPUT cFieldName,
        INPUT-OUTPUT lIsBreakByUsed
        ).
          
    cLimitingQuery = "FOR EACH ar-invl NO-LOCK"
                     + " WHERE ar-invl.company EQ " + QUOTER(cocode)
                     + " AND ar-invl.posted EQ YES " 
                     + pGetWhereCriteria("ar-invl",YES)
                     + " USE-INDEX inv-no-desc "                    
                     + " ,FIRST ar-inv  no-lock"
                     + " WHERE ar-inv.x-no EQ ar-invl.x-no "
                     + " AND ar-inv.inv-date GE " + quoter(fi_bdate)    
                     + " AND ar-inv.inv-date LE " + quoter(fi_edate)
                     + "  " + pGetWhereCriteria("ar-inv",YES) 
                     + " ,FIRST cust OF ar-inv NO-LOCK "
                     + " BREAK BY ar-invl.inv-no DESC"
                     .  
                                 
    /* Limit the query if order no is 0 or cadd No is Blank */                    
    IF fi_inv-no EQ 0 THEN             
        RUN Browse_PrepareAndExecuteLimitingQuery(
            INPUT  cLimitingQuery,   /* Query */
            INPUT  cQueryBuffers,    /* Buffers Name */
            INPUT  iRecordLimit,     /* Record Limit */
            INPUT  dQueryTimeLimit,  /* Time Limit*/
            INPUT  lEnableShowAll,   /* Enable ShowAll Button? */
            INPUT  cFieldBuffer,     /* Buffer name to fetch the field's value*/
            INPUT  cFieldName,       /* Field Name*/
            INPUT  iplInitialLoad,   /* Initial Query*/
            INPUT  lIsBreakByUsed,   /* Is breakby used */
            OUTPUT cResponse           
            ).       
  
    ELSE 
        cResponse = "InvoiceNo". /* For identification purpose */
        
    IF cResponse EQ "" AND lButtongoPressed THEN  
        MESSAGE "No Records Found..."
            VIEW-AS ALERT-BOX ERROR.
              
    ELSE IF cResponse EQ "ShowALL" THEN 
        RUN pPrepareAndExecuteQueryForShowAll.
            
    ELSE DO:
                
        cBrowseWhereClause = (IF fi_inv-no EQ 0 THEN " AND ar-invl.inv-no  GE " + STRING(INTEGER(cResponse)) ELSE "").        
               
        cBrowseQuery = "FOR EACH ar-invl NO-LOCK"
                       + " WHERE ar-invl.company EQ " + QUOTER(cocode)
                       + " AND ar-invl.posted EQ YES "
                       + cBrowseWhereClause   
                       + pGetWhereCriteria("ar-invl",YES)
                       + " USE-INDEX inv-no-desc"             
                       + " ,FIRST ar-inv  no-lock"
                       + " WHERE ar-inv.x-no EQ ar-invl.x-no "
                       + " AND ar-inv.inv-date GE " + quoter(fi_bdate)    
                       + " AND ar-inv.inv-date LE " + quoter(fi_edate)
                       + "  " + pGetWhereCriteria("ar-inv",YES) 
                       + " ,FIRST cust OF ar-inv NO-LOCK "                        
                       + " BY " + pGetSortCondition(lv-sort-by,lv-sort-by-lab) + ( IF ll-sort-asc THEN  "" ELSE " DESC") +  " BY ar-invl.inv-no BY ar-invl.bol-no BY ar-invl.i-no BY ar-invl.line"
                       .                                                               
        RUN Browse_PrepareAndExecuteBrowseQuery(
            INPUT  BROWSE {&BROWSE-NAME}:QUERY, /* Browse Query Handle */      
            INPUT  cBrowseQuery,                /* BRowse Query */             
            INPUT  NO,                          /* Show limit alert? */        
            INPUT  0,                           /* Record limit */             
            INPUT  0,                           /* Time Limit */               
            INPUT  lEnableShowAll,              /* Enable ShowAll Button */    
            OUTPUT cResponse                                                   
            ).
    END.                                     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckCustUserRecord B-table-Win 
PROCEDURE pCheckCustUserRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opcCustNo AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cGetQueryString AS CHARACTER NO-UNDO.
  DEFINE VARIABLE h_buffer        AS HANDLE    NO-UNDO.
  DEFINE VARIABLE h_buffer2        AS HANDLE    NO-UNDO.
  DEFINE VARIABLE h_lquery        AS HANDLE NO-UNDO.
  DEFINE VARIABLE fhCustomer as handle no-undo.
                               
        cGetQueryString = "FOR EACH ar-invl NO-LOCK"
                     + " WHERE ar-invl.company EQ " + QUOTER(cocode)                     
                     + pGetWhereCriteria("ar-invl", NO) 
                     + " ,FIRST ar-inv  no-lock"
                     + " WHERE ar-inv.x-no EQ ar-invl.x-no "
                     + " AND ar-inv.inv-date GE " + quoter(fi_bdate)    
                     + " AND ar-inv.inv-date LE " + quoter(fi_edate)       
                     .  
        CREATE BUFFER h_buffer FOR TABLE "ar-invl".
        CREATE BUFFER h_buffer2 FOR TABLE "ar-inv".
        CREATE QUERY h_lquery.                     
        
        h_lquery:ADD-BUFFER(h_buffer).
        h_lquery:ADD-BUFFER(h_buffer2).
        h_lquery:QUERY-PREPARE(cGetQueryString).
        h_lquery:QUERY-OPEN().
     
        h_lquery:GET-FIRST().
        MAIN-LOOP:
        REPEAT:            
            IF h_lquery:QUERY-OFF-END THEN
                LEAVE MAIN-LOOP.  
            fhCustomer = h_buffer:BUFFER-FIELD("cust-no" ).
            opcCustNo = fhCustomer:BUFFER-VALUE .   
            LEAVE MAIN-LOOP.           
        END.
        DELETE OBJECT h_lquery.
        DELETE OBJECT h_buffer .    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-defaults B-table-Win 
PROCEDURE set-defaults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN fi_inv-no = 0
         fi_cust-no = ""
         fi_i-no = ""
         fi_po-no = ""
         fi_part-no = ""
         fi_bol-no = 0
         fi_est-no = ""
         fi_ord-no = 0
         fi_actnum = ""
         fi_bdate = (TODAY - 365) 
         fi_edate = (TODAY) .

  DISPLAY fi_inv-no fi_cust-no fi_i-no fi_po-no
          fi_part-no fi_bol-no fi_est-no fi_ord-no fi_actnum fi_bdate fi_edate
          WITH FRAME {&FRAME-NAME}.

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

  /*{methods/setfocus.i fi_inv-no}*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pGetWhereCriteria B-table-Win
FUNCTION pGetWhereCriteria RETURNS CHARACTER 
  ( ipcTable AS CHARACTER, ipcCustListCheck AS LOGICAL ):
/*------------------------------------------------------------------------------
 Purpose: Prepares and returns the where clause criteria based on table name
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cWhereCriteria AS CHARACTER NO-UNDO.    
       IF ipcTable EQ "ar-invl" THEN
       DO:
        IF fi_est-no NE "" THEN fi_est-no = FILL(" ",8 - LENGTH(TRIM(fi_est-no))) + TRIM(fi_est-no).
        cWhereCriteria = cWhereCriteria 
                         + (IF custCount  NE "" AND ipcCustListCheck THEN " AND ((LOOKUP(ar-invl.cust-no," + QUOTER(custcount) + ") NE 0" + " AND ar-invl.cust-no NE '') OR " + QUOTER(custcount) + " EQ '')" ELSE "") 
                         + (IF fi_i-no    NE ""  THEN " AND ar-invl.i-no  BEGINS "    + QUOTER(fi_i-no)   ELSE "")
                         + (IF fi_cust-no NE "" THEN " AND ar-invl.cust-no BEGINS "   + QUOTER(fi_cust-no)  ELSE "")
                         + (IF fi_est-no  NE "" THEN " AND ar-invl.est-no BEGINS "    + QUOTER(fi_est-no)   ELSE "")
                         + (IF fi_part-no NE "" THEN " AND ar-invl.part-no BEGINS "    + QUOTER(fi_part-no)   ELSE "")
                         + (IF fi_po-no   NE "" THEN " AND ar-invl.po-no EQ " + QUOTER(fi_po-no)  ELSE "")
                         + (IF fi_actnum  NE "" THEN " AND ar-invl.actnum   BEGINS "    + QUOTER (fi_actnum)    ELSE "")
                         + (IF fi_inv-no NE 0 THEN " AND ar-invl.inv-no EQ " + QUOTER(fi_inv-no)  ELSE "")
                         + (IF fi_bol-no NE 0 THEN " AND ar-invl.bol-no EQ " + QUOTER(fi_bol-no)  ELSE "")
                         + (IF fi_ord-no NE 0 THEN " AND ar-invl.ord-no EQ " + QUOTER(fi_ord-no)  ELSE ""). 
       END.
       ELSE IF ipcTable EQ "ar-inv" THEN
       DO:
          cWhereCriteria = cWhereCriteria + " AND ("                           
                         + (IF tb_open     THEN " ar-inv.due NE 0 " ELSE "")
                         + (IF NOT tb_open THEN " ar-inv.due EQ 0 " ELSE "")
                         + (IF tb_paid     THEN " OR ar-inv.due EQ 0 " ELSE "")
                         + (IF NOT tb_paid THEN " OR ar-inv.due NE 0 " ELSE "")  
                         + " )"
                        .               
       END.
    
    RETURN cWhereCriteria.      
END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pGetSortCondition B-table-Win
FUNCTION pGetSortCondition RETURNS CHARACTER 
  (ipcSortBy AS CHARACTER,ipcSortLabel AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Retuns the sort condition based on the input 
 Notes:
------------------------------------------------------------------------------*/
    
    RETURN (IF ipcSortBy EQ 'ord-no'    THEN "STRING(ar-invl.ord-no,'9999999999')"        ELSE ~
            IF ipcSortBy EQ 'actnum'    THEN "ar-invl.actnum"                             ELSE ~
            IF ipcSortBy EQ 'cust-no'   THEN "ar-invl.cust-no"                            ELSE ~
            IF ipcSortBy EQ 'i-no'      THEN "ar-invl.i-no"                               ELSE ~              
            IF ipcSortBy EQ 'est-no'    THEN "ar-invl.est-no"                             ELSE ~
            IF ipcSortBy EQ 'inv-no'    THEN "STRING(ar-invl.inv-no,'9999999999')"        ELSE ~
            IF ipcSortBy EQ 'bol-no'    THEN "STRING(ar-invl.bol-no,'9999999999')"        ELSE ~
            IF ipcSortBy EQ 'po-no'     THEN "ar-invl.po-no"                              ELSE ~
            IF ipcSortBy EQ 'i-name'    THEN "ar-invl.i-name"                             ELSE ~
            IF ipcSortBy EQ 'gross'     THEN "STRING(ar-inv.gross,'99999999999.99')"                      ELSE ~
            IF ipcSortBy EQ 'due'       THEN "STRING(ar-inv.due,'99999999999.99')"                        ELSE ~
            IF ipcSortBy EQ 'part-no'   THEN "ar-invl.part-no"                            ELSE ~                      
                                             "STRING(YEAR(ar-inv.inv-date),'9999')
                                             + STRING(MONTH(ar-inv.inv-date),'99')
                                             + STRING(DAY(ar-inv.inv-date),'99')"
            ).
END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

