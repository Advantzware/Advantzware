&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"ASI SmartNavBrowser Object Template with Wizard.

Use this template to create a new SmartNavBrowser object with the assistance of the SmartBrowser Wizard. When completed, this object can then be drawn onto any 'smart' container such as a SmartWindow, SmartDialog or SmartFrame."
*/
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/<table>.w

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
{methods/defines/hndldefs.i}
{custom/globdefs.i}
/*&Scoped-define Item-KEY-PHRASE TRUE */

def var CurRowIdent as rowid no-undo.
def var ll-is-corr-style as log no-undo.
def var K_frac as dec init 6.25 no-undo.

{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}

DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR lv-frst-rowid2 AS ROWID NO-UNDO.
DEF VAR lv-last-rowid2 AS ROWID NO-UNDO.
DEF VAR lv-first-run AS LOG INIT YES NO-UNDO.
DEF VAR ll-initial AS LOG INIT YES NO-UNDO.

DEF VAR lv-sort-by AS CHAR INIT "rfq-no" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "rfq-no" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR lv-rfq-date-entered AS LOG NO-UNDO.
DEF VAR lv-show-prev AS LOG NO-UNDO.
DEF VAR lv-show-next AS LOG NO-UNDO.
DEF VAR lv-last-show-rfq-no AS INT NO-UNDO.
DEF VAR lv-first-show-rfq-no AS INT NO-UNDO.

&SCOPED-DEFINE key-phrase  YES

            /*
&SCOPED-DEFINE for-rfq                                         ~
         EACH rfq WHERE ~{&KEY-PHRASE}                     ~
                     AND est.company  EQ g_company             ~
                     AND est.est-no   EQ eb.est-no             ~
                     AND est.est-type GE 1                     ~
                     AND est.est-type LE 4                     ~
                     AND ((est.est-type EQ 1 AND tb_single) OR ~
                          (est.est-type EQ 2 AND tb_set)    OR ~
                          (est.est-type GT 2 AND tb_tancom))

&SCOPED-DEFINE for-eqty                            ~
          EACH ASI.est-qty WHERE  est-qty.company = ASI.est.company ~
                             AND ASI.est-qty.est-no = ASI.est.est-no ~
                             AND ASI.est-qty.eqty = eb.eqty

&SCOPED-DEFINE for-ef                     ~
           each ASI.ef  WHERE ASI.ef.company = ASI.est.company ~
                          AND ASI.ef.est-no = ASI.est.est-no AND ef.form-no = eb.form-no  

&SCOPED-DEFINE for-eb                     ~
       FOR each ASI.eb WHERE ASI.eb.company = g_company AND eb.loc = g_loc  ~
                 and eb.form-no > 0 and eb.blank-no > 0      ~
                 AND eb.cust-no BEGINS begin_cust-no            ~
                 AND eb.part-no BEGINS vi_part-no            ~
                 AND eb.stock-no BEGINS vi_stock-no          ~
                 AND eb.style BEGINS vi_style                 ~
                 AND eb.part-dscr1 BEGINS vi_part-dscr1    ~
                 AND (eb.die-no BEGINS vi_die-no OR vi_die-no = "") ~
                 AND (eb.cad-no BEGINS vi_cad-no  OR vi_cad-no = "")~
                 AND (eb.plate-no BEGINS vi_plate-no OR vi_plate-no = "") ~
                 AND (eb.len = vi_len OR vi_len = 0)        ~
                 AND (eb.wid = vi_wid OR vi_wid = 0)        ~
                 AND (eb.dep = vi_dep OR vi_dep = 0)        ~


*/    
&scoped-define for-rfq ~
        FOR EACH rfq WHERE rfq.company = g_company ~
                       AND rfq.loc = g_loc

&SCOPED-DEFINE for-rfqitem ~
          EACH rfqitem OF rfq WHERE rfqitem.seq < 999   ~
               AND rfqitem.part-no BEGINS vi_part-no            ~
               AND rfqitem.part-dscr1 BEGINS vi_part-dscr1        ~
               AND rfqitem.style BEGINS vi_style                 ~
               AND rfqitem.len >= vi_len                    

&SCOPED-DEFINE sortby-log                             ~
    IF lv-sort-by EQ "rfq-no"  THEN string(rfq.rfq-no,"99999999")   ELSE     ~
    IF lv-sort-by EQ "req-date"  THEN string((YEAR(rfq.req-date) * 10000) + (MONTH(rfq.req-date) * 100) + DAY(rfq.req-date))  ELSE ~
    IF lv-sort-by EQ "part-no"  THEN rfqitem.part-no ELSE ~
    IF lv-sort-by EQ "cust-no" THEN rfq.cust-no  ELSE ~
    IF lv-sort-by EQ "style"  THEN rfqitem.style  ELSE ~
    IF lv-sort-by EQ "procat"  THEN rfqitem.procat  ELSE ~
    IF lv-sort-by EQ "qty"  THEN string(rfqitem.qty[1],"9999999999")  ELSE ~
    IF lv-sort-by EQ "len"  THEN string(rfqitem.len,">>>>>>9.9999")  ELSE ~
    IF lv-sort-by EQ "wid"  THEN string(rfqitem.wid,">>>>>>9.9999")  ELSE ~
    IF lv-sort-by EQ "dep"  THEN string(rfqitem.dep,">>>>>>9.9999")  ELSE ~
    IF lv-sort-by EQ "i-col"  THEN string(rfqitem.i-col)  ELSE ~
    IF lv-sort-by EQ "i-coat"  THEN string(rfqitem.i-coat)  ELSE ~
    IF lv-sort-by EQ "cal"  THEN string(rfqitem.cal)  ELSE ~
    IF lv-sort-by EQ "part-dscr1"  THEN rfqitem.part-dscr1  ELSE ~
    IF lv-sort-by EQ "est-no"  THEN rfqitem.est-no ELSE ""
    

&SCOPED-DEFINE sortby BY rfq.rfq-no BY rfqitem.part-no 

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

DEF VAR lv-disp-qty AS INT FORM ">>>>>>>>9" NO-UNDO.
DEF VAR browser-log AS LOG NO-UNDO.
DEF VAR lv-rfqbrows-int AS INT NO-UNDO.

DO TRANSACTION:
  find first sys-ctrl where sys-ctrl.company eq g_company
                        and sys-ctrl.name    eq "RFQBROWS" no-lock no-error.
  if not avail sys-ctrl then do:
     create sys-ctrl.
     ASSIGN sys-ctrl.company = cocode
            sys-ctrl.name    = "RFQBROWS"
            sys-ctrl.descrip = "Show records when first entering RFQ browsers."
            sys-ctrl.log-fld = YES
            sys-ctrl.int-fld = 10.
     MESSAGE sys-ctrl.descrip VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
     UPDATE sys-ctrl.log-fld.
  end.
  ASSIGN browser-log = sys-ctrl.log-fld.
END.
lv-rfqbrows-int = sys-ctrl.int-fld.

ll-initial = browser-log.

DEF BUFFER blast-rfq FOR rfq.
DEF VAR lv-last-rfq-no AS int NO-UNDO.

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
&Scoped-define INTERNAL-TABLES rfq rfqitem

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table rfq.rfq-no rfqitem.part-no ~
rfq.cust-no rfq.req-date rfqitem.style rfqitem.procat rfqitem.qty[1] ~
rfqitem.i-col rfqitem.i-coat ~
to-corrware-dim(ll-is-corr-style, rfqitem.len) @ rfqitem.len rfqitem.board ~
to-corrware-dim(ll-is-corr-style, rfqitem.wid) @ rfqitem.wid rfqitem.cal ~
to-corrware-dim(ll-is-corr-style, rfqitem.dep) @ rfqitem.dep rfqitem.est-no ~
rfqitem.part-dscr1 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rfq.rfq-no ~
rfqitem.part-no rfq.cust-no rfq.req-date rfqitem.style rfqitem.procat ~
rfqitem.qty[1] rfqitem.i-col rfqitem.i-coat rfqitem.board rfqitem.cal ~
rfqitem.est-no rfqitem.part-dscr1 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rfq rfqitem
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rfq
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table rfqitem
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rfq WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST rfqitem OF rfq ~
      WHERE rfqitem.seq < 999 OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rfq WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST rfqitem OF rfq ~
      WHERE rfqitem.seq < 999 OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rfq rfqitem
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rfq
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table rfqitem


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 vi_rfq-no begin_cust-no vi_part-no ~
vi_part-dscr1 vi_style vi_est-no vi_len vi_wid vi_dep btn_go btn_prev 
&Scoped-Define DISPLAYED-OBJECTS vi_rfq-no begin_cust-no vi_part-no ~
vi_part-dscr1 vi_style vi_est-no vi_len vi_wid vi_dep fi_sort-by 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD to-corrware-dim B-table-Win 
FUNCTION to-corrware-dim RETURNS DECIMAL
  ( input ip-is-corr-style as log, input  ip-dim as decimal )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 14 BY 1
     FONT 6.

DEFINE BUTTON btn_next 
     LABEL "Show &Next" 
     SIZE 16 BY 1
     FONT 6.

DEFINE BUTTON btn_prev 
     LABEL "Show &Previous" 
     SIZE 20 BY 1
     FONT 6.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE vi_dep AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_est-no AS CHARACTER FORMAT "x(8)":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE vi_len AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_part-dscr1 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_part-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_req-date AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE vi_rfq-no AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE vi_style AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE vi_wid AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      rfq, 
      rfqitem SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      rfq.rfq-no FORMAT "->,>>>,>>9":U LABEL-BGCOLOR 14
      rfqitem.part-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      rfq.cust-no FORMAT "X(8)":U LABEL-BGCOLOR 14
      rfq.req-date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      rfqitem.style COLUMN-LABEL "Style" FORMAT "x(4)":U LABEL-BGCOLOR 14
      rfqitem.procat FORMAT "x(5)":U LABEL-BGCOLOR 14
      rfqitem.qty[1] COLUMN-LABEL "Qty" FORMAT "->,>>>,>>9":U LABEL-BGCOLOR 14
      rfqitem.i-col FORMAT ">9":U LABEL-BGCOLOR 14
      rfqitem.i-coat FORMAT ">9":U LABEL-BGCOLOR 14
      to-corrware-dim(ll-is-corr-style, rfqitem.len) @ rfqitem.len
            LABEL-BGCOLOR 14
      rfqitem.board FORMAT "x(10)":U LABEL-BGCOLOR 14
      to-corrware-dim(ll-is-corr-style, rfqitem.wid) @ rfqitem.wid
            LABEL-BGCOLOR 14
      rfqitem.cal FORMAT "9.99999":U LABEL-BGCOLOR 14
      to-corrware-dim(ll-is-corr-style, rfqitem.dep) @ rfqitem.dep
            LABEL-BGCOLOR 14
      rfqitem.est-no FORMAT "x(8)":U LABEL-BGCOLOR 14
      rfqitem.part-dscr1 FORMAT "x(60)":U LABEL-BGCOLOR 14
  ENABLE
      rfq.rfq-no
      rfqitem.part-no
      rfq.cust-no
      rfq.req-date
      rfqitem.style
      rfqitem.procat
      rfqitem.qty[1]
      rfqitem.i-col
      rfqitem.i-coat
      rfqitem.board
      rfqitem.cal
      rfqitem.est-no
      rfqitem.part-dscr1
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 146 BY 15
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     vi_rfq-no AT ROW 1.95 COL 3 NO-LABEL
     begin_cust-no AT ROW 1.95 COL 19 COLON-ALIGNED NO-LABEL
     vi_part-no AT ROW 1.95 COL 34 COLON-ALIGNED NO-LABEL
     vi_part-dscr1 AT ROW 1.95 COL 56 COLON-ALIGNED NO-LABEL
     vi_style AT ROW 1.95 COL 98 COLON-ALIGNED NO-LABEL
     vi_est-no AT ROW 1.95 COL 117 NO-LABEL
     vi_len AT ROW 1.95 COL 132 COLON-ALIGNED NO-LABEL
     vi_wid AT ROW 2.91 COL 132 COLON-ALIGNED NO-LABEL
     vi_dep AT ROW 3.86 COL 132 COLON-ALIGNED NO-LABEL
     btn_go AT ROW 4.81 COL 3
     btn_prev AT ROW 4.81 COL 18
     btn_next AT ROW 4.81 COL 39
     vi_req-date AT ROW 4.81 COL 56 COLON-ALIGNED NO-LABEL
     fi_sort-by AT ROW 4.81 COL 113 COLON-ALIGNED NO-LABEL
     Browser-Table AT ROW 6 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     "L x W x D" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.24 COL 134
          FGCOLOR 9 FONT 6
     "Estimate" VIEW-AS TEXT
          SIZE 11 BY .67 AT ROW 1.24 COL 118
          FGCOLOR 9 FONT 6
     "Customer Part#" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 1.24 COL 37
          FGCOLOR 9 FONT 6
     "RFQ#" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.24 COL 5
          FGCOLOR 9 FONT 6
     "Style" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.24 COL 102
          FGCOLOR 9 FONT 6
     "Part Description" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 1.24 COL 65
          FGCOLOR 9 FONT 6
     "Customer" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.24 COL 22
          FGCOLOR 9 FONT 6
     "Click Field Heading to Sort By:" VIEW-AS TEXT
          SIZE 36 BY .62 AT ROW 5.05 COL 79
          FONT 6
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 
         DEFAULT-BUTTON btn_go.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser Template
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
         HEIGHT             = 20.05
         WIDTH              = 146.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
/* BROWSE-TAB Browser-Table fi_sort-by F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE Browser-Table IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3
       Browser-Table:MAX-DATA-GUESS IN FRAME F-Main         = 30
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2".

/* SETTINGS FOR BUTTON btn_next IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vi_est-no IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN vi_req-date IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vi_req-date:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vi_rfq-no IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "rfq.rfq,rfq.rfqitem OF rfq.rfq"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST OUTER"
     _Where[2]         = "rfq.rfqitem.seq < 999"
     _FldNameList[1]   > rfq.rfq.rfq-no
"rfq.rfq-no" ? ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > rfq.rfqitem.part-no
"rfqitem.part-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > rfq.rfq.cust-no
"rfq.cust-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > rfq.rfq.req-date
"rfq.req-date" ? ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > rfq.rfqitem.style
"rfqitem.style" "Style" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > rfq.rfqitem.procat
"rfqitem.procat" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > rfq.rfqitem.qty[1]
"rfqitem.qty[1]" "Qty" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > rfq.rfqitem.i-col
"rfqitem.i-col" ? ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > rfq.rfqitem.i-coat
"rfqitem.i-coat" ? ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"to-corrware-dim(ll-is-corr-style, rfqitem.len) @ rfqitem.len" ? ? ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > rfq.rfqitem.board
"rfqitem.board" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"to-corrware-dim(ll-is-corr-style, rfqitem.wid) @ rfqitem.wid" ? ? ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > rfq.rfqitem.cal
"rfqitem.cal" ? ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"to-corrware-dim(ll-is-corr-style, rfqitem.dep) @ rfqitem.dep" ? ? ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > rfq.rfqitem.est-no
"rfqitem.est-no" ? "x(8)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > rfq.rfqitem.part-dscr1
"rfqitem.part-dscr1" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no B-table-Win
ON LEAVE OF begin_cust-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container", OUTPUT char-hdl).
    RUN select-page IN WIDGET-HANDLE(char-hdl) (2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* APPLY "ENTRY" TO VI_EST-NO IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:
    find style where style.company = rfq.company and
                      style.style = rfqitem.style
                      no-lock no-error.

   if avail style and style.industry = "2" then   ll-is-corr-style = yes.
   else ll-is-corr-style = no.

   IF ll-is-corr-style then
      assign rfqitem.len:format in browse {&browse-name} = ">>>>9.99"
             rfqitem.wid:format in browse {&browse-name} = ">>>>9.99"
             rfqitem.dep:format in browse {&browse-name} = ">>>>9.99"
             .
   else assign rfqitem.len:format in browse {&browse-name} = ">>9.99999"
               rfqitem.wid:format in browse {&browse-name} = ">>9.99999"
               rfqitem.dep:format in browse {&browse-name} = ">>9.99999"
             .
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
  {methods/template/local/setvalue.i}
    
  CurRowIdent = rowid(rfq).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  SESSION:SET-WAIT-STATE("general").

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "leave" TO FOCUS.

    ASSIGN
     begin_cust-no
     vi_part-no
     vi_est-no
     vi_rfq-no  
     vi_style
     vi_req-date
     vi_len   
     vi_wid
     vi_dep
     vi_part-dscr1
     .

    RUN dispatch ("open-query").
    APPLY "VALUE-CHANGED":U TO {&BROWSE-NAME}.
  END.
  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_next B-table-Win
ON CHOOSE OF btn_next IN FRAME F-Main /* Show Next */
DO:
   SESSION:SET-WAIT-STATE("general").
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     vi_est-no:SCREEN-VALUE = ""
     begin_cust-no:SCREEN-VALUE = ""
     vi_part-no:SCREEN-VALUE = ""
     vi_rfq-no:SCREEN-VALUE    = ""
     vi_style:SCREEN-VALUE   = ""
     vi_len:SCREEN-VALUE  = ""
     vi_part-dscr1:SCREEN-VALUE  = ""
    /* vi_est-date:SCREEN-VALUE = ?  /*string(date(1,1,year(today)))*/ */
        .
    lv-show-next = YES.

    APPLY "choose" TO btn_go.
  END.
   SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_prev B-table-Win
ON CHOOSE OF btn_prev IN FRAME F-Main /* Show Previous */
DO:
   SESSION:SET-WAIT-STATE("general").
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     vi_rfq-no:SCREEN-VALUE = ""
     begin_cust-no:SCREEN-VALUE = ""
     vi_part-no:SCREEN-VALUE = ""
     vi_est-no:SCREEN-VALUE    = ""
     vi_style:SCREEN-VALUE   = ""
     vi_len:SCREEN-VALUE  = ""
     vi_part-dscr1:SCREEN-VALUE = ""
    /* vi_est-date:SCREEN-VALUE = ?  /*string(date(1,1,year(today)))*/ */
        .
    lv-show-prev = YES.
  
    ENABLE btn_next .
    APPLY "choose" TO btn_go.
  END.
   SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_dep B-table-Win
ON LEAVE OF vi_dep IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_est-no B-table-Win
ON HELP OF vi_est-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.

    run windows/l-esttyp.w (g_company,g_loc,"1234","EST",focus:screen-value, output char-val).
    if char-val <> "" then FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_est-no B-table-Win
ON LEAVE OF vi_est-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_len B-table-Win
ON LEAVE OF vi_len IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_part-dscr1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_part-dscr1 B-table-Win
ON LEAVE OF vi_part-dscr1 IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_part-no B-table-Win
ON HELP OF vi_part-no IN FRAME F-Main
DO:
  DEF VAR char-val AS cha NO-UNDO.

    run windows/l-esttyp.w (g_company,g_loc,"1234","part",focus:screen-value, output char-val).
    if char-val <> "" then FOCUS:SCREEN-VALUE = ENTRY(2,char-val).
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_part-no B-table-Win
ON LEAVE OF vi_part-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_req-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_req-date B-table-Win
ON LEAVE OF vi_req-date IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  lv-rfq-date-entered = SELF:MODIFIED.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_rfq-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_rfq-no B-table-Win
ON HELP OF vi_rfq-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.

    run windows/l-esttyp.w (g_company,g_loc,"1234","EST",focus:screen-value, output char-val).
    if char-val <> "" then FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_rfq-no B-table-Win
ON LEAVE OF vi_rfq-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_style B-table-Win
ON LEAVE OF vi_style IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vi_wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vi_wid B-table-Win
ON LEAVE OF vi_wid IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
/*
FIND FIRST ce-ctrl WHERE ce-ctrl.company = gcompany and
                         ce-ctrl.loc = gloc NO-LOCK NO-ERROR.
vi_est-no = IF AVAIL ce-ctrl THEN string(ce-ctrl.e-num - 10) ELSE "".
vi_est-date = DATE(1,1,YEAR(TODAY)).
*/

FIND LAST blast-rfq WHERE blast-rfq.company = g_company NO-LOCK NO-ERROR.
lv-last-rfq-no = IF AVAIL blast-rfq THEN blast-rfq.rfq-no ELSE 0.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE entry-to-frame B-table-Win 
PROCEDURE entry-to-frame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      APPLY "entry" TO  vi_rfq-no IN FRAME {&FRAME-NAME}.  
      RETURN NO-APPLY.
  END.

  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE first-run B-table-Win 
PROCEDURE first-run :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-rfq-no AS INT NO-UNDO.

RUN set-defaults.

IF ll-initial THEN DO:
  li = 0.

  FIND LAST rfq {&where-first1} USE-INDEX rfq NO-LOCK NO-ERROR.
  DO WHILE AVAIL rfq:
    IF rfq.rfq-no NE lv-rfq-no THEN li = li + 1.
    lv-rfq-no = rfq.rfq-no.
    IF li GE lv-rfqbrows-int THEN LEAVE.

    FIND PREV rfq {&where-first1} NO-LOCK NO-ERROR.
  END.

  &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
             FOR EACH rfq WHERE rfq.rfq-no >= lv-rfq-no  ~
                            NO-LOCK ,  ~
                   {&for-rfqitem}  NO-LOCK
            
  

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 

END.

ELSE lv-first-run = NO.

ll-initial = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-num-records B-table-Win 
PROCEDURE get-num-records :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-num-record AS INT.


  op-num-record = NUM-RESULTS("{&browse-name}") - 1.
  IF op-num-record LT 0 THEN op-num-record = 0.
  IF op-num-record NE 0 AND lv-last-rowid EQ ip-rowid THEN op-num-record = ?.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-hide B-table-Win 
PROCEDURE local-hide :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

   def buffer bf-first for est.
  
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */

  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'hide':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
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

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN 
   rfq.rfq-no:READ-ONLY IN BROWSE {&browse-name} = YES
   rfq.req-date:READ-ONLY IN BROWSE {&browse-name} = YES
   rfq.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES
   rfqitem.part-no:READ-ONLY IN BROWSE {&browse-name} = YES 
   rfqitem.style:READ-ONLY IN BROWSE {&browse-name} = YES
   rfqitem.part-dscr1:READ-ONLY IN BROWSE {&browse-name} = YES
   rfqitem.cal:READ-ONLY IN BROWSE {&browse-name} = YES
   rfqitem.procat:READ-ONLY IN BROWSE {&browse-name} = YES
   rfqitem.i-col:READ-ONLY IN BROWSE {&browse-name} = YES
   rfqitem.i-coat:READ-ONLY IN BROWSE {&browse-name} = YES
   /*rfqitem.len:READ-ONLY IN BROWSE {&browse-name} = YES
   rfqitem.wid:READ-ONLY IN BROWSE {&browse-name} = YES
   rfqitem.dep:READ-ONLY IN BROWSE {&browse-name} = YES */
   rfqitem.board:READ-ONLY IN BROWSE {&browse-name} = YES
   rfqitem.est-no:READ-ONLY IN BROWSE {&browse-name} = YES
   rfqitem.qty[1]:READ-ONLY IN BROWSE {&browse-name} = YES
   .
  
  /*RUN set-focus. */
  ENABLE {&browse-name} WITH FRAME {&FRAME-NAME}. 

  APPLY "entry" TO  vi_rfq-no IN FRAME {&FRAME-NAME}. 



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

  IF lv-first-run THEN DO:
     RUN first-run.
  END.
  ELSE IF lv-show-prev OR lv-show-next THEN DO:
      RUN show-all.
  END.
  ELSE DO:
    {rfq/j-rfqlst.i} 
  END.

  IF AVAIL {&first-table-in-query-{&browse-name}} THEN DO:
    
    /*RUN dispatch ("display-fields").
    RUN dispatch ("row-changed").
    
    APPLY "value-changed" TO BROWSE {&browse-name}.
    
    GET LAST {&browse-name}.
    IF AVAIL {&first-table-in-query-{&browse-name}} THEN
      ASSIGN lv-last-rowid = ROWID({&first-table-in-query-{&browse-name}})
             lv-last-show-rfq-no = rfq.rfq-no.

    
    GET FIRST {&browse-name}.
    IF AVAIL {&first-table-in-query-{&browse-name}} THEN
      ASSIGN lv-frst-rowid = ROWID({&first-table-in-query-{&browse-name}})
             lv-first-show-rfq-no = rfq.rfq-no.*/
    
    ASSIGN
       lv-first-run = NO
       lv-show-prev = NO
       lv-show-next = NO.
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE New_record B-table-Win 
PROCEDURE New_record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 def input parameter ip-rowid as rowid no-undo.
  
  lv-first-run = YES.
  
  run local-open-query.
  
  /*
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"itemrec-target",OUTPUT char-hdl).
  RUN dispatch IN WIDGET-HANDLE(char-hdl) ('open-query').
  */

  do with frame {&frame-name}:
    reposition {&browse-name} to rowid ip-rowid no-error.  
    run dispatch ('row-changed').
    apply "value-changed" to {&browse-name}.
    return no-apply.  
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRow B-table-Win 
PROCEDURE RefreshRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input parameter pcMode     as character no-undo.
  define input parameter prRowIdent as rowid     no-undo.
  
  if pcMode = 'newRecord':U then do:
     def var lv-rowid-rfqitem as rowid no-undo.
     run get-rowid (output lv-rowid-rfqitem).

    /*run dispatch ( 'open-query':U ). */
    {&open-query-{&browse-name}}

/*    
message  string(prrowident) string(lv-rowid-rfqitem) string(currowident)
        skip
        "{&open-query-{&browse-name}}"
        view-as alert-box.
*/

    reposition browser-table to rowid if prrowident <> ? then prrowident else CurRowIdent. /* prRowIdent.  lv-rowid-rfqitem. */

    do while true:
      if available rfq then
         if (prrowident <> ? and rowid(rfq) = prRowIdent) or
            (currowident = rowid(rfq) ) 
          then leave.
      browse browser-Table:select-next-row().
      browse browser-Table:fetch-selected-row(1).
    end.  
   
  end.
  else do :
/*    find current rfqitem no-lock no-error.*/
  end.
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
  {src/adm/template/snd-list.i "rfq"}
  {src/adm/template/snd-list.i "rfqitem"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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
   ASSIGN vi_est-no = ""
          begin_cust-no = ""
          vi_part-no = ""
          vi_rfq-no = 0
          vi_style = ""
          vi_len = 0
          vi_req-date = ? /* DATE(1,1,YEAR(TODAY)) */
          /*tb_single = YES
          tb_set = YES
          tb_tancom = YES*/.

   DISP  vi_est-no 
         begin_cust-no
         vi_part-no 
         vi_rfq-no
         vi_style
         vi_len 
         vi_req-date
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
/*{methods/setfocus.i {&browse-name} } */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-all B-table-Win 
PROCEDURE show-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-rfq-no AS INT NO-UNDO.

RUN set-defaults.

IF lv-show-prev THEN DO:
   li = 0.
  FOR EACH rfq NO-LOCK WHERE rfq.rfq-no <= lv-last-show-rfq-no
                         BY rfq.rfq-no DESC:

     li = li + 1.
     lv-rfq-no = rfq.rfq-no.
     IF li >= lv-rfqbrows-int THEN LEAVE.
  END.
/*MESSAGE "PREV last: " lv-last-show-rfq-no lv-rfq-no VIEW-AS ALERT-BOX.*/
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
           FOR EACH rfq WHERE rfq.rfq-no >= lv-rfq-no  ~
                          AND rfq.rfq-no <= lv-last-show-rfq-no   ~
                          NO-LOCK USE-INDEX rfq,  ~
                   {&for-rfqitem}  NO-LOCK
            
  

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 


END.  /* lv-show-prev */
ELSE IF lv-show-next THEN DO:
    li = 0.   

    FOR EACH rfq WHERE rfq.rfq-no >= lv-first-show-rfq-no NO-LOCK BY rfq.rfq-no  :

       li = li + 1.
       lv-rfq-no = rfq.rfq-no.
       IF li >= lv-rfqbrows-int THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
             FOR EACH rfq WHERE rfq.rfq-no <= lv-rfq-no  ~
                            AND rfq.rfq-no >= lv-first-show-rfq-no   ~
                            NO-LOCK ,  ~
                 {&for-rfqitem}  NO-LOCK

        IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                       ELSE {&open-query} {&sortby-phrase-desc}. 

END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION to-corrware-dim B-table-Win 
FUNCTION to-corrware-dim RETURNS DECIMAL
  ( input ip-is-corr-style as log, input  ip-dim as decimal ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var out-dim as dec no-undo.
  
  if ip-is-corr-style then 
     /*round(trunc({1},0) + (({1} - trunc({1},0)) / K_FRAC),2)   sys/inc/k16.i */
     out-dim = round(trunc(ip-dim,0) + ((ip-dim - trunc(ip-dim,0)) / K_FRAC),2).
  else out-dim = ip-dim.
/*
  if rfqitem.rfq-no = 343 then
    message "rfqpart, function is corr-style? " ip-is-corr-style 
            " Input: " ip-dim "Return value:" out-dim view-as alert-box.
*/          
  RETURN out-dim.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

