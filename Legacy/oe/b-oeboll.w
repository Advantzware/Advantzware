&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: oe\b-oeboll.w

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

&SCOPED-DEFINE yellowColumnsName b-oeboll
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR lv-copy-rowid AS ROWID NO-UNDO.
DEF VAR lv-rell-rowid AS ROWID NO-UNDO.
DEF VAR lv-copy-lot-no AS CHAR NO-UNDO.
DEF VAR v-old-cases AS INT NO-UNDO.
DEF VAR v-old-qty-case AS INT NO-UNDO.
DEF VAR v-old-partial AS INT NO-UNDO.
DEF VAR lWasAdded AS LOG NO-UNDO.
DEF VAR rOeRell AS ROWID NO-UNDO.
{custom/globdefs.i}

{sys/inc/var.i "new shared"}

ASSIGN
 cocode = g_company
 locode = g_loc.

{oe/d-selbin.i NEW}

DEF BUFFER alt-rell FOR oe-rell.

DEF VAR lv-ord-ok AS cha INIT "R,I,S,P,A,N,U" NO-UNDO.
DEF VAR lv-i-no LIKE oe-boll.i-no NO-UNDO.
DEF VAR lv-tag LIKE oe-boll.tag NO-UNDO.
DEF VAR lv-loc LIKE oe-boll.loc NO-UNDO.
DEF VAR lv-loc-bin LIKE oe-boll.loc-bin NO-UNDO.
DEF VAR lv-job-no LIKE oe-boll.job-no NO-UNDO.
DEF VAR lv-job-no2 LIKE oe-boll.job-no2 NO-UNDO.
DEF VAR lv-cust-no LIKE oe-boll.cust-no NO-UNDO.
DEF VAR lv-i-name LIKE itemfg.i-name NO-UNDO.
DEF VAR lv-part-no LIKE itemfg.part-no NO-UNDO.
DEF VAR ll-qty-warned AS LOG NO-UNDO.
DEFINE VARIABLE fgBin LIKE fg-bin.units-pallet NO-UNDO.
DEF VAR li-lot-no AS CHAR NO-UNDO.
DEF VAR li-cost AS DEC NO-UNDO.
DEF VAR lv-relase AS CHAR NO-UNDO.
DEF VAR lr-rel-lib AS HANDLE NO-UNDO.

DEFINE VARIABLE glPOModified AS LOG NO-UNDO.

DEFINE TEMP-TABLE w-rowid FIELD w-rowid AS CHAR
          INDEX w-rowid IS PRIMARY w-rowid.

DO TRANSACTION:
  {sys/inc/relmerge.i}
END.

{sys/inc/fgrecpt.i}

&SCOPED-DEFINE SETVALUE NO


DO TRANSACTION:
    {sys\inc\BOLWeight.i} 
END.

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
&Scoped-define EXTERNAL-TABLES oe-bolh
&Scoped-define FIRST-EXTERNAL-TABLE oe-bolh


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-bolh.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-boll

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table oe-boll.ord-no oe-boll.i-no ~
display-i-name (0) @ lv-i-name oe-boll.po-no oe-boll.tag oe-boll.loc ~
oe-boll.loc-bin oe-boll.job-no oe-boll.job-no2 oe-boll.cust-no ~
oe-boll.cases oe-boll.qty-case oe-boll.partial oe-boll.qty fgBin() @ fgBin ~
oe-boll.tot-pallets oe-boll.weight oe-boll.freight oe-boll.p-c ~
oe-boll.lot-no get-cost() @ li-cost display-cust-item(0) @ lv-part-no ~
get-release() @ lv-relase 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table oe-boll.ord-no ~
oe-boll.i-no oe-boll.po-no oe-boll.tag oe-boll.loc oe-boll.loc-bin ~
oe-boll.job-no oe-boll.job-no2 oe-boll.cases oe-boll.qty-case ~
oe-boll.partial oe-boll.qty oe-boll.tot-pallets oe-boll.weight ~
oe-boll.freight oe-boll.p-c 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table oe-boll
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table oe-boll
&Scoped-define QUERY-STRING-Browser-Table FOR EACH oe-boll WHERE ~{&KEY-PHRASE} ~
      AND oe-boll.company = oe-bolh.company ~
 AND oe-boll.b-no = oe-bolh.b-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH oe-boll WHERE ~{&KEY-PHRASE} ~
      AND oe-boll.company = oe-bolh.company ~
 AND oe-boll.b-no = oe-bolh.b-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table oe-boll
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table oe-boll


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find fi_By fi_SortByLabel fi_AutoFindLabel 
&Scoped-Define DISPLAYED-OBJECTS browse-order fi_sortby auto_find fi_By ~
fi_SortByLabel fi_AutoFindLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-cust-item B-table-Win 
FUNCTION display-cust-item RETURNS CHARACTER
  ( INPUT ip-int AS INT  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-i-name B-table-Win 
FUNCTION display-i-name RETURNS CHARACTER
  ( INPUT ip-int AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fgBin B-table-Win 
FUNCTION fgBin RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fgBinScreen B-table-Win 
FUNCTION fgBinScreen RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-cost B-table-Win 
FUNCTION get-cost RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-lot-no B-table-Win 
FUNCTION get-lot-no RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-release B-table-Win 
FUNCTION get-release RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fi_AutoFindLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Auto Find:" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE fi_By AS CHARACTER FORMAT "X(256)":U INITIAL "By:" 
      VIEW-AS TEXT 
     SIZE 3.6 BY .62 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_SortByLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By:" 
      VIEW-AS TEXT 
     SIZE 7.4 BY .62 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Order", 1
     SIZE 43 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      oe-boll SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      oe-boll.ord-no COLUMN-LABEL "Order" FORMAT ">>>>>>":U WIDTH 10
            COLUMN-FGCOLOR 9 LABEL-BGCOLOR 14
      oe-boll.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U WIDTH 21
            LABEL-BGCOLOR 14
      display-i-name (0) @ lv-i-name COLUMN-LABEL "FG Item Name" FORMAT "x(15)":U
            WIDTH 21
      oe-boll.po-no FORMAT "x(15)":U WIDTH 20 LABEL-BGCOLOR 14
      oe-boll.tag COLUMN-LABEL "Tag" FORMAT "x(20)":U WIDTH 30
            LABEL-BGCOLOR 14
      oe-boll.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U WIDTH 8 LABEL-BGCOLOR 14
      oe-boll.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U WIDTH 12
            LABEL-BGCOLOR 14
      oe-boll.job-no COLUMN-LABEL "Job #" FORMAT "x(6)":U WIDTH 9
            LABEL-BGCOLOR 14
      oe-boll.job-no2 COLUMN-LABEL "" FORMAT "99":U WIDTH 3
      oe-boll.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
            WIDTH 14 LABEL-BGCOLOR 14
      oe-boll.cases COLUMN-LABEL "Units" FORMAT "->>>,>>>":U WIDTH 12
      oe-boll.qty-case COLUMN-LABEL "Qty/Unit" FORMAT ">>>,>>>":U
            WIDTH 12
      oe-boll.partial COLUMN-LABEL "Partial" FORMAT "->>>,>>>":U
            WIDTH 12
      oe-boll.qty COLUMN-LABEL "Qty Shipped" FORMAT "->>,>>>,>>9":U
      fgBin() @ fgBin COLUMN-LABEL "Unts/Pallet" FORMAT "->,>>>,>>9":U
      oe-boll.tot-pallets COLUMN-LABEL "Pallets" FORMAT "->,>>>,>>9":U
      oe-boll.weight COLUMN-LABEL "Weight" FORMAT "->>>>>>9":U
            WIDTH 11
      oe-boll.freight FORMAT ">>>,>>9.99":U WIDTH 14
      oe-boll.p-c COLUMN-LABEL "P/C" FORMAT "C/P":U WIDTH 4
      oe-boll.lot-no COLUMN-LABEL "Customer Lot #" FORMAT "x(15)":U
            WIDTH 20
      get-cost() @ li-cost COLUMN-LABEL "Cost/M" FORMAT "->>>,>>>,>>9.99":U
            WIDTH 12
      display-cust-item(0) @ lv-part-no FORMAT "x(15)":U
      get-release() @ lv-relase COLUMN-LABEL "Release" FORMAT "x(8)":U
  ENABLE
      oe-boll.ord-no
      oe-boll.i-no
      oe-boll.po-no
      oe-boll.tag
      oe-boll.loc
      oe-boll.loc-bin
      oe-boll.job-no
      oe-boll.job-no2
      oe-boll.cases
      oe-boll.qty-case
      oe-boll.partial
      oe-boll.qty
      oe-boll.tot-pallets
      oe-boll.weight
      oe-boll.freight
      oe-boll.p-c
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 6.43
         FONT 2 ROW-HEIGHT-CHARS .71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 7.67 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 7.67 COL 56 COLON-ALIGNED NO-LABEL
     auto_find AT ROW 7.67 COL 101.6 COLON-ALIGNED HELP
          "Enter Auto Find Value" NO-LABEL
     Btn_Clear_Find AT ROW 7.71 COL 131.2 HELP
          "CLEAR AUTO FIND Value"
     fi_By AT ROW 7.71 COL 2 NO-LABEL
     fi_SortByLabel AT ROW 7.86 COL 49.6 NO-LABEL
     fi_AutoFindLabel AT ROW 7.86 COL 93.8 NO-LABEL
     RECT-4 AT ROW 7.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.oe-bolh
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
         HEIGHT             = 7.86
         WIDTH              = 145.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}
{custom/yellowColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

ASSIGN 
       li-cost:VISIBLE IN BROWSE Browser-Table = FALSE.

/* SETTINGS FOR FILL-IN fi_AutoFindLabel IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_By IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fi_SortByLabel IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.oe-boll"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.oe-boll.company = ASI.oe-bolh.company
 AND ASI.oe-boll.b-no = ASI.oe-bolh.b-no"
     _FldNameList[1]   > ASI.oe-boll.ord-no
"oe-boll.ord-no" "Order" ">>>>>>" "integer" ? 9 ? 14 ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.oe-boll.i-no
"oe-boll.i-no" "FG Item#" ? "character" ? ? ? 14 ? ? yes ? no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"display-i-name (0) @ lv-i-name" "FG Item Name" "x(15)" ? ? ? ? ? ? ? no "Enter Finished Goods Name used for Alpha Numeric Searches." no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.oe-boll.po-no
"oe-boll.po-no" ? ? "character" ? ? ? 14 ? ? yes ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.oe-boll.tag
"oe-boll.tag" "Tag" "x(20)" "character" ? ? ? 14 ? ? yes ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.oe-boll.loc
"oe-boll.loc" "Whse" ? "character" ? ? ? 14 ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.oe-boll.loc-bin
"oe-boll.loc-bin" "Bin" ? "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.oe-boll.job-no
"oe-boll.job-no" "Job #" ? "character" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.oe-boll.job-no2
"oe-boll.job-no2" "" ? "integer" ? ? ? ? ? ? yes ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.oe-boll.cust-no
"oe-boll.cust-no" "Customer#" ? "character" ? ? ? 14 ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.oe-boll.cases
"oe-boll.cases" "Units" "->>>,>>>" "integer" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.oe-boll.qty-case
"oe-boll.qty-case" "Qty/Unit" ">>>,>>>" "integer" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.oe-boll.partial
"oe-boll.partial" "Partial" "->>>,>>>" "decimal" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.oe-boll.qty
"oe-boll.qty" "Qty Shipped" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"fgBin() @ fgBin" "Unts/Pallet" "->,>>>,>>9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.oe-boll.tot-pallets
"oe-boll.tot-pallets" "Pallets" "->,>>>,>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.oe-boll.weight
"oe-boll.weight" "Weight" "->>>>>>9" "integer" ? ? ? ? ? ? yes ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.oe-boll.freight
"oe-boll.freight" ? ">>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.oe-boll.p-c
"oe-boll.p-c" "P/C" ? "logical" ? ? ? ? ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.oe-boll.lot-no
"oe-boll.lot-no" "Customer Lot #" ? "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"get-cost() @ li-cost" "Cost/M" "->>>,>>>,>>9.99" ? ? ? ? ? ? ? no ? no no "12" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"display-cust-item(0) @ lv-part-no" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"get-release() @ lv-relase" "Release" "x(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'TABLEIO-SOURCE':U, OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN new-state IN WIDGET-HANDLE(char-hdl) ('update-begin':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR hlp-recid AS RECID NO-UNDO.

   CASE FOCUS:NAME :
       WHEN "ord-no" THEN DO:
         RUN windows/l-ordcs.w (cocode, oe-bolh.cust-no, oe-bolh.ship-id, FOCUS:SCREEN-VALUE IN BROWSE {&browse-name}, 1, OUTPUT char-val, OUTPUT hlp-recid).
         FIND oe-rell WHERE RECID(oe-rell) EQ hlp-recid NO-LOCK NO-ERROR.
         IF ROWID(oe-rell) NE lv-rell-rowid THEN DO:
           lv-rell-rowid = ROWID(oe-rell).
           RUN new-release.
         END.
       END.

       WHEN "i-no" THEN DO:
         RUN windows/l-ordcs.w (cocode, oe-bolh.cust-no, oe-bolh.ship-id, FOCUS:SCREEN-VALUE IN BROWSE {&browse-name}, 2, OUTPUT char-val, OUTPUT hlp-recid).
         FIND oe-rell WHERE RECID(oe-rell) EQ hlp-recid NO-LOCK NO-ERROR.
         IF ROWID(oe-rell) NE lv-rell-rowid THEN DO:
           lv-rell-rowid = ROWID(oe-rell).
           RUN new-release.
         END.
       END.        

       /*WHEN "tag" THEN DO:
         RUN windows/l-fgibn1.w (oe-boll.company, oe-boll.i-no:screen-value in browse {&browse-name}, oe-boll.job-no:screen-value in browse {&browse-name}, INT(oe-boll.job-no2:screen-value in browse {&browse-name}), oe-boll.loc:screen-value in browse {&browse-name}, oe-boll.loc-bin:screen-value in browse {&browse-name}, oe-boll.tag:screen-value in browse {&browse-name}, output lv-rowid).
         IF char-val NE "" AND FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} NE ENTRY(1,char-val) THEN DO:
           FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(1,char-val).
           RUN new-tag.
         END.
       END.*/

       WHEN "tag" THEN DO:
         RUN fgbin-help.
       END.

       WHEN "loc" THEN DO:
         RUN windows/l-loc.w (cocode, FOCUS:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT char-val).
         IF char-val NE "" THEN FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(1,char-val).
       END.

       WHEN "loc-bin" THEN DO:
         RUN windows/l-fgbin.w (cocode, oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}, FOCUS:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT char-val).
         IF char-val NE "" THEN FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(1,char-val).
       END.

       WHEN "job-no" THEN DO:
         RUN job-help.
       END.

       WHEN "job-no2" THEN DO:
         RUN job-help.
       END.

       WHEN "cust-no" THEN DO:
         RUN fgbin-help.
       END.

   END CASE.

   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  ll-qty-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i}*/
   {brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
  RUN startSearch.
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
    
    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.ord-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.ord-no IN BROWSE Browser-Table /* Order */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-ord-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.ord-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF oe-boll.ord-no IN BROWSE Browser-Table /* Order */
DO:
  ll-qty-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.i-no IN BROWSE Browser-Table /* FG Item# */
DO:
  DEF VAR ll AS CHAR NO-UNDO.

  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  IF adm-new-record THEN DO:
      ll = oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}.
      FIND FIRST oe-rel NO-LOCK
          WHERE oe-rel.company EQ oe-bolh.company
            AND oe-rel.ord-no  EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND oe-rel.i-no  EQ oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND oe-rel.stat EQ "P"
          NO-ERROR.
      IF NOT AVAIL oe-rel THEN
      FIND FIRST oe-rel NO-LOCK
          WHERE oe-rel.company EQ oe-bolh.company
            AND oe-rel.ord-no  EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND oe-rel.i-no  EQ oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND oe-rel.stat NE "C"
          NO-ERROR.
    
      IF NOT AVAIL oe-rel THEN
      FIND FIRST oe-rel NO-LOCK
          WHERE oe-rel.company EQ oe-bolh.company
            AND oe-rel.ord-no  EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND oe-rel.i-no  EQ oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}  
          NO-ERROR.
    
      IF AVAIL oe-rel THEN
          oe-boll.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = oe-rel.po-no.
  END.
/*                                                 */
/*   IF LASTKEY NE -1 THEN DO:                     */
/*     RUN valid-po NO-ERROR.                      */
/*     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */
/*   END.                                          */


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF oe-boll.i-no IN BROWSE Browser-Table /* FG Item# */
DO:
  RUN new-i-no.

  IF adm-new-record THEN DO:
      FIND FIRST oe-rel NO-LOCK
          WHERE oe-rel.company EQ oe-bolh.company
            AND oe-rel.ord-no  EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND oe-rel.i-no  EQ oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND oe-rel.stat EQ "P"
          NO-ERROR.
    
      IF AVAIL oe-rel THEN
          oe-boll.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = oe-rel.po-no.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.po-no IN BROWSE Browser-Table /* Customer PO */
DO:
/*   IF LASTKEY NE -1 THEN DO:                     */
/*     RUN valid-po NO-ERROR.                      */
/*     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */
/*   END.                                          */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF oe-boll.po-no IN BROWSE Browser-Table /* Customer PO */
DO:
  glPOModified = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.tag IN BROWSE Browser-Table /* Tag */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF oe-boll.tag IN BROWSE Browser-Table /* Tag */
DO:
  RUN new-tag.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.loc IN BROWSE Browser-Table /* Whse */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.job-no IN BROWSE Browser-Table /* Job # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.job-no2 IN BROWSE Browser-Table
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.cust-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.cust-no IN BROWSE Browser-Table /* Customer# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.cases Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF oe-boll.cases IN BROWSE Browser-Table /* Units */
DO:
  v-old-cases = INT(oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.cases Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.cases IN BROWSE Browser-Table /* Units */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    RUN valid-qty NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  IF SELF:MODIFIED AND BOLWt-log 
    THEN RUN calc-wgt.*/

  DEF VAR v-tot-pallets AS INT NO-UNDO.

  IF INT(oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}) NE v-old-cases THEN
  DO:
     RUN oe/pallcalc2.p (INPUT cocode,
                         INPUT oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                         INPUT oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT oe-boll.cust-no:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT INT(oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name}),
                         INPUT INT(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}),
                         INPUT INT(oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}),
                         OUTPUT v-tot-pallets).

     IF LASTKEY NE 13 THEN
        oe-boll.tot-pallets:SCREEN-VALUE = STRING(v-tot-pallets).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.cases Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF oe-boll.cases IN BROWSE Browser-Table /* Units */
DO:
   RUN value-changed-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF oe-boll.qty-case IN BROWSE Browser-Table /* Qty/Unit */
DO:
  v-old-qty-case = INT(oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.qty-case IN BROWSE Browser-Table /* Qty/Unit */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    RUN valid-qty NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.*/

  DEF VAR v-tot-pallets AS INT NO-UNDO.

  IF INT(oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}) NE v-old-qty-case THEN
  DO:
     RUN oe/pallcalc2.p (INPUT cocode,
                         INPUT oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                         INPUT oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT oe-boll.cust-no:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT INT(oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name}),
                         INPUT INT(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}),
                         INPUT INT(oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}),
                         OUTPUT v-tot-pallets).

     IF LASTKEY NE 13 THEN
        oe-boll.tot-pallets:SCREEN-VALUE = STRING(v-tot-pallets).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF oe-boll.qty-case IN BROWSE Browser-Table /* Qty/Unit */
DO:
   RUN value-changed-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.partial Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF oe-boll.partial IN BROWSE Browser-Table /* Partial */
DO:
  v-old-partial = INT(oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.partial Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.partial IN BROWSE Browser-Table /* Partial */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    RUN valid-qty NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.*/

  DEF VAR v-tot-pallets AS INT NO-UNDO.

  IF INT(oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name}) NE v-old-partial THEN
  DO:
     RUN oe/pallcalc2.p (INPUT cocode,
                         INPUT oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                         INPUT oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT oe-boll.cust-no:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT INT(oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name}),
                         INPUT INT(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}),
                         INPUT INT(oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}),
                         OUTPUT v-tot-pallets).

     IF LASTKEY NE 13 THEN
        oe-boll.tot-pallets:SCREEN-VALUE = STRING(v-tot-pallets).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.tot-pallets Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.tot-pallets IN BROWSE Browser-Table /* tot-pallets */
DO:

  IF LASTKEY NE -1 THEN DO:
    RUN valid-pallet-qty  NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.partial Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF oe-boll.partial IN BROWSE Browser-Table /* Partial */
DO:
   RUN value-changed-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF oe-boll.qty IN BROWSE Browser-Table /* Qty Shipped */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.qty IN BROWSE Browser-Table /* Qty Shipped */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    RUN valid-qty NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.weight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.weight Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.weight IN BROWSE Browser-Table /* Weight */
DO:

  IF AVAIL oe-bolh AND oe-bolh.posted THEN DO WITH FRAME {&frame-name}:
    APPLY "row-leave" TO {&browse-name}.       
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

RUN sbo/oerel-recalc-act.p PERSISTENT SET lr-rel-lib.


&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

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
  {src/adm/template/row-list.i "oe-bolh"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-bolh"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE apply-tag-selections B-table-Win 
PROCEDURE apply-tag-selections :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       To be implemented at a later date to reduce time record
               locks held
------------------------------------------------------------------------------*/
 
DEF BUFFER xoe-rell FOR oe-rell.

DEF VAR ip-run AS INT INIT 2 NO-UNDO.

DEF VAR v-qty AS INT NO-UNDO. /* stored as w-bin.to-apply for now */
DEF INPUT PARAMETER ipr-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER op-rowid-list AS CHAR NO-UNDO.
/* controls whether include file adds oe-boll or updates */
DEF VAR ip-rowid AS ROWID NO-UNDO.

DEF VAR li AS INT NO-UNDO.
DEF VAR li-selected-qty LIKE v-qty NO-UNDO.
DEF VAR ll-update-qty-no AS LOG NO-UNDO.
DEF VAR ll-BOLQtyPopup AS LOG NO-UNDO.
def var v-i-no    like oe-ordl.i-no NO-UNDO.
def var v-ord     like oe-ordl.ord-no NO-UNDO.
def var v-line    like oe-ordl.line NO-UNDO.
def var v-job-no  like oe-ordl.job-no NO-UNDO.
def var v-job-no2 like oe-ordl.job-no2 NO-UNDO.

DEF VAR nufile AS LOG NO-UNDO.     /* no mods in main program */
DEF VAR fil_id AS RECID NO-UNDO.   /* no mods in main program */
DEF VAR bolh_id AS RECID NO-UNDO.  /* no mods in main program */
DEF VAR boll_id AS RECID NO-UNDO.  /* no mods in main program */
DEF VAR ll-change-qty AS LOG NO-UNDO.

ll-change-qty = ip-run NE 3.

DEF BUFFER bf-oerel FOR oe-rel.


DEF BUFFER b-reftable FOR reftable.
DEF BUFFER xoe-boll FOR oe-boll.

 FIND xoe-boll WHERE ROWID(xoe-boll) EQ ipr-rowid NO-LOCK NO-ERROR.

 for each w-bin WHERE w-bin.selekt-log EQ YES 
          AND (AVAIL xoe-boll  AND                                           
           NOT CAN-FIND(FIRST oe-boll                                   
                        WHERE oe-boll.company  EQ xoe-boll.company      
                          AND oe-boll.b-no     EQ xoe-boll.b-no         
                          AND oe-boll.ord-no   EQ xoe-boll.ord-no       
                          AND oe-boll.i-no     EQ xoe-boll.i-no         
                          AND oe-boll.line     EQ xoe-boll.line         
                          AND oe-boll.rel-no   EQ xoe-boll.rel-no       
                          AND oe-boll.b-ord-no EQ xoe-boll.b-ord-no     
                          AND oe-boll.po-no    EQ xoe-boll.po-no        
                          AND oe-boll.job-no   EQ w-bin.job-no          
                          AND oe-boll.job-no2  EQ w-bin.job-no2         
                          AND oe-boll.loc      EQ w-bin.loc             
                          AND oe-boll.loc-bin  EQ w-bin.loc-bin         
                          AND oe-boll.tag      EQ w-bin.tag             
                          AND oe-boll.cust-no  EQ w-bin.cust-no         
                          AND ROWID(oe-boll)   NE ROWID(xoe-boll)       
                        USE-INDEX b-no) ), 
     first fg-bin
     where recid(fg-bin) eq w-bin.rec-id
     no-lock,
     first itemfg
     where itemfg.company eq cocode
       and itemfg.i-no    eq fg-bin.i-no
     no-lock
     break by w-bin.seq BY w-bin.tag:


   IF FALSE AND avail xoe-rell then do:
     find first oe-relh where oe-relh.r-no eq xoe-rell.r-no no-lock.

     IF ip-run NE 1 THEN
     DO:
        {oe/sel-bins.i "oe-rel"}
     END.
     ELSE
     DO:
        {oe/sel-binsrel.i "oe-rel"}
     END.


   end. /* avail xoe-rell */
   else  do:

     /* should not be found since loop is excluding existing lines */
     /* for same values */
     ip-rowid = ipr-rowid.
     find first oe-bolh where oe-bolh.b-no eq xoe-boll.b-no no-lock.

     bolh_id = recid(oe-bolh).

    /*   v-qty = w-bin.to-apply. */
     v-qty = w-bin.qty.
     
     /* this does a leave if the v-qty is zero */
     {oe/sel-bins.i "oe-bol"}
     op-rowid-list = op-rowid-list + STRING(ROWID(oe-boll)) + ",".
     oe-boll.weight = oe-boll.qty / 100 * itemfg.weight-100.

     if not avail oe-ordl then
     find first oe-ordl where oe-ordl.company eq cocode
                          and oe-ordl.ord-no  eq xoe-boll.ord-no
                          and oe-ordl.i-no    eq xoe-boll.i-no 
                        no-lock no-error.     

   end. /* not avail xoe-rell */


   /* update oe-rel.qty (actual qty), 08081302 -rely on order inq to ensure no */
   /* locking issues                                                           */
/*    FIND first bf-oerel WHERE /*oe-rel.link-no EQ {&TABLENAME}.r-no USE-INDEX link*/ */
/*                   bf-oerel.r-no = oe-rell.link-no AND                               */
/*                   bf-oerel.ord-no = oe-rell.ord-no AND                              */
/*                   bf-oerel.i-no = oe-rell.i-no                                      */
/*                   EXCLUSIVE-LOCK NO-ERROR.                                          */
/*                                                                                     */
/*    IF AVAIL bf-oerel THEN bf-oerel.qty = bf-oerel.qty + oe-rell.qty.                */
/*    FIND CURRENT bf-oerel NO-LOCK NO-ERROR.                                          */


 end. /* each w-bin */

 /* Process the oe-boll.p-c flag only once since it will check all */
 FIND oe-boll WHERE ROWID(oe-boll) EQ ipr-rowid EXCLUSIVE-LOCK NO-ERROR.
 {oe/oe-bolpc.i ALL}
 RELEASE oe-boll.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE apply-value-changed B-table-Win 
PROCEDURE apply-value-changed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR hFirstColumn AS HANDLE.

    hFirstColumn = oe-boll.ord-no:HANDLE IN BROWSE {&BROWSE-NAME}.
    APPLY 'ENTRY' TO hFirstColumn.
    RUN local-open-query.
    APPLY 'value-changed' TO BROWSE Browser-Table.
    APPLY "Entry" TO hFirstColumn.
    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

    /* To ensure vertical scrollbar is enabled when required */
    BROWSE {&BROWSE-NAME}:DOWN = BROWSE {&BROWSE-NAME}:DOWN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-all-freight B-table-Win 
PROCEDURE calc-all-freight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN calc-freight IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-orig-qty B-table-Win 
PROCEDURE calc-orig-qty :
/*------------------------------------------------------------------------------
  Purpose: Calculate current quantity on oe-rell related to oe-boll    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iprBollRow AS ROWID NO-UNDO.
  DEFINE OUTPUT PARAMETER v-rel-row AS ROWID NO-UNDO.
  DEFINE OUTPUT PARAMETER v-rell-row AS ROWID NO-UNDO.
  DEFINE OUTPUT PARAMETER v-qty-prior AS INT NO-UNDO.
  DEFINE OUTPUT PARAMETER v-last-tag AS CHAR NO-UNDO.

  DEF BUFFER bf-oe-boll FOR oe-boll.

  FIND bf-oe-boll WHERE ROWID(bf-oe-boll) EQ iprBollRow NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-oe-boll THEN
    RETURN.

  FOR EACH oe-rel
      WHERE oe-rel.company EQ bf-oe-boll.company
        AND oe-rel.ord-no  EQ bf-oe-boll.ord-no
        AND oe-rel.LINE    EQ bf-oe-boll.LINE
        AND oe-rel.i-no    EQ bf-oe-boll.i-no
        AND INDEX("SILC", oe-rel.stat) EQ 0
        AND oe-rel.link-no NE 0
      USE-INDEX ord-item NO-LOCK:
    v-rell-row = ?.
    FOR EACH oe-rell
        WHERE oe-rell.company  EQ oe-rel.company
          AND oe-rell.r-no     EQ oe-rel.link-no
          AND oe-rell.ord-no   EQ oe-rel.ord-no
          AND oe-rell.i-no     EQ oe-rel.i-no
          AND oe-rell.line     EQ oe-rel.line
          AND oe-rell.rel-no   EQ oe-rel.rel-no
          AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
          AND oe-rell.po-no    EQ oe-rel.po-no
          AND oe-rell.tag      EQ bf-oe-boll.tag
          AND oe-rell.posted   EQ YES
        USE-INDEX r-no
        BREAK BY oe-rell.r-no:
      IF LAST(oe-rell.r-no) THEN LEAVE.
    END.
    IF AVAIL oe-rell THEN
      v-qty-prior = v-qty-prior + oe-rell.qty.  

    v-rel-row = ROWID(oe-rel).  
    IF AVAIL oe-rell THEN
    v-rell-row = ROWID(oe-rell).
  END.

  v-last-tag = bf-oe-boll.tag.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-qty B-table-Win 
PROCEDURE calc-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name} = 
        STRING((DEC(oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
                DEC(oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})) +
               DEC(oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name}),
               oe-boll.qty:FORMAT IN BROWSE {&browse-name}).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-wgt B-table-Win 
PROCEDURE calc-wgt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    FIND FIRST itemfg
        WHERE itemfg.company EQ oe-bolh.company
          AND itemfg.i-no    EQ oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

      oe-boll.weight:SCREEN-VALUE IN BROWSE {&browse-name} = 
           STRING(DEC(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}) /
                 100 * (IF AVAIL itemfg THEN itemfg.weight-100 ELSE 0),
                 oe-boll.weight:FORMAT IN BROWSE {&browse-name}).
    
    IF BOLWt-log THEN DO:      

      RELEASE loadtag.

      IF TRIM(oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}) NE "" THEN 
         FIND FIRST loadtag NO-LOCK 
              WHERE loadtag.company = oe-boll.company
                AND loadtag.item-type = NO
                AND loadtag.tag-no  = oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name} NO-ERROR.
        IF AVAIL loadtag THEN 
           ASSIGN oe-boll.weight:SCREEN-VALUE IN BROWSE {&browse-name} 
                         = STRING(INT(oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}) * 
                                  loadtag.misc-dec[1] + loadtag.misc-dec[3]).
        
    END.

  END. /* Do with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE custom-panel-state B-table-Win 
PROCEDURE custom-panel-state :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAM io-panel-state AS CHAR NO-UNDO.


  IF NOT AVAIL oe-bolh OR oe-bolh.posted THEN
    io-panel-state = "disable-all".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-boll B-table-Win 
PROCEDURE delete-boll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR v-do-bol AS LOG NO-UNDO.
DEF VAR v-rel-row AS ROWID NO-UNDO.
DEF VAR v-rell-row AS ROWID NO-UNDO.
DEF VAR v-qty-prior AS INT NO-UNDO.

    DEF VAR v-last-tag AS CHAR.
    FOR EACH oe-rel
        WHERE oe-rel.company EQ oe-boll.company
          AND oe-rel.ord-no  EQ oe-boll.ord-no
          AND oe-rel.LINE    EQ oe-boll.LINE
          AND oe-rel.i-no    EQ oe-boll.i-no
          AND INDEX("SILC", oe-rel.stat) EQ 0
          AND oe-rel.link-no NE 0
        USE-INDEX ord-item NO-LOCK:
      v-rell-row = ?.
      FOR EACH oe-rell
          WHERE oe-rell.company  EQ oe-rel.company
            AND oe-rell.r-no     EQ oe-rel.link-no
            AND oe-rell.ord-no   EQ oe-rel.ord-no
            AND oe-rell.i-no     EQ oe-rel.i-no
            AND oe-rell.line     EQ oe-rel.line
            AND oe-rell.rel-no   EQ oe-rel.rel-no
            AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
            AND oe-rell.po-no    EQ oe-rel.po-no
            AND oe-rell.tag      EQ oe-boll.tag
            AND oe-rell.posted   EQ YES
          USE-INDEX r-no
          BREAK BY oe-rell.r-no:
        IF LAST(oe-rell.r-no) THEN LEAVE.
      END.
      IF AVAIL oe-rell THEN
        v-qty-prior = v-qty-prior + oe-rell.qty.  

      v-rel-row = ROWID(oe-rel).  
      IF AVAIL oe-rell THEN
      v-rell-row = ROWID(oe-rell).
    END.

    v-last-tag = oe-boll.tag.

    if oe-bolh.posted then oe-boll.deleted = yes.
    else do:
        DISABLE TRIGGERS FOR LOAD OF oe-relh.
        {oe/bollrell.i}
    
    
    
        delete oe-boll.
    
    END.








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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-orditm B-table-Win 
PROCEDURE display-orditm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input param ip-recid as recid no-undo.
  DEF VAR v-sum-qty AS INT NO-UNDO.
  DEF VAR v-qty AS INT NO-UNDO.
  DEF BUFFER xoe-rell FOR oe-rell.

  find first oe-rell where recid(oe-rell) = ip-recid NO-LOCK NO-ERROR.
      
  if not avail oe-rell or oe-rell.s-code eq "I" then do:
    message "ERROR: Line Item is INVOICE ONLY" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.

  ASSIGN v-sum-qty = 0
         v-qty     = 0.

  for each xoe-rell
              where xoe-rell.company eq cocode
                and xoe-rell.r-no    eq oe-rell.r-no
                and xoe-rell.i-no    eq oe-rell.i-no
                and xoe-rell.posted  eq yes
                and xoe-rell.qty     gt 0
              USE-INDEX r-no no-lock:
            assign
             v-sum-qty = v-sum-qty + xoe-rell.qty.
             v-qty     = v-qty     + xoe-rell.qty.
  end.

  if v-sum-qty eq 0 then do:
            bell.
            message "ERROR: No release qty remains for this item".
            next-prompt oe-boll.i-no.
            next.
  end.

  ASSIGN lv-rell-rowid = ROWID(oe-rell)
         oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = oe-rell.i-no
         oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name} = oe-rell.loc
         oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = oe-rell.loc-bin
         oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name} = oe-rell.tag
         oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name} = string(oe-rell.cases)
         oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = string(oe-rell.qty-case)
         .
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgbin-help B-table-Win 
PROCEDURE fgbin-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    RUN windows/l-fgibn4.w (cocode, oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}, oe-boll.job-no:screen-value in browse {&browse-name}, INT(oe-boll.job-no2:screen-value in browse {&browse-name}), oe-boll.loc:screen-value in browse {&browse-name}, oe-boll.loc-bin:screen-value in browse {&browse-name}, oe-boll.tag:screen-value in browse {&browse-name}, output lv-rowid).

    FIND fg-bin WHERE ROWID(fg-bin) EQ lv-rowid NO-LOCK NO-ERROR.

    IF AVAIL fg-bin AND (oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}      NE fg-bin.job-no  OR
                         INT(oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE fg-bin.job-no2 OR
                         oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}         NE fg-bin.loc     OR
                         oe-boll.loc-bin:SCREEN-VALUE IN browse {&browse-name}     NE fg-bin.loc-bin OR
                         oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}         NE fg-bin.tag)
    THEN DO:
      ASSIGN
       oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = fg-bin.job-no
       oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.job-no2)
       oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.loc
       oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.loc-bin
       oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.tag.

      RUN new-bin.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hit-save-button B-table-Win 
PROCEDURE hit-save-button :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN get-link-handle IN adm-broker-hdl
                       (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
  phandle = WIDGET-HANDLE(char-hdl).
  RUN auto-save IN phandle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE job-help B-table-Win 
PROCEDURE job-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-val AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    RUN set-local-vars.

    RUN windows/l-fgjob.w (cocode, lv-i-no, lv-job-no, OUTPUT char-val).

    IF char-val NE "" THEN
      ASSIGN
       oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = ENTRY(1,char-val)
       oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(2,char-val).
  END.  
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
 IF AVAIL oe-bolh AND oe-bolh.posted THEN DO:
     MESSAGE "BOL has been posted, add not allowed..." VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* RUN calc-all-freight. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR char-hdl AS CHAR NO-UNDO.

DEF BUFFER b-oe-bolh FOR oe-bolh.
DEF BUFFER b-oe-boll FOR oe-boll.
DEF VAR v-qty AS INT NO-UNDO.
DEF VAR v-qty-case AS INT NO-UNDO.
DEF VAR v-partial AS INT NO-UNDO.
DEF VAR v-weight AS DEC NO-UNDO.
DEF VAR v-pallets AS DEC NO-UNDO.
DEF VAR v-freight AS DEC NO-UNDO.
DEF VAR v-freight-related-modified AS LOG NO-UNDO.
DEF VAR dFreight AS DEC NO-UNDO.
DEF BUFFER bf-oe-boll FOR oe-boll.
DEF BUFFER bf2-oe-boll FOR oe-boll.
DEF BUFFER bf3-oe-boll FOR oe-boll.

DEF VAR iLastBolLine AS INT NO-UNDO.

  IF AVAIL(oe-boll) THEN
    ASSIGN v-qty = oe-boll.qty
           v-qty-case = oe-boll.qty-case
           v-partial  = oe-boll.partial
           v-weight   = oe-boll.weight
           v-freight  = oe-boll.freight
           v-pallets  = oe-boll.tot-pallets.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .


  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN BROWSE {&browse-name} oe-boll.cust-no.
  END.

  v-freight-related-modified = NO.
  IF v-qty NE oe-boll.qty
    OR v-qty-case NE oe-boll.qty-case
    OR v-partial  NE oe-boll.partial
    OR v-weight   NE oe-boll.weight
    OR v-freight  NE oe-boll.freight
    OR v-pallets  NE oe-boll.tot-pallets THEN
    v-freight-related-modified = YES.

  IF oe-boll.r-no EQ 0 THEN DO:
      FIND FIRST oe-relh NO-LOCK
      WHERE oe-relh.company  EQ oe-bolh.company
        AND oe-relh.release# EQ oe-bolh.release#
      NO-ERROR.
    IF AVAIL oe-relh THEN
        ASSIGN
        oe-boll.r-no = oe-relh.r-no
        lv-relase:SCREEN-VALUE = string(oe-bolh.release#) .
  END.

  FIND FIRST bf-oe-boll WHERE bf-oe-boll.company EQ oe-bolh.company
     AND bf-oe-boll.bol-no EQ oe-bolh.bol-no
     AND bf-oe-boll.i-no = oe-boll.i-no
     NO-LOCK NO-ERROR.
  FIND bf2-oe-boll WHERE ROWID(bf2-oe-boll) EQ ROWID(oe-boll) NO-LOCK NO-ERROR.

  IF lv-rell-rowid NE ? THEN
  FIND oe-rell WHERE ROWID(oe-rell) EQ lv-rell-rowid NO-LOCK NO-ERROR.
  IF NOT AVAIL oe-rell THEN
  FIND FIRST oe-rell NO-LOCK
      WHERE oe-rell.company EQ oe-bolh.company
        AND oe-rell.ord-no  EQ oe-boll.ord-no
        AND oe-rell.i-no    EQ oe-boll.i-no
      NO-ERROR.
  IF AVAIL oe-rell THEN
    ASSIGN
     oe-boll.line     = oe-rell.line
     oe-boll.rel-no   = oe-rell.rel-no
     oe-boll.b-ord-no = oe-rell.b-ord-no.

  FIND b-oe-bolh WHERE ROWID(b-oe-bolh) EQ ROWID(oe-bolh) NO-ERROR.
  IF AVAIL b-oe-bolh THEN DO:
    b-oe-bolh.tot-wt = 0.
    b-oe-bolh.freight = 0.
    FOR EACH b-oe-boll OF b-oe-bolh NO-LOCK:
      b-oe-bolh.tot-wt = b-oe-bolh.tot-wt + b-oe-boll.weight.
      b-oe-bolh.freight = b-oe-bolh.freight + b-oe-boll.freight.      
    END.

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "record-source", OUTPUT char-hdl).
    RUN dispatch IN WIDGET-HANDLE(char-hdl) ('display-fields').
  END.

  IF adm-new-record OR v-freight-related-modified THEN DO:
    IF oe-boll.LINE EQ 0 THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = oe-boll.company
                           AND oe-ordl.ord-no = oe-boll.ord-no
                            AND oe-ordl.i-no = oe-boll.i-no NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN oe-boll.LINE = oe-ordl.LINE.
    END.

    /* Task 04171407, only recalc freight if quantity is changed, */
    /* if weight or freight changed, just add up and display in header */ 
    IF (v-qty NE oe-boll.qty
      OR v-qty-case NE oe-boll.qty-case
      OR v-partial  NE oe-boll.partial
      OR v-pallets  NE oe-boll.tot-pallets 
      OR adm-new-record)
      AND (v-freight EQ oe-boll.freight) THEN DO:
    
      RUN oe/calcBolFrt.p (INPUT ROWID(oe-bolh), OUTPUT dFreight).
    END.
    ELSE DO:
         IF AVAIL(b-oe-bolh) THEN
           dFreight = b-oe-bolh.freight.
         ELSE
           IF AVAIL(oe-bolh) THEN
             dFreight = b-oe-bolh.freight.
    END.
     
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:       
       RUN calc-freight-header IN WIDGET-HANDLE(char-hdl) (INPUT dFreight).
       RUN dispatch IN WIDGET-HANDLE(char-hdl) ('display-fields').
    END.
     
    /* RUN calc-all-freight. */
  END. /* if freight modified or new record */


  oe-bolh.tot-pallets = oe-boll.tot-pallets.

  FOR EACH b-oe-boll fields(tot-pallets freight) WHERE
      b-oe-boll.company EQ oe-bolh.company AND
      b-oe-boll.b-no    EQ oe-bolh.b-no AND
      ROWID(b-oe-boll) NE ROWID(oe-boll) EXCLUSIVE-LOCK:
  
     oe-bolh.tot-pallets = oe-bolh.tot-pallets + b-oe-boll.tot-pallets.

  END.

  IF adm-new-record THEN DO:
    {oe/oe-bolpc.i ALL}
    FOR EACH b-oe-boll fields(p-c) 
       WHERE b-oe-boll.company EQ oe-boll.company 
         AND b-oe-boll.ord-no    EQ oe-boll.ord-no
         AND b-oe-boll.i-no      EQ oe-boll.i-no
         AND ROWID(b-oe-boll) NE ROWID(oe-boll) EXCLUSIVE-LOCK:
      b-oe-boll.p-c = oe-boll.p-c.
    END.


  END.

  DISPLAY oe-boll.weight
          oe-boll.freight WITH BROWSE {&browse-name}.
  
  /* #PN# If the scheduled release and actual release had to be created  */
  /* #PN# then make sure the oe-rell created has the same values entered */
  /* #PN# on the BOL Line                                                */
  IF lWasAdded THEN DO:
    FIND oe-rell WHERE ROWID(oe-rell) EQ rOeRell EXCLUSIVE-LOCK NO-ERROR.

    FIND CURRENT oe-boll EXCLUSIVE-LOCK.
    IF AVAIL oe-rell THEN DO:

      ASSIGN
         oe-boll.rel-no   = oe-rell.rel-no
         oe-boll.r-no     = oe-rell.r-no
         oe-boll.line     = oe-rell.line
         oe-boll.b-ord-no = oe-rell.b-ord-no.

      ASSIGN
       oe-rell.po-no    = oe-boll.po-no
       oe-rell.loc-bin  = oe-boll.loc-bin
       oe-rell.loc      = oe-boll.loc
       oe-rell.tag      = oe-boll.tag
       oe-rell.job-no   = oe-boll.job-no
       oe-rell.job-no2  = oe-boll.job-no2
       oe-rell.cust-no  = oe-boll.cust-no
       oe-rell.cases    = oe-boll.cases
       oe-rell.qty-case = oe-boll.qty-case
       oe-rell.partial  = oe-boll.partial
       oe-rell.s-code   = oe-boll.s-code
       oe-rell.qty      = oe-boll.qty
       oe-rell.lot-no   = oe-boll.lot-no.

    END. /* avail oe-rell */
    FIND CURRENT bf2-oe-boll NO-LOCK.
    RELEASE oe-rell.
  END. /* Was added */

  /* Ensure no 0 bol-line's */
  iLastBolLine = 0.
  FOR EACH  bf2-oe-boll  EXCLUSIVE
      WHERE bf2-oe-boll.company EQ oe-bolh.company  
        AND bf2-oe-boll.b-no EQ oe-bolh.b-no
        AND ROWID(bf2-oe-boll) NE ROWID(oe-boll)
      BY bf2-oe-boll.bol-line:
      iLastBolLine = iLastBolLine + 1.
      IF bf2-oe-boll.bol-line EQ 0 THEN
        bf2-oe-boll.bol-line = iLastBolLine.
  END.
  iLastBolLine = iLastBolLine + 1.
  
  IF oe-boll.bol-line EQ 0 THEN        
     oe-boll.bol-line = iLastBolLine.

  /* If there was already a BOL Line for this item, can take the r-no */
  /* from there. With a blank r-no, the BOL will not post properly    */  
  FIND FIRST bf-oe-boll WHERE bf-oe-boll.company EQ oe-bolh.company
     AND bf-oe-boll.bol-no EQ oe-bolh.bol-no
     AND bf-oe-boll.i-no = oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
     AND bf-oe-boll.r-no GT 0
     NO-LOCK NO-ERROR.
  FIND LAST bf3-oe-boll WHERE bf3-oe-boll.company = oe-bolh.company
      AND bf3-oe-boll.b-no EQ oe-boll.b-no
      AND bf3-oe-boll.i-no EQ oe-boll.i-no
      AND bf3-oe-boll.r-no EQ 0
    USE-INDEX bol-line NO-LOCK NO-ERROR.
  
  IF AVAIL bf3-oe-boll AND bf3-oe-boll.r-no EQ 0 AND AVAIL bf-oe-boll THEN DO:    
    oe-boll.r-no = bf-oe-boll.r-no.        
  END.

  Browser-Table:REFRESH() IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL oe-bolh AND oe-bolh.posted THEN DO:
     MESSAGE "BOL has been posted, copy not allowed..." VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.
  IF AVAIL oe-boll THEN DO:
    ASSIGN
      lv-copy-rowid = ROWID(oe-boll)
      lv-copy-lot-no = get-lot-no().

    IF NOT AVAIL itemfg THEN
    FIND FIRST itemfg OF oe-boll NO-LOCK NO-ERROR.
  END.

  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-boll FOR oe-boll.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF adm-new-record AND NOT adm-adding-record THEN DO:
    FIND b-oe-boll WHERE ROWID(b-oe-boll) EQ lv-copy-rowid NO-LOCK NO-ERROR.
    IF AVAIL b-oe-boll THEN BUFFER-COPY b-oe-boll TO oe-boll.

    IF lv-copy-lot-no NE "" THEN
    DO:
       
       ASSIGN oe-boll.lot-no = lv-copy-lot-no.

       lv-copy-lot-no = "".
    END.
  END.
   
  ELSE
    ASSIGN
     oe-boll.company = oe-bolh.company
     oe-boll.b-no    = oe-bolh.b-no
     oe-boll.bol-no  = oe-bolh.bol-no.

  FIND FIRST b-oe-boll NO-LOCK
      WHERE b-oe-boll.company EQ oe-bolh.company
        AND b-oe-boll.b-no    EQ oe-bolh.b-no
        AND b-oe-boll.s-code  NE ""
        AND ROWID(b-oe-boll)  NE ROWID(oe-boll)
      NO-ERROR.
  IF AVAIL b-oe-boll THEN oe-boll.s-code = b-oe-boll.s-code.
  IF oe-boll.po-no:SCREEN-VALUE IN BROWSE {&browse-name} GT "" THEN
      oe-boll.po-no = oe-boll.po-no:SCREEN-VALUE IN BROWSE {&browse-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-boll FOR oe-boll.
  DEF BUFFER b2-oe-boll FOR oe-boll.

  DEF VAR lv-bol-rowid AS ROWID NO-UNDO.
  DEF VAR li-ord-no LIKE oe-boll.ord-no NO-UNDO.
  DEF VAR li-boll-cnt AS INT NO-UNDO.
  DEF VAR dFreight AS DEC DECIMALS 6 NO-UNDO.
  li-boll-cnt = 0.
  FOR EACH bf-boll 
    WHERE bf-boll.company = oe-boll.company
      AND bf-boll.bol-no  = oe-boll.bol-no
    NO-LOCK:
      li-boll-cnt = li-boll-cnt + 1.
                   
  END.
  /* 03111302 if only one record, must delete entire bol */
  IF li-boll-cnt LE 1 THEN DO:
      MESSAGE "Must Delete Entire BOL from DELETE Button Above."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL oe-bolh AND oe-bolh.posted THEN DO:
     MESSAGE "BOL has been posted, delete not allowed..." VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.

  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  li-ord-no = oe-boll.ord-no.

  DEF VAR v-do-bol AS LOG NO-UNDO.

  DEF VAR v-rel-row AS ROWID NO-UNDO.
  DEF VAR v-rell-row AS ROWID NO-UNDO.
  DEF VAR v-qty-prior AS INT NO-UNDO.

      DEF VAR v-last-tag AS CHAR NO-UNDO.
/*   RUN calc-orig-qty (INPUT ROWID(oe-boll), OUTPUT v-rel-row, */
/*                      OUTPUT v-rell-row, OUTPUT v-qty-prior,  */
/*                      OUTPUT v-last-tag).                     */
  v-qty-prior = oe-boll.qty.

  if oe-bolh.posted then oe-boll.deleted = yes.
  else do:
      /* Dispatch standard ADM method.                             */
     DISABLE TRIGGERS FOR LOAD OF oe-relh.
     /* wfk - taken out for task 10111304 */
      /* {oe/bollrell.i}  */
     
     RUN oe/bollrell.p (INPUT ROWID(oe-boll), INPUT YES /* single line delete */).

     RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
  END.
  /* Code placed here will execute AFTER standard behavior.    */
    /* old not working
      DISABLE TRIGGERS FOR LOAD OF oe-relh.
      {oe/bollrell.i}
      */
  
/*   RUN oe/reduce-actrel.p (INPUT v-rel-row, INPUT v-rell-row, INPUT v-qty-prior). */
  RUN update-rel-qty (INPUT ROWID(oe-boll), INPUT v-qty-prior, INPUT 0).

  FIND CURRENT oe-bolh EXCLUSIVE-LOCK.
  FIND FIRST bf-boll where bf-boll.company eq oe-bolh.company and bf-boll.b-no eq oe-bolh.b-no NO-LOCK NO-ERROR.

  IF NOT AVAIL bf-boll THEN DELETE oe-bolh.

  /*   IF AVAIL oe-bolh THEN                                            */
  /*     RUN oe/palcalc.p (ROWID(oe-bolh), OUTPUT oe-bolh.tot-pallets). */

  IF AVAIL oe-bolh THEN
  DO:
    oe-bolh.tot-pallets = 0.
   
    FOR EACH b2-oe-boll WHERE b2-oe-boll.company EQ oe-bolh.company
                         AND b2-oe-boll.b-no    EQ oe-bolh.b-no
                         AND b2-oe-boll.deleted = NO:
       IF b2-oe-boll.tot-pallets = 0 THEN
          RUN oe/pallcalc.p (ROWID(oe-boll), OUTPUT b2-oe-boll.tot-pallets).
       oe-bolh.tot-pallets = oe-bolh.tot-pallets + b2-oe-boll.tot-pallets.
    END.
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
       RUN recalc-freight (OUTPUT dFreight).
       RUN calc-freight-header IN WIDGET-HANDLE(char-hdl) (INPUT oe-bolh.freight).
       RUN dispatch IN WIDGET-HANDLE(char-hdl) ('display-fields').
    END.
    Browser-Table:REFRESH() IN FRAME {&FRAME-NAME}.
  END.

  IF li-ord-no NE 0 THEN
  FOR EACH oe-rel
      WHERE oe-rel.company EQ cocode
        AND oe-rel.ord-no  EQ li-ord-no:
    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).
  END.

  RUN redisplay-header.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy B-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  IF VALID-HANDLE(lr-rel-lib) THEN
     DELETE OBJECT lr-rel-lib.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-cnt AS INT NO-UNDO.

/*   oe-boll.weight:READ-ONLY IN BROWSE {&browse-name} = NO. */
  /* Code placed here will execute PRIOR to standard behavior. */
  /*IF AVAIL oe-bolh AND oe-bolh.posted THEN DO:
     MESSAGE "BOL has been posted, update not allowed..." VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.*/

  DO li-cnt = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    APPLY "cursor-left" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
  END.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-rell-rowid = ?.

  IF NOT adm-adding-record THEN DO:
    FIND FIRST oe-rell
        WHERE oe-rell.company  EQ oe-boll.company
          AND oe-rell.ord-no   EQ oe-boll.ord-no
          AND oe-rell.i-no     EQ oe-boll.i-no
          AND oe-rell.line     EQ oe-boll.line
          AND oe-rell.r-no     EQ oe-boll.r-no
          AND oe-rell.rel-no   EQ oe-boll.rel-no
          AND oe-rell.b-ord-no EQ oe-boll.b-ord-no
        NO-LOCK NO-ERROR.
    IF AVAIL oe-rell THEN lv-rell-rowid = ROWID(oe-rell).
  END.

  DO WITH FRAME {&FRAME-NAME}:
    IF AVAIL oe-bolh AND oe-bolh.posted THEN
      APPLY "entry" TO oe-boll.weight IN BROWSE {&browse-name}.
    ELSE 
      APPLY "entry" TO oe-boll.ord-no IN BROWSE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit B-table-Win 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(lr-rel-lib) THEN
     DELETE OBJECT lr-rel-lib.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
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
  {methods/run_link.i "PANEL-TARGET" "set-buttons" "('disable-all')"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-cnt AS INT NO-UNDO.
  DEF VAR v-tot-pallets AS INT NO-UNDO.
  DEF BUFFER bf-oe-ordl FOR oe-ordl.
  DEF BUFFER bf-oe-boll FOR oe-boll.
  DEF BUFFER bf2-oe-boll FOR oe-boll.
  DEF VAR lCreateRelease AS LOG NO-UNDO.
  DEF VAR dTotQty AS DEC NO-UNDO.

  DEF VAR rOeBoll AS ROWID NO-UNDO.
  DEF VAR rAddedOeBoll AS ROWID NO-UNDO.
  DEF BUFFER bf3-oe-boll FOR oe-boll.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  IF adm-new-record THEN
    rAddedOeBoll = ROWID(oe-boll).
  RUN calc-qty.

  RUN valid-ord-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc-bin NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-tag NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-pallet-qty  NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-job-no (2) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-qty NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(oe-boll.weight:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
       RUN calc-wgt.
    
    /*pressing enter*/
    IF LASTKEY EQ 13 OR (adm-new-record AND INT(oe-boll.tot-pallets:SCREEN-VALUE) EQ 0) THEN
    DO:
       RUN oe/pallcalc2.p (INPUT cocode,
                           INPUT oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name},
                           INPUT oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                           INPUT INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                           INPUT oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name},
                           INPUT oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name},
                           INPUT oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name},
                           INPUT oe-boll.cust-no:SCREEN-VALUE IN BROWSE {&browse-name},
                           INPUT INT(oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name}),
                           INPUT INT(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}),
                           INPUT INT(oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}),
                           OUTPUT v-tot-pallets).

       oe-boll.tot-pallets:SCREEN-VALUE = STRING(v-tot-pallets).
    END.
    IF adm-new-record AND INT(fgBin:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      fgBin:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fgBinScreen()).
  END. /* do with frame */
 
  FIND FIRST bf-oe-boll WHERE bf-oe-boll.company EQ oe-bolh.company
     AND bf-oe-boll.bol-no EQ oe-bolh.bol-no
     AND bf-oe-boll.i-no = oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
     NO-LOCK NO-ERROR.

  FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ oe-bolh.company
    AND bf-oe-ordl.ord-no EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
    AND bf-oe-ordl.i-no EQ oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
    NO-LOCK NO-ERROR.
  
  lWasAdded = FALSE.
  IF adm-new-record THEN DO:

    dTotQty = (INT(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}) *
              INT(oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name})) +
              INT(oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name}).
    IF NOT AVAIL bf-oe-boll THEN DO:
        IF NOT AVAIL bf-oe-ordl THEN
          RETURN NO-APPLY.
        MESSAGE 'A release was not found on this Bill of Lading' SKIP
                'for the order number entered. ' SKIP
                'Do you want to create a new release?' VIEW-AS ALERT-BOX
                QUESTION BUTTONS OK-CANCEL UPDATE lCreateRelease.
        IF lCreateRelease THEN DO:
        rOeRell = ?.
            RUN oe/createPostedRelease.p 
                             (INPUT ROWID(bf-oe-ordl), 
                              INPUT ROWID(oe-bolh),
                              INPUT dTotQty, /* Quantity to use */
                              INPUT NO, /* Do not create an oe-boll */
                              OUTPUT rOeREll).
            lWasAdded = TRUE.
        END.
        ELSE
          RETURN NO-APPLY.
        
          
    END. /* if not avail bf-oe-boll */
      
  END. /* if adm-new-record */


  FIND bf2-oe-boll WHERE ROWID(bf2-oe-boll) EQ ROWID(oe-boll) NO-LOCK NO-ERROR.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  


  DO WITH FRAME {&FRAME-NAME}:
    DO li-cnt = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
      APPLY 'cursor-left' TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
    END.    
  END.

  ASSIGN
     lv-rell-rowid = ?.
/*      oe-boll.weight:READ-ONLY IN BROWSE {&browse-name} = yes */

  /* RUN calc-all-freight. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-bin B-table-Win 
PROCEDURE new-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-itemfg FOR itemfg.
  DO WITH FRAME {&FRAME-NAME}:
    oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    FIND FIRST fg-bin 
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no  EQ oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no2 EQ INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND fg-bin.loc     EQ oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.loc-bin EQ oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.tag     EQ oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.cust-no EQ oe-boll.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN DO:
      FIND bf-itemfg 
        WHERE bf-itemfg.company EQ fg-bin.company
          AND bf-itemfg.i-no    EQ fg-bin.i-no
        NO-LOCK NO-ERROR.

      ASSIGN
       oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.case-count)
       oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}   = fg-bin.job-no
       oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(fg-bin.job-no2)
       oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc)
       oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.loc-bin)
       oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.tag)
       oe-boll.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.cust-no)
       oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0))           
       oe-boll.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}  = fg-bin.cust-no
       oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(IF fg-bin.case-count GT 0 THEN fg-bin.case-count
                   ELSE bf-itemfg.case-count)       
       oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.qty)
       oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.partial-count)  /*Bug 08291409 - partial being recalcuated*/
       oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(TRUNC((DEC(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}) - DEC(oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name})) / DEC(oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}),0))
      .
        oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(
          DEC(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}) - (DEC(oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}) * DEC(oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}))).
        
        IF DEC(oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name}) GE DEC(oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}) AND fg-bin.partial-count EQ 0 THEN
          oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(TRUNC(DEC(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}) / DEC(oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}),0)).
        
        oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(DEC(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}) - (DEC(oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}) * 
                                                                DEC(oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}))).

    END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-i-no B-table-Win 
PROCEDURE new-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-itemfg FOR itemfg.


  DO WITH FRAME {&FRAME-NAME}:
    lv-i-name:SCREEN-VALUE IN BROWSE {&browse-name} = display-i-name (1).
    ll-qty-warned = NO.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-release B-table-Win 
PROCEDURE new-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-tot-pallets AS INT NO-UNDO.

  FIND FIRST oe-rell WHERE ROWID(oe-rell) EQ lv-rell-rowid NO-LOCK NO-ERROR.

  IF AVAIL oe-rell THEN DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(oe-rell.ord-no)
     oe-boll.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = oe-rell.po-no
     oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = oe-rell.i-no
     oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name} = oe-rell.tag
     oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name} = oe-rell.loc
     oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = oe-rell.loc-bin
     oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = oe-rell.job-no
     oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(oe-rell.job-no2)
     oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(oe-rell.cases)
     oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(oe-rell.qty-case)
     oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(oe-rell.partial).

    RUN value-changed-qty.
    
    RUN oe/pallcalc2.p (INPUT cocode,
                        INPUT oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name},
                        INPUT oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                        INPUT INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                        INPUT oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name},
                        INPUT oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name},
                        INPUT oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name},
                        INPUT oe-boll.cust-no:SCREEN-VALUE IN BROWSE {&browse-name},
                        INPUT INT(oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name}),
                        INPUT INT(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}),
                        INPUT INT(oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}),
                        OUTPUT v-tot-pallets).

    IF LASTKEY NE 13 THEN
       oe-boll.tot-pallets:SCREEN-VALUE = STRING(v-tot-pallets).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-tag B-table-Win 
PROCEDURE new-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    DEF VAR v-tot-pallets AS INT NO-UNDO.
    RUN set-local-vars.

    IF lv-tag NE "" THEN DO:
      FIND FIRST fg-bin
          WHERE fg-bin.company  EQ cocode
            AND fg-bin.tag      EQ trim(lv-tag)
            AND fg-bin.i-no     EQ trim(lv-i-no)
        USE-INDEX tag NO-LOCK NO-ERROR.
        
      IF AVAIL fg-bin THEN DO:
      ASSIGN
         oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = fg-bin.job-no
         oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.job-no2)
         oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.loc
         oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.loc-bin
         oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.tag
        .
        RUN new-bin.

        RUN oe/pallcalc2.p (INPUT cocode,
                   INPUT oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name},
                   INPUT oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                   INPUT INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                   INPUT oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name},
                   INPUT oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name},
                   INPUT oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name},
                   INPUT oe-boll.cust-no:SCREEN-VALUE IN BROWSE {&browse-name},
                   INPUT INT(oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name}),
                   INPUT INT(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}),
                   INPUT INT(oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}),
                   OUTPUT v-tot-pallets).

        RUN value-changed-qty.
        oe-boll.tot-pallets:SCREEN-VALUE = STRING(v-tot-pallets).
        fgBin:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fgBinScreen()).
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalc-freight B-table-Win 
PROCEDURE recalc-freight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER opdFreight AS DECIMAL DECIMALS 6 NO-UNDO.
DEF VAR dFreight AS DEC DECIMALS 6 NO-UNDO.
DEF VAR v-pallets AS INT NO-UNDO.
DEF VAR v-qty-pal AS DEC NO-UNDO.
DEF VAR v-frt-chg AS DEC NO-UNDO.
DEF VAR v-del-zone LIKE oe-ordl.del-zone NO-UNDO.
DEF VAR v-other-freight AS DEC NO-UNDO DECIMALS 10.
DEF VAR dTotBasis AS DECIMAL NO-UNDO DECIMALS 10.
DEF VAR tot-other-freight AS DEC NO-UNDO DECIMALS 10.
DEF VAR ldMinRate AS DEC NO-UNDO.
DEF VAR dTotFreight AS DEC NO-UNDO DECIMALS 10.
DEF BUFFER bf-oe-boll FOR oe-boll.
   FIND CURRENT oe-bolh EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL oe-bolh THEN
     RETURN.

   ASSIGN oe-bolh.tot-pallets = 0
          dTotFreight         = 0
          tot-other-freight   = 0.
   

   /* Obtain the total freight for all lines on BOL */
   FOR EACH bf-oe-boll
       WHERE bf-oe-boll.company EQ oe-bolh.company
         AND bf-oe-boll.b-no    EQ oe-bolh.b-no:
            RUN oe/pallcalc.p (ROWID(bf-oe-boll), OUTPUT bf-oe-boll.tot-pallets).
            oe-bolh.tot-pallets = oe-bolh.tot-pallets + bf-oe-boll.tot-pallets.
   END. /* each oe-boll */
   RUN oe/calcBolFrt.p (INPUT ROWID(oe-bolh), OUTPUT dTotFreight).   
   oe-bolh.freight = dTotFreight.
   FIND CURRENT oe-bolh NO-LOCK.
   opdFreight = dTotFreight.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE redisplay-header B-table-Win 
PROCEDURE redisplay-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    IF NOT AVAIL oe-bolh THEN
      RUN clear-header IN WIDGET-HANDLE(char-hdl).
    ELSE
      RUN dispatch IN WIDGET-HANDLE(char-hdl) ('display-fields').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-display B-table-Win 
PROCEDURE refresh-display :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* RUN dispatch ('open-query'). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query B-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ('row-changed').
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-bin B-table-Win 
PROCEDURE select-bin :
/*------------------------------------------------------------------------------
  Purpose:     from Char. ASI's oe/sel-bins.p 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-all-or-one AS cha NO-UNDO.
DEF VAR lv-rowids AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR ll AS LOG.
DEF VAR iSelectedQty AS INT NO-UNDO.
DEF VAR lvQty AS INT NO-UNDO.
DEF VAR v-rel-row AS ROWID NO-UNDO.
DEF VAR v-rell-row AS ROWID NO-UNDO.
DEF VAR v-qty-prior AS INT NO-UNDO.
DEF VAR v-last-tag AS CHAR NO-UNDO.
DEF VAR dFreight AS DEC DECIMALS 6 NO-UNDO.
DEF VAR v-pallets AS INT NO-UNDO.
DEF VAR v-qty-pal AS DEC NO-UNDO.
DEF VAR v-frt-chg AS DEC NO-UNDO.
DEF VAR v-del-zone LIKE oe-ordl.del-zone NO-UNDO.
DEF VAR v-other-freight AS DEC NO-UNDO DECIMALS 10.
DEF VAR dTotBasis AS DECIMAL NO-UNDO DECIMALS 10.
DEF VAR tot-other-freight AS DEC NO-UNDO DECIMALS 10.
DEF VAR ldMinRate AS DEC NO-UNDO.
DEF VAR dTotFreight AS DEC NO-UNDO DECIMALS 10.


DEF BUFFER bf-bolh FOR oe-bolh.
DEF BUFFER bf-boll FOR oe-boll.

 IF AVAIL oe-boll THEN DO:

   IF relmerge-int NE 0 THEN
     MESSAGE "Select Bins for All Jobs?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE ll.

   /* Reduce oe-rel by original quantity before selecting tags */
/*    FOR EACH bf-boll WHERE bf-boll.company EQ oe-bolh.company                  */
/*                       AND bf-boll.b-no    EQ oe-bolh.b-no                     */
/*                       AND bf-boll.deleted EQ NO:                              */
/*        RUN update-rel-qty (INPUT ROWID(bf-boll), INPUT bf-boll.qty, INPUT 0). */
/*                                                                               */
/*    END.                                                                       */


   lv-all-or-one = IF ll THEN "ALL" ELSE "ONE".

   RUN oe/d-selbin.w (2, ROWID(oe-boll), lv-all-or-one, oe-boll.i-no,
                      OUTPUT lv-rowids).
   ASSIGN v-rel-row = ROWID(oe-boll) .
   
   IF lv-rowids NE "" THEN DO:
     RUN dispatch ('open-query').
     FOR EACH w-rowid:
       DELETE w-rowid.
     END.
     DO li = 1 TO NUM-ENTRIES(lv-rowids):
       IF ENTRY(li,lv-rowids) NE "" THEN DO:
         CREATE w-rowid.
         w-rowid = ENTRY(li,lv-rowids).
       END.
     END.
     
     DO WHILE AVAIL oe-boll AND CAN-FIND(FIRST w-rowid):
       FIND FIRST w-rowid WHERE w-rowid EQ STRING(ROWID(oe-boll)) NO-ERROR.
       IF AVAIL w-rowid THEN DO:       
          DELETE w-rowid.
          iSelectedQty = iSelectedQty + oe-boll.qty.       
       END.
       RUN dispatch ('get-next').
     END.
     RUN redisplay-header.
   END.

   /* gdm - */
   FIND CURRENT oe-bolh EXCLUSIVE-LOCK.
   RUN recalc-freight (OUTPUT dTotFreight).
/*    ASSIGN oe-bolh.tot-pallets = 0                                                       */
/*           dTotFreight         = 0                                                       */
/*           tot-other-freight   = 0.                                                      */
/*                                                                                         */
/*    /* Obtain total basis weight for all lines on the BOL */                             */
/*    FOR EACH oe-boll                                                                     */
/*        WHERE oe-boll.company EQ oe-bolh.company                                         */
/*          AND oe-boll.b-no    EQ oe-bolh.b-no:                                           */
/*      RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,                                   */
/*                  INPUT oe-bolh.cust-no,                                                 */
/*                  INPUT oe-bolh.ship-id,                                                 */
/*                  INPUT oe-bolh.carrier,                                                 */
/*                  INPUT rowid(oe-boll),                                                  */
/*                  OUTPUT v-other-freight).                                               */
/*      tot-other-freight = tot-other-freight + v-other-freight.                           */
/*    END.                                                                                 */
/*                                                                                         */
/*                                                                                         */
/*    /* Obtain the total freight for all lines on BOL */                                  */
/*    FOR EACH oe-boll                                                                     */
/*        WHERE oe-boll.company EQ oe-bolh.company                                         */
/*          AND oe-boll.b-no    EQ oe-bolh.b-no:                                           */
/*                                                                                         */
/*      FIND FIRST shipto                                                                  */
/*            WHERE shipto.company EQ oe-bolh.company                                      */
/*              AND shipto.cust-no EQ oe-bolh.cust-no                                      */
/*              AND shipto.ship-id EQ oe-bolh.ship-id                                      */
/*            NO-LOCK NO-ERROR.                                                            */
/*      IF NOT AVAIL shipto THEN                                                           */
/*        NEXT.                                                                            */
/*                                                                                         */
/*      RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,                                   */
/*                  INPUT oe-bolh.cust-no,                                                 */
/*                  INPUT oe-bolh.ship-id,                                                 */
/*                  INPUT oe-bolh.carrier,                                                 */
/*                  INPUT rowid(oe-boll),                                                  */
/*                  OUTPUT v-other-freight).                                               */
/*                                                                                         */
/*      RUN oe/getLineFrt.p (oe-bolh.company,                                              */
/*                           shipto.loc,                                                   */
/*                           oe-bolh.carrier,                                              */
/*                           shipto.dest-code,                                             */
/*                           shipto.ship-zip,                                              */
/*                           v-other-freight,                                              */
/*                           tot-other-freight,                                            */
/*                           1,                                                            */
/*                           OUTPUT dFreight,                                              */
/*                           OUTPUT ldMinRate).                                            */
/*                                                                                         */
/*      ASSIGN oe-boll.freight = dFreight.                                                 */
/*             dTotFreight = dTotFreight + dFreight.                                       */
/*             dTotBasis = dTotBasis + v-other-freight.                                    */
/*             oe-bolh.tot-pallets = oe-bolh.tot-pallets + oe-boll.tot-pallets.            */
/*    END. /* each oe-boll */                                                              */
/*                                                                                         */
/*    /* If the total freight calculated is below minimum, distribute minimum to lines */  */
/*    IF dTotFreight LT ldMinRate THEN DO:                                                 */
/*                                                                                         */
/*      dTotFreight = ldMinRate.                                                           */
/*                                                                                         */
/*      /* distribute total to lines */                                                    */
/*      FOR EACH oe-boll                                                                   */
/*          WHERE oe-boll.company EQ oe-bolh.company                                       */
/*            AND oe-boll.b-no    EQ oe-bolh.b-no                                          */
/*          EXCLUSIVE-LOCK:                                                                */
/*                                                                                         */
/*        RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,                                 */
/*                  INPUT oe-bolh.cust-no,                                                 */
/*                  INPUT oe-bolh.ship-id,                                                 */
/*                  INPUT oe-bolh.carrier,                                                 */
/*                  INPUT rowid(oe-boll),                                                  */
/*                  OUTPUT v-other-freight).                                               */
/*                                                                                         */
/*        /* line freight is total / (basis  / total basis */                              */
/*        oe-boll.freight = dTotFreight * v-other-freight / dTotBasis.                     */
/*                                                                                         */
/*      END. /* each bol */                                                                */
/*                                                                                         */
/*    END. /* if below minimum */                                                          */

/*    oe-bolh.freight = dTotFreight. */


   RUN dispatch ('open-query').
   RUN redisplay-header.
   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
  
       RUN calc-freight-header IN WIDGET-HANDLE(char-hdl) (INPUT dTotFreight).
       RUN dispatch IN WIDGET-HANDLE(char-hdl) ('display-fields').
   END.
   /* Replace quantity on oe-rel after selecting tags - taken out per Joe */  
/*    FOR EACH bf-boll WHERE bf-boll.company EQ oe-bolh.company             */
/*                       AND bf-boll.b-no    EQ oe-bolh.b-no                */
/*                       AND bf-boll.deleted EQ NO:                         */
/*                                                                          */
/*        RUN update-rel-qty (INPUT ROWID(bf-boll), INPUT 0 /* orig qty */, */
/*                            INPUT bf-boll.qty /* new qty */ ).            */
/*                                                                          */
/*    END.                                                                  */

   FIND CURRENT oe-bolh NO-LOCK.

   ll-qty-warned = NO.
   RUN valid-qty-bin (iSelectedQty).

   Browser-Table:REFRESH() IN FRAME {&FRAME-NAME}.
   RUN redisplay-header.
   /* gdm - */
        IF AVAIL oe-boll THEN do:  /* task 07231405 */
        REPOSITION {&browse-name} TO ROWID v-rel-row NO-ERROR.
        RUN dispatch ('get-next').
        END.
 END.
/*MESSAGE "Select Bin/Tags completed." VIEW-AS ALERT-BOX. */
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
  {src/adm/template/snd-list.i "oe-bolh"}
  {src/adm/template/snd-list.i "oe-boll"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-local-vars B-table-Win 
PROCEDURE set-local-vars :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    ASSIGN
     lv-i-no = oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-tag = oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-loc = oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-loc-bin = oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-job-no = oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-job-no2 = INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
     lv-cust-no = oe-boll.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetBrowserSize B-table-Win 
PROCEDURE SetBrowserSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM ifHeight AS DECIMAL NO-UNDO.
    DEF INPUT PARAM ifWidth  AS DECIMAL NO-UNDO.

    MESSAGE 'SetBrowserSize'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ASSIGN
        SELF:WIDTH-CHARS    = ifWidth
        SELF:HEIGHT-CHARS   = ifHeight NO-ERROR.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-rel-qty B-table-Win 
PROCEDURE update-rel-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER iprOeBoll AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipiOrigQty AS INT NO-UNDO.
DEF INPUT PARAMETER ipiNewQty  AS INT NO-UNDO.
DEF VAR d-out AS DEC NO-UNDO.

DEF BUFFER bf-oe-boll FOR oe-boll.
DEF BUFFER bf-oe-rel FOR oe-rel.
FIND bf-oe-boll WHERE ROWID(bf-oe-boll) EQ iprOeBoll NO-LOCK NO-ERROR.
/* update oe-rel.qty (actual qty), 08081302 -rely on order inq to ensure no */
/* locking issues                                                           */
/* find related oe-rel */          
/* FIND FIRST bf-oe-rel                                                         */
/*     WHERE bf-oe-rel.company EQ bf-oe-boll.company                            */
/*       AND bf-oe-rel.ord-no  EQ bf-oe-boll.ord-no                             */
/*       AND bf-oe-rel.LINE    EQ bf-oe-boll.LINE                               */
/*       AND bf-oe-rel.i-no    EQ bf-oe-boll.i-no                               */
/*       AND INDEX("SILC", bf-oe-rel.stat) EQ 0                                 */
/*       AND bf-oe-rel.link-no NE 0                                             */
/*     USE-INDEX ord-item EXCLUSIVE-LOCK.                                       */
/*                                                                              */
/*   IF AVAIL bf-oe-rel AND VALID-HANDLE(lr-rel-lib) THEN                       */
/*     RUN recalc-act-qty IN lr-rel-lib (INPUT ROWID(bf-oe-rel), OUTPUT d-out). */
/*                                                                              */
/* /* update by the difference in quantity */                                   */
/* IF AVAIL bf-oe-rel THEN                                                      */
/*   bf-oe-rel.qty = bf-oe-rel.qty + (ipiNewQty - ipiOrigQty).                  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no B-table-Win 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF oe-boll.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND
       (oe-boll.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} NE oe-bolh.cust-no OR
        NOT CAN-FIND(FIRST fg-bin 
                     WHERE fg-bin.company EQ cocode
                       AND fg-bin.i-no    EQ oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND fg-bin.job-no  EQ oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND fg-bin.job-no2 EQ INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                       AND fg-bin.loc     EQ oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND fg-bin.loc-bin EQ oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND fg-bin.tag     EQ oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND fg-bin.cust-no EQ oe-boll.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}))
        THEN DO:
      MESSAGE "Invalid Customer#, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-boll.ord-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no B-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ cocode
          AND oe-ordl.ord-no  EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
          AND oe-ordl.i-no    EQ oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF NOT AVAIL oe-ordl THEN DO:
      MESSAGE TRIM(oe-boll.i-no:LABEL IN BROWSE {&browse-name}) +
              " not on Order, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-boll.i-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no B-table-Win 
PROCEDURE valid-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF VAR ll-valid AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN set-local-vars.

    IF lv-job-no NE "" THEN DO:
      ll-valid = CAN-FIND(FIRST job-hdr
                          WHERE job-hdr.company  EQ cocode
                            AND job-hdr.job-no   EQ lv-job-no
                            AND (job-hdr.job-no2 EQ lv-job-no2 OR ip-int LT 2)
                            AND job-hdr.i-no     EQ lv-i-no
                          USE-INDEX job-no) OR
                 CAN-FIND(FIRST fg-bin
                          WHERE fg-bin.company  EQ cocode
                            AND fg-bin.i-no     EQ lv-i-no
                            AND fg-bin.loc      EQ lv-loc
                            AND fg-bin.loc-bin  EQ lv-loc-bin
                            AND fg-bin.tag      EQ lv-tag
                            AND fg-bin.job-no   EQ lv-job-no
                            AND (fg-bin.job-no2 EQ lv-job-no2 OR ip-int LT 2)).

      IF NOT ll-valid THEN
      FOR EACH job
          WHERE job.company EQ cocode
            AND job.job-no  EQ lv-job-no
            AND (job.job-no2 EQ lv-job-no2 OR ip-int LT 2)
          NO-LOCK,

          EACH reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ lv-i-no
          NO-LOCK:
        ll-valid = YES.
        LEAVE.
      END.

      IF NOT ll-valid THEN DO:
        MESSAGE "Invalid Job#, try help..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO oe-boll.job-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.

    IF relmerge-int EQ 0 THEN DO:
      FIND FIRST oe-ordl
          WHERE oe-ordl.company  EQ cocode
            AND oe-ordl.ord-no   EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND oe-ordl.i-no     EQ oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND ((oe-ordl.job-no EQ lv-job-no AND
                  (oe-ordl.job-no2 EQ INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR
                   ip-int EQ 1)) OR
                 TRIM(oe-ordl.job-no) EQ "")
          NO-LOCK NO-ERROR.
      IF NOT AVAIL oe-ordl THEN DO:
        MESSAGE TRIM(oe-boll.job-no:LABEL IN BROWSE {&browse-name}) +
              " not for Order/FG, try help..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO oe-boll.job-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc B-table-Win 
PROCEDURE valid-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    RUN set-local-vars.

    IF NOT CAN-FIND(FIRST loc
                    WHERE loc.company EQ cocode
                      AND loc.loc     EQ lv-loc) THEN DO:
      MESSAGE "Must enter a valid warehouse...".
      APPLY "entry" TO oe-boll.loc IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
    IF lv-tag NE "" AND fgrecpt-int = 1 and
       NOT can-find(FIRST fg-bin WHERE fg-bin.company EQ cocode
                                    AND fg-bin.tag     EQ lv-tag
                                    AND fg-bin.i-no    eq lv-i-no
                                    AND fg-bin.loc EQ lv-loc
                                    AND fg-bin.loc-bin EQ lv-loc-bin
                                    AND fg-bin.cust-no EQ lv-cust-no
                                    AND fg-bin.qty > 0)
    THEN DO:
         MESSAGE "Invalid Location for the Tag."  VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO oe-boll.tag IN BROWSE {&browse-name}.
         RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin B-table-Win 
PROCEDURE valid-loc-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    RUN set-local-vars.

    IF NOT CAN-FIND(FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                      AND fg-bin.i-no    EQ ""
                      AND fg-bin.loc     EQ lv-loc
                      AND fg-bin.loc-bin EQ lv-loc-bin) THEN DO:
      MESSAGE "Must enter a valid bin...".
      APPLY "entry" TO oe-boll.loc-bin IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
    IF fgrecpt-int = 1 and
       NOT can-find(FIRST fg-bin WHERE fg-bin.company EQ cocode
                                    AND fg-bin.tag     EQ lv-tag
                                    AND fg-bin.i-no    eq lv-i-no
                                    AND fg-bin.loc EQ lv-loc
                                    AND fg-bin.loc-bin EQ lv-loc-bin
                                    AND fg-bin.cust-no EQ lv-cust-no
                                    AND fg-bin.qty > 0)
    THEN DO:
         MESSAGE "Invalid Bin for the Tag."  VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO oe-boll.tag IN BROWSE {&browse-name}.
         RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ord-no B-table-Win 
PROCEDURE valid-ord-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST oe-ord NO-LOCK
        WHERE oe-ord.company EQ oe-bolh.company
          AND oe-ord.ord-no  EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-ERROR.
    IF NOT AVAIL oe-ord THEN lv-msg = "Invalid Order#,try help...".
    ELSE IF LOOKUP(oe-ord.stat,lv-ord-ok) EQ 0 THEN
    DO:
       IF oe-ord.stat EQ "H" THEN
          lv-msg = "Order is on Credit Hold.".
       ELSE
          lv-msg = "Invalid Order#,try help...".
    END.
    IF lv-msg EQ "" AND AVAILABLE oe-ord AND oe-ord.priceHold THEN 
        lv-msg = "Order is on Price Hold.".
        
    IF lv-msg EQ ""                              AND
       AVAILABLE oe-ord                          AND 
       oe-bolh.cust-no NE oe-ord.cust-no         AND
       oe-boll.s-code NE "T"                     AND 
       NOT CAN-FIND(FIRST shipto
                    WHERE shipto.company EQ oe-ord.company
                      AND shipto.cust-no EQ oe-ord.cust-no
                      AND shipto.ship-id EQ oe-bolh.cust-no
                      AND shipto.bill    EQ YES) THEN
      lv-msg = "BOL Customer is not a billable ShipTo# for Order Customer".

    IF lv-msg NE "" THEN DO:   
      MESSAGE TRIM(lv-msg) 
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-boll.ord-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po B-table-Win 
PROCEDURE valid-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:

    FIND FIRST oe-rel NO-LOCK
        WHERE oe-rel.company EQ oe-bolh.company
          AND oe-rel.ord-no  EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
          AND oe-rel.po-no  EQ oe-boll.po-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND oe-rel.stat   EQ "P"
          NO-ERROR.
    IF NOT AVAIL oe-rel THEN
        FIND FIRST oe-rel NO-LOCK
            WHERE oe-rel.company EQ oe-bolh.company
              AND oe-rel.ord-no  EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
              AND oe-rel.po-no  EQ oe-boll.po-no:SCREEN-VALUE IN BROWSE {&browse-name}
              NO-ERROR.

    IF NOT AVAIL oe-rel THEN lv-msg = "Invalid PO# for this order...".
    ELSE IF AVAIL oe-rel AND oe-rel.stat NE "P" AND adm-new-record THEN
        lv-msg = "Release status must be 'P' to add to this bill of lading...".

    IF lv-msg NE "" THEN DO:   
      MESSAGE TRIM(lv-msg) 
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-boll.po-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty B-table-Win 
PROCEDURE valid-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      
  IF NOT ll-qty-warned THEN DO WITH FRAME {&FRAME-NAME}:
    ll-qty-warned = YES.

    FIND FIRST oe-ord
        WHERE oe-ord.company EQ cocode
          AND oe-ord.ord-no  EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
         NO-LOCK NO-ERROR.

    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ cocode
          AND oe-ordl.ord-no  EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
          AND oe-ordl.line    EQ oe-boll.line
         NO-LOCK NO-ERROR.

    IF AVAIL oe-ord                                AND
       AVAIL oe-ordl                               AND
       oe-ordl.ship-qty + INT(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}) GT
       oe-ordl.qty * (1 + (oe-ordl.over-pct / 100)) THEN
      MESSAGE "Qty Shipped will exceed Qty Ordered + Allowable Overrun..."  VIEW-AS ALERT-BOX.
  END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty-bin B-table-Win 
PROCEDURE valid-qty-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ipSelectedQty AS INT.

  IF NOT ll-qty-warned THEN DO WITH FRAME {&FRAME-NAME}:
    ll-qty-warned = YES.

    FIND FIRST oe-ord
        WHERE oe-ord.company EQ cocode
          AND oe-ord.ord-no  EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
         NO-LOCK NO-ERROR.

    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ cocode
          AND oe-ordl.ord-no  EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
          AND oe-ordl.line    EQ oe-boll.line
         NO-LOCK NO-ERROR.
    IF AVAIL oe-ord                                AND
       AVAIL oe-ordl                               AND
       oe-ordl.ship-qty + ipSelectedQty GT
       oe-ordl.qty * (1 + (oe-ordl.over-pct / 100)) THEN
      MESSAGE "Qty Shipped will exceed Qty Ordered + Allowable Overrun..."  VIEW-AS ALERT-BOX.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag B-table-Win 
PROCEDURE valid-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    RUN set-local-vars.

    IF lv-tag NE ""                                 AND
       ((CAN-FIND(FIRST fg-bin
                  WHERE fg-bin.company EQ cocode
                    AND fg-bin.tag     EQ lv-tag
                    AND fg-bin.i-no    NE lv-i-no
                  USE-INDEX tag) AND
         CAN-FIND(FIRST itemfg
                  WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ lv-i-no
                    AND (itemfg.isaset EQ NO OR
                         itemfg.alloc  NE YES))) OR
         CAN-FIND(FIRST fg-rctd
                  WHERE fg-rctd.company   EQ cocode
                    AND fg-rctd.tag       EQ lv-tag
                    AND fg-rctd.i-no      NE lv-i-no
                    AND fg-rctd.rita-code EQ "R"))  THEN DO:

      MESSAGE "Tag exists for another item..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-boll.tag IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  
    IF lv-tag NE "" AND fgrecpt-int = 1 and
       NOT can-find(FIRST fg-bin WHERE fg-bin.company EQ cocode
                                    AND fg-bin.tag     EQ lv-tag
                                    AND fg-bin.i-no    eq lv-i-no
                                    AND fg-bin.loc EQ lv-loc
                                    AND fg-bin.loc-bin EQ lv-loc-bin
                                    AND fg-bin.qty > 0)
    THEN DO:
         MESSAGE "Invalid Tag."  VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO oe-boll.tag IN BROWSE {&browse-name}.
         RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-qty B-table-Win 
PROCEDURE value-changed-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-boll FOR oe-boll.
  
  DEF VAR li-all-qty AS INT NO-UNDO.
  DEF VAR lv-line LIKE oe-boll.line NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

  
  DO WITH FRAME {&FRAME-NAME}:
    RUN calc-qty.

    IF adm-new-record AND NOT adm-adding-record THEN
    FIND FIRST b-oe-boll WHERE ROWID(b-oe-boll) EQ lv-copy-rowid NO-LOCK NO-ERROR.
               
    IF lv-rell-rowid NE ? THEN
    FIND oe-rell WHERE ROWID(oe-rell) EQ lv-rell-rowid NO-LOCK NO-ERROR.

    IF AVAIL oe-boll THEN
    FIND CURRENT oe-boll NO-LOCK NO-ERROR.

    lv-line = IF AVAIL oe-rell   THEN oe-rell.line   ELSE
              IF AVAIL b-oe-boll THEN b-oe-boll.line ELSE
              IF AVAIL oe-boll   THEN oe-boll.line   ELSE 0.
  
    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ cocode
          AND oe-ordl.ord-no  EQ INT(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
          AND oe-ordl.i-no    EQ oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND (oe-ordl.line   EQ lv-line OR lv-line EQ 0)
        NO-LOCK NO-ERROR.

    IF AVAIL oe-ordl THEN
    FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

    IF AVAIL oe-ord THEN DO:
      li-all-qty = 0.
      FOR EACH b-oe-boll FIELDS(qty)
          WHERE b-oe-boll.company EQ oe-bolh.company
            AND b-oe-boll.bol-no  EQ oe-bolh.bol-no
            AND b-oe-boll.ord-no  EQ oe-ordl.ord-no
            AND b-oe-boll.i-no    EQ oe-ordl.i-no
            AND b-oe-boll.line    EQ oe-ordl.line
            AND ROWID(b-oe-boll)  NE ROWID(oe-boll)
          NO-LOCK:
        li-all-qty = li-all-qty + b-oe-boll.qty.
      END.

      oe-boll.p-c:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING(INT(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}) + li-all-qty + oe-ordl.ship-qty GE
                 oe-ordl.qty * (1 - (oe-ordl.under-pct / 100))).
    END.

    ll-qty-warned = NO.

    RUN calc-wgt.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-pallet-qty B-table-Win 
PROCEDURE valid-pallet-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* ticket 23785 */
  DO WITH FRAME {&FRAME-NAME}:
    IF INTEGER(oe-boll.tot-pallets:SCREEN-VALUE IN BROWSE {&browse-name}) GT 9999999 THEN DO:
        ASSIGN oe-boll.tot-pallets:SCREEN-VALUE IN BROWSE {&browse-name} = string(SUBSTRING(oe-boll.tot-pallets:SCREEN-VALUE IN BROWSE {&browse-name},1,5)) .
        MESSAGE 
          "Value is too large for field." SKIP
          "Enter a quantity less than 10MM."
          VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO oe-boll.tot-pallets IN BROWSE {&browse-name}.
         RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-cust-item B-table-Win 
FUNCTION display-cust-item RETURNS CHARACTER
  ( INPUT ip-int AS INT  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 FIND itemfg 
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ IF ip-int EQ 0 THEN oe-boll.i-no ELSE oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.

  RETURN IF AVAIL itemfg THEN itemfg.part-no ELSE "".   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-i-name B-table-Win 
FUNCTION display-i-name RETURNS CHARACTER
  ( INPUT ip-int AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND itemfg 
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ IF ip-int EQ 0 THEN oe-boll.i-no ELSE oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.

  RETURN IF AVAIL itemfg THEN itemfg.i-name ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fgBin B-table-Win 
FUNCTION fgBin RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iUnitPallet AS INTEGER NO-UNDO .
  FIND FIRST fg-bin NO-LOCK WHERE fg-bin.company EQ oe-boll.company
                              AND fg-bin.job-no EQ oe-boll.job-no
                              AND fg-bin.job-no2 EQ oe-boll.job-no2
                              AND fg-bin.i-no EQ oe-boll.i-no
                              AND fg-bin.loc EQ oe-boll.loc
                              AND fg-bin.loc-bin EQ oe-boll.loc-bin
                              AND fg-bin.tag EQ oe-boll.tag NO-ERROR.

  iUnitPallet = IF AVAILABLE fg-bin THEN fg-bin.cases-unit ELSE 0 .
  IF iUnitPallet EQ 0 THEN DO:
     FIND FIRST oe-ordl NO-LOCK
           WHERE oe-ordl.company EQ oe-boll.company 
             AND oe-ordl.ord-no EQ oe-boll.ord-no 
             AND oe-ordl.i-no EQ oe-boll.i-no NO-ERROR.
      IF AVAIL oe-ordl THEN
          ASSIGN
          iUnitPallet = oe-ordl.cases-unit .
  END.
  RETURN iUnitPallet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fgBinScreen B-table-Win 
FUNCTION fgBinScreen RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iUnitPallet AS INTEGER NO-UNDO .
  FIND FIRST fg-bin NO-LOCK WHERE fg-bin.company EQ oe-bolh.company
                              AND fg-bin.job-no EQ oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                              AND fg-bin.job-no2 EQ INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                              AND fg-bin.i-no EQ oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                              AND fg-bin.loc EQ oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                              AND fg-bin.loc-bin EQ oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                              AND fg-bin.tag EQ oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name} NO-ERROR.

  iUnitPallet = IF AVAILABLE fg-bin THEN fg-bin.cases-unit ELSE 0 .
  IF iUnitPallet EQ 0 THEN DO:
      FIND FIRST oe-ordl NO-LOCK
           WHERE oe-ordl.company EQ oe-boll.company 
             AND oe-ordl.ord-no EQ int(oe-boll.ord-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
             AND oe-ordl.i-no EQ oe-boll.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NO-ERROR.
      IF AVAIL oe-ordl THEN
          ASSIGN
          iUnitPallet = oe-ordl.cases-unit .
  END.
  RETURN iUnitPallet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-cost B-table-Win 
FUNCTION get-cost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF li-cost:VISIBLE IN BROWSE Browser-Table EQ NO THEN
     RETURN 0.
    
  DEF VAR v-t-cost AS DEC DECIMALS 4 NO-UNDO.
  DEF VAR v-uom AS CHAR NO-UNDO.
  def var v-cost AS DEC DECIMALS 4 extent 4 NO-UNDO.
  def var v-cost-m AS DEC DECIMALS 4 extent 4 NO-UNDO.
  DEF VAR i AS INT NO-UNDO.

  find first job-hdr WHERE
       job-hdr.company eq oe-boll.company AND
       job-hdr.job-no  eq oe-boll.job-no AND
       job-hdr.job-no2 eq oe-boll.job-no2 AND
       job-hdr.i-no    eq oe-boll.i-no
       no-lock no-error.

  find first itemfg WHERE
       itemfg.company EQ oe-boll.company AND
       itemfg.i-no eq oe-boll.i-no
       NO-LOCK no-error.

    find first fg-bin
        where fg-bin.company eq oe-boll.company
          and fg-bin.i-no    eq oe-boll.i-no
          and fg-bin.tag     eq oe-boll.tag
          and fg-bin.loc     eq oe-boll.loc
          and fg-bin.loc-bin eq oe-boll.loc-bin
          and fg-bin.job-no  eq oe-boll.job-no
          and fg-bin.job-no2 eq oe-boll.job-no2
        no-lock no-error.
  
    if avail fg-bin and fg-bin.std-tot-cost ne 0 then
      assign
       v-cost-m[1] = fg-bin.std-lab-cost
       v-cost-m[2] = fg-bin.std-fix-cost
       v-cost-m[3] = fg-bin.std-var-cost
       v-cost-m[4] = fg-bin.std-mat-cost
       v-uom       = fg-bin.pur-uom.
       
    else
    if avail job-hdr and job-hdr.std-tot-cost ne 0 then
      assign
       v-cost-m[1] = job-hdr.std-lab-cost
       v-cost-m[2] = job-hdr.std-fix-cost
       v-cost-m[3] = job-hdr.std-var-cost
       v-cost-m[4] = job-hdr.std-mat-cost
       v-uom       = "M".
       
    else   
      assign
       v-cost-m[1] = itemfg.std-lab-cost
       v-cost-m[2] = itemfg.std-fix-cost
       v-cost-m[3] = itemfg.std-var-cost
       v-cost-m[4] = itemfg.std-mat-cost.

    if v-uom eq "" then
       v-uom = itemfg.prod-uom.

    do i = 1 to 4:

       if v-uom ne "M" then
          run sys/ref/convcuom3.p(cocode,v-uom, "M", 0, 0, 0, 0,
                                 v-cost-m[i], output v-cost-m[i]).
       
       v-cost[i] = v-cost[i] + (v-cost-m[i] * oe-boll.qty / 1000).
    end.
  
  do i = 1 to 4:
     v-cost[i] = v-cost[i] / (oe-boll.qty / 1000).
    
     if v-cost[i] eq ? then v-cost[i] = 0.
  end.

  v-t-cost = v-cost[1] + v-cost[2] + v-cost[3] + v-cost[4].

  if v-t-cost eq ? then
     v-t-cost = 0.

  RETURN v-t-cost.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-lot-no B-table-Win 
FUNCTION get-lot-no RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR v-lot-no AS CHAR NO-UNDO.

      v-lot-no = oe-boll.lot-no.
  RETURN v-lot-no.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-release B-table-Win 
FUNCTION get-release RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR v-release AS CHAR NO-UNDO.

  FIND FIRST oe-relh WHERE oe-relh.company = cocode
                       AND oe-relh.r-no = oe-boll.r-no
                     NO-LOCK NO-ERROR.
  IF AVAIL oe-relh THEN
      v-release = STRING(oe-relh.release#).

  RETURN v-release.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

