&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
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

/*&SCOPED-DEFINE yellowColumnsName b-ldtag
*/

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

DEF VAR lv-qty-onhand AS INT NO-UNDO.
DEF VAR ll-first AS LOG INIT YES NO-UNDO.

DEF VAR ll-initial AS LOG INIT YES NO-UNDO.
DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR char-hdl AS cha NO-UNDO.
DEF VAR pHandle AS HANDLE NO-UNDO.

DEF VAR lv-show-prev AS LOG NO-UNDO.
DEF VAR lv-show-next AS LOG NO-UNDO.
DEF VAR lv-first-show-tag-no AS cha NO-UNDO.
DEF VAR lv-last-show-tag-no AS cha NO-UNDO.

DEF VAR lv-sort-by AS CHAR INIT "tag-no" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Tag" NO-UNDO.
DEF VAR ll-sort-asc AS LOG INIT YES NO-UNDO.

ASSIGN cocode = g_company
       locode = g_loc.

DO TRANSACTION:
  {sys/inc/browser.i "SSFGLabel"}
END.


&SCOPED-DEFINE key-phrase loadtag.company EQ cocode AND loadtag.item-type = NO AND loadtag.IS-case-tag = NO
&SCOPED-DEFINE for-each1 ~
    FOR EACH ASI.loadtag WHERE {&KEY-PHRASE} ~
      AND ASI.loadtag.company EQ g_company  ~
AND loadtag.item-type EQ NO /* FG Item */ ~
AND loadtag.is-case-tag EQ NO            ~
AND loadtag.tag-no BEGINS tb_tag-no       ~
AND loadtag.loc BEGINS tb_loc             ~
AND loadtag.loc-bin BEGINS tb_loc-bin      ~
AND (loadtag.job-no EQ tb_job-no OR tb_job-no EQ '')  ~
AND (loadtag.job-no2 EQ tb_job-no2 OR tb_job-no EQ '')  ~
AND (loadtag.po-no EQ tb_po-no OR tb_po-no EQ 0)       ~
AND (loadtag.ord-no EQ tb_ord-no OR tb_ord-no EQ 0)    ~
AND loadtag.i-no BEGINS tb_i-no                         ~
AND loadtag.i-name BEGINS tb_i-name USE-INDEX tag  NO-LOCK, ~
        first ASI.rfidtag OF ASI.loadtag  OUTER-JOIN 
          
&SCOPED-DEFINE for-each2 ~
    FOR EACH ASI.loadtag WHERE ASI.loadtag.company EQ g_company  ~
AND loadtag.item-type EQ NO /* FG Item */ ~
AND loadtag.is-case-tag EQ NO            ~
AND loadtag.tag-no BEGINS tb_tag-no       ~
AND loadtag.loc BEGINS tb_loc             ~
AND loadtag.loc-bin BEGINS tb_loc-bin      ~
AND (loadtag.job-no EQ tb_job-no OR tb_job-no EQ '')  ~
AND (loadtag.job-no2 EQ tb_job-no2 OR tb_job-no EQ '')  ~
AND (loadtag.po-no EQ tb_po-no OR tb_po-no EQ 0)       ~
AND (loadtag.ord-no EQ tb_ord-no OR tb_ord-no EQ 0)    ~
AND loadtag.i-no BEGINS tb_i-no                         ~
AND loadtag.i-name BEGINS tb_i-name USE-INDEX tag  NO-LOCK, ~
        each ASI.rfidtag OF ASI.loadtag WHERE rfidtag.rfidtag = tb_rfidtag  

&SCOPED-DEFINE sortby-log                                      ~
    IF lv-sort-by EQ "tag-no"     THEN loadtag.tag-no          ELSE ~
    IF lv-sort-by EQ "loc"       THEN loadtag.loc              ELSE ~
    IF lv-sort-by EQ "loc-bin"    THEN loadtag.loc-bin         ELSE ~
    IF lv-sort-by EQ "i-no"       THEN loadtag.i-no            ELSE ~
    IF lv-sort-by EQ "i-name"     THEN loadtag.i-name          ELSE ~
    IF lv-sort-by EQ "job-no"     THEN STRING(loadtag.job-no,"x(6)") + STRING(loadtag.job-no2,"99")   ELSE ~
    IF lv-sort-by EQ "po-no" THEN STRING(loadtag.po-no)   ELSE ~
    IF lv-sort-by EQ "rfidtag" THEN STRING(rfidtag.rfidtag)   ELSE ~
        loadtag.tag-no

&SCOPED-DEFINE sortby BY loadtag.tag-no

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})         

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC    

ll-initial = browser-log.

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
&Scoped-define INTERNAL-TABLES loadtag rfidtag

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table loadtag.tag-no loadtag.loc ~
loadtag.loc-bin loadtag.job-no loadtag.job-no2 loadtag.po-no loadtag.ord-no ~
loadtag.i-no loadtag.i-name loadtag.qty-case loadtag.case-bundle ~
loadtag.pallet-count loadtag.partial loadtag.tot-cases ~
display-onhand() @ lv-qty-onhand loadtag.sts rfidtag.rfidtag 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table loadtag.tag-no ~
loadtag.loc loadtag.loc-bin loadtag.job-no loadtag.job-no2 loadtag.po-no ~
loadtag.ord-no loadtag.i-no loadtag.i-name 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table loadtag
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table loadtag
&Scoped-define QUERY-STRING-Browser-Table FOR EACH loadtag WHERE ~{&KEY-PHRASE} ~
      AND ll-first eq no and ~
ASI.loadtag.company EQ g_company ~
AND loadtag.item-type EQ NO /* FG Item */ ~
AND loadtag.is-case-tag EQ NO ~
AND loadtag.tag-no BEGINS tb_tag-no ~
AND loadtag.loc BEGINS tb_loc ~
AND loadtag.loc-bin BEGINS tb_loc-bin ~
AND (loadtag.job-no EQ tb_job-no OR tb_job-no EQ '') ~
AND (loadtag.job-no2 EQ tb_job-no2 OR tb_job-no EQ '') ~
AND (loadtag.po-no EQ tb_po-no OR tb_po-no EQ 0) ~
AND (loadtag.ord-no EQ tb_ord-no OR tb_ord-no EQ 0) ~
AND loadtag.i-no BEGINS tb_i-no ~
AND loadtag.i-name BEGINS tb_i-name NO-LOCK, ~
      FIRST rfidtag OF loadtag OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH loadtag WHERE ~{&KEY-PHRASE} ~
      AND ll-first eq no and ~
ASI.loadtag.company EQ g_company ~
AND loadtag.item-type EQ NO /* FG Item */ ~
AND loadtag.is-case-tag EQ NO ~
AND loadtag.tag-no BEGINS tb_tag-no ~
AND loadtag.loc BEGINS tb_loc ~
AND loadtag.loc-bin BEGINS tb_loc-bin ~
AND (loadtag.job-no EQ tb_job-no OR tb_job-no EQ '') ~
AND (loadtag.job-no2 EQ tb_job-no2 OR tb_job-no EQ '') ~
AND (loadtag.po-no EQ tb_po-no OR tb_po-no EQ 0) ~
AND (loadtag.ord-no EQ tb_ord-no OR tb_ord-no EQ 0) ~
AND loadtag.i-no BEGINS tb_i-no ~
AND loadtag.i-name BEGINS tb_i-name NO-LOCK, ~
      FIRST rfidtag OF loadtag OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table loadtag rfidtag
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table loadtag
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table rfidtag


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 tb_tag-no tb_loc tb_loc-bin tb_job-no ~
tb_job-no2 tb_po-no tb_ord-no tb_i-no tb_i-name tb_rfidtag btn_go btn_next ~
Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS tb_tag-no tb_loc tb_loc-bin tb_job-no ~
tb_job-no2 tb_po-no tb_ord-no tb_i-no tb_i-name tb_rfidtag fi_sort-by 

/* Custom List Definitions                                              */
/* filterFields,List-2,List-3,List-4,List-5,List-6                      */
&Scoped-define filterFields tb_tag-no tb_loc tb_loc-bin tb_job-no ~
tb_job-no2 tb_po-no tb_ord-no tb_i-no tb_i-name tb_rfidtag 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-onhand B-table-Win 
FUNCTION display-onhand RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1
     FONT 6.

DEFINE BUTTON btn_next 
     LABEL "Show &Next" 
     SIZE 16 BY 1
     FONT 6.

DEFINE BUTTON btn_prev 
     LABEL "Show &Previous" 
     SIZE 20 BY 1
     FONT 6.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE tb_i-name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_i-no AS CHARACTER FORMAT "x(15)" 
     VIEW-AS FILL-IN 
     SIZE 21.8 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_job-no AS CHARACTER FORMAT "x(6)" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_job-no2 AS INTEGER FORMAT ">9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.8 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_loc AS CHARACTER FORMAT "x(5)" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_loc-bin AS CHARACTER FORMAT "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_ord-no AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_po-no AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE tb_rfidtag AS CHARACTER FORMAT "X(24)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_tag-no AS CHARACTER FORMAT "X(20)" 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 3.1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      loadtag, 
      rfidtag SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      loadtag.tag-no COLUMN-LABEL "Tag" FORMAT "X(23)":U LABEL-BGCOLOR 14
      loadtag.loc FORMAT "x(5)":U LABEL-BGCOLOR 14
      loadtag.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U LABEL-BGCOLOR 14
      loadtag.job-no COLUMN-LABEL "Job" FORMAT "x(6)":U LABEL-BGCOLOR 14
      loadtag.job-no2 COLUMN-LABEL "#" FORMAT ">9":U
      loadtag.po-no COLUMN-LABEL "PO" FORMAT ">>>>>9":U LABEL-BGCOLOR 14
      loadtag.ord-no COLUMN-LABEL "Order" FORMAT ">>>>>9":U LABEL-BGCOLOR 14
      loadtag.i-no COLUMN-LABEL "Item" FORMAT "x(15)":U LABEL-BGCOLOR 14
      loadtag.i-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      loadtag.qty-case COLUMN-LABEL "Unit!Count" FORMAT "->,>>>,>>9":U
      loadtag.case-bundle COLUMN-LABEL "Units/!Pallet" FORMAT "->,>>>,>>9":U
      loadtag.pallet-count COLUMN-LABEL "Qty Per Pallet/Tag#" FORMAT "->,>>>,>>9":U
      loadtag.partial COLUMN-LABEL "Partial" FORMAT ">>>,>>9":U
      loadtag.tot-cases FORMAT "->,>>>,>>9":U
      display-onhand() @ lv-qty-onhand COLUMN-LABEL "Qty! On Hand"
      loadtag.sts FORMAT "x(15)":U
      rfidtag.rfidtag COLUMN-LABEL "RFID#" FORMAT "x(24)":U
  ENABLE
      loadtag.tag-no
      loadtag.loc
      loadtag.loc-bin
      loadtag.job-no
      loadtag.job-no2
      loadtag.po-no
      loadtag.ord-no
      loadtag.i-no
      loadtag.i-name
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 14.76
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tb_tag-no AT ROW 1.71 COL 2 NO-LABEL
     tb_loc AT ROW 1.71 COL 32 COLON-ALIGNED HELP
          "Enter the plant/warehouse location" NO-LABEL
     tb_loc-bin AT ROW 1.71 COL 46 COLON-ALIGNED HELP
          "Enter Bin Location where Item is Stocked" NO-LABEL
     tb_job-no AT ROW 1.71 COL 58 COLON-ALIGNED HELP
          "Job Number." NO-LABEL
     tb_job-no2 AT ROW 1.71 COL 67 COLON-ALIGNED HELP
          "Enter Job sub-number." NO-LABEL
     tb_po-no AT ROW 1.71 COL 71 COLON-ALIGNED NO-LABEL
     tb_ord-no AT ROW 1.71 COL 80 COLON-ALIGNED NO-LABEL
     tb_i-no AT ROW 1.71 COL 89 COLON-ALIGNED HELP
          "Enter Item Number." NO-LABEL
     tb_i-name AT ROW 1.71 COL 111 COLON-ALIGNED HELP
          "Enter finished goods item name." NO-LABEL
     tb_rfidtag AT ROW 2.91 COL 2 NO-LABEL WIDGET-ID 10
     btn_go AT ROW 4.33 COL 2 WIDGET-ID 2
     btn_prev AT ROW 4.33 COL 16 WIDGET-ID 6
     btn_next AT ROW 4.33 COL 37 WIDGET-ID 4
     fi_sort-by AT ROW 4.33 COL 66 COLON-ALIGNED WIDGET-ID 8
     Browser-Table AT ROW 5.52 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     "Name" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.05 COL 126
          FGCOLOR 9 FONT 6
     "Warehouse" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.05 COL 34
          FGCOLOR 9 FONT 6
     "Job" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.05 COL 64
          FGCOLOR 9 FONT 6
     "PO" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 1.05 COL 75
          FGCOLOR 9 FONT 6
     "Item" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.05 COL 98
          FGCOLOR 9 FONT 6
     "Bin" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 1.05 COL 52
          FGCOLOR 9 FONT 6
     "Tag#/RFID#" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.05 COL 10
          FGCOLOR 9 FONT 6
     "Order" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.05 COL 83
          FGCOLOR 9 FONT 6
     RECT-4 AT ROW 19.1 COL 1
     RECT-5 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse,DB-Fields
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
         HEIGHT             = 19.52
         WIDTH              = 145.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table fi_sort-by F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON btn_prev IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tb_i-name IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_i-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_job-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_job-no2 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_loc IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_loc-bin IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_ord-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_po-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tb_rfidtag IN FRAME F-Main
   ALIGN-L 1                                                            */
/* SETTINGS FOR FILL-IN tb_tag-no IN FRAME F-Main
   ALIGN-L 1                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.loadtag,ASI.rfidtag OF ASI.loadtag"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST OUTER"
     _Where[1]         = "ll-first eq no and
ASI.loadtag.company EQ g_company
AND loadtag.item-type EQ NO /* FG Item */
AND loadtag.is-case-tag EQ NO
AND loadtag.tag-no BEGINS tb_tag-no
AND loadtag.loc BEGINS tb_loc
AND loadtag.loc-bin BEGINS tb_loc-bin
AND (loadtag.job-no EQ tb_job-no OR tb_job-no EQ '')
AND (loadtag.job-no2 EQ tb_job-no2 OR tb_job-no EQ '')
AND (loadtag.po-no EQ tb_po-no OR tb_po-no EQ 0)
AND (loadtag.ord-no EQ tb_ord-no OR tb_ord-no EQ 0)
AND loadtag.i-no BEGINS tb_i-no
AND loadtag.i-name BEGINS tb_i-name"
     _FldNameList[1]   > ASI.loadtag.tag-no
"loadtag.tag-no" "Tag" "X(23)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.loadtag.loc
"loadtag.loc" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.loadtag.loc-bin
"loadtag.loc-bin" "Bin" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.loadtag.job-no
"loadtag.job-no" "Job" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.loadtag.job-no2
"loadtag.job-no2" "#" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.loadtag.po-no
"loadtag.po-no" "PO" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.loadtag.ord-no
"loadtag.ord-no" "Order" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.loadtag.i-no
"loadtag.i-no" "Item" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.loadtag.i-name
"loadtag.i-name" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.loadtag.qty-case
"loadtag.qty-case" "Unit!Count" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.loadtag.case-bundle
"loadtag.case-bundle" "Units/!Pallet" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.loadtag.pallet-count
"loadtag.pallet-count" "Qty Per Pallet/Tag#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.loadtag.partial
"loadtag.partial" "Partial" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   = ASI.loadtag.tot-cases
     _FldNameList[15]   > "_<CALC>"
"display-onhand() @ lv-qty-onhand" "Qty! On Hand" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.loadtag.sts
"loadtag.sts" ? "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.rfidtag.rfidtag
"rfidtag.rfidtag" "RFID#" "x(24)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
  /*RUN startSearch.*/
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

  fi_sort-by:SCREEN-VALUE = lv-sort-by-lab.
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&filterFields}
           .

    RUN dispatch ("open-query").

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_next B-table-Win
ON CHOOSE OF btn_next IN FRAME F-Main /* Show Next */
DO:
  SESSION:SET-WAIT-STATE("general").

  DO WITH FRAME {&FRAME-NAME}:
   /* RUN set-defaults.*/
   /* tb_closed:SCREEN-VALUE  = "yes".*/

    lv-show-next = YES.
    
    ENABLE btn_prev .
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
    /*RUN set-defaults.*/
    /* tb_closed:SCREEN-VALUE  = "yes".*/

    lv-show-prev = YES.
   
    ENABLE btn_next .
    APPLY "choose" TO btn_go.
  END.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_job-no B-table-Win
ON LEAVE OF tb_job-no IN FRAME F-Main
DO:
  DEF VAR lv-job-no AS CHAR NO-UNDO.
  lv-job-no = SELF:SCREEN-VALUE.
  lv-job-no = FILL(" ",6 - LENGTH(TRIM(lv-job-no))) + lv-job-no.
  SELF:SCREEN-VALUE = lv-job-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE first-query B-table-Win 
PROCEDURE first-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-tag-no LIKE loadta.tag-no INIT "" NO-UNDO.

  &SCOPED-DEFINE key-phrase loadtag.company EQ cocode AND loadtag.item-type = NO AND loadtag.IS-case-tag = NO

  &SCOPED-DEFINE where-first1                         ~
        WHERE {&key-phrase}               
          

  RUN set-defaults.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&filterFields} .
  END.
  
  adm-query-opened = YES.
  find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "SSFGLabel"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign sys-ctrl.company = cocode
            sys-ctrl.name    = "SSFGLabel"
            sys-ctrl.descrip = "# of Records to be displayed in oe browser"
            sys-ctrl.log-fld = YES
            sys-ctrl.char-fld = ""
            sys-ctrl.int-fld = 30.
  end.
.
  IF ll-initial THEN DO:
    RELEASE loadtag.
    FIND first loadtag {&where-first1} USE-INDEX tag NO-LOCK NO-ERROR.
    DO WHILE AVAIL loadtag:
      IF loadtag.tag-no NE lv-tag-no THEN li = li + 1.
      lv-tag-no = loadtag.tag-no.
      IF li GE sys-ctrl.int-fld THEN LEAVE.

      FIND NEXT loadtag {&where-first1} USE-INDEX tag NO-LOCK NO-ERROR.
    END.


    &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}                 ~
        FOR EACH loadtag                        ~
            {&where-first1}                     ~
                AND loadtag.tag-no LE lv-tag-no ~
            USE-INDEX tag NO-LOCK, ~
          FIRST rfidtag OF loadtag OUTER-JOIN NO-LOCK

    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}. 
                   
          
  END.

  ELSE ll-first = NO.

  ll-initial = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-is-case-tag B-table-Win 
PROCEDURE get-is-case-tag :
DEF OUTPUT PARAM op-is-case-tag LIKE loadtag.is-case-tag INIT NO NO-UNDO.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-item-type B-table-Win 
PROCEDURE get-item-type :
DEF OUTPUT PARAM op-item-type LIKE loadtag.item-type INIT NO NO-UNDO.
       
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

  &SCOPED-DEFINE key-phrase loadtag.company EQ cocode AND loadtag.item-type = NO AND loadtag.IS-case-tag = NO

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
   ASSIGN loadtag.tag-no:READ-ONLY IN BROWSE {&browse-name} = YES
          loadtag.loc:READ-ONLY IN BROWSE {&browse-name} = YES
          loadtag.loc-bin:READ-ONLY IN BROWSE {&browse-name} = YES
          loadtag.job-no:READ-ONLY IN BROWSE {&browse-name} = YES
          loadtag.job-no2:READ-ONLY IN BROWSE {&browse-name} = YES
          loadtag.po-no:READ-ONLY IN BROWSE {&browse-name} = YES
          loadtag.ord-no:READ-ONLY IN BROWSE {&browse-name} = YES
          loadtag.i-no:READ-ONLY IN BROWSE {&browse-name} = YES
          loadtag.i-name:READ-ONLY IN BROWSE {&browse-name} = YES
          .

  APPLY 'ENTRY':U TO tb_tag-no IN FRAME {&FRAME-NAME}.
  

 /*
  APPLY "choose" TO btn_go.
  */
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
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .  */

  /* Code placed here will execute AFTER standard behavior.    */

  IF ll-first THEN RUN first-query.
  ELSE IF lv-show-prev OR lv-show-next THEN RUN show-all.
  ELSE DO:
    {loadtags/j-ldtag.i} 
  END.

  IF AVAIL {&first-table-in-query-{&browse-name}} THEN DO:
    RUN dispatch ("display-fields").

    RUN dispatch ("row-changed").

    /*RUN dispatch ('get-last':U).*/
    GET LAST {&browse-name}.
    IF AVAIL {&first-table-in-query-{&browse-name}} THEN
      ASSIGN lv-last-rowid = ROWID({&first-table-in-query-{&browse-name}})
             lv-last-show-tag-no = loadtag.tag-no.

    /*RUN dispatch ('get-first':U).*/
    GET FIRST {&browse-name}.
    IF AVAIL {&first-table-in-query-{&browse-name}} THEN
       ASSIGN lv-frst-rowid = ROWID({&first-table-in-query-{&browse-name}})
              lv-first-show-tag-no = loadtag.tag-no.
    ll-first = NO.
  END.
      
/*IF AVAIL oe-ord THEN*/ APPLY "value-changed" TO BROWSE {&browse-name}.
ASSIGN
lv-show-prev = NO
lv-show-next = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Navigate-browser B-table-Win 
PROCEDURE Navigate-browser :
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
    
  IF ROWID(job-hdr) EQ lv-last-rowid THEN
    op-nav-type = "L".
      
  IF ROWID(job-hdr) EQ lv-frst-rowid THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Navigate-Browser2 B-table-Win 
PROCEDURE Navigate-Browser2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ip-nav-type AS CHAR.
  DEF OUTPUT PARAMETER op-nav-type AS CHAR.
  
  DEF VAR hld-rowid AS ROWID NO-UNDO.
 
  hld-rowid = ROWID(loadtag).

  IF ip-nav-type NE "" THEN
  CASE ip-nav-type:
    WHEN "F" THEN RUN dispatch ('get-first':U).
    WHEN "L" THEN RUN dispatch ('get-last':U).
    WHEN "N" THEN DO WHILE ROWID(loadtag) EQ hld-rowid:
                    RUN dispatch ('get-next':U).
                  END. 
    WHEN "P" THEN DO WHILE ROWID(loadtag) EQ hld-rowid:
                    RUN dispatch ('get-prev':U).
    END.
  END CASE.
    
  IF ROWID(loadtag) EQ lv-last-rowid THEN
    op-nav-type = "L".
      
  IF ROWID(loadtag) EQ lv-frst-rowid THEN
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
  {src/adm/template/snd-list.i "loadtag"}
  {src/adm/template/snd-list.i "rfidtag"}

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
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     tb_tag-no:SCREEN-VALUE = ""
     tb_loc:SCREEN-VALUE = ""
     tb_loc-bin:SCREEN-VALUE = ""
     tb_i-no:SCREEN-VALUE    = ""
     tb_ord-no:SCREEN-VALUE = ""
     tb_po-no:SCREEN-VALUE  = ""
     tb_i-no:SCREEN-VALUE  = ""
     tb_i-name:SCREEN-VALUE  = ""
     tb_job-no:SCREEN-VALUE  = ""
     tb_job-no2:SCREEN-VALUE = "".
  END.

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
DEF VAR lv-tag-no AS cha NO-UNDO.

&SCOPED-DEFINE for-each-f1                            ~
    FOR EACH loadtag                                  ~
        WHERE {&key-phrase}                       
     

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "SSFGLabel"
                        no-lock no-error.
if not avail sys-ctrl then do transaction:
   create sys-ctrl.
   assign sys-ctrl.company = cocode
          sys-ctrl.name    = "SSFGLabel"
          sys-ctrl.descrip = "# of Records to be displayed in OE browser"
          sys-ctrl.log-fld = YES
          sys-ctrl.char-fld = ""
          sys-ctrl.int-fld = 30.
end.

RUN set-defaults. 
                 
IF lv-show-prev THEN DO:

 {&for-each-f1}                    ~
                AND loadtag.tag-no lE lv-first-show-tag-no  ~
            USE-INDEX tag  NO-LOCK BREAK BY loadtag.tag-no DESC:
   
           
     IF FIRST-OF(loadtag.tag-no) THEN li = li + 1.
     lv-tag-no = loadtag.tag-no.
     IF li GE sys-ctrl.int-fld THEN LEAVE.
  END.

    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
            {&for-each-f1}                    ~
             AND loadtag.tag-no gE  lv-tag-no  ~
               AND loadtag.tag-no LE lv-first-show-tag-no ~
            USE-INDEX tag  NO-LOCK, ~
            FIRST rfidtag OF loadtag OUTER-JOIN NO-LOCK

    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.
  
END.  /* lv-show-prev */
ELSE IF lv-show-next THEN DO:
    
    {&for-each-f1}                    ~
          AND loadtag.tag-no gE lv-last-show-tag-no  ~
          USE-INDEX tag  NO-LOCK BREAK BY loadtag.tag-no  :
     
       IF FIRST-OF(loadtag.tag-no) THEN li = li + 1.
       lv-tag-no = loadtag.tag-no.
       IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
            {&for-each-f1}                    ~
                AND loadtag.tag-no lE lv-tag-no  ~
                AND loadtag.tag-no gE lv-last-show-tag-no ~
            USE-INDEX tag NO-LOCK, ~
            FIRST rfidtag OF loadtag OUTER-JOIN NO-LOCK 

    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-onhand B-table-Win 
FUNCTION display-onhand RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 IF AVAIL loadtag THEN
   FIND FIRST fg-bin WHERE fg-bin.company EQ loadtag.company 
                    AND fg-bin.i-no    EQ loadtag.i-no
                    /*AND fg-bin.loc     EQ loadtag.loc
                      AND fg-bin.loc-bin EQ loadtag.loc-bin*/
                      AND fg-bin.tag     EQ loadtag.tag-no
                      /*AND fg-bin.job-no = loadtag.job-no
                      AND fg-bin.job-no2 = loadtag.job-no2*/
                      NO-LOCK NO-ERROR.
  IF AVAIL fg-bin THEN RETURN int(fg-bin.qty).
  ELSE RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

