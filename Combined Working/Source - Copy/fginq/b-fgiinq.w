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

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "trans-date" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "TR Date" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR li-pallets AS INT NO-UNDO.
DEF VAR li-qty-pal AS INT NO-UNDO.
DEF VAR lv-char-val AS CHAR NO-UNDO.
DEF VAR lv-item-list AS cha NO-UNDO.
DEF VAR v-col-move AS LOG INIT YES NO-UNDO.

{sys/inc/oeinq.i}
ll-sort-asc = NOT oeinq.

&SCOPED-DEFINE key-phrase fg-rcpth.company EQ cocode

&SCOPED-DEFINE for-each1                              ~
    FOR EACH fg-rcpth                                 ~
        WHERE {&key-phrase}                           ~
          AND fg-rcpth.trans-date GE fi_date          ~
          AND fg-rcpth.i-no       BEGINS fi_i-no      ~
          AND (fg-rcpth.i-no EQ fi_i-no OR fi_i-no EQ "") ~
          AND fg-rcpth.rita-code  BEGINS fi_rita-code ~
          AND (fg-rcpth.po-no     EQ TRIM(STRING(fi_po-no,">>>>>>>>")) OR fi_po-no EQ 0) ~
          AND (fg-rcpth.job-no EQ fi_job-no OR fi_job-no EQ "")  ~
          AND (fg-rcpth.job-no2   EQ fi_job-no2 OR fi_job-no2 EQ 0 OR fi_job-no EQ "")

&SCOPED-DEFINE for-each2                           ~
    EACH fg-rdtlh NO-LOCK                          ~
    WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no      ~
      AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code ~
      AND fg-rdtlh.tag MATCHES fi_tag#

&SCOPED-DEFINE sortby-log                                                                                  ~
    IF lv-sort-by EQ "i-no"       THEN fg-rcpth.i-no                                                  ELSE ~
    IF lv-sort-by EQ "rita-code"  THEN fg-rcpth.rita-code                                             ELSE ~
    IF lv-sort-by EQ "loc"        THEN fg-rdtlh.loc                                                   ELSE ~
    IF lv-sort-by EQ "loc-bin"    THEN fg-rdtlh.loc-bin                                               ELSE ~
    IF lv-sort-by EQ "tag"        THEN fg-rdtlh.tag                                                   ELSE ~
    IF lv-sort-by EQ "qty"        THEN STRING(fg-rdtlh.qty,"9999999999")                              ELSE ~
    IF lv-sort-by EQ "cost"       THEN STRING(fg-rdtlh.cost,"9999999999.99999")                       ELSE ~
    IF lv-sort-by EQ "job-no"     THEN STRING(fg-rcpth.job-no,"x(6)") + STRING(fg-rcpth.job-no2,"99") ELSE ~
    IF lv-sort-by EQ "po-no"      THEN STRING(INT(fg-rcpth.po-no),"9999999999")                       ELSE ~
                                       STRING(INT(fg-rcpth.trans-date),"9999999999") + fg-rdtlh.rec_key + STRING(fg-rcpth.r-no,"9999999999")

&SCOPED-DEFINE sortby BY fg-rcpth.i-no BY fg-rcpth.r-no BY fg-rcpth.job-no BY fg-rcpth.job-no2

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fg-rcpth fg-rdtlh

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table fg-rcpth.i-no fg-rcpth.po-no ~
fg-rcpth.job-no fg-rcpth.job-no2 fg-rcpth.trans-date fg-rcpth.rita-code ~
fg-rdtlh.loc fg-rdtlh.loc-bin fg-rdtlh.qty fg-rdtlh.tag fg-rdtlh.cost ~
get-pallet-info (output li-qty-pal) @ li-pallets li-qty-pal @ li-qty-pal ~
fg-rdtlh.tot-wt 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table fg-rcpth.i-no ~
fg-rcpth.po-no fg-rcpth.job-no fg-rcpth.job-no2 fg-rcpth.trans-date ~
fg-rcpth.rita-code fg-rdtlh.loc fg-rdtlh.loc-bin fg-rdtlh.qty fg-rdtlh.tag ~
fg-rdtlh.cost 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table fg-rcpth fg-rdtlh
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table fg-rcpth
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table fg-rdtlh
&Scoped-define QUERY-STRING-Browser-Table FOR EACH fg-rcpth WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH fg-rdtlh WHERE fg-rdtlh.r-no eq fg-rcpth.r-no and ~
fg-rdtlh.rita-code eq fg-rcpth.rita-code NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH fg-rcpth WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH fg-rdtlh WHERE fg-rdtlh.r-no eq fg-rcpth.r-no and ~
fg-rdtlh.rita-code eq fg-rcpth.rita-code NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table fg-rcpth fg-rdtlh
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table fg-rcpth
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table fg-rdtlh


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table fi_i-no fi_job-no fi_job-no2 ~
fi_rita-code fi_date btn_go fi_tag# fi_po-no btnPreFix RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_i-no fi_job-no fi_job-no2 fi_rita-code ~
fi_date fi_name fi_q-onh fi_q-avail fi_sort-by fi_tag# fi_po-no FI_moveCol

/* Custom List Definitions                                              */
/* goFields,List-2,List-3,List-4,List-5,List-6                          */
&Scoped-define goFields fi_i-no fi_job-no fi_job-no2 fi_rita-code fi_date ~
fi_tag# fi_po-no 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-pallet-info B-table-Win 
FUNCTION get-pallet-info RETURNS INTEGER
  (OUTPUT op-qty-pal AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnPreFix 
     LABEL "<== Prefix FG Item#" 
     SIZE 21 BY 1.

DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_show 
     LABEL "&Show All" 
     SIZE 12 BY 1.

DEFINE VARIABLE fi_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.


DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_name AS CHARACTER FORMAT "x(30)" 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Vendor PO#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_q-avail AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 
     LABEL "QtyAvail" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_q-onh AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 
     LABEL "QtyOnHand" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_rita-code AS CHARACTER FORMAT "X":U 
     LABEL "Trans Code" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_tag# AS CHARACTER FORMAT "X(20)":U 
     LABEL "Tag#" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      fg-rcpth
    FIELDS(fg-rcpth.i-no
      fg-rcpth.po-no
      fg-rcpth.job-no
      fg-rcpth.job-no2
      fg-rcpth.trans-date
      fg-rcpth.rita-code), 
      fg-rdtlh SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      fg-rcpth.i-no COLUMN-LABEL "Item#" FORMAT "x(15)":U WIDTH 25
            COLUMN-FONT 2 LABEL-BGCOLOR 14
      fg-rcpth.po-no COLUMN-LABEL "Vendor PO#" FORMAT "x(9)":U
            WIDTH 14 LABEL-BGCOLOR 14
      fg-rcpth.job-no FORMAT "x(6)":U COLUMN-FONT 2 LABEL-BGCOLOR 14
      fg-rcpth.job-no2 COLUMN-LABEL "" FORMAT ">9":U LABEL-BGCOLOR 14
      fg-rcpth.trans-date COLUMN-LABEL "TR Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      fg-rcpth.rita-code COLUMN-LABEL "TR Code" FORMAT "x(1)":U
            LABEL-BGCOLOR 14
      fg-rdtlh.loc FORMAT "x(5)":U LABEL-BGCOLOR 14
      fg-rdtlh.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U WIDTH 9
            LABEL-BGCOLOR 14
      fg-rdtlh.qty COLUMN-LABEL "Quantity" FORMAT "->>>>,>>9.9<<":U
            LABEL-BGCOLOR 14
      fg-rdtlh.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U WIDTH 28
            LABEL-BGCOLOR 14
      fg-rdtlh.cost COLUMN-LABEL "Cost/M" FORMAT "->>>,>>9.99<<":U
            LABEL-BGCOLOR 14
      get-pallet-info (output li-qty-pal) @ li-pallets COLUMN-LABEL "Pallets" FORMAT "->>>>>>":U
            LABEL-BGCOLOR 14
      li-qty-pal @ li-qty-pal COLUMN-LABEL "Qty/Pallet" FORMAT "->>>>>>":U
            LABEL-BGCOLOR 14
      fg-rdtlh.tot-wt FORMAT ">>,>>9.99":U
  ENABLE
      fg-rcpth.i-no HELP "FG Item Number"
      fg-rcpth.po-no
      fg-rcpth.job-no
      fg-rcpth.job-no2
      fg-rcpth.trans-date
      fg-rcpth.rita-code
      fg-rdtlh.loc
      fg-rdtlh.loc-bin
      fg-rdtlh.qty
      fg-rdtlh.tag
      fg-rdtlh.cost
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 14.76
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 6 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     fi_i-no AT ROW 1.24 COL 12 COLON-ALIGNED
     fi_job-no AT ROW 1.24 COL 62 COLON-ALIGNED
     fi_job-no2 AT ROW 1.24 COL 75 COLON-ALIGNED
     fi_rita-code AT ROW 1.24 COL 94 COLON-ALIGNED
     fi_date AT ROW 1.24 COL 118 COLON-ALIGNED
     btn_go AT ROW 4.81 COL 14
     btn_show AT ROW 4.81 COL 28
     fi_name AT ROW 3.62 COL 12 COLON-ALIGNED HELP
          "Enter Finished Goods Name used for Alpha Numeric Searches."
     fi_q-onh AT ROW 3.62 COL 83 COLON-ALIGNED
     fi_q-avail AT ROW 3.62 COL 117 COLON-ALIGNED
     fi_sort-by AT ROW 4.81 COL 62 COLON-ALIGNED
     fi_tag# AT ROW 2.43 COL 12 COLON-ALIGNED
     fi_po-no AT ROW 2.43 COL 118 COLON-ALIGNED WIDGET-ID 2
     btnPreFix AT ROW 2.43 COL 54 WIDGET-ID 4
     FI_moveCol AT ROW 4.81 COL 125 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     "Click on Yellow Field to" VIEW-AS TEXT
          SIZE 23 BY 1 AT ROW 4.81 COL 104
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
         HEIGHT             = 19.91
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
/* BROWSE-TAB Browser-Table FI_moveCol F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1.
/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btn_show IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btn_show:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi_date IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_i-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_job-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_job-no2 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_po-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_q-avail IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_q-onh IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_rita-code IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_tag# IN FRAME F-Main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.fg-rcpth,ASI.fg-rdtlh WHERE ASI.fg-rcpth ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _JoinCode[2]      = "fg-rdtlh.r-no eq fg-rcpth.r-no and
fg-rdtlh.rita-code eq fg-rcpth.rita-code"
     _FldNameList[1]   > ASI.fg-rcpth.i-no
"fg-rcpth.i-no" "Item#" "x(15)" "character" ? ? 2 14 ? ? yes "FG Item Number" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.fg-rcpth.po-no
"fg-rcpth.po-no" "Vendor PO#" ? "character" ? ? ? 14 ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.fg-rcpth.job-no
"fg-rcpth.job-no" ? ? "character" ? ? 2 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.fg-rcpth.job-no2
"fg-rcpth.job-no2" "" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.fg-rcpth.trans-date
"fg-rcpth.trans-date" "TR Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.fg-rcpth.rita-code
"fg-rcpth.rita-code" "TR Code" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.fg-rdtlh.loc
"fg-rdtlh.loc" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.fg-rdtlh.loc-bin
"fg-rdtlh.loc-bin" "Bin" ? "character" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.fg-rdtlh.qty
"fg-rdtlh.qty" "Quantity" ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.fg-rdtlh.tag
"fg-rdtlh.tag" "Tag#" "x(20)" "character" ? ? ? 14 ? ? yes ? no no "28" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.fg-rdtlh.cost
"fg-rdtlh.cost" "Cost/M" ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"get-pallet-info (output li-qty-pal) @ li-pallets" "Pallets" "->>>>>>" ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"li-qty-pal @ li-qty-pal" "Qty/Pallet" "->>>>>>" ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   = ASI.fg-rdtlh.tot-wt
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
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.


  ASSIGN
   lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

  IF lv-column-nam BEGINS "li-" THEN DO:
    APPLY 'END-SEARCH' TO {&BROWSE-NAME}.
    RETURN NO-APPLY.
  END.

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

  IF AVAIL fg-rcpth THEN RUN display-itemfg (ROWID(fg-rcpth)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPreFix
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPreFix B-table-Win
ON CHOOSE OF btnPreFix IN FRAME F-Main /* <== Prefix FG Item# */
DO:
  fi_tag#:SCREEN-VALUE = itemfg.i-no + FILL(' ',15 - LENGTH(itemfg.i-no)) + '0'.
  APPLY 'ENTRY':U TO fi_tag#.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    IF TRIM(fi_i-no:SCREEN-VALUE) EQ "" AND
       TRIM(fi_tag#:SCREEN-VALUE) NE "" THEN DO:

      RELEASE loadtag.
      RELEASE fg-rcpth.

      fi_i-no:SCREEN-VALUE = SUBSTR(fi_tag#:SCREEN-VALUE,1,15).

      FIND FIRST loadtag NO-LOCK
          WHERE loadtag.company   EQ cocode
            AND loadtag.item-type EQ NO
            AND loadtag.tag-no    EQ fi_tag#:SCREEN-VALUE
        NO-ERROR.
      IF AVAIL loadtag THEN
        fi_i-no:SCREEN-VALUE = loadtag.i-no.
      ELSE
      FOR EACH fg-rdtlh NO-LOCK
          WHERE fg-rdtlh.company EQ cocode
            AND fg-rdtlh.tag     BEGINS fi_tag#:SCREEN-VALUE,
          FIRST fg-rcpth WHERE fg-rcpth.r-no EQ fg-rdtlh.r-no
          BY fg-rcpth.trans-date
          BY fg-rdtlh.trans-time
          BY fg-rcpth.rec_key:
        fi_i-no:SCREEN-VALUE = fg-rcpth.i-no.
        LEAVE.
      END.
    END.

    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    ASSIGN {&goFields}
      fi_tag# = fi_tag# + '*'
      ll-first = NO.
  END.

  RUN dispatch ("open-query").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show B-table-Win
ON CHOOSE OF btn_show IN FRAME F-Main /* Show All */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_i-no:SCREEN-VALUE      = ""
     fi_job-no:SCREEN-VALUE    = ""
     fi_job-no2:SCREEN-VALUE   = ""
     fi_rita-code:SCREEN-VALUE = ""
     fi_date:SCREEN-VALUE      = "01/01/0001"
     fi_tag#:SCREEN-VALUE      = ""
     fi_po-no:SCREEN-VALUE     = "".

    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON HELP OF fi_i-no IN FRAME F-Main /* FG Item# */
DO:
  ASSIGN {&goFields}
    fi_tag# = fi_tag# + '*'.

  IF fi_job-no <> "" THEN DO:
     IF fi_job-no NE "" THEN 
        fi_job-no = FILL(" ",6 - LENGTH(TRIM(fi_job-no))) + TRIM(fi_job-no).
     FOR EACH job-hdr WHERE job-hdr.company = cocode 
                        AND job-hdr.job-no = fi_job-no
                        AND job-hdr.job-no2 = fi_job-no2 NO-LOCK: 
         lv-item-list = lv-item-list + job-hdr.i-no + ",".
     END.
  END.
  ELSE lv-item-list = "".

  RUN windows/l-itemfj.w (cocode,  "", lv-item-list, OUTPUT lv-char-val).
  IF ENTRY(1,lv-char-val) NE {&self-name}:SCREEN-VALUE THEN DO: 
    {&self-name}:SCREEN-VALUE = ENTRY(1,lv-char-val).
    APPLY "value-changed" TO {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON LEAVE OF fi_job-no IN FRAME F-Main /* Job# */
DO:
  ASSIGN {&self-name} fi_job-no2.
  IF LASTKEY = -1 THEN RETURN.

  RUN valid-job (1) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON VALUE-CHANGED OF fi_job-no IN FRAME F-Main /* Job# */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1 + IF LASTKEY EQ 32 THEN 1 ELSE 0. /* added by script _caps.p */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no2 B-table-Win
ON LEAVE OF fi_job-no2 IN FRAME F-Main /* - */
DO:
    ASSIGN {&self-name}.
    IF LASTKEY = -1 THEN RETURN.

    RUN valid-job (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_po-no B-table-Win
ON LEAVE OF fi_po-no IN FRAME F-Main /* Vendor PO# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_rita-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_rita-code B-table-Win
ON HELP OF fi_rita-code IN FRAME F-Main /* Trans Code */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   RUN windows/l-tranCd.w (fi_rita-code:SCREEN-VALUE, OUTPUT char-val).
              IF char-val <> "" THEN 
                 fi_rita-code:SCREEN-VALUE = ENTRY(1,char-val).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_rita-code B-table-Win
ON VALUE-CHANGED OF fi_rita-code IN FRAME F-Main /* Trans Code */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1 + IF LASTKEY EQ 32 THEN 1 ELSE 0. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_tag#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_tag# B-table-Win
ON HELP OF fi_tag# IN FRAME F-Main /* Tag# */
DO:
  RUN windows/l-fgtag.w (cocode,fi_i-no:SCREEN-VALUE,'',OUTPUT lv-char-val).
  IF ENTRY(1,lv-char-val) NE SELF:SCREEN-VALUE THEN DO: 
    SELF:SCREEN-VALUE = ENTRY(1,lv-char-val).
    APPLY 'VALUE-CHANGED' TO {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
SESSION:DATA-ENTRY-RETURN = YES.
fi_date = 01/01/0001.
FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND
     sys-ctrl.NAME = "FGHistoryDate" NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
   DO:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = g_company
         sys-ctrl.name     = "FGHistoryDate"
         sys-ctrl.descrip  = "Default date on Finished Goods History"
         sys-ctrl.log-fld = YES
         sys-ctrl.date-fld = 01/01/2011.
   END.
   IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN
        fi_date = sys-ctrl.date-fld.
   ELSE
      fi_date = date("01/01/" + SUBSTRING(string(TODAY),7,11)).

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}


&SCOPED-DEFINE cellColumnDat b-fgiinq
{methods/browsers/setCellColumns.i}

    FI_moveCol = "Sort".
DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-itemfg B-table-Win 
PROCEDURE display-itemfg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.


  FIND fg-rcpth WHERE ROWID(fg-rcpth) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL fg-rcpth THEN
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ fg-rcpth.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN
      ASSIGN
       fi_name:SCREEN-VALUE    = itemfg.i-name
       fi_q-onh:SCREEN-VALUE   = STRING(itemfg.q-onh)
       fi_q-avail:SCREEN-VALUE = STRING(itemfg.q-avail).
  END.

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
  IF AVAIL fg-rcpth THEN RUN display-itemfg (ROWID(fg-rcpth)).

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
   fg-rcpth.i-no:READ-ONLY IN BROWSE {&browse-name} = YES
   fg-rcpth.po-no:READ-ONLY IN BROWSE {&browse-name} = YES
   fg-rcpth.job-no:READ-ONLY IN BROWSE {&browse-name} = YES
   fg-rcpth.job-no2:READ-ONLY IN BROWSE {&browse-name} = YES
   fg-rcpth.trans-date:READ-ONLY IN BROWSE {&browse-name} = YES
   fg-rcpth.rita-code:READ-ONLY IN BROWSE {&browse-name} = YES
   fg-rdtlh.loc:READ-ONLY IN BROWSE {&browse-name} = YES
   fg-rdtlh.loc-bin:READ-ONLY IN BROWSE {&browse-name} = YES
   fg-rdtlh.tag:READ-ONLY IN BROWSE {&browse-name} = YES
   fg-rdtlh.qty:READ-ONLY IN BROWSE {&browse-name} = YES
   fg-rdtlh.cost:READ-ONLY IN BROWSE {&browse-name} = YES
   FI_moveCol = "Sort"
   .
  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.

  RUN set-focus.

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
  SESSION:SET-WAIT-STATE("general").

  /* Dispatch standard ADM method.                             */
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .*/

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-first THEN
    OPEN QUERY Browser-Table
        FOR EACH fg-rcpth
            WHERE fg-rcpth.company EQ cocode
              AND fg-rcpth.i-no    EQ "zzzzzzzzzzzzzzzzzzzzzzzzz"
            NO-LOCK,
            EACH fg-rdtlh
            WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
              AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
            NO-LOCK.

  ELSE DO:
    {fginq/j-fgiinq.i}
    fi_tag# = SUBSTR(fi_tag#,1,LENGTH(fi_tag#) - 1).
  END.

  RUN dispatch ("display-fields").

  RUN dispatch ("row-changed").

  ll-first = NO.

  SESSION:SET-WAIT-STATE("").

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
  {src/adm/template/snd-list.i "fg-rcpth"}
  {src/adm/template/snd-list.i "fg-rdtlh"}

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

 APPLY "entry" TO fi_i-no IN FRAME {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR first-cust AS CHAR NO-UNDO.
DEF VAR last-cust AS CHAR NO-UNDO.

/*GET FIRST Browser-Table .
ASSIGN first-cust = cust.cust-no .
GET LAST Browser-Table .
ASSIGN last-cust = cust.cust-no . */

/*RUN fg/phon-exp.w (first-cust ,last-cust).*/

RUN fg/fgnq-exp.w ("", "").


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
    fi_i-no:SCREEN-VALUE = CAPS(fi_i-no:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST itemfg WHERE itemfg.company EQ cocode
                                   AND itemfg.i-no    EQ fi_i-no:screen-value)
    THEN DO:
      IF fi_job-no = "" THEN DO:
         MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO fi_i-no.
         RETURN ERROR.
      END.
      ELSE DO:
          APPLY "help" TO fi_i-no.
          RETURN .
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job B-table-Win 
PROCEDURE valid-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-what AS INT NO-UNDO.
DEF VAR lv-i-no AS cha NO-UNDO.
DEF VAR lv-cnt AS INT NO-UNDO.

IF fi_job-no NE "" AND length(fi_job-no) < 6 THEN 
        fi_job-no = FILL(" ",6 - LENGTH(TRIM(fi_job-no))) + TRIM(fi_job-no).

IF ip-what = 1 THEN DO:
   ASSIGN lv-cnt = 0
          lv-i-no = "".

   FOR EACH job-hdr WHERE job-hdr.company = g_company
                      AND job-hdr.job-no = fi_job-no
                      AND job-hdr.job-no2 = fi_job-no2 NO-LOCK:
       lv-cnt = lv-cnt + 1.
       lv-i-no = job-hdr.i-no.
   END.

   IF lv-cnt < 1 AND fi_job-no <> "" THEN DO:
       MESSAGE "Invalid Job#. Try Help..." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO fi_job-no IN FRAME {&FRAME-NAME}.
       RETURN ERROR.
   END.
   ELSE IF lv-cnt = 1 THEN fi_i-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = lv-i-no.
END.
ELSE IF ip-what = 2 THEN DO:
   ASSIGN lv-cnt = 0
          lv-i-no = "".

   FOR EACH job-hdr WHERE job-hdr.company = g_company
                      AND job-hdr.job-no = fi_job-no 
                      AND job-hdr.job-no2 = fi_job-no2 NO-LOCK:
       lv-cnt = lv-cnt + 1.
       lv-i-no = job-hdr.i-no.
   END.

   IF lv-cnt < 1 AND fi_job-no <> "" THEN DO:
       MESSAGE "Invalid Job#. Try Help..." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO fi_job-no2.
       RETURN ERROR.
   END.
   ELSE IF lv-cnt = 1 THEN fi_i-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = lv-i-no.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-pallet-info B-table-Win 
FUNCTION get-pallet-info RETURNS INTEGER
  (OUTPUT op-qty-pal AS INT):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

find first fg-bin
    where fg-bin.company eq cocode
      and fg-bin.i-no    eq fg-rcpth.i-no
      and fg-bin.job-no  eq fg-rcpth.job-no
      and fg-bin.job-no2 eq fg-rcpth.job-no2
      and fg-bin.loc     eq fg-rdtlh.loc
      and fg-bin.loc-bin eq fg-rdtlh.loc-bin
      and fg-bin.tag     eq fg-rdtlh.tag
      and fg-bin.cust-no eq fg-rdtlh.cust-no
    no-lock no-error.  

/*
if avail fg-bin then
  assign
   op-qty-pal = (if fg-bin.case-count   eq 0 then 1 else fg-bin.case-count)   *
                (if fg-bin.cases-unit   eq 0 then 1 else fg-bin.cases-unit)   *
                (if fg-bin.units-pallet eq 0 then 1 else fg-bin.units-pallet)
   /*li-pallets = fg-rdtlh.qty / op-qty-pal*/.

else
  op-qty-pal = (if fg-rdtlh.qty-case     eq 0 then 1 else fg-rdtlh.qty-case)    *
               (if fg-rdtlh.stacks-unit  eq 0 then 1 else fg-rdtlh.stacks-unit) *
               (if fg-rdtlh.units-pallet eq 0 then 1 else fg-rdtlh.units-pallet).
  /*assign
   li-pallets = 1
   op-qty-pal = fg-rdtlh.qty.*/

*/

ASSIGN
op-qty-pal = (IF fg-rdtlh.qty-case     NE 0 THEN fg-rdtlh.qty-case     ELSE
                IF AVAIL fg-bin AND
                   fg-bin.case-count     NE 0 THEN fg-bin.case-count     ELSE 1) *
               (IF fg-rdtlh.stacks-unit  NE 0 THEN fg-rdtlh.stacks-unit  ELSE
                IF AVAIL fg-bin AND
                   fg-bin.cases-unit     NE 0 THEN fg-bin.cases-unit     ELSE 1) *
               (IF fg-rdtlh.units-pallet NE 0 THEN fg-rdtlh.units-pallet ELSE
                IF AVAIL fg-bin AND
                   fg-bin.units-pallet   NE 0 THEN fg-bin.units-pallet   ELSE 1)

li-pallets = fg-rdtlh.qty / op-qty-pal.

{sys/inc/roundup.i li-pallets}

if li-pallets lt 0 then
   li-pallets = li-pallets * -1.

RETURN li-pallets.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

