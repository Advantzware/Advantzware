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

  File:  oeinq\b-ordfgi.w

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
DEF VAR v-parent AS HANDLE.
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR lv-sort-by AS CHAR INIT "i-no" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "FG Item#" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR li-pallets AS INT NO-UNDO.
DEF VAR li-case-count-pal AS INT NO-UNDO.
DEF VAR lead-days AS CHAR NO-UNDO.
DEF VAR v-col-move AS LOG INIT TRUE NO-UNDO.
DEF VAR v-last-receipt-units AS INT NO-UNDO.
DEF VAR v-last-receipt-unit-qty AS INT NO-UNDO.

/* {sys/inc/oeinq.i}        */
/* ll-sort-asc = NOT oeinq. */

&SCOPED-DEFINE key-phrase itemfg.company EQ cocode

&SCOPED-DEFINE for-each1                                        ~
    FOR EACH itemfg                                             ~
        WHERE {&key-phrase}                                     ~
          AND itemfg.i-no       BEGINS fi_i-no                ~
          AND itemfg.part-dscr1  BEGINS fi_part-dscr1           ~
          AND (itemfg.i-name     BEGINS TRIM(fi_i-name) OR fi_i-name EQ "") ~
          AND itemfg.part-no     BEGINS fi_part-no              
          

&SCOPED-DEFINE for-each2                           ~
    FOR EACH itemfg NO-LOCK                          ~
    WHERE itemfg.company      EQ cocode      ~
      AND itemfg.part-dscr1 EQ itemfg.part-dscr1              ~
          AND itemfg.i-no       BEGINS fi_i-no                ~
          AND itemfg.part-dscr1  BEGINS fi_part-dscr1           ~
          AND (itemfg.i-name     BEGINS TRIM(STRING(fi_i-name)) OR fi_i-name EQ "") ~
          AND itemfg.part-no     BEGINS fi_part-no  

&SCOPED-DEFINE sortby-log                                                                                   ~
    IF lv-sort-by EQ "i-no"        THEN itemfg.i-no                                                  ELSE ~
    IF lv-sort-by EQ "part-dscr1"   THEN itemfg.part-dscr1                                             ELSE ~
    IF lv-sort-by EQ "part-dscr2"     THEN itemfg.part-dscr2                                               ELSE ~
    IF lv-sort-by EQ "case-count"    THEN STRING(itemfg.case-count,"-9999999999.99999")                  ELSE ~
    IF lv-sort-by EQ "part-no"      THEN STRING(itemfg.part-no,"x(6)")                                ELSE ~
    itemfg.i-name

&SCOPED-DEFINE sortby BY itemfg.i-no BY itemfg.part-no

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}
/*
DO TRANSACTION:
   {sys\inc\fgsecur.i}
END.
  */
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
&Scoped-define INTERNAL-TABLES itemfg 

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table itemfg.i-no itemfg.i-name ~
itemfg.part-no ~
 itemfg.part-dscr1 ~
itemfg.part-dscr2 get-last-rec-unit() @ v-last-receipt-units ~
 get-last-rec-qty-unit() @ v-last-receipt-unit-qty itemfg.case-count 

 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table itemfg.case-count 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table itemfg 

&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table itemfg

&Scoped-define QUERY-STRING-Browser-Table FOR EACH itemfg WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH itemfg WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table itemfg



/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table /* btCompress BTN-updt-lead-days */ ~
fi_i-no fi_part-no fi_part-dscr1 /* fi_date btn_del fi_mat-type# btnPreFix */ ~
fi_i-name /*btn_copy */ btn_go btn_show  RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_i-no fi_part-no  fi_part-dscr1 ~
/* fi_date fi_mat-type# */ fi_i-name /* fi_name */ /* fi_q-onh fi_q-avail */ fi_sort-by FI_moveCol 

/* Custom List Definitions                                              */
/* goFields,List-2,List-3,List-4,List-5,List-6                          */
&Scoped-define goFields fi_i-no fi_part-no fi_part-dscr1 /* fi_date */ ~
/* fi_mat-type# */ fi_i-name 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-pallet-info B-table-Win 
FUNCTION get-pallet-info RETURNS INTEGER
  (OUTPUT op-case-count-pal AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-last-rec-unit B-table-Win 
FUNCTION get-last-rec-unit RETURNS INTEGER
  () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-last-rec-qty-unit B-table-Win 
FUNCTION get-last-rec-qty-unit RETURNS INTEGER
  () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btCompress 
     LABEL "Compress Trns" 
     SIZE 17 BY .95.

DEFINE BUTTON BTN-updt-lead-days 
     LABEL "Update Trans Time" 
     SIZE 21 BY 1.14.

DEFINE BUTTON btnPreFix 
     LABEL "<== Prefix FG Item#" 
     SIZE 21 BY 1.

DEFINE BUTTON btn_copy 
     LABEL "Copy" 
     SIZE 15 BY 1.

DEFINE BUTTON btn_del 
     LABEL "Delete" 
     SIZE 15 BY 1.

DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_show 
     LABEL "&Exit" 
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

DEFINE VARIABLE fi_part-no AS CHARACTER FORMAT "X(18)":U 
     LABEL "Part #" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_name AS CHARACTER FORMAT "x(30)" 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-name AS CHAR FORMAT "x(30)":U
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_q-avail AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 
     LABEL "case-countAvail" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_q-onh AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 
     LABEL "case-countOnHand" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_part-dscr1 AS CHARACTER FORMAT "X(30)":U 
     LABEL "Part Dscr" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE fi_mat-type# AS CHARACTER FORMAT "X(20)":U 
     LABEL "mat-type#" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 156 BY 5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      itemfg
       SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      itemfg.i-no COLUMN-LABEL "FG Item#" FORMAT "x(18)":U WIDTH 21
            LABEL-BGCOLOR 14
      itemfg.i-name COLUMN-LABEL "Name" FORMAT "x(32)":U
            WIDTH 32 LABEL-BGCOLOR 14
      itemfg.part-no FORMAT "x(32)":U WIDTH 32 LABEL-BGCOLOR 14      
      itemfg.part-dscr1 COLUMN-LABEL "Dscr1" FORMAT "x(32)":U
            WIDTH 32 LABEL-BGCOLOR 14
      itemfg.part-dscr2 COLUMN-LABEL "Dscr2" FORMAT "x(32)":U WIDTH 32
            LABEL-BGCOLOR 14
      get-last-rec-unit() @ v-last-receipt-units COLUMN-LABEL "Last Rec!Units" FORMAT "->>>>,>>9":U
      get-last-rec-qty-unit() @ v-last-receipt-unit-qty COLUMN-LABEL "Last Rec!Qty" FORMAT "->>>>,>>9":U
      itemfg.case-count COLUMN-LABEL "Count" FORMAT "->>>>,>>9":U
            LABEL-BGCOLOR 14
  ENABLE
/*      itemfg.i-no
      itemfg.i-name
      itemfg.part-no      
      itemfg.part-dscr1
      itemfg.part-dscr2 */
      itemfg.case-count
    WITH NO-ASSIGN SEPARATORS SIZE 228 BY 20
         FONT 2.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 6 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
/*      btCompress AT ROW 4.81 COL 139.4 WIDGET-ID 12        */
/*      BTN-updt-lead-days AT ROW 3.57 COL 135.4 WIDGET-ID 8 */
     fi_i-no AT ROW 1.24 COL 12 COLON-ALIGNED
     fi_part-no AT ROW 1.24 COL 62 COLON-ALIGNED
     
     fi_part-dscr1 AT ROW 1.24 COL 114 COLON-ALIGNED
/*      fi_date AT ROW 1.24 COL 116 COLON-ALIGNED     */
/*      btn_del AT ROW 1.24 COL 141.4 WIDGET-ID 4     */
/*      fi_mat-type# AT ROW 2.43 COL 12 COLON-ALIGNED */
/*      btnPreFix AT ROW 2.43 COL 54                  */
     fi_i-name AT ROW 2.43 COL 12 COLON-ALIGNED WIDGET-ID 2
/*      btn_copy AT ROW 2.43 COL 141.4 WIDGET-ID 6 */
    /* fi_name AT ROW 3.62 COL 12 COLON-ALIGNED HELP
          "Enter Finished Goods Name used for Alpha Numeric Searches." */
/*      fi_q-onh AT ROW 3.62 COL 83 COLON-ALIGNED    */
/*      fi_q-avail AT ROW 3.62 COL 115 COLON-ALIGNED */
     btn_go AT ROW 4.81 COL 3
     btn_show AT ROW 4.81 COL 18
     fi_sort-by AT ROW 4.81 COL 39.2 COLON-ALIGNED
     FI_moveCol AT ROW 4.81 COL 124 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     "Click on Yellow Field, Sorts From 1st to Last" VIEW-AS TEXT
          SIZE 43 BY 1 AT ROW 4.81 COL 81.2
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
   External Tables: ASI.cust
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
         HEIGHT             = 20.52
         WIDTH              = 156.
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

/* SETTINGS FOR FILL-IN fi_date IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_i-no IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi_part-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_i-name IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_q-avail IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_q-onh IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_part-dscr1 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_mat-type# IN FRAME F-Main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.itemfg OF ASI.cust ,ASI.itemfg WHERE ASI.itemfg ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,, FIRST OUTER"
     _JoinCode[2]      = "itemfg.company EQ itemfg.company
AND itemfg.part-dscr1 EQ itemfg.part-dscr1"
     _FldNameList[1]   > ASI.itemfg.i-no
"itemfg.i-no" "FG Item#" "x(15)" "character" ? ? ? 14 ? ? yes "" no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.itemfg.i-name
"itemfg.i-name" "Vendor PO#" ? "character" ? ? ? 14 ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.itemfg.part-no
"itemfg.part-no" ? ? "character" ? ? ? 14 ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.itemfg.job-date
"itemfg.job-date" "TR Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"STRING(itemfg.lead-days,'HH:MM') @ lead-days" "Tr Time" ? ? ? ? ? ? ? ? no ? no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.itemfg.part-dscr1
"itemfg.part-dscr1" "TR!Code" ? "character" ? ? ? 14 ? ? yes ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.itemfg.part-dscr2
"itemfg.part-dscr2" "Cust#" ? "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.itemfg.loc
"itemfg.loc" "Ware-!house" ? "character" ? ? ? 14 ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.itemfg.frt-class
"itemfg.frt-class" "Bin" ? "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.itemfg.case-count
"itemfg.case-count" "Quantity" "->>>>,>>9.99" "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.itemfg.mat-type
"itemfg.mat-type" "mat-type#" "x(25)" "character" ? ? ? 14 ? ? yes ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.itemfg.cost-ptd
"itemfg.cost-ptd" "cost-ptd" ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.itemfg.weight-100
"itemfg.weight-100" "Units" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.itemfg.case-count
"itemfg.case-count" "case-count/Unit" "->>>,>>9" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"get-pallet-info (output li-case-count-pal) @ li-pallets" "Pallets" "->>>>>>" ? ? ? ? 14 ? ? no ? no no "9.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.itemfg.w-score
"itemfg.w-score" "Units/Pallet" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
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
ON MOUSE-SELECT-DBLCLICK OF Browser-Table IN FRAME F-Main
DO:
/*   IF USERID("nosweat") EQ "asi" THEN DO: */
    RUN set-read-only (NO).

    APPLY "entry" TO itemfg.i-no IN BROWSE {&browse-name}.
/*   END. */
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

  IF lv-column-nam BEGINS "li-" THEN DO:
    APPLY 'END-SEARCH' TO {&BROWSE-NAME}.
    RETURN NO-APPLY.
  END.

  IF lv-column-nam BEGINS "part-no" THEN
    ASSIGN
     lv-column-nam = "part-no"
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

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*
&Scoped-define SELF-NAME itemfg.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF itemfg.i-no IN BROWSE Browser-Table /* FG Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 */
/*
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF itemfg.i-no IN BROWSE Browser-Table /* FG Item# */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
*/
/*
&Scoped-define SELF-NAME itemfg.i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.i-name Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF itemfg.i-name IN BROWSE Browser-Table /* Vendor PO# */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
*/

  /*
&Scoped-define SELF-NAME itemfg.part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.part-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF itemfg.part-no IN BROWSE Browser-Table /* Job# */
DO:
  /*IF itemfg.part-dscr1:SCREEN-VALUE IN BROWSE {&browse-name} NE "S" THEN DO:
    APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    */
/*
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.part-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF itemfg.part-no IN BROWSE Browser-Table /* Job# */
DO:
  DEF VAR lv-part-no AS CHAR NO-UNDO.

  
  ASSIGN
   lv-part-no = TRIM({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name})  
   lv-part-no = FILL(" ",6 - LENGTH(lv-part-no)) + lv-part-no
   {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} = lv-part-no .
   
END.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 */
/*
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.part-no Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF itemfg.part-no IN BROWSE Browser-Table /* Job# */
DO:
  RUN update-record.
END.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 */

/*
&Scoped-define SELF-NAME itemfg.part-dscr1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.part-dscr1 Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF itemfg.part-dscr1 IN BROWSE Browser-Table /* TR!Code */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
  */
    /*
&Scoped-define SELF-NAME itemfg.part-dscr2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.part-dscr2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF itemfg.part-dscr2 IN BROWSE Browser-Table /* Cust# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-part-dscr2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
      */
/*
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.part-dscr2 Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF itemfg.part-dscr2 IN BROWSE Browser-Table /* Cust# */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
  */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.case-count Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF itemfg.case-count IN BROWSE Browser-Table /* Quantity */
DO:
    IF LASTKEY NE -1 THEN DO:
      RUN valid-count NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.

  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* &Scoped-define SELF-NAME BTN-updt-lead-days                              */
/* &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-updt-lead-days B-table-Win */
/* ON CHOOSE OF BTN-updt-lead-days IN FRAME F-Main /* Update Trans Time */  */
/* DO:                                                                      */
/*    Browser-Table:REFRESH().                                              */
/*    RUN dispatch ("open-query").                                          */
/*                                                                          */
/* END.                                                                     */
/*                                                                          */
/* /* _UIB-CODE-BLOCK-END */                                                */
/* &ANALYZE-RESUME                                                          */


/* &Scoped-define SELF-NAME btnPreFix                               */
/* &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPreFix B-table-Win  */
/* ON CHOOSE OF btnPreFix IN FRAME F-Main /* <== Prefix FG Item# */ */
/* DO:                                                              */
/* END.                                                             */
/*                                                                  */
/* /* _UIB-CODE-BLOCK-END */                                        */
/* &ANALYZE-RESUME                                                  */


/* &Scoped-define SELF-NAME btn_copy                              */
/* &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_copy B-table-Win */
/* ON CHOOSE OF btn_copy IN FRAME F-Main /* Copy */               */
/* DO:                                                            */
/* END.                                                           */
/*                                                                */
/* /* _UIB-CODE-BLOCK-END */                                      */
/* &ANALYZE-RESUME                                                */


/* &Scoped-define SELF-NAME btn_del                              */
/* &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_del B-table-Win */
/* ON CHOOSE OF btn_del IN FRAME F-Main /* Delete */             */
/* DO:                                                           */
/* END.                                                          */
/*                                                               */
/* /* _UIB-CODE-BLOCK-END */                                     */
/* &ANALYZE-RESUME                                               */


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&goFields}.

    RUN dispatch ("row-changed").

  &SCOPED-DEFINE open-query           ~
      OPEN QUERY {&browse-name}       ~
          {&for-each2}                
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show B-table-Win
ON CHOOSE OF btn_show IN FRAME F-Main /* Show All */
DO:
    RUN close-it IN v-parent.
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
   DO WITH FRAME F-Main:
     ASSIGN {&self-name} = CAPS({&self-name}:SCREEN-VALUE). 
     DISP {&self-name} WITH FRAME F-MAIN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-name B-table-Win
ON VALUE-CHANGED OF fi_i-name IN FRAME F-Main /* FG Item# */
DO:
   DO WITH FRAME F-Main:
     ASSIGN {&self-name} = CAPS({&self-name}:SCREEN-VALUE). 
     DISP {&self-name} WITH FRAME F-MAIN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-name B-table-Win
ON LEAVE OF itemfg.case-count IN BROWSE {&BROWSE-name} /* FG Item# */
DO:
   IF LASTKEY NE -1 THEN DO:
     RUN valid-count NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME fi_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no B-table-Win
ON LEAVE OF fi_part-no IN FRAME F-Main /* Job# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no B-table-Win
ON VALUE-CHANGED OF fi_part-no IN FRAME F-Main /* Job# */
DO:
   DO WITH FRAME F-Main:
     ASSIGN {&self-name} = CAPS({&self-name}:SCREEN-VALUE). 
     DISP {&self-name} WITH FRAME F-MAIN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-name B-table-Win
ON LEAVE OF fi_i-name IN FRAME F-Main /* Vendor PO# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_part-dscr1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-dscr1 B-table-Win
ON HELP OF fi_part-dscr1 IN FRAME F-Main /* Trans Code */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-dscr1 B-table-Win
ON LEAVE OF fi_part-dscr1 IN FRAME F-Main /* Trans Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-dscr1 B-table-Win
ON VALUE-CHANGED OF fi_part-dscr1 IN FRAME F-Main /* Trans Code */
DO:
   DO WITH FRAME F-Main:
     ASSIGN {&self-name} = CAPS({&self-name}:SCREEN-VALUE). 
     DISP {&self-name} WITH FRAME F-MAIN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */


&SCOPED-DEFINE cellColumnDat b-ordfgi

/*{methods/browsers/setCellColumns.i} */

{sys/inc/f3help.i}  /* asi field contents help */
SESSION:DATA-ENTRY-RETURN = YES.

/* FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND            */
/*      sys-ctrl.NAME = "FGHistoryDate" NO-LOCK NO-ERROR.                */
/*                                                                       */
/*   IF NOT AVAIL sys-ctrl THEN                                          */
/*    DO:                                                                */
/*       CREATE sys-ctrl.                                                */
/*       ASSIGN                                                          */
/*          sys-ctrl.company  = g_company                                */
/*          sys-ctrl.name     = "FGHistoryDate"                          */
/*          sys-ctrl.descrip  = "Default date on Finished Goods History" */
/*          sys-ctrl.log-fld = YES                                       */
/*          sys-ctrl.date-fld = 01/01/2011.                              */
/*    END.                                                               */
/*    IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN                        */
/*         fi_date = sys-ctrl.date-fld.                                  */
/*    ELSE                                                               */
/*       fi_date = date("01/01/" + SUBSTRING(string(TODAY),7,11)).       */


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
  {src/adm/template/row-list.i "itemfg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "itemfg"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-current-item B-table-Win 
PROCEDURE get-current-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER op-i-no AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    op-i-no = CAPS(itemfg.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
              NO-ERROR.
  END.
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

/*     IF USERID("NOSWEAT") EQ "ASI" THEN    */
/*        ASSIGN btn_del:HIDDEN = NO         */
/*               btn_del:SENSITIVE = YES     */
/*               btn_copy:HIDDEN = NO        */
/*               btn_copy:SENSITIVE = YES    */
/*               btCompress:HIDDEN = NO      */
/*               btCompress:SENSITIVE = YES. */

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

  

  /* Code placed here will execute AFTER standard behavior.    */
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  {methods/winReSizeLocInit.i}
  
  ASSIGN
/*    itemfg.i-no:READ-ONLY IN BROWSE {&browse-name} = YES       */
/*    itemfg.i-name:READ-ONLY IN BROWSE {&browse-name} = YES     */
/*    itemfg.part-no:READ-ONLY IN BROWSE {&browse-name} = YES    */
/*    itemfg.part-dscr1:READ-ONLY IN BROWSE {&browse-name} = YES */
/*    itemfg.part-dscr2:READ-ONLY IN BROWSE {&browse-name} = YES */
   itemfg.case-count:READ-ONLY IN BROWSE {&browse-name} = YES
   FI_moveCol = "Sort".

  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.
/*   IF USERID("NOSWEAT") NE "ASI" THEN  */
/*       ASSIGN btn_copy:VISIBLE = NO    */
/*              btn_del:VISIBLE = NO     */
/*              btCompress:VISIBLE = NO. */
  RUN set-focus.
  /* RUN setCellColumns. */
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

  fi_mat-type# = fi_mat-type# + '*'.
  
  fi_mat-type# = SUBSTR(fi_mat-type#,1,LENGTH(fi_mat-type#) - 1).

  RUN dispatch ("display-fields").

  RUN dispatch ("row-changed").
   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query B-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  

  DO WITH FRAME {&FRAME-NAME}:
    RUN dispatch ("open-query").
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ("row-changed").
  END.

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
  {src/adm/template/snd-list.i "itemfg"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-read-only B-table-Win 
PROCEDURE set-read-only :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-log AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
/*      itemfg.i-no:READ-ONLY IN BROWSE {&browse-name}         = ip-log */
/*      itemfg.i-name:READ-ONLY IN BROWSE {&browse-name}       = ip-log */
/*      itemfg.part-no:READ-ONLY IN BROWSE {&browse-name}      = ip-log */
/*      itemfg.part-dscr1:READ-ONLY IN BROWSE {&browse-name}   = ip-log */
/*      itemfg.part-dscr2:READ-ONLY IN BROWSE {&browse-name}   = ip-log */
     itemfg.case-count:READ-ONLY IN BROWSE {&browse-name}   = ip-log.
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
      /* Object instance weight-100 can go here to replace standard behavior
         or add new weight-100. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-record B-table-Win 
PROCEDURE update-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>                          
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-itemfg FOR itemfg.
  

  DO WITH FRAME {&FRAME-NAME}:
    FIND b-itemfg WHERE ROWID(b-itemfg) EQ ROWID(itemfg).    

    ASSIGN
     b-itemfg.case-count = INT(itemfg.case-count:SCREEN-VALUE IN BROWSE {&browse-name}).

    FIND b-itemfg WHERE ROWID(b-itemfg) EQ ROWID(itemfg) NO-LOCK NO-ERROR.
    /*
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */

    RUN set-read-only (YES).

    RUN repo-query (ROWID(itemfg)).
      APPLY 'return' TO fi_i-no.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-dscr2 B-table-Win 
PROCEDURE valid-part-dscr2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
  DEF BUFFER b-cust FOR cust.

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
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-frt-class B-table-Win 
PROCEDURE valid-frt-class :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-mat-type B-table-Win 
PROCEDURE set-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipr-parent AS HANDLE.

  v-parent = ipr-parent.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag B-table-Win 
PROCEDURE valid-count :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i-cases AS INT NO-UNDO.

  DO WITH FRAME f-main:
    i-cases = INTEGER(itemfg.case-count:SCREEN-VALUE IN BROWSE {&browse-name}).
    IF i-cases LT 1 THEN DO:
        MESSAGE "Count cannot be less than 1..." VIEW-AS ALERT-BOX ERROR.
          itemfg.case-count:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.case-count, "->>>,>>>").
          APPLY "entry" TO itemfg.case-count IN BROWSE {&browse-name}.
          RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-pallet-info B-table-Win 
FUNCTION get-pallet-info RETURNS INTEGER
  (OUTPUT op-case-count-pal AS INT):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


RETURN li-pallets.

END FUNCTION.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-last-rec-unit B-table-Win 
FUNCTION get-last-rec-unit RETURNS INTEGER
  ( ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
        FOR EACH fg-rcpth NO-LOCK
            WHERE fg-rcpth.company      EQ itemfg.company
              AND fg-rcpth.i-no         EQ itemfg.i-no
              AND fg-rcpth.rita-code    EQ "R",

            EACH fg-rdtlh NO-LOCK
            WHERE fg-rdtlh.r-no         EQ fg-rcpth.r-no
/*              AND fg-rdtlh.loc          EQ tt-fg-bin.loc
              AND fg-rdtlh.loc-bin      EQ tt-fg-bin.loc-bin
              AND fg-rdtlh.tag          EQ tt-fg-bin.tag
              AND fg-rdtlh.cust-no      EQ tt-fg-bin.cust-no
              AND fg-rdtlh.rita-code    EQ fg-rcpth.rita-code */
            USE-INDEX rm-rdtl

            BREAK BY fg-rcpth.trans-date DESCENDING
                  BY fg-rdtlh.trans-time
                  BY fg-rcpth.r-no:
            v-last-receipt-units = fg-rdtlh.cases.

        END.
RETURN v-last-receipt-units.

END FUNCTION.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-last-rec-qty-unit B-table-Win 
FUNCTION get-last-rec-qty-unit RETURNS INTEGER
  ( ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    FOR EACH fg-rcpth NO-LOCK
        WHERE fg-rcpth.company      EQ itemfg.company
          AND fg-rcpth.i-no         EQ itemfg.i-no
          AND fg-rcpth.rita-code    EQ "R",

        EACH fg-rdtlh NO-LOCK
        WHERE fg-rdtlh.r-no         EQ fg-rcpth.r-no
/*              AND fg-rdtlh.loc          EQ tt-fg-bin.loc
          AND fg-rdtlh.loc-bin      EQ tt-fg-bin.loc-bin
          AND fg-rdtlh.tag          EQ tt-fg-bin.tag
          AND fg-rdtlh.cust-no      EQ tt-fg-bin.cust-no
          AND fg-rdtlh.rita-code    EQ fg-rcpth.rita-code */
        USE-INDEX rm-rdtl

        BREAK BY fg-rcpth.trans-date DESCENDING
              BY fg-rdtlh.trans-time
              BY fg-rcpth.r-no:
        v-last-receipt-unit-qty = fg-rdtlh.qty-case.

    END.


RETURN v-last-receipt-unit-qty.

END FUNCTION.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
