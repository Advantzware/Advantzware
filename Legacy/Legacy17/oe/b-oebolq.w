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

  File:  oe\b-oebolq.w

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

/*{sys/inc/oeinq.i} */

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "bol-no" /*"req-date" */ NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "BOL#" /*"Due Date"*/  NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR phandle AS HANDLE NO-UNDO.
DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR lv-release AS CHAR NO-UNDO.
DEF VAR v-col-move AS LOG INIT TRUE NO-UNDO.

ll-sort-asc = NO /*oeinq*/  .

&SCOPED-DEFINE key-phrase oe-bolh.company EQ cocode

&SCOPED-DEFINE for-each1                                       ~
     FOR EACH ASI.oe-boll  WHERE oe-boll.company EQ g_company  ~
                             AND oe-boll.i-no BEGINS fi_i-no   ~
                             AND oe-boll.po-no BEGINS fi_po-no ~
                             AND oe-boll.posted EQ tb_posted

&SCOPED-DEFINE for-each2    ~
     FIRST ASI.oe-bolh NO-LOCK WHERE oe-bolh.company EQ oe-boll.company ~
                                AND oe-bolh.b-no EQ oe-boll.b-no       ~
                                AND oe-bolh.deleted EQ NO              ~
                                AND oe-bolh.cust-no BEGINS fi_cust-no ~
                                AND oe-bolh.posted EQ tb_posted  ~
                      USE-INDEX b-no

&SCOPED-DEFINE for-each3                                             ~
     EACH ASI.itemfg NO-LOCK WHERE itemfg.company EQ oe-boll.company ~
                               AND itemfg.i-no    EQ oe-boll.i-no    ~
                               AND itemfg.part-no BEGINS fi_part-no  ~
                               AND itemfg.i-name  BEGINS fi_i-name

&SCOPED-DEFINE for-each11                                    ~
     FOR EACH ASI.oe-boll  WHERE oe-boll.company = g_company ~
                             AND oe-boll.posted EQ tb_posted

&SCOPED-DEFINE for-each21    ~
     FIRST ASI.oe-bolh NO-LOCK WHERE oe-bolh.company EQ oe-boll.company ~
                                AND oe-bolh.b-no EQ oe-boll.b-no       ~
                                AND oe-bolh.deleted EQ NO              ~
                                AND oe-bolh.posted EQ tb_posted  ~
                      USE-INDEX b-no

&SCOPED-DEFINE for-each31                                            ~
     EACH ASI.itemfg NO-LOCK WHERE itemfg.company EQ oe-boll.company ~
                               AND itemfg.i-no    EQ oe-boll.i-no   ~
                               AND itemfg.part-no BEGINS fi_part-no

&SCOPED-DEFINE sortby-log                                                     ~
    IF lv-sort-by EQ "ord-no"   THEN STRING(oe-boll.ord-no,"9999999999") ELSE ~
    IF lv-sort-by EQ "bol-no"   THEN string(oe-bolh.bol-no,"9999999999") ELSE ~
    IF lv-sort-by EQ "cust-no"  THEN oe-bolh.cust-no                     ELSE ~
    IF lv-sort-by EQ "part-no"  THEN itemfg.part-no                      ELSE ~
    IF lv-sort-by EQ "i-no"     THEN oe-boll.i-no                        ELSE ~
    IF lv-sort-by EQ "po-no"    THEN oe-boll.po-no                       ELSE ~
    IF lv-sort-by EQ "ship-id"  THEN oe-bolh.ship-id                     ELSE ~
    IF lv-sort-by EQ "i-name"   THEN itemfg.i-name                       ELSE ~
    IF lv-sort-by EQ "bol-date" THEN STRING(YEAR(oe-boll.bol-date),'9999') + ~
        STRING(MONTH(oe-boll.bol-date),'99') + ~
        STRING(DAY(oe-boll.bol-date),'99')          ELSE ~
    IF lv-sort-by EQ "release#" THEN string(oe-bolh.release#,"9999999999")  ELSE ~
        STRING(oe-bolh.bol-no)

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc  ~
    BY ({&sortby-log}) DESC        ~
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
&Scoped-define INTERNAL-TABLES oe-boll oe-bolh itemfg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table oe-bolh.bol-no oe-boll.ord-no ~
oe-boll.po-no oe-bolh.cust-no itemfg.part-no oe-boll.i-no itemfg.i-name ~
oe-bolh.ship-id oe-boll.bol-date get-release() @ lv-release 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table oe-bolh.bol-no ~
oe-boll.ord-no oe-boll.po-no oe-bolh.cust-no oe-boll.i-no itemfg.i-name ~
oe-bolh.ship-id 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table oe-bolh oe-boll itemfg
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table oe-bolh
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table oe-boll
&Scoped-define THIRD-ENABLED-TABLE-IN-QUERY-Browser-Table itemfg
&Scoped-define QUERY-STRING-Browser-Table FOR EACH oe-boll WHERE ~{&KEY-PHRASE} ~
      AND oe-boll.b-no = 9000000 NO-LOCK, ~
      EACH oe-bolh OF oe-boll NO-LOCK, ~
      EACH itemfg OF oe-boll NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH oe-boll WHERE ~{&KEY-PHRASE} ~
      AND oe-boll.b-no = 9000000 NO-LOCK, ~
      EACH oe-bolh OF oe-boll NO-LOCK, ~
      EACH itemfg OF oe-boll NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table oe-boll oe-bolh itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table oe-boll
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table oe-bolh
&Scoped-define THIRD-TABLE-IN-QUERY-Browser-Table itemfg


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_bol-no fi_ord-no fi_i-no fi_po-no ~
fi_cust-no fi_i-name tb_posted btn_go btn_show Browser-Table fi_part-no ~
RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_bol-no fi_ord-no fi_i-no fi_po-no ~
fi_cust-no fi_i-name tb_posted fi_sort-by FI_moveCol fi_part-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-release B-table-Win 
FUNCTION get-release RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1
     FONT 6.

DEFINE BUTTON btn_show 
     LABEL "&Show All" 
     SIZE 12 BY 1
     FONT 6.

DEFINE VARIABLE fi_bol-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_cust-no AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-name AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_part-no AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_po-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 3.57.

DEFINE VARIABLE tb_posted AS LOGICAL INITIAL no 
     LABEL "Posted" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1
     FGCOLOR 12 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      oe-boll, 
      oe-bolh, 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      oe-bolh.bol-no COLUMN-LABEL "BOL#" FORMAT ">>>>>>>9":U LABEL-BGCOLOR 14
      oe-boll.ord-no FORMAT ">>>>>9":U LABEL-BGCOLOR 14
      oe-boll.po-no COLUMN-LABEL "Customer's PO" FORMAT "x(15)":U
            LABEL-BGCOLOR 14
      oe-bolh.cust-no FORMAT "x(8)":U LABEL-BGCOLOR 14
      itemfg.part-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-boll.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U LABEL-BGCOLOR 14
      itemfg.i-name COLUMN-LABEL "FG Item Name" FORMAT "x(15)":U
            WIDTH 22 LABEL-BGCOLOR 14
      oe-bolh.ship-id COLUMN-LABEL "Ship To" FORMAT "x(8)":U WIDTH 10
            LABEL-BGCOLOR 14
      oe-boll.bol-date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      get-release() @ lv-release COLUMN-LABEL "Release" WIDTH 10.2
  ENABLE
      oe-bolh.bol-no
      oe-boll.ord-no
      oe-boll.po-no
      oe-bolh.cust-no
      oe-boll.i-no
      itemfg.i-name
      oe-bolh.ship-id
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 146 BY 15.48
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_bol-no AT ROW 1.95 COL 5 NO-LABEL
     fi_ord-no AT ROW 1.95 COL 21 NO-LABEL
     fi_i-no AT ROW 1.95 COL 36 COLON-ALIGNED NO-LABEL
     fi_po-no AT ROW 1.95 COL 59 COLON-ALIGNED NO-LABEL
     fi_cust-no AT ROW 1.95 COL 82 COLON-ALIGNED NO-LABEL
     fi_i-name AT ROW 1.95 COL 99 COLON-ALIGNED NO-LABEL
     tb_posted AT ROW 1.71 COL 124
     btn_go AT ROW 3.14 COL 6
     btn_show AT ROW 3.14 COL 22
     fi_sort-by AT ROW 3.14 COL 76.4 COLON-ALIGNED NO-LABEL
     FI_moveCol AT ROW 3.14 COL 132.6 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     Browser-Table AT ROW 4.81 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     fi_part-no AT ROW 3.1 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     "BOL#" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 1.24 COL 6
          FGCOLOR 9 FONT 6
     "Browser Col. Mode:" VIEW-AS TEXT
          SIZE 22.6 BY .62 AT ROW 3.33 COL 111.8 WIDGET-ID 6
          FONT 6
     "Sorted By:" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 3.14 COL 66.2
          FONT 6
     "Order#" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 1.24 COL 23
          FGCOLOR 9 FONT 6
     "Customer#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 84
          FGCOLOR 9 FONT 6
     "FG Item#/Part#" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 1.24 COL 39.2
          FGCOLOR 9 FONT 6
     "Customer PO#" VIEW-AS TEXT
          SIZE 18 BY .71 AT ROW 1.24 COL 62
          FGCOLOR 9 FONT 6
     "FG Item Name" VIEW-AS TEXT
          SIZE 17 BY .71 AT ROW 1.24 COL 102
          FGCOLOR 9 FONT 6
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
         HEIGHT             = 19.48
         WIDTH              = 158.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src\adm\method\query.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB Browser-Table FI_moveCol F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_bol-no IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_ord-no IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.oe-boll,ASI.oe-bolh OF ASI.oe-boll,ASI.itemfg OF ASI.oe-boll"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ",,"
     _Where[1]         = "ASI.oe-boll.b-no = 9000000"
     _FldNameList[1]   > ASI.oe-bolh.bol-no
"oe-bolh.bol-no" "BOL#" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.oe-boll.ord-no
"oe-boll.ord-no" ? ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.oe-boll.po-no
"oe-boll.po-no" "Customer's PO" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.oe-bolh.cust-no
"oe-bolh.cust-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.itemfg.part-no
"itemfg.part-no" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.oe-boll.i-no
"oe-boll.i-no" "FG Item#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.itemfg.i-name
"itemfg.i-name" "FG Item Name" "x(15)" "character" ? ? ? 14 ? ? yes ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.oe-bolh.ship-id
"oe-bolh.ship-id" "Ship To" ? "character" ? ? ? 14 ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.oe-boll.bol-date
"oe-boll.bol-date" ? ? "date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"get-release() @ lv-release" "Release" ? ? ? ? ? ? ? ? no ? no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
  DEF VAR phandle AS HANDLE NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.


  {methods/run_link.i "container-source" "select-page" "(2)"}
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

  RUN dispatch ("open-query").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:

  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR phandle AS HANDLE NO-UNDO.
  {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
     "(oe-bolh.rec_key,{methods/headers/oe-bolh.i})"}
  {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
     "(CAN-FIND(FIRST notes WHERE notes.rec_key = oe-bolh.rec_key))"}
  {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
     "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = oe-bolh.rec_key))"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN   
     fi_bol-no
     tb_posted
     fi_i-no
     fi_cust-no
     fi_ord-no
     fi_po-no
     fi_i-name
     fi_part-no.                /*Task# 01241405 */

    ll-first = NO.

    RUN dispatch ("open-query").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show B-table-Win
ON CHOOSE OF btn_show IN FRAME F-Main /* Show All */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    RUN set-defaults.

    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_bol-no B-table-Win
ON HELP OF fi_bol-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR rec-val AS RECID NO-UNDO.
   ASSIGN tb_posted.
   RUN windows/l-bolh2.w (g_company,tb_posted,SELF:SCREEN-VALUE,OUTPUT char-val,OUTPUT rec-val).
   IF char-val <> "" THEN
      ASSIGN fi_bol-no:SCREEN-VALUE = ENTRY(1,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON VALUE-CHANGED OF fi_cust-no IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-name B-table-Win
ON VALUE-CHANGED OF fi_i-name IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON LEAVE OF fi_i-no IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON VALUE-CHANGED OF fi_i-no IN FRAME F-Main
DO:
  /* This was causing field not to accept spaces in the item number */
  /* {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE). */
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no B-table-Win
ON VALUE-CHANGED OF fi_part-no IN FRAME F-Main
DO:
 IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_po-no B-table-Win
ON VALUE-CHANGED OF fi_po-no IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

&SCOPED-DEFINE cellColumnDat oeb-oebolq

{methods/browsers/setCellColumns.i}

SESSION:DATA-ENTRY-RETURN = YES.

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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE browse-rowid B-table-Win 
PROCEDURE browse-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.


  IF AVAIL {&FIRST-TABLE-IN-QUERY-{&browse-name}} THEN
    op-rowid = ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}}).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-header B-table-Win 
PROCEDURE clear-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR browser-handle AS WIDGET-HANDLE NO-UNDO.
      DEF VAR q-stat AS LOG NO-UNDO.

  /*
  RUN dispatch ('get-next').
  */

/*
    GET NEXT {&browse-NAME}. 

    IF AVAIL({&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}) THEN
    DO:
        IF adm-last-rowid = ROWID({&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}})
             THEN RUN new-state ('last-record,SELF':U).
        ELSE RUN new-state ('not-first-or-last,SELF':U).
        RUN dispatch IN THIS-PROCEDURE ('row-changed':U).
    END.
    ELSE 
        RUN dispatch IN THIS-PROCEDURE ('get-last':U).  
*/
       browser-handle = {&BROWSE-NAME}:HANDLE IN FRAME {&FRAME-NAME}.
       q-stat = browser-handle:SELECT-NEXT-ROW(). 
       IF NOT q-stat THEN browser-handle:SELECT-PREV-ROW(). 
       RUN dispatch IN THIS-PROCEDURE ('row-changed':U).

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

  MESSAGE 'Disable-Navigation'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  {methods/run_link.i "NAVIGATION-SOURCE" "dispatch" "('disable':U) NO-ERROR"}

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

  {methods/run_link.i "NAVIGATION-SOURCE" "dispatch" "('enable':U) NO-ERROR"}

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
  DEF BUFFER b-oe-ord FOR oe-ord.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-b-no LIKE oe-bolh.b-no NO-UNDO.

  RUN set-defaults.

  li = 0.

  FOR EACH oe-bolh
      WHERE oe-bolh.company EQ g_company
        AND oe-bolh.posted  EQ tb_posted
        AND oe-bolh.deleted EQ NO
        AND CAN-FIND(FIRST oe-boll
                     WHERE oe-boll.company EQ oe-bolh.company
                       AND oe-boll.b-no    EQ oe-bolh.b-no)
      USE-INDEX post NO-LOCK
      BREAK BY oe-bolh.b-no DESC:
    IF FIRST-OF(oe-bolh.b-no) THEN li = li + 1.
    lv-b-no = oe-bolh.b-no.
    IF li GE 100 THEN LEAVE.
  END.

  &SCOPED-DEFINE open-query               ~
      OPEN QUERY {&browse-name}           ~
        {&for-each11}                     ~
              AND oe-boll.b-no GE lv-b-no ~
            USE-INDEX posted NO-LOCK,      ~
            {&for-each21},                ~
            {&for-each31}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-line-est B-table-Win 
PROCEDURE get-line-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output parameter op-est-no as cha no-undo.


  op-est-no = if available oe-ordl then oe-ordl.est-no else oe-ord.est-no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE is-row-available B-table-Win 
PROCEDURE is-row-available :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-row-avail AS LOG NO-UNDO.

  op-row-avail =  IF AVAIL oe-bolh THEN YES ELSE NO.

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

  /* Code placed here will execute AFTER standard behavior.    */

  RUN setCellColumns.

  ASSIGN oe-bolh.bol-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-boll.ord-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-bolh.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-boll.po-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-boll.i-no:READ-ONLY IN BROWSE {&browse-name} = YES
      itemfg.i-name:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-bolh.ship-id:READ-ONLY IN BROWSE {&browse-name} = YES
      .

  FI_moveCol = "Sort".
  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.

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
  IF ll-first THEN RUN first-query.

  ELSE DO:
    {oe/j-oebolq.i}
  END.

  IF AVAIL oe-bolh THEN DO:
    RUN dispatch ("display-fields").

    RUN dispatch ("row-changed").

    APPLY "value-changed" TO BROWSE {&browse-name}.

    GET LAST {&browse-name}.
    IF AVAIL oe-bolh THEN lv-last-rowid = ROWID(oe-bolh).

    GET FIRST {&browse-name}.
    IF AVAIL oe-bolh THEN lv-frst-rowid = ROWID(oe-bolh).
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

  IF AVAIL oe-bolh THEN APPLY 'value-changed' TO BROWSE {&browse-name}.


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

  DEF VAR hld-rowid AS ROWID NO-UNDO.


  hld-rowid = ROWID(oe-bolh).

  IF ip-nav-type NE "" THEN
  CASE ip-nav-type:
    WHEN "F" THEN GET FIRST {&browse-name}.
    WHEN "L" THEN GET LAST  {&browse-name}.
    WHEN "N" THEN DO WHILE ROWID(oe-bolh) EQ hld-rowid:
                    GET NEXT {&browse-name}.
                  END.
    WHEN "P" THEN DO WHILE ROWID(oe-bolh) EQ hld-rowid:
                    GET PREV {&browse-name}.
                  END.
  END CASE.

  RUN dispatch ("display-fields").

  RUN repo-query2 (ROWID(oe-boll)).

  RUN dispatch ("row-changed").

  IF ROWID(oe-bolh) EQ lv-last-rowid THEN
    op-nav-type = "L".

  IF ROWID(oe-bolh) EQ lv-frst-rowid THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE record-added B-table-Win 
PROCEDURE record-added :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ll-first = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query B-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv-tmp-rowid AS ROWID NO-UNDO.
 lv-tmp-rowid = ROWID(oe-boll).

 RUN reopen-query1 (lv-tmp-rowid).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query1 B-table-Win 
PROCEDURE reopen-query1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  DEF BUFFER b-oe-boll FOR oe-boll.
  DEF BUFFER b-oe-bolh FOR oe-bolh.


  FIND b-oe-bolh WHERE ROWID(b-oe-bolh) EQ ip-rowid NO-LOCK NO-ERROR.
  IF AVAIL b-oe-bolh THEN DO:
    FIND FIRST b-oe-boll OF b-oe-bolh NO-LOCK.
    ip-rowid = ROWID(b-oe-boll).
  END.

  DO WITH FRAME {&FRAME-NAME}: 
    RUN dispatch ("open-query").
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    RUN dispatch ("row-changed").
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

  RUN dispatch ("open-query").
  RUN repo-query2 (ip-rowid).
  RUN dispatch ("row-changed").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query2 B-table-Win 
PROCEDURE repo-query2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-his B-table-Win 
PROCEDURE select-his :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  find first cust {sys/ref/custW.i} and
                  cust.cust-no eq oe-ord.cust-no
                  use-index cust no-lock no-error.

  def var char-hdl as cha no-undo.
  run get-link-handle in adm-broker-hdl (this-procedure,"container-source",output char-hdl).
  run init-history in widget-handle(char-hdl) (this-procedure).

  /*run dispatch ('open-query'). ?? */

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
  {src/adm/template/snd-list.i "oe-boll"}
  {src/adm/template/snd-list.i "oe-bolh"}
  {src/adm/template/snd-list.i "itemfg"}

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
     fi_cust-no:SCREEN-VALUE = ""
     fi_i-no:SCREEN-VALUE    = ""
     fi_bol-no:SCREEN-VALUE = ""
     fi_ord-no:SCREEN-VALUE  = ""
     fi_po-no:SCREEN-VALUE   = ""
     fi_i-name:SCREEN-VALUE    = ""
     fi_part-no:SCREEN-VALUE     = "".
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

  {methods/setfocus.i {&BROWSE-NAME}}

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "tab" TO {&BROWSE-NAME}.
  END.

  /* gdm - 07100905 */
  APPLY 'ENTRY':U TO fi_bol-no IN FRAME {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-release B-table-Win 
FUNCTION get-release RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR v-release AS CHAR NO-UNDO.
DEF BUFFER bf-oe-relh FOR oe-relh.
  FIND FIRST bf-oe-relh WHERE bf-oe-relh.company = cocode
                       AND bf-oe-relh.r-no = oe-boll.r-no
                     NO-LOCK NO-ERROR.
  IF AVAIL bf-oe-relh THEN
      v-release = STRING(bf-oe-relh.release#).

  RETURN v-release.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

