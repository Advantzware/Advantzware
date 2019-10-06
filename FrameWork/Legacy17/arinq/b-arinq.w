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

&SCOPED-DEFINE key-phrase ar-invl.company EQ cocode AND ar-invl.posted EQ YES

&SCOPED-DEFINE for-each1                            ~
    FOR EACH ar-invl                                ~
        WHERE {&key-phrase}                         ~
          AND ar-invl.cust-no   BEGINS fi_cust-no   ~
          AND ar-invl.i-no      BEGINS fi_i-no      ~
          AND ar-invl.est-no    BEGINS fi_est-no    ~
          AND ar-invl.part-no   BEGINS fi_part-no   ~
          AND ar-invl.po-no     BEGINS fi_po-no     ~
          AND ar-invl.actnum    BEGINS fi_actnum    ~
          AND (ar-invl.inv-no   EQ     fi_inv-no OR fi_inv-no EQ 0) ~
          AND (ar-invl.bol-no   EQ     fi_bol-no OR fi_bol-no EQ 0) ~
          AND (ar-invl.ord-no   EQ     fi_ord-no OR fi_ord-no EQ 0)

&SCOPED-DEFINE for-each11                           ~
    FOR EACH ar-invl                                ~
        WHERE {&key-phrase}

&SCOPED-DEFINE for-each2                     ~
    FIRST ar-inv                             ~
    WHERE ar-inv.x-no EQ ar-invl.x-no        ~
      AND (ar-inv.inv-date GE fi_date OR fi_date = ?)        ~
      AND ((ar-inv.due NE 0 AND tb_open)      ~
       OR  (ar-inv.due EQ 0 AND NOT tb_open)  ~
       OR  (ar-inv.due EQ 0 AND tb_paid)      ~
       OR  (ar-inv.due NE 0 AND NOT tb_paid)) ~
    NO-LOCK,                                  ~
    FIRST cust OF ar-inv NO-LOCK

&SCOPED-DEFINE sortby-log                                                                                                                                ~
    IF lv-sort-by EQ "ord-no"  THEN STRING(ar-invl.ord-no,"9999999999")                                                                             ELSE ~
    IF lv-sort-by EQ "actnum"  THEN ar-invl.actnum                                                                                                  ELSE ~
    IF lv-sort-by EQ "cust-no" THEN ar-invl.cust-no                                                                                                 ELSE ~
    IF lv-sort-by EQ "i-no"    THEN ar-invl.i-no                                                                                                    ELSE ~
    IF lv-sort-by EQ "est-no"  THEN ar-invl.est-no                                                                                                  ELSE ~
    IF lv-sort-by EQ "inv-no"  THEN STRING(ar-invl.inv-no,"9999999999")                                                                             ELSE ~
    IF lv-sort-by EQ "bol-no"  THEN STRING(ar-invl.bol-no,"9999999999")                                                                             ELSE ~
    IF lv-sort-by EQ "po-no"   THEN ar-invl.po-no                                                                                                   ELSE ~
    IF lv-sort-by EQ "part-no" THEN ar-invl.part-no                                                                                                 ELSE ~
                                    STRING(YEAR(ar-inv.inv-date),"9999") + STRING(MONTH(ar-inv.inv-date),"99") + STRING(DAY(ar-inv.inv-date),"99")

&SCOPED-DEFINE sortby BY ar-invl.inv-no BY ar-invl.bol-no DESC BY ar-invl.i-no BY ar-invl.line

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

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
ar-invl.ord-no ar-invl.po-no ar-invl.est-no ar-invl.i-name 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ar-invl.inv-no ~
ar-invl.bol-no ar-invl.cust-no ar-inv.inv-date ar-invl.actnum ar-invl.i-no ~
ar-invl.part-no ar-invl.ord-no ar-invl.po-no ar-invl.est-no 
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
fi_cust-no fi_date fi_actnum fi_i-no fi_part-no fi_ord-no fi_po-no ~
fi_bol-no fi_est-no btn_go btn_show RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_sortBy tb_open tb_paid fi_inv-no ~
fi_cust-no fi_date fi_actnum fi_i-no fi_part-no fi_ord-no fi_po-no ~
fi_bol-no fi_est-no FI_moveCol 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
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

DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

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

DEFINE VARIABLE fi_sortBy AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 5.

DEFINE VARIABLE tb_open AS LOGICAL INITIAL yes 
     LABEL "Open Invoices" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE tb_paid AS LOGICAL INITIAL yes 
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
      ar-invl.inv-no COLUMN-LABEL "Inv#" FORMAT ">>>>>>>>":U WIDTH 9
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
      ar-invl.part-no COLUMN-LABEL "Cust Part#" FORMAT "x(15)":U
            WIDTH 20 LABEL-BGCOLOR 14
      ar-invl.ord-no FORMAT ">>>>>>>>":U WIDTH 9 LABEL-BGCOLOR 14
      ar-invl.po-no COLUMN-LABEL "Cust PO#" FORMAT "x(15)":U WIDTH 20
            LABEL-BGCOLOR 14
      ar-invl.est-no COLUMN-LABEL "Est#" FORMAT "x(8)":U WIDTH 12
            LABEL-BGCOLOR 14
      ar-invl.i-name FORMAT "x(30)":U LABEL-BGCOLOR 14
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
     fi_date AT ROW 1.24 COL 87 COLON-ALIGNED
     fi_actnum AT ROW 1.24 COL 114 COLON-ALIGNED
     fi_i-no AT ROW 2.43 COL 55 COLON-ALIGNED
     fi_part-no AT ROW 2.43 COL 87 COLON-ALIGNED
     fi_ord-no AT ROW 2.43 COL 132 COLON-ALIGNED
     fi_po-no AT ROW 3.62 COL 55 COLON-ALIGNED
     fi_bol-no AT ROW 3.62 COL 87 COLON-ALIGNED
     fi_est-no AT ROW 3.62 COL 132 COLON-ALIGNED
     btn_go AT ROW 4.81 COL 17
     btn_show AT ROW 4.81 COL 32
     FI_moveCol AT ROW 4.76 COL 135.8 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     "Browser Col. Mode:" VIEW-AS TEXT
          SIZE 22.6 BY .62 AT ROW 5 COL 114.6 WIDGET-ID 6
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

/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */
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
"ar-invl.inv-no" "Inv#" ">>>>>>>>" "integer" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
"ar-invl.ord-no" ? ">>>>>>>>" "integer" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.ar-invl.po-no
"ar-invl.po-no" "Cust PO#" ? "character" ? ? ? 14 ? ? yes ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.ar-invl.est-no
"ar-invl.est-no" "Est#" "x(8)" "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.ar-invl.i-name
"ar-invl.i-name" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
                             'begin_inv,end_inv,begin_cust,end_cust,begin_date,end_date,tb_reprint,tb_posted',
                             STRING(ar-inv.inv-no) + ',' + STRING(ar-inv.inv-no) + ',' +
                             ar-inv.cust-no + ',' + ar-inv.cust-no + ',' +
                             STRING(ar-inv.inv-date) + ',' + STRING(ar-inv.inv-date) + ',' +
                             STRING(ar-inv.printed) + ',' + STRING(ar-inv.posted)).
  END.
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
     fi_date
     ll-first = NO.
     IF fi_cust-no EQ "" AND fi_i-no EQ "" AND fi_po-no EQ "" AND
        fi_inv-no:SCREEN-VALUE EQ "" AND fi_ord-no:SCREEN-VALUE EQ "" AND fi_bol-no:SCREEN-VALUE EQ "" AND 
        fi_est-no:SCREEN-VALUE EQ "" AND fi_actnum:SCREEN-VALUE EQ "" AND fi_part-no EQ "" AND
        fi_date:SCREEN-VALUE  EQ "" THEN ll-first = YES.
    RUN dispatch ('open-query').
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
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
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
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1. /* added by script _caps.p */
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

&SCOPED-DEFINE cellColumnDat arinqb-arinq

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE first-query B-table-Win 
PROCEDURE first-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-ar-inv FOR ar-inv.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-x-no LIKE ar-invl.x-no NO-UNDO.

  RUN set-defaults.

  {&for-each11}
      USE-INDEX x-no NO-LOCK,
      {&for-each2}
      BREAK BY ar-invl.x-no DESC:
    IF FIRST-OF(ar-invl.x-no) THEN li = li + 1.
    lv-x-no = ar-invl.x-no.
    IF li GE 30 THEN LEAVE.
  END.


  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each11}                          ~
              AND ar-invl.x-no GE lv-x-no ~
            USE-INDEX x-no NO-LOCK,         ~
            {&for-each2}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.

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
   .

  FI_moveCol = "Sort".
  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.  

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

  IF ll-first THEN RUN first-query.
  ELSE DO:
    {arinq/j-arinq.i}
  END.

  ll-first = NO.

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
         fi_date = ?.

  DISPLAY fi_inv-no fi_cust-no fi_i-no fi_po-no
          fi_part-no fi_bol-no fi_est-no fi_ord-no fi_actnum fi_date
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

 RUN arinq/rd-invexp.w (CustFrom,invFrom,itemFrom,bolFrom,poFrom,CustTo,invTo,itemTo,bolTo,poTo) .

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

