&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

def input parameter ip-company like itemfg.company no-undo.
DEF INPUT PARAM ip-cust-no LIKE ar-inv.cust-no NO-UNDO.
def input parameter ip-cur-val as cha no-undo.
def output parameter op-char-val as cha no-undo. /* string i-code + i-name */
def output param op-rec-val as recid no-undo.
def var lv-type-dscr as cha no-undo.

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
          AND ar-invl.cust-no   = fi_cust-no   ~
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
      AND ar-inv.posted ~
      AND (ar-inv.inv-date GE fi_date OR fi_date = ?)  ~
      AND (ar-inv.due GE fi_due)  ~
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
    IF lv-sort-by EQ "due"  THEN STRING(ar-inv.due,"->>,>>>,>>9.99")                                                                             ELSE ~
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

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ar-invl ar-inv cust

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 ar-invl.inv-no ar-invl.bol-no ~
ar-invl.cust-no ar-inv.inv-date ar-inv.net ar-inv.paid ar-inv.due ~
ar-invl.actnum ar-invl.i-no ar-invl.part-no ar-invl.ord-no ar-invl.po-no ~
ar-invl.est-no ar-invl.i-name 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 ar-invl.inv-no ~
ar-invl.bol-no ar-invl.cust-no ar-inv.inv-date ar-inv.net ar-inv.paid ~
ar-inv.due ar-invl.actnum ar-invl.i-no ar-invl.part-no ar-invl.ord-no ~
ar-invl.po-no ar-invl.est-no 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-3 ar-invl ar-inv
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-3 ar-invl
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-BROWSE-3 ar-inv
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH ar-invl WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH ar-inv WHERE ar-inv.x-no = ar-invl.x-no ~
      AND ar-inv.posted NO-LOCK, ~
      EACH cust OF ar-invl NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY BROWSE-3 FOR EACH ar-invl WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH ar-inv WHERE ar-inv.x-no = ar-invl.x-no ~
      AND ar-inv.posted NO-LOCK, ~
      EACH cust OF ar-invl NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 ar-invl ar-inv cust
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 ar-invl
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-3 ar-inv
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-3 cust


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_inv-no fi_date fi_actnum fi_due fi_i-no ~
fi_part-no fi_ord-no fi_po-no fi_bol-no fi_est-no btn_go BROWSE-3 bt-ok ~
bt-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_inv-no fi_cust-no fi_date fi_actnum ~
fi_due fi_i-no fi_part-no fi_ord-no fi_po-no fi_bol-no fi_est-no fi_sortBy ~
FI_moveCol 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 17 BY 1.14.

DEFINE BUTTON bt-ok 
     LABEL "&OK" 
     SIZE 17 BY 1.14.

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

DEFINE VARIABLE fi_due AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Balance Due" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      ar-invl, 
      ar-inv, 
      cust SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 Dialog-Frame _STRUCTURED
  QUERY BROWSE-3 NO-LOCK DISPLAY
      ar-invl.inv-no COLUMN-LABEL "Inv#" FORMAT ">>>>>9":U
      ar-invl.bol-no COLUMN-LABEL "BOL#" FORMAT ">>>>>>>9":U
      ar-invl.cust-no COLUMN-LABEL "Cust#" FORMAT "x(8)":U
      ar-inv.inv-date COLUMN-LABEL "Inv Date" FORMAT "99/99/9999":U
      ar-inv.net COLUMN-LABEL "Net" FORMAT "->>,>>>,>>9.99":U
      ar-inv.paid FORMAT "->,>>>,>>9.99":U
      ar-inv.due FORMAT "->>,>>>,>>9.99":U
      ar-invl.actnum COLUMN-LABEL "GL Acct#" FORMAT "x(25)":U
      ar-invl.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U
      ar-invl.part-no FORMAT "x(15)":U
      ar-invl.ord-no FORMAT ">>>>>9":U
      ar-invl.po-no COLUMN-LABEL "Cust PO#" FORMAT "x(15)":U
      ar-invl.est-no COLUMN-LABEL "Est#" FORMAT "x(5)":U
      ar-invl.i-name FORMAT "x(30)":U
  ENABLE
      ar-invl.inv-no
      ar-invl.bol-no
      ar-invl.cust-no
      ar-inv.inv-date
      ar-inv.net
      ar-inv.paid
      ar-inv.due
      ar-invl.actnum
      ar-invl.i-no
      ar-invl.part-no
      ar-invl.ord-no
      ar-invl.po-no
      ar-invl.est-no
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 171 BY 16.19
         BGCOLOR 8 FONT 2 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fi_inv-no AT ROW 1.24 COL 11 COLON-ALIGNED WIDGET-ID 18
     fi_cust-no AT ROW 1.24 COL 39 COLON-ALIGNED WIDGET-ID 10
     fi_date AT ROW 1.24 COL 65 COLON-ALIGNED WIDGET-ID 12
     fi_actnum AT ROW 1.24 COL 94 COLON-ALIGNED WIDGET-ID 6
     fi_due AT ROW 1.24 COL 147 COLON-ALIGNED WIDGET-ID 36
     fi_i-no AT ROW 2.43 COL 39 COLON-ALIGNED WIDGET-ID 16
     fi_part-no AT ROW 2.43 COL 71 COLON-ALIGNED WIDGET-ID 22
     fi_ord-no AT ROW 2.43 COL 112 COLON-ALIGNED WIDGET-ID 20
     fi_po-no AT ROW 3.62 COL 39 COLON-ALIGNED WIDGET-ID 24
     fi_bol-no AT ROW 3.62 COL 71 COLON-ALIGNED WIDGET-ID 8
     fi_est-no AT ROW 3.62 COL 112 COLON-ALIGNED WIDGET-ID 14
     btn_go AT ROW 5.29 COL 5 WIDGET-ID 28
     btn_show AT ROW 5.29 COL 20 WIDGET-ID 30
     fi_sortBy AT ROW 5.29 COL 55.2 COLON-ALIGNED WIDGET-ID 26
     FI_moveCol AT ROW 5.29 COL 135.8 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     BROWSE-3 AT ROW 6.71 COL 3 WIDGET-ID 200
     bt-ok AT ROW 23.62 COL 109 WIDGET-ID 34
     bt-cancel AT ROW 23.62 COL 135 WIDGET-ID 32
     SPACE(23.39) SKIP(0.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Invoice Information detail" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-3 FI_moveCol Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BROWSE-3:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR BUTTON btn_show IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       btn_show:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN fi_cust-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sortBy IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _TblList          = "asi.ar-invl,asi.ar-inv WHERE asi.ar-invl ...,asi.cust OF asi.ar-invl"
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ",,"
     _JoinCode[2]      = "asi.ar-inv.x-no = asi.ar-invl.x-no"
     _Where[2]         = "asi.ar-inv.posted"
     _FldNameList[1]   > asi.ar-invl.inv-no
"ar-invl.inv-no" "Inv#" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.ar-invl.bol-no
"ar-invl.bol-no" "BOL#" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.ar-invl.cust-no
"ar-invl.cust-no" "Cust#" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.ar-inv.inv-date
"ar-inv.inv-date" "Inv Date" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.ar-inv.net
"ar-inv.net" "Net" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.ar-inv.paid
"ar-inv.paid" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.ar-inv.due
"ar-inv.due" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.ar-invl.actnum
"ar-invl.actnum" "GL Acct#" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.ar-invl.i-no
"ar-invl.i-no" "FG Item#" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.ar-invl.part-no
"ar-invl.part-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.ar-invl.ord-no
"ar-invl.ord-no" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.ar-invl.po-no
"ar-invl.po-no" "Cust PO#" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.ar-invl.est-no
"ar-invl.est-no" "Est#" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   = asi.ar-invl.i-name
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Invoice Information detail */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 Dialog-Frame
ON START-SEARCH OF BROWSE-3 IN FRAME Dialog-Frame
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


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   op-char-val = ar-invl.inv-no:screen-value in browse {&browse-name}.
   op-rec-val = recid(ar-inv).              
   apply "window-close" to frame {&frame-name}. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go Dialog-Frame
ON CHOOSE OF btn_go IN FRAME Dialog-Frame /* Go */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN

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
     fi_due
     ll-first = NO.

    RUN OpenQuery.
    fi_sortby:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
                             TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".

    APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show Dialog-Frame
ON CHOOSE OF btn_show IN FRAME Dialog-Frame /* Show All */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN

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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_actnum Dialog-Frame
ON LEAVE OF fi_actnum IN FRAME Dialog-Frame /* GL Acct# */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no Dialog-Frame
ON LEAVE OF fi_cust-no IN FRAME Dialog-Frame /* Customer# */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no Dialog-Frame
ON VALUE-CHANGED OF fi_cust-no IN FRAME Dialog-Frame /* Customer# */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1 + IF LASTKEY EQ 32 THEN 1 ELSE 0. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_date Dialog-Frame
ON LEAVE OF fi_date IN FRAME Dialog-Frame /* Inv Date */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_due
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_due Dialog-Frame
ON LEAVE OF fi_due IN FRAME Dialog-Frame /* Balance Due */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_est-no Dialog-Frame
ON LEAVE OF fi_est-no IN FRAME Dialog-Frame /* Est# */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no Dialog-Frame
ON VALUE-CHANGED OF fi_i-no IN FRAME Dialog-Frame /* FG Item# */
DO:
  /*IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).*/
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1 + IF LASTKEY EQ 32 THEN 1 ELSE 0. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_inv-no Dialog-Frame
ON LEAVE OF fi_inv-no IN FRAME Dialog-Frame /* Invoice# */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  fi_cust-no = ip-cust-no.
  RUN enable_UI.
  RUN Initproc.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY fi_inv-no fi_cust-no fi_date fi_actnum fi_due fi_i-no fi_part-no 
          fi_ord-no fi_po-no fi_bol-no fi_est-no fi_sortBy FI_moveCol 
      WITH FRAME Dialog-Frame.
  ENABLE fi_inv-no fi_date fi_actnum fi_due fi_i-no fi_part-no fi_ord-no 
         fi_po-no fi_bol-no fi_est-no btn_go BROWSE-3 bt-ok bt-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE first-query Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitProc Dialog-Frame 
PROCEDURE InitProc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*RUN setCellColumns.*/

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN 
   ar-invl.inv-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.bol-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.i-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.est-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.ord-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES 
   ar-inv.inv-date:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-inv.net:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-inv.paid:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-inv.due:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.actnum:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.part-no:READ-ONLY IN BROWSE {&browse-name} = YES
   ar-invl.po-no:READ-ONLY IN BROWSE {&browse-name} = YES
   .

   APPLY 'choose' TO btn_go IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery Dialog-Frame 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF ll-first THEN RUN first-query.
  ELSE DO:
    /*{arinq/j-arinq.i}*/

      IF fi_est-no NE "" THEN fi_est-no = FILL(" ",8 - LENGTH(TRIM(fi_est-no))) + TRIM(fi_est-no).

      IF fi_inv-no NE 0 THEN DO:
        &SCOPED-DEFINE open-query                   ~
            OPEN QUERY {&browse-name}               ~
              {&for-each1}                          ~
                    AND ar-invl.inv-no EQ fi_inv-no ~
                  USE-INDEX inv-no-desc NO-LOCK,    ~
                  {&for-each2}

        IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                       ELSE {&open-query} {&sortby-phrase-desc}. 
      END.

      ELSE
      IF fi_bol-no NE 0 THEN DO:
        &SCOPED-DEFINE open-query                   ~
            OPEN QUERY {&browse-name}               ~
              {&for-each1}                          ~
                    AND ar-invl.bol-no EQ fi_bol-no ~
                  USE-INDEX bol-no NO-LOCK,         ~
                  {&for-each2}

        IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                       ELSE {&open-query} {&sortby-phrase-desc}. 
      END.

      ELSE
      IF fi_ord-no NE 0 THEN DO:
        &SCOPED-DEFINE open-query                   ~
            OPEN QUERY {&browse-name}               ~
              {&for-each1}                          ~
                    AND ar-invl.ord-no EQ fi_ord-no ~
                  USE-INDEX ord-no NO-LOCK,         ~
                  {&for-each2}

        IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                       ELSE {&open-query} {&sortby-phrase-desc}. 
      END.

      ELSE
      IF fi_est-no NE "" THEN DO:
        &SCOPED-DEFINE open-query             ~
            OPEN QUERY {&browse-name}         ~
                {&for-each1}                  ~
                    USE-INDEX est-no NO-LOCK, ~
                    {&for-each2}

        IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                       ELSE {&open-query} {&sortby-phrase-desc}.
      END.

      ELSE
      IF fi_po-no NE "" THEN DO:
        &SCOPED-DEFINE open-query              ~
            OPEN QUERY {&browse-name}          ~
                {&for-each1}                   ~
                    USE-INDEX cust-po NO-LOCK, ~
                    {&for-each2}

        IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                       ELSE {&open-query} {&sortby-phrase-desc}.
      END.

      ELSE
      IF fi_part-no NE "" THEN DO:
        &SCOPED-DEFINE open-query              ~
            OPEN QUERY {&browse-name}          ~
                {&for-each1}                   ~
                    USE-INDEX part-no NO-LOCK, ~
                    {&for-each2}

        IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                       ELSE {&open-query} {&sortby-phrase-desc}.
      END.

      ELSE
      IF fi_i-no NE "" THEN DO:
        &SCOPED-DEFINE open-query           ~
            OPEN QUERY {&browse-name}       ~
                {&for-each1}                ~
                    USE-INDEX i-no NO-LOCK, ~
                    {&for-each2}

        IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                       ELSE {&open-query} {&sortby-phrase-desc}.
      END.

      ELSE
      IF fi_cust-no NE "" THEN DO:
        &SCOPED-DEFINE open-query             ~
            OPEN QUERY {&browse-name}         ~
                {&for-each1}                  ~
                    USE-INDEX inv-no NO-LOCK, ~
                    {&for-each2}

        IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                       ELSE {&open-query} {&sortby-phrase-desc}.
      END.

      ELSE
      IF fi_actnum NE "" THEN DO:
        &SCOPED-DEFINE open-query             ~
            OPEN QUERY {&browse-name}         ~
                {&for-each1}                  ~
                    USE-INDEX actnum NO-LOCK, ~
                    {&for-each2}

        IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                       ELSE {&open-query} {&sortby-phrase-desc}.
      END.
      ELSE DO:

        &SCOPED-DEFINE open-query             ~
            OPEN QUERY {&browse-name}         ~
                {&for-each11}                 ~
                    USE-INDEX posted NO-LOCK, ~
                    {&for-each2}

        IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                       ELSE {&open-query} {&sortby-phrase-desc}.
      END.



      IF AVAIL ar-invl THEN
        ASSIGN
         lv-last-rowid  = ROWID(ar-invl)
         lv-last-rowid2 = ROWID(ar-inv).


      IF AVAIL ar-invl THEN
        ASSIGN
         lv-frst-rowid  = ROWID(ar-invl)
         lv-frst-rowid2 = ROWID(ar-inv).

  END.

  ll-first = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-defaults Dialog-Frame 
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
         fi_date = ?
         fi_due = 0.

  DISPLAY fi_inv-no fi_cust-no fi_i-no fi_po-no
          fi_part-no fi_bol-no fi_est-no fi_ord-no fi_actnum fi_date fi_due
          WITH FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

