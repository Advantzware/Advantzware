&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  rm\b-rcptd.w

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

&SCOPED-DEFINE yellowColumnsName b-rcptd
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/gcompany.i}
{custom/gloc.i}
{custom/globdefs.i}
{sys/inc/var.i new shared}
{sys/inc/varasgn.i}

ASSIGN
    cocode = g_company
    locode = g_loc.

DEFINE VARIABLE char-val          AS cha           NO-UNDO.
DEFINE VARIABLE ext-cost          AS DECIMAL       NO-UNDO.
DEFINE VARIABLE lv-rowid          AS ROWID         NO-UNDO.
DEFINE VARIABLE ll-qty-valid      AS LOG           NO-UNDO.
DEFINE VARIABLE hd-post           AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hd-post-child     AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE ll-help-run       AS LOG           NO-UNDO.  /* set on browse help, reset row-entry */

DEFINE VARIABLE lv-po-wid         LIKE po-ordl.s-wid NO-UNDO.
DEFINE VARIABLE lv-po-len         LIKE po-ordl.s-len FORM ">>,>>9.9999" NO-UNDO.
DEFINE VARIABLE v-avgcost         AS LOG           NO-UNDO.
DEFINE VARIABLE ll-tag-meth       AS LOG           NO-UNDO.
DEFINE VARIABLE ll-warned         AS LOG           NO-UNDO.
DEFINE VARIABLE li-pos            AS INTEGER       NO-UNDO.
DEFINE VARIABLE lv-job-no         AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lv-job-no2        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lv-setup          AS DECIMAL       NO-UNDO.
DEFINE VARIABLE lv-adder          AS DECIMAL       DECIMALS 4 NO-UNDO.
DEFINE VARIABLE lv-msf            AS DECIMAL       DECIMALS 3 NO-UNDO.
DEFINE VARIABLE fg-uom-list       AS CHARACTER     NO-UNDO.
DEFINE VARIABLE ll-add-setup      AS LOG           NO-UNDO.
DEFINE VARIABLE lv-save-fld       AS CHARACTER     EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-bin             AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-copy-mode       AS LOG           NO-UNDO.
DEFINE VARIABLE v-copy-mode-dec-1 AS LOG           NO-UNDO.
DEFINE VARIABLE v-new-mode        AS LOG           NO-UNDO.
DEFINE VARIABLE lv-entry-qty      AS DECIMAL       NO-UNDO.
DEFINE VARIABLE lv-entry-qty-uom  AS CHARACTER     NO-UNDO.
DEFINE VARIABLE op-error          AS LOG           NO-UNDO.

DEFINE BUFFER b-po-ord  FOR po-ord.
DEFINE BUFFER b-company FOR company.
DEFINE BUFFER b-rm-rctd FOR rm-rctd.



DEF VAR lAllowRmAdd AS LOG NO-UNDO.
DEF VAR lcReturn   AS CHAR NO-UNDO.
DEF VAR llRecFound AS LOG  NO-UNDO.
DEFINE VARIABLE lRMOverrunCost AS LOGICAL NO-UNDO .

RUN sys/ref/nk1look.p (cocode, "RMAllowAdd", "L", NO, NO, "", "", 
    OUTPUT lcReturn, OUTPUT llRecFound).

   lAllowRmAdd = LOGICAL(lcReturn) NO-ERROR.

RUN sys/ref/nk1look.p (cocode, "RMOverrunCostProtection", "L", NO, NO, "", "", 
    OUTPUT lcReturn, OUTPUT llRecFound).

   lRMOverrunCost = LOGICAL(lcReturn) NO-ERROR.

DEFINE TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty  AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    FIELD setups   AS DECIMAL DECIMALS 2 EXTENT 20.

{windows/l-poitmw.i NEW}

{fg/d-selpos.i NEW}

DEFINE TEMP-TABLE tt-rm-rctd NO-UNDO LIKE rm-rctd
    FIELD tt-rowid AS ROWID
    FIELD po-rowid AS ROWID.


DEFINE NEW SHARED TEMP-TABLE tt-selected NO-UNDO
    FIELD tt-rowid       AS ROWID 
    FIELD tt-import-zero AS LOG
    FIELD tt-item        AS CHARACTER .

DO TRANSACTION:
    {sys/inc/rmrecpt.i}
    {sys/inc/rmunderover.i}
    {sys/inc/poholdreceipts.i}  /* ticket 17372 */
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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rm-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rctd.r-no rm-rctd.rct-date ~
rm-rctd.po-no rm-rctd.job-no rm-rctd.job-no2 rm-rctd.s-num rm-rctd.b-num ~
rm-rctd.i-no rm-rctd.i-name rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom ~
calc-ext-cost() @ ext-cost display-dimension('W') @ lv-po-wid ~
display-dimension('L') @ lv-po-len display-setup() @ lv-setup ~
display-adder() @ lv-adder display-msf() @ lv-msf rm-rctd.user-id ~
rm-rctd.tag2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.rct-date ~
rm-rctd.po-no rm-rctd.job-no rm-rctd.job-no2 rm-rctd.s-num rm-rctd.b-num ~
rm-rctd.i-no rm-rctd.i-name rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom rm-rctd.tag2 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = cocode and ~
rm-rctd.rita-code = "R" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = cocode and ~
rm-rctd.rita-code = "R" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-5 Browser-Table browse-order ~
auto_find Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order fi_sortby auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-adder B-table-Win 
FUNCTION display-adder RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-adder-screen B-table-Win 
FUNCTION display-adder-screen RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-dimension B-table-Win 
FUNCTION display-dimension RETURNS DECIMAL
    ( INPUT ip-dim AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-dimension-screen B-table-Win 
FUNCTION display-dimension-screen RETURNS DECIMAL
    ( INPUT ip-dim AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-msf B-table-Win 
FUNCTION display-msf RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-msf-screen B-table-Win 
FUNCTION display-msf-screen RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-setup B-table-Win 
FUNCTION display-setup RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-setup-screen B-table-Win 
FUNCTION display-setup-screen RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear" 
     SIZE 8 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 75 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 17.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      rm-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      rm-rctd.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>9":U LABEL-BGCOLOR 14
      rm-rctd.rct-date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      rm-rctd.po-no FORMAT "x(6)":U WIDTH 9 LABEL-BGCOLOR 14
      rm-rctd.job-no FORMAT "x(6)":U LABEL-BGCOLOR 14
      rm-rctd.job-no2 FORMAT "99":U
      rm-rctd.s-num COLUMN-LABEL "S" FORMAT ">9":U
      rm-rctd.b-num COLUMN-LABEL "B" FORMAT ">9":U
      rm-rctd.i-no COLUMN-LABEL "Item" FORMAT "x(10)":U LABEL-BGCOLOR 14
      rm-rctd.i-name COLUMN-LABEL "Name/Desc" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      rm-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U LABEL-BGCOLOR 14
      rm-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U LABEL-BGCOLOR 14
      rm-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U LABEL-BGCOLOR 14
      rm-rctd.qty COLUMN-LABEL "Qty" FORMAT "->>>,>>>,>>9.9<<":U
            WIDTH 22 LABEL-BGCOLOR 14
      rm-rctd.pur-uom COLUMN-LABEL "PUOM" FORMAT "x(4)":U WIDTH 7
            LABEL-BGCOLOR 14
      rm-rctd.cost COLUMN-LABEL "Cost" FORMAT "->,>>>,>>9.99<<<<":U
            LABEL-BGCOLOR 14
      rm-rctd.cost-uom COLUMN-LABEL "CUOM" FORMAT "x(4)":U WIDTH 7
            LABEL-BGCOLOR 14
      calc-ext-cost() @ ext-cost COLUMN-LABEL "Ext.Amount" FORMAT "->,>>>,>>9.99<<":U
            WIDTH 20.2 COLUMN-BGCOLOR 14
      display-dimension('W') @ lv-po-wid COLUMN-LABEL "Width"
      display-dimension('L') @ lv-po-len COLUMN-LABEL "Length"
      display-setup() @ lv-setup COLUMN-LABEL "Setup" FORMAT ">>,>>9.99":U
      display-adder() @ lv-adder COLUMN-LABEL "Adder Cost/MSF" FORMAT "-z,zz9.9999":U
      display-msf() @ lv-msf COLUMN-LABEL "MSF" FORMAT "->>>>,>>9.999":U
      rm-rctd.user-id COLUMN-LABEL "User ID" FORMAT "x(8)":U WIDTH 15
      rm-rctd.tag2 COLUMN-LABEL "Cert/Lot/Mill#" FORMAT "x(30)":U
            WIDTH 40
  ENABLE
      rm-rctd.rct-date
      rm-rctd.po-no
      rm-rctd.job-no
      rm-rctd.job-no2
      rm-rctd.s-num
      rm-rctd.b-num
      rm-rctd.i-no
      rm-rctd.i-name
      rm-rctd.loc
      rm-rctd.loc-bin
      rm-rctd.tag
      rm-rctd.qty
      rm-rctd.pur-uom
      rm-rctd.cost
      rm-rctd.cost-uom
      rm-rctd.tag2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.24
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.71 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 16.71 COL 80.6 COLON-ALIGNED NO-LABEL
     auto_find AT ROW 16.71 COL 113.4 COLON-ALIGNED HELP
          "Enter Auto Find Value" NO-LABEL
     Btn_Clear_Find AT ROW 16.71 COL 138 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.71 COL 3
     RECT-4 AT ROW 16.48 COL 2
     RECT-5 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
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
         HEIGHT             = 18.38
         WIDTH              = 154.4.
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
/* BROWSE-TAB Browser-Table RECT-5 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.rm-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "rm-rctd.company = cocode and
rm-rctd.rita-code = ""R"""
     _FldNameList[1]   > asi.rm-rctd.r-no
"rm-rctd.r-no" "Seq#" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.rm-rctd.rct-date
"rm-rctd.rct-date" ? ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.rm-rctd.po-no
"rm-rctd.po-no" ? "x(6)" "character" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.rm-rctd.job-no
"rm-rctd.job-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.rm-rctd.job-no2
"rm-rctd.job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.rm-rctd.s-num
"rm-rctd.s-num" "S" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.rm-rctd.b-num
"rm-rctd.b-num" "B" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.rm-rctd.i-no
"rm-rctd.i-no" "Item" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.rm-rctd.i-name
"rm-rctd.i-name" "Name/Desc" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.rm-rctd.loc
"rm-rctd.loc" "Whse" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.rm-rctd.loc-bin
"rm-rctd.loc-bin" "Bin" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.rm-rctd.tag
"rm-rctd.tag" "Tag#" "x(20)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.rm-rctd.qty
"rm-rctd.qty" "Qty" "->>>,>>>,>>9.9<<" "decimal" ? ? ? 14 ? ? yes ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > asi.rm-rctd.pur-uom
"rm-rctd.pur-uom" "PUOM" "x(4)" "character" ? ? ? 14 ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > asi.rm-rctd.cost
"rm-rctd.cost" "Cost" "->,>>>,>>9.99<<<<" "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > asi.rm-rctd.cost-uom
"rm-rctd.cost-uom" "CUOM" "x(4)" "character" ? ? ? 14 ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"calc-ext-cost() @ ext-cost" "Ext.Amount" "->,>>>,>>9.99<<" ? 14 ? ? ? ? ? no ? no no "20.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"display-dimension('W') @ lv-po-wid" "Width" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"display-dimension('L') @ lv-po-len" "Length" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"display-setup() @ lv-setup" "Setup" ">>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"display-adder() @ lv-adder" "Adder Cost/MSF" "-z,zz9.9999" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"display-msf() @ lv-msf" "MSF" "->>>>,>>9.999" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > asi.rm-rctd.user-id
"rm-rctd.user-id" "User ID" ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > asi.rm-rctd.tag2
"rm-rctd.tag2" "Cert/Lot/Mill#" "x(30)" "character" ? ? ? ? ? ? yes ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
        RUN update-begin.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ENTRY OF Browser-Table IN FRAME F-Main
DO:
        RUN get-matrix (YES).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO: 
        DEFINE VARIABLE ll-tag#    AS LOG           NO-UNDO.
        DEFINE VARIABLE help-recid AS RECID         NO-UNDO.
        DEFINE VARIABLE help-rowid AS ROWID         NO-UNDO.
        DEFINE VARIABLE lv-focus   AS WIDGET-HANDLE NO-UNDO.


        ASSIGN
            ll-help-run = YES
            lv-focus    = FOCUS.

        CASE lv-focus:NAME :
            WHEN "po-no" THEN 
                DO:
                    RUN windows/l-poordl.w (rm-rctd.company,lv-focus:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        ASSIGN 
                            rm-rctd.po-no:screen-value IN BROWSE {&browse-name}   = ENTRY(1,char-val)
                            rm-rctd.i-no:screen-value IN BROWSE {&browse-name}    = ENTRY(2,char-val)
                            rm-rctd.i-name:screen-value IN BROWSE {&browse-name}  = ENTRY(3,char-val)
                            rm-rctd.job-no:screen-value IN BROWSE {&browse-name}  = ENTRY(4,char-val)
                            rm-rctd.job-no2:screen-value IN BROWSE {&browse-name} = ENTRY(5,char-val)
                            .
                        FIND po-ordl WHERE po-ordl.company = rm-rctd.company AND
                            po-ordl.po-no = integer(ENTRY(1,char-val)) AND
                            po-ordl.line = integer(ENTRY(6,char-val))
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE po-ordl THEN RUN update-from-po-line.

                        ELSE 
                        DO:
                            FIND FIRST item WHERE item.company = rm-rctd.company AND
                                item.i-no = entry(2,char-val)
                                NO-LOCK NO-ERROR.
                            ASSIGN 
                                rm-rctd.pur-uom:screen-value IN BROWSE {&browse-name}  = item.cons-uom
                                rm-rctd.cost-uom:screen-value IN BROWSE {&browse-name} = item.cons-uom                       
                                /*                       r
                                m-rctd.s-num:screen-value in browse {&browse-name} = string(item.s-num)*/
                                .                      
                        END.
                        IF NOT AVAILABLE item THEN FIND FIRST item WHERE item.company = rm-rctd.company AND
                                item.i-no = entry(2,char-val)
                                NO-LOCK NO-ERROR.
                        IF v-bin NE "user entered" THEN
                          ASSIGN 
                            rm-rctd.loc:screen-value IN BROWSE {&browse-name}     = item.loc
                            rm-rctd.loc-bin:screen-value IN BROWSE {&browse-name} = item.loc-bin.
                        IF rm-rctd.loc-bin:screen-value IN BROWSE {&browse-name} EQ "" THEN 
                        DO:
                            /*FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ gcompany
                                                  AND sys-ctrl.name    EQ "RMWHSBIN" NO-LOCK NO-ERROR.
                            IF NOT AVAIL sys-ctrl THEN DO:
                               CREATE sys-ctrl.
                               ASSIGN
                                 sys-ctrl.company  = gcompany
                                 sys-ctrl.name     = "RMWHSBIN"
                                 sys-ctrl.descrip  = "Default Location for RM Warehouse / Bin?"
                                 sys-ctrl.char-fld = "RMITEM".
                            END.*/
                            IF v-bin NE "user entered" THEN
                            ASSIGN 
                                rm-rctd.loc-bin:screen-value IN BROWSE {&browse-name} = SUBSTR(v-bin,6).
                        END.

                        RUN tag-method (OUTPUT ll-tag#).
                        IF ll-tag# AND rm-rctd.po-no:screen-value IN BROWSE {&browse-name} <> ""
                            THEN 
                        DO:
                            RUN tag-sequence.
                        END.
                        ext-cost = 0.
                        DISPLAY ext-cost WITH BROWSE {&browse-name}.
                        IF v-bin NE "" AND v-bin NE 'RMITEM' AND v-bin NE "user entered" THEN
                            ASSIGN
                                rm-rctd.loc:screen-value IN BROWSE {&browse-name}     = SUBSTR(v-bin,1,5)
                                rm-rctd.loc-bin:screen-value IN BROWSE {&browse-name} = SUBSTR(v-bin,6).
                    END.  /* char-val <> "" */  
                END.
            WHEN "i-no" THEN 
                DO:
                    IF DEC(rm-rctd.po-no:SCREEN-VALUE) NE 0 THEN 
                    DO:
                        RUN windows/l-poitmw.w (YES, rm-rctd.company,rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name},
                            YES, lv-focus:SCREEN-VALUE,
                            ROWID(rm-rctd), OUTPUT help-rowid).
                        FIND po-ordl WHERE ROWID(po-ordl) EQ help-rowid NO-LOCK NO-ERROR.
                        IF AVAILABLE po-ordl THEN 
                        DO:
                            ASSIGN 
                                lv-focus:SCREEN-VALUE                                 = po-ordl.i-no
                                rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}  = po-ordl.i-name
                                rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = po-ordl.job-no
                                rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(po-ordl.job-no2).
                            RUN update-from-po-line.
                        END.
                    END.

                    ELSE
                        IF rm-rctd.job-no:SCREEN-VALUE NE "" THEN 
                        DO:
                            RUN windows/l-jobmat.w (rm-rctd.company,rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                                rm-rctd.job-no2:SCREEN-VALUE,rm-rctd.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT help-recid).
                            IF help-recid <> ? THEN RUN DISPLAY-jobmat (help-recid).
                        END.

                        ELSE 
                        DO:
                            /* company,industry,mat-type,i-code,i-no, output, output */
                            RUN windows/l-itmRE.w (rm-rctd.company,"","","R",lv-focus:SCREEN-VALUE, OUTPUT char-val,OUTPUT help-recid).
                            IF char-val <> "" THEN 
                            DO :
                                ASSIGN 
                                    lv-focus:SCREEN-VALUE       = ENTRY(1,char-val)
                                    rm-rctd.i-name:SCREEN-VALUE = ENTRY(2,char-val).
                                RUN display-item(help-recid).
                            END.
                        END.  
                END.
            WHEN "s-num" OR 
            WHEN "b-num" THEN 
                DO:
                    IF rm-rctd.po-no:SCREEN-VALUE NE "" THEN 
                    DO:
                        RUN windows/l-poitmw.w (YES, rm-rctd.company,rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name},
                            YES, rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name},
                            ROWID(rm-rctd), OUTPUT help-rowid).
                        FIND po-ordl WHERE ROWID(po-ordl) EQ help-rowid NO-LOCK NO-ERROR.
                        IF AVAILABLE po-ordl THEN 
                        DO:
                            ASSIGN 
                                rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}    = po-ordl.i-no
                                rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}  = po-ordl.i-name
                                rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = po-ordl.job-no
                                rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(po-ordl.job-no2).
                            RUN update-from-po-line.
                        END.
                    END.  
                END.
            WHEN "job-no" OR 
            WHEN "job-no2" THEN 
                DO:
                    IF DEC(rm-rctd.po-no:SCREEN-VALUE) EQ 0 THEN 
                    DO:
                        RUN windows/l-jobno.w (rm-rctd.company,rm-rctd.job-no:screen-value, OUTPUT char-val, OUTPUT help-recid).
                        IF char-val <> "" THEN
                            ASSIGN /*lv-focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     */ 
                                rm-rctd.job-no:screen-value  = ENTRY(1,char-val)
                                rm-rctd.job-no2:screen-value = ENTRY(2,char-val).
                    END.
                    ELSE 
                    DO:
                        RUN windows/l-pojob.w (rm-rctd.company,rm-rctd.po-no:screen-value,rm-rctd.i-no:screen-value, OUTPUT char-val).
                        IF char-val <> "" THEN
                            ASSIGN /*lv-focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     */ 
                                rm-rctd.job-no:screen-value  = ENTRY(1,char-val)
                                rm-rctd.job-no2:screen-value = ENTRY(2,char-val).
           
                    END.   
                END.  
            WHEN "loc" THEN 
                DO:
                    RUN rm/l-loc.w (rm-rctd.company,lv-focus:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN
                        ASSIGN lv-focus:SCREEN-VALUE = ENTRY(1,char-val)
                            /*rm-rctd.loc-bin:screen-value in browse {&browse-name} = entry(2,char-val) */.
           
                END.
            WHEN "loc-bin" THEN 
                DO:
                    RUN rm/l-locbin.w (rm-rctd.company,rm-rctd.loc:screen-value, OUTPUT char-val).
                    IF char-val <> "" THEN
                        ASSIGN lv-focus:SCREEN-VALUE    = ENTRY(1,char-val)
                            rm-rctd.loc:screen-value = ENTRY(2,char-val)
                            rm-rctd.qty:screen-value = ENTRY(3,char-val).   
                END.
        END CASE.

        RETURN NO-APPLY. 

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON RETURN OF Browser-Table IN FRAME F-Main
DO:
        RUN update-begin.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
        /* This code displays initial values for newly added or copied rows. */
        {src/adm/template/brsentry.i}
  
        ASSIGN
            ll-help-run  = NO
            ll-qty-valid = NO
            ll-warned    = NO.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
        /* Do not disable this code or no updates will take place except
         by pressing the Save button on an Update SmartPanel. */
        /*  {src/adm/template/brsleave.i}  */

        IF KEYFUNCTION(LASTKEY) = "page-up" OR 
            keyfunction(LASTKEY) = "page-down" OR
            keyfunction(LASTKEY) = "cursor-up" OR
            keyfunction(LASTKEY) = "cursor-down" 
            THEN 
        DO:  
            RETURN NO-APPLY.
        END.

        {est/brsleave.i}  /* same as src but update will be same as add record*/

        RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR.

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

        IF AVAILABLE rm-rctd                                                          AND
            CAN-FIND(FIRST tt-rm-rctd WHERE tt-rm-rctd.tt-rowid EQ ROWID(rm-rctd)) THEN
            RUN update-tt.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.rct-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.rct-date Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.rct-date IN BROWSE Browser-Table /* Receipt Date */
DO:
        IF v-copy-mode-dec-1 THEN
        DO:
            APPLY "TAB" TO rm-rctd.rct-date IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.rct-date Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.rct-date IN BROWSE Browser-Table /* Receipt Date */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            {custom/currentDatePrompt.i SELF:SCREEN-VALUE}
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:
        IF v-copy-mode-dec-1 THEN
        DO:
            APPLY "TAB" TO rm-rctd.po-no IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF rmrecpt-cha NE "POPUP"                                        AND
                INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND
                {&self-name}:MODIFIED                                         THEN DO:

            RUN display-po-job.
            IF lv-rowid EQ ? THEN RUN find-exact-po.

            FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

            IF NOT AVAILABLE po-ordl THEN
                FIND FIRST po-ordl
                    WHERE po-ordl.company   EQ rm-rctd.company
                    AND po-ordl.po-no     EQ INT(SELF:SCREEN-VALUE IN BROWSE {&browse-name})
                    AND po-ordl.item-type EQ YES
                    NO-LOCK NO-ERROR.

            IF AVAILABLE po-ordl THEN 
            DO:
                lv-rowid = ROWID(po-ordl).
                RUN display-po-info.
            END.
        END.

        RUN valid-po-no (1) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:
        ll-warned = NO.
        IF INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
    {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} = "".
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.job-no IN BROWSE Browser-Table /* Job # */
DO:
        lv-job-no = rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.

        IF CAN-FIND(FIRST tt-rm-rctd WHERE tt-rm-rctd.tt-rowid EQ ROWID(rm-rctd)) THEN
            RUN update-tt.
        /*IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
          APPLY "leave" TO {&self-name}.
          RETURN NO-APPLY.
        END.*/

        IF v-copy-mode-dec-1 THEN
        DO:
            APPLY "TAB" TO rm-rctd.job-no IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.job-no IN BROWSE Browser-Table /* Job # */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF {&self-name}:MODIFIED IN BROWSE {&browse-name} THEN DO:
            IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN 
            DO:
                RUN find-exact-po.

                FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

                IF NOT AVAILABLE po-ordl THEN
                    FIND FIRST po-ordl
                        WHERE po-ordl.company   EQ rm-rctd.company
                        AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND po-ordl.job-no    EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

                IF AVAILABLE po-ordl THEN 
                DO:
                    lv-rowid = ROWID(po-ordl).
                    RUN display-po-info.
                END.
            END.

            ELSE RUN check-for-job-mat.
        END.

        RUN valid-job-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.job-no IN BROWSE Browser-Table /* Job # */
DO:
        ll-warned = NO.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.job-no2 IN BROWSE Browser-Table
DO:
        /*IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
          APPLY "leave" TO {&self-name}.
          RETURN NO-APPLY.
        END.*/

        IF v-copy-mode-dec-1 THEN
        DO:
            APPLY "TAB" TO rm-rctd.job-no2 IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.job-no2 IN BROWSE Browser-Table
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF {&self-name}:MODIFIED IN BROWSE {&browse-name} THEN DO:
            IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN 
            DO:
                RUN find-exact-po.

                FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

                IF NOT AVAILABLE po-ordl THEN
                    FIND FIRST po-ordl
                        WHERE po-ordl.company   EQ rm-rctd.company
                        AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                        AND po-ordl.job-no2   EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

                IF AVAILABLE po-ordl THEN 
                DO:
                    lv-rowid = ROWID(po-ordl).
                    RUN display-po-info.
                END.
            END.

            ELSE RUN check-for-job-mat.
        END.

        RUN valid-job-no2 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.job-no2 IN BROWSE Browser-Table
DO:
        ll-warned = NO.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.s-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.s-num Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.s-num IN BROWSE Browser-Table /* S */
DO:
        /*IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
          APPLY "leave" TO {&self-name}.
          RETURN NO-APPLY.
        END.*/

        IF v-copy-mode-dec-1 THEN
        DO:
            APPLY "TAB" TO rm-rctd.s-num IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.s-num Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.s-num IN BROWSE Browser-Table /* S */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF {&self-name}:MODIFIED IN BROWSE {&browse-name} THEN DO:
            IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN 
            DO:
                RUN find-exact-po.

                FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

                IF NOT AVAILABLE po-ordl THEN
                    FIND FIRST po-ordl
                        WHERE po-ordl.company   EQ rm-rctd.company
                        AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                        AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND po-ordl.s-num     EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

                IF AVAILABLE po-ordl THEN 
                DO:
                    lv-rowid = ROWID(po-ordl).
                    RUN display-po-info.
                END.
            END.

            ELSE RUN check-for-job-mat.
        END.

        RUN valid-s-num NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.s-num Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.s-num IN BROWSE Browser-Table /* S */
DO:
        ll-warned = NO.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.b-num Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.b-num IN BROWSE Browser-Table /* B */
DO:
        IF v-copy-mode-dec-1 THEN
        DO:
            APPLY "TAB" TO rm-rctd.b-num IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.b-num Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.b-num IN BROWSE Browser-Table /* B */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF {&self-name}:MODIFIED IN BROWSE {&browse-name} THEN DO:
            IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN 
            DO:
                RUN find-exact-po.

                FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

                IF NOT AVAILABLE po-ordl THEN
                    FIND FIRST po-ordl
                        WHERE po-ordl.company   EQ rm-rctd.company
                        AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                        AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND po-ordl.b-num     EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

                IF AVAILABLE po-ordl THEN 
                DO:
                    lv-rowid = ROWID(po-ordl).
                    RUN display-po-info.
                END.
            END.

            ELSE RUN check-for-job-mat.
        END.

        RUN valid-b-num NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.b-num Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.b-num IN BROWSE Browser-Table /* B */
DO:
        ll-warned = NO.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
        IF v-copy-mode-dec-1 THEN
        DO:
            APPLY "TAB" TO rm-rctd.i-no IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF {&self-name}:MODIFIED IN BROWSE {&browse-name} THEN DO:
            IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN 
            DO:
                RUN find-exact-po.

                FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

                IF NOT AVAILABLE po-ordl THEN
                    FIND FIRST po-ordl NO-LOCK
                        WHERE po-ordl.company   EQ rm-rctd.company
                        AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                        AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND po-ordl.i-no      EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.item-type EQ YES
          USE-INDEX item-ordno NO-ERROR.

                IF AVAILABLE po-ordl THEN 
                DO:
                    lv-rowid = ROWID(po-ordl).
                    RUN display-po-info.
                END.
            END.

            ELSE RUN check-for-job-mat.
        END.

        RUN valid-i-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
         
        FIND FIRST ITEM
            WHERE ITEM.company EQ cocode
            AND ITEM.i-no    EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
         AND ITEM.i-code  EQ "R"
       NO-LOCK NO-ERROR.
    
        IF AVAIL(ITEM) THEN
            ASSIGN rm-rctd.pur-uom:screen-value IN BROWSE {&browse-name} = item.cons-uom.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
        ll-warned = NO.
        FIND item NO-LOCK
            WHERE item.company EQ rm-rctd.company
            AND item.i-no    BEGINS rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            NO-ERROR.             
        IF AVAILABLE item THEN 
        DO:
            rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
            RUN display-item (RECID(item)).
            ASSIGN 
                rm-rctd.pur-uom:screen-value IN BROWSE {&browse-name} = item.cons-uom.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-name Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.i-name IN BROWSE Browser-Table /* Name/Desc */
DO:
        IF v-copy-mode-dec-1 THEN
        DO:
            APPLY "TAB" TO rm-rctd.i-name IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.loc IN BROWSE Browser-Table /* Whse */
DO:
        IF v-copy-mode-dec-1 THEN
        DO:
            APPLY "TAB" TO rm-rctd.loc IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc IN BROWSE Browser-Table /* Whse */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-loc (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
        IF v-copy-mode-dec-1 THEN
        DO:
            APPLY "TAB" TO rm-rctd.loc-bin IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-loc-bin (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
        IF v-copy-mode-dec-1 THEN
            v-copy-mode-dec-1 = NO.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-tag (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.qty IN BROWSE Browser-Table /* Qty */
DO:
        lv-entry-qty = DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.qty IN BROWSE Browser-Table /* Qty */
DO:
        IF lv-entry-qty NE DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
            RUN po-cost.

        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-qty (FOCUS, OUTPUT op-error).
            IF op-error THEN
                RETURN NO-APPLY.

            RUN get-matrix (NO).

            IF NOT ll-qty-valid THEN DO:
                IF rm-rctd.qty NE DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
                    lv-adder:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(display-adder-screen()).
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.pur-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.pur-uom IN BROWSE Browser-Table /* PUOM */
DO:
        lv-entry-qty-uom = rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.pur-uom IN BROWSE Browser-Table /* PUOM */
DO:
        IF lv-entry-qty-uom NE rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name} THEN
            RUN po-cost.

        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-uom (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    
            RUN get-matrix (NO).
    
            IF NOT ll-qty-valid THEN 
            DO:
                {rm/chkporun.i}
                ll-qty-valid = YES.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.cost IN BROWSE Browser-Table /* Cost */
DO:
        lv-save-fld[1] = {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.cost IN BROWSE Browser-Table /* Cost */
DO:
        RUN get-matrix (NO).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.cost-uom IN BROWSE Browser-Table /* CUOM */
DO:
        lv-save-fld[2] = {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.cost-uom IN BROWSE Browser-Table /* CUOM */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-uom (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            /*
            ll-add-setup = DEC(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}) NE
                               DEC(lv-save-fld[1]) OR
                           TRIM(rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}) NE
                               TRIM(lv-save-fld[2]). */

            RUN get-matrix (NO).
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/tag#.i}
ll-tag-meth = v-tag#.

{ce/msfcalc.i}

FIND FIRST sys-ctrl WHERE
    sys-ctrl.company EQ gcompany AND
    sys-ctrl.name    EQ "RMWHSBIN"
    NO-LOCK NO-ERROR.

IF NOT AVAILABLE sys-ctrl THEN
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = gcompany
        sys-ctrl.name     = "RMWHSBIN"
        sys-ctrl.descrip  = "Default Location for RM Warehouse / Bin?"
        sys-ctrl.char-fld = "RMITEM".
    FIND CURRENT sys-ctrl NO-LOCK.
END.
v-bin = sys-ctrl.char-fld.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-add B-table-Win 
PROCEDURE auto-add :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    RUN get-link-handle IN adm-broker-hdl
        (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
    phandle = WIDGET-HANDLE(char-hdl).
    RUN auto-add IN phandle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-add-tt B-table-Win 
PROCEDURE auto-add-tt :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    IF CAN-FIND(FIRST tt-rm-rctd) THEN RUN auto-add.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancel-item B-table-Win 
PROCEDURE cancel-item :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF AVAILABLE rm-rctd AND rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN
        RUN dispatch IN THIS-PROCEDURE (INPUT 'cancel-record':U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-for-job-mat B-table-Win 
PROCEDURE check-for-job-mat :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE "" THEN 
        DO:
            FIND FIRST job
                WHERE job.company EQ rm-rctd.company
                AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                NO-LOCK NO-ERROR.
            IF AVAILABLE job THEN
                FIND FIRST job-mat
                    WHERE job-mat.company  EQ rm-rctd.company
                    AND job-mat.job      EQ job.job
                    AND job-mat.job-no   EQ job.job-no
                    AND job-mat.job-no2  EQ job.job-no2
                    AND job-mat.frm      EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                    AND job-mat.blank-no EQ INT(rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name})
                    AND job-mat.i-no     EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                    NO-LOCK NO-ERROR.
            IF AVAILABLE job-mat THEN RUN display-jobmat (RECID(job-mat)).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convert-vend-comp-curr B-table-Win 
PROCEDURE convert-vend-comp-curr :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ip-cost AS DECIMAL DECIMALS 4 NO-UNDO.
    
    FIND FIRST b-po-ord WHERE
        b-po-ord.company EQ po-ordl.company AND
        b-po-ord.po-no   EQ po-ordl.po-no
        NO-LOCK NO-ERROR.

    IF AVAILABLE b-po-ord THEN
    DO:
        FIND FIRST vend WHERE
            vend.company EQ po-ord.company AND
            vend.vend-no EQ po-ord.vend-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE vend THEN
        DO:
            FIND FIRST b-company WHERE
                b-company.company EQ cocode
                NO-LOCK.

            IF vend.curr-code NE b-company.curr-code THEN
            DO:
                FIND FIRST currency WHERE
                    currency.company EQ cocode AND
                    currency.c-code EQ vend.curr-code
                    NO-LOCK NO-ERROR.

                IF AVAILABLE currency THEN
                DO:
                    ip-cost = ip-cost * currency.ex-rate.

                    RELEASE currency.
                END.
            END.

            RELEASE b-company.
            RELEASE vend.
        END.

        RELEASE b-po-ord.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-from-po B-table-Win 
PROCEDURE create-from-po :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE ld AS DECIMAL NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        li-pos = 0.

        FOR EACH tt-pol WHERE tt-pol.selekt,
            FIRST po-ordl WHERE ROWID(po-ordl) EQ tt-pol.row-id NO-LOCK:

            CREATE tt-rm-rctd.
            ASSIGN
                li-pos              = li-pos + 1
                tt-rm-rctd.rct-date = DATE(rm-rctd.rct-date:SCREEN-VALUE IN BROWSE {&browse-name})
                tt-rm-rctd.i-no     = po-ordl.i-no
                tt-rm-rctd.po-no    = STRING(po-ordl.po-no)
                tt-rm-rctd.po-rowid = ROWID(po-ordl).

        /*ld = po-ordl.ord-qty.
        
        FIND FIRST item
            WHERE item.company EQ po-ordl.company
              AND item.i-no    EQ po-ordl.i-no
            NO-LOCK NO-ERROR.
  
        IF po-ordl.pr-qty-uom NE rm-rctd.pur-uom:SCREEN-VALUE THEN
          RUN sys/ref/convquom.p (po-ordl.pr-qty-uom,
                                  rm-rctd.pur-uom:SCREEN-VALUE,
                                  po-ordl.s-len,
                                  po-ordl.s-wid,
                                  item.s-dep,
                                  item.basis-w,
                                  ld,
                                  OUTPUT ld).
  
        {sys/inc/roundup.i ld}
  
        tt-rm-rctd.qty = ld.
  
        IF tt-rm-rctd.qty LT 0 THEN tt-rm-rctd.qty = 0.*/
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item B-table-Win 
PROCEDURE create-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     CREATE ITEM.
     ASSIGN ITEM.company = cocode
            ITEM.loc = locode
            ITEM.i-no = rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            ITEM.i-code = "R".

    /* run rm/item.p */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-neg-rctd B-table-Win 
PROCEDURE create-neg-rctd :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER in-recid AS RECID.
    DEFINE VARIABLE lv-rno AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-rm-rctd FOR rm-rctd.

    FIND rm-rctd WHERE RECID(rm-rctd) = in-recid NO-LOCK NO-ERROR.
    IF AVAILABLE rm-rctd THEN 
    DO:
        RUN sys/ref/asiseq.p (INPUT g_company, INPUT "rm_rcpt_seq", OUTPUT lv-rno) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

        CREATE bf-rm-rctd.
        BUFFER-COPY rm-rctd EXCEPT r-no rec_key
            TO bf-rm-rctd.
        ASSIGN 
            bf-rm-rctd.r-no = lv-rno
            bf-rm-rctd.qty  = rm-rctd.qty * -1.
    END.
    RELEASE bf-rm-rctd.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-rcptd B-table-Win 
PROCEDURE create-rcptd :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-list AS cha NO-UNDO.
    DEFINE INPUT PARAMETER ip-date AS DATE NO-UNDO.

    DEFINE VARIABLE lv-rno LIKE fg-rctd.r-no NO-UNDO.
    DEFINE BUFFER b-rm-rctd  FOR rm-rctd.
    DEFINE BUFFER bf-rm-rctd FOR rm-rctd.
    DEFINE VARIABLE li-tag-seq    AS INTEGER.
    DEFINE VARIABLE lv-rctd-rowid AS ROWID     NO-UNDO.
    DEFINE VARIABLE lv-locode     LIKE locode.
    DEFINE VARIABLE li            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-count       AS INTEGER   INIT 1 NO-UNDO .
    DEFINE VARIABLE cNewTag       AS CHARACTER NO-UNDO.
  
    FOR EACH tt-selected,
        FIRST po-ordl WHERE ROWID(po-ordl) = tt-selected.tt-rowid NO-LOCK BY tt-item :
  
        FIND LAST b-rm-rctd USE-INDEX rm-rctd NO-LOCK NO-ERROR.

        IF AVAILABLE b-rm-rctd AND b-rm-rctd.r-no GT lv-rno THEN lv-rno = b-rm-rctd.r-no.

        FIND LAST rm-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAILABLE rm-rcpth AND rm-rcpth.r-no GT lv-rno THEN lv-rno = rm-rcpth.r-no.

        CREATE rm-rctd .
  
        RUN sys/ref/asiseq.p (INPUT g_company, INPUT "rm_rcpt_seq", OUTPUT lv-rno) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

        FIND FIRST tt-rm-rctd NO-ERROR.

        IF AVAILABLE tt-rm-rctd THEN 
        DO:
            BUFFER-COPY tt-rm-rctd EXCEPT rec_key TO rm-rctd.
            tt-rm-rctd.tt-rowid = ROWID(rm-rctd).
        END.

        ASSIGN
            v-new-mode        = YES
            rm-rctd.company   = cocode
            rm-rctd.r-no      = lv-rno
            rm-rctd.rita-code = "R"
            rm-rctd.loc       = gloc
            rm-rctd.s-num     = 0
            rm-rctd.b-num     = 0
            rm-rctd.rct-date  = TODAY 
            lv-rowid          = ? .

        ASSIGN              
            rm-rctd.po-no    = STRING(po-ordl.po-no)
            rm-rctd.i-no     = po-ordl.i-no
            rm-rctd.i-name   = po-ordl.i-name
            rm-rctd.job-no   = po-ordl.job-no
            rm-rctd.job-no2  = po-ordl.job-no2
            rm-rctd.s-num    = (po-ordl.s-num)
            rm-rctd.b-num    = (po-ordl.b-num)
            rm-rctd.pur-uom  = po-ordl.pr-qty-uom
            rm-rctd.cost-uom = po-ordl.pr-uom
            .

        IF v-count = 1 THEN
            ASSIGN
                lv-rctd-rowid = ROWID(rm-rctd)
                v-count       = v-count + 1.
  
 
        IF ip-date <> ? THEN
            rm-rctd.rct-date = ip-date .
    
        FIND FIRST item WHERE item.company = rm-rctd.company AND
            item.i-no = po-ordl.i-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE ITEM  THEN
            ASSIGN rm-rctd.loc     = item.loc
                rm-rctd.loc-bin = item.loc-bin.

        IF rm-rctd.loc-bin = "" THEN
            rm-rctd.loc-bin = SUBSTR(v-bin,6).

        IF ll-tag-meth  THEN
        DO:
            RUN pGetTagSequence(cocode, po-ordl.po-no, OUTPUT cNewTag). 
            rm-rctd.tag = cNewTag.
        END.
        /*        DO:                                                                            */
        /*            ASSIGN                                                                     */
        /*                li-tag-seq = 0                                                         */
        /*                lv-locode  = "".                                                       */
        /*                                                                                       */
        /*            DO WHILE TRUE:                                                             */
        /*                FIND FIRST bf-rm-rctd                                                  */
        /*                    WHERE bf-rm-rctd.company EQ cocode                                 */
        /*                    AND bf-rm-rctd.loc     GT lv-locode                                */
        /*                    NO-LOCK NO-ERROR.                                                  */
        /*                                                                                       */
        /*                IF AVAILABLE bf-rm-rctd THEN                                           */
        /*                DO:                                                                    */
        /*                    lv-locode = bf-rm-rctd.loc.                                        */
        /*                                                                                       */
        /*                    FOR EACH bf-rm-rctd                                                */
        /*                        WHERE bf-rm-rctd.company EQ cocode                             */
        /*                        AND bf-rm-rctd.loc     EQ lv-locode                            */
        /*                        AND bf-rm-rctd.tag     BEGINS STRING(po-ordl.po-no,"999999")   */
        /*                        USE-INDEX tag NO-LOCK                                          */
        /*                        BY bf-rm-rctd.tag DESCENDING:                                  */
        /*                                                                                       */
        /*                        IF INT(SUBSTR(bf-rm-rctd.tag,7,2)) GT li-tag-seq THEN          */
        /*                            li-tag-seq = INT(SUBSTR(bf-rm-rctd.tag,7,2)).              */
        /*                        LEAVE.                                                         */
        /*                    END.                                                               */
        /*                END.                                                                   */
        /*                                                                                       */
        /*                ELSE LEAVE.                                                            */
        /*            END.                                                                       */
        /*            MESSAGE "before" li-tag-seq VIEW-AS ALERT-BOX.                             */
        /*            IF li-tag-seq GT 0 THEN                                                    */
        /*            DO:                                                                        */
        /*                FIND LAST rm-rdtlh NO-LOCK                                             */
        /*                    WHERE rm-rdtlh.company EQ cocode                                   */
        /*                    AND rm-rdtlh.tag     BEGINS STRING(po-ordl.po-no,"999999")         */
        /*                    AND INT(SUBSTR(rm-rdtlh.tag,7,2)) GT li-tag-seq                    */
        /*                    USE-INDEX tag NO-ERROR.                                            */
        /*                IF AVAILABLE rm-rdtlh THEN li-tag-seq = INT(SUBSTR(rm-rdtlh.tag,7,2)). */
        /*            END.                                                                       */
        /*                                                                                       */
        /*            ASSIGN                                                                     */
        /*                li-tag-seq  = li-tag-seq + 1                                           */
        /*                rm-rctd.tag = STRING(po-ordl.po-no,"999999") + STRING(li-tag-seq,"99").*/
        /*        END. /* ll-tag-meth*/                                                          */
        /*        MESSAGE "after" li-tag-seq VIEW-AS ALERT-BOX.                                  */
        FIND CURRENT ITEM NO-LOCK NO-ERROR.
        IF AVAILABLE ITEM AND ITEM.inv-by-cust AND v-new-mode THEN 
        DO:
            RUN create-neg-rctd (INPUT RECID(rm-rctd)).                       
            ASSIGN 
                v-new-mode = NO.
        END.

    END.
 
    RUN delete-tt.
    RUN dispatch ('open-query').
  
    RUN repo-query (lv-rctd-rowid).
    RUN update-begin.
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-tt B-table-Win 
PROCEDURE delete-tt :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    IF AVAILABLE rm-rctd THEN 
    DO:
        FIND FIRST tt-rm-rctd WHERE tt-rm-rctd.tt-rowid EQ ROWID(rm-rctd) NO-ERROR.
        IF AVAILABLE tt-rm-rctd THEN DELETE tt-rm-rctd.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item B-table-Win 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.


    FIND ITEM WHERE RECID(ITEM) EQ ip-recid NO-LOCK NO-ERROR.

    DO WITH FRAME {&FRAME-NAME}:
        IF AVAILABLE item THEN 
        DO:
            ASSIGN                
                rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name} = item.cons-uom.
                
           /* Description may be overridden on PO */
           IF  rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}  = "" THEN 
              rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}  = item.i-name       .         
            IF v-bin NE "user entered" THEN
            ASSIGN
                rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = item.loc
                rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = item.loc-bin.

            IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN
                RUN update-from-po-line.
            ELSE
                IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" OR
                    item.i-code EQ "R"                                         THEN 
                DO:
                    {rm/avgcost.i}

                    ASSIGN
                        /*rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name} = item.cons-uom */
                        rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}     = IF DEC(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
             IF v-avgcost THEN STRING(item.avg-cost)
             ELSE STRING(item.last-cost)
           ELSE rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}
                        rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = item.cons-uom. 
                END.
        END.

        IF (rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     EQ "" OR
            rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} EQ "") AND v-bin NE "user entered" THEN 
        DO:
            FIND FIRST cust
                WHERE cust.company EQ cocode
                AND cust.active  EQ "X" 
                NO-LOCK NO-ERROR.
            IF AVAILABLE cust THEN 
            DO:
                FIND FIRST shipto
                    WHERE shipto.company EQ cocode
                    AND shipto.cust-no EQ cust.cust-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE shipto THEN
                    ASSIGN   
                        rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = shipto.loc
                        rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = shipto.loc-bin.
            END.
        END.

        IF v-bin NE "" AND v-bin NE 'RMITEM' AND v-bin NE "user entered" THEN
            ASSIGN
                rm-rctd.loc:screen-value IN BROWSE {&browse-name}     = SUBSTR(v-bin,1,5)
                rm-rctd.loc-bin:screen-value IN BROWSE {&browse-name} = SUBSTR(v-bin,6).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-jobmat B-table-Win 
PROCEDURE display-jobmat :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        FIND job-mat WHERE RECID(job-mat) EQ ip-recid NO-LOCK NO-ERROR.
        IF AVAILABLE job-mat THEN 
        DO:
            FIND job-mat WHERE RECID(job-mat) EQ ip-recid NO-LOCK.

            ASSIGN
                rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}   = job-mat.job-no
                rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(job-mat.job-no2)
                rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(job-mat.frm)
                rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(job-mat.blank-no)
                rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}     = job-mat.i-no
                rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}     = STRING(job-mat.std-cost)
                rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = job-mat.sc-uom.

            FIND FIRST item 
                WHERE item.company EQ cocode
                AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                NO-LOCK NO-ERROR.

            IF AVAILABLE item THEN RUN display-item (RECID(item)).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-po-info B-table-Win 
PROCEDURE display-po-info :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-rm-rctd FOR rm-rctd.
  
    DEFINE VARIABLE lv-i-no    LIKE rm-rctd.i-no NO-UNDO.
    DEFINE VARIABLE lv-locode  LIKE locode.
    DEFINE VARIABLE li-tag-seq AS INTEGER.
    DEFINE VARIABLE cNewTag    AS CHARACTER NO-UNDO.


    FIND po-ordl WHERE ROWID(po-ordl) EQ lv-rowid NO-LOCK NO-ERROR.

    IF AVAILABLE po-ordl THEN 
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            lv-i-no                                                = rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}

            rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}     = po-ordl.i-no
            rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}   = po-ordl.i-name
            rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}   = po-ordl.job-no
            rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(po-ordl.job-no2)
            rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(po-ordl.s-num)
            rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(po-ordl.b-num)
            rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}  = po-ordl.pr-qty-uom
            rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.pr-uom
            lv-setup                                               = po-ordl.setup
            ll-warned                                              = NO.

        RUN po-cost.

        lv-adder = display-adder-screen().

        RUN convert-vend-comp-curr(INPUT-OUTPUT lv-setup).
    
        DISPLAY lv-setup lv-adder WITH BROWSE {&browse-name}.

        IF rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NE lv-i-no THEN 
        DO:
            FIND FIRST ITEM
                WHERE item.company EQ rm-rctd.company
                AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                NO-LOCK NO-ERROR. 
            IF AVAILABLE ITEM THEN RUN display-item (RECID(ITEM)).
        END.

        IF ll-tag-meth AND adm-new-record THEN 
        DO:
            RUN pGetTagSequence(cocode, po-ordl.po-no, OUTPUT cNewTag).
            rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = cNewTag.
        END.
    /*        DO:                                                                                                                  */
    /*            ASSIGN                                                                                                           */
    /*                li-tag-seq = 0                                                                                               */
    /*                lv-locode  = "".                                                                                             */
    /*                                                                                                                             */
    /*            DO WHILE TRUE:                                                                                                   */
    /*                FIND FIRST b-rm-rctd                                                                                         */
    /*                    WHERE b-rm-rctd.company EQ cocode                                                                        */
    /*                    AND b-rm-rctd.loc     GT lv-locode                                                                       */
    /*                    NO-LOCK NO-ERROR.                                                                                        */
    /*                                                                                                                             */
    /*                IF AVAILABLE b-rm-rctd THEN                                                                                  */
    /*                DO:                                                                                                          */
    /*                    lv-locode = b-rm-rctd.loc.                                                                               */
    /*                                                                                                                             */
    /*                    FOR EACH b-rm-rctd                                                                                       */
    /*                        WHERE b-rm-rctd.company EQ cocode                                                                    */
    /*                        AND b-rm-rctd.loc     EQ lv-locode                                                                   */
    /*                        AND b-rm-rctd.tag     BEGINS STRING(po-ordl.po-no,"999999")                                          */
    /*                        USE-INDEX tag NO-LOCK                                                                                */
    /*                        BY b-rm-rctd.tag DESCENDING:                                                                         */
    /*                                                                                                                             */
    /*                        IF INT(SUBSTR(b-rm-rctd.tag,7,2)) GT li-tag-seq THEN                                                 */
    /*                            li-tag-seq = INT(SUBSTR(b-rm-rctd.tag,7,2)).                                                     */
    /*                        LEAVE.                                                                                               */
    /*                    END.                                                                                                     */
    /*                END.                                                                                                         */
    /*                                                                                                                             */
    /*                ELSE LEAVE.                                                                                                  */
    /*            END.                                                                                                             */
    /*                                                                                                                             */
    /*            IF li-tag-seq EQ 0 THEN                                                                                          */
    /*            DO:                                                                                                              */
    /*                FIND LAST rm-rdtlh NO-LOCK                                                                                   */
    /*                    WHERE rm-rdtlh.company EQ cocode                                                                         */
    /*                    AND rm-rdtlh.tag     BEGINS STRING(po-ordl.po-no,"999999")                                               */
    /*                    AND INT(SUBSTR(rm-rdtlh.tag,7,2)) GT li-tag-seq                                                          */
    /*                    USE-INDEX tag NO-ERROR.                                                                                  */
    /*                IF AVAILABLE rm-rdtlh THEN li-tag-seq = INT(SUBSTR(rm-rdtlh.tag,7,2)).                                       */
    /*            END.                                                                                                             */
    /*                                                                                                                             */
    /*            ASSIGN                                                                                                           */
    /*                li-tag-seq                                        = li-tag-seq + 1                                           */
    /*                rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(po-ordl.po-no,"999999") + STRING(li-tag-seq,"99").*/
    /*        END.                                                                                                                 */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-po-job B-table-Win 
PROCEDURE display-po-job :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li-po-cnt AS INTEGER NO-UNDO.
    DEFINE VARIABLE po-list   AS cha     NO-UNDO .
    DEFINE VARIABLE v-date    AS DATE    NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        lv-rowid = ?.
        FOR EACH po-ordl
            WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
            AND po-ordl.item-type EQ YES
            AND ((po-ordl.job-no  EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) AND
            po-ordl.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})                                                                                    AND
            po-ordl.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}) OR
            adm-adding-record)
            NO-LOCK:
            ASSIGN
                li-po-cnt = li-po-cnt + 1
                lv-rowid  = ROWID(po-ordl).
            IF li-po-cnt GT 1 THEN LEAVE.
        END.
 
        IF li-po-cnt GT 1 THEN 
        DO:
            lv-rowid = ?.
            EMPTY TEMP-TABLE tt-selected.
            RUN windows/l-poitmf.w (YES, rm-rctd.company, rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name},
                YES, rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name},
                ROWID(rm-rctd), OUTPUT lv-rowid, OUTPUT po-list).
      
            IF po-list <> ""  THEN 
            DO:
                v-date = DATE(rm-rctd.rct-date:SCREEN-VALUE IN BROWSE {&browse-name}) .
                RUN dispatch ('cancel-record').
                RUN create-rcptd(po-list,v-date) .
                RETURN.
            END.
        END.

    /*
       IF li-po-cnt >= 2 THEN RUN rm/d-selpo.w (rm-rctd.company,
                                             INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}),
                                             "",
                                             0,
                                             "",
                                             OUTPUT lv-rowid
                                             ).
    END.
    ELSE DO:
        
       IF li-po-cnt >= 2 THEN RUN rm/d-selpo.w (rm-rctd.company,
                                             INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}),
                                             FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}),
                                             INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                                             rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name},
                                             OUTPUT lv-rowid
                                             ).
    END.
    */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-exact-po B-table-Win 
PROCEDURE find-exact-po :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        /* A specific item name may have been selected so find with it first */
        FIND FIRST po-ordl NO-LOCK
            WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no    EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.b-num     EQ INT(rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.i-name      EQ rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.item-type EQ YES
            USE-INDEX item-ordno NO-ERROR.
        IF NOT AVAILABLE po-ordl THEN 
        FIND FIRST po-ordl NO-LOCK
            WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no    EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.b-num     EQ INT(rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.item-type EQ YES
            USE-INDEX item-ordno NO-ERROR.            
        lv-rowid = IF AVAILABLE po-ordl THEN ROWID(po-ordl) ELSE ?.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix B-table-Win 
PROCEDURE get-matrix :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-first-disp AS LOG NO-UNDO.
  
    DEFINE VARIABLE v-len       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep       LIKE po-ordl.s-len NO-UNDO. 
    DEFINE VARIABLE v-bwt       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE lv-out-qty  LIKE rm-rctd.qty NO-UNDO.
    DEFINE VARIABLE lv-out-cost LIKE rm-rctd.cost NO-UNDO.
    DEFINE VARIABLE lv-qty-uom  LIKE rm-rctd.pur-uom NO-UNDO.
    DEFINE VARIABLE lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.
    DEFINE VARIABLE lv-cost     AS DECIMAL DECIMALS 10 NO-UNDO.

    /* gdm - 07210901 */
    DEFINE VARIABLE v-po-cost   LIKE po-ordl.cost NO-UNDO.
    DEFINE VARIABLE v-po-cuom   LIKE lv-cost-uom NO-UNDO. 
    DEFINE VARIABLE dOverPer AS DECIMAL NO-UNDO .
    DEFINE VARIABLE dConsumQty AS DECIMAL NO-UNDO .
    DEFINE VARIABLE dPoQty AS DECIMAL NO-UNDO .
    DEFINE VARIABLE dRecQty AS DECIMAL NO-UNDO .
  
    IF AVAILABLE rm-rctd THEN
        IF ip-first-disp AND
            rm-rctd.i-no NE "" THEN 
        DO: /* for row-display */
            FIND item  WHERE item.company EQ cocode                           /* no screen-value used */
                AND item.i-no  EQ rm-rctd.i-no /*:screen-value in browse {&browse-name}*/
                USE-INDEX i-no NO-ERROR.
            IF NOT AVAILABLE item THEN LEAVE.

            IF item.cons-uom EQ "" THEN item.cons-uom = rm-rctd.pur-uom.

            FIND CURRENT ITEM NO-LOCK.

            ASSIGN
                lv-qty-uom  = item.cons-uom
                lv-cost-uom = item.cons-uom
                v-dep       = item.s-dep.

            FIND FIRST po-ordl WHERE po-ordl.company = rm-rctd.company
                AND po-ordl.po-no = integer(rm-rctd.po-no)
                AND po-ordl.i-no  = rm-rctd.i-no
                AND po-ordl.job-no = rm-rctd.job-no
                AND po-ordl.job-no2 = rm-rctd.job-no2
                AND po-ordl.item-type = YES 
                AND po-ordl.s-num = rm-rctd.s-num
                AND po-ordl.b-num = rm-rctd.b-num
                NO-LOCK NO-ERROR.
            /*if not avail po-ordl then return.  */
            IF AVAILABLE po-ordl THEN 
            DO:
                ASSIGN  
                    v-len = po-ordl.s-len
                    v-wid = po-ordl.s-wid
                    v-bwt = 0
                    dOverPer = po-ordl.over-pct
                    dPoQty = po-ordl.ord-qty  . 
                    dConsumQty =  dPoQty +  (dPoQty  * ( dOverPer / 100) ) .
                {rm/pol-dims.i}
            END.
            ELSE 
            DO:
                ASSIGN 
                    lv-qty-uom  = rm-rctd.pur-uom
                    lv-cost-uom = rm-rctd.cost-uom.
                FIND FIRST job WHERE job.company EQ cocode
                    AND job.job-no  EQ rm-rctd.job-no
                    AND job.job-no2 EQ rm-rctd.job-no2
                    NO-LOCK NO-ERROR.
                IF AVAILABLE job THEN 
                DO :
                    FIND FIRST job-mat WHERE job-mat.company  EQ cocode
                        AND job-mat.job      EQ job.job
                        AND job-mat.i-no     EQ rm-rctd.i-no
                        AND job-mat.frm      EQ rm-rctd.s-num
                        AND job-mat.blank-no EQ rm-rctd.b-num
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE job-mat THEN ASSIGN v-len       = job-mat.len
                            v-wid       = job-mat.wid
                            v-bwt       = job-mat.basis-w
                            lv-out-cost = job-mat.std-cost
                            lv-cost-uom = job-mat.sc-uom.
                END.
                IF v-len EQ 0 THEN v-len = IF AVAILABLE item THEN item.s-len ELSE 0.
                IF v-wid EQ 0 THEN v-wid = IF AVAILABLE item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAILABLE item THEN item.s-wid ELSE 0.
                IF v-bwt EQ 0 THEN v-bwt = IF AVAILABLE item THEN item.basis-w ELSE 0.    
            END.
  
            /* convert qty    pr-qty-uom or po-ordl.pr-uom cons-uom*/
            /* run rm/convquom.p(rm-rctd.pur-uom,
                               po-ordl.cons-uom,
                                    v-bwt,
                                    v-len,
                                    input v-wid,
                                    input v-dep,
                                    input rm-rctd.qty,
                                    output lv-out-qty).
             
             /* convert cost pr-uom*/
             run rm/convcuom.p(rm-rctd.cost-uom, po-ordl.cons-uom,
                               v-bwt, v-len, v-wid, v-dep,
                                          rm-rctd.cost, output lv-out-cost).
             */
            /*SYS/REF/ */
            IF rm-rctd.pur-uom EQ lv-qty-uom THEN
                lv-out-qty = rm-rctd.qty.
            ELSE
                RUN custom/convquom.p (cocode,
                    rm-rctd.pur-uom,
                    lv-qty-uom,
                    v-bwt,
                    v-len,
                    INPUT v-wid,
                    INPUT v-dep,
                    INPUT rm-rctd.qty,
                    OUTPUT lv-out-qty).

           /* convert cost pr-uom*/
                /*sys/ref/convcuom.p*/
                IF rm-rctd.cost-uom EQ "L" THEN
                    lv-out-cost = rm-rctd.cost / lv-out-qty.
                ELSE
                    IF rm-rctd.cost-uom EQ lv-cost-uom THEN
                        lv-out-cost = rm-rctd.cost.
                    ELSE
                        RUN custom/convcuom.p (cocode,
                            rm-rctd.cost-uom, lv-cost-uom,                    
                            v-bwt, v-len, v-wid, v-dep,
                            rm-rctd.cost, OUTPUT lv-out-cost).
    
                IF lv-out-qty  EQ ? THEN lv-out-qty  = 0.
                IF lv-out-cost EQ ? THEN lv-out-cost = 0.
               
                 IF lRMOverrunCost AND NOT AVAIL po-ordl THEN
                    dConsumQty = lv-out-qty . 

                 IF ll-add-setup AND lv-out-qty NE 0 THEN
                           lv-out-cost = lv-out-cost + (lv-setup / lv-out-qty).
           
                IF lRMOverrunCost THEN do:
                   IF lv-out-qty GT dConsumQty   THEN DO:
                       ext-cost = ROUND(dConsumQty * lv-out-cost,2).
                   END.
                   ELSE DO:
                    ext-cost = ROUND(lv-out-qty * lv-out-cost,2).
                   END.
                END.
                ELSE do:
                     ext-cost = ROUND(lv-out-qty * lv-out-cost,2).
                END.   
        END.

        ELSE
            IF NOT ip-first-disp                                        AND
                rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN 
            DO: /* in update mode - use screen-value */
                FIND item  WHERE item.company EQ cocode
                    AND item.i-no  EQ rm-rctd.i-no:screen-value IN BROWSE {&browse-name}
                    USE-INDEX i-no NO-ERROR.
                IF NOT AVAILABLE item THEN LEAVE.

                IF item.cons-uom EQ "" THEN item.cons-uom = rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}.

                FIND CURRENT ITEM NO-LOCK.

                ASSIGN
                    lv-qty-uom  = item.cons-uom
                    lv-cost-uom = item.cons-uom
                    v-dep       = item.s-dep.

                FIND FIRST po-ordl WHERE po-ordl.company = rm-rctd.company
                    AND po-ordl.po-no = integer(rm-rctd.po-no:screen-value IN BROWSE {&browse-name})
                    AND po-ordl.i-no  = rm-rctd.i-no:screen-value
                    AND po-ordl.job-no = (rm-rctd.job-no:screen-value)
                    AND po-ordl.job-no2 = integer(rm-rctd.job-no2:screen-value)
                    AND po-ordl.item-type = YES
                    AND po-ordl.s-num = integer(rm-rctd.s-num:screen-value)
                    AND po-ordl.b-num = integer(rm-rctd.b-num:screen-value)
                    NO-LOCK NO-ERROR.
  
                IF AVAILABLE po-ordl THEN 
                DO:
                    ASSIGN  
                        v-len     = po-ordl.s-len
                        v-wid     = po-ordl.s-wid
                        v-bwt     = 0

                        /* gdm - 07210901 */
                        v-po-cost = po-ordl.cost
                        v-po-cuom = po-ordl.pr-uom
                        dOverPer = po-ordl.over-pct
                        dPoQty = po-ordl.ord-qty .
                        dConsumQty =  dPoQty +  (dPoQty  * ( dOverPer / 100) ) .

                    {rm/pol-dims.i}
                END.
                ELSE 
                DO:
                    ASSIGN 
                        lv-qty-uom  = item.cons-uom
                        lv-cost-uom = ITEM.cons-uom .
                    FIND FIRST job WHERE job.company EQ cocode
                        AND job.job-no  EQ rm-rctd.job-no:screen-value
                        AND job.job-no2 EQ integer(rm-rctd.job-no2:screen-value)
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE job THEN 
                    DO :
                        FIND FIRST job-mat WHERE job-mat.company  EQ cocode
                            AND job-mat.job      EQ job.job
                            AND job-mat.i-no     EQ rm-rctd.i-no:screen-value
                            AND job-mat.frm      EQ integer(rm-rctd.s-num:screen-value)
                            AND job-mat.blank-no EQ integer(rm-rctd.b-num:screen-value)
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE job-mat THEN ASSIGN v-len       = job-mat.len
                                v-wid       = job-mat.wid
                                v-bwt       = job-mat.basis-w
                                lv-out-cost = job-mat.std-cost
                                lv-cost-uom = job-mat.sc-uom.
                    END.
                    IF v-len EQ 0 THEN v-len = IF AVAILABLE item THEN item.s-len ELSE 0.
                    IF v-wid EQ 0 THEN v-wid = IF AVAILABLE item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAILABLE item THEN item.s-wid ELSE 0.
                    IF v-bwt EQ 0 THEN v-bwt = IF AVAILABLE item THEN item.basis-w ELSE 0.

                    /* gdm - 07210901 */
                    ASSIGN  
                        v-po-cost = lv-out-cost 
                        v-po-cuom = lv-cost-uom .
                END.
    
                /* convert qty */

                IF rm-rctd.pur-uom:screen-value IN BROWSE {&browse-name} EQ lv-qty-uom THEN
                    lv-out-qty = DEC(rm-rctd.qty:screen-value IN BROWSE {&browse-name}).
                ELSE
                    /*IF ITEM.mat-type <> "P" THEN*/
                    RUN custom/convquom.p (cocode,
                        rm-rctd.pur-uom:screen-value IN BROWSE {&browse-name} ,
                        lv-qty-uom,
                        v-bwt,
                        v-len,
                        INPUT v-wid,
                        INPUT v-dep,
                        DEC(rm-rctd.qty:screen-value IN BROWSE {&browse-name}),
                        OUTPUT lv-out-qty).
                /*ELSE
                  run custom/convquom.p (cocode,
                                         item.cons-uom ,
                                         lv-qty-uom,
                                         v-bwt,
                                         v-len,
                                         input v-wid,
                                         input v-dep,
                                         DEC(rm-rctd.qty:screen-value in browse {&browse-name}),
                                         output lv-out-qty).*/
                 
                /* convert cost */
                IF rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ "L" THEN
                    lv-out-cost = DEC(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}) / lv-out-qty .
                ELSE
                    /* gdm - 07210901 */
                    IF v-po-cuom EQ "L" THEN
                        lv-out-cost = DEC(v-po-cost) / lv-out-qty .
                    ELSE
                        IF rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ lv-cost-uom THEN
                            lv-out-cost = DEC(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}).
                        ELSE
                            RUN custom/convcuom.p (cocode,
                                rm-rctd.cost-uom:screen-value IN BROWSE {&browse-name},
                                lv-cost-uom,
                                v-bwt, v-len, v-wid, v-dep,
                                dec(rm-rctd.cost:screen-value IN BROWSE {&browse-name}),
                                OUTPUT lv-out-cost).
 
                /*
                /*add in setup charge*/
                IF lv-setup NE 0 AND lv-out-qty NE 0 AND
                  ((TRIM(rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}) NE
                  TRIM(rm-rctd.cost-uom)) OR
                  (DEC(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}) NE rm-rctd.cost)) THEN DO:
              
                  lv-out-cost = lv-out-cost + (lv-setup / lv-out-qty).
                END. */

                IF lv-out-qty  EQ ? THEN lv-out-qty  = 0.
                IF lv-out-cost EQ ? THEN lv-out-cost = 0.

                IF lRMOverrunCost AND NOT AVAIL po-ordl THEN
                    dConsumQty = lv-out-qty .
        
                 IF ll-add-setup AND lv-out-qty NE 0 THEN
                           lv-out-cost = lv-out-cost + (lv-setup / lv-out-qty).
               
                
                IF lRMOverrunCost THEN do:
                   IF lv-out-qty GT dConsumQty   THEN DO:
                      
                       ext-cost = ROUND(dConsumQty * lv-out-cost,2).
                   END.
                   ELSE DO:
                    ext-cost = ROUND(lv-out-qty * lv-out-cost,2).
                   END.
                END.
                ELSE do:
                     ext-cost = ROUND(lv-out-qty * lv-out-cost,2).
                END.   
   
                IF lv-out-cost LT 0 THEN
                    ASSIGN lv-out-cost = ABSOLUTE(-1 *  lv-out-cost) .
                ASSIGN
                    rm-rctd.cost:SCREEN-VALUE     = STRING(lv-out-cost)
                    rm-rctd.cost-uom:SCREEN-VALUE = lv-cost-uom
                    rm-rctd.qty:SCREEN-VALUE      = STRING(lv-out-qty)
                    rm-rctd.pur-uom:SCREEN-VALUE  = lv-qty-uom
                    lv-msf                        = display-msf-screen()
                    lv-adder                      = display-adder-screen().

                DISPLAY ext-cost lv-setup lv-msf lv-adder WITH BROWSE {&browse-name}.
            END.

    IF ll-add-setup THEN
        ASSIGN
            ll-add-setup   = NO
            lv-save-fld[1] = rm-rctd.cost:SCREEN-VALUE
            lv-save-fld[2] = rm-rctd.cost-uom:SCREEN-VALUE.

/* ======================================================================= */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-remain-qty B-table-Win 
PROCEDURE get-remain-qty :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER op-remain-qty AS DECIMAL NO-UNDO.

    DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.


    FIND po-ordl WHERE ROWID(po-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

    IF AVAILABLE po-ordl THEN 
    DO:
        FIND FIRST item
            WHERE item.company EQ po-ordl.company
            AND item.i-no    EQ po-ordl.i-no
            NO-LOCK NO-ERROR.

        op-remain-qty = po-ordl.cons-qty.

        RUN windows/l-poitmw.w (NO, po-ordl.company, STRING(po-ordl.po-no),
            YES, "", ROWID(rm-rctd), OUTPUT lv-rowid).

        FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(po-ordl) NO-ERROR.

        IF AVAILABLE tt-report THEN op-remain-qty = DEC(tt-report.key-01).

        IF AVAILABLE ITEM                                                                AND
            po-ordl.cons-uom NE rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name} THEN
            RUN custom/convquom.p (cocode,
                po-ordl.cons-uom,
                rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name},
                item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                op-remain-qty, OUTPUT op-remain-qty).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    ASSIGN
        v-copy-mode       = NO
        v-copy-mode-dec-1 = NO.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

/* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN
        v-copy-mode       = NO
        v-copy-mode-dec-1 = NO
        v-new-mode        = NO.

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

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN
        v-copy-mode = YES
        v-new-mode  = YES.

    IF rmrecpt-dec = 1 THEN
        v-copy-mode-dec-1 = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rno LIKE fg-rctd.r-no NO-UNDO.
    DEFINE BUFFER b-rm-rctd FOR rm-rctd.
 
    /* Code placed here will execute PRIOR to standard behavior. */
  
    FIND LAST b-rm-rctd USE-INDEX rm-rctd NO-LOCK NO-ERROR.

    IF AVAILABLE b-rm-rctd AND b-rm-rctd.r-no GT lv-rno THEN lv-rno = b-rm-rctd.r-no.

    FIND LAST rm-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAILABLE rm-rcpth AND rm-rcpth.r-no GT lv-rno THEN lv-rno = rm-rcpth.r-no.


    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

    RUN sys/ref/asiseq.p (INPUT g_company, INPUT "rm_rcpt_seq", OUTPUT lv-rno) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

    FIND FIRST tt-rm-rctd NO-ERROR.

    IF AVAILABLE tt-rm-rctd THEN 
    DO:
        BUFFER-COPY tt-rm-rctd EXCEPT rec_key TO rm-rctd.
        tt-rm-rctd.tt-rowid = ROWID(rm-rctd).
    END.

    ASSIGN
        v-new-mode        = YES
        rm-rctd.company   = cocode
        rm-rctd.r-no      = lv-rno
        rm-rctd.rita-code = "R".

    IF adm-adding-record THEN 
    DO:
        ASSIGN
           /* rm-rctd.loc                                         = gloc*/
            rm-rctd.s-num                                       = 0
            rm-rctd.s-num:screen-value IN BROWSE {&browse-name} = "0"
            rm-rctd.b-num                                       = 0
            rm-rctd.b-num:screen-value IN BROWSE {&browse-name} = "0"  
            .

        /*FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ gcompany
                              AND sys-ctrl.name    EQ "RMWHSBIN" NO-LOCK NO-ERROR.
     
        IF NOT AVAIL sys-ctrl THEN DO:
         CREATE sys-ctrl.
         ASSIGN
          sys-ctrl.company  = gcompany
          sys-ctrl.name     = "RMWHSBIN"
          sys-ctrl.descrip  = "Default Location for RM Warehouse / Bin?"
          sys-ctrl.char-fld = "RMITEM".
        END.*/

  IF v-bin NE "user entered" THEN
    ASSIGN
        rm-rctd.loc-bin = SUBSTR(v-bin,6)
        rm-rctd.loc = SUBSTR(v-bin,1,5).
    
        FIND FIRST b-rm-rctd WHERE b-rm-rctd.company = cocode
            AND b-rm-rctd.rita-code = "R"
            AND recid(b-rm-rctd) <> RECID(rm-rctd) NO-LOCK NO-ERROR.
        IF AVAILABLE b-rm-rctd AND rm-rctd.rct-date EQ ? THEN rm-rctd.rct-date = b-rm-rctd.rct-date.
        IF rm-rctd.rct-date EQ ? THEN
            rm-rctd.rct-date     = TODAY .

        DISPLAY rm-rctd.rct-date rm-rctd.loc-bin WITH BROWSE {&browse-name}.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
  
    ASSIGN
        v-copy-mode       = NO
        v-copy-mode-dec-1 = NO
        v-new-mode        = NO.

    /* WFK - don't know why rm-rctd is not available here, but it isn't */
    IF NOT AVAILABLE rm-rctd THEN
        FIND FIRST rm-rctd WHERE rm-rctd.r-no EQ
            INTEGER(rm-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name})
            EXCLUSIVE-LOCK NO-ERROR.
 
    IF NOT adm-new-record THEN 
    DO:
        {custom/askdel.i}
    END.

    RUN delete-tt.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

/* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields B-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    IF VALID-HANDLE(hd-post-child) THEN  hd-post-child:SENSITIVE = YES.
/* value assigned from local-enable-fields*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE out-hd-lst AS cha           NO-UNDO.
    DEFINE VARIABLE li         AS INTEGER       NO-UNDO.
    DEFINE VARIABLE hd-next    AS WIDGET-HANDLE NO-UNDO.


    /* Code placed here will execute PRIOR to standard behavior. */
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-target", OUTPUT out-hd-lst).
    hd-post = WIDGET-HANDLE(out-hd-lst).  /* procedure */
    IF VALID-HANDLE(WIDGET-HANDLE(out-hd-lst)) THEN 
    DO:
        hd-post-child = hd-post:CURRENT-WINDOW.    
        /*  
         do while valid-handle(hd-post-child):
            ii = ii + 1.
            hd-post-child = hd-post-child:first-child.  /* frame */
           /* if hd-post-child:type = "field-group" 
               then hd-next = hd-post-child:next-sibling.
           */
           message ii valid-handle(hd-post-child) hd-post-child:name hd-post-child:type.   
         end. 
        */ 
        hd-post-child = hd-post-child:FIRST-CHILD.  /* frame */
        hd-post-child = hd-post-child:FIRST-CHILD. /* field-group */
        hd-post-child = hd-post-child:FIRST-CHILD.  /* field */
        /*   message valid-handle(hd-post-child) hd-post-child:name hd-post-child:type.
        */
        hd-post-child:SENSITIVE = NO.
    END.
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
        {&BROWSE-NAME}:READ-ONLY = NO.

        APPLY "entry" TO rm-rctd.rct-date IN BROWSE {&browse-name}.

        DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
            APPLY 'cursor-left' TO {&BROWSE-NAME}.
        END.
    END.
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li AS INTEGER NO-UNDO.
    

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN valid-all NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

    /* create a negative receipt for NON INVENTORY items item.inv-by-cust = yes */
    FIND CURRENT ITEM NO-LOCK NO-ERROR.
    IF AVAILABLE ITEM AND ITEM.inv-by-cust AND v-new-mode THEN 
    DO:
        RUN create-neg-rctd (INPUT RECID(rm-rctd)).                       
        ASSIGN 
            v-new-mode = NO.
    END.

    RUN delete-tt.

    RUN repo-query (ROWID(rm-rctd)).

    DO WITH FRAME {&FRAME-NAME}:
        DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
            APPLY 'cursor-left' TO {&BROWSE-NAME}.
        END.
    END.

    IF rmrecpt-dec = 1 THEN
    DO:
        CASE browse-order:

            WHEN 1 THEN
                FOR EACH b-rm-rctd WHERE
                    b-rm-rctd.company EQ cocode AND
                    b-rm-rctd.rita-code = "R"
                    NO-LOCK
                    BY b-rm-rctd.r-no DESCENDING:
               
                    RUN repo-query(ROWID(b-rm-rctd)) NO-ERROR.
                    LEAVE.
                END.
            WHEN 2 THEN
                FOR EACH b-rm-rctd WHERE
                    b-rm-rctd.company EQ cocode AND
                    b-rm-rctd.rita-code = "R"
                    NO-LOCK
                    BY b-rm-rctd.tag DESCENDING
                    BY b-rm-rctd.r-no DESCENDING:

                    RUN repo-query(ROWID(b-rm-rctd)) NO-ERROR.
                    LEAVE.
                END.
            WHEN 3 THEN
                FOR EACH b-rm-rctd WHERE
                    b-rm-rctd.company EQ cocode AND
                    b-rm-rctd.rita-code = "R"
                    NO-LOCK
                    BY b-rm-rctd.rct-date DESCENDING
                    BY b-rm-rctd.r-no DESCENDING:

                    RUN repo-query(ROWID(b-rm-rctd)) NO-ERROR.
                    LEAVE.
                END.
            WHEN 4 THEN
                FOR EACH b-rm-rctd WHERE
                    b-rm-rctd.company EQ cocode AND
                    b-rm-rctd.rita-code = "R"
                    NO-LOCK
                    BY b-rm-rctd.po-no DESCENDING 
                    BY b-rm-rctd.r-no DESCENDING:

                    RUN repo-query(ROWID(b-rm-rctd)) NO-ERROR.
                    LEAVE.
                END.
            WHEN 5 THEN
                FOR EACH b-rm-rctd WHERE
                    b-rm-rctd.company EQ cocode AND
                    b-rm-rctd.rita-code = "R"
                    NO-LOCK
                    BY b-rm-rctd.i-no DESCENDING
                    BY b-rm-rctd.r-no DESCENDING:

                    RUN repo-query(ROWID(b-rm-rctd)) NO-ERROR.
                    LEAVE.
                END.
            WHEN 6 THEN
                FOR EACH b-rm-rctd WHERE
                    b-rm-rctd.company EQ cocode AND
                    b-rm-rctd.rita-code = "R"
                    NO-LOCK
                    BY b-rm-rctd.loc DESCENDING
                    BY b-rm-rctd.r-no DESCENDING:

                    RUN repo-query(ROWID(b-rm-rctd)) NO-ERROR.
                    LEAVE.
                END.
        END CASE.
    END.

    RUN auto-add-tt.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetTagSequence B-table-Win 
PROCEDURE pGetTagSequence :
/*------------------------------------------------------------------------------
     Purpose: Returns the next available tag sequence for a given PO
     Notes: This is logic for N-K TAG# Logical Value = Yes
     Usage: RUN pGetTagSequence(cocode, po-ordl.po-no, rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name})
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoNo LIKE po-ordl.po-no NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSequence AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-rm-rctd FOR rm-rctd.
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iSeq      AS INTEGER   NO-UNDO.
    
    ASSIGN
        iSeq      = 0
        cLocation = "".

    DO WHILE TRUE:
        FIND FIRST bf-rm-rctd
            WHERE bf-rm-rctd.company EQ ipcCompany
            AND bf-rm-rctd.loc     GT cLocation
            NO-LOCK NO-ERROR.

        IF AVAILABLE bf-rm-rctd THEN 
        DO:
            cLocation = bf-rm-rctd.loc.

            FOR EACH bf-rm-rctd
                WHERE bf-rm-rctd.company EQ ipcCompany
                AND bf-rm-rctd.loc     EQ cLocation
                AND bf-rm-rctd.tag     BEGINS STRING(ipiPoNo,"999999")
                USE-INDEX tag NO-LOCK
                BY bf-rm-rctd.tag DESCENDING:

                IF INT(SUBSTR(bf-rm-rctd.tag,7,2)) GT iSeq THEN
                    iSeq = INT(SUBSTR(bf-rm-rctd.tag,7,2)).
                LEAVE.
            END.
        END.

        ELSE LEAVE. /*no more locations*/
    END.

    IF iSeq GT 0 THEN 
    DO:
        FIND LAST rm-rdtlh NO-LOCK
            WHERE rm-rdtlh.company EQ ipcCompany
            AND rm-rdtlh.tag     BEGINS STRING(ipiPoNo,"999999")
            AND INT(SUBSTR(rm-rdtlh.tag,7,2)) GT iSeq
            USE-INDEX tag NO-ERROR.
        IF AVAILABLE rm-rdtlh THEN 
            iSeq = INT(SUBSTR(rm-rdtlh.tag,7,2)).
    END.

    ASSIGN
        iSeq        = iSeq + 1
        opcSequence = STRING(ipiPoNo,"999999") + STRING(iSeq,"99").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE po-cost B-table-Win 
PROCEDURE po-cost :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
     ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-cost    AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE v-len      LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid      LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep      LIKE po-ordl.s-len NO-UNDO. 
    DEFINE VARIABLE v-bwt      LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE lv-out-qty LIKE rm-rctd.qty NO-UNDO.
    DEFINE VARIABLE lv-qty-uom LIKE rm-rctd.pur-uom NO-UNDO.

    IF AVAILABLE po-ordl THEN 
    DO WITH FRAME {&FRAME-NAME}:
        IF po-ordl.i-no NE rm-rctd.i-no:screen-value IN BROWSE {&browse-name} THEN
            RUN find-exact-po.
        IF NOT AVAILABLE po-ordl THEN
            LEAVE.
      
        FIND FIRST ITEM WHERE
            item.company EQ cocode AND
            item.i-no EQ rm-rctd.i-no:screen-value IN BROWSE {&browse-name}
            USE-INDEX i-no
            NO-LOCK NO-ERROR.

        IF NOT AVAILABLE item THEN
            LEAVE.

        ASSIGN
            lv-qty-uom = po-ordl.pr-qty-uom
            v-len      = po-ordl.s-len
            v-wid      = po-ordl.s-wid
            v-bwt      = 0.

        {rm/pol-dims.i}

        IF rm-rctd.pur-uom:screen-value IN BROWSE {&browse-name} EQ lv-qty-uom THEN
            lv-out-qty = DEC(rm-rctd.qty:screen-value IN BROWSE {&browse-name}).
        ELSE
            RUN custom/convquom.p (INPUT cocode,
                INPUT rm-rctd.pur-uom:screen-value IN BROWSE {&browse-name},
                INPUT lv-qty-uom,
                INPUT v-bwt,
                INPUT v-len,
                INPUT v-wid,
                INPUT v-dep,
                INPUT DEC(rm-rctd.qty:screen-value IN BROWSE {&browse-name}),
                OUTPUT lv-out-qty).

        /*have to take into account receipt quantity when dividing setup into
          p.o. cost*/

        FIND FIRST po-ord WHERE
            po-ord.company EQ cocode AND
            po-ord.po-no EQ po-ordl.po-no
            NO-LOCK NO-ERROR.

        ll-add-setup = NO.

        IF lv-out-qty LT po-ordl.ord-qty THEN
            lv-cost = po-ordl.cost +
                (po-ordl.setup /
                ((po-ordl.t-cost - po-ordl.setup) / po-ordl.cost)).
        ELSE
            ASSIGN
                lv-cost      = po-ordl.cost
                ll-add-setup = IF AVAILABLE po-ord AND po-ord.type NE "S" THEN YES
                         ELSE NO.

        rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.pr-uom.

        RUN rm/getpocst.p (BUFFER po-ordl,
            rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name},
            INPUT-OUTPUT lv-cost).

        RUN convert-vend-comp-curr(INPUT-OUTPUT lv-cost).
        IF lv-cost LT 0 THEN lv-cost = ABSOLUTE(-1 * lv-cost) .
        rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-cost).
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
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:    
        RUN clear_auto_find.
        RUN change-order (browse-order:SCREEN-VALUE).
        REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
        RUN dispatch ("row-changed").
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
  {src/adm/template/snd-list.i "rm-rctd"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-method B-table-Win 
PROCEDURE tag-method :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-tag# AS LOG NO-UNDO.
  
    {rm/tag#.i}
    op-tag# = v-tag#.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-sequence B-table-Win 
PROCEDURE tag-sequence :
/*------------------------------------------------------------------------------
      Purpose:
      Parameters:  <none>
      Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-tag-seq AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-locode  AS cha     NO-UNDO.
    DEFINE BUFFER xrm-rctd FOR rm-rctd.

    ASSIGN
        v-tag-seq = 0
        v-locode  = "".

    DO WHILE TRUE:
        FIND FIRST xrm-rctd
            WHERE xrm-rctd.company EQ rm-rctd.company
            AND xrm-rctd.loc     GT v-locode
            NO-LOCK NO-ERROR.

        IF AVAILABLE xrm-rctd THEN
        DO:
            v-locode = xrm-rctd.loc.

            FOR EACH xrm-rctd WHERE xrm-rctd.company EQ rm-rctd.company
                AND xrm-rctd.loc     EQ v-locode
                AND xrm-rctd.tag     BEGINS string(int(rm-rctd.po-no:screen-value IN BROWSE {&browse-name}),"999999")
                USE-INDEX tag NO-LOCK
                BY xrm-rctd.tag DESCENDING:

                IF int(substr(xrm-rctd.tag,7,2)) GT v-tag-seq THEN
                    v-tag-seq = int(substr(xrm-rctd.tag,7,2)).
                LEAVE.
            END.
        END.

        ELSE LEAVE.
    END.  /* do while */
    /* ======= may not need any more
      v-locode = "".
      if v-tag-seq eq 0 then do while true:
        find first rm-rctdh where rm-rctdh.company eq rm-rcth.company
              and rm-rctdh.loc     gt v-locode
            no-lock no-error.

        if avail rm-rctdh then do:
          v-locode = rm-rctdh.loc.

          for each rm-rctdh
              where rm-rctdh.company eq cocode
                and rm-rctdh.loc     eq v-locode
                and rm-rctdh.tag     begins string(int(rm-rctd.po-no),"999999")
              use-index tag no-lock
              by rm-rctdh.tag desc:

            if int(substr(rm-rctdh.tag,7,2)) gt v-tag-seq then
              v-tag-seq = int(substr(rm-rctdh.tag,7,2)).
            leave.
          end.
        end.

        else leave.
      end.
    ============================== */
    ASSIGN
        v-tag-seq = v-tag-seq + 1
        /* rm-rctd.tag:screen-value in browse {&browse-name}
         = string(int(rm-rctd.po-no:screen-value in browse {&browse-name}),"999999") + string(v-tag-seq,"99").
          */
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-begin B-table-Win 
PROCEDURE update-begin :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE phandle  AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE char-hdl AS cha           NO-UNDO.   
    RUN get-link-handle IN adm-broker-hdl
        (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
    phandle = WIDGET-HANDLE(char-hdl).
   
    RUN new-state IN phandle ('update-begin':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-from-po-line B-table-Win 
PROCEDURE update-from-po-line :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-ord-qty LIKE po-ordl.ord-qty NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(po-ordl.s-num)
            rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(po-ordl.b-num)
            rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}  = po-ordl.pr-qty-uom
            rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.pr-uom
            lv-po-wid:SCREEN-VALUE IN BROWSE {&browse-name}        = STRING(po-ordl.s-wid)
            lv-po-len:SCREEN-VALUE IN BROWSE {&browse-name}        = STRING(po-ordl.s-len)
            ll-warned                                              = NO
            lv-setup                                               = po-ordl.setup
            lv-adder                                               = display-adder-screen().

        RUN convert-vend-comp-curr(INPUT-OUTPUT lv-setup).

        DISPLAY lv-setup lv-adder WITH BROWSE {&browse-name}.

        RUN po-cost.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-tt B-table-Win 
PROCEDURE update-tt :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&frame-name}:
        FIND FIRST tt-rm-rctd NO-ERROR.
        IF AVAILABLE tt-rm-rctd THEN 
        DO:
            lv-rowid = tt-rm-rctd.po-rowid.
            RUN display-po-info.
        /*RUN show-bin-info.*/
        END.

        /*IF li-pos EQ 1 THEN*/
        APPLY "entry" TO rm-rctd.qty IN BROWSE {&browse-name}.
    /*ELSE
      RUN dispatch ("update-record").*/
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-ttt B-table-Win 
PROCEDURE update-ttt :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    FIND FIRST tt-rm-rctd NO-ERROR.

    IF AVAILABLE tt-rm-rctd THEN 
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = tt-rm-rctd.po-no
            rm-rctd.i-no:SCREEN-VALUE  IN BROWSE {&browse-name} = tt-rm-rctd.i-no
            rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(tt-rm-rctd.qty)
            tt-rm-rctd.tt-rowid                                 = ROWID(rm-rctd).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-all B-table-Win 
PROCEDURE valid-all :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    RUN get-matrix (NO).

    RUN validate-item NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.

    RUN valid-po-no (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.

    IF NOT CAN-FIND(FIRST tt-rm-rctd WHERE tt-rowid EQ ROWID(rm-rctd)) THEN
        RUN update-ttt.

    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.

    RUN valid-job-no2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.

    RUN valid-s-num NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.

    RUN valid-b-num NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.

    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.

    DO WITH FRAME {&FRAME-NAME}:
        RUN valid-loc (rm-rctd.loc:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        RUN valid-loc-bin (rm-rctd.loc-bin:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        RUN valid-tag (rm-rctd.tag:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.
    
        RUN valid-qty (rm-rctd.qty:HANDLE IN BROWSE {&browse-name}, OUTPUT op-error).
        IF op-error THEN RETURN ERROR.
    
        RUN valid-uom (rm-rctd.pur-uom:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        RUN valid-uom (rm-rctd.cost-uom:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-b-num B-table-Win 
PROCEDURE valid-b-num :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

        IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN 
        DO:
            FIND FIRST po-ordl
                WHERE po-ordl.company   EQ rm-rctd.company
                AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.b-num     EQ INT(rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.item-type EQ YES
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE po-ordl THEN ERROR-STATUS:ERROR = YES.
        END.

        ELSE
            IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN 
            DO:
                FIND FIRST job
                    WHERE job.company EQ rm-rctd.company
                    AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                    AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                    NO-LOCK NO-ERROR.
                IF AVAILABLE job THEN
                    FIND FIRST job-mat
                        WHERE job-mat.company  EQ rm-rctd.company
                        AND job-mat.job      EQ job.job
                        AND job-mat.frm      EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND job-mat.blank-no EQ INT(rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name})
                        NO-LOCK NO-ERROR.
                IF NOT AVAILABLE job-mat THEN ERROR-STATUS:ERROR = YES.
            END.

        IF ERROR-STATUS:ERROR THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO rm-rctd.b-num IN BROWSE {&browse-name}.
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
    DEFINE VARIABLE v-msg AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ll    AS LOG       NO-UNDO.

        
    DO WITH FRAME {&FRAME-NAME}:
        rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

        IF v-msg EQ "" THEN DO:
            IF NOT CAN-FIND(FIRST item 
               WHERE item.company EQ cocode
                 AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})/*)*/ THEN DO: 
               IF lAllowRmAdd EQ YES THEN DO:
                   MESSAGE " Item does not Exist in Raw Materials File. Create New Item Code? "
                       VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
                   IF ll-ans THEN DO:
                       RUN create-item.
                       NEXT.
                   END.
                   ELSE 
                       v-msg = "Invalid entry, try help".
               END.
             ELSE DO:
                  v-msg = "Invalid entry, try help".
             END.
           END.
        END.

        IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN 
        DO:
            RELEASE po-ord.

            FIND FIRST po-ordl
                WHERE po-ordl.company   EQ rm-rctd.company
                AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                AND po-ordl.item-type EQ YES
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE po-ordl THEN v-msg = "Invalid PO Line Item, try help".
       
            ELSE
                FIND FIRST po-ord WHERE
                    po-ord.company EQ po-ordl.company AND
                    po-ord.po-no   EQ po-ordl.po-no AND
                    po-ord.TYPE    EQ "S"
                    NO-LOCK NO-ERROR.
      
            IF AVAILABLE po-ord THEN 
            DO:
                FOR EACH rm-rcpth
                    WHERE rm-rcpth.company   EQ po-ord.company
                    AND rm-rcpth.vend-no   EQ po-ord.vend-no
                    AND rm-rcpth.po-no     EQ TRIM(STRING(po-ord.po-no,">>>>>>>>>>"))
                    AND rm-rcpth.rita-code EQ "I"
                    USE-INDEX vend NO-LOCK /*,
            EACH rm-rdtlh
            WHERE rm-rdtlh.r-no             EQ rm-rcpth.r-no
              AND rm-rdtlh.rita-code        EQ rm-rcpth.rita-code
              AND SUBSTR(rm-rdtlh.BOL,1,30) EQ po-ordl.i-no
              AND SUBSTR(rm-rdtlh.BOL,31,3) EQ STRING(po-ordl.line,"999")
            NO-LOCK*/ :
                    LEAVE.
                END.
                IF NOT AVAILABLE rm-rcpth THEN v-msg = "No RM issued to this Sheet PO".
            END.

            IF v-msg EQ "" AND po-ordl.stat EQ "C" THEN v-msg = "Closed".
        END.

        ELSE
            IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN 
            DO:
                FIND FIRST job
                    WHERE job.company EQ rm-rctd.company
                    AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                    AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                    NO-LOCK NO-ERROR.
                IF AVAILABLE job THEN
                    FIND FIRST job-mat
                        WHERE job-mat.company  EQ rm-rctd.company
                        AND job-mat.job      EQ job.job
                        AND job-mat.frm      EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND job-mat.blank-no EQ INT(rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND job-mat.i-no     EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
                IF NOT AVAILABLE job-mat THEN v-msg = "This RM does not exist on Job, try help".
            END.

            ELSE 
            DO:
                FIND FIRST ITEM
                    WHERE ITEM.company EQ cocode
                    AND ITEM.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                    AND ITEM.i-code  EQ "R"
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE item THEN v-msg = "This item does not exist in RM file, try help".
            END.

        IF v-msg NE "" THEN 
        DO:
            ll = NO.
            IF v-msg EQ "Closed" THEN 
            DO:
                IF ll-warned THEN ll = YES.
                ELSE
                    MESSAGE "PO Line is closed and may be re-opened during posting, continue?..."
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                        UPDATE ll.
            END.
            ELSE MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.

            IF ll THEN ll-warned = YES.
            ELSE 
            DO:
                APPLY "entry" TO rm-rctd.i-no IN BROWSE {&browse-name}.
                RETURN ERROR.
            END.
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
 
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
            ERROR-STATUS:ERROR                                   = NO.

        IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN 
        DO:
            FIND FIRST po-ordl
                WHERE po-ordl.company   EQ rm-rctd.company
                AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                AND po-ordl.item-type EQ YES
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE po-ordl THEN ERROR-STATUS:ERROR = YES.
        END.

        ELSE
            IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN 
            DO:
                FIND FIRST job
                    WHERE job.company EQ rm-rctd.company
                    AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE job THEN ERROR-STATUS:ERROR = YES.
            END.

            ELSE
                ASSIGN
                    rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = ""
                    rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name}   = "".

        IF ERROR-STATUS:ERROR THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO rm-rctd.job-no IN BROWSE {&browse-name}.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no2 B-table-Win 
PROCEDURE valid-job-no2 :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
            ERROR-STATUS:ERROR                                   = NO.

        IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN 
        DO:
            FIND FIRST po-ordl
                WHERE po-ordl.company   EQ rm-rctd.company
                AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.item-type EQ YES
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE po-ordl THEN ERROR-STATUS:ERROR = YES.
        END.

        ELSE
            IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN 
            DO:
                FIND FIRST job
                    WHERE job.company EQ rm-rctd.company
                    AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                    AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE job THEN ERROR-STATUS:ERROR = YES. 
            END.

        IF ERROR-STATUS:ERROR THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO rm-rctd.job-no2 IN BROWSE {&browse-name}.
            RETURN ERROR.
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
    DEFINE INPUT PARAMETER ip-focus AS WIDGET-HANDLE NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST loc
            WHERE loc.company EQ cocode
            AND loc.loc     EQ ip-focus:SCREEN-VALUE)
            THEN 
        DO:
            MESSAGE "Invalid Warehouse, try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO ip-focus.
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
    DEFINE INPUT PARAMETER ip-focus AS WIDGET-HANDLE NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST rm-bin
            WHERE rm-bin.company EQ cocode
            AND rm-bin.i-no    EQ ""
            AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
            AND rm-bin.loc-bin EQ ip-focus:SCREEN-VALUE)
            THEN 
        DO:
            MESSAGE "Invalid Bin, try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO ip-focus.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no B-table-Win 
PROCEDURE valid-po-no :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-type AS INTEGER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
  
        IF NOT AVAILABLE rm-rctd THEN
            FIND FIRST rm-rctd WHERE rm-rctd.r-no EQ
                INTEGER(rm-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name})
                NO-LOCK NO-ERROR.
        IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0              AND
            NOT CAN-FIND(FIRST tt-rm-rctd WHERE tt-rm-rctd.tt-rowid EQ ROWID(rm-rctd)) THEN 
        DO:

            FIND FIRST po-ordl
                WHERE po-ordl.company   EQ cocode
                AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.item-type EQ YES
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE po-ordl THEN 
            DO:
                MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO rm-rctd.po-no IN BROWSE {&browse-name}.
                RETURN ERROR.
            END.

            FIND FIRST po-ord
                WHERE po-ord.company EQ po-ordl.company
                AND po-ord.po-no   EQ po-ordl.po-no
                NO-LOCK NO-ERROR.
                
            IF AVAILABLE po-ord AND po-ord.stat = "H" AND POHoldRct-log THEN DO: /* ticket 17372 */
              MESSAGE "Unable to receive goods or materials for a purchase order that is on hold!"
                      VIEW-AS ALERT-BOX error. 
              RETURN ERROR.
            END.
      
            IF rmrecpt-cha EQ "POPUP"         AND
                ip-type EQ 1                   AND
                AVAILABLE po-ord                   AND
                adm-adding-record              AND
                NOT CAN-FIND(FIRST tt-rm-rctd) THEN 
            DO:

                RUN fg/d-selpos.w (ROWID(po-ord), YES).

                RUN create-from-po.       

                RUN update-ttt.
            END.
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
    DEFINE INPUT PARAMETER ip-focus AS WIDGET-HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        IF DEC(ip-focus:SCREEN-VALUE) EQ 0 THEN 
        DO:
            MESSAGE "Receipt qty may not be zero..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO ip-focus.
            op-error = YES.
        END.

                {rm/chkporun.i}
                ll-qty-valid = YES.

    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-num B-table-Win 
PROCEDURE valid-s-num :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DO WITH FRAME {&FRAME-NAME}:
        rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

        IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN 
        DO:
            FIND FIRST po-ordl
                WHERE po-ordl.company   EQ rm-rctd.company
                AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.item-type EQ YES
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE po-ordl THEN ERROR-STATUS:ERROR = YES.
        END.

        ELSE
            IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN 
            DO:
                FIND FIRST job
                    WHERE job.company EQ rm-rctd.company
                    AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                    AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                    NO-LOCK NO-ERROR.
                IF AVAILABLE job THEN
                    FIND FIRST job-mat
                        WHERE job-mat.company EQ rm-rctd.company
                        AND job-mat.job     EQ job.job
                        AND job-mat.frm     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                        NO-LOCK NO-ERROR.
                IF NOT AVAILABLE job-mat THEN ERROR-STATUS:ERROR = YES.
            END.

        IF ERROR-STATUS:ERROR THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO rm-rctd.s-num IN BROWSE {&browse-name}.
            RETURN ERROR.
        END.
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
    DEFINE INPUT PARAMETER ip-focus AS WIDGET-HANDLE NO-UNDO.

    DEFINE BUFFER b-rm-rctd FOR rm-rctd.


    DO WITH FRAME {&FRAME-NAME}:
        IF rm-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND
            ip-focus:SCREEN-VALUE EQ ""                              THEN 
        DO:
            MESSAGE TRIM(ip-focus:LABEL) + " must be entered when " +
                TRIM(rm-rctd.tag2:LABEL IN BROWSE {&browse-name}) +
                " is entered..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO ip-focus.
            RETURN ERROR.
        END.

        IF rmrecpt-int EQ 1 THEN 
        DO:
            FIND FIRST loadtag WHERE loadtag.company = rm-rctd.company
                AND loadtag.item-type = YES
                AND loadtag.tag-no = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
            IF NOT AVAILABLE loadtag THEN 
            DO:
                MESSAGE "Invalid Tag#. Try help or Scan valid tag#..." VIEW-AS ALERT-BOX ERROR.
                ip-focus:SCREEN-VALUE = "".
                APPLY "entry" TO ip-focus.
                RETURN ERROR.
            END.
        END.
    
        IF v-copy-mode AND ip-focus:SCREEN-VALUE NE "" AND
            CAN-FIND(FIRST b-rm-rctd
            WHERE b-rm-rctd.company EQ cocode
            AND b-rm-rctd.tag     EQ ip-focus:SCREEN-VALUE
            AND b-rm-rctd.r-no    NE 0) THEN
        DO:
            MESSAGE "This Tag Number Has Already Been Used.  Please Enter A Unique Tag Number"
                VIEW-AS ALERT-BOX.
            APPLY "entry" TO ip-focus.
            RETURN ERROR.
        END.

        /* Check for a unique entry of a tag number when one is entered */
        IF ip-focus:SCREEN-VALUE NE  "" THEN 
        DO:
            RELEASE b-rm-rctd.
            RELEASE rm-rcpth.
      
            FIND FIRST b-rm-rctd
                WHERE b-rm-rctd.company   EQ rm-rctd.company
                AND b-rm-rctd.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                AND b-rm-rctd.tag       EQ ip-focus:SCREEN-VALUE
                AND ROWID(b-rm-rctd)    NE ROWID(rm-rctd)
                USE-INDEX tag NO-LOCK NO-ERROR.
            IF NOT AVAILABLE b-rm-rctd THEN
                FIND FIRST rm-rdtlh
                    WHERE rm-rdtlh.company EQ rm-rctd.company
                    AND rm-rdtlh.loc     EQ rm-rctd.loc
                    AND rm-rdtlh.tag     EQ ip-focus:SCREEN-VALUE
                    USE-INDEX tag NO-LOCK NO-ERROR.
            
            IF AVAILABLE rm-rdtlh THEN
                FIND FIRST rm-rcpth WHERE rm-rcpth.r-no EQ rm-rdtlh.r-no NO-LOCK NO-ERROR.
            IF AVAILABLE b-rm-rctd OR
                (AVAILABLE rm-rcpth AND (rm-rcpth.i-no NE rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} OR
                DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0)) THEN 
            DO:
                MESSAGE "This Tag Number Has Already Been Used.  Please Enter A Unique Tag Number"
                    VIEW-AS ALERT-BOX.
                APPLY "entry" TO ip-focus.
                RETURN ERROR.
            END.

            /* Check for a unique entry, 1 entry per tag per bin */
            FIND FIRST b-rm-rctd
                WHERE b-rm-rctd.r-no    EQ rm-rctd.r-no
                AND b-rm-rctd.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                AND b-rm-rctd.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                AND b-rm-rctd.tag     EQ ip-focus:SCREEN-VALUE
                AND ROWID(b-rm-rctd)  NE ROWID(rm-rctd)
                NO-LOCK NO-ERROR.
            IF AVAILABLE b-rm-rctd THEN 
            DO:
                MESSAGE "Transaction has already been entered with same Tag & Bin".
                APPLY "entry" TO ip-focus.
                RETURN ERROR.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom B-table-Win 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-focus AS WIDGET-HANDLE NO-UNDO.

    DEFINE VARIABLE lv-uom-list AS cha       INIT ["EA,TON,MSF,MSH,LB,LF"] NO-UNDO.
    DEFINE VARIABLE lv-uom-help AS CHARACTER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST item
            WHERE item.company EQ cocode
            AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            NO-LOCK NO-ERROR.

        IF AVAILABLE item THEN RUN sys/ref/uom-rm.p (INPUT item.mat-type, OUTPUT lv-uom-list).

        IF AVAILABLE item AND INDEX("MOXY789@",ITEM.mat-type) GT 0 AND
            ip-focus:NAME EQ "cost-uom"         THEN
            lv-uom-list = lv-uom-list + ",L".

        lv-uom-help = "Must enter one of the following as the UOM: " + lv-uom-list.

        IF INDEX(lv-uom-list,ip-focus:SCREEN-VALUE) LE 0 THEN 
        DO:
            MESSAGE TRIM(lv-uom-help) + "..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO ip-focus.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-item B-table-Win 
PROCEDURE validate-item :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE v-rmpostgl-char AS cha NO-UNDO.
    DO TRANSACTION:
        {sys/inc/rmpostgl.i}
    END.
    v-rmpostgl-char = sys-ctrl.char-fld.

    FIND FIRST ITEM WHERE ITEM.company = rm-rctd.company
        AND ITEM.i-no = rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

    IF  v-rmpostgl-char = "REALONLY" AND AVAILABLE ITEM AND  /* task# 05190516 */
        item.i-code = "E" AND TRIM(rm-rctd.job-no:SCREEN-VALUE) EQ ""
        THEN 
    DO:
        MESSAGE "N-K-1 RMPOSTGL is REALONLY." SKIP
            "Estimated Materials can't be received without Job#."
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rm-rctd.job-no.
        RETURN ERROR.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    RUN get-matrix (YES).
    RETURN ext-cost.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-adder B-table-Win 
FUNCTION display-adder RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-add-cost AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-cost     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-wid      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-len      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-basis-w  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-dep      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-qty-comp AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-setup    LIKE e-item-vend.setup NO-UNDO.
    DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-uom      AS CHARACTER INIT "MSF" NO-UNDO.

    DEFINE BUFFER xjob-mat FOR job-mat.
    DEFINE BUFFER ITEM-1   FOR ITEM.

    IF AVAILABLE rm-rctd THEN 
    DO:

        FIND FIRST po-ordl
            WHERE po-ordl.company   EQ cocode
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no)
            AND po-ordl.job-no    EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no))) + TRIM(rm-rctd.job-no)
            AND po-ordl.job-no2   EQ rm-rctd.job-no2
            AND po-ordl.s-num     EQ rm-rctd.s-num
            AND po-ordl.b-num     EQ rm-rctd.b-num
            AND po-ordl.i-no      EQ rm-rctd.i-no
            AND po-ordl.item-type EQ YES
            NO-LOCK NO-ERROR.

        IF AVAILABLE po-ordl THEN 
        DO:
      
            FIND FIRST po-ord WHERE
                po-ord.company EQ po-ordl.company AND
                po-ord.po-no   EQ po-ordl.po-no
                NO-LOCK NO-ERROR.

            FIND FIRST job WHERE
                job.company EQ po-ordl.company AND
                job.job-no  EQ po-ordl.job-no AND
                job.job-no2 EQ po-ordl.job-no2
                NO-LOCK NO-ERROR.
       
            IF AVAILABLE job THEN
                FIND FIRST xjob-mat WHERE
                    xjob-mat.company  EQ job.company AND
                    xjob-mat.job      EQ job.job AND
                    xjob-mat.job-no   EQ job.job-no AND
                    xjob-mat.job-no2  EQ job.job-no2 AND
                    xjob-mat.frm      EQ rm-rctd.s-num AND
                    xjob-mat.blank-no EQ rm-rctd.b-num 
                    USE-INDEX seq-idx
                    NO-LOCK NO-ERROR.
      
            IF AVAILABLE xjob-mat AND AVAILABLE po-ord THEN
                FOR EACH job-mat NO-LOCK
                    WHERE job-mat.company  EQ xjob-mat.company
                    AND job-mat.job      EQ xjob-mat.job
                    AND job-mat.frm      EQ xjob-mat.frm
                    AND job-mat.job-no   EQ xjob-mat.job-no
                    AND job-mat.job-no2  EQ xjob-mat.job-no2
                    USE-INDEX seq-idx,
         
                    FIRST item NO-LOCK
                    WHERE item.company  EQ job-mat.company
                    AND item.i-no     EQ job-mat.i-no
                    AND item.mat-type EQ "A":
         
                    FIND FIRST e-item NO-LOCK
                        WHERE e-item.company EQ rm-rctd.company
                        AND e-item.i-no    EQ rm-rctd.i-no
                        NO-ERROR.
         
                    FIND FIRST e-item-vend NO-LOCK
                        WHERE e-item-vend.company EQ item.company
                        AND e-item-vend.i-no    EQ item.i-no
                        AND e-item-vend.vend-no EQ po-ord.vend-no
                        NO-ERROR.
         
                    ASSIGN 
                        v-len = display-dimension('L')
                        v-wid = display-dimension('W').

                    FIND FIRST ITEM-1 WHERE
                        item-1.company EQ cocode AND
                        item-1.i-no EQ rm-rctd.i-no
                        NO-LOCK NO-ERROR.
    
                    IF AVAILABLE ITEM-1 THEN
                        ASSIGN
                            v-basis-w = item-1.basis-w
                            v-dep     = item-1.s-dep.

                    IF AVAILABLE e-item AND AVAILABLE e-item-vend AND po-ord.vend-no NE "" THEN 
                    DO:
                        IF rm-rctd.pur-uom EQ e-item.std-uom THEN
                            v-qty-comp = rm-rctd.qty.
                        ELSE
                            RUN custom/convquom.p (cocode,
                                rm-rctd.pur-uom, e-item.std-uom,
                                v-basis-w, v-len, v-wid, v-dep,
                                rm-rctd.qty,
                                OUTPUT v-qty-comp).
                        v-setup = 0.

                        EMPTY TEMP-TABLE tt-eiv.
                        CREATE tt-eiv.
                        DO i = 1 TO 10:
                            ASSIGN
                                tt-eiv.run-qty[i]  = e-item-vend.run-qty[i]
                                tt-eiv.run-cost[i] = e-item-vend.run-cost[i]
                                tt-eiv.setups[i]   = e-item-vend.setups[i].
                        END.
         
                        
                        IF AVAILABLE e-item-vend THEN
                        DO:                            
          
                            DO i = 1 TO 10:
                                ASSIGN
                                    tt-eiv.run-qty[i + 10]  = e-item-vend.runQtyXtra[i]
                                    tt-eiv.run-cost[i + 10] = e-item-vend.runCostXtra[i]
                                    tt-eiv.setups[i + 10]   = e-item-vend.setupsXtra[i].
                            END.
                        END.

                        DO i = 1 TO 20:
                            IF v-qty-comp LE tt-eiv.run-qty[i] THEN
                                LEAVE.
                        END.
        
                        ASSIGN
                            v-setup = tt-eiv.setups[i]
                            v-cost  = IF v-qty-comp NE 0 THEN ((tt-eiv.run-cost[i] * v-qty-comp) + v-setup) / v-qty-comp
                     ELSE 0.

                        /* This adds the Adder cost in */
                        IF e-item.std-uom NE v-uom THEN
                            RUN custom/convcuom.p (cocode,
                                e-item.std-uom, v-uom, job-mat.basis-w,
                                job-mat.len, job-mat.wid, v-dep,
                                v-cost, OUTPUT v-cost).
                    END.
        
                    ELSE 
                    DO:
                        v-cost = job-mat.std-cost.

                        IF job-mat.sc-uom NE v-uom THEN
                            RUN custom/convcuom.p (cocode,
                                job-mat.sc-uom, v-uom, job-mat.basis-w,
                                job-mat.len, job-mat.wid, v-dep,
                                job-mat.std-cost, OUTPUT v-cost).
                    END.
         
                    v-add-cost = v-add-cost + v-cost.
                END.
        END.
    END.
     
    RUN convert-vend-comp-curr(INPUT-OUTPUT v-add-cost).
    RETURN v-add-cost.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-adder-screen B-table-Win 
FUNCTION display-adder-screen RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-add-cost AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-cost     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-wid      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-len      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-basis-w  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-dep      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-qty-comp AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-setup    LIKE e-item-vend.setup NO-UNDO.
    DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-uom      AS CHARACTER INIT "MSF" NO-UNDO.

    DEFINE BUFFER xjob-mat FOR job-mat.
    DEFINE BUFFER ITEM-1   FOR ITEM.

    IF AVAILABLE rm-rctd THEN 
    DO:

        FIND FIRST po-ordl
            WHERE po-ordl.company   EQ cocode
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no    EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.b-num     EQ INT(rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.item-type EQ YES
            NO-LOCK NO-ERROR.

        IF AVAILABLE po-ordl THEN 
        DO:
      
            FIND FIRST po-ord WHERE
                po-ord.company EQ po-ordl.company AND
                po-ord.po-no   EQ po-ordl.po-no
                NO-LOCK NO-ERROR.

            FIND FIRST job WHERE
                job.company EQ po-ordl.company AND
                job.job-no  EQ po-ordl.job-no AND
                job.job-no2 EQ po-ordl.job-no2
                NO-LOCK NO-ERROR.
       
            IF AVAILABLE job THEN
                FIND FIRST xjob-mat WHERE
                    xjob-mat.company  EQ job.company AND
                    xjob-mat.job      EQ job.job AND
                    xjob-mat.job-no   EQ job.job-no AND
                    xjob-mat.job-no2  EQ job.job-no2 AND
                    xjob-mat.frm      EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name}) AND
                    xjob-mat.blank-no EQ INT(rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name}) 
                    USE-INDEX seq-idx
                    NO-LOCK NO-ERROR.
      
            IF AVAILABLE xjob-mat AND AVAILABLE po-ord THEN
                FOR EACH job-mat NO-LOCK
                    WHERE job-mat.company  EQ xjob-mat.company
                    AND job-mat.job      EQ xjob-mat.job
                    AND job-mat.frm      EQ xjob-mat.frm
                    AND job-mat.job-no   EQ xjob-mat.job-no
                    AND job-mat.job-no2  EQ xjob-mat.job-no2
                    USE-INDEX seq-idx,
         
                    FIRST item NO-LOCK
                    WHERE item.company  EQ job-mat.company
                    AND item.i-no     EQ job-mat.i-no
                    AND item.mat-type EQ "A":
         
                    FIND FIRST e-item NO-LOCK
                        WHERE e-item.company EQ rm-rctd.company
                        AND e-item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-ERROR.
         
                    FIND FIRST e-item-vend NO-LOCK
                        WHERE e-item-vend.company EQ item.company
                        AND e-item-vend.i-no    EQ item.i-no
                        AND e-item-vend.vend-no EQ po-ord.vend-no
                        NO-ERROR.
         
                    ASSIGN 
                        v-len = display-dimension-screen('L')
                        v-wid = display-dimension-screen('W').

                    FIND FIRST ITEM-1 WHERE
                        item-1.company EQ cocode AND
                        item-1.i-no EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
    
                    IF AVAILABLE ITEM-1 THEN
                        ASSIGN
                            v-basis-w = item-1.basis-w
                            v-dep     = item-1.s-dep.

                    IF AVAILABLE e-item AND AVAILABLE e-item-vend AND po-ord.vend-no NE "" THEN 
                    DO:
                        IF rm-rctd.pur-uom:SCREEN-VALUE EQ e-item.std-uom THEN
                            v-qty-comp = DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}).
                        ELSE
                            RUN custom/convquom.p (cocode,
                                rm-rctd.pur-uom:SCREEN-VALUE, e-item.std-uom,
                                v-basis-w, v-len, v-wid, v-dep,
                                DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}),
                                OUTPUT v-qty-comp).
            
        
                        v-setup = 0.
                        EMPTY TEMP-TABLE tt-eiv.
                        CREATE tt-eiv.
                        DO i = 1 TO 10:
                            ASSIGN
                                tt-eiv.run-qty[i]  = e-item-vend.run-qty[i]
                                tt-eiv.run-cost[i] = e-item-vend.run-cost[i]
                                tt-eiv.setups[i]   = e-item-vend.setups[i].
                        END.
          
                        
          
                        IF AVAILABLE e-item-vend THEN
                        DO:                            
          
                            DO i = 1 TO 10:
                                ASSIGN
                                    tt-eiv.run-qty[i + 10]  = e-item-vend.runQtyXtra[i]
                                    tt-eiv.run-cost[i + 10] = e-item-vend.runCostXtra[i]
                                    tt-eiv.setups[i + 10]   = e-item-vend.setupsXtra[i].
                            END.
                        END.

                        DO i = 1 TO 20:
                            IF v-qty-comp LE tt-eiv.run-qty[i] THEN
                                LEAVE.
                        END.
        
                        ASSIGN
                            v-setup = tt-eiv.setups[i]
                            v-cost  = IF v-qty-comp NE 0 THEN ((tt-eiv.run-cost[i] * v-qty-comp) + v-setup) / v-qty-comp
                     ELSE 0.

                        /* This adds the Adder cost in */
                        IF e-item.std-uom NE v-uom THEN
                            RUN custom/convcuom.p (cocode,
                                e-item.std-uom, v-uom, job-mat.basis-w,
                                job-mat.len, job-mat.wid, v-dep,
                                v-cost, OUTPUT v-cost).
                    END.
        
                    ELSE 
                    DO:
                        v-cost = job-mat.std-cost.

                        IF job-mat.sc-uom NE v-uom THEN
                            RUN custom/convcuom.p (cocode,
                                job-mat.sc-uom, v-uom, job-mat.basis-w,
                                job-mat.len, job-mat.wid, v-dep,
                                job-mat.std-cost, OUTPUT v-cost).
                    END.
         
                    v-add-cost = v-add-cost + v-cost.
                END.
        END.
    END.
     
    RUN convert-vend-comp-curr(INPUT-OUTPUT v-add-cost).
    RETURN v-add-cost.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-dimension B-table-Win 
FUNCTION display-dimension RETURNS DECIMAL
    ( INPUT ip-dim AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  from rcptdims.v  
        Notes:  
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE ld-dim    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-wid-num AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-len-num AS DECIMAL NO-UNDO.
    DEFINE BUFFER b-jm FOR job-mat.

    IF AVAILABLE rm-rctd THEN 
    DO:
        FIND FIRST ITEM
            WHERE ITEM.company EQ cocode
            AND ITEM.i-no    EQ rm-rctd.i-no
            AND ITEM.i-code  EQ "R"
            NO-LOCK NO-ERROR.
        IF AVAILABLE ITEM THEN
            ASSIGN
                v-wid-num = IF ITEM.r-wid NE 0 THEN ITEM.r-wid ELSE ITEM.s-wid
                v-len-num = ITEM.s-len.

        IF rm-rctd.po-no <> "" THEN 
        DO:
            FIND FIRST po-ordl WHERE po-ordl.company   EQ cocode
                AND po-ordl.po-no     EQ int(rm-rctd.po-no)
                AND po-ordl.i-no      EQ rm-rctd.i-no
                AND po-ordl.job-no    EQ rm-rctd.job-no
                AND po-ordl.job-no2   EQ rm-rctd.job-no2
                AND po-ordl.item-type EQ YES
                AND po-ordl.s-num     EQ rm-rctd.s-num
                NO-LOCK NO-ERROR.
            IF AVAILABLE po-ordl THEN
                ASSIGN  v-wid-num = po-ordl.s-wid
                    v-len-num = po-ordl.s-len.
            ELSE 
            DO:
                IF rm-rctd.job-no NE "" THEN
                    FIND FIRST b-jm WHERE b-jm.company EQ cocode
                        AND b-jm.rm-i-no EQ rm-rctd.i-no
                        AND b-jm.job-no  EQ rm-rctd.job-no
                        AND b-jm.job-no2 EQ rm-rctd.job-no2
                        AND b-jm.frm     EQ rm-rctd.s-num
                        NO-LOCK NO-ERROR.
                IF AVAILABLE b-jm THEN ASSIGN v-wid-num = b-jm.wid
                        v-len-num = b-jm.len.
                ELSE 
                DO:
                    FIND FIRST ITEM WHERE item.company EQ cocode
                        AND item.i-no    EQ rm-rctd.i-no
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE item THEN
                        IF item.r-wid EQ 0 THEN
                            ASSIGN v-wid-num = item.s-wid
                                v-len-num = item.s-len.
                        ELSE ASSIGN v-wid-num = item.r-wid
                                v-len-num = 12.
                END.
            END.
        END.
      
        IF ip-dim = "W" THEN ld-dim = v-wid-num.
        ELSE IF ip-dim = "L" THEN ld-dim = v-len-num.
    END.
  
    RETURN ld-dim.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-dimension-screen B-table-Win 
FUNCTION display-dimension-screen RETURNS DECIMAL
    ( INPUT ip-dim AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  from rcptdims.v  
        Notes:  
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE ld-dim    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-wid-num AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-len-num AS DECIMAL NO-UNDO.
    DEFINE BUFFER b-jm FOR job-mat.

    IF AVAILABLE rm-rctd THEN 
    DO:
        FIND FIRST ITEM
            WHERE ITEM.company EQ cocode
            AND ITEM.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND ITEM.i-code  EQ "R"
            NO-LOCK NO-ERROR.
        IF AVAILABLE ITEM THEN
            ASSIGN
                v-wid-num = IF ITEM.r-wid NE 0 THEN ITEM.r-wid ELSE ITEM.s-wid
                v-len-num = ITEM.s-len.

        IF rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN 
        DO:
            FIND FIRST po-ordl WHERE po-ordl.company   EQ cocode
                AND po-ordl.po-no     EQ int(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                AND po-ordl.job-no    EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                AND po-ordl.item-type EQ YES
                AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                NO-LOCK NO-ERROR.
            IF AVAILABLE po-ordl THEN
                ASSIGN  v-wid-num = po-ordl.s-wid
                    v-len-num = po-ordl.s-len.
            ELSE 
            DO:
                IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
                    FIND FIRST b-jm WHERE b-jm.company EQ cocode
                        AND b-jm.rm-i-no EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                        AND b-jm.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                        AND b-jm.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND b-jm.frm     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
                        NO-LOCK NO-ERROR.
                IF AVAILABLE b-jm THEN ASSIGN v-wid-num = b-jm.wid
                        v-len-num = b-jm.len.
                ELSE 
                DO:
                    FIND FIRST ITEM WHERE item.company EQ cocode
                        AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE item THEN
                        IF item.r-wid EQ 0 THEN
                            ASSIGN v-wid-num = item.s-wid
                                v-len-num = item.s-len.
                        ELSE ASSIGN v-wid-num = item.r-wid
                                v-len-num = 12.
                END.
            END.
        END.
      
        IF ip-dim = "W" THEN ld-dim = v-wid-num.
        ELSE IF ip-dim = "L" THEN ld-dim = v-len-num.
    END.
  
    RETURN ld-dim.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-msf B-table-Win 
FUNCTION display-msf RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-msf    AS DECIMAL DECIMALS 3 NO-UNDO.
    DEFINE VARIABLE v-wid     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-len     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-basis-w AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-out-qty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-dep     AS DECIMAL NO-UNDO.

    IF AVAILABLE rm-rctd THEN 
    DO:
  
        ASSIGN 
            v-len = display-dimension('L')
            v-wid = display-dimension('W').

        FIND FIRST po-ordl
            WHERE po-ordl.company   EQ cocode
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no)
            AND po-ordl.job-no    EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no))) + TRIM(rm-rctd.job-no)
            AND po-ordl.job-no2   EQ rm-rctd.job-no2
            AND po-ordl.s-num     EQ rm-rctd.s-num
            AND po-ordl.b-num     EQ rm-rctd.b-num
            AND po-ordl.i-no      EQ rm-rctd.i-no
            AND po-ordl.item-type EQ YES
            NO-LOCK NO-ERROR.
   
        IF AVAILABLE po-ordl THEN 
        DO:
            IF rm-rctd.pur-uom EQ "EA" /*OR
         (NOT po-ordl.item-type AND
          LOOKUP(rm-rctd.pur-uom,fg-uom-list) GT 0)*/ THEN
                lv-msf = IF v-corr THEN ((v-len * v-wid * .007 * rm-rctd.qty) / 1000)
                ELSE ((v-len * v-wid) / 144) * (rm-rctd.qty / 1000).
            ELSE 
            DO:
                /*convert whatever the UOM is into "EACH" first*/
        
                FIND FIRST ITEM WHERE
                    item.company EQ cocode AND
                    item.i-no EQ rm-rctd.i-no
                    NO-LOCK NO-ERROR.
    
                IF AVAILABLE ITEM THEN
                    ASSIGN
                        v-basis-w = item.basis-w
                        v-dep     = item.s-dep.
   
                IF rm-rctd.pur-uom NE "EA" THEN 
                DO:
                    RUN custom/convquom.p (cocode,
                        rm-rctd.pur-uom,
                        "EA",
                        v-basis-w,
                        v-len,
                        v-wid,
                        v-dep,
                        rm-rctd.qty,
                        OUTPUT v-out-qty).
     
                    /*now convert from "EACH" into MSF*/   
                    lv-msf = IF v-corr
                        THEN
                        ((v-len * v-wid * .007 * v-out-qty) / 1000)
                        ELSE
                        ((((v-len * v-wid) / 144) * v-out-qty) / 1000).
                    IF rm-rctd.pur-uom EQ "ROLL" THEN
                        lv-msf = lv-msf * (12 / v-len).
                END. 
            END.
        END.
    END.
    RETURN lv-msf.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-msf-screen B-table-Win 
FUNCTION display-msf-screen RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-msf    AS DECIMAL DECIMALS 3 NO-UNDO.
    DEFINE VARIABLE v-wid     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-len     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-basis-w AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-out-qty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-dep     AS DECIMAL NO-UNDO.

    IF AVAILABLE rm-rctd THEN 
    DO:
  
        ASSIGN 
            v-len = display-dimension-screen('L')
            v-wid = display-dimension-screen('W').

        FIND FIRST po-ordl
            WHERE po-ordl.company   EQ cocode
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no    EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.b-num     EQ INT(rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.item-type EQ YES
            NO-LOCK NO-ERROR.
   
        IF AVAILABLE po-ordl THEN 
        DO:
            IF rm-rctd.pur-uom:screen-value IN BROWSE {&browse-name} EQ "EA" /*OR
         (NOT po-ordl.item-type AND
          LOOKUP(rm-rctd.pur-uom:screen-value in BROWSE {&browse-name},fg-uom-list) GT 0)*/ THEN
                lv-msf = IF v-corr THEN ((v-len * v-wid * .007 * DEC(rm-rctd.qty:screen-value IN BROWSE {&browse-name})) / 1000)
                ELSE ((v-len * v-wid) / 144) * (DEC(rm-rctd.qty:screen-value IN BROWSE {&browse-name}) / 1000).
            ELSE 
            DO:
                /*convert whatever the UOM is into "EACH" first*/
        
                FIND FIRST ITEM WHERE
                    item.company EQ cocode AND
                    item.i-no EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                    NO-LOCK NO-ERROR.
    
                IF AVAILABLE ITEM THEN
                    ASSIGN
                        v-basis-w = item.basis-w
                        v-dep     = item.s-dep.
   
                IF rm-rctd.pur-uom:screen-value IN BROWSE {&browse-name} NE "EA" THEN 
                DO:
                    RUN custom/convquom.p (cocode,
                        rm-rctd.pur-uom:screen-value IN BROWSE {&browse-name},
                        "EA",
                        v-basis-w,
                        v-len,
                        v-wid,
                        v-dep,
                        DEC(rm-rctd.qty:screen-value IN BROWSE {&browse-name}),
                        OUTPUT v-out-qty).
     
                    /*now convert from "EACH" into MSF*/   
                    lv-msf = IF v-corr
                        THEN
                        ((v-len * v-wid * .007 * v-out-qty) / 1000)
                        ELSE
                        ((((v-len * v-wid) / 144) * v-out-qty) / 1000).
                    IF rm-rctd.pur-uom:screen-value IN BROWSE {&browse-name} EQ "ROLL" THEN
                        lv-msf = lv-msf * (12 / v-len).
                END. 
            END.
        END.
    END.
    RETURN lv-msf.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-setup B-table-Win 
FUNCTION display-setup RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ld-setup AS DECIMAL NO-UNDO.
  
    IF AVAILABLE rm-rctd AND
        rm-rctd.po-no NE "" THEN 
    DO:
    
        FIND FIRST po-ordl
            WHERE po-ordl.company   EQ cocode
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no)
            AND po-ordl.job-no    EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no))) + TRIM(rm-rctd.job-no)
            AND po-ordl.job-no2   EQ rm-rctd.job-no2
            AND po-ordl.s-num     EQ rm-rctd.s-num
            AND po-ordl.b-num     EQ rm-rctd.b-num
            AND po-ordl.i-no      EQ rm-rctd.i-no
            AND po-ordl.item-type EQ YES
            NO-LOCK NO-ERROR.

        IF AVAILABLE po-ordl THEN
        DO:
            ld-setup = po-ordl.setup.

            RUN convert-vend-comp-curr(INPUT-OUTPUT ld-setup).
        END.
    END.

    RETURN ld-setup.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-setup-screen B-table-Win 
FUNCTION display-setup-screen RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ld-setup AS DECIMAL NO-UNDO.
  
    IF AVAILABLE rm-rctd AND
        rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN 
    DO:
    
        FIND FIRST po-ordl
            WHERE po-ordl.company   EQ cocode
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no    EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) + TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.job-no2   EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.s-num     EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.b-num     EQ INT(rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.item-type EQ YES
            NO-LOCK NO-ERROR.

        IF AVAILABLE po-ordl THEN
        DO:
            ld-setup = po-ordl.setup.
            RUN convert-vend-comp-curr(INPUT-OUTPUT ld-setup).
        END.
    END.

    RETURN ld-setup.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

