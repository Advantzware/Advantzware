&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  fg\b-rcptd.w

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

&SCOPED-DEFINE yellowColumnsName b-rcptd-fg
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE useMatches YES
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.

DEFINE VARIABLE poSelected AS INTEGER NO-UNDO.
DEF VAR ll-help-run AS LOG NO-UNDO.  /* set on browse help, reset row-entry */
DEF VAR ls-prev-po AS cha NO-UNDO.
DEF VAR lv-overrun-checked AS LOG NO-UNDO.
DEF VAR lv-closed-checked AS LOG NO-UNDO.
DEF VAR lv-job-no AS CHAR NO-UNDO.
DEF VAR lv-job-no2 AS CHAR NO-UNDO.

DEF BUFFER b-fg-rctd FOR fg-rctd.  /* for tag validation */
DEF BUFFER b-fg-rdtlh FOR fg-rdtlh. /* for tag validation */
DEF BUFFER b2-fg-rdtlh FOR fg-rdtlh. /* for tag validation */
DEF BUFFER b-tag-rctd FOR fg-rctd .   /* task 11111306 */
DEF BUFFER reftable-job FOR reftable.
DEF BUFFER b-po-ord FOR po-ord.
DEF BUFFER b-company FOR company.

DEF VAR lv-prev-job2 AS cha NO-UNDO.
DEF VAR lv-new-job-ran AS LOG NO-UNDO.
DEF VAR ll-qty-case-ent AS LOG NO-UNDO.
DEF VAR lv-num-rec AS INT NO-UNDO.
DEF VAR fg-uom-list  AS CHAR NO-UNDO.
DEF VAR lv-frst-rno LIKE fg-rctd.r-no NO-UNDO.
DEF VAR lv-rct-date-checked AS LOG NO-UNDO.
DEF VAR ll-set-parts AS LOG NO-UNDO.
DEF VAR lv-linker LIKE fg-rcpts.linker NO-UNDO.
DEF VAR trans-time AS CHAR NO-UNDO.
DEF VAR v-copy-mode AS LOG NO-UNDO.
DEF VAR v-auto-add-tag AS LOG NO-UNDO.
DEF VAR lrMissingRow AS ROWID NO-UNDO.
DEF VAR lv-cost-basis AS CHAR NO-UNDO.
DEF VAR gvcCurrentItem AS CHAR NO-UNDO.

{fg/d-selpos.i NEW}


DEF TEMP-TABLE tt-fg-rctd LIKE fg-rctd
    FIELD tt-rowid AS ROWID
    FIELD po-rowid AS ROWID.

DEF BUFFER b-tt FOR tt-fg-rctd.
DEFINE TEMP-TABLE w-rowid FIELD w-rowid AS CHAR
          INDEX w-rowid IS PRIMARY w-rowid.
{pc/pcprdd4u.i NEW}

{fg/invrecpt.i NEW}
{oe/d-selbin.i NEW}
DO TRANSACTION:
  {sys/inc/fgpofrt.i}
  {sys/inc/fgrecpt.i}
  {sys/inc/autopost.i}
  {sys/inc/fgsetrec.i}
  {sys/inc/oeship.i}
  {sys/inc/fgsecur.i}
  {sys/inc/rfidtag.i}
  {sys/inc/relmerge.i}
  {sys/inc/POHoldReceipts.i}  /* ticket 17372 */
END.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).
{fg/fullset.i NEW}
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "FGPOTAG#"
    NO-LOCK NO-ERROR.
v-auto-add-tag = NO.
IF AVAIL sys-ctrl THEN
  v-auto-add-tag = sys-ctrl.log-fld.

&SCOPED-DEFINE item-key-phrase TRUE
&SCOPED-DEFINE init-proc init-proc

/* gdm - 11160901 */
DO TRANSACTION:
    {sys/inc/jobreopn.i}
END.

DEF VAR lvReturnChar AS CHAR NO-UNDO.
DEF VAR lvFound AS LOG NO-UNDO.
DEF VAR fgRecptPassWord-log AS LOGICAL NO-UNDO.
DEF VAR fgRecptPassWord-char AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (cocode, "FGRecptPassWord", "L", NO, NO, "", "", 
    OUTPUT lvReturnChar, OUTPUT lvFound).
IF lvFound THEN
    fgRecptPassWord-log = LOGICAL(lvReturnChar).
RUN sys/ref/nk1look.p (cocode, "FGRecptPassWord", "C", NO, NO, "", "", 
    OUTPUT fgRecptPassWord-char, OUTPUT lvFound).


DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lFGSetAssembly AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cFGSetAssembly AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lGetBin AS LOGICAL     NO-UNDO.
RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "FGSetAssembly",
                       INPUT "L",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT cFGSetAssembly,
                       OUTPUT lFound).
IF lFound THEN
    lFGSetAssembly = cFGSetAssembly EQ "YES".
RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "FGSetAssembly",
                       INPUT "C",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT cFGSetAssembly,
                       OUTPUT lFound).

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
&Scoped-define INTERNAL-TABLES fg-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table fg-rctd.r-no fg-rctd.rct-date ~
STRING(fg-rctd.trans-time,'HH:MM') @ trans-time fg-rctd.tag fg-rctd.po-no ~
fg-rctd.job-no fg-rctd.job-no2 fg-rctd.i-no fg-rctd.i-name fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.cases fg-rctd.qty-case fg-rctd.cases-unit ~
fg-rctd.partial fg-rctd.std-cost fg-rctd.cost-uom fg-rctd.t-qty ~
fg-rctd.frt-cost fg-rctd.ext-cost fg-rctd.stack-code fg-rctd.tot-wt ~
fg-rctd.created-by fg-rctd.updated-by 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table fg-rctd.rct-date ~
fg-rctd.tag fg-rctd.po-no fg-rctd.job-no fg-rctd.job-no2 fg-rctd.i-no ~
fg-rctd.i-name fg-rctd.loc fg-rctd.loc-bin fg-rctd.cases fg-rctd.qty-case ~
fg-rctd.cases-unit fg-rctd.partial fg-rctd.std-cost fg-rctd.cost-uom ~
fg-rctd.frt-cost fg-rctd.stack-code 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table fg-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company eq cocode and ~
fg-rctd.r-no ge lv-frst-rno and ~
(fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") ~
AND (fg-rctd.SetHeaderRno EQ INTEGER(SUBSTRING(lv-linker, 10, 10)) OR (NOT ll-set-parts AND fg-rctd.SetHeaderRno EQ 0)) ~
use-index fg-rctd NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company eq cocode and ~
fg-rctd.r-no ge lv-frst-rno and ~
(fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") ~
AND (fg-rctd.SetHeaderRno EQ INTEGER(SUBSTRING(lv-linker, 10, 10)) OR (NOT ll-set-parts AND fg-rctd.SetHeaderRno EQ 0)) ~
use-index fg-rctd NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table fg-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-1 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order fi_sortby auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD maxComponentQty B-table-Win 
FUNCTION maxComponentQty RETURNS DECIMAL
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
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 117.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 92 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 2.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      fg-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      fg-rctd.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>>":U WIDTH 12
            LABEL-BGCOLOR 14
      fg-rctd.rct-date COLUMN-LABEL "Receipt!Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      STRING(fg-rctd.trans-time,'HH:MM') @ trans-time COLUMN-LABEL "Receipt!Time"
            WIDTH 10
      fg-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U WIDTH 29
            LABEL-BGCOLOR 14
      fg-rctd.po-no FORMAT "x(9)":U WIDTH 14 LABEL-BGCOLOR 14
      fg-rctd.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U WIDTH 8
            LABEL-BGCOLOR 14
      fg-rctd.job-no2 FORMAT "99":U
      fg-rctd.i-no COLUMN-LABEL "Item" FORMAT "X(15)":U WIDTH 22
            LABEL-BGCOLOR 14
      fg-rctd.i-name COLUMN-LABEL "Name/Desc" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      fg-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U WIDTH 8 LABEL-BGCOLOR 14
      fg-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U WIDTH 14
            LABEL-BGCOLOR 14
      fg-rctd.cases COLUMN-LABEL "Units" FORMAT "->>>,>>9":U LABEL-BGCOLOR 14
      fg-rctd.qty-case COLUMN-LABEL "Unit!Count" FORMAT ">>>,>>9":U
            LABEL-BGCOLOR 14
      fg-rctd.cases-unit COLUMN-LABEL "Units/!Skid" FORMAT ">>>9":U
            LABEL-BGCOLOR 14
      fg-rctd.partial COLUMN-LABEL "Partial" FORMAT "->>>,>>9":U
            LABEL-BGCOLOR 14
      fg-rctd.std-cost COLUMN-LABEL "Cost/UOM" FORMAT ">,>>>,>>9.99<<<<":U
            WIDTH 18 LABEL-BGCOLOR 14
      fg-rctd.cost-uom COLUMN-LABEL "UOM" FORMAT "x(3)":U LABEL-BGCOLOR 14
      fg-rctd.t-qty COLUMN-LABEL "Total!Qty" FORMAT "->>>,>>>,>>9.99":U
            LABEL-BGCOLOR 14
      fg-rctd.frt-cost COLUMN-LABEL "Freight Cost" FORMAT ">>>,>>9.99<<":U
            WIDTH 18 LABEL-BGCOLOR 14
      fg-rctd.ext-cost COLUMN-LABEL "Extended Cost" FORMAT "->,>>>,>>9.99":U
            WIDTH 18 LABEL-BGCOLOR 14
      fg-rctd.stack-code COLUMN-LABEL "FG Lot#" FORMAT "X(20)":U
            WIDTH 21.8 LABEL-BGCOLOR 14
      fg-rctd.tot-wt FORMAT ">>,>>9.99":U
      fg-rctd.created-by COLUMN-LABEL "Created By" FORMAT "x(8)":U
            LABEL-BGCOLOR 14
      fg-rctd.updated-by COLUMN-LABEL "Last Updated By" FORMAT "x(8)":U
            LABEL-BGCOLOR 14
  ENABLE
      fg-rctd.rct-date
      fg-rctd.tag
      fg-rctd.po-no
      fg-rctd.job-no
      fg-rctd.job-no2
      fg-rctd.i-no HELP "FG Item Number"
      fg-rctd.i-name
      fg-rctd.loc
      fg-rctd.loc-bin
      fg-rctd.cases
      fg-rctd.qty-case
      fg-rctd.cases-unit
      fg-rctd.partial
      fg-rctd.std-cost
      fg-rctd.cost-uom
      fg-rctd.frt-cost
      fg-rctd.stack-code
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 14.52
         FONT 2 ROW-HEIGHT-CHARS .67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 15.76 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 15.76 COL 96 COLON-ALIGNED NO-LABEL
     auto_find AT ROW 16.95 COL 11 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 16.95 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 15.76 COL 2
     RECT-1 AT ROW 15.52 COL 1
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
     _TblList          = "ASI.fg-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _Where[1]         = "fg-rctd.company eq cocode and
fg-rctd.r-no ge lv-frst-rno and
(fg-rctd.rita-code eq ""R"" or fg-rctd.rita-code eq ""E"")
AND (fg-rctd.SetHeaderRno EQ INTEGER(SUBSTRING(lv-linker, 10, 10)) OR (NOT ll-set-parts AND fg-rctd.SetHeaderRno EQ 0))
use-index fg-rctd"
     _FldNameList[1]   > ASI.fg-rctd.r-no
"fg-rctd.r-no" "Seq#" ">>>>>>>>" "integer" ? ? ? 14 ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.fg-rctd.rct-date
"fg-rctd.rct-date" "Receipt!Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"STRING(fg-rctd.trans-time,'HH:MM') @ trans-time" "Receipt!Time" ? ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.fg-rctd.tag
"fg-rctd.tag" "Tag#" "x(20)" "character" ? ? ? 14 ? ? yes ? no no "29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.fg-rctd.po-no
"fg-rctd.po-no" ? ? "character" ? ? ? 14 ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.fg-rctd.job-no
"fg-rctd.job-no" "Job#" ? "character" ? ? ? 14 ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.fg-rctd.job-no2
"fg-rctd.job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.fg-rctd.i-no
"fg-rctd.i-no" "Item" "X(15)" "character" ? ? ? 14 ? ? yes "FG Item Number" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.fg-rctd.i-name
"fg-rctd.i-name" "Name/Desc" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.fg-rctd.loc
"fg-rctd.loc" "Whse" ? "character" ? ? ? 14 ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.fg-rctd.loc-bin
"fg-rctd.loc-bin" "Bin" ? "character" ? ? ? 14 ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.fg-rctd.cases
"fg-rctd.cases" "Units" "->>>,>>9" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.fg-rctd.qty-case
"fg-rctd.qty-case" "Unit!Count" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.fg-rctd.cases-unit
"fg-rctd.cases-unit" "Units/!Skid" ">>>9" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.fg-rctd.partial
"fg-rctd.partial" "Partial" "->>>,>>9" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.fg-rctd.std-cost
"fg-rctd.std-cost" "Cost/UOM" ">,>>>,>>9.99<<<<" "decimal" ? ? ? 14 ? ? yes ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.fg-rctd.cost-uom
"fg-rctd.cost-uom" "UOM" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.fg-rctd.t-qty
"fg-rctd.t-qty" "Total!Qty" ? "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.fg-rctd.frt-cost
"fg-rctd.frt-cost" "Freight Cost" ? "decimal" ? ? ? 14 ? ? yes ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.fg-rctd.ext-cost
"fg-rctd.ext-cost" "Extended Cost" "->,>>>,>>9.99" "decimal" ? ? ? 14 ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.fg-rctd.stack-code
"fg-rctd.stack-code" "FG Lot#" "X(20)" "character" ? ? ? 14 ? ? yes ? no no "21.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   = ASI.fg-rctd.tot-wt
     _FldNameList[23]   > ASI.fg-rctd.created-by
"fg-rctd.created-by" "Created By" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > ASI.fg-rctd.updated-by
"fg-rctd.updated-by" "Last Updated By" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON CURSOR-DOWN OF Browser-Table IN FRAME F-Main
DO:
  RUN get-matrix (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
    DEF VAR phandle AS WIDGET-HANDLE NO-UNDO.
   DEF VAR char-hdl AS cha NO-UNDO.   
   RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
   phandle = WIDGET-HANDLE(char-hdl).
   
   RUN new-state IN phandle ('update-begin':U).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO: 
   DEF VAR ll-tag# AS LOG NO-UNDO.
   DEF VAR rec-val AS RECID NO-UNDO.
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR lv-cost AS DEC DECIMALS 4 NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
     ll-help-run = YES.
     CASE FOCUS:NAME:
        WHEN "po-no" THEN DO:
             RUN windows/l-pofg.w (fg-rctd.company,fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT char-val).
             IF char-val <> "" THEN DO:
                ASSIGN fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(1,char-val)
                       fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(2,char-val)
                       fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(3,char-val)
                       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(4,char-val)
                       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(5,char-val)
                       .
               FIND po-ordl WHERE po-ordl.company = cocode AND
                                  po-ordl.po-no = integer(ENTRY(1,char-val)) AND
                                  po-ordl.line = integer(ENTRY(6,char-val))
                                  NO-LOCK NO-ERROR.
               IF AVAIL po-ordl THEN DO:
                  ASSIGN /*-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.cons-uom /*pr-qty-uom */*/
                         fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.pr-uom
                         lv-cost = po-ordl.cost * (IF po-ordl.disc NE 0 THEN (1 - (po-ordl.disc / 100)) ELSE 1).

                  RUN convert-vend-comp-curr(INPUT-OUTPUT lv-cost).
                  fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-cost).
               END. /* if avail po-ordl */

               FIND FIRST itemfg WHERE itemfg.company = cocode AND
                                        itemfg.i-no = entry(2,char-val)
                                        NO-LOCK NO-ERROR.
               IF AVAIL itemfg THEN DO:                         
                  ASSIGN fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} =  itemfg.def-loc
                         fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} =  itemfg.def-loc-bin
                         fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.case-count)
                       /*  fg-rctd.cost-uom = if itemfg.pur-man = itemfg.pur-uom
                                            else itemfg.prod-uom  */                        
                         .
               END. /* if avail itemfg */
               fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} = "0".

             END.  /* char-val <> "" */
             RETURN NO-APPLY.   
       END.
       WHEN "i-no" THEN DO:
             IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:
                RUN windows/l-poitem.w (fg-rctd.company,fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}, FOCUS:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT char-val).
                IF char-val <> "" THEN DO :
                   ASSIGN FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(1,char-val)
                       fg-rctd.i-name:screen-value = ENTRY(2,char-val)
                       fg-rctd.job-no:screen-value = ENTRY(3,char-val)
                       fg-rctd.job-no2:screen-value = ENTRY(4,char-val)
                       .
                END.
             END.
             ELSE IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
                  RUN windows/l-jobit1.w (fg-rctd.company,fg-rctd.job-no:SCREEN-VALUE,fg-rctd.job-no2:screen-value, FOCUS:SCREEN-VALUE, OUTPUT char-val,OUTPUT rec-val).
                  IF char-val <> ""  THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
                  IF rec-val <> ? THEN DO:
                     FIND tt-job-hdr WHERE RECID(tt-job-hdr) = rec-val NO-LOCK NO-ERROR.

                     IF AVAIL tt-job-hdr THEN 
                         ASSIGN fg-rctd.std-cost:SCREEN-VALUE = STRING(tt-job-hdr.std-mat-cost +
                                                              tt-job-hdr.std-lab-cost +
                                                              tt-job-hdr.std-fix-cost +
                                                              tt-job-hdr.std-var-cost)
                               .

                  END.

             END.
             ELSE DO:
                  RUN windows/l-itemf2.w (fg-rctd.company, "", fg-rctd.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
                  IF rec-val <> ? THEN DO:
                     FIND itemfg WHERE RECID(itemfg) = rec-val NO-LOCK.
                     ASSIGN fg-rctd.i-no:SCREEN-VALUE  = itemfg.i-no
                       fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name
                       fg-rctd.loc:SCREEN-VALUE = itemfg.def-loc
                       fg-rctd.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
                       fg-rctd.std-cost:SCREEN-VALUE = STRING(itemfg.avg-cost)
                       fg-rctd.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .
                  END.
             END.
             RETURN NO-APPLY.   
       END.
       WHEN "job-no" /*or when "job-no2" */ THEN DO:
             RUN windows/l-jobno.w (fg-rctd.company, FOCUS:SCREEN-VALUE,OUTPUT char-val, OUTPUT rec-val).
             IF char-val <> "" THEN
                ASSIGN /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       */ 
                       fg-rctd.job-no:screen-value = ENTRY(1,char-val)
                       fg-rctd.job-no2:screen-value = ENTRY(2,char-val)
                       fg-rctd.i-no:SCREEN-VALUE = ENTRY(3,char-val)
                       .
             IF rec-val <> ? THEN DO:
                FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.

                IF AVAIL job-hdr THEN 
                   fg-rctd.std-cost:SCREEN-VALUE = STRING(job-hdr.std-mat-cost +
                                                          job-hdr.std-lab-cost +
                                                          job-hdr.std-fix-cost +
                                                          job-hdr.std-var-cost)
                          .
             END.
             FIND FIRST itemfg WHERE itemfg.company = g_company
                           AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name
                        fg-rctd.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .

             RETURN NO-APPLY.   
       END.  
       WHEN "job-no2" THEN DO:
             RUN windows/l-jobno2.w (fg-rctd.company, fg-rctd.job-no:screen-value,FOCUS:SCREEN-VALUE,OUTPUT char-val, OUTPUT rec-val).
             IF char-val <> "" THEN
                ASSIGN /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       fg-rctd.job-no:screen-value = entry(1,char-val) */
                       fg-rctd.job-no2:screen-value = ENTRY(2,char-val)
                       fg-rctd.i-no:SCREEN-VALUE = ENTRY(3,char-val)
                       .
             IF rec-val <> ? THEN DO:
                FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.

                IF AVAIL job-hdr THEN 
                   fg-rctd.std-cost:SCREEN-VALUE = STRING(job-hdr.std-mat-cost +
                                                          job-hdr.std-lab-cost +
                                                          job-hdr.std-fix-cost +
                                                          job-hdr.std-var-cost)
                   .
             END.
             FIND itemfg WHERE itemfg.company = g_company
                           AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name
                        fg-rctd.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .
             RETURN NO-APPLY.   
       END.  
       WHEN "loc" THEN DO:
             RUN windows/l-loc.w (fg-rctd.company,FOCUS:SCREEN-VALUE, OUTPUT char-val).
             IF char-val <> "" THEN DO :
                ASSIGN FOCUS:SCREEN-VALUE IN  BROWSE {&browse-name}  = ENTRY(1,char-val)
                       .

             END.
             RETURN NO-APPLY.   
       END.
       WHEN "loc-bin" THEN DO:
             RUN windows/l-fgbin.w (fg-rctd.company,fg-rctd.loc:screen-value, fg-rctd.loc-bin:screen-value,OUTPUT char-val).
             IF char-val <> "" THEN DO :
                ASSIGN FOCUS:SCREEN-VALUE  = ENTRY(1,char-val)
                       /*fg-rctd.loc:screen-value = entry(2,char-val)
                        fg-rctd.tag:screen-value = entry(4,char-val)*/
                       .

             END.
             RETURN NO-APPLY.   
       END.  
         WHEN "tag" THEN DO:  /* task 11111306 */
             IF ll-set-parts THEN DO:
                  DEF VAR lv-all-or-one AS cha NO-UNDO.
                  DEF VAR lchk AS LOG NO-UNDO.
                  DEF VAR fg-item-name AS CHAR NO-UNDO.
                 
                  /*IF relmerge-int NE 0 THEN
                     MESSAGE "Select Bins for All Jobs?" VIEW-AS ALERT-BOX  
                      QUESTION BUTTON YES-NO UPDATE lchk .

                 lv-all-or-one = IF lchk THEN "ALL" ELSE "ONE".*/
                 lv-all-or-one = "ALL" .
                   
                 FIND FIRST b-tag-rctd WHERE  b-tag-rctd.company EQ cocode AND 
                       b-tag-rctd.r-no EQ int(SUBSTRING(reftable.dscr,9,35)) AND 
                       (b-tag-rctd.rita-code EQ "R" OR b-tag-rctd.rita-code EQ "E")  NO-LOCK NO-ERROR.
                
                 IF AVAIL b-tag-rctd THEN ASSIGN fg-item-name = b-tag-rctd.i-no .
                 ELSE fg-item-name = ""  .
                    
                 RUN oe/l-tagnew.w (6, ROWID(fg-rctd), lv-all-or-one,fg-rctd.i-no:screen-value ,fg-rctd.job-no:screen-value,fg-rctd.job-no2:screen-value,
                                    lv-linker, OUTPUT char-val).
                   IF char-val <> "" THEN DO :
                       ASSIGN fg-rctd.tag:SCREEN-VALUE =  ENTRY(1,char-val)
                           fg-rctd.loc:SCREEN-VALUE =  ENTRY(2,char-val)
                           fg-rctd.loc-bin:SCREEN-VALUE =  ENTRY(3,char-val) .
                   END.
             END.
         END.   /* task 11111306 */
     END CASE.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}

    ASSIGN
     lv-prev-job2       = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}
     ll-qty-case-ent    = NO
     lv-overrun-checked = NO 
     lv-closed-checked  = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i} */
    {est/brsleave.i}

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
      

  RUN set-query.

  IF AVAIL fg-rctd                                                          AND
     CAN-FIND(FIRST tt-fg-rctd WHERE tt-fg-rctd.tt-rowid EQ ROWID(fg-rctd)) THEN
    RUN update-tt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.rct-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.rct-date Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.rct-date IN BROWSE Browser-Table /* Receipt!Date */
DO:
  IF LASTKEY NE -1 AND ll-set-parts THEN DO:
    IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
      APPLY "back-tab" TO {&self-name} IN BROWSE {&browse-name}.
    ELSE
      APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
  RUN reset-cursor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.rct-date Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.rct-date IN BROWSE Browser-Table /* Receipt!Date */
DO:
  IF LASTKEY <> -1 THEN DO:
     {custom/currentDatePrompt.i SELF:SCREEN-VALUE}
     lv-rct-date-checked = YES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:    
  DEF VAR lNegative AS LOG NO-UNDO.
  IF LASTKEY NE -1 AND SELF:MODIFIED THEN DO:    
    IF avail(fg-rctd) AND fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE fg-rctd.tag THEN
      RUN valid-tag (FOCUS, OUTPUT lNegative) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    IF lNegative THEN DO:
      IF lNegative AND INTEGER(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN
        fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} = 
               STRING(INTEGER(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) * -1).

    END.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
  RUN new-tag.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:
    
  IF LASTKEY NE -1                                                  AND
     (ll-set-parts                                               OR
      fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" OR
      fg-rctd.rita-code EQ "E")                                     THEN DO:
    IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
      APPLY "back-tab" TO {&self-name} IN BROWSE {&browse-name}.
    ELSE
      APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:

  DEF VAR liDummy AS INT NO-UNDO.
  liDummy =  BROWSE {&browse-name}:NUM-SELECTED-ROWS.

  /* should never be here if no row is selected */
  IF liDummy EQ 0 THEN
    RETURN.

  IF LASTKEY NE -1 AND SELF:MODIFIED THEN DO:
      IF INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
          {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} = "".
      IF {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
          FIND FIRST po-ordl
              WHERE po-ordl.company   EQ fg-rctd.company
              AND po-ordl.po-no     EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name})
              AND po-ordl.item-type EQ NO
              AND po-ordl.i-no      EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
              NO-LOCK NO-ERROR.

          IF NOT AVAIL po-ordl THEN
              FIND FIRST po-ordl
              WHERE po-ordl.company   EQ fg-rctd.company
              AND po-ordl.po-no     EQ INT({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name})
              AND po-ordl.item-type EQ NO
              NO-LOCK NO-ERROR.
          IF AVAIL po-ordl THEN RUN display-po (ROWID(po-ordl)).
      END.

  
    RUN valid-po-no (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  IF fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} EQ ? 
    OR fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} EQ "?" THEN
       fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = "0".

  IF fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} EQ ? 
    OR fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} EQ "?" THEN
       fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
  FIND po-ord 
      WHERE po-ord.company EQ cocode
        AND po-ord.po-no EQ INTEGER(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
      NO-LOCK NO-ERROR.
  IF AVAIL po-ord THEN DO:

      /* 10021210 */
      FIND FIRST shipto WHERE shipto.company EQ cocode
                          AND shipto.cust-no EQ po-ord.cust-no
                          AND shipto.ship-id EQ po-ord.ship-id
                          NO-LOCK NO-ERROR.
         IF AVAIL shipto AND shipto.loc GT "" THEN
             ASSIGN
             fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} = shipto.loc
             fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = shipto.loc-bin.
  END.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.job-no IN BROWSE Browser-Table /* Job# */
DO:
  lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.

  IF LASTKEY NE -1 AND ll-set-parts THEN DO:
    IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
      APPLY "back-tab" TO {&self-name} IN BROWSE {&browse-name}.
    ELSE
      APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.

  IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
    IF CAN-FIND(FIRST tt-fg-rctd WHERE tt-fg-rctd.tt-rowid EQ ROWID(fg-rctd)) THEN
      RUN update-tt.
    ELSE
      APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.job-no IN BROWSE Browser-Table /* Job# */
DO:

  DEF VAR liDummy AS INT NO-UNDO.
  liDummy =  BROWSE {&browse-name}:NUM-SELECTED-ROWS.

  /* should never be here if no row is selected */
  IF liDummy EQ 0 THEN
    RETURN.

  IF LASTKEY NE -1 AND SELF:MODIFIED THEN DO:
    IF NOT fgRecptPassWord-log THEN
      RUN valid-job-no (INPUT YES) NO-ERROR.
    ELSE
      /* run with 'no' so no message until save */
      RUN valid-job-no (INPUT NO) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.job-no2 IN BROWSE Browser-Table
DO:
  lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.
  lv-prev-job2 =  SELF:SCREEN-VALUE.

  IF LASTKEY NE -1 AND ll-set-parts THEN DO:
    IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
      APPLY "back-tab" TO {&self-name} IN BROWSE {&browse-name}.
    ELSE
      APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.job-no2 IN BROWSE Browser-Table
DO:
  IF LASTKEY NE -1 AND SELF:MODIFIED THEN DO:
    RUN valid-job-no2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF lv-prev-job2 <> fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} THEN DO:
       RUN new-job-no.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.job-no2 IN BROWSE Browser-Table
DO:
  /*RUN new-job-no.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  IF LASTKEY NE -1 AND ll-set-parts THEN DO:
    IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
      APPLY "back-tab" TO {&self-name} IN BROWSE {&browse-name}.
    ELSE
      APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  IF LASTKEY = -1 THEN RETURN.

  IF INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND
     fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NE ""      THEN DO:
    RUN valid-po-no (0) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "FG does not exist on PO..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fg-rctd.i-no IN BROWSE {&browse-name}.
      RETURN NO-APPLY.
    END.
  END.

  FIND FIRST itemfg {sys/look/itemfgrlW.i}
             AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                 NO-LOCK NO-ERROR.
  IF NOT AVAIL itemfg THEN DO:
     IF fg-rctd.i-no:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Invalid Item. Try help. " VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.i-no IN BROWSE {&browse-name}.
        RETURN NO-APPLY.
     END.
     ELSE DO:
       MESSAGE  " F/G Item is not on file.  Would you like to add it? "
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
       IF NOT ll-ans THEN DO:
           APPLY "entry" TO fg-rctd.i-no IN BROWSE {&browse-name}.
           RETURN NO-APPLY.           
       END.
       ELSE DO:
           RUN fg/d-crtitm.w (SELF:SCREEN-VALUE) .
           FIND FIRST itemfg {sys/look/itemfgrlW.i}
                     AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                     NO-LOCK NO-ERROR.
           IF AVAIL itemfg THEN ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name
                                       fg-rctd.loc:SCREEN-VALUE = itemfg.def-loc
                                       fg-rctd.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
                                        .
       END.
     END.
  END.

  RUN valid-i-no (FOCUS) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /*IF SELF:MODIFIED THEN*/ 
  IF adm-new-record THEN
     RUN get-def-values.
  IF fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} EQ ? 
    OR fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} EQ "?" THEN
       fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = "0".

  IF fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} EQ ? 
    OR fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} EQ "?" THEN
       fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-name Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.i-name IN BROWSE Browser-Table /* Name/Desc */
DO:
  IF LASTKEY NE -1 AND ll-set-parts THEN DO:
    IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
      APPLY "back-tab" TO {&self-name} IN BROWSE {&browse-name}.
    ELSE
      APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc IN BROWSE Browser-Table /* Whse */
DO:
    IF LASTKEY = -1 THEN RETURN.

    IF SELF:MODIFIED THEN DO:
       FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
       IF NOT AVAIL loc THEN DO:
          MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
    IF LASTKEY = -1 THEN RETURN .

  IF SELF:MODIFIED THEN DO:
       FIND FIRST fg-bin WHERE fg-bin.company = g_company 
                           AND fg-bin.i-no = ""
                           AND fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                           AND fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                           USE-INDEX co-ino NO-LOCK NO-ERROR.
       IF NOT AVAIL fg-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.cases IN BROWSE Browser-Table /* Units */
DO:
  /* If it's in cases, needs to validate before continuing */
  IF LASTKEY = -1 AND NOT FOCUS:NAME EQ "cases" THEN RETURN .
  RUN new-qty.
 
  RUN valid-cases (FOCUS) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.qty-case IN BROWSE Browser-Table /* Unit!Count */
DO:
  ll-qty-case-ent = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.qty-case IN BROWSE Browser-Table /* Unit!Count */
DO:
  RUN new-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cases-unit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases-unit Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.cases-unit IN BROWSE Browser-Table /* Units/!Skid */
DO:
  RUN new-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.partial Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF fg-rctd.partial IN BROWSE Browser-Table /* Partial */
DO:
  RUN new-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.std-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.std-cost Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.std-cost IN BROWSE Browser-Table /* Cost/UOM */
DO:
  IF LASTKEY NE -1                                                      AND
     (ll-set-parts OR
      TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE "") THEN DO:
    IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
      APPLY "back-tab" TO {&self-name} IN BROWSE {&browse-name}.
    ELSE
      APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.std-cost Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.std-cost IN BROWSE Browser-Table /* Cost/UOM */
DO:
    IF SELF:MODIFIED THEN
  RUN get-matrix (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cost-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cost-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.cost-uom IN BROWSE Browser-Table /* UOM */
DO:
  IF LASTKEY NE -1                                                      AND
     (ll-set-parts OR
      TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE "") THEN DO:
    IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
      APPLY "back-tab" TO {&self-name} IN BROWSE {&browse-name}.
    ELSE
      APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cost-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.cost-uom IN BROWSE Browser-Table /* UOM */
DO:
  IF LASTKEY NE -1 AND SELF:MODIFIED THEN DO:
    RUN valid-uom NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN get-matrix (NO).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.frt-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.frt-cost Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.frt-cost IN BROWSE Browser-Table /* Freight Cost */
DO:
  IF LASTKEY NE -1                                                      AND
     (ll-set-parts OR
      TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE "") THEN DO:
    IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
      APPLY "back-tab" TO {&self-name} IN BROWSE {&browse-name}.
    ELSE
      APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.frt-cost Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.frt-cost IN BROWSE Browser-Table /* Freight Cost */
DO:
    IF SELF:MODIFIED THEN
  RUN get-matrix (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.stack-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.stack-code Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.stack-code IN BROWSE Browser-Table /* FG Lot# */
DO:
  IF LASTKEY NE -1 THEN DO:    
    RUN valid-lot# (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

IF fgsecurity-log THEN
DO:
   FIND FIRST usergrps WHERE
        usergrps.usergrps = fgsecurity-char
        NO-LOCK NO-ERROR.

   IF AVAIL usergrps AND
      (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND
       TRIM(usergrps.users) NE "*") THEN
      ASSIGN
         fg-rctd.std-cost:VISIBLE IN BROWSE {&BROWSE-NAME} = NO
         fg-rctd.frt-cost:VISIBLE IN BROWSE {&BROWSE-NAME} = NO
         fg-rctd.ext-cost:VISIBLE IN BROWSE {&BROWSE-NAME} = NO.
END.

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

  IF CAN-FIND(FIRST tt-fg-rctd) THEN RUN auto-add.

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
   
    IF AVAIL fg-rctd AND fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN
      RUN dispatch IN THIS-PROCEDURE (INPUT 'cancel-record':U).
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

   DEFINE INPUT-OUTPUT PARAMETER ip-cost AS DEC DECIMALS 4 NO-UNDO.

   FIND FIRST b-po-ord WHERE
        b-po-ord.company EQ po-ordl.company AND
        b-po-ord.po-no   EQ po-ordl.po-no
        NO-LOCK NO-ERROR.

   IF AVAIL b-po-ord THEN
   DO:
      FIND FIRST vend WHERE
           vend.company EQ b-po-ord.company AND
           vend.vend-no EQ b-po-ord.vend-no
           NO-LOCK NO-ERROR.

      IF AVAIL vend THEN
      DO:
         FIND FIRST b-company WHERE
              b-company.company EQ cocode
              NO-LOCK.

         IF vend.curr-code NE b-company.curr-code THEN
         DO:
            FIND FIRST currency WHERE
                 currency.company EQ b-po-ord.company AND
                 currency.c-code EQ vend.curr-code
                 NO-LOCK NO-ERROR.

            IF AVAIL currency THEN
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyBinInfo B-table-Win 
PROCEDURE copyBinInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER iprFgBin AS ROWID NO-UNDO.
DEF INPUT PARAMETER iprFgRctd AS ROWID NO-UNDO.

DEF BUFFER bfFgBin FOR fg-bin.
DEF BUFFER bfFgRctd FOR fg-rctd.

 DEF VAR dMaxQty     AS DECIMAL NO-UNDO.
 DEF VAR cHeaderItem AS CHAR NO-UNDO.
 DEF VAR dMaxCompQty AS DEC NO-UNDO. /* Max component quantity */
 DEF VAR dTotalQty   AS DEC NO-UNDO.
DEF VAR rSaveFgrctd AS ROWID NO-UNDO.
IF AVAIL fg-rctd THEN
  rSaveFgRctd = ROWID(fg-rctd).

FIND bfFgBin WHERE ROWID(bfFgBin) EQ iprFgBin NO-LOCK.
IF iprFgRctd NE ? THEN DO:
    FIND FIRST bfFgRctd WHERE ROWID(bfFgRctd) EQ iprFgRctd EXCLUSIVE-LOCK NO-ERROR.
    FIND fg-rctd WHERE ROWID(fg-rctd) EQ iprFgRctd EXCLUSIVE-LOCK NO-ERROR.
END.

/* Obtain quantity of set header record */                        
dMaxCompQty = maxComponentQty().

IF AVAIL bfFgBin  THEN DO:
  
  /* dTotalQty is the qty in other lines with the same item number */
  RUN get-selected-full-qty (INPUT bfFgBin.std-tot-cost, INPUT NO, OUTPUT dTotalQty).
  
  dTotalQty = ABS(dTotalQty).
  IF iprFgRctd EQ ? THEN
    dTotalQty = dTotalQty - ABS(DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name})).
  ELSE
    dTotalQty = dTotalQty - ABS(DEC(fg-rctd.t-qty)).
  
  /* dMaxCompQty is the max that can be used from bfFgBin */
  IF ABS(dMaxCompQty) GT 0 AND ABS(dTotalQty) GT 0 THEN DO:
    
    dMaxCompQty = dMaxCompQty - ABS(dTotalQty).
    
  END.  
    
  IF dMaxCompQty LT 0 THEN DO:    
    dMaxCompQty = 0.  
  END.

  IF dMaxCompQty GT bfFgBin.qty THEN
    dMaxCompQty = bfFgBin.qty.
  
  IF iprFgRctd EQ ? THEN DO:
  
    IF /*ABS(DECIMAL(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name})) GE bfFgBin.qty */ 
        ABS(dTotalQty) + ABS(bfFgBin.qty) LE ABS(dMaxCompQty) THEN DO:
    
      ASSIGN
        fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = bfFgBin.i-no
        fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(-1 * TRUNC((bfFgBin.qty - bfFgBin.partial-count) / bfFgBin.case-count,0))
        fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(bfFgBin.case-count)
        fg-rctd.cases-unit:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(bfFgBin.cases-unit)
        fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(-1 * bfFgBin.partial-count)
        fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(-1 * bfFgBin.qty).
    END.
    ELSE DO:
      
      ASSIGN            
        fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = bfFgBin.i-no
        fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} = "0" /* STRING(-1 * TRUNC(dMaxCompQty / bfFgBin.case-count,0)) */
        fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(dMaxCompQty)
        fg-rctd.cases-unit:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(bfFgBin.cases-unit)
        fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(-1 * dMaxCompQty)
        fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(-1 * dMaxCompQty).
    END.
  END.
  ELSE DO:
    IF ABS(dTotalQty) + ABS(bfFgBin.qty) LE ABS(dMaxCompQty) THEN DO:
    
      ASSIGN
        fg-rctd.i-no = bfFgBin.i-no
        fg-rctd.cases = -1 * TRUNC((bfFgBin.qty - bfFgBin.partial-count) / bfFgBin.case-count,0)
        fg-rctd.qty-case = bfFgBin.case-count
        fg-rctd.cases-unit = bfFgBin.cases-unit
        fg-rctd.partial = -1 * bfFgBin.partial-count
        fg-rctd.t-qty = -1 * bfFgBin.qty.
    END.
    ELSE DO:
    
      ASSIGN            
        fg-rctd.i-no = bfFgBin.i-no
        fg-rctd.cases = 0 /* STRING(-1 * TRUNC(dMaxCompQty / bfFgBin.case-count,0)) */
        fg-rctd.qty-case = dMaxCompQty
        fg-rctd.cases-unit = bfFgBin.cases-unit
        fg-rctd.partial = -1 * dMaxCompQty
        fg-rctd.t-qty = -1 * dMaxCompQty.
    END.
  END.
    /* Task 12061305 */
    IF bfFgBin.job-no <> "" THEN DO:
      IF iprFgRctd EQ ? THEN
        ASSIGN
          fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = bfFgBin.job-no
          fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(STRING(bfFgBin.job-no2)))) +
                                                            TRIM(STRING(bfFgBin.job-no2)).
      ELSE
        ASSIGN
          fg-rctd.job-no = bfFgBin.job-no
          fg-rctd.job-no2 = bfFgBin.job-no2.
    END.
END. /* avail bin */
IF rSaveFgRctd NE ? THEN 
  FIND fg-rctd WHERE ROWID(fg-rctd) EQ rSaveFgRctd NO-LOCK NO-ERROR.
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
  DEF VAR ld AS DEC NO-UNDO.

  poSelected = 0.
  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH tt-pol WHERE tt-pol.selekt,
        FIRST po-ordl WHERE ROWID(po-ordl) EQ tt-pol.row-id NO-LOCK:

      CREATE tt-fg-rctd.
      ASSIGN
       poSelected              = poSelected + 1
       tt-fg-rctd.rct-date     = DATE(fg-rctd.rct-date:SCREEN-VALUE IN BROWSE {&browse-name})
       tt-fg-rctd.trans-time   = TIME
       tt-fg-rctd.i-no         = po-ordl.i-no
       tt-fg-rctd.po-no        = STRING(po-ordl.po-no)
       tt-fg-rctd.po-rowid     = ROWID(po-ordl)
       tt-fg-rctd.units-pallet = 1
       tt-fg-rctd.cases-unit   = 1.

      ld = po-ordl.ord-qty.

      IF LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0 THEN
        RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, "EA", 0, 0, 0, 0,
                                ld, OUTPUT ld).

      {sys/inc/roundup.i ld}
      
      tt-fg-rctd.t-qty = ld - 0 /*po-ordl.t-rec-qty*/.
                         
      IF tt-fg-rctd.t-qty LT 0 THEN tt-fg-rctd.t-qty = 0.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-loadtag B-table-Win 
PROCEDURE create-loadtag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       From r-loadtg.w
------------------------------------------------------------------------------*/

DEF INPUT-OUTPUT PARAM io-tag-no AS CHAR NO-UNDO.
DEF INPUT PARAM fg-rctd-row AS ROWID NO-UNDO.

DEF BUFFER b-loadtag FOR loadtag.
DEF BUFFER b-po-ordl FOR po-ordl.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER b-fg-rctd FOR fg-rctd.

DEFINE VARIABLE li                    AS INTEGER       NO-UNDO.
DEFINE VARIABLE lv-got-job            AS LOGICAL       NO-UNDO.
DEFINE VARIABLE lv-out-cost           AS DECIMAL       DECIMALS 4 NO-UNDO.
DEFINE VARIABLE lv-out-qty            AS DECIMAL       NO-UNDO.
DEFINE VARIABLE lv-from-uom           AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lv-cost-uom           AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lv-ord-qty            AS INTEGER       NO-UNDO.
DEFINE VARIABLE lv-ord-uom            AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lv-setup-included     AS LOGICAL       NO-UNDO.
DEFINE VARIABLE lv-setup-per-cost-uom AS DECIMAL       NO-UNDO.
DEFINE VARIABLE v-bwt                 LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE v-len                 LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE v-wid                 LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE v-dep                 LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE dRFIDTag              AS DECIMAL       NO-UNDO.

DEF VAR v-fgrecpt   AS LOG NO-UNDO. 
DEF VAR tb_ret      AS LOG INIT YES NO-UNDO.
DEF VAR v-loadtag   AS CHAR NO-UNDO .
DEF VAR v-mult      AS INT NO-UNDO.
DEF VAR v-cas-lab   AS LOG NO-UNDO.
DEF VAR v-tags      AS DEC NO-UNDO.

FIND FIRST sys-ctrl
  WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "FGRECPT"
  NO-LOCK NO-ERROR.
ASSIGN
v-fgrecpt = AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "LoadTag".

FIND FIRST sys-ctrl
  WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "LOADTAG"
  NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN
ASSIGN v-loadtag = sys-ctrl.char-fld
       v-mult    = sys-ctrl.int-fld
       v-cas-lab = sys-ctrl.log-fld
       v-tags    = sys-ctrl.dec-fld.

FIND b-fg-rctd WHERE ROWID(b-fg-rctd) EQ fg-rctd-row NO-LOCK NO-ERROR.
FIND FIRST itemfg WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ b-fg-rctd.i-no
                  NO-LOCK NO-ERROR.


CREATE loadtag.
ASSIGN
  loadtag.company      = cocode
  loadtag.tag-no       = io-tag-no
  loadtag.item-type    = NO /*FGitem*/
  loadtag.job-no       = b-fg-rctd.job-no
  loadtag.job-no2      = b-fg-rctd.job-no2
  /*
  loadtag.ord-no       = IF CAN-FIND(FIRST cust WHERE cust.company = cocode
                                                  AND cust.cust-no     = itemfg.cust-no
                                                  AND cust.active      = "X")
                         THEN 0 ELSE b-fg-rctd.ord-no
  */
  loadtag.i-no         = CAPS(b-fg-rctd.i-no)
  loadtag.i-name       = b-fg-rctd.i-name
  loadtag.qty          = b-fg-rctd.qty
  loadtag.qty-case     = b-fg-rctd.qty-case
  loadtag.case-bundle  = b-fg-rctd.cases-stack
  loadtag.pallet-count = IF b-fg-rctd.units-pallet GT 0 THEN 
                            b-fg-rctd.qty MOD b-fg-rctd.units-pallet
                         ELSE
                            0
  loadtag.partial      = IF b-fg-rctd.qty-case GT 0 THEN 
                            b-fg-rctd.qty MOD b-fg-rctd.qty-case
                         ELSE
                            0
  loadtag.sts          = "Printed" 
  loadtag.tag-date     = TODAY
  loadtag.tag-time     = TIME
  loadtag.misc-dec[1]  = b-fg-rctd.tot-wt
  /*
  loadtag.misc-dec[2]  = b-fg-rctd.pallt-wt
  loadtag.misc-char[2] = b-fg-rctd.lot
  */
  loadtag.po-no = INT(b-fg-rctd.po-no).

IF v-fgrecpt AND tb_ret THEN loadtag.tot-cases  = (loadtag.pallet-COUNT - loadtag.partial) / loadtag.case-bundle.

IF v-loadtag = "CentBox" THEN DO:
  ASSIGN loadtag.loc     = itemfg.def-loc
         loadtag.loc-bin = itemfg.def-loc-bin.
  FIND FIRST fg-bin WHERE fg-bin.company EQ itemfg.company
                      AND fg-bin.i-no    EQ itemfg.i-no
                      AND fg-bin.job-no  EQ b-fg-rctd.job-no
                      AND fg-bin.tag     EQ loadtag.tag-no
                    NO-LOCK NO-ERROR.
  IF AVAIL fg-bin THEN
  ASSIGN loadtag.loc     = fg-bin.loc
         loadtag.loc-bin = fg-bin.loc-bin.
  
END.
ELSE RUN fg/autopost.p (ROWID(itemfg), b-fg-rctd.job-no, b-fg-rctd.job-no2,
                        OUTPUT loadtag.loc , OUTPUT loadtag.loc-bin).

IF RFIDTag-log THEN DO:
  FIND FIRST oe-ctrl WHERE oe-ctrl.company = loadtag.company 
                     NO-ERROR.
  dRFIDTag = IF AVAIL oe-ctrl AND oe-ctrl.spare-char-1 <> ""
             THEN dec(oe-ctrl.spare-char-1) ELSE 111110000000000000000001.
  oe-ctrl.spare-char-1 = STRING(dRFIDTag + 1).
  CREATE rfidtag.
  ASSIGN rfidtag.company   = loadtag.company
         rfidtag.item-type = loadtag.item-type
         rfidtag.tag-no    = loadtag.tag-no
         rfidtag.rfidtag   = STRING(dRFIDTag).
  RELEASE oe-ctrl.
END.


FIND CURRENT loadtag NO-LOCK NO-ERROR.
FIND CURRENT b-fg-rctd NO-LOCK NO-ERROR.

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
    
  IF NOT AVAIL fg-rctd AND INTEGER(fg-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN DO:
      FIND FIRST fg-rctd
        WHERE fg-rctd.r-no EQ INTEGER(fg-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
      NO-LOCK NO-ERROR.
  END.    /*Mode 001*/
    
  IF AVAIL fg-rctd THEN DO:
    FIND FIRST tt-fg-rctd WHERE tt-fg-rctd.tt-rowid EQ ROWID(fg-rctd) NO-ERROR.
    IF AVAIL tt-fg-rctd THEN DELETE tt-fg-rctd.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteSetParts B-table-Win 
PROCEDURE DeleteSetParts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcLinker AS CHAR NO-UNDO.
DEF BUFFER b-fg-rctd FOR fg-rctd.

FOR EACH fg-rcpts
    WHERE fg-rcpts.company EQ cocode
      AND fg-rcpts.linker  EQ ipcLinker 
    NO-LOCK :
    FOR EACH b-fg-rctd
        WHERE b-fg-rctd.company EQ cocode
          AND b-fg-rctd.r-no    EQ fg-rcpts.r-no USE-INDEX fg-rctd
        EXCLUSIVE-LOCK:
        DELETE b-fg-rctd.
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-po B-table-Win 
PROCEDURE display-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
    
  DEF VAR lv-cost AS DEC DECIMALS 4 NO-UNDO.

  FIND po-ordl WHERE ROWID(po-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL po-ordl THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.i-no
     fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.i-name
     fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.pr-uom
     lv-cost = po-ordl.cost * (IF po-ordl.disc NE 0 THEN (1 - (po-ordl.disc / 100)) ELSE 1).

    RUN convert-vend-comp-curr(INPUT-OUTPUT lv-cost).

    fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-cost).

    FIND FIRST itemfg WHERE itemfg.company = fg-rctd.company AND
                        itemfg.i-no = po-ordl.i-no
                        NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN 
       ASSIGN fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} =  itemfg.def-loc
              fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} =  itemfg.def-loc-bin
              fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.case-count)
            /*  fg-rctd.cost-uom = if itemfg.pur-man = itemfg.pur-uom
                            else itemfg.prod-uom  */.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-current-qty B-table-Win 
PROCEDURE get-current-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER opcItem AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER opdQty AS DEC NO-UNDO.
  IF AVAIL fg-rctd THEN
    ASSIGN opdQty = fg-rctd.t-qty
           opcItem = fg-rctd.i-no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-def-values B-table-Win 
PROCEDURE get-def-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF adm-new-record THEN
      ASSIGN
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = ""
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = ""
       fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = ""
       fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = ""
       fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = "".

    RUN get-values.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-fg-bin-cost B-table-Win 
PROCEDURE get-fg-bin-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN
      ASSIGN
       fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.std-tot-cost)
       fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.pur-uom).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-first-r-no B-table-Win 
PROCEDURE get-first-r-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bq-fg-rctd FOR fg-rctd.

  lv-frst-rno = 999999999.

  FOR EACH bq-fg-rctd FIELDS(r-no)
      WHERE bq-fg-rctd.company   EQ cocode
        AND bq-fg-rctd.rita-code EQ "R"
        AND bq-fg-rctd.r-no      LT lv-frst-rno
      USE-INDEX rita-code NO-LOCK
      BY bq-fg-rctd.r-no:
    lv-frst-rno = bq-fg-rctd.r-no.
    LEAVE.
  END.
  RELEASE bq-fg-rctd.

  FOR EACH bq-fg-rctd FIELDS(r-no)
      WHERE bq-fg-rctd.company   EQ cocode
        AND bq-fg-rctd.rita-code EQ "E"
        AND bq-fg-rctd.r-no      LT lv-frst-rno
      USE-INDEX rita-code NO-LOCK
      BY bq-fg-rctd.r-no:
    lv-frst-rno = bq-fg-rctd.r-no.
    LEAVE.
  END.
  RELEASE bq-fg-rctd.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-freight-cost B-table-Win 
PROCEDURE get-freight-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-cost LIKE fg-rctd.frt-cost NO-UNDO.

  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ld-qty AS DEC NO-UNDO.
  DEF VAR ld-wgt AS DEC EXTENT 2 NO-UNDO.
  DEF VAR ld-cst AS DEC EXTENT 2 NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RELEASE po-ord.

    FIND FIRST po-ordl
        WHERE po-ordl.company   EQ cocode
          AND po-ordl.po-no     EQ INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.i-no      EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND po-ordl.job-no    EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND po-ordl.job-no2   EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND po-ordl.item-type EQ NO
        NO-LOCK NO-ERROR.

    IF AVAIL po-ordl THEN
      RUN po/getfrtcs.p (ROWID(po-ordl),
                         DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}),
                         OUTPUT op-cost).

    RUN convert-vend-comp-curr(INPUT-OUTPUT op-cost).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-job-no B-table-Win 
PROCEDURE get-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER iplAskPasswd AS LOG NO-UNDO.
  DEF VAR lvPasswordEntered AS LOG NO-UNDO.
  DEF VAR lcRitaCode AS CHAR NO-UNDO.
  IF AVAIL(fg-rctd) THEN
      lcRitaCode = fg-rctd.rita-code.
  ELSE
      lcRitaCode = "R".
  DO WITH FRAME {&frame-name}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE TRIM(lv-job-no)  OR
       DEC(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE DEC(lv-job-no2) THEN
      RUN new-job-no.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
      IF fgrecpt                                                AND
         fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" AND
         lcRitaCode NE "E"                                  AND
         iplAskPasswd THEN DO:

        /* Check password for override */
        RUN sys/ref/d-psswrd.w (INPUT "FGRecptPassWord", INPUT "FGRecptPassWord",
                                OUTPUT lvPasswordEntered).
      END. /* If nk1 is set to validate blank job/po */
    END. /* If job# blank */

    ELSE DO:
      IF INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:
       
      END.

      FIND FIRST job-hdr
          WHERE job-hdr.company EQ cocode
            AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job-hdr THEN DO:
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-linker B-table-Win 
PROCEDURE get-linker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-linker LIKE lv-linker NO-UNDO.


  op-linker = IF AVAIL fg-rctd                 AND
                 CAN-FIND(FIRST itemfg
                          WHERE itemfg.company EQ fg-rctd.company
                            AND itemfg.i-no    EQ fg-rctd.i-no
                            AND itemfg.isaset) THEN
                "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
              ELSE "".

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
  DEF INPUT PARAMETER ip-first-disp AS LOG NO-UNDO.

  DEFINE VARIABLE v-len             LIKE po-ordl.s-len    NO-UNDO.
  DEFINE VARIABLE v-wid             LIKE po-ordl.s-len    NO-UNDO.
  DEFINE VARIABLE v-dep             LIKE po-ordl.s-len    NO-UNDO.
  DEFINE VARIABLE v-bwt             LIKE po-ordl.s-len    NO-UNDO.
  DEFINE VARIABLE v-tot-msf         AS DECIMAL            NO-UNDO.
  DEFINE VARIABLE lv-out-qty        AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE lv-out-cost       AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE lv-over-cost      AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE lv-ext-cost       AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE lv-cost-uom       LIKE rm-rctd.cost-uom NO-UNDO.
  DEFINE VARIABLE lv-from-uom       LIKE rm-rctd.cost-uom NO-UNDO.
  DEFINE VARIABLE lv-out-ea         AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE v-rec-qty         AS INTEGER          NO-UNDO.
  DEFINE VARIABLE v-job-qty         AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE v-ord-qty         AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE v-ord-cost        AS DECIMAL          NO-UNDO .
  DEFINE VARIABLE v-ord-uom         AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE v-setup-qty       AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE lv-adjusted-ea    AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE v-ord-po-uom      AS CHARACTER        NO-UNDO.
  DEFINE VARIABLE v-cost-per-ea     AS DECIMAL          NO-UNDO .
  DEFINE VARIABLE v-cost-setup      AS DECIMAL          NO-UNDO .
  DEFINE VARIABLE v-cost-with-setup AS DECIMAL          NO-UNDO .
  DEFINE VARIABLE v-corr            AS LOGICAL          NO-UNDO.
  DEFINE VARIABLE v-basis-w         AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE v-out-qty         AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE v-qty-per-msf     AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE v-tot-out         AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE v-tot-cost        AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE v-full-qty        AS DECIMAL          NO-UNDO.
  DEFINE VARIABLE lvlTotalCostCalculated AS LOG         NO-UNDO.
  DEF VAR lvCalcCostUom LIKE fg-rctd.cost-uom NO-UNDO.
  DEF VAR lvCalcStdCost LIKE fg-rctd.std-cost NO-UNDO.
  DEF VAR lvCalcExtCost LIKE fg-rctd.ext-cost NO-UNDO.
  DEF VAR lvCalcFrtCost LIKE fg-rctd.frt-cost NO-UNDO.
  DEF VAR lvSetupPerCostUom AS DEC NO-UNDO.

  DEF BUFFER b-job-hdr FOR job-hdr.
  IF NOT AVAIL fg-rctd THEN RETURN.  /* no records */

DO WITH FRAME {&FRAME-NAME}:
FIND itemfg WHERE itemfg.company EQ cocode
              AND itemfg.i-no  EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            USE-INDEX i-no NO-LOCK NO-ERROR.

ASSIGN
  v-bwt             = 0
  v-dep             = 0
  v-ord-qty         = 0
  v-ord-cost        = 0
  v-ord-uom         = ""
  v-ord-po-uom      = ""
  v-rec-qty         = 0
  v-job-qty         = 0
  v-ord-cost        = 0
  v-ord-uom         = ""
  v-ord-po-uom      = ""
  v-cost-per-ea     = 0
  v-cost-setup      = 0
  v-cost-with-setup = 0
  v-cost-setup      = 0.

IF AVAIL itemfg THEN
  ASSIGN v-len       = itemfg.t-len
         v-wid       = itemfg.t-wid
         lv-cost-uom = itemfg.prod-uom.

/* Always find just to get quantity */
FIND FIRST po-ordl WHERE po-ordl.company = cocode
                     AND po-ordl.po-no = int(fg-rctd.po-no)
                     AND po-ordl.i-no  = fg-rctd.i-no
                     AND po-ordl.job-no = fg-rctd.job-no
                     AND po-ordl.job-no2 = fg-rctd.job-no2
                     AND po-ordl.item-type = NO
                     NO-LOCK NO-ERROR.
IF NOT AVAIL po-ordl THEN
    FIND FIRST po-ordl WHERE po-ordl.company = cocode
                         AND po-ordl.po-no 
                             = int(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                         AND po-ordl.i-no  = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                         AND po-ordl.item-type = NO
                         NO-LOCK NO-ERROR.

IF AVAIL(po-ordl) THEN DO:

  ASSIGN v-ord-qty  = po-ordl.ord-qty
         v-ord-cost = po-ordl.cons-cost
         v-ord-uom  = po-ordl.cons-uom
         v-ord-po-uom = po-ordl.pr-qty-uom.
END.


IF ip-first-disp  AND AVAIL fg-rctd AND fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO: /* for row-display */  
  IF AVAIL po-ordl THEN
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid.
  
  ASSIGN
   lv-out-qty  = fg-rctd.t-qty
   lv-from-uom = fg-rctd.cost-uom
   lv-out-cost = fg-rctd.std-cost.
END. /* avail fg-rctd */
/* ======================================================================= */
ELSE
IF AVAIL fg-rctd AND fg-rctd.i-no:SCREEN-VALUE <> "" THEN DO: /* in update mode - use screen-value */
  FIND FIRST po-ordl WHERE po-ordl.company = fg-rctd.company
                       AND po-ordl.po-no = INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
                       AND po-ordl.i-no  = fg-rctd.i-no:screen-value
                       AND po-ordl.job-no = (fg-rctd.job-no:screen-value)
                       AND po-ordl.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       AND po-ordl.item-type = NO
                       NO-LOCK NO-ERROR.
  v-rec-qty = INT(fg-rctd.t-qty:SCREEN-VALUE).

  FOR EACH b-fg-rctd
      WHERE b-fg-rctd.company    EQ fg-rctd.company
        AND b-fg-rctd.rita-code  EQ "R"
        AND b-fg-rctd.i-no       EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        AND INT(b-fg-rctd.po-no) EQ INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
        AND b-fg-rctd.job-no     EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
        AND b-fg-rctd.job-no2    EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        AND ROWID(b-fg-rctd)     NE ROWID(fg-rctd)
      NO-LOCK:
    v-rec-qty = v-rec-qty + b-fg-rctd.t-qty.
  END.

  
  /* If available PO orderline */
  IF AVAIL po-ordl THEN DO:
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     v-rec-qty = v-rec-qty + po-ordl.t-rec-qty.

    IF LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0 THEN DO:
       RUN sys/ref/convquom.p("EA", po-ordl.pr-qty-uom, 0, 0, 0, 0,
                              v-rec-qty, OUTPUT v-rec-qty).

    END.
    IF v-rec-qty GT (po-ordl.ord-qty * (1 + (po-ordl.over-pct / 100))) AND
       NOT lv-overrun-checked                                          THEN DO:
       MESSAGE "The PO Qty + overrun has been exceeded..."
                  VIEW-AS ALERT-BOX WARNING .
       lv-overrun-checked = YES.
       /*APPLY "entry" TO fg-rctd.cases.
       RETURN ERROR.  */
    END.
    DEF VAR lv-use-full-qty AS LOG.
    DEF VAR lv-full-qty AS DEC NO-UNDO.
    /* Created task 09261318 to be used by receiving screens in addition */            
    RUN fg/calcRcptCostFromPO.p 
      (INPUT cocode ,
      INPUT ROWID(po-ordl),
      INPUT ROWID(fg-rctd),
      INPUT fg-rctd.qty-case:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.cases:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.partial:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.job-no:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.job-no2:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.cost-uom:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.t-qty:screen-value IN BROWSE {&browse-name},
      OUTPUT lv-use-full-qty,
      OUTPUT lv-full-qty,
      OUTPUT lvCalcCostUom,
      OUTPUT lvCalcStdCost,
      OUTPUT lvCalcExtCost,
      OUTPUT lvCalcFrtCost,
      OUTPUT lvSetupPerCostUom).
      
    ASSIGN
      lvlTotalCostCalculated = TRUE
      fg-rctd.cost-uom:screen-value IN BROWSE {&browse-name} = lvCalcCostUom
      fg-rctd.std-cost:screen-value IN BROWSE {&browse-name} = STRING(lvCalcStdCost)
      fg-rctd.ext-cost:screen-value IN BROWSE {&browse-name} = STRING(lvCalcExtCost).
  ASSIGN
   lv-out-qty  = DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name})
   lv-from-uom = fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}
   lv-out-cost = DEC(fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}).
  END. /* IF AVAIL po-ordl */
  /* Else if not available PO orderline and job number is entered... */
  ELSE IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
       FIND FIRST job-hdr WHERE job-hdr.company = fg-rctd.company                       
                       AND job-hdr.i-no  = fg-rctd.i-no:screen-value
                       AND job-hdr.job-no = (fg-rctd.job-no:screen-value)
                       AND job-hdr.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       NO-LOCK NO-ERROR.
       IF AVAIL job-hdr THEN DO:

/*           FOR EACH fg-act                                                                         */
/*               WHERE fg-act.company EQ cocode                                                      */
/*                 AND fg-act.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}        */
/*                 AND fg-act.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})  */
/*                 AND fg-act.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}          */
/*               NO-LOCK:                                                                            */
/*             v-rec-qty = v-rec-qty + fg-act.qty.                                                   */
/*           END.                                                                                    */

          FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND
                                    sys-ctrl.name = "JOB QTY" 
                                    NO-LOCK NO-ERROR.
          IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-job-qty = job-hdr.qty                          .
          ELSE DO:
              FIND FIRST oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ job-hdr.company
                    AND oe-ordl.ord-no  EQ job-hdr.ord-no
                    AND oe-ordl.i-no    EQ job-hdr.i-no
                  NO-ERROR.
              FIND FIRST oe-ord NO-LOCK
                  WHERE oe-ord.company EQ job-hdr.company
                    AND oe-ord.ord-no  EQ job-hdr.ord-no
                  NO-ERROR.
              
              v-job-qty = (job-hdr.qty * (1 + ((IF AVAIL oe-ordl THEN oe-ordl.over-pct ELSE
                                                IF AVAIL oe-ord  THEN oe-ord.over-pct  ELSE 0) / 100))).
          END.


          {sys/inc/roundup.i v-job-qty}

          IF v-rec-qty GT v-job-qty AND NOT lv-overrun-checked THEN DO:
             MESSAGE "Receipt Qty has exceeded Job Qty..." SKIP
                     "Rec Qty: " v-rec-qty SKIP 
                     "Job Qty: " v-job-qty SKIP 
                     "Job Hdr Qty: " job-hdr.qty
                 VIEW-AS ALERT-BOX WARNING.
             /*RETURN ERROR.*/
             lv-overrun-checked = YES.
          END.          
       END.
  END.

    
  ASSIGN
   lv-out-qty  = DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name})
   lv-from-uom = fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}
   lv-out-cost = DEC(fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}).
  
END.


IF lv-from-uom EQ "L" THEN
    ASSIGN
     lv-from-uom = "EA"
     lv-out-cost = lv-out-cost / lv-out-qty.
    

/* Calculate for quantity comparison purposes */
RUN rm/convquom.p(v-ord-po-uom, 'EA',                   
          v-bwt, v-len, v-wid, v-dep,
          v-ord-qty, OUTPUT v-ord-qty).

RUN rm/convquom.p(v-ord-po-uom, 'EA',            
          v-bwt, v-len, v-wid, v-dep,
          lv-out-qty, OUTPUT lv-out-ea).
/* Per Joe, if quantity is less than PO qty, use PO price */

IF v-ord-qty > 0 AND v-ord-qty > (IF lv-use-full-qty THEN
                                    lv-full-qty ELSE
                                    lv-out-qty) 
                 AND v-ord-cost > 0 
                 AND v-ord-uom > "" THEN DO:
    
    ASSIGN lv-out-cost = v-ord-cost
           lv-from-uom = v-ord-uom.
    
END.


    



/* convert cost pr-uom*/
IF lv-from-uom EQ lv-cost-uom               OR
   (LOOKUP(lv-from-uom,fg-uom-list) GT 0 AND
    LOOKUP(lv-cost-uom,fg-uom-list) GT 0)   THEN.
ELSE DO:
        RUN rm/convcuom.p(lv-from-uom, lv-cost-uom,                   
                          v-bwt, v-len, v-wid, v-dep,
                          lv-out-cost, OUTPUT lv-out-cost).        
    END.
  
IF LOOKUP(lv-cost-uom,fg-uom-list) EQ 0 THEN
  RUN rm/convquom.p("EA", lv-cost-uom,                   
                    v-bwt, v-len, v-wid, v-dep,
                    lv-out-qty, OUTPUT lv-out-qty).


 /* get quantity in eaches */
  RUN rm/convquom.p(lv-cost-uom, "EA",                   
                    v-bwt, v-len, v-wid, v-dep,
                    lv-out-qty, OUTPUT lv-out-ea).

 ASSIGN v-basis-w = 0
        v-corr = NO.

 /* wfk - 05101305 - implemented lv-adjusted-ea since using a qty lower than */
 /*                  the order qty resulted in a much lower cost with no     */
 /*                  setup included. Therefore, use the PO qty if the current */
 /*                  is too low                                              */
 IF lv-out-ea GE v-ord-qty THEN
   lv-adjusted-ea = lv-out-ea.
 ELSE
   lv-adjusted-ea = v-ord-qty.

 /* Calculate quantity of other matching lines to get a full qty */
 RUN get-set-full-qty (INPUT 0, INPUT YES, OUTPUT v-full-qty).

 IF v-full-qty GT lv-adjusted-ea THEN
    ASSIGN lv-adjusted-ea = v-full-qty
           lv-cost-basis = "FULLQTY". /* Set this to update other records on save */
 /* If lvlTotalCostCalculated then setup was already distributed */
 IF NOT lvlTotalCostCalculated AND AVAIL(po-ordl) AND v-ord-qty <= lv-adjusted-ea AND po-ordl.setup > 0 THEN DO:

   lv-over-cost = po-ordl.cost.
   IF po-ordl.PR-uom EQ lv-cost-uom               OR
     (LOOKUP(lv-from-uom,fg-uom-list) GT 0 AND
      LOOKUP(lv-cost-uom,fg-uom-list) GT 0)   THEN.
   ELSE
     RUN rm/convcuom.p(po-ordl.PR-uom, lv-cost-uom, 
                       v-bwt, v-len, v-wid, v-dep,
                       po-ordl.cost, OUTPUT lv-over-cost).
   v-setup-qty = lv-adjusted-ea.
   IF lv-cost-uom NE 'EA' THEN
       RUN rm/convquom.p("EA", lv-cost-uom,                   
                         v-bwt, v-len, v-wid, v-dep,
                         lv-adjusted-ea, OUTPUT v-setup-qty).
  
   lv-out-cost = lv-over-cost + (po-ordl.setup / v-setup-qty).  
   
 END.

ASSIGN
 lv-ext-cost = lv-out-qty * lv-out-cost
 fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = lv-cost-uom
 fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-out-cost)
 fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-ext-cost +
                  DEC(fg-rctd.frt-cost:SCREEN-VALUE IN BROWSE {&browse-name})).

/* Override this with code from addons that works properly */
IF ip-first-disp  AND AVAIL fg-rctd AND fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO: /* for row-display */  
  FIND FIRST po-ordl WHERE po-ordl.company = cocode
                       AND po-ordl.po-no = int(fg-rctd.po-no)
                       AND po-ordl.i-no  = fg-rctd.i-no
                       AND po-ordl.job-no = fg-rctd.job-no
                       AND po-ordl.job-no2 = fg-rctd.job-no2
                       AND po-ordl.item-type = NO
                       NO-LOCK NO-ERROR.

  IF AVAIL po-ordl THEN
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid.

  lv-out-qty = fg-rctd.t-qty.

  /* convert cost pr-uom*/
  IF fg-rctd.cost-uom EQ lv-cost-uom               OR
     (LOOKUP(fg-rctd.cost-uom,fg-uom-list) GT 0 AND
      LOOKUP(lv-cost-uom,fg-uom-list)      GT 0)   THEN
    lv-out-cost = fg-rctd.std-cost.
  ELSE
    RUN rm/convcuom.p(fg-rctd.cost-uom, lv-cost-uom,                   
                      v-bwt, v-len, v-wid, v-dep,
                      fg-rctd.std-cost, OUTPUT lv-out-cost).
END. /* avail fg-rctd */
/* ======================================================================= */
ELSE
IF AVAIL fg-rctd AND fg-rctd.i-no:SCREEN-VALUE <> "" THEN DO: /* in update mode - use screen-value */
  FIND FIRST po-ordl WHERE po-ordl.company = cocode
                       AND po-ordl.po-no = integer(fg-rctd.po-no:screen-value IN BROWSE {&browse-name}) 
                       AND po-ordl.i-no  = fg-rctd.i-no:screen-value
                       AND po-ordl.job-no = (fg-rctd.job-no:screen-value)
                       AND po-ordl.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       AND po-ordl.item-type = NO
                       NO-LOCK NO-ERROR.
  
  IF AVAIL po-ordl THEN DO:
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     v-rec-qty = po-ordl.t-rec-qty + int(fg-rctd.t-qty:SCREEN-VALUE).

    IF LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0 THEN
       RUN sys/ref/convquom.p("EA", po-ordl.pr-qty-uom, 0, 0, 0, 0,
                              v-rec-qty, OUTPUT v-rec-qty).
    IF v-rec-qty GT (po-ordl.ord-qty * 
                    (1 + (po-ordl.over-pct / 100)))
       AND NOT lv-overrun-checked
    THEN DO:
       MESSAGE "The PO Qty + overrun has been exceeded. "
                  VIEW-AS ALERT-BOX WARNING .
       lv-overrun-checked = YES.
    END.

    


    /* Created task 09261318 to be used by receiving screens in addition */            
    RUN fg/calcRcptCostFromPO.p 
      (INPUT cocode ,
      INPUT ROWID(po-ordl),
      INPUT ROWID(fg-rctd),
      INPUT fg-rctd.qty-case:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.cases:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.partial:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.job-no:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.job-no2:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.cost-uom:screen-value IN BROWSE {&browse-name},
      INPUT fg-rctd.t-qty:screen-value IN BROWSE {&browse-name},
      OUTPUT lv-use-full-qty,
      OUTPUT lv-full-qty,
      OUTPUT lvCalcCostUom,
      OUTPUT lvCalcStdCost,
      OUTPUT lvCalcExtCost,
      OUTPUT lvCalcFrtCost,
      OUTPUT lvSetupPerCostUom).
    
    ASSIGN
      fg-rctd.cost-uom:screen-value IN BROWSE {&browse-name} = lvCalcCostUom
      fg-rctd.std-cost:screen-value IN BROWSE {&browse-name} = STRING(lvCalcStdCost)
      fg-rctd.ext-cost:screen-value IN BROWSE {&browse-name} = STRING(lvCalcExtCost).
    ASSIGN
     lv-out-qty  = DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name})
     lv-cost-uom = fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-out-cost = DEC(fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}).

  END.
  ELSE IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
       FIND FIRST job-hdr WHERE job-hdr.company = cocode                       
                       AND job-hdr.i-no  = fg-rctd.i-no:screen-value
                       AND job-hdr.job-no = (fg-rctd.job-no:screen-value)
                       AND job-hdr.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       NO-LOCK NO-ERROR.
       IF AVAIL job-hdr THEN DO: 
          FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
                                    sys-ctrl.name = "JOB QTY" 
                                    NO-LOCK NO-ERROR.
          IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-rec-qty = job-hdr.qty                          .
          ELSE DO:
              FIND FIRST oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ job-hdr.company
                    AND oe-ordl.ord-no  EQ job-hdr.ord-no
                    AND oe-ordl.i-no    EQ job-hdr.i-no
                  NO-ERROR.
              FIND FIRST oe-ord NO-LOCK
                  WHERE oe-ord.company EQ job-hdr.company
                    AND oe-ord.ord-no  EQ job-hdr.ord-no
                  NO-ERROR.
              
              v-rec-qty = (job-hdr.qty * (1 + (IF AVAIL oe-ordl THEN oe-ordl.over-pct ELSE
                                               IF AVAIL oe-ord  THEN oe-ord.over-pct  ELSE 0 / 100))).
      
          END.
          IF v-rec-qty <  int(fg-rctd.t-qty:SCREEN-VALUE) AND NOT lv-overrun-checked THEN DO:
             MESSAGE "Receipt quantity exceeds job quantity." VIEW-AS ALERT-BOX WARNING.
             lv-overrun-checked = YES.
          END.
          
       END.
  END.

  lv-out-qty = DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}). 
  
  IF fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ lv-cost-uom               OR
     (LOOKUP(fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name},fg-uom-list) GT 0 AND
      LOOKUP(lv-cost-uom,fg-uom-list)                                            GT 0)   THEN
    lv-out-cost = DEC(fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}).
  ELSE
    RUN rm/convcuom.p(fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}, lv-cost-uom,                   
                      v-bwt, v-len, v-wid, v-dep,
                      fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT lv-out-cost).
END.
  
IF LOOKUP(lv-cost-uom,fg-uom-list) EQ 0 THEN
  RUN rm/convquom.p("EA", lv-cost-uom,                   
                    v-bwt, v-len, v-wid, v-dep,
                    lv-out-qty, OUTPUT lv-out-qty).

ASSIGN
 fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = lv-cost-uom
 fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-out-cost)
 fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} =
       STRING((lv-out-qty * lv-out-cost) +
           dec(fg-rctd.frt-cost:screen-value IN BROWSE {&browse-name})).


END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix-all B-table-Win 
PROCEDURE get-matrix-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-first-disp AS LOG NO-UNDO.
  DEF VAR v-len LIKE po-ordl.s-len NO-UNDO.
  DEF VAR v-wid LIKE po-ordl.s-len NO-UNDO.
  DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO. 
  DEF VAR v-bwt LIKE po-ordl.s-len NO-UNDO.
  DEF VAR lv-out-qty AS DEC NO-UNDO.
  DEF VAR lv-out-cost AS DEC NO-UNDO.
  DEF VAR v-rec-qty AS INT NO-UNDO.
  DEF VAR ll-ea AS LOG NO-UNDO.

  DEF BUFFER b-fg-rctd FOR fg-rctd.


  cocode = g_company.

  lv-out-qty = 0.
  FOR EACH b-fg-rctd WHERE b-fg-rctd.company EQ g_company AND
           (b-fg-rctd.rita-code EQ "R" OR b-fg-rctd.rita-code EQ "E")
           AND trim(b-fg-rctd.job-no) = trim(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
           AND b-fg-rctd.job-no2 = INT(fg-rctd.job-no2:SCREEN-VALUE)
           AND b-fg-rctd.i-no = fg-rctd.i-no:SCREEN-VALUE 
           AND (RECID(b-fg-rctd) <> recid(fg-rctd) 
                OR (adm-new-record AND NOT adm-adding-record))
           NO-LOCK :

      lv-out-qty = lv-out-qty + b-fg-rctd.t-qty.     
  END.
  
  lv-out-qty = lv-out-qty + int(fg-rctd.t-qty:SCREEN-VALUE).

  IF fg-rctd.i-no:SCREEN-VALUE <> "" THEN DO: /* in update mode - use screen-value */
       FIND itemfg  WHERE itemfg.company EQ cocode
                AND itemfg.i-no  EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      USE-INDEX i-no NO-LOCK NO-ERROR.
       FIND FIRST po-ordl WHERE po-ordl.company = fg-rctd.company
                       AND po-ordl.po-no = integer(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
                       AND po-ordl.i-no  = fg-rctd.i-no:screen-value
                       AND po-ordl.job-no = (fg-rctd.job-no:screen-value)
                       AND po-ordl.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       AND po-ordl.item-type = NO
                       NO-LOCK NO-ERROR.
  
       IF AVAIL po-ordl THEN DO:
          v-rec-qty = po-ordl.t-rec-qty + lv-out-qty.
          RUN sys/ref/ea-um-fg.p (po-ordl.pr-qty-uom, OUTPUT ll-ea).
          IF NOT ll-ea THEN
            RUN sys/ref/convquom.p("EA", po-ordl.pr-qty-uom, 0, 0, 0, 0,
                                   v-rec-qty, OUTPUT v-rec-qty).
         IF v-rec-qty GT (po-ordl.ord-qty * 
                    (1 + (po-ordl.over-pct / 100)))
            AND NOT lv-overrun-checked
          THEN DO:
             MESSAGE "The PO Qty + overrun has been exceeded. "
                     VIEW-AS ALERT-BOX WARNING .
             lv-overrun-checked = YES.
          END.
       END.
       ELSE IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
         FIND FIRST job-hdr WHERE job-hdr.company = fg-rctd.company                       
                       AND job-hdr.i-no  = fg-rctd.i-no:screen-value
                       AND job-hdr.job-no = (fg-rctd.job-no:screen-value)
                       AND job-hdr.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       NO-LOCK NO-ERROR.
         IF AVAIL job-hdr THEN DO: 
           FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND
                                    sys-ctrl.name = "JOB QTY" 
                                    NO-LOCK NO-ERROR.
           IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-rec-qty = job-hdr.qty                          .
           ELSE DO:
              FIND FIRST oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ job-hdr.company
                    AND oe-ordl.ord-no  EQ job-hdr.ord-no
                    AND oe-ordl.i-no    EQ job-hdr.i-no
                  NO-ERROR.
              FIND FIRST oe-ord NO-LOCK
                  WHERE oe-ord.company EQ job-hdr.company
                    AND oe-ord.ord-no  EQ job-hdr.ord-no
                  NO-ERROR.
              
              v-rec-qty = (job-hdr.qty * (1 + ((IF AVAIL oe-ordl THEN oe-ordl.over-pct ELSE
                                                IF AVAIL oe-ord  THEN oe-ord.over-pct  ELSE 0) / 100))).
           END.
           IF v-rec-qty <  lv-out-qty AND NOT lv-overrun-checked THEN DO:
              MESSAGE "Receipt Qty has exceeded Job Qty. " VIEW-AS ALERT-BOX WARNING.
             /*RETURN ERROR.*/
              lv-overrun-checked = YES.
           END.
           
         END.
       END.
     
  END. /* i-no <> ""*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-next-tag B-table-Win 
PROCEDURE get-next-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipc-i-no AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER opc-next-tag AS CHAR NO-UNDO.
  DEF BUFFER bf-loadtag FOR loadtag.
  DEF VAR io-tag-no AS INT NO-UNDO.

  FIND LAST bf-loadtag NO-LOCK
      WHERE bf-loadtag.company     EQ cocode
        AND bf-loadtag.item-type   EQ NO
        AND bf-loadtag.is-case-tag EQ NO
        AND bf-loadtag.tag-no      BEGINS ipc-i-no
        AND SUBSTR(bf-loadtag.tag-no,1,15) EQ ipc-i-no
      USE-INDEX tag NO-ERROR.

  io-tag-no = (IF AVAIL bf-loadtag THEN INT(SUBSTR(bf-loadtag.tag-no,16,5)) ELSE 0) + 1.

  opc-next-tag = STRING(CAPS(ipc-i-no),"x(15)") + STRING(io-tag-no,"99999").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-selected-full-qty B-table-Win 
PROCEDURE get-selected-full-qty :
/*------------------------------------------------------------------------------
Purpose:
Parameters:  <none>
Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-cost-to-set AS DEC NO-UNDO.
DEF INPUT PARAMETER ip-on-screen AS LOG NO-UNDO.
DEF OUTPUT PARAMETER op-out-qty AS DEC NO-UNDO.

DEFINE VARIABLE v-len          LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE v-wid          LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE v-dep          LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE v-bwt          LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE lv-out-qty     AS DECIMAL       NO-UNDO.
DEFINE VARIABLE lv-out-cost    AS DECIMAL       NO-UNDO.
DEFINE VARIABLE lv-calc-cost   AS DECIMAL.
DEFINE VARIABLE lv-recalc-cost AS DECIMAL.
DEFINE VARIABLE lv-ext-cost    AS DECIMAL.
DEFINE VARIABLE v-rec-qty      AS INTEGER       NO-UNDO.
DEFINE VARIABLE ll-ea          AS LOGICAL       NO-UNDO.

DEF BUFFER b-fg-rctd FOR fg-rctd.
DEF BUFFER b1-fg-rctd FOR fg-rctd.



cocode = g_company.
/* wfk - 6/9/2014 - taking out job join for test */
lv-out-qty = 0.
EACH-EXISTING:
FOR EACH b-fg-rctd WHERE b-fg-rctd.company EQ g_company 
    AND (b-fg-rctd.rita-code EQ "R" OR b-fg-rctd.rita-code EQ "E")
    AND b-fg-rctd.i-no = fg-rctd.i-no
    AND (RECID(b-fg-rctd) <> RECID(fg-rctd)
         OR (adm-new-record AND NOT adm-adding-record))
    AND b-fg-rctd.SetHeaderRno EQ INTEGER(SUBSTRING(lv-linker, 10, 10))
  NO-LOCK:


  
  lv-out-qty = lv-out-qty + b-fg-rctd.t-qty.
  IF ip-cost-to-set GT 0 THEN DO:
    
    /* convert cost to b1-fg-rctd uom */
    
    FIND b1-fg-rctd WHERE ROWID(b1-fg-rctd) EQ ROWID(b-fg-rctd)
      EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL b1-fg-rctd THEN DO WITH FRAME {&FRAME-NAME}:
      
      FIND itemfg WHERE itemfg.company EQ cocode
      AND itemfg.i-no  EQ b-fg-rctd.i-no
      USE-INDEX i-no NO-LOCK NO-ERROR.
      
      ASSIGN
      v-bwt             = 0
      v-dep             = 0.
      
      IF AVAIL itemfg THEN
      ASSIGN v-len       = itemfg.t-len
             v-wid       = itemfg.t-wid.
      
      /* Always find just to get quantity */
      FIND FIRST po-ordl WHERE po-ordl.company = cocode
          AND po-ordl.po-no   = INT(b-fg-rctd.po-no)
          AND po-ordl.i-no    = b-fg-rctd.i-no
          AND po-ordl.job-no  = b-fg-rctd.job-no
          AND po-ordl.job-no2 = b-fg-rctd.job-no2
          AND po-ordl.item-type = NO
        NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN
        FIND FIRST po-ordl WHERE po-ordl.company = cocode
            AND po-ordl.po-no   = INTEGER(b-fg-rctd.po-no)
            AND po-ordl.i-no    = b-fg-rctd.i-no
            AND po-ordl.item-type = NO
          NO-LOCK NO-ERROR.
      
      
      IF AVAIL po-ordl THEN
      ASSIGN
        v-len = po-ordl.s-len
        v-wid = po-ordl.s-wid.
        lv-calc-cost = ip-cost-to-set.
        lv-recalc-cost = lv-calc-cost.
        
      IF fg-rctd.cost-uom EQ b-fg-rctd.cost-uom               OR
        (LOOKUP(fg-rctd.cost-uom,fg-uom-list) GT 0 AND
        LOOKUP(b-fg-rctd.cost-uom,fg-uom-list) GT 0)   THEN.
      ELSE
        RUN rm/convcuom.p(fg-rctd.cost-uom, b-fg-rctd.cost-uom,
                          v-bwt, v-len, v-wid, v-dep,
                          lv-calc-cost, OUTPUT lv-recalc-cost).
      
      b1-fg-rctd.std-cost = lv-recalc-cost.
      ASSIGN
      lv-ext-cost = b1-fg-rctd.t-qty * b1-fg-rctd.std-cost
      b1-fg-rctd.ext-cost = lv-ext-cost + b1-fg-rctd.frt-cost.
    END.
    
  END.
END.
IF ip-on-screen THEN DO:
  
  lv-out-qty = lv-out-qty + fg-rctd.t-qty.
END.

op-out-qty = lv-out-qty.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-set-full-qty B-table-Win 
PROCEDURE get-set-full-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-cost-to-set AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-on-screen AS LOG NO-UNDO.
  DEF OUTPUT PARAMETER op-out-qty AS DEC NO-UNDO.
  DEF VAR v-len LIKE po-ordl.s-len NO-UNDO.
  DEF VAR v-wid LIKE po-ordl.s-len NO-UNDO.
  DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO. 
  DEF VAR v-bwt LIKE po-ordl.s-len NO-UNDO.
  DEF VAR lv-out-qty AS DEC NO-UNDO.
  DEF VAR lv-out-cost AS DEC NO-UNDO.
  DEF VAR lv-calc-cost AS DEC.
  DEF VAR lv-recalc-cost AS DEC.
  DEF VAR lv-ext-cost AS DEC.
  DEF VAR v-rec-qty AS INT NO-UNDO.
  DEF VAR ll-ea AS LOG NO-UNDO.

  DEF BUFFER b-fg-rctd FOR fg-rctd.
  DEF BUFFER b1-fg-rctd FOR fg-rctd.


  cocode = g_company.

  lv-out-qty = 0.
  FOR EACH b-fg-rctd WHERE b-fg-rctd.company EQ g_company AND
           (b-fg-rctd.rita-code EQ "R" OR b-fg-rctd.rita-code EQ "E")
           AND trim(b-fg-rctd.job-no) = trim(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
           AND b-fg-rctd.job-no2 = INT(fg-rctd.job-no2:SCREEN-VALUE)
           AND b-fg-rctd.i-no = fg-rctd.i-no:SCREEN-VALUE 
           AND (RECID(b-fg-rctd) <> recid(fg-rctd) 
                OR (adm-new-record AND NOT adm-adding-record))
           AND b-fg-rctd.SetHeaderRno EQ INTEGER(SUBSTRING(lv-linker, 10, 10))
           AND b-fg-rctd.SetHeaderRno GT 0
           NO-LOCK:     


      lv-out-qty = lv-out-qty + b-fg-rctd.t-qty.     
      IF ip-cost-to-set GT 0 THEN DO:

          /* convert cost to b1-fg-rctd uom */

          FIND b1-fg-rctd WHERE ROWID(b1-fg-rctd) EQ ROWID(b-fg-rctd)
              EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL b1-fg-rctd THEN DO WITH FRAME {&FRAME-NAME}:
        
            FIND itemfg WHERE itemfg.company EQ cocode
                          AND itemfg.i-no  EQ b-fg-rctd.i-no
                        USE-INDEX i-no NO-LOCK NO-ERROR.
            
            ASSIGN
              v-bwt             = 0
              v-dep             = 0.
            
            IF AVAIL itemfg THEN
              ASSIGN v-len       = itemfg.t-len
                     v-wid       = itemfg.t-wid.
            
            /* Always find just to get quantity */
            FIND FIRST po-ordl WHERE po-ordl.company = cocode
                                 AND po-ordl.po-no   = int(b-fg-rctd.po-no)
                                 AND po-ordl.i-no    = b-fg-rctd.i-no
                                 AND po-ordl.job-no  = b-fg-rctd.job-no
                                 AND po-ordl.job-no2 = b-fg-rctd.job-no2
                                 AND po-ordl.item-type = NO
                                 NO-LOCK NO-ERROR.
            IF NOT AVAIL po-ordl THEN
                FIND FIRST po-ordl WHERE po-ordl.company = cocode
                                     AND po-ordl.po-no   = integer(b-fg-rctd.po-no)
                                     AND po-ordl.i-no    = b-fg-rctd.i-no
                                     AND po-ordl.item-type = NO
                                     NO-LOCK NO-ERROR.
            
            
            IF AVAIL po-ordl THEN
              ASSIGN
                v-len = po-ordl.s-len
                v-wid = po-ordl.s-wid.
            lv-calc-cost = ip-cost-to-set.
            lv-recalc-cost = lv-calc-cost.
            IF fg-rctd.cost-uom EQ b-fg-rctd.cost-uom               OR
              (LOOKUP(fg-rctd.cost-uom,fg-uom-list) GT 0 AND
               LOOKUP(b-fg-rctd.cost-uom,fg-uom-list) GT 0)   THEN.
            ELSE
               RUN rm/convcuom.p(fg-rctd.cost-uom, b-fg-rctd.cost-uom, 
                                 v-bwt, v-len, v-wid, v-dep,
                                 lv-calc-cost, OUTPUT lv-recalc-cost).
            
            b1-fg-rctd.std-cost = lv-recalc-cost.
            ASSIGN
             lv-ext-cost = b1-fg-rctd.t-qty * b1-fg-rctd.std-cost                          
             b1-fg-rctd.ext-cost = lv-ext-cost + b1-fg-rctd.frt-cost.
          END.

      END.
  END.
  IF ip-on-screen THEN
    lv-out-qty = lv-out-qty + int(fg-rctd.t-qty:SCREEN-VALUE).
  op-out-qty = lv-out-qty.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-values B-table-Win 
PROCEDURE get-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-loc AS CHAR NO-UNDO.
  DEF VAR lv-loc-bin AS CHAR NO-UNDO.
  DEF VAR lv-qty-case AS CHAR NO-UNDO.
  DEF VAR lv-cost-uom AS CHAR NO-UNDO.
  DEF VAR lv-std-cost AS CHAR NO-UNDO.
  DEF VAR v-cost AS DEC DECIMALS 10 NO-UNDO.
  DEF VAR lv-save AS CHAR EXTENT 20 NO-UNDO.
  DEF VAR ll-ea AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST itemfg
        {sys/look/itemfgrlW.i}
          AND itemfg.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN DO:
        
        /* Assign from itemfg only if blank. Don't overwrite if name already set from PO. */
        IF fg-rctd.i-name:SCREEN-VALUE = "" OR fg-rctd.po-no:SCREEN-VALUE = "" AND AVAIL itemfg THEN
            ASSIGN fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name.
            
    
    /*     find first fg-ctrl where fg-ctrl.company eq cocode no-lock no-error.  */
        
        ASSIGN
         lv-qty-case = STRING(itemfg.case-count)
         lv-cost-uom = IF itemfg.pur-man THEN itemfg.pur-uom ELSE itemfg.prod-uom.
    
        RUN fg/autopost.p (ROWID(itemfg),
                           fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                           INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                           OUTPUT lv-loc, OUTPUT lv-loc-bin).
        
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ cocode
              AND fg-bin.loc     EQ lv-loc
              AND fg-bin.loc-bin EQ lv-loc-bin
              AND fg-bin.i-no    EQ ""
            NO-LOCK NO-ERROR.
        IF AVAIL fg-bin THEN 
          ASSIGN
           lv-std-cost = IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" AND
                                                      fg-rctd.job-no:SCREEN-VALUE = "" 
                                                   THEN STRING(itemfg.last-cost) 
                                                   ELSE lv-std-cost
           lv-qty-case = /*IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" and
                                                      fg-rctd.job-no:SCREEN-VALUE = "" 
                                                    THEN   STRING(itemfg.case-count)
                                                    ELSE lv-qty-case
                                                    */
                                                    STRING(itemfg.case-count)
           lv-cost-uom = itemfg.prod-uom.
    
        ASSIGN
         lv-save[1] = fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}
         lv-save[2] = fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}.
    
        RUN get-fg-bin-cost.
    END.
    ASSIGN
     lv-std-cost = fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-cost-uom = fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}

     fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = lv-save[1]
     fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = lv-save[2].

    /**  Find the Job Header record in then job file and use Standard Cost
         from that job. **/
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ cocode
          AND job-hdr.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND job-hdr.job-no2 EQ int(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.

    IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job.job-no2 EQ int(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable-job
          WHERE reftable-job.reftable EQ "jc/jc-calc.p"
            AND reftable-job.company  EQ job.company
            AND reftable-job.loc      EQ ""
            AND reftable-job.code     EQ STRING(job.job,"999999999")
            AND reftable-job.code2    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
    END.

    IF AVAIL job-hdr AND job-hdr.std-tot-cost GT 0 THEN
      ASSIGN
       lv-cost-uom = "M"
       lv-std-cost = STRING(job-hdr.std-tot-cost).
    ELSE
    IF AVAIL reftable-job AND reftable-job.val[5] GT 0 THEN
      ASSIGN
       lv-cost-uom = "M"
       lv-std-cost = STRING(reftable-job.val[5]).

    /** If no Job Header is avail for this Job# then Find the Item
        record for then item and use Standard Cost from that item. **/
    ELSE DO:
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ cocode           
            AND po-ordl.po-no     EQ int(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.i-no      EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND po-ordl.item-type EQ NO
          NO-LOCK NO-ERROR.
          
      IF AVAIL po-ordl THEN DO:
        ASSIGN
         lv-cost-uom = po-ordl.pr-uom.
         lv-std-cost = STRING(po-ordl.cost * (IF po-ordl.disc NE 0 THEN (1 - (po-ordl.disc / 100)) ELSE 1)).

        RUN convert-vend-comp-curr(INPUT-OUTPUT lv-std-cost).

        RUN show-freight.
        
      END.
     
      ELSE
      IF AVAIL itemfg          AND
         DEC(lv-std-cost) EQ 0 THEN DO:
        ASSIGN
         lv-cost-uom = itemfg.prod-uom
         lv-std-cost = STRING(itemfg.total-std-cost).

        IF /*itemfg.total-std-cost EQ 0 AND*/ itemfg.isaset THEN DO:
            RUN fg/costset.p (ROWID(itemfg), OUTPUT v-cost).

          IF lv-cost-uom NE "M" THEN DO:
            RUN sys/ref/ea-um-fg.p (lv-cost-uom, OUTPUT ll-ea).
            IF ll-ea THEN lv-cost-uom = "EA".
            RUN sys/ref/convcuom.p("M", lv-cost-uom,
                                   0, 0, 0, 0, v-cost, OUTPUT v-cost).
            IF ll-ea THEN lv-cost-uom = itemfg.prod-uom.
          END.
          lv-std-cost = STRING(v-cost).
        END.
      END.
    END.

    /* #pn# If there is a tag, quantites should default from there */
    /* #pn# task 10311308                                          */
    IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} GT "" THEN
        RUN new-tag.
    IF fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     EQ "" OR
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
      ASSIGN
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = lv-loc
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = lv-loc-bin.

    IF INT(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = lv-qty-case.

    IF fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
      fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = lv-cost-uom.

    IF DEC(fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = lv-std-cost.

    IF INT(fg-rctd.cases-unit:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      fg-rctd.cases-unit:SCREEN-VALUE IN BROWSE {&browse-name} = "1".

    IF fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} EQ ? 
        OR fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} EQ "?" THEN
     fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = "0".

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc B-table-Win 
PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN get-link-handle IN adm-broker-hdl
                       (THIS-PROCEDURE,'linker-source':U,OUTPUT char-hdl).
  ll-set-parts = VALID-HANDLE(WIDGET-HANDLE(char-hdl)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR ls-tmp-qty AS cha NO-UNDO.
  DEF VAR ls-tmp-uom AS cha NO-UNDO.
  DEF VAR ls-tmp-cst AS cha NO-UNDO.
  DEF VAR v-next-tag AS cha NO-UNDO.
  DEF VAR v-full-qty AS DEC NO-UNDO.
  DEF VAR lQtyChanged AS LOG NO-UNDO.
  DEF VAR lOK AS LOG NO-UNDO.
  DEFINE VARIABLE iLinker AS INTEGER NO-UNDO. 
  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Needed since the newly created row can become unavailable for some reason */
  IF NOT AVAIL fg-rctd AND lrMissingRow NE ? THEN DO:
      FIND fg-rctd 
        WHERE ROWID(fg-rctd) EQ lrMissingRow
      NO-LOCK NO-ERROR.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     v-copy-mode = NO
     ls-tmp-qty = fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}
     ls-tmp-uom = fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}
     ls-tmp-cst = fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name}.
    
  END.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */                
  IF DEC(ls-tmp-qty) NE fg-rctd.t-qty THEN lQtyChanged = YES.
  ASSIGN
   fg-rctd.t-qty    = DEC(ls-tmp-qty)
   fg-rctd.pur-uom  = ls-tmp-uom
   fg-rctd.cost-uom = ls-tmp-uom
   fg-rctd.ext-cost = fg-rctd.std-cost * fg-rctd.t-qty /
                      (IF fg-rctd.cost-uom EQ "M" THEN 1000 ELSE 1).
  IF fg-rctd.po-no GT "" THEN DO:
      FIND FIRST po-ord WHERE po-ord.company EQ fg-rctd.company
          AND po-ord.po-no EQ INTEGER(fg-rctd.po-no)
          NO-LOCK NO-ERROR.
      IF AVAIL po-ord THEN
          FIND FIRST po-ordl WHERE po-ordl.company EQ po-ord.company
              AND po-ordl.po-no EQ po-ord.po-no
              AND po-ordl.i-no  EQ fg-rctd.i-no 
          NO-LOCK NO-ERROR.
      DEF BUFFER bfItemfg FOR itemfg.
      IF AVAIL po-ordl THEN
          FIND FIRST itemfg WHERE itemfg.company EQ po-ordl.company
            AND itemfg.i-no EQ po-ordl.i-no 
          NO-LOCK NO-ERROR.
     
      IF AVAIL itemfg AND itemfg.pur-man = TRUE AND AVAIL(po-ordl) AND po-ordl.job-no GT "" THEN
          ASSIGN fg-rctd.job-no = po-ordl.job-no
                 fg-rctd.job-no2 = po-ordl.job-no2.

      
  END.
  IF ll-set-parts THEN DO:
    FIND FIRST fg-rcpts WHERE fg-rcpts.r-no EQ fg-rctd.r-no NO-ERROR.
    IF NOT AVAIL fg-rcpts THEN DO:
      iLinker = INTEGER(SUBSTRING(lv-linker, 10, 10)) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN 
        fg-rctd.setHeaderRno = iLinker. 
      CREATE fg-rcpts.
      fg-rcpts.r-no       = fg-rctd.r-no.
    END.
    ASSIGN
     fg-rcpts.company    = cocode
     fg-rcpts.i-no       = fg-rctd.i-no
     fg-rcpts.i-name     = fg-rctd.i-name
     fg-rcpts.trans-date = fg-rctd.rct-date
     fg-rcpts.linker     = lv-linker.
  END.

  ELSE DO: 
      FIND FIRST itemfg WHERE itemfg.company EQ fg-rctd.company
             AND itemfg.i-no EQ fg-rctd.i-no 
           NO-LOCK NO-ERROR.
      IF lQtyChanged AND CAN-FIND(FIRST fg-rcpts
                          WHERE fg-rcpts.company EQ cocode
                            AND fg-rcpts.linker  EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")) 
                     AND NOT (FGSetRec-int EQ 1 AND itemfg.alloc NE YES) THEN DO:
          MESSAGE "Set Parts Receipts will be reset since the set header quantity was changed. Please review the Set Parts tab."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RUN DeleteSetParts (INPUT ("fg-rctd: " + STRING(fg-rctd.r-no,"9999999999"))).
      END.
      IF NOT (FGSetRec-Int EQ 1 AND itemfg.alloc NE YES) THEN
        RUN fg/comprcpt.p (ROWID(fg-rctd)).
  END.
      
  
  IF v-auto-add-tag AND fg-rctd.tag EQ "" THEN DO:
     RUN get-next-tag (INPUT fg-rctd.i-no, OUTPUT v-next-tag).
     RUN create-loadtag (INPUT-OUTPUT v-next-tag, INPUT ROWID(fg-rctd)).
     fg-rctd.tag = v-next-tag.
  END.
  
  /* If total quantity was used for cost, update the other records with the new cost */  
  IF lv-cost-basis = "FULLQTY" THEN DO:  
      RUN get-set-full-qty (INPUT fg-rctd.std-cost, INPUT YES, OUTPUT v-full-qty).
  END.
     


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR phandle AS HANDLE NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

/*   IF NOT AVAIL fg-rctd AND NOT adm-new-record THEN DO:             */
/*       RUN local-open-query.                                        */
/*       RUN get-link-handle IN adm-broker-hdl                        */
/*         (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).       */
/*       phandle = WIDGET-HANDLE(char-hdl).                           */
/*       /* if fg-rctd is not available, something in out of sync, */ */
/*       /* since the add button is enabled, simulate pressing add */ */
/*       /* then cancel to clear the current record properly */       */
/*                                                                    */
/*       RUN auto-save    IN phandle.                                 */
/*       RUN auto-cancel in phandle.                                  */
/*                                                                    */
/*       RETURN.                                                      */
/*   END.     
           */                                             
                                                        
IF adm-new-record THEN
    RUN delete-tt.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .
  /* Code placed here will execute AFTER standard behavior.    */

  v-copy-mode = NO.

 /* RUN auto-add-tt.*/  /*Mode 001*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  v-copy-mode = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
  DEF BUFFER b-fg-rctd FOR fg-rctd.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  lv-cost-basis = "".
  FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .  

  /* Code placed here will execute AFTER standard behavior.    */
  DO WHILE TRUE:
    lv-rno = lv-rno + 1.
    FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth THEN NEXT.
    FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL b-fg-rctd THEN NEXT.
    LEAVE.
  END.

  FIND FIRST tt-fg-rctd NO-ERROR.

  IF AVAIL tt-fg-rctd THEN DO:
    BUFFER-COPY tt-fg-rctd EXCEPT rec_key TO fg-rctd.
    tt-fg-rctd.tt-rowid = ROWID(fg-rctd).
  END.

  ELSE
  IF adm-adding-record THEN DO:
    ASSIGN
     fg-rctd.rct-date    = TODAY
     fg-rctd.trans-time   = TIME
     fg-rctd.units-pallet = 1
     fg-rctd.cases-unit   = 1
     fg-rctd.ext-cost     = 0
     fg-rctd.partial      = 0
     fg-rctd.qty          = 0
     fg-rctd.qty-case     = 0.

    /*BV - This code sets the new receipt date to the latest existing receipt date. This code
    was identified as causing slowness at Hughes, however we found a bunch of "orphaned" set parts receipts 
    that should be handled separately (04181326)*/

    /*BV - This code sets the new receipt date to the latest existing receipt date. This code
    was identified as causing slowness at Hughes, however we found a bunch of "orphaned" set parts receipts 
    that should be handled separately (04181326)*/
    FOR EACH b-fg-rctd NO-LOCK
        WHERE b-fg-rctd.company   EQ g_company
          AND b-fg-rctd.rita-code EQ "R"
          AND ROWID(b-fg-rctd)    NE ROWID(fg-rctd)
          AND b-fg-rctd.SetHeaderRno EQ 0
        BY b-fg-rctd.r-no DESC:  /*Last one added, not necessarily the last date*/
        
      fg-rctd.rct-date = b-fg-rctd.rct-date.
      
      LEAVE.
    END.
  
  END.  

  ASSIGN
   fg-rctd.company   = g_company
   fg-rctd.r-no      = lv-rno
   fg-rctd.rita-code = "R"
   /* gdm - */
   fg-rctd.trans-time   = TIME
   .   

  IF adm-adding-record THEN DISP fg-rctd.rct-date  fg-rctd.cases-unit WITH BROWSE {&browse-name}. 


/*
  run tag-method (output lv-tag-meth). 
  /*  if lv-tag-meth and fg-rctd:po-no:screen*/
  run tag-sequence.
*/  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-fg-rctd FOR fg-rctd.

  DEF VAR ll AS LOG NO-UNDO.

  v-copy-mode = NO.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.
  
 /* RUN DeleteSetParts (INPUT "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")).*/
  FOR EACH fg-rcpts                                                              
       WHERE fg-rcpts.company EQ cocode                                           
         AND fg-rcpts.linker  EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
       NO-LOCK: 
                                                                                  
     FOR EACH b-fg-rctd                                                           
         WHERE b-fg-rctd.company EQ cocode                                        
           AND b-fg-rctd.r-no    EQ fg-rcpts.r-no
         EXCLUSIVE-LOCK USE-INDEX fg-rctd:                                
       DELETE b-fg-rctd.                                                          
     END.                                                                         
   END.                                                                        

  RUN delete-tt.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN reset-cursor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR out-hd-lst AS cha NO-UNDO.
  DEF VAR ii AS INT NO-UNDO.
  DEF VAR hd-next AS WIDGET-HANDLE NO-UNDO.

  lv-rct-date-checked = NO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    IF AVAIL fg-rctd THEN DO:
      ASSIGN
       lv-job-no  = fg-rctd.job-no
       lv-job-no2 = STRING(fg-rctd.job-no2).
    END.

    ELSE
      ASSIGN
       lv-job-no  = ""
       lv-job-no2 = "00".

    IF ll-set-parts THEN
      APPLY "entry" TO fg-rctd.tag IN BROWSE {&browse-name}.
    ELSE
      APPLY "entry" TO fg-rctd.rct-date IN BROWSE {&browse-name}.
  END.
  /*
  IF fg-rctd.cases-unit:SCREEN-VALUE IN BROWSE {&browse-name} EQ ? 
      OR fg-rctd.cases-unit:SCREEN-VALUE IN BROWSE {&browse-name} EQ "?" THEN
    fg-rctd.cases-unit:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
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
  IF ll-set-parts THEN DO:
    lv-linker = "".
    RUN get-link-handle IN adm-broker-hdl
                       (THIS-PROCEDURE,'linker-source':U,OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN get-linker IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-linker).
    IF lv-linker EQ "" THEN RETURN "adm-error".
  END.

  RUN get-first-r-no.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-changed B-table-Win 
PROCEDURE local-row-changed :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-changed':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN set-query.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
                   
  /* Code placed here will execute PRIOR to standard behavior. */
    DEF BUFFER bf-rctd FOR fg-rctd .
    DEF BUFFER bf-reftable FOR reftable .
    DEF VAR v-tag-change AS CHAR NO-UNDO.
    DEF VAR v-tag-change2 AS CHAR NO-UNDO.
    DEF VAR cval AS CHAR NO-UNDO.
    DEF VAR lNegative AS LOG NO-UNDO.
    DEF VAR lNewTagRan AS LOG NO-UNDO.
    DEF VAR hPanel AS HANDLE NO-UNDO.
  /* Needed since the newly created row can become unavailable for some reason */
  lrMissingRow = ?.
  IF NOT AVAIL fg-rctd AND INTEGER(fg-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN DO:
      FIND FIRST fg-rctd
        WHERE fg-rctd.r-no EQ INTEGER(fg-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
      NO-LOCK NO-ERROR.
      IF AVAIL fg-rctd THEN
        lrMissingRow = ROWID(fg-rctd).      
  END.

  ASSIGN
    lv-cost-basis = ""
    lNewTagRan    = FALSE
    lNegative     = FALSE.
  DO WITH FRAME {&FRAME-NAME}:
     v-tag-change = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} .
    IF NOT lv-rct-date-checked THEN DO:
       DEF VAR lv-rct-date AS cha NO-UNDO.
       lv-rct-date = fg-rctd.rct-date:SCREEN-VALUE IN BROWSE {&browse-name}.
       {custom/currentDatePrompt.i lv-rct-date}
    END.

    IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE fg-rctd.tag 
      AND NOT adm-new-record THEN DO:
      /* Note: if adm-new-record, tag will always be different, */
      /*       and cases will be overriden                      */
      RUN new-tag.
      lNewTagRan = TRUE.
    END.
      
    IF fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     EQ ""      OR
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""      OR
       INT(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 OR
       fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""     OR
       DEC(fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      RUN get-values.
  END.

  RUN get-matrix (NO).

  RUN valid-tag (fg-rctd.tag:HANDLE, OUTPUT lNegative) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  IF lNegative AND lNewTagRan THEN DO:
    IF lNegative AND INTEGER(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN
      fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} = 
             STRING(INTEGER(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) * -1).
  END.

  RUN valid-lot# (fg-rctd.stack-code:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-po-no (1) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  IF NOT CAN-FIND(FIRST tt-fg-rctd WHERE tt-rowid EQ ROWID(fg-rctd)) THEN
    RUN update-ttt.
  
  IF ll-set-parts THEN DO: 
    RUN valid-qty (fg-rctd.t-qty:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
  END.
  

  /* Run with check on password, if relevant */
  RUN valid-job-no (INPUT YES) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-job-no2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  IF lv-prev-job2 <> fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} AND
     NOT lv-new-job-ran THEN RUN new-job-no.

  IF adm-new-record THEN DO:

    /* Instead of 0 at 500 with -300 partial, make it */
    /* 1 at -500 with 0 partial 03101503              */
    IF INT(fg-rctd.cases:SCREEN-VALUE) EQ 0 AND INT(fg-rctd.partial:SCREEN-VALUE) NE 0 THEN
      ASSIGN 
          fg-rctd.cases:SCREEN-VALUE = STRING((IF  INT(fg-rctd.partial:SCREEN-VALUE) LT 0 THEN -1 ELSE 1))
          fg-rctd.qty-case:SCREEN-VALUE = STRING((IF INT(fg-rctd.partial:SCREEN-VALUE) LT 0 THEN - INT(fg-rctd.partial:SCREEN-VALUE) ELSE INT(fg-rctd.partial:SCREEN-VALUE)))
          fg-rctd.partial:SCREEN-VALUE = "0"
          .
  END.



  RUN validate-record NO-ERROR.

  IF ERROR-STATUS:ERROR THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
     
    IF VALID-HANDLE(HANDLE(char-hdl)) THEN DO:
   
      hPanel = HANDLE(char-hdl).
      RUN notify IN hPanel (INPUT 'cancel-record':U).
      RUN delete-tt.
      ASSIGN
       adm-adding-record = NO
       adm-new-record    = NO
       lv-rct-date-checked = NO   .
      RUN reset-cursor.
      RETURN ERROR.
    END.
      

  END.

  /* Needed since the fg-rctd can become unavailable for some reason */
  lrMissingRow = ?.
  IF NOT AVAIL fg-rctd AND INTEGER(fg-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN DO:
      FIND FIRST fg-rctd
        WHERE fg-rctd.r-no EQ INTEGER(fg-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
      NO-LOCK NO-ERROR.
      IF AVAIL fg-rctd THEN
        lrMissingRow = ROWID(fg-rctd).
  END.
   ASSIGN v-tag-change2 = fg-rctd.tag  .
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN delete-tt.

  ASSIGN lv-new-job-ran = NO
         lv-prev-job2 = "".


  IF NOT ll-set-parts THEN RUN fg/invrecpt.p (ROWID(fg-rctd), 1).
  
/*   IF v-tag-change NE  v-tag-change2 AND fg-rctd.tag NE "" THEN                                                */
/*   IF ll-set-parts THEN do:  /* task 12041308 */                                                               */
/*                                                                                                               */
/*       FIND FIRST bf-rctd WHERE bf-rctd.company eq cocode                                                      */
/*           AND bf-rctd.r-no ge lv-frst-rno                                                                     */
/*           AND bf-rctd.i-no EQ fg-rctd.i-no                                                                    */
/*           AND rowid(bf-rctd) <> ROWID(fg-rctd)                                                                */
/*           AND (bf-rctd.rita-code eq "R" or bf-rctd.rita-code eq "E") EXCLUSIVE-LOCK NO-ERROR.                 */
/*       IF AVAIL bf-rctd THEN                                                                                   */
/*       FIND FIRST bf-reftable WHERE bf-reftable.reftable EQ "fg-rctd.user-id" AND                              */
/*           bf-reftable.company  EQ cocode AND                                                                  */
/*           bf-reftable.loc      EQ STRING(bf-rctd.r-no,"9999999999") AND                                       */
/*           (bf-reftable.dscr    EQ lv-linker AND bf-reftable.dscr begins "fg-rctd: ") exclusive-LOCK NO-ERROR. */
/*         IF AVAIL bf-reftable THEN do:                                                                         */
/*           ASSIGN                                                                                              */
/*             bf-rctd.tag      = fg-rctd.tag                                                                    */
/*             bf-rctd.job-no   = fg-rctd.job-no                                                                 */
/*             bf-rctd.job-no2  = fg-rctd.job-no2                                                                */
/*             bf-reftable.CODE = reftable.code                                                                  */
/*             bf-reftable.CODE2 = reftable.code2 .                                                              */
/*                                                                                                               */
/*           IF bf-rctd.std-cost = 0 THEN DO:                                                                    */
/*               ASSIGN                                                                                          */
/*                 bf-rctd.cost-uom    = fg-rctd.cost-uom                                                        */
/*                 bf-rctd.cases       = -(fg-rctd.cases)                                                        */
/*                 bf-rctd.qty-case    = fg-rctd.qty-case                                                        */
/*                 bf-rctd.cases-unit  = fg-rctd.cases-unit                                                      */
/*                 bf-rctd.partial     = fg-rctd.partial                                                         */
/*                 bf-rctd.std-cost    = fg-rctd.std-cost                                                        */
/*                 bf-rctd.t-qty       = -(fg-rctd.t-qty)                                                        */
/*                 bf-rctd.frt-cost    = fg-rctd.frt-cost                                                        */
/*                 bf-rctd.ext-cost    = -(fg-rctd.ext-cost)  .                                                  */
/*           END.                                                                                                */
/*                                                                                                               */
/*         END.                                                                                                  */
/*   END. /* ll-set-parts */ /* task 12041308 */                                                                 */
/*                                                                                                               */
  RUN repo-query (ROWID(fg-rctd)).

  RUN reset-cursor.

  ASSIGN
   adm-adding-record = NO
   adm-new-record    = NO
   lv-rct-date-checked = NO   .
  
  RUN auto-add-tt. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-no B-table-Win 
PROCEDURE new-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    lv-closed-checked = NO.
    lv-new-job-ran = YES.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ fg-rctd.company
          AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND job-hdr.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        BREAK BY job-hdr.frm      DESC
              BY job-hdr.blank-no DESC:

      IF LAST(job-hdr.frm)                                                  OR
         job-hdr.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} THEN DO:
        ASSIGN
         lv-job-no                     = fg-rctd.job-no:SCREEN-VALUE
         lv-job-no2                    = fg-rctd.job-no2:SCREEN-VALUE
         fg-rctd.i-no:SCREEN-VALUE     = job-hdr.i-no
         fg-rctd.std-cost:SCREEN-VALUE = STRING(job-hdr.std-mat-cost +
                                                job-hdr.std-lab-cost +
                                                job-hdr.std-fix-cost +
                                                job-hdr.std-var-cost).

        RUN get-def-values.

        LEAVE.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-qty B-table-Win 
PROCEDURE new-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR liDummy AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    liDummy =  BROWSE {&browse-name}:NUM-SELECTED-ROWS.

    /* should never be here if no row is selected */
    IF liDummy EQ 0 THEN
        RETURN ERROR.

    fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(INT(fg-rctd.cases:SCREEN-VALUE) *
               INT(fg-rctd.qty-case:SCREEN-VALUE) +
               INT(fg-rctd.partial:SCREEN-VALUE)).
    
    IF NOT adm-new-record OR ll-qty-case-ent THEN DO:
      RUN show-freight.

      RUN get-matrix (NO).
    END.
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
 DEF VAR hProc       AS HANDLE NO-UNDO.
 DEF VAR dMaxQty     AS DECIMAL NO-UNDO.
 DEF VAR cHeaderItem AS CHAR NO-UNDO.
 DEF VAR dMaxCompQty AS DEC NO-UNDO. /* Max component quantity */
 DEF VAR dTotalQty   AS DEC NO-UNDO.
 DEF BUFFER bfItemfg FOR itemfg.

DO WITH FRAME {&FRAME-NAME}:

    IF NOT ll-set-parts THEN DO:      
        FIND FIRST loadtag NO-LOCK
            WHERE loadtag.company   EQ cocode
              AND loadtag.item-type EQ NO
              AND loadtag.tag-no    EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
            NO-ERROR.
        
        IF AVAIL loadtag THEN DO:
            fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = loadtag.i-no.
            fg-rctd.stack-code:SCREEN-VALUE IN BROWSE {&browse-name} = loadtag.misc-char[2] .  /* task 12051302 */
            IF INTEGER(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) = 0 THEN
            fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(loadtag.case-bundle).
            IF INTEGER(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}) = 0 THEN
            fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(loadtag.qty-case).
            IF INTEGER(loadtag.case-bundle) > 1 AND INTEGER(fg-rctd.cases-unit:SCREEN-VALUE IN BROWSE {&browse-name}) = 1 THEN
                  fg-rctd.cases-unit:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(loadtag.case-bundle).
            /* Task 12061305 */  
            IF loadtag.job-no <> "" THEN DO:
                ASSIGN
                    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = loadtag.job-no
                    fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(STRING(loadtag.job-no2)))) +
                                                                           TRIM(STRING(loadtag.job-no2)).
                IF NOT fgRecptPassWord-log THEN
                    RUN get-job-no (INPUT YES) NO-ERROR.
                ELSE
                    /* run with 'no' so no message until save */
                    RUN get-job-no (INPUT NO) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            END.  /* Task 12061305 */
            IF adm-new-record THEN
                fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = string(loadtag.po-no). 
        END.
    END.
    ELSE DO:
        FIND FIRST fg-bin NO-LOCK
            WHERE fg-bin.company EQ cocode
              AND fg-bin.tag EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
              AND fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE ""
              NO-ERROR.
       
        /* Obtain quantity of set header record */                        
       dMaxCompQty = maxComponentQty().
        IF AVAIL fg-bin  THEN DO:

          /* dTotalQty is the qty in other lines with the same item number */
          RUN get-set-full-qty (INPUT fg-bin.std-tot-cost, INPUT YES, OUTPUT dTotalQty).
          
          dTotalQty = dTotalQty - DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}).
          
          /* dMaxCompQty is the max that can be used from fg-bin */
          IF ABS(dMaxCompQty) GT 0 AND ABS(dTotalQty) GT 0 THEN
            dMaxCompQty = dMaxCompQty - ABS(dTotalQty).
          IF dMaxCompQty LT 0 THEN
            dMaxCompQty = 0.
          IF /*ABS(DECIMAL(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name})) GE fg-bin.qty */ 
              ABS(dTotalQty) + ABS(fg-bin.qty) LE ABS(dMaxCompQty) THEN
            ASSIGN
              fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.i-no
              fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(-1 * TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0))
              fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.case-count)
              fg-rctd.cases-unit:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.cases-unit)
              fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(-1 * fg-bin.partial-count)
              fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(-1 * fg-bin.qty).
          ELSE
            ASSIGN            
              fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.i-no
              fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} = "0" /* STRING(-1 * TRUNC(dMaxCompQty / fg-bin.case-count,0)) */
              fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(dMaxCompQty)
              fg-rctd.cases-unit:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.cases-unit)
              fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(-1 * dMaxCompQty)
              fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(-1 * dMaxCompQty).

            /* Task 12061305 */
            IF fg-bin.job-no <> "" THEN
                ASSIGN
                  fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.job-no
                  fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = FILL(" ",6 - LENGTH(TRIM(STRING(fg-bin.job-no2)))) +
                                                                          TRIM(STRING(fg-bin.job-no2)).
        END.
    END.
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
      

    /* Needed since browse went blank after adding an item */
    RUN local-open-query.

    RUN clear_auto_find.

    RUN change-order (browse-order:SCREEN-VALUE).

    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.

  END.

  RUN dispatch ('row-changed').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-cursor B-table-Win 
PROCEDURE reset-cursor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-right' TO {&BROWSE-NAME}.
    END.
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-bin B-table-Win 
PROCEDURE select-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE lv-rowids      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE li             AS INTEGER      NO-UNDO.
DEFINE VARIABLE iSelectedQty   AS INTEGER      NO-UNDO.
DEFINE VARIABLE v-next-tag     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lv-rno         LIKE fg-rctd.r-no NO-UNDO.
DEFINE VARIABLE full-qty       AS INTEGER      NO-UNDO.
DEFINE VARIABLE rPrevRec       AS ROWID        NO-UNDO.
DEFINE VARIABLE cSelectionItem AS CHARACTER    NO-UNDO.
DEFINE VARIABLE dMaxQty        AS DECIMAL      NO-UNDO.
DEFINE VARIABLE iLinker        AS INTEGER NO-UNDO.

DEF BUFFER bfFgRctd FOR fg-rctd.
DEF BUFFER b-fg-rctd FOR fg-rctd.

IF NOT AVAIL fg-rctd THEN
  RETURN.

dMaxQty = maxComponentQty().
cSelectionItem = fg-rctd.i-no.
iSelectedQty = 0.
gvcCurrentItem = fg-rctd.i-no.

 RUN fg/d-selbin.w (INPUT 3,
                    INPUT ?,
                    INPUT "ALL",
                    INPUT fg-rctd.i-no,
                    INPUT dMaxQty,
                    INPUT lv-linker,
                    OUTPUT lv-rowids).
 
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

   DO WHILE AVAIL fg-rctd AND CAN-FIND(FIRST w-rowid):
     FIND FIRST fg-bin WHERE fg-bin.company EQ fg-rctd.company
       AND fg-bin.i-no EQ fg-rctd.i-no
       AND fg-bin.tag  EQ fg-rctd.tag
       AND fg-bin.loc  EQ fg-rctd.loc
       AND fg-bin.loc-bin EQ fg-rctd.loc-bin
       NO-LOCK NO-ERROR.
   
     IF AVAIL fg-bin THEN
     FIND FIRST w-rowid WHERE w-rowid EQ STRING(ROWID(fg-bin)) NO-ERROR.

     /* If the selected record already exists, remove w-rowid so no duplicates */
     IF AVAIL w-rowid THEN DO:
       
        DELETE w-rowid.        
        iSelectedQty = iSelectedQty + fg-rctd.qty.
     END.
     ELSE DO:
       /* If current tag was not selected by user, delete it */
       IF fg-rctd.i-no = cSelectionItem THEN DO:
         
         FIND CURRENT fg-rctd EXCLUSIVE-LOCK.

         DELETE fg-rctd.
       END.
     END.
     RELEASE fg-rctd.
    
     RUN dispatch ('get-next').
     IF AVAIL(fg-rctd) AND ROWID(fg-rctd) EQ rPrevRec THEN
       LEAVE.
     rPrevRec = ROWID(fg-rctd).

   END. /* do while .. */

   CREATE-FG-RCTD:
   FOR EACH w-rowid:
     
     /* Create fg-rctd from bin */
     FIND FIRST fg-bin WHERE ROWID(fg-bin) EQ TO-ROWID(w-rowid.w-rowid) NO-LOCK NO-ERROR. 

     IF NOT AVAIL fg-bin THEN
       NEXT CREATE-FG-RCTD.

     /* Don't create records with a zero quantity which would happen */
     /* if the selected tags have a qty greater than what's needed   */
     IF ABS(iSelectedQty) GE dMaxQty THEN DO:
    
       LEAVE CREATE-FG-RCTD.

     END.
       
     CREATE bfFgRctd.

     /* Code placed here will execute PRIOR to standard behavior. */
     lv-cost-basis = "".
     FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
     IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

     FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
     IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

     
     DO WHILE TRUE:
       lv-rno = lv-rno + 1.
       FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
       IF AVAIL fg-rcpth THEN NEXT.
       FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
       IF AVAIL b-fg-rctd THEN NEXT.
       LEAVE.
     END.

     ASSIGN
      bfFgRctd.rct-date    = TODAY
      bfFgRctd.trans-time   = TIME
      bfFgRctd.units-pallet = 1
      bfFgRctd.cases-unit   = 1
      bfFgRctd.ext-cost     = 0
      bfFgRctd.partial      = 0
      bfFgRctd.qty          = 0
      bfFgRctd.qty-case     = 0
      bfFgRctd.i-no         = fg-bin.i-no
      bfFgRctd.tag          = fg-bin.tag
      bfFgRctd.po-no        = fg-bin.po-no
      bfFgRctd.loc          = fg-bin.loc
      bfFgRctd.loc-bin      = fg-bin.loc-bin.

     /*BV - This code sets the new receipt date to the latest existing receipt date. This code
     was identified as causing slowness at Hughes, however we found a bunch of "orphaned" set parts receipts 
     that should be handled separately (04181326)*/

     FOR EACH b-fg-rctd NO-LOCK
         WHERE b-fg-rctd.company   EQ g_company
           AND b-fg-rctd.rita-code EQ "R"
           AND ROWID(b-fg-rctd)    NE ROWID(bfFgRctd)         
           AND b-fg-rctd.setHeaderRno EQ 0 /*not a set part receipt*/           
         BY b-fg-rctd.r-no DESC:  /*Last one added, not necessarily the last date*/
       bfFgRctd.rct-date = b-fg-rctd.rct-date.
       LEAVE.
     END.
   

     ASSIGN
      bfFgRctd.company   = g_company
      bfFgRctd.r-no      = lv-rno
      bfFgRctd.rita-code = "R"
      bfFgRctd.trans-time   = TIME
      .   

        ASSIGN bfFgRctd.created-by   = USERID("nosweat")
               bfFgRctd.SetHeaderRno = INTEGER(SUBSTRING(lv-linker, 10, 10)).

      /* as in local-assign logic */
      IF ll-set-parts THEN DO:
        FIND FIRST fg-rcpts WHERE fg-rcpts.r-no EQ bfFgRctd.r-no NO-ERROR.
        IF NOT AVAIL fg-rcpts THEN DO:
          CREATE fg-rcpts.
          fg-rcpts.r-no       = bfFgRctd.r-no.
        END.
        iLinker = INTEGER(SUBSTRING(lv-linker, 10, 10)) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN 
          fg-rctd.setHeaderRno = iLinker. 
        
        ASSIGN
         fg-rcpts.company    = cocode
         fg-rcpts.i-no       = bfFgRctd.i-no
         fg-rcpts.i-name     = bfFgRctd.i-name
         fg-rcpts.trans-date = bfFgRctd.rct-date
         fg-rcpts.linker     = lv-linker. /* Set in local-open-query */
         FIND FIRST itemfg WHERE itemfg.company = g_company
            AND itemfg.i-no = bfFgRctd.i-no  NO-LOCK NO-ERROR.
         IF AVAIL ITEMfg THEN
             ASSIGN bfFgRctd.i-name = itemfg.i-name.
      END.
      
      /* As in local-assign logic */
      IF v-auto-add-tag AND bfFgRctd.tag EQ "" THEN DO:
         RUN get-next-tag (INPUT bfFgRctd.i-no, OUTPUT v-next-tag).
         RUN create-loadtag (INPUT-OUTPUT v-next-tag, INPUT ROWID(bfFgRctd)).
         bfFgRctd.tag = v-next-tag.
      END.
  

      /* If total quantity was used for cost, update the other records with the new cost */  
      IF lv-cost-basis = "FULLQTY" THEN DO:  
          RUN get-set-full-qty (INPUT bfFgRctd.std-cost, INPUT NO, OUTPUT full-qty).
      END.
      
      RUN copyBinInfo (INPUT ROWID(fg-bin), ROWID(bfFgRctd)).
    
      iSelectedQty = iSelectedQty +  bfFgRctd.t-qty.
      RUN dispatch ('open-query').

   END. /* each w-rowid */
 END. /* lv-rowids not blank */
 RUN dispatch ('open-query').


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
  {src/adm/template/snd-list.i "fg-rctd"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-query B-table-Win 
PROCEDURE set-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF NOT ll-set-parts THEN DO:
    RUN get-link-handle IN adm-broker-hdl
                         (THIS-PROCEDURE,'linker-target':U,OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(1,char-hdl))) THEN
      RUN dispatch IN WIDGET-HANDLE(ENTRY(1,char-hdl)) ("open-query").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-bin-info B-table-Win 
PROCEDURE show-bin-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN get-def-values.
    
    ASSIGN
     li = TRUNC(INT(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) /
                INT(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}),0).
    
    ASSIGN
     fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(li)
     fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(INT(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) -
               (li * DEC(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}))).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-freight B-table-Win 
PROCEDURE show-freight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld AS DEC NO-UNDO.


  IF fgpofrt-log THEN 
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ld = DEC(fg-rctd.frt-cost:SCREEN-VALUE IN BROWSE {&browse-name})
     fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} =
         STRING(DEC(fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name}) - ld).

    RUN get-freight-cost (OUTPUT ld).

    ASSIGN
     fg-rctd.frt-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld)
     fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} =
         STRING(DEC(fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name}) + ld).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-method B-table-Win 
PROCEDURE tag-method :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*
  def output parameter op-tag# as log no-undo.
  def var cocode like rm-rcth.company no-undo.
  
  
  {rm/tag#.i}
  op-tag# = v-tag#.
  
  */
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
  DEF VAR v-tag-seq AS INT NO-UNDO.
  DEF VAR v-locode AS cha NO-UNDO.

  
  ASSIGN v-tag-seq = 0
         v-locode  = "".

  DO WHILE TRUE WITH FRAME {&FRAME-NAME}:
    FIND FIRST b-fg-rctd
        WHERE b-fg-rctd.company EQ fg-rctd.company
          AND b-fg-rctd.loc     GT v-locode
        NO-LOCK NO-ERROR.

    IF AVAIL b-fg-rctd THEN DO:
      v-locode = b-fg-rctd.loc.

      FOR EACH b-fg-rctd WHERE b-fg-rctd.company EQ fg-rctd.company
            AND b-fg-rctd.loc     EQ v-locode
            AND b-fg-rctd.tag     BEGINS string(int(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}),"999999")
            USE-INDEX tag NO-LOCK
            BY b-fg-rctd.tag DESC:

           IF int(substr(b-fg-rctd.tag,7,2)) GT v-tag-seq THEN
           v-tag-seq = int(substr(b-fg-rctd.tag,7,2)).
            LEAVE.
      END.
    END.

    ELSE LEAVE.
  END.  /* do while */
/* ======= may not need any more 
  v-locode = "".
  if v-tag-seq eq 0 then do while true:
    find first fg-rctdh"where fg-rctdh.company eq rm-rcth.company
          and fg-rctdh.loc     gt v-locode
        no-lock no-error.

    if avail fg-rctdh then do:
      v-locode = fg-rctdh.loc.

      for each fg-rctdh
          where fg-rctdh.company eq cocode
            and fg-rctdh.loc     eq v-locode
            and fg-rctdh.tag     begins string(int(fg-rctd.po-no),"999999")
          use-index tag no-lock
          by fg-rctdh.tag desc:

        if int(substr(fg-rctdh.tag,7,2)) gt v-tag-seq then
          v-tag-seq = int(substr(fg-rctdh.tag,7,2)).
        leave.
      end.
    end.

    else leave.
  end.
============================== */
  ASSIGN
   v-tag-seq   = v-tag-seq + 1.
/*   fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
          = string(int(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}),"999999") + string(v-tag-seq,"99").
*/          

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
    FIND FIRST tt-fg-rctd NO-ERROR.
    IF AVAIL tt-fg-rctd THEN DO:
      RUN display-po (tt-fg-rctd.po-rowid).
      RUN show-bin-info.
    END.

    IF poSelected EQ 1 THEN
    APPLY "entry" TO fg-rctd.loc IN BROWSE {&browse-name}.
    ELSE
    RUN dispatch ("update-record").
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

  FIND FIRST tt-fg-rctd NO-ERROR.
     
  IF AVAIL tt-fg-rctd THEN DO WITH FRAME {&FRAME-NAME}:

    ASSIGN
     fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = tt-fg-rctd.po-no
     fg-rctd.i-no:SCREEN-VALUE  IN BROWSE {&browse-name} = tt-fg-rctd.i-no
     fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(tt-fg-rctd.t-qty)
     tt-fg-rctd.tt-rowid                                 = ROWID(fg-rctd).

    RUN show-bin-info.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cases B-table-Win 
PROCEDURE valid-cases :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.

  DEF VAR lv-msg AS CHAR NO-UNDO.
  
  DEF VAR llValid AS LOG NO-UNDO.
  DEF VAR lcJobNo AS CHAR NO-UNDO.
  DEF VAR lcJobNo2 AS CHAR NO-UNDO.
  DEF VAR lcLoc AS CHAR NO-UNDO.
  DEF VAR lcLocBin AS CHAR NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF INTE(ip-focus:SCREEN-VALUE)
         GE 0 AND fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} GT "" THEN DO:
  
        IF lv-msg EQ "" AND NOT ll-set-parts AND
           (CAN-FIND(FIRST b-fg-rctd
                     WHERE b-fg-rctd.company   EQ cocode
                       AND b-fg-rctd.tag       EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND b-fg-rctd.rita-code NE "P"
                       AND RECID(b-fg-rctd)    NE RECID(fg-rctd)) OR
            CAN-FIND(FIRST b-fg-rdtlh
                     WHERE b-fg-rdtlh.company   EQ cocode
                       AND b-fg-rdtlh.tag       EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND b-fg-rdtlh.qty       GT 0
                       AND b-fg-rdtlh.rita-code NE "S"))          AND
           (INTE(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-NAME}) > 0 OR
              CAN-FIND(FIRST b2-fg-rdtlh
                       WHERE b2-fg-rdtlh.company   EQ cocode
                         AND b2-fg-rdtlh.tag       EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                         AND b2-fg-rdtlh.qty       LT ABS(INTE(fg-rctd.cases:SCREEN-VALUE))
                         AND b2-fg-rdtlh.rita-code NE "S") ) THEN
          ASSIGN lv-msg = "Tag# has already been used, please re-enter".
  
        IF lv-msg EQ "" AND NOT ll-set-parts AND v-copy-mode AND ip-focus:SCREEN-VALUE NE "" AND
           CAN-FIND(FIRST b-fg-rctd
                     WHERE b-fg-rctd.company EQ cocode
                       AND b-fg-rctd.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND b-fg-rctd.rita-code NE "P"
                       AND b-fg-rctd.r-no    NE 0) THEN
           ASSIGN lv-msg = "Tag# has already been used, please re-enter".
        
        IF lv-msg NE "" THEN DO:
          MESSAGE "Tag# has already been used, please enter a negative quantity" + "..." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO ip-focus.
          RETURN ERROR.
        END.
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
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.
  DEF VAR lActive AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF oeship-log                   AND
       CAN-FIND(FIRST itemfg
                WHERE itemfg.company EQ cocode
                  AND itemfg.i-no    EQ ip-focus:SCREEN-VALUE
                  AND itemfg.isaset
                  AND itemfg.alloc) THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " may not be an unassembled set header...".
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
    RUN fg/GetItemfgActInact.p (INPUT cocode,
                                INPUT fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name},
                                OUTPUT lActive).
    IF NOT lActive THEN DO:
    /*                                                                                                       */
/*     FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"                                         */
/*                           AND reftable.company  EQ cocode                                             */
/*                           AND reftable.loc      EQ ""                                                 */
/*                           AND reftable.code     EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} */
/*                           NO-LOCK NO-ERROR.                                                           */
/*         IF AVAIL reftable AND reftable.code2 = "I" THEN DO:                                           */
       MESSAGE fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} + " has InActive Status. Receipt cannot be created for the Inactive Item."
           VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO fg-rctd.i-no.
       RETURN ERROR.
    END.   

    IF NOT CAN-FIND(FIRST itemfg
                WHERE itemfg.company EQ cocode
                  AND itemfg.i-no    EQ ip-focus:SCREEN-VALUE
                  AND itemfg.prod-uom NE "") THEN DO:
    
       MESSAGE fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} + " has no cost UOM. Please correct the item master and try again."
           VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO fg-rctd.i-no.
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
  DEF INPUT PARAMETER iplAskPasswd AS LOG NO-UNDO.
  DEF VAR lvPasswordEntered AS LOG NO-UNDO.
  DEF VAR lcRitaCode AS CHAR NO-UNDO.
  IF AVAIL(fg-rctd) THEN
      lcRitaCode = fg-rctd.rita-code.
  ELSE
      lcRitaCode = "R".
  DO WITH FRAME {&frame-name}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE TRIM(lv-job-no)  OR
       DEC(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE DEC(lv-job-no2) THEN
      RUN new-job-no.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
      IF fgrecpt                                                AND
         fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" AND
         lcRitaCode NE "E"                                  AND
         iplAskPasswd THEN DO:

        /* Check password for override */
        RUN sys/ref/d-psswrd.w (INPUT "FGRecptPassWord", INPUT "FGRecptPassWord",
                                OUTPUT lvPasswordEntered).
        IF NOT lvPasswordEntered THEN DO:
            MESSAGE "You must enter a Job or a PO..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fg-rctd.job-no IN BROWSE {&browse-name}.
            RETURN ERROR.
        END.

      END. /* If nk1 is set to validate blank job/po */
    END. /* If job# blank */

    ELSE DO:
      IF INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:
        fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = "".
        MESSAGE "You may only enter a Job or a PO, Job No will be erased..."
            VIEW-AS ALERT-BOX ERROR.
      END.

      FIND FIRST job-hdr
          WHERE job-hdr.company EQ cocode
            AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job-hdr THEN DO:
        MESSAGE "Invalid Job#. Try Help..." VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
      END.
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
  DEF VAR lv-ans AS LOG NO-UNDO.
  DEF VAR lv-err AS LOG INIT NO NO-UNDO.

  DO WITH FRAME {&frame-name}:
      IF NOT AVAIL fg-rctd AND INTEGER(fg-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN DO:
          FIND FIRST fg-rctd
              WHERE fg-rctd.r-no EQ INTEGER(fg-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
              NO-LOCK NO-ERROR.
      END.  /*Mode 001*/

      fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}). /*Mode 001*/

    IF TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE TRIM(lv-job-no)  OR
       DEC(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE DEC(lv-job-no2) THEN
      RUN new-job-no.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      FOR EACH job-hdr
          WHERE job-hdr.company EQ fg-rctd.company
            AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE
            AND job-hdr.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE)
          NO-LOCK,
          FIRST job
          WHERE job.company EQ job-hdr.company
            AND job.job     EQ job-hdr.job
            AND job.job-no  EQ job-hdr.job-no
            AND job.job-no2 EQ job-hdr.job-no2
          NO-LOCK:
        LEAVE.
      END.
          
      IF NOT AVAIL job-hdr THEN
      FOR EACH job
          WHERE job.company EQ fg-rctd.company
            AND job.job-no  EQ fg-rctd.job-no:SCREEN-VALUE
            AND job.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE)
          NO-LOCK,
          FIRST job-hdr
          WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
          NO-LOCK:
        LEAVE.
      END.

      IF NOT AVAIL job-hdr THEN DO:
        MESSAGE "Invalid Job#. Try Help..." VIEW-AS ALERT-BOX ERROR.
        lv-err = YES.
      END.      

      IF NOT lv-err AND NOT lv-closed-checked AND
         job.opened EQ NO                     THEN DO:
        ASSIGN
         lv-ans            = NO
         lv-closed-checked = YES.

        /* gdm - 11160901 */
        IF jobreopn-log EQ YES 
          THEN
           MESSAGE 
              "Job is CLOSED, would you like to reopen?"
             VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL UPDATE lv-ans.
          ELSE 
           ASSIGN lv-ans = jobreopn-log.
        /* gdm - 11160901 end */
        
        CASE lv-ans:
           WHEN YES THEN RUN jc/jc-reopn.p (ROWID(job)).
           WHEN NO  THEN.
           OTHERWISE lv-err = YES.
         END CASE.
      END.
    END.

    IF lv-err THEN DO:
      lv-closed-checked = NO.
      APPLY "entry" TO fg-rctd.job-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-lot# B-table-Win 
PROCEDURE valid-lot# :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF ip-focus:SCREEN-VALUE NE "" THEN DO:
      IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = ""  THEN DO:
        MESSAGE TRIM(ip-focus:LABEL) + " may not be entered when tag# is blank".
        APPLY "entry" TO ip-focus.
        RETURN ERROR.
      END.
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
  DEF INPUT PARAM ip-type AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
      IF NOT AVAIL fg-rctd AND INTEGER(fg-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN DO:
          FIND FIRST fg-rctd
              WHERE fg-rctd.r-no EQ INTEGER(fg-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
              NO-LOCK NO-ERROR.
      END.  /*Mode 001*/

    IF INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0              AND
       NOT CAN-FIND(FIRST tt-fg-rctd WHERE tt-fg-rctd.tt-rowid EQ ROWID(fg-rctd)) THEN DO:
      IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
        MESSAGE "You may only enter a Job or a PO, PO will be erased..." VIEW-AS ALERT-BOX ERROR.
        fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = "".
        RETURN.
      END.

      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ fg-rctd.company
            AND po-ordl.po-no     EQ INT(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND po-ordl.item-type EQ NO
            AND (po-ordl.i-no     EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} OR
                 fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "")
          NO-LOCK NO-ERROR.
      IF NOT AVAIL po-ordl THEN DO:
        IF ip-type NE 0 THEN DO:
          MESSAGE "Invalid PO#, try help..." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.po-no IN BROWSE {&browse-name}.
        END.
        RETURN ERROR.
      END.

      ASSIGN fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.i-name.

      FIND FIRST po-ord
          WHERE po-ord.company EQ po-ordl.company
            AND po-ord.po-no   EQ po-ordl.po-no
          NO-LOCK NO-ERROR.
          
      IF AVAILABLE po-ord AND po-ord.stat = "H" AND POHoldRct-log THEN DO: /* ticket 17372 */
         MESSAGE "Unable to receive goods or materials for a purchase order that is on hold!"
         VIEW-AS ALERT-BOX ERROR. 
         RETURN ERROR.
      END.
      
      /* WFK - Task 09261318 - Don't pull qty from PO if there is a tag per Joe, */
      /* so not running create-from-po if there is                               */
      IF ip-type EQ 1                   AND
         AVAIL po-ord                   AND
         adm-adding-record              AND
          fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" AND
         NOT CAN-FIND(FIRST tt-fg-rctd) THEN DO:

        RUN fg/d-selpos.w (ROWID(po-ord), NO).

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

  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.

  DEF VAR lv-msg AS CHAR NO-UNDO.
  
  DEF VAR llValid AS LOG NO-UNDO.
  DEF VAR dMaxQty AS DEC NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF ip-focus:SCREEN-VALUE NE "" THEN DO:
      IF DECIMAL(ip-focus:SCREEN-VALUE) EQ 0 THEN
        lv-msg = "Quantity cannot be zero".
      dMaxQty = MaxComponentQty().
      IF ABS(DECIMAL(ip-focus:SCREEN-VALUE)) GT ABS(dMaxQty) AND dMaxQty GT 0 THEN 
        lv-msg = "Quantity cannot be greater than " + STRING(dMaxQty) + " for this component.".
    
      IF lv-msg NE "" THEN DO:
        MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ip-focus.
        RETURN ERROR.
      END.
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
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.
  DEF OUTPUT PARAM op-negative AS LOG NO-UNDO.

  DEF VAR lv-msg AS CHAR NO-UNDO.
  
  DEF VAR llValid AS LOG NO-UNDO.
  DEF VAR lcJobNo AS CHAR NO-UNDO.
  DEF VAR lcJobNo2 AS CHAR NO-UNDO.
  DEF VAR lcLoc AS CHAR NO-UNDO.
  DEF VAR lcLocBin AS CHAR NO-UNDO.
  DEF VAR lTagError AS LOG NO-UNDO.
  DEF VAR lTagErrorAccepted AS LOG NO-UNDO.
  DEF VAR lOk AS LOG NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF ip-focus:SCREEN-VALUE NE "" THEN DO:

      IF lv-msg EQ "" AND NOT ll-set-parts AND
         (CAN-FIND(FIRST b-fg-rctd
                   WHERE b-fg-rctd.company EQ cocode
                     AND b-fg-rctd.tag     EQ ip-focus:SCREEN-VALUE
                     AND b-fg-rctd.rita-code NE "P"
                     AND RECID(b-fg-rctd)  NE RECID(fg-rctd)) OR
          CAN-FIND(FIRST b-fg-rdtlh
                   WHERE b-fg-rdtlh.company   EQ cocode
                     AND b-fg-rdtlh.tag       EQ ip-focus:SCREEN-VALUE
                     AND b-fg-rdtlh.qty       GT 0
                     AND b-fg-rdtlh.rita-code NE "S"))          AND
         (INTE(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-NAME}) > 0 OR
            CAN-FIND(FIRST b2-fg-rdtlh
                     WHERE b2-fg-rdtlh.company   EQ cocode
                       AND b2-fg-rdtlh.tag       EQ ip-focus:SCREEN-VALUE
                       AND b2-fg-rdtlh.qty       LT ABS(INTE(fg-rctd.cases:SCREEN-VALUE))
                       AND b2-fg-rdtlh.rita-code NE "S") ) THEN
        ASSIGN lv-msg = "Tag# has already been used, please re-enter"
               lTagError = TRUE.

      IF lv-msg EQ "" AND NOT ll-set-parts AND v-copy-mode AND ip-focus:SCREEN-VALUE NE "" AND
         CAN-FIND(FIRST b-fg-rctd
                   WHERE b-fg-rctd.company EQ cocode
                     AND b-fg-rctd.tag     EQ ip-focus:SCREEN-VALUE
                     AND b-fg-rctd.rita-code NE "P"
                     AND b-fg-rctd.r-no    NE 0) THEN
         ASSIGN lv-msg = "Tag# has already been used, please re-enter"
                lTagError = TRUE.

      /* If this is a negative return, tag will be already used */
      /* This allows this procedure to be called from local-update */
      IF lTagError AND (INTE(fg-rctd.cases:SCREEN-VALUE)
                      * INTE(fg-rctd.qty-case:SCREEN-VALUE)) LT 0 THEN
        ASSIGN lTagError = FALSE
               lv-msg = "".

      IF lTagError THEN DO:  
        IF FOCUS:NAME EQ "btn-save" THEN DO:
          MESSAGE "Tag# has already been used, please re-enter" + "..." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.cases IN BROWSE {&browse-name}.
          RETURN ERROR.
        END.
        ELSE DO:        
          /* Ask if this will be a negative return */
          MESSAGE "Tag has already been used." SKIP
                  "If this is a negative return, click OK," SKIP
                  "Otherwise, click CANCEL to try a different tag."
            VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL UPDATE lOk.
          IF lOk THEN
            ASSIGN lTagErrorAccepted = TRUE
                   op-negative = TRUE.
          ELSE DO:
            APPLY "entry" TO ip-focus.
            RETURN ERROR.
          END.
          IF lTagErrorAccepted THEN
            lv-msg = "".
        END.
      END.

      /* check for assembled and unassembled set parts on-hand or pending receipt*/
      IF lv-msg EQ "" AND ll-set-parts THEN 
          RUN fg/ValidFGRcptTagSP.p (INPUT ROWID(fg-rctd),
                                     INPUT ip-focus:SCREEN-VALUE,
                                     INPUT INT(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}),
                                     INPUT cocode,
                                     INPUT NO,
                                     OUTPUT llValid,
                                     OUTPUT lv-msg,
                                     OUTPUT lcJobNo,
                                     OUTPUT lcJobNo2,
                                     OUTPUT lcLoc,
                                     OUTPUT lcLocBin
                                    ).
          IF llValid THEN
            ASSIGN 
                fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-NAME} = lcJobNo
                fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-NAME} = lcJobNo2
                fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-NAME} = lcLoc
                fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-NAME} = lcLocBin.
/*           AND int(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-NAME}) < 0 THEN DO:                */
/*             iTotalQty = 0.                                                                         */
/*         FOR EACH b-fg-rctd                                                                         */
/*             WHERE b-fg-rctd.company EQ cocode                                                      */
/*                  AND b-fg-rctd.tag     EQ ip-focus:SCREEN-VALUE                                    */
/*                  AND RECID(b-fg-rctd) NE RECID(fg-rctd) NO-LOCK:                                   */
/*             iTotalQty = iTotalQty + b-fg-rctd.t-qty.                                               */
/*         END.                                                                                       */
/*         FIND FIRST bf-fg-bin                                                                       */
/*             WHERE bf-fg-bin.company EQ cocode                                                      */
/*                 AND bf-fg-bin.tag   EQ ip-focus:SCREEN-VALUE NO-LOCK NO-ERROR.                     */
/*         IF AVAIL bf-fg-bin THEN                                                                    */
/*             ASSIGN                                                                                 */
/*                 iTotalQty = iTotalQty + bf-fg-bin.qty                                              */
/*                 fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-NAME} = bf-fg-bin.job-no            */
/*                 fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-NAME} = string(bf-fg-bin.job-no2). */
/*                                                                                                    */
/*         ELSE                                                                                       */
/*             lv-msg = "Invalid Tag#, try help or scan valid tag#".                                  */
/*         IF lv-msg EQ ""                                                                            */
/*             AND iTotalQty LT ABS(int(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-NAME})) THEN    */
/*             lv-msg = "Insufficient quantity in bin".                                               */
/*       END.                                                                                         */

      IF lv-msg EQ ""                                                   AND
         fgrecpt-int EQ 1                                               AND
         NOT CAN-FIND(FIRST loadtag
                      WHERE loadtag.company   EQ cocode
                        AND loadtag.item-type EQ NO
                        AND loadtag.tag-no    EQ ip-focus:SCREEN-VALUE) THEN
        lv-msg = "Invalid Tag#, try help or scan valid tag#".
      
      IF lv-msg NE "" THEN DO:
        MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
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

  DEF VAR lv-uom-list AS cha NO-UNDO.
  DEF VAR lv-uom AS CHAR NO-UNDO.
  DEF VAR lv-uom-help AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name})
     lv-uom = fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}.

    RUN sys/ref/uom-fg.p (NO, OUTPUT lv-uom-list).

    lv-uom-help = "Must enter one of the following as the UOM: " + lv-uom-list.

    IF INDEX(lv-uom-list,lv-uom) LE 0 THEN DO:
      MESSAGE TRIM(lv-uom-help) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fg-rctd.cost-uom IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-record B-table-Win 
PROCEDURE validate-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-max-qty AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

  IF dec(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
        /* In case user pressed save before tab out of tag field */
        fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(INT(fg-rctd.cases:SCREEN-VALUE) *
               INT(fg-rctd.qty-case:SCREEN-VALUE) +
               INT(fg-rctd.partial:SCREEN-VALUE)).
        IF dec(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
          MESSAGE "Receipt quantity cannot be 0."
              VIEW-AS ALERT-BOX.
          APPLY "entry" TO fg-rctd.cases.
          RETURN ERROR.
        END.
    END.
  FIND itemfg WHERE itemfg.company = cocode
                AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                NO-LOCK NO-ERROR.
  IF NOT AVAIL itemfg THEN DO:
     IF fg-rctd.i-no:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Invalid Item. Try help. " VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.i-no .
        RETURN ERROR.
     END.
     ELSE DO:
        MESSAGE  " F/G Item is not on file.  Would you like to add it? "
                 VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
        IF NOT ll-ans THEN DO:
            APPLY "entry" TO fg-rctd.i-no .
            RETURN ERROR.           
        END.
        ELSE DO:
            RUN fg/d-crtitm.w (fg-rctd.i-no:SCREEN-VALUE).
            FIND FIRST itemfg {sys/look/itemfgrlW.i}
                       AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                       NO-LOCK NO-ERROR.
            IF AVAIL itemfg THEN ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name.
        END.
     END.
  END.
      
  RUN valid-i-no (fg-rctd.i-no:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  IF itemfg.isaset                                                        AND
     (itemfg.alloc EQ NO                OR
      (itemfg.alloc EQ YES      AND
       fgrecpt-char NE "Manual" AND
       TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE "")) THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
         STRING((INT(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
                 INT(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})) +
                INT(fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name}),"->>>,>>>,>>9.99")
     li-max-qty = DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}).

    /* Only need to check components if creating adjustments for components */
    IF FgSetRec-Int NE 1 THEN DO:    
      RUN fg/checksetb.p (ROWID(itemfg),
                         ROWID(fg-rctd),
                         fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                         INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                         INPUT fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name},
                         INPUT-OUTPUT li-max-qty).
      
      IF li-max-qty LT DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
        ll = NO.

          RUN fg/cmpQtyMsg.w (ROWID(itemfg),
                             ROWID(fg-rctd),
                             fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                             INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}),
                             INPUT DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}),
                             INPUT fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name},
                             OUTPUT ll).
        
        IF ll THEN  
          ASSIGN
           fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(li-max-qty)
           fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}  = 
                STRING(TRUNC((li-max-qty - DEC(fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name})) /
                             DEC(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}),0))
           fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = 
                STRING(li-max-qty - (DEC(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
                                     DEC(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name}))).
  
        IF NOT ll OR li-max-qty EQ 0 THEN DO:
          APPLY "entry" TO fg-rctd.cases IN BROWSE {&browse-name}.
          RETURN ERROR.
        END.
      END.
    END.
    ELSE DO:
      IF li-max-qty EQ 0 THEN DO:
        APPLY "entry" TO fg-rctd.cases IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
  END.
  
  FIND FIRST loc WHERE loc.company = g_company
                        AND loc.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                        NO-LOCK NO-ERROR.
       IF NOT AVAIL loc THEN DO:
          MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.loc.
          RETURN ERROR.
  END.
  
  FIND FIRST fg-bin WHERE fg-bin.company = g_company 
                      AND fg-bin.i-no = ""
                      AND fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                      USE-INDEX co-ino NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-bin THEN DO:
          MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.loc-bin.
          RETURN ERROR.
  END.

  RUN get-matrix (NO) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN get-matrix-all (FALSE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  IF INT(fg-rctd.cases-unit:SCREEN-VALUE) < 1 THEN DO:  /* task# 06200520*/
    MESSAGE "Unit/Pallet must be greater than or equal to 1." VIEW-AS ALERT-BOX.
    APPLY "entry" TO fg-rctd.cases-unit .
    RETURN ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION maxComponentQty B-table-Win 
FUNCTION maxComponentQty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cHeaderItem AS CHAR        NO-UNDO.
  DEFINE VARIABLE dMaxQty     AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dMaxCompQty AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE hProc       AS HANDLE      NO-UNDO.
  DEFINE BUFFER   bfItemfg FOR itemfg.
  /* Obtain quantity of set header record */
  RUN get-link-handle IN adm-broker-hdl
                       (THIS-PROCEDURE,'container-source':U,OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
    hProc = WIDGET-HANDLE(char-hdl).
    RUN get-header-qty IN hProc (OUTPUT cHeaderItem, OUTPUT dMaxQty).
    
    IF cHeaderItem GT "" THEN
      FIND bfItemfg WHERE bfItemfg.company EQ cocode 
        AND bfItemfg.i-no = cHeaderItem NO-LOCK NO-ERROR.
  END.

  /* Obtain the Quantity for current component */
  IF AVAIL bfItemfg THEN
    RUN fg/fullset.p (INPUT ROWID(bfItemFg)).

  FIND FIRST tt-fg-set WHERE tt-fg-set.part-no EQ fg-rctd.i-no
    NO-LOCK NO-ERROR.
  
  IF AVAIL tt-fg-set THEN
    dMaxCompQty = dMaxQty * tt-fg-set.part-qty-dec.
  RETURN dMaxCompQty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

