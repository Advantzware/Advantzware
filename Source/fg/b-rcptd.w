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
ASSIGN 
    cocode = g_company
    locode = g_loc.

DEFINE VARIABLE poSelected         AS INTEGER NO-UNDO.
DEF    VAR      ll-help-run        AS LOG     NO-UNDO.  /* set on browse help, reset row-entry */
DEF    VAR      ls-prev-po         AS cha     NO-UNDO.
DEF    VAR      lv-overrun-checked AS LOG     NO-UNDO.
DEF    VAR      lv-closed-checked  AS LOG     NO-UNDO.
DEF    VAR      lv-job-no          AS CHAR    NO-UNDO.
DEF    VAR      lv-job-no2         AS CHAR    NO-UNDO.

DEF BUFFER b-fg-rctd    FOR fg-rctd.  /* for tag validation */
DEF BUFFER b-fg-rdtlh   FOR fg-rdtlh. /* for tag validation */
DEF BUFFER b2-fg-rdtlh  FOR fg-rdtlh. /* for tag validation */
DEF BUFFER b-tag-rctd   FOR fg-rctd .   /* task 11111306 */
DEF BUFFER reftable-job FOR reftable.
DEF BUFFER b-po-ord     FOR po-ord.
DEF BUFFER b-company    FOR company.

DEF VAR lv-prev-job2        AS cha   NO-UNDO.
DEF VAR lv-new-job-ran      AS LOG   NO-UNDO.
DEF VAR ll-qty-case-ent     AS LOG   NO-UNDO.
DEF VAR lv-num-rec          AS INT   NO-UNDO.
DEF VAR lv-frst-rno         LIKE fg-rctd.r-no NO-UNDO.
DEF VAR lv-rct-date-checked AS LOG   NO-UNDO.
DEF VAR ll-set-parts        AS LOG   NO-UNDO.
DEF VAR lv-linker           LIKE fg-rcpts.linker NO-UNDO.
DEF VAR trans-time          AS CHAR  NO-UNDO.
DEF VAR v-copy-mode         AS LOG   NO-UNDO.
DEF VAR lrMissingRow        AS ROWID NO-UNDO.
DEF VAR gvcCurrentItem      AS CHAR  NO-UNDO.
DEFINE VARIABLE hInventoryProcs AS HANDLE NO-UNDO.
DEFINE VARIABLE lColMove AS LOGICAL NO-UNDO INIT TRUE.
DEF VAR lv-cost-basis AS CHAR NO-UNDO.
DEF VAR v-auto-add-tag AS LOG NO-UNDO.
DEF VAR fg-uom-list  AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-fg-rctd LIKE fg-rctd
    FIELD tt-rowid AS ROWID
    FIELD po-rowid AS ROWID.

DEF BUFFER b-tt FOR tt-fg-rctd.
DEFINE TEMP-TABLE w-rowid 
    FIELD w-rowid AS CHAR
    INDEX w-rowid IS PRIMARY w-rowid.
{Inventory/ttInventory.i "NEW SHARED"}
{pc/pcprdd4u.i NEW}
{fg/invrecpt.i NEW}
{oe/d-selbin.i NEW}
{fg/fullset.i NEW}
{fg/d-selpos.i NEW}
{sys/inc/rfidtag.i}

&SCOPED-DEFINE item-key-phrase TRUE
&SCOPED-DEFINE init-proc init-proc

/*Globals for NK1 Settings*/
DEFINE VARIABLE glFGReceiptPassWord AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glFGSetAssembly     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glFGUnderOver       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcFGUnderOver       AS CHARACTER NO-UNDO.
DEFINE VARIABLE glFGPOFrt           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glJobReopn          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glFGPOTag#          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glPOHoldReceipts    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glRFIDTag           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glFGSecurity        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcFGSecurity        AS CHARACTER NO-UNDO.
DEFINE VARIABLE glOEShip            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcOEShip            AS CHARACTER NO-UNDO.
DEFINE VARIABLE giFGSetRec          AS INTEGER   NO-UNDO.
DEFINE VARIABLE glFGRecpt           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE giFGRecpt           AS INTEGER   NO-UNDO.
DEFINE VARIABLE gcFGRecpt           AS CHARACTER NO-UNDO.
DEFINE VARIABLE glAverageCost       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPostSec            AS LOGICAL   NO-UNDO .
DEFINE VARIABLE lAccessClose        AS LOGICAL   NO-UNDO .
DEFINE VARIABLE cAccessList         AS CHARACTER   NO-UNDO .
RUN pSetGlobalSettings(g_company).  /*Sets all of the above based on NK1 Settings*/


DEFINE VARIABLE hdCostProcs AS HANDLE.
RUN system\CostProcs.p PERSISTENT SET hdCostProcs.

RUN methods/prgsecur.p
	    (INPUT "p-fgrcpt.",
	     INPUT "UPDATE", /* based on run, create, update, delete or all */
	     INPUT NO,    /* use the directory in addition to the program */
	     INPUT NO,    /* Show a message if not authorized */
	     INPUT NO,    /* Group overrides user security? */
	     OUTPUT lPostSec, /* Allowed? Yes/NO */
	     OUTPUT lAccessClose, /* used in template/windows.i  */
	     OUTPUT cAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */
         
RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).         

/* ********************  Preprocessor Definitions  ******************** */



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
fg-rctd.po-line fg-rctd.job-no fg-rctd.job-no2 fg-rctd.i-no fg-rctd.i-name ~
fg-rctd.loc fg-rctd.loc-bin fg-rctd.cases fg-rctd.qty-case ~
fg-rctd.cases-unit fg-rctd.partial fg-rctd.std-cost fg-rctd.cost-uom ~
fg-rctd.t-qty fg-rctd.frt-cost fg-rctd.ext-cost fg-rctd.stack-code ~
fg-rctd.tot-wt fg-rctd.created-by fg-rctd.updated-by 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table fg-rctd.rct-date ~
fg-rctd.tag fg-rctd.po-no fg-rctd.job-no fg-rctd.job-no2 fg-rctd.i-no ~
fg-rctd.i-name fg-rctd.loc fg-rctd.loc-bin fg-rctd.cases fg-rctd.qty-case ~
fg-rctd.cases-unit fg-rctd.partial fg-rctd.std-cost fg-rctd.cost-uom ~
fg-rctd.frt-cost fg-rctd.stack-code fg-rctd.po-line
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table fg-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company eq cocode and ~
fg-rctd.r-no ge lv-frst-rno and ~
(fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") ~
AND (fg-rctd.SetHeaderRno GT 0 AND fg-rctd.SetHeaderRno EQ INTEGER(SUBSTRING(lv-linker, 10, 10)) OR (NOT ll-set-parts AND fg-rctd.SetHeaderRno EQ 0)) ~
use-index fg-rctd NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company eq cocode and ~
fg-rctd.r-no ge lv-frst-rno and ~
(fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") ~
AND (fg-rctd.SetHeaderRno GT 0 AND fg-rctd.SetHeaderRno EQ INTEGER(SUBSTRING(lv-linker, 10, 10)) OR (NOT ll-set-parts AND fg-rctd.SetHeaderRno EQ 0)) ~
use-index fg-rctd NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table fg-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table fg-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-1 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order fi_sortby auto_find
// fi_movecol

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

DEFINE VARIABLE auto_find    AS CHARACTER FORMAT "X(256)":U 
    LABEL "Auto Find" 
    VIEW-AS FILL-IN 
    SIZE 117.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby    AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 35 BY 1
    BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "N/A", 1
    SIZE 92 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 145 BY 2.62.

/*DEFINE VARIABLE fi_movecol AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 
     BGCOLOR 14 FONT 6 NO-UNDO.*/

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
    fg-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
    QUERY Browser-Table NO-LOCK DISPLAY
    fg-rctd.r-no            COLUMN-LABEL "Seq#"         FORMAT ">>>>>>>>":U     WIDTH 12    LABEL-BGCOLOR 14
    fg-rctd.rct-date        COLUMN-LABEL "Receipt!Date" FORMAT "99/99/9999":U   WIDTH 14    LABEL-BGCOLOR 14
    STRING(fg-rctd.trans-time,'HH:MM') @ trans-time COLUMN-LABEL "Receipt!Time" WIDTH 10
    fg-rctd.tag             COLUMN-LABEL "Tag#"         FORMAT "x(20)":U        WIDTH 29    LABEL-BGCOLOR 14
    fg-rctd.po-no           COLUMN-LABEL "PO No."       FORMAT "x(9)":U         WIDTH 14    LABEL-BGCOLOR 14
    fg-rctd.po-line         COLUMN-LABEL "PO Ln"        FORMAT ">>9":U          WIDTH 8
    fg-rctd.job-no          COLUMN-LABEL "Job#"         FORMAT "x(6)":U         WIDTH 8     LABEL-BGCOLOR 14
    fg-rctd.job-no2         COLUMN-LABEL ""             FORMAT "99":U           WIDTH 4
    fg-rctd.i-no            COLUMN-LABEL "Item"         FORMAT "X(15)":U        WIDTH 22    LABEL-BGCOLOR 14
    fg-rctd.i-name          COLUMN-LABEL "Name/Desc"    FORMAT "x(30)":U        WIDTH 45    LABEL-BGCOLOR 14
    fg-rctd.loc             COLUMN-LABEL "Whse"         FORMAT "x(5)":U         WIDTH 8     LABEL-BGCOLOR 14
    fg-rctd.loc-bin         COLUMN-LABEL "Bin"          FORMAT "x(8)":U         WIDTH 14    LABEL-BGCOLOR 14
    fg-rctd.cases           COLUMN-LABEL "Units"        FORMAT "->>>,>>9":U     WIDTH 14    LABEL-BGCOLOR 14
    fg-rctd.qty-case        COLUMN-LABEL "Unit!Count"   FORMAT ">>>,>>9":U      WIDTH 14    LABEL-BGCOLOR 14
    fg-rctd.cases-unit      COLUMN-LABEL "Units!/Skid"  FORMAT ">>>9":U         WIDTH 9     LABEL-BGCOLOR 14
    fg-rctd.partial         COLUMN-LABEL "Partial"      FORMAT "->>>,>>9":U     WIDTH 14    LABEL-BGCOLOR 14
    fg-rctd.std-cost        COLUMN-LABEL "Cost/UOM"     FORMAT ">,>>>,>>9.99<<<<":U WIDTH 18 LABEL-BGCOLOR 14
    fg-rctd.cost-uom        COLUMN-LABEL "UOM"          FORMAT "x(3)":U         WIDTH 5     LABEL-BGCOLOR 14
    fg-rctd.t-qty           COLUMN-LABEL "Total!Qty"    FORMAT "->>>,>>>,>>9.99":U WIDTH 16 LABEL-BGCOLOR 14
    fg-rctd.frt-cost        COLUMN-LABEL "Freight Cost" FORMAT ">>>,>>9.99<<":U WIDTH 18    LABEL-BGCOLOR 14
    fg-rctd.ext-cost        COLUMN-LABEL "Extended Cost" FORMAT "->,>>>,>>9.99":U WIDTH 18  LABEL-BGCOLOR 14
    fg-rctd.stack-code      COLUMN-LABEL "FG Lot#"      FORMAT "X(20)":U        WIDTH 22  LABEL-BGCOLOR 14
    fg-rctd.tot-wt          COLUMN-LABEL "Tot. Wgt."    FORMAT ">>,>>9.99":U    WIDTH 13
    fg-rctd.created-by      COLUMN-LABEL "Created By"   FORMAT "x(8)":U         WIDTH 14    LABEL-BGCOLOR 14
    fg-rctd.updated-by      COLUMN-LABEL "Last Updated By" FORMAT "x(8)":U      WIDTH 18    LABEL-BGCOLOR 14
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
   // fi_movecol AT ROW 15.76 COL 132 COLON-ALIGNED NO-LABEL 
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
IF NOT THIS-PROCEDURE:PERSISTENT THEN 
DO:
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
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
    FRAME F-Main:SCROLLABLE = FALSE
    FRAME F-Main:HIDDEN     = TRUE.

ASSIGN 
    Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
    fi_sortby:READ-ONLY IN FRAME F-Main = TRUE.

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
AND (fg-rctd.SetHeaderRno GT 0 AND fg-rctd.SetHeaderRno EQ INTEGER(SUBSTRING(lv-linker, 10, 10)) OR (NOT ll-set-parts AND fg-rctd.SetHeaderRno EQ 0))
use-index fg-rctd"
     _FldNameList[1]   > ASI.fg-rctd.r-no
"r-no" "Seq#" ">>>>>>>>" "integer" ? ? ? 14 ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.fg-rctd.rct-date
"rct-date" "Receipt!Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"STRING(fg-rctd.trans-time,'HH:MM') @ trans-time" "Receipt!Time" ? ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.fg-rctd.tag
"tag" "Tag#" "x(20)" "character" ? ? ? 14 ? ? yes ? no no "29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.fg-rctd.po-no
"po-no" ? ? "character" ? ? ? 14 ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.fg-rctd.po-line
"po-line" "PO Ln#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.fg-rctd.job-no
"job-no" "Job#" ? "character" ? ? ? 14 ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.fg-rctd.job-no2
"job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.fg-rctd.i-no
"i-no" "Item" "X(15)" "character" ? ? ? 14 ? ? yes "FG Item Number" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.fg-rctd.i-name
"i-name" "Name/Desc" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.fg-rctd.loc
"fg-rctd.loc" "Whse" ? "character" ? ? ? 14 ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.fg-rctd.loc-bin
"fg-rctd.loc-bin" "Bin" ? "character" ? ? ? 14 ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.fg-rctd.cases
"fg-rctd.cases" "Units" "->>>,>>9" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.fg-rctd.qty-case
"fg-rctd.qty-case" "Unit!Count" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.fg-rctd.cases-unit
"fg-rctd.cases-unit" "Units/!Skid" ">>>9" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.fg-rctd.partial
"fg-rctd.partial" "Partial" "->>>,>>9" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.fg-rctd.std-cost
"fg-rctd.std-cost" "Cost/UOM" ">,>>>,>>9.99<<<<" "decimal" ? ? ? 14 ? ? yes ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.fg-rctd.cost-uom
"fg-rctd.cost-uom" "UOM" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.fg-rctd.t-qty
"fg-rctd.t-qty" "Total!Qty" ? "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.fg-rctd.frt-cost
"fg-rctd.frt-cost" "Freight Cost" ? "decimal" ? ? ? 14 ? ? yes ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.fg-rctd.ext-cost
"fg-rctd.ext-cost" "Extended Cost" "->,>>>,>>9.99" "decimal" ? ? ? 14 ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.fg-rctd.stack-code
"fg-rctd.stack-code" "FG Lot#" "X(20)" "character" ? ? ? 14 ? ? yes ? no no "21.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   = ASI.fg-rctd.tot-wt
     _FldNameList[24]   > ASI.fg-rctd.created-by
"fg-rctd.created-by" "Created By" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > ASI.fg-rctd.updated-by
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
    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO.
   IF AVAILABLE fg-rctd THEN DO:
     IF lPostSec THEN do:
         RUN fg/d-rcptd.w (RECID(fg-rctd),"update",ll-set-parts,lv-linker, OUTPUT lv-rowid) . 
         RUN repo-query (lv-rowid).
     END.
     ELSE DO:
         RUN fg/d-rcptd.w (RECID(fg-rctd),"view",ll-set-parts,lv-linker, OUTPUT lv-rowid) . 
         RUN repo-query (lv-rowid).
     END.
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

RUN Inventory/InventoryProcs.p PERSISTENT SET hInventoryProcs.
 
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

IF glFGSecurity THEN
DO:
    FIND FIRST usergrps WHERE
        usergrps.usergrps = gcFGSecurity
        NO-LOCK NO-ERROR.

    IF AVAIL usergrps AND
        (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND
        TRIM(usergrps.users) NE "*") THEN
        ASSIGN
            fg-rctd.std-cost:VISIBLE IN BROWSE {&BROWSE-NAME} = NO
            fg-rctd.frt-cost:VISIBLE IN BROWSE {&BROWSE-NAME} = NO
            fg-rctd.ext-cost:VISIBLE IN BROWSE {&BROWSE-NAME} = NO.
END.
&SCOPED-DEFINE cellColumnDat b-rcptd.w 
 {methods/browsers/setCellColumns.i}
/* Ticket# : 92946
   Hiding this widget for now, as browser's column label should be indicating the column which is sorted by */
fi_sortby:HIDDEN  = TRUE.
fi_sortby:VISIBLE = FALSE.
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-tt B-table-Win 
PROCEDURE delete-tt :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    
    IF NOT AVAIL fg-rctd AND INTEGER(fg-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN 
    DO:
        FIND FIRST fg-rctd
            WHERE fg-rctd.r-no EQ INTEGER(fg-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
            NO-LOCK NO-ERROR.
    END.    /*Mode 001*/
    
    IF AVAIL fg-rctd THEN 
    DO:
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
        AND (bq-fg-rctd.SetHeaderRno GT 0 AND bq-fg-rctd.SetHeaderRno EQ INTEGER(SUBSTRING(lv-linker, 10, 10)) 
            OR (NOT ll-set-parts AND bq-fg-rctd.SetHeaderRno EQ 0))        USE-INDEX rita-code NO-LOCK
        BY bq-fg-rctd.r-no:
        lv-frst-rno = bq-fg-rctd.r-no.
        LEAVE.
    END.
    RELEASE bq-fg-rctd.

    FOR EACH bq-fg-rctd FIELDS(r-no)
        WHERE bq-fg-rctd.company   EQ cocode
        AND bq-fg-rctd.rita-code EQ "E"
        AND bq-fg-rctd.r-no      LT lv-frst-rno
        AND (bq-fg-rctd.SetHeaderRno GT 0 AND bq-fg-rctd.SetHeaderRno EQ INTEGER(SUBSTRING(lv-linker, 10, 10)) 
    OR (NOT ll-set-parts AND bq-fg-rctd.SetHeaderRno EQ 0))                    USE-INDEX rita-code NO-LOCK
        BY bq-fg-rctd.r-no:
        lv-frst-rno = bq-fg-rctd.r-no.
        LEAVE.
    END.
    RELEASE bq-fg-rctd.

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
        ELSE string(fg-rctd.r-no).

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

    DEFINE VARIABLE v-len                  LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid                  LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep                  LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-bwt                  LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-tot-msf              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-out-qty             AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-out-cost            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-over-cost           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-ext-cost            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-cost-uom            LIKE rm-rctd.cost-uom NO-UNDO.
    DEFINE VARIABLE lv-from-uom            LIKE rm-rctd.cost-uom NO-UNDO.
    DEFINE VARIABLE lv-out-ea              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-rec-qty              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-job-qty              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-ord-qty              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-ord-cost             AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-ord-uom              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-setup-qty            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-adjusted-ea         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-ord-po-uom           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-cost-per-ea          AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-cost-setup           AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-cost-with-setup      AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-corr                 AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE v-basis-w              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-out-qty              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-qty-per-msf          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-tot-out              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-tot-cost             AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-full-qty             AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lvlTotalCostCalculated AS LOG       NO-UNDO.
    DEFINE VARIABLE lvCalcCostUom          LIKE fg-rctd.cost-uom NO-UNDO.
    DEFINE VARIABLE lvCalcStdCost          LIKE fg-rctd.std-cost NO-UNDO.
    DEFINE VARIABLE lvCalcExtCost          LIKE fg-rctd.ext-cost NO-UNDO.
    DEFINE VARIABLE lvCalcFrtCost          LIKE fg-rctd.frt-cost NO-UNDO.
    DEFINE VARIABLE lvSetupPerCostUom      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-use-full-qty        AS LOG.                                       
    DEFINE VARIABLE lv-full-qty            AS DECIMAL   NO-UNDO.
    DEFINE BUFFER b-job-hdr FOR job-hdr.
    IF NOT AVAILABLE fg-rctd THEN RETURN.  /* no records */

    DO WITH FRAME {&FRAME-NAME}:
        IF AVAILABLE fg-rctd AND fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN 
        DO: /* in update mode - use screen-value */
            IF INTEGER(fg-rctd.po-no:SCREEN-VALUE) NE 0 THEN 
            DO:  
                // This was incorrectly copied from the update dialog
                // RUN pDisplayPO(ip-first-disp).
 
            END.
           
            ELSE IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN 
                DO:
                    FIND FIRST job-hdr WHERE job-hdr.company = cocode                       
                        AND job-hdr.i-no  = fg-rctd.i-no:screen-value
                        AND job-hdr.job-no = (fg-rctd.job-no:screen-value)
                        AND job-hdr.job-no2 = integer(fg-rctd.job-no2:screen-value)
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE job-hdr THEN 
                    DO: 
                        FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
                            sys-ctrl.name = "JOB QTY" 
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN v-rec-qty = job-hdr.qty                          .
                        ELSE 
                        DO:
                            FIND FIRST oe-ordl NO-LOCK
                                WHERE oe-ordl.company EQ job-hdr.company
                                AND oe-ordl.ord-no  EQ job-hdr.ord-no
                                AND oe-ordl.i-no    EQ job-hdr.i-no
                                NO-ERROR.
                            FIND FIRST oe-ord NO-LOCK
                                WHERE oe-ord.company EQ job-hdr.company
                                AND oe-ord.ord-no  EQ job-hdr.ord-no
                                NO-ERROR.
              
                            v-rec-qty = (job-hdr.qty * (1 + (IF AVAILABLE oe-ordl THEN oe-ordl.over-pct ELSE
                                IF AVAILABLE oe-ord  THEN oe-ord.over-pct  ELSE 0 / 100))).
      
                        END.
                        IF v-rec-qty <  int(fg-rctd.t-qty:SCREEN-VALUE) AND NOT lv-overrun-checked THEN 
                        DO:
                            MESSAGE "Receipt quantity exceeds job quantity." VIEW-AS ALERT-BOX WARNING.
                            lv-overrun-checked = YES.
                        END.
          
                    END.
                END.
        END. 
    END. /*Do with Frame*/

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

    RUN setCellColumns.
 /* FI_moveCol = "Sort".
  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.*/
   
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
    IF NOT adm-new-record THEN 
    DO:
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
    DEF VAR out-hd-lst AS cha           NO-UNDO.
    DEF VAR ii         AS INT           NO-UNDO.
    DEF VAR hd-next    AS WIDGET-HANDLE NO-UNDO.

    lv-rct-date-checked = NO.

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
        IF AVAIL fg-rctd THEN 
        DO:
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit B-table-Win
PROCEDURE local-exit:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DELETE OBJECT hInventoryProcs.


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
    IF ll-set-parts THEN 
    DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetGlobalSettings B-table-Win 
PROCEDURE pSetGlobalSettings PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose: Sets all NK1 Global Variables for the program
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

    RUN sys/ref/nk1look.p (ipcCompany, "FGRecptPassWord", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound). 
    glFGReceiptPassWord = lFound AND cReturn EQ "YES".

    RUN sys/ref/nk1look.p (ipcCompany, "FGUnderOver", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    glFGUnderOver = lFound AND cReturn EQ "YES".

    RUN sys/ref/nk1look.p (ipcCompany, "FGUnderOver", "L", NO, NO, "", "", OUTPUT gcFGUnderOver, OUTPUT lFound).

    RUN sys/ref/nk1look.p (ipcCompany, "FGPOFRT", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    glFGPOFrt = lFound AND cReturn EQ "YES".

    RUN sys/ref/nk1look.p (ipcCompany, "JOBREOPN", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    glJobReopn = lFound AND cReturn EQ "YES".

    RUN sys/ref/nk1look.p (ipcCompany, "FGPOTAG#", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    glFGPOTag# = lFound AND cReturn EQ "YES".

    RUN sys/ref/nk1look.p (ipcCompany, "POHoldReceipts", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    glPOHoldReceipts = lFound AND cReturn EQ "YES".

    RUN sys/ref/nk1look.p (ipcCompany, "RFIDTag", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    glRFIDTag = lFound AND cReturn EQ "YES".

    RUN sys/ref/nk1look.p (ipcCompany, "FGSECURE", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    glFGSecurity = lFound AND cReturn EQ "YES".

    RUN sys/ref/nk1look.p (ipcCompany, "FGSECURE", "C", NO, NO, "", "", OUTPUT gcFGSecurity, OUTPUT lFound).

    RUN sys/ref/nk1look.p (ipcCompany, "OESHIP", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    glOEShip = lFound AND cReturn EQ "YES".

    RUN sys/ref/nk1look.p (ipcCompany, "FGSETREC", "I", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN giFGSetRec = INTEGER(cReturn).

    RUN sys/ref/nk1look.p (ipcCompany, "FGRECPT", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    glFGRecpt = lFound AND cReturn EQ "YES".

    RUN sys/ref/nk1look.p (ipcCompany, "FGRECPT", "I", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN giFGRecpt = INTEGER(cReturn).

    RUN sys/ref/nk1look.p (ipcCompany, "FGRECPT", "C", NO, NO, "", "", OUTPUT gcFGRecpt, OUTPUT lFound).

    FIND FIRST fg-ctrl NO-LOCK 
        WHERE fg-ctrl.company EQ ipcCompany
        NO-ERROR.
    glAverageCost = AVAILABLE fg-ctrl AND fg-ctrl.inv-meth EQ "A".
    
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
         FOR EACH ASI.reftable WHERE reftable.reftable EQ "fg-rctd.user-id" 
           AND reftable.company  EQ fg-rctd.company 
           AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") 
           AND (reftable.dscr    EQ lv-linker AND reftable.dscr BEGINS "fg-rctd: ")
           EXCLUSIVE-LOCK:
           DELETE reftable.
         END.
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
          ,
         FIRST reftable NO-LOCK
         WHERE reftable.reftable EQ "fg-rctd.user-id"
           AND reftable.company  EQ b-fg-rctd.company
           AND reftable.loc      EQ STRING(b-fg-rctd.r-no,"9999999999")
           AND NOT reftable.dscr BEGINS "fg-rctd: " /*not a set part receipt*/
 /*           AND ((reftable.dscr   EQ lv-linker AND reftable.dscr BEGINS "fg-rctd: ") OR */
 /*                (NOT ll-set-parts AND NOT reftable.dscr BEGINS "fg-rctd: "))           */
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
      FIND FIRST reftable NO-LOCK
         WHERE reftable.reftable EQ "fg-rctd.user-id"
           AND reftable.company  EQ bfFgRctd.company
           AND reftable.loc      EQ STRING(bfFgRctd.r-no,"9999999999")
           AND reftable.dscr EQ lv-linker
           AND reftable.dscr BEGINS "fg-rctd: " 
          NO-ERROR.
      IF NOT AVAIL reftable THEN DO:
        CREATE reftable.
        ASSIGN
         reftable.reftable = "fg-rctd.user-id"
         reftable.company  = bfFgRctd.company
         reftable.loc      = STRING(bfFgRctd.r-no,"9999999999")
         reftable.dscr = lv-linker.         
      END.

      /* as in local-assign logic */
      IF ll-set-parts THEN DO:
        FIND FIRST fg-rcpts WHERE fg-rcpts.r-no EQ bfFgRctd.r-no NO-ERROR.
        IF NOT AVAIL fg-rcpts THEN DO:
          CREATE fg-rcpts.
          fg-rcpts.r-no       = bfFgRctd.r-no.
        END.
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
      IF glFGPOTag# AND bfFgRctd.tag EQ "" THEN DO:
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
           NO-LOCK,     
    FIRST ASI.reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND
      reftable.company  EQ fg-rctd.company AND
      reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999")  AND
      (reftable.dscr    EQ lv-linker AND reftable.dscr BEGINS "fg-rctd: ")  
    USE-INDEX loc:

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

    IF NOT ll-set-parts THEN 
    DO:
        RUN get-link-handle IN adm-broker-hdl
            (THIS-PROCEDURE,'linker-target':U,OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(1,char-hdl))) THEN
            RUN dispatch IN WIDGET-HANDLE(ENTRY(1,char-hdl)) ("open-query").
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-tt B-table-Win 
PROCEDURE update-tt :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DO WITH FRAME {&frame-name}:
        FIND FIRST tt-fg-rctd NO-ERROR.
        IF AVAIL tt-fg-rctd THEN 
        DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns B-table-Win 
PROCEDURE move-columns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
      {&BROWSE-NAME}:COLUMN-MOVABLE = lColMove
         {&BROWSE-NAME}:COLUMN-RESIZABLE = lColMove
        lColMove = NOT lColMove.
    /*    FI_moveCol = IF lColMove = NO THEN "Move" ELSE "Sort".
     DISPLAY FI_moveCol.*/
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddRecord B-table-Win 
PROCEDURE pAddRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
  DEFINE BUFFER bff-fg-rctd FOR fg-rctd .
    
   RUN fg/d-rcptd.w (?,"Add",ll-set-parts,lv-linker, OUTPUT lv-rowid) . 
   FIND FIRST bff-fg-rctd NO-LOCK
       WHERE bff-fg-rctd.company EQ cocode
       AND ROWID(bff-fg-rctd) EQ lv-rowid NO-ERROR .
   IF AVAIL bff-fg-rctd THEN
       RUN repo-query (lv-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRecord B-table-Win 
PROCEDURE pUpdateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
    
    IF AVAILABLE fg-rctd THEN
    DO:
       RUN fg/d-rcptd.w (RECID(fg-rctd),"update",ll-set-parts,lv-linker, OUTPUT lv-rowid) . 
       RUN repo-query (lv-rowid).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pViewRecord B-table-Win 
PROCEDURE pViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
    
    IF AVAILABLE fg-rctd THEN
    DO:
       RUN fg/d-rcptd.w (RECID(fg-rctd),"view",ll-set-parts,lv-linker, OUTPUT lv-rowid) . 
       RUN repo-query (lv-rowid).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCopyRecord B-table-Win 
PROCEDURE pCopyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
   DEF BUFFER b-fg-rctd FOR fg-rctd.
   DEFINE BUFFER bff-fg-rctd FOR fg-rctd.
   DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
    
    IF AVAILABLE fg-rctd THEN
    DO:
        lv-rno = 0.
       FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
       IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

       FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
       IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

       CREATE bff-fg-rctd.

       DO WHILE TRUE:
            lv-rno = lv-rno + 1.
            FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
            IF AVAIL fg-rcpth THEN NEXT.
            FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
            IF AVAIL b-fg-rctd THEN NEXT.
            LEAVE.
        END.

       BUFFER-COPY fg-rctd EXCEPT r-no rec_key TO bff-fg-rctd .
       ASSIGN bff-fg-rctd.r-no = lv-rno.

       RUN fg/d-rcptd.w (RECID(bff-fg-rctd),"copy",ll-set-parts,lv-linker, OUTPUT lv-rowid) . 
       IF lv-rowid NE ? THEN
           RUN repo-query (lv-rowid).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetRecord B-table-Win 
PROCEDURE pGetRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER oplRecord AS LOGICAL NO-UNDO . 
    
    IF AVAILABLE fg-rctd THEN
        oplRecord = TRUE .

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
    DEFINE OUTPUT PARAMETER opcItem AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQty AS DECIMAL NO-UNDO.
    IF AVAILABLE fg-rctd THEN
        ASSIGN opdQty  = fg-rctd.t-qty
            opcItem = fg-rctd.i-no.

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
    DEFINE VARIABLE cHeaderItem AS CHAR    NO-UNDO.
    DEFINE VARIABLE dMaxQty     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMaxCompQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE hProc       AS HANDLE  NO-UNDO.
    DEFINE BUFFER bfItemfg FOR itemfg.
    /* Obtain quantity of set header record */
    RUN get-link-handle IN adm-broker-hdl
        (THIS-PROCEDURE,'container-source':U,OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
    DO:
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

