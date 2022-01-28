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

  File: poinq/b-po-inq.w

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
&SCOPED-DEFINE exclude-row-display

{methods/template/brwcustomdef.i}
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF NEW SHARED VAR factor# AS DECIMAL NO-UNDO.
DEF NEW SHARED VAR v-default-gl-log AS LOG NO-UNDO.
DEF NEW SHARED VAR v-default-gl-cha AS cha NO-UNDO.
DEF NEW SHARED VAR v-po-qty AS LOG INITIAL TRUE NO-UNDO.
DEF NEW SHARED VAR v-po-msf LIKE sys-ctrl.int-fld NO-UNDO.

DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR lv-frst-rowid2 AS ROWID NO-UNDO.
DEF VAR lv-last-rowid2 AS ROWID NO-UNDO.
DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR ll-new-record AS LOG INIT NO NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "po-no" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "PO#" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR lv-sort-list1 AS CHAR NO-UNDO
  INIT "s-wid,s-len,cons-qty,cons-uom,t-rec-qty,cost,pr-uom,cons-uom,ord-qty".
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR phandle AS HANDLE NO-UNDO.
DEF VAR lv-t-rec-qty LIKE po-ordl.t-rec-qty NO-UNDO.
DEF VAR lv-show-prev AS LOG NO-UNDO.
DEF VAR lv-show-next AS LOG NO-UNDO.
DEF VAR lv-last-show-po-no AS INT NO-UNDO.
DEF VAR lv-first-show-po-no AS INT NO-UNDO.
DEF VAR ll-show-all AS LOG NO-UNDO.
DEF VAR v-col-move AS LOG INIT TRUE NO-UNDO.
DEF VAR v-rec-key-list AS CHAR NO-UNDO.

DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
DEF VAR period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE lInquery AS LOGICAL INIT NO NO-UNDO .
DEFINE VARIABLE lButtongoPressed AS LOGICAL   NO-UNDO.

    IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
       INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
       INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("NOSWEAT") + "..".
    ELSE
    ASSIGN
      period_pos = INDEX(PROGRAM-NAME(1),".")
      v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
      v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

/* gdm - 01310801 */
DEF VAR v-paidflg AS LOG NO-UNDO.
DEFINE VARIABLE cPoStatus     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPoLineStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIsMatches    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lEnableShowAll     AS LOGICAL   NO-UNDO.

DEFINE VARIABLE iRecordLimit       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cBrowseWhereClause AS CHARACTER NO-UNDO.
DEFINE VARIABLE dQueryTimeLimit    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cQueryBuffers      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldBuffer       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIsBreakByUsed     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFormattedJobno    AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdJobProcs         AS HANDLE    NO-UNDO.
RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.

DEFINE VARIABLE iInitialQueryLimit AS INTEGER   NO-UNDO.
DEFINE VARIABLE cReturn            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound             AS LOGICAL   NO-UNDO.

RUN sys/ref/nk1look.p(
    INPUT  cocode,
    INPUT  "POBrowse", 
    INPUT  "I",      /* Logical */
    INPUT  NO,      /* check by cust */
    INPUT  NO,      /* use cust not vendor */
    INPUT  "",      /* cust */
    INPUT  "",      /* ship-to*/
    OUTPUT cReturn,
    OUTPUT lFound
    ).
IF lFound THEN
    iInitialQueryLimit = INTEGER(cReturn).        
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
&Scoped-define INTERNAL-TABLES po-ordl po-ord

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table po-ordl.po-no po-ord.vend-no ~
po-ordl.due-date po-ord.ship-id po-ord.ship-name po-ordl.job-no ~
po-ordl.job-no2 po-ordl.s-num po-ordl.i-no po-ordl.i-name ~
dim-in-16 (po-ordl.s-wid) @ po-ordl.s-wid po-ordl.s-wid ~
dim-in-16 (po-ordl.s-len) @ po-ordl.s-len po-ordl.s-len po-ordl.vend-i-no ~
po-ordl.ord-qty qty-in-ord-uom () @ lv-t-rec-qty po-ordl.pr-qty-uom ~
po-ordl.t-rec-qty po-ordl.cons-uom po-ordl.cost po-ordl.pr-uom po-ord.buyer ~
is-it-polinestat() @ cPoLineStatus is-it-postat() @ cPoStatus ~
is-it-paid() @ v-paidflg po-ordl.cust-no po-ordl.line 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table po-ordl.po-no ~
po-ord.vend-no po-ordl.due-date po-ord.ship-id po-ord.ship-name ~
po-ordl.job-no po-ordl.job-no2 po-ordl.s-num po-ordl.i-no po-ordl.i-name ~
po-ordl.s-wid po-ordl.s-len po-ordl.vend-i-no po-ordl.ord-qty ~
po-ordl.pr-qty-uom po-ordl.t-rec-qty po-ordl.cons-uom po-ordl.cost ~
po-ordl.pr-uom po-ord.buyer po-ordl.cust-no 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table po-ordl po-ord
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table po-ordl
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table po-ord
&Scoped-define QUERY-STRING-Browser-Table FOR EACH po-ordl WHERE ~{&KEY-PHRASE} ~
      AND po-ordl.company EQ g_company AND ~
po-ordl.po-no EQ 9999999 NO-LOCK, ~
      FIRST po-ord WHERE po-ord.company EQ po-ordl.company AND ~
po-ord.po-no EQ po-ordl.po-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH po-ordl WHERE ~{&KEY-PHRASE} ~
      AND po-ordl.company EQ g_company AND ~
po-ordl.po-no EQ 9999999 NO-LOCK, ~
      FIRST po-ord WHERE po-ord.company EQ po-ordl.company AND ~
po-ord.po-no EQ po-ordl.po-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table po-ordl po-ord
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table po-ordl
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table po-ord


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table tb_approved fi_po-no ~
fi_vend-no fi_i-no fi_vend-i-no fi_due-date fi_job-no fi_job-no2 btn_go ~
btn_show btn_prev tb_open tb_closed tb_hold RECT-1 
&Scoped-Define DISPLAYED-OBJECTS tb_approved fi_po-no fi_vend-no fi_i-no ~
fi_vend-i-no fi_due-date fi_job-no fi_job-no2 tb_unpaid tb_paid fi_sort-by ~
tb_open tb_closed tb_hold 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dim-in-16 B-table-Win 
FUNCTION dim-in-16 RETURNS DECIMAL
  ( ip-dim AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetSortCondition B-table-Win 
FUNCTION fGetSortCondition RETURNS CHARACTER
  (ipcSortBy AS CHARACTER, ipcSortLabel AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getcurrentpo B-table-Win 
FUNCTION getcurrentpo RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetPOLine B-table-Win 
FUNCTION GetPOLine RETURNS INTEGER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-it-paid B-table-Win 
FUNCTION is-it-paid RETURNS LOGICAL
  (  /* parameter-definitions */  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-it-polinestat B-table-Win 
FUNCTION is-it-polinestat RETURNS CHARACTER
  (  /* parameter-definitions */  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-it-postat B-table-Win 
FUNCTION is-it-postat RETURNS CHARACTER
  (  /* parameter-definitions */  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isPaidCheck B-table-Win 
FUNCTION isPaidCheck RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pGetWhereCriteria B-table-Win 
FUNCTION pGetWhereCriteria RETURNS CHARACTER
  ( ipcTable AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD qty-in-ord-uom B-table-Win 
FUNCTION qty-in-ord-uom RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_next 
     LABEL "Show &Next" 
     SIZE 15 BY 1
     FONT 6.

DEFINE BUTTON btn_prev 
     LABEL "Show &Previous" 
     SIZE 20 BY 1
     FONT 6.

DEFINE BUTTON btn_show 
     LABEL "&Show All" 
     SIZE 12 BY 1.

DEFINE VARIABLE fi_due-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 28.6 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE fi_vend-i-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_vend-no AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 3.57.

DEFINE VARIABLE tb_approved AS LOGICAL INITIAL yes 
     LABEL "Not Hold" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.6 BY .81 NO-UNDO.

DEFINE VARIABLE tb_closed AS LOGICAL INITIAL no 
     LABEL "Closed" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.8 BY 1
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE tb_hold AS LOGICAL INITIAL yes 
     LABEL "Hold" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE tb_open AS LOGICAL INITIAL yes 
     LABEL "Open" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.6 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE tb_paid AS LOGICAL INITIAL yes 
     LABEL "Paid" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE tb_unpaid AS LOGICAL INITIAL yes 
     LABEL "UnPaid" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY 1
     FGCOLOR 9  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      po-ordl
    FIELDS(po-ordl.po-no
      po-ordl.due-date
      po-ordl.job-no
      po-ordl.job-no2
      po-ordl.s-num
      po-ordl.i-no
      po-ordl.i-name
      po-ordl.s-wid
      po-ordl.s-wid
      po-ordl.s-wid
      po-ordl.s-len
      po-ordl.s-len
      po-ordl.s-len
      po-ordl.vend-i-no
      po-ordl.ord-qty
      po-ordl.pr-qty-uom
      po-ordl.t-rec-qty
      po-ordl.cons-uom
      po-ordl.cost
      po-ordl.pr-uom
      po-ordl.cust-no
      po-ordl.line), 
      po-ord SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      po-ordl.po-no COLUMN-LABEL "PO#" FORMAT ">>>>>>>>":U LABEL-BGCOLOR 14
      po-ord.vend-no COLUMN-LABEL "Vendor#" FORMAT "x(8)":U LABEL-BGCOLOR 14
      po-ordl.due-date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      po-ord.ship-id FORMAT "x(8)":U LABEL-BGCOLOR 14
      po-ord.ship-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      po-ordl.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U LABEL-BGCOLOR 14
      po-ordl.job-no2 COLUMN-LABEL "" FORMAT "99":U LABEL-BGCOLOR 14
      po-ordl.s-num COLUMN-LABEL "Form#" FORMAT ">>>":U LABEL-BGCOLOR 14
      po-ordl.i-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      po-ordl.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      dim-in-16 (po-ordl.s-wid) @ po-ordl.s-wid
      po-ordl.s-wid COLUMN-LABEL "Width" FORMAT ">>,>>9.99<<<":U
            LABEL-BGCOLOR 14
      dim-in-16 (po-ordl.s-len) @ po-ordl.s-len
      po-ordl.s-len COLUMN-LABEL "Length" FORMAT ">>,>>9.99<<<":U
            LABEL-BGCOLOR 14
      po-ordl.vend-i-no COLUMN-LABEL "Vendor Item#" FORMAT "x(15)":U
            LABEL-BGCOLOR 14
      po-ordl.ord-qty COLUMN-LABEL "Qty Ordered" FORMAT "->>>,>>>,>>9.9<<<<<":U
            WIDTH 22.8 LABEL-BGCOLOR 14
      qty-in-ord-uom () @ lv-t-rec-qty COLUMN-LABEL "PO Qty Received" FORMAT "->>>,>>>,>>9.9<<":U
            WIDTH 21.2
      po-ordl.pr-qty-uom COLUMN-LABEL "Ord UOM" FORMAT "x(4)":U
            WIDTH 10 LABEL-BGCOLOR 14
      po-ordl.t-rec-qty COLUMN-LABEL "Qty Received" FORMAT "->>>,>>>,>>9.9<<<<<":U
            WIDTH 23.8 LABEL-BGCOLOR 14
      po-ordl.cons-uom COLUMN-LABEL "Rec. UOM" FORMAT "x(4)":U
            LABEL-BGCOLOR 14
      po-ordl.cost FORMAT "->,>>>,>>9.99<<<<":U LABEL-BGCOLOR 14
      po-ordl.pr-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U LABEL-BGCOLOR 14
      po-ord.buyer FORMAT "x(10)":U LABEL-BGCOLOR 14
      is-it-polinestat() @ cPoLineStatus COLUMN-LABEL "Line Status"
            LABEL-BGCOLOR 14
      is-it-postat() @ cPoStatus COLUMN-LABEL "PO Status" LABEL-BGCOLOR 14
      is-it-paid() @ v-paidflg COLUMN-LABEL "Paid" FORMAT "YES / NO":U
      po-ordl.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
            LABEL-BGCOLOR 14
      po-ordl.line COLUMN-LABEL "Line #" FORMAT ">>>>>>>":U LABEL-BGCOLOR 14
  ENABLE
      po-ordl.po-no
      po-ord.vend-no
      po-ordl.due-date
      po-ord.ship-id
      po-ord.ship-name
      po-ordl.job-no
      po-ordl.job-no2
      po-ordl.s-num
      po-ordl.i-no
      po-ordl.i-name
      po-ordl.s-wid
      po-ordl.s-len
      po-ordl.vend-i-no
      po-ordl.ord-qty
      po-ordl.pr-qty-uom
      po-ordl.t-rec-qty
      po-ordl.cons-uom
      po-ordl.cost
      po-ordl.pr-uom
      po-ord.buyer
      po-ordl.cust-no
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 16.43
         FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 4.57 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     tb_approved AT ROW 1.33 COL 134.4 WIDGET-ID 12
     fi_po-no AT ROW 2.19 COL 2.6 NO-LABEL
     fi_vend-no AT ROW 2.19 COL 13 COLON-ALIGNED NO-LABEL
     fi_i-no AT ROW 2.19 COL 28.4 COLON-ALIGNED NO-LABEL
     fi_vend-i-no AT ROW 2.19 COL 52.8 COLON-ALIGNED NO-LABEL
     fi_due-date AT ROW 2.19 COL 77.2 COLON-ALIGNED NO-LABEL
     fi_job-no AT ROW 2.19 COL 93 COLON-ALIGNED NO-LABEL
     fi_job-no2 AT ROW 2.19 COL 103.4 COLON-ALIGNED NO-LABEL
     tb_unpaid AT ROW 2.14 COL 110.2
     tb_paid AT ROW 1.24 COL 110.2
     btn_go AT ROW 3.38 COL 2.2
     btn_show AT ROW 3.38 COL 49
     fi_sort-by AT ROW 3.38 COL 76.4 COLON-ALIGNED
     btn_prev AT ROW 3.38 COL 14
     btn_next AT ROW 3.38 COL 34
     tb_open AT ROW 1.24 COL 122.6 WIDGET-ID 10
     tb_closed AT ROW 2.14 COL 122.6 WIDGET-ID 8
     tb_hold AT ROW 2.19 COL 134.6 WIDGET-ID 14
     "Vendor#" VIEW-AS TEXT
          SIZE 10 BY .95 AT ROW 1.24 COL 17
          FGCOLOR 9 
     "PO#" VIEW-AS TEXT
          SIZE 6 BY .95 AT ROW 1.24 COL 6.4
          FGCOLOR 9 
     "RM/FG Item#" VIEW-AS TEXT
          SIZE 16 BY .95 AT ROW 1.24 COL 33.4
          FGCOLOR 9 
     "Start Due Date" VIEW-AS TEXT
          SIZE 18 BY .95 AT ROW 1.24 COL 78.6
          FGCOLOR 9 
     "Vendor Item#" VIEW-AS TEXT
          SIZE 17 BY .95 AT ROW 1.24 COL 56.8
          FGCOLOR 9 
     "Job#" VIEW-AS TEXT
          SIZE 7 BY .86 AT ROW 1.24 COL 99.6
          FGCOLOR 9 
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 6
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
         HEIGHT             = 20.1
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
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2
       Browser-Table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

ASSIGN 
       po-ordl.s-len:AUTO-RESIZE IN BROWSE Browser-Table = TRUE.

/* SETTINGS FOR BUTTON btn_next IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_po-no IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_paid IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_unpaid IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.po-ordl,ASI.po-ord WHERE ASI.po-ordl ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED, FIRST, FIRST OUTER, OUTER, OUTER,"
     _Where[1]         = "po-ordl.company EQ g_company AND
po-ordl.po-no EQ 9999999"
     _JoinCode[2]      = "po-ord.company EQ po-ordl.company AND
po-ord.po-no EQ po-ordl.po-no"
     _FldNameList[1]   > ASI.po-ordl.po-no
"po-ordl.po-no" "PO#" ">>>>>>>" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.po-ord.vend-no
"po-ord.vend-no" "Vendor#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.po-ordl.due-date
"po-ordl.due-date" ? ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.po-ord.ship-id
"po-ord.ship-id" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.po-ord.ship-name
"po-ord.ship-name" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.po-ordl.job-no
"po-ordl.job-no" "Job#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.po-ordl.job-no2
"po-ordl.job-no2" "" "99" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.po-ordl.s-num
"po-ordl.s-num" "Form#" ">>>" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.po-ordl.i-no
"po-ordl.i-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.po-ordl.i-name
"po-ordl.i-name" "Item Name" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"dim-in-16 (po-ordl.s-wid) @ po-ordl.s-wid" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.po-ordl.s-wid
"po-ordl.s-wid" "Width" ">>,>>9.99<<<" "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"dim-in-16 (po-ordl.s-len) @ po-ordl.s-len" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.po-ordl.s-len
"po-ordl.s-len" "Length" ">>,>>9.99<<<" "decimal" ? ? ? 14 ? ? yes ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.po-ordl.vend-i-no
"po-ordl.vend-i-no" "Vendor Item#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.po-ordl.ord-qty
"po-ordl.ord-qty" "Qty Ordered" ? "decimal" ? ? ? 14 ? ? yes ? no no "22.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"qty-in-ord-uom () @ lv-t-rec-qty" "PO Qty Received" "->>>,>>>,>>9.9<<" ? ? ? ? ? ? ? no ? no no "21.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.po-ordl.pr-qty-uom
"po-ordl.pr-qty-uom" "Ord UOM" ? "character" ? ? ? 14 ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.po-ordl.t-rec-qty
"po-ordl.t-rec-qty" "Qty Received" ? "decimal" ? ? ? 14 ? ? yes ? no no "23.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.po-ordl.cons-uom
"po-ordl.cons-uom" "Rec. UOM" ? "character" ? ? ? 14 ? ? yes "" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.po-ordl.cost
"po-ordl.cost" ? ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.po-ordl.pr-uom
"po-ordl.pr-uom" "UOM" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.po-ord.buyer
"po-ord.buyer" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"is-it-polinestat() @ cPoLineStatus" "Line Status" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"is-it-postat() @ cPoStatus" "PO Status" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"is-it-paid() @ v-paidflg" "Paid" "YES / NO" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > ASI.po-ordl.cust-no
"po-ordl.cust-no" "Customer#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > ASI.po-ordl.line
"po-ordl.line" "Line #" ">>>>>>>" "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:
    {methods/template/brwrowdisplay.i}
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
  {methods/template/sortindicator.i} 
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.


  ASSIGN
   lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.
  IF lv-sort-by = "statl" THEN ASSIGN lv-sort-by = "stat" .
  IF lv-column-nam EQ "lv-t-rec-qty" THEN RETURN NO-APPLY.
  ELSE
  IF lv-column-nam EQ "job-no2" OR
     lv-column-nam EQ "s-num"   THEN
    ASSIGN
     lv-column-nam = "job-no"
     lv-column-lab = "Job#".

  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.
  ELSE
    ASSIGN
     lv-sort-by     = lv-column-nam
     lv-sort-by-lab = lv-column-lab.

    IF lv-column-lab =  "Line Status" AND lv-sort-by = "stat" THEN DO:
    ASSIGN lv-sort-by = "statl" .
     lv-sort-by-lab = lv-column-lab.
    END.

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  RUN dispatch ("open-query").
  {methods/template/sortindicatorend.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  RUN set-value.

  RUN dept-pan-image-proc.

END.

/*DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}

END.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
    cFormattedJobno      = DYNAMIC-FUNCTION (
                                       "fAddSpacesToString" IN hdJobProcs, fi_job-no:SCREEN-VALUE, 6, TRUE
                                   )
            .
    fi_job-no:SCREEN-VALUE = cFormattedJobno .    
    
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     tb_unpaid
     tb_paid
     fi_vend-no
     fi_i-no
     fi_vend-i-no
     fi_po-no
     fi_job-no
     fi_job-no2
     fi_due-date
     ll-first = NO
     tb_open
     tb_closed
     tb_hold
     tb_approved
     lButtongoPressed = YES.

    RUN dispatch ("open-query").

    APPLY "VALUE-CHANGED":U TO BROWSE {&browse-name}.
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
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

    RUN set-defaults.
    lv-show-next = YES.
    ENABLE btn_next .

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
    RUN set-defaults.
    lv-show-prev = YES.
    ENABLE btn_next .
    APPLY "choose" TO btn_go.
  END.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show B-table-Win
ON CHOOSE OF btn_show IN FRAME F-Main /* Show All */
DO:
  DO WITH FRAME {&FRAME-NAME}:
/*     RUN set-defaults. */

   ASSIGN
/*       tb_paid:SCREEN-VALUE  = "yes" */
      ll-show-all = YES.

    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON HELP OF fi_i-no IN FRAME F-Main
DO:
  /*DEFINE VARIABLE rtnValue AS CHARACTER NO-UNDO.

  RUN windows/l-poordl2.w (g_company,tb_open,tb_closed,SELF:SCREEN-VALUE,OUTPUT rtnValue).
  IF rtnValue EQ '' OR rtnValue EQ ? THEN RETURN NO-APPLY.
  SELF:SCREEN-VALUE = ENTRY(2,rtnValue).
  */
  DEF VAR lv-itemtype AS cha NO-UNDO.
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR look-recid AS RECID NO-UNDO.

  RUN windows/l-itmtyp.w (OUTPUT lv-itemtype).
  IF lv-itemtype = "RM" THEN DO:
     RUN windows/l-itmall.w (g_company, "","", fi_i-no:SCREEN-VALUE,"", OUTPUT char-val, OUTPUT look-recid).
     IF char-val <> "" THEN ASSIGN fi_i-no:SCREEN-VALUE = ENTRY(1,char-val).
  END.
  ELSE DO:  /* finished good */
     RUN windows/l-itemf2.w (g_company, "", fi_i-no:screen-value,"", OUTPUT char-val, OUTPUT look-recid).
     IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON VALUE-CHANGED OF fi_i-no IN FRAME F-Main
DO:
    /* Progress does not preserve trailing spaces in a fill-in's screen-value attribute */
    /* So once the value is converted to upper case any trailing spaces will be removed in fill-in*/
    /* So do not convert current value to uppercase when last key pressed is a space */
    IF LASTKEY EQ 32 THEN 
        RETURN.
    
    {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON HELP OF fi_job-no IN FRAME F-Main
DO:

  DEF VAR lv-itemtype AS cha NO-UNDO.
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR look-recid AS RECID NO-UNDO.

  RUN windows/l-jobpo.w (g_company, FOCUS:SCREEN-VALUE,OUTPUT char-val, OUTPUT look-recid).
     IF char-val <> "" THEN ASSIGN fi_job-no:SCREEN-VALUE = ENTRY(1,char-val)
                                    fi_job-no2:SCREEN-VALUE = ENTRY(2,char-val). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON VALUE-CHANGED OF fi_job-no IN FRAME F-Main
DO:
  IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_vend-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend-i-no B-table-Win
ON VALUE-CHANGED OF fi_vend-i-no IN FRAME F-Main
DO:
  IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_vend-no B-table-Win
ON VALUE-CHANGED OF fi_vend-no IN FRAME F-Main
DO:
  IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_closed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_closed B-table-Win
ON VALUE-CHANGED OF tb_closed IN FRAME F-Main /* Closed */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_open
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_open B-table-Win
ON VALUE-CHANGED OF tb_open IN FRAME F-Main /* Open */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_paid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_paid B-table-Win
ON VALUE-CHANGED OF tb_paid IN FRAME F-Main /* Paid */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_unpaid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_unpaid B-table-Win
ON VALUE-CHANGED OF tb_unpaid IN FRAME F-Main /* UnPaid */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

RUN po/po-sysct.p.

{methods/ctrl-a_browser.i}
{sys/inc/f3help.i}

&SCOPED-DEFINE cellColumnDat poinqb-po-inq

{methods/browsers/setCellColumns.i}

SESSION:DATA-ENTRY-RETURN = YES.

 DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}  /* task 07101525 */
    fi_due-date = DATE(fi_due-date:SCREEN-VALUE)  .
    IF fi_due-date LT TODAY - 365  THEN
        ASSIGN
           fi_due-date:SCREEN-VALUE = STRING(TODAY - 180) 
           fi_due-date = TODAY - 180 .
  END.


&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}
/* Ticket# : 92946
   Hiding this widget for now, as browser's column label should be indicating the column which is sorted by */
fi_sort-by:HIDDEN IN FRAME {&frame-name} = TRUE.
fi_sort-by:VISIBLE IN FRAME {&frame-name} = FALSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-po-best B-table-Win 
PROCEDURE add-po-best :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR op-job-no AS CHAR NO-UNDO.
   DEF VAR op-job-no2 AS INT NO-UNDO.

   RUN po\po-autobrd.w(OUTPUT op-job-no,
                       OUTPUT op-job-no2).

   IF op-job-no NE "" THEN
   DO:
      FIND FIRST job WHERE
           job.company EQ cocode AND
           job.job-no EQ op-job-no AND
           job.job-no2 EQ op-job-no2
           NO-LOCK NO-ERROR.

      IF AVAIL job THEN
      DO:
         fil_id = RECID(job).
         RUN po\doPo.p (YES) /* Yes Indicates to prompt for RM */.
         RUN local-open-query.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearFilters B-table-Win 
PROCEDURE clearFilters :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiPONo AS INTEGER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          fi_vend-no:SCREEN-VALUE = ""
          fi_i-no:SCREEN-VALUE = ""
          fi_vend-i-no:SCREEN-VALUE = ""
          fi_po-no:SCREEN-VALUE = ""
          fi_job-no:SCREEN-VALUE = ""
          fi_job-no2:SCREEN-VALUE = ""
          fi_vend-no
          fi_i-no
          fi_vend-i-no
          fi_po-no
          fi_job-no
          fi_job-no2
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dept-pan-image-proc B-table-Win 
PROCEDURE dept-pan-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-spec AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

   FIND FIRST notes WHERE notes.rec_key = po-ord.rec_key
       NO-LOCK NO-ERROR.

   IF AVAIL notes THEN
      v-spec = TRUE.
   ELSE v-spec = FALSE.

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'spec-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN dept-pen-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE liPOFrom AS INT NO-UNDO.
DEFINE VARIABLE liPOTo AS INT NO-UNDO.
DEFINE VARIABLE lcVendFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcVendTo   AS CHAR NO-UNDO.
DEFINE VARIABLE lcItemFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcItemTo   AS CHAR NO-UNDO.
DEFINE VARIABLE lcVendItemFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcVendItemTo   AS CHAR NO-UNDO.
DEFINE VARIABLE lcJobFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcJobTo   AS CHAR NO-UNDO.
DEFINE VARIABLE liJob2From AS INT NO-UNDO.
DEFINE VARIABLE liJob2To   AS INT NO-UNDO.
DEFINE VARIABLE ldDateFrom  AS DATE NO-UNDO.
DEFINE VARIABLE ldDateTo    AS DATE NO-UNDO.
DEFINE VARIABLE liOpenClosed AS INT NO-UNDO.


IF fi_po-no NE 0 THEN
    ASSIGN 
        liPOFrom = fi_po-no
        liPOTo = liPOFrom.
IF fi_vend-no NE "" THEN
    ASSIGN
        lcVendFrom = fi_vend-no
        lcVendTo = lcVendFrom.
IF fi_i-no NE "" THEN
    ASSIGN
        lcItemFrom = fi_i-no
        lcItemTo = lcItemFrom.
IF fi_vend-i-no NE "" THEN
    ASSIGN 
        lcVendItemFrom = fi_vend-i-no
        lcVendItemTo = lcVendItemFrom.
IF fi_due-date GT 01/01/0001 THEN
    ASSIGN
        ldDateFrom = fi_due-date.
IF fi_job-no NE "" THEN
    ASSIGN
        lcJobFrom = fi_job-no
        lcJobTo = lcJobFrom
        liJob2From = fi_job-no2
        liJob2To = liJob2From.

IF tb_open AND tb_closed THEN
    liOpenClosed = 3.
ELSE IF tb_open THEN
    liOpenClosed = 1.
ELSE
    liOpenClosed = 2.

    RUN po/rd-poexp.w (liPOFrom,
                        liPOTo,
                        lcVendFrom,
                        lcVendTo,
                        lcItemFrom,
                        lcItemTo,
                        lcVendItemFrom,
                        lcVendItemTo,
                        lcJobFrom,
                        lcJobTo,
                        liJob2From,
                        liJob2To,
                        ldDateFrom,
                        ldDateTo,
                        liOpenClosed).


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
  IF ll-first THEN DO:
    RUN set-defaults.
    RUN query-first.
  END.
  ELSE
    RUN query-go.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-first B-table-Win 
PROCEDURE local-get-first :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-first':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue2.i}

  IF AVAIL po-ord THEN do:
     RUN paper-clip-image-proc(INPUT po-ord.rec_key).
     RUN dept-pan-image-proc.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-last B-table-Win 
PROCEDURE local-get-last :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-last':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue2.i}

  IF AVAIL po-ord THEN do:
     RUN paper-clip-image-proc(INPUT po-ord.rec_key).
     RUN dept-pan-image-proc.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-next B-table-Win 
PROCEDURE local-get-next :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-next':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue2.i}

  IF AVAIL po-ord THEN do:
     RUN paper-clip-image-proc(INPUT po-ord.rec_key).
     RUN dept-pan-image-proc.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-prev B-table-Win 
PROCEDURE local-get-prev :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-prev':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue2.i}

  IF AVAIL po-ord THEN do:
     RUN paper-clip-image-proc(INPUT po-ord.rec_key).
     RUN dept-pan-image-proc.
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
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ll-open AS LOG INIT ? NO-UNDO.
  DEFINE VARIABLE cScreenType AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  {methods/run_link.i "container-source" "GetScreenType" "(Output cScreenType)"}
  
  RUN Browser_GetRecordAndTimeLimit(
    INPUT  cocode,
    INPUT  cScreenType,
    OUTPUT iRecordLimit,
    OUTPUT dQueryTimeLimit,
    OUTPUT lEnableShowAll
    ).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN setCellColumns.

  ASSIGN
   po-ordl.po-no:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ord.vend-no:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.job-no:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.job-no2:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.s-num:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.i-no:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.i-name:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.vend-i-no:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.ord-qty:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.pr-qty-uom:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.t-rec-qty:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.cons-uom:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ord.buyer:READ-ONLY IN BROWSE {&browse-name} = YES 
   po-ordl.due-date:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ord.ship-id:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ord.ship-name:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.s-wid:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.s-len:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.cost:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.pr-uom:READ-ONLY IN BROWSE {&browse-name} = YES
   po-ordl.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES 
   .

  /*FI_moveCol = "Sort".
  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.*/

  RUN set-focus.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"inquiry-source",OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO WITH FRAME {&FRAME-NAME}:
    RUN get-ip-rowid IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rowid, OUTPUT ll-open).

    FIND itemfg WHERE ROWID(itemfg) EQ lv-rowid NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN DO:
      ASSIGN
       ll-first               = NO
       fi_i-no:SCREEN-VALUE   = itemfg.i-no
       tb_open:SCREEN-VALUE   = STRING(ll-open EQ ? OR ll-open)
       tb_closed:SCREEN-VALUE = STRING(ll-open EQ ? OR NOT ll-open)
       tb_paid:SCREEN-VALUE = STRING(ll-open EQ ? OR ll-open) 
       lInquery = YES .
       .

      APPLY "choose" TO btn_go.
      APPLY "entry" TO fi_i-no.
    END.
  END.

  APPLY "VALUE-CHANGED":U TO BROWSE {&browse-name}.

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
  IF INDEX(fi_vend-no,"*")   GT 0 OR
     INDEX(fi_i-no,"*")      GT 0 OR
     INDEX(fi_vend-i-no,"*") GT 0 THEN
     lIsMatches = YES.
  ELSE lIsMatches = NO.   
     
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-show-all THEN 
      RUN pPrepareAndExecuteQueryForShowAll.
      
  ELSE IF lv-show-prev OR lv-show-next THEN
  DO:
      //RUN show-prev-next.
      RUN pPrepareAndExecuteQueryForPrevNext(
            IF lv-show-prev THEN lv-last-show-po-no ELSE lv-first-show-po-no,
            INPUT lv-show-prev
            ).
  END.
  ELSE /*first query or go button */ /*RUN first-query.*/
  RUN pPrepareAndExecuteQuery(
      INPUT IF lButtongoPressed THEN NO ELSE YES /* If Button go is pressed then only show the limit alert */ 
      ).

  IF lButtongoPressed THEN 
        lButtongoPressed = NO.  
    
  IF AVAIL {&FIRST-TABLE-IN-QUERY-{&browse-name}} THEN DO:
    RUN dispatch ("display-fields").

    RUN dispatch ("row-changed"). 
    
    GET LAST {&browse-name}.
    IF AVAIL {&FIRST-TABLE-IN-QUERY-{&browse-name}} THEN DO:
     
        ASSIGN
          lv-last-rowid  = ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}})
          lv-last-show-po-no = po-ordl.po-no.     
    END.

  
    GET FIRST {&browse-name}.
    IF AVAIL {&FIRST-TABLE-IN-QUERY-{&browse-name}} THEN DO:
     
        ASSIGN
          lv-frst-rowid  = ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}})
          lv-first-show-po-no = po-ordl.po-no.         
    END. 
   
  END.

  ASSIGN
    lv-show-prev = NO
    lv-show-next = NO
    .

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
  ll-new-record = NO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN set-value.

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
     v-col-move = NOT v-col-move.
  /*   FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
  DISPLAY FI_moveCol.*/
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

  IF ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}}) EQ lv-last-rowid THEN
    op-nav-type = "L".

  IF ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}}) EQ lv-frst-rowid THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE one-row-query B-table-Win 
PROCEDURE one-row-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    DEFINE VARIABLE cQuery      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSortPhrase AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse   AS CHARACTER NO-UNDO.
    
    IF fi_due-date LE 10/01/2018 AND lv-sort-by EQ "rec_key" THEN 
    lv-sort-by = "po-no".          
   
    ASSIGN
        cBrowseWhereClause = " AND ROWID(po-ordl) EQ " + "TO-ROWID(" + "'" + STRING(ip-rowid) + "')"
        cSortPhrase        = " BY " + fGetSortCondition(lv-sort-by, lv-sort-by-lab)
                           + (IF ll-sort-asc THEN "" ELSE " DESCENDING")
                           + " BY po-ordl.line BY po-ordl.i-no"
                           .            
    RUN pQueryString (cBrowseWhereClause, cSortPhrase, OUTPUT cQuery).                 
    RUN Browse_PrepareAndExecuteBrowseQuery(
        INPUT  BROWSE {&BROWSE-NAME}:QUERY, /* Browse Query Handle */      
        INPUT  cQuery,                      /* BRowse Query */             
        INPUT  NO,                          /* Show limit alert? */        
        INPUT  0,                           /* Record limit */             
        INPUT  0,                           /* Time Limit */               
        INPUT  lEnableShowAll,              /* Enable ShowAll Button */    
        OUTPUT cResponse
        ).               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE paper-clip-image-proc B-table-Win 
PROCEDURE paper-clip-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.

   DEF VAR v-po-no AS CHAR NO-UNDO.
   DEF VAR v-att AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

   {sys/ref/attachpologic.i}

   IF v-po-no <> "" THEN
      v-att = CAN-FIND(FIRST asi.attach WHERE
              attach.rec_key EQ v-rec-key-list AND          
              attach.company = cocode AND
              attach.est-no = v-po-no).

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN paper-clip-image IN WIDGET-HANDLE(char-hdl) (INPUT v-att).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssignCommonRecords B-table-Win 
PROCEDURE pAssignCommonRecords PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Assign Common Records for the query
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopcQueryBuffer      AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcFieldBuffer      AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcFieldName        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplIsBreakByUsed    AS LOGICAL   NO-UNDO.
   
    ASSIGN         
/*        iopcQueryBuffer   = "po-ordl,po-ord,reftable,ap-invl,ap-inv,ap-ctrl"*/
        iopcQueryBuffer   = "po-ordl,po-ord"
        iopcFieldBuffer   = "po-ordl"
        iopcFieldName     = "po-no" 
        ioplIsBreakByUsed = YES 
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrepareAndExecuteQuery B-table-Win 
PROCEDURE pPrepareAndExecuteQuery :
/*------------------------------------------------------------------------------
 Purpose: Private procedure to prepare and execute query in browse
 Notes:
------------------------------------------------------------------------------*/   
    DEFINE INPUT PARAMETER iplInitialLoad    AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cLimitingQuery        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBrowseQuery          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSortPhrase           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iRecordCount          AS INTEGER   NO-UNDO.
    
    RUN pAssignCommonRecords(
        INPUT-OUTPUT cQueryBuffers,
        INPUT-OUTPUT cFieldBuffer,
        INPUT-OUTPUT cFieldName,
        INPUT-OUTPUT lIsBreakByUsed
        ).                       
    ASSIGN
        cBrowseWhereClause = ""
        cSortPhrase        = " BREAK BY po-ordl.po-no DESCENDING"
                           .                
    RUN pQueryString (cBrowseWhereClause, cSortPhrase, OUTPUT cLimitingQuery).             
    /* Limit the query if order no is 0 or cadd No is Blank */                    
    IF fi_po-no EQ 0 THEN
        IF iplInitialLoad THEN DO:
            FOR EACH po-ordl NO-LOCK 
                WHERE po-ordl.company  EQ cocode 
                  AND po-ordl.opened   EQ TRUE 
                  AND po-ordl.due-date GE fi_due-date, 
                FIRST po-ord NO-LOCK 
                WHERE po-ord.company EQ po-ordl.company 
                  AND po-ord.po-no EQ po-ordl.po-no 
                BREAK BY po-ordl.po-no DESCENDING:                
                IF FIRST-OF(po-ordl.po-no) THEN 
                    iRecordCount = iRecordCount + 1.
                cResponse = STRING(po-ordl.po-no).
                IF iRecordCount GE iInitialQueryLimit THEN 
                    LEAVE.
            END. 
        END.
        ELSE DO:
            RUN Browse_PrepareAndExecuteLimitingQuery(
                INPUT  cLimitingQuery,   /* Query */
                INPUT  cQueryBuffers,    /* Buffers Name */
                INPUT  iRecordLimit,     /* Record Limit */
                INPUT  dQueryTimeLimit,  /* Time Limit*/
                INPUT  lEnableShowAll,   /* Enable ShowAll Button? */
                INPUT  cFieldBuffer,     /* Buffer name to fetch the field's value*/
                INPUT  cFieldName,       /* Field Name*/
                INPUT  iplInitialLoad,   /* Initial Query*/
                INPUT  lIsBreakByUsed,   /* Is breakby used */
                OUTPUT cResponse           
                ).              
        END.
    ELSE 
        cResponse = "PurchaseNo". /* For identification purpose */            
    IF cResponse EQ "" AND lButtongoPressed THEN  
        MESSAGE "No Records Found..."
            VIEW-AS ALERT-BOX ERROR.              
    ELSE IF cResponse EQ "ShowALL" THEN 
        RUN pPrepareAndExecuteQueryForShowAll.            
    ELSE DO:
        ASSIGN
            cBrowseWhereClause = IF fi_po-no EQ 0 THEN " AND po-ordl.po-no GE " + STRING(INTEGER(cResponse)) ELSE ""
            cSortPhrase        = " BY " + fGetSortCondition(lv-sort-by, lv-sort-by-lab)
                               + (IF ll-sort-asc THEN  "" ELSE " DESCENDING")
                               + " BY po-ordl.po-no BY po-ordl.line BY po-ordl.i-no"
                               .                
        RUN pQueryString (cBrowseWhereClause, cSortPhrase, OUTPUT cBrowseQuery).
        RUN Browse_PrepareAndExecuteBrowseQuery(
            INPUT  BROWSE {&BROWSE-NAME}:QUERY, /* Browse Query Handle */      
            INPUT  cBrowseQuery,                /* BRowse Query */             
            INPUT  NO,                          /* Show limit alert? */        
            INPUT  0,                           /* Record limit */             
            INPUT  0,                           /* Time Limit */               
            INPUT  lEnableShowAll,              /* Enable ShowAll Button */    
            OUTPUT cResponse                                                   
            ).
    END.                                     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrepareAndExecuteQueryForPrevNext B-table-Win 
PROCEDURE pPrepareAndExecuteQueryForPrevNext PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Private procedure to parepare an execute query for prev and next 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcValue    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPrevious AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE cLimitingQuery AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBrowseQuery   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSortPhrase    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse      AS CHARACTER NO-UNDO. 
           
    RUN pAssignCommonRecords(
        INPUT-OUTPUT cQueryBuffers,
        INPUT-OUTPUT cFieldBuffer,
        INPUT-OUTPUT cFieldName,
        INPUT-OUTPUT lIsBreakByUsed
        ).   

    ASSIGN
        cBrowseWhereClause = " AND po-ordl.po-no " + (IF iplPrevious THEN "LE " ELSE "GE ") + STRING(INTEGER(ipcValue))             
        cSortPhrase        = " BREAK BY po-ordl.po-no " + (IF iplPrevious THEN "DESCENDING" ELSE "" )
                           .
    RUN pQueryString (cBrowseWhereClause, cSortPhrase, OUTPUT cLimitingQuery).
    RUN Browse_PrepareAndExecuteLimitingQuery(
        INPUT  cLimitingQuery,   /* Query */
        INPUT  cQueryBuffers,    /* Buffers Name */
        INPUT  iRecordLimit,     /* Record Limit */
        INPUT  dQueryTimeLimit,  /* Time Limit*/
        INPUT  lEnableShowAll,   /* Enable ShowAll Button? */
        INPUT  cFieldBuffer,     /* Buffer name to fetch the field's value*/
        INPUT  cFieldName,       /* Field Name*/
        INPUT  NO,               /* Initial Query*/
        INPUT  lIsBreakByUsed,   /* Is breakby used */
        OUTPUT cResponse           
        ). 
    
    IF cResponse EQ "" THEN
        MESSAGE "No Records Found..."
            VIEW-AS ALERT-BOX ERROR. 
    ELSE IF cResponse EQ "ShowAll" THEN
        RUN pPrepareAndExecuteQueryForShowAll.
    ELSE DO:
        ASSIGN
            cBrowseWhereClause = " AND po-ordl.po-no " + (IF iplPrevious THEN "LE " ELSE "GE ") + STRING(INTEGER(ipcValue))
                               + " AND po-ordl.po-no " + (IF iplPrevious THEN "GE " ELSE "LE ") + STRING(INTEGER(cResponse))               
            cSortPhrase        = " BY " + fGetSortCondition(lv-sort-by, lv-sort-by-lab)
                               + (IF ll-sort-asc THEN "" ELSE " DESCENDING")
                               + " BY po-ordl.po-no BY po-ordl.line BY po-ordl.i-no"
                               .
        RUN pQueryString (cBrowseWhereClause, cSortPhrase, OUTPUT cBrowseQuery).
        RUN Browse_PrepareAndExecuteBrowseQuery(
            INPUT  BROWSE {&BROWSE-NAME}:QUERY, /* Browse Query Handle */
            INPUT  cBrowseQuery,                /* Browse Query */
            INPUT  NO,                          /* Show limit alert? */
            INPUT  0,                           /* Record limit */
            INPUT  0,                           /* Time Limit */
            INPUT  lEnableShowAll,              /* Enable ShowAll Button */
            OUTPUT cResponse
            ).
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrepareAndExecuteQueryForShowAll B-table-Win 
PROCEDURE pPrepareAndExecuteQueryForShowAll PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Private procedure to show all records in browse
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cShowAllQuery AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSortPhrase   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse     AS CHARACTER NO-UNDO.
               
    cSortPhrase = " BY " + fGetSortCondition(lv-sort-by, lv-sort-by-lab)
                + (IF ll-sort-asc THEN "" ELSE " DESCENDING")
                + " BY po-ordl.po-no BY po-ordl.line BY po-ordl.i-no"
                .   
    RUN pQueryString ("", cSortPhrase, OUTPUT cShowAllQuery).
                   
    RUN Browse_PrepareAndExecuteBrowseQuery(
        INPUT  BROWSE {&BROWSE-NAME}:QUERY, /* Browse Query Handle */      
        INPUT  cShowAllQuery,               /* BRowse Query */             
        INPUT  NO,                          /* Show limit alert? */        
        INPUT  0,                           /* Record limit */             
        INPUT  0,                           /* Time Limit */               
        INPUT  lEnableShowAll,              /* Enable ShowAll Button */    
        OUTPUT cResponse                                                  
        ).
    ll-show-all = NO.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQueryString B-table-Win 
PROCEDURE pQueryString PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcWhereClause AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSortPhrase  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcQueryString AS CHARACTER NO-UNDO.

    IF (tb_paid EQ NO AND tb_unpaid EQ NO) OR
       (tb_open EQ NO AND tb_closed EQ NO) THEN
    ipcWhereClause = ipcWhereClause + " AND FALSE".
    opcQueryString = "FOR EACH po-ordl NO-LOCK"
                   + " WHERE po-ordl.company EQ " + QUOTER(cocode)
                   + ipcWhereClause
                   + pGetWhereCriteria("po-ordl")
                   + ", FIRST po-ord NO-LOCK"
                   + " WHERE po-ord.company EQ po-ordl.company AND po-ord.po-no EQ po-ordl.po-no " 
                   + pGetWhereCriteria("po-ord")
/*                   + ", FIRST reftable OUTER-JOIN NO-LOCK"                                           */
/*                   + " WHERE reftable.reftable EQ 'AP-INVL'"                                         */
/*                   + " AND reftable.company EQ ''"                                                   */
/*                   + " AND reftable.loc EQ ''"                                                       */
/*                   + " AND reftable.code EQ  STRING(po-ordl.po-no,'9999999999')"                     */
/*                   + " AND (reftable.spare-char-1 EQ '' OR reftable.spare-char-1 EQ po-ordl.company)"*/
/*                   + ", FIRST ap-invl OUTER-JOIN NO-LOCK"                                            */
/*                   + " WHERE ap-invl.i-no EQ int(reftable.code2)"                                    */
/*                   + " AND ap-invl.company EQ " + QUOTER(cocode)                                     */
/*                   + " AND ap-invl.po-no EQ po-ordl.po-no"                                           */
/*                   + " AND (ap-invl.line + (ap-invl.po-no * -1000)) EQ po-ordl.line"                 */
/*                   + ", FIRST ap-inv OUTER-JOIN NO-LOCK"                                             */
/*                   + " WHERE ap-inv.i-no EQ ap-invl.i-no"                                            */
/*                   + " AND ap-inv.company EQ " + QUOTER(cocode)                                      */
/*                   + " AND ap-inv.due EQ 0"                                                          */
/*                   + ", FIRST ap-ctrl NO-LOCK"                                                       */
/*                   + " WHERE ap-ctrl.company EQ po-ordl.company"                                     */
/*                   + " AND (( " + QUOTER(tb_unpaid) + " AND " + QUOTER(tb_paid) + ")"                */
/*                   + " OR (" + QUOTER(tb_paid) + " AND AVAIL(ap-inv))"                               */
/*                   + " OR (" + QUOTER(tb_unpaid) + " AND NOT AVAIL(ap-inv))) "                       */
                   + ipcSortPhrase
                   .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printPO B-table-Win 
PROCEDURE printPO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*IF NOT AVAILABLE oe-ordl THEN RETURN.
  RUN custom/setUserPrint.p (po-ordl.company,'po-ordl_.',
                             'begin_po-no,end_po-no',
                             po-ordl.po-no /*+ ',' + STRING(po-ordl.po-no2)*/ ).
  RUN Get_Procedure IN Persistent-Handle ('po-ordl_.',OUTPUT run-proc,yes).*/
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

  ASSIGN
   ll-first      = YES
   ll-new-record = YES.
   RUN set-defaults.

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
 lv-tmp-rowid = ROWID(po-ordl).

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
 
  DEF BUFFER b-po-ordl FOR po-ordl.
  DEF BUFFER b-po-ord  FOR po-ord.   

  FIND b-po-ord WHERE ROWID(b-po-ord) EQ ip-rowid NO-LOCK NO-ERROR.
  IF AVAIL b-po-ord THEN DO:
    FIND FIRST b-po-ordl WHERE
         b-po-ordl.company EQ b-po-ord.company AND
         b-po-ordl.po-no EQ b-po-ord.po-no
         NO-LOCK.
    ip-rowid = ROWID(b-po-ordl).
  END. 
  
  DO WITH FRAME {&FRAME-NAME}:
    RUN dispatch ("open-query").
    RUN repo-query (ip-rowid).
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
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      RUN one-row-query (ip-rowid).
      REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    END.
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
  {src/adm/template/snd-list.i "po-ordl"}
  {src/adm/template/snd-list.i "po-ord"}

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
     tb_unpaid:SCREEN-VALUE    = "yes"
     tb_paid:SCREEN-VALUE      = "yes"
     fi_vend-no:SCREEN-VALUE   = ""
     fi_i-no:SCREEN-VALUE      = ""
     fi_vend-i-no:SCREEN-VALUE = ""
     fi_po-no:SCREEN-VALUE     = ""
     fi_job-no:SCREEN-VALUE    = ""
     fi_job-no2:SCREEN-VALUE   = ""
     tb_open:SCREEN-VALUE      = "yes"
     tb_closed:SCREEN-VALUE    = "no"
     .
     IF fi_due-date:SCREEN-VALUE = "" THEN
     fi_due-date:SCREEN-VALUE  = STRING(TODAY - 180) .   /* task 07101525 */ 
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

  APPLY 'ENTRY':U TO fi_po-no IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-value B-table-Win 
PROCEDURE set-value :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF ll-new-record THEN DO:
    {methods/template/local/setvalue.i}

    IF AVAIL po-ordl THEN
       RUN paper-clip-image-proc(INPUT po-ordl.rec_key).
  END.
  ELSE DO:
    {methods/template/local/setvalue2.i}

    IF AVAIL po-ord THEN
       RUN paper-clip-image-proc(INPUT po-ord.rec_key).
  END.

  RUN setUserPrint.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setUserPrint B-table-Win 
PROCEDURE setUserPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF AVAIL po-ord THEN
    RUN custom/setUserPrint.p (g_company,'po-ordl_.',
                               'begin_po-no,end_po-no,begin_vend-no,end_vend-no,tb_reprint,tb_reprint-closed',
                               STRING(po-ord.po-no) + ',' + STRING(po-ord.po-no) + ',' +
                               po-ord.vend-no + ',' + po-ord.vend-no + ',' + STRING(po-ord.printed) + ',' +
                               (IF po-ord.stat EQ "C" AND po-ord.printed THEN "YES" ELSE "NO")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-prev-next B-table-Win 
PROCEDURE show-prev-next :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-po-no AS INT NO-UNDO.

IF fi_job-no NE "" THEN fi_job-no = FILL(" ",6 - LENGTH(TRIM(fi_job-no))) + TRIM(fi_job-no).

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
     AND sys-ctrl.name EQ "POBROWSE"
     NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN sys-ctrl.company = cocode
         sys-ctrl.name    = "POBROWSE"
         sys-ctrl.descrip = "# of Records to be displayed in PO browser"
         sys-ctrl.log-fld = YES
         sys-ctrl.char-fld = ""
         sys-ctrl.int-fld = 30.
END.

IF lv-show-prev THEN DO:

  IF fi_po-no NE 0 THEN DO:
        IF lIsMatches THEN DO:
            &SCOPED-DEFINE open-query          ~
              OPEN QUERY {&browse-name}        ~
              {&for-each1}                     ~
                AND po-ordl.po-no EQ fi_po-no  ~
                USE-INDEX po-no NO-LOCK,       ~
                {&for-each2},                  ~
                {&for-each3},                  ~
                {&for-each4},                  ~
                {&for-each5},                  ~
                {&for-each6}
    
    
            IF LOOKUP(lv-sort-by,lv-sort-list1) GT 0 THEN
                IF ll-sort-asc THEN 
                    {&open-query} {&sortby-phrase-asc1}.
                ELSE 
                    {&open-query} {&sortby-phrase-desc1}.
            ELSE
                IF ll-sort-asc THEN 
                    {&open-query} {&sortby-phrase-asc2}.
                ELSE 
                    {&open-query} {&sortby-phrase-desc2}.
        END. 
        ELSE DO:  
            &SCOPED-DEFINE open-query          ~
              OPEN QUERY {&browse-name}        ~
              {&for-each8}                     ~
                AND po-ordl.po-no EQ fi_po-no  ~
                USE-INDEX po-no NO-LOCK,       ~
                {&for-each2},                  ~
                {&for-each3},                  ~
                {&for-each4},                  ~
                {&for-each5},                  ~
                {&for-each6}
    
    
            IF LOOKUP(lv-sort-by,lv-sort-list1) GT 0 THEN
                IF ll-sort-asc THEN 
                    {&open-query} {&sortby-phrase-asc1}.
                ELSE 
                    {&open-query} {&sortby-phrase-desc1}.
            ELSE
                IF ll-sort-asc THEN 
                    {&open-query} {&sortby-phrase-asc2}.
                ELSE 
                    {&open-query} {&sortby-phrase-desc2}.                 
        END.    
  END.

  ELSE IF fi_job-no NE "" THEN DO:

    {&for-each1}
      AND po-ordl.po-no LE lv-last-show-po-no
      USE-INDEX job-no NO-LOCK,
      {&for-each2}

      BREAK BY po-ordl.po-no DESC:
        IF FIRST-OF(po-ordl.po-no)  THEN li = li + 1.
        lv-po-no = po-ordl.po-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.
    RUN pPrepareAndExecutePrevNext(
        INPUT lv-po-no,
        INPUT lv-last-show-po-no
        ).
  END.

  ELSE IF fi_i-no NE "" THEN DO:

    {&for-each1}
      AND po-ordl.po-no LE lv-last-show-po-no
      USE-INDEX ITEM NO-LOCK,
      {&for-each2}

      BREAK BY po-ordl.po-no DESC:
        IF FIRST-OF(po-ordl.po-no) THEN li = li + 1.
        lv-po-no = po-ordl.po-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.
      
    RUN pPrepareAndExecutePrevNext(
        INPUT lv-po-no,
        INPUT lv-last-show-po-no
        ).
  END.

  ELSE IF fi_vend-i-no NE "" THEN DO:

    {&for-each1}
      AND po-ordl.po-no LE lv-last-show-po-no
      USE-INDEX vend-i-no NO-LOCK,
      {&for-each2}

      BREAK BY po-ordl.po-no DESC:
        IF FIRST-OF(po-ordl.po-no) THEN li = li + 1.
        lv-po-no = po-ordl.po-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

    RUN pPrepareAndExecutePrevNext(
        INPUT lv-po-no,
        INPUT lv-last-show-po-no
        ).
  END.

  ELSE IF tb_open NE tb_closed THEN DO:

    {&for-each1}
      AND po-ordl.po-no LE lv-last-show-po-no
      USE-INDEX opened NO-LOCK,
      {&for-each2}  

      BREAK BY po-ordl.po-no DESC:

        IF FIRST-OF(po-ordl.po-no) THEN li = li + 1.
        lv-po-no = po-ordl.po-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.
    RUN pPrepareAndExecutePrevNext(
        INPUT lv-po-no,
        INPUT lv-last-show-po-no
        ).
  END.

  ELSE DO:

    {&for-each1}
      AND po-ordl.po-no LE lv-last-show-po-no
      NO-LOCK,
      {&for-each2}

      BREAK BY po-ordl.po-no DESC:
        IF FIRST-OF(po-ordl.po-no) THEN li = li + 1.
        lv-po-no = po-ordl.po-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.
    RUN pPrepareAndExecutePrevNext(
        INPUT lv-po-no,
        INPUT lv-last-show-po-no
        ).
  END.
END.  /* lv-show-prev */
ELSE /*IF lv-show-next THEN*/ DO:

  IF fi_po-no NE 0 THEN DO:
        IF lIsMatches THEN DO:
            &SCOPED-DEFINE open-query          ~
              OPEN QUERY {&browse-name}        ~
              {&for-each1}                     ~
                AND po-ordl.po-no EQ fi_po-no  ~
                USE-INDEX po-no NO-LOCK,       ~
                {&for-each2},                  ~
                {&for-each3},                  ~
                {&for-each4},                  ~
                {&for-each5},                  ~
                {&for-each6}
    
    
            IF LOOKUP(lv-sort-by,lv-sort-list1) GT 0 THEN
                IF ll-sort-asc THEN 
                    {&open-query} {&sortby-phrase-asc1}.
                ELSE 
                    {&open-query} {&sortby-phrase-desc1}.
            ELSE
                IF ll-sort-asc THEN 
                    {&open-query} {&sortby-phrase-asc2}.
                ELSE 
                    {&open-query} {&sortby-phrase-desc2}.
        END. 
        ELSE DO:  
            &SCOPED-DEFINE open-query          ~
              OPEN QUERY {&browse-name}        ~
              {&for-each8}                     ~
                AND po-ordl.po-no EQ fi_po-no  ~
                USE-INDEX po-no NO-LOCK,       ~
                {&for-each2},                  ~
                {&for-each3},                  ~
                {&for-each4},                  ~
                {&for-each5},                  ~
                {&for-each6}
    
    
            IF LOOKUP(lv-sort-by,lv-sort-list1) GT 0 THEN
                IF ll-sort-asc THEN 
                    {&open-query} {&sortby-phrase-asc1}.
                ELSE 
                    {&open-query} {&sortby-phrase-desc1}.
            ELSE
                IF ll-sort-asc THEN 
                    {&open-query} {&sortby-phrase-asc2}.
                ELSE 
                    {&open-query} {&sortby-phrase-desc2}.                 
        END.    
  END.

  ELSE IF fi_job-no NE "" THEN DO:

    {&for-each1}
      AND po-ordl.po-no GE lv-first-show-po-no
      USE-INDEX job-no NO-LOCK,
      {&for-each2}

      BREAK BY po-ordl.po-no:
        IF FIRST-OF(po-ordl.po-no) THEN li = li + 1.
        lv-po-no = po-ordl.po-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.
    RUN pPrepareAndExecutePrevNext(
        INPUT lv-first-show-po-no,
        INPUT lv-po-no
        ).
  END.

  ELSE IF fi_i-no NE "" THEN DO:

    {&for-each1}
      AND po-ordl.po-no GE lv-first-show-po-no
      USE-INDEX ITEM NO-LOCK,
      {&for-each2}

      BREAK BY po-ordl.po-no:
        IF FIRST-OF(po-ordl.po-no) THEN li = li + 1.
        lv-po-no = po-ordl.po-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.
      
    RUN pPrepareAndExecutePrevNext(
        INPUT lv-first-show-po-no,
        INPUT lv-po-no
        ).      
  END.

  ELSE IF fi_vend-i-no NE "" THEN DO:

    {&for-each1}
      AND po-ordl.po-no GE lv-first-show-po-no
      USE-INDEX vend-i-no NO-LOCK,
      {&for-each2}

      BREAK BY po-ordl.po-no:
        IF FIRST-OF(po-ordl.po-no) THEN li = li + 1.
        lv-po-no = po-ordl.po-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

    RUN pPrepareAndExecutePrevNext(
        INPUT lv-first-show-po-no,
        INPUT lv-po-no
        ).
  END.

  ELSE IF tb_open NE tb_closed THEN DO:

    {&for-each1}
      AND po-ordl.po-no GE lv-first-show-po-no
      USE-INDEX opened NO-LOCK,
      {&for-each2}

      BREAK BY po-ordl.po-no:
        IF FIRST-OF(po-ordl.po-no) THEN li = li + 1.
        lv-po-no = po-ordl.po-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.
    RUN pPrepareAndExecutePrevNext(
        INPUT lv-first-show-po-no,
        INPUT lv-po-no
        ).
  END.

  ELSE DO:

    {&for-each1}
      AND po-ordl.po-no GE lv-first-show-po-no
      NO-LOCK,
      {&for-each2}

      BREAK BY po-ordl.po-no:
        IF FIRST-OF(po-ordl.po-no) THEN li = li + 1.
        lv-po-no = po-ordl.po-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

    RUN pPrepareAndExecutePrevNext(
        INPUT lv-first-show-po-no,
        INPUT lv-po-no
        ).
  END.
END.
*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-proc B-table-Win 
PROCEDURE value-changed-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
       IF AVAIL po-ord THEN do:
           RUN paper-clip-image-proc(INPUT po-ord.rec_key).
           RUN dept-pan-image-proc.
       END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dim-in-16 B-table-Win 
FUNCTION dim-in-16 RETURNS DECIMAL
  ( ip-dim AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN (ip-dim - TRUNC(ip-dim, 0)) * factor# + TRUNC(ip-dim, 0).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetSortCondition B-table-Win 
FUNCTION fGetSortCondition RETURNS CHARACTER
  (ipcSortBy AS CHARACTER, ipcSortLabel AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Retuns the sort condition based on the input 
 Notes:
------------------------------------------------------------------------------*/    
     RETURN IF ipcSortBy EQ "po-no"         THEN "STRING(po-ordl.po-no,'9999999999')"                                                         ELSE ~
            IF ipcSortBy EQ "vend-no"       THEN "po-ord.vend-no"                                                                             ELSE ~
            IF ipcSortBy EQ "ship-id"       THEN "po-ord.ship-id"                                                                             ELSE ~
            IF ipcSortBy EQ "ship-name"     THEN "po-ord.ship-name"                                                                           ELSE ~
            IF ipcSortBy EQ "job-no"        THEN "STRING(po-ordl.job-no,'x(6)') + STRING(po-ordl.job-no2,'99') + STRING(po-ordl.s-num,'999')" ELSE ~
            IF ipcSortBy EQ "i-no"          THEN "po-ordl.i-no"                                                                               ELSE ~
            IF ipcSortBy EQ "i-name"        THEN "po-ordl.i-name"                                                                             ELSE ~
            IF ipcSortBy EQ "vend-i-no"     THEN "po-ordl.vend-i-no"                                                                          ELSE ~
            IF ipcSortBy EQ "buyer"         THEN "po-ord.buyer"                                                                               ELSE ~
            IF ipcSortBy EQ "cPoStatus"     THEN "po-ord.stat"                                                                                ELSE ~
            IF ipcSortBy EQ "cust-no"       THEN "po-ordl.cust-no"                                                                            ELSE ~
            IF ipcSortBy EQ "cPolineStatus" THEN "STRING(po-ordl.stat)"                                                                       ELSE ~
            IF ipcSortBy EQ "line"          THEN "(STRING(po-ordl.po-no,'9999999999') + STRING(po-ordl.LINE,'9999'))"                         ELSE ~
            IF ipcSortBy EQ "ord-qty"       THEN "STRING(9999999999.99999 + po-ordl.ord-qty,'-9999999999.99999')"                             ELSE ~
            IF ipcSortBy EQ "pr-qty-uom"    THEN "po-ordl.pr-qty-uom"                                                                         ELSE ~
            IF ipcSortBy EQ "pr-uom"        THEN "po-ordl.pr-uom"                                                                             ELSE ~
            IF ipcSortBy EQ "t-rec-qty"     THEN "STRING(9999999999.99999 + po-ordl.t-rec-qty,'-9999999999.99999')"                           ELSE ~
            IF ipcSortBy EQ "cons-uom"      THEN "po-ordl.cons-uom"                                                                           ELSE ~
            IF ipcSortBy EQ "cost"          THEN "STRING(po-ordl.cost,'99999.99999')"                                                         ELSE ~
            IF ipcSortBy EQ "s-wid"         THEN "STRING(po-ordl.s-wid,'99999.99999')"                                                        ELSE ~
            IF ipcSortBy EQ "s-len"         THEN "STRING(po-ordl.s-len,'99999.99999')"                                                        ELSE ~
                                                 "STRING(YEAR(po-ordl.due-date),'9999')
                                                + STRING(MONTH(po-ordl.due-date),'99')
                                                + STRING(DAY(po-ordl.due-date),'99')"
            .
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getcurrentpo B-table-Win 
FUNCTION getcurrentpo RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF AVAIL po-ordl THEN
    RETURN po-ordl.po-no.
  ELSE RETURN -1.  /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetPOLine B-table-Win 
FUNCTION GetPOLine RETURNS INTEGER
  (  ):
/*------------------------------------------------------------------------------
 Purpose: Returns the current po's line
 Notes:
------------------------------------------------------------------------------*/
    IF AVAILABLE po-ordl 
        THEN RETURN po-ordl.line.
    ELSE RETURN -1.    

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-it-paid B-table-Win 
FUNCTION is-it-paid RETURNS LOGICAL
  (  /* parameter-definitions */  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR v-flg AS LOG.

FOR EACH  reftable NO-LOCK
    WHERE reftable.reftable EQ "AP-INVL" 
      AND reftable.company  EQ ""        
      AND reftable.loc      EQ ""        
      AND reftable.code     EQ STRING(po-ordl.po-no,"9999999999"),
    EACH  ap-invl NO-LOCK
    WHERE ap-invl.company           EQ po-ordl.company
      AND ap-invl.i-no              EQ int(reftable.code2) 
      AND ap-invl.po-no             EQ po-ordl.po-no 
      AND (ap-invl.line + 
           (ap-invl.po-no * -1000)) EQ po-ordl.line,
    EACH  ap-inv NO-LOCK
    WHERE ap-inv.company EQ ap-invl.company 
      AND ap-inv.i-no EQ ap-invl.i-no 
      AND ap-inv.due  EQ 0:

   v-flg = TRUE.

END.

  RETURN v-flg.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-it-polinestat B-table-Win 
FUNCTION is-it-polinestat RETURNS CHARACTER
  (  /* parameter-definitions */  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-result AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    
    IF AVAILABLE po-ordl THEN DO: 
        lc-result = po-ordl.stat .
        RUN oe/getStatusDesc.p( INPUT po-ordl.stat, OUTPUT cResult) .
        IF cResult NE "" THEN
            lc-result  = cResult .
    END.
    RETURN lc-result.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-it-postat B-table-Win 
FUNCTION is-it-postat RETURNS CHARACTER
  (  /* parameter-definitions */  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-result AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    
    IF AVAILABLE po-ord THEN DO: 
        lc-result = po-ord.stat .
        RUN oe/getStatusDesc.p( INPUT po-ord.stat, OUTPUT cResult) .
        IF cResult NE "" THEN
            lc-result  = cResult .
    END.
    RETURN lc-result.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isPaidCheck B-table-Win 
FUNCTION isPaidCheck RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*    FOR EACH reftable NO-LOCK                                         */
/*        WHERE reftable.reftable EQ "AP-INVL"                          */
/*          AND reftable.company  EQ ""                                 */
/*          AND reftable.loc      EQ ""                                 */
/*          AND reftable.code     EQ STRING(po-ordl.po-no,"9999999999"),*/
/*        EACH ap-invl NO-LOCK                                          */
/*        WHERE ap-invl.company   EQ po-ordl.company                    */
/*          AND ap-invl.i-no      EQ INTEGER(reftable.code2)            */
/*          AND ap-invl.po-no     EQ po-ordl.po-no                      */
/*          AND (ap-invl.line +                                         */
/*                  (ap-invl.po-no * -1000)) EQ po-ordl.line,           */
/*        EACH ap-inv                                                   */
/*        WHERE ap-inv.company EQ ap-invl.company                       */
/*          AND ap-inv.i-no    EQ ap-invl.i-no                          */
/*          AND ap-inv.due     EQ 0                                     */
/*        :                                                             */
/*        RETURN TRUE.                                                  */
/*    END. /* each reftable */                                          */
/*    RETURN FALSE.                                                     */

    IF CAN-FIND(FIRST reftable
                WHERE reftable.reftable EQ "AP-INVL"
                  AND reftable.company  EQ ""
                  AND reftable.loc      EQ ""
                  AND reftable.code     EQ STRING(po-ordl.po-no,"9999999999")) THEN DO:
        FIND FIRST reftable NO-LOCK
             WHERE reftable.reftable EQ "AP-INVL"
               AND reftable.company  EQ ""
               AND reftable.loc      EQ ""
               AND reftable.code     EQ STRING(po-ordl.po-no,"9999999999")
             NO-ERROR.
        IF AVAILABLE reftable THEN DO:
            IF CAN-FIND(FIRST ap-invl
                        WHERE ap-invl.company EQ po-ordl.company
                          AND ap-invl.i-no    EQ INTEGER(reftable.code2)
                          AND ap-invl.po-no   EQ po-ordl.po-no
                          AND (ap-invl.line +
                              (ap-invl.po-no * -1000)) EQ po-ordl.line) THEN
            FOR EACH ap-invl NO-LOCK
                WHERE ap-invl.company   EQ po-ordl.company
                  AND ap-invl.i-no      EQ INTEGER(reftable.code2)
                  AND ap-invl.po-no     EQ po-ordl.po-no
                  AND (ap-invl.line +
                      (ap-invl.po-no * -1000)) EQ po-ordl.line
                :
                RETURN CAN-FIND(FIRST ap-inv
                                WHERE ap-inv.company EQ ap-invl.company
                                  AND ap-inv.i-no    EQ ap-invl.i-no
                                  AND ap-inv.due     EQ 0).
            END. /* each ap-invl */
        END. /* if can-find */
    END. /* if can-find */
    RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pGetWhereCriteria B-table-Win 
FUNCTION pGetWhereCriteria RETURNS CHARACTER
  ( ipcTable AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose: Prepares and returns the where clause criteria based on table name
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cWhereCriteria AS CHARACTER NO-UNDO.
    
    IF ipcTable EQ "po-ord" THEN DO:
        IF tb_approved NE tb_hold THEN DO:
            IF tb_approved THEN 
            cWhereCriteria = " AND po-ord.stat NE 'H'".
            ELSE            
            IF tb_hold THEN
            cWhereCriteria = cWhereCriteria + " AND po-ord.stat EQ 'H'".
        END.
    END. 
    ELSE DO:         
        IF tb_open NE tb_closed THEN
        cWhereCriteria = cWhereCriteria + " AND po-ordl.opened EQ " + STRING(tb_open).
        IF fi_due-date NE ? THEN
        cWhereCriteria = cWhereCriteria + " AND po-ordl.due-date GE " + STRING(fi_due-date,"99/99/9999").  
        
        ASSIGN            
            cWhereCriteria = cWhereCriteria  
                           + (IF fi_po-no NE 0 THEN " AND po-ordl.po-no EQ " + STRING(fi_po-no) ELSE "")
                           + (IF INDEX(fi_vend-no,"*") GT 0 THEN " AND po-ordl.vend-no MATCHES " + QUOTER(fi_vend-no + "*") ELSE IF fi_vend-no NE "" THEN " AND po-ordl.vend-no BEGINS " + QUOTER(fi_vend-no) ELSE "")
                           + (IF INDEX(fi_i-no,"*") GT 0 THEN " AND po-ordl.i-no MATCHES " + QUOTER(fi_i-no + "*") ELSE IF fi_i-no NE "" THEN " AND po-ordl.i-no BEGINS " + QUOTER(fi_i-no) ELSE "")
                           + (IF INDEX(fi_vend-i-no,"*") GT 0 THEN " AND po-ordl.vend-i-no MATCHES " + QUOTER(fi_vend-i-no + "*") ELSE IF fi_vend-i-no NE "" THEN " AND po-ordl.vend-i-no BEGINS " + QUOTER(fi_vend-i-no) ELSE "")
                           + (IF fi_job-no NE "" THEN " AND po-ordl.job-no BEGINS " + QUOTER(fi_job-no) ELSE "")
                           + (IF fi_job-no NE "" AND fi_job-no2 NE 0 THEN " AND po-ordl.job-no2 EQ " + STRING(fi_job-no2) ELSE "")
                           . 
    END.     
    RETURN cWhereCriteria.      
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION qty-in-ord-uom B-table-Win 
FUNCTION qty-in-ord-uom RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ld AS DEC DECIMALS 10 EXTENT 2 NO-UNDO.

  DEF BUFFER b-po-ordl FOR po-ordl.



  FIND b-po-ordl WHERE ROWID(b-po-ordl) EQ ROWID(po-ordl) NO-LOCK NO-ERROR.

  ld[2] = b-po-ordl.t-rec-qty.

  IF b-po-ordl.item-type EQ YES                 AND
     b-po-ordl.pr-qty-uom NE b-po-ordl.cons-uom THEN DO:
    ld[2] = 0.

    FOR EACH rm-rcpth
        WHERE rm-rcpth.company   EQ b-po-ordl.company
          AND rm-rcpth.po-no     EQ STRING(b-po-ordl.po-no)
          AND rm-rcpth.po-line   EQ b-po-ordl.line
          AND rm-rcpth.i-no      EQ b-po-ordl.i-no
          AND rm-rcpth.rita-code EQ "R" NO-LOCK,
        EACH rm-rdtlh
        WHERE rm-rdtlh.r-no    EQ rm-rcpth.r-no
          AND rm-rdtlh.job-no  EQ b-po-ordl.job-no
          AND rm-rdtlh.job-no2 EQ b-po-ordl.job-no2
          AND rm-rdtlh.s-num   EQ b-po-ordl.s-num
        NO-LOCK:

      IF b-po-ordl.pr-qty-uom EQ "ROLL" AND rm-rdtlh.tag NE "" THEN ld[1] = 1.

      ELSE DO:
        ld[1] = rm-rdtlh.qty.

        IF rm-rcpth.pur-uom NE b-po-ordl.pr-qty-uom THEN DO:
          FIND FIRST item
              WHERE item.company EQ b-po-ordl.company
                AND item.i-no    EQ b-po-ordl.i-no
              NO-LOCK NO-ERROR.

          RUN custom/convquom.p(cocode, rm-rcpth.pur-uom, b-po-ordl.pr-qty-uom,
                                (IF AVAIL item THEN item.basis-w ELSE 0),
                                (IF b-po-ordl.pr-qty-uom EQ "ROLL" THEN 12
                                 ELSE b-po-ordl.s-len), b-po-ordl.s-wid,
                                (IF AVAIL item THEN item.s-dep ELSE 0),
                                ld[1], OUTPUT ld[1]).
        END.
      END.

      ld[2] = ld[2] + ld[1].
    END.
  END.

  IF b-po-ordl.pr-qty-uom EQ "EA" THEN DO:
    {sys/inc/roundup.i ld[2]}
  END.

  RETURN ld[2].   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

