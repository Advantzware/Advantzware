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

  File:  rminq\b-rmiinq.w

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

&SCOPED-DEFINE cellColumnDat browsers-hist
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

DEFINE VARIABLE lvFirstRowID AS ROWID NO-UNDO.
DEFINE VARIABLE lvLastRowID AS ROWID NO-UNDO.
DEF VAR v-called-setCellColumns AS LOG NO-UNDO.
DEF VAR v-col-move AS LOG NO-UNDO INIT TRUE.

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "trans-date" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "TR Date" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR ld-ext-cost AS DEC NO-UNDO.


DEF BUFFER rm-rcpth-1 FOR rm-rcpth.
DEF BUFFER rm-rdtlh-1 FOR rm-rdtlh.

&SCOPED-DEFINE key-phrase rm-rcpth.company EQ cocode

&SCOPED-DEFINE for-each1                              ~
    FOR EACH rm-rcpth                                 ~
        WHERE {&key-phrase}                           ~
          AND rm-rcpth.trans-date GE fi_date          ~
          AND rm-rcpth.i-no       BEGINS fi_rm-i-no   ~
          AND (rm-rcpth.i-no      EQ fi_rm-i-no OR fi_rm-i-no EQ "") ~
          AND rm-rcpth.rita-code  BEGINS fi_rita-code ~
          AND (rm-rcpth.po-no     EQ TRIM(STRING(fi_po-no,">>>>>>>>")) OR fi_po-no EQ 0) ~
          AND rm-rcpth.job-no     BEGINS fi_job-no ~
          AND (rm-rcpth.job-no    EQ fi_job-no OR fi_job-no EQ "")   ~
          AND (rm-rcpth.job-no2   EQ fi_job-no2 OR fi_job-no2 EQ 0 OR fi_job-no EQ "")

&SCOPED-DEFINE for-each2                           ~
    EACH rm-rdtlh                                  ~
    WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no      ~
      AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code ~
      AND rm-rdtlh.tag MATCHES fi_tag#             ~
    USE-INDEX rm-rdtl NO-LOCK

&SCOPED-DEFINE sortby-log                                                                                  ~
    IF lv-sort-by EQ "i-no"       THEN rm-rcpth.i-no                                                  ELSE ~
    IF lv-sort-by EQ "po-no"      THEN STRING(INT(rm-rcpth.po-no),"9999999999")                       ELSE ~
    IF lv-sort-by EQ "rita-code"  THEN rm-rcpth.rita-code                                             ELSE ~
    IF lv-sort-by EQ "loc"        THEN rm-rdtlh.loc                                                   ELSE ~
    IF lv-sort-by EQ "loc-bin"    THEN rm-rdtlh.loc-bin                                               ELSE ~
    IF lv-sort-by EQ "tag"        THEN rm-rdtlh.tag                                                   ELSE ~
    IF lv-sort-by EQ "tag2"       THEN rm-rdtlh.tag2                                                  ELSE ~
    IF lv-sort-by EQ "pur-uom"    THEN rm-rcpth.pur-uom                                               ELSE ~
    IF lv-sort-by EQ "qty"        THEN STRING(rm-rdtlh.qty,"-9999999999.99")                          ELSE ~
    IF lv-sort-by EQ "cost"       THEN STRING(rm-rdtlh.cost,"-9999999999.99")                         ELSE ~
    IF lv-sort-by EQ "ld-ext-cost"       THEN STRING((rm-rdtlh.qty * rm-rdtlh.cost),"-9999999999.99") ELSE ~
    IF lv-sort-by EQ "job-no"     THEN STRING(rm-rcpth.job-no,"x(6)") + STRING(rm-rcpth.job-no2,"99") ELSE ~
                                       STRING(YEAR(rm-rcpth.trans-date),"9999") + STRING(MONTH(rm-rcpth.trans-date),"99") + STRING(DAY(rm-rcpth.trans-date),"99") + STRING(rm-rcpth.r-no,"9999999999")

&SCOPED-DEFINE sortby BY rm-rcpth.i-no BY rm-rcpth.job-no BY rm-rcpth.job-no2

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
&Scoped-define INTERNAL-TABLES rm-rcpth rm-rdtlh

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rcpth.i-no rm-rcpth.po-no ~
rm-rcpth.job-no rm-rcpth.job-no2 rm-rdtlh.s-num rm-rcpth.trans-date ~
rm-rcpth.rita-code rm-rdtlh.loc rm-rdtlh.loc-bin rm-rdtlh.tag rm-rdtlh.qty ~
rm-rcpth.pur-uom rm-rdtlh.cost disp-uom () @ rm-rcpth.loc rm-rcpth.loc ~
disp-uom () @ rm-rcpth.loc rm-rdtlh.qty * rm-rdtlh.cost @ ld-ext-cost ~
rm-rdtlh.tag2 rm-rdtlh.user-id rm-rdtlh.receiver-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rcpth.i-no ~
rm-rcpth.po-no rm-rcpth.job-no rm-rcpth.job-no2 rm-rdtlh.s-num ~
rm-rcpth.trans-date rm-rcpth.rita-code rm-rdtlh.loc rm-rdtlh.loc-bin ~
rm-rdtlh.tag rm-rdtlh.qty rm-rcpth.pur-uom rm-rdtlh.cost rm-rdtlh.tag2 ~
rm-rdtlh.user-id rm-rdtlh.receiver-no 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rcpth rm-rdtlh
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rcpth
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rdtlh
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rcpth WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH rm-rdtlh OF rm-rcpth NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rcpth WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH rm-rdtlh OF rm-rcpth NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-rcpth rm-rdtlh
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-rcpth
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table rm-rdtlh


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table btCopy btDelete fi_rm-i-no ~
fi_tag# fi_job-no fi_job-no2 fi_rita-code fi_date btn_go btn_show fi_po-no ~
RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_rm-i-no fi_tag# fi_job-no fi_job-no2 ~
fi_rita-code fi_date fi_sort-by fi_name fi_q-onh fi_q-ton fi_q-lf fi_q-msf ~
fi_po-no FI_moveCol

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi_tag# fi_po-no 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD disp-uom B-table-Win 
FUNCTION disp-uom RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btCopy 
     LABEL "Copy" 
     SIZE 12 BY 1.19.

DEFINE BUTTON btDelete 
     LABEL "Delete" 
     SIZE 12 BY 1.19.

DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_show 
     LABEL "&Show All" 
     SIZE 12 BY 1.

DEFINE VARIABLE fi_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/00 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_name AS CHARACTER FORMAT "x(30)" 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Vendor PO#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_q-lf AS INTEGER FORMAT "->>>,>>>,>>9.9<<<<" INITIAL 0 
     LABEL "LF" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_q-msf AS INTEGER FORMAT "->>>,>>>,>>9.9<<<<" INITIAL 0 
     LABEL "MSF" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_q-onh AS INTEGER FORMAT "->>>,>>>,>>9.9<<<<" INITIAL 0 
     LABEL "OH:  EA" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_q-ton AS INTEGER FORMAT "->>>,>>>,>>9.9<<<<" INITIAL 0 
     LABEL "TON" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_rita-code AS CHARACTER FORMAT "X":U 
     LABEL "Trans Code" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_rm-i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fi_tag# AS CHARACTER FORMAT "X(20)":U 
     LABEL "Tag#" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 4.76.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      rm-rcpth
    FIELDS(rm-rcpth.i-no
      rm-rcpth.po-no
      rm-rcpth.job-no
      rm-rcpth.job-no2
      rm-rcpth.trans-date
      rm-rcpth.rita-code
      rm-rcpth.pur-uom
      rm-rcpth.loc
      rm-rcpth.loc
      rm-rcpth.loc), 
      rm-rdtlh SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      rm-rcpth.i-no COLUMN-LABEL "Item#" FORMAT "x(10)":U LABEL-BGCOLOR 14
      rm-rcpth.po-no COLUMN-LABEL "Vendor PO#" FORMAT "x(9)":U
            WIDTH 14 LABEL-BGCOLOR 14
      rm-rcpth.job-no FORMAT "x(6)":U LABEL-BGCOLOR 14
      rm-rcpth.job-no2 COLUMN-LABEL "" FORMAT "99":U LABEL-BGCOLOR 14
      rm-rdtlh.s-num COLUMN-LABEL "S" FORMAT ">9":U WIDTH 3 LABEL-BGCOLOR 14
      rm-rcpth.trans-date COLUMN-LABEL "TR Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      rm-rcpth.rita-code COLUMN-LABEL "C" FORMAT "x(1)":U LABEL-BGCOLOR 14
      rm-rdtlh.loc COLUMN-LABEL "Whs" FORMAT "x(5)":U LABEL-BGCOLOR 14
      rm-rdtlh.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U LABEL-BGCOLOR 14
      rm-rdtlh.tag COLUMN-LABEL "Tag" FORMAT "x(20)":U LABEL-BGCOLOR 14
      rm-rdtlh.qty COLUMN-LABEL "Qty" FORMAT "->>>>,>>9.9<<<<<":U
            LABEL-BGCOLOR 14
      rm-rcpth.pur-uom COLUMN-LABEL "Qty/UOM" FORMAT "x(8)":U WIDTH 10.2
            LABEL-BGCOLOR 14
      rm-rdtlh.cost COLUMN-LABEL "Cost" FORMAT "->>>,>>9.99<<<<":U
            LABEL-BGCOLOR 14
      disp-uom () @ rm-rcpth.loc
      rm-rcpth.loc COLUMN-LABEL "Cost/UOM" FORMAT "x(3)":U
      disp-uom () @ rm-rcpth.loc
      rm-rdtlh.qty * rm-rdtlh.cost @ ld-ext-cost COLUMN-LABEL "Ext Cost" FORMAT "->>,>>>,>>9.99":U
            LABEL-BGCOLOR 14
      rm-rdtlh.tag2 COLUMN-LABEL "Cert/Lot/Mill#" FORMAT "x(30)":U
            WIDTH 40 LABEL-BGCOLOR 14
      rm-rdtlh.user-id COLUMN-LABEL "UserID" FORMAT "x(8)":U WIDTH 12
      rm-rdtlh.receiver-no COLUMN-LABEL "Invoice Link" FORMAT "x(20)":U
  ENABLE
      rm-rcpth.i-no
      rm-rcpth.po-no
      rm-rcpth.job-no
      rm-rcpth.job-no2
      rm-rdtlh.s-num
      rm-rcpth.trans-date
      rm-rcpth.rita-code
      rm-rdtlh.loc
      rm-rdtlh.loc-bin
      rm-rdtlh.tag
      rm-rdtlh.qty
      rm-rcpth.pur-uom
      rm-rdtlh.cost
      rm-rdtlh.tag2
      rm-rdtlh.user-id
      rm-rdtlh.receiver-no
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 15
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 5.76 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     btCopy AT ROW 2.91 COL 92 WIDGET-ID 2
     btDelete AT ROW 2.91 COL 106 WIDGET-ID 4
     fi_rm-i-no AT ROW 1.48 COL 10.6 COLON-ALIGNED
     fi_tag# AT ROW 1.48 COL 39.6 COLON-ALIGNED
     fi_job-no AT ROW 1.48 COL 85.2 COLON-ALIGNED
     fi_job-no2 AT ROW 1.48 COL 96.2 COLON-ALIGNED
     fi_rita-code AT ROW 1.48 COL 112.6 COLON-ALIGNED
     fi_date AT ROW 1.48 COL 130 COLON-ALIGNED
     btn_go AT ROW 2.91 COL 2
     btn_show AT ROW 2.91 COL 15
     fi_sort-by AT ROW 2.91 COL 36.40 COLON-ALIGNED
     FI_moveCol AT ROW 2.91 COL 80.4 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi_name AT ROW 4.33 COL 1.6 HELP
          "Enter Finished Goods Name used for Alpha Numeric Searches."
     fi_q-onh AT ROW 4.33 COL 70 COLON-ALIGNED
     fi_q-ton AT ROW 4.33 COL 91 COLON-ALIGNED
     fi_q-lf AT ROW 4.33 COL 110 COLON-ALIGNED
     fi_q-msf AT ROW 4.33 COL 131 COLON-ALIGNED
     fi_po-no AT ROW 2.91 COL 130 COLON-ALIGNED
     "BrwsCol. Mode:" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 3.10 COL 64.60 WIDGET-ID 6
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
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2.

/* SETTINGS FOR FILL-IN fi_name IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi_po-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_q-lf IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_q-msf IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_q-onh IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_q-ton IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_tag# IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.rm-rcpth,ASI.rm-rdtlh OF ASI.rm-rcpth"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _FldNameList[1]   > ASI.rm-rcpth.i-no
"rm-rcpth.i-no" "Item#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.rm-rcpth.po-no
"rm-rcpth.po-no" "Vendor PO#" ? "character" ? ? ? 14 ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.rm-rcpth.job-no
"rm-rcpth.job-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.rm-rcpth.job-no2
"rm-rcpth.job-no2" "" "99" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.rm-rdtlh.s-num
"rm-rdtlh.s-num" "S" ? "integer" ? ? ? 14 ? ? yes ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.rm-rcpth.trans-date
"rm-rcpth.trans-date" "TR Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.rm-rcpth.rita-code
"rm-rcpth.rita-code" "C" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.rm-rdtlh.loc
"rm-rdtlh.loc" "Whs" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.rm-rdtlh.loc-bin
"rm-rdtlh.loc-bin" "Bin" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.rm-rdtlh.tag
"rm-rdtlh.tag" "Tag" "x(20)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.rm-rdtlh.qty
"rm-rdtlh.qty" "Qty" ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.rm-rcpth.pur-uom
"rm-rcpth.pur-uom" "Qty/UOM" "x(8)" "character" ? ? ? 14 ? ? yes ? no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.rm-rdtlh.cost
"rm-rdtlh.cost" "Cost" ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"disp-uom () @ rm-rcpth.loc" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.rm-rcpth.loc
"rm-rcpth.loc" "Cost/UOM" "x(3)" "character" ? ? ? 14 ? ? no "" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"disp-uom () @ rm-rcpth.loc" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"rm-rdtlh.qty * rm-rdtlh.cost @ ld-ext-cost" "Ext Cost" "->>,>>>,>>9.99" ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.rm-rdtlh.tag2
"rm-rdtlh.tag2" "Cert/Lot/Mill#" "x(30)" "character" ? ? ? 14 ? ? yes ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.rm-rdtlh.user-id
"rm-rdtlh.user-id" "UserID" ? "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.rm-rdtlh.receiver-no
"rm-rdtlh.receiver-no" "Invoice Link" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
    DEFINE VARIABLE ll       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
 
  IF USERID("nosweat") EQ "asi" OR AVAIL users AND users.securityLevel GT 899 THEN DO:
     
    RUN rminq/d-rmiinq.w (ROWID(rm-rcpth),ROWID(rm-rdtlh), "update", OUTPUT lv-rowid) .
    
    RUN repo-query (ROWID(rm-rcpth)).
    
  END.
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

  IF lv-column-nam BEGINS "job-no" THEN
    ASSIGN
     lv-column-nam = "job-no"
     lv-column-lab = "Job#".
  IF lv-column-lab = "Cost/UOM" THEN
      ASSIGN
     lv-column-nam = "pur-UOM"
     lv-column-lab = "Cost/UOM".

 /**/

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

  IF AVAIL rm-rcpth THEN RUN display-item (ROWID(rm-rcpth)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rcpth.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rcpth.i-no IN BROWSE Browser-Table /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME rm-rdtlh.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rdtlh.tag IN BROWSE Browser-Table /* Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
      
    RUN valid-tag-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rcpth.i-no IN BROWSE Browser-Table /* Item# */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rcpth.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rcpth.po-no IN BROWSE Browser-Table /* Vendor PO# */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rcpth.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rcpth.job-no IN BROWSE Browser-Table /* Job# */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rcpth.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rcpth.job-no2 IN BROWSE Browser-Table
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rdtlh.s-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.s-num Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rdtlh.s-num IN BROWSE Browser-Table /* S */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rcpth.trans-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.trans-date Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rcpth.trans-date IN BROWSE Browser-Table /* TR Date */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rcpth.rita-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.rita-code Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rcpth.rita-code IN BROWSE Browser-Table /* C */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rdtlh.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rdtlh.loc IN BROWSE Browser-Table /* Whs */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rdtlh.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rdtlh.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rdtlh.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rdtlh.tag IN BROWSE Browser-Table /* Tag */
DO:
    RUN valid-tag-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rdtlh.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rdtlh.qty IN BROWSE Browser-Table /* Qty */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rcpth.pur-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rcpth.pur-uom IN BROWSE Browser-Table /* Qty/UOM */
DO:
  IF LASTKEY NE -1 THEN RUN convert-uoms.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rcpth.pur-uom IN BROWSE Browser-Table /* Qty/UOM */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rdtlh.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.cost Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rdtlh.cost IN BROWSE Browser-Table /* Cost */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rcpth.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rcpth.loc IN BROWSE Browser-Table /* Cost/UOM */
DO:
  IF LASTKEY NE -1 THEN RUN convert-uoms.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rcpth.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rcpth.loc IN BROWSE Browser-Table /* Cost/UOM */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rdtlh.tag2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.tag2 Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rdtlh.tag2 IN BROWSE Browser-Table /* Cert/Lot/Mill# */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rdtlh.user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.user-id Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rdtlh.user-id IN BROWSE Browser-Table /* UserID */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rdtlh.receiver-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtlh.receiver-no Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF rm-rdtlh.receiver-no IN BROWSE Browser-Table /* Invoice Link */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCopy B-table-Win
ON CHOOSE OF btCopy IN FRAME F-Main /* Copy */
DO:
    DEF VAR confirm AS LOG NO-UNDO.
    DEF VAR v-rcpth-no LIKE rm-rctd.r-no NO-UNDO.
    DEF BUFFER b-rm-rcpth FOR rm-rcpth.
    DEF BUFFER b-rm-rdtlh FOR rm-rdtlh.
    DEF BUFFER b-reftable FOR reftable.
        MESSAGE "Do you Wish To Copy This Transaction Record?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE confirm.

      IF confirm THEN
      DO:

        RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT v-rcpth-no) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
          MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.

         CREATE b-rm-rcpth.
         FIND CURRENT rm-rcpth EXCLUSIVE-LOCK NO-ERROR.
         BUFFER-COPY rm-rcpth EXCEPT r-no TO b-rm-rcpth 
             ASSIGN b-rm-rcpth.r-no = v-rcpth-no.
         RELEASE b-rm-rcpth.
         FIND CURRENT rm-rcpth NO-LOCK NO-ERROR.

         IF AVAILABLE rm-rdtlh THEN
         DO:
            FIND CURRENT rm-rdtlh EXCLUSIVE-LOCK NO-ERROR.
            CREATE b-rm-rdtlh.
            BUFFER-COPY rm-rdtlh EXCEPT r-no TO b-rm-rdtlh
                ASSIGN b-rm-rdtlh.r-no = v-rcpth-no.
            RELEASE b-rm-rdtlh.
            FIND CURRENT rm-rdtlh NO-LOCK NO-ERROR.
         END.

         IF AVAIL reftable THEN
         DO:
            FIND CURRENT reftable EXCLUSIVE-LOCK NO-ERROR.
            CREATE b-reftable.
            BUFFER-COPY reftable EXCEPT loc TO b-reftable
               ASSIGN b-reftable.loc = STRING(v-rcpth-no,"9999999999").
            RELEASE b-reftable.
            FIND CURRENT reftable NO-LOCK NO-ERROR.
         END.

         RUN local-open-query.
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete B-table-Win
ON CHOOSE OF btDelete IN FRAME F-Main /* Delete */
DO:
   DEF VAR confirm AS LOG NO-UNDO.

   IF AVAIL rm-rcpth THEN
   DO:
      MESSAGE "Do you Wish To Delete This Transaction Record?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE confirm.

      IF confirm THEN
      DO:
         FIND CURRENT rm-rcpth EXCLUSIVE-LOCK NO-ERROR.
         DELETE rm-rcpth.
         RUN local-open-query.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  SESSION:SET-WAIT-STATE("general").

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_rm-i-no
     fi_tag#
     fi_job-no
     fi_job-no2
     fi_rita-code
     fi_date
     fi_po-no.

    {rminq/j-rmiinq.i}

    RUN dispatch ("row-changed").
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

    IF fi_rm-i-no:SENSITIVE THEN
       fi_rm-i-no:SCREEN-VALUE   = "".

    ASSIGN
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


&Scoped-define SELF-NAME fi_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON LEAVE OF fi_job-no IN FRAME F-Main /* Job# */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN fi_job-no:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM(fi_job-no:SCREEN-VALUE)))
                                  + TRIM(fi_job-no:SCREEN-VALUE).
  END.
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
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
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
ON VALUE-CHANGED OF fi_rita-code IN FRAME F-Main /* Trans Code */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1 + IF LASTKEY EQ 32 THEN 1 ELSE 0. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_rm-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_rm-i-no B-table-Win
ON LEAVE OF fi_rm-i-no IN FRAME F-Main /* RM Item# */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_tag#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_tag# B-table-Win
ON HELP OF fi_tag# IN FRAME F-Main /* Tag# */
DO:
  DEF VAR lv-char-val AS CHAR NO-UNDO.

  RUN windows/l-fgtag.w (cocode,fi_rm-i-no:SCREEN-VALUE,'',OUTPUT lv-char-val).
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

&SCOPED-DEFINE cellColumnDat b-rmiinq

{methods/browsers/setCellColumns.i}    
SESSION:DATA-ENTRY-RETURN = YES.



&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).
&ENDIF

DO WITH FRAME {&FRAME-NAME}:
    fi_date:SCREEN-VALUE = STRING(TODAY - 180) .
    fi_date = TODAY - 180 .
  END.

{methods/winReSize.i}
/*
&SCOPED-DEFINE cellColumnDat browsers-hist

{methods/browsers/setCellColumns.i} */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convert-uoms B-table-Win 
PROCEDURE convert-uoms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-len         LIKE po-ordl.s-len      NO-UNDO.
  DEF VAR v-wid         LIKE po-ordl.s-len      NO-UNDO.
  DEF VAR v-dep         LIKE po-ordl.s-len      NO-UNDO. 
  DEF VAR v-bwt         LIKE po-ordl.s-len      NO-UNDO.
  DEF VAR lv-out-qty    LIKE rm-rdtlh.qty       NO-UNDO.
  DEF VAR lv-out-cost   LIKE rm-rdtlh.cost      NO-UNDO.
  DEF VAR lv-qty-uom    LIKE rm-rcpth.pur-uom   NO-UNDO.
  DEF VAR lv-cost-uom   LIKE rm-rcpth.pur-uom   NO-UNDO.
  DEF VAR lv-po-no      LIKE po-ordl.po-no      NO-UNDO.

  DEF BUFFER b-rcpth FOR rm-rcpth.
  DEF BUFFER b-rdtlh FOR rm-rdtlh.


  DO WITH FRAME {&FRAME-NAME}:
    FIND b-rcpth WHERE ROWID(b-rcpth) EQ ROWID(rm-rcpth) NO-LOCK NO-ERROR.
    FIND b-rdtlh WHERE ROWID(b-rdtlh) EQ ROWID(rm-rdtlh) NO-LOCK NO-ERROR.

    FIND FIRST item
        WHERE item.company EQ rm-rcpth.company
          AND item.i-no    EQ rm-rcpth.i-no
        NO-ERROR.
    IF AVAIL item THEN DO:
      IF item.cons-uom EQ "" THEN item.cons-uom = b-rcpth.pur-uom.

      ASSIGN
       lv-qty-uom  = item.cons-uom
       lv-cost-uom = item.cons-uom
       v-dep       = item.s-dep
       lv-po-no    = INT(b-rcpth.po-no) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN lv-po-no = 0.

      RELEASE po-ordl.
      IF lv-po-no NE 0 THEN
      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ b-rcpth.company
            AND po-ordl.po-no     EQ lv-po-no
            AND po-ordl.i-no      EQ b-rcpth.i-no
            AND po-ordl.job-no    EQ b-rcpth.job-no
            AND po-ordl.job-no2   EQ b-rcpth.job-no2
            AND po-ordl.item-type EQ YES 
            AND po-ordl.s-num     EQ b-rdtlh.s-num
            AND po-ordl.b-num     EQ b-rdtlh.b-num
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN DO:
        ASSIGN
         v-len = po-ordl.s-len
         v-wid = po-ordl.s-wid
         v-bwt = 0.
        {rm/pol-dims.i}
      END.

      FIND FIRST job
          WHERE job.company EQ b-rcpth.company
            AND job.job-no  EQ b-rcpth.job-no
            AND job.job-no2 EQ b-rcpth.job-no2
          NO-LOCK NO-ERROR.
      IF AVAIL job THEN DO:
        FIND FIRST job-mat
            WHERE job-mat.company  EQ b-rcpth.company
              AND job-mat.job      EQ job.job
              AND job-mat.i-no     EQ b-rcpth.i-no
              AND job-mat.frm      EQ b-rdtlh.s-num
              AND job-mat.blank-no EQ b-rdtlh.b-num
            NO-LOCK NO-ERROR.
        IF AVAIL job-mat THEN
          ASSIGN
           v-len         = job-mat.len
           v-wid         = job-mat.wid
           v-bwt         = job-mat.basis-w.
      END.
      IF v-len EQ 0 THEN v-len = IF AVAIL item THEN item.s-len ELSE 0.
      IF v-wid EQ 0 THEN v-wid = IF AVAIL item AND item.r-wid NE 0 THEN item.r-wid
                                   ELSE
                                   IF AVAIL item THEN item.s-wid ELSE 0.
      IF v-bwt EQ 0 THEN v-bwt = IF AVAIL item THEN item.basis-w ELSE 0.
    END.
    FIND CURRENT item NO-LOCK NO-ERROR.

    /* convert qty */
    IF rm-rcpth.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name} EQ lv-qty-uom THEN
      lv-out-qty = DEC(rm-rdtlh.qty:SCREEN-VALUE IN BROWSE {&browse-name}).
    ELSE
      RUN custom/convquom.p(b-rcpth.company,
                            rm-rcpth.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name} ,
                            lv-qty-uom,
                            v-bwt, v-len, v-wid, v-dep,
                            DEC(rm-rdtlh.qty:SCREEN-VALUE IN BROWSE {&browse-name}),
                            OUTPUT lv-out-qty).

    /* convert cost */
    IF rm-rcpth.loc:SCREEN-VALUE IN BROWSE {&browse-name} EQ "L" THEN
      lv-out-cost = DEC(rm-rdtlh.cost:SCREEN-VALUE IN BROWSE {&browse-name}) / lv-out-qty.
    ELSE
      RUN custom/convcuom.p(b-rcpth.company,
                            rm-rcpth.loc:SCREEN-VALUE IN BROWSE {&browse-name},
                            lv-cost-uom,
                            v-bwt, v-len, v-wid, v-dep,
                            DEC(rm-rdtlh.cost:SCREEN-VALUE IN BROWSE {&browse-name}),
                            OUTPUT lv-out-cost).
.
    ASSIGN
     rm-rdtlh.qty:SCREEN-VALUE IN BROWSE {&browse-name}     = STRING(lv-out-qty)
     rm-rcpth.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name} = lv-qty-uom
     rm-rdtlh.cost:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(lv-out-cost)
     rm-rcpth.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = lv-cost-uom.

    DISPLAY DEC(rm-rdtlh.qty:SCREEN-VALUE IN BROWSE {&browse-name}) *
            DEC(rm-rdtlh.cost:SCREEN-VALUE IN BROWSE {&browse-name})
                @ ld-ext-cost WITH BROWSE {&browse-name}.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item B-table-Win 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  DEF VAR v-onh           LIKE item.q-onh EXTENT 4       NO-UNDO.
  DEF VAR v-uom           LIKE item.cons-uom EXTENT 4    NO-UNDO
                          INIT ["EA","TON","LF","MSF"].


  FIND rm-rcpth WHERE ROWID(rm-rcpth) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL rm-rcpth THEN
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST item
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rcpth.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL item THEN DO:
      DO i = 1 TO 4:
        IF ITEM.cons-uom EQ v-uom[i] THEN
          v-onh[i] = item.q-onh.
        ELSE
          RUN sys/ref/convquom.p(item.cons-uom, v-uom[i], item.basis-w,
                                 (if item.r-wid eq 0 THEN item.s-len
                                                     ELSE 12),
                                 (if item.r-wid eq 0 THEN item.s-wid
                                                     ELSE item.r-wid),
                                 item.s-dep,                    
                                 item.q-onh, OUTPUT v-onh[i]).
      END.

      ASSIGN
       fi_name:SCREEN-VALUE    = item.i-name
       fi_q-onh:SCREEN-VALUE   = STRING(v-onh[1])
       fi_q-ton:SCREEN-VALUE   = STRING(v-onh[2])
       fi_q-lf:SCREEN-VALUE    = STRING(v-onh[3])
       fi_q-msf:SCREEN-VALUE   = STRING(v-onh[4]).
    END.
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
  IF AVAIL rm-rcpth THEN RUN display-item (ROWID(rm-rcpth)).

  DO WITH FRAME {&FRAME-NAME}:
    fi_sort-by:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
                              TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".
  END.

  FIND FIRST users NO-LOCK WHERE 
      users.user_id EQ USERID(LDBNAME(1)) 
      NO-ERROR.
  
  IF AVAIL users AND users.securityLevel LT 900 THEN
     ASSIGN btCopy:HIDDEN = YES
            btCopy:SENSITIVE = NO
            btDelete:HIDDEN = YES
            btDelete:SENSITIVE = NO.
  ELSE
      ASSIGN btCopy:HIDDEN = NO
            btCopy:SENSITIVE = YES
            btDelete:HIDDEN = NO
            btDelete:SENSITIVE = YES.

  IF v-called-setCellColumns = NO THEN DO:
     RUN setCellColumns.
     v-called-setCellColumns = YES.
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

  DISPLAY fi_date WITH FRAME {&FRAME-NAME}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   rm-rcpth.i-no:READ-ONLY IN BROWSE {&browse-name}       = YES
   rm-rcpth.po-no:READ-ONLY IN BROWSE {&browse-name}      = YES
   rm-rcpth.job-no:READ-ONLY IN BROWSE {&browse-name}     = YES
   rm-rcpth.job-no2:READ-ONLY IN BROWSE {&browse-name}    = YES
   rm-rcpth.trans-date:READ-ONLY IN BROWSE {&browse-name} = YES
   rm-rcpth.rita-code:READ-ONLY IN BROWSE {&browse-name}  = YES
   rm-rdtlh.loc:READ-ONLY IN BROWSE {&browse-name}        = YES
   rm-rdtlh.loc-bin:READ-ONLY IN BROWSE {&browse-name}    = YES
   rm-rdtlh.tag:READ-ONLY IN BROWSE {&browse-name}        = YES
   rm-rdtlh.tag2:READ-ONLY IN BROWSE {&browse-name}       = YES.

  RUN set-read-only (YES).

  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  {methods/winReSizeLocInit.i}

  RUN set-focus.

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
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lv-i-no LIKE item.i-no NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .*/

  /* Code placed here will execute AFTER standard behavior.    */

  IF ll-first THEN DO:
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"history-source", OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO WITH FRAME {&FRAME-NAME}:
      RUN get-i-no IN WIDGET-HANDLE(char-hdl) (OUTPUT fi_rm-i-no).
      DISPLAY fi_rm-i-no.
      DISABLE fi_rm-i-no.
      ll-first = NO.
    END.

    ELSE 
      OPEN QUERY Browser-Table FOR EACH rm-rcpth WHERE rm-rcpth.company EQ cocode AND rm-rcpth.i-no EQ "zzzzzzzzzzzzzzzzzzzzzzzzz" NO-LOCK, ~
                                   EACH rm-rdtlh WHERE TRUE /* Join to rm-rcpth incomplete */ ~
                                                   AND rm-rdtlh.r-no eq rm-rcpth.r-no and ~
                                                       rm-rdtlh.rita-code eq rm-rcpth.rita-code NO-LOCK.
  END.

  IF NOT ll-first THEN DO:
    {rminq/j-rmiinq.i}
  END.

  RUN dispatch ("display-fields").

  RUN dispatch ("row-changed").

  ll-first = NO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-item B-table-Win 
PROCEDURE new-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN set-defaults.

  RUN dispatch ("open-query").

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
  {src/adm/template/snd-list.i "rm-rcpth"}
  {src/adm/template/snd-list.i "rm-rdtlh"}

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
     fi_rm-i-no   = ""
     fi_job-no    = ""
     fi_job-no2   = 0
     fi_rita-code = ""
     fi_date      = 01/01/0001
     fi_tag#      = ""
     fi_po-no     = 0
     ll-first     = yes. /* DD 05/01/2007 TASK 0501-0709 */

    DISPLAY
     fi_rm-i-no
     fi_job-no
     fi_job-no2
     fi_rita-code
     fi_date
     fi_tag#
     fi_po-no.
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
    APPLY "entry" TO fi_rm-i-no.
  END.

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
     rm-rcpth.i-no:READ-ONLY IN BROWSE {&browse-name}       = ip-log
     rm-rcpth.po-no:READ-ONLY IN BROWSE {&browse-name}      = ip-log
     rm-rcpth.job-no:READ-ONLY IN BROWSE {&browse-name}     = ip-log
     rm-rcpth.job-no2:READ-ONLY IN BROWSE {&browse-name}    = ip-log
     rm-rdtlh.s-num:READ-ONLY IN BROWSE {&browse-name}      = ip-log
     rm-rcpth.rita-code:READ-ONLY IN BROWSE {&browse-name}  = ip-log
     rm-rcpth.trans-date:READ-ONLY IN BROWSE {&browse-name} = ip-log
     rm-rdtlh.loc:READ-ONLY IN BROWSE {&browse-name}        = ip-log
     rm-rdtlh.loc-bin:READ-ONLY IN BROWSE {&browse-name}    = ip-log
     rm-rdtlh.tag:READ-ONLY IN BROWSE {&browse-name}        = ip-log
     rm-rdtlh.qty:READ-ONLY IN BROWSE {&browse-name}        = ip-log
     rm-rcpth.pur-uom:READ-ONLY IN BROWSE {&browse-name}    = ip-log
     rm-rdtlh.cost:READ-ONLY IN BROWSE {&browse-name}       = ip-log
     /*rm-rcpth.loc:READ-ONLY IN BROWSE {&browse-name}        = ip-log*/
     rm-rdtlh.tag2:READ-ONLY IN BROWSE {&browse-name}       = ip-log
     rm-rdtlh.receiver-no:READ-ONLY IN BROWSE {&browse-name} = ip-log
     rm-rdtlh.user-id:READ-ONLY IN BROWSE {&browse-name}    = ip-log.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-mat-act-cost B-table-Win 
PROCEDURE update-mat-act-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE PARAMETER BUFFER b-rm-rcpth FOR rm-rcpth.
   DEFINE PARAMETER BUFFER b-rm-rdtlh FOR rm-rdtlh.

   DEF VAR v-bwt       LIKE item.basis-w  NO-UNDO.
   DEF VAR v-len       LIKE item.s-len    NO-UNDO.
   DEF VAR v-wid       LIKE item.s-wid    NO-UNDO.
   DEF VAR v-dep       LIKE item.s-dep    NO-UNDO.
   DEF VAR v-cost      LIKE rm-rdtlh.cost NO-UNDO.

   FIND FIRST job WHERE
        job.company EQ b-rm-rcpth.company AND
        job.job-no  EQ b-rm-rcpth.job-no AND
        job.job-no2 EQ b-rm-rcpth.job-no2
        NO-LOCK NO-ERROR.

   FIND FIRST item WHERE
        item.company  EQ b-rm-rcpth.company AND
        item.i-no     EQ b-rm-rcpth.i-no
        NO-LOCK NO-ERROR.

   IF AVAIL job AND AVAIL ITEM THEN
   DO:
      FOR EACH job-mat WHERE
          job-mat.company  EQ job.company AND
          job-mat.job      EQ job.job AND
          job-mat.job-no   EQ job.job-no AND
          job-mat.job-no2  EQ job.job-no2 AND
          job-mat.frm      EQ b-rm-rdtlh.s-num AND
          job-mat.i-no     EQ b-rm-rcpth.i-no
          NO-LOCK
          BREAK BY job-mat.blank-no DESC:

         IF LAST(job-mat.blank-no) OR
            job-mat.blank-no EQ b-rm-rdtlh.b-num THEN
            LEAVE.
      END.

      IF AVAIL job-mat THEN DO:
         ASSIGN
           v-bwt = job-mat.basis-w
           v-len = job-mat.len
           v-wid = job-mat.wid
           v-dep = item.s-dep.

         IF v-len EQ 0 THEN v-len = item.s-len.

         IF v-wid EQ 0 THEN
           v-wid = IF item.r-wid NE 0 THEN item.r-wid ELSE item.s-wid.

         IF v-bwt EQ 0 THEN v-bwt = item.basis-w.

         IF item.cons-uom EQ b-rm-rcpth.pur-uom THEN
           v-cost = b-rm-rdtlh.cost.
         ELSE
           RUN sys/ref/convcuom.p(item.cons-uom, b-rm-rcpth.pur-uom,
                                  v-bwt, v-len, v-wid, v-dep,
                                  b-rm-rdtlh.cost, OUTPUT v-cost).    

         FIND FIRST mat-act
             WHERE mat-act.company   EQ job-mat.company
               AND mat-act.mat-date  EQ b-rm-rcpth.post-date
               AND mat-act.job       EQ job.job
               AND mat-act.job-no    EQ job-mat.job-no
               AND mat-act.job-no2   EQ job-mat.job-no2
               AND mat-act.s-num     EQ job-mat.frm
               AND mat-act.b-num     EQ job-mat.blank-no
               AND mat-act.i-no      EQ job-mat.i-no
               AND mat-act.rm-i-no   EQ job-mat.i-no
               AND mat-act.tag       EQ b-rm-rdtlh.tag
               AND mat-act.loc       EQ b-rm-rdtlh.loc
               AND mat-act.loc-bin   EQ b-rm-rdtlh.loc-bin
             NO-ERROR.

         IF AVAIL mat-act THEN DO:
            IF b-rm-rcpth.pur-uom NE job-mat.sc-uom THEN
               RUN sys/ref/convcuom.p(b-rm-rcpth.pur-uom, job-mat.sc-uom,
                                      v-bwt, v-len, v-wid, v-dep,
                                      v-cost, OUTPUT v-cost).

            ASSIGN
               mat-act.cost = v-cost
               mat-act.ext-cost = v-cost * mat-act.qty.

            RELEASE mat-act.
         END.
      END.
   END.

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
  DEF BUFFER b-rcpth FOR rm-rcpth.
  DEF BUFFER b-rdtlh FOR rm-rdtlh.

  DISABLE TRIGGERS FOR LOAD OF rm-rcpth.
  DISABLE TRIGGERS FOR LOAD OF rm-rdtlh.

  DO WITH FRAME {&FRAME-NAME}:
    FIND b-rcpth WHERE ROWID(b-rcpth) EQ ROWID(rm-rcpth).
    FIND b-rdtlh WHERE ROWID(b-rdtlh) EQ ROWID(rm-rdtlh).

    ASSIGN
     b-rcpth.i-no       = rm-rcpth.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rcpth.po-no      = rm-rcpth.po-no:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rcpth.job-no     = rm-rcpth.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rcpth.job-no2    = INT(rm-rcpth.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rcpth.rita-code  = rm-rcpth.rita-code:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rdtlh.s-num      = INT(rm-rdtlh.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rcpth.trans-date = DATE(rm-rcpth.trans-date:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rdtlh.loc        = rm-rdtlh.loc:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rdtlh.loc-bin    = rm-rdtlh.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rdtlh.tag        = rm-rdtlh.tag:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rdtlh.qty        = DEC(rm-rdtlh.qty:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rcpth.pur-uom    = rm-rcpth.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rdtlh.cost       = DEC(rm-rdtlh.cost:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rdtlh.tag2       = rm-rdtlh.tag2:SCREEN-VALUE IN BROWSE {&browse-name}
     /*b-rcpth.loc        = rm-rcpth.loc:SCREEN-VALUE IN BROWSE {&browse-name}*/
     b-rdtlh.user-id    = rm-rdtlh.user-id:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rdtlh.receiver-no = rm-rdtlh.receiver-no:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rdtlh.job-no     = b-rcpth.job-no
     b-rdtlh.job-no2    = b-rcpth.job-no2
     b-rdtlh.rita-code  = b-rcpth.rita-code
     b-rcpth.user-id    = b-rdtlh.user-id.

    IF b-rcpth.rita-code EQ "I" THEN
       RUN update-mat-act-cost(BUFFER b-rcpth,
                               BUFFER b-rdtlh).

    FIND b-rcpth WHERE ROWID(b-rcpth) EQ ROWID(rm-rcpth) NO-LOCK NO-ERROR.
    FIND b-rdtlh WHERE ROWID(b-rdtlh) EQ ROWID(rm-rdtlh) NO-LOCK NO-ERROR.

    RUN set-read-only (YES).

    RUN repo-query (ROWID(rm-rcpth)).
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
  DEF BUFFER b-item FOR item.


  DO WITH FRAME {&FRAME-NAME}:
    rm-rcpth.i-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(rm-rcpth.i-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF NOT CAN-FIND(FIRST b-item
                    WHERE b-item.company EQ cocode
                      AND b-item.i-no    EQ rm-rcpth.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE "Invalid RM Item#..." VIEW-AS ALERT-BOX ERROR.
      rm-rcpth.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = rm-rcpth.i-no.
      APPLY "entry" TO rm-rcpth.i-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag-no B-table-Win 
PROCEDURE valid-tag-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-tag LIKE rm-rdtlh.tag NO-UNDO.
  DEF BUFFER b-item FOR item.
  DEF BUFFER b-rm-rdtlh FOR rm-rdtlh.

  DO WITH FRAME {&FRAME-NAME}:
      lv-tag = rm-rdtlh.tag:SCREEN-VALUE IN BROWSE {&browse-name}.
      
   IF rm-rdtlh.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND
       rm-rcpth.rita-code:SCREEN-VALUE IN BROWSE {&browse-name} EQ "R" AND  
       int(rm-rdtlh.qty:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN do:

       FIND FIRST  b-rm-rdtlh NO-LOCK
           WHERE b-rm-rdtlh.company EQ cocode
           /*AND b-rm-rcpth.i-no EQ fi_rm-i-no*/
           AND b-rm-rdtlh.tag     EQ lv-tag 
           AND ROWID(b-rm-rdtlh)  NE ROWID(rm-rdtlh) NO-ERROR .

       IF AVAIL b-rm-rdtlh THEN DO:
           MESSAGE "This Tag Number has already been used..." VIEW-AS ALERT-BOX INFO.
           rm-rdtlh.tag:SCREEN-VALUE IN BROWSE {&browse-name} = rm-rdtlh.tag.
           APPLY "entry" TO rm-rdtlh.tag IN BROWSE {&browse-name}.
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
  DEF BUFFER b-rm-rdtlh FOR rm-rdtlh.

  DEF VAR lv-tag LIKE rm-rdtlh.tag NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    lv-tag = rm-rdtlh.tag:SCREEN-VALUE IN BROWSE {&browse-name}.

    IF lv-tag NE rm-rdtlh.tag AND
       (lv-tag EQ ""       OR
        (NOT CAN-FIND(FIRST loadtag
                      WHERE loadtag.company   EQ item.company
                        AND loadtag.item-type EQ YES
                        AND loadtag.i-no      EQ rm-rcpth.i-no:SCREEN-VALUE
                        AND loadtag.tag-no    EQ lv-tag)             AND
         NOT CAN-FIND(FIRST b-rm-rdtlh
                      WHERE b-rm-rdtlh.company EQ cocode
                        AND b-rm-rdtlh.tag     EQ lv-tag
                        AND ROWID(b-rm-rdtlh)  NE ROWID(rm-rdtlh)))) THEN DO:
      MESSAGE "Invalid Tag# Change..." VIEW-AS ALERT-BOX ERROR.
      rm-rdtlh.tag:SCREEN-VALUE IN BROWSE {&browse-name} = rm-rdtlh.tag.
      APPLY "entry" TO rm-rdtlh.tag IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION disp-uom B-table-Win 
FUNCTION disp-uom RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lv-uom LIKE rm-rcpth.pur-uom NO-UNDO.

  DEF BUFFER b-rcpth FOR rm-rcpth.


  DO WITH FRAME {&FRAME-NAME}:
    lv-uom = IF AVAIL rm-rcpth THEN rm-rcpth.pur-uom
             ELSE rm-rcpth.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}.
  END.

  RETURN lv-uom.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR first-item AS CHAR NO-UNDO.
DEF VAR last-item AS CHAR NO-UNDO.

GET FIRST Browser-Table .
IF AVAIL rm-rcpth  THEN
ASSIGN first-item = rm-rcpth.i-no .
GET LAST Browser-Table .
IF AVAIL rm-rcpth  THEN
ASSIGN last-item = rm-rcpth.i-no . 

RUN rminq/rmiinq-exp.w (first-item, last-item).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
