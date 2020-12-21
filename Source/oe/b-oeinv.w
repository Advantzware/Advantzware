&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  oe\b-oeinv.w

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
&SCOPED-DEFINE proc-init proc-init
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}

{sys/inc/var.i "new shared"}
{methods/template/brwcustomdef.i}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR ls-status AS cha NO-UNDO.


DEF VAR li-ord-no LIKE inv-line.ord-no NO-UNDO.
DEF VAR vcText    AS CHAR NO-UNDO INIT 'By:'.
DEF VAR li-cust-Po LIKE oe-ord.po-no NO-UNDO.

DEF SHARED VARIABLE vfWinOrigW      AS DECIMAL  NO-UNDO.
DEF SHARED VARIABLE vfWinOrigH      AS DECIMAL  NO-UNDO.

/* gdm - 11180901*/
DEF VAR v-sort-name  AS LOG NO-UNDO.
DEF VAR invcopys-cha AS CHAR NO-UNDO.

DEF BUFFER b-cust FOR cust.

{sys/inc/invcopys.i}
IF AVAIL sys-ctrl THEN
   invcopys-cha  = sys-ctrl.char-fld.
   
DEFINE VARIABLE dMargin  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lFirst   AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE cFreight AS CHARACTER NO-UNDO.

DEFINE VARIABLE cSortBy  AS CHARACTER NO-UNDO INITIAL "inv-no".
DEFINE VARIABLE lSortAsc AS LOGICAL   NO-UNDO.

&SCOPED-DEFINE for-each1                             ~
    FOR EACH inv-head NO-LOCK                        ~
        WHERE inv-head.company   EQ     cocode       ~
          AND inv-head.cust-no   BEGINS fiCustNumber ~
          AND inv-head.cust-name BEGINS fiCustName   ~
          AND (inv-head.inv-no   EQ     fiInvoice  OR fiInvoice EQ 0) ~
          AND (inv-head.bol-no   EQ     fiBol      OR fiBol EQ 0)     ~
          AND (IF cbStatus = "A" THEN YES ELSE IF cbStatus = '' THEN inv-head.stat = cbStatus or inv-head.stat = "X" ELSE IF cbStatus = "H" THEN inv-head.stat = "H" ELSE inv-head.stat = "W")~
          AND (IF cbPrinted = "All" THEN YES ELSE inv-head.printed = LOGICAL(cbPrinted)) ~
          and (if cbAutoApproved = "All" then YES ELSE inv-head.autoApproved EQ LOGICAL(cbAutoApproved)) ~
          AND inv-head.multi-invoice = NO 
          
    &SCOPED-DEFINE for-each2                     ~
    FIRST cust NO-LOCK                    ~
          WHERE cust.company EQ cocode   ~
          AND cust.cust-no EQ inv-head.cust-no    ~
          AND (cust.accountant BEGINS fiBillOwner or fiBillOwner eq "")  
 
  &SCOPED-DEFINE sortby-log  BY ~
  IF cSortBy  EQ 'cust-no'       THEN inv-head.cust-no ELSE ~
  IF cSortBy  EQ 'cust-name'     THEN inv-head.cust-name ELSE ~
  IF cSortBy  EQ 'inv-date'      THEN STRING(YEAR(inv-head.inv-date),'9999') + ~
     STRING(MONTH(inv-head.inv-date),'99') + ~
     STRING(DAY(inv-head.inv-date),'99')  ELSE ~
  IF cSortBy  EQ 'bol-no'        THEN STRING(inv-head.bol-no,'>>>>>>>9') ELSE ~
  IF cSortBy  EQ 'printed'       THEN STRING(inv-head.printed,'Y/N') ELSE ~
  IF cSortBy  EQ 'ls-status'     THEN STRING(inv-head.stat) ELSE ~
  IF cSortBy  EQ 'li-ord-no'     THEN string(f-ordno()) ELSE ~
  IF cSortBy  EQ 't-inv-rev'     THEN STRING(inv-head.t-inv-rev,'->>,>>>,>>9.99') ELSE ~
  IF cSortBy  EQ 'r-no'          THEN STRING(inv-head.r-no,'>>>>>>>9') ELSE ~
  IF cSortBy  EQ 't-inv-tax'     THEN STRING (inv-head.t-inv-tax, '->>,>>>,>>9.99') ELSE ~
  IF cSortBy  EQ 't-inv-freight' THEN STRING(inv-head.t-inv-freight,'->>,>>>,>>9.99') ELSE ~
  IF cSortBy  EQ 'fob-code'      THEN inv-head.fob-code ELSE ~
  IF cSortBy  EQ 'multi-invoice' THEN STRING(inv-head.multi-invoice) ELSE ~
  IF cSortBy  EQ 'multi-inv-no'  THEN STRING(inv-head.multi-inv-no,'>>>>>9') ELSE ~
  IF cSortBy  EQ 'ediInvoice'    THEN STRING(inv-head.ediInvoice) ELSE ~
  IF cSortBy  EQ 'spare-char-5'  THEN STRING(inv-head.spare-char-5) ELSE ~
  IF cSortBy  EQ 'cFreight'      THEN inv-head.frt-pay ELSE ~
  IF cSortBy  EQ 'accountant'    THEN cust.accountant ELSE ~
  STRING(inv-head.inv-no,'>>>>>>9') ~  
  
&SCOPED-DEFINE sortby-phrase-asc ~
    {&sortby-log}                ~
    
&SCOPED-DEFINE sortby-phrase-desc ~
    {&sortby-log} DESCENDING      ~

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
&Scoped-define INTERNAL-TABLES inv-head cust

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table inv-head.inv-no ~
inv-head.cust-no inv-head.cust-name inv-head.inv-date inv-head.bol-no ~
f-ordno() @ li-ord-no inv-head.printed inv-head.t-inv-rev ~
getStatus() @ ls-status inv-head.autoApproved inv-head.r-no ~
inv-head.company f-cust-Po() @ li-cust-Po inv-head.t-inv-tax ~
inv-head.t-inv-freight inv-head.fob-code pGetFreightTerms() @ cFreight ~
inv-head.multi-invoice inv-head.multi-inv-no inv-head.ediInvoice ~
inv-head.spare-char-5 pGetMargin() @ dMargin cust.accountant
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH inv-head WHERE ~{&KEY-PHRASE} ~
      AND inv-head.company = cocode and ~
ASI.inv-head.multi-invoice = no NO-LOCK, ~
FIRST cust NO-LOCK                    ~
          WHERE cust.company EQ cocode   ~
          AND cust.cust-no EQ inv-head.cust-no    ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH inv-head WHERE ~{&KEY-PHRASE} ~
      AND inv-head.company = cocode and ~
ASI.inv-head.multi-invoice = no NO-LOCK, ~
FIRST cust NO-LOCK                    ~
          WHERE cust.company EQ cocode   ~
          AND cust.cust-no EQ inv-head.cust-no    ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table inv-head cust
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table inv-head
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table inv-head


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-7 fiCustNumber btGo btShow ~
fiInvoice fiBol cbPrinted fiBillOwner fiCustName cbStatus cbAutoApproved ~
Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS fiCustNumber fiInvoice fiBol cbPrinted ~
fiBillOwner fiCustName cbStatus cbAutoApproved 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-cust-Po B-table-Win 
FUNCTION f-cust-Po RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-ordno B-table-Win 
FUNCTION f-ordno RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStatus B-table-Win 
FUNCTION getStatus RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pGetFreightTerms B-table-Win 
FUNCTION pGetFreightTerms RETURNS CHARACTER PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pGetMargin B-table-Win 
FUNCTION pGetMargin RETURNS DECIMAL PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btGo 
     LABEL "Go" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE BUTTON btShow 
     LABEL "Show All" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cbAutoApproved AS CHARACTER FORMAT "X(256)" INITIAL "All" 
     LABEL "Auto Approved" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All","Yes","No" 
     DROP-DOWN-LIST
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE cbPrinted AS CHARACTER FORMAT "X(256)" INITIAL "All" 
     LABEL "Printed" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All","Yes","No" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbStatus AS CHARACTER FORMAT "X(256)":U INITIAL "A" 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "All","A",
                     "Released","",
                     "Hold","H",
                     "Wait/App","W"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE fiBillOwner AS CHARACTER FORMAT "X(8)":U 
     LABEL "Accountant" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiBol AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "BOL #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiCustName AS CHARACTER FORMAT "X(30)":U 
     LABEL "Customer Name" 
     VIEW-AS FILL-IN 
     SIZE 36.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiCustNumber AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiInvoice AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     LABEL "Invoice #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_AutoFindLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Auto Find:" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE fi_By AS CHARACTER FORMAT "X(256)":U INITIAL "By:" 
      VIEW-AS TEXT 
     SIZE 3.6 BY .62 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 73 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 2.86.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      inv-head,
      cust 
      FIELDS(cust.company
             cust.cust-no
             cust.accountant) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      inv-head.inv-no COLUMN-LABEL "Invoice#" FORMAT ">>>>>>9":U
            LABEL-BGCOLOR 14
      inv-head.cust-no COLUMN-LABEL "Cust #" FORMAT "x(8)":U WIDTH 10.6
            LABEL-BGCOLOR 14
      inv-head.cust-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      inv-head.inv-date COLUMN-LABEL "Inv Date" FORMAT "99/99/9999":U
            WIDTH 14.6 LABEL-BGCOLOR 14
      inv-head.bol-no COLUMN-LABEL "BOL #" FORMAT ">>>>>>>9":U
            LABEL-BGCOLOR 14
      f-ordno() @ li-ord-no COLUMN-LABEL "Order#" LABEL-BGCOLOR 14
      inv-head.printed FORMAT "Yes/No":U WIDTH 9.6 LABEL-BGCOLOR 14
      inv-head.t-inv-rev COLUMN-LABEL "Invoice Amt" FORMAT "->>,>>>,>>9.99":U
            WIDTH 16.6 LABEL-BGCOLOR 14
      getStatus() @ ls-status COLUMN-LABEL "Status" FORMAT "x(8)":U
            WIDTH 11.6 LABEL-BGCOLOR 14
      inv-head.autoApproved COLUMN-LABEL "Auto" FORMAT "Yes/No":U
            WIDTH 9.6 LABEL-BGCOLOR 14
      inv-head.r-no FORMAT ">>>>>>>9":U LABEL-BGCOLOR 14
      inv-head.company FORMAT "x(3)":U
      f-cust-Po() @ li-cust-Po COLUMN-LABEL "PO#" LABEL-BGCOLOR 14
      inv-head.t-inv-tax COLUMN-LABEL "Tax Amt" FORMAT "->>,>>>,>>9.99":U
            WIDTH 14.2 LABEL-BGCOLOR 14
      inv-head.t-inv-freight COLUMN-LABEL "Freight Amt" FORMAT "->>,>>9.99":U
            WIDTH 15.6 LABEL-BGCOLOR 14
      inv-head.fob-code FORMAT "x(5)":U LABEL-BGCOLOR 14
      pGetFreightTerms() @ cFreight COLUMN-LABEL "Freight Terms" FORMAT "x(12)":U
            LABEL-BGCOLOR 14
      inv-head.multi-invoice COLUMN-LABEL "Grouped" FORMAT "yes/no":U
            LABEL-BGCOLOR 14
      inv-head.multi-inv-no COLUMN-LABEL "Group #" FORMAT ">>>>>9":U
            LABEL-BGCOLOR 14
      inv-head.ediInvoice FORMAT "yes/no":U LABEL-BGCOLOR 14
      inv-head.spare-char-5 COLUMN-LABEL "Comment" FORMAT "x(60)":U
            LABEL-BGCOLOR 14
      pGetMargin() @ dMargin COLUMN-LABEL "Margin%" LABEL-BGCOLOR 14
      cust.accountant COLUMN-LABEL "Accountant" FORMAT "x(10)":U
            LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 16.57
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiCustNumber AT ROW 1.19 COL 44.6 COLON-ALIGNED WIDGET-ID 26
     btGo AT ROW 1.19 COL 113.4 WIDGET-ID 34
     btShow AT ROW 1.19 COL 128.8 WIDGET-ID 36
     fiInvoice AT ROW 1.24 COL 13.2 COLON-ALIGNED WIDGET-ID 30
     fiBol AT ROW 1.24 COL 67.2 COLON-ALIGNED WIDGET-ID 32
     cbPrinted AT ROW 1.24 COL 91.4 COLON-ALIGNED WIDGET-ID 46
     fiBillOwner AT ROW 2.38 COL 13.2 COLON-ALIGNED WIDGET-ID 152
     fiCustName AT ROW 2.38 COL 44.6 COLON-ALIGNED WIDGET-ID 28
     cbStatus AT ROW 2.48 COL 91.2 COLON-ALIGNED WIDGET-ID 50
     cbAutoApproved AT ROW 2.48 COL 126.6 COLON-ALIGNED WIDGET-ID 150
     Browser-Table AT ROW 3.86 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 19.33 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 19.33 COL 37 COLON-ALIGNED NO-LABEL
     auto_find AT ROW 19.33 COL 87.4 COLON-ALIGNED HELP
          "Enter Auto Find Value" NO-LABEL
     Btn_Clear_Find AT ROW 19.38 COL 130.8 HELP
          "CLEAR AUTO FIND Value"
     fi_By AT ROW 19.38 COL 2.6 NO-LABEL
     fi_AutoFindLabel AT ROW 19.57 COL 79 NO-LABEL
     RECT-4 AT ROW 19.1 COL 1
     RECT-7 AT ROW 1 COL 1 WIDGET-ID 48
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 
         DEFAULT-BUTTON btGo.


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


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table cbAutoApproved F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN auto_find IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       auto_find:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RADIO-SET browse-order IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       browse-order:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

ASSIGN 
       inv-head.r-no:VISIBLE IN BROWSE Browser-Table = FALSE
       inv-head.company:VISIBLE IN BROWSE Browser-Table = FALSE.

/* SETTINGS FOR BUTTON Btn_Clear_Find IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Clear_Find:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi_AutoFindLabel IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       fi_AutoFindLabel:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi_By IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       fi_By:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.inv-head"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _OrdList          = "ASI.inv-head.inv-no|yes"
     _Where[1]         = "ASI.inv-head.company = cocode and
ASI.inv-head.multi-invoice = no"
     _FldNameList[1]   > ASI.inv-head.inv-no
"inv-head.inv-no" "Invoice#" ">>>>>>9" "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.inv-head.cust-no
"inv-head.cust-no" "Cust #" ? "character" ? ? ? 14 ? ? no ? no no "10.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.inv-head.cust-name
"inv-head.cust-name" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.inv-head.inv-date
"inv-head.inv-date" "Inv Date" ? "date" ? ? ? 14 ? ? no ? no no "14.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.inv-head.bol-no
"inv-head.bol-no" "BOL #" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"f-ordno() @ li-ord-no" "Order#" ? ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.inv-head.printed
"inv-head.printed" ? "Yes/No" "logical" ? ? ? 14 ? ? no ? no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.inv-head.t-inv-rev
"inv-head.t-inv-rev" "Invoice Amt" ? "decimal" ? ? ? 14 ? ? no ? no no "16.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"getStatus() @ ls-status" "Status" "x(8)" ? ? ? ? 14 ? ? no ? no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.inv-head.autoApproved
"inv-head.autoApproved" "Auto" "Yes/No" "logical" ? ? ? 14 ? ? no ? no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.inv-head.r-no
"inv-head.r-no" ? ? "integer" ? ? ? 14 ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.inv-head.company
"inv-head.company" ? ? "character" ? ? ? ? ? ? no "" no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"f-cust-Po() @ li-cust-Po" "PO#" ? ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.inv-head.t-inv-tax
"inv-head.t-inv-tax" "Tax Amt" "->>,>>>,>>9.99" "decimal" ? ? ? 14 ? ? no ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.inv-head.t-inv-freight
"inv-head.t-inv-freight" "Freight Amt" ? "decimal" ? ? ? 14 ? ? no ? no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.inv-head.fob-code
"inv-head.fob-code" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"pGetFreightTerms() @ cFreight" "Freight Terms" "x(12)" ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.inv-head.multi-invoice
"inv-head.multi-invoice" "Grouped" ? "logical" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.inv-head.multi-inv-no
"inv-head.multi-inv-no" "Group #" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "FILL-IN" "," ? ? 5 no 0 no no
     _FldNameList[20]   > ASI.inv-head.ediInvoice
"inv-head.ediInvoice" ? ? "logical" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.inv-head.spare-char-5
"inv-head.spare-char-5" "Comment" "x(60)" "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"pGetMargin() @ dMargin" "Margin%" ? ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.cust.accountant
"cust.accountant" "Accountant" "x(10)" "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:  
    &scoped-define exclude-row-display true 
    {methods/template/brwrowdisplay.i}     
  /* gdm - 11180901 */
  IF invcopys-cha NE "" THEN
     RUN set-row-bgcolor.
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
    DEFINE VARIABLE hdColumn    AS HANDLE     NO-UNDO.
    DEFINE VARIABLE cColumnName AS CHARACTER  NO-UNDO.

    ASSIGN
        hdColumn     = {&BROWSE-NAME}:CURRENT-COLUMN 
        cColumnName  = hdColumn:NAME
        .

    IF cSortBy EQ cColumnName THEN 
        lSortAsc = NOT lSortAsc.
    ELSE
        cSortBy = cColumnName.
        
    APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

    APPLY "CHOOSE" TO btGo.
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
  {methods/template/local/setvalue.i}

  FIND FIRST b-cust WHERE
       b-cust.company EQ inv-head.company AND
       b-cust.cust-no EQ inv-head.cust-no
       NO-LOCK NO-ERROR.

  IF AVAIL b-cust THEN
     RUN pushpin-image-proc(INPUT b-cust.rec_key).
  RUN dept-pan-image-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btGo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGo B-table-Win
ON CHOOSE OF btGo IN FRAME F-Main /* Go */
DO: 
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            fiinvoice
            fibol
            ficustName
            ficustNumber
            cbprinted
            cbstatus
            cbAutoApproved
            lFirst = NO
            fiBillOwner
            .
            
    IF ficustName:SCREEN-VALUE EQ "" AND ficustNumber:SCREEN-VALUE EQ "" AND 
       fiinvoice:SCREEN-VALUE  EQ "" AND fibol:SCREEN-VALUE EQ "" THEN 
        lFirst = YES.
        
    RUN dispatch ('open-query').

    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
    
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btShow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btShow B-table-Win
ON CHOOSE OF btShow IN FRAME F-Main /* Show All */
DO: 
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            ficustName:SCREEN-VALUE    = ""
            ficustNumber:SCREEN-VALUE  = ""
            fibol:SCREEN-VALUE         = ""
            fiinvoice:SCREEN-VALUE     = ""
            fiBillOwner:SCREEN-VALUE   = ""
            cbstatus:SCREEN-VALUE      = "A"
            cbprinted:SCREEN-VALUE     = "All"
            cbAutoApproved:SCREEN-VALUE = "All"
            .    
    END.  
    APPLY "CHOOSE" TO btGo.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fiBillOwner
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBillOwner B-table-Win
ON HELP OF fiBillOwner IN FRAME F-Main
DO:   
  DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.   

  run windows/l-users.w (fiBillOwner:SCREEN-VALUE in frame {&frame-name}, output char-val).
  IF char-val <> "" THEN ASSIGN fiBillOwner:SCREEN-VALUE = ENTRY(1,char-val)  . 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-inv-file B-table-Win 
PROCEDURE check-inv-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   RUN oe/AutoApproveInvoices.w .
   RUN value-changed-proc. 

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
   DEFINE VARIABLE v-spec AS LOG NO-UNDO.
   DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

   FIND FIRST notes WHERE notes.rec_key = inv-head.rec_key
       NO-LOCK NO-ERROR.

   IF AVAILABLE notes THEN
      v-spec = TRUE.
   ELSE v-spec = FALSE.

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN dept-pen-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN oe/rd-invexp.w .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-init B-table-Win 
PROCEDURE proc-init :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/   

    fiBillOwner = USERID(LDBNAME(1)).

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
    IF lFirst THEN DO:
       &SCOPED-DEFINE open-query               ~
        OPEN QUERY {&browse-name} {&for-each1}, ~
        {&for-each2}
        
        IF lSortAsc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}. 
    END.
  
    ELSE DO:
        IF fibol NE 0 THEN DO:
            &SCOPED-DEFINE open-query              ~
            OPEN QUERY {&browse-name} {&for-each1} ~
            AND inv-head.bol-no EQ fibol ,          ~
            {&for-each2}
            
            IF lSortAsc THEN {&open-query} {&sortby-phrase-asc}.
                        ELSE {&open-query} {&sortby-phrase-desc}. 
        END.
      
        ELSE IF ficustNAME NE "" OR ficustNumber NE "" THEN DO:
            &SCOPED-DEFINE open-query              ~
            OPEN QUERY {&browse-name} {&for-each1}, ~
            {&for-each2}
   
            IF lSortAsc THEN {&open-query} {&sortby-phrase-asc}.
                        ELSE {&open-query} {&sortby-phrase-desc}.
         END. 
        
        ELSE IF fiinvoice NE 0 THEN DO:
             &SCOPED-DEFINE open-query               ~
             OPEN QUERY {&browse-name} {&for-each1}  ~
             AND inv-head.inv-no = fiinvoice,        ~
             {&for-each2}
  
             IF lSortAsc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}.   
        END.
          
    
  END.

  lFirst = NO.

  RUN dispatch ("display-fields").

  RUN dispatch ("row-changed").

  IF AVAIL inv-head THEN APPLY "value-changed" TO BROWSE {&browse-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pushpin-image-proc B-table-Win 
PROCEDURE pushpin-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.
   
   DEF VAR v-att AS LOG NO-UNDO.
   DEF VAR lv-ord-no AS CHAR NO-UNDO.
   DEF VAR v-ord-no AS INT NO-UNDO.

   FIND FIRST inv-line OF inv-head
        WHERE inv-line.ord-no NE 0
        NO-LOCK NO-ERROR.
  
   IF NOT AVAIL inv-line THEN
      FIND FIRST inv-misc OF inv-head
           WHERE inv-misc.ord-no NE 0
           NO-LOCK NO-ERROR.

   ASSIGN
      lv-ord-no = IF AVAIL inv-line THEN STRING(inv-line.ord-no)
                  ELSE
                  IF AVAIL inv-misc THEN STRING(inv-misc.ord-no)
                  ELSE
                  STRING(0)
    
    v-att = CAN-FIND(FIRST asi.attach WHERE
            attach.company = cocode AND
            attach.rec_key = ip-rec_key AND
            (attach.est-no EQ lv-ord-no OR ATTACH.est-no EQ "")).

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attachcust-target':U, OUTPUT char-hdl).
  
   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN pushpin-image IN WIDGET-HANDLE(char-hdl) (INPUT v-att).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshBrowse B-table-Win 
PROCEDURE refreshBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAILABLE inv-head THEN
  BROWSE {&BROWSE-NAME}:REFRESH().

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
  {src/adm/template/snd-list.i "inv-head"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-row-bgcolor B-table-Win 
PROCEDURE set-row-bgcolor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-inv-head FOR inv-head.
DEF BUFFER bf-inv-line FOR inv-line.
DEF BUFFER bf-oe-ord   FOR oe-ord.
DEF BUFFER bf-oe-ordl  FOR oe-ordl.

DEF VAR v-totqty LIKE inv-line.qty NO-UNDO.

IF AVAIL inv-head THEN DO:
   ASSIGN li-ord-no = f-ordno()
          li-cust-Po = f-cust-PO().
  
   IF li-ord-no NE 0 THEN DO:
  
      FIND FIRST bf-oe-ord NO-LOCK 
        WHERE bf-oe-ord.company EQ inv-head.company 
          AND bf-oe-ord.ord-no  EQ li-ord-no NO-ERROR.
     
      FOR EACH bf-inv-line OF inv-head NO-LOCK
          BREAK BY bf-inv-line.line
                BY bf-inv-line.i-no:
        
          IF FIRST-OF(bf-inv-line.i-no) THEN DO:
         
             ASSIGN v-totqty = 0.
             FOR EACH inv-line FIELDS(ship-qty) NO-LOCK
               WHERE inv-line.company EQ bf-inv-line.company 
                 AND inv-line.inv-no  EQ bf-inv-line.inv-no  
                 AND inv-line.line    EQ bf-inv-line.line
                 AND inv-line.ord-no  EQ bf-inv-line.ord-no
                 AND inv-line.i-no    EQ bf-inv-line.i-no:
                 ASSIGN v-totqty = v-totqty + inv-line.ship-qty.
             END.
             
             IF v-totqty NE bf-inv-line.qty THEN DO:
            
                ASSIGN inv-head.inv-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12
                       inv-head.cust-no:BGCOLOR IN BROWSE {&BROWSE-NAME}   = 12
                       inv-head.cust-name:BGCOLOR IN BROWSE {&BROWSE-NAME} = 12
                       inv-head.inv-date:BGCOLOR IN BROWSE {&BROWSE-NAME}  = 12
                       inv-head.bol-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12
                       li-ord-no:BGCOLOR IN BROWSE {&BROWSE-NAME}          = 12
                       inv-head.printed:BGCOLOR IN BROWSE {&BROWSE-NAME}   = 12
                       inv-head.t-inv-rev:BGCOLOR IN BROWSE {&BROWSE-NAME} = 12
                       ls-status:BGCOLOR IN BROWSE {&BROWSE-NAME}          = 12
                       inv-head.r-no:BGCOLOR IN BROWSE {&BROWSE-NAME}      = 12
                       li-cust-Po:BGCOLOR IN BROWSE {&BROWSE-NAME}         = 12.
               
                LEAVE.
             END.
          END.
      END.
   END.
   ASSIGN li-ord-no =  0
          li-cust-Po = "".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-proc B-table-Win 
PROCEDURE value-changed-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-cust-Po B-table-Win 
FUNCTION f-cust-Po RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lf-cust-Po AS CHAR NO-UNDO.
  DEF VAR lv-ord-no LIKE inv-line.ord-no.
  DEF BUFFER b-inv-line FOR inv-line.
  DEF BUFFER b-inv-misc FOR inv-misc.
  DEF BUFFER b-oe-ord FOR oe-ord.


  FIND FIRST b-inv-line OF inv-head NO-LOCK
      WHERE b-inv-line.ord-no NE 0
      NO-ERROR.
  IF NOT AVAIL b-inv-line THEN
      FIND FIRST b-inv-misc OF inv-head NO-LOCK
      WHERE b-inv-misc.ord-no NE 0
      NO-ERROR.
  lv-ord-no = IF AVAIL b-inv-line THEN b-inv-line.ord-no ELSE
      IF AVAIL b-inv-misc THEN b-inv-misc.ord-no ELSE 0.

  IF lv-ord-no NE 0 THEN DO:
      FIND FIRST b-oe-ord WHERE b-oe-ord.company EQ inv-head.company
          AND b-oe-ord.ord-no EQ lv-ord-no NO-LOCK NO-ERROR.
      IF AVAIL b-oe-ord THEN
          lf-cust-Po = b-oe-ord.po-no.
  END.
  
  RETURN lf-cust-Po.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-ordno B-table-Win 
FUNCTION f-ordno RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lf-ord-no AS INT NO-UNDO.


  RELEASE inv-line.
  RELEASE inv-misc.

  FIND FIRST inv-line OF inv-head NO-LOCK
      WHERE inv-line.ord-no NE 0
      NO-ERROR.
  IF NOT AVAIL inv-line THEN
  FIND FIRST inv-misc OF inv-head NO-LOCK
      WHERE inv-misc.ord-no NE 0
      NO-ERROR.
  lf-ord-no = IF AVAIL inv-line THEN inv-line.ord-no ELSE
              IF AVAIL inv-misc THEN inv-misc.ord-no ELSE 0.
  
  RETURN lf-ord-no.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStatus B-table-Win 
FUNCTION getStatus RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cReturn AS CHAR NO-UNDO.
CASE inv-head.stat:
    WHEN "H" THEN
        cReturn = "On Hold".
    WHEN "" OR WHEN "X" THEN
        cReturn = "Released".
    WHEN "W" THEN
        cReturn = "Wait/App".
    OTHERWISE
        cReturn = "".
END.

  RETURN cReturn.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pGetFreightTerms B-table-Win 
FUNCTION pGetFreightTerms RETURNS CHARACTER PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    CASE inv-head.frt-pay:       
        WHEN "P" THEN
            RETURN "P-Prepaid". 
        WHEN "C" THEN 
            RETURN "C-Collect".
        WHEN "B" THEN 
            RETURN "B-Bill".
        WHEN "T" THEN
             RETURN "T-3rd Party".
    END CASE.
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pGetMargin B-table-Win 
FUNCTION pGetMargin RETURNS DECIMAL PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dMarginPct AS DECIMAL NO-UNDO.
    
    IF inv-head.t-inv-rev NE 0 AND inv-head.t-inv-rev NE ? THEN DO:
        dMarginPct = (inv-head.t-inv-rev - inv-head.t-inv-cost) / (inv-head.t-inv-rev) * 100 .
        IF dMarginPct GT 999.99 THEN 
            RETURN 999.99.
        ELSE IF dMarginPct LT 0 AND dMarginPct LT -999.99 THEN  
            RETURN -999.99.  
        ELSE 
            RETURN dMarginPct.                   
    END.    
    ELSE
        RETURN 0.            
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

