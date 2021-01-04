&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: po\d-fg-rcpth.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-recid2 AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO .   /* add,update,view */
DEFINE OUTPUT PARAMETER op-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN 
    cocode = g_company.
ASSIGN 
    locode = g_loc.

{sa/sa-sls01.i}



DEFINE VARIABLE ll-order-warned AS LOGICAL NO-UNDO.

DEFINE VARIABLE li-pallets      AS INTEGER NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lAdjustReason-log AS LOGICAL NO-UNDO .
DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
DEFINE VARIABLE cComboList AS CHARACTER NO-UNDO .

RUN sys/ref/nk1look.p (INPUT cocode, "AdjustReason", "L" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT cRtnChar, OUTPUT lRecFound).
lAdjustReason-log = LOGICAL(cRtnChar) NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fg-rcpth fg-rdtlh

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame fg-rcpth.i-no fg-rdtlh.tag ~
fg-rcpth.job-no fg-rcpth.job-no2 fg-rcpth.po-no fg-rcpth.po-line ~
fg-rdtlh.loc fg-rdtlh.loc-bin fg-rdtlh.qty fg-rdtlh.cost fg-rdtlh.cases ~
fg-rdtlh.qty-case fg-rdtlh.stacks-unit fg-rdtlh.partial fg-rcpth.pur-uom ~
fg-rdtlh.tot-wt fg-rdtlh.stack-code fg-rdtlh.user-id fg-rcpth.post-date ~
fg-rdtlh.cust-no fg-rdtlh.reject-code[1] fg-rdtlh.enteredBy ~
fg-rdtlh.enteredDT fg-rcpth.rita-code fg-rcpth.trans-date 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame fg-rcpth.i-no ~
fg-rdtlh.tag fg-rcpth.job-no fg-rcpth.job-no2 fg-rcpth.po-no ~
fg-rcpth.po-line fg-rdtlh.loc fg-rdtlh.loc-bin fg-rdtlh.qty fg-rdtlh.cost ~
fg-rdtlh.cases fg-rdtlh.qty-case fg-rdtlh.stacks-unit fg-rdtlh.partial ~
fg-rcpth.pur-uom fg-rdtlh.tot-wt fg-rdtlh.stack-code fg-rdtlh.cust-no ~
fg-rdtlh.reject-code[1] fg-rcpth.rita-code fg-rcpth.trans-date 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame fg-rcpth fg-rdtlh
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame fg-rcpth
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Dialog-Frame fg-rdtlh
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH fg-rcpth ~
      WHERE fg-rcpth.company eq cocode  SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH fg-rcpth ~
      WHERE fg-rcpth.company eq cocode  SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame fg-rcpth fg-rdtlh
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame fg-rcpth
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame fg-rdtlh


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS fg-rcpth.i-no fg-rdtlh.tag fg-rcpth.job-no ~
fg-rcpth.job-no2 fg-rcpth.po-no fg-rcpth.po-line fg-rdtlh.loc ~
fg-rdtlh.loc-bin fg-rdtlh.qty fg-rdtlh.cost fg-rdtlh.cases ~
fg-rdtlh.qty-case fg-rdtlh.stacks-unit fg-rdtlh.partial fg-rcpth.pur-uom ~
fg-rdtlh.tot-wt fg-rdtlh.stack-code fg-rdtlh.cust-no ~
fg-rdtlh.reject-code[1] fg-rcpth.rita-code fg-rcpth.trans-date 
&Scoped-define ENABLED-TABLES fg-rcpth fg-rdtlh
&Scoped-define FIRST-ENABLED-TABLE fg-rcpth
&Scoped-define SECOND-ENABLED-TABLE fg-rdtlh
&Scoped-Define ENABLED-OBJECTS btnCalendar-1 Btn_OK ~
Btn_Done Btn_Cancel RECT-21 RECT-38 RECT-39 RECT-40 RECT-41 RECT-42 RECT-43 
&Scoped-Define DISPLAYED-FIELDS fg-rcpth.i-no fg-rdtlh.tag fg-rcpth.job-no ~
fg-rcpth.job-no2 fg-rcpth.po-no fg-rcpth.po-line fg-rdtlh.loc ~
fg-rdtlh.loc-bin fg-rdtlh.qty fg-rdtlh.cost fg-rdtlh.cases ~
fg-rdtlh.qty-case fg-rdtlh.stacks-unit fg-rdtlh.partial fg-rcpth.pur-uom ~
fg-rdtlh.tot-wt fg-rdtlh.stack-code fg-rdtlh.user-id fg-rcpth.post-date ~
fg-rdtlh.cust-no fg-rdtlh.reject-code[1] fg-rdtlh.enteredBy ~
fg-rdtlh.enteredDT fg-rcpth.rita-code fg-rcpth.trans-date 
&Scoped-define DISPLAYED-TABLES fg-rcpth fg-rdtlh
&Scoped-define FIRST-DISPLAYED-TABLE fg-rcpth
&Scoped-define SECOND-DISPLAYED-TABLE fg-rdtlh
&Scoped-Define DISPLAYED-OBJECTS fi_pallet fi_qty-pallet fi_bol-ship ~
fi_vend-no fi_vend-name fi_BenQtyBef fi_ben-qty fi_tr-time fi_bol-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-bol Dialog-Frame 
FUNCTION display-bol RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-ship Dialog-Frame 
FUNCTION display-ship RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-fg-qty Dialog-Frame 
FUNCTION get-fg-qty RETURNS INTEGER
    ( /* parameter-definitions */ INPUT ip-int AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-pallet-info Dialog-Frame 
FUNCTION get-pallet-info RETURNS INTEGER
    (OUTPUT op-qty-pal AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-vend-info Dialog-Frame 
FUNCTION get-vend-info RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-vend-no Dialog-Frame 
FUNCTION get-vend-no RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON Btn_Cancel 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE fi_ben-qty AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Bin Change" 
     VIEW-AS FILL-IN 
     SIZE 26.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_BenQtyBef AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Before Qty" 
     VIEW-AS FILL-IN 
     SIZE 22.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_bol-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Bol No" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_bol-ship AS CHARACTER FORMAT "x(8)":U 
     LABEL "BOL Cust" 
     VIEW-AS FILL-IN 
     SIZE 22.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_pallet AS INTEGER FORMAT "->>>>>>":U INITIAL 0 
     LABEL "Pallets" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_qty-pallet AS INTEGER FORMAT "->>>>>>":U INITIAL 0 
     LABEL "Qty/Pallet" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_tr-time AS CHARACTER FORMAT "X(15)":U 
     LABEL "Tr Time" 
     VIEW-AS FILL-IN 
     SIZE 15.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_vend-name AS CHARACTER FORMAT "x(20)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_vend-no AS CHARACTER FORMAT "x(10)":U 
     LABEL "Vendor" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 19 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 48 BY 9.14
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 39.6 BY 11.52
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 41.6 BY 11.52
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 48 BY 4.05
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 39.6 BY 3.57
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 89 BY 3.19
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      fg-rcpth, 
      fg-rdtlh SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fg-rcpth.i-no AT ROW 1.71 COL 17 COLON-ALIGNED
          LABEL "FG Item#" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          BGCOLOR 15 FONT 1
     fg-rdtlh.tag AT ROW 2.91 COL 17 COLON-ALIGNED
          LABEL "Tag#" FORMAT "x(25)"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 1
     fg-rcpth.job-no AT ROW 4.1 COL 17 COLON-ALIGNED
          LABEL "Job" FORMAT "x(6)"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 FONT 1
     fg-rcpth.job-no2 AT ROW 4.1 COL 33.2 COLON-ALIGNED NO-LABEL FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 6.6 BY 1
          BGCOLOR 15 FONT 1
     fg-rcpth.po-no AT ROW 6.48 COL 17 COLON-ALIGNED
          LABEL "Vendor PO#" FORMAT "x(9)"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          BGCOLOR 15 FONT 1
     fg-rcpth.po-line AT ROW 7.67 COL 17 COLON-ALIGNED
          LABEL "PO Ln" FORMAT ">99"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
          BGCOLOR 15 FONT 1
    fg-rdtlh.stack-code AT ROW 8.86 COL 17 COLON-ALIGNED
          LABEL "FG Lot#" FORMAT "X(20)"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 1

     fg-rdtlh.loc AT ROW 1.71 COL 66 COLON-ALIGNED FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fg-rdtlh.loc-bin AT ROW 2.91 COL 66 COLON-ALIGNED
          LABEL "Bin" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
    fg-rdtlh.cases AT ROW 4.1 COL 66 COLON-ALIGNED
          LABEL "Units" FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
    fg-rdtlh.qty-case AT ROW 5.29 COL 66 COLON-ALIGNED
          LABEL "Qty/Unit" FORMAT "->>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
    fg-rdtlh.partial AT ROW 6.48 COL 66 COLON-ALIGNED
          LABEL "Partial" FORMAT "->>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
    fg-rdtlh.stacks-unit AT ROW 7.67 COL 66 COLON-ALIGNED
          LABEL "Units/Pallet" FORMAT ">,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1

     fg-rdtlh.qty AT ROW 8.86 COL 66 COLON-ALIGNED
          LABEL "Quantity" FORMAT "->>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     
     fi_pallet AT ROW 10.1 COL 66 COLON-ALIGNED
     fi_qty-pallet AT ROW 11.29 COL 66 COLON-ALIGNED
     
     
     fg-rdtlh.user-id AT ROW 10.29 COL 106 COLON-ALIGNED
          LABEL "Posted By" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 22.8 BY 1
          BGCOLOR 15 FONT 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     fg-rcpth.rita-code AT ROW 1.71 COL 122 COLON-ALIGNED
          LABEL "TR Code" FORMAT "x(1)"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 FONT 1
     fg-rcpth.trans-date AT ROW 2.91 COL 107.4 COLON-ALIGNED
          LABEL "TR Date" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 FONT 1
     fg-rdtlh.cust-no AT ROW 11.1 COL 15.8 COLON-ALIGNED
          LABEL "Customer" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          BGCOLOR 15 FONT 1

    fg-rdtlh.cost AT ROW 13.14 COL 67 COLON-ALIGNED
          LABEL "Cost" FORMAT "->>>,>>9.99<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1

    fg-rcpth.pur-uom AT ROW 14.33 COL 67 COLON-ALIGNED
          LABEL "UOM for Cost" FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fg-rdtlh.tot-wt AT ROW 15.43 COL 67 COLON-ALIGNED
          LABEL "Lbs / 100" FORMAT ">>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1

     fi_bol-ship AT ROW 13.52 COL 16.6 COLON-ALIGNED
     fg-rcpth.post-date AT ROW 11.48 COL 106 COLON-ALIGNED
          LABEL "Posted" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          BGCOLOR 15 FONT 1
     fi_vend-no AT ROW 5.29 COL 17 COLON-ALIGNED
     fi_vend-name AT ROW 8.86 COL 102 COLON-ALIGNED
     fi_BenQtyBef AT ROW 17.33 COL 17 COLON-ALIGNED
     fi_ben-qty AT ROW 17.33 COL 57.4 COLON-ALIGNED
    
     fg-rdtlh.reject-code[1] AT ROW 18.48 COL 26 COLON-ALIGNED
          LABEL "Adjustment Reason"
          VIEW-AS COMBO-BOX SORT INNER-LINES 5
          LIST-ITEM-PAIRS "Item 1"," Item 1"
          DROP-DOWN-LIST
          SIZE 58.6 BY 1
          BGCOLOR 15 FONT 1
     fg-rdtlh.enteredBy AT ROW 7.67 COL 106 COLON-ALIGNED
          LABEL "Scanned By" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 22.8 BY 1
          BGCOLOR 15 FONT 1
     fg-rdtlh.enteredDT AT ROW 6.48 COL 103 COLON-ALIGNED NO-LABEL FORMAT "99/99/9999 HH:MM:SS.SSS"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
          BGCOLOR 15 FONT 1
     
     btnCalendar-1 AT ROW 2.86 COL 124.4
     fi_tr-time AT ROW 4.1 COL 111.6 COLON-ALIGNED
     fi_bol-no AT ROW 12.29 COL 16 COLON-ALIGNED
     Btn_OK AT ROW 18.19 COL 116
     Btn_Done AT ROW 18.48 COL 117
     Btn_Cancel AT ROW 18.19 COL 125
     "Scan Date/Time" VIEW-AS TEXT
          SIZE 20 BY .95 AT ROW 5.33 COL 107.6 WIDGET-ID 6
     RECT-21 AT ROW 17.95 COL 115
     RECT-38 AT ROW 1.38 COL 2
     RECT-39 AT ROW 1.38 COL 51.4 WIDGET-ID 2
     RECT-40 AT ROW 1.38 COL 92 WIDGET-ID 4
     RECT-41 AT ROW 10.81 COL 2 WIDGET-ID 8
     RECT-42 AT ROW 13.05 COL 51.2 WIDGET-ID 10
     RECT-43 AT ROW 16.95 COL 2 WIDGET-ID 12
     SPACE(44.79) SKIP(0.76)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "History Item Update".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
/*{methods/template/viewer.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME Dialog-Frame
   3                                                                    */
/* SETTINGS FOR FILL-IN fg-rdtlh.cases IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rdtlh.cost IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rdtlh.cost IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rdtlh.reject-code[1] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fg-rdtlh.enteredBy IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN fg-rdtlh.enteredDT IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN fi_bol-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_bol-ship IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_pallet IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_qty-pallet IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_tr-time IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_vend-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_vend-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_BenQtyBef IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_ben-qty IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fg-rcpth.i-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rcpth.job-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rcpth.job-no2 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rdtlh.loc IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN fg-rdtlh.loc-bin IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rdtlh.partial IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rcpth.po-line IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rcpth.po-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rcpth.post-date IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN fg-rcpth.pur-uom IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rdtlh.qty IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rdtlh.qty-case IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rcpth.rita-code IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rdtlh.stack-code IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rdtlh.stacks-unit IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rdtlh.tag IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rdtlh.tot-wt IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rcpth.trans-date IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rdtlh.user-id IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.fg-rcpth,asi.fg-rdtlh "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.fg-rcpth.company eq cocode "
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON CTRL-O OF FRAME Dialog-Frame /* History Item Update */
OR ctrl-o OF btnCalendar-1 ANYWHERE
    DO:
        DEFINE VARIABLE char-hdl AS CHARACTER.
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* History Item Update */
DO:
        DEFINE VARIABLE char-val  AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE hlp-recid AS RECID         NO-UNDO.
        DEFINE VARIABLE lw-focus  AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE ll        AS LOGICAL       INITIAL YES NO-UNDO.
    
        lw-focus = FOCUS.

        CASE lw-focus:NAME :
            WHEN "po-no" THEN 
                DO:
                    RUN windows/l-poopen.w (g_company, lw-focus:SCREEN-VALUE, OUTPUT char-val).     
                    IF char-val NE "" THEN ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val) .
                END.
            WHEN "i-no" THEN 
                DO:
                    RUN windows/l-itemfg.w (g_company, "",lw-focus:SCREEN-VALUE, OUTPUT char-val).     
                    ASSIGN 
                        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
                END.     
            WHEN "job-no" THEN 
                DO:
                    RUN windows/l-jobno.w (g_company,lw-focus:SCREEN-VALUE, OUTPUT char-val,OUTPUT hlp-recid).     
                    ASSIGN 
                        fg-rcpth.job-no:SCREEN-VALUE = ENTRY(1,char-val).
                    ASSIGN 
                        fg-rcpth.job-no2:SCREEN-VALUE = ENTRY(2,char-val).
                END.    
            WHEN "loc" THEN 
                DO:
                    RUN windows/l-loc.w (g_company,lw-focus:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val NE "" THEN 
                    DO :
                        ASSIGN 
                            lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
                    /*oe-rell.loc-bin:screen-value  = entry(2,char-val)*/

                    END.  
                END.
            WHEN "loc-bin" THEN 
                DO:
                    RUN windows/l-fgbin.w (g_company,fg-rdtlh.loc:SCREEN-VALUE, lw-focus:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val NE "" THEN 
                    DO :
                        ASSIGN 
                            lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
                    END.   
                END.
            WHEN "cust-no" THEN 
                DO:
                    RUN windows/l-cust.w (g_company,lw-focus:SCREEN-VALUE, OUTPUT char-val).     
                    ASSIGN 
                        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
                END.    
      

        END CASE.

        RETURN NO-APPLY.


    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* History Item Update */
ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* History Item Update */
DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 Dialog-Frame
ON CHOOSE OF btnCalendar-1 IN FRAME Dialog-Frame
DO:
        {methods/btnCalendar.i fg-rcpth.trans-date}
        APPLY "entry" TO fg-rcpth.trans-date .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
        IF AVAILABLE fg-rcpth THEN
            op-rowid = ROWID(fg-rcpth).
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
DO:
  &IF DEFINED (adm-panel) NE 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
        APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Save */
DO:
        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.

    IF fg-rdtlh.cust-no:MODIFIED THEN 
    DO:
        RUN valid-cust-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
    
    IF fg-rcpth.i-no:MODIFIED THEN 
    DO:
        RUN valid-i-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
    
    IF fg-rdtlh.loc:MODIFIED THEN 
    DO:
        RUN valid-loc NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
    
    IF fg-rdtlh.loc-bin:MODIFIED THEN 
    DO:
        RUN valid-loc-bin NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
    
    IF fg-rdtlh.tag:MODIFIED THEN 
    DO:
        MESSAGE "MOD" VIEW-AS ALERT-BOX.
        RUN valid-tag NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        RUN valid-tag-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.

    RUN valid-reason NO-ERROR .
    IF lCheckError THEN RETURN NO-APPLY.

        RUN update-record .

        op-rowid = ROWID(fg-rcpth).

        APPLY "go" TO FRAME {&FRAME-NAME}.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.cases Dialog-Frame
ON LEAVE OF fg-rdtlh.cases IN FRAME Dialog-Frame /* Units */
DO:
    IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
    DO:
        RUN reCalcQty.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.cases Dialog-Frame
ON RETURN OF fg-rdtlh.cases IN FRAME Dialog-Frame /* Units */
DO:
    IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
    DO:
        RUN reCalcQty.
  
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.cost
&Scoped-define SELF-NAME fg-rdtlh.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.cust-no Dialog-Frame
ON LEAVE OF fg-rdtlh.cust-no IN FRAME Dialog-Frame /* Customer */
DO:
        IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
        DO:
            RUN valid-cust-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rcpth.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.i-no Dialog-Frame
ON LEAVE OF fg-rcpth.i-no IN FRAME Dialog-Frame /* FG Item# */
DO:
    IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
    DO:
            RUN valid-i-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rcpth.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.job-no Dialog-Frame
ON ENTRY OF fg-rcpth.job-no IN FRAME Dialog-Frame /* Job */
DO:
    /*IF fg-rcpth.rita-code:SCREEN-VALUE  NE "S" THEN DO:
      APPLY "tab" TO {&self-name}.
      RETURN NO-APPLY.
    END.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.job-no Dialog-Frame
ON LEAVE OF fg-rcpth.job-no IN FRAME Dialog-Frame /* Job */
DO:
        DEFINE VARIABLE lv-job-no AS CHARACTER NO-UNDO.

    IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
    DO:

        ASSIGN
            lv-job-no = TRIM({&self-name}:SCREEN-VALUE )
           lv-job-no = FILL(" ",6 - LENGTH(lv-job-no)) + lv-job-no
            {&self-name}:SCREEN-VALUE  = lv-job-no.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rcpth.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.job-no2 Dialog-Frame
ON ENTRY OF fg-rcpth.job-no2 IN FRAME Dialog-Frame /* Run # */
DO:
    /*IF fg-rcpth.rita-code:SCREEN-VALUE  NE "S" THEN DO:
      APPLY "tab" TO {&self-name}.
      RETURN NO-APPLY.
    END.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.loc Dialog-Frame
ON LEAVE OF fg-rdtlh.loc IN FRAME Dialog-Frame /* Warehouse */
DO:
    IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
    DO:
            RUN valid-loc NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.loc-bin Dialog-Frame
ON LEAVE OF fg-rdtlh.loc-bin IN FRAME Dialog-Frame /* Bin */
DO:
    IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
    DO:
            RUN valid-loc-bin NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.partial Dialog-Frame
ON LEAVE OF fg-rdtlh.partial IN FRAME Dialog-Frame /* Partial */
DO:
    IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
    DO:
        RUN reCalcQty.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.partial Dialog-Frame
ON RETURN OF fg-rdtlh.partial IN FRAME Dialog-Frame /* Partial */
DO:
    IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
    DO:
        RUN reCalcQty.
   
    END.
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rcpth.po-no
&Scoped-define SELF-NAME fg-rcpth.post-date
&Scoped-define SELF-NAME fg-rcpth.pur-uom
&Scoped-define SELF-NAME fg-rdtlh.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.qty Dialog-Frame
ON LEAVE OF fg-rdtlh.qty IN FRAME Dialog-Frame /* Quantity */
DO:
    /*   fg-rdtlh.cases:SCREEN-VALUE   =                        */
    /*       string(INTEGER(fg-rdtlh.qty:SCREEN-VALUE  ) /      */
    /*              INTEGER(fg-rdtlh.qty-case:SCREEN-VALUE  )). */
    /*                                                                                  */
    IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
    DO:
        RUN reCalcUnits.

    END.
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.qty Dialog-Frame
ON RETURN OF fg-rdtlh.qty IN FRAME Dialog-Frame /* Quantity */
DO:
    IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
    DO:
        RUN reCalcUnits.
    END.
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.qty-case Dialog-Frame
ON LEAVE OF fg-rdtlh.qty-case IN FRAME Dialog-Frame /* Qty/Unit */
DO:
    IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
    DO:

        RUN reCalcQty.
    END.
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.qty-case Dialog-Frame
ON RETURN OF fg-rdtlh.qty-case IN FRAME Dialog-Frame /* Qty/Unit */
DO:
    IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
    DO:

        RUN reCalcQty.
   
    END.
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rcpth.rita-code
&Scoped-define SELF-NAME fg-rdtlh.stacks-unit
&Scoped-define SELF-NAME fg-rdtlh.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.tag Dialog-Frame
ON LEAVE OF fg-rdtlh.tag IN FRAME Dialog-Frame /* Tag# */
DO:
    IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
    DO:
            RUN valid-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            RUN valid-tag-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.tag Dialog-Frame
ON RETURN OF fg-rdtlh.tag IN FRAME Dialog-Frame /* Tag# */
DO:
    IF LASTKEY NE -1 
        AND SELF:MODIFIED THEN 
    DO:
        RUN valid-tag-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    END.
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.tot-wt
&Scoped-define SELF-NAME fg-rcpth.trans-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.trans-date Dialog-Frame
ON HELP OF fg-rcpth.trans-date IN FRAME Dialog-Frame /* TR Date */
DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fg-rdtlh.reject-code[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.reject-code[1] Dialog-Frame
ON LEAVE OF fg-rdtlh.reject-code[1] IN FRAME Dialog-Frame /* Reason */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-reason NO-ERROR.
    IF lCheckError THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i} 
SESSION:DATA-ENTRY-RETURN = YES.       

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    FIND FIRST fg-rcpth NO-LOCK
        WHERE fg-rcpth.company EQ cocode
        AND RECID(fg-rcpth)  EQ ip-recid NO-ERROR .
    
    FIND FIRST fg-rdtlh NO-LOCK
        WHERE fg-rdtlh.company EQ cocode
        AND RECID(fg-rdtlh)  EQ ip-recid2 NO-ERROR .

    IF AVAILABLE fg-rcpth THEN
        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no EQ fg-rcpth.i-no NO-ERROR .
    

    IF ip-type NE "view" THEN 
    DO: 
        RUN enable_UI.
        RUN display-item.

        ASSIGN 
            ll-order-warned = NO.
        btn_done:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE 
    DO:
        RUN display-item.
        ASSIGN 
            btn_done:HIDDEN IN FRAME {&FRAME-NAME} = NO.
        btn_done:SENSITIVE                        = YES.
        btn_ok:HIDDEN                             = YES.
        btn_cancel:HIDDEN                         = YES.
    END.

    fg-rcpth.rita-code:SENSITIVE = FALSE.
           
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-type-list Dialog-Frame 
PROCEDURE build-type-list :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE hPgmReason AS HANDLE    NO-UNDO.
    &IF DEFINED(FWD-VERSION) EQ 0 &THEN
    
    ASSIGN cComboList = ""  .
    RUN "fg/ReasonCode.p" PERSISTENT SET hPgmReason.
    RUN pBuildReasonCode IN hPgmReason ("ADJ",OUTPUT cComboList).
    DELETE OBJECT hPgmReason.
  
    DO WITH FRAME {&FRAME-NAME}:
        fg-rdtlh.reject-code[1]:LIST-ITEM-PAIRS = cComboList .
    END.
    &ENDIF
     
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item Dialog-Frame 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
          Purpose:     
          PARAMs:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li-qty-pal AS INTEGER NO-UNDO.

    IF AVAILABLE fg-rcpth  THEN 
    DO:
        
        ASSIGN 
            fi_tr-time = STRING(fg-rdtlh.trans-time,'HH:MM') .
      
        fi_bol-no = STRING(display-bol()) .
        fi_pallet = get-pallet-info (OUTPUT li-qty-pal) .
        fi_qty-pallet = li-qty-pal .
        fi_bol-ship = display-ship() .
        fi_vend-no = fg-rcpth.vend-no .
        fi_vend-name = get-vend-info ().
        fi_BenQtyBef = get-fg-qty (1) .
        fi_ben-qty = get-fg-qty (2) .

        RUN build-type-list .

        /* This is unneccesary, and falsely sets the MODIFIED attribute to TRUE */
        DISPLAY  /* fg-rcpth.i-no fg-rcpth.po-no 
            fg-rcpth.job-no fg-rcpth.job-no2 fg-rcpth.trans-date 
            fg-rcpth.rita-code 
            fg-rdtlh.cust-no fg-rdtlh.loc fg-rdtlh.loc-bin fg-rdtlh.qty fg-rdtlh.tag 
            fg-rdtlh.cost fg-rdtlh.cases fg-rdtlh.qty-case 
            fg-rdtlh.stacks-unit 
            fg-rdtlh.partial fg-rdtlh.stack-code 
            fg-rdtlh.tot-wt fg-rdtlh.user-id /*fg-rcpth.b-no*/ fg-rcpth.pur-uom 
            fg-rcpth.post-date  
            fg-rdtlh.reject-code[1] fg-rdtlh.enteredBy 
            fg-rdtlh.enteredDT fg-rcpth.po-line */ 
            fi_tr-time fi_bol-no fi_pallet fi_qty-pallet
            fi_bol-ship fi_vend-no fi_vend-name fi_BenQtyBef fi_ben-qty
            WITH FRAME Dialog-Frame.
    END.


    IF ip-type NE "view" THEN 
    DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.
    END.

    VIEW FRAME {&FRAME-NAME}. 
    APPLY "entry" TO FRAME {&FRAME-NAME}.


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
  DISPLAY fi_pallet fi_qty-pallet fi_bol-ship fi_vend-no fi_vend-name 
          fi_BenQtyBef fi_ben-qty fi_tr-time fi_bol-no 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE fg-rcpth THEN 
    DISPLAY fg-rcpth.i-no fg-rcpth.job-no fg-rcpth.job-no2 fg-rcpth.po-no 
          fg-rcpth.po-line fg-rcpth.pur-uom fg-rcpth.post-date 
          fg-rcpth.rita-code fg-rcpth.trans-date 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE fg-rdtlh THEN 
    DISPLAY fg-rdtlh.tag fg-rdtlh.loc fg-rdtlh.loc-bin fg-rdtlh.qty fg-rdtlh.cost 
          fg-rdtlh.cases fg-rdtlh.qty-case fg-rdtlh.stacks-unit fg-rdtlh.partial 
          fg-rdtlh.tot-wt fg-rdtlh.stack-code fg-rdtlh.user-id fg-rdtlh.cust-no 
          fg-rdtlh.reject-code[1] fg-rdtlh.enteredBy fg-rdtlh.enteredDT 
      WITH FRAME Dialog-Frame.
  ENABLE fg-rcpth.i-no fg-rdtlh.tag fg-rcpth.job-no fg-rcpth.job-no2 
         fg-rcpth.po-no fg-rcpth.po-line fg-rdtlh.loc fg-rdtlh.loc-bin 
         fg-rdtlh.qty fg-rdtlh.cost fg-rdtlh.cases fg-rdtlh.qty-case 
         fg-rdtlh.stacks-unit fg-rdtlh.partial fg-rcpth.pur-uom fg-rdtlh.tot-wt 
         fg-rdtlh.stack-code fg-rdtlh.cust-no 
         fg-rdtlh.reject-code[1] fg-rcpth.rita-code fg-rcpth.trans-date 
         btnCalendar-1 Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 RECT-39 
         RECT-40 RECT-41 RECT-42 RECT-43 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reCalcQty Dialog-Frame 
PROCEDURE reCalcQty :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE liQty        AS INTEGER.
    DEFINE VARIABLE liQtyPerCase AS INTEGER.
    DEFINE VARIABLE liPartial    AS INTEGER.
    DEFINE VARIABLE liCases      AS INTEGER.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            liCases      = INTEGER(fg-rdtlh.cases:SCREEN-VALUE )
            liQtyPerCase = INTEGER(fg-rdtlh.qty-case:SCREEN-VALUE )
            liPartial    = INTEGER(fg-rdtlh.partial:SCREEN-VALUE ).

        liQty = (liQtyPerCase * liCases) + liPartial.
        fg-rdtlh.qty:SCREEN-VALUE = STRING(liQty).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reCalcUnits Dialog-Frame 
PROCEDURE reCalcUnits :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE liQty        AS INTEGER.
    DEFINE VARIABLE liQtyPerCase AS INTEGER.
    DEFINE VARIABLE liPartial    AS INTEGER.
    DEFINE VARIABLE liCases      AS INTEGER.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            liQty        = INTEGER(fg-rdtlh.qty:SCREEN-VALUE )
            liQtyPerCase = INTEGER(fg-rdtlh.qty-case:SCREEN-VALUE )
            liPartial    = INTEGER(fg-rdtlh.partial:SCREEN-VALUE ).

        liCases = TRUNC((liQty - liPartial )/ liQtyPerCase, 0).
        liPartial = liQty - (liCases * liQtyPerCase).

        ASSIGN 
            fg-rdtlh.cases:SCREEN-VALUE   = STRING(liCases)
            fg-rdtlh.partial:SCREEN-VALUE = STRING(liPartial).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-record Dialog-Frame 
PROCEDURE update-record :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>                          
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-rcpth FOR fg-rcpth.
    DEFINE BUFFER b-rdtlh FOR fg-rdtlh.

    DISABLE TRIGGERS FOR LOAD OF fg-rcpth.
    DISABLE TRIGGERS FOR LOAD OF fg-rdtlh.

    DO WITH FRAME {&FRAME-NAME}:
        FIND b-rcpth WHERE ROWID(b-rcpth) EQ ROWID(fg-rcpth).
        FIND b-rdtlh WHERE ROWID(b-rdtlh) EQ ROWID(fg-rdtlh).


        ASSIGN
            b-rcpth.i-no           = fg-rcpth.i-no:SCREEN-VALUE 
            b-rcpth.po-no          = fg-rcpth.po-no:SCREEN-VALUE 
            b-rcpth.po-line        = INTEGER(fg-rcpth.po-line:SCREEN-VALUE )
            b-rcpth.job-no         = fg-rcpth.job-no:SCREEN-VALUE 
            b-rcpth.job-no2        = INT(fg-rcpth.job-no2:SCREEN-VALUE )
            b-rcpth.trans-date     = DATE(fg-rcpth.trans-date:SCREEN-VALUE )
            b-rcpth.rita-code      = fg-rcpth.rita-code:SCREEN-VALUE 
            b-rcpth.update-by      = USERID("NOSWEAT")
            b-rdtlh.cust-no        = fg-rdtlh.cust-no:SCREEN-VALUE 
            b-rdtlh.loc            = fg-rdtlh.loc:SCREEN-VALUE 
            b-rdtlh.loc-bin        = fg-rdtlh.loc-bin:SCREEN-VALUE 
            b-rdtlh.qty            = DEC(fg-rdtlh.qty:SCREEN-VALUE )
            b-rdtlh.tot-wt         = DEC(fg-rdtlh.tot-wt:SCREEN-VALUE )
            b-rdtlh.tag            = fg-rdtlh.tag:SCREEN-VALUE 
            b-rdtlh.cost           = DEC(fg-rdtlh.cost:SCREEN-VALUE )
            b-rdtlh.qty-case       = INT(fg-rdtlh.qty-case:SCREEN-VALUE )
            b-rdtlh.partial        = INT(fg-rdtlh.partial:SCREEN-VALUE )
            b-rdtlh.cases          = INT(fg-rdtlh.cases:SCREEN-VALUE )
            b-rdtlh.stacks-unit    = INT(fg-rdtlh.stacks-unit:SCREEN-VALUE )
            b-rdtlh.stack-code     = fg-rdtlh.stack-code:SCREEN-VALUE 
            b-rcpth.pur-uom        = fg-rcpth.pur-uom:SCREEN-VALUE 
            b-rdtlh.rita-code      = b-rcpth.rita-code 
            b-rcpth.post-date      = DATE(fg-rcpth.post-date:SCREEN-VALUE )
            b-rdtlh.reject-code[1] = fg-rdtlh.reject-code[1]:SCREEN-VALUE 
            .

        FIND b-rcpth WHERE ROWID(b-rcpth) EQ ROWID(fg-rcpth) NO-LOCK NO-ERROR.
        FIND b-rdtlh WHERE ROWID(b-rdtlh) EQ ROWID(fg-rdtlh) NO-LOCK NO-ERROR.
   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no Dialog-Frame 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        fg-rdtlh.cust-no:SCREEN-VALUE  =
            CAPS(fg-rdtlh.cust-no:SCREEN-VALUE ).

        IF fg-rdtlh.cust-no:SCREEN-VALUE  NE "" AND
            NOT CAN-FIND(FIRST cust
            WHERE cust.company EQ itemfg.company
            AND cust.cust-no EQ fg-rdtlh.cust-no:SCREEN-VALUE )
            THEN 
        DO:
            MESSAGE "Invalid Customer#..." VIEW-AS ALERT-BOX ERROR.
            fg-rdtlh.cust-no:SCREEN-VALUE  = fg-rdtlh.cust-no.
            APPLY "entry" TO fg-rdtlh.cust-no .
            RETURN ERROR.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no Dialog-Frame 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-itemfg FOR itemfg.


    DO WITH FRAME {&FRAME-NAME}:
        fg-rcpth.i-no:SCREEN-VALUE  =
            CAPS(fg-rcpth.i-no:SCREEN-VALUE ).

        IF NOT CAN-FIND(FIRST b-itemfg
            WHERE b-itemfg.company EQ itemfg.company
            AND b-itemfg.i-no    EQ fg-rcpth.i-no:SCREEN-VALUE )
            THEN 
        DO:
            MESSAGE "Invalid FG Item#..." VIEW-AS ALERT-BOX ERROR.
            fg-rcpth.i-no:SCREEN-VALUE  = fg-rcpth.i-no.
            APPLY "entry" TO fg-rcpth.i-no .
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc Dialog-Frame 
PROCEDURE valid-loc :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        fg-rdtlh.loc:SCREEN-VALUE  =
            CAPS(fg-rdtlh.loc:SCREEN-VALUE ).

        IF NOT CAN-FIND(FIRST loc
            WHERE loc.company EQ itemfg.company
            AND loc.loc     EQ fg-rdtlh.loc:SCREEN-VALUE )
            THEN 
        DO:
            MESSAGE "Invalid Warehouse..." VIEW-AS ALERT-BOX ERROR.
            fg-rdtlh.loc:SCREEN-VALUE  = fg-rdtlh.loc.
            APPLY "entry" TO fg-rdtlh.loc .
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin Dialog-Frame 
PROCEDURE valid-loc-bin :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        fg-rdtlh.loc-bin:SCREEN-VALUE  =
            CAPS(fg-rdtlh.loc-bin:SCREEN-VALUE ).

        IF NOT CAN-FIND(FIRST fg-bin
            WHERE fg-bin.company EQ itemfg.company
            AND fg-bin.i-no    EQ ""
            AND fg-bin.loc     EQ fg-rdtlh.loc:SCREEN-VALUE 
            AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin:SCREEN-VALUE )
            THEN 
        DO:
            MESSAGE "Invalid Bin..." VIEW-AS ALERT-BOX ERROR.
            fg-rdtlh.loc-bin:SCREEN-VALUE  = fg-rdtlh.loc-bin.
            APPLY "entry" TO fg-rdtlh.loc-bin .
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag Dialog-Frame 
PROCEDURE valid-tag :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-fg-rdtlh FOR fg-rdtlh.

    DEFINE VARIABLE lv-tag LIKE fg-rdtlh.tag NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        lv-tag = fg-rdtlh.tag:SCREEN-VALUE .

        IF lv-tag NE fg-rdtlh.tag AND
            (lv-tag EQ ""       OR
            (NOT CAN-FIND(FIRST loadtag
            WHERE loadtag.company   EQ itemfg.company
            AND loadtag.item-type EQ NO
            AND loadtag.i-no      EQ itemfg.i-no
            AND loadtag.tag-no    EQ lv-tag)             AND
            NOT CAN-FIND(FIRST b-fg-rdtlh
            WHERE b-fg-rdtlh.company EQ itemfg.company
            AND b-fg-rdtlh.tag     EQ lv-tag
            AND ROWID(b-fg-rdtlh)  NE ROWID(fg-rdtlh)))) THEN 
        DO:
            MESSAGE "Invalid Tag# Change..." VIEW-AS ALERT-BOX ERROR.
            fg-rdtlh.tag:SCREEN-VALUE  = fg-rdtlh.tag.
            APPLY "entry" TO fg-rdtlh.tag .
            RETURN ERROR.
        END.



    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag-no Dialog-Frame 
PROCEDURE valid-tag-no :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-tag LIKE fg-rdtlh.tag NO-UNDO.
    DEFINE BUFFER b-fg-rdtlh FOR fg-rdtlh.

    DO WITH FRAME {&FRAME-NAME}:
        lv-tag = fg-rdtlh.tag:SCREEN-VALUE .

        IF fg-rdtlh.tag:SCREEN-VALUE  NE "" AND
            fg-rcpth.rita-code:SCREEN-VALUE  EQ "R" AND  
            int(fg-rdtlh.qty:SCREEN-VALUE ) GT 0 THEN 
        DO:

            FIND FIRST  b-fg-rdtlh NO-LOCK
                WHERE b-fg-rdtlh.company EQ cocode
                /*AND b-fg-rcpth.i-no EQ fi_rm-i-no*/
                AND b-fg-rdtlh.tag     EQ lv-tag 
                AND ROWID(b-fg-rdtlh)  NE ROWID(fg-rdtlh) NO-ERROR .

            IF AVAILABLE b-fg-rdtlh THEN 
            DO:
                MESSAGE "This Tag Number has already been used..." VIEW-AS ALERT-BOX INFORMATION.
                fg-rdtlh.tag:SCREEN-VALUE  = fg-rdtlh.tag.
                APPLY "entry" TO fg-rdtlh.tag .
                RETURN ERROR.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-reason Dialog-Frame 
PROCEDURE valid-reason :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  lCheckError = NO .
  DO WITH FRAME {&FRAME-NAME}:
   
    IF cComboList NE "" AND lAdjustReason-log AND (fg-rdtlh.reject-code[1]:SCREEN-VALUE  EQ "" OR fg-rdtlh.reject-code[1]:SCREEN-VALUE EQ ? ) 
        AND fg-rcpth.rita-code:SCREEN-VALUE EQ "A" THEN DO:
      MESSAGE "Please Enter , Adjustment Reason code..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fg-rdtlh.reject-code[1] .
      lCheckError = YES .
    END.
  
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-bol Dialog-Frame 
FUNCTION display-bol RETURNS INTEGER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    FIND FIRST oe-bolh
        WHERE oe-bolh.company EQ cocode
        AND oe-bolh.b-no    EQ fg-rcpth.b-no NO-LOCK NO-ERROR.


    IF AVAILABLE oe-bolh THEN
        RETURN INT(oe-bolh.bol-no)    .
    ELSE
        RETURN 0.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-ship Dialog-Frame 
FUNCTION display-ship RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    FIND FIRST oe-bolh
        WHERE oe-bolh.company EQ cocode
        AND oe-bolh.b-no    EQ fg-rcpth.b-no NO-LOCK NO-ERROR.

    IF AVAILABLE oe-bolh THEN 
    DO:
        RETURN STRING(oe-bolh.cust-no)    .
    END.
    ELSE
        RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-fg-qty Dialog-Frame 
FUNCTION get-fg-qty RETURNS INTEGER
    ( /* parameter-definitions */ INPUT ip-int AS INTEGER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iReturnVal  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBinQtyb    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBinQtya    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBinQtydeff AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth .
    DEFINE BUFFER bf-fg-rdtlh FOR fg-rdtlh .

    iBinQtyb = 0 .  

    IF fg-rcpth.rita-code = "C" THEN 
    DO:
        FOR EACH bf-fg-rcpth NO-LOCK
            WHERE bf-fg-rcpth.company EQ cocode 
            AND bf-fg-rcpth.i-no EQ fg-rcpth.i-no
            AND bf-fg-rcpth.job-no EQ fg-rcpth.job-no
            AND bf-fg-rcpth.job-no2 EQ fg-rcpth.job-no2
            AND bf-fg-rcpth.po-no EQ fg-rcpth.po-no
            AND bf-fg-rcpth.rita-code NE "C" ,
            EACH bf-fg-rdtlh NO-LOCK WHERE
            bf-fg-rdtlh.r-no EQ bf-fg-rcpth.r-no
            AND bf-fg-rdtlh.loc EQ fg-rdtlh.loc
            AND bf-fg-rdtlh.loc-bin EQ fg-rdtlh.loc-bin
            AND bf-fg-rdtlh.tag EQ fg-rdtlh.tag
            AND bf-fg-rdtlh.cust-no EQ fg-rdtlh.cust-no 
            AND bf-fg-rdtlh.bol-no EQ fg-rdtlh.bol-no
            AND bf-fg-rdtlh.inv-no EQ fg-rdtlh.inv-no BY fg-rcpth.trans-date DESCENDING :
            iBinQtyb = iBinQtyb +  bf-fg-rdtlh.qty  .
            LEAVE .
        END.

        ASSIGN 
            iBinQtya    = fg-rdtlh.qty  
            iBinQtydeff = iBinQtya - iBinQtyb .

        IF ip-int = 1 THEN
            iReturnVal = iBinQtyb .
        ELSE 
            iReturnVal = iBinQtydeff.

    END. /* fg-rcpth.rita-code = "C" */
    ELSE 
    DO:
        IF ip-int = 1 THEN
            iReturnVal = 0 .
        ELSE 
            iReturnVal = fg-rdtlh.qty .

    END.

    RETURN iReturnVal .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-pallet-info Dialog-Frame 
FUNCTION get-pallet-info RETURNS INTEGER
    (OUTPUT op-qty-pal AS INTEGER):
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ fg-rcpth.i-no
        AND fg-bin.job-no  EQ fg-rcpth.job-no
        AND fg-bin.job-no2 EQ fg-rcpth.job-no2
        AND fg-bin.loc     EQ fg-rdtlh.loc
        AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
        AND fg-bin.tag     EQ fg-rdtlh.tag
        AND fg-bin.cust-no EQ fg-rdtlh.cust-no
        NO-LOCK NO-ERROR.  

    /*if avail fg-bin then
      assign
       op-qty-pal = (if fg-bin.case-count   eq 0 then 1 else fg-bin.case-count)   *
                    (if fg-bin.cases-unit   eq 0 then 1 else fg-bin.cases-unit)   *
                    (if fg-bin.units-pallet eq 0 then 1 else fg-bin.units-pallet)
       /*li-pallets = fg-rdtlh.qty / op-qty-pal*/.
    
    else*/
    op-qty-pal = (IF fg-rdtlh.qty-case     NE 0 THEN fg-rdtlh.qty-case     ELSE
        IF AVAILABLE fg-bin AND
        fg-bin.case-count     NE 0 THEN fg-bin.case-count     ELSE 1) *
        (IF fg-rdtlh.stacks-unit  NE 0 THEN fg-rdtlh.stacks-unit  ELSE
        IF AVAILABLE fg-bin AND
        fg-bin.cases-unit     NE 0 THEN fg-bin.cases-unit     ELSE 1) *
        (IF fg-rdtlh.units-pallet NE 0 THEN fg-rdtlh.units-pallet ELSE
        IF AVAILABLE fg-bin AND
        fg-bin.units-pallet   NE 0 THEN fg-bin.units-pallet   ELSE 1).
    /*assign
     li-pallets = 1
     op-qty-pal = fg-rdtlh.qty.*/

    li-pallets = fg-rdtlh.qty / op-qty-pal.

    {sys/inc/roundup.i li-pallets}

    IF op-qty-pal LT 0 THEN
        ASSIGN
            op-qty-pal = op-qty-pal * -1
            li-pallets = li-pallets * -1.

    RETURN li-pallets.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-vend-info Dialog-Frame 
FUNCTION get-vend-info RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    IF AVAILABLE fg-rcpth THEN 
    DO:
        FIND FIRST vend
            WHERE vend.company EQ cocode
            AND vend.vend-no    EQ fg-rcpth.vend-no
            NO-LOCK NO-ERROR.  

        IF AVAILABLE vend THEN
            RETURN STRING(vend.NAME) .
        ELSE
            RETURN "" .
    END.
    ELSE RETURN "" .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-vend-no Dialog-Frame 
FUNCTION get-vend-no RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    FIND FIRST po-ord
        WHERE po-ord.company EQ cocode
        AND po-ord.po-no    EQ int(fg-rcpth.po-no)
        NO-LOCK NO-ERROR.  

    IF AVAILABLE po-ord THEN
        RETURN STRING(po-ord.vend-no) .
    ELSE
        RETURN "" .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

