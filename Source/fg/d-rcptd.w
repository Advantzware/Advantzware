&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: fg\d-rcptd.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO .   /* add,update,view */
DEFINE INPUT PARAMETER ip-set-parts AS LOGICAL NO-UNDO .
DEFINE INPUT PARAMETER ipcLinkerSet AS CHARACTER NO-UNDO .   
DEFINE OUTPUT PARAMETER op-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}
{sys/inc/var.i new shared}

ASSIGN 
    cocode = g_company
    locode = g_loc
    .
DEFINE VARIABLE lv-item-recid       AS RECID     NO-UNDO.
DEFINE VARIABLE ll-order-warned     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ll-new-record       AS LOGICAL   NO-UNDO.

DEFINE VARIABLE poSelected          AS INTEGER   NO-UNDO.
DEFINE VARIABLE ll-help-run         AS LOG       NO-UNDO.  /* set on browse help, reset row-entry */
DEFINE VARIABLE ls-prev-po          AS cha       NO-UNDO.
DEFINE VARIABLE lv-overrun-checked  AS LOG       NO-UNDO.
DEFINE VARIABLE lv-overrun2-checked AS LOG       NO-UNDO.
DEFINE VARIABLE lv-closed-checked   AS LOG       NO-UNDO.
DEFINE VARIABLE lv-job-no           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-job-no2          AS CHARACTER NO-UNDO.

DEFINE BUFFER b-fg-rctd    FOR fg-rctd.  /* for tag validation */
DEFINE BUFFER b-fg-rdtlh   FOR fg-rdtlh. /* for tag validation */
DEFINE BUFFER b2-fg-rdtlh  FOR fg-rdtlh. /* for tag validation */
DEFINE BUFFER b-tag-rctd   FOR fg-rctd .   /* task 11111306 */
DEFINE BUFFER reftable-job FOR reftable.
DEFINE BUFFER b-po-ord     FOR po-ord.
DEFINE BUFFER b-company    FOR company.

DEFINE VARIABLE lv-prev-job2        AS cha       NO-UNDO.
DEFINE VARIABLE lv-new-job-ran      AS LOG       NO-UNDO.
DEFINE VARIABLE ll-qty-case-ent     AS LOG       NO-UNDO.
DEFINE VARIABLE lv-num-rec          AS INTEGER   NO-UNDO.
DEFINE VARIABLE fg-uom-list         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-frst-rno         LIKE fg-rctd.r-no NO-UNDO.
DEFINE VARIABLE lv-rct-date-checked AS LOG       NO-UNDO.
DEFINE VARIABLE ll-set-parts        AS LOG       NO-UNDO.
DEFINE VARIABLE lv-linker           LIKE fg-rcpts.linker NO-UNDO.
DEFINE VARIABLE trans-time          AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-copy-mode         AS LOG       NO-UNDO.
DEFINE VARIABLE lrMissingRow        AS ROWID     NO-UNDO.
DEFINE VARIABLE gvcCurrentItem      AS CHARACTER NO-UNDO.
DEFINE VARIABLE hInventoryProcs     AS HANDLE    NO-UNDO.
DEFINE VARIABLE lMultipleAdds       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lUpdateRecords      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFatalQtyError      AS logical   NO-UNDO.

DEFINE TEMP-TABLE tt-fg-rctd LIKE fg-rctd
    FIELD tt-rowid AS ROWID
    FIELD po-rowid AS ROWID.

DEFINE BUFFER b-tt FOR tt-fg-rctd.
DEFINE TEMP-TABLE w-rowid 
    FIELD w-rowid AS CHARACTER
    INDEX w-rowid IS PRIMARY w-rowid.
{Inventory/ttInventory.i "NEW SHARED"}
{pc/pcprdd4u.i NEW}
{fg/invrecpt.i NEW}
{oe/d-selbin.i NEW}
{fg/fullset.i NEW}
{fg/d-selpos.i NEW}

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

DEFINE VARIABLE char-hdl            AS cha       NO-UNDO.   

/*&SCOPED-DEFINE item-key-phrase TRUE
&SCOPED-DEFINE init-proc init-proc*/

/*Globals for NK1 Settings*/
DEFINE VARIABLE glFGReceiptPassWord AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glFGSetAssembly     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glFGUnderOver       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcFGUnderOver       AS CHARACTER NO-UNDO.
DEFINE VARIABLE giFGUnderOver       AS INTEGER   NO-UNDO.
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
DEFINE VARIABLE gcPoBeforeChange    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllowUserOverRun   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lAccessClose        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cAccessList         AS CHARACTER NO-UNDO.
RUN pSetGlobalSettings(g_company).  /*Sets all of the above based on NK1 Settings*/

DEFINE VARIABLE hdCostProcs AS HANDLE.
RUN system\CostProcs.p PERSISTENT SET hdCostProcs.

RUN methods/prgsecur.p
            (INPUT "FGUnOvAllow",
             INPUT "ACCESS", /* based on run, create, update, delete or all */
             INPUT NO,    /* use the directory in addition to the program */
             INPUT NO,    /* Show a message if not authorized */
             INPUT NO,    /* Group overrides user security? */
             OUTPUT lAllowUserOverRun, /* Allowed? Yes/NO */
             OUTPUT lAccessClose, /* used in template/windows.i  */
             OUTPUT cAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */
             
DEFINE VARIABLE hdOrderProcs AS HANDLE NO-UNDO.
DEFINE VARIABLE hdJobProcs   AS HANDLE NO-UNDO.

RUN oe/OrderProcs.p PERSISTENT SET hdOrderProcs.
RUN jc/JobProcs.p   PERSISTENT SET hdJobProcs.
                          
DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound    AS LOGICAL   NO-UNDO.

RUN sys/ref/nk1look.p (
    INPUT cocode,           /* Company Code */ 
    INPUT "FGReceiptRules", /* sys-ctrl name */
    INPUT "I",              /* Output return value */
    INPUT NO,               /* Use ship-to */
    INPUT NO,               /* ship-to vendor */
    INPUT "",               /* ship-to vendor value */
    INPUT "",               /* shi-id value */
    OUTPUT cReturnValue, 
    OUTPUT lRecFound
    ). 
             

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fg-rctd

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame fg-rctd.rct-date fg-rctd.tag ~
fg-rctd.stack-code fg-rctd.po-no fg-rctd.po-line fg-rctd.job-no ~
fg-rctd.job-no2 fg-rctd.i-no fg-rctd.i-name fg-rctd.loc fg-rctd.loc-bin ~
fg-rctd.cases fg-rctd.qty-case fg-rctd.cases-unit fg-rctd.partial ~
fg-rctd.t-qty fg-rctd.tot-wt fg-rctd.std-cost fg-rctd.cost-uom ~
fg-rctd.frt-cost fg-rctd.ext-cost fg-rctd.created-by fg-rctd.updated-by ~
fg-rctd.r-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame fg-rctd.rct-date ~
fg-rctd.tag fg-rctd.stack-code fg-rctd.po-no fg-rctd.po-line fg-rctd.job-no ~
fg-rctd.job-no2 fg-rctd.i-no fg-rctd.loc fg-rctd.loc-bin fg-rctd.cases ~
fg-rctd.qty-case fg-rctd.cases-unit fg-rctd.partial fg-rctd.std-cost ~
fg-rctd.cost-uom fg-rctd.frt-cost fg-rctd.ext-cost 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame fg-rctd
&Scoped-define TABLES-IN-QUERY-Dialog-Frame fg-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame fg-rctd


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS fg-rctd.rct-date fg-rctd.tag ~
fg-rctd.stack-code fg-rctd.po-no fg-rctd.po-line fg-rctd.job-no ~
fg-rctd.job-no2 fg-rctd.i-no fg-rctd.loc fg-rctd.loc-bin fg-rctd.cases ~
fg-rctd.qty-case fg-rctd.cases-unit fg-rctd.partial fg-rctd.std-cost ~
fg-rctd.cost-uom fg-rctd.frt-cost fg-rctd.ext-cost 
&Scoped-define ENABLED-TABLES fg-rctd
&Scoped-define FIRST-ENABLED-TABLE fg-rctd
&Scoped-Define ENABLED-OBJECTS Btn_Cancel Btn_Done btnCalendar-1 Btn_OK 
&Scoped-Define DISPLAYED-FIELDS fg-rctd.rct-date fg-rctd.tag ~
fg-rctd.stack-code fg-rctd.po-no fg-rctd.po-line fg-rctd.job-no ~
fg-rctd.job-no2 fg-rctd.i-no fg-rctd.i-name fg-rctd.loc fg-rctd.loc-bin ~
fg-rctd.cases fg-rctd.qty-case fg-rctd.cases-unit fg-rctd.partial ~
fg-rctd.t-qty fg-rctd.tot-wt fg-rctd.std-cost fg-rctd.cost-uom ~
fg-rctd.frt-cost fg-rctd.ext-cost fg-rctd.created-by fg-rctd.updated-by ~
fg-rctd.r-no 
&Scoped-define DISPLAYED-TABLES fg-rctd
&Scoped-define FIRST-DISPLAYED-TABLE fg-rctd
&Scoped-Define DISPLAYED-OBJECTS cTransTime fi_tr-time 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD maxComponentQty Dialog-Frame 
FUNCTION maxComponentQty RETURNS DECIMAL
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
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Done" 
     SIZE 17 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE cTransTime AS CHARACTER FORMAT "x(8)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_tr-time AS CHARACTER FORMAT "X(15)":U 
     LABEL "Tr Time" 
     VIEW-AS FILL-IN 
     SIZE 23.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 51 BY 3.81
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 51 BY 5
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 51 BY 6.19
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 50 BY 3.81
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 50 BY 6.19
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 50 BY 5
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 19 BY 2.38
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      fg-rctd SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_Cancel AT ROW 17.19 COL 95
     cTransTime AT ROW 1.48 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     Btn_Done AT ROW 17.19 COL 86
     fg-rctd.rct-date AT ROW 1.48 COL 19 COLON-ALIGNED
          LABEL "Receipt Date" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 1
     btnCalendar-1 AT ROW 1.48 COL 35
     fg-rctd.tag AT ROW 2.67 COL 19 COLON-ALIGNED
          LABEL "Tag" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 31.2 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.stack-code AT ROW 3.86 COL 19 COLON-ALIGNED
          LABEL "FG Lot#" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 31.2 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.po-no AT ROW 1.48 COL 73 COLON-ALIGNED
          LABEL "PO #" FORMAT "x(9)"
          VIEW-AS FILL-IN 
          SIZE 23.8 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.po-line AT ROW 2.67 COL 73 COLON-ALIGNED
          LABEL "PO Ln#" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.job-no AT ROW 3.86 COL 73 COLON-ALIGNED
          LABEL "Job#" FORMAT "x(6)"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.job-no2 AT ROW 3.86 COL 92.2 COLON-ALIGNED NO-LABEL FORMAT "99"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.i-no AT ROW 5.52 COL 19 COLON-ALIGNED
          LABEL "Item No" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 31.2 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.i-name AT ROW 6.71 COL 19 COLON-ALIGNED
          LABEL "Name/Desc" FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 31.2 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.loc AT ROW 7.91 COL 19 COLON-ALIGNED
          LABEL "Warehouse" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.loc-bin AT ROW 9.1 COL 19 COLON-ALIGNED
          LABEL "Bin" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.cases AT ROW 5.52 COL 73 COLON-ALIGNED
          LABEL "Units" FORMAT "->>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.qty-case AT ROW 6.71 COL 73 COLON-ALIGNED
          LABEL "Unit Count" FORMAT ">>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.cases-unit AT ROW 7.91 COL 73 COLON-ALIGNED
          LABEL "Units/Skid" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.partial AT ROW 9.1 COL 73 COLON-ALIGNED
          LABEL "Partial" FORMAT "->>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.t-qty AT ROW 10.29 COL 73 COLON-ALIGNED
          LABEL "Total Qty" FORMAT "->>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.tot-wt AT ROW 15.52 COL 19 COLON-ALIGNED
          LABEL "Total Weight" FORMAT ">>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.std-cost AT ROW 10.76 COL 19 COLON-ALIGNED
          LABEL "Cost/UOM" FORMAT ">,>>>,>>9.99<<<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     fg-rctd.cost-uom AT ROW 11.95 COL 19 COLON-ALIGNED
          LABEL "UOM" FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 17.2 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.frt-cost AT ROW 13.14 COL 19 COLON-ALIGNED
          LABEL "Freight Cost" FORMAT ">>>,>>9.99<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.ext-cost AT ROW 14.33 COL 19 COLON-ALIGNED
          LABEL "Extended Cost" FORMAT "->,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fi_tr-time AT ROW 11.95 COL 73 COLON-ALIGNED NO-TAB-STOP 
     fg-rctd.created-by AT ROW 13.14 COL 73 COLON-ALIGNED
          LABEL "Created By" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 23.8 BY 1
          BGCOLOR 15 FONT 1 NO-TAB-STOP 
     fg-rctd.updated-by AT ROW 14.33 COL 73 COLON-ALIGNED
          LABEL "Last Updated By" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 23.8 BY 1
          BGCOLOR 15 FONT 1 NO-TAB-STOP 
     fg-rctd.r-no AT ROW 15.52 COL 73 COLON-ALIGNED
          LABEL "Seq#" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1 NO-TAB-STOP 
     Btn_OK AT ROW 17.19 COL 86
     RECT-7 AT ROW 16.95 COL 85
     RECT-2 AT ROW 5.29 COL 2
     RECT-1 AT ROW 1.24 COL 2 WIDGET-ID 2
     RECT-4 AT ROW 1.24 COL 54 WIDGET-ID 4
     RECT-3 AT ROW 10.52 COL 2 WIDGET-ID 6
     RECT-5 AT ROW 5.29 COL 54 WIDGET-ID 8
     RECT-6 AT ROW 11.71 COL 54 WIDGET-ID 10
     SPACE(0.00) SKIP(2.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Warehouse Transaction(Finished Goods) Update".


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
/* SETTINGS FOR FILL-IN fg-rctd.cases IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.cases-unit IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.cost-uom IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.created-by IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       fg-rctd.created-by:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN cTransTime IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fg-rctd.ext-cost IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fi_tr-time IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fi_tr-time:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fg-rctd.frt-cost IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.i-name IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN fg-rctd.i-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.job-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.job-no2 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.loc IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.loc-bin IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.partial IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.po-line IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.po-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.qty-case IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.r-no IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       fg-rctd.r-no:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fg-rctd.rct-date IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-7 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fg-rctd.stack-code IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.std-cost IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.t-qty IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN fg-rctd.tag IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.tot-wt IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN fg-rctd.updated-by IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       fg-rctd.updated-by:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.fg-rctd "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.fg-rctd.company eq cocode "
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Warehouse Transaction(Finished Goods) Update */
DO:
        DEFINE VARIABLE ll-tag#   AS LOG     NO-UNDO.
        DEFINE VARIABLE rec-val   AS RECID   NO-UNDO.
        DEFINE VARIABLE char-val  AS cha     NO-UNDO.
        DEFINE VARIABLE lv-cost   AS DECIMAL DECIMALS 4 NO-UNDO.
        DEFINE VARIABLE rRecidVal AS RECID   NO-UNDO.
        DO WITH FRAME {&FRAME-NAME}:
            ll-help-run = YES.
            CASE FOCUS:NAME:
                WHEN "po-no" THEN 
                    DO:
                        RUN windows/l-pofg.w (fg-rctd.company,fg-rctd.po-no:SCREEN-VALUE , OUTPUT char-val).
                        IF char-val <> "" THEN 
                        DO:
                            ASSIGN 
                                fg-rctd.po-no:SCREEN-VALUE   = ENTRY(1,char-val)
                                fg-rctd.i-no:SCREEN-VALUE    = ENTRY(2,char-val)
                                fg-rctd.i-name:SCREEN-VALUE  = ENTRY(3,char-val)
                                fg-rctd.job-no:SCREEN-VALUE  = ENTRY(4,char-val)
                                fg-rctd.job-no2:SCREEN-VALUE = ENTRY(5,char-val)
                                fg-rctd.po-line:SCREEN-VALUE = ENTRY(6,char-val)
                                .
                            RUN pDisplayPO(YES).
                            RUN pGetLocBin .

                            fg-rctd.ext-cost:SCREEN-VALUE  = "0".

                        END.  /* char-val <> "" */
                        RETURN NO-APPLY.   
                    END.
                WHEN "i-no" THEN 
                    DO:
                        IF fg-rctd.po-no:SCREEN-VALUE  <> "" THEN 
                        DO:
                            RUN windows/l-poitem.w (fg-rctd.company,fg-rctd.po-no:SCREEN-VALUE , FOCUS:SCREEN-VALUE , OUTPUT char-val).
                            IF char-val <> "" THEN 
                            DO :
                                ASSIGN 
                                    FOCUS:SCREEN-VALUE           = ENTRY(1,char-val)
                                    fg-rctd.i-name:screen-value  = ENTRY(2,char-val)
                                    fg-rctd.job-no:screen-value  = ENTRY(3,char-val)
                                    fg-rctd.job-no2:screen-value = ENTRY(4,char-val)
                                    fg-rctd.po-line:screen-value = ENTRY(6,char-val)
                                    .
                                RUN pDisplayPO(YES).
                            END.
                        END.
                        ELSE IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN 
                            DO:
                                RUN windows/l-jobit1.w (fg-rctd.company,fg-rctd.job-no:SCREEN-VALUE,fg-rctd.job-no2:screen-value, FOCUS:SCREEN-VALUE, OUTPUT char-val,OUTPUT rec-val).
                                IF char-val <> ""  THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val). 
                                IF rec-val <> ? THEN 
                                DO:
                                    FIND tt-job-hdr WHERE RECID(tt-job-hdr) = rec-val NO-LOCK NO-ERROR.

                                    IF AVAILABLE tt-job-hdr THEN 
                                        ASSIGN fg-rctd.std-cost:SCREEN-VALUE = STRING(tt-job-hdr.std-mat-cost +
                                                              tt-job-hdr.std-lab-cost +
                                                              tt-job-hdr.std-fix-cost +
                                                              tt-job-hdr.std-var-cost)
                                            .

                                END.

                            END.
                            ELSE 
                            DO:
                                RUN windows/l-itemf2.w (fg-rctd.company, "", fg-rctd.i-no:SCREEN-VALUE,"", OUTPUT char-val, OUTPUT rec-val).
                                IF rec-val <> ? THEN 
                                DO:
                                    FIND itemfg WHERE RECID(itemfg) = rec-val NO-LOCK.
                                    RUN pDisplayFG(YES,BUFFER itemfg).

                                END.
                            END.
                        RETURN NO-APPLY.   
                    END.
                WHEN "job-no" /*or when "job-no2" */ THEN 
                    DO:
                        RUN windows/l-jobno.w (fg-rctd.company, FOCUS:SCREEN-VALUE,OUTPUT char-val, OUTPUT rec-val).
                        IF char-val <> "" THEN do:
                            ASSIGN 
                                fg-rctd.job-no:screen-value  = ENTRY(1,char-val)
                                fg-rctd.job-no2:screen-value = ENTRY(2,char-val)
                                fg-rctd.i-no:SCREEN-VALUE    = ENTRY(3,char-val)
                                .
                            RUN  pGetUnassembledItem(cocode , ENTRY(3,char-val)) .
                        END.
                        IF rec-val <> ? THEN 
                        DO:
                            FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.

                            IF AVAILABLE job-hdr THEN 
                                fg-rctd.std-cost:SCREEN-VALUE = STRING(job-hdr.std-mat-cost +
                                    job-hdr.std-lab-cost +
                                    job-hdr.std-fix-cost +
                                    job-hdr.std-var-cost)
                                    .
                        END.
                        FIND FIRST itemfg WHERE itemfg.company = g_company
                            AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
                        IF AVAILABLE ITEMfg THEN
                            ASSIGN fg-rctd.i-name:SCREEN-VALUE   = itemfg.i-name
                                fg-rctd.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .

                        RETURN NO-APPLY.   
                    END.  
                WHEN "job-no2" THEN 
                    DO:
                        RUN windows/l-jobno2.w (fg-rctd.company, fg-rctd.job-no:screen-value,FOCUS:SCREEN-VALUE,OUTPUT char-val, OUTPUT rec-val).
                        IF char-val <> "" THEN
                            ASSIGN /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                       fg-rctd.job-no:screen-value = entry(1,char-val) */
                                fg-rctd.job-no2:screen-value = ENTRY(2,char-val)
                                fg-rctd.i-no:SCREEN-VALUE    = ENTRY(3,char-val)
                                .
                        IF rec-val <> ? THEN 
                        DO:
                            FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.

                            IF AVAILABLE job-hdr THEN 
                                fg-rctd.std-cost:SCREEN-VALUE = STRING(job-hdr.std-mat-cost +
                                    job-hdr.std-lab-cost +
                                    job-hdr.std-fix-cost +
                                    job-hdr.std-var-cost)
                                    .
                        END.
                        FIND itemfg WHERE itemfg.company = g_company
                            AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
                        IF AVAILABLE ITEMfg THEN
                            ASSIGN fg-rctd.i-name:SCREEN-VALUE   = itemfg.i-name
                                fg-rctd.cost-uom:SCREEN-VALUE = itemfg.prod-uom  .
                        RETURN NO-APPLY.   
                    END.  
                WHEN "loc" THEN 
                    DO:
                        RUN windows/l-loc.w (fg-rctd.company,FOCUS:SCREEN-VALUE, OUTPUT char-val).
                        IF char-val <> "" THEN 
                        DO :
                            ASSIGN 
                                FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                .

                        END.
                        RETURN NO-APPLY.   
                    END.
                WHEN "loc-bin" THEN 
                    DO:
                        RUN windows/l-fgbin.w (fg-rctd.company,fg-rctd.loc:screen-value, fg-rctd.loc-bin:screen-value,OUTPUT char-val).
                        IF char-val <> "" THEN 
                        DO :
                            ASSIGN 
                                FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                /*fg-rctd.loc:screen-value = entry(2,char-val)
                                 fg-rctd.tag:screen-value = entry(4,char-val)*/
                                .

                        END.
                        RETURN NO-APPLY.   
                    END.  
                WHEN "tag" THEN 
                    DO:  /* task 11111306 */
                        IF ip-set-parts THEN 
                        DO:
                            DEFINE VARIABLE lv-all-or-one AS cha       NO-UNDO.
                            DEFINE VARIABLE lchk          AS LOG       NO-UNDO.
                            DEFINE VARIABLE fg-item-name  AS CHARACTER NO-UNDO.
                 
                            /*IF relmerge-int NE 0 THEN
                               MESSAGE "Select Bins for All Jobs?" VIEW-AS ALERT-BOX  
                                QUESTION BUTTON YES-NO UPDATE lchk .
          
                           lv-all-or-one = IF lchk THEN "ALL" ELSE "ONE".*/
                            lv-all-or-one = "ALL" .
                   
                            FIND FIRST b-tag-rctd WHERE  b-tag-rctd.company EQ cocode AND 
                                b-tag-rctd.r-no EQ int(SUBSTRING(reftable.dscr,9,35)) AND 
                                (b-tag-rctd.rita-code EQ "R" OR b-tag-rctd.rita-code EQ "E")  NO-LOCK NO-ERROR.
                
                            IF AVAILABLE b-tag-rctd THEN ASSIGN fg-item-name = b-tag-rctd.i-no .
                            ELSE fg-item-name = ""  .
                        
                            RUN oe/l-tagnew.w (6, ROWID(fg-rctd), lv-all-or-one,fg-rctd.i-no:screen-value ,fg-rctd.job-no:screen-value,fg-rctd.job-no2:screen-value,
                                lv-linker, OUTPUT char-val).
                            IF char-val <> "" THEN 
                            DO :
                                ASSIGN 
                                    fg-rctd.tag:SCREEN-VALUE     = ENTRY(1,char-val)
                                    fg-rctd.loc:SCREEN-VALUE     = ENTRY(2,char-val)
                                    fg-rctd.loc-bin:SCREEN-VALUE = ENTRY(3,char-val) .
                            END.
                        END. /* If ip-parts-set */
                        ELSE 
                        DO:
                            RUN windows/l-ldtag.w (INPUT cocode,
                                INPUT FALSE,
                                INPUT fg-rctd.tag:screen-value,
                                OUTPUT char-val,
                                OUTPUT rRecidVal).
                            IF char-val NE "" THEN ASSIGN 
                                fg-rctd.tag:SCREEN-VALUE = ENTRY(1,char-val).
                                                         
                        END.
                    END.   /* task 11111306 */
            END CASE.
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Warehouse Transaction(Finished Goods) Update */
ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Warehouse Transaction(Finished Goods) Update */
DO:
        DISABLE TRIGGERS FOR LOAD OF fg-rctd .
    
        IF AVAILABLE fg-rctd THEN
            op-rowid = ROWID(fg-rctd) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST fg-rctd EXCLUSIVE-LOCK
                WHERE RECID(fg-rctd) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE fg-rctd THEN DELETE fg-rctd .
            op-rowid = ? .
        END.
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.

    /*APPLY "END-ERROR":U TO SELF.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 Dialog-Frame
ON CHOOSE OF btnCalendar-1 IN FRAME Dialog-Frame
DO:
        {methods/btnCalendar.i fg-rctd.rct-date}
        APPLY "entry" TO fg-rctd.rct-date .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
        DISABLE TRIGGERS FOR LOAD OF fg-rctd .
    
        IF AVAILABLE fg-rctd THEN
            op-rowid = ROWID(fg-rctd) .

        IF lv-item-recid NE ? 
        OR lFatalQtyError THEN 
        DO:
            IF AVAIL fg-rctd THEN DO:
                FIND CURRENT fg-rctd EXCLUSIVE.
                DELETE fg-rctd .
            END.
            op-rowid = ? .
        END.
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
DO:
        IF AVAILABLE fg-rctd THEN
            ASSIGN op-rowid = ROWID(fg-rctd) .
        
        IF lv-item-recid NE ? 
        OR lFatalQtyError THEN 
        DO:
            IF AVAIL fg-rctd THEN 
            DO:
                FIND CURRENT fg-rctd EXCLUSIVE.
                DELETE fg-rctd .
            END.
            op-rowid = ? .
        END.
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
        DEFINE BUFFER bf-rctd     FOR fg-rctd .
        DEFINE BUFFER bf-reftable FOR reftable .
        DEFINE VARIABLE v-tag-change  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE v-tag-change2 AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cval          AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lNegative     AS LOG       NO-UNDO.
        DEFINE VARIABLE lNewTagRan    AS LOG       NO-UNDO.
        DEFINE VARIABLE hPanel        AS HANDLE    NO-UNDO.
        DEFINE VARIABLE ls-tmp-qty    AS cha       NO-UNDO.
        DEFINE VARIABLE ls-tmp-uom    AS cha       NO-UNDO.
        DEFINE VARIABLE ls-tmp-cst    AS cha       NO-UNDO.
        DEFINE VARIABLE v-next-tag    AS cha       NO-UNDO.
        DEFINE VARIABLE v-full-qty    AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE lQtyChanged   AS LOG       NO-UNDO.
        DEFINE VARIABLE lOK           AS LOG       NO-UNDO.
        DEFINE VARIABLE iLinker       AS INTEGER   NO-UNDO.

        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
   
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN
                v-copy-mode = NO
                ls-tmp-qty  = fg-rctd.t-qty:SCREEN-VALUE 
                ls-tmp-uom  = fg-rctd.cost-uom:SCREEN-VALUE 
                ls-tmp-cst  = fg-rctd.ext-cost:SCREEN-VALUE .

            ASSIGN lUpdateRecords = YES.

            v-tag-change = fg-rctd.tag:SCREEN-VALUE  .
            IF NOT lv-rct-date-checked THEN 
            DO:
                DEFINE VARIABLE lv-rct-date AS cha NO-UNDO.
                lv-rct-date = fg-rctd.rct-date:SCREEN-VALUE .
                {custom/currentDatePrompt.i lv-rct-date}
            END.

            IF fg-rctd.tag:SCREEN-VALUE  NE fg-rctd.tag 
                AND NOT adm-new-record THEN 
            DO:
                /* Note: if adm-new-record, tag will always be different, */
                /*       and cases will be overriden                      */
                RUN new-tag.
                lNewTagRan = TRUE.
            END.
      
            IF fg-rctd.loc:SCREEN-VALUE      EQ ""      OR
                INT(fg-rctd.qty-case:SCREEN-VALUE ) EQ 0 OR
                fg-rctd.cost-uom:SCREEN-VALUE  EQ ""     OR
                DEC(fg-rctd.std-cost:SCREEN-VALUE ) EQ 0 THEN
                RUN get-values.
    

            RUN get-matrix (NO).
        END.
        RUN valid-tag (fg-rctd.tag:HANDLE, OUTPUT lNegative) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        IF lNegative AND lNewTagRan THEN 
        DO:
            IF lNegative AND INTEGER(fg-rctd.cases:SCREEN-VALUE ) GT 0 THEN
                fg-rctd.cases:SCREEN-VALUE  = 
                    STRING(INTEGER(fg-rctd.cases:SCREEN-VALUE ) * -1).
        END.

        RUN valid-lot# (fg-rctd.stack-code:HANDLE) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-po-no (1) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        IF lRecFound AND INTEGER(cReturnValue) EQ 1 THEN DO:       
            RUN CheckPOLineStatus IN hdOrderProcs(
                INPUT cocode,
                INPUT INTEGER(fg-rctd.po-no:SCREEN-VALUE),
                INPUT INTEGER(fg-rctd.po-line:SCREEN-VALUE)
                ) NO-ERROR.
             
            IF ERROR-STATUS:ERROR THEN DO:
                APPLY "ENTRY":U TO fg-rctd.po-no.
                RETURN NO-APPLY.
            END.    
        END.            

        IF NOT CAN-FIND(FIRST tt-fg-rctd WHERE tt-rowid EQ ROWID(fg-rctd)) THEN
            RUN update-ttt.
  
        IF ip-set-parts THEN 
        DO: 
            RUN valid-qty (fg-rctd.t-qty:HANDLE) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
  

        /* Run with check on password, if relevant */
        RUN non-blank-job-po NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO: 
            RETURN NO-APPLY.
        END.
    
        RUN valid-job-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   
        IF lRecFound AND INTEGER(cReturnValue) EQ 1 THEN DO:         
            RUN CheckJobStatus IN hdJobProcs(
                INPUT cocode,
                INPUT fg-rctd.job-no:SCREEN-VALUE,
                INPUT INTEGER(fg-rctd.job-no2:SCREEN-VALUE)
                ) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                APPLY "ENTRY":U TO fg-rctd.job-no2.
                RETURN NO-APPLY. 
            END.    
        END.   
        
        RUN valid-job-no2 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        IF lv-prev-job2 <> fg-rctd.job-no2:SCREEN-VALUE  AND
            NOT lv-new-job-ran THEN RUN new-job-no.

        IF adm-new-record THEN 
        DO:

            /* Instead of 0 at 500 with -300 partial, make it */
            /* 1 at -500 with 0 partial 03101503              */
            IF INT(fg-rctd.cases:SCREEN-VALUE) EQ 0 AND INT(fg-rctd.partial:SCREEN-VALUE) NE 0 THEN
                ASSIGN 
                    fg-rctd.cases:SCREEN-VALUE    = STRING((IF  INT(fg-rctd.partial:SCREEN-VALUE) LT 0 THEN -1 ELSE 1))
                    fg-rctd.qty-case:SCREEN-VALUE = STRING((IF INT(fg-rctd.partial:SCREEN-VALUE) LT 0 THEN - INT(fg-rctd.partial:SCREEN-VALUE) ELSE INT(fg-rctd.partial:SCREEN-VALUE)))
                    fg-rctd.partial:SCREEN-VALUE  = "0"
                    .
        END.
        RUN valid-blank-qty NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.


        RUN validate-record NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:   
            ASSIGN 
                lv-rct-date-checked = FALSE.
            RUN delete-tt.
            IF lFatalQtyError THEN DO:
                APPLY 'choose' TO btn_Cancel.
            END.
            RETURN NO-APPLY. 
        END.
        
        /* Needed since the fg-rctd can become unavailable for some reason */
        lrMissingRow = ?.
        IF NOT AVAILABLE fg-rctd AND INTEGER(fg-rctd.r-no:SCREEN-VALUE ) GT 0 THEN 
        DO:
            FIND FIRST fg-rctd
                WHERE fg-rctd.r-no EQ INTEGER(fg-rctd.r-no:SCREEN-VALUE ) 
                NO-LOCK NO-ERROR.
            IF AVAILABLE fg-rctd THEN
                lrMissingRow = ROWID(fg-rctd).
        END.
        ASSIGN 
            v-tag-change2 = fg-rctd.tag  .

  
        DO TRANSACTION:
            FIND CURRENT fg-rctd EXCLUSIVE-LOCK NO-ERROR.

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
            END.
        END.

        /* Code placed here will execute AFTER standard behavior.    */                
        IF DEC(ls-tmp-qty) NE fg-rctd.t-qty THEN lQtyChanged = YES.
        ASSIGN
            fg-rctd.t-qty     = DEC(ls-tmp-qty)
            fg-rctd.pur-uom   = ls-tmp-uom
            fg-rctd.cost-uom  = ls-tmp-uom                    
            fg-rctd.ext-cost  = fg-rctd.std-cost * fg-rctd.t-qty /
                      (IF fg-rctd.cost-uom EQ "M" THEN 1000 ELSE 1).
        IF fg-rctd.po-no GT "" THEN 
        DO:
            FIND FIRST po-ord WHERE po-ord.company EQ fg-rctd.company
                AND po-ord.po-no EQ INTEGER(fg-rctd.po-no)
                NO-LOCK NO-ERROR.
            IF AVAILABLE po-ord THEN
                FIND FIRST po-ordl WHERE po-ordl.company EQ po-ord.company
                    AND po-ordl.po-no EQ po-ord.po-no
                    AND po-ordl.i-no  EQ fg-rctd.i-no 
                    NO-LOCK NO-ERROR.
            DEFINE BUFFER bfItemfg FOR itemfg.
            IF AVAILABLE po-ordl THEN
                FIND FIRST itemfg WHERE itemfg.company EQ po-ordl.company
                    AND itemfg.i-no EQ po-ordl.i-no 
                    NO-LOCK NO-ERROR.
     
            IF AVAILABLE itemfg AND itemfg.pur-man = TRUE AND AVAIL(po-ordl) AND po-ordl.job-no GT "" THEN
                ASSIGN fg-rctd.job-no  = po-ordl.job-no
                    fg-rctd.job-no2 = po-ordl.job-no2.

      
        END.
        IF ip-set-parts THEN 
        DO:
            FIND FIRST fg-rcpts WHERE fg-rcpts.r-no EQ fg-rctd.r-no NO-ERROR.
            IF NOT AVAILABLE fg-rcpts THEN 
            DO:
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

        ELSE 
        DO: 
            FIND FIRST itemfg WHERE itemfg.company EQ fg-rctd.company
                AND itemfg.i-no EQ fg-rctd.i-no 
                NO-LOCK NO-ERROR.
            IF lQtyChanged AND CAN-FIND(FIRST fg-rcpts
                WHERE fg-rcpts.company EQ cocode
                AND fg-rcpts.linker  EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")) 
                AND NOT (giFGSetRec EQ 1 AND itemfg.alloc NE YES) THEN 
            DO:
                MESSAGE "Set Parts Receipts will be reset since the set header quantity was changed. Please review the Set Parts tab."
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                RUN DeleteSetParts (INPUT ("fg-rctd: " + STRING(fg-rctd.r-no,"9999999999"))).
            END.
            IF NOT (giFGSetRec EQ 1 AND itemfg.alloc NE YES) THEN
                RUN fg/comprcpt.p (ROWID(fg-rctd)).
        END.
      
  
        IF glFGPOTag# AND fg-rctd.tag EQ "" THEN 
        DO:
            RUN get-next-tag (INPUT fg-rctd.i-no, OUTPUT v-next-tag).
            RUN create-loadtag (INPUT-OUTPUT v-next-tag, INPUT ROWID(fg-rctd)).
            fg-rctd.tag = v-next-tag.
        END.



        RUN delete-tt.

        ASSIGN 
            lv-new-job-ran = NO
            lv-prev-job2   = ""
            lUpdateRecords = NO .


        IF NOT ip-set-parts THEN RUN fg/invrecpt.p (ROWID(fg-rctd), 1).

        FIND CURRENT fg-rctd NO-LOCK NO-ERROR .
        op-rowid = ROWID(fg-rctd).

        APPLY "go" TO FRAME {&FRAME-NAME}.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases Dialog-Frame
ON LEAVE OF fg-rctd.cases IN FRAME Dialog-Frame /* Units */
DO:
        /* If it's in cases, needs to validate before continuing */
        IF LASTKEY = -1 AND NOT FOCUS:NAME EQ "cases" THEN RETURN .
        RUN new-qty.
 
        RUN valid-cases (FOCUS) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cases-unit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases-unit Dialog-Frame
ON LEAVE OF fg-rctd.cases-unit IN FRAME Dialog-Frame /* Units/Skid */
DO:
        RUN new-qty.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cost-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cost-uom Dialog-Frame
ON ENTRY OF fg-rctd.cost-uom IN FRAME Dialog-Frame /* UOM */
DO:
        IF LASTKEY NE -1                                                      AND
            (ip-set-parts OR
            TRIM(fg-rctd.job-no:SCREEN-VALUE ) NE "") THEN 
        DO:
            IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
                APPLY "back-tab" TO {&self-name} .
            ELSE
                APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cost-uom Dialog-Frame
ON LEAVE OF fg-rctd.cost-uom IN FRAME Dialog-Frame /* UOM */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-uom NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            RUN get-matrix (NO).
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.frt-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.frt-cost Dialog-Frame
ON ENTRY OF fg-rctd.frt-cost IN FRAME Dialog-Frame /* Freight Cost */
DO:
        IF LASTKEY NE -1                                                      AND
            (ip-set-parts OR
            TRIM(fg-rctd.job-no:SCREEN-VALUE ) NE "") THEN 
        DO:
            IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
                APPLY "back-tab" TO {&self-name} .
            ELSE
                APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.frt-cost Dialog-Frame
ON LEAVE OF fg-rctd.frt-cost IN FRAME Dialog-Frame /* Freight Cost */
DO:
        RUN get-matrix (NO).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-name Dialog-Frame
ON ENTRY OF fg-rctd.i-name IN FRAME Dialog-Frame /* Name/Desc */
DO:
        IF LASTKEY NE -1 AND ip-set-parts THEN 
        DO:
            IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
                APPLY "back-tab" TO {&self-name} .
            ELSE
                APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Dialog-Frame
ON ENTRY OF fg-rctd.i-no IN FRAME Dialog-Frame /* Item No */
DO:
        IF LASTKEY NE -1 AND ip-set-parts THEN 
        DO:
            IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
                APPLY "back-tab" TO {&self-name} .
            ELSE
                APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Dialog-Frame
ON LEAVE OF fg-rctd.i-no IN FRAME Dialog-Frame /* Item No */
DO:
        IF LASTKEY = -1 THEN RETURN.

        IF INT(fg-rctd.po-no:SCREEN-VALUE ) NE 0 AND
            fg-rctd.i-no:SCREEN-VALUE  NE ""      THEN 
        DO:
            RUN valid-po-no (0) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
                MESSAGE "FG does not exist on PO..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO fg-rctd.i-no .
                RETURN NO-APPLY.
            END.
        END.

        FIND FIRST itemfg {sys/look/itemfgrlW.i}
            AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE 
        NO-LOCK NO-ERROR.
        IF NOT AVAILABLE itemfg THEN 
        DO:
            IF fg-rctd.i-no:SCREEN-VALUE = "" THEN 
            DO:
                MESSAGE "Invalid Item. Try help. " VIEW-AS ALERT-BOX.
                APPLY "entry" TO fg-rctd.i-no .
                RETURN NO-APPLY.
            END.
            ELSE 
            DO:
                MESSAGE  " F/G Item is not on file.  Would you like to add it? "
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
                IF NOT ll-ans THEN 
                DO:
                    APPLY "entry" TO fg-rctd.i-no .
                    RETURN NO-APPLY.           
                END.
                ELSE 
                DO:
                    RUN fg/d-crtitm.w (SELF:SCREEN-VALUE) .
                    FIND FIRST itemfg {sys/look/itemfgrlW.i}
                        AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE 
                    NO-LOCK NO-ERROR.
                END.
            END.
        END.
                
        RUN valid-i-no (FOCUS) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        /*IF SELF:MODIFIED THEN*/ 
        IF (adm-new-record OR adm-adding-record) AND fg-rctd.job-no:SCREEN-VALUE EQ "" THEN
            RUN get-def-values.
        IF fg-rctd.partial:SCREEN-VALUE  EQ ? 
            OR fg-rctd.partial:SCREEN-VALUE  EQ "?" THEN
            fg-rctd.partial:SCREEN-VALUE  = "0".

        IF fg-rctd.cases:SCREEN-VALUE  EQ ? 
            OR fg-rctd.cases:SCREEN-VALUE  EQ "?" THEN
            fg-rctd.cases:SCREEN-VALUE  = "0".
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Dialog-Frame
ON VALUE-CHANGED OF fg-rctd.i-no IN FRAME Dialog-Frame /* Item No */
DO:

    RUN pGetLocBin . 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no Dialog-Frame
ON ENTRY OF fg-rctd.job-no IN FRAME Dialog-Frame /* Job# */
DO:
        lv-job-no = fg-rctd.job-no:SCREEN-VALUE .

        IF LASTKEY NE -1 AND ip-set-parts THEN 
        DO:
            IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
                APPLY "back-tab" TO {&self-name} .
            ELSE
                APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.

        IF fg-rctd.po-no:SCREEN-VALUE  NE "" THEN 
        DO:
            IF CAN-FIND(FIRST tt-fg-rctd WHERE tt-fg-rctd.tt-rowid EQ ROWID(fg-rctd)) THEN
                RUN update-tt.
            ELSE
                APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no Dialog-Frame
ON LEAVE OF fg-rctd.job-no IN FRAME Dialog-Frame /* Job# */
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            IF glFGRecpt AND NOT glFGReceiptPassWord THEN
                RUN non-blank-job-po.
            IF ERROR-STATUS:ERROR THEN
                RETURN.
          
            RUN valid-job-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
                RETURN NO-APPLY.
        END.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Dialog-Frame
ON ENTRY OF fg-rctd.job-no2 IN FRAME Dialog-Frame
DO:
        lv-job-no = fg-rctd.job-no:SCREEN-VALUE .
        lv-prev-job2 =  SELF:SCREEN-VALUE.

        IF LASTKEY NE -1 AND ip-set-parts THEN 
        DO:
            IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
                APPLY "back-tab" TO {&self-name} .
            ELSE
                APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
        IF fg-rctd.po-no:SCREEN-VALUE  NE "" THEN 
        DO:
            IF CAN-FIND(FIRST tt-fg-rctd WHERE tt-fg-rctd.tt-rowid EQ ROWID(fg-rctd)) THEN
                RUN update-tt.
            ELSE
                APPLY "tab" TO {&self-name}.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Dialog-Frame
ON LEAVE OF fg-rctd.job-no2 IN FRAME Dialog-Frame
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF lRecFound AND INTEGER(cReturnValue) EQ 1 THEN DO:         
                RUN CheckJobStatus IN hdJobProcs(
                    INPUT cocode,
                    INPUT fg-rctd.job-no:SCREEN-VALUE,
                    INPUT INTEGER(fg-rctd.job-no2:SCREEN-VALUE)
                    ) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    RETURN NO-APPLY. 
            END.
            
            RUN valid-job-no2 NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            IF lv-prev-job2 <> fg-rctd.job-no2:SCREEN-VALUE  THEN 
            DO:
                RUN new-job-no.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Dialog-Frame
ON VALUE-CHANGED OF fg-rctd.job-no2 IN FRAME Dialog-Frame
DO:
    /*RUN new-job-no.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc Dialog-Frame
ON LEAVE OF fg-rctd.loc IN FRAME Dialog-Frame /* Warehouse */
DO:
        DEFINE VARIABLE lActiveBin AS LOGICAL NO-UNDO.
        IF LASTKEY = -1 THEN RETURN.

        IF SELF:MODIFIED THEN 
        DO:
            RUN ValidateLoc IN hInventoryProcs (cocode, fg-rctd.loc:SCREEN-VALUE , OUTPUT lActiveBin).
            IF NOT lActiveBin THEN 
            DO:
                MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin Dialog-Frame
ON LEAVE OF fg-rctd.loc-bin IN FRAME Dialog-Frame /* Bin */
DO:
        DEFINE VARIABLE lActiveBin AS LOGICAL NO-UNDO.
        IF LASTKEY = -1 THEN RETURN .

        IF SELF:MODIFIED THEN 
        DO:
            RUN ValidateBin IN hInventoryProcs (cocode, fg-rctd.loc:SCREEN-VALUE , 
                fg-rctd.loc-bin:SCREEN-VALUE , 
                OUTPUT lActiveBin ).
            IF NOT lActiveBin THEN 
            DO:
                MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.partial Dialog-Frame
ON VALUE-CHANGED OF fg-rctd.partial IN FRAME Dialog-Frame /* Partial */
DO:
        RUN new-qty.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.po-line
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.po-line Dialog-Frame
ON ENTRY OF fg-rctd.po-line IN FRAME Dialog-Frame /* PO Ln# */
DO:
    
        IF LASTKEY NE -1                                                  AND
            (ip-set-parts                                               OR
            fg-rctd.job-no:SCREEN-VALUE  NE "" OR
            fg-rctd.rita-code EQ "E")                                     THEN 
        DO:
            IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
                APPLY "back-tab" TO {&self-name} .
            ELSE
                APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.po-line
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.po-line Dialog-Frame
ON VALUE-CHANGED OF fg-rctd.po-line IN FRAME Dialog-Frame /* PO Ln# */
DO:
    
        IF LASTKEY NE -1  THEN  DO:
          IF SELF:modified AND SELF:screen-value <> "" THEN DO:          
            FIND FIRST po-ordl NO-LOCK
                WHERE po-ord.company EQ cocode
                  AND po-ordl.po-no EQ INTEGER(fg-rctd.po-no:screen-value)
                  AND po-ordl.LINE EQ INTEGER(fg-rctd.po-line:SCREEN-VALUE)
                 NO-ERROR.
             IF AVAIL po-ordl THEN
             DO:  
              ASSIGN 
                
                fg-rctd.i-no:SCREEN-VALUE    = po-ordl.i-no  
                fg-rctd.i-name:SCREEN-VALUE  = po-ordl.i-name
                fg-rctd.job-no:SCREEN-VALUE  = po-ordl.job-no
                fg-rctd.job-no2:SCREEN-VALUE = string(po-ordl.job-no2) .
               RUN pDisplayPO(YES).   
               RUN pGetLocBin .
             END.
          END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME fg-rctd.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.po-no Dialog-Frame
ON ENTRY OF fg-rctd.po-no IN FRAME Dialog-Frame /* PO # */
DO:
        gcPoBeforeChange = fg-rctd.po-no:SCREEN-VALUE  .
        IF LASTKEY NE -1                                                  AND
            (ip-set-parts                                               OR
            fg-rctd.job-no:SCREEN-VALUE  NE "" OR
            fg-rctd.rita-code EQ "E")                                     THEN 
        DO:
            IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
                APPLY "back-tab" TO {&self-name} .
            ELSE
                APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.po-no Dialog-Frame
ON LEAVE OF fg-rctd.po-no IN FRAME Dialog-Frame /* PO # */
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            IF INT({&self-name}:SCREEN-VALUE ) EQ 0 THEN
                {&self-name}:SCREEN-VALUE  = "". 
            IF {&self-name}:SCREEN-VALUE  NE gcPoBeforeChange THEN 
            DO:
                FIND FIRST po-ordl
                    WHERE po-ordl.company   EQ fg-rctd.company
                    AND po-ordl.po-no     EQ INT({&self-name}:SCREEN-VALUE )
                    AND po-ordl.item-type EQ NO
                    AND po-ordl.i-no      EQ fg-rctd.i-no:SCREEN-VALUE 
                    AND po-ordl.line      EQ INT(fg-rctd.po-line:SCREEN-VALUE )
                    NO-LOCK NO-ERROR.

                IF NOT AVAILABLE po-ordl THEN
                    FIND FIRST po-ordl
                        WHERE po-ordl.company   EQ fg-rctd.company
                        AND po-ordl.po-no     EQ INT({&self-name}:SCREEN-VALUE )
                        AND po-ordl.item-type EQ NO
                        AND po-ordl.i-no      EQ fg-rctd.i-no:SCREEN-VALUE 
                        NO-LOCK NO-ERROR.

                IF NOT AVAILABLE po-ordl THEN
                    FIND FIRST po-ordl
                        WHERE po-ordl.company   EQ fg-rctd.company
                        AND po-ordl.po-no     EQ INT({&self-name}:SCREEN-VALUE )
                        AND po-ordl.item-type EQ NO
                        NO-LOCK NO-ERROR.
                IF AVAILABLE po-ordl THEN RUN display-po (ROWID(po-ordl)).
                RUN pGetLocBin .
            END.

            RUN valid-po-no (1) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
         
            IF lRecFound AND INTEGER(cReturnValue) EQ 1 THEN DO:
                RUN CheckPOLineStatus IN hdOrderProcs(
                    INPUT cocode,
                    INPUT INTEGER(fg-rctd.po-no:SCREEN-VALUE),
                    INPUT INTEGER(fg-rctd.po-line:SCREEN-VALUE)
                    ) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    RETURN NO-APPLY.
            END.      
            IF lMultipleAdds THEN
                APPLY "WINDOW-CLOSE":U TO FRAME {&FRAME-NAME}.
        END.

        IF fg-rctd.partial:SCREEN-VALUE  EQ ? 
            OR fg-rctd.partial:SCREEN-VALUE  EQ "?" THEN
            fg-rctd.partial:SCREEN-VALUE  = "0".

        IF fg-rctd.cases:SCREEN-VALUE  EQ ? 
            OR fg-rctd.cases:SCREEN-VALUE  EQ "?" THEN
            fg-rctd.cases:SCREEN-VALUE  = "0".
        IF {&self-name}:SCREEN-VALUE  NE gcPoBeforeChange THEN do: 
            FIND po-ord 
                WHERE po-ord.company EQ cocode
                AND po-ord.po-no EQ INTEGER(fg-rctd.po-no:SCREEN-VALUE )
                NO-LOCK NO-ERROR.
            IF AVAILABLE po-ord THEN 
                DO:
                
                /* 10021210 */
                FIND FIRST shipto WHERE shipto.company EQ cocode
                    AND shipto.cust-no EQ po-ord.cust-no
                    AND shipto.ship-id EQ po-ord.ship-id
                    NO-LOCK NO-ERROR.
                IF AVAILABLE shipto AND shipto.loc GT "" THEN
                    ASSIGN
                    fg-rctd.loc:SCREEN-VALUE     = shipto.loc
                    fg-rctd.loc-bin:SCREEN-VALUE = shipto.loc-bin.
                END.
        END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case Dialog-Frame
ON ENTRY OF fg-rctd.qty-case IN FRAME Dialog-Frame /* Unit Count */
DO:
        ll-qty-case-ent = YES.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case Dialog-Frame
ON LEAVE OF fg-rctd.qty-case IN FRAME Dialog-Frame /* Unit Count */
DO:
        RUN new-qty.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.rct-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.rct-date Dialog-Frame
ON ENTRY OF fg-rctd.rct-date IN FRAME Dialog-Frame /* Receipt Date */
DO:
        IF LASTKEY NE -1 AND ip-set-parts THEN 
        DO:
            IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
                APPLY "back-tab" TO {&self-name} .
            ELSE
                APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    /*RUN reset-cursor.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.rct-date Dialog-Frame
ON HELP OF fg-rctd.rct-date IN FRAME Dialog-Frame /* Receipt Date */
DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.rct-date Dialog-Frame
ON LEAVE OF fg-rctd.rct-date IN FRAME Dialog-Frame /* Receipt Date */
DO:
        IF LASTKEY <> -1 THEN 
        DO:
            {custom/currentDatePrompt.i SELF:SCREEN-VALUE}
            lv-rct-date-checked = YES.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.stack-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.stack-code Dialog-Frame
ON LEAVE OF fg-rctd.stack-code IN FRAME Dialog-Frame /* FG Lot# */
DO:
        IF LASTKEY NE -1 THEN 
        DO:    
            RUN valid-lot# (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.std-cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.std-cost Dialog-Frame
ON ENTRY OF fg-rctd.std-cost IN FRAME Dialog-Frame /* Cost/UOM */
DO:
        IF LASTKEY NE -1                                                      AND
            (ip-set-parts OR
            TRIM(fg-rctd.job-no:SCREEN-VALUE ) NE "") THEN 
        DO:
            IF KEYFUNCTION(LASTKEY) EQ "SHIFT-TAB" THEN
                APPLY "back-tab" TO {&self-name} .
            ELSE
                APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.std-cost Dialog-Frame
ON LEAVE OF fg-rctd.std-cost IN FRAME Dialog-Frame /* Cost/UOM */
DO:
        RUN get-matrix (NO).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Dialog-Frame
ON LEAVE OF fg-rctd.tag IN FRAME Dialog-Frame /* Tag */
DO:    
        DEFINE VARIABLE lNegative AS LOG NO-UNDO.
        IF LASTKEY NE -1 THEN 
        DO: 
            IF adm-new-record OR
                ( AVAIL fg-rctd AND fg-rctd.tag NE fg-rctd.tag:SCREEN-VALUE) THEN
                RUN new-tag.

            IF avail(fg-rctd) AND fg-rctd.tag:SCREEN-VALUE  NE fg-rctd.tag THEN 
            DO:
                RUN valid-tag (FOCUS, OUTPUT lNegative) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            END.
            IF lNegative THEN 
            DO:
                IF lNegative AND INTEGER(fg-rctd.cases:SCREEN-VALUE ) GT 0 THEN
                    fg-rctd.cases:SCREEN-VALUE  = 
                        STRING(INTEGER(fg-rctd.cases:SCREEN-VALUE ) * -1).

            END.
        END.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Dialog-Frame
ON VALUE-CHANGED OF fg-rctd.tag IN FRAME Dialog-Frame /* Tag */
DO:
        RUN new-tag.
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
        
    RUN Inventory/InventoryProcs.p PERSISTENT SET hInventoryProcs.
    
    IF ip-type EQ "copy" THEN ASSIGN 
        lv-item-recid = ip-recid.
    IF ip-type EQ "add" THEN ASSIGN 
        adm-adding-record = YES.

    IF ip-recid EQ ? THEN 
    DO:
        RUN create-item.
    END.
    ELSE FIND fg-rctd NO-LOCK WHERE RECID(fg-rctd) EQ ip-recid NO-ERROR.

    IF ip-type NE "view" THEN 
    DO: 
        RUN enable_UI.
        RUN display-item.

        ASSIGN 
            ll-order-warned = NO.
        adm-new-record = NO .
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
        adm-new-record                            = NO .
    END.
    APPLY 'entry' TO fg-rctd.po-no.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convert-vend-comp-curr Dialog-Frame 
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
            vend.company EQ b-po-ord.company AND
            vend.vend-no EQ b-po-ord.vend-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE vend THEN
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-from-po Dialog-Frame 
PROCEDURE create-from-po :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dCostExtended        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostExtendedFreight AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOM          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCostUOM             AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE iCount               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ld                   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE li                   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-rno             LIKE fg-rctd.r-no NO-UNDO.
    DEFINE VARIABLE rwRowid              AS ROWID     NO-UNDO.
    DEFINE VARIABLE v-next-tag           AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER b-fg-rctd FOR fg-rctd.

    poSelected = 0.
    DO WITH FRAME {&FRAME-NAME}:
        FOR EACH tt-pol WHERE tt-pol.selekt,
            FIRST po-ordl WHERE ROWID(po-ordl) EQ tt-pol.row-id NO-LOCK:
            CREATE tt-fg-rctd.
            ASSIGN
                poSelected              = poSelected + 1
                tt-fg-rctd.rct-date     = DATE(fg-rctd.rct-date:SCREEN-VALUE )
                tt-fg-rctd.trans-time   = TIME
                tt-fg-rctd.i-no         = po-ordl.i-no
                tt-fg-rctd.po-no        = STRING(po-ordl.po-no)
                tt-fg-rctd.po-line      = po-ordl.line
                tt-fg-rctd.po-rowid     = ROWID(po-ordl)
                tt-fg-rctd.units-pallet = 1
                tt-fg-rctd.cases-unit   = 1
                tt-fg-rctd.qty-case     = 1
                ld                      = po-ordl.ord-qty
                .
            IF LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0 THEN
                RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, "EA", 0, 0, 0, 0,
                    ld, OUTPUT ld).
            {sys/inc/roundup.i ld}
            tt-fg-rctd.t-qty = ld.
            IF tt-fg-rctd.t-qty LT 0 THEN tt-fg-rctd.t-qty = 0.
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ po-ordl.company
                AND itemfg.i-no    EQ po-ordl.i-no
                NO-ERROR.
            IF AVAILABLE itemfg THEN
                ASSIGN
                    tt-fg-rctd.i-name   = itemfg.i-name
                    tt-fg-rctd.loc      = itemfg.def-loc
                    tt-fg-rctd.loc-bin  = itemfg.def-loc-bin
                    tt-fg-rctd.qty-case = itemfg.case-count
                    .     
            IF tt-fg-rctd.qty-case LE 0 THEN tt-fg-rctd.qty-case = MAX(tt-fg-rctd.t-qty,1).       
            ASSIGN
                li                 = TRUNCATE(tt-fg-rctd.t-qty / tt-fg-rctd.qty-case, 0)
                tt-fg-rctd.cases   = li
                tt-fg-rctd.partial = tt-fg-rctd.t-qty - (li * tt-fg-rctd.qty-case)
                .
            IF tt-fg-rctd.partial LT 0 THEN
                tt-fg-rctd.partial = 0.
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
            IF AVAILABLE fg-rctd THEN 
            DO:
                FIND CURRENT fg-rctd EXCLUSIVE-LOCK.
                lv-item-recid = ?.
            END.
            ELSE
                CREATE fg-rctd.
            BUFFER-COPY tt-fg-rctd EXCEPT rec_key TO fg-rctd.        
            ASSIGN
                tt-fg-rctd.tt-rowid = ROWID(fg-rctd)
                fg-rctd.company     = g_company
                fg-rctd.r-no        = lv-rno
                fg-rctd.rita-code   = "R"
                fg-rctd.trans-time  = TIME
                op-rowid            = ROWID(fg-rctd)
                iCount              = iCount + 1
                .   
            RUN pGetCostsFromPO (g_company, fg-rctd.po-no, fg-rctd.po-line, fg-rctd.i-no, fg-rctd.t-qty,
                OUTPUT dCostPerUOM, OUTPUT cCostUOM, OUTPUT dCostExtended, OUTPUT dCostExtendedFreight). 
            ASSIGN                                                                             
                fg-rctd.cost-uom = cCostUOM
                fg-rctd.std-cost = dCostPerUOM
                fg-rctd.ext-cost = dCostExtended
                fg-rctd.frt-cost = dCostExtendedFreight
                .
            DISPLAY
                fg-rctd.cost-uom
                fg-rctd.std-cost
                fg-rctd.ext-cost
                fg-rctd.frt-cost
                fg-rctd.po-line
                fg-rctd.i-no
                fg-rctd.i-name
                fg-rctd.loc
                fg-rctd.loc-bin
                fg-rctd.qty-case
                fg-rctd.cases-unit                 
                    WITH FRAME {&FRAME-NAME}.
            IF glFGPOTag# AND fg-rctd.tag EQ "" THEN 
            DO:
                RUN get-next-tag (INPUT fg-rctd.i-no, OUTPUT v-next-tag).
                RUN create-loadtag (INPUT-OUTPUT v-next-tag, INPUT ROWID(fg-rctd)).
                fg-rctd.tag = v-next-tag.
            END.
            ELSE fg-rctd.tag = fg-rctd.tag:SCREEN-VALUE .
            
            IF rwRowid EQ ? THEN
                rwRowid = ROWID(fg-rctd) .
            RELEASE fg-rctd.
        END. /* for each */
    END. /* do with */

    lMultipleAdds = iCount GT 1.

    IF NOT ip-set-parts AND lMultipleAdds THEN RUN fg/invrecpt.p (rwRowid, 1).
      
    IF lMultipleAdds EQ NO AND op-rowid NE ? THEN do:
        FIND FIRST fg-rctd NO-LOCK
            WHERE ROWID(fg-rctd) EQ op-rowid
            NO-ERROR.
        IF AVAIL fg-rctd THEN do:
           fg-rctd.tag:SCREEN-VALUE IN FRAME {&frame-name} = fg-rctd.tag .
           lv-item-recid = RECID(fg-rctd) .
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item Dialog-Frame 
PROCEDURE create-item :
/*------------------------------------------------------------------------------
                  Purpose:     
                  PARAMs:  <none>
                  Notes:       
                ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rno LIKE fg-rctd.r-no NO-UNDO.
    DEFINE BUFFER b-fg-rctd FOR fg-rctd.

    DO WITH FRAME {&FRAME-NAME}:
        lv-rno = 0.
        FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAILABLE b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

        FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.
        
        CREATE fg-rctd.

        DO WHILE TRUE:
            lv-rno = lv-rno + 1.
            FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
            IF AVAILABLE fg-rcpth THEN NEXT.
            FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
            IF AVAILABLE b-fg-rctd THEN NEXT.
            LEAVE.
        END.

        
        ASSIGN 
            fg-rctd.company   = cocode
            fg-rctd.r-no      = lv-rno
            fg-rctd.rita-code = "R".


        ASSIGN
            fg-rctd.rct-date     = TODAY
            fg-rctd.trans-time   = TIME
            fg-rctd.units-pallet = 1
            fg-rctd.cases-unit   = 1
            fg-rctd.ext-cost     = 0
            fg-rctd.partial      = 0
            fg-rctd.qty          = 0
            fg-rctd.qty-case     = 0.
        
        FOR EACH b-fg-rctd NO-LOCK
            WHERE b-fg-rctd.company EQ g_company
            AND b-fg-rctd.rita-code EQ "R"
            AND ROWID(b-fg-rctd)    NE ROWID(fg-rctd)
            AND b-fg-rctd.SetHeaderRno EQ 0
            BY b-fg-rctd.r-no DESCENDING:  /*Last one added, not necessarily the last date*/
            
            fg-rctd.rct-date = b-fg-rctd.rct-date.
            LEAVE.
        END.

        cTransTime = STRING(fg-rctd.trans-time,"HH:MM:SS").
        DISPLAY fg-rctd.rct-date fg-rctd.cases-unit cTransTime.
        ASSIGN 
            lv-item-recid = RECID(fg-rctd).
        ll-new-record = YES.
        adm-new-record = YES .

        FIND CURRENT fg-rctd NO-LOCK NO-ERROR.
    END. /* avail oe-relh */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-loadtag Dialog-Frame 
PROCEDURE create-loadtag :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       From r-loadtg.w
            ------------------------------------------------------------------------------*/

    DEFINE INPUT-OUTPUT PARAMETER io-tag-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER fg-rctd-row AS ROWID NO-UNDO.

    DEFINE BUFFER b-loadtag FOR loadtag.
    DEFINE BUFFER b-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-eb     FOR eb.
    DEFINE BUFFER b-fg-rctd FOR fg-rctd.

    DEFINE VARIABLE li                    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-got-job            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lv-out-cost           AS DECIMAL   DECIMALS 4 NO-UNDO.
    DEFINE VARIABLE lv-out-qty            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-from-uom           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-cost-uom           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-ord-qty            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-ord-uom            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-setup-included     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lv-setup-per-cost-uom AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-bwt                 LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-len                 LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid                 LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep                 LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE dRFIDTag              AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE v-fgrecpt             AS LOG       NO-UNDO. 
    DEFINE VARIABLE tb_ret                AS LOG       INIT YES NO-UNDO.
    DEFINE VARIABLE v-loadtag             AS CHARACTER NO-UNDO .
    DEFINE VARIABLE v-mult                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-cas-lab             AS LOG       NO-UNDO.
    DEFINE VARIABLE v-tags                AS DECIMAL   NO-UNDO.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "FGRECPT"
        NO-LOCK NO-ERROR.
    ASSIGN
        v-fgrecpt = AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "LoadTag".

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "LOADTAG"
        NO-LOCK NO-ERROR.
    IF AVAILABLE sys-ctrl THEN
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
        loadtag.po-no        = INT(b-fg-rctd.po-no).

    IF v-fgrecpt AND tb_ret THEN loadtag.tot-cases  = (loadtag.pallet-COUNT - loadtag.partial) / loadtag.case-bundle.

    IF v-loadtag = "CentBox" THEN 
    DO:
        ASSIGN 
            loadtag.loc     = itemfg.def-loc
            loadtag.loc-bin = itemfg.def-loc-bin.
        FIND FIRST fg-bin WHERE fg-bin.company EQ itemfg.company
            AND fg-bin.i-no    EQ itemfg.i-no
            AND fg-bin.job-no  EQ b-fg-rctd.job-no
            AND fg-bin.tag     EQ loadtag.tag-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE fg-bin THEN
            ASSIGN loadtag.loc     = fg-bin.loc
                loadtag.loc-bin = fg-bin.loc-bin.
  
    END.
    ELSE RUN fg/autopost.p (ROWID(itemfg), b-fg-rctd.job-no, b-fg-rctd.job-no2,
            OUTPUT loadtag.loc , OUTPUT loadtag.loc-bin).

    IF glRFIDTag THEN 
    DO:
        FIND FIRST oe-ctrl WHERE oe-ctrl.company = loadtag.company 
            NO-ERROR.
        dRFIDTag = IF AVAILABLE oe-ctrl AND oe-ctrl.spare-char-1 <> ""
            THEN dec(oe-ctrl.spare-char-1) ELSE 111110000000000000000001.
        oe-ctrl.spare-char-1 = STRING(dRFIDTag + 1).
        CREATE rfidtag.
        ASSIGN 
            rfidtag.company   = loadtag.company
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-tt Dialog-Frame 
PROCEDURE delete-tt :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:  
        IF NOT AVAILABLE fg-rctd AND INTEGER(fg-rctd.r-no:SCREEN-VALUE ) GT 0 THEN 
        DO:
            FIND FIRST fg-rctd
                WHERE fg-rctd.r-no EQ INTEGER(fg-rctd.r-no:SCREEN-VALUE ) 
                NO-LOCK NO-ERROR.
        END.    /*Mode 001*/
    
        IF AVAILABLE fg-rctd THEN 
        DO:
            FIND FIRST tt-fg-rctd WHERE tt-fg-rctd.tt-rowid EQ ROWID(fg-rctd) NO-ERROR.
            IF AVAILABLE tt-fg-rctd THEN DELETE tt-fg-rctd.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteSetParts Dialog-Frame 
PROCEDURE DeleteSetParts :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinker AS CHARACTER NO-UNDO.
    DEFINE BUFFER b-fg-rctd FOR fg-rctd.

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
    IF AVAILABLE fg-rctd  THEN 
    DO:
        ASSIGN 
            fi_tr-time = STRING(fg-rctd.trans-time,'HH:MM:SS') .

        DISPLAY  fg-rctd.r-no fg-rctd.rct-date 
            fg-rctd.tag fg-rctd.po-no 
            fg-rctd.po-line fg-rctd.job-no fg-rctd.job-no2 fg-rctd.i-no fg-rctd.i-name 
            fg-rctd.loc fg-rctd.loc-bin fg-rctd.cases fg-rctd.qty-case 
            fg-rctd.cases-unit fg-rctd.partial fg-rctd.std-cost fg-rctd.cost-uom 
            fg-rctd.t-qty fg-rctd.frt-cost fg-rctd.ext-cost fg-rctd.stack-code 
            fg-rctd.tot-wt fg-rctd.created-by fg-rctd.updated-by  fi_tr-time
            WITH FRAME Dialog-Frame.
    END.


    IF ip-type NE "view" THEN 
    DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.
    END.

    IF glFGSecurity THEN
    DO:
        FIND FIRST usergrps WHERE
            usergrps.usergrps = gcFGSecurity
            NO-LOCK NO-ERROR.

        IF AVAILABLE usergrps AND
            (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND
            TRIM(usergrps.users) NE "*") THEN
            ASSIGN
                fg-rctd.std-cost:VISIBLE IN FRAME {&FRAME-NAME} = NO
                fg-rctd.frt-cost:VISIBLE IN FRAME {&FRAME-NAME} = NO
                fg-rctd.ext-cost:VISIBLE IN FRAME {&FRAME-NAME} = NO.
    END.

    VIEW FRAME {&FRAME-NAME}. 
    IF NOT ip-set-parts THEN
        APPLY "entry" TO FRAME {&FRAME-NAME}.
    ELSE 
    DO:
        lv-linker = ipcLinkerSet .
        APPLY "entry" TO fg-rctd.tag IN FRAME {&FRAME-NAME} .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-po Dialog-Frame 
PROCEDURE display-po :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
    
    DEFINE VARIABLE lv-cost AS DECIMAL DECIMALS 4 NO-UNDO.

    FIND po-ordl WHERE ROWID(po-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

    IF AVAILABLE po-ordl THEN
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            fg-rctd.i-no:SCREEN-VALUE     = po-ordl.i-no
            fg-rctd.i-name:SCREEN-VALUE   = po-ordl.i-name
            fg-rctd.cost-uom:SCREEN-VALUE = po-ordl.pr-uom
            fg-rctd.po-line:SCREEN-VALUE  = STRING(po-ordl.line).
            
        RUN pDisplayPO(NO).

    END.

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
  DISPLAY cTransTime fi_tr-time 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE fg-rctd THEN 
    DISPLAY fg-rctd.rct-date fg-rctd.tag fg-rctd.stack-code fg-rctd.po-no 
          fg-rctd.po-line fg-rctd.job-no fg-rctd.job-no2 fg-rctd.i-no 
          fg-rctd.i-name fg-rctd.loc fg-rctd.loc-bin fg-rctd.cases 
          fg-rctd.qty-case fg-rctd.cases-unit fg-rctd.partial fg-rctd.t-qty 
          fg-rctd.tot-wt fg-rctd.std-cost fg-rctd.cost-uom fg-rctd.frt-cost 
          fg-rctd.ext-cost fg-rctd.created-by fg-rctd.updated-by fg-rctd.r-no 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_Cancel Btn_Done fg-rctd.rct-date btnCalendar-1 fg-rctd.tag 
         fg-rctd.stack-code fg-rctd.po-no fg-rctd.po-line fg-rctd.job-no 
         fg-rctd.job-no2 fg-rctd.i-no fg-rctd.loc fg-rctd.loc-bin fg-rctd.cases 
         fg-rctd.qty-case fg-rctd.cases-unit fg-rctd.partial fg-rctd.std-cost 
         fg-rctd.cost-uom fg-rctd.frt-cost fg-rctd.ext-cost Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-current-qty Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-def-values Dialog-Frame 
PROCEDURE get-def-values :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF adm-new-record THEN
            ASSIGN
                fg-rctd.loc:SCREEN-VALUE      = ""
                fg-rctd.loc-bin:SCREEN-VALUE  = ""
                fg-rctd.std-cost:SCREEN-VALUE = ""
                fg-rctd.qty-case:SCREEN-VALUE = ""
                fg-rctd.cost-uom:SCREEN-VALUE = "".

        RUN get-values.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-fg-bin-cost Dialog-Frame 
PROCEDURE get-fg-bin-cost :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
  
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE 
            AND fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE 
            AND fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE )
            AND fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE 
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE 
            AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE 
            NO-LOCK NO-ERROR.
        IF AVAILABLE fg-bin THEN
            ASSIGN
                fg-rctd.std-cost:SCREEN-VALUE = STRING(fg-bin.std-tot-cost)
                fg-rctd.cost-uom:SCREEN-VALUE = STRING(fg-bin.pur-uom).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-first-r-no Dialog-Frame 
PROCEDURE get-first-r-no :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE BUFFER bq-fg-rctd FOR fg-rctd.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-freight-cost Dialog-Frame 
PROCEDURE get-freight-cost :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-cost LIKE fg-rctd.frt-cost NO-UNDO.

    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO.
    DEFINE VARIABLE ld-qty   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ld-wgt   AS DECIMAL EXTENT 2 NO-UNDO.
    DEFINE VARIABLE ld-cst   AS DECIMAL EXTENT 2 NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        RELEASE po-ord.

        FIND FIRST po-ordl
            WHERE po-ordl.company   EQ cocode
            AND po-ordl.po-no     EQ INT(fg-rctd.po-no:SCREEN-VALUE )
            AND po-ordl.i-no      EQ fg-rctd.i-no:SCREEN-VALUE 
            AND po-ordl.job-no    EQ fg-rctd.job-no:SCREEN-VALUE 
            AND po-ordl.job-no2   EQ INT(fg-rctd.job-no2:SCREEN-VALUE )
            AND po-ordl.item-type EQ NO
            NO-LOCK NO-ERROR.

        IF AVAILABLE po-ordl THEN
            RUN po/getfrtcs.p (ROWID(po-ordl),
                DEC(fg-rctd.t-qty:SCREEN-VALUE ),
                OUTPUT op-cost).

        RUN convert-vend-comp-curr(INPUT-OUTPUT op-cost).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-job-no Dialog-Frame 
PROCEDURE get-job-no :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplAskPasswd AS LOG NO-UNDO.
    DEFINE VARIABLE lvPasswordEntered AS LOG       NO-UNDO.
    DEFINE VARIABLE lcRitaCode        AS CHARACTER NO-UNDO.
    IF AVAIL(fg-rctd) THEN
        lcRitaCode = fg-rctd.rita-code.
    ELSE
        lcRitaCode = "R".
    DO WITH FRAME {&frame-name}:
        fg-rctd.job-no:SCREEN-VALUE  =
            FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE ))) +
            TRIM(fg-rctd.job-no:SCREEN-VALUE ).

        IF TRIM(fg-rctd.job-no:SCREEN-VALUE ) NE TRIM(lv-job-no)  OR
            DEC(fg-rctd.job-no2:SCREEN-VALUE ) NE DEC(lv-job-no2) THEN
            RUN new-job-no.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-linker Dialog-Frame 
PROCEDURE get-linker :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-linker LIKE lv-linker NO-UNDO.


    op-linker = IF AVAILABLE fg-rctd                 AND
        CAN-FIND(FIRST itemfg
        WHERE itemfg.company EQ fg-rctd.company
        AND itemfg.i-no    EQ fg-rctd.i-no
        AND itemfg.isaset) THEN
        "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
        ELSE STRING(fg-rctd.r-no).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix Dialog-Frame 
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
        IF AVAILABLE fg-rctd AND fg-rctd.i-no:SCREEN-VALUE  <> "" THEN 
        DO: /* in update mode - use screen-value */
            IF INTEGER(fg-rctd.po-no:SCREEN-VALUE) NE 0 THEN 
            DO:  
                RUN pDisplayPO(ip-first-disp).
 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix-all Dialog-Frame 
PROCEDURE get-matrix-all :
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
    DEFINE VARIABLE lv-out-qty  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-out-cost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-rec-qty   AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll-ea       AS LOG     NO-UNDO.

    DEFINE BUFFER b-fg-rctd FOR fg-rctd.


    cocode = g_company.
    DO WITH FRAME {&FRAME-NAME}:
        lv-out-qty = 0.
        FOR EACH b-fg-rctd WHERE b-fg-rctd.company EQ g_company AND
            (b-fg-rctd.rita-code EQ "R" OR b-fg-rctd.rita-code EQ "E")
            AND trim(b-fg-rctd.job-no) = trim(fg-rctd.job-no:SCREEN-VALUE )
            AND b-fg-rctd.job-no2 = INT(fg-rctd.job-no2:SCREEN-VALUE)
            AND b-fg-rctd.i-no = fg-rctd.i-no:SCREEN-VALUE 
            AND (RECID(b-fg-rctd) <> recid(fg-rctd) 
            OR (adm-new-record AND NOT adm-adding-record))
            NO-LOCK :

            lv-out-qty = lv-out-qty + b-fg-rctd.t-qty.     
        END.
  
        lv-out-qty = lv-out-qty + int(fg-rctd.t-qty:SCREEN-VALUE).

        IF fg-rctd.i-no:SCREEN-VALUE <> "" THEN 
        DO: /* in update mode - use screen-value */
            FIND itemfg  WHERE itemfg.company EQ cocode
                AND itemfg.i-no  EQ fg-rctd.i-no:SCREEN-VALUE 
                USE-INDEX i-no NO-LOCK NO-ERROR.
            FIND FIRST po-ordl WHERE po-ordl.company = cocode
                AND po-ordl.po-no = integer(fg-rctd.po-no:SCREEN-VALUE ) 
                AND po-ordl.i-no  = fg-rctd.i-no:screen-value
                AND po-ordl.job-no = (fg-rctd.job-no:screen-value)
                AND po-ordl.job-no2 = integer(fg-rctd.job-no2:screen-value)
                AND po-ordl.item-type = NO
                NO-LOCK NO-ERROR.
  
            IF AVAILABLE po-ordl THEN 
            DO:
                v-rec-qty = po-ordl.t-rec-qty + lv-out-qty.
                RUN sys/ref/ea-um-fg.p (po-ordl.pr-qty-uom, OUTPUT ll-ea).
                IF NOT ll-ea THEN
                    RUN sys/ref/convquom.p("EA", po-ordl.pr-qty-uom, 0, 0, 0, 0,
                        v-rec-qty, OUTPUT v-rec-qty).
                RUN valid-porec-qty(INPUT v-rec-qty).
                IF lFatalQtyError THEN RETURN ERROR.
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
                        FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND
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
              
                            v-rec-qty = (job-hdr.qty * (1 + ((IF AVAILABLE oe-ordl THEN oe-ordl.over-pct ELSE
                                IF AVAILABLE oe-ord  THEN oe-ord.over-pct  ELSE 0) / 100))).
                        END.
                        IF v-rec-qty <  lv-out-qty AND NOT lv-overrun-checked THEN 
                        DO:
                            MESSAGE "Receipt Qty has exceeded Job Qty. " VIEW-AS ALERT-BOX WARNING.
                            /*RETURN ERROR.*/
                            lv-overrun-checked = YES.
                        END.
           
                    END.
                END.
     
        END. /* i-no <> ""*/
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-next-tag Dialog-Frame 
PROCEDURE get-next-tag :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipc-i-no AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opc-next-tag AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-loadtag FOR loadtag.
    DEFINE VARIABLE io-tag-no AS INTEGER NO-UNDO.

    FIND LAST bf-loadtag NO-LOCK
        WHERE bf-loadtag.company     EQ cocode
        AND bf-loadtag.item-type   EQ NO
        AND bf-loadtag.is-case-tag EQ NO
        AND bf-loadtag.tag-no      BEGINS ipc-i-no
        AND SUBSTR(bf-loadtag.tag-no,1,15) EQ ipc-i-no
        USE-INDEX tag NO-ERROR.

    io-tag-no = (IF AVAILABLE bf-loadtag THEN INT(SUBSTR(bf-loadtag.tag-no,16,5)) ELSE 0) + 1.

    opc-next-tag = STRING(CAPS(ipc-i-no),"x(15)") + STRING(io-tag-no,"99999").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-set-full-qty Dialog-Frame 
PROCEDURE get-set-full-qty :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-cost-to-set AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-on-screen AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER op-out-qty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-len          LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid          LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep          LIKE po-ordl.s-len NO-UNDO. 
    DEFINE VARIABLE v-bwt          LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE lv-out-qty     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-out-cost    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-calc-cost   AS DECIMAL.
    DEFINE VARIABLE lv-recalc-cost AS DECIMAL.
    DEFINE VARIABLE lv-ext-cost    AS DECIMAL.
    DEFINE VARIABLE v-rec-qty      AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll-ea          AS LOG     NO-UNDO.

    DEFINE BUFFER b-fg-rctd  FOR fg-rctd.
    DEFINE BUFFER b1-fg-rctd FOR fg-rctd.


    cocode = g_company.
    DO WITH FRAME {&FRAME-NAME}:
        lv-out-qty = 0.
        FOR EACH b-fg-rctd WHERE b-fg-rctd.company EQ g_company AND
            (b-fg-rctd.rita-code EQ "R" OR b-fg-rctd.rita-code EQ "E")
            AND trim(b-fg-rctd.job-no) = trim(fg-rctd.job-no:SCREEN-VALUE )
            AND b-fg-rctd.job-no2 = INT(fg-rctd.job-no2:SCREEN-VALUE)
            AND b-fg-rctd.i-no = fg-rctd.i-no:SCREEN-VALUE 
            AND (RECID(b-fg-rctd) <> recid(fg-rctd) 
            OR (adm-new-record AND NOT adm-adding-record))
            AND b-fg-rctd.SetHeaderRno EQ INTEGER(SUBSTRING(lv-linker, 10, 10))
            AND b-fg-rctd.SetHeaderRno GT 0
            NO-LOCK:     


            lv-out-qty = lv-out-qty + b-fg-rctd.t-qty.     
            IF ip-cost-to-set GT 0 THEN 
            DO:

                /* convert cost to b1-fg-rctd uom */

                FIND b1-fg-rctd WHERE ROWID(b1-fg-rctd) EQ ROWID(b-fg-rctd)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE b1-fg-rctd THEN 
                DO WITH FRAME {&FRAME-NAME}:
        
                    FIND itemfg WHERE itemfg.company EQ cocode
                        AND itemfg.i-no  EQ b-fg-rctd.i-no
                        USE-INDEX i-no NO-LOCK NO-ERROR.
            
                    ASSIGN
                        v-bwt = 0
                        v-dep = 0.
            
                    IF AVAILABLE itemfg THEN
                        ASSIGN v-len = itemfg.t-len
                            v-wid = itemfg.t-wid.
            
                    /* Always find just to get quantity */
                    FIND FIRST po-ordl WHERE po-ordl.company = cocode
                        AND po-ordl.po-no   = int(b-fg-rctd.po-no)
                        AND po-ordl.i-no    = b-fg-rctd.i-no
                        AND po-ordl.job-no  = b-fg-rctd.job-no
                        AND po-ordl.job-no2 = b-fg-rctd.job-no2
                        AND po-ordl.item-type = NO
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE po-ordl THEN
                        FIND FIRST po-ordl WHERE po-ordl.company = cocode
                            AND po-ordl.po-no   = integer(b-fg-rctd.po-no)
                            AND po-ordl.i-no    = b-fg-rctd.i-no
                            AND po-ordl.item-type = NO
                            NO-LOCK NO-ERROR.
            
            
                    IF AVAILABLE po-ordl THEN
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
                        lv-ext-cost         = b1-fg-rctd.t-qty * b1-fg-rctd.std-cost                          
                        b1-fg-rctd.ext-cost = lv-ext-cost + b1-fg-rctd.frt-cost.
                END.

            END.
        END.
        IF ip-on-screen THEN
            lv-out-qty = lv-out-qty + int(fg-rctd.t-qty:SCREEN-VALUE).
        op-out-qty = lv-out-qty.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-values Dialog-Frame 
PROCEDURE get-values :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-loc      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-loc-bin  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-qty-case AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-cost-uom AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-std-cost AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-cost      AS DECIMAL   DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE lv-save     AS CHARACTER EXTENT 20 NO-UNDO.
    DEFINE VARIABLE ll-ea       AS LOG       NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST itemfg
            {sys/look/itemfgrlW.i}
            AND itemfg.i-no EQ fg-rctd.i-no:SCREEN-VALUE 
        NO-LOCK NO-ERROR.
        IF AVAILABLE itemfg THEN
        DO:
        
            /* Assign from itemfg only if blank. Don't overwrite if name already set from PO. */
            IF fg-rctd.i-name:SCREEN-VALUE = "" OR fg-rctd.po-no:SCREEN-VALUE = "" AND AVAILABLE itemfg THEN
                ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name.
            
    
            /*     find first fg-ctrl where fg-ctrl.company eq cocode no-lock no-error.  */
        
            ASSIGN
                lv-qty-case = STRING(itemfg.case-count)
                lv-cost-uom = itemfg.prod-uom.
    
            RUN fg/autopost.p (ROWID(itemfg),
                fg-rctd.job-no:SCREEN-VALUE ,
                INT(fg-rctd.job-no2:SCREEN-VALUE ),
                OUTPUT lv-loc, OUTPUT lv-loc-bin).
        
            FIND FIRST fg-bin
                WHERE fg-bin.company EQ cocode
                AND fg-bin.loc     EQ lv-loc
                AND fg-bin.loc-bin EQ lv-loc-bin
                AND fg-bin.i-no    EQ ""
                NO-LOCK NO-ERROR.
            IF AVAILABLE fg-bin THEN 
                ASSIGN
                    lv-qty-case = STRING(itemfg.case-count)
                    lv-cost-uom = itemfg.prod-uom.
        END.

        /**  Find the Job Header record in then job file and use Standard Cost
             from that job. **/
        FIND FIRST job-hdr
            WHERE job-hdr.company EQ cocode
            AND job-hdr.i-no    EQ fg-rctd.i-no:SCREEN-VALUE 
            AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE 
            AND job-hdr.job-no2 EQ int(fg-rctd.job-no2:SCREEN-VALUE )
            NO-LOCK NO-ERROR.

        IF NOT AVAILABLE job-hdr THEN 
        DO:
            FIND FIRST job
                WHERE job.company EQ cocode
                AND job.job-no  EQ fg-rctd.job-no:SCREEN-VALUE 
                AND job.job-no2 EQ int(fg-rctd.job-no2:SCREEN-VALUE )
                NO-LOCK NO-ERROR.
            IF AVAILABLE job THEN
                FIND FIRST reftable-job
                    WHERE reftable-job.reftable EQ "jc/jc-calc.p"
                    AND reftable-job.company  EQ job.company
                    AND reftable-job.loc      EQ ""
                    AND reftable-job.code     EQ STRING(job.job,"999999999")
                    AND reftable-job.code2    EQ fg-rctd.i-no:SCREEN-VALUE 
                    NO-LOCK NO-ERROR.
        END.

        IF AVAILABLE job-hdr AND job-hdr.std-tot-cost GT 0 THEN
            ASSIGN
                lv-cost-uom = "M"
                lv-std-cost = STRING(job-hdr.std-tot-cost).
        ELSE
            IF AVAILABLE reftable-job AND reftable-job.val[5] GT 0 THEN
                ASSIGN
                    lv-cost-uom = "M"
                    lv-std-cost = STRING(reftable-job.val[5]).

            /** If no Job Header is avail for this Job# then Find the Item
                record for then item and use Standard Cost from that item. **/
            ELSE 
            DO:
                FIND FIRST po-ordl
                    WHERE po-ordl.company   EQ cocode           
                    AND po-ordl.po-no     EQ int(fg-rctd.po-no:SCREEN-VALUE )
                    AND po-ordl.i-no      EQ fg-rctd.i-no:SCREEN-VALUE 
                    AND po-ordl.line      EQ int(fg-rctd.po-line:SCREEN-VALUE )
                    AND po-ordl.item-type EQ NO
                    NO-LOCK NO-ERROR.
          
                IF AVAILABLE po-ordl THEN 
                DO:
                    RUN pDisplayPO(NO).
                /*                    ASSIGN                                                                                             */
                /*                        lv-cost-uom = po-ordl.pr-uom.                                                                  */
                /*                    lv-std-cost = STRING(po-ordl.cost * (IF po-ordl.disc NE 0 THEN (1 - (po-ordl.disc / 100)) ELSE 1)).*/
                /*                                                                                                                       */
                /*                    RUN convert-vend-comp-curr(INPUT-OUTPUT lv-std-cost).                                              */
                /*                                                                                                                       */
                /*                    RUN show-freight.                                                                                  */
        
                END.
     
                ELSE
                    IF AVAILABLE itemfg          AND
                        DEC(lv-std-cost) EQ 0 THEN 
                    DO:
                        RUN pDisplayFG(NO,BUFFER itemfg).
                    END.
            END.
         
        /* #pn# If there is a tag, quantites should default from there */
        /* #pn# task 10311308                                          */
        IF fg-rctd.tag:SCREEN-VALUE  GT "" THEN
            RUN new-tag.
        IF fg-rctd.loc:SCREEN-VALUE      EQ "" THEN
            ASSIGN
                fg-rctd.loc:SCREEN-VALUE     = lv-loc
                fg-rctd.loc-bin:SCREEN-VALUE = lv-loc-bin.

        IF INT(fg-rctd.qty-case:SCREEN-VALUE ) EQ 0 THEN
            fg-rctd.qty-case:SCREEN-VALUE  = lv-qty-case.

        IF fg-rctd.cost-uom:SCREEN-VALUE  EQ "" THEN
            fg-rctd.cost-uom:SCREEN-VALUE  = lv-cost-uom.

        IF DEC(fg-rctd.std-cost:SCREEN-VALUE ) EQ 0 THEN
            fg-rctd.std-cost:SCREEN-VALUE  = lv-std-cost.

        IF INT(fg-rctd.cases-unit:SCREEN-VALUE ) EQ 0 THEN
            fg-rctd.cases-unit:SCREEN-VALUE  = "1".

        IF fg-rctd.partial:SCREEN-VALUE  EQ ? 
            OR fg-rctd.partial:SCREEN-VALUE  EQ "?" THEN
            fg-rctd.partial:SCREEN-VALUE  = "0".

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit Dialog-Frame 
PROCEDURE local-exit :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-no Dialog-Frame 
PROCEDURE new-job-no :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
DEFINE BUFFER bf-job-hdr FOR job-hdr .
    DO WITH FRAME {&FRAME-NAME}:
        lv-closed-checked = NO.
        lv-new-job-ran = YES.

        IF fg-rctd.job-no:SCREEN-VALUE  NE "" THEN
            FOR EACH bf-job-hdr NO-LOCK
                WHERE bf-job-hdr.company EQ cocode
                AND bf-job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE 
                AND bf-job-hdr.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE )
                BREAK BY bf-job-hdr.frm      DESCENDING
                BY bf-job-hdr.blank-no DESCENDING:

                IF LAST(bf-job-hdr.frm)                                                  OR
                    bf-job-hdr.i-no EQ fg-rctd.i-no:SCREEN-VALUE  THEN 
                DO:
                    ASSIGN
                        lv-job-no                     = fg-rctd.job-no:SCREEN-VALUE
                        lv-job-no2                    = fg-rctd.job-no2:SCREEN-VALUE
                        fg-rctd.i-no:SCREEN-VALUE     = bf-job-hdr.i-no
                        fg-rctd.std-cost:SCREEN-VALUE = STRING(bf-job-hdr.std-mat-cost +
                                                bf-job-hdr.std-lab-cost +
                                                bf-job-hdr.std-fix-cost +
                                                bf-job-hdr.std-var-cost).
                      RUN  pGetUnassembledItem(cocode , bf-job-hdr.i-no) .

                    RUN get-def-values.
                    IF NOT lUpdateRecords THEN
                        RUN pGetUnitCountFromJob(bf-job-hdr.ord-no ,fg-rctd.i-no:SCREEN-VALUE,bf-job-hdr.job-no,bf-job-hdr.job-no2) .
                      
                    LEAVE.
                END.
            END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-qty Dialog-Frame 
PROCEDURE new-qty :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    

    DO WITH FRAME {&FRAME-NAME}:
       
        fg-rctd.t-qty:SCREEN-VALUE  =
            STRING(INT(fg-rctd.cases:SCREEN-VALUE) *
            INT(fg-rctd.qty-case:SCREEN-VALUE) +
            INT(fg-rctd.partial:SCREEN-VALUE)).
    
        IF NOT adm-new-record OR ll-qty-case-ent THEN 
        DO:
            RUN get-matrix (NO).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-tag Dialog-Frame 
PROCEDURE new-tag :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE VARIABLE hProc       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE dMaxQty     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cHeaderItem AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dMaxCompQty AS DECIMAL   NO-UNDO. /* Max component quantity */
    DEFINE VARIABLE dTotalQty   AS DECIMAL   NO-UNDO.
    DEFINE BUFFER bfItemfg FOR itemfg.

    DO WITH FRAME {&FRAME-NAME}:

        IF NOT ip-set-parts THEN 
        DO:      
            FIND FIRST loadtag NO-LOCK
                WHERE loadtag.company   EQ cocode
                AND loadtag.item-type EQ NO
                AND loadtag.tag-no    EQ fg-rctd.tag:SCREEN-VALUE 
                NO-ERROR.
        
            IF AVAILABLE loadtag THEN 
            DO:
                fg-rctd.i-no:SCREEN-VALUE  = loadtag.i-no.
                fg-rctd.stack-code:SCREEN-VALUE  = loadtag.misc-char[2] .  /* task 12051302 */
                IF INTEGER(fg-rctd.cases:SCREEN-VALUE ) = 0 THEN
                    fg-rctd.cases:SCREEN-VALUE  = STRING(loadtag.case-bundle).
                IF INTEGER(fg-rctd.qty-case:SCREEN-VALUE ) = 0 THEN
                    fg-rctd.qty-case:SCREEN-VALUE  = STRING(loadtag.qty-case).
                IF INTEGER(loadtag.case-bundle) > 1 AND INTEGER(fg-rctd.cases-unit:SCREEN-VALUE ) = 1 THEN
                    fg-rctd.cases-unit:SCREEN-VALUE  = STRING(loadtag.case-bundle).
                /* Task 12061305 */  
                IF loadtag.job-no <> "" THEN 
                DO:
                    ASSIGN
                        fg-rctd.job-no:SCREEN-VALUE  = loadtag.job-no
                        fg-rctd.job-no2:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM(STRING(loadtag.job-no2)))) +
                                                                           TRIM(STRING(loadtag.job-no2)).
                    IF NOT glFGReceiptPassWord THEN
                        RUN get-job-no (INPUT YES) NO-ERROR.
                    ELSE
                        /* run with 'no' so no message until save */
                        RUN get-job-no (INPUT NO) NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
                END.  /* Task 12061305 */
                IF adm-new-record THEN
                    fg-rctd.po-no:SCREEN-VALUE  = STRING(loadtag.po-no). 
            END.
        END.
        ELSE 
        DO:
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ cocode
                AND fg-bin.tag EQ fg-rctd.tag:SCREEN-VALUE 
                AND fg-rctd.tag:SCREEN-VALUE  NE ""
                NO-ERROR.
       
            /* Obtain quantity of set header record */                        
            dMaxCompQty = maxComponentQty().
            IF AVAILABLE fg-bin  THEN 
            DO:

                /* dTotalQty is the qty in other lines with the same item number */
                RUN get-set-full-qty (INPUT fg-bin.std-tot-cost, INPUT YES, OUTPUT dTotalQty).
          
                dTotalQty = dTotalQty - DEC(fg-rctd.t-qty:SCREEN-VALUE ).
          
                /* dMaxCompQty is the max that can be used from fg-bin */
                IF ABS(dMaxCompQty) GT 0 AND ABS(dTotalQty) GT 0 THEN
                    dMaxCompQty = dMaxCompQty - ABS(dTotalQty).
                IF dMaxCompQty LT 0 THEN
                    dMaxCompQty = 0.
                IF /*ABS(DECIMAL(fg-rctd.t-qty:SCREEN-VALUE )) GE fg-bin.qty */ 
                    ABS(dTotalQty) + ABS(fg-bin.qty) LE ABS(dMaxCompQty) THEN
                    ASSIGN
                        fg-rctd.i-no:SCREEN-VALUE       = fg-bin.i-no
                        fg-rctd.cases:SCREEN-VALUE      = STRING(-1 * TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0))
                        fg-rctd.qty-case:SCREEN-VALUE   = STRING(fg-bin.case-count)
                        fg-rctd.cases-unit:SCREEN-VALUE = STRING(fg-bin.cases-unit)
                        fg-rctd.partial:SCREEN-VALUE    = STRING(-1 * fg-bin.partial-count)
                        fg-rctd.t-qty:SCREEN-VALUE      = STRING(-1 * fg-bin.qty).
                ELSE
                    ASSIGN            
                        fg-rctd.i-no:SCREEN-VALUE       = fg-bin.i-no
                        fg-rctd.cases:SCREEN-VALUE      = "0" /* STRING(-1 * TRUNC(dMaxCompQty / fg-bin.case-count,0)) */
                        fg-rctd.qty-case:SCREEN-VALUE   = STRING(dMaxCompQty)
                        fg-rctd.cases-unit:SCREEN-VALUE = STRING(fg-bin.cases-unit)
                        fg-rctd.partial:SCREEN-VALUE    = STRING(-1 * dMaxCompQty)
                        fg-rctd.t-qty:SCREEN-VALUE      = STRING(-1 * dMaxCompQty).

                /* Task 12061305 */
                IF fg-bin.job-no <> "" THEN
                    ASSIGN
                        fg-rctd.job-no:SCREEN-VALUE  = fg-bin.job-no
                        fg-rctd.job-no2:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM(STRING(fg-bin.job-no2)))) +
                                                                          TRIM(STRING(fg-bin.job-no2)).
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE non-blank-job-po Dialog-Frame 
PROCEDURE non-blank-job-po :
/*------------------------------------------------------------------------------
                 Purpose:
                 Notes:
                ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lvPasswordEntered AS LOG       NO-UNDO.
    DEFINE VARIABLE lcRitaCode        AS CHARACTER NO-UNDO.    
      
    IF AVAIL(fg-rctd) THEN
        lcRitaCode = fg-rctd.rita-code.
    ELSE
        lcRitaCode = "R".
        
    
    IF NOT ip-set-parts THEN 
    DO WITH FRAME {&frame-name}:
        IF glFGRecpt
            AND fg-rctd.job-no:SCREEN-VALUE  EQ ""                                                                
            AND fg-rctd.po-no:SCREEN-VALUE  EQ "" 
            AND lcRitaCode NE "E"                                  THEN 
        DO:
            /* Job Number and PO # are blank */
            /* Check password for override */
            lvPasswordEntered = NO.           
            IF glFGReceiptPassWord THEN 
            DO:
                RUN sys/ref/d-psswrd.w (INPUT "FGRecptPassWord", INPUT "FGRecptPassWord",
                    OUTPUT lvPasswordEntered).
            END.
            IF NOT lvPasswordEntered OR NOT glFGReceiptPassWord THEN 
            DO:
                MESSAGE "You must enter a Job or a PO..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO fg-rctd.job-no .
                RETURN ERROR.
            END.
                
        END. /* If checking for blank job and PO */                                               
      
    END. /* Do with frame ... */
    RETURN. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayFG Dialog-Frame 
PROCEDURE pDisplayFG PRIVATE :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplGetLocBin AS LOGICAL NO-UNDO .
    DEFINE PARAMETER BUFFER ipbf-itemfg FOR itemfg.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            fg-rctd.i-no:SCREEN-VALUE     = ipbf-itemfg.i-no
            fg-rctd.i-name:SCREEN-VALUE   = ipbf-itemfg.i-name .
        IF iplGetLocBin EQ YES  THEN
            ASSIGN
            fg-rctd.loc:SCREEN-VALUE      = ipbf-itemfg.def-loc
            fg-rctd.loc-bin:SCREEN-VALUE  = ipbf-itemfg.def-loc-bin .
        ASSIGN
            fg-rctd.std-cost:SCREEN-VALUE = IF glAverageCost THEN STRING(ipbf-itemfg.avg-cost) ELSE STRING(ipbf-itemfg.last-cost)
            fg-rctd.cost-uom:SCREEN-VALUE = ipbf-itemfg.prod-uom  .
        IF NOT lUpdateRecords THEN
            fg-rctd.qty-case:SCREEN-VALUE = STRING(ipbf-itemfg.case-count) .  
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayPO Dialog-Frame 
PROCEDURE pDisplayPO PRIVATE :
/*------------------------------------------------------------------------------
             Purpose: Encapsulates displaying the PO given current screen-values
             Notes:
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplUpdateFreight AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE dCostPerUOM          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostExtended        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostExtendedFreight AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCostUOM             AS CHARACTER NO-UNDO. 
    
    DO WITH FRAME {&FRAME-NAME}:
        IF fg-rctd.po-line:SCREEN-VALUE  EQ "" OR fg-rctd.po-line:SCREEN-VALUE  EQ "0" THEN 
            fg-rctd.po-line:SCREEN-VALUE  = "1".
        RUN pGetCostsFromPO(cocode, INTEGER(fg-rctd.po-no:SCREEN-VALUE ), INTEGER(fg-rctd.po-line:SCREEN-VALUE ), 
            fg-rctd.i-no:SCREEN-VALUE , DECIMAL(fg-rctd.t-qty:SCREEN-VALUE ),
            OUTPUT dCostPerUOM, OUTPUT cCostUOM, OUTPUT dCostExtended, OUTPUT dCostExtendedFreight). 
        ASSIGN                                                                             
            fg-rctd.cost-uom:SCREEN-VALUE = cCostUOM
            fg-rctd.std-cost:SCREEN-VALUE = STRING(dCostPerUOM)
            fg-rctd.ext-cost:SCREEN-VALUE = STRING(dCostExtended).
        IF iplUpdateFreight THEN 
            fg-rctd.frt-cost:SCREEN-VALUE  = STRING(dCostExtendedFreight).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCostsFromPO Dialog-Frame 
PROCEDURE pGetCostsFromPO PRIVATE :
/*------------------------------------------------------------------------------
             Purpose:
             Notes:
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPONumber AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPOLine AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQty AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotalFreight AS DECIMAL NO-UNDO.

    DEFINE VARIABLE dCostPerEA        AS DECIMAL.
    DEFINE VARIABLE dCostFreight      AS DECIMAL.
    DEFINE VARIABLE dCostFreightPerEA AS DECIMAL.
    DEFINE VARIABLE lFound            AS LOGICAL.
    
    RUN GetCostForPOLine IN hdCostProcs (ipcCompany, ipiPONumber, ipiPOLine, ipcFGItemID, OUTPUT opdCostPerUOM, OUTPUT opcCostUOM, OUTPUT dCostFreight, OUTPUT lFound).
    dCostPerEA = DYNAMIC-FUNCTION('fConvert' IN hdCostProcs, opcCostUOM, "EA",0,0,0,0,1,1, opdCostPerUOM).
    dCostFreightPerEA = DYNAMIC-FUNCTION('fConvert' IN hdCostProcs, opcCostUOM, "EA",0,0,0,0,1,1, dCostFreight).
    ASSIGN 
        opdCostTotal        = ipdQty * dCostPerEA
        opdCostTotalFreight = ipdQty * dCostFreightPerEA.
    IF glFGPOFrt THEN 
        opdCostTotal = opdCostTotal + opdCostTotalFreight.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetLocBin Dialog-Frame 
PROCEDURE pGetLocBin :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
    ------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}: 
    FIND FIRST itemfg {sys/look/itemfgrlW.i}
        AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE 
        NO-LOCK NO-ERROR.
    IF AVAILABLE itemfg THEN
        ASSIGN
        fg-rctd.i-name:SCREEN-VALUE  = itemfg.i-name
        fg-rctd.loc:SCREEN-VALUE     = itemfg.def-loc
        fg-rctd.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin .
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUnassembledItem Dialog-Frame 
PROCEDURE pGetUnassembledItem :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO .
    DEFINE INPUT PARAMETER ipcFGItem AS CHARACTER NO-UNDO .
    DEFINE BUFFER bf-itemfg FOR itemfg .
DO WITH FRAME {&FRAME-NAME}: 
    FIND FIRST bf-itemfg NO-LOCK
        WHERE bf-itemfg.company EQ ipcCompany
          AND bf-itemfg.i-no    EQ ipcFGItem
          AND bf-itemfg.isaset  EQ YES
          AND bf-itemfg.alloc   EQ YES NO-ERROR .  /* bf-itemfg.alloc EQ YES use for unassemble */
    IF AVAIL bf-itemfg THEN
        FIND FIRST fg-set NO-LOCK
        WHERE fg-set.company = bf-itemfg.company 
        AND fg-set.set-no = bf-itemfg.i-no NO-ERROR .
    IF AVAIL bf-itemfg AND AVAIL fg-set THEN
        fg-rctd.i-no:SCREEN-VALUE     = fg-set.part-no .
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUnitCountFromJob Dialog-Frame 
PROCEDURE pGetUnitCountFromJob :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiOrder AS INTEGER NO-UNDO .
    DEFINE INPUT PARAMETER ipcFGItem AS CHARACTER NO-UNDO .
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO .
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER NO-UNDO .
    DEFINE BUFFER bf-itemfg FOR itemfg .

      DO WITH FRAME {&FRAME-NAME}: 
          
          FIND FIRST oe-ordl NO-LOCK
              WHERE oe-ordl.company EQ cocode
              AND oe-ordl.ord-no  EQ ipiOrder
              AND oe-ordl.i-no    EQ ipcFGItem
              AND oe-ordl.job-no  EQ ipcJobNo
              AND oe-ordl.job-no2 EQ ipiJobNo2 NO-ERROR.
          IF AVAIL oe-ordl THEN
              fg-rctd.qty-case:SCREEN-VALUE = string(oe-ordl.cas-cnt) .
          ELSE do:
              FIND FIRST bf-itemfg NO-LOCK
                  WHERE bf-itemfg.company EQ cocode
                  AND bf-itemfg.i-no    EQ ipcFGItem NO-ERROR .  
               IF AVAIL bf-itemfg THEN
                   fg-rctd.qty-case:SCREEN-VALUE     = string(bf-itemfg.case-count) .
          END.
      END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetGlobalSettings Dialog-Frame 
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

    RUN sys/ref/nk1look.p (ipcCompany, "FGUnderOver", "C", NO, NO, "", "", OUTPUT gcFGUnderOver, OUTPUT lFound).

    RUN sys/ref/nk1look.p (ipcCompany, "FGUnderOver", "I", NO, NO, "", "", OUTPUT giFGUnderOver, OUTPUT lFound).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-bin-info Dialog-Frame 
PROCEDURE show-bin-info :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li AS INTEGER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        RUN get-def-values.
    
        ASSIGN
            li = TRUNC(INT(fg-rctd.t-qty:SCREEN-VALUE ) /
                INT(fg-rctd.qty-case:SCREEN-VALUE ),0).
    
        ASSIGN
            fg-rctd.cases:SCREEN-VALUE   = STRING(li)
            fg-rctd.partial:SCREEN-VALUE = STRING(INT(fg-rctd.t-qty:SCREEN-VALUE ) -
               (li * DEC(fg-rctd.qty-case:SCREEN-VALUE ))).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-freight Dialog-Frame 
PROCEDURE show-freight :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ld AS DECIMAL NO-UNDO.


    IF glFGPOFrt THEN 
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            ld                            = DEC(fg-rctd.frt-cost:SCREEN-VALUE )
            fg-rctd.ext-cost:SCREEN-VALUE = STRING(DEC(fg-rctd.ext-cost:SCREEN-VALUE ) - ld) .

        RUN get-freight-cost (OUTPUT ld).

        ASSIGN
            fg-rctd.frt-cost:SCREEN-VALUE = STRING(ld)
            fg-rctd.ext-cost:SCREEN-VALUE = STRING(DEC(fg-rctd.ext-cost:SCREEN-VALUE ) + ld).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-sequence Dialog-Frame 
PROCEDURE tag-sequence :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-tag-seq AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-locode  AS cha     NO-UNDO.

  
    ASSIGN 
        v-tag-seq = 0
        v-locode  = "".

    DO WHILE TRUE WITH FRAME {&FRAME-NAME}:
        FIND FIRST b-fg-rctd
            WHERE b-fg-rctd.company EQ cocode
            AND b-fg-rctd.loc     GT v-locode
            NO-LOCK NO-ERROR.

        IF AVAILABLE b-fg-rctd THEN 
        DO:
            v-locode = b-fg-rctd.loc.

            FOR EACH b-fg-rctd WHERE b-fg-rctd.company EQ cocode
                AND b-fg-rctd.loc     EQ v-locode
                AND b-fg-rctd.tag     BEGINS string(int(fg-rctd.po-no:SCREEN-VALUE ),"999999")
                USE-INDEX tag NO-LOCK
                BY b-fg-rctd.tag DESCENDING:

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
        v-tag-seq = v-tag-seq + 1.
/*   fg-rctd.tag:SCREEN-VALUE 
          = string(int(fg-rctd.po-no:SCREEN-VALUE ),"999999") + string(v-tag-seq,"99").
*/          

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-tt Dialog-Frame 
PROCEDURE update-tt :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
  
    DO WITH FRAME {&frame-name}:
        FIND FIRST tt-fg-rctd NO-ERROR.
        IF AVAILABLE tt-fg-rctd THEN 
        DO:
            RUN display-po (tt-fg-rctd.po-rowid).
            RUN show-bin-info.
        END.

        IF poSelected EQ 1 THEN
            APPLY "entry" TO fg-rctd.loc .
        ELSE
            RUN dispatch ("update-record").
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-ttt Dialog-Frame 
PROCEDURE update-ttt :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/

    FIND FIRST tt-fg-rctd NO-ERROR.
     
    IF AVAILABLE tt-fg-rctd THEN 
    DO WITH FRAME {&FRAME-NAME}:

        ASSIGN
            fg-rctd.po-no:SCREEN-VALUE = tt-fg-rctd.po-no
            fg-rctd.i-no:SCREEN-VALUE  = tt-fg-rctd.i-no
            fg-rctd.t-qty:SCREEN-VALUE = STRING(tt-fg-rctd.t-qty)
            tt-fg-rctd.tt-rowid        = ROWID(fg-rctd).

        RUN show-bin-info.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-blank-qty Dialog-Frame 
PROCEDURE valid-blank-qty :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF dec(fg-rctd.t-qty:SCREEN-VALUE ) EQ 0 THEN 
        DO:
            /* In case user pressed save before tab out of tag field */
            fg-rctd.t-qty:SCREEN-VALUE  =
                STRING(INT(fg-rctd.cases:SCREEN-VALUE) *
                INT(fg-rctd.qty-case:SCREEN-VALUE) +
                INT(fg-rctd.partial:SCREEN-VALUE)).
            IF dec(fg-rctd.t-qty:SCREEN-VALUE ) EQ 0 THEN 
            DO:   
                MESSAGE "Receipt quantity cannot be 0."
                    VIEW-AS ALERT-BOX.
                APPLY "entry" TO fg-rctd.cases.
                RETURN ERROR.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cases Dialog-Frame 
PROCEDURE valid-cases :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ip-focus AS WIDGET-HANDLE NO-UNDO.

    DEFINE VARIABLE lv-msg   AS CHARACTER NO-UNDO.
  
    DEFINE VARIABLE llValid  AS LOG       NO-UNDO.
    DEFINE VARIABLE lcJobNo  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcJobNo2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcLoc    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcLocBin AS CHARACTER NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

        IF INTE(ip-focus:SCREEN-VALUE)
            GE 0 AND fg-rctd.tag:SCREEN-VALUE  GT "" THEN 
        DO:
  
            IF lv-msg EQ "" AND NOT ip-set-parts AND
                (CAN-FIND(FIRST b-fg-rctd
                WHERE b-fg-rctd.company   EQ cocode
                AND b-fg-rctd.tag       EQ fg-rctd.tag:SCREEN-VALUE 
                AND b-fg-rctd.rita-code NE "P"
                AND RECID(b-fg-rctd)    NE RECID(fg-rctd)) OR
                CAN-FIND(FIRST b-fg-rdtlh
                WHERE b-fg-rdtlh.company   EQ cocode
                AND b-fg-rdtlh.tag       EQ fg-rctd.tag:SCREEN-VALUE 
                AND b-fg-rdtlh.qty       GT 0
                AND b-fg-rdtlh.rita-code NE "S"))          AND
                (INTE(fg-rctd.cases:SCREEN-VALUE ) > 0 OR
                CAN-FIND(FIRST b2-fg-rdtlh
                WHERE b2-fg-rdtlh.company   EQ cocode
                AND b2-fg-rdtlh.tag       EQ fg-rctd.tag:SCREEN-VALUE 
                AND b2-fg-rdtlh.qty       LT ABS(INTE(fg-rctd.cases:SCREEN-VALUE))
                AND b2-fg-rdtlh.rita-code NE "S") ) THEN
                ASSIGN lv-msg = "Tag# has already been used, please re-enter".
  
            IF lv-msg EQ "" AND NOT ip-set-parts AND v-copy-mode AND ip-focus:SCREEN-VALUE NE "" AND
                CAN-FIND(FIRST b-fg-rctd
                WHERE b-fg-rctd.company EQ cocode
                AND b-fg-rctd.tag     EQ fg-rctd.tag:SCREEN-VALUE 
                AND b-fg-rctd.rita-code NE "P"
                AND b-fg-rctd.r-no    NE 0) THEN
                ASSIGN lv-msg = "Tag# has already been used, please re-enter".
        
            IF lv-msg NE "" THEN 
            DO:
                MESSAGE "Tag# has already been used, please enter a negative quantity" + "..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO ip-focus.
                RETURN ERROR.
            END.
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
    DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.
    DEFINE VARIABLE lActive AS LOG NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        IF glOEShip                   AND
            CAN-FIND(FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ fg-rctd.i-no:SCREEN-VALUE 
            AND itemfg.isaset
            AND itemfg.alloc) THEN 
        DO:
            MESSAGE TRIM(ip-focus:LABEL) + " may not be an unassembled set header...".
            APPLY "entry" TO ip-focus.
            RETURN ERROR.
        END.
        RUN fg/GetItemfgActInact.p (INPUT cocode,
            INPUT fg-rctd.i-no:SCREEN-VALUE ,
            OUTPUT lActive).
        IF NOT lActive THEN 
        DO:
            /*                                                                                                       */
            /*     FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"                                         */
            /*                           AND reftable.company  EQ cocode                                             */
            /*                           AND reftable.loc      EQ ""                                                 */
            /*                           AND reftable.code     EQ fg-rctd.i-no:SCREEN-VALUE  */
            /*                           NO-LOCK NO-ERROR.                                                           */
            /*         IF AVAIL reftable AND reftable.code2 = "I" THEN DO:                                           */
            MESSAGE fg-rctd.i-no:SCREEN-VALUE  + " has InActive Status. Receipt cannot be created for the Inactive Item."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fg-rctd.i-no.
            RETURN ERROR.
        END.   

        IF NOT CAN-FIND(FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ fg-rctd.i-no:SCREEN-VALUE 
            AND itemfg.prod-uom NE "") THEN 
        DO:
    
            MESSAGE fg-rctd.i-no:SCREEN-VALUE  + " has no cost UOM. Please correct the item master and try again."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fg-rctd.i-no.
            RETURN ERROR.
        END. 

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no Dialog-Frame 
PROCEDURE valid-job-no :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF fg-rctd.job-no:SCREEN-VALUE  GT "" THEN 
        DO WITH FRAME {&frame-name}:
            fg-rctd.job-no:SCREEN-VALUE  =
                FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE ))) +
                TRIM(fg-rctd.job-no:SCREEN-VALUE ).

            IF TRIM(fg-rctd.job-no:SCREEN-VALUE ) NE TRIM(lv-job-no)  OR
                DEC(fg-rctd.job-no2:SCREEN-VALUE ) NE DEC(lv-job-no2) THEN
                RUN new-job-no.

            IF fg-rctd.job-no:SCREEN-VALUE  = "" 
                AND INT(fg-rctd.po-no:SCREEN-VALUE ) NE 0 THEN 
            DO:
                fg-rctd.job-no:SCREEN-VALUE  = "".
                MESSAGE "You may only enter a Job or a PO, Job No will be erased..."
                    VIEW-AS ALERT-BOX ERROR.
            END.
            FIND FIRST job-hdr
                WHERE job-hdr.company EQ cocode
                AND job-hdr.job-no  EQ fg-rctd.job-no:SCREEN-VALUE
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE job-hdr THEN 
            DO:
                MESSAGE "Invalid Job#. Try Help..." VIEW-AS ALERT-BOX ERROR.
                RETURN ERROR.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no2 Dialog-Frame 
PROCEDURE valid-job-no2 :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-ans AS LOG NO-UNDO.
    DEFINE VARIABLE lv-err AS LOG INIT NO NO-UNDO.

    DO WITH FRAME {&frame-name}:
        IF NOT AVAILABLE fg-rctd AND INTEGER(fg-rctd.r-no:SCREEN-VALUE ) GT 0 THEN 
        DO:
            FIND FIRST fg-rctd
                WHERE fg-rctd.r-no EQ INTEGER(fg-rctd.r-no:SCREEN-VALUE ) 
                NO-LOCK NO-ERROR.
        END.  /*Mode 001*/

        fg-rctd.job-no:SCREEN-VALUE  =
            FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE ))) +
            TRIM(fg-rctd.job-no:SCREEN-VALUE ). /*Mode 001*/

        IF TRIM(fg-rctd.job-no:SCREEN-VALUE ) NE TRIM(lv-job-no)  OR
            DEC(fg-rctd.job-no2:SCREEN-VALUE ) NE DEC(lv-job-no2) THEN
            RUN new-job-no.

        IF fg-rctd.job-no:SCREEN-VALUE  NE "" THEN 
        DO:
            FOR EACH job-hdr
                WHERE job-hdr.company EQ cocode
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
          
            IF NOT AVAILABLE job-hdr THEN
                FOR EACH job
                    WHERE job.company EQ cocode
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

            IF NOT AVAILABLE job-hdr THEN 
            DO:
                MESSAGE "Invalid Job#. Try Help..." VIEW-AS ALERT-BOX ERROR.
                lv-err = YES.
            END.      

            IF NOT lv-err AND NOT lv-closed-checked AND
                job.opened EQ NO                     THEN 
            DO:
                ASSIGN
                    lv-ans            = NO
                    lv-closed-checked = YES.

                /* gdm - 11160901 */
                IF glJobReopn EQ YES 
                    THEN
                    MESSAGE 
                        "Job is CLOSED, would you like to reopen?"
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL UPDATE lv-ans.
                ELSE 
                    ASSIGN lv-ans = glJobReopn.
                /* gdm - 11160901 end */
        
                CASE lv-ans:
                    WHEN YES THEN RUN jc/jc-reopn.p (ROWID(job)).
                    WHEN NO  THEN.
                    OTHERWISE 
                    lv-err = YES.
                END CASE.
            END.
        END.

        IF lv-err THEN 
        DO:
            lv-closed-checked = NO.
            APPLY "entry" TO fg-rctd.job-no .
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-lot# Dialog-Frame 
PROCEDURE valid-lot# :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        IF ip-focus:SCREEN-VALUE NE "" THEN 
        DO:
            IF fg-rctd.tag:SCREEN-VALUE  = ""  THEN 
            DO:
                MESSAGE TRIM(ip-focus:LABEL) + " may not be entered when tag# is blank".
                APPLY "entry" TO ip-focus.
                RETURN ERROR.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no Dialog-Frame 
PROCEDURE valid-po-no :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-type AS INTEGER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT AVAILABLE fg-rctd AND INTEGER(fg-rctd.r-no:SCREEN-VALUE ) GT 0 THEN 
        DO:
            FIND FIRST fg-rctd
                WHERE fg-rctd.r-no EQ INTEGER(fg-rctd.r-no:SCREEN-VALUE ) 
                NO-LOCK NO-ERROR.
        END.  /*Mode 001*/

        IF INT(fg-rctd.po-no:SCREEN-VALUE ) NE 0              AND
            NOT CAN-FIND(FIRST tt-fg-rctd WHERE tt-fg-rctd.tt-rowid EQ ROWID(fg-rctd)) THEN 
        DO:
            IF fg-rctd.job-no:SCREEN-VALUE  NE "" THEN 
            DO:
                MESSAGE "You may only enter a Job or a PO, PO will be erased..." VIEW-AS ALERT-BOX ERROR.
                fg-rctd.po-no:SCREEN-VALUE  = "".
                RETURN.
            END.

            FIND FIRST po-ordl
                WHERE po-ordl.company   EQ cocode
                AND po-ordl.po-no     EQ INT(fg-rctd.po-no:SCREEN-VALUE )
                AND po-ordl.item-type EQ NO
                AND (po-ordl.i-no     EQ fg-rctd.i-no:SCREEN-VALUE  OR
                fg-rctd.i-no:SCREEN-VALUE  EQ "")
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE po-ordl THEN 
            DO:
                IF ip-type NE 0 THEN 
                DO:
                    MESSAGE "Invalid PO#, try help..." VIEW-AS ALERT-BOX ERROR.
                    APPLY "entry" TO fg-rctd.po-no .
                END.
                RETURN ERROR.
            END.

            ASSIGN 
                fg-rctd.i-name:SCREEN-VALUE = po-ordl.i-name.

            FIND FIRST po-ord
                WHERE po-ord.company EQ po-ordl.company
                AND po-ord.po-no   EQ po-ordl.po-no
                NO-LOCK NO-ERROR.
          
            IF AVAILABLE po-ord AND po-ord.stat = "H" AND glPOHoldReceipts THEN 
            DO: /* ticket 17372 */
                MESSAGE "Unable to receive goods or materials for a purchase order that is on hold!"
                    VIEW-AS ALERT-BOX ERROR. 
                RETURN ERROR.
            END.
      
            /* WFK - Task 09261318 - Don't pull qty from PO if there is a tag per Joe, */
            /* so not running create-from-po if there is                               */
            IF ip-type EQ 1 AND
                AVAILABLE po-ord AND
                adm-adding-record AND
                NOT CAN-FIND(FIRST tt-fg-rctd) THEN 
            DO:
                RUN fg/d-selpos.w (ROWID(po-ord), NO).
                RUN create-from-po.
            /*                RUN update-ttt.*/
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-porec-qty Dialog-Frame 
PROCEDURE valid-porec-qty :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipRecQty AS INTEGER NO-UNDO.
    IF lAllowUserOverRun THEN ASSIGN giFGUnderOver = 0 .
    IF glFGUnderOver 
    AND (gcFGUnderOver EQ "OverRuns Only" OR gcFGUnderOver EQ "UnderRuns and OverRun")
    AND ipRecQty GT po-ordl.ord-qty * (1 + (po-ordl.over-pct / 100)) AND NOT lv-overrun2-checked  THEN DO:  
        IF giFGUnderOver EQ 1 THEN DO:        
            MESSAGE 
                "The PO Quantity entered is more than the" STRING(po-ordl.over-pct,">>9.99%") SKIP 
                "Overrun allowed for this PO line Item, and excess overruns are not allowed."
                VIEW-AS ALERT-BOX WARNING .
            lv-overrun2-checked = YES.
            lFatalQtyError = YES.
        END.
        ELSE DO:
            MESSAGE 
                "The PO Quantity entered is more than the" STRING(po-ordl.over-pct,">>9.99%") SKIP 
                "Overrun allowed for this PO line Item..."
                VIEW-AS ALERT-BOX WARNING .
            lv-overrun2-checked = YES.
        END.
    END.
    ELSE IF glFGUnderOver 
    AND (gcFGUnderOver EQ "UnderRuns Only" OR gcFGUnderOver EQ "UnderRuns and OverRun")
    AND ipRecQty LT po-ordl.ord-qty - (po-ordl.ord-qty * po-ordl.under-pct / 100) AND NOT lv-overrun2-checked THEN DO:
            MESSAGE 
                "The PO Quantity entered is less than the" STRING(po-ordl.under-pct,">>9.99%") SKIP 
                "Underrun allowed for this PO line Item..."
                VIEW-AS ALERT-BOX WARNING .
            lv-overrun2-checked = YES.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty Dialog-Frame 
PROCEDURE valid-qty :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ip-focus AS WIDGET-HANDLE NO-UNDO.

    DEFINE VARIABLE lv-msg  AS CHARACTER NO-UNDO.
  
    DEFINE VARIABLE llValid AS LOG       NO-UNDO.
    DEFINE VARIABLE dMaxQty AS DECIMAL   NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        IF ip-focus:SCREEN-VALUE NE "" THEN 
        DO:
            IF DECIMAL(ip-focus:SCREEN-VALUE) EQ 0 THEN
                lv-msg = "Quantity cannot be zero".
            dMaxQty = MaxComponentQty().
            IF ABS(DECIMAL(ip-focus:SCREEN-VALUE)) GT ABS(dMaxQty) AND dMaxQty GT 0 THEN 
                lv-msg = "Quantity cannot be greater than " + STRING(dMaxQty) + " for this component.".
    
            IF lv-msg NE "" THEN 
            DO:
                MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO ip-focus.
                RETURN ERROR.
            END.
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
    DEFINE INPUT PARAMETER ip-focus AS WIDGET-HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER op-negative AS LOG NO-UNDO.

    DEFINE VARIABLE lv-msg            AS CHARACTER NO-UNDO.
  
    DEFINE VARIABLE llValid           AS LOG       NO-UNDO.
    DEFINE VARIABLE lcJobNo           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcJobNo2          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcLoc             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcLocBin          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lTagError         AS LOG       NO-UNDO.
    DEFINE VARIABLE lTagErrorAccepted AS LOG       NO-UNDO.
    DEFINE VARIABLE lOk               AS LOG       NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

        IF ip-focus:SCREEN-VALUE NE "" THEN 
        DO:

            IF lv-msg EQ "" AND NOT ip-set-parts AND
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
                (INTE(fg-rctd.cases:SCREEN-VALUE ) > 0 OR
                CAN-FIND(FIRST b2-fg-rdtlh
                WHERE b2-fg-rdtlh.company   EQ cocode
                AND b2-fg-rdtlh.tag       EQ ip-focus:SCREEN-VALUE
                AND b2-fg-rdtlh.qty       LT ABS(INTE(fg-rctd.cases:SCREEN-VALUE))
                AND b2-fg-rdtlh.rita-code NE "S") ) THEN
                ASSIGN lv-msg    = "Tag# has already been used, please re-enter"
                    lTagError = TRUE.

            IF lv-msg EQ "" AND NOT ip-set-parts AND v-copy-mode AND ip-focus:SCREEN-VALUE NE "" AND
                CAN-FIND(FIRST b-fg-rctd
                WHERE b-fg-rctd.company EQ cocode
                AND b-fg-rctd.tag     EQ ip-focus:SCREEN-VALUE
                AND b-fg-rctd.rita-code NE "P"
                AND b-fg-rctd.r-no    NE 0) THEN
                ASSIGN lv-msg    = "Tag# has already been used, please re-enter"
                    lTagError = TRUE.

            /* If this is a negative return, tag will be already used */
            /* This allows this procedure to be called from local-update */
            IF lTagError AND (INTE(fg-rctd.cases:SCREEN-VALUE)
                * INTE(fg-rctd.qty-case:SCREEN-VALUE)) LT 0 THEN
                ASSIGN lTagError = FALSE
                    lv-msg    = "".

            IF lTagError THEN 
            DO:  
                IF FOCUS:NAME EQ "btn-save" THEN 
                DO:
                    MESSAGE "Tag# has already been used, please re-enter" + "..." VIEW-AS ALERT-BOX ERROR.
                    APPLY "entry" TO fg-rctd.cases .
                    RETURN ERROR.
                END.
                ELSE 
                DO:        
                    /* Ask if this will be a negative return */
                    MESSAGE "Tag has already been used." SKIP
                        "If this is a negative return, click OK," SKIP
                        "Otherwise, click CANCEL to try a different tag."
                        VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL UPDATE lOk.
                    IF lOk THEN
                        ASSIGN lTagErrorAccepted = TRUE
                            op-negative       = TRUE.
                    ELSE 
                    DO:
                        APPLY "entry" TO ip-focus.
                        RETURN ERROR.
                    END.
                    IF lTagErrorAccepted THEN
                        lv-msg = "".
                END.
            END.

            /* check for assembled and unassembled set parts on-hand or pending receipt*/
            IF lv-msg EQ "" AND ip-set-parts THEN 
                RUN fg/ValidFGRcptTagSP.p (INPUT ROWID(fg-rctd),
                    INPUT ip-focus:SCREEN-VALUE,
                    INPUT INT(fg-rctd.t-qty:SCREEN-VALUE ),
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
                    fg-rctd.job-no:SCREEN-VALUE  = lcJobNo
                    fg-rctd.job-no2:SCREEN-VALUE = lcJobNo2
                    fg-rctd.loc:SCREEN-VALUE     = lcLoc
                    fg-rctd.loc-bin:SCREEN-VALUE = lcLocBin.
            /*           AND int(fg-rctd.t-qty:SCREEN-VALUE ) < 0 THEN DO:                */
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
            /*                 fg-rctd.job-no:SCREEN-VALUE  = bf-fg-bin.job-no            */
            /*                 fg-rctd.job-no2:SCREEN-VALUE  = string(bf-fg-bin.job-no2). */
            /*                                                                                                    */
            /*         ELSE                                                                                       */
            /*             lv-msg = "Invalid Tag#, try help or scan valid tag#".                                  */
            /*         IF lv-msg EQ ""                                                                            */
            /*             AND iTotalQty LT ABS(int(fg-rctd.t-qty:SCREEN-VALUE )) THEN    */
            /*             lv-msg = "Insufficient quantity in bin".                                               */
            /*       END.                                                                                         */
            FIND FIRST loadtag NO-LOCK
                WHERE loadtag.company   EQ cocode
                AND loadtag.item-type EQ NO
                AND loadtag.tag-no    EQ ip-focus:SCREEN-VALUE
                NO-ERROR.
            IF lv-msg EQ ""                                                   AND
                giFGRecpt EQ 1                                               AND
                NOT AVAILABLE loadtag THEN
                lv-msg = "Invalid Tag#, try help or scan valid tag#".
                
            IF lv-msg EQ "" AND AVAILABLE loadtag 
                AND loadtag.i-no NE fg-rctd.i-no:SCREEN-VALUE THEN 
                lv-msg = "Tag# is associated with a different item number.".
                
            IF lv-msg NE "" THEN 
            DO:
                MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO ip-focus.
                RETURN ERROR.
            END.
        END. /* If screen value not blank */
    END. /* DO with frame */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom Dialog-Frame 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/

    DEFINE VARIABLE lv-uom-list AS cha       NO-UNDO.
    DEFINE VARIABLE lv-uom      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-uom-help AS CHARACTER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            fg-rctd.cost-uom:SCREEN-VALUE = CAPS(fg-rctd.cost-uom:SCREEN-VALUE )
            lv-uom                        = fg-rctd.cost-uom:SCREEN-VALUE .

        RUN sys/ref/uom-fg.p (NO, OUTPUT lv-uom-list).

        lv-uom-help = "Must enter one of the following as the UOM: " + lv-uom-list.

        IF INDEX(lv-uom-list,lv-uom) LE 0 THEN 
        DO:
            MESSAGE TRIM(lv-uom-help) + "..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fg-rctd.cost-uom .
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-record Dialog-Frame 
PROCEDURE validate-record :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li-max-qty AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll         AS LOG     NO-UNDO.
    DEFINE VARIABLE lActiveBin AS LOGICAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:  
        FIND itemfg WHERE itemfg.company = cocode
            AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE 
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE itemfg THEN 
        DO:
            IF fg-rctd.i-no:SCREEN-VALUE = "" THEN 
            DO:
                MESSAGE "Invalid Item. Try help. " VIEW-AS ALERT-BOX.
                APPLY "entry" TO fg-rctd.i-no .
                RETURN ERROR.
            END.
            ELSE 
            DO:
                MESSAGE  " F/G Item is not on file.  Would you like to add it? "
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
                IF NOT ll-ans THEN 
                DO:
                    APPLY "entry" TO fg-rctd.i-no .
                    RETURN ERROR.           
                END.
                ELSE 
                DO:
                    RUN fg/d-crtitm.w (fg-rctd.i-no:SCREEN-VALUE).
                    FIND FIRST itemfg {sys/look/itemfgrlW.i}
                        AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE 
                    NO-LOCK NO-ERROR.
                    IF AVAILABLE itemfg THEN ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name.
                END.
            END.
        END.
      
        RUN valid-i-no (fg-rctd.i-no:HANDLE ) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        IF itemfg.isaset                                                        AND
            (itemfg.alloc EQ NO                OR
            (itemfg.alloc EQ YES      AND
            gcFGRecpt NE "Manual" AND
            TRIM(fg-rctd.job-no:SCREEN-VALUE ) NE "")) THEN
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN
                fg-rctd.t-qty:SCREEN-VALUE = STRING((INT(fg-rctd.cases:SCREEN-VALUE ) *
                 INT(fg-rctd.qty-case:SCREEN-VALUE )) +
                INT(fg-rctd.partial:SCREEN-VALUE ),"->>>,>>>,>>9.99")
                li-max-qty                 = DEC(fg-rctd.t-qty:SCREEN-VALUE ).

            /* Only need to check components if creating adjustments for components */
            IF giFGSetRec NE 1 THEN 
            DO:    
                RUN fg/checksetb.p (ROWID(itemfg),
                    ROWID(fg-rctd),
                    fg-rctd.job-no:SCREEN-VALUE ,
                    INT(fg-rctd.job-no2:SCREEN-VALUE ),
                    INPUT fg-rctd.loc:SCREEN-VALUE ,
                    INPUT-OUTPUT li-max-qty).
      
                IF li-max-qty LT DEC(fg-rctd.t-qty:SCREEN-VALUE ) THEN 
                DO:
                    ll = NO.

                    RUN fg/cmpQtyMsg.w (ROWID(itemfg),
                        ROWID(fg-rctd),
                        fg-rctd.job-no:SCREEN-VALUE ,
                        INT(fg-rctd.job-no2:SCREEN-VALUE ),
                        INPUT DEC(fg-rctd.t-qty:SCREEN-VALUE ),
                        INPUT fg-rctd.loc:SCREEN-VALUE ,
                        OUTPUT ll).
        
                    IF ll THEN  
                        ASSIGN
                            fg-rctd.t-qty:SCREEN-VALUE   = STRING(li-max-qty)
                            fg-rctd.cases:SCREEN-VALUE   = STRING(TRUNC((li-max-qty - DEC(fg-rctd.partial:SCREEN-VALUE )) /
                             DEC(fg-rctd.qty-case:SCREEN-VALUE ),0))
                            fg-rctd.partial:SCREEN-VALUE = STRING(li-max-qty - (DEC(fg-rctd.cases:SCREEN-VALUE ) *
                                     DEC(fg-rctd.qty-case:SCREEN-VALUE ))).
  
                    IF NOT ll OR li-max-qty EQ 0 THEN 
                    DO:
                        APPLY "entry" TO fg-rctd.cases .
                        RETURN ERROR.
                    END.
                END.
            END.
            ELSE 
            DO:
                IF li-max-qty EQ 0 THEN 
                DO:
                    APPLY "entry" TO fg-rctd.cases .
                    RETURN ERROR.
                END.
            END.
        END.
 
        RUN ValidateLoc IN hInventoryProcs (cocode, fg-rctd.loc:SCREEN-VALUE , OUTPUT lActiveBin).
        IF NOT lActiveBin THEN 
        DO:
            MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fg-rctd.loc.
            RETURN ERROR.
        END.
  
        RUN ValidateBin IN hInventoryProcs (cocode, fg-rctd.loc:SCREEN-VALUE , 
            fg-rctd.loc-bin:SCREEN-VALUE , 
            OUTPUT lActiveBin ).
        IF NOT lActiveBin THEN 
        DO:
            MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fg-rctd.loc-bin.
            RETURN ERROR.
        END.

        RUN get-matrix (NO) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        RUN get-matrix-all (FALSE) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        IF INT(fg-rctd.cases-unit:SCREEN-VALUE) < 1 THEN 
        DO:  /* task# 06200520*/
            MESSAGE "Unit/Pallet must be greater than or equal to 1." VIEW-AS ALERT-BOX.
            APPLY "entry" TO fg-rctd.cases-unit .
            RETURN ERROR.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION maxComponentQty Dialog-Frame 
FUNCTION maxComponentQty RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cHeaderItem AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dMaxQty     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dMaxCompQty AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE hProc       AS HANDLE    NO-UNDO.
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
    IF AVAILABLE bfItemfg THEN
        RUN fg/fullset.p (INPUT ROWID(bfItemFg)).

    FIND FIRST tt-fg-set WHERE tt-fg-set.part-no EQ fg-rctd.i-no
        NO-LOCK NO-ERROR.
  
    IF AVAILABLE tt-fg-set THEN
        dMaxCompQty = dMaxQty * tt-fg-set.part-qty-dec.
    RETURN dMaxCompQty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

