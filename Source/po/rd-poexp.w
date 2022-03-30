&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-poexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-poexp 
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
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER piPOFrom LIKE po-ordl.po-no NO-UNDO.
DEFINE INPUT PARAMETER piPOTo LIKE po-ordl.po-no NO-UNDO.
DEFINE INPUT PARAMETER pcVendFrom LIKE po-ord.vend-no NO-UNDO.
DEFINE INPUT PARAMETER pcVendTo   LIKE po-ord.vend-no NO-UNDO.
DEFINE INPUT PARAMETER pcItemFrom LIKE po-ordl.i-no NO-UNDO.
DEFINE INPUT PARAMETER pcItemTo   LIKE po-ordl.i-no NO-UNDO.
DEFINE INPUT PARAMETER pcVendItemFrom LIKE po-ordl.vend-i-no NO-UNDO.
DEFINE INPUT PARAMETER pcVendItemTo   LIKE po-ordl.vend-i-no NO-UNDO.
DEFINE INPUT PARAMETER pcJobFrom LIKE po-ordl.job-no NO-UNDO.
DEFINE INPUT PARAMETER pcJobTo   LIKE po-ordl.job-no NO-UNDO.
DEFINE INPUT PARAMETER piJob2From LIKE po-ordl.job-no2 NO-UNDO.
DEFINE INPUT PARAMETER piJob2To   LIKE po-ordl.job-no2 NO-UNDO.
DEFINE INPUT PARAMETER pdDateFrom  LIKE po-ordl.due-date NO-UNDO.
DEFINE INPUT PARAMETER pdDateTo    LIKE po-ordl.due-date NO-UNDO.
DEFINE INPUT PARAMETER piOpenClosed AS INTEGER NO-UNDO.


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.


ASSIGN 
    cTextListToSelect = "PO #,Vendor #,Due Date,Ship ID(Vendor or Cust ShipId or Company),Ship Name," +
                            "Ship Address 1,Ship Address 2,Ship City,Ship State,Ship Zip," +
                            "Shipping Carrier,Total Freight,Freight Payment,FOB," +
                            "Tax Group,Tax,Taxable,Payment Terms,Total Cost," +
                            "Job #,Item Type,Item #,Item Name,Width,Length," +
                            "Form #,Blank #,Description 1,Description 2," +
                            "Vendor Item #,Order Qty,Order UOM," +
                            "Qty Received,Rec. UOM,Item Cost,UOM,Buyer," +
                            "Status,Item Status,Printed,Opened,Type,Contact," +
                            "PO Date,Last Ship Date," +
                            "Vendor Name,Vendor Address 1,Vendor Address 2,Vendor City,Vendor State,Vendor Zip," +
                            "Setup,Discount,GL Number,Overrun,Underrun," +
                            "Customer #,Order #,Customer # From Order,FG Item # From Job,Cust Part#,Adder," +
                            "RM Item Code,FG Item Code,Style from Job,Buyer ID,User ID,Po Line," +
                            "ShipTo Customer,Drop Shipment Type,RM Category,RM Category description,FG Category,FG Category description,Required Date," +
                            "PoDate Change Notes,Required Date Change Notes,Last Ship Date Change Notes,Due Date Change Notes".
cFieldListToSelect = "po-ordl.po-no,po-ord.vend-no,po-ordl.due-date,po-ord.ship-id,po-ord.ship-name," +
    "po-ord.ship-addr[1],po-ord.ship-addr[2],po-ord.ship-city,po-ord.ship-state,po-ord.ship-zip," +
    "po-ord.carrier,po-ord.t-freight,po-ord.frt-pay,po-ord.fob-code," +
    "po-ord.tax-gr,po-ord.tax,po-ordl.tax,po-ord.terms,po-ord.t-cost," +
    "po-ordl.job-no,po-ordl.item-type,po-ordl.i-no,po-ordl.i-name,po-ordl.s-wid,po-ordl.s-len," +
    "po-ordl.s-num,po-ordl.b-num,po-ordl.dscr[1],po-ordl.dscr[2]," +
    "po-ordl.vend-i-no,po-ordl.ord-qty,po-ordl.pr-qty-uom," +
    "po-ordl.t-rec-qty,po-ordl.cons-uom,po-ordl.cost,po-ordl.pr-uom,po-ord.buyer," +
    "po-ord.stat,po-ordl.stat,po-ord.printed,po-ordl.opened,po-ord.type,po-ord.contact," +
    "po-ord.po-date,po-ord.last-ship-date," +
    "vend.name,vend.add1,vend.add2,vend.city,vend.state,vend.zip," +
    "po-ordl.setup,po-ordl.disc,po-ordl.actnum,po-ordl.over-pct,po-ordl.under-pct," +
    "po-ordl.cust-no,po-ordl.ord-no,po-ordl.dfuncCustfromOrder,po-ordl.dfuncFGFromJob,cust-part,adders," +
    "rm-item,fg-item,style-job,po-ord.buyer,po-ord.user-id,po-ordl.line," +
    "shipto-cust,dropshipment,rm-cat,rm-cat-dscr,fg-cat,fg-cat-dscr,po-ord.due-date," +
    "po-date-notes,required-date-note,lastShip-date-notes,due-date-notes".

/*vend.name
       lv_vend-add1:SCREEN-VALUE  = vend.add1
       lv_vend-add2:SCREEN-VALUE  = vend.add2
       lv_vend-city:SCREEN-VALUE  = vend.city
       lv_vend-state:SCREEN-VALUE = vend.state
       lv_vend-zip:SCREEN-VALUE   = vend.zip.*/
{sys/inc/ttRptSel.i}

ASSIGN 
    cTextListToDefault = "Vendor #,PO #,Po Line,Due Date,Ship ID(Vendor or Cust ShipId or Company),Ship Name," +
                            "Ship Address 1,Ship Address 2,Ship City,Ship State,Ship Zip," +
                            "Shipping Carrier,Total Freight,Freight Payment,FOB," +
                            "Tax Group,Tax,Taxable,Payment Terms,Total Cost," +
                            "Job #,Item Type,Item #,Item Name,Width,Length," +
                            "Form #,Blank #,Description 1,Description 2," +
                            "Vendor Item #,Order Qty,Order UOM," +
                            "Qty Received,Rec. UOM,Item Cost,UOM,Buyer," +
                            "Status,Item Status,Printed,Opened,Type,Contact," +
                            "PO Date,Last Ship Date," +                             
                            "Setup,Discount,GL Number,Overrun,Underrun," +
                            "Customer #,Order #,ShipTo Customer,Drop Shipment Type,Required Date".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-poexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rd_open-closed rd_printed begin_po end_po ~
begin_vend-no end_vend-no begin_item end_item begin_vend-i-no end_vend-i-no ~
begin_job begin_job2 end_job end_job2 begin_date end_date sl_avail Btn_Def ~
Btn_Add sl_selected Btn_Remove btn_Up btn_down tb_OpenCSV fi_file btn-ok ~
btn-cancel begin_po-date end_po-date begin_buyer end_buyer begin_user-id ~
end_user-id tbAutoClose RECT-6 RECT-7 RECT-8 
&Scoped-Define DISPLAYED-OBJECTS rd_open-closed rd_printed begin_po end_po ~
begin_vend-no end_vend-no begin_item end_item begin_vend-i-no end_vend-i-no ~
begin_job begin_job2 end_job end_job2 begin_date end_date sl_avail ~
sl_selected tb_OpenCSV fi_file begin_po-date end_po-date begin_buyer ~
end_buyer begin_user-id end_user-id tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-poexp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-poexp 
FUNCTION assignParam RETURNS CHARACTER
    ( ipc-param AS CHARACTER , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-poexp 
FUNCTION buildHeader RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue rd-poexp 
FUNCTION getValue RETURNS CHARACTER
    ( BUFFER ipb-po-ordl FOR po-ordl, 
    BUFFER ipb-po-ord FOR po-ord, 
    BUFFER ipb-vend FOR vend, 
    ipc-field AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-po-ord rd-poexp 
FUNCTION getValue-po-ord RETURNS CHARACTER
    ( BUFFER ipb-buffer FOR po-ord, ipc-field AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-po-ordl rd-poexp 
FUNCTION getValue-po-ordl RETURNS CHARACTER
    ( BUFFER ipb-buffer FOR po-ordl, ipc-field AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-vend rd-poexp 
FUNCTION getValue-vend RETURNS CHARACTER
    ( BUFFER ipb-buffer FOR vend, ipc-field AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE BUTTON Btn_Add 
    LABEL "&Add >>" 
    SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Def 
    LABEL "&Default" 
    SIZE 16 BY 1.1.

DEFINE BUTTON btn_down 
    LABEL "Move Down" 
    SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Remove 
    LABEL "<< &Remove" 
    SIZE 16 BY 1.1.

DEFINE BUTTON btn_Up 
    LABEL "Move Up" 
    SIZE 16 BY 1.1.

DEFINE VARIABLE begin_buyer     AS CHARACTER FORMAT "X(10)" 
    LABEL "From Buyer ID" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_date      AS DATE      FORMAT "99/99/9999" INITIAL 01/01/1901 
    LABEL "From Due Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_item      AS CHARACTER FORMAT "X(15)" 
    LABEL "From FG Item #" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_job       AS CHARACTER FORMAT "X(9)" 
    LABEL "From Job #" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1.

DEFINE VARIABLE begin_job2      AS INTEGER   FORMAT "999":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po        AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    LABEL "From PO #" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_po-date   AS DATE      FORMAT "99/99/9999" INITIAL 01/01/1901 
    LABEL "From PO Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_user-id   AS CHARACTER FORMAT "X(8)" 
    LABEL "From User ID" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_vend-i-no AS CHARACTER FORMAT "X(15)" 
    LABEL "From Vendor Item #" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_vend-no   AS CHARACTER FORMAT "X(8)" 
    LABEL "From Vendor #" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_buyer       AS CHARACTER FORMAT "X(10)" INITIAL "zzzzzzzz" 
    LABEL "To Buyer ID" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date        AS DATE      FORMAT "99/99/9999" INITIAL 12/31/2099 
    LABEL "To Due Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_item        AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "To FG Item #" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_job         AS CHARACTER FORMAT "X(9)" INITIAL "zzzzzzzzz" 
    LABEL "To Job #" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1.

DEFINE VARIABLE end_job2        AS INTEGER   FORMAT ">>>":U INITIAL 999 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_po          AS INTEGER   FORMAT ">>>>>>>>" INITIAL 99999999 
    LABEL "To PO #" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_po-date     AS DATE      FORMAT "99/99/9999" INITIAL 12/31/2099 
    LABEL "To PO Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_user-id     AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "To User ID" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_vend-i-no   AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "To Vendor Item #" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_vend-no     AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "To Vendor #" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file         AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\POsExport.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE rd_open-closed  AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Open", 1,
    "Closed", 2,
    "Both", 3
    SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE rd_printed      AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Printed", 1,
    "Not Printed", 2,
    "Both", 3
    SIZE 45 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 99 BY 6.43.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 99 BY 12.38.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 99 BY 2.14.

DEFINE VARIABLE sl_avail    AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.1 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.1 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel    AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV  AS LOGICAL   INITIAL YES 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.8 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-poexp
    rd_open-closed AT ROW 2 COL 34 NO-LABELS WIDGET-ID 2
    rd_printed AT ROW 2.91 COL 34 NO-LABELS WIDGET-ID 126
    begin_po AT ROW 3.95 COL 29.8 COLON-ALIGNED HELP
    "Enter Beginning PO Number" WIDGET-ID 120
    end_po AT ROW 3.95 COL 72.8 COLON-ALIGNED HELP
    "Enter Ending PO Number" WIDGET-ID 122
    begin_vend-no AT ROW 5 COL 29.8 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number" WIDGET-ID 6
    end_vend-no AT ROW 5 COL 72.8 COLON-ALIGNED HELP
    "Enter Ending Vendor Number" WIDGET-ID 16
    begin_item AT ROW 6.05 COL 29.8 COLON-ALIGNED HELP
    "Enter Beginning Item Number" WIDGET-ID 100
    end_item AT ROW 6.05 COL 72.8 COLON-ALIGNED HELP
    "Enter Ending Item Number" WIDGET-ID 102
    begin_vend-i-no AT ROW 7.1 COL 29.8 COLON-ALIGNED HELP
    "Enter Beginning Vendor Item Number" WIDGET-ID 104
    end_vend-i-no AT ROW 7.1 COL 72.8 COLON-ALIGNED HELP
    "Enter Ending Vendor Item Number" WIDGET-ID 106
    begin_job AT ROW 8.14 COL 29.8 COLON-ALIGNED HELP
    "Enter Beginning Job Number" WIDGET-ID 108
    begin_job2 AT ROW 8.14 COL 41.6 COLON-ALIGNED HELP
    "Enter Beginning Job Number" NO-LABELS WIDGET-ID 116
    end_job AT ROW 8.14 COL 72.8 COLON-ALIGNED HELP
    "Enter Ending Job Number" WIDGET-ID 110
    end_job2 AT ROW 8.14 COL 84.5 COLON-ALIGNED HELP
    "Enter Ending Job Number" NO-LABELS WIDGET-ID 118
    begin_date AT ROW 9.19 COL 29.8 COLON-ALIGNED HELP
    "Enter Beginning Due Date" WIDGET-ID 112
    end_date AT ROW 9.19 COL 72.8 COLON-ALIGNED HELP
    "Enter Ending Due Date" WIDGET-ID 114
    sl_avail AT ROW 15.33 COL 4.4 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 15.33 COL 44 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    Btn_Add AT ROW 16.38 COL 44 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 130
    sl_selected AT ROW 15.33 COL 67.4 NO-LABELS WIDGET-ID 28
    Btn_Remove AT ROW 17.43 COL 44 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 134
    btn_Up AT ROW 18.48 COL 44 WIDGET-ID 136
    btn_down AT ROW 19.52 COL 44 WIDGET-ID 132
    tb_excel AT ROW 21.19 COL 36 WIDGET-ID 32
    tb_OpenCSV AT ROW 21.62 COL 87.8 RIGHT-ALIGNED WIDGET-ID 34
    fi_file AT ROW 21.52 COL 21.4 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 22
    btn-ok AT ROW 24.1 COL 31.6 WIDGET-ID 14
    btn-cancel AT ROW 24.1 COL 53.8 WIDGET-ID 12
    begin_po-date AT ROW 10.24 COL 29.8 COLON-ALIGNED HELP
    "Enter Beginning Due Date" WIDGET-ID 142
    end_po-date AT ROW 10.24 COL 72.8 COLON-ALIGNED HELP
    "Enter Ending Due Date" WIDGET-ID 144
    begin_buyer AT ROW 11.29 COL 29.8 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number" WIDGET-ID 146
    end_buyer AT ROW 11.29 COL 72.8 COLON-ALIGNED HELP
    "Enter Ending Vendor Number" WIDGET-ID 148
    begin_user-id AT ROW 12.33 COL 29.8 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number" WIDGET-ID 150
    end_user-id AT ROW 12.33 COL 72.8 COLON-ALIGNED HELP
    "Enter Ending Vendor Number" WIDGET-ID 152
    tbAutoClose AT ROW 23.24 COL 31.8 WIDGET-ID 64
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 14.62 COL 4.4 WIDGET-ID 140
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    BGCOLOR 15  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME rd-poexp
    "Selected Columns" VIEW-AS TEXT
    SIZE 20.6 BY .62 AT ROW 14.62 COL 67.4 WIDGET-ID 138
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 4 WIDGET-ID 36
    " Export Selection" VIEW-AS TEXT
    SIZE 17 BY .62 AT ROW 13.91 COL 4 WIDGET-ID 86
    RECT-6 AT ROW 14.33 COL 3 WIDGET-ID 30
    RECT-7 AT ROW 1.52 COL 3 WIDGET-ID 38
    RECT-8 AT ROW 21 COL 3 WIDGET-ID 84
    SPACE(0.99) SKIP(2.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    BGCOLOR 15 
    TITLE "Export POs to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd-poexp
   FRAME-NAME Custom                                                    */
ASSIGN 
    FRAME rd-poexp:SCROLLABLE = FALSE
    FRAME rd-poexp:HIDDEN     = TRUE.

ASSIGN 
    begin_buyer:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    begin_item:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    begin_job:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    begin_po:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    begin_po-date:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    begin_user-id:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    begin_vend-i-no:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    begin_vend-no:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    end_buyer:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    end_item:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    end_job:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    end_po:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    end_po-date:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    end_user-id:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    end_vend-i-no:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    end_vend-no:PRIVATE-DATA IN FRAME rd-poexp = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME rd-poexp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-poexp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_excel:HIDDEN IN FRAME rd-poexp       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME rd-poexp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME rd-poexp
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME rd-poexp = "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-poexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-poexp rd-poexp
ON HELP OF FRAME rd-poexp /* Export POs to Excel */
    DO:
        DEFINE VARIABLE lw-focus   AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE ls-cur-val AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE char-val   AS CHARACTER     NO-UNDO.

        lw-focus = FOCUS.
    
        CASE lw-focus:NAME :

            WHEN "begin_vend-no" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-vendno.w (cocode, "", ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* vend-no*/
            WHEN "end_vend-no" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-vendno.w (cocode, "", ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* vend-no*/
            WHEN "begin_item" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-itemfg.w (cocode, "", ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* itemfg */
            WHEN "end_item" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-itemfg.w (cocode, "", ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* itemfg*/
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-poexp rd-poexp
ON WINDOW-CLOSE OF FRAME rd-poexp /* Export POs to Excel */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_buyer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_buyer rd-poexp
ON LEAVE OF begin_buyer IN FRAME rd-poexp /* From Buyer ID */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date rd-poexp
ON LEAVE OF begin_date IN FRAME rd-poexp /* From Due Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_item rd-poexp
ON LEAVE OF begin_item IN FRAME rd-poexp /* From FG Item # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job rd-poexp
ON LEAVE OF begin_job IN FRAME rd-poexp /* From Job # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po rd-poexp
ON LEAVE OF begin_po IN FRAME rd-poexp /* From PO # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-date rd-poexp
ON LEAVE OF begin_po-date IN FRAME rd-poexp /* From PO Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_user-id rd-poexp
ON LEAVE OF begin_user-id IN FRAME rd-poexp /* From User ID */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-i-no rd-poexp
ON LEAVE OF begin_vend-i-no IN FRAME rd-poexp /* From Vendor Item # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no rd-poexp
ON LEAVE OF begin_vend-no IN FRAME rd-poexp /* From Vendor # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-poexp
ON CHOOSE OF btn-cancel IN FRAME rd-poexp /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-poexp
ON CHOOSE OF btn-ok IN FRAME rd-poexp /* OK */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
  
        ASSIGN 
            fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
        RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
        fi_file:SCREEN-VALUE =  cFileName.
  
        RUN GetSelectionList.  
        RUN run-report.

        IF NOT tb_OpenCSV THEN 
        DO:        
            MESSAGE "CSV file have been created." SKIP(1)
                "~"OK"~" to open CSV file?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                TITLE "" UPDATE lChoice AS LOGICAL.
                 
            IF lChoice THEN
            DO:
                OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
            END.
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY "END-ERROR":U TO SELF.                 
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-poexp
ON CHOOSE OF Btn_Add IN FRAME rd-poexp /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.

    /*
    DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
      IF sl_avail:IS-SELECTED(i) AND
        (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR sl_selected:NUM-ITEMS = 0) THEN
      /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
          cSelectedList = cSelectedList +
                          entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
    END.
    cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
    sl_selected:LIST-ITEM-PAIRS = cSelectedList.
    sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-poexp
ON CHOOSE OF Btn_Def IN FRAME rd-poexp /* Default */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-poexp
ON CHOOSE OF btn_down IN FRAME rd-poexp /* Move Down */
    DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-poexp
ON CHOOSE OF Btn_Remove IN FRAME rd-poexp /* << Remove */
    DO:
        /* DO i = sl_selected:NUM-ITEMS TO 1 BY -1 WITH FRAME {&FRAME-NAME}:
           IF sl_selected:IS-SELECTED(i) THEN
           ldummy = sl_selected:DELETE(i).
         END
         */
        APPLY "DEFAULT-ACTION" TO sl_selected  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-poexp
ON CHOOSE OF btn_Up IN FRAME rd-poexp /* Move Up */
    DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_buyer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_buyer rd-poexp
ON LEAVE OF end_buyer IN FRAME rd-poexp /* To Buyer ID */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date rd-poexp
ON LEAVE OF end_date IN FRAME rd-poexp /* To Due Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_item rd-poexp
ON LEAVE OF end_item IN FRAME rd-poexp /* To FG Item # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job rd-poexp
ON LEAVE OF end_job IN FRAME rd-poexp /* To Job # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po rd-poexp
ON LEAVE OF end_po IN FRAME rd-poexp /* To PO # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-date rd-poexp
ON LEAVE OF end_po-date IN FRAME rd-poexp /* To PO Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_user-id rd-poexp
ON LEAVE OF end_user-id IN FRAME rd-poexp /* To User ID */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-i-no rd-poexp
ON LEAVE OF end_vend-i-no IN FRAME rd-poexp /* To Vendor Item # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-no rd-poexp
ON LEAVE OF end_vend-no IN FRAME rd-poexp /* To Vendor # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-poexp
ON HELP OF fi_file IN FRAME rd-poexp /* Name */
    DO:
        DEFINE VARIABLE ls-filename AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG       NO-UNDO.

        SYSTEM-DIALOG GET-FILE ls-filename 
            TITLE "Select File to Save "
            FILTERS "Excel Files    (*.csv)" "*.csv",
            "All Files    (*.*) " "*.*"
            INITIAL-DIR "c:\tmp"
            MUST-EXIST
            USE-FILENAME
            UPDATE ll-ok.

        IF ll-ok THEN SELF:SCREEN-VALUE = ls-filename.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-poexp
ON LEAVE OF fi_file IN FRAME rd-poexp /* Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-poexp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-poexp
    DO:
  
        IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) OR
            sl_selected:NUM-ITEMS = 0)
            THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
                ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
                /* sl_selected:SCREEN-VALUE = sl_selected:ENTRY(sl_selected:NUM-ITEMS) */
                .

  
    /* for pairs
        DEF VAR cSelectedList AS CHARACTER NO-UNDO.
        cSelectedList = sl_Selected:LIST-ITEM-PAIRS.
        DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
        IF sl_avail:IS-SELECTED(i) AND
          (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR
             sl_selected:NUM-ITEMS = 0) THEN
        /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
            cSelectedList = cSelectedList +
                            entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
        MESSAGE i sl_avail:IS-SELECTED(i) NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i))
            sl_selected:NUM-ITEMS
            SKIP cSelectedList
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
      cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
      sl_selected:LIST-ITEM-PAIRS = cSelectedList.
      sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
      */

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-poexp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-poexp
    DO:
        DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
            IF {&SELF-NAME}:IS-SELECTED(i) THEN 
            DO:
                ASSIGN 
                    ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
                    ldummy = /*{&SELF-NAME}:DELETE(i)*/
                       {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
                    .
            END.           
        END.
        IF {&SELF-NAME}:NUM-ITEMS NE 0 THEN
            ASSIGN
                {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(1)
                .
    

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-poexp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-poexp /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV rd-poexp
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME rd-poexp /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-poexp 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
  
    {methods/nowait.i}
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        RUN Set-Sort-Data.

        APPLY "entry" TO begin_po.
        fi_file:SCREEN-VALUE = "c:\tmp\POsExport.csv".
    END.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-poexp  _DEFAULT-DISABLE
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
    HIDE FRAME rd-poexp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-poexp 
PROCEDURE DisplaySelectionDefault :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
  
    DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

        cListContents = cListContents +                   
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToDefault)   .
    END.            
    sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-poexp 
PROCEDURE DisplaySelectionList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

    /*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
    /*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
  
    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:
     
        RETURN.
    END.

    EMPTY TEMP-TABLE ttRptList.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

        cListContents = cListContents +
            /* (IF cListContents = "" THEN ""  ELSE ",") +
             ENTRY(iCount,cTextListToSelect) + "," +
             ENTRY(1,cFieldListToSelect)
             paris */
                     
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
            .
    END.
  
    /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
  
    sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-poexp 
PROCEDURE DisplaySelectionList2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

    /*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
    /*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:
     
        RETURN.
    END.

    EMPTY TEMP-TABLE ttRptList.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

        cListContents = cListContents +
            /* (IF cListContents = "" THEN ""  ELSE ",") +
             ENTRY(iCount,cTextListToSelect) + "," +
             ENTRY(1,cFieldListToSelect)
             paris */
                     
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
            .
    END.
  
    /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
  
    sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

    DO iCount = 1 TO sl_selected:NUM-ITEMS:
        ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
    END.

    {sys/ref/SelColCorrect.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-poexp  _DEFAULT-ENABLE
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
    DISPLAY rd_open-closed rd_printed begin_po end_po begin_vend-no end_vend-no 
        begin_item end_item begin_vend-i-no end_vend-i-no begin_job begin_job2 
        end_job end_job2 begin_date end_date sl_avail sl_selected tb_OpenCSV 
        fi_file begin_po-date end_po-date begin_buyer end_buyer begin_user-id 
        end_user-id tbAutoClose 
        WITH FRAME rd-poexp.
    ENABLE rd_open-closed rd_printed begin_po end_po begin_vend-no end_vend-no 
        begin_item end_item begin_vend-i-no end_vend-i-no begin_job begin_job2 
        end_job end_job2 begin_date end_date sl_avail Btn_Def Btn_Add 
        sl_selected Btn_Remove btn_Up btn_down tb_OpenCSV fi_file btn-ok 
        btn-cancel begin_po-date end_po-date begin_buyer end_buyer 
        begin_user-id end_user-id tbAutoClose RECT-6 RECT-7 RECT-8 
        WITH FRAME rd-poexp.
    VIEW FRAME rd-poexp.
    {&OPEN-BROWSERS-IN-QUERY-rd-poexp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-poexp 
PROCEDURE GetSelectionList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTmpList AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttRptSelected.
    cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME} :
        FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.  
        CREATE ttRptSelected.
        ASSIGN 
            ttRptSelected.TextList  = ENTRY(i,cTmpList)
            ttRptSelected.FieldList = ttRptList.FieldList
            /* ttRptSelected.FieldLength */
            .   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-poexp 
PROCEDURE Move-Field :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER move AS CHARACTER NO-UNDO.

    DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME}
        WITH FRAME {&FRAME-NAME}:
        IF sl_selected:IS-SELECTED(i) THEN
        DO:
            IF move = "Down" AND i NE sl_selected:NUM-ITEMS THEN
                ASSIGN
                    ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
                    ldummy                   = sl_selected:DELETE(i)
                    sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
                    .
            ELSE
                IF move = "Up" AND i NE 1 THEN
                    ASSIGN
                        ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
                        ldummy                   = sl_selected:DELETE(i + 1)
                        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i - 1)
                        .
            LEAVE.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-poexp 
PROCEDURE run-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-adder              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFGItem              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNotes               AS CHARACTER NO-UNDO.
    DEFINE BUFFER xjob-mat  FOR job-mat.
    DEFINE BUFFER xitem     FOR item.

    DEFINE BUFFER bf-itemfg FOR itemfg.
    DEFINE BUFFER bf-item   FOR ITEM.

    begin_job = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job)). 
    end_job   = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', end_job)).

    v-excelheader = buildHeader().
    SESSION:SET-WAIT-STATE ("general").

    IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
    IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.


    FOR EACH po-ordl WHERE po-ordl.company = cocode
        AND po-ordl.po-no GE begin_po
        AND po-ordl.po-no LE end_po
        AND po-ordl.vend-no GE begin_vend-no
        AND po-ordl.vend-no LE end_vend-no
        AND po-ordl.i-no GE begin_item
        AND po-ordl.i-no LE end_item
        AND po-ordl.vend-i-no GE begin_vend-i-no
        AND po-ordl.vend-i-no LE end_vend-i-no
        AND FILL(" ", iJobLen - length(TRIM(po-ordl.job-no))) + trim(po-ordl.job-no) GE begin_job
        AND FILL(" ", iJobLen - length(TRIM(po-ordl.job-no))) + trim(po-ordl.job-no) LE end_job
        AND po-ordl.job-no2 GE begin_job2
        AND po-ordl.job-no2 LE end_job2
        AND po-ordl.due-date  GE begin_date
        AND po-ordl.due-date  LE end_date ,
        EACH po-ord WHERE po-ord.company = cocode
        AND po-ord.po-no = po-ordl.po-no
        AND po-ord.po-date GE begin_po-date
        AND po-ord.po-date LE end_po-date
        AND po-ord.buyer GE begin_buyer
        AND po-ord.buyer LE end_buyer
        AND po-ord.USER-ID GE begin_user-id
        AND po-ord.USER-ID LE end_user-id,
        EACH vend WHERE vend.company EQ cocode
        AND vend.vend-no EQ po-ord.vend-no
        NO-LOCK:

        v-excel-detail-lines = "".
        CASE rd_open-closed:
            WHEN 1 THEN 
                IF NOT po-ordl.opened THEN NEXT.
            WHEN 2 THEN 
                IF po-ordl.opened THEN NEXT.
        END CASE.
        CASE rd_printed:
            WHEN 1 THEN 
                IF NOT po-ord.printed THEN NEXT.
            WHEN 2 THEN 
                IF po-ord.printed THEN NEXT.
        END CASE.
    
        FIND FIRST bf-itemfg NO-LOCK
            WHERE bf-itemfg.company = po-ordl.company
            AND bf-itemfg.i-no = po-ordl.i-no  NO-ERROR.
          
        FIND FIRST bf-ITEM NO-LOCK
            WHERE bf-ITEM.company EQ po-ordl.company
            AND bf-ITEM.i-no EQ po-ordl.i-no NO-ERROR.     
    
        FOR EACH ttRptSelected:

            IF LOOKUP(ttRptSelected.FieldList,"po-ordl.job-no,cust-part,adders,rm-item,fg-item,style-job,shipto-cust,dropshipment,rm-cat,rm-cat-dscr,fg-cat,fg-cat-dscr,po-date-notes,required-date-note,lastShip-date-notes,due-date-notes") EQ 0 THEN 
            DO:
                v-excel-detail-lines = v-excel-detail-lines + 
                    appendXLLine(getValue(BUFFER po-ordl,BUFFER po-ord,BUFFER vend,ttRptSelected.FieldList)).
            END.
            ELSE 
            DO:
                CASE ttRptSelected.FieldList:
                    WHEN "shipto-cust" THEN 
                        DO:
                            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(po-ord.cust-no)).
                        END.
                    WHEN "po-ordl.job-no" THEN 
                        DO:
                            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ordl.job-no + "-" + STRING(po-ordl.job-no2,"99")).
                        END.
             
                    WHEN "dropshipment" THEN 
                        DO:
                            IF po-ord.TYPE EQ "D" THEN 
                            DO:
                                IF po-ord.cust-no NE "" THEN
                                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine("Customer").
                                ELSE v-excel-detail-lines = v-excel-detail-lines + appendXLLine("Vendor").
                            END.
                            ELSE  v-excel-detail-lines = v-excel-detail-lines + appendXLLine("").
                        END.
                    WHEN "cust-part" THEN 
                        DO:
                            FIND FIRST itemfg
                                WHERE itemfg.company = po-ordl.company
                                AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
                
                            FIND FIRST  oe-ordl 
                                WHERE oe-ordl.company = po-ordl.company
                                AND oe-ordl.i-no = po-ordl.i-no 
                                AND oe-ordl.ord-no = po-ordl.ord-no NO-LOCK NO-ERROR.

                            IF AVAILABLE oe-ordl AND oe-ordl.part-no NE "" THEN 
                            DO:
                                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(oe-ordl.part-no)).                          
                            END.
                            ELSE IF AVAILABLE itemfg AND itemfg.part-no NE "" THEN 
                                DO:
                                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(itemfg.part-no)).                          
                                END.
                                ELSE v-excel-detail-lines = v-excel-detail-lines + "," .
                        END.
                    WHEN "rm-item"  THEN 
                        DO:                  
                            IF AVAILABLE bf-ITEM AND bf-ITEM.i-no NE "" THEN
                                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(bf-ITEM.i-no)).                          
                            ELSE v-excel-detail-lines = v-excel-detail-lines + "," .
                        END.
                    WHEN "fg-item" THEN 
                        DO: 
                            IF AVAILABLE bf-itemfg AND bf-itemfg.i-no NE "" THEN
                                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(bf-itemfg.i-no)).                          
                            ELSE v-excel-detail-lines = v-excel-detail-lines + "," .
                        END.
                    WHEN "rm-cat"  THEN 
                        DO:                  
                            IF AVAILABLE bf-ITEM AND bf-ITEM.procat NE "" THEN
                                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(bf-ITEM.procat)).                          
                            ELSE v-excel-detail-lines = v-excel-detail-lines + "," .
                        END.
                    WHEN "rm-cat-dscr"  THEN 
                        DO:                  
                            IF AVAILABLE bf-ITEM AND bf-ITEM.procat NE "" THEN 
                            DO:
                                FIND FIRST procat NO-LOCK
                                    WHERE procat.company EQ cocode
                                    AND procat.procat EQ bf-ITEM.procat NO-ERROR.
                                IF AVAILABLE procat THEN     
                                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(procat.dscr)).  
                                ELSE v-excel-detail-lines = v-excel-detail-lines + "," .
                            END.    
                            ELSE v-excel-detail-lines = v-excel-detail-lines + "," .
                        END.
                    WHEN "fg-cat" THEN 
                        DO: 
                            IF AVAILABLE bf-itemfg AND bf-itemfg.procat NE "" THEN
                                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(bf-itemfg.procat)).                          
                            ELSE v-excel-detail-lines = v-excel-detail-lines + "," .
                        END.
                    WHEN "fg-cat-dscr" THEN 
                        DO: 
                            IF AVAILABLE bf-itemfg AND bf-itemfg.procat NE "" THEN 
                            DO:
                                FIND FIRST fgcat NO-LOCK
                                    WHERE fgcat.company EQ cocode
                                    AND trim(fgcat.procat) EQ trim(bf-itemfg.procat) NO-ERROR.  
                                IF AVAILABLE fgcat THEN     
                                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(fgcat.dscr)).  
                                ELSE v-excel-detail-lines = v-excel-detail-lines + "," .
                            END.                                             
                            ELSE v-excel-detail-lines = v-excel-detail-lines + "," .
                        END.
                    WHEN "style-job" THEN 
                        DO:
                            FIND FIRST job-hdr NO-LOCK
                                WHERE job-hdr.company = po-ordl.company
                                AND job-hdr.job-no = po-ordl.job-no
                                AND job-hdr.job-no2 = po-ordl.job-no2
                                AND job-hdr.job-no NE ""  NO-ERROR.

                            IF AVAILABLE job-hdr THEN 
                                ASSIGN cFGItem = job-hdr.i-no .
                            ELSE cFGItem = "" .
                
                            FIND FIRST itemfg NO-LOCK
                                WHERE itemfg.company EQ job-hdr.company
                                AND itemfg.i-no EQ cFGItem 
                                AND itemfg.i-no NE "" NO-ERROR.

                            IF AVAILABLE itemfg AND itemfg.i-no NE "" THEN
                                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(itemfg.style)).                          
                            ELSE v-excel-detail-lines = v-excel-detail-lines + "," .

                        END.
                    WHEN "po-date-notes" THEN 
                        DO:
                            RUN pGetNotes(INPUT po-ord.rec_key, "P", OUTPUT cNotes).
                            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(cNotes)).  
                        END.
                    WHEN "required-date-note" THEN 
                        DO:
                            RUN pGetNotes(INPUT po-ord.rec_key, "R", OUTPUT cNotes).
                            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(cNotes)).  
                        END. 
                    WHEN "lastShip-date-notes" THEN 
                        DO:
                            RUN pGetNotes(INPUT po-ord.rec_key, "L", OUTPUT cNotes).
                            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(cNotes)).  
                        END.  
                    WHEN "due-date-notes" THEN 
                        DO:
                            RUN pGetNotes(INPUT po-ordl.rec_key, "D", OUTPUT cNotes).
                            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(cNotes)).  
                        END.    
                    WHEN "adders" THEN 
                        DO:

                            v-adder = "" .

                            FIND FIRST job WHERE job.company EQ cocode 
                                AND job.job-no EQ STRING(FILL(" ", iJobLen - 
                                LENGTH(TRIM(po-ordl.job-no)))) +
                                TRIM(po-ordl.job-no) 
                                AND job.job-no2 EQ po-ordl.job-no2 NO-LOCK NO-ERROR.
                            IF AVAILABLE job THEN 
                            DO:
                 
                                FOR EACH job-mat
                                    WHERE job-mat.company  EQ cocode
                                    AND job-mat.job      EQ job.job
                                    AND job-mat.job-no   EQ job.job-no
                                    AND job-mat.job-no2  EQ job.job-no2
                                    AND job-mat.i-no     EQ po-ordl.i-no
                                    AND job-mat.frm      EQ po-ordl.s-num
                                    USE-INDEX job NO-LOCK
                                    BREAK BY job-mat.blank-no DESCENDING:
                 
                 
                                    IF LAST(job-mat.blank-no) OR
                                        job-mat.blank-no EQ po-ordl.b-num 
                                        THEN LEAVE.
                                END. /* FOR EACH JOB-MAT*/
                 
                 
                                IF AVAILABLE job-mat THEN 
                                DO:
                 
                                    /* Adder i-no and i-name to po of exist */
                                    FOR EACH xjob-mat NO-LOCK 
                                        WHERE xjob-mat.company  EQ cocode
                                        AND xjob-mat.job      EQ job-mat.job
                                        AND xjob-mat.job-no   EQ job-mat.job-no
                                        AND xjob-mat.job-no2  EQ job-mat.job-no2
                                        AND xjob-mat.frm      EQ job-mat.frm
                                        AND xjob-mat.blank-no EQ job-mat.blank-no
                                        AND xjob-mat.i-no     NE job-mat.i-no:
                 
                                        FIND FIRST xitem 
                                            WHERE xitem.company  EQ cocode
                                            AND xitem.i-no     EQ xjob-mat.i-no
                                            AND xitem.mat-type EQ "A" NO-LOCK NO-ERROR.
                       
                                        IF AVAILABLE xitem THEN
                                            ASSIGN v-adder = xitem.i-name .

                                    END. /* FOR EACH xjob-mat */
                                END.
                            END.
                            IF v-adder NE "" THEN 
                            DO:
                                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(v-adder)).                          
                            END. /* IF AVAIL xitem */
                            ELSE v-excel-detail-lines = v-excel-detail-lines + ",".
                        END.
             
                /*             WHEN "po-ord.vend-no" THEN                                                                                      */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ordl.vend-no).                                */
                /*             WHEN "po-ordl.due-date" THEN                                                                                    */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(po-ordl.due-date)).                       */
                /*             WHEN "po-ord.ship-id" THEN                                                                                      */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ord.ship-id).                                 */
                /*             WHEN "po-ord.ship-name" THEN                                                                                    */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ord.ship-name).                               */
                /*             WHEN "po-ordl.job-no" THEN                                                                                      */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ordl.job-no + "-" + STRING(po-ordl.job-no2)). */
                /*             WHEN "po-ordl.i-no" THEN                                                                                        */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ordl.i-no).                                   */
                /*             WHEN "po-ordl.i-name" THEN                                                                                      */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ordl.i-name).                                 */
                /*             WHEN "po-ordl.s-wid" THEN                                                                                       */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(po-ordl.s-wid)).                          */
                /*             WHEN "po-ordl.s-len" THEN                                                                                       */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(po-ordl.s-len)).                          */
                /*             WHEN "po-ordl.vend-i-no" THEN                                                                                   */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ordl.vend-i-no).                              */
                /*             WHEN "po-ordl.ord-qty" THEN                                                                                     */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(po-ordl.ord-qty)).                        */
                /*             WHEN "po-ordl.pr-qty-uom" THEN                                                                                  */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ordl.pr-qty-uom).                             */
                /*             WHEN "po-ord.printed" THEN                                                                                      */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(po-ord.printed)).                         */
                /*             WHEN "po-ordl.opened" THEN                                                                                      */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(po-ordl.opened)).                         */
                /*             WHEN "po-ordl.t-rec-qty" THEN                                                                                   */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(po-ordl.t-rec-qty)).                      */
                /*             WHEN "po-ordl.cons-uom" THEN                                                                                    */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ordl.cons-uom).                               */
                /*             WHEN "po-ordl.cost" THEN                                                                                        */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(po-ordl.cost)).                           */
                /*             WHEN "po-ordl.pr-uom" THEN                                                                                      */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ordl.pr-uom).                                 */
                /*             WHEN "po-ord.buyer" THEN                                                                                        */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ord.buyer).                                   */
                /*             WHEN "po-ord.stat" THEN                                                                                         */
                /*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ord.stat).                                    */
                END CASE. 
            END.
        END.
        /*     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(po-ordl.po-no)).                          */
        /*     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ordl.vend-no).                                */
        /*     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(po-ordl.due-date)).                       */
        /*     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ord.ship-id).                                 */
        /*     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ord.ship-name).                               */
        /*     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ordl.job-no + "-" + STRING(po-ordl.job-no2)). */
        /*     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ordl.i-no).                                   */
        /*     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(po-ordl.i-name).                                 */
   
        PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-poexp 
PROCEDURE Set-Sort-Data :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:

        /* If a customer number was entered, find first and last matching customers. */
        ASSIGN 
            begin_po:SCREEN-VALUE       = STRING(piPOFrom)
            begin_job:SCREEN-VALUE      = pcJobFrom
            begin_job2:SCREEN-VALUE     = STRING(piJob2From)
            end_job2:SCREEN-VALUE       = STRING(999)
            end_date:SCREEN-VALUE       = STRING(12/31/2099)
            rd_open-closed:SCREEN-VALUE = STRING(piOpenClosed).
        
        IF pdDateFrom NE ? THEN 
            begin_date:SCREEN-VALUE = STRING(pdDateFrom).
        ELSE
            begin_date:SCREEN-VALUE = STRING(01/01/1901).
        IF pcJobFrom NE "" THEN 
            end_job:SCREEN-VALUE   = pcJobTo.
        ELSE
            end_job:SCREEN-VALUE   = "ZZZZZZZZZZZZ".
        IF piPOFrom NE 0 THEN 
            end_po:SCREEN-VALUE   = STRING(piPOTo).
        ELSE
            end_po:SCREEN-VALUE   = STRING(99999999).
        ASSIGN 
            begin_vend-no:SCREEN-VALUE   = assignParam(pcVendFrom,NO)
            end_vend-no:SCREEN-VALUE     = assignParam(pcVendTo,YES)
            begin_item:SCREEN-VALUE      = assignParam(pcItemFrom,NO)
            end_item:SCREEN-VALUE        = assignParam(pcItemTo,YES)
            begin_vend-i-no:SCREEN-VALUE = assignParam(pcVendItemFrom,NO)
            end_vend-i-no:SCREEN-VALUE   = assignParam(pcVendItemTo,YES).
    END.
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetNotes rd-poexp 
PROCEDURE pGetNotes :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPoOrdRecKey AS CHARACTER NO-UNDO.     
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNotes AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-notes FOR notes. 
    FOR EACH bf-notes NO-LOCK
        WHERE bf-notes.rec_key EQ ipcPoOrdRecKey 
        and bf-notes.note_type eq ipcType:
                                 
        opcNotes = bf-notes.note_title + " - " + bf-notes.note_text .  
    END.                                                            

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-poexp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Adds a value to a csv line
        Notes:  Protects agains commans and quotes.
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-line AS CHARACTER NO-UNDO.

    ipc-append = REPLACE(ipc-append, '"', '').
    ipc-append = REPLACE(ipc-append, ',', ' ').
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-poexp 
FUNCTION assignParam RETURNS CHARACTER
    ( ipc-param AS CHARACTER , ipl-end AS LOG) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-return AS CHARACTER.

    IF ipl-end THEN
        lc-return = ipc-param + "ZZZZZZZZZZZZZZZ".
    ELSE
        lc-return = ipc-param.

    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-poexp 
FUNCTION buildHeader RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-header AS CHARACTER NO-UNDO.

    FOR EACH ttRptSelected:
        lc-header = lc-header + appendXLLine(ttRptSelected.TextList).
    END.
    /*     lc-header = lc-header + appendXLLine ("PO #").      */
    /*     lc-header = lc-header + appendXLLine ("Vendor #").  */
    /*     lc-header = lc-header + appendXLLine ("Due Date").  */
    /*     lc-header = lc-header + appendXLLine ("Ship ID").   */
    /*     lc-header = lc-header + appendXLLine ("Ship Name"). */
    /*     lc-header = lc-header + appendXLLine ("Job #").     */
    /*     lc-header = lc-header + appendXLLine ("Item #").    */
    /*     lc-header = lc-header + appendXLLine ("Item Name"). */

  
    RETURN lc-header.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue rd-poexp 
FUNCTION getValue RETURNS CHARACTER
    ( BUFFER ipb-po-ordl FOR po-ordl, 
    BUFFER ipb-po-ord FOR po-ord, 
    BUFFER ipb-vend FOR vend, 
    ipc-field AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Take a buffer and field name as string and return the value
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-return AS CHARACTER FORMAT "x(100)" NO-UNDO.
    DEFINE VARIABLE li-period AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lc-table  AS CHARACTER NO-UNDO.

    li-period = INDEX(ipc-field,".").
    IF li-period > 0 THEN 
    DO:
        lc-table = SUBSTRING(ipc-field, 1, li-period - 1).
        ipc-field = SUBSTRING(ipc-field, li-period + 1,  LENGTH(ipc-field) - li-period).
        CASE lc-table:
            WHEN "po-ordl" THEN
                lc-return = getValue-po-ordl(BUFFER ipb-po-ordl, ipc-field).
            WHEN "po-ord" THEN
                lc-return = getValue-po-ord(BUFFER ipb-po-ord, ipc-field).
            WHEN "vend" THEN
                lc-return = getValue-vend(BUFFER ipb-vend, ipc-field).
            OTHERWISE
            lc-return = "".
        END CASE.
    END.
    ELSE
        lc-return = getValue-po-ordl(BUFFER ipb-po-ordl, ipc-field).
    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-po-ord rd-poexp 
FUNCTION getValue-po-ord RETURNS CHARACTER
    ( BUFFER ipb-buffer FOR po-ord, ipc-field AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Take a buffer and field name as string and return the value
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE h-field   AS HANDLE.
    DEFINE VARIABLE li-extent AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lc-return AS CHARACTER FORMAT "x(100)" NO-UNDO.

    CASE ipc-field :
        WHEN "dfunc"  THEN 
            DO:
            END.
        OTHERWISE 
        DO:
            IF INDEX(ipc-field,"[") > 0 THEN 
            DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-buffer:BUFFER-FIELD(ipc-field).
            IF h-field:EXTENT = 0 THEN
                lc-return = STRING(h-field:BUFFER-VALUE /*, h-field:FORMAT*/ ).
            ELSE
                lc-return = STRING(h-field:BUFFER-VALUE(li-extent) /*, h-field:FORMAT*/ ).
        END.
    END CASE.
    IF lc-return EQ ? THEN lc-return = "".
    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-po-ordl rd-poexp 
FUNCTION getValue-po-ordl RETURNS CHARACTER
    ( BUFFER ipb-buffer FOR po-ordl, ipc-field AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Take a buffer and field name as string and return the value
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE h-field   AS HANDLE.
    DEFINE VARIABLE li-extent AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lc-return AS CHARACTER FORMAT "x(100)" NO-UNDO.
    DEFINE BUFFER lb-oe-ord  FOR oe-ord.
    DEFINE BUFFER lb-job-hdr FOR job-hdr.

    CASE ipc-field :
        WHEN "dfuncFGFromJob"  THEN 
            DO:
                IF ipb-buffer.job-no NE "" THEN 
                DO:
                    FIND FIRST lb-job-hdr WHERE lb-job-hdr.company = ipb-buffer.company
                        AND lb-job-hdr.job-no = ipb-buffer.job-no
                        AND lb-job-hdr.job-no2 = ipb-buffer.job-no2 NO-LOCK NO-ERROR.
                    IF AVAILABLE lb-job-hdr THEN
                        lc-return = lb-job-hdr.i-no.
                END.
            END.
        WHEN "dfuncCustFromOrder"  THEN 
            DO:
                IF ipb-buffer.ord-no > 0 THEN 
                DO:
                    FIND FIRST lb-oe-ord WHERE lb-oe-ord.company = ipb-buffer.company
                        AND lb-oe-ord.ord-no = ipb-buffer.ord-no NO-LOCK NO-ERROR.
                    IF AVAILABLE lb-oe-ord THEN
                        lc-return = lb-oe-ord.cust-no.
                END.
            END. 
        WHEN "item-type"  THEN 
            DO:
                lc-return = IF ipb-buffer.item-type EQ YES THEN "RM" ELSE "FG".            
            END.
        OTHERWISE 
        DO:
            IF INDEX(ipc-field,"[") > 0 THEN 
            DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-buffer:BUFFER-FIELD(ipc-field).
            IF h-field:EXTENT = 0 THEN
                lc-return = STRING(h-field:BUFFER-VALUE /*, h-field:FORMAT*/ ).
            ELSE
                lc-return = STRING(h-field:BUFFER-VALUE(li-extent) /*, h-field:FORMAT*/ ).
        END.
    END CASE.
    IF lc-return EQ ? THEN lc-return = "".
    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-vend rd-poexp 
FUNCTION getValue-vend RETURNS CHARACTER
    ( BUFFER ipb-buffer FOR vend, ipc-field AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Take a buffer and field name as string and return the value
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE h-field   AS HANDLE.
    DEFINE VARIABLE li-extent AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lc-return AS CHARACTER FORMAT "x(100)" NO-UNDO.

    CASE ipc-field :
        WHEN "dfunc"  THEN 
            DO:
            END.
        OTHERWISE 
        DO:
            IF INDEX(ipc-field,"[") > 0 THEN 
            DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-buffer:BUFFER-FIELD(ipc-field).
            IF h-field:EXTENT = 0 THEN
                lc-return = STRING(h-field:BUFFER-VALUE /*, h-field:FORMAT*/ ).
            ELSE
                lc-return = STRING(h-field:BUFFER-VALUE(li-extent) /*, h-field:FORMAT*/ ).
        END.
    END CASE.
    IF lc-return EQ ? THEN lc-return = "".
    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

