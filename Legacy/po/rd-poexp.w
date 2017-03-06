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
DEFINE INPUT PARAMETER piOpenClosed AS INT NO-UNDO.


/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEFINE STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.
ASSIGN cTextListToSelect = "PO #,Vendor #,Due Date,Ship ID,Ship Name," +
                            "Ship Address 1,Ship Address 2,Ship City,Ship State,Ship Zip," +
                            "Shipping Carrier,Total Freight,Freight Payment,FOB," +
                            "Tax Code,Tax,Taxable,Payment Terms,Total Cost," +
                            "Job #,Item #,Item Name,Width,Length," +
                            "Sheet #,Blank #,Description 1,Description 2," +
                            "Vendor Item #,Order Qty,Order UOM," +
                            "Qty Received,Rec. UOM,Item Cost,UOM,Buyer," +
                            "Status,Item Status,Printed,Opened,Type,Contact," +
                            "PO Date,Last Ship Date," +
                            "Vendor Name,Vendor Address 1,Vendor Address 2,Vendor City,Vendor State,Vendor Zip," +
                            "Setup,Discount,GL Number,Overrun,Underrun," +
                            "Customer #,Order #,Customer # From Order,FG Item # From Job,Cust Part#,Adder," +
                            "RM Item Code,FG Item Code,Style from Job"
       cFieldListToSelect = "po-ordl.po-no,po-ord.vend-no,po-ordl.due-date,po-ord.ship-id,po-ord.ship-name," +
                            "po-ord.ship-addr[1],po-ord.ship-addr[2],po-ord.ship-city,po-ord.ship-state,po-ord.ship-zip," +
                            "po-ord.carrier,po-ord.t-freight,po-ord.frt-pay,po-ord.fob-code," +
                            "po-ord.tax-gr,po-ord.tax,po-ordl.tax,po-ord.terms,po-ord.t-cost," +
                            "po-ordl.job-no,po-ordl.i-no,po-ordl.i-name,po-ordl.s-wid,po-ordl.s-len," +
                            "po-ordl.s-num,po-ordl.b-num,po-ordl.dscr[1],po-ordl.dscr[2]," +
                            "po-ordl.vend-i-no,po-ordl.ord-qty,po-ordl.pr-qty-uom," +
                            "po-ordl.t-rec-qty,po-ordl.cons-uom,po-ordl.cost,po-ordl.pr-uom,po-ord.buyer," +
                            "po-ord.stat,po-ordl.stat,po-ord.printed,po-ordl.opened,po-ord.type,po-ord.contact," +
                            "po-ord.po-date,po-ord.last-ship-date," +
                            "vend.name,vend.add1,vend.add2,vend.city,vend.state,vend.zip," +
                            "po-ordl.setup,po-ordl.disc,po-ordl.actnum,po-ordl.over-pct,po-ordl.under-pct," +
                            "po-ordl.cust-no,po-ordl.ord-no,po-ordl.dfuncCustfromOrder,po-ordl.dfuncFGFromJob,cust-part,adders," +
                            "rm-item,fg-item,style-job"
    .

/*vend.name
       lv_vend-add1:SCREEN-VALUE  = vend.add1
       lv_vend-add2:SCREEN-VALUE  = vend.add2
       lv_vend-city:SCREEN-VALUE  = vend.city
       lv_vend-state:SCREEN-VALUE = vend.state
       lv_vend-zip:SCREEN-VALUE   = vend.zip.*/
{sys/inc/ttRptSel.i}

    ASSIGN cTextListToDefault  = "PO #,Vendor #,Due Date,Ship ID,Ship Name," +
                            "Ship Address 1,Ship Address 2,Ship City,Ship State,Ship Zip," +
                            "Shipping Carrier,Total Freight,Freight Payment,FOB," +
                            "Tax Code,Tax,Taxable,Payment Terms,Total Cost," +
                            "Job #,Item #,Item Name,Width,Length," +
                            "Sheet #,Blank #,Description 1,Description 2," +
                            "Vendor Item #,Order Qty,Order UOM," +
                            "Qty Received,Rec. UOM,Item Cost,UOM,Buyer," +
                            "Status,Item Status,Printed,Opened,Type,Contact," +
                            "PO Date,Last Ship Date," +
                            "Vendor Name,Vendor Address 1,Vendor Address 2,Vendor City,Vendor State,Vendor Zip," +
                            "Setup,Discount,GL Number,Overrun,Underrun," +
                            "Customer #,Order #,Customer # From Order,FG Item # From Job,Cust Part#,Adder," +
                            "RM Item Code,FG Item Code".

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
begin_job begin_job2 end_job end_job2 begin_date end_date sl_avail Btn_Def Btn_Add ~
sl_selected Btn_Remove btn_Up btn_down tb_runExcel fi_file btn-ok ~
btn-cancel RECT-6 RECT-7 RECT-8 begin_po-date end_po-date 
&Scoped-Define DISPLAYED-OBJECTS rd_open-closed rd_printed begin_po end_po ~
begin_vend-no end_vend-no begin_item end_item begin_vend-i-no end_vend-i-no ~
begin_job begin_job2 end_job end_job2 begin_date end_date sl_avail ~
sl_selected tb_excel tb_runExcel fi_file begin_po-date end_po-date 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-poexp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-poexp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG)  FORWARD.

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
    ipc-field AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-po-ord rd-poexp 
FUNCTION getValue-po-ord RETURNS CHARACTER
  ( BUFFER ipb-buffer FOR po-ord, ipc-field AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-po-ordl rd-poexp 
FUNCTION getValue-po-ordl RETURNS CHARACTER
  ( BUFFER ipb-buffer FOR po-ordl, ipc-field AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-vend rd-poexp 
FUNCTION getValue-vend RETURNS CHARACTER
  ( BUFFER ipb-buffer FOR vend, ipc-field AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Add 
     LABEL "&Add >>" 
     SIZE 16 BY 1.

DEFINE BUTTON Btn_Def 
     LABEL "&Default" 
     SIZE 16 BY 1.

DEFINE BUTTON btn_down 
     LABEL "Move Down" 
     SIZE 16 BY 1.

DEFINE BUTTON Btn_Remove 
     LABEL "<< &Remove" 
     SIZE 16 BY 1.

DEFINE BUTTON btn_Up 
     LABEL "Move Up" 
     SIZE 16 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/1901 
     LABEL "From Due Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_item AS CHARACTER FORMAT "X(15)" 
     LABEL "From FG Item #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_job AS CHARACTER FORMAT "X(6)" 
     LABEL "From Job #" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE begin_job2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "From PO #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_po-date AS DATE FORMAT "99/99/9999" INITIAL 01/01/1901 
     LABEL "From PO Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_vend-i-no AS CHARACTER FORMAT "X(15)" 
     LABEL "From Vendor Item #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_vend-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From Vendor #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/2099 
     LABEL "To Due Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_item AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To FG Item #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_job AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "To Job #" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE end_job2 AS INTEGER FORMAT ">>":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE end_po AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "To PO #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_po-date AS DATE FORMAT "99/99/9999" INITIAL 12/31/2099 
     LABEL "To PO Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_vend-i-no AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To Vendor Item #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_vend-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Vendor #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-po.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE rd_open-closed AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", 1,
"Closed", 2,
"Both", 3
     SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE rd_printed AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Printed", 1,
"Not Printed", 2,
"Both", 3
     SIZE 45 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 6.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 10.48.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 2.71.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5.10 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5.10 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL yes 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-poexp
     rd_open-closed AT ROW 1.57 COL 34 NO-LABEL WIDGET-ID 2
     rd_printed AT ROW 2.67 COL 34 NO-LABEL WIDGET-ID 126
     begin_po AT ROW 3.62 COL 28 COLON-ALIGNED HELP
          "Enter Beginning PO Number" WIDGET-ID 120
     end_po AT ROW 3.62 COL 71 COLON-ALIGNED HELP
          "Enter Ending PO Number" WIDGET-ID 122
     begin_vend-no AT ROW 4.81 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number" WIDGET-ID 6
     end_vend-no AT ROW 4.81 COL 71 COLON-ALIGNED HELP
          "Enter Ending Vendor Number" WIDGET-ID 16
     begin_item AT ROW 5.95 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Item Number" WIDGET-ID 100
     end_item AT ROW 5.95 COL 71 COLON-ALIGNED HELP
          "Enter Ending Item Number" WIDGET-ID 102
     begin_vend-i-no AT ROW 7.1 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Vendor Item Number" WIDGET-ID 104
     end_vend-i-no AT ROW 7.1 COL 71 COLON-ALIGNED HELP
          "Enter Ending Vendor Item Number" WIDGET-ID 106
     begin_job AT ROW 8.24 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 108
     begin_job2 AT ROW 8.24 COL 41.2 COLON-ALIGNED HELP
          "Enter Beginning Job Number" NO-LABEL WIDGET-ID 116
     end_job AT ROW 8.24 COL 71 COLON-ALIGNED HELP
          "Enter Ending Job Number" WIDGET-ID 110
     end_job2 AT ROW 8.24 COL 84 COLON-ALIGNED HELP
          "Enter Ending Job Number" NO-LABEL WIDGET-ID 118
     begin_date AT ROW 9.38 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Due Date" WIDGET-ID 112
     end_date AT ROW 9.33 COL 71 COLON-ALIGNED HELP
          "Enter Ending Due Date" WIDGET-ID 114
     sl_avail AT ROW 13.05 COL 9 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 13.29 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 14.24 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     sl_selected AT ROW 13.05 COL 64 NO-LABEL WIDGET-ID 28
     Btn_Remove AT ROW 15.19 COL 44 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 16.14 COL 44 WIDGET-ID 136
     btn_down AT ROW 17.1 COL 44 WIDGET-ID 132
     tb_excel AT ROW 18.86 COL 36 WIDGET-ID 32
     tb_runExcel AT ROW 18.86 COL 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 19.81 COL 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     btn-ok AT ROW 21.71 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 21.71 COL 60.2 WIDGET-ID 12
     begin_po-date AT ROW 10.48 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Due Date" WIDGET-ID 142
     end_po-date AT ROW 10.43 COL 71 COLON-ALIGNED HELP
          "Enter Ending Due Date" WIDGET-ID 144
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 12.33 COL 10 WIDGET-ID 140
     "Selected Columns" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 12.33 COL 63.4 WIDGET-ID 138
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     "Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 11.38 COL 3 WIDGET-ID 86
     RECT-6 AT ROW 11.62 COL 2 WIDGET-ID 30
     RECT-7 AT ROW 1.24 COL 2 WIDGET-ID 38
     RECT-8 AT ROW 18.38 COL 2 WIDGET-ID 84
     SPACE(2.39) SKIP(2.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
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
       FRAME rd-poexp:SCROLLABLE       = FALSE
       FRAME rd-poexp:HIDDEN           = TRUE.

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

ASSIGN 
       begin_item:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

ASSIGN 
       begin_job:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

ASSIGN 
       begin_po:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

ASSIGN 
       begin_po-date:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

ASSIGN 
       begin_vend-i-no:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

ASSIGN 
       begin_vend-no:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

ASSIGN 
       end_item:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

ASSIGN 
       end_job:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

ASSIGN 
       end_po:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

ASSIGN 
       end_po-date:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

ASSIGN 
       end_vend-i-no:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

ASSIGN 
       end_vend-no:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-poexp
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME rd-poexp
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME rd-poexp     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-poexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-poexp rd-poexp
ON HELP OF FRAME rd-poexp /* Export POs to Excel */
DO:
    DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
    DEF VAR ls-cur-val AS CHAR NO-UNDO.
    DEF VAR char-val AS CHAR NO-UNDO.

    lw-focus = FOCUS.
    
    case lw-focus:name :

       when "begin_vend-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-vendno.w (cocode, "", ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* vend-no*/
       when "end_vend-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-vendno.w (cocode, "", ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* vend-no*/
       when "begin_item" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-itemfg.w (cocode, "", ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg */
       when "end_item" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-itemfg.w (cocode, "", ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg*/
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


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date rd-poexp
ON LEAVE OF begin_date IN FRAME rd-poexp /* From Due Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_item rd-poexp
ON LEAVE OF begin_item IN FRAME rd-poexp /* From FG Item # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job rd-poexp
ON LEAVE OF begin_job IN FRAME rd-poexp /* From Job # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po rd-poexp
ON LEAVE OF begin_po IN FRAME rd-poexp /* From PO # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME begin_po-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-date rd-poexp
ON LEAVE OF begin_po-date IN FRAME rd-poexp /* From PO Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-i-no rd-poexp
ON LEAVE OF begin_vend-i-no IN FRAME rd-poexp /* From Vendor Item # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no rd-poexp
ON LEAVE OF begin_vend-no IN FRAME rd-poexp /* From Vendor # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-poexp
ON CHOOSE OF btn-cancel IN FRAME rd-poexp /* Cancel */
DO:
   apply "close" to this-procedure.
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
  RUN GetSelectionList.  
  run run-report.

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-poexp
ON CHOOSE OF Btn_Add IN FRAME rd-poexp /* Add >> */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

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
  DEF VAR cSelectedList AS cha NO-UNDO.

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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date rd-poexp
ON LEAVE OF end_date IN FRAME rd-poexp /* To Due Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_item rd-poexp
ON LEAVE OF end_item IN FRAME rd-poexp /* To FG Item # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job rd-poexp
ON LEAVE OF end_job IN FRAME rd-poexp /* To Job # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po rd-poexp
ON LEAVE OF end_po IN FRAME rd-poexp /* To PO # */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-date rd-poexp
ON LEAVE OF end_po-date IN FRAME rd-poexp /* To PO Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-i-no rd-poexp
ON LEAVE OF end_vend-i-no IN FRAME rd-poexp /* To Vendor Item # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-no rd-poexp
ON LEAVE OF end_vend-no IN FRAME rd-poexp /* To Vendor # */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-poexp
ON LEAVE OF fi_file IN FRAME rd-poexp /* If Yes, File Name */
DO:
     assign {&self-name}.
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
    DEF VAR cSelectedList AS cha NO-UNDO.
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
    IF {&SELF-NAME}:IS-SELECTED(i) THEN DO:
       ASSIGN ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel rd-poexp
ON VALUE-CHANGED OF tb_runExcel IN FRAME rd-poexp /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-poexp 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpw.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN DisplaySelectionList.
  RUN enable_UI.
   {methods/nowait.i}
   DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    RUN Set-Sort-Data.

    APPLY "entry" TO begin_po.
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
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.
  
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
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

/*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
  
  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:
     
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
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
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
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

/*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:
     
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
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .
  END.
  
 /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
  
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

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
          end_job end_job2 begin_date end_date sl_avail sl_selected tb_excel 
          tb_runExcel fi_file begin_po-date end_po-date 
      WITH FRAME rd-poexp.
  ENABLE rd_open-closed rd_printed begin_po end_po begin_vend-no end_vend-no 
         begin_item end_item begin_vend-i-no end_vend-i-no begin_job begin_job2 
         end_job end_job2 begin_date end_date sl_avail Btn_Def Btn_Add sl_selected 
         Btn_Remove btn_Up btn_down tb_runExcel fi_file btn-ok btn-cancel 
         RECT-6 RECT-7 RECT-8 begin_po-date end_po-date 
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
 DEF VAR cTmpList AS cha NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

 DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME} :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.  
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
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
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
        ldummy = sl_selected:DELETE(i)
        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
        .
      ELSE
      IF move = "Up" AND i NE 1 THEN
      ASSIGN
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
        ldummy = sl_selected:DELETE(i + 1)
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
DEF VAR v-excelheader AS CHAR NO-UNDO.
DEF VAR v-excel-detail-lines AS CHAR NO-UNDO.
DEF VAR v-adder AS CHAR NO-UNDO.
DEFINE VARIABLE cFGItem AS CHAR NO-UNDO .
DEF BUFFER xjob-mat FOR job-mat.
DEF BUFFER xitem    FOR item.

v-excelheader = buildHeader().
SESSION:SET-WAIT-STATE ("general").

IF tb_excel THEN OUTPUT STREAM excel TO VALUE(fi_file).
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
        AND po-ordl.job-no GE begin_job
        AND po-ordl.job-no LE end_job
        AND po-ordl.job-no2 GE begin_job2
        AND po-ordl.job-no2 LE end_job2
        AND po-ordl.due-date  GE begin_date
        AND po-ordl.due-date  LE end_date ,
    EACH po-ord WHERE po-ord.company = cocode
        AND po-ord.po-no = po-ordl.po-no
        AND po-ord.po-date GE begin_po-date
        AND po-ord.po-date LE end_po-date,
    EACH vend WHERE vend.company EQ cocode
          AND vend.vend-no EQ po-ord.vend-no
    NO-LOCK:

    v-excel-detail-lines = "".
    CASE rd_open-closed:
        WHEN 1 THEN IF NOT po-ordl.opened THEN NEXT.
        WHEN 2 THEN IF po-ordl.opened THEN NEXT.
    END CASE.
    CASE rd_printed:
        WHEN 1 THEN IF NOT po-ord.printed THEN NEXT.
        WHEN 2 THEN IF po-ord.printed THEN NEXT.
    END CASE.
/* "po-ordl.po-no,po-ord.vend-no,po-ordl.due-date,po-ord.ship-id,po-ord.ship-name," + */
/* "po-ordl.job-no,po-ordl.i-no,po-ordl.i-name,po-ordl.s-wid,po-ordl.s-len," +        */
/* "po-ordl.vend-i-no,po-ordl.ord-qty,po-ordl.pr-qty-uom," +                          */
/* "po-ordl.t-rec-qty,po-ordl.cons-uom,po-ordl.cost,po-ordl.pr-uom,po-ord.buyer," +   */
/* "po-ord.stat,po-ord.printed,po-ordl.opened"                                        */
    FOR EACH ttRptSelected:

        IF lookup(ttRptSelected.FieldList,"cust-part,adders,rm-item,fg-item,style-job") EQ 0 THEN do:
        v-excel-detail-lines = v-excel-detail-lines + 
            appendXLLine(getValue(BUFFER po-ordl,BUFFER po-ord,BUFFER vend,ttRptSelected.FieldList)).
        END.
        ELSE do:
         CASE ttRptSelected.FieldList:                                                                                       
             WHEN "cust-part" THEN do:
                 FIND FIRST itemfg
                 WHERE itemfg.company = po-ordl.company
                     AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
                
                 FIND FIRST  oe-ordl 
                   WHERE oe-ordl.company = po-ordl.company
                     AND oe-ordl.i-no = po-ordl.i-no 
                     AND oe-ordl.ord-no = po-ordl.ord-no NO-LOCK NO-ERROR.

                 IF AVAIL oe-ordl AND oe-ordl.part-no NE "" THEN DO:
                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(oe-ordl.part-no)).                          
                 END.
                 ELSE IF AVAIL itemfg AND itemfg.part-no NE "" THEN DO:
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(itemfg.part-no)).                          
                 END.
                 ELSE v-excel-detail-lines = v-excel-detail-lines + "," .
             END.
             WHEN "rm-item"  THEN DO:
                 FIND FIRST ITEM WHERE ITEM.company EQ po-ordl.company
                     AND ITEM.i-no EQ po-ordl.i-no NO-LOCK NO-ERROR.

                 IF AVAIL ITEM AND ITEM.i-no NE "" THEN
                     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(ITEM.i-no)).                          
                 ELSE v-excel-detail-lines = v-excel-detail-lines + "," .
             END.
             WHEN "fg-item" THEN DO:
                 FIND FIRST itemfg
                 WHERE itemfg.company = po-ordl.company
                     AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.

                 IF AVAIL itemfg AND itemfg.i-no NE "" THEN
                     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(itemfg.i-no)).                          
                 ELSE v-excel-detail-lines = v-excel-detail-lines + "," .
             END.
             WHEN "style-job" THEN DO:
                 FIND FIRST job-hdr NO-LOCK
                      WHERE job-hdr.company = po-ordl.company
                        AND job-hdr.job-no = po-ordl.job-no
                        AND job-hdr.job-no2 = po-ordl.job-no2
                        AND job-hdr.job-no NE ""  NO-ERROR.

                 IF AVAIL job-hdr THEN 
                     ASSIGN cFGItem = job-hdr.i-no .
                 ELSE cFGItem = "" .
                
                   FIND FIRST itemfg NO-LOCK
                     WHERE itemfg.company EQ job-hdr.company
                       AND itemfg.i-no EQ cFGItem 
                       AND itemfg.i-no NE "" NO-ERROR.

                 IF AVAIL itemfg AND itemfg.i-no NE "" THEN
                     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(itemfg.style)).                          
                 ELSE v-excel-detail-lines = v-excel-detail-lines + "," .

             END.
             WHEN "adders" THEN DO:

                 v-adder = "" .

                 FIND FIRST job WHERE job.company EQ cocode 
                           and job.job-no EQ STRING(FILL(" ",6 - 
                                                    LENGTH(TRIM(po-ordl.job-no)))) +
                                                    TRIM(po-ordl.job-no) 
                           AND job.job-no2 EQ po-ordl.job-no2 NO-LOCK NO-ERROR.
                 IF AVAIL job THEN DO:
                 
                   FOR EACH job-mat
                     WHERE job-mat.company  EQ cocode
                       AND job-mat.job      EQ job.job
                       AND job-mat.job-no   EQ job.job-no
                       AND job-mat.job-no2  EQ job.job-no2
                       AND job-mat.i-no     EQ po-ordl.i-no
                       AND job-mat.frm      EQ po-ordl.s-num
                      USE-INDEX job NO-LOCK
                      BREAK BY job-mat.blank-no DESC:
                 
                 
                      IF LAST(job-mat.blank-no) OR
                         job-mat.blank-no EQ po-ordl.b-num 
                        THEN LEAVE.
                   END. /* FOR EACH JOB-MAT*/
                 
                 
                   IF AVAIL job-mat THEN DO:
                 
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
                       
                       IF AVAIL xitem THEN
                           ASSIGN v-adder = xitem.i-name .

                     END. /* FOR EACH xjob-mat */
                   END.
                 END.
                 IF v-adder NE "" THEN DO:
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(v-adder)).                          
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

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
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
        begin_po:SCREEN-VALUE = string(piPOFrom)
        begin_job:SCREEN-VALUE = pcJobFrom
        begin_job2:SCREEN-VALUE = string(piJob2From)
        end_job2:SCREEN-VALUE = string(99)
        end_date:SCREEN-VALUE = string(12/31/2099)
        rd_open-closed:SCREEN-VALUE = STRING(piOpenClosed).
        
        IF pdDateFrom NE ? THEN 
            begin_date:SCREEN-VALUE = string(pdDateFrom).
        ELSE
            begin_date:SCREEN-VALUE = string(01/01/1901).
        IF pcJobFrom NE "" THEN 
            end_job:SCREEN-VALUE   = pcJobTo.
        ELSE
            end_job:SCREEN-VALUE   = "ZZZZZZZZZZZZ".
        IF piPOFrom NE 0 THEN 
            end_po:SCREEN-VALUE   = string(piPOTo).
        ELSE
            end_po:SCREEN-VALUE   = string(99999999).
    ASSIGN 
        begin_vend-no:SCREEN-VALUE = assignParam(pcVendFrom,NO)
        end_vend-no:SCREEN-VALUE   = assignParam(pcVendTo,YES)
        begin_item:SCREEN-VALUE = assignParam(pcItemFrom,NO)
        end_item:SCREEN-VALUE   = assignParam(pcItemTo,YES)
        begin_vend-i-no:SCREEN-VALUE = assignParam(pcVendItemFrom,NO)
        end_vend-i-no:SCREEN-VALUE   = assignParam(pcVendItemTo,YES).
END.
RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-poexp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Adds a value to a csv line
    Notes:  Protects agains commans and quotes.
------------------------------------------------------------------------------*/
    DEF VAR lc-line AS CHAR NO-UNDO.

    ipc-append = REPLACE(ipc-append, '"', '').
    ipc-append = REPLACE(ipc-append, ',', ' ').
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-poexp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR lc-return AS CHAR.

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
DEF VAR lc-header AS CHAR NO-UNDO.

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
    ipc-field AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.
    DEF VAR li-period AS INT NO-UNDO.
    DEF VAR lc-table AS CHAR NO-UNDO.

    li-period = INDEX(ipc-field,".").
    IF li-period > 0 THEN DO:
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
  ( BUFFER ipb-buffer FOR po-ord, ipc-field AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR h-field AS HANDLE.
    DEF VAR li-extent AS INT NO-UNDO.
    DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.

    CASE ipc-field :
        WHEN "dfunc"  THEN DO:
        END.
        OTHERWISE DO:
            IF INDEX(ipc-field,"[") > 0 THEN DO:
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
  ( BUFFER ipb-buffer FOR po-ordl, ipc-field AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR h-field AS HANDLE.
    DEF VAR li-extent AS INT NO-UNDO.
    DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.
    DEF BUFFER lb-oe-ord FOR oe-ord.
    DEF BUFFER lb-job-hdr FOR job-hdr.

    CASE ipc-field :
        WHEN "dfuncFGFromJob"  THEN DO:
            IF ipb-buffer.job-no NE "" THEN DO:
                FIND FIRST lb-job-hdr WHERE lb-job-hdr.company = ipb-buffer.company
                    AND lb-job-hdr.job-no = ipb-buffer.job-no
                    AND lb-job-hdr.job-no2 = ipb-buffer.job-no2 NO-LOCK NO-ERROR.
                IF AVAIL lb-job-hdr THEN
                    lc-return = lb-job-hdr.i-no.
            END.
        END.
        WHEN "dfuncCustFromOrder"  THEN DO:
            IF ipb-buffer.ord-no > 0 THEN DO:
                FIND FIRST lb-oe-ord WHERE lb-oe-ord.company = ipb-buffer.company
                    AND lb-oe-ord.ord-no = ipb-buffer.ord-no NO-LOCK NO-ERROR.
                IF AVAIL lb-oe-ord THEN
                    lc-return = lb-oe-ord.cust-no.
            END.
        END.
        OTHERWISE DO:
            IF INDEX(ipc-field,"[") > 0 THEN DO:
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
  ( BUFFER ipb-buffer FOR vend, ipc-field AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR h-field AS HANDLE.
    DEF VAR li-extent AS INT NO-UNDO.
    DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.

    CASE ipc-field :
        WHEN "dfunc"  THEN DO:
        END.
        OTHERWISE DO:
            IF INDEX(ipc-field,"[") > 0 THEN DO:
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

