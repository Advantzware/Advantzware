&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: companyCopy.w

  Description: copy master files from one company to another

  Input Parameters: copmanyFrom and companyTo

  Output Parameters: <none>

  Author: Ron Stark

  Created: 3.17.2005

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipCompanyFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipCompanyTo AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipCompanyFrom AS CHARACTER NO-UNDO INITIAL '001'.
DEFINE VARIABLE ipCompanyTo AS CHARACTER NO-UNDO INITIAL '003'.
&ENDIF

/* Local Variable Definitions ---                                       */

{system/fSuperRunning.i}

SESSION:SET-WAIT-STATE ("").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tg_account tg_ap-buy tg_ap-ctrl tg_ap-ledger ~
tg_ar-ctrl tg_bank tg_buyer tg_carr-mtx tg_carrier tg_ce-ctrl tg_costtype ~
tg_crew tg_currency tg_cust tg_cust-markup tg_db-ctrl tg_e-item ~
tg_cust-part tg_cust-prod-sales tg_custype tg_e-itemfg-vend tg_emp ~
tg_e-item-cust tg_e-item-vend tg_e-itemfg tg_fg-act tg_fg-bin tg_fg-ctrl ~
tg_fg-set tg_fgcat tg_flute tg_gl-ctrl tg_gl-rpt tg_gl-rptd tg_item ~
tg_item-bom tg_item-spec tg_itemfg tg_itemfg-bom tg_itemfg-ink tg_loadtag ~
tg_loc tg_itemfg-loc tg_itemfgdtl tg_jc-ctrl tg_mach tg_mach-adder ~
tg_mach-calendar tg_mat-act tg_matprep tg_mmtx tg_mmty tg_mstd tg_oe-ctrl ~
tg_oe-prmtx tg_period tg_po-ctrl tg_prep tg_procat tg_prod tg_prodl  ~
tg_rm-bin tg_rm-ctrl tg_routing tg_routing-mtx tg_shift tg_shipto tg_sman ~
tg_sman-mtx tg_soldto tg_stack-flute tg_stack-size tg_stax tg_stax-group ~
tg_std-code tg_style tg_sys-ctrl tg_terms tg_terr tg_test-red tg_usercomp ~
tg_vend tg_ventype tg_mach-part tg_reftable tg_box-design  ~
btnOK btnCancel 
&Scoped-Define DISPLAYED-OBJECTS companyFrom copyTable companyTo ~
tg_copy-master tg_account tg_ap-buy tg_ap-ctrl tg_ap-ledger tg_ar-ctrl ~
tg_bank tg_buyer tg_carr-mtx tg_carrier tg_ce-ctrl tg_costtype tg_crew ~
tg_currency tg_cust tg_cust-markup tg_db-ctrl tg_e-item tg_cust-part ~
tg_cust-prod-sales tg_custype tg_e-itemfg-vend tg_emp tg_e-item-cust ~
tg_e-item-vend tg_e-itemfg tg_fg-act tg_fg-bin tg_fg-ctrl tg_fg-set ~
tg_fgcat tg_flute tg_gl-ctrl tg_gl-rpt tg_gl-rptd tg_item tg_item-bom ~
tg_item-spec tg_itemfg tg_itemfg-bom tg_itemfg-ink tg_loadtag tg_loc ~
tg_itemfg-loc tg_itemfgdtl tg_jc-ctrl tg_mach tg_mach-adder ~
tg_mach-calendar tg_mat-act tg_matprep tg_mmtx tg_mmty tg_mstd tg_oe-ctrl ~
tg_oe-prmtx tg_period tg_po-ctrl tg_prep tg_procat tg_prod tg_prodl  ~
tg_rm-bin tg_rm-ctrl tg_routing tg_routing-mtx tg_shift tg_shipto tg_sman ~
tg_sman-mtx tg_soldto tg_stack-flute tg_stack-size tg_stax tg_stax-group ~
tg_std-code tg_style tg_sys-ctrl tg_terms tg_terr tg_test-red tg_usercomp ~
tg_vend tg_ventype tg_mach-part tg_reftable tg_box-design  

/* Custom List Definitions                                              */
/* buttons,List-2,List-3,List-4,List-5,List-6                           */
&Scoped-define buttons btnOK btnCancel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextFGSet C-Win 
FUNCTION getNextFGSet RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextMMtxNo C-Win 
FUNCTION getNextMMtxNo RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "&Cancel" 
     SIZE 14 BY 1.14.

DEFINE BUTTON btnOK 
     LABEL "&OK" 
     SIZE 14 BY 1.14.

DEFINE VARIABLE copyTable AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 42 BY 21.19
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE companyFrom AS CHARACTER FORMAT "X(256)":U 
     LABEL "Copy From Company" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE companyTo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Copy To Company" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tg_account AS LOGICAL INITIAL yes 
     LABEL "Account?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_ap-buy AS LOGICAL INITIAL yes 
     LABEL "AP Buyer?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_ap-ctrl AS LOGICAL INITIAL yes 
     LABEL "AP Control?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_ap-ledger AS LOGICAL INITIAL yes 
     LABEL "AP Ledger?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_ar-ctrl AS LOGICAL INITIAL yes 
     LABEL "AR Control?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_bank AS LOGICAL INITIAL yes 
     LABEL "Bank?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_buyer AS LOGICAL INITIAL yes 
     LABEL "Buyer?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_carr-mtx AS LOGICAL INITIAL yes 
     LABEL "Carr Pr matrix?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_carrier AS LOGICAL INITIAL yes 
     LABEL "Carrier?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_ce-ctrl AS LOGICAL INITIAL yes 
     LABEL "Cost est Control?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.8 BY .81 NO-UNDO.

DEFINE VARIABLE tg_copy-master AS LOGICAL INITIAL yes 
     LABEL "Copy Company Master Files?" 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY .81 NO-UNDO.

DEFINE VARIABLE tg_copy-transaction AS LOGICAL INITIAL no 
     LABEL "Copy History Files and All Transaction Files?" 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY .81 NO-UNDO.

DEFINE VARIABLE tg_costtype AS LOGICAL INITIAL yes 
     LABEL "Mat Cost Type?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_crew AS LOGICAL INITIAL yes 
     LABEL "Crew?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_currency AS LOGICAL INITIAL yes 
     LABEL "Currency?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_cust AS LOGICAL INITIAL yes 
     LABEL "Customer?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_cust-markup AS LOGICAL INITIAL yes 
     LABEL "Cust Markup?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_cust-part AS LOGICAL INITIAL yes 
     LABEL "Cust Part?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_cust-prod-sales AS LOGICAL INITIAL yes 
     LABEL "Cust Product?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_custype AS LOGICAL INITIAL yes 
     LABEL "Cust Type?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_db-ctrl AS LOGICAL INITIAL yes 
     LABEL "DB Control" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_e-item AS LOGICAL INITIAL yes 
     LABEL "Est Item File?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_e-item-cust AS LOGICAL INITIAL yes 
     LABEL "Est Item Cust?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "e-item-cust ,Estimating info for item file" NO-UNDO.

DEFINE VARIABLE tg_e-item-vend AS LOGICAL INITIAL yes 
     LABEL "e-item-vend ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Estimated item cost by qty level matrix by vendor" NO-UNDO.

DEFINE VARIABLE tg_e-itemfg AS LOGICAL INITIAL yes 
     LABEL "e-itemfg ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Estimating Finished Good items" NO-UNDO.

DEFINE VARIABLE tg_e-itemfg-vend AS LOGICAL INITIAL yes 
     LABEL "e-itemfg-vend?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Estimated itemfg cost by qty level matrix by vendor" NO-UNDO.

DEFINE VARIABLE tg_emp AS LOGICAL INITIAL yes 
     LABEL "Emp?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_fg-act AS LOGICAL INITIAL yes 
     LABEL "fg-act?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "This is the Job Costing Finished Goods WIP File" NO-UNDO.

DEFINE VARIABLE tg_fg-bin AS LOGICAL INITIAL yes 
     LABEL "FG Bin File?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Finished Goods - Bin File" NO-UNDO.

DEFINE VARIABLE tg_fg-ctrl AS LOGICAL INITIAL yes 
     LABEL "FG Control?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Finished Goods - Control File" NO-UNDO.

DEFINE VARIABLE tg_fg-set AS LOGICAL INITIAL yes 
     LABEL "fg-set?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Bill of materials file for sets" NO-UNDO.

DEFINE VARIABLE tg_fgcat AS LOGICAL INITIAL yes 
     LABEL "fgcat?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Bill of materials file for sets" NO-UNDO.

DEFINE VARIABLE tg_flute AS LOGICAL INITIAL yes 
     LABEL "Flute?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_gl-ctrl AS LOGICAL INITIAL yes 
     LABEL "G/L Control?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "G/L Control File" NO-UNDO.

DEFINE VARIABLE tg_gl-rpt AS LOGICAL INITIAL yes 
     LABEL "gl-rpt?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "General Ledger Report Writer File" NO-UNDO.

DEFINE VARIABLE tg_gl-rptd AS LOGICAL INITIAL yes 
     LABEL "gl-rptd?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "General Ledger Report Writer File" NO-UNDO.

DEFINE VARIABLE tg_item AS LOGICAL INITIAL yes 
     LABEL "Raw Materials?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Raw Materials Inventory File" NO-UNDO.

DEFINE VARIABLE tg_item-bom AS LOGICAL INITIAL yes 
     LABEL "item-bom?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Item Bill Of Material File" NO-UNDO.

DEFINE VARIABLE tg_item-spec AS LOGICAL INITIAL yes 
     LABEL "item-spec?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "RM/FG Item Special Properties Master File" NO-UNDO.

DEFINE VARIABLE tg_itemfg AS LOGICAL INITIAL yes 
     LABEL "Finished Goods?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 TOOLTIP "Finished Goods item file" NO-UNDO.

DEFINE VARIABLE tg_itemfg-bom AS LOGICAL INITIAL yes 
     LABEL "itemfg-bom?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Finished Goods item file" NO-UNDO.

DEFINE VARIABLE tg_itemfg-ink AS LOGICAL INITIAL yes 
     LABEL "itemfg-ink?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_itemfg-loc AS LOGICAL INITIAL yes 
     LABEL "itemfg-loc?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_itemfgdtl AS LOGICAL INITIAL yes 
     LABEL "itemfgdtl?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Finished Goods Item Job File" NO-UNDO.

DEFINE VARIABLE tg_jc-ctrl AS LOGICAL INITIAL yes 
     LABEL "Job Cost Control?" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE tg_loadtag AS LOGICAL INITIAL yes 
     LABEL "Loadtag?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_loc AS LOGICAL INITIAL yes 
     LABEL "Plants/Warehouses?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.6 BY .81 NO-UNDO.

DEFINE VARIABLE tg_mach AS LOGICAL INITIAL yes 
     LABEL "Machine?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tg_mach-adder AS LOGICAL INITIAL yes 
     LABEL "mach-adder?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Machine Adder MR Hours/Run Speed" NO-UNDO.

DEFINE VARIABLE tg_mach-calendar AS LOGICAL INITIAL yes 
     LABEL "Mach Calendar?" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE tg_mat-act AS LOGICAL INITIAL yes 
     LABEL "mat-act?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Materials actually used in Jobs)" NO-UNDO.

DEFINE VARIABLE tg_matprep AS LOGICAL INITIAL yes 
     LABEL "Mat Prep Type?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Preperation Material Types" NO-UNDO.

DEFINE VARIABLE tg_mmtx AS LOGICAL INITIAL yes 
     LABEL "Mach St Matrix?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 TOOLTIP "Machine Standards Matrix" NO-UNDO.

DEFINE VARIABLE tg_mmty AS LOGICAL INITIAL yes 
     LABEL "MMTY?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "MMTX matrix for Make Ready, with decimals" NO-UNDO.

DEFINE VARIABLE tg_mstd AS LOGICAL INITIAL yes 
     LABEL "MSTD?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Machine Standards File" NO-UNDO.

DEFINE VARIABLE tg_oe-ctrl AS LOGICAL INITIAL yes 
     LABEL "oe-ctrl?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Order entry control file" NO-UNDO.
DEFINE VARIABLE tg_oe-prmtx AS LOGICAL INITIAL yes 
     LABEL "oe-prmtx?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Order Entry Pricing Matrix" NO-UNDO.
DEFINE VARIABLE tg_period AS LOGICAL INITIAL yes 
     LABEL "period?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Company Accounting Periods" NO-UNDO.
DEFINE VARIABLE tg_po-ctrl AS LOGICAL INITIAL yes 
     LABEL "po-ctrl?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Purchase Order control file" NO-UNDO.

DEFINE VARIABLE tg_prep AS LOGICAL INITIAL yes 
     LABEL "prep?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Preperation Codes" NO-UNDO.
DEFINE VARIABLE tg_procat AS LOGICAL INITIAL yes 
     LABEL "procat?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Product category reference file" NO-UNDO.
DEFINE VARIABLE tg_prod AS LOGICAL INITIAL yes 
     LABEL "prod?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "finished goods Product line" NO-UNDO.
DEFINE VARIABLE tg_prodl AS LOGICAL INITIAL yes 
     LABEL "prodl?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Product Line detail" NO-UNDO.
DEFINE VARIABLE tg_rm-bin AS LOGICAL INITIAL yes 
     LABEL "rm-bin?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Raw Material Inventory Bin File" NO-UNDO.

DEFINE VARIABLE tg_rm-ctrl AS LOGICAL INITIAL yes 
     LABEL "RM ctrl?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Raw Material Inventory Control File" NO-UNDO.
DEFINE VARIABLE tg_routing AS LOGICAL INITIAL yes 
     LABEL "Routing?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Default Machine Routings" NO-UNDO.
DEFINE VARIABLE tg_routing-mtx AS LOGICAL INITIAL yes 
     LABEL "routing-mtx?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Default Routing Matrix - By Style, MSF, Blank L & W" NO-UNDO.
DEFINE VARIABLE tg_shift AS LOGICAL INITIAL yes 
     LABEL "Shift?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Work Shift Master File" NO-UNDO.
DEFINE VARIABLE tg_shipto AS LOGICAL INITIAL yes 
     LABEL "Ship To?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Shipto" NO-UNDO.

DEFINE VARIABLE tg_sman AS LOGICAL INITIAL yes 
     LABEL "Sales Rep?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Sales Rep" NO-UNDO.
DEFINE VARIABLE tg_sman-mtx AS LOGICAL INITIAL yes 
     LABEL "sman-mtx?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Salesman Product Matrix" NO-UNDO.
DEFINE VARIABLE tg_soldto AS LOGICAL INITIAL yes 
     LABEL "Sold To?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Sold to" NO-UNDO.
DEFINE VARIABLE tg_stack-flute AS LOGICAL INITIAL yes 
     LABEL "stack-flute?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Stacking Matrix by Flute type" NO-UNDO.
DEFINE VARIABLE tg_stack-size AS LOGICAL INITIAL yes 
     LABEL "stack-size?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Stacking Matrix by Box Size" NO-UNDO.

DEFINE VARIABLE tg_stax AS LOGICAL INITIAL yes 
     LABEL "Tax Codes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Tax Codes" NO-UNDO.
DEFINE VARIABLE tg_stax-group AS LOGICAL INITIAL yes 
     LABEL "stax-group?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Tax Authority Master" NO-UNDO.
DEFINE VARIABLE tg_std-code AS LOGICAL INITIAL yes 
     LABEL "std-code?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Codes for Machine Standard file matrix" NO-UNDO.
DEFINE VARIABLE tg_style AS LOGICAL INITIAL yes 
     LABEL "Style?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Style" NO-UNDO.
DEFINE VARIABLE tg_sys-ctrl AS LOGICAL INITIAL yes 
     LABEL "sys-ctrl?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "System Control Flag File" NO-UNDO.

DEFINE VARIABLE tg_terms AS LOGICAL INITIAL yes 
     LABEL "Terms?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Terms" NO-UNDO.
DEFINE VARIABLE tg_terr AS LOGICAL INITIAL yes 
     LABEL "Territory?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Territory" NO-UNDO.
DEFINE VARIABLE tg_test-red AS LOGICAL INITIAL yes 
     LABEL "test-red?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Test Run Speed Reduction" NO-UNDO.
DEFINE VARIABLE tg_usercomp AS LOGICAL INITIAL yes 
     LABEL "User Comp/Loc?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "User Company/Location" NO-UNDO.
DEFINE VARIABLE tg_vend AS LOGICAL INITIAL yes 
     LABEL "Vendor file?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Vendor file" NO-UNDO.

DEFINE VARIABLE tg_ventype AS LOGICAL INITIAL yes 
     LABEL "Vendor Type?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Vendor Type File" NO-UNDO.
DEFINE VARIABLE tg_mach-part AS LOGICAL INITIAL NO 
     LABEL "mach-part?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Machine" NO-UNDO.
DEFINE VARIABLE tg_reftable AS LOGICAL INITIAL NO 
     LABEL "Reftable?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Reference tables" NO-UNDO.
DEFINE VARIABLE tg_box-design AS LOGICAL INITIAL NO 
     LABEL "Box Design?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Box Design" NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     companyFrom AT ROW 1.48 COL 28 COLON-ALIGNED
     copyTable AT ROW 1.48 COL 119.8 NO-LABEL
     companyTo AT ROW 1.67 COL 63.6 COLON-ALIGNED
     tg_copy-master AT ROW 2.91 COL 29
     tg_account AT ROW 4.33 COL 4.8 WIDGET-ID 2
     tg_ap-buy AT ROW 4.33 COL 24.8 WIDGET-ID 38
     tg_ap-ctrl AT ROW 4.33 COL 45 WIDGET-ID 72
     tg_ap-ledger AT ROW 4.33 COL 66 WIDGET-ID 106
     tg_ar-ctrl AT ROW 4.33 COL 88.2 WIDGET-ID 104
     tg_bank AT ROW 5.24 COL 4.8 WIDGET-ID 4
     tg_buyer AT ROW 5.24 COL 24.8 WIDGET-ID 56
     tg_carr-mtx AT ROW 5.24 COL 45 WIDGET-ID 74
     tg_carrier AT ROW 5.29 COL 66 WIDGET-ID 28
     tg_ce-ctrl AT ROW 5.29 COL 88.2 WIDGET-ID 54
     tg_costtype AT ROW 6.14 COL 4.8 WIDGET-ID 6
     tg_crew AT ROW 6.14 COL 24.8 WIDGET-ID 58
     tg_currency AT ROW 6.14 COL 45 WIDGET-ID 76
     tg_cust AT ROW 6.14 COL 66 WIDGET-ID 102
     tg_cust-markup AT ROW 6.14 COL 88 WIDGET-ID 100
     tg_db-ctrl AT ROW 6.95 COL 66 WIDGET-ID 36
     tg_e-item AT ROW 6.95 COL 88 WIDGET-ID 50
     tg_cust-part AT ROW 7 COL 4.8 WIDGET-ID 8
     tg_cust-prod-sales AT ROW 7 COL 24.8 WIDGET-ID 60
     tg_custype AT ROW 7 COL 45 WIDGET-ID 78
     tg_e-itemfg-vend AT ROW 7.81 COL 66 WIDGET-ID 26
     tg_emp AT ROW 7.81 COL 88 WIDGET-ID 52
     tg_e-item-cust AT ROW 7.91 COL 4.8 WIDGET-ID 10
     tg_e-item-vend AT ROW 7.91 COL 24.8 WIDGET-ID 62
     tg_e-itemfg AT ROW 7.91 COL 45 WIDGET-ID 80
     tg_fg-act AT ROW 8.81 COL 4.8 WIDGET-ID 12
     tg_fg-bin AT ROW 8.81 COL 24.8 WIDGET-ID 64
     tg_fg-ctrl AT ROW 8.81 COL 45 WIDGET-ID 82
     tg_fg-set AT ROW 8.81 COL 66 WIDGET-ID 96
     tg_fgcat AT ROW 8.81 COL 88 WIDGET-ID 98
     tg_flute AT ROW 9.71 COL 4.8 WIDGET-ID 14
     tg_gl-ctrl AT ROW 9.71 COL 24.8 WIDGET-ID 66
     tg_gl-rpt AT ROW 9.71 COL 45 WIDGET-ID 84
     tg_gl-rptd AT ROW 9.71 COL 66 WIDGET-ID 32
     tg_item AT ROW 9.71 COL 88.2 WIDGET-ID 46
     tg_item-bom AT ROW 10.62 COL 4.8 WIDGET-ID 16
     tg_item-spec AT ROW 10.62 COL 24.8 WIDGET-ID 68
     tg_itemfg AT ROW 10.62 COL 45 WIDGET-ID 86
     tg_itemfg-bom AT ROW 10.62 COL 66 WIDGET-ID 34
     tg_itemfg-ink AT ROW 10.62 COL 88.4 WIDGET-ID 48
     tg_loadtag AT ROW 11.48 COL 65.8 WIDGET-ID 94
     tg_loc AT ROW 11.48 COL 88.4 WIDGET-ID 92
     tg_itemfg-loc AT ROW 11.52 COL 4.8 WIDGET-ID 18
     tg_itemfgdtl AT ROW 11.52 COL 24.8 WIDGET-ID 70
     tg_jc-ctrl AT ROW 11.52 COL 45 WIDGET-ID 88
     tg_mach AT ROW 12.38 COL 4.8 WIDGET-ID 20
     tg_mach-adder AT ROW 12.38 COL 24.8 WIDGET-ID 40
     tg_mach-calendar AT ROW 12.38 COL 45 WIDGET-ID 90
     tg_mat-act AT ROW 12.43 COL 65.8 WIDGET-ID 22
     tg_matprep AT ROW 12.43 COL 88.4 WIDGET-ID 42
     tg_mmtx AT ROW 13.33 COL 4.8 WIDGET-ID 24
     tg_mmty AT ROW 13.33 COL 24.8 WIDGET-ID 44
     tg_mstd AT ROW 13.33 COL 45 WIDGET-ID 24
     tg_oe-ctrl AT ROW 13.33 COL 65.8 WIDGET-ID 44
     tg_oe-prmtx AT ROW 13.33 COL 88.4 WIDGET-ID 24
     tg_period AT ROW 14.23 COL 4.8 WIDGET-ID 24
     tg_po-ctrl AT ROW 14.23 COL 24.8 WIDGET-ID 44
     tg_prep AT ROW 14.23 COL 45 WIDGET-ID 24
     tg_procat AT ROW 14.23 COL 65.8 WIDGET-ID 44
     tg_prod AT ROW 14.23 COL 88.4 WIDGET-ID 24
     tg_prodl AT ROW 15.13 COL 4.8 WIDGET-ID 24
     tg_rm-bin AT ROW 15.13 COL 24.8 WIDGET-ID 44
     tg_rm-ctrl AT ROW 15.13 COL 45 WIDGET-ID 24
     tg_routing AT ROW 15.13 COL 65.8 WIDGET-ID 44
     tg_routing-mtx AT ROW 15.13 COL 88.4 WIDGET-ID 24
     tg_shift AT ROW 16.03 COL 4.8 WIDGET-ID 24
     tg_shipto AT ROW 16.03 COL 24.8 WIDGET-ID 44
     tg_sman AT ROW 16.03 COL 45 WIDGET-ID 24
     tg_sman-mtx AT ROW 16.03 COL 65.8 WIDGET-ID 44
     tg_soldto AT ROW 16.03 COL 88.4 WIDGET-ID 24
     tg_stack-flute AT ROW 16.93 COL 4.8 WIDGET-ID 24
     tg_stack-size AT ROW 16.93 COL 24.8 WIDGET-ID 44
     tg_stax AT ROW 16.93 COL 45 WIDGET-ID 24
     tg_stax-group AT ROW 16.93 COL 65.8 WIDGET-ID 44
     tg_std-code AT ROW 16.93 COL 88.4 WIDGET-ID 24
     tg_style AT ROW 17.83 COL 4.8 WIDGET-ID 24
     tg_sys-ctrl AT ROW 17.83 COL 24.8 WIDGET-ID 44
     tg_terms AT ROW 17.83 COL 45 WIDGET-ID 24
     tg_terr AT ROW 17.83 COL 65.8 WIDGET-ID 44
     tg_test-red AT ROW 17.83 COL 88.4 WIDGET-ID 24
     tg_usercomp AT ROW 18.73 COL 4.8 WIDGET-ID 24
     tg_vend AT ROW 18.73 COL 24.8 WIDGET-ID 44
     tg_ventype AT ROW 18.73 COL 45 WIDGET-ID 24
     tg_mach-part AT ROW 18.73 COL 65.8 WIDGET-ID 44
     tg_reftable AT ROW 18.73 COL 88.4 WIDGET-ID 24
     tg_box-design AT ROW 19.63 COL 4.8 WIDGET-ID 24

     tg_copy-transaction AT ROW 21.57 COL 49.4
     btnOK AT ROW 22.81 COL 51.4
     btnCancel AT ROW 22.81 COL 76.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 164.8 BY 23.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Copy Company Master Files"
         HEIGHT             = 23.67
         WIDTH              = 164.8
         MAX-HEIGHT         = 26.19
         MAX-WIDTH          = 176.2
         VIRTUAL-HEIGHT     = 26.19
         VIRTUAL-WIDTH      = 176.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btnCancel IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnCancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

/* SETTINGS FOR BUTTON btnOK IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnOK:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

/* SETTINGS FOR FILL-IN companyFrom IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN companyTo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR copyTable IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       copyTable:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.
       

/* SETTINGS FOR TOGGLE-BOX tg_copy-master IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Copy Company Master Files */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Copy Company Master Files */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME DEFAULT-FRAME /* OK */
DO:
  DISABLE {&BUTTONS} WITH FRAME {&FRAME-NAME}.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  RUN startCopy.
  MESSAGE 'Copy Complete!'  VIEW-AS ALERT-BOX.
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  ASSIGN
    companyFrom:SCREEN-VALUE = ipCompanyFrom
    companyTo:SCREEN-VALUE = ipCompanyTo.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{custom/companyCopy.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY companyFrom copyTable companyTo tg_copy-master tg_account tg_ap-buy 
          tg_ap-ctrl tg_ap-ledger tg_ar-ctrl tg_bank tg_buyer tg_carr-mtx 
          tg_carrier tg_ce-ctrl tg_costtype tg_crew tg_currency tg_cust 
          tg_cust-markup tg_db-ctrl tg_e-item tg_cust-part tg_cust-prod-sales 
          tg_custype tg_e-itemfg-vend tg_emp tg_e-item-cust tg_e-item-vend 
          tg_e-itemfg tg_fg-act tg_fg-bin tg_fg-ctrl tg_fg-set tg_fgcat tg_flute 
          tg_gl-ctrl tg_gl-rpt tg_gl-rptd tg_item tg_item-bom tg_item-spec 
          tg_itemfg tg_itemfg-bom tg_itemfg-ink tg_loadtag tg_loc tg_itemfg-loc 
          tg_itemfgdtl tg_jc-ctrl tg_mach tg_mach-adder tg_mach-calendar 
          tg_mat-act tg_matprep tg_mmtx tg_mmty tg_mstd tg_oe-ctrl 
          tg_oe-prmtx tg_period tg_po-ctrl tg_prep tg_procat tg_prod tg_prodl  
          tg_rm-bin tg_rm-ctrl tg_routing tg_routing-mtx tg_shift tg_shipto tg_sman 
          tg_sman-mtx tg_soldto tg_stack-flute tg_stack-size tg_stax tg_stax-group 
          tg_std-code tg_style tg_sys-ctrl tg_terms tg_terr tg_test-red tg_usercomp 
          tg_vend tg_ventype tg_mach-part tg_reftable tg_box-design  
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tg_account tg_ap-buy tg_ap-ctrl tg_ap-ledger tg_ar-ctrl tg_bank 
         tg_buyer tg_carr-mtx tg_carrier tg_ce-ctrl tg_costtype tg_crew 
         tg_currency tg_cust tg_cust-markup tg_db-ctrl tg_e-item tg_cust-part 
         tg_cust-prod-sales tg_custype tg_e-itemfg-vend tg_emp tg_e-item-cust 
         tg_e-item-vend tg_e-itemfg tg_fg-act tg_fg-bin tg_fg-ctrl tg_fg-set 
         tg_fgcat tg_flute tg_gl-ctrl tg_gl-rpt tg_gl-rptd tg_item tg_item-bom 
         tg_item-spec tg_itemfg tg_itemfg-bom tg_itemfg-ink tg_loadtag tg_loc 
         tg_itemfg-loc tg_itemfgdtl tg_jc-ctrl tg_mach tg_mach-adder 
         tg_mach-calendar tg_mat-act tg_matprep tg_mmtx tg_mmty tg_mstd tg_oe-ctrl 
         tg_oe-prmtx tg_period tg_po-ctrl tg_prep tg_procat tg_prod tg_prodl  
         tg_rm-bin tg_rm-ctrl tg_routing tg_routing-mtx tg_shift tg_shipto tg_sman 
         tg_sman-mtx tg_soldto tg_stack-flute tg_stack-size tg_stax tg_stax-group 
         tg_std-code tg_style tg_sys-ctrl tg_terms tg_terr tg_test-red tg_usercomp 
         tg_vend tg_ventype tg_mach-part tg_reftable tg_box-design 
         btnOK btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showMsg C-Win 
PROCEDURE showMsg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipTableName AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEndMsg AS LOGICAL NO-UNDO.

  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    copyTable:SCREEN-VALUE = copyTable:SCREEN-VALUE +
      (IF ipEndMsg THEN 'Done.' + CHR(10)
       ELSE 'Copying ' + ipTableName + ' ... ').
    ldummy = copyTable:MOVE-TO-EOF().
    PROCESS EVENTS.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextFGSet C-Win 
FUNCTION getNextFGSet RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  get next fg-set.s-no value for unique index create
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER bFGSet FOR fg-set.

  FIND LAST bFGSet USE-INDEX s-no NO-ERROR.
  RETURN (IF AVAILABLE bFGSet THEN bFGSet.s-no ELSE 0) + 1.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextMMtxNo C-Win 
FUNCTION getNextMMtxNo RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  get next mmtx.mmtx-no value for unique index create
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER bMMtx FOR mmtx.

  FIND LAST bMMtx USE-INDEX mmtx-no NO-ERROR.
  RETURN (IF AVAILABLE bMMtx THEN bMMtx.mmtx-no ELSE 0) + 1.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

