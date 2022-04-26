&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-fgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-fgexp 
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
DEFINE INPUT PARAMETER ipcItemFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcItemTo   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcItemNameFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcItemNameTo   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCustPartFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCustPartTo   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCustFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCustTo   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEstFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEstTo   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcStyleFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcStyleTo   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCategoryFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCategoryTo   AS CHARACTER NO-UNDO.

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
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

ASSIGN 
    cTextListToSelect  = "Item #,Item Name,Customer Part #,Customer,Customer Name," +
                            "Estimate,Style,Category,Category Description,Description,Description 2,Description 3,Stock/Custom," +
                            "Die #,Plate #,UPC#,CAD #,Quality/SPC #,Stocked," +
                            "Set Header,Group,Exempt from Disc," +
                            "P/M,Sell Price,Sell Price UOM,Type Code,Currency," +
                            "Warehouse,Bin,Inventory Class,Cycle Count Code,Production Code," +
                            "Count,Weight,Freeze Weight,Pk Note,Freight Class,Freight Class Desc,Pallet#,Zone,Sub Zone," +
                            "StackHeight,PalletLen,PalletWid,PalletDep,StdPalletVol," +
                            "Std Material Cost,Std Labor Cost,Std Var OH Cost,Std Fix OH Cost," +
                            "Total Std Cost,Average Cost,Last Cost,Cost UOM,Full Cost," +
                            "Varied,Taxable,Status,Ship Method," +  
                            "Vendor 1,Vendor 1 Item #,Vendor 2,Vendor 2 Item #," +
                            "Stocked,Set Allocation,Reorder Policy,Reorder Level," +
                            "Minimum Order,Maximum Order,Purchased Qty UOM,Lead Time (Days),Beginning Date," +
                            "Beginning Balance,Qty On-hand,Qty On Ord,Qty Allocated,Qty Backordered,Qty Available," +
                            "Qty Ordered PTD,Qty Ordered YTD,Qty Ordered Last Yr," +
                            "Qty Produced PTD,Qty Produced YTD,Qty Produced Last Yr," +
                            "Qty Shipped PTD,Qty Shipped YTD,Qty Shipped Last Yr," +
                            "Qty Invoiced PTD,Qty Invoiced YTD,Qty Invoiced Last Yr," +
                            "Total MSF PTD,Total MSF YTD,Total MSF Last Yr," +
                            "Box Length,Box Width,Box Depth,Blank Length,Blank Width," +
                            "Total Sq In,Total Sq Ft,Est Ink1,Est Ink2,Est Ink3,Est Ink4,Est Ink5," +
                            "Est Ink6,Est Ink7,Est Ink8,Est Ink9,Est Ink10,Est Coating1,Est Coating2,Est Coating3," +
                            "Board Code,Board Name,Caliper,Case Code,Case Name,Case Qty,Skid Code," + 
                            "Skid Name,Skid Qty,Release Sequence,Units/Pall,Factor Invoice,Sales Rep," +
                            "Spec Note 1 Group,Spec Note 1 Title,Spec Note 1 Note [Large]," +
                            "Spec Note 2 Group,Spec Note 2 Title,Spec Note 2 Note [Large]," +
                            "Spec Note 3 Group,Spec Note 3 Title,Spec Note 3 Note [Large]," +
                            "Spec Note 4 Group,Spec Note 4 Title,Spec Note 4 Note [Large]," +
                            "Spec Note 5 Group,Spec Note 5 Title,Spec Note 5 Note [Large]," +
                            "Setup By UserId,Setup Date,Modified By,Modified Date,Pallet Quantity," +
                            "FgItem Color1,FgItem Color2,FgItem Color3,FgItem Color4,FgItem Color5," +
                            "FgItem Color6,FgItem Color7,FgItem Color8,FgItem Color9,FgItem Color10,Product Tax Class"
    cFieldListToSelect = "i-no,i-name,part-no,cust-no,cust-name," +
                            "est-no,style,procat,procat-desc,part-dscr1,part-dscr2,part-dscr3,i-code," +
                            "die-no,plate-no,upc-no,cad-no,spc-no,stocked," +
                            "isaset,spare-char-1,exempt-disc," +
                            "pur-man,sell-price,sell-uom,type-code,curr-code[1]," +
                            "def-loc,def-loc-bin,class,cc-code,prod-code," + 
                            "case-count,weight-100,spare-int-1,prod-notes,frt-class,frt-class-dscr,trno,spare-char-4,subZone," +
                            "stackHeight,unitLength,unitWidth,unitHeight,palletVolume," +
                            "std-mat-cost,std-lab-cost,std-var-cost,std-fix-cost," +
                            "total-std-cost,avg-cost,last-cost,prod-uom,spare-dec-1," +
                            "spare-char-2,taxable,stat,ship-meth," + 
                            "vend-no,vend-item,vend2-no,vend2-item," +
                            "stocked,dfuncAlloc,ord-policy,ord-level," +
                            "ord-min,ord-max,pur-uom,lead-days,beg-date," +
                            "beg-bal,q-onh,q-ono,q-alloc,q-back,q-avail," +
                            "q-ptd,q-ord-ytd,u-ord," +
                            "q-prod-ptd,q-prod-ytd,u-prod," +
                            "q-ship-ptd,q-ship-ytd,u-ship," +
                            "q-inv-ptd,q-inv-ytd,u-inv," +
                            "dfuncTotMSFPTD,ytd-msf,lyytd-msf," +
                            "l-score[50],w-score[50],d-score[50],t-len,t-wid," +
                            "t-sqin,t-sqft,col1,col2,col3,col4,col5," +
                            "col6,col7,col8,col9,col10,cat1,cat2,cat3," +
                            "brd-cd,brd-nam,calp,cas-cd,cas-nam,cas-qt,skid-cd," +
                            "skid-nam,skid-qt,spare-int-2,case-pall,factored,spare-char-3," +
                            "spc-grp1,spc-title1,spc-note1," +
                            "spc-grp2,spc-title2,spc-note2," +
                            "spc-grp3,spc-title3,spc-note3," +
                            "spc-grp4,spc-title4,spc-note4," +
                            "spc-grp5,spc-title5,spc-note5," +
                            "setupBy,setupDate,modifiedBy,modifiedDate,pallet-qty," +
                            "fgcol1,fgcol2,fgcol3,fgcol4,fgcol5," +
                            "fgcol6,fgcol7,fgcol8,fgcol9,fgcol10,productTaxClass"
    /*         cFieldListToSelect = "itemfg.i-no,itemfg.i-name,itemfg.part-no,itemfg.cust-no," +                 */
    /*                             "itemfg.est-no,itemfg.style,itemfg.procat,itemfg.part-dscr1,itemfg.i-code," + */
    /*                             "itemfg.cad-no,itemfg.spc-no,itemfg.stocked,itemfg.q-onh"                     */
    .
{sys/inc/ttRptSel.i}

ASSIGN 
    cTextListToDefault = "Item #,Customer,Customer Part #,Item Name," +
                            "Description,Description 2,Description 3,Group,Exempt from Disc," +
                            "Style,Die #,Plate #,CAD #,Quality/SPC #,UPC#,Release Sequence,Taxable," +
                            "Varied,Status,P/M,Ship Method,Stock/Custom,Sell Price,Sell Price UOM," +
                            "Currency,Category,Type Code,Warehouse,Bin,Inventory Class,Cycle Count Code," +
                            "Count,Units/Pall,Production Code,Weight,Pk Note,Freight Class,Freight Class Desc,Pallet#,Zone,Sub Zone," +
                            "StackHeight,PalletLen,PalletWid,PalletDep,StdPalletVol," +
                            "Std Material Cost,Std Labor Cost,Std Var OH Cost,Std Fix OH Cost," +
                            "Full Cost,Cost UOM,Stocked,Box Length,Box Width,Box Depth,Blank Length,Blank Width," +
                            "Factor Invoice,Reorder Policy,Reorder Level,Minimum Order,Maximum Order," +
                            "Purchased Qty UOM,Lead Time (Days),Beginning Date,Sales Rep," +
                            "Spec Note 1 Group,Spec Note 1 Title,Spec Note 1 Note [Large]," +
                            "Spec Note 2 Group,Spec Note 2 Title,Spec Note 2 Note [Large]," +
                            "Spec Note 3 Group,Spec Note 3 Title,Spec Note 3 Note [Large]," +
                            "Spec Note 4 Group,Spec Note 4 Title,Spec Note 4 Note [Large]," +
                            "Spec Note 5 Group,Spec Note 5 Title,Spec Note 5 Note [Large],Product Tax Class," +
                            "Vendor 1,Vendor 1 Item #,Vendor 2,Vendor 2 Item #"
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-fgexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_i-no end_i-no ~
begin_i-name end_i-name begin_cust-part-no end_cust-part-no begin_cust ~
end_cust begin_est end_est begin_style end_style begin_procat end_procat ~
v-dept tb_active tb_inactive tb_spec sl_avail sl_selected Btn_Def Btn_Add ~
Btn_Remove btn_Up btn_down fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no end_i-no begin_i-name ~
end_i-name begin_cust-part-no end_cust-part-no begin_cust end_cust ~
begin_est end_est begin_style end_style begin_procat end_procat v-dept ~
tb_active tb_inactive tb_spec sl_avail sl_selected fi_file tb_OpenCSV ~
tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-fgexp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-fgexp 
FUNCTION assignParam RETURNS CHARACTER
    ( ipc-param AS CHARACTER , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-fgexp 
FUNCTION buildHeader RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-itemfg rd-fgexp 
FUNCTION getValue-itemfg RETURNS CHARACTER
    ( BUFFER ipb-itemfg FOR itemfg, ipc-field AS CHARACTER )  FORWARD.

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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "From Customer" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_cust-part-no AS CHARACTER FORMAT "X(30)" 
     LABEL "From Customer Part #" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_est AS CHARACTER FORMAT "X(8)" 
     LABEL "From Estimate" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_i-name AS CHARACTER FORMAT "x(15)" 
     LABEL "From Item Name" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "x(15)" 
     LABEL "From Item #" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(8)" 
     LABEL "From Category" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_style AS CHARACTER FORMAT "X(8)" 
     LABEL "From Style" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzzzzzzzzzzzzz" 
     LABEL "To Customer" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_cust-part-no AS CHARACTER FORMAT "X(30)" INITIAL "zzzzzzzzzzzzzzzzzzz" 
     LABEL "To Customer Part #" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_est AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzzzzzzzzzzzzz" 
     LABEL "To Estimate" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_i-name AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzzzzzz" 
     LABEL "To Item Name" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzzzzzz" 
     LABEL "To Item #" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzz" 
     LABEL "To Category" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_style AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To Style" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\FGIemExport.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 52 BY 1.

DEFINE VARIABLE v-dept AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.67.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 2.48.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_active AS LOGICAL INITIAL yes 
     LABEL "Active ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_inactive AS LOGICAL INITIAL yes 
     LABEL "Inactive ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.2 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL yes 
     LABEL "Open CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_spec AS LOGICAL INITIAL yes 
     LABEL "Print Spec Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-fgexp
     begin_i-no AT ROW 1.95 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Item Number" WIDGET-ID 142
     end_i-no AT ROW 1.95 COL 71 COLON-ALIGNED HELP
          "Enter Ending Item #" WIDGET-ID 144
     begin_i-name AT ROW 3.14 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Item Number" WIDGET-ID 150
     end_i-name AT ROW 3.14 COL 71 COLON-ALIGNED HELP
          "Enter Ending Item #" WIDGET-ID 156
     begin_cust-part-no AT ROW 4.33 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Part Number" WIDGET-ID 120
     end_cust-part-no AT ROW 4.33 COL 71 COLON-ALIGNED HELP
          "Enter Ending Customer Part Number" WIDGET-ID 122
     begin_cust AT ROW 5.52 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 6
     end_cust AT ROW 5.52 COL 71 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 16
     begin_est AT ROW 6.71 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Estimate" WIDGET-ID 100
     end_est AT ROW 6.71 COL 71 COLON-ALIGNED HELP
          "Enter Ending Estimate" WIDGET-ID 102
     begin_style AT ROW 7.91 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Style" WIDGET-ID 104
     end_style AT ROW 7.91 COL 71 COLON-ALIGNED HELP
          "Enter Ending Style" WIDGET-ID 106
     begin_procat AT ROW 9.1 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Category" WIDGET-ID 108
     end_procat AT ROW 9.1 COL 71 COLON-ALIGNED HELP
          "Enter Ending Category" WIDGET-ID 110
     v-dept AT ROW 10.91 COL 76.4 COLON-ALIGNED HELP
          "Enter Spec Code" NO-LABEL WIDGET-ID 162
     tb_active AT ROW 11 COL 16.6 WIDGET-ID 158
     tb_inactive AT ROW 11 COL 36.6 WIDGET-ID 160
     tb_spec AT ROW 11 COL 56 WIDGET-ID 164
     sl_avail AT ROW 13.76 COL 7 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 13.76 COL 62.6 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 13.91 COL 42.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 15.14 COL 42.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     Btn_Remove AT ROW 16.33 COL 42.2 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 17.52 COL 42.2 WIDGET-ID 136
     btn_down AT ROW 18.71 COL 42.2 WIDGET-ID 132
     fi_file AT ROW 21.38 COL 18.8 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     tb_OpenCSV AT ROW 21.48 COL 87.6 RIGHT-ALIGNED WIDGET-ID 34
     tbAutoClose AT ROW 23.14 COL 42 WIDGET-ID 58
     tb_excel AT ROW 23.24 COL 4 WIDGET-ID 32
     btn-ok AT ROW 23.95 COL 32.6 WIDGET-ID 14
     btn-cancel AT ROW 23.95 COL 52.6 WIDGET-ID 12
     "Available Columns" VIEW-AS TEXT
          SIZE 18.6 BY .62 AT ROW 13.05 COL 13.4 WIDGET-ID 140
     "Selected Columns" VIEW-AS TEXT
          SIZE 18.6 BY .62 AT ROW 13.05 COL 69.4 WIDGET-ID 138
     " Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 12.33 COL 5 WIDGET-ID 86
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21.2 BY .71 AT ROW 1.14 COL 5 WIDGET-ID 36
     RECT-6 AT ROW 12.67 COL 4 WIDGET-ID 30
     RECT-7 AT ROW 1.52 COL 4 WIDGET-ID 38
     RECT-8 AT ROW 20.71 COL 4 WIDGET-ID 84
     SPACE(2.99) SKIP(2.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 
         TITLE "Export FG Items to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd-fgexp
   FRAME-NAME                                                           */
ASSIGN 
       FRAME rd-fgexp:SCROLLABLE       = FALSE
       FRAME rd-fgexp:HIDDEN           = TRUE.

ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       begin_cust-part-no:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       begin_est:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       begin_i-name:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       begin_procat:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       begin_style:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_cust-part-no:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_est:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_i-name:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_procat:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_style:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       tb_active:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-fgexp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_excel:HIDDEN IN FRAME rd-fgexp           = TRUE
       tb_excel:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       tb_inactive:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME rd-fgexp
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       tb_spec:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       v-dept:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-fgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON HELP OF FRAME rd-fgexp /* Export FG Items to Excel */
DO:
        DEFINE VARIABLE lw-focus   AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE ls-cur-val AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE char-val   AS CHARACTER     NO-UNDO.

        lw-focus = FOCUS.

        CASE lw-focus:NAME :

            WHEN "begin_i-no" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-itemfg.w (cocode, "", ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* itemfg */
            WHEN "end_i-no" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-itemfg.w (cocode, "", ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* itemfg*/
            WHEN "begin_cust" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-cust.w (cocode, ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* cust */
            WHEN "end_cust" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-cust.w (cocode, ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* cust*/
            WHEN "begin_est" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-fgest.w (cocode, "", ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        FIND FIRST eb WHERE eb.company = cocode AND recid(eb) = int(ENTRY(1,char-val))
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE eb THEN lw-focus:SCREEN-VALUE =  eb.est-no.
                    END.
                    RETURN NO-APPLY.
                END.  /* est */
            WHEN "end_est" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-fgest.w (cocode, "", ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        FIND FIRST eb WHERE eb.company = cocode AND recid(eb) = int(ENTRY(1,char-val))
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE eb THEN lw-focus:SCREEN-VALUE =  eb.est-no.
                    END.
                    RETURN NO-APPLY.
                END.  /* est*/
            WHEN "begin_style" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-style.w (cocode, ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* style */
            WHEN "end_style" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-style.w (cocode, ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* style */
            WHEN "begin_procat" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-fgcat.w (cocode, ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* procat */
            WHEN "end_procat" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-fgcat.w (cocode, ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* procat */
            WHEN "v-dept" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
           
                    RUN cec/l-itspec.w (cocode, FOCUS:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        ASSIGN 
                            FOCUS:SCREEN-VALUE = ENTRY(1,char-val) .
                    END.
                END.

        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON WINDOW-CLOSE OF FRAME rd-fgexp /* Export FG Items to Excel */
DO:
        IF VALID-HANDLE(hdOutputProcs) THEN  
            DELETE PROCEDURE hdOutputProcs.  
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust rd-fgexp
ON LEAVE OF begin_cust IN FRAME rd-fgexp /* From Customer */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-part-no rd-fgexp
ON LEAVE OF begin_cust-part-no IN FRAME rd-fgexp /* From Customer Part # */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est rd-fgexp
ON LEAVE OF begin_est IN FRAME rd-fgexp /* From Estimate */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-name rd-fgexp
ON LEAVE OF begin_i-name IN FRAME rd-fgexp /* From Item Name */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no rd-fgexp
ON LEAVE OF begin_i-no IN FRAME rd-fgexp /* From Item # */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat rd-fgexp
ON LEAVE OF begin_procat IN FRAME rd-fgexp /* From Category */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_style rd-fgexp
ON LEAVE OF begin_style IN FRAME rd-fgexp /* From Style */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-fgexp
ON CHOOSE OF btn-cancel IN FRAME rd-fgexp /* Cancel */
DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-fgexp
ON CHOOSE OF btn-ok IN FRAME rd-fgexp /* OK */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-fgexp
ON CHOOSE OF Btn_Add IN FRAME rd-fgexp /* Add >> */
DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-fgexp
ON CHOOSE OF Btn_Def IN FRAME rd-fgexp /* Default */
DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-fgexp
ON CHOOSE OF btn_down IN FRAME rd-fgexp /* Move Down */
DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-fgexp
ON CHOOSE OF Btn_Remove IN FRAME rd-fgexp /* << Remove */
DO:
        APPLY "DEFAULT-ACTION" TO sl_selected  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-fgexp
ON CHOOSE OF btn_Up IN FRAME rd-fgexp /* Move Up */
DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust rd-fgexp
ON LEAVE OF end_cust IN FRAME rd-fgexp /* To Customer */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-part-no rd-fgexp
ON LEAVE OF end_cust-part-no IN FRAME rd-fgexp /* To Customer Part # */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est rd-fgexp
ON LEAVE OF end_est IN FRAME rd-fgexp /* To Estimate */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-name rd-fgexp
ON LEAVE OF end_i-name IN FRAME rd-fgexp /* To Item Name */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no rd-fgexp
ON LEAVE OF end_i-no IN FRAME rd-fgexp /* To Item # */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_procat rd-fgexp
ON LEAVE OF end_procat IN FRAME rd-fgexp /* To Category */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_style rd-fgexp
ON LEAVE OF end_style IN FRAME rd-fgexp /* To Style */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgexp
ON HELP OF fi_file IN FRAME rd-fgexp /* Name */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgexp
ON LEAVE OF fi_file IN FRAME rd-fgexp /* Name */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-fgexp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-fgexp
DO:
  
        IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) OR
            sl_selected:NUM-ITEMS = 0)
            THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
                ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
                /* sl_selected:SCREEN-VALUE = sl_selected:ENTRY(sl_selected:NUM-ITEMS) */
                .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-fgexp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-fgexp
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


&Scoped-define SELF-NAME tb_active
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_active rd-fgexp
ON VALUE-CHANGED OF tb_active IN FRAME rd-fgexp /* Active ? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-fgexp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-fgexp /* Export To Excel? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inactive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inactive rd-fgexp
ON VALUE-CHANGED OF tb_inactive IN FRAME rd-fgexp /* Inactive ? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV rd-fgexp
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME rd-fgexp /* Open CSV? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_spec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_spec rd-fgexp
ON VALUE-CHANGED OF tb_spec IN FRAME rd-fgexp /* Print Spec Notes? */
DO:
        ASSIGN {&self-name}.
        IF tb_spec THEN
            v-dept:SENSITIVE = YES .
        ELSE 
        DO:
            v-dept:SENSITIVE = NO .
            v-dept:SCREEN-VALUE = "" .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-dept rd-fgexp
ON LEAVE OF v-dept IN FRAME rd-fgexp
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-fgexp 


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

APPLY "entry" TO begin_i-no.
fi_file:SCREEN-VALUE = "c:\tmp\FGIemExport.csv".
END.


IF tb_spec:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes" THEN
    v-dept:SENSITIVE IN FRAME {&FRAME-NAME} = YES .
ELSE 
DO:
    v-dept:SENSITIVE IN FRAME {&FRAME-NAME} = NO .
    v-dept:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" .
END.

WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-fgexp  _DEFAULT-DISABLE
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
  HIDE FRAME rd-fgexp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-fgexp  _DEFAULT-ENABLE
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
  DISPLAY begin_i-no end_i-no begin_i-name end_i-name begin_cust-part-no 
          end_cust-part-no begin_cust end_cust begin_est end_est begin_style 
          end_style begin_procat end_procat v-dept tb_active tb_inactive tb_spec 
          sl_avail sl_selected fi_file tb_OpenCSV tbAutoClose 
      WITH FRAME rd-fgexp.
  ENABLE RECT-6 RECT-7 RECT-8 begin_i-no end_i-no begin_i-name end_i-name 
         begin_cust-part-no end_cust-part-no begin_cust end_cust begin_est 
         end_est begin_style end_style begin_procat end_procat v-dept tb_active 
         tb_inactive tb_spec sl_avail sl_selected Btn_Def Btn_Add Btn_Remove 
         btn_Up btn_down fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
      WITH FRAME rd-fgexp.
  VIEW FRAME rd-fgexp.
  {&OPEN-BROWSERS-IN-QUERY-rd-fgexp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-fgexp 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
    DEFINE BUFFER b-itemfg FOR itemfg.

    DEFINE VARIABLE list-name          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-pdf-file        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-mail-file       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ret-code           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-style            LIKE eb.style NO-UNDO.
    DEFINE VARIABLE v-len              LIKE eb.len NO-UNDO.
    DEFINE VARIABLE v-wid              LIKE eb.wid NO-UNDO.
    DEFINE VARIABLE v-dep              LIKE eb.dep NO-UNDO.
    DEFINE VARIABLE v-blen             LIKE eb.t-len NO-UNDO.
    DEFINE VARIABLE v-bwid             LIKE eb.t-wid NO-UNDO.
    DEFINE VARIABLE v-cad              LIKE eb.cad-no NO-UNDO.
    DEFINE VARIABLE v-die              LIKE eb.die-no NO-UNDO.
    DEFINE VARIABLE v-plate            LIKE eb.plate NO-UNDO.
    DEFINE VARIABLE v-board            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-board2           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-ink-cnt          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-coat-cnt         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-ink              AS cha       FORM "x(10)" EXTENT 10 NO-UNDO.
    DEFINE VARIABLE v-coat             AS cha       FORM "x(10)" EXTENT 10 NO-UNDO. 
    DEFINE VARIABLE v-case-count       LIKE itemfg.case-count NO-UNDO.
    DEFINE VARIABLE v-pallet-count     LIKE itemfg.case-count NO-UNDO.
    DEFINE VARIABLE v-cases-per-pallet LIKE itemfg.case-count NO-UNDO.
    DEFINE VARIABLE v-clip             LIKE ITEM.cal NO-UNDO.
    DEFINE VARIABLE v-tr-no            AS cha       INIT "" NO-UNDO.
    DEFINE VARIABLE v-tr-name          AS cha       INIT "" NO-UNDO.
    DEFINE VARIABLE v-cas-no           AS cha       INIT "" NO-UNDO.
    DEFINE VARIABLE v-cas-name         AS cha       INIT "" NO-UNDO.
    DEFINE VARIABLE v-spec-note        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSpecGroup         AS CHARACTER EXTENT 7 NO-UNDO.
    DEFINE VARIABLE cSpecTitle         AS CHARACTER EXTENT 7 NO-UNDO.
    DEFINE VARIABLE cSpecNote          AS CHARACTER EXTENT 7 NO-UNDO.
    DEFINE VARIABLE cFGColor           AS CHARACTER FORM "x(10)" EXTENT 10 NO-UNDO.

    v-excelheader = buildHeader().
    SESSION:SET-WAIT-STATE ("general").

    IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
    IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

    FOR EACH b-itemfg WHERE b-itemfg.company = cocode
        AND b-itemfg.i-no GE begin_i-no
        AND b-itemfg.i-no LE end_i-no
        AND b-itemfg.i-name GE begin_i-name
        AND b-itemfg.i-name LE end_i-name
        AND b-itemfg.part-no GE begin_cust-part-no
        AND b-itemfg.part-no LE end_cust-part-no
        AND b-itemfg.cust-no GE begin_cust
        AND b-itemfg.cust-no LE end_cust
        AND b-itemfg.est-no GE begin_est
        AND b-itemfg.est-no LE end_est
        AND b-itemfg.style GE begin_style
        AND b-itemfg.style LE end_style
        AND b-itemfg.procat GE begin_procat
        AND b-itemfg.procat LE end_procat
        AND ( ((b-itemfg.stat = "A" OR b-itemfg.stat = "" ) AND tb_active ) 
        OR (b-itemfg.stat = "I" AND tb_inactive ) )
        NO-LOCK:

        v-excel-detail-lines = "".
        ASSIGN
            v-len              = 0     
            v-wid              = 0    
            v-dep              = 0     
            v-blen             = 0  
            v-bwid             = 0 
            v-cad              = ""  
            v-die              = ""  
            v-plate            = ""
            v-tr-no            = "" 
            v-tr-name          = ""
            v-cas-no           = ""
            v-cas-name         = ""
            v-board            = ""
            v-board2           = ""
            v-case-count       = 0
            v-pallet-count     = 0
            v-cases-per-pallet = 0 
            v-clip             = 0
            v-ink              = ""
            v-coat             = ""
            cFGColor           = "".


        v-spec-note = "".
        i = 0.
        IF tb_spec THEN 
        DO:
            MAIN-NOTES:
            FOR EACH notes WHERE notes.rec_key = b-itemfg.rec_key AND
                can-do(v-dept,notes.note_code) NO-LOCK:
                IF AVAILABLE notes THEN v-spec-note = v-spec-note + " " + notes.note_text.
                i = i + 1.
                ASSIGN 
                    cSpecGroup[i] = notes.note_code
                    cSpecTitle[i] = notes.note_title
                    cSpecNote[i]  = notes.note_text.
                IF i GE 6 THEN LEAVE MAIN-NOTES.
            END.
        END.


        FIND FIRST est WHERE est.company = cocode
            AND est.est-no = b-itemfg.est-no NO-LOCK NO-ERROR.
        /* lv-est-rec-key = IF AVAIL est THEN est.rec_key ELSE "".*/
     
        IF b-itemfg.ship-meth THEN
            ASSIGN v-case-count   = b-itemfg.case-count
                v-pallet-count = 0.
        ELSE
            ASSIGN v-case-count   = 0
                v-pallet-count = b-itemfg.case-count.
     
        FIND FIRST eb WHERE eb.company = cocode
            AND eb.est-no = b-itemfg.est-no 
            AND eb.stock-no = b-itemfg.i-no NO-LOCK NO-ERROR.
     
        IF AVAILABLE eb THEN 
        DO:
            FIND FIRST ef WHERE ef.company = cocode
                AND ef.est-no = b-itemfg.est-no 
                AND ef.form-no = eb.form-no  NO-LOCK NO-ERROR.
        
            FIND FIRST style WHERE style.company = b-itemfg.company
                AND style.style = eb.style NO-LOCK NO-ERROR. 
     
            ASSIGN 
                v-style            = IF AVAILABLE style THEN style.dscr ELSE eb.style
                v-len              = eb.len
                v-wid              = eb.wid
                v-dep              = eb.dep
                v-blen             = eb.t-len
                v-bwid             = eb.t-wid
                v-cad              = eb.cad-no
                v-die              = eb.die-no
                v-plate            = eb.plate-no
                v-tr-no            = eb.tr-no
                v-cas-no           = eb.cas-no
                v-board            = IF AVAILABLE ef THEN ef.board ELSE ""
                v-board2           = IF AVAILABLE ef THEN ef.brd-dscr ELSE ""
                v-case-count       = eb.cas-cnt
                v-pallet-count     = eb.tr-cnt
                v-cases-per-pallet = eb.cas-pal
                .  
            FIND FIRST ITEM WHERE ITEM.company = eb.company
                AND ITEM.i-no = eb.tr-no NO-LOCK NO-ERROR.

            IF AVAILABLE ITEM THEN v-tr-name = ITEM.i-NAME.
            FIND FIRST ITEM WHERE ITEM.company = eb.company
                AND ITEM.i-no = eb.cas-no NO-LOCK NO-ERROR.
            IF AVAILABLE ITEM THEN v-cas-name = ITEM.i-NAME.
            FIND FIRST ITEM WHERE ITEM.company = eb.company
                AND ITEM.i-no = v-board NO-LOCK NO-ERROR.

        

            IF AVAILABLE ITEM THEN v-clip = ITEM.cal . 
     
            ASSIGN 
                v-ink-cnt  = 1
                v-coat-cnt = 1. 
     
            DO i = 1 TO 10:
                IF eb.est-type <= 4 THEN 
                DO:
                    IF eb.i-code2[i] <> "" THEN 
                    DO:
                        FIND FIRST ITEM WHERE ITEM.company = cocode
                            AND ITEM.i-no = eb.i-code2[i] NO-LOCK NO-ERROR.
                        IF AVAILABLE ITEM AND ITEM.mat-type = "V" THEN 
                            ASSIGN v-coat[v-coat-cnt] = eb.i-code2[i]
                                v-coat-cnt         = v-coat-cnt + 1.
                        ELSE ASSIGN v-ink[v-ink-cnt] = eb.i-code2[i]
                                v-ink-cnt        = v-ink-cnt + 1.
                    END.
                END.
                ELSE 
                DO:
                    IF eb.i-code[i] <> "" THEN 
                    DO:
                        FIND FIRST ITEM WHERE ITEM.company = cocode
                            AND ITEM.i-no = eb.i-code[i] NO-LOCK NO-ERROR.
                        IF AVAILABLE ITEM AND ITEM.mat-type = "V" THEN 
                            ASSIGN v-coat[v-coat-cnt] = eb.i-code[i]
                                v-coat-cnt         = v-coat-cnt + 1.
                        ELSE ASSIGN v-ink[v-ink-cnt] = eb.i-code[i]
                                v-ink-cnt        = v-ink-cnt + 1.
                    END.
                END.
            END. 
     
        END.  
        v-ink-cnt = 1.
        MAIN-LOOP-INK:
        FOR EACH itemfg-ink OF b-itemfg NO-LOCK:  
            cFGColor[v-ink-cnt] = itemfg-ink.rm-i-no.            
            IF v-ink-cnt GE 10 THEN LEAVE MAIN-LOOP-INK.
            v-ink-cnt = v-ink-cnt + 1 .
        END.        
        
        /*      IF v-case-count EQ 0 AND v-cases-per-pallet EQ 0 THEN                                          */
        /*      DO:                                                                                            */
        /*         FIND FIRST fg-bin WHERE                                                                     */
        /*              fg-bin.company = cocode AND                                                            */
        /*              fg-bin.i-no = b-itemfg.i-no                                                            */
        /*              NO-LOCK NO-ERROR.                                                                      */
        /*         IF AVAIL fg-bin THEN DO:                                                                    */
        /*            FOR EACH fg-rcpth WHERE fg-rcpth.company = cocode                                        */
        /*                                AND fg-rcpth.i-no = b-itemfg.i-no                                    */
        /*                                AND fg-rcpth.rita-code = "R" NO-LOCK                                 */
        /*                                BY fg-rcpth.trans-date DESC.                                         */
        /*                FIND FIRST fg-rdtlh WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no                        */
        /*                                      AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code NO-LOCK NO-ERROR. */
        /*                IF AVAIL fg-rdtlh THEN ASSIGN v-case-count = fg-rdtlh.qty-case                       */
        /*                                              v-cases-per-pallet = fg-rdtlh.stacks-unit.             */
        /*            END.                                                                                     */
        /*         END.                                                                                        */
        /*      END.                                                                                           */
   

        FOR EACH ttRptSelected:

            IF LOOKUP(ttRptSelected.FieldList,"fgcol1,fgcol2,fgcol3,fgcol4,fgcol5,fgcol6,fgcol7,fgcol8,fgcol9,fgcol10,col1,col2,col3,col4,col5,col6,col7,col8,col9,col10,cat1,cat2,cat3,brd-cd,brd-nam,calp,cas-cd,cas-nam,cas-qt,skid-cd,skid-nam,skid-qt,spec-cod1,spc-grp1,spc-title1,spc-note1,spc-grp2,spc-title2,spc-note2,spc-grp3,spc-title3,spc-note3,spc-grp4,spc-title4,spc-note4,spc-grp5,spc-title5,spc-note5,pallet-qty") = 0 THEN
                v-excel-detail-lines = v-excel-detail-lines + 
                    appendXLLine(getValue-itemfg(BUFFER b-itemfg,ttRptSelected.FieldList)).
        
        
            ELSE 
            DO:
                CASE ttRptSelected.FieldList:                                                              
                    WHEN "col1" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-ink[1]).   
                    WHEN "col2" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-ink[2]). 
                    WHEN "col3" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-ink[3]). 
                    WHEN "col4" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-ink[4]). 
                    WHEN "col5" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-ink[5]). 
                    WHEN "col6" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-ink[6]). 
                    WHEN "col7" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-ink[7]). 
                    WHEN "col8" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-ink[8]). 
                    WHEN "col9" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-ink[9]). 
                    WHEN "col10" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-ink[10]).
                    WHEN "cat1" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-coat[1]).   
                    WHEN "cat2" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-coat[2]). 
                    WHEN "cat3" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-coat[3]).
                    WHEN "brd-cd" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-board).    
                    WHEN "brd-nam" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-board2).
                    WHEN "calp" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(v-clip)).
                    WHEN "cas-cd" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-cas-no).
                    WHEN "cas-nam" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-cas-name).
                    WHEN "cas-qt" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(v-case-count)).    
                    WHEN "skid-cd" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(v-tr-no)).
                    WHEN "skid-nam" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-tr-name).
                    WHEN "skid-qt" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(v-pallet-count)).
                    WHEN "spec-cod1" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-spec-note).
                    WHEN "spc-grp1" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecGroup[1]).
                    WHEN "spc-title1" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecTitle[1]).
                    WHEN "spc-note1" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecNote[1]).

                    WHEN "spc-grp2" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecGroup[2]).
                    WHEN "spc-title2" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecTitle[2]).
                    WHEN "spc-note2" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecNote[2]).

                    WHEN "spc-grp3" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecGroup[3]).
                    WHEN "spc-title3" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecTitle[3]).
                    WHEN "spc-note3" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecNote[3]).

                    WHEN "spc-grp4" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecGroup[4]).
                    WHEN "spc-title4" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecTitle[4]).
                    WHEN "spc-note4" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecNote[4]).

                    WHEN "spc-grp5" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecGroup[5]).
                    WHEN "spc-title5" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecTitle[5]).
                    WHEN "spc-note5" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cSpecNote[5]).
                    WHEN "pallet-qty" THEN                                                              
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING((b-itemfg.case-count * b-itemfg.case-pall ) + b-itemfg.quantityPartial)).
                    WHEN "fgcol1" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cFGColor[1]).   
                    WHEN "fgcol2" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cFGColor[2]). 
                    WHEN "fgcol3" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cFGColor[3]). 
                    WHEN "fgcol4" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cFGColor[4]). 
                    WHEN "fgcol5" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cFGColor[5]). 
                    WHEN "fgcol6" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cFGColor[6]). 
                    WHEN "fgcol7" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cFGColor[7]). 
                    WHEN "fgcol8" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cFGColor[8]). 
                    WHEN "fgcol9" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cFGColor[9]). 
                    WHEN "fgcol10" THEN                                                                
                        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cFGColor[10]).    
                
                END CASE.  
            END.
        END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-fgexp 
PROCEDURE Set-Sort-Data :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:

        /* If a customer number was entered, find first and last matching customers. */
        ASSIGN 
            begin_i-no:SCREEN-VALUE         = assignParam(ipcItemFrom,NO)
            end_i-no:SCREEN-VALUE           = assignParam(ipcItemTo,YES)
            begin_i-name:SCREEN-VALUE       = assignParam(ipcItemNameFrom,NO)
            end_i-name:SCREEN-VALUE         = assignParam(ipcItemNameTo,YES)
            begin_cust-part-no:SCREEN-VALUE = assignParam(ipcCustPartFrom,NO)
            end_cust-part-no:SCREEN-VALUE   = assignParam(ipcCustPartTo,YES)
            begin_cust:SCREEN-VALUE         = assignParam(ipcCustFrom,NO)
            end_cust:SCREEN-VALUE           = assignParam(ipcCustTo,YES)
            begin_est:SCREEN-VALUE          = assignParam(ipcEstFrom,NO)
            end_est:SCREEN-VALUE            = assignParam(ipcEstTo,YES)
            begin_style:SCREEN-VALUE        = assignParam(ipcStyleFrom,NO)
            end_style:SCREEN-VALUE          = assignParam(ipcStyleTo,YES)
            begin_procat:SCREEN-VALUE       = assignParam(ipcCategoryFrom,NO)
            end_procat:SCREEN-VALUE         = assignParam(ipcCategoryTo,YES).

    END.

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-fgexp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Adds a value to a csv line
        Notes:  Protects agains commans and quotes.
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-line AS CHARACTER NO-UNDO.
    
    ipc-append = DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, ipc-append).
    
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-itemfg rd-fgexp 
FUNCTION getValue-itemfg RETURNS CHARACTER
    ( BUFFER ipb-itemfg FOR itemfg, ipc-field AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Take a buffer and field name as string and return the value
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE h-field   AS HANDLE.
    DEFINE VARIABLE li-extent AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lc-return AS CHARACTER FORMAT "x(100)" NO-UNDO.

    CASE ipc-field :
        WHEN "dfuncStatus"  THEN 
            DO:
                /*             FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"      */
                /*                 AND reftable.company  EQ ipb-itemfg.company                */
                /*                 AND reftable.loc      EQ ""                                */
                /*                 AND reftable.code     EQ ipb-itemfg.i-no NO-LOCK NO-ERROR. */
                /*             IF AVAIL reftable THEN DO: */
                /*                 IF reftable.code2 EQ "A" THEN */
                IF ipb-itemfg.stat = "A" THEN
                    lc-return = "Active".
                ELSE
                    lc-return = "Inactive".
            /*             END. */
            END.
        WHEN "dfuncAlloc"  THEN 
            DO:
                CASE ipb-itemfg.alloc :
                    WHEN YES THEN
                        lc-return = "Unassembled".
                    WHEN NO THEN
                        lc-return = "Assembled".
                    OTHERWISE
                    lc-return = "Assembled w/Part Receipts".
                END CASE.
            END.
        WHEN "dfuncTotMSFPTD"  THEN 
            DO:
                IF g_period NE 0 THEN lc-return = STRING(ipb-itemfg.ptd-msf[g_period]).
            END.
        WHEN "pur-man" THEN
            DO:
                IF ipb-itemfg.pur-man EQ TRUE THEN
                    lc-return = "P".
                ELSE
                    lc-return = "M".            
            END.                               
        WHEN "ship-meth"  THEN 
            DO:
                CASE ipb-itemfg.ship-meth :
                    WHEN YES THEN
                        lc-return = "Case".
                    WHEN NO THEN
                        lc-return = "Pallet".
                END CASE.
            END.
        OTHERWISE 
        DO:
            IF INDEX(ipc-field,"[") > 0 THEN 
            DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-itemfg:BUFFER-FIELD(ipc-field).
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

