&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jcrep\r-jobsum.w

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha NO-UNDO.

DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = gcompany
 locode = gloc.

DO TRANSACTION:
   {sys/inc/tspost.i}
END.

{jc/rep/job-sum.i new}

DEFINE VARIABLE ll-secure AS LOG NO-UNDO.

DEFINE STREAM excel.
DEFINE STREAM excel2 .
DEFINE VARIABLE fi_file AS CHARACTER NO-UNDO.

ASSIGN
    fi_file = "c:\tmp\r-jobsumN2.csv".


DEFINE VARIABLE v-header-1 AS CHARACTER INIT "FG ITEM,FG ITEM DESCRIPTION,SELLING PRICE/M,ORDERED,POSTED,FINISHED,ALLOWED,ACTUAL SPOILAGE,ACT SPL%,ESTIMATE SPOILAGE,EST SPL%,OVER-RUN %" NO-UNDO .
DEFINE VARIABLE v-header-2 AS CHARACTER INIT "MACH CODE,MACH DESCRIPTION,M R,QUANTITY PRODUCED,EST HOURS,EST SPEED,MACH EST COST,ACT HOURS,ACT SPEED,MACH ACT COST,MACH COST VARIANCE,MACH COST VAR%,MACH EST QTY,MACH ACTUAL,MACH VAR %" NO-UNDO .
DEFINE VARIABLE v-header-3 AS CHARACTER INIT "ITEM CODE,ITEM DESCRIPTION,ITEM EST QUANTITY,ITEM EST UM,ITEM EST COST,ITEM ACT QUANTITY,ITEM ACT UM,ITEM ACT COST,ITEM COST VARI,ITEM COST VAR%" NO-UNDO .
DEFINE VARIABLE v-header-4 AS CHARACTER INIT "MISC CODE,MISC DESCRIPTION,MISC EST COST,MISC ACT COST,MISC VARI,MISC VAR%" NO-UNDO .
DEFINE VARIABLE ldummy AS LOG NO-UNDO.
DEFINE VARIABLE cTextListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cColumnInit AS LOG INIT YES NO-UNDO.
DEFINE VARIABLE iColumnLength AS INTEGER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.

ASSIGN cTextListToSelect = "FG ITEM,FG ITEM DESCRIPTION,SELLING PRICE/M,ORDERED,POSTED,FINISHED,ALLOWED,ACTUAL SPOILAGE,ACT SPL%,ESTIMATE SPOILAGE,EST SPL%,OVER-RUN %," + 
                           "---MACHINE HEADER------,MACH CODE,MACH DESCRIPTION,M R,QUANTITY PRODUCED,EST HOURS,EST SPEED,MACH EST COST,ACT HOURS,ACT SPEED,MACH ACT COST,MACH COST VARIANCE,MACH COST VAR%,MACH EST QTY,MACH ACTUAL,MACH VAR %," +
                           "---ITEM HEADER---,ITEM CODE,ITEM DESCRIPTION,ITEM EST QUANTITY,ITEM EST UM,ITEM EST COST,ITEM ACT QUANTITY,ITEM ACT UM,ITEM ACT COST,ITEM COST VARI,ITEM COST VAR%," +
                           "---MISC HEADER---,MISC CODE,MISC DESCRIPTION,MISC EST COST,MISC ACT COST,MISC VARI,MISC VAR%"
       cFieldListToSelect = "fg-item,fgitem-desc,sel-price,order,posted,finished,allowed,act-spoil,act-spoil%,est-spoil,est-spoil-per,over-run," +
                            "machheader,machine,mach-desc,mr,qty-prod,est-hours,est-speed,est-cost,act-hours,act-speed,act-cost,cost-vari,cost-var%,est-qty,actual,var-per," +
                            "itemheader,item-code,item-desc,item-est-qty,item-est-um,item-est-cost,item-act-qty,item-act-um,item-act-cost,item-cost-var,item-cost-var-per," +
                            "mischeader,misc-code,misc-desc,misc-est-cost,misc-act-cost,misc-vari,misc-vari-per"

       cFieldLength = "15,30,10,10,10,10,10,9,5,9,6,6," +
                      "15,7,16,2,8,7,6,8,7,6,6,12,6,12,9,9," +
                      "15,15,20,10,3,10,9,3,10,10,8," +
                      "15,11,25,10,10,10,10"   

        cFieldType = "c,c,i,i,i,i,i,i,i,i,i,i," + "c,c,c,c,i,i,i,i,i,i,i,i,i,i,i,i," + "c,c,c,i,i,i,i,i,i,i,i," + "c,c,c,i,i,i,i"    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "FG ITEM,FG ITEM DESCRIPTION,SELLING PRICE/M,ORDERED,POSTED,FINISHED,ALLOWED,ACTUAL SPOILAGE,ACT SPL%,ESTIMATE SPOILAGE,EST SPL%,OVER-RUN %," + 
                           "---MACHINE HEADER------,MACH CODE,MACH DESCRIPTION,M R,QUANTITY PRODUCED,EST HOURS,EST SPEED,MACH EST COST,ACT HOURS,ACT SPEED,MACH ACT COST,MACH COST VARIANCE,MACH COST VAR%,MACH EST QTY,MACH ACTUAL,MACH VAR %," +
                           "---ITEM HEADER---,ITEM CODE,ITEM DESCRIPTION,ITEM EST QUANTITY,ITEM EST UM,ITEM EST COST,ITEM ACT QUANTITY,ITEM ACT UM,ITEM ACT COST,ITEM COST VARI,ITEM COST VAR%," +
                           "---MISC HEADER---,MISC CODE,MISC DESCRIPTION,MISC EST COST,MISC ACT COST,MISC VARI,MISC VAR%" 
                            .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_jstat tb_fold tb_corr ~
begin_job-no begin_job-no2 end_job-no end_job-no2 begin_date end_date ~
tb_totals tb_inv_tot_only labor-mkup tb_curr tb_show-dl tb_show-fo ~
tb_show-vo tb_prep tb_excel rs_sort-material tb_exclude_prep ~
tb_exclude_run_if_no_prod tb_waste-from-issued tb_incl_farmout tb_sum_mat ~
tb_sum-mischg tb_JobsSheets rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm btn-ok btn-cancel btn_SelectColumns tb_excel2 tb_runExcel fi_file2
&Scoped-Define DISPLAYED-OBJECTS lbl_jstat rd_jstat tb_fold lbl_industry ~
tb_corr begin_job-no begin_job-no2 end_job-no end_job-no2 begin_date ~
end_date tb_totals tb_inv_tot_only labor-mkup tb_curr tb_curr-crew ~
tb_show-dl tb_show-fo tb_show-vo tb_prep tb_excel rs_sort-material ~
tb_exclude_prep tb_exclude_run_if_no_prod tb_waste-from-issued ~
tb_incl_farmout tb_sum_mat tb_sum-mischg tb_JobsSheets rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_excel2 tb_runExcel fi_file2

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getInvoiceTotal C-Win 
FUNCTION getInvoiceTotal RETURNS DECIMAL
  ( ipiOrder AS INTEGER, ipcJob AS CHARACTER, ipcJobNo2 AS INTEGER, ipcPriceOrQty AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn_SelectColumns 
     LABEL "Select Columns" 
     SIZE 30 BY 1.67.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE labor-mkup AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Labor Markup" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_industry AS CHARACTER FORMAT "X(256)":U INITIAL "Industry?" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_jstat AS CHARACTER FORMAT "X(256)":U INITIAL "Job Status?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .95 NO-UNDO.

DEFINE VARIABLE fi_file2 AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-jobsumN.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Email", 5
     SIZE 23 BY 4.52 NO-UNDO.

DEFINE VARIABLE rd_jstat AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All", "All"
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE rs_sort-material AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item Code", "Item Code",
"Type", "Material Type"
     SIZE 24 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.14.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 20.95.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 4.52 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 4.52 NO-UNDO.

DEFINE VARIABLE tb_corr AS LOGICAL INITIAL YES 
     LABEL "Corrugated" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .71 NO-UNDO.

DEFINE VARIABLE tb_curr AS LOGICAL INITIAL YES 
     LABEL "Calculate Using Current Machine Rates?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tb_curr-crew AS LOGICAL INITIAL NO 
     LABEL "and Crew Sizes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO 
     LABEL "Export Grand Totals to Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel2 AS LOGICAL INITIAL YES 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL NO 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_exclude_prep AS LOGICAL INITIAL NO 
     LABEL "Exclude Prep?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_exclude_run_if_no_prod AS LOGICAL INITIAL NO 
     LABEL "Exclude run hours if ZERO production?" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE tb_fold AS LOGICAL INITIAL YES 
     LABEL "Folding" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .71 NO-UNDO.

DEFINE VARIABLE tb_incl_farmout AS LOGICAL INITIAL NO 
     LABEL "Include Farmout Costs" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_inv_tot_only AS LOGICAL INITIAL NO 
     LABEL "Invoiced Totals Only" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE tb_JobsSheets AS LOGICAL INITIAL NO 
     LABEL "Show Jobs Estimated Sheets" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prep AS LOGICAL INITIAL NO 
     LABEL "Add Billable Prep Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tb_show-dl AS LOGICAL INITIAL NO 
     LABEL "Show Direct Labor?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tb_show-fo AS LOGICAL INITIAL NO 
     LABEL "Show Fixed Overhead?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tb_show-vo AS LOGICAL INITIAL NO 
     LABEL "Show Variable Overhead?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sum-mischg AS LOGICAL INITIAL NO 
     LABEL "Subtotal Miscellaneous Charges" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sum_mat AS LOGICAL INITIAL NO 
     LABEL "Summarize Materials" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_totals AS LOGICAL INITIAL NO 
     LABEL "Print Totals" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE tb_waste-from-issued AS LOGICAL INITIAL NO 
     LABEL "Waste = Board Issued - Quantity Posted" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     btn_SelectColumns AT ROW 20.00 COL 60 WIDGET-ID 10
     lbl_jstat AT ROW 2.05 COL 27.2 COLON-ALIGNED NO-LABELS
     rd_jstat AT ROW 2.05 COL 42.2 NO-LABELS
     tb_fold AT ROW 3.48 COL 47.2
     lbl_industry AT ROW 3.71 COL 34.2 COLON-ALIGNED NO-LABELS
     tb_corr AT ROW 4.19 COL 47.2
     begin_job-no AT ROW 5.29 COL 29.2 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 5.29 COL 41.2 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 5.29 COL 64.2 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 5.29 COL 76.2 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_date AT ROW 6.24 COL 29.2 COLON-ALIGNED
     end_date AT ROW 6.24 COL 64.2 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     tb_totals AT ROW 7.19 COL 35.2
     tb_inv_tot_only AT ROW 7.19 COL 51.2 WIDGET-ID 20
     labor-mkup AT ROW 8.38 COL 29.2 COLON-ALIGNED HELP
          "Enter a Negative or Positive Percentage"
     tb_curr AT ROW 9.57 COL 23 WIDGET-ID 2
     tb_curr-crew AT ROW 9.57 COL 66.2 WIDGET-ID 18
     tb_show-dl AT ROW 10.48 COL 23
     tb_show-fo AT ROW 11.43 COL 23
     tb_show-vo AT ROW 12.38 COL 23
     tb_prep AT ROW 13.33 COL 23 WIDGET-ID 4
     tb_excel AT ROW 14.24 COL 23
     rs_sort-material AT ROW 14.57 COL 67 NO-LABELS WIDGET-ID 6
     tb_exclude_prep AT ROW 15.19 COL 23 WIDGET-ID 12
     tb_exclude_run_if_no_prod AT ROW 16.14 COL 23 WIDGET-ID 14
     tb_waste-from-issued AT ROW 17.05 COL 23 WIDGET-ID 16
     tb_incl_farmout AT ROW 18 COL 23 WIDGET-ID 22
     tb_sum_mat AT ROW 18.95 COL 23 WIDGET-ID 24
     tb_sum-mischg AT ROW 19.91 COL 23 WIDGET-ID 26
     tb_JobsSheets AT ROW 20.86 COL 23 WIDGET-ID 28
     rd-dest AT ROW 23.19 COL 5 NO-LABELS
     lv-ornt AT ROW 23.24 COL 30 NO-LABELS
     lines-per-page AT ROW 23.43 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 24.52 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 25.52 COL 27 COLON-ALIGNED NO-LABELS
     td-show-parm AT ROW 27.81 COL 5
     tb_excel2 AT ROW 26.91 COL 66.80 RIGHT-ALIGNED
     tb_runExcel AT ROW 26.91 COL 88.6 RIGHT-ALIGNED
     btn-ok AT ROW 29.33 COL 24
     btn-cancel AT ROW 29.33 COL 56
     sl_avail AT ROW 18.43 COL 10 NO-LABELS WIDGET-ID 26
     sl_selected AT ROW 18.43 COL 50 NO-LABELS WIDGET-ID 28
     fi_file2 AT ROW 27.76 COL 44.6 COLON-ALIGNED HELP
          "Enter File Name"
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 22.24 COL 4
     "Material Sort By" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 13.86 COL 70 WIDGET-ID 10
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 21.95 COL 1
     RECT-7 AT ROW 1.24 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 94.4 BY 29.71.


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
         TITLE              = "Job Summary Analysis Report"
         HEIGHT             = 29.95
         WIDTH              = 95
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_industry IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_jstat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_jstat".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_corr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_curr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_curr-crew IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_curr-crew:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_exclude_prep:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_exclude_run_if_no_prod:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_fold:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_incl_farmout:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_inv_tot_only:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_JobsSheets:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prep:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_show-dl:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_show-fo:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_show-vo:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sum-mischg:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sum_mat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_totals:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_waste-from-issued:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR SELECTION-LIST sl_avail IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       sl_avail:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_selected IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       sl_selected:HIDDEN IN FRAME FRAME-A           = TRUE.
/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".
ASSIGN 
       fi_file2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE     */
/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Job Summary Analysis Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Job Summary Analysis Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   APPLY "close" TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  RUN GetSelectionList.

  RUN run-report. 
  STATUS DEFAULT "Processing Complete". 

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 5 THEN
       DO:
          DEFINE VARIABLE lv-tmp AS CHARACTER INIT "-0" NO-UNDO.

          {custom/asimailr.i &TYPE="Customer"
                             &begin_cust=lv-tmp
                             &END_cust=lv-tmp
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
       END.
  END CASE. 

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn_SelectColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_SelectColumns C-Win
ON CHOOSE OF btn_SelectColumns IN FRAME FRAME-A /* Select Columns */
DO:
    DEFINE VARIABLE cTextSelected AS cha NO-UNDO.
    DEFINE VARIABLE cTextListed AS cha NO-UNDO.

    RUN displaySelectionList2.

    ASSIGN cTextSelected = sl_selected:LIST-ITEMS
           cTextListed = sl_avail:LIST-ITEMS.

    IF NOT cColumnInit THEN RUN custom/d-rptsel.w (INPUT-OUTPUT cTextListed, INPUT-OUTPUT cTextSelected, INPUT-OUTPUT cTextListToDefault, INPUT-OUTPUT cTextListToSelect).

    ASSIGN sl_selected:LIST-ITEMS = cTextSelected
           sl_avail:LIST-ITEMS = cTextListed.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME labor-mkup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL labor-mkup C-Win
ON LEAVE OF labor-mkup IN FRAME FRAME-A /* Labor Markup */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file2 C-Win
ON LEAVE OF fi_file2 IN FRAME FRAME-A /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEFINE VARIABLE char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON LEAVE OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
   ASSIGN lv-font-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON LEAVE OF lv-ornt IN FRAME FRAME-A
DO:
  ASSIGN lv-ornt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON VALUE-CHANGED OF lv-ornt IN FRAME FRAME-A
DO:
  {custom/chgfont.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_jstat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_jstat C-Win
ON VALUE-CHANGED OF rd_jstat IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_corr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_corr C-Win
ON VALUE-CHANGED OF tb_corr IN FRAME FRAME-A /* Corrugated */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_curr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_curr C-Win
ON VALUE-CHANGED OF tb_curr IN FRAME FRAME-A /* Calculate Using Current Machine Rates? */
DO:
  ASSIGN {&self-name}.
  IF tb_curr THEN 
      tb_curr-crew:SENSITIVE = YES.
  ELSE 
      ASSIGN 
        tb_curr-crew:SENSITIVE = NO
        tb_curr-crew:SCREEN-VALUE = "NO"
        tb_curr-crew = NO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_curr-crew
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_curr-crew C-Win
ON VALUE-CHANGED OF tb_curr-crew IN FRAME FRAME-A /* and Crew Sizes? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export Grand Totals to Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_exclude_prep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_exclude_prep C-Win
ON VALUE-CHANGED OF tb_exclude_prep IN FRAME FRAME-A /* Exclude Prep? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_exclude_run_if_no_prod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_exclude_run_if_no_prod C-Win
ON VALUE-CHANGED OF tb_exclude_run_if_no_prod IN FRAME FRAME-A /* Exclude run hours if ZERO production? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fold C-Win
ON VALUE-CHANGED OF tb_fold IN FRAME FRAME-A /* Folding */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_incl_farmout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_incl_farmout C-Win
ON VALUE-CHANGED OF tb_incl_farmout IN FRAME FRAME-A /* Include Farmout Costs */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inv_tot_only
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inv_tot_only C-Win
ON VALUE-CHANGED OF tb_inv_tot_only IN FRAME FRAME-A /* Invoiced Totals Only */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_JobsSheets
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_JobsSheets C-Win
ON VALUE-CHANGED OF tb_JobsSheets IN FRAME FRAME-A /* Show Jobs Estimated Sheets */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prep C-Win
ON VALUE-CHANGED OF tb_prep IN FRAME FRAME-A /* Add Billable Prep Charges? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_show-dl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-dl C-Win
ON VALUE-CHANGED OF tb_show-dl IN FRAME FRAME-A /* Show Direct Labor? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_show-fo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-fo C-Win
ON VALUE-CHANGED OF tb_show-fo IN FRAME FRAME-A /* Show Fixed Overhead? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_show-vo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-vo C-Win
ON VALUE-CHANGED OF tb_show-vo IN FRAME FRAME-A /* Show Variable Overhead? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sum-mischg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sum-mischg C-Win
ON VALUE-CHANGED OF tb_sum-mischg IN FRAME FRAME-A /* Subtotal Miscellaneous Charges */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sum_mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sum_mat C-Win
ON VALUE-CHANGED OF tb_sum_mat IN FRAME FRAME-A /* Summarize Materials */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_totals
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_totals C-Win
ON VALUE-CHANGED OF tb_totals IN FRAME FRAME-A /* Print Totals */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_waste-from-issued
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_waste-from-issued C-Win
ON VALUE-CHANGED OF tb_waste-from-issued IN FRAME FRAME-A /* Waste = Board Issued - Quantity Posted */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel2 C-Win
ON VALUE-CHANGED OF tb_excel2 IN FRAME FRAME-A /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
         AND sys-ctrl.name   EQ "CEMENU"
       NO-LOCK NO-ERROR.
  IF AVAILABLE sys-ctrl THEN
    ASSIGN
     tb_fold = INDEX(" FB",SUBSTR(sys-ctrl.char-fld,1,1)) GT 0
     tb_corr = INDEX(" CB",SUBSTR(sys-ctrl.char-fld,1,1)) GT 0.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    IF tb_curr:SCREEN-VALUE = "YES" THEN
        tb_curr-crew:SENSITIVE = YES.
    APPLY 'choose' TO btn_SelectColumns IN FRAME {&FRAME-NAME}.
    cColumnInit = NO.
    APPLY "entry" TO begin_job-no.
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 C-Win 
PROCEDURE DisplaySelectionList2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cListContents AS cha NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE cTmpList AS cha NO-UNDO.

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

  cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

   DO iCount = 1 TO sl_selected:NUM-ITEMS:
       IF LOOKUP(ENTRY(iCount,cTmpList), cTextListToSelect) = 0 THEN
        ldummy = sl_selected:DELETE(ENTRY(iCount,cTmpList)).
  END.

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
  DISPLAY lbl_jstat rd_jstat tb_fold lbl_industry tb_corr begin_job-no 
          begin_job-no2 end_job-no end_job-no2 begin_date end_date tb_totals 
          tb_inv_tot_only labor-mkup tb_curr tb_curr-crew tb_show-dl tb_show-fo 
          tb_show-vo tb_prep tb_excel rs_sort-material tb_exclude_prep 
          tb_exclude_run_if_no_prod tb_waste-from-issued tb_incl_farmout 
          tb_sum_mat tb_sum-mischg tb_JobsSheets rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel2 tb_runExcel fi_file2
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 rd_jstat tb_fold tb_corr begin_job-no begin_job-no2 
         end_job-no end_job-no2 begin_date end_date tb_totals tb_inv_tot_only 
         labor-mkup tb_curr tb_show-dl tb_show-fo tb_show-vo tb_prep tb_excel 
         rs_sort-material tb_exclude_prep tb_exclude_run_if_no_prod 
         tb_waste-from-issued tb_incl_farmout tb_sum_mat tb_sum-mischg 
         tb_JobsSheets rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         btn-ok btn-cancel btn_SelectColumns tb_excel2 tb_runExcel fi_file2
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList C-Win 
PROCEDURE GetSelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VARIABLE cTmpList AS cha NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
 iColumnLength = 0.

 DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     

    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
           ttRptSelected.HeadingFromLeft = IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
           iColumnLength = iColumnLength + ttRptSelected.FieldLength + 1.
           .        

 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file C-Win 
PROCEDURE output-to-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 {custom/out2file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
  */
  RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-screen C-Win 
PROCEDURE output-to-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ----------------------------------------------- jc/rep/job-sum.p 08/94 JLF */
/* Job Summary Report                                                         */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDisplay AS cha NO-UNDO.
DEFINE VARIABLE cExcelDisplay AS cha NO-UNDO.
DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE cTmpField AS CHA NO-UNDO.
DEFINE VARIABLE cVarValue AS cha NO-UNDO.
DEFINE VARIABLE cExcelVarValue AS cha NO-UNDO.
DEFINE VARIABLE cSelectedList AS cha NO-UNDO.
DEFINE VARIABLE cFieldName AS cha NO-UNDO.
DEFINE VARIABLE fg-str-tit AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE fg-str-tit2 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE fg-str-tit3 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE fg-str-line AS cha FORM "x(300)" NO-UNDO.

DEFINE VARIABLE mach-str-tit AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE mach-str-tit2 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE mach-str-tit3 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE mach-str-line AS cha FORM "x(300)" NO-UNDO.

DEFINE VARIABLE item-str-tit AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE item-str-tit2 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE item-str-tit3 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE item-str-line AS cha FORM "x(300)" NO-UNDO.

DEFINE VARIABLE misc-str-tit AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE misc-str-tit2 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE misc-str-tit3 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE misc-str-line AS cha FORM "x(300)" NO-UNDO.

/*{sys/form/r-top5DL3.f} */
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.


ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

  v-stat        = SUBSTR(rd_jstat,1,1)
  v-ind         = IF tb_fold THEN
                    IF tb_corr THEN "B" ELSE "F"
                  ELSE
                    IF tb_corr THEN "C" ELSE ""
  v-job-no[1]   = FILL(" ",6 - length(TRIM(begin_job-no))) +
                  trim(begin_job-no) + string(int(begin_job-no2),"99")
  v-job-no[2]   = FILL(" ",6 - length(TRIM(end_job-no)))   +
                  trim(end_job-no)   + string(int(end_job-no2),"99") 

  v-date[1]     = begin_date
  v-date[2]     = END_date
  v-tot         = tb_totals  
  v-lab-mrk     = labor-mkup 
  v-lab         = tb_show-dl
  v-foh         = tb_show-fo
  v-voh         = tb_show-vo
  v-charge-prep = tb_prep
  v-mat-sort    = rs_sort-material
  v-inv-tot-only = tb_inv_tot_only
  v-incl-farmout = tb_incl_farmout
  v-merge-mat    = tb_sum_mat
  v-tot-mchg     = tb_sum-mischg . 

IF v-tot THEN DO: 
  IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).
  v-tot = ll-secure. 
END.

DEFINE VARIABLE cslist AS cha NO-UNDO.
 FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

     IF LOOKUP(ttRptSelected.TextList, v-header-1) <> 0    THEN DO:

         IF ttRptSelected.TextList = "SELLING PRICE/M" THEN 
             ASSIGN fg-str-tit = fg-str-tit +   "   SELLING" + " "
                    fg-str-tit2 = fg-str-tit2 + "   PRICE/M" + " "
                    fg-str-tit3 = fg-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
         ELSE IF ttRptSelected.TextList = "ACTUAL SPOILAGE" THEN 
             ASSIGN fg-str-tit = fg-str-tit +   "   ACTUAL" + " "
                    fg-str-tit2 = fg-str-tit2 + " SPOILAGE" + " "
                    fg-str-tit3 = fg-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
          ELSE IF ttRptSelected.TextList = "ACT SPL%" THEN 
             ASSIGN fg-str-tit = fg-str-tit +   "  ACT" + " "
                    fg-str-tit2 = fg-str-tit2 + " SPL%" + " "
                    fg-str-tit3 = fg-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
           ELSE IF ttRptSelected.TextList = "ESTIMATE SPOILAGE" THEN 
             ASSIGN fg-str-tit = fg-str-tit +   " ESTIMATE" + " "
                    fg-str-tit2 = fg-str-tit2 + " SPOILAGE" + " "
                    fg-str-tit3 = fg-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
            ELSE IF ttRptSelected.TextList = "EST SPL%" THEN 
             ASSIGN fg-str-tit = fg-str-tit +   "   EST" + " "
                    fg-str-tit2 = fg-str-tit2 + "   SPL%" + " "
                    fg-str-tit3 = fg-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
             ELSE IF ttRptSelected.TextList = "OVER-RUN %" THEN 
             ASSIGN fg-str-tit = fg-str-tit +   " OVER-" + " "
                    fg-str-tit2 = fg-str-tit2 + " RUN %" + " "
                    fg-str-tit3 = fg-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
             ELSE DO:

                 IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
                     THEN ASSIGN fg-str-tit2 = fg-str-tit2 + ttRptSelected.TextList + " "
                     fg-str-tit = fg-str-tit + FILL(" ",ttRptSelected.FieldLength) + " " 
                     fg-str-tit3 = fg-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " "
                   /*  excelheader2 = excelHeader2 + ttRptSelected.TextList + ","*/ .      
                 ELSE 
                     ASSIGN fg-str-tit2 = fg-str-tit2 + 
                         (IF ttRptSelected.HeadingFromLeft THEN
                             ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
                             ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                         fg-str-tit = fg-str-tit + FILL(" ",ttRptSelected.FieldLength) + " "
                         fg-str-tit3 = fg-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " "
                      /*   excelheader2 = excelHeader2 + ttRptSelected.TextList + "," */
                         .        
             END.

          excelheader1 = excelHeader1 + ttRptSelected.TextList + "," .  
          cSlist = cSlist + ttRptSelected.FieldList + ",".

          IF LOOKUP(ttRptSelected.TextList, "SELLING PRICE/M,ORDERED,POSTED,FINISHED,ALLOWED,ACTUAL SPOILAGE,ACT SPL%,ESTIMATE SPOILAGE,EST SPL%,OVER-RUN %") <> 0    THEN
              ASSIGN
              fg-str-line = fg-str-line + FILL("-",ttRptSelected.FieldLength) + " " .
          ELSE
              fg-str-line = fg-str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
     END.

     IF LOOKUP(ttRptSelected.TextList, v-header-2) <> 0    THEN DO:

         IF ttRptSelected.TextList = "MACH CODE" THEN 
             ASSIGN mach-str-tit =  mach-str-tit +   "   MACH" + " "
                    mach-str-tit2 = mach-str-tit2 +  "   CODE" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
         ELSE IF ttRptSelected.TextList = "M R" THEN 
             ASSIGN mach-str-tit =  mach-str-tit +   " M" + " "
                    mach-str-tit2 = mach-str-tit2 + " R" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
         ELSE IF ttRptSelected.TextList = "QUANTITY PRODUCED" THEN 
             ASSIGN mach-str-tit =  mach-str-tit +   "QUANTITY" + " "
                    mach-str-tit2 = mach-str-tit2 +  "PRODUCED" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
          ELSE IF ttRptSelected.TextList = "EST HOURS" THEN 
             ASSIGN mach-str-tit =  mach-str-tit +   "    EST" + " "
                    mach-str-tit2 = mach-str-tit2 + "  HOURS" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
           ELSE IF ttRptSelected.TextList = "EST SPEED" THEN 
             ASSIGN mach-str-tit =  mach-str-tit +   "   EST" + " "
                    mach-str-tit2 = mach-str-tit2 + " SPEED" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
            ELSE IF ttRptSelected.TextList = "MACH EST COST" THEN 
             ASSIGN mach-str-tit =  mach-str-tit +   "     EST" + " "
                    mach-str-tit2 = mach-str-tit2 + "    COST" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
             ELSE IF ttRptSelected.TextList = "ACT HOURS" THEN 
             ASSIGN mach-str-tit =  mach-str-tit +   "    ACT" + " "
                    mach-str-tit2 = mach-str-tit2 + "  HOURS" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .

             ELSE IF ttRptSelected.TextList = "ACT SPEED" THEN 
             ASSIGN mach-str-tit =  mach-str-tit +   "   ACT" + " "
                    mach-str-tit2 = mach-str-tit2 + " SPEED" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
             ELSE IF ttRptSelected.TextList = "MACH ACT COST" THEN 
             ASSIGN mach-str-tit =  mach-str-tit +   "   ACT" + " "
                    mach-str-tit2 = mach-str-tit2 +  "  COST" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
             ELSE IF ttRptSelected.TextList = "MACH COST VARIANCE" THEN 
             ASSIGN mach-str-tit =  mach-str-tit +   "        COST" + " "
                    mach-str-tit2 = mach-str-tit2 +  "    VARIANCE" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .

              ELSE IF ttRptSelected.TextList = "MACH COST VAR%" THEN 
             ASSIGN mach-str-tit =  mach-str-tit +   "  COST" + " "
                    mach-str-tit2 = mach-str-tit2 + "  VAR%" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
             ELSE IF ttRptSelected.TextList = "MACH EST QTY" THEN 
             ASSIGN mach-str-tit =  mach-str-tit +   "         EST" + " "
                    mach-str-tit2 = mach-str-tit2 + "         QTY" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
             ELSE IF ttRptSelected.TextList = "MACH ACTUAL" THEN 
             ASSIGN mach-str-tit =  mach-str-tit +   "         " + " "
                    mach-str-tit2 = mach-str-tit2 + "   ACTUAL" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
              ELSE IF ttRptSelected.TextList = "MACH VAR %" THEN 
             ASSIGN mach-str-tit =  mach-str-tit +   "         " + " "
                    mach-str-tit2 = mach-str-tit2 + "     VAR%" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .

             ELSE DO:

                 IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
                     THEN ASSIGN mach-str-tit2 = mach-str-tit2 + ttRptSelected.TextList + " "
                     mach-str-tit = mach-str-tit + FILL(" ",ttRptSelected.FieldLength) + " "
                     mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " "
                  /*   excelheader2 = excelHeader2 + ttRptSelected.TextList + ","*/ .        
                 ELSE 
                     ASSIGN mach-str-tit2 = mach-str-tit2 + 
                         (IF ttRptSelected.HeadingFromLeft THEN
                             ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
                             ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                         mach-str-tit = mach-str-tit + FILL(" ",ttRptSelected.FieldLength) + " "
                         mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " "
                    /*     excelheader2 = excelHeader2 + ttRptSelected.TextList + "," */
                         .   
             END.

          excelheader2 = excelHeader2 + ttRptSelected.TextList + "," .  
          cSlist = cSlist + ttRptSelected.FieldList + ",".

          IF LOOKUP(ttRptSelected.TextList, "EST HOURS,MACH EST COST,ACT HOURS,MACH ACT COST,MACH COST VARIANCE,MACH COST VAR%") <> 0    THEN
              ASSIGN
              mach-str-line = mach-str-line + FILL("-",ttRptSelected.FieldLength) + " " .
          ELSE
              mach-str-line = mach-str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
     END.

     IF LOOKUP(ttRptSelected.TextList, v-header-3) <> 0    THEN DO:

         IF ttRptSelected.TextList = "ITEM EST QUANTITY" THEN 
             ASSIGN item-str-tit =  item-str-tit +  "      EST" + " "
                    item-str-tit2 = item-str-tit2 + " QUANTITY" + " "
                    item-str-tit3 = item-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
         ELSE IF ttRptSelected.TextList = "ITEM EST UM" THEN 
             ASSIGN item-str-tit =  item-str-tit +  "EST" + " "
                    item-str-tit2 = item-str-tit2 + " UM" + " "
                    item-str-tit3 = item-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
          ELSE IF ttRptSelected.TextList = "ITEM EST COST" THEN 
             ASSIGN item-str-tit =  item-str-tit +  "       EST" + " "
                    item-str-tit2 = item-str-tit2 + "      COST" + " "
                    item-str-tit3 = item-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
            ELSE IF ttRptSelected.TextList = "ITEM ACT QUANTITY" THEN 
             ASSIGN item-str-tit =  item-str-tit +  "      ACT" + " "
                    item-str-tit2 = item-str-tit2 + " QUANTITY" + " "
                    item-str-tit3 = item-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
             ELSE IF ttRptSelected.TextList = "ITEM ACT UM" THEN 
             ASSIGN item-str-tit =  item-str-tit +   "ACT" + " "
                    item-str-tit2 = item-str-tit2 + " UM" + " "
                    item-str-tit3 = item-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .

             ELSE IF ttRptSelected.TextList = "ITEM ACT COST" THEN 
             ASSIGN item-str-tit =  item-str-tit +  "       ACT" + " "
                    item-str-tit2 = item-str-tit2 + "      COST" + " "
                    item-str-tit3 = item-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
             ELSE IF ttRptSelected.TextList = "ITEM COST VARI" THEN 
             ASSIGN item-str-tit =  item-str-tit +  "      COST" + " "
                    item-str-tit2 = item-str-tit2 + "      VARI" + " "
                    item-str-tit3 = item-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
             ELSE IF ttRptSelected.TextList = "ITEM COST VAR%" THEN 
             ASSIGN item-str-tit =  item-str-tit +  "    COST" + " "
                    item-str-tit2 = item-str-tit2 + "   VAR %" + " "
                    item-str-tit3 = item-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
             ELSE DO:  

                 IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
                     THEN ASSIGN item-str-tit2 = item-str-tit2 + ttRptSelected.TextList + " "
                     item-str-tit = item-str-tit + FILL(" ",ttRptSelected.FieldLength) + " "
                     item-str-tit3 = item-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " "
                  /*   excelheader2 = excelHeader2 + ttRptSelected.TextList + "," */ .        
                 ELSE 
                     ASSIGN item-str-tit2 = item-str-tit2 + 
                         (IF ttRptSelected.HeadingFromLeft THEN
                             ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
                             ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                         item-str-tit = item-str-tit + FILL(" ",ttRptSelected.FieldLength) + " "
                         item-str-tit3 = item-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " "
                     /*   excelheader2 = excelHeader2 + ttRptSelected.TextList + "," */
                         .      
             END.

          excelheader3 = excelHeader3 + ttRptSelected.TextList + "," .  
          cSlist = cSlist + ttRptSelected.FieldList + ",".

          IF LOOKUP(ttRptSelected.TextList, "ITEM EST COST,ITEM ACT COST,ITEM COST VARI,ITEM COST VAR%") <> 0    THEN
              ASSIGN
              item-str-line = item-str-line + FILL("-",ttRptSelected.FieldLength) + " " .
          ELSE
              item-str-line = item-str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
     END.

     IF LOOKUP(ttRptSelected.TextList, v-header-4) <> 0    THEN DO:

         IF ttRptSelected.TextList = "MISC EST COST" THEN 
             ASSIGN misc-str-tit =  misc-str-tit +  "  EST COST" + " "
                    misc-str-tit3 = misc-str-tit3 + "          " + " "
                    misc-str-tit2 = misc-str-tit2 + FILL("-",ttRptSelected.FieldLength) + " " .
         ELSE IF ttRptSelected.TextList = "MISC ACT COST" THEN 
             ASSIGN misc-str-tit =  misc-str-tit +  "  ACT COST" + " "
                    misc-str-tit3 = misc-str-tit3 + "          " + " "
                    misc-str-tit2 = misc-str-tit2 + FILL("-",ttRptSelected.FieldLength) + " " .
         ELSE DO:

             IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
                 THEN ASSIGN misc-str-tit = misc-str-tit + ttRptSelected.TextList + " "
                 misc-str-tit2 = misc-str-tit2 + FILL("-",ttRptSelected.FieldLength) + " "
                /* excelheader2 = excelHeader2 + ttRptSelected.TextList + ","*/ .        
             ELSE 
                 ASSIGN misc-str-tit = misc-str-tit + 
                     (IF ttRptSelected.HeadingFromLeft THEN
                         ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
                         ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                     misc-str-tit2 = misc-str-tit2 + FILL("-",ttRptSelected.FieldLength) + " "
                 /*    excelheader2 = excelHeader2 + ttRptSelected.TextList + "," */
                     .        
         END.

          excelheader4 = excelHeader4 + ttRptSelected.TextList + "," .  
          cSlist = cSlist + ttRptSelected.FieldList + ",".

          IF LOOKUP(ttRptSelected.TextList, "MISC EST COST,MISC ACT COST,MISC VARI,MISC VAR%") <> 0    THEN
              ASSIGN
              misc-str-line = misc-str-line + FILL("-",ttRptSelected.FieldLength) + " " .
          ELSE
              misc-str-line = misc-str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
     END.

 END.

IF tb_excel2 THEN DO:
  OUTPUT STREAM excel2 TO VALUE(fi_file2).
END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Job #,Customer,Item Code,Selling Price,Ordered,Est Hrs,Est Cost,Act Hrs,"
              + "Act Cost,Est Mat Cost,Act Mat Cost,Commission,"
              + "Freight,Boxes Sales,Prep Sales,Total Sales,Total Cost,"
              + "Customer Part #,Item Name,Category,Actual Board Mat,Actual Other Mat,Actual Labor,Qty Produced"   .
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

SESSION:SET-WAIT-STATE ("general").
{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF td-show-parm THEN RUN show-param.

DISPLAY "" WITH FRAME r-top.

{jcrep/r-jobsumN.i}


IF tb_excel2 THEN DO:
  OUTPUT STREAM excel2 CLOSE.
  IF tb_runExcel THEN
  OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file2)).
END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  /*IF tb_runExcel THEN*/
  OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-frame-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-group-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field2-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE parm-fld-list AS cha NO-UNDO.
  DEFINE VARIABLE parm-lbl-list AS cha NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-label AS cha.

  lv-frame-hdl = FRAME {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                     .
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     .
              lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                  END.
                  lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
              END.       
           END.                 
        END.            
     lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
  END.

  PUT SPACE(28)
      "< Selection Parameters >"
      SKIP(1).

  DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
    IF ENTRY(i,parm-fld-list) NE "" OR
       entry(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                 trim(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mach-data C-Win 
PROCEDURE mach-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER v-header AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER v-qty1 AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER v-qty2 AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER v-qty3 AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER v-qty4 AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER v-qty5 AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER v-qty6 AS DECIMAL NO-UNDO.

DEFINE VARIABLE cDisplay AS cha NO-UNDO.
DEFINE VARIABLE cExcelDisplay AS cha NO-UNDO.
DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE cTmpField AS CHA NO-UNDO.
DEFINE VARIABLE cVarValue AS cha NO-UNDO.
DEFINE VARIABLE cExcelVarValue AS cha NO-UNDO.
DEFINE VARIABLE cSelectedList AS cha NO-UNDO.
DEFINE VARIABLE cFieldName AS cha NO-UNDO.

cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):  
                IF LOOKUP(ENTRY(i,cSelectedList), v-header-2) = 0    THEN NEXT .
               cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "machine"    THEN cVarValue = "" .
                         WHEN "mach-desc"   THEN cVarValue = "" .
                         WHEN "mr"   THEN cVarValue =  "".
                         WHEN "qty-prod"   THEN cVarValue = "".
                         WHEN "est-hours"  THEN cVarValue = IF v-qty1 <> ? AND v-qty1 <> 0 THEN STRING(v-qty1,">>>9.99-") ELSE "" .
                         WHEN "est-speed"   THEN cVarValue = /*IF work-mch.est-speed <> ? THEN  STRING(work-mch.est-speed,">>>>>9-")  ELSE*/ "".
                         WHEN "est-cost"  THEN cVarValue = IF v-qty2 <> ? AND v-qty2 <> 0 THEN STRING(v-qty2,">>>>,>>9-") ELSE "".
                         WHEN "act-hours"   THEN cVarValue = IF v-qty3 <> ? AND v-qty3 <> 0 THEN  STRING(v-qty3,">>>,>>9-") ELSE "".
                         WHEN "act-speed"  THEN cVarValue = /*IF work-mch.run-speed <> ? THEN  STRING(work-mch.run-speed,">>>9.9-") ELSE*/ "" .
                         WHEN "act-cost"    THEN cVarValue = IF v-qty4 <> ? AND v-qty4 <> 0 THEN STRING(v-qty4,">>>>>9-") ELSE "" .

                         WHEN "cost-vari"   THEN cVarValue = IF v-qty5 <> ? AND v-qty5 <> 0 THEN  STRING(v-qty5,">>>>,>>>,>>9-") ELSE "".
                         WHEN "cost-var%"   THEN cVarValue = IF v-qty6 <> ? AND v-qty6 <> 0 THEN STRING(v-qty6,">>>9.9-") ELSE "".
                         WHEN "est-qty"  THEN cVarValue = /*IF work-mch.run-waste <> ? THEN  STRING(work-mch.run-waste,">>>>,>>>,>>9-") ELSE*/ "".
                         WHEN "actual"   THEN cVarValue = /*IF work-mch.wst-qty <> ? THEN  STRING(work-mch.wst-qty,">,>>>,>>9-") ELSE*/ "".
                         WHEN "var-per"   THEN cVarValue = /*IF v-run-wst-var <> ? THEN  STRING(v-run-wst-var,">>>>>>9.9-") ELSE*/ "".
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED "        " + string(v-header,"x(27)")  + substring(cDisplay,36,250) SKIP .
            IF tb_excel2 THEN DO:
                 PUT STREAM excel2 UNFORMATTED ','  v-header 
                     SUBSTRING(cExcelDisplay,6,300) SKIP.
             END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getInvoiceTotal C-Win 
FUNCTION getInvoiceTotal RETURNS DECIMAL
  ( ipiOrder AS INTEGER, ipcJob AS CHARACTER, ipcJobNo2 AS INTEGER, ipcPriceOrQty AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE dInvTot AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-subtot-lines AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-subtot-qty AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-inv-freight AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-inv-total AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-total-qty AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-total-prep AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-subtot-prep AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-save-x AS INTEGER NO-UNDO.
v-subtot-lines = 0.
FOR EACH oe-ord WHERE oe-ord.company EQ cocode
    AND oe-ord.ord-no = ipiOrder NO-LOCK,
    EACH oe-ordl OF oe-ord NO-LOCK,
    EACH oe-boll WHERE oe-boll.company EQ oe-ordl.company
      AND oe-boll.ord-no EQ oe-ordl.ord-no
    NO-LOCK
    BREAK BY oe-boll.bol-no:
    IF FIRST-OF(oe-boll.bol-no) THEN DO:
        ASSIGN v-inv-total    = 0
               v-inv-freight  = 0
               v-subtot-lines = 0
               v-subtot-qty   = 0
               v-total-qty    = 0
               v-total-prep   = 0
               v-subtot-prep  = 0.
        FOR EACH inv-head WHERE inv-head.company EQ oe-boll.company
            AND inv-head.bol-no EQ oe-boll.bol-no
            NO-LOCK:

            FOR EACH inv-line NO-LOCK WHERE inv-line.r-no = inv-head.r-no
              AND inv-line.job-no EQ ipcJob
              AND inv-line.job-no2 EQ ipcJobNo2:
                v-subtot-lines = v-subtot-lines + inv-line.t-price.
                v-subtot-qty   = v-subtot-qty   + inv-line.qty.
                FOR EACH inv-misc WHERE inv-misc.r-no EQ inv-line.r-no
                  AND inv-misc.LINE EQ inv-line.LINE NO-LOCK:
                  v-subtot-prep = v-subtot-prep + misc.cost.

                END.
            END.
            v-inv-freight = IF inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0.
            ASSIGN v-inv-total = v-subtot-lines /* 8/5/14 inv-head.t-inv-tax +  v-inv-freight */
                   v-total-qty = v-total-qty + v-subtot-qty
                   v-total-prep = v-total-prep + v-subtot-prep.
        END.
        CASE ipcPriceOrQty:
          WHEN "Price" THEN
            dInvTot = dInvTot + v-inv-total.
          WHEN "Qty" THEN
            dInvTot = dInvTot + v-total-qty.
          WHEN "Prep" THEN
            dInvTot = dInvTot + v-total-prep.
        END CASE.

        ASSIGN v-inv-total     = 0
               v-inv-freight   = 0
               v-subtot-lines  = 0
               v-total-qty     = 0
               v-subtot-qty    = 0
               v-total-prep   = 0
               v-subtot-prep  = 0
               v-save-x     = 0.
        FOR EACH ar-invl WHERE ar-invl.company EQ oe-boll.company
            AND ar-invl.bol-no EQ oe-boll.bol-no
            AND ar-invl.job-no EQ ipcJob
            AND ar-invl.job-no2 EQ ipcJobNo2
            NO-LOCK,
            FIRST ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no
              NO-LOCK.
            ASSIGN v-subtot-lines = v-subtot-lines + ar-invl.amt
                   v-subtot-qty   = v-subtot-qty   + ar-invl.qty
                   v-save-x       = ar-invl.x-no.

            /* Avoid doing a break by since assiging inv-total each time */
            v-inv-freight = IF NOT(ar-inv.freight EQ 0 OR NOT ar-inv.f-bill) THEN
                          ar-inv.freight 
                       ELSE 0.


        END.
        ASSIGN v-inv-total = v-subtot-lines  /* + /* 8/5/14 ar-inv.tax-amt + */ v-inv-freight */
                   .
        v-total-qty = v-total-qty + v-subtot-qty.
        IF v-save-x GT 0 THEN DO:
          FOR EACH ar-invl WHERE ar-invl.x-no EQ v-save-x
              AND ar-invl.prep-charge GT ""
              NO-LOCK,
              FIRST ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no
                NO-LOCK.

               v-subtot-prep  = v-subtot-prep  + ar-invl.amt.                                         
          END.
          ASSIGN v-total-prep = v-total-prep + v-subtot-prep.
        END.

        CASE ipcPriceOrQty:
          WHEN "Price" THEN
            dInvTot = dInvTot + v-inv-total.
          WHEN "Qty" THEN
            dInvTot = dInvTot + v-total-qty.
          WHEN "Prep" THEN
            dInvTot = dInvTot + v-total-prep.
        END CASE.


    END. /* first of bol-no */
END. /* each oe-ord */

RETURN dInvTot.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

