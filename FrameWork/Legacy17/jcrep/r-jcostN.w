&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&SCOPED-DEFINE WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL NO-UNDO.
DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_curr AS LOGICAL INITIAL YES NO-UNDO.
DEFINE STREAM excel.
DEFINE VARIABLE v-est-mAT-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-est-board-mat-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-est-other-mat-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-est-lab-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-est-foh-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-est-voh-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-act-mAT-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-act-board-mat-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-act-other-mat-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-act-lab-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-act-foh-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-act-voh-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-std-price AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-act-price AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-op-cost AS DECIMAL NO-UNDO.
/* gdm - 11170812 */
DEFINE VARIABLE v-gest-mAT-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-gest-lab-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-gest-foh-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-gest-voh-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-gact-mAT-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-gact-lab-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-gact-foh-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-gact-voh-cost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-gstd-price AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-gact-price AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-matvar AS DECIMAL FORMAT "->>>>>9.9" NO-UNDO.
DEFINE VARIABLE v-boardvar AS DECIMAL FORMAT "->>>>>9.9" NO-UNDO.
DEFINE VARIABLE v-othervar AS DECIMAL FORMAT "->>>>>9.9" NO-UNDO.
DEFINE VARIABLE v-labvar AS DECIMAL FORMAT "->>>>>9.9" NO-UNDO.
DEFINE VARIABLE v-fixvar AS DECIMAL FORMAT "->>>>>9.9" NO-UNDO.
DEFINE VARIABLE v-varvar AS DECIMAL FORMAT "->>>>>9.9" NO-UNDO.
DEFINE VARIABLE v-totvar AS DECIMAL FORMAT "->>>>>9.9" NO-UNDO.
DEFINE VARIABLE v-constn AS DECIMAL FORMAT "->>>>>9.9" NO-UNDO.
DEFINE VARIABLE v-conact AS DECIMAL FORMAT "->>>>>9.9" NO-UNDO.
DEFINE VARIABLE v-tot-cost AS DECIMAL NO-UNDO .
DEFINE VARIABLE v-mater AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-prep AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-labor AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-lab-m AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-comm AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-frate AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-total AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-sale AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-t-inv-qty AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-avg-prc AS DECIMAL FORMAT ">>>>>9.99" NO-UNDO.
/*var declarations for compiling - used in job-clsr.i*/
DEFINE VARIABLE tb_exclude_run_if_no_prod AS LOGICAL INIT NO NO-UNDO.

DEFINE VARIABLE tb_exclude_prep AS LOGICAL NO-UNDO.
{methods/defines/hndldefs.i}
{methods/prgsecdt.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}

ASSIGN 
 cocode = gcompany
 locode = gloc.

{jc/rep/job-cls.i NEW}

DO TRANSACTION:
   {sys/inc/tspost.i}
   {rm/msfcalc.i}
END.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE cTextListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength AS INTEGER NO-UNDO.
DEFINE VARIABLE cFieldType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.

ASSIGN cTextListToSelect = "JOB#,FG ITEM,ITEM DESCRIPTION,FG CATEGORY,CUSTOMER,CUSTOMER NAME," +       /*6*/
                           "QTY ORDERED,QTY PRODUCED,MAT. ACT COST,MAT STND COST,MAT VAR,MAT % VAR," + /*6*/
                           "LAB. ACT COST,LAB STND COST,LAB VAR,LAB % VAR," +                          /*4*/ 
                           "FIXED O/H ACT COST,FIXED O/H STND COST,FIXED O/H VAR,FIXED O/H % VAR," +   /*4*/ 
                           "VAR O/H ACT COST,VAR O/H STND COST,VAR O/H VAR,VAR O/H % VAR," +           /*4*/ 
                           "TOTAL ACT COST,TOTAL STND COST,TOTAL VAR,TOTAL VAR%," +                    /*4*/ 
                           "MAT USAGE,LABOR EFF,FIXED O/H EFF,VAR O/H EFF," +                          /*4*/ 
                           "SP STANDARD,SP ACTUAL,COS STAND,COS ACTUAL," +                             /*4*/ 
                           "CONT. STAND,CONT. ACTUAL,%CONT. STAND,%CONT ACTUAL,TOTAL VARIANCE," +      /*5*/ 
                           "QTY INVOICED,BOX SALES,PREP COST,TOTAL COST,LAB ACT COST STD CREW," +      /*5*/
                           "MAT & FARM ACT COST"


       cFieldListToSelect = "v-job-no,work-item.i-no,v-i-name,v-fgcat,work-item.cust-no,v-custname," + 
                            "v-qty-ord,v-qty-prod,v-act-mAT-cost,v-est-mAT-cost,v-var-mat-cost,v-var%-mat-cost," +
                                  /* v-var-mat-cost = v-est-mAT-cost - v-act-mAT-cost
                                     v-var%-mat-cost = ((v-est-mAT-cost - v-act-mAT-cost) / v-act-mAT-cost)  * 100) ELSE 0) */
                            "v-act-lab-cost,v-est-lab-cost,v-var-lab-cost,v-var%-lab-cost," +
                              /* v-est-lab-cost - v-act-lab-cost, (((v-est-lab-cost - v-act-lab-cost) / v-act-lab-cost) * 100) ELSE 0) */
                             "v-act-foh-cost,v-est-foh-cost,v-var-foh-cost,v-var%-foh-cost," +
                             "v-act-voh-cost,v-est-voh-cost,v-var-voh-cost,v-var%-voh-cost," +
                             "v-act-tot-cost,v-est-tot-cost,v-var-tot-cost,v-var%-tot-cost," +
                          /* (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost, 
                             (v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost),
                             ((v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost) -
                                (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)),
                             ((((v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost) -
                                   (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)) /
                                     (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)) * 100.00) ELSE 0) ,
                          */
                            "v-mat-usage,v-lab-eff,v-foh-eff,v-voh-eff," +
                          /*  (v-est-mAT-cost - v-act-mAT-cost),
                             (v-est-lab-cost - v-act-lab-cost),
                             (v-est-foh-cost - v-act-foh-cost),
                            (v-est-voh-cost - v-act-voh-cost),
                            */                         
    /*    '"' ((v-est-mAT-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost) -
            (v-act-mAT-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) '",'
    */        
                         "v-std-price,v-act-price,v-std-cost,v-act-cost," +
                 /* v-std-cost = (v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost) 
                    v-act-cost = (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost) */
                         "v-std-cont,v-act-cont,v-std-cont%,v-act-cont%,v-tot-cont," +
                         "qty-inv,box-sales,prep-cost,total-cost,lab-act-cost,mat-fram-act-cost" 
    /*    '"' (v-std-price - 
            (v-est-mAT-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost)) '",'
        '"' (v-act-price - /*Contribution */
            (v-act-mAT-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) '",'
        '"' (IF v-std-price NE 0 THEN ((v-std-price -
            (v-est-mAT-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost)) /
            v-std-price) * 100.00 ELSE 0) '",'
        '"' (IF v-act-price NE 0 THEN ((v-act-price -
            (v-act-mAT-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) /
            v-act-price) * 100.00 ELSE 0) '",'
        '"' ((v-est-mAT-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost) -
            (v-act-mAT-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) '",'  


         '"' STRING(v-gstd-price,">>>,>>>,>>9.99-") '",' 
         '"' STRING(v-gest-mAT-cost + v-gest-lab-cost + v-gest-foh-cost + v-gest-voh-cost,">>>,>>>,>>9.99-") '",'
         '"' STRING(v-gstd-price - (v-gest-mAT-cost + v-gest-lab-cost + v-gest-foh-cost + v-gest-voh-cost),">>>,>>>,>>9.99-" ) '",'
         '"' STRING((((v-gstd-price - (v-gest-mAT-cost + v-gest-lab-cost + v-gest-foh-cost + v-gest-voh-cost) ) 
                       / v-gstd-price) * 100.00),">>>,>>>,>>9.99-") '",'
         '"' STRING(v-gact-price,">>>,>>>,>>9.99-") '",' 
         '"' STRING(v-gact-mAT-cost + v-gact-lab-cost + v-gact-foh-cost + v-gact-voh-cost,">>>,>>>,>>9.99-") '",'
         '"' STRING(v-gstd-price - (v-gact-mAT-cost + v-gact-lab-cost + v-gact-foh-cost + v-gact-voh-cost),">>>,>>>,>>9.99-" ) '",'
         '"' STRING((((v-gstd-price - (v-gact-mAT-cost + v-gact-lab-cost + v-gact-foh-cost + v-gact-voh-cost) ) 
                    / v-gact-price) * 100.00),">>>,>>>,>>9.99-") '",'.

    */

       cFieldLength = "9,15,25,11,8,30," + "13,13,13,13,13,13," + "13,13,13,13," + "13,13,13,13," + "13,13,13,13," +
                      "13,13,13,13," + "13,13,13,13," + "13,13,13,13," + "13,13,13,13,13," + "13,13,13,13,21,19"
       cFieldType   = "c,c,c,c,c,c," + "i,i,i,i,i,i," + "i,i,i,i," + "i,i,i,i," + "i,i,i,i," + 
                      "i,i,i,i," + "i,i,i,i," + "i,i,i,i," + "i,i,i,i,i," + "i,i,i,i,i,i"
       .

        ASSIGN cTextListToDefault  =  "JOB#,QTY ORDERED,CUSTOMER,CUSTOMER NAME,FG CATEGORY,FG ITEM,ITEM DESCRIPTION," +       /*7*/   
                                      "QTY PRODUCED,MAT. ACT COST,MAT STND COST,MAT VAR,MAT % VAR," + /*5*/   
                                      "LAB. ACT COST,LAB STND COST,LAB VAR,LAB % VAR," +                          /*4*/   
                                      "FIXED O/H ACT COST,FIXED O/H STND COST,FIXED O/H VAR,FIXED O/H % VAR," +   /*4*/   
                                      "VAR O/H ACT COST,VAR O/H STND COST,VAR O/H VAR,VAR O/H % VAR," +           /*4*/   
                                      "TOTAL ACT COST,TOTAL STND COST,TOTAL VAR,TOTAL VAR%," +                    /*4*/   
                                      "MAT USAGE,LABOR EFF,FIXED O/H EFF,VAR O/H EFF," +                          /*4*/   
                                      "SP STANDARD,SP ACTUAL,COS STAND,COS ACTUAL," +                             /*4*/   
                                      "CONT. STAND,CONT. ACTUAL,%CONT. STAND,%CONT ACTUAL,TOTAL VARIANCE"         /*5*/   .

{sys/inc/ttRptSel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&SCOPED-DEFINE PROCEDURE-TYPE WINDOW
&SCOPED-DEFINE DB-AWARE NO

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&SCOPED-DEFINE FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&SCOPED-DEFINE ENABLED-OBJECTS RECT-6 RECT-7 begin_job-no begin_job-no2 ~
end_job-no end_job-no2 begin_clsdate end_clsdate begin_cust-no end_cust-no ~
begin_fgcat end_fgcat Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up ~
btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel 
&SCOPED-DEFINE DISPLAYED-OBJECTS begin_job-no begin_job-no2 begin_clsdate ~
end_clsdate begin_cust-no end_cust-no begin_fgcat end_fgcat sl_avail ~
sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getInvoiceTotal C-Win 
FUNCTION getInvoiceTotal RETURNS DECIMAL
  ( ipiOrder AS INTEGER, ipcJob AS CHARACTER, ipcJobNo2 AS INTEGER, ipcPriceOrQty AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

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

DEFINE VARIABLE begin_clsdate AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "From Close Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From Cust #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning InvDate" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_fgcat AS CHARACTER FORMAT "X(8)" 
     LABEL "From FG Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_clsdate AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To Close Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Cust #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending InvDate" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_fgcat AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To FG Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-jcost.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_qty AS CHARACTER FORMAT "X(256)":U INITIAL "Base Standard Cost On Which Qty?" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .95 NO-UNDO.

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
     SIZE 19 BY 5.71 NO-UNDO.

DEFINE VARIABLE rd_qty AS CHARACTER INITIAL "Produced" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Order", "Order",
"Produced", "Produced"
     SIZE 29 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 7.38.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 6.43 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 35 BY 6.43 NO-UNDO.

DEFINE VARIABLE exclude-billed-prep AS LOGICAL INITIAL NO  
     LABEL "Exclude Billed Prep" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES  
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_invoiced AS LOGICAL INITIAL NO  
     LABEL "Print Only Invoiced Jobs If yes, then" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL NO  
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_sep_board AS LOGICAL INITIAL NO  
     LABEL "Separate Board / Other Mat'l" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL YES  
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tgl_SumTot AS LOGICAL INITIAL NO  
     LABEL "Print Summary - Grand Total Only" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_job-no AT ROW 2.19 COLUMN 20.8 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 2.19 COLUMN 33 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 2.29 COLUMN 60.4 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 2.29 COLUMN 72.6 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_clsdate AT ROW 3.38 COLUMN 21 COLON-ALIGNED HELP
          "Enter Beginning Date" WIDGET-ID 2
     end_clsdate AT ROW 3.43 COLUMN 60.6 COLON-ALIGNED HELP
          "Enter Ending Date" WIDGET-ID 6
     lbl_qty AT ROW 3.62 COLUMN 97 COLON-ALIGNED NO-LABEL
     begin_cust-no AT ROW 4.57 COLUMN 21 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 4
     end_cust-no AT ROW 4.57 COLUMN 60.6 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 8
     rd_qty AT ROW 5.05 COLUMN 106 NO-LABEL
     begin_fgcat AT ROW 6 COLUMN 21 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 46
     end_fgcat AT ROW 6 COLUMN 59.6 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 48
     tgl_SumTot AT ROW 6.24 COLUMN 99 WIDGET-ID 10
     tb_sep_board AT ROW 7.43 COLUMN 103 WIDGET-ID 12
     tb_invoiced AT ROW 8.62 COLUMN 105
     begin_date AT ROW 9.81 COLUMN 115 COLON-ALIGNED
     sl_avail AT ROW 10.52 COLUMN 2 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 10.52 COLUMN 61.4 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 10.52 COLUMN 41 HELP
          "Default Selected Table to Tables to Audit" WIDGET-ID 56
     end_date AT ROW 10.76 COLUMN 115 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     Btn_Add AT ROW 11.70 COLUMN 41 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 12.98 COLUMN 41 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     exclude-billed-prep AT ROW 12.67 COLUMN 108 WIDGET-ID 14
     btn_Up AT ROW 14.26 COLUMN 41 WIDGET-ID 40
     btn_down AT ROW 15.54 COLUMN 41 WIDGET-ID 42
     rd-dest AT ROW 19.1 COLUMN 7 NO-LABEL
     lv-ornt AT ROW 19.33 COLUMN 33 NO-LABEL
     lines-per-page AT ROW 19.33 COLUMN 86 COLON-ALIGNED
     lv-font-no AT ROW 20.76 COLUMN 36 COLON-ALIGNED
     lv-font-name AT ROW 21.71 COLUMN 30 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 23.14 COLUMN 32
     tb_excel AT ROW 24.1 COLUMN 69 RIGHT-ALIGNED
     tb_runExcel AT ROW 24.1 COLUMN 93 RIGHT-ALIGNED
     fi_file AT ROW 25.05 COLUMN 47 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 27.24 COLUMN 20
     btn-cancel AT ROW 27.24 COLUMN 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 18.14 COLUMN 7
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COLUMN 5
          BGCOLOR 2 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 9.57 COLUMN 60 WIDGET-ID 44
     "Available Columns" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 9.57 COLUMN 2 WIDGET-ID 38
     RECT-6 AT ROW 17.43 COLUMN 3
     RECT-7 AT ROW 1 COLUMN 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COLUMN 1.6 ROW 1.24
         SIZE 135.4 BY 27.81.


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
         TITLE              = "Job Actual/Standard Cost"
         HEIGHT             = 28.05
         WIDTH              = 98
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
       begin_clsdate:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_date IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       begin_date:HIDDEN IN FRAME FRAME-A           = TRUE
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_fgcat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_clsdate:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_date IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       end_date:HIDDEN IN FRAME FRAME-A           = TRUE
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_fgcat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_job-no IN FRAME FRAME-A
   NO-DISPLAY                                                           */
ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_job-no2 IN FRAME FRAME-A
   NO-DISPLAY                                                           */
ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX exclude-billed-prep IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       exclude-billed-prep:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_qty IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lbl_qty:HIDDEN IN FRAME FRAME-A           = TRUE
       lbl_qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_qty".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rd_qty IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rd_qty:HIDDEN IN FRAME FRAME-A           = TRUE
       rd_qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_invoiced IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_invoiced:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_invoiced:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_sep_board IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_sep_board:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tgl_SumTot IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tgl_SumTot:HIDDEN IN FRAME FRAME-A           = TRUE
       tgl_SumTot:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&SCOPED-DEFINE SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Job Actual/Standard Cost */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Job Actual/Standard Cost */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_clsdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_clsdate C-Win
ON LEAVE OF begin_clsdate IN FRAME FRAME-A /* From Close Date */
DO:
    ASSIGN {&self-name}.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* From Cust # */
DO:
   ASSIGN {&self-name}
       end_cust-no  = begin_cust-no:SCREEN-VALUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning InvDate */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_fgcat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_fgcat C-Win
ON LEAVE OF begin_fgcat IN FRAME FRAME-A /* From FG Category */
DO:
   ASSIGN {&self-name}
       end_cust-no  = begin_cust-no:SCREEN-VALUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
  assign 
     {&self-name}
     end_job-no  = begin_job-no:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
DO:
  ASSIGN 
     {&self-name}
     end_job-no2:SCREEN-VALUE = begin_job-no2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   APPLY "close" TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  ASSIGN 
      begin_job-no
      begin_job-no2
      end_job-no
      end_job-no2
      begin_cust-no
      end_cust-no
      begin_clsdate
      end_clsdate
      tgl_SumTot
      exclude-billed-prep
      tb_exclude_prep = exclude-billed-prep.

  RUN GetSelectionList.
  RUN run-report. 

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 5 THEN
       DO:
          DEFINE VARIABLE lv-tmp AS CHARACTER INITIAL "-0" NO-UNDO.

          {custom/asimailr.i &TYPE="Customer"
                             &begin_cust=lv-tmp
                             &END_cust=lv-tmp
                             &mail-subject=c-win:TITLE 
                             &mail-body=c-win:TITLE 
                             &mail-file=list-name }
       END.
  END CASE. 

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def C-Win
ON CHOOSE OF Btn_Def IN FRAME FRAME-A /* Default */
DO:
  DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

  RUN DisplaySelectionDefault.  /* task 04141411 */ 
  RUN DisplaySelectionList2 .

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
DO:
  RUN Move-Field ("Down").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove C-Win
ON CHOOSE OF Btn_Remove IN FRAME FRAME-A /* << Remove */
DO:
 /* DO i = sl_selected:NUM-ITEMS TO 1 BY -1 WITH FRAME {&FRAME-NAME}:
    IF sl_selected:IS-SELECTED(i) THEN
    ldummy = sl_selected:DELETE(i).
  END
  */
  APPLY "DEFAULT-ACTION" TO sl_selected  .
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
DO:
  RUN Move-Field ("Up").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_clsdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_clsdate C-Win
ON LEAVE OF end_clsdate IN FRAME FRAME-A /* To Close Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* To Cust # */
DO:
     ASSIGN {&self-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending InvDate */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_fgcat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_fgcat C-Win
ON LEAVE OF end_fgcat IN FRAME FRAME-A /* To FG Category */
DO:
     ASSIGN {&self-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val NE "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
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


&SCOPED-DEFINE SELF-NAME lv-ornt
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


&SCOPED-DEFINE SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME rd_qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_qty C-Win
ON VALUE-CHANGED OF rd_qty IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail C-Win
ON DEFAULT-ACTION OF sl_avail IN FRAME FRAME-A
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


&SCOPED-DEFINE SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected C-Win
ON DEFAULT-ACTION OF sl_selected IN FRAME FRAME-A
DO:
   DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN DO:
       ASSIGN ldummy = sl_Avail:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
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


&SCOPED-DEFINE SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tb_invoiced
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_invoiced C-Win
ON VALUE-CHANGED OF tb_invoiced IN FRAME FRAME-A /* Print Only Invoiced Jobs If yes, then */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tgl_SumTot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgl_SumTot C-Win
ON VALUE-CHANGED OF tgl_SumTot IN FRAME FRAME-A /* Print Summary - Grand Total Only */
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

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList C-Win 
PROCEDURE DisplaySelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) NE NUM-ENTRIES(cFieldListToSelect) THEN DO:   
     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +                   
                    (IF cListContents EQ "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .    
  END.

  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

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
  DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE cTmpList AS CHARACTER NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) NE NUM-ENTRIES(cFieldListToSelect) THEN DO:
    RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    (IF cListContents EQ "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect) .
  END.
  /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

  cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

   DO iCount = 1 TO sl_selected:NUM-ITEMS: /* task 08191414 */
       IF LOOKUP(ENTRY(iCount,cTmpList), cTextListToSelect) EQ 0 THEN
        ldummy = sl_selected:DELETE(ENTRY(iCount,cTmpList)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList C-Win 
PROCEDURE DisplaySelectionDefault :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

     cListContents = cListContents +                   
                    (IF cListContents EQ "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToDefault)   .
  END.            
  sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

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
  DISPLAY begin_job-no begin_job-no2 begin_clsdate end_clsdate begin_cust-no 
          end_cust-no begin_fgcat end_fgcat sl_avail sl_selected rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_job-no begin_job-no2 end_job-no end_job-no2 
         begin_clsdate end_clsdate begin_cust-no end_cust-no begin_fgcat 
         end_fgcat Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gather-data C-Win 
PROCEDURE gather-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE v-job-no  LIKE job.job-no  EXTENT 2 INITIAL ["", "zzzzzz"] NO-UNDO.
DEFINE VARIABLE v-job-no2 LIKE job.job-no2 EXTENT 2 INITIAL [00, 99]       NO-UNDO.

DEFINE VARIABLE ll AS LOGICAL NO-UNDO.

ASSIGN
    v-job-no[1] = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) +
                  TRIM(begin_job-no) + STRING(INTEGER(begin_job-no2),"99")
    v-job-no[2] = FILL(" ",6 - LENGTH(TRIM(end_job-no)))   +
                  TRIM(end_job-no)   + STRING(INTEGER(end_job-no2),"99"). 


EMPTY TEMP-TABLE tt-report.

    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ cocode
          AND job-hdr.opened  EQ NO
          AND job-hdr.job-no  GE SUBSTRING(v-job-no[1],1,6)
          AND job-hdr.job-no  LE SUBSTRING(v-job-no[2],1,6)
          AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
              TRIM(job-hdr.job-no) + STRING(INTEGER(job-hdr.job-no2),"99") GE v-job-no[1]
          AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no)))   +
              TRIM(job-hdr.job-no) + STRING(INTEGER(job-hdr.job-no2),"99") LE v-job-no[2]
          AND job-hdr.cust-no GE begin_cust-no 
          AND job-hdr.cust-no LE end_cust-no USE-INDEX opened:
        FIND FIRST job NO-LOCK
            WHERE job.company EQ job-hdr.company
              AND job.opened  EQ job-hdr.opened
              AND job.close-date GE begin_clsdate
              AND job.close-date LE END_clsdate
              AND job.job-no  EQ job-hdr.job-no
              AND job.job-no2 EQ job-hdr.job-no2
              AND job.job     EQ job-hdr.job NO-ERROR.
        IF NOT AVAILABLE job THEN NEXT.


        STATUS INPUT "  Processing Job#      "  + STRING(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2).

      ll = NOT tb_invoiced.

      IF NOT ll THEN
      FOR EACH oe-boll NO-LOCK
          WHERE oe-boll.company EQ job-hdr.company
            AND oe-boll.job-no  EQ job-hdr.job-no
            AND oe-boll.job-no2 EQ job-hdr.job-no2
            AND CAN-FIND(FIRST oe-bolh
                         WHERE oe-bolh.b-no   EQ oe-boll.b-no
                           AND oe-bolh.posted EQ YES),

          FIRST ar-invl NO-LOCK
          WHERE ar-invl.company EQ oe-boll.company
            AND ar-invl.b-no    EQ oe-boll.b-no
            AND ar-invl.ord-no  EQ oe-boll.ord-no
            AND ar-invl.i-no    EQ oe-boll.i-no
            AND ar-invl.po-no   EQ oe-boll.po-no
            AND CAN-FIND(FIRST ar-inv WHERE ar-inv.x-no     EQ ar-invl.x-no
                                        AND ar-inv.posted   EQ YES
                                        AND ar-inv.inv-date GE begin_date
                                        AND ar-inv.inv-date LE end_date):
        ll = YES.
        LEAVE.
      END.

      IF ll THEN DO:
        CREATE tt-report.
        ASSIGN
         tt-report.rec-id = RECID(job-hdr)
         tt-report.key-01 = FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
                            TRIM(job-hdr.job-no) +
                            STRING(INTEGER(job-hdr.job-no2),"99").
      END.
    END.



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
 DEFINE VARIABLE cTmpList AS CHARACTER NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
 iColumnLength = 0.

 DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
    FIND FIRST ttRptList NO-LOCK WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-ERROR.     

    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
           ttRptSelected.HeadingFromLeft = IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
           iColumnLength = iColumnLength + ttRptSelected.FieldLength + 1.
           .        

 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field C-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-detail-proc C-Win 
PROCEDURE output-detail-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
      v-matvar = (((v-est-mAT-cost - v-act-mAT-cost) / v-act-mAT-cost) * 100.00)
      v-labvar = (((v-est-lab-cost - v-act-lab-cost) / v-act-lab-cost) * 100.00)  
      v-fixvar = (((v-est-foh-cost - v-act-foh-cost) / v-act-foh-cost) * 100.00) 
      v-varvar = (((v-est-voh-cost - v-act-voh-cost) / v-act-voh-cost) * 100.00) 
      v-totvar = (((
                    (v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost) -
                    (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)
                   ) / (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)
                  ) * 100.00).

   IF v-matvar EQ ? THEN v-matvar = 0.
   IF v-labvar EQ ? THEN v-labvar = 0.
   IF v-fixvar EQ ? THEN v-fixvar = 0.
   IF v-varvar EQ ? THEN v-varvar = 0.
   IF v-totvar EQ ? THEN v-totvar = 0.

   DISPLAY SKIP(1)
       "MATERIAL" AT 37 "LABOR" AT 54 "FIXED O/H" AT 67
       "VARIABLE O/H" AT 80 "TOTAL" AT 104 
     SKIP
       "------------" AT 35
       "------------" AT 50
       "------------" AT 65
       "------------" AT 80
       "------------" AT 100 SKIP
       "ACTUAL COST" AT 15
       v-act-mAT-cost AT 32
       v-act-lab-cost AT 47
       v-act-foh-cost AT 62
       v-act-voh-cost AT 77
      (v-act-mAT-cost + v-act-lab-cost +
       v-act-foh-cost + v-act-voh-cost) AT 98 FORMAT ">>>,>>>,>>9.99-" 
     SKIP (1)
       "STANDARD COST" AT 15
       v-est-mAT-cost AT 32
       v-est-lab-cost AT 47
       v-est-foh-cost AT 62
       v-est-voh-cost AT 77
      (v-est-mAT-cost + v-est-lab-cost +
       v-est-foh-cost + v-est-voh-cost) AT 98 FORMAT ">>>,>>>,>>9.99-" 
     SKIP(1)
       "VARIANCES" AT 15
      (v-est-mAT-cost - v-act-mAT-cost) AT 33 FORMAT ">>>,>>>,>>9.99-"
      (v-est-lab-cost - v-act-lab-cost) AT 48 FORMAT ">>>,>>>,>>9.99-"
      (v-est-foh-cost - v-act-foh-cost) AT 63 FORMAT ">>>,>>>,>>9.99-"
      (v-est-voh-cost - v-act-voh-cost) AT 78 FORMAT ">>>,>>>,>>9.99-"
     ((v-est-mAT-cost + v-est-lab-cost +
       v-est-foh-cost + v-est-voh-cost) -
      (v-act-mAT-cost + v-act-lab-cost +
       v-act-foh-cost + v-act-voh-cost)) AT 98 FORMAT ">>>,>>>,>>9.99-"
     SKIP(1)
       "% VARIANCES" AT 15
       v-matvar AT 37
       v-labvar AT 52
       v-fixvar AT 67 
       v-varvar AT 82 
       v-totvar AT 102               
     WITH FRAME det NO-LABELS NO-BOX STREAM-IO WIDTH 132.

   ASSIGN
       v-constn = ((v-std-price - 
                   (v-est-mAT-cost + v-est-lab-cost + 
                    v-est-foh-cost + v-est-voh-cost)
                  ) / v-std-price) * 100.00 
      v-conact = ((v-act-price -
                   (v-act-mAT-cost + v-act-lab-cost + 
                    v-act-foh-cost + v-act-voh-cost)
                  ) / v-act-price) * 100.00 .

   IF v-constn EQ ? THEN ASSIGN v-constn = 0.
   IF v-conact EQ ? THEN ASSIGN v-conact = 0.

   DISPLAY SKIP(1)
       "      STANDARD                ACTUAL" AT 80 SKIP
       "P.P.V" AT 10
       "---------------        --------------" AT 80 SKIP
       "MATERIAL USAGE" AT 10
      (v-est-mAT-cost - v-act-mAT-cost) AT 33 FORMAT ">>>,>>>,>>9.99-"
       "SELLING PRICE" AT 60 v-std-price AT 80
       v-act-price AT 102 SKIP
       "LABOR EFFICIENCY" AT 10
      (v-est-lab-cost - v-act-lab-cost) AT 33 FORMAT ">>>,>>>,>>9.99-"
       "COST OF SALES" AT 60
      (v-est-mAT-cost + v-est-lab-cost +
       v-est-foh-cost + v-est-voh-cost) AT 81 FORMAT ">>>,>>>,>>9.99-"
      (v-act-mAT-cost + v-act-lab-cost +
       v-act-foh-cost + v-act-voh-cost) AT 103 FORMAT ">>>,>>>,>>9.99-" SKIP
       "FIXED O/H EFF" AT 10
      (v-est-foh-cost - v-act-foh-cost) AT 33 FORMAT ">>>,>>>,>>9.99-"
       "CONTRIBUTION" AT 60                
      (v-std-price -
      (v-est-mAT-cost + v-est-lab-cost +
       v-est-foh-cost + v-est-voh-cost)) AT 81 FORMAT ">>>,>>>,>>9.99-"
      (v-act-price -
      (v-act-mAT-cost + v-act-lab-cost +
       v-act-foh-cost + v-act-voh-cost)) AT 103 FORMAT ">>>,>>>,>>9.99-" SKIP
       "VARIABLE O/H EFF" AT 10
      (v-est-voh-cost - v-act-voh-cost) AT 33 FORMAT ">>>,>>>,>>9.99-" 
     SKIP
       "---------------" AT 32
       "% CONTRIBUTION" AT 60
       v-constn AT 83 
       v-conact AT 105               
     SKIP
       "TOTAL VARIANCE" AT 10
     ((v-est-mAT-cost + v-est-lab-cost +
       v-est-foh-cost + v-est-voh-cost) -
      (v-act-mAT-cost + v-act-lab-cost +
       v-act-foh-cost + v-act-voh-cost)) AT 33 FORMAT ">>>,>>>,>>9.99-"
     WITH FRAME dett STREAM-IO WIDTH 132 NO-BOX NO-LABELS.

   IF tb_excel THEN DO:
      PUT STREAM excel UNFORMATTED

        '"' v-act-mAT-cost '",'              
        '"' v-est-mAT-cost '",'  
        '"' (v-est-mAT-cost - v-act-mAT-cost) '",'
        '"' (IF v-act-mat-cost NE 0 THEN (((v-est-mAT-cost - v-act-mAT-cost) / v-act-mAT-cost)
            * 100) ELSE 0) '",'
        '"' v-act-lab-cost '",'
        '"' v-est-lab-cost '",'
        '"' (v-est-lab-cost - v-act-lab-cost) '",'
        '"' (IF v-act-lab-cost NE 0 THEN(((v-est-lab-cost - v-act-lab-cost) / v-act-lab-cost)
            * 100) ELSE 0) '",'    
        '"' v-act-foh-cost '",'              
        '"' v-est-foh-cost '",'  
        '"' (v-est-foh-cost - v-act-foh-cost) '",'
        '"' (IF v-act-foh-cost NE 0 THEN (((v-est-foh-cost - v-act-foh-cost) / v-act-foh-cost)
            * 100) ELSE 0) '",'
        '"' v-act-voh-cost '",'              
        '"' v-est-voh-cost '",'  
        '"' (v-est-voh-cost - v-act-voh-cost) '",'
        '"' (IF v-act-voh-cost NE 0 THEN (((v-est-voh-cost - v-act-voh-cost) / v-act-voh-cost)
            * 100) ELSE 0) '",'                
        '"' (v-act-mAT-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)  '",'  
        '"' (v-est-mAT-cost + v-est-lab-cost + 
            v-est-foh-cost + v-est-voh-cost)  '",'
        '"' ((v-est-mAT-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost) -
            (v-act-mAT-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) '",'
        '"' (IF v-act-mAT-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost NE 0 THEN ((((v-est-mAT-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost) -
            (v-act-mAT-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) /
            (v-act-mAT-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost))
            * 100.00) ELSE 0) '",'
        '"' (v-est-mAT-cost - v-act-mAT-cost) '",' 
        '"' (v-est-lab-cost - v-act-lab-cost) '",'  
        '"' (v-est-foh-cost - v-act-foh-cost) '",'
        '"' (v-est-voh-cost - v-act-voh-cost) '",'
        '"' ((v-est-mAT-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost) -
            (v-act-mAT-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) '",'
        '"' v-std-price '",'
        '"' v-act-price '",' 
        '"' (v-est-mAT-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost) '",'
        '"' (v-act-mAT-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost) '",'
        '"' (v-std-price - 
            (v-est-mAT-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost)) '",'
        '"' (v-act-price - /*Contribution */
            (v-act-mAT-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) '",'
        '"' (IF v-std-price NE 0 THEN ((v-std-price -
            (v-est-mAT-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost)) /
            v-std-price) * 100.00 ELSE 0) '",'
        '"' (IF v-act-price NE 0 THEN ((v-act-price -
            (v-act-mAT-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) /
            v-act-price) * 100.00 ELSE 0) '",'
        '"' ((v-est-mAT-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost) -
            (v-act-mAT-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) '",'  
         SKIP(1).
   END. /* IF tb_excel */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-detail-sep-proc C-Win 
PROCEDURE output-detail-sep-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
      v-boardvar = (((v-est-board-mat-cost - v-act-board-mat-cost) / v-act-board-mat-cost) * 100.00)
      v-othervar = (((v-est-other-mat-cost - v-act-other-mat-cost) / v-act-other-mat-cost) * 100.00)
      v-labvar = (((v-est-lab-cost - v-act-lab-cost) / v-act-lab-cost) * 100.00)  
      v-fixvar = (((v-est-foh-cost - v-act-foh-cost) / v-act-foh-cost) * 100.00) 
      v-varvar = (((v-est-voh-cost - v-act-voh-cost) / v-act-voh-cost) * 100.00) 
      v-totvar = (((
                    (v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost) -
                    (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)
                   ) / (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)
                  ) * 100.00).

   IF v-boardvar EQ ? THEN v-boardvar = 0.
   IF v-othervar EQ ? THEN v-othervar = 0.
   IF v-labvar EQ ? THEN v-labvar = 0.
   IF v-fixvar EQ ? THEN v-fixvar = 0.
   IF v-varvar EQ ? THEN v-varvar = 0.
   IF v-totvar EQ ? THEN v-totvar = 0.

   DISPLAY SKIP(1)
       "BOARD" AT 42 "OTHER" AT 57 "LABOR" AT 72 "FIXED O/H" AT 83
       "VARIABLE O/H" AT 95 "TOTAL" AT 117 
     SKIP
       "------------" AT 35
       "------------" AT 50
       "------------" AT 65
       "------------" AT 80
       "------------" AT 95
       "------------" AT 110 SKIP
       "ACTUAL COST" AT 15
       v-act-board-mat-cost AT 32
       v-act-other-mat-cost AT 47
       v-act-lab-cost AT 62
       v-act-foh-cost AT 77
       v-act-voh-cost AT 92
      (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
       v-act-foh-cost + v-act-voh-cost) AT 108 FORMAT ">>>,>>>,>>9.99-" 
     SKIP (1)
       "STANDARD COST" AT 15
       v-est-board-mat-cost AT 32
       v-est-other-mat-cost AT 47
       v-est-lab-cost AT 62
       v-est-foh-cost AT 77
       v-est-voh-cost AT 92
      (v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost +
       v-est-foh-cost + v-est-voh-cost) AT 108 FORMAT ">>>,>>>,>>9.99-" 
     SKIP(1)
       "VARIANCES" AT 15
      (v-est-board-mat-cost - v-act-board-mat-cost) AT 33 FORMAT ">>>,>>>,>>9.99-"
      (v-est-other-mat-cost - v-act-other-mat-cost) AT 48 FORMAT ">>>,>>>,>>9.99-"
      (v-est-lab-cost - v-act-lab-cost) AT 63 FORMAT ">>>,>>>,>>9.99-"
      (v-est-foh-cost - v-act-foh-cost) AT 78 FORMAT ">>>,>>>,>>9.99-"
      (v-est-voh-cost - v-act-voh-cost) AT 93 FORMAT ">>>,>>>,>>9.99-"
     ((v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost +
       v-est-foh-cost + v-est-voh-cost) -
      (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
       v-act-foh-cost + v-act-voh-cost)) AT 108 FORMAT ">>>,>>>,>>9.99-"
     SKIP(1)
       "% VARIANCES" AT 15
       v-boardvar AT 38
       v-othervar AT 53
       v-labvar AT 68
       v-fixvar AT 83 
       v-varvar AT 98
       v-totvar AT 113               
     WITH FRAME det-sep NO-LABELS NO-BOX STREAM-IO WIDTH 132.

   ASSIGN
      v-constn = ((v-std-price - 
                  (v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost + 
                   v-est-foh-cost + v-est-voh-cost)
                 ) / v-std-price) * 100.00 
      v-conact = ((v-act-price -
                  (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost + 
                   v-act-foh-cost + v-act-voh-cost)
                  ) / v-act-price) * 100.00.

   IF v-constn EQ ? THEN ASSIGN v-constn = 0.
   IF v-conact EQ ? THEN ASSIGN v-conact = 0.

   DISPLAY SKIP(1)
       "      STANDARD                ACTUAL" AT 80 SKIP
       "P.P.V" AT 10
       "---------------        --------------" AT 80 SKIP
       "BOARD USAGE" AT 10
      (v-est-board-mat-cost - v-act-board-mat-cost) AT 33 FORMAT ">>>,>>>,>>9.99-"
       "SELLING PRICE" AT 60 v-std-price AT 80
       v-act-price AT 102 SKIP
       "OTHER USAGE" AT 10
       (v-est-other-mat-cost - v-act-other-mat-cost) AT 33 FORMAT ">>>,>>>,>>9.99-"
       "COST OF SALES" AT 60
      (v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost +
       v-est-foh-cost + v-est-voh-cost) AT 81 FORMAT ">>>,>>>,>>9.99-"
      (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
       v-act-foh-cost + v-act-voh-cost) AT 103 FORMAT ">>>,>>>,>>9.99-" SKIP
       "LABOR EFFICIENCY" AT 10
      (v-est-lab-cost - v-act-lab-cost) AT 33 FORMAT ">>>,>>>,>>9.99-"
       "CONTRIBUTION" AT 60                
      (v-std-price -
      (v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost +
       v-est-foh-cost + v-est-voh-cost)) AT 81 FORMAT ">>>,>>>,>>9.99-"
      (v-act-price -
      (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
       v-act-foh-cost + v-act-voh-cost)) AT 103 FORMAT ">>>,>>>,>>9.99-" SKIP
       "FIXED O/H EFF" AT 10
      (v-est-foh-cost - v-act-foh-cost) AT 33 FORMAT ">>>,>>>,>>9.99-" SKIP
       "VARIABLE O/H EFF" AT 10
      (v-est-voh-cost - v-act-voh-cost) AT 33 FORMAT ">>>,>>>,>>9.99-"
     SKIP
       "---------------" AT 32
       "% CONTRIBUTION" AT 60
       v-constn AT 83 
       v-conact AT 105               
     SKIP
       "TOTAL VARIANCE" AT 10
     ((v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost +
       v-est-foh-cost + v-est-voh-cost) -
      (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
       v-act-foh-cost + v-act-voh-cost)) AT 33 FORMAT ">>>,>>>,>>9.99-"
     WITH FRAME dett STREAM-IO WIDTH 132 NO-BOX NO-LABELS.

   IF tb_excel THEN DO:
      PUT STREAM excel UNFORMATTED
        '"' v-act-board-mat-cost '",'
        '"' v-act-other-mat-cost '",'
        '"' v-est-board-mat-cost '",'
        '"' v-est-other-mat-cost '",'
        '"' (v-est-board-mat-cost - v-act-board-mat-cost) '",'
        '"' (v-est-other-mat-cost - v-act-other-mat-cost) '",'
        '"' (IF v-act-board-mat-cost NE 0 THEN (((v-est-board-mat-cost - v-act-board-mat-cost) / v-act-board-mat-cost)
            * 100) ELSE 0) '",'
        '"' (IF v-act-other-mat-cost NE 0 THEN (((v-est-other-mat-cost - v-act-other-mat-cost) / v-act-other-mat-cost)
            * 100) ELSE 0) '",'
        '"' v-act-lab-cost '",'
        '"' v-est-lab-cost '",'
        '"' (v-est-lab-cost - v-act-lab-cost) '",'
        '"' (IF v-act-lab-cost NE 0 THEN (((v-est-lab-cost - v-act-lab-cost) / v-act-lab-cost)
            * 100) ELSE 0) '",'    
        '"' v-act-foh-cost '",'              
        '"' v-est-foh-cost '",'  
        '"' (v-est-foh-cost - v-act-foh-cost) '",'
        '"' (IF v-act-foh-cost NE 0 THEN (((v-est-foh-cost - v-act-foh-cost) / v-act-foh-cost)
            * 100) ELSE 0) '",'
        '"' v-act-voh-cost '",'              
        '"' v-est-voh-cost '",'  
        '"' (v-est-voh-cost - v-act-voh-cost) '",'
        '"' (IF v-act-voh-cost NE 0 THEN (((v-est-voh-cost - v-act-voh-cost) / v-act-voh-cost)
            * 100) ELSE 0) '",'                
        '"' (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)  '",'  
        '"' (v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost + 
            v-est-foh-cost + v-est-voh-cost)  '",'
        '"' ((v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost) -
            (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) '",'
        '"' (IF v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost NE 0 THEN ((((v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost) -
            (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) /
            (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost))
            * 100.00) ELSE 0) '",'
        '"' (v-est-board-mat-cost - v-act-board-mat-cost) '",'
        '"' (v-est-other-mat-cost - v-act-other-mat-cost) '",'
        '"' (v-est-lab-cost - v-act-lab-cost) '",'  
        '"' (v-est-foh-cost - v-act-foh-cost) '",'
        '"' (v-est-voh-cost - v-act-voh-cost) '",'
        '"' ((v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost) -
            (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) '",'
        '"' v-std-price '",'
        '"' v-act-price '",' 
        '"' (v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost) '",'
        '"' (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost) '",'
        '"' (v-std-price - 
            (v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost)) '",'
        '"' (v-act-price - /*Contribution */
            (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) '",'
        '"' (IF v-std-price NE 0 THEN ((v-std-price -
            (v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost)) /
            v-std-price) * 100.00 ELSE 0) '",'
        '"' (IF v-act-price NE 0 THEN ((v-act-price -
            (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) /
            v-act-price) * 100.00 ELSE 0) '",'
        '"' ((v-est-board-mat-cost + v-est-other-mat-cost + v-est-lab-cost +
            v-est-foh-cost + v-est-voh-cost) -
            (v-act-board-mat-cost + v-act-other-mat-cost + v-act-lab-cost +
            v-act-foh-cost + v-act-voh-cost)) '",'  
         SKIP(1).
   END. /* IF tb_excel */
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
 /*    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
    /*     CREATE-TEST-FILE*/
         SAVE-AS
         USE-FILENAME

         UPDATE OKpressed.

     IF NOT OKpressed THEN  RETURN NO-APPLY. */

{custom/out2file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-port C-Win 
PROCEDURE output-to-port :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN custom/d-print.w (list-name).

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
  RUN custom/prntproc.p (list-name,INTEGER(lv-font-no),lv-ornt).
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
  RUN scr-rpt.w (list-name,c-win:TITLE,INTEGER(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ jc/rep/wip-aud.p  8/94 gb */
/* WIP Job Audit Listing Report                                               */
/* ---------------------------------------------------------------------------*/
/*{sys/form/r-topw.f}*/
DEFINE VARIABLE cDisplay AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelDisplay AS CHARACTER NO-UNDO.
DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE cTmpField AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVarValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
DEFINE VARIABLE str-tit4 AS CHARACTER FORMAT "x(299)" NO-UNDO.
DEFINE VARIABLE str-tit5 AS CHARACTER FORMAT "x(299)" NO-UNDO.
DEFINE VARIABLE v-prep-cost AS DECIMAL NO-UNDO .
DEFINE VARIABLE v-prof AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-com LIKE eb.comm INITIAL 0 NO-UNDO.
DEFINE VARIABLE v-wt AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE v-rate AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE c1 AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE c2 AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE dFrt AS DECIMAL NO-UNDO.
DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.

{sys/form/r-top5DL3.f} 

cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

ASSIGN
 v-est-mAT-cost       = 0
 v-est-board-mat-cost = 0
 v-est-other-mat-cost = 0
 v-est-lab-cost       = 0
 v-est-foh-cost       = 0
 v-est-voh-cost       = 0
 v-act-mAT-cost       = 0
 v-act-board-mat-cost = 0
 v-act-other-mat-cost = 0
 v-act-lab-cost       = 0
 v-act-foh-cost       = 0
 v-act-voh-cost       = 0
 v-std-price          = 0
 v-act-price          = 0
 v-op-cost            = 0
 v-gest-mAT-cost      = 0
 v-gest-lab-cost      = 0
 v-gest-foh-cost      = 0
 v-gest-voh-cost      = 0
 v-gact-mAT-cost      = 0
 v-gact-lab-cost      = 0
 v-gact-foh-cost      = 0
 v-gact-voh-cost      = 0
 v-gstd-price         = 0
 v-gact-price         = 0
 v-constn             = 0
 v-conact             = 0.

DEFINE VARIABLE v-ip-basis-w LIKE job-mAT.basis-w NO-UNDO.
DEFINE VARIABLE v-ip-len LIKE job-mAT.len NO-UNDO.
DEFINE VARIABLE v-ip-wid LIKE job-mAT.wid NO-UNDO.
DEFINE VARIABLE v-ip-sc-uom LIKE job-mAT.sc-uom NO-UNDO.
DEFINE VARIABLE v-stAT LIKE job.stAT NO-UNDO.

DEFINE VARIABLE v-t-qty-ord AS INTEGER NO-UNDO.
DEFINE VARIABLE v-num-up AS INTEGER NO-UNDO.

DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.
DEFINE BUFFER bwork-item FOR work-item.

ASSIGN 
    str-tit2 = c-win:TITLE 
    str-tit2 = str-tit2 + 
               IF tgl_SumTot THEN " - Grand Total Only " ELSE ""
    {sys/inc/ctrtext.i str-tit2 112}.  

FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

   IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
   THEN ASSIGN str-tit4 = str-tit4 + ttRptSelected.TextList + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + "," .        
   ELSE 
   ASSIGN str-tit4 = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
             str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
          excelheader = excelHeader + ttRptSelected.TextList + ","
             .        
 END.
{sys/inc/print1.i}
{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   /*IF NOT tgl_SumTot THEN
   DO:
      IF NOT tb_sep_board THEN
         excelheader = "Job#,,Item Code,Description,Cust. #,Customer Name,Qty Ordered," +
                       "QTY Produced,MAT. Act Cost,MAT Stnd cost,MAT Var," +
                       "MAT % Var,Lab. Act Cost,Lab Stnd cost,Lab Var," +
                       "Lab % Var,Fixed O/H Act Cost,Fixed O/H Stnd cost," +
                       "Fixed O/H Var,Fixed O/H % Var,Var O/H Act Cost," +
                       "Var O/H Stnd cost,Var O/H Var,Var O/H % Var," +
                       "TotaL ACT Cost,Total Stnd Cost,Total Var,Total Var%," +
                       "MAT Usage,Labor Eff,Fixed O/H Eff,Var O/H Eff,Total Var," +
                       "SP Standard,SP Actual,COS Stand,COS Actual," +
                       "Cont. Stand,Cont. Actual,%Cont. Stand,%Cont Actual," +
                       "Total Variance".
      ELSE
         excelheader = "Job#,,Item Code,Description,Cust. #,Customer Name,Qty Ordered," +
                       "QTY Produced,Board Act Cost,Other Act Cost,Board Stnd Cost,Other Stnd Cost,Board Var," +
                       "Other Var,Board % Var,Other % Var,Lab. Act Cost,Lab Stnd cost,Lab Var," +
                       "Lab % Var,Fixed O/H Act Cost,Fixed O/H Stnd cost," +
                       "Fixed O/H Var,Fixed O/H % Var,Var O/H Act Cost," +
                       "Var O/H Stnd cost,Var O/H Var,Var O/H % Var," +
                       "TotaL ACT Cost,Total Stnd Cost,Total Var,Total Var%," +
                       "Board Usage,Other Usage,Labor Eff,Fixed O/H Eff,Var O/H Eff,Total Var," +
                       "SP Standard,SP Actual,COS Stand,COS Actual," +
                       "Cont. Stand,Cont. Actual,%Cont. Stand,%Cont Actual," +
                       "Total Variance".
   END.
   ELSE
      excelheader = "Stnd Selling Price,Stnd Cost of Sales,Stnd Contribution,Stnd % Contribution,"
                  + "Act Selling Price,Act Cost of Sales,Act Contribution,Act % Contribution,".

   */
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.  

SESSION:SET-WAIT-STATE ("general").

DISPLAY "" /*str-tit4 str-tit5*/ WITH FRAME r-top.

RUN gATher-dATa.

FOR EACH tt-report,
    FIRST job-hdr NO-LOCK
    WHERE RECID(job-hdr) EQ tt-report.rec-id,
    FIRST job NO-LOCK
    WHERE job.company EQ job-hdr.company
      AND job.job     EQ job-hdr.job
      AND job.job-no  EQ job-hdr.job-no
      AND job.job-no2 EQ job-hdr.job-no2
    BREAK BY tt-report.key-01:

    IF FIRST-OF(tt-report.key-01) THEN DO:
        v-std-price = 0.
        FOR EACH work-item:
            DELETE work-item.
        END.
    END.

    {jc/rep/job-clsh2.i}

    STATUS INPUT "  Processing............. Job # "  + 
                  STRING(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2).

    IF LAST-OF(tt-report.key-01) AND CAN-FIND(FIRST work-item) THEN DO:

     /*   IF NOT tgl_SumTot THEN
            PUT "   JOB #  ITEM CODE       DESCRIPTION            "
                "        CUSTOMER NAME"
                "                          QTY ORDERED   QTY PRODUCED" SKIP 
                FILL("-", 132) FORMAT "x(132)" SKIP.
     */
       /*
        FOR EACH work-item
            BREAK BY work-item.cust-no
                  BY work-item.i-no:
            ASSIGN cDisplay = ""
                  cTmpField = ""
                  cVarValue = ""
                  cExcelDisplay = ""
                  cExcelVarValue = ""
                  .
            FIND cust NO-LOCK 
                WHERE cust.company = cocode 
                  AND cust.cust-no = work-item.cust-no NO-ERROR.
            FIND itemfg NO-LOCK 
                WHERE itemfg.company = cocode 
                  AND itemfg.i-no    = work-item.i-no NO-ERROR.

        END. /* for each work-item */
        */
       /*   ====
         IF NOT tgl_SumTot THEN
              DISPLAY 
                job.job-no SPACE(0) "-" SPACE(0)
                job.job-no2
                work-item.i-no
                itemfg.i-name WHEN AVAIL itemfg
                work-item.cust-no
                cust.name WHEN AVAIL cust
                work-item.qty-ord "   "
                work-item.qty-prod
               WITH FRAME deth STREAM-IO WIDTH 132 NO-BOX NO-LABELS DOWN.

            IF tb_excel THEN DO:

              IF NOT tgl_SumTot THEN
              DO:
                  PUT STREAM excel UNFORMATTED
                  '"' job.job-no '",'              
                  '"' job.job-no2 '",'  
                  '"' work-item.i-no '",'
                  '"' (IF AVAIL itemfg THEN itemfg.i-name ELSE " ") '",'
                  '"' work-item.cust-no '",'
                  '"' (IF AVAIL cust THEN cust.NAME ELSE " ") '",'    
                  '"' work-item.qty-ord '",'
                  '"' work-item.qty-prod '",'.

                 IF NOT LAST(work-item.cust-no) THEN PUT STREAM excel UNFORMATTED SKIP.
              END.
            END. /* IF tb_excel */               
            */



        {jc/rep/job-clsm.i}             /*** Get the MATerial Costs ***/
        {jc/rep/job-clsr.i}             /*** Get the Routing Costs  ***/
        {jc/rep/job-clsp.i}             /*** Get the Prep/Misc Costs  ***/


        IF v-std-price    EQ ? THEN ASSIGN v-std-price    = 0.
        IF v-est-mAT-cost EQ ? THEN ASSIGN v-est-mAT-cost = 0.
        IF v-est-board-mat-cost EQ ? THEN ASSIGN v-est-board-mat-cost = 0.
        IF v-est-other-mat-cost EQ ? THEN ASSIGN v-est-other-mat-cost = 0.
        IF v-est-lab-cost EQ ? THEN ASSIGN v-est-lab-cost = 0.
        IF v-est-foh-cost EQ ? THEN ASSIGN v-est-foh-cost = 0.
        IF v-est-voh-cost EQ ? THEN ASSIGN v-est-voh-cost = 0.
        IF v-act-price    EQ ? THEN ASSIGN v-act-price    = 0.
        IF v-act-mAT-cost EQ ? THEN ASSIGN v-act-mAT-cost = 0.
        IF v-act-board-mat-cost EQ ? THEN ASSIGN v-act-board-mat-cost = 0.
        IF v-act-other-mat-cost EQ ? THEN ASSIGN v-act-other-mat-cost = 0.
        IF v-act-lab-cost EQ ? THEN ASSIGN v-act-lab-cost = 0.
        IF v-act-foh-cost EQ ? THEN ASSIGN v-act-foh-cost = 0.
        IF v-act-voh-cost EQ ? THEN ASSIGN v-act-voh-cost = 0.

        /* gdm - 11170812 */
        ASSIGN 
           v-gstd-price    = v-gstd-price    + v-std-price
           v-gest-mAT-cost = v-gest-mAT-cost + v-est-mAT-cost 
           v-gest-lab-cost = v-gest-lab-cost + v-est-lab-cost
           v-gest-foh-cost = v-gest-foh-cost + v-est-foh-cost 
           v-gest-voh-cost = v-gest-voh-cost + v-est-voh-cost     
           v-gact-price    = v-gact-price    + v-act-price
           v-gact-mAT-cost = v-gact-mAT-cost + v-act-mAT-cost
           v-gact-lab-cost = v-gact-lab-cost + v-act-lab-cost
           v-gact-foh-cost = v-gact-foh-cost + v-act-foh-cost
           v-gact-voh-cost = v-gact-voh-cost + v-act-voh-cost.

       find first work-item NO-LOCK WHERE work-item.cust-no EQ job-hdr.cust-no AND
                            work-item.i-no    EQ job-hdr.i-no  NO-ERROR.
       FIND cust NO-LOCK WHERE cust.company EQ cocode 
                  AND cust.cust-no EQ work-item.cust-no NO-ERROR.
       FIND itemfg NO-LOCK WHERE itemfg.company EQ cocode 
                           AND itemfg.i-no EQ work-item.i-no NO-ERROR.

       {jcrep/r-jcostN1.i}


      /*  IF NOT tgl_SumTot THEN DO:
           IF NOT tb_sep_board THEN
              RUN output-detail-proc.
           ELSE
              RUN output-detail-sep-proc.
        END. /* IF NOT tgl_SumTot */
      */  
    END. /* IF LAST-OF */
END. /* FOR EACH tt-report */
/*
IF tgl_SumTot THEN DO:
    PUT 
     SKIP (5)            
      "                         SELLING PRICE         COST OF SALES         CONTRIBUTION      % CONTRIBUTION " 
     SKIP
      "                         -------------         -------------       ---------------    --------------- "                
          SKIP.  

    PUT
      "    STANDARD       "       AT 1
      v-gstd-price                AT 24
      (v-gest-mAT-cost + v-gest-lab-cost + v-gest-foh-cost + v-gest-voh-cost) AT 47 FORMAT ">>>,>>>,>>9.99-"
      (v-gstd-price - (v-gest-mAT-cost + v-gest-lab-cost + v-gest-foh-cost + v-gest-voh-cost) ) AT 67 FORMAT ">>>,>>>,>>9.99-"
      ((
       (v-gstd-price - (v-gest-mAT-cost + v-gest-lab-cost + v-gest-foh-cost + v-gest-voh-cost) ) 
        / v-gstd-price) * 100.00) AT 85 FORMAT ">>>,>>>,>>9.99-" SKIP
      "     ACTUAL        "       AT 1 
      v-gact-price                AT 24  
      (v-gact-mAT-cost + v-gact-lab-cost + v-gact-foh-cost + v-gact-voh-cost) AT 47 FORMAT ">>>,>>>,>>9.99-" 
      (v-gstd-price - (v-gact-mAT-cost + v-gact-lab-cost + v-gact-foh-cost + v-gact-voh-cost) ) AT 67 FORMAT ">>>,>>>,>>9.99-"
      ((                                
      (v-gstd-price - (v-gact-mAT-cost + v-gact-lab-cost + v-gact-foh-cost + v-gact-voh-cost) ) 
      / v-gact-price) * 100.00) AT 85 FORMAT ">>>,>>>,>>9.99-".

    IF tb_excel THEN DO:
       PUT STREAM excel UNFORMATTED
         '"' STRING(v-gstd-price,">>>,>>>,>>9.99-") '",' 
         '"' STRING(v-gest-mAT-cost + v-gest-lab-cost + v-gest-foh-cost + v-gest-voh-cost,">>>,>>>,>>9.99-") '",'
         '"' STRING(v-gstd-price - (v-gest-mAT-cost + v-gest-lab-cost + v-gest-foh-cost + v-gest-voh-cost),">>>,>>>,>>9.99-" ) '",'
         '"' STRING((((v-gstd-price - (v-gest-mAT-cost + v-gest-lab-cost + v-gest-foh-cost + v-gest-voh-cost) ) 
                       / v-gstd-price) * 100.00),">>>,>>>,>>9.99-") '",'
         '"' STRING(v-gact-price,">>>,>>>,>>9.99-") '",' 
         '"' STRING(v-gact-mAT-cost + v-gact-lab-cost + v-gact-foh-cost + v-gact-voh-cost,">>>,>>>,>>9.99-") '",'
         '"' STRING(v-gstd-price - (v-gact-mAT-cost + v-gact-lab-cost + v-gact-foh-cost + v-gact-voh-cost),">>>,>>>,>>9.99-" ) '",'
         '"' STRING((((v-gstd-price - (v-gact-mAT-cost + v-gact-lab-cost + v-gact-foh-cost + v-gact-voh-cost) ) 
                    / v-gact-price) * 100.00),">>>,>>>,>>9.99-") '",'.
    END.



END. /* IF tgl_SumTot */
*/

IF tb_excel THEN DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_runExcel THEN OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

END PROCEDURE.
/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME   

/*
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-tot-cost C-Win 
PROCEDURE get-tot-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER v-recid  AS RECID.
DEFINE OUTPUT PARAMETER v-total AS DECIMAL NO-UNDO .

DEFINE var v-prof    as   dec format "->>,>>>,>>9.99" no-undo.
DEFINE var v-com     like eb.comm init 0              no-undo.
DEFINE var v-wt      as   dec init 0                  no-undo.
DEFINE var v-rate    as   dec init 0                  no-undo.
DEFINE VAR c1        AS   CHAR INIT ""                NO-UNDO.
DEFINE VAR c2        AS   CHAR INIT ""                NO-UNDO.
DEFINE VAR dFrt      AS   DEC                         NO-UNDO.
DEFINE VAR iOrder    AS   INT                         NO-UNDO.
DEFINE var v-mater   as dec format "->>,>>>,>>9.99" no-undo.
DEFINE var v-prep    as dec format "->>,>>>,>>9.99" no-undo.
DEFINE var v-labor   as dec format "->>,>>>,>>9.99" no-undo.
DEFINE var v-lab-m   as dec format "->>,>>>,>>9.99" no-undo.
DEFINE var v-comm    as dec format "->>,>>>,>>9.99" no-undo.
DEFINE var v-frate   as dec format "->>,>>>,>>9.99" no-undo.
/*DEFINE var v-total   as dec format "->>,>>>,>>9.99" no-undo.*/

{sys/inc/msfcalc.i}

if v-est then do:
  v-frate = 0.

  find job where recid(job) eq v-recid no-lock no-error.
  find first est  
      where est.company EQ job.company
        AND est.est-no  EQ job.est-no 
      no-lock no-error.

  if avail est then
  for each job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job     eq job.job
        and job-hdr.job-no  eq job.job-no
        and job-hdr.job-no2 eq job.job-no2
      no-lock,

      first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq job-hdr.i-no
      no-lock,

      first eb
      where eb.company    EQ job.company
        AND eb.est-no     EQ job.est-no
        and eb.form-no    EQ job-hdr.frm
        and (eb.blank-no  EQ job-hdr.blank-no OR
             est.est-type EQ 5                OR
             est.est-type EQ 6)
        and eb.chg-method EQ "P"
      no-lock,

      first work-item
      where work-item.form-no eq job-hdr.frm
        and work-item.i-no    eq job-hdr.i-no
      no-lock:  
      iOrder = 0.
      iOrder = INTEGER(job.job-no) NO-ERROR.
    FIND FIRST oe-boll WHERE oe-boll.company EQ job.company
        AND oe-boll.ord-no EQ iOrder
        AND oe-boll.i-no   EQ job-hdr.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL oe-boll OR iOrder EQ 0 THEN DO:
        /* get freight from boll's */

        FOR EACH oe-boll WHERE oe-boll.company EQ job.company
                           AND oe-boll.ord-no EQ iOrder
                           AND oe-boll.i-no   EQ job-hdr.i-no
            NO-LOCK,
            FIRST oe-bolh 
                    WHERE oe-bolh.company = oe-boll.company 
                      AND oe-bolh.b-no = oe-boll.b-no                      
                    NO-LOCK.

                v-frate = v-frate + oe-boll.freight.
        END.
    END.
    ELSE DO:
        /* Original Code */
        v-wt = itemfg.weight-100 * work-item.qty-prod / 100.

        if eb.fr-out-c ne 0 then
          v-frate = v-frate + (eb.fr-out-c * (v-wt / 100)).

        else
        if eb.fr-out-m ne 0 then
          v-frate = v-frate + (eb.fr-out-m * (work-item.qty-prod / 1000)).

        else do:
          v-rate = 0.

          release carr-mtx.

          find first carrier
              where carrier.company eq cocode
                and carrier.loc     eq locode
                and carrier.carrier eq eb.carrier
              no-lock no-error.

          if avail carrier then
          find first carr-mtx
              where carr-mtx.company  eq cocode
                and carr-mtx.loc      eq locode
                and carr-mtx.carrier  eq carrier.carrier
                and carr-mtx.del-zone eq eb.dest-code
              no-lock no-error.

          if avail carr-mtx then do:
            if carrier.by-pallet ne ? THEN DO:
              run util/ucarrier.p (recid(carrier)).
              FIND CURRENT carrier NO-LOCK NO-ERROR.
            END.

            if carrier.chg-method eq "P" then do:
              v-wt = work-item.qty-prod / (eb.cas-cnt * eb.cas-pal).

              {sys/inc/roundup.i v-wt}
            end.

            else
            if carrier.chg-method eq "M" then
              v-wt = (if v-corr then (work-item.qty-prod * itemfg.t-sqin * .007)
                                else (work-item.qty-prod * itemfg.t-sqin / 144)) /
                     1000.

            do i = 1 to 10:
              if carr-mtx.weight[i] ge v-wt then do:
                v-rate = carr-mtx.rate[i].
                leave.
              end.
            end.

            v-rate = v-rate * v-wt / (if carrier.chg-method eq "W" then 100 else 1).

            if v-rate lt carr-mtx.min-rate then v-rate = carr-mtx.min-rate.
          end. /* avail carr-mtx */

          IF v-rate NE ? THEN v-frate = v-frate + v-rate.
        end. /* eb.fr-out is zero */
    END. /* not avail an oe-boll */
  end. /* each job-hdr */

  find first eb
      where eb.company EQ job.company
        AND eb.est-no  EQ job.est-no
        and eb.form-no  ne 0
      no-lock no-error.
  if avail eb then v-com = eb.comm.
  v-comm  = v-sale * (v-com / 100).
end.

/***  Print Grand Totals and Profit ***/
assign
/* v-lab-m = v-labor * v-lab-mrk / 100*/
 v-total = /*v-mater + v-prep + v-labor + v-lab-m +*/ v-comm + v-frate .
/* v-prof  = (v-sale + v-misc-prep) - v-total.*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME */


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
  DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
  DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-label AS CHARACTER.

  lv-frame-hdl = FRAME {&FRAME-NAME}:HANDLE.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") GT 0
        THEN DO:
           IF lv-field-hdl:LABEL NE ? THEN 
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
       ENTRY(i,parm-lbl-list) ne "" THEN DO:

      lv-label = FILL(" ",34 - LENGTH(TRIM(ENTRY(i,parm-lbl-list)))) +
                 TRIM(ENTRY(i,parm-lbl-list)) + ":".

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*RETURN string(hField:BUFFER-VALUE, hField:FORMAT) */
  RETURN STRING(hipField:BUFFER-VALUE).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



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
FOR EACH oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
    AND oe-ord.ord-no EQ ipiOrder ,
    EACH oe-ordl OF oe-ord NO-LOCK,
    EACH oe-boll NO-LOCK WHERE oe-boll.company EQ oe-ordl.company
      AND oe-boll.ord-no EQ oe-ordl.ord-no

    BREAK BY oe-boll.bol-no:
    IF FIRST-OF(oe-boll.bol-no) THEN DO:
        ASSIGN v-inv-total    = 0
               v-inv-freight  = 0
               v-subtot-lines = 0
               v-subtot-qty   = 0
               v-total-qty    = 0
               v-total-prep   = 0
               v-subtot-prep  = 0.
        FOR EACH inv-head NO-LOCK WHERE inv-head.company EQ oe-boll.company
            AND inv-head.bol-no EQ oe-boll.bol-no :

            FOR EACH inv-line NO-LOCK WHERE inv-line.r-no EQ inv-head.r-no
              AND inv-line.job-no EQ ipcJob
              AND inv-line.job-no2 EQ ipcJobNo2:
                v-subtot-lines = v-subtot-lines + inv-line.t-price.
                v-subtot-qty   = v-subtot-qty   + inv-line.inv-qty .
                FOR EACH inv-misc NO-LOCK WHERE inv-misc.r-no EQ inv-line.r-no
                  AND inv-misc.LINE EQ inv-line.LINE :
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
               v-total-prep    = 0
               v-subtot-prep   = 0
               v-save-x        = 0.
        FOR EACH ar-invl NO-LOCK WHERE ar-invl.company EQ oe-boll.company
            AND ar-invl.bol-no EQ oe-boll.bol-no
            AND ar-invl.job-no EQ ipcJob
            AND ar-invl.job-no2 EQ ipcJobNo2,
            FIRST ar-inv NO-LOCK WHERE ar-inv.x-no EQ ar-invl.x-no  .
            ASSIGN v-subtot-lines = v-subtot-lines + ar-invl.amt
                   v-subtot-qty   = v-subtot-qty   + ar-invl.inv-qty
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
          FOR EACH ar-invl NO-LOCK WHERE ar-invl.x-no EQ v-save-x
              AND ar-invl.prep-charge GT "" ,
              FIRST ar-inv NO-LOCK WHERE ar-inv.x-no EQ ar-invl.x-no .

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

