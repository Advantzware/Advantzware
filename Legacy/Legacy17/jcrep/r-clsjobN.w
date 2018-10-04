&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jcrep\r-clsjob.w

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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR tb_curr AS LOG INIT YES NO-UNDO.
DEF STREAM excel.
DEF VAR v-est-mAT-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-est-board-mat-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-est-other-mat-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-est-lab-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-est-foh-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-est-voh-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-act-mAT-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-act-board-mat-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-act-other-mat-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-act-lab-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-act-foh-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-act-voh-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-std-price    AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-act-price    AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-op-cost      AS DEC NO-UNDO.
/* gdm - 11170812 */
DEF VAR v-gest-mAT-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-gest-lab-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-gest-foh-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-gest-voh-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-gact-mAT-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-gact-lab-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-gact-foh-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-gact-voh-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-gstd-price    AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-gact-price    AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-matvar        AS DEC FORMAT "->>>>>9.9" NO-UNDO.
DEF VAR v-boardvar      AS DEC FORMAT "->>>>>9.9" NO-UNDO.
DEF VAR v-othervar      AS DEC FORMAT "->>>>>9.9" NO-UNDO.
DEF VAR v-labvar        AS DEC FORMAT "->>>>>9.9" NO-UNDO.
DEF VAR v-fixvar        AS DEC FORMAT "->>>>>9.9" NO-UNDO.
DEF VAR v-varvar        AS DEC FORMAT "->>>>>9.9" NO-UNDO.
DEF VAR v-totvar        AS DEC FORMAT "->>>>>9.9" NO-UNDO.
DEF VAR v-constn        AS DEC FORMAT "->>>>>9.9" NO-UNDO.
DEF VAR v-conact        AS DEC FORMAT "->>>>>9.9" NO-UNDO.
/*var declarations for compiling - used in job-clsr.i*/
DEF VAR tb_exclude_run_if_no_prod AS LOG INIT NO NO-UNDO.

DEF VAR tb_exclude_prep AS LOG NO-UNDO.
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

{jc/rep/job-cls.i NEW}

DO TRANSACTION:
   {sys/inc/tspost.i}
END.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "Job#,Job#2,Item Code,Description,Cust. #,Customer Name,Qty Ordered,QTY Produced,MAT. Act Cost," +
                       "MAT Stnd cost,MAT Var,MAT %Var,Board Act Cost,Other Act Cost,Board Stnd Cost,Other Stnd Cost,Board Var," +
                       "Other Var,Board % Var,Other % Var,Lab. Act Cost,Lab Stnd cost,Lab Var," +
                       "Lab % Var,Fixed O/H Act Cost,Fixed O/H Stnd cost," +
                       "Fixed O/H Var,Fixed O/H % Var,Var O/H Act Cost," +
                       "Var O/H Stnd cost,Var O/H Var,Var O/H % Var," +
                       "TotaL ACT Cost,Total Stnd Cost,Total Var,Total Var%," +
                       "MAT Usage,Board Usage,Other Usage,Labor Eff,Fixed O/H Eff,Var O/H Eff,Total Var," +
                       "SP Standard,SP Actual,COS Stand,COS Actual," +
                       "Cont. Stand,Cont. Actual,%Cont. Stand,%Cont Actual,Total Variance" 

       cFieldListToSelect = "job,job2,ino,dscr,cust,cust-name,qty-ord,qty-pro,mat-act-cst," +
                                        "mat-stnd-cost,mat-var,mar-%-var,brd-act-cst,oth-act-cst,brd-stnd-cost,oth-stnd-cost,brd-var," +
                                        "oth-var,brd-%var,oth-%-var,lab-act-cst,lab-stnd-cost,lab-var," +
                                        "lab-%-var,fix-oh-act-cst,fix-oh-stnd-cost," +
                                        "fix-oh-var,fix-oh-%-var,var-oh-act-cost," + 
                                        "var-oh-stnd-cst,var-oh-var,var-oh-%-var," +             
                                        "ttl-act-cost,ttl-stnd-cst,ttl-var,ttl-%var," + 
                                        "mat-usg,brd-usg,oth-usg,lbr-eff,fix-oh-eff,var-on-eff,ttl-var," + 
                                        "sp-stnd,sp-act,cos-stnd,cos-act," + 
                                        "cnt-stnd,cnt-acl,%-cnt-stnd,%cnt-acl,ttl-varnc" 
       cFieldLength = "6,5,15,30,8,30,13,13,15," + "15,15,9,15,15,15,15,15,"  + "15,11,11,15,15,15," + "9,18,19,"  
                                + "15,15,16," + "17,15,13,"  + "15,15,15,10," + "15,15,15,15,15,15,15,"  + "15,15,15,15," + "15,15,15,15,15"  
       cFieldType = "c,c,c,c,c,c,i,i,i," + "i,i,i,i,i,i,i,i,"  + "i,i,i,i,i,i," + "i,i,i,"  + "i,i,i," + "i,i,i,"  + "i,i,i,i," + "i,i,i,i,i,i,i,"  + "i,i,i,i," + "i,i,i,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  =  "Job#,Job#2,Item Code,Description,Cust. #,Customer Name,Qty Ordered,QTY Produced"  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_job-no begin_job-no2 ~
end_job-no end_job-no2 begin_cust-no end_cust-no begin_clsdate end_clsdate ~
rd_qty tgl_SumTot tb_sep_board tb_invoiced begin_date end_date ~
exclude-billed-prep rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
tb_excel tb_runExcel fi_file btn-ok btn-cancel sl_avail Btn_Def sl_selected Btn_Add ~
Btn_Remove btn_Up btn_down
&Scoped-Define DISPLAYED-OBJECTS begin_job-no begin_job-no2 begin_cust-no ~
end_cust-no begin_clsdate end_clsdate lbl_qty rd_qty tgl_SumTot ~
tb_sep_board tb_invoiced begin_date end_date exclude-billed-prep rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel ~
tb_runExcel fi_file sl_avail sl_selected

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-cstshp.csv" 
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
     SIZE 94 BY 8.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.71.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE exclude-billed-prep AS LOGICAL INITIAL no 
     LABEL "Exclude Billed Prep" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_invoiced AS LOGICAL INITIAL no 
     LABEL "Print Only Invoiced Jobs     If yes, then" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_sep_board AS LOGICAL INITIAL no 
     LABEL "Separate Board / Other Mat'l" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tgl_SumTot AS LOGICAL INITIAL no 
     LABEL "Print Summary - Grand Total Only" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_job-no AT ROW 2.19 COL 20.8 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 2.19 COL 33 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 2.29 COL 60.4 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 2.29 COL 72.6 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_cust-no AT ROW 3.38 COL 20.8 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 4
     end_cust-no AT ROW 3.38 COL 60.4 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 8
     begin_clsdate AT ROW 4.52 COL 20.8 COLON-ALIGNED HELP
          "Enter Beginning Date" WIDGET-ID 2
     end_clsdate AT ROW 4.57 COL 60.4 COLON-ALIGNED HELP
          "Enter Ending Date" WIDGET-ID 6
     lbl_qty AT ROW 6.19 COL 11.8 COLON-ALIGNED NO-LABEL
     rd_qty AT ROW 6.19 COL 50.6 NO-LABEL
     tgl_SumTot AT ROW 7.76 COL 13.6 WIDGET-ID 10
     tb_sep_board AT ROW 7.76 COL 51.4 WIDGET-ID 12
     tb_invoiced AT ROW 9.24 COL 13.8
     begin_date AT ROW 9.24 COL 71.4 COLON-ALIGNED
     end_date AT ROW 10.19 COL 71.4 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     exclude-billed-prep AT ROW 10.52 COL 14 WIDGET-ID 14
     sl_avail AT ROW 12.62 COL 4.2 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 12.62 COL 40.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 12.62 COL 59.6 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 13.62 COL 40.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 14.62 COL 40.2 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 15.67 COL 40.2 WIDGET-ID 40
     btn_down AT ROW 16.67 COL 40.2 WIDGET-ID 42
     rd-dest AT ROW 19.43 COL 5 NO-LABEL
     lv-ornt AT ROW 19.67 COL 31 NO-LABEL
     lines-per-page AT ROW 19.67 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 21.1 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 22.05 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 23.48 COL 30
     tb_excel AT ROW 24.43 COL 67 RIGHT-ALIGNED
     tb_runExcel AT ROW 24.43 COL 91 RIGHT-ALIGNED
     fi_file AT ROW 25.38 COL 45 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 27.57 COL 18
     btn-cancel AT ROW 27.57 COL 56
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11.91 COL 5 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 18.48 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.91 COL 59.6 WIDGET-ID 44
     RECT-6 AT ROW 18.14 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 28.05.


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
         TITLE              = "Closed Job Analysis"
         HEIGHT             = 28.38
         WIDTH              = 96
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
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
       begin_clsdate:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
       end_clsdate:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
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

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_qty IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_qty".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_invoiced:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tgl_SumTot:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Closed Job Analysis */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Closed Job Analysis */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_clsdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_clsdate C-Win
ON LEAVE OF begin_clsdate IN FRAME FRAME-A /* From Close Date */
DO:
    ASSIGN {&self-name}.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* From Cust # */
DO:
   ASSIGN {&self-name}
       end_cust-no  = begin_cust-no:SCREEN-VALUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning InvDate */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
  assign 
     {&self-name}
     end_job-no  = begin_job-no:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
DO:
  assign 
     {&self-name}
     end_job-no2:SCREEN-VALUE = begin_job-no2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   apply "close" to this-procedure.
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
  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       WHEN 5 THEN
       DO:
          DEF VAR lv-tmp AS CHAR INIT "-0" NO-UNDO.

          {custom/asimailr.i &TYPE="Customer"
                             &begin_cust=lv-tmp
                             &END_cust=lv-tmp
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
       END.
  end case. 

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def C-Win
ON CHOOSE OF Btn_Def IN FRAME FRAME-A /* Default */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

  RUN DisplaySelectionDefault.  /* task 04041406 */ 
  RUN DisplaySelectionList2 .

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
DO:
  RUN Move-Field ("Down").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
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


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
DO:
  RUN Move-Field ("Up").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME end_clsdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_clsdate C-Win
ON LEAVE OF end_clsdate IN FRAME FRAME-A /* To Close Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* To Cust # */
DO:
     ASSIGN {&self-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending InvDate */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_qty C-Win
ON VALUE-CHANGED OF rd_qty IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME sl_avail
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


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected C-Win
ON DEFAULT-ACTION OF sl_selected IN FRAME FRAME-A
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_invoiced
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_invoiced C-Win
ON VALUE-CHANGED OF tb_invoiced IN FRAME FRAME-A /* Print Only Invoiced Jobs     If yes, then */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgl_SumTot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgl_SumTot C-Win
ON VALUE-CHANGED OF tgl_SumTot IN FRAME FRAME-A /* Print Summary - Grand Total Only */
DO:
  ASSIGN {&self-name}.

  IF {&self-name}:SCREEN-VALUE EQ "YES" THEN DO:
      ASSIGN sl_avail:SENSITIVE = FALSE
           Btn_Def:SENSITIVE = FALSE
           sl_selected:SENSITIVE = FALSE
           Btn_Add:SENSITIVE = FALSE
           Btn_Remove:SENSITIVE = FALSE
           btn_Up:SENSITIVE = FALSE
            btn_down:SENSITIVE = FALSE .
  END.
  ELSE DO:
      ASSIGN sl_avail:SENSITIVE = TRUE
           Btn_Def:SENSITIVE = TRUE
           sl_selected:SENSITIVE = TRUE
           Btn_Add:SENSITIVE = TRUE
           Btn_Remove:SENSITIVE = TRUE
           btn_Up:SENSITIVE = TRUE
            btn_down:SENSITIVE = TRUE .
  END.


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
    {custom/usrprint.i}
    RUN DisplaySelectionList2 .
    IF tgl_SumTot:SCREEN-VALUE EQ "YES" THEN DO:
      ASSIGN sl_avail:SENSITIVE = FALSE
           Btn_Def:SENSITIVE = FALSE
           sl_selected:SENSITIVE = FALSE
           Btn_Add:SENSITIVE = FALSE
           Btn_Remove:SENSITIVE = FALSE
           btn_Up:SENSITIVE = FALSE
            btn_down:SENSITIVE = FALSE .
  END.
  ELSE DO:
      ASSIGN sl_avail:SENSITIVE = TRUE
           Btn_Def:SENSITIVE = TRUE
           sl_selected:SENSITIVE = TRUE
           Btn_Add:SENSITIVE = TRUE
           Btn_Remove:SENSITIVE = TRUE
           btn_Up:SENSITIVE = TRUE
            btn_down:SENSITIVE = TRUE .
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault C-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList C-Win 
PROCEDURE DisplaySelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 C-Win 
PROCEDURE DisplaySelectionList2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.
  DEF VAR cTmpList AS cha NO-UNDO.

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
  DISPLAY begin_job-no begin_job-no2 begin_cust-no end_cust-no begin_clsdate 
          end_clsdate lbl_qty rd_qty tgl_SumTot tb_sep_board tb_invoiced 
          begin_date end_date exclude-billed-prep rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file sl_avail sl_selected
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_job-no begin_job-no2 end_job-no end_job-no2 
         begin_cust-no end_cust-no begin_clsdate end_clsdate rd_qty tgl_SumTot 
         tb_sep_board tb_invoiced begin_date end_date exclude-billed-prep 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel sl_avail Btn_Def sl_selected Btn_Add Btn_Remove
         btn_Up btn_down
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
DEF VAR v-job-no  LIKE job.job-no  EXTENT 2 INIT ["", "zzzzzz"] NO-UNDO.
DEF VAR v-job-no2 LIKE job.job-no2 EXTENT 2 INIT [00, 99]       NO-UNDO.

DEF VAR ll AS LOG NO-UNDO.

ASSIGN
    v-job-no[1] = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) +
                  TRIM(begin_job-no) + STRING(INT(begin_job-no2),"99")
    v-job-no[2] = FILL(" ",6 - LENGTH(TRIM(end_job-no)))   +
                  TRIM(end_job-no)   + STRING(INT(end_job-no2),"99"). 


EMPTY TEMP-TABLE tt-report.

    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ cocode
          AND job-hdr.opened  EQ NO
          AND job-hdr.job-no  GE SUBSTR(v-job-no[1],1,6)
          AND job-hdr.job-no  LE SUBSTR(v-job-no[2],1,6)
          AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
              TRIM(job-hdr.job-no) + STRING(INT(job-hdr.job-no2),"99") GE v-job-no[1]
          AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no)))   +
              TRIM(job-hdr.job-no) + STRING(INT(job-hdr.job-no2),"99") LE v-job-no[2]
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
        IF NOT AVAIL job THEN NEXT.


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
                            STRING(INT(job-hdr.job-no2),"99").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList C-Win 
PROCEDURE GetSelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR cTmpList AS cha NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
 iColumnLength = 0.

 DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     

    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
           ttRptSelected.HeadingFromLeft = IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ jc/rep/wip-aud.p  8/94 gb */
/* WIP Job Audit Listing Report                                               */
/* ---------------------------------------------------------------------------*/

/*{sys/form/r-topw.f}*/

ASSIGN
 v-est-mAT-cost = 0
 v-est-board-mat-cost = 0
 v-est-other-mat-cost = 0
 v-est-lab-cost = 0
 v-est-foh-cost = 0
 v-est-voh-cost = 0
 v-act-mAT-cost = 0
 v-act-board-mat-cost = 0
 v-act-other-mat-cost = 0
 v-act-lab-cost = 0
 v-act-foh-cost = 0
 v-act-voh-cost = 0
 v-std-price    = 0
 v-act-price    = 0
 v-op-cost     = 0
 v-gest-mAT-cost = 0
 v-gest-lab-cost = 0
 v-gest-foh-cost = 0
 v-gest-voh-cost = 0
 v-gact-mAT-cost = 0
 v-gact-lab-cost = 0
 v-gact-foh-cost = 0
 v-gact-voh-cost = 0
 v-gstd-price    = 0
 v-gact-price    = 0
 v-constn        = 0
 v-conact        = 0.

DEF VAR v-ip-basis-w   LIKE job-mAT.basis-w NO-UNDO.
DEF VAR v-ip-len       LIKE job-mAT.len     NO-UNDO.
DEF VAR v-ip-wid       LIKE job-mAT.wid     NO-UNDO.
DEF VAR v-ip-sc-uom    LIKE job-mAT.sc-uom  NO-UNDO.
DEF VAR v-stAT         LIKE job.stAT        NO-UNDO.

DEF VAR v-t-qty-ord    AS INT NO-UNDO.
DEF VAR v-num-up       AS INT NO-UNDO.

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR str-tit4 AS cha FORM "x(2000)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(2000)" NO-UNDO.
DEF VAR str-line AS cha FORM "x(3000)" NO-UNDO.

{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF VAR excelheader AS CHARACTER  NO-UNDO.

ASSIGN 
    str-tit2 = c-win:TITLE 
    str-tit2 = str-tit2 + 
               IF tgl_SumTot THEN " - Grand Total Only " ELSE ""
    {sys/inc/ctrtext.i str-tit2 112}.  


DEF VAR cslist AS cha NO-UNDO.
IF NOT tgl_SumTot THEN
 FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

   IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
   THEN do: 
       ASSIGN str-tit4 = str-tit4 + ttRptSelected.TextList + " "
           str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " " .
       IF NOT tgl_SumTot THEN
           excelheader = excelHeader + ttRptSelected.TextList + "," .        
   END.
   ELSE do: 
   ASSIGN str-tit4 = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
          str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " " .
            IF NOT tgl_SumTot THEN
                ASSIGN excelheader = excelHeader + ttRptSelected.TextList + "," .        
   END.
          cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF td-show-parm THEN RUN show-param.

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
 /*  IF NOT tgl_SumTot THEN
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
   ELSE */
   IF tgl_SumTot THEN
       ASSIGN excelheader = "Stnd Selling Price,Stnd Cost of Sales,Stnd Contribution,Stnd % Contribution,"
                                            + "Act Selling Price,Act Cost of Sales,Act Contribution,Act % Contribution,".

   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.  

SESSION:SET-WAIT-STATE ("general").

DISPLAY "" WITH FRAME r-top.

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

    {jc/rep/job-clsh.i}

    STATUS INPUT "  Processing............. Job # "  + 
                  STRING(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2).

    IF LAST-OF(tt-report.key-01) AND CAN-FIND(FIRST work-item) THEN DO:

      /*  IF NOT tgl_SumTot THEN
            PUT "   JOB #  ITEM CODE       DESCRIPTION            "
                "        CUSTOMER NAME"
                "                          QTY ORDERED   QTY PRODUCED" SKIP 
                FILL("-", 132) FORMAT "x(132)" SKIP. */

        FOR EACH work-item
            BREAK BY work-item.cust-no
                  BY work-item.i-no:

            FIND cust NO-LOCK 
                WHERE cust.company = cocode 
                  AND cust.cust-no = work-item.cust-no NO-ERROR.

            FIND itemfg NO-LOCK 
                WHERE itemfg.company = cocode 
                  AND itemfg.i-no    = work-item.i-no NO-ERROR.

         /*   IF NOT tgl_SumTot THEN
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
            END. /* IF tb_excel */     */
        END. /* FOR EACH work-item */



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

      /*  IF NOT tgl_SumTot THEN DO:
           IF NOT tb_sep_board THEN
              RUN output-detail-proc.
           ELSE
              RUN output-detail-sep-proc.
        END. /* IF NOT tgl_SumTot */ */


        FOR EACH work-item
            BREAK BY work-item.cust-no
                  BY work-item.i-no:

            FIND cust NO-LOCK 
                WHERE cust.company = cocode 
                  AND cust.cust-no = work-item.cust-no NO-ERROR.

            FIND itemfg NO-LOCK 
                WHERE itemfg.company = cocode 
                  AND itemfg.i-no    = work-item.i-no NO-ERROR.

          IF NOT tgl_SumTot THEN do:
            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "job"                     THEN cVarValue = job.job-no .                                                                                                                                                                                                                                                  
                         WHEN "job2"                    THEN cVarValue = string(job.job-no2 ) .                                                                                                                                                                                                   
                         WHEN "ino"                     THEN cVarValue = work-item.i-no .                                                                                                                                                                                      
                         WHEN "dscr"                    THEN cVarValue = (IF AVAIL itemfg THEN itemfg.i-name ELSE " ") .                                                                                                      
                         WHEN "cust"                    THEN cVarValue = work-item.cust-no .                                                                                                                                                                                                      
                         WHEN "cust-name"               THEN cVarValue = (IF AVAIL cust THEN cust.NAME ELSE " ") .                                                                                                                                                                                
                         WHEN "qty-ord"                 THEN cVarValue = string(work-item.qty-ord,"->,>>>,>>9.99") .                                                                                                                                                                                                      
                         WHEN "qty-pro"                 THEN cVarValue = string(work-item.qty-prod,"->,>>>,>>9.99") .                                                                                                                                                                                 
                         WHEN "mat-act-cst"             THEN cVarValue = string(v-act-mAT-cost,"->>>,>>>,>>9.99") .                                                                                                                                                  
                         WHEN "mat-stnd-cost"           THEN cVarValue = STRING(v-est-mAT-cost,"->>>,>>>,>>9.99").                                                                                                                                                                                                                    
                         WHEN "mat-var"                 THEN cVarValue = STRING((v-est-mAT-cost - v-act-mAT-cost),"->>>,>>>,>>9.99") .                                                                                                                                                                                                                                    
                         WHEN "mar-%-var"               THEN cVarValue = IF v-act-mat-cost NE 0 THEN STRING((((v-est-mAT-cost - v-act-mAT-cost) / v-act-mAT-cost) * 100),"->>>>>9.9") ELSE "" .                                                                                                                                                    
                         WHEN "brd-act-cst"             THEN cVarValue = STRING(v-act-board-mat-cost,"->>>,>>>,>>9.99") .                                                                                                                                                                                   
                         WHEN "oth-act-cst"             THEN cVarValue = STRING(v-act-other-mat-cost,"->>>,>>>,>>9.99") .                                                                                                                                  
                         WHEN "brd-stnd-cost"           THEN cVarValue = STRING(v-est-board-mat-cost,"->>>,>>>,>>9.99") .                                                                                                                                                                                                      
                         WHEN "oth-stnd-cost"           THEN cVarValue = STRING(v-est-other-mat-cost,"->>>,>>>,>>9.99") .                                                                                                                                                                                                        
                         WHEN "brd-var"                 THEN cVarValue = STRING((v-est-board-mat-cost - v-act-board-mat-cost),"->>>,>>>,>>9.99") .                                                                                                                                                                                                        
                         WHEN "oth-var"                 THEN cVarValue = STRING((v-est-other-mat-cost - v-act-other-mat-cost),"->>>,>>>,>>9.99") .                                                                                                                                                                                   
                         WHEN "brd-%var"                THEN cVarValue = IF v-act-board-mat-cost NE 0 THEN STRING((((v-est-board-mat-cost - v-act-board-mat-cost) / v-act-board-mat-cost) * 100),"->>>>>9.9") ELSE "" .
                         WHEN "oth-%-var"               THEN cVarValue = IF v-act-other-mat-cost NE 0 THEN STRING((((v-est-other-mat-cost - v-act-other-mat-cost) / v-act-other-mat-cost) * 100),"->>>>>9.9") ELSE ""  .                                                                                                                                                                                                      
                         WHEN "lab-act-cst"             THEN cVarValue = STRING(v-act-lab-cost,"->>>,>>>,>>9.99") .                                                                                                     
                         WHEN "lab-stnd-cost"           THEN cVarValue = STRING(v-est-lab-cost,"->>>,>>>,>>9.99") .                                                                                                                                                                                    
                         WHEN "lab-var"                 THEN cVarValue = STRING((v-est-lab-cost - v-act-lab-cost),"->>>,>>>,>>9.99") .                                                        
                         WHEN "lab-%-var"               THEN cVarValue = IF v-act-lab-cost NE 0 THEN STRING((((v-est-lab-cost - v-act-lab-cost) / v-act-lab-cost) * 100),"->>>>>9.9") ELSE "" .                                                                                                                              
                         WHEN "fix-oh-act-cst"          THEN cVarValue = STRING(v-act-foh-cost,"->>,>>>,>>>,>>9.99") .                                                                                                                                                                                                        
                         WHEN "fix-oh-stnd-cost"        THEN cVarValue = STRING(v-est-foh-cost,"->>>,>>>,>>>,>>9.99") .                                                                                                                                                                                                                               
                         WHEN "fix-oh-var"              THEN cVarValue = STRING((v-est-foh-cost - v-act-foh-cost),"->>>,>>>,>>9.99") .                                                                                                                                                                                                                                 
                         WHEN "fix-oh-%-var"            THEN cVarValue = IF v-act-foh-cost NE 0 THEN STRING((((v-est-foh-cost - v-act-foh-cost) / v-act-foh-cost) * 100),"->>>>>9.9") ELSE ""  .                                                                                                                          
                         WHEN "var-oh-act-cost"         THEN cVarValue = STRING(v-act-voh-cost,"->>>,>>>,>>9.99") .                                                                                                                                                                                                                      
                         WHEN "var-oh-stnd-cst"         THEN cVarValue = STRING(v-est-voh-cost,"->>>,>>>,>>9.99") .                                                                                                                                                                                  
                         WHEN "var-oh-var"              THEN cVarValue = STRING((v-est-voh-cost - v-act-voh-cost),"->>>,>>>,>>9.99") .                                                                                                                                                                                                                                
                         WHEN "var-oh-%-var"            THEN cVarValue = IF v-act-voh-cost NE 0 THEN STRING((((v-est-voh-cost - v-act-voh-cost) / v-act-voh-cost) * 100),"->>>>>9.9") ELSE "" .                                                                                                                                           
                         WHEN "ttl-act-cost"            THEN cVarValue = STRING((v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost),"->>>,>>>,>>9.99") .                                                                                                                                            
                         WHEN "ttl-stnd-cst"                THEN cVarValue = STRING((v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost),"->>>,>>>,>>9.99")     .                                                                                                                                               
                         WHEN "ttl-var"                        THEN cVarValue = STRING(((v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost) - (v-act-mAT-cost + v-act-lab-cost +  v-act-foh-cost + v-act-voh-cost)),"->>>,>>>,>>9.99")  .                                                                                                                                     
                         WHEN "ttl-%var"                THEN cVarValue = IF v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost NE 0 THEN STRING(((((v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost) - (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)) / (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)) * 100.00),"->>>>>9.9") ELSE "".
                         WHEN "mat-usg"                 THEN cVarValue = STRING((v-est-mAT-cost - v-act-mAT-cost),"->>>,>>>,>>9.99") .
                         WHEN "brd-usg"                 THEN cVarValue = STRING((v-est-board-mat-cost - v-act-board-mat-cost),"->>>,>>>,>>9.99") .                                                                                                                                                                                                                       
                         WHEN "oth-usg"                 THEN cVarValue = STRING((v-est-other-mat-cost - v-act-other-mat-cost),"->>>,>>>,>>9.99") .                                                                                                                                                                                                                       
                         WHEN "lbr-eff"                  THEN cVarValue = STRING((v-est-lab-cost - v-act-lab-cost),"->>>,>>>,>>9.99") .                                                                                                                                                                                                                      
                         WHEN "fix-oh-eff"              THEN cVarValue = STRING((v-est-foh-cost - v-act-foh-cost),"->>>,>>>,>>9.99") .                                                                                                                                                                                                                   
                         WHEN "var-on-eff"              THEN cVarValue = STRING((v-est-voh-cost - v-act-voh-cost),"->>>,>>>,>>9.99") .                                                                                                                                                                                                                                
                         WHEN "ttl-var"                 THEN cVarValue = STRING(((v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost) - (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)),"->>>,>>>,>>9.99") .                                                                                                                          
                         WHEN "sp-stnd"                 THEN cVarValue = STRING(v-std-price,"->>>,>>>,>>9.99") .                                                                                                                                                                                                                          
                         WHEN "sp-act"                  THEN cVarValue = STRING(v-act-price,"->>>,>>>,>>9.99") .                                                                                                                                                                                                                                            
                         WHEN "cos-stnd"                THEN cVarValue = STRING((v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost),"->>>,>>>,>>9.99").                                                                                                                                                              
                         WHEN "cos-act"                 THEN cVarValue = STRING((v-act-mAT-cost + v-act-lab-cost +  v-act-foh-cost + v-act-voh-cost),"->>>,>>>,>>9.99") .                                                                                                                                       
                         WHEN "cnt-stnd"                THEN cVarValue = STRING((v-std-price - (v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost)),"->>>,>>>,>>9.99") .                                                                                                                                         
                         WHEN "cnt-acl"                 THEN cVarValue = STRING((v-act-price - (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)),"->>>,>>>,>>9.99").                                                                                                                                     
                         WHEN "%-cnt-stnd"              THEN cVarValue = IF v-std-price NE 0 THEN STRING((((v-std-price - (v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost)) / v-std-price) * 100.00),"->>>>>9.9") ELSE "".  
                         WHEN "%cnt-acl"                THEN cVarValue = IF v-act-price NE 0 THEN STRING((((v-act-price - (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)) / v-act-price) * 100.00),"->>>>>9.9") ELSE "" .
                         WHEN "ttl-varnc"               THEN cVarValue = STRING(((v-est-mAT-cost + v-est-lab-cost + v-est-foh-cost + v-est-voh-cost) - (v-act-mAT-cost + v-act-lab-cost + v-act-foh-cost + v-act-voh-cost)),"->>>,>>>,>>9.99").                                                                                                                                                                                                                                


                    END CASE.                                                                                                                                                                                                                                  

                    cExcelVarValue = cVarValue.                                                                                                                                                                                                         
                    cDisplay = cDisplay + cVarValue +                                                                                                                                                                                                                                                        
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).                                                                                                                                           
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".                                                                                                                                                                                                                      
            END.                                                                                                                                                                                                                                                                        

            PUT UNFORMATTED cDisplay skip(1) .                                                                                                                                                                                                                                                        
            IF tb_excel THEN DO:                                                                                                                                                                                                                                                                     
                 PUT STREAM excel UNFORMATTED                                                                                                                                                                                                                                                  
                       cExcelDisplay SKIP .                                                                                                                                                                                                                                                        
             END.     
          END. /* for each work-item */
             /*IF NOT LAST(work-item.cust-no) THEN PUT STREAM excel UNFORMATTED SKIP.                                                                                                                                                                                                              */
          END.
    END. /* IF LAST-OF */
END. /* FOR EACH tt-report */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-frame-hdl as handle no-undo.
  def var lv-group-hdl as handle no-undo.
  def var lv-field-hdl as handle no-undo.
  def var lv-field2-hdl as handle no-undo.
  def var parm-fld-list as cha no-undo.
  def var parm-lbl-list as cha no-undo.
  def var i as int no-undo.
  def var lv-label as cha.

  lv-frame-hdl = frame {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:first-child.
  lv-field-hdl = lv-group-hdl:first-child .

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," 
                     .
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     .
              lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name then do:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".
                  end.
                  lv-field2-hdl = lv-field2-hdl:next-sibling.                 
              end.       
           end.                 
        end.            
     lv-field-hdl = lv-field-hdl:next-sibling.   
  end.

  put space(28)
      "< Selection Parameters >"
      skip(1).

  do i = 1 to num-entries(parm-fld-list,","):
    if entry(i,parm-fld-list) ne "" or
       entry(i,parm-lbl-list) ne "" then do:

      lv-label = fill(" ",34 - length(trim(entry(i,parm-lbl-list)))) +
                 trim(entry(i,parm-lbl-list)) + ":".

      put lv-label format "x(35)" at 5
          space(1)
          trim(entry(i,parm-fld-list)) format "x(40)"
          skip.              
    end.
  end.

  put fill("-",80) format "x(80)" skip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*RETURN string(hField:BUFFER-VALUE, hField:FORMAT) */
  RETURN string(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

