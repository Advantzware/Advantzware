&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: rmrep\r-rmcost.w

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
DEFINE VARIABLE list-name AS cha       NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE BUFFER b-qty FOR reftable.

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

DEFINE VARIABLE fco            AS ch        NO-UNDO.
DEFINE VARIABLE tco            LIKE fco NO-UNDO.
DEFINE VARIABLE floc           LIKE ITEM.loc NO-UNDO.
DEFINE VARIABLE tloc           LIKE ITEM.loc NO-UNDO.
DEFINE VARIABLE fcat           AS ch        INITIAL "000000" NO-UNDO.
DEFINE VARIABLE tcat           LIKE fcat INITIAL "ZZZZZZ" NO-UNDO.
DEFINE VARIABLE fmat           AS ch        INITIAL "000000" NO-UNDO.
DEFINE VARIABLE tmat           LIKE fmat INITIAL "ZZZZZZ" NO-UNDO.
DEFINE VARIABLE doe            AS LOGICAL   INITIAL TRUE NO-UNDO.
DEFINE VARIABLE dor            AS LOGICAL   INITIAL TRUE NO-UNDO.
DEFINE VARIABLE detail         AS LOGICAL   INITIAL FALSE NO-UNDO.

DEFINE VARIABLE head5          AS ch        FORMAT "x(39)" NO-UNDO.
DEFINE VARIABLE head21         AS ch        FORMAT "x(76)" NO-UNDO.
DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.

DEFINE VARIABLE cDisplay       AS cha       NO-UNDO.
DEFINE VARIABLE cExcelDisplay  AS cha       NO-UNDO.
DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
DEFINE VARIABLE cTmpField      AS CHA       NO-UNDO.
DEFINE VARIABLE cVarValue      AS cha       NO-UNDO.
DEFINE VARIABLE cExcelVarValue AS cha       NO-UNDO.
DEFINE VARIABLE cSelectedList  AS cha       NO-UNDO.
DEFINE VARIABLE cFieldName     AS cha       NO-UNDO.
DEFINE VARIABLE str-tit4       AS cha       FORM "x(200)" NO-UNDO.
DEFINE VARIABLE str-tit5       AS cha       FORM "x(200)" NO-UNDO.
DEFINE VARIABLE str-line       AS cha       FORM "x(300)" NO-UNDO.

{sys/form/r-top5DL3.f} 


DEFINE BUFFER b-blank-vend-qty  FOR reftable.
DEFINE BUFFER b-blank-vend-cost FOR reftable.
DEFINE BUFFER bitem             FOR ITEM.


head21 =
    "            On Hand      On Order     Committed   Backordered     Available".
head5 = "===== Board/Paper Speed Reduction =====".

FORM
    " Vendor 1:" item.vend-no  "Item #:" item.vend-item
    "Auto Alloc.?" item.alloc   SKIP
    " Vendor 2:" item.vend2-no "Item #:" item.vend2-item
    "    Stocked?" item.stocked SKIP
    " Reorder Policy:" item.ord-policy
    "Purchased or Manf?" TO 65 item.pur-man SKIP
    " Reorder Level :" item.ord-level
    "Purchased UOM:" TO 65 item.pur-uom SKIP
    " Minimum Order :" item.ord-min
    "Lead Time (Days):" TO 65 item.lead-days SKIP
    " Maximum Order :" item.ord-max
    "Beg Balance :" TO 60 item.beg-bal SKIP
    "Beg Bal Date:" TO 60 item.beg-date
    "  Warehouse:" item.loc loc.dscr FORMAT "x(25)"
    "Last Count:" TO 60 item.last-count SKIP
    "        Bin:" item.loc-bin "Count Date:" TO 60 item.last-date  SKIP
    " Cycle Code:" item.cc-code SPACE(10) "  Cons. UOM:" item.cons-uom
    "Last Cost :" TO 60 item.last-cost  SKIP
    " Purch. Rpt Code:" item.pur-rcode "Prod.Code:" item.pic-code
    "Avg. Cost :" TO 60 item.avg-cost SKIP
    head21 SKIP
    " QTY:" item.q-onh item.q-ono item.q-comm item.q-back item.q-avail SKIP
    "****************" SKIP
    WITH FRAME item2 STREAM-IO WIDTH 80 OVERLAY NO-LABELS NO-UNDERLINE.

/*{sys/form/r-topw.f}*/

FIND FIRST ce-ctrl
    WHERE ce-ctrl.company EQ cocode
    AND ce-ctrl.loc EQ locode
    NO-LOCK NO-ERROR.

DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-e-i-v NO-UNDO
    FIELD vend-no  AS CHARACTER
    FIELD std-uom  AS CHARACTER
    FIELD run-qty  AS DECIMAL   DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL   DECIMALS 4 EXTENT 20
    FIELD roll-w   AS DECIMAL   DECIMALS 4 EXTENT 30.

DEFINE STREAM s-temp.

DEFINE VARIABLE v-export           AS LOGICAL.
DEFINE VARIABLE v-excel-hdr        AS CHARACTER.
DEFINE VARIABLE v-exp-name         AS CHARACTER FORMAT "x(40)" INIT "c:\tmp\r-rmcost.csv".
DEFINE VARIABLE v-acct-descr       AS CHARACTER.
DEFINE VARIABLE v-procat           AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-last-cost        LIKE item.last-cost NO-UNDO.
DEFINE VARIABLE v-avg-cost         LIKE item.avg-cost NO-UNDO.
DEFINE VARIABLE v-loc-descr        AS CHARACTER NO-UNDO.


DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS cha       NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha       NO-UNDO.
DEFINE VARIABLE cFieldLength       AS cha       NO-UNDO.
DEFINE VARIABLE cFieldType         AS cha       NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS cha       NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO .
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

/*"i-no,i-cod,name,mat-typ,desc,cst-ty,est-dsc,cat," +
                            "cal,sh-len,sh-wid,bas-wt,reg,rol-wid,shrnk," +
                            "dep1,per1,dep2,per2,dep3,per3,dep4,per4,dep5,per5,dep6,per6,dep7,per7,dep8,per8,"+
                            "dep9,per9,dep10,per10,vend1,vend1-i,aut-all,vend2,vend2-i," +
                            "stok,re-ord-pol,re-ord-lvl,ord-min,ord-max,pur-maf,pur-uom,ld-dy," +
                            "ware,bin,cyc-cd,cns-uom,pur-rpt,prod-cd,beg-bel,beg-dt,lst-cnt," +
                            "cnt-dt,lst-cst,avg-cst,oh,on-ord,comm,bk-ord,avail" */

ASSIGN 
    cTextListToSelect  = "Item No,Item Code,Name,Mat'l Type,Desc,Cost Type,Est.Desc,Category," +
                           "Caliper,Sheet Len,Sheet Wid,Basis Wt,Reg.#,Roll Wid,Shrink %," +
                           "Dept1,%1,Dept2,%2,Dept3,%3,Dept4,%4,Dept5,%5,Dept6,%6,Dept7,%7,Dept8,%8," +
                           "Dept9,%9,Dept10,%10,Vendor 1,Vend 1 Item #,Auto Alloc,Vendor 2,Vend 2 Item #," +
                           "Stocked,Re-Ord Pol,Re-Ord Level,Ord Min,Ord MAX,Pur/Manf,UOM,Lead Days," +
                           "Ware,Bin Loc,Cyc CD,Cons Uom,Pur Rpt Cd,Prod Cd,Beg Bal,Beg Date,last Cnt," +
                           "Cnt Date,Last Cost,Avg Cost,On Hand,On Order,Committed,Backordered,Available" 
    cFieldListToSelect = "item.i-no,item.i-code,item.i-name,item.mat-type,item.i-dscr,item.cost-type,item.est-dscr,item.procat," +
                            "item.cal,item.s-len,item.s-wid,item.basis-w,item.reg-no,item.r-wid,item.shrink," +
                            "dept-name[1],speed%[1],dept-name[2],speed%[2],dept-name[3],speed%[3],dept-name[4],speed%[4],dept-name[5],speed%[5],dept-name[6],speed%[6],dept-name[7],speed%[7],dept-name[8],speed%[8]," +
                            "dept-name[9],speed%[9],dept-name[10],speed%[10],item.vend-no,item.vend-item,item.alloc,item.vend2-no,item.vend2-item," +
                            "item.stocked,ord-policy,item.ord-level,item.ord-min,item.ord-max,pur-man,item.pur-uom,item.lead-days," +
                            "item.loc,item.loc-bin,item.cc-code,item.cons-uom,item.pur-rcode,item.pic-code,item.beg-bal,beg-date,item.last-count," +
                            "last-date,item.last-cost,item.avg-cost,item.q-onh,item.q-ono,item.q-comm,item.q-back,item.q-avail"
    cFieldLength       = "10,10,30,10,30,9,20,8," + "7,11,11,8,5,11,8," + "5,3,5,3,5,3,5,3,5,3,5,3,5,3,5,3," + "5,3,6,3,10,16,10,10,16,"
                    + "7,10,17,17,17,8,3,9," + "5,12,6,8,10,7,17,10,17," + "10,11,11,18,18,17,17,18"
    cFieldType         = "c,c,c,c,c,c,c,c," + "i,i,i,i,i,i,i," + "c,i,c,i,c,i,c,i,c,i,c,i,c,i,c,i," + "c,i,c,i,c,c,c,c,c," 
                  + "c,c,i,i,i,i,c,i," + "c,c,c,c,c,c,i,c,i," + "c,i,i,i,i,i,i,i"
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Category,Item No,Name,Caliper,Basis Wt,Last Cost,UOM,On Hand,On Order,Committed,Available"  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_procat end_procat ~
begin_vend end_vend begin_mat end_mat begin_whs end_whs tb_real tb_est ~
tb_detailed sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down ~
rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_procat end_procat begin_vend ~
end_vend begin_mat end_mat begin_whs end_whs tb_real tb_est tb_detailed ~
sl_avail sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.29.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.29.

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

DEFINE VARIABLE begin_mat AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Material Type" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whs AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_mat AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Material Type" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_whs AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)":U INITIAL "c:~\tmp~\r-rmcost.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 43 BY 1
     FGCOLOR 0  NO-UNDO.

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

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To Email", 5,
"To CSV", 3
     SIZE 18 BY 5.48 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 6.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 8.33.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Detail Real Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_est AS LOGICAL INITIAL yes 
     LABEL "Estimated Materials?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL no 
     LABEL "Open CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_real AS LOGICAL INITIAL yes 
     LABEL "Real Materials?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_procat AT ROW 2.24 COL 32 COLON-ALIGNED
     end_procat AT ROW 2.24 COL 70 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_vend AT ROW 3.29 COL 32 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend AT ROW 3.29 COL 70 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     begin_mat AT ROW 4.33 COL 32 COLON-ALIGNED HELP
          "Enter Beginning Material Number" WIDGET-ID 58
     end_mat AT ROW 4.33 COL 70 COLON-ALIGNED HELP
          "Enter Ending Material Number" WIDGET-ID 60
     begin_whs AT ROW 5.43 COL 32 COLON-ALIGNED HELP
          "Enter Beginng Warehouse" WIDGET-ID 62
     end_whs AT ROW 5.43 COL 70 COLON-ALIGNED HELP
          "Enter Ending Warehouse" WIDGET-ID 64
     tb_real AT ROW 6.71 COL 46
     tb_est AT ROW 7.67 COL 46
     tb_detailed AT ROW 8.62 COL 46
     sl_avail AT ROW 10.67 COL 4.2 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 10.67 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 10.67 COL 66.8 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 11.67 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 12.67 COL 44 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 13.71 COL 44 WIDGET-ID 40
     btn_down AT ROW 14.76 COL 44 WIDGET-ID 42
     lines-per-page AT ROW 16.24 COL 91.4 COLON-ALIGNED
     lv-ornt AT ROW 16.48 COL 40.4 NO-LABEL
     rd-dest AT ROW 17.19 COL 6.4 NO-LABEL
     lv-font-no AT ROW 17.43 COL 88.4 COLON-ALIGNED
     lv-font-name AT ROW 17.67 COL 34.4 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.1 COL 69.4
     fi_file AT ROW 21.48 COL 31 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 6
     tb_OpenCSV AT ROW 21.62 COL 76.4 WIDGET-ID 4
     tbAutoClose AT ROW 23.24 COL 33.2 WIDGET-ID 16
     btn-ok AT ROW 24.24 COL 32.8
     btn-cancel AT ROW 24.24 COL 61.4
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 9.86 COL 4.2 WIDGET-ID 38
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 9.86 COL 66.4 WIDGET-ID 44
     " Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 16.14 COL 6
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.14 COL 4.4
          BGCOLOR 15 
     RECT-6 AT ROW 16.48 COL 4.2
     RECT-7 AT ROW 1.48 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101.8 BY 25.1
         BGCOLOR 15 .


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
         TITLE              = "Raw Materials Cost"
         HEIGHT             = 25.1
         WIDTH              = 101.8
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
       begin_mat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_mat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lines-per-page:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-name:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-no:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-ornt:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_est:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_real:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       td-show-parm:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Raw Materials Cost */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Raw Materials Cost */
DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mat C-Win
ON LEAVE OF begin_mat IN FRAME FRAME-A /* Beginning Material Type */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat C-Win
ON LEAVE OF begin_procat IN FRAME FRAME-A /* Beginning Category */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whs C-Win
ON LEAVE OF begin_whs IN FRAME FRAME-A /* Beginning Warehouse */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
        DELETE PROCEDURE hdOutputProcs.
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.
        IF rd-dest EQ 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.

        SESSION:SET-WAIT-STATE ("general").

        RUN GetSelectionList.
        RUN run-report.
        STATUS DEFAULT "Processing Complete".
        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN 
                DO:
                    IF NOT tb_OpenCSV THEN 
                    DO:        
                        MESSAGE  "CSV file have been created." SKIP(1)
                            "~"OK~" to open CSV file?"
                            VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                            TITLE "" UPDATE lChoice AS LOGICAL.
               
                        IF lChoice THEN
                        DO:
                            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)). 
                        END.
                    END.
                    ELSE DO:
                        OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                    END.
                END. /* WHEN 3 THEN DO: */
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &type= 'Raw Materials Cost'
                            &begin_cust= "begin_procat"
                            &END_cust= "end_procat" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = "Raw Materials Cost"
                             &begin_cust= "begin_procat"
                             &END_cust= "end_procat"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Raw Materials Cost"
                                  &begin_cust="end_procat"
                                  &END_cust="begin_procat"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
                    END.
                END.
            WHEN 6 THEN RUN output-to-port.

        END CASE. 
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
        SESSION:SET-WAIT-STATE ("").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def C-Win
ON CHOOSE OF Btn_Def IN FRAME FRAME-A /* Default */
DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
DO:
        RUN Move-Field ("Down").
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
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mat C-Win
ON LEAVE OF end_mat IN FRAME FRAME-A /* Ending Material Type */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_procat C-Win
ON LEAVE OF end_procat IN FRAME FRAME-A /* Ending Category */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whs C-Win
ON LEAVE OF end_whs IN FRAME FRAME-A /* Ending Warehouse */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Name */
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
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE        = ENTRY(1,char-val)
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
        RUN pChangeDest.
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


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detail Real Items? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_est C-Win
ON VALUE-CHANGED OF tb_est IN FRAME FRAME-A /* Estimated Materials? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_real
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_real C-Win
ON VALUE-CHANGED OF tb_real IN FRAME FRAME-A /* Real Materials? */
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
ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    /* security check need {methods/prgsecur.i} in definition section */
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    Btn_Def:load-image("Graphics/32x32/default.png").
    Btn_Add:load-image("Graphics/32x32/additem.png").
    Btn_Remove:load-image("Graphics/32x32/remove.png").
    btn_Up:load-image("Graphics/32x32/moveup.png").
    btn_down:load-image("Graphics/32x32/movedown.png").  
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "MR1" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i} 
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_procat.
    END.
    RUN pChangeDest.
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
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.

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

    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 C-Win 
PROCEDURE DisplaySelectionList2 :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.
  
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
  DISPLAY begin_procat end_procat begin_vend end_vend begin_mat end_mat 
          begin_whs end_whs tb_real tb_est tb_detailed sl_avail sl_selected 
          rd-dest fi_file tb_OpenCSV tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_procat end_procat begin_vend end_vend begin_mat 
         end_mat begin_whs end_whs tb_real tb_est tb_detailed sl_avail Btn_Def 
         sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file 
         tb_OpenCSV tbAutoClose btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE est-board C-Win 
PROCEDURE est-board :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    DEFINE VARIABLE i AS INTEGER NO-UNDO.

    RUN print-estimated (ip-rowid).

    FOR EACH tt-e-i-v BREAK BY tt-e-i-v.vend-no:
        IF rd-dest EQ 3 THEN
            PUT STREAM s-temp 
                '"' tt-e-i-v.vend-no '",'
                '"' tt-e-i-v.std-uom '",'.

        DO i = 1 TO 20:
            IF i = 1 THEN 
            DO:
                /* PUT SPACE(3) "Vendor: " tt-e-i-v.vend-no
                     SPACE(1) "Cost--> Up To " tt-e-i-v.run-qty[1] FORMAT ">,>>>,>>9.9<<" TO 45
                     SPACE(1) tt-e-i-v.std-uom to 49
                     SPACE(1) "$" tt-e-i-v.run-cost[1] FORMAT ">>>,>>9.9999"
                     SPACE(3). */

                IF rd-dest EQ 3 THEN 
                    PUT STREAM s-temp 
                        '"' STRING(tt-e-i-v.run-qty[1],">>>>>>9.9<<")     '",'
                        '"' "$" STRING(tt-e-i-v.run-cost[1],">>>>>9.9999") '",'.

                DO j = 1 TO 5:
                    /*  IF tt-e-i-v.roll-w[j] NE 0 THEN
                         PUT tt-e-i-v.roll-w[j] SPACE(3). */

                    IF rd-dest EQ 3 THEN 
                        PUT STREAM s-temp 
                            '"' STRING(tt-e-i-v.roll-w[j],">>9.9999") '",'.
                END.
                IF rd-dest EQ 3 THEN
                    PUT SKIP.
            END.

            ELSE 
                IF tt-e-i-v.run-qty[i] NE 0 THEN 
                DO:

                    /*  PUT tt-e-i-v.run-qty[i] FORMAT ">,>>>,>>9.9<<" TO 45
                          SPACE(5) "$" tt-e-i-v.run-cost[i] FORMAT ">>>,>>9.9999" " " AT 66. */

                    IF rd-dest EQ 3 THEN 
                        PUT STREAM s-temp 
                            '"' STRING(tt-e-i-v.run-qty[i],">>>>>>9.9<<")     '",'
                            '"' "$" STRING(tt-e-i-v.run-cost[i],">>>>>9.9999") '",'.

                END.
                ELSE 
                DO:
                    /* PUT SPACE(66).*/
                    IF rd-dest EQ 3 THEN 
                        PUT STREAM s-temp ",,".
                END.

            IF i GT 1 AND i LT 7 THEN
            DO:
                DO j = 1 TO 5:
                    IF ((i - 1) * 5) + j LE 26 THEN
                    DO:
                        /* IF tt-e-i-v.roll-w[((i - 1) * 5) + j] ne 0 THEN
                            PUT tt-e-i-v.roll-w[((i - 1) * 5) + j] SPACE(3).*/

                        IF rd-dest EQ 3 THEN 
                            PUT STREAM s-temp '"' STRING(tt-e-i-v.roll-w[((i - 1) * 5) + j],">>9.9999") '",'.
                    END.
                END.
                IF rd-dest EQ 3 THEN
                    PUT SKIP.
            END.
        END.

        PUT SKIP.
        DOWN.
    END. 
    IF rd-dest EQ 3 THEN PUT STREAM s-temp SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE est-not-board C-Win 
PROCEDURE est-not-board :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    DEFINE VARIABLE i AS INTEGER NO-UNDO.

    RUN print-estimated (ip-rowid).

    FOR EACH tt-e-i-v BREAK BY tt-e-i-v.vend-no:

        IF rd-dest EQ 3 THEN 
            PUT STREAM s-temp 
                '"' tt-e-i-v.vend-no '",'
                '"' tt-e-i-v.std-uom '",'.

        DO i = 1 TO 20:
            IF i EQ 1 THEN 
            DO:
                PUT SPACE(3) "Vendor: " tt-e-i-v.vend-no
                    SPACE(1) "Cost--> Up To " tt-e-i-v.run-qty[1] FORMAT ">,>>>,>>9.9<<" TO 45
                    SPACE(1) tt-e-i-v.std-uom TO 49
                    SPACE(1) "$" tt-e-i-v.run-cost[1] FORMAT ">>>,>>9.9999"
                    SPACE(4) SKIP.

                IF rd-dest EQ 3 THEN 
                    PUT STREAM s-temp 
                        '"' STRING(tt-e-i-v.run-qty[1],">,>>>,>>9.9<<")  '",'
                        '"' "$" STRING(tt-e-i-v.run-cost[1],">>>>>9.9999") '",'.
            END.

            ELSE 
                IF tt-e-i-v.run-qty[i] NE 0 THEN
                DO:
                    PUT tt-e-i-v.run-qty[i] FORMAT ">,>>>,>>9.9<<" TO 45
                        SPACE(5) "$" tt-e-i-v.run-cost[i] FORMAT ">>>,>>9.9999" SKIP.

                    IF rd-dest EQ 3 THEN 
                        PUT STREAM s-temp 
                            '"' STRING(tt-e-i-v.run-qty[i],">,>>>,>>9.9<<")    '",'
                            '"' "$" STRING(tt-e-i-v.run-cost[i],">>>>>9.9999") '",'.
                END.
        END.
        PUT SKIP(1).
        DOWN.
    END.
    IF rd-dest EQ 3 THEN PUT STREAM s-temp SKIP.

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
        ASSIGN 
            ttRptSelected.TextList        = ENTRY(i,cTmpList)
            ttRptSelected.FieldList       = ttRptList.FieldList
            ttRptSelected.FieldLength     = int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
            ttRptSelected.DisplayOrder    = i
            ttRptSelected.HeadingFromLeft = IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
            iColumnLength                 = iColumnLength + ttRptSelected.FieldLength + 1.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file C-Win 
PROCEDURE output-to-file :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    {custom\out2file.i}

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
    RUN custom\d-print.w (list-name).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChangeDest C-Win 
PROCEDURE pChangeDest :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF rd-dest:SCREEN-VALUE EQ "3" THEN
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "Yes"
                fi_file:SENSITIVE       = YES
                tb_OpenCSV:SENSITIVE    = YES       
                .
        ELSE 
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file:SENSITIVE       = NO
                tb_OpenCSV:SENSITIVE    = NO       
                .
       
        ASSIGN 
            fi_file:SCREEN-VALUE = "c:\tmp\r-rmcost.csv".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-estimated C-Win 
PROCEDURE print-estimated :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    DEFINE VARIABLE v-index        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cReturnValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lNewTableBased AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE cItemTypeRM    AS CHARACTER NO-UNDO INITIAL "RM". /* Raw Materials */
    DEFINE VARIABLE iIndex1        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndex2        AS INTEGER   NO-UNDO.

    EMPTY TEMP-TABLE tt-e-i-v.

    FIND item WHERE ROWID(item) EQ ip-rowid NO-LOCK NO-ERROR.

    RELEASE e-item.

    RUN sys/ref/nk1look.p (
        INPUT  cocode,         /* Company       */
        INPUT  "vendItemCost", /* Sys-Ctrl Name */
        INPUT  "L",            /* Logical       */
        INPUT  NO,             /* Check by cust */
        INPUT  YES,            /* Use Cust      */
        INPUT  "",             /* Customer      */
        INPUT  "",             /* Ship-to       */
        OUTPUT cReturnValue,
        OUTPUT lRecFound
        ).
           
    IF lRecFound AND AVAILABLE(ITEM) THEN
        lNewTableBased = LOGICAL(cReturnValue).
    
    /* Use new vendItemCost and vendItemCostLevel tables */
    IF lNewTableBased THEN 
    DO:
        FOR EACH vendItemCost NO-LOCK
            WHERE vendItemCost.VendorID GE begin_vend
            AND vendItemCost.VendorID LE end_vend           
            AND vendItemCost.company  EQ item.company
            AND vendItemCost.itemID   EQ item.i-no
            AND venditemcost.itemType EQ cItemTypeRM:
       
            CREATE tt-e-i-v.
            ASSIGN 
                tt-e-i-v.vend-no = vendItemCost.vendorID
                tt-e-i-v.std-uom = vendItemCost.vendorUOM
                iIndex1          = 1
                .
       
            DO iIndex2 = 1 TO 26:
                tt-e-i-v.roll-w[iIndex2] = vendItemCost.validWidth[iIndex2].
            END.
        
            ASSIGN 
                tt-e-i-v.roll-w[27] = vendItemCost.dimWidthMinimum
                tt-e-i-v.roll-w[28] = vendItemCost.dimWidthMaximum 
                tt-e-i-v.roll-w[29] = vendItemCost.dimLengthMinimum
                tt-e-i-v.roll-w[30] = vendItemCost.dimLengthMaximum
                .

            FOR EACH vendItemCostLevel NO-LOCK
                WHERE vendItemCost.vendItemCostID EQ vendItemCostLevel.vendItemCostID:
                ASSIGN
                    tt-e-i-v.run-cost[iIndex1] = vendItemCostLevel.costPerUOM 
                    tt-e-i-v.run-qty[iIndex1]  = vendItemCostLevel.quantityBase 
                    iIndex1                    = iIndex1 + 1
                    .                                  
            END.   
        END.
    END.            
    ELSE 
    DO: /* use legacy e-item and e-item-vend tables */
        FIND FIRST e-item
            WHERE e-item.company EQ item.company
            AND e-item.i-no    EQ item.i-no
            NO-LOCK NO-ERROR.
         
        IF AVAILABLE e-item THEN 
        DO:
            FOR EACH e-item-vend OF e-item NO-LOCK 
                WHERE e-item-vend.item-type EQ YES
                AND e-item-vend.vend-no   GE begin_vend
                AND e-item-vend.vend-no   LE end_vend :
          
          
                CREATE tt-e-i-v.
                ASSIGN 
                    tt-e-i-v.vend-no = e-item-vend.vend-no
                    tt-e-i-v.std-uom = e-item.std-uom
                    .

                DO v-index = 1 TO 10:
                    ASSIGN
                        tt-e-i-v.run-cost[v-index] = e-item-vend.run-cost[v-index]
                        tt-e-i-v.run-qty[v-index]  = e-item-vend.run-qty[v-index]
                        tt-e-i-v.roll-w[v-index]   = e-item-vend.roll-w[v-index]
                        . 
                END.
        

                FIND FIRST b-qty WHERE
                    b-qty.reftable = "vend-qty" AND
                    b-qty.company  = e-item-vend.company AND
                    b-qty.CODE     = e-item-vend.i-no AND
                    b-qty.code2    = e-item-vend.vend-no
                    NO-LOCK NO-ERROR.

                IF AVAILABLE b-qty THEN 
                DO:
                    DO v-index = 1 TO 10:
                        ASSIGN
                            tt-e-i-v.run-qty[v-index + 10]  = e-item-vend.runQtyXtra[v-index]
                            tt-e-i-v.run-cost[v-index + 10] = e-item-vend.runCostXtra[v-index]
                            .
                    END.
                END.

                DO v-index = 11 TO 30:
                    tt-e-i-v.roll-w[v-index] = e-item-vend.roll-w[v-index].
                END.

                RELEASE tt-e-i-v.
            END.

            IF "" GE begin_vend AND "" LE end_vend  AND
                NOT CAN-FIND(FIRST tt-e-i-v WHERE vend-no EQ "") THEN 
            DO:
                CREATE tt-e-i-v.
                tt-e-i-v.std-uom = e-item.std-uom.

                DO v-index = 1 TO 10:
                    ASSIGN
                        tt-e-i-v.run-cost[v-index] = e-item.run-cost[v-index]
                        tt-e-i-v.run-qty[v-index]  = e-item.run-qty[v-index]
                        tt-e-i-v.roll-w[v-index]   = e-item.roll-w[v-index]
                        .
                END.

        
                DO v-index = 1 TO 10:
                    ASSIGN
                        tt-e-i-v.run-qty[v-index + 10]  = e-item.runQty[v-index]
                        tt-e-i-v.run-cost[v-index + 10] = e-item.runCost[v-index]  
                        .
                END.

                RELEASE tt-e-i-v.
            END.
        END. 
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-board C-Win 
PROCEDURE run-board :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rm-cst-amt LIKE item.last-cost.
    DEFINE VARIABLE v-printed  AS LOG INIT NO NO-UNDO.

    FORM
        SKIP
        "  Item No:" TO 10 item.i-no     " Item Code:" TO 60 item.i-code    SKIP
        "     Name:" TO 10 item.i-name   "Mat'l Type:" TO 60 item.mat-type  SKIP
        "     Desc:" TO 10 item.i-dscr   " Cost Type:" TO 60 item.cost-type SKIP
        " Est.Desc:" TO 10 item.est-dscr "  Category:" TO 60 item.procat    SKIP
        "  Caliper:" item.cal     TO 20 " Sheet Len:" TO 45 item.s-len SKIP
        " Basis Wt:" item.basis-w TO 20 " Sheet Wid:" TO 45 item.s-wid SKIP
        "    Reg.#:" item.reg-no  TO 20 "  Roll Wid:" TO 45 item.r-wid SKIP
        " Shrink %:" item.shrink  TO 20  SKIP
        SPACE(21) head5 SKIP
        "   Department Name: "
        SPACE(2) item.dept-name[1] SPACE(2) item.dept-name[2]
        SPACE(2) item.dept-name[3] SPACE(2) item.dept-name[4]
        SPACE(2) item.dept-name[5] SPACE(2) item.dept-name[6]
        SPACE(2) item.dept-name[7] SPACE(2) item.dept-name[8]
        SPACE(2) item.dept-name[9] SPACE(2) item.dept-name[10] 
        SKIP
        "       Reduction %: " item.speed%[1 FOR 10]
        SKIP
        WITH FRAME item STREAM-IO WIDTH 80 OVERLAY NO-LABELS NO-UNDERLINE.

    FORM
        item.procat
        item.i-no
        item.i-name    FORMAT "x(27)"
        item.cal
        item.basis-w
        item.last-cost
        item.cons-uom
        item.q-onh     FORMAT "->>>>,>>9.999"
        item.q-ono     FORMAT "->>>>,>>9.999"
        item.q-comm    FORMAT  ">>>>,>>9.999"
        item.q-avail   FORMAT "->>>>,>>9.999"
        SKIP
        HEADER
        "CAT   ITEM       DESCRIPTION                 CALIPER WEIGHT     COST     UOM       On Hand      On Order     Allocated     Available"
        WITH FRAME itemx NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 132.

    FORM
        item.procat item.i-no item.i-name item.cal item.basis-w

        HEADER
        "CAT   ITEM       DESCRIPTION                    CALIPER WEIGHT   <               Valid   Roll   Widths               >"
        WITH FRAME iteme NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 119.


    SESSION:SET-WAIT-STATE ("general").

    ASSIGN 
        str-tit2 = c-win:TITLE + " - Board/Paper List"
        {sys/inc/ctrtext.i str-tit2 112}.

    DISPLAY "" WITH FRAME r-top.
    /*      
    IF dor THEN DO:
       IF NOT detail THEN DO:
          IF rd-dest EQ 3 THEN
             PUT STREAM s-temp UNFORMATTED
                "CAT,ITEM,DESCRIPTION,CALIPER,WEIGHT,COST,UOM,On Hand,On Order,Allocated,Available"
                SKIP.
    
          FOR EACH ITEM NO-LOCK WHERE 
                   item.company = cocode 
               AND item.loc = locode 
               AND (item.i-code = "R") 
               AND LOOKUP(item.mat-type,"B,P") GT 0 
               AND item.procat >= fcat
               AND item.procat <= tcat
             BREAK BY item.company 
                   BY item.loc 
                   BY item.i-code
                   BY item.mat-type 
                   BY item.procat 
                   BY item.i-no WITH FRAME itemx:
    
             CLEAR FRAME itemx ALL NO-PAUSE.                 
             v-printed = YES.
    
             FIND FIRST rm-ctrl WHERE rm-ctrl.company = cocode NO-LOCK.
    
             IF rm-ctrl.avg-lst-cst = TRUE THEN  
                rm-cst-amt = item.avg-cost.
             ELSE
                rm-cst-amt = item.last-cost.
    
             DISPLAY 
                item.procat  WHEN FIRST-OF(item.procat)
                item.i-no
                item.i-name
                item.cal
                item.basis-w
                rm-cst-amt @ item.last-cost
                item.cons-uom
                item.q-onh
                item.q-ono
                item.q-comm
                item.q-avail.
    
             DOWN.
    
             IF rd-dest EQ 3 THEN DO:
                ASSIGN v-procat = IF FIRST-OF(item.procat) THEN item.procat ELSE "".
                PUT STREAM s-temp 
                   '"' v-procat      '",'
                   '"' item.i-no     '",'
                   '"' item.i-name   '",'
                   '"' item.cal      '",'
                   '"' item.basis-w  '",'
                   '"' rm-cst-amt    FORMAT ">>>>>9.9999"       '",'
                   '"' item.cons-uom '",'
                   '"' item.q-onh    FORMAT "->>>>>>>>9.9<<<<<" '",'
                   '"' item.q-ono    FORMAT "->>>>>>>>9.9<<<<<" '",'
                   '"' item.q-comm   FORMAT "->>>>>>>>9.9<<<<<" '",'
                   '"' item.q-avail  FORMAT "->>>>>>>>9.9<<<<<" '"'
                   SKIP. 
             END.
          END. /* end for for each */
    
       END. /* non detail ends here */
       ELSE DO:
    
          IF rd-dest EQ 3 THEN 
             PUT STREAM s-temp 
               "Item No,Item Code,Name,Mat'l Type,Desc,Cost Type,Est.Desc,Category,
                Caliper,Sheet Len,Sheet Wid,Basis Wt,Reg.#,Roll Wid,Shrink %,
                Dept , % ,Dept , % ,Dept , % ,Dept , % ,
                Dept , % ,Dept , % ,Dept , % ,Dept , % ,
                Dept , % ,Dept , % ,
                Vendor 1,Vend 1 Item #,Auto Alloc,Vendor 2,Vend 2 Item #,
                Stocked,Re-Ord Pol,Re-Ord Level,Ord Min,Ord MAX,Pur/Manf,
                Pur UOM,Lead Days,Warehouse,Bin Loc,Cyc CD,Cons Uom,Pur Rpt Cd,
                Prod Cd,Beg Bal,Beg Date,last Cnt,Cnt Date,Last Cost,Avg Cost,
                On Hand,On Order,Committed,Backordered,Available,
                Vend 1,UOM,
                Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 6,Run Cost 6,Est Roll W,Run Qty 7,Run Cost 7,Run Qty 8,Run Cost 8,
                Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12,
                Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16,
                Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20,
                Vend 2,UOM,
                Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 6,Run Cost 6,Est Roll W,Run Qty 7,Run Cost 7,Run Qty 8,Run Cost 8,
                Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12,
                Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16,
                Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20,
                Vend 3,UOM,
                Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 6,Run Cost 6,Est Roll W,Run Qty 7,Run Cost 7,Run Qty 8,Run Cost 8,
                Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12,
                Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16,
                Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20,
                Vend 4,UOM,
                Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 6,Run Cost 6,Est Roll W,Run Qty 7,Run Cost 7,Run Qty 8,Run Cost 8,
                Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12,
                Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16,
                Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20,
                Vend 5,UOM,
                Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W,
                Run Qty 6,Run Cost 6,Est Roll WRun Qty 7,Run Cost 7,Run Qty 8,Run Cost 8,
                Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12,
                Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16,
                Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20," 
                SKIP.
    
          FOR EACH ITEM NO-LOCK WHERE 
                   item.company = cocode 
               AND item.loc = locode  
               AND (item.i-code = "R") 
               AND LOOKUP(item.mat-type,"B,P") GT 0 
               AND item.procat >= fcat
               AND item.procat <= tcat
             BREAK BY item.company 
                   BY item.loc 
                   BY item.i-code
                   BY item.mat-type 
                   BY item.procat  
                   BY item.i-no WITH FRAME item:
    
             CLEAR FRAME item ALL NO-PAUSE.
             v-printed = YES.
    
             DISPLAY head5
                item.i-no
                item.i-name
                item.i-dscr
                item.est-dscr
                item.i-code
                item.mat-type
                item.cost-type
                item.procat
                head5
                item.cal     WHEN item.cal     NE 0
                item.basis-w WHEN item.basis-w NE 0
                item.reg-no  WHEN item.reg-no  NE ""
                item.shrink  WHEN item.shrink  NE 0
                item.s-len   WHEN item.s-len   NE 0
                item.s-wid   WHEN item.s-wid   NE 0
                item.r-wid   WHEN item.r-wid   NE 0
                item.dept-name[1 ]   WHEN item.dept-name[1 ] NE ""
                item.speed%[1 ]      WHEN item.speed[1 ]     NE 0
                item.dept-name[2 ]   WHEN item.dept-name[2 ] NE ""
                item.speed%[2 ]      WHEN item.speed[2 ]     NE 0
                item.dept-name[3 ]   WHEN item.dept-name[3 ] NE ""
                item.speed%[3 ]      WHEN item.speed[3 ]     NE 0
                item.dept-name[4 ]   WHEN item.dept-name[4 ] NE ""
                item.speed%[4 ]      WHEN item.speed[4 ]     NE 0
                item.dept-name[5 ]   WHEN item.dept-name[5 ] NE ""
                item.speed%[5 ]      WHEN item.speed[5 ]     NE 0
                item.dept-name[6 ]   WHEN item.dept-name[6]  NE ""
                item.speed%[6 ]      WHEN item.speed[6 ]     NE 0
                item.dept-name[7 ]   WHEN item.dept-name[7 ] NE ""
                item.speed%[7 ]      WHEN item.speed[7 ]     NE 0
                item.dept-name[8 ]   WHEN item.dept-name[8 ] NE ""
                item.speed%[8 ]      WHEN item.speed[8 ]     NE 0
                item.dept-name[9 ]   WHEN item.dept-name[9 ] NE ""
                item.speed%[9 ]      WHEN item.speed[9 ]     NE 0
                item.dept-name[10]   WHEN item.dept-name[10] NE ""
                item.speed%[10]      WHEN item.speed[10]     NE 0.
    
             IF rd-dest EQ 3 THEN DO:
                PUT STREAM s-temp
                   '"' item.i-no        '",'          
                   '"' item.i-code      '",'        
                   '"' item.i-name      '",'        
                   '"' item.mat-type    '",'        
                   '"' item.i-dscr      '",'        
                   '"' item.cost-type   '",'        
                   '"' item.est-dscr    '",'        
                   '"' item.procat      '",'        
                   '"' item.cal         '",'        
                   '"' item.s-len       '",'        
                   '"' item.s-wid       '",'        
                   '"' item.basis-w     '",'        
                   '"' item.reg-no      '",'      
                   '"' item.r-wid       '",'        
                   '"' item.shrink      '",'        
                   '"' item.dept-name[1 ] '",'       
                   '"' item.speed%[1 ]     '",'   
                   '"' item.dept-name[2 ]  '",'   
                   '"' item.speed%[2 ]     '",'   
                   '"' item.dept-name[3 ]  '",'   
                   '"' item.speed%[3 ]     '",'   
                   '"' item.dept-name[4 ]  '",'   
                   '"' item.speed%[4 ]     '",'   
                   '"' item.dept-name[5 ]  '",'   
                   '"' item.speed%[5 ]     '",'   
                   '"' item.dept-name[6 ]  '",'   
                   '"' item.speed%[6 ]     '",'   
                   '"' item.dept-name[7 ]  '",'   
                   '"' item.speed%[7 ]     '",'   
                   '"' item.dept-name[8 ]  '",'   
                   '"' item.speed%[8 ]     '",'   
                   '"' item.dept-name[9 ]  '",'   
                   '"' item.speed%[9 ]     '",'   
                   '"' item.dept-name[10]  '",'   
                   '"' item.speed%[10 ]    '",'
                    .              
    
             END.
    
             DO WITH FRAME item2:
                FIND FIRST loc WHERE 
                           loc.company = cocode
                      AND loc.loc = item.loc NO-LOCK NO-ERROR.
    
                /* CTS */
                CLEAR FRAME item2 ALL NO-PAUSE.
                DISPLAY
                   item.vend-no
                   item.vend-item
                   item.alloc
                   item.vend2-no
                   item.vend2-item
                   item.stocked
                   item.ord-policy
                   item.ord-level   WHEN item.ord-level   NE 0
                   item.ord-min     WHEN item.ord-min     NE 0
                   item.ord-max     WHEN item.ord-max     NE 0
                   item.pur-man
                   item.pur-uom
                   item.lead-days   WHEN item.lead-days   NE 0
                   item.loc
                   loc.dscr         WHEN available loc
                   item.loc-bin
                   item.cc-code
                   item.cons-uom
                   item.pur-rcode
                   item.pic-code
                   item.beg-bal      WHEN item.beg-bal    NE 0
                   item.beg-date
                   item.last-count   WHEN item.last-count NE 0
                   item.last-date
                   item.last-cost    WHEN item.last-cost  NE 0
                   item.avg-cost     WHEN item.avg-cost   NE 0
                   head21
                   item.q-onh        WHEN item.q-onh      NE 0
                   item.q-ono        WHEN item.q-ono      NE 0
                   item.q-comm       WHEN item.q-comm     NE 0
                   item.q-back       WHEN item.q-back     NE 0
                   item.q-avail      WHEN item.q-avail    NE 0.
    
               IF rd-dest EQ 3 THEN
                PUT STREAM s-temp  
                   '"' item.vend-no     '",'
                   '"' item.vend-item   '",'
                   '"' item.alloc       '",'
                   '"' item.vend2-no    '",'
                   '"' item.vend2-item  '",'
                   '"' item.stocked     '",'
                   '"' item.ord-policy  '",'
                   '"' item.ord-level   '",'
                   '"' item.ord-min     '",'
                   '"' item.ord-max     '",'
                   '"' item.pur-man     '",'
                   '"' item.pur-uom     '",'
                   '"' item.lead-days   '",'
                   '"' item.loc " " v-loc-descr       '",'
                   '"' item.loc-bin     '",'
                   '"' item.cc-code     '",'
                   '"' item.cons-uom    '",'
                   '"' item.pur-rcode   '",'
                   '"' item.pic-code    '",'
                   '"' item.beg-bal     '",'
                   '"' item.beg-date    '",'
                   '"' item.last-count  '",'
                   '"' item.last-date   '",'
                   '"' item.last-cost   '",'
                   '"' item.avg-cost    '",'
                   '"' item.q-onh       '",'
                   '"' item.q-ono       '",'
                   '"' item.q-comm      '",'
                   '"' item.q-back      '",'
                   '"' item.q-avail     '",'
                   .
             END. /* end for frame item2 */
    
             RUN est-board (ROWID(item)).
    
          END. /* end for each */
       END. /* end for detail */
    END. /* end for dor */
    
    
    IF doe THEN DO:
       IF rd-dest EQ 3 THEN DO:
          PUT STREAM s-temp 
             "CAT,"
             "ITEM,"       
             "DESCRIPTION,"                    
             "CALIPER WEIGHT,"   
             "<               Valid   Roll   Widths               >"
    
             SKIP.
        END.
    
        FOR EACH ITEM NO-LOCK WHERE 
                 item.company = cocode 
             AND item.loc = locode
             AND (item.i-code = "E")                             
             AND LOOKUP(item.mat-type,"B,P") GT 0     
             AND item.procat >= fcat
             AND item.procat <= tcat
          BREAK BY item.company 
                BY item.loc 
                BY item.i-code
                BY item.mat-type 
                BY item.procat 
                BY item.i-no WITH FRAME iteme:
    
          DISPLAY
             "" @ item.procat
             item.procat when first-of(item.procat)
             item.i-no
             item.i-name
             item.cal
             item.basis-w.
          PUT SKIP.
    
          IF rd-dest EQ 3 THEN DO:
             v-procat = IF first-of(item.procat) THEN item.procat
                     ELSE "".
    
             PUT STREAM s-temp  
                '"' v-procat      '",'
                '"' item.i-no     '",'
                '"' item.i-name   '",'
                '"' item.cal      '",'
                '"' item.basis-w  '"'
             SKIP.
    
          END.
    
          RUN est-board (ROWID(item)).
        END.
    END. */

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-case C-Win 
PROCEDURE run-case :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE v-printed AS LOG INIT NO NO-UNDO.

    FORM
        SKIP
        "  Item No:" TO 10 item.i-no     " Item Code:" TO 60 item.i-code    SKIP
        "     Name:" TO 10 item.i-name   "Mat'l Type:" TO 60 item.mat-type  SKIP
        "     Desc:" TO 10 item.i-dscr   " Cost Type:" TO 60 item.cost-type SKIP
        " Est.Desc:" TO 10 item.est-dscr "  Category:" TO 60 item.procat    SKIP
        "   Length:" TO 10 item.case-l     "Lbs/Case:" TO 60 item.avg-w     SKIP
        "    Width:" TO 10 item.case-w  "Pieces/Case:" TO 60 item.box-case  SKIP
        "    Depth:" TO 10 item.case-d "Cases/Pallet:" TO 60 item.case-pall SKIP
        WITH FRAME item STREAM-IO WIDTH 80 OVERLAY NO-LABELS NO-UNDERLINE.

    FORM
        item.procat item.i-no item.i-name FORMAT "x(27)"
        item.avg-w item.case-pall item.last-cost item.cons-uom
        /* CTS */
        item.q-onh    /* CTS */ FORMAT "->>>>>,>>9.9<<"
        item.q-ono    /* CTS */ FORMAT "->>>>>,>>9.9<<"
        item.q-comm   /* CTS */ FORMAT ">>>>>,>>9.9<<"
        item.q-avail  /* CTS */ FORMAT "->>>>>,>>9.9<<"
        /* CTS end */
        SKIP
        HEADER
        "CAT   ITEM       DESCRIPTION                 Lbs/Cas Cas/Pal    COST   UOM       On Hand      On Order     Allocated     Available"
        WITH FRAME itemx NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 132.

    FORM
        item.procat item.i-no item.i-name SPACE(3) item.avg-w SPACE(5)
        item.case-pall FORMAT ">>9"
        HEADER
        "CAT   ITEM       DESCRIPTION                      Lbs/Cas Cas/Pal"
        WITH FRAME iteme NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 119.


    ASSIGN
        str-tit2 = c-win:TITLE + " - Corrugated Case List"
        {sys/inc/ctrtext.i str-tit2 112}.

    DISPLAY "" WITH FRAME r-top.
/*
IF dor THEN DO:
IF NOT detail THEN DO:

IF rd-dest EQ 3 THEN 
   PUT STREAM s-temp UNFORMATTED 
      "CAT,ITEM,DESCRIPTION,Lbs/Cas Cas/Pal,COST,UOM,On Hand,On Order,Allocated,Available"                 
   SKIP. 

FOR EACH item NO-LOCK WHERE 
         item.company = cocode 
     AND item.loc = locode   
     AND item.i-code = "R"
     AND item.mat-type = "C"
     AND item.procat >= fcat
     AND item.procat <= tcat
   BREAK BY item.i-code 
         BY item.mat-type 
         BY item.procat 
         BY item.i-no WITH FRAME itemx:

   v-printed = YES.

   DISPLAY
      item.procat  WHEN FIRST-OF(item.procat)
      item.i-no
      item.i-name
      item.avg-w
      item.case-pall
      item.last-cost WHEN ce-ctrl.r-cost = NO
      item.avg-cost WHEN ce-ctrl.r-cost = YES @ item.last-cost
      item.cons-uom
      item.q-onh
      item.q-ono
      item.q-comm
      item.q-avail.
   DOWN.

   IF rd-dest EQ 3 THEN DO:
      IF FIRST-OF(ITEM.procat) THEN 
            v-procat = ITEM.procat.
         ELSE
            v-procat = "".

         IF ce-ctrl.r-cost = NO THEN
            v-last-cost = ITEM.last-cost.
         ELSE
            v-last-cost = item.avg-cost.

         PUT STREAM s-temp UNFORMATTED 
            '"' v-procat      '",'
            '"' item.i-no     '",'
            '"' item.i-name   '",'
            '"' item.avg-w    '",'
            '"' item.case-pall    '",'
            '"' v-last-cost   '",' 
            '"' item.cons-uom '",'
            '"' item.q-onh    '",'
            '"' item.q-ono    '",'
            '"' item.q-comm   '",'
            '"' item.q-avail  '"'
            SKIP.
   END.
END.
END.
ELSE DO:
IF rd-dest EQ 3 THEN
  PUT STREAM s-temp UNFORMATTED
      "Item No,Item Code,Name,Mat'l Type,Desc,Cost Type,Est. Desc,Category,Length,Width,Depth,Lbs/Case,Pieces/Case,Cases/Pallet,
      Vendor 1,Item #,Auto Alloc.?,Vendor 2,Item #,Stocked?,Reorder Policy,Purchased or Manf?,Reorder Level ,Purchased UOM, 
      Minimum Order ,Lead Time (Days), Maximum Order ,Beg Balance ,Beg Bal Date,Warehouse,Last Count,Bin,Count Date, 
      Cycle Code,Cons. UOM,Last Cost ,Purch. Rpt Code,Prod.Code,Avg. Cost , 
      On Hand,On Order,Committed,Backordered,Available"
      SKIP.

FOR EACH item NO-LOCK WHERE 
         item.company = cocode 
     AND item.loc = locode 
     AND item.i-code = "R" 
     AND item.mat-type = "C"
     AND item.procat >= fcat
     AND item.procat <= tcat
   BREAK BY item.i-code 
         BY item.mat-type 
         BY item.procat 
         BY item.i-no WITH FRAME item:

   v-printed = yes.

   DISPLAY
      item.i-no
      item.i-name
      item.i-dscr
      item.est-dscr
      item.i-code
      item.mat-type
      item.cost-type
      item.procat
      item.case-l
      item.case-w
      item.case-d
      item.avg-w
      item.box-case
      item.case-pall.

   IF rd-dest EQ 3 THEN DO:
      PUT STREAM s-temp UNFORMATTED 
         '"' item.i-no        '",'
         '"' item.i-code      '",'
         '"' item.i-name      '",'
         '"' item.mat-type    '",'
         '"' item.i-dscr      '",'
         '"' item.cost-type   '",'
         '"' item.est-dscr    '",'
         '"' item.procat      '",'
         '"' item.case-l      '",'
         '"' item.case-w      '",'
         '"' item.case-d      '",'
         '"' item.avg-w       '",'
         '"' item.box-case    '",'
         '"' item.case-pall   '",'.
   END.

   DO WITH FRAME item2:
      FIND FIRST loc WHERE loc.company = cocode 
             AND loc.loc = item.loc NO-LOCK NO-ERROR.
      DISPLAY
         item.vend-no
         item.vend-item
         item.alloc
         item.vend2-no
         item.vend2-item
         item.stocked
         item.ord-policy
         item.ord-level   WHEN ITEM.ord-level NE 0
         item.ord-min     WHEN item.ord-min   NE 0
         item.ord-max     WHEN item.ord-max   NE 0
         item.pur-man
         item.pur-uom
         item.lead-days   WHEN item.lead-days NE 0
         item.loc
         loc.dscr         WHEN AVAILABLE loc
         item.loc-bin
         item.cc-code
         item.cons-uom
         item.pur-rcode
         item.pic-code
         item.beg-bal      WHEN item.beg-bal NE 0
         item.beg-date
         item.last-count   WHEN item.last-count NE 0
         item.last-date      
         item.last-cost    WHEN item.last-cost  NE 0
         item.avg-cost     WHEN item.avg-cost   NE 0
         head21
         item.q-onh        WHEN item.q-onh   NE 0
         item.q-ono        WHEN item.q-ono   NE 0
         item.q-comm       WHEN item.q-comm  NE 0
         item.q-back       WHEN item.q-back  NE 0
         item.q-avail      WHEN item.q-avail NE 0 
         SKIP
         /*"*****************"*/.
   END.
   IF rd-dest EQ 3 THEN DO:
      IF AVAIL loc THEN 
         v-loc-descr = loc.dscr.
      ELSE
         v-loc-descr = "".

      PUT STREAM s-temp UNFORMATTED 
         '"' item.vend-no     '",'
         '"' item.vend-item   '",'
         '"' item.alloc       '",'
         '"' item.vend2-no    '",'
         '"' item.vend2-item  '",'
         '"' item.stocked     '",'
         '"' item.ord-policy  '",'
         '"' item.pur-man     '",'
         '"' item.ord-level   '",'
         '"' item.pur-uom     '",'
         '"' item.ord-min     '",'
         '"' item.lead-days   '",'
         '"' item.ord-max     '",'
         '"' item.beg-bal     '",' 
         '"' item.beg-date    '",'              
         '"' item.loc " " v-loc-descr       '",'
         '"' item.last-count  '",'
         '"' item.loc-bin     '",'
         '"' item.last-date   '",'
         '"' item.cc-code     '",'
         '"' item.cons-uom    '",'
         '"' item.last-cost   '",' 
         '"' item.pur-rcode   '",'
         '"' item.pic-code    '",'
         '"' item.avg-cost    '",'
         '"' item.q-onh       '",' 
         '"' item.q-ono       '",' 
         '"' item.q-comm      '",' 
         '"' item.q-back      '",' 
         '"' item.q-avail     '"'
         SKIP.
   END.

   RUN est-not-board(ROWID(item)).
END.
END.
END.

IF doe THEN DO:
IF v-printed THEN 
page.

IF rd-dest EQ 3 THEN
PUT STREAM s-temp UNFORMATTED
   'CAT,ITEM,DESCRIPTION,Lbs/Cas Cas/Pal'
   SKIP. 

FOR EACH ITEM NO-LOCK WHERE 
      item.company = cocode 
  AND item.loc = locode   
  AND item.i-code = "E"
  AND item.mat-type = "C"
  AND item.procat >= fcat
  AND item.procat <= tcat
BREAK BY item.i-code 
      BY item.mat-type 
      BY item.procat 
      BY item.i-no WITH FRAME iteme:

DISPLAY 
   item.procat when first-of(item.procat)
   item.i-no
   item.i-name
   item.avg-w
   item.case-pall.
PUT SKIP.

IF rd-dest EQ 3 THEN DO:
   IF FIRST-OF(ITEM.procat) THEN
      v-procat = ITEM.procat.
   ELSE
      v-procat = "".

   PUT STREAM s-temp UNFORMATTED
      '"' v-procat      '",'
      '"' item.i-no     '",'
      '"' item.i-name   '",'
      '"' sqin-lb       '",'
      '"' linin-lb      '"'  
      SKIP.
END.

RUN est-not-board (ROWID(item)).
END.
END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-film C-Win 
PROCEDURE run-film :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE v-printed AS LOG INIT NO NO-UNDO.

    FORM
        "  Item No:" TO 10 item.i-no     " Item Code:" TO 60 item.i-code    SKIP
        "     Name:" TO 10 item.i-name   "Mat'l Type:" TO 60 item.mat-type  SKIP
        "     Desc:" TO 10 item.i-dscr   " Cost Type:" TO 60 item.cost-type SKIP
        " Est.Desc:" TO 10 item.est-dscr "  Category:" TO 60 item.procat    SKIP
        "Sq. In/Lb:" TO 10 item.sqin-lb   SKIP
        WITH FRAME item STREAM-IO WIDTH 80 OVERLAY NO-LABELS NO-UNDERLINE.

    FORM
        item.procat item.i-no item.i-name FORMAT "x(30)"
        SPACE(3) item.sqin-lb FORMAT ">>>>>9"
        item.last-cost item.cons-uom
        item.q-onh item.q-ono item.q-comm item.q-avail SKIP
        HEADER
        "CAT   ITEM       DESCRIPTION                      Sq~"/Lb     COST     UOM       On Hand      On Order     Allocated     Available"
        WITH FRAME itemx NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 132.

    FORM
        item.procat item.i-no item.i-name SPACE(10) item.sqin-lb FORMAT ">>>>>9"
        HEADER
        "CAT   ITEM       DESCRIPTION                             Sq~"/Lb "
        WITH FRAME iteme NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 119.

    ASSIGN
        str-tit2 = c-win:TITLE + " - Film/Leaf List"
        {sys/inc/ctrtext.i str-tit2 112}.

    DISPLAY "" WITH FRAME r-top.
/*
IF dor THEN DO:

/* VIEW FRAME r-top. */

IF NOT detail THEN DO:
IF rd-dest EQ 3 THEN 
   PUT STREAM s-temp UNFORMATTED 
      'CAT,ITEM,DESCRIPTION,Sq~"/Lb,COST,UOM,On Hand,On Order,Allocated,Available'                 
   SKIP.

FOR EACH ITEM NO-LOCK WHERE
         item.company = cocode 
     AND item.loc = locode 
     AND item.i-code = "R"
     AND index("FLW",item.mat-type) > 0
     AND item.procat >= fcat
     AND item.procat <= tcat
   BREAK BY item.company 
         BY item.loc 
         BY item.i-code
         BY item.mat-type 
         BY item.procat 
         BY item.i-no 
   WITH FRAME itemx:

   v-printed = yes.

   DISPLAY
      item.procat    WHEN FIRST-OF(item.procat)
      item.i-no
      item.i-name
      item.sqin-lb
      item.last-cost WHEN ce-ctrl.r-cost = NO
      item.avg-cost  WHEN ce-ctrl.r-cost = YES @ item.last-cost
      item.cons-uom
      item.q-onh
      item.q-ono
      item.q-comm
      item.q-avail.
   DOWN.

   IF rd-dest EQ 3 THEN DO:
      IF FIRST-OF(ITEM.procat) THEN 
         v-procat = ITEM.procat.
      ELSE
         v-procat = "".

      IF ce-ctrl.r-cost = NO THEN
         v-last-cost = ITEM.last-cost.
      ELSE
         v-last-cost = item.avg-cost.

      PUT STREAM s-temp UNFORMATTED 
         '"' v-procat      '",'
         '"' item.i-no     '",'
         '"' item.i-name   '",'
         '"' item.sqin-lb  '",'
         '"' v-last-cost   '",' 
         '"' item.cons-uom '",'
         '"' item.q-onh    '",'
         '"' item.q-ono    '",'
         '"' item.q-comm   '",'
         '"' item.q-avail  '"'
         SKIP.
   END.
END.
END.
ELSE DO:
IF rd-dest EQ 3 THEN
  PUT STREAM s-temp UNFORMATTED
     "Item No,Item Code,Name,Mat'l Type,Desc,Cost Type,Est.Desc,Category,Sq. In/Lb,
      Vendor 1,Item #,Auto Alloc.?,Vendor 2,Item #,Stocked?,Reorder Policy,Purchased or Manf?,Reorder Level ,Purchased UOM, 
      Minimum Order ,Lead Time (Days), Maximum Order ,Beg Balance ,Beg Bal Date,Warehouse,Last Count,Bin,Count Date, 
      Cycle Code,Cons. UOM,Last Cost ,Purch. Rpt Code,Prod.Code,Avg. Cost , 
      On Hand,On Order,Committed,Backordered,Available"
      SKIP.

FOR EACH item NO-LOCK WHERE
         item.company = cocode 
     AND item.loc = locode 
     AND item.i-code = "R"
     AND index("FLW",item.mat-type) > 0
     AND item.procat >= fcat
     AND item.procat <= tcat
   BREAK BY item.company 
         BY item.loc 
         BY item.i-code
         BY item.mat-type 
         BY item.procat 
         BY item.i-no 
   WITH FRAME item:

   v-printed = YES.

   DISPLAY
      item.i-no
      item.i-name
      item.i-dscr
      item.est-dscr
      item.i-code
      item.mat-type
      item.cost-type
      item.procat
      item.sqin-lb      WHEN item.sqin-lb  NE 0.

   IF rd-dest EQ 3 THEN DO:
      PUT STREAM s-temp UNFORMATTED 
         '"' item.i-no        '",'
         '"' item.i-code      '",'
         '"' item.i-name      '",'
         '"' item.mat-type    '",'
         '"' item.i-dscr      '",'
         '"' item.cost-type   '",'
         '"' item.est-dscr    '",'
         '"' item.procat      '",'
         '"' item.sqin-lb     '",'. 
   END.

   DO WITH FRAME item2:
      FIND FIRST loc WHERE 
                 loc.company = cocode 
             AND loc.loc = item.loc no-lock no-error.
      DISPLAY
         item.vend-no
         item.vend-item
         item.alloc
         item.vend2-no
         item.vend2-item
         item.stocked
         item.ord-policy
         item.ord-level   WHEN item.ord-level NE 0
         item.ord-min     WHEN item.ord-min   NE 0
         item.ord-max     WHEN item.ord-max   NE 0
         item.pur-man
         item.pur-uom
         item.lead-days   WHEN item.lead-days NE 0
         item.loc
         loc.dscr         WHEN available loc
         item.loc-bin
         item.cc-code
         item.cons-uom
         item.pur-rcode
         item.pic-code
         item.beg-bal      WHEN item.beg-bal NE 0
         item.beg-date
         item.last-count   WHEN item.last-count NE 0
         item.last-date
         item.last-cost    WHEN item.last-cost  NE 0
         item.avg-cost     WHEN item.avg-cost   NE 0
         head21
         item.q-onh        WHEN item.q-onh   NE 0
         item.q-ono        WHEN item.q-ono   NE 0
         item.q-comm       WHEN item.q-comm  NE 0
         item.q-back       WHEN item.q-back  NE 0
         item.q-avail      WHEN item.q-avail NE 0.
   END.

   IF rd-dest EQ 3 THEN DO:
      IF AVAIL loc THEN 
         v-loc-descr = loc.dscr.
      ELSE
         v-loc-descr = "".

      PUT STREAM s-temp UNFORMATTED 
         '"' item.vend-no     '",'
         '"' item.vend-item   '",'
         '"' item.alloc       '",'
         '"' item.vend2-no    '",'
         '"' item.vend2-item  '",'
         '"' item.stocked     '",'
         '"' item.ord-policy  '",'
         '"' item.pur-man     '",'
         '"' item.ord-level   '",'
         '"' item.pur-uom     '",'
         '"' item.ord-min     '",'
         '"' item.lead-days   '",'
         '"' item.ord-max     '",'
         '"' item.beg-bal     '",' 
         '"' item.beg-date    '",'              
         '"' item.loc " " v-loc-descr       '",'
         '"' item.last-count  '",'
         '"' item.loc-bin     '",'
         '"' item.last-date   '",'
         '"' item.cc-code     '",'
         '"' item.cons-uom    '",'
         '"' item.last-cost   '",' 
         '"' item.pur-rcode   '",'
         '"' item.pic-code    '",'
         '"' item.avg-cost    '",'
         '"' item.q-onh       '",' 
         '"' item.q-ono       '",' 
         '"' item.q-comm      '",' 
         '"' item.q-back      '",' 
         '"' item.q-avail     '"'
         SKIP.
   END.

   RUN est-not-board(ROWID(item)).
END.
END.
END.

IF doe THEN DO:
IF v-printed THEN PAGE.

IF rd-dest EQ 3 THEN
PUT STREAM s-temp UNFORMATTED
   "CAT,ITEM,DESCRIPTION,Sq~"/Lb"
   SKIP. 

FOR EACH ITEM NO-LOCK WHERE 
      item.company = cocode 
  AND item.loc = locode
  AND item.i-code = "E"
  AND index("FLW",item.mat-type) > 0
  AND item.procat >= fcat
  AND item.procat <= tcat
BREAK BY item.company 
      BY item.loc 
      BY item.i-code
      BY item.mat-type 
      BY item.procat 
      BY item.i-no 
WITH FRAME iteme:

DISPLAY
   item.procat WHEN FIRST-OF(item.procat)
   item.i-no
   item.i-name
   item.sqin-lb.
PUT SKIP.

IF rd-dest EQ 3 THEN DO:
   IF FIRST-OF(ITEM.procat) THEN
      v-procat = ITEM.procat.
   ELSE
      v-procat = "".

   PUT STREAM s-temp UNFORMATTED
      '"' v-procat      '",'
      '"' item.i-no     '",'
      '"' item.i-name   '",'
      '"' item.sqin-lb  '"'  
      SKIP.
END.

RUN est-not-board (ROWID(item)).
END.
END. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-glue C-Win 
PROCEDURE run-glue :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE v-printed AS LOG INIT NO NO-UNDO.

    FORM
        "  Item No:" TO 10 item.i-no     " Item Code:" TO 60 item.i-code     SKIP
        "     Name:" TO 10 item.i-name   "Mat'l Type:" TO 60 item.mat-type   SKIP
        "     Desc:" TO 10 item.i-dscr   " Cost Type:" TO 60 item.cost-type  SKIP
        " Est.Desc:" TO 10 item.est-dscr "  Category:" TO 60 item.procat     SKIP
        "Sq. In/Lb:" TO 10 item.sqin-lb                                      SKIP
        "Lin.In/Lb:" TO 10 item.linin-lb                                     SKIP
        WITH FRAME ITEM STREAM-IO WIDTH 80 OVERLAY NO-LABELS NO-UNDERLINE.

    FORM
        item.procat item.i-no item.i-name            FORMAT "x(27)"
        item.sqin-lb FORMAT ">>>>>9" item.linin-lb   FORMAT ">>>>>9"
        item.last-cost item.cons-uom
        item.q-onh item.q-ono item.q-comm item.q-avail SKIP
        HEADER
        'CAT   ITEM       DESCRIPTION                 Sq"/Lb Lin"/Lb    COST     UOM       On Hand      On Order     Allocated     Available'
        WITH FRAME itemx NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 132.

    FORM
        item.procat item.i-no item.i-name SPACE(10)
        item.sqin-lb  FORMAT ">>>>>9" SPACE(2) item.linin-lb FORMAT ">>>>>9"
        HEADER
        'CAT   ITEM       DESCRIPTION                             Sq"/Lb Lin"/Lb'
        WITH FRAME iteme NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 119.

    ASSIGN
        str-tit2 = c-win:TITLE + " - Adhesives List"
        {sys/inc/ctrtext.i str-tit2 112}.

    DISPLAY "" WITH FRAME r-top.
/*
IF dor THEN DO:
  IF NOT detail THEN DO: 
     IF rd-dest EQ 3 THEN 
        PUT STREAM s-temp UNFORMATTED 
           "CAT,ITEM,DESCRIPTION,Sq'"'/Lb,Lin'"'/Lb,COST,UOM,On Hand,On Order,Allocated,Available"                 
        SKIP. 

     FOR EACH item NO-LOCK WHERE
              item.company = cocode 
          AND item.loc = locode 
          AND item.i-code = "R"
          AND item.mat-type EQ "G" 
          AND item.procat >= fcat
          AND item.procat <= tcat
        BREAK BY item.company 
              BY item.loc 
              BY item.i-code
              BY item.mat-type 
              BY item.procat 
              BY item.i-no WITH FRAME itemx:

        v-printed = YES.

        DISPLAY
           item.procat  WHEN FIRST-OF(item.procat)
           item.i-no
           item.i-name
           item.sqin-lb
           item.linin-lb
           item.last-cost WHEN ce-ctrl.r-cost = NO
           item.avg-cost WHEN ce-ctrl.r-cost = YES @ item.last-cost
           item.cons-uom
           item.q-onh
           item.q-ono
           item.q-comm
           item.q-avail.
        DOWN.

        IF rd-dest EQ 3 THEN DO:
           IF FIRST-OF(ITEM.procat) THEN 
                 v-procat = ITEM.procat.
              ELSE
                 v-procat = "".

              IF ce-ctrl.r-cost = NO THEN
                 v-last-cost = ITEM.last-cost.
              ELSE
                 v-last-cost = item.avg-cost.

              PUT STREAM s-temp UNFORMATTED 
                 '"' v-procat      '",'
                 '"' item.i-no     '",'
                 '"' item.i-name   '",'
                 '"' item.sqin-lb  '",'
                 '"' item.linin-lb    '",'
                 '"' v-last-cost   '",' 
                 '"' item.cons-uom '",'
                 '"' item.q-onh    '",'
                 '"' item.q-ono    '",'
                 '"' item.q-comm   '",'
                 '"' item.q-avail  '"'
                 SKIP.
        END.
     END.
  END.
  ELSE DO: 
     IF rd-dest EQ 3 THEN
       PUT STREAM s-temp UNFORMATTED
           "Item No,Item Code,Name,Mat'l Type,Desc,Cost Type,Est. Desc,Category,Sq. In/Lb,Lin.In/Lb,
           Vendor 1,Item #,Auto Alloc.?,Vendor 2,Item #,Stocked?,Reorder Policy,Purchased or Manf?,Reorder Level ,Purchased UOM, 
           Minimum Order ,Lead Time (Days), Maximum Order ,Beg Balance ,Beg Bal Date,Warehouse,Last Count,Bin,Count Date, 
           Cycle Code,Cons. UOM,Last Cost ,Purch. Rpt Code,Prod.Code,Avg. Cost , 
           On Hand,On Order,Committed,Backordered,Available"
           SKIP.
     FOR EACH item NO-LOCK WHERE
              item.company = cocode 
          AND item.loc = locode 
          AND item.i-code = "R"
          AND item.mat-type = "G"
          AND item.procat >= fcat
          AND item.procat <= tcat
        BREAK BY item.company 
              BY item.loc  
              BY item.i-code
              BY item.mat-type 
              BY item.procat  
              BY item.i-no WITH FRAME item:

        v-printed = YES.

        DISPLAY
           item.i-no
           item.i-name
           item.i-dscr
           item.est-dscr
           item.i-code
           item.mat-type
           item.cost-type
           item.procat
           item.sqin-lb      WHEN item.sqin-lb   NE 0
           item.linin-lb     WHEN item.linin-lb  NE 0.

        IF rd-dest EQ 3 THEN DO:
           PUT STREAM s-temp UNFORMATTED 
              '"' item.i-no        '",'
              '"' item.i-code      '",'
              '"' item.i-name      '",'
              '"' item.mat-type    '",'
              '"' item.i-dscr      '",'
              '"' item.cost-type   '",'
              '"' item.est-dscr    '",'
              '"' item.procat      '",'
              '"' item.sqin-lb     '",'
              '"' item.linin-lb    '",'.
        END.

        DO WITH FRAME item2:
           FIND FIRST loc WHERE loc.company = cocode 
                            AND loc.loc = item.loc NO-LOCK NO-ERROR.
           DISPLAY
              item.vend-no
              item.vend-item
              item.alloc
              item.vend2-no
              item.vend2-item
              item.stocked
              item.ord-policy
              item.ord-level   WHEN item.ord-level NE 0
              item.ord-min     WHEN item.ord-min   NE 0
              item.ord-max     WHEN item.ord-max   NE 0
              item.pur-man
              item.pur-uom
              item.lead-days   WHEN item.lead-days NE 0
              item.loc
              loc.dscr         WHEN AVAILABLE loc
              item.loc-bin
              item.cc-code
              item.cons-uom
              item.pur-rcode
              item.pic-code
              item.beg-bal      WHEN item.beg-bal NE 0
              item.beg-date
              item.last-count   WHEN item.last-count NE 0
              item.last-date
              item.last-cost    WHEN item.last-cost  NE 0
              item.avg-cost     WHEN item.avg-cost   NE 0
              head21
              item.q-onh        WHEN item.q-onh   NE 0
              item.q-ono        WHEN item.q-ono   NE 0
              item.q-comm       WHEN item.q-comm  NE 0
              item.q-back       WHEN item.q-back  NE 0
              item.q-avail      WHEN item.q-avail NE 0.
        END.

        IF rd-dest EQ 3 THEN DO:
           IF AVAIL loc THEN 
              v-loc-descr = loc.dscr.
           ELSE
              v-loc-descr = "".

           PUT STREAM s-temp UNFORMATTED 
              '"' item.vend-no     '",'
              '"' item.vend-item   '",'
              '"' item.alloc       '",'
              '"' item.vend2-no    '",'
              '"' item.vend2-item  '",'
              '"' item.stocked     '",'
              '"' item.ord-policy  '",'
              '"' item.pur-man     '",'
              '"' item.ord-level   '",'
              '"' item.pur-uom     '",'
              '"' item.ord-min     '",'
              '"' item.lead-days   '",'
              '"' item.ord-max     '",'
              '"' item.beg-bal     '",' 
              '"' item.beg-date    '",'              
              '"' item.loc " " v-loc-descr       '",'
              '"' item.last-count  '",'
              '"' item.loc-bin     '",'
              '"' item.last-date   '",'
              '"' item.cc-code     '",'
              '"' item.cons-uom    '",'
              '"' item.last-cost   '",' 
              '"' item.pur-rcode   '",'
              '"' item.pic-code    '",'
              '"' item.avg-cost    '",'
              '"' item.q-onh       '",' 
              '"' item.q-ono       '",' 
              '"' item.q-comm      '",' 
              '"' item.q-back      '",' 
              '"' item.q-avail     '"'
              SKIP.
        END.

        RUN est-not-board(ROWID(item)).
     END.
  END.
END.

IF doe THEN DO:
  IF v-printed THEN 
     page.

  IF rd-dest EQ 3 THEN
     PUT STREAM s-temp UNFORMATTED
        'CAT,ITEM,DESCRIPTION,Sq"/Lb Lin"/Lb'
        SKIP.  

  FOR EACH ITEM NO-LOCK WHERE
           item.company = cocode 
       AND item.loc = locode 
       AND item.i-code = "E"
       AND item.mat-type EQ "G"
       AND item.procat >= fcat
       AND item.procat <= tcat
     BREAK BY item.company 
           BY item.loc 
           BY item.i-code
           BY item.mat-type 
           BY item.procat 
           BY item.i-no WITH FRAME iteme:

     DISPLAY
        item.procat WHEN FIRST-OF(item.procat)
        item.i-no
        item.i-name
        item.sqin-lb
        item.linin-lb.
     PUT SKIP.

     IF rd-dest EQ 3 THEN DO:
        IF FIRST-OF(ITEM.procat) THEN
           v-procat = ITEM.procat.
        ELSE
           v-procat = "".

        PUT STREAM s-temp UNFORMATTED
           '"' v-procat      '",'
           '"' item.i-no     '",'
           '"' item.i-name   '",'
           '"' sqin-lb       '",'
           '"' linin-lb      '"'  
           SKIP.
     END.

     RUN est-not-board (ROWID(item)).
  END.
END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-ink C-Win 
PROCEDURE run-ink :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE v-printed AS LOG INIT NO NO-UNDO.

    FORM
        "  Item No:" TO 10 item.i-no     " Item Code:" TO 60 item.i-code    SKIP
        "     Name:" TO 10 item.i-name   "Mat'l Type:" TO 60 item.mat-type  SKIP
        "     Desc:" TO 10 item.i-dscr   " Cost Type:" TO 60 item.cost-type SKIP
        " Est.Desc:" TO 10 item.est-dscr "  Category:" TO 60 item.procat    SKIP
        "   Ink Type:" item.ink-type   TO 20 "     MSI/Lb:" TO 60 item.yield   SKIP
        " Press Type:" item.press-type TO 20 "Min Lbs/Job:" TO 60 item.min-lbs SKIP
        WITH FRAME item STREAM-IO WIDTH 80 OVERLAY NO-LABELS NO-UNDERLINE.

    FORM
        item.procat item.i-no item.i-name FORMAT "x(27)"
        SPACE(3) item.min-lbs SPACE(1) item.yield FORMAT ">>>>>9" item.last-cost
        item.cons-uom item.q-onh item.q-ono item.q-comm item.q-avail SKIP
        HEADER
        "CAT   ITEM       DESCRIPTION                 Min.Lbs MSI/Lb     COST     UOM       On Hand      On Order     Allocated     Available"
        WITH FRAME itemx NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 132.

    FORM
        item.procat item.i-no item.i-name SPACE(3) item.min-lbs SPACE(4)
        item.yield FORMAT ">>>>>9"
        HEADER
        "CAT   ITEM       DESCRIPTION                    Min.Lbs MSI/Lb "
        WITH FRAME iteme NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 119.

    ASSIGN
        str-tit2 = c-win:TITLE + " - Ink/Coating List"
        {sys/inc/ctrtext.i str-tit2 112}.

    DISPLAY "" WITH FRAME r-top.
/*
IF dor THEN DO:
   IF NOT detail THEN DO: 
      IF rd-dest EQ 3 THEN 
         PUT STREAM s-temp UNFORMATTED 
            "CAT,ITEM,DESCRIPTION,Min.Lbs,MSI/Lb,COST,UOM,On Hand,On Order,Allocated,Available"                 
         SKIP. 

      FOR EACH ITEM NO-LOCK WHERE
               item.company = cocode 
          AND item.loc = locode 
          AND item.i-code = "R"
          AND LOOKUP(ITEM.mat-type,"I,V") GT 0
          AND item.procat >= fcat
          AND item.procat <= tcat
         BREAK BY item.company 
               BY item.loc 
               BY item.i-code
               BY item.mat-type 
               BY item.procat 
               BY item.i-no WITH FRAME itemx:

         v-printed = yes.

         DISPLAY 
            item.procat  WHEN FIRST-OF(item.procat)
            item.i-no
            item.i-name
            item.min-lbs
            item.yield
            item.last-cost WHEN ce-ctrl.r-cost = NO
            item.avg-cost WHEN ce-ctrl.r-cost = YES @ item.last-cost
            item.cons-uom
            item.q-onh
            item.q-ono
            item.q-comm
            item.q-avail.
         DOWN.

         IF rd-dest EQ 3 THEN DO:
            IF FIRST-OF(ITEM.procat) THEN 
               v-procat = ITEM.procat.
            ELSE
               v-procat = "".

            IF ce-ctrl.r-cost = NO THEN
               v-last-cost = ITEM.last-cost.
            ELSE
               v-last-cost = item.avg-cost.

            PUT STREAM s-temp UNFORMATTED 
               '"' v-procat      '",'
               '"' item.i-no     '",'
               '"' item.i-name   '",'
               '"' item.min-lbs  '",'
               '"' item.yield    '",'
               '"' v-last-cost   '",' 
               '"' item.cons-uom '",'
               '"' item.q-onh    '",'
               '"' item.q-ono    '",'
               '"' item.q-comm   '",'
               '"' item.q-avail  '"'
               SKIP.
         END.
      END.
   END.
   ELSE DO:
      IF rd-dest EQ 3 THEN
        PUT STREAM s-temp UNFORMATTED
            "Item No,Item Code,Name,Mat'l Type,Desc,Cost Type,Est. Desc,Category,Ink Type,MSI/Lb,Press Type,Min Lbs/Job,
            Vendor 1,Item #,Auto Alloc.?,Vendor 2,Item #,Stocked?,Reorder Policy,Purchased or Manf?,Reorder Level ,Purchased UOM, 
            Minimum Order ,Lead Time (Days), Maximum Order ,Beg Balance ,Beg Bal Date,Warehouse,Last Count,Bin,Count Date, 
            Cycle Code,Cons. UOM,Last Cost ,Purch. Rpt Code,Prod.Code,Avg. Cost , 
            On Hand,On Order,Committed,Backordered,Available"
            SKIP.

      FOR EACH item NO-LOCK WHERE
               item.company = cocode 
           AND item.loc = locode 
           AND item.i-code = "R"
           AND LOOKUP(ITEM.mat-type,"I,V") GT 0      
           AND item.procat >= fcat
           AND item.procat <= tcat
         BREAK BY item.company 
               BY item.loc 
               BY item.i-code
               BY item.mat-type  
               BY item.procat  
               BY item.i-no WITH FRAME item:

         v-printed = yes.

         DISPLAY
            item.i-no
            item.i-name
            item.i-dscr
            item.est-dscr
            item.i-code
            item.mat-type
            item.cost-type
            item.procat
            item.ink-type   WHEN item.ink-type   NE ""
            item.press-type WHEN item.press-type NE ""
            item.yield      WHEN item.yield      NE 0
            item.min-lbs    WHEN item.min-lbs    NE 0.

         IF rd-dest EQ 3 THEN DO:
            PUT STREAM s-temp UNFORMATTED 
               '"' item.i-no        '",'
               '"' item.i-code      '",'
               '"' item.i-name      '",'
               '"' item.mat-type    '",'
               '"' item.i-dscr      '",'
               '"' item.cost-type   '",'
               '"' item.est-dscr    '",'
               '"' item.procat      '",'
               '"' item.ink-type    '",'
               '"' item.yield       '",'
               '"' item.press-type  '",'
               '"' item.min-lbs     '",'.
         END.

         DO WITH FRAME item2:
            FIND FIRST loc WHERE 
                       loc.company = cocode 
                   AND loc.loc = item.loc NO-LOCK NO-ERROR.
            DISPLAY
               item.vend-no
               item.vend-item
               item.alloc
               item.vend2-no
               item.vend2-item
               item.stocked
               item.ord-policy
               item.ord-level   WHEN item.ord-level NE 0
               item.ord-min     WHEN item.ord-min   NE 0
               item.ord-max     WHEN item.ord-max   NE 0
               item.pur-man
               item.pur-uom
               item.lead-days   WHEN item.lead-days NE 0
               item.loc
               loc.dscr         WHEN AVAILABLE loc
               item.loc-bin
               item.cc-code
               item.cons-uom
               item.pur-rcode
               item.pic-code
               item.beg-bal      WHEN item.beg-bal NE 0
               item.beg-date
               item.last-count   WHEN item.last-count NE 0
               item.last-date
               item.last-cost    WHEN item.last-cost  NE 0
               item.avg-cost     WHEN item.avg-cost   NE 0
               head21 
               item.q-onh        WHEN item.q-onh   NE 0
               item.q-ono        WHEN item.q-ono   NE 0
               item.q-comm       WHEN item.q-comm  NE 0
               item.q-back       WHEN item.q-back  NE 0
               item.q-avail      WHEN item.q-avail NE 0.
         END.

         IF rd-dest EQ 3 THEN DO:
            IF AVAIL loc THEN 
               v-loc-descr = loc.dscr.
            ELSE
               v-loc-descr = "".

            PUT STREAM s-temp UNFORMATTED 
               '"' item.vend-no     '",'
               '"' item.vend-item   '",'
               '"' item.alloc       '",'
               '"' item.vend2-no    '",'
               '"' item.vend2-item  '",'
               '"' item.stocked     '",'
               '"' item.ord-policy  '",'
               '"' item.pur-man     '",'
               '"' item.ord-level   '",'
               '"' item.pur-uom     '",'
               '"' item.ord-min     '",'
               '"' item.lead-days   '",'
               '"' item.ord-max     '",'
               '"' item.beg-bal     '",' 
               '"' item.beg-date    '",'              
               '"' item.loc " " v-loc-descr       '",'
               '"' item.last-count  '",'
               '"' item.loc-bin     '",'
               '"' item.last-date   '",'
               '"' item.cc-code     '",'
               '"' item.cons-uom    '",'
               '"' item.last-cost   '",' 
               '"' item.pur-rcode   '",'
               '"' item.pic-code    '",'
               '"' item.avg-cost    '",'
               '"' item.q-onh       '",' 
               '"' item.q-ono       '",' 
               '"' item.q-comm      '",' 
               '"' item.q-back      '",' 
               '"' item.q-avail     '"'
               SKIP.
         END.

         RUN est-not-board(ROWID(item)).
      END.
   END.
END.

IF doe THEN DO:
   IF v-printed THEN PAGE.

   IF rd-dest EQ 3 THEN
      PUT STREAM s-temp UNFORMATTED
         "CAT,ITEM,DESCRIPTION,Min.Lbs,MSI/Lb"
         SKIP.  

   FOR EACH ITEM NO-LOCK WHERE 
            item.company = cocode 
       AND  item.loc = locode
       AND  item.i-code = "E"
       AND  LOOKUP(ITEM.mat-type,"I,V") GT 0      
       AND  item.procat >= fcat
       AND  item.procat <= tcat
      BREAK BY item.company 
            BY item.loc  
            BY item.i-code
            BY item.mat-type 
            BY item.procat 
            BY item.i-no WITH FRAME iteme:

      DISPLAY
         "" @ item.procat
         item.procat when first-of(item.procat)
         item.i-no
         item.i-name
         item.min-lbs
         item.yield.
         PUT SKIP.

      IF rd-dest EQ 3 THEN DO:
         IF FIRST-OF(ITEM.procat) THEN
            v-procat = ITEM.procat.
         ELSE
            v-procat = "".

         PUT STREAM s-temp UNFORMATTED
            '"' v-procat      '",'
            '"' item.i-no     '",'
            '"' item.i-name   '",'
            '"' item.min-lbs  '",'
            '"' item.yield    '"'  
            SKIP.
      END.

      RUN est-not-board(ROWID(item)).
   END.
END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-misc C-Win 
PROCEDURE run-misc :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE v-printed AS LOG INIT NO NO-UNDO.

    FORM
        SKIP
        "  Item No:" TO 10 item.i-no     " Item Code:" TO 60 item.i-code    SKIP
        "     Name:" TO 10 item.i-name   "Mat'l Type:" TO 60 item.mat-type  SKIP
        "     Desc:" TO 10 item.i-dscr   " Cost Type:" TO 60 item.cost-type SKIP
        " Est.Desc:" TO 10 item.est-dscr "  Category:" TO 60 item.procat    SKIP
        WITH FRAME ITEM STREAM-IO WIDTH 80 OVERLAY NO-LABELS NO-UNDERLINE.

    FORM
        item.procat item.i-no item.i-name FORMAT "x(30)" SPACE(11)
        item.last-cost item.cons-uom
        item.q-onh   FORMAT "->>>>,>>9.999"
        item.q-ono   FORMAT "->>>>,>>9.999"
        item.q-comm  FORMAT "->>>>,>>9.999"
        item.q-avail FORMAT "->>>>,>>9.999"
        SKIP
        HEADER
        "CAT   ITEM       DESCRIPTION                                    COST   UOM       On Hand      On Order     Allocated     Available"
        WITH FRAME itemx NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 132.

    FORM
        item.procat item.i-no item.i-name
        HEADER
        "CAT   ITEM       DESCRIPTION   "
        WITH FRAME iteme NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 119.


    ASSIGN
        str-tit2 = c-win:TITLE + " - Miscellaneous List"
        {sys/inc/ctrtext.i str-tit2 112}.

    DISPLAY "" WITH FRAME r-top.
/*  
IF dor THEN DO:
IF NOT detail THEN DO:

  IF rd-dest EQ 3 THEN 
     PUT STREAM s-temp UNFORMATTED 
        "CAT,ITEM,DESCRIPTION,COST,UOM,On Hand,On Order,Allocated,Available"                 
     SKIP. 

  FOR EACH item NO-LOCK WHERE 
           item.company EQ cocode
       AND item.i-code  EQ "R"
       AND INDEX("MTDAOXY789",ITEM.mat-type) GT 0
       AND item.procat  GE fcat
       AND item.procat  LE tcat
     BREAK BY item.i-code
           BY item.mat-type
           BY item.procat
           BY item.i-no WITH FRAME itemx:

     v-printed = YES.

     DISPLAY
        item.procat  WHEN FIRST-OF(item.procat)
        item.i-no
        item.i-name
        item.last-cost WHEN ce-ctrl.r-cost = NO
        item.avg-cost WHEN ce-ctrl.r-cost = YES @ item.last-cost
        item.cons-uom
        item.q-onh
        item.q-ono
        item.q-comm
        item.q-avail.
     DOWN.

     IF rd-dest EQ 3 THEN DO:
        IF FIRST-OF(ITEM.procat) THEN 
              v-procat = ITEM.procat.
           ELSE
              v-procat = "".

           IF ce-ctrl.r-cost = NO THEN
              v-last-cost = ITEM.last-cost.
           ELSE
              v-last-cost = item.avg-cost.

           PUT STREAM s-temp UNFORMATTED 
              '"' v-procat      '",'
              '"' item.i-no     '",'
              '"' item.i-name   '",'
              '"' v-last-cost   '",' 
              '"' item.cons-uom '",'
              '"' item.q-onh    '",'
              '"' item.q-ono    '",'
              '"' item.q-comm   '",'
              '"' item.q-avail  '"'
              SKIP.
     END.
  END.
END.
ELSE DO:
  IF rd-dest EQ 3 THEN
    PUT STREAM s-temp UNFORMATTED
        "Item No,Item Code,Name,Mat'l Type,Desc,Cost Type,Est. Desc,Category,
        Vendor 1,Item #,Auto Alloc.?,Vendor 2,Item #,Stocked?,Reorder Policy,Purchased or Manf?,Reorder Level ,Purchased UOM, 
        Minimum Order ,Lead Time (Days), Maximum Order ,Beg Balance ,Beg Bal Date,Warehouse,Last Count,Bin,Count Date, 
        Cycle Code,Cons. UOM,Last Cost ,Purch. Rpt Code,Prod.Code,Avg. Cost , 
        On Hand,On Order,Committed,Backordered,Available"
        SKIP.

  FOR EACH item NO-LOCK WHERE 
           item.company EQ cocode
      AND item.i-code  EQ "R"
      AND INDEX("MTDAOXY789",ITEM.mat-type) GT 0
      AND item.procat  GE fcat
      AND item.procat  LE tcat
     BREAK BY item.i-code
           BY item.mat-type
           BY item.procat
           BY item.i-no WITH FRAME item:

     v-printed = YES. 

     DISPLAY 
        item.i-no
        item.i-name
        item.i-dscr
        item.est-dscr
        item.i-code
        item.mat-type
        item.cost-type
        item.procat.

     IF rd-dest EQ 3 THEN DO:
        PUT STREAM s-temp UNFORMATTED 
           '"' item.i-no        '",'
           '"' item.i-code      '",'
           '"' item.i-name      '",'
           '"' item.mat-type    '",'
           '"' item.i-dscr      '",'
           '"' item.cost-type   '",'
           '"' item.est-dscr    '",'
           '"' item.procat      '",'.
     END.         

     DO WITH FRAME item2:
        FIND FIRST loc WHERE 
                   loc.company = cocode 
           AND loc.loc = item.loc NO-LOCK NO-ERROR.

        DISPLAY
           item.vend-no
           item.vend-item
           item.alloc
           item.vend2-no
           item.vend2-item
           item.stocked
           item.ord-policy
           item.ord-level   WHEN item.ord-level NE 0
           item.ord-min     WHEN item.ord-min   NE 0
           item.ord-max     WHEN item.ord-max   NE 0
           item.pur-man
           item.pur-uom
           item.lead-days   WHEN item.lead-days NE 0
           item.loc
           loc.dscr         WHEN available loc
           item.loc-bin
           item.cc-code
           item.cons-uom
           item.pur-rcode
           item.pic-code
           item.beg-bal      WHEN item.beg-bal NE 0
           item.beg-date
           item.last-count   WHEN item.last-count NE 0
           item.last-date       
           item.last-cost    WHEN item.last-cost  NE 0
           item.avg-cost     WHEN item.avg-cost   NE 0
           head21
           item.q-onh        WHEN item.q-onh   NE 0
           item.q-ono        WHEN item.q-ono   NE 0
           item.q-comm       WHEN item.q-comm  NE 0
           item.q-back       WHEN item.q-back  NE 0
           item.q-avail      WHEN item.q-avail NE 0 
           SKIP.
     END.

     IF rd-dest EQ 3 THEN DO:
        IF AVAIL loc THEN 
           v-loc-descr = loc.dscr.
        ELSE
           v-loc-descr = "".

        PUT STREAM s-temp UNFORMATTED 
           '"' item.vend-no     '",'
           '"' item.vend-item   '",'
           '"' item.alloc       '",'
           '"' item.vend2-no    '",'
           '"' item.vend2-item  '",'
           '"' item.stocked     '",'
           '"' item.ord-policy  '",'
           '"' item.pur-man     '",'
           '"' item.ord-level   '",'
           '"' item.pur-uom     '",'
           '"' item.ord-min     '",'
           '"' item.lead-days   '",'
           '"' item.ord-max     '",'
           '"' item.beg-bal     '",' 
           '"' item.beg-date    '",'              
           '"' item.loc " " v-loc-descr       '",'
           '"' item.last-count  '",'
           '"' item.loc-bin     '",'
           '"' item.last-date   '",'
           '"' item.cc-code     '",'
           '"' item.cons-uom    '",'
           '"' item.last-cost   '",' 
           '"' item.pur-rcode   '",'
           '"' item.pic-code    '",'
           '"' item.avg-cost    '",'
           '"' item.q-onh       '",' 
           '"' item.q-ono       '",' 
           '"' item.q-comm      '",' 
           '"' item.q-back      '",' 
           '"' item.q-avail     '"'
           SKIP.
     END.

     RUN est-not-board(ROWID(item)).
  END.
END.
END.
IF doe THEN DO:
IF v-printed THEN
  page.

IF rd-dest EQ 3 THEN
  PUT STREAM s-temp UNFORMATTED
     'CAT,ITEM,DESCRIPTION'
     SKIP.

FOR EACH item NO-LOCK WHERE 
        item.company EQ cocode
    AND item.i-code  EQ "E"
    AND INDEX("MTDAOXY789",ITEM.mat-type) GT 0
    AND item.procat  GE fcat
    AND item.procat  LE tcat
   BREAK BY item.i-code
         BY item.mat-type
         BY item.procat
         BY item.i-no WITH FRAME iteme:

  DISPLAY 
     item.procat WHEN FIRST-OF(item.procat)
     item.i-no
     item.i-name.
  PUT SKIP.

  IF rd-dest EQ 3 THEN DO:
     IF FIRST-OF(ITEM.procat) THEN
        v-procat = ITEM.procat.
     ELSE
        v-procat = "".

     PUT STREAM s-temp UNFORMATTED
        '"' v-procat      '",'
        '"' item.i-no     '",'
        '"' item.i-name   '"'
        SKIP.
  END.

  RUN est-not-board (ROWID(item)).
END.
END. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- rm/menurep1.p 9/92 cd */
    /*                                                                            */
    /* raw materials costs - category sub menu                                    */
    /*                                                                            */
    /* -------------------------------------------------------------------------- */

    DEFINE VARIABLE rm-cst-amt LIKE item.last-cost.
    DEFINE VARIABLE v-printed  AS LOG INIT NO NO-UNDO.

    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.


    ASSIGN
        fco        = cocode
        tco        = cocode
        floc       = begin_whs
        tloc       = end_whs
        fcat       = begin_procat
        tcat       = end_procat
        fmat       = begin_mat
        tmat       = end_mat
        doe        = tb_est
        dor        = tb_real
        detail     = tb_detailed
        v-exp-name = cFileName.

    DEFINE VARIABLE cslist AS cha NO-UNDO.
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
            THEN ASSIGN str-tit4    = str-tit4 + ttRptSelected.TextList + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + "," .        
        ELSE 
            ASSIGN str-tit4    = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + ","
                .        
        cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    IF detail THEN
        excelheader = excelHeader + 
            "Vend 1,UOM," +
            "Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," +
            "Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," +
            "Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," +
            "Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," +
            "Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," +
            "Run Qty 6,Run Cost 6,Est Roll W,Run Qty 7,Run Cost 7,Run Qty 8,Run Cost 8," +
            "Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12," +
            "Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16," +
            "Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20," +
            "Vend 2,UOM," +
            "Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 6,Run Cost 6,Est Roll W,Run Qty 7,Run Cost 7,Run Qty 8,Run Cost 8," +
            "Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12," +
            "Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16," +
            "Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20," +
            "Vend 3,UOM," +
            "Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 6,Run Cost 6,Est Roll W,Run Qty 7,Run Cost 7,Run Qty 8,Run Cost 8," +
            "Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12," +
            "Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16," +
            "Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20," +
            "Vend 4,UOM," +
            "Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 6,Run Cost 6,Est Roll W,Run Qty 7,Run Cost 7,Run Qty 8,Run Cost 8," +
            "Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12," +
            "Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16," +
            "Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20," +
            "Vend 5,UOM," +
            "Run Qty 1,Run Cost 1,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 2,Run Cost 2,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 3,Run Cost 3,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 4,Run Cost 4,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 5,Run Cost 5,Est Roll W,Est Roll W,Est Roll W,Est Roll W,Est Roll W," + 
            "Run Qty 6,Run Cost 6,Est Roll WRun Qty 7,Run Cost 7,Run Qty 8,Run Cost 8," +
            "Run Qty 9,Run Cost 9,Run Qty 10,Run Cost 10,Run Qty 11,Run Cost 11,Run Qty 12,Run Cost 12," +
            "Run Qty 13,Run Cost 13,Run Qty 14,Run Cost 14,Run Qty 15,Run Cost 15,Run Qty 16,Run Cost 16," +
            "Run Qty 17,Run Cost 17,Run Qty 18,Run Cost 18,Run Qty 19,Run Cost 19,Run Qty 20,Run Cost 20"  . 

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    /*IF rd-dest EQ 3 THEN
       OUTPUT STREAM s-temp TO VALUE(v-exp-name).*/

    IF td-show-parm THEN RUN show-param.

    /*if rd_mat-type eq "Board - Paper" then
       run run-board. 
    else
    if rd_mat-type eq "Ink - Coating" then 
       run run-ink.
    else
    if rd_mat-type eq "Film - Leaf"   then 
       run run-film. 
    else
    if rd_mat-type eq "Adhesives"     then 
       run run-glue.
    else
    if rd_mat-type eq "Case"          then 
       run run-case. 
    else
    if rd_mat-type eq "Miscellaneous - Adders" then 
       run run-misc.*/


    SESSION:SET-WAIT-STATE ("general").

    ASSIGN 
        str-tit2 = c-win:TITLE + " - Board/Paper List"
        {sys/inc/ctrtext.i str-tit2 112}.

    DISPLAY "" WITH FRAME r-top.

    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM s-temp TO VALUE(cFileName).
        PUT STREAM s-temp UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    ASSIGN 
        str-tit4 = ""
        str-tit5 = "" .

    FOR EACH ITEM NO-LOCK WHERE 
        item.company = cocode 
        AND item.loc GE floc
        AND item.loc LE tloc  
        AND ((item.i-code EQ "R" AND dor EQ YES) OR
        (item.i-code EQ "E" AND doe EQ YES))
        /*  AND LOOKUP(item.mat-type,"B,P") GT 0 */
        AND item.procat >= fcat
        AND item.procat <= tcat
        AND item.mat-type >= fmat
        AND item.mat-type <= tmat 
        BREAK BY item.company 
        BY item.loc 
        BY item.i-code
        BY item.mat-type 
        BY item.procat  
        BY item.i-no WITH FRAME item:

        {custom/statusMsg.i " 'Processing Item#  '  + item.i-no "}

        CLEAR FRAME item ALL NO-PAUSE.
        v-printed = YES.

        /* DISPLAY head5
            item.i-no
            item.i-name
            item.i-dscr
            item.est-dscr
            item.i-code
            item.mat-type
            item.cost-type
            item.procat
            head5
            item.cal     WHEN item.cal     NE 0
            item.basis-w WHEN item.basis-w NE 0
            item.reg-no  WHEN item.reg-no  NE ""
            item.shrink  WHEN item.shrink  NE 0
            item.s-len   WHEN item.s-len   NE 0
            item.s-wid   WHEN item.s-wid   NE 0
            item.r-wid   WHEN item.r-wid   NE 0
            item.dept-name[1 ]   WHEN item.dept-name[1 ] NE ""
            item.speed%[1 ]      WHEN item.speed[1 ]     NE 0
            item.dept-name[2 ]   WHEN item.dept-name[2 ] NE ""
            item.speed%[2 ]      WHEN item.speed[2 ]     NE 0
            item.dept-name[3 ]   WHEN item.dept-name[3 ] NE ""
            item.speed%[3 ]      WHEN item.speed[3 ]     NE 0
            item.dept-name[4 ]   WHEN item.dept-name[4 ] NE ""
            item.speed%[4 ]      WHEN item.speed[4 ]     NE 0
            item.dept-name[5 ]   WHEN item.dept-name[5 ] NE ""
            item.speed%[5 ]      WHEN item.speed[5 ]     NE 0
            item.dept-name[6 ]   WHEN item.dept-name[6]  NE ""
            item.speed%[6 ]      WHEN item.speed[6 ]     NE 0
            item.dept-name[7 ]   WHEN item.dept-name[7 ] NE ""
            item.speed%[7 ]      WHEN item.speed[7 ]     NE 0
            item.dept-name[8 ]   WHEN item.dept-name[8 ] NE ""
            item.speed%[8 ]      WHEN item.speed[8 ]     NE 0
            item.dept-name[9 ]   WHEN item.dept-name[9 ] NE ""
            item.speed%[9 ]      WHEN item.speed[9 ]     NE 0
            item.dept-name[10]   WHEN item.dept-name[10] NE ""
            item.speed%[10]      WHEN item.speed[10]     NE 0.

         IF rd-dest EQ 3 THEN DO:
            PUT STREAM s-temp
               '"' item.i-no           '",'          
               '"' item.i-code         '",'        
               '"' item.i-name         '",'        
               '"' item.mat-type       '",'        
               '"' item.i-dscr         '",'        
               '"' item.cost-type      '",'        
               '"' item.est-dscr       '",'        
               '"' item.procat         '",'        
               '"' item.cal            '",'        
               '"' item.s-len          '",'        
               '"' item.s-wid          '",'        
               '"' item.basis-w        '",'        
               '"' item.reg-no         '",'      
               '"' item.r-wid          '",'        
               '"' item.shrink         '",'        
               '"' item.dept-name[1 ]  '",'       
               '"' item.speed%[1 ]     '",'   
               '"' item.dept-name[2 ]  '",'   
               '"' item.speed%[2 ]     '",'   
               '"' item.dept-name[3 ]  '",'   
               '"' item.speed%[3 ]     '",'   
               '"' item.dept-name[4 ]  '",'   
               '"' item.speed%[4 ]     '",'   
               '"' item.dept-name[5 ]  '",'   
               '"' item.speed%[5 ]     '",'   
               '"' item.dept-name[6 ]  '",'   
               '"' item.speed%[6 ]     '",'   
               '"' item.dept-name[7 ]  '",'   
               '"' item.speed%[7 ]     '",'   
               '"' item.dept-name[8 ]  '",'   
               '"' item.speed%[8 ]     '",'   
               '"' item.dept-name[9 ]  '",'   
               '"' item.speed%[9 ]     '",'   
               '"' item.dept-name[10]  '",'   
               '"' item.speed%[10 ]    '",'
                .              

         END. 

         DO WITH FRAME item2: */
        FIND FIRST loc WHERE 
            loc.company = cocode
            AND loc.loc = item.loc NO-LOCK NO-ERROR.

        /* CTS */
        /*    CLEAR FRAME item2 ALL NO-PAUSE.
            DISPLAY
               item.vend-no
               item.vend-item
               item.alloc
               item.vend2-no
               item.vend2-item
               item.stocked
               item.ord-policy
               item.ord-level   WHEN item.ord-level   NE 0
               item.ord-min     WHEN item.ord-min     NE 0
               item.ord-max     WHEN item.ord-max     NE 0
               item.pur-man
               item.pur-uom
               item.lead-days   WHEN item.lead-days   NE 0
               item.loc
               loc.dscr         WHEN available loc
               item.loc-bin
               item.cc-code
               item.cons-uom
               item.pur-rcode
               item.pic-code
               item.beg-bal      WHEN item.beg-bal    NE 0
               item.beg-date
               item.last-count   WHEN item.last-count NE 0
               item.last-date
               item.last-cost    WHEN item.last-cost  NE 0
               item.avg-cost     WHEN item.avg-cost   NE 0
               head21
               item.q-onh        WHEN item.q-onh      NE 0
               item.q-ono        WHEN item.q-ono      NE 0
               item.q-comm       WHEN item.q-comm     NE 0
               item.q-back       WHEN item.q-back     NE 0
               item.q-avail      WHEN item.q-avail    NE 0.

           IF rd-dest EQ 3 THEN
            PUT STREAM s-temp  
               '"' item.vend-no     '",'
               '"' item.vend-item   '",'
               '"' item.alloc       '",'
               '"' item.vend2-no    '",'
               '"' item.vend2-item  '",'
               '"' item.stocked     '",'
               '"' item.ord-policy  '",'
               '"' item.ord-level   '",'
               '"' item.ord-min     '",'
               '"' item.ord-max     '",'
               '"' item.pur-man     '",'
               '"' item.pur-uom     '",'
               '"' item.lead-days   '",'
               '"' item.loc " " v-loc-descr       '",'
               '"' item.loc-bin     '",'
               '"' item.cc-code     '",'
               '"' item.cons-uom    '",'
               '"' item.pur-rcode   '",'
               '"' item.pic-code    '",'
               '"' item.beg-bal     '",'
               '"' item.beg-date    '",'
               '"' item.last-count  '",'
               '"' item.last-date   '",'
               '"' item.last-cost   '",'
               '"' item.avg-cost    '",'
               '"' item.q-onh       '",'
               '"' item.q-ono       '",'
               '"' item.q-comm      '",'
               '"' item.q-back      '",'
               '"' item.q-avail     '",'
               .
         END. /* end for frame item2 */ */


        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".    

        BUFFER bitem:FIND-BY-ROWID(ROWID(ITEM), NO-LOCK) .

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            IF INDEX(cTmpField,".") > 0 THEN 
            DO:
                cFieldName = cTmpField.
                cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                hField = BUFFER bitem:BUFFER-FIELD(cTmpField).
                IF hField <> ? THEN 
                DO:                 
                    cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).

                    IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) <> "C" THEN 
                    DO:
                        IF LENGTH(TRIM(cTmpField)) < int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))
                            THEN cTmpField = FILL(" ",INT(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(TRIM(cTmpField)) )
                                + TRIM(cTmpField).
                        /*MESSAGE "3n.i:" w-ord.ord-no w-ord.i-no w-ord.onh-qty w-ord.ord-qty SKIP
                            cfieldname ":" cTmpfield LENGTH(trim(cTmpField)) int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                        cTmpField = cTmpField + " ".
                    END.
                    ELSE cTmpField =  cTmpField + 
                            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)).

                    cDisplay = cDisplay + cTmpField.
                    .
                    cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".
                END.
                ELSE 
                DO:
                    cTmpField = SUBSTRING(cFieldName,1,int( ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + quoter(" ")  /*GetFieldValue(hField))*/ + ",".
                END.
            END.
            ELSE 
            DO:       
                CASE cTmpField:                               
                    WHEN "beg-date" THEN 
                        cVarValue = IF ITEM.beg-date NE ? THEN STRING(ITEM.beg-date) ELSE "".
                    WHEN "last-date" THEN 
                        cVarValue = IF ITEM.last-date NE ? THEN STRING(ITEM.last-date) ELSE "" .                
                    WHEN "dept-name[1]" THEN 
                        cVarValue = STRING(item.dept-name[1]).  
                    WHEN "speed%[1]" THEN 
                        cVarValue = STRING(ITEM.speed%[1]).
                    WHEN "dept-name[2]" THEN 
                        cVarValue = STRING(item.dept-name[2]).  
                    WHEN "speed%[2]" THEN 
                        cVarValue = STRING(ITEM.speed%[2]).
                    WHEN "dept-name[3]" THEN 
                        cVarValue = STRING(item.dept-name[3]).  
                    WHEN "speed%[3]" THEN 
                        cVarValue = STRING(ITEM.speed%[3]).
                    WHEN "dept-name[4]" THEN 
                        cVarValue = STRING(item.dept-name[4]).  
                    WHEN "speed%[4]" THEN 
                        cVarValue = STRING(ITEM.speed%[4]).
                    WHEN "dept-name[5]" THEN 
                        cVarValue = STRING(item.dept-name[5]).  
                    WHEN "speed%[5]" THEN 
                        cVarValue = STRING(ITEM.speed%[5]).
                    WHEN "dept-name[6]" THEN 
                        cVarValue = STRING(item.dept-name[6]).  
                    WHEN "speed%[6]" THEN 
                        cVarValue = STRING(ITEM.speed%[6]).
                    WHEN "dept-name[7]" THEN 
                        cVarValue = STRING(item.dept-name[7]).  
                    WHEN "speed%[7]" THEN 
                        cVarValue = STRING(ITEM.speed%[7]).
                    WHEN "dept-name[8]" THEN 
                        cVarValue = STRING(item.dept-name[8]).  
                    WHEN "speed%[8]" THEN 
                        cVarValue = STRING(ITEM.speed%[8]).
                    WHEN "dept-name[9]" THEN 
                        cVarValue = STRING(item.dept-name[9]).  
                    WHEN "speed%[9]" THEN 
                        cVarValue = STRING(ITEM.speed%[9]).
                    WHEN "dept-name[10]" THEN 
                        cVarValue = STRING(item.dept-name[10]).  
                    WHEN "speed%[10]" THEN 
                        cVarValue = STRING(ITEM.speed%[10]).
                    WHEN "ord-policy" THEN 
                        cVarValue = IF item.ord-policy EQ YES THEN "R" ELSE "L" .                
                    WHEN "pur-man" THEN 
                        cVarValue = IF item.pur-man EQ YES THEN "P" ELSE "M" .                
                END CASE.
                IF  cTmpField = "beg-date" THEN
                     cExcelVarValue = IF ITEM.beg-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",ITEM.beg-date) ELSE "".
                ELSE IF  cTmpField = "last-date" THEN
                     cExcelVarValue = IF ITEM.last-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",ITEM.last-date) ELSE "".
                ELSE cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cExcelVarValue)) + ",".            
            END.

        END.

        IF detail THEN 
        DO:
            PUT UNFORMATTED cDisplay .
            IF rd-dest EQ 3 THEN 
            DO:
                PUT STREAM s-temp UNFORMATTED  
                    cExcelDisplay .
            END.

            RUN est-board (ROWID(item)). 
        END.
        ELSE 
        DO: 
            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest EQ 3 THEN 
            DO:
                PUT STREAM s-temp UNFORMATTED  
                    cExcelDisplay SKIP.
            END.
        END.    

    END. /* end for each */
    /* END. /* end for detail */*/
    /*END. /* end for dor */*/


    /*IF doe THEN DO:
       IF rd-dest EQ 3 THEN DO:
          PUT STREAM s-temp 
             "CAT,"
             "ITEM,"       
             "DESCRIPTION,"                    
             "CALIPER WEIGHT,"   
             "<               Valid   Roll   Widths               >"
    
             SKIP.
        END.
    
        FOR EACH ITEM NO-LOCK WHERE 
                 item.company = cocode                                      
             AND item.loc = locode                                          
             AND (item.i-code = "E")                                        
             AND LOOKUP(item.mat-type,"B,P") GT 0                           
             AND item.procat >= fcat                                        
             AND item.procat <= tcat                                        
          BREAK BY item.company                                             
                BY item.loc                                                 
                BY item.i-code                                              
                BY item.mat-type                                            
                BY item.procat                                              
                BY item.i-no WITH FRAME iteme:                              
    
          DISPLAY
             "" @ item.procat
             item.procat when first-of(item.procat)
             item.i-no
             item.i-name
             item.cal
             item.basis-w.
          PUT SKIP.
    
          IF rd-dest EQ 3 THEN DO:
             v-procat = IF first-of(item.procat) THEN item.procat
                     ELSE "".
    
             PUT STREAM s-temp  
                '"' v-procat      '",'
                '"' item.i-no     '",'
                '"' item.i-name   '",'
                '"' item.cal      '",'
                '"' item.basis-w  '"'
             SKIP.
    
          END.
    
          RUN est-board (ROWID(item)).
        END.
    END.*/

    SESSION:SET-WAIT-STATE ("").


    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM s-temp close.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    OUTPUT CLOSE.
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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS cha     NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS cha     NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.

    ASSIGN
        lv-frame-hdl = FRAME {&frame-name}:HANDLE
        lv-group-hdl = lv-frame-hdl:FIRST-CHILD
        lv-field-hdl = lv-group-hdl:FIRST-CHILD.

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
            THEN 
        DO:
            IF lv-field-hdl:LABEL <> ? THEN 
                ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + ",".
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    .
                lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN 
                    DO:
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
            entry(i,parm-lbl-list) NE "" THEN 
        DO:

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
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

