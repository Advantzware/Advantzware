&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-comprm.w

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

DEF VAR v-per-rpt   AS   LOG FORMAT "PTD/YTD" INIT YES.
DEF VAR v-period    AS   INT INIT 1.
DEF VAR v-cat       LIKE itemfg.procat.
DEF VAR v-sman      AS   CHAR FORMAT "x(3)" EXTENT 2 INIT ["", "zzz"].
DEF VAR v-date      AS   DATE EXTENT 2 INIT [01/01/01, TODAY].
DEF VAR v-cust      AS   CHARACTER EXTENT 2 INIT ["", "zzzzzzzz"].
DEF VAR v-sumdet    AS   LOG FORMAT "Summary/Detail" INIT YES.
DEF VAR v-cost1     AS   CHAR.
DEF VAR v-year      AS   INTEGER.

DEF TEMP-TABLE w-comm NO-UNDO
   FIELD sman    AS   CHAR
   FIELD samt    LIKE ar-invl.amt
   FIELD camt    LIKE ar-invl.amt
   FIELD cost    LIKE ar-invl.amt.

{ oerep/tt-comm-calc.i "NEW SHARED" }

/* DEF VAR v-head          AS CHAR FORMAT "x(200)" EXTENT 4. */
DEF VAR v-exp-head      AS CHAR FORM "x(132)" NO-UNDO.
DEF VAR v-comma         AS CHAR FORM "x" INIT "," NO-UNDO.
DEF VAR v-part-fg       LIKE ar-invl.part-no NO-UNDO.
DEF VAR v-print-fmt     AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form  AS LOGICAL.
DEF VAR ls-fax-file     AS CHAR NO-UNDO.
DEF STREAM st-excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "Sman,Customer,Cust. Name,Inv No.,FG Number,FG Cat.," +
                           "Inv Qty,Inv Price,UOM,SetSales Price," + 
                           "Comm Cost,Total Item Res,Total Item Cost,Margin%,Profit Margin$,Comm"
       cFieldListToSelect = "rep,cust,cust-nam,inv-no,i-no,cat," +
                            "inv-qty,inv-pric,uom,set-sal," +
                            "com-cst,ttl-itm,ttl-itm-cst,marg%,marg$,com"
       cFieldLength = "4,8,22,7,15,7," + "11,14,3,14," + "14,14,15,8,14,8" 
       cFieldType = "c,c,c,i,c,c," + "i,i,c,i," + "i,i,i,i,i,i"
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Sman,Customer,Cust. Name,Inv No.,FG Number,FG Cat.," +
                           "Inv Qty,Inv Price,UOM,SetSales Price," + 
                           "Comm Cost,Total Item Res,Total Item Cost,Margin%,Profit Margin$,Comm" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_ptd begin_period begin_date ~
end_date begin_slsmn end_slsmn begin_cust-no end_cust-no fg-cat tb_detailed ~
tb_prep tb_inv-costs lbl_cost rd_cost1 rd_part-fg sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down lv-ornt lines-per-page ~
rd-dest lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lbl_ptd rd_ptd begin_period begin_date ~
end_date begin_slsmn end_slsmn begin_cust-no end_cust-no fg-cat tb_detailed ~
tb_prep tb_inv-costs lbl_cost rd_cost1 lbl_part-fg rd_part-fg sl_avail ~
sl_selected lv-ornt lines-per-page rd-dest lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "From Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE begin_period AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "For Period?" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fg-cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "For Category" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-comms2.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_cost AS CHARACTER FORMAT "X(256)":U INITIAL "Cost?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_part-fg AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_ptd AS CHARACTER FORMAT "X(256)":U INITIAL "PTD / YTD?" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

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
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_cost1 AS CHARACTER INITIAL "FG" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "FG", "FG",
"Estimated Board", "Estimated Board",
"Order", "Order"
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE rd_part-fg AS CHARACTER INITIAL "FG Item#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer Part#", "Cust Part#",
"FG Item#", "FG Item#"
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE rd_ptd AS CHARACTER INITIAL "PTD" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "PTD", "PTD",
"YTD", "YTD"
     SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.71.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_inv-costs AS LOGICAL INITIAL no 
     LABEL "Use Invoice Costs?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prep AS LOGICAL INITIAL no 
     LABEL "Show Prep Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     lbl_ptd AT ROW 2.19 COL 10 COLON-ALIGNED NO-LABEL
     rd_ptd AT ROW 2.19 COL 26 NO-LABEL
     begin_period AT ROW 3.38 COL 24 COLON-ALIGNED
     begin_date AT ROW 4.57 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 4.57 COL 67 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_slsmn AT ROW 5.52 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 5.52 COL 67 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_cust-no AT ROW 6.48 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 6.48 COL 67 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     fg-cat AT ROW 7.43 COL 24 COLON-ALIGNED HELP
          "Enter Category, or leave blank for all"
     tb_detailed AT ROW 8.62 COL 26
     tb_prep AT ROW 8.67 COL 41.8
     tb_inv-costs AT ROW 9.57 COL 26 WIDGET-ID 2
     lbl_cost AT ROW 9.57 COL 24 COLON-ALIGNED NO-LABEL
     rd_cost1 AT ROW 9.57 COL 34 NO-LABEL
     lbl_part-fg AT ROW 10.62 COL 24 COLON-ALIGNED NO-LABEL
     rd_part-fg AT ROW 10.62 COL 34 NO-LABEL
     sl_avail AT ROW 12.52 COL 4.6 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 12.52 COL 40.8 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 12.52 COL 60 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 13.52 COL 40.8 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 14.52 COL 40.8 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 15.57 COL 40.8 WIDGET-ID 40
     btn_down AT ROW 16.57 COL 40.8 WIDGET-ID 42
     lv-ornt AT ROW 18.62 COL 31 NO-LABEL
     lines-per-page AT ROW 18.62 COL 84 COLON-ALIGNED
     rd-dest AT ROW 19.05 COL 6 NO-LABEL
     lv-font-no AT ROW 19.86 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 20.81 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.29 COL 30
     tb_excel AT ROW 23.86 COL 67.8 RIGHT-ALIGNED
     tb_runExcel AT ROW 23.86 COL 89.8 RIGHT-ALIGNED
     fi_file AT ROW 24.71 COL 45.8 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 27.05 COL 19
     btn-cancel AT ROW 27.05 COL 57
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11.81 COL 5.4 WIDGET-ID 38
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 18.1 COL 4
     "(Leave Blank For all Categories)" VIEW-AS TEXT
          SIZE 31 BY .71 AT ROW 7.67 COL 46
          FGCOLOR 1 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.76 COL 59.6 WIDGET-ID 44
     RECT-6 AT ROW 17.81 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 27.57.


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
         TITLE              = "Commission Report"
         HEIGHT             = 27.81
         WIDTH              = 95.4
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fg-cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       lbl_cost:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_cost".

/* SETTINGS FOR FILL-IN lbl_part-fg IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_part-fg:HIDDEN IN FRAME FRAME-A           = TRUE
       lbl_part-fg:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_part-fg".

/* SETTINGS FOR FILL-IN lbl_ptd IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_ptd:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_ptd".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_cost1:HIDDEN IN FRAME FRAME-A           = TRUE
       rd_cost1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_part-fg:HIDDEN IN FRAME FRAME-A           = TRUE
       rd_part-fg:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_ptd:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_detailed:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_inv-costs:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_inv-costs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prep:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_prep:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Commission Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Commission Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_period C-Win
ON LEAVE OF begin_period IN FRAME FRAME-A /* For Period? */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN show-period-dates NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   apply "close" to this-procedure.
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
  run run-report.
  STATUS DEFAULT "Processing Complete". 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= "Customer "
                            &begin_cust= "begin_cust-no"
                            &END_cust= "begin_cust-no" 
                            &fax-subject=c-win:TITLE
                            &fax-body=c-win:TITLE
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Customer "
                             &begin_cust= "begin_cust-no"
                             &END_cust= "begin_cust-no"
                             &mail-subject=c-win:TITLE
                             &mail-body=c-win:TITLE
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer "
                                  &begin_cust="begin_cust-no"
                                  &END_cust="begin_cust-no"
                                  &mail-subject=c-win:TITLE
                                  &mail-body=c-win:TITLE
                                  &mail-file=list-name }

           END.
       END.
      WHEN 6 THEN RUN OUTPUT-TO-PORT.
  end case. 
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


&Scoped-define SELF-NAME rd_part-fg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_part-fg C-Win
ON VALUE-CHANGED OF rd_part-fg IN FRAME FRAME-A
DO:
  assign {&self-name}.

  if rd_ptd eq "YTD" then do:
    find first period
        where period.company eq gcompany
          and period.yr      eq v-year
        no-lock no-error.

    begin_date = if avail period then period.pst
                                 else date(1,1,year(today)).

    display begin_date WITH FRAME FRAME-A IN WINDOW C-Win.
  end.

  run show-period-dates.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_ptd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_ptd C-Win
ON VALUE-CHANGED OF rd_ptd IN FRAME FRAME-A
DO:
  assign {&self-name}.

  if rd_ptd eq "YTD" then do:
    find first period
        where period.company eq gcompany
          and period.yr      eq v-year
        no-lock no-error.

    begin_date = if avail period then period.pst
                                 else date(1,1,year(today)).

    display begin_date WITH FRAME FRAME-A IN WINDOW C-Win.
  end.

  run show-period-dates.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
DO:
  IF {&self-name}:SCREEN-VALUE EQ "Yes" THEN
    rd_part-fg:SENSITIVE = YES.
  ELSE
    rd_part-fg:SENSITIVE = NO.
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


&Scoped-define SELF-NAME tb_inv-costs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inv-costs C-Win
ON VALUE-CHANGED OF tb_inv-costs IN FRAME FRAME-A /* Use Invoice Costs? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prep C-Win
ON VALUE-CHANGED OF tb_prep IN FRAME FRAME-A /* Show Prep Charges? */
DO:
  IF {&self-name}:SCREEN-VALUE EQ "Yes" THEN
    rd_part-fg:SENSITIVE = YES.
  ELSE
    rd_part-fg:SENSITIVE = NO.
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

  FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  IF NOT CAN-FIND(FIRST asi._file WHERE asi._file._FILE-NAME = "item-comm") THEN DO:
    MESSAGE "Table 'ITEM-COMM' is not in the database."
       VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY "close" TO THIS-PROCEDURE.
    RETURN.
  END. 

  ASSIGN
   begin_date = TODAY
   end_date   = TODAY.

  find first period
      where period.company eq gcompany
        and period.pst     le today
        and period.pend    ge today
        and period.pstat
      no-lock no-error.

  if available period then
    assign
     begin_period = period.pnum
     v-year       = period.yr
     begin_date   = period.pst.

  else
    assign
     begin_period = month(today)
     v-year       = year(today).

  fi_file = "c:\tmp\commprm.csv".

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    rd_part-fg:SENSITIVE = NO.
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO rd_ptd.

    FIND FIRST period
        WHERE period.company EQ gcompany
          AND period.pst     LE DATE(begin_date:SCREEN-VALUE)
          AND period.pend    GE DATE(end_date:SCREEN-VALUE)
          AND period.pstat
        NO-LOCK NO-ERROR.
    v-year = IF AVAIL period THEN period.yr ELSE YEAR(TODAY).

    IF AVAIL oe-ctrl AND NOT oe-ctrl.prep-comm THEN
      ASSIGN
       tb_prep:SCREEN-VALUE = "NO"
       tb_prep:SENSITIVE    = NO.
    ASSIGN
       tb_detailed:HIDDEN = TRUE 
       tb_prep:HIDDEN = TRUE
       lbl_cost:HIDDEN = TRUE
       lbl_part-fg:HIDDEN = TRUE
       rd_cost1:HIDDEN = TRUE
       rd_part-fg:HIDDEN = TRUE.
  END.

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
  DISPLAY lbl_ptd rd_ptd begin_period begin_date end_date begin_slsmn end_slsmn 
          begin_cust-no end_cust-no fg-cat tb_detailed tb_prep lbl_cost rd_cost1 
          lbl_part-fg rd_part-fg tb_inv-costs lv-ornt lines-per-page rd-dest 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file sl_avail sl_selected
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 rd_ptd begin_period begin_date end_date begin_slsmn 
         end_slsmn begin_cust-no end_cust-no fg-cat tb_detailed tb_prep 
         lbl_cost rd_cost1 rd_part-fg tb_inv-costs lv-ornt lines-per-page 
         rd-dest lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok 
         btn-cancel sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up btn_down
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE old-print C-Win 
PROCEDURE old-print :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*{sys/form/r-top3w.f}

def var v-frst      as   log extent 2.
/* def var p-sman      as   char format "x(3)".                 */
/* def var v-camt      like ar-invl.amt.                        */
/* def var v-prof      like ar-invl.amt.                        */
def var v-comm      as   dec format ">>9.99".
/* def var v-gp        as   dec format ">>9.99".                */
/* def var v-slsm      like ar-invl.sman extent 1.              */
/* def var v-slsc      like ar-invl.s-comm extent 1.            */
/* def var v-slsp      like ar-invl.s-pct extent 1.             */
/* def var v-inv-no    like ar-invl.inv-no.                     */
/* def var v-procat    like itemfg.procat.                      */
/* def var v-qty       as   dec.                                */
/* def var v-amt       like ar-invl.amt.                        */
def var v-cost      like ar-invl.t-cost.
/* def var v-cust-part like ar-invl.part-no no-undo.            */
/* def var v-ord-no    like ar-invl.ord-no.                     */
/* def var v-job-no    like job.job-no.                         */
/* def var v-job-no2   like job.job-no2.                        */
/* def var v-i-no      like ar-invl.i-no.                       */
/* DEF VAR v-basis     LIKE sman.commbasis NO-UNDO.             */
/* DEF VAR ld-inv-pct  AS   DEC NO-UNDO.                        */
/* DEF VAR ld-csh-pct  AS   DEC NO-UNDO.                        */
/* DEF VAR ll-comp     AS   LOG NO-UNDO.                        */

def var v-tot-samt  as   dec format "->>>>>>>9.99" extent 3.
def var v-tot-camt  as   dec format "->>>>>>>9.99" extent 3.
def var v-tot-cost  as   dec format "->>>>>>>9.99" extent 3.

def var v-head      as   character format "x(200)" extent 4.
/* DEF VAR v-exp-head AS cha FORM "x(132)" NO-UNDO.             */
/* DEF VAR v-comma AS cha FORM "x" INIT "," NO-UNDO.            */
/* DEF VAR v-part-fg LIKE v-cust-part NO-UNDO.                  */



FORMAT HEADER
   v-head[1] SKIP
   v-head[2] SKIP
   v-head[3] SKIP
   v-head[4]
WITH FRAME r-top WIDTH 200.

ASSIGN
   str-tit3 = (IF v-per-rpt THEN "P" ELSE "Y") +
              "TD (" + STRING(v-date[1]) + "-" + STRING(v-date[2]) +
              ") - By Sales Rep By Customer"

{sys/inc/ctrtext.i str-tit3 132}

v-head[1] = "".

/* IF v-sumdet THEN                                                                                                                                                  */
/*    ASSIGN                                                                                                                                                         */
/*       v-head[2] = "                                      Total "                                                                                                  */
/*       v-head[3] = "Customer  Name                         InvoicePrice $   Total Set Price $    Comm Cost % Item Resale $ Item Cost $ Profit Margin $ Commission" */
/*       v-head[4] = fill("-",112)                                                                                                                                   */
/*       v-exp-head =  "Sales Rep,Customer,Name,Total Sales $, Comm $, Comm %, Cost $, GP %".                                                                         */
/*                                                                                                                                                                   */
/* ELSE                                                                                                                                                              */
   ASSIGN                                                                                                                              
      v-head[2] = fill(" ",36)  +                     "Invoice                  FG                  Invoice         Sell   Commission   Total Item   Total Item   Profit       Profit  "
      v-head[3] = "Customer Name                        Number FG Number       Cat.        QTY        Price        Price         Cost     Resale        Cost      Margin%      Margin$   Comm."

      v-head[4] = fill("-",138)
      v-exp-head = "Sman,Customer,Name," + TRIM(rd_part-fg) + ",Order#,Inv#," +
                   "Cat,Quantity,Sell Price,Total Cost,GP %,Comm Amt,Comm Pct".

 DISPLAY "" WITH frame r-top.
/*  IF tb_excel THEN PUT STREAM st-excell v-exp-head SKIP. */

FOR EACH tt-report,
   FIRST cust WHERE cust.company EQ cocode
                AND cust.cust-no EQ tt-report.key-02 NO-LOCK
   BREAK BY tt-report.key-01
         BY tt-report.key-02
         BY tt-report.key-03
         BY tt-report.row-id
         BY tt-report.key-10
         BY tt-report.rec-id
         BY ROWID(tt-report):

/*    IF NOT v-sumdet THEN DO: */
      DISPLAY
         tt-report.key-01  WHEN FIRST-OF(tt-report.key-01) FORMAT "x(3)"
         SPACE(2)
         tt-report.key-02  WHEN FIRST-OF(tt-report.key-02)
         cust.name         WHEN FIRST-OF(tt-report.key-02) FORMAT "x(22)"
         tt-report.inv-no
         tt-report.part-fg
         tt-report.procat
         tt-report.qty              FORMAT "->>>>>>>9"
         tt-report.amt              FORMAT "->>>>>>>9.99"
         tt-report.ordl-sell-price  FORMAT "->>>>>>>9.99"
         tt-report.commission-cost  FORMAT "->>>>>>>9.99"
         tt-report.tot-item-resale  FORMAT "->>>>>>>9.99"
         tt-report.total-costs      FORMAT "->>>>>>>9.99"
         tt-report.profit-margin-percent   FORMAT "->>>9.99"
         tt-report.profit-margin    FORMAT "->>>>>>>9.99"
         tt-report.commission       FORMAT "->>>9.99"
      WITH FRAME detail NO-BOX NO-LABELS STREAM-IO WIDTH 200.

/*       IF tb_excel THEN                                     */
/*          PUT STREAM                                        */
/*             st-excell tt-report.key-01 FORM "x(3)" v-comma */
/*             tt-report.key-02   v-comma                     */
/*             REPLACE(cust.NAME,',','') v-comma              */
/*             v-part-fg v-comma                              */
/*             v-ord-no  v-comma                              */
/*             v-inv-no  v-comma                              */
/*             v-procat  v-comma                              */
/*             v-qty     FORMAT "->>>>>>>9"        v-comma    */
/*             v-amt     FORMAT "->>>>>>>9.99"     v-comma    */
/*             v-cost    FORMAT "->>>>>>>9.99"     v-comma    */
/*             v-gp      FORMAT "->>>>9.99"        v-comma    */
/*             v-camt    FORMAT "->>>>>9.99"       v-comma    */
/*             v-comm    FORMAT "->>>9.99"                    */
/*             SKIP.                                          */


   IF LAST-OF(tt-report.key-02) THEN DO:
      ASSIGN
         v-comm = v-tot-camt[1] / v-tot-samt[1] * 100
         v-cost = (v-tot-samt[1] - v-tot-cost[1]) / v-tot-samt[1] * 100.
/*          v-gp   = round((v-tot-samt[1] - v-tot-cost[1]) / v-tot-samt[1] * 100,2). */

      IF v-comm = ? THEN v-comm = 0.
      IF v-cost = ? THEN v-cost = 0.
/*       IF v-gp   = ? THEN v-gp   = 0. */

      IF v-sumdet THEN DO:

/*          DISPLAY                                                    */
/*             p-sman            FORMAT "x(3)"                         */
/*             SPACE(7)                                                */
/*             tt-report.key-02                                        */
/*             SPACE(2)                                                */
/*             cust.name                                               */
/*             SPACE(2)                                                */
/*             v-tot-samt[1]                                           */
/*             SPACE(2)                                                */
/*             v-tot-camt[1]                                           */
/*             SPACE(2)                                                */
/*             v-comm            FORMAT "->>>9.99"                     */
/*             SPACE(2)                                                */
/*             v-tot-cost[1]                                           */
/*             SPACE(2)                                                */
/*             v-cost            FORMAT "->>>9.99"                     */
/*            WITH FRAME SUMMARY NO-BOX NO-LABELS STREAM-IO WIDTH 200. */
/*                                                                     */
/*          IF tb_excel THEN                                           */
/*             PUT STREAM st-excell                                    */
/*                p-sman      FORMAT "x(3)" v-comma                    */
/*                tt-report.key-02 v-comma                             */
/*                cust.NAME  v-comma                                   */
/*                v-tot-samt[1] v-comma                                */
/*                v-tot-camt[1] v-comma                                */
/*                v-comm      FORMAT "->>>9.99" v-comma                */
/*                v-tot-cost[1] v-comma                                */
/*                v-cost      FORMAT "->>>9.99"                        */
/*                SKIP.                                                */
      END.
      ELSE DO:
         FIND FIRST w-comm WHERE w-comm.sman EQ tt-report.key-01 NO-ERROR.

         IF NOT AVAIL w-comm THEN DO:
            CREATE w-comm.
            w-comm.sman = tt-report.key-01.
         END.

         ASSIGN
            w-comm.samt = w-comm.samt + v-tot-samt[1]
            w-comm.camt = w-comm.camt + v-tot-camt[1]
            w-comm.cost = w-comm.cost + v-tot-cost[1].

/*          IF (NOT FIRST-OF(tt-report.key-01)) AND (NOT FIRST-OF(tt-report.key-02)) THEN DO: */
/*                                                                                            */
/*             DOWN WITH FRAME detail.                                                        */
/*                                                                                            */
/*             PUT SKIP(1).                                                                   */
/*                                                                                            */
/*             DISPLAY "      Customer Totals:"    @ cust.name                                */
/*                v-tot-samt[1]               @ v-amt                                         */
/*                v-tot-cost[1]               @ v-cost                                        */
/*                v-gp                                                                        */
/*                v-tot-camt[1]               @ v-camt                                        */
/*                v-comm                                                                      */
/*             WITH FRAME detail.                                                             */
/*                                                                                            */
/*             PUT SKIP(1).                                                                   */
/*          END.                                                                              */
/*          ELSE                                                                              */
/*             DISPLAY SKIP(1) WITH FRAME skip-a-line NO-BOX NO-LABELS STREAM-IO.             */
      END.

      ASSIGN
         p-sman        = ""
         v-tot-samt[2] = v-tot-samt[2] + v-tot-samt[1]
         v-tot-camt[2] = v-tot-camt[2] + v-tot-camt[1]
         v-tot-cost[2] = v-tot-cost[2] + v-tot-cost[1]
         v-tot-samt[1] = 0
         v-tot-camt[1] = 0
         v-tot-cost[1] = 0.
   END. /* IF LAST-OF(tt-report.key-02) */

   IF LAST-OF(tt-report.key-01) THEN DO:
      ASSIGN
         v-comm = v-tot-camt[2] / v-tot-samt[2] * 100
         v-cost = (v-tot-samt[2] - v-tot-cost[2]) / v-tot-samt[2] * 100.
/*          v-gp   = ROUND((v-tot-samt[2] - v-tot-cost[2]) / v-tot-samt[2] * 100,2). */

      IF v-comm = ? THEN v-comm = 0.
      IF v-cost = ? THEN v-cost = 0.
/*       IF v-gp   = ? THEN v-gp   = 0. */

/*       IF ((NOT v-frst[2]) AND (NOT LAST(tt-report.key-01))) OR ((NOT v-frst[1]) AND LAST(tt-report.key-01)) THEN */
/*          IF v-sumdet THEN DO:                                                                                    */
/*             DISPLAY                                                                                              */
/*                SKIP(1)                                                                                           */
/*                space(5)                                                                                          */
/*                "SalesRep Totals:"                                                                                */
/*                space(31)                                                                                         */
/*                v-tot-samt[2]                                                                                     */
/*                space(2)                                                                                          */
/*                v-tot-camt[2]                                                                                     */
/*                space(2)                                                                                          */
/*                v-comm            FORMAT "->>>9.99"                                                               */
/*                space(2)                                                                                          */
/*                v-tot-cost[2]                                                                                     */
/*                space(2)                                                                                          */
/*                v-cost            FORMAT "->>>9.99"                                                               */
/*                skip(1)                                                                                           */
/*             WITH FRAME salesman-sum NO-BOX NO-LABELS STREAM-IO WIDTH 200.                                        */
/*                                                                                                                  */
/*          END.                                                                                                    */
/*          ELSE DO:                                      */
/*             DOWN WITH FRAME detail.                    */
/*             PUT SKIP(1).                               */
/*             DISPLAY                                    */
/*                "      Sales Rep Totals:"    @ cust.name */
/*                v-tot-samt[2]               @ v-amt     */
/*                v-tot-cost[2]               @ v-cost    */
/*                v-gp                                    */
/*                v-tot-camt[2]               @ v-camt    */
/*                v-comm                                  */
/*            WITH FRAME detail.                          */
/*          END.                                          */


      ASSIGN
         v-frst[1]     = NO
         v-tot-samt[3] = v-tot-samt[3] + v-tot-samt[2]
         v-tot-camt[3] = v-tot-camt[3] + v-tot-camt[2]
         v-tot-cost[3] = v-tot-cost[3] + v-tot-cost[2]
         v-tot-samt[2] = 0
         v-tot-camt[2] = 0
         v-tot-cost[2] = 0.
   END.

   IF LAST-OF(tt-report.key-02) THEN v-frst[2] = NO.

/*    DELETE tt-report. */
/* END. /* input-work */ */



IF NOT v-sumdet THEN DO:
/*    ASSIGN                                                                         */
/*       str-tit3 = (IF v-per-rpt THEN "P" ELSE "Y") +                               */
/*                   "TD (" + STRING(v-date[1]) + "-" + STRING(v-date[2]) +          */
/*                   ") - By Sales Rep"                                               */
/*       {sys/inc/ctrtext.i str-tit3 132}                                            */
/*       v-head[2] = "SalesRep                                           Total Sa" + */
/*                   "les $        Comm $    Comm %        Cost $      GP %"         */
/*       v-head[3] = FILL("-",112).                                                  */
/*    PAGE.                                                                          */

   ASSIGN
      v-tot-samt[3] = 0
      v-tot-camt[3] = 0
      v-tot-cost[3] = 0.

   recap-work:
   FOR EACH w-comm
      BREAK BY w-comm.sman:

      ASSIGN
         v-comm = w-comm.camt / w-comm.samt * 100
         v-cost = (w-comm.samt - w-comm.cost) / w-comm.samt * 100

         v-tot-samt[3] = v-tot-samt[3] + w-comm.samt
         v-tot-camt[3] = v-tot-camt[3] + w-comm.camt
         v-tot-cost[3] = v-tot-cost[3] + w-comm.cost.

      IF v-comm = ? THEN v-comm = 0.
      IF v-cost = ? THEN v-cost = 0.

/*       DISPLAY                                                       */
/*          w-comm.sman FORMAT "x(3)"                                  */
/*          SPACE(49)                                                  */
/*          w-comm.samt FORMAT "->>>>>>>9.99"                          */
/*          SPACE(2)                                                   */
/*          w-comm.camt FORMAT "->>>>>>>9.99"                          */
/*          SPACE(2)                                                   */
/*          v-comm      FORMAT "->>>9.99"                              */
/*          SPACE(2)                                                   */
/*          w-comm.cost FORMAT "->>>>>>>9.99"                          */
/*          SPACE(2)                                                   */
/*          v-cost      FORMAT "->>>9.99"                              */
/*       WITH FRAME salesman-det NO-BOX NO-LABELS STREAM-IO WIDTH 200. */

   END.  /* recap-work */
END. /* IF NOT v-sumdet */

ASSIGN
   v-comm = v-tot-camt[3] / v-tot-samt[3] * 100.
   v-cost = (v-tot-samt[3] - v-tot-cost[3]) / v-tot-samt[3] * 100.

IF v-comm = ? THEN v-comm = 0.
IF v-cost = ? THEN v-cost = 0.

/* DISPLAY                                                    */
/*    SKIP(1)                                                 */
/*    "Grand Totals:"                                         */
/*    SPACE(39)                                               */
/*    v-tot-samt[3]                                           */
/*    SPACE(2)                                                */
/*    v-tot-camt[3]                                           */
/*    SPACE(2)                                                */
/*    v-comm            FORMAT "->>>9.99"                     */
/*    SPACE(2)                                                */
/*    v-tot-cost[3]                                           */
/*    SPACE(2)                                                */
/*    v-cost            FORMAT "->>>9.99"                     */
/* WITH FRAME grand-tot NO-BOX NO-LABELS STREAM-IO WIDTH 200. */
END.
 */
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
  /*   DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY.
     */
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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-report C-Win 
PROCEDURE print-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*{sys/form/r-top3w.f}*/

def var v-frst      as   log extent 2.
def var v-comm      as   dec format ">>9.99".
def var v-cost      like ar-invl.t-cost.
def var v-tot-samt  as   dec format "->>>>>>>9.99" extent 3.
def var v-tot-camt  as   dec format "->>>>>>>9.99" extent 3.
def var v-tot-cost  as   dec format "->>>>>>>9.99" extent 3.
def var v-head      as   character format "x(200)" extent 4.
DEF VAR v-slsm      LIKE tt-comm-calc.slsm.
DEF VAR v-cust-no   LIKE tt-comm-calc.cust-no.
DEF VAR v-cust-name LIKE cust.name.
DEF VAR v-set-sal AS DEC NO-UNDO.
DEF VAR v-com-cst AS DEC NO-UNDO .

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-line AS cha FORM "x(300)" NO-UNDO.
{sys/form/r-top5L3.f}
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF VAR excelheader AS CHAR NO-UNDO.
/*
FORMAT HEADER
   v-head[1] SKIP
   v-head[2] SKIP
   v-head[3] SKIP
   v-head[4]
WITH FRAME r-top WIDTH 200.*/

ASSIGN
   str-tit3 = (IF v-per-rpt THEN "P" ELSE "Y") +
              "TD (" + STRING(v-date[1]) + "-" + STRING(v-date[2]) +
              ") - By Sales Rep By Customer"

{sys/inc/ctrtext.i str-tit3 132}

v-head[1] = "".

   ASSIGN                                                                                                                              
      v-head[2] = fill(" ",36)  +                     "Invoice                  FG                      Invoice      Set Sales     Commission     Total Item     Total Item  Profit         Profit  "
      v-head[3] = "SMAN Customer Name                   Number FG Number       Cat.          QTY          Price          Price           Cost       Resale          Cost     Margin%        Margin$     Comm."

      v-head[4] = fill("-",185).
/*       v-exp-head = "Sman,Customer,Name," + ",Order#,Inv#," +                    */
/*                    "Cat,Quantity,Sell Price,Total Cost,GP %,Comm Amt,Comm Pct". */


/*IF tb_excel THEN DO:
   OUTPUT STREAM st-excel TO VALUE(fi_file).
   PUT STREAM st-excel UNFORMATTED
   "Salesman,Customer,Cust. Name,Invoice Number,FG Number,FG Cat.,Invoice Qty,Invoice Price,UOM,Set Sales Price,Commission Cost,Total Item Resale,Total Item Cost,Profit Margin$,Commission"
   SKIP.
END. */

DEF VAR cslist AS cha NO-UNDO.
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
          cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "TOTAL VALUE") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

IF tb_excel THEN DO:
  OUTPUT STREAM st-excel TO VALUE(fi_file).
  PUT STREAM st-excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

 DISPLAY "" WITH frame r-top.

ASSIGN 
    v-set-sal = 0 
    v-com-cst = 0 .

FOR EACH tt-comm-calc,
   FIRST cust WHERE cust.company EQ cocode
                AND cust.cust-no EQ tt-comm-calc.cust-no NO-LOCK
   BREAK BY tt-comm-calc.slsm[1]
         BY tt-comm-calc.cust-no
         BY tt-comm-calc.inv-no:

   {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

     /* DISPLAY
         tt-comm-calc.slsm[1]  WHEN FIRST-OF(tt-comm-calc.slsm[1]) FORMAT "x(3)"
         SPACE(2)
         tt-comm-calc.cust-no  WHEN FIRST-OF(tt-comm-calc.cust-no)
         cust.name         WHEN FIRST-OF(tt-comm-calc.cust-no) FORMAT "x(22)"
         tt-comm-calc.inv-no
         tt-comm-calc.i-no
         tt-comm-calc.procat
         tt-comm-calc.qty              FORMAT "->>,>>>,>>9"
         tt-comm-calc.amt              FORMAT "->>,>>>,>>9.99"
         tt-comm-calc.set-sales-price  * tt-comm-calc.set-sell-price-qty FORMAT "->>,>>>,>>9.99"
         tt-comm-calc.commission-cost  * tt-comm-calc.base-cost-qty      FORMAT "->>,>>>,>>9.99"
         tt-comm-calc.tot-item-resale  FORMAT "->>,>>>,>>9.99"
         tt-comm-calc.total-costs      FORMAT "->>,>>>,>>9.99"
         tt-comm-calc.profit-margin-percent   FORMAT "->>>9.99"
         tt-comm-calc.profit-margin    FORMAT "->>,>>>,>>9.99"
         tt-comm-calc.commission       FORMAT "->>>9.99"
      WITH FRAME detail NO-BOX NO-LABELS STREAM-IO WIDTH 200.


      IF tb_excel THEN DO:
         IF FIRST-OF(tt-comm-calc.slsm[1]) THEN
            v-slsm = tt-comm-calc.slsm[1].
         ELSE
            v-slsm = "".
         IF FIRST-OF(tt-comm-calc.cust-no) THEN
            ASSIGN
               v-cust-no = tt-comm-calc.cust-no
               v-cust-name = cust.name.
         ELSE
            ASSIGN
               v-cust-no = ""
               v-cust-name = "".

         PUT STREAM st-excel UNFORMATTED
            '"' v-slsm                                                  '",'
            '"' v-cust-no                                               '",'
            '"' v-cust-name                                             '",'
            '"' tt-comm-calc.inv-no                                     '",'
            '"' tt-comm-calc.i-no                                    '",'
            '"' tt-comm-calc.procat                                     '",'
            '"' tt-comm-calc.qty     FORMAT "->>,>>>,>>9"      '",'
            '"' tt-comm-calc.amt              FORMAT "->>,>>>,>>9.99"   '",'
            '"' tt-comm-calc.uom                                        '",'
            '"' tt-comm-calc.set-sales-price * tt-comm-calc.set-sell-price-qty FORMAT "->>,>>>,>>9.99"   '",'
            '"' tt-comm-calc.commission-cost * tt-comm-calc.base-cost-qty      FORMAT "->>,>>>,>>9.99"   '",'
            '"' tt-comm-calc.tot-item-resale  FORMAT "->>,>>>,>>9.99"   '",'
            '"' tt-comm-calc.total-costs      FORMAT "->>,>>>,>>9.99"   '",'
            '"' tt-comm-calc.profit-margin    FORMAT "->>,>>>,>>9.99"   '",'
            '"' tt-comm-calc.commission       FORMAT "->>>9.99"         '",'
            SKIP.*/

            ASSIGN v-set-sal = tt-comm-calc.set-sales-price * tt-comm-calc.set-sell-price-qty 
                   v-com-cst = tt-comm-calc.commission-cost * tt-comm-calc.base-cost-qty      .

            ASSIGN cDisplay = ""
                    cTmpField = ""
                    cVarValue = ""
                    cExcelDisplay = ""
                    cExcelVarValue = "".

             DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                     CASE cTmpField:             
                          WHEN "rep"           THEN cVarValue = STRING(tt-comm-calc.slsm[1]) .  
                          WHEN "cust"          THEN cVarValue = STRING(tt-comm-calc.cust-no,"x(8)")   .   
                          WHEN "cust-nam"      THEN cVarValue = string(cust.NAME,"x(22)")   .                
                          WHEN "inv-no"        THEN cVarValue = STRING(tt-comm-calc.inv-no).
                          WHEN "i-no"          THEN cVarValue = string(tt-comm-calc.i-no) .              
                          WHEN "cat"           THEN cVarValue = string(tt-comm-calc.procat)  .                           
                          WHEN "inv-qty"       THEN cVarValue = string(tt-comm-calc.qty,"->>,>>>,>>9") .                          
                          WHEN "inv-pric"      THEN cVarValue = STRING(tt-comm-calc.amt,"->>,>>>,>>9.99").     
                          WHEN "uom"           THEN cVarValue = STRING(tt-comm-calc.uom).
                          WHEN "set-sal"       THEN cVarValue = STRING(v-set-sal,"->>,>>>,>>9.99")   .  
                          WHEN "com-cst"       THEN cVarValue = string(v-com-cst,"->>,>>>,>>9.99") .              
                          WHEN "ttl-itm"       THEN cVarValue = string(tt-comm-calc.tot-item-resale,"->>,>>>,>>9.99")  .                           
                          WHEN "ttl-itm-cst"   THEN cVarValue = string(tt-comm-calc.total-costs,"->>,>>>,>>9.99") .                          
                          WHEN "marg%"         THEN cVarValue = STRING(tt-comm-calc.profit-margin-percent,"->>>9.99").     
                          WHEN "marg$"         THEN cVarValue = STRING(tt-comm-calc.profit-margin,"->>,>>>,>>9.99").
                          WHEN "com"           THEN cVarValue = STRING(tt-comm-calc.commission,"->>>9.99")   .  

                     END CASE.

                     cExcelVarValue = cVarValue.
                     cDisplay = cDisplay + cVarValue +
                                FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                     cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
             END.

             PUT UNFORMATTED cDisplay SKIP.
             IF tb_excel THEN DO:
                  PUT STREAM st-excel UNFORMATTED  
                        cExcelDisplay SKIP.
              END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
{sys/form/r-top3w.f}

DEF BUFFER b-ar-invl  FOR ar-invl.
DEF BUFFER b-ar-cashl FOR ar-cashl.

DEF VAR v-slsm      LIKE ar-invl.sman EXTENT 1.
DEF VAR v-slsc      LIKE ar-invl.s-comm EXTENT 1.
DEF VAR v-frst      AS LOG EXTENT 2 NO-UNDO.
DEF VAR ld-inv-pct  AS DEC NO-UNDO.
DEF VAR ld-csh-pct  AS DEC NO-UNDO.
DEF VAR ll-comp     AS LOG NO-UNDO.
DEF VAR v-head      AS CHAR FORMAT "x(200)" EXTENT 3.
/*
FORMAT HEADER
       v-head[1] SKIP
       v-head[2] SKIP
       v-head[3]
    WITH FRAME r-top WIDTH 200.
*/
ASSIGN
 str-tit2 = TRIM(c-win:TITLE) + " (O-R-6)"
 {sys/inc/ctrtext.i str-tit2 112}

 v-per-rpt   = rd_ptd EQ "PTD"
 v-period    = begin_period
 v-date[1]   = begin_date
 v-date[2]   = end_date
 v-cat       = fg-cat
 v-sman[1]   = begin_slsmn
 v-sman[2]   = end_slsmn
 v-cust[1]   = begin_cust-no
 v-cust[2]   = end_cust-no
 v-sumdet    = NOT tb_detailed
 v-cost1     = SUBSTR(rd_cost1,1,1).

SESSION:SET-WAIT-STATE ("general").

FOR EACH tt-comm-calc:
  DELETE tt-comm-calc.
END.
FOR EACH w-comm:
  DELETE w-comm.
END.

FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK.

FOR EACH cust WHERE cust.company EQ cocode
                AND cust.cust-no GE v-cust[1]
                AND cust.cust-no LE v-cust[2] NO-LOCK:

     {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

   FOR EACH ar-inv WHERE ar-inv.company  EQ cocode
                     AND ar-inv.posted   EQ YES
                     AND ar-inv.cust-no  EQ cust.cust-no
                     AND ar-inv.inv-date GE v-date[1]
                     AND ar-inv.inv-date LE v-date[2] NO-LOCK,
      EACH ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no
                     AND ((tb_prep AND ar-invl.billable) OR NOT ar-invl.misc) NO-LOCK:

         RUN oe/invlcomp.p (ROWID(ar-invl), OUTPUT ll-comp).
         IF ll-comp THEN NEXT.

         FIND FIRST oe-ordl WHERE oe-ordl.company EQ ar-invl.company
                              AND oe-ordl.ord-no  EQ ar-invl.ord-no
                              AND oe-ordl.i-no    EQ ar-invl.i-no
                              AND oe-ordl.is-a-component EQ NO NO-LOCK NO-ERROR.
         IF v-cat NE "" THEN DO:
            RELEASE itemfg.
            IF NOT ar-invl.misc THEN
               FIND FIRST itemfg WHERE itemfg.company EQ cocode
                                   AND itemfg.i-no    EQ ar-invl.i-no NO-LOCK NO-ERROR.

            IF (ar-invl.misc AND v-cat NE "MISC")             OR
               (NOT ar-invl.misc AND
               (NOT AVAIL itemfg OR itemfg.procat NE v-cat)) THEN NEXT.
         END.

         DO i = 1 TO 3:
            v-slsm[1] = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman 
                        ELSE ar-invl.sman[i].

            IF v-slsm[1] LT v-sman[1] OR
               v-slsm[1] GT v-sman[2] OR
               (i NE 1 AND (v-slsm[1] EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

            CREATE tt-comm-calc.
            ASSIGN
               tt-comm-calc.company  = ar-inv.company
               tt-comm-calc.slsm[1]  = v-slsm[1]
               tt-comm-calc.cust-no  = cust.cust-no
               tt-comm-calc.inv-no  = ar-inv.inv-no
               tt-comm-calc.inv-type  = "ar-invl"
               tt-comm-calc.rec-id  = RECID(ar-invl)
               tt-comm-calc.row-id  = ROWID(ar-invl)
               tt-comm-calc.ordl-sell-price = IF AVAIL oe-ordl THEN oe-ordl.price ELSE 0
               tt-comm-calc.cas-cnt = IF AVAIL oe-ordl THEN oe-ordl.cas-cnt ELSE 0
               tt-comm-calc.ordl-qty = IF AVAIL oe-ordl THEN oe-ordl.qty ELSE 0
               tt-comm-calc.inv-date = ar-inv.inv-date
               tt-comm-calc.bol-no   = ar-invl.bol-no
               tt-comm-calc.cost-uom = ar-invl.dscr[1]
               tt-comm-calc.uom      = ar-invl.pr-uom
               tt-comm-calc.commission-cost = ar-invl.cost.

         END.
   END.

   FOR EACH ar-cashl WHERE ar-cashl.company EQ cocode
                      AND ar-cashl.cust-no  EQ cust.cust-no
                      AND ar-cashl.posted   EQ YES
                      AND ar-cashl.memo     EQ YES
                      AND ar-cashl.inv-date GE v-date[1]
                      AND ar-cashl.inv-date LE v-date[2] 
                      AND CAN-FIND(FIRST account WHERE account.company EQ ar-cashl.company
                                                   AND account.actnum  EQ ar-cashl.actnum
                                                   AND account.type    EQ "R") NO-LOCK,
       EACH ar-cash WHERE ar-cash.c-no       EQ ar-cashl.c-no
                      AND ar-cash.company    EQ cocode
                      AND ar-cash.cust-no    EQ ar-cashl.cust-no
                       /*AND ar-cash.check-date GE 1/1/2012
                       AND ar-cash.check-date LE 12/31/2012 */
                      AND ar-cash.posted     EQ YES NO-LOCK
                      USE-INDEX c-no:

        {custom/statusMsg.i "'Processing Customer # ' + string(ar-cash.cust-no)"} 

         RELEASE tt-comm-calc.
         RELEASE ar-invl.

         RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

         IF AVAIL oe-retl THEN 
            FIND FIRST ar-invl WHERE ar-invl.company EQ ar-cashl.company
                                 AND ar-invl.cust-no EQ cust.cust-no
                                 AND ar-invl.inv-no  EQ ar-cashl.inv-no
                                 AND ar-invl.i-no    EQ oe-retl.i-no
                                 AND ((tb_prep AND ar-invl.billable) OR NOT ar-invl.misc) NO-LOCK NO-ERROR.

         IF ar-cashl.inv-no NE 0 AND
            (AVAIL ar-invl       OR
            (NOT AVAIL reftable  AND NOT ar-cashl.dscr MATCHES "*oe return*") OR
             SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5) EQ "items") THEN

            FOR EACH b-ar-invl WHERE b-ar-invl.company EQ ar-cashl.company
                                 AND b-ar-invl.cust-no EQ cust.cust-no
                                 AND b-ar-invl.inv-no  EQ ar-cashl.inv-no
                                 AND ((tb_prep AND b-ar-invl.billable) OR NOT b-ar-invl.misc)
                                 AND (NOT AVAIL ar-invl OR ROWID(b-ar-invl) EQ ROWID(ar-invl)) NO-LOCK:

               IF AVAIL ar-invl THEN DO:
                  RUN oe/invlcomp.p (ROWID(b-ar-invl), OUTPUT ll-comp).
                  IF ll-comp THEN NEXT.
                  FIND FIRST oe-ordl WHERE oe-ordl.company EQ ar-invl.company
                                       AND oe-ordl.ord-no  EQ ar-invl.ord-no
                                       AND oe-ordl.i-no    EQ ar-invl.i-no
                                       AND oe-ordl.is-a-component EQ NO NO-LOCK NO-ERROR.
               END.

               IF v-cat NE "" THEN DO:
                  RELEASE itemfg.
                  IF NOT b-ar-invl.misc THEN
                     FIND FIRST itemfg WHERE itemfg.company eq b-ar-invl.company
                                         AND itemfg.i-no    eq b-ar-invl.i-no NO-LOCK NO-ERROR.

                  IF (b-ar-invl.misc AND v-cat NE "MISC")           OR
                     (NOT b-ar-invl.misc AND
                     (NOT AVAIL itemfg OR itemfg.procat NE v-cat)) THEN NEXT.
               END.

               DO i = 1 TO 3:
                  v-slsm[1] = IF b-ar-invl.sman[i] EQ "" AND i EQ 1 THEN
                                 cust.sman ELSE b-ar-invl.sman[i].

                  IF v-slsm[1]   LT v-sman[1] OR
                     v-slsm[1]   GT v-sman[2] OR
                     (i NE 1 AND (v-slsm[1] EQ "" OR b-ar-invl.s-pct[i] EQ 0)) THEN NEXT.

                  CREATE tt-comm-calc.
                  ASSIGN
                     tt-comm-calc.company = b-ar-invl.company
                     tt-comm-calc.slsm[1] = v-slsm[1]
                     tt-comm-calc.inv-no = b-ar-invl.inv-no
                     tt-comm-calc.row-id = ROWID(b-ar-invl)
                     tt-comm-calc.cust-no = cust.cust-no
                     tt-comm-calc.inv-type = "ar-cashl"
                     tt-comm-calc.rec-id = RECID(ar-cashl)
                     tt-comm-calc.ordl-sell-price = IF AVAIL oe-ordl THEN oe-ordl.price ELSE 0
                     tt-comm-calc.cas-cnt = IF AVAIL oe-ordl THEN oe-ordl.cas-cnt ELSE 0
                     tt-comm-calc.ordl-qty = IF AVAIL oe-ordl THEN oe-ordl.qty ELSE 0
                     tt-comm-calc.inv-date = ar-cashl.inv-date
                     tt-comm-calc.bol-no   = b-ar-invl.bol-no
                     tt-comm-calc.cost-uom = b-ar-invl.dscr[1]
                     tt-comm-calc.uom      = b-ar-invl.pr-uom
                     tt-comm-calc.commission-cost = b-ar-invl.cost.
               END.
            END. 
         ELSE DO:
            IF v-cat NE "" AND v-cat NE "CRMEM" THEN 
               NEXT.
            ELSE
               IF cust.sman GE v-sman[1] AND cust.sman LE v-sman[2] THEN DO:
                  CREATE tt-comm-calc.
                  ASSIGN
                     tt-comm-calc.company = ar-cashl.company
                     tt-comm-calc.slsm[1] = cust.sman
                     tt-comm-calc.inv-no = ar-cashl.inv-no
                     tt-comm-calc.inv-date = ar-cashl.inv-date
                     tt-comm-calc.bol-no   = b-ar-invl.bol-no
                     tt-comm-calc.cost-uom = b-ar-invl.dscr[1]
                     tt-comm-calc.uom      = b-ar-invl.pr-uom
                     tt-comm-calc.commission-cost = b-ar-invl.cost.
               END.

            IF AVAIL tt-comm-calc THEN
            ASSIGN
               tt-comm-calc.cust-no  = cust.cust-no
               tt-comm-calc.inv-type  = "ar-cashl"
               tt-comm-calc.rec-id  = RECID(ar-cashl).
         END.
      END.
END. 

input-work:
FOR EACH tt-comm-calc,
   FIRST cust WHERE cust.company EQ cocode
                AND cust.cust-no EQ tt-comm-calc.cust-no NO-LOCK
   BREAK BY tt-comm-calc.slsm[1]
         BY tt-comm-calc.cust-no
         BY tt-comm-calc.inv-no
         BY tt-comm-calc.row-id
         BY tt-comm-calc.inv-type
         BY tt-comm-calc.rec-id
         BY ROWID(tt-comm-calc):

     {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

   IF FIRST(tt-comm-calc.slsm[1])    THEN v-frst[1] = YES.
   IF FIRST-OF(tt-comm-calc.slsm[1]) THEN v-frst[2] = YES.

   FIND FIRST sman WHERE sman.company EQ cocode
                     AND sman.sman    EQ tt-comm-calc.slsm[1] NO-LOCK NO-ERROR.

   RELEASE ar-invl.
   RELEASE ar-cashl.
   ASSIGN
      tt-comm-calc.cust-part = ""
      tt-comm-calc.job-no    = ""
      tt-comm-calc.job-no2   = 0
      tt-comm-calc.ord-no    = 0
      tt-comm-calc.i-no      = ""
      tt-comm-calc.amt       = 0
      tt-comm-calc.cost      = 0
      tt-comm-calc.qty       = 0.
   IF tt-comm-calc.inv-type EQ "ar-invl" THEN
      FIND ar-invl WHERE RECID(ar-invl) EQ tt-comm-calc.rec-id NO-LOCK NO-ERROR.

   IF AVAIL ar-invl THEN DO:
      RELEASE prep.
      RELEASE itemfg.

      IF ar-invl.misc THEN
         FIND FIRST prep WHERE prep.company EQ cocode
                           AND prep.code    EQ ar-invl.i-name NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST itemfg WHERE itemfg.company EQ cocode
                             AND itemfg.i-no    EQ ar-invl.i-no NO-LOCK NO-ERROR.

      DO i = 1 TO 3:
         IF ar-invl.sman[i] EQ tt-comm-calc.slsm[1] OR ar-invl.sman[1] EQ "" THEN 
            LEAVE.
         IF i EQ 3 THEN 
            NEXT input-work.
      END.

      ASSIGN
         tt-comm-calc.procat  = IF ar-invl.misc THEN
                                IF AVAIL prep THEN prep.fgcat ELSE "MISC"
                             ELSE
                                IF AVAIL itemfg THEN itemfg.procat ELSE "ARINV"
         tt-comm-calc.slsp[1] = IF ar-invl.sman[i] EQ "" OR (ar-invl.s-pct[i] EQ 0 AND i EQ 1) THEN 100
                             ELSE ar-invl.s-pct[i]
         tt-comm-calc.qty     = ar-invl.inv-qty * (tt-comm-calc.slsp[1] / 100)
/*          tt-comm-calc.qty     = (IF ar-invl.inv-qty NE 0 THEN ar-invl.inv-qty  */
/*                               ELSE ar-invl.qty) * (tt-comm-calc.slsp[1] / 100) */
/*  05/15/09        tt-comm-calc.amt       = ar-invl.unit-pr    * (tt-comm-calc.slsp[1] / 100) */
         tt-comm-calc.amt       = ar-invl.unit-pr   
         tt-comm-calc.cust-part = ar-invl.part-no
         tt-comm-calc.job-no    = ar-invl.job-no
         tt-comm-calc.job-no2   = ar-invl.job-no2
         tt-comm-calc.ord-no    = ar-invl.ord-no
         tt-comm-calc.i-no      = ar-invl.i-no
         tt-comm-calc.sname     = IF AVAIL sman THEN sman.sname ELSE ""
         tt-comm-calc.orig-inv-amt = tt-comm-calc.amt.

      ASSIGN
         tt-comm-calc.ordl-sell-price = tt-comm-calc.ordl-sell-price * tt-comm-calc.qty
         tt-comm-calc.pr-uom  = ar-invl.pr-qty-uom.  

      IF ar-invl.misc AND NOT oe-ctrl.prep-comm THEN v-slsc[1] = 0.
   END. 
   ELSE
      IF tt-comm-calc.inv-type EQ "ar-cashl" THEN
         FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-comm-calc.rec-id NO-LOCK NO-ERROR.

   IF AVAIL ar-cashl THEN DO:
      RELEASE oe-retl.
      RELEASE ar-invl.

      FIND ar-invl WHERE ROWID(ar-invl) EQ tt-comm-calc.row-id NO-LOCK NO-ERROR.

      RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

      IF AVAIL oe-retl AND NOT AVAIL ar-invl THEN 
         FIND FIRST ar-invl WHERE ar-invl.company EQ cocode
                              AND ar-invl.cust-no EQ cust.cust-no
                              AND ar-invl.inv-no  EQ ar-cashl.inv-no
                              AND ar-invl.i-no    EQ oe-retl.i-no NO-LOCK NO-ERROR.

      IF AVAIL ar-invl THEN DO:
         DO i = 1 to 3:
            IF ar-invl.sman[i] EQ tt-comm-calc.slsm[1] OR ar-invl.sman[1] EQ "" THEN 
               LEAVE.
            IF i EQ 3 THEN 
               NEXT input-work.
         END.

         RELEASE prep.
         RELEASE itemfg.

         IF ar-invl.misc THEN
            FIND FIRST prep WHERE prep.company EQ cocode
                              AND prep.code    EQ ar-invl.i-name NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST itemfg WHERE itemfg.company EQ cocode
                                AND itemfg.i-no    EQ ar-invl.i-no NO-LOCK NO-ERROR.

         ASSIGN
            tt-comm-calc.slsp[1]   = IF ar-invl.sman[i] EQ "" OR (ar-invl.s-pct[i] EQ 0 AND i EQ 1) THEN 100
                                  ELSE ar-invl.s-pct[i]
            tt-comm-calc.qty       = 0
/*    05/15/09         tt-comm-calc.amt       = (ar-cashl.amt-paid - ar-cashl.amt-disc) * (tt-comm-calc.slsp[1] / 100) */
            tt-comm-calc.amt       = (ar-cashl.amt-paid - ar-cashl.amt-disc) 
            tt-comm-calc.cost      = 0 
            tt-comm-calc.procat    = IF ar-invl.misc THEN
                                     IF AVAIL prep THEN prep.fgcat ELSE "MISC"
                                  ELSE
                                     IF AVAIL itemfg THEN itemfg.procat ELSE "CRMEMO"
            tt-comm-calc.cust-part = ar-invl.part-no
            tt-comm-calc.job-no    = ar-invl.job-no
            tt-comm-calc.job-no2   = ar-invl.job-no2
            tt-comm-calc.ord-no    = ar-invl.ord-no
            tt-comm-calc.i-no      = ar-invl.i-no.


         IF AVAIL oe-retl THEN
            ASSIGN
               tt-comm-calc.qty  = oe-retl.tot-qty-return * -1.
         ELSE DO:
            ld-inv-pct = 0.
            FOR EACH b-ar-invl WHERE b-ar-invl.x-no EQ ar-invl.x-no NO-LOCK:
               ld-inv-pct = ld-inv-pct + b-ar-invl.amt.
                ACCUMULATE 1 (TOTAL). 
            END.
            ld-inv-pct = IF ld-inv-pct EQ 0 THEN 
                            (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                                 ELSE (ACCUM TOTAL 1))
                         ELSE 
                            (ar-invl.amt / ld-inv-pct).

            IF ld-inv-pct EQ ? THEN ld-inv-pct = 0.

            ld-csh-pct = 0.
            FOR EACH b-ar-cashl WHERE b-ar-cashl.c-no   EQ ar-cashl.c-no
                                  AND b-ar-cashl.inv-no EQ ar-cashl.inv-no NO-LOCK:
               ld-csh-pct = ld-csh-pct + (b-ar-cashl.amt-paid - b-ar-cashl.amt-disc).
            END.
            ld-csh-pct = (ar-cashl.amt-paid - ar-cashl.amt-disc) / ld-csh-pct.

            IF ld-csh-pct EQ ? THEN 
               ld-csh-pct = 0.

            tt-comm-calc.amt = tt-comm-calc.amt * ld-inv-pct.
         END.
      END. 
      ELSE
         ASSIGN
            tt-comm-calc.procat    = "CRMEM"
            tt-comm-calc.slsp[1]   = 100
            tt-comm-calc.qty       = 0
            tt-comm-calc.amt       = ar-cashl.amt-paid - ar-cashl.amt-disc
            tt-comm-calc.cost      = 0
            tt-comm-calc.slsc[1]   = IF AVAIL sman THEN sman.scomm else 0
            tt-comm-calc.cust-part = "".
   END. 

   IF tt-comm-calc.i-no NE "" THEN
      IF v-cost1 EQ "E" THEN
         RUN sys/inc/bordcost.p (tt-comm-calc.job-no, tt-comm-calc.job-no2, tt-comm-calc.i-no, 
                                 tt-comm-calc.bol-no, tt-comm-calc.qty, NO,
                                 OUTPUT tt-comm-calc.cost).
      ELSE
         IF v-cost1 EQ "O" AND tt-comm-calc.ord-no NE 0 THEN DO:
            FIND FIRST oe-ordl WHERE oe-ordl.company EQ cocode
                                 AND oe-ordl.ord-no  EQ tt-comm-calc.ord-no
                                 AND oe-ordl.i-no    EQ tt-comm-calc.i-no NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl THEN tt-comm-calc.cost = oe-ordl.cost * tt-comm-calc.qty / 1000.
         END.

   IF tt-comm-calc.cost    EQ ? THEN tt-comm-calc.cost    = 0.
   IF tt-comm-calc.slsc[1] EQ ? THEN tt-comm-calc.slsc[1] = 0.

   IF tt-comm-calc.qty EQ 0 AND AVAIL ar-cashl THEN tt-comm-calc.cost = tt-comm-calc.amt.

   IF FIRST-OF(tt-comm-calc.slsm[1]) THEN DO:
      IF NOT FIRST(tt-comm-calc.slsm[1]) THEN PAGE.
         p-sman = tt-comm-calc.slsm[1].
   END.

   tt-comm-calc.part-fg = IF rd_part-fg BEGINS "Cust" THEN tt-comm-calc.cust-part ELSE tt-comm-calc.i-no.
   tt-comm-calc.orig-inv-qty = tt-comm-calc.qty.

   IF LAST-OF(tt-comm-calc.cust-no) THEN v-frst[2] = NO.
END. 

RUN oerep/r-comprw.w (INPUT tb_inv-costs) .

{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

RUN print-report.

IF tb_excel THEN DO:
   OUTPUT STREAM st-excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT start excel.exe VALUE(SEARCH(fi_file)).
END. 

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

end procedure.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-period-dates C-Win 
PROCEDURE show-period-dates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF rd_ptd:SCREEN-VALUE eq "PTD" then do:
      FIND FIRST period
          WHERE period.company EQ cocode
            AND period.yr      EQ v-year
            AND period.pnum    EQ INT(begin_period:SCREEN-VALUE)
          NO-LOCK NO-ERROR.

      IF AVAIL period THEN
        ASSIGN
         v-year                  = period.yr
         begin_date:SCREEN-VALUE = STRING(period.pst)
         end_date:SCREEN-VALUE   = STRING(IF period.pend LT TODAY THEN period.pend ELSE TODAY).

      ELSE DO: 
        MESSAGE begin_period "is not a valid period. "
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
      END.
    END.
  END.

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

