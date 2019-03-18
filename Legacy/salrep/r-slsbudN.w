&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-slsbud.w

  Description: Sales vs. Budget

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
DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

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

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR v-cust-no AS CHAR NO-UNDO.
DEF VAR v-ytd-period AS INT NO-UNDO.
DEF VAR v-year AS INT NO-UNDO.
DEF VAR v-period AS INT NO-UNDO.
DEF VAR fdate AS DATE EXTENT 2 NO-UNDO.
DEF VAR edate AS DATE EXTENT 2 NO-UNDO.
DEF VAR v-enable-fg AS LOG NO-UNDO.
DEF VAR v-prod-line-mode AS LOG NO-UNDO.
DEF VAR cRtnChar AS CHAR NO-UNDO .
DEF VAR lRecFound AS CHAR NO-UNDO .

DEF STREAM excel.

DEF TEMP-TABLE tt-data NO-UNDO
    FIELD sman-no AS CHAR
    FIELD cust-no AS CHAR
    FIELD cust-name AS CHAR
    FIELD prod-cat AS CHAR
    FIELD month-act-dollars AS DEC
    FIELD month-act-msf AS DEC
    FIELD month-act-tons AS DEC
    FIELD month-budget-dollars AS DEC
    FIELD month-budget-msf AS DEC
    FIELD month-budget-tons AS DEC
    FIELD ytd-act-dollars AS DEC
    FIELD ytd-act-msf AS DEC
    FIELD ytd-act-tons AS DEC
    FIELD ytd-budget-dollars AS DEC
    FIELD ytd-budget-msf AS DEC
    FIELD ytd-budget-tons AS DEC
    INDEX idx1 sman-no ASC cust-no ASC.

DEF TEMP-TABLE tt-total-data
    FIELD sman-no AS CHAR
    FIELD prod-cat AS CHAR
    FIELD cust-no AS CHAR
    FIELD month-act-dollars AS DEC
    FIELD month-act-msf AS DEC
    FIELD month-act-tons AS DEC
    FIELD month-budget-dollars AS DEC
    FIELD month-budget-msf AS DEC
    FIELD month-budget-tons AS DEC
    FIELD ytd-act-dollars AS DEC
    FIELD ytd-act-msf AS DEC
    FIELD ytd-act-tons AS DEC
    FIELD ytd-budget-dollars AS DEC
    FIELD ytd-budget-msf AS DEC
    FIELD ytd-budget-tons AS DEC
    INDEX idx sman-no ASC prod-cat.

DEF TEMP-TABLE tt-prod-cat-data
    FIELD prod-cat AS CHAR
    FIELD month-act-dollars AS DEC
    FIELD month-act-msf AS DEC
    FIELD month-act-tons AS DEC
    FIELD month-budget-dollars AS DEC
    FIELD month-budget-msf AS DEC
    FIELD month-budget-tons AS DEC
    FIELD ytd-act-dollars AS DEC
    FIELD ytd-act-msf AS DEC
    FIELD ytd-act-tons AS DEC
    FIELD ytd-budget-dollars AS DEC
    FIELD ytd-budget-msf AS DEC
    FIELD ytd-budget-tons AS DEC
    INDEX idx prod-cat.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD row-id AS ROWID
    FIELD prod-cat AS CHAR.

DEF TEMP-TABLE tt-fg-cat NO-UNDO
    FIELD prodline AS CHAR
    FIELD fg-cat AS CHAR
    INDEX prodline fg-cat.

DEF BUFFER b-tt-report FOR tt-report.
DEF BUFFER b-ar-invl FOR ar-invl.
DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-fgcat FOR fgcat.

DEF VAR v-gt-month-act-dol AS DEC NO-UNDO.
DEF VAR v-gt-month-bud-dol AS DEC NO-UNDO.
DEF VAR v-gt-ytd-act-dol AS DEC NO-UNDO.
DEF VAR v-gt-ytd-bud-dol AS DEC NO-UNDO.
DEF VAR v-gt-month-act-msf AS DEC NO-UNDO.
DEF VAR v-gt-month-bud-msf AS DEC NO-UNDO.
DEF VAR v-gt-ytd-act-msf AS DEC NO-UNDO.
DEF VAR v-gt-ytd-bud-msf AS DEC NO-UNDO.
DEF VAR v-gt-month-act-tons AS DEC NO-UNDO.
DEF VAR v-gt-month-bud-tons AS DEC NO-UNDO.
DEF VAR v-gt-ytd-act-tons AS DEC NO-UNDO.
DEF VAR v-gt-ytd-bud-tons AS DEC NO-UNDO.

def TEMP-TABLE w-data no-undo
  field w-type      as   char
  field w-sman-no   AS CHAR
  FIELD prod-cat    AS CHAR
  field w-sqft      AS DEC     extent 2 
  field w-amt       AS DEC     extent 2
  FIELD w-tons      AS DEC     EXTENT 2.

def TEMP-TABLE w-data1 like w-data.


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
DEF VAR str-line AS cha FORM "x(350)" NO-UNDO.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "SM #,SalesRep Name,Mnth Acl $,Mnth Bgt$,Mnth Var $,YTD Acl $,YTD Bgt $,YTD Var $," +
                           "Cust #,Customer Name,Categ,Mnth Acl MSF,Mnth Bgt MSF,Mnth Var MSF,YTD Acl MSF,YTD Bgt MSF,YTD Var MSF," + 
                           "Mnth Acl Ton,Mnth Bgt Ton,Mnth Var Ton,YTD Acl Ton,YTD Bgt Ton,YTD Var Ton"
       cFieldListToSelect = "rep,rep-nam,mth-act,mth-bgt,mth-var,ytd-act,ytd-bgt,ytd-var," +
                            "cust,name,cat,mth-act-m,mth-bgt-m,mth-var-m,ytd-act-m,ytd-bgt-m,ytd-var-m," +
                            "mth-act-t,mth-bgt-t,mth-var-t,ytd-act-t,ytd-bgt-t,ytd-var-t"
       cFieldLength = "4,25,11,11,11,11,11,11," + "8,30,5,12,12,12,11,11,11," + "12,12,12,11,11,11" 
       cFieldType = "c,c,i,i,i,i,i,i," + "c,c,c,i,i,i,i,i,i," + "i,i,i,i,i,i"
    .



{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "SM #,SalesRep Name,Cust #,Customer Name,Categ,Mnth Acl $,Mnth Bgt$,Mnth Var $,YTD Acl $,YTD Bgt $,YTD Var $," 
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rs_detail begin_slsmn ~
end_slsmn tb_cust-list btnCustList begin_cust-no end_cust-no begin_date ~
end_date rs-category begin_fg-cat end_fg-cat tg_disp_cents rd_sortby ~
tg_display-zero sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up ~
btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS rs_detail begin_slsmn end_slsmn ~
tb_cust-list begin_cust-no end_cust-no begin_date end_date rs-category ~
begin_fg-cat end_fg-cat begin_prolin end_prolin tg_disp_cents rd_sortby ~
tg_display-zero sl_avail sl_selected rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

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
     SIZE 20 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Start Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_fg-cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning FG Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_prolin AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Product Line" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "End Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_fg-cat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending FG Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_prolin AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Product Line" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-slsbud.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1.

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

DEFINE VARIABLE rd_sortby AS CHARACTER INITIAL "PC" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Product Category then Customer", "PC",
"Customer then Product Category", "CU"
     SIZE 36 BY 1.91 NO-UNDO.

DEFINE VARIABLE rs-category AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "FG Category", "FG",
"Product Line", "ProdLine"
     SIZE 38 BY .71 NO-UNDO.

DEFINE VARIABLE rs_detail AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Detail", yes,
"Summary", no
     SIZE 26 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 12.62.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.95 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.95 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg_display-zero AS LOGICAL INITIAL yes 
     LABEL "Display Zero Values?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tg_disp_cents AS LOGICAL INITIAL yes 
     LABEL "Display Cents?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     rs_detail AT ROW 2.05 COL 5 NO-LABEL
     begin_slsmn AT ROW 3.24 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 3.24 COL 67 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     tb_cust-list AT ROW 4.33 COL 28.4 WIDGET-ID 6
     btnCustList AT ROW 4.43 COL 61 WIDGET-ID 8
     begin_cust-no AT ROW 5.38 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 5.38 COL 67 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_date AT ROW 6.38 COL 26 COLON-ALIGNED HELP
          "Enter Start Invoice Date"
     end_date AT ROW 6.38 COL 67 COLON-ALIGNED HELP
          "Enter End Invoice Date"
     rs-category AT ROW 7.62 COL 28 NO-LABEL
     begin_fg-cat AT ROW 8.48 COL 26.2 COLON-ALIGNED HELP
          "Enter Beginning Product Category"
     end_fg-cat AT ROW 8.48 COL 67.2 COLON-ALIGNED HELP
          "Enter Ending Product Category"
     begin_prolin AT ROW 9.48 COL 26.2 COLON-ALIGNED HELP
          "Enter Beginning Product Category"
     end_prolin AT ROW 9.48 COL 67.2 COLON-ALIGNED HELP
          "Enter Ending Product Category"
     tg_disp_cents AT ROW 11.43 COL 49.4
     rd_sortby AT ROW 11.52 COL 5 NO-LABEL WIDGET-ID 2
     tg_display-zero AT ROW 12.57 COL 49.2
     sl_avail AT ROW 14.57 COL 4 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 14.57 COL 59 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 14.67 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 15.81 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 17 COL 40.4 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 18.19 COL 40.4 WIDGET-ID 40
     btn_down AT ROW 19.38 COL 40.4 WIDGET-ID 42
     rd-dest AT ROW 21.86 COL 6 NO-LABEL
     lv-ornt AT ROW 22.33 COL 31 NO-LABEL
     lines-per-page AT ROW 22.33 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 23.43 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 24.38 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 26.05 COL 30
     tb_excel AT ROW 27.33 COL 67.4 RIGHT-ALIGNED
     tb_runExcel AT ROW 27.33 COL 89.4 RIGHT-ALIGNED
     fi_file AT ROW 28.43 COL 45.4 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 30.19 COL 19
     btn-cancel AT ROW 30.19 COL 57
     "Available Columns" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 13.81 COL 4.8 WIDGET-ID 38
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 13.86 COL 59.2 WIDGET-ID 44
     "Sort By:" VIEW-AS TEXT
          SIZE 24 BY .71 AT ROW 10.76 COL 5 WIDGET-ID 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 20.91 COL 5
     RECT-6 AT ROW 20.67 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 30.57.


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
         TITLE              = "Sales vs. Budget"
         HEIGHT             = 30.81
         WIDTH              = 95
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
       begin_fg-cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_prolin IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       begin_prolin:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       end_fg-cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_prolin IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       end_prolin:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_sortby:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Sales vs. Budget */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sales vs. Budget */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON HELP OF FRAME FRAME-A
DO:
   DEF VAR char-val AS CHAR NO-UNDO.
   DEF VAR lv-handle AS HANDLE NO-UNDO.

   CASE FOCUS:NAME:
      when "begin_prolin" OR WHEN "end_prolin" then do:
            run windows/l-prodlin.w 
               (cocode,focus:screen-value in frame {&frame-name}, output char-val).
            if char-val <> "" then 
               focus:screen-value in frame {&frame-name} = entry(1,char-val).
            return no-apply.  
      end.
      OTHERWISE DO:
         lv-handle = FOCUS:HANDLE.
         RUN applhelp.p.

         IF g_lookup-var NE "" THEN lv-handle:SCREEN-VALUE = g_lookup-var.
      END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Start Invoice Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_fg-cat C-Win
ON LEAVE OF begin_fg-cat IN FRAME FRAME-A /* Beginning FG Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_prolin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_prolin C-Win
ON LEAVE OF begin_prolin IN FRAME FRAME-A /* Beginning Product Line */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Sales Rep# */
DO:
   assign {&self-name}.
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
     ASSIGN {&DISPLAYED-OBJECTS}
            v-prod-line-mode = NOT rs-category EQ "FG".
  END.

    RUN GetSelectionList.
    FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT tb_cust-list OR  NOT AVAIL ttCustList THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust-no,
                    INPUT end_cust-no).
  END.
    run run-report. 
    STATUS DEFAULT "Processing Complete".

    SESSION:SET-WAIT-STATE ("").

    case rd-dest:
         when 1 then run output-to-printer.
         when 2 then run output-to-screen.
         when 3 then run output-to-file.
         when 4 then do:
             {custom/asifax.i &type= "Customer"
                              &begin_cust="begin_cust-no"
                              &END_cust="begin_cust-no" 
                              &fax-subject=c-win:TITLE
                              &fax-body=c-win:TITLE
                              &fax-file=list-name }
         END. 
         when 5 then do:
             IF is-xprint-form THEN DO:
                {custom/asimail.i &TYPE = "Customer "
                               &begin_cust="begin_cust-no"
                               &END_cust="begin_cust-no"
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


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.

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


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* End Invoice Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_fg-cat C-Win
ON LEAVE OF end_fg-cat IN FRAME FRAME-A /* Ending FG Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_prolin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_prolin C-Win
ON LEAVE OF end_prolin IN FRAME FRAME-A /* Ending Product Line */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Sales Rep# */
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


&Scoped-define SELF-NAME rd_sortby
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sortby C-Win
ON VALUE-CHANGED OF rd_sortby IN FRAME FRAME-A
DO:
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN rd_sortby.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-category
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-category C-Win
ON VALUE-CHANGED OF rs-category IN FRAME FRAME-A
DO:
   DO WITH FRAME {&FRAME-NAME}:

      ASSIGN rs-category
             v-enable-fg = rs-category EQ "FG"
             begin_fg-cat:SENSITIVE = v-enable-fg
             end_fg-cat:SENSITIVE = v-enable-fg
             begin_prolin:SENSITIVE = NOT v-enable-fg
             end_prolin:SENSITIVE = NOT v-enable-fg.

      IF v-enable-fg THEN
         ASSIGN
            begin_prolin:SCREEN-VALUE = ""
            end_prolin:SCREEN-VALUE = ""
            end_fg-cat:SCREEN-VALUE = "zzzzz".
      ELSE
         ASSIGN
            begin_fg-cat:SCREEN-VALUE = ""
            end_fg-cat:SCREEN-VALUE = ""
            end_prolin:SCREEN-VALUE = "zzzzzzzz".
   END.
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


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
  assign {&self-name}.
  EMPTY TEMP-TABLE ttCustList.
  RUN SetCustRange(INPUT tb_cust-list).
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
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN.
  END.

  ASSIGN
     begin_date = TODAY
     end_date = TODAY.

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "HS",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
     {custom/usrprint.i}
     RUN DisplaySelectionList2.
     ASSIGN
        v-enable-fg = rs-category:SCREEN-VALUE EQ "FG"
        begin_fg-cat:SENSITIVE = v-enable-fg
        end_fg-cat:SENSITIVE = v-enable-fg
        begin_prolin:SENSITIVE = NOT v-enable-fg
        end_prolin:SENSITIVE = NOT v-enable-fg. 
  END.

  APPLY "entry" TO rs_detail IN FRAME {&FRAME-NAME}.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'HS',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""HS""}

  IF ou-log THEN DO:
      ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
        tb_cust-list = YES 
        .
      RUN SetCustRange(INPUT tb_cust-list).
  END.
  ELSE
      ASSIGN
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        .

   IF ou-log AND ou-cust-int = 0 THEN do:
       ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
        tb_cust-list = NO
        .
      RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
   END.

  /* gdm - 03090904 */
   /* FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name EQ "SalesBudget" NO-ERROR.
    IF NOT AVAIL sys-ctrl THEN DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.name    = "SalesBudget"
            sys-ctrl.log-fld = NO
            sys-ctrl.descrip = "Budget Report".
    END.

    RELEASE sys-ctrl.*/
   RUN sys/ref/nk1look.p (INPUT cocode, "SalesBudget", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
/* gdm - 0309094 end */

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actual-proc C-Win 
PROCEDURE actual-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def var v-sman-no as char format "x(3)" no-undo.
   def var v-amt       like ar-inv.gross no-undo.
   def var v-sqft      like itemfg.t-sqft  format "->>,>>9.999" no-undo.
   def var v-pmsf      as   dec format "->>>>>9.99" extent 2 no-undo.
   def var v-misc as log no-undo.
   def var v-pct as dec format "99.99" no-undo.
   DEF VAR ld-inv-pct AS DEC NO-UNDO.
   DEF VAR v-prod-cat AS CHAR NO-UNDO.
   DEF VAR v-prod-cat-ar-cashl AS CHAR NO-UNDO.

   FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
      EACH cust FIELDS(cust-no sman NAME) where
       cust.company eq cocode AND
       cust.cust-no EQ ttCustList.cust-no /*begin_cust-no AND
       cust.cust-no LE end_cust-no*/
       no-lock:

       EMPTY TEMP-TABLE tt-report.
       EMPTY TEMP-TABLE w-data.
       EMPTY TEMP-TABLE w-data1.

       for each ar-inv FIELDS(x-no cust-no)
           where ar-inv.company  eq cocode
             and ar-inv.posted   eq yes
             and ar-inv.cust-no  eq cust.cust-no
             and ar-inv.inv-date ge fdate[2]
             and ar-inv.inv-date le end_date
             and ar-inv.type    ne "FC" /*or v-inc-fc*/
           no-lock,

           each ar-invl FIELDS(i-no actnum sman s-pct LINE)
           where ar-invl.x-no eq ar-inv.x-no
             and (ar-invl.billable or not ar-invl.misc)
           no-lock
           transaction:

         {sa/sa-sman7.i "ar-invl"}
       end.

       for each ar-cash FIELDS(c-no cust-no check-date)
           where ar-cash.company    eq cocode
             and ar-cash.cust-no    eq cust.cust-no
             and ar-cash.check-date ge begin_date
             and ar-cash.check-date le end_date
             and ar-cash.posted     eq yes
           no-lock,

           EACH ar-cashl FIELDS(inv-no dscr company c-no)
           WHERE ar-cashl.c-no    EQ ar-cash.c-no
             AND ar-cashl.posted  EQ YES
             AND ar-cashl.memo    EQ YES
             AND (CAN-FIND(FIRST account
                           WHERE account.company EQ ar-cashl.company
                             AND account.actnum  EQ ar-cashl.actnum
                             AND account.type    EQ "R"))
           NO-LOCK
           transaction:

         release ar-invl.

         RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

         if avail oe-retl then
         find first ar-invl
             where ar-invl.company eq cocode
               and ar-invl.cust-no eq ar-cash.cust-no
               and ar-invl.inv-no  eq ar-cashl.inv-no
               and ar-invl.i-no    eq oe-retl.i-no
               and (ar-invl.billable or not ar-invl.misc)
             no-lock no-error.

         IF ar-cashl.inv-no NE 0 AND
            (AVAIL ar-invl OR
             (NOT AVAIL reftable AND
              NOT ar-cashl.dscr MATCHES "*oe return*") OR
             SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5) EQ "items") THEN
         FOR EACH b-ar-invl
             WHERE b-ar-invl.company EQ ar-cashl.company
               AND b-ar-invl.cust-no EQ cust.cust-no
               AND b-ar-invl.inv-no  EQ ar-cashl.inv-no
               AND (b-ar-invl.billable OR NOT b-ar-invl.misc)
               AND (NOT AVAIL ar-invl OR ROWID(b-ar-invl) EQ ROWID(ar-invl))
             NO-LOCK:

             {sa/sa-sman7.i "ar-cashl" "b-"}
         end.

         ELSE
         do:
            find first fgcat
                where fgcat.company eq cocode
                  and fgcat.glacc   eq ar-cashl.actnum
                no-lock no-error.

            IF avail fgcat THEN
               v-prod-cat-ar-cashl = fgcat.procat.
            ELSE
               v-prod-cat-ar-cashl = "MISC".

            IF (cust.sman GE begin_slsmn AND
                cust.sman LE END_slsmn) THEN NEXT.

            IF NOT v-prod-line-mode AND
               NOT(v-prod-cat-ar-cashl GE begin_fg-cat AND
                   v-prod-cat-ar-cashl LE end_fg-cat) THEN NEXT.
            ELSE
               IF v-prod-line-mode AND 
                  NOT can-find(FIRST tt-fg-cat WHERE
                               tt-fg-cat.fg-cat = v-prod-cat-ar-cashl) THEN
                NEXT.

            IF NOT v-prod-line-mode THEN
             DO:
                FIND FIRST tt-data WHERE
                     tt-data.cust-no = cust.cust-no AND
                     tt-data.sman-no = cust.sman AND
                     tt-data.prod-cat = v-prod-cat-ar-cashl
                     NO-ERROR.

                IF NOT AVAIL tt-data THEN
                DO:
                   CREATE tt-data.
                   ASSIGN tt-data.cust-no = cust.cust-no
                          tt-data.cust-name = cust.NAME
                          tt-data.sman-no = cust.sman
                          tt-data.prod-cat = v-prod-cat-ar-cashl.
                   RELEASE tt-data.
                END.
             END. /*NOT v-prod-line-mode*/
             ELSE
             DO:
                FIND FIRST tt-fg-cat WHERE
                     tt-fg-cat.fg-cat = v-prod-cat-ar-cashl.

                FIND FIRST tt-data WHERE
                     tt-data.cust-no = cust.cust-no AND
                     tt-data.sman-no = cust.sman AND
                     tt-data.prod-cat = tt-fg-cat.prodline
                     NO-ERROR.

                IF NOT AVAIL tt-data THEN
                DO:
                   CREATE tt-data.
                   ASSIGN tt-data.cust-no = cust.cust-no
                          tt-data.cust-name = cust.NAME
                          tt-data.sman-no = cust.sman
                          tt-data.prod-cat = tt-fg-cat.prodline.
                   RELEASE tt-data.
                END.
             END.

            create tt-report.
            assign
            tt-report.key-01  = IF AVAIL reftable OR
                                ar-cashl.dscr MATCHES "*oe return*" THEN "2" ELSE "4"
            tt-report.key-02  = cust.sman
            tt-report.key-09  = cust.cust-no
            tt-report.key-10  = "ar-cashl"
            tt-report.rec-id  = recid(ar-cashl)
            tt-report.prod-cat = v-prod-cat-ar-cashl.
         end.
       end.

       for each tt-report
           break by tt-report.key-02
                 BY tt-report.prod-cat

           transaction:

         find first w-data WHERE
              w-data.w-type    eq tt-report.key-01 AND
              w-data.w-sman-no eq tt-report.key-02 AND
              w-data.prod-cat  EQ tt-report.prod-cat
              no-error.

         if not avail w-data then do:
           create w-data.
           assign
            w-data.w-type    = tt-report.key-01
            w-data.w-sman-no = tt-report.key-02
            w-data.prod-cat  = tt-report.prod-cat.
         end.

         find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock no-error.

         if avail ar-invl then do:
           find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.

           find first itemfg
               where itemfg.company eq cocode
                 and itemfg.i-no    eq ar-invl.i-no
               no-lock no-error.

           assign
            v-pct  = 1
            v-amt  = ar-invl.amt
            v-sqft = if ar-invl.amt-msf ne 0 then ar-invl.amt-msf
                     else
                     if avail itemfg then
                       (itemfg.t-sqft * ar-invl.ship-qty / 1000) else 0.

           if v-sqft eq ? then v-sqft = 0.

           do i = 1 to 3:
             if ar-invl.sman[i] eq tt-report.key-02 then
               assign
                v-pct = ar-invl.s-pct[i] / 100
                i     = 3.
           end.

           if v-pct eq 0 then
           do i = 1 to 3:
             if i eq 1 then j = 0.
             if ar-invl.sman[i] ne "" then j = j + 1.
             if i eq 3 then v-pct = 1 / j.
           end.

           if v-pct le 0 or v-pct eq ? then v-pct = 1.

           do i = 1 to 2:
             if ar-inv.inv-date ge fdate[i] and
                ar-inv.inv-date le edate[i] then
               assign
                w-data.w-sqft[i] = w-data.w-sqft[i] + (v-sqft * v-pct)
                w-data.w-amt[i]  = w-data.w-amt[i]  + (v-amt  * v-pct)
                w-data.w-tons[i] = w-data.w-tons[i]
                                 + ((if ar-invl.t-weight ne 0 then ar-invl.t-weight
                                     else
                                        if avail itemfg then
                                           (ar-invl.ship-qty * itemfg.weight-100 / 100)
                                     else 0) / 2000).
           end.
         end.

         else do:
           find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.

           if avail ar-cashl then do:
             find ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.

             assign
              v-amt  = ar-cashl.amt-paid - ar-cashl.amt-disc
              v-sqft = 0
              v-pct  = 1.

             RELEASE ar-invl.
             RELEASE oe-retl.

             FIND ar-invl WHERE ROWID(ar-invl) EQ tt-report.row-id NO-LOCK NO-ERROR.

             IF NOT AVAIL ar-invl THEN
               RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

             IF AVAIL oe-retl THEN DO:
               find first itemfg
                   where itemfg.company eq cocode
                     and itemfg.i-no    eq oe-retl.i-no
                   no-lock no-error.

               v-sqft = IF AVAIL itemfg THEN
                          (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                        ELSE 0.

               IF AVAIL itemfg AND tt-report.prod-cat = "" THEN
                  tt-report.prod-cat = itemfg.procat.
             END.

             ELSE
             IF AVAIL ar-invl THEN DO:
               ld-inv-pct = 0.

               IF tt-report.prod-cat = "" THEN
               DO:
                  FIND FIRST b-itemfg WHERE
                       b-itemfg.company EQ cocode AND
                       b-itemfg.i-no EQ ar-invl.i-no
                       NO-LOCK NO-ERROR.

                  IF AVAIL b-itemfg THEN
                  DO:
                     tt-report.prod-cat = b-itemfg.procat.
                     RELEASE b-itemfg.
                  END.
               END.

               FOR EACH b-ar-invl WHERE
                   b-ar-invl.x-no EQ ar-invl.x-no NO-LOCK:
                   ld-inv-pct = ld-inv-pct + b-ar-invl.amt.
                   ACCUMULATE 1 (TOTAL).
               END.
               ld-inv-pct = IF ld-inv-pct EQ 0 THEN
                               (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                                                            ELSE (ACCUM TOTAL 1))
                            ELSE (ar-invl.amt / ld-inv-pct).

               IF ld-inv-pct EQ ? THEN ld-inv-pct = 0.

               v-amt = v-amt * ld-inv-pct.

               if v-sqft eq ? then v-sqft = 0.

               do i = 1 to 3:
                 if ar-invl.sman[i] eq tt-report.key-02 then
                   assign
                    v-pct = ar-invl.s-pct[i] / 100
                    i     = 3.
               end.

               if v-pct eq 0 then
               do i = 1 to 3:
                 if i eq 1 then j = 0.
                 if ar-invl.sman[i] ne "" then j = j + 1.
                 if i eq 3 then v-pct = 1 / j.
               end.

               if v-pct le 0 or v-pct eq ? then v-pct = 1.
             end.

             do i = 1 to 2:
               if ar-cash.check-date ge fdate[i] and
                  ar-cash.check-date le edate[i] then
                  assign
                     w-data.w-sqft[i] = w-data.w-sqft[i] - (v-sqft * v-pct)
                     w-data.w-amt[i]  = w-data.w-amt[i]  + (v-amt  * v-pct)
                     w-data.w-tons[i] = w-data.w-tons[i] +
                                        (((if AVAIL oe-retl AND avail itemfg then
                                          (oe-retl.tot-qty-return * itemfg.weight-100 / 100)
                                          else 0) / 2000)).
             end.

             RELEASE oe-retl.
             RELEASE itemfg.
           end.
         end.

         if last-of(tt-report.prod-cat) then do:

           create w-data1.
           w-data1.w-sman-no = tt-report.key-02.

           for each w-data where
               w-data.w-sman-no eq w-data1.w-sman-no AND
               w-data.prod-cat EQ tt-report.prod-cat:

               do i = 1 to 2:
                  assign
                  w-data1.w-sqft[i] = w-data1.w-sqft[i] + w-data.w-sqft[i]
                  w-data1.w-amt[i]  = w-data1.w-amt[i]  + w-data.w-amt[i]
                  w-data1.w-tons[i] = w-data1.w-tons[i] + w-data.w-tons[i].
               end.
           end.

           RELEASE tt-data.

           IF NOT v-prod-line-mode THEN
              FIND FIRST tt-data WHERE
                   tt-data.cust-no = tt-report.key-09 AND
                   tt-data.sman-no = tt-report.key-02 AND
                   tt-data.prod-cat = tt-report.prod-cat
                   NO-ERROR.
           ELSE
           DO:
              FIND FIRST tt-fg-cat WHERE
                   tt-fg-cat.fg-cat = v-prod-cat.

              FIND FIRST tt-data WHERE
                   tt-data.cust-no = cust.cust-no AND
                   tt-data.sman-no = v-sman-no AND
                   tt-data.prod-cat = tt-fg-cat.prodline
                   NO-ERROR.
           END.

           IF AVAIL tt-data THEN
              ASSIGN
                 tt-data.month-act-dollars = w-data1.w-amt[1]
                 tt-data.ytd-act-dollars = w-data1.w-amt[2]
                 tt-data.month-act-msf = w-data1.w-sqft[1]
                 tt-data.ytd-act-msf = w-data1.w-sqft[2]
                 tt-data.month-act-tons = w-data1.w-tons[1]
                 tt-data.ytd-act-tons = w-data1.w-tons[2].

           delete w-data1.
         end.

         delete tt-report.
       end.
   end. /*end cust*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildCustList C-Win 
PROCEDURE BuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Builds the temp table of customers   
  Parameters:  Company Code, Customer list logical and/or customer range
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplList AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginCust AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndCust AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-cust FOR cust.

DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.

IF iplList THEN DO:
    RUN sys/ref/CustList.p (INPUT ipcCompany,
                            INPUT 'HS',
                            INPUT YES,
                            OUTPUT lActive).
END.
ELSE DO:
    FOR EACH bf-cust
        WHERE bf-cust.company EQ ipcCompany
          AND bf-cust.cust-no GE ipcBeginCust
          AND bf-cust.cust-no LE ipcEndCust
        NO-LOCK:
        CREATE ttCustList.
        ASSIGN 
            ttCustList.cust-no = bf-cust.cust-no
            ttCustList.log-fld = YES
        .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt-data C-Win 
PROCEDURE create-tt-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAM ip-prod-cat AS CHAR NO-UNDO.
   DEFINE INPUT PARAM ip-prod-cat-label AS CHAR NO-UNDO.

   DEF VAR v-count AS INT NO-UNDO.

   DO v-count = 1 TO v-ytd-period:

    FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
        EACH smanbcst WHERE
          smanbcst.company EQ cocode AND
          smanbcst.sman EQ sman.sman AND
          smanbcst.budget-yr EQ v-year AND
          smanbcst.budget-period EQ v-count AND
          smanbcst.procat EQ ip-prod-cat AND
          smanbcst.cust EQ ttCustList.cust-no /*begin_cust-no AND
          smanbcst.cust LE end_cust-no */
          NO-LOCK,
          FIRST cust FIELDS(cust-no NAME) WHERE
                cust.company EQ cocode AND
                cust.cust-no EQ smanbcst.cust
                NO-LOCK:

          FIND FIRST tt-data WHERE
               tt-data.cust-no = cust.cust-no AND
               tt-data.sman-no = sman.sman AND
               tt-data.prod-cat = ip-prod-cat-label
               NO-ERROR.

          IF NOT AVAIL tt-data THEN
          DO:
             CREATE tt-data.
             ASSIGN tt-data.cust-no = cust.cust-no
                    tt-data.cust-name = cust.NAME
                    tt-data.sman-no = sman.sman
                    tt-data.prod-cat = ip-prod-cat-label.
          END.

          IF v-period EQ v-count THEN
             ASSIGN
                tt-data.month-budget-dollars = tt-data.month-budget-dollars + smanbcst.budget-amt
                tt-data.month-budget-msf = tt-data.month-budget-msf + smanbcst.msf
                tt-data.month-budget-tons = tt-data.month-budget-tons + smanbcst.tons.

          ASSIGN
             tt-data.ytd-budget-dollars = tt-data.ytd-budget-dollars + smanbcst.budget-amt
             tt-data.ytd-budget-msf     = tt-data.ytd-budget-msf    + smanbcst.msf
             tt-data.ytd-budget-tons    = tt-data.ytd-budget-tons   + smanbcst.tons.
      END.
   END.

   IF v-period GT v-ytd-period THEN
    FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
       EACH smanbcst WHERE
          smanbcst.company EQ cocode AND
          smanbcst.sman EQ sman.sman AND
          smanbcst.budget-yr EQ v-year AND
          smanbcst.budget-period EQ v-period AND
          smanbcst.procat EQ ip-prod-cat AND
          smanbcst.cust EQ ttCustList.cust-no /*begin_cust-no and
          smanbcst.cust LE end_cust-no */
          NO-LOCK,
          FIRST cust FIELDS(cust-no NAME) WHERE
                cust.company EQ cocode AND
                cust.cust-no EQ smanbcst.cust
                NO-LOCK:

          FIND FIRST tt-data WHERE
               tt-data.cust-no = cust.cust-no AND
               tt-data.sman-no = sman.sman AND
               tt-data.prod-cat = ip-prod-cat-label
               NO-ERROR.

          IF NOT AVAIL tt-data THEN
          DO:
             CREATE tt-data.
             ASSIGN tt-data.cust-no = cust.cust-no
                    tt-data.cust-name = cust.NAME
                    tt-data.sman-no = sman.sman
                    tt-data.prod-cat = ip-prod-cat-label.
          END.

          ASSIGN
             tt-data.month-budget-dollars = tt-data.month-budget-dollars + smanbcst.budget-amt
             tt-data.month-budget-msf = tt-data.month-budget-msf + smanbcst.msf
             tt-data.month-budget-tons = tt-data.month-budget-tons + smanbcst.tons.
      END.

   RELEASE tt-data.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
/*------------------------------------------------------------------------------
  Purpose:  Display a UI of selected customers   
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
                                  INPUT 'HS').


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY rs_detail begin_slsmn end_slsmn tb_cust-list begin_cust-no end_cust-no 
          begin_date end_date rs-category begin_fg-cat end_fg-cat begin_prolin 
          end_prolin tg_disp_cents rd_sortby tg_display-zero sl_avail 
          sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 rs_detail begin_slsmn end_slsmn tb_cust-list btnCustList 
         begin_cust-no end_cust-no begin_date end_date rs-category begin_fg-cat 
         end_fg-cat tg_disp_cents rd_sortby tg_display-zero sl_avail 
         sl_selected Btn_Def Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc C-Win 
PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST period WHERE
     period.company EQ cocode AND
     period.pst LE begin_date AND
     period.pend GE begin_date
     NO-LOCK NO-ERROR.

IF AVAIL period THEN
   ASSIGN
      v-period = period.pnum
      v-year   = period.yr
      fdate[1] = period.pst
      edate[1] = period.pend.

find first period
    where period.company eq cocode
      and period.yr      eq v-year
    no-lock.

assign
   fdate[2] = period.pst
   edate[2] = end_date.

FIND FIRST period WHERE
     period.company EQ cocode AND
     period.pst LE end_date AND
     period.pend GE end_date
     NO-LOCK NO-ERROR.

v-ytd-period = period.pnum.

EMPTY TEMP-TABLE tt-fg-cat.

IF v-prod-line-mode THEN
   FOR EACH prod FIELDS(prolin) WHERE
       prod.company EQ cocode AND
       prod.prolin GE begin_prolin AND
       prod.prolin LE end_prolin
       NO-LOCK,
       EACH prodl fields(procat) WHERE
            prodl.company EQ cocode AND
            prodl.prolin EQ prod.prolin
            NO-LOCK:

       CREATE tt-fg-cat.
       ASSIGN tt-fg-cat.prodline = prod.prolin
              tt-fg-cat.fg-cat = prodl.procat.
       RELEASE tt-fg-cat.
   END.

RUN actual-proc.

FOR EACH sman FIELDS(sman) WHERE
    sman.company EQ cocode AND
    sman.sman GE begin_slsmn AND
    sman.sman LE end_slsmn
    NO-LOCK:

     {custom/statusMsg.i " 'Processing Sales Rep#  '  + sman.sman "}

    IF NOT v-prod-line-mode THEN
       FOR EACH b-fgcat FIELDS(procat) WHERE
           b-fgcat.company EQ cocode AND
           b-fgcat.procat GE begin_fg-cat AND
           b-fgcat.procat LE end_fg-cat
           NO-LOCK:

           RUN create-tt-data(INPUT b-fgcat.procat,
                              INPUT b-fgcat.procat).
       END.
    ELSE
       FOR EACH prod FIELDS(prolin) WHERE
           prod.company EQ cocode AND
           prod.prolin GE begin_prolin AND
           prod.prolin LE end_prolin
           NO-LOCK,
           EACH prodl fields(procat) WHERE
                prodl.company EQ cocode AND
                prodl.prolin EQ prod.prolin
                NO-LOCK:

           RUN create-tt-data(INPUT prodl.procat,
                              INPUT prod.prolin).
       END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-excel-proc C-Win 
PROCEDURE print-excel-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-one AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-two AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-three AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-four AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-five AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-six AS DEC NO-UNDO.

  /* IF tg_disp_tons THEN
      PUT STREAM excel UNFORMATTED
          '"' ip-one '",'
          '"' REPLACE(ip-two,","," ") '",'
          '"' STRING(ip-three,"->>,>>>,>>9.99") '",'
          '"' STRING(ip-four,"->>,>>>,>>9.99") '",'
          '"' STRING(ip-three - ip-four,"->>,>>>,>>9.99") '",'
          '"' STRING(ip-five,"->>,>>>,>>9.99") '",'
          '"' STRING(ip-six,"->>,>>>,>>9.99") '",'
          '"' STRING(ip-five - ip-six,"->>,>>>,>>9.99") '",'.
   ELSE
      PUT STREAM excel UNFORMATTED
          '"' ip-one '",'
          '"' REPLACE(ip-two,","," ") '",'
          '"' STRING(ip-three,"->>,>>>,>>9") '",'
          '"' STRING(ip-four,"->>,>>>,>>9") '",'
          '"' STRING(ip-three - ip-four,"->>,>>>,>>9") '",'
          '"' STRING(ip-five,"->>,>>>,>>9") '",'
          '"' STRING(ip-six,"->>,>>>,>>9") '",'
          '"' STRING(ip-five - ip-six,"->>,>>>,>>9") '",'. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-excel2-proc C-Win 
PROCEDURE print-excel2-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-one AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-two AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-three AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-four AS DEC NO-UNDO.

   PUT STREAM excel UNFORMATTED
       '"' STRING(ip-one,"->>,>>>,>>9.99") '",'
       '"' STRING(ip-two,"->>,>>>,>>9.99") '",'
       '"' STRING(ip-one - ip-two,"->>,>>>,>>9.99") '",'
       '"' STRING(ip-three,"->>,>>>,>>9.99") '",'
       '"' STRING(ip-four,"->>,>>>,>>9.99") '",'
       '"' STRING(ip-three - ip-four,"->>,>>>,>>9.99") '",'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-grand-totals-proc C-Win 
PROCEDURE print-grand-totals-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

PUT "REPORT TOTALS" FORMAT "X(15)"
    SKIP(1)
  /*  WITH DOWN FRAME frame-report-totals NO-LABELS NO-BOX STREAM-IO WIDTH 132 */ .

IF tb_excel THEN
   PUT STREAM excel UNFORMATTED SKIP(1) "REPORT TOTALS" SKIP(1).

IF tg_disp_cents THEN
DO:
   FOR EACH tt-prod-cat-data:
      /* DISPLAY "" FORMAT "X(15)"
               tt-prod-cat-data.prod-cat FORMAT "X(23)"
               tt-prod-cat-data.month-act-dollars FORMAT "->>,>>>,>>9.99"
               tt-prod-cat-data.month-budget-dollars FORMAT "->>,>>>,>>9.99"
               tt-prod-cat-data.month-act-dollars - tt-prod-cat-data.month-budget-dollars FORMAT "->>,>>>,>>9.99"
               tt-prod-cat-data.ytd-act-dollars FORMAT "->>,>>>,>>9.99"
               tt-prod-cat-data.ytd-budget-dollars FORMAT "->>,>>>,>>9.99"
               tt-prod-cat-data.ytd-act-dollars - tt-prod-cat-data.ytd-budget-dollars FORMAT "->>,>>>,>>9.99"
               SKIP
          WITH DOWN FRAME frame-prod-cat-1 NO-LABELS NO-BOX STREAM-IO WIDTH 132.

       IF tb_excel THEN
       DO:
          RUN print-excel-proc("",tt-prod-cat-data.prod-cat,tt-prod-cat-data.month-act-dollars,
                               tt-prod-cat-data.month-budget-dollars,tt-prod-cat-data.ytd-act-dollars,
                               tt-prod-cat-data.ytd-budget-dollars).

          IF tg_disp_msf THEN
             RUN print-excel2-proc(tt-prod-cat-data.month-act-msf,tt-prod-cat-data.month-budget-msf,
                                   tt-prod-cat-data.ytd-act-msf,tt-prod-cat-data.ytd-budget-msf).

          IF tg_disp_tons THEN
             RUN print-excel2-proc(tt-prod-cat-data.month-act-tons,tt-prod-cat-data.month-budget-tons,
                                   tt-prod-cat-data.ytd-act-tons,tt-prod-cat-data.ytd-budget-tons).

          PUT STREAM excel UNFORMATTED SKIP.
       END. */

       ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:
                        WHEN "rep"         THEN cVarValue = "" .
                        WHEN "rep-nam"     THEN cVarValue = ""  .
                        WHEN "mth-act"     THEN cVarValue = string(tt-prod-cat-data.month-act-dollars,"->>,>>>,>>9") .                               
                        WHEN "mth-bgt"     THEN cVarValue = string(tt-prod-cat-data.month-budget-dollars,"->>,>>>,>>9") .                             
                        WHEN "mth-var"     THEN cVarValue = string((tt-prod-cat-data.month-act-dollars - tt-prod-cat-data.month-budget-dollars),"->>,>>>,>>9") . 
                        WHEN "ytd-act"     THEN cVarValue = string(tt-prod-cat-data.ytd-act-dollars,"->>,>>>,>>9") .                                  
                        WHEN "ytd-bgt"     THEN cVarValue = string(tt-prod-cat-data.ytd-budget-dollars,"->>,>>>,>>9") .                               
                        WHEN "ytd-var"     THEN cVarValue = string((tt-prod-cat-data.ytd-act-dollars - tt-prod-cat-data.ytd-budget-dollars),"->>,>>>,>>9") .     
                        WHEN "cust"        THEN cVarValue = "" .
                        WHEN "name"        THEN cVarValue = "" .
                        WHEN "cat"         THEN cVarValue = tt-prod-cat-data.prod-cat .

                        WHEN "mth-act-m"   THEN cVarValue = string(tt-prod-cat-data.month-act-msf,"->>,>>>,>>9") .                                 
                        WHEN "mth-bgt-m"   THEN cVarValue = string(tt-prod-cat-data.month-budget-msf,"->>,>>>,>>9") .                              
                        WHEN "mth-var-m"   THEN cVarValue = string((tt-prod-cat-data.month-act-msf - tt-prod-cat-data.month-budget-msf),"->>,>>>,>>9") .
                        WHEN "ytd-act-m"   THEN cVarValue = string(tt-prod-cat-data.ytd-act-msf,"->>,>>>,>>9") .                                   
                        WHEN "ytd-bgt-m"   THEN cVarValue = string(tt-prod-cat-data.ytd-budget-msf,"->>,>>>,>>9") .                                
                        WHEN "ytd-var-m"   THEN cVarValue = string((tt-prod-cat-data.ytd-act-msf - tt-prod-cat-data.ytd-budget-msf),"->>,>>>,>>9") .    

                        WHEN "mth-act-t"   THEN cVarValue = string(tt-prod-cat-data.month-act-tons,"->>,>>>,>>9") .                              
                        WHEN "mth-bgt-t"   THEN cVarValue = string(tt-prod-cat-data.month-budget-tons,"->>,>>>,>>9") .                           
                        WHEN "mth-var-t"   THEN cVarValue = string((tt-prod-cat-data.month-act-tons - tt-prod-cat-data.month-budget-tons),"->>,>>>,>>9") . 
                        WHEN "ytd-act-t"   THEN cVarValue = string(tt-prod-cat-data.ytd-act-tons,"->>,>>>,>>9") .                                
                        WHEN "ytd-bgt-t"   THEN cVarValue = string(tt-prod-cat-data.ytd-budget-tons,"->>,>>>,>>9") .                             
                        WHEN "ytd-var-t"   THEN cVarValue = string((tt-prod-cat-data.ytd-act-tons - tt-prod-cat-data.ytd-budget-tons),"->>,>>>,>>9") .     

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

   END.

 /*  DISPLAY SKIP(2)
           "" FORMAT "X(15)"
           "TOTAL" FORMAT "X(23)"
           v-gt-month-act-dol FORMAT "->>,>>>,>>9.99"
           v-gt-month-bud-dol FORMAT "->>,>>>,>>9.99"
           v-gt-month-act-dol - v-gt-month-bud-dol FORMAT "->>,>>>,>>9.99"
           v-gt-ytd-act-dol FORMAT "->>,>>>,>>9.99"
           v-gt-ytd-bud-dol FORMAT "->>,>>>,>>9.99"
           v-gt-ytd-act-dol - v-gt-ytd-bud-dol FORMAT "->>,>>>,>>9.99"
      WITH DOWN FRAME frame-p-line-tot NO-LABELS NO-BOX STREAM-IO WIDTH 132.

   DOWN WITH FRAME frame-p-line-tot. */
END.
ELSE
DO:
   FOR EACH tt-prod-cat-data:
      /* DISPLAY "" FORMAT "X(15)"
               tt-prod-cat-data.prod-cat FORMAT "X(23)"
               tt-prod-cat-data.month-act-dollars FORMAT "->>,>>>,>>9"
               tt-prod-cat-data.month-budget-dollars FORMAT "->>,>>>,>>9"
               tt-prod-cat-data.month-act-dollars - tt-prod-cat-data.month-budget-dollars FORMAT "->>,>>>,>>9"
               tt-prod-cat-data.ytd-act-dollars FORMAT "->>,>>>,>>9"
               tt-prod-cat-data.ytd-budget-dollars FORMAT "->>,>>>,>>9"
               tt-prod-cat-data.ytd-act-dollars - tt-prod-cat-data.ytd-budget-dollars FORMAT "->>,>>>,>>9"
               SKIP
          WITH DOWN FRAME frame-prod-cat-2 NO-LABELS NO-BOX STREAM-IO WIDTH 132.

       IF tb_excel THEN
       DO:
          RUN print-excel-proc("",tt-prod-cat-data.prod-cat,tt-prod-cat-data.month-act-dollars,
                               tt-prod-cat-data.month-budget-dollars,tt-prod-cat-data.ytd-act-dollars,
                               tt-prod-cat-data.ytd-budget-dollars).

          IF tg_disp_msf THEN
             RUN print-excel2-proc(tt-prod-cat-data.month-act-msf,tt-prod-cat-data.month-budget-msf,
                                   tt-prod-cat-data.ytd-act-msf,tt-prod-cat-data.ytd-budget-msf).

          IF tg_disp_tons THEN
             RUN print-excel2-proc(tt-prod-cat-data.month-act-tons,tt-prod-cat-data.month-budget-tons,
                                   tt-prod-cat-data.ytd-act-tons,tt-prod-cat-data.ytd-budget-tons).

          PUT STREAM excel UNFORMATTED SKIP.
       END. */

        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:
                        WHEN "rep"         THEN cVarValue = "" .
                        WHEN "rep-nam"     THEN cVarValue = ""  .
                        WHEN "mth-act"     THEN cVarValue = string(tt-prod-cat-data.month-act-dollars,"->>,>>>,>>9") .                               
                        WHEN "mth-bgt"     THEN cVarValue = string(tt-prod-cat-data.month-budget-dollars,"->>,>>>,>>9") .                             
                        WHEN "mth-var"     THEN cVarValue = string((tt-prod-cat-data.month-act-dollars - tt-prod-cat-data.month-budget-dollars),"->>,>>>,>>9") . 
                        WHEN "ytd-act"     THEN cVarValue = string(tt-prod-cat-data.ytd-act-dollars,"->>,>>>,>>9") .                                  
                        WHEN "ytd-bgt"     THEN cVarValue = string(tt-prod-cat-data.ytd-budget-dollars,"->>,>>>,>>9") .                               
                        WHEN "ytd-var"     THEN cVarValue = string((tt-prod-cat-data.ytd-act-dollars - tt-prod-cat-data.ytd-budget-dollars),"->>,>>>,>>9") .     
                        WHEN "cust"        THEN cVarValue = "" .
                        WHEN "name"        THEN cVarValue = "" .
                        WHEN "cat"         THEN cVarValue = tt-prod-cat-data.prod-cat .

                        WHEN "mth-act-m"   THEN cVarValue = string(tt-prod-cat-data.month-act-msf,"->>,>>>,>>9") .                                 
                        WHEN "mth-bgt-m"   THEN cVarValue = string(tt-prod-cat-data.month-budget-msf,"->>,>>>,>>9") .                              
                        WHEN "mth-var-m"   THEN cVarValue = string((tt-prod-cat-data.month-act-msf - tt-prod-cat-data.month-budget-msf),"->>,>>>,>>9") .
                        WHEN "ytd-act-m"   THEN cVarValue = string(tt-prod-cat-data.ytd-act-msf,"->>,>>>,>>9") .                                   
                        WHEN "ytd-bgt-m"   THEN cVarValue = string(tt-prod-cat-data.ytd-budget-msf,"->>,>>>,>>9") .                                
                        WHEN "ytd-var-m"   THEN cVarValue = string((tt-prod-cat-data.ytd-act-msf - tt-prod-cat-data.ytd-budget-msf),"->>,>>>,>>9") .    

                        WHEN "mth-act-t"   THEN cVarValue = string(tt-prod-cat-data.month-act-tons,"->>,>>>,>>9") .                              
                        WHEN "mth-bgt-t"   THEN cVarValue = string(tt-prod-cat-data.month-budget-tons,"->>,>>>,>>9") .                           
                        WHEN "mth-var-t"   THEN cVarValue = string((tt-prod-cat-data.month-act-tons - tt-prod-cat-data.month-budget-tons),"->>,>>>,>>9") . 
                        WHEN "ytd-act-t"   THEN cVarValue = string(tt-prod-cat-data.ytd-act-tons,"->>,>>>,>>9") .                                
                        WHEN "ytd-bgt-t"   THEN cVarValue = string(tt-prod-cat-data.ytd-budget-tons,"->>,>>>,>>9") .                             
                        WHEN "ytd-var-t"   THEN cVarValue = string((tt-prod-cat-data.ytd-act-tons - tt-prod-cat-data.ytd-budget-tons),"->>,>>>,>>9") .     

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

   END.

  /* DISPLAY SKIP(2)
           "" FORMAT "X(15)"
           "TOTAL" FORMAT "X(23)"
           v-gt-month-act-dol FORMAT "->>,>>>,>>9"
           v-gt-month-bud-dol FORMAT "->>,>>>,>>9"
           v-gt-month-act-dol - v-gt-month-bud-dol FORMAT "->>,>>>,>>9"
           v-gt-ytd-act-dol FORMAT "->>,>>>,>>9"
           v-gt-ytd-bud-dol FORMAT "->>,>>>,>>9"
           v-gt-ytd-act-dol - v-gt-ytd-bud-dol FORMAT "->>,>>>,>>9"
      WITH DOWN FRAME frame-p-line-tot-2 NO-LABELS NO-BOX STREAM-IO WIDTH 132.

   DOWN WITH FRAME frame-p-line-tot-2. */
END.

/*IF tb_excel THEN
DO:
   PUT STREAM excel UNFORMATTED SKIP(1).

   RUN print-excel-proc("","TOTAL",v-gt-month-act-dol,v-gt-month-bud-dol,
                        v-gt-ytd-act-dol,v-gt-ytd-bud-dol).

   IF tg_disp_msf THEN
      RUN print-excel2-proc(v-gt-month-act-msf,v-gt-month-bud-msf,v-gt-ytd-act-msf,v-gt-ytd-bud-msf).

   IF tg_disp_tons THEN
      RUN print-excel2-proc(v-gt-month-act-tons,v-gt-month-bud-tons,v-gt-ytd-act-tons,v-gt-ytd-bud-tons).
END. */

PUT SKIP str-line SKIP .
 ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:
                        WHEN "rep"         THEN cVarValue = "" .
                        WHEN "rep-nam"     THEN cVarValue = "" .
                        WHEN "mth-act"     THEN cVarValue = string(v-gt-month-act-dol,"->>,>>>,>>9") .                               
                        WHEN "mth-bgt"     THEN cVarValue = string(v-gt-month-bud-dol,"->>,>>>,>>9") .                             
                        WHEN "mth-var"     THEN cVarValue = string((v-gt-month-act-dol - v-gt-month-bud-dol),"->>,>>>,>>9") . 
                        WHEN "ytd-act"     THEN cVarValue = string(v-gt-ytd-act-dol,"->>,>>>,>>9") .                                  
                        WHEN "ytd-bgt"     THEN cVarValue = string(v-gt-ytd-bud-dol,"->>,>>>,>>9") .                               
                        WHEN "ytd-var"     THEN cVarValue = string((v-gt-ytd-act-dol - v-gt-ytd-bud-dol),"->>,>>>,>>9") .     
                        WHEN "cust"        THEN cVarValue = "" .
                        WHEN "name"        THEN cVarValue = "" .
                        WHEN "cat"         THEN cVarValue = "" .

                        WHEN "mth-act-m"   THEN cVarValue = string(v-gt-month-act-msf,"->>,>>>,>>9") .                                 
                        WHEN "mth-bgt-m"   THEN cVarValue = string(v-gt-month-bud-msf,"->>,>>>,>>9") .                              
                        WHEN "mth-var-m"   THEN cVarValue = string((v-gt-month-act-msf - v-gt-month-bud-msf),"->>,>>>,>>9") .
                        WHEN "ytd-act-m"   THEN cVarValue = string(v-gt-ytd-act-msf,"->>,>>>,>>9") .                                   
                        WHEN "ytd-bgt-m"   THEN cVarValue = string(v-gt-ytd-bud-msf,"->>,>>>,>>9") .                                
                        WHEN "ytd-var-m"   THEN cVarValue = string((v-gt-ytd-act-msf - v-gt-ytd-bud-msf),"->>,>>>,>>9") .    

                        WHEN "mth-act-t"   THEN cVarValue = string(v-gt-month-act-tons,"->>,>>>,>>9") .                              
                        WHEN "mth-bgt-t"   THEN cVarValue = string(v-gt-month-bud-tons,"->>,>>>,>>9") .                           
                        WHEN "mth-var-t"   THEN cVarValue = string((v-gt-month-act-tons - v-gt-month-bud-tons),"->>,>>>,>>9") . 
                        WHEN "ytd-act-t"   THEN cVarValue = string(v-gt-ytd-act-tons,"->>,>>>,>>9") .                                
                        WHEN "ytd-bgt-t"   THEN cVarValue = string(v-gt-ytd-bud-tons,"->>,>>>,>>9") .                             
                        WHEN "ytd-var-t"   THEN cVarValue = string((v-gt-ytd-act-tons - v-gt-ytd-bud-tons),"->>,>>>,>>9") .     

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED " TOTALS" substring(cDisplay,8,350) SKIP.
            IF tb_excel THEN DO:
                PUT STREAM excel UNFORMATTED  
                    "TOTAL " + substring(cExcelDisplay,3,300) SKIP.
            END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintDetail C-Win 
PROCEDURE PrintDetail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH tt-data WHERE
           tt-data.sman-no EQ tt-total-data.sman-no AND
           (tt-data.prod-cat EQ tt-total-data.prod-cat AND rd_sortby EQ "PC"
            OR tt-data.cust-no EQ tt-total-data.cust-no AND rd_sortby EQ "CU")
           NO-LOCK:

       {custom/statusMsg.i " 'Processing Sales Rep#  '  + tt-data.sman-no "}

          IF NOT(NOT tg_display-zero AND tt-data.month-act-dollars EQ 0 AND
             tt-data.month-budget-dollars EQ 0 AND tt-data.ytd-act-dollars EQ 0 AND
             tt-data.ytd-budget-dollars EQ 0) THEN
             DO:
                IF tg_disp_cents THEN
                DO:
                  /* DISPLAY (IF rd_sortby EQ "PC" THEN tt-data.cust-no ELSE tt-data.prod-cat) FORMAT "X(8)"
                           (IF rd_sortby EQ "PC" THEN tt-data.cust-name ELSE "") FORMAT "X(30)"
                           tt-data.month-act-dollars FORMAT "->>,>>>,>>9.99"
                           tt-data.month-budget-dollars FORMAT "->>,>>>,>>9.99"
                           tt-data.month-act-dollars - tt-data.month-budget-dollars FORMAT "->>,>>>,>>9.99"
                           tt-data.ytd-act-dollars FORMAT "->>,>>>,>>9.99"
                           tt-data.ytd-budget-dollars FORMAT "->>,>>>,>>9.99"
                           tt-data.ytd-act-dollars - tt-data.ytd-budget-dollars FORMAT "->>,>>>,>>9.99"
                      WITH DOWN FRAME frame-cust-line NO-LABELS NO-BOX STREAM-IO WIDTH 132.

                   DOWN WITH FRAME frame-cust-line. */
                END.
                ELSE
                DO:
                  /* DISPLAY (IF rd_sortby EQ "PC" THEN tt-data.cust-no ELSE tt-data.prod-cat) FORMAT "X(8)"
                           (IF rd_sortby EQ "PC" THEN tt-data.cust-name ELSE "") FORMAT "X(30)"
                           tt-data.month-act-dollars FORMAT "->>,>>>,>>9"
                           tt-data.month-budget-dollars FORMAT "->>,>>>,>>9"
                           tt-data.month-act-dollars - tt-data.month-budget-dollars FORMAT "->>,>>>,>>9"
                           tt-data.ytd-act-dollars FORMAT "->>,>>>,>>9"
                           tt-data.ytd-budget-dollars FORMAT "->>,>>>,>>9"
                           tt-data.ytd-act-dollars - tt-data.ytd-budget-dollars FORMAT "->>,>>>,>>9"
                      WITH DOWN FRAME frame-cust-line-2 NO-LABELS NO-BOX STREAM-IO WIDTH 132.

                   DOWN WITH FRAME frame-cust-line-2. */
                END.

               /* IF tb_excel THEN
                DO: 
                   IF rd_sortby EQ "PC" THEN 
                       RUN print-excel-proc(tt-data.cust-no,tt-data.cust-name,tt-data.month-act-dollars,tt-data.month-budget-dollars,
                                        tt-data.ytd-act-dollars,tt-data.ytd-budget-dollars).
                   ELSE
                       RUN print-excel-proc(tt-data.prod-cat,"",tt-data.month-act-dollars,tt-data.month-budget-dollars,
                                        tt-data.ytd-act-dollars,tt-data.ytd-budget-dollars).

                   IF tg_disp_msf THEN
                      RUN print-excel2-proc(tt-data.month-act-msf,tt-data.month-budget-msf,tt-data.ytd-act-msf,tt-data.ytd-budget-msf).

                   IF tg_disp_tons THEN
                      RUN print-excel2-proc(tt-data.month-act-tons,tt-data.month-budget-tons,tt-data.ytd-act-tons,tt-data.ytd-budget-tons).

                   PUT STREAM excel UNFORMATTED SKIP.
                END. */

                ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:
                        WHEN "rep"         THEN cVarValue = tt-total-data.sman-no .
                        WHEN "rep-nam"     THEN cVarValue = IF AVAIL sman THEN STRING(sman.sname,"X(25)") ELSE "". 
                        WHEN "mth-act"     THEN cVarValue = string(tt-data.month-act-dollars,"->>,>>>,>>9") .                               
                        WHEN "mth-bgt"     THEN cVarValue = string(tt-data.month-budget-dollars,"->>,>>>,>>9") .                             
                        WHEN "mth-var"     THEN cVarValue = string((tt-data.month-act-dollars - tt-data.month-budget-dollars),"->>,>>>,>>9") . 
                        WHEN "ytd-act"     THEN cVarValue = string(tt-data.ytd-act-dollars,"->>,>>>,>>9") .                                  
                        WHEN "ytd-bgt"     THEN cVarValue = string(tt-data.ytd-budget-dollars,"->>,>>>,>>9") .                               
                        WHEN "ytd-var"     THEN cVarValue = string((tt-data.ytd-act-dollars - tt-data.ytd-budget-dollars),"->>,>>>,>>9") .     
                        WHEN "cust"        THEN cVarValue = string(tt-data.cust-no) .
                        WHEN "name"        THEN cVarValue = string(tt-data.cust-name,"x(30)") .
                        WHEN "cat"         THEN cVarValue = tt-data.prod-cat .

                        WHEN "mth-act-m"   THEN cVarValue = string(tt-data.month-act-msf,"->>,>>>,>>9") .                                 
                        WHEN "mth-bgt-m"   THEN cVarValue = string(tt-data.month-budget-msf,"->>,>>>,>>9") .                              
                        WHEN "mth-var-m"   THEN cVarValue = string((tt-data.month-act-msf - tt-data.month-budget-msf),"->>,>>>,>>9") .
                        WHEN "ytd-act-m"   THEN cVarValue = string(tt-data.ytd-act-msf,"->>,>>>,>>9") .                                   
                        WHEN "ytd-bgt-m"   THEN cVarValue = string(tt-data.ytd-budget-msf,"->>,>>>,>>9") .                                
                        WHEN "ytd-var-m"   THEN cVarValue = string((tt-data.ytd-act-msf - tt-data.ytd-budget-msf),"->>,>>>,>>9") .    

                        WHEN "mth-act-t"   THEN cVarValue = string(tt-data.month-act-tons,"->>,>>>,>>9") .                              
                        WHEN "mth-bgt-t"   THEN cVarValue = string(tt-data.month-budget-tons,"->>,>>>,>>9") .                           
                        WHEN "mth-var-t"   THEN cVarValue = string((tt-data.month-act-tons - tt-data.month-budget-tons),"->>,>>>,>>9") . 
                        WHEN "ytd-act-t"   THEN cVarValue = string(tt-data.ytd-act-tons,"->>,>>>,>>9") .                                
                        WHEN "ytd-bgt-t"   THEN cVarValue = string(tt-data.ytd-budget-tons,"->>,>>>,>>9") .                             
                        WHEN "ytd-var-t"   THEN cVarValue = string((tt-data.ytd-act-tons - tt-data.ytd-budget-tons),"->>,>>>,>>9") .     

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
           END.
       END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE round-proc C-Win 
PROCEDURE round-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH tt-data:
       ASSIGN
          tt-data.month-act-dollars = ROUND(tt-data.month-act-dollars,0)
          tt-data.month-budget-dollars = ROUND(tt-data.month-budget-dollars,0)
          tt-data.ytd-act-dollars = ROUND(tt-data.ytd-act-dollars,0)
          tt-data.ytd-budget-dollars = ROUND(tt-data.ytd-budget-dollars,0).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */
DEF VAR v-head-1 AS CHAR FORMAT "X(132)" NO-UNDO.
DEF VAR v-head-2 AS CHAR FORMAT "X(132)" NO-UNDO.
DEF VAR v-head-3 AS CHAR FORMAT "X(132)" NO-UNDO.

DEF VAR v-sls-month-act-dol-tot AS DEC NO-UNDO.
DEF VAR v-sls-month-bud-dol-tot AS DEC NO-UNDO.
DEF VAR v-sls-ytd-act-dol-tot AS DEC NO-UNDO.
DEF VAR v-sls-ytd-bud-dol-tot AS DEC NO-UNDO.
DEF VAR v-sls-month-act-msf-tot AS DEC NO-UNDO.
DEF VAR v-sls-month-bud-msf-tot AS DEC NO-UNDO.
DEF VAR v-sls-ytd-act-msf-tot AS DEC NO-UNDO.
DEF VAR v-sls-ytd-bud-msf-tot AS DEC NO-UNDO.
DEF VAR v-sls-month-act-tons-tot AS DEC NO-UNDO.
DEF VAR v-sls-month-bud-tons-tot AS DEC NO-UNDO.
DEF VAR v-sls-ytd-act-tons-tot AS DEC NO-UNDO.
DEF VAR v-sls-ytd-bud-tons-tot AS DEC NO-UNDO.
DEF VAR v-month-act-dol-tot AS DEC NO-UNDO.
DEF VAR v-month-bud-dol-tot AS DEC NO-UNDO.
DEF VAR v-ytd-act-dol-tot AS DEC NO-UNDO.
DEF VAR v-ytd-bud-dol-tot AS DEC NO-UNDO.
DEF VAR v-month-act-msf-tot AS DEC NO-UNDO.
DEF VAR v-month-bud-msf-tot AS DEC NO-UNDO.
DEF VAR v-ytd-act-msf-tot AS DEC NO-UNDO.
DEF VAR v-ytd-bud-msf-tot AS DEC NO-UNDO.
DEF VAR v-month-act-tons-tot AS DEC NO-UNDO.
DEF VAR v-month-bud-tons-tot AS DEC NO-UNDO.
DEF VAR v-ytd-act-tons-tot AS DEC NO-UNDO.
DEF VAR v-ytd-bud-tons-tot AS DEC NO-UNDO.
DEF VAR v-space AS INT NO-UNDO.
DEF VAR cHeadTemp1 AS CHAR NO-UNDO.
DEF VAR cHeadTemp2 AS CHAR NO-UNDO.
DEF VAR excelheader1 AS CHAR NO-UNDO.
DEF VAR excelheader2 AS CHAR NO-UNDO.
DEF VAR excelheader3 AS CHAR NO-UNDO.

{custom/statusMsg.i " 'Processing...   '"}

cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF VARIABLE excelheader AS CHAR NO-UNDO.

ASSIGN
    str-tit4 = " " 
    str-tit5 = " " 
    str-line = " " .

ASSIGN
   v-gt-month-act-dol = 0
   v-gt-month-bud-dol = 0
   v-gt-ytd-act-dol = 0
   v-gt-ytd-bud-dol = 0
   v-gt-month-act-msf = 0
   v-gt-month-bud-msf = 0
   v-gt-ytd-act-msf = 0
   v-gt-ytd-bud-msf = 0
   v-gt-month-act-tons = 0
   v-gt-month-bud-tons = 0
   v-gt-ytd-act-tons = 0
   v-gt-ytd-bud-tons = 0.

/*{sys/form/r-topsw.f}*/
{sys/form/r-top5DL3.f}

format header
   v-head-1 skip
   v-head-2 SKIP
   v-head-3
   fill("_",132) format "x(132)"
   with no-labels no-box no-underline stream-io width 132 frame f-top page-top.
ASSIGN
   str-tit2 = c-win:TITLE
   {sys/inc/ctrtext.i str-tit2 112}.
ASSIGN str-tit3 = STRING(begin_date) + "-" + STRING(END_date) .
   {sys/inc/ctrtext.i str-tit3 130}.


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

        IF LOOKUP(ttRptSelected.TextList, "Mnth Acl $,Mnth Bgt$,Mnth Var $,YTD Acl $,YTD Bgt $,YTD Var $," +
                                          "Mnth Acl MSF,Mnth Bgt MSF,Mnth Var MSF,YTD Acl MSF,YTD Bgt MSF,YTD Var MSF," +
                                          "Mnth Acl Ton,Mnth Bgt Ton,Mnth Var Ton,YTD Acl Ton,YTD Bgt Ton,YTD Var Ton") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

/*DISPLAY  "TESTING" AT 60.*/
display "" with frame r-top.

IF NOT tg_disp_cents THEN
   v-space = 3.

IF tb_excel THEN do:
  OUTPUT STREAM excel TO VALUE(fi_file).
   /* ASSIGN excelheader1 = "SM #,SalesRep nm.,Month,Month,Month,YTD,YTD,YTD".
        IF rd_sortby EQ "PC" THEN
            ASSIGN 
            excelheader2 = ",Category,Actual $,Budget $,Var. $,Actual $,Budget $,Var. $"
            excelheader3 = "Cu.#,Customer nm.".
        ELSE
            ASSIGN 
            excelheader2 = ",Customer #.,Actual $,Budget $,Var. $,Actual $,Budget $,Var. $"
            excelheader3 = "Category".
    IF tg_disp_msf THEN
     ASSIGN
        excelheader1 = excelheader1 + ",Month,Month,Month,YTD,YTD,YTD"
        excelheader2 = excelheader2 + ",Actual MSF,Budget MSF,Var. MSF,Actual MSF,Budget MSF,Var. MSF".
    IF tg_disp_tons THEN
     ASSIGN
        excelheader1 = excelheader1 + ",Month,Month,Month,YTD,YTD,YTD"
        excelheader2 = excelheader2 + ",Actual Tons,Budget Tons,Var. Tons,Actual Tons,Budget Tons,Var. Tons".
    IF tb_excel THEN PUT STREAM excel UNFORMATTED excelheader1 SKIP excelheader2 SKIP excelheader3 SKIP. */
PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.
/*IF rd_sortby EQ "PC" THEN
    ASSIGN 
        cHeadTemp1 = "         Category"
        cHeadTemp2 = "Cu.#     Customer nm.".
ELSE
    ASSIGN 
        cHeadTemp1 = "         Cust #  "
        cHeadTemp2 = "         Category".

ASSIGN
   v-head-1 = "SM #     SalesRep nm." + FILL(" ",28 - v-space) + "Month" + FILL(" ",10 - v-space) + "Month" + FILL(" ",10 - v-space) + "Month" + FILL(" ",12 - v-space) + "YTD" + FILL(" ",12 - v-space) + "YTD" + FILL(" ",12 - v-space) + "YTD"
   v-head-2 = cHeadTemp1 + FILL(" ",31 - v-space) + "Actual" + FILL(" ",9 - v-space) + "Budget" + FILL(" ",11 - v-space) + "Var." + FILL(" ",9 - v-space) + "Actual" + FILL(" ",9 - v-space) +  "Budget" + FILL(" ",12 - v-space)  + "Var."
   v-head-3 = cHeadTemp2 . 

DISPLAY "" WITH FRAME f-top. */
EMPTY TEMP-TABLE tt-data.
EMPTY TEMP-TABLE tt-total-data.
EMPTY TEMP-TABLE tt-prod-cat-data.
RUN init-proc.
IF tg_disp_cents EQ NO THEN
   RUN round-proc.

RUN total-proc.
FOR EACH tt-total-data
    BREAK BY tt-total-data.sman-no:

    {custom/statusMsg.i " 'Processing Sales Rep#  '  + tt-total-data.sman-no "}

    IF FIRST-OF(tt-total-data.sman-no) THEN
    DO:
       ASSIGN
          v-sls-month-act-dol-tot = 0
          v-sls-month-bud-dol-tot = 0
          v-sls-ytd-act-dol-tot = 0
          v-sls-ytd-bud-dol-tot = 0
          v-sls-month-act-msf-tot = 0
          v-sls-month-bud-msf-tot = 0
          v-sls-ytd-act-msf-tot = 0
          v-sls-ytd-bud-msf-tot = 0
          v-sls-month-act-tons-tot = 0
          v-sls-month-bud-tons-tot = 0
          v-sls-ytd-act-tons-tot = 0
          v-sls-ytd-bud-tons-tot = 0.

       FIND FIRST sman WHERE
            sman.company EQ cocode AND
            sman.sman EQ tt-total-data.sman-no
            NO-LOCK NO-ERROR.

     /*  DISPLAY tt-total-data.sman-no FORMAT "X(3)"
               space(6) sman.sname FORMAT "X(20)" WHEN AVAIL sman
               WITH DOWN FRAME frame-sman NO-BOX STREAM-IO NO-LABELS WIDTH 132.

       DOWN WITH FRAME frame-sman. */


     /*  IF rs_detail THEN
          PUT SKIP(2).
       ELSE
          PUT SKIP(1). */

      /* IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              SKIP(1)
              '"' tt-total-data.sman-no '",'
              '"' REPLACE(sman.sname,","," ") '",'
              SKIP(1). */
    END.

    IF rs_detail THEN /*Print customer line*/
    DO:
        RUN PrintDetail.
    END.

    ASSIGN
       v-sls-month-act-dol-tot = v-sls-month-act-dol-tot + tt-total-data.month-act-dollars
       v-sls-month-bud-dol-tot = v-sls-month-bud-dol-tot + tt-total-data.month-budget-dollars
       v-sls-ytd-act-dol-tot = v-sls-ytd-act-dol-tot + tt-total-data.ytd-act-dollars
       v-sls-ytd-bud-dol-tot = v-sls-ytd-bud-dol-tot + tt-total-data.ytd-budget-dollars
       v-month-act-dol-tot = v-month-act-dol-tot + tt-total-data.month-act-dollars
       v-month-bud-dol-tot = v-month-bud-dol-tot + tt-total-data.month-budget-dollars
       v-ytd-act-dol-tot = v-ytd-act-dol-tot + tt-total-data.ytd-act-dollars
       v-ytd-bud-dol-tot = v-ytd-bud-dol-tot + tt-total-data.ytd-budget-dollars.

  /*  IF tg_disp_msf THEN */
       ASSIGN
          v-sls-month-act-msf-tot = v-sls-month-act-msf-tot + tt-total-data.month-act-msf
          v-sls-month-bud-msf-tot = v-sls-month-bud-msf-tot + tt-total-data.month-budget-msf
          v-sls-ytd-act-msf-tot = v-sls-ytd-act-msf-tot + tt-total-data.ytd-act-msf
          v-sls-ytd-bud-msf-tot = v-sls-ytd-bud-msf-tot + tt-total-data.ytd-budget-msf
          v-month-act-msf-tot = v-month-act-msf-tot + tt-total-data.month-act-msf
          v-month-bud-msf-tot = v-month-bud-msf-tot + tt-total-data.month-budget-msf
          v-ytd-act-msf-tot = v-ytd-act-msf-tot + tt-total-data.ytd-act-msf
          v-ytd-bud-msf-tot = v-ytd-bud-msf-tot + tt-total-data.ytd-budget-msf.

  /*  IF tg_disp_tons THEN */
       ASSIGN
          v-sls-month-act-tons-tot = v-sls-month-act-tons-tot + tt-total-data.month-act-tons
          v-sls-month-bud-tons-tot = v-sls-month-bud-tons-tot + tt-total-data.month-budget-tons
          v-sls-ytd-act-tons-tot = v-sls-ytd-act-tons-tot + tt-total-data.ytd-act-tons
          v-sls-ytd-bud-tons-tot = v-sls-ytd-bud-tons-tot + tt-total-data.ytd-budget-tons
          v-month-act-tons-tot = v-month-act-tons-tot + tt-total-data.month-act-tons
          v-month-bud-tons-tot = v-month-bud-tons-tot + tt-total-data.month-budget-tons
          v-ytd-act-tons-tot = v-ytd-act-tons-tot + tt-total-data.ytd-act-tons
          v-ytd-bud-tons-tot = v-ytd-bud-tons-tot + tt-total-data.ytd-budget-tons.

    /*Print product category total for salesrep*/
    IF tg_disp_cents THEN
    DO:
       IF rs_detail THEN
          PUT SKIP(1).

      /* DISPLAY "" FORMAT "X(8)"
               (IF rd_sortby EQ "PC" THEN tt-total-data.prod-cat ELSE tt-total-data.cust-no) FORMAT "X(30)"
               tt-total-data.month-act-dollars FORMAT "->>,>>>,>>9.99"
               tt-total-data.month-budget-dollars FORMAT "->>,>>>,>>9.99"
               tt-total-data.month-act-dollars - tt-total-data.month-budget-dollars  FORMAT "->>,>>>,>>9.99"
               tt-total-data.ytd-act-dollars FORMAT "->>,>>>,>>9.99"
               tt-total-data.ytd-budget-dollars FORMAT "->>,>>>,>>9.99"
               tt-total-data.ytd-act-dollars - tt-total-data.ytd-budget-dollars  FORMAT "->>,>>>,>>9.99"
          WITH DOWN FRAME frame-prod-line-tot NO-LABELS NO-BOX STREAM-IO WIDTH 132. 

       DOWN WITH FRAME frame-prod-line-tot. */


     /*  IF rs_detail THEN
          PUT SKIP(1).
       ELSE
          PUT SKIP. */
    END.
    ELSE
    DO:
       IF rs_detail THEN
          PUT SKIP(1).

     /*  DISPLAY "" FORMAT "X(8)"
               (IF rd_sortby EQ "PC" THEN tt-total-data.prod-cat ELSE tt-total-data.cust-no) FORMAT "X(30)"
               tt-total-data.month-act-dollars FORMAT "->>,>>>,>>9"
               tt-total-data.month-budget-dollars FORMAT "->>,>>>,>>9"
               tt-total-data.month-act-dollars - tt-total-data.month-budget-dollars  FORMAT "->>,>>>,>>9"
               tt-total-data.ytd-act-dollars FORMAT "->>,>>>,>>9"
               tt-total-data.ytd-budget-dollars FORMAT "->>,>>>,>>9"
               tt-total-data.ytd-act-dollars - tt-total-data.ytd-budget-dollars FORMAT "->>,>>>,>>9"
          WITH DOWN FRAME frame-prod-line-tot-2 NO-LABELS NO-BOX STREAM-IO WIDTH 132.

       DOWN WITH FRAME frame-prod-line-tot-2. */

       IF rs_detail THEN
          PUT SKIP(1).
       ELSE
          PUT SKIP.
    END.

  /*  IF tb_excel THEN
    DO:
       IF rs_detail THEN
          PUT STREAM excel UNFORMATTED SKIP(1).

       IF rd_sortby EQ "PC" THEN 
           RUN print-excel-proc("",tt-total-data.prod-cat,tt-total-data.month-act-dollars,tt-total-data.month-budget-dollars,
                            tt-total-data.ytd-act-dollars,tt-total-data.ytd-budget-dollars).
       ELSE
           RUN print-excel-proc("",tt-total-data.cust-no,tt-total-data.month-act-dollars,tt-total-data.month-budget-dollars,
                            tt-total-data.ytd-act-dollars,tt-total-data.ytd-budget-dollars).

       IF tg_disp_msf THEN
          RUN print-excel2-proc(tt-total-data.month-act-msf,tt-total-data.month-budget-msf,tt-total-data.ytd-act-msf,tt-total-data.ytd-budget-msf).

       IF tg_disp_tons THEN
          RUN print-excel2-proc(tt-total-data.month-act-tons,tt-total-data.month-budget-tons,tt-total-data.ytd-act-tons,tt-total-data.ytd-budget-tons).

       PUT STREAM excel UNFORMATTED SKIP.

       IF rs_detail THEN
          PUT STREAM excel UNFORMATTED SKIP(1).
    END. */

    ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:
                        WHEN "rep"         THEN cVarValue = tt-total-data.sman-no .
                        WHEN "rep-nam"     THEN cVarValue = IF AVAIL sman THEN STRING(sman.sname,"X(25)") ELSE "". 
                        WHEN "mth-act"     THEN cVarValue = string(tt-total-data.month-act-dollars,"->>,>>>,>>9") .                               
                        WHEN "mth-bgt"     THEN cVarValue = string(tt-total-data.month-budget-dollars,"->>,>>>,>>9") .                             
                        WHEN "mth-var"     THEN cVarValue = string((tt-total-data.month-act-dollars - tt-total-data.month-budget-dollars),"->>,>>>,>>9") . 
                        WHEN "ytd-act"     THEN cVarValue = string(tt-total-data.ytd-act-dollars,"->>,>>>,>>9") .                                  
                        WHEN "ytd-bgt"     THEN cVarValue = string(tt-total-data.ytd-budget-dollars,"->>,>>>,>>9") .                               
                        WHEN "ytd-var"     THEN cVarValue = string((tt-total-data.ytd-act-dollars - tt-total-data.ytd-budget-dollars),"->>,>>>,>>9") .     
                        WHEN "cust"        THEN cVarValue = string(tt-total-data.cust-no) .
                        WHEN "name"        THEN cVarValue = "" .
                        WHEN "cat"         THEN cVarValue = tt-total-data.prod-cat .

                        WHEN "mth-act-m"   THEN cVarValue = string(tt-total-data.month-act-msf,"->>,>>>,>>9") .                                 
                        WHEN "mth-bgt-m"   THEN cVarValue = string(tt-total-data.month-budget-msf,"->>,>>>,>>9") .                              
                        WHEN "mth-var-m"   THEN cVarValue = string((tt-total-data.month-act-msf - tt-total-data.month-budget-msf),"->>,>>>,>>9") .
                        WHEN "ytd-act-m"   THEN cVarValue = string(tt-total-data.ytd-act-msf,"->>,>>>,>>9") .                                   
                        WHEN "ytd-bgt-m"   THEN cVarValue = string(tt-total-data.ytd-budget-msf,"->>,>>>,>>9") .                                
                        WHEN "ytd-var-m"   THEN cVarValue = string((tt-total-data.ytd-act-msf - tt-total-data.ytd-budget-msf),"->>,>>>,>>9") .    

                        WHEN "mth-act-t"   THEN cVarValue = string(tt-total-data.month-act-tons,"->>,>>>,>>9") .                              
                        WHEN "mth-bgt-t"   THEN cVarValue = string(tt-total-data.month-budget-tons,"->>,>>>,>>9") .                           
                        WHEN "mth-var-t"   THEN cVarValue = string((tt-total-data.month-act-tons - tt-total-data.month-budget-tons),"->>,>>>,>>9") . 
                        WHEN "ytd-act-t"   THEN cVarValue = string(tt-total-data.ytd-act-tons,"->>,>>>,>>9") .                                
                        WHEN "ytd-bgt-t"   THEN cVarValue = string(tt-total-data.ytd-budget-tons,"->>,>>>,>>9") .                             
                        WHEN "ytd-var-t"   THEN cVarValue = string((tt-total-data.ytd-act-tons - tt-total-data.ytd-budget-tons),"->>,>>>,>>9") .     

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

    FIND FIRST tt-prod-cat-data WHERE
         tt-prod-cat-data.prod-cat EQ tt-total-data.prod-cat
         NO-ERROR.

    IF NOT AVAIL tt-prod-cat-data THEN
    DO:
       CREATE tt-prod-cat-data.
       tt-prod-cat-data.prod-cat = tt-total-data.prod-cat.
    END.

    ASSIGN
       tt-prod-cat-data.month-act-dollars = tt-prod-cat-data.month-act-dollars
                                          + tt-total-data.month-act-dollars
       tt-prod-cat-data.month-budget-dollars = tt-prod-cat-data.month-budget-dollars
                                             + tt-total-data.month-budget-dollars
       tt-prod-cat-data.ytd-act-dollars = tt-prod-cat-data.ytd-act-dollars
                                        + tt-total-data.ytd-act-dollars
       tt-prod-cat-data.ytd-budget-dollars = tt-prod-cat-data.ytd-budget-dollars
                                           + tt-total-data.ytd-budget-dollars. 

   /* IF tg_disp_msf THEN */
       ASSIGN
          tt-prod-cat-data.month-act-msf = tt-prod-cat-data.month-act-msf
                                         + tt-total-data.month-act-msf
          tt-prod-cat-data.month-budget-msf = tt-prod-cat-data.month-budget-msf
                                            + tt-total-data.month-budget-msf
          tt-prod-cat-data.ytd-act-msf = tt-prod-cat-data.ytd-act-msf
                                       + tt-total-data.ytd-act-msf
          tt-prod-cat-data.ytd-budget-msf = tt-prod-cat-data.ytd-budget-msf
                                          + tt-total-data.ytd-budget-msf.

  /*  IF tg_disp_tons THEN */
       ASSIGN
          tt-prod-cat-data.month-act-tons = tt-prod-cat-data.month-act-tons
                                          + tt-total-data.month-act-tons
          tt-prod-cat-data.month-budget-tons = tt-prod-cat-data.month-budget-tons
                                             + tt-total-data.month-budget-tons
          tt-prod-cat-data.ytd-act-tons = tt-prod-cat-data.ytd-act-tons
                                        + tt-total-data.ytd-act-tons
          tt-prod-cat-data.ytd-budget-tons = tt-prod-cat-data.ytd-budget-tons
                                           + tt-total-data.ytd-budget-tons.

    IF LAST-OF(tt-total-data.sman-no) THEN
    DO:
       /*Print salesrep total*/
       IF tg_disp_cents THEN
       DO:
          FIND FIRST sman WHERE
               sman.company EQ cocode AND
               sman.sman EQ tt-total-data.sman-no
               NO-LOCK NO-ERROR.

        /*  DISPLAY SKIP(1)
                  "TOTAL" FORMAT "X(8)"
                  (IF AVAIL sman THEN sman.sname ELSE "") FORMAT "X(30)"
                  v-sls-month-act-dol-tot FORMAT "->>,>>>,>>9.99"
                  v-sls-month-bud-dol-tot FORMAT "->>,>>>,>>9.99"
                  v-sls-month-act-dol-tot - v-sls-month-bud-dol-tot FORMAT "->>,>>>,>>9.99"
                  v-sls-ytd-act-dol-tot FORMAT "->>,>>>,>>9.99"
                  v-sls-ytd-bud-dol-tot FORMAT "->>,>>>,>>9.99"
                  v-sls-ytd-act-dol-tot - v-sls-ytd-bud-dol-tot FORMAT "->>,>>>,>>9.99"
                  SKIP(1)
             WITH DOWN FRAME frame-sman-line-tot NO-LABELS NO-BOX STREAM-IO WIDTH 132.

          DOWN WITH FRAME frame-sman-line-tot. */
       END.
       ELSE
       DO:
          FIND FIRST sman WHERE
               sman.company EQ cocode AND
               sman.sman EQ tt-total-data.sman-no
               NO-LOCK NO-ERROR.

        /*  DISPLAY SKIP(1)
                  "TOTAL" FORMAT "X(8)"
                  (IF AVAIL sman THEN sman.sname ELSE "") FORMAT "X(30)"
                  v-sls-month-act-dol-tot FORMAT "->>,>>>,>>9"
                  v-sls-month-bud-dol-tot FORMAT "->>,>>>,>>9"
                  v-sls-month-act-dol-tot - v-sls-month-bud-dol-tot FORMAT "->>,>>>,>>9"
                  v-sls-ytd-act-dol-tot FORMAT "->>,>>>,>>9"
                  v-sls-ytd-bud-dol-tot FORMAT "->>,>>>,>>9"
                  v-sls-ytd-act-dol-tot - v-sls-ytd-bud-dol-tot FORMAT "->>,>>>,>>9"
                  SKIP(1)
             WITH DOWN FRAME frame-sman-line-tot-2 NO-LABELS NO-BOX STREAM-IO WIDTH 132.

          DOWN WITH FRAME frame-sman-line-tot-2. */
       END.

       PUT SKIP str-line SKIP .
       ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:
                        WHEN "rep"         THEN cVarValue = "" .
                        WHEN "rep-nam"     THEN cVarValue = "" .
                        WHEN "mth-act"     THEN cVarValue = string(v-sls-month-act-dol-tot,"->>,>>>,>>9") .                               
                        WHEN "mth-bgt"     THEN cVarValue = string(v-sls-month-bud-dol-tot,"->>,>>>,>>9") .                             
                        WHEN "mth-var"     THEN cVarValue = string((v-sls-month-act-dol-tot - v-sls-month-bud-dol-tot),"->>,>>>,>>9") . 
                        WHEN "ytd-act"     THEN cVarValue = string(v-sls-ytd-act-dol-tot,"->>,>>>,>>9") .                                  
                        WHEN "ytd-bgt"     THEN cVarValue = string(v-sls-ytd-bud-dol-tot,"->>,>>>,>>9") .                               
                        WHEN "ytd-var"     THEN cVarValue = string((v-sls-ytd-act-dol-tot - v-sls-ytd-bud-dol-tot),"->>,>>>,>>9") .     
                        WHEN "cust"        THEN cVarValue = "" .
                        WHEN "name"        THEN cVarValue = "" .
                        WHEN "cat"         THEN cVarValue = "" .

                        WHEN "mth-act-m"   THEN cVarValue = string(v-sls-month-act-msf-tot,"->>,>>>,>>9") .                                 
                        WHEN "mth-bgt-m"   THEN cVarValue = string(v-sls-month-bud-msf-tot,"->>,>>>,>>9") .                              
                        WHEN "mth-var-m"   THEN cVarValue = string((v-sls-month-act-msf-tot - v-sls-month-bud-msf-tot),"->>,>>>,>>9") .
                        WHEN "ytd-act-m"   THEN cVarValue = string(v-sls-ytd-act-msf-tot,"->>,>>>,>>9") .                                   
                        WHEN "ytd-bgt-m"   THEN cVarValue = string(v-sls-ytd-bud-msf-tot,"->>,>>>,>>9") .                                
                        WHEN "ytd-var-m"   THEN cVarValue = string((v-sls-ytd-act-msf-tot - v-sls-ytd-bud-msf-tot),"->>,>>>,>>9") .    

                        WHEN "mth-act-t"   THEN cVarValue = string(v-sls-month-act-tons-tot,"->>,>>>,>>9") .                              
                        WHEN "mth-bgt-t"   THEN cVarValue = string(v-sls-month-bud-tons-tot,"->>,>>>,>>9") .                           
                        WHEN "mth-var-t"   THEN cVarValue = string((v-sls-month-act-tons-tot - v-sls-month-bud-tons-tot),"->>,>>>,>>9") . 
                        WHEN "ytd-act-t"   THEN cVarValue = string(v-sls-ytd-act-tons-tot,"->>,>>>,>>9") .                                
                        WHEN "ytd-bgt-t"   THEN cVarValue = string(v-sls-ytd-bud-tons-tot,"->>,>>>,>>9") .                             
                        WHEN "ytd-var-t"   THEN cVarValue = string((v-sls-ytd-act-tons-tot - v-sls-ytd-bud-tons-tot),"->>,>>>,>>9") .     

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED " TOTALS  " + (IF AVAIL sman THEN sman.sname ELSE "") FORMAT "X(30)" +  substring(cDisplay,31,350) SKIP.
            IF tb_excel THEN DO:
                PUT STREAM excel UNFORMATTED  
                    "TOTAL " + (IF AVAIL sman THEN sman.sname ELSE "") FORMAT "X(30)" + substring(cExcelDisplay,3,300) SKIP.
            END.

       ASSIGN
          v-gt-month-act-dol = v-gt-month-act-dol + v-sls-month-act-dol-tot
          v-gt-month-bud-dol = v-gt-month-bud-dol + v-sls-month-bud-dol-tot
          v-gt-ytd-act-dol = v-gt-ytd-act-dol + v-sls-ytd-act-dol-tot
          v-gt-ytd-bud-dol = v-gt-ytd-bud-dol + v-sls-ytd-bud-dol-tot.

     /*  IF tb_excel THEN
       DO:
          IF tg_disp_msf THEN */
             ASSIGN
                v-gt-month-act-msf = v-gt-month-act-msf + v-sls-month-act-msf-tot
                v-gt-month-bud-msf = v-gt-month-bud-msf + v-sls-month-bud-msf-tot
                v-gt-ytd-act-msf = v-gt-ytd-act-msf + v-sls-ytd-act-msf-tot
                v-gt-ytd-bud-msf = v-gt-ytd-bud-msf + v-sls-ytd-bud-msf-tot.

        /*  IF tg_disp_tons THEN */
             ASSIGN
                v-gt-month-act-tons = v-gt-month-act-tons + v-sls-month-act-tons-tot
                v-gt-month-bud-tons = v-gt-month-bud-tons + v-sls-month-bud-tons-tot
                v-gt-ytd-act-tons = v-gt-ytd-act-tons + v-sls-ytd-act-tons-tot
                v-gt-ytd-bud-tons = v-gt-ytd-bud-tons + v-sls-ytd-bud-tons-tot.

        /*  PUT STREAM excel UNFORMATTED
              SKIP(1).

          RUN print-excel-proc("TOTAL",(IF AVAIL sman THEN sman.sname ELSE ""),v-sls-month-act-dol-tot,v-sls-month-bud-dol-tot,
                               v-sls-ytd-act-dol-tot,v-sls-ytd-bud-dol-tot).

          IF tg_disp_msf THEN
             RUN print-excel2-proc(v-sls-month-act-msf-tot,v-sls-month-bud-msf-tot,v-sls-ytd-act-msf-tot,v-sls-ytd-bud-msf-tot).

          IF tg_disp_tons THEN
             RUN print-excel2-proc(v-sls-month-act-tons-tot,v-sls-month-bud-tons-tot,v-sls-ytd-act-tons-tot,v-sls-ytd-bud-tons-tot).

          PUT STREAM excel UNFORMATTED SKIP. 
       END.                                  */

       PAGE.
    END. /* last-of salesrep*/
END.

RUN print-grand-totals-proc.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END. 

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCustRange C-Win 
PROCEDURE SetCustRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
        begin_cust-no:SENSITIVE = NOT iplChecked
        end_cust-no:SENSITIVE = NOT iplChecked
        begin_cust-no:VISIBLE = NOT iplChecked
        end_cust-no:VISIBLE = NOT iplChecked
        btnCustList:SENSITIVE = iplChecked
       .
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE total-proc C-Win 
PROCEDURE total-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH tt-data  /* btr - need to filter by the entered slsmn */
       WHERE tt-data.sman-no GE begin_slsmn AND
             tt-data.sman-no LE END_slsmn:

       {custom/statusMsg.i " 'Processing Sales Rep#  '  + tt-data.sman "}

       FIND FIRST tt-total-data WHERE
            tt-total-data.sman-no EQ tt-data.sman-no AND
            (tt-total-data.prod-cat EQ tt-data.prod-cat AND rd_sortby EQ "PC"
             OR tt-total-data.cust-no EQ tt-data.cust-no AND rd_sortby EQ "CU")
            NO-ERROR.

       IF NOT AVAIL tt-total-data THEN
       DO:
          CREATE tt-total-data.
          ASSIGN
             tt-total-data.sman-no = tt-data.sman-no
             tt-total-data.prod-cat = tt-data.prod-cat
             tt-total-data.cust-no = tt-data.cust-no.
       END.

       ASSIGN tt-total-data.month-act-dollars = tt-total-data.month-act-dollars
                                              + tt-data.month-act-dollars
              tt-total-data.month-act-msf = tt-total-data.month-act-msf
                                          + tt-data.month-act-msf
              tt-total-data.month-act-tons = tt-total-data.month-act-tons
                                           + tt-data.month-act-tons
              tt-total-data.month-budget-dollars = tt-total-data.month-budget-dollars
                                                 + tt-data.month-budget-dollars
              tt-total-data.month-budget-msf = tt-total-data.month-budget-msf
                                             + tt-data.month-budget-msf
              tt-total-data.month-budget-tons = tt-total-data.month-budget-tons
                                              + tt-data.month-budget-tons
              tt-total-data.ytd-act-dollars = tt-total-data.ytd-act-dollars
                                            + tt-data.ytd-act-dollars
              tt-total-data.ytd-act-msf = tt-total-data.ytd-act-msf
                                        + tt-data.ytd-act-msf
              tt-total-data.ytd-act-tons = tt-total-data.ytd-act-tons
                                         + tt-data.ytd-act-tons
              tt-total-data.ytd-budget-dollars = tt-total-data.ytd-budget-dollars
                                               + tt-data.ytd-budget-dollars
              tt-total-data.ytd-budget-msf = tt-total-data.ytd-budget-msf
                                           + tt-data.ytd-budget-msf
              tt-total-data.ytd-budget-tons = tt-total-data.ytd-budget-tons
                                            + tt-data.ytd-budget-tons.
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

