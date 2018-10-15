&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-slmpef.w

  Description: Sales Rep Performance Report

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

def var called as log no-undo.

called = YES.

def var fdate as date extent 3 no-undo.
/* [day] [period] [year] */
def var edate as date extent 3 no-undo.
def var tdate as date format "99/99/9999" no-undo.
def var fsman as char format "x(3)" no-undo.
def var tsman as char format "x(3)" no-undo.
def var v-inc-fc as log init no no-undo.

def var v-sman-no   as   char format "x(3)" no-undo.
def var v-amt       like ar-inv.gross no-undo.
def var v-sqft      like itemfg.t-sqft  format "->>,>>9.999" no-undo.
def var v-pmsf      as   dec format "->>>>>9.99" extent 3 no-undo.

def var v-tot-sqft  like v-sqft extent 6.
def var v-tot-amt   like v-amt  extent 6.

def var v-period as int no-undo.
def var v-year as int format "9999" no-undo.
def var v-qty like ar-invl.ship-qty format "->>>,>>9.99" no-undo.
def var v-exclude as log no-undo.
def var v-pct as dec format "99.99" no-undo.
def var v-misc as log no-undo.
def var v-leave as log no-undo.
def var v-fac as int.
def var head as char no-undo.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD row-id AS ROWID.

DEF BUFFER b-tt-report FOR tt-report.
DEF BUFFER b-ar-invl FOR ar-invl.

def TEMP-TABLE w-data no-undo
  field w-type      as   char
  field w-sman-no   like v-sman-no
  field w-sqft      like v-sqft     extent 3    column-label "Sq Ft/M"
  field w-amt       like v-amt      extent 3    column-label "Amount"
  field w-pmsf      like v-pmsf                 column-label "$/MSF"
  field w-cust-no   like ar-inv.cust-no column-label "Customer"  .

def TEMP-TABLE w-data1 NO-UNDO like w-data.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

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

DEF STREAM excel.

head = if called then "SALES ANALYSIS - SALESREP PERFORMANCE tt-report"
                 else "SALES ANALYSIS - SALESREP PRODUCTION tt-report".


form w-data1.w-sman-no   column-label "No"
     sman.sname         column-label "Name" format "x(17)"
     w-data1.w-sqft[1]
     w-data1.w-amt[1]  FORMAT "->>,>>>,>>9.99" 
     w-data1.w-pmsf[1]
     w-data1.w-sqft[2]
     w-data1.w-amt[2]  FORMAT "->>,>>>,>>9.99" 
     w-data1.w-pmsf[2]
     w-data1.w-sqft[3]      
     w-data1.w-amt[3]  FORMAT "->>,>>>,>>9.99" 
     w-data1.w-pmsf[3]
     header skip(1)
     "    SalesRep"
     "---------------Daily----------------" at 23
     "-----------Period to Date-----------"
     "------------Year to Date-------------"

    with no-box frame f-prod down STREAM-IO width 144.

form w-data1.w-sman-no column-label "No"
     sman.sname column-label "Name" format "x(20)"
     w-data1.w-sqft[1]
     w-data1.w-amt[1] FORMAT "->>,>>>,>>9.99" 
     space(2)
     w-data1.w-sqft[2]
     w-data1.w-amt[2] FORMAT "->>,>>>,>>9.99" 
     space(2)
     w-data1.w-sqft[3]
     w-data1.w-amt[3] FORMAT "->>,>>>,>>9.99" 
     header skip(1)
     "    SalesRep"
     "----------Daily-----------" at 26 space(2)
     "------Period to Date------" space(2)
     "-------Year to Date-------"

    with no-box frame f-perf down STREAM-IO width 144.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "No,Sales Rep Name,Dly Sq Ft/M,Dly Amount,"
                         + "PTD Sq Ft/M,PTD Amount,YTD Sq Ft/M,YTD Amount,Cust#"
       cFieldListToSelect = "rep,name,dly-sf,dly-amt," +
                            "ptd-sf,ptd-amt,ytd-sf,ytd-amt,cust"
       cFieldLength = "3,25,11,13," + "11,13,11,14,8" 
       cFieldType = "c,c,i,i," + "i,i,i,i,c"
    .



{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "No,Sales Rep Name,Dly Sq Ft/M,Dly Amount,"
                         + "PTD Sq Ft/M,PTD Amount,YTD Sq Ft/M,YTD Amount"     .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 inv-date tb_cust-list ~
btnCustList begin_cust-no end_cust-no begin_slsmn end_slsmn tb_fin-chg ~
sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up btn_down rd-dest ~
lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS inv-date tb_cust-list begin_cust-no ~
end_cust-no begin_slsmn end_slsmn tb_fin-chg sl_avail sl_selected rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel ~
tb_runExcel fi_file 

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
     SIZE 17 BY 1.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-slmpef.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE inv-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

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
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 9.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.62.

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

DEFINE VARIABLE tb_fin-chg AS LOGICAL INITIAL no 
     LABEL "Include Finance Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

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
     inv-date AT ROW 1.91 COL 32 COLON-ALIGNED HELP
          "Enter Invoice Date"
     tb_cust-list AT ROW 3.14 COL 31.8 WIDGET-ID 6
     btnCustList AT ROW 3.19 COL 63.8 WIDGET-ID 8
     begin_cust-no AT ROW 4.1 COL 29.2 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 58
     end_cust-no AT ROW 4.1 COL 70.2 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 60
     begin_slsmn AT ROW 5.05 COL 29.2 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 5.05 COL 70.2 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     tb_fin-chg AT ROW 7.19 COL 35
     sl_avail AT ROW 9.43 COL 5 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 9.43 COL 60 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 9.52 COL 41.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 10.67 COL 41.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 11.86 COL 41.4 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 13.05 COL 41.4 WIDGET-ID 40
     btn_down AT ROW 14.24 COL 41.4 WIDGET-ID 42
     rd-dest AT ROW 16.81 COL 6 NO-LABEL
     lv-ornt AT ROW 17.05 COL 30 NO-LABEL
     lines-per-page AT ROW 17.05 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 18.95 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 19.91 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 21.19 COL 29
     tb_excel AT ROW 22.81 COL 49 RIGHT-ALIGNED
     tb_runExcel AT ROW 22.81 COL 70 RIGHT-ALIGNED
     fi_file AT ROW 23.62 COL 27 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 25.71 COL 19
     btn-cancel AT ROW 25.71 COL 57
     "Available Columns" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 8.67 COL 5.8 WIDGET-ID 38
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 8.71 COL 60.2 WIDGET-ID 44
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.86 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 15.62 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 26.33.


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
         TITLE              = "Sales Analysis - Sales Rep Performance Report"
         HEIGHT             = 26.57
         WIDTH              = 95.8
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
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_fin-chg:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Sales Analysis - Sales Rep Performance Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sales Analysis - Sales Rep Performance Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Salesrep# */
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
    ASSIGN {&displayed-objects}.
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

  SESSION:SET-WAIT-STATE ("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type="Salesman"
                            &begin_cust=begin_slsmn
                            &END_cust= begin_slsmn
                            &fax-subject="cwin:title"
                            &fax-body="cwin:title"
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Salesman"
                             &begin_cust= begin_slsmn
                             &END_cust=begin_slsmn
                             &mail-subject="cwin:title"
                             &mail-body="cwin:title"
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Salesman"
                                  &begin_cust= begin_slsmn
                                  &END_cust=begin_slsmn
                                  &mail-subject="cwin:title"
                                  &mail-body="cwin:title"
                                  &mail-file=list-name }
     END.
       END.
       WHEN 6 THEN RUN OUTPUT-to-port.

  end case. 
  SESSION:SET-WAIT-STATE("").
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


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Salesrep# */
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


&Scoped-define SELF-NAME inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-date C-Win
ON LEAVE OF inv-date IN FRAME FRAME-A /* Invoice Date */
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


&Scoped-define SELF-NAME tb_fin-chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fin-chg C-Win
ON VALUE-CHANGED OF tb_fin-chg IN FRAME FRAME-A /* Include Finance Charges? */
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
     RETURN .
  END.

  assign
   inv-date = TODAY.

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "HT",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO inv-date IN FRAME {&FRAME-NAME}.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'HT',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""HT""}

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

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
                            INPUT 'HT',
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
/*------------------------------------------------------------------------------
  Purpose:  Display a UI of selected customers   
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
                                  INPUT 'HT').


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
  DISPLAY inv-date tb_cust-list begin_cust-no end_cust-no begin_slsmn end_slsmn 
          tb_fin-chg sl_avail sl_selected rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 inv-date tb_cust-list btnCustList begin_cust-no 
         end_cust-no begin_slsmn end_slsmn tb_fin-chg sl_avail sl_selected 
         Btn_Def Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/test/asi/sa/sa
**       by: Christopher A. Heins
** Descript: Invoicing summary by rep
**
*****************************************************************************
\***************************************************************************/

/*{sys/form/r-topw.f}*/

DEF VAR lv-r-no LIKE oe-retl.r-no NO-UNDO.
DEF VAR lv-type AS CHAR NO-UNDO.
DEF VAR ld-inv-pct  AS   DEC NO-UNDO.

DEF VAR excelheader AS CHAR NO-UNDO.

{sys/form/r-top5DL3.f}
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.


ASSIGN str-tit4 = "" .
       str-tit5 = "" .
       str-line = "" .


ASSIGN
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 tdate        = inv-date
 fsman        = begin_slsmn
 tsman        = end_slsmn
 v-inc-fc     = tb_fin-chg.


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

        IF LOOKUP(ttRptSelected.TextList, "Dly Sq Ft/M,Dly Amount,PTD Sq Ft/M,PTD Amount,YTD Sq Ft/M,YTD Amount") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
 /* excelheader = "No,Sales Rep Name,Daily Sq Ft/M,Daily Amount,"
              + "PTD Sq Ft/M,PTD Amount,YTD Sq Ft/M,YTD Amount". */
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

display "" with frame r-top.

SESSION:SET-WAIT-STATE ("general").

   if tdate eq ? then tdate = today.

    find first period
        where period.company eq cocode
          and period.pst     le tdate
          and period.pend    ge tdate
        no-lock.

    assign
     v-period = period.pnum
     v-year   = period.yr
     fdate[2] = period.pst
     edate[2] = tdate
     fdate[1] = tdate
     edate[1] = tdate.

    find first period
        where period.company eq cocode
          and period.yr      eq v-year
        no-lock.

    assign
     fdate[3] = period.pst
     edate[3] = tdate.

    EMPTY TEMP-TABLE tt-report.
    ASSIGN
      v-tot-amt = 0
      v-tot-sqft = 0.

  FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
    each cust
        where cust.company eq cocode
          and cust.cust-no EQ ttCustList.cust-no 
        no-lock:
      for each ar-inv
          where ar-inv.company  eq cocode
            and ar-inv.posted   eq yes
            and ar-inv.cust-no  eq cust.cust-no
            and ar-inv.inv-date ge fdate[3]
            and ar-inv.inv-date le edate[3]
            and (ar-inv.type    ne "FC" or v-inc-fc)
          no-lock,

          each ar-invl
          where ar-invl.x-no eq ar-inv.x-no
            and (ar-invl.billable or not ar-invl.misc)
          no-lock
          transaction:
          

        {sa/sa-sman4.i "ar-invl"}
      end.

      for each ar-cash
          where ar-cash.company    eq cocode
            and ar-cash.cust-no    eq cust.cust-no
            and ar-cash.check-date ge fdate[3]
            and ar-cash.check-date le edate[3]
            and ar-cash.posted     eq yes
          no-lock,

          EACH ar-cashl
          WHERE ar-cashl.c-no    EQ ar-cash.c-no
            AND ar-cashl.posted  EQ YES
            AND ar-cashl.memo    EQ YES
            AND (/*ar-cashl.inv-no NE 0 OR*/
                 CAN-FIND(FIRST account
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

        IF ar-cashl.inv-no NE 0                                                       AND
           (AVAIL ar-invl                             OR
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
          {sa/sa-sman4.i "ar-cashl" "b-"}
        end.

        else
        if cust.sman ge fsman and
           cust.sman le tsman then do:
          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = IF AVAIL reftable                      OR
                               ar-cashl.dscr MATCHES "*oe return*" THEN "2" ELSE "4"
           tt-report.key-02  = cust.sman
           tt-report.key-09  = cust.cust-no
           tt-report.key-10  = "ar-cashl"
           tt-report.rec-id  = recid(ar-cashl).
        
        end.
      end.
    end.

    for each tt-report
        where tt-report.term-id eq ""

        break by tt-report.key-02

        transaction:

      find first w-data
          where w-data.w-type    eq tt-report.key-01
            and w-data.w-sman-no eq tt-report.key-02
          no-lock no-error.

      if not avail w-data then do:
        create w-data.
        assign
         w-data.w-type    = tt-report.key-01
         w-data.w-sman-no = tt-report.key-02
         w-data.w-cust-no  = tt-report.key-09.
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

        do i = 1 to 3:
          if ar-inv.inv-date ge fdate[i] and
             ar-inv.inv-date le edate[i] then
            assign
             w-data.w-sqft[i] = w-data.w-sqft[i] + (v-sqft * v-pct)
             w-data.w-amt[i]  = w-data.w-amt[i]  + (v-amt  * v-pct).
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
          END.

          ELSE
          IF AVAIL ar-invl THEN DO:
            ld-inv-pct = 0.
            FOR EACH b-ar-invl WHERE b-ar-invl.x-no EQ ar-invl.x-no NO-LOCK:
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

          do i = 1 to 3:
            if ar-cash.check-date ge fdate[i] and
               ar-cash.check-date le edate[i] then
              assign
               w-data.w-sqft[i] = w-data.w-sqft[i] - (v-sqft * v-pct)
               w-data.w-amt[i]  = w-data.w-amt[i]  + (v-amt  * v-pct).
          end.
        end.
      end.

      if last-of(tt-report.key-02) then do:
        create w-data1.
        ASSIGN
        w-data1.w-sman-no = tt-report.key-02
        w-data1.w-cust-no  = tt-report.key-09.

        for each w-data where w-data.w-sman-no eq w-data1.w-sman-no:
          do i = 1 to 3:
            assign
             w-data1.w-sqft[i] = w-data1.w-sqft[i] + w-data.w-sqft[i]
             w-data1.w-amt[i]  = w-data1.w-amt[i]  + w-data.w-amt[i]

             v-tot-amt[i]  = v-tot-amt[i]  + w-data.w-amt[i]
             v-tot-sqft[i] = v-tot-sqft[i] + w-data.w-sqft[i].
          end.
        end.

        do i = 1 to 3:
          w-data1.w-pmsf[i] = if w-data1.w-sqft[i] eq 0 then 0
                             else (w-data1.w-amt[i] / w-data1.w-sqft[i]).
        end.

        put skip(1).
        find first sman
            where sman.company eq cocode
              and sman.sman    eq w-data1.w-sman-no
            no-lock no-error.
        find first cust
            where cust.company eq cocode
              and cust.sman    eq w-data1.w-cust-no
            no-lock no-error.
        if not called then do:
          /*display w-data1.w-sman-no
                  sman.sname when avail sman
                  w-data1.w-sqft[1 for 3]
                  w-data1.w-amt[1 for 3] 
                  w-data1.w-pmsf[1 for 3]
              with frame f-prod down.
          down with frame f-prod. */
        end.
        else do:
         /* display w-data1.w-sman-no
                  sman.sname when avail sman
                  w-data1.w-sqft[1 for 3]
                  w-data1.w-amt[1 for 3] 
              with frame f-perf down.
          down with frame f-perf. */
        end.

       /* IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               '"' w-data1.w-sman-no    '",'
               '"' IF AVAIL sman THEN sman.sname ELSE "" '",'
               '"' STRING(w-data1.w-sqft[1],"->>,>>9.999")    '",'
               '"' STRING(w-data1.w-amt[1],"->,>>>,>>9.99")    '",'
               '"' STRING(w-data1.w-sqft[2],"->>,>>9.999")    '",'
               '"' STRING(w-data1.w-amt[2],"->,>>>,>>9.99")    '",'
               '"' STRING(w-data1.w-sqft[3],"->>,>>9.999")    '",'
               '"' STRING(w-data1.w-amt[3],"->>,>>>,>>9.99")    '",'
               SKIP. */

        ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".
                  DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:
                    WHEN "rep"      THEN cVarValue = w-data1.w-sman-no .
                    WHEN "name"     THEN cVarValue = IF AVAIL sman THEN STRING(sman.sname,"X(25)") ELSE "". 
                    WHEN "dly-sf"   THEN cVarValue = STRING(w-data1.w-sqft[1],"->>,>>9.999")  .  
                    WHEN "dly-amt"  THEN cVarValue = STRING(w-data1.w-amt[1],"->,>>>,>>9.99") . 
                    WHEN "ptd-sf"   THEN cVarValue = STRING(w-data1.w-sqft[2],"->>,>>9.999") .  
                    WHEN "ptd-amt"  THEN cVarValue = STRING(w-data1.w-amt[2],"->,>>>,>>9.99") . 
                    WHEN "ytd-sf"   THEN cVarValue = STRING(w-data1.w-sqft[3],"->>,>>9.999") .  
                    WHEN "ytd-amt"  THEN cVarValue = STRING(w-data1.w-amt[3],"->>,>>>,>>9.99") .
                    WHEN "cust"     THEN cVarValue = string(w-data1.w-cust-no,"x(8)") .

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

        delete w-data1.
      end.

      delete tt-report.
    end.

    RUN run-report-2.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-2 C-Win 
PROCEDURE run-report-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  for each w-data break by w-data.w-type:
      if w-data.w-type ne "1" then
      do i = 1 to 3:
        assign
         v-tot-amt[i]  = v-tot-amt[i]  + w-data.w-amt[i]
         v-tot-sqft[i] = v-tot-sqft[i] + w-data.w-sqft[i].
      end.

      if last-of(w-data.w-type) then do:
        if w-data.w-type eq "1" then
         /* if not called then do with frame f-prod:
            underline w-data1.w-sqft[1 for 3]
                      w-data1.w-amt[1 for 3]
                      w-data1.w-pmsf[1 for 3].
            down 1.
          end.

          else
          do with frame f-perf:
            underline w-data1.w-sqft[1 for 3]
                      w-data1.w-amt[1 for 3].
            down 1.
          end. */

        do i = 1 to 3:
          v-pmsf[i] = v-tot-amt[i] / v-tot-sqft[i].
          if v-pmsf[i] eq ? then v-pmsf[i] = 0.
        end.

       /* if not called then do: /*with frame f-prod: */
          if w-data.w-type eq "1" then
            display "  TOTAL SALES" @ sman.sname.
          else
          if w-data.w-type eq "2" then
            display "  MISC" @ sman.sname.
          else
          if w-data.w-type eq "3" then
            display "  PREP" @ sman.sname.
          else
            display "  MEMO" @ sman.sname.

          display v-tot-amt[1]  @ w-data1.w-amt[1]
                  v-tot-sqft[1] @ w-data1.w-sqft[1]
                  v-pmsf[1]     @ w-data1.w-pmsf[1]
                  v-tot-amt[2]  @ w-data1.w-amt[2]
                  v-tot-sqft[2] @ w-data1.w-sqft[2]
                  v-pmsf[2]     @ w-data1.w-pmsf[2]
                  v-tot-amt[3]  @ w-data1.w-amt[3]
                  v-tot-sqft[3] @ w-data1.w-sqft[3]
                  v-pmsf[3]     @ w-data1.w-pmsf[3].
          down 1. */

         /* IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 SKIP(1)
                 '"' ""     '",'
                 '"' if w-data.w-type eq "1" THEN "  TOTAL SALES"
                     ELSE IF w-data.w-type EQ "2" THEN "  MISC"
                     ELSE IF w-data.w-type EQ "3" THEN "  PREP"
                     ELSE "  MEMO"                            '",'
                 '"' STRING(v-tot-sqft[1],"->>,>>9.999")    '",'
                 '"' STRING(v-tot-amt[1],"->,>>>,>>9.99")    '",'
                 '"' STRING(v-tot-sqft[2],"->>,>>9.999")    '",'
                 '"' STRING(v-tot-amt[2],"->,>>>,>>9.99")    '",'
                 '"' STRING(v-tot-sqft[3],"->>,>>9.999")    '",'
                 '"' STRING(v-tot-amt[3],"->,>>>,>>9.99")    '",'
                 SKIP.

          if w-data.w-type eq "1" then do:
            underline w-data1.w-sqft[1 for 3]
                      w-data1.w-amt[1 for 3]
                      w-data1.w-pmsf[1 for 3].
            down 1.
          end. */
        PUT SKIP str-line SKIP .
            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:
                        WHEN "rep"      THEN cVarValue = "" .
                        WHEN "name"     THEN cVarValue = "". 
                        WHEN "dly-sf"   THEN cVarValue = STRING(v-tot-sqft[1],"->>,>>9.999")  .  
                        WHEN "dly-amt"  THEN cVarValue = STRING(v-tot-amt[1],"->,>>>,>>9.99") . 
                        WHEN "ptd-sf"   THEN cVarValue = STRING(v-tot-sqft[2],"->>,>>9.999") .   
                        WHEN "ptd-amt"  THEN cVarValue = STRING(v-tot-amt[2],"->,>>>,>>9.99") . 
                        WHEN "ytd-sf"   THEN cVarValue = STRING(v-tot-sqft[3],"->>,>>9.999")  .  
                        WHEN "ytd-amt"  THEN cVarValue = STRING(v-tot-amt[3],"->>,>>>,>>9.99")  .
                        WHEN "cust"    THEN cVarValue = "" .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            if w-data.w-type eq "1" THEN do:
                PUT UNFORMATTED "  TOTAL SALES " substring(cDisplay,15,350) SKIP.
                IF tb_excel THEN DO:
                    PUT STREAM excel UNFORMATTED  
                        "  TOTAL SALES " + substring(cExcelDisplay,3,300) SKIP.
                END.
            END.
            ELSE if w-data.w-type eq "2" THEN do:
                PUT UNFORMATTED "  MISC " substring(cDisplay,8,350) SKIP.
                IF tb_excel THEN DO:
                    PUT STREAM excel UNFORMATTED  
                        "  MISC " + substring(cExcelDisplay,3,300) SKIP.
                END.
            END.
            ELSE if w-data.w-type eq "3" THEN do:
                PUT UNFORMATTED "  PREP " substring(cDisplay,8,350) SKIP.
                IF tb_excel THEN DO:
                    PUT STREAM excel UNFORMATTED  
                        "  PREP " + substring(cExcelDisplay,3,300) SKIP.
                END.
            END.
            ELSE do:
                PUT UNFORMATTED "  MEMO " substring(cDisplay,8,350) SKIP.
                IF tb_excel THEN DO:
                    PUT STREAM excel UNFORMATTED  
                        "  MEMO " + substring(cExcelDisplay,3,300) SKIP.
                END.
            END.

       /* end.

        else
        do: /*with frame f-perf:*/
          if w-data.w-type eq "1" then
            display "  TOTAL SALES" @ sman.sname.
          else
          if w-data.w-type eq "2" then
            display "  MISC" @ sman.sname.
          else
          if w-data.w-type eq "3" then
            display "  PREP" @ sman.sname.
          else
            display "  MEMO" @ sman.sname.

          display v-tot-amt[1]  @ w-data1.w-amt[1]
                  v-tot-sqft[1] @ w-data1.w-sqft[1]
                  v-tot-amt[2]  @ w-data1.w-amt[2]
                  v-tot-sqft[2] @ w-data1.w-sqft[2]
                  v-tot-amt[3]  @ w-data1.w-amt[3]
                  v-tot-sqft[3] @ w-data1.w-sqft[3].
          down 1.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 SKIP(1)
                 '"' ""     '",'
                 '"' if w-data.w-type eq "1" THEN "  TOTAL SALES"
                     ELSE IF w-data.w-type EQ "2" THEN "  MISC"
                     ELSE IF w-data.w-type EQ "3" THEN "  PREP"
                     ELSE "  MEMO"                            '",'
                 '"' STRING(v-tot-sqft[1],"->>,>>9.999")    '",'
                 '"' STRING(v-tot-amt[1],"->,>>>,>>9.99")    '",'
                 '"' STRING(v-tot-sqft[2],"->>,>>9.999")    '",'
                 '"' STRING(v-tot-amt[2],"->,>>>,>>9.99")    '",'
                 '"' STRING(v-tot-sqft[3],"->>,>>9.999")    '",'
                 '"' STRING(v-tot-amt[3],"->,>>>,>>9.99")    '",'
                 SKIP.

          if w-data.w-type eq "1" then do:
            underline w-data1.w-sqft[1 for 3]
                      w-data1.w-amt[1 for 3].
            down 1.
          end.
        end.  */

        do i = 4 to 6:
          assign
           v-tot-amt[i]  = v-tot-amt[i]  + (v-tot-amt[i - 3] *
                                         if w-data.w-type eq "1" then 1 else -1)
           v-tot-sqft[i] = v-tot-sqft[i] + (v-tot-sqft[i - 3] *
                                         if w-data.w-type eq "1" then 1 else -1)

           v-tot-amt[i - 3]  = 0
           v-tot-sqft[i - 3] = 0.
        end.
      end.

      if last(w-data.w-type) then do:
        do i = 4 to 6:
          v-pmsf[i - 3] = v-tot-amt[i] / v-tot-sqft[i].
          if v-pmsf[i - 3] eq ? then v-pmsf[i - 3] = 0.
        end.

       /* if not called then do with frame f-prod:
          display "  SALES"       @ sman.sname
                  v-tot-amt[4]    @ w-data1.w-amt[1]
                  v-tot-sqft[4]   @ w-data1.w-sqft[1]
                  v-pmsf[1]       @ w-data1.w-pmsf[1]
                  v-tot-amt[5]    @ w-data1.w-amt[2]
                  v-tot-sqft[5]   @ w-data1.w-sqft[2]
                  v-pmsf[2]       @ w-data1.w-pmsf[2]
                  v-tot-amt[6]    @ w-data1.w-amt[3]
                  v-tot-sqft[6]   @ w-data1.w-sqft[3]
                  v-pmsf[3]       @ w-data1.w-pmsf[3].
          down 1.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 SKIP(1)
                 '"' ""     '",'
                 '"' if w-data.w-type eq "1" THEN "  TOTAL SALES"
                     ELSE IF w-data.w-type EQ "2" THEN "  MISC"
                     ELSE IF w-data.w-type EQ "3" THEN "  PREP"
                     ELSE "  MEMO"                            '",'
                 '"' STRING(v-tot-sqft[4],"->>,>>9.999")    '",'
                 '"' STRING(v-tot-amt[4],"->,>>>,>>9.99")    '",'
                 '"' STRING(v-tot-sqft[5],"->>,>>9.999")    '",'
                 '"' STRING(v-tot-amt[5],"->,>>>,>>9.99")    '",'
                 '"' STRING(v-tot-sqft[6],"->>,>>9.999")    '",'
                 '"' STRING(v-tot-amt[6],"->,>>>,>>9.99")    '",'
                 SKIP.
        end. 

        else
        do with frame f-perf:
          display "  SALES"       @ sman.sname
                  v-tot-amt[4]    @ w-data1.w-amt[1]
                  v-tot-sqft[4]   @ w-data1.w-sqft[1]
                  v-tot-amt[5]    @ w-data1.w-amt[2]
                  v-tot-sqft[5]   @ w-data1.w-sqft[2]
                  v-tot-amt[6]    @ w-data1.w-amt[3]
                  v-tot-sqft[6]   @ w-data1.w-sqft[3].
          down 1.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 SKIP(1)
                 '"' ""     '",'
                 '"' "  SALES"                           '",'
                 '"' STRING(v-tot-sqft[4],"->>,>>9.999")    '",'
                 '"' STRING(v-tot-amt[4],"->,>>>,>>9.99")    '",'
                 '"' STRING(v-tot-sqft[5],"->>,>>9.999")    '",'
                 '"' STRING(v-tot-amt[5],"->,>>>,>>9.99")    '",'
                 '"' STRING(v-tot-sqft[6],"->>,>>9.999")    '",'
                 '"' STRING(v-tot-amt[6],"->,>>>,>>9.99")    '",'
                 SKIP.
        end. */
        PUT SKIP str-line SKIP .
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:
                        WHEN "rep"      THEN cVarValue = "" .
                        WHEN "name"     THEN cVarValue = "". 
                        WHEN "dly-sf"   THEN cVarValue = STRING(v-tot-sqft[4],"->>,>>9.999")  .  
                        WHEN "dly-amt"  THEN cVarValue = STRING(v-tot-amt[4],"->,>>>,>>9.99") . 
                        WHEN "ptd-sf"   THEN cVarValue = STRING(v-tot-sqft[5],"->>,>>9.999") .   
                        WHEN "ptd-amt"  THEN cVarValue = STRING(v-tot-amt[5],"->,>>>,>>9.99") . 
                        WHEN "ytd-sf"   THEN cVarValue = STRING(v-tot-sqft[6],"->>,>>9.999")  .  
                        WHEN "ytd-amt"  THEN cVarValue = STRING(v-tot-amt[6],"->>,>>>,>>9.99")  .
                        WHEN "cust"    THEN cVarValue = "".

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED "  SALES " substring(cDisplay,9,350) SKIP.
            IF tb_excel THEN DO:
                PUT STREAM excel UNFORMATTED  
                    "  SALES " + substring(cExcelDisplay,3,300) SKIP.
            END.

      end.

      delete w-data.
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
  def var lv-label as cha NO-UNDO.

  ASSIGN
  lv-frame-hdl = frame {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:first-child
  lv-field-hdl = lv-group-hdl:first-child.

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," .
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name THEN
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".

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

