&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fgrep\r-fgpstr.w

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
{methods/prgsecdt.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

{sys/inc/custlistform.i ""IL6"" }

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

def new shared var v-types as char format "x(10)".

def new shared var b-post-date as date init today no-undo.
def new shared var e-post-date as date init today no-undo.
def new shared var v-pr-tots      as log   format "Y/N"  init false no-undo.
def var v-post-date as date no-undo init today.
def shared var file_stamp as ch format "x(12)".    
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD rec-id2 AS RECID
    FIELD DATE AS DATE
    INDEX rec-id2 rec-id2.

DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

                                /* 10 + 6 + 8 = 24 columns */
ASSIGN cTextListToSelect = "DATE,ITEM,DESCRIPTN,PO#,JOB#,VENDOR#,TRANSACTION TYPE,TAG#,RFID#,UNITS," +
                           "COUNT,BIN,CUOM,TOTAL QTY,TOTAL COST,TOT SELL VAL," +
                           "CUSTOMER PART#,DIE#,# UP,CAD#,PLATE#,NUM OF COLORS,SHEET SIZE,CALIPER,USER-ID,WHSE,WT/100,REC TIME,POSTED," +
                           "CATGY,UNIT COST,UNIT SELL,SUOM,PROMISE DATE,ORD DUE DATE,START DATE,SHIPTO,SHIPTO NAME"
       cFieldListToSelect = "fg-rcpth.trans-date,fg-rcpth.i-no,fg-rcpth.i-name,fg-rcpth.po-no,fg-rcpth.job-no," +
                            "po-ord.vend-no,v-tran-type,v-tag,v-rfid#,v-cases,v-qty-case,fg-rdtlh.loc-bin,lv-cost-uom,v-fg-qty,v-fg-cost,v-fg-value," +
                            "itemfg.part-no,itemfg.die-no,v-numUp,itemfg.cad-no,itemfg.plate-no,v-numColors,v-SheetSize,v-Caliper,fg-rcpth.user-id,fg-rdtld.loc,wt-h,rec-time,fg-rcpth.post-date," +
                            "itemfg.procat,unt-cst,unt-sel,suom,prom-date,due-date,job-start,shipto,shipname"
       cFieldLength = "9,16,11,9,13,11,1,20,24,8," + "8,9,9,10,10,13," + "15,15,4,15,15,13,15,7,10,10,9,8,8," +
                      "5,11,14,4,12,12,10,8,30"
       .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "DATE,ITEM,DESCRIPTN,PO#,VENDOR#,TRANSACTION TYPE,TAG#,UNITS," +
                           "COUNT,BIN,CUOM,TOTAL QTY,TOTAL COST,TOTAL SELL VALUE,RFID#" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 from_date to_date tb_cust-list ~
btnCustList begin_cust end_cust begin_i-no end_i-no begin_user end_user ~
tb_rec tb_ship tb_tran tb_adj tb_ret tb_count tb_total Btn_Def sl_avail ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lines-per-page ~
lv-ornt lv-font-no td-show-parm tb_runExcel tb_excel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS from_date to_date tb_cust-list begin_cust ~
end_cust begin_i-no end_i-no begin_user end_user tb_rec tb_ship tb_tran ~
tb_adj tb_ret tb_count tb_total sl_avail sl_selected rd-dest lines-per-page ~
lv-ornt lv-font-no lv-font-name td-show-parm tb_runExcel tb_excel fi_file 

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


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_user AS CHARACTER FORMAT "x(8)" 
     LABEL "Beginning Order User ID" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_user AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Order User ID" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-fgpstr.csv" 
     LABEL "Excel File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE from_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Beginning Post Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "10" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE to_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Ending Post Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

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
     SIZE 21 BY 6.19 NO-UNDO.

DEFINE VARIABLE rsShowTag AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "  Tag No.", "Tag#",
"RFID#", "RFID#"
     SIZE 31 BY .95 NO-UNDO.

DEFINE VARIABLE rsShowVendor AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Vendor", "Vendor",
"Job#", "Job#"
     SIZE 31 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.14.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 11.91.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5 NO-UNDO.

DEFINE VARIABLE tb_adj AS LOGICAL INITIAL no 
     LABEL "Adjustments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_count AS LOGICAL INITIAL no 
     LABEL "Cycle Counts" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_rec AS LOGICAL INITIAL no 
     LABEL "Receipts" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_ret AS LOGICAL INITIAL no 
     LABEL "Credit Returns" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_ship AS LOGICAL INITIAL no 
     LABEL "Shipments" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_total AS LOGICAL INITIAL no 
     LABEL "Show Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tb_tran AS LOGICAL INITIAL no 
     LABEL "Transfers" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .71
     FONT 6 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     from_date AT ROW 1.71 COL 29 COLON-ALIGNED
     to_date AT ROW 1.71 COL 72 COLON-ALIGNED
     tb_cust-list AT ROW 2.86 COL 32 WIDGET-ID 6
     btnCustList AT ROW 2.91 COL 64 WIDGET-ID 8
     begin_cust AT ROW 4 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 2
     end_cust AT ROW 4 COL 72 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 4
     begin_i-no AT ROW 5 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Order Number" WIDGET-ID 6
     end_i-no AT ROW 5 COL 72 COLON-ALIGNED HELP
          "Enter Ending Item Number" WIDGET-ID 8
     begin_user AT ROW 6.1 COL 29 COLON-ALIGNED HELP
          "Enter Beginning User ID" WIDGET-ID 46
     end_user AT ROW 6.1 COL 72 COLON-ALIGNED HELP
          "Enter Ending User ID" WIDGET-ID 48
     tb_rec AT ROW 7.48 COL 38
     tb_ship AT ROW 8.19 COL 38
     rsShowVendor AT ROW 8.81 COL 64 NO-LABEL WIDGET-ID 14
     tb_tran AT ROW 8.91 COL 38
     tb_adj AT ROW 9.62 COL 38
     rsShowTag AT ROW 10 COL 64 NO-LABEL WIDGET-ID 18
     tb_ret AT ROW 10.33 COL 38
     tb_count AT ROW 11.05 COL 38
     tb_total AT ROW 11.76 COL 38
     Btn_Def AT ROW 13.76 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_avail AT ROW 13.81 COL 6 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 13.81 COL 60 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 14.76 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 15.76 COL 40 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 16.81 COL 40 WIDGET-ID 40
     btn_down AT ROW 17.86 COL 40 WIDGET-ID 42
     rd-dest AT ROW 19.81 COL 5 NO-LABEL
     lines-per-page AT ROW 19.91 COL 84 COLON-ALIGNED
     lv-ornt AT ROW 19.95 COL 31 NO-LABEL
     lv-font-no AT ROW 21.24 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 22.19 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 23.38 COL 31
     tb_runExcel AT ROW 24 COL 93 RIGHT-ALIGNED
     tb_excel AT ROW 24.05 COL 71 RIGHT-ALIGNED
     fi_file AT ROW 24.76 COL 49 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 26.43 COL 24
     btn-cancel AT ROW 26.43 COL 59
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 13.1 COL 2 WIDGET-ID 38
     "Transaction Types" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 7.38 COL 14
          FONT 6
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 13.1 COL 59.4 WIDGET-ID 44
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 19.14 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1 COL 3
          BGCOLOR 2 
     RECT-6 AT ROW 19.05 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 94.4 BY 26.81.


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
         TITLE              = "Finished Goods Posting History Report"
         HEIGHT             = 27.05
         WIDTH              = 95.6
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_user:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_user:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       from_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rsShowTag IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rsShowTag:HIDDEN IN FRAME FRAME-A           = TRUE
       rsShowTag:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rsShowTag,parm".

/* SETTINGS FOR RADIO-SET rsShowVendor IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rsShowVendor:HIDDEN IN FRAME FRAME-A           = TRUE
       rsShowVendor:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rsShowVendor,parm".

ASSIGN 
       tb_adj:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_count:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_rec:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_ret:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_ship:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_total:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_tran:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       to_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Finished Goods Posting History Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finished Goods Posting History Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON HELP OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_user C-Win
ON LEAVE OF begin_user IN FRAME FRAME-A /* Beginning Order User ID */
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
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  RUN GetSelectionList.

  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT tb_cust-list OR  NOT AVAIL ttCustList THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust,
                    INPUT end_cust).
  END.
  run run-report. 
  STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE("general").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=from_date
                            &END_cust=to_date
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
                                  &mail-file=list-name }

           END.
 
       END. 
       WHEN 6 THEN run output-to-port.
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


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON HELP OF end_cust IN FRAME FRAME-A /* Ending Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val) .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_user C-Win
ON LEAVE OF end_user IN FRAME FRAME-A /* Ending Order User ID */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Excel File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME from_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_date C-Win
ON LEAVE OF from_date IN FRAME FRAME-A /* Beginning Post Date */
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
  IF SELF:SCREEN-VALUE BEGINS "L" THEN 
    ASSIGN lv-font-no = "12"
           lines-per-page = 55
           lv-font-name = "Courier New Size=8 (15CPI)".
    
 ELSE
    ASSIGN lv-font-no = "10"
           lines-per-page = 99
           lv-font-name = "Courier NEW SIZE=6 (20 CPI)".
 
 DISPL lv-font-no lines-per-page lv-font-name WITH FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME tb_adj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_adj C-Win
ON VALUE-CHANGED OF tb_adj IN FRAME FRAME-A /* Adjustments */
DO:
      assign {&self-name}.
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


&Scoped-define SELF-NAME tb_rec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rec C-Win
ON VALUE-CHANGED OF tb_rec IN FRAME FRAME-A /* Receipts */
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


&Scoped-define SELF-NAME tb_ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ship C-Win
ON VALUE-CHANGED OF tb_ship IN FRAME FRAME-A /* Shipments */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_total
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_total C-Win
ON VALUE-CHANGED OF tb_total IN FRAME FRAME-A /* Show Totals? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_tran
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tran C-Win
ON VALUE-CHANGED OF tb_tran IN FRAME FRAME-A /* Transfers */
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


&Scoped-define SELF-NAME to_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_date C-Win
ON LEAVE OF to_date IN FRAME FRAME-A /* Ending Post Date */
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

  ASSIGN
   from_date = TODAY
   to_date   = TODAY.
 
  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:      
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO from_date.
  END.

   RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'IL6',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i}

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
                            INPUT 'IL6',
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-case-and-tag C-Win 
PROCEDURE calc-case-and-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipr-fg-rcpth AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipr-fg-rdtlh AS ROWID NO-UNDO.
DEF INPUT PARAMETER v-fg-qty     AS INT  NO-UNDO.

DEF OUTPUT PARAMETER opv-cases AS INTEGER NO-UNDO.
DEF OUTPUT PARAMETER opv-qty-case LIKE fg-bin.case-count NO-UNDO.
DEF OUTPUT PARAMETER opv-tag      LIKE fg-rdtlh.tag NO-UNDO.

DEF BUFFER bf-fg-rcpth FOR fg-rcpth.
DEF BUFFER bf-fg-rdtlh FOR fg-rdtlh.

FIND bf-fg-rcpth WHERE ROWID(bf-fg-rcpth) = ipr-fg-rcpth NO-LOCK NO-ERROR.
FIND bf-fg-rdtlh WHERE ROWID(bf-fg-rdtlh) = ipr-fg-rdtlh NO-LOCK NO-ERROR.
IF NOT AVAIL bf-fg-rcpth THEN
    RETURN.
IF NOT AVAIL bf-fg-rdtlh THEN
    RETURN.

    if bf-fg-rdtlh.qty-case eq 0 then do:
      find first fg-bin
          where fg-bin.company eq bf-fg-rcpth.company
            and fg-bin.job-no  eq bf-fg-rcpth.job-no
            and fg-bin.job-no2 eq bf-fg-rcpth.job-no2
            and fg-bin.i-no    eq bf-fg-rcpth.i-no
            and fg-bin.loc     eq bf-fg-rdtlh.loc
            and fg-bin.loc-bin eq bf-fg-rdtlh.loc-bin
            and fg-bin.tag     eq bf-fg-rdtlh.tag
          use-index job no-lock no-error.   

      if avail fg-bin then
        assign
         opv-cases    = trunc((v-fg-qty / fg-bin.case-count),0)
         opv-qty-case = fg-bin.case-count.
         
      else do:
        find first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq bf-fg-rcpth.i-no
            no-lock no-error.
        if avail itemfg then
          assign
           opv-cases    = trunc((v-fg-qty / itemfg.case-count),0)
           opv-qty-case = itemfg.case-count.
      end.
    end.
    
    else
      assign
       opv-cases    = bf-fg-rdtlh.cases
       opv-qty-case = bf-fg-rdtlh.qty-case.             

    opv-tag = IF SUBSTRING(bf-fg-rdtlh.tag,1,15) EQ bf-fg-rcpth.i-no
            THEN SUBSTRING(bf-fg-rdtlh.tag,16,8) ELSE bf-fg-rdtlh.tag.
       

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-fg-value C-Win 
PROCEDURE calc-fg-value :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipv-sell-price AS DEC NO-UNDO.
    DEF INPUT PARAMETER ipv-sell-uom AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipr-fg-rdtlh AS ROWID NO-UNDO.
    DEF OUTPUT PARAMETER opv-fg-value AS DEC NO-UNDO.

    DEF BUFFER bf-fg-rdtlh FOR fg-rdtlh.

    FIND FIRST bf-fg-rdtlh WHERE ROWID(bf-fg-rdtlh) = ipr-fg-rdtlh NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-fg-rdtlh THEN
        RETURN.

    IF ipv-sell-uom = "L" THEN
        opv-fg-value = ipv-sell-price. /*  * bf-fg-rdtlh.qty.*/

    ELSE
    IF ipv-sell-uom = "CS" THEN
        opv-fg-value = ipv-sell-price * (bf-fg-rdtlh.qty / bf-fg-rdtlh.qty-case).
    ELSE IF ipv-sell-uom = "M" THEN
        opv-fg-value = ipv-sell-price * (bf-fg-rdtlh.qty / 1000).
    ELSE DO:
        FIND first uom
            WHERE uom.uom  EQ ipv-sell-uom
            AND uom.mult NE 0 NO-LOCK NO-ERROR.
        IF AVAIL uom THEN
            ASSIGN opv-fg-value = ipv-sell-price * (bf-fg-rdtlh.qty / uom.mult).
    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-msf-for-r C-Win 
PROCEDURE calc-msf-for-r :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipr-fg-rcpth AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipr-fg-rdtlh AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipr-lastof-key-02 AS LOG NO-UNDO.
DEF INPUT PARAMETER ipv-corr     AS LOG NO-UNDO.

DEF OUTPUT PARAMETER opv-on         AS DEC NO-UNDO.
DEF OUTPUT PARAMETER opv-qty-pallet AS INT NO-UNDO.
DEF OUTPUT PARAMETER opv-msf-1      AS DEC NO-UNDO.
DEF OUTPUT PARAMETER opv-msf-2      AS DEC NO-UNDO.

DEF BUFFER bf-fg-rcpth FOR fg-rcpth.
DEF BUFFER bf-fg-rdtlh FOR fg-rdtlh.

FIND bf-fg-rcpth WHERE ROWID(bf-fg-rcpth) = ipr-fg-rcpth NO-LOCK NO-ERROR.
IF NOT AVAIL bf-fg-rcpth THEN
    RETURN.
FIND bf-fg-rdtlh WHERE ROWID(bf-fg-rdtlh) = ipr-fg-rdtlh NO-LOCK NO-ERROR.
IF NOT AVAIL bf-fg-rdtlh THEN
    RETURN.

      opv-on = 1.
      find first job-hdr
           where job-hdr.company eq cocode
             and job-hdr.job-no  eq bf-fg-rcpth.job-no
             and job-hdr.job-no2 eq bf-fg-rcpth.job-no2
             and job-hdr.i-no    eq bf-fg-rcpth.i-no
           use-index job-no no-lock no-error.

      /* For calculating the quantity per pallet. */
      if available job-hdr then do:
        find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.job-no  eq job-hdr.job-no
              and fg-bin.job-no2 eq job-hdr.job-no2
              and fg-bin.i-no    eq job-hdr.i-no
              and fg-bin.loc-bin eq bf-fg-rdtlh.loc-bin
              and fg-bin.tag     eq bf-fg-rdtlh.tag
            no-lock no-error.
        ASSIGN opv-qty-pallet = bf-fg-rdtlh.cases * if avail fg-bin then
                                          fg-bin.cases-unit else 1.
      end.
      
      if avail job-hdr and job-hdr.est-no = "" THEN DO:
        release ef.

        run sys/inc/numup.p (job-hdr.company, job-hdr.est-no, job-hdr.frm, output opv-on).

        find first ef
            where ef.company   EQ job-hdr.company
              AND ef.est-no    EQ job-hdr.est-no
              and ef.form-no   EQ job-hdr.frm
            no-lock no-error.

        IF AVAIL ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT opv-on).

        if ipr-lastof-key-02 then           
            for each mch-act FIELDS(waste)
                where mch-act.company  eq cocode
                  and mch-act.job      eq job-hdr.job
                  and mch-act.job-no   eq job-hdr.job-no
                  and mch-act.job-no2  eq job-hdr.job-no2
                  and mch-act.frm      eq job-hdr.frm
                use-index job no-lock:
              opv-msf-2 = opv-msf-2 + (mch-act.waste * job-hdr.sq-in / 100).
            end.

        for each job-mat
            where job-mat.company eq cocode
              and job-mat.job     eq job-hdr.job
              and job-mat.job-no  eq job-hdr.job-no
              and job-mat.job-no2 eq job-hdr.job-no2
              and job-mat.frm     eq job-hdr.frm
            no-lock,
            first item
            where item.company    eq cocode
              and item.i-no       eq job-mat.i-no
              and item.mat-type   eq "B"
            no-lock:
          leave.
        end.

        if avail job-mat then do:
          assign
           opv-msf-1 = bf-fg-rdtlh.qty / opv-on * (job-mat.len * job-mat.wid)
           opv-msf-2 = opv-msf-2      / opv-on * (job-mat.len * job-mat.wid).

          if ipv-corr then
            assign
             opv-msf-1 = opv-msf-1 * .007
             opv-msf-2 = opv-msf-2 * .007.
          else
            assign
             opv-msf-1 = opv-msf-1 / 144
             opv-msf-2 = opv-msf-2 / 144.
        end.
      end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-sell-price C-Win 
PROCEDURE calc-sell-price :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipr-fg-rcpth AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER opv-sell-price LIKE oe-ordl.price NO-UNDO.
DEF OUTPUT PARAMETER opv-sell-uom LIKE oe-ordl.pr-uom NO-UNDO.

DEF BUFFER bf-fg-rcpth FOR fg-rcpth.
DEF BUFFER bf-itemfg FOR itemfg.

FIND bf-fg-rcpth WHERE ROWID(bf-fg-rcpth) = ipr-fg-rcpth NO-LOCK NO-ERROR.
IF NOT AVAIL bf-fg-rcpth THEN
    RETURN.

    /* If there is a job number for receipt... */
    IF TRIM(bf-fg-rcpth.job-no) NE "" THEN
    FOR EACH job-hdr FIELDS(company ord-no i-no job-no job-no2)
        WHERE job-hdr.company EQ bf-fg-rcpth.company
          AND job-hdr.job-no  EQ bf-fg-rcpth.job-no
          AND job-hdr.job-no2 EQ bf-fg-rcpth.job-no2
          AND job-hdr.i-no    EQ bf-fg-rcpth.i-no
          AND job-hdr.ord-no  NE 0
        USE-INDEX job-no NO-LOCK,
        FIRST oe-ordl FIELDS(price pr-uom)
        WHERE oe-ordl.company EQ job-hdr.company
          AND oe-ordl.ord-no  EQ job-hdr.ord-no
          AND oe-ordl.i-no    EQ job-hdr.i-no
          AND oe-ordl.job-no  EQ job-hdr.job-no
          AND oe-ordl.job-no2 EQ job-hdr.job-no2
          AND (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
        USE-INDEX item-ord NO-LOCK
        BY job-hdr.ord-no DESC:
      ASSIGN
       opv-sell-price = oe-ordl.price
       opv-sell-uom   = oe-ordl.pr-uom.
      LEAVE.
    END.

    /* Else if there is a PO number for receipt... */
    ELSE
    IF INT(bf-fg-rcpth.po-no) NE 0 THEN
    FOR EACH po-ordl FIELDS(company ord-no i-no)
        WHERE po-ordl.company EQ bf-fg-rcpth.company
          AND po-ordl.po-no   EQ INT(bf-fg-rcpth.po-no)
          AND po-ordl.i-no    EQ bf-fg-rcpth.i-no
          AND po-ordl.ord-no  NE 0
        USE-INDEX job-no NO-LOCK,
        FIRST oe-ordl FIELDS(price pr-uom)
        WHERE oe-ordl.company EQ po-ordl.company
          AND oe-ordl.ord-no  EQ po-ordl.ord-no
          AND oe-ordl.i-no    EQ po-ordl.i-no
          AND (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
        USE-INDEX item-ord NO-LOCK
        BY po-ordl.ord-no DESC:
      ASSIGN
       opv-sell-price = oe-ordl.price
       opv-sell-uom   = oe-ordl.pr-uom.
      LEAVE.
    END.
    IF opv-sell-price EQ 0 THEN DO:
        FIND bf-itemfg WHERE bf-itemfg.company = bf-fg-rcpth.company
                         AND bf-itemfg.i-no    = bf-fg-rcpth.i-no
                       NO-LOCK NO-ERROR.
        IF AVAIL bf-itemfg THEN
            ASSIGN opv-sell-price = bf-itemfg.sell-price
                   opv-sell-uom   = bf-itemfg.sell-uom.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt-report C-Win 
PROCEDURE create-tt-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var type as ch format "X" initial "R" NO-UNDO.
def var type-prt as ch format "X(11)" initial "           " NO-UNDO.
def var v-fg-qty as int format "->>>,>>>,>>9" NO-UNDO.
def var v-fg-cost as dec format "->>>,>>9.99<<" NO-UNDO.
def var v-tot-qty like v-fg-qty NO-UNDO.
def var v-tot-cost like v-fg-cost NO-UNDO.
def var v-fg-value as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-msf as dec format ">,>>9.999" extent 6 NO-UNDO.
def var v-grd-tot-cost as dec format "->>,>>>,>>9.99<<" NO-UNDO.                     
def var v-tot-value as dec format "->>,>>>,>>9.99" NO-UNDO.                  
def var v-grd-tot-value as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-cum-tot as de NO-UNDO.                                   
def var v-tran-type as char format "x(1)" NO-UNDO.      
def var v-entrytype as char initial "REC  ,TRAN ,ADJ  ,SHIP ,RET  ,COUNT" NO-UNDO.
def var v-on like eb.num-up NO-UNDO.
def var v-qty-pallet as decimal format "->>,>>>,>>9" no-undo.
def var v-whse like fg-rdtlh.loc NO-UNDO.   
def var v-cases like fg-rdtlh.cases no-undo.
def var v-qty-case like fg-rdtlh.qty-case no-undo.
def var v-i-no like fg-rcpth.i-no no-undo.
DEF VAR v-tag AS cha NO-UNDO.
DEF VAR lv-sell-price LIKE itemfg.sell-price NO-UNDO.
DEF VAR lv-sell-uom LIKE itemfg.sell-uom NO-UNDO.
DEF VAR lv-cost-uom LIKE fg-rcpth.pur-uom NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR v-type AS CHAR NO-UNDO.
DEF VAR v-date AS DATE NO-UNDO.
EMPTY TEMP-TABLE tt-report.


IF NOT(begin_i-no EQ "" AND END_i-no EQ "zzzzzzzzzzzzzzz") THEN
  DO:

    FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
       EACH fg-rcpth
        where fg-rcpth.company eq cocode
          AND fg-rcpth.i-no    GE begin_i-no
          AND fg-rcpth.i-no    LE end_i-no
        NO-LOCK,
        FIRST itemfg WHERE
              itemfg.company EQ cocode AND
              itemfg.i-no    EQ fg-rcpth.i-no AND
              itemfg.cust-no EQ ttCustList.cust-no /*begin_cust AND
              itemfg.cust-no LE END_cust*/
              NO-LOCK:
        LEAVE.
    END.
    
    do while avail fg-rcpth:
        {custom/statusMsg.i " 'Processing FG Item#  '  + fg-rcpth.i-no "}
      v-i-no = fg-rcpth.i-no.
   
      /* Create tt-report file for History Records */
      do i = 1 to length(trim(v-types)):
        if index("RSTAEC",substr(v-types,i,1)) gt 0 then
        DO:
           v-type = substr(v-types,i,1).
          
           for each fg-rcpth 
               where fg-rcpth.company                  eq cocode
                 and fg-rcpth.i-no                     eq v-i-no
                 and fg-rcpth.rita-code                eq v-type
                 and ((fg-rcpth.post-date              ge b-post-date and
                       fg-rcpth.post-date              le e-post-date and
                       fg-rcpth.post-date              ne ?) or
                      (fg-rcpth.trans-date             ge b-post-date and
                       fg-rcpth.trans-date             le e-post-date and
                       fg-rcpth.post-date              eq ?))
                 AND fg-rcpth.USER-ID GE begin_user
                 AND fg-rcpth.USER-ID LE END_user
               use-index i-no no-lock,
               each fg-rdtlh
               where fg-rdtlh.r-no      eq fg-rcpth.r-no
                 and fg-rdtlh.rita-code eq fg-rcpth.rita-code
               no-lock:
           
               create tt-report.
               assign
                  tt-report.term-id = ""
                  tt-report.key-01  = fg-rdtlh.loc
                  tt-report.key-02  = fg-rcpth.i-no
                  tt-report.key-03  = fg-rdtlh.loc-bin
                  tt-report.key-04  = fg-rdtlh.tag
                  tt-report.rec-id  = recid(fg-rdtlh)
                  tt-report.DATE    = fg-rcpth.trans-date.
           end.
        END.
      end.
      
      FOR EACH ttCustList 
       WHERE ttCustList.log-fld
       NO-LOCK,
         EACH fg-rcpth WHERE
          fg-rcpth.company eq cocode AND
          fg-rcpth.i-no    GT v-i-no AND
          fg-rcpth.i-no    GE begin_i-no AND
          fg-rcpth.i-no    LE end_i-no
          NO-LOCK,
          FIRST itemfg WHERE
                itemfg.company EQ cocode AND
                itemfg.i-no    EQ fg-rcpth.i-no AND
                itemfg.cust-no EQ ttCustList.cust-no /*begin_cust AND
                itemfg.cust-no LE END_cust*/
                NO-LOCK:
          LEAVE.
       END.
    end.
  END. /*begin_i-no eq blank*/
  ELSE
  DO:
     do i = 1 to length(trim(v-types)):
        if index("RSTAEC",substr(v-types,i,1)) gt 0 then
        DO:
           v-type = substr(v-types,i,1).
          
           IF NOT(begin_cust EQ "" AND END_cust EQ "zzzzzzzz") THEN
           DO v-date = b-post-date TO e-post-date:
              FOR EACH ttCustList 
                   WHERE ttCustList.log-fld
                   NO-LOCK,
                  each fg-rcpth FIELDS(r-no rita-code i-no trans-date) 
                  where fg-rcpth.company                eq cocode
                    and fg-rcpth.rita-code              eq v-type
                    and fg-rcpth.post-date              EQ v-date
                    AND CAN-FIND(FIRST itemfg WHERE
                        itemfg.company EQ cocode AND
                        itemfg.i-no    EQ fg-rcpth.i-no AND
                         itemfg.cust-no EQ ttCustList.cust-no /*begin_cust AND
                        itemfg.cust-no LE END_cust*/ ) 
                  AND fg-rcpth.USER-ID GE begin_user
                 AND fg-rcpth.USER-ID LE END_user
                  USE-INDEX post-date
                  no-lock,
                  each fg-rdtlh FIELDS(loc loc-bin tag)
                  where fg-rdtlh.r-no      eq fg-rcpth.r-no
                    and fg-rdtlh.rita-code eq fg-rcpth.rita-code
                  NO-LOCK:

              
                  create tt-report.
                  assign
                     tt-report.term-id = ""
                     tt-report.key-01  = fg-rdtlh.loc
                     tt-report.key-02  = fg-rcpth.i-no
                     tt-report.key-03  = fg-rdtlh.loc-bin
                     tt-report.key-04  = fg-rdtlh.tag
                     tt-report.rec-id  = recid(fg-rdtlh)
                     tt-report.rec-id2 = RECID(fg-rcpth)
                     tt-report.DATE    = fg-rcpth.trans-date.
                  RELEASE tt-report.
              end.
             
              FOR EACH ttCustList 
                  WHERE ttCustList.log-fld
                  NO-LOCK,
                  each fg-rcpth FIELDS(r-no rita-code i-no trans-date)
                  where fg-rcpth.company                eq cocode AND
                        fg-rcpth.rita-code              eq v-type AND
                        fg-rcpth.post-date              eq ? AND
                        fg-rcpth.trans-date             EQ v-date AND
                        NOT CAN-FIND(FIRST tt-report WHERE
                                     tt-report.rec-id2  = recid(fg-rcpth))
                        AND CAN-FIND(FIRST itemfg WHERE
                        itemfg.company EQ cocode AND
                        itemfg.i-no    EQ fg-rcpth.i-no AND
                        itemfg.cust-no EQ ttCustList.cust-no /*begin_cust AND
                        itemfg.cust-no LE END_cust*/)             
                  AND fg-rcpth.USER-ID GE begin_user
                 AND fg-rcpth.USER-ID LE END_user
                        USE-INDEX post-date
                        NO-LOCK,
                   each fg-rdtlh FIELDS(loc loc-bin tag) WHERE
                        fg-rdtlh.r-no      eq fg-rcpth.r-no AND
                        fg-rdtlh.rita-code eq fg-rcpth.rita-code
                        NO-LOCK:
             
                        create tt-report.
                        assign
                           tt-report.term-id = ""
                           tt-report.key-01  = fg-rdtlh.loc
                           tt-report.key-02  = fg-rcpth.i-no
                           tt-report.key-03  = fg-rdtlh.loc-bin
                           tt-report.key-04  = fg-rdtlh.tag
                           tt-report.rec-id  = recid(fg-rdtlh)
                           tt-report.DATE    = fg-rcpth.trans-date. 
                        RELEASE tt-report.
              END.
           END. /*v-date loop*/
           ELSE
           DO v-date = b-post-date TO e-post-date:

              for each fg-rcpth FIELDS(r-no rita-code i-no trans-date) 
                  where fg-rcpth.company                eq cocode
                    and fg-rcpth.rita-code              eq v-type
                    and fg-rcpth.post-date              EQ v-date
                  USE-INDEX post-date
                  no-lock,
                  each fg-rdtlh FIELDS(loc loc-bin tag)
                  where fg-rdtlh.r-no      eq fg-rcpth.r-no
                    and fg-rdtlh.rita-code eq fg-rcpth.rita-code
                  AND fg-rcpth.USER-ID GE begin_user
                 AND fg-rcpth.USER-ID LE END_user
                  NO-LOCK:
              
                  create tt-report.
                  assign
                     tt-report.term-id = ""
                     tt-report.key-01  = fg-rdtlh.loc
                     tt-report.key-02  = fg-rcpth.i-no
                     tt-report.key-03  = fg-rdtlh.loc-bin
                     tt-report.key-04  = fg-rdtlh.tag
                     tt-report.rec-id  = recid(fg-rdtlh)
                     tt-report.rec-id2 = RECID(fg-rcpth)
                     tt-report.DATE    = fg-rcpth.trans-date.
                  RELEASE tt-report.
              end.
             
              for each fg-rcpth FIELDS(r-no rita-code i-no trans-date)
                  where fg-rcpth.company                eq cocode AND
                        fg-rcpth.rita-code              eq v-type AND
                        fg-rcpth.post-date              eq ? AND
                        fg-rcpth.trans-date             EQ v-date AND
                        NOT CAN-FIND(FIRST tt-report WHERE
                                     tt-report.rec-id2  = recid(fg-rcpth))
                  AND fg-rcpth.USER-ID GE begin_user
                 AND fg-rcpth.USER-ID LE END_user
                        USE-INDEX post-date
                        NO-LOCK,
                   each fg-rdtlh FIELDS(loc loc-bin tag) WHERE
                        fg-rdtlh.r-no      eq fg-rcpth.r-no AND
                        fg-rdtlh.rita-code eq fg-rcpth.rita-code
                        NO-LOCK:
             
                        create tt-report.
                        assign
                           tt-report.term-id = ""
                           tt-report.key-01  = fg-rdtlh.loc
                           tt-report.key-02  = fg-rcpth.i-no
                           tt-report.key-03  = fg-rdtlh.loc-bin
                           tt-report.key-04  = fg-rdtlh.tag
                           tt-report.rec-id  = recid(fg-rdtlh)
                           tt-report.DATE    = fg-rcpth.trans-date. 
                        RELEASE tt-report.
              END.
           END. /*v-date loop*/
        END.
      end.
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
                                  INPUT 'IL6').
    

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
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect) .
  END.
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

  cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

   DO iCount = 1 TO sl_selected:NUM-ITEMS: /* task 08191414 */
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
  DISPLAY from_date to_date tb_cust-list begin_cust end_cust begin_i-no end_i-no 
          begin_user end_user tb_rec tb_ship tb_tran tb_adj tb_ret tb_count 
          tb_total sl_avail sl_selected rd-dest lines-per-page lv-ornt 
          lv-font-no lv-font-name td-show-parm tb_runExcel tb_excel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 from_date to_date tb_cust-list btnCustList begin_cust 
         end_cust begin_i-no end_i-no begin_user end_user tb_rec tb_ship 
         tb_tran tb_adj tb_ret tb_count tb_total Btn_Def sl_avail sl_selected 
         Btn_Add Btn_Remove btn_Up btn_down rd-dest lines-per-page lv-ornt 
         lv-font-no td-show-parm tb_runExcel tb_excel fi_file btn-ok btn-cancel 
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
           iColumnLength = iColumnLength + ttRptSelected.FieldLength + 1.
           .        
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-values C-Win 
PROCEDURE init-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
   tb_rec   = CAN-FIND(FIRST fg-rcpth
                       WHERE fg-rcpth.company   EQ gcompany
                         AND fg-rcpth.rita-code EQ "R")
   tb_ship  = CAN-FIND(FIRST fg-rcpth
                       WHERE fg-rcpth.company   EQ gcompany
                         AND fg-rcpth.rita-code EQ "S")
   tb_tran  = CAN-FIND(FIRST fg-rcpth
                       WHERE fg-rcpth.company   EQ gcompany
                         AND fg-rcpth.rita-code EQ "T")
   tb_adj   = CAN-FIND(FIRST fg-rcpth
                       WHERE fg-rcpth.company   EQ gcompany
                         AND fg-rcpth.rita-code EQ "A")
   tb_ret   = CAN-FIND(FIRST fg-rcpth
                       WHERE fg-rcpth.company   EQ gcompany
                         AND fg-rcpth.rita-code EQ "E")
   tb_count = CAN-FIND(FIRST fg-rcpth
                       WHERE fg-rcpth.company   EQ gcompany
                         AND fg-rcpth.rita-code EQ "C").

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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR str-tit4 AS cha NO-UNDO.
DEF VAR str-tit5 AS cha NO-UNDO.
/*DEF VAR iStrTitLength AS INT NO-UNDO.
iStrTitLength = iColumnLength - 10.
{sys/form/r-top5DL.f &Width=icolumnLength &TitleLength=iStrTitLength }
*/
{sys/form/r-top5DL.f}
DEF BUFFER b-fg-bin FOR fg-bin.

def var type as ch format "X" initial "R" NO-UNDO.
def var type-prt as ch format "X(11)" initial "           " NO-UNDO.
def var v-fg-qty as int format "->>>,>>>,>>9" NO-UNDO.
def var v-fg-cost as dec format "->>>,>>9.99<<" NO-UNDO.
def var v-tot-qty like v-fg-qty NO-UNDO.
def var v-tot-cost like v-fg-cost NO-UNDO.
def var v-fg-value as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-msf as dec format ">,>>9.999" extent 6 NO-UNDO.
def var v-grd-tot-cost as dec format "->>,>>>,>>9.99<<" NO-UNDO.                     
def var v-tot-value as dec format "->>,>>>,>>9.99" NO-UNDO.                  
def var v-grd-tot-value as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-cum-tot as de NO-UNDO.                                   
def var v-tran-type as char format "x(1)" NO-UNDO.      
def var v-entrytype as char initial "REC  ,TRAN ,ADJ  ,SHIP ,ERET ,COUNT" NO-UNDO.
def var v-on like eb.num-up NO-UNDO.
def var v-qty-pallet as decimal format "->>,>>>,>>9" no-undo.
def var v-whse like fg-rdtlh.loc NO-UNDO.   
def var v-cases like fg-rdtlh.cases no-undo.
def var v-qty-case like fg-rdtlh.qty-case no-undo.
def var v-i-no like fg-rcpth.i-no no-undo.
DEF VAR v-tag AS cha NO-UNDO.
DEF VAR lv-sell-price LIKE itemfg.sell-price NO-UNDO.
DEF VAR lv-sell-uom LIKE itemfg.sell-uom NO-UNDO.
DEF VAR lv-cost-uom LIKE fg-rcpth.pur-uom NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR v-type AS CHAR NO-UNDO.
DEF VAR v-date AS DATE NO-UNDO.
DEF VAR v-current-job LIKE fg-rcpth.job-no NO-UNDO INIT "".
DEF VAR v-current-job2 LIKE fg-rcpth.job-no2 NO-UNDO INIT "".
DEF VAR v-new-job AS LOGICAL NO-UNDO INIT NO.
DEF VAR v-tot-pos1 AS INT NO-UNDO.
DEF VAR v-tot-pos2 AS INT NO-UNDO.
DEF VAR v-tot-pos3 AS INT NO-UNDO.
def buffer b-fgrdtlh for fg-rdtlh.
DEF VAR cSpace AS cha INIT " " NO-UNDO.
DEF VAR prom-date AS DATE NO-UNDO.
DEF VAR due-date AS DATE NO-UNDO.
DEF VAR job-start AS DATE NO-UNDO.
DEF VAR v-stnd-cost AS DECIMAL INIT 0 NO-UNDO.

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

DEF VAR v-shipto AS CHAR NO-UNDO .
DEF VAR v-shipto-name AS CHAR NO-UNDO .

DEF BUFFER bfg-rcpth FOR fg-rcpth.
DEF BUFFER bpo-ord FOR po-ord.
DEF BUFFER bitemfg FOR itemfg.
DEF VAR v-numUp AS INT NO-UNDO.
DEF VAR v-numColors AS INT NO-UNDO.
DEF VAR v-SheetSize AS cha NO-UNDO.
DEF VAR v-Caliper AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
{ce/msfcalc.i}

form fg-rcpth.trans-date            label "DATE"   format "99/99/99"
     fg-rcpth.i-no   format "x(15)" label "ITEM"
     fg-rcpth.i-name format "x(9)"  label "DESCRIPTN"
     fg-rcpth.po-no                 label "P.O. #"
     po-ord.vend-no                 label "VENDOR"
     v-tran-type                    label "T"
     fg-rdtlh.tag                   label "TAG #"
     v-cases                        label "UNITS"   format "->>,>>9"
     v-qty-case                     label "COUNT"   format "->>>,>>9"
     fg-rdtlh.qty-case              label "COUNT"   format "->>>,>>9"
     fg-rdtlh.loc-bin               label "BIN"
     lv-cost-uom                    label "CUOM"
     v-fg-qty                       label "TOT QTY"
     v-fg-cost                      label "TOT COST"
     v-fg-value                     label "TOT SELL VALUE"    
    with frame itemx no-box down STREAM-IO width 200.

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
  /* IF  rsShowVendor = "Vendor" THEN
        excelheader = "DATE,ITEM,DESCRIPTN,P.O. #,VENDOR,T,TAG #,UNITS,"
               + "COUNT,BIN,UOM,TOT QTY,TOT COST,TOT SELL VALUE".
   ELSE
        excelheader = "DATE,ITEM,DESCRIPTN,P.O. #,JOB #,T,TAG #,UNITS,"
               + "COUNT,BIN,UOM,TOT QTY,TOT COST,TOT SELL VALUE".
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
   */
END.

assign
 str-tit2 = "I-L-6 "+ c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 b-post-date   = from_date
 e-post-date   = to_date  
 v-types       = TRIM(STRING(tb_rec,"R/"))  + TRIM(STRING(tb_ret,"E/"))   +
                 TRIM(STRING(tb_tran,"T/")) + TRIM(STRING(tb_ship,"S/"))  +
                 TRIM(STRING(tb_adj,"A/"))  + TRIM(STRING(tb_count,"C/"))
 v-pr-tots     = tb_total.
 
FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:
    IF ttRptSelected.TextList = "WHSE" THEN 
        ASSIGN excelheader = excelHeader + ttRptSelected.TextList + ",".
    ELSE  IF ttRptSelected.TextList = "Transaction Type" THEN 
       ASSIGN str-tit4 = str-tit4 + 
               substring(ttRptSelected.TextList,1,1) + " "
            str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + substring(ttRptSelected.TextList,1,1) + ",".
    ELSE
     ASSIGN str-tit4 = str-tit4 + 
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength + 1 - LENGTH(ttRptSelected.TextList))
            str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".
END.
/*
ASSIGN str-tit4 = 
      (IF {sys/inc/rptDisp.i "fg-rcpth.trans-date"} THEN 
        "DATE     " ELSE "" ) +  /*9*/
    (IF {sys/inc/rptDisp.i "fg-rcpth.i-no"} THEN 
        "ITEM            " ELSE "" ) +   /*16*/
    (IF {sys/inc/rptDisp.i "fg-rcpth.i-name"} THEN 
        "DESCRIPTION " ELSE "" ) +  /*11*/
    (IF {sys/inc/rptDisp.i "fg-rcpth.po-no"} THEN 
        "P.O. #    " ELSE "" ) +  /*9*/
    (IF {sys/inc/rptDisp.i "po-ord.vend-no"} THEN 
        "VENDOR #   " ELSE "")  +  /*10*/
    (IF {sys/inc/rptDisp.i "fg-rcpth.job-no"} THEN
       "JOB         " ELSE "" ) +   /*11*/
    (IF {sys/inc/rptDisp.i "v-tran-type"} THEN 
        "T " ELSE "" ) +    /*1*/
    (IF {sys/inc/rptDisp.i "rfidtag.rfidtag"} THEN
       "RFID TAG #    " ELSE "" ) +  /*13*/
    (IF {sys/inc/rptDisp.i "v-tag"} THEN
       "TAG #         " ELSE "" ) +  /*13*/
    (IF {sys/inc/rptDisp.i "v-cases"}  THEN
       "UNITS    " ELSE "" ) +  /*8*/
    (IF {sys/inc/rptDisp.i "v-qty-case"}  THEN
       "COUNT    " ELSE "" ) +  /*8*/
    (IF {sys/inc/rptDisp.i "fg-rdtlh.loc-bin"}  THEN
       "BIN       " ELSE "" ) +  /*9*/
    (IF {sys/inc/rptDisp.i "lv-cost-uom"}       THEN
       "UOM       " ELSE "" ) +  /*9*/
    (IF {sys/inc/rptDisp.i "v-fg-qty"}  THEN
       "TOT QTY    " ELSE "" ) +  /*10*/
    (IF {sys/inc/rptDisp.i "v-fg-cost"}   THEN
       "TOT COST  " ELSE "" ) +  /*9*/
    (IF {sys/inc/rptDisp.i "v-fg-value"}  THEN
       "TOT SELL VALUE" ELSE ""  )  /*14*/
 str-tit5 = 
      (IF {sys/inc/rptDisp.i "fg-rcpth.trans-date"} THEN 
        "-------- " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "fg-rcpth.i-no"} THEN 
        "--------------- " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "fg-rcpth.i-name"} THEN 
        "----------- " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "fg-rcpth.po-no"} THEN 
        "--------- " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "po-ord.vend-no"} THEN 
        "---------- " ELSE "")  +
    (IF {sys/inc/rptDisp.i "fg-rcpth.job-no"} THEN
       "----------- " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "v-tran-type"} THEN 
        "_ " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "rfidtag.rfidtag"} THEN
       "------------- " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "v-tag"} THEN
       "------------- " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "v-cases"}  THEN
       "-------- " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "v-qty-case"}  THEN
       "-------- " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "fg-rdtlh.loc-bin"}  THEN
       "--------- " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "lv-cost-uom"}       THEN
       "--------- " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "v-fg-qty"}  THEN
       "---------- " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "v-fg-cost"}   THEN
       "--------- " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "v-fg-value"}  THEN
       "------------- " ELSE "" 
    .*/

{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}
if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE("general").
RUN create-tt-report.

IF tb_excel THEN PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.

{fgrep/r-fgpstrN.i}

  if v-pr-tots then
    put " " to 124 skip       
        "MSF->  FG: " + trim(STRING(v-msf[5],"->>,>>9.9<<")) +
        "  Wst: " + trim(STRING(v-msf[6],"->>,>>9.9<<"))    +
        "  Tot: " + trim(STRING(v-msf[5] + v-msf[6],"->>,>>9.9<<"))
                             format "x(63)" at 15
        "GRAND TOTAL COST & SELL VALUE:" to 111
        v-grd-tot-cost to v-tot-pos2
        v-grd-tot-value to v-tot-pos3
        skip .  

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
     OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report_Save C-Win 
PROCEDURE run-report_Save :
/* ---------------------------------------------- fg/rep/fg-posth.p 03/00 EKW */
/* finished goods - warehouse transactions posting tt-report (From History)      */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

DEF BUFFER b-fg-bin FOR fg-bin.

def var type as ch format "X" initial "R" NO-UNDO.
def var type-prt as ch format "X(11)" initial "           " NO-UNDO.
def var v-fg-qty as int format "->>>,>>>,>>9" NO-UNDO.
def var v-fg-cost as dec format "->>>,>>9.99<<" NO-UNDO.
def var v-tot-qty like v-fg-qty NO-UNDO.
def var v-tot-cost like v-fg-cost NO-UNDO.
def var v-fg-value as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-msf as dec format ">,>>9.999" extent 6 NO-UNDO.
def var v-grd-tot-cost as dec format "->>,>>>,>>9.99<<" NO-UNDO.                     
def var v-tot-value as dec format "->>,>>>,>>9.99" NO-UNDO.                  
def var v-grd-tot-value as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-cum-tot as de NO-UNDO.                                   
def var v-tran-type as char format "x(1)" NO-UNDO.      
def var v-entrytype as char initial "REC  ,TRAN ,ADJ  ,SHIP ,ERET ,COUNT" NO-UNDO.
def var v-on like eb.num-up NO-UNDO.
def var v-qty-pallet as decimal format "->>,>>>,>>9" no-undo.
def var v-whse like fg-rdtlh.loc NO-UNDO.   
def var v-cases like fg-rdtlh.cases no-undo.
def var v-qty-case like fg-rdtlh.qty-case no-undo.
def var v-i-no like fg-rcpth.i-no no-undo.
DEF VAR v-tag AS cha NO-UNDO.
DEF VAR lv-sell-price LIKE itemfg.sell-price NO-UNDO.
DEF VAR lv-sell-uom LIKE itemfg.sell-uom NO-UNDO.
DEF VAR lv-cost-uom LIKE fg-rcpth.pur-uom NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR v-type AS CHAR NO-UNDO.
DEF VAR v-date AS DATE NO-UNDO.
DEF VAR v-current-job LIKE fg-rcpth.job-no NO-UNDO INIT "".
DEF VAR v-current-job2 LIKE fg-rcpth.job-no2 NO-UNDO INIT "".
DEF VAR v-new-job AS LOGICAL NO-UNDO INIT NO.
DEF VAR v-tot-pos1 AS INT NO-UNDO.
DEF VAR v-tot-pos2 AS INT NO-UNDO.
DEF VAR v-tot-pos3 AS INT NO-UNDO.
def buffer b-fgrdtlh for fg-rdtlh.

{ce/msfcalc.i}

form fg-rcpth.trans-date            label "DATE"   format "99/99/99"
     fg-rcpth.i-no   format "x(15)" label "ITEM"
     fg-rcpth.i-name format "x(9)"  label "DESCRIPTN"
     fg-rcpth.po-no                 label "P.O. #"
     po-ord.vend-no                 label "VENDOR"
     v-tran-type                    label "T"
     fg-rdtlh.tag                   label "TAG #"
     v-cases                        label "UNITS"   format "->>,>>9"
     v-qty-case                     label "COUNT"   format "->>>,>>9"
     fg-rdtlh.qty-case              label "COUNT"   format "->>>,>>9"
     fg-rdtlh.loc-bin               label "BIN"
     lv-cost-uom                    label "CUOM"
     v-fg-qty                       label "TOT QTY"
     v-fg-cost                      label "TOT COST"
     v-fg-value                     label "TOT SELL VALUE"    
    with frame itemx no-box down STREAM-IO width 200.

form fg-rcpth.trans-date            label "DATE"   format "99/99/99"
     fg-rcpth.i-no   format "x(15)" label "ITEM"
     fg-rcpth.i-name format "x(9)"  label "DESCRIPTN"
     fg-rcpth.po-no                 label "P.O. #"
     fg-rcpth.job-no                label "Job" FORMAT "x(11)"
     v-tran-type                    label "T"
     fg-rdtlh.tag                   label "TAG #"
     v-cases                        label "UNITS"   format "->>,>>9"
     v-qty-case                     label "COUNT"   format "->>>,>>9"
     fg-rdtlh.qty-case              label "COUNT"   format "->>>,>>9"
     fg-rdtlh.loc-bin               label "BIN"
     lv-cost-uom                    label "CUOM"
     v-fg-qty                       label "TOT QTY"
     v-fg-cost                      label "TOT COST"
     v-fg-value                     label "TOT SELL VALUE"    
    with frame itemy no-box down STREAM-IO width 200.

form fg-rcpth.trans-date            label "DATE"   format "99/99/99"
     fg-rcpth.i-no   format "x(15)" label "ITEM"
     fg-rcpth.i-name format "x(9)"  label "DESCRIPTN"
     fg-rcpth.po-no                 label "P.O. #"
     po-ord.vend-no                 label "VENDOR"
     v-tran-type                    label "T"
     rfidtag.rfidtag                COLUMN-LABEL "RFID!TAG #"
     v-cases                        label "UNITS"   format "->>,>>9"
     v-qty-case                     label "COUNT"   format "->>>,>>9"
     fg-rdtlh.qty-case              label "COUNT"   format "->>>,>>9"
     fg-rdtlh.loc-bin               label "BIN"
     lv-cost-uom                    label "CUOM"
     v-fg-qty                       label "TOT QTY"
     v-fg-cost                      label "TOT COST"
     v-fg-value                     label "TOT SELL VALUE"    
    with frame itemz no-box down STREAM-IO width 200.

form fg-rcpth.trans-date            label "DATE"   format "99/99/99"
     fg-rcpth.i-no   format "x(15)" label "ITEM"
     fg-rcpth.i-name format "x(9)"  label "DESCRIPTN"
     fg-rcpth.po-no                 label "P.O. #"
     fg-rcpth.job-no                label "Job" FORMAT "x(11)"
     v-tran-type                    label "T"
     rfidtag.rfidtag                COLUMN-LABEL "RFID!TAG #"
     v-cases                        label "UNITS"   format "->>,>>9"
     v-qty-case                     label "COUNT"   format "->>>,>>9"
     fg-rdtlh.qty-case              label "COUNT"   format "->>>,>>9"
     fg-rdtlh.loc-bin               label "BIN"
     lv-cost-uom                    label "CUOM"
     v-fg-qty                       label "TOT QTY"
     v-fg-cost                      label "TOT COST"
     v-fg-value                     label "TOT SELL VALUE"    
    with frame itemzz no-box down STREAM-IO width 200.


IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   excelheader = "DATE,ITEM,DESCRIPTN,P.O. #,VENDOR,T,TAG #,UNITS,"
               + "COUNT,COUNT,BIN,CUOM,TOT QTY,TOT COST,TOT SELL VALUE".
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

assign
 str-tit2 = "I-L-6 "+ c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 b-post-date   = from_date
 e-post-date   = to_date  
 v-types       = TRIM(STRING(tb_rec,"R/"))  + TRIM(STRING(tb_ret,"E/"))   +
                 TRIM(STRING(tb_tran,"T/")) + TRIM(STRING(tb_ship,"S/"))  +
                 TRIM(STRING(tb_adj,"A/"))  + TRIM(STRING(tb_count,"C/"))
 v-pr-tots     = tb_total.
 
{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE("general").

RUN create-tt-report.

  for each tt-report where tt-report.term-id eq "" no-lock,
      first fg-rdtlh where recid(fg-rdtlh) eq tt-report.rec-id no-lock,
      /* find fg-rcpth */
      first fg-rcpth
      where fg-rdtlh.r-no      eq fg-rcpth.r-no
        and fg-rdtlh.rita-code eq fg-rcpth.rita-code
      no-lock
      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            by tt-report.key-04
            BY tt-report.DATE
      /* with frame itemx */:

      {custom/statusMsg.i " 'Processing FG Item#  '  + fg-rdtlh.i-no "}

      IF fg-rcpth.job-no <> v-current-job THEN
      ASSIGN v-new-job = YES
             v-current-job = fg-rcpth.job-no
             v-current-job2 = fg-rcpth.job-no2.
      ELSE
          ASSIGN v-new-job = NO.

    if first-of(tt-report.key-01) then do:             
      v-whse = fg-rdtlh.loc.
      
      if first(tt-report.key-01) then do:
        hide frame r-top.
        VIEW frame r-top.
        page.
      end.
      else put skip(3) "WHSE: " v-whse skip(1).
    end.

    if fg-rcpth.rita-code eq "S" then do:
      find first fg-bin
          where fg-bin.company eq fg-rcpth.company
            and fg-bin.job-no  eq fg-rcpth.job-no
            and fg-bin.job-no2 eq fg-rcpth.job-no2
            and fg-bin.i-no    eq fg-rcpth.i-no
            and fg-bin.loc     eq fg-rdtlh.loc
            and fg-bin.loc-bin eq fg-rdtlh.loc-bin
            and fg-bin.tag     eq fg-rdtlh.tag
          use-index job no-lock no-error.
      ASSIGN lv-cost-uom = if avail fg-bin then fg-bin.pur-uom else "M".
    end.
    ELSE 
        ASSIGN lv-cost-uom = fg-rcpth.pur-uom.

    /* SAB: Moved this up to get the UOM ahead of the "v-fg-cost" calculation below. */
    ASSIGN
     lv-sell-price = 0
     lv-sell-uom   = "EA".

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq fg-rcpth.i-no
        use-index i-no no-lock no-error.
    if avail itemfg then do:
      ASSIGN
       lv-sell-price = itemfg.sell-price
       lv-sell-uom   = itemfg.sell-uom    
       lv-cost-uom   = itemfg.prod-uom.

    /* calculate the cost based on fg-rcpth.pur-uom. */
    assign
     v-fg-qty   = fg-rdtlh.qty
     v-fg-cost  = fg-rdtlh.cost * (v-fg-qty / IF lv-cost-uom EQ "M" THEN 1000 ELSE 1)
     v-fg-value = 0
     v-msf[1]   = 0
     v-msf[2]   = 0.

    release job-mat.
    if fg-rcpth.rita-code eq "R" then
        RUN calc-msf-for-r (INPUT ROWID(fg-rcpth),
                            INPUT ROWID(fg-rdtlh),
                            INPUT last-of(tt-report.key-02),
                            INPUT v-corr,
                            OUTPUT v-on,
                            OUTPUT v-qty-pallet,
                            OUTPUT v-msf[1],
                            OUTPUT v-msf[2]).

      if fg-rcpth.rita-code eq "R" then do:
        if v-msf[1] gt fg-rdtlh.qty * itemfg.t-sqft then
          v-msf[2] = v-msf[2] +
                     (v-msf[1] - (fg-rdtlh.qty * itemfg.t-sqft)).
          v-msf[1] = fg-rdtlh.qty * itemfg.t-sqft.
      END.

    END.

    RUN calc-sell-price (INPUT ROWID(fg-rcpth), 
                         OUTPUT lv-sell-price, 
                         OUTPUT lv-sell-uom).

    RUN calc-fg-value (INPUT lv-sell-price,
                       INPUT lv-sell-uom,
                       INPUT ROWID(fg-rdtlh),
                       OUTPUT v-fg-value).
    assign
     v-msf[1] = v-msf[1] / 1000
     v-msf[2] = v-msf[2] / 1000.

    if index("RTASEC", fg-rcpth.rita-code) ne 0 then
      v-tran-type = entry(index("RTASEC", fg-rcpth.rita-code),v-entrytype).
    else v-tran-type = "".

    if line-counter eq 56 then page.

    if fg-rcpth.po-no ne " " then
    find first po-ord
        where po-ord.company eq cocode
          and po-ord.po-no   eq int(fg-rcpth.po-no)
        no-lock no-error.                
    RUN calc-case-and-tag (INPUT ROWID(fg-rcpth),
                           INPUT ROWID(fg-rdtlh),
                           INPUT v-fg-qty,
                           OUTPUT v-cases,
                           OUTPUT v-qty-case,
                           OUTPUT v-tag).

    find first loadtag where loadtag.item-type = NO
                         AND loadtag.company eq cocode
                         and loadtag.i-no    eq fg-rcpth.i-no
                         AND loadtag.tag-no  EQ fg-rdtlh.tag
                       NO-LOCK NO-ERROR.
    IF AVAIL loadtag THEN
      FIND FIRST rfidtag OF loadtag NO-LOCK NO-ERROR.

    IF rsShowVendor = "Vendor" AND rsShowTag = "Tag#" THEN DO:

        display fg-rcpth.trans-date     /*  when first-of(tt-report.key-02) */
                fg-rcpth.i-no           /*  when first-of(tt-report.key-02) */
                fg-rcpth.i-name
                fg-rcpth.po-no
                po-ord.vend-no            when avail po-ord AND fg-rcpth.po-no <> ""                
                v-tran-type
                v-tag @ fg-rdtlh.tag
                v-cases
                v-qty-case
                v-fg-qty            /* (sub-total by fg-rcpth.i-no) */
                fg-rdtlh.loc-bin
                lv-cost-uom       
                v-fg-cost           /* (sub-total by fg-rcpth.i-no) */
                v-fg-value
                                                 
            with frame itemx.
        down with frame itemx.

    END.
    ELSE IF rsShowVendor = "Job#" AND rsShowTag = "Tag#" THEN DO:

        display fg-rcpth.trans-date     /*  when first-of(tt-report.key-02) */
                fg-rcpth.i-no           /*  when first-of(tt-report.key-02) */
                fg-rcpth.i-name
                fg-rcpth.po-no
                /* po-ord.vend-no            when avail po-ord AND fg-rcpth.po-no <> "" */
                fg-rcpth.job-no + "-" + string(fg-rcpth.job-no2) WHEN fg-rcpth.job-no GT "" @ fg-rcpth.job-no
                v-tran-type
                v-tag @ fg-rdtlh.tag
                v-cases
                v-qty-case
                v-fg-qty            /* (sub-total by fg-rcpth.i-no) */
                fg-rdtlh.loc-bin
                lv-cost-uom       
                v-fg-cost           /* (sub-total by fg-rcpth.i-no) */
                v-fg-value
                                                 
            with frame itemy.
        down with frame itemy.

    END.
    ELSE IF rsShowVendor = "Vendor" AND rsShowTag = "RFID#" THEN DO:

            display fg-rcpth.trans-date     /*  when first-of(tt-report.key-02) */
                    fg-rcpth.i-no           /*  when first-of(tt-report.key-02) */
                    fg-rcpth.i-name
                    fg-rcpth.po-no
                    po-ord.vend-no            when avail po-ord AND fg-rcpth.po-no <> ""                     
                    v-tran-type
                    substring(rfidtag.rfidtag, 13)  WHEN AVAIL(rfidtag) @ rfidtag.rfidtag
                    v-cases
                    v-qty-case
                    v-fg-qty            /* (sub-total by fg-rcpth.i-no) */
                    fg-rdtlh.loc-bin
                    lv-cost-uom       
                    v-fg-cost           /* (sub-total by fg-rcpth.i-no) */
                    v-fg-value
    
                with frame itemz.
            down with frame itemz.

    END.
    ELSE IF rsShowVendor = "Job#" AND rsShowTag = "RFID#" THEN DO:

            display fg-rcpth.trans-date     /*  when first-of(tt-report.key-02) */
                    fg-rcpth.i-no           /*  when first-of(tt-report.key-02) */
                    fg-rcpth.i-name
                    fg-rcpth.po-no
                    /* po-ord.vend-no            when avail po-ord AND fg-rcpth.po-no <> "" */
                    fg-rcpth.job-no + "-" + string(fg-rcpth.job-no2) WHEN fg-rcpth.job-no GT ""  @ fg-rcpth.job-no
                    v-tran-type
                    substring(rfidtag.rfidtag, 13) WHEN AVAIL(rfidtag) @ rfidtag.rfidtag
                    v-cases
                    v-qty-case
                    v-fg-qty            /* (sub-total by fg-rcpth.i-no) */
                    fg-rdtlh.loc-bin
                    lv-cost-uom       
                    v-fg-cost           /* (sub-total by fg-rcpth.i-no) */
                    v-fg-value
                with frame itemzz.
            down with frame itemzz.

    END.

    IF tb_excel THEN 
      PUT STREAM excel UNFORMATTED
          '"' STRING(fg-rcpth.trans-date)                              '",'
          '"' STRING(fg-rcpth.i-no)                                    '",'
          '"' fg-rcpth.i-name                                          '",'
          '"' fg-rcpth.po-no                                           '",'
          '"' (IF avail po-ord AND fg-rcpth.po-no <> "" THEN po-ord.vend-no
               ELSE "")                                                '",'
          '"' STRING(v-tran-type,"X(1)")                               '",'
          '"' v-tag                                                    '",'
          '"' STRING(v-cases,"->>,>>9")                                '",'
          '"' STRING(v-qty-case,"->>>,>>9")                            '",'
          '"' ""                                                       '",'
          '"' fg-rdtlh.loc-bin                                         '",'
          '"' lv-cost-uom                                              '",'
          '"' STRING(v-fg-qty,"->>>,>>9")                              '",'
          '"' STRING(v-fg-cost,"->>>,>>9.99<<")                        '",'
          '"' STRING(v-fg-value,"->>,>>>,>>9.99")                      '",'
          SKIP.

    if fg-rdtlh.rita-code eq "T" then
      put "To: " to 94 fg-rdtlh.loc2 fg-rdtlh.loc-bin2 skip.

    if v-pr-tots then do:    
      IF rsShowVendor = "Vendor" AND rsShowTag = "TAG#" THEN
      ASSIGN v-tot-pos1 = 116
             v-tot-pos2 = 128
             v-tot-pos3 = 143.
      ELSE IF rsShowVendor = "Vendor" AND rsShowTag = "RFID#"
              OR rsShowVendor = "Job#" AND rsShowTag = "Tag#" THEN
      ASSIGN v-tot-pos1 = 119
             v-tot-pos2 = 131
             v-tot-pos3 = 146.
      ELSE IF rsShowVendor = "Job#" AND rsShowTag = "RFID#" THEN
      ASSIGN v-tot-pos1 = 123
             v-tot-pos2 = 135
             v-tot-pos3 = 150.

      assign
       v-tot-qty = v-tot-qty + v-fg-qty
       v-tot-cost = v-tot-cost + v-fg-cost
       v-grd-tot-cost = v-grd-tot-cost + v-fg-cost   
          /* Do this when sell uom = "L" and first-of job */
/*        v-tot-value = v-tot-value + v-fg-value  */
/*        v-grd-tot-value = v-grd-tot-value + v-fg-value */
       v-msf[3] = v-msf[3] + v-msf[1]
       v-msf[4] = v-msf[4] + v-msf[2].

      /* Do not accumulate total for sell-uom = "L" */
      IF (v-new-job = YES AND lv-sell-uom = "L") OR (lv-sell-uom <> "L") THEN
          ASSIGN v-tot-value = v-tot-value + v-fg-value
                 v-grd-tot-value = v-grd-tot-value + v-fg-value.

      if fg-rdtlh.rita-code eq "R" or
         fg-rdtlh.rita-code eq "A" or
         fg-rdtlh.rita-code eq "E" then
        v-cum-tot  = v-cum-tot + v-fg-cost.
      else
      if fg-rdtlh.rita-code eq "S" then
        v-cum-tot  = v-cum-tot - v-fg-cost.
    end.  /*   if v-pr-tots   */ 
    
    if v-pr-tots then do:                                                              if last-of(tt-report.key-02) then do:
        put "-----------" to v-tot-pos1
            "----------" to v-tot-pos2
            "--------------" to v-tot-pos3
            skip.                

        if fg-rcpth.rita-code eq "R" then
          put "MSF->  FG: " + trim(STRING(v-msf[3],"->>,>>9.9<<")) +
              "  Wst: " + trim(STRING(v-msf[4],"->>,>>9.9<<"))    +
              "  Tot: " + trim(STRING(v-msf[3] + v-msf[4],"->>,>>9.9<<"))
                             format "x(63)" at 15.

        put v-tot-qty to v-tot-pos1
            v-tot-cost to v-tot-pos2
            v-tot-value to v-tot-pos3 skip(1).

        assign
         v-msf[5]    = v-msf[5] + v-msf[3]
         v-msf[6]    = v-msf[6] + v-msf[4]
         v-tot-qty   = 0
         v-tot-cost  = 0
         v-tot-value = 0
         v-msf[3]    = 0
         v-msf[4]    = 0.
      end.  /* if last-of(fg-rcpth.i-no) */        
    end. /* if v-pr-tots */
  end.

  if v-pr-tots then
    put " " to 124 skip       
        "MSF->  FG: " + trim(STRING(v-msf[5],"->>,>>9.9<<")) +
        "  Wst: " + trim(STRING(v-msf[6],"->>,>>9.9<<"))    +
        "  Tot: " + trim(STRING(v-msf[5] + v-msf[6],"->>,>>9.9<<"))
                             format "x(63)" at 15
        "GRAND TOTAL COST & SELL VALUE:" to 111
        v-grd-tot-cost to v-tot-pos2
        v-grd-tot-value to v-tot-pos3
        skip .  

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
     OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

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
        begin_cust:SENSITIVE = NOT iplChecked
        end_cust:SENSITIVE = NOT iplChecked
        begin_cust:VISIBLE = NOT iplChecked
        end_cust:VISIBLE = NOT iplChecked
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
     if lookup("parm",lv-field-hdl:private-data) > 0
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
                  if lookup(lv-field-hdl:NAME,lv-field2-hdl:private-data) > 0 then do:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
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

