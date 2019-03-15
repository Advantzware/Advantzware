&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-hsaldt.w

  Description: S/A High Sales By Date Range

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

/*{sys/inc/custlistform.i ""HR2"" }*/
{sys/ref/CustList.i NEW}

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD dec1 AS DEC FIELD dec2 AS DEC.

DEF TEMP-TABLE tt-report2 NO-UNDO LIKE report
    FIELD dec1 AS DEC FIELD dec2 AS DEC
    INDEX high-sales key-01 dec1 DESC dec2 DESC.

DEF STREAM excel.
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.
DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "Customer,Name,Rep,Sales Rep Name,Date Range 1,Date Range 2,Date Range 3,Date Range 4,Total Amt" 

       cFieldListToSelect = "cust,name,rep,rep-name,date1,date2,date3,date4,tot" 

       cFieldLength = "8,30,3,20,17,17,17,17,17" 
       cFieldType = "c,c,c,c,i,i,i,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Customer,Name,Rep,Date Range 1,Date Range 2,Date Range 3,Date Range 4,Total Amt"
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date1 end_date1 ~
begin_date2 end_date2 begin_date3 end_date3 begin_date4 end_date4 srt-col ~
cust rd_print tb_cust-list btnCustList begin_cust end_cust begin_slsmn ~
end_slsmn tb_zer-col sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up ~
btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date1 end_date1 begin_date2 ~
end_date2 begin_date3 end_date3 begin_date4 end_date4 srt-col cust lbl_sort ~
rd_print tb_cust-list begin_cust end_cust begin_slsmn end_slsmn tb_zer-col ~
sl_avail sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 begin_date1 end_date1 begin_date2 end_date2 ~
begin_date3 end_date3 begin_date4 end_date4 

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
DEFINE BUTTON btn-cancel /*AUTO-END-KEY */
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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Cust#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date1 AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "1" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_date2 AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "2" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_date3 AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "3" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_date4 AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "4" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE cust AS INTEGER FORMAT ">>>>>" INITIAL 99999 
     LABEL "Customers To Print" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Cust#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_date1 AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date2 AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date3 AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date4 AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-hsaldt.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

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

DEFINE VARIABLE srt-col AS INTEGER FORMAT ">>":U INITIAL 1 
     LABEL "Sort By Column" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_print AS CHARACTER INITIAL "Customer" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer", "Customer",
"Salesrep", "Salesrep"
     SIZE 31 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 8.57.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 11.91.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .95 NO-UNDO.

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

DEFINE VARIABLE tb_zer-col AS LOGICAL INITIAL yes 
     LABEL "Print Customers with Zero Sales?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date1 AT ROW 2.14 COL 30 COLON-ALIGNED
     end_date1 AT ROW 2.14 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_date2 AT ROW 3.1 COL 30 COLON-ALIGNED
     end_date2 AT ROW 3.1 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_date3 AT ROW 4.05 COL 30 COLON-ALIGNED
     end_date3 AT ROW 4.05 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_date4 AT ROW 5 COL 30 COLON-ALIGNED
     end_date4 AT ROW 5 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     srt-col AT ROW 6.19 COL 30 COLON-ALIGNED
     cust AT ROW 7.38 COL 30 COLON-ALIGNED HELP
          "Customers To Print"
     lbl_sort AT ROW 7.38 COL 45.8 COLON-ALIGNED NO-LABEL
     rd_print AT ROW 7.38 COL 57.8 NO-LABEL
     tb_cust-list AT ROW 8.48 COL 31.8 WIDGET-ID 6
     btnCustList AT ROW 8.52 COL 64.2 WIDGET-ID 8
     begin_cust AT ROW 9.67 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 9.67 COL 68.6 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_slsmn AT ROW 10.62 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number" WIDGET-ID 2
     end_slsmn AT ROW 10.62 COL 68.6 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number" WIDGET-ID 4
     tb_zer-col AT ROW 11.81 COL 32
     sl_avail AT ROW 13.81 COL 4.6 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 13.81 COL 40.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 13.81 COL 60 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 14.81 COL 40.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 15.81 COL 40.6 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 16.86 COL 40.6 WIDGET-ID 40
     btn_down AT ROW 17.86 COL 40.6 WIDGET-ID 42
     rd-dest AT ROW 20.05 COL 6 NO-LABEL
     lv-ornt AT ROW 20.52 COL 31 NO-LABEL
     lines-per-page AT ROW 20.52 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 22.43 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 23.38 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 24.62 COL 30
     tb_excel AT ROW 25.81 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 25.81 COL 71 RIGHT-ALIGNED
     fi_file AT ROW 26.62 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 28.62 COL 19
     btn-cancel AT ROW 28.62 COL 57
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 13.1 COL 5.4 WIDGET-ID 38
     "Enter 99 For Total" VIEW-AS TEXT
          SIZE 22 BY .95 AT ROW 6.14 COL 38
          FGCOLOR 9 
     "Date Ranges - Column" VIEW-AS TEXT
          SIZE 23 BY 1.19 AT ROW 2.14 COL 5
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 19.19 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 13.1 COL 60 WIDGET-ID 44
     RECT-6 AT ROW 19.57 COL 2
     RECT-7 AT ROW 1 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 29.29.


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
         TITLE              = "S/A High Sales By Date Range"
         HEIGHT             = 29.52
         WIDTH              = 96.8
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
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_date1 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       begin_date1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_date2 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       begin_date2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_date3 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       begin_date3:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_date4 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       begin_date4:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_date1 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       end_date1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_date2 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       end_date2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_date3 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       end_date3:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_date4 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       end_date4:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_print".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_print:PRIVATE-DATA IN FRAME FRAME-A     = 
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

ASSIGN 
       tb_zer-col:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* S/A High Sales By Date Range */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* S/A High Sales By Date Range */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON HELP OF begin_cust IN FRAME FRAME-A /* Beginning Cust# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Cust# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date1 C-Win
ON LEAVE OF begin_date1 IN FRAME FRAME-A /* 1 */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date2 C-Win
ON LEAVE OF begin_date2 IN FRAME FRAME-A /* 2 */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date3 C-Win
ON LEAVE OF begin_date3 IN FRAME FRAME-A /* 3 */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date4 C-Win
ON LEAVE OF begin_date4 IN FRAME FRAME-A /* 4 */
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
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust,
                    INPUT END_cust).
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
           {custom/asifax.i &begin_cust=begin_slsmn
                            &END_cust=END_slsmn
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "salesman"
                             &begin_cust= begin_slsmn
                             &END_cust=end_slsmn
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Salesman"
                                  &begin_cust= begin_slsmn
                                  &END_cust=end_slsmn
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
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


&Scoped-define SELF-NAME cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust C-Win
ON LEAVE OF cust IN FRAME FRAME-A /* Customers To Print */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON HELP OF end_cust IN FRAME FRAME-A /* Ending Cust# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date1 C-Win
ON LEAVE OF end_date1 IN FRAME FRAME-A /* Ending Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date2 C-Win
ON LEAVE OF end_date2 IN FRAME FRAME-A /* Ending Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date3 C-Win
ON LEAVE OF end_date3 IN FRAME FRAME-A /* Ending Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date4 C-Win
ON LEAVE OF end_date4 IN FRAME FRAME-A /* Ending Date */
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

    RUN WINDOWS/l-fonts.w ({&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
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


&Scoped-define SELF-NAME rd_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_print C-Win
ON VALUE-CHANGED OF rd_print IN FRAME FRAME-A
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


&Scoped-define SELF-NAME srt-col
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL srt-col C-Win
ON LEAVE OF srt-col IN FRAME FRAME-A /* Sort By Column */
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


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zer-col
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zer-col C-Win
ON VALUE-CHANGED OF tb_zer-col IN FRAME FRAME-A /* Print Customers with Zero Sales? */
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

  ASSIGN
   begin_date1 = TODAY - 27
   end_date1   = begin_date1 + 6
   begin_date2 = END_date1 + 1
   end_date2   = begin_date2 + 6 
   begin_date3 = END_date2 + 1 
   end_date3   = begin_date3 + 6 
   begin_date4 = END_date3 + 1
   end_date4   = TODAY.
  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "HR2",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    ASSIGN
     begin_date1:SCREEN-VALUE = STRING(begin_date1)
     end_date1:SCREEN-VALUE   = STRING(end_date1)
     begin_date2:SCREEN-VALUE = STRING(begin_date2)
     end_date2:SCREEN-VALUE   = STRING(end_date2) 
     begin_date3:SCREEN-VALUE = STRING(begin_date3) 
     end_date3:SCREEN-VALUE   = STRING(end_date3) 
     begin_date4:SCREEN-VALUE = STRING(begin_date4)
     end_date4:SCREEN-VALUE   = STRING(end_date4).

    APPLY "entry" TO begin_date1.
  END.

   RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'HR2',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""HR2""}

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
                            INPUT 'HR2',
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
                                  INPUT 'HR2').


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
  DISPLAY begin_date1 end_date1 begin_date2 end_date2 begin_date3 end_date3 
          begin_date4 end_date4 srt-col cust lbl_sort rd_print tb_cust-list 
          begin_cust end_cust begin_slsmn end_slsmn tb_zer-col sl_avail 
          sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date1 end_date1 begin_date2 end_date2 begin_date3 
         end_date3 begin_date4 end_date4 srt-col cust rd_print tb_cust-list 
         btnCustList begin_cust end_cust begin_slsmn end_slsmn tb_zer-col 
         sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel 
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
/*     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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
/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-topw.f}*/

def var fdate     as   date format "99/99/9999" extent 4 NO-UNDO.
def var tdate     like fdate NO-UNDO.
def var fsman     like cust.sman            init "" NO-UNDO.
def var tsman     like fsman                init "zzz" NO-UNDO.
def var fcus     like cust.cust-no          init "" NO-UNDO.
def var tcus     like cust.cust-no          init "zzzzzzzz" NO-UNDO.
def var v-col     as   int                  format ">9" NO-UNDO.
def var v-custs   as   int                  format ">>,>>9" NO-UNDO.
def var v-sort    as   log      init yes    format "Customer/Salesrep" NO-UNDO.
def var v-inc     as   log      init YES NO-UNDO.

def var v-amt1    as   DEC NO-UNDO.
def var v-slsm    like ar-invl.sman  extent 1 NO-UNDO.
def var v-slsp    like ar-invl.s-pct extent 1 NO-UNDO.

def var v-amt     as   dec  extent 5 NO-UNDO.
def var v-tot-amt as   dec  extent 5 NO-UNDO.
def var v-label   as   char extent 4  format "x(17)" NO-UNDO.

def var v-prt     as   INT NO-UNDO.

DEF VAR li AS INT NO-UNDO.
DEF VAR lv-start AS INT NO-UNDO.
DEF VAR lo-date AS DATE INIT 12/31/9999 NO-UNDO.
DEF VAR hi-date AS DATE INIT 01/01/0001 NO-UNDO.
DEF VAR lv-sman AS CHAR NO-UNDO.

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

{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.

FORM HEADER
     "Salesrep:"
     lv-sman        FORMAT "x(40)"

    WITH PAGE-TOP FRAME r-top2 STREAM-IO WIDTH 180.

form cust.cust-no
     cust.name
     v-amt[1]                                      format "->,>>>,>>>,>>9.99"
     v-amt[2]                                      format "->,>>>,>>>,>>9.99"
     v-amt[3]                                      format "->,>>>,>>>,>>9.99"
     v-amt[4]                                      format "->,>>>,>>>,>>9.99"
     v-amt[5]                                      format "->,>>>,>>>,>>9.99"

    header skip(1)
           "Customer Name                          "
           v-label[1]
           v-label[2]
           v-label[3]
           v-label[4]
           "        Total Amt"                                          skip

    with no-box no-labels frame custx down STREAM-IO width 180.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 fdate[1]   = begin_date1
 tdate[1]   = end_date1
 fdate[2]   = begin_date2
 tdate[2]   = end_date2
 fdate[3]   = begin_date3
 tdate[3]   = end_date3
 fdate[4]   = begin_date4
 tdate[4]   = end_date4
 fsman      = begin_slsmn
 tsman      = end_slsmn
 v-col      = srt-col
 v-custs    = cust
 v-sort     = rd_print EQ "Customer"
 v-inc      = tb_zer-col
 lSelected  = tb_cust-list
 fcus       = begin_cust
 tcus       = END_cust. 

do li = 1 to 4:
  v-label[li] = string(fdate[li],"99/99/99") + "-" + string(tdate[li],"99/99/99").
  IF fdate[li] LT lo-date THEN lo-date = fdate[li].
  IF tdate[li] GT hi-date THEN hi-date = tdate[li].
end.

FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

    IF LOOKUP(ttRptSelected.TextList, "Date Range 1,Date Range 2,Date Range 3,Date Range 4,Total Amt") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 

    IF ttRptSelected.TextList = "Date Range 1" THEN
        ASSIGN 
         ttRptSelected.TextList = v-label[1] .
    IF ttRptSelected.TextList = "Date Range 2" THEN
        ASSIGN 
         ttRptSelected.TextList = v-label[2] .
    IF ttRptSelected.TextList = "Date Range 3" THEN
        ASSIGN 
         ttRptSelected.TextList = v-label[3] .
    IF ttRptSelected.TextList = "Date Range 4" THEN
        ASSIGN 
         ttRptSelected.TextList = v-label[4] .
END.

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

       /* IF LOOKUP(ttRptSelected.TextList, "Date Range 1,Date Range 2,Date Range 3,Date Range 4,Total Amt") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . */
 END.


IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.
IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN fcus = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN tcus = ttCustList.cust-no .
END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

EMPTY TEMP-TABLE tt-report.
EMPTY TEMP-TABLE tt-report2.

SESSION:SET-WAIT-STATE ("general").
      FOR EACH ar-inv
          WHERE ar-inv.company  EQ cocode
            AND ar-inv.cust-no  GE fcus
            AND ar-inv.cust-no  LE tcus
            AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-inv.cust-no
            AND ttCustList.log-fld no-lock) else true)
            AND ar-inv.inv-date GE lo-date
            AND ar-inv.inv-date LE hi-date
            AND ar-inv.posted   EQ YES
          USE-INDEX inv-date NO-LOCK,

          FIRST cust
          WHERE cust.company EQ ar-inv.company
            AND cust.cust-no EQ ar-inv.cust-no
          NO-LOCK:

        for each ar-invl
            where ar-invl.x-no    eq ar-inv.x-no
              and (ar-invl.billable or not ar-invl.misc)
            no-lock:
            {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
          do i = 1 to 3:
            assign
             v-amt     = 0
             v-slsm[1] = if ar-invl.sman[i] eq "" and i eq 1 then
                           cust.sman else ar-invl.sman[i].

            if v-slsm[1]   lt fsman                         or
               v-slsm[1]   gt tsman                         or
               (i ne 1 and
                (v-slsm[1] eq "" or ar-invl.s-pct[i] eq 0)) then next.

            assign
             v-slsp[1] = if ar-invl.sman[i] eq ""              or
                            (ar-invl.s-pct[i] eq 0 and i eq 1) then 100
                         else ar-invl.s-pct[i]
             v-amt1    = ar-invl.amt * v-slsp[1] / 100.

            if v-col ne 99                     and
               ar-inv.inv-date ge fdate[v-col] and
               ar-inv.inv-date le tdate[v-col] then
              v-amt[1] = v-amt[1] + v-amt1.

            v-amt[2] = v-amt[2] + v-amt1.

            create tt-report.
            assign
             tt-report.term-id = ""
             tt-report.key-01  = if v-sort then "" else v-slsm[1]
             tt-report.key-02  = cust.cust-no
             tt-report.key-03  = string(v-amt[1],"-9999999999999999999.99")
             tt-report.key-04  = string(v-amt[2],"-9999999999999999999.99")
             tt-report.key-10  = "ar-invl"
             tt-report.rec-id  = recid(ar-invl)
             tt-report.key-06   = v-slsm[1] .
          end.
        end.
      END.

     FOR each cust where cust.company eq cocode
          AND cust.cust-no GE fcus
          AND cust.cust-no LE tcus
          AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
          AND ttCustList.log-fld no-lock) else true) no-lock,

          each ar-cash
          where ar-cash.company    eq cocode
            and ar-cash.cust-no    eq cust.cust-no
            and ar-cash.check-date ge lo-date
            and ar-cash.check-date le hi-date
            and ar-cash.posted     eq yes
          use-index ar-cash no-lock,

          EACH ar-cashl
          WHERE ar-cashl.c-no    EQ ar-cash.c-no
            AND ar-cashl.posted  EQ YES
            AND ar-cashl.memo    EQ YES
            AND CAN-FIND(FIRST account
                         WHERE account.company EQ ar-cashl.company
                           AND account.actnum  EQ ar-cashl.actnum
                           AND account.type    EQ "R")
          NO-LOCK:
         {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
        release ar-invl.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

        if avail oe-retl then
        find first ar-invl
            where ar-invl.company eq cocode
              and ar-invl.cust-no eq cust.cust-no
              and ar-invl.inv-no  eq ar-cashl.inv-no
              and ar-invl.i-no    eq oe-retl.i-no
              and (ar-invl.billable or not ar-invl.misc)
            no-lock no-error.

        do i = 1 to 3:
          assign
           v-amt     = 0
           v-slsm[1] = if (not avail ar-invl)                or
                          (ar-invl.sman[i] eq "" and i eq 1) then
                         cust.sman else ar-invl.sman[i].

          if v-slsm[1]   lt fsman                         or
             v-slsm[1]   gt tsman                         or
             (i ne 1 and
              (v-slsm[1] eq "" or ar-invl.s-pct[i] eq 0)) then next.

          assign
           v-slsp[1] = if (not avail ar-invl)                or
                          ar-invl.sman[i] eq ""              or
                          (ar-invl.s-pct[i] eq 0 and i eq 1) then 100
                       else ar-invl.s-pct[i]
           v-amt1    = (ar-cashl.amt-paid - ar-cashl.amt-disc) *
                       v-slsp[1] / 100.

          if v-col ne 99                        and
             ar-cash.check-date ge fdate[v-col] and
             ar-cash.check-date le tdate[v-col] then
            v-amt[1] = v-amt[1] + v-amt1.

          v-amt[2] = v-amt[2] + v-amt1.

          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = if v-sort then "" else v-slsm[1]
           tt-report.key-02  = cust.cust-no
           tt-report.key-03  = string(v-amt[1],"-9999999999999999999.99")
           tt-report.key-04  = string(v-amt[2],"-9999999999999999999.99")
           tt-report.key-10  = "ar-cashl"
           tt-report.rec-id  = recid(ar-cashl)
           tt-report.key-06   = v-slsm[1] .

          if not avail ar-invl then leave.
        end.
      end.

    v-amt = 0.

    for each tt-report where tt-report.term-id eq "" no-lock

        break by tt-report.key-01
              by tt-report.key-02:

      assign
       v-amt[1] = v-amt[1] + dec(tt-report.key-03)
       v-amt[2] = v-amt[2] + dec(tt-report.key-04).

      if last-of(tt-report.key-02) then do:
        if v-amt[1] ne 0 or v-inc or v-col eq 99 then do:
          create tt-report2.
          assign
           tt-report2.term-id = ""
           tt-report2.key-01  = tt-report.key-01
           tt-report2.key-02  = if v-col eq 99 then ""
                                else string(v-amt[1],"-9999999999999999999.99")
           tt-report2.key-03  = string(v-amt[2],"-9999999999999999999.99")
           tt-report2.key-04  = tt-report.key-02
           tt-report2.dec1    = if v-col eq 99 then 0 else v-amt[1]
           tt-report2.dec2    = v-amt[2]
           tt-report2.key-06   = tt-report.key-06 .
        end.

        v-amt = 0.
      end.
    end.

    for each tt-report2 where tt-report2.term-id eq "",

        first cust
        where cust.company eq cocode
          and cust.cust-no eq tt-report2.key-04
        no-lock

        break by tt-report2.key-01
              by tt-report2.dec1 desc
              by tt-report2.dec2 DESC

        with frame custx:
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
      if first-of(tt-report2.key-01) then do:
        v-prt = 0.

        IF FIRST(tt-report2.key-01) THEN VIEW FRAME r-top.

        if not v-sort then do:
          find first sman
              where sman.company eq cocode
                and sman.sman    eq tt-report2.key-01
              no-lock no-error.
          lv-sman = TRIM(tt-report2.key-01) + " " +
                    TRIM(IF AVAIL sman THEN sman.sname ELSE "Not on file").

          IF FIRST(tt-report2.key-01) THEN /*VIEW FRAME r-top2.*/
          PAGE.
        END.
        ELSE
        IF FIRST(tt-report2.key-01) THEN PAGE.
      end.

      assign
       v-amt = 0
       v-prt = v-prt + 1.

      for each tt-report
          where tt-report.term-id eq ""
            and tt-report.key-01  eq tt-report2.key-01
            and tt-report.key-02  eq tt-report2.key-04:

        if tt-report.key-10 eq "ar-invl" then do:
          find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock.
          find ar-inv  where ar-inv.x-no    eq ar-invl.x-no  no-lock.

          do i = 1 to 4:
            if ar-inv.inv-date ge fdate[i] and
               ar-inv.inv-date le tdate[i] then
              v-amt[i] = v-amt[i] + dec(tt-report.key-04).
          end.

          v-amt[5] = v-amt[5] + dec(tt-report.key-04).
        end.

        else
        if tt-report.key-10 eq "ar-cashl" then do:
          find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
          find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

          do i = 1 to 4:
            if ar-cash.check-date ge fdate[i] and
               ar-cash.check-date le tdate[i] then
              v-amt[i] = v-amt[i] + dec(tt-report.key-04).
          end.

          v-amt[5] = v-amt[5] + dec(tt-report.key-04).
        end.
      end.

      if v-prt le v-custs then do:

          find first sman
              where sman.company eq cocode
                and sman.sman    eq tt-report2.key-06
              no-lock no-error.
       /* display cust.cust-no
                cust.name
                v-amt[1]
                v-amt[2]
                v-amt[3]
                v-amt[4]
                v-amt[5].
        down.*/
           ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = string(cust.cust-no,"x(8)") .
                         WHEN "name"    THEN cVarValue = string(cust.name,"x(30)").
                         WHEN "rep"     THEN cVarValue =  string(tt-report2.key-06,"x(3)") .
                         WHEN "rep-name"    THEN cVarValue = IF AVAIL  sman THEN string(sman.sname,"x(20)") ELSE "Not on file" .
                         WHEN "date1"   THEN cVarValue = STRING(v-amt[1],"->,>>>,>>>,>>9.99").
                         WHEN "date2"   THEN cVarValue = STRING(v-amt[2],"->,>>>,>>>,>>9.99") .
                         WHEN "date3"   THEN cVarValue = STRING(v-amt[3],"->,>>>,>>>,>>9.99") .
                         WHEN "date4"   THEN cVarValue = STRING(v-amt[4],"->,>>>,>>>,>>9.99") .
                         WHEN "tot"     THEN cVarValue = STRING(v-amt[5],"->,>>>,>>>,>>9.99") .

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

        /*IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               '"' lv-sman                              '",'
               '"' cust.cust-no                         '",'
               '"' cust.name                            '",'
               '"' STRING(v-amt[1],"->,>>>,>>>,>>9.99") '",'
               '"' STRING(v-amt[2],"->,>>>,>>>,>>9.99") '",'
               '"' STRING(v-amt[3],"->,>>>,>>>,>>9.99") '",'
               '"' STRING(v-amt[4],"->,>>>,>>>,>>9.99") '",'
               '"' STRING(v-amt[5],"->,>>>,>>>,>>9.99") '",'
               SKIP.*/
      end.

      do i = 1 to 4:
        v-tot-amt[i] = v-tot-amt[i] + v-amt[i].
      end.

      if last-of(tt-report2.key-01) or v-prt eq v-custs then do:
       /* underline cust.name.
        if v-label[1] ne "" then underline v-amt[1].
        if v-label[2] ne "" then underline v-amt[2].
        if v-label[3] ne "" then underline v-amt[3].
        if v-label[4] ne "" then underline v-amt[4].
        underline v-amt[5].
        down.*/

        do i = 1 to 4:
          v-tot-amt[5] = v-tot-amt[5] + v-tot-amt[i].
        end.

        /*display "Totals"                           @ cust.name
                v-tot-amt[1] when v-label[1] ne "" @ v-amt[1]
                v-tot-amt[2] when v-label[2] ne "" @ v-amt[2]
                v-tot-amt[3] when v-label[3] ne "" @ v-amt[3]
                v-tot-amt[4] when v-label[4] ne "" @ v-amt[4]
                v-tot-amt[5]                       @ v-amt[5].
        down.*/

        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = "" .
                         WHEN "name"    THEN cVarValue = "".
                         WHEN "rep"     THEN cVarValue =  "".
                         WHEN "rep-name"    THEN cVarValue = "" .
                         WHEN "date1"   THEN cVarValue = STRING(v-tot-amt[1],"->,>>>,>>>,>>9.99").
                         WHEN "date2"   THEN cVarValue = STRING(v-tot-amt[2],"->,>>>,>>>,>>9.99") .
                         WHEN "date3"   THEN cVarValue = STRING(v-tot-amt[3],"->,>>>,>>>,>>9.99") .
                         WHEN "date4"   THEN cVarValue = STRING(v-tot-amt[4],"->,>>>,>>>,>>9.99") .
                         WHEN "tot"     THEN cVarValue = STRING(v-tot-amt[5],"->,>>>,>>>,>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
           PUT str-line SKIP .
            PUT UNFORMATTED "           Totals" substring(cDisplay,18,300) SKIP(1).
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED ' Totals ,'  
                      SUBSTRING(cExcelDisplay,4,300) SKIP(1).
             END.

        /*IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               SKIP(1)
               '"' lv-sman   '",'
               '"' ""        '",'
               '"' ""        '",'
               '"' ""        '",'
               '"' ""        '",'
               '"' ""        '",'
               '"' ""        '",'
               '"' ""        '",'
               '"' IF v-label[1] ne "" THEN 
                      STRING(v-tot-amt[1],"->,>>>,>>>,>>9.99")
                   ELSE "" '",'
               '"' IF v-label[2] ne "" THEN 
                      STRING(v-tot-amt[2],"->,>>>,>>>,>>9.99")
                   ELSE "" '",'
               '"' IF v-label[3] ne "" THEN 
                      STRING(v-tot-amt[3],"->,>>>,>>>,>>9.99")
                   ELSE "" '",'
               '"' IF v-label[4] ne "" THEN 
                      STRING(v-tot-amt[4],"->,>>>,>>>,>>9.99")
                   ELSE "" '",'
               '"' STRING(v-tot-amt[5],"->,>>>,>>>,>>9.99") '",'
               SKIP(1).*/

        if last-of(tt-report2.key-01) then v-tot-amt = 0.
      end.
    end.

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

