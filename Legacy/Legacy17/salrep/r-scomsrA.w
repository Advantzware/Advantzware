&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-scomsr.w

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
 locode = gloc  .

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
def var fdate     as   date format "99/99/9999" extent 4 NO-UNDO.
def var tdate     like fdate NO-UNDO.
def var fsman     like cust.sman            init "" NO-UNDO.
def var tsman     like fsman                init "zzz" NO-UNDO.

def var v-custs   as   int                  format ">>,>>9" NO-UNDO.
def var v-sort    as   log      init yes    format "Customer/Salesrep" NO-UNDO.
def var v-inc     as   log      init YES NO-UNDO.

def var v-amt1    as   DEC NO-UNDO.
def var v-slsm    like ar-invl.sman  extent 1 NO-UNDO.
def var v-slsp    like ar-invl.s-pct extent 1 NO-UNDO.

def var v-amt     as   dec  extent 5 NO-UNDO.
def var v-tot-amt as   dec  extent 5 NO-UNDO.
def var v-get-mth as   INT  extent 10 NO-UNDO.
def var v-get-year as   INT /*FORMAT 9999*/  NO-UNDO.
def var v-label   as   char extent 4  format "x(17)" NO-UNDO.
def var v-head as char format "x(300)" extent 2 NO-UNDO.
def var v-prt     as   INT NO-UNDO.

/*DEF VAR li AS INT NO-UNDO. */
DEF VAR lv-start AS INT NO-UNDO.
DEF VAR lo-date AS DATE INIT 12/31/9999 NO-UNDO.
DEF VAR hi-date AS DATE INIT 01/01/0001 NO-UNDO.
def var fcust as ch init "" NO-UNDO.
def var tcust like fcust init "zzzzzzzz" NO-UNDO.
DEF VAR lv-sman AS CHAR NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
/*DEF VAR custcount AS CHAR NO-UNDO.*/
DEF VAR salecount AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO 
    FIELD cust         AS CHAR
    FIELD cust-name    AS CHAR
    FIELD sman         AS CHAR
    FIELD curr-year    AS DECIMAL
    FIELD Sales1       AS DECIMAL
    FIELD Sales2       AS DECIMAL
    FIELD Sales3       AS DECIMAL
    FIELD Sales4       AS DECIMAL
    FIELD Sales5       AS DECIMAL
    FIELD Delta1       AS DECIMAL
    FIELD Delta2       AS DECIMAL
    FIELD Delta3       AS DECIMAL
    FIELD Delta4       AS DECIMAL
    FIELD Delta5       AS DECIMAL
    FIELD per1         AS DECIMAL
    FIELD per2         AS DECIMAL
    FIELD per3         AS DECIMAL
    FIELD per4         AS DECIMAL .

DEF TEMP-TABLE tt-budget NO-UNDO 
    FIELD cust         AS CHAR
    FIELD sman         AS CHAR
    FIELD vyear    AS DATE
    FIELD budget       AS DECIMAL .


DEF TEMP-TABLE tt-report2 NO-UNDO LIKE tt-report.

DEF STREAM excel.

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
begin_date2 end_date2 begin_date3 end_date3 begin_date4 end_date4 ~
fi_budgetyear tb_col rd_print tb_cust-list btnCustList begin_cust-no ~
end_cust-no begin_slsmn end_slsmn tb_zer-col rd-dest lv-ornt lines-per-page ~
lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date1 end_date1 begin_date2 ~
end_date2 begin_date3 end_date3 begin_date4 end_date4 fi_budgetyear tb_col ~
lbl_sort rd_print tb_cust-list begin_cust-no end_cust-no begin_slsmn ~
end_slsmn tb_zer-col rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 begin_date1 end_date1 begin_date2 end_date2 ~
begin_date3 end_date3 begin_date4 end_date4 

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

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
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

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
     SIZE 16.4 BY 1.

DEFINE VARIABLE fi_budgetyear AS INTEGER FORMAT ">>>>>" INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-scomsr.csv" 
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
     SIZE 92 BY 9.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 13.1.

DEFINE VARIABLE tb_col AS LOGICAL INITIAL no 
     LABEL "Print Budget Year ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

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

DEFINE VARIABLE tb_zer-col AS LOGICAL INITIAL yes 
     LABEL "Page Break by Sales Rep ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date1 AT ROW 2.43 COL 30 COLON-ALIGNED
     end_date1 AT ROW 2.43 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_date2 AT ROW 3.38 COL 30 COLON-ALIGNED
     end_date2 AT ROW 3.38 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_date3 AT ROW 4.33 COL 30 COLON-ALIGNED
     end_date3 AT ROW 4.33 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_date4 AT ROW 5.29 COL 30 COLON-ALIGNED
     end_date4 AT ROW 5.29 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     fi_budgetyear AT ROW 6.62 COL 57.2 COLON-ALIGNED HELP
          "Customers To Print" NO-LABEL
     tb_col AT ROW 6.71 COL 32 WIDGET-ID 6
     lbl_sort AT ROW 8 COL 20 COLON-ALIGNED NO-LABEL
     rd_print AT ROW 8 COL 32 NO-LABEL
     tb_cust-list AT ROW 9.43 COL 32 WIDGET-ID 6
     btnCustList AT ROW 9.48 COL 64 WIDGET-ID 8
     begin_cust-no AT ROW 10.67 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 10.67 COL 71 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_slsmn AT ROW 11.67 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number" WIDGET-ID 2
     end_slsmn AT ROW 11.67 COL 71 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number" WIDGET-ID 4
     tb_zer-col AT ROW 13 COL 32.2
     rd-dest AT ROW 15.29 COL 6 NO-LABEL
     lv-ornt AT ROW 15.76 COL 31 NO-LABEL
     lines-per-page AT ROW 15.76 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 17.67 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 18.62 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.86 COL 30
     tb_excel AT ROW 21.05 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 21.05 COL 71 RIGHT-ALIGNED
     fi_file AT ROW 21.86 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 23.76 COL 19
     btn-cancel AT ROW 23.76 COL 57
     "Date Ranges - Column" VIEW-AS TEXT
          SIZE 23 BY 1.19 AT ROW 2.43 COL 5
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.57 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 14.1 COL 2
     RECT-7 AT ROW 1 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 24.48.


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
         TITLE              = "Sales Comparison/Sales Rep"
         HEIGHT             = 24.81
         WIDTH              = 96.6
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       fi_budgetyear:PRIVATE-DATA IN FRAME FRAME-A     = 
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

ASSIGN 
       tb_col:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Sales Comparison/Sales Rep */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sales Comparison/Sales Rep */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

  SESSION:SET-WAIT-STATE("general").
  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust-no,
                    INPUT end_cust-no).
  END.
  run run-report. 

  STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE(""). 

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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
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


&Scoped-define SELF-NAME fi_budgetyear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_budgetyear C-Win
ON LEAVE OF fi_budgetyear IN FRAME FRAME-A
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
  IF rd_print = "Customer" THEN do: 
      tb_zer-col:SCREEN-VALUE = "No" .
      tb_zer-col:SENSITIVE = NO.
  END.
  ELSE
      ASSIGN
          tb_zer-col:SCREEN-VALUE = "Yes"
          tb_zer-col:SENSITIVE = YES .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_col
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_col C-Win
ON VALUE-CHANGED OF tb_col IN FRAME FRAME-A /* Print Budget Year ? */
DO:
  assign {&self-name}.
  IF tb_col = YES THEN
      fi_budgetyear:SENSITIVE =YES .
  ELSE do:
      ASSIGN
          fi_budgetyear:SCREEN-VALUE = ""
          fi_budgetyear:SENSITIVE = NO .
  END.
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
ON VALUE-CHANGED OF tb_zer-col IN FRAME FRAME-A /* Page Break by Sales Rep ? */
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
/*  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.*/

  ASSIGN
   begin_date1 = TODAY - 27
   end_date1   = begin_date1 + 6
   begin_date2 = END_date1 + 1
   end_date2   = begin_date2 + 6 
   begin_date3 = END_date2 + 1 
   end_date3   = begin_date3 + 6 
   begin_date4 = END_date3 + 1
   end_date4   = TODAY.

  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "HR16",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
  IF begin_date1:SCREEN-VALUE = "" THEN 
      ASSIGN
          begin_date1:SCREEN-VALUE = STRING(begin_date1)
          end_date1:SCREEN-VALUE   = STRING(end_date1)
          begin_date2:SCREEN-VALUE = STRING(begin_date2)
          end_date2:SCREEN-VALUE   = STRING(end_date2) 
          begin_date3:SCREEN-VALUE = STRING(begin_date3) 
          end_date3:SCREEN-VALUE   = STRING(end_date3) 
          begin_date4:SCREEN-VALUE = STRING(begin_date4)
          end_date4:SCREEN-VALUE   = STRING(end_date4).


    IF tb_col:SCREEN-VALUE = "YES" THEN
        fi_budgetyear:SENSITIVE =YES .
    ELSE do:
        ASSIGN
            fi_budgetyear:SCREEN-VALUE = ""
            fi_budgetyear:SENSITIVE = NO .
    END.
    IF rd_print:SCREEN-VALUE = "Customer" THEN do: 
      tb_zer-col:SCREEN-VALUE = "No" .
      tb_zer-col:SENSITIVE = NO.
  END.
  ELSE
      ASSIGN
          tb_zer-col:SCREEN-VALUE = "Yes" 
          tb_zer-col:SENSITIVE = YES .

    APPLY "entry" TO begin_cust-no.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'HR16',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""HR16""}

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

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
                            INPUT 'HR16',
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
                                  INPUT 'HR16').


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
          begin_date4 end_date4 fi_budgetyear tb_col lbl_sort rd_print 
          tb_cust-list begin_cust-no end_cust-no begin_slsmn end_slsmn 
          tb_zer-col rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date1 end_date1 begin_date2 end_date2 begin_date3 
         end_date3 begin_date4 end_date4 fi_budgetyear tb_col rd_print 
         tb_cust-list btnCustList begin_cust-no end_cust-no begin_slsmn 
         end_slsmn tb_zer-col rd-dest lv-ornt lines-per-page lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nosort C-Win 
PROCEDURE nosort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
           DEF VAR sal-tot AS DECIMAL EXTENT 5 NO-UNDO.
           DEF VAR sal-del AS DECIMAL EXTENT 5 NO-UNDO.
           DEF VAR sal-per AS DECIMAL EXTENT 5 NO-UNDO.
           {sys/form/r-topw.f}

           FORM HEADER
                "Salesrep:"
                lv-sman        FORMAT "x(40)"
               WITH PAGE-TOP FRAME r-top2 STREAM-IO WIDTH 180.

          FOR EACH tt-report2 NO-LOCK BREAK BY tt-report2.sman:
              format header
                     skip(1)
                     "                         " 
                     "          " 
                     "                  " 
                     "                  "
                     "                  "
                     "%-OF Sales        " 
                     "                  "  
                     "                  " 
                     "%-OF Sales        " 
                     "                  " 
                     "                  "
                     "%-OF Sales        " 
                     "                  " 
                     "                  "  
                     "%-OF Sales        " 
                     SKIP
                     "Name/City/State          " 
                     "Sales Rep " 
                     "Budget Year" string(v-get-year,"9999")
                     "Sales  "  string(v-label[1]) 
                     "Delta             " 
                     "Difference        " 
                     "Sales "   string(v-label[2]) "  " 
                     "Delta             " 
                     "Difference        " 
                     "Sales "   string(v-label[3]) "  "
                     "Delta             "  
                     "Difference        " 
                     "Sales "  string(v-label[4]) "  "
                     "Delta             "  
                     "Difference        " 
                     SKIP 
                     "-------------------------"
                     "----------"
                     "------------------"
                     "------------------"
                     "------------------"
                     "------------------"
                     "------------------"
                     "------------------"
                     "------------------"
                     "------------------"
                     "------------------"
                     "------------------"
                     "------------------"
                     "------------------"
                     "------------------"

                  with frame r-top-2 stream-io width 380 no-labels
                       no-box no-underline page-top.

         IF FIRST-OF(tt-report2.sman) THEN do:
             ASSIGN
                 sal-tot = 0
                 sal-del = 0 .
             find first sman
              where sman.company eq cocode
                and sman.sman    eq tt-report2.sman
              no-lock no-error.
         IF AVAIL sman THEN
             lv-sman = string(sman.sman) + "  " + sman.sname .
         IF AVAIL sman AND tb_excel THEN
              PUT STREAM excel UNFORMATTED
              SKIP
               '"' string(sman.sman) + "    " + sman.sname                                  '",'
              SKIP.
         END.
         IF FIRST(tt-report2.sman) THEN VIEW FRAME r-top.
         IF FIRST-OF(tt-report2.sman) THEN do:
             IF v-inc THEN
             VIEW FRAME r-top2. 
             VIEW FRAME r-top-2.

         END.
         IF v-inc THEN do:
             IF FIRST-OF(tt-report2.sman) THEN PAGE.
         END.

          PUT tt-report2.cust-name          FORMAT "x(25)" SPACE(1)
                  tt-report2.sman      FORMAT "x(10)" SPACE(1)
                  tt-report2.curr-year FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.Sales1    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.Delta1    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.per1      FORMAT "->,>>>,>>>,>>9.99%" SPACE(1)
                  tt-report2.Sales2    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.Delta2    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.per2      FORMAT "->,>>>,>>>,>>9.99%" SPACE(1)
                  tt-report2.Sales3    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  tt-report2.Delta3    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  tt-report2.per3      FORMAT "->,>>>,>>>,>>9.99%" SPACE(1) 
                  tt-report2.Sales4    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  tt-report2.Delta4    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  tt-report2.per4      FORMAT "->,>>>,>>>,>>9.99%" SKIP  .



          IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               '"' tt-report2.cust-name                                  '",'
               '"' tt-report2.sman                                  '",'
               '"' "$ " + STRING(tt-report2.curr-year,"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(tt-report2.Sales1,"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(tt-report2.Delta1,"->,>>>,>>>,>>9.99")   '",'
               '"' IF tt-report2.per1 = 0 THEN "" ELSE STRING(tt-report2.per1,"->,>>>,>>>,>>9.99") + "%"   '",'
               '"' "$ " + STRING(tt-report2.Sales2,"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(tt-report2.Delta2,"->,>>>,>>>,>>9.99")   '",'
               '"' IF tt-report2.per2 = 0 THEN "" ELSE STRING(tt-report2.per2,"->,>>>,>>>,>>9.99") + "%"    '",'
               '"' "$ " + STRING(tt-report2.Sales3,"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(tt-report2.Delta3,"->,>>>,>>>,>>9.99")   '",'
               '"' IF tt-report2.per3 = 0 THEN "" ELSE STRING(tt-report2.per3,"->,>>>,>>>,>>9.99") + "%"   '",'
               '"' "$ " + STRING(tt-report2.Sales4,"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(tt-report2.Delta4,"->,>>>,>>>,>>9.99")   '",'
               '"' IF tt-report2.per4 = 0 THEN "" ELSE STRING(tt-report2.per4,"->,>>>,>>>,>>9.99") + "%"   '",'
               SKIP.

               ASSIGN
                   sal-tot[1] = sal-tot[1] + tt-report2.curr-year 
                   sal-tot[2] = sal-tot[2] + tt-report2.Sales1 
                   sal-tot[3] = sal-tot[3] + tt-report2.Sales2 
                   sal-tot[4] = sal-tot[4] + tt-report2.Sales3 
                   sal-tot[5] = sal-tot[5] + tt-report2.Sales4 
                   sal-del[1] = sal-del[1] + tt-report2.Delta1
                   sal-del[2] = sal-del[2] + tt-report2.Delta2
                   sal-del[3] = sal-del[3] + tt-report2.Delta3
                   sal-del[4] = sal-del[4] + tt-report2.Delta4 .

          IF LAST-OF(tt-report2.sman) THEN do:

              ASSIGN
                  sal-per[1] = sal-del[1] / sal-tot[2]   * 100
                  sal-per[2] = sal-del[2] / sal-tot[3]   * 100
                  sal-per[3] = sal-del[3] / sal-tot[4]   * 100
                  sal-per[4] = sal-del[4] / sal-tot[5]   * 100.

              PUT SKIP 
                  "                         "
                  "            "
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SKIP  .

              PUT 
                  SPACE(23) "TOTAL Sales: "  SPACE(1)
                  sal-tot[1] FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  sal-tot[2]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  sal-del[1]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  sal-per[1]      FORMAT "->,>>>,>>>,>>9.99%" SPACE(1)
                  sal-tot[3]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  sal-del[2]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  sal-per[2]     FORMAT "->,>>>,>>>,>>9.99%" SPACE(1)
                  sal-tot[4]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  sal-del[3]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  sal-per[3]     FORMAT "->,>>>,>>>,>>9.99%" SPACE(1) 
                  sal-tot[5]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  sal-del[4]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  sal-per[4]      FORMAT "->,>>>,>>>,>>9.99%" SKIP(1)  .

            IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
               '"' "TOTAL Sales "     + tt-report2.sman            '",'
               '"'                                                '",'
               '"' "$ " + STRING(sal-tot[1],"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(sal-tot[2],"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(sal-del[1],"->,>>>,>>>,>>9.99")   '",'
               '"' IF sal-per[1] = ? THEN "" ELSE STRING(sal-per[1],"->,>>>,>>>,>>9.99") + "%"   '",'
               '"' "$ " + STRING(sal-tot[3],"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(sal-del[2],"->,>>>,>>>,>>9.99")   '",'
               '"' IF sal-per[2] = ? THEN "" ELSE STRING(sal-per[2],"->,>>>,>>>,>>9.99") + "%"    '",'
               '"' "$ " + STRING(sal-tot[4],"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(sal-del[3],"->,>>>,>>>,>>9.99")   '",'
               '"' IF sal-per[3] = ? THEN "" ELSE STRING(sal-per[3],"->,>>>,>>>,>>9.99") + "%"   '",'
               '"' "$ " + STRING(sal-tot[5],"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(sal-del[4],"->,>>>,>>>,>>9.99")   '",'
               '"' IF sal-per[4] = ? THEN "" ELSE STRING(sal-per[4],"->,>>>,>>>,>>9.99") + "%"   '",'

              SKIP(1).
          END.
      end.


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
DEF VAR v-tot AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR v-per-tot AS DEC  NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.
{sys/form/r-topw.f}

    {custom/statusMsg.i "'Processing...'"} 

FORM HEADER
     "Salesrep:"
     lv-sman        FORMAT "x(40)"
    WITH PAGE-TOP FRAME r-top2 STREAM-IO WIDTH 180.

FORMAT tt-report2.cust     FORMAT "X(30)"
     tt-report2.sman     FORMAT "X(20)"
     tt-report2.curr-year   format "->,>>>,>>>,>>9.99"
     tt-report2.Sales1      format "->,>>>,>>>,>>9.99"
     tt-report2.Delta1      format "->,>>>,>>>,>>9.99"
     tt-report2.per1        format "->,>>>,>>>,>>9.99"
     tt-report2.Sales2      format "->,>>>,>>>,>>9.99"
     tt-report2.Delta2      format "->,>>>,>>>,>>9.99"
     tt-report2.per2        format "->,>>>,>>>,>>9.99"
     tt-report2.Sales3      format "->,>>>,>>>,>>9.99"
     tt-report2.Delta3      format "->,>>>,>>>,>>9.99"
     tt-report2.per3        format "->,>>>,>>>,>>9.99"
     tt-report2.Sales4      format "->,>>>,>>>,>>9.99"
     tt-report2.Delta4      format "->,>>>,>>>,>>9.99"
     tt-report2.per4        format "->,>>>,>>>,>>9.99"

     with frame custx down no-labels no-box stream-io width 380.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 fcust = begin_cust-no
 tcust = end_cust-no
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

 v-custs    = 0
 v-sort     = rd_print EQ "Customer"
 v-inc      = tb_zer-col
 lSelected  = tb_cust-list. 
IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
END.


DO WITH FRAME {&frame-name}:
 ASSIGN
  v-label[1] = SUBSTRING(begin_date1:SCREEN-VALUE,7,10) 
  v-label[2] = SUBSTRING(begin_date2:SCREEN-VALUE,7,10) 
  v-label[3] = SUBSTRING(begin_date3:SCREEN-VALUE,7,10) 
  v-label[4] = SUBSTRING(begin_date4:SCREEN-VALUE,7,10) 
  v-get-mth[1] = int(SUBSTRING(begin_date1:SCREEN-VALUE,1,2))
  v-get-mth[2] = int(SUBSTRING(begin_date2:SCREEN-VALUE,1,2))  
  v-get-mth[3] = int(SUBSTRING(begin_date3:SCREEN-VALUE,1,2))  
  v-get-mth[4] = int(SUBSTRING(begin_date4:SCREEN-VALUE,1,2))  
  v-get-mth[5] = int(SUBSTRING(end_date1:SCREEN-VALUE,1,2))
  v-get-mth[6] = int(SUBSTRING(end_date2:SCREEN-VALUE,1,2))  
  v-get-mth[7] = int(SUBSTRING(end_date3:SCREEN-VALUE,1,2))  
  v-get-mth[8] = int(SUBSTRING(end_date4:SCREEN-VALUE,1,2)) 
     .               

ASSIGN hi-date = TODAY .
        IF tb_col THEN
        lo-date = DATE("01/01/" + STRING(fi_budgetyear)).
do li = 1 to 4:
  IF fdate[li] LT lo-date THEN lo-date = fdate[li].
  IF tdate[li] GT hi-date THEN hi-date = tdate[li].
end.

END.
IF tb_col THEN do:
ASSIGN
    v-get-year = fi_budgetyear  
    v-get-mth[10] = int(12)  .
END.
ELSE do:
    ASSIGN
    v-get-year = int(SUBSTRING(string(TODAY,"99/99/9999"),7,10))  
    v-get-mth[10] = int(SUBSTRING(string(TODAY),1,2))  .

END.

format header
       skip(1)
       "                         " 
       "          " 
       "                  " 
       "                  "
       "                  "
       "%-OF Sales        " 
       "                  "  
       "                  " 
       "%-OF Sales        " 
       "                  " 
       "                  "
       "%-OF Sales        " 
       "                  " 
       "                  "  
       "%-OF Sales        " 
       SKIP
       "Name/City/State          " 
       "Sales Rep " 
       "Budget Year" string(v-get-year,"9999")
       "Sales  "  string(v-label[1]) 
       "Delta             " 
       "Difference        " 
       "Sales "   string(v-label[2]) "  " 
       "Delta             " 
       "Difference        " 
       "Sales "   string(v-label[3]) "  "
       "Delta             "  
       "Difference        " 
       "Sales "  string(v-label[4]) "  "
       "Delta             "  
       "Difference        " 
       SKIP 
       "-------------------------"
       "----------"
       "------------------"
       "------------------"
       "------------------"
       "------------------"
       "------------------"
       "------------------"
       "------------------"
       "------------------"
       "------------------"
       "------------------"
       "------------------"
       "------------------"
       "------------------"

    with frame r-top-2 stream-io width 380 no-labels
         no-box no-underline page-top.


IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Name/City/State,Salesrep," + " Budget Year " + string(v-get-year)  + "," + " Sales " + v-label[1] +  ","  + "Delta" + ",%-OF Sales Difference," + " Sales " + v-label[2] +  ","
              + "Delta" + ",%-OF Sales Difference," + " Sales " + v-label[3] +  "," +  "Delta" + ",%-OF Sales Difference," + " Sales " +  v-label[4] + "," + "Delta" + ",%-OF Sales Difference"
              .
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.                                       

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

EMPTY TEMP-TABLE tt-report.
EMPTY TEMP-TABLE tt-report2.
EMPTY TEMP-TABLE tt-budget.
ASSIGN
    salecount = ""
    custcount = "" .


SESSION:SET-WAIT-STATE ("general").

        FOR EACH ar-inv
          WHERE ar-inv.company  EQ cocode
            AND ar-inv.cust-no  GE fcust
            AND ar-inv.cust-no  LE tcust
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

            {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

        for each ar-invl
            where ar-invl.x-no    eq ar-inv.x-no
              and (ar-invl.billable or not ar-invl.misc)
            no-lock:

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

            v-amt[1] = v-amt[1] + v-amt1.
            v-tot = v-tot + v-amt1.
            create tt-budget.
            assign

             tt-budget.cust    = cust.cust-no
             tt-budget.sman    = v-slsm[1]
             tt-budget.vyear   = ar-inv.inv-date
             tt-budget.budget  = v-amt[1] .
            IF LOOKUP(tt-budget.sman,salecount) = 0 THEN
                ASSIGN salecount = salecount + tt-budget.sman + ",".
            IF LOOKUP(tt-budget.cust, custcount) = 0 THEN
                ASSIGN custcount = custcount + tt-budget.cust + "," .
          end.
        end.
      END.





      FOR each cust where cust.company eq cocode
          AND cust.cust-no GE fcust
          AND cust.cust-no LE tcust
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

          {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

        release ar-invl.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

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


          v-amt[1] = v-amt[1] + v-amt1.

            create tt-budget.
            assign

             tt-budget.cust    = cust.cust-no
             tt-budget.sman    = v-slsm[1]
             tt-budget.vyear   = ar-cash.check-date
             tt-budget.budget  = v-amt[1] .
          IF LOOKUP(tt-budget.sman,salecount) = 0 THEN
              ASSIGN salecount = salecount + tt-budget.sman + ",".
          IF LOOKUP(tt-budget.cust, custcount) = 0 THEN
              ASSIGN custcount = custcount + tt-budget.cust + "," .         
          if not avail ar-invl then leave.
        end.
      end.

       FOR EACH tt-budget 
          WHERE tt-budget.vyear  GE date( "01/01/" + string(v-get-year))
           AND tt-budget.vyear LE date("12/31/" + string(v-get-year))  NO-LOCK:

          IF tt-budget.budget = 0  THEN NEXT.

           CREATE tt-report .
           ASSIGN
                tt-report.cust    = tt-budget.cust
                tt-report.sman    = tt-budget.sman
                tt-report.curr-year = 0 .
           FIND FIRST smanbcst NO-LOCK WHERE smanbcst.company = cocode
                                 AND smanbcst.sman = tt-budget.sman
                                 AND smanbcst.budget-yr = YEAR(tt-budget.vyear)
                                 AND smanbcst.budget-period = MONTH(tt-budget.vyear)
                                NO-ERROR.
           IF AVAIL smanbcst THEN
               tt-report.curr-year = smanbcst.budget-amt.

          IF tt-budget.budget > 0 THEN DO:
              IF LOOKUP(tt-budget.sman,salecount) = 0 THEN
                ASSIGN salecount = salecount + tt-budget.sman + ",".
              IF LOOKUP(tt-budget.cust, custcount) = 0 THEN
                ASSIGN custcount = custcount + tt-budget.cust + "," .
          END.
      END.   


       FOR EACH tt-budget 
          WHERE tt-budget.vyear  GE fdate[1]
           AND tt-budget.vyear LE tdate[1]
           AND LOOKUP(tt-budget.cust,custcount) <> 0 
           AND LOOKUP(tt-budget.sman,salecount) <> 0 NO-LOCK:
            IF tt-budget.budget = 0 THEN NEXT.
           CREATE tt-report .
           ASSIGN
                tt-report.cust    = tt-budget.cust
                tt-report.sman    = tt-budget.sman
                tt-report.Sales1  = tt-budget.budget .
      END. 

      FOR EACH tt-budget 
          WHERE tt-budget.vyear  GE fdate[2]
           AND tt-budget.vyear LE tdate[2] 
           AND LOOKUP(tt-budget.cust,custcount) <> 0 
           AND LOOKUP(tt-budget.sman,salecount) <> 0 NO-LOCK:
              IF tt-budget.budget = 0 THEN NEXT.

           CREATE tt-report .
           ASSIGN
                tt-report.cust    = tt-budget.cust
                tt-report.sman    = tt-budget.sman
                tt-report.Sales2  = tt-budget.budget .
      END.

      FOR EACH tt-budget 
          WHERE tt-budget.vyear  GE fdate[3]
           AND tt-budget.vyear LE tdate[3] 
           AND LOOKUP(tt-budget.cust,custcount) <> 0 
           AND LOOKUP(tt-budget.sman,salecount) <> 0 NO-LOCK:
           IF tt-budget.budget = 0 THEN NEXT.

           CREATE tt-report .
           ASSIGN
                tt-report.cust    = tt-budget.cust
                tt-report.sman    = tt-budget.sman
                tt-report.Sales3  = tt-budget.budget .
      END. 

      FOR EACH tt-budget 
          WHERE tt-budget.vyear  GE fdate[4]
           AND tt-budget.vyear LE tdate[4] 
           AND LOOKUP(tt-budget.cust,custcount) <> 0 
           AND LOOKUP(tt-budget.sman,salecount) <> 0 NO-LOCK:
            IF tt-budget.budget = 0 THEN NEXT.

           CREATE tt-report .
           ASSIGN
                tt-report.cust    = tt-budget.cust
                tt-report.sman    = tt-budget.sman
                tt-report.Sales4  = tt-budget.budget .
      END. 

      FOR EACH tt-report NO-LOCK BREAK BY tt-report.sman  BY tt-report.cust:

          {custom/statusMsg.i "'Processing Customer # ' + string(tt-report.cust)"} 

          IF FIRST-OF(tt-report.cust)  THEN do:
              CREATE tt-report2.
              ASSIGN
                  tt-report2.cust    = tt-report.cust     
                  tt-report2.sman    = tt-report.sman .
          END.
          ASSIGN
              tt-report2.curr-year = tt-report2.curr-year + tt-report.curr-year
              tt-report2.Sales1 = tt-report2.Sales1 + tt-report.Sales1  
              tt-report2.Sales2 = tt-report2.Sales2 + tt-report.Sales2 
              tt-report2.Sales3 = tt-report2.Sales3 + tt-report.Sales3 
              tt-report2.Sales4 = tt-report2.Sales4 + tt-report.Sales4 .
      END.

      FOR EACH tt-report2 NO-LOCK BREAK BY tt-report2.cust:

          {custom/statusMsg.i "'Processing Customer # ' + string(tt-report2.cust)"} 

           v-per-tot = 0.
           FOR EACH smanbcst NO-LOCK WHERE smanbcst.company = cocode
                                 AND smanbcst.sman = tt-report2.sman
                                 AND smanbcst.cust-no = tt-report2.cust
                                 AND smanbcst.budget-yr = (IF tb_col = NO THEN YEAR(TODAY)
                                                           ELSE fi_budgetyear)
                                 /*AND smanbcst.budget-period = MONTH(tt-budget.vyear) */
                                .
               v-per-tot = v-per-tot + smanbcst.budget-amt.
           END.
               tt-report2.curr-year = v-per-tot.
          ASSIGN
              tt-report2.Delta1 = tt-report2.Sales1 - tt-report2.curr-year 
              tt-report2.per1   = tt-report2.Sales1 / tt-report2.curr-year  * 100 
              tt-report2.Delta2 = tt-report2.Sales2 - tt-report2.curr-year 
              tt-report2.per2   = tt-report2.Sales2 / tt-report2.curr-year  * 100 
              tt-report2.Delta3 = tt-report2.Sales3 - tt-report2.curr-year
              tt-report2.per3   = tt-report2.Sales3 / tt-report2.curr-year  * 100
              tt-report2.Delta4 = tt-report2.Sales4  - tt-report2.curr-year
              tt-report2.per4   = tt-report2.Sales4 / tt-report2.curr-year  * 100 .
          IF tt-report2.per1 = ? THEN ASSIGN tt-report2.per1 = 100.
          IF tt-report2.per2 = ? THEN ASSIGN tt-report2.per2 = 100.
          IF tt-report2.per3 = ? THEN ASSIGN tt-report2.per3 = 100.
          IF tt-report2.per4 = ? THEN ASSIGN tt-report2.per4 = 100.

          FIND FIRST cust WHERE cust.company = cocode 
                                AND cust.cust-no = tt-report2.cust NO-LOCK NO-ERROR.
          IF AVAIL cust THEN do:
              ASSIGN
              tt-report2.cust-name = cust.name .
              IF cust.city <> "" THEN
                    tt-report2.cust-name =  tt-report2.cust-name + "," + cust.city. 
              IF cust.state <> "" THEN
                   tt-report2.cust-name =  tt-report2.cust-name + "," + cust.state.
          END.

      END.

      IF v-sort THEN do:
      FOR EACH tt-report2 NO-LOCK BREAK BY tt-report2.cust :
         IF FIRST(tt-report2.cust) THEN VIEW FRAME r-top.
         IF FIRST(tt-report2.cust) THEN VIEW FRAME r-top-2.

          PUT tt-report2.cust-name          FORMAT "x(25)" SPACE(1)
                  tt-report2.sman      FORMAT "x(10)" SPACE(1)
                  tt-report2.curr-year FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.Sales1    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.Delta1    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.per1      FORMAT "->,>>>,>>>,>>9.99%" SPACE(1)
                  tt-report2.Sales2    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.Delta2    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.per2      FORMAT "->,>>>,>>>,>>9.99%" SPACE(1)
                  tt-report2.Sales3    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  tt-report2.Delta3    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  tt-report2.per3      FORMAT "->,>>>,>>>,>>9.99%" SPACE(1) 
                  tt-report2.Sales4    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  tt-report2.Delta4    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  tt-report2.per4      FORMAT "->,>>>,>>>,>>9.99%" SKIP 
                    .

          /*PUT SKIP .*/

          IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               '"' tt-report2.cust-name                                  '",'
               '"' tt-report2.sman                                  '",'
               '"' STRING(tt-report2.curr-year,"$->,>>>,>>>,>>9.99")   '",'
               '"' STRING(tt-report2.Sales1,"$->,>>>,>>>,>>9.99")   '",'
               '"' STRING(tt-report2.Delta1,"$->,>>>,>>>,>>9.99")   '",'
               '"' IF tt-report2.per1 = 0 THEN "" ELSE STRING(tt-report2.per1,"->,>>>,>>>,>>9.99") + "%"    '",'
               '"' "$ " + STRING(tt-report2.Sales2,"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(tt-report2.Delta2,"->,>>>,>>>,>>9.99")   '",'
               '"' IF tt-report2.per2 = 0 THEN "" ELSE STRING(tt-report2.per2,"->,>>>,>>>,>>9.99") + "%"  '",'
               '"' "$ " + STRING(tt-report2.Sales3,"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(tt-report2.Delta3,"->,>>>,>>>,>>9.99")   '",'
               '"' IF tt-report2.per3 = 0 THEN "" ELSE STRING(tt-report2.per3,"->,>>>,>>>,>>9.99") + "%"  '",'
               '"' "$ " + STRING(tt-report2.Sales4,"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(tt-report2.Delta4,"->,>>>,>>>,>>9.99")   '",'
               '"' IF tt-report2.per4 = 0 THEN "" ELSE STRING(tt-report2.per4,"->,>>>,>>>,>>9.99") + "%"  '",'
               SKIP.

       end.
      END.
      ELSE DO:
        RUN nosort.
      END.

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

