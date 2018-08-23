&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-hisatk.w

  Description: High Sales Tracking

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
DEF VAR list-name AS CHAR NO-UNDO.
DEF VAR init-dir AS CHARACTER NO-UNDO.
DEF VAR ou-log LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEF VAR ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
{sys/inc/var.i new shared}
{sys/ref/CustList.i NEW}

ASSIGN
    cocode = gcompany
    locode = gloc.


DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD dec1 AS DEC FIELD dec2 AS DEC
    INDEX key-01 key-01 key-02
    INDEX key-02 key-02.

DEF TEMP-TABLE tt-report2 NO-UNDO LIKE report
    FIELD dec1 AS DEC FIELD dec2 AS DEC
    INDEX high-sales key-01 dec1 DESC dec2 DESC.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.
DEF VAR glCustListActive AS LOGICAL     NO-UNDO.

ASSIGN 
    cTextListToSelect = "Rep,SalesRep Name,Customer,Name,January,February,March,April,May,June," +
                        "July,August,September,October,November,December,YTD Amt"
    cFieldListToSelect = "rep,rname,cust,name,p1,p2,p3,p4,p5,p6," +
                         "p7,p8,p9,p10,p11,p12,ytd-amt"
    cFieldLength = "3,25,8,30,17,17,17,17,17,17," + "17,17,17,17,17,17,17"
    cFieldType = "c,c,c,c,i,i,i,i,i,i," + "i,i,i,i,i,i,i" 
    .

{sys/inc/ttRptSel.i}

ASSIGN 
    cTextListToDefault  = "Customer,Name,January,February,March,April,May,June," +
                          "July,August,September,October,November,December,YTD Amt".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date srt-period ~
rd_print tb_cust-list begin_cust end_cust btnCustList begin_slsmn end_slsmn ~
tb_prt-cust sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date srt-period lbl_sort ~
rd_print tb_cust-list begin_cust end_cust begin_slsmn end_slsmn tb_prt-cust ~
sl_avail sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Cust#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Cust#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "End Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-hisatk.csv" 
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

DEFINE VARIABLE srt-period AS INTEGER FORMAT "->9" INITIAL 0 
     LABEL "Sort By Period" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

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
     SIZE 34 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.76.

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

DEFINE VARIABLE tb_prt-cust AS LOGICAL INITIAL yes 
     LABEL "Print Customers w/Zero Balance?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

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
     begin_date AT ROW 2.1 COL 32 COLON-ALIGNED HELP
          "Enter AS Of Date"
     end_date AT ROW 2.1 COL 63.8 COLON-ALIGNED HELP
          "Enter AS Of Date"
     srt-period AT ROW 3.24 COL 32 COLON-ALIGNED HELP
          "Enter Periods to Sort By"
     lbl_sort AT ROW 4.81 COL 21 COLON-ALIGNED NO-LABEL
     rd_print AT ROW 4.81 COL 34 NO-LABEL
     tb_cust-list AT ROW 6 COL 31.8 WIDGET-ID 6
     begin_cust AT ROW 6.95 COL 24.6 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 6.95 COL 63.8 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     btnCustList AT ROW 8.05 COL 64.2 WIDGET-ID 8
     begin_slsmn AT ROW 8.1 COL 24.6 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number" WIDGET-ID 6
     end_slsmn AT ROW 8.1 COL 63.8 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number" WIDGET-ID 8
     tb_prt-cust AT ROW 9.29 COL 31
     sl_avail AT ROW 11.62 COL 4.2 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 11.62 COL 40.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 11.62 COL 59.6 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 12.62 COL 40.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 13.62 COL 40.2 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 14.67 COL 40.2 WIDGET-ID 40
     btn_down AT ROW 15.67 COL 40.2 WIDGET-ID 42
     rd-dest AT ROW 17.71 COL 5 NO-LABEL
     lv-ornt AT ROW 18.19 COL 31 NO-LABEL
     lines-per-page AT ROW 18.19 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 20.1 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 21.05 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.24 COL 30
     tb_excel AT ROW 23.52 COL 50.2 RIGHT-ALIGNED
     tb_runExcel AT ROW 23.52 COL 71.2 RIGHT-ALIGNED
     fi_file AT ROW 24.33 COL 28.2 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 26.05 COL 19
     btn-cancel AT ROW 26.05 COL 57
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 10.91 COL 59.6 WIDGET-ID 44
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 10.91 COL 5 WIDGET-ID 38
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 17 COL 3
     "(Enter 99 For YTD)" VIEW-AS TEXT
          SIZE 22 BY 1 AT ROW 3.57 COL 51
          FGCOLOR 9 
     RECT-6 AT ROW 17.43 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 28.38.


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
         TITLE              = "High Sales Tracking"
         HEIGHT             = 26.86
         WIDTH              = 96.2
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
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
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

ASSIGN 
       srt-period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prt-cust:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* High Sales Tracking */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  IF we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* High Sales Tracking */
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
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Cust# */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Start Date */
DO:
    DEF VAR daTest AS DATE.
    ASSIGN 
        {&self-name}
        daTest = DATE(SELF:SCREEN-VALUE)
        daTest = daTest + 364
        end_date:SCREEN-VALUE = STRING(MONTH(daTest),"99") + "/" +
                                STRING(DAY(daTest) - 1,"99") + "/" + 
                                STRING(YEAR(daTest),"9999").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Salesrep# */
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
  
    FIND FIRST ttCustList NO-LOCK NO-ERROR.
    IF NOT AVAIL ttCustList 
    AND tb_cust-list THEN DO:
        EMPTY TEMP-TABLE ttCustList.
        RUN BuildCustList(INPUT cocode,
                        INPUT tb_cust-list AND glCustListActive ,
                        INPUT begin_cust,
                        INPUT END_cust).
    END.       
  
    RUN GetSelectionList.
    
    RUN run-report. 
    STATUS DEFAULT "Processing Complete".
  
    CASE rd-dest:
        WHEN 1 THEN RUN output-to-printer.
        WHEN 2 THEN RUN output-to-screen.
        WHEN 3 THEN RUN output-to-file.
        WHEN 4 THEN DO:
            /*run output-to-fax.*/
            {custom/asifax.i &begin_cust=begin_slsmn
                             &END_cust=END_slsmn
                             &fax-subject=c-win:title
                             &fax-body=c-win:title
                             &fax-file=list-name }
        END.
        WHEN 5 THEN DO:
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
        WHEN 6 THEN RUN output-to-port.
    END CASE. 
    SESSION:SET-WAIT-STATE ("").
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def C-Win
ON CHOOSE OF Btn_Def IN FRAME FRAME-A /* Default */
DO:
    DEF VAR cSelectedList AS cha NO-UNDO.

    RUN DisplaySelectionDefault.
    RUN DisplaySelectionList2.

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
ON HELP OF end_cust IN FRAME FRAME-A /* Ending Cust# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Cust# */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* End Date */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Salesrep# */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
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
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w ({&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN 
        {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
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


&Scoped-define SELF-NAME rd_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_print C-Win
ON VALUE-CHANGED OF rd_print IN FRAME FRAME-A
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail C-Win
ON DEFAULT-ACTION OF sl_avail IN FRAME FRAME-A
DO:
    IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) 
    OR sl_selected:NUM-ITEMS = 0) THEN ASSIGN 
        ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
        ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected C-Win
ON DEFAULT-ACTION OF sl_selected IN FRAME FRAME-A
DO:
    DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
        IF {&SELF-NAME}:IS-SELECTED(i) THEN DO:
            ASSIGN 
                ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
                ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
                .
        END.           
    END.
    IF {&SELF-NAME}:NUM-ITEMS NE 0 THEN ASSIGN
        {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(1)
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME srt-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL srt-period C-Win
ON LEAVE OF srt-period IN FRAME FRAME-A /* Sort By Period */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
    ASSIGN {&self-name}.
    EMPTY TEMP-TABLE ttCustList.
    RUN SetCustRange(INPUT tb_cust-list).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-cust C-Win
ON VALUE-CHANGED OF tb_prt-cust IN FRAME FRAME-A /* Print Customers w/Zero Balance? */
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: HANDLE ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    /* security check need {methods/prgsecur.i} in definition section */
    IF access-close THEN DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    FIND FIRST period NO-LOCK WHERE
        period.company EQ cocode AND 
        period.pst     LE TODAY AND 
        period.pend    GE TODAY
        NO-ERROR.  
    IF NOT AVAIL period THEN DO:
        MESSAGE
            "There is an error in your G/L Calendar." SKIP
            "Please contact your system administrator."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    ASSIGN
        srt-period = period.pnum.

    RUN DisplaySelectionList.
    RUN enable_UI.

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ("HR1",
                                cocode, 
                                OUTPUT ou-log,
                                OUTPUT ou-cust-int).

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_date IN FRAME {&FRAME-NAME}.
    END.

    RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'HR1',
                            INPUT NO,
                            OUTPUT glCustListActive).
  
    {sys/inc/chblankcust.i ""HR1""}

    IF ou-log THEN DO:
        ASSIGN 
            tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
            tb_cust-list = YES 
            .
        RUN SetCustRange(INPUT tb_cust-list).
    END.
    ELSE ASSIGN
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        .

    IF ou-log 
    AND ou-cust-int = 0 THEN DO:
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

    DEF VAR lActive AS LOGICAL     NO-UNDO.

    IF iplList THEN DO:
        RUN sys/ref/CustList.p (INPUT ipcCompany,
                                INPUT 'HR1',
                                INPUT YES,
                                OUTPUT lActive).
    END.
    ELSE DO:
        FOR EACH bf-cust NO-LOCK WHERE
            bf-cust.company EQ ipcCompany AND 
            bf-cust.cust-no GE ipcBeginCust AND 
            bf-cust.cust-no LE ipcEndCust:
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
                                  INPUT 'HR1').


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
        ASSIGN
            cListContents = cListContents + 
                            (IF cListContents = "" THEN ""  ELSE ",") +
                            ENTRY(iCount,cTextListToDefault)   .
    END.            
    ASSIGN 
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
        ASSIGN 
            cListContents = cListContents +
                            (IF cListContents = "" THEN ""  ELSE ",") +
                             ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
            .
    END.

    ASSIGN 
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
        ASSIGN 
            cListContents = cListContents +
                            (IF cListContents = "" THEN ""  ELSE ",") +
                            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
            .
    END.

    ASSIGN 
        sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

    DO iCount = 1 TO sl_selected:NUM-ITEMS:
        ASSIGN 
            ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
    END.

    ASSIGN 
        cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    DO iCount = 1 TO sl_selected:NUM-ITEMS:
        IF LOOKUP(ENTRY(iCount,cTmpList), cTextListToSelect) = 0 THEN ASSIGN
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
  DISPLAY begin_date end_date srt-period lbl_sort rd_print tb_cust-list 
          begin_cust end_cust begin_slsmn end_slsmn tb_prt-cust sl_avail 
          sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date end_date srt-period rd_print tb_cust-list 
         begin_cust end_cust btnCustList begin_slsmn end_slsmn tb_prt-cust 
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
    ASSIGN 
        cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}
        iColumnLength = 0.

    DO i = 1 TO sl_selected:NUM-ITEMS:
        FIND FIRST ttRptList NO-LOCK WHERE 
            ttRptList.TextList = ENTRY(i,cTmpList) 
            NO-ERROR.     

        CREATE ttRptSelected.
        ASSIGN 
            ttRptSelected.TextList =  ENTRY(i,cTmpList)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field C-Win 
PROCEDURE Move-Field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER move AS CHARACTER NO-UNDO.

    DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME} WITH FRAME {&FRAME-NAME}:
        IF sl_selected:IS-SELECTED(i) THEN DO:
            IF move = "Down" 
            AND i NE sl_selected:NUM-ITEMS THEN ASSIGN
                ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
                ldummy = sl_selected:DELETE(i)
                sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
                .
            ELSE IF move = "Up" 
            AND i NE 1 THEN ASSIGN
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
    RUN custom/prntproc.p (list-name,
                           INT(lv-font-no),
                           lv-ornt).
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
    RUN scr-rpt.w (list-name,
                   c-win:title,
                   int(lv-font-no),
                   lv-ornt).
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
    DEF VAR fdate AS date EXTENT 20 NO-UNDO.
    DEF VAR tdate AS date EXTENT 20 NO-UNDO.
    DEF VAR fsman LIKE cust.sman init "" NO-UNDO.
    DEF VAR tsman LIKE fsman init "zzz" NO-UNDO.
    DEF VAR fcus LIKE cust.cust-no init "" NO-UNDO.
    DEF VAR tcus LIKE cust.cust-no init "" NO-UNDO.
    DEF VAR v-date AS date FORMAT "99/99/9999" NO-UNDO.
    DEF VAR v-per-1 AS int FORMAT ">9" NO-UNDO.
    DEF VAR v-per-2 AS int FORMAT ">9" NO-UNDO.
    DEF VAR v-custs AS int FORMAT ">>,>>9" NO-UNDO.
    DEF VAR v-sort AS log init yes FORMAT "Customer/Salesrep" NO-UNDO.
    DEF VAR v-inc AS log init YES NO-UNDO.
    DEF VAR v-amt1 AS DEC NO-UNDO.
    DEF VAR v-slsm LIKE ar-invl.sman EXTENT 1 NO-UNDO.
    DEF VAR v-slsp LIKE ar-invl.s-pct EXTENT 1 NO-UNDO.
    DEF VAR v-amt AS dec EXTENT 21 NO-UNDO.
    DEF VAR v-tot-amt AS dec EXTENT 21 NO-UNDO.
    DEF VAR v-label AS CHAR EXTENT 4 FORMAT "x(17)" NO-UNDO.
    DEF VAR v-yr LIKE period.yr NO-UNDO.
    DEF VAR v AS INT NO-UNDO.
    DEF VAR v1 AS INT NO-UNDO.
    DEF VAR v-ytd AS LOG NO-UNDO.
    DEF VAR v-prt AS INT NO-UNDO.
    DEF VAR v-j AS DEC NO-UNDO.
    DEF VAR lv-sman AS CHAR NO-UNDO.
    DEF VAR lv-sname AS CHAR NO-UNDO.
    DEF VAR dSdate AS DATE FORMAT "99/99/9999" NO-UNDO.
    DEF VAR dEdate AS DATE FORMAT "99/99/9999" NO-UNDO.
    DEF VAR cDisplay AS CHAR NO-UNDO.
    DEF VAR cExcelDisplay AS CHAR NO-UNDO.
    DEF VAR hField AS HANDLE NO-UNDO.
    DEF VAR cTmpField AS CHAR NO-UNDO.
    DEF VAR cVarValue AS CHAR NO-UNDO.
    DEF VAR cExcelVarValue AS CHAR NO-UNDO.
    DEF VAR cSelectedList AS CHAR NO-UNDO.
    DEF VAR cFieldName AS CHAR NO-UNDO.
    DEF VAR str-tit4 AS CHAR FORMAT "x(200)" NO-UNDO.
    DEF VAR str-tit5 AS CHAR FORMAT "x(200)" NO-UNDO.
    DEF VAR str-line AS CHAR FORMAT "x(300)" NO-UNDO.
    DEF VAR excelheader AS CHAR NO-UNDO.
    DEF VAR lSelected AS LOG INIT YES NO-UNDO.
    DEF VAR d-gr-tot-amt AS DECIMAL EXTENT 21 NO-UNDO.
    DEF VAR lp-zero AS LOGICAL INITIAL YES NO-UNDO.
    DEF VAR cslist AS CHAR NO-UNDO.

    {sys/form/r-top5DL3.f} 
    ASSIGN 
        cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    FORM HEADER
        "Salesrep:"
        lv-sman FORMAT "x(40)"
        WITH PAGE-TOP FRAME r-top2 STREAM-IO WIDTH 180.

    FORM cust.cust-no
        cust.name
        v-amt[17] FORMAT "->,>>>,>>>,>>9.99"
        v-amt[18] FORMAT "->,>>>,>>>,>>9.99"
        v-amt[19] FORMAT "->,>>>,>>>,>>9.99"
        v-amt[20] FORMAT "->,>>>,>>>,>>9.99"
        v-amt[21] FORMAT "->,>>>,>>>,>>9.99" COLUMN-LABEL "YTD" 
        HEADER SKIP(1)
        "Customer Name                          "
        v-label[1]
        v-label[2]
        v-label[3]
        v-label[4]
        "          YTD Amt" SKIP
        WITH NO-BOX NO-LABELS FRAME custx DOWN STREAM-IO WIDTH 180.

    SESSION:SET-WAIT-STATE ("general").

    EMPTY TEMP-TABLE tt-report.
    EMPTY TEMP-TABLE tt-report2.

    FIND FIRST company NO-LOCK WHERE
        company.company EQ cocode.

    ASSIGN
        v-per-1 = 12
        v-per-2 = period.pnum
        v-per-2 = srt-period
        str-tit2 = c-win:title
        {sys/inc/ctrtext.i str-tit2 112}
        dSdate = begin_date
        dEdate = end_date
        v-sort = rd_print EQ "Customer"
        fsman = begin_slsmn
        tsman = end_slsmn
        v-inc = tb_prt-cust
        lSelected = tb_cust-list
        fcus =  begin_cust
        tcus =  END_cust
        .

    FIND FIRST period NO-LOCK WHERE
        period.company EQ cocode AND
        period.pst LE dEdate AND
        period.pend GE dSdate.
 
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:
        IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength THEN ASSIGN 
            str-tit4 = str-tit4 + ttRptSelected.TextList + " "
            str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + "," .        
        ELSE ASSIGN 
            str-tit4 = str-tit4 + 
                        (IF ttRptSelected.HeadingFromLeft THEN
                            ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
                        ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
            str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".
        ASSIGN
            cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "Prd 1,Prd 2,Prd 3,Prd 4,Prd 5,Prd 6,Prd 7,Prd 8,Prd 9,Prd 10,Prd 11,Prd 12,YTD Amt") <> 0 THEN ASSIGN
            str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE ASSIGN
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    FIND LAST period NO-LOCK WHERE
        period.company EQ cocode AND
        period.pst LE dEdate AND
        period.pend GE dSdate.

    ASSIGN 
        v-yr = period.yr.

    IF  v-per-2 eq 99 THEN DO:
        FIND LAST period NO-LOCK WHERE
            period.company EQ cocode AND
            period.yr EQ v-yr AND
            period.pnum LE v-per-1.
        ASSIGN
            v-ytd   = yes
            v-per-2 = period.pnum.
    END.

    ASSIGN
        v-date = period.pend
        v1 = v-per-2 - v-per-1 + 1.

    IF v1 LT 1 THEN ASSIGN
        v1 = 1.

    FOR EACH period NO-LOCK WHERE
        period.company EQ cocode AND
        period.yr EQ v-yr AND
        period.pnum LE v-per-2:

        ASSIGN
            fdate[period.pnum] = period.pst
            tdate[period.pnum] = period.pend.

        IF period.pnum GE v1 THEN DO:
            ASSIGN
                v = v + 1
                v-label[v] = v-label[v] + (if v-label[v] eq "" THEN "Period " ELSE "/") +
                             trim(STRING(period.pnum,">9")).
            IF v GE 4 THEN ASSIGN
                v = 0.
        END.
    END.

    DO i = 1 TO 4:
        ASSIGN 
            v-label[i] = fill(" ",17 - length(trim(v-label[i]))) + trim(v-label[i]).
    END.

    {sys/inc/print1.i}
    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN 
        RUN show-param.

    IF lselected THEN DO:
        FIND FIRST ttCustList NO-LOCK WHERE 
            ttCustList.log-fld 
            USE-INDEX cust-no 
            NO-ERROR.
        IF AVAIL ttCustList THEN ASSIGN 
            fcus = ttCustList.cust-no.
        FIND LAST ttCustList NO-LOCK WHERE 
            ttCustList.log-fld 
            USE-INDEX cust-no 
            NO-ERROR.
        IF AVAIL ttCustList THEN ASSIGN 
            tcus = ttCustList.cust-no .
    END.

    IF tb_excel THEN DO:
        OUTPUT STREAM excel TO VALUE(fi_file).
        PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' skip.
    END. 

    FOR EACH ar-inv NO-LOCK WHERE
        ar-inv.company  EQ cocode AND 
        ar-inv.cust-no  GE fcus AND 
        ar-inv.cust-no  LE tcus AND 
        (if lselected THEN can-find(first ttCustList where ttCustList.cust-no eq ar-inv.cust-no
        AND ttCustList.log-fld no-lock) ELSE true) AND 
        ar-inv.inv-date GE dSdate AND 
        ar-inv.inv-date LE dEdate AND 
        ar-inv.posted   EQ YES
        USE-INDEX inv-date,
        FIRST cust NO-LOCK WHERE 
            cust.company EQ ar-inv.company AND 
            cust.cust-no EQ ar-inv.cust-no:

        FOR EACH ar-invl NO-LOCK WHERE
            ar-invl.x-no EQ ar-inv.x-no AND 
            (ar-invl.billable OR NOT ar-invl.misc):
      
            {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
    
            DO i = 1 TO 3:
                ASSIGN
                    v-amt = 0
                    v-slsm[1] = IF ar-invl.sman[i] EQ "" 
                                AND i EQ 1 
                                AND NOT ar-invl.misc THEN
                                    cust.sman 
                                ELSE 
                                    ar-invl.sman[i].

                IF v-slsm[1] LT fsman
                OR v-slsm[1] GT tsman
                OR (i NE 1 AND
                    (v-slsm[1] EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.
      
                ASSIGN
                    v-slsp[1] = IF ar-invl.sman[i] EQ ""
                                OR (ar-invl.s-pct[i] EQ 0 AND i EQ 1) THEN 
                                    100
                                ELSE 
                                    ar-invl.s-pct[i]
                    v-amt1 = ar-invl.amt * v-slsp[1] / 100.
      
                IF ar-inv.inv-date GE fdate[v-per-2] 
                AND ar-inv.inv-date LE tdate[v-per-2] THEN ASSIGN
                    v-amt[1] = v-amt[1] + v-amt1.

                ASSIGN 
                    v-amt[2] = v-amt[2] + v-amt1.

                CREATE tt-report.
                ASSIGN
                    tt-report.key-01  = IF v-sort THEN cust.cust-no ELSE v-slsm[1]
                    tt-report.key-02  = cust.cust-no
                    tt-report.key-03  = v-slsm[1]
                    tt-report.dec1    = v-amt[1]
                    tt-report.dec2    = v-amt[2]
                    tt-report.key-10  = "ar-invl"
                    tt-report.rec-id  = recid(ar-invl).
                RELEASE tt-report.
            END.
        END.
    END.

    FOR EACH cust NO-LOCK WHERE
        cust.company EQ cocode AND 
        cust.cust-no GE fcus AND 
        cust.cust-no LE tcus AND 
        (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE 
                                    ttCustList.cust-no EQ cust.cust-no AND 
                                    ttCustList.log-fld) ELSE TRUE),
        EACH ar-cash NO-LOCK WHERE
            ar-cash.company EQ cocode AND
            ar-cash.cust-no EQ cust.cust-no AND
            ar-cash.check-date GE dSdate AND
            ar-cash.check-date LE dEdate AND
            ar-cash.posted EQ TRUE
            USE-INDEX ar-cash,
        EACH ar-cashl NO-LOCK WHERE
            ar-cashl.c-no EQ ar-cash.c-no AND 
            ar-cashl.posted EQ YES AND 
            ar-cashl.memo EQ YES AND 
                CAN-FIND(FIRST account WHERE 
                            account.company EQ ar-cashl.company AND 
                            account.actnum  EQ ar-cashl.actnum AND 
                            account.type    EQ "R"):
    
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
        RELEASE ar-invl.
        RUN salrep/getoeret.p (ROWID(ar-cashl), 
                               BUFFER reftable, 
                               BUFFER oe-retl).

        IF AVAIL oe-retl THEN FIND FIRST ar-invl NO-LOCK WHERE
            ar-invl.company EQ cocode AND
            ar-invl.cust-no EQ cust.cust-no AND
            ar-invl.inv-no EQ ar-cashl.inv-no AND
            ar-invl.i-no EQ oe-retl.i-no AND
            (ar-invl.billable OR NOT ar-invl.misc)
            NO-ERROR.
 
        DO i = 1 TO 3:
            ASSIGN
                v-amt = 0
                v-slsm[1] = IF (NOT AVAIL ar-invl)
                            OR (ar-invl.sman[i] EQ "" AND i EQ 1) THEN
                                cust.sman 
                            ELSE 
                                ar-invl.sman[i].

            IF v-slsm[1] LT fsman
            OR v-slsm[1] GT tsman 
            OR (i NE 1 AND (v-slsm[1] EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

            ASSIGN
                v-slsp[1] = IF (NOT AVAIL ar-invl)
                            OR ar-invl.sman[i] EQ ""
                            OR (ar-invl.s-pct[i] EQ 0 AND i EQ 1) THEN
                                100
                            ELSE
                                ar-invl.s-pct[i]
                v-amt1 = (ar-cashl.amt-paid - ar-cashl.amt-disc) * v-slsp[1] / 100.
      
            IF ar-cash.check-date GE fdate[v-per-2] 
            AND ar-cash.check-date LE tdate[v-per-2] THEN
                v-amt[1] = v-amt[1] + v-amt1.

            ASSIGN 
                v-amt[2] = v-amt[2] + v-amt1.

            CREATE tt-report.
            ASSIGN
                tt-report.key-01  = IF v-sort THEN cust.cust-no ELSE  v-slsm[1]
                tt-report.key-02  = cust.cust-no
                tt-report.key-03  = v-slsm[1]
                tt-report.dec1    = v-amt[1]
                tt-report.dec2    = v-amt[2]
                tt-report.key-10  = "ar-cashl"
                tt-report.rec-id  = recid(ar-cashl).
            RELEASE tt-report.

            IF NOT AVAIL ar-invl THEN LEAVE.
        END.
    END.

    IF v-inc THEN FOR EACH cust NO-LOCK WHERE 
        cust.company EQ cocode AND
        cust.cust-no GE fcus AND
        cust.cust-no LE tcus AND
        (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE 
                                        ttCustList.cust-no EQ cust.cust-no AND 
                                        ttCustList.log-fld) ELSE TRUE) AND
        NOT CAN-FIND(FIRST tt-report WHERE
                        tt-report.key-02 EQ cust.cust-no):

        CREATE tt-report.
        ASSIGN
            tt-report.key-01  = IF v-sort THEN cust.cust-no ELSE cust.sman
            tt-report.key-02  = cust.cust-no
            tt-report.key-03  = cust.sman .
        RELEASE tt-report.
    END.

    ASSIGN
        v-amt = 0.

    FOR EACH tt-report NO-LOCK
        BREAK BY tt-report.key-02
        BY tt-report.key-03:

        ASSIGN
            v-amt[1] = v-amt[1] + tt-report.dec1
            v-amt[2] = v-amt[2] + tt-report.dec2.

        IF LAST-OF(tt-report.key-03) THEN DO:
            IF v-amt[1] NE 0 
            OR v-inc 
            OR v-ytd THEN DO:
                CREATE tt-report2.
                ASSIGN
                    tt-report2.key-01 = tt-report.key-01
                    tt-report2.key-02 = tt-report.key-02
                    tt-report2.key-03 = tt-report.key-03 
                    tt-report2.dec1   = IF v-ytd THEN 0 ELSE v-amt[1]
                    tt-report2.dec2   = v-amt[2].
            END.
            ASSIGN
                v-amt = 0.
        END.
    END.

    ASSIGN
        lv-sman  = ""
        lv-sname = "".

    FOR EACH tt-report2,
        FIRST cust NO-LOCK WHERE
            cust.company EQ cocode AND
            cust.cust-no EQ tt-report2.key-02
        BREAK BY tt-report2.key-01
        BY tt-report2.dec1 DESC
        BY tt-report2.dec2 DESC
        WITH FRAME custx:
    
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
  
        IF FIRST-OF(tt-report2.key-01) THEN DO:
            ASSIGN 
                v-prt = 0.
            IF FIRST(tt-report2.key-01) THEN 
                VIEW FRAME r-top.
        END.
  
        FIND FIRST sman NO-LOCK WHERE
            sman.company EQ cocode AND
            sman.sman EQ tt-report2.key-03
            NO-ERROR.
      
        ASSIGN 
            lv-sman = TRIM(tt-report2.key-03)
            lv-sname = TRIM(IF AVAIL sman THEN sman.sname ELSE "Not on file")
            v-amt = 0
            v-prt = v-prt + 1.

        FOR EACH tt-report WHERE
            tt-report.key-01 EQ tt-report2.key-01 AND
            tt-report.key-02 EQ tt-report2.key-02:

            IF tt-report.key-10 EQ "ar-invl" THEN DO:
                FIND ar-invl NO-LOCK WHERE 
                    RECID(ar-invl) EQ tt-report.rec-id.
                FIND ar-inv NO-LOCK WHERE 
                    ar-inv.x-no EQ ar-invl.x-no.

                DO v = v1 TO v-per-2: 
                    IF ar-inv.inv-date GE fdate[v] 
                    AND ar-inv.inv-date LE tdate[v] THEN ASSIGN
                        v-amt[v] = v-amt[v] + DEC(tt-report.dec2).
                END.

                ASSIGN 
                    v-amt[21] = v-amt[21] + DEC(tt-report.dec2).
            END.
            ELSE IF tt-report.key-10 EQ "ar-cashl" THEN DO:
                FIND ar-cashl NO-LOCK WHERE 
                    RECID(ar-cashl) EQ tt-report.rec-id.
                FIND ar-cash NO-LOCK WHERE 
                    ar-cash.c-no EQ ar-cashl.c-no.

                DO v = v1 TO v-per-2:
                    IF ar-cash.check-date GE fdate[v] 
                    AND ar-cash.check-date LE tdate[v] THEN ASSIGN
                        v-amt[v] = v-amt[v] + dec(tt-report.dec2).
                END.

                ASSIGN 
                    v-amt[21] = v-amt[21] + dec(tt-report.dec2).
            END.
        END.
  
        ASSIGN 
            lp-zero = YES.
  
        DO i = 1 TO 21: 
            IF v-amt[i] NE 0 THEN ASSIGN 
                lp-zero = NO.
        END. 

        IF v-inc 
        OR (NOT v-inc AND (lp-zero EQ NO)) THEN DO:
     
            ASSIGN 
                v = 0
                cDisplay = ""
                cTmpField = ""
                cVarValue = ""
                cExcelDisplay = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):
                ASSIGN
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, 
                                                     INPUT ENTRY(i,cSelectedList)), 
                                                     cFieldListToSelect).
                CASE cTmpField:  
                    WHEN "rep" THEN cVarValue = STRING(lv-sman,"x(3)") .
                    WHEN "rname" THEN cVarValue = STRING(lv-sname,"x(25)").
                    WHEN "cust" THEN cVarValue = STRING(cust.cust-no,"x(8)") .
                    WHEN "name" THEN cVarValue = STRING(cust.name,"x(30)").
                    WHEN "p1" THEN cVarValue = STRING(v-amt[1],"->,>>>,>>>,>>9.99").
                    WHEN "p2" THEN cVarValue = STRING(v-amt[2],"->,>>>,>>>,>>9.99").
                    WHEN "p3" THEN cVarValue = STRING(v-amt[3],"->,>>>,>>>,>>9.99").
                    WHEN "p4" THEN cVarValue = STRING(v-amt[4],"->,>>>,>>>,>>9.99").
                    WHEN "p5" THEN cVarValue = STRING(v-amt[5],"->,>>>,>>>,>>9.99").
                    WHEN "p6" THEN cVarValue = STRING(v-amt[6],"->,>>>,>>>,>>9.99").
                    WHEN "p7" THEN cVarValue = STRING(v-amt[7],"->,>>>,>>>,>>9.99").
                    WHEN "p8" THEN cVarValue = STRING(v-amt[8],"->,>>>,>>>,>>9.99").
                    WHEN "p9" THEN cVarValue = STRING(v-amt[9],"->,>>>,>>>,>>9.99").
                    WHEN "p10" THEN cVarValue = STRING(v-amt[10],"->,>>>,>>>,>>9.99").
                    WHEN "p11" THEN cVarValue = STRING(v-amt[11],"->,>>>,>>>,>>9.99").
                    WHEN "p12" THEN cVarValue = STRING(v-amt[12],"->,>>>,>>>,>>9.99").
                    WHEN "ytd-amt" THEN cVarValue = STRING(v-amt[21],"->,>>>,>>>,>>9.99").
                END CASE.

                ASSIGN
                    cExcelVarValue = cVarValue
                    cDisplay = cDisplay + cVarValue + 
                               FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue))
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
            END.

            PUT SKIP.

            IF tb_excel THEN
                PUT STREAM excel UNFORMATTED SKIP(1).

            DO i = 1 TO 21:
                ASSIGN 
                    v-tot-amt[i] = v-tot-amt[i] + v-amt[i].
            END.
        END.

        IF LAST-OF(tt-report2.key-01) THEN DO:
            ASSIGN
                v = 0.

            DO i = v1 TO v-per-2:
                ASSIGN 
                    v = v + 1.

                IF v MODULO 4 EQ 0 
                OR i EQ v-per-2   THEN DO:
                    ASSIGN 
                        v-j = v / 4.
                    {sys/inc/roundup.i v-j}
                    ASSIGN
                        j = v-j
                        j = v1 + (4 * (j - 1)).
                END.
            END.

            ASSIGN 
                cDisplay = ""
                cTmpField = ""
                cVarValue = ""
                cExcelDisplay = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                ASSIGN
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:   
                    WHEN "rep" THEN cVarValue = "" .
                    WHEN "rname" THEN cVarValue = "".
                    WHEN "cust" THEN cVarValue = "" .
                    WHEN "name" THEN cVarValue = "".
                    WHEN "p1" THEN cVarValue = STRING(v-tot-amt[1],"->,>>>,>>>,>>9.99").
                    WHEN "p2" THEN cVarValue = STRING(v-tot-amt[2],"->,>>>,>>>,>>9.99").
                    WHEN "p3" THEN cVarValue = STRING(v-tot-amt[3],"->,>>>,>>>,>>9.99").
                    WHEN "p4" THEN cVarValue = STRING(v-tot-amt[4],"->,>>>,>>>,>>9.99").
                    WHEN "p5" THEN cVarValue = STRING(v-tot-amt[5],"->,>>>,>>>,>>9.99").
                    WHEN "p6" THEN cVarValue = STRING(v-tot-amt[6],"->,>>>,>>>,>>9.99").
                    WHEN "p7" THEN cVarValue = STRING(v-tot-amt[7],"->,>>>,>>>,>>9.99").
                    WHEN "p8" THEN cVarValue = STRING(v-tot-amt[8],"->,>>>,>>>,>>9.99").
                    WHEN "p9" THEN cVarValue = STRING(v-tot-amt[9],"->,>>>,>>>,>>9.99").
                    WHEN "p10" THEN cVarValue = STRING(v-tot-amt[10],"->,>>>,>>>,>>9.99").
                    WHEN "p11" THEN cVarValue = STRING(v-tot-amt[11],"->,>>>,>>>,>>9.99").
                    WHEN "p12" THEN cVarValue = STRING(v-tot-amt[12],"->,>>>,>>>,>>9.99").
                    WHEN "ytd-amt" THEN cVarValue = STRING(v-tot-amt[21],"->,>>>,>>>,>>9.99").
                END CASE.

                ASSIGN
                    cExcelVarValue = cVarValue
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue))
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT str-line SKIP.
            PUT UNFORMATTED 
                "         TOTALS" subSTRING(cDisplay,16,350) SKIP(1).
            IF tb_excel THEN DO:
                PUT STREAM excel UNFORMATTED  
                    '               TOTALS ,'
                    SUBSTRING(cExcelDisplay,4,350) SKIP(1).
            END.
   
            DO i = 1 TO 21:
                ASSIGN 
                    d-gr-tot-amt[i] = d-gr-tot-amt[i] + v-tot-amt[i].
            END.

            IF LAST-OF(tt-report2.key-01) THEN ASSIGN
                v-tot-amt = 0.
        END.
    END.

    ASSIGN 
        cDisplay = ""
        cTmpField = ""
        cVarValue = ""
        cExcelDisplay = ""
        cExcelVarValue = "".

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        ASSIGN 
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:   
            WHEN "rep" THEN cVarValue = "" .
            WHEN "rname" THEN cVarValue = "".
            WHEN "cust" THEN cVarValue = "" .
            WHEN "name" THEN cVarValue = "".
            WHEN "p1" THEN cVarValue = STRING(d-gr-tot-amt[1],"->,>>>,>>>,>>9.99").
            WHEN "p2" THEN cVarValue = STRING(d-gr-tot-amt[2],"->,>>>,>>>,>>9.99").
            WHEN "p3" THEN cVarValue = STRING(d-gr-tot-amt[3],"->,>>>,>>>,>>9.99").
            WHEN "p4" THEN cVarValue = STRING(d-gr-tot-amt[4],"->,>>>,>>>,>>9.99").
            WHEN "p5" THEN cVarValue = STRING(d-gr-tot-amt[5],"->,>>>,>>>,>>9.99").
            WHEN "p6" THEN cVarValue = STRING(d-gr-tot-amt[6],"->,>>>,>>>,>>9.99").
            WHEN "p7" THEN cVarValue = STRING(d-gr-tot-amt[7],"->,>>>,>>>,>>9.99").
            WHEN "p8" THEN cVarValue = STRING(d-gr-tot-amt[8],"->,>>>,>>>,>>9.99").
            WHEN "p9" THEN cVarValue = STRING(d-gr-tot-amt[9],"->,>>>,>>>,>>9.99").
            WHEN "p10" THEN cVarValue = STRING(d-gr-tot-amt[10],"->,>>>,>>>,>>9.99").
            WHEN "p11" THEN cVarValue = STRING(d-gr-tot-amt[11],"->,>>>,>>>,>>9.99").
            WHEN "p12" THEN cVarValue = STRING(d-gr-tot-amt[12],"->,>>>,>>>,>>9.99").
            WHEN "ytd-amt" THEN cVarValue = STRING(d-gr-tot-amt[21],"->,>>>,>>>,>>9.99").
        END CASE.

        ASSIGN
            cExcelVarValue = cVarValue
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue))
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT str-line SKIP.
    PUT UNFORMATTED "   GRAND TOTALS" subSTRING(cDisplay,16,350) SKIP.
    
    IF tb_excel THEN DO:
        PUT STREAM excel UNFORMATTED  
            '        GRAND TOTALS ,'
            SUBSTRING(cExcelDisplay,4,350) SKIP(1).
    END. 

    IF tb_excel THEN DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_runExcel THEN
            OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

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
    DEF VAR lv-frame-hdl AS HANDLE NO-UNDO.
    DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
    DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
    DEF VAR lv-field2-hdl AS HANDLE NO-UNDO.
    DEF VAR parm-fld-list AS CHAR NO-UNDO.
    DEF VAR parm-lbl-list AS CHAR NO-UNDO.
    DEF VAR i AS INT NO-UNDO.
    DEF VAR lv-label AS CHAR.

    ASSIGN 
        lv-frame-hdl = frame {&frame-name}:handle
        lv-group-hdl = lv-frame-hdl:first-child
        lv-field-hdl = lv-group-hdl:first-child
        .

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP(lv-field-hdl:private-data,"parm") GT 0 THEN DO:
            IF lv-field-hdl:label NE ? THEN ASSIGN 
                parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," 
                .
            ELSE DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                    lv-field2-hdl = lv-group-hdl:first-child
                    .
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN 
                        LEAVE. 
                    IF lv-field2-hdl:private-data EQ lv-field-hdl:name THEN DO:
                        ASSIGN 
                            parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".
                    END.
                    ASSIGN
                        lv-field2-hdl = lv-field2-hdl:next-sibling.                 
                END.       
            END.                 
        END.            
        ASSIGN 
            lv-field-hdl = lv-field-hdl:next-sibling.   
    END.

    PUT SPACE(28) "< Selection Parameters >" SKIP(1).

    DO i = 1 TO num-entries(parm-fld-list,","):
        IF ENTRY(i,parm-fld-list) NE "" 
        OR ENTRY(i,parm-lbl-list) NE "" THEN DO:
            ASSIGN
                lv-label = FILL(" ",34 - length(trim(ENTRY(i,parm-lbl-list)))) +
                           TRIM(ENTRY(i,parm-lbl-list)) + ":".

            PUT 
                lv-label FORMAT "x(35)" AT 5 SPACE(1)
                TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)" SKIP.              
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
  /*RETURN STRING(hField:BUFFER-VALUE, hField:FORMAT) */
  RETURN STRING(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

